package io.chesstory.runtime

/** Supplied finite values test resource identity and bounded prices. None of
  * these values is an engine study, a forced result or a root-causal proof. */
class AnchoredChoicePriceSafetyTest extends munit.FunSuite:
  import AnchoredChoiceTestFixture.*
  import CommentaryAnswer.*
  import CommentaryProposal.*
  import ObservedLineProtocol.PreparedPath
  import RootQuestionSummary.MoveRef

  private val original = CommentaryFact.Piece("white", "pawn", "g3").witness
  private val advancedPromotion = decisionPrefix ++ List(root, resistance,
    "g4g5", "d6e7", "g5g6", "e7f8", "g6g7", "f8f7", "g7g8q", "f7g8")
  private val deferredPromotion = decisionPrefix ++ List(keeping, resistance,
    root, "d6e7", "g4g5", "e7f8", "g5g6", "f8e8", "g6g7", "e8f7", "g7g8q", "f7g8")
  private val advancedQuietPromotion = decisionPrefix ++ List(root, resistance,
    "g4g5", "d6c6", "g5g6", "c6b5", "g6g7", "b5b6", "g7g8q", "b6c6", "g8b8", "c6d7", "b8d6", "d7d6")
  private val deferredQuietPromotion = decisionPrefix ++ List(keeping, resistance,
    root, "d6c6", "g4g5", "c6b5", "g5g6", "b5b6", "g6g7", "b6c6", "g7g8q", "c6d7", "g8b8", "d7c6", "b8d6", "c6d6")
  private val advancedDisconnected = decisionPrefix ++ List(root, resistance,
    "g4g5", "f6g5", "e3f3", "d6e6", "f3g3", "e6e7", "g3g4", "e7e8", "g4g5")
  private val deferredDisconnected = decisionPrefix ++ List(keeping, resistance,
    root, "d6e5", "g4g5", "f6g5", "f3g3", "e5e6", "g3g4", "e6e7", "g4g5")
  private val advancedOtherLoss = decisionPrefix ++ List(root, resistance, "e4e5", "f6e5")
  private val deferredOtherLoss = decisionPrefix ++ List(keeping, resistance, "e4e5", "f6e5")

  private def withLines(advanced: List[String], deferred: List[String]): AnchoredFixture =
    val base = fixture()
    val replaced = Set(decisionPrefix ++ List(root, resistance),
      decisionPrefix ++ List(keeping, resistance))
    val lines = base.paths.paths.map(_.moves).filterNot(replaced) ++ List(advanced, deferred)
    val (packet, paths, receipt) = rootPacket(lines = lines)
    val batch = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, keepRoot)),
      lines.map(moves => CommentaryDiscovery.SourcePath(moves,
        Source("user", "anchored-resource-price-safety-test"))), Nil, pvGuided = true)
    val generated = batch.additions.find(entry =>
      entry.request.origin.question.producer == "C-reserve-timing" && entry.request.requirements.exists {
        case value: CompareMoves => value.id == "reserve_timing" && value.alternativeMove == keeping
        case _ => false
      }).get
    val answer = CommentaryInvestigation.pairedAnswer(packet, root, keepRoot,
      receipt.work, receipt.lines, Some(paths)).get
    val answered = generated.copy(readings = generated.readings.map(row =>
      if row.requirementId == "reserve_root" then pairReading(
        generated.request.requirements.collectFirst { case value: CompareMoves if value.id == "reserve_root" => value }.get,
        answer) else row))
    AnchoredFixture(packet, paths, CommentaryFactReader.read(packet, paths, List(answered)).head, receipt)

  private def pair(f: AnchoredFixture, task: AnchoredChoiceReading.Task): List[PreparedPath] =
    val owner = task.stage.rootOwner.answer
    List(owner.referenceLineId, owner.alternativeLineId).map(f.paths.path(f.packet, _).get)

  private def previous(f: AnchoredFixture, roots: List[PreparedPath]): Answer =
    val seed = Continuation(roots.map(MoveRef(_, 0)))
    RootQuestionSummary.completePairAnswer(f.packet, f.paths, roots,
      "selected_comparison", List(Section(List(seed)))).get.copy(completionSources = Some(List(seed)))

  private def deliver(f: AnchoredFixture, task: AnchoredChoiceReading.Task,
      roots: List[PreparedPath], before: Answer): Answer =
    val positions = f.packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(f.packet, f.paths).local
    AnchoredChoiceAnswer.withAnswer(f.packet, f.paths, roots, List(task), before,
      (answer, old) => MoveWorkProjection.protectRootUses(f.packet, f.paths, roots,
        positions, notes, answer, old))

  private def current(f: AnchoredFixture) =
    read(f, own(f) ++ opposing(f) ++ capturePairs(f)).get

  private def decision(answer: Answer) = answer.core.flatMap(_.statements).collectFirst {
    case value: AnchoredDecision => value.value
  }.get

  test("a promoted original resource keeps the actual queen price and its checking promotion"):
    val f = withLines(advancedPromotion, deferredPromotion)
    val task = current(f)
    assert(task.complete)
    val roots = pair(f, task)
    val before = previous(f, roots)
    val answer = deliver(f, task, roots, before)
    assertEquals(answer.question, "functional_response")
    val selected = decision(answer)
    assertEquals(selected.advance.moves, advancedPromotion)
    assertEquals(selected.other.moves, deferredPromotion)
    val covered = answer.core.flatMap(_.covered).toSet
    List(selected.advance -> 11, selected.other -> 13).foreach { (path, lossOffset) =>
      val reading = f.paths.continuationReadingFor(path).get
      val loss = reading.events.find(_.plyOffset == lossOffset).get
      assert(loss.originalVictim.contains(original))
      assertEquals(path.moves(lossOffset), "f7g8")
      val material = path.certified.flatMap(_._2.material).get
      val taken = material.events.find(_.plyOffset == lossOffset).get
      val promotion = material.events.find(_.plyOffset == lossOffset - 1).get
      assertEquals(taken.capture.get.capturedRole.name, "queen")
      assertEquals(promotion.movement.beforeRole.name, "pawn")
      assertEquals(promotion.movement.afterRole.name, "queen")
      assert(promotion.promotionGainCp > 0)
      assert(covered(MoveRef(path, lossOffset)), "the original pawn's current queen role is actually lost")
      val statedPromotion = answer.core.flatMap(_.statements).exists {
        case value: MaterialEvents => (value.path eq path) && value.events.contains(promotion)
        case value: Material => (value.path eq path) && value.cost.events.contains(promotion)
        case _ => false
      }
      assert(statedPromotion,
        "the promoted role and its price cannot be detached from the original pawn's loss")
      assert(answer.required.contains(MoveRef(path, lossOffset - 1)))
      assert(answer.required.contains(MoveRef(path, lossOffset)))
      val positions = f.packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
      val arm = new RootQuestionSummary.Arm(f.packet, f.paths, path, positions, reading)
      assert(!arm.identityRoles(List(loss)), "original pawn identity is not the captured queen role")
      assert(arm.checks(lossOffset - 1))
      assert(arm.recovery(loss).isEmpty, "the actual king capture has no recorded recapture")
    }
    assertEquals(answer.comparison, before.comparison)
    assertEquals(task.stage.rootOwner.answer, selected.task.stage.rootOwner.answer)
    assert(RootQuestionSummary.realize(f.paths, roots, answer).nonEmpty)

  test("a prior nonchecking promotion remains the exact original resource's role and price context"):
    val f = withLines(advancedQuietPromotion, deferredQuietPromotion)
    val task = current(f)
    assert(task.complete)
    val roots = pair(f, task)
    val before = previous(f, roots)
    val answer = deliver(f, task, roots, before)
    assertEquals(answer.question, "functional_response")
    val selected = decision(answer)
    val covered = answer.core.flatMap(_.covered).toSet
    List((selected.advance, 10, 15), (selected.other, 12, 17)).foreach {
      (path, promotionOffset, lossOffset) =>
        val reading = f.paths.continuationReadingFor(path).get
        val material = path.certified.flatMap(_._2.material).get
        val promotion = material.events.find(_.plyOffset == promotionOffset).get
        val loss = reading.events.find(_.plyOffset == lossOffset).get
        assert(loss.originalVictim.contains(original))
        assertEquals(material.events.find(_.plyOffset == lossOffset).get.capture.get.capturedRole.name, "queen")
        val positions = f.packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
        val arm = new RootQuestionSummary.Arm(f.packet, f.paths, path, positions, reading)
        assert(!arm.checks(promotionOffset), "this role change cannot borrow a preceding-check context")
        assert(arm.checks(lossOffset - 1))
        assert(covered(MoveRef(path, lossOffset)))
        val statedPromotion = answer.core.flatMap(_.statements).exists {
          case value: MaterialEvents => (value.path eq path) && value.events.contains(promotion)
          case value: Material => (value.path eq path) && value.cost.events.contains(promotion)
          case _ => false
        }
        assert(statedPromotion, "a later queen loss cannot hide its own original pawn's nonchecking promotion")
        assert(answer.required.contains(MoveRef(path, promotionOffset)))
        assert(answer.required.contains(MoveRef(path, lossOffset - 1)))
        assert(answer.required.contains(MoveRef(path, lossOffset)))
    }
    assertEquals(answer.comparison, before.comparison)
    assert(RootQuestionSummary.realize(f.paths, roots, answer).nonEmpty)

  test("a disconnected later king gain is not an immediate recovery of the resource price"):
    val f = withLines(advancedDisconnected, deferredDisconnected)
    val task = current(f)
    assert(task.complete)
    val roots = pair(f, task)
    val before = previous(f, roots)
    val answer = deliver(f, task, roots, before)
    assertEquals(answer.question, "functional_response")
    val selected = decision(answer)
    val covered = answer.core.flatMap(_.covered).toSet
    List(selected.advance -> 5, selected.other -> 7).foreach { (path, lossOffset) =>
      val reading = f.paths.continuationReadingFor(path).get
      val loss = reading.events.find(_.plyOffset == lossOffset).get
      val positions = f.packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
      val arm = new RootQuestionSummary.Arm(f.packet, f.paths, path, positions, reading)
      assert(loss.originalVictim.contains(original))
      assert(arm.recovery(loss).nonEmpty, "the recorded line eventually takes that same capturer")
      assert(arm.continuousRecovery(loss).isEmpty,
        "the recovering king cannot attack g5 immediately after fxg5")
      assert(covered(MoveRef(path, lossOffset)))
      assert(!covered(MoveRef(path, 12)),
        "a later disconnected Kxg5 does not extend this selected immediate resource-loss account")
    }
    assertEquals(answer.comparison, before.comparison)

  test("a different original pawn loss is not the retained resource's later price"):
    val f = withLines(advancedOtherLoss, deferredOtherLoss)
    val task = current(f)
    assert(task.complete)
    val roots = pair(f, task)
    val answer = deliver(f, task, roots, previous(f, roots))
    assertEquals(answer.question, "functional_response")
    val selected = decision(answer)
    val covered = answer.core.flatMap(_.covered).toSet
    List(selected.advance, selected.other).foreach { path =>
      val loss = f.paths.continuationReadingFor(path).get.events.find(_.plyOffset == 5).get
      assert(loss.originalVictim.contains(CommentaryFact.Piece("white", "pawn", "e4").witness))
      assert(!loss.originalVictim.contains(original))
      assert(!covered(MoveRef(path, 5)),
        "an unseen later loss of another original pawn cannot become a Pg3 lifetime price")
    }

  test("identical FEN and forecast in a foreign prepared owner cannot inject resource prices"):
    val f = withLines(advancedDisconnected, deferredDisconnected)
    val foreign = withLines(advancedDisconnected, deferredDisconnected)
    val task = current(f)
    val localTask = current(foreign)
    assert(task.complete && localTask.complete)
    assertEquals(task.stage.prefix, localTask.stage.prefix)
    assertEquals(task.use.get.reference.moves, localTask.use.get.reference.moves)
    assert(!(task.use.get.reference eq localTask.use.get.reference))
    val roots = pair(foreign, localTask)
    val before = previous(foreign, roots)
    val refused = deliver(foreign, task, roots, before)
    assertEquals(refused, before,
      "equal history, forecast and supplied numbers do not transfer the private price owner")
