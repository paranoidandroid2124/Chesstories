package io.chesstory.runtime

/** Finite supplied values isolate exact conditional prices, not engine
  * strength, forced conversion, or the original numerical cause. */
class AnchoredChoicePriceTest extends munit.FunSuite:
  import AnchoredChoiceTestFixture.*
  import CommentaryAnswer.*
  import CommentaryProposal.*
  import RootQuestionSummary.MoveRef

  private val advanced = decisionPrefix ++ List(root, resistance, "g4g5", "f6g5")
  private val deferred = decisionPrefix ++ List(keeping, resistance, root, "d6e5", "g4g5", "f6g5")

  private def fixtureWithLoss(): AnchoredFixture =
    val base = fixture()
    val replaced = Set(decisionPrefix ++ List(root, resistance),
      decisionPrefix ++ List(keeping, resistance))
    val lines = base.paths.paths.map(_.moves).filterNot(replaced) ++ List(advanced, deferred)
    val (packet, paths, receipt) = rootPacket(lines = lines)
    val batch = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, keepRoot)),
      lines.map(moves => CommentaryDiscovery.SourcePath(moves,
        Source("user", "anchored-resource-later-price-test"))), Nil, pvGuided = true)
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

  test("both current valued resistance leaders retain the later loss of the exact original resource"):
    val f = fixtureWithLoss()
    val task = read(f, own(f) ++ opposing(f) ++ capturePairs(f)).get
    assert(task.complete)
    val owner = task.stage.rootOwner.answer
    val roots = List(owner.referenceLineId, owner.alternativeLineId).map(f.paths.path(f.packet, _).get)
    val seed = Continuation(roots.map(MoveRef(_, 0)))
    val before = RootQuestionSummary.completePairAnswer(f.packet, f.paths, roots,
      "selected_comparison", List(Section(List(seed)))).get.copy(completionSources = Some(List(seed)))
    val positions = f.packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(f.packet, f.paths).local
    val delivered = AnchoredChoiceAnswer.withAnswer(f.packet, f.paths, roots, List(task), before,
      (answer, original) => MoveWorkProjection.protectRootUses(f.packet, f.paths, roots,
        positions, notes, answer, original))
    assertEquals(delivered.question, "functional_response")
    val decision = delivered.core.flatMap(_.statements).collectFirst { case value: AnchoredDecision => value.value }.get
    assertEquals(decision.advance.moves, advanced)
    assertEquals(decision.other.moves, deferred)
    val original = CommentaryFact.Piece("white", "pawn", "g3").witness
    assert(decision.states.collect { case value: PiecePresence => value }.forall(_.original == original))
    val covered = delivered.core.flatMap(_.covered).toSet
    List(decision.advance -> 5, decision.other -> 7).foreach { (path, offset) =>
      val event = f.paths.continuationReadingFor(path).get.events.find(_.plyOffset == offset).get
      assert(event.originalVictim.contains(original))
      assertEquals(path.moves(offset), "f6g5")
      assert(covered(MoveRef(path, offset)),
        "the separately owned later resource loss cannot borrow a price from another root or branch")
    }
    assertEquals(task.stage.rootOwner.answer, owner)
    assertEquals(delivered.comparison, before.comparison)
    assert(RootQuestionSummary.realize(f.paths, roots, delivered).nonEmpty)
    assert(delivered.completionSources.get.forall(!_.covered.exists(ref => Set(5, 7)(ref.offset))),
      "computed later prices must not seed another selected lifetime episode")
    val exchanged = delivered.copy(core = delivered.core.map(section => section.copy(
      statements = section.statements.map {
        case value: MaterialEvents if !roots.exists(_ eq value.path) && value.events.nonEmpty &&
            value.events.forall(row => row.capture.nonEmpty && row.promotionGainCp == 0) =>
          val reading = f.paths.continuationReadingFor(value.path).get
          val arm = new RootQuestionSummary.Arm(f.packet, f.paths, value.path, positions, reading)
          val events = value.events.map(row => reading.events.find(_.plyOffset == row.plyOffset).get)
          assert(arm.identityRoles(events))
          arm.pieceExchange(events)
        case value => value
      })))
    assertEquals(exchanged.required, delivered.required)
    assertEquals(exchanged.core.flatMap(_.covered).toSet, covered)
    assert(RootQuestionSummary.realize(f.paths, roots, exchanged).nonEmpty,
      "an exact ordinary physical capture still expresses its closed price as a certified piece exchange")
    val damaged = delivered.copy(core = delivered.core.map(section => section.copy(
      statements = section.statements.map {
        case value: MaterialEvents => value.copy(events = value.events.filterNot(event =>
          covered(MoveRef(value.path, event.plyOffset)) && Set(5, 7)(event.plyOffset)))
        case value => value
      })))
    assertEquals(damaged.required, delivered.required)
    assert(RootQuestionSummary.realize(f.paths, roots, damaged).isEmpty,
      "time-qualified absence or required references cannot replace a selected resource's actual capture price")
    val moved = damaged.copy(details = damaged.details ++ delivered.core.map(section =>
      Section(section.statements.collect { case value: MaterialEvents => value })))
    assert(RootQuestionSummary.realize(f.paths, roots, moved).isEmpty,
      "an exact price moved to detail cannot close the independently selected core decision")
