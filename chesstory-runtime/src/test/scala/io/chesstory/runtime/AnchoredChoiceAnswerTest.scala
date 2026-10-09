package io.chesstory.runtime

import play.api.libs.json.*

/** Supplied current receipts exercise typed choice delivery and exact prices.
  * They establish no best-play theorem, automatic discovery, or root cause.
  */
class AnchoredChoiceAnswerTest extends munit.FunSuite:
  import AnchoredChoiceTestFixture.*
  import CommentaryAnswer.*
  import RootQuestionSummary.MoveRef
  import ObservedLineProtocol.PreparedPath

  private def pair(f: AnchoredFixture): List[PreparedPath] =
    val owner = CommentaryIdea.reserveChoices(List(f.entry)).map(_.owner.answer)
      .headOption.orElse(CommentaryIdea.reserveTimings(List(f.entry)).map(_.owner.answer).headOption).get
    List(owner.referenceLineId, owner.alternativeLineId).map(f.paths.path(f.packet, _).get)

  private def previous(f: AnchoredFixture, roots: List[PreparedPath]): Answer =
    val seed = Continuation(roots.map(MoveRef(_, 0)))
    RootQuestionSummary.completePairAnswer(f.packet, f.paths, roots, "selected_comparison",
      List(Section(List(seed)))).get.copy(
        evidence = List("supplied-original-root-owner"),
        comparison = Some(ComparisonScope(Set(CommentaryFact.Piece("white", "pawn", "g3").witness))),
        completionSources = Some(List(seed)))

  private def deliver(f: AnchoredFixture, value: AnchoredChoiceReading.Task,
      roots: List[PreparedPath], before: Answer): Answer =
    val positions = f.packet.resources.map(view =>
      view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(f.packet, f.paths).local
    AnchoredChoiceAnswer.withAnswer(f.packet, f.paths, roots, List(value), before,
      (answer, original) => MoveWorkProjection.protectRootUses(f.packet, f.paths, roots,
        positions, notes, answer, original))

  private def deliverFamily(f: AnchoredFixture, family: AnchoredChoiceReading.Family,
      roots: List[PreparedPath], before: Answer): Answer =
    val positions = f.packet.resources.map(view =>
      view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(f.packet, f.paths).local
    AnchoredChoiceAnswer.withFamily(f.packet, f.paths, roots, List(family), before,
      (answer, original) => MoveWorkProjection.protectRootUses(f.packet, f.paths, roots,
        positions, notes, answer, original))

  private def complete(f: AnchoredFixture) = read(f, own(f) ++ opposing(f) ++ capturePairs(f)).get
  private def center(answer: Answer) = answer.core.flatMap(_.statements).collect {
    case value: AnchoredDecision => value
  }

  private def english(f: AnchoredFixture, statement: Statement): String =
    CommentaryEnglish.statement(statement).map {
      case text: String => text
      case move: MoveRef => f.paths.notation.line(move.path.moves)(move.offset)
    }.mkString

  test("the completed local choice preserves the original root judgment and expresses both current resistances"):
    val f = fixture()
    val task = complete(f)
    assert(task.complete)
    val roots = pair(f)
    val before = previous(f, roots)
    val originalAnswer = task.stage.rootOwner.answer
    val delivered = deliver(f, task, roots, before)
    assertEquals(delivered.question, "functional_response")
    assertEquals(delivered.comparison, before.comparison)
    assert(delivered.evidence.contains("supplied-original-root-owner"))
    assert(before.completionSources.get.forall(delivered.completionSources.get.contains))
    assertEquals(task.stage.rootOwner.answer, originalAnswer,
      "the current stage cannot rewrite either original numerical value or receipt")
    assertEquals(originalAnswer.evaluationRelation, "better")
    val statement = center(delivered).head
    assertEquals(center(delivered).size, 1)
    val value = statement.value
    assertEquals(value.rootAdvance.path.line.ref.id, originalAnswer.referenceLineId)
    assertEquals(value.rootDeferred.path.line.ref.id, originalAnswer.alternativeLineId)
    assertEquals(value.prefix.map(move => move.path.moves(move.offset)), decisionPrefix)
    assertEquals(value.choice.path.moves(value.choice.offset), root)
    assertEquals(value.alternative.path.moves(value.alternative.offset), keeping)
    assertEquals(value.replies.map(move => move.path.moves(move.offset)), List(resistance, resistance))
    val present = value.states.collect { case state: PiecePresence => state }
    assertEquals(present.size, 2)
    assert(present.forall(_.original == task.stage.pawn.witness))
    assertEquals(present.map(_.current.square.key), List("g4", "g3"))
    assert(RootQuestionSummary.realize(f.paths, roots, delivered).nonEmpty)
    val text = english(f, statement)
    assert(text.contains("defers that advance") && text.contains("prefer Kf3 to an immediate g4"), text)
    assert(text.toLowerCase.contains("a leading tested reply"), text)
    List("forced", "must", "Kf4", "cycle", "whole evaluation").foreach(word =>
      assert(!text.contains(word), text))

  test("an unvalued omitted capture or a foreign prepared owner cannot upgrade the original answer"):
    val f = fixture()
    val roots = pair(f)
    val before = previous(f, roots)
    val incomplete = read(f, own(f) ++ opposing(f) ++ capturePairs(f).take(1)).get
    assert(!incomplete.complete)
    assert(deliver(f, incomplete, roots, before) eq before)
    val foreign = fixture()
    val foreignTask = complete(foreign)
    assert(foreignTask.complete)
    assert(deliver(f, foreignTask, roots, before) eq before)
    assert(deliver(f, complete(f), pair(foreign), before) eq before)

  test("another current root line cannot substitute for the exact original numerical owner"):
    val f = fixture()
    val roots = pair(f)
    val before = previous(f, roots)
    val replacement = f.paths.paths.find(_.moves == (decisionPrefix :+ root)).get
    assertNotEquals(replacement.line.ref.id, roots(1).line.ref.id)
    assertEquals(replacement.ids.head, roots(1).ids.head)
    assertEquals(replacement.positions.head, roots(1).positions.head)
    assert(deliver(f, complete(f), List(roots.head, replacement), before) eq before,
      "a same-canonical first move cannot borrow another full root line's values")

  test("one original reserve family keeps its differing plans conditional on their own replies"):
    val f = AnchoredChoiceFamilyFixture.fixture()
    val family = AnchoredChoiceFamilyFixture.read(f, AnchoredChoiceFamilyFixture.completeSearches(f)).get
    assert(family.complete && family.followupComplete)
    val owner = family.owner.answer
    val roots = List(owner.referenceLineId, owner.alternativeLineId).map(f.paths.path(f.packet, _).get)
    val before = previous(f, roots)
    assertEquals(family.owner.answer, owner)
    assertEquals(family.initialStages.map(_.stage.prefix).toSet,
      Set(AnchoredChoiceFamilyFixture.blockade, AnchoredChoiceFamilyFixture.kingReply))
    val underPawn = family.initialStages.find(_.stage.prefix == AnchoredChoiceFamilyFixture.blockade).get
    val underKing = family.initialStages.find(_.stage.prefix == AnchoredChoiceFamilyFixture.kingReply).get
    assert(underPawn.use.get.refuted && underKing.use.get.useful)
    // Independently owned stages retain both reply-dependent judgments even
    // when the bounded Family center chooses only its current leading plan.
    val deliveries = List(underPawn, underKing).map(task => deliver(f, task, roots, before))
    deliveries.foreach { answer =>
      assertEquals(answer.question, "functional_response")
      assertEquals(answer.comparison, before.comparison)
      assert(before.completionSources.get.forall(answer.completionSources.get.contains))
      assert(RootQuestionSummary.realize(f.paths, roots, answer).nonEmpty)
    }
    assertEquals(center(deliveries.head).head.value.alternative.path.moves(underPawn.stage.prefix.size), keeping)
    assertEquals(center(deliveries(1)).head.value.alternative.path.moves(underKing.stage.prefix.size),
      AnchoredChoiceFamilyFixture.kingPlan)
    val text = deliveries.flatMap(center).map(english(f, _)).mkString(" ")
    assert(text.contains("prefer Kf3") && text.contains("competitive alongside Kd2"), text)
    assert(!text.contains("forced") && !text.contains("winning"), text)
    val incomplete = AnchoredChoiceFamilyFixture.read(f,
      AnchoredChoiceFamilyFixture.initialCompleteSearches(f)).get
    assert(incomplete.initialComplete && !incomplete.complete)
    assert(deliverFamily(f, incomplete, roots, before) eq before)
    val foreign = AnchoredChoiceFamilyFixture.fixture()
    val foreignFamily = AnchoredChoiceFamilyFixture.read(foreign,
      AnchoredChoiceFamilyFixture.completeSearches(foreign)).get
    assert(foreignFamily.complete)
    assert(deliverFamily(f, foreignFamily, roots, before) eq before)

  test("a completed response family delivers its leading plan and later use within both witness layers"):
    val f = AnchoredChoiceFamilyFixture.fixture()
    val searches = AnchoredChoiceFamilyFixture.completeSearches(f, longest = true)
    val family = AnchoredChoiceFamilyFixture.read(f, searches).get
    assert(family.complete && family.stages.size == 8)
    val roots = pair(f)
    val before = previous(f, roots)
    val positions = f.packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(f.packet, f.paths).local
    val delivered = AnchoredChoiceAnswer.withFamily(f.packet, f.paths, roots, List(family), before,
      (answer, original) => MoveWorkProjection.protectRootUses(f.packet, f.paths, roots, positions, notes, answer, original))
    assertEquals(delivered.question, "functional_response",
      "individually completed stages must also survive the automatic family delivery path")
    val core = center(delivered).map(_.value)
    val detail = delivered.details.flatMap(_.statements).collect { case AnchoredDecision(value) => value }
    val admitted = (core ++ detail).map(_.task.stage.id).toSet
    assert(admitted.subsetOf(family.stages.map(_.stage.id).toSet))
    assert((core ++ detail).forall(_.task.complete))
    val retained = family.roots.find(_.rootMove == family.owner.comparison.alternativeMove).get
    val leading = retained.resistance.leaderMove.get
    val initial = family.initialStages.find(_.stage.prefix == List(retained.rootMove, leading)).get
    val keepingBranch = initial.branches.find(_.keepsPawnHere).get
    val currentSource = keepingBranch.resistance.value.get
    val later = family.followupStages.find(task => task.stage.parentStageId.contains(initial.stage.id) &&
      task.stage.source.exists(value => value.question == currentSource.question &&
        value.answer == currentSource.answer && (value.reference eq currentSource.reference))).get
    assertEquals(core.map(_.task.stage.id).toSet, Set(initial.stage.id, later.stage.id),
      "only the current leading plan and its own exact leading resistance can select the central use")
    assert(detail.nonEmpty, "compatible independent plans remain whole price-closed detail accounts")
    List(delivered.core, delivered.details).foreach { sections =>
      val foreign = sections.flatMap(_.references).map(_.path).distinct.filterNot(path => roots.exists(_ eq path))
      assert(foreign.size <= 5, "the fixed witness bounds apply independently to each layer")
    }
    delivered.details.foreach { section =>
      val covered = section.covered.toSet
      section.statements.collect { case AnchoredDecision(value) => value }.foreach { value =>
        assert(value.closedPrices.flatMap(_.covered).forall(covered),
          "a detail account cannot omit its original resource's actual later loss")
        value.replies.foreach { reply =>
          val material = reply.path.certified.flatMap(_._2.material).get
          if material.events.exists(event => event.plyOffset == reply.offset && event.capture.nonEmpty) then
            assert(covered(reply), "an admitted selected capture must retain its exact price")
        }
        if value.task.stage.prefix == AnchoredChoiceFamilyFixture.checkingPrefix then
          assert(covered(MoveRef(value.advance, 4)) && covered(MoveRef(value.advance, 5)),
            "an admitted checking account must retain both central pawn exchange prices")
      }
    }
    assertEquals(family.stages.map(_.stage.id).toSet.size, 8,
      "undisplayed finite comparisons stay in the owned Family, not silently removed to fit text")
    assertEquals(delivered.comparison, before.comparison)
    assert(before.completionSources.get.forall(delivered.completionSources.get.contains))
    assertEquals(family.owner.answer, CommentaryIdea.reserveChoices(List(f.entry)).head.owner.answer)
    val summary = RootQuestionSummary.realize(f.paths, roots, delivered).get
    List("units", "detail_units").foreach { field =>
      val units = (summary.json \ field).asOpt[List[JsObject]].getOrElse(Nil)
      assert(units.size <= 8)
      units.foreach(unit => List("en", "ko").foreach(language =>
        assert((unit \ language).as[JsArray].value.size <= 80)))
    }

  test("family organization preserves earlier selected prices when exact witnesses can be shared"):
    val f = AnchoredChoiceFamilyFixture.fixture()
    val searches = AnchoredChoiceFamilyFixture.completeSearches(f, longest = true)
    val family = AnchoredChoiceFamilyFixture.read(f, searches).get
    val checking = family.followupStages.find(_.stage.prefix == AnchoredChoiceFamilyFixture.checkingPrefix).get
    val pricePaths = checking.branches.map(branch =>
      branch.resistance.value.get.comparison.pathsByMove(branch.resistance.leaderMove.get))
    val roots = pair(f)
    val seed = previous(f, roots)
    val oldPrices = pricePaths.map { path =>
      val events = path.certified.flatMap(_._2.material).get.events.filter(event => Set(4, 5)(event.plyOffset))
      assertEquals(events.map(_.plyOffset).toSet, Set(4, 5))
      Section(List(MaterialEvents(path, events), Continuation(List(MoveRef(path, path.moves.size - 1)))),
        events.map(event => MoveRef(path, event.plyOffset)))
    }
    val before = seed.copy(core = seed.core ++ oldPrices)
    assert(RootQuestionSummary.realize(f.paths, roots, before).nonEmpty)
    val positions = f.packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(f.packet, f.paths).local
    val delivered = AnchoredChoiceAnswer.withFamily(f.packet, f.paths, roots, List(family), before,
      (answer, original) => MoveWorkProjection.protectRootUses(f.packet, f.paths, roots, positions, notes, answer, original))
    assertEquals(delivered.question, "functional_response")
    assert(before.required.forall(delivered.required.contains))
    assert(before.core.flatMap(_.covered).forall(delivered.core.flatMap(_.covered).contains))
    assertEquals(delivered.core.flatMap(_.references).map(_.path).distinct
      .filterNot(path => roots.exists(_ eq path)).size, 5)
    assertEquals(delivered.comparison, before.comparison)
    assert(before.completionSources.get.forall(delivered.completionSources.get.contains))
    val king = family.initialStages.find(_.stage.prefix == AnchoredChoiceFamilyFixture.kingReply).get
    val extra = king.branches.head.resistance.captures.head.value.get.reference
    val events = extra.certified.flatMap(_._2.material).get.events
    assert(events.nonEmpty)
    val extraPrice = Section(List(MaterialEvents(extra, events), Continuation(List(MoveRef(extra, extra.moves.size - 1)))))
    val cannotFit = before.copy(core = before.core :+ extraPrice)
    assert(RootQuestionSummary.realize(f.paths, roots, cannotFit).nonEmpty)
    val refused = AnchoredChoiceAnswer.withFamily(f.packet, f.paths, roots, List(family), cannotFit,
      (answer, original) => MoveWorkProjection.protectRootUses(f.packet, f.paths, roots, positions, notes, answer, original))
    assert(refused eq cannotFit,
      "an additional independent selected price cannot be removed to force the new center inside five witnesses")

  test("a legal current resource under a changed leading reply awaits its own value and resistances"):
    val f = AnchoredChoiceFamilyFixture.fixture()
    val prefix = AnchoredChoiceFamilyFixture.blockade :+ keeping
    val response = AnchoredChoiceFamilyFixture.supplied(f, "held-peer-leader", prefix,
      List(peer -> 490, resistance -> 500, pawnReply -> 520), longest = true)
    val captureResponse = AnchoredChoiceFamilyFixture.supplied(f, "held-peer-capture", prefix,
      List(peer -> 490, capture -> 515), restricted = true, longest = true)
    val responses = AnchoredChoiceFamilyFixture.resistances(f, longest = true)
      .filterNot(_.anchor.prefixMoves == prefix) :+ response
    val captures = AnchoredChoiceFamilyFixture.captures(f, longest = true)
      .filterNot(_.anchor.prefixMoves == prefix) :+ captureResponse
    val initial =
      AnchoredChoiceFamilyFixture.firstPools(f) ++ AnchoredChoiceFamilyFixture.ownPools(f) ++
        responses ++ captures ++ AnchoredChoiceFamilyFixture.later(f)
    val family = AnchoredChoiceFamilyFixture.read(f, initial).get
    assert(family.initialComplete && !family.complete)
    assert(family.followupStages.nonEmpty,
      "the alternative adequate replies still own their independent later pawn uses")
    val pending = family.followupStages.find(_.stage.prefix == (prefix :+ peer)).get
    assert(pending.use.isEmpty && !pending.complete)
    assert(!family.followupBoundary.exists(boundary => boundary.prefix == (prefix :+ peer) &&
      boundary.reason == "no_owned_later_advance"), "current legal availability cannot be discarded for PV absence")
    val roots = pair(f)
    val before = previous(f, roots)
    assert(deliverFamily(f, family, roots, before) eq before)
    val completed = AnchoredChoiceFamilyFixture.read(f,
      initial ++ AnchoredChoiceFamilyFixture.absentAdvanceSearches(f)).get
    assert(completed.complete)
    val delivered = deliverFamily(f, completed, roots, before)
    assertEquals(delivered.question, "functional_response")
    assertEquals(center(delivered).map(_.value.task.stage.prefix).toSet,
      Set(AnchoredChoiceFamilyFixture.blockade, prefix :+ peer))
    assertEquals(center(delivered).head.value.replies(1).path.moves(prefix.size), peer)
    assert(center(delivered).forall(value => !value.value.laterAdvanceUnestablished))
    val current = center(delivered).find(_.value.task.stage.prefix == (prefix :+ peer)).get.value
    assert(current.task.use.get.refuted && current.task.ownQuestion.alternativeMove ==
      AnchoredChoiceFamilyFixture.fallbackOther)
    val text = center(delivered).map(english(f, _)).mkString(" ")
    assert(!text.contains("never") && !text.contains("cannot advance"), text)
    assertEquals(delivered.comparison, before.comparison)

  test("an exact checked reply limits the retained advance while preserving its capture and mandatory recapture"):
    val f = AnchoredChoiceFamilyFixture.fixture()
    val kingAt = AnchoredChoiceFamilyFixture.kingReply :+ AnchoredChoiceFamilyFixture.kingPlan
    val first = AnchoredChoiceFamilyFixture.firstPools(f).filterNot(_.anchor.prefixMoves == List(keepRoot)) :+
      AnchoredChoiceFamilyFixture.supplied(f, "retained-Ke6-leading", List(keepRoot),
        List("e5e6" -> 500, "f7f6" -> 510, "e5d6" -> 800))
    val responses = AnchoredChoiceFamilyFixture.resistances(f).filterNot(_.anchor.prefixMoves == kingAt) :+
      AnchoredChoiceFamilyFixture.supplied(f, "checked-Kd2-leading", kingAt,
        List(capture -> 600, "f7f6" -> 610, "e6e5" -> 620))
    val captures = AnchoredChoiceFamilyFixture.captures(f).filterNot(_.anchor.prefixMoves == kingAt)
    val searches = first ++ AnchoredChoiceFamilyFixture.ownPools(f) ++ responses ++ captures ++
      AnchoredChoiceFamilyFixture.absentAdvanceSearches(f)
    val family = AnchoredChoiceFamilyFixture.read(f, searches).get
    assert(family.complete && !family.followupComplete)
    val boundary = family.followupBoundary.find(value => value.prefix == (kingAt :+ capture) &&
      value.reason == "no_owned_later_advance").get
    val path = boundary.source.get.reference
    assert(path.moves.startsWith(kingAt ++ List(capture, "b2c3")))
    val position = f.paths.positionView(path, kingAt.size + 1).get
    assert(position.currentCheckSources(chess.White).nonEmpty)
    assert(!position.legalRows.get.exists(_.move == root), "g4 is currently illegal before the check reply")
    assertEquals(f.paths.trackedPieceAfter(path, family.pawn.witness, kingAt.size + 1)
      .flatten.map(_.square.key), Some("g3"))
    val roots = pair(f)
    val before = previous(f, roots)
    val delivered = deliverFamily(f, family, roots, before)
    assertEquals(delivered.question, "functional_response")
    assertEquals(center(delivered).map(_.value.task.stage.prefix), List(AnchoredChoiceFamilyFixture.kingReply))
    val statement = center(delivered).head
    assertEquals(statement.value.replies(1).path.moves(kingAt.size), capture)
    assert(statement.value.laterAdvanceUnestablished)
    val priced = delivered.core.flatMap(_.covered).toSet
    val selected = statement.value.replies(1).path
    assert(priced(MoveRef(selected, 3)) && priced(MoveRef(selected, 4)),
      "the bxc3+ limit cannot hide either the original c3 pawn or mandatory b2xc3 price")
    val events = f.paths.continuationReadingFor(selected).get.events
    assert(events.exists(event => event.plyOffset == 3 &&
      event.originalVictim.contains(CommentaryFact.Piece("white", "pawn", "c3").witness)))
    assert(events.exists(event => event.plyOffset == 4 &&
      event.originalVictim.contains(CommentaryFact.Piece("black", "pawn", "b4").witness)))
    val text = english(f, statement)
    assert(text.contains("not established by the examined continuation"), text)
    assert(!text.contains("never") && !text.contains("cannot advance"), text)
    assertEquals(delivered.comparison, before.comparison)
    assert(before.completionSources.get.forall(delivered.completionSources.get.contains))
    assert(RootQuestionSummary.realize(f.paths, roots, delivered).nonEmpty)

  test("a later checking advance uses the existing king-supported passer and retains the preceding exchange price"):
    val f = AnchoredChoiceFamilyFixture.fixture()
    val searches = AnchoredChoiceFamilyFixture.completeSearches(f, longest = true)
    val family = AnchoredChoiceFamilyFixture.read(f, searches).get
    assert(family.complete)
    val task = family.followupStages.find(_.stage.prefix == AnchoredChoiceFamilyFixture.checkingPrefix).get
    val roots = pair(f)
    val before = previous(f, roots)
    val delivered = deliver(f, task, roots, before)
    assertEquals(delivered.question, "functional_response")
    val statement = center(delivered).head
    val role = statement.value.states
    assert(role.exists {
      case Action(_, MoveWorkProjection.Detail.PassedPawn("g4", Some(true))) => true
      case _ => false
    }, "the f-pawn exchange made the g-pawn passed before its advance")
    assert(role.exists {
      case ControlState(_, king, Some(pawn), "g4", true, _) =>
        king.role.name == "king" && king.square.key == "f3" && pawn.square.key == "g4"
      case _ => false
    })
    assert(role.exists { case value: CheckStatus => value.checking && value.reply.nonEmpty; case _ => false })
    assert(statement.value.roleEvidence.nonEmpty)
    val path = statement.value.advance
    val priced = delivered.core.flatMap(_.covered).toSet
    assert(priced(MoveRef(path, 4)) && priced(MoveRef(path, 5)))
    val events = f.paths.continuationReadingFor(path).get.events
    assert(events.exists(event => event.plyOffset == 4 &&
      event.originalVictim.contains(CommentaryFact.Piece("black", "pawn", "f7").witness)))
    assert(events.exists(event => event.plyOffset == 5 &&
      event.originalVictim.contains(CommentaryFact.Piece("white", "pawn", "e4").witness)))
    assertEquals(delivered.comparison, before.comparison)
    assert(RootQuestionSummary.realize(f.paths, roots, delivered).nonEmpty)
    val text = english(f, statement)
    assert(text.contains("passed pawn") && text.contains("g4+") && text.contains("in check"), text)
    assert(!text.contains("Creates a passed pawn") && !text.contains("winning") && !text.contains("promotes"), text)

  test("the same supported quiet pawn advance does not inherit the checking branch's passed-pawn role"):
    val f = AnchoredChoiceFamilyFixture.fixture()
    val searches = AnchoredChoiceFamilyFixture.completeSearches(f, longest = true)
    val task = AnchoredChoiceFamilyFixture.read(f, searches).get.followupStages
      .find(_.stage.prefix == AnchoredChoiceFamilyFixture.quietPrefix).get
    val roots = pair(f)
    val delivered = deliver(f, task, roots, previous(f, roots))
    assertEquals(delivered.question, "functional_response")
    val statement = center(delivered).head
    assert(!statement.value.states.exists {
      case Action(_, _: MoveWorkProjection.Detail.PassedPawn) | _: CheckStatus => true
      case _ => false
    })
    val before = f.paths.positionView(statement.value.advance, statement.value.at).get
    assert(before.pawnStates.exists(value => value.square.key == "f6"))
    val text = english(f, statement)
    assert(!text.contains("passed pawn") && !text.contains("in check"), text)
    assert(RootQuestionSummary.realize(f.paths, roots, delivered).nonEmpty)

  test("a newly selected capture resistance closes its exact immediate recapture prices on both branches"):
    val f = fixture()
    val captureLeaders = List(root, keeping).map { move =>
      val base = if move == root then 0 else 500
      supplied(f, "capture-leader-" + move, decisionPrefix :+ move,
        List(capture -> base, resistance -> (base + 10), peer -> (base + 20)))
    }
    val task = read(f, own(f) ++ captureLeaders).get
    assert(task.complete)
    assert(task.branches.forall(_.resistance.requiredCaptureMoves.isEmpty))
    val roots = pair(f)
    val before = previous(f, roots)
    val delivered = deliver(f, task, roots, before)
    assertEquals(delivered.question, "functional_response")
    val statement = center(delivered).head
    assertEquals(statement.value.replies.map(move => move.path.moves(move.offset)), List(capture, capture))
    val covered = delivered.core.flatMap(_.covered).toSet
    statement.value.replies.foreach { reply =>
      assert(covered(MoveRef(reply.path, 3)), "the selected b4 pawn's actual capture is required")
      assert(covered(MoveRef(reply.path, 4)), "the same-square immediate white recapture is required")
      val events = reply.path.certified.flatMap(_._2.material).get.events
      assert(events.exists(event => event.plyOffset == 3 &&
        event.capture.exists(capturer => !capturer.side.white && capturer.capturedRole.name == "pawn" &&
          capturer.square.key == "c3")))
      assert(events.exists(event => event.plyOffset == 4 &&
        event.capture.exists(capturer => capturer.side.white && capturer.capturedRole.name == "pawn" &&
          capturer.square.key == "c3")))
    }
    assert(delivered.completionSources.get.contains(statement))
    assert(!delivered.completionSources.get.flatMap(_.covered).exists(_.offset == 4),
      "a computed recapture price cannot seed another selected lifetime episode")
    assert(RootQuestionSummary.realize(f.paths, roots, delivered).nonEmpty)
