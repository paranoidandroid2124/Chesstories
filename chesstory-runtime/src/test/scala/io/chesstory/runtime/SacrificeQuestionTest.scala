package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Legal development positions and invented receipts, never engine-strength evidence. */
class SacrificeQuestionTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val fen = "6k1/5ppp/8/8/8/3B4/6PP/5RK1 w - - 0 1"
  private val line = List("f1f7", "g8f7", "d3h7", "g7g6", "h7g6")
  private val roots = List(line -> -150, List("f1e1") -> -200, List("d3e2") -> -220)

  private def submit(active: AwaitingEngineWork, rows: List[(List[String], Int)]): CommentaryJobState =
    val work = active.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(active, EngineWorkReport.Completed(profile, work.workId,
      work.executionKeySha256, rows.map { (moves, cp) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), work.requiredDepth)
      }), 1L).toOption.get

  private def observed(board: String = fen, rows: List[(List[String], Int)] = roots,
      prefix: List[String] = Nil, supplied: List[List[String]] = Nil): CommentaryJobState =
    val history = CanonicalPositionHistory.from(board, Nil, board).toOption.get.extend(prefix).toOption.get
    rows.foreach((moves, _) => assert(history.extend(moves).isRight, s"illegal development line $moves"))
    val move = rows.head._1.head
    val start = CommentaryJobReducer.startJob(board, prefix, history.currentFen, move,
      history.extend(List(move)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L,
      inputLines = supplied.map(moves => lila.chessjudgment.model.judgment.SuppliedCommentaryLine(
        lila.chessjudgment.model.judgment.CommentaryLineSource.Study, moves)))
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    val preview = submit(start, rows.map((moves, cp) => moves.take(1) -> cp)).asInstanceOf[AwaitingEngineWork]
    assert(!preview.proposalEntries.exists(_.request.origin.question.producer.startsWith("C-sacrifice")))
    submit(preview, rows)

  private def sacrifice(state: CommentaryJobState, moves: List[String] = line): Entry =
    state.proposalEntries.find(entry => entry.request.origin.question.producer == "C-sacrifice" &&
      entry.request.requirements.exists {
        case fact: Fact => fact.id.startsWith("sacrifice_loss_") && fact.path.source == Declared(moves)
        case _ => false
      }).getOrElse(fail(s"missing loss/compensation question for $moves"))

  private def reading(entry: Entry, id: String): Reading = entry.readings.find(_.requirementId == id).get
  private def text(entry: Entry): String = (CommentaryProposal.snapshot(entry) \ "answer_text").as[String]
  private def comparisons(entry: Entry): List[CompareMoves] = entry.request.requirements.collect { case value: CompareMoves => value }
  private def pair(work: IssuedEngineWork): Set[String] = work.rootRestriction match
    case EngineRootRestriction.Restricted(moves) => moves.toSet
    case _ => Set.empty

  private def acceptanceWork(state: CommentaryJobState): (AwaitingEngineWork, String) =
    val child = state.asInstanceOf[AwaitingEngineWork]
    assertEquals(child.issuedWork.searchAnchor.map(_.prefixMoves), Some(List("f1f7")))
    assert(pair(child.issuedWork)("g8f7"))
    child -> pair(child.issuedWork).filterNot(_ == "g8f7").head

  test("observed loss, material returns and check form an automatic conditional question, not a compensation verdict"):
    val state = observed()
    val entry = sacrifice(state)
    assert(validRequests(List(entry.request)))
    assertEquals(reading(entry, "sacrifice_loss_1_role").result.get.samples.flatMap(_.value), List("rook"))
    assertEquals(reading(entry, "sacrifice_loss_1").state, "supported")
    assertEquals(reading(entry, "sacrifice_gain_2_role").result.get.samples.flatMap(_.value), List("pawn"))
    assertEquals(reading(entry, "sacrifice_gain_4").state, "supported")
    assertEquals(reading(entry, "sacrifice_check_4").state, "supported")
    assertEquals(reading(entry, "sacrifice_current").state, "supported")
    assert(comparisons(entry).exists(_.prefixMovesUci == List("f1f7")))
    assertEquals(comparisons(entry).find(_.id == "sacrifice_decline").map(_.alternativeMove), Some("g7g6"),
      "an observed later action available now must precede an arbitrary unobserved waiting move")
    assert(text(entry).contains("observed loss: white rook originally on f1"))
    assert(text(entry).contains("check against black is present"))
    assert(text(entry).contains("do not by themselves prove adequate compensation"))
    assert(text(entry).contains("unverified"))

  private val observedDeclines = List(List("f1f7", "h7h6"), List("f1f7", "g7g6"))

  test("an observed gain cannot promote a different comparison whose own arm names only the loss"):
    val state = observed()
    val entry = sacrifice(state)
    val comparison = CompareMoves("loss_only_decline", List("f1f7"), "g8f7", "h7h6")
    val extended = entry.copy(request = entry.request.copy(requirements = entry.request.requirements :+ comparison),
      readings = entry.readings :+ Reading(comparison.id), generatedArms = entry.generatedArms :+
        entry.generatedArms.head.copy(id = "loss_only_arm", headRequirementIds = List("sacrifice_loss_1", comparison.id),
          memberRequirementIds = entry.request.requirements.map(_.id) :+ comparison.id))
    val idea = CommentaryIdea.read(List(extended)).head
    assert(idea.observedReturn, "this same idea really does contain supported pawn gains")
    assert(idea.members.head.ready(comparison), "the loss-only arm's stated context is supported")
    assertEquals(idea.priority(comparison), None,
      "supporting members outside this arm cannot supply its missing compensation content")
    val actual = comparisons(extended).find(_.id == "sacrifice_decline").get
    assert(idea.priority(actual).nonEmpty)
    val packet = state.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    List((3, "removed", "refuted"), (6, "present", "partial")).foreach { (ply, expected, outcome) =>
      val unavailable = Fact(s"unavailable_$ply", Path("blocked", Declared(line)),
        Predicate("presence", Some(Piece("white", "bishop", "d3"))), Ply(ply), expected = Some(expected))
      val group = FactGroup(s"blocked_$ply", "all", List("sacrifice_gain_4", unavailable.id))
      val blocked = extended.copy(request = extended.request.copy(
        requirements = extended.request.requirements ++ List(unavailable, group)),
        readings = extended.readings ++ List(Reading(unavailable.id), Reading(group.id)),
        generatedArms = extended.generatedArms :+ extended.generatedArms.last.copy(id = s"blocked_arm_$ply",
          headRequirementIds = List(group.id, comparison.id),
          memberRequirementIds = extended.request.requirements.map(_.id) ++ List(unavailable.id, group.id)))
      val reread = CommentaryFactReader.read(packet, prepared, List(blocked)).head
      assertEquals(reading(reread, unavailable.id).state, outcome)
      assertEquals(CommentaryIdea.read(List(reread)).head.priority(comparison), None,
        "a supported gain in a different blocked arm cannot complete this ready loss-only arm")
    }

  test("a recovery condition links only its exact pending gain and never supplies its answer"):
    val active = observed().asInstanceOf[AwaitingEngineWork]
    val entry = sacrifice(active)
    val packet = active.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val donor = prepared.continuationReadings.find(_.path.moves == line).get
    val recovery = donor.events.find(_.plyOffset == 4).get
    val inquiry = active.continuationInquiries.find(inquiry => inquiry.question.prefixMoves == line.take(2) &&
      inquiry.premiseObservations.exists(_.premise.origin.triggerIds.contains(recovery.occurrenceId))).get
    val scoped = inquiry.copy(premiseObservations = inquiry.premiseObservations.filter(observation =>
      observation.premise.origin.triggerIds.contains(recovery.occurrenceId)))
    def fact(id: String) = entry.request.requirements.collectFirst { case value: Fact if value.id == id => value }.get
    val test = LineContinuationInvestigation.conditionTests(packet, prepared, List(scoped))(scoped.question.id)
    assert(test(List(fact("sacrifice_gain_4") -> reading(entry, "sacrifice_gain_4").result.get)))
    assert(!test(List(fact("sacrifice_gain_2") -> reading(entry, "sacrifice_gain_2").result.get)),
      "Bxh7 does not stand in for the later capture of the original g7 pawn")
    assert(!test(List(fact("sacrifice_loss_1") -> reading(entry, "sacrifice_loss_1").result.get)),
      "a shared sacrificed rook does not identify which recovery is still being tested")
    val unavailable = scoped.copy(premiseObservations = scoped.premiseObservations.map(_.copy(observationState = "unavailable")))
    assert(!LineContinuationInvestigation.conditionTests(packet, prepared, List(unavailable))(unavailable.question.id)(
      List(fact("sacrifice_gain_4") -> reading(entry, "sacrifice_gain_4").result.get)))
    assertEquals(scoped.state, "deferred", "a scheduling link is not a returned reply or proof of adequate compensation")

  test("an observed quiet target escape is tied to the loss without expanding every unseen legal escape"):
    val unobserved = observed()
    assert(!unobserved.proposalEntries.exists(entry => entry.request.origin.question.producer == "C-sacrifice-counter" &&
      comparisons(entry).exists(_.alternativeMove == "h7h6")))
    assertEquals(unobserved.localExpansionsUsed, 0)
    val state = observed(supplied = observedDeclines)
    val counter = state.proposalEntries.find(entry => entry.request.origin.question.producer == "C-sacrifice-counter" &&
      comparisons(entry).exists(compare => compare.prefixMovesUci == List("f1f7") && compare.alternativeMove == "h7h6"))
      .getOrElse(fail("the observed h6 branch must produce the question without an authored interpretation"))
    assertEquals(reading(counter, "sacrifice_loss_1").state, "supported")
    assertEquals(reading(counter, "contrast_context").state, "supported")
    assertEquals(reading(counter, "contrast_strength").answer, None)
    assert(counter.questionText.get.contains("d3h7"))
    assert(counter.questionText.get.contains("g8f7"))
    assert(!counter.request.requirements.exists(_.id.contains("sacrifice_gain_4")),
      "the first return's counter must not copy later unrelated exchanges")
    val action = counter.request.requirements.collectFirst { case fact: Fact if fact.id == "reference_action" => fact }.get
    assertEquals(action.predicate.target, Some(Piece("black", "pawn", "h7")))

  test("a short acceptance without an observed return leaves compensation unknown"):
    val short = line.take(2)
    val entry = sacrifice(observed(rows = roots.updated(0, short -> -150)), short)
    assertEquals(reading(entry, "sacrifice_loss_1").state, "supported")
    assert(!entry.request.requirements.exists(_.id.startsWith("sacrifice_check")))
    assert(entry.questionText.get.contains("No subsequent compensation"))
    assert(!text(entry).contains("observed return:"))
    assert(!text(entry).contains("adequate compensation is established"))
    assert(text(entry).contains("later defenses remain unverified"))

  test("acceptance or decline precedes incidental ways of preventing the return within one sacrifice idea"):
    val state = observed(supplied = observedDeclines)
    val entry = sacrifice(state)
    val idea = CommentaryIdea.read(state.proposalEntries).find(_.members.exists(_.entry.request.id == entry.request.id)).get
    val decline = comparisons(entry).find(_.id == "sacrifice_decline").get
    val incidental = idea.members.flatMap(_.comparisons).find(value => value.expectedRelation.nonEmpty &&
      value.alternativeMove == "h7h6").get
    assert(idea.priority(decline).get._2 < idea.priority(incidental).get._2)
    assertEquals(pair(state.asInstanceOf[AwaitingEngineWork].issuedWork), Set(decline.referenceMove, decline.alternativeMove))
    assertEquals(idea.state, "candidate")
    assert(idea.text.contains("whether the opponent should accept it"))
    assert(idea.members.size > 1, "loss and counters to its return must share their explanatory dependency")

  test("accepting an opponent sacrifice preserves the investor side and reuses an identical root decline pair"):
    val accepted = line.tail
    val state = observed(fen, List(accepted -> -150, List("h7h6") -> -200, List("g8h8") -> -220), List("f1f7"))
    val entry = sacrifice(state, accepted)
    assert(entry.questionText.get.contains("compensates white"))
    assertEquals(reading(entry, "sacrifice_loss_0_role").result.get.samples.flatMap(_.value), List("rook"))
    assert(validRequests(List(entry.request)))
    assertEquals(comparisons(entry).map(compare => (compare.prefixMovesUci, compare.referenceMove, compare.alternativeMove)).distinct.size,
      comparisons(entry).size)

  test("a stronger decline invalidates shallow applicability; deeper root replacement withdraws the old story and renews its answer"):
    val state = observed()
    val entry = sacrifice(state)
    val (child, decline) = acceptanceWork(state)
    val challenged = submit(child, List(line.tail -> -150, List(decline) -> -600))
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(child.progress.physicalWorksIssued, 3,
      "two root searches and the first general investigation leave one general slot available")
    assertEquals(challenged.responseSearches.size, 1)
    assertEquals(challenged.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(challenged.assessmentNeedsRefresh, true)
    val pending = challenged.proposalEntries.find(_.request.id == entry.request.id).get
    assertEquals(reading(pending, "sacrifice_value").answer, None)
    assertNotEquals(reading(pending, "sacrifice_current").state, "supported")
    assertEquals(reading(pending, "sacrifice_observed").state, "supported")
    val newLine = List("f1f7", decline)
    val deeper = submit(challenged, List(newLine -> -600, List("f1e1") -> -200, List("d3e2") -> -220))
    val renewed = deeper.proposalEntries.find(_.request.id == entry.request.id).get
    assertEquals(reading(renewed, "sacrifice_value").answer.flatMap(_.referenceDepth), Some(challenged.issuedWork.requiredDepth))
    assertNotEquals(reading(renewed, "sacrifice_current").state, "supported")
    assertEquals(reading(renewed, "sacrifice_observed").state, "supported")
    assert(text(renewed).contains("withdrawn as a current explanation"))
    val renewedIdea = CommentaryIdea.read(deeper.proposalEntries)
      .find(_.members.exists(_.entry.request.id == renewed.request.id)).get
    assertEquals(renewedIdea.state, "withdrawn")
    assert(renewedIdea.text.contains("Withdrawn as a current explanation"))
    assertEquals(renewedIdea.priority(comparisons(renewed).find(_.id == "sacrifice_decline").get), None)
    deeper match
      case next: AwaitingEngineWork =>
        assert(!next.issuedWork.searchAnchor.exists(_.prefixMoves.startsWith(line.take(2))),
          "after the engine replaces acceptance, the old accepted branch does not take the remaining slot")
      case _ => ()
    val retiredIds = renewedIdea.members.map(_.entry.request.id).toSet
    assert(!CommentaryIdea.currentQuestions(deeper.proposalEntries).exists(entry => retiredIds(entry.request.id)),
      "shared current work may refresh old readers, but the retired idea cannot request its own follow-up")
    val packet = deeper.observationsPacket.get
    assertEquals(CommentaryProposal.comparisonCandidates(packet, ObservedLineProtocol.prepare(packet), List(renewed)), Nil)
    assertEquals(deeper.assessmentNeedsRefresh, false)
    val assessment = deeper match
      case complete: CompletedCommentaryJob => complete
      case _ => deeper.assessment.get
    assert((RuntimeProtocol.completedResultJson(assessment) \\ "verdict_code").contains(JsString("blunder")),
      "the refreshed root judgment must change along with the question and withdrawn story")

  test("an automatically investigated decline answers the sacrifice question with its actual side and receipt"):
    val (child, decline) = acceptanceWork(observed())
    val returned = submit(child, List(line.tail -> -150, List(decline) -> 100))
    val answered = returned.proposalEntries.filter(_.request.origin.question.producer == "C-sacrifice").flatMap { entry =>
      comparisons(entry).filter(compare => compare.prefixMovesUci == List("f1f7") &&
        compare.alternativeMove == decline).map(compare => entry -> reading(entry, compare.id))
    }.find(_._2.answer.nonEmpty).getOrElse(fail("the independently returned decline must join the loss question and reuse its answer"))
    assertEquals(answered._2.answer.map(_.evaluationRelation), Some("worse"))
    assertEquals(answered._2.answer.map(_.workId), Some(child.issuedWork.workId))
    assert(text(answered._1).contains(s"$decline evaluates below g8f7 for the side to move at the position after f1f7"))
    assert(text(answered._1).contains("do not by themselves prove adequate compensation"))

  test("a quiet check is an observed possible return even without a capture at that ply"):
    val checking = line.take(2) :+ "d3g6"
    val entry = sacrifice(observed(rows = roots.updated(0, checking -> -150)), checking)
    assertEquals(reading(entry, "sacrifice_check_2").state, "supported")
    assert(text(entry).contains("check against black is present"))
    assert(!entry.request.requirements.exists(_.id == "sacrifice_gain_2"))

  test("an exact ordinary root recapture accounts for the preceding gain instead of inventing a sacrifice"):
    val moves = List("d8d5", "b1c3", "d5d8")
    val delayed = List("g8f6", "d2d4", "f6d5")
    val state = observed(chess.variant.Standard.initialFen.value,
      List(moves -> 0, delayed -> -20, List("d8d6") -> -30), List("e2e4", "d7d5", "e4d5"))
    assert(!state.proposalEntries.exists(entry => entry.request.origin.question.producer == "C-sacrifice" &&
      entry.request.requirements.exists { case fact: Fact =>
        Set[PathSource](Declared(moves), Declared(delayed))(fact.path.source)
        case _ => false }))

  test("a material exchange recovered on the next own move is not a deferred compensation story"):
    val moves = List("d1d7", "d8d7")
    val state = observed("3r2k1/3r4/8/8/8/8/8/3R2K1 w - - 0 1",
      List(moves -> 0, List("d1e1") -> -20, List("g1f1") -> -30))
    assert(!state.proposalEntries.exists(entry => entry.request.origin.question.producer == "C-sacrifice" &&
      entry.request.requirements.exists { case fact: Fact => fact.path.source == Declared(moves); case _ => false }))

  test("a recovered early exchange does not steal the question from a later lasting loss"):
    val moves = List("d4e3", "d2e3", "d8d1", "g1h2")
    val state = observed("3r2k1/8/8/8/3p4/4P3/3B1P2/3R2K1 b - - 0 1",
      List(moves -> -200, List("d8e8") -> -150, List("g8h7") -> -100))
    val entry = sacrifice(state, moves)
    assert(entry.questionText.get.contains("beginning with d8d1"))
    assert(comparisons(entry).exists(compare => compare.prefixMovesUci == moves.take(2) && compare.referenceMove == "d8d1"))

  test("a short child answer leaves its compensation conditions unresolved even when the root still shows them"):
    val (child, decline) = acceptanceWork(observed())
    val returned = submit(child, List(List("g8f7") -> -150, List(decline) -> 100))
    val entry = returned.proposalEntries.filter(_.request.origin.question.producer == "C-sacrifice").find(entry =>
      comparisons(entry).exists(compare => compare.prefixMovesUci == List("f1f7") && compare.alternativeMove == decline &&
        reading(entry, compare.id).answer.nonEmpty)).get
    assertEquals(reading(entry, "sacrifice_current").state, "supported")
    val replyGroup = entry.request.requirements.collectFirst {
      case group: FactGroup if group.id.startsWith("sacrifice_reply_") => group
    }.get
    assert(!isResolved(reading(entry, replyGroup.id)))
    assert(entry.generatedArms.exists(_.headRequirementIds.contains(replyGroup.id + "_context")))
    val packet = returned.observationsPacket.get
    val candidates = CommentaryProposal.comparisonCandidates(packet, ObservedLineProtocol.prepare(packet), List(entry))
    assert(candidates.exists(value => (value \ "selection").as[String] == "selected" &&
      (value \ "detail" \ "prefix_moves_uci").as[List[String]].isEmpty),
      "an unanswered child continuation must not hide the still-current answered root comparison")
    assert(text(entry).contains("Unresolved conditions:"))
    assert(text(entry).contains("g8f7 from the position after f1f7"))
    assert(text(entry).contains("has not established"))
