package io.chesstory.runtime

import lila.chessjudgment.model.judgment.{ CommentaryLineSource, EvaluationSource, EvidenceBackedJudgmentPacket, SuppliedCommentaryLine }
import lila.chessjudgment.model.line.CanonicalPositionHistory
import lila.chessjudgment.analysis.assembly.*
import play.api.libs.json.*

/** Supplied legal examples and invented reports test investigation behaviour, not engine discovery or strength. */
class LineContinuationJobTest extends munit.FunSuite:
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val anand = "2rq1rk1/p3bppp/bpn1p3/1P1n4/3P4/P1N2N2/1BB2PPP/R2QR1K1 b - - 0 15"
  private val line = List("d5c3", "d1d3", "g7g6", "b2c3", "d8c7", "b5a6", "c6a5", "c3a5")

  private def start(history: CanonicalPositionHistory, actual: String, supplied: List[List[String]],
      maximumLocalExpansions: Int = CommentaryProposal.MaximumLocalExpansions): AwaitingEngineWork =
    CommentaryJobReducer.startJob(history.initialFen, history.movePrefixUci, history.currentFen, actual,
      history.extend(List(actual)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true,
        maximumLocalExpansions = maximumLocalExpansions), 0L,
      supplied.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).toOption.get.asInstanceOf[AwaitingEngineWork]

  private def submit(state: AwaitingEngineWork, rows: List[(List[String], Int)]): CommentaryJobState =
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile,
      state.issuedWork.workId, state.issuedWork.executionKeySha256,
      rows.map((moves, cp) => ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16))), 1L)
      .toOption.get

  private def supportChoice(board: String, donor: List[String]): AwaitingEngineWork =
    val history = CanonicalPositionHistory.from(board, Nil, board).toOption.get
    assert(history.extend(donor).isRight)
    submit(start(history, "g1h1", Nil), List(donor -> 100,
      List("g1h2") -> 90, List("d2c4") -> 80)).asInstanceOf[AwaitingEngineWork]

  private def begun(): AwaitingEngineWork =
    val history = CanonicalPositionHistory.from(anand, Nil, anand).toOption.get
    submit(start(history, line.head, Nil), List(List(line.head) -> -50,
      List("a6b7") -> -40, List("c6b8") -> -30)).asInstanceOf[AwaitingEngineWork]

  private def deeper(): AwaitingEngineWork =
    val first = begun()
    assertEquals(first.issuedWork.searchAnchor.get.prefixMoves, List("d5c3"))
    submit(first, List(line.tail -> 120, List("b2c3", "a6b5") -> 10, List("b5c6") -> 0))
      .asInstanceOf[AwaitingEngineWork]

  test("one immediate recovery does not preempt its opponent reply but an observed alternative preserves the choice"):
    val initial = chess.variant.Standard.initialFen.value
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(List(
      "e2e4", "e7e5", "d2d4", "e5d4", "c2c3", "d4c3", "f1c4", "c3b2",
      "c1b2", "d7d5", "c4d5", "g8f6")).toOption.get
    val queen = List("d5f7", "e8f7", "d1d8")
    val alternative = queen.take(2) :+ "d1h5"
    assert(history.extend(queen).isRight)
    assert(history.extend(alternative).isRight)
    def afterReply(supplied: List[List[String]]): AwaitingEngineWork =
      val first = submit(start(history, queen.head, supplied), List(queen -> 0,
        List("b1c3") -> -10, List("g1f3") -> -20)).asInstanceOf[AwaitingEngineWork]
      // The opponent's choice before the observed queen gain remains a concrete question.
      assertEquals(first.issuedWork.searchAnchor.get.prefixMoves, queen.take(1))
      submit(first, List(queen.tail -> 0, List("e8e7") -> 10)).asInstanceOf[AwaitingEngineWork]
    val single = afterReply(Nil)
    val immediate = single.continuationInquiries.find(_.question.prefixMoves == queen.take(2)).get
    assertEquals(immediate.question.kinds, List("other_target_gain"))
    assertEquals(immediate.question.observedMoves, List("d1d8"))
    assertEquals(immediate.state, "deferred")
    assertEquals(single.issuedWork.searchAnchor.get.prefixMoves, queen)
    assert(single.continuationInquiries.exists(inquiry => inquiry.question.prefixMoves == queen &&
      inquiry.question.kinds.contains("material_followup")))
    assertEquals(single.responseSearches.last.lines.head.evaluation.moves.head, "e8f7")
    val stopped = CommentaryJobReducer.cancelJob(single).asInstanceOf[CompletedCommentaryJob]
    assert(stopped.continuationInquiries.exists(inquiry => inquiry.question.id == immediate.question.id &&
      inquiry.state == "deferred" && inquiry.question.observedMoves.contains("d1d8")))
    // This author-provided extra choice is a different input, not a newly discovered engine alternative.
    val multiple = afterReply(List(alternative))
    assertEquals(multiple.issuedWork.searchAnchor.get.prefixMoves, queen.take(2))
    val choice = multiple.continuationInquiries.find(_.question.prefixMoves == queen.take(2)).get
    assertEquals(choice.question.observedMoves.toSet, Set("d1d8", "d1h5"))
    assertEquals(choice.state, "awaiting_engine")

  test("one observed king constraint discovers the recapture choice and supplements every omitted recapturer"):
    val fen = "7k/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val prefix = List("g1h1", "d5e4")
    val suffix = List("h8g7", "h1g2", "g7g6", "g2h2")
    val knight = prefix ++ List("d2e4") ++ suffix
    val pawn = prefix ++ List("f3e4") ++ suffix
    // Only the knight line is present in an invented root receipt. No question or alternative path is supplied.
    val survey = supportChoice(fen, knight)
    assertEquals(survey.issuedWork.searchAnchor.get.prefixMoves, prefix)
    assertEquals(survey.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val inquiry = survey.continuationInquiries.find(_.state == "awaiting_engine").get
    assert(inquiry.question.kinds.contains("replacement_function_choice"))
    assert(List("d2e4", "f3e4").forall(inquiry.question.legalMoves.contains))
    val donor = survey.continuationReadings.find(_.path.moves == knight).get
    assert(inquiry.question.triggerOccurrenceIds.contains(donor.path.ids(6)))
    val support = donor.supportComparisons.find(row => row.kind == "replacement_after_capture").get
    val target = support.targets.find(_.originalTarget.square.key == "g5").get.originalTarget
    assert(donor.supportApplications.exists((row, original, point) =>
      row == support && original == target && point.count == 7 && !point.available))
    val probe = submit(survey, List(List("h1h2") -> 300, List("f3f4") -> 200,
      List("g5g6") -> 100)).asInstanceOf[AwaitingEngineWork]
    assertEquals(probe.issuedWork.searchAnchor.get.prefixMoves, prefix)
    assertEquals(probe.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List("d2e4", "f3e4", "h1h2")))
    val awaiting = probe.continuationInquiries.find(_.question.id == inquiry.question.id).get
    assertEquals(awaiting.state, "surveyed")
    assertEquals(awaiting.routeChallenge.get.state, "awaiting_engine")
    val classes = awaiting.routeChallenge.get.moveClasses.filter(_.kind == "replacement_recapture")
    assertEquals(classes.map(_.movesUci).toSet, Set(List("d2e4"), List("f3e4")))
    assert(classes.forall(_.sampledWorkIds.isEmpty))
    val shortDone = submit(probe, List(List("h1h2") -> 300, List("d2e4") -> 200, List("f3e4") -> 100))
      .asInstanceOf[CompletedCommentaryJob]
    val shortInquiry = shortDone.continuationInquiries.find(_.question.id == inquiry.question.id).get
    assertEquals(shortInquiry.routeChallenge.get.state, "compared") // The engine samples are present, the use is not.
    val shortSearch = shortDone.responseSearches.find(_.work.workId == probe.issuedWork.workId).get
    assert(shortInquiry.unresolvedSourceLineIds.contains(shortSearch.pathsByMove("f3e4").line.ref.id))
    assert(!shortInquiry.unresolvedSourceLineIds.contains(shortSearch.pathsByMove("d2e4").line.ref.id))
    // The existing longer exact knight path is reused; it is never pasted onto the pawn path.
    val deferredUse = shortDone.continuationInquiries.find(_.question.kinds.contains("support_application_not_observed")).get
    assertEquals(deferredUse.question.prefixMoves, pawn.take(3))
    assertEquals(deferredUse.state, "deferred")
    assertEquals(deferredUse.stopReason, Some("investigation_limit"))
    assert(shortInquiry.followupQuestionIds.contains(deferredUse.question.id))
    assert(deferredUse.question.comparisonSources.forall(source => source.kind == "support_application" &&
      source.workId == probe.issuedWork.workId && source.observationLineId.exists(id =>
        shortDone.continuationReadings.exists(reading => reading.path.line.ref.id == id && reading.path.moves == knight))))
    val done = submit(probe, List(List("h1h2") -> 300, knight.drop(2) -> 200, pawn.drop(2) -> 100))
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.progress.physicalWorksIssued, 3)
    val finished = done.continuationInquiries.find(_.question.id == inquiry.question.id).get
    assertEquals(finished.routeChallenge.get.state, "compared")
    assert(finished.routeChallenge.get.moveClasses.filter(_.kind == "replacement_recapture")
      .forall(group => group.comparison.exists(_.workId == probe.issuedWork.workId)))
    val work = done.responseSearches.find(_.work.workId == probe.issuedWork.workId).get
    val recapturePair = work.selectedMovePairs(List("d2e4", "f3e4")).head
    assert(!work.selectedLinePairs.contains(recapturePair)) // The full work's leader is a quiet move.
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("8" * 32,
      "replacement-function", 10000L, done))
    val comparisons = (wire \ "observations" \ "selection" \ "candidates").as[List[JsObject]].filter(candidate =>
      (candidate \ "detail" \ "type").asOpt[String].contains("line_continuation_comparison"))
    val pairIds = Set(recapturePair._1.line.ref.id, recapturePair._2.line.ref.id)
    assert(comparisons.exists(candidate => (candidate \ "line_ids").as[List[String]].toSet == pairIds &&
      (candidate \ "detail" \ "differences").as[List[JsObject]].exists(row =>
        (row \ "left" \ "application" \ "kind").asOpt[String].contains("king_capture"))))

  test("the remaining slot surveys an unobserved support use at the exact recapture peer frontier"):
    val fen = "7k/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val prefix = List("g1h1", "d5e4")
    val suffix = List("h8g7", "h1g2", "g7g6", "g2h2")
    val knight = prefix ++ List("d2e4") ++ suffix
    val survey = supportChoice(fen, knight)
    val frontier = submit(survey, List(List("h1h2") -> 300, List("d2e4") -> 200, List("f3e4") -> 100))
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(frontier.issuedWork.searchAnchor.get.prefixMoves, prefix :+ "f3e4")
    assertEquals(frontier.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val question = frontier.continuationInquiries.find(_.state == "awaiting_engine").get.question
    assert(question.kinds.contains("support_application_not_observed"))
    assertEquals(question.comparisonSources.size, 1)
    val premise = question.comparisonSources.head
    assertEquals(premise.workId, survey.issuedWork.workId)
    assertEquals(premise.kind, "support_application")
    assert(frontier.continuationReadings.exists(reading => premise.observationLineId.contains(reading.path.line.ref.id) &&
      reading.path.moves == knight))
    assert(!question.sourceLineIds.exists(premise.observationLineId.contains)) // A different branch is a premise, not a peer fact.
    val done = submit(frontier, List(suffix -> 0, List("h8g8") -> 10, List("h8h7") -> 20))
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.progress.physicalWorksIssued, 3)
    val read = done.continuationInquiries.find(_.question.id == question.id).get
    assertEquals(read.state, "surveyed")
    val childSearch = done.responseSearches.find(_.work.workId == frontier.issuedWork.workId).get
    assert(!read.unresolvedSourceLineIds.contains(childSearch.pathsByMove("h8g7").line.ref.id))
    assert(read.unresolvedSourceLineIds.contains(childSearch.pathsByMove("h8g8").line.ref.id))
    assert(read.unresolvedSourceLineIds.contains(childSearch.pathsByMove("h8h7").line.ref.id))
    assertEquals(done.continuationInquiries.filter(_.question.kinds.contains("support_application_not_observed"))
      .map(_.question.prefixMoves), List(prefix :+ "f3e4")) // No recursive questions at the next short endpoints.

  test("a geometric support snapshot without a later king opportunity does not invent a recapture inquiry"):
    val fen = "7k/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val state = submit(start(history, "g1h1", Nil), List(List("g1h1", "d5e4", "d2e4") -> 100,
      List("g1h2") -> 90, List("d2c4") -> 80))
    assert(!state.continuationInquiries.exists(_.question.kinds.contains("replacement_function_choice")))

  test("an actual support recapture discovers its earlier provider choice but a target loss alone leaves the peer unfinished"):
    val fen = "6rk/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val prefix = List("g1h1", "d5e4")
    val donor = prefix ++ List("d2e4", "g8g5", "e4g5")
    val survey = supportChoice(fen, donor)
    assertEquals(survey.issuedWork.searchAnchor.get.prefixMoves, prefix)
    val question = survey.continuationInquiries.find(_.state == "awaiting_engine").get.question
    assert(question.kinds.contains("replacement_function_choice"))
    val reading = survey.continuationReadings.find(_.path.moves == donor).get
    assert(reading.directSupportRecaptures.exists(use => use.originalTarget.square.key == "g5" &&
      use.recapture.occurrenceId == reading.path.ids.last))
    assertEquals(LineContinuationInvestigation.refutingMoveClasses(survey.observationsPacket.get,
      prefix, question.sourceLineIds, question.triggerOccurrenceIds, survey.preparedPaths.get)
      .filter(_.kind == "replacement_recapture").map(_.movesUci).toSet,
      Set(List("d2e4"), List("f3e4")))
    val frontier = submit(survey, List(List("h1h2") -> 300, List("d2e4") -> 200,
      List("f3e4") -> 100)).asInstanceOf[AwaitingEngineWork]
    assertEquals(frontier.issuedWork.searchAnchor.get.prefixMoves, prefix :+ "f3e4")
    assertEquals(frontier.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val pending = frontier.continuationInquiries.find(_.state == "awaiting_engine").get.question
    assert(pending.kinds.contains("support_application_not_observed"))
    assert(pending.comparisonSources.exists(_.observationLineId.contains(reading.path.line.ref.id)))
    val short = submit(frontier, List(List("g8g5") -> -100, List("h8h7") -> 10,
      List("g8g7") -> 20)).asInstanceOf[CompletedCommentaryJob]
    val shortSearch = short.responseSearches.find(_.work.workId == frontier.issuedWork.workId).get
    val shortInquiry = short.continuationInquiries.find(_.question.id == pending.id).get
    assertEquals(shortInquiry.state, "surveyed")
    assert(shortInquiry.unresolvedSourceLineIds.contains(shortSearch.pathsByMove("g8g5").line.ref.id))
    // The pawn provider cannot recapture on g5; a later observed Nd2-f3xg5 can.
    // This legal but deliberately cooperative continuation is not a claim about the rook's best play.
    val otherProvider = List("g8g5", "d2f3", "h8h7", "f3g5")
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    assert(history.extend(prefix ++ List("f3e4") ++ otherProvider).isRight)
    val observed = submit(frontier, List(otherProvider -> 100, List("h8h7") -> 10,
      List("g8g7") -> 20)).asInstanceOf[CompletedCommentaryJob]
    val observedSearch = observed.responseSearches.find(_.work.workId == frontier.issuedWork.workId).get
    val observedInquiry = observed.continuationInquiries.find(_.question.id == pending.id).get
    assert(!observedInquiry.unresolvedSourceLineIds.contains(observedSearch.pathsByMove("g8g5").line.ref.id))
    assert(observedInquiry.unresolvedSourceLineIds.contains(observedSearch.pathsByMove("h8h7").line.ref.id))
    assertEquals(observed.progress.physicalWorksIssued, 3)
    assertEquals(observed.continuationInquiries.count(_.question.kinds.contains("support_application_not_observed")), 1)

  test("an omitted exposed capture does not consume the survey reserved for the other selected arm"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val bishop = List("f1d3", "d5e4", "d3e4")
    val pawn = List("d2d3", "d5e4", "d3e4")
    assert(history.extend(bishop ++ List("d8d2", "d1d2")).isRight)
    val first = submit(start(history, bishop.head, Nil), List(pawn -> 100, bishop -> 90,
      List("e4d5") -> 80)).asInstanceOf[AwaitingEngineWork]
    assertEquals(first.issuedWork.searchAnchor.get.prefixMoves, bishop)
    val firstQuestion = first.continuationInquiries.find(_.state == "awaiting_engine").get.question
    assert(firstQuestion.kinds.contains("capture_exposure_response"))
    val otherArm = submit(first, List(List("d8d7") -> -10, List("d8d6") -> 0,
      List("g8h8") -> 10)).asInstanceOf[AwaitingEngineWork]
    assertEquals(otherArm.issuedWork.searchAnchor.get.prefixMoves, pawn)
    assertEquals(otherArm.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val missing = otherArm.continuationInquiries.find(_.question.id == firstQuestion.id).get.routeChallenge.get
    assertEquals(missing.state, "not_returned")
    assert(missing.moveClasses.exists(group => group.kind == "exposed_capture" &&
      group.movesUci == List("d8d2") && group.sampledWorkIds.isEmpty))
    val done = submit(otherArm, List(List("d8d1") -> -900, List("d8d7") -> 0,
      List("d8d6") -> 10)).asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.progress.physicalWorksIssued, 3)
    val retained = done.continuationInquiries.find(_.question.id == firstQuestion.id).get.routeChallenge.get
    assertEquals(retained.state, "not_returned")
    assertEquals(retained.stopReason, Some("investigation_limit"))
    val search = done.responseSearches.find(_.work.workId == otherArm.issuedWork.workId).get
    val queenLoss = search.pathsByMove("d8d1")
    val response = done.continuationInquiries.find(_.question.positionId == otherArm.issuedWork.searchAnchor.get.positionId).get
    assert(response.unresolvedSourceLineIds.contains(queenLoss.line.ref.id))
    assert(done.continuationReadings.find(_.path eq queenLoss).get.events.exists(event =>
      event.originalVictim.exists(_.role.name == "queen")))

  test("an authored conditional exposure remains evidence without outranking a currently selected exposure"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val authored = List("f1d3", "d5e4", "d3e4")
    val selected = List("d2d3", "d5e4", "d3e4")
    val awaiting = submit(start(history, authored.head, List(authored)), List(selected -> 100,
      List("f1d3", "d8d7") -> 90, List("e4d5") -> 80)).asInstanceOf[AwaitingEngineWork]
    assertEquals(awaiting.issuedWork.searchAnchor.get.prefixMoves, selected)
    assert(awaiting.continuationReadings.exists(reading => reading.path.moves == authored &&
      reading.captureExposures.exists(_.captures.exists(_.move == "d8d2"))))
    assert(awaiting.continuationInquiries.exists(inquiry => inquiry.question.prefixMoves == authored &&
      inquiry.question.kinds.contains("capture_exposure_response") && inquiry.state == "deferred"))

  test("a recovery in the current root PV directs investigation to its threatened stationary victim"):
    val history = CanonicalPositionHistory.from(anand, Nil, anand).toOption.get
    val next = submit(start(history, line.head, Nil), List(line -> -50,
      List("a6b7") -> -40, List("c6b8") -> -30)).asInstanceOf[AwaitingEngineWork]
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, line.take(2))
    val q = next.continuationInquiries.find(_.state == "awaiting_engine").get.question
    val prepared = next.preparedPaths.get
    val challenges = LineContinuationInvestigation.refutingMoves(next.observationsPacket.get,
      q.prefixMoves, q.sourceLineIds, q.triggerOccurrenceIds, prepared)
    assert(challenges.contains("c3b5")) // Moves the knight before its recorded capture on c3.

  test("a supplied Anand suffix beyond a short root PV follows the current root capture reply"):
    val history = CanonicalPositionHistory.from(anand, Nil, anand).toOption.get
    val next = submit(start(history, line.head, List(line)), List(List(line.head) -> -50,
      List("a6b7") -> -40, List("c6b8") -> -30)).asInstanceOf[AwaitingEngineWork]
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, line.take(1))
    val conditional = next.continuationInquiries.find(_.question.prefixMoves == line.take(2)).get
    assertEquals(conditional.state, "deferred")
    val relation = CommentaryJobReducer.investigationPrefixRelation(next, line.head)
    assertEquals(relation(conditional.question.prefixOccurrenceIds), 1)

  test("a supplied cooperative support use cannot outrank current replies and a child survey supersedes the old short prefix"):
    val fen = "6rk/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val donor = List("g1h1", "d5e4", "d2e4", "g8g5", "e4g5")
    val first = submit(start(history, "g1h1", List(donor)), List(List("g1h2") -> 110,
      List("g1h1") -> 100, List("d2c4") -> 90)).asInstanceOf[AwaitingEngineWork]
    assertEquals(first.issuedWork.searchAnchor.get.prefixMoves, List("g1h1"))
    assert(first.responseInquiries.exists(inquiry => inquiry.question.rootMove == "g1h1" &&
      inquiry.question.captureMoves.contains("g8g5")))
    val support = first.continuationInquiries.find(_.question.kinds.contains("replacement_function_choice")).get
    assertEquals(support.question.prefixMoves, donor.take(2))
    assertEquals(support.state, "deferred")
    val relation = CommentaryJobReducer.investigationPrefixRelation(first, "g1h2")
    assertEquals(relation(support.question.prefixOccurrenceIds), 1)
    val second = submit(first, List(List("g8g5") -> -100, List("g8g7") -> 0,
      List("h8h7") -> 10)).asInstanceOf[AwaitingEngineWork]
    assertEquals(second.issuedWork.searchAnchor.get.prefixMoves, List("g1h2"))
    val currentLoss = second.continuationInquiries.find(_.question.prefixMoves == List("g1h1", "g8g5")).get
    assert(currentLoss.question.kinds.contains("material_followup"))
    assert(currentLoss.question.legalMoves.contains("e4d5")) // A new current loss supplies an immediate legal recovery proposal.
    assertEquals(currentLoss.state, "deferred") // The reference arm still needs its first response survey.
    val updated = CommentaryJobReducer.investigationPrefixRelation(second, "g1h2")
    val retained = second.continuationInquiries.find(_.question.id == support.question.id).get
    assertEquals(updated(retained.question.prefixOccurrenceIds), 2)
    val search = second.responseSearches.find(_.work.workId == first.issuedWork.workId).get
    assertEquals(updated(search.pathsByMove("g8g5").ids), 0)
    assert(second.continuationReadings.exists(reading => reading.path.moves == donor &&
      reading.directSupportRecaptures.nonEmpty)) // Different from weak or impossible.

  test("a weak legal recovery proposal on the bad move does not starve the reference response and its actual recapture"):
    val fen = "6rk/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val defended = List("f3f4", "g8g5", "f4g5")
    assert(history.extend(defended).isRight)
    val first = submit(start(history, "g1h1", Nil), List(List("f3f4") -> 0,
      List("g1h1") -> -500, List("d2c4") -> -600)).asInstanceOf[AwaitingEngineWork]
    assertEquals(first.issuedWork.searchAnchor.get.prefixMoves, List("g1h1"))
    val reference = submit(first, List(List("g8g5") -> -500, List("d5e4") -> -100,
      List("g8g7") -> 0)).asInstanceOf[AwaitingEngineWork]
    assertEquals(reference.issuedWork.searchAnchor.get.prefixMoves, List("f3f4"))
    assertEquals(reference.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val proposal = reference.continuationInquiries.find(_.question.prefixMoves == List("g1h1", "g8g5")).get
    assert(proposal.question.kinds.contains("material_followup"))
    assert(proposal.question.legalMoves.contains("e4d5"))
    assertEquals(proposal.state, "deferred")
    val done = submit(reference, List(defended.tail -> 500, List("d5e4") -> 0,
      List("g8g7") -> 10)).asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.progress.physicalWorksIssued, 3)
    assert(done.continuationReadings.exists(reading => reading.path.moves == defended &&
      reading.captureLinks.nonEmpty))
    assert(done.responseInquiries.exists(inquiry => inquiry.question.rootMove == "f3f4" &&
      inquiry.question.originalTarget.square.key == "g5" && inquiry.answer.nonEmpty && inquiry.reading.exists(_.complete)))
    val deferred = done.continuationInquiries.find(_.question.id == proposal.question.id).get
    assertEquals(deferred.state, "deferred")
    assertEquals(deferred.stopReason, Some("investigation_limit"))

  test("an omitted exposure proposal leaves room for the reference arm's first complete reply survey"):
    val fen = "3r2k1/8/8/8/4P3/8/3N4/3Q3K w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    assert(history.extend(List("d2c4", "d8d1")).isRight)
    val defended = List("d1e1", "d8d2", "e1d2")
    assert(history.extend(defended).isRight)
    val first = submit(start(history, "d2c4", Nil), List(List("d1e1") -> 0,
      List("d2c4") -> -900, List("d2b3") -> -1000)).asInstanceOf[AwaitingEngineWork]
    assertEquals(first.issuedWork.searchAnchor.get.prefixMoves, List("d2c4"))
    val exposure = first.continuationInquiries.find(_.state == "awaiting_engine").get.question
    assert(exposure.kinds.contains("capture_exposure_response"))
    val reference = submit(first, List(List("d8d7") -> -800, List("d8d6") -> -790,
      List("g8h8") -> -780)).asInstanceOf[AwaitingEngineWork]
    assertEquals(reference.issuedWork.searchAnchor.get.prefixMoves, List("d1e1"))
    assertEquals(reference.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val unreturned = reference.continuationInquiries.find(_.question.id == exposure.id).get.routeChallenge.get
    assertEquals(unreturned.state, "not_returned")
    assert(unreturned.moveClasses.exists(group => group.kind == "exposed_capture" &&
      group.movesUci == List("d8d1") && group.sampledWorkIds.isEmpty))
    val done = submit(reference, List(defended.tail -> 400, List("g8h8") -> 0,
      List("d8d7") -> 10)).asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.progress.physicalWorksIssued, 3)
    assert(done.continuationReadings.exists(_.path.moves == defended))
    val missing = done.continuationInquiries.find(_.question.id == exposure.id).get.routeChallenge.get
    assertEquals(missing.state, "not_returned")
    assertEquals(missing.stopReason, Some("investigation_limit"))
    assert(!done.continuationReadings.exists(_.path.moves == List("d2c4", "d8d1")))

  test("a newly observed current Orton loss precedes the supplied deeper recovery"):
    val fen = "1rr3k1/4q1pp/p2pp3/4p1N1/BPn2nQ1/P5PP/1P3P2/1R2R1K1 b - - 0 27"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val donor = List("h7h5", "g4h4", "e7g5", "h4g5", "f4h3", "g1g2", "h3g5")
    val first = submit(start(history, "h7h5", List(donor)), List(List("h7h5") -> -100,
      List("g8h8") -> -90, List("e7d7") -> -80)).asInstanceOf[AwaitingEngineWork]
    assertEquals(first.issuedWork.searchAnchor.get.prefixMoves, List("h7h5"))
    val next = submit(first, List(List("g4h4") -> 100, List("g3f4") -> 90,
      List("g4f4") -> 80)).asInstanceOf[AwaitingEngineWork]
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, List("h7h5", "g3f4"))
    assertEquals(next.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val currentLoss = next.continuationInquiries.find(_.state == "awaiting_engine").get
    assert(currentLoss.question.kinds.contains("material_followup"))
    assert(currentLoss.question.legalMoves.contains("e7g5"))
    val question = next.continuationInquiries.find(_.question.prefixMoves == donor.take(3)).get.question
    assert(next.continuationInquiries.exists(inquiry => inquiry.question.id == question.id && inquiry.state == "deferred"))
    assertEquals(CommentaryJobReducer.investigationPrefixRelation(next, "h7h5")(question.prefixOccurrenceIds), 1)
    assert(next.continuationReadings.exists(_.path.moves == donor))

  test("a supplied support extension is investigated when current premises are absent without washing its provenance"):
    val fen = "7k/8/8/3p2P1/8/5P2/3NP3/6K1 w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val donor = List("e2e4", "d5e4", "d2e4", "h8g7", "g1g2", "g7g6", "g2h2")
    assert(history.extend(donor).isRight)
    List(List("e2e4") -> 1, List("e2e4", "h8g7") -> 2).foreach { (current, expectedRelation) =>
      val next = submit(start(history, "e2e4", List(donor)), List(current -> 100,
        List("g1h1") -> 90, List("d2c4") -> 80)).asInstanceOf[AwaitingEngineWork]
      assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, donor.take(2))
      assertEquals(next.responseInquiries, Nil)
      val inquiry = next.continuationInquiries.find(_.state == "awaiting_engine").get
      assert(inquiry.question.kinds.contains("replacement_function_choice"))
      assertEquals(CommentaryJobReducer.investigationPrefixRelation(next, "e2e4")(inquiry.question.prefixOccurrenceIds),
        expectedRelation)
      val frontier = submit(next, List(List("g1h1") -> 300, List("d2e4") -> 200,
        List("f3e4") -> 100)).asInstanceOf[AwaitingEngineWork]
      val pending = frontier.continuationInquiries.find(_.state == "awaiting_engine").get.question
      assertEquals(pending.prefixMoves, List("e2e4", "d5e4", "f3e4"))
      // A local survey certifies neither a previously unobserved reply nor a different current reply.
      assertEquals(CommentaryJobReducer.investigationPrefixRelation(frontier, "e2e4")(pending.prefixOccurrenceIds),
        expectedRelation)
    }

  test("an observed delayed removal investigates its earlier reply premise at an exact deeper prefix"):
    val second = deeper()
    val anchor = second.issuedWork.searchAnchor.get
    assertEquals(anchor.prefixMoves, List("d5c3", "d1d3"))
    assertEquals(second.issuedWork.enginePositionMovesUci, anchor.prefixMoves)
    assertEquals(second.issuedWork.commentaryLinePrefixUci, Nil)
    assertEquals(second.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val question = second.continuationInquiries.find(_.state == "awaiting_engine").get.question
    assertEquals(question.positionId, anchor.positionId)
    assert(question.legalMoves.contains("g7g6"))
    assert(question.legalMoves.contains("f7f5"))
    val done = submit(second, List(line.drop(2) -> 120, List("f7f5") -> 10, List("f7f6") -> 20))
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.progress.physicalWorksIssued, 3)
    assertEquals(done.responseSearches.map(_.anchor.prefixMoves).distinct.size, 2)
    assert(done.continuationInquiries.exists(inquiry => inquiry.question.id == question.id && inquiry.state == "surveyed"))
    assert(done.observationsPacket.get.assembly.input.evaluationObservations.forall(_.source != EvaluationSource.ResponseComparison))
    assertEquals(done.assessmentNeedsRefresh, false)
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("d" * 32, "deeper", 10000L, done))
    val evaluations = (wire \\ "conditional_evaluations").flatMap(_.as[List[JsObject]])
    assert(evaluations.exists(value => (value \ "ply_offset").as[Int] == 2 &&
      (value \ "search_position_id").as[String] == anchor.positionId))
    assert(done.continuationReadings.exists(_.path.moves == line))

    // Black's synthetic returned leader is ...f5 (10cp for White), so only
    // its selected pair with ...g6 supplies this missing continuation premise.
    val frontier = done.continuationInquiries.find(inquiry =>
      inquiry.question.prefixMoves == List("d5c3", "d1d3", "f7f5")).get
    assertEquals(frontier.state, "deferred")
    assertEquals(frontier.stopReason, Some("investigation_limit"))
    assertEquals(frontier.question.kinds, List("continuation_not_observed", "material_followup"))
    assert(frontier.question.legalMoves.contains("b2c3"))
    assert(frontier.question.legalMoves.contains("d3d2")) // A quiet reply remains in the unrestricted question.
    assertEquals(frontier.question.observedMoves, Nil)
    assertEquals(frontier.question.comparisonSources.size, 1)
    val premise = frontier.question.comparisonSources.head
    assertEquals(premise.workId, second.issuedWork.workId)
    val search = done.responseSearches.find(_.work.workId == premise.workId).get
    assert(search.selectedLinePairs.exists { (left, right) =>
      Set(left.line.ref.id, right.line.ref.id) == Set(premise.donorLineId, premise.peerLineId)
    })
    val donor = done.continuationReadings.find(_.path.line.ref.id == premise.donorLineId).get
    val peer = done.continuationReadings.find(_.path.line.ref.id == premise.peerLineId).get
    assertEquals(peer.path.moves, frontier.question.prefixMoves)
    assertEquals(donor.path.moves, line)
    assertEquals(frontier.question.triggerOccurrenceIds.toSet,
      Set(donor.path.ids.head, donor.path.ids(3), peer.path.ids.last))
    assert(!frontier.question.sourceLineIds.contains(premise.donorLineId))
    assert(frontier.question.sourceLineIds.contains(premise.peerLineId))
    assert(!done.continuationInquiries.exists(inquiry =>
      inquiry.question.prefixMoves == List("d5c3", "d1d3", "f7f6") &&
        inquiry.question.kinds.contains("continuation_not_observed"))) // No all-pairs donor transfer.
    val surveyed = done.continuationInquiries.find(_.question.id == question.id).get
    assert(surveyed.unresolvedSourceLineIds.contains(peer.path.line.ref.id))
    assert(surveyed.followupQuestionIds.contains(frontier.question.id))
    assert(frontier.unresolvedSourceLineIds.contains(peer.path.line.ref.id))
    val frontierWire = (wire \\ "continuation_inquiries").flatMap(_.as[List[JsObject]])
      .find(value => (value \ "id").as[String] == frontier.question.id).get
    assertEquals((frontierWire \ "comparison_sources").as[List[JsObject]].map(row =>
      (row \ "donor_line_id").as[String]), List(premise.donorLineId))

    def withObservedReply(rootReply: String) =
      val history = CanonicalPositionHistory.from(anand, Nil, anand).toOption.get
      val added = List("d5c3", "d1d3", rootReply, "b2c3")
      val supplied = List(line, List("d5c3", "d1d3", "f7f5"), List("d5c3", "d1d3", "f7f6"), added)
      val input = MoveReviewInputAdmission.admitIndependent(history, line.head,
        suppliedLines = supplied.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
      val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
      val prepared = ObservedLineProtocol.prepare(packet)
      val owned = TargetResponseInvestigation.readSearch(packet,
        CommentaryInvestigation.EngineCandidates(search.work, search.lines),
        history.extend(search.anchor.prefixMoves).toOption.get, prepared).get
      (packet, prepared, owned, prepared.certifiedPath(packet, added).get.line.ref.id)

    // A continuation from the other exact peer cannot discharge this missing observation.
    val (otherPacket, otherPrepared, otherSearch, otherLineId) = withObservedReply("f7f6")
    assert(LineContinuationInvestigation.unobservedContinuations(otherPacket, otherSearch, otherPrepared)
      .exists(_._2.prefixMovesUci == frontier.question.prefixMoves))
    val stillOpen = CommentaryJobReducer.refreshContinuationInquiries(otherPacket, otherPrepared,
      Nil, List(frontier), Nil).head
    assertEquals(stillOpen.state, "deferred")
    assert(!stillOpen.question.sourceLineIds.contains(otherLineId))

    // Rebind the old short receipt alongside an independently supplied exact extension.
    // Its next capture is known now, even though no full search of this frontier took place.
    val (knownPacket, knownPrepared, oldShortSearch, witnessLineId) = withObservedReply("f7f5")
    assert(!LineContinuationInvestigation.unobservedContinuations(knownPacket, oldShortSearch, knownPrepared)
      .exists(_._2.prefixMovesUci == frontier.question.prefixMoves))
    val observed = CommentaryJobReducer.refreshContinuationInquiries(knownPacket, knownPrepared,
      Nil, List(frontier), Nil).head
    assertEquals(observed.state, "observed")
    assertEquals(observed.searchWorkIds, Nil)
    assertEquals(observed.stopReason, None)
    assertEquals(observed.question.triggerOccurrenceIds, frontier.question.triggerOccurrenceIds)
    assertEquals(observed.question.comparisonSources, frontier.question.comparisonSources)
    assert(observed.question.sourceLineIds.contains(witnessLineId))
    assert(observed.question.observedMoves.contains("b2c3"))
    assertEquals(observed.unresolvedSourceLineIds, Nil)
    val extra = frontier.question.premises.head
    val mixed = frontier.copy(question = frontier.question.copy(premises = frontier.question.premises :+
      extra.copy(origin = extra.origin.copy(kind = "delayed_capturer_removal"))))
    assertEquals(CommentaryJobReducer.refreshContinuationInquiries(knownPacket, knownPrepared,
      Nil, List(mixed), Nil).head.state, "deferred")
    // Unavailable retained provenance cannot count as satisfying an additional premise.
    val unavailable = frontier.copy(question = frontier.question.copy(premises = frontier.question.premises :+
      extra.copy(origin = extra.origin.copy(comparisonSources =
        List(premise.copy(donorLineId = "unavailable-retained-donor"))))))
    val unresolved = CommentaryJobReducer.refreshContinuationInquiries(knownPacket, knownPrepared,
      Nil, List(unavailable), Nil).head
    assertEquals(unresolved.state, "deferred")
    assert(unresolved.question.sourceLineIds.contains(witnessLineId))
    assertEquals(unresolved.question.comparisonSources.size, 2)

  test("cancelling a deeper investigation retains earlier facts and the exact unanswered choice"):
    val active = deeper()
    val waiting = active.continuationInquiries.filter(_.state == "awaiting_engine").map(_.question.id).toSet
    assert(waiting.nonEmpty)
    val done = CommentaryJobReducer.cancelJob(active).asInstanceOf[CompletedCommentaryJob]
    assert(done.continuationInquiries.filter(inquiry => waiting(inquiry.question.id)).forall(inquiry =>
      inquiry.state == "stopped" && inquiry.stopReason.contains("cancelled")))
    assertEquals(done.responseSearches.map(_.work.workId), active.responseSearches.map(_.work.workId))
    assertEquals(done.continuationReadings.map(_.id).toSet, active.continuationReadings.map(_.id).toSet)
    assertEquals(done.progress.physicalReportsAccepted, 2)

  test("a material question tracks the other pawn independently of an unrelated attacked root target"):
    val initial = chess.variant.Standard.initialFen.value
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(List(
      "e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")).toOption.get
    val exchange = List("f3e5", "d8d4", "e5f3", "d4e4", "d1e2", "e4e2", "e1e2")
    val next = submit(start(history, "f3e5", List(exchange)), List(List("f3e5") -> 30,
      List("e1g1") -> 20, List("d2d4") -> 10)).asInstanceOf[AwaitingEngineWork]
    assertEquals(next.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, List("f3e5"))
    assert(next.continuationInquiries.exists(inquiry => inquiry.state == "awaiting_engine" &&
      inquiry.question.kinds.contains("other_target_gain")))
    // The newly opened d-file exposes Pd2 to Qd8. That older target query is not a question about Pe4 recovery.
    assert(!next.responseInquiries.exists(_.question.originalTarget.square.key == "e4"))
    assert(next.continuationInquiries.flatMap(_.question.observedMoves).contains("d8d4"))

  test("a bare root capture opens a reply investigation without a supplied future gain or attacked friendly target"):
    // Ruy-derived control diagram with White Qd1/Pd2 removed, not a recorded game.
    val fen = "r1bqkbnr/1pp2ppp/p1p5/4p3/4P3/5N2/PPP2PPP/RNB1K2R w KQkq - 0 5"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val next = submit(start(history, "f3e5", Nil), List(List("f3e5") -> 30,
      List("e1g1") -> 20, List("c2c3") -> 10)).asInstanceOf[AwaitingEngineWork]
    assert(next.responseInquiries.isEmpty)
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, List("f3e5"))
    val question = next.continuationInquiries.find(_.state == "awaiting_engine").get.question
    assert(question.kinds.contains("root_capture_response"))
    val prepared = next.preparedPaths.get
    def follows(path: ObservedLineProtocol.PreparedPath): Boolean =
      path.moves.startsWith(question.prefixMoves) && path.moves.size > question.prefixMoves.size
    assert(!prepared.investigationPaths.exists(follows), "no independent source supplied a future reply")
    val probed = prepared.paths.filter(path => path.certified.nonEmpty && path.ids == path.declared && follows(path))
    assertEquals(question.observedMoves.toSet, probed.map(_.moves(question.prefixMoves.size)).toSet)
    assert(probed.forall(path => next.observationsPacket.get.assembly.input.legalLines.exists(line =>
      line.moves == path.sourceMoves && line.discoveryOnly)))
    assertEquals(next.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assert(question.legalMoves.contains("d8d4"))

  test("short replies distinguish legal checking resources from endpoints with neither captures nor checks"):
    // Removing White Qd1/Pd2 avoids an unrelated capture but leaves ...Bb4+
    // and queen checks. The second constructed diagram also removes Black Qd8/Bf8.
    List("r1bqkbnr/1pp2ppp/p1p5/4p3/4P3/5N2/PPP2PPP/RNB1K2R w KQkq - 0 5" -> true,
      "r1b1k1nr/1pp2ppp/p1p5/4p3/4P3/5N2/PPP2PPP/RNB1K2R w KQkq - 0 5" -> false).foreach { (fen, checks) =>
      val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
      val first = submit(start(history, "f3e5", Nil), List(List("f3e5") -> 30,
        List("e1g1") -> 20, List("c2c3") -> 10)).asInstanceOf[AwaitingEngineWork]
      val rows = List(List("a6a5", "e5f3") -> 0, List("b7b6", "e5f3") -> 10,
        List("g7g6", "e5f3") -> 20)
      val next = submit(first, rows)
      val inquiry = next.continuationInquiries.find(_.question.prefixMoves == List("f3e5")).get
      assertEquals(inquiry.state, "surveyed")
      assertEquals(inquiry.searchWorkIds, List(first.issuedWork.workId))
      assertEquals(inquiry.unresolvedSourceLineIds.size, 3)
      val prepared = next.preparedPaths.get
      val paths = prepared.paths.filter(path => inquiry.unresolvedSourceLineIds.contains(path.line.ref.id))
      assertEquals(paths.map(_.moves).toSet, rows.map(row => "f3e5" :: row._1).toSet)
      paths.foreach { path =>
        val view = prepared.resourceViews(next.observationsPacket.get)(path.positions.last.id.get)
        assert(view.legalRows.exists(_.forall(_.capture.isEmpty)))
        assertEquals(view.checkingMoves.exists(_.nonEmpty), checks)
        if checks then
          assert(view.checkingMoves.exists(_.exists(_.move == "f8b4")))
          assert(history.extend(path.moves :+ "f8b4").toOption.get.currentPosition.check.yes)
      }
      val followups = next.continuationInquiries.filter(value => inquiry.followupQuestionIds.contains(value.question.id))
      if checks then
        assertEquals(followups.map(_.question.prefixMoves).toSet, paths.map(_.moves).toSet)
        assert(followups.forall(_.question.kinds.contains("material_followup")))
        assertEquals(followups.count(_.state == "awaiting_engine"), 1)
      else
        assertEquals(followups, Nil)
        assert(!next.continuationInquiries.exists(_.question.prefixMoves.size > 1))
    }

  test("a forced reply becomes a certified observation without an engine receipt or invented score"):
    val fen = "R4rk1/5ppp/3K4/8/8/8/8/8 w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    assertEquals(history.extend(List("a8f8")).toOption.get.currentLegalMoveUcis, List("g8f8"))
    // The single available local edge belongs to the already required reply, before C probes.
    val next = submit(start(history, "a8f8", Nil, maximumLocalExpansions = 1), List(List("a8f8") -> 0,
      List("a8a7") -> -10, List("d6c5") -> -20))
    val forced = next.continuationInquiries.find(_.question.prefixMoves == List("a8f8")).get
    assertEquals(forced.state, "forced")
    assertEquals(forced.question.observedMoves, List("g8f8"))
    assertEquals(forced.searchWorkIds, Nil)
    assertEquals(forced.unresolvedSourceLineIds, Nil)
    val reading = next.continuationReadings.find(_.path.moves == List("a8f8", "g8f8")).get
    assertEquals(reading.captureLinks.map(_.elapsedPlies), List(1))
    assertEquals(next.progress.physicalReportsAccepted, 1)
    assertEquals(next.localExpansionsUsed, 1)
    assert(next.responseSearches.isEmpty)
    assert(next.observationsPacket.get.assembly.input.evaluationObservations.forall(_.source != EvaluationSource.ResponseComparison))

  test("forced reply observation shares the local expansion limit and leaves its missing witness open"):
    val fen = "R4rk1/5ppp/3K4/8/8/8/8/8 w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val next = submit(start(history, "a8f8", Nil, maximumLocalExpansions = 0), List(List("a8f8") -> 0,
      List("a8a7") -> -10, List("d6c5") -> -20))
    assertEquals(next.localExpansionsUsed, 0)
    assert(!next.observationsPacket.get.assembly.input.legalLines.exists(_.moves == List("a8f8", "g8f8")))
    val stopped = CommentaryJobReducer.cancelJob(next)
    val forced = stopped.continuationInquiries.find(_.question.prefixMoves == List("a8f8")).get
    assertEquals(forced.question.observedMoves, Nil)
    assertEquals(forced.stopReason, Some("local_expansion_limit"))

  test("a deep anchor cannot bind a shallow or different-history receipt to a certified path"):
    val active = deeper()
    val anchor = active.issuedWork.searchAnchor.get
    val packet = active.observationsPacket.get
    val prepared = active.preparedPaths.get
    val prior = active.responseSearches.head
    val receipt = CommentaryInvestigation.EngineCandidates(prior.work.copy(searchAnchor = Some(anchor)), prior.lines)
    val history = CanonicalPositionHistory.from(anand, Nil, anand).toOption.get.extend(anchor.prefixMoves).toOption.get
    assertEquals(TargetResponseInvestigation.readSearch(packet, receipt, history, prepared), None)

  test("an omitted eight-move route class uses one complete pool and never mixes the old representative score"):
    val initial = chess.variant.Standard.initialFen.value
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(List(
      "e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")).toOption.get
    val donor = List("f3e5", "d8d4", "b1c3", "d4e5")
    val survey = submit(start(history, "f3e5", Nil), List(donor -> 50,
      List("e1g1") -> 40, List("d2d4") -> 30)).asInstanceOf[AwaitingEngineWork]
    assertEquals(survey.issuedWork.searchAnchor.get.prefixMoves, donor.take(2))
    val probe = submit(survey, List(donor.drop(2) -> 20, List("c2c3") -> 10, List("d2d3") -> 0))
      .asInstanceOf[AwaitingEngineWork]
    val inquiry = probe.continuationInquiries.find(_.question.prefixMoves == donor.take(2)).get
    val route = inquiry.routeChallenge.get
    val moves = List("e5c4", "e5c6", "e5d3", "e5d7", "e5f3", "e5f7", "e5g4", "e5g6")
    assertEquals(inquiry.state, "surveyed")
    assertEquals(route.state, "awaiting_engine")
    assertEquals(route.referenceMove, "b1c3")
    assertEquals(route.referenceWorkId, survey.issuedWork.workId)
    assertEquals(route.moveClasses.map(_.movesUci), List(moves))
    assertEquals(probe.issuedWork.rootRestriction, EngineRootRestriction.Restricted((moves :+ "b1c3").sorted))
    assertEquals(probe.issuedWork.multiPv, 3) // Nine allowed moves; no UCI truncation to two challengers.
    assertEquals(probe.progress.physicalWorksIssued, 3)

    val cancelled = CommentaryJobReducer.cancelJob(probe).asInstanceOf[CompletedCommentaryJob]
      .continuationInquiries.find(_.question.id == inquiry.question.id).get
    assertEquals(cancelled.state, "surveyed")
    assertEquals(cancelled.searchWorkIds, List(survey.issuedWork.workId))
    assertEquals(cancelled.routeChallenge.get.state, "stopped")
    assertEquals(cancelled.routeChallenge.get.stopReason, Some("cancelled"))

    // Synthetic top three all belong to the challenge class. R was requested but is absent.
    val withoutReference = submit(probe, List(List("e5f3") -> 100, List("e5d3") -> 90, List("e5g4") -> 80))
      .asInstanceOf[CompletedCommentaryJob]
    val sampled = withoutReference.continuationInquiries.find(_.question.id == inquiry.question.id).get.routeChallenge.get
    assertEquals(sampled.state, "sampled")
    assertEquals(sampled.stopReason, Some("investigation_limit"))
    assertEquals(sampled.referenceWorkId, survey.issuedWork.workId)
    assertEquals(sampled.moveClasses.head.sampledWorkIds, List(probe.issuedWork.workId))
    assertEquals(sampled.moveClasses.head.comparison, None)
    assertEquals(withoutReference.progress.physicalWorksIssued, 3)

    val compared = submit(probe, List(List("e5f3") -> 100, List("e5d3") -> 90, donor.drop(2) -> 0))
      .asInstanceOf[CompletedCommentaryJob]
    val answered = compared.continuationInquiries.find(_.question.id == inquiry.question.id).get
    assertEquals(answered.routeChallenge.get.state, "compared")
    val pair = answered.routeChallenge.get.moveClasses.head.comparison.get
    assertEquals(pair.workId, probe.issuedWork.workId)
    val search = compared.responseSearches.find(_.work.workId == pair.workId).get
    assertEquals(pair.referenceLineId, search.pathsByMove("b1c3").line.ref.id)
    assertEquals(pair.challengeLineId, search.pathsByMove("e5f3").line.ref.id)
    val reused = CommentaryJobReducer.readRouteChallenge(compared.observationsPacket.get, compared.preparedPaths.get,
      answered.question, compared.responseSearches, answered.routeChallenge).get
    assertEquals(reused, answered.routeChallenge.get)
    assertEquals(reused.missingMoves, Nil)
    // A named class can require R versus a nonleader. This does not change the discovery star.
    val named = search.representativePair("b1c3", List("e5d3")).get
    assertEquals(named._1.line.ref.id, search.pathsByMove("b1c3").line.ref.id)
    assertEquals(named._2.line.ref.id, search.pathsByMove("e5d3").line.ref.id)
    assert(!search.selectedLinePairs.exists((left, right) =>
      Set(left.line.ref.id, right.line.ref.id) == Set(named._1.line.ref.id, named._2.line.ref.id)))

    // A hypothetical later full-scope receipt refreshes R. This tests the pure
    // reader's ownership boundary; this job does not issue a third exploration.
    val originalWork = survey.issuedWork
    val newerWork = IssuedEngineWork.create(originalWork.engineProfile, "synthetic-later-full-survey",
      originalWork.purpose, originalWork.enginePositionInitialFen, originalWork.enginePositionMovesUci,
      originalWork.searchFen, EngineRootRestriction.Unrestricted, originalWork.commentaryLineOriginFen,
      originalWork.commentaryLinePrefixUci, originalWork.searchLimits, originalWork.maxSearchElapsedMs,
      originalWork.searchAnchor)
    val newer = TargetResponseInvestigation.readSearch(compared.observationsPacket.get,
      CommentaryInvestigation.EngineCandidates(newerWork, search.lines), history.extend(donor.take(2)).toOption.get,
      compared.preparedPaths.get).get
    val refreshed = CommentaryJobReducer.readRouteChallenge(compared.observationsPacket.get, compared.preparedPaths.get,
      answered.question, compared.responseSearches :+ newer, answered.routeChallenge).get
    assertEquals(refreshed.referenceWorkId, newerWork.workId)
    assertEquals(refreshed.referenceMove, "e5f3")
    assertEquals(refreshed.state, "sampled") // R itself is now a member, not a fabricated self comparison.
    assertEquals(refreshed.moveClasses.head.comparison, None)

    val alreadySampled = submit(survey, List(List("e5f3") -> 10, donor.drop(2) -> 20, List("c2c3") -> 0))
    val shared = alreadySampled.continuationInquiries.find(_.question.id == inquiry.question.id).get.routeChallenge.get
    assertEquals(shared.state, "compared")
    assertEquals(shared.moveClasses.head.sampledWorkIds, List(survey.issuedWork.workId))
    assertEquals(shared.moveClasses.head.comparison.get.workId, survey.issuedWork.workId)

  test("one sampled recovery class cannot hide another class at the same exact choice"):
    val fen = "1rr3k1/4q1pp/p2pp3/4p1N1/BPn2nQ1/P5PP/1P3P2/1R2R1K1 b - - 0 27"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    // Without ...h5 Qh4 the g-pawn is not pinned: both captures of Nf4 are legal.
    val donor = List("e7g5", "g4g5", "f4h3", "g1g2", "h3g5")
    val survey = submit(start(history, "e7g5", List(donor)), List(List("e7g5") -> -100,
      List("h7h5") -> -90, List("g8h8") -> -80)).asInstanceOf[AwaitingEngineWork]
    assertEquals(survey.issuedWork.searchAnchor.get.prefixMoves, List("e7g5"))
    val probe = submit(survey, List(List("h3h4") -> 100, donor.tail -> 90, List("a4b3") -> 80))
      .asInstanceOf[AwaitingEngineWork]
    val inquiry = probe.continuationInquiries.find(_.question.prefixMoves == List("e7g5")).get
    val route = inquiry.routeChallenge.get
    assertEquals(route.state, "awaiting_engine")
    assertEquals(route.referenceMove, "h3h4")
    assertEquals(route.moveClasses.map(_.movesUci).toSet,
      Set(List("g3f4", "g4f4"), List("g3f4", "g4f4", "h3h4")))
    val known = route.moveClasses.find(_.movesUci.contains("h3h4")).get
    val missing = route.moveClasses.find(group => !group.movesUci.contains("h3h4")).get
    assertEquals(known.sampledWorkIds, List(survey.issuedWork.workId))
    assertEquals(known.comparison, None) // R already samples this class; do not compare it to itself.
    assertEquals(missing.sampledWorkIds, Nil)
    assertEquals(probe.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List("g3f4", "g4f4", "h3h4")))
    val done = submit(probe, List(List("g3f4") -> 200, List("g4f4") -> 100, List("h3h4") -> 0))
      .asInstanceOf[CompletedCommentaryJob]
    val completed = done.continuationInquiries.find(_.question.id == inquiry.question.id).get.routeChallenge.get
    assertEquals(completed.state, "compared")
    assert(completed.moveClasses.forall(_.sampledWorkIds.nonEmpty))
    assertEquals(completed.moveClasses.find(group => !group.movesUci.contains("h3h4")).get.comparison.get.workId,
      probe.issuedWork.workId)
