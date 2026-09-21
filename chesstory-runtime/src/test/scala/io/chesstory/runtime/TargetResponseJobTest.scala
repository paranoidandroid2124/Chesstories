package io.chesstory.runtime

import lila.chessjudgment.model.judgment.{ CommentaryLineSource, EvaluationSource, SuppliedCommentaryLine }
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Invented reports test child/root ownership and bounded work, never engine strength. */
class TargetResponseJobTest extends munit.FunSuite:
  private val board = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
  private val profile = CommentaryEngineProfile.ServerAdmitted

  private def start(fen: String = board, actual: String = "d2d3", deadline: Long = 10000L,
      lines: List[List[String]] = Nil): AwaitingEngineWork =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    CommentaryJobReducer.startJob(fen, Nil, fen, actual,
      history.extend(List(actual)).toOption.get.currentFen,
      CommentaryJobPolicy(16, deadline, profile, investigateChoices = true), 0L,
      lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).toOption.get.asInstanceOf[AwaitingEngineWork]

  private def submit(state: AwaitingEngineWork, rows: List[(List[String], Int, Int)], now: Long = 1L): CommentaryJobState =
    val work = state.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile, work.workId,
      work.executionKeySha256, rows.map { (moves, cp, depth) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), depth)
      }), now).toOption.get

  private def rootThenSurvey(reference: String = "d2d3"): AwaitingEngineWork =
    val rootMoves = List(reference, if reference == "d2d3" then "f1d3" else "d2d3", "e4d5")
    val child = submit(start(), rootMoves.zip(List(50, 40, 30)).map { (move, cp) => (List(move), cp, 16) })
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(child.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(child.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    child

  private def finishRemainingWork(state: CommentaryJobState): CompletedCommentaryJob = state match
    case done: CompletedCommentaryJob => done
    case next: AwaitingEngineWork if next.issuedWork.purpose == EngineWorkPurpose.ResponseComparison =>
      assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, List("d2d3", "d5e4", "d3e4"))
      assert(next.continuationInquiries.exists(inquiry => inquiry.state == "awaiting_engine" &&
        inquiry.question.kinds.contains("capture_exposure_response")))
      submit(next, List((List("d8d1"), -900, 16), (List("d8d7"), 0, 16), (List("d8d6"), 10, 16)))
        .asInstanceOf[CompletedCommentaryJob]
    case next: AwaitingEngineWork =>
      assertEquals(next.issuedWork.purpose, EngineWorkPurpose.InvestigationComparison)
      val pair = next.issuedWork.rootRestriction match
        case EngineRootRestriction.Restricted(moves) => moves
        case _ => fail("root refinements remain exact comparisons")
      submit(next, pair.take(next.issuedWork.multiPv).map(move => (List(move), if move == pair.head then 50 else 10, 16)))
        .asInstanceOf[CompletedCommentaryJob]
    case _ => fail("unexpected state")

  private def review(state: CompletedCommentaryJob): CompletedMoveReview = state.result match
    case CompletedPositionCommentary.FocusedMoveReview(_, value) => value
    case _ => fail("expected the retained root assessment")

  private def selected(state: AwaitingEngineWork) = state.responseInquiries.find(_.state == "awaiting_engine").get

  private val childRows: List[(List[String], Int, Int)] =
    List((List("d5e4", "d3e4"), -300, 16), (List("d8d7"), 200, 16), (List("g8f7"), 300, 16))

  test("child Black scores bind full history and full-root paths without changing the root assessment"):
    val active = rootThenSurvey()
    val q = selected(active).question
    val work = active.issuedWork
    assertEquals(work.enginePositionInitialFen, board)
    assertEquals(work.enginePositionMovesUci, List(q.rootMove))
    assertEquals(work.commentaryLineOriginFen, work.searchFen)
    assertEquals(work.commentaryLinePrefixUci, Nil)
    assertEquals(work.searchAnchor, Some(EngineSearchAnchor(q.rootMove, q.rootMoveOccurrenceId, q.childPositionId)))
    val baseline = review(active.assessment.get)
    val done = finishRemainingWork(submit(active, childRows))
    assertEquals(review(done).bestMoveUci, baseline.bestMoveUci)
    assertEquals(done.assessmentNeedsRefresh, false)
    val inquiry = done.responseInquiries.find(_.question.id == q.id).get
    val answer = inquiry.answer.get
    assertEquals(answer.pair.evaluationRelation, "worse") // +200 is worse for the Black mover than -300.
    assertEquals(answer.searchPositionId, q.childPositionId)
    assertNotEquals(answer.pair.referenceOccurrenceId, q.rootMoveOccurrenceId)
    assertEquals((answer.pair.referenceEvaluation \ "source").as[String], "response_comparison")
    val packet = done.observationsPacket.get
    assert(packet.assembly.input.evaluationObservations.forall(_.source != EvaluationSource.ResponseComparison))
    val full = ObservedLineProtocol.prepare(packet).path(packet, answer.pair.referenceLineId).get
    assertEquals(full.moves, q.rootMove :: childRows.head._1)
    assert(inquiry.reading.exists(_.complete))
    assert(inquiry.reading.exists(_.belongsTo(packet)))
    val wire = RuntimeProtocol.observationsJson(packet, done.inquiries, done.pvComparisons,
      done.responseInquiries, done.preparedPaths, done.responseSearches)
    val conditional = (wire \\ "conditional_evaluations").flatMap(_.as[List[JsObject]])
    assert(conditional.exists(value => (value \ "search_position_id").as[String] == q.childPositionId &&
      (value \ "ply_offset").as[Int] == 1))
    assertEquals(done.progress.physicalWorksIssued, 3)

  test("cancel and the original deadline preserve root facts and leave the child unanswered"):
    val active = rootThenSurvey()
    val q = selected(active).question
    val cancelled = CommentaryJobReducer.cancelJob(active).asInstanceOf[CompletedCommentaryJob]
    val expired = CommentaryJobReducer.expireJob(active, 10000L).asInstanceOf[CompletedCommentaryJob]
    List(cancelled -> "cancelled", expired -> "deadline_exceeded").foreach { (done, reason) =>
      assertEquals(review(done).bestMoveUci, "d2d3")
      val inquiry = done.responseInquiries.find(_.question.id == q.id).get
      assertEquals(inquiry.answer, None)
      assertEquals(inquiry.state, "stopped")
      assertEquals(inquiry.stopReason, Some(reason))
      assertEquals(done.assessmentNeedsRefresh, false)
      assertEquals(done.progress.physicalReportsAccepted, 1)
    }
    val late = submit(active, childRows, now = 10000L).asInstanceOf[CompletedCommentaryJob]
    assertEquals(late.responseInquiries.find(_.question.id == q.id).get.answer, None)
    assertEquals(review(late).bestMoveUci, "d2d3")
    assertEquals(late.assessmentNeedsRefresh, false)
    assertEquals(late.progress.physicalReportsAccepted, 1)

  test("unequal or below-required child depth cannot manufacture a paired response answer"):
    val active = rootThenSurvey()
    val q = selected(active).question
    List(15, 17).foreach { depth =>
      val rows = childRows.updated(0, (childRows.head._1, -300, depth))
      val done = submit(active, rows).asInstanceOf[CompletedCommentaryJob]
      assertEquals(review(done).bestMoveUci, "d2d3")
      val inquiry = done.responseInquiries.find(_.question.id == q.id).get
      assertEquals(inquiry.answer, None)
      assertEquals(inquiry.stopReason, Some("invalid_engine_work_report"))
      assertEquals(done.assessmentNeedsRefresh, false)
    }

  test("an exact one-ply child terminal stays a conditional endpoint rather than a root result"):
    val fen = "8/k7/1R6/8/8/8/8/K7 w - - 0 1"
    val active = submit(start(fen, "a1b1"), List(
      (List("a1b1"), 50, 16), (List("a1a2"), 40, 16), (List("a1b2"), 30, 16))).asInstanceOf[AwaitingEngineWork]
    assertEquals(active.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    val q = selected(active).question
    assertEquals(q.referenceMove, "a7b6")
    assertEquals(q.alternativeMove, "a7a8")
    val inconsistent = submit(active, List((List(q.referenceMove), 999, 16), (List(q.alternativeMove), 100, 16)))
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(inconsistent.responseInquiries.find(_.question.id == q.id).get.answer, None)
    val done = finishRemainingWork(submit(active, List((List(q.referenceMove), 0, 16), (List(q.alternativeMove), 100, 16))))
    val answer = done.responseInquiries.find(_.question.id == q.id).get.answer.get
    assertEquals((answer.pair.referenceEvaluation \ "kind").as[String], "exact_automatic_terminal")
    assertEquals((answer.pair.referenceEvaluation \ "terminal" \ "kind").as[String], "insufficient_material")
    assertEquals(answer.pair.referenceDepth, None)
    assertEquals(review(done).bestMoveUci, "a1b1")
    assertEquals(done.assessmentNeedsRefresh, false)
    assert(done.observationsPacket.get.assembly.input.evaluationObservations.forall(_.source != EvaluationSource.ResponseComparison))

  test("a returned reply's new capture exposure shares the parent budget before new root hypotheses"):
    val active = rootThenSurvey("f1d3")
    assert(active.responseInquiries.map(_.question.rootMove).toSet == Set("d2d3", "f1d3"))
    val answeredId = selected(active).question.id
    val next = submit(active, childRows).asInstanceOf[AwaitingEngineWork]
    assertEquals(next.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, List("d2d3", "d5e4", "d3e4"))
    val done = finishRemainingWork(next)
    assertEquals(done.progress.physicalWorksIssued, 3) // Parent and deeper reply use the same two slots.
    assertEquals(done.progress.physicalReportsAccepted, 3)
    assertEquals(done.responseInquiries.count(_.answer.nonEmpty), 1)
    assert(done.responseInquiries.find(_.question.id == answeredId).get.answer.nonEmpty)
    assertEquals(done.responseSearches.size, 2)
    assert(done.inquiries.exists(row => row.state == "deferred" && row.stopReason.contains("investigation_limit")))

  test("a top-three class omission requests the entire missing class and does not compare scores across searches"):
    val active = rootThenSurvey()
    val q = selected(active).question
    val next = submit(active, List((List("d8d7"), 20, 16), (List("g8f7"), 30, 16),
      (List("d8c8"), 40, 16))).asInstanceOf[AwaitingEngineWork]
    assertEquals(next.issuedWork.rootRestriction, EngineRootRestriction.Restricted(q.captureMoves.sorted))
    assertEquals(next.issuedWork.multiPv, 1)
    val done = submit(next, List((List("d5e4", "d3e4"), -500, 16))).asInstanceOf[CompletedCommentaryJob]
    val inquiry = done.responseInquiries.find(_.question.id == q.id).get
    assertEquals(inquiry.state, "observed")
    assertEquals(inquiry.answer, None) // Both moves are evaluated, but not by the same work.
    assert(inquiry.reading.exists(_.complete))
    assertEquals(inquiry.question.referenceSelection.workId, Some(next.issuedWork.workId))
    assertEquals(inquiry.question.alternativeSelection.workId, Some(active.issuedWork.workId))
    assertEquals(done.responseSearches.size, 2)
    assertEquals(review(done).bestMoveUci, "d2d3")

  test("an omitted class cannot monopolize the other root arm's first survey"):
    val active = rootThenSurvey("f1d3")
    val next = submit(active, List((List("d8d7"), 20, 16), (List("g8f7"), 30, 16),
      (List("d8c8"), 40, 16))).asInstanceOf[AwaitingEngineWork]
    assertEquals(next.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(next.issuedWork.searchAnchor.get.rootMove, "f1d3")
    val done = submit(next, childRows).asInstanceOf[CompletedCommentaryJob]
    val actual = done.responseInquiries.find(_.question.rootMove == "d2d3").get
    assertEquals(actual.question.referenceSelection.source, "legal_seed")
    assertEquals(actual.state, "deferred")
    assert(actual.stopReason.contains("investigation_limit"))
    assertEquals(done.progress.physicalWorksIssued, 3)

  test("new representatives retain a complete older contrast without lending its completion to a new partial line"):
    val oldCapture = List("d2d3", "d5e4", "d3e4")
    val oldDecline = List("d2d3", "d8d7")
    val active = submit(start(lines = List(oldCapture, oldDecline)), List(
      (List("d2d3"), 50, 16), (List("f1d3"), 40, 16), (List("e4d5"), 30, 16)))
      .asInstanceOf[AwaitingEngineWork]
    val prior = selected(active)
    assert(prior.reading.exists(_.complete))
    assertEquals(active.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val next = submit(active, List((List("d5e4"), -300, 16), (List("g8f7"), -200, 16),
      (List("d8d7"), 200, 16)))
    val done = CommentaryJobReducer.cancelJob(next).asInstanceOf[CompletedCommentaryJob]
    val inquiry = done.responseInquiries.find(_.question.id == prior.question.id).get
    assertEquals(inquiry.question.alternativeMove, "g8f7")
    assert(inquiry.reading.exists(!_.complete))
    assert(inquiry.retainedReadings.exists(_.complete))
    assert(inquiry.retainedReadings.exists(_.id == prior.reading.get.id))
    val readingIds = inquiry.reading.toList.flatMap(_.lineIds).toSet
    assert(!inquiry.retainedReadings.flatMap(_.lineIds).forall(readingIds))
    assertEquals(inquiry.answer.get.pair.alternativeEvaluation.value("white_score").as[JsObject].value("value"), JsNumber(-200))
    assertEquals(review(done).bestMoveUci, "d2d3")

  test("several original targets at one exact child share one survey and a new quiet reply"):
    // Adams's published diagram: candidates and synthetic scores here are supplied, not engine-discovered.
    val fen = "2rq1rk1/p3bppp/bpn1p3/1P1n4/3P4/P1N2N2/1BB2PPP/R2QR1K1 b - - 0 15"
    val actual = "d5c3"
    val started = start(fen, actual, lines = List(List(actual, "b2c3", "a6b5")))
    val child = submit(started, List((List(actual), -50, 16), (List("a6b7"), -40, 16),
      (List("c6b8"), -30, 16))).asInstanceOf[AwaitingEngineWork]
    assertEquals(child.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assert(Set("a6", "c6").subsetOf(child.responseInquiries.map(_.question.originalTarget.square.key).toSet))
    val next = submit(child, List((List("d1d3", "g7g6", "b2c3"), 120, 16),
      (List("b5a6"), 10, 16), (List("b5c6"), 0, 16)))
    val refinement = next.asInstanceOf[AwaitingEngineWork]
    assertEquals(refinement.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(refinement.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(refinement.issuedWork.searchAnchor.get.prefixMoves, List(actual, "d1d3"))
    assertEquals(refinement.issuedWork.multiPv, 3) // The newly observed delay probes Black's intervening choice first.
    val done = CommentaryJobReducer.cancelJob(next).asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.responseSearches.size, 1)
    val contrasted = done.responseInquiries.filter(inquiry => Set("a6", "c6")(inquiry.question.originalTarget.square.key))
    assertEquals(contrasted.size, 2)
    assert(contrasted.forall(_.question.alternativeMove == "d1d3"))
    assert(contrasted.forall(_.answer.nonEmpty))
    assertEquals(contrasted.flatMap(_.question.alternativeSelection.workId).distinct,
      List(child.issuedWork.workId))
    assertEquals(review(done).bestMoveUci, actual)
