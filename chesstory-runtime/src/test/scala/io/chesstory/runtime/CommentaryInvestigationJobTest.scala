package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Deliberately invented scores exercise the contract, not chess strength. */
class CommentaryInvestigationJobTest extends munit.FunSuite:
  private val initial = chess.variant.Standard.initialFen.value
  private val owner = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private def start(played: String = "g1f3", deadline: Long = 10000L,
      proposals: List[CommentaryProposal.Request] = Nil): AwaitingEngineWork =
    CommentaryJobReducer.startJob(initial, Nil, initial, played,
      owner.extend(List(played)).toOption.get.currentFen,
      CommentaryJobPolicy(16, deadline, profile, investigateChoices = true), 0L, proposals = proposals).toOption.get
      .asInstanceOf[AwaitingEngineWork]

  private def submit(state: AwaitingEngineWork, lines: List[(String, Int)], now: Long = 1L,
      depth: Int = 16): CommentaryJobState =
    val work = state.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile, work.workId,
      work.executionKeySha256, lines.map { (move, score) =>
        ReportedEngineLineSuffix(List(move), ReportedWhiteEngineScore.Centipawns(score), depth)
      }), now).toOption.get

  private val root = List("e2e4" -> 30, "d2d4" -> 20, "g1f3" -> 10)
  private def probe(): AwaitingEngineWork = submit(start(), root).asInstanceOf[AwaitingEngineWork]
  private def review(state: CompletedCommentaryJob): CompletedMoveReview = state.result match
    case CompletedPositionCommentary.FocusedMoveReview(_, value) => value
    case _ => fail("expected a focused review")

  private def opening(rows: List[(String, Int)], previewDepth: Option[Int] = Some(8),
      proposals: List[CommentaryProposal.Request] = Nil): CommentaryJobState =
    val history = owner.extend(List("e2e4", "e7e5", "g1f3", "b8c6")).toOption.get
    val started = CommentaryJobReducer.startJob(initial, history.movePrefixUci, history.currentFen, "f1b5",
      history.extend(List("f1b5")).toOption.get.currentFen,
      // Match the HTTP policy: preview readings must precede the full-depth investigation.
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = previewDepth), 0L,
      proposals = proposals)
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    assertEquals(started.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(started.issuedWork.requiredDepth, previewDepth.getOrElse(16))
    val full = previewDepth.fold(started)(depth => submit(started, rows, depth = depth).asInstanceOf[AwaitingEngineWork])
    assertEquals(full.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(full.issuedWork.requiredDepth, 16)
    submit(full, rows)

  test("quiet alternative is an owned missing comparison; known root pair reuses its receipt"):
    val active = probe()
    assertEquals(active.issuedWork.purpose, EngineWorkPurpose.InvestigationComparison)
    assertEquals(active.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("e2e4", "g1h3")))
    assertEquals(active.issuedWork.enginePositionMovesUci, Nil)
    assertEquals(active.issuedWork.searchFen, initial)
    assertEquals(active.progress.selectedCommentariesCompleted, 1)
    assertEquals(active.progress.physicalWorksIssued, 2)
    assertEquals(active.rootSearches.map(_.work.workId), List("work:0", "work:1"))
    assertEquals(active.rootSearches.map(_.state), List("completed", "awaiting_engine"))
    assert(active.rootSearches.last.work eq active.issuedWork)
    assertEquals(active.rootSearches.last.receipt, None)
    assertEquals(review(active.assessment.get).bestMoveUci, "e2e4")
    val reused = active.inquiries.find(_.question.alternativeMove == "d2d4").get
    assertEquals(reused.state, "answered")
    assertEquals(reused.answer.get.workId, "work:0")
    val unresolved = active.inquiries.find(_.question.alternativeMove == "g1h3").get
    assertEquals(unresolved.state, "awaiting_engine")
    assert(unresolved.question.reasons.exists(_.kind == CommentaryInvestigation.Kind.SameActorChoice))

  test("refuted alternative retains the basic review and two exact investigated paths"):
    val active = probe()
    val done = submit(active, List("e2e4" -> 30, "g1h3" -> -50)).asInstanceOf[CompletedCommentaryJob]
    assertEquals(review(done).bestMoveUci, "e2e4")
    assertEquals(done.progress.physicalWorksIssued, 2)
    val answer = done.inquiries.find(_.question.alternativeMove == "g1h3").get.answer.get
    assertEquals(answer.evaluationRelation, "worse")
    assertEquals(done.assessmentNeedsRefresh, false)
    val ids = done.observationsPacket.get.assembly.legalLines.map(_.ref.id).toSet
    assert(ids(answer.referenceLineId) && ids(answer.alternativeLineId))

  test("a stopped root receipt retains returned moves whose new paths could not be assembled"):
    val active = probe()
    val earlierPacket = active.observationsPacket.get
    // Discovery can already own Nh3. This receipt extends it along a genuinely new exact path.
    val unassembled = List("g1h3", "a7a6", "h3g5")
    assertEquals(active.preparedPaths.get.certifiedPath(earlierPacket, unassembled), None)
    val issued = active.issuedWork
    val done = CommentaryJobReducer.submitIssuedWorkReport(active, EngineWorkReport.Completed(profile,
      issued.workId, issued.executionKeySha256, List(
        ReportedEngineLineSuffix(List("e2e4"), ReportedWhiteEngineScore.Centipawns(30), 16),
        ReportedEngineLineSuffix(unassembled, ReportedWhiteEngineScore.Centipawns(-50), 16))), 1L)
      .toOption.get
    val search = done.rootSearches.find(_.work.workId == active.issuedWork.workId).get
      .copy(stopReason = Some(CommentaryJobStopCondition.ReviewConstructionFailed))
    // The reducer records the real admitted receipt before assembling its new
    // paths. An assembly failure retains this earlier packet with that receipt.
    val observations = RuntimeProtocol.observationsJson(earlierPacket, prepared = active.preparedPaths,
      rootSearches = List(search))
    val exposed = (observations \ "selection" \ "root_searches").as[List[JsObject]].head
    assertEquals((exposed \ "state").as[String], "stopped")
    assertEquals((exposed \ "stop_reason").as[String], "review_construction_failed")
    val candidates = (exposed \ "candidates").as[List[JsObject]]
    assertEquals(candidates.map(row => (row \ "move_uci").as[String]),
      search.receipt.get.lines.map(_.evaluation.moves.head))
    val missing = candidates.find(row => (row \ "move_uci").as[String] == "g1h3").get
    assertEquals((missing \ "line_id").get, JsNull)
    assertEquals((missing \ "occurrence_id").get, JsNull)
    val retained = candidates.find(row => (row \ "move_uci").as[String] == "e2e4").get
    val existing = active.preparedPaths.get.certifiedPath(earlierPacket, List("e2e4")).get
    assertEquals((retained \ "line_id").as[String], existing.line.ref.id)
    assertEquals((retained \ "occurrence_id").as[String], existing.ids.head)

  test("deadline and cancel preserve an already completed assessment and the unanswered obligation"):
    val initialWork = start()
    val initialStopped = CommentaryJobReducer.cancelJob(initialWork).asInstanceOf[StoppedCommentaryJob]
    assertEquals(initialStopped.rootSearches.map(_.work), List(initialWork.issuedWork))
    assertEquals(initialStopped.rootSearches.head.state, "stopped")
    assertEquals(initialStopped.rootSearches.head.stopReason, Some(CommentaryJobStopCondition.Cancelled))
    assertEquals(initialStopped.rootSearches.head.receipt, None)
    val active = probe()
    val expired = CommentaryJobReducer.expireJob(active, 10000L).asInstanceOf[CompletedCommentaryJob]
    assertEquals(review(expired).bestMoveUci, "e2e4")
    assertEquals(expired.inquiries.find(_.question.alternativeMove == "g1h3").get.stopReason, Some("deadline_exceeded"))
    val refresh = submit(active, List("e2e4" -> 30, "g1h3" -> 80)).asInstanceOf[AwaitingEngineWork]
    val cancelled = CommentaryJobReducer.cancelJob(refresh).asInstanceOf[CompletedCommentaryJob]
    assertEquals(review(cancelled).bestMoveUci, "e2e4")
    assertEquals(cancelled.assessmentNeedsRefresh, true)
    assertEquals(cancelled.rootSearches.last.work, refresh.issuedWork)
    assertEquals(cancelled.rootSearches.last.state, "stopped")
    assertEquals(cancelled.rootSearches.last.stopReason, Some(CommentaryJobStopCondition.Cancelled))
    assertEquals(cancelled.rootSearches.last.receipt, None)
    assert(cancelled.rootSearches.init.forall(_.state == "completed"))
    val inquiry = cancelled.inquiries.find(_.question.alternativeMove == "g1h3").get
    assertEquals(inquiry.state, "answered")
    assertEquals(inquiry.stopReason, Some("cancelled"))
    assertEquals(inquiry.assessmentEffect, Some("needs_refresh"))

  test("a budget-failed root assessment refresh retains the owned rebound and baseline"):
    // These invented scores isolate refresh failure; they are not an R02 engine recording.
    val active = probe()
    val baseline = active.assessment.get
    val pairWork = active.issuedWork
    assertEquals(pairWork.rootRestriction, EngineRootRestriction.Restricted(List("e2e4", "g1h3")))
    val refresh = submit(active, List("e2e4" -> 30, "g1h3" -> 80)).asInstanceOf[AwaitingEngineWork]
    val observed = refresh.inquiries.find(_.question.alternativeMove == "g1h3").get
    assertEquals(observed.question.referenceMove, "e2e4")
    assertEquals(observed.state, "answered")
    assertEquals(observed.answer.get.workId, pairWork.workId)
    assertEquals(observed.answer.get.executionKeySha256, pairWork.executionKeySha256)
    assertEquals(observed.answer.get.evaluationRelation, "better")
    assertEquals(refresh.assessmentNeedsRefresh, true)
    assertEquals(RuntimeProtocol.completedResultJson(refresh.assessment.get), RuntimeProtocol.completedResultJson(baseline))
    val work = refresh.issuedWork
    assertEquals(work.purpose, EngineWorkPurpose.InvestigationComparison)
    assertEquals(work.rootRestriction, EngineRootRestriction.Restricted(List("e2e4", "g1f3", "g1h3")))
    val wrongOwner = EngineWorkReport.ExecutorFailed(profile, pairWork.workId,
      pairWork.executionKeySha256, EngineWorkFailure.BudgetExhausted)
    assertEquals(CommentaryJobReducer.submitIssuedWorkReport(refresh, wrongOwner, 2L),
      Left(EngineWorkReportRejection.WorkNotOutstanding))
    val done = CommentaryJobReducer.submitIssuedWorkReport(refresh, EngineWorkReport.ExecutorFailed(profile,
      work.workId, work.executionKeySha256, EngineWorkFailure.BudgetExhausted), 2L).toOption.get
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(RuntimeProtocol.completedResultJson(done), RuntimeProtocol.completedResultJson(baseline))
    val inquiry = done.inquiries.find(_.question.id == observed.question.id).get
    assertEquals(inquiry.state, "answered")
    assertEquals(inquiry.answer, observed.answer, "retain the actually observed owned pair")
    assertEquals(inquiry.stopReason, Some("search_budget_exhausted"))
    assertEquals(inquiry.assessmentEffect, Some("needs_refresh"))
    assertEquals(done.assessmentNeedsRefresh, true)
    val pair = done.rootSearches.find(_.work.workId == pairWork.workId).get
    assert(pair.receipt.get.work eq pairWork)
    assertEquals(pair.state, "completed")
    val failed = done.rootSearches.find(_.work.workId == work.workId).get
    assert(failed.work eq work)
    assertEquals(failed.state, "stopped")
    assertEquals(failed.stopReason, Some(CommentaryJobStopCondition.SearchBudgetExhausted))
    assertEquals(failed.receipt, None)

  test("wrong work owner is rejected and invalid pair cannot overwrite the preserved review"):
    val active = probe()
    val wrong = EngineWorkReport.ExecutorFailed(profile, "work:0", active.issuedWork.executionKeySha256,
      EngineWorkFailure.ExecutionFailed)
    assertEquals(CommentaryJobReducer.submitIssuedWorkReport(active, wrong, 1L),
      Left(EngineWorkReportRejection.WorkNotOutstanding))
    val stopped = submit(active, List("e2e4" -> 30, "d2d4" -> 50)).asInstanceOf[CompletedCommentaryJob]
    assertEquals(review(stopped).bestMoveUci, "e2e4")
    assertEquals(stopped.inquiries.find(_.question.alternativeMove == "g1h3").get.stopReason,
      Some("invalid_engine_work_report"))

  test("root candidates alone answer their questions without forcing another engine work"):
    val done = submit(start("d2d4"), root).asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.progress.physicalWorksIssued, 1)
    assertEquals(done.rootSearches.size, 1)
    assertEquals(done.rootSearches.head.state, "completed")
    assertEquals(done.inquiries.size, 1)
    assertEquals(done.inquiries.head.state, "answered")

  test("a refresh that ranks the actual move first reuses its pair in the regenerated questions"):
    val refresh = submit(probe(), List("e2e4" -> 30, "g1h3" -> 80)).asInstanceOf[AwaitingEngineWork]
    val done = submit(refresh, List("e2e4" -> 30, "g1h3" -> 40, "g1f3" -> 70)).asInstanceOf[CompletedCommentaryJob]
    assertEquals(review(done).bestMoveUci, "g1f3")
    val regenerated = done.inquiries.find(inquiry =>
      inquiry.question.referenceMove == "g1f3" && inquiry.question.alternativeMove == "g1h3").get
    assertEquals(regenerated.state, "answered")
    assertEquals(regenerated.answer.get.workId, refresh.issuedWork.workId)
    assertEquals(regenerated.answer.get.evaluationRelation, "worse")
    assertEquals(done.inquiries.map(_.question.id).distinct.size, done.inquiries.size)

  test("a played move outside the root bundle shares one ledger across focus, probe and refresh"):
    val focus = submit(start("b1c3"), root).asInstanceOf[AwaitingEngineWork]
    assertEquals(focus.issuedWork.purpose, EngineWorkPurpose.FocusComparison)
    val probe = submit(focus, List("e2e4" -> 30, "b1c3" -> 10)).asInstanceOf[AwaitingEngineWork]
    assertEquals(probe.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("e2e4", "b1a3")))
    val refresh = submit(probe, List("e2e4" -> 30, "b1a3" -> 80)).asInstanceOf[AwaitingEngineWork]
    val done = submit(refresh, List("e2e4" -> 30, "b1a3" -> 70, "b1c3" -> 10)).asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.progress.physicalWorksIssued, 4)
    assertEquals(done.progress.physicalReportsAccepted, 4)
    assertEquals(done.rootSearches.map(_.work.purpose), List(EngineWorkPurpose.RootSearch,
      EngineWorkPurpose.FocusComparison, EngineWorkPurpose.InvestigationComparison, EngineWorkPurpose.InvestigationComparison))
    assert(done.rootSearches.forall(search => search.state == "completed" && search.receipt.nonEmpty))
    assertEquals(review(done).bestMoveUci, "b1a3")

  test("unequal-depth refresh cannot upgrade a provisional discovery to a completed assessment"):
    val refresh = submit(probe(), List("e2e4" -> 30, "g1h3" -> 80)).asInstanceOf[AwaitingEngineWork]
    val work = refresh.issuedWork
    val done = CommentaryJobReducer.submitIssuedWorkReport(refresh, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, List(
        ReportedEngineLineSuffix(List("e2e4"), ReportedWhiteEngineScore.Centipawns(30), 17),
        ReportedEngineLineSuffix(List("g1h3"), ReportedWhiteEngineScore.Centipawns(70), 17),
        ReportedEngineLineSuffix(List("g1f3"), ReportedWhiteEngineScore.Centipawns(-10), 16))), 2L).toOption.get
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.assessmentNeedsRefresh, true)
    assertEquals(review(done).bestMoveUci, "e2e4")
    val inquiry = done.inquiries.find(_.question.alternativeMove == "g1h3").get
    assertEquals(inquiry.stopReason, Some("invalid_engine_work_report"))
    assertEquals(inquiry.assessmentEffect, Some("needs_refresh"))
    assertEquals(inquiry.state, "answered")
    assertEquals(inquiry.answer.get.workId, work.workId)
    assertEquals(inquiry.answer.get.referenceDepth, Some(17))
    assertEquals(inquiry.answer.get.alternativeDepth, Some(17))
    val invalid = done.rootSearches.last
    assertEquals(invalid.state, "stopped")
    assertEquals(invalid.stopReason, Some(CommentaryJobStopCondition.InvalidEngineWorkReport))
    assert(invalid.receipt.nonEmpty) // Admitted lines survive a failed same-depth comparison.
    assert(invalid.receipt.get.work eq invalid.work)
    assertEquals(invalid.receipt.get.lines.flatMap(_.evaluation.engineLine.map(_.depth)), List(17, 17, 16))

  test("a losing restricted pair preserves draw availability without declaring the claim dominant"):
    val history = owner.extend(List.fill(2)(List("g1f3", "g8f6", "f3g1", "f6g8")).flatten).toOption.get
    val initialWork = CommentaryJobReducer.startJob(initial, history.movePrefixUci, history.currentFen, "g1f3",
      history.extend(List("g1f3")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true), 0L).toOption.get.asInstanceOf[AwaitingEngineWork]
    val probe = submit(initialWork, List("e2e4" -> 100, "d2d4" -> 90, "g1f3" -> 80)).asInstanceOf[AwaitingEngineWork]
    val refresh = submit(probe, List("e2e4" -> -80, "g1h3" -> -20)).asInstanceOf[AwaitingEngineWork]
    val done = submit(refresh, List("e2e4" -> -80, "g1h3" -> -20, "g1f3" -> -40)).asInstanceOf[CompletedCommentaryJob]
    done.result match
      case CompletedPositionCommentary.FocusedMoveReview(claims, record) =>
        assert(claims.nonEmpty)
        assertEquals(record.bestMoveUci, "g1h3")
        assertEquals(done.assessmentScope, "investigation_pair")
      case _ => fail("a restricted comparison cannot displace all root choices with a draw recommendation")

  test("an observed PV functional contrast investigates all linked quiet choices without preselecting Bc4 over Ba6"):
    val history = owner.extend(List("e2e4", "e7e5", "g1f3", "b8c6")).toOption.get
    val active = opening(List("f1b5" -> 30, "b1c3" -> 20, "d2d4" -> 20))
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(active.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List("f1b5", "f1a6", "f1c4", "f1d3", "f1e2")))
    assertEquals(active.issuedWork.multiPv, 3)
    val bc4 = active.inquiries.find(_.question.alternativeMove == "f1c4").get
    assertEquals(bc4.state, "awaiting_engine")
    assertEquals(bc4.stopReason, None)
    assert(bc4.question.orderingCandidateIds.nonEmpty)
    assert(bc4.question.orderingCandidateIds.forall(id => active.pvComparisons.exists(reading =>
      reading.id == id && reading.eligibleQuietFocus && reading.participants.exists(_.square.key == "f1"))))
    assertEquals(active.inquiries.find(_.question.alternativeMove == "f3e5").get.state, "deferred")
    assert(bc4.question.reasons.exists(_.kind == CommentaryInvestigation.Kind.SameActorChoice))
    assertEquals(active.inquiries.find(_.question.alternativeMove == "f1a6").get.state, "awaiting_engine")
    assertEquals(active.issuedWork.enginePositionMovesUci, history.movePrefixUci)
    val work = active.issuedWork
    val done = CommentaryJobReducer.submitIssuedWorkReport(active, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, List(
        ReportedEngineLineSuffix(List("f1b5", "a7a6"), ReportedWhiteEngineScore.Centipawns(30), 16),
        ReportedEngineLineSuffix(List("f1c4", "g8f6"), ReportedWhiteEngineScore.Centipawns(30), 16),
        ReportedEngineLineSuffix(List("f1e2", "g8f6"), ReportedWhiteEngineScore.Centipawns(10), 16))), 1L).toOption.get
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.inquiries.find(_.question.alternativeMove == "f1c4").get.answer.get.evaluationRelation, "equal")
    assertEquals(done.inquiries.find(_.question.alternativeMove == "f3e5").get.state, "deferred")
    val search = done.rootSearches.find(_.work.workId == work.workId).get
    assert(search.work eq work)
    assertEquals(search.state, "completed")
    assert(search.receipt.get.work eq work)
    val investigated = search.work.rootRestriction match
      case EngineRootRestriction.Restricted(moves) => moves.toSet
      case _ => fail("expected the issued quiet comparison pool")
    val returned = search.receipt.get.lines.map(_.evaluation.moves.head).toSet
    assert(investigated("f1a6") && !returned("f1a6"))
    assertEquals(done.inquiries.find(_.question.alternativeMove == "f1a6").get.state, "deferred")
    assert(!investigated("f3e5")) // Present hypothesis, never selected for this additional comparison.
    val cancelled = CommentaryJobReducer.cancelJob(active).asInstanceOf[CompletedCommentaryJob]
    val stoppedSearch = cancelled.rootSearches.find(_.work.workId == work.workId).get
    assertEquals(stoppedSearch.work.rootRestriction, search.work.rootRestriction)
    assertEquals(stoppedSearch.state, "stopped")
    assertEquals(stoppedSearch.stopReason, Some(CommentaryJobStopCondition.Cancelled))
    assertEquals(stoppedSearch.receipt, None)
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("1" * 32,
      "opening-done", 10000L, done))
    val sharedWork = (snapshot \ "observations" \ "selection" \ "root_searches").as[List[JsObject]]
      .find(row => (row \ "work_id").as[String] == work.workId).get
    assertEquals((sharedWork \ "root_restriction" \ "moves_uci").as[List[String]].toSet, investigated)
    assertEquals((sharedWork \ "candidates").as[List[JsObject]].map(row => (row \ "move_uci").as[String]).toSet, returned)
    val lineIds = done.observationsPacket.get.assembly.legalLines.map(_.ref.id).toSet
    assert((sharedWork \ "candidates").as[List[JsObject]].forall(row => lineIds((row \ "line_id").as[String])))
    val linked = (snapshot \ "observations" \ "selection" \ "inquiries").as[List[JsObject]]
      .find(value => (value \ "alternative_move").as[String] == "f1c4").get
    assert((linked \ "explanation_candidate_ids").as[List[String]].nonEmpty)
    assert((linked \ "next_evidence").toOption.isEmpty)
    assertEquals((linked \ "state").as[String], "answered")
    assertEquals((linked \ "answer" \ "evaluation_relation").as[String], "equal")

  test("retained inquiries link only to PV ordering candidates still present in the response"):
    val active = opening(List("f1b5" -> 30, "b1c3" -> 20, "d2d4" -> 20))
      .asInstanceOf[AwaitingEngineWork]
    val inquiry = active.inquiries.find(_.question.alternativeMove == "f1c4").get
    assert(inquiry.question.orderingCandidateIds.size >= 2)
    val retained = active.pvComparisons.filter(_.id == inquiry.question.orderingCandidateIds.head)
    val observations = RuntimeProtocol.observationsJson(active.observationsPacket.get,
      inquiries = List(inquiry), pvComparisons = retained, prepared = active.preparedPaths)
    val shown = (observations \ "selection" \ "inquiries").as[List[JsObject]].head
    assertEquals((shown \ "ordering_candidate_ids").as[List[String]], retained.map(_.id))
    assertEquals((shown \ "state").as[String], inquiry.state)
    assertEquals(inquiry.question.orderingCandidateIds.size, 2,
      "projection must not rewrite the scheduler's retained question")
    val without = RuntimeProtocol.observationsJson(active.observationsPacket.get,
      inquiries = List(inquiry), prepared = active.preparedPaths)
    assertEquals((without \ "selection" \ "inquiries").as[List[JsObject]]
      .flatMap(value => (value \ "ordering_candidate_ids").as[List[String]]), Nil)

  test("a poor third PV is still read but its presence does not promote quiet alternatives"):
    val done = opening(List("f1b5" -> 30, "b1c3" -> -300, "d2d4" -> -400))
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.pvComparisons.size, 2)
    assert(done.pvComparisons.forall(_.qualityBand == PrincipalVariationComparison.QualityBand.Separated))
    assert(done.inquiries.forall(_.question.orderingCandidateIds.isEmpty))
    assertEquals(done.progress.physicalWorksIssued, 2)
    assert(done.rootSearches.forall(_.work.purpose == EngineWorkPurpose.RootSearch))

  test("a one-cp new reference still refreshes an actual move across a grade boundary"):
    val active = submit(start(), List("e2e4" -> 100, "d2d4" -> 90, "g1f3" -> 45)).asInstanceOf[AwaitingEngineWork]
    val refresh = submit(active, List("e2e4" -> 100, "g1h3" -> 101)).asInstanceOf[AwaitingEngineWork]
    assertEquals(refresh.assessmentNeedsRefresh, true)
    assertEquals(refresh.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("e2e4", "g1f3", "g1h3")))
    val answer = refresh.inquiries.find(_.question.alternativeMove == "g1h3").get.answer.get
    assertEquals(answer.evaluationRelation, "better")
    assert(refresh.pvComparisons.exists(reading => reading.answer == answer &&
      reading.qualityBand == PrincipalVariationComparison.QualityBand.Close))
    val done = submit(refresh, List("e2e4" -> 100, "g1h3" -> 101, "g1f3" -> 45)).asInstanceOf[CompletedCommentaryJob]
    assertEquals(review(done).bestMoveUci, "g1h3")
    assertEquals(review(done).playedRootRank, None)
    assertEquals(review(done).bestRootRank, None)
    assertEquals(done.assessmentScope, "investigation_pair")
    assertEquals(done.assessmentNeedsRefresh, false)
    assertEquals(done.progress.physicalWorksIssued, 3)
    assertEquals(done.inquiries.find(_.question.alternativeMove == "g1h3").get.assessmentEffect, Some("refreshed"))
    val wire = RuntimeProtocol.completedResultJson(done)
    assertEquals((wire \ "selected_move_reviews").as[List[JsObject]].head.value("move_uci"), JsString("g1h3"))
    assert((wire \ "selected_move_reviews").as[List[JsObject]].forall(value => (value \ "selection" \ "root_rank").toOption.isEmpty))
    def verdicts(state: CompletedCommentaryJob) = (RuntimeProtocol.completedResultJson(state) \\ "verdict_code")
      .map(_.as[String]).toSet
    assert(verdicts(active.assessment.get)("inaccuracy"))
    assert(verdicts(done)("mistake"))

  test("without preview a PV-linked family competes before unrelated automatic root pairs"):
    val family = opening(List("f1b5" -> 30, "b1c3" -> 20, "d2d4" -> 20), previewDepth = None)
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(family.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List("f1b5", "f1a6", "f1c4", "f1d3", "f1e2")))
    assertEquals(family.issuedWork.multiPv, 3)
    assertEquals(family.progress.physicalWorksIssued, 2)
    val chosen = family.inquiries.filter(_.state == "awaiting_engine")
    assertEquals(chosen.map(_.question.alternativeMove).toSet, Set("f1a6", "f1c4", "f1d3", "f1e2"))
    assert(chosen.forall(_.question.orderingCandidateIds.nonEmpty))

  test("an unrelated submitted comparison leaves one slot for the unsearched family"):
    import CommentaryProposal.*
    val source = Source("user", "compare-a3-first")
    val requested = Request("a3-first", Origin(source, source, source, source),
      List(CompareMoves("pair", Nil, "f1b5", "a2a3")))
    val pair = opening(List("f1b5" -> 30, "b1c3" -> 20, "d2d4" -> 20), previewDepth = None,
      proposals = List(requested)).asInstanceOf[AwaitingEngineWork]
    assertEquals(pair.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("f1b5", "a2a3")))
    val family = submit(pair, List("f1b5" -> 30, "a2a3" -> 0)).asInstanceOf[AwaitingEngineWork]
    assertEquals(family.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List("f1b5", "f1a6", "f1c4", "f1d3", "f1e2")))
    assertEquals(family.issuedWork.multiPv, 3)
    assertEquals(family.progress.physicalWorksIssued, 3)
    val done = submit(family, List("f1b5" -> 30, "f1c4" -> 30, "f1e2" -> 10))
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.progress.physicalWorksIssued, 3)
    assertEquals(done.progress.physicalReportsAccepted, 3)
    assertEquals(done.rootSearches.count(_.work.purpose == EngineWorkPurpose.InvestigationComparison), 2)
    assertEquals(done.inquiries.find(_.question.alternativeMove == "f1c4").get.answer.map(_.evaluationRelation), Some("equal"))
    val omitted = done.inquiries.find(_.question.alternativeMove == "f1a6").get
    assertEquals(omitted.state, "deferred")
    assertEquals(omitted.answer, None)
    assertEquals(done.proposalEntries.find(_.request.id == requested.id).get.readings.head.answer.map(_.workId),
      Some(pair.issuedWork.workId))

  test("synthetic deeper evidence within18 rechecks actual and replaces its verdict and explanation"):
    val active = probe()
    val baseline = active.assessment.get
    val before = review(baseline).judgmentPacket
    // The reference's name and score are unchanged. Its deeper receipt still leaves actual unverified.
    val verify = submit(active, List("e2e4" -> 30, "g1h3" -> -50), depth = 18).asInstanceOf[AwaitingEngineWork]
    assertEquals(verify.issuedWork.requiredDepth, 18)
    assertEquals(verify.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("e2e4", "g1f3")))
    assertEquals(verify.assessmentNeedsRefresh, true)
    assertEquals(RuntimeProtocol.completedResultJson(verify.assessment.get), RuntimeProtocol.completedResultJson(baseline))
    val failed = CommentaryJobReducer.expireJob(verify, 10000L).asInstanceOf[CompletedCommentaryJob]
    assertEquals(failed.assessmentNeedsRefresh, true)
    assertEquals(RuntimeProtocol.completedResultJson(failed), RuntimeProtocol.completedResultJson(baseline))
    val tooShallow = submit(verify, List("e2e4" -> 30, "g1f3" -> -400), depth = 16)
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(tooShallow.assessmentNeedsRefresh, true)
    assertEquals(review(tooShallow).judgmentPacket.primary, before.primary)
    val work = verify.issuedWork
    val newActualPath = List("g1f3", "d7d5", "d2d4", "g8f6")
    val following = CommentaryJobReducer.submitIssuedWorkReport(verify, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, List(
        ReportedEngineLineSuffix(List("e2e4"), ReportedWhiteEngineScore.Centipawns(30), 18),
        ReportedEngineLineSuffix(newActualPath, ReportedWhiteEngineScore.Centipawns(-400), 18))), 2L)
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    assertEquals(following.assessmentNeedsRefresh, false)
    val nextWork = following.issuedWork
    assertEquals(nextWork.executionKeySha256, "1b2a719c3430c676ab7768037ba55bc2917cc02b90b2ab14987dfb4873861dab")
    val currentQuestionIds = CommentaryIdea.currentQuestions(following.proposalEntries).map(_.request.id).toSet
    // This fixed synthetic state owns a pending first-reply pool, not an
    // answered pair or a chess-strength claim. The original synthetic20
    // fixture is frozen separately; this current18 fixture owns its new key.
    assert(following.proposalEntries.exists(entry => currentQuestionIds(entry.request.id) &&
        entry.request.requirements.exists {
      case pair: CommentaryProposal.CompareMoves =>
        val exactPair = nextWork.rootRestriction ==
          EngineRootRestriction.Restricted(List(pair.referenceMove, pair.alternativeMove))
        val firstChildPool = nextWork.rootRestriction == EngineRootRestriction.Unrestricted &&
          nextWork.purpose == EngineWorkPurpose.ResponseComparison && nextWork.workId == "work:3" &&
          nextWork.executionKeySha256 == "1b2a719c3430c676ab7768037ba55bc2917cc02b90b2ab14987dfb4873861dab" &&
          pair.prefixMovesUci == List(following.playedMoveUci) && nextWork.multiPv == 3 &&
          nextWork.engineProfile == profile && owner.extend(pair.prefixMovesUci).toOption.exists(child =>
            nextWork.enginePositionInitialFen == child.initialFen &&
            nextWork.enginePositionMovesUci == child.movePrefixUci && nextWork.searchFen == child.currentFen &&
            List(pair.referenceMove, pair.alternativeMove).forall(child.currentLegalMoveUcis.contains)) &&
          following.observationsPacket.exists(packet => packet.assembly.playedTransition.exists(root =>
            nextWork.searchAnchor.exists(anchor => anchor.rootMove == following.playedMoveUci &&
              anchor.rootOccurrenceId == root.evidence.id && root.to.id.contains(anchor.positionId) &&
              anchor.prefixMoves == pair.prefixMovesUci)))
        (exactPair || firstChildPool) && nextWork.requiredDepth == work.requiredDepth &&
          CommentaryProposal.neededRequirementIds(entry)(pair.id) &&
          nextWork.searchAnchor.map(_.prefixMoves).getOrElse(Nil) == pair.prefixMovesUci &&
          nextWork.proposalRefs.contains(entry.request.id -> pair.id) &&
          entry.readings.exists(reading => reading.requirementId == pair.id &&
            reading.state == "awaiting_engine" && reading.workId.contains(nextWork.workId) && reading.answer.isEmpty)
      case _ => false
    }))
    val done = CommentaryJobReducer.cancelJob(following).asInstanceOf[CompletedCommentaryJob]
    assertEquals(review(done).bestMoveUci, "e2e4")
    assertEquals(done.assessmentNeedsRefresh, false)
    val packet = review(done).judgmentPacket
    val actual = packet.assembly.input.assessmentCandidates.find(_.rootMove.contains("g1f3")).get.evaluation
    assertEquals(actual.moves, newActualPath)
    assertEquals(actual.engineLine.map(line => (line.scoreCp, line.depth)), Some((-400, 18)))
    assertNotEquals(packet.primary, before.primary)
    assert((RuntimeProtocol.completedResultJson(done) \\ "verdict_code").contains(JsString("blunder")))
    assertEquals(done.assessmentScope, "investigation_pair")
    assert(before.assembly.input.evaluationObservations.forall(packet.assembly.input.evaluationObservations.contains))
    assert(before.assembly.legalLines.map(_.ref.id).toSet.subsetOf(packet.assembly.legalLines.map(_.ref.id).toSet))

  test("a deeper actual-reference pair cannot be replaced by a later shallower investigation"):
    val active = submit(start(), root, depth = 18).asInstanceOf[AwaitingEngineWork]
    assertEquals(active.issuedWork.requiredDepth, 18)
    val rejected = submit(active, List("e2e4" -> 30, "g1h3" -> 80), depth = 16).asInstanceOf[CompletedCommentaryJob]
    assertEquals(RuntimeProtocol.completedResultJson(rejected), RuntimeProtocol.completedResultJson(active.assessment.get))
    assertEquals(rejected.rootSearches.last.receipt, None)
    assertEquals(rejected.assessmentNeedsRefresh, false)
    val verify = submit(active, List("e2e4" -> 30, "g1h3" -> 80), depth = 18).asInstanceOf[AwaitingEngineWork]
    assertEquals(verify.issuedWork.requiredDepth, 18)
    assertEquals(verify.issuedWork.multiPv, 3)
    // The earlier new-candidate win disappears; the old reference remains in this same new work.
    val done = submit(verify, List("e2e4" -> 40, "g1f3" -> 10, "g1h3" -> 20), depth = 18)
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(review(done).bestMoveUci, "e2e4")
    assertEquals(done.assessmentNeedsRefresh, false)
    assertEquals(review(done).judgmentPacket.assembly.input.assessmentCandidates
      .flatMap(_.evaluation.engineLine.map(_.depth)).distinct, List(18))

  test("historical20 and24 comparison receipts retain the original assessment without issuing over18 work"):
    List(20, 24).foreach { historicalDepth =>
      val active = probe()
      val baseline = active.assessment.get
      val oldPrimary = review(baseline).judgmentPacket.primary
      val stopped = submit(active, List("e2e4" -> 30, "g1h3" -> -50), depth = historicalDepth)
        .asInstanceOf[CompletedCommentaryJob]
      assertEquals(stopped.progress.physicalWorksIssued, active.progress.physicalWorksIssued)
      assert(stopped.rootSearches.forall(_.work.requiredDepth <= 18))
      assertEquals(stopped.rootSearches.last.work, active.issuedWork)
      assertEquals(stopped.rootSearches.last.receipt.get.lines.flatMap(_.evaluation.engineLine.map(_.depth)).distinct,
        List(historicalDepth), "the supplied historical receipt is not downcast")
      assertEquals(stopped.verification.get.targetDepth, historicalDepth)
      assertEquals(stopped.verification.get.state, "stopped")
      assertEquals(stopped.verification.get.stopReason, Some("depth_limit"))
      assert(stopped.assessmentNeedsRefresh)
      assertEquals(RuntimeProtocol.completedResultJson(stopped), RuntimeProtocol.completedResultJson(baseline))
      assertEquals(review(stopped).judgmentPacket.primary, oldPrimary)
      assertEquals(stopped.inquiries.find(_.question.alternativeMove == "g1h3").get.answer.map(_.workId),
        Some(active.issuedWork.workId), "the old completed pair retains its physical owner")
    }

  test("a historical24 root receipt preserves its grade but cannot authorize a new24 explanation search"):
    val started = start()
    val stopped = submit(started, root, depth = 24).asInstanceOf[CompletedCommentaryJob]
    assertEquals(stopped.progress.physicalWorksIssued, 1)
    assertEquals(stopped.rootSearches.map(_.work), List(started.issuedWork))
    assertEquals(stopped.rootSearches.head.receipt.get.lines.flatMap(_.evaluation.engineLine.map(_.depth)).distinct,
      List(24))
    assertEquals(review(stopped).bestMoveUci, "e2e4")
    assertEquals(review(stopped).judgmentPacket.assembly.input.assessmentCandidates
      .flatMap(_.evaluation.engineLine.map(_.depth)).distinct, List(24))
    assertEquals(stopped.verification.get.targetDepth, 24)
    assertEquals(stopped.verification.get.stopReason, Some("depth_limit"))
    assert(stopped.assessmentNeedsRefresh)

  test("admitted but unequal-depth root evidence preserves the old assessment as needing refresh"):
    val active = probe()
    val work = active.issuedWork
    val done = CommentaryJobReducer.submitIssuedWorkReport(active, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, List(
        ReportedEngineLineSuffix(List("e2e4"), ReportedWhiteEngineScore.Centipawns(50), 20),
        ReportedEngineLineSuffix(List("g1h3"), ReportedWhiteEngineScore.Centipawns(80), 16))), 1L)
      .toOption.get.asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.assessmentNeedsRefresh, true)
    assertEquals(RuntimeProtocol.completedResultJson(done), RuntimeProtocol.completedResultJson(active.assessment.get))
    assert(done.rootSearches.last.receipt.nonEmpty)
    assertEquals(done.rootSearches.last.stopReason, Some(CommentaryJobStopCondition.InvalidEngineWorkReport))

  test("a same-reference best-choice pair refreshes with its runner-up and inherits the assessment depth"):
    val rows = List("g1f3" -> 30, "e2e4" -> 20, "d2d4" -> 10)
    val active = submit(start(), rows).asInstanceOf[AwaitingEngineWork]
    val done = submit(active, List("g1f3" -> 30, "g1h3" -> -100), depth = 18).asInstanceOf[CompletedCommentaryJob]
    assertEquals(done.progress.physicalWorksIssued, 2)
    assertEquals(review(done).bestMoveUci, "g1f3")
    assertEquals(done.assessmentScope, "investigation_pair")
    val staleChoice = done.proposalEntries.find(entry => entry.admission == "generated" &&
      entry.request.requirements.exists {
        case pair: CommentaryProposal.CompareMoves => pair.id == "contrast_opponent_choice" &&
          pair.referenceMove == "g1f3" && pair.alternativeMove == "d2d4"
        case _ => false
      }).get
    assertEquals(staleChoice.request.requirements.collect { case fact: CommentaryProposal.Fact => fact.id }.toSet,
      Set("choice_reference", "choice_alternative"))
    assertEquals(staleChoice.readings.find(_.requirementId == "contrast_opponent_choice").get.answer, None,
      "a bare comparison cannot borrow its old depth-16 answer after the assessment reaches depth18")
    assertEquals(review(done).judgmentPacket.assembly.input.assessmentCandidates
      .flatMap(_.evaluation.engineLine.map(_.depth)).distinct, List(18))
    val deeper = submit(start(), rows, depth = 18).asInstanceOf[AwaitingEngineWork]
    assertEquals(deeper.issuedWork.requiredDepth, 18)
    assertEquals(deeper.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("g1f3", "g1h3")))
    assertEquals(deeper.issuedWork.multiPv, 2)
    val refreshed = submit(deeper, List("g1f3" -> 30, "g1h3" -> -100), depth = 18).asInstanceOf[CompletedCommentaryJob]
    assertEquals(refreshed.assessmentNeedsRefresh, false)
    assertEquals(review(refreshed).bestMoveUci, "g1f3")

  test("a submitted alias refreshes a bare automatic pair once and both owners reuse its answer"):
    import CommentaryProposal.*
    val source = Source("user", "explicit-current-comparison")
    val requested = Request("explicit-pair", Origin(source, source, source, source),
      List(CompareMoves("contrast_opponent_choice", Nil, "g1f3", "d2d4")))
    val active = submit(start(proposals = List(requested)),
      List("g1f3" -> 30, "e2e4" -> 20, "d2d4" -> 10)).asInstanceOf[AwaitingEngineWork]
    val before = active.proposalEntries.find(_.request.id == requested.id).get.readings.head
    assertEquals(before.answer.map(_.workId), Some("work:0"))
    val refresh = submit(active, List("g1f3" -> 30, "g1h3" -> -100), depth = 18)
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(refresh.assessmentNeedsRefresh, false)
    assertEquals(refresh.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("g1f3", "d2d4")))
    assertEquals(refresh.issuedWork.requiredDepth, 18)
    val owners = refresh.proposalEntries.filter(entry => entry.readings.exists(reading =>
      reading.requirementId == "contrast_opponent_choice" && reading.state == "awaiting_engine" &&
        reading.workId.contains(refresh.issuedWork.workId)))
    assert(owners.exists(_.request.id == requested.id))
    assert(owners.exists(_.admission == "generated"))
    val done = submit(refresh, List("g1f3" -> 30, "d2d4" -> 10), depth = 18).asInstanceOf[CompletedCommentaryJob]
    val answers = owners.map(owner => done.proposalEntries.find(_.request.id == owner.request.id).get.readings
      .find(_.requirementId == "contrast_opponent_choice").get.answer.get)
    assertEquals(answers.map(_.workId).distinct, List(refresh.issuedWork.workId))
    assertEquals(answers.map(_.referenceDepth).distinct, List(Some(18)))
    assertEquals(answers.distinct.size, 1)
    assertEquals(done.progress.physicalWorksIssued, 3)
    assertEquals(done.progress.physicalReportsAccepted, 3)

  test("an unobserved opponent choice precedes both a queen-capture seed and a qualified quiet root hypothesis"):
    val fen = "r1b1kbnr/pppp1ppp/2n5/4q3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val started = CommentaryJobReducer.startJob(fen, Nil, fen, "f1b5",
      history.extend(List("f1b5")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true), 0L).toOption.get.asInstanceOf[AwaitingEngineWork]
    val active = submit(started, List("f1b5" -> 30, "b1c3" -> 20, "d2d4" -> 20)).asInstanceOf[AwaitingEngineWork]
    val quiet = active.inquiries.find(_.question.alternativeMove == "f1c4").get
    assert(quiet.question.orderingCandidateIds.nonEmpty)
    assertEquals(quiet.state, "deferred")
    assertEquals(active.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(active.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val capture = active.inquiries.find(_.question.alternativeMove == "f3e5").get
    assertEquals(capture.question.reasons.flatMap(_.capture).map(_.role.name).distinct, List("queen"))
    assertEquals(capture.state, "deferred")

  test("a root family survey omitting its reference triggers one exact three-way verification instead of inventing superiority"):
    val family = opening(List("b1c3" -> 30, "f1b5" -> 20, "d2d4" -> 20))
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(family.issuedWork.multiPv, 3)
    val verify = submit(family, List("f1c4" -> 30, "f1d3" -> 30, "f1e2" -> 30)).asInstanceOf[AwaitingEngineWork]
    val moves = verify.issuedWork.rootRestriction match
      case EngineRootRestriction.Restricted(moves) => moves
      case _ => fail("verification must name all compared roots")
    assertEquals(moves.toSet, Set("b1c3", "f1b5", "f1c4"))
    assertEquals(verify.issuedWork.multiPv, 3)
    assertEquals(verify.assessmentNeedsRefresh, true)
    val candidate = verify.inquiries.find(_.question.alternativeMove == "f1c4").get
    assertEquals(candidate.answer, None)
    val cancelled = CommentaryJobReducer.cancelJob(verify).asInstanceOf[CompletedCommentaryJob]
    assertEquals(review(cancelled).bestMoveUci, "b1c3")
    assertEquals(cancelled.assessmentNeedsRefresh, true)
    val done = submit(verify, List("b1c3" -> 30, "f1b5" -> 20, "f1c4" -> 30)).asInstanceOf[CompletedCommentaryJob]
    assertEquals(review(done).bestMoveUci, "b1c3") // A missing reference may have been tied, not worse.
    assertEquals(done.assessmentNeedsRefresh, false)
    assertEquals(done.progress.physicalWorksIssued, 4)
    List((31, 20, "f1c4"), (30, 40, "f1b5")).foreach { (candidateCp, actualCp, expected) =>
      val result = submit(verify, List("b1c3" -> 30, "f1b5" -> actualCp, "f1c4" -> candidateCp))
      val updated = result match
        case next: AwaitingEngineWork =>
          assertEquals(next.assessmentNeedsRefresh, false)
          assertEquals(review(next.assessment.get).bestMoveUci, expected)
          if expected == "f1c4" then
            assertEquals(next.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("f1c4", "d2d4")))
            val choice = next.proposalEntries.find(entry => entry.readings.exists(reading =>
              reading.requirementId == "contrast_opponent_choice" && reading.state == "awaiting_engine" &&
                reading.workId.contains(next.issuedWork.workId))).get
            // Unlike a bare source pair, this comparison tests the actual d-pawn capture options.
            assert(choice.request.requirements.exists {
              case fact: CommentaryProposal.Fact => fact.predicate.kind == "legal_capture" &&
                fact.predicate.target.contains(CommentaryFact.Piece("white", "pawn", "d2"))
              case _ => false
            })
            submit(next, List("f1c4" -> candidateCp, "d2d4" -> 20)).asInstanceOf[CompletedCommentaryJob]
          else
            // The earlier family did not include Bb5. Its score in another work
            // cannot stand in for these comparisons against the new reference.
            assertEquals(next.issuedWork.rootRestriction,
              EngineRootRestriction.Restricted(List("f1b5", "f1a6", "f1d3", "f1e2")))
            assertEquals(next.issuedWork.multiPv, 3)
            assertEquals(next.inquiries.filter(_.state == "awaiting_engine").map(_.question.referenceMove).distinct,
              List("f1b5"))
            submit(next, List("f1b5" -> actualCp, "f1d3" -> 30, "f1e2" -> 30)).asInstanceOf[CompletedCommentaryJob]
        case done: CompletedCommentaryJob => done
        case other => fail(s"expected a refreshed assessment, got ${other.getClass.getSimpleName}")
      assertEquals(review(updated).bestMoveUci, expected)
      assertEquals(updated.assessmentNeedsRefresh, false)
      assertEquals(updated.progress.physicalWorksIssued, 5)
      assertEquals(updated.progress.physicalReportsAccepted, updated.progress.physicalWorksIssued)
    }
