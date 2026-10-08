package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Invented reports test disclosure, requests and evidence ownership, never engine strength or latency. */
class ProgressiveCommentaryJobTest extends munit.FunSuite:
  private val initial = chess.variant.Standard.initialFen.value
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val simple = List(List("e2e4") -> 30, List("d2d4") -> 20, List("g1f3") -> 10)
  private def history(prefix: List[String] = Nil, fen: String = initial) =
    CanonicalPositionHistory.from(fen, Nil, fen).toOption.get.extend(prefix).toOption.get
  private def start(actual: String = "d2d4", owner: CanonicalPositionHistory = history(), limit: Int = 600,
      proposals: List[CommentaryProposal.Request] = Nil, deadline: Long = 10000L) =
    CommentaryJobReducer.startJob(owner.initialFen, owner.movePrefixUci, owner.currentFen, actual,
      owner.extend(List(actual)).toOption.get.currentFen,
      CommentaryJobPolicy(16, deadline, profile, investigateChoices = true,
        maximumLocalExpansions = limit, previewDepth = Some(8)), 0L, proposals = proposals).toOption.get.asInstanceOf[AwaitingEngineWork]
  private def submit(state: AwaitingEngineWork, rows: List[(List[String], Int)], now: Long = 1L): CommentaryJobState =
    val work = state.issuedWork
    val next = CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile, work.workId,
      work.executionKeySha256, rows.map { (moves, cp) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), work.requiredDepth)
      }), now).toOption.get
    next.observationsPacket.foreach { packet =>
      val readings = next.responseInquiries.flatMap(inquiry => inquiry.reading.toList ++ inquiry.retainedReadings)
      assert(readings.forall(_.belongsTo(packet)), "every intermediate response reading must own the current packet")
    }
    next
  private def waiting(state: CommentaryJobState) = state.asInstanceOf[AwaitingEngineWork]
  private def finish(state: CommentaryJobState): CompletedCommentaryJob = state match
    case done: CompletedCommentaryJob => done
    case next: AwaitingEngineWork => CommentaryJobReducer.cancelJob(next).asInstanceOf[CompletedCommentaryJob]
    case _ => fail("expected a retained assessment")
  private def completedAssessment(state: CommentaryJobState): CompletedCommentaryJob = state match
    case done: CompletedCommentaryJob => done
    case _ => state.assessment.get
  private def review(state: CommentaryJobState): CompletedMoveReview = completedAssessment(state).result match
    case CompletedPositionCommentary.FocusedMoveReview(_, value) => value
    case _ => fail("expected move assessment")
  private def wire(state: CommentaryJobState) = PositionCommentaryJobProtocol.projectJobSnapshot(
    CommentaryJobSnapshot("1" * 32, "progressive-test", 10000L, state))
  private def assessedDepths(state: CommentaryJobState) = review(state).judgmentPacket.assembly.input
    .assessmentCandidates.flatMap(_.evaluation.engineLine.map(_.depth)).toSet

  test("shared comparison work preserves distinct expectations, orientations and job ownership"):
    import CommentaryProposal.{ CompareMoves, Origin, Request, Source }
    val source = Source("user", "shared-comparison-test")
    val origin = Origin(source, source, source, source)
    val requests = List(
      Request("one", origin, List(CompareMoves("choice", Nil, "e2e4", "d2d4", Some("better")))),
      Request("two", origin, List(CompareMoves("choice", Nil, "e2e4", "d2d4", Some("worse")))),
      Request("reverse", origin, List(CompareMoves("choice", Nil, "d2d4", "e2e4", Some("better")))))
    def run(rows: List[(List[String], Int)]) =
      val verifying = waiting(submit(start(limit = 0, proposals = requests), rows))
      finish(submit(verifying, rows))
    val done = run(simple)
    val answers = done.proposalEntries.filter(entry => requests.exists(_.id == entry.request.id))
      .map(entry => entry.request.id -> entry.readings.head).toMap
    assertEquals(answers("one").state, "refuted")
    assertEquals(answers("two").state, "supported")
    assertEquals(answers("reverse").state, "supported")
    assertEquals(answers("one").answer, answers("two").answer)
    assertEquals(answers("one").answer.get.referenceLineId, answers("reverse").answer.get.alternativeLineId)
    assertEquals(done.progress.physicalWorksIssued, 2)
    val explained = done.proposalEntries.filter(entry => requests.exists(_.id == entry.request.id)).map { entry =>
      val choice = entry.request.requirements.head.asInstanceOf[CompareMoves]
      entry.copy(generatedArms = List(CommentaryProposal.GeneratedArm("arm-" + entry.request.id,
        Nil, choice.referenceMove, Some(choice.alternativeMove), List(choice.id), List(choice.id), source, Nil)))
    }
    val packet = done.observationsPacket.get
    val paths = ObservedLineProtocol.prepare(packet)
    val candidates = CommentaryProposal.comparisonCandidates(packet, paths, explained)
    assertEquals(candidates.size, 1, "opposite phrasings of one owned comparison produce one explanation")
    assertEquals(CommentaryProposal.comparisonCandidates(packet, paths, explained.reverse), candidates)
    assertEquals(explained.map(_.readings.head.state), List("refuted", "supported", "supported"),
      "display deduplication must not merge the questions' different expectations or answers")
    val other = run(simple.updated(0, List("e2e4") -> -30))
    val changed = other.proposalEntries.find(_.request.id == "one").get.readings.head
    assertEquals(changed.state, "supported")
    assertNotEquals(changed.answer.get.evaluationRelation, answers("one").answer.get.evaluationRelation)

  test("a signal-less close first result is returned before discovery and unconditionally requests deeper root work"):
    val first = start()
    assertEquals(first.issuedWork.requiredDepth, 8)
    assertEquals(first.issuedWork.multiPv, 3)
    val verifying = waiting(submit(first, simple))
    assertEquals(verifying.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(verifying.issuedWork.requiredDepth, 16)
    assertEquals(verifying.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(verifying.localExpansionsUsed, 0)
    assertEquals(verifying.proposalEntries, Nil)
    assertEquals(verifying.inquiries, Nil)
    assertEquals(assessedDepths(verifying), Set(8))
    assertEquals((wire(verifying) \ "verification" \ "state").as[String], "pending")
    assert((wire(verifying) \ "assessment").isDefined)
    val done = finish(submit(verifying, simple.updated(1, List("d2d4") -> -450)))
    assertEquals(review(done).bestMoveUci, "e2e4")
    assertEquals(assessedDepths(done), Set(16))
    assertEquals(done.verification.get.state, "complete")
    assertEquals(done.assessmentNeedsRefresh, false)
    val actual = review(done).judgmentPacket.assembly.input.assessmentCandidates.find(_.evaluation.moves.head == "d2d4").get
    assertEquals(actual.evaluation.engineLine.get.scoreCp, -450)
    def verdicts(state: CommentaryJobState) = (RuntimeProtocol.completedResultJson(completedAssessment(state)) \\ "verdict_code")
      .map(_.as[String]).toSet
    assertNotEquals(verdicts(verifying), verdicts(done))
    assert(done.pvComparisons.forall(_.answer.requiredDepth >= 16))

  test("a root comparison's changed PV and small drift do not repeat the actual comparison, but a material change does"):
    import CommentaryProposal.{ CompareMoves, Origin, Request, Source }
    val source = Source("user", "root-drift")
    val request = Request("root-drift", Origin(source, source, source, source),
      List(CompareMoves("choice", Nil, "e2e4", "c2c4")))
    def ready() = waiting(submit(waiting(submit(start(limit = 0, proposals = List(request)), simple)), simple))
    val current = ready()
    assertEquals(current.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("e2e4", "c2c4")))
    val unchanged = submit(current, List(List("e2e4", "e7e5", "g1f3") -> 33, List("c2c4") -> -100))
    assertEquals(review(unchanged).judgmentPacket.assembly.input.assessmentCandidates,
      review(current).judgmentPacket.assembly.input.assessmentCandidates)
    assert(unchanged.proposalEntries.find(_.request.id == request.id).get.readings.head.answer.nonEmpty)
    unchanged match
      case next: AwaitingEngineWork => assertNotEquals(next.issuedWork.rootRestriction,
        EngineRootRestriction.Restricted(List("e2e4", "d2d4")))
      case _ => ()
    val changed = waiting(submit(ready(), List(List("e2e4", "e7e5", "g1f3") -> 300, List("c2c4") -> -100)))
    assertEquals(changed.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("e2e4", "d2d4")))
    assertEquals((wire(changed) \ "assessment_state").as[String], "needs_refresh")

  test("a missing actual move is compared at each depth without mixing shallow and verified scores"):
    val focus = waiting(submit(start("b1c3"), simple))
    assertEquals(focus.issuedWork.purpose, EngineWorkPurpose.FocusComparison)
    assertEquals(focus.issuedWork.requiredDepth, 8)
    val verifying = waiting(submit(focus, List(List("e2e4") -> 30, List("b1c3") -> 20)))
    assertEquals(assessedDepths(verifying), Set(8))
    val deeperFocus = waiting(submit(verifying, simple))
    assertEquals(deeperFocus.issuedWork.purpose, EngineWorkPurpose.FocusComparison)
    assertEquals(deeperFocus.issuedWork.requiredDepth, 16)
    assertEquals(assessedDepths(deeperFocus), Set(8))
    val done = finish(submit(deeperFocus, List(List("e2e4") -> 30, List("b1c3") -> -450)))
    assertEquals(assessedDepths(done), Set(16))
    assertEquals(done.verification.get.completedDepth, Some(16))

  test("deadline, cancellation and invalid verification keep the first result explicitly unverified"):
    val pending = waiting(submit(start(limit = 0), simple))
    val work = pending.issuedWork
    val failed = CommentaryJobReducer.submitIssuedWorkReport(pending, EngineWorkReport.ExecutorFailed(profile,
      work.workId, work.executionKeySha256, EngineWorkFailure.ExecutionFailed), 2L).toOption.get
    List(CommentaryJobReducer.expireJob(pending, 10000L), CommentaryJobReducer.cancelJob(pending), failed,
      submit(pending, simple, 10000L)).foreach { stopped =>
      assertEquals(assessedDepths(stopped), Set(8))
      assertEquals(stopped.verification.get.state, "stopped")
      assert(stopped.verification.get.stopReason.nonEmpty)
      assertEquals(stopped.assessmentNeedsRefresh, false)
    }
    assertEquals(failed.progress.physicalReportsAccepted, 2)
    assertEquals(CommentaryJobReducer.expireJob(pending, 10000L).progress.physicalReportsAccepted, 1)

  test("a budget-exhausted verified focus gets one larger-node attempt within the root time limit"):
    def focus() =
      val preview = waiting(submit(start("b1c3", limit = 0), simple))
      val verified = waiting(submit(preview, List(List("e2e4") -> 30, List("b1c3") -> 20)))
      waiting(submit(verified, simple))
    def fail(state: AwaitingEngineWork, failure: EngineWorkFailure, now: Long = 2L) =
      val work = state.issuedWork
      CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.ExecutorFailed(profile,
        work.workId, work.executionKeySha256, failure), now).toOption.get
    val original = focus()
    val retried = waiting(fail(original, EngineWorkFailure.BudgetExhausted))
    assertEquals(retried.issuedWork.purpose, EngineWorkPurpose.FocusComparison)
    assertEquals(retried.issuedWork.rootRestriction, original.issuedWork.rootRestriction)
    assertEquals(retried.issuedWork.searchLimits, EngineSearchLimits(16, 20000000, 5000, 2))
    assertNotEquals(retried.issuedWork.executionKeySha256, original.issuedWork.executionKeySha256)
    assertEquals(retried.rootSearches.find(_.work == original.issuedWork).get.stopReason,
      Some(CommentaryJobStopCondition.SearchBudgetExhausted))
    assertEquals(assessedDepths(retried), Set(8))
    assertEquals(retried.progress.physicalReportsAccepted, original.progress.physicalReportsAccepted + 1)
    val done = finish(submit(retried, List(List("e2e4") -> 30, List("b1c3") -> 20)))
    assertEquals(done.verification.get.completedDepth, Some(16))
    assertEquals(assessedDepths(done), Set(16))
    val exhausted = fail(retried, EngineWorkFailure.BudgetExhausted)
    assert(exhausted.isInstanceOf[CompletedCommentaryJob])
    assertEquals(exhausted.verification.get.stopReason, Some("search_budget_exhausted"))
    assertEquals(assessedDepths(exhausted), Set(8))
    val compact = waiting(fail(original, EngineWorkFailure.BudgetExhausted, 2001L))
    assertEquals(compact.issuedWork.searchLimits, EngineSearchLimits(16, 20000000, 2500, 2))
    assertEquals(compact.issuedWork.maxSearchElapsedMs, 3500)
    assertEquals(compact.issuedWork.rootRestriction, original.issuedWork.rootRestriction)
    assert(fail(compact, EngineWorkFailure.BudgetExhausted, 2002L).isInstanceOf[CompletedCommentaryJob],
      "the larger-node short retry cannot recursively renew itself")
    assert(fail(original, EngineWorkFailure.BudgetExhausted, 4501L).isInstanceOf[CompletedCommentaryJob],
      "less than the execution and delivery allowance cannot start the retry")
    assert(CommentaryJobReducer.expireJob(retried, 10000L).isInstanceOf[CompletedCommentaryJob])
    List(EngineWorkFailure.Cancelled, EngineWorkFailure.ExecutionFailed, EngineWorkFailure.EngineUnavailable,
      EngineWorkFailure.BrowserUnsupported).foreach(reason =>
      assert(fail(original, reason).isInstanceOf[CompletedCommentaryJob]))
    val shallow = waiting(submit(start("b1c3", limit = 0), simple))
    assert(fail(shallow, EngineWorkFailure.BudgetExhausted).isInstanceOf[StoppedCommentaryJob])

  test("the mandatory verified focus can use the selected reply allowance once, leaving delivery time"):
    val preview = waiting(submit(start("b1c3", limit = 0, deadline = 30000L), simple))
    val root = waiting(submit(preview, List(List("e2e4") -> 30, List("b1c3") -> 20)))
    val focus = waiting(submit(root, simple))
    def exhausted(state: AwaitingEngineWork, now: Long) =
      val work = state.issuedWork
      CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.ExecutorFailed(profile,
        work.workId, work.executionKeySha256, EngineWorkFailure.BudgetExhausted), now).toOption.get
    val retry = waiting(exhausted(focus, 19000L))
    assertEquals(retry.issuedWork.searchLimits, EngineSearchLimits(16, 40000000, 8000, 2))
    assertEquals(retry.issuedWork.maxSearchElapsedMs, 9000)
    assertEquals(retry.issuedWork.rootRestriction, focus.issuedWork.rootRestriction)
    assertEquals(retry.issuedWork.enginePositionInitialFen, focus.issuedWork.enginePositionInitialFen)
    assertEquals(retry.issuedWork.enginePositionMovesUci, focus.issuedWork.enginePositionMovesUci)
    assertEquals(assessedDepths(retry), Set(8))
    assertEquals(waiting(exhausted(focus, 19001L)).issuedWork.searchLimits,
      EngineSearchLimits(16, 20000000, 5000, 2))
    assert(exhausted(retry, 19002L).isInstanceOf[CompletedCommentaryJob], "one failed retry must remain unverified")
    val done = finish(submit(retry, List(List("e2e4") -> 30, List("b1c3") -> -450), 27000L))
    assertEquals(assessedDepths(done), Set(16))
    assertEquals(done.verification.get.state, "complete")

  private val targetBoard = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"

  test("a verification report with mixed depths retains the preview and the refresh obligation"):
    val pending = waiting(submit(start(), simple))
    val work = pending.issuedWork
    val stopped = CommentaryJobReducer.submitIssuedWorkReport(pending, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, simple.zipWithIndex.map { case ((moves, cp), index) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), if index == 0 then 17 else 16)
      }), 2L).toOption.get
    assertEquals(stopped.verification.get.state, "stopped")
    assertEquals(stopped.verification.get.stopReason, Some("invalid_engine_work_report"))
    assertEquals(stopped.assessmentNeedsRefresh, true)
    assertEquals(assessedDepths(stopped), Set(8))

  test("a score-dependent draw recommendation is provisional until the root is verified"):
    val owner = history(List.fill(2)(List("g1f3", "g8f6", "f3g1", "f6g8")).flatten)
    val pending = waiting(submit(start(owner = owner), List(List("e2e4") -> -100,
      List("d2d4") -> -110, List("g1f3") -> -120)))
    assertEquals(pending.verification.get.state, "pending")
    assert(pending.assessment.get.result.isInstanceOf[CompletedPositionCommentary.PositionAction])
    assertEquals(pending.localExpansionsUsed, 0)
    val done = finish(submit(pending, List(List("d2d4") -> 50, List("e2e4") -> -100, List("g1f3") -> -120)))
    assertEquals(done.verification.get.state, "complete")
    assertEquals(review(done).bestMoveUci, "d2d4")

  private val targetRoots = List(List("d2d3") -> 50, List("f1d3") -> 40, List("e4d5") -> 30)

  private def conflictingComparisons(secondCp: Int = 450, differentLine: Boolean = true,
      fullAgenda: Boolean = false): CommentaryJobState =
    import CommentaryProposal.*
    val source = Source("user", "same-position-comparisons")
    val request = Request("shared-reply", Origin(source, source, source, source), List(
      CompareMoves("first", List("d2d3"), "d5e4", "d8d7"),
      CompareMoves("second", List("d2d3"), "d5e4", "g8f7")) ++
      (if fullAgenda then (1 to 597).map(i => Uninterpreted(s"unknown-$i", "Unspecified hypothesis")).toList else Nil))
    val roots = targetRoots.updated(0, List("d2d3", "d5e4", "d3e4") -> 50)
    val first = waiting(submit(waiting(submit(start("d2d3", history(fen = targetBoard),
      proposals = List(request)), roots)), roots))
    assertEquals(first.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("d5e4", "d8d7")))
    val second = waiting(submit(first, List(List("d5e4", "d3e4") -> 30, List("d8d7") -> 500)))
    assertEquals(second.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("d5e4", "g8f7")))
    submit(second, List(List("d5e4", if differentLine then "f3e5" else "d3e4") -> secondCp, List("g8f7") -> 500))

  test("contradictory shared moves ask their exact divergence before rechecking the root, without importing child scores"):
    val probe = waiting(conflictingComparisons())
    assertEquals(probe.issuedWork.searchAnchor.get.prefixMoves, List("d2d3", "d5e4"))
    assertEquals(probe.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("d3e4", "f3e5")))
    assertEquals(probe.issuedWork.requiredDepth, 17)
    assertEquals(probe.verification.get.triggerWorkIds, List("work:2", "work:3"))
    assert(probe.assessmentNeedsRefresh)
    val foreign = start("d2d3", history(fen = targetBoard)).observationsPacket.get
    assertEquals(CommentaryComparisonConflict.read(foreign, probe.responseSearches, 16), Nil)
    val question = probe.proposalEntries.find(_.request.origin.question.producer == "C-comparison-conflict").get
    assertEquals(question.readings.head.state, "awaiting_engine")
    assert(probe.proposalEntries.find(_.request.id == "shared-reply").get.readings.forall(_.answer.isEmpty))
    assertEquals(assessedDepths(probe), Set(16))
    val verifying = waiting(submit(probe, List(List("d3e4") -> 40, List("f3e5") -> 450)))
    assertEquals(verifying.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(verifying.issuedWork.requiredDepth, 17)
    assertEquals(assessedDepths(verifying), Set(16))
    val answered = verifying.proposalEntries.find(_.request.id == question.request.id).get.readings.head
    assertEquals(answered.answer.get.workId, probe.issuedWork.workId)
    assertEquals(answered.answer.get.evaluationRelation, "better")
    val done = finish(submit(verifying, targetRoots.updated(0, List("d2d3", "d5e4", "f3e5") -> 450)))
    assertEquals(assessedDepths(done), Set(17))
    assertEquals(done.assessmentNeedsRefresh, false)
    assertEquals(done.proposalEntries.find(_.request.id == question.request.id).get.readings.head.answer, answered.answer)
    assert(done.proposalEntries.find(_.request.id == "shared-reply").get.readings.forall(_.answer.isEmpty))
    assertEquals(done.progress.physicalWorksIssued, 6)
    val ignored = finish(submit(verifying, targetRoots.updated(0, List("d2d3", "d5e4", "d3e4") -> 50)))
    assert(ignored.assessmentNeedsRefresh)
    assertEquals(ignored.verification.get.state, "stopped")
    assertEquals(ignored.verification.get.stopReason, Some("verification_limit"))
    assertEquals(ignored.verification.get.triggerWorkIds, List("work:2", "work:3", probe.issuedWork.workId))

  test("a full ordinary agenda leaves room for the question required to verify its own conflicting answers"):
    val probe = waiting(conflictingComparisons(fullAgenda = true))
    assertEquals(probe.proposalEntries.map(_.request.requirements.size).sum, CommentaryProposal.MaximumRequirements)
    assertEquals(probe.issuedWork.searchAnchor.get.prefixMoves, List("d2d3", "d5e4"))
    assert(probe.proposalEntries.exists(_.request.origin.question.producer == "C-comparison-conflict"))

  test("interrupted or shallow conflict probes retain the root refresh obligation and never certify either old answer"):
    val probe = waiting(conflictingComparisons())
    val work = probe.issuedWork
    val shallow = CommentaryJobReducer.submitIssuedWorkReport(probe, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, List("d3e4", "f3e5").map(move =>
        ReportedEngineLineSuffix(List(move), ReportedWhiteEngineScore.Centipawns(100), 16))), 2L).toOption.get
    List(CommentaryJobReducer.cancelJob(probe), CommentaryJobReducer.expireJob(probe, 10000L), shallow).foreach { state =>
      assert(state.assessmentNeedsRefresh)
      assertEquals(state.verification.get.state, "stopped")
      assertEquals(assessedDepths(state), Set(16))
      assert(state.proposalEntries.find(_.request.id == "shared-reply").get.readings.forall(_.answer.isEmpty))
      val question = state.proposalEntries.find(_.request.origin.question.producer == "C-comparison-conflict").get
      assertEquals(question.readings.head.state, "stopped")
      assert(question.readings.head.answer.isEmpty)
    }

  test("small drift needs no conflict probe, and identical continuations require root verification without inventing a divergence"):
    val ordinary = finish(conflictingComparisons(secondCp = 35))
    assert(!ordinary.proposalEntries.exists(_.request.origin.question.producer == "C-comparison-conflict"))
    assertEquals(ordinary.progress.physicalWorksIssued, 4)
    val verifying = waiting(conflictingComparisons(differentLine = false))
    assertEquals(verifying.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(verifying.issuedWork.requiredDepth, 17)
    assert(!verifying.proposalEntries.exists(_.request.origin.question.producer == "C-comparison-conflict"))
    assert(verifying.assessmentNeedsRefresh)

  private val childRows = List(List("d5e4", "d3e4") -> -300, List("d8d7") -> 200, List("g8f7") -> 300)
  private def childWork() =
    val deeper = waiting(submit(start("d2d3", history(fen = targetBoard)), targetRoots))
    val child = waiting(submit(deeper, targetRoots))
    assertEquals(child.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(child.issuedWork.searchAnchor.get.prefixMoves, List("d2d3"))
    child

  test("a counterexample on an assessed opponent prefix requests root verification, never imports child scores"):
    val child = childWork()
    val baseline = review(child)
    val pending = waiting(submit(child, childRows))
    assertEquals(pending.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(pending.issuedWork.requiredDepth, 17)
    assertEquals(pending.issuedWork.enginePositionMovesUci, Nil)
    assertEquals(pending.issuedWork.searchFen, targetBoard)
    assertEquals(pending.verification.get.reason, "child_challenge")
    assertEquals(pending.verification.get.triggerWorkIds, List(child.issuedWork.workId))
    assertEquals(pending.assessmentNeedsRefresh, true)
    assert(pending.responseInquiries.exists(inquiry => inquiry.state == "answered" &&
      inquiry.answer.exists(_.pair.workId == child.issuedWork.workId)))
    assert(!pending.responseInquiries.exists(_.state == "awaiting_engine"))
    assertEquals(review(pending).judgmentPacket, baseline.judgmentPacket)
    val stopped = finish(pending)
    assertEquals(stopped.verification.get.state, "stopped")
    assertEquals(stopped.assessmentNeedsRefresh, true)
    val done = finish(submit(pending, List(List("f1d3") -> 40, List("e4d5") -> 30, List("d2d3") -> -300)))
    assertEquals(review(done).bestMoveUci, "f1d3")
    assertEquals(done.verification.get.completedDepth, Some(17))
    assertEquals(done.assessmentNeedsRefresh, false)
    assertEquals(assessedDepths(done), Set(17))
    assert(done.responseSearches.exists(_.work.workId == child.issuedWork.workId))
    assert(done.responseInquiries.exists(_.answer.nonEmpty))

  test("a recheck missing the played move publishes completed child evidence while waiting for focus"):
    val pending = waiting(submit(childWork(), childRows))
    val baseline = review(pending)
    val focus = waiting(submit(pending, List(List("f1d3") -> 40,
      List("e4d5") -> 30, List("f3g5") -> 20)))
    assertEquals(focus.issuedWork.purpose, EngineWorkPurpose.FocusComparison)
    assertEquals(focus.issuedWork.requiredDepth, 17)
    assertEquals(assessedDepths(focus), Set(16))
    assertEquals(review(focus).judgmentPacket, baseline.judgmentPacket)
    assertEquals(focus.verification.get.state, "pending")
    assertEquals(focus.assessmentNeedsRefresh, true)
    assert(focus.responseSearches.nonEmpty)
    assert(focus.responseSearches.forall(_.belongsTo(focus.observationsPacket.get)))
    assert(focus.responseInquiries.exists(_.state == "answered"))
    assertEquals((wire(focus) \ "issued_engine_work" \ "purpose").as[String], "focus_comparison")
    val stopped = finish(focus)
    assertEquals((wire(stopped) \ "verification" \ "state").as[String], "stopped")
    val done = finish(submit(focus, List(List("f1d3") -> 40, List("d2d3") -> -300)))
    assertEquals(assessedDepths(done), Set(17))
    assertEquals((wire(done) \ "verification" \ "state").as[String], "complete")

  test("same-reply drift preserves its answer while a stronger reply or material worsening reopens the root"):
    import CommentaryProposal.{CompareMoves, Origin, Request, Source}
    val source = Source("user", "child-drift-regression")
    val request = Request("reply-strength", Origin(source, source, source, Source("none", "child-drift-regression")),
      List(CompareMoves("pair", List("d2d3"), "d5e4", "d8d7")))
    val roots = targetRoots.updated(0, List("d2d3", "d5e4", "d3e4") -> 50)
    val deeper = waiting(submit(start("d2d3", history(fen = targetBoard), proposals = List(request)), roots))
    val child = waiting(submit(deeper, roots))
    assertEquals(child.issuedWork.searchAnchor.get.prefixMoves, List("d2d3"))
    assertEquals(child.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("d5e4", "d8d7")))
    val baseline = review(child).judgmentPacket
    // This reproduces the small adverse drift seen with the same reply in the live h6 investigation.
    val stable = submit(child, List(List("d5e4", "d3e4") -> 41, List("d8d7") -> 42))
    assertEquals(stable.verification.get.state, "complete")
    assertEquals(stable.verification.get.triggerWorkIds, Nil)
    assertEquals(stable.assessmentNeedsRefresh, false)
    assertEquals(review(stable).judgmentPacket, baseline)
    val answer = stable.proposalEntries.find(_.request.id == request.id).get.readings.head.answer.get
    assertEquals(answer.workId, child.issuedWork.workId)
    assertEquals(answer.evaluationRelation, "worse")
    List(
      // Small scores alone are not a veto: a different reply wins the exact comparison.
      List(List("d8d7") -> 40, List("d5e4", "d3e4") -> 41),
      // A material deterioration still matters even when the same reply remains strongest.
      List(List("d5e4", "d3e4") -> -300, List("d8d7") -> 200)
    ).foreach { rows =>
      val pending = waiting(submit(child, rows))
      assertEquals(pending.issuedWork.purpose, EngineWorkPurpose.RootSearch)
      assertEquals(pending.verification.get.reason, "child_challenge")
      assertEquals(pending.assessmentNeedsRefresh, true)
      assertEquals(review(pending).judgmentPacket, baseline, "a child challenge is not a root score")
    }

  test("a new challenge after the recheck allowance is spent stays unresolved with its exact trigger"):
    val pending = waiting(submit(childWork(), childRows))
    val child = waiting(submit(pending, List(List("d2d3", "d5e4", "d3e4") -> -300,
      List("f1d3") -> -310, List("e4d5") -> -320)))
    // The deeper root result also reopens the now-shallow automatic counter comparison.
    // A new strong answer at this assessed prefix still cannot exceed the recheck allowance.
    assertEquals(child.issuedWork.searchAnchor.get.prefixMoves, List("d2d3"))
    val replies = child.issuedWork.rootRestriction match
      case EngineRootRestriction.Restricted(moves) => moves
      case _ => fail("the reopened question must retain its exact reply pair")
    assert(replies.contains("d5e4"))
    val stopped = submit(child, replies.map(move => List(move) -> (if move == "d5e4" then -300 else -900)))
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(stopped.verification.get.state, "stopped")
    assertEquals(stopped.verification.get.stopReason, Some("verification_limit"))
    assert(!stopped.continuationInquiries.exists(_.state == "awaiting_engine"))
    assert(!stopped.continuationInquiries.exists(_.routeChallenge.exists(_.state == "awaiting_engine")))
    assert(stopped.verification.get.triggerWorkIds.contains(child.issuedWork.workId))
    assertEquals(stopped.assessmentNeedsRefresh, true)
    assertEquals(assessedDepths(stopped), Set(17))

  test("a worse off-PV continuation remains conditional when the assessed line does not reach that exact prefix"):
    import CommentaryProposal.{CompareMoves, Origin, Request, Source}
    val prefix = List("d2d3", "d5e4", "d3e4")
    val source = Source("user", "off-pv-test")
    // Name the branch explicitly so this ownership check is independent of which
    // newly discovered counter deserves the first automatic investigation slot.
    val request = Request("off-pv", Origin(source, source, source, Source("none", "off-pv-test")),
      List(CompareMoves("pair", prefix, "d8d7", "d8d1")))
    val deeper = waiting(submit(start("d2d3", history(fen = targetBoard), proposals = List(request)), targetRoots))
    val next = waiting(submit(deeper, targetRoots))
    val baseline = review(next)
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, prefix)
    assert(!baseline.judgmentPacket.assembly.input.assessmentCandidates.exists(_.evaluation.moves.startsWith(prefix)))
    val done = finish(submit(next, List(List("d8d1") -> -900, List("d8d7") -> 0)))
    assertEquals(done.verification.get.state, "complete")
    assertEquals(done.verification.get.triggerWorkIds, Nil)
    assertEquals(done.assessmentNeedsRefresh, false)
    assertEquals(review(done).bestMoveUci, baseline.bestMoveUci)
    assertEquals(review(done).judgmentPacket.assembly.input.assessmentCandidates.map(_.evaluation.engineLine),
      baseline.judgmentPacket.assembly.input.assessmentCandidates.map(_.evaluation.engineLine))
    assertEquals(assessedDepths(done), Set(16))
    assert(done.responseSearches.exists(search => search.anchor.prefixMoves == prefix &&
      search.work.workId == next.issuedWork.workId))

  test("a child report beyond the supported root depth leaves a visible unresolved verification"):
    val child = childWork()
    val work = child.issuedWork
    val stopped = CommentaryJobReducer.submitIssuedWorkReport(child, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, childRows.map { (moves, cp) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 100)
      }), 2L).toOption.get
    assertEquals(stopped.verification.get.stopReason, Some("depth_limit"))
    assertEquals(stopped.verification.get.targetDepth, 100)
    assertEquals(stopped.assessmentNeedsRefresh, true)
    assertEquals(assessedDepths(stopped), Set(16))

  test("PV scope and same-pool allocation are distinct: witnessed quiet alternatives precede an unanchored capture"):
    val owner = history(List("e2e4", "e7e5", "g1f3", "b8c6"))
    val rows = List(List("f1b5") -> 30, List("b1c3") -> 20, List("d2d4") -> 20)
    val verifying = waiting(submit(start("f1b5", owner), rows))
    val active = submit(verifying, rows)
    val pool = active.inquiries.map(_.question)
    val quiet = pool.find(_.alternativeMove == "f1c4").get
    val capture = pool.find(_.alternativeMove == "f3e5").get
    assert(quiet.orderingCandidateIds.nonEmpty)
    assert(CommentaryInvestigation.investigationTier(quiet) < CommentaryInvestigation.investigationTier(capture))
    val byUci = pool.sortBy(_.alternativeMove)
    val byEvidence = pool.sortBy(q => (CommentaryInvestigation.investigationTier(q), q.id))
    assertEquals(byUci.map(_.id).toSet, byEvidence.map(_.id).toSet)
    assert(byEvidence.indexOf(quiet) < byEvidence.indexOf(capture))
    assert(active.inquiries.find(_.question.id == capture.id).get.state != "awaiting_engine")
    val packet = active.observationsPacket.get
    val paths = ObservedLineProtocol.prepare(packet)
    val sources = rows.map(row => CommentaryDiscovery.SourcePath(row._1,
      CommentaryProposal.Source("generator", "root_search", sourceRefs = List(verifying.issuedWork.workId))))
    val prefixes = rows.flatMap(_._1.inits).toSet
    def discover(guided: Boolean) = CommentaryDiscovery.discover(packet, paths,
      List(CommentaryDiscovery.Anchor(Nil, "f1b5")), sources, Nil, pvGuided = guided)
    val broad = discover(false)
    val guided = discover(true)
    assertEquals(broad.scans.head.legalMoves, guided.scans.head.legalMoves)
    assert(broad.replayNeeds.exists(prefix => !prefixes(prefix)))
    assert(guided.replayNeeds.forall(prefixes))
    assert(guided.scans.head.examinedMoves.toSet.subsetOf(rows.map(_._1.head).toSet))
    assertEquals(guided.scans.head.state, "partial")

  test("ordinary recapture also receives verification; a capture alone is not proof of a forced tactic"):
    val owner = history(List("e2e4", "d7d5", "e4d5"))
    val rows = List(List("d8d5") -> -20, List("g8f6") -> -10, List("c7c6") -> 0)
    val pending = waiting(submit(start("d8d5", owner), rows))
    assertEquals(pending.verification.get.state, "pending")
    val done = finish(submit(pending, rows))
    assertEquals(done.verification.get.state, "complete")
    assertEquals(assessedDepths(done), Set(16))
    assertEquals(done.assessmentNeedsRefresh, false)

  // Exposed source probe: ref-bc7b3f1ff52f4c7a9940dad53cd6b975, PDF pp.20–22.
  // The author supplied the sacrifice, both responses, the continuation and interpretation.
  // Only the legal target/response question below is discovered automatically; all scores are invented.
  private val bosboom = List("e2e4", "c7c5", "g1f3", "e7e6", "d2d4", "c5d4", "f3d4", "a7a6",
    "f1d3", "g8f6", "b1c3", "d8c7", "a2a4", "b7b6", "e1g1", "c8b7", "d1e2", "h7h5",
    "h2h3", "f8c5", "d4b3")
  test("source sacrifice keeps acceptance and decline as separate legal classes at the exact child"):
    val owner = history(bosboom)
    assertEquals(owner.currentFen, "rn2k2r/1bqp1pp1/pp2pn2/2b4p/P3P3/1NNB3P/1PP1QPP1/R1B2RK1 b kq - 2 11")
    val accept = List("f6g4", "h3g4", "h5g4", "e4e5", "b7f3", "g2f3", "c7d8")
    val decline = List("f6g4", "e4e5", "c7e5", "e2e5", "g4e5", "a4a5")
    assert(owner.extend(accept).isRight)
    assert(owner.extend(decline).isRight)
    val firstRows = List(List("f6g4") -> 0, List("h5h4") -> 10, List("b6b5") -> 20)
    val pending = waiting(submit(start("f6g4", owner), firstRows))
    val active = submit(pending, firstRows.updated(0, accept -> -300))
    val question = active.responseInquiries.find(q => q.question.rootMove == "f6g4" &&
      q.question.captureMoves.contains("h3g4")).get.question
    assert(question.declineMoves.contains("e4e5"))
    assert(!question.captureMoves.contains("e4e5"))
    assertEquals(assessedDepths(active), Set(16))
    assertEquals(active.verification.get.state, "complete")

    // Review the accepting move itself: the first synthetic report contains no sacrificial continuation.
    // The author-supplied Bf3/Qd8 line only enters with the deeper synthetic report.
    val acceptingOwner = owner.extend(List("f6g4")).toOption.get
    val light = List(List("e4e5") -> 20, List("h3g4") -> 10, List("g1h1") -> 0)
    val verification = waiting(submit(start("h3g4", acceptingOwner), light))
    assertEquals(verification.inquiries, Nil)
    assertEquals(verification.issuedWork.requiredDepth, 16)
    val punished = finish(submit(verification, light.updated(1, accept.tail -> -400)))
    val actual = review(punished).judgmentPacket.assembly.input.assessmentCandidates
      .find(_.evaluation.moves.head == "h3g4").get.evaluation
    assertEquals(actual.moves, accept.tail)
    assertEquals(actual.engineLine.get.scoreCp, -400)
    assert((RuntimeProtocol.completedResultJson(punished) \\ "verdict_code").contains(JsString("blunder")))
    assertNotEquals(review(punished).judgmentPacket.primary, review(verification).judgmentPacket.primary)

  // Same exposed source, PDF pp.10–13. The author supplies the preparatory purpose.
  test("a quiet preparatory source line remains an observed action sequence, not proof against every response"):
    val owner = history(List("e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "g7g6", "d2d4", "e5d4",
      "f3d4", "f8g7", "c1e3", "g8f6", "b1c3", "e8g8", "e1g1", "c6e7", "d1d2", "d7d5",
      "e4d5", "e7d5", "c3d5", "d8d5", "b5e2", "f6g4", "e2g4", "c8g4", "d4b3", "d5d2",
      "b3d2", "a8d8", "c2c3", "f8e8", "d2b3", "b7b6", "h2h3", "g4e6", "f1d1", "c7c5",
      "e3g5", "f7f6", "g5f4", "g8f7", "f2f3", "g6g5", "d1d8", "e8d8", "f4e3"))
    assertEquals(owner.currentFen, "3r4/p4kbp/1p2bp2/2p3p1/8/1NP1BP1P/PP4P1/R5K1 b - - 1 24")
    val plan = List("h7h6", "a1e1", "f6f5", "f3f4", "g7f6", "g2g3", "a7a5")
    assert(owner.extend(plan).isRight)
    val firstRows = List(List("h7h6") -> -50, List("g5g4") -> -40, List("a7a5") -> -30)
    val pending = waiting(submit(start("h7h6", owner), firstRows))
    val active = submit(pending, firstRows.updated(0, plan -> -50))
    val packet = active.observationsPacket.get
    assert(ObservedLineProtocol.prepare(packet).certifiedPath(packet, plan).nonEmpty)
    val support = active.proposalEntries.find(entry => entry.request.origin.question.producer == "C-support" &&
      entry.request.requirements.exists {
        case fact: CommentaryProposal.Fact => fact.predicate == CommentaryFact.Predicate("support",
          Some(CommentaryFact.Piece("black", "pawn", "h7")), Some(CommentaryFact.Piece("black", "pawn", "g5")))
        case _ => false
      }).getOrElse(fail("the exact h-pawn support for g5 should be discovered from the returned paths"))
    assert(support.readings.exists(reading => reading.requirementId == "reference" &&
      reading.result.exists(_.samples.exists(_.value.contains("present")))))
    assert(active.proposalEntries.forall(_.request.origin.question.kind == "generator"))
    assertEquals(active.verification.get.state, "complete")
    assert(active.discoveryState.scans.exists(_.state == "partial"))
