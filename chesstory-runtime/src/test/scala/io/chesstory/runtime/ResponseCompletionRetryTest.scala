package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import play.api.libs.json.*

/** v24 begins at the unchanged actual SG14 first-Black-response work after
  * three real reports. The pre-priority eight-test source and two-report v16
  * fixture are sealed in target/meaning-conditions-20261008/
  * response-retry-before-first-target24. A separate deeper-response test keeps
  * that earlier real root prefix, then submits an explicitly invented pool.
  * All failure/follow-up reports below are invented scheduling evidence. Their
  * scores never establish engine strength, strongest defense, or meaning.
  */
class ResponseCompletionRetryTest extends munit.FunSuite:
  private def loadFixture(relative: String): JsValue =
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()

  private lazy val fixture = loadFixture(
    "judgment-evaluation/fixtures/public-commentary-v6/response-first-target-prefix-sg14-produced.json.gz")
  private lazy val oldRootFixture = loadFixture(
    "judgment-evaluation/fixtures/public-commentary-v6/response-continuation-prefix-sg14-produced.json.gz")
  private def parsedRequest(value: JsValue) = PositionCommentaryJobProtocol.parseJobRequest(
    Json.stringify((value \ "request").get).getBytes(UTF_8)).toOption.get
  private def parsedReports(value: JsValue) = (value \ "saved_reports").as[List[JsObject]].map(saved =>
    PositionCommentaryJobProtocol.parseEngineWorkReport(
      Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get)
  private lazy val reports = parsedReports(fixture)
  private val root = "a4b3"
  private val firstResponsePrefix = List(root)
  private val deeperPrefix = List(root, "c6e7")

  private def awaiting(state: CommentaryJobState): AwaitingEngineWork = state match
    case active: AwaitingEngineWork => active
    case other => fail(s"expected an owned response work, got ${other.getClass.getSimpleName}")

  private def actualPrefix(value: JsValue, budgetMs: Long): AwaitingEngineWork =
    val input = parsedRequest(value)
    var state = CommentaryJobReducer.startJob(input.initialFen, input.movePrefixUci,
      input.currentFen, input.playedMoveUci, input.resultingFen,
      CommentaryJobPolicy(16, budgetMs + 1L, input.engineProfile,
        investigateChoices = true, previewDepth = Some(8)), 1L).toOption.get
    parsedReports(value).zipWithIndex.foreach { (report, index) =>
      val active = awaiting(state)
      assertEquals(report.reportedWorkId, active.issuedWork.workId)
      assertEquals(report.reportedExecutionKeySha256, active.issuedWork.executionKeySha256)
      state = CommentaryJobReducer.submitIssuedWorkReport(active, report, index.toLong + 2L).toOption.get
    }
    awaiting(state)

  private def prefix(budgetMs: Long = 30_000L): AwaitingEngineWork =
    val active = actualPrefix(fixture, budgetMs)
    assertEquals(active.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(active.issuedWork.workId, "work:3")
    assertEquals(active.issuedWork.executionKeySha256,
      (fixture \ "pending_snapshot" \ "issued_engine_work" \ "execution_key_sha256").as[String])
    assertEquals(active.issuedWork.searchAnchor.map(_.prefixMoves), Some(firstResponsePrefix))
    assertEquals(active.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(active.issuedWork.searchLimits, EngineSearchLimits(16, 2_000_000, 2500, 3))
    assertEquals(active.issuedWork.maxSearchElapsedMs, 3500)
    assertEquals(currentInquiry(active).state, "awaiting_engine")
    assertEquals(currentInquiry(active).answer, None)
    assertEquals(active.issuedWork.searchAnchor.map(_.rootOccurrenceId),
      Some(currentInquiry(active).question.rootMoveOccurrenceId))
    assertEquals(active.issuedWork.searchAnchor.map(_.positionId),
      Some(currentInquiry(active).question.childPositionId))
    active

  private def currentInquiry(state: CommentaryJobState): CommentaryResponseInquiry =
    state.responseInquiries.find(inquiry => inquiry.question.rootMove == root &&
      inquiry.question.captureMoves.contains("c6d4")).getOrElse(
      fail("the current SG14 first-response question keeps its canonical inquiry owner"))

  private def currentDeeper(state: CommentaryJobState): CommentaryContinuationInquiry =
    state.continuationInquiries.find(_.question.prefixMoves == deeperPrefix).getOrElse(
      fail("the retained independent exposure continuation keeps its exact prefix owner"))

  private def assertNoCompletedResponse(state: CommentaryJobState, original: AwaitingEngineWork): Unit =
    assert(!state.responseSearches.exists(search => search.anchor == original.issuedWork.searchAnchor.get),
      "an unfinished attempt supplies no completed search witness at this exact anchor")
    assertEquals(currentInquiry(state).answer, None,
      "unfinished search never supplies an answer or a numeric strength receipt")

  private def failed(active: AwaitingEngineWork, now: Long = 10L,
      failure: EngineWorkFailure = EngineWorkFailure.BudgetExhausted): CommentaryJobState =
    val work = active.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(active, EngineWorkReport.ExecutorFailed(active.engineProfile,
      work.workId, work.executionKeySha256, failure), now).toOption.get

  private def expandedSameWork(previous: AwaitingEngineWork, next: CommentaryJobState): Boolean = next match
    case active: AwaitingEngineWork => active.issuedWork.searchAnchor == previous.issuedWork.searchAnchor &&
      active.issuedWork.searchLimits == EngineSearchLimits(16, 20_000_000, 5000, 3) &&
      active.issuedWork.maxSearchElapsedMs == 6000
    case _ => false

  private def assertSameScope(original: AwaitingEngineWork, retry: AwaitingEngineWork): Unit =
    val first = original.issuedWork
    val next = retry.issuedWork
    assertEquals(next.purpose, first.purpose)
    assertEquals(next.enginePositionInitialFen, first.enginePositionInitialFen)
    assertEquals(next.enginePositionMovesUci, first.enginePositionMovesUci)
    assertEquals(next.searchFen, first.searchFen)
    assertEquals(next.rootRestriction, first.rootRestriction)
    assertEquals(next.searchAnchor, first.searchAnchor)
    assertEquals(next.requiredDepth, first.requiredDepth)
    assertEquals(next.multiPv, first.multiPv)
    assertEquals(next.proposalRefs, first.proposalRefs)
    assertNotEquals(next.workId, first.workId)
    assertNotEquals(next.executionKeySha256, first.executionKeySha256)

  private def assertSameOwner(original: AwaitingEngineWork, retry: AwaitingEngineWork): Unit =
    assertSameScope(original, retry)
    assertEquals(currentInquiry(retry).question, currentInquiry(original).question)
    assertEquals(currentInquiry(retry).state, "awaiting_engine")
    assertEquals(currentInquiry(retry).stopReason, None)

  private def completed(active: AwaitingEngineWork, choices: List[(String, Int)],
      now: Long = 12L): CommentaryJobState =
    // Legal one-ply choices and close synthetic scores isolate completion and
    // ownership. They are not actual book/standalone engine analysis.
    val lines = choices.map { (move, score) =>
      ReportedEngineLineSuffix(List(move), ReportedWhiteEngineScore.Centipawns(score), 16)
    }
    val work = active.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(active, EngineWorkReport.Completed(active.engineProfile,
      work.workId, work.executionKeySha256, lines), now).toOption.get

  test("a current finite response gets one budget-fitting completion attempt with the same owner"):
    val original = prefix()
    val retry = awaiting(failed(original))
    assert(expandedSameWork(original, retry))
    assertSameOwner(original, retry)
    assertEquals(retry.issuedWork.workId, "work:4")
    assertEquals(original.issuedWork.searchLimits, EngineSearchLimits(16, 2_000_000, 2500, 3),
      "issuing a retry does not relabel the original unfinished search")
    assertNoCompletedResponse(retry, original)

  test("the exact twelve-second remaining boundary permits retry but one millisecond less does not"):
    val original = prefix()
    val enough = failed(original, now = 18_001L)
    assert(expandedSameWork(original, enough))
    assertSameOwner(original, awaiting(enough))
    val short = failed(original, now = 18_002L)
    assert(!expandedSameWork(original, short))
    assertEquals(currentInquiry(short).state, "stopped")
    assertEquals(currentInquiry(short).stopReason, Some("search_budget_exhausted"))

  test("the old ten-second job retains its immediate unresolved-response boundary"):
    val original = prefix(budgetMs = 10_000L)
    val stopped = failed(original)
    assert(!expandedSameWork(original, stopped))
    assertEquals(currentInquiry(stopped).question, currentInquiry(original).question)
    assertEquals(currentInquiry(stopped).state, "stopped")
    assertEquals(currentInquiry(stopped).stopReason, Some("search_budget_exhausted"))
    assertNoCompletedResponse(stopped, original)

  test("execution failure and cancellation do not acquire a completion retry"):
    List(EngineWorkFailure.ExecutionFailed, EngineWorkFailure.Cancelled,
      EngineWorkFailure.EngineUnavailable).foreach { failure =>
      val original = prefix()
      val stopped = failed(original, failure = failure)
      assert(!expandedSameWork(original, stopped), s"$failure is not an unfinished node/time search")
      assertEquals(currentInquiry(stopped).question, currentInquiry(original).question)
      assertEquals(currentInquiry(stopped).state, "stopped")
      assert(currentInquiry(stopped).stopReason.nonEmpty)
      assertNoCompletedResponse(stopped, original)
    }

  test("failure of the expanded attempt is unknown and never creates an unbounded retry loop"):
    val original = prefix()
    val retry = awaiting(failed(original))
    val stopped = failed(retry, now = 12L)
    assert(!expandedSameWork(retry, stopped))
    assertEquals(currentInquiry(stopped).question, currentInquiry(original).question)
    assertEquals(currentInquiry(stopped).state, "stopped")
    assertEquals(currentInquiry(stopped).stopReason, Some("search_budget_exhausted"))
    assertNoCompletedResponse(stopped, original)

  test("a delayed report for the old work key cannot answer the expanded work"):
    val original = prefix()
    val retry = awaiting(failed(original))
    val old = original.issuedWork
    val stale = EngineWorkReport.ExecutorFailed(original.engineProfile, old.workId,
      old.executionKeySha256, EngineWorkFailure.BudgetExhausted)
    assert(CommentaryJobReducer.submitIssuedWorkReport(retry, stale, 12L).isLeft)
    assertEquals(currentInquiry(retry).state, "awaiting_engine")
    assertEquals(retry.issuedWork.requiredDepth, 16)

  test("a root-position receipt key cannot be borrowed by the response retry"):
    val original = prefix()
    val retry = awaiting(failed(original))
    val foreign = reports.last
    val falseOwner = EngineWorkReport.ExecutorFailed(retry.engineProfile, retry.issuedWork.workId,
      foreign.reportedExecutionKeySha256, EngineWorkFailure.BudgetExhausted)
    assert(CommentaryJobReducer.submitIssuedWorkReport(retry, falseOwner, 12L).isLeft)
    assertEquals(retry.issuedWork.searchAnchor.map(_.prefixMoves), Some(firstResponsePrefix))
    assertEquals(currentInquiry(retry).state, "awaiting_engine")
    assertNoCompletedResponse(retry, original)

  test("completed first-response retry uses its own receipt and retains the unanswered independent continuation"):
    val original = prefix()
    val retry = awaiting(failed(original))
    val previousDeeper = currentDeeper(original)
    val next = completed(retry, List("c6e7" -> 30, "a7a6" -> 31, "g7f8" -> 32))
    val inquiry = currentInquiry(next)
    assertEquals(inquiry.question.rootMoveOccurrenceId, currentInquiry(original).question.rootMoveOccurrenceId)
    assertEquals(inquiry.answer, None,
      "completing the pool does not answer the omitted Nxd4 capture class")
    assert(inquiry.stopReason != Some("search_budget_exhausted"))
    val search = next.responseSearches.find(_.work.workId == retry.issuedWork.workId).getOrElse(
      fail("only the completed retry owns the current first-response survey"))
    assertEquals(search.anchor, retry.issuedWork.searchAnchor.get)
    assertEquals(search.work.requiredDepth, 16)
    assertEquals(search.work.rootRestriction, EngineRootRestriction.Unrestricted)
    assert(!next.responseSearches.exists(_.work.workId == original.issuedWork.workId))
    assert(!search.lines.exists(_.evaluation.moves.head == "c6d4"))
    assert(Set(inquiry.question.referenceMove, inquiry.question.alternativeMove) == Set("c6d4", "c6e7"))
    val deeper = currentDeeper(next)
    assertEquals(deeper.question.prefixMoves, previousDeeper.question.prefixMoves)
    assertEquals(deeper.question.prefixOccurrenceIds, previousDeeper.question.prefixOccurrenceIds)
    assertEquals(deeper.question.positionId, previousDeeper.question.positionId)
    assert(deeper.searchWorkIds.isEmpty,
      "finishing the first-response pool does not fabricate a receipt for the independent continuation")

  test("after the first pool a deeper finite continuation keeps the same retry scope and its own completion receipt"):
    // Only these two starting reports are real v16 evidence. The later black
    // pool and white continuation reports are invented scheduling evidence.
    val first = actualPrefix(oldRootFixture, budgetMs = 30_000L)
    assertEquals(first.issuedWork.searchAnchor.map(_.prefixMoves), Some(firstResponsePrefix))
    assertEquals(currentInquiry(first).state, "awaiting_engine")
    val afterPool = completed(first, List("c6e7" -> 30, "a7a6" -> 31, "g7f8" -> 32), now = 10L)
    assertEquals(currentInquiry(afterPool).answer, None,
      "omitting Nxd4 still leaves the original capture-versus-decline question unanswered")
    val original = awaiting(afterPool)
    val before = currentDeeper(original)
    assertEquals(original.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(original.issuedWork.searchAnchor.map(_.prefixMoves), Some(deeperPrefix))
    assertEquals(original.issuedWork.searchAnchor, Some(before.question.anchor))
    assertEquals(original.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(original.issuedWork.searchLimits, EngineSearchLimits(16, 2_000_000, 2500, 3))
    assertEquals(before.state, "awaiting_engine")
    assert(before.searchWorkIds.isEmpty)
    val retry = awaiting(failed(original, now = 11L))
    assert(expandedSameWork(original, retry))
    assertSameScope(original, retry)
    assertEquals(currentDeeper(retry).question, before.question)
    assertEquals(currentDeeper(retry).state, "awaiting_engine")
    assertEquals(currentDeeper(retry).stopReason, None)
    assert(currentDeeper(retry).searchWorkIds.isEmpty)
    assert(!retry.responseSearches.exists(_.anchor == before.question.anchor))
    assert(retry.responseSearches.exists(_.work.workId == first.issuedWork.workId),
      "the existing first pool receipt is retained without lending its result to the deeper position")
    val next = completed(retry, List("f3e5" -> 30, "a1b1" -> 29, "b5e2" -> 28), now = 12L)
    val deeper = currentDeeper(next)
    assert(deeper.state != "stopped" && deeper.stopReason.isEmpty)
    assert(deeper.searchWorkIds.contains(retry.issuedWork.workId))
    assert(!deeper.searchWorkIds.contains(original.issuedWork.workId))
    val search = next.responseSearches.find(_.work.workId == retry.issuedWork.workId).getOrElse(
      fail("the completed deeper retry owns its own exact-position receipt"))
    assertEquals(search.anchor, before.question.anchor)
    assertEquals(search.work.requiredDepth, 16)
    assertEquals(search.work.rootRestriction, EngineRootRestriction.Unrestricted)
    assert(next.responseSearches.exists(_.work.workId == first.issuedWork.workId))
    assertEquals(currentInquiry(next).answer, None)
