package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import play.api.libs.json.*

/** The request and first two root reports are unchanged actual SG14 evidence.
  * Only the later one-ply report is invented scheduling evidence. Priority and
  * receipt ownership do not establish the strongest defense or a score cause.
  */
class FirstTargetPriorityTest extends munit.FunSuite:
  private lazy val fixture =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/response-continuation-prefix-sg14-produced.json.gz"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()
  private lazy val request = PositionCommentaryJobProtocol.parseJobRequest(
    Json.stringify((fixture \ "request").get).getBytes(UTF_8)).toOption.get
  private lazy val reports = (fixture \ "saved_reports").as[List[JsObject]].map(saved =>
    PositionCommentaryJobProtocol.parseEngineWorkReport(
      Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get)
  private val root = "a4b3"
  private val deferredPrefix = List(root, "c6e7")

  private def awaiting(state: CommentaryJobState): AwaitingEngineWork = state match
    case active: AwaitingEngineWork => active
    case other => fail(s"expected the current first-response survey, got ${other.getClass.getSimpleName}")

  private def prefix(): AwaitingEngineWork =
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 30_001L, request.engineProfile,
        investigateChoices = true, previewDepth = Some(8)), 1L).toOption.get
    reports.zipWithIndex.foreach { (report, index) =>
      val active = awaiting(state)
      assertEquals(report.reportedWorkId, active.issuedWork.workId)
      assertEquals(report.reportedExecutionKeySha256, active.issuedWork.executionKeySha256)
      state = CommentaryJobReducer.submitIssuedWorkReport(active, report, index.toLong + 2L).toOption.get
    }
    awaiting(state)

  private def currentTarget(state: CommentaryJobState): CommentaryResponseInquiry =
    state.responseInquiries.find(inquiry => inquiry.question.rootMove == root &&
      inquiry.question.captureMoves.contains("c6d4")).getOrElse(
      fail("the existing capture-versus-decline question keeps the exact current root owner"))

  private def assertFirstTarget(active: AwaitingEngineWork): Unit =
    val work = active.issuedWork
    val inquiry = currentTarget(active)
    assertEquals(work.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(work.searchAnchor.map(_.prefixMoves), Some(List(root)))
    assertEquals(work.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(work.requiredDepth, 16)
    assertEquals(work.enginePositionMovesUci, request.movePrefixUci :+ root)
    assertEquals(work.searchAnchor.map(_.positionId), Some(inquiry.question.childPositionId))
    assertEquals(work.searchAnchor.map(_.rootOccurrenceId), Some(inquiry.question.rootMoveOccurrenceId))
    assertEquals(inquiry.state, "awaiting_engine")
    assertEquals(inquiry.answer, None)
    assert(Set(inquiry.question.referenceMove, inquiry.question.alternativeMove) == Set("c6d4", "c6e7"))

  test("the current first opponent pool precedes a deeper independent exposure continuation"):
    val active = prefix()
    assertFirstTarget(active)
    val continuation = active.continuationInquiries.find(_.question.prefixMoves == deferredPrefix).getOrElse(
      fail("reordering the root question does not delete the already discovered continuation"))
    assertEquals(continuation.state, "deferred")
    assert(continuation.searchWorkIds.isEmpty)
    assert(active.responseSearches.isEmpty,
      "selecting the strongest-reply survey does not pretend that it has already completed")

  test("the old deeper-continuation key cannot answer the reordered first-response work"):
    val active = prefix()
    assertFirstTarget(active)
    val oldKey = (fixture \ "pending_snapshot" \ "issued_engine_work" \ "execution_key_sha256").as[String]
    assertNotEquals(active.issuedWork.executionKeySha256, oldKey)
    val unowned = EngineWorkReport.ExecutorFailed(active.engineProfile, active.issuedWork.workId,
      oldKey, EngineWorkFailure.BudgetExhausted)
    assert(CommentaryJobReducer.submitIssuedWorkReport(active, unowned, 10L).isLeft)
    assertEquals(currentTarget(active).state, "awaiting_engine")
    assertEquals(active.issuedWork.searchAnchor.map(_.prefixMoves), Some(List(root)))

  test("a completed pool owns its exact child receipt while an omitted capture class stays unanswered"):
    val active = prefix()
    assertFirstTarget(active)
    val work = active.issuedWork
    // These legal black choices and close synthetic scores isolate agenda and
    // ownership. They are not the independently completed book d24 analysis.
    val lines = List("c6e7" -> 30, "a7a6" -> 31, "g7f8" -> 32).map { (move, score) =>
      ReportedEngineLineSuffix(List(move), ReportedWhiteEngineScore.Centipawns(score), 16)
    }
    val next = CommentaryJobReducer.submitIssuedWorkReport(active, EngineWorkReport.Completed(active.engineProfile,
      work.workId, work.executionKeySha256, lines), 10L).toOption.get
    val search = next.responseSearches.find(_.work.workId == work.workId).getOrElse(
      fail("the completed first-target work owns its own response receipt"))
    assertEquals(search.anchor, work.searchAnchor.get)
    assertEquals(search.work.requiredDepth, 16)
    assertEquals(search.work.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(search.lines.head.evaluation.moves.head, "c6e7")
    assert(!search.lines.exists(line => line.evaluation.moves.head == "c6d4"))
    assertEquals(currentTarget(next).answer, None,
      "a top-N omission cannot certify the unreturned capture class or close the named comparison")
    assertEquals(currentTarget(next).question.rootMoveOccurrenceId, currentTarget(active).question.rootMoveOccurrenceId)

  test("expiry of the selected first-response work retains an unknown question without a completed receipt"):
    val active = prefix()
    assertFirstTarget(active)
    val stopped = CommentaryJobReducer.expireJob(active, 30_002L)
    assertEquals(currentTarget(stopped).question, currentTarget(active).question)
    assertEquals(currentTarget(stopped).answer, None)
    assert(!stopped.responseSearches.exists(_.work.workId == active.issuedWork.workId))
