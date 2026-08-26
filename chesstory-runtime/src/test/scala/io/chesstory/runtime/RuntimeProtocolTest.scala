package io.chesstory.runtime

import java.net.{ InetSocketAddress, URI }
import java.net.http.{ HttpClient, HttpRequest, HttpResponse }
import java.nio.charset.StandardCharsets
import java.util.concurrent.Executors

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration.*

import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, CanonicalPositionHistory }
import lila.chessjudgment.model.strategic.EngineLine
import lila.chessjudgment.analysis.assembly.{ MoveReviewInputNormalizer, RawMoveReviewInput }
import play.api.libs.json.*

class RuntimeProtocolTest extends munit.FunSuite:

  private val engineProfile = CommentaryEngineProfile.ServerAdmitted
  private val initialFen = chess.variant.Standard.initialFen.value

  private def history(moves: List[String] = Nil): CanonicalPositionHistory =
    CanonicalPositionHistory
      .from(initialFen, Nil, initialFen)
      .toOption
      .get
      .extend(moves)
      .toOption
      .get

  private def policy(deadline: Long = Long.MaxValue): CommentaryJobPolicy =
    CommentaryJobPolicy(16, 32, deadline, engineProfile)

  private def resultingFen(move: String): String =
    history().extend(List(move)).toOption.get.currentFen

  private def completedReport(
      work: IssuedEngineWork,
      lines: List[(List[String], Int)]
  ): EngineWorkReport =
    EngineWorkReport.Completed(
      work.engineProfile,
      work.workId,
      work.executionKeySha256,
      lines.map { case (moves, score) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(score), work.requiredDepth)
      }
    )

  private def rootLines: List[(List[String], Int)] = List(
    List("e2e4", "e7e5", "g1f3") -> 30,
    List("d2d4", "d7d5", "g1f3") -> 20,
    List("g1f3", "g8f6", "d2d4") -> 10
  )

  private val causalFen = "4k3/p7/3p4/8/4P3/8/8/4K1N1 w - - 0 1"
  private val causalPlayedMove = "g1f3"
  private val causalRootLines = List(
    List("g1f3", "a7a6", "e1d1") -> 30,
    List("e1d1", "a7a6", "g1f3") -> 40,
    List("g1h3", "a7a6", "e1d1") -> 0
  )
  private val causalReplyLines = List(
    List("a7a6", "e4e5", "d6e5", "f3e5") -> 30
  )
  private val causalBranchReplyLines = List(
    List("a7a6", "e4e5") -> 30,
    List("e8d7", "e4e5") -> 25,
    List("e8f7", "e4e5") -> 20
  )

  private def wireVariations(lines: List[(List[String], Int)], depth: Int): JsArray =
    JsArray(lines.map { case (moves, score) =>
      Json.obj("moves" -> moves, "scoreCp" -> score, "depth" -> depth)
    })

  private def wireProbeResult(
      probe: JsObject,
      lines: List[(List[String], Int)]
  ): JsObject =
    val base = Json.obj(
      "id" -> (probe \ "id").as[String],
      "replyLines" -> wireVariations(lines, (probe \ "depth").as[Int]),
      "depth" -> (probe \ "depth").as[Int]
    )
    base

  private val directInput = Json.obj(
    "fen" -> initialFen,
    "playedMoveUci" -> "d2d4",
    "variations" -> Json.arr(
      Json.obj("moves" -> Json.arr("e2e4", "e7e5", "g1f3"), "scoreCp" -> 30, "depth" -> 16),
      Json.obj("moves" -> Json.arr("d2d4", "d7d5", "g1f3"), "scoreCp" -> 20, "depth" -> 16)
    ),
    "ply" -> 0,
    "movePrefixUci" -> Json.arr()
  )

  private def directRequest(body: JsObject = directInput): JsObject =
    Json.obj(
      "schema_version" -> RuntimeProtocol.RequestSchema,
      "request_id" -> "direct-v6",
      "input" -> body
    )

  private def jobRequest(playedMove: String = "d2d4"): JsObject =
    Json.obj(
      "schema_version" -> PositionCommentaryJobProtocol.JobRequestSchema,
      "request_id" -> "job-v6",
      "variant" -> "standard",
      "initial_fen" -> initialFen,
      "move_prefix_uci" -> Json.arr(),
      "current_fen" -> initialFen,
      "focus" -> Json.obj(
        "kind" -> "played_move",
        "played_move_uci" -> playedMove,
        "resulting_fen" -> resultingFen(playedMove)
      ),
      "engine_profile" -> engineProfile.wireId
    )

  test("v6 job request owns one standard played-move focus and rejects unsupported schemas"):
    val parsed = PositionCommentaryJobProtocol
      .parseJobRequest(Json.stringify(jobRequest()).getBytes(StandardCharsets.UTF_8))
      .toOption
      .get
    assertEquals(parsed.playedMoveUci, "d2d4")
    assertEquals(parsed.resultingFen, resultingFen("d2d4"))
    assertEquals(parsed.engineProfile, engineProfile)

    val unsupported = jobRequest() + (
      "schema_version" -> JsString("unsupported.position-commentary.job-request")
    )
    assertEquals(
      PositionCommentaryJobProtocol.parseJobRequest(
        Json.stringify(unsupported).getBytes(StandardCharsets.UTF_8)
      ),
      Left(PositionCommentaryJobProtocol.PositionCommentaryJobWireFailure.UnsupportedJobRequestSchema)
    )

    val chess960 = jobRequest() + ("variant" -> JsString("chess960"))
    assertEquals(
      PositionCommentaryJobProtocol.parseJobRequest(
        Json.stringify(chess960).getBytes(StandardCharsets.UTF_8)
      ),
      Left(PositionCommentaryJobProtocol.PositionCommentaryJobWireFailure.UnsupportedVariant)
    )

  test("v6 issues one unrestricted root bundle instead of one search per legal move"):
    val started = CommentaryJobReducer
      .startJob(
        initialFen,
        Nil,
        initialFen,
        "d2d4",
        resultingFen("d2d4"),
        policy(),
        nowEpochMs = 0L
      )
      .toOption
      .get
    val root = started match
      case awaiting: AwaitingEngineWork => awaiting
      case other                       => fail(s"expected root work, found $other")
    assertEquals(root.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(root.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(root.issuedWork.multiPv, 3)
    assertEquals(root.issuedWork.enginePositionMovesUci, Nil)
    assertEquals(root.progress.physicalWorksIssued, 1)
    assert(root.progress.legalMoveCount > root.progress.physicalWorksIssued)

    val afterRoot = CommentaryJobReducer
      .submitIssuedWorkReport(root, completedReport(root.issuedWork, rootLines), nowEpochMs = 1L)
      .toOption
      .get
    afterRoot match
      case awaiting: AwaitingEngineWork =>
        assertEquals(awaiting.issuedWork.purpose, EngineWorkPurpose.CausalProbe)
      case completed: CompletedCommentaryJob =>
        assertEquals(completed.progress.selectedCommentariesCompleted, 1)
      case other => fail(s"covered focus must not schedule a focus comparison: $other")
    assertEquals(afterRoot.progress.physicalReportsAccepted, 1)

  test("v6 adds one restricted focus comparison only when played move is outside the root bundle"):
    val played = "a2a3"
    val root = CommentaryJobReducer
      .startJob(
        initialFen,
        Nil,
        initialFen,
        played,
        resultingFen(played),
        policy(),
        nowEpochMs = 0L
      )
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .get
    val afterRoot = CommentaryJobReducer
      .submitIssuedWorkReport(root, completedReport(root.issuedWork, rootLines), nowEpochMs = 1L)
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .get
    assertEquals(afterRoot.issuedWork.purpose, EngineWorkPurpose.FocusComparison)
    assertEquals(
      afterRoot.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List("e2e4", played))
    )
    assertEquals(afterRoot.issuedWork.multiPv, 2)
    assertEquals(afterRoot.progress.physicalWorksIssued, 2)

    val afterFocus = CommentaryJobReducer
      .submitIssuedWorkReport(
        afterRoot,
        completedReport(
          afterRoot.issuedWork,
          List(
            List("e2e4", "e7e5", "g1f3") -> 30,
            List("a2a3", "e7e5", "g1f3") -> -10
          )
        ),
        nowEpochMs = 2L
      )
      .toOption
      .get
    assertEquals(afterFocus.progress.physicalReportsAccepted, 2)
    afterFocus match
      case awaiting: AwaitingEngineWork =>
        assertEquals(awaiting.issuedWork.purpose, EngineWorkPurpose.CausalProbe)
      case _: CompletedCommentaryJob => ()
      case other => fail(s"focused review did not enter commentary assembly: $other")

  test("v6 fulfills an additional causal search inside the same monotonic job ledger"):
    val causalHistory = CanonicalPositionHistory.from(causalFen, Nil, causalFen).toOption.get
    val causalResultingFen = causalHistory.extend(List(causalPlayedMove)).toOption.get.currentFen
    val root = CommentaryJobReducer
      .startJob(
        causalFen,
        Nil,
        causalFen,
        causalPlayedMove,
        causalResultingFen,
        CommentaryJobPolicy(20, 32, Long.MaxValue, engineProfile),
        nowEpochMs = 0L
      )
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .get
    val completeRootLines = causalRootLines
    val afterRoot = CommentaryJobReducer
      .submitIssuedWorkReport(root, completedReport(root.issuedWork, completeRootLines), nowEpochMs = 1L)
      .toOption
      .get
    val causal = afterRoot match
      case value: AwaitingEngineWork => value
      case other                     => fail(s"expected causal work after root report, found $other")
    assertEquals(causal.issuedWork.purpose, EngineWorkPurpose.CausalProbe)
    assertEquals(
      causal.issuedWork.commentaryLinePrefixUci,
      List("a7a6", "e4e5"),
      clues(causal.issuedWork)
    )
    assertEquals(causal.issuedWork.multiPv, 1, clues(causal.issuedWork))
    assertEquals(causal.progress.physicalWorksIssued, 2)
    assertEquals(causal.progress.physicalReportsAccepted, 1)

    val forcedPrefix = causal.issuedWork.commentaryLinePrefixUci
    val causalSuffixes = causalReplyLines.map { case (moves, score) =>
      assertEquals(moves.take(forcedPrefix.size), forcedPrefix)
      moves.drop(forcedPrefix.size) -> score
    }
    val afterContinuation = CommentaryJobReducer
      .submitIssuedWorkReport(causal, completedReport(causal.issuedWork, causalSuffixes), nowEpochMs = 2L)
      .toOption
      .get
    val replyCensus = afterContinuation match
      case value: AwaitingEngineWork => value
      case other                     => fail(s"expected one reply census after exact causal continuation, found $other")
    assertEquals(replyCensus.issuedWork.purpose, EngineWorkPurpose.CausalProbe)
    assertEquals(replyCensus.issuedWork.commentaryLinePrefixUci, Nil)
    assertEquals(replyCensus.issuedWork.multiPv, 3)
    val completed = CommentaryJobReducer
      .submitIssuedWorkReport(
        replyCensus,
        completedReport(replyCensus.issuedWork, causalBranchReplyLines),
        nowEpochMs = 3L
      )
      .toOption
      .collect { case value: CompletedCommentaryJob => value }
      .getOrElse(fail("expected completion after the bounded reply census"))
    assertEquals(completed.progress.physicalWorksIssued, 3)
    assertEquals(completed.progress.physicalReportsAccepted, 3)
    assertEquals(completed.progress.causalWavesCompleted, 2)
    assertEquals(completed.progress.selectedCommentariesCompleted, 1)

  test("execution identity and minimal v6 engine report bind exactly one outstanding work"):
    val root = CommentaryJobReducer
      .startJob(
        initialFen,
        Nil,
        initialFen,
        "d2d4",
        resultingFen("d2d4"),
        policy(),
        nowEpochMs = 0L
      )
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .get
    val reportJson = Json.obj(
      "schema_version" -> PositionCommentaryJobProtocol.EngineWorkReportSchema,
      "engine_profile" -> engineProfile.wireId,
      "work_id" -> root.issuedWork.workId,
      "execution_key_sha256" -> root.issuedWork.executionKeySha256,
      "outcome" -> Json.obj(
        "kind" -> "completed",
        "line_suffixes" -> rootLines.map { case (moves, score) =>
          Json.obj(
            "moves" -> moves,
            "depth" -> 16,
            "white_score" -> Json.obj("kind" -> "cp", "value" -> score)
          )
        }
      )
    )
    val parsed = PositionCommentaryJobProtocol
      .parseEngineWorkReport(Json.stringify(reportJson).getBytes(StandardCharsets.UTF_8))
      .toOption
      .get
    assertEquals(parsed.reportedExecutionKeySha256, root.issuedWork.executionKeySha256)
    val wrongKey = parsed match
      case EngineWorkReport.Completed(profile, workId, _, lines) =>
        EngineWorkReport.Completed(profile, workId, "0" * 64, lines)
      case _ => fail("expected completed report")
    assertEquals(
      CommentaryJobReducer.submitIssuedWorkReport(root, wrongKey, 1L),
      Left(EngineWorkReportRejection.WorkNotOutstanding)
    )

  test("admission replays browser lines and truncates only at an automatic terminal"):
    val drawFen = "7k/8/8/8/K7/1p6/8/8 w - - 0 1"
    val drawHistory = CanonicalPositionHistory.from(drawFen, Nil, drawFen).toOption.get
    val work = IssuedEngineWork.create(
      engineProfile,
      "work:0",
      EngineWorkPurpose.RootSearch,
      drawFen,
      Nil,
      drawFen,
      EngineRootRestriction.Unrestricted,
      drawFen,
      Nil,
      EngineSearchLimits(16, 5_000_000, 5_000, 1),
      6_000
    )
    assertEquals(drawHistory.currentFen, drawFen)
    val suffixMoves = List("a4b3", "h8h7", "b3c3")
    assertEquals(
      EngineLineAdmission.bindReportedLineSuffixes(
        work,
        List(
          ReportedEngineLineSuffix(
            suffixMoves,
            ReportedWhiteEngineScore.Centipawns(-24),
            16
          )
        )
      ),
      None
    )
    val admitted = EngineLineAdmission.bindReportedLineSuffixes(
      work,
      List(
        ReportedEngineLineSuffix(
          suffixMoves,
          ReportedWhiteEngineScore.Centipawns(0),
          16
        )
      )
    )
    assertEquals(
      admitted,
      Some(
        List(
          CandidateLineEvaluation.ExactAutomaticTerminal(
            List("a4b3"),
            AutomaticTerminal.InsufficientMaterial
          )
        )
      )
    )

  test("direct development response emits the v6 schema"):
    val result = RuntimeProtocol.evaluate(directRequest())
    assertEquals(result.httpStatus, 200, clues(result.body))
    assertEquals((result.body \ "schema_version").as[String], "chesstory.move-meaning.response.v6")
    assertEquals((result.body \ "status").as[String], "ready")
    assert((result.body \ "move_commentary" \ "primary").toOption.nonEmpty)

  test("selected causal projection carries its exact proof line, channel, and proof segment"):
    val causalHistory = CanonicalPositionHistory.from(causalFen, Nil, causalFen).toOption.get
    causalRootLines.foreach { case (moves, _) =>
      assert(causalHistory.extend(moves).isRight, clues(moves))
    }
    assert(
      MoveReviewInputNormalizer
        .normalize(
          RawMoveReviewInput(
            causalFen,
            causalPlayedMove,
            causalRootLines.map { case (moves, score) => EngineLine(moves, score, depth = 20) }
          )
        )
        .nonEmpty
    )
    val selectedCauseInput = Json.obj(
      "fen" -> causalFen,
      "playedMoveUci" -> causalPlayedMove,
      "variations" -> wireVariations(causalRootLines, depth = 20)
    )
    val first = RuntimeProtocol.evaluate(directRequest(selectedCauseInput))
    assertEquals(first.httpStatus, 202, clues(first.body))
    val continuationProbe = (first.body \ "probe_requests").as[List[JsObject]].head
    assert(
      (continuationProbe \ "id").as[String].length <= 256,
      clues((continuationProbe \ "id").as[String])
    )
    val continuationResult = wireProbeResult(continuationProbe, causalReplyLines)
    val afterContinuation = RuntimeProtocol.evaluate(
      directRequest(selectedCauseInput + ("probeResults" -> Json.arr(continuationResult)))
    )
    assertEquals(afterContinuation.httpStatus, 202, clues(afterContinuation.body))
    val replyProbe = (afterContinuation.body \ "probe_requests").as[List[JsObject]].head
    val replyResult = wireProbeResult(replyProbe, causalBranchReplyLines)
    val ready = RuntimeProtocol.evaluate(
      directRequest(
        selectedCauseInput +
          ("probeResults" -> Json.arr(continuationResult, replyResult))
      )
    )
    assertEquals(ready.httpStatus, 200, clues(ready.body))
    val causalExplanations = (ready.body \ "move_commentary" \ "causal_explanations").as[List[JsObject]]
    assertEquals(causalExplanations.size, 1)
    val facets = (causalExplanations.head \ "facets").as[List[JsObject]]
    assertEquals(facets.size, 1)
    val facet = facets.head
    assertEquals((facet \ "proof_line_moves").toOption, None)
    val channel = (facet \ "channels")(0).as[JsObject]
    assert((channel \ "channel_id").as[String].startsWith("cause-channel:"))
    assert((channel \ "causal_signature").as[String].nonEmpty)
    val proofLine = (channel \ "proof_line_moves").as[List[String]]
    assert(proofLine.nonEmpty)
    val steps = (channel \ "proof_segment" \ "steps").as[List[JsObject]]
    assert(steps.nonEmpty)
    steps.foreach: step =>
      val offset = (step \ "ply_offset").as[Int]
      assertEquals((step \ "move_uci").as[String], proofLine(offset))
    val planEvents = steps.flatMap(step => (step \ "plan_event").asOpt[JsObject])
    assert(planEvents.nonEmpty)
    planEvents.foreach { event =>
      assert((event \ "goal_kind").as[String].nonEmpty)
      assert((event \ "actor_from").as[String].matches("[a-h][1-8]"))
      assert((event \ "actor_to").as[String].matches("[a-h][1-8]"))
    }

  test("HTTP v6 route keeps Stockfish work in the browser and stops an executor failure"):
    val server = ChesstoryRuntime.start(new InetSocketAddress("127.0.0.1", 0), None, workerCount = 1)
    try
      val client = HttpClient.newHttpClient()
      val jobs = URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/v1/position-commentary-jobs")
      val created = client.send(
        HttpRequest.newBuilder(jobs)
          .header("Content-Type", "application/json")
          .POST(HttpRequest.BodyPublishers.ofString(Json.stringify(jobRequest())))
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      assertEquals(created.statusCode(), 201)
      val awaiting = Json.parse(created.body()).as[JsObject]
      assertEquals((awaiting \ "schema_version").as[String], PositionCommentaryJobProtocol.JobStatusSchema)
      assertEquals((awaiting \ "issued_engine_work" \ "purpose").as[String], "root_search")
      assertEquals((awaiting \ "issued_engine_work" \ "variant").as[String], "standard")
      val workId = (awaiting \ "issued_engine_work" \ "work_id").as[String]
      val executionKey = (awaiting \ "issued_engine_work" \ "execution_key_sha256").as[String]
      val reportUri = URI.create(
        s"$jobs/${(awaiting \ "job_id").as[String]}/engine-work-reports"
      )
      val failed = client.send(
        HttpRequest.newBuilder(reportUri)
          .header("Content-Type", "application/json")
          .POST(
            HttpRequest.BodyPublishers.ofString(
              Json.stringify(
                Json.obj(
                  "schema_version" -> PositionCommentaryJobProtocol.EngineWorkReportSchema,
                  "engine_profile" -> engineProfile.wireId,
                  "work_id" -> workId,
                  "execution_key_sha256" -> executionKey,
                  "outcome" -> Json.obj("kind" -> "executor_failed", "failure_code" -> "engine_failure")
                )
              )
            )
          )
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      assertEquals(failed.statusCode(), 200)
      val stopped = Json.parse(failed.body())
      assertEquals((stopped \ "state").as[String], "stopped")
      assertEquals((stopped \ "stop_condition").as[String], "engine_execution_failed")
      assertEquals((stopped \ "progress" \ "physical_reports_accepted").as[Int], 1)
    finally server.stop(0)

  test("HTTP v6 completes a focused review from one browser-owned engine report"):
    val forcedFen = "B5Q1/5n1k/2n5/1P3p2/1P1PN1P1/1b1N2R1/8/2B3K1 b - - 0 56"
    val forcedMove = "h7g8"
    val forcedResultingFen = "B5k1/5n2/2n5/1P3p2/1P1PN1P1/1b1N2R1/8/2B3K1 w - - 0 57"
    val request = Json.obj(
      "schema_version" -> PositionCommentaryJobProtocol.JobRequestSchema,
      "request_id" -> "forced-v6-roundtrip",
      "variant" -> "standard",
      "initial_fen" -> forcedFen,
      "move_prefix_uci" -> Json.arr(),
      "current_fen" -> forcedFen,
      "focus" -> Json.obj(
        "kind" -> "played_move",
        "played_move_uci" -> forcedMove,
        "resulting_fen" -> forcedResultingFen
      ),
      "engine_profile" -> engineProfile.wireId
    )
    val server = ChesstoryRuntime.start(new InetSocketAddress("127.0.0.1", 0), None, workerCount = 2)
    try
      val client = HttpClient.newHttpClient()
      val jobs = URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/v1/position-commentary-jobs")
      val created = client.send(
        HttpRequest.newBuilder(jobs)
          .header("Content-Type", "application/json")
          .POST(HttpRequest.BodyPublishers.ofString(Json.stringify(request)))
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      assertEquals(created.statusCode(), 201)
      val awaiting = Json.parse(created.body()).as[JsObject]
      assertEquals((awaiting \ "issued_engine_work" \ "purpose").as[String], "root_search")
      assertEquals((awaiting \ "issued_engine_work" \ "search_limits" \ "multi_pv").as[Int], 1)
      val report = Json.obj(
        "schema_version" -> PositionCommentaryJobProtocol.EngineWorkReportSchema,
        "engine_profile" -> engineProfile.wireId,
        "work_id" -> (awaiting \ "issued_engine_work" \ "work_id").as[String],
        "execution_key_sha256" -> (awaiting \ "issued_engine_work" \ "execution_key_sha256").as[String],
        "outcome" -> Json.obj(
          "kind" -> "completed",
          "line_suffixes" -> Json.arr(
            Json.obj(
              "moves" -> Json.arr(forcedMove),
              "depth" -> 16,
              "white_score" -> Json.obj("kind" -> "cp", "value" -> 0)
            )
          )
        )
      )
      val reportUri = URI.create(
        s"$jobs/${(awaiting \ "job_id").as[String]}/engine-work-reports"
      )
      val completed = client.send(
        HttpRequest.newBuilder(reportUri)
          .header("Content-Type", "application/json")
          .POST(HttpRequest.BodyPublishers.ofString(Json.stringify(report)))
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      assertEquals(completed.statusCode(), 200, completed.body())
      val response = Json.parse(completed.body())
      assertEquals(
        (response \ "schema_version").as[String],
        "chesstory.position-commentary.response.v6",
        clues(completed.body())
      )
      assertEquals((response \ "progress" \ "phase").as[String], "completed")
      assertEquals((response \ "progress" \ "physical_works_issued").as[Int], 1)
      assertEquals((response \ "progress" \ "physical_reports_accepted").as[Int], 1)
      assertEquals((response \ "result" \ "kind").as[String], "forced_single_move")
      assertEquals((response \ "result" \ "move_uci").as[String], forcedMove)
    finally server.stop(0)

  test("hundreds of concurrent v6 creations preserve exact capacity and independent ledgers"):
    val registry = CommentaryJobRegistry(maximumRetainedJobs = 128, terminalJobRetentionMillis = 120000L)
    val executor = Executors.newFixedThreadPool(16)
    given ExecutionContext = ExecutionContext.fromExecutorService(executor)
    try
      val created = Await.result(
        Future.traverse((0 until 200).toList) { index =>
          Future(
            registry.createJob(
              requestId = s"concurrent-$index",
              initialFen = initialFen,
              movePrefixUci = Nil,
              currentFen = initialFen,
              playedMoveUci = "e2e4",
              resultingFen = resultingFen("e2e4"),
              policy = policy(),
              nowEpochMs = 0L
            )
          )
        },
        30.seconds
      )
      val snapshots = created.flatMap(_.toOption)
      assertEquals(snapshots.size, 128)
      assertEquals(snapshots.map(_.jobId).distinct.size, 128)
      assertEquals(
        created.count(_.left.toOption.contains(CommentaryJobCreationRejection.RegistryCapacityReached)),
        72
      )
      assert(snapshots.forall(_.state.isInstanceOf[AwaitingEngineWork]))
      assert(snapshots.forall(snapshot =>
        snapshot.state.asInstanceOf[AwaitingEngineWork].issuedWork.workId == "work:0"
      ))
    finally
      executor.shutdownNow()
