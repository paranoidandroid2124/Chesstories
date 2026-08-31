package io.chesstory.runtime

import java.net.{ InetSocketAddress, URI }
import java.net.http.{ HttpClient, HttpRequest, HttpResponse }
import java.nio.charset.StandardCharsets
import java.util.concurrent.Executors

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration.*

import lila.chessjudgment.model.{ ProbeResolution, ProbeResult }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, CanonicalPositionHistory }
import lila.chessjudgment.analysis.assembly.{ MoveReviewJudgmentOrchestrator, RawMoveReviewInput }
import lila.chessjudgment.model.line.EngineLine
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
    CommentaryJobPolicy(16, deadline, engineProfile)

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

  private def awaitCausalWork(
      initial: CommentaryJobState,
      targetPrefix: List[String],
      continuationFor: IssuedEngineWork => String,
      nowEpochMs: Long
  ): IssuedEngineWork =
    def target(state: CommentaryJobState): Option[IssuedEngineWork] = state match
      case awaiting: AwaitingEngineWork
          if awaiting.issuedWork.enginePositionMovesUci.endsWith(targetPrefix) =>
        Some(awaiting.issuedWork)
      case _ => None

    var state = initial
    var reports = 0
    while target(state).isEmpty do
      state match
        case awaiting: AwaitingEngineWork =>
          assertEquals(awaiting.issuedWork.purpose, EngineWorkPurpose.CausalProbe)
          state = CommentaryJobReducer
            .submitIssuedWorkReport(
              awaiting,
              completedReport(
                awaiting.issuedWork,
                List(List(continuationFor(awaiting.issuedWork)) -> 0)
              ),
              nowEpochMs + reports
            )
            .toOption
            .get
          reports += 1
          assert(reports <= 8, clues(state))
        case other => fail(s"existing root/focus evidence hid the causal probe: $other")
    target(state).get

  private def rootLines: List[(List[String], Int)] = List(
    List("e2e4", "e7e5", "g1f3") -> 30,
    List("d2d4", "d7d5", "g1f3") -> 20,
    List("g1f3", "g8f6", "d2d4") -> 10
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

  test("focused best cannot replace the root-owned reference evaluation"):
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
    val focus = CommentaryJobReducer
      .submitIssuedWorkReport(root, completedReport(root.issuedWork, rootLines), nowEpochMs = 1L)
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .get

    val stopped = CommentaryJobReducer
      .submitIssuedWorkReport(
        focus,
        completedReport(
          focus.issuedWork,
          List(
            List("e2e4", "e7e5", "g1f3") -> 31,
            List("a2a3", "e7e5", "g1f3") -> -10
          )
        ),
        nowEpochMs = 2L
      )
      .toOption
      .collect { case value: StoppedCommentaryJob => value }
      .getOrElse(fail("a conflicting focused reference evaluation must fail closed"))

    assertEquals(stopped.stopCondition, CommentaryJobStopCondition.MoveReviewPreparationFailed)

  test("one matching root PV still issues a browser-owned causal search"):
    val rootFen = "7k/8/8/PP6/8/8/8/4K3 w - - 0 1"
    val playedMove = "a5a6"
    val positionHistory = CanonicalPositionHistory.from(rootFen, Nil, rootFen).toOption.get
    val root = CommentaryJobReducer
      .startJob(
        rootFen,
        Nil,
        rootFen,
        playedMove,
        positionHistory.extend(List(playedMove)).toOption.get.currentFen,
        policy(),
        nowEpochMs = 0L
      )
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .get
    val afterRoot = CommentaryJobReducer
      .submitIssuedWorkReport(
        root,
        completedReport(
          root.issuedWork,
          List(
            List("b5b6", "h8g8", "b6b7") -> 10,
            List("a5a6", "h8g8", "a6a7") -> 0,
            List("e1e2", "h8g8", "e2e3") -> -10
          )
        ),
        nowEpochMs = 1L
      )
      .toOption
      .get
    val causal = awaitCausalWork(
      afterRoot,
      List(playedMove, "h8g8"),
      _ => "a6a7",
      nowEpochMs = 2L
    )

    assertEquals(causal.purpose, EngineWorkPurpose.CausalProbe)
    assertEquals(causal.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(causal.multiPv, 1)

  test("competing root and focus PVs still issue a browser-owned causal search"):
    val rootFen = "7k/8/8/PP6/8/8/8/4K3 w - - 0 1"
    val playedMove = "b5b6"
    val positionHistory = CanonicalPositionHistory.from(rootFen, Nil, rootFen).toOption.get
    val root = CommentaryJobReducer
      .startJob(
        rootFen,
        Nil,
        rootFen,
        playedMove,
        positionHistory.extend(List(playedMove)).toOption.get.currentFen,
        policy(),
        nowEpochMs = 0L
      )
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .get
    val focus = CommentaryJobReducer
      .submitIssuedWorkReport(
        root,
        completedReport(
          root.issuedWork,
          List(
            List("a5a6", "h8g8", "a6a7") -> 10,
            List("e1e2", "h8g8", "e2e3") -> 0,
            List("e1d1", "h8g8", "d1d2") -> -10
          )
        ),
        nowEpochMs = 1L
      )
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .get
    assertEquals(focus.issuedWork.purpose, EngineWorkPurpose.FocusComparison)
    val afterFocus = CommentaryJobReducer
      .submitIssuedWorkReport(
        focus,
        completedReport(
          focus.issuedWork,
          List(
            List("a5a6", "h8g8", "b5b6") -> 10,
            List("b5b6", "h8g8", "b6b7") -> -20
          )
        ),
        nowEpochMs = 2L
      )
      .toOption
      .get
    val causal = awaitCausalWork(
      afterFocus,
      List("a5a6", "h8g8"),
      work =>
        if work.enginePositionMovesUci.headOption.contains("a5a6") then "a6a7"
        else "b6b7",
      nowEpochMs = 3L
    )

    assertEquals(causal.purpose, EngineWorkPurpose.CausalProbe)
    assertEquals(causal.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(causal.multiPv, 1)

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
    def admittedEvaluations(
        suffixes: List[ReportedEngineLineSuffix]
    ): Option[List[CandidateLineEvaluation]] =
      for
        physical <- EngineLineAdmission.admitReportedLineSuffixes(work, suffixes)
        admitted <- EngineLineAdmission.bindAdmittedLineSuffixes(work, physical)
      yield admitted.map(_.evaluation)
    assertEquals(
      admittedEvaluations(
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
    val admitted = admittedEvaluations(
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

  test("causal prefix and cached physical suffix rebind to one logical occurrence without replay"):
    val logicalOrigin = history(List("e2e4"))
    val logicalPrefix = List("e7e5")
    val searchHistory = logicalOrigin.extend(logicalPrefix).toOption.get
    val work = IssuedEngineWork.create(
      engineProfile,
      "work:cached",
      EngineWorkPurpose.CausalProbe,
      searchHistory.initialFen,
      searchHistory.movePrefixUci,
      searchHistory.currentFen,
      EngineRootRestriction.Unrestricted,
      logicalOrigin.currentFen,
      logicalPrefix,
      EngineSearchLimits(16, 5_000_000, 5_000, 1),
      6_000
    )
    val reported = List(
      ReportedEngineLineSuffix(
        List("g1f3", "b8c6"),
        ReportedWhiteEngineScore.Centipawns(20),
        16
      )
    )
    val physical = EngineLineAdmission
      .admitReportedLineSuffixes(work, reported)
      .getOrElse(fail("expected one physical admission"))
    val first = EngineLineAdmission
      .bindAdmittedLineSuffixes(work, physical)
      .flatMap(_.headOption)
      .getOrElse(fail("expected the first logical binding"))
    val rebound = EngineLineAdmission
      .bindAdmittedLineSuffixes(work, physical)
      .flatMap(_.headOption)
      .getOrElse(fail("expected the cached logical binding"))
    val firstSteps = first.continuationAfter(logicalOrigin).getOrElse(fail("expected retained causal steps"))
    val reboundSteps = rebound
      .continuationAfter(logicalOrigin)
      .getOrElse(fail("expected cached retained causal steps"))
    val siblingOccurrence = history(List("d2d4"))

    assertEquals(firstSteps.map(_.uci), logicalPrefix ++ reported.head.moves)
    assertEquals(reboundSteps.map(_.uci), firstSteps.map(_.uci))
    assertEquals(first.continuationAfter(siblingOccurrence), None)
    assert(
      firstSteps.zip(reboundSteps).forall { case (left, right) =>
        left.move.asInstanceOf[AnyRef] eq right.move.asInstanceOf[AnyRef]
      }
    )

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
      assertEquals((response \ "result" \ "presentation").toOption, None)
    finally server.stop(0)

  test("v6 commentary serializes an occurrence-bound L2 resource differential"):
    val rootFen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1"
    val referenceMoves = List("c4f7", "f8f7", "d1d8")
    val playedMoves = List("d1d8", "f8d8")
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = playedMoves.head,
          variations = List(
            EngineLine(referenceMoves, scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(playedMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected an evidence-backed judgment packet"))

    val commentary = RuntimeProtocol.moveCommentaryJson(packet)
    assertEquals((commentary \ "primary" \ "presentation").toOption, None)
    assertEquals(
      commentary.keys.intersect(Set("structural_idea_units", "responsibility_links")),
      Set.empty[String]
    )
    val moveOrderFacet = (commentary \ "causal_explanations").as[List[JsObject]]
      .flatMap(idea => (idea \ "facets").as[List[JsObject]])
      .find(facet => (facet \ "kind").as[String] == "wrong_move_order")
      .getOrElse(fail("expected the selected L2 cause in public commentary"))
    val channel = (moveOrderFacet \ "channels").as[List[JsObject]] match
      case exact :: Nil => exact
      case other        => fail(s"expected one exact L2 channel, found ${other.size}")

    assertEquals((moveOrderFacet \ "proof_confidence").as[String], "legal_replay_verified")
    assertEquals((moveOrderFacet \ "effect_mode").as[String], "alternative_resource")
    assertEquals((moveOrderFacet \ "exposure").as[String], "primary")
    assertEquals((moveOrderFacet \ "source_side").as[String], "reference")
    assertEquals((moveOrderFacet \ "comparison_kind").as[String], "played_vs_best")
    assert(!moveOrderFacet.keys.contains("only_move_qualifiers"))
    assert(!moveOrderFacet.keys.contains("event_move"))
    assertEquals((channel \ "direct_change").as[String], "occurred")
    assertEquals((channel \ "played_change").as[String], "missed")
    val forbiddenDuplicateFields = Set(
      "actor",
      "targets",
      "mechanisms",
      "consequences",
      "witnesses",
      "proof_line_moves",
      "horizon",
      "proof_segment"
    )
    assertEquals(channel.keys.intersect(forbiddenDuplicateFields), Set.empty[String])
    val proof = (channel \ "resource_differential_proof").as[JsObject]
    assertEquals((proof \ "family").as[String], "immediate_forced_reply_resource_differential")
    assertEquals((proof \ "trigger_mechanism").as[String], "forced_displacement")
    assert((proof \ "semantic_id").as[String].matches("[0-9a-f]{64}"))
    assert((proof \ "occurrence_id").as[String].matches("[0-9a-f]{64}"))
    assert((proof \ "dependency_fingerprint").as[String].matches("[0-9a-f]{64}"))
    val referenceBranch = (proof \ "counterfactual_reference_branch").as[JsObject]
    val playedBranch = (proof \ "played_root_branch").as[JsObject]
    assertEquals((referenceBranch \ "line_role").as[String], "best_reference")
    assertEquals((referenceBranch \ "branch_role").as[String], "counterfactual_reference")
    assertEquals(
      (referenceBranch \ "root_provenance").as[String],
      "counterfactual_analyzed_root"
    )
    assert((referenceBranch \ "branch_id").as[String].matches("[0-9a-f]{64}"))
    assertEquals(
      (referenceBranch \ "steps").as[List[JsObject]].map(step => (step \ "provenance").as[String]),
      List.fill(3)("certified_analysis_move")
    )
    assertEquals(
      (referenceBranch \ "steps").as[List[JsObject]].map(step => (step \ "move_uci").as[String]),
      referenceMoves
    )
    assertEquals((playedBranch \ "line_role").as[String], "played")
    assertEquals((playedBranch \ "branch_role").as[String], "observed_played_root")
    assertEquals((playedBranch \ "root_provenance").as[String], "observed_game_root")
    assertEquals(
      (playedBranch \ "steps").as[List[JsObject]].map(step => (step \ "provenance").as[String]),
      List("observed_game_move", "certified_analysis_move")
    )
    assertEquals(
      (playedBranch \ "steps").as[List[JsObject]].map(step => (step \ "move_uci").as[String]),
      playedMoves
    )
    assertEquals((proof \ "realizing_move").as[String], "d1d8")
    assertEquals((proof \ "played_root_branch_legal_defense_move").as[String], "f8d8")
    assertEquals(proof.keys.intersect(Set("premise_ids", "closed_absence")), Set.empty[String])
    val proofPaths = (proof \ "proof_paths").as[List[JsObject]]
    assertEquals(proofPaths.size, 1)
    val proofPath = proofPaths.head
    assert((proofPath \ "path_occurrence_id").as[String].matches("[0-9a-f]{64}"))
    val premises = (proofPath \ "premises").as[List[JsObject]]
    assertEquals(
      premises.map(premise => (premise \ "role").as[String]),
      List(
        "created_check_response",
        "reference_capture_recapture",
        "played_capture_recapture"
      )
    )
    assertEquals((premises.head \ "branch_id").as[String], (referenceBranch \ "branch_id").as[String])
    assertEquals((premises(1) \ "step_index").as[Int], 2)
    assertEquals((premises(2) \ "branch_id").as[String], (playedBranch \ "branch_id").as[String])
    val absenceUses = (proofPath \ "closed_absence_uses").as[List[JsObject]]
    assertEquals(absenceUses.size, 1)
    val absenceUse = absenceUses.head
    assertEquals(
      (absenceUse \ "issuer").as[String],
      "position_relation_extractor.closed_relation_inventory"
    )
    assert((absenceUse \ "issuer_evidence_id").as[String].nonEmpty)
    assert((absenceUse \ "issuer_occurrence_id").as[String].matches("[0-9a-f]{64}"))
    assertEquals((absenceUse \ "query").as[String], "legal-capture:black:d8")
    assertEquals((absenceUse \ "branch_id").as[String], (referenceBranch \ "branch_id").as[String])
    assertEquals((absenceUse \ "after_step_index").as[Int], 2)
    assertEquals((absenceUse \ "position" \ "scope").as[String], "best_line")
    assertEquals((proof \ "participants" \ "trigger" \ "from").as[String], "c4")
    assertEquals((proof \ "participants" \ "realizer" \ "to").as[String], "d8")
    assertEquals((proof \ "participants" \ "captured_target" \ "square").as[String], "d8")
    assertEquals((proof \ "participants" \ "played_defense" \ "move_uci").as[String], "f8d8")
    assertEquals((proof \ "participants" \ "disabled_defender" \ "side").as[String], "black")
    assertEquals((proof \ "participants" \ "disabled_defender" \ "piece").as[String], "rook")
    assertEquals((proof \ "participants" \ "disabled_defender" \ "square").as[String], "f8")

  test("v6 commentary serializes the exact sole-recapturer removal proof"):
    val rootFen = "8/4p2k/5n2/3r2B1/8/8/8/K2Q4 w - - 0 1"
    val referenceMoves = List("g5f6", "e7f6", "d1d5")
    val playedMoves = List("d1d5", "f6d5")
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = playedMoves.head,
          variations = List(
            EngineLine(referenceMoves, scoreCp = 600, depth = 24),
            EngineLine(playedMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected an evidence-backed defense-obligation packet"))

    val moveOrderProofRecords = packet.evidenceGraph.records.filter(_.ref.layer == EvidenceLayer.CausalProof)
    assertEquals(moveOrderProofRecords.size, 1)
    val defenseCauseRecords = packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder &&
            cause.supportEvidence.exists(source => moveOrderProofRecords.exists(_.ref == source)) => record
    }
    assertEquals(
      defenseCauseRecords.size,
      1,
      clues(
        moveOrderProofRecords.map(record => record.ref -> packet.evidenceGraph.proofEligible(record)),
        packet.evidenceGraph.records.collect {
          case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) =>
            (ref, cause.kind, cause.supportEvidence)
        }
      )
    )
    val selectedCauseIds = packet.playerFacingClaimDecisions.flatMap(_.causeSelections.map(_.causeEvidence.id)).toSet
    assert(
      defenseCauseRecords.exists(record => selectedCauseIds(record.ref.id)),
      clues(defenseCauseRecords.map(_.ref.id), packet.playerFacingClaimDecisions, packet.causeDispositionLedger)
    )

    val proof = RuntimeProtocol.moveCommentaryJson(packet)
      .value("causal_explanations")
      .as[List[JsObject]]
      .flatMap(idea => (idea \ "facets").as[List[JsObject]])
      .find(facet => (facet \ "kind").as[String] == "wrong_move_order")
      .toList
      .flatMap(facet => (facet \ "channels").as[List[JsObject]])
      .flatMap(channel => (channel \ "defense_obligation_change_proof").asOpt[JsObject]) match
        case exact :: Nil => exact
        case other        => fail(s"expected one public removal proof, found ${other.size}")

    assertEquals((proof \ "contract").as[String], "defense_obligation_change")
    assertEquals((proof \ "mechanism").as[String], "sole_recapturer_removal")
    assertEquals((proof \ "later_exploit_move").as[String], "d1d5")
    assertEquals((proof \ "played_sole_recapture_move").as[String], "f6d5")
    val premises = (proof \ "proof_paths" \ 0 \ "premises").as[List[JsObject]]
    assertEquals(
      premises.map(premise => (premise \ "role").as[String]),
      List(
        "reference_defender_removal",
        "reference_later_exploit_inventory",
        "played_immediate_exploit_inventory"
      )
    )
    assertEquals(premises.map(premise => (premise \ "step_index").as[Int]), List(0, 2, 0))
    assertEquals((proof \ "participants" \ "remover" \ "to").as[String], "f6")
    assertEquals((proof \ "participants" \ "removal_recapture" \ "move_uci").as[String], "e7f6")
    assertEquals((proof \ "participants" \ "removed_defender" \ "piece").as[String], "knight")
    assertEquals((proof \ "participants" \ "removed_defender" \ "square").as[String], "f6")
    assertEquals((proof \ "participants" \ "later_exploit" \ "to").as[String], "d5")
    assertEquals((proof \ "participants" \ "captured_target" \ "piece").as[String], "rook")
    assertEquals((proof \ "participants" \ "played_sole_recapture" \ "move_uci").as[String], "f6d5")
    assertEquals(
      (proof \ "proof_paths" \ 0 \ "closed_absence_uses" \ 0 \ "query").as[String],
      "legal-capture:black:d5"
    )

  test("v6 commentary serializes the closed passed-pawn result without a generic proof segment"):
    val raw = RawMoveReviewInput(
      fen = "7k/8/8/PP6/8/8/8/4K3 w - - 0 1",
      playedMoveUci = "a5a6",
      variations = List(
        EngineLine(List("b5b6", "h8g8", "b6b7"), scoreCp = 10, depth = 20),
        EngineLine(List("a5a6", "h8g8", "a6a7"), scoreCp = 0, depth = 20)
      )
    )
    val initial = MoveReviewJudgmentOrchestrator
      .execute(raw)
      .getOrElse(fail("expected an initial bounded passed-pawn-result review"))
    val replyRequests = initial.probeRequests.filter(request =>
      lila.chessjudgment.model.judgment.EvidenceRef.sameMove(request.candidateMove, raw.playedMoveUci)
    )
    assert(replyRequests.nonEmpty, "the passed-pawn result needs its complete legal-reply demand")
    val referenceReplyRequests = initial.probeRequests.filter(request =>
      EvidenceRef.sameMove(request.candidateMove, "b5b6")
    )
    assert(referenceReplyRequests.nonEmpty, "the sibling endpoint must close the same passed-pawn-result family")
    val passedPawnResultReplyRequests = (replyRequests ++ referenceReplyRequests).distinctBy(_.id)
    val probeResults = passedPawnResultReplyRequests.map { request =>
      val reply = request.moves match
        case exact :: Nil => exact
        case other        => fail(s"expected one exact reply move, got $other")
      val realizingMove =
        if EvidenceRef.sameMove(request.candidateMove, raw.playedMoveUci) then "a6a7"
        else "e1e2"
      ProbeResult(
        request.id,
        ProbeResolution.EngineSearch(
          List(
            CandidateLineEvaluation.EngineSearch(
              EngineLine(List(reply, realizingMove), scoreCp = 0, depth = request.depth)
            )
          ),
          request.depth
        )
      )
    }
    val resolved = MoveReviewJudgmentOrchestrator
      .execute(raw.copy(probeResults = probeResults))
      .getOrElse(fail("expected the closed passed-pawn-result review to assemble"))
    val passedPawnResultProofRecords = resolved.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: PassedPawnResultProofEvidence, _) => record
    }
    assert(passedPawnResultProofRecords.nonEmpty, "the closed passed-pawn result must reach the typed L2 producer")
    val passedPawnResultCauseRecords = resolved.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.PassedPawnResult => record
    }
    assert(
      passedPawnResultCauseRecords.nonEmpty,
      clues(
        passedPawnResultProofRecords.map(_.ref.id),
        resolved.evidenceGraph.records.collect {
          case EvidenceRecord(ref, CandidateComparisonEvidence(fact), _) =>
            (ref.id, fact.kind, fact.comparison.verdict, fact.referenceLine.rootMove, fact.candidateLine.rootMove)
        }
      )
    )
    val selectedCauseIds = resolved.playerFacingClaimDecisions.flatMap(_.causeSelections.map(_.causeEvidence.id)).toSet
    assert(
      passedPawnResultCauseRecords.exists(record => selectedCauseIds(record.ref.id)),
      clues(passedPawnResultCauseRecords.map(_.ref.id), resolved.playerFacingClaimDecisions, resolved.causeDispositionLedger)
    )
    val commentary = RuntimeProtocol.moveCommentaryJson(resolved)
    assertEquals(
      commentary.keys.intersect(Set("structural_idea_units", "responsibility_links")),
      Set.empty[String]
    )
    val passedPawnResultFacet = (commentary \ "causal_explanations").as[List[JsObject]]
      .flatMap(idea => (idea \ "facets").as[List[JsObject]])
      .find(facet => (facet \ "kind").as[String] == "passed_pawn_result")
      .getOrElse(fail("expected the selected typed passed-pawn result in public commentary"))
    val channel = (passedPawnResultFacet \ "channels").as[List[JsObject]] match
      case exact :: Nil => exact
      case other        => fail(s"expected one exact passed-pawn-result channel, found ${other.size}")

    assertEquals((passedPawnResultFacet \ "proof_confidence").as[String], "legal_replay_verified")
    assertEquals((passedPawnResultFacet \ "comparison_kind").as[String], "played_vs_best")
    assert(!passedPawnResultFacet.keys.contains("only_move_qualifiers"))
    assert(!passedPawnResultFacet.keys.contains("event_move"))
    assertEquals((channel \ "direct_change").as[String], "occurred")
    assert(!channel.keys.contains("played_change"))
    assertEquals(
      channel.keys.intersect(
        Set("actor", "targets", "mechanisms", "consequences", "witnesses", "proof_line_moves", "horizon", "proof_segment")
      ),
      Set.empty[String]
    )
    val proof = (channel \ "passed_pawn_result_proof").as[JsObject]
    assertEquals((proof \ "contract").as[String], "passed_pawn_result_under_closed_replies")
    assert((proof \ "semantic_id").as[String].matches("[0-9a-f]{64}"))
    assert((proof \ "occurrence_id").as[String].matches("[0-9a-f]{64}"))
    assert((proof \ "dependency_fingerprint").as[String].matches("[0-9a-f]{64}"))
    assertEquals((proof \ "consequence_kind").as[String], "passed_pawn_progress")
    assertEquals((proof \ "root_move").as[String], "a5a6")
    assertEquals((proof \ "realizing_move").as[String], "a6a7")
    assert((proof \ "root_ply").as[Int] <= (proof \ "realizing_ply").as[Int])
    assert((proof \ "result_target_subjects").as[List[String]].nonEmpty)
    assertEquals((proof \ "root_actor" \ "from").as[String], "a5")
    assertEquals((proof \ "realizing_actor" \ "to").as[String], "a7")
    val inventory = (proof \ "closed_legal_reply_inventory").as[JsObject]
    assertEquals(
      (inventory \ "issuer").as[String],
      "structural_delta.canonical_legal_reply_inventory"
    )
    assertEquals(
      (inventory \ "coverage_issuer").as[String],
      "passed_pawn_result_event.branch_complete_reply_coverage"
    )
    assert((inventory \ "coverage_evidence_id").as[String].nonEmpty)
    val legalReplies = (inventory \ "legal_reply_moves").as[List[String]]
    assertEquals(legalReplies, replyRequests.flatMap(_.moves).distinct.sorted)
    val branches = (proof \ "branches").as[List[JsObject]]
    val expectedBranches = branches.filter(branch => (branch \ "role").as[String] == "expected_result_route")
    val replyBranches = branches.filter(branch => (branch \ "role").as[String] == "legal_reply")
    assertEquals(expectedBranches.size, 1)
    assertEquals(replyBranches.map(branch => (branch \ "reply_move").as[String]).sorted, legalReplies)
    assert(replyBranches.forall(branch => (branch \ "source_probe_id").as[String].nonEmpty))
    assert(branches.flatMap(branch => (branch \ "steps").as[List[JsObject]]).forall(step =>
      (step \ "step_key").as[String].nonEmpty &&
        (step \ "line" \ "line_id").as[String].nonEmpty &&
        Set("observed_game_move", "certified_analysis_move")((step \ "provenance").as[String])
    ))
    val proofPaths = (proof \ "proof_paths").as[List[JsObject]]
    assertEquals(proofPaths.map(path => (path \ "path_occurrence_id").as[String]).distinct.size, proofPaths.size)
    assertEquals(proofPaths.map(path => (path \ "reply_branch_id").as[String]).toSet, replyBranches.map(branch =>
      (branch \ "branch_id").as[String]
    ).toSet)
    assert(proofPaths.forall(path => (path \ "premises").as[List[JsObject]].nonEmpty))
    assert(proofPaths.forall(path => (path \ "closure_use_ids").as[List[String]].nonEmpty))
    assert(proofPaths.forall(path =>
      Set("exact_move", "equivalent_function")((path \ "realization_match_kind").as[String]) &&
        (path \ "realization_actor" \ "legal_move_relation").as[String].matches("[0-9a-f]{64}") &&
        (path \ "realization_move").as[String].matches("[a-h][1-8][a-h][1-8][qrbn]?") &&
        (path \ "realization_ply").as[Int] >= 0
    ))
    assert(proofPaths.forall { path =>
      val branchId = (path \ "reply_branch_id").as[String]
      val move = (path \ "realization_move").as[String]
      val ply = (path \ "realization_ply").as[Int]
      replyBranches
        .find(branch => (branch \ "branch_id").as[String] == branchId)
        .exists(branch => (branch \ "steps").as[List[JsObject]].exists(step =>
          (step \ "move_uci").as[String] == move && (step \ "ply").as[Int] == ply
        ))
    })
    assert((proof \ "lower_premise_ids").as[List[String]].nonEmpty)

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
