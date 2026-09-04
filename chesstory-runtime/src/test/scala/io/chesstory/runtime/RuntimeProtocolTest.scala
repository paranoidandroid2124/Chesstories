package io.chesstory.runtime

import java.net.{ InetSocketAddress, URI }
import java.net.http.{ HttpClient, HttpRequest, HttpResponse }
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths }
import java.util.concurrent.Executors

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration.*

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CandidateLineEvaluation,
  CanonicalPositionHistory
}
import lila.chessjudgment.analysis.assembly.{
  ExplanationRequest,
  MoveReviewInputAdmission,
  MoveReviewJudgmentOrchestrator,
  RawMoveReviewInput
}
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

  private def assertObservedBranchOwners(
      branch: JsObject,
      subject: JsObject
  ): Unit =
    assertEquals(branch.keys.intersect(Set("line_role", "line_rank")), Set.empty[String])
    assertEquals(
      (branch \ "line_owner_evidence_id").as[String],
      (subject \ "line_owner_evidence_id").as[String]
    )
    assertEquals(
      (branch \ "root_transition_evidence_id").as[String],
      (subject \ "transition_evidence_id").as[String]
    )
    assertEquals((branch \ "line_id").as[String], (subject \ "line_id").as[String])

  private def assertCounterfactualBranchOwners(
      branch: JsObject,
      subject: JsObject
  ): Unit =
    assertEquals(branch.keys.intersect(Set("line_role", "line_rank")), Set.empty[String])
    assertNotEquals(
      (branch \ "line_owner_evidence_id").as[String],
      (subject \ "line_owner_evidence_id").as[String]
    )
    assertNotEquals(
      (branch \ "root_transition_evidence_id").as[String],
      (subject \ "transition_evidence_id").as[String]
    )

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
    val completed = afterRoot match
      case value: CompletedCommentaryJob => value
      case other => fail(s"covered focus must complete from admitted root evidence: $other")
    assertEquals(completed.progress.selectedCommentariesCompleted, 1)
    assertEquals(completed.progress.physicalWorksIssued, 1)
    assertEquals(afterRoot.progress.physicalReportsAccepted, 1)

  test("v6 produces BestChoice from one LegalLine owner when the played move is best"):
    val played = "e2e4"
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
      .getOrElse(fail("expected the root search"))
    val afterRoot = CommentaryJobReducer
      .submitIssuedWorkReport(root, completedReport(root.issuedWork, rootLines), nowEpochMs = 1L)
      .toOption
      .getOrElse(fail("the root report must be admitted"))
    val completed = afterRoot match
      case value: CompletedCommentaryJob => value
      case other => fail(s"the best played move must produce a completed review, found $other")
    val review = completed.result match
      case CompletedPositionCommentary.FocusedMoveReview(_, value) => value
      case other => fail(s"expected a focused move review, found $other")
    val packet = review.judgmentPacket
    val rootMoves = packet.candidateLines.map(line => EvidenceRef.normalizeMove(line.ref.rootMove))

    assertEquals(rootMoves.distinct.size, rootMoves.size)
    assertEquals(
      packet.candidateLines.filter(line => EvidenceRef.sameMove(line.ref.rootMove, played)).map(_.role),
      List(LineNodeRole.BestReference)
    )
    packet.primary match
      case PlayerFacingPrimary.BestChoice(comparisonEvidence) =>
        val comparison = packet.evidenceGraph
          .candidateComparisonRecord(comparisonEvidence)
          .getOrElse(fail("expected the producer-owned BestVsSecond comparison"))
        comparison.payload match
          case CandidateComparisonEvidence(fact) =>
            assertEquals(fact.kind, CandidateComparisonKind.BestVsSecond)
            assertEquals(fact.referenceLine.rootMove, played)
            assertEquals(fact.candidateLine.rootMove, "d2d4")
          case other => fail(s"expected candidate comparison evidence, found $other")
      case other => fail(s"expected BestChoice, found $other")

    val response = RuntimeProtocol.encodePositionCommentaryResponse(
      CommentaryJobSnapshot("b" * 32, "request-best-choice", Long.MaxValue, completed),
      completed
    )
    val fixture = Json.parse(
      Files.readString(
        Paths.get("..", "judgment-evaluation", "fixtures", "public-commentary-v6", "best-choice-produced.json"),
        StandardCharsets.UTF_8
      )
    )
    assertEquals(Json.parse(Json.stringify(response)), fixture)
    assertEquals((response \ "schema_version").as[String], "chesstory.position-commentary.response.v6")
    val selectedReviews = (response \ "result" \ "selected_move_reviews").as[List[JsObject]]
    assertEquals(selectedReviews.size, 1)
    assertEquals((selectedReviews.head \ "selection" \ "roles").as[List[String]], List("best", "played"))
    val primary = (selectedReviews.head \ "commentary" \ "primary").as[JsObject]
    assertEquals((primary \ "kind").as[String], "best_choice")
    assertEquals((primary \ "best_endpoint" \ "moves").as[List[String]].headOption, Some(played))
    assertEquals((primary \ "runner_up_endpoint" \ "moves").as[List[String]].headOption, Some("d2d4"))

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
      case _: CompletedCommentaryJob => ()
      case other => fail(s"focused review did not complete from admitted root/focus evidence: $other")

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

  test("Scala producer matches the immutable occurrence-relation public-v6 fixture"):
    val rootFen =
      "r2qrbk1/1bpn1p1p/p2p1np1/Pp2p3/3PP3/2P2NNP/1PB2PP1/R1BQR1K1 w - - 1 17"
    val vacatingMoves = List("d4d5", "c7c6", "d5c6", "b7c6", "b2b4")
    val immediateMoves = List("b2b4", "e5d4")
    val rootHistory = CanonicalPositionHistory
      .from(rootFen, Nil, rootFen)
      .toOption
      .getOrElse(fail("expected the fixture root history"))
    val resulting = rootHistory
      .extend(List(immediateMoves.head))
      .toOption
      .map(_.currentFen)
      .getOrElse(fail("expected the fixture played move"))
    val started = CommentaryJobReducer
      .startJob(
        rootFen,
        Nil,
        rootFen,
        immediateMoves.head,
        resulting,
        policy(),
        nowEpochMs = 0L
      )
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .getOrElse(fail("expected the fixture root search"))
    val finished = CommentaryJobReducer
      .submitIssuedWorkReport(
        started,
        completedReport(
          started.issuedWork,
          List(
            vacatingMoves -> 600,
            immediateMoves -> 0,
            List("c1g5") -> -200
          )
        ),
        nowEpochMs = 1L
      )
      .toOption
      .collect { case value: CompletedCommentaryJob => value }
      .getOrElse(fail("expected the fixture review to complete from the root report"))
    val response = RuntimeProtocol.encodePositionCommentaryResponse(
      CommentaryJobSnapshot("c" * 32, "request-occurrence-explanation", Long.MaxValue, finished),
      finished
    )
    val playedReview = (response \ "result" \ "selected_move_reviews")
      .as[List[JsObject]]
      .find(review => (review \ "move_uci").as[String] == immediateMoves.head)
      .getOrElse(fail("expected the played review in the produced fixture"))
    val occurrenceResults = (playedReview \ "commentary" \ "occurrence_relations").as[List[JsObject]]
    assertEquals(occurrenceResults.size, 1)
    assertEquals((occurrenceResults.head \ "relation_kind").as[String], "capture_exclusion_move_order")

    val fixturePath = Paths.get(
      "..",
      "judgment-evaluation",
      "fixtures",
      "public-commentary-v6",
      "occurrence-relation-produced.json"
    )
    val fixture = Json.parse(Files.readString(fixturePath, StandardCharsets.UTF_8))
    assertEquals(Json.parse(Json.stringify(response)), fixture)

  test("v6 commentary serializes the capture-exclusion occurrence relation"):
    val rootFen =
      "r2qrbk1/1bpn1p1p/p2p1np1/Pp2p3/3PP3/2P2NNP/1PB2PP1/R1BQR1K1 w - - 1 17"
    val referenceMoves = List("d4d5", "c7c6", "d5c6", "b7c6", "b2b4")
    val playedMoves = List("b2b4", "e5d4")
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
      .getOrElse(fail("expected an evidence-backed capture-exclusion packet"))

    val commentary = RuntimeProtocol.moveCommentaryJson(packet)
    assertEquals((commentary \ "causal_explanations").asOpt[List[JsObject]].getOrElse(Nil), Nil)
    val occurrenceResult = (commentary \ "occurrence_relations").as[List[JsObject]] match
      case exact :: Nil => exact
      case other        => fail(s"expected one capture-exclusion occurrence result, found ${other.size}")
    assertEquals(
      occurrenceResult.keys,
      Set(
        "subject_occurrence",
        "relation_kind",
        "proof"
      )
    )
    assertEquals((occurrenceResult \ "relation_kind").as[String], "capture_exclusion_move_order")
    val subject = (occurrenceResult \ "subject_occurrence").as[JsObject]
    assertEquals((subject \ "move_uci").as[String], "b2b4")
    assertEquals((subject \ "root_provenance").as[String], "observed_game_root")

    val proof = (occurrenceResult \ "proof").as[JsObject]
    assertEquals(
      proof.keys,
      Set(
        "source_evidence_id",
        "semantic_id",
        "occurrence_id",
        "dependency_fingerprint",
        "vacating_branch",
        "immediate_capture_branch",
        "proof_paths",
        "participants",
        "later_deferred_step_index"
      )
    )
    assertEquals((proof \ "subject_occurrence").toOption, None)
    val vacating = (proof \ "vacating_branch").as[JsObject]
    val immediate = (proof \ "immediate_capture_branch").as[JsObject]
    assertCounterfactualBranchOwners(vacating, subject)
    assertObservedBranchOwners(immediate, subject)
    assertEquals((vacating \ "branch_role").as[String], "vacating_then_deferred")
    assertEquals((vacating \ "root_provenance").as[String], "counterfactual_analyzed_root")
    assertEquals((immediate \ "branch_role").as[String], "immediate_deferred_capture")
    assertEquals((immediate \ "root_provenance").as[String], "observed_game_root")
    assertEquals(
      (immediate \ "steps").as[List[JsObject]].map(step => (step \ "provenance").as[String]),
      List("observed_game_move", "certified_analysis_move")
    )
    val path = (proof \ "proof_paths").as[List[JsObject]] match
      case exact :: Nil => exact
      case other =>
        fail(s"expected one independent capture-exclusion path, found ${other.size}")
    val premises = (path \ "premises").as[List[JsObject]]
    val deferredMoves = premises.filter(premise =>
      Set("immediate_deferred_move", "later_deferred_move")((premise \ "role").as[String])
    )
    assertEquals(deferredMoves.map(move => (move \ "move_uci").as[String]), List.fill(2)("b2b4"))
    assertEquals(
      deferredMoves.map(move => (move \ "legal_move_semantic_id").as[String]).distinct.size,
      1
    )
    assertEquals(
      deferredMoves.map(move => (move \ "issuer_occurrence_id").as[String]).distinct.size,
      2
    )

    val absences = (path \ "closed_absence_uses").as[List[JsObject]]
    assertEquals(absences.map(use => (use \ "after_step_index").as[Int]), List(0, 4))
    assertEquals(
      absences.map(use => (use \ "query").as[String]).distinct,
      List("legal-move-from-to:black:e5:d4")
    )
    val states = (path \ "closed_state_uses").as[List[JsObject]]
    assertEquals(states.size, 14)
    assertEquals(states.count(use => (use \ "role").as[String] == "vacated_target"), 5)
    assertEquals(states.count(use => (use \ "role").as[String] == "reply_actor"), 5)
    assertEquals(states.count(use => (use \ "role").as[String] == "deferred_actor"), 4)
    assertEquals((proof \ "participants" \ "captured_target" \ "square").as[String], "d4")
    assertEquals((proof \ "later_deferred_step_index").as[Int], 4)

  test("v6 commentary serializes the relocation-enabled recapture occurrence relation"):
    val rootFen = "r3k3/3pq3/8/4P3/8/8/P7/K7 b q - 0 1"
    val relocatedMoves = List("e8c8", "a2a3", "d7d5", "e5d6", "d8d6")
    val retainedMoves = List("d7d5", "e5d6", "e7d6")
    val rootHistory = CanonicalPositionHistory
      .from(rootFen, Nil, rootFen)
      .toOption
      .getOrElse(fail("expected the relocation fixture root history"))
    val resulting = rootHistory
      .extend(List(retainedMoves.head))
      .toOption
      .map(_.currentFen)
      .getOrElse(fail("expected the relocation fixture played move"))
    val started = CommentaryJobReducer
      .startJob(
        rootFen,
        Nil,
        rootFen,
        retainedMoves.head,
        resulting,
        policy(),
        nowEpochMs = 0L
      )
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .getOrElse(fail("expected the relocation fixture root search"))
    val finished = CommentaryJobReducer
      .submitIssuedWorkReport(
        started,
        completedReport(
          started.issuedWork,
          List(
            relocatedMoves -> 600,
            retainedMoves -> 0,
            List("a8b8") -> -200
          )
        ),
        nowEpochMs = 1L
      )
      .toOption
      .collect { case value: CompletedCommentaryJob => value }
      .getOrElse(fail("expected the relocation fixture review to complete"))
    val response = RuntimeProtocol.encodePositionCommentaryResponse(
      CommentaryJobSnapshot("d" * 32, "request-relocation-enables-recapture", Long.MaxValue, finished),
      finished
    )
    val playedReview = (response \ "result" \ "selected_move_reviews")
      .as[List[JsObject]]
      .find(review => (review \ "move_uci").as[String] == retainedMoves.head)
      .getOrElse(fail("expected the played review in the relocation fixture"))
    val occurrenceResult = (playedReview \ "commentary" \ "occurrence_relations")
      .as[List[JsObject]] match
        case exact :: Nil => exact
        case other =>
          fail(s"expected one relocation-enabled-recapture occurrence result, found ${other.size}")
    assertEquals((occurrenceResult \ "relation_kind").as[String], "relocation_enables_recapture")
    val subject = (occurrenceResult \ "subject_occurrence").as[JsObject]
    assertEquals((subject \ "move_uci").as[String], retainedMoves.head)
    assertEquals((subject \ "root_provenance").as[String], "observed_game_root")

    val proof = (occurrenceResult \ "proof").as[JsObject]
    assertEquals(
      proof.keys,
      Set(
        "source_evidence_id",
        "semantic_id",
        "occurrence_id",
        "dependency_fingerprint",
        "relocated_responder_branch",
        "retained_responder_branch",
        "proof_paths",
        "participants",
        "relocation",
        "target_capture",
        "relocated_responder_recapture",
        "retained_other_recapture"
      )
    )
    val relocated = (proof \ "relocated_responder_branch").as[JsObject]
    val retained = (proof \ "retained_responder_branch").as[JsObject]
    assertCounterfactualBranchOwners(relocated, subject)
    assertObservedBranchOwners(retained, subject)
    assertEquals((relocated \ "branch_role").as[String], "relocated_responder")
    assertEquals((retained \ "branch_role").as[String], "retained_responder")
    assertEquals(
      (relocated \ "steps").as[List[JsObject]].map(step => (step \ "move_uci").as[String]),
      relocatedMoves
    )
    assertEquals(
      (retained \ "steps").as[List[JsObject]].map(step => (step \ "move_uci").as[String]),
      retainedMoves
    )

    val participants = (proof \ "participants").as[JsObject]
    assertEquals((participants \ "attacker_at_common_root" \ "side").as[String], "white")
    assertEquals((participants \ "attacker_at_common_root" \ "piece").as[String], "pawn")
    assertEquals((participants \ "attacker_at_common_root" \ "square").as[String], "e5")
    assertEquals((participants \ "attacker_at_capture" \ "square").as[String], "e5")
    assertEquals((participants \ "target_at_common_root" \ "piece").as[String], "pawn")
    assertEquals((participants \ "target_at_common_root" \ "square").as[String], "d7")
    assertEquals((participants \ "captured_target" \ "piece").as[String], "pawn")
    assertEquals((participants \ "captured_target" \ "square").as[String], "d5")
    assertEquals((participants \ "recapture_square").as[String], "d6")
    assertNotEquals(
      (participants \ "captured_target" \ "square").as[String],
      (participants \ "recapture_square").as[String]
    )
    assertEquals((participants \ "tracked_responder_at_seed" \ "square").as[String], "a8")
    assertEquals((participants \ "tracked_responder_at_staging" \ "square").as[String], "d8")
    assertEquals((participants \ "other_recapturer" \ "piece").as[String], "queen")
    assertEquals((participants \ "other_recapturer" \ "square").as[String], "e7")

    assertEquals((proof \ "relocation" \ "move_uci").as[String], "e8c8")
    assertEquals((proof \ "relocation" \ "movement" \ "from").as[String], "a8")
    assertEquals((proof \ "relocation" \ "movement" \ "to").as[String], "d8")
    assertEquals((proof \ "relocation" \ "step_index").as[Int], 0)
    assertEquals((proof \ "target_capture" \ "move_uci").as[String], "e5d6")
    assertEquals((proof \ "target_capture" \ "relocated_step_index").as[Int], 3)
    assertEquals((proof \ "target_capture" \ "retained_step_index").as[Int], 1)
    assertEquals((proof \ "relocated_responder_recapture" \ "move_uci").as[String], "d8d6")
    assertEquals((proof \ "relocated_responder_recapture" \ "step_index").as[Int], 4)
    assertEquals((proof \ "retained_other_recapture" \ "move_uci").as[String], "e7d6")
    assertEquals((proof \ "retained_other_recapture" \ "step_index").as[Int], 2)

    val path = (proof \ "proof_paths").as[List[JsObject]] match
      case exact :: Nil => exact
      case other => fail(s"expected one exact lower proof path, found ${other.size}")
    val premises = (path \ "premises").as[List[JsObject]]
    val inventories = premises.filter(premise => (premise \ "contract").as[String] == "capture_recapture_inventory")
    assertEquals(
      inventories.map(premise => (premise \ "role").as[String]),
      List("relocated_recapture_inventory", "retained_recapture_inventory")
    )
    val endpointMoves = premises.filter(premise => (premise \ "contract").as[String] == "legal_move")
    assertEquals(
      endpointMoves.map(premise => (premise \ "role").as[String]),
      List(
        "relocated_target_capture",
        "relocated_responder_recapture",
        "retained_target_capture",
        "retained_other_recapture"
      )
    )
    val targetCapture = (proof \ "target_capture").as[JsObject]
    val capturedTarget = (participants \ "captured_target").as[JsObject]
    val capturedAttacker = Json.obj(
      "side" -> (targetCapture \ "movement" \ "side").as[String],
      "piece" -> (targetCapture \ "movement" \ "piece_after").as[String],
      "square" -> (targetCapture \ "movement" \ "to").as[String]
    )
    def assertEndpoint(
        role: String,
        branch: JsObject,
        stepIndex: Int,
        transition: JsObject,
        captured: JsObject
    ): Unit =
      val premise = endpointMoves.find(value => (value \ "role").as[String] == role)
        .getOrElse(fail(s"expected endpoint premise $role"))
      assertEquals((premise \ "branch_id").as[String], (branch \ "branch_id").as[String])
      assertEquals((premise \ "branch_role").as[String], (branch \ "branch_role").as[String])
      assertEquals((premise \ "step_index").as[Int], stepIndex)
      assertEquals((premise \ "move_uci").as[String], (transition \ "move_uci").as[String])
      assertEquals((premise \ "movement").as[JsObject], (transition \ "movement").as[JsObject])
      assertEquals((premise \ "capture").as[JsObject], captured)
      assertEquals(
        (premise \ "source_premise_ids").as[List[String]],
        List(
          (premise \ "issuer_evidence_id").as[String],
          (premise \ "issuer_occurrence_id").as[String],
          s"legal-move:${(premise \ "legal_move_semantic_id").as[String]}"
        ).sorted
      )
    assertEndpoint(
      "relocated_target_capture",
      relocated,
      (targetCapture \ "relocated_step_index").as[Int],
      targetCapture,
      capturedTarget
    )
    assertEndpoint(
      "retained_target_capture",
      retained,
      (targetCapture \ "retained_step_index").as[Int],
      targetCapture,
      capturedTarget
    )
    assertEndpoint(
      "relocated_responder_recapture",
      relocated,
      (proof \ "relocated_responder_recapture" \ "step_index").as[Int],
      (proof \ "relocated_responder_recapture").as[JsObject],
      capturedAttacker
    )
    assertEndpoint(
      "retained_other_recapture",
      retained,
      (proof \ "retained_other_recapture" \ "step_index").as[Int],
      (proof \ "retained_other_recapture").as[JsObject],
      capturedAttacker
    )
    val continuity = premises.filter(premise => (premise \ "contract").as[String] == "object_continuity_step")
    val continuityRoles = Set(
      "relocated_branch_target_continuity",
      "retained_branch_target_continuity",
      "relocated_responder_continuity",
      "retained_responder_continuity",
      "relocated_branch_attacker_continuity",
      "retained_branch_attacker_continuity"
    )
    assertEquals(continuity.map(premise => (premise \ "role").as[String]).toSet, continuityRoles)
    assert(continuity.size > 9)
    assert(continuity.groupBy(premise => (premise \ "role").as[String]).values.exists(_.size > 1))
    val occurrenceKeys = continuity.map(premise => (
      (premise \ "role").as[String],
      (premise \ "branch_id").as[String],
      (premise \ "step_index").as[Int],
      (premise \ "issuer_occurrence_id").as[String]
    ))
    assertEquals(occurrenceKeys.distinct.size, occurrenceKeys.size)
    val branchesById = List(relocated, retained).map(branch => (branch \ "branch_id").as[String] -> branch).toMap
    continuity.foreach { premise =>
      val sources = (premise \ "source_premise_ids").as[List[String]]
      assertEquals(
        sources,
        List(
          (premise \ "issuer_evidence_id").as[String],
          (premise \ "issuer_occurrence_id").as[String],
          s"legal-move:${(premise \ "legal_move_semantic_id").as[String]}",
          s"transition-footprint:${(premise \ "transition_footprint_id").as[String]}"
        ).sorted
      )
      val branch = branchesById((premise \ "branch_id").as[String])
      val steps = (branch \ "steps").as[List[JsObject]]
      val step = steps((premise \ "step_index").as[Int])
      assertEquals((premise \ "overall_move_uci").as[String], (step \ "move_uci").as[String])
      val before = (premise \ "before").as[JsObject]
      val after = (premise \ "after").as[JsObject]
      (premise \ "transition_kind").as[String] match
        case "retained" =>
          assertEquals(before, after)
          assert(!premise.keys.contains("selected_transition"))
        case "primary" | "secondary" =>
          val selected = (premise \ "selected_transition").as[JsObject]
          assertEquals((selected \ "side").as[String], (before \ "side").as[String])
          assertEquals((selected \ "from").as[String], (before \ "square").as[String])
          assertEquals((selected \ "piece_before").as[String], (before \ "piece").as[String])
          assertEquals((selected \ "to").as[String], (after \ "square").as[String])
          assertEquals((selected \ "piece_after").as[String], (after \ "piece").as[String])
        case other => fail(s"unexpected object-continuity transition kind $other")
    }
    assert(continuity.exists(premise => (premise \ "transition_kind").as[String] == "secondary"))
    premises.foreach { premise =>
      val sources = (premise \ "source_premise_ids").as[List[String]]
      assert(sources.contains((premise \ "issuer_evidence_id").as[String]))
      assert(sources.contains((premise \ "issuer_occurrence_id").as[String]))
    }
    val absence = (path \ "closed_absence_uses").as[List[JsObject]] match
      case exact :: Nil => exact
      case other => fail(s"expected one exact seed-recapture absence, found ${other.size}")
    assertEquals((absence \ "role").as[String], "retained_seed_recapture_absent")
    assertEquals((absence \ "query").as[String], "legal-move-from-to:black:a8:d6")
    assertEquals((absence \ "after_step_index").as[Int], 1)
    assertEquals((absence \ "branch_id").as[String], (retained \ "branch_id").as[String])
    val states = (path \ "closed_state_uses").as[List[JsObject]]
    val retainedContinuity = continuity.filter(premise => (premise \ "transition_kind").as[String] == "retained")
    assertEquals(states.size, retainedContinuity.size)
    retainedContinuity.foreach { premise =>
      val after = (premise \ "after").as[JsObject]
      val matching = states.filter(state =>
        (state \ "role").as[String] == (premise \ "role").as[String] &&
          (state \ "branch_id").as[String] == (premise \ "branch_id").as[String] &&
          (state \ "branch_role").as[String] == (premise \ "branch_role").as[String] &&
          (state \ "after_step_index").as[Int] == (premise \ "step_index").as[Int] &&
          (state \ "query").as[String] ==
            s"occupied-by:${(after \ "side").as[String]}:${(after \ "piece").as[String]}@${(after \ "square").as[String]}"
      )
      assertEquals(matching.size, 1)
    }

    val fixture = Json.parse(
      Files.readString(
        Paths.get(
          "..",
          "judgment-evaluation",
          "fixtures",
          "public-commentary-v6",
          "relocation-enables-recapture-produced.json"
        ),
        StandardCharsets.UTF_8
      )
    )
    assertEquals(Json.parse(Json.stringify(response)), fixture)

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
