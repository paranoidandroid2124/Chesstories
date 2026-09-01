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
    assertEquals(response, fixture)
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

  test("v6 commentary serializes unique-check-reply defender displacement before capture"):
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
      .find(facet => (facet \ "kind").as[String] == "wrong_move_order")
      .getOrElse(fail("expected the selected L2 cause in public commentary"))
    val channel = (moveOrderFacet \ "channels").as[List[JsObject]] match
      case exact :: Nil => exact
      case other        => fail(s"expected one exact L2 channel, found ${other.size}")

    assertEquals(moveOrderFacet.keys, Set("cause_evidence_id", "kind", "exposure", "channels"))
    assertEquals((moveOrderFacet \ "exposure").as[String], "primary")
    assertEquals(channel.keys, Set("channel_id", "unique_check_reply_defender_displacement_before_capture_proof"))
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
    val proof = (channel \ "unique_check_reply_defender_displacement_before_capture_proof").as[JsObject]
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
    assertEquals((playedBranch \ "branch_role").as[String], "played_root_analysis_continuation")
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
    premises.foreach { premise =>
      val sources = (premise \ "source_premise_ids").as[List[String]]
      assert(sources.contains((premise \ "issuer_evidence_id").as[String]))
      assert(sources.contains((premise \ "issuer_occurrence_id").as[String]))
    }
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
      .getOrElse(fail("expected an evidence-backed sole-recapturer-removal packet"))

    val moveOrderProofRecords = packet.evidenceGraph.records.filter(_.ref.layer == EvidenceLayer.CausalProof)
    assertEquals(moveOrderProofRecords.size, 1)
    val defenseCauseRecords = packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder &&
            cause.proofSources.exists(source => moveOrderProofRecords.exists(_.ref == source)) => record
    }
    assertEquals(
      defenseCauseRecords.size,
      1,
      clues(
        moveOrderProofRecords.map(record => record.ref -> packet.evidenceGraph.proofEligible(record)),
        packet.evidenceGraph.records.collect {
          case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) =>
            (ref, cause.kind, cause.proofSources)
        }
      )
    )
    val selectedCauseIds = packet.playerFacingClaimDecisions.flatMap(_.causeSelections.map(_.causeEvidence.id)).toSet
    assert(
      defenseCauseRecords.exists(record => selectedCauseIds(record.ref.id)),
      clues(defenseCauseRecords.map(_.ref.id), packet.playerFacingClaimDecisions, packet.causeDispositionLedger)
    )

    val channel = RuntimeProtocol.moveCommentaryJson(packet)
      .value("causal_explanations")
      .as[List[JsObject]]
      .find(facet => (facet \ "kind").as[String] == "wrong_move_order")
      .toList
      .flatMap(facet => (facet \ "channels").as[List[JsObject]])
      .filter(channel => (channel \ "sole_recapturer_removal_before_target_capture_proof").asOpt[JsObject].nonEmpty) match
        case exact :: Nil => exact
        case other        => fail(s"expected one public removal channel, found ${other.size}")
    assertEquals(channel.keys, Set("channel_id", "sole_recapturer_removal_before_target_capture_proof"))
    val proof = (channel \ "sole_recapturer_removal_before_target_capture_proof").as[JsObject]

    assertEquals((proof \ "later_exploit_move").as[String], "d1d5")
    assertEquals((proof \ "played_sole_recapture_move").as[String], "f6d5")
    val playedBranch = (proof \ "played_root_branch").as[JsObject]
    assertEquals((playedBranch \ "branch_role").as[String], "played_root_analysis_continuation")
    assertEquals((playedBranch \ "root_provenance").as[String], "observed_game_root")
    assertEquals(
      (playedBranch \ "steps").as[List[JsObject]].map(step => (step \ "provenance").as[String]),
      List("observed_game_move", "certified_analysis_move")
    )
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
    premises.foreach { premise =>
      val sources = (premise \ "source_premise_ids").as[List[String]]
      assert(sources.contains((premise \ "issuer_evidence_id").as[String]))
      assert(sources.contains((premise \ "issuer_occurrence_id").as[String]))
    }
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

  test("v6 commentary serializes only the common capture-exclusion move-order proof"):
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

    val channel = (RuntimeProtocol.moveCommentaryJson(packet) \ "causal_explanations")
      .as[List[JsObject]]
      .filter(facet => (facet \ "kind").as[String] == "wrong_move_order")
      .flatMap(facet => (facet \ "channels").as[List[JsObject]])
      .filter(channel =>
        (channel \ "capture_exclusion_move_order_proof").asOpt[JsObject].nonEmpty
      ) match
        case exact :: Nil => exact
        case other        => fail(s"expected one public capture-exclusion channel, found ${other.size}")
    assertEquals(channel.keys, Set("channel_id", "capture_exclusion_move_order_proof"))

    val proof = (channel \ "capture_exclusion_move_order_proof").as[JsObject]
    assertEquals(
      proof.keys,
      Set(
        "source_evidence_id",
        "semantic_id",
        "occurrence_id",
        "dependency_fingerprint",
        "counterfactual_reference_branch",
        "played_root_branch",
        "proof_paths"
      )
    )
    val path = (proof \ "proof_paths").as[List[JsObject]] match
      case exact :: Nil => exact
      case other =>
        fail(s"expected one independent capture-exclusion path, found ${other.size}")
    val premises = (path \ "premises").as[List[JsObject]]
    val deferredMoves = premises.filter(premise =>
      Set("played_deferred_move", "reference_deferred_move")((premise \ "role").as[String])
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
    assertEquals(states.count(use => (use \ "role").as[String] == "reference_vacated_target"), 5)
    assertEquals(states.count(use => (use \ "role").as[String] == "reference_reply_actor"), 5)
    assertEquals(states.count(use => (use \ "role").as[String] == "reference_deferred_actor"), 4)

  test("v6 commentary keeps direct line-access proof in its exact typed channel"):
    val rootFen = "7k/q7/8/8/8/8/N7/R6K w - - 0 1"
    val referenceSourceMoves = List("a2b4", "h8g8", "a1a7")
    val playedMoves = List("h1h2", "h8g8")
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = playedMoves.head,
          variations = List(
            EngineLine(referenceSourceMoves, scoreCp = 600, depth = 24),
            EngineLine(playedMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected an exact direct line-access packet"))

    val facet = (RuntimeProtocol.moveCommentaryJson(packet) \ "causal_explanations")
      .as[List[JsObject]]
      .find(facet => (facet \ "kind").as[String] == "missed_tactical_resource")
      .getOrElse(fail("expected the selected direct line-access cause"))
    val channel = (facet \ "channels").as[List[JsObject]] match
      case exact :: Nil => exact
      case other        => fail(s"expected one direct line-access channel, found ${other.size}")
    assertEquals(facet.keys, Set("cause_evidence_id", "kind", "exposure", "channels"))
    assertEquals((facet \ "exposure").as[String], "primary")
    assertEquals(channel.keys, Set("channel_id", "vacated_gate_enables_unrecapturable_slider_capture_proof"))
    assertEquals(
      channel.keys.intersect(
        Set("actor", "targets", "mechanisms", "consequences", "witnesses", "proof_line_moves", "horizon", "proof_segment")
      ),
      Set.empty[String],
      "the exact L2 proof must never fall through the generic channel"
    )

    val proof = (channel \ "vacated_gate_enables_unrecapturable_slider_capture_proof").as[JsObject]
    assertEquals((proof \ "exploit_move").as[String], "a1a7")
    val reference = (proof \ "counterfactual_reference_branch").as[JsObject]
    val played = (proof \ "played_root_branch").as[JsObject]
    assertEquals(
      (reference \ "steps").as[List[JsObject]].map(step => (step \ "move_uci").as[String]),
      referenceSourceMoves
    )
    assertEquals(
      (played \ "steps").as[List[JsObject]].map(step => (step \ "move_uci").as[String]),
      playedMoves
    )
    assertEquals((played \ "branch_role").as[String], "played_root_analysis_continuation")
    assertEquals((played \ "root_provenance").as[String], "observed_game_root")
    assertEquals(
      (played \ "steps").as[List[JsObject]].map(step => (step \ "provenance").as[String]),
      List("observed_game_move", "certified_analysis_move")
    )

    val path = (proof \ "proof_paths").as[List[JsObject]] match
      case exact :: Nil => exact
      case other        => fail(s"expected one retained proof path, found ${other.size}")
    val premises = (path \ "premises").as[List[JsObject]]
    assertEquals(
      premises.map(premise => (premise \ "role").as[String]),
      List("reference_root_slider_reach", "reference_exploit_capture")
    )
    assertEquals(premises.map(premise => (premise \ "step_index").as[Int]), List(0, 2))
    premises.foreach { premise =>
      val sources = (premise \ "source_premise_ids").as[List[String]]
      assert(sources.contains((premise \ "issuer_evidence_id").as[String]))
      assert(sources.contains((premise \ "issuer_occurrence_id").as[String]))
    }
    val absences = (path \ "closed_absence_uses").as[List[JsObject]]
    assertEquals(
      absences.map(use => (use \ "role").as[String]),
      List(
        "reference_immediate_recapture_absent",
        "played_exploit_move_absent",
        "played_replacement_capture_absent"
      )
    )
    assertEquals(
      absences.map(use => (use \ "branch_id").as[String]),
      List(
        (reference \ "branch_id").as[String],
        (played \ "branch_id").as[String],
        (played \ "branch_id").as[String]
      )
    )
    assert(absences.forall(use => (use \ "issuer_occurrence_id").as[String].matches("[0-9a-f]{64}")))

    val states = (path \ "closed_state_uses").as[List[JsObject]]
    assertEquals(
      states.map(use => (use \ "role").as[String]),
      List(
        "reference_intervening_slider_reach",
        "reference_target_persistence",
        "reference_target_persistence",
        "played_slider_persistence",
        "played_target_persistence",
        "played_gate_blocker_persistence",
        "played_slider_persistence",
        "played_target_persistence",
        "played_gate_blocker_persistence",
        "played_blocked_slider_reach"
      )
    )
    assert(states.take(3).forall(use => (use \ "branch_id").as[String] == (reference \ "branch_id").as[String]))
    assert(states.drop(3).forall(use => (use \ "branch_id").as[String] == (played \ "branch_id").as[String]))
    assert(states.forall(use => (use \ "issuer_occurrence_id").as[String].matches("[0-9a-f]{64}")))
    assertEquals((proof \ "participants" \ "enabler" \ "from").as[String], "a2")
    assertEquals((proof \ "participants" \ "slider" \ "square").as[String], "a1")
    assertEquals((proof \ "participants" \ "gate_blocker" \ "square").as[String], "a2")
    assertEquals((proof \ "participants" \ "exploit" \ "to").as[String], "a7")
    assertEquals((proof \ "participants" \ "captured_target" \ "square").as[String], "a7")

  test("v6 commentary serializes the exact one-leg square-release route proof"):
    val rootFen = "1r4k1/p1q2p1p/2BRbp1B/4p3/P1p4P/6P1/1P2PP1K/3R4 w - - 0 30"
    val referenceMoves = List("c6g2", "e6f5", "d6c6")
    val playedMoves = List("d1d2", "e6f5")
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
      .getOrElse(fail("expected an exact square-release route packet"))

    val facet = (RuntimeProtocol.moveCommentaryJson(packet) \ "causal_explanations")
      .as[List[JsObject]]
      .find(facet => (facet \ "kind").as[String] == "missed_square_release")
      .getOrElse(fail("expected the selected square-release cause"))
    val channel = (facet \ "channels").as[List[JsObject]] match
      case exact :: Nil => exact
      case other        => fail(s"expected one square-release route channel, found ${other.size}")
    assertEquals(facet.keys, Set("cause_evidence_id", "kind", "exposure", "channels"))
    assertEquals((facet \ "exposure").as[String], "primary")
    assertEquals(channel.keys, Set("channel_id", "square_release_route_proof"))
    assertEquals(
      channel.keys.intersect(
        Set("actor", "targets", "mechanisms", "consequences", "witnesses", "proof_line_moves", "horizon", "proof_segment")
      ),
      Set.empty[String]
    )

    val proof = (channel \ "square_release_route_proof").as[JsObject]
    assert((proof \ "semantic_id").as[String].matches("[0-9a-f]{64}"))
    assert((proof \ "occurrence_id").as[String].matches("[0-9a-f]{64}"))
    assert((proof \ "dependency_fingerprint").as[String].matches("[0-9a-f]{64}"))
    assertEquals((proof \ "terminal_step_index").as[Int], 2)
    assertEquals((proof \ "terminal" \ "kind").as[String], "occupation")
    assertEquals((proof \ "terminal_reply_move").toOption, None)
    assertEquals((proof \ "occupation_move").toOption, None)
    val route = (proof \ "route").as[List[JsObject]]
    assertEquals(route.map(step => (step \ "move_uci").as[String]), List("d6c6"))
    assertEquals(route.map(step => (step \ "step_index").as[Int]), List(2))
    assertEquals((route.head \ "from").as[String], "d6")
    assertEquals((route.head \ "to").as[String], "c6")

    val reference = (proof \ "counterfactual_reference_branch").as[JsObject]
    val played = (proof \ "played_root_branch").as[JsObject]
    assertEquals(
      (reference \ "steps").as[List[JsObject]].map(step => (step \ "move_uci").as[String]),
      referenceMoves
    )
    assertEquals(
      (played \ "steps").as[List[JsObject]].map(step => (step \ "move_uci").as[String]),
      playedMoves
    )
    assertEquals((played \ "branch_role").as[String], "played_root_analysis_continuation")
    assertEquals((played \ "root_provenance").as[String], "observed_game_root")
    assertEquals(
      (played \ "steps").as[List[JsObject]].map(step => (step \ "provenance").as[String]),
      List("observed_game_move", "certified_analysis_move")
    )

    val path = (proof \ "proof_paths").as[List[JsObject]] match
      case exact :: Nil => exact
      case other        => fail(s"expected one retained vacancy proof path, found ${other.size}")
    val premises = (path \ "premises").as[List[JsObject]]
    assertEquals(
      premises.map(premise => (premise \ "role").as[String]),
      List("reference_release_move", "reference_route_move_0")
    )
    assertEquals(premises.map(premise => (premise \ "contract").as[String]).distinct, List("legal_move"))
    assertEquals(
      premises.map(premise => (premise \ "movement_mode").as[String]).distinct,
      List("controlled_destination")
    )
    assertEquals(premises.map(premise => (premise \ "step_index").as[Int]), List(0, 2))
    assertEquals(premises.map(premise => (premise \ "move_uci").as[String]), List("c6g2", "d6c6"))
    premises.foreach { premise =>
      assert((premise \ "legal_move_semantic_id").as[String].matches("[0-9a-f]{64}"))
      val sources = (premise \ "source_premise_ids").as[List[String]]
      assert(sources.contains((premise \ "issuer_evidence_id").as[String]))
      assert(sources.contains((premise \ "issuer_occurrence_id").as[String]))
    }

    val absences = (path \ "closed_absence_uses").as[List[JsObject]]
    assertEquals(absences.map(use => (use \ "role").as[String]), List("played_first_route_leg_absent"))
    assertEquals(absences.map(use => (use \ "query").as[String]), List("legal-move-from-to:white:d6:c6"))
    assertEquals((absences.head \ "branch_id").as[String], (played \ "branch_id").as[String])

    val states = (path \ "closed_state_uses").as[List[JsObject]]
    assertEquals(
      states.map(use => (use \ "role").as[String]),
      List(
        "reference_vacancy",
        "reference_vacancy",
        "reference_route_piece_0",
        "played_blocker_persistence",
        "played_blocker_persistence",
        "played_route_origin_persistence",
        "played_route_origin_persistence"
      )
    )
    assertEquals(
      states.map(use => (use \ "query").as[String]),
      List(
        "vacant:c6",
        "vacant:c6",
        "occupied-by:white:rook@c6",
        "occupied-by:white:bishop@c6",
        "occupied-by:white:bishop@c6",
        "occupied-by:white:rook@d6",
        "occupied-by:white:rook@d6"
      )
    )
    assert(states.take(3).forall(use => (use \ "branch_id").as[String] == (reference \ "branch_id").as[String]))
    assert(states.drop(3).forall(use => (use \ "branch_id").as[String] == (played \ "branch_id").as[String]))

    assertEquals((proof \ "participants" \ "releaser" \ "from").as[String], "c6")
    assertEquals((proof \ "participants" \ "releaser" \ "piece_before").as[String], "bishop")
    assertEquals((proof \ "participants" \ "released_blocker" \ "square").as[String], "c6")
    assertEquals((proof \ "participants" \ "route_piece" \ "square").as[String], "d6")
    assertEquals((proof \ "participants" \ "route_piece" \ "piece").as[String], "rook")

  test("v6 commentary preserves the Silicon Road multi-leg route and all five check replies"):
    val rootFen = "1r3n2/pp6/2p1p2k/N3R2p/3PP1p1/PP2K1P1/7P/8 w - - 1 31"
    val referenceMoves = List(
      "b3b4",
      "h6g6",
      "a5b3",
      "b7b6",
      "b3c1",
      "b8e8",
      "c1d3",
      "f8d7",
      "d3f4",
      "g6f7"
    )
    val playedMoves = List("a3a4", "h6g6")
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
      .getOrElse(fail("expected the exact Silicon Road route packet"))

    val facets = (RuntimeProtocol.moveCommentaryJson(packet) \ "causal_explanations")
      .as[List[JsObject]]
      .filter(facet => (facet \ "kind").as[String] == "missed_square_release")
    val proofs = facets.flatMap(facet =>
      (facet \ "channels").as[List[JsObject]].map(channel =>
        (channel \ "square_release_route_proof").as[JsObject]
      )
    )
    assertEquals(facets.size, 2)
    assertEquals(proofs.size, 2)
    val proof = proofs.find(value => (value \ "terminal" \ "kind").as[String] == "created_check")
      .getOrElse(fail("expected the multi-leg created-check route"))

    assertEquals(
      (proof \ "route").as[List[JsObject]].map(step => (step \ "move_uci").as[String]),
      List("a5b3", "b3c1", "c1d3", "d3f4")
    )
    assertEquals(
      (proof \ "route").as[List[JsObject]].map(step => (step \ "step_index").as[Int]),
      List(2, 4, 6, 8)
    )
    assertEquals((proof \ "terminal_step_index").as[Int], 8)
    assertEquals((proof \ "terminal_reply_move").as[String], "g6f7")
    val terminal = (proof \ "terminal").as[JsObject]
    assert((terminal \ "assertion_id").as[String].matches("[0-9a-f]{64}"))
    assertEquals((terminal \ "terminal_state").as[String], "ongoing")
    val responses = (terminal \ "responses").as[List[JsObject]]
    assertEquals(responses.size, 5)
    assertEquals(
      responses.map(response => (response \ "resource" \ "move_uci").as[String]).contains("g6f7"),
      true
    )

    val path = (proof \ "proof_paths").as[List[JsObject]] match
      case exact :: Nil => exact
      case other        => fail(s"expected one independent terminal path, found ${other.size}")
    assertEquals(
      (path \ "premises").as[List[JsObject]].map(premise => (premise \ "role").as[String]),
      List(
        "reference_terminal_resource",
        "reference_release_move",
        "reference_route_move_0",
        "reference_route_move_1",
        "reference_route_move_2",
        "reference_route_move_3",
        "reference_terminal_reply"
      )
    )
    assertEquals((path \ "closed_state_uses").as[List[JsObject]].size, 13)

  test("v6 commentary serializes the closed passed-pawn result without a generic proof segment"):
    val raw = RawMoveReviewInput(
      fen = "7k/5K2/6P1/8/8/8/8/8 w - - 0 1",
      playedMoveUci = "g6g7",
      variations = List(
        EngineLine(List("f7e7", "h8g8", "e7e8"), scoreCp = 600, depth = 20),
        EngineLine(List("g6g7", "h8h7", "g7g8q"), scoreCp = -600, depth = 20)
      )
    )
    val resolved = MoveReviewJudgmentOrchestrator
      .execute(raw)
      .getOrElse(fail("expected the admitted main PV to close passed-pawn progress after the only legal reply"))
    val passedPawnProgressRealizedAfterOnlyLegalReplyProofRecords = resolved.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence, _) => record
    }
    assert(passedPawnProgressRealizedAfterOnlyLegalReplyProofRecords.nonEmpty, "the closed passed-pawn result must reach the typed L2 producer")
    val passedPawnResultCauseRecords = resolved.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.PassedPawnProgress => record
    }
    assert(
      passedPawnResultCauseRecords.nonEmpty,
      clues(
        passedPawnProgressRealizedAfterOnlyLegalReplyProofRecords.map(_.ref.id),
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
    val passedPawnProgressFacet = (commentary \ "causal_explanations").as[List[JsObject]]
      .find(facet => (facet \ "kind").as[String] == "passed_pawn_progress")
      .getOrElse(fail("expected the selected typed passed-pawn result in public commentary"))
    val channel = (passedPawnProgressFacet \ "channels").as[List[JsObject]] match
      case exact :: Nil => exact
      case other =>
        fail(s"expected one passed-pawn-progress-realized-after-only-legal-reply channel, found ${other.size}")

    assertEquals(passedPawnProgressFacet.keys, Set("cause_evidence_id", "kind", "exposure", "channels"))
    assertEquals((passedPawnProgressFacet \ "exposure").as[String], "complementary")
    assertEquals(channel.keys, Set("channel_id", "passed_pawn_progress_realized_after_only_legal_reply_proof"))
    assertEquals(
      channel.keys.intersect(
        Set("actor", "targets", "mechanisms", "consequences", "witnesses", "proof_line_moves", "horizon", "proof_segment")
      ),
      Set.empty[String]
    )
    val proof = (channel \ "passed_pawn_progress_realized_after_only_legal_reply_proof").as[JsObject]
    assert(!proof.keys.contains("comparison_evidence_id"))
    assert((proof \ "semantic_id").as[String].matches("[0-9a-f]{64}"))
    assert((proof \ "occurrence_id").as[String].matches("[0-9a-f]{64}"))
    assert((proof \ "dependency_fingerprint").as[String].matches("[0-9a-f]{64}"))
    assertEquals((proof \ "root_move").as[String], "g6g7")
    assertEquals((proof \ "realizing_move").as[String], "g7g8q")
    assert((proof \ "root_ply").as[Int] <= (proof \ "realizing_ply").as[Int])
    assert((proof \ "result_target_subjects").as[List[String]].nonEmpty)
    assertEquals((proof \ "root_actor" \ "from").as[String], "g6")
    assertEquals((proof \ "realizing_actor" \ "to").as[String], "g8")
    val inventory = (proof \ "closed_legal_reply_inventory").as[JsObject]
    assertEquals(
      inventory.keys,
      Set(
        "issuer_evidence_id",
        "root_after",
        "legal_reply_move",
        "analysis_continuation_branch_id"
      )
    )
    assert((inventory \ "issuer_evidence_id").as[String].nonEmpty)
    assertEquals((inventory \ "legal_reply_move").as[String], "h8h7")
    val branches = (proof \ "branches").as[List[JsObject]]
    assertEquals(branches.size, 1)
    val analysisContinuation = branches.head
    assertEquals((analysisContinuation \ "role").as[String], "played_root_analysis_continuation")
    assertEquals((analysisContinuation \ "root_provenance").as[String], "observed_game_root")
    assertEquals((analysisContinuation \ "reply_move").as[String], "h8h7")
    assert((analysisContinuation \ "source_occurrence_id").as[String].nonEmpty)
    assertEquals(
      (inventory \ "analysis_continuation_branch_id").as[String],
      (analysisContinuation \ "branch_id").as[String]
    )
    val continuationSteps = (analysisContinuation \ "steps").as[List[JsObject]]
    assert(continuationSteps.forall(step =>
      (step \ "step_key").as[String].nonEmpty && (step \ "line" \ "line_id").as[String].nonEmpty
    ))
    assertEquals(
      continuationSteps.map(step => (step \ "provenance").as[String]),
      List("observed_game_move", "certified_analysis_move", "certified_analysis_move")
    )
    val proofPaths = (proof \ "proof_paths").as[List[JsObject]]
    assertEquals(proofPaths.map(path => (path \ "path_occurrence_id").as[String]).distinct.size, proofPaths.size)
    assertEquals(
      proofPaths.map(path => (path \ "analysis_continuation_branch_id").as[String]).toSet,
      Set((analysisContinuation \ "branch_id").as[String])
    )
    assert(proofPaths.forall(path => (path \ "premises").as[List[JsObject]].nonEmpty))
    assert(proofPaths.forall(path =>
      (path \ "premises").as[List[JsObject]].forall(premise =>
        Set("dependency", "result")((premise \ "role").as[String])
      )
    ))
    val dependencyPremises = proofPaths.flatMap(path =>
      (path \ "premises").as[List[JsObject]].filter(_.keys.contains("dependency_proof"))
    )
    val positionStateUses = dependencyPremises.flatMap(premise =>
      (premise \ "dependency_proof" \ "position_state_issuers").as[List[JsObject]].map(premise -> _)
    )
    assert(positionStateUses.nonEmpty, "the public causal route must retain its closed intervening states")
    assert(positionStateUses.forall { case (premise, issuer) =>
      val state = (issuer \ "state").as[JsObject]
      val evidenceOwner = (issuer \ "issuer_evidence_id").as[String]
      val sourceIds = (premise \ "source_premise_ids").as[List[String]]
      val lineRole = (issuer \ "line" \ "line_role").as[String]
      val expectedScope = lineRole match
        case "played"        => "played_line"
        case "best_reference" => "best_line"
        case "alternative"    => "candidate_line"
      Set("occupied_by", "slider_reach", "pawn_topology")((state \ "kind").as[String]) &&
        !issuer.keys.contains("query") && sourceIds.contains(evidenceOwner) &&
        !sourceIds.contains((issuer \ "semantic_proof_id").as[String]) &&
        sourceIds.contains((issuer \ "issuer_occurrence_id").as[String]) &&
        (issuer \ "scope").as[String] == expectedScope
    })
    assert(proofPaths.forall(path => (path \ "closure_use_ids").as[List[String]].nonEmpty))
    assert(proofPaths.forall(path =>
      (path \ "realization_actor" \ "legal_move_relation").as[String].matches("[0-9a-f]{64}") &&
        (path \ "realization_move").as[String].matches("[a-h][1-8][a-h][1-8][qrbn]?") &&
        (path \ "realization_ply").as[Int] >= 0
    ))
    assert(proofPaths.forall { path =>
      val branchId = (path \ "analysis_continuation_branch_id").as[String]
      val move = (path \ "realization_move").as[String]
      val ply = (path \ "realization_ply").as[Int]
      Option(analysisContinuation)
        .filter(branch => (branch \ "branch_id").as[String] == branchId)
        .exists(branch => (branch \ "steps").as[List[JsObject]].exists(step =>
          (step \ "move_uci").as[String] == move && (step \ "ply").as[Int] == ply
        ))
    })
    val lowerPremiseIds = (proof \ "lower_premise_ids").as[List[String]]
    assert(lowerPremiseIds.nonEmpty)
    assert(positionStateUses.forall { case (_, issuer) =>
      lowerPremiseIds.contains((issuer \ "issuer_evidence_id").as[String])
    })

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
