package io.chesstory.runtime

import java.net.{ InetSocketAddress, URI }
import java.net.http.{ HttpClient, HttpRequest, HttpResponse }
import java.nio.charset.StandardCharsets
import java.util.concurrent.Executors

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration.*

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CandidateLineEvaluation,
  CanonicalPositionHistory
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

  private def start(played: String = "d2d4"): AwaitingEngineWork =
    CommentaryJobReducer.startJob(initialFen, Nil, initialFen, played, resultingFen(played), policy(), 0L)
      .toOption.get match
      case root: AwaitingEngineWork => root
      case other => fail(s"expected the root search, found $other")

  private def post(client: HttpClient, uri: URI, body: JsValue): HttpResponse[String] =
    client.send(HttpRequest.newBuilder(uri).header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(Json.stringify(body))).build(), HttpResponse.BodyHandlers.ofString())

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
    assertEquals(parsed.remainingBudgetMs, None)

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

  test("v6 accepts only a bounded optional remaining request budget"):
    def parse(value: JsValue) = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify(jobRequest() + ("remaining_budget_ms" -> value)).getBytes(StandardCharsets.UTF_8)
    )
    List(1, 10000, 180000).foreach { value =>
      assertEquals(parse(JsNumber(value)).toOption.get.remainingBudgetMs, Some(value))
    }
    List(JsNumber(0), JsNumber(-1), JsNumber(180001), JsNumber(BigDecimal("1.5")), JsNull, JsString("10000"))
      .foreach { value =>
        assertEquals(parse(value), Left(PositionCommentaryJobProtocol.PositionCommentaryJobWireFailure.InvalidJobRequest))
      }

  private def proposalSource(kind: String, producer: String): JsObject =
    Json.obj("kind" -> kind, "producer" -> producer)

  private def proposalRequest(requirements: List[JsObject], id: String = "proposal:1"): JsObject =
    Json.obj("id" -> id, "origin" -> Json.obj(
      "question" -> (proposalSource("author", "opening-book") + ("source_refs" -> Json.arr("chapter:2"))),
      "candidates" -> proposalSource("user", "reader"),
      "priority" -> proposalSource("generator", "quiet-candidates"),
      "evidence" -> proposalSource("model", "suggested-line")), "requirements" -> requirements)

  private def observeRequirement(id: String = "observe:1"): JsObject =
    Json.obj("id" -> id, "kind" -> "observe_move", "prefix_moves_uci" -> Json.arr(), "move_uci" -> "b1c3")

  private def compareRequirement: JsObject =
    Json.obj("id" -> "compare:1", "kind" -> "compare_moves", "prefix_moves_uci" -> Json.arr("e2e4"),
      "reference_move" -> "e7e5", "alternative_move" -> "c7c5", "expected_relation" -> "equal")

  private def parseProposals(proposals: JsValue) = PositionCommentaryJobProtocol.parseJobRequest(
    Json.stringify(jobRequest() + ("proposals" -> proposals)).getBytes(StandardCharsets.UTF_8))

  test("v6 proposal input preserves independent origins and typed requirements without making evidence"):
    val input = proposalRequest(List(observeRequirement(), compareRequirement,
      Json.obj("id" -> "meaning:1", "kind" -> "uninterpreted", "text" -> "Does this prepare the pawn break?")))
    val parsed = parseProposals(Json.arr(input)).toOption.get.proposals.head
    assertEquals(parsed.id, "proposal:1")
    assertEquals(parsed.origin.question, CommentaryProposal.Source("author", "opening-book", None, List("chapter:2")))
    assertEquals(parsed.origin.candidates.kind, "user")
    assertEquals(parsed.origin.priority.kind, "generator")
    assertEquals(parsed.origin.evidence.kind, "model")
    assertEquals(parsed.requirements, List[CommentaryProposal.Requirement](
      CommentaryProposal.ObserveMove("observe:1", Nil, "b1c3"),
      CommentaryProposal.CompareMoves("compare:1", List("e2e4"), "e7e5", "c7c5", Some("equal")),
      CommentaryProposal.Uninterpreted("meaning:1", "Does this prepare the pawn break?")))
    assertEquals(parseProposals(Json.arr()).toOption.get.proposals, Nil)
    assertEquals(parseProposals(Json.arr(proposalRequest(List(compareRequirement - "expected_relation"))))
      .toOption.get.proposals.head.requirements.head,
      CommentaryProposal.CompareMoves("compare:1", List("e2e4"), "e7e5", "c7c5", None))

  test("v6 rejects malformed proposals, duplicate identities and claimed admission while leaving legality to admission"):
    val valid = proposalRequest(List(observeRequirement()))
    val invalidRequirements = List(
      observeRequirement() + ("move_uci" -> JsString("B1C3")),
      observeRequirement() + ("prefix_moves_uci" -> Json.toJson(List.fill(80)("g1f3"))),
      observeRequirement() + ("resulting_fen" -> JsString(initialFen)),
      compareRequirement + ("expected_relation" -> JsString("best")),
      compareRequirement + ("expected_relation" -> JsNull),
      compareRequirement + ("alternative_move" -> JsString("e7e5")),
      Json.obj("id" -> "meaning:1", "kind" -> "uninterpreted", "text" -> ""))
    val invalidInputs = List[JsValue](JsNull, Json.obj(), Json.arr(valid, valid),
      Json.arr(valid + ("origin_admission" -> JsString("internal"))),
      Json.arr(proposalRequest(Nil)),
      Json.arr(proposalRequest(List(observeRequirement(), observeRequirement())))) ++
      invalidRequirements.map(requirement => Json.arr(proposalRequest(List(requirement))))
    invalidInputs.foreach(value => assertEquals(parseProposals(value),
      Left(PositionCommentaryJobProtocol.PositionCommentaryJobWireFailure.InvalidJobRequest)))
    val illegalButCanonical = observeRequirement() + ("move_uci" -> JsString("b1b8"))
    assert(parseProposals(Json.arr(proposalRequest(List(illegalButCanonical)))).isRight)

  test("v6 fact proposals preserve finite selectors and only typed earlier dependencies"):
    val piece = Json.obj("side" -> "white", "role" -> "pawn", "square" -> "e2")
    val fact = Json.obj("id" -> "fact:1", "kind" -> "fact", "path" -> Json.obj("binding" -> "route",
      "source" -> Json.obj("kind" -> "declared", "moves_uci" -> Json.arr("e2e4"))),
      "predicate" -> Json.obj("kind" -> "presence", "subject" -> piece),
      "at" -> Json.obj("kind" -> "ply", "ply_count" -> 0), "expected" -> "present")
    val later = fact ++ Json.obj("id" -> "fact:2", "at" -> Json.obj("kind" -> "event", "event" -> "move",
      "actor" -> piece, "ordinal" -> 1, "phase" -> "after", "from_ply" -> 0, "through_ply" -> 1))
    val comparison = Json.obj("id" -> "time", "kind" -> "fact_comparison",
      "left_requirement_id" -> "fact:1", "right_requirement_id" -> "fact:2", "relation" -> "before")
    val group = Json.obj("id" -> "all", "kind" -> "fact_group", "mode" -> "all", "members" -> Json.arr("fact:1", "time"))
    val parsed = parseProposals(Json.arr(proposalRequest(List(fact, later, comparison, group)))).toOption.get
    assertEquals(parsed.proposals.head.requirements.head, CommentaryProposal.Fact("fact:1",
      CommentaryFact.Path("route", CommentaryFact.Declared(List("e2e4"))),
      CommentaryFact.Predicate("presence", subject = Some(CommentaryFact.Piece("white", "pawn", "e2"))),
      CommentaryFact.Ply(0), expected = Some("present")))
    val pairPath = (fact \ "path").as[JsObject] + ("source" -> Json.obj("kind" -> "comparison",
      "requirement_id" -> "compare:1", "arm" -> "alternative"))
    val paired = fact + ("path" -> pairPath)
    assert(parseProposals(Json.arr(proposalRequest(List(compareRequirement, paired)))).isRight)
    assert(parseProposals(Json.arr(proposalRequest(List(paired, compareRequirement)))).isLeft)
    val premisePath = pairPath + ("source" -> Json.obj("kind" -> "premise", "inquiry_id" -> "inquiry:1", "premise_id" -> "premise:1"))
    assert(parseProposals(Json.arr(proposalRequest(List(fact + ("path" -> premisePath))))).isRight)
    val malformed = List(
      List(fact + ("expected" -> JsString("best"))),
      List(fact + ("result" -> Json.obj("state" -> "supported"))),
      List(fact + ("through" -> Json.obj("kind" -> "ply", "ply_count" -> 1)) - "expected"),
      List(fact + ("path" -> (premisePath + ("source" -> ((premisePath \ "source").as[JsObject] +
        ("witness_line_ids" -> Json.arr("line:foreign"))))))),
      List(fact, group), List(group, fact, later, comparison),
      List(fact, later, comparison + ("right_requirement_id" -> JsString("time"))),
      List(fact + ("at" -> Json.obj("kind" -> "event", "event" -> "move", "target" -> piece,
        "ordinal" -> 1, "phase" -> "after", "from_ply" -> 0, "through_ply" -> 1))))
    malformed.foreach(requirements => assert(parseProposals(Json.arr(proposalRequest(requirements))).isLeft))

  test("v6 limits all proposal requirements together and bounds submitted source metadata"):
    val maximum = (1 to 600).toList.map(index => observeRequirement(s"observe:$index"))
    assert(parseProposals(Json.arr(proposalRequest(maximum))).isRight)
    assert(parseProposals(Json.arr(proposalRequest(maximum),
      proposalRequest(List(observeRequirement()), "proposal:2"))).isLeft)
    val valid = proposalRequest(List(observeRequirement()))
    val origin = (valid \ "origin").as[JsObject]
    List(
      proposalSource("engine", "stockfish"),
      proposalSource("generator", "x" * 129),
      proposalSource("generator", "producer") + ("version" -> JsNull),
      proposalSource("generator", "producer") + ("source_refs" -> Json.toJson(List.fill(65)("ref")))
    ).foreach(source => assert(parseProposals(Json.arr(valid + ("origin" ->
      (origin + ("question" -> source))))).isLeft))

  test("v6 produces BestChoice from one LegalLine owner when the played move is best"):
    val played = "e2e4"
    val root = start(played)
    assertEquals(root.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(root.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(root.issuedWork.multiPv, 3)
    assertEquals(root.issuedWork.enginePositionMovesUci, Nil)
    val afterRoot = CommentaryJobReducer
      .submitIssuedWorkReport(root, completedReport(root.issuedWork, rootLines), nowEpochMs = 1L)
      .toOption
      .getOrElse(fail("the root report must be admitted"))
    val completed = afterRoot match
      case value: CompletedCommentaryJob => value
      case other => fail(s"the best played move must produce a completed review, found $other")
    assertEquals(completed.progress.physicalWorksIssued, 1)
    assertEquals(completed.progress.physicalReportsAccepted, 1)
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
      case Some(PlayerFacingPrimary.BestChoice(comparisonEvidence)) =>
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
    assertEquals((response \ "schema_version").as[String], "chesstory.position-commentary.response.v6")
    val selectedReviews = (response \ "result" \ "selected_move_reviews").as[List[JsObject]]
    assertEquals(selectedReviews.size, 1)
    assertEquals((selectedReviews.head \ "selection" \ "roles").as[List[String]], List("best", "played"))
    val primary = (selectedReviews.head \ "commentary" \ "primary").as[JsObject]
    assertEquals((primary \ "kind").as[String], "best_choice")
    assertEquals((primary \ "best_endpoint" \ "moves").as[List[String]].headOption, Some(played))
    assertEquals((primary \ "runner_up_endpoint" \ "moves").as[List[String]].headOption, Some("d2d4"))

  test("focused comparison preserves a changed best suffix and compares its own scores"):
    val played = "a2a3"
    val root = start(played)
    val focus = CommentaryJobReducer
      .submitIssuedWorkReport(root, completedReport(root.issuedWork, rootLines), nowEpochMs = 1L)
      .toOption
      .collect { case value: AwaitingEngineWork => value }
      .get

    assertEquals(focus.issuedWork.purpose, EngineWorkPurpose.FocusComparison)
    assertEquals(focus.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("e2e4", played)))
    assertEquals(focus.issuedWork.multiPv, 2)
    assertEquals(focus.progress.physicalWorksIssued, 2)

    val completed = CommentaryJobReducer
      .submitIssuedWorkReport(
        focus,
        completedReport(
          focus.issuedWork,
          List(
            List("e2e4", "c7c5", "g1f3") -> 31,
            List("a2a3", "e7e5", "g1f3") -> -10
          )
        ),
        nowEpochMs = 2L
      )
      .toOption
      .collect { case value: CompletedCommentaryJob => value }
      .getOrElse(fail("different search observations must not stop legal-line preservation"))
    assertEquals(completed.progress.physicalReportsAccepted, 2)
    val review = completed.result match
      case CompletedPositionCommentary.FocusedMoveReview(_, value) => value
      case _ => fail("expected the focused move review")
    val packet = review.judgmentPacket
    val lines = packet.evidenceGraph.records.collect {
      case EvidenceRecord(_, line: LineFactEvidence, _) => line.lineReplayMoves
    }
    assert(lines.contains(List("e2e4", "e7e5", "g1f3")))
    assert(lines.contains(List("e2e4", "c7c5", "g1f3")))
    val reference = packet.candidateLines.find(_.role == LineNodeRole.BestReference).get
    assertEquals(reference.evaluation.engineLine.map(_.scoreCp), Some(31))
    assertEquals(reference.evaluation.moves, List("e2e4", "c7c5", "g1f3"))

  test("execution identity and minimal v6 engine report bind exactly one outstanding work"):
    val root = start("d2d4")
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

  test("engine work failure reports preserve cancellation, budget, and execution categories"):
    val root = start("d2d4")

    def parseFailure(code: String) =
      PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify(
          Json.obj(
            "schema_version" -> PositionCommentaryJobProtocol.EngineWorkReportSchema,
            "engine_profile" -> engineProfile.wireId,
            "work_id" -> root.issuedWork.workId,
            "execution_key_sha256" -> root.issuedWork.executionKeySha256,
            "outcome" -> Json.obj("kind" -> "executor_failed", "failure_code" -> code)
          )
        ).getBytes(StandardCharsets.UTF_8)
      )

    List(
      ("cancelled", EngineWorkFailure.Cancelled, CommentaryJobStopCondition.Cancelled),
      ("lease_expired", EngineWorkFailure.BudgetExhausted, CommentaryJobStopCondition.SearchBudgetExhausted),
      ("search_budget_exhausted", EngineWorkFailure.BudgetExhausted, CommentaryJobStopCondition.SearchBudgetExhausted),
      ("engine_failure", EngineWorkFailure.ExecutionFailed, CommentaryJobStopCondition.EngineExecutionFailed),
      ("engine_unavailable", EngineWorkFailure.EngineUnavailable, CommentaryJobStopCondition.EngineUnavailable),
      ("browser_unsupported", EngineWorkFailure.BrowserUnsupported, CommentaryJobStopCondition.BrowserUnsupported)
    ).foreach { case (code, expectedFailure, expectedStop) =>
      val report = parseFailure(code).toOption.get
      report match
        case EngineWorkReport.ExecutorFailed(_, _, _, failure) => assertEquals(failure, expectedFailure)
        case _ => fail(s"expected executor failure for $code")
      val stopped = CommentaryJobReducer
        .submitIssuedWorkReport(root, report, nowEpochMs = 1L)
        .toOption
        .collect { case value: StoppedCommentaryJob => value }
        .get
      assertEquals(stopped.stopCondition, expectedStop)
    }

    assertEquals(
      parseFailure("unknown_failure"),
      Left(PositionCommentaryJobProtocol.PositionCommentaryJobWireFailure.InvalidEngineWorkReport)
    )

  test("admission replays browser lines and truncates only at an automatic terminal"):
    val drawFen = "7k/8/8/8/K7/1p6/8/8 w - - 0 1"
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
      val beforeRequest = System.currentTimeMillis()
      val created = post(client, jobs,
        jobRequest("g1f3") + ("remaining_budget_ms" -> JsNumber(10000)) +
          ("proposals" -> Json.arr(proposalRequest(List(observeRequirement(),
            Json.obj("id" -> "meaning:1", "kind" -> "uninterpreted", "text" -> "A possible preparation"))))))
      assertEquals(created.statusCode(), 201)
      val awaiting = Json.parse(created.body()).as[JsObject]
      val deadline = (awaiting \ "deadline_epoch_ms").as[Long]
      assert(deadline >= beforeRequest + 10000)
      assert(deadline <= System.currentTimeMillis() + 10000)
      assertEquals((awaiting \ "schema_version").as[String], PositionCommentaryJobProtocol.JobStatusSchema)
      assertEquals((awaiting \ "issued_engine_work" \ "purpose").as[String], "root_search")
      assertEquals((awaiting \ "issued_engine_work" \ "variant").as[String], "standard")
      val observations = (awaiting \ "observations").as[JsObject]
      val proposal = (observations \ "selection" \ "proposals").as[List[JsObject]].head
      assertEquals((proposal \ "admission").as[String], "submitted")
      assertEquals((proposal \ "origin" \ "question" \ "source_refs").as[List[String]], List("chapter:2"))
      assertEquals((observations \ "selection" \ "local_expansions_used").as[Int], 0)
      val facts = (observations \ "statements").as[List[JsObject]]
      val development = facts.filter(fact => (fact \ "kind").as[String] == "development")
      assertEquals(development.size, 1)
      assertEquals((development.head \ "central_squares").as[List[String]], List("d4", "e5"))
      assert(facts.exists(fact => (fact \ "kind").as[String] == "piece_control"))
      val workId = (awaiting \ "issued_engine_work" \ "work_id").as[String]
      val executionKey = (awaiting \ "issued_engine_work" \ "execution_key_sha256").as[String]
      val reportUri = URI.create(
        s"$jobs/${(awaiting \ "job_id").as[String]}/engine-work-reports"
      )
      val failed = post(client, reportUri, Json.obj(
        "schema_version" -> PositionCommentaryJobProtocol.EngineWorkReportSchema,
        "engine_profile" -> engineProfile.wireId, "work_id" -> workId,
        "execution_key_sha256" -> executionKey,
        "outcome" -> Json.obj("kind" -> "executor_failed", "failure_code" -> "engine_failure")))
      assertEquals(failed.statusCode(), 200)
      val stopped = Json.parse(failed.body())
      assertEquals((stopped \ "state").as[String], "stopped")
      assertEquals((stopped \ "stop_condition").as[String], "engine_execution_failed")
      assertEquals((stopped \ "progress" \ "physical_reports_accepted").as[Int], 1)
      val stoppedProposals = (stopped \ "observations" \ "selection" \ "proposals").as[List[JsObject]]
      val expectedRequirements = (proposal \ "requirements").as[List[JsObject]].map { row =>
        if (row \ "id").as[String] == "observe:1" then
          row ++ Json.obj("state" -> "stopped", "stop_reason" -> "engine_execution_failed")
        else row
      }
      assertEquals(stoppedProposals, List(proposal + ("requirements" -> Json.toJson(expectedRequirements))))
      def withoutProposalReadings(value: JsObject): JsObject =
        value + ("selection" -> ((value \ "selection").as[JsObject] - "proposals"))
      CommentaryDeliveryPreservationAssertions.assertRootStopPreservesObservations(
        withoutProposalReadings(observations), withoutProposalReadings((stopped \ "observations").as[JsObject]),
        workId, "engine_execution_failed")
      val jobUri = URI.create(s"$jobs/${(awaiting \ "job_id").as[String]}")
      for encoding <- List("gzip", "gzip;q=0", "br, gzip;q=0.5") do
        val delivered = client.send(HttpRequest.newBuilder(jobUri).header("Accept-Encoding", encoding).GET().build(),
          HttpResponse.BodyHandlers.ofByteArray())
        val compressed = encoding != "gzip;q=0"
        assertEquals(delivered.headers().firstValue("Content-Encoding").orElse(""), if compressed then "gzip" else "")
        val bytes = if compressed then
          val input = new java.util.zip.GZIPInputStream(new java.io.ByteArrayInputStream(delivered.body()))
          try input.readAllBytes() finally input.close()
        else delivered.body()
        assertEquals(Json.parse(bytes), stopped)
        if compressed then assert(delivered.body().length < bytes.length)
      val release = HttpRequest.newBuilder(jobUri).header("Prefer", "return=minimal").DELETE().build()
      val released = client.send(release, HttpResponse.BodyHandlers.ofString())
      assertEquals(released.statusCode(), 204)
      assertEquals(released.body(), "")
      assertEquals(client.send(HttpRequest.newBuilder(jobUri).GET().build(),
        HttpResponse.BodyHandlers.ofString()).statusCode(), 404)
      assertEquals(client.send(release, HttpResponse.BodyHandlers.ofString()).statusCode(), 204)
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
      val created = post(client, jobs, request)
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
      val completed = post(client, reportUri, report)
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
      assert(snapshots.forall(_.isInstanceOf[CommentaryJobSnapshot]))
      assert(snapshots.forall(snapshot =>
        snapshot.asInstanceOf[CommentaryJobSnapshot].state.asInstanceOf[AwaitingEngineWork].issuedWork.workId == "work:0"
      ))
    finally
      executor.shutdownNow()
