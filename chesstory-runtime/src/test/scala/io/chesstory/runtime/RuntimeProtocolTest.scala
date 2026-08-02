package io.chesstory.runtime

import chess.opening.OpeningDb
import lila.chessjudgment.analysis.assembly.JudgmentBoundaryIntervention
import lila.chessjudgment.analysis.opening.OpeningRecognitionIndex
import lila.chessjudgment.model.{ PlanId, ProbePurpose, ProbeRequest }
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

class RuntimeProtocolTest extends munit.FunSuite:

  private val input = Json.obj(
    "fen" -> chess.variant.Standard.initialFen.value,
    "playedMoveUci" -> "d2d4",
    "variations" -> Json.arr(
      Json.obj("moves" -> Json.arr("e2e4", "e7e5", "g1f3"), "scoreCp" -> 30, "depth" -> 16),
      Json.obj("moves" -> Json.arr("d2d4", "d7d5", "g1f3"), "scoreCp" -> 20, "depth" -> 16)
    ),
    "ply" -> 0,
    "movePrefixUci" -> Json.arr()
  )

  private def request(schema: String = RuntimeProtocol.RequestSchema, body: JsObject = input): JsObject =
    Json.obj("schema_version" -> schema, "request_id" -> "test-1", "input" -> body)

  test("opening recognition reuses the canonical scalachess database"):
    val canonical = OpeningDb.all.head
    val moves = canonical.uci.value.split("\\s+").toList
    val recognized = OpeningRecognitionIndex.default.recognize(moves, canonical.fen.value, moves.size)
    assert(recognized.exists(_.candidates.exists(_.identity.name.contains(canonical.name.value))))

  test("a unique canonical position preserves opening identity across move-order transposition"):
    val canonical = OpeningDb.all.find: opening =>
      opening.eco.value == "E08" &&
        opening.name.value == "Catalan Opening: Closed" &&
        opening.uci.value.endsWith("d1c2")
    val alternateMoves =
      "d2d4 g8f6 c2c4 e7e6 g1f3 d7d5 g2g3 f8e7 f1g2 e8g8 e1g1 b8d7 d1c2".split(" ").toList
    val recognized = canonical.flatMap: opening =>
      OpeningRecognitionIndex.default.recognize(alternateMoves, opening.fen.value, alternateMoves.size)
    assertEquals(recognized.map(_.matchedBy), Some(OpeningRecognitionMatchKind.PositionTransposition))
    assertEquals(recognized.flatMap(_.bestIdentity.flatMap(_.name)), Some("Catalan Opening: Closed"))

  test("v3 response exposes the ready public envelope"):
    RuntimeResources.verify()
    val trace = RuntimeProtocol.evaluateWithBoundary(request(), JudgmentBoundaryIntervention.identity)
    val result = trace.result
    assertEquals(result.httpStatus, 200)
    assertEquals((result.body \ "schema_version").as[String], RuntimeProtocol.ResponseSchema)
    assertEquals((result.body \ "request_id").as[String], "test-1")
    assertEquals((result.body \ "ok").as[Boolean], true)
    assertEquals((result.body \ "status").as[String], "ready")
    assertEquals((result.body \ "availability" \ "state").as[String], "ready")
    val review = (result.body \ "move_review").as[JsObject]
    assert((review \ "idea_status").asOpt[String].nonEmpty, review)
    assert((review \ "explanations").asOpt[JsArray].nonEmpty, review)
    val packet = trace.boundaryExecution.flatMap(_.packet).getOrElse(fail("expected P packet"))
    val observations = (review \ "observations").as[List[JsObject]]
    assert(packet.selectedContentClaimIds.nonEmpty, packet.selectedContentClaimIds)
    val observationKinds = observations.map(value => (value \ "kind").as[String])
    assert(observationKinds.contains("candidate_comparison"), observationKinds)
    assert(observationKinds.contains("strategic_mechanism"), observationKinds)
    assertEquals(observations.map(value => (value \ "claim_id").as[String]), packet.selectedContentClaimIds)
    def assertExactCarrier(value: JsObject, carrier: EvidenceRef): Unit =
      packet.evidenceGraph.record(carrier) match
        case Some(EvidenceRecord(ref, _, parents)) =>
          assertEquals((value \ "carrier" \ "id").as[String], ref.id)
          assertEquals((value \ "parents").as[List[JsObject]].map(value => (value \ "id").as[String]), parents.map(_.id))
        case _ => fail(s"expected registered carrier: ${carrier.id}")
    observations.zip(packet.selectedContentClaimIds).foreach: (value, claimId) =>
      packet.claims.filter(_.id == claimId) match
        case claim :: Nil =>
          claim.content match
            case Some(JudgmentClaimContent.CandidateComparison(carrier, comparison)) =>
              assertEquals((value \ "kind").as[String], "candidate_comparison")
              assertExactCarrier(value, carrier)
              val emitted = (value \ "comparison").as[JsObject]
              def roleCode(role: LineNodeRole): String = role match
                case LineNodeRole.Played        => "played"
                case LineNodeRole.BestReference => "best_reference"
                case LineNodeRole.Alternative   => "alternative"
                case LineNodeRole.Threat        => "threat"
              def assertComparisonLine(actual: JsObject, line: SemanticLineKey): Unit =
                assertEquals(actual.keys, Set("root_move", "role", "rank"))
                assertEquals((actual \ "root_move").as[String], line.rootMove)
                assertEquals((actual \ "role").as[String], roleCode(line.role))
                assertEquals((actual \ "rank").as[Int], line.rank)
              assertComparisonLine(
                (emitted \ "reference").as[JsObject],
                comparison.referenceLine
              )
              assertComparisonLine(
                (emitted \ "candidate").as[JsObject],
                comparison.candidateLine
              )
            case Some(JudgmentClaimContent.StrategicMechanism(carrier)) =>
              assertEquals((value \ "kind").as[String], "strategic_mechanism")
              assertExactCarrier(value, carrier)
            case _ => fail(s"expected selected content: $claimId")
        case _ => fail(s"expected unique selected claim: $claimId")

  test("v3 effect scope exposes the central PlanResult semantic key"):
    val planResult = PlanResultSemanticIdentity(
      source = PlanResultSourceOccurrence("e4e5", 2),
      selectedInducedResponse = Some(PlanResultSourceOccurrence("g6g5", 1)),
      consequenceKind = TransitionConsequenceKind.SpaceGain,
      polarity = StructuralSignalPolarity.Gain,
      goalTargetSubjects = List("center"),
      strength = 3,
      robustness = PlanCausalRobustness.Robust,
      branches = List(PlanResultBranchIdentity(
        replyMoveUci = "g6g5",
        outcome = PlanCausalBranchOutcome.Realized,
        observedThroughPlyOffset = 2,
        realizationMoveUci = Some("e4e5"),
        realizationPlyOffset = Some(2),
        terminalOutcome = Some(PlanCausalTerminalOutcome.Victory),
        terminalPlyOffset = Some(2),
        terminalMoveUci = Some("e4e5")
      )),
      causalRoute = Nil
    )
    def scope(result: PlanResultSemanticIdentity): JsObject =
      RuntimeProtocol.rootOwnedEffectIdentityJson(RootOwnedEffectIdentity(
        RootOwnedEffectPrimitiveKind.PlanResult,
        List("square:e5"),
        List("plan-annotation"),
        Nil,
        Some(result)
      ))
    val scoped = scope(planResult)
    assertEquals((scoped \ "plan_result_semantic_key").as[String], planResult.stableKey)
    assertEquals((scoped \ "plan_ids").as[List[String]], List("plan-annotation"))
    val alternateResponse = planResult.copy(
      selectedInducedResponse = Some(PlanResultSourceOccurrence("b5b4", 1))
    )
    val alternateTerminal = planResult.copy(
      branches = planResult.branches.map(_.copy(terminalOutcome = Some(PlanCausalTerminalOutcome.Draw)))
    )
    List(alternateResponse, alternateTerminal).foreach: different =>
      assertNotEquals(different.stableKey, planResult.stableKey)
      assertEquals((scope(different) \ "plan_result_semantic_key").as[String], different.stableKey)
  test("a request without a primary engine comparison is unavailable before projection"):
    val noPlayedLine = input + ("variations" -> Json.arr(
      Json.obj(
        "moves" -> Json.arr("e2e4", "e7e5", "g1f3"),
        "scoreCp" -> 30,
        "depth" -> 16
      )
    ))
    val trace = RuntimeProtocol.evaluateWithBoundary(
      request(body = noPlayedLine),
      JudgmentBoundaryIntervention.identity
    )
    assertEquals(trace.result.httpStatus, 422)
    assertEquals((trace.result.body \ "error").as[String], "move_review_not_buildable")
    assertEquals(trace.projection, None)

  test("v3 selected Cause projects its comparison and direct channel"):
    val selectedCauseInput = Json.obj(
      "fen" -> "8/5ppp/8/5P1P/2k3P1/2p5/5P2/2K5 b - - 0 1",
      "playedMoveUci" -> "h7h6",
      "variations" -> Json.arr(
        Json.obj(
          "moves" -> Json.arr("f7f6", "h5h6", "g7h6", "c1d1", "c4d5", "d1c2", "d5e4", "g4g5", "h6g5", "c2c3", "g5g4", "c3c2", "e4f4", "c2c3"),
          "scoreCp" -> -534,
          "depth" -> 20
        ),
        Json.obj(
          "moves" -> Json.arr("h7h6", "f5f6", "g7f6", "f2f4", "c4d4", "g4g5", "h6g5", "f4g5", "f6f5", "h5h6", "f5f4", "h6h7", "f4f3", "h7h8q", "d4e3", "h8h5", "c3c2", "c1c2", "e3f4", "h5h4", "f4f5"),
          "scoreCp" -> 682,
          "depth" -> 20
        )
      )
    )
    val result = RuntimeProtocol.evaluate(request(body = selectedCauseInput))
    assertEquals(result.httpStatus, 200)
    val review = (result.body \ "move_review").as[JsObject]
    assertEquals((review \ "idea_status").as[String], "certified")
    val idea = (review \ "explanations")(0)("ideas")(0).as[JsObject]
    assertEquals((idea \ "comparison" \ "kind").as[String], "played_vs_best")
    assert((idea \ "comparison" \ "reference" \ "root_move").asOpt[String].exists(_.nonEmpty), idea)
    assert((idea \ "comparison" \ "candidate" \ "root_move").asOpt[String].exists(_.nonEmpty), idea)
    val channel = (idea \ "channels")(0).as[JsObject]
    assert((channel \ "causal_signature").asOpt[String].exists(_.nonEmpty), channel)
    assert((channel \ "actor").as[JsObject].values.exists(_ != JsNull), channel)
    assert((channel \ "targets").as[JsArray].value.nonEmpty, channel)
    assert((channel \ "mechanisms").as[JsArray].value.nonEmpty, channel)
    assert((channel \ "consequences").as[JsArray].value.nonEmpty, channel)
    assert((channel \ "proof_segment").asOpt[JsObject].nonEmpty, channel)
    assert((channel \ "effect_descriptor").asOpt[JsObject].nonEmpty, channel)

  test("v3 success envelope is withheld when the primary comparison is not engine-backed"):
    val lowDepthVariations = (input \ "variations").as[JsArray].value.map: variation =>
      variation.as[JsObject] + ("depth" -> JsNumber(1))
    val result = RuntimeProtocol.evaluate(request(body = input + ("variations" -> JsArray(lowDepthVariations))))

    assertEquals(result.httpStatus, 200)
    assertEquals((result.body \ "status").as[String], "withheld")
    assertEquals(
      (result.body \ "availability").as[JsObject],
      Json.obj("state" -> "withheld", "reason" -> "insufficient_engine_depth")
    )
    val review = (result.body \ "move_review").as[JsObject]
    assertEquals((review \ "renderable").as[Boolean], false)
    assertEquals((review \ "explanations").as[List[JsObject]], Nil)
    assertEquals((review \ "observations").as[List[JsObject]], Nil)

  test("v1 rejects unknown schema and oversized PV bundles"):
    val unsupported = RuntimeProtocol.evaluate(request("v2"))
    assertEquals(unsupported.httpStatus, 400)
    assertEquals((unsupported.body \ "error").as[String], "unsupported_schema_version")
    val oversized = input + ("variations" -> JsArray(List.fill(17)((input \ "variations")(0))))
    val result = RuntimeProtocol.evaluate(request(body = oversized))
    assertEquals(result.httpStatus, 400)
    assertEquals((result.body \ "error").as[String], "input_limits_exceeded")
    val zeroMate = input + ("variations" -> Json.arr(
      Json.obj(
        "moves" -> Json.arr("e2e4", "e7e5"),
        "scoreCp" -> 0,
        "mate" -> 0,
        "depth" -> 18
      )
    ))
    val zeroMateResult = RuntimeProtocol.evaluate(request(body = zeroMate))
    assertEquals(zeroMateResult.httpStatus, 400)
    assertEquals((zeroMateResult.body \ "error").as[String], "input_limits_exceeded")

  test("public probe request omits internal graph metadata"):
    val json = RuntimeProtocol.publicProbeRequestJson(
      ProbeRequest(
        id = "opaque-probe",
        fen = chess.variant.Standard.initialFen.value,
        moves = Nil,
        depth = 18,
        purpose = Some(ProbePurpose.ReplyMultipv),
        multiPv = Some(3),
        planId = Some(PlanId.QueensideAttack.toString),
        planScore = Some(0.9),
        baselineEvalCp = Some(20),
        objective = Some("internal-objective"),
        seedId = Some("internal-seed"),
        requiredSignals = List("replyLines"),
        horizon = Some("ply:8"),
        candidateMove = Some("d2d4"),
        opponentResourceMove = Some("e7e5"),
        depthFloor = Some(16),
        variationHash = Some("opaque-hash")
      )
    )
    assert(Set("id", "fen", "moves", "depth", "purpose", "horizon").subsetOf(json.keys), json.keys)
    List("planId", "planScore", "objective", "seedId", "requiredSignals").foreach: internalKey =>
      assert((json \ internalKey).toOption.isEmpty, clues(internalKey))
    assertEquals((json \ "horizon").as[String], "ply:8")
    assertEquals((json \ "opponentResourceMove").as[String], "e7e5")
    assert(!Json.stringify(json).contains("internal"))

  test("stripped client probe result still closes the certified branch contract"):
    val baseInput = Json.obj(
      "fen" -> "r1b2rk1/pp2ppbp/5np1/q1nP4/4PB2/2N2P2/PP4PP/2RQKBNR b K - 0 10",
      "playedMoveUci" -> "b7b5",
      "variations" -> Json.arr(Json.obj(
        "moves" -> Json.arr("b7b5", "b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4"),
        "scoreCp" -> 0,
        "depth" -> 16
      )),
      "ply" -> 19,
      "openingContext" -> Json.obj("name" -> "Grunfeld / b5-b4 counterplay versus center"),
      "movePrefixUci" -> Json.arr(
        "d2d4", "g8f6", "c2c4", "g7g6", "b1c3", "d7d5", "c1f4", "f8g7", "e2e3", "c7c5",
        "d4c5", "d8a5", "a1c1", "e8h8", "c4d5", "b8d7", "f2f3", "d7c5", "e3e4"
      )
    )
    val initial = RuntimeProtocol.evaluate(request(body = baseInput))
    assertEquals(initial.httpStatus, 200)
    val probe = (initial.body \ "probe_requests").as[JsArray].value.head.as[JsObject]
    assert((probe \ "horizon").as[String].matches("ply:[1-9][0-9]*"), probe)
    val replyLines = Json.arr(
      Json.obj("moves" -> Json.arr("b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4"), "scoreCp" -> 0, "depth" -> 16),
      Json.obj("moves" -> Json.arr("f1e2", "b5b4"), "scoreCp" -> 0, "depth" -> 16),
      Json.obj("moves" -> Json.arr("h2h3", "b5b4"), "scoreCp" -> 0, "depth" -> 16)
    )
    val result = Json.obj(
      "id" -> (probe \ "id").as[String],
      "fen" -> (probe \ "fen").as[String],
      "replyLines" -> replyLines,
      "purpose" -> (probe \ "purpose").as[String],
      "probedMove" -> (probe \ "candidateMove").as[String],
      "depth" -> (probe \ "depth").as[Int],
      "candidateMove" -> (probe \ "candidateMove").as[String],
      "horizon" -> (probe \ "horizon").as[String],
      "variationHash" -> (probe \ "variationHash").as[String]
    )
    val closed = RuntimeProtocol.evaluate(request(body = baseInput + ("probeResults" -> Json.arr(result))))
    assertEquals(closed.httpStatus, 200)
    assertEquals((closed.body \ "probe_requests").as[JsArray].value.toList, Nil)
    val duplicate = RuntimeProtocol.evaluate(request(body = baseInput + ("probeResults" -> Json.arr(result, result))))
    assertEquals(duplicate.httpStatus, 400)
    assertEquals((duplicate.body \ "error").as[String], "input_limits_exceeded")

  test("engine-backed verdict remains public when no differential Cause is certified"):
    val baseInput = Json.obj(
      "fen" -> "r4rkb/4np1p/p1b1p1pP/1p2P3/3P4/3BBN2/P2K1PP1/1R5R w - - 1 20",
      "playedMoveUci" -> "f3g5",
      "variations" -> Json.arr(
        Json.obj("moves" -> Json.arr("f3g5", "c6d5"), "scoreCp" -> 77, "depth" -> 18),
        Json.obj("moves" -> Json.arr("f3h4", "f7f6"), "scoreCp" -> 23, "depth" -> 18),
        Json.obj("moves" -> Json.arr("d3c2", "f7f6"), "scoreCp" -> 21, "depth" -> 18)
      ),
      "ply" -> 38
    )
    val initial = RuntimeProtocol.evaluate(request(body = baseInput))
    assertEquals(initial.httpStatus, 200)
    val probe = (initial.body \ "probe_requests").as[JsArray].value.collectFirst {
      case obj: JsObject
          if (obj \ "candidateMove").asOpt[String].contains("f3g5") &&
            (obj \ "opponentResourceMove").asOpt[String].contains("f7f6") =>
        obj
    }.getOrElse(fail("expected public f7-f6 resource probe"))
    assert((probe \ "objective").toOption.isEmpty, probe)
    val result = Json.obj(
      "id" -> (probe \ "id").as[String],
      "fen" -> (probe \ "fen").as[String],
      "replyLines" -> Json.arr(
        Json.obj("moves" -> Json.arr("f7f6", "g5e6"), "scoreCp" -> 193, "depth" -> 16)
      ),
      "purpose" -> (probe \ "purpose").as[String],
      "probedMove" -> (probe \ "candidateMove").as[String],
      "depth" -> 16,
      "candidateMove" -> (probe \ "candidateMove").as[String],
      "opponentResourceMove" -> (probe \ "opponentResourceMove").as[String],
      "variationHash" -> (probe \ "variationHash").as[String]
    )
    val closed = RuntimeProtocol.evaluate(
      request(body = baseInput + ("probeResults" -> Json.arr(result)))
    )
    assertEquals(closed.httpStatus, 200)
    assertEquals((closed.body \ "status").as[String], "ready")
    assert((closed.body \ "move_review" \ "renderable").as[Boolean], closed.body)
    val verdict = (closed.body \ "move_review" \ "verdict").as[JsObject]
    assertEquals((verdict \ "comparison_kind").as[String], "played_vs_best")
    assertEquals(
      (closed.body \ "move_review" \ "explanations").as[List[JsObject]],
      Nil
    )
    assertEquals(
      (closed.body \ "move_review" \ "idea_status").as[String],
      "no_certified_differential_idea"
    )
    val pendingIds = (closed.body \ "probe_requests").as[JsArray].value.flatMap(value => (value \ "id").asOpt[String])
    assert(!pendingIds.contains((probe \ "id").as[String]), pendingIds)

  test("client tablebase proof closes its request without bypassing selected Cause exposure"):
    val baseInput = Json.obj(
      "fen" -> "8/1p1k4/1P6/2PK4/8/8/8/8 w - - 0 1",
      "playedMoveUci" -> "d5e5",
      "variations" -> Json.arr(
        Json.obj(
          "moves" -> Json.arr("d5d4", "d7d8", "d4e5", "d8d7", "e5d5", "d7e7", "c5c6", "e7f7", "c6b7", "f7f6", "b7b8q", "f6g5"),
          "scoreCp" -> 1241,
          "depth" -> 18
        ),
        Json.obj(
          "moves" -> Json.arr("d5e5", "d7c6", "e5d4", "c6d7", "d4d5", "d7e7", "c5c6", "b7c6", "d5c6", "e7e6", "b6b7", "e6e7"),
          "scoreCp" -> 897,
          "depth" -> 18
        )
      ),
      "ply" -> 0
    )
    val initial = RuntimeProtocol.evaluate(request(body = baseInput))
    val probe = (initial.body \ "probe_requests").as[JsArray].value
      .map(_.as[JsObject])
      .find(request => (request \ "purpose").asOpt[String].contains("endgame_tablebase"))
      .getOrElse(fail("expected tablebase request"))
    assertEquals((initial.body \ "probe_requests").as[JsArray].value.count(request => (request \ "purpose").asOpt[String].contains("endgame_tablebase")), 1)
    val evidence = Json.obj(
      "terminal" -> Json.obj("fen" -> (probe \ "fen").as[String], "wdl" -> -2, "dtm" -> -22),
      "comparison" -> Json.obj("fen" -> (probe \ "comparisonFen").as[String], "wdl" -> 2, "dtm" -> 27),
      "legalMoves" -> Json.arr(
        Json.obj("moveUci" -> "d7e7", "resultingWdl" -> 2, "resultingDtm" -> 21),
        Json.obj("moveUci" -> "d7e8", "resultingWdl" -> 2, "resultingDtm" -> 19),
        Json.obj("moveUci" -> "d7c8", "resultingWdl" -> 2, "resultingDtm" -> 17),
        Json.obj("moveUci" -> "d7d8", "resultingWdl" -> 2, "resultingDtm" -> 15)
      )
    )
    val probeResult = Json.obj(
      "id" -> (probe \ "id").as[String],
      "fen" -> (probe \ "fen").as[String],
      "purpose" -> (probe \ "purpose").as[String],
      "variationHash" -> (probe \ "variationHash").as[String],
      "tablebase" -> evidence
    )
    val closed = RuntimeProtocol.evaluate(request(body = baseInput + ("probeResults" -> Json.arr(probeResult))))
    assertEquals(closed.httpStatus, 200)
    assert(!(closed.body \ "probe_requests").as[JsArray].value.exists(request => (request \ "id").asOpt[String].contains((probe \ "id").as[String])))
    assert((closed.body \\ "exact_proof").isEmpty)
    assert((closed.body \\ "exact_endgame_techniques").isEmpty)
