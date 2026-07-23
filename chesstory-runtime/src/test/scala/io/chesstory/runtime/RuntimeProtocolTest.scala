package io.chesstory.runtime

import chess.opening.OpeningDb
import lila.chessjudgment.analysis.opening.OpeningRecognitionIndex
import lila.chessjudgment.model.{ PlanId, ProbePurpose, ProbeRequest }
import lila.chessjudgment.model.judgment.OpeningRecognitionMatchKind
import play.api.libs.json.*

class RuntimeProtocolTest extends munit.FunSuite:

  private val input = Json.obj(
    "fen" -> chess.variant.Standard.initialFen.value,
    "playedMoveUci" -> "d2d4",
    "variations" -> Json.arr(
      Json.obj("moves" -> Json.arr("e2e4", "e7e5", "g1f3"), "scoreCp" -> 30, "depth" -> 16),
      Json.obj("moves" -> Json.arr("d2d4", "d7d5", "g1f3"), "scoreCp" -> 20, "depth" -> 16)
    ),
    "currentEvalCp" -> 20,
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

  test("v2 response exposes only chess meaning and the probe contract"):
    RuntimeResources.verify()
    val result = RuntimeProtocol.evaluate(request())
    assertEquals(result.httpStatus, 200)
    assertEquals((result.body \ "schema_version").as[String], RuntimeProtocol.ResponseSchema)
    assert(Set("schema_version", "probe_requests", "move_review").subsetOf(result.body.keys), result.body.keys)
    (result.body \ "move_review").asOpt[JsObject].foreach: review =>
      assert(Set("renderable", "explanations").subsetOf(review.keys), review.keys)
    val serialized = Json.stringify(result.body)
    List(
      "evidenceGraph",
      "validation",
      "quality",
      "positions",
      "claims",
      "principal_plan_id",
      "source_ids",
      "cause_ids",
      "positive_functional_proof_ids",
      "idea_chains",
      "move_semantics",
      "proof_levels",
      "carriers",
      "purpose_carriers",
      "function_carriers",
      "consequence_carriers",
      "principal_plan_event",
      "enabled_by_starting_move",
      "ply_offset"
    ).foreach: forbidden =>
      assert(!serialized.contains(s"\"$forbidden\""), clues(forbidden))

  test("v1 rejects unknown schema and oversized PV bundles"):
    assertEquals(RuntimeProtocol.evaluate(request("v2")).httpStatus, 400)
    val oversized = input + ("variations" -> JsArray(List.fill(17)((input \ "variations")(0))))
    val result = RuntimeProtocol.evaluate(request(body = oversized))
    assertEquals(result.httpStatus, 400)
    assertEquals((result.body \ "error").as[String], "input_limits_exceeded")

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
      "currentEvalCp" -> 0,
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
      "evalCp" -> 0,
      "replyLines" -> replyLines,
      "deltaVsBaseline" -> 0,
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

  test("public opponent resource probe closes without internal routing metadata"):
    val baseInput = Json.obj(
      "fen" -> "r4rkb/4np1p/p1b1p1pP/1p2P3/3P4/3BBN2/P2K1PP1/1R5R w - - 1 20",
      "playedMoveUci" -> "f3g5",
      "variations" -> Json.arr(
        Json.obj("moves" -> Json.arr("f3g5", "c6d5"), "scoreCp" -> 77, "depth" -> 18),
        Json.obj("moves" -> Json.arr("f3h4", "f7f6"), "scoreCp" -> 23, "depth" -> 18),
        Json.obj("moves" -> Json.arr("d3c2", "f7f6"), "scoreCp" -> 21, "depth" -> 18)
      ),
      "currentEvalCp" -> 77,
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
      "evalCp" -> 193,
      "replyLines" -> Json.arr(
        Json.obj("moves" -> Json.arr("f7f6", "g5e6"), "scoreCp" -> 193, "depth" -> 16)
      ),
      "deltaVsBaseline" -> 116,
      "purpose" -> (probe \ "purpose").as[String],
      "probedMove" -> (probe \ "candidateMove").as[String],
      "depth" -> 16,
      "candidateMove" -> (probe \ "candidateMove").as[String],
      "opponentResourceMove" -> (probe \ "opponentResourceMove").as[String],
      "variationHash" -> (probe \ "variationHash").as[String]
    )
    val closed = RuntimeProtocol.evaluate(request(body = baseInput + ("probeResults" -> Json.arr(result))))
    assertEquals(closed.httpStatus, 200)
    assertEquals((closed.body \ "status").as[String], "ready")
    assertEquals((closed.body \ "availability" \ "reason").asOpt[String], None)
    assert((closed.body \ "move_review" \ "renderable").as[Boolean], closed.body)
    val pendingIds = (closed.body \ "probe_requests").as[JsArray].value.flatMap(value => (value \ "id").asOpt[String])
    assert(!pendingIds.contains((probe \ "id").as[String]), pendingIds)

  test("client tablebase proof returns human chess moves and closes only its exact request"):
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
      "currentEvalCp" -> 897,
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
      "evalCp" -> 0,
      "deltaVsBaseline" -> 0,
      "purpose" -> (probe \ "purpose").as[String],
      "variationHash" -> (probe \ "variationHash").as[String],
      "tablebase" -> evidence
    )
    val closed = RuntimeProtocol.evaluate(request(body = baseInput + ("probeResults" -> Json.arr(probeResult))))
    assertEquals(closed.httpStatus, 200)
    assert(!(closed.body \ "probe_requests").as[JsArray].value.exists(request => (request \ "id").asOpt[String].contains((probe \ "id").as[String])))
    val exactProofs = (closed.body \\ "exact_proof").flatMap(_.asOpt[JsObject])
    assert(exactProofs.exists(proof => (proof \ "kind").asOpt[String].contains("zugzwang")))
    val replies = exactProofs.flatMap(proof => (proof \ "legal_replies").asOpt[List[JsObject]].getOrElse(Nil))
    assertEquals(replies.flatMap(reply => (reply \ "uci").asOpt[String]).distinct.sorted, List("d7c8", "d7d8", "d7e7", "d7e8"))
    assert(replies.forall(reply => (reply \ "san").asOpt[String].nonEmpty && (reply \ "notation").asOpt[String].exists(_.startsWith("3..."))))
