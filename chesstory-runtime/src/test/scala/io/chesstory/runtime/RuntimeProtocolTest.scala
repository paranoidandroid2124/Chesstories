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

  test("v3 response exposes only Cause-owned chess meaning and the probe contract"):
    RuntimeResources.verify()
    val trace = RuntimeProtocol.evaluateWithBoundary(request(), JudgmentBoundaryIntervention.identity)
    val result = trace.result
    assertEquals(result.httpStatus, 200)
    val execution = trace.boundaryExecution.getOrElse(fail("expected a closed judgment execution"))
    val packet = execution.packet.getOrElse(fail("expected a closed judgment packet"))
    assertEquals(packet.causeExposureResolution, execution.r.causeExposureResolution)
    assertEquals(packet.causeDispositionLedger, execution.r.causeDispositionLedger)
    assertEquals(
      packet.directCauseImportanceResolution,
      execution.r.causeExposureResolution.importanceResolution
    )
    assertEquals((result.body \ "schema_version").as[String], RuntimeProtocol.ResponseSchema)
    assert(Set("schema_version", "probe_requests", "move_review").subsetOf(result.body.keys), result.body.keys)
    (result.body \ "move_review").asOpt[JsObject].foreach: review =>
      assert(Set("renderable", "idea_status", "idea_status_detail", "explanations").subsetOf(review.keys), review.keys)
      val explanations = (review \ "explanations").asOpt[JsArray].toList.flatMap(_.value)
      val publicCauseIds = explanations.flatMap(explanation =>
        (explanation \ "ideas").asOpt[JsArray].toList.flatMap(_.value)
      ).flatMap(idea => (idea \ "cause_evidence_id").asOpt[String])
      assertEquals(publicCauseIds.distinct.size, publicCauseIds.size)
      assertEquals(publicCauseIds.toSet, packet.causeDispositionLedger.selectedCauseEvidenceIds)
      assertEquals(
        (review \ "idea_status_detail").as[JsObject],
        RuntimeProtocol.causeDispositionPublicJson(packet.causeDispositionLedger)
      )
      assertEquals(
        (review \ "idea_status").as[String],
        if publicCauseIds.nonEmpty then "certified" else "no_certified_differential_idea"
      )
      explanations.foreach: explanation =>
        assert(!(explanation.as[JsObject].keys contains "line"), explanation)
        (explanation \ "ideas").asOpt[JsArray].toList.flatMap(_.value).foreach: idea =>
          assert((idea \ "comparison_exposure_rank").asOpt[Int].nonEmpty, idea)
          assert((idea \ "priority_rank").toOption.isEmpty, idea)
          (idea \ "channels").as[List[JsObject]].foreach: channel =>
            assert((channel \ "proof_segment").toOption.nonEmpty, channel)
            assert((channel \ "importance_effect").toOption.nonEmpty, channel)
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
      "tested_plan_limits",
      "chess_relations",
      "semantic_names",
      "packet_linked_claim_ids",
      "selected_public_idea_claim_ids",
      "priority_rank",
      "priority_rank_meaning"
    ).foreach: forbidden =>
      assert(!serialized.contains(s"\"$forbidden\""), clues(forbidden))

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

  test("packet-selected Cause projection fails closed instead of omitting the idea"):
    val expected = Json.obj("cause" -> "selected")
    assertEquals(
      RuntimeProtocol.requireSelectedCauseProjection("cause-1", Some(expected)),
      expected
    )
    val error = intercept[IllegalStateException]:
      RuntimeProtocol.requireSelectedCauseProjection[JsObject]("cause-1", None)
    assert(error.getMessage.contains("cause-1"))

    val host = "registered-host"
    assertEquals(RuntimeProtocol.requireSelectedCauseHost("claim-1", Some(host)), host)
    val hostError = intercept[IllegalStateException]:
      RuntimeProtocol.requireSelectedCauseHost[String]("claim-1", None)
    assert(hostError.getMessage.contains("claim-1"))

  test("Cause disposition summary is canonical and exact public coverage fails closed"):
    val position = PositionNodeRef(
      chess.variant.Standard.initialFen.value,
      0,
      Some(chess.White)
    )
    def causeRef(id: String): EvidenceRef =
      EvidenceRef(
        id,
        EvidenceProducer.RelativeMoveProducer,
        EvidenceLayer.RelativeCause,
        position,
        None,
        EvidenceScope.Counterfactual,
        EvidenceConfidence.EngineBacked
      )
    val emptySummary = RuntimeProtocol.causeDispositionPublicJson(CauseDispositionLedger.empty)
    assertEquals(
      (emptySummary \ "authority").as[String],
      RuntimeProtocol.CauseDispositionSummaryAuthority
    )
    assertEquals((emptySummary \ "total_cause_count").as[Int], 0)
    assertEquals(
      (emptySummary \ "abstention_codes").as[List[String]],
      List("no_relative_cause_generated")
    )
    assertEquals(
      (emptySummary \ "status_counts").as[JsObject].values.map(_.as[Int]).sum,
      0
    )
    assertEquals(
      (emptySummary \ "reason_counts").as[JsObject].values.map(_.as[Int]).sum,
      0
    )

    val unready = CauseDisposition(
      causeEvidence = causeRef("cause-unready"),
      status = CauseDispositionStatus.ObjectUnready,
      reason = CauseDispositionReason.ObjectReadinessFailed,
      proposedClaimIds = Nil,
      certifiedClaimIds = Nil,
      rankEligibleClaimIds = Nil,
      selectedOwnerClaimId = None,
      relatedCauseEvidenceIds = Nil,
      relatedClaimIds = Nil
    )
    val abstainingLedger = CauseDispositionLedger(List(unready))
    val abstainingSummary = RuntimeProtocol.causeDispositionPublicJson(abstainingLedger)
    assertEquals((abstainingSummary \ "total_cause_count").as[Int], 1)
    assertEquals((abstainingSummary \ "selected_cause_ids").as[List[String]], Nil)
    assertEquals(
      (abstainingSummary \ "abstention_codes").as[List[String]],
      List("object_readiness_failed")
    )
    assertEquals(
      (abstainingSummary \ "status_counts" \ "object_unready").as[Int],
      1
    )
    assertEquals(
      (abstainingSummary \ "reason_counts" \ "object_readiness_failed").as[Int],
      1
    )

    val selected = CauseDisposition(
      causeEvidence = causeRef("cause-selected"),
      status = CauseDispositionStatus.Selected,
      reason = CauseDispositionReason.PlayerFacingSelection,
      proposedClaimIds = List("claim-selected"),
      certifiedClaimIds = List("claim-selected"),
      rankEligibleClaimIds = List("claim-selected"),
      selectedOwnerClaimId = Some("claim-selected"),
      relatedCauseEvidenceIds = Nil,
      relatedClaimIds = Nil
    )
    val selectedLedger = CauseDispositionLedger(List(selected))
    val selectedSummary = RuntimeProtocol.causeDispositionPublicJson(selectedLedger)
    assertEquals(
      (selectedSummary \ "selected_cause_ids").as[List[String]],
      List(selected.causeEvidence.id)
    )
    assertEquals((selectedSummary \ "abstention_codes").as[List[String]], Nil)
    RuntimeProtocol.requirePublicIdeaCauseCoverage(
      selectedLedger,
      List(selected.causeEvidence.id)
    )
    intercept[IllegalStateException]:
      RuntimeProtocol.requirePublicIdeaCauseCoverage(selectedLedger, Nil)
    intercept[IllegalStateException]:
      RuntimeProtocol.requirePublicIdeaCauseCoverage(
        selectedLedger,
        List(selected.causeEvidence.id, selected.causeEvidence.id)
      )

    val withheld = RuntimeProtocol.emptyMoveMeaningPayloadJson(
      primaryEngineBacked = false,
      ledger = selectedLedger
    )
    assertEquals((withheld \ "idea_status").as[String], "unavailable")
    assertEquals((withheld \ "explanations").as[JsArray].value.toList, Nil)
    assertEquals(
      (withheld \ "idea_status_detail" \ "selected_cause_ids").as[List[String]],
      List(selected.causeEvidence.id)
    )

  test("empty idea status keeps engine availability separate from ledger abstention"):
    val ledger = CauseDispositionLedger.empty
    val engineBacked = RuntimeProtocol.emptyMoveMeaningPayloadJson(
      primaryEngineBacked = true,
      ledger = ledger
    )
    val unavailable = RuntimeProtocol.emptyMoveMeaningPayloadJson(
      primaryEngineBacked = false,
      ledger = ledger
    )

    assertEquals(
      (engineBacked \ "idea_status").as[String],
      "no_certified_differential_idea"
    )
    assertEquals((unavailable \ "idea_status").as[String], "unavailable")
    assertEquals(
      (engineBacked \ "idea_status_detail").as[JsObject],
      RuntimeProtocol.causeDispositionPublicJson(ledger)
    )
    assertEquals(
      (unavailable \ "idea_status_detail").as[JsObject],
      RuntimeProtocol.causeDispositionPublicJson(ledger)
    )

  test("proof segment public codec preserves only typed ordered move steps"):
    val segment = DirectCauseProofSegment(
      DirectCauseProofTerminalRelation.ProducesLineConsequence,
      List(
        DirectCauseProofStep(0, "e2e4", DirectCauseProofStepRole.RootAction),
        DirectCauseProofStep(1, "e7e5", DirectCauseProofStepRole.CausalLink),
        DirectCauseProofStep(2, "d1h5", DirectCauseProofStepRole.TerminalEvent)
      )
    )
    assertEquals(
      RuntimeProtocol.directCauseProofSegmentPublicJson(segment),
      Json.obj(
        "terminal_relation" -> "produces_line_consequence",
        "steps" -> Json.arr(
          Json.obj("ply_offset" -> 0, "move_uci" -> "e2e4", "role" -> "root_action"),
          Json.obj("ply_offset" -> 1, "move_uci" -> "e7e5", "role" -> "causal_link"),
          Json.obj("ply_offset" -> 2, "move_uci" -> "d1h5", "role" -> "terminal_event")
        )
      )
    )

  test("direct-effect admission exposes only fail-closed unresolved and restricted states"):
    assertEquals(
      RuntimeProtocol.directEffectAdmissionPublicJson(DirectEffectAdmission.Unresolved),
      Json.obj("status" -> "unresolved", "causal_signatures" -> Json.arr())
    )
    assertEquals(
      RuntimeProtocol.directEffectAdmissionPublicJson(
        DirectEffectAdmission.Restricted(Set("owned-b", "owned-a"))
      ),
      Json.obj(
        "status" -> "restricted",
        "causal_signatures" -> Json.arr("owned-a", "owned-b")
      )
    )
    assert(!DirectEffectAdmission.Unresolved.productionReady)
    assert(!DirectEffectAdmission.Restricted(Set.empty).productionReady)
    assert(DirectEffectAdmission.Restricted(Set("owned-a")).productionReady)

  test("material outcome projection keeps event salience separate from durable value"):
    val descriptor = RootOwnedEffectDescriptor(
      identity = RootOwnedEffectIdentity(
        RootOwnedEffectPrimitiveKind.LineEpisode,
        List("outcome:material-transfer"),
        Nil,
        Nil
      ),
      magnitude = DirectEffectMagnitudeKnowledge.Exact(
        DirectCauseImportanceMeasure.MaterialOutcome(
          durableNetCp = 400,
          onsetPlyOffset = 0
        )
      ),
      materialEventSalience = Some(RootOwnedMaterialEventSalience(
        moveUci = "d1d8",
        plyOffset = 0,
        capturedRole = EvidencePieceRole("queen"),
        square = EvidenceSquare("d8"),
        targetValueCp = 900
      ))
    )

    assertEquals(
      RuntimeProtocol.rootOwnedEffectDescriptorPublicJson(descriptor),
      Json.obj(
        "effect_scope" -> Json.obj(
          "primitive_kind" -> "line_episode",
          "target_signatures" -> Json.arr("outcome:material-transfer"),
          "plan_ids" -> Json.arr(),
          "strategic_axes" -> Json.arr()
        ),
        "magnitude_status" -> "exact",
        "measure" -> Json.obj(
          "kind" -> "material_outcome",
          "durable_net_cp" -> 400,
          "onset_ply_offset" -> 0
        ),
        "material_event_salience" -> Json.obj(
          "move_uci" -> "d1d8",
          "ply_offset" -> 0,
          "captured_role" -> "queen",
          "square" -> "d8",
          "target_value_cp" -> 900
        )
      )
    )

  test("importance projection preserves every typed profile relation and decision"):
    val relations = List(
      DirectCauseImportanceRelationDecision(
        "cause-a",
        "channel-a1",
        "cause-b",
        "channel-b1",
        DirectCauseImportanceRelation.Dominates,
        Some("material:materialgain")
      ),
      DirectCauseImportanceRelationDecision(
        "cause-a",
        "channel-a2",
        "cause-b",
        "channel-b2",
        DirectCauseImportanceRelation.Dominates,
        Some("material:materialgain")
      ),
      DirectCauseImportanceRelationDecision(
        "cause-a",
        "channel-a1",
        "cause-c",
        "channel-c1",
        DirectCauseImportanceRelation.Incomparable,
        None
      )
    )
    val decisions = List(
      DirectCauseImportanceDecision("cause-a", List("channel-a1", "channel-a2"), Nil, Nil, false),
      DirectCauseImportanceDecision("cause-b", List("channel-b1", "channel-b2"), Nil, List("cause-a"), true),
      DirectCauseImportanceDecision("cause-c", Nil, List("channel-c1"), Nil, false)
    )
    val json = RuntimeProtocol.directCauseImportancePublicJson(
      DirectCauseImportanceResolution(
        selectedCauseEvidenceIds = List("cause-a", "cause-b", "cause-c"),
        profiles = Nil,
        relations = relations,
        decisions = decisions
      )
    )
    val projectedRelations = (json \ "relations").as[JsArray].value.map(_.as[JsObject])

    assertEquals(
      (json \ "comparison_exposure_rank_meaning").as[String],
      "comparison_exposure_authority"
    )
    assert((json \ "priority_rank_meaning").toOption.isEmpty)
    assertEquals((json \ "selected_cause_ids").as[List[String]], List("cause-a", "cause-b", "cause-c"))
    assertEquals(projectedRelations.size, 3)
    assertEquals(
      projectedRelations.flatMap(item => (item \ "left_causal_signature").asOpt[String]).toSet,
      Set("channel-a1", "channel-a2")
    )
    assert(projectedRelations.exists(item =>
      (item \ "relation").asOpt[String].contains("incomparable") &&
        (item \ "domain").toOption.contains(JsNull)
    ))
    assertEquals((json \ "relation_summary" \ "comparable_profile_pairs").as[Int], 2)
    assertEquals((json \ "relation_summary" \ "incomparable_profile_pairs").as[Int], 1)
    assertEquals((json \ "decisions").as[JsArray].value.size, 3)

    val scope = RootOwnedEffectIdentity(
      RootOwnedEffectPrimitiveKind.PlanResult,
      List("plansubject:minority-attack", "square:b5"),
      List("minority-attack"),
      List(RootOwnedStrategicAxisIdentity(
        StrategicAxisKind.PlanCoherence,
        StrategicAxisPolarity.Gain,
        "minority-attack",
        Some(StrategicAxisComparisonOutcome.ReferenceOnly)
      ))
    )
    val scopeJson = RuntimeProtocol.rootOwnedEffectIdentityJson(scope)
    assertEquals((scopeJson \ "primitive_kind").as[String], "plan_result")
    assertEquals((scopeJson \ "plan_ids").as[List[String]], List("minority-attack"))
    assertEquals(
      (scopeJson \ "strategic_axes" \ 0 \ "label").as[String],
      "minority-attack"
    )

  test("v1 rejects unknown schema and oversized PV bundles"):
    assertEquals(RuntimeProtocol.evaluate(request("v2")).httpStatus, 400)
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
    val closedTrace = RuntimeProtocol.evaluateWithBoundary(
      request(body = baseInput + ("probeResults" -> Json.arr(result))),
      JudgmentBoundaryIntervention.identity
    )
    val closed = closedTrace.result
    assertEquals(closed.httpStatus, 200)
    assertEquals((closed.body \ "status").as[String], "ready")
    assertEquals((closed.body \ "availability" \ "reason").asOpt[String], None)
    assert((closed.body \ "move_review" \ "renderable").as[Boolean], closed.body)
    val verdict = (closed.body \ "move_review" \ "verdict").as[JsObject]
    val packet = closedTrace.boundaryExecution
      .flatMap(_.packet)
      .getOrElse(fail("expected a packet-backed exact verdict"))
    assertEquals(
      RuntimeProtocol.primaryEngineBackedVerdictPublicJson(packet),
      Some(verdict)
    )
    assertEquals((verdict \ "comparison_kind").as[String], "played_vs_best")
    assertEquals((verdict \ "mover").as[String], "white")
    assertEquals((verdict \ "played_move").as[String], "f3g5")
    assert((verdict \ "reference_move").asOpt[String].exists(_.nonEmpty), verdict)
    val delta = (verdict \ "candidate_win_percent_delta_for_mover").as[Double]
    val loss = (verdict \ "win_percent_loss_for_mover").as[Double]
    assertEquals(loss, (-delta).max(0.0))
    val classificationAuthority = RuntimeProtocol
      .primaryEngineBackedVerdictClassificationAuthorityJson(packet)
      .getOrElse(fail("expected central verdict classification authority"))
    assertEquals(
      (classificationAuthority \ "policy_version").as[String],
      RuntimeProtocol.VerdictClassificationPolicyVersion
    )
    assertEquals(
      (classificationAuthority \ "expected_verdict_code").as[String],
      (verdict \ "verdict_code").as[String]
    )
    assertEquals(
      (classificationAuthority \ "candidate_win_percent_delta_for_mover").as[Double],
      delta
    )
    assertEquals(
      (classificationAuthority \ "win_percent_loss_for_mover").as[Double],
      loss
    )
    assertEquals(
      (closed.body \ "move_review" \ "explanations").as[List[JsObject]],
      Nil
    )
    assertEquals(
      (closed.body \ "move_review" \ "idea_status").as[String],
      "no_certified_differential_idea"
    )
    val dispositionDetail =
      (closed.body \ "move_review" \ "idea_status_detail").as[JsObject]
    assertEquals(packet.causeDispositionLedger.selectedCauseEvidenceIds, Set.empty[String])
    assertEquals(
      dispositionDetail,
      RuntimeProtocol.causeDispositionPublicJson(packet.causeDispositionLedger)
    )
    val expectedAbstentionCodes =
      if packet.causeDispositionLedger.dispositions.isEmpty then
        List("no_relative_cause_generated")
      else
        packet.causeDispositionLedger.dispositions
          .map(disposition =>
            disposition.reason.toString
              .replaceAll("([a-z0-9])([A-Z])", "$1_$2")
              .toLowerCase
          )
          .distinct
          .sorted
    assertEquals(
      (dispositionDetail \ "abstention_codes").as[List[String]],
      expectedAbstentionCodes
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
