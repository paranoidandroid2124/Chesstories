package io.chesstory.evaluation.runtimeadapter

import chess.White
import io.chesstory.runtime.RuntimeProtocol
import lila.chessjudgment.analysis.assembly.{ ClaimRankingResult, RankedClaimDecision }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.EngineLine
import play.api.libs.json.{ JsArray, JsNull, JsObject, JsString, JsValue, Json }

class CauseAuditProjectionValidationTest extends munit.FunSuite:

  test("observation schema arguments are exact and fail closed"):
    assertEquals(
      CauseAuditAdapterCli.observationSchemaForArgs(Array.empty[String]),
      "chesstory.cause-audit-runtime-observation.v3"
    )
    assertEquals(
      CauseAuditAdapterCli.observationSchemaForArgs(Array("--observation-schema-version=3")),
      "chesstory.cause-audit-runtime-observation.v3"
    )
    assertEquals(
      CauseAuditAdapterCli.observationSchemaForArgs(Array("--historical-observation-schema-version=2")),
      "chesstory.cause-audit-runtime-observation.v2"
    )
    List(
      Array("--observation-schema-version=2"),
      Array("--observation-schema-version=3", "--historical-observation-schema-version=2"),
      Array("--observation-schema-version=3", "--observation-schema-version=3"),
      Array("--unknown")
    ).foreach { args =>
      intercept[IllegalArgumentException] {
        CauseAuditAdapterCli.observationSchemaForArgs(args)
      }
    }

  private val position = PositionNodeRef(
    "1r2k3/8/8/8/8/8/4Q3/4K3 w - - 0 1",
    0,
    Some(White)
  )
  private val reference = LineNodeRef("reference", "e2b5", 1, LineNodeRole.BestReference)
  private val played = LineNodeRef("played", "e2e3", 9, LineNodeRole.Played)
  private val comparisonRef = ref(
    "comparison",
    EvidenceProducer.RelativeMoveProducer,
    EvidenceLayer.CandidateComparison,
    Some(played),
    EvidenceConfidence.EngineBacked
  )
  private val proofRef = ref(
    "direct-relation",
    EvidenceProducer.TacticalRelationProducer,
    EvidenceLayer.Relation,
    Some(reference),
    EvidenceConfidence.LegalReplayVerified
  )
  private val wrapperRef = ref(
    "tactical-carrier",
    EvidenceProducer.TacticalMechanismProducer,
    EvidenceLayer.TacticalMechanism,
    Some(reference),
    EvidenceConfidence.LegalReplayVerified
  )
  private val causeRef = ref(
    "cause",
    EvidenceProducer.RelativeMoveProducer,
    EvidenceLayer.RelativeCause,
    Some(reference),
    EvidenceConfidence.EngineBacked
  )
  private val referenceNode = CandidateLineNode(
    reference,
    EngineLine(List(reference.rootMove), 900, depth = 18),
    ref(
      "reference-eval",
      EvidenceProducer.EngineEvalProducer,
      EvidenceLayer.Eval,
      Some(reference),
      EvidenceConfidence.EngineBacked
    )
  )
  private val playedNode = CandidateLineNode(
    played,
    EngineLine(List(played.rootMove), 0, depth = 18),
    ref(
      "played-eval",
      EvidenceProducer.EngineEvalProducer,
      EvidenceLayer.Eval,
      Some(played),
      EvidenceConfidence.EngineBacked
    )
  )
  private val comparison = CandidateComparisonFact(
    CandidateComparisonKind.PlayedVsBest,
    reference,
    played,
    EvalComparison.fromLines(White, referenceNode, playedNode)
  )
  private val unresolvedCause = RelativeCauseFact(
    RelativeCauseKind.WrongMoveOrder,
    comparisonRef,
    List(proofRef),
    RelativeCauseSourceSide.Reference,
    CauseAttribution(
      CauseAttributionKind.ReferenceCreatesResource,
      rootMoveMatched = true,
      directProofEligible = true
    ),
    Some(RelativeCauseProof(
      RelativeCauseProofSection(
        RelativeCauseProofRole.DirectProof,
        RelativeCauseProofStrength.Primary,
        List(proofRef)
      )
    ))
  )
  private val proof = RelationFactEvidence
    .from(
      RelationWitnessDetail.Zwischenzug(
        intermediateMove = reference.rootMove,
        expectedRecaptureSquare = EvidenceSquare("d5"),
        checkingPieceSquare = EvidenceSquare(reference.rootMove.slice(2, 4)),
        checkingPieceRole = EvidencePieceRole("queen"),
        checkedKingSquare = EvidenceSquare("e8"),
        threatType = RelationThreatSignal.Check
      ),
      List(reference.rootMove)
    )
    .getOrElse(fail("synthetic relation must be typed"))
  private val carrier = TacticalMechanismEvidence(
    TacticalMechanismKind.Tempo,
    Some(reference.rootMove),
    Some(reference),
    List(TacticalMechanismSignal(
      TacticalMechanismSignalKind.Relation,
      "zwischenzug",
      EvidenceLayer.Relation,
      Some(proofRef),
      Some(RelationFactKind.Zwischenzug)
    ))
  )
  private val preAdmissionGraph = List(
    EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
    EvidenceRecord(proofRef, proof)
  ).foldLeft(TypedEvidenceGraph.empty)((value, record) => value.add(record))
  private val admittedSignature = RelativeCauseConstructionAdmission
    .diagnosticRawDirectChannels(unresolvedCause, preAdmissionGraph)
    .headOption
    .map(_.causalSignature)
    .getOrElse(fail("synthetic Cause must own a pre-admission sentence-ready channel"))
  private val cause = unresolvedCause.copy(
    directEffectAdmission = DirectEffectAdmission.Restricted(Set(admittedSignature))
  )
  private val graph = preAdmissionGraph.add(
    EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause))
  )
  private val wrapperGraph = graph.add(
    EvidenceRecord(wrapperRef, carrier, parents = List(proofRef))
  )
  private val channel = RelativeCauseConstructionAdmission
    .admittedDirectChannels(cause, graph)
    .headOption
    .getOrElse(fail("synthetic Cause must own a sentence-ready channel"))
  private val payload = publicPayload(channel)

  test("a forged public causal signature fails selection parsing without hiding the emitted Cause ID"):
    val (validIds, validCount) = CauseAuditAdapterCli.projectedSelectionValidation(payload, graph)
    assertEquals(validIds, List(causeRef.id))
    assertEquals(validCount, 1)

    val forged = replaceString(payload, channel.causalSignature, "forged-signature")
    val (forgedIds, forgedCount) = CauseAuditAdapterCli.projectedSelectionValidation(forged, graph)
    assertEquals(forgedIds, validIds)
    assertEquals(forgedCount, 0)

  test("every raw public idea must parse exactly once"):
    assertEquals(
      CauseAuditAdapterCli.projectedIdeaParseValidation(payload, graph),
      (1, 1, true)
    )
    val explanation = (payload \ "explanations" \ 0).as[JsObject]
    val ideas = (explanation \ "ideas").as[JsArray]
    val malformed = payload + (
      "explanations" -> Json.arr(
        explanation + (
          "ideas" -> JsArray(
            ideas.value ++ Seq(
              Json.obj("host" -> Json.obj("claim_id" -> "missing-cause-id")),
              JsString("not-an-idea-object")
            )
          )
        )
      )
    )

    assertEquals(
      CauseAuditAdapterCli.projectedIdeaParseValidation(malformed, graph),
      (3, 1, false)
    )

  test("public idea items and units preserve exact membership and order"):
    assertEquals(
      CauseAuditAdapterCli.projectedIdeaUnitValidation(payload, graph),
      (1, 1, true, true)
    )
    val wrongRole = mutateIdeaItems(payload)(item =>
      item + ("item_role" -> JsString("forged_role"))
    )
    assertEquals(
      CauseAuditAdapterCli.projectedIdeaUnitValidation(wrongRole, graph),
      (1, 1, true, false)
    )
    assertEquals(
      CauseAuditAdapterCli.projectedIdeaParseValidation(wrongRole, graph),
      (1, 0, false)
    )
    val wrongMembership = mutateIdeaUnits(payload)(unit =>
      unit + ("member_cause_evidence_ids" -> Json.arr("forged-cause"))
    )
    assertEquals(
      CauseAuditAdapterCli.projectedIdeaUnitValidation(wrongMembership, graph),
      (1, 0, false, false)
    )

  test("public proof segment is required and must equal the selected channel's owned proof"):
    val (validIds, validCount) = CauseAuditAdapterCli.projectedSelectionValidation(payload, graph)
    assertEquals(validCount, 1)

    val missing = mutateChannels(payload)(_ - "proof_segment")
    val (missingIds, missingCount) = CauseAuditAdapterCli.projectedSelectionValidation(missing, graph)
    assertEquals(missingIds, validIds)
    assertEquals(missingCount, 0)

    val forged = replaceString(payload, "instantiates_relation", "creates_threat")
    val (forgedIds, forgedCount) = CauseAuditAdapterCli.projectedSelectionValidation(forged, graph)
    assertEquals(forgedIds, validIds)
    assertEquals(forgedCount, 0)

  test("public admission, descriptor, and ambiguity state are exact typed copies"):
    val (_, validCount) = CauseAuditAdapterCli.projectedSelectionValidation(payload, graph)
    assertEquals(validCount, 1)

    val missingAdmission = mutateCause(payload)(_ - "direct_effect_admission")
    assertEquals(CauseAuditAdapterCli.projectedSelectionValidation(missingAdmission, graph)._2, 0)

    val forgedDescriptor = mutateChannels(payload)(channel =>
      channel + ("descriptor_ambiguous" -> play.api.libs.json.JsBoolean(true))
    )
    assertEquals(CauseAuditAdapterCli.projectedSelectionValidation(forgedDescriptor, graph)._2, 0)

    val missingMeasuredEffect = mutateChannels(payload)(_ - "importance_effect")
    assertEquals(CauseAuditAdapterCli.projectedSelectionValidation(missingMeasuredEffect, graph)._2, 0)
    val forgedMeasuredEffect = mutateChannels(payload)(channel =>
      channel + ("importance_effect" -> Json.obj("domain" -> "forged"))
    )
    assertEquals(CauseAuditAdapterCli.projectedSelectionValidation(forgedMeasuredEffect, graph)._2, 0)

  test("registered carrier metadata is part of exact public P preservation"):
    val (validIds, validCount) = CauseAuditAdapterCli.projectedSelectionValidation(payload, graph)
    assertEquals(validCount, 1)
    val mutations = List[(String, JsObject => JsObject)](
      "producer" -> (carrier => carrier + ("producer" -> JsString("forged_producer"))),
      "layer" -> (carrier => carrier + ("layer" -> JsString("forged_layer"))),
      "scope" -> (carrier => carrier + ("scope" -> JsString("forged_scope"))),
      "semantic line" -> (carrier =>
        carrier + ("line" -> ((carrier \ "line").as[JsObject] +
          ("root_move" -> JsString("e2e4"))))
      )
    )
    mutations.foreach { case (label, mutation) =>
      val forged = mutateCarriers(payload, mutation)
      val (forgedIds, forgedCount) = CauseAuditAdapterCli.projectedSelectionValidation(forged, graph)
      assertEquals(forgedIds, validIds, clues(label))
      assertEquals(forgedCount, 0, clues(label))
    }

  test("v3 mode adds versioned R fields without changing v2"):
    val request = Json.stringify(Json.obj(
      "schema_version" -> RuntimeProtocol.RequestSchema,
      "request_id" -> "cause-audit-adapter-v3-test",
      "input" -> Json.obj(
        "fen" -> chess.variant.Standard.initialFen.value,
        "playedMoveUci" -> "d2d4",
        "variations" -> Json.arr(
          Json.obj(
            "moves" -> Json.arr("e2e4", "e7e5", "g1f3"),
            "scoreCp" -> 900,
            "depth" -> 18
          ),
          Json.obj(
            "moves" -> Json.arr("d2d4", "d7d5", "g1f3"),
            "scoreCp" -> 0,
            "depth" -> 18
          ),
          Json.obj(
            "moves" -> Json.arr("g1f3", "d7d5", "d2d4"),
            "scoreCp" -> -900,
            "depth" -> 18
          )
        ),
        "ply" -> 0,
        "movePrefixUci" -> Json.arr()
      )
    ))
    val v2 = CauseAuditAdapterCli.observeHistoricalV2(request)
    val v3 = CauseAuditAdapterCli.observeV3(request)

    assertEquals((v2 \ "schema_version").as[String], "chesstory.cause-audit-runtime-observation.v2")
    assertEquals((v3 \ "schema_version").as[String], "chesstory.cause-audit-runtime-observation.v3")
    List(v2, v3).foreach { observation =>
      assertEquals(
        (observation \ "request_sha256").as[String],
        NativeTreeEncoder.sha256Utf8(request)
      )
      assertEquals(
        (observation \ "hash_contract").as[String],
        NativeTreeEncoder.HashContract
      )
    }
    assert((v2 \ "r_native_cause_selections").toOption.isEmpty)
    assert((v2 \ "importance").toOption.isEmpty)
    assert((v2 \ "idea_units").toOption.isEmpty)
    assert((v2 \ "idea_importance").toOption.isEmpty)
    assert((v2 \ "cause_disposition_ledger").toOption.isEmpty)
    assert((v2 \ "comparison_endpoint_evidence_snapshots").toOption.isEmpty)
    assert((v2 \ "verdict").toOption.isEmpty)
    assert((v2 \ "public_response" \ "idea_status").toOption.isEmpty)
    assert((v2 \ "public_response" \ "primary_engine_backed").toOption.isEmpty)
    assert((v3 \ "public_response" \ "idea_status").asOpt[String].nonEmpty)
    assert((v3 \ "public_response" \ "primary_engine_backed").asOpt[Boolean].nonEmpty)
    assert((v3 \ "public_response" \ "idea_status_detail").asOpt[JsObject].nonEmpty)
    val endpointSnapshots =
      (v3 \ "comparison_endpoint_evidence_snapshots").as[List[JsObject]]
    assert(endpointSnapshots.nonEmpty)
    val comparisonEvidenceIds = endpointSnapshots.map(snapshot =>
      (snapshot \ "comparison_evidence" \ "id").as[String]
    )
    assertEquals(comparisonEvidenceIds.distinct.size, comparisonEvidenceIds.size)
    val generatedCauseComparisonIds = (v3 \ "causes").as[List[JsObject]].map(cause =>
      (cause \ "c" \ "comparison_evidence" \ "id").as[String]
    ).distinct
    assert(generatedCauseComparisonIds.forall(comparisonEvidenceIds.contains))
    endpointSnapshots.foreach { snapshot =>
      List("reference", "candidate").foreach { sideName =>
        val side = (snapshot \ sideName).as[JsObject]
        assertEquals((side \ "source_side").as[String], sideName)
        (side \ "witnesses").as[List[JsObject]].foreach { witness =>
          assertEquals((witness \ "source_side").as[String], sideName)
          List(
            "carrier",
            "provenance",
            "carrier_ancestor_source_ids",
            "primitive_proof_source",
            "actor",
            "target",
            "mechanism",
            "consequence",
            "proof_segment",
            "effect_descriptor"
          ).foreach(field => assert((witness \ field).toOption.nonEmpty))
        }
      }
    }
    val verdictObservation = (v3 \ "verdict").as[JsObject]
    val packetVerdict = (verdictObservation \ "packet_canonical").as[JsObject]
    val selectedVerdict = (verdictObservation \ "selected_projection").as[JsObject]
    val publicVerdict = (verdictObservation \ "final_public_response").as[JsObject]
    val classificationAuthority =
      (verdictObservation \ "classification_authority").as[JsObject]
    assertEquals(
      NativeTreeEncoder.sha256Canonical(packetVerdict),
      NativeTreeEncoder.sha256Canonical(selectedVerdict)
    )
    assertEquals(
      NativeTreeEncoder.sha256Canonical(selectedVerdict),
      NativeTreeEncoder.sha256Canonical(publicVerdict)
    )
    assertEquals((packetVerdict \ "comparison_kind").as[String], "played_vs_best")
    assertEquals((packetVerdict \ "mover").as[String], "white")
    assertEquals((packetVerdict \ "played_move").as[String], "d2d4")
    assert((packetVerdict \ "reference_move").asOpt[String].exists(_.nonEmpty))
    val delta = (packetVerdict \ "candidate_win_percent_delta_for_mover").as[Double]
    val loss = (packetVerdict \ "win_percent_loss_for_mover").as[Double]
    assertEquals(loss, (-delta).max(0.0))
    assertEquals(
      (classificationAuthority \ "policy_version").as[String],
      RuntimeProtocol.VerdictClassificationPolicyVersion
    )
    assertEquals(
      (classificationAuthority \ "expected_verdict_code").as[String],
      (packetVerdict \ "verdict_code").as[String]
    )
    assertEquals(
      (classificationAuthority \ "candidate_win_percent_delta_for_mover").as[Double],
      delta
    )
    assertEquals(
      (classificationAuthority \ "win_percent_loss_for_mover").as[Double],
      loss
    )
    assert(
      (v2 \ "causes").as[List[JsObject]].forall(cause =>
        (cause \ "r" \ "native_selection").toOption.isEmpty
      )
    )

    val topLevelSelections = (v3 \ "r_native_cause_selections").as[List[JsObject]]
    assert(topLevelSelections.forall(selection =>
      (selection \ "comparison_semantic_key").asOpt[String].exists(_.nonEmpty)
    ))
    val perCauseSelections = (v3 \ "causes").as[List[JsObject]].flatMap(cause =>
      (cause \ "r" \ "native_selection").asOpt[JsObject]
    )
    assertEquals(
      topLevelSelections.map(NativeTreeEncoder.sha256Canonical).sorted,
      perCauseSelections.map(NativeTreeEncoder.sha256Canonical).sorted
    )
    val ideaUnits = (v3 \ "idea_units").as[JsObject]
    val rIdeaUnits = (ideaUnits \ "r_native").as[JsArray]
    val packetIdeaUnits = (ideaUnits \ "packet_native").as[JsArray]
    val publicIdeaUnits = (ideaUnits \ "public_projection").as[JsArray]
    assertEquals(
      NativeTreeEncoder.sha256Canonical(rIdeaUnits),
      NativeTreeEncoder.sha256Canonical(packetIdeaUnits)
    )
    assertEquals(
      NativeTreeEncoder.sha256Canonical(packetIdeaUnits),
      NativeTreeEncoder.sha256Canonical(publicIdeaUnits)
    )
    assertEquals((ideaUnits \ "public_idea_units_parse_closed").as[Boolean], true)
    assertEquals((ideaUnits \ "public_item_membership_closed").as[Boolean], true)
    val itemLinks = (ideaUnits \ "public_item_links").as[List[JsObject]]
    assertEquals(
      itemLinks.map(item => (item \ "cause_evidence_id").as[String]),
      topLevelSelections
        .sortBy(item => (item \ "selection_order").as[Int])
        .map(item => (item \ "cause_evidence_id").as[String])
    )
    assert(itemLinks.forall(item => (item \ "item_role").as[String] == "cause_facet"))
    val ideaImportance = (v3 \ "idea_importance").as[JsObject]
    val nativeIdeaImportance = (ideaImportance \ "r_native").as[JsObject]
    val packetIdeaImportance = (ideaImportance \ "packet_native").as[JsObject]
    assertEquals(
      NativeTreeEncoder.sha256Canonical(nativeIdeaImportance),
      NativeTreeEncoder.sha256Canonical(packetIdeaImportance)
    )
    if rIdeaUnits.value.nonEmpty then
      val packetIdeaProjection = (ideaImportance \ "packet_public_projection").as[JsObject]
      val publicIdeaProjection = (ideaImportance \ "public_projection").as[JsObject]
      assertEquals(
        NativeTreeEncoder.sha256Canonical(packetIdeaProjection),
        NativeTreeEncoder.sha256Canonical(publicIdeaProjection)
      )
      assertEquals(
        (nativeIdeaImportance \ "selected_cause_ids").as[List[String]],
        rIdeaUnits.value.map(unit => (unit \ "lead_cause_evidence_id").as[String]).toList
      )
    val disposition = (v3 \ "cause_disposition_ledger").as[JsObject]
    val rDisposition = (disposition \ "r_native").as[JsArray]
    val packetDisposition = (disposition \ "packet_native").as[JsArray]
    assertEquals(
      NativeTreeEncoder.sha256Canonical(rDisposition),
      NativeTreeEncoder.sha256Canonical(packetDisposition)
    )
    val rDispositionSummary = (disposition \ "r_public_summary").as[JsObject]
    val packetDispositionSummary = (disposition \ "packet_public_summary").as[JsObject]
    val selectedDispositionSummary = (disposition \ "selected_projection").as[JsObject]
    val publicDispositionSummary = (disposition \ "final_public_response").as[JsObject]
    assertEquals(
      NativeTreeEncoder.sha256Canonical(rDispositionSummary),
      NativeTreeEncoder.sha256Canonical(packetDispositionSummary)
    )
    assertEquals(
      NativeTreeEncoder.sha256Canonical(packetDispositionSummary),
      NativeTreeEncoder.sha256Canonical(selectedDispositionSummary)
    )
    assertEquals(
      NativeTreeEncoder.sha256Canonical(selectedDispositionSummary),
      NativeTreeEncoder.sha256Canonical(publicDispositionSummary)
    )
    assertEquals(
      NativeTreeEncoder.sha256Canonical(publicDispositionSummary),
      NativeTreeEncoder.sha256Canonical(
        (v3 \ "public_response" \ "idea_status_detail").as[JsObject]
      )
    )
    val importance = (v3 \ "importance").as[JsObject]
    val nativeImportance = (importance \ "r_native").as[JsObject]
    assertEquals(
      (nativeImportance \ "relation_policy_version").as[String],
      RuntimeProtocol.DirectCauseImportanceRelationPolicyVersion
    )
    assertEquals(
      (nativeImportance \ "ordering_policy_version").as[String],
      RuntimeProtocol.PlayerFacingIdeaOrderingPolicyVersion
    )
    (nativeImportance \ "profiles").as[List[JsObject]].foreach { profile =>
      assert((profile \ "universe" \ "root_board_state").asOpt[String].exists(_.nonEmpty))
      assert((profile \ "universe" \ "impact").asOpt[String].exists(_.nonEmpty))
      assert((profile \ "domain_kind").asOpt[String].nonEmpty)
    }
    assertEquals(
      (nativeImportance \ "selected_cause_ids").as[List[String]].sorted,
      topLevelSelections.map(item => (item \ "cause_evidence_id").as[String]).sorted
    )
    val rProjection = (importance \ "r_public_projection").as[JsObject]
    if topLevelSelections.nonEmpty then
      val packetNative = (importance \ "packet_native").as[JsObject]
      val packetProjection = (importance \ "packet_public_projection").as[JsObject]
      val publicProjection = (importance \ "public_projection").as[JsObject]
      assertEquals(
        NativeTreeEncoder.sha256Canonical(nativeImportance),
        NativeTreeEncoder.sha256Canonical(packetNative)
      )
      assertEquals(
        NativeTreeEncoder.sha256Canonical(rProjection),
        NativeTreeEncoder.sha256Canonical(packetProjection)
      )
      assertEquals(
        NativeTreeEncoder.sha256Canonical(packetProjection),
        NativeTreeEncoder.sha256Canonical(publicProjection)
      )
      assertEquals(
        (publicProjection \ "selected_cause_ids").as[List[String]].sorted,
        topLevelSelections.map(item => (item \ "cause_evidence_id").as[String]).sorted
      )
      (publicProjection \ "relations").as[List[JsObject]].foreach { relation =>
        assert((relation \ "left_causal_signature").asOpt[String].nonEmpty)
        assert((relation \ "right_causal_signature").asOpt[String].nonEmpty)
      }
      assertEquals(
        (publicProjection \ "decisions").as[List[JsObject]]
          .map(item => (item \ "cause_evidence_id").as[String]).sorted,
        topLevelSelections.map(item => (item \ "cause_evidence_id").as[String]).sorted
      )
    else
      assertEquals((importance \ "public_projection").toOption, Some(JsNull))
    assertEquals(
      (rProjection \ "comparison_exposure_rank_meaning").as[String],
      "comparison_exposure_authority"
    )
    assertEquals(
      (rProjection \ "relation_policy_version").as[String],
      RuntimeProtocol.DirectCauseImportanceRelationPolicyVersion
    )
    assertEquals(
      (rProjection \ "ordering_policy_version").as[String],
      RuntimeProtocol.PlayerFacingIdeaOrderingPolicyVersion
    )

  test("v3 unavailable observation keeps all exact verdict copies explicitly null"):
    val unavailable = CauseAuditAdapterCli.observeV3("{}")
    val verdict = (unavailable \ "verdict").as[JsObject]
    assertEquals((verdict \ "packet_canonical").toOption, Some(JsNull))
    assertEquals((verdict \ "selected_projection").toOption, Some(JsNull))
    assertEquals((verdict \ "final_public_response").toOption, Some(JsNull))
    assertEquals((verdict \ "classification_authority").toOption, Some(JsNull))

  test("v3 projects the non-empty R authority and typed C channel from native values"):
    val hostClaim = JudgmentClaim(
      "synthetic-host",
      ClaimFamily.Tactical,
      ClaimSubject.ReferenceMove,
      position,
      Some(reference),
      Some(reference.rootMove),
      List(causeRef),
      EvidenceScope.BestLine,
      EvidenceConfidence.EngineBacked
    )
    val exposure = PlayerFacingCauseExposurePipeline.resolve(
      List(hostClaim),
      graph,
      Set(played.rootMove)
    )
    assert(exposure.selections.nonEmpty, "synthetic Cause must survive native R selection")
    val ranking = ClaimRankingResult(
      ranked = List(RankedClaimDecision(
        claim = hostClaim,
        exposureTier = PlayerFacingClaimTier.Primary,
        playerFacingCauseSelections = exposure.selections
      )),
      deduplicationTrace = Nil,
      causeExposureResolution = exposure,
      causeDispositionLedger = CauseDispositionLedger(Nil),
      causeDominanceDecisions = exposure.dominanceDecisions,
      crossComparisonExposureDecisions = exposure.crossDecisions,
      onlyMoveConstraintResolutions = exposure.onlyMoveConstraintResolutions
    )

    val selections = CauseAuditAdapterCli.projectedRSelectionDocuments(ranking, graph)
    assertEquals(selections.size, 1)
    assertEquals((selections.head \ "cause_evidence_id").as[String], causeRef.id)
    assertEquals(
      (selections.head \ "comparison_exposure_rank").as[Int],
      exposure.selections.head.comparisonExposureRank
    )
    assertEquals((selections.head \ "selection_order").as[Int], exposure.selections.head.selectionOrder)
    val rChannel = (selections.head \ "channels").as[List[JsObject]].head
    assertEquals(
      (rChannel \ "line").as[JsObject].keys,
      Set("line_id", "role", "rank", "root_move")
    )
    assert((rChannel \ "horizon").toOption.nonEmpty)
    assertEquals(
      (rChannel \ "proof_segment" \ "terminal_relation").as[String],
      "instantiates_relation"
    )
    assert((rChannel \ "importance_effect").toOption.nonEmpty)

    val wrappedChannel = channel.copy(binding = channel.binding.copy(
      source = wrapperRef,
      provenance = List(proofRef)
    ))
    val v2Channel = CauseAuditAdapterCli.projectedCOwnedChannelDocument(wrappedChannel, 2, wrapperGraph)
    val v3Channel = CauseAuditAdapterCli.projectedCOwnedChannelDocument(wrappedChannel, 3, wrapperGraph)
    assert((v2Channel \ "line").toOption.isEmpty)
    assert((v2Channel \ "horizon").toOption.isEmpty)
    assert((v2Channel \ "proof_segment").toOption.isEmpty)
    assert((v2Channel \ "effect_descriptor").toOption.isEmpty)
    assert((v2Channel \ "importance_effect").toOption.isEmpty)
    assert((v2Channel \ "carrier").toOption.isEmpty)
    assert((v2Channel \ "provenance").toOption.isEmpty)
    assert((v2Channel \ "carrier_ancestor_source_ids").toOption.isEmpty)
    assert((v2Channel \ "primitive_proof_source").toOption.isEmpty)
    assert((v2Channel \ "evidence_identity_sha256").toOption.isEmpty)
    assertEquals(
      (v3Channel \ "line").as[JsObject].keys,
      Set("line_id", "role", "rank", "root_move")
    )
    assert((v3Channel \ "horizon").toOption.nonEmpty)
    assertEquals(
      (v3Channel \ "proof_segment" \ "terminal_relation").as[String],
      "instantiates_relation"
    )
    assert((v3Channel \ "effect_descriptor" \ "effect_scope").asOpt[JsObject].nonEmpty)
    assert((v3Channel \ "importance_effect").toOption.nonEmpty)
    val carrierJson = (v3Channel \ "carrier").as[JsObject]
    assertEquals(
      carrierJson.keys,
      Set(
        "id",
        "producer",
        "layer",
        "scope",
        "confidence",
        "line",
        "record_registered",
        "record_payload_type",
        "record_sha256"
      )
    )
    assertEquals((carrierJson \ "record_registered").as[Boolean], true)
    assertEquals(
      (carrierJson \ "record_payload_type").as[String],
      "TacticalMechanismEvidence"
    )
    assert((carrierJson \ "record_sha256").as[String].matches("[0-9a-f]{64}"))
    val provenanceJson = (v3Channel \ "provenance").as[List[JsObject]]
    assertEquals(provenanceJson.size, 1)
    assertEquals(provenanceJson.head.keys, carrierJson.keys)
    assertEquals((provenanceJson.head \ "id").as[String], proofRef.id)
    assertEquals((provenanceJson.head \ "record_registered").as[Boolean], true)
    assertEquals(
      (provenanceJson.head \ "record_payload_type").as[String],
      "RelationFactEvidence"
    )
    assert((provenanceJson.head \ "record_sha256").as[String].matches("[0-9a-f]{64}"))
    assertEquals(
      (v3Channel \ "carrier_ancestor_source_ids").as[List[String]],
      List(proofRef.id)
    )
    assertEquals(
      (v3Channel \ "primitive_proof_source").as[JsObject],
      provenanceJson.head
    )
    assertEquals(
      (v3Channel \ "evidence_identity_sha256").as[String],
      NativeTreeEncoder.sha256Canonical(Json.obj(
        "carrier" -> carrierJson,
        "provenance" -> provenanceJson,
        "primitive_proof_source" -> provenanceJson.head
      ))
    )
    assertEquals((v3Channel \ "importance_descriptor_ambiguous").as[Boolean], false)
    assertEquals((v3Channel \ "proof_segment_ambiguous").as[Boolean], false)

  private def publicPayload(channel: DirectCauseChannel): JsObject =
    val binding = channel.binding
    Json.obj(
      "explanations" -> Json.arr(
        Json.obj(
          "ideas" -> Json.arr(
            Json.obj(
              "cause_evidence_id" -> causeRef.id,
              "item_role" -> "cause_facet",
              "idea_unit_id" -> causeRef.id,
              "comparison_exposure_rank" -> 0,
              "selection_order" -> 0,
              "host" -> Json.obj(
                "claim_id" -> "synthetic-host",
                "family" -> "tactical",
                "tier" -> "primary"
              ),
              "cause" -> Json.obj(
                "kind" -> code(cause.kind),
                "effect_mode" -> "alternative_resource",
                "exposure" -> "primary",
                "source_side" -> code(cause.sourceSide),
                "direct_effect_admission" -> RuntimeProtocol.directEffectAdmissionPublicJson(
                  cause.directEffectAdmission
                ),
                "attribution" -> Json.obj(
                  "kind" -> code(cause.attribution.kind),
                  "root_move_matched" -> cause.attribution.rootMoveMatched,
                  "direct_proof_eligible" -> cause.attribution.directProofEligible
                )
              ),
              "comparison" -> Json.obj(
                "evidence_id" -> comparisonRef.id,
                "kind" -> code(comparison.kind),
                "reference" -> lineJson(reference),
                "candidate" -> lineJson(played),
                "event" -> lineJson(reference),
                "compared_move" -> played.rootMove,
                "verdict" -> code(comparison.comparison.verdict),
                "mover" -> "white",
                "delta" -> Json.obj(
                  "candidate_win_percent_delta_for_mover" -> comparison.comparison.candidateWinPercentDeltaForMover,
                  "win_percent_loss_for_mover" -> comparison.comparison.winPercentLossForMover
                )
              ),
              "channels" -> Json.arr(channelJson(channel)),
              "only_move" -> JsNull
            )
          ),
          "idea_units" -> Json.arr(
            Json.obj(
              "idea_id" -> causeRef.id,
              "kind" -> "single_cause",
              "lead_cause_evidence_id" -> causeRef.id,
              "member_cause_evidence_ids" -> Json.arr(causeRef.id),
              "importance_layer" -> 0,
              "priority_status" -> "unique_top",
              "serialization_order" -> 0
            )
          )
        )
      )
    )

  private def channelJson(channel: DirectCauseChannel): JsObject =
    val binding = channel.binding
    val rootMove = binding.line.map(_.rootMove).getOrElse("")
    def actorKey(kind: EvidenceObjectKind): Option[String] =
      binding.actor.find(_.kind == kind).map(_.key.toLowerCase)
    Json.obj(
      "causal_signature" -> channel.causalSignature,
      "direct_change" -> code(channel.directChange),
      "played_change" -> "missed",
      "carrier" -> evidenceRefJson(binding.source),
      "provenance" -> binding.provenance.map(evidenceRefJson),
      "actor" -> Json.obj(
        "move" -> rootMove,
        "side" -> actorKey(EvidenceObjectKind.Side),
        "piece" -> actorKey(EvidenceObjectKind.Piece),
        "from" -> rootMove.take(2),
        "to" -> rootMove.slice(2, 4)
      ),
      "targets" -> objectsJson(binding.target),
      "mechanisms" -> objectsJson(binding.mechanism),
      "consequences" -> objectsJson(binding.consequence),
      "witnesses" -> objectsJson(binding.witness),
      "line" -> binding.line.map(lineJson),
      "horizon" -> binding.horizon,
      "proof_segment" -> channel.proofSegment.map(RuntimeProtocol.directCauseProofSegmentPublicJson),
      "effect_descriptor" -> channel.rootOwnedEffectDescriptor.map(
        RuntimeProtocol.rootOwnedEffectDescriptorPublicJson
      ),
      "importance_effect" -> RuntimeProtocol.directCauseMeasuredEffectPublicJson(channel),
      "descriptor_ambiguous" -> channel.importanceDescriptorAmbiguous,
      "proof_segment_ambiguous" -> channel.proofSegmentAmbiguous
    )

  private def evidenceRefJson(value: EvidenceRef): JsObject =
    Json.obj(
      "id" -> value.id,
      "producer" -> code(value.producer),
      "layer" -> code(value.layer),
      "scope" -> code(value.scope),
      "line" -> value.line.map(lineJson)
    )

  private def lineJson(value: LineNodeRef): JsObject =
    Json.obj(
      "id" -> value.id,
      "root_move" -> value.rootMove,
      "role" -> code(value.role),
      "rank" -> value.rank
    )

  private def objectsJson(values: List[ConcreteChessObject]): List[JsObject] =
    values.distinctBy(_.signaturePart).sortBy(_.signaturePart).map(value =>
      Json.obj("kind" -> code(value.kind), "key" -> value.key)
    )

  private def ref(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      line: Option[LineNodeRef],
      confidence: EvidenceConfidence
  ): EvidenceRef =
    EvidenceRef(
      id,
      producer,
      layer,
      position,
      line,
      line.map(_.role.scope).getOrElse(EvidenceScope.Counterfactual),
      confidence
    )

  private def replaceString(payload: JsObject, before: String, after: String): JsObject =
    def replace(value: JsValue): JsValue =
      value match
        case JsString(text) if text == before => JsString(after)
        case objectValue: JsObject => JsObject(objectValue.fields.map { case (key, item) => key -> replace(item) })
        case array: play.api.libs.json.JsArray => play.api.libs.json.JsArray(array.value.map(replace))
        case other => other
    replace(payload).as[JsObject]

  private def mutateCarriers(payload: JsObject, mutation: JsObject => JsObject): JsObject =
    def mutate(value: JsValue): JsValue =
      value match
        case objectValue: JsObject =>
          JsObject(objectValue.fields.map {
            case ("carrier", carrier: JsObject) => "carrier" -> mutation(carrier)
            case (key, item)                    => key -> mutate(item)
          })
        case array: play.api.libs.json.JsArray => play.api.libs.json.JsArray(array.value.map(mutate))
        case other => other
    mutate(payload).as[JsObject]

  private def mutateChannels(payload: JsObject)(mutation: JsObject => JsObject): JsObject =
    def mutate(value: JsValue): JsValue =
      value match
        case objectValue: JsObject if objectValue.keys.contains("causal_signature") &&
            objectValue.keys.contains("direct_change") =>
          mutation(objectValue)
        case objectValue: JsObject =>
          JsObject(objectValue.fields.map { case (key, item) => key -> mutate(item) })
        case array: play.api.libs.json.JsArray =>
          play.api.libs.json.JsArray(array.value.map(mutate))
        case other => other
    mutate(payload).as[JsObject]

  private def mutateCause(payload: JsObject)(mutation: JsObject => JsObject): JsObject =
    def mutate(value: JsValue): JsValue =
      value match
        case objectValue: JsObject if objectValue.keys.contains("effect_mode") &&
            objectValue.keys.contains("source_side") =>
          mutation(objectValue)
        case objectValue: JsObject =>
          JsObject(objectValue.fields.map { case (key, item) => key -> mutate(item) })
        case array: play.api.libs.json.JsArray =>
          play.api.libs.json.JsArray(array.value.map(mutate))
        case other => other
    mutate(payload).as[JsObject]

  private def mutateIdeaItems(payload: JsObject)(mutation: JsObject => JsObject): JsObject =
    def mutate(value: JsValue): JsValue =
      value match
        case objectValue: JsObject if objectValue.keys.contains("cause_evidence_id") &&
            objectValue.keys.contains("item_role") && objectValue.keys.contains("channels") =>
          mutation(objectValue)
        case objectValue: JsObject =>
          JsObject(objectValue.fields.map { case (key, item) => key -> mutate(item) })
        case array: play.api.libs.json.JsArray =>
          play.api.libs.json.JsArray(array.value.map(mutate))
        case other => other
    mutate(payload).as[JsObject]

  private def mutateIdeaUnits(payload: JsObject)(mutation: JsObject => JsObject): JsObject =
    def mutate(value: JsValue): JsValue =
      value match
        case objectValue: JsObject if objectValue.keys.contains("idea_id") &&
            objectValue.keys.contains("member_cause_evidence_ids") =>
          mutation(objectValue)
        case objectValue: JsObject =>
          JsObject(objectValue.fields.map { case (key, item) => key -> mutate(item) })
        case array: play.api.libs.json.JsArray =>
          play.api.libs.json.JsArray(array.value.map(mutate))
        case other => other
    mutate(payload).as[JsObject]

  private def code(value: Any): String =
    value.toString.replaceAll("([a-z0-9])([A-Z])", "$1_$2").toLowerCase
