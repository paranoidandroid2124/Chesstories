package io.chesstory.evaluation.runtimeadapter

import chess.White
import io.chesstory.runtime.RuntimeProtocol
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.EngineLine
import play.api.libs.json.{ JsObject, Json }

class CauseAuditProjectionValidationTest extends munit.FunSuite:

  test("observation schema selection preserves active v3 and historical v2"):
    assertEquals(
      CauseAuditAdapterCli.observationSchemaForArgs(Array.empty[String]),
      "chesstory.cause-audit-runtime-observation.v3"
    )
    assertEquals(
      CauseAuditAdapterCli.observationSchemaForArgs(Array("--historical-observation-schema-version=2")),
      "chesstory.cause-audit-runtime-observation.v2"
    )
    List(
      Array("--observation-schema-version=2"),
      Array("--observation-schema-version=3", "--historical-observation-schema-version=2"),
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

  test("v3 projects neutral endpoint evidence while historical v2 stays unchanged"):
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
    assert((v2 \ "comparison_endpoint_evidence_snapshots").toOption.isEmpty)

    val endpointSnapshots =
      (v3 \ "comparison_endpoint_evidence_snapshots").as[List[JsObject]]
    assert(endpointSnapshots.nonEmpty)
    val comparisonEvidenceIds = endpointSnapshots.map(snapshot =>
      (snapshot \ "comparison_evidence" \ "id").as[String]
    )
    assertEquals(comparisonEvidenceIds.distinct.size, comparisonEvidenceIds.size)
    val generatedCauseComparisonIds = (v3 \ "causes").as[List[JsObject]].flatMap(cause =>
      (cause \ "c" \ "comparison_evidence" \ "id").asOpt[String]
    ).toSet
    assert(generatedCauseComparisonIds.subsetOf(comparisonEvidenceIds.toSet))

    endpointSnapshots.foreach { snapshot =>
      List("reference", "candidate").foreach { sideName =>
        val side = (snapshot \ sideName).as[JsObject]
        assertEquals((side \ "source_side").as[String], sideName)
        assert((side \ "line" \ "root_move").asOpt[String].exists(_.nonEmpty), side)
        (side \ "witnesses").as[List[JsObject]].foreach { witness =>
          assertEquals((witness \ "source_side").as[String], sideName)
          assert(
            Set(
              "carrier",
              "provenance",
              "primitive_proof_source",
              "actor",
              "target",
              "mechanism",
              "consequence",
              "proof_segment",
              "effect_descriptor"
            ).subsetOf(witness.keys),
            witness
          )
          assert(
            (witness \ "effect_descriptor" \ "effect_scope" \ "primitive_kind")
              .asOpt[String]
              .exists(_.nonEmpty),
            witness
          )
          assert(
            (witness \ "effect_descriptor" \ "effect_scope" \ "strategic_axes")
              .asOpt[List[JsObject]]
              .nonEmpty,
            witness
          )
        }
      }
    }

  test("v3 owned-channel projection exposes traceable evidence without reshaping v2"):
    val wrappedChannel = channel.copy(binding = channel.binding.copy(
      source = wrapperRef,
      provenance = List(proofRef)
    ))
    val v2Channel = CauseAuditAdapterCli.projectedCOwnedChannelDocument(wrappedChannel, 2, wrapperGraph)
    val v3Channel = CauseAuditAdapterCli.projectedCOwnedChannelDocument(wrappedChannel, 3, wrapperGraph)
    val v3EvidenceFields = Set(
      "line",
      "horizon",
      "proof_segment",
      "effect_descriptor",
      "importance_effect",
      "carrier",
      "provenance",
      "primitive_proof_source",
      "evidence_identity_sha256"
    )

    assert(v3EvidenceFields.intersect(v2Channel.keys).isEmpty, v2Channel)
    assert(v3EvidenceFields.subsetOf(v3Channel.keys), v3Channel)
    assert((v3Channel \ "carrier" \ "record_registered").as[Boolean], v3Channel)
    assert((v3Channel \ "provenance").as[List[JsObject]].nonEmpty, v3Channel)
    assert(
      (v3Channel \ "primitive_proof_source" \ "id").asOpt[String].exists(_.nonEmpty),
      v3Channel
    )
    assert(
      (v3Channel \ "evidence_identity_sha256").as[String].matches("[0-9a-f]{64}"),
      v3Channel
    )

  test("v3 canonicalizes duplicate witnesses without changing historical v2"):
    val first = ConcreteChessObject(EvidenceObjectKind.Square, "a1")
    val second = ConcreteChessObject(EvidenceObjectKind.Square, "b2")
    val duplicated = List(second, first, second)
    val duplicatedChannel = channel.copy(binding = channel.binding.copy(witness = duplicated))

    val v2Witnesses = (
      CauseAuditAdapterCli.projectedCOwnedChannelDocument(duplicatedChannel, 2, graph) \ "witness"
    ).as[List[JsObject]]
    val v3Witnesses = (
      CauseAuditAdapterCli.projectedCOwnedChannelDocument(duplicatedChannel, 3, graph) \ "witness"
    ).as[List[JsObject]]

    assertEquals(
      v2Witnesses,
      duplicated.map(value => Json.obj("kind" -> code(value.kind), "key" -> value.key))
    )
    assertEquals(v3Witnesses.size, 2)
    assertEquals(
      v3Witnesses.map(value =>
        ((value \ "kind").as[String], (value \ "key").as[String])
      ).toSet,
      Set(("square", "a1"), ("square", "b2"))
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

  private def code(value: Any): String =
    value.toString.replaceAll("([a-z0-9])([A-Z])", "$1_$2").toLowerCase
