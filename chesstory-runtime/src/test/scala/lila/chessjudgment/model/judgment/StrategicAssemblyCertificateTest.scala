package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.model.position.PositionFeatures

class StrategicAssemblyCertificateTest extends munit.FunSuite:

  private val position = PositionNodeRef(PositionFeatures.empty.fen, 0, Some(White))

  private val source = EvidenceRecord(
    EvidenceRef(
      id = "strategic-source",
      producer = EvidenceProducer.PositionFeatureProducer,
      layer = EvidenceLayer.PositionFeature,
      position = position,
      line = None,
      scope = EvidenceScope.CurrentPosition,
      confidence = EvidenceConfidence.BoardDerived
    ),
    PositionFeatureEvidence(PositionFeatures.empty)
  )

  test("a strategic assembly certificate binds the exact carrier and signal inventory"):
    val ref = EvidenceRef(
      id = "strategic-carrier",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = position,
      line = None,
      scope = EvidenceScope.CurrentPosition,
      confidence = EvidenceConfidence.BoardDerived
    )
    val axis = StrategicAxisDetail(
      StrategicAxisKind.PlanCoherence,
      StrategicAxisPolarity.Gain,
      "plan-a"
    )
    val bare = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.PlanPressure,
      signals = List(
        StrategicMechanismSignal(
          StrategicMechanismSignalKind.PlanPressure,
          axis.label,
          source.ref,
          axis = Some(axis)
        )
      ),
      semanticAnchors = List(
        EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StrategicMechanism, "plan-pressure")
      )
    )
    val parents = List(source.ref)
    val certified = bare.copy(
      assemblyProof = Some(StrategicMechanismAssemblyProof.from(ref, parents, bare))
    )
    val record = EvidenceRecord(ref, certified, parents)
    val graph = TypedEvidenceGraph.empty.addAll(List(source, record))

    assert(graph.proofEligible(record))

    val tampered = record.copy(
      payload = certified.copy(signals = certified.signals.map(_.copy(label = "different-plan")))
    )
    val tamperedGraph = TypedEvidenceGraph.empty.addAll(List(source, tampered))
    assert(!tamperedGraph.proofEligible(tampered))
