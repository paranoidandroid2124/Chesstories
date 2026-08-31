package lila.chessjudgment.model.judgment

import chess.White

import lila.chessjudgment.model.position.PositionFeatures

class ConversionContextPolicyTest extends munit.FunSuite:

  test("position features alone are not a conversion mechanism"):
    val features = PositionFeatures.empty
    val position = PositionNodeRef(features.fen, 0, Some(White))
    val record = EvidenceRecord(
      EvidenceRef(
        "low-material",
        EvidenceProducer.PositionFeatureProducer,
        EvidenceLayer.PositionFeature,
        position,
        None,
        EvidenceScope.CurrentPosition,
        EvidenceConfidence.BoardDerived
      ),
      PositionFeatureEvidence(features)
    )

    assert(!ConversionContextPolicy.supports(List(record), RelativeCauseKind.ConversionSecured))
