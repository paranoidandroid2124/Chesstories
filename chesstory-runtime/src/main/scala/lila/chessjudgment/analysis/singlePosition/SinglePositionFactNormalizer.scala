package lila.chessjudgment.analysis.singlePosition

import lila.chessjudgment.model.judgment.*

object SinglePositionFactNormalizer:

  def fromAssessment(
      id: String,
      assessment: SinglePositionAssessment,
      position: PositionNodeRef,
      scope: EvidenceScope,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.SinglePositionProducer,
        layer = EvidenceLayer.SinglePosition,
        position = position,
        line = None,
        scope = scope,
        confidence = EvidenceConfidence.Mixed
      )
    EvidenceRecord(
      ref = ref,
      payload = SinglePositionEvidence(assessment),
      parents = parents
    )
