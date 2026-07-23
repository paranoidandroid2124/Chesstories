package lila.chessjudgment.analysis.opening

import lila.chessjudgment.model.judgment.*

object OpeningContextFactNormalizer:

  def fromContext(
      id: String,
      identity: Option[OpeningIdentity],
      signals: List[OpeningContextSignal],
      recognition: Option[OpeningRecognition] = None,
      themePriorSelection: Option[OpeningThemePriorSelection] = None,
      position: PositionNodeRef,
      line: Option[LineNodeRef] = None,
      scope: EvidenceScope,
      confidence: EvidenceConfidence,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.OpeningContextProducer,
        layer = EvidenceLayer.OpeningContext,
        position = position,
        line = line,
        scope = scope,
        confidence = confidence
      )
    EvidenceRecord(
      ref = ref,
      payload = OpeningContextEvidence(
        identity = identity,
        signals = signals.distinct,
        recognition = recognition,
        themePriorSelection = themePriorSelection
      ),
      parents = parents
    )
