package lila.chessjudgment.analysis.evaluation

import lila.chessjudgment.model.judgment.*

object EvalFactNormalizer:

  def fromCandidateLine(
      id: String,
      line: CandidateLineNode,
      position: PositionNodeRef,
      scope: EvidenceScope,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.EngineEvalProducer,
        layer = EvidenceLayer.Eval,
        position = position,
        line = Some(line.ref),
        scope = scope,
        confidence =
          if JudgmentThresholds.engineBackedByDepth(line.depth, line.mate) then EvidenceConfidence.EngineBacked
          else EvidenceConfidence.Mixed
      )
    EvidenceRecord(
      ref = ref,
      payload = EvalFactEvidence(
        line = line.ref,
        whitePovEvalCp = line.whitePovEvalCp,
        mate = line.mate,
        depth = line.depth
      ),
      parents = parents
    )
