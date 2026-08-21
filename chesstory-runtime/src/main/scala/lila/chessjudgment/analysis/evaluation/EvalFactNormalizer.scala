package lila.chessjudgment.analysis.evaluation

import lila.chessjudgment.model.line.CandidateLineEvaluation
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
        producer = (line.evaluation match
          case CandidateLineEvaluation.EngineSearch(_) => EvidenceProducer.EngineEvalProducer
          case CandidateLineEvaluation.ExactAutomaticTerminal(_, _) => EvidenceProducer.LegalLineProducer),
        layer = EvidenceLayer.Eval,
        position = position,
        line = Some(line.ref),
        scope = scope,
        confidence = line.evaluation match
          case CandidateLineEvaluation.EngineSearch(_) => EvidenceConfidence.EngineBacked
          case CandidateLineEvaluation.ExactAutomaticTerminal(_, _) => EvidenceConfidence.LegalReplayVerified
      )
    EvidenceRecord(
      ref = ref,
      payload = CandidateLineEvaluationEvidence(
        line = line.ref,
        evaluation = line.evaluation
      ),
      parents = parents
    )
