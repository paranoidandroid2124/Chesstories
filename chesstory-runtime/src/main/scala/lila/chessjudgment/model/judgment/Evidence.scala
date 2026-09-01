package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.PrincipalVariationEvidence

case class EvidenceRef(
    id: String,
    producer: EvidenceProducer,
    layer: EvidenceLayer,
    position: PositionNodeRef,
    line: Option[LineNodeRef],
    scope: EvidenceScope,
    confidence: EvidenceConfidence
)
object EvidenceRef:
  def sameMove(left: String, right: String): Boolean =
    normalizeMove(left) == normalizeMove(right)

  def normalizeMove(raw: String): String =
    PrincipalVariationEvidence.normalizeUci(raw)

enum EvidenceProducer:
  case PositionOccurrenceProducer
  case LegalLineProducer
  case EngineEvalProducer
  case RelationProducer
  case MoveTransitionProducer
  case StructuralDeltaProducer
  case CausalProofProducer
  case PassedPawnResultEventProducer
  case RelativeMoveProducer

enum EvidenceScope:
  case BeforePosition
  case AfterPlayedPosition
  case AfterReferencePosition
  case CurrentPosition
  case PlayedTransition
  case ReferenceTransition
  case AlternativeTransition
  case BestLine
  case PlayedLine
  case CandidateLine
  case Counterfactual

enum EvidenceConfidence:
  case LegalReplayVerified
  case EngineBacked
  case BoardDerived
  case Mixed
