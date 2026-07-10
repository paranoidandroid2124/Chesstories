package lila.chessjudgment.model.judgment

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

  def sameDestinationDifferentOrigin(left: String, right: String): Boolean =
    val normalizedLeft = normalizeMove(left)
    val normalizedRight = normalizeMove(right)
    normalizedLeft.length >= 4 &&
      normalizedRight.length >= 4 &&
      normalizedLeft.take(2) != normalizedRight.take(2) &&
      normalizedLeft.slice(2, 4) == normalizedRight.slice(2, 4)

  def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

enum EvidenceProducer:
  case BoardFactProducer
  case SinglePositionProducer
  case LegalLineProducer
  case EngineEvalProducer
  case TacticalRelationProducer
  case PawnStructureProducer
  case StrategicFeatureProducer
  case StrategicMechanismProducer
  case OpeningContextProducer
  case FeatureAnchorProducer
  case ApplicabilityAssessmentProducer
  case ThreatPressureProducer
  case MoveMotifProducer
  case TacticalMechanismProducer
  case MoveTransitionProducer
  case StructuralDeltaProducer
  case PlanPressureProducer
  case PlanTransitionProducer
  case RelativeMoveProducer
  case ChessIdeaProducer
  case ClaimComposer

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
  case ThreatLine
  case Counterfactual

enum EvidenceConfidence:
  case LegalReplayVerified
  case EngineBacked
  case BoardDerived
  case Heuristic
  case Mixed
