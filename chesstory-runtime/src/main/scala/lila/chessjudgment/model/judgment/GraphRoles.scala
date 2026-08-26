package lila.chessjudgment.model.judgment

enum PositionNodeRole:
  case Before
  case AfterPlayed
  case AfterReference
  case AfterAlternative
  case AfterThreat
  case AfterThreatContinuation

  def scope: EvidenceScope =
    this match
      case Before           => EvidenceScope.BeforePosition
      case AfterPlayed      => EvidenceScope.AfterPlayedPosition
      case AfterReference   => EvidenceScope.AfterReferencePosition
      case AfterAlternative => EvidenceScope.AlternativeTransition
      case AfterThreat      => EvidenceScope.ThreatLine
      case AfterThreatContinuation => EvidenceScope.ThreatLine

enum LineNodeRole:
  case Played
  case BestReference
  case Alternative
  case Threat

  def scope: EvidenceScope =
    this match
      case Played        => EvidenceScope.PlayedLine
      case BestReference => EvidenceScope.BestLine
      case Alternative   => EvidenceScope.CandidateLine
      case Threat        => EvidenceScope.ThreatLine

  def subject: ClaimSubject =
    this match
      case Played        => ClaimSubject.PlayedMove
      case BestReference => ClaimSubject.ReferenceMove
      case Alternative   => ClaimSubject.CandidateLine
      case Threat        => ClaimSubject.Threat

object LineNodeRole:

  def fromMoveEvidenceScope(scope: EvidenceScope): Option[LineNodeRole] =
    scope match
      case EvidenceScope.PlayedTransition =>
        Some(LineNodeRole.Played)
      case EvidenceScope.ReferenceTransition =>
        Some(LineNodeRole.BestReference)
      case EvidenceScope.AlternativeTransition =>
        Some(LineNodeRole.Alternative)
      case EvidenceScope.ThreatLine =>
        Some(LineNodeRole.Threat)
      case _ =>
        None

enum TransitionEdgeRole:
  case Played
  case Reference
  case Alternative
  case Threat
  case ThreatContinuation

  def lineRole: LineNodeRole =
    this match
      case Played      => LineNodeRole.Played
      case Reference   => LineNodeRole.BestReference
      case Alternative => LineNodeRole.Alternative
      case Threat      => LineNodeRole.Threat
      case ThreatContinuation => LineNodeRole.Threat

  def scope: EvidenceScope =
    this match
      case Played      => EvidenceScope.PlayedTransition
      case Reference   => EvidenceScope.ReferenceTransition
      case Alternative => EvidenceScope.AlternativeTransition
      case Threat      => EvidenceScope.ThreatLine
      case ThreatContinuation => EvidenceScope.ThreatLine

  def subject: ClaimSubject =
    this match
      case Played      => ClaimSubject.PlayedMove
      case Reference   => ClaimSubject.ReferenceMove
      case Alternative => ClaimSubject.CandidateLine
      case Threat      => ClaimSubject.Threat
      case ThreatContinuation => ClaimSubject.Threat

enum EvidenceLayer:
  case PositionFeature
  case StrategicMechanism
  case OpeningContext
  case FeatureAnchor
  case ApplicabilityAssessment
  case Line
  case Eval
  case TacticalMechanism
  case MoveTransition
  case Relation
  case StructuralDelta
  case PlanCausalEvent
  case PlanTransition
  case CandidateComparison
  case RelativeAssessment
  case RelativeCause

enum ClaimSubject:
  case Position
  case PlayedMove
  case ReferenceMove
  case CandidateLine
  case Threat
  case Plan
