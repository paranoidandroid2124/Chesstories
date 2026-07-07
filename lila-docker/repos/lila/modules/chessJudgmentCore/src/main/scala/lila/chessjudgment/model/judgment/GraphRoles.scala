package lila.chessjudgment.model.judgment

enum PositionNodeRole:
  case Before
  case AfterPlayed
  case AfterReference
  case AfterAlternative
  case AfterThreat

  def scope: EvidenceScope =
    this match
      case Before           => EvidenceScope.BeforePosition
      case AfterPlayed      => EvidenceScope.AfterPlayedPosition
      case AfterReference   => EvidenceScope.AfterReferencePosition
      case AfterAlternative => EvidenceScope.AlternativeTransition
      case AfterThreat      => EvidenceScope.ThreatLine

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

  def subject: IdeaSubject =
    this match
      case Played        => IdeaSubject.PlayedMove
      case BestReference => IdeaSubject.ReferenceMove
      case Alternative | Threat =>
        IdeaSubject.CandidateLine

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

  def lineRole: LineNodeRole =
    this match
      case Played      => LineNodeRole.Played
      case Reference   => LineNodeRole.BestReference
      case Alternative => LineNodeRole.Alternative
      case Threat      => LineNodeRole.Threat

  def scope: EvidenceScope =
    this match
      case Played      => EvidenceScope.PlayedTransition
      case Reference   => EvidenceScope.ReferenceTransition
      case Alternative => EvidenceScope.AlternativeTransition
      case Threat      => EvidenceScope.ThreatLine

  def subject: IdeaSubject =
    this match
      case Played      => IdeaSubject.PlayedMove
      case Reference   => IdeaSubject.ReferenceMove
      case Alternative => IdeaSubject.CandidateLine
      case Threat      => IdeaSubject.CandidateLine

enum EvidenceLayer:
  case Board
  case SinglePosition
  case PawnStructure
  case Strategic
  case StrategicMechanism
  case OpeningContext
  case FeatureAnchor
  case ApplicabilityAssessment
  case ThreatPressure
  case Line
  case Eval
  case MoveMotif
  case TacticalMechanism
  case MoveTransition
  case Relation
  case StructuralDelta
  case PlanPressure
  case PlanTransition
  case CandidateComparison
  case Counterfactual
  case RelativeAssessment
  case RelativeCause
  case MoveVerdictCertification
  case ChessIdea
  case Claim

enum ChessIdeaFamily:
  case Tactical
  case Strategic
  case PawnStructure
  case Opening
  case Defensive
  case Conversion
  case Material
  case Evaluation

enum IdeaSubject:
  case Position
  case PlayedMove
  case ReferenceMove
  case CandidateLine
  case Threat
  case Plan
