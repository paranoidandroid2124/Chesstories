package lila.chessjudgment.model.judgment

enum PositionNodeRole:
  case Before
  case AfterPlayed
  case AfterReference
  case AfterAlternative

  def scope: EvidenceScope =
    this match
      case Before           => EvidenceScope.BeforePosition
      case AfterPlayed      => EvidenceScope.AfterPlayedPosition
      case AfterReference   => EvidenceScope.AfterReferencePosition
      case AfterAlternative => EvidenceScope.AlternativeTransition

enum LineNodeRole:
  case Played
  case BestReference
  case Alternative

  def scope: EvidenceScope =
    this match
      case Played        => EvidenceScope.PlayedLine
      case BestReference => EvidenceScope.BestLine
      case Alternative   => EvidenceScope.CandidateLine

enum TransitionEdgeRole:
  case Played
  case Reference
  case Alternative

  def scope: EvidenceScope =
    this match
      case Played      => EvidenceScope.PlayedTransition
      case Reference   => EvidenceScope.ReferenceTransition
      case Alternative => EvidenceScope.AlternativeTransition

enum EvidenceLayer:
  case PositionOccurrence
  case Line
  case Eval
  case MoveTransition
  case Relation
  case StructuralDelta
  case CausalProof
  case PassedPawnResultEvent
  case CandidateComparison
  case RelativeAssessment
  case OccurrenceExplanation
