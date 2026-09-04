package lila.chessjudgment.model.judgment

enum PositionNodeRole:
  case Before
  case AfterPlayed
  case AfterAlternative
  case LineOccurrence

  def scope: EvidenceScope =
    this match
      case Before           => EvidenceScope.BeforePosition
      case AfterPlayed      => EvidenceScope.AfterPlayedPosition
      case AfterAlternative => EvidenceScope.AlternativeTransition
      case LineOccurrence   => EvidenceScope.LegalLine

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
  case Alternative
  case LineContinuation

  def scope: EvidenceScope =
    this match
      case Played      => EvidenceScope.PlayedTransition
      case Alternative => EvidenceScope.AlternativeTransition
      case LineContinuation => EvidenceScope.LegalLine

enum EvidenceLayer:
  case PositionOccurrence
  case Line
  case Eval
  case MoveTransition
  case Relation
  case StructuralDelta
  case CandidateComparison
  case RelativeAssessment
