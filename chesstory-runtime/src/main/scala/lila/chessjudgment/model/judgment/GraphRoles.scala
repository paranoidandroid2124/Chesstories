package lila.chessjudgment.model.judgment

enum PositionNodeRole:
  case Before
  case AfterPlayed
  case AfterReference
  case AfterAlternative
  case AfterBranchReply
  case AfterBranchReplyContinuation

  def scope: EvidenceScope =
    this match
      case Before           => EvidenceScope.BeforePosition
      case AfterPlayed      => EvidenceScope.AfterPlayedPosition
      case AfterReference   => EvidenceScope.AfterReferencePosition
      case AfterAlternative => EvidenceScope.AlternativeTransition
      case AfterBranchReply             => EvidenceScope.BranchReplyLine
      case AfterBranchReplyContinuation => EvidenceScope.BranchReplyLine

enum LineNodeRole:
  case Played
  case BestReference
  case Alternative
  case BranchReply

  def scope: EvidenceScope =
    this match
      case Played        => EvidenceScope.PlayedLine
      case BestReference => EvidenceScope.BestLine
      case Alternative   => EvidenceScope.CandidateLine
      case BranchReply   => EvidenceScope.BranchReplyLine

  def claimSubject: Option[ClaimSubject] =
    this match
      case Played        => Some(ClaimSubject.PlayedMove)
      case BestReference => Some(ClaimSubject.ReferenceMove)
      case Alternative   => Some(ClaimSubject.CandidateLine)
      case BranchReply   => None

object LineNodeRole:

  def fromMoveEvidenceScope(scope: EvidenceScope): Option[LineNodeRole] =
    scope match
      case EvidenceScope.PlayedTransition =>
        Some(LineNodeRole.Played)
      case EvidenceScope.ReferenceTransition =>
        Some(LineNodeRole.BestReference)
      case EvidenceScope.AlternativeTransition =>
        Some(LineNodeRole.Alternative)
      case EvidenceScope.BranchReplyLine =>
        Some(LineNodeRole.BranchReply)
      case _ =>
        None

enum TransitionEdgeRole:
  case Played
  case Reference
  case Alternative
  case BranchReply
  case BranchReplyContinuation

  def lineRole: LineNodeRole =
    this match
      case Played      => LineNodeRole.Played
      case Reference   => LineNodeRole.BestReference
      case Alternative => LineNodeRole.Alternative
      case BranchReply             => LineNodeRole.BranchReply
      case BranchReplyContinuation => LineNodeRole.BranchReply

  def scope: EvidenceScope =
    this match
      case Played      => EvidenceScope.PlayedTransition
      case Reference   => EvidenceScope.ReferenceTransition
      case Alternative => EvidenceScope.AlternativeTransition
      case BranchReply             => EvidenceScope.BranchReplyLine
      case BranchReplyContinuation => EvidenceScope.BranchReplyLine

enum EvidenceLayer:
  case PositionOccurrence
  case Line
  case Eval
  case TacticalMechanism
  case MoveTransition
  case Relation
  case StructuralDelta
  case CausalProof
  case PassedPawnResultEvent
  case CandidateComparison
  case RelativeAssessment
  case RelativeCause

enum ClaimSubject:
  case PlayedMove
  case ReferenceMove
  case CandidateLine
