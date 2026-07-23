package lila.chessjudgment.model.judgment

final case class ChessIdeaRef(
    id: String,
    family: ChessIdeaFamily
)

final case class ChessIdea(
    ref: ChessIdeaRef,
    subject: IdeaSubject,
    primaryPosition: PositionNodeRef,
    primaryLine: Option[LineNodeRef],
    moveUci: Option[String],
    evidence: List[EvidenceRef],
    requiredLayers: List[EvidenceLayer],
    scope: EvidenceScope,
    confidence: EvidenceConfidence
)

enum IdeaVerdictRelation:
  case SupportsVerdict
  case ExplainsIdeaDespiteBadVerdict
  case RefutesIdea
  case DefensiveNecessity

final case class IdeaVerdictBinding(
    idea: ChessIdeaRef,
    verdict: EvalComparison,
    relation: IdeaVerdictRelation,
    evidence: List[EvidenceRef]
)
