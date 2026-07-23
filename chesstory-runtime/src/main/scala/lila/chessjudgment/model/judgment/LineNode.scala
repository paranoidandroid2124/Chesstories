package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.strategic.VariationLine

case class LineNodeRef(
    id: String,
    rootMove: String,
    rank: Int,
    role: LineNodeRole
)

case class CandidateLineNode(
    ref: LineNodeRef,
    line: VariationLine,
    evidence: EvidenceRef
):
  val role: LineNodeRole = ref.role
  val whitePovEvalCp: Int = line.scoreCp
  val mate: Option[Int] = line.mate
  val depth: Int = line.depth
