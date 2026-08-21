package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.CandidateLineEvaluation

case class LineNodeRef(
    id: String,
    rootMove: String,
    rank: Int,
    role: LineNodeRole
)

/** Evidence-id-independent line identity used only where semantic equality is
  * required. Role and rank remain part of the comparison relation.
  */
final case class SemanticLineKey(
    role: LineNodeRole,
    rank: Int,
    rootMove: String
):
  def stableKey: String = s"$role:$rank:$rootMove"

object SemanticLineKey:
  def from(line: LineNodeRef): SemanticLineKey =
    SemanticLineKey(
      role = line.role,
      rank = line.rank,
      rootMove = EvidenceRef.normalizeMove(line.rootMove)
    )

case class CandidateLineNode(
    ref: LineNodeRef,
    evaluation: CandidateLineEvaluation,
    evidence: EvidenceRef
):
  val role: LineNodeRole = ref.role
