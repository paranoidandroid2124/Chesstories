package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.CandidateLineEvaluation

case class LineNodeRef(
    id: String,
    rootMove: String
):
  def rootMoveIn(normalizedMoves: Set[String]): Boolean =
    normalizedMoves(EvidenceRef.normalizeMove(rootMove))

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
  def from(line: CandidateLineNode): SemanticLineKey =
    SemanticLineKey(
      role = line.role,
      rank = line.rank,
      rootMove = EvidenceRef.normalizeMove(line.ref.rootMove)
    )

  def same(left: CandidateLineNode, right: CandidateLineNode): Boolean =
    from(left) == from(right)

  def sameOptional(left: Option[CandidateLineNode], right: Option[CandidateLineNode]): Boolean =
    left.map(from) == right.map(from)

case class LegalLineNode(
    ref: LineNodeRef,
    evidence: EvidenceRef
)

case class CandidateLineNode(
    ref: LineNodeRef,
    role: LineNodeRole,
    rank: Int,
    evaluation: CandidateLineEvaluation,
    lineEvidence: EvidenceRef
)
