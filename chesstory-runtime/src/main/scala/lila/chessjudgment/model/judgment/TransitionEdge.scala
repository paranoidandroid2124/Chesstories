package lila.chessjudgment.model.judgment

case class MoveTransitionEdge(
    role: TransitionEdgeRole,
    from: PositionNodeRef,
    moveUci: String,
    to: PositionNodeRef,
    evidence: EvidenceRef
):
  def matches(record: EvidenceRecord): Boolean =
    record.payload match
      case MoveTransitionEvidence(moveUci, from, to, _) =>
        record.ref.scope == role.scope && moveUci == this.moveUci && from == this.from && to == this.to
      case payload: StructuralDeltaEvidence =>
        payload.role == role && payload.moveUci == moveUci && payload.from == from && payload.to == to
      case _ =>
        false
