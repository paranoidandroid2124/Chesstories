package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.PlanSequenceSummary

case class MoveTransitionEdge(
    role: TransitionEdgeRole,
    id: String,
    from: PositionNodeRef,
    moveUci: String,
    to: PositionNodeRef,
    planTransition: Option[PlanSequenceSummary],
    evidence: EvidenceRef
):
  def matches(record: EvidenceRecord): Boolean =
    record.payload match
      case MoveTransitionEvidence(moveUci, from, to) =>
        record.ref.scope == role.scope && moveUci == this.moveUci && from == this.from && to == this.to
      case payload: StructuralDeltaEvidence =>
        payload.role == role && payload.moveUci == moveUci && payload.from == from && payload.to == to
      case _: StrategicMechanismEvidence =>
        record.ref.scope == role.scope &&
          (
            record.ref.line.exists(line => line.rootMove == moveUci && line.role == role.lineRole) ||
              record.parents.exists(_.id == evidence.id)
          )
      case _ =>
        false
