package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** Internal shared theorem boundary for two exact vacancy families.
  *
  * It certifies only this lower statement: one root mover leaves its exact
  * square, a later same-side movement occurs, while the retained sibling
  * retains the exact blocker and later mover and its closed inventory lacks
  * that same from/to legal movement. Slider geometry and destination
  * occupation remain typed obligations of their respective public families.
  */
private[chessjudgment] final case class VacancySiblingClosure private (
    enabler: RelationMoveTransitionWitness,
    enablerMoveUci: String,
    blocker: RelationColoredPieceWitness,
    laterMove: RelationMoveTransitionWitness,
    laterMoveUci: String,
    laterMover: RelationColoredPieceWitness,
    enablingBranchId: String,
    retainedBranchId: String,
    laterStepIndex: Int,
    retainedBlockerPersistence: List[CausalClosedStateBinding],
    retainedMoverPersistence: List[CausalClosedStateBinding],
    retainedMoveAbsence: CausalClosedAbsenceBinding
):
  private[chessjudgment] def matches(
      exactEnabler: RelationMoveTransitionWitness,
      exactEnablerMoveUci: String,
      exactBlocker: RelationColoredPieceWitness,
      exactLaterMove: RelationMoveTransitionWitness,
      exactLaterMoveUci: String,
      exactLaterMover: RelationColoredPieceWitness,
      enablingBranch: CausalBranchOccurrence,
      retainedBranch: CausalBranchOccurrence,
      exactLaterStepIndex: Int,
      exactRetainedBlockerPersistence: List[CausalClosedStateBinding],
      exactRetainedMoverPersistence: List[CausalClosedStateBinding],
      exactRetainedMoveAbsence: CausalClosedAbsenceBinding
  ): Boolean =
    VacancySiblingClosure.certified(
      exactEnabler,
      exactEnablerMoveUci,
      exactBlocker,
      exactLaterMove,
      exactLaterMoveUci,
      exactLaterMover,
      enablingBranch,
      retainedBranch,
      exactLaterStepIndex,
      exactRetainedBlockerPersistence,
      exactRetainedMoverPersistence,
      exactRetainedMoveAbsence
    ).contains(this)

private[chessjudgment] object VacancySiblingClosure:
  def certified(
      enabler: RelationMoveTransitionWitness,
      enablerMoveUci: String,
      blocker: RelationColoredPieceWitness,
      laterMove: RelationMoveTransitionWitness,
      laterMoveUci: String,
      laterMover: RelationColoredPieceWitness,
      enablingBranch: CausalBranchOccurrence,
      retainedBranch: CausalBranchOccurrence,
      laterStepIndex: Int,
      retainedBlockerPersistence: List[CausalClosedStateBinding],
      retainedMoverPersistence: List[CausalClosedStateBinding],
      retainedMoveAbsence: CausalClosedAbsenceBinding
  ): Option[VacancySiblingClosure] =
    val retainedPreUseIndex = laterStepIndex - 1
    val exactBlocker = RelationColoredPieceWitness(
      enabler.from,
      enabler.beforeRole,
      enabler.side
    )
    val exactLaterMover = RelationColoredPieceWitness(
      laterMove.from,
      laterMove.beforeRole,
      laterMove.side
    )
    val sameRoot =
      for
        enablingRoot <- enablingBranch.stepAt(0)
        retainedRoot <- retainedBranch.stepAt(0)
      yield enablingRoot.step.ply == retainedRoot.step.ply &&
        PrincipalVariationEvidence.sameBoardState(
          enablingRoot.step.fenBefore,
          retainedRoot.step.fenBefore
        )
    val enablingMovesMatch =
      enablingBranch.stepAt(0).exists(step =>
        EvidenceRef.sameMove(step.step.moveUci, enablerMoveUci)
      ) && enablingBranch.stepAt(laterStepIndex).exists(step =>
        EvidenceRef.sameMove(step.step.moveUci, laterMoveUci)
      )
    def exactPersistence(bindings: List[CausalClosedStateBinding]): Boolean =
      bindings.size == laterStepIndex && bindings.zipWithIndex.forall { case (binding, stepIndex) =>
        binding.branchId == retainedBranch.branchId &&
          binding.branchRole == retainedBranch.role &&
          binding.afterStepIndex == stepIndex
      }
    val retainedCoordinatesMatch =
      exactPersistence(retainedBlockerPersistence) && exactPersistence(retainedMoverPersistence) &&
        retainedMoveAbsence.branchId == retainedBranch.branchId &&
        retainedMoveAbsence.branchRole == retainedBranch.role &&
        retainedMoveAbsence.afterStepIndex == retainedPreUseIndex
    val exactStates =
      retainedBlockerPersistence.forall(_.query ==
        PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(blocker)) &&
        retainedMoverPersistence.forall(_.query ==
          PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(laterMover))
    val exactAbsence =
      retainedMoveAbsence.authority.query ==
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
          laterMove.side,
          laterMove.from,
          laterMove.to
        )
    Option.when(
      laterStepIndex >= 2 && enabler.side == laterMove.side &&
        EvidenceRef.normalizeMove(enablerMoveUci).nonEmpty &&
        EvidenceRef.normalizeMove(laterMoveUci).nonEmpty &&
        blocker == exactBlocker && laterMover == exactLaterMover &&
        enablingBranch.branchId != retainedBranch.branchId &&
        enablingBranch.role != retainedBranch.role && enablingBranch.line != retainedBranch.line &&
        enablingBranch.replaySteps.size > laterStepIndex &&
        retainedBranch.replaySteps.size == laterStepIndex &&
        sameRoot.contains(true) && enablingMovesMatch && retainedCoordinatesMatch &&
        exactStates && exactAbsence
    )(
      VacancySiblingClosure(
        enabler,
        EvidenceRef.normalizeMove(enablerMoveUci),
        blocker,
        laterMove,
        EvidenceRef.normalizeMove(laterMoveUci),
        laterMover,
        enablingBranch.branchId,
        retainedBranch.branchId,
        laterStepIndex,
        retainedBlockerPersistence,
        retainedMoverPersistence,
        retainedMoveAbsence
      )
    )
