package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** Internal shared theorem boundary for two exact vacancy families.
  *
  * It certifies only this lower statement: a reference root mover leaves its
  * exact square, a later same-side movement occurs, while the Played sibling
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
    referenceBranchId: String,
    playedBranchId: String,
    laterStepIndex: Int,
    playedBlockerState: CausalClosedStateBinding,
    playedMoverState: CausalClosedStateBinding,
    playedMoveAbsence: CausalClosedAbsenceBinding
):
  private[chessjudgment] def matches(
      exactEnabler: RelationMoveTransitionWitness,
      exactEnablerMoveUci: String,
      exactBlocker: RelationColoredPieceWitness,
      exactLaterMove: RelationMoveTransitionWitness,
      exactLaterMoveUci: String,
      exactLaterMover: RelationColoredPieceWitness,
      referenceBranch: CausalBranchOccurrence,
      playedBranch: CausalBranchOccurrence,
      exactLaterStepIndex: Int,
      exactPlayedBlockerState: CausalClosedStateBinding,
      exactPlayedMoverState: CausalClosedStateBinding,
      exactPlayedMoveAbsence: CausalClosedAbsenceBinding
  ): Boolean =
    VacancySiblingClosure.certified(
      exactEnabler,
      exactEnablerMoveUci,
      exactBlocker,
      exactLaterMove,
      exactLaterMoveUci,
      exactLaterMover,
      referenceBranch,
      playedBranch,
      exactLaterStepIndex,
      exactPlayedBlockerState,
      exactPlayedMoverState,
      exactPlayedMoveAbsence
    ).contains(this)

private[chessjudgment] object VacancySiblingClosure:
  def certified(
      enabler: RelationMoveTransitionWitness,
      enablerMoveUci: String,
      blocker: RelationColoredPieceWitness,
      laterMove: RelationMoveTransitionWitness,
      laterMoveUci: String,
      laterMover: RelationColoredPieceWitness,
      referenceBranch: CausalBranchOccurrence,
      playedBranch: CausalBranchOccurrence,
      laterStepIndex: Int,
      playedBlockerState: CausalClosedStateBinding,
      playedMoverState: CausalClosedStateBinding,
      playedMoveAbsence: CausalClosedAbsenceBinding
  ): Option[VacancySiblingClosure] =
    val playedPreUseIndex = laterStepIndex - 1
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
        referenceRoot <- referenceBranch.stepAt(0)
        playedRoot <- playedBranch.stepAt(0)
      yield referenceRoot.step.ply == playedRoot.step.ply &&
        PrincipalVariationEvidence.sameBoardState(
          referenceRoot.step.fenBefore,
          playedRoot.step.fenBefore
        )
    val referenceMovesMatch =
      referenceBranch.stepAt(0).exists(step =>
        EvidenceRef.sameMove(step.step.moveUci, enablerMoveUci)
      ) && referenceBranch.stepAt(laterStepIndex).exists(step =>
        EvidenceRef.sameMove(step.step.moveUci, laterMoveUci)
      )
    val playedCoordinatesMatch =
      List(playedBlockerState, playedMoverState).forall(binding =>
        binding.branchId == playedBranch.branchId &&
          binding.branchRole == ComparedLineBranchRole.PlayedRootAnalysisContinuation &&
          binding.afterStepIndex == playedPreUseIndex
      ) && playedMoveAbsence.branchId == playedBranch.branchId &&
        playedMoveAbsence.branchRole == ComparedLineBranchRole.PlayedRootAnalysisContinuation &&
        playedMoveAbsence.afterStepIndex == playedPreUseIndex
    val exactStates =
      playedBlockerState.query ==
        PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(blocker) &&
        playedMoverState.query ==
          PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(laterMover)
    val exactAbsence =
      playedMoveAbsence.authority.query ==
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
        referenceBranch.role == ComparedLineBranchRole.CounterfactualReference &&
        referenceBranch.line.role == LineNodeRole.BestReference &&
        playedBranch.role == ComparedLineBranchRole.PlayedRootAnalysisContinuation &&
        playedBranch.line.role == LineNodeRole.Played &&
        referenceBranch.replaySteps.size > laterStepIndex &&
        playedBranch.replaySteps.size == laterStepIndex &&
        sameRoot.contains(true) && referenceMovesMatch && playedCoordinatesMatch &&
        exactStates && exactAbsence
    )(
      VacancySiblingClosure(
        enabler,
        EvidenceRef.normalizeMove(enablerMoveUci),
        blocker,
        laterMove,
        EvidenceRef.normalizeMove(laterMoveUci),
        laterMover,
        referenceBranch.branchId,
        playedBranch.branchId,
        laterStepIndex,
        playedBlockerState,
        playedMoverState,
        playedMoveAbsence
      )
    )
