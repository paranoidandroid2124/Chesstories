package lila.chessjudgment.analysis.transition

import lila.chessjudgment.model.*
import lila.chessjudgment.analysis.evaluation.JudgmentThresholds
import lila.chessjudgment.analysis.plan.PlanInteractionContext
import lila.chessjudgment.model.strategic.PlanContinuity
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanTheme

/**
 * Analyzes plan transitions across moves.
 * 
 * Core responsibilities:
 * - Classify transition type (Continuation, NaturalShift, etc.)
 */
object TransitionAnalyzer:

  /**
   * Main entry: analyze transition from previous continuity to the current plan.
   * Returns a transition assessment.
   */
  def analyze(
    previousPlan: Plan,
    currentPlan: Plan,
    continuity: PlanContinuity,
    currentEvent: PlanEventIdentity,
    ctx: PlanInteractionContext
  ): PlanSequenceSummary = {
    val previousEvent = continuity.principalEvent
    val transType = previousEvent match
      case None => TransitionType.Opening
      case Some(previous) if previous.completedBy(currentEvent) => TransitionType.Completion
      case Some(previous) if previous.continuesInto(currentEvent) => TransitionType.Continuation
      case Some(previous) => classifyShift(previous, currentEvent, currentPlan.color, ctx)
    
    PlanSequenceSummary(
      transitionType = transType,
      primaryPlanId = Some(currentPlan.id),
      previousPlanId = Some(previousPlan.id),
      continuity = Some(continuity),
      previousEvent = previousEvent,
      currentEvent = Some(currentEvent)
    )
  }

  private def classifyShift(
    previousEvent: PlanEventIdentity,
    currentEvent: PlanEventIdentity,
    currentColor: chess.Color,
    ctx: PlanInteractionContext
  ): TransitionType =
    if isForcedPivot(previousEvent, currentEvent, ctx) then TransitionType.ForcedPivot
    else if isConversionShift(currentEvent, currentColor, ctx) then TransitionType.NaturalShift
    else if isOpportunisticShift(currentEvent, currentColor, ctx) then TransitionType.Opportunistic
    else TransitionType.NaturalShift

  private def isForcedPivot(
    previousEvent: PlanEventIdentity,
    currentEvent: PlanEventIdentity,
    ctx: PlanInteractionContext
  ): Boolean =
    ctx.underDefensivePressure &&
      currentEvent.goalTheme == PlanTheme.RestrictionProphylaxis &&
      previousEvent.goalTheme != PlanTheme.RestrictionProphylaxis

  private def isConversionShift(
    currentEvent: PlanEventIdentity,
    currentColor: chess.Color,
    ctx: PlanInteractionContext
  ): Boolean =
    val winningWindow =
      ctx.positionAssessment.exists(_.simplifyBias.shouldSimplify) &&
        ctx.winPercentAdvantageFor(currentColor) >= JudgmentThresholds.CONVERSION_EDGE_WP
    val phaseDrivenEndgame =
      ctx.phaseEnumOpt.contains(lila.chessjudgment.analysis.singlePosition.GamePhaseType.Endgame)
    val currentConversion =
      currentEvent.goalTheme == PlanTheme.FavorableExchange ||
        currentEvent.goalTheme == PlanTheme.AdvantageTransformation

    currentConversion && (winningWindow || phaseDrivenEndgame)

  private def isOpportunisticShift(
    currentEvent: PlanEventIdentity,
    currentColor: chess.Color,
    ctx: PlanInteractionContext
  ): Boolean =
    val currentAttack =
      currentEvent.goalTheme == PlanTheme.FlankInfrastructure ||
        currentEvent.goalTheme == PlanTheme.WeaknessFixation
    val attackingWindow = ctx.strategicThreatToThem && currentAttack
    attackingWindow && !isConversionShift(currentEvent, currentColor, ctx)
