package lila.chessjudgment.analysis.transition

import lila.chessjudgment.model.*
import lila.chessjudgment.analysis.evaluation.JudgmentThresholds
import lila.chessjudgment.analysis.plan.PlanInteractionContext
import lila.chessjudgment.model.strategic.PlanContinuity

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
    currentPlans: ActivePlans,
    continuity: PlanContinuity,
    ctx: PlanInteractionContext
  ): PlanSequenceSummary = {
    val currPlan = currentPlans.primary.plan
    val prevPlanKey = continuity.planId

    val transType = prevPlanKey match
      case None => TransitionType.Opening
      case Some(prev) if continuityMatches(prev, currPlan) => TransitionType.Continuation
      case Some(_) => classifyShift(Some(previousPlan), currPlan, ctx)
    
    PlanSequenceSummary(
      transitionType = transType,
      primaryPlanId = Some(currPlan.id.toString),
      previousPlanId = Some(previousPlan.id.toString),
      continuity = Some(continuity)
    )
  }

  private def continuityMatches(previousKey: String, currentPlan: Plan): Boolean =
    previousKey.equalsIgnoreCase(currentPlan.id.toString)

  private def classifyShift(
    prevPlan: Option[Plan],
    currPlan: Plan,
    ctx: PlanInteractionContext
  ): TransitionType =
    if isForcedPivot(prevPlan, currPlan, ctx) then TransitionType.ForcedPivot
    else if isConversionShift(prevPlan, currPlan, ctx) then TransitionType.NaturalShift
    else if isOpportunisticShift(prevPlan, currPlan, ctx) then TransitionType.Opportunistic
    else TransitionType.NaturalShift

  private def isForcedPivot(
    prevPlan: Option[Plan],
    currPlan: Plan,
    ctx: PlanInteractionContext
  ): Boolean =
    ctx.underDefensivePressure &&
      currPlan.category == PlanCategory.Defensive &&
      prevPlan.forall(_.category != PlanCategory.Defensive)

  private def isConversionShift(
    prevPlan: Option[Plan],
    currPlan: Plan,
    ctx: PlanInteractionContext
  ): Boolean =
    val winningWindow =
      ctx.positionAssessment.exists(_.simplifyBias.shouldSimplify) &&
        ctx.winPercentAdvantageFor(currPlan.color) >= JudgmentThresholds.CONVERSION_EDGE_WP
    val phaseDrivenEndgame =
      ctx.phaseEnumOpt.contains(lila.chessjudgment.analysis.singlePosition.GamePhaseType.Endgame) &&
        (currPlan.category == PlanCategory.Endgame || currPlan.category == PlanCategory.Transition)
    val currentConversion =
      currPlan.category == PlanCategory.Transition ||
        currPlan.category == PlanCategory.Endgame ||
        currPlan.id == PlanId.Exchange ||
        currPlan.id == PlanId.QueenTrade
    val previousAttackOrRace =
      prevPlan.exists(plan =>
        plan.category == PlanCategory.Attack ||
          plan.category == PlanCategory.Structural ||
          isCounterplayPlan(plan)
      )

    currentConversion && (winningWindow || phaseDrivenEndgame || previousAttackOrRace)

  private def isOpportunisticShift(
    prevPlan: Option[Plan],
    currPlan: Plan,
    ctx: PlanInteractionContext
  ): Boolean =
    val currentAttack = currPlan.category == PlanCategory.Attack || isCounterplayPlan(currPlan)
    val attackingWindow = ctx.strategicThreatToThem && currentAttack
    attackingWindow && !isConversionShift(prevPlan, currPlan, ctx)

  private def isCounterplayPlan(plan: Plan): Boolean =
    plan.id == PlanId.Counterplay
