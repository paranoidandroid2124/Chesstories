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
 * - Calculate momentum for plan persistence
 * - Calculate transition momentum
 */
object TransitionAnalyzer:

  private val MOMENTUM_DECAY = 0.15      // Linear decay per move
  private val MOMENTUM_BOOST = 0.25       // Boost (net +0.1 after decay)

  private val MOMENTUM_MAX = 1.0
  private val MOMENTUM_MIN = 0.0

  /**
   * Main entry: analyze transition from previous continuity to the current plan.
   * Returns a transition assessment and momentum score.
   */
  def analyze(
    previousPlan: Plan,
    currentPlans: ActivePlans,
    continuity: PlanContinuity,
    ctx: PlanInteractionContext
  ): PlanSequenceSummary = {
    val currPlan = currentPlans.primary.plan
    val prevPlanKey = continuity.planId
    val prevMomentum = 0.5 + (continuity.consecutivePlies * MOMENTUM_BOOST)

    val transType = prevPlanKey match
      case None => TransitionType.Opening
      case Some(prev) if continuityMatches(prev, currPlan) => TransitionType.Continuation
      case Some(_) => classifyShift(Some(previousPlan), currPlan, ctx)
    
    val momentum = calcMomentum(prevMomentum, transType)

    PlanSequenceSummary(
      transitionType = transType,
      momentum = momentum,
      primaryPlanId = Some(currPlan.id.toString),
      secondaryPlanId = currentPlans.secondary.map(_.plan.id.toString),
      previousPlanId = Some(previousPlan.id.toString),
      continuity = Some(continuity)
    )
  }

  private def continuityMatches(previousKey: String, currentPlan: Plan): Boolean =
    previousKey.equalsIgnoreCase(currentPlan.id.toString)

  /**
   * Classify how the plan changed.
   */
  def classifyTransition(
    prevPlan: Option[Plan],
    currPlan: Plan,
    ctx: PlanInteractionContext
  ): TransitionType = prevPlan match {
    case None => 
      TransitionType.Opening
      
    case Some(prev) if prev.id == currPlan.id => 
      TransitionType.Continuation
      
    case Some(prev) =>
      classifyShift(Some(prev), currPlan, ctx)
  }

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

  /**
   * Calculate momentum based on transition type.
   * 
   * - Continuation: boost by 0.1, capped at 1.0
   * - ForcedPivot: reset to 0.3 (low, forced change)
   * - Opportunistic: reset to 0.6 (moderate, new attack)
   * - NaturalShift: reset to 0.5 (neutral, planned change)
   * - Opening: start at 0.5
   */
  def calcMomentum(
    prevMomentum: Double,
    transitionType: TransitionType
  ): Double = transitionType match {
    case TransitionType.Continuation =>
      (prevMomentum - MOMENTUM_DECAY + MOMENTUM_BOOST).max(MOMENTUM_MIN).min(MOMENTUM_MAX)
      
    case TransitionType.Opening =>
      0.5
      
    case TransitionType.ForcedPivot =>
      0.3
      
    case TransitionType.Opportunistic =>
      0.6
      
    case TransitionType.NaturalShift =>
      0.5
  }

  // Extension to check if category is attack
  extension (cat: PlanCategory)
    def isAttack: Boolean = cat == PlanCategory.Attack
