package lila.chessjudgment.model

import lila.chessjudgment.model.strategic.PlanContinuity

case class PlanSequenceSummary(
  transitionType: TransitionType,
  momentum: Double,
  primaryPlanId: Option[String] = None,
  secondaryPlanId: Option[String] = None,
  previousPlanId: Option[String] = None,
  continuity: Option[PlanContinuity] = None
)

enum TransitionType:
  case Continuation   // Same plan persists
  case NaturalShift   // Phase change → new plan
  case ForcedPivot    // Threat forces abandonment
  case Opportunistic  // Unexpected opportunity
  case Opening        // First move of sequence
