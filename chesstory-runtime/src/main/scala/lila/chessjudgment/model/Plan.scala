package lila.chessjudgment.model

import chess.Color
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanSignal, PlanTheme }

case class PlanScoringResult(
    topPlans: List[PlanMatch],
    phase: String,
    compatibilityEvents: List[CompatibilityEvent] = Nil
):
  def confidence: Double = topPlans.headOption.map(_.score).getOrElse(0.0)

case class CompatibilityEvent(
  adjustment: CompatibilityAdjustment
)

enum CompatibilityAdjustment:
  case DefensivePressure
  case ConversionWindow
  case OpenCenterFlankRisk
  case OpeningPhase

sealed trait PlanSupport

object PlanSupport:
  case class Theme(theme: PlanTheme) extends PlanSupport
  case class Subplan(kind: PlanKind) extends PlanSupport
  case class ThemePolicyScore(score: Double) extends PlanSupport

final case class PlanMatch(
    plan: Plan,
    score: Double,
    evidence: List[EvidenceAtom],
    themePolicyScore: Option[Double] = None,
    blockers: List[PlanSignal] = Nil,
    missingSignals: List[PlanSignal] = Nil
):
  /**
   * Compatibility projection for plan-causal consumers. Theme and subplan are
   * never stored independently: both are determined by the canonical kind.
   */
  def support: List[PlanSupport] =
    List(
      PlanSupport.Theme(plan.theme),
      PlanSupport.Subplan(plan.kind)
    ) ++ themePolicyScore.map(score => PlanSupport.ThemePolicyScore(score.max(0.0).min(1.0)))

object PlanMatch:
  /**
   * Transitional constructor for callers that still provide the old support
   * projection. Conflicting identities are rejected and only the policy score
   * is retained.
   */
  def apply(
      plan: Plan,
      score: Double,
      evidence: List[EvidenceAtom],
      support: List[PlanSupport]
  ): PlanMatch =
    val declaredThemes = support.collect { case PlanSupport.Theme(theme) => theme }.distinct
    val declaredKinds = support.collect { case PlanSupport.Subplan(kind) => kind }.distinct
    require(declaredThemes.forall(_ == plan.theme), "plan theme must be derived from plan kind")
    require(declaredKinds.forall(_ == plan.kind), "plan subplan must equal canonical plan kind")
    new PlanMatch(
      plan = plan,
      score = score,
      evidence = evidence,
      themePolicyScore = support.collectFirst { case PlanSupport.ThemePolicyScore(value) => value },
      blockers = Nil,
      missingSignals = Nil
    )

final case class ActivePlans private[chessjudgment] (
    allPlans: List[PlanMatch],
    compatibilityEvents: List[CompatibilityEvent] = Nil
):
  require(allPlans.nonEmpty, "active plans require at least one ranked plan")
  def primary: PlanMatch = allPlans.head
  def secondary: Option[PlanMatch] = allPlans.lift(1)

case class EvidenceAtom(
    motif: Motif,
    weight: Double
)

enum PlanCategory:
  case Opening, Attack, Positional, Structural, Endgame, Defensive, Transition

/**
 * A plan persists exactly one classification value. Theme, category and the
 * legacy `id` access are projections of that value.
 */
final case class Plan(kind: PlanKind, color: Color):
  def id: PlanKind = kind
  def theme: PlanTheme = kind.theme
  def category: PlanCategory = kind.category

object Plan:
  // Compatibility factory retained for the plan-causal restriction proof path.
  def Prophylaxis(color: Color): Plan =
    Plan(PlanKind.ProphylaxisRestraint, color)

/**
 * Source-compatibility alias for graph records created before PlanKind became
 * the sole plan identity. It introduces no second runtime value.
 */
type PlanId = PlanKind

object PlanId:
  val QueensideAttack: PlanKind = PlanKind.QueensideWingExpansion
  val KingsideAttack: PlanKind = PlanKind.KingsideWingExpansion
