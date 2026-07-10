package lila.chessjudgment.model

import chess.Color
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanSignal, PlanTheme }

case class PlanScoringResult(
    topPlans: List[PlanMatch],
    confidence: Double,
    phase: String,
    compatibilityEvents: List[CompatibilityEvent] = Nil
)

case class CompatibilityEvent(
  originalScore: Double,
  finalScore: Double,
  delta: Double,
  adjustment: CompatibilityAdjustment,
  adjustmentType: CompatibilityAdjustmentType
)

enum CompatibilityAdjustment:
  case DefensivePressure
  case ConversionWindow
  case OpenCenterFlankRisk
  case OpeningPhase

enum CompatibilityAdjustmentType:
  case Boost
  case Downweight

sealed trait PlanSupport

object PlanSupport:
  case class Theme(theme: PlanTheme) extends PlanSupport
  case class Subplan(kind: PlanKind) extends PlanSupport
  case class ThemePolicyScore(score: Double) extends PlanSupport

case class PlanMatch(
    plan: Plan,
    score: Double,
    evidence: List[EvidenceAtom],
    support: List[PlanSupport] = Nil,
    blockers: List[PlanSignal] = Nil,
    missingSignals: List[PlanSignal] = Nil
)

case class ActivePlans(
    primary: PlanMatch,
    secondary: Option[PlanMatch],
    suppressed: List[PlanMatch],
    allPlans: List[PlanMatch],
    compatibilityEvents: List[CompatibilityEvent] = Nil
)

case class EvidenceAtom(
    motif: Motif,
    weight: Double
)

/**
 * High-level strategic plan inferred from motif patterns.
 * 
 * Categories:
 * - Opening Plans: Development, center occupation, and theory-tempo discipline
 * - Attack Plans: Direct assault on opponent's king or weaknesses
 * - Positional Plans: Improving piece placement and control
 * - Structural Plans: Exploiting or creating pawn structure advantages
 * - Endgame Plans: Specific endgame techniques
 * - Defensive Plans: Consolidation and prophylaxis
 */
sealed trait Plan:
  def id: PlanId
  def category: PlanCategory
  def color: Color

enum PlanCategory:
  case Opening, Attack, Positional, Structural, Endgame, Defensive, Transition

object Plan:

  case class OpeningDevelopment(color: Color) extends Plan:
    val id = PlanId.OpeningDevelopment
    val category = PlanCategory.Opening

  case class KingsideAttack(color: Color) extends Plan:
    val id = PlanId.KingsideAttack
    val category = PlanCategory.Attack

  case class QueensideAttack(color: Color) extends Plan:
    val id = PlanId.QueensideAttack
    val category = PlanCategory.Attack

  case class CentralBreakthrough(color: Color) extends Plan:
    val id = PlanId.CentralBreakthrough
    val category = PlanCategory.Attack

  case class PawnStorm(color: Color) extends Plan:
    val id = PlanId.PawnStorm
    val category = PlanCategory.Attack

  case class PerpetualCheck(color: Color) extends Plan:
    val id = PlanId.PerpetualCheck
    val category = PlanCategory.Attack

  case class DirectMate(color: Color) extends Plan:
    val id = PlanId.DirectMate
    val category = PlanCategory.Attack

  case class CentralControl(color: Color) extends Plan:
    val id = PlanId.CentralControl
    val category = PlanCategory.Positional

  case class PieceActivation(color: Color) extends Plan:
    val id = PlanId.PieceActivation
    val category = PlanCategory.Positional

  case class MinorPieceManeuver(color: Color) extends Plan:
    val id = PlanId.MinorPieceManeuver
    val category = PlanCategory.Positional

  case class RookActivation(color: Color) extends Plan:
    val id = PlanId.RookActivation
    val category = PlanCategory.Positional

  case class FileControl(color: Color) extends Plan:
    val id = PlanId.FileControl
    val category = PlanCategory.Positional

  case class PassedPawnCreation(color: Color) extends Plan:
    val id = PlanId.PassedPawnCreation
    val category = PlanCategory.Structural

  case class PassedPawnPush(color: Color) extends Plan:
    val id = PlanId.PassedPawnPush
    val category = PlanCategory.Structural

  case class WeakPawnAttack(color: Color) extends Plan:
    val id = PlanId.WeakPawnAttack
    val category = PlanCategory.Structural

  case class PawnBreakPreparation(color: Color) extends Plan:
    val id = PlanId.PawnBreakPreparation
    val category = PlanCategory.Structural

  case class KingActivation(color: Color) extends Plan:
    val id = PlanId.KingActivation
    val category = PlanCategory.Endgame

  case class Opposition(color: Color) extends Plan:
    val id = PlanId.Opposition
    val category = PlanCategory.Endgame

  case class Triangulation(color: Color) extends Plan:
    val id = PlanId.Triangulation
    val category = PlanCategory.Endgame

  case class Promotion(color: Color) extends Plan:
    val id = PlanId.Promotion
    val category = PlanCategory.Endgame

  case class Fortress(color: Color) extends Plan:
    val id = PlanId.Fortress
    val category = PlanCategory.Endgame

  case class DefensiveConsolidation(color: Color) extends Plan:
    val id = PlanId.DefensiveConsolidation
    val category = PlanCategory.Defensive

  case class Prophylaxis(color: Color) extends Plan:
    val id = PlanId.Prophylaxis
    val category = PlanCategory.Defensive

  case class Exchange(color: Color) extends Plan:
    val id = PlanId.Exchange
    val category = PlanCategory.Defensive

  case class Counterplay(color: Color) extends Plan:
    val id = PlanId.Counterplay
    val category = PlanCategory.Defensive

  case class Simplification(color: Color) extends Plan:
    val id = PlanId.Simplification
    val category = PlanCategory.Transition

  case class QueenTrade(color: Color) extends Plan:
    val id = PlanId.QueenTrade
    val category = PlanCategory.Transition

  case class Blockade(color: Color) extends Plan:
    val id = PlanId.Blockade
    val category = PlanCategory.Structural

  case class PawnChain(color: Color) extends Plan:
    val id = PlanId.PawnChain
    val category = PlanCategory.Structural

  case class MinorityAttack(color: Color) extends Plan:
    val id = PlanId.MinorityAttack
    val category = PlanCategory.Structural

  case class SpaceAdvantage(color: Color) extends Plan:
    val id = PlanId.SpaceAdvantage
    val category = PlanCategory.Positional

  case class Zugzwang(color: Color) extends Plan:
    val id = PlanId.Zugzwang
    val category = PlanCategory.Endgame

  case class Sacrifice(color: Color) extends Plan:
    val id = PlanId.Sacrifice
    val category = PlanCategory.Attack

enum PlanId:
  // Opening
  case OpeningDevelopment
  // Attack
  case KingsideAttack, QueensideAttack, CentralBreakthrough, PawnStorm, PerpetualCheck, DirectMate, Sacrifice
  // Positional
  case CentralControl, PieceActivation, MinorPieceManeuver, RookActivation, FileControl, SpaceAdvantage
  // Structural
  case PassedPawnCreation, PassedPawnPush, WeakPawnAttack, PawnBreakPreparation, Blockade, PawnChain, MinorityAttack
  // Endgame
  case KingActivation, Opposition, Triangulation, Promotion, Fortress, Zugzwang
  // Defensive
  case DefensiveConsolidation, Prophylaxis, Exchange, Counterplay
  // Transition
  case Simplification, QueenTrade

object PlanId:
  def fromString(raw: String): Option[PlanId] =
    values.find(_.toString.equalsIgnoreCase(Option(raw).getOrElse("").trim))
