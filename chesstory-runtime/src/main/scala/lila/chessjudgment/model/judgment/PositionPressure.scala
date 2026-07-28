package lila.chessjudgment.model.judgment

import chess.{ Color, Role, Square }
import lila.chessjudgment.model.Motif

/**
 * Type of threat detected.
 */
enum ThreatKind:
  case Mate        // Checkmate threat
  case Material    // Material or line-backed loss threat
  case Positional  // Positional disadvantage

/**
 * Urgency level for defense.
 */
enum ThreatSeverity:
  case Urgent
  case Important
  case Low

enum ThreatDriver:
  case MateThreat
  case MaterialThreat
  case PositionalThreat

/**
 * Single threat detected in the position.
 */
case class Threat(
  threatActor: Color,           // Side posing the threat
  kind: ThreatKind,
  turnsToImpact: Int,           // Moves until threat materializes (1 = immediate)
  motifs: List[Motif],          // Typed tactical or positional motif anchors
  attackSquares: List[String],  // Squares being attacked
  targetPieces: List[String],   // Pieces under threat
  bestDefense: Option[String],  // Defensive move UCI when available
  defenseCount: Int             // Number of adequate defenses
):
  def sideUnderPressure: Color = !threatActor
  def isStrategic: Boolean = turnsToImpact >= 3
  def severity: ThreatSeverity =
    kind match
      case ThreatKind.Mate       => ThreatSeverity.Urgent
      case ThreatKind.Material   => ThreatSeverity.Important
      case ThreatKind.Positional =>
        if isStrategic then ThreatSeverity.Important
        else ThreatSeverity.Low

/**
 * Tension resolution policy recommendation.
 * Concept 10: tensionPolicy
 */
enum TensionPolicy:
  case Maintain   // Keep tension, don't resolve yet
  case Release    // Resolve tension now (capture/advance)
  case Ignore     // No significant tension to manage

enum PawnPlayDriver:
  case BreakReady
  case PassedPawn
  case Defensive
  case TensionCritical
  case TensionActive
  case Quiet

/**
 * Board-backed pawn-play facts used downstream.
 */
final case class PawnPlayAnalysis private[chessjudgment] (
  breakFile: Option[String],

  advanceOrCapture: Boolean,

  blockadeSquare: Option[Square],
  blockadeRole: Option[Role],

  minorityAttack: Boolean,

  tensionPolicy: TensionPolicy,

  primaryDriver: PawnPlayDriver,
  tensionEdges: List[String] = Nil,      // Pawn tension as origin-target edges (e.g., "d4-e5")
  counterBreakFiles: List[String] = Nil  // Files where opponent pawn breaks are available
):
  def pawnBreakReady: Boolean = breakFile.nonEmpty
  def counterBreak: Boolean = counterBreakFiles.nonEmpty
  def tensionSquares: List[String] =
    tensionEdges
      .flatMap(_.split("-", 2).lift(1))
      .filter(_.matches("""[a-h][1-8]"""))
      .distinct
      .sorted
