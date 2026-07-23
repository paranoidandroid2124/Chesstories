package lila.chessjudgment.analysis.singlePosition

import chess.{Square, Role}
import lila.chessjudgment.analysis.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.Motif

/**
 * Node-local container for single-position assessment results.
 * Each field contains both the assessment and its evidence.
 */
case class SinglePositionAssessment(
  nature: NatureResult,
  criticality: CriticalityResult,
  candidateSet: CandidateSetTopology,
  gamePhase: GamePhaseResult,
  simplifyBias: SimplifyBiasResult,
  drawBias: DrawBiasResult,
  riskProfile: RiskProfileResult,
  judgmentFocus: JudgmentFocusResult
)
// 1. Nature (Static/Dynamic/Chaos)

enum NatureType:
  case Static, Dynamic, Chaos

case class NatureResult(
  natureType: NatureType,
  // Evidence
  tensionScore: Int,        // Number of pawn tensions
  openFilesCount: Int,      // Open files count
  mobilityDiff: Int,        // Mobility difference (white - black)
  lockedCenter: Boolean     // Center pawns locked
):
  def isStatic: Boolean = natureType == NatureType.Static
  def isDynamic: Boolean = natureType == NatureType.Dynamic
  def isChaos: Boolean = natureType == NatureType.Chaos
// 2. Criticality (Normal/Critical/Forced)

enum CriticalityType:
  case InsufficientData, Normal, CriticalMoment, ForcedSequence

case class CriticalityResult(
  criticalityType: CriticalityType,
  // Evidence
  rawSideRelativeEvalDeltaCpForDiagnostics: Option[Int],
  evalDeltaWinPercent: Option[Double],
  mateDistance: Option[Int],    // Mate distance if applicable
  forcingMovesInPv: Int         // Count of checks/captures in PV
):
  def isNormal: Boolean = criticalityType == CriticalityType.Normal
  def isCritical: Boolean = criticalityType == CriticalityType.CriticalMoment
  def isForced: Boolean = criticalityType == CriticalityType.ForcedSequence
// Candidate set topology

enum CandidateSetType:
  case OnlyMove, NarrowChoice, StyleChoice

enum CandidateFailureMode:
  case InsufficientData
  case DecisiveEvaluationLoss
  case LosesMaterial
  case PositionCollapses
  case SignificantDisadvantage

case class CandidateSetTopology(
  candidateSetType: CandidateSetType,
  // Evidence
  bestLineSideRelativeEvalCp: Option[Int],
  secondLineSideRelativeEvalCp: Option[Int],
  thirdLineSideRelativeEvalCp: Option[Int],
  gapBestToSecondWp: Option[Double],
  spreadTop3Wp: Option[Double],
  secondCandidateFailure: Option[CandidateFailureMode]
):
  def isOnlyMove: Boolean = candidateSetType == CandidateSetType.OnlyMove
  def isNarrowChoice: Boolean = candidateSetType == CandidateSetType.NarrowChoice
  def isStyleChoice: Boolean = candidateSetType == CandidateSetType.StyleChoice
// 4. Game Phase (Opening/Middlegame/Endgame)

enum GamePhaseType:
  case Opening, Middlegame, Endgame

case class GamePhaseResult(
  phaseType: GamePhaseType,
  // Evidence
  totalMaterial: Int,           // Total material on board
  queensOnBoard: Boolean,       // Whether queens are present
  minorPiecesCount: Int         // Total minor pieces
):
  def isOpening: Boolean = phaseType == GamePhaseType.Opening
  def isMiddlegame: Boolean = phaseType == GamePhaseType.Middlegame
  def isEndgame: Boolean = phaseType == GamePhaseType.Endgame
// 5. Simplify Bias (Window for simplification)

case class SimplifyBiasResult(
  isSimplificationWindow: Boolean,
  // Evidence
  rawEvalAdvantageCpForDiagnostics: Int,
  evalAdvantageWinPercent: Double,
  isEndgameNear: Boolean,       // Phase is endgame or near
  exchangeAvailable: Boolean    // Queen/Rook exchange possible
):
  def shouldSimplify: Boolean = isSimplificationWindow
// 6. Draw Bias (Drawish tendency)

case class DrawBiasResult(
  isDrawish: Boolean,
  // Evidence
  materialSymmetry: Boolean,    // Material is roughly equal
  oppositeColorBishops: Boolean,// Opposite color bishops
  fortressLikely: Boolean,      // Structural fortress possible
  insufficientMaterial: Boolean // Insufficient mating material
):
  def tendsToDraw: Boolean = isDrawish
// 7. Risk Profile (Volatility proxy)

enum RiskLevel:
  case Low, Medium, High

case class RiskProfileResult(
  riskLevel: RiskLevel,
  // Evidence
  evalVolatility: Int,          // Eval swing across depths
  tacticalMotifsCount: Int,     // Number of tactical motifs
  kingExposureSum: Int          // Sum of both kings' exposure
):
  def isLowRisk: Boolean = riskLevel == RiskLevel.Low
  def isMediumRisk: Boolean = riskLevel == RiskLevel.Medium
  def isHighRisk: Boolean = riskLevel == RiskLevel.High
// 8. Judgment focus

enum JudgmentFocusType:
  case Plan
  case Tactics
  case Defense
  case Conversion

enum JudgmentDriver:
  case ForcedSequence
  case ConversionOpportunity
  case TacticalComplexity
  case ChaoticPosition
  case StrategicStructure
  case DynamicPosition

case class JudgmentFocusResult(
  focus: JudgmentFocusType,
  // Evidence
  primaryDriver: JudgmentDriver
):
  def isPlanMode: Boolean = focus == JudgmentFocusType.Plan
  def isTacticsMode: Boolean = focus == JudgmentFocusType.Tactics
  def isDefenseMode: Boolean = focus == JudgmentFocusType.Defense
  def isConvertMode: Boolean = focus == JudgmentFocusType.Conversion

/**
 * Represents a single PV line from engine analysis.
 * Used as input to SinglePositionAssessor.
 */
case class PvLine(
  moves: List[String],  // UCI moves in the line
  sideRelativeEvalCp: Int, // Centipawn evaluation, positive for the side to move
  mate: Option[Int],    // Mate distance (positive = winning)
  depth: Int            // Search depth
):
  def winPercentForSideToMove: Double =
    PerspectiveMath.winPercentFromRelativeEval(sideRelativeEvalCp, mate)
  def winPercentLossTo(candidate: PvLine): Double =
    PerspectiveMath.winPercentLossFromRelativeEval(
      sideRelativeEvalCp,
      mate,
      candidate.sideRelativeEvalCp,
      candidate.mate
    )

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
  case NoThreat

enum ThreatEvidenceSource:
  case MotifPattern
  case CandidateLineValueDelta
  case MotifAndLineValueDelta

/**
 * Single threat detected in the position.
 */
case class Threat(
  kind: ThreatKind,
  lossIfIgnoredCp: Int,         // Estimated loss if ignored (centipawns)
  lossIfIgnoredWinPercent: Option[Double],
  turnsToImpact: Int,           // Moves until threat materializes (1 = immediate)
  evidenceSource: ThreatEvidenceSource,
  motifs: List[Motif],          // Typed tactical or positional motif anchors
  attackSquares: List[String],  // Squares being attacked
  targetPieces: List[String],   // Pieces under threat
  bestDefense: Option[String],  // Defensive move UCI when available
  defenseCount: Int             // Number of adequate defenses
):
  def isImmediate: Boolean = turnsToImpact <= 2
  def isStrategic: Boolean = turnsToImpact >= 3
  def severity: ThreatSeverity =
    if kind == ThreatKind.Mate ||
        lossIfIgnoredWinPercent.exists(_ >= JudgmentThresholds.URGENT_THREAT_WP)
    then ThreatSeverity.Urgent
    else if lossIfIgnoredWinPercent.exists(_ >= JudgmentThresholds.MATERIAL_THREAT_WP)
    then ThreatSeverity.Important
    else ThreatSeverity.Low

/**
 * Assessment of defensive options.
 */
case class DefenseAssessment(
  necessity: ThreatSeverity,
  onlyDefense: Option[String],       // UCI if only one adequate defense exists
  alternatives: List[String],        // Alternative defensive moves
  counterIsBetter: Boolean,          // Counter-attack is superior to defense
  prophylaxisNeeded: Boolean,        // Prophylactic defense needed
  resourceCoverageScore: Int         // Defensive resource score (0-100)
)

/**
 * Complete threat analysis result for a position.
 * Contains both individual threats and aggregate flags.
 */
case class ThreatAnalysis(
  // Individual threats
  threats: List[Threat],
  
  // Defense assessment
  defense: DefenseAssessment,
  
  // Aggregate flags (for quick routing)
  threatSeverity: ThreatSeverity,
  immediateThreat: Boolean,          // Any threat with turnsToImpact <= 2
  strategicThreat: Boolean,          // Any threat with turnsToImpact >= 3  
  threatIgnorable: Boolean,          // Safe to ignore all threats
  defenseRequired: Boolean,          // Must defend
  counterThreatBetter: Boolean,      // Counter-attack preferred
  prophylaxisNeeded: Boolean,        // Prophylaxis needed
  resourceAvailable: Boolean,        // Adequate defensive resources
  
  maxLossIfIgnored: Int,             // Highest lossIfIgnoredCp among threats
  maxWinPercentLossIfIgnored: Option[Double],
  primaryDriver: ThreatDriver,
  insufficientData: Boolean          // True if MultiPV was inadequate
):
  def hasThreat: Boolean = threats.nonEmpty
  def threatCount: Int = threats.size
  def hasForcedMateEvidence: Boolean =
    primaryDriver == ThreatDriver.MateThreat ||
      threats.exists(_.kind == ThreatKind.Mate)
  def isProofSignalDefensivePressure: Boolean =
    (!insufficientData || hasForcedMateEvidence) &&
      (
        defenseRequired ||
          prophylaxisNeeded ||
          threatSeverity != ThreatSeverity.Low ||
          maxWinPercentLossIfIgnored.exists(_ >= JudgmentThresholds.SIGNIFICANT_THREAT_WP)
      )

/**
 * Tension resolution policy recommendation.
 * Concept 10: tensionPolicy
 */
enum TensionPolicy:
  case Maintain   // Keep tension, don't resolve yet
  case Release    // Resolve tension now (capture/advance)
  case Ignore     // No significant tension to manage

/**
 * Passed pawn advancement urgency.
 * Concept 5: passedPawnUrgency
 */
enum PassedPawnUrgency:
  case Critical   // Must push NOW (rank 6-7, near promotion)
  case Important  // Should prioritize pushing
  case Background // Strategic asset, can wait
  case Blocked    // Cannot advance without support

enum PawnPlayDriver:
  case BreakReady
  case PassedPawn
  case Defensive
  case TensionCritical
  case TensionActive
  case Quiet

/**
 * Complete break and pawn-play analysis result.
 * Contains all 10 concepts for Break & Pawn Play.
 */
case class PawnPlayAnalysis(
  // === Concept 1-3: Break Analysis ===
  pawnBreakReady: Boolean,              // Concept 1: A pawn break is immediately executable
  breakFile: Option[String],            // Concept 2: File where primary break exists
  breakImpact: Int,                     // Concept 3: Estimated cp impact of break (0-500)
  
  // === Concept 4: Tension Resolution ===
  advanceOrCapture: Boolean,            // Concept 4: Pawn tension MUST be resolved this move
  
  // === Concept 5-7: Passed Pawn Handling ===
  passedPawnUrgency: PassedPawnUrgency, // Concept 5: How urgent is passed pawn push
  passerBlockade: Boolean,              // Concept 6: Passed pawn is blockaded
  blockadeSquare: Option[Square],       // Specific square of blockade
  blockadeRole: Option[Role],           // Role of blockading piece
  pusherSupport: Boolean,               // Concept 7: Rook/King support for promotion
  
  // === Concept 8-9: Strategic Posture ===
  minorityAttack: Boolean,              // Concept 8: Queenside minority attack available
  counterBreak: Boolean,                // Concept 9: Opponent has ready counter-break
  
  // === Concept 10: Tension Policy ===
  tensionPolicy: TensionPolicy,         // Concept 10: Maintain/Release/Ignore recommendation
  
  tensionSquares: List[String],         // Squares with pawn tension (e.g., "d4", "e5")
  primaryDriver: PawnPlayDriver,
  tensionEdges: List[String] = Nil,      // Pawn tension as origin-target edges (e.g., "d4-e5")
  counterBreakFiles: List[String] = Nil  // Files where opponent pawn breaks are available
):
  def hasBreakOpportunity: Boolean = pawnBreakReady
  def needsPassedPawnAction: Boolean = passedPawnUrgency == PassedPawnUrgency.Critical
  def hasTensionToResolve: Boolean = tensionPolicy == TensionPolicy.Release
