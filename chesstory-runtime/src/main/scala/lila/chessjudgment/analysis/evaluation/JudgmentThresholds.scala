package lila.chessjudgment.analysis.evaluation

/**
 * Centralized threshold constants for analysis logic.
 */
object JudgmentThresholds:

  val ENGINE_BACKED_DEPTH = 8

  val PLAYABLE_LOSS_WP = 2.5
  val DECISIVE_CP_DEGRADATION = 500

  val ONLY_MOVE_GAP_WP = 10.0
  val STYLE_CHOICE_SPREAD_WP = 3.0

  val THREAT_RELIABLE_DEPTH = 16

  /** WinPercent difference thresholds */
  val BLUNDER_WP = 20.0
  val INACCURACY_WP = 5.0
  val SIGNIFICANT_THREAT_WP = 2.5
  val MATERIAL_THREAT_WP = 5.0
  val URGENT_THREAT_WP = 15.0
  val IGNORABLE_THREAT_WP = 2.0
  val ONLY_DEFENSE_TOLERANCE_WP = 1.25
  val DRAW_RESOURCE_BALANCE_EDGE_WP = 7.5
  val CRITICAL_CANDIDATE_GAP_WP = 5.0
  val FORCING_CANDIDATE_GAP_WP = 10.0
  val CONVERSION_EDGE_WP = 15.0
  val DECISIVE_EDGE_WP = 25.0
  val DECISIVE_CANDIDATE_FAILURE_WP = 35.0
  val MATERIAL_CANDIDATE_FAILURE_WP = 20.0
  val POSITION_COLLAPSE_WIN_PERCENT = 32.0

  def engineBackedByDepth(depth: Int, mate: Option[Int]): Boolean =
    mate.nonEmpty || depth >= ENGINE_BACKED_DEPTH
