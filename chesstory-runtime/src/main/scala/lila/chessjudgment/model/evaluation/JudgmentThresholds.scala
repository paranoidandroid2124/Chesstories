package lila.chessjudgment.model.evaluation

/**
 * Centralized threshold constants for analysis logic.
 */
object JudgmentThresholds:

  val ENGINE_BACKED_DEPTH = 8

  val PLAYABLE_LOSS_WP = 2.5
  val DECISIVE_CP_DEGRADATION = 500

  val ONLY_MOVE_GAP_WP = 10.0
  val STYLE_CHOICE_SPREAD_WP = 3.0

  /** WinPercent difference thresholds */
  val BLUNDER_WP = 20.0
  val INACCURACY_WP = 5.0
  val TACTICAL_IMPACT_WP = 2.5
  val MATERIAL_IMPACT_WP = 5.0

  def engineBackedByDepth(depth: Int, mate: Option[Int]): Boolean =
    mate.nonEmpty || depth >= ENGINE_BACKED_DEPTH
