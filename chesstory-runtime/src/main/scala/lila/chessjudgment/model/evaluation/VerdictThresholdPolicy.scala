package lila.chessjudgment.model.evaluation

import lila.chessjudgment.model.judgment.MoveChoiceVerdict
import lila.chessjudgment.model.judgment.CandidateSetType

object VerdictThresholdPolicy:

  /** Versioned authority for audit projections that compare a public verdict
    * with a fresh classification from the registered comparison lines.
    */
  val PolicyVersion = "chesstory.verdict-threshold-policy.v1"

  def verdictFromWinPercent(
      candidateWinPercentDeltaForMover: Double,
      winPercentLossForMover: Double,
      rawCpLossForMover: Int = 0,
      mateDistanceLossForMover: Option[Int] = None,
      candidateSetType: Option[CandidateSetType] = None
  ): MoveChoiceVerdict =
    if candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP then MoveChoiceVerdict.ImprovesOnReference
    else if mateDistanceLossForMover.exists(_ > 0) then MoveChoiceVerdict.PlayableLoss
    else if winPercentLossForMover == 0.0 then MoveChoiceVerdict.MatchesReference
    else if candidateSetType.contains(CandidateSetType.StyleChoice) &&
        winPercentLossForMover <= JudgmentThresholds.STYLE_CHOICE_SPREAD_WP
    then MoveChoiceVerdict.PlayableLoss
    else if winPercentLossForMover <= JudgmentThresholds.PLAYABLE_LOSS_WP &&
        rawCpLossForMover >= JudgmentThresholds.DECISIVE_CP_DEGRADATION
    then MoveChoiceVerdict.Inaccuracy
    else if winPercentLossForMover <= JudgmentThresholds.PLAYABLE_LOSS_WP then MoveChoiceVerdict.PlayableLoss
    else if winPercentLossForMover <= JudgmentThresholds.INACCURACY_WP then MoveChoiceVerdict.Inaccuracy
    else if winPercentLossForMover < JudgmentThresholds.BLUNDER_WP then MoveChoiceVerdict.Mistake
    else MoveChoiceVerdict.Blunder
