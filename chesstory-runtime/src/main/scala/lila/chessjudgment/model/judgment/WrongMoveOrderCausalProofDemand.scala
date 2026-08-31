package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.evaluation.JudgmentThresholds

/** Sole upper-consumer demand gate shared by the bounded causal families
  * selected for WrongMoveOrder. Evaluation activates dispatch but proves no
  * L2 proposition.
  */
private[chessjudgment] object WrongMoveOrderCausalProofDemand:
  def accepts(fact: CandidateComparisonFact): Boolean =
    fact.kind == CandidateComparisonKind.PlayedVsBest &&
      fact.referenceLine.role == LineNodeRole.BestReference &&
      fact.candidateLine.role == LineNodeRole.Played &&
      fact.hasDistinctRootMoves && fact.comparison.verdict.isActionableLoss &&
      fact.comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP

  def acceptsRecord(
      source: EvidenceRecord,
      fact: CandidateComparisonFact,
      root: PositionNodeRef,
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord
  ): Boolean =
    accepts(fact) && CandidateComparisonRecordAuthority.exactShape(
      source,
      fact,
      root,
      referenceSource,
      playedSource
    )
