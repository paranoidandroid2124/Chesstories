package lila.chessjudgment.model.judgment

/** Sole authority for binding a graph record to one certified line occurrence.
  * It is independent of the evaluation policy that may demand an L2 proof.
  */
private[chessjudgment] object CertifiedComparedLineAuthority:
  def exactRecord(
      source: EvidenceRecord,
      line: LineNodeRef,
      replay: CanonicalLineReplay
  ): Boolean =
    CertifiedLineReplayRecord.from(source).exists(authority =>
      authority.line == line && authority.replay == replay
    )

/** Sole upper-consumer demand gate for bounded causal proof families attached
  * to an actionable PlayedVsBest comparison. Evaluation activates dispatch
  * but proves no L2 proposition.
  */
private[chessjudgment] object ActionablePlayedVsBestCausalProofDemand:

  def accepts(fact: CandidateComparisonFact): Boolean =
    fact.kind == CandidateComparisonKind.PlayedVsBest &&
      fact.referenceLine.role == LineNodeRole.BestReference &&
      fact.candidateLine.role == LineNodeRole.Played &&
      fact.hasDistinctRootMoves && fact.comparison.verdict.isActionableLoss

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
