package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.{ AdmittedMoveReviewInput, JudgmentAssemblyContext }

object MoveReviewAssembler:

  /** Preserves the admitted occurrence assembly independently of public assessment projection. */
  def assemble(
      preparedInput: AdmittedMoveReviewInput
  ): Option[JudgmentAssemblyContext] =
    NodeLineTransitionAssembler
      .assemble(preparedInput)
      .map(EvidenceFactAssembler.enrich)
      .map(RelativeAssessmentAssembler.enrichComparisonEvidence)
      .map(RelativeAssessmentAssembler.enrichAssessment)
