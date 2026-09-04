package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.{ AdmittedMoveReviewInput, JudgmentAssemblyContext }

object MoveReviewAssembler:

  /** Preserves the admitted occurrence assembly independently of public assessment projection. */
  def assemble(
      preparedInput: AdmittedMoveReviewInput
  ): JudgmentAssemblyContext =
    RelativeAssessmentAssembler.enrichAssessment(
      RelativeAssessmentAssembler.enrichComparisonEvidence(
        EvidenceFactAssembler.enrich(
          NodeLineTransitionAssembler.assemble(preparedInput)
        )
      )
    )
