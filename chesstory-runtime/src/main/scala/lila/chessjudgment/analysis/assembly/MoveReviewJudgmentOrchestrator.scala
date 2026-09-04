package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

object MoveReviewJudgmentOrchestrator:

  final class ReviewExecution private[assembly] (
      val assembly: JudgmentAssemblyContext
  ):
    lazy val assessmentPacket: Option[EvidenceBackedJudgmentPacket] =
      EvidenceBackedJudgmentPacket.fromAssembly(assembly)

  /** Preserves the admitted lower assembly whether or not a public assessment can be projected. */
  def executePreparedReview(
      preparedInput: AdmittedMoveReviewInput
  ): Option[ReviewExecution] =
    NodeLineTransitionAssembler
      .assemble(preparedInput)
      .map(EvidenceFactAssembler.enrich)
      .map(RelativeAssessmentAssembler.enrichComparisonEvidence)
      .map(RelativeAssessmentAssembler.enrichAssessment)
      .map(new ReviewExecution(_))
