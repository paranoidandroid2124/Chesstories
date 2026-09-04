package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

object MoveReviewJudgmentOrchestrator:

  final class ReviewExecution private[assembly] (
      val assembly: JudgmentAssemblyContext
  ):
    val judgment: Option[EvidenceBackedJudgmentPacket] =
      EvidenceBackedJudgmentPacket.fromAssembly(assembly)

  /** Development-only direct v6 entrypoint; player jobs enter through executePreparedReview. */
  def execute(raw: RawMoveReviewInput): Option[EvidenceBackedJudgmentPacket] =
    MoveReviewInputAdmission.admit(raw).flatMap { prepared =>
      executePreparedReview(prepared)
    }.flatMap(_.judgment)

  /** Preserves the admitted lower assembly whether or not it projects a player judgment. */
  def executePreparedReview(
      preparedInput: AdmittedMoveReviewInput
  ): Option[ReviewExecution] =
    NodeLineTransitionAssembler
      .assemble(preparedInput)
      .map(EvidenceFactAssembler.enrich)
      .map(RelativeAssessmentAssembler.enrichComparisonEvidence)
      .map(RelativeAssessmentAssembler.enrichAssessment)
      .map(new ReviewExecution(_))
