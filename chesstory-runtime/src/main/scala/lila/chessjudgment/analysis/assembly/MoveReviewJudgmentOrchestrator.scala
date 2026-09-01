package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

object MoveReviewJudgmentOrchestrator:

  final case class ReviewExecution(
      packet: EvidenceBackedJudgmentPacket,
      preparedInput: AdmittedMoveReviewInput
  )

  /** Development-only direct v6 entrypoint; player jobs enter through executePreparedReview. */
  def execute(raw: RawMoveReviewInput): Option[EvidenceBackedJudgmentPacket] =
    MoveReviewInputAdmission.admit(raw).flatMap { prepared =>
      executePreparedReview(
        prepared,
        Some(ExplanationRequest.forObservedMove(prepared))
      )
    }.map(_.packet)

  /** Sole fact, Assessment, requested Explanation, and packet authority for an admitted review. */
  def executePreparedReview(
      preparedInput: AdmittedMoveReviewInput,
      explanationRequest: Option[ExplanationRequest]
  ): Option[ReviewExecution] =
    for
      base <- NodeLineTransitionAssembler
        .assemble(preparedInput)
        .map(EvidenceFactAssembler.enrich)
      explanationDemand = explanationRequest.flatMap(
        OccurrenceExplanationDemand.resolve(base, _)
      )
      proofContext = explanationDemand
        .map(demand =>
          OccurrenceExplanationAssembler.enrichProofs(
            base,
            JudgmentProvenanceAllocator.forInput(preparedInput),
            demand
          )
        )
        .getOrElse(base)
      comparisonContext = RelativeAssessmentAssembler.enrichComparisonEvidence(proofContext)
      _ <- Option.when(
        evidenceClosed(comparisonContext) && preRelativeAssessmentPayloadsClosed(comparisonContext)
      )((): Unit)
      assessmentContext = RelativeAssessmentAssembler.enrichAssessment(comparisonContext)
      completedContext = explanationDemand
        .map(demand =>
          OccurrenceExplanationAssembler.enrichCauses(
            assessmentContext,
            JudgmentProvenanceAllocator.forInput(preparedInput),
            demand
          )
        )
        .getOrElse(assessmentContext)
      packet <- EvidenceBackedJudgmentPacket.fromAssembly(completedContext)
    yield ReviewExecution(packet, preparedInput)

  private def preRelativeAssessmentPayloadsClosed(context: JudgmentAssemblyContext): Boolean =
    context.evidenceGraph.records.forall(record => !lateStagePayload(record.payload))

  private def lateStagePayload(payload: EvidencePayload): Boolean =
    payload match
      case _: RelativeAssessmentEvidence => true
      case _: OccurrenceExplanationCauseEvidence => true
      case _                              => false

  private def evidenceClosed(context: JudgmentAssemblyContext): Boolean =
    val records = context.evidenceGraph.records
    val recordIds = records.map(_.ref.id)
    val recordsById = records.map(record => record.ref.id -> record).toMap
    val positionRefs = context.positions.map(_.ref).toSet
    val legalLineRefs = context.legalLines.map(_.ref).toSet
    def registered(ref: EvidenceRef): Boolean =
      recordsById.get(ref.id).exists(_.ref == ref)
    context.root.exists(root => root.fen == context.input.beforeFen && root.ply == context.input.beforePly) &&
    records.nonEmpty &&
    context.legalLines.nonEmpty &&
    legalLineRefs.size == context.legalLines.size &&
    context.legalLines.map(_.evidence.id).distinct.size == context.legalLines.size &&
    recordIds.distinct.size == recordIds.size &&
    records.forall(record =>
      positionRefs(record.ref.position) &&
        record.ref.line.forall(legalLineRefs) &&
        record.payloadLineRefs.forall(legalLineRefs) &&
        record.parents.forall(registered)
    ) &&
    context.legalLines.forall(line => registered(line.evidence)) &&
    context.lines.forall(candidate =>
      context.legalLines.exists(owner =>
        owner.ref == candidate.ref && owner.evidence == candidate.lineEvidence
      )
    ) &&
    context.transitions.forall(transition =>
      positionRefs(transition.from) &&
        positionRefs(transition.to) &&
        registered(transition.evidence)
    )
