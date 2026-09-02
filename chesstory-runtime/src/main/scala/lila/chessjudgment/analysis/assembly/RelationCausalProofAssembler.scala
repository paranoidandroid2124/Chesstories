package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Sole graph producer for the four relation-backed causal families. Each
  * family orients exact roots from its own lower facts; only deterministic
  * graph ownership is shared here.
  */
private[chessjudgment] object RelationCausalProofAssembler:

  private[assembly] def fromDemand(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    val records =
      uniqueCheckReply(allocator, demand) ++
        soleRecapturerRemoval(allocator, demand) ++
        vacatedGateCapture(allocator, demand) ++
        squareReleaseRoute(allocator, demand)
    require(
      records.map(_.ref.id).distinct.size == records.size,
      "one relation-causal evidence id may have only one graph owner"
    )
    records.sortBy(_.ref.id)

  private def uniqueCheckReply(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    val certified = UniqueCheckReplyProofDemand.from(demand).flatMap { exactDemand =>
      UniqueCheckReplyDefenderDisplacementBeforeCaptureProof.certifyDemanded(
        exactDemand.subject,
        exactDemand.displacementBranch,
        exactDemand.immediateCaptureBranch,
        exactDemand.lower
      )
    }
    OccurrenceExplanationAssembler.occurrenceProofRecords(
      family = BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture,
      allocator = allocator,
      subject = demand.subject
    )(certified)(
      semanticId = _.semantic.semanticId,
      occurrenceId = _.occurrence.occurrenceId,
      dependencyId = _.dependency.value,
      proofPathIds = _.occurrence.proofPaths.map(_.pathOccurrenceId),
      parentSources = _.proof.parentSources,
      payload = exact =>
        UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence(
          exact.semantic,
          exact.occurrence,
          demand.subject.publicOccurrence,
          exact.dependency.value,
          exact.proof
        )
    )

  private def soleRecapturerRemoval(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    val certified = SoleRecapturerRemovalProofDemand.from(demand).flatMap { exactDemand =>
      SoleRecapturerRemovalBeforeTargetCaptureProof.certifyDemanded(
        exactDemand.subject,
        exactDemand.removalBranch,
        exactDemand.immediateCaptureBranch,
        exactDemand.lower
      )
    }
    OccurrenceExplanationAssembler.occurrenceProofRecords(
      family = BoundedCausalContractKind.SoleRecapturerRemovalBeforeTargetCapture,
      allocator = allocator,
      subject = demand.subject
    )(certified)(
      semanticId = _.semantic.semanticId,
      occurrenceId = _.occurrence.occurrenceId,
      dependencyId = _.dependency.value,
      proofPathIds = _.occurrence.proofPaths.map(_.pathOccurrenceId),
      parentSources = _.parentSources,
      payload = exact =>
        SoleRecapturerRemovalBeforeTargetCaptureEvidence(
          exact.semantic,
          exact.occurrence,
          demand.subject.publicOccurrence,
          exact.dependency.value,
          exact
        )
    )

  private def vacatedGateCapture(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    val certified = VacatedGateCaptureProofDemand.from(demand).flatMap { exactDemand =>
      VacatedGateEnablesUnrecapturableSliderCaptureProof.certifyDemanded(
        exactDemand.subject,
        exactDemand.vacatedGateBranch,
        exactDemand.retainedGateBranch,
        exactDemand.lowers
      )
    }
    OccurrenceExplanationAssembler.occurrenceProofRecords(
      family = BoundedCausalContractKind.VacatedGateEnablesUnrecapturableSliderCapture,
      allocator = allocator,
      subject = demand.subject
    )(certified)(
      semanticId = _.semantic.semanticId,
      occurrenceId = _.occurrence.occurrenceId,
      dependencyId = _.dependency.value,
      proofPathIds = _.occurrence.proofPaths.map(_.pathOccurrenceId),
      parentSources = _.parentSources,
      payload = exact =>
        VacatedGateEnablesUnrecapturableSliderCaptureEvidence(
          exact.semantic,
          exact.occurrence,
          demand.subject.publicOccurrence,
          exact.dependency.value,
          exact
        )
    )

  private def squareReleaseRoute(
      allocator: JudgmentProvenanceAllocator,
      demand: OccurrenceExplanationDemand
  ): List[EvidenceRecord] =
    val certified = SquareReleaseRouteProofDemand.from(demand).flatMap { exactDemand =>
      SquareReleaseRouteProof.certifyDemanded(
        exactDemand.subject,
        exactDemand.releasedSquareBranch,
        exactDemand.retainedSquareBranch,
        exactDemand.lowers
      )
    }
    OccurrenceExplanationAssembler.occurrenceProofRecords(
      family = BoundedCausalContractKind.SquareReleaseRoute,
      allocator = allocator,
      subject = demand.subject
    )(certified)(
      semanticId = _.semantic.semanticId,
      occurrenceId = _.occurrence.occurrenceId,
      dependencyId = _.dependency.value,
      proofPathIds = _.occurrence.proofPaths.map(_.pathOccurrenceId),
      parentSources = _.parentSources,
      payload = exact =>
        SquareReleaseRouteEvidence(
          exact.semantic,
          exact.occurrence,
          demand.subject.publicOccurrence,
          exact.dependency.value,
          exact
        )
    )
