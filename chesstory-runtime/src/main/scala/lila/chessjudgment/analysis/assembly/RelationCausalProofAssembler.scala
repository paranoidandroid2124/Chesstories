package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Sole graph producer for typed compared-line relation proofs. Family proof
  * objects retain their chess-specific certification; this owner only binds
  * demanded certificates to the common graph/cache envelope.
  */
private[chessjudgment] object RelationCausalProofAssembler:

  private[assembly] def fromDemand(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    val records =
      uniqueCheckReply(context, allocator, demand) ++
        soleRecapturerRemoval(context, allocator, demand) ++
        vacatedGateCapture(context, allocator, demand) ++
        squareReleaseRoute(context, allocator, demand) ++
        captureExclusionMoveOrder(context, allocator, demand)
    require(
      records.map(_.ref.id).distinct.size == records.size,
      "one relation-causal evidence id may have only one graph owner"
    )
    records

  private def uniqueCheckReply(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    demand.uniqueCheckReplyDefenderDisplacementBeforeCaptureDemand.toList.flatMap { exactDemand =>
      val input = demand.input
      val family = "unique-check-reply-defender-displacement-before-capture"
      records(
        family,
        context,
        allocator,
        input,
        {
          case payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence
              if payload.consumesDependencies(input.referenceSource, input.playedSource) =>
            Some(payload.dependencyId -> payload.resultSet)
          case _ => None
        }
      )(
        UniqueCheckReplyDefenderDisplacementBeforeCaptureProof.certifyDemanded(
          input.comparison.referenceLine,
          input.comparison.candidateLine,
          input.referenceSource,
          input.playedSource,
          input.referenceReplay,
          input.playedReplay,
          exactDemand
        )
      )(
        semantic = _.semantic,
        record = exact => comparedLineRecord(
          exact.semantic.semanticId,
          exact.occurrence.occurrenceId,
          exact.dependency.value,
          exact.occurrence.referenceLine,
          exact.proof.parentSources,
          exact.occurrence.proofPaths
        ),
        payload = (exact, resultSet) =>
          UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence(
            exact.semantic,
            exact.occurrence,
            exact.dependency.value,
            resultSet,
            Some(exact.proof)
          )
      )
    }

  private def soleRecapturerRemoval(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    demand.soleRecapturerRemovalBeforeTargetCaptureDemand.toList.flatMap { exactDemand =>
      val input = demand.input
      val family = "sole-recapturer-removal-before-target-capture"
      records(
        family,
        context,
        allocator,
        input,
        {
          case payload: SoleRecapturerRemovalBeforeTargetCaptureEvidence
              if payload.consumesDependencies(input.referenceSource, input.playedSource) =>
            Some(payload.dependencyId -> payload.resultSet)
          case _ => None
        }
      )(
        SoleRecapturerRemovalBeforeTargetCaptureProof.certifyDemanded(
          input.comparison.referenceLine,
          input.comparison.candidateLine,
          input.referenceSource,
          input.playedSource,
          input.referenceReplay,
          input.playedReplay,
          exactDemand
        )
      )(
        semantic = _.semantic,
        record = exact => comparedLineRecord(
          exact.semantic.semanticId,
          exact.occurrence.occurrenceId,
          exact.dependency.value,
          exact.occurrence.referenceLine,
          exact.parentSources,
          exact.occurrence.proofPaths
        ),
        payload = (exact, resultSet) =>
          SoleRecapturerRemovalBeforeTargetCaptureEvidence(
            exact.semantic,
            exact.occurrence,
            exact.dependency.value,
            resultSet,
            Some(exact)
          )
      )
    }

  private def vacatedGateCapture(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    if demand.vacatedGateEnablesUnrecapturableSliderCaptureDemands.isEmpty then Nil
    else
      val input = demand.input
      val family = "vacated-gate-enables-unrecapturable-slider-capture"
      records(
        family,
        context,
        allocator,
        input,
        {
          case payload: VacatedGateEnablesUnrecapturableSliderCaptureEvidence
              if payload.consumesDependencies(input.referenceSource, input.playedSource) =>
            Some(payload.dependencyId -> payload.resultSet)
          case _ => None
        }
      )(
        VacatedGateEnablesUnrecapturableSliderCaptureProof.certifyDemanded(
          input.comparison.referenceLine,
          input.comparison.candidateLine,
          input.referenceSource,
          input.playedSource,
          input.referenceReplay,
          input.playedReplay,
          demand.vacatedGateEnablesUnrecapturableSliderCaptureDemands
        )
      )(
        semantic = _.semantic,
        record = exact => comparedLineRecord(
          exact.semantic.semanticId,
          exact.occurrence.occurrenceId,
          exact.dependency.value,
          exact.occurrence.referenceLine,
          exact.parentSources,
          exact.occurrence.proofPaths
        ),
        payload = (exact, resultSet) =>
          VacatedGateEnablesUnrecapturableSliderCaptureEvidence(
            exact.semantic,
            exact.occurrence,
            exact.dependency.value,
            resultSet,
            Some(exact)
          )
      )

  private def squareReleaseRoute(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    if demand.squareReleaseRouteDemands.isEmpty then Nil
    else
      val input = demand.input
      val family = "square-release-route"
      records(
        family,
        context,
        allocator,
        input,
        {
          case payload: SquareReleaseRouteEvidence
              if payload.consumesDependencies(input.referenceSource, input.playedSource) =>
            Some(payload.dependencyId -> payload.resultSet)
          case _ => None
        }
      )(
        SquareReleaseRouteProof.certifyDemanded(
          input.comparison.referenceLine,
          input.comparison.candidateLine,
          input.referenceSource,
          input.playedSource,
          input.referenceReplay,
          input.playedReplay,
          demand.squareReleaseRouteDemands
        )
      )(
        semantic = _.semantic,
        record = exact => comparedLineRecord(
          exact.semantic.semanticId,
          exact.occurrence.occurrenceId,
          exact.dependency.value,
          exact.occurrence.referenceLine,
          exact.parentSources,
          exact.occurrence.proofPaths
        ),
        payload = (exact, resultSet) =>
          SquareReleaseRouteEvidence(
            exact.semantic,
            exact.occurrence,
            exact.dependency.value,
            resultSet,
            Some(exact)
          )
      )

  private def captureExclusionMoveOrder(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    if demand.captureExclusionMoveOrderDemands.isEmpty then Nil
    else
      val input = demand.input
      val family = "capture-exclusion-move-order"
      records(
        family,
        context,
        allocator,
        input,
        {
          case payload: CaptureExclusionMoveOrderEvidence
              if payload.consumesDependencies(input.referenceSource, input.playedSource) =>
            Some(payload.dependencyId -> payload.resultSet)
          case _ => None
        }
      )(
        CaptureExclusionMoveOrderProof.certifyDemanded(
          input.comparison.referenceLine,
          input.comparison.candidateLine,
          input.referenceSource,
          input.playedSource,
          input.referenceReplay,
          input.playedReplay,
          demand.captureExclusionMoveOrderDemands
        )
      )(
        semantic = _.semantic,
        record = exact => comparedLineRecord(
          exact.semantic.semanticId,
          exact.occurrence.occurrenceId,
          exact.dependency.value,
          exact.occurrence.referenceLine,
          exact.parentSources,
          exact.occurrence.proofPaths
        ),
        payload = (exact, resultSet) =>
          CaptureExclusionMoveOrderEvidence(
            exact.semantic,
            exact.occurrence,
            exact.dependency.value,
            resultSet,
            Some(exact)
          )
      )

  private def records[A, S](
      family: String,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      input: ExactPlayedVsBestCausalInput,
      existingPayload: EvidencePayload => Option[(String, ExactCausalProofResultSet)]
  )(
      derive: => List[A]
  )(
      semantic: A => S,
      record: A => ExactCausalProofOwnerReuse.ComparedLineProofRecord,
      payload: (A, ExactCausalProofResultSet) => EvidencePayload
  ): List[EvidenceRecord] =
    ExactCausalProofOwnerReuse.comparedLineRecords(
      family,
      context,
      input,
      existingPayload
    )(derive)(
      semantic,
      record,
      (exact, proofRecord, resultSet) =>
        EvidenceRecord(
          EvidenceRef(
            allocator.evidenceId(proofRecord.evidenceIdInput(family)),
            EvidenceProducer.CausalProofProducer,
            EvidenceLayer.CausalProof,
            proofRecord.referencePosition,
            Some(proofRecord.referenceLine),
            proofRecord.referenceLine.role.scope,
            EvidenceConfidence.LegalReplayVerified
          ),
          payload(exact, resultSet),
          proofRecord.parentSources
        )
    )

  private def comparedLineRecord(
      semanticId: String,
      occurrenceId: String,
      dependencyId: String,
      referenceLine: LineNodeRef,
      parentSources: List[EvidenceRef],
      proofPaths: List[CausalProofPathOccurrence]
  ): ExactCausalProofOwnerReuse.ComparedLineProofRecord =
    ExactCausalProofOwnerReuse.ComparedLineProofRecord(
      semanticId,
      occurrenceId,
      dependencyId,
      referenceLine,
      parentSources,
      proofPaths.map(_.pathOccurrenceId).sorted
    )
