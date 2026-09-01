package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Sole graph owner for exact capture-exclusion move-order results. */
private[chessjudgment] object CaptureExclusionMoveOrderAssembler:

  private[assembly] def fromDemand(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    if demand.captureExclusionMoveOrderSeeds.isEmpty then Nil
    else
      val input = demand.input
      val family = "capture-exclusion-move-order"
      ExactCausalProofOwnerReuse.comparedLineRecords(
        family = family,
        context = context,
        input = input,
        existingPayload = {
          case payload: CaptureExclusionMoveOrderEvidence
              if payload.consumesDependencies(input.referenceSource, input.playedSource) =>
            Some(payload.dependencyId -> payload.resultSet)
          case _ => None
        }
      )(
        CaptureExclusionMoveOrderProof.deriveChangedDependencies(
          input.comparison.referenceLine,
          input.comparison.candidateLine,
          input.referenceSource,
          input.playedSource,
          input.referenceReplay,
          input.playedReplay,
          demand.captureExclusionMoveOrderSeeds
        )
      )(
        semantic = _.semantic,
        record = exact =>
          ExactCausalProofOwnerReuse.ComparedLineProofRecord(
            exact.semantic.semanticId,
            exact.occurrence.occurrenceId,
            exact.dependency.value,
            exact.occurrence.referenceLine,
            exact.parentSources,
            exact.occurrence.proofPaths.map(_.pathOccurrenceId).sorted
          ),
        create = (exact, proofRecord, resultSet) =>
          val payload = CaptureExclusionMoveOrderEvidence(
            exact.semantic,
            exact.occurrence,
            exact.dependency.value,
            resultSet,
            Some(exact)
          )
          EvidenceRecord(
            ref = EvidenceRef(
              id = allocator.evidenceId(proofRecord.evidenceIdInput(family)),
              producer = EvidenceProducer.CausalProofProducer,
              layer = EvidenceLayer.CausalProof,
              position = proofRecord.referencePosition,
              line = Some(proofRecord.referenceLine),
              scope = proofRecord.referenceLine.role.scope,
              confidence = EvidenceConfidence.LegalReplayVerified
            ),
            payload = payload,
            parents = proofRecord.parentSources
          )
      )
