package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Sole graph owner for exact square-release route results. */
private[chessjudgment] object SquareReleaseRouteAssembler:

  private[assembly] def fromDemand(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    if demand.squareReleaseRouteSeeds.isEmpty then Nil
    else
      val input = demand.input
      val family = "square-release-route"
      ExactCausalProofOwnerReuse.comparedLineRecords(
        family = family,
        context = context,
        input = input,
        existingPayload = {
          case payload: SquareReleaseRouteEvidence
              if payload.consumesDependencies(input.referenceSource, input.playedSource) =>
            Some(payload.dependencyId -> payload.resultSet)
          case _ => None
        }
      )(
        SquareReleaseRouteProof.deriveChangedDependencies(
          input.comparison.referenceLine,
          input.comparison.candidateLine,
          input.referenceSource,
          input.playedSource,
          input.referenceReplay,
          input.playedReplay,
          demand.squareReleaseRouteSeeds
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
          val payload = SquareReleaseRouteEvidence(
            semantic = exact.semantic,
            occurrence = exact.occurrence,
            dependencyFingerprint = exact.dependency.value,
            resultSet = resultSet,
            occurrenceProof = Some(exact)
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
