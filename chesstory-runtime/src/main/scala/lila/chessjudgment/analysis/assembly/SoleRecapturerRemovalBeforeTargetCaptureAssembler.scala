package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Exact demand-to-family derivation and graph-ownership boundary. */
private[chessjudgment] object SoleRecapturerRemovalBeforeTargetCaptureAssembler:

  private[assembly] def fromDemand(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    demand.soleRecapturerRemovalBeforeTargetCaptureSeed.toList.flatMap { changedSeed =>
      val input = demand.input
      val existingOwners =
        context.evidenceGraph.recordsFor(input.comparison.referenceLine).collect {
          case record @ EvidenceRecord(_, payload: SoleRecapturerRemovalBeforeTargetCaptureEvidence, _)
              if payload.consumesDependencies(
                input.comparison,
                input.referenceSource,
                input.playedSource,
                input.demandSource
              ) && context.evidenceGraph.proofEligible(record) =>
            record
        }
      val existing = existingOwners.map {
        case record @ EvidenceRecord(_, payload: SoleRecapturerRemovalBeforeTargetCaptureEvidence, _) =>
          (payload.dependencyId, payload.resultSet, record)
        case _ =>
          throw IllegalStateException(
            "a sole-recapturer-removal-before-target-capture lookup returned a foreign payload"
          )
      }

      ExactCausalProofOwnerReuse.resolve(
        family = "sole-recapturer-removal-before-target-capture",
        existing = existing
      )(
        derive(input, changedSeed).map(exact => exact.dependency.value -> exact)
      ) { (exact, resultSet) =>
        val payload = SoleRecapturerRemovalBeforeTargetCaptureEvidence(
          semantic = exact.semantic,
          occurrence = exact.occurrence,
          dependencyFingerprint = exact.dependency.value,
          resultSet = resultSet,
          occurrenceProof = Some(exact)
        )
        val pathIds = payload.proofPaths.map(_.pathOccurrenceId).mkString(":")
        EvidenceRecord(
          ref = EvidenceRef(
            id = allocator.evidenceId(
              s"causal-proof:sole-recapturer-removal-before-target-capture:${payload.semanticId}:${payload.occurrenceId}:${payload.dependencyId}:$pathIds"
            ),
            producer = EvidenceProducer.CausalProofProducer,
            layer = EvidenceLayer.CausalProof,
            position = exact.parentSources
              .find(_.line.contains(exact.occurrence.referenceLine))
              .map(_.position)
              .getOrElse(
                throw IllegalStateException(
                  "a sole-recapturer-removal-before-target-capture proof lost its reference line parent"
                )
              ),
            line = Some(exact.occurrence.referenceLine),
            scope = exact.occurrence.referenceLine.role.scope,
            confidence = EvidenceConfidence.LegalReplayVerified
          ),
          payload = payload,
          parents = exact.parentSources
        )
      }.sortBy(_.ref.id)
    }

  private def derive(
      input: ExactPlayedVsBestCausalInput,
      changedSeed: SoleRecapturerRemovalBeforeTargetCaptureChangedSeed
  ): List[CertifiedSoleRecapturerRemovalBeforeTargetCapture] =
    val certified = SoleRecapturerRemovalBeforeTargetCaptureProof.deriveImmediate(
      input.comparison.referenceLine,
      input.comparison.candidateLine,
      input.referenceSource,
      input.playedSource,
      input.demandSource,
      input.comparison,
      input.referenceReplay,
      input.playedReplay,
      changedSeed
    )
    certified
      .groupBy(_.semantic.semanticId)
      .foreach { case (semanticId, occurrences) =>
        require(
          occurrences.map(_.semantic).distinct.size == 1,
          s"sole-recapturer-removal-before-target-capture semantic id '$semanticId' resolved to conflicting proofs"
        )
      }
    require(
      certified.map(_.occurrence.occurrenceId).distinct.size == certified.size,
      "one sole-recapturer-removal-before-target-capture occurrence may be produced only once"
    )
    certified.sortBy(_.occurrence.occurrenceId)
