package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Exact actionable-demand dispatch and sole graph ownership boundary for the
  * vacated-gate-enables-unrecapturable-slider-capture family.
  */
private[chessjudgment] object VacatedGateEnablesUnrecapturableSliderCaptureAssembler:

  private[assembly] def fromDemand(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    if demand.vacatedGateEnablesUnrecapturableSliderCaptureSeeds.isEmpty then Nil
    else
      val input = demand.input
      val existingOwners =
        context.evidenceGraph.recordsFor(input.comparison.referenceLine).collect {
          case record @ EvidenceRecord(_, payload: VacatedGateEnablesUnrecapturableSliderCaptureEvidence, _)
              if payload.consumesDependencies(
                input.comparison,
                input.referenceSource,
                input.playedSource,
                input.demandSource
              ) && context.evidenceGraph.proofEligible(record) =>
            record
        }
      val existing = existingOwners.map {
        case record @ EvidenceRecord(_, payload: VacatedGateEnablesUnrecapturableSliderCaptureEvidence, _) =>
          (payload.dependencyId, payload.resultSet, record)
        case _ =>
          throw IllegalStateException(
            "a vacated-gate-enables-unrecapturable-slider-capture lookup returned a foreign payload"
          )
      }

      ExactCausalProofOwnerReuse.resolve(
        family = "vacated-gate-enables-unrecapturable-slider-capture",
        existing = existing
      )(
        derive(input, demand.vacatedGateEnablesUnrecapturableSliderCaptureSeeds).map(exact => exact.dependency.value -> exact)
      ) { (exact, resultSet) =>
        val payload = VacatedGateEnablesUnrecapturableSliderCaptureEvidence(
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
              s"causal-proof:vacated-gate-enables-unrecapturable-slider-capture:${payload.semanticId}:${payload.occurrenceId}:${payload.dependencyId}:$pathIds"
            ),
            producer = EvidenceProducer.CausalProofProducer,
            layer = EvidenceLayer.CausalProof,
            position = exact.parentSources
              .find(_.line.contains(exact.occurrence.referenceLine))
              .map(_.position)
              .getOrElse(
                throw IllegalStateException(
                  "a vacated-gate-enables-unrecapturable-slider-capture proof lost its reference line parent"
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

  private def derive(
      input: ExactPlayedVsBestCausalInput,
      changedSeeds: List[VacatedGateEnablesUnrecapturableSliderCaptureChangedSeed]
  ): List[CertifiedVacatedGateEnablesUnrecapturableSliderCapture] =
    val certified = VacatedGateEnablesUnrecapturableSliderCaptureProof.deriveChangedDependencies(
      input.comparison.referenceLine,
      input.comparison.candidateLine,
      input.referenceSource,
      input.playedSource,
      input.demandSource,
      input.comparison,
      input.referenceReplay,
      input.playedReplay,
      changedSeeds
    )
    certified
      .groupBy(_.semantic.semanticId)
      .foreach { case (semanticId, occurrences) =>
        require(
          occurrences.map(_.semantic).distinct.size == 1,
          s"vacated-gate-enables-unrecapturable-slider-capture semantic id '$semanticId' resolved to conflicting proofs"
        )
      }
    require(
      certified.map(_.occurrence.occurrenceId).distinct.size == certified.size,
      "one exact vacated-gate-enables-unrecapturable-slider-capture occurrence may be produced only once"
    )
    certified.sortBy(_.occurrence.occurrenceId)
