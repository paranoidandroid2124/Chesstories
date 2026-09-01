package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Demand dispatcher for the unique-check-reply defender-displacement L2 proof family. The actionable
  * PlayedVsBest demand names exactly one sibling pair, so no all-lines pair
  * product is constructed. This family admits only the immediate third-ply
  * realizer; a later occurrence remains unproved until intervening
  * dependencies exist.
  */
private[chessjudgment] object UniqueCheckReplyDefenderDisplacementBeforeCaptureAssembler:

  private[assembly] def fromDemand(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: RelationCausalProofDemand
  ): List[EvidenceRecord] =
    demand.uniqueCheckReplyDefenderDisplacementBeforeCaptureSeed.toList.flatMap { changedSeed =>
      val input = demand.input
      val existingDependencyOwners =
        context.evidenceGraph.recordsFor(input.comparison.referenceLine).collect {
          case record @ EvidenceRecord(_, payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _)
              if payload.consumesDependencies(
                input.comparison,
                input.referenceSource,
                input.playedSource,
                input.demandSource
              ) && context.evidenceGraph.proofEligible(record) =>
            record
        }

      val existing = existingDependencyOwners.map {
        case record @ EvidenceRecord(_, payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) =>
          (payload.dependencyId, payload.resultSet, record)
        case _ =>
          throw IllegalStateException(
            "a unique-check-reply defender-displacement lookup returned a foreign payload"
          )
      }

      ExactCausalProofOwnerReuse.resolve(
        family = "unique-check-reply-defender-displacement-before-capture",
        existing = existing
      )(
        derive(input, changedSeed).map(exact => exact.dependency.value -> exact)
      ) { (exact, resultSet) =>
        val payload = UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence(
          semantic = exact.semantic,
          occurrence = exact.occurrence,
          dependencyFingerprint = exact.dependency.value,
          resultSet = resultSet,
          occurrenceProof = Some(exact.proof)
        )
        recordFor(allocator, exact, payload)
      }.sortBy(_.ref.id)
    }

  private def derive(
      input: ExactPlayedVsBestCausalInput,
      changedSeed: UniqueCheckReplyDefenderDisplacementBeforeCaptureChangedSeed
  ): List[CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture] =
    val certified = UniqueCheckReplyDefenderDisplacementBeforeCaptureProof.deriveImmediate(
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
          s"L2 semantic id '$semanticId' resolved to conflicting proofs"
        )
      }
    require(
      certified.map(_.occurrence.occurrenceId).distinct.size == certified.size,
      "one L2 occurrence may be produced only once"
    )

    certified.sortBy(_.occurrence.occurrenceId)

  private def recordFor(
      allocator: JudgmentProvenanceAllocator,
      exact: CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture,
      payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence
  ): EvidenceRecord =
    val pathIds = payload.proofPaths.map(_.pathOccurrenceId).mkString(":")
    EvidenceRecord(
      ref = EvidenceRef(
        id = allocator.evidenceId(
          s"causal-proof:unique-check-reply-defender-displacement-before-capture:${payload.semanticId}:${payload.occurrenceId}:${payload.dependencyId}:$pathIds"
        ),
        producer = EvidenceProducer.CausalProofProducer,
        layer = EvidenceLayer.CausalProof,
        position = exact.proof.parentSources
          .find(_.line.contains(exact.occurrence.referenceLine))
          .map(_.position)
          .getOrElse(
            throw IllegalStateException("an L2 proof lost its reference line parent")
          ),
        line = Some(exact.occurrence.referenceLine),
        scope = exact.occurrence.referenceLine.role.scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      payload = payload,
      parents = exact.proof.parentSources
    )
