package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Demand dispatcher for the forced-reply L2 proof family. The actionable
  * PlayedVsBest demand names exactly one sibling pair, so no all-lines pair
  * product is constructed. This family admits only the immediate third-ply
  * realizer; a later occurrence remains unproved until intervening
  * dependencies exist.
  */
private[chessjudgment] object ForcedReplyResourceDifferentialAssembler:

  def fromAssembly(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demandSource: EvidenceRecord
  ): List[EvidenceRecord] =
    val demandingComparison = context.evidenceGraph.record(demandSource.ref).flatMap {
      case exact @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if exact == demandSource && context.evidenceGraph.proofEligible(exact) =>
        Some(fact)
      case _ => None
    }
    val demandedPair = for
      fact <- demandingComparison
      _ <- Option.when(ActionablePlayedVsBestCausalProofDemand.accepts(fact))((): Unit)
      reference <- context.line(LineNodeRole.BestReference)
      played <- context.line(LineNodeRole.Played)
      if reference.ref == fact.referenceLine
      if played.ref == fact.candidateLine
      if !EvidenceRef.sameMove(reference.ref.rootMove, played.ref.rootMove)
      referenceSource <- context.evidenceGraph.uniqueProofEligibleLineFactRecordFor(reference.ref).map(_._1)
      playedSource <- context.evidenceGraph.uniqueProofEligibleLineFactRecordFor(played.ref).map(_._1)
      referencePayload <- referenceSource.payload match
        case value: LineFactEvidence => Some(value)
        case _                       => None
      playedPayload <- playedSource.payload match
        case value: LineFactEvidence => Some(value)
        case _                       => None
      referenceReplay <- referencePayload.certifiedReplay
      playedReplay <- playedPayload.certifiedReplay
    yield (
      reference.ref,
      played.ref,
      referenceSource,
      playedSource,
      demandSource,
      fact,
      referenceReplay,
      playedReplay
    )

    val exactDependencies = demandedPair.toList
    val existingDependencyOwners = exactDependencies.flatMap {
      case (
            referenceLine,
            _,
            referenceSource,
            playedSource,
            demandSource,
            fact,
            _,
            _
          ) =>
        context.evidenceGraph.recordsFor(referenceLine).collect {
          case record @ EvidenceRecord(_, payload: ForcedReplyResourceDifferentialEvidence, _)
              if payload.consumesDependencies(fact, referenceSource, playedSource, demandSource) &&
                context.evidenceGraph.proofEligible(record) =>
            record
        }
    }

    existingDependencyOwners
      .groupBy {
        case EvidenceRecord(_, payload: ForcedReplyResourceDifferentialEvidence, _) =>
          payload.exactReuseIdentity
        case _ =>
          throw IllegalStateException("a forced-reply dependency lookup returned a foreign payload")
      }
      .foreach { case (_, owners) =>
        if owners.size != 1 then
          throw IllegalStateException(
            "one exact L2 proof dependency identity has multiple graph owners"
          )
      }
    if existingDependencyOwners.nonEmpty then existingDependencyOwners.sortBy(_.ref.id)
    else deriveNew(allocator, exactDependencies)

  private def deriveNew(
      allocator: JudgmentProvenanceAllocator,
      exactDependencies: List[(
        LineNodeRef,
        LineNodeRef,
        EvidenceRecord,
        EvidenceRecord,
        EvidenceRecord,
        CandidateComparisonFact,
        CanonicalLineReplay,
        CanonicalLineReplay
      )]
  ): List[EvidenceRecord] =
    val certified = exactDependencies.flatMap {
      case (
            referenceLine,
            playedLine,
            referenceSource,
            playedSource,
            demandSource,
            fact,
            referenceReplay,
            playedReplay
          ) =>
        ForcedReplyResourceProof.deriveImmediate(
          referenceLine,
          playedLine,
          referenceSource,
          playedSource,
          demandSource,
          fact,
          referenceReplay,
          playedReplay
        )
    }

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

    certified.sortBy(_.occurrence.occurrenceId).map { exact =>
      val payload = ForcedReplyResourceDifferentialEvidence(
        semantic = exact.semantic,
        occurrence = exact.occurrence,
        dependencyFingerprint = exact.dependency.value,
        occurrenceProof = Some(exact.proof)
      )
      val pathIds = payload.proofPaths.map(_.pathOccurrenceId).mkString(":")
      EvidenceRecord(
        ref = EvidenceRef(
          id = allocator.evidenceId(
            s"causal-proof:forced-reply-resource:${payload.semanticId}:${payload.occurrenceId}:${payload.dependencyId}:$pathIds"
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
    }
