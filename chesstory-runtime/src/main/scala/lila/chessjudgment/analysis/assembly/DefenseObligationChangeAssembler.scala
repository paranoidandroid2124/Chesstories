package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Exact demand-to-family derivation and graph-ownership boundary. */
private[chessjudgment] object DefenseObligationChangeAssembler:

  private final case class DemandedInput(
      comparison: CandidateComparisonFact,
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay
  )

  def fromAssembly(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demandSource: EvidenceRecord
  ): List[EvidenceRecord] =
    val inputs = demandedInputs(context, demandSource)

    val existingOwners = inputs.flatMap { input =>
      context.evidenceGraph.recordsFor(input.comparison.referenceLine).collect {
        case record @ EvidenceRecord(_, payload: DefenseObligationChangeEvidence, _)
            if payload.consumesDependencies(
              input.comparison,
              input.referenceSource,
              input.playedSource,
              demandSource
            ) &&
              context.evidenceGraph.proofEligible(record) =>
          record
      }
    }
    existingOwners
      .groupBy {
        case EvidenceRecord(_, payload: DefenseObligationChangeEvidence, _) =>
          payload.exactReuseIdentity
        case _ =>
          throw IllegalStateException("a defense-obligation lookup returned a foreign payload")
      }
      .foreach { case (_, owners) =>
        if owners.size != 1 then
          throw IllegalStateException(
            "one exact defense-obligation dependency identity has multiple graph owners"
          )
      }

    if existingOwners.nonEmpty then existingOwners.sortBy(_.ref.id)
    else
      derive(inputs, demandSource).map { exact =>
        val payload = DefenseObligationChangeEvidence(
          semantic = exact.semantic,
          occurrence = exact.occurrence,
          dependencyFingerprint = exact.dependency.value,
          occurrenceProof = Some(exact)
        )
        val pathIds = payload.proofPaths.map(_.pathOccurrenceId).mkString(":")
        EvidenceRecord(
          ref = EvidenceRef(
            id = allocator.evidenceId(
              s"causal-proof:defense-obligation-change:${payload.semanticId}:${payload.occurrenceId}:${payload.dependencyId}:$pathIds"
            ),
            producer = EvidenceProducer.CausalProofProducer,
            layer = EvidenceLayer.CausalProof,
            position = exact.parentSources
              .find(_.line.contains(exact.occurrence.referenceLine))
              .map(_.position)
              .getOrElse(
                throw IllegalStateException("a defense-obligation proof lost its reference line parent")
              ),
            line = Some(exact.occurrence.referenceLine),
            scope = exact.occurrence.referenceLine.role.scope,
            confidence = EvidenceConfidence.LegalReplayVerified
          ),
          payload = payload,
          parents = exact.parentSources
        )
      }

  def deriveDemanded(
      context: JudgmentAssemblyContext,
      demandSource: EvidenceRecord
  ): List[CertifiedDefenseObligationChange] =
    derive(demandedInputs(context, demandSource), demandSource)

  private def demandedInputs(
      context: JudgmentAssemblyContext,
      demandSource: EvidenceRecord
  ): List[DemandedInput] =
    val demandingComparison = context.evidenceGraph.record(demandSource.ref).flatMap {
      case exact @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if exact == demandSource && context.evidenceGraph.proofEligible(exact) =>
        Some(fact)
      case _ => None
    }
    val exactDependencies = for
      fact <- demandingComparison.toList
      if WrongMoveOrderCausalProofDemand.accepts(fact)
      reference <- context.line(LineNodeRole.BestReference).toList
      played <- context.line(LineNodeRole.Played).toList
      if reference.ref == fact.referenceLine
      if played.ref == fact.candidateLine
      if !EvidenceRef.sameMove(reference.ref.rootMove, played.ref.rootMove)
      referenceSource <- context.evidenceGraph.uniqueProofEligibleLineFactRecordFor(reference.ref).map(_._1).toList
      playedSource <- context.evidenceGraph.uniqueProofEligibleLineFactRecordFor(played.ref).map(_._1).toList
      referenceReplay <- certifiedReplay(referenceSource).toList
      playedReplay <- certifiedReplay(playedSource).toList
    yield DemandedInput(
      fact,
      referenceSource,
      playedSource,
      referenceReplay,
      playedReplay
    )
    exactDependencies

  private def derive(
      inputs: List[DemandedInput],
      demandSource: EvidenceRecord
  ): List[CertifiedDefenseObligationChange] =
    val certified = inputs.flatMap(input =>
      DefenseObligationChangeProof.deriveImmediate(
        input.comparison.referenceLine,
        input.comparison.candidateLine,
        input.referenceSource,
        input.playedSource,
        demandSource,
        input.comparison,
        input.referenceReplay,
        input.playedReplay
      )
    )
    certified
      .groupBy(_.semantic.semanticId)
      .foreach { case (semanticId, occurrences) =>
        require(
          occurrences.map(_.semantic).distinct.size == 1,
          s"defense-obligation semantic id '$semanticId' resolved to conflicting proofs"
        )
      }
    require(
      certified.map(_.occurrence.occurrenceId).distinct.size == certified.size,
      "one defense-obligation occurrence may be produced only once"
    )
    certified.sortBy(_.occurrence.occurrenceId)

  private def certifiedReplay(source: EvidenceRecord): Option[CanonicalLineReplay] =
    source.payload match
      case value: LineFactEvidence => value.certifiedReplay
      case _                       => None
