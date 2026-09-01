package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Seals one played root with its certified analysis-result continuation and
  * all graph-owned dependency routes after an exact PlayedVsBest demand.
  */
private[chessjudgment] object PassedPawnProgressRealizedAfterOnlyLegalReplyProofAssembler:

  private[assembly] def fromDemand(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demand: PassedPawnResultEventAssembler.PassedPawnResultDemand
  ): List[EvidenceRecord] =
    val records = demand.rootLines.toList.sortBy(_.id).flatMap { line =>
      val lineRecords = context.evidenceGraph.recordsFor(line)
      val proofOwnersBySemantic = lineRecords.collect {
        case record @ EvidenceRecord(_, proof: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence, _)
            if context.evidenceGraph.proofEligible(record) =>
          proof.semanticIdentity -> record
      }.groupMap(_._1)(_._2)
      lineRecords.flatMap {
        case source @ EvidenceRecord(_, event: PassedPawnResultEventEvidence, _)
            if event.rootLine == line && context.evidenceGraph.proofEligible(source) =>
          exactRootReplyInventoryOwner(context.evidenceGraph, source, event).toList.flatMap { inventoryOwner =>
            exactRouteGroups(event).flatMap { case (semanticIdentity, routes) =>
              exactOwnersForDependencies(
                proofOwnersBySemantic.getOrElse(semanticIdentity, Nil),
                source,
                inventoryOwner,
                routes
              ) match
                case exact :: Nil => List(exact)
                case Nil =>
                  PassedPawnProgressRealizedAfterOnlyLegalReplyProofDerivation
                    .derive(source, inventoryOwner, routes)
                    .map { proof =>
                      val parents = List(source.ref, inventoryOwner.ref)
                      require(
                        parents.map(_.id).distinct.size == parents.size,
                        "passed-pawn causal proof parents must have distinct derivation owners"
                      )
                      EvidenceRecord(
                        ref = EvidenceRef(
                          id = allocator.evidenceId(
                            s"causal-proof:passed-pawn-progress-realized-after-only-legal-reply:${proof.semanticId}:${proof.occurrenceId}:${proof.dependencyFingerprint}"
                          ),
                          producer = EvidenceProducer.CausalProofProducer,
                          layer = EvidenceLayer.CausalProof,
                          position = proof.event.rootTransition.from,
                          line = Some(proof.rootLine),
                          scope = proof.event.rootTransition.role.scope,
                          confidence = EvidenceConfidence.LegalReplayVerified
                        ),
                        payload = proof,
                        parents = parents.sortBy(_.id)
                      )
                    }
                    .toList
                case _ :: _ =>
                  throw IllegalStateException(
                    "one exact passed-pawn analysis-continuation dependency manifest has multiple graph owners"
                  )
            }
          }
        case _ => Nil
      }
    }
    require(
      records.map(_.ref.id).distinct.size == records.size,
      "one exact passed-pawn analysis-result occurrence may be produced only once"
    )
    records.sortBy(record =>
      record.payload match
        case proof: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence =>
          (proof.semanticId, proof.occurrenceId, proof.dependencyFingerprint)
        case _ => ("", "", "")
    )

  private[assembly] def existingProofOwnersForExactDependencies(
      graph: TypedEvidenceGraph,
      line: LineNodeRef,
      source: EvidenceRecord,
      inventory: EvidenceRecord,
      routes: List[PassedPawnResultRoute]
  ): List[EvidenceRecord] =
    exactOwnersForDependencies(
      graph.recordsFor(line).filter(graph.proofEligible),
      source,
      inventory,
      routes
    )

  private def exactOwnersForDependencies(
      eligibleOwners: List[EvidenceRecord],
      source: EvidenceRecord,
      inventory: EvidenceRecord,
      routes: List[PassedPawnResultRoute]
  ): List[EvidenceRecord] =
    eligibleOwners.collect {
      case record @ EvidenceRecord(_, proof: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence, _)
          if proof.consumesExactDependencies(source, inventory, routes) =>
        record
    }

  private def exactRouteGroups(
      event: PassedPawnResultEventEvidence
  ): List[(PassedPawnProgressSemanticIdentity, List[PassedPawnResultRoute])] =
    event.exactOnlyReplyResultRoutes
      .groupBy(PassedPawnProgressSemanticIdentity.from(event, _))
      .toList
      .map { case (semanticIdentity, routes) => semanticIdentity -> routes.sortBy(_.stableKey) }
      .sortBy(_._1.stableKey)

  private[assembly] def exactRootReplyInventoryOwner(
      graph: TypedEvidenceGraph,
      source: EvidenceRecord,
      event: PassedPawnResultEventEvidence
  ): Option[EvidenceRecord] =
    source.parents.flatMap(graph.record).collect {
      case record @ EvidenceRecord(ref, payload: StructuralDeltaEvidence, _)
          if ref.producer == EvidenceProducer.StructuralDeltaProducer &&
            ref.layer == EvidenceLayer.StructuralDelta &&
            ref.confidence == EvidenceConfidence.BoardDerived &&
            ref.position == event.rootTransition.from && ref.line.contains(event.rootLine) &&
            ref.scope == event.rootTransition.role.scope && payload.transition == event.rootTransition &&
            payload.transitionIsCertified && payload.canonicalOutputShapeCertified &&
            payload.canonicalTransitionProof == event.canonicalRootTransitionProof &&
            graph.proofEligible(record) =>
        record
    } match
      case exact :: Nil => Some(exact)
      case _            => None
