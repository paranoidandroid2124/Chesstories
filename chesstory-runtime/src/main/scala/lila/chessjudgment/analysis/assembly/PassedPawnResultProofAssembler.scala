package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Seals robust passed-pawn results into the shared bounded-causal contract only
  * after an exact PlayedVsBest comparison has demanded the two endpoints.
  */
private[chessjudgment] object PassedPawnResultProofAssembler:

  def fromAssembly(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demandingComparison: EvidenceRecord
  ): List[EvidenceRecord] =
    val demandedLines = PassedPawnResultEventAssembler.demandedRootLines(context, demandingComparison)
    val records =
      if demandedLines.isEmpty then Nil
      else
        demandedLines.toList.sortBy(_.id).flatMap { line =>
          context.evidenceGraph.recordsFor(line).flatMap {
            case source @ EvidenceRecord(_, event: PassedPawnResultEventEvidence, _)
                if event.rootLine == line && context.evidenceGraph.proofEligible(source) =>
              exactRootReplyInventoryOwner(context.evidenceGraph, source, event).toList.flatMap { inventoryOwner =>
                event.exactRobustPublicResultAssessments.flatMap { assessment =>
                  existingProofOwnersForExactDependencies(
                    context.evidenceGraph,
                    line,
                    source,
                    demandingComparison,
                    inventoryOwner,
                    assessment
                  ) match
                    case exact :: Nil => List(exact)
                    case Nil =>
                      PassedPawnResultProofDerivation
                        .derive(source, demandingComparison, inventoryOwner, assessment)
                        .map { proof =>
                          EvidenceRecord(
                            ref = EvidenceRef(
                              id = allocator.evidenceId(
                                s"causal-proof:passed-pawn-result:${proof.semanticId}:${proof.occurrenceId}:${proof.dependencyFingerprint}"
                              ),
                              producer = EvidenceProducer.CausalProofProducer,
                              layer = EvidenceLayer.CausalProof,
                              position = proof.event.rootTransition.from,
                              line = Some(proof.rootLine),
                              scope = proof.event.rootTransition.role.scope,
                              confidence = EvidenceConfidence.LegalReplayVerified
                            ),
                            payload = proof,
                            parents = List(source.ref, demandingComparison.ref, inventoryOwner.ref)
                              .distinctBy(_.id)
                              .sortBy(_.id)
                          )
                        }
                        .toList
                    case _ :: _ =>
                      throw IllegalStateException(
                        "one exact passed-pawn-result L2 dependency manifest has multiple graph owners"
                      )
                }
              }
            case _ => Nil
          }
        }
    require(
      records.map(_.ref.id).distinct.size == records.size,
      "one exact passed-pawn result causal occurrence may be produced only once"
    )
    records.sortBy(record =>
      record.payload match
        case proof: PassedPawnResultProofEvidence =>
          (proof.semanticId, proof.occurrenceId, proof.dependencyFingerprint)
        case _ => ("", "", "")
    )

  private[assembly] def existingProofOwnersForExactDependencies(
      graph: TypedEvidenceGraph,
      line: LineNodeRef,
      source: EvidenceRecord,
      comparison: EvidenceRecord,
      inventory: EvidenceRecord,
      assessment: PassedPawnResultReplyAssessment
  ): List[EvidenceRecord] =
    graph.recordsFor(line).collect {
      case record @ EvidenceRecord(_, proof: PassedPawnResultProofEvidence, _)
          if graph.proofEligible(record) &&
            proof.consumesExactDependencies(source, comparison, inventory, assessment) =>
        record
    }

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
            ref.position == event.rootTransition.from &&
            ref.line.contains(event.rootLine) &&
            ref.scope == event.rootTransition.role.scope &&
            payload.transition == event.rootTransition &&
            payload.transitionIsCertified && payload.canonicalOutputShapeCertified &&
            payload.canonicalTransitionProof == event.canonicalRootTransitionProof &&
            graph.proofEligible(record) =>
        record
    } match
      case exact :: Nil => Some(exact)
      case _            => None
