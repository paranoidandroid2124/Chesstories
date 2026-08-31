package lila.chessjudgment.model.judgment

/** C-stage admission of a fully constructed Cause before semantic
  * canonicalization. Every initial Cause must own at least one direct channel
  * admitted for its exact comparison. Canonical merging may later remove all
  * public signatures when otherwise equivalent carriers disagree.
  */
object RelativeCauseConstructionAdmission:

  private[chessjudgment] def initiallyReady(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    val admittedChannels = admittedDirectChannels(cause, graph)
    val comparisonReady = graph.candidateComparisonRecord(cause.comparisonEvidence).exists {
      case EvidenceRecord(_, CandidateComparisonEvidence(comparison), _) =>
        comparison.hasDistinctRootMoves
      case _ => false
    }
    val bindingReady = graph.relativeCauseBinding(cause).exists(binding =>
      binding.sourceSide == cause.sourceSide &&
        RelativeCauseKind.sourceAttributionCompatible(
          cause.kind,
          cause.sourceSide,
          cause.attribution.kind
        ) &&
        cause.attribution.rootMoveMatched &&
        cause.attribution.directProofEligible &&
        admittedChannels.nonEmpty &&
        ownedDepthReady(cause, graph, admittedChannels)
    )
    comparisonReady && bindingReady

  private def ownedDepthReady(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      admittedChannels: List[DirectCauseChannel]
  ): Boolean =
    if cause.passedPawnResultCauseKind then
      cause.hasOwnedPassedPawnResultProof(graph) &&
        admittedChannels.forall(
          graph.relativeCauseChannelHasOwnedPassedPawnResultProof(cause, _)
        )
    else cause.hasOwnedTypedDepth(graph)

  def admittedDirectChannels(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    graph.relativeCauseBinding(cause).toList.flatMap { _ =>
      cause.directEffectAdmission match
        case DirectEffectAdmission.Restricted(endpointObservations) if endpointObservations.nonEmpty =>
          EvidenceObjectBinding
            .rawDirectSentenceChannelsForProjection(cause, graph)
            .filter(channel => endpointObservations.contains(channel.exactOccurrenceFingerprint))
        case DirectEffectAdmission.Unresolved | DirectEffectAdmission.Restricted(_) =>
          Nil
    }
