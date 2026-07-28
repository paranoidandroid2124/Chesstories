package lila.chessjudgment.model.judgment

import scala.util.Try

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
      case EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison), _) =>
        comparisonRef.confidence == EvidenceConfidence.EngineBacked &&
          graph.comparisonFor(cause).contains(comparison) &&
          comparison.hasDistinctRootMoves
      case _ => false
    }
    val bindingReady = authoritativeBinding(cause, graph).exists(binding =>
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
    if cause.strategicCauseKind then
      cause.hasOwnedAdmissibleLongTermProof(graph) &&
        admittedChannels.forall(
          graph.relativeCauseStrategicChannelHasOwnedLongTermProof(cause, _)
        )
    else cause.hasOwnedTypedDepth(graph)

  private[chessjudgment] def authoritativeBinding(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): Option[RelativeCauseBinding] =
    Try(graph.requiredRelativeCauseBinding(cause)).toOption

  private def rawDirectChannels(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(cause, graph)

  /** Diagnostic-only raw ownership view. Its channels have not crossed C's
    * admission selector and therefore carry no public eligibility.
    */
  def diagnosticRawDirectChannels(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    rawDirectChannels(cause, graph)

  def admittedDirectChannels(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    authoritativeBinding(cause, graph).toList.flatMap { _ =>
      cause.directEffectAdmission match
        case DirectEffectAdmission.Restricted(signatures) if signatures.nonEmpty =>
          rawDirectChannels(cause, graph)
            .filter(channel => signatures(channel.causalSignature))
        case DirectEffectAdmission.Unresolved | DirectEffectAdmission.Restricted(_) =>
          Nil
    }
