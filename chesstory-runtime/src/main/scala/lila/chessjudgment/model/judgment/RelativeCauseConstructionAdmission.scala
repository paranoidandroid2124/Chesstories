package lila.chessjudgment.model.judgment

/** C-stage admission of a fully constructed Cause. Every initial Cause owns
  * every declared proof source through one exact typed channel occurrence.
  */
object RelativeCauseConstructionAdmission:

  private[chessjudgment] def initiallyReady(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    initiallyReadyWithChannels(cause, graph, admittedDirectChannels(cause, graph))

  private[chessjudgment] def initiallyReadyWithChannels(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      admittedChannels: List[DirectCauseChannel]
  ): Boolean =
    val comparisonReady = graph.candidateComparisonRecord(cause.comparisonEvidence).exists {
      case EvidenceRecord(_, CandidateComparisonEvidence(comparison), _) =>
        ActionablePlayedVsBestCausalProofDemand.accepts(comparison)
      case _ => false
    }
    val exactSourceOwnership =
      admittedChannels.nonEmpty &&
        admittedChannels.map(_.source.id).sorted == cause.proofSources.map(_.id) &&
        admittedChannels.map(_.exactOccurrenceFingerprint).distinct.size == admittedChannels.size
    val bindingReady = graph.relativeCauseBinding(cause).exists(binding =>
      binding.sourceSide == cause.sourceSide &&
        binding.evidenceLines == List(binding.eventLine) &&
        admittedChannels.forall(_.eventLine == binding.eventLine)
    )
    comparisonReady && exactSourceOwnership && bindingReady

  def admittedDirectChannels(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    DirectCauseChannel.certifiedForCause(cause, graph)
