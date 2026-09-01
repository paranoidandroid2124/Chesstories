package lila.chessjudgment.model.judgment

/** Cause owned by one requested subject occurrence and one exact typed L2
  * proof. Assessment facts may be presented beside it but are not part of its
  * truth, identity, parents, or dependency fingerprint.
  */
private[chessjudgment] final case class OccurrenceExplanationCause(
    subject: ExplanationSubjectOccurrence,
    proofSource: EvidenceRef,
    proofOccurrence: RootOwnedEffectOccurrenceIdentity
):
  require(
    proofSource.id == proofOccurrence.sourceEvidenceId,
    "an occurrence Cause must retain its typed proof owner"
  )

object OccurrenceExplanationCause:
  private[chessjudgment] def proofOwnsSubject(
      proof: DirectCauseChannel,
      subject: ExplanationSubjectOccurrence
  ): Boolean =
    proof.rootOwnedProof match
      case RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture(_, result) =>
        result.subjectOccurrence == subject &&
          List(result.displacementBranch, result.immediateCaptureBranch).exists(branchOwnsSubject(_, subject))
      case RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture(_, result) =>
        result.subjectOccurrence == subject &&
          List(result.removalBranch, result.immediateCaptureBranch).exists(branchOwnsSubject(_, subject))
      case RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture(_, result) =>
        result.subjectOccurrence == subject &&
          List(result.vacatedGateBranch, result.retainedGateBranch).exists(branchOwnsSubject(_, subject))
      case RootOwnedEffectProof.SquareReleaseRoute(_, result) =>
        result.subjectOccurrence == subject &&
          List(result.releasedRouteBranch, result.retainedBlockerBranch).exists(branchOwnsSubject(_, subject))
      case RootOwnedEffectProof.CaptureExclusionMoveOrder(_, result) =>
        result.subjectOccurrence == subject &&
          List(result.vacatingBranch, result.immediateBranch).exists(branchOwnsSubject(_, subject))
      case RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply(_, result) =>
        result.subjectOccurrence == subject &&
          branchOwnsSubject(result.analysisContinuationBranch, subject)

  private def branchOwnsSubject(
      branch: CausalBranchOccurrence,
      subject: ExplanationSubjectOccurrence
  ): Boolean =
    branch.line.id == subject.line.id &&
      branch.lineOwnerEvidenceId == subject.lineOwnerEvidenceId &&
      branch.rootTransitionEvidenceId == subject.transitionEvidenceId &&
      branch.rootProvenance == subject.rootProvenance

final class CertifiedOccurrenceExplanation private[chessjudgment] (
    val causeEvidence: EvidenceRef,
    val subject: ExplanationSubjectOccurrence,
    val rootOwnedProof: RootOwnedEffectProof
)

object CertifiedOccurrenceExplanation:
  private[chessjudgment] def fromRecord(
      record: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): Option[CertifiedOccurrenceExplanation] =
    for
      registered <- graph.record(record.ref)
      if registered == record && graph.proofEligible(registered)
      cause <- registered.payload match
        case OccurrenceExplanationCauseEvidence(exact) => Some(exact)
        case _                                         => None
      proofRecord <- graph.record(cause.proofSource)
      if graph.proofEligible(proofRecord)
      proof <- DirectCauseChannel.fromTypedProofRecord(proofRecord)
      if proof.typedProofIdentity == cause.proofOccurrence
      if OccurrenceExplanationCause.proofOwnsSubject(proof, cause.subject)
    yield new CertifiedOccurrenceExplanation(
      record.ref,
      cause.subject,
      proof.rootOwnedProof
    )
