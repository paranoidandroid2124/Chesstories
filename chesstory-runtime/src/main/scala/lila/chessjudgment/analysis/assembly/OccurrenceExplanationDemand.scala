package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.judgment.*

/** Explicit upper request for the already admitted history occurrence. It
  * contains no evaluation, rank, verdict, or candidate-comparison identity.
  */
final case class ExplanationRequest private (
    beforeFen: String,
    beforePly: Int,
    moveUci: String,
    afterFen: String
)

object ExplanationRequest:
  def forObservedMove(input: AdmittedMoveReviewInput): ExplanationRequest =
    new ExplanationRequest(
      PrincipalVariationEvidence.normalizeFen(input.beforeFen),
      input.beforePly,
      EvidenceRef.normalizeMove(input.playedMoveUci),
      PrincipalVariationEvidence.normalizeFen(input.afterPlayedFen)
    )

/** Graph-resolved explanation demand. The subject is the unique observed
  * occurrence requested above; available branches are every admitted exact
  * root occurrence, independent of assessment rank or comparison kind.
  */
private[chessjudgment] final case class OccurrenceExplanationDemand private (
    subject: CertifiedRootOccurrence,
    availableBranches: List[CertifiedRootOccurrence]
):
  require(subject.isObserved, "an explanation subject must be history-observed")
  require(availableBranches.contains(subject), "the subject must belong to the admitted branch inventory")
  require(
    availableBranches.nonEmpty &&
      availableBranches.map(_.line).distinct.size == availableBranches.size &&
      availableBranches.map(_.lineOwner.ref.id).distinct.size == availableBranches.size,
    "each explanation branch needs one exact LegalLine owner"
  )
  require(
    availableBranches.forall(branch =>
      branch.transition.from.ply == subject.transition.from.ply &&
        PrincipalVariationEvidence.sameBoardState(
          branch.transition.from.fen,
          subject.transition.from.fen
        )
    ),
    "all demanded explanation branches must share the subject root occurrence"
  )

private[chessjudgment] object OccurrenceExplanationDemand:
  def resolve(
      context: JudgmentAssemblyContext,
      request: ExplanationRequest
  ): Option[OccurrenceExplanationDemand] =
    context.certifiedRootOccurrences.flatMap { branches =>
      branches.filter(branch =>
          branch.isObserved &&
            branch.transition.from.ply == request.beforePly &&
            PrincipalVariationEvidence.sameBoardState(branch.transition.from.fen, request.beforeFen) &&
            EvidenceRef.sameMove(branch.transition.moveUci, request.moveUci) &&
            PrincipalVariationEvidence.sameBoardState(branch.transition.to.fen, request.afterFen)
        ) match
        case subject :: Nil => Some(OccurrenceExplanationDemand(subject, branches.sortBy(_.line.id)))
        case _              => None
    }

/** Family-private orientation of two exact sibling branches. Either branch
  * may be the observed subject; chess semantics, not rank, decides which one
  * is the vacating sequence.
  */
private[assembly] final case class CaptureExclusionProofDemand private (
    subject: CertifiedRootOccurrence,
    vacatingBranch: CertifiedRootOccurrence,
    immediateBranch: CertifiedRootOccurrence,
    occurrences: List[CaptureExclusionMoveOrderDemand]
):
  require(occurrences.nonEmpty, "capture exclusion needs an exact lower occurrence demand")
  require(
    subject == vacatingBranch || subject == immediateBranch,
    "a capture-exclusion proof must explain its requested subject occurrence"
  )
  require(
    vacatingBranch.line != immediateBranch.line,
    "capture-exclusion sibling branches need distinct LegalLine owners"
  )

private[assembly] object CaptureExclusionProofDemand:
  def from(demand: OccurrenceExplanationDemand): List[CaptureExclusionProofDemand] =
    val candidates = for
      vacating <- demand.availableBranches
      immediate <- demand.availableBranches
      if vacating.line != immediate.line
      if demand.subject == vacating || demand.subject == immediate
      occurrences = exactOccurrences(vacating.replay, immediate.replay)
      if occurrences.nonEmpty
    yield CaptureExclusionProofDemand(demand.subject, vacating, immediate, occurrences)

    val keys = candidates.map(candidate =>
      List(
        candidate.subject.transition.evidence.id,
        candidate.vacatingBranch.lineOwner.ref.id,
        candidate.vacatingBranch.transitionOwner.ref.id,
        candidate.immediateBranch.lineOwner.ref.id,
        candidate.immediateBranch.transitionOwner.ref.id,
        candidate.occurrences.map(_.stableKey).mkString("[", ",", "]")
      ).mkString("|")
    )
    require(
      keys.distinct.size == keys.size,
      "one exact capture-exclusion sibling demand may be dispatched only once"
    )
    candidates.sortBy(candidate =>
      (candidate.vacatingBranch.line.id, candidate.immediateBranch.line.id)
    )

  private def exactOccurrences(
      vacating: CanonicalLineReplay,
      immediate: CanonicalLineReplay
  ): List[CaptureExclusionMoveOrderDemand] =
    val vacatingSteps = vacating.replaySteps
    val immediateSteps = immediate.replaySteps
    (for
      vacatingRootStep <- vacatingSteps.headOption.toList
      immediateRootStep <- immediateSteps.headOption.toList
      immediateReplyStep <- immediateSteps.lift(1).toList
      vacatingRoot <- vacating.legalMoveOccurrence(vacatingRootStep).toList
      immediateRoot <- immediate.legalMoveOccurrence(immediateRootStep).toList
      immediateReply <- immediate.legalMoveOccurrence(immediateReplyStep).toList
      replyCapture <- immediateReply.movement.capture.toList
      if replyCapture.capturedSquare == immediateReply.movement.to
      if vacatingRoot.movement.witness.from == replyCapture.capturedSquare &&
        vacatingRoot.movement.witness.side == replyCapture.capturedSide &&
        vacatingRoot.movement.witness.beforeRole == replyCapture.capturedRole &&
        vacatingRoot.movement.witness.side == immediateRoot.movement.witness.side &&
        immediateReply.movement.witness.side != immediateRoot.movement.witness.side
      deferredIndex <- vacatingSteps.indices.drop(2).filter(_ % 2 == 0).toList
      deferredStep <- vacatingSteps.lift(deferredIndex).toList
      laterDeferred <- vacating.legalMoveOccurrence(deferredStep).toList
      if CaptureExclusionMoveOrderDemand.sameMove(immediateRoot, laterDeferred)
    yield CaptureExclusionMoveOrderDemand(
      vacatingRoot,
      immediateRoot,
      immediateReply,
      laterDeferred,
      deferredIndex
    )).sortBy(_.stableKey)
