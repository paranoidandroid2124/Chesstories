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

  def subjectSiblingOrientations: List[(CertifiedRootOccurrence, CertifiedRootOccurrence)] =
    availableBranches
      .filterNot(_.lineOwner.ref.id == subject.lineOwner.ref.id)
      .flatMap(sibling => List(subject -> sibling, sibling -> subject))
      .sortBy { case (left, right) => (left.lineOwner.ref.id, right.lineOwner.ref.id) }

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

private[assembly] final case class RelocationEnablesRecaptureProofDemand private (
    subject: CertifiedRootOccurrence,
    relocatedBranch: CertifiedRootOccurrence,
    retainedBranch: CertifiedRootOccurrence,
    occurrences: List[RelocationEnablesRecaptureDemand]
):
  require(occurrences.nonEmpty, "relocation recapture needs an exact lower occurrence demand")
  require(
    subject == relocatedBranch || subject == retainedBranch,
    "a relocation-recapture proof must explain its requested subject occurrence"
  )
  require(
    relocatedBranch.line != retainedBranch.line,
    "relocation-recapture sibling branches need distinct LegalLine owners"
  )

private[assembly] object RelocationEnablesRecaptureProofDemand:
  def from(demand: OccurrenceExplanationDemand): List[RelocationEnablesRecaptureProofDemand] =
    val catalogEntries = demand.availableBranches.map(branch =>
      branch.lineOwner.ref.id -> RelocationEnablesRecaptureDemand.capturesWithImmediateReply(branch.replay)
    )
    require(
      catalogEntries.map(_._1).distinct.size == catalogEntries.size,
      "each relocation-recapture catalog entry needs one exact LegalLine owner"
    )
    val captureCatalog = catalogEntries.toMap

    val candidates = for
      (relocated, retained) <- demand.subjectSiblingOrientations
      occurrences = RelocationEnablesRecaptureDemand.fromCatalog(
        captureCatalog(relocated.lineOwner.ref.id),
        captureCatalog(retained.lineOwner.ref.id)
      )
      if occurrences.nonEmpty
    yield RelocationEnablesRecaptureProofDemand(demand.subject, relocated, retained, occurrences)

    val keys = candidates.map(candidate =>
      List(
        candidate.subject.transition.evidence.id,
        candidate.relocatedBranch.lineOwner.ref.id,
        candidate.relocatedBranch.transitionOwner.ref.id,
        candidate.retainedBranch.lineOwner.ref.id,
        candidate.retainedBranch.transitionOwner.ref.id,
        candidate.occurrences.map(_.stableKey).mkString("[", ",", "]")
      ).mkString("|")
    )
    require(
      keys.distinct.size == keys.size,
      "one exact relocation-recapture sibling demand may be dispatched only once"
    )
    candidates.sortBy(candidate =>
      (candidate.relocatedBranch.line.id, candidate.retainedBranch.line.id)
    )
