package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.analysis.policy.ClaimAdmissionStatus
import lila.chessjudgment.model.judgment.*

/** The sole derivation of one terminal disposition for every C-stage
  * RelativeCause record.
  *
  * This policy consumes, without re-deciding, the admission decisions made by
  * `ClaimTruthPolicy`, the lineage emitted by `ClaimDeduplicator`, and the
  * canonical `PlayerFacingCauseExposureResolution`.  It therefore exposes a
  * missing Jp host or a Ja/R suppression instead of allowing a ready Cause to
  * disappear between stage-specific collections.
  */
object CauseDispositionPolicy:

  def resolve(
      graph: ClaimCandidateGraph,
      deduplication: ClaimDeduplicationResult,
      exposure: PlayerFacingCauseExposureResolution
  ): CauseDispositionLedger =
    val causeRecords = graph.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(_), _) => record
    }.sortBy(_.ref.id)
    val proposalDecisions = graph.decisions
    val proposalIds = proposalDecisions.map(_.claim.id)
    val rankedClaims = deduplication.decisions.map(_.claim)
    val rankedClaimIds = rankedClaims.map(_.id).toSet
    val trace = deduplication.trace
    val traceByOriginalClaimId = trace.map(item => item.originalClaimId -> item.keptClaimId).toMap
    val readyHostsByCauseId = exposure.readyByClaim.toList
      .flatMap { case (claimId, causes) =>
        causes.map { case (_, causeRef) => causeRef.id -> claimId }
      }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.distinct.sorted)
      .toMap
    val dominanceByCauseId =
      exposure.dominanceDecisions.map(decision => decision.causeEvidenceId -> decision).toMap
    val crossByCauseId =
      exposure.crossDecisions.map(decision => decision.causeEvidenceId -> decision).toMap

    require(
      proposalIds.size == proposalIds.distinct.size,
      "Cause disposition requires unique Jp claim ids"
    )
    require(
      rankedClaimIds.size == rankedClaims.size,
      "Cause disposition requires unique post-deduplication claim ids"
    )
    require(
      traceByOriginalClaimId.size == trace.size,
      "Cause disposition requires one claim-deduplication successor per original claim"
    )
    require(
      dominanceByCauseId.size == exposure.dominanceDecisions.size,
      "Cause disposition requires one dominance decision per ready Cause"
    )
    require(
      crossByCauseId.size == exposure.crossDecisions.size,
      "Cause disposition requires one cross-comparison decision per retained Cause"
    )

    def directlyHosts(claim: JudgmentClaim, causeRef: EvidenceRef): Boolean =
      claim.evidence.contains(causeRef)

    def terminalClaimId(claimId: String): Option[String] =
      @annotation.tailrec
      def loop(current: String, visited: Set[String]): Option[String] =
        if rankedClaimIds(current) then Some(current)
        else if visited(current) then None
        else traceByOriginalClaimId.get(current) match
          case Some(next) => loop(next, visited + current)
          case None       => None
      loop(claimId, Set.empty)

    def rDisposition(
        causeRef: EvidenceRef,
        proposedClaimIds: List[String],
        certifiedClaimIds: List[String],
        rankEligibleClaimIds: List[String]
    ): CauseDisposition =
      val dominance = dominanceByCauseId.getOrElse(
        causeRef.id,
        throw IllegalArgumentException(
          s"rank-eligible Cause '${causeRef.id}' has no dominance decision"
        )
      )
      val authority = CauseRDispositionAuthority
        .from(dominance, crossByCauseId.get(causeRef.id))
        .getOrElse(
          throw IllegalArgumentException(
            s"retained Cause '${causeRef.id}' has no cross-comparison exposure decision"
          )
        )
      val owner =
        Option.when(authority.status == CauseDispositionStatus.Selected)(
          exposure.ownerClaimIdByCauseId.getOrElse(
            causeRef.id,
            throw IllegalArgumentException(
              s"selected Cause '${causeRef.id}' has no canonical claim owner"
            )
          )
        )
      CauseDisposition(
        causeEvidence = causeRef,
        status = authority.status,
        reason = authority.reason,
        proposedClaimIds = proposedClaimIds,
        certifiedClaimIds = certifiedClaimIds,
        rankEligibleClaimIds = rankEligibleClaimIds,
        selectedOwnerClaimId = owner,
        relatedCauseEvidenceIds = authority.relatedCauseEvidenceIds,
        relatedClaimIds = Nil
      )

    val dispositions = causeRecords.map {
      case EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), _) =>
        val hostDecisions = proposalDecisions.filter(decision => directlyHosts(decision.claim, causeRef))
        val proposedClaimIds = hostDecisions.map(_.claim.id).distinct.sorted
        val certifiedClaimIds = hostDecisions.collect {
          case decision if decision.status == ClaimAdmissionStatus.Certified => decision.claim.id
        }.distinct.sorted
        val deferredClaimIds = hostDecisions.collect {
          case decision if decision.status == ClaimAdmissionStatus.Deferred => decision.claim.id
        }.distinct.sorted
        val rejectedClaimIds = hostDecisions.collect {
          case decision if decision.status == ClaimAdmissionStatus.Rejected => decision.claim.id
        }.distinct.sorted
        val rankEligibleClaimIds = readyHostsByCauseId.getOrElse(causeRef.id, Nil)
        val objectReady =
          PlayerFacingCauseReadinessPolicy.ready(cause, causeRef, graph.evidenceGraph)

        if !objectReady then
          CauseDisposition(
            causeRef,
            CauseDispositionStatus.ObjectUnready,
            CauseDispositionReason.ObjectReadinessFailed,
            proposedClaimIds,
            certifiedClaimIds,
            Nil,
            None,
            Nil,
            Nil
          )
        else if proposedClaimIds.isEmpty then
          CauseDisposition(
            causeRef,
            CauseDispositionStatus.Unproposed,
            CauseDispositionReason.NoClaimProposal,
            Nil,
            Nil,
            Nil,
            None,
            Nil,
            Nil
          )
        else if certifiedClaimIds.isEmpty && deferredClaimIds.nonEmpty then
          CauseDisposition(
            causeRef,
            CauseDispositionStatus.AdmissionDeferred,
            CauseDispositionReason.ClaimAdmissionDeferred,
            proposedClaimIds,
            Nil,
            Nil,
            None,
            Nil,
            Nil
          )
        else if certifiedClaimIds.isEmpty && rejectedClaimIds.nonEmpty then
          CauseDisposition(
            causeRef,
            CauseDispositionStatus.Rejected,
            CauseDispositionReason.ClaimAdmissionRejected,
            proposedClaimIds,
            Nil,
            Nil,
            None,
            Nil,
            Nil
          )
        else if rankEligibleClaimIds.isEmpty then
          val terminalClaimIds = certifiedClaimIds.flatMap(terminalClaimId).distinct.sorted
          require(
            terminalClaimIds.nonEmpty,
            s"certified Cause '${causeRef.id}' disappeared without ClaimDeduplicator lineage"
          )
          CauseDisposition(
            causeRef,
            CauseDispositionStatus.Redundant,
            CauseDispositionReason.CertifiedClaimDeduplicated,
            proposedClaimIds,
            certifiedClaimIds,
            Nil,
            None,
            Nil,
            terminalClaimIds
          )
        else
          rDisposition(
            causeRef,
            proposedClaimIds,
            certifiedClaimIds,
            rankEligibleClaimIds
          )
      case _ =>
        throw IllegalStateException("Cause disposition received a non-Cause record")
    }
    val ledger = CauseDispositionLedger(dispositions)
    require(
      ledger.closedFor(graph.evidenceGraph, exposure, rankedClaimIds),
      "Cause disposition must exactly cover C and agree with canonical R exposure"
    )
    ledger
