package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.analysis.policy.ClaimAdmissionStatus
import lila.chessjudgment.model.judgment.*

/** Closes every constructed Cause by requiring one selected, certified public
  * claim owner. Missing proposal/admission/exposure is never a silent state.
  */
object CauseDispositionPolicy:

  def resolve(
      graph: ClaimCandidateGraph,
      exposure: PlayerFacingCauseExposureResolution
  ): CauseDispositionLedger =
    val causeRecords = graph.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(_), _) => record
    }.sortBy(_.ref.id)
    val causeIds = causeRecords.map(_.ref.id)
    require(causeIds.distinct.size == causeIds.size, "Cause records require unique evidence owners")

    val proposalIds = graph.decisions.map(_.claim.id)
    require(proposalIds.distinct.size == proposalIds.size, "Cause disposition requires unique Jp claim ids")
    val certifiedClaimIds = graph.certified.map(_.claim.id).toSet
    require(certifiedClaimIds.size == graph.certified.size, "Cause disposition requires unique certified claim ids")

    val selectedIds = exposure.selectedCauseEvidenceIds
    val certifiedByCauseId = exposure.certifiedCauses.map(certified =>
      certified.selection.causeEvidence.id -> certified
    ).toMap
    require(
      selectedIds == causeIds.toSet && exposure.ownerClaimIdByCauseId.keySet == causeIds.toSet &&
        certifiedByCauseId.keySet == causeIds.toSet && certifiedByCauseId.size == exposure.certifiedCauses.size,
      "every constructed Cause must have one exact selected exposure owner"
    )

    val dispositions = causeRecords.map {
      case EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), _) =>
        val certified = certifiedByCauseId(causeRef.id)
        require(
          certified.selection.causeEvidence == causeRef && certified.cause == cause,
          s"constructed Cause '${causeRef.id}' does not match its certified fact and channel bundle"
        )
        val hostDecisions = graph.decisions.filter(_.claim.evidence.contains(causeRef))
        require(hostDecisions.nonEmpty, s"constructed Cause '${causeRef.id}' has no proposed claim host")
        val certifiedHosts = hostDecisions.collect {
          case decision if decision.status == ClaimAdmissionStatus.Certified => decision.claim.id
        }
        require(
          certifiedHosts.nonEmpty,
          s"constructed Cause '${causeRef.id}' has no certified claim host"
        )
        val owner = exposure.ownerClaimIdByCauseId.getOrElse(
          causeRef.id,
          throw IllegalStateException(s"selected Cause '${causeRef.id}' has no claim owner")
        )
        require(
          certifiedHosts.contains(owner) && certifiedClaimIds(owner),
          s"selected Cause '${causeRef.id}' owner '$owner' is not a certified host"
        )
        CauseDisposition(causeRef, owner)
      case _ =>
        throw IllegalStateException("Cause disposition received a non-Cause record")
    }

    val ledger = CauseDispositionLedger(dispositions)
    require(
      ledger.closedFor(graph.evidenceGraph, exposure, certifiedClaimIds),
      "Cause disposition must exactly cover C and agree with canonical R exposure"
    )
    ledger
