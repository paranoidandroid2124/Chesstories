package lila.chessjudgment.model.judgment

/** The terminal Jp -> Ja -> R disposition of one C-stage RelativeCause
  * record.  The cases describe where the Cause stopped; they do not make a
  * second truth, deduplication, fallback, or exposure decision.
  */
enum CauseDispositionStatus:
  case Selected
  case Dominated
  case Redundant
  case Diagnostic
  case Inferior
  case AdmissionDeferred
  case Rejected
  case Unproposed
  case ObjectUnready

/** The existing stage result that authorizes a terminal disposition. */
enum CauseDispositionReason:
  case PlayerFacingSelection
  case DominatedFallback
  case CrossComparisonRedundancy
  case CertifiedClaimDeduplicated
  case DiagnosticComparison
  case InferiorAlternative
  case ClaimAdmissionDeferred
  case ClaimAdmissionRejected
  case NoClaimProposal
  case ObjectReadinessFailed

final case class CauseDisposition(
    causeEvidence: EvidenceRef,
    status: CauseDispositionStatus,
    reason: CauseDispositionReason,
    proposedClaimIds: List[String],
    certifiedClaimIds: List[String],
    rankEligibleClaimIds: List[String],
    selectedOwnerClaimId: Option[String],
    relatedCauseEvidenceIds: List[String],
    relatedClaimIds: List[String]
):
  require(
    causeEvidence.layer == EvidenceLayer.RelativeCause,
    "a Cause disposition must identify a RelativeCause record"
  )
  require(canonicalIds(proposedClaimIds), "proposed Cause hosts must be canonical")
  require(canonicalIds(certifiedClaimIds), "certified Cause hosts must be canonical")
  require(canonicalIds(rankEligibleClaimIds), "rank-eligible Cause hosts must be canonical")
  require(canonicalIds(relatedCauseEvidenceIds), "related Cause ids must be canonical")
  require(canonicalIds(relatedClaimIds), "related claim ids must be canonical")
  require(
    certifiedClaimIds.forall(proposedClaimIds.contains),
    "every certified Cause host must originate in Jp"
  )
  require(statusReasonCompatible, "Cause disposition status and authority must agree")
  require(
    selectedOwnerClaimId.nonEmpty == (status == CauseDispositionStatus.Selected),
    "exactly a selected Cause must carry its selected claim owner"
  )
  require(
    selectedOwnerClaimId.forall(rankEligibleClaimIds.contains),
    "a selected Cause owner must be one of its rank-eligible claim hosts"
  )

  private def canonicalIds(ids: List[String]): Boolean =
    ids.forall(_.trim.nonEmpty) && ids == ids.distinct.sorted

  private def statusReasonCompatible: Boolean =
    (status, reason) match
      case (CauseDispositionStatus.Selected, CauseDispositionReason.PlayerFacingSelection) =>
        rankEligibleClaimIds.nonEmpty && relatedCauseEvidenceIds.isEmpty && relatedClaimIds.isEmpty
      case (CauseDispositionStatus.Dominated, CauseDispositionReason.DominatedFallback) =>
        rankEligibleClaimIds.nonEmpty && relatedCauseEvidenceIds.nonEmpty && relatedClaimIds.isEmpty
      case (CauseDispositionStatus.Redundant, CauseDispositionReason.CrossComparisonRedundancy) =>
        rankEligibleClaimIds.nonEmpty && relatedCauseEvidenceIds.nonEmpty && relatedClaimIds.isEmpty
      case (CauseDispositionStatus.Redundant, CauseDispositionReason.CertifiedClaimDeduplicated) =>
        certifiedClaimIds.nonEmpty && rankEligibleClaimIds.isEmpty &&
          relatedCauseEvidenceIds.isEmpty && relatedClaimIds.nonEmpty
      case (CauseDispositionStatus.Diagnostic, CauseDispositionReason.DiagnosticComparison) =>
        rankEligibleClaimIds.nonEmpty && relatedCauseEvidenceIds.isEmpty && relatedClaimIds.isEmpty
      case (CauseDispositionStatus.Inferior, CauseDispositionReason.InferiorAlternative) =>
        rankEligibleClaimIds.nonEmpty && relatedCauseEvidenceIds.isEmpty && relatedClaimIds.isEmpty
      case (CauseDispositionStatus.AdmissionDeferred, CauseDispositionReason.ClaimAdmissionDeferred) =>
        proposedClaimIds.nonEmpty && certifiedClaimIds.isEmpty && rankEligibleClaimIds.isEmpty &&
          relatedCauseEvidenceIds.isEmpty && relatedClaimIds.isEmpty
      case (CauseDispositionStatus.Rejected, CauseDispositionReason.ClaimAdmissionRejected) =>
        proposedClaimIds.nonEmpty && certifiedClaimIds.isEmpty && rankEligibleClaimIds.isEmpty &&
          relatedCauseEvidenceIds.isEmpty && relatedClaimIds.isEmpty
      case (CauseDispositionStatus.Unproposed, CauseDispositionReason.NoClaimProposal) =>
        proposedClaimIds.isEmpty && certifiedClaimIds.isEmpty && rankEligibleClaimIds.isEmpty &&
          relatedCauseEvidenceIds.isEmpty && relatedClaimIds.isEmpty
      case (CauseDispositionStatus.ObjectUnready, CauseDispositionReason.ObjectReadinessFailed) =>
        rankEligibleClaimIds.isEmpty && relatedCauseEvidenceIds.isEmpty && relatedClaimIds.isEmpty
      case _ => false

private[chessjudgment] final case class CauseRDispositionAuthority(
    status: CauseDispositionStatus,
    reason: CauseDispositionReason,
    relatedCauseEvidenceIds: List[String]
)

/** Exact translation of already canonical R decisions into ledger state.
  * Both the assembler and packet closure check reuse this translation; neither
  * owns a second interpretation of dominance or cross-comparison status.
  */
private[chessjudgment] object CauseRDispositionAuthority:

  def from(
      dominance: RelativeCauseDominanceDecision,
      cross: Option[CrossComparisonExposureDecision]
  ): Option[CauseRDispositionAuthority] =
    if dominance.status == RelativeCauseDominanceStatus.DominatedFallback then
      Some(
        CauseRDispositionAuthority(
          CauseDispositionStatus.Dominated,
          CauseDispositionReason.DominatedFallback,
          dominance.dominatingCauseEvidenceIds.distinct.sorted
        )
      )
    else
      cross.map { decision =>
        decision.status match
          case CrossComparisonExposureStatus.SelectedPrimary |
              CrossComparisonExposureStatus.SelectedComplementary =>
            CauseRDispositionAuthority(
              CauseDispositionStatus.Selected,
              CauseDispositionReason.PlayerFacingSelection,
              Nil
            )
          case CrossComparisonExposureStatus.RedundantAcrossComparison =>
            CauseRDispositionAuthority(
              CauseDispositionStatus.Redundant,
              CauseDispositionReason.CrossComparisonRedundancy,
              List(decision.representativeCauseEvidenceId).distinct.sorted
            )
          case CrossComparisonExposureStatus.DiagnosticComparison =>
            CauseRDispositionAuthority(
              CauseDispositionStatus.Diagnostic,
              CauseDispositionReason.DiagnosticComparison,
              Nil
            )
          case CrossComparisonExposureStatus.InferiorAlternative =>
            CauseRDispositionAuthority(
              CauseDispositionStatus.Inferior,
              CauseDispositionReason.InferiorAlternative,
              Nil
            )
      }

/** Complete terminal ledger for the C-stage RelativeCause identity set. */
final case class CauseDispositionLedger(
    dispositions: List[CauseDisposition]
):
  require(
    dispositions.map(_.causeEvidence.id) == dispositions.map(_.causeEvidence.id).distinct.sorted,
    "Cause dispositions must be unique and ordered by Cause evidence id"
  )

  lazy val byCauseEvidenceId: Map[String, CauseDisposition] =
    dispositions.map(disposition => disposition.causeEvidence.id -> disposition).toMap

  def selectedCauseEvidenceIds: Set[String] =
    dispositions.collect {
      case disposition if disposition.status == CauseDispositionStatus.Selected =>
        disposition.causeEvidence.id
    }.toSet

  /** Packet-side closure check.  Jp/Ja history is carried by the ledger and
    * cannot be reconstructed from the ranked assembly, but the packet can and
    * must verify full C identity coverage plus every R-derived disposition,
    * final owner, and selected Cause exactly.
    */
  private[chessjudgment] def closedFor(
      graph: TypedEvidenceGraph,
      exposure: PlayerFacingCauseExposureResolution,
      rankedClaimIds: Set[String]
  ): Boolean =
    val causeRecords = graph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(_), _) => record
    }
    val causeRecordsById = causeRecords.map(record => record.ref.id -> record).toMap
    val causeIds = causeRecordsById.keySet
    val dispositionIds = byCauseEvidenceId.keySet
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
    val selectedIds = exposure.selectedCauseEvidenceIds

    def expectedRDisposition(
        disposition: CauseDisposition
    ): Option[CauseRDispositionAuthority] =
      dominanceByCauseId.get(disposition.causeEvidence.id).flatMap { dominance =>
        CauseRDispositionAuthority.from(dominance, crossByCauseId.get(disposition.causeEvidence.id))
      }

    val rCauseIds = readyHostsByCauseId.keySet
    val rDispositionsClosed = dispositions.forall { disposition =>
      val causeId = disposition.causeEvidence.id
      val expectedReadyHosts = readyHostsByCauseId.getOrElse(causeId, Nil)
      val expectedOwner = exposure.ownerClaimIdByCauseId.get(causeId)
      expectedRDisposition(disposition) match
        case Some(authority) =>
          disposition.rankEligibleClaimIds == expectedReadyHosts &&
            disposition.status == authority.status &&
            disposition.reason == authority.reason &&
            disposition.relatedCauseEvidenceIds == authority.relatedCauseEvidenceIds &&
            disposition.relatedClaimIds.isEmpty &&
            disposition.selectedOwnerClaimId == expectedOwner
        case None =>
          expectedReadyHosts.isEmpty &&
            disposition.rankEligibleClaimIds.isEmpty &&
            disposition.selectedOwnerClaimId.isEmpty &&
            !Set(
              CauseDispositionStatus.Selected,
              CauseDispositionStatus.Dominated,
              CauseDispositionStatus.Diagnostic,
              CauseDispositionStatus.Inferior
            )(disposition.status) &&
            !(disposition.status == CauseDispositionStatus.Redundant &&
              disposition.reason == CauseDispositionReason.CrossComparisonRedundancy)
    }

    causeIds.size == causeRecords.size &&
      dispositionIds == causeIds &&
      dispositions.forall(disposition =>
        causeRecordsById.get(disposition.causeEvidence.id).exists(_.ref == disposition.causeEvidence)
      ) &&
      readyHostsByCauseId.values.flatten.forall(rankedClaimIds) &&
      rCauseIds == dominanceByCauseId.keySet &&
      crossByCauseId.keySet == exposure.retainedCauseEvidenceIds &&
      exposure.ownerClaimIdByCauseId.keySet == selectedIds &&
      selectedCauseEvidenceIds == selectedIds &&
      rDispositionsClosed

object CauseDispositionLedger:
  val empty: CauseDispositionLedger = CauseDispositionLedger(Nil)
