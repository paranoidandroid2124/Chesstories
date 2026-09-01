package lila.chessjudgment.model.judgment

/** Terminal owner of one fully constructed C-stage Cause. There is no
  * non-selected terminal state: construction succeeded, so failure to expose
  * the Cause is an invariant violation rather than a disposition.
  */
final case class CauseDisposition(
    causeEvidence: EvidenceRef,
    selectedOwnerClaimId: String
):
  require(
    causeEvidence.layer == EvidenceLayer.RelativeCause,
    "a Cause disposition must identify a RelativeCause record"
  )
  require(selectedOwnerClaimId.trim.nonEmpty, "a Cause disposition requires its selected claim owner")

/** Complete terminal owner ledger for the C-stage RelativeCause identity set. */
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
    byCauseEvidenceId.keySet

  private[chessjudgment] def closedFor(
      graph: TypedEvidenceGraph,
      exposure: PlayerFacingCauseExposureResolution,
      certifiedClaimIds: Set[String]
  ): Boolean =
    val causeRecords = graph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(_), _) => record
    }
    val causeRecordsById = causeRecords.map(record => record.ref.id -> record).toMap
    val causeIds = causeRecordsById.keySet
    val certifiedByCauseId = exposure.certifiedCauses.map(certified =>
      certified.selection.causeEvidence.id -> certified
    ).toMap
    val readyHostPairs = exposure.readyByClaim.toList.flatMap { case (claimId, causes) =>
      causes.map { case (_, causeRef) => causeRef.id -> claimId }
    }
    val readyHostsByCauseId = readyHostPairs
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.sorted)
      .toMap
    val readyHostPairsUnique = readyHostPairs.distinct.size == readyHostPairs.size
    val selectedIds = exposure.selectedCauseEvidenceIds

    causeIds.size == causeRecords.size &&
      byCauseEvidenceId.keySet == causeIds &&
      selectedIds == causeIds &&
      certifiedByCauseId.keySet == causeIds &&
      certifiedByCauseId.size == exposure.certifiedCauses.size &&
      readyHostsByCauseId.keySet == causeIds &&
      exposure.ownerClaimIdByCauseId.keySet == causeIds &&
      readyHostPairsUnique &&
      dispositions.forall { disposition =>
        val causeId = disposition.causeEvidence.id
        val owner = disposition.selectedOwnerClaimId
        causeRecordsById.get(causeId).exists {
          case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) =>
            ref == disposition.causeEvidence && certifiedByCauseId.get(causeId).exists(certified =>
              certified.selection.causeEvidence == ref && certified.cause == cause
            )
          case _ => false
        } &&
          exposure.ownerClaimIdByCauseId.get(causeId).contains(owner) &&
          readyHostsByCauseId.getOrElse(causeId, Nil).contains(owner) &&
          certifiedClaimIds(owner)
      }
