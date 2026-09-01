package lila.chessjudgment.model.judgment

enum PlayerFacingCauseExposureTier:
  case Primary
  case Complementary

final case class PlayerFacingCauseChannelSelection(
    channelId: String,
    exactOccurrence: RootOwnedEffectChannelOccurrenceFingerprint
)

final case class PlayerFacingCauseSelection(
    causeEvidence: EvidenceRef,
    exposure: PlayerFacingCauseExposureTier,
    channels: List[PlayerFacingCauseChannelSelection]
):
  require(channels.nonEmpty, "a player-facing Cause selection requires a direct causal channel")

object PlayerFacingCauseSelectionPolicy:

  private[judgment] def build(
      causeEvidence: EvidenceRef,
      exposure: PlayerFacingCauseExposureTier,
      channels: List[DirectCauseChannel]
  ): Option[PlayerFacingCauseSelection] =
    val exactChannels = channels.sortBy(channel =>
      channel.exactOccurrenceFingerprint.stableSortKey
    )
    val exactOccurrences = exactChannels.map(_.exactOccurrenceFingerprint)
    val transportReady =
      exactChannels.nonEmpty &&
        exactOccurrences.distinct.size == exactOccurrences.size &&
        exactOccurrences
          .groupBy(_.stableSortKey)
          .values
          .forall(group => group.distinct.size == 1)
    Option.when(transportReady)(
      PlayerFacingCauseSelection(
        causeEvidence = causeEvidence,
        exposure = exposure,
        channels = exactChannels.map { channel =>
          val occurrence = channel.exactOccurrenceFingerprint
          PlayerFacingCauseChannelSelection(
            channelId = s"cause-channel:${occurrence.stablePublicId(causeEvidence.id)}",
            exactOccurrence = occurrence
          )
        }
      )
    )

  private[judgment] def restoreExactChannels(
      selection: PlayerFacingCauseSelection,
      admittedChannels: List[DirectCauseChannel]
  ): Option[List[DirectCauseChannel]] =
    val byExactOccurrence = admittedChannels.groupBy(_.exactOccurrenceFingerprint)
    val restored = selection.channels.map { selected =>
      byExactOccurrence.getOrElse(selected.exactOccurrence, Nil) match
        case exact :: Nil => Some(exact)
        case _            => None
    }
    Option.when(restored.forall(_.nonEmpty))(restored.flatten)

/** Final R-owned certified Cause. Its exact Cause fact, selection, and every
  * typed proof channel travel together; independent channels and independent
  * Causes are never merged or deduplicated here.
  */
final case class PlayerFacingCertifiedCause(
    ownerClaimId: String,
    cause: RelativeCauseFact,
    selection: PlayerFacingCauseSelection,
    directChannels: List[DirectCauseChannel]
):
  require(ownerClaimId.nonEmpty, "a player-facing certified Cause requires a claim owner")
  require(
    selection.channels.map(channel =>
      channel.exactOccurrence
    ) == directChannels.map(channel =>
      channel.exactOccurrenceFingerprint
    ),
    "a player-facing certified Cause must retain every exact selected direct channel"
  )
  require(
    cause.proofSources.map(_.id) == directChannels.map(_.source.id).sorted,
    "a player-facing certified Cause must retain its exact typed proof owners"
  )

/** Graph-owned Cause records referenced by candidate claims. Exact typed
  * channel readiness is closed once per unique Cause in the exposure pipeline.
  */
object PlayerFacingCauseReadinessPolicy:

  def collectRegistered(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): List[(RelativeCauseFact, EvidenceRef)] =
    claim.evidence
      .flatMap(ref =>
        graph.record(ref).toList.collect {
          case record @ EvidenceRecord(registeredRef, RelativeCauseFactEvidence(cause), _)
              if registeredRef == ref && graph.proofEligible(record) =>
            cause -> registeredRef
        }
      )
      .sortBy { case (cause, ref) => (cause.kind.toString, ref.id) }

final case class PlayerFacingCauseExposureResolution(
    readyByClaim: Map[String, List[(RelativeCauseFact, EvidenceRef)]],
    ownerClaimIdByCauseId: Map[String, String],
    certifiedCauses: List[PlayerFacingCertifiedCause]
):
  def selectedCauseEvidenceIds: Set[String] =
    certifiedCauses.map(_.selection.causeEvidence.id).toSet

  def selectionsForClaim(claimId: String): List[PlayerFacingCauseSelection] =
    certifiedCauses.collect { case certified if certified.ownerClaimId == claimId => certified.selection }

object PlayerFacingCauseExposurePipeline:

  def resolve(
      claims: List[JudgmentClaim],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): PlayerFacingCauseExposureResolution =
    val normalizedPlayedMoves = playedMoves.map(EvidenceRef.normalizeMove).filter(_.nonEmpty)
    val registeredByClaim = claims.map(claim =>
      claim.id -> PlayerFacingCauseReadinessPolicy.collectRegistered(claim, graph)
    ).toMap
    val registeredGroups = registeredByClaim.values.flatten.toList.groupBy(_._2.id)
    registeredGroups.foreach { case (causeId, owners) =>
      require(
        owners.map(_._1).distinct.size == 1 && owners.map(_._2).distinct.size == 1,
        s"one registered Cause id '$causeId' resolves to conflicting exact facts"
      )
    }
    val readyWithChannels = registeredGroups.toList.sortBy(_._1).flatMap { case (_, owners) =>
      val (cause, ref) = owners.head
      val channels = RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
      Option.when(RelativeCauseConstructionAdmission.initiallyReadyWithChannels(cause, graph, channels))(
        (cause, ref, channels)
      )
    }
    val readyIds = readyWithChannels.map(_._2.id).toSet
    val readyByClaim = registeredByClaim.view.mapValues(_.filter(entry => readyIds(entry._2.id))).toMap
    val allReady = readyWithChannels.map { case (cause, ref, _) => cause -> ref }
    val directChannelsByCauseId = readyWithChannels.map { case (_, ref, channels) =>
      ref.id -> channels
    }.toMap
    val selections = PlayerFacingCauseAdmissionPolicy.resolve(
      allReady,
      directChannelsByCauseId,
      graph,
      normalizedPlayedMoves
    )
    val selectedIds = selections.map(_.causeEvidence.id)
    require(
      selectedIds.distinct.size == selectedIds.size && selectedIds.toSet == readyIds,
      "every R-ready Cause must own one supported actionable PVB typed-proof admission"
    )

    val causesById = allReady.map { case (cause, ref) => ref.id -> cause }.toMap
    val ownerClaimIdByCauseId = selectedIds.flatMap { causeId =>
      val owningClaims = claims.filter(claim =>
        readyByClaim.getOrElse(claim.id, Nil).exists(_._2.id == causeId)
      )
      PlayerFacingCauseClaimOwnershipPolicy
        .ownerClaimId(causesById(causeId), owningClaims)
        .map(ownerId => causeId -> ownerId)
    }.toMap
    require(
      ownerClaimIdByCauseId.keySet == selectedIds.toSet,
      "every admitted Cause must own one player-facing claim host"
    )

    val certifiedCauses = selections.map { selection =>
      val causeId = selection.causeEvidence.id
      val directChannels = PlayerFacingCauseSelectionPolicy
        .restoreExactChannels(selection, directChannelsByCauseId(causeId))
        .getOrElse(
          throw IllegalStateException(
            s"selected Cause channel occurrence cannot be restored exactly: $causeId"
          )
        )
      PlayerFacingCertifiedCause(
        ownerClaimId = ownerClaimIdByCauseId(causeId),
        cause = causesById(causeId),
        selection = selection,
        directChannels = directChannels
      )
    }
    PlayerFacingCauseExposureResolution(
      readyByClaim = readyByClaim,
      ownerClaimIdByCauseId = ownerClaimIdByCauseId,
      certifiedCauses = certifiedCauses
    )

/** Sole R-stage admission. It consumes only typed lower proofs already bound
  * to the actionable PlayedVsBest occurrence. No score magnitude, alternate
  * comparison, representative election, or semantic deduplication is allowed.
  */
object PlayerFacingCauseAdmissionPolicy:

  def resolve(
      causes: List[(RelativeCauseFact, EvidenceRef)],
      directChannelsByCauseId: Map[String, List[DirectCauseChannel]],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): List[PlayerFacingCauseSelection] =
    require(
      directChannelsByCauseId.keySet == causes.map(_._2.id).toSet,
      "player-facing Cause admission needs one precomputed channel set per ready Cause"
    )
    val normalizedPlayedMoves = playedMoves.map(EvidenceRef.normalizeMove).filter(_.nonEmpty)
    causes
      .flatMap { case (cause, ref) =>
        graph.record(ref).flatMap {
          case EvidenceRecord(registeredRef, RelativeCauseFactEvidence(registered), _)
              if registeredRef == ref && registered == cause =>
            admit(
              cause,
              ref,
              directChannelsByCauseId(ref.id),
              graph,
              normalizedPlayedMoves
            )
          case _ => None
        }
      }
      .sortBy(admission =>
        (
          admission.exposure.ordinal,
          admission.causeEvidence.id
        )
      )

  private def admit(
      cause: RelativeCauseFact,
      ref: EvidenceRef,
      channels: List[DirectCauseChannel],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Option[PlayerFacingCauseSelection] =
    for
      comparison <- graph.comparisonFor(cause)
      binding <- graph.relativeCauseBinding(cause)
      if comparison.kind == CandidateComparisonKind.PlayedVsBest
      if comparison.hasDistinctRootMoves
      if comparison.comparison.verdict.isActionableLoss
      if comparison.candidateLine.rootMoveIn(playedMoves)
      if !comparison.referenceLine.rootMoveIn(playedMoves)
      exposure <- exposureFor(cause, binding.eventLine, comparison)
      selection <- PlayerFacingCauseSelectionPolicy.build(
        causeEvidence = ref,
        exposure = exposure,
        channels = channels
      )
    yield selection

  private def exposureFor(
      cause: RelativeCauseFact,
      eventLine: LineNodeRef,
      comparison: CandidateComparisonFact
  ): Option[PlayerFacingCauseExposureTier] =
    (cause.kind, cause.sourceSide) match
      case (
            RelativeCauseKind.WrongMoveOrder,
            RelativeCauseSourceSide.Reference
          ) if eventLine == comparison.referenceLine =>
        Some(PlayerFacingCauseExposureTier.Primary)
      case (
            RelativeCauseKind.MissedTacticalResource,
            RelativeCauseSourceSide.Reference
          ) if eventLine == comparison.referenceLine =>
        Some(PlayerFacingCauseExposureTier.Primary)
      case (
            RelativeCauseKind.MissedSquareRelease,
            RelativeCauseSourceSide.Reference
          ) if eventLine == comparison.referenceLine =>
        Some(PlayerFacingCauseExposureTier.Primary)
      case (
            RelativeCauseKind.PassedPawnProgress,
            RelativeCauseSourceSide.Candidate
          ) if eventLine == comparison.candidateLine =>
        Some(PlayerFacingCauseExposureTier.Complementary)
      case _ => None

/** A selected Cause owns one public claim host even when proposal assembly
  * certified the same Cause under several families.
  */
object PlayerFacingCauseClaimOwnershipPolicy:

  def ownerClaimId(
      cause: RelativeCauseFact,
      claims: Iterable[JudgmentClaim]
  ): Option[String] =
    claims.toList.sortBy(claim => ownershipOrder(cause, claim)).headOption.map(_.id)

  private def ownershipOrder(
      cause: RelativeCauseFact,
      claim: JudgmentClaim
  ): (Int, Int, String) =
    val preferredFamily = ClaimFamily.fromCause(cause.kind)
    (
      if preferredFamily == claim.family then 0 else 1,
      claim.family.ordinal,
      claim.id
    )
