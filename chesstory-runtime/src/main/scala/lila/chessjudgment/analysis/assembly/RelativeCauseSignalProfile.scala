package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

private[chessjudgment] final case class RelativeCauseDraft private (
    kind: RelativeCauseKind,
    support: List[EvidenceRecord],
    sourceSide: RelativeCauseSourceSide
)

private[chessjudgment] object RelativeCauseDraft:
  def fromExactSupport(
      support: List[EvidenceRecord]
  ): RelativeCauseDraft =
    val duplicateOwners = support
      .groupBy(_.ref.id)
      .collect { case (id, records) if records.size > 1 => id }
      .toList
      .sorted
    require(
      duplicateOwners.isEmpty,
      s"one exact relative Cause draft repeats support owners: ${duplicateOwners.mkString(",")}"
    )
    require(support.nonEmpty, "a relative Cause draft requires exact typed proof support")
    val typedChannels = support.map(record =>
      DirectCauseChannel.fromTypedProofRecord(record).getOrElse(
        throw IllegalArgumentException(
          s"relative Cause support is not an exact typed direct proof: ${record.ref.id}"
        )
      )
    )
    val family = typedChannels.head.familyMetadata
    require(
      typedChannels.tail.forall(_.familyMetadata == family),
      "one relative Cause draft cannot mix typed direct-proof families with different Cause meanings"
    )
    RelativeCauseDraft(
      kind = family.kind,
      support = support.sortBy(_.ref.id),
      sourceSide = family.sourceSide
    )

private[chessjudgment] final case class RelativeCauseSignalProfile(
    fact: CandidateComparisonFact,
    demandSource: EvidenceRecord,
    graph: TypedEvidenceGraph,
    referenceRecords: List[EvidenceRecord],
    candidateRecords: List[EvidenceRecord]
):
  require(
    demandSource.payload == CandidateComparisonEvidence(fact) &&
      graph.record(demandSource.ref).contains(demandSource) && graph.proofEligible(demandSource),
    "a relative cause profile needs its exact graph-owned comparison demand"
  )
  val referenceMoveOrderProofs: List[EvidenceRecord] =
    RelativeCauseSignalProfile.moveOrderCausalProofRecords(
      graph,
      referenceRecords,
      fact,
      demandSource
    )
  val referenceVacatedGateEnablesUnrecapturableSliderCaptures: List[EvidenceRecord] =
    RelativeCauseSignalProfile.vacatedGateEnablesUnrecapturableSliderCaptureRecords(
      graph,
      referenceRecords,
      fact,
      demandSource
    )

private[chessjudgment] object RelativeCauseDraftPlanner:
  def drafts(
      profile: RelativeCauseSignalProfile
  ): List[RelativeCauseDraft] =
    if !profile.fact.hasDistinctRootMoves then Nil
    else
      val candidates = rawDrafts(profile) ++ endpointPositiveDrafts(profile)
      def identity(draft: RelativeCauseDraft): (String, String, String) =
        (
          draft.kind.toString,
          draft.sourceSide.toString,
          draft.support.map(_.ref.id).mkString("|")
        )
      val duplicateIdentities = candidates
        .groupBy(identity)
        .collect { case (key, duplicates) if duplicates.size > 1 => key }
        .toList
        .sorted
      require(
        duplicateIdentities.isEmpty,
        s"one exact relative Cause draft may have only one producer: ${duplicateIdentities.mkString("; ")}"
      )
      candidates.sortBy(identity)

  /** The only candidate-owned positive Cause currently exposed is an exact
    * passed-pawn progress proof for an actionable played-vs-best comparison.
    * Other comparison kinds do not create dormant public Cause records.
    */
  private def endpointPositiveDrafts(
      profile: RelativeCauseSignalProfile
  ): List[RelativeCauseDraft] =
    Option
      .when(ActionablePlayedVsBestCausalProofDemand.accepts(profile.fact))(profile.candidateRecords)
      .toList
      .flatten
      .flatMap { record =>
        exactPassedPawnProgressRealizedAfterOnlyLegalReplyProofSupportRecord(
          record,
          profile.fact.candidateLine,
          profile.graph,
          profile.fact
        ).map(proofRecord => RelativeCauseDraft.fromExactSupport(List(proofRecord))).toList
      }

  private def rawDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    import profile.*
    List(
      causeDraft(
        referenceVacatedGateEnablesUnrecapturableSliderCaptures,
        referenceVacatedGateEnablesUnrecapturableSliderCaptures.nonEmpty &&
          ActionablePlayedVsBestCausalProofDemand.accepts(fact)
      ),
      causeDraft(
        referenceMoveOrderProofs,
        referenceMoveOrderProofs.nonEmpty &&
          ActionablePlayedVsBestCausalProofDemand.accepts(fact)
      ),
    ).flatten

  private def causeDraft(
      support: List[EvidenceRecord],
      condition: Boolean
  ): Option[RelativeCauseDraft] =
    Option.when(condition)(
      RelativeCauseDraft.fromExactSupport(support)
    )

  private def exactPassedPawnProgressRealizedAfterOnlyLegalReplyProofSupportRecord(
      record: EvidenceRecord,
      eventLine: LineNodeRef,
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact
  ): Option[EvidenceRecord] =
    record match
      case exact @ EvidenceRecord(ref, proof: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence, _)
          if ref.line.contains(eventLine) && proof.rootLine == eventLine &&
            graph.proofEligible(exact) &&
            proof.resultActor.side == fact.comparison.mover &&
            graph.record(proof.comparisonDemand).exists {
              case EvidenceRecord(_, CandidateComparisonEvidence(exact), _) => exact == fact
              case _ => false
            } =>
        Some(exact)
      case _ =>
        None

private[chessjudgment] object RelativeCauseSignalProfile:
  def from(
      fact: CandidateComparisonFact,
      demandSource: EvidenceRecord,
      graph: TypedEvidenceGraph,
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): RelativeCauseSignalProfile =
    RelativeCauseSignalProfile(
      fact = fact,
      demandSource = demandSource,
      graph = graph,
      referenceRecords = referenceRecords,
      candidateRecords = candidateRecords
    )

  private[chessjudgment] def moveOrderCausalProofRecords(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord],
      fact: CandidateComparisonFact,
      demand: EvidenceRecord
  ): List[EvidenceRecord] =
    val exactDependencies = for
      reference <- graph.uniqueProofEligibleLineFactRecordFor(fact.referenceLine).map(_._1)
      played <- graph.uniqueProofEligibleLineFactRecordFor(fact.candidateLine).map(_._1)
      if demand.payload == CandidateComparisonEvidence(fact)
      if graph.proofEligible(demand)
    yield (reference, played, demand)
    exactDependencies.toList.flatMap { case (reference, played, demand) =>
      records.collect {
        case record @ EvidenceRecord(_, payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _)
            if payload.occurrence.referenceLine == fact.referenceLine &&
              payload.occurrence.playedLine == fact.candidateLine &&
              payload.proofPaths.nonEmpty &&
              payload.consumesDependencies(fact, reference, played, demand) &&
              graph.proofEligible(record) =>
          record
        case record @ EvidenceRecord(_, payload: SoleRecapturerRemovalBeforeTargetCaptureEvidence, _)
            if payload.occurrence.referenceLine == fact.referenceLine &&
              payload.occurrence.playedLine == fact.candidateLine &&
              payload.proofPaths.nonEmpty &&
              payload.consumesDependencies(fact, reference, played, demand) &&
              graph.proofEligible(record) =>
          record
      }
    }

  private[chessjudgment] def vacatedGateEnablesUnrecapturableSliderCaptureRecords(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord],
      fact: CandidateComparisonFact,
      demand: EvidenceRecord
  ): List[EvidenceRecord] =
    val exactDependencies = for
      reference <- graph.uniqueProofEligibleLineFactRecordFor(fact.referenceLine).map(_._1)
      played <- graph.uniqueProofEligibleLineFactRecordFor(fact.candidateLine).map(_._1)
      if demand.payload == CandidateComparisonEvidence(fact) && graph.proofEligible(demand)
    yield (reference, played)
    exactDependencies.toList.flatMap { case (reference, played) =>
      records.collect {
        case record @ EvidenceRecord(_, payload: VacatedGateEnablesUnrecapturableSliderCaptureEvidence, _)
            if payload.occurrence.referenceLine == fact.referenceLine &&
              payload.occurrence.playedLine == fact.candidateLine &&
              payload.proofPaths.nonEmpty &&
              payload.consumesDependencies(fact, reference, played, demand) &&
              graph.proofEligible(record) =>
          record
      }
    }
