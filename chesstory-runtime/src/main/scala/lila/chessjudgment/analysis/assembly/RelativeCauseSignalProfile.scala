package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.evaluation.JudgmentThresholds
import lila.chessjudgment.model.line.CandidateLineEvaluation
import lila.chessjudgment.model.judgment.*

/** Generation intent only. This classification may decide which drafts are
  * proposed, but it never grants public Cause admission.
  */
private[chessjudgment] enum RelativeCauseDraftIntent:
  case EndpointPositive
  case EvaluativeFailure

private[chessjudgment] object RelativeCauseDraftIntent:
  private val EndpointPositiveKinds = Set(
    RelativeCauseKind.RecaptureRecoveryWindow,
    RelativeCauseKind.PassedPawnResult,
    RelativeCauseKind.DrawResource,
    RelativeCauseKind.KingForcing,
    RelativeCauseKind.MaterialSwing
  )

  def classify(
      kind: RelativeCauseKind,
      attributionKind: CauseAttributionKind
  ): RelativeCauseDraftIntent =
    val createsEndpointValue =
      attributionKind == CauseAttributionKind.ReferenceCreatesResource ||
        attributionKind == CauseAttributionKind.CandidateCreatesValue
    if createsEndpointValue && EndpointPositiveKinds(kind) then
      RelativeCauseDraftIntent.EndpointPositive
    else RelativeCauseDraftIntent.EvaluativeFailure

private[chessjudgment] final case class RelativeCauseDraft(
    kind: RelativeCauseKind,
    support: List[EvidenceRecord],
    sourceSide: Option[RelativeCauseSourceSide] = None,
    attributionKind: CauseAttributionKind = CauseAttributionKind.Unattributed,
    intent: RelativeCauseDraftIntent = RelativeCauseDraftIntent.EvaluativeFailure
)

private[chessjudgment] object RelativeCauseDraft:
  def classified(
      kind: RelativeCauseKind,
      support: List[EvidenceRecord],
      sourceSide: Option[RelativeCauseSourceSide],
      attributionKind: CauseAttributionKind
  ): RelativeCauseDraft =
    RelativeCauseDraft(
      kind = kind,
      support = support.distinctBy(_.ref.id),
      sourceSide = sourceSide,
      attributionKind = attributionKind,
      intent = RelativeCauseDraftIntent.classify(kind, attributionKind)
    )

private[chessjudgment] final case class RelativeCauseSignalProfile(
    fact: CandidateComparisonFact,
    demandSource: EvidenceRecord,
    graph: TypedEvidenceGraph,
    referenceRecords: List[EvidenceRecord],
    candidateRecords: List[EvidenceRecord],
    sharedRecords: List[EvidenceRecord]
):
  require(
    demandSource.payload == CandidateComparisonEvidence(fact) &&
      graph.record(demandSource.ref).contains(demandSource) && graph.proofEligible(demandSource),
    "a relative cause profile needs its exact graph-owned comparison demand"
  )
  val involvedRecords: List[EvidenceRecord] =
    (referenceRecords ++ candidateRecords).distinctBy(_.ref.id)
  val allRecords: List[EvidenceRecord] =
    (referenceRecords ++ candidateRecords ++ sharedRecords).distinctBy(_.ref.id)
  val badLoss: Boolean =
    fact.comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP
  val actionablePlayedLoss: Boolean =
    fact.kind == CandidateComparisonKind.PlayedVsBest &&
      fact.comparison.verdict.isActionableLoss
  val tacticalLoss: Boolean =
    fact.comparison.winPercentLossForMover >= JudgmentThresholds.TACTICAL_IMPACT_WP
  val majorLoss: Boolean =
    fact.comparison.winPercentLossForMover >= JudgmentThresholds.MATERIAL_IMPACT_WP
  val playedCandidateSideComparison: Boolean =
    RelativeCauseDraftPlanner.playedMoveCandidateSideComparison(fact.kind)

  val referenceRecaptureResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.recaptureResourceRecords(referenceRecords, fact.referenceLine.rootMove)
  val referenceRootOwnedTacticalResource: List[EvidenceRecord] =
    RootOwnedCausePolicy.forcingResourceRecords(graph, referenceRecords, fact.referenceLine)
  val candidateRecaptureResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.recaptureResourceRecords(candidateRecords, fact.candidateLine.rootMove)
  val referenceMoveOrderResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.forcedReplyResourceDifferentialRecords(
      graph,
      referenceRecords,
      fact,
      demandSource
    )
  val referenceOpponentPromotionLiability: List[EvidenceRecord] =
    RelativeCauseSignalProfile.opponentPromotionLiabilityRecords(
      fact,
      RelativeCauseSourceSide.Reference,
      referenceRecords
    )
  val candidateOpponentPromotionLiability: List[EvidenceRecord] =
    RelativeCauseSignalProfile.opponentPromotionLiabilityRecords(
      fact,
      RelativeCauseSourceSide.Candidate,
      candidateRecords
    )
  val candidateTacticalMechanism: List[EvidenceRecord] =
    RelativeCauseSignalProfile.tacticalMechanismRecords(graph, candidateRecords)
  val candidateMaterialLossLiability: List[EvidenceRecord] =
    RelativeCauseSignalProfile.candidateMaterialLossLiabilityRecords(fact, candidateRecords)

private[chessjudgment] object RelativeCauseDraftPlanner:
  def drafts(
      profile: RelativeCauseSignalProfile
  ): List[RelativeCauseDraft] =
    if !profile.fact.hasDistinctRootMoves then Nil
    else
      val candidates = rawDrafts(profile) ++ endpointPositiveDrafts(profile)
      def identity(draft: RelativeCauseDraft): (String, String, String, String, String) =
        (
          draft.kind.toString,
          draft.sourceSide.map(_.toString).getOrElse("none"),
          draft.attributionKind.toString,
          draft.intent.toString,
          draft.support.map(_.ref.id).sorted.mkString("|")
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

  def playedMoveCandidateSideComparison(kind: CandidateComparisonKind): Boolean =
    kind == CandidateComparisonKind.PlayedVsBest || kind == CandidateComparisonKind.PlayedVsAlternative

  /** Exact positive ideas are generated symmetrically for both endpoints.
    * Evaluation loss remains relevant only to EvaluativeFailure drafts; the
    * endpoint delta later decides whether these specific effects are novel.
    */
  private def endpointPositiveDrafts(
      profile: RelativeCauseSignalProfile
  ): List[RelativeCauseDraft] =
    List(
      (
        RelativeCauseSourceSide.Reference,
        profile.fact.referenceLine,
        profile.referenceRecords
      ),
      (
        RelativeCauseSourceSide.Candidate,
        profile.fact.candidateLine,
        profile.candidateRecords
      )
    ).flatMap { case (sourceSide, eventLine, records) =>
      endpointPositiveDraftsFor(profile, sourceSide, eventLine, records)
    }

  private def endpointPositiveDraftsFor(
      profile: RelativeCauseSignalProfile,
      sourceSide: RelativeCauseSourceSide,
      eventLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[RelativeCauseDraft] =
    val attributionKind =
      if sourceSide == RelativeCauseSourceSide.Reference then CauseAttributionKind.ReferenceCreatesResource
      else CauseAttributionKind.CandidateCreatesValue
    def draft(kind: RelativeCauseKind, support: List[EvidenceRecord]): Option[RelativeCauseDraft] =
      Option
        .when(support.nonEmpty)(
          RelativeCauseDraft.classified(kind, support, Some(sourceSide), attributionKind)
        )
        .filter(_.intent == RelativeCauseDraftIntent.EndpointPositive)

    def nonTactical(records: List[EvidenceRecord]): List[EvidenceRecord] =
      records.filterNot(_.payload.isInstanceOf[TacticalMechanismEvidence])

    val classifiedSupport = List(
      RelativeCauseKind.RecaptureRecoveryWindow -> nonTactical(
        RelativeCauseSignalProfile.recaptureResourceRecords(records, eventLine.rootMove)
      ),
      RelativeCauseKind.KingForcing -> nonTactical(
        RelativeCauseSignalProfile.forcingLineResourceRecords(records, eventLine)
      ),
      RelativeCauseKind.DrawResource -> nonTactical(
        RelativeCauseSignalProfile.drawResourceRecords(records)
      ),
      RelativeCauseKind.MaterialSwing -> nonTactical(
        RelativeCauseSignalProfile.materialGainRecords(profile.graph, records)
      )
    ).flatMap { case (kind, support) => support.map(kind -> _) } ++
      RelativeCauseSignalProfile.tacticalMechanismRecords(profile.graph, records).flatMap {
        case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          endpointPositiveMechanismCauseKind(payload.kind).map(_ -> record)
        case _ =>
          None
      }
    val classifiedKeys = classifiedSupport.map { case (kind, record) => kind -> record.ref.id }
    require(
      classifiedKeys.distinct.size == classifiedKeys.size,
      "one endpoint-positive evidence record may have only one producer per Cause meaning"
    )
    val grouped = classifiedSupport
      .groupMap(_._1)(_._2)
      .toList
      .sortBy(_._1.toString)
      .flatMap { case (kind, support) => draft(kind, support) }
    val passedPawnResults = records.flatMap { record =>
      exactPassedPawnResultProofSupportRecord(
        RelativeCauseKind.PassedPawnResult,
        record,
        eventLine,
        profile.graph,
        profile.fact
      ).flatMap(proofRecord => draft(RelativeCauseKind.PassedPawnResult, List(proofRecord))).toList
    }
    grouped ++ passedPawnResults

  private def rawDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    import profile.*
    List(
      causeDraft(
        RelativeCauseKind.MissedTacticalResource,
        referenceRecaptureResource,
        referenceRecaptureResource.nonEmpty && candidateRecaptureResource.isEmpty && badLoss,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      ),
      causeDraft(
        RelativeCauseKind.MissedTacticalResource,
        referenceRootOwnedTacticalResource,
        referenceRootOwnedTacticalResource.nonEmpty && badLoss,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      ),
      causeDraft(
        RelativeCauseKind.WrongMoveOrder,
        referenceMoveOrderResource,
        referenceMoveOrderResource.nonEmpty &&
          ForcedReplyResourceDifferentialDemand.accepts(fact),
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      ),
      causeDraft(
        if playedCandidateSideComparison then RelativeCauseKind.TacticalRefutationOfPlayed
        else RelativeCauseKind.CandidateTacticalLiability,
        candidateOpponentPromotionLiability,
        candidateOpponentPromotionLiability.nonEmpty && referenceOpponentPromotionLiability.isEmpty && badLoss,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateAllowsLiability
      ),
      causeDraft(
        RelativeCauseKind.MaterialSwing,
        candidateMaterialLossLiability,
        majorLoss &&
          candidateMaterialLossLiability.nonEmpty,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateAllowsLiability
      ),
    ).flatten ++ candidateLiabilityMechanismDrafts(profile)

  private def causeDraft(
      kind: RelativeCauseKind,
      support: List[EvidenceRecord],
      condition: Boolean,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind
  ): Option[RelativeCauseDraft] =
    Option.when(condition)(
      RelativeCauseDraft.classified(kind, support, Some(sourceSide), attributionKind)
    )

  private def candidateLiabilityMechanismDrafts(
      profile: RelativeCauseSignalProfile
  ): List[RelativeCauseDraft] =
    val actionableLoss = profile.fact.comparison.verdict.isActionableLoss
    Option
      .when(actionableLoss && profile.tacticalLoss)(
        mechanismCauseKinds(
          profile.candidateTacticalMechanism.filter(
            candidateBadMechanismCanProveLiability(_, profile)
          ),
          badLoss = true,
          playedCandidate = profile.playedCandidateSideComparison,
          sourceSide = RelativeCauseSourceSide.Candidate,
          attributionKind = CauseAttributionKind.CandidateAllowsLiability
        )
      )
      .getOrElse(Nil)

  private def mechanismCauseKinds(
      records: List[EvidenceRecord],
      badLoss: Boolean,
      playedCandidate: Boolean = false,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind
  ): List[RelativeCauseDraft] =
    records.flatMap {
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.canAnchorTacticalClaim =>
        val causeKind = TacticalMechanismKind.relativeCauseKind(payload.kind, badLoss, playedCandidate)
        causeKind.map(kind =>
          RelativeCauseDraft.classified(kind, List(record), Some(sourceSide), attributionKind)
        )
      case _ =>
        None
    }

  private[chessjudgment] def endpointPositiveMechanismCauseKind(
      mechanismKind: TacticalMechanismKind
  ): Option[RelativeCauseKind] =
    mechanismKind match
      case TacticalMechanismKind.MaterialGain =>
        Some(RelativeCauseKind.MaterialSwing)
      case TacticalMechanismKind.PawnPromotion =>
        None
      case _ =>
        TacticalMechanismKind.relativeCauseKind(mechanismKind, badLoss = false, playedCandidate = false)

  private def candidateBadMechanismCanProveLiability(
      record: EvidenceRecord,
      profile: RelativeCauseSignalProfile
  ): Boolean =
    record.payload match
      case payload: TacticalMechanismEvidence =>
        payload.kind match
          case TacticalMechanismKind.RecaptureChoice =>
            false
          case TacticalMechanismKind.KingForcing =>
            candidateNegativeLineConsequences(record, profile).exists(
              _.kind == LineConsequenceKind.Mate
            )
          case _ =>
            false
      case _ =>
        false

  private def candidateNegativeLineConsequences(
      record: EvidenceRecord,
      profile: RelativeCauseSignalProfile
  ): List[LineConsequence] =
    val sourceLabelsById = record.payload match
      case payload: TacticalMechanismEvidence => payload.lineConsequenceSourceLabelsByEvidenceId
      case _                                  => Map.empty[String, Set[String]]
    if sourceLabelsById.isEmpty then Nil
    else
      TypedEvidenceGraph
        .ownedLineConsequences(
          profile.candidateRecords,
          profile.fact,
          RelativeCauseSourceSide.Candidate,
          CauseAttributionKind.CandidateAllowsLiability,
          sourceLabelsById
        )
        .map(_._2)

  private def exactPassedPawnResultProofSupportRecord(
      kind: RelativeCauseKind,
      record: EvidenceRecord,
      eventLine: LineNodeRef,
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact
  ): Option[EvidenceRecord] =
    record match
      case exact @ EvidenceRecord(ref, proof: PassedPawnResultProofEvidence, _)
          if RelativeCauseKind.requiresExactPassedPawnResult(kind) &&
            ref.line.contains(eventLine) && proof.rootLine == eventLine &&
            graph.proofEligible(exact) &&
            proof.resultActor.side == fact.comparison.mover &&
            graph.record(proof.comparisonDemand).exists {
              case EvidenceRecord(_, CandidateComparisonEvidence(exact), _) => exact == fact
              case _ => false
            } &&
            RelativeCauseKind.passedPawnResultProofCanProveCause(kind, proof) =>
        Some(exact)
      case _ =>
        None

private[chessjudgment] object RelativeCauseSignalProfile:
  def from(
      fact: CandidateComparisonFact,
      demandSource: EvidenceRecord,
      graph: TypedEvidenceGraph,
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord]
  ): RelativeCauseSignalProfile =
    RelativeCauseSignalProfile(
      fact = fact,
      demandSource = demandSource,
      graph = graph,
      referenceRecords = referenceRecords,
      candidateRecords = candidateRecords,
      sharedRecords = sharedRecords
    )

  private[chessjudgment] def forcedReplyResourceDifferentialRecords(
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
        case record @ EvidenceRecord(_, payload: ForcedReplyResourceDifferentialEvidence, _)
            if payload.occurrence.referenceLine == fact.referenceLine &&
              payload.occurrence.playedLine == fact.candidateLine &&
              payload.proofPaths.nonEmpty &&
              payload.consumesDependencies(fact, reference, played, demand) &&
              graph.proofEligible(record) =>
          record
      }
    }

  private[chessjudgment] def tacticalMechanismRecords(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.filter {
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        (payload.canAnchorTacticalClaim || payload.canAnchorDefensiveClaim) &&
          (
            payload.kind != TacticalMechanismKind.MaterialGain ||
              materialGainMechanismOwnsAdmissiblePrimitive(graph, record, payload, records)
          )
      case _ =>
        false
    }

  private[chessjudgment] def recaptureResourceRecords(
      records: List[EvidenceRecord],
      rootMove: String
  ): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.rootIsRecapture(rootMove) &&
          (
            payload.consequencesForRootMove(rootMove).exists(consequence =>
              consequence.kind == LineConsequenceKind.RecaptureSequence ||
                consequence.kind == LineConsequenceKind.RecoveryWindow
            ) ||
              payload.hasMaterialRecaptureChain ||
              payload.hasClosedMaterialRecovery
          )
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.RecaptureChoice &&
          payload.canAnchorTacticalClaim &&
          payload.moveUci.exists(EvidenceRef.sameMove(_, rootMove))
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def forcingLineResourceRecords(
      records: List[EvidenceRecord],
      eventLine: LineNodeRef
  ): List[EvidenceRecord] =
    val rootMove = eventLine.rootMove
    records.filter {
      case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
        ref.line.contains(eventLine) &&
          payload.line == eventLine &&
          (
            payload.eventsForRootMove(rootMove).exists(event =>
              event.kind == LineEventKind.Check || event.kind == LineEventKind.Mate
            ) ||
              payload.consequencesForRootMove(rootMove).exists(_.kind == LineConsequenceKind.Mate)
          )
      case EvidenceRecord(ref, CandidateLineEvaluationEvidence(_, CandidateLineEvaluation.EngineSearch(line)), _) =>
        ref.line.contains(eventLine) && line.mate.nonEmpty
      case EvidenceRecord(ref, payload: TacticalMechanismEvidence, _) =>
        ref.line.contains(eventLine) &&
          payload.line.contains(eventLine) &&
          payload.moveUci.exists(EvidenceRef.sameMove(_, rootMove)) &&
          payload.kind == TacticalMechanismKind.KingForcing &&
          payload.hasConcreteProof
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def drawResourceRecords(
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasDirectCauseProjectionEligibleConsequence(LineConsequenceKind.DrawResource)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.DrawResource && payload.canAnchorDefensiveClaim
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def opponentPromotionLiabilityRecords(
      fact: CandidateComparisonFact,
      sourceSide: RelativeCauseSourceSide,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    val liabilityRecordIds =
      TypedEvidenceGraph
        .ownedLineConsequences(
          records,
          fact,
          sourceSide,
          CauseAttributionKind.CandidateAllowsLiability
        )
        .collect {
          case (ref, consequence) if consequence.kind == LineConsequenceKind.Promotion =>
            ref.id
        }
        .toSet
    records.filter(record => liabilityRecordIds.contains(record.ref.id)).distinctBy(_.ref.id)

  private[chessjudgment] def materialGainRecords(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload
          .rootOwnedCausalEpisodes(payload.line.rootMove)
          .exists(_.consequence.kind == LineConsequenceKind.MaterialGain)
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.kind == TacticalMechanismKind.MaterialGain &&
          payload.canAnchorTacticalClaim &&
          materialGainMechanismOwnsAdmissiblePrimitive(graph, record, payload, records)
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private def materialGainMechanismOwnsAdmissiblePrimitive(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      mechanism: TacticalMechanismEvidence,
      records: List[EvidenceRecord]
  ): Boolean =
    mechanism.line.exists { eventLine =>
      RootOwnedEffectPolicy.tacticalCarrierOwnsEventRoot(graph, record.ref, mechanism, eventLine) &&
        mechanism.signals.exists(signal =>
          signal.source.exists(source =>
            records
              .find(_.ref == source)
              .exists(RootOwnedEffectPolicy.materialGainPrimitiveOwnsEventRoot(signal, _, eventLine))
          )
        )
    }

  private def materialLossRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload
          .rootOwnedCausalEpisodes(payload.line.rootMove)
          .exists(_.consequence.kind == LineConsequenceKind.MaterialLoss)
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def candidateMaterialLossLiabilityRecords(
      fact: CandidateComparisonFact,
      candidateRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    val ownedRecordIds =
      TypedEvidenceGraph
        .ownedLineConsequences(
          candidateRecords,
          fact,
          RelativeCauseSourceSide.Candidate,
          CauseAttributionKind.CandidateAllowsLiability
        )
        .collect {
          case (ref, consequence) if consequence.kind == LineConsequenceKind.MaterialLoss => ref.id
        }
        .toSet
    candidateRecords.filter(record => ownedRecordIds(record.ref.id)).distinctBy(_.ref.id)
