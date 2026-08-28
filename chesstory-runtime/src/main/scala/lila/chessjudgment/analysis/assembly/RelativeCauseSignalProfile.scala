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
    RelativeCauseKind.ConversionSecured,
    RelativeCauseKind.SacrificeCompensation,
    RelativeCauseKind.PlanImprovement,
    RelativeCauseKind.DefensiveResource,
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
    graph: TypedEvidenceGraph,
    referenceRecords: List[EvidenceRecord],
    candidateRecords: List[EvidenceRecord],
    sharedRecords: List[EvidenceRecord],
    referenceEndpointMoveOrderRecords: List[EvidenceRecord] = Nil,
    candidateEndpointMoveOrderRecords: List[EvidenceRecord] = Nil
):
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
    fact.comparison.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP
  val majorLoss: Boolean =
    fact.comparison.winPercentLossForMover >= JudgmentThresholds.MATERIAL_THREAT_WP
  val candidateBetter: Boolean =
    fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP
  val exactReferenceMove: Boolean =
    fact.kind == CandidateComparisonKind.PlayedVsBest &&
      fact.comparison.verdict == MoveChoiceVerdict.MatchesReference
  val candidateProvedValue: Boolean =
    candidateBetter || exactReferenceMove
  val primaryPlayedPositive: Boolean =
    fact.kind == CandidateComparisonKind.PlayedVsBest && candidateBetter
  val playedCandidateSideComparison: Boolean =
    RelativeCauseDraftPlanner.playedMoveCandidateSideComparison(fact.kind)

  val referenceDrawResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.drawResourceRecords(referenceRecords)
  val candidateDrawResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.drawResourceRecords(candidateRecords)
  val referenceRecaptureResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.recaptureResourceRecords(referenceRecords, fact.referenceLine.rootMove)
  val referenceRootOwnedTacticalResource: List[EvidenceRecord] =
    RootOwnedCausePolicy.forcingResourceRecords(graph, referenceRecords, fact.referenceLine)
  val candidateRecaptureResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.recaptureResourceRecords(candidateRecords, fact.candidateLine.rootMove)
  val referenceMoveOrderResource: List[EvidenceRecord] =
    referenceEndpointMoveOrderRecords.distinctBy(_.ref.id)
  val candidateMoveOrderResource: List[EvidenceRecord] =
    candidateEndpointMoveOrderRecords.distinctBy(_.ref.id)
  val candidateTempoLiability: List[EvidenceRecord] =
    RelativeCauseSignalProfile.tempoLiabilityRecords(fact, candidateRecords)
  val referenceForcingLineResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.forcingLineResourceRecords(
      referenceRecords,
      fact.referenceLine
    )
  val candidateForcingLineResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.forcingLineResourceRecords(
      candidateRecords,
      fact.candidateLine
    )
  val referenceDefensiveResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.defensiveResourceRecords(referenceRecords)
  val candidateDefensiveResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.defensiveResourceRecords(candidateRecords)
  val sharedDefensiveResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.defensiveResourceRecords(sharedRecords)
  val referencePromotionResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.promotionRaceRecords(referenceRecords)
  val candidatePromotionResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.promotionRaceRecords(candidateRecords)
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
  val referencePassedPawnResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.passedPawnResourceRecords(referenceRecords)
  val candidatePassedPawnResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.passedPawnResourceRecords(candidateRecords)
  val candidatePassedPawnConcession: List[EvidenceRecord] =
    RelativeCauseSignalProfile.passedPawnConcessionRecords(candidateRecords)
  val referenceTacticalMechanism: List[EvidenceRecord] =
    RelativeCauseSignalProfile.tacticalMechanismRecords(graph, referenceRecords)
  val candidateTacticalMechanism: List[EvidenceRecord] =
    RelativeCauseSignalProfile.tacticalMechanismRecords(graph, candidateRecords)
  val candidateThreatBranchTacticalMechanism: List[EvidenceRecord] =
    RelativeCauseSignalProfile.threatBranchTacticalMechanismRecords(graph, candidateRecords)
  val candidateMaterialLossLiability: List[EvidenceRecord] =
    RelativeCauseSignalProfile.candidateMaterialLossLiabilityRecords(fact, candidateRecords)
  val strategicContrasts: List[EvidenceRecord] =
    RelativeCauseSignalProfile.strategicContrastRecords(fact, allRecords)
  val candidateCurrentMoveStrategicSupport: List[EvidenceRecord] =
    RelativeCauseSignalProfile.currentMoveStrategicSupportRecords(
      fact,
      if exactReferenceMove then candidateRecords ++ sharedRecords else candidateRecords,
      graph
    )

private[chessjudgment] object RelativeCauseDraftPlanner:
  def drafts(
      profile: RelativeCauseSignalProfile
  ): List[RelativeCauseDraft] =
    if !profile.fact.hasDistinctRootMoves then Nil
    else
      (rawDrafts(profile) ++ endpointPositiveDrafts(profile))
        .distinctBy(draft =>
          (
            draft.kind,
            draft.sourceSide,
            draft.attributionKind,
            draft.intent,
            draft.support.map(_.ref.id).distinct.sorted.mkString("|")
          )
        )

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

    val grouped = List(
      draft(
        RelativeCauseKind.DefensiveResource,
        RelativeCauseSignalProfile.defensiveResourceRecords(records)
      ),
      draft(
        RelativeCauseKind.RecaptureRecoveryWindow,
        RelativeCauseSignalProfile.recaptureResourceRecords(records, eventLine.rootMove)
      ),
      draft(
        RelativeCauseKind.KingForcing,
        RelativeCauseSignalProfile.forcingLineResourceRecords(
          records,
          eventLine
        )
      ),
      draft(
        RelativeCauseKind.ConversionSecured,
        (
          RelativeCauseSignalProfile.promotionRaceRecords(records) ++
            RelativeCauseSignalProfile.passedPawnResourceRecords(records)
        ).distinctBy(_.ref.id)
      ),
      draft(
        RelativeCauseKind.DrawResource,
        RelativeCauseSignalProfile.drawResourceRecords(records)
      ),
      draft(
        RelativeCauseKind.MaterialSwing,
        RelativeCauseSignalProfile.materialGainRecords(profile.graph, records)
      ),
      draft(
        RelativeCauseKind.SacrificeCompensation,
        RelativeCauseSignalProfile.sacrificeCompensationSupportRecords(profile.allRecords, eventLine)
      )
    ).flatten

    val tactical = RelativeCauseSignalProfile.tacticalMechanismRecords(profile.graph, records).flatMap {
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        endpointPositiveMechanismCauseKind(payload).flatMap(kind => draft(kind, List(record)))
      case _ =>
        None
    }
    val strategic = records.flatMap {
      case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _)
          if record.referencesLine(eventLine) && payload.canSupportStrategicCause =>
        payload.signals
          .filter(signal => signal.source.line.contains(eventLine))
          .flatMap(signal =>
            RelativeCauseSignalProfile
              .currentMoveStrategicSupportCauseKindsForSignal(signal, profile.allRecords)
              .filterNot(_ == RelativeCauseKind.PlanContradiction)
              .flatMap { kind =>
                if RelativeCauseKind.requiresExactPlanResult(kind) then
                  exactPlanEventSupportRecord(
                    kind,
                    signal,
                    eventLine,
                    profile.allRecords,
                    profile.graph,
                    profile.fact.comparison.mover,
                    sourceSide
                  ).flatMap(event => draft(kind, List(event))).toList
                else draft(kind, List(record)).toList
              }
          )
      case _ =>
        Nil
    }
    grouped ++ tactical ++ strategic

  private def rawDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    import profile.*
    val candidateRootIsRecapture =
      candidateRecords.exists {
        case EvidenceRecord(_, payload: LineFactEvidence, _) =>
          payload.rootIsRecapture(fact.candidateLine.rootMove)
        case _ =>
          false
      }
    val exactCurrentMoveRecaptureResource =
      exactReferenceMove &&
        candidateRootIsRecapture &&
        candidateRecaptureResource.exists {
          case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
            payload.kind == TacticalMechanismKind.RecaptureChoice &&
              payload.moveUci.exists(EvidenceRef.sameMove(_, fact.candidateLine.rootMove))
          case EvidenceRecord(_, payload: LineFactEvidence, _) =>
            payload.rootIsRecapture(fact.candidateLine.rootMove)
          case _ =>
            false
        }
    val candidateForcingLineSupport = candidateForcingLineResource
    List(
      causeDraft(
        RelativeCauseKind.DefensiveResource,
        referenceDefensiveResource,
        referenceDefensiveResource.nonEmpty && badLoss,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      ),
      causeDraft(
        RelativeCauseKind.DefensiveResource,
        candidateDefensiveResource,
        candidateDefensiveResource.nonEmpty && candidateProvedValue,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateCreatesValue
      ),
      causeDraft(
        RelativeCauseKind.DefensiveResource,
        sharedDefensiveResource,
        sharedDefensiveResource.nonEmpty && badLoss,
        RelativeCauseSourceSide.Shared,
        CauseAttributionKind.SharedContext
      ),
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
        RelativeCauseKind.RecaptureRecoveryWindow,
        candidateRecaptureResource,
        candidateRecaptureResource.nonEmpty && (candidateBetter || exactCurrentMoveRecaptureResource),
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateCreatesValue
      ),
      causeDraft(
        RelativeCauseKind.WrongMoveOrder,
        referenceMoveOrderResource,
        referenceMoveOrderResource.nonEmpty && candidateMoveOrderResource.isEmpty && badLoss,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      ),
      causeDraft(
        RelativeCauseKind.TempoLoss,
        candidateTempoLiability,
        candidateTempoLiability.nonEmpty && badLoss,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateAllowsLiability
      ),
      causeDraft(
        RelativeCauseKind.KingForcing,
        referenceForcingLineResource,
        referenceForcingLineResource.nonEmpty && badLoss,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      ),
      causeDraft(
        RelativeCauseKind.KingForcing,
        candidateForcingLineSupport,
        candidateForcingLineSupport.nonEmpty && candidateProvedValue,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateCreatesValue
      ),
      causeDraft(
        RelativeCauseKind.DrawResource,
        referenceDrawResource,
        referenceDrawResource.nonEmpty && badLoss,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      ),
      causeDraft(
        RelativeCauseKind.DrawResource,
        candidateDrawResource,
        candidateDrawResource.nonEmpty && candidateProvedValue,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateCreatesValue
      ),
      causeDraft(
        RelativeCauseKind.ConversionSecured,
        referencePromotionResource,
        referencePromotionResource.nonEmpty && badLoss,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      ),
      causeDraft(
        RelativeCauseKind.ConversionSecured,
        candidatePromotionResource,
        candidatePromotionResource.nonEmpty && candidateProvedValue,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateCreatesValue
      ),
      causeDraft(
        RelativeCauseKind.ConversionSecured,
        referencePassedPawnResource,
        referencePassedPawnResource.nonEmpty && badLoss,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      ),
      causeDraft(
        RelativeCauseKind.ConversionSecured,
        candidatePassedPawnResource,
        candidatePassedPawnResource.nonEmpty && candidateProvedValue,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateCreatesValue
      ),
      causeDraft(
        RelativeCauseKind.ConversionMiss,
        candidatePassedPawnConcession,
        candidatePassedPawnConcession.nonEmpty && badLoss,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateAllowsLiability
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
    ).flatten ++ strategicContrastDrafts(profile) ++ currentMoveStrategicSupportDrafts(profile) ++ mechanismDrafts(profile)

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

  private def mechanismDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    val actionableLoss = profile.fact.comparison.verdict.isActionableLoss
    val referenceCauses =
      Option
        .when(actionableLoss && profile.tacticalLoss)(
          mechanismCauseKinds(
            profile.referenceTacticalMechanism.filter {
              case EvidenceRecord(_, payload: TacticalMechanismEvidence, _)
                  if payload.kind == TacticalMechanismKind.Tempo =>
                false
              case _ =>
                true
            },
            badLoss = false,
            sourceSide = RelativeCauseSourceSide.Reference,
            attributionKind = CauseAttributionKind.ReferenceCreatesResource
          )
        )
        .getOrElse(Nil)
    val candidateBadCauses =
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
    val candidateValueMechanisms =
      if profile.candidateBetter then profile.candidateTacticalMechanism
      else if profile.exactReferenceMove then profile.candidateThreatBranchTacticalMechanism
      else Nil
    val candidateValueCauses =
      Option
        .when(candidateValueMechanisms.nonEmpty)(
          mechanismCauseKinds(
            candidateValueMechanisms,
            badLoss = false,
            candidateValue = true,
            sourceSide = RelativeCauseSourceSide.Candidate,
            attributionKind = CauseAttributionKind.CandidateCreatesValue
          )
        )
        .getOrElse(Nil)
    referenceCauses ++ candidateBadCauses ++ candidateValueCauses

  private def mechanismCauseKinds(
      records: List[EvidenceRecord],
      badLoss: Boolean,
      playedCandidate: Boolean = false,
      candidateValue: Boolean = false,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind
  ): List[RelativeCauseDraft] =
    records.flatMap {
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.canAnchorTacticalClaim =>
        val causeKind =
          if candidateValue then endpointPositiveMechanismCauseKind(payload)
          else Some(TacticalMechanismKind.relativeCauseKind(payload.kind, badLoss, playedCandidate))
        causeKind.map(kind =>
          RelativeCauseDraft.classified(kind, List(record), Some(sourceSide), attributionKind)
        )
      case _ =>
        None
    }

  private def endpointPositiveMechanismCauseKind(
      payload: TacticalMechanismEvidence
  ): Option[RelativeCauseKind] =
    payload.kind match
      case TacticalMechanismKind.MaterialGain =>
        Some(RelativeCauseKind.MaterialSwing)
      case TacticalMechanismKind.PawnPromotion =>
        Some(RelativeCauseKind.ConversionSecured)
      case TacticalMechanismKind.Tempo | TacticalMechanismKind.Refutation =>
        None
      case _ =>
        Some(TacticalMechanismKind.relativeCauseKind(payload.kind, badLoss = false, playedCandidate = false))

  private def candidateBadMechanismCanProveLiability(
      record: EvidenceRecord,
      profile: RelativeCauseSignalProfile
  ): Boolean =
    record.payload match
      case payload: TacticalMechanismEvidence =>
        payload.kind match
          case TacticalMechanismKind.RecaptureChoice =>
            false
          case TacticalMechanismKind.Tempo =>
            candidateNegativeLineConsequences(record, profile).exists(
              _.kind == LineConsequenceKind.ImmediateReplyCheck
            )
          case TacticalMechanismKind.KingForcing =>
            candidateNegativeLineConsequences(record, profile).exists(
              _.kind == LineConsequenceKind.Mate
            )
          case TacticalMechanismKind.Refutation =>
            false
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

  private def strategicContrastDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    profile.strategicContrasts.flatMap {
      case record @ EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        strategicContrastCauseKinds(payload, profile)
          .filterNot { case (kind, _, _) => RelativeCauseKind.requiresExactPlanResult(kind) }
          .map { case (kind, sourceSide, attributionKind) =>
            RelativeCauseDraft.classified(kind, List(record), Some(sourceSide), attributionKind)
          }
      case _ =>
        Nil
    }.distinctBy(draft => (draft.kind, draft.sourceSide, draft.support.map(_.ref.id).sorted.mkString("|")))

  private def currentMoveStrategicSupportDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    profile.candidateCurrentMoveStrategicSupport.flatMap {
        case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
          val causeKinds =
            currentMoveStrategicSupportCauseKinds(payload, profile.fact.candidateLine, profile.allRecords)
              .filter(kind =>
                val attributionKind =
                  if kind == RelativeCauseKind.PlanContradiction then CauseAttributionKind.CandidateAllowsLiability
                  else CauseAttributionKind.CandidateCreatesValue
                val endpointPositive =
                  RelativeCauseDraftIntent.classify(kind, attributionKind) ==
                    RelativeCauseDraftIntent.EndpointPositive
                if endpointPositive then
                  kind match
                    case RelativeCauseKind.PlanImprovement =>
                      currentMoveOwnedPlanEpisodeRecords(kind, payload, profile).nonEmpty
                    case _ =>
                      true
                else if kind == RelativeCauseKind.PlanContradiction then
                  currentMoveOwnedPlanEpisodeRecords(kind, payload, profile).nonEmpty
                else profile.exactReferenceMove
              )
          causeKinds.flatMap(kind =>
            val sourceSide = RelativeCauseSourceSide.Candidate
            val attributionKind =
              if kind == RelativeCauseKind.PlanContradiction then CauseAttributionKind.CandidateAllowsLiability
              else CauseAttributionKind.CandidateCreatesValue
            if RelativeCauseKind.requiresExactPlanResult(kind) then
              currentMoveOwnedPlanEpisodeRecords(kind, payload, profile).map(event =>
                RelativeCauseDraft.classified(kind, List(event), Some(sourceSide), attributionKind)
              )
            else
              List(RelativeCauseDraft.classified(kind, List(record), Some(sourceSide), attributionKind))
          )
        case _ =>
          Nil
      }.distinctBy(draft => (draft.kind, draft.support.map(_.ref.id).sorted.mkString("|")))

  private def currentMoveOwnedPlanEpisodeRecords(
      kind: RelativeCauseKind,
      payload: StrategicMechanismEvidence,
      profile: RelativeCauseSignalProfile
  ): List[EvidenceRecord] =
    Option
      .when(payload.kind == StrategicMechanismKind.PlanPressure)(payload.signals)
      .toList
      .flatten
      .flatMap(signal =>
        exactPlanEventSupportRecord(
          kind,
          signal,
          profile.fact.candidateLine,
          profile.allRecords,
          profile.graph,
          profile.fact.comparison.mover,
          RelativeCauseSourceSide.Candidate
        )
      )
      .distinctBy(_.ref.id)

  private def exactPlanEventSupportRecord(
      kind: RelativeCauseKind,
      signal: StrategicMechanismSignal,
      eventLine: LineNodeRef,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph,
      mover: chess.Color,
      sourceSide: RelativeCauseSourceSide
  ): Option[EvidenceRecord] =
    for
      axis <- signal.axis
      if RelativeCauseKind.requiresExactPlanResult(kind)
      if signal.kind == StrategicMechanismSignalKind.PlanPressure
      if signal.source.layer == EvidenceLayer.PlanCausalEvent
      if signal.source.line.contains(eventLine)
      if axis.kind == StrategicAxisKind.PlanCoherence
      if RelativeCauseKind.strategicAxisCanProveCause(kind, axis)
      record <- records.find(_.ref == signal.source)
      event <- record.payload match
        case payload: PlanCausalEventEvidence => Some(payload)
        case _                                => None
      if signal.label.trim.equalsIgnoreCase(event.planId.id)
      if axis.label.trim.equalsIgnoreCase(event.planId.id)
      if graph.proofEligible(record)
      if RootOwnedEffectPolicy.planEventOwnsRoot(record.ref, event, eventLine, mover)
      if RelativeCauseKind.planCausalEventCanProveCause(kind, event)
    yield record


  private def currentMoveStrategicSupportCauseKinds(
      payload: StrategicMechanismEvidence,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[RelativeCauseKind] =
    RelativeCauseSignalProfile.currentMoveStrategicSupportSignals(payload, candidateLine, records)
      .flatMap(signal => RelativeCauseSignalProfile.currentMoveStrategicSupportCauseKindsForSignal(signal, records))
      .distinct

  private def strategicContrastCauseKinds(
      payload: StrategicMechanismContrastEvidence,
      profile: RelativeCauseSignalProfile
  ): List[(RelativeCauseKind, RelativeCauseSourceSide, CauseAttributionKind)] =
    val structuralLoss = profile.badLoss || profile.actionablePlayedLoss
    def endpointPositive(kind: RelativeCauseKind, attributionKind: CauseAttributionKind): Boolean =
      RelativeCauseDraftIntent.classify(kind, attributionKind) ==
        RelativeCauseDraftIntent.EndpointPositive
    val axisCauses =
      payload.actionableComparisons.flatMap { axisComparison =>
        val positiveKind = strategicPositiveCause(axisComparison.axis)
        if axisComparison.referenceLead then
          positiveKind
            .filter(kind => structuralLoss || endpointPositive(kind, CauseAttributionKind.ReferenceCreatesResource))
            .map(kind => (kind, RelativeCauseSourceSide.Reference, CauseAttributionKind.ReferenceCreatesResource))
            .toList
        else if axisComparison.candidateNegative && structuralLoss then
          strategicNegativeCause(axisComparison.axis)
            .map(kind => (kind, RelativeCauseSourceSide.Candidate, CauseAttributionKind.CandidateAllowsLiability))
            .toList
        else if axisComparison.candidateLead then
          positiveKind
            .filter(kind =>
              profile.primaryPlayedPositive ||
                profile.candidateBetter ||
                endpointPositive(kind, CauseAttributionKind.CandidateCreatesValue)
            )
            .map(kind => (kind, RelativeCauseSourceSide.Candidate, CauseAttributionKind.CandidateCreatesValue))
            .toList
        else Nil
      }
    axisCauses.distinct

  private def strategicPositiveCause(axis: StrategicAxisDetail): Option[RelativeCauseKind] =
    axis.kind match
      case StrategicAxisKind.PlanCoherence if axis.polarity == StrategicAxisPolarity.Gain =>
        Some(RelativeCauseKind.PlanImprovement)
      case _ =>
        None

  private def strategicNegativeCause(axis: StrategicAxisDetail): Option[RelativeCauseKind] =
    axis.kind match
      case StrategicAxisKind.PlanCoherence if axis.polarity == StrategicAxisPolarity.Concede =>
        Some(RelativeCauseKind.PlanContradiction)
      case _ =>
        None

private[chessjudgment] object RelativeCauseSignalProfile:
  def from(
      fact: CandidateComparisonFact,
      graph: TypedEvidenceGraph,
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord],
      referenceEndpointMoveOrderRecords: List[EvidenceRecord] = Nil,
      candidateEndpointMoveOrderRecords: List[EvidenceRecord] = Nil
  ): RelativeCauseSignalProfile =
    RelativeCauseSignalProfile(
      fact = fact,
      graph = graph,
      referenceRecords = referenceRecords,
      candidateRecords = candidateRecords,
      sharedRecords = sharedRecords,
      referenceEndpointMoveOrderRecords = referenceEndpointMoveOrderRecords,
      candidateEndpointMoveOrderRecords = candidateEndpointMoveOrderRecords
    )


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

  private[chessjudgment] def threatBranchTacticalMechanismRecords(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    tacticalMechanismRecords(graph, records).filter(_.ref.line.exists(_.role == LineNodeRole.Threat))

  private[chessjudgment] def strategicContrastRecords(
      fact: CandidateComparisonFact,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _)
          if payload.hasActionableContrast &&
            payload.comparisonKind == fact.kind &&
            payload.referenceLine == fact.referenceLine &&
            payload.candidateLine == fact.candidateLine =>
        record
    }.distinctBy(_.ref.id)

  private[chessjudgment] def defensiveResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorDefensiveClaim
      case _ =>
        false
    }.distinctBy(_.ref.id)

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

  private[chessjudgment] def tempoLiabilityRecords(
      fact: CandidateComparisonFact,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    def ownsImmediateReplyLiability(sourceLabels: Map[String, Set[String]]): Boolean =
      sourceLabels.nonEmpty &&
        TypedEvidenceGraph
          .ownedLineConsequences(
            records,
            fact,
            RelativeCauseSourceSide.Candidate,
            CauseAttributionKind.CandidateAllowsLiability,
            sourceLabels
          )
          .exists(_._2.kind == LineConsequenceKind.ImmediateReplyCheck)
    val ownedLineIds =
      TypedEvidenceGraph
        .ownedLineConsequences(
          records,
          fact,
          RelativeCauseSourceSide.Candidate,
          CauseAttributionKind.CandidateAllowsLiability
        )
        .collect {
          case (ref, consequence) if consequence.kind == LineConsequenceKind.ImmediateReplyCheck => ref.id
        }
        .toSet
    records.filter {
      case EvidenceRecord(ref, _: LineFactEvidence, _) =>
        ownedLineIds.contains(ref.id)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.Tempo &&
          payload.canAnchorTacticalClaim &&
          ownsImmediateReplyLiability(payload.lineConsequenceSourceLabelsByEvidenceId)
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
        payload.hasProofSignalConsequence(LineConsequenceKind.DrawResource)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.DrawResource && payload.canAnchorDefensiveClaim
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def promotionRaceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload
          .rootOwnedCausalEpisodes(payload.line.rootMove)
          .exists(episode =>
            episode.consequence.kind == LineConsequenceKind.Promotion ||
              episode.consequence.kind == LineConsequenceKind.PromotionRace
          )
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
          case (ref, consequence)
              if consequence.kind == LineConsequenceKind.Promotion ||
                consequence.kind == LineConsequenceKind.PromotionRace =>
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

  private[chessjudgment] def passedPawnResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
        payload.hasConsequence(TransitionConsequenceKind.PassedPawnProgress)
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def passedPawnConcessionRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
        payload.hasConsequence(TransitionConsequenceKind.PassedPawnConcession)
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def currentMoveStrategicSupportRecords(
      fact: CandidateComparisonFact,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _)
          if graph.proofEligible(record) &&
            record.referencesLine(fact.candidateLine) &&
            payload.canSupportStrategicCause &&
            currentMoveStrategicSupportSignals(payload, fact.candidateLine, records).nonEmpty =>
        record
    }.distinctBy(_.ref.id)

  private[chessjudgment] def currentMoveStrategicSupportSignals(
      payload: StrategicMechanismEvidence,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[StrategicMechanismSignal] =
    payload.signals.filter(signal => currentMoveStrategicSupportSignal(signal, candidateLine, records))

  private[chessjudgment] def currentMoveStrategicSupportSignal(
      signal: StrategicMechanismSignal,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    signal.kind == StrategicMechanismSignalKind.PlanPressure &&
      signal.source.layer == EvidenceLayer.PlanCausalEvent &&
      signal.source.scope == EvidenceScope.PlayedTransition &&
      signal.source.line.contains(candidateLine) &&
      currentMoveStrategicSupportCauseKindsForSignal(signal, records).nonEmpty

  private[chessjudgment] def currentMoveStrategicSupportCauseKindsForSignal(
      signal: StrategicMechanismSignal,
      records: List[EvidenceRecord]
  ): List[RelativeCauseKind] =
    signal.axis.toList.flatMap { axis =>
      axis.kind match
        case StrategicAxisKind.PlanCoherence if axis.polarity == StrategicAxisPolarity.Concede =>
          List(RelativeCauseKind.PlanContradiction)
        case StrategicAxisKind.PlanCoherence if StrategicMechanismContrastEvidence.currentMovePlanCoherenceAxis(axis) =>
          List(RelativeCauseKind.PlanImprovement)
        case _ =>
          Nil
    }

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

  private[chessjudgment] def sacrificeCompensationSupportRecords(
      records: List[EvidenceRecord],
      eventLine: LineNodeRef
  ): List[EvidenceRecord] =
    val material = records.filter {
      case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
        ref.line.contains(eventLine) &&
          payload.line == eventLine &&
          payload
            .rootOwnedCausalEpisodes(eventLine.rootMove)
            .exists(_.consequence.kind == LineConsequenceKind.Sacrifice)
      case _ =>
        false
    }
    val compensation = records.filter {
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        sacrificeCompensationReturnComparisons(payload, eventLine, records).nonEmpty
      case _ =>
        false
    }
    Option
      .when(material.nonEmpty && compensation.nonEmpty)((material ++ compensation).distinctBy(_.ref.id))
      .getOrElse(Nil)

  private[chessjudgment] def sacrificeCompensationReturnComparisons(
      payload: StrategicMechanismContrastEvidence,
      eventLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[StrategicAxisComparison] =
    val recordsById = records.map(record => record.ref.id -> record).toMap
    def exactPlanReturn(
        source: EvidenceRef,
        axis: StrategicAxisDetail,
        visited: Set[String]
    ): Boolean =
      !visited(source.id) && recordsById.get(source.id).exists {
        case EvidenceRecord(ref, event: PlanCausalEventEvidence, _) =>
          ref.line.contains(eventLine) &&
            event.rootLine == eventLine &&
            EvidenceRef.sameMove(event.rootMove, eventLine.rootMove) &&
            RelativeCauseKind.planCausalEventCanProveCause(
              RelativeCauseKind.SacrificeCompensation,
              event
            )
        case EvidenceRecord(ref, mechanism: StrategicMechanismEvidence, _) if ref.line.contains(eventLine) =>
          mechanism.signals.exists(signal =>
            signal.axis.exists(_.stableKey == axis.stableKey) &&
              exactPlanReturn(signal.source, axis, visited + source.id)
          )
        case _ =>
          false
      }
    val sourceSide =
      if eventLine == payload.referenceLine then Some(RelativeCauseSourceSide.Reference)
      else if eventLine == payload.candidateLine then Some(RelativeCauseSourceSide.Candidate)
      else None
    sourceSide.toList.flatMap(side =>
      payload.sustainedCauseComparisons(RelativeCauseKind.SacrificeCompensation, side).filter { comparison =>
        comparison.sourcesFor(side).exists(exactPlanReturn(_, comparison.axis, Set.empty))
      }
    )
