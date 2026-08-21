package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.evaluation.JudgmentThresholds
import lila.chessjudgment.model.judgment.ThreatDriver
import lila.chessjudgment.analysis.tactical.TacticalMotifClassifier
import lila.chessjudgment.model.Motif
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
    RelativeCauseKind.OnlyDefenseNecessity,
    RelativeCauseKind.RecaptureRecoveryWindow,
    RelativeCauseKind.ConversionSecured,
    RelativeCauseKind.SacrificeCompensation,
    RelativeCauseKind.StructuralImprovement,
    RelativeCauseKind.TargetPressureGain,
    RelativeCauseKind.CenterControlGain,
    RelativeCauseKind.PawnWeaknessTarget,
    RelativeCauseKind.PawnBreakOpportunity,
    RelativeCauseKind.ActivityGain,
    RelativeCauseKind.OpponentRestriction,
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

  val referenceOnlyDefense: List[EvidenceRecord] =
    RelativeCauseSignalProfile.onlyDefenseRecords(referenceRecords)
  val candidateOnlyDefense: List[EvidenceRecord] =
    RelativeCauseSignalProfile.onlyDefenseRecords(candidateRecords)
  val referenceOnlyDefenseFunction: List[EvidenceRecord] =
    RelativeCauseSignalProfile.referenceOnlyDefenseFunctionRecords(fact, sharedRecords)
  val referenceConversionWindow: List[EvidenceRecord] =
    RelativeCauseSignalProfile.conversionWindowRecords(
      referenceRecords,
      fact.referenceLine,
      fact.comparison.mover
    )
  val candidateConversionWindow: List[EvidenceRecord] =
    RelativeCauseSignalProfile.conversionWindowRecords(
      candidateRecords,
      fact.candidateLine,
      fact.comparison.mover
    )
  val candidateConversionMissWindow: List[EvidenceRecord] =
    RelativeCauseSignalProfile.conversionMissRecords(candidateRecords)
  val referenceDrawResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.drawResourceRecords(referenceRecords)
  val candidateDrawResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.drawResourceRecords(candidateRecords)
  val referenceRecaptureResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.recaptureResourceRecords(referenceRecords, fact.referenceLine.rootMove)
  val referenceRootOwnedTacticalResource: List[EvidenceRecord] =
    RootOwnedCausePolicy.forcingResourceRecords(referenceRecords, fact.referenceLine)
  val candidateRecaptureResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.recaptureResourceRecords(candidateRecords, fact.candidateLine.rootMove)
  val referenceMoveOrderResource: List[EvidenceRecord] =
    (
      RelativeCauseSignalProfile.moveOrderResourceRecords(referenceRecords) ++
        referenceEndpointMoveOrderRecords
    ).distinctBy(_.ref.id)
  val candidateMoveOrderResource: List[EvidenceRecord] =
    (
      RelativeCauseSignalProfile.moveOrderResourceRecords(candidateRecords) ++
        candidateEndpointMoveOrderRecords
    ).distinctBy(_.ref.id)
  val candidateTempoLiability: List[EvidenceRecord] =
    RelativeCauseSignalProfile.tempoLiabilityRecords(fact, candidateRecords)
  val referenceForcingLineResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.forcingLineResourceRecords(
      referenceRecords,
      fact.referenceLine,
      fact.comparison.mover
    )
  val candidateForcingLineResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.forcingLineResourceRecords(
      candidateRecords,
      fact.candidateLine,
      fact.comparison.mover
    )
  val candidateWrongRecapturerChoice: List[EvidenceRecord] =
    RelativeCauseSignalProfile.wrongRecapturerChoiceRecords(fact, referenceRecords, candidateRecords)
  val referenceDefensiveResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.defensiveResourceRecords(referenceRecords)
  val candidateDefensiveResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.defensiveResourceRecords(candidateRecords)
  val sharedDefensiveResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.defensiveResourceRecords(sharedRecords)
  val candidateLooseMaterialLiability: List[EvidenceRecord] =
    RelativeCauseSignalProfile.looseMaterialLiabilityRecords(fact.comparison.mover, candidateRecords, sharedRecords)
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
    RelativeCauseSignalProfile.tacticalMechanismRecords(referenceRecords)
  val candidateTacticalMechanism: List[EvidenceRecord] =
    RelativeCauseSignalProfile.tacticalMechanismRecords(candidateRecords)
  val candidateThreatBranchTacticalMechanism: List[EvidenceRecord] =
    RelativeCauseSignalProfile.threatBranchTacticalMechanismRecords(candidateRecords)
  val candidateRelationPayoffMaterial: List[EvidenceRecord] =
    RelativeCauseSignalProfile.relationPayoffMaterialRecords(candidateRecords ++ sharedRecords)
  val candidateMaterialLossLiability: List[EvidenceRecord] =
    RelativeCauseSignalProfile.candidateMaterialLossLiabilityRecords(fact, candidateRecords)
  val sacrificeCompensationSupport: List[EvidenceRecord] =
    RelativeCauseSignalProfile.sacrificeCompensationSupportRecords(
      candidateRecords,
      fact.candidateLine.rootMove
    )
  val candidateMoveHasSacrifice: Boolean =
    candidateRecords.exists {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.consequencesForRootMove(fact.candidateLine.rootMove).exists(
          _.kind == LineConsequenceKind.Sacrifice
        )
      case _ => false
    }
  val strategicContrasts: List[EvidenceRecord] =
    RelativeCauseSignalProfile.strategicContrastRecords(fact, allRecords)
  val candidateCurrentMoveStrategicSupport: List[EvidenceRecord] =
    RelativeCauseSignalProfile.currentMoveStrategicSupportRecords(
      fact,
      if exactReferenceMove then candidateRecords ++ sharedRecords else candidateRecords
    )

private[chessjudgment] object RelativeCauseDraftPlanner:
  def drafts(
      profile: RelativeCauseSignalProfile,
      comparisonRecord: EvidenceRecord
  ): List[RelativeCauseDraft] =
    if !profile.fact.hasDistinctRootMoves then Nil
    else
      (rawDrafts(profile) ++ endpointPositiveDrafts(profile) ++ comparisonOwnedDrafts(profile, comparisonRecord))
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

  private def comparisonOwnedDrafts(
      profile: RelativeCauseSignalProfile,
      comparisonRecord: EvidenceRecord
  ): List[RelativeCauseDraft] =
    comparisonRecord.payload match
      case CandidateComparisonEvidence(registered)
          if registered == profile.fact &&
            profile.actionablePlayedLoss &&
            registered.defensiveRecaptureResource.nonEmpty =>
        List(
          RelativeCauseDraft.classified(
            kind = RelativeCauseKind.DefensiveResource,
            support = List(comparisonRecord),
            sourceSide = Some(RelativeCauseSourceSide.Reference),
            attributionKind = CauseAttributionKind.ReferenceCreatesResource
          )
        )
      case _ =>
        Nil

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
        RelativeCauseKind.OnlyDefenseNecessity,
        RelativeCauseSignalProfile.onlyDefenseRecords(records)
      ),
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
          eventLine,
          profile.fact.comparison.mover
        )
      ),
      draft(
        RelativeCauseKind.ConversionSecured,
        (
          RelativeCauseSignalProfile.conversionWindowRecords(
            records,
            eventLine,
            profile.fact.comparison.mover
          ) ++
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
        RelativeCauseSignalProfile.materialGainRecords(records)
      ),
      draft(
        RelativeCauseKind.SacrificeCompensation,
        RelativeCauseSignalProfile.sacrificeCompensationSupportRecords(records, eventLine.rootMove)
      )
    ).flatten

    val tactical = RelativeCauseSignalProfile.tacticalMechanismRecords(records).flatMap {
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
    val exactCurrentMoveForcingResource =
      exactReferenceMove &&
        candidateForcingLineResource.exists(record =>
          RelativeCauseSignalProfile.currentMoveMateThreatRecord(
            record,
            fact.candidateLine.rootMove,
            fact.comparison.mover
          )
        )
    val exactCurrentMoveOnlyDefense =
      if exactReferenceMove then
        (referenceOnlyDefense ++ referenceOnlyDefenseFunction).filter {
          case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
            payload.onlyDefense.exists(move =>
              EvidenceRef.normalizeMove(move) == EvidenceRef.normalizeMove(fact.candidateLine.rootMove)
            )
          case _ =>
            false
        }
      else Nil
    val candidateOnlyDefenseSupport =
      (candidateOnlyDefense ++ exactCurrentMoveOnlyDefense).distinctBy(_.ref.id)
    val candidateForcingLineSupport =
      if exactCurrentMoveForcingResource && !candidateBetter then
        candidateForcingLineResource.filter(record =>
          RelativeCauseSignalProfile.currentMoveMateThreatRecord(
            record,
            fact.candidateLine.rootMove,
            fact.comparison.mover
          )
        )
      else candidateForcingLineResource
    List(
      causeDraft(
        RelativeCauseKind.OnlyDefenseNecessity,
        referenceOnlyDefense,
        referenceOnlyDefense.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference)
      ),
      causeDraft(
        RelativeCauseKind.OnlyDefenseNecessity,
        candidateOnlyDefenseSupport,
        candidateOnlyDefenseSupport.nonEmpty && candidateProvedValue,
        Some(RelativeCauseSourceSide.Candidate),
        Some(CauseAttributionKind.CandidateCreatesValue)
      ),
      causeDraft(
        RelativeCauseKind.OnlyDefenseNecessity,
        referenceOnlyDefenseFunction,
        referenceOnlyDefenseFunction.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference)
      ),
      causeDraft(
        RelativeCauseKind.DefensiveResource,
        referenceDefensiveResource,
        referenceDefensiveResource.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference)
      ),
      causeDraft(
        RelativeCauseKind.DefensiveResource,
        candidateDefensiveResource,
        candidateDefensiveResource.nonEmpty && candidateProvedValue,
        Some(RelativeCauseSourceSide.Candidate)
      ),
      causeDraft(
        RelativeCauseKind.DefensiveResource,
        sharedDefensiveResource,
        sharedDefensiveResource.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Shared)
      ),
      causeDraft(
        RelativeCauseKind.MissedTacticalResource,
        referenceRecaptureResource,
        referenceRecaptureResource.nonEmpty && candidateRecaptureResource.isEmpty && badLoss
      ),
      causeDraft(
        RelativeCauseKind.MissedTacticalResource,
        referenceRootOwnedTacticalResource,
        referenceRootOwnedTacticalResource.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference),
        Some(CauseAttributionKind.ReferenceCreatesResource)
      ),
      causeDraft(
        RelativeCauseKind.WrongRecapturer,
        candidateWrongRecapturerChoice,
        candidateWrongRecapturerChoice.nonEmpty && badLoss
      ),
      causeDraft(
        RelativeCauseKind.RecaptureRecoveryWindow,
        candidateRecaptureResource,
        candidateRecaptureResource.nonEmpty && (candidateBetter || exactCurrentMoveRecaptureResource)
      ),
      causeDraft(
        RelativeCauseKind.WrongMoveOrder,
        referenceMoveOrderResource,
        referenceMoveOrderResource.nonEmpty && candidateMoveOrderResource.isEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference),
        Some(CauseAttributionKind.ReferenceCreatesResource)
      ),
      causeDraft(
        RelativeCauseKind.TempoLoss,
        candidateTempoLiability,
        candidateTempoLiability.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Candidate),
        Some(CauseAttributionKind.CandidateAllowsLiability)
      ),
      causeDraft(
        RelativeCauseKind.KingForcing,
        referenceForcingLineResource,
        referenceForcingLineResource.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference)
      ),
      causeDraft(
        RelativeCauseKind.KingForcing,
        candidateForcingLineSupport,
        candidateForcingLineSupport.nonEmpty && (candidateBetter || exactCurrentMoveForcingResource),
        Some(RelativeCauseSourceSide.Candidate),
        Some(CauseAttributionKind.CandidateCreatesValue)
      ),
      causeDraft(
        RelativeCauseKind.ConversionMiss,
        candidateConversionMissWindow,
        candidateConversionMissWindow.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Candidate)
      ),
      causeDraft(
        RelativeCauseKind.ConversionSecured,
        referenceConversionWindow,
        referenceConversionWindow.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference),
        Some(CauseAttributionKind.ReferenceCreatesResource)
      ),
      causeDraft(RelativeCauseKind.ConversionSecured, candidateConversionWindow, candidateConversionWindow.nonEmpty && candidateProvedValue),
      causeDraft(
        RelativeCauseKind.DrawResource,
        referenceDrawResource,
        referenceDrawResource.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference)
      ),
      causeDraft(
        RelativeCauseKind.DrawResource,
        candidateDrawResource,
        candidateDrawResource.nonEmpty && candidateProvedValue,
        Some(RelativeCauseSourceSide.Candidate)
      ),
      causeDraft(
        RelativeCauseKind.ConversionSecured,
        referencePromotionResource,
        referencePromotionResource.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference),
        Some(CauseAttributionKind.ReferenceCreatesResource)
      ),
      causeDraft(RelativeCauseKind.ConversionSecured, candidatePromotionResource, candidatePromotionResource.nonEmpty && candidateProvedValue),
      causeDraft(
        RelativeCauseKind.ConversionSecured,
        referencePassedPawnResource,
        referencePassedPawnResource.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference)
      ),
      causeDraft(
        RelativeCauseKind.ConversionSecured,
        candidatePassedPawnResource,
        candidatePassedPawnResource.nonEmpty && candidateProvedValue,
        Some(RelativeCauseSourceSide.Candidate)
      ),
      causeDraft(
        RelativeCauseKind.ConversionMiss,
        candidatePassedPawnConcession,
        candidatePassedPawnConcession.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Candidate),
        Some(CauseAttributionKind.CandidateAllowsLiability)
      ),
      causeDraft(
        if playedCandidateSideComparison then RelativeCauseKind.TacticalRefutationOfPlayed
        else RelativeCauseKind.CandidateTacticalLiability,
        candidateOpponentPromotionLiability,
        candidateOpponentPromotionLiability.nonEmpty && referenceOpponentPromotionLiability.isEmpty && badLoss,
        Some(RelativeCauseSourceSide.Candidate),
        Some(CauseAttributionKind.CandidateAllowsLiability)
      ),
      causeDraft(
        RelativeCauseKind.MaterialSwing,
        candidateRelationPayoffMaterial,
        candidateRelationPayoffMaterial.nonEmpty && exactReferenceMove,
        Some(RelativeCauseSourceSide.Candidate),
        Some(CauseAttributionKind.CandidateCreatesValue)
      ),
      causeDraft(
        if playedCandidateSideComparison then RelativeCauseKind.TacticalRefutationOfPlayed
        else RelativeCauseKind.CandidateTacticalLiability,
        candidateLooseMaterialLiability,
        candidateLooseMaterialLiability.nonEmpty && badLoss
      ),
      causeDraft(
        RelativeCauseKind.MaterialSwing,
        candidateMaterialLossLiability,
        majorLoss &&
          candidateMaterialLossLiability.nonEmpty,
        Some(RelativeCauseSourceSide.Candidate),
        Some(CauseAttributionKind.CandidateAllowsLiability)
      ),
      causeDraft(
        RelativeCauseKind.SacrificeCompensation,
        sacrificeCompensationSupport,
        sacrificeCompensationSupport.nonEmpty &&
          candidateMoveHasSacrifice &&
          (
            primaryPlayedPositive ||
              candidateBetter ||
              (playedCandidateSideComparison && !badLoss)
          ),
        Some(RelativeCauseSourceSide.Candidate),
        Some(CauseAttributionKind.CandidateCreatesValue)
      )
    ).flatten ++ strategicContrastDrafts(profile) ++ currentMoveStrategicSupportDrafts(profile) ++ mechanismDrafts(profile)

  private def causeDraft(
      kind: RelativeCauseKind,
      support: List[EvidenceRecord],
      condition: Boolean,
      sourceSide: Option[RelativeCauseSourceSide] = None,
      attributionKind: Option[CauseAttributionKind] = None
  ): Option[RelativeCauseDraft] =
    val effectiveAttribution =
      attributionKind.getOrElse(RelativeCauseKind.defaultAttributionKind(kind, sourceSide))
    Option.when(condition)(
      RelativeCauseDraft.classified(kind, support, sourceSide, effectiveAttribution)
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
                payload.hasMoverZwischenzug
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
      case TacticalMechanismKind.Tempo | TacticalMechanismKind.RelationMechanism |
          TacticalMechanismKind.Refutation =>
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
          case TacticalMechanismKind.Refutation | TacticalMechanismKind.RelationMechanism =>
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
          .filterNot { case (kind, _) => RelativeCauseKind.requiresExactPlanResult(kind) }
          .map { case (kind, sourceSide) =>
          val attributionKind = RelativeCauseKind.defaultAttributionKind(kind, Some(sourceSide))
          RelativeCauseDraft.classified(kind, List(record), Some(sourceSide), attributionKind)
          }
      case _ =>
        Nil
    }.distinctBy(draft => (draft.kind, draft.sourceSide, draft.support.map(_.ref.id).sorted.mkString("|")))

  private def currentMoveStrategicSupportDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    val candidateCurrentMoveCanOwnValue =
      profile.playedCandidateSideComparison &&
        profile.candidateBetter
    profile.candidateCurrentMoveStrategicSupport.flatMap {
        case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
          val relationPayoffs =
            RelativeCauseSignalProfile.currentMoveRelationPayoffRecords(profile.fact.candidateLine, profile.allRecords)
          val targetPressureRelationProofs =
            RelativeCauseSignalProfile.currentMoveTargetPressureRelationRecords(profile.fact.candidateLine, profile.allRecords)
          val concreteTargetCarriers =
            StrategicMechanismContrastEvidence.currentMoveConcreteTargetCarrierRecords(
              profile.fact.candidateLine,
              profile.allRecords
            )
          val causeKinds =
            currentMoveStrategicSupportCauseKinds(payload, profile.fact.candidateLine, profile.allRecords)
              .filter(kind =>
                val sourceSide = RelativeCauseSourceSide.Candidate
                val attributionKind = RelativeCauseKind.defaultAttributionKind(kind, Some(sourceSide))
                val endpointPositive =
                  RelativeCauseDraftIntent.classify(kind, attributionKind) ==
                    RelativeCauseDraftIntent.EndpointPositive
                if endpointPositive then
                  kind match
                    case RelativeCauseKind.OpponentRestriction =>
                      currentMoveConcreteCounterplayCanOwnValue(kind, payload, profile)
                    case RelativeCauseKind.TargetPressureGain =>
                      concreteTargetCarriers.nonEmpty || targetPressureRelationProofs.nonEmpty
                    case RelativeCauseKind.PawnWeaknessTarget =>
                      concreteTargetCarriers.nonEmpty || relationPayoffs.nonEmpty
                    case RelativeCauseKind.PlanImprovement =>
                      currentMoveOwnedPlanEpisodeRecords(kind, payload, profile).nonEmpty
                    case RelativeCauseKind.ActivityGain =>
                      currentMoveConcreteActivityCanOwnValue(kind, payload, profile)
                    case _ =>
                      true
                else if kind == RelativeCauseKind.PlanContradiction then
                  currentMoveOwnedPlanEpisodeRecords(kind, payload, profile).nonEmpty
                else
                  (
                    profile.exactReferenceMove &&
                      (
                        kind != RelativeCauseKind.ActivityGain ||
                          currentMoveConcreteActivityCanOwnValue(kind, payload, profile)
                      )
                  ) ||
                    (
                      candidateCurrentMoveCanOwnValue &&
                        (
                          kind == RelativeCauseKind.PawnBreakOpportunity ||
                            currentMoveConcreteActivityCanOwnValue(kind, payload, profile)
                        )
                    )
              )
          causeKinds.flatMap(kind =>
            val sourceSide = RelativeCauseSourceSide.Candidate
            val attributionKind = RelativeCauseKind.defaultAttributionKind(kind, Some(sourceSide))
            if RelativeCauseKind.requiresExactPlanResult(kind) then
              currentMoveOwnedPlanEpisodeRecords(kind, payload, profile).map(event =>
                RelativeCauseDraft.classified(kind, List(event), Some(sourceSide), attributionKind)
              )
            else
              val support =
                if kind == RelativeCauseKind.TargetPressureGain then record :: targetPressureRelationProofs
                else if kind == RelativeCauseKind.PawnWeaknessTarget then record :: relationPayoffs
                else List(record)
              List(RelativeCauseDraft.classified(kind, support, Some(sourceSide), attributionKind))
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
      if RelativeCauseKind.strategicAxisCanProveCause(kind, axis, sourceSide)
      record <- records.find(_.ref == signal.source)
      event <- record.payload match
        case payload: PlanCausalEventEvidence => Some(payload)
        case _                                => None
      if signal.label.trim.equalsIgnoreCase(event.planId.id)
      if axis.label.trim.equalsIgnoreCase(event.planId.id)
      if record.ref.confidence != EvidenceConfidence.Heuristic
      if RootOwnedEffectPolicy.planEventOwnsRoot(record.ref, event, eventLine, mover)
      if RelativeCauseKind.planCausalEventCanProveCause(kind, event)
    yield record

  private def currentMoveConcreteActivityCanOwnValue(
      kind: RelativeCauseKind,
      payload: StrategicMechanismEvidence,
      profile: RelativeCauseSignalProfile
  ): Boolean =
    kind == RelativeCauseKind.ActivityGain &&
      currentMoveConcreteActivitySignals(payload, profile.fact.candidateLine, profile.allRecords)
        .exists(signal =>
          StrategicMechanismContrastEvidence
            .currentMoveConcreteActivitySource(signal.source, profile.allRecords)
        )

  private def currentMoveConcreteActivitySignals(
      payload: StrategicMechanismEvidence,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[StrategicMechanismSignal] =
    payload.signals.filter(signal =>
      RelativeCauseSignalProfile.currentMoveStrategicSupportSignal(signal, candidateLine, records) &&
        signal.axis.exists(axis =>
          StrategicMechanismContrastEvidence.currentMoveActivityValueAxis(axis)
        )
    )

  private def currentMoveConcreteCounterplayCanOwnValue(
      kind: RelativeCauseKind,
      payload: StrategicMechanismEvidence,
      profile: RelativeCauseSignalProfile
  ): Boolean =
    kind == RelativeCauseKind.OpponentRestriction &&
      currentMoveConcreteCounterplaySignals(payload, profile.fact.candidateLine, profile.allRecords)
        .exists(signal => currentMoveConcreteCounterplaySource(signal, profile.fact.candidateLine, profile.allRecords))

  private def currentMoveConcreteCounterplaySignals(
      payload: StrategicMechanismEvidence,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[StrategicMechanismSignal] =
    RelativeCauseSignalProfile.currentMoveStrategicSupportSignals(payload, candidateLine, records).filter(signal =>
        signal.axis.exists(axis =>
            axis.kind == StrategicAxisKind.Counterplay &&
            axis.polarity == StrategicAxisPolarity.Restrain
        )
    )

  private def currentMoveConcreteCounterplaySource(
      signal: StrategicMechanismSignal,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    records.exists {
      case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) if ref.id == signal.source.id =>
        val diagonalRestriction =
          payload.consequencesOf(TransitionConsequenceKind.OpponentMobilityRestriction).exists(consequence =>
            consequence.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
        )
        diagonalRestriction || RelativeCauseSignalProfile.currentMoveCounterBreakSupportSignal(signal, candidateLine, records)
      case EvidenceRecord(ref, payload: PlanCausalEventEvidence, _)
          if ref.id == signal.source.id &&
            ref.line.contains(candidateLine) &&
            payload.opponentResourceDeterrence.nonEmpty =>
        payload.structuralConsequences.exists(consequence =>
          consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
            consequence.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
        )
      case _ =>
        RelativeCauseSignalProfile.currentMoveCounterBreakSupportSignal(signal, candidateLine, records)
    }

  private def currentMoveStrategicSupportCauseKinds(
      payload: StrategicMechanismEvidence,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[RelativeCauseKind] =
    RelativeCauseSignalProfile.currentMoveStrategicSupportSignals(payload, candidateLine, records)
      .flatMap(signal =>
        RelativeCauseSignalProfile.currentMoveStrategicSupportCauseKindsForSignal(signal, records) ++
          Option.when(RelativeCauseSignalProfile.currentMoveCounterBreakSupportSignal(signal, candidateLine, records))(
            RelativeCauseKind.OpponentRestriction
          )
      )
      .distinct

  private def strategicContrastCauseKinds(
      payload: StrategicMechanismContrastEvidence,
      profile: RelativeCauseSignalProfile
  ): List[(RelativeCauseKind, RelativeCauseSourceSide)] =
    val structuralLoss = profile.badLoss || profile.actionablePlayedLoss
    def endpointPositive(kind: RelativeCauseKind, sourceSide: RelativeCauseSourceSide): Boolean =
      val attributionKind = RelativeCauseKind.defaultAttributionKind(kind, Some(sourceSide))
      RelativeCauseDraftIntent.classify(kind, attributionKind) ==
        RelativeCauseDraftIntent.EndpointPositive
    val axisCauses =
      payload.actionableComparisons.flatMap(axisComparison =>
        val axis = axisComparison.axis
        val referenceKind = strategicReferenceLeadCause(axisComparison, profile.allRecords)
        val candidateKind = strategicCandidatePositiveCause(axisComparison, profile.allRecords)
        if axisComparison.referenceLead &&
            referenceLeadCanExplainStructuralLoss(axis) &&
            (structuralLoss || endpointPositive(referenceKind, RelativeCauseSourceSide.Reference))
        then List(referenceKind -> RelativeCauseSourceSide.Reference)
        else if axisComparison.candidateNegative && structuralLoss then
          List(strategicCandidateNegativeCause(axisComparison, profile.allRecords) -> RelativeCauseSourceSide.Candidate)
        else if axisComparison.candidateLead &&
            candidateLeadCanExplainCandidateValue(axis) &&
            (
              profile.primaryPlayedPositive ||
                profile.candidateBetter ||
                endpointPositive(candidateKind, RelativeCauseSourceSide.Candidate)
            )
        then List(candidateKind -> RelativeCauseSourceSide.Candidate)
        else Nil
      )
    axisCauses.distinct

  private def strategicReferenceLeadCause(
      comparison: StrategicAxisComparison,
      records: List[EvidenceRecord]
  ): RelativeCauseKind =
    val axis = comparison.axis
    axis.kind match
      case StrategicAxisKind.Target
          if RelativeCauseSignalProfile.comparisonHasStructuralConsequence(
            comparison,
            RelativeCauseSourceSide.Reference,
            records,
            Set(TransitionConsequenceKind.WeakPawnTargetCreated, TransitionConsequenceKind.WeakSquareTargetCreated)
          ) =>
        RelativeCauseKind.PawnWeaknessTarget
      case StrategicAxisKind.Target =>
        RelativeCauseKind.TargetPressureGain
      case StrategicAxisKind.SpaceCenter =>
        RelativeCauseKind.CenterControlGain
      case StrategicAxisKind.PawnBreak =>
        RelativeCauseKind.PawnBreakOpportunity
      case StrategicAxisKind.Activity =>
        RelativeCauseKind.ActivityGain
      case StrategicAxisKind.Counterplay if axis.polarity == StrategicAxisPolarity.Restrain =>
        RelativeCauseKind.OpponentRestriction
      case StrategicAxisKind.Counterplay
          if axis.polarity == StrategicAxisPolarity.Concede &&
            RelativeCauseSignalProfile.comparisonHasStructuralConsequence(
              comparison,
              RelativeCauseSourceSide.Reference,
              records,
              Set(TransitionConsequenceKind.KingSafetyConcession, TransitionConsequenceKind.KingRingPressureConcession)
            ) =>
        RelativeCauseKind.KingSafetyConcession
      case StrategicAxisKind.PlanCoherence =>
        RelativeCauseKind.PlanImprovement
      case _ =>
        RelativeCauseKind.MissedStrategicImprovement

  private def referenceLeadCanExplainStructuralLoss(axis: StrategicAxisDetail): Boolean =
    axis.polarity match
      case StrategicAxisPolarity.Loss | StrategicAxisPolarity.Release | StrategicAxisPolarity.Concede =>
        false
      case _ =>
        true

  private def candidateLeadCanExplainCandidateValue(axis: StrategicAxisDetail): Boolean =
    axis.polarity match
      case StrategicAxisPolarity.Loss | StrategicAxisPolarity.Release | StrategicAxisPolarity.Concede =>
        false
      case _ =>
        true

  private def strategicCandidateNegativeCause(
      comparison: StrategicAxisComparison,
      records: List[EvidenceRecord]
  ): RelativeCauseKind =
    val axis = comparison.axis
    axis.kind match
      case StrategicAxisKind.Target =>
        RelativeCauseKind.TargetPressureRelease
      case StrategicAxisKind.SpaceCenter =>
        RelativeCauseKind.StrategicConcession
      case StrategicAxisKind.PawnBreak =>
        RelativeCauseKind.StrategicConcession
      case StrategicAxisKind.Activity =>
        RelativeCauseKind.ActivityLoss
      case StrategicAxisKind.Counterplay if axis.polarity == StrategicAxisPolarity.Restrain =>
        RelativeCauseKind.StrategicConcession
      case StrategicAxisKind.Counterplay
          if axis.polarity == StrategicAxisPolarity.Concede &&
            RelativeCauseSignalProfile.comparisonHasStructuralConsequence(
              comparison,
              RelativeCauseSourceSide.Candidate,
              records,
              Set(TransitionConsequenceKind.KingSafetyConcession, TransitionConsequenceKind.KingRingPressureConcession)
            ) =>
        RelativeCauseKind.KingSafetyConcession
      case StrategicAxisKind.PlanCoherence =>
        RelativeCauseKind.PlanContradiction
      case _ =>
        RelativeCauseKind.StrategicConcession

  private def strategicCandidatePositiveCause(
      comparison: StrategicAxisComparison,
      records: List[EvidenceRecord]
  ): RelativeCauseKind =
    val axis = comparison.axis
    axis.kind match
      case StrategicAxisKind.Target
          if RelativeCauseSignalProfile.comparisonHasStructuralConsequence(
            comparison,
            RelativeCauseSourceSide.Candidate,
            records,
            Set(TransitionConsequenceKind.WeakPawnTargetCreated, TransitionConsequenceKind.WeakSquareTargetCreated)
          ) =>
        RelativeCauseKind.PawnWeaknessTarget
      case StrategicAxisKind.Target =>
        RelativeCauseKind.TargetPressureGain
      case StrategicAxisKind.SpaceCenter =>
        RelativeCauseKind.CenterControlGain
      case StrategicAxisKind.PawnBreak =>
        RelativeCauseKind.PawnBreakOpportunity
      case StrategicAxisKind.Activity =>
        RelativeCauseKind.ActivityGain
      case StrategicAxisKind.Counterplay if axis.polarity == StrategicAxisPolarity.Restrain =>
        RelativeCauseKind.OpponentRestriction
      case StrategicAxisKind.PlanCoherence =>
        RelativeCauseKind.PlanImprovement
      case _ =>
        RelativeCauseKind.StructuralImprovement

private[chessjudgment] object RelativeCauseSignalProfile:
  def from(
      fact: CandidateComparisonFact,
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord],
      referenceEndpointMoveOrderRecords: List[EvidenceRecord] = Nil,
      candidateEndpointMoveOrderRecords: List[EvidenceRecord] = Nil
  ): RelativeCauseSignalProfile =
    RelativeCauseSignalProfile(
      fact = fact,
      referenceRecords = referenceRecords,
      candidateRecords = candidateRecords,
      sharedRecords = sharedRecords,
      referenceEndpointMoveOrderRecords = referenceEndpointMoveOrderRecords,
      candidateEndpointMoveOrderRecords = candidateEndpointMoveOrderRecords
    )

  private[chessjudgment] def structuralConsequencesForSources(
      sources: List[EvidenceRef],
      records: List[EvidenceRecord]
  ): List[TransitionConsequence] =
    val sourceIds = sources.map(_.id).toSet
    records.collect {
      case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) if sourceIds(ref.id) =>
        payload.consequences
      case EvidenceRecord(ref, payload: PlanCausalEventEvidence, _)
          if sourceIds(ref.id) &&
            (
              payload.opponentResourceDeterrence.nonEmpty ||
                (
                  DirectOpponentRestrictionProof.rootMoveDirectlyRestrictsOpponent(payload) &&
                    DirectOpponentRestrictionProof.exactRootPawnBlockadeConsequences(payload).nonEmpty
                )
            ) =>
        payload.structuralConsequences
    }.flatten

  private[chessjudgment] def comparisonHasStructuralConsequence(
      comparison: StrategicAxisComparison,
      sourceSide: RelativeCauseSourceSide,
      records: List[EvidenceRecord],
      kinds: Set[TransitionConsequenceKind]
  ): Boolean =
    structuralConsequencesForSources(comparison.sourcesFor(sourceSide), records)
      .exists(consequence => kinds(consequence.kind))

  private[chessjudgment] def tacticalMechanismRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        (payload.canAnchorTacticalClaim || payload.canAnchorDefensiveClaim) &&
          (
            payload.kind != TacticalMechanismKind.MaterialGain ||
              materialGainMechanismOwnsAdmissiblePrimitive(record, payload, records)
          )
      case _ =>
        false
    }

  private[chessjudgment] def threatBranchTacticalMechanismRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    tacticalMechanismRecords(records).filter(_.ref.line.exists(_.role == LineNodeRole.Threat))

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

  private[chessjudgment] def onlyDefenseRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        payload.canAnchorDefensiveResource
      case _ => false
    }

  private[chessjudgment] def defensiveResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorDefensiveClaim
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        payload.isProofSignalDefensivePressure &&
          payload.episode.bestDefense.nonEmpty &&
          payload.onlyDefense.isEmpty
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
              payload.hasMaterialRecoveryWindow
          )
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.RecaptureChoice &&
          payload.canAnchorTacticalClaim &&
          payload.moveUci.exists(EvidenceRef.sameMove(_, rootMove))
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def moveOrderResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: MoveMotifEvidence, _) =>
        TacticalMotifClassifier.isRootCauseEligible(payload) && (payload.motif match
          case _: Motif.Zwischenzug => true
          case _                    => false
        )
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        payload.kind == RelationFactKind.Zwischenzug && payload.hasConcreteRelationProof
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.Tempo &&
          payload.hasMoverZwischenzug &&
          payload.canAnchorTacticalClaim
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
      eventLine: LineNodeRef,
      mover: chess.Color
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
      case record
          if record.ref.line.contains(eventLine) &&
            currentMoveMateThreatRecord(record, rootMove, mover) =>
        true
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def currentMoveMateThreatRecord(
      record: EvidenceRecord,
      rootMove: String,
      mover: chess.Color
  ): Boolean =
    record match
      case EvidenceRecord(ref, payload: ThreatEpisodeEvidence, _) =>
        ref.line.exists(line => EvidenceRef.sameMove(line.rootMove, rootMove)) &&
        payload.episode.driver == ThreatDriver.MateThreat &&
          payload.isProofSignalDefensivePressure &&
          payload.episode.threatActor == mover &&
          payload.episode.motifs.exists(motif =>
            motif.plyIndex == 0 &&
              motif.color == mover &&
              motif.move.exists(EvidenceRef.sameMove(_, rootMove))
          )
      case _ =>
        false

  private[chessjudgment] def wrongRecapturerChoiceRecords(
      fact: CandidateComparisonFact,
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    val referenceMove = normalizeMove(fact.referenceLine.rootMove)
    val candidateMove = normalizeMove(fact.candidateLine.rootMove)
    val referenceRecovery = recaptureResourceRecords(referenceRecords, referenceMove)
    val candidateLoss = materialLossRecords(candidateRecords)
    Option
      .when(
        EvidenceRef.sameDestinationDifferentOrigin(referenceMove, candidateMove) &&
          referenceRecovery.nonEmpty &&
          candidateLoss.nonEmpty &&
          EvidenceRecord.hasRootCaptureEvent(referenceRecords, referenceMove) &&
          EvidenceRecord.hasRootCaptureEvent(candidateRecords, candidateMove)
      )(
        EvidenceRecord.rootCaptureRecords(referenceRecords, referenceMove) ++
          EvidenceRecord.rootCaptureRecords(candidateRecords, candidateMove) ++
          referenceRecovery ++
          candidateLoss ++
          recaptureResourceRecords(candidateRecords, candidateMove)
      )
      .getOrElse(Nil)
      .distinctBy(_.ref.id)

  private[chessjudgment] def conversionWindowRecords(
      records: List[EvidenceRecord],
      eventLine: LineNodeRef,
      mover: chess.Color
  ): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(ref, payload: RelationFactEvidence, _)
          if payload.kind == RelationFactKind.BadPieceLiquidation =>
        RootOwnedEffectPolicy.relationRecordOwnsEventRoot(ref, payload, eventLine)
      case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
        RootOwnedEffectPolicy.lineRecordOwnsEventRoot(ref, payload, eventLine) &&
          payload
            .endgameTechniquesTriggeredByRootMove(eventLine.rootMove, RelativeCauseKind.ConversionSecured)
            .exists(horizon =>
              horizon.techniqueSide == mover &&
                horizon.entryPlyOffset == 0 &&
                payload.lineReplaySteps.lift(horizon.terminalPlyOffset).nonEmpty
          )
      case _ => false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def conversionMissRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.failedWinningEndgameTechniqueHorizons.nonEmpty
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def drawResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.maintainedDefensiveEndgameTechniqueHorizons.nonEmpty ||
          payload.hasProofSignalConsequence(LineConsequenceKind.DrawResource)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.DrawResource && payload.canAnchorDefensiveClaim
      case EvidenceRecord(ref, payload: RelationFactEvidence, _) =>
        payload.hasConcreteRelationProof &&
          payload.hasLineProof &&
          (
            payload.kind == RelationFactKind.StalemateTrap ||
              (
                payload.kind == RelationFactKind.PerpetualCheck &&
                  ref.confidence == EvidenceConfidence.LegalReplayVerified
              )
          )
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

  private[chessjudgment] def looseMaterialLiabilityRecords(
      mover: chess.Color,
      records: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    val looseAnchors =
      (records ++ sharedRecords).flatMap {
        case EvidenceRecord(_, payload: BoardFactEvidence, _) => payload.looseMaterialAnchors
        case _                                                => Nil
      }
    materialLossRecords(records).filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.materialCaptures.exists(capture =>
          capture.side != mover &&
            looseAnchors.exists(anchor =>
              anchor.side != mover &&
                anchor.detail.exists(detail =>
                  detail.subjectColor.contains(mover) &&
                    detail.subjectSquare.contains(capture.square) &&
                    detail.subjectRole.contains(capture.capturedRole)
                )
            )
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

  private[chessjudgment] def relationPayoffMaterialRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    val material = materialGainRecords(records)
    val relation =
      records.filter {
        case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
          payload.hasConcreteRelationProof &&
            relationMaterialPayoffKind(payload.kind)
        case _ =>
          false
      }
    Option.when(material.nonEmpty && relation.nonEmpty)(material ++ relation).getOrElse(Nil).distinctBy(_.ref.id)

  private[chessjudgment] def relationMaterialPayoffKind(kind: RelationFactKind): Boolean =
    kind match
      case RelationFactKind.DefenderTrade | RelationFactKind.Overload | RelationFactKind.Deflection |
          RelationFactKind.DiscoveredAttack | RelationFactKind.Fork | RelationFactKind.Decoy |
          RelationFactKind.Interference | RelationFactKind.Clearance | RelationFactKind.XRay |
          RelationFactKind.Battery | RelationFactKind.Pin | RelationFactKind.Skewer =>
        true
      case _ =>
        false

  private[chessjudgment] def relationTargetPressureProofKind(kind: RelationFactKind): Boolean =
    relationMaterialPayoffKind(kind) ||
      kind == RelationFactKind.Zwischenzug

  private[chessjudgment] def materialGainRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload
          .rootOwnedCausalEpisodes(payload.line.rootMove)
          .exists(_.consequence.kind == LineConsequenceKind.MaterialGain)
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.MaterialGain &&
          payload.canAnchorTacticalClaim &&
          materialGainMechanismOwnsAdmissiblePrimitive(record, payload, records)
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private def materialGainMechanismOwnsAdmissiblePrimitive(
      record: EvidenceRecord,
      mechanism: TacticalMechanismEvidence,
      records: List[EvidenceRecord]
  ): Boolean =
    mechanism.line.exists { eventLine =>
      RootOwnedEffectPolicy.tacticalCarrierOwnsEventRoot(record.ref, mechanism, eventLine) &&
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
        payload.hasAnyConsequence(
          Set(
            TransitionConsequenceKind.PassedPawnProgress,
            TransitionConsequenceKind.PromotionPressureGain
          )
        )
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.hasPassedPawnResourceSignal
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def passedPawnConcessionRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
        payload.hasAnyConsequence(
          Set(
            TransitionConsequenceKind.PassedPawnConcession,
            TransitionConsequenceKind.PromotionPressureConcession
          )
        )
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private def strategicMechanismRecords(
      records: List[EvidenceRecord]
  )(mechanismPredicate: StrategicMechanismEvidence => Boolean): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _) if mechanismPredicate(payload) =>
        record
    }.distinctBy(_.ref.id)

  private[chessjudgment] def currentMoveStrategicSupportRecords(
      fact: CandidateComparisonFact,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _)
          if record.referencesLine(fact.candidateLine) &&
            payload.canSupportStrategicCause &&
            currentMoveStrategicSupportSignals(payload, fact.candidateLine, records).nonEmpty =>
        record
    }.distinctBy(_.ref.id)

  private[chessjudgment] def currentMoveStrategicSupportSignals(
      payload: StrategicMechanismEvidence,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[StrategicMechanismSignal] =
    payload.signals.filter(signal =>
      currentMoveStrategicSupportSignal(signal, candidateLine, records) ||
        currentMoveCounterBreakSupportSignal(signal, candidateLine, records)
    )

  private[chessjudgment] def currentMoveStrategicSupportSignal(
      signal: StrategicMechanismSignal,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    (
      signal.kind == StrategicMechanismSignalKind.StructuralDelta ||
        signal.kind == StrategicMechanismSignalKind.PlanPressure &&
          signal.source.layer == EvidenceLayer.PlanCausalEvent
      ) &&
      signal.source.scope == EvidenceScope.PlayedTransition &&
      signal.source.line.contains(candidateLine) &&
      currentMoveStrategicSupportCauseKindsForSignal(signal, records).nonEmpty

  private[chessjudgment] def currentMoveStrategicSupportCauseKindsForSignal(
      signal: StrategicMechanismSignal,
      records: List[EvidenceRecord]
  ): List[RelativeCauseKind] =
    val consequences = structuralConsequencesForSources(List(signal.source), records)
    signal.axis.toList.flatMap { axis =>
      axis.kind match
        case StrategicAxisKind.Target if axis.polarity == StrategicAxisPolarity.Gain =>
          if consequences.exists(consequence =>
              consequence.kind == TransitionConsequenceKind.WeakPawnTargetCreated ||
                consequence.kind == TransitionConsequenceKind.WeakSquareTargetCreated
            )
          then List(RelativeCauseKind.PawnWeaknessTarget)
          else if consequences.exists(consequence =>
              consequence.kind == TransitionConsequenceKind.TargetPressureGain ||
                consequence.kind == TransitionConsequenceKind.KingSafetyPressure ||
                consequence.kind == TransitionConsequenceKind.KingRingPressureGain
            )
          then List(RelativeCauseKind.TargetPressureGain)
          else Nil
        case StrategicAxisKind.Activity
            if StrategicMechanismContrastEvidence.currentMoveActivityValueAxis(axis) =>
          List(RelativeCauseKind.ActivityGain)
        case StrategicAxisKind.PlanCoherence if axis.polarity == StrategicAxisPolarity.Concede =>
          List(RelativeCauseKind.PlanContradiction)
        case StrategicAxisKind.PlanCoherence if StrategicMechanismContrastEvidence.currentMovePlanCoherenceAxis(axis) =>
          List(RelativeCauseKind.PlanImprovement)
        case StrategicAxisKind.Counterplay if axis.polarity == StrategicAxisPolarity.Restrain &&
            consequences.exists(consequence =>
              consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
                consequence.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
            ) =>
          List(RelativeCauseKind.OpponentRestriction)
        case StrategicAxisKind.PawnBreak if axis.polarity == StrategicAxisPolarity.Release &&
            consequences.exists(_.kind == TransitionConsequenceKind.PawnTensionResolution) =>
          List(RelativeCauseKind.PawnBreakOpportunity)
        case StrategicAxisKind.PawnBreak
            if Set(
              StrategicAxisPolarity.Support,
              StrategicAxisPolarity.Preserve,
              StrategicAxisPolarity.Gain
            )(axis.polarity) && consequences.exists(_.kind == TransitionConsequenceKind.PawnTensionGain) =>
          List(RelativeCauseKind.PawnBreakOpportunity)
        case _ =>
          Nil
    }

  private[chessjudgment] def currentMoveRelationPayoffRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    currentMoveRelationRecords(candidateLine, records, relationMaterialPayoffKind)

  private[chessjudgment] def currentMoveTargetPressureRelationRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    currentMoveRelationRecords(candidateLine, records, relationTargetPressureProofKind)

  private def currentMoveRelationRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord],
      kindAllowed: RelationFactKind => Boolean
  ): List[EvidenceRecord] =
    records.filter {
      case record @ EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        payload.hasConcreteRelationProof &&
          payload.hasLineProof &&
          payload.lineProofCount > 1 &&
          kindAllowed(payload.kind) &&
          (
            record.referencesLine(candidateLine) ||
              record.ref.line.exists(line => EvidenceRef.sameMove(line.rootMove, candidateLine.rootMove)) ||
              payload.mentionsLineMove(candidateLine.rootMove)
          )
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def currentMoveCounterBreakSupportSignal(
      signal: StrategicMechanismSignal,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    val restrainedFiles = records.collect {
      case EvidenceRecord(ref, PawnStructureFactEvidence(_, Some(pawnPlay)), _)
          if ref.id == signal.source.id =>
        pawnPlay.counterBreakFiles.flatMap(normalizedFile)
    }.flatten.toSet
    val carrierFiles =
      StrategicMechanismContrastEvidence
        .currentMoveBreakCarrierRecords(candidateLine, records)
        .flatMap {
          case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
            payload.consequences
              .filter(consequence =>
                consequence.kind == TransitionConsequenceKind.PawnTensionGain ||
                  consequence.kind == TransitionConsequenceKind.PawnTensionResolution
              )
              .flatMap(_.subjects.flatMap(breakCarrierFiles))
          case _ =>
            Nil
        }
        .toSet
    signal.kind == StrategicMechanismSignalKind.PawnStructure &&
      signal.source.layer == EvidenceLayer.PawnStructure &&
      signal.axis.exists(axis =>
        axis.kind == StrategicAxisKind.Counterplay && axis.polarity == StrategicAxisPolarity.Restrain
      ) &&
      restrainedFiles.nonEmpty &&
      restrainedFiles.intersect(carrierFiles).nonEmpty

  private def normalizedFile(raw: String): Option[String] =
    "[a-h]".r.findFirstIn(Option(raw).getOrElse("").trim.toLowerCase)

  private def breakCarrierFiles(subject: String): List[String] =
    val normalized = Option(subject).getOrElse("").trim.toLowerCase
    if normalized.startsWith("break-file:") then
      normalizedFile(normalized.stripPrefix("break-file:")).toList
    else if normalized.startsWith("created-tension:") || normalized.startsWith("resolved-tension:") then
      "[a-h][1-8]".r.findAllIn(normalized).map(_.take(1)).toList.distinct
    else Nil

  private[chessjudgment] def referenceOnlyDefenseFunctionRecords(
      fact: CandidateComparisonFact,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        !payload.insufficientData &&
          payload.onlyDefense.exists(move => normalizeMove(move) == normalizeMove(fact.referenceLine.rootMove)) &&
          payload.defenseRequired
      case _ =>
        false
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
      rootMove: String
  ): List[EvidenceRecord] =
    val material = sacrificeMaterialRecords(records, rootMove)
    val compensation = compensationSupportRecords(records)
    Option
      .when(material.nonEmpty && compensation.nonEmpty)((material ++ compensation).distinctBy(_.ref.id))
      .getOrElse(Nil)

  private def sacrificeMaterialRecords(records: List[EvidenceRecord], rootMove: String): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.consequencesForRootMove(rootMove).exists(_.kind == LineConsequenceKind.Sacrifice)
      case _ =>
        false
    }

  private def compensationSupportRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    strategicCompensationRecords(records)

  private def strategicCompensationRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    strategicMechanismRecords(records)(_.canSupportCompensation)

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase
