package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.evaluation.JudgmentThresholds
import lila.chessjudgment.model.judgment.ThreatDriver
import lila.chessjudgment.analysis.tactical.TacticalMotifClassifier
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.judgment.*

private[chessjudgment] final case class RelativeCauseDraft(
    kind: RelativeCauseKind,
    support: List[EvidenceRecord],
    sourceSide: Option[RelativeCauseSourceSide] = None,
    attributionKind: CauseAttributionKind = CauseAttributionKind.Unattributed
)

private[chessjudgment] final case class RelativeCauseSignalProfile(
    fact: CandidateComparisonFact,
    candidateSet: Option[CandidateSetDescriptor],
    referenceRecords: List[EvidenceRecord],
    candidateRecords: List[EvidenceRecord],
    sharedRecords: List[EvidenceRecord]
):
  val involvedRecords: List[EvidenceRecord] =
    (referenceRecords ++ candidateRecords).distinctBy(_.ref.id)
  val allRecords: List[EvidenceRecord] =
    (referenceRecords ++ candidateRecords ++ sharedRecords).distinctBy(_.ref.id)
  val badLoss: Boolean =
    fact.comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP
  val actionablePlayedLoss: Boolean =
    fact.kind == CandidateComparisonKind.PlayedVsBest &&
      (fact.comparison.verdict match
        case MoveChoiceVerdict.Inaccuracy | MoveChoiceVerdict.Mistake | MoveChoiceVerdict.Blunder => true
        case _                                                                                   => false
      )
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
  val referenceTacticalRisk: List[EvidenceRecord] =
    RelativeCauseSignalProfile.tacticalRiskRecords(referenceRecords)
  val referenceConversionWindow: List[EvidenceRecord] =
    RelativeCauseSignalProfile.conversionWindowRecords(referenceRecords)
  val candidateConversionWindow: List[EvidenceRecord] =
    RelativeCauseSignalProfile.conversionWindowRecords(candidateRecords)
  val candidateConversionMissWindow: List[EvidenceRecord] =
    RelativeCauseSignalProfile.conversionMissRecords(candidateRecords)
  val referenceDrawResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.drawResourceRecords(referenceRecords)
  val candidateDrawResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.drawResourceRecords(candidateRecords)
  val referenceRecaptureResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.recaptureResourceRecords(referenceRecords, fact.referenceLine.rootMove)
  val candidateRecaptureResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.recaptureResourceRecords(candidateRecords, fact.candidateLine.rootMove)
  val referenceMoveOrderResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.moveOrderResourceRecords(referenceRecords)
  val candidateMoveOrderResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.moveOrderResourceRecords(candidateRecords)
  val candidateTempoLiability: List[EvidenceRecord] =
    RelativeCauseSignalProfile.tempoLiabilityRecords(fact, candidateRecords)
  val referenceForcingLineResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.forcingLineResourceRecords(referenceRecords)
  val candidateForcingLineResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.forcingLineResourceRecords(candidateRecords)
  val candidateWrongRecapturerChoice: List[EvidenceRecord] =
    RelativeCauseSignalProfile.wrongRecapturerChoiceRecords(fact, referenceRecords, candidateRecords)
  val referenceDefensiveResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.defensiveResourceRecords(referenceRecords)
  val candidateDefensiveResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.defensiveResourceRecords(candidateRecords)
  val sharedDefensiveResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.defensiveResourceRecords(sharedRecords)
  val referenceLooseMaterialExploit: List[EvidenceRecord] =
    RelativeCauseSignalProfile.looseMaterialExploitRecords(referenceRecords, sharedRecords)
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
  val referenceEndgameResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.endgameResourceRecords(referenceRecords)
  val candidateEndgameResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.endgameResourceRecords(candidateRecords)
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
  def drafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    if !profile.fact.hasDistinctRootMoves then Nil
    else suppressGenericCompanions(rawDrafts(profile))

  def playedMoveCandidateSideComparison(kind: CandidateComparisonKind): Boolean =
    kind == CandidateComparisonKind.PlayedVsBest || kind == CandidateComparisonKind.PlayedVsAlternative

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
        candidateForcingLineResource.exists(RelativeCauseSignalProfile.currentMoveMateThreatRecord)
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
        candidateForcingLineResource.filter(RelativeCauseSignalProfile.currentMoveMateThreatRecord)
      else candidateForcingLineResource
    List(
      causeDraft(
        RelativeCauseKind.OnlyMoveNecessity,
        Nil,
        fact.kind == CandidateComparisonKind.PlayedVsBest &&
          candidateSet.exists(_.onlyMove) &&
          badLoss,
        Some(RelativeCauseSourceSide.Reference)
      ),
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
        referenceMoveOrderResource.nonEmpty && candidateMoveOrderResource.isEmpty && badLoss
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
      causeDraft(RelativeCauseKind.MissedTacticalResource, referenceLooseMaterialExploit, referenceLooseMaterialExploit.nonEmpty && badLoss),
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
    Option.when(condition)(
      RelativeCauseDraft(
        kind,
        support.distinctBy(_.ref.id),
        sourceSide,
        attributionKind.getOrElse(RelativeCauseKind.defaultAttributionKind(kind, sourceSide))
      )
    )

  private def mechanismDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    val actionableLoss =
      profile.fact.comparison.verdict match
        case MoveChoiceVerdict.Inaccuracy | MoveChoiceVerdict.Mistake | MoveChoiceVerdict.Blunder => true
        case _                                                                                   => false
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
            wrongRecapturerProven = profile.candidateWrongRecapturerChoice.nonEmpty,
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
      wrongRecapturerProven: Boolean = false,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind
  ): List[RelativeCauseDraft] =
    records.flatMap {
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.canAnchorTacticalClaim =>
        val skipWrongRecapturerCompanion =
          badLoss && payload.kind == TacticalMechanismKind.RecaptureChoice && wrongRecapturerProven
        val causeKind =
          if candidateValue then candidateValueMechanismCauseKind(payload)
          else Some(TacticalMechanismKind.relativeCauseKind(payload.kind, badLoss, playedCandidate))
        Option.unless(skipWrongRecapturerCompanion)(causeKind).flatten.map(kind =>
          RelativeCauseDraft(
            kind,
            List(record),
            Some(sourceSide),
            attributionKind
          )
        )
      case _ =>
        None
    }

  private def candidateValueMechanismCauseKind(
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
        strategicContrastCauseKinds(payload, profile).map { case (kind, sourceSide) =>
          RelativeCauseDraft(
            kind,
            List(record),
            Some(sourceSide),
            RelativeCauseKind.defaultAttributionKind(kind, Some(sourceSide))
          )
        }
      case _ =>
        Nil
    }.distinctBy(draft => (draft.kind, draft.sourceSide, draft.support.map(_.ref.id).sorted.mkString("|")))

  private def currentMoveStrategicSupportDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    val candidateCurrentMoveCanOwnValue =
      profile.playedCandidateSideComparison &&
        profile.candidateBetter
    val ownedEpisodeFailure = profile.candidateCurrentMoveStrategicSupport.exists {
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        currentMoveOwnedPlanEpisodeCause(
          RelativeCauseKind.PlanContradiction,
          payload,
          profile.fact.candidateLine
        )
      case _ =>
        false
    }
    if !profile.exactReferenceMove && !candidateCurrentMoveCanOwnValue && !ownedEpisodeFailure then Nil
    else
      profile.candidateCurrentMoveStrategicSupport.flatMap {
        case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
          val relationPayoffs =
            RelativeCauseSignalProfile.currentMoveRelationPayoffRecords(profile.fact.candidateLine, profile.allRecords)
          val targetPressureRelationProofs =
            RelativeCauseSignalProfile.currentMoveTargetPressureRelationRecords(profile.fact.candidateLine, profile.allRecords)
          val concreteTargetCarriers =
            RelativeCauseSignalProfile.currentMoveConcreteTargetCarrierRecords(profile.fact.candidateLine, profile.allRecords)
          val causeKinds =
            currentMoveStrategicSupportCauseKinds(payload, profile.fact.candidateLine, profile.allRecords)
              .filter(kind =>
                if kind == RelativeCauseKind.OpponentRestriction then
                  currentMoveConcreteCounterplayCanOwnValue(kind, payload, profile)
                else if kind == RelativeCauseKind.TargetPressureGain then
                  profile.exactReferenceMove &&
                    (concreteTargetCarriers.nonEmpty || targetPressureRelationProofs.nonEmpty)
                else if kind == RelativeCauseKind.PawnWeaknessTarget then
                  profile.exactReferenceMove &&
                    (concreteTargetCarriers.nonEmpty || relationPayoffs.nonEmpty)
                else if kind == RelativeCauseKind.PlanImprovement then
                  profile.candidateProvedValue &&
                    currentMoveOwnedPlanEpisodeCause(kind, payload, profile.fact.candidateLine)
                else if kind == RelativeCauseKind.PlanContradiction then
                  currentMoveOwnedPlanEpisodeCause(kind, payload, profile.fact.candidateLine)
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
          causeKinds.map(kind =>
            val support =
              if kind == RelativeCauseKind.TargetPressureGain then record :: targetPressureRelationProofs
              else if kind == RelativeCauseKind.PawnWeaknessTarget then record :: relationPayoffs
              else List(record)
            RelativeCauseDraft(
              kind,
              support,
              Some(RelativeCauseSourceSide.Candidate),
              RelativeCauseKind.defaultAttributionKind(kind, Some(RelativeCauseSourceSide.Candidate))
            )
          )
        case _ =>
          Nil
      }.distinctBy(draft => (draft.kind, draft.support.map(_.ref.id).sorted.mkString("|")))

  private def currentMoveOwnedPlanEpisodeCause(
      kind: RelativeCauseKind,
      payload: StrategicMechanismEvidence,
      candidateLine: LineNodeRef
  ): Boolean =
    payload.kind == StrategicMechanismKind.PlanPressure &&
      payload.signals.exists(signal =>
        signal.kind == StrategicMechanismSignalKind.PlanPressure &&
          signal.source.layer == EvidenceLayer.PlanCausalEvent &&
          signal.source.line.contains(candidateLine) &&
          signal.axis.exists(axis =>
            axis.kind == StrategicAxisKind.PlanCoherence &&
              (kind match
                case RelativeCauseKind.PlanImprovement =>
                  StrategicMechanismContrastEvidence.currentMovePlanCoherenceAxis(axis)
                case RelativeCauseKind.PlanContradiction =>
                  axis.polarity == StrategicAxisPolarity.Concede
                case _ =>
                  false)
          )
      )

  private def currentMoveConcreteActivityCanOwnValue(
      kind: RelativeCauseKind,
      payload: StrategicMechanismEvidence,
      profile: RelativeCauseSignalProfile
  ): Boolean =
    kind == RelativeCauseKind.ActivityGain &&
      currentMoveConcreteActivitySignals(payload, profile.fact.candidateLine, profile.allRecords)
        .exists(signal => RelativeCauseSignalProfile.currentMoveConcreteActivitySource(signal.source, profile.allRecords))

  private def currentMoveConcreteActivitySignals(
      payload: StrategicMechanismEvidence,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[StrategicMechanismSignal] =
    payload.signals.filter(signal =>
      RelativeCauseSignalProfile.currentMoveStrategicSupportSignal(signal, candidateLine, records) &&
        signal.axis.exists(axis =>
          RelativeCauseSignalProfile.currentMoveActivityValueAxis(axis)
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
    val axisCauses =
      payload.actionableComparisons.flatMap(axisComparison =>
        val axis = axisComparison.axis
        if axisComparison.referenceLead && structuralLoss && referenceLeadCanExplainStructuralLoss(axis) then
          List(strategicReferenceLeadCause(axisComparison, profile.allRecords) -> RelativeCauseSourceSide.Reference)
        else if axisComparison.candidateNegative && structuralLoss then
          List(strategicCandidateNegativeCause(axisComparison, profile.allRecords) -> RelativeCauseSourceSide.Candidate)
        else if axisComparison.candidateLead && (profile.primaryPlayedPositive || profile.candidateBetter) &&
            candidateLeadCanExplainCandidateValue(axis) then
          List(strategicCandidatePositiveCause(axisComparison, profile.allRecords) -> RelativeCauseSourceSide.Candidate)
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
        RelativeCauseKind.CenterControlGain
      case StrategicAxisKind.PawnBreak =>
        RelativeCauseKind.StrategicConcession
      case StrategicAxisKind.Activity =>
        RelativeCauseKind.ActivityLoss
      case StrategicAxisKind.Counterplay if axis.polarity == StrategicAxisPolarity.Restrain =>
        RelativeCauseKind.OpponentRestriction
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

  private def suppressGenericCompanions(
      drafts: List[RelativeCauseDraft]
  ): List[RelativeCauseDraft] =
    drafts.filterNot {
      case draft @ RelativeCauseDraft(RelativeCauseKind.RecaptureRecoveryWindow, _, _, _) =>
        drafts.exists(other =>
          other.kind == RelativeCauseKind.WrongRecapturer &&
            sameDraftChannel(draft, other) &&
            supportOverlaps(draft.support, other.support)
        )
      case draft @ RelativeCauseDraft(RelativeCauseKind.TempoLoss, _, _, _) =>
        drafts.exists(other =>
          other.kind == RelativeCauseKind.WrongMoveOrder &&
            sameDraftChannel(draft, other) &&
            supportOverlaps(draft.support, other.support)
        )
      case _ =>
        false
    }

  private def sameDraftChannel(left: RelativeCauseDraft, right: RelativeCauseDraft): Boolean =
    left.sourceSide == right.sourceSide && left.attributionKind == right.attributionKind

  private def supportOverlaps(left: List[EvidenceRecord], right: List[EvidenceRecord]): Boolean =
    val leftIds = left.map(_.ref.id).toSet
    leftIds.nonEmpty && right.exists(record => leftIds.contains(record.ref.id))

private[chessjudgment] object RelativeCauseSignalProfile:
  def from(
      fact: CandidateComparisonFact,
      candidateSet: Option[CandidateSetDescriptor],
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord]
  ): RelativeCauseSignalProfile =
    RelativeCauseSignalProfile(
      fact = fact,
      candidateSet = candidateSet,
      referenceRecords = referenceRecords,
      candidateRecords = candidateRecords,
      sharedRecords = sharedRecords
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
          if sourceIds(ref.id) && payload.opponentResourceDeterrence.nonEmpty =>
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
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorTacticalClaim || payload.canAnchorDefensiveClaim
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

  private[chessjudgment] def tacticalRiskRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorTacticalClaim
      case _ =>
        false
    }

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

  private[chessjudgment] def forcingLineResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasLineEvent(LineEventKind.Mate) ||
          payload.hasProofSignalConsequence(LineConsequenceKind.Mate)
      case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) =>
        mate.nonEmpty
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.KingForcing && payload.hasConcreteProof
      case record if currentMoveMateThreatRecord(record) =>
        true
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def currentMoveMateThreatRecord(record: EvidenceRecord): Boolean =
    record match
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        payload.episode.driver == ThreatDriver.MateThreat &&
          payload.isProofSignalDefensivePressure &&
          payload.episode.motifs.exists(_.plyIndex == 0)
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

  private[chessjudgment] def conversionWindowRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: RelationFactEvidence, _) if payload.kind == RelationFactKind.BadPieceLiquidation =>
        true
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.maintainedWinningEndgameTechniqueHorizons.nonEmpty
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
        payload.hasProofSignalConsequence(LineConsequenceKind.Promotion) ||
          payload.hasProofSignalConsequence(LineConsequenceKind.PromotionRace) ||
          payload.materialOutcomeProfile.gainSignals.contains(LineMaterialOutcomeSignal.PromotionGain) ||
          payload.materialOutcomeProfile.lossSignals.contains(LineMaterialOutcomeSignal.PromotionLoss)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.PawnPromotion && payload.canAnchorTacticalClaim
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def looseMaterialExploitRecords(
      records: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    val material = materialGainRecords(records)
    val relation = looseMaterialRelationRecords(records)
    Option
      .when((material.nonEmpty || relation.nonEmpty) && looseMaterialContextPresent(records ++ sharedRecords))(
        material ++ relation
      )
      .getOrElse(Nil)
      .distinctBy(_.ref.id)

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

  private def looseMaterialContextPresent(records: List[EvidenceRecord]): Boolean =
    records.exists {
      case EvidenceRecord(_, payload: BoardFactEvidence, _) =>
        payload.looseMaterialAnchors.nonEmpty
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        looseMaterialRelation(payload)
      case _ =>
        false
    }

  private def looseMaterialRelationRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        looseMaterialRelation(payload)
      case _ =>
        false
    }

  private def looseMaterialRelation(payload: RelationFactEvidence): Boolean =
    payload.hasConcreteRelationProof &&
      (
        payload.kind == RelationFactKind.HangingPiece ||
          payload.kind == RelationFactKind.TrappedPiece ||
          payload.kind == RelationFactKind.Domination
      )

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

  private def materialGainRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.materialOutcomeProfile.gainMagnitude != LineMaterialOutcomeMagnitude.None ||
          payload.hasProofSignalConsequence(LineConsequenceKind.MaterialGain)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.MaterialGain && payload.canAnchorTacticalClaim
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private def materialLossRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.materialOutcomeProfile.lossMagnitude != LineMaterialOutcomeMagnitude.None ||
          payload.hasProofSignalConsequence(LineConsequenceKind.MaterialLoss)
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

  private[chessjudgment] def endgameResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    strategicMechanismRecords(records)(payload =>
      payload.kind == StrategicMechanismKind.Endgame && payload.canSupportStrategicCause
    )

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
        case StrategicAxisKind.Activity if currentMoveActivityValueAxis(axis) =>
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

  private[chessjudgment] def currentMoveActivityValueAxis(axis: StrategicAxisDetail): Boolean =
    StrategicMechanismContrastEvidence.currentMoveActivityValueAxis(axis)

  private[chessjudgment] def currentMoveConcreteActivityCarrierRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    StrategicMechanismContrastEvidence.currentMoveConcreteActivityCarrierRecords(candidateLine, records)

  private[chessjudgment] def currentMoveConcreteTargetCarrierRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    StrategicMechanismContrastEvidence.currentMoveConcreteTargetCarrierRecords(candidateLine, records)

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

  private[chessjudgment] def currentMoveConcretePlanCarrierRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    StrategicMechanismContrastEvidence.currentMoveConcretePlanCarrierRecords(candidateLine, records)

  private[chessjudgment] def currentMoveConcreteActivitySource(
      source: EvidenceRef,
      records: List[EvidenceRecord]
  ): Boolean =
    StrategicMechanismContrastEvidence.currentMoveConcreteActivitySource(source, records)

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

  private[chessjudgment] def currentMoveCounterplayRestraintCarrier(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    StrategicMechanismContrastEvidence.currentMoveCounterplayRestraintCarrier(candidateLine, records)

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
