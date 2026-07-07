package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.analysis.evaluation.JudgmentThresholds
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
    RelativeCauseSignalProfile.recaptureResourceRecords(referenceRecords)
  val candidateRecaptureResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.recaptureResourceRecords(candidateRecords)
  val referenceMoveOrderResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.moveOrderResourceRecords(referenceRecords)
  val candidateMoveOrderResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.moveOrderResourceRecords(candidateRecords)
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
    RelativeCauseSignalProfile.looseMaterialLiabilityRecords(candidateRecords, sharedRecords)
  val referencePromotionResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.promotionRaceRecords(referenceRecords)
  val candidatePromotionResource: List[EvidenceRecord] =
    RelativeCauseSignalProfile.promotionRaceRecords(candidateRecords)
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
  val materialSwingSupport: List[EvidenceRecord] =
    RelativeCauseSignalProfile.materialSwingSupportRecords(referenceRecords, candidateRecords)
  val materialDeteriorationSupport: List[EvidenceRecord] =
    RelativeCauseSignalProfile.materialDeteriorationSupportRecords(referenceRecords, candidateRecords)
  val materialLossSupport: List[EvidenceRecord] =
    (materialSwingSupport ++ materialDeteriorationSupport).distinctBy(_.ref.id)
  val sacrificeCompensationSupport: List[EvidenceRecord] =
    RelativeCauseSignalProfile.sacrificeCompensationSupportRecords(candidateRecords)
  val strategicContrasts: List[EvidenceRecord] =
    RelativeCauseSignalProfile.strategicContrastRecords(fact, allRecords)
  val candidateCurrentMoveStrategicSupport: List[EvidenceRecord] =
    RelativeCauseSignalProfile.currentMoveStrategicSupportRecords(
      fact,
      if exactReferenceMove then candidateRecords ++ sharedRecords else candidateRecords
    )

private[chessjudgment] object RelativeCauseDraftPlanner:
  def drafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    suppressGenericCompanions(rawDrafts(profile))

  def producedCauseBindableRecords(profile: RelativeCauseSignalProfile): List[EvidenceRecord] =
    drafts(profile).flatMap(_.support).distinctBy(_.ref.id)

  def playedMoveCandidateSideComparison(kind: CandidateComparisonKind): Boolean =
    kind == CandidateComparisonKind.PlayedVsBest || kind == CandidateComparisonKind.PlayedVsAlternative

  private def rawDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    import profile.*
    val exactCurrentMoveRecaptureResource =
      exactReferenceMove &&
        candidateRecaptureResource.exists {
          case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
            payload.kind == TacticalMechanismKind.RecaptureChoice &&
              payload.moveUci.exists(EvidenceRef.sameMove(_, fact.candidateLine.rootMove))
          case EvidenceRecord(_, payload: LineFactEvidence, _) =>
            payload.rootOwnedProofSignalConsequences(fact.candidateLine.rootMove).exists(consequence =>
              consequence.kind == LineConsequenceKind.RecaptureSequence ||
                consequence.kind == LineConsequenceKind.RecoveryWindow
            )
          case _ =>
            false
        }
    List(
      causeDraft(
        RelativeCauseKind.OnlyMoveNecessity,
        Nil,
        fact.comparison.candidateSet.exists(_.onlyMove) && badLoss,
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
        candidateOnlyDefense,
        candidateOnlyDefense.nonEmpty && candidateBetter,
        Some(RelativeCauseSourceSide.Candidate)
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
        candidateDefensiveResource.nonEmpty && candidateBetter,
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
        candidateMoveOrderResource,
        candidateMoveOrderResource.nonEmpty && (badLoss || candidateBetter)
      ),
      causeDraft(
        RelativeCauseKind.KingForcing,
        referenceForcingLineResource,
        referenceForcingLineResource.nonEmpty && badLoss,
        Some(RelativeCauseSourceSide.Reference)
      ),
      causeDraft(
        RelativeCauseKind.KingForcing,
        candidateForcingLineResource,
        candidateForcingLineResource.nonEmpty && candidateBetter,
        Some(RelativeCauseSourceSide.Candidate)
      ),
      causeDraft(RelativeCauseKind.ConversionMiss, referenceConversionWindow, referenceConversionWindow.nonEmpty && badLoss),
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
      causeDraft(RelativeCauseKind.ConversionMiss, referencePromotionResource, referencePromotionResource.nonEmpty && badLoss),
      causeDraft(RelativeCauseKind.ConversionSecured, candidatePromotionResource, candidatePromotionResource.nonEmpty && candidateProvedValue),
      causeDraft(RelativeCauseKind.MissedTacticalResource, referenceLooseMaterialExploit, referenceLooseMaterialExploit.nonEmpty && badLoss),
      causeDraft(
        if playedCandidateSideComparison then RelativeCauseKind.TacticalRefutationOfPlayed
        else RelativeCauseKind.CandidateTacticalLiability,
        candidateLooseMaterialLiability,
        candidateLooseMaterialLiability.nonEmpty && badLoss
      ),
      causeDraft(
        RelativeCauseKind.MaterialSwing,
        materialLossSupport,
        majorLoss &&
          materialLossSupport.nonEmpty
      ),
      causeDraft(
        RelativeCauseKind.SacrificeCompensation,
        sacrificeCompensationSupport,
        (primaryPlayedPositive || candidateBetter) &&
          sacrificeCompensationSupport.nonEmpty
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
        attributionKind.getOrElse(defaultAttributionKind(kind, sourceSide))
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
            profile.referenceTacticalMechanism,
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
            profile.candidateTacticalMechanism,
            badLoss = true,
            playedCandidate = profile.playedCandidateSideComparison,
            sourceSide = RelativeCauseSourceSide.Candidate,
            attributionKind = CauseAttributionKind.CandidateAllowsLiability
          )
        )
        .getOrElse(Nil)
    val candidateBetterCauses =
      Option
        .when(profile.candidateBetter)(
          mechanismCauseKinds(
            profile.candidateTacticalMechanism,
            badLoss = false,
            sourceSide = RelativeCauseSourceSide.Candidate,
            attributionKind = CauseAttributionKind.CandidateCreatesValue
          )
        )
        .getOrElse(Nil)
    referenceCauses ++ candidateBadCauses ++ candidateBetterCauses

  private def mechanismCauseKinds(
      records: List[EvidenceRecord],
      badLoss: Boolean,
      playedCandidate: Boolean = false,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind
  ): List[RelativeCauseDraft] =
    records.collect {
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.canAnchorTacticalIdea =>
        RelativeCauseDraft(
          TacticalMechanismKind.relativeCauseKind(payload.kind, badLoss, playedCandidate),
          List(record),
          Some(sourceSide),
          attributionKind
        )
    }

  private def strategicContrastDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    profile.strategicContrasts.flatMap {
      case record @ EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        strategicContrastCauseKinds(payload, profile).map { case (kind, sourceSide) =>
          RelativeCauseDraft(
            kind,
            strategicContrastSupportRecords(record, payload, profile, kind, sourceSide),
            Some(sourceSide),
            defaultAttributionKind(kind, Some(sourceSide))
          )
        }
      case _ =>
        Nil
    }.distinctBy(draft => (draft.kind, draft.sourceSide, draft.support.map(_.ref.id).sorted.mkString("|")))

  private def strategicContrastSupportRecords(
      record: EvidenceRecord,
      payload: StrategicMechanismContrastEvidence,
      profile: RelativeCauseSignalProfile,
      kind: RelativeCauseKind,
      sourceSide: RelativeCauseSourceSide
  ): List[EvidenceRecord] =
    val sameRootBreakCarriers =
      if exactSameRootConcreteCarrierCause(payload, profile, kind, sourceSide) && kind == RelativeCauseKind.OpponentRestriction then
        RelativeCauseSignalProfile.currentMoveSameRootBreakCarrierRecords(profile.fact.candidateLine, profile.allRecords)
      else Nil
    val sameRootActivityCarriers =
      if exactSameRootConcreteCarrierCause(payload, profile, kind, sourceSide) && kind == RelativeCauseKind.ActivityGain then
        RelativeCauseSignalProfile.currentMoveConcreteActivityCarrierRecords(profile.fact.candidateLine, profile.allRecords)
      else Nil
    val sameRootTargetCarriers =
      if exactSameRootConcreteCarrierCause(payload, profile, kind, sourceSide) &&
          (kind == RelativeCauseKind.TargetPressureGain || kind == RelativeCauseKind.PawnWeaknessTarget)
      then RelativeCauseSignalProfile.currentMoveConcreteTargetCarrierRecords(profile.fact.candidateLine, profile.allRecords)
      else Nil
    val sameRootPlanCarriers =
      if sourceSide == RelativeCauseSourceSide.Candidate &&
          profile.exactReferenceMove &&
          kind == RelativeCauseKind.PlanImprovement &&
          payload.planComparison.exists(_.hasPlanDelta)
      then RelativeCauseSignalProfile.currentMoveConcretePlanCarrierRecords(profile.fact.candidateLine, profile.allRecords)
      else Nil
    (record :: sameRootBreakCarriers ++ sameRootActivityCarriers ++ sameRootTargetCarriers ++ sameRootPlanCarriers).distinctBy(_.ref.id)

  private def exactSameRootConcreteCarrierCause(
      payload: StrategicMechanismContrastEvidence,
      profile: RelativeCauseSignalProfile,
      kind: RelativeCauseKind,
      sourceSide: RelativeCauseSourceSide
  ): Boolean =
    sourceSide == RelativeCauseSourceSide.Candidate &&
      profile.exactReferenceMove &&
      payload.axisComparisons.exists(comparison =>
        comparison.outcome == StrategicAxisComparisonOutcome.SharedSustained &&
          RelativeCauseSignalProfile
            .exactSameRootConcreteCarrierCauseKindsForAxis(profile.fact.candidateLine, profile.allRecords, comparison.axis)
            .contains(kind)
      )

  private def currentMoveStrategicSupportDrafts(profile: RelativeCauseSignalProfile): List[RelativeCauseDraft] =
    val candidateCurrentMoveCanOwnValue =
      profile.playedCandidateSideComparison &&
        profile.candidateBetter
    if !profile.exactReferenceMove && !candidateCurrentMoveCanOwnValue then Nil
    else
      profile.candidateCurrentMoveStrategicSupport.flatMap {
        case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
          val causeKinds =
            currentMoveStrategicSupportCauseKinds(payload, profile.fact.candidateLine, profile.allRecords)
              .filter(kind =>
                if kind == RelativeCauseKind.OpponentRestriction then
                  currentMoveConcreteCounterplayCanOwnValue(kind, payload, profile)
                else if kind == RelativeCauseKind.TargetPressureGain || kind == RelativeCauseKind.PawnWeaknessTarget then
                  profile.exactReferenceMove &&
                    RelativeCauseSignalProfile.currentMoveConcreteTargetCarrierRecords(profile.fact.candidateLine, profile.allRecords).nonEmpty
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
            RelativeCauseDraft(
              kind,
              List(record),
              Some(RelativeCauseSourceSide.Candidate),
              defaultAttributionKind(kind, Some(RelativeCauseSourceSide.Candidate))
            )
          )
        case _ =>
          Nil
      }.distinctBy(draft => (draft.kind, draft.support.map(_.ref.id).sorted.mkString("|")))

  private def currentMoveConcreteActivityCanOwnValue(
      kind: RelativeCauseKind,
      payload: StrategicMechanismEvidence,
      profile: RelativeCauseSignalProfile
  ): Boolean =
    kind == RelativeCauseKind.ActivityGain &&
      currentMoveConcreteActivitySignals(payload, profile.fact.candidateLine)
        .exists(signal => RelativeCauseSignalProfile.currentMoveConcreteActivitySource(signal.source, profile.allRecords))

  private def currentMoveConcreteActivitySignals(
      payload: StrategicMechanismEvidence,
      candidateLine: LineNodeRef
  ): List[StrategicMechanismSignal] =
    payload.signals.filter(signal =>
      RelativeCauseSignalProfile.currentMoveStrategicSupportSignal(signal, candidateLine) &&
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
            axis.polarity == StrategicAxisPolarity.Restrain &&
            (axis.label == "opponent-diagonal-restriction" || RelativeCauseSignalProfile.currentMoveCounterBreakAxis(axis))
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
        val sameRootCounterBreak =
          signal.axis.exists(RelativeCauseSignalProfile.currentMoveCounterBreakAxis) &&
            RelativeCauseSignalProfile.currentMoveCounterplayRestraintCarrier(candidateLine, records)
        diagonalRestriction || sameRootCounterBreak
      case _ =>
        signal.axis.exists(RelativeCauseSignalProfile.currentMoveCounterBreakAxis) &&
          RelativeCauseSignalProfile.currentMoveCounterplayRestraintCarrier(candidateLine, records)
    }

  private def currentMoveStrategicSupportCauseKinds(
      payload: StrategicMechanismEvidence,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[RelativeCauseKind] =
    RelativeCauseSignalProfile.currentMoveStrategicSupportSignals(payload, candidateLine, records)
      .flatMap(signal => signal.axis.toList.flatMap(RelativeCauseSignalProfile.currentMoveStrategicSupportCauseKindsForAxis))
      .distinct

  private def strategicContrastCauseKinds(
      payload: StrategicMechanismContrastEvidence,
      profile: RelativeCauseSignalProfile
  ): List[(RelativeCauseKind, RelativeCauseSourceSide)] =
    val structuralLoss = profile.badLoss || profile.actionablePlayedLoss
    val planComparisonHasDelta =
      payload.planComparison.exists(_.hasPlanDelta)
    val axisCauses =
      payload.actionableComparisons.flatMap(axisComparison =>
        val axis = axisComparison.axis
        if planComparisonHasDelta && axis.kind == StrategicAxisKind.PlanCoherence then
          Nil
        else if axisComparison.referenceLead && structuralLoss && referenceLeadCanExplainStructuralLoss(axis) then
          List(strategicReferenceLeadCause(axis) -> RelativeCauseSourceSide.Reference)
        else if axisComparison.candidateNegative && structuralLoss then
          List(strategicCandidateNegativeCause(axis) -> RelativeCauseSourceSide.Candidate)
        else if axisComparison.candidateLead && (profile.primaryPlayedPositive || profile.candidateBetter) &&
            candidateLeadCanExplainCandidateValue(axis) then
          List(strategicCandidatePositiveCause(axis) -> RelativeCauseSourceSide.Candidate)
        else Nil
      )
    val exactSameRootConcreteCauses =
      if profile.exactReferenceMove then
        payload.axisComparisons.flatMap {
          case comparison if comparison.outcome == StrategicAxisComparisonOutcome.SharedSustained =>
            RelativeCauseSignalProfile
              .exactSameRootConcreteCarrierCauseKindsForAxis(profile.fact.candidateLine, profile.allRecords, comparison.axis)
              .map(_ -> RelativeCauseSourceSide.Candidate)
          case _ =>
            Nil
        }
      else Nil
    val specificAxisCauses =
      (axisCauses ++ exactSameRootConcreteCauses).filter((kind, _) => specificStrategicAxisCause(kind))
    val narrowedAxisCauses =
      if specificAxisCauses.nonEmpty then (axisCauses ++ exactSameRootConcreteCauses).filterNot((kind, _) => broadStrategicAxisFallback(kind))
      else axisCauses ++ exactSameRootConcreteCauses
    val currentMovePlanCarriers =
      RelativeCauseSignalProfile.currentMoveConcretePlanCarrierRecords(profile.fact.candidateLine, profile.allRecords)
    val exactSameRootPlanCarrier =
      profile.exactReferenceMove && currentMovePlanCarriers.nonEmpty
    val planCauses =
      if specificAxisCauses.nonEmpty && currentMovePlanCarriers.isEmpty then Nil
      else payload.planComparison.toList.filter(_.hasPlanDelta).flatMap { plan =>
        if structuralLoss && plan.outcome != StrategicAxisComparisonOutcome.CandidateOnly then
          List(RelativeCauseKind.PlanContradiction -> RelativeCauseSourceSide.Candidate)
        else if (profile.candidateBetter || exactSameRootPlanCarrier) &&
            (plan.outcome != StrategicAxisComparisonOutcome.ReferenceOnly || exactSameRootPlanCarrier)
        then
          List(RelativeCauseKind.PlanImprovement -> RelativeCauseSourceSide.Candidate)
        else Nil
      }
    (narrowedAxisCauses ++ planCauses).distinct

  private def strategicReferenceLeadCause(axis: StrategicAxisDetail): RelativeCauseKind =
    axis.kind match
      case StrategicAxisKind.Target if weakTargetAxis(axis) =>
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
      case StrategicAxisKind.Counterplay if axis.label.contains("king-safety") =>
        RelativeCauseKind.KingSafetyConcession
      case StrategicAxisKind.PlanCoherence =>
        RelativeCauseKind.PlanContradiction
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

  private def strategicCandidateNegativeCause(axis: StrategicAxisDetail): RelativeCauseKind =
    axis.kind match
      case StrategicAxisKind.Target if axis.polarity == StrategicAxisPolarity.Release =>
        RelativeCauseKind.TargetPressureRelease
      case StrategicAxisKind.Target if weakTargetAxis(axis) =>
        RelativeCauseKind.PawnWeaknessTarget
      case StrategicAxisKind.Target =>
        RelativeCauseKind.TargetPressureGain
      case StrategicAxisKind.SpaceCenter =>
        RelativeCauseKind.CenterControlGain
      case StrategicAxisKind.PawnBreak =>
        RelativeCauseKind.PawnBreakOpportunity
      case StrategicAxisKind.Activity =>
        RelativeCauseKind.ActivityLoss
      case StrategicAxisKind.Counterplay if axis.polarity == StrategicAxisPolarity.Restrain =>
        RelativeCauseKind.OpponentRestriction
      case StrategicAxisKind.Counterplay if axis.label.contains("king-safety") =>
        RelativeCauseKind.KingSafetyConcession
      case StrategicAxisKind.PlanCoherence =>
        RelativeCauseKind.PlanContradiction
      case _ =>
        RelativeCauseKind.StrategicConcession

  private def strategicCandidatePositiveCause(axis: StrategicAxisDetail): RelativeCauseKind =
    axis.kind match
      case StrategicAxisKind.Target if weakTargetAxis(axis) =>
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

  private def specificStrategicAxisCause(kind: RelativeCauseKind): Boolean =
    kind match
      case RelativeCauseKind.TargetPressureGain | RelativeCauseKind.TargetPressureRelease |
          RelativeCauseKind.CenterControlGain |
          RelativeCauseKind.PawnWeaknessTarget | RelativeCauseKind.PawnBreakOpportunity |
          RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss | RelativeCauseKind.OpponentRestriction |
          RelativeCauseKind.KingSafetyConcession =>
        true
      case _ =>
        false

  private def weakTargetAxis(axis: StrategicAxisDetail): Boolean =
    axis.label.contains("weak-pawn") || axis.label.contains("weak-square")

  private def broadStrategicAxisFallback(kind: RelativeCauseKind): Boolean =
    kind match
      case RelativeCauseKind.PlanContradiction | RelativeCauseKind.PlanImprovement |
          RelativeCauseKind.MissedStrategicImprovement | RelativeCauseKind.StrategicConcession |
          RelativeCauseKind.StructuralImprovement =>
        true
      case _ =>
        false

  private def suppressGenericCompanions(
      drafts: List[RelativeCauseDraft]
  ): List[RelativeCauseDraft] =
    val hasShortTermCause = drafts.exists(draft => shortTermCause(draft.kind))
    val hasNonConcessionCause = drafts.exists(draft => draft.kind != RelativeCauseKind.StrategicConcession)
    val hasSpecificMaterialCause = drafts.exists(draft => specificMaterialCause(draft.kind))
    val hasNonPlanSpecificStructuralCause =
      drafts.exists(draft => specificStructuralCause(draft.kind) && !planFallbackCause(draft.kind))
    drafts.filterNot {
      case RelativeCauseDraft(RelativeCauseKind.StrategicConcession, _, _, _) =>
        hasShortTermCause || hasNonConcessionCause
      case RelativeCauseDraft(RelativeCauseKind.MissedStrategicImprovement, _, _, _) =>
        hasShortTermCause
      case RelativeCauseDraft(RelativeCauseKind.MaterialSwing, _, _, _) =>
        hasSpecificMaterialCause
      case RelativeCauseDraft(RelativeCauseKind.StructuralImprovement, _, _, _) =>
        hasNonPlanSpecificStructuralCause
      case draft @ RelativeCauseDraft(RelativeCauseKind.RecaptureRecoveryWindow, _, _, _) =>
        drafts.exists(other =>
          other.kind == RelativeCauseKind.WrongRecapturer &&
            supportOverlaps(draft.support, other.support)
        )
      case draft @ RelativeCauseDraft(RelativeCauseKind.TempoLoss, _, _, _) =>
        drafts.exists(other =>
          other.kind == RelativeCauseKind.WrongMoveOrder &&
            supportOverlaps(draft.support, other.support)
        )
      case _ =>
        false
    }

  private def supportOverlaps(left: List[EvidenceRecord], right: List[EvidenceRecord]): Boolean =
    val leftIds = left.map(_.ref.id).toSet
    leftIds.nonEmpty && right.exists(record => leftIds.contains(record.ref.id))

  private def defaultAttributionKind(
      kind: RelativeCauseKind,
      sourceSide: Option[RelativeCauseSourceSide]
  ): CauseAttributionKind =
    sourceSide match
      case Some(RelativeCauseSourceSide.Reference) =>
        CauseAttributionKind.ReferenceCreatesResource
      case Some(RelativeCauseSourceSide.Candidate) =>
        kind match
          case RelativeCauseKind.RecaptureRecoveryWindow | RelativeCauseKind.ConversionSecured |
              RelativeCauseKind.SacrificeCompensation | RelativeCauseKind.StructuralImprovement |
              RelativeCauseKind.TargetPressureGain | RelativeCauseKind.CenterControlGain |
              RelativeCauseKind.PawnWeaknessTarget | RelativeCauseKind.PawnBreakOpportunity |
              RelativeCauseKind.ActivityGain |
              RelativeCauseKind.OpponentRestriction |
              RelativeCauseKind.PlanImprovement |
              RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
            CauseAttributionKind.CandidateCreatesValue
          case _ =>
            CauseAttributionKind.CandidateAllowsLiability
      case Some(RelativeCauseSourceSide.Shared) =>
        CauseAttributionKind.SharedContext
      case Some(RelativeCauseSourceSide.Mixed) | None =>
        kind match
          case RelativeCauseKind.OnlyMoveNecessity | RelativeCauseKind.OnlyDefenseNecessity |
              RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.MissedStrategicImprovement |
              RelativeCauseKind.ConversionMiss | RelativeCauseKind.KingForcing =>
            CauseAttributionKind.ReferenceCreatesResource
          case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.WrongMoveOrder |
              RelativeCauseKind.TempoLoss | RelativeCauseKind.TacticalRefutationOfPlayed |
              RelativeCauseKind.CandidateTacticalLiability | RelativeCauseKind.MaterialSwing |
              RelativeCauseKind.PlanContradiction | RelativeCauseKind.TargetPressureRelease |
              RelativeCauseKind.ActivityLoss | RelativeCauseKind.OpponentRestriction |
              RelativeCauseKind.StrategicConcession | RelativeCauseKind.KingSafetyConcession =>
            CauseAttributionKind.CandidateAllowsLiability
          case _ =>
            CauseAttributionKind.Unattributed

  private def specificMaterialCause(kind: RelativeCauseKind): Boolean =
    kind match
      case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow |
          RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured |
          RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability =>
        true
      case _ =>
        false

  private def specificStructuralCause(kind: RelativeCauseKind): Boolean =
    kind match
      case RelativeCauseKind.TargetPressureGain | RelativeCauseKind.TargetPressureRelease |
          RelativeCauseKind.CenterControlGain |
          RelativeCauseKind.KingSafetyConcession | RelativeCauseKind.PawnWeaknessTarget |
          RelativeCauseKind.PawnBreakOpportunity | RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss |
          RelativeCauseKind.OpponentRestriction |
          RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction |
          RelativeCauseKind.MissedStrategicImprovement | RelativeCauseKind.StrategicConcession |
          RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        true
      case _ =>
        false

  private def planFallbackCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.PlanImprovement || kind == RelativeCauseKind.PlanContradiction

  private def shortTermCause(kind: RelativeCauseKind): Boolean =
    kind != RelativeCauseKind.DrawResource && ClaimEventCluster.kindForCause(kind).nonEmpty

private[chessjudgment] object RelativeCauseSignalProfile:
  def from(
      fact: CandidateComparisonFact,
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord]
  ): RelativeCauseSignalProfile =
    RelativeCauseSignalProfile(
      fact = fact,
      referenceRecords = referenceRecords,
      candidateRecords = candidateRecords,
      sharedRecords = sharedRecords
    )

  private[chessjudgment] def tacticalMechanismRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorTacticalIdea || payload.canAnchorDefensiveIdea
      case _ =>
        false
    }

  private[chessjudgment] def strategicContrastRecords(
      fact: CandidateComparisonFact,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _)
          if StrategicMechanismContrastEvidence.hasActionableContrastOrSameRootCarrier(fact, payload, records) &&
            payload.comparisonKind == fact.kind &&
            payload.referenceLine == fact.referenceLine &&
            payload.candidateLine == fact.candidateLine =>
        record
    }.distinctBy(_.ref.id)

  private[chessjudgment] def tacticalRiskRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorTacticalIdea
      case _ =>
        false
    }

  private[chessjudgment] def onlyDefenseRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        payload.onlyDefense.nonEmpty && payload.isProofSignalDefensivePressure
      case _ => false
    }

  private[chessjudgment] def defensiveResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        !payload.insufficientData &&
          (payload.defenseRequired ||
            payload.prophylaxisNeeded ||
            payload.maxWinPercentLossIfIgnored.exists(_ >= JudgmentThresholds.SIGNIFICANT_THREAT_WP))
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorDefensiveIdea
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def recaptureResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasRecaptureRecoveryConsequence ||
          payload.hasMaterialRecaptureChain ||
          payload.hasMaterialRecoveryWindow
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.RecaptureChoice && payload.canAnchorTacticalIdea
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
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasProofSignalConsequence(LineConsequenceKind.ImmediateReplyCheck)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.Tempo && payload.canAnchorTacticalIdea
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def forcingLineResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasProofSignalConsequence(LineConsequenceKind.ForcedTheme) ||
          payload.hasLineEvent(LineEventKind.Mate) ||
          payload.hasProofSignalConsequence(LineConsequenceKind.Mate)
      case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) =>
        mate.nonEmpty
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.KingForcing && payload.hasConcreteProof
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def wrongRecapturerChoiceRecords(
      fact: CandidateComparisonFact,
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    val referenceMove = normalizeMove(fact.referenceLine.rootMove)
    val candidateMove = normalizeMove(fact.candidateLine.rootMove)
    val referenceRecovery = recaptureResourceRecords(referenceRecords)
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
          recaptureResourceRecords(candidateRecords)
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
        payload.maintainedDefensiveEndgameTechniqueHorizons.nonEmpty
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def promotionRaceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasProofSignalConsequence(LineConsequenceKind.PromotionRace) ||
          payload.materialOutcomeProfile.gainSignals.contains(LineMaterialOutcomeSignal.PromotionGain) ||
          payload.materialOutcomeProfile.lossSignals.contains(LineMaterialOutcomeSignal.PromotionLoss)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.PawnPromotion && payload.canAnchorTacticalIdea
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
      records: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    val material = materialLossRecords(records)
    val relation = looseMaterialRelationRecords(records)
    Option
      .when((material.nonEmpty || relation.nonEmpty) && looseMaterialContextPresent(records ++ sharedRecords))(
        material ++ relation
      )
      .getOrElse(Nil)
      .distinctBy(_.ref.id)

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

  private def materialGainRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.materialOutcomeProfile.gainMagnitude != LineMaterialOutcomeMagnitude.None ||
          payload.hasProofSignalConsequence(LineConsequenceKind.MaterialGain)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.MaterialGain && payload.canAnchorTacticalIdea
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private def materialLossRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.materialOutcomeProfile.lossMagnitude != LineMaterialOutcomeMagnitude.None ||
          payload.hasProofSignalConsequence(LineConsequenceKind.MaterialLoss)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.MaterialGain && payload.canAnchorTacticalIdea
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def passedPawnResourceRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.hasPassedPawnResourceSignal
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasProofSignalConsequence(LineConsequenceKind.PromotionRace) ||
          payload.materialOutcomeProfile.gainSignals.contains(LineMaterialOutcomeSignal.PromotionGain) ||
          payload.materialOutcomeProfile.lossSignals.contains(LineMaterialOutcomeSignal.PromotionLoss)
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.kind == TacticalMechanismKind.PawnPromotion && payload.canAnchorTacticalIdea
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def passedPawnConcessionRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.hasPassedPawnConcessionSignal
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
      currentMoveStrategicSupportSignal(signal, candidateLine) ||
        currentMoveCounterBreakSupportSignal(signal, candidateLine, records)
    )

  private[chessjudgment] def currentMoveStrategicSupportSignal(
      signal: StrategicMechanismSignal,
      candidateLine: LineNodeRef
  ): Boolean =
    signal.kind == StrategicMechanismSignalKind.StructuralDelta &&
      signal.source.scope == EvidenceScope.PlayedTransition &&
      signal.source.line.contains(candidateLine) &&
      signal.axis.exists(axis => currentMoveStrategicSupportCauseKindsForAxis(axis).nonEmpty)

  private[chessjudgment] def currentMoveStrategicSupportCauseKindsForAxis(axis: StrategicAxisDetail): List[RelativeCauseKind] =
    axis.kind match
      case StrategicAxisKind.Target if axis.polarity == StrategicAxisPolarity.Gain =>
        if axis.label.contains("weak-pawn") || axis.label.contains("weak-square") then List(RelativeCauseKind.PawnWeaknessTarget)
        else List(RelativeCauseKind.TargetPressureGain)
      case StrategicAxisKind.Activity if currentMoveActivityValueAxis(axis) =>
        List(RelativeCauseKind.ActivityGain)
      case StrategicAxisKind.PlanCoherence if StrategicMechanismContrastEvidence.currentMovePlanCoherenceAxis(axis) =>
        List(RelativeCauseKind.PlanImprovement)
      case StrategicAxisKind.Counterplay
          if axis.polarity == StrategicAxisPolarity.Restrain &&
            (axis.label == "opponent-diagonal-restriction" || currentMoveCounterBreakAxis(axis)) =>
        List(RelativeCauseKind.OpponentRestriction)
      case StrategicAxisKind.PawnBreak
          if currentMovePawnBreakAxis(axis) &&
            (
              axis.polarity == StrategicAxisPolarity.Release ||
                (
                  !currentMovePawnBreakResolutionAxis(axis) &&
                    (
                      axis.polarity == StrategicAxisPolarity.Support ||
                        axis.polarity == StrategicAxisPolarity.Preserve
                    )
                )
            ) =>
        List(RelativeCauseKind.PawnBreakOpportunity)
      case _ =>
        Nil

  private[chessjudgment] def currentMoveActivityValueAxis(axis: StrategicAxisDetail): Boolean =
    StrategicMechanismContrastEvidence.currentMoveActivityValueAxis(axis)

  private[chessjudgment] def exactSameRootConcreteCarrierCauseKindsForAxis(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord],
      axis: StrategicAxisDetail
  ): List[RelativeCauseKind] =
    StrategicMechanismContrastEvidence.exactSameRootConcreteCarrierCauseKindsForAxis(candidateLine, records, axis)

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

  private def currentMoveCounterBreakSupportSignal(
      signal: StrategicMechanismSignal,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    signal.axis.exists(currentMoveCounterBreakAxis) &&
      currentMoveCounterplayRestraintCarrier(candidateLine, records)

  private[chessjudgment] def currentMoveCounterBreakAxis(axis: StrategicAxisDetail): Boolean =
    StrategicMechanismContrastEvidence.currentMoveCounterBreakAxis(axis)

  private[chessjudgment] def currentMoveSameRootBreakCarrier(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    StrategicMechanismContrastEvidence.currentMoveSameRootBreakCarrier(candidateLine, records)

  private[chessjudgment] def currentMoveCounterplayRestraintCarrier(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    StrategicMechanismContrastEvidence.currentMoveCounterplayRestraintCarrier(candidateLine, records)

  private[chessjudgment] def currentMoveSameRootBreakCarrierRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    StrategicMechanismContrastEvidence.currentMoveSameRootBreakCarrierRecords(candidateLine, records)

  private def currentMovePawnBreakAxis(axis: StrategicAxisDetail): Boolean =
    val normalized = axis.label.toLowerCase
    normalized.contains("created-tension") ||
      normalized.contains("resolved-tension") ||
      normalized.contains("tension")

  private def currentMovePawnBreakResolutionAxis(axis: StrategicAxisDetail): Boolean =
    val normalized = axis.label.toLowerCase
    normalized.contains("resolved-tension") ||
      normalized.contains("-release-")

  private[chessjudgment] def referenceOnlyDefenseFunctionRecords(
      fact: CandidateComparisonFact,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
        !payload.insufficientData &&
          payload.onlyDefense.exists(move => normalizeMove(move) == normalizeMove(fact.referenceLine.rootMove)) &&
          (payload.defenseRequired ||
            payload.maxWinPercentLossIfIgnored.exists(_ >= JudgmentThresholds.MATERIAL_THREAT_WP))
      case _ =>
        false
    }

  private[chessjudgment] def candidateConcreteTacticalBridgeRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.canAnchorTacticalIdea
      case _ =>
        false
    }.distinctBy(_.ref.id)

  private[chessjudgment] def materialSwingSupportRecords(
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    Option
      .when(typedMaterialConsequenceSwing(referenceRecords, candidateRecords))(
        (proofSignalMaterialSummaryRecords(referenceRecords) ++ proofSignalMaterialSummaryRecords(candidateRecords)).distinctBy(_.ref.id)
      )
      .getOrElse(Nil)

  private[chessjudgment] def materialDeteriorationSupportRecords(
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    Option
      .when(materialDeteriorates(referenceRecords, candidateRecords))(
        proofSignalMaterialSummaryRecords(referenceRecords) ++ proofSignalMaterialSummaryRecords(candidateRecords)
      )
      .getOrElse(Nil)
      .distinctBy(_.ref.id)

  private[chessjudgment] def sacrificeCompensationSupportRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    val material = sacrificeMaterialRecords(records)
    val compensation = compensationSupportRecords(records)
    Option
      .when(material.nonEmpty && compensation.nonEmpty)((material ++ compensation).distinctBy(_.ref.id))
      .getOrElse(Nil)

  private def sacrificeMaterialRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasSacrificeConsequence
      case _ =>
        false
    }

  private def compensationSupportRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    (
      strategicCompensationRecords(records) ++
        compensationAnchorRecords(records)
    ).distinctBy(_.ref.id)

  private def strategicCompensationRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    strategicMechanismRecords(records)(_.canSupportCompensation)

  private def compensationAnchorRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    strategicMechanismRecords(records)(payload =>
      payload.canSupportCompensation || payload.canAnchorPlanIdea || payload.canAnchorPawnStructureIdea
    )

  private def proofSignalMaterialSummaryRecords(records: List[EvidenceRecord]): List[EvidenceRecord] =
    records.filter {
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.hasMaterialConsequence || payload.hasRecaptureRecoveryConsequence
      case _ =>
        false
    }

  private def typedMaterialConsequenceSwing(
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): Boolean =
    materialGainMagnitude(referenceRecords).ordinal > materialGainMagnitude(candidateRecords).ordinal ||
      materialLossMagnitude(candidateRecords).ordinal > materialLossMagnitude(referenceRecords).ordinal

  private def materialDeteriorates(
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  ): Boolean =
    typedMaterialConsequenceSwing(referenceRecords, candidateRecords)

  private def materialGainMagnitude(records: List[EvidenceRecord]): LineMaterialOutcomeMagnitude =
    LineFactEvidence.materialOutcomeProfile(records).gainMagnitude

  private def materialLossMagnitude(records: List[EvidenceRecord]): LineMaterialOutcomeMagnitude =
    LineFactEvidence.materialOutcomeProfile(records).lossMagnitude

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase
