package lila.chessjudgment.analysis.qc

import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*
import MoveReviewPhase3AuditContract.ExpectedSemanticSlot

private[qc] object MoveReviewPhase3AuditMetrics:
  def axislessStructuralAnchorInventoryJson(
      graph: TypedEvidenceGraph,
      lineRefSummary: LineNodeRef => JsObject
  ): JsObject =
    val entries =
      graph.records.collect { case record @ EvidenceRecord(ref, _: StructuralDeltaEvidence, _) =>
        val axislessSignals =
          StrategicMechanismEvidence
            .sourceMechanisms(record)
            .collect { case (mechanism, signal) if signal.axis.isEmpty =>
              (mechanism, signal)
            }
            .filter((_, signal) => axislessStructuralInventorySignal(signal))
        ref -> axislessSignals
      }.filter(_._2.nonEmpty)
    val signals = entries.flatMap(_._2)
    Json.obj(
      "classification" -> "audit_only",
      "axislessRecordCount" -> entries.size,
      "axislessRecordIds" -> entries.map(_._1.id).distinct.sorted,
      "axislessSignalCount" -> signals.size,
      "axislessSignalLabelCounts" -> stringCountsJson(signals.map(_._2.label)),
      "axislessMechanismKindCounts" -> stringCountsJson(signals.map(_._1.toString)),
      "records" -> JsArray(
        entries.sortBy(_._1.id).map { case (ref, recordSignals) =>
          Json.obj(
            "recordId" -> ref.id,
            "line" -> ref.line.map(lineRefSummary),
            "signalCount" -> recordSignals.size,
            "signalLabels" -> recordSignals.map(_._2.label).distinct.sorted,
            "mechanismKinds" -> recordSignals.map(_._1.toString).distinct.sorted
          )
        }
      )
    )

  private def multiEvidenceInputPublicClaim(diagnostic: PublicMoveMeaningClaimDiagnostic): Boolean =
    diagnostic.causeEvidenceIds.size > 1 ||
      diagnostic.sourceEvidenceIds.size > 1

  private[qc] def moveMeaningSurfaceDiagnosticsJson(
      diagnostics: List[CandidateComparisonDiagnostic]
  ): JsObject =
    val publicClaimDiagnostics =
      diagnostics.flatMap(_.moveJudgmentView.publicMoveMeaningClaimDiagnostics).distinct
    val multiInputPublicClaimCount =
      publicClaimDiagnostics.count(multiEvidenceInputPublicClaim)
    val boardCarrierlessPublicClaimCount =
      publicClaimDiagnostics.count(diagnostic => !diagnostic.hasBoardCarrier)
    Json.obj(
      "publicMoveMeaningClaimCount" -> publicClaimDiagnostics.size,
      "boardCarrierlessPublicMoveMeaningClaimCount" -> boardCarrierlessPublicClaimCount,
      "multiEvidenceInputPublicClaimCount" -> multiInputPublicClaimCount
    )

  private[qc] def moveMeaningSurfaceCorpusDiagnosticsJson(coverages: List[JsValue]): JsObject =
    val publicClaimCount =
      coverages.map(coverage => (coverage \ "publicMoveMeaningClaimCount").asOpt[Int].getOrElse(0)).sum
    val boardCarrierlessPublicClaimCount =
      coverages.map(coverage => (coverage \ "boardCarrierlessPublicMoveMeaningClaimCount").asOpt[Int].getOrElse(0)).sum
    val multiInputPublicClaimCount =
      coverages.map(coverage => (coverage \ "multiEvidenceInputPublicClaimCount").asOpt[Int].getOrElse(0)).sum
    Json.obj(
      "publicMoveMeaningClaimCount" -> publicClaimCount,
      "boardCarrierlessPublicMoveMeaningClaimCount" -> boardCarrierlessPublicClaimCount,
      "multiEvidenceInputPublicClaimCount" -> multiInputPublicClaimCount
    )

  private[qc] def axislessStructuralInventorySignal(signal: StrategicMechanismSignal): Boolean =
    signal.label != "structural-improvement" &&
      signal.label != "strategic-concession" &&
      signal.label != "pawn-structure-delta"

  private[qc] def stringCountsJson(values: List[String]): JsObject =
    JsObject(
      values
        .filter(_.nonEmpty)
        .groupBy(identity)
        .view
        .mapValues(_.size)
        .toList
        .sortBy { case (value, _) => value }
        .map { case (value, count) => value -> JsNumber(count) }
    )
private[qc] final class MoveReviewPhase3AuditFunnelMetrics(lineRefSummary: LineNodeRef => JsObject):
  private def axislessStructuralAnchorInventoryJson(graph: TypedEvidenceGraph): JsObject =
    MoveReviewPhase3AuditMetrics.axislessStructuralAnchorInventoryJson(graph, lineRefSummary)

  private def badVerdict(verdict: MoveChoiceVerdict): Boolean =
    verdict match
      case MoveChoiceVerdict.Inaccuracy | MoveChoiceVerdict.Mistake | MoveChoiceVerdict.Blunder => true
      case _                                                                                   => false

  private def noEventCauseComparison(diagnostic: CandidateComparisonDiagnostic): Boolean =
    !diagnostic.decisionTrace.tacticalLoss &&
      !diagnostic.decisionTrace.materialSwingEvidence &&
      diagnostic.causeKinds.forall(kind =>
        !ClaimEventCluster.kindForCause(kind).exists(kind =>
          kind == ClaimEventClusterKind.TacticalEvent ||
            kind == ClaimEventClusterKind.DefensiveEvent ||
            kind == ClaimEventClusterKind.ConversionEvent ||
            kind == ClaimEventClusterKind.MaterialEvent
        )
      )
  def relativeCauseStageCountsJson(diagnostics: List[CandidateComparisonDiagnostic]): JsObject =
    val entries = diagnosticFlows(diagnostics)
    Json.obj(
      "all" -> stageCountsJson(entries),
      "event" -> stageCountsJson(entries.filterNot((_, flow) => noEventCauseFlow(flow))),
      "structuralNoEvent" -> stageCountsJson(entries.filter((_, flow) => noEventCauseFlow(flow))),
      "primaryPlayed" -> stageCountsJson(entries.filter((_, flow) =>
        flow.causeComparisonKind == CandidateComparisonKind.PlayedVsBest &&
          flow.causeRole == RelativeCauseRole.PrimaryPlayedCause
      )),
      "contextAlternative" -> stageCountsJson(entries.filter((_, flow) =>
        flow.causeRole == RelativeCauseRole.PlayedAlternativeContext ||
          flow.causeRole == RelativeCauseRole.AlternativeDiagnostic ||
          flow.causeComparisonKind != CandidateComparisonKind.PlayedVsBest
      ))
    )

  private[qc] def planTechniqueEligibilityFunnelJson(
      diagnostics: List[CandidateComparisonDiagnostic]
  ): JsObject =
    val classifications = diagnostics.map(diagnostic => diagnostic -> planTechniqueEligibilityClassification(diagnostic))
    def comparisonIds(classification: String): List[String] =
      classifications
        .collect { case (diagnostic, `classification`) => diagnostic.id }
        .distinct
        .sorted
    val structuralAxisPresentIds = comparisonIds("structural_axis_present")
    val playableLossNoAxisIds = comparisonIds("playable_loss_no_axis")
    val tacticalOnlyNoAxisIds = comparisonIds("tactical_only_no_axis")
    val structuralAxisMissingCandidateIds = comparisonIds("structural_axis_missing_candidate")
    val tacticalOnlyDiagnostics = classifications.collect {
      case (diagnostic, "tactical_only_no_axis") => diagnostic
    }
    Json.obj(
      "classification" -> "audit_only",
      "comparisonCount" -> diagnostics.size,
      "classificationCounts" -> stringCountsJson(classifications.map(_._2)),
      "structuralAxisPresentCount" -> structuralAxisPresentIds.size,
      "structuralAxisPresentComparisonIds" -> structuralAxisPresentIds,
      "playableLossNoAxisCount" -> playableLossNoAxisIds.size,
      "playableLossNoAxisComparisonIds" -> playableLossNoAxisIds,
      "tacticalOnlyNoAxisCount" -> tacticalOnlyNoAxisIds.size,
      "tacticalOnlyNoAxisComparisonIds" -> tacticalOnlyNoAxisIds,
      "tacticalOnlyNoAxisRelationKindCounts" -> stringCountsJson(
        tacticalOnlyDiagnostics.flatMap(_.decisionTrace.relationKinds.map(_.toString))
      ),
      "tacticalOnlyNoAxisCauseKindCounts" -> stringCountsJson(
        tacticalOnlyDiagnostics.flatMap(_.causeKinds.map(_.toString))
      ),
      "structuralAxisMissingCandidateCount" -> structuralAxisMissingCandidateIds.size,
      "structuralAxisMissingCandidateComparisonIds" -> structuralAxisMissingCandidateIds,
      "structuralAxisMissingCandidateVerdictCounts" -> stringCountsJson(
        classifications.collect {
          case (diagnostic, "structural_axis_missing_candidate") => diagnostic.verdict.toString
        }
      )
    )

  private[qc] def planTechniqueAnchorEligibilityJson(
      diagnostics: List[CandidateComparisonDiagnostic],
      graph: TypedEvidenceGraph
  ): JsObject =
    val eligibility = planTechniqueEligibilityFunnelJson(diagnostics)
    val inventory = axislessStructuralAnchorInventoryJson(graph)
    val axislessRecordCount = (inventory \ "axislessRecordCount").asOpt[Int].getOrElse(0)
    val axislessSignalCount = (inventory \ "axislessSignalCount").asOpt[Int].getOrElse(0)
    val axislessLabelCounts =
      jsonStringIntCounts((inventory \ "axislessSignalLabelCounts").asOpt[JsObject].getOrElse(Json.obj()))
    val unitCandidates = axislessStructuralAnchorUnitCandidates(graph)
    val objectBoundUnitCandidates = unitCandidates.filter(_.objectSubjects.nonEmpty)
    val broadUnitCandidates = unitCandidates.filter(_.objectSubjects.isEmpty)
    val unmappedLabelCounts =
      axislessLabelCounts.filter((label, _) => axislessStructuralAnchorPlanTechniqueUnitCandidates(label).isEmpty)
    val missingCandidateIds =
      (eligibility \ "structuralAxisMissingCandidateComparisonIds").asOpt[List[String]].getOrElse(Nil).distinct.sorted
    val missingCandidateIdsWithAnyPacketAxislessAnchor =
      if axislessSignalCount > 0 then missingCandidateIds else Nil
    val resolution =
      if missingCandidateIds.nonEmpty && axislessSignalCount > 0 then "upstream_axis_generation_candidate"
      else if missingCandidateIds.nonEmpty then "structural_missing_without_axisless_anchor"
      else if axislessSignalCount > 0 then "inventory_only_no_structural_missing_candidate"
      else "no_axisless_structural_anchor_or_missing_candidate"
    Json.obj(
      "classification" -> "audit_only",
      "bindingScope" -> "packet_level",
      "resolution" -> resolution,
      "axislessStructuralAnchorRecordCount" -> axislessRecordCount,
      "axislessStructuralAnchorSignalCount" -> axislessSignalCount,
      "axislessStructuralAnchorLabelCounts" -> (inventory \ "axislessSignalLabelCounts").asOpt[JsObject].getOrElse(Json.obj()),
      "axislessStructuralAnchorMechanismKindCounts" ->
        (inventory \ "axislessMechanismKindCounts").asOpt[JsObject].getOrElse(Json.obj()),
      "axislessStructuralAnchorPlanTechniqueUnitCandidateCounts" -> planTechniqueUnitCandidateCountsJson(unitCandidates),
      "axislessStructuralAnchorPlanTechniqueUnitCandidateLabels" -> planTechniqueUnitCandidateLabelsJson(unitCandidates),
      "axislessStructuralAnchorPlanTechniqueUnitObjectBoundCandidateCounts" ->
        planTechniqueUnitCandidateCountsJson(objectBoundUnitCandidates),
      "axislessStructuralAnchorPlanTechniqueUnitObjectBoundLabels" ->
        planTechniqueUnitCandidateLabelsJson(objectBoundUnitCandidates),
      "axislessStructuralAnchorPlanTechniqueUnitBroadCandidateCounts" -> planTechniqueUnitCandidateCountsJson(broadUnitCandidates),
      "axislessStructuralAnchorPlanTechniqueUnitBroadLabels" -> planTechniqueUnitCandidateLabelsJson(broadUnitCandidates),
      "axislessStructuralAnchorUnmappedLabelCounts" -> JsObject(
        unmappedLabelCounts.map { case (label, count) => label -> JsNumber(count) }
      ),
      "axislessStructuralAnchorInventory" -> inventory,
      "structuralAxisMissingCandidateCount" -> missingCandidateIds.size,
      "structuralAxisMissingCandidateComparisonIds" -> missingCandidateIds,
      "structuralAxisMissingCandidateWithAnyPacketAxislessAnchorCount" -> missingCandidateIdsWithAnyPacketAxislessAnchor.size,
      "structuralAxisMissingCandidateWithAnyPacketAxislessAnchorComparisonIds" ->
        missingCandidateIdsWithAnyPacketAxislessAnchor,
      "structuralAxisMissingCandidateWithoutAnyPacketAxislessAnchorCount" ->
        (missingCandidateIds.size - missingCandidateIdsWithAnyPacketAxislessAnchor.size),
      "structuralAxisMissingCandidateWithoutAnyPacketAxislessAnchorComparisonIds" ->
        missingCandidateIds.diff(missingCandidateIdsWithAnyPacketAxislessAnchor)
    )

  private def jsonStringIntCounts(json: JsObject): List[(String, Int)] =
    json.value.toList.collect { case (label, JsNumber(count)) => label -> count.toInt }.sortBy(_._1)

  private final case class AxislessStructuralAnchorUnitCandidate(
      unit: PositionPlanTechniqueUnit,
      label: String,
      recordId: String,
      objectSubjects: List[String]
  )

  private def axislessStructuralAnchorUnitCandidates(graph: TypedEvidenceGraph): List[AxislessStructuralAnchorUnitCandidate] =
    graph.records.collect { case record @ EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) =>
      StrategicMechanismEvidence
        .sourceMechanisms(record)
        .collect { case (_, signal) if signal.axis.isEmpty && MoveReviewPhase3AuditMetrics.axislessStructuralInventorySignal(signal) =>
          val objectSubjects = structuralDeltaObjectSubjectsForSignal(signal.label, payload)
          axislessStructuralAnchorPlanTechniqueUnitCandidates(signal.label).map(unit =>
            AxislessStructuralAnchorUnitCandidate(
              unit = unit,
              label = signal.label,
              recordId = ref.id,
              objectSubjects = objectSubjects
            )
          )
        }
        .flatten
    }.flatten

  private def structuralDeltaObjectSubjectsForSignal(label: String, payload: StructuralDeltaEvidence): List[String] =
    import TransitionConsequenceKind.*
    val matchedConsequences =
      label match
        case "activity-gain" =>
          payload.consequences.filter(consequence =>
            consequence.positive &&
              StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, TransitionConsequenceCategory.PieceActivity)
          )
        case "activity-loss" =>
          val activityLossKinds = Set(
            DevelopmentLagIncreased,
            DevelopmentPieceRetreated,
            DevelopmentMobilityLoss,
            DevelopmentCenterControlLoss,
            DevelopmentUnsafePlacement,
            MobilityLoss,
            FileAccessLoss
          )
          payload.consequences.filter(consequence => activityLossKinds.contains(consequence.kind))
        case "center-control-gain" =>
          payload.consequences.filter(_.kind == CenterControlGain)
        case "passed-pawn-progress" =>
          payload.consequences.filter(_.kind == PassedPawnProgress)
        case "promotion-pressure-gain" =>
          payload.consequences.filter(_.kind == PromotionPressureGain)
        case "counterplay-restraint" | "counterplay-restriction" | "counterplay-support" | "counterplay-race" =>
          payload.consequences.filter(consequence =>
            StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, TransitionConsequenceCategory.StrategicSupport)
          )
        case "pawn-break-ready" | "tension-active" | "tension-release" =>
          payload.consequences.filter(consequence =>
            StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, TransitionConsequenceCategory.PawnStructureDelta)
          )
        case "compensation" | "long-diagonal-pressure" =>
          payload.consequences.filter(consequence =>
            StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, TransitionConsequenceCategory.StrategicSupport)
          )
        case _ =>
          Nil
    val developmentSubjects =
      if label == "activity-gain" || label == "activity-loss" then
        payload.developmentChoices.flatMap(choice => List(choice.role, choice.from, choice.to))
      else Nil
    (matchedConsequences.flatMap(_.subjects) ++ developmentSubjects).map(_.trim).filter(_.nonEmpty).distinct.sorted

  private def planTechniqueUnitCandidateCountsJson(candidates: List[AxislessStructuralAnchorUnitCandidate]): JsObject =
    JsObject(
      candidates
        .groupMapReduce(_.unit)(_ => 1)(_ + _)
        .toList
        .sortBy(_._1.toString)
        .map { case (unit, count) => unit.toString -> JsNumber(count) }
    )

  private def planTechniqueUnitCandidateLabelsJson(candidates: List[AxislessStructuralAnchorUnitCandidate]): JsObject =
    JsObject(
      candidates
        .groupMap(_.unit)(_.label)
        .toList
        .sortBy(_._1.toString)
        .map { case (unit, labels) => unit.toString -> JsArray(labels.distinct.sorted.map(JsString.apply)) }
    )

  private def axislessStructuralAnchorPlanTechniqueUnitCandidates(label: String): List[PositionPlanTechniqueUnit] =
    label match
      case "activity-gain" | "activity-loss" =>
        List(PositionPlanTechniqueUnit.PieceRerouteRoute)
      case "center-control-gain" | "passed-pawn-progress" | "promotion-pressure-gain" =>
        List(PositionPlanTechniqueUnit.StructuralTransformation)
      case "counterplay-restraint" | "counterplay-restriction" =>
        List(PositionPlanTechniqueUnit.SpacePreventionResourceDenial)
      case "counterplay-support" | "counterplay-race" =>
        List(PositionPlanTechniqueUnit.CounterplayRace)
      case "pawn-break-ready" | "tension-active" | "tension-release" =>
        List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute)
      case "compensation" | "long-diagonal-pressure" =>
        List(PositionPlanTechniqueUnit.CompensationSource)
      case _ =>
        Nil

  private def planTechniqueEligibilityClassification(diagnostic: CandidateComparisonDiagnostic): String =
    if primaryActionableStructuralOpportunity(diagnostic) then "structural_axis_present"
    else if diagnostic.verdict == MoveChoiceVerdict.PlayableLoss && structuralOpportunityAxisOpportunities(diagnostic).isEmpty then
      "playable_loss_no_axis"
    else if primaryBadNoAxisComparison(diagnostic) && tacticalNoAxisEvidence(diagnostic) then "tactical_only_no_axis"
    else if primaryBadNoAxisComparison(diagnostic) then "structural_axis_missing_candidate"
    else "not_structural_eligibility_target"

  private def primaryBadNoAxisComparison(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.comparisonKind == CandidateComparisonKind.PlayedVsBest &&
      badVerdict(diagnostic.verdict) &&
      diagnostic.subjectBinding != SubjectBindingClass.Other &&
      structuralOpportunityAxisOpportunities(diagnostic).isEmpty

  private def tacticalNoAxisEvidence(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.decisionTrace.tacticalLoss ||
      diagnostic.tacticalLossTrace.applicable ||
      diagnostic.decisionTrace.relationKinds.nonEmpty ||
      diagnostic.decisionTrace.referenceRelationKinds.nonEmpty ||
      diagnostic.decisionTrace.candidateRelationKinds.nonEmpty ||
      diagnostic.decisionTrace.referenceTacticalMechanismKinds.nonEmpty ||
      diagnostic.decisionTrace.candidateTacticalMechanismKinds.nonEmpty ||
      diagnostic.decisionTrace.referenceTacticalRisk ||
      diagnostic.decisionTrace.candidateTacticalRefutationBridge ||
      !noEventCauseComparison(diagnostic)

  def structuralWitnessFunnelJson(diagnostics: List[CandidateComparisonDiagnostic]): JsObject =
    val opportunity = diagnostics.filter(structuralOpportunity)
    val opportunityFlows = diagnosticFlows(opportunity).filter((_, flow) => noEventCauseFlow(flow))
    Json.obj(
      "opportunityComparisonIds" -> opportunity.map(_.id),
      "opportunityComparisonCount" -> opportunity.size,
      "structuralCauseProducedComparisonCount" -> opportunity.count(diagnostic =>
        diagnostic.relativeCauseDiagnostics.causeFlow.exists(noEventCauseFlow)
      ),
      "ownedAdmissibleLongTermProofComparisonCount" -> opportunity.count(diagnostic =>
        diagnostic.relativeCauseDiagnostics.causeFlow.exists(flow => noEventCauseFlow(flow) && flow.hasOwnedAdmissibleLongTermProof)
      ),
      "structuralIdeaCreatedComparisonCount" -> opportunity.count(diagnostic =>
        diagnostic.relativeCauseDiagnostics.causeFlow.exists(flow => noEventCauseFlow(flow) && !flow.causeWithoutIdea)
      ),
      "structuralClaimCandidateCreatedComparisonCount" -> opportunity.count(diagnostic =>
        diagnostic.relativeCauseDiagnostics.causeFlow.exists(flow => noEventCauseFlow(flow) && flow.claimCandidateIds.nonEmpty)
      ),
      "structuralFinalClaimIncludedComparisonCount" -> opportunity.count(diagnostic =>
        diagnostic.relativeCauseDiagnostics.causeFlow.exists(flow => noEventCauseFlow(flow) && flow.claimIds.nonEmpty)
      ),
      "structuralRootSurfacedComparisonCount" -> opportunity.count(diagnostic =>
        diagnostic.moveJudgmentView.primaryRootCauseEvidenceIds.nonEmpty &&
          diagnostic.moveJudgmentView.primaryRootCauseKinds.exists(kind => ClaimEventCluster.kindForCause(kind).isEmpty)
      ),
      "tacticalWitnessSurfacedComparisonCount" -> opportunity.count(
        _.moveJudgmentView.primaryTacticalWitnessCauseEvidenceIds.nonEmpty
      ),
      "stageCounts" -> stageCountsJson(opportunityFlows)
    )

  private def primaryActionableStructuralOpportunity(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.comparisonKind == CandidateComparisonKind.PlayedVsBest &&
      badVerdict(diagnostic.verdict) &&
      diagnostic.subjectBinding != SubjectBindingClass.Other &&
      structuralOpportunityAxisOpportunities(diagnostic).nonEmpty

  private def structuralOpportunityAxisOpportunities(diagnostic: CandidateComparisonDiagnostic): List[String] =
    val axes = diagnostic.decisionTrace.semanticAxisDiagnostics
    val referenceAxes =
      (axes.referenceLeadAxes ++ axes.referenceOnlyAxes)
        .distinct
        .filter(referenceAxisCanExplainStructuralLoss)
        .filterNot(planCoherenceAxis)
    val candidateAxes =
      (axes.candidateLeadAxes ++ axes.candidateOnlyAxes)
        .distinct
        .filter(candidateAxisCanExplainStructuralLoss)
        .filterNot(planCoherenceAxis)
    (referenceAxes ++ candidateAxes).distinct

  private final case class SemanticRubricSlotRow(
      comparisonId: String,
      unit: PositionPlanTechniqueUnit,
      axisKey: Option[String],
      terminalStage: String,
      strictLineageTerminalStage: String,
      objectBound: Boolean,
      exactAxisOrPattern: Boolean,
      causeOwned: Boolean,
      claimSurvived: Boolean,
      viewSurfaced: Boolean,
      ownedCauseLinked: Boolean,
      clusteredCoherent: Boolean,
      strictCauseLineageBound: Boolean,
      strictPrimaryRootLineageBound: Boolean,
      strictPrimaryRootTierLineageBound: Boolean,
      frameIds: List[String],
      detailMechanismKinds: List[StrategicMechanismKind],
      detailSemanticAnchorKeys: List[String],
      semanticDetailTokens: List[String],
      semanticDetailTokenGroups: List[List[String]],
      objectBindingSignatures: List[String],
      sourceEvidenceIds: List[String],
      causeKinds: List[RelativeCauseKind],
      primaryRootCauseKinds: List[RelativeCauseKind],
      primaryRootCauseEvidenceIds: List[String],
      primaryRootArbitrationTiers: List[MoveJudgmentCauseRootArbitrationTier],
      primaryRootCauseEvidenceTiers: List[PrimaryRootCauseEvidenceTier],
      causeIds: List[String],
      causeIdKinds: List[(String, RelativeCauseKind)],
      claimIds: List[String]
  )

  private[qc] def semanticRubricExpectedSlotCoverageJson(
      expectedSlots: List[ExpectedSemanticSlot],
      diagnostics: List[CandidateComparisonDiagnostic],
      expectedQuestionIds: List[String] = Nil
  ): JsObject =
    val primaryRows = diagnostics.filter(primaryActionableStructuralOpportunity).flatMap(semanticRubricSlotRows)
    val explicitProbeRows =
      Option
        .when(expectedSlots.exists(expectedSemanticSlotRequiresExplicitProbeRows))(
          diagnostics.flatMap(semanticRubricSlotRows)
        )
        .getOrElse(Nil)
    val slotRows =
      expectedSlots.map(slot =>
        val rows =
          if expectedSemanticSlotRequiresExplicitProbeRows(slot) then (primaryRows ++ explicitProbeRows).distinct
          else primaryRows
        expectedSemanticSlotCoverage(slot, rows)
      )
    val questionIds = slotRows.flatMap(row => (row \ "questionId").asOpt[String]).distinct.sorted
    val matchedQuestionIds =
      slotRows
        .filter(row => (row \ "matched").as[Boolean])
        .flatMap(row => (row \ "questionId").asOpt[String])
        .distinct
        .sorted
    val expectedQuestions = expectedQuestionIds.map(_.trim).filter(_.nonEmpty).distinct.sorted
    val measuredExpectedQuestionIds = expectedQuestions.filter(questionIds.toSet)
    val unmeasuredExpectedQuestionIds = expectedQuestions.diff(measuredExpectedQuestionIds)
    val coveredExpectedQuestionIds =
      if expectedQuestions.isEmpty then matchedQuestionIds
      else matchedQuestionIds.filter(measuredExpectedQuestionIds.contains)
    val missingExpectedQuestionIds = measuredExpectedQuestionIds.diff(coveredExpectedQuestionIds)
    val missingSlotIds =
      slotRows.filterNot(row => (row \ "matched").as[Boolean]).map(row => (row \ "id").as[String])
    val failedRequiredTerminalStageSlotIds =
      slotRows.filter(row => !(row \ "terminalStageSatisfied").as[Boolean]).map(row => (row \ "id").as[String])
    val classifiedMissingSlotIds = classifiedMissingSlotIdsFor(slotRows)
    val unclassifiedMissingSlotIds = unclassifiedMissingSlotIdsFor(missingSlotIds, classifiedMissingSlotIds)
    val planOptionOwnedCauseExpectationSlotIds = slotIdsWithFlag(slotRows, "planOptionOwnedCauseExpectation")
    val viewOnlyOwnedCauseExpectationSlotIds = slotIdsWithFlag(slotRows, "viewOnlyOwnedCauseExpectation")
    val semanticTokenExpectationMissSlotIds = slotIdsWithFlag(slotRows, "semanticTokenExpectationMiss")
    Json.obj(
      "classification" -> "audit_only",
      "expectedSlotCount" -> slotRows.size,
      "matchedSlotCount" -> slotRows.count(row => (row \ "matched").as[Boolean]),
      "viewSurfacedCount" -> slotRows.count(row => (row \ "viewSurfaced").as[Boolean]),
      "ownedCauseLinkedCount" -> slotRows.count(row => (row \ "ownedCauseLinked").as[Boolean]),
      "clusteredCoherentCount" -> slotRows.count(row => (row \ "clusteredCoherent").as[Boolean]),
      "coLocatedSemanticDetailTokenFailureCount" -> slotRows.count(row =>
        (row \ "coLocatedSemanticDetailTokenFailure").asOpt[Boolean].contains(true)
      ),
      "causeLineageTokenCoLocationSatisfied" -> slotRows.forall(row =>
        (row \ "causeLineageTokenCoLocationSatisfied").asOpt[Boolean].getOrElse(true)
      ),
      "rootLineageTokenCoLocationSatisfied" -> slotRows.forall(row =>
        (row \ "rootLineageTokenCoLocationSatisfied").asOpt[Boolean].getOrElse(true)
      ),
      "rootTierLineageTokenCoLocationSatisfied" -> slotRows.forall(row =>
        (row \ "rootTierLineageTokenCoLocationSatisfied").asOpt[Boolean].getOrElse(true)
      ),
      "causeBorrowFalsePositiveCount" -> slotRows.count(row =>
        (row \ "causeBorrowFalsePositive").asOpt[Boolean].contains(true)
      ),
      "primaryRootBorrowFalsePositiveCount" -> slotRows.count(row =>
        (row \ "primaryRootBorrowFalsePositive").asOpt[Boolean].contains(true)
      ),
      "rootTierBorrowFalsePositiveCount" -> slotRows.count(row =>
        (row \ "rootTierBorrowFalsePositive").asOpt[Boolean].contains(true)
      ),
      "coLocatedSemanticDetailTokenFailureSlotIds" -> coLocatedSemanticDetailTokenFailureSlotIds(slotRows),
      "terminalStageCounts" -> stringCountsJson(slotRows.map(row => (row \ "terminalStage").as[String])),
      "missingSlotIds" -> missingSlotIds,
      "missingUniqueSlotIds" -> missingSlotIds.distinct.sorted,
      "missingSlotIdCounts" -> stringCountsJson(missingSlotIds),
      "classifiedMissingSlotIds" -> classifiedMissingSlotIds,
      "classifiedMissingSlotCount" -> classifiedMissingSlotIds.size,
      "unclassifiedMissingSlotIds" -> unclassifiedMissingSlotIds,
      "unclassifiedMissingSlotCount" -> unclassifiedMissingSlotIds.size,
      "failedRequiredTerminalStageSlotIds" -> failedRequiredTerminalStageSlotIds,
      "failedRequiredTerminalStageUniqueSlotIds" -> failedRequiredTerminalStageSlotIds.distinct.sorted,
      "failedRequiredTerminalStageSlotIdCounts" -> stringCountsJson(failedRequiredTerminalStageSlotIds),
      "planOptionOwnedCauseExpectationSlotIds" -> planOptionOwnedCauseExpectationSlotIds,
      "planOptionOwnedCauseExpectationSlotCount" -> planOptionOwnedCauseExpectationSlotIds.size,
      "viewOnlyOwnedCauseExpectationSlotIds" -> viewOnlyOwnedCauseExpectationSlotIds,
      "viewOnlyOwnedCauseExpectationSlotCount" -> viewOnlyOwnedCauseExpectationSlotIds.size,
      "semanticTokenExpectationMissSlotIds" -> semanticTokenExpectationMissSlotIds,
      "semanticTokenExpectationMissSlotCount" -> semanticTokenExpectationMissSlotIds.size,
      "questionIds" -> questionIds,
      "expectedQuestionIds" -> expectedQuestions,
      "measuredExpectedQuestionIds" -> measuredExpectedQuestionIds,
      "unmeasuredExpectedQuestionIds" -> unmeasuredExpectedQuestionIds,
      "unmeasuredExpectedQuestionIdCount" -> unmeasuredExpectedQuestionIds.size,
      "coveredExpectedQuestionIds" -> coveredExpectedQuestionIds,
      "missingExpectedQuestionIds" -> missingExpectedQuestionIds,
      "missingExpectedQuestionIdCount" -> missingExpectedQuestionIds.size,
      "expectedQuestionCoverageComplete" -> (missingExpectedQuestionIds.isEmpty && unmeasuredExpectedQuestionIds.isEmpty),
      "missingQuestionIdSlotCount" -> slotRows.count(row => (row \ "questionId").asOpt[String].isEmpty),
      "missingQuestionIdSlotIds" -> slotRows
        .filter(row => (row \ "questionId").asOpt[String].isEmpty)
        .map(row => (row \ "id").as[String]),
      "byQuestionId" -> semanticRubricExpectedSlotCoverageByQuestionIdJson(slotRows, measuredExpectedQuestionIds),
      "slots" -> JsArray(slotRows)
    ) ++ MoveReviewPhase3AuditMetrics.moveMeaningSurfaceDiagnosticsJson(diagnostics)

  private def expectedSemanticSlotRequiresExplicitProbeRows(slot: ExpectedSemanticSlot): Boolean =
    slot.axisKey.nonEmpty ||
      slot.requiredMechanismKinds.nonEmpty ||
      slot.requiredCauseKinds.nonEmpty ||
      slot.requiredPrimaryRootCauseKinds.nonEmpty ||
      slot.requiredPrimaryRootArbitrationTiers.nonEmpty ||
      slot.requiredTerminalConsequenceKinds.nonEmpty ||
      slot.requiredSemanticDetailTokens.nonEmpty ||
      slot.requiredCoLocatedSemanticDetailTokens.nonEmpty ||
      slot.requiredSemanticAnchorTokens.nonEmpty ||
      slot.requiredObjectBindingTokens.nonEmpty ||
      slot.requiredTerminalStage.nonEmpty

  private[qc] def semanticRubricExpectedSlotCorpusCoverageJson(coverages: List[JsValue]): JsObject =
    val slotRows =
      coverages.flatMap(coverage => (coverage \ "slots").asOpt[List[JsObject]].getOrElse(Nil))
    val questionIds =
      (slotRows.flatMap(row => (row \ "questionId").asOpt[String]) ++
        coverages.flatMap(coverage => (coverage \ "questionIds").asOpt[List[String]].getOrElse(Nil)))
        .distinct
        .sorted
    val matchedQuestionIds =
      slotRows
        .filter(row => (row \ "matched").as[Boolean])
        .flatMap(row => (row \ "questionId").asOpt[String])
        .distinct
        .sorted
    val expectedQuestions =
      coverages
        .flatMap(coverage => (coverage \ "expectedQuestionIds").asOpt[List[String]].getOrElse(Nil))
        .map(_.trim)
        .filter(_.nonEmpty)
        .distinct
        .sorted
    val measuredExpectedQuestionIds = expectedQuestions.filter(questionIds.toSet)
    val unmeasuredExpectedQuestionIds = expectedQuestions.diff(measuredExpectedQuestionIds)
    val coveredExpectedQuestionIds =
      if expectedQuestions.isEmpty then matchedQuestionIds
      else matchedQuestionIds.filter(measuredExpectedQuestionIds.contains)
    val missingExpectedQuestionIds = measuredExpectedQuestionIds.diff(coveredExpectedQuestionIds)
    val missingSlotIds =
      slotRows.filterNot(row => (row \ "matched").as[Boolean]).map(row => (row \ "id").as[String])
    val failedRequiredTerminalStageSlotIds =
      slotRows.filter(row => !(row \ "terminalStageSatisfied").as[Boolean]).map(row => (row \ "id").as[String])
    val classifiedMissingSlotIds = classifiedMissingSlotIdsFor(slotRows)
    val unclassifiedMissingSlotIds = unclassifiedMissingSlotIdsFor(missingSlotIds, classifiedMissingSlotIds)
    val planOptionOwnedCauseExpectationSlotIds = slotIdsWithFlag(slotRows, "planOptionOwnedCauseExpectation")
    val viewOnlyOwnedCauseExpectationSlotIds = slotIdsWithFlag(slotRows, "viewOnlyOwnedCauseExpectation")
    val semanticTokenExpectationMissSlotIds = slotIdsWithFlag(slotRows, "semanticTokenExpectationMiss")
    Json.obj(
      "classification" -> "audit_only",
      "coverageRowCount" -> coverages.size,
      "expectedSlotCount" -> slotRows.size,
      "matchedSlotCount" -> slotRows.count(row => (row \ "matched").as[Boolean]),
      "viewSurfacedCount" -> slotRows.count(row => (row \ "viewSurfaced").as[Boolean]),
      "ownedCauseLinkedCount" -> slotRows.count(row => (row \ "ownedCauseLinked").as[Boolean]),
      "clusteredCoherentCount" -> slotRows.count(row => (row \ "clusteredCoherent").as[Boolean]),
      "coLocatedSemanticDetailTokenFailureCount" -> slotRows.count(row =>
        (row \ "coLocatedSemanticDetailTokenFailure").asOpt[Boolean].contains(true)
      ),
      "causeLineageTokenCoLocationSatisfied" -> slotRows.forall(row =>
        (row \ "causeLineageTokenCoLocationSatisfied").asOpt[Boolean].getOrElse(true)
      ),
      "rootLineageTokenCoLocationSatisfied" -> slotRows.forall(row =>
        (row \ "rootLineageTokenCoLocationSatisfied").asOpt[Boolean].getOrElse(true)
      ),
      "rootTierLineageTokenCoLocationSatisfied" -> slotRows.forall(row =>
        (row \ "rootTierLineageTokenCoLocationSatisfied").asOpt[Boolean].getOrElse(true)
      ),
      "causeBorrowFalsePositiveCount" -> slotRows.count(row =>
        (row \ "causeBorrowFalsePositive").asOpt[Boolean].contains(true)
      ),
      "primaryRootBorrowFalsePositiveCount" -> slotRows.count(row =>
        (row \ "primaryRootBorrowFalsePositive").asOpt[Boolean].contains(true)
      ),
      "rootTierBorrowFalsePositiveCount" -> slotRows.count(row =>
        (row \ "rootTierBorrowFalsePositive").asOpt[Boolean].contains(true)
      ),
      "coLocatedSemanticDetailTokenFailureSlotIds" -> coLocatedSemanticDetailTokenFailureSlotIds(slotRows),
      "terminalStageCounts" -> stringCountsJson(slotRows.map(row => (row \ "terminalStage").as[String])),
      "missingSlotIds" -> missingSlotIds,
      "missingUniqueSlotIds" -> missingSlotIds.distinct.sorted,
      "missingSlotIdCounts" -> stringCountsJson(missingSlotIds),
      "classifiedMissingSlotIds" -> classifiedMissingSlotIds,
      "classifiedMissingSlotCount" -> classifiedMissingSlotIds.size,
      "unclassifiedMissingSlotIds" -> unclassifiedMissingSlotIds,
      "unclassifiedMissingSlotCount" -> unclassifiedMissingSlotIds.size,
      "failedRequiredTerminalStageSlotIds" -> failedRequiredTerminalStageSlotIds,
      "failedRequiredTerminalStageUniqueSlotIds" -> failedRequiredTerminalStageSlotIds.distinct.sorted,
      "failedRequiredTerminalStageSlotIdCounts" -> stringCountsJson(failedRequiredTerminalStageSlotIds),
      "planOptionOwnedCauseExpectationSlotIds" -> planOptionOwnedCauseExpectationSlotIds,
      "planOptionOwnedCauseExpectationSlotCount" -> planOptionOwnedCauseExpectationSlotIds.size,
      "viewOnlyOwnedCauseExpectationSlotIds" -> viewOnlyOwnedCauseExpectationSlotIds,
      "viewOnlyOwnedCauseExpectationSlotCount" -> viewOnlyOwnedCauseExpectationSlotIds.size,
      "semanticTokenExpectationMissSlotIds" -> semanticTokenExpectationMissSlotIds,
      "semanticTokenExpectationMissSlotCount" -> semanticTokenExpectationMissSlotIds.size,
      "questionIds" -> questionIds,
      "expectedQuestionIds" -> expectedQuestions,
      "measuredExpectedQuestionIds" -> measuredExpectedQuestionIds,
      "unmeasuredExpectedQuestionIds" -> unmeasuredExpectedQuestionIds,
      "unmeasuredExpectedQuestionIdCount" -> unmeasuredExpectedQuestionIds.size,
      "coveredExpectedQuestionIds" -> coveredExpectedQuestionIds,
      "missingExpectedQuestionIds" -> missingExpectedQuestionIds,
      "missingExpectedQuestionIdCount" -> missingExpectedQuestionIds.size,
      "expectedQuestionCoverageComplete" -> (missingExpectedQuestionIds.isEmpty && unmeasuredExpectedQuestionIds.isEmpty),
      "missingQuestionIdSlotCount" -> slotRows.count(row => (row \ "questionId").asOpt[String].isEmpty),
      "missingQuestionIdSlotIds" -> slotRows
        .filter(row => (row \ "questionId").asOpt[String].isEmpty)
        .map(row => (row \ "id").as[String]),
      "byQuestionId" -> semanticRubricExpectedSlotCoverageByQuestionIdJson(slotRows, measuredExpectedQuestionIds),
      "slots" -> JsArray(slotRows)
    ) ++ MoveReviewPhase3AuditMetrics.moveMeaningSurfaceCorpusDiagnosticsJson(coverages)

  private def semanticRubricExpectedSlotCoverageByQuestionIdJson(
      slotRows: List[JsObject],
      expectedQuestionIds: List[String]
  ): JsObject =
    val rowsByQuestionId = slotRows.groupBy(row => (row \ "questionId").asOpt[String].getOrElse("unassigned"))
    val questionIds = (rowsByQuestionId.keySet.toList ++ expectedQuestionIds).distinct.sorted
    JsObject(
      questionIds
        .map { questionId =>
          val rows = rowsByQuestionId.getOrElse(questionId, Nil)
          val missingSlotIds =
            rows.filterNot(row => (row \ "matched").as[Boolean]).map(row => (row \ "id").as[String])
          questionId -> Json.obj(
            "missingExpectedQuestion" -> (expectedQuestionIds.contains(questionId) && rows.isEmpty),
            "expectedSlotCount" -> rows.size,
            "matchedSlotCount" -> rows.count(row => (row \ "matched").as[Boolean]),
            "viewSurfacedCount" -> rows.count(row => (row \ "viewSurfaced").as[Boolean]),
            "ownedCauseLinkedCount" -> rows.count(row => (row \ "ownedCauseLinked").as[Boolean]),
            "clusteredCoherentCount" -> rows.count(row => (row \ "clusteredCoherent").as[Boolean]),
            "coLocatedSemanticDetailTokenFailureCount" -> rows.count(row =>
              (row \ "coLocatedSemanticDetailTokenFailure").asOpt[Boolean].contains(true)
            ),
            "causeBorrowFalsePositiveCount" -> rows.count(row =>
              (row \ "causeBorrowFalsePositive").asOpt[Boolean].contains(true)
            ),
            "primaryRootBorrowFalsePositiveCount" -> rows.count(row =>
              (row \ "primaryRootBorrowFalsePositive").asOpt[Boolean].contains(true)
            ),
            "rootTierBorrowFalsePositiveCount" -> rows.count(row =>
              (row \ "rootTierBorrowFalsePositive").asOpt[Boolean].contains(true)
            ),
            "coLocatedSemanticDetailTokenFailureSlotIds" -> coLocatedSemanticDetailTokenFailureSlotIds(rows),
            "terminalStageCounts" -> stringCountsJson(rows.map(row => (row \ "terminalStage").as[String])),
            "missingSlotIds" -> missingSlotIds,
            "missingUniqueSlotIds" -> missingSlotIds.distinct.sorted,
            "missingSlotIdCounts" -> stringCountsJson(missingSlotIds),
            "slotIds" -> rows.map(row => (row \ "id").as[String]).distinct.sorted,
            "units" -> rows.map(row => (row \ "unit").as[String]).distinct.sorted
          )
        }
    )

  private def coLocatedSemanticDetailTokenFailureSlotIds(slotRows: List[JsObject]): List[String] =
    slotRows
      .filter(row => (row \ "coLocatedSemanticDetailTokenFailure").asOpt[Boolean].contains(true))
      .flatMap(row => (row \ "id").asOpt[String])
      .distinct
      .sorted

  private def slotIdsWithFlag(slotRows: List[JsObject], flag: String): List[String] =
    slotRows
      .filter(row => (row \ flag).asOpt[Boolean].contains(true))
      .flatMap(row => (row \ "id").asOpt[String])
      .distinct
      .sorted

  private def classifiedMissingSlotIdsFor(slotRows: List[JsObject]): List[String] =
    slotRows
      .filter(row => !(row \ "matched").as[Boolean] && knownMissingClassification(row))
      .flatMap(row => (row \ "id").asOpt[String])
      .distinct
      .sorted

  private def unclassifiedMissingSlotIdsFor(
      missingSlotIds: List[String],
      classifiedMissingSlotIds: List[String]
  ): List[String] =
    val classified = classifiedMissingSlotIds.toSet
    missingSlotIds.distinct.sorted.filterNot(classified)

  private def knownMissingClassification(row: JsObject): Boolean =
    List(
      "coLocatedSemanticDetailTokenFailure",
      "causeBorrowFalsePositive",
      "primaryRootBorrowFalsePositive",
      "rootTierBorrowFalsePositive",
      "planOptionOwnedCauseExpectation",
      "viewOnlyOwnedCauseExpectation",
      "semanticTokenExpectationMiss"
    ).exists(flag => (row \ flag).asOpt[Boolean].contains(true))

  private def expectedSemanticSlotCoverage(
      slot: ExpectedSemanticSlot,
      rows: List[SemanticRubricSlotRow]
  ): JsObject =
    val unitEligibleRows =
      rows.filter(row =>
        row.unit == slot.unit &&
          slot.axisKey.forall(expectedAxis => row.axisKey.contains(expectedAxis)) &&
          slot.requiredMechanismKinds.forall(kind => row.detailMechanismKinds.contains(kind))
      )
    val structurallyEligibleRows =
      unitEligibleRows.filter(row =>
          (slot.requiredCauseKinds.isEmpty ||
            slot.requiredCauseKinds.exists(kind => row.causeKinds.contains(kind))) &&
          (slot.requiredPrimaryRootCauseKinds.isEmpty ||
            slot.requiredPrimaryRootCauseKinds.exists(kind => row.primaryRootCauseKinds.contains(kind))) &&
          (slot.requiredPrimaryRootArbitrationTiers.isEmpty ||
            slot.requiredPrimaryRootArbitrationTiers.exists(tier => row.primaryRootArbitrationTiers.contains(tier)))
      )
    val semanticDetailTokenRows =
      unitEligibleRows.filter(row => semanticDetailTokensSatisfiedForSlot(slot, row))
    val coLocatedSemanticDetailTokenRows =
      semanticDetailTokenRows.filter(row =>
        coLocatedTokensSatisfied(slot.requiredCoLocatedSemanticDetailTokens, semanticDetailTokenGroupsForSlot(slot, row))
      )
    val structurallySemanticDetailTokenRows =
      structurallyEligibleRows.filter(row => semanticDetailTokensSatisfiedForSlot(slot, row))
    val structurallyCoLocatedSemanticDetailTokenRows =
      structurallySemanticDetailTokenRows.filter(row =>
        coLocatedTokensSatisfied(slot.requiredCoLocatedSemanticDetailTokens, semanticDetailTokenGroupsForSlot(slot, row))
      )
    val semanticDetailTokensSatisfied = semanticDetailTokenRows.nonEmpty
    val coLocatedSemanticDetailTokensSatisfied =
      slot.requiredCoLocatedSemanticDetailTokens.isEmpty || coLocatedSemanticDetailTokenRows.nonEmpty
    val coLocatedSemanticDetailTokenFailure =
      slot.requiredCoLocatedSemanticDetailTokens.nonEmpty &&
        semanticDetailTokensSatisfied &&
        !coLocatedSemanticDetailTokensSatisfied
    val bestCoLocatedSemanticDetailTokenGroup =
      bestCoLocatedSemanticDetailTokens(slot, semanticDetailTokenRows)
    val missingCoLocatedSemanticDetailTokensFromBestGroup =
      missingRequiredTokens(slot.requiredCoLocatedSemanticDetailTokens, bestCoLocatedSemanticDetailTokenGroup)
    val objectBindingTokensSatisfiedRows =
      coLocatedSemanticDetailTokenRows.filter(row => objectBindingTokensSatisfiedForSlot(slot, row))
    val objectBindingTokenCoLocatedRows =
      objectBindingTokensSatisfiedRows.filter(row =>
        objectBindingTokensCoLocated(
          slot.requiredObjectBindingTokens,
          semanticDetailCoLocationTokens(slot),
          semanticDetailTokenGroupsForSlot(slot, row)
        )
      )
    val structurallyObjectBindingTokensSatisfiedRows =
      structurallyCoLocatedSemanticDetailTokenRows.filter(row => objectBindingTokensSatisfiedForSlot(slot, row))
    val structurallyObjectBindingTokenCoLocatedRows =
      structurallyObjectBindingTokensSatisfiedRows.filter(row =>
        objectBindingTokensCoLocated(
          slot.requiredObjectBindingTokens,
          semanticDetailCoLocationTokens(slot),
          semanticDetailTokenGroupsForSlot(slot, row)
        )
      )
    val objectBindingTokensSatisfied =
      slot.requiredObjectBindingTokens.isEmpty || objectBindingTokensSatisfiedRows.nonEmpty
    val objectBindingTokenCoLocationSatisfied =
      slot.requiredObjectBindingTokens.isEmpty || objectBindingTokenCoLocatedRows.nonEmpty
    val bestObjectBindingTokenGroup =
      bestObjectBindingTokenGroupFor(
        slot,
        objectBindingTokensSatisfiedRows
      )
    val missingObjectBindingTokensFromBestGroup =
      missingObjectBindingTokens(slot.requiredObjectBindingTokens, bestObjectBindingTokenGroup)
    val legacyMatches =
      structurallyObjectBindingTokenCoLocatedRows.filter(row =>
          tokensSatisfied(slot.requiredSemanticAnchorTokens, row.detailSemanticAnchorKeys)
      )
    val causeLineageTokenCoLocatedRows =
      if causeLineageTokenCoLocationRequired(slot) then
        structurallyObjectBindingTokenCoLocatedRows.filter(row => causeLineageTokensCoLocated(slot, row))
      else structurallyObjectBindingTokenCoLocatedRows
    val rootLineageTokenCoLocatedRows =
      if rootLineageTokenCoLocationRequired(slot) then
        causeLineageTokenCoLocatedRows.filter(row => rootLineageTokensCoLocated(slot, row))
      else causeLineageTokenCoLocatedRows
    val rootTierLineageTokenCoLocatedRows =
      if rootTierLineageTokenCoLocationRequired(slot) then
        rootLineageTokenCoLocatedRows.filter(row => rootTierLineageTokensCoLocated(slot, row))
      else rootLineageTokenCoLocatedRows
    val causeLineageTokenCoLocationSatisfied =
      !causeLineageTokenCoLocationRequired(slot) || causeLineageTokenCoLocatedRows.nonEmpty
    val rootLineageTokenCoLocationSatisfied =
      !rootLineageTokenCoLocationRequired(slot) || rootLineageTokenCoLocatedRows.nonEmpty
    val rootTierLineageTokenCoLocationSatisfied =
      !rootTierLineageTokenCoLocationRequired(slot) || rootTierLineageTokenCoLocatedRows.nonEmpty
    val matches =
      rootTierLineageTokenCoLocatedRows.filter(row =>
        tokensSatisfied(slot.requiredSemanticAnchorTokens, row.detailSemanticAnchorKeys)
      )
    val legacyBest = legacyMatches.headOption
    val legacyTerminalStage = legacyBest.map(_.terminalStage).getOrElse("missing_semantic_slot")
    val legacyMatched =
      legacyBest.nonEmpty &&
        slot.requiredTerminalStage.forall(semanticRubricStageSatisfied(legacyTerminalStage, _))
    val causeBorrowFalsePositive =
      slot.requiredCauseKinds.nonEmpty && legacyMatched && !causeLineageTokenCoLocationSatisfied
    val primaryRootBorrowFalsePositive =
      slot.requiredPrimaryRootCauseKinds.nonEmpty && legacyMatched && !rootLineageTokenCoLocationSatisfied
    val rootTierBorrowFalsePositive =
      slot.requiredPrimaryRootArbitrationTiers.nonEmpty && legacyMatched && !rootTierLineageTokenCoLocationSatisfied
    val best = matches.headOption
    val terminalStage = best.map(_.terminalStage).getOrElse("missing_semantic_slot")
    val strictLineageTerminalStage = best.map(_.strictLineageTerminalStage).getOrElse("missing_semantic_slot")
    val terminalStageSatisfied =
      slot.requiredTerminalStage.forall(semanticRubricStageSatisfied(terminalStage, _))
    val strictLineageTerminalStageSatisfied =
      slot.requiredTerminalStage.forall(semanticRubricStageSatisfied(strictLineageTerminalStage, _))
    val matched = best.nonEmpty && terminalStageSatisfied
    Json.obj(
      "id" -> slot.id,
      "unit" -> slot.unit.toString,
      "axisKey" -> slot.axisKey,
      "questionId" -> slot.questionId,
      "description" -> slot.description,
      "requiredTerminalStage" -> slot.requiredTerminalStage,
      "requiredMechanismKinds" -> slot.requiredMechanismKinds.map(_.toString),
      "requiredCauseKinds" -> slot.requiredCauseKinds.map(_.toString),
      "requiredPrimaryRootCauseKinds" -> slot.requiredPrimaryRootCauseKinds.map(_.toString),
      "requiredPrimaryRootArbitrationTiers" -> slot.requiredPrimaryRootArbitrationTiers.map(_.toString),
      "requiredTerminalConsequenceKinds" -> slot.requiredTerminalConsequenceKinds.map(_.toString),
      "requiredSemanticDetailTokens" -> slot.requiredSemanticDetailTokens,
      "requiredCoLocatedSemanticDetailTokens" -> slot.requiredCoLocatedSemanticDetailTokens,
      "requiredSemanticAnchorTokens" -> slot.requiredSemanticAnchorTokens,
      "requiredObjectBindingTokens" -> slot.requiredObjectBindingTokens,
      "matched" -> matched,
      "terminalStage" -> terminalStage,
      "strictLineageTerminalStage" -> strictLineageTerminalStage,
      "terminalStageSatisfied" -> terminalStageSatisfied,
      "strictLineageTerminalStageSatisfied" -> strictLineageTerminalStageSatisfied,
      "semanticDetailTokensSatisfied" -> semanticDetailTokensSatisfied,
      "coLocatedSemanticDetailTokensSatisfied" -> coLocatedSemanticDetailTokensSatisfied,
      "coLocatedSemanticDetailTokenFailure" -> coLocatedSemanticDetailTokenFailure,
      "bestCoLocatedSemanticDetailTokenGroup" -> bestCoLocatedSemanticDetailTokenGroup,
      "missingCoLocatedSemanticDetailTokensFromBestGroup" -> missingCoLocatedSemanticDetailTokensFromBestGroup,
      "objectBindingTokensSatisfied" -> objectBindingTokensSatisfied,
      "objectBindingTokenCoLocationSatisfied" -> objectBindingTokenCoLocationSatisfied,
      "bestObjectBindingTokenGroup" -> bestObjectBindingTokenGroup,
      "missingObjectBindingTokensFromBestGroup" -> missingObjectBindingTokensFromBestGroup,
      "causeLineageTokenCoLocationRequired" -> causeLineageTokenCoLocationRequired(slot),
      "rootLineageTokenCoLocationRequired" -> rootLineageTokenCoLocationRequired(slot),
      "rootTierLineageTokenCoLocationRequired" -> rootTierLineageTokenCoLocationRequired(slot),
      "causeLineageTokenCoLocationSatisfied" -> causeLineageTokenCoLocationSatisfied,
      "rootLineageTokenCoLocationSatisfied" -> rootLineageTokenCoLocationSatisfied,
      "rootTierLineageTokenCoLocationSatisfied" -> rootTierLineageTokenCoLocationSatisfied,
      "causeBorrowFalsePositive" -> causeBorrowFalsePositive,
      "primaryRootBorrowFalsePositive" -> primaryRootBorrowFalsePositive,
      "rootTierBorrowFalsePositive" -> rootTierBorrowFalsePositive,
      "planOptionOwnedCauseExpectation" -> planOptionOwnedCauseExpectation(slot),
      "viewOnlyOwnedCauseExpectation" -> viewOnlyOwnedCauseExpectation(slot, best),
      "semanticTokenExpectationMiss" -> (slot.requiredSemanticDetailTokens.nonEmpty && !semanticDetailTokensSatisfied),
      "legacyMatchedBeforeStrictLineage" -> legacyMatched,
      "legacyTerminalStageBeforeStrictLineage" -> legacyTerminalStage,
      "objectBound" -> best.exists(_.objectBound),
      "exactAxisOrPattern" -> best.exists(_.exactAxisOrPattern),
      "causeOwned" -> best.exists(_.causeOwned),
      "claimSurvived" -> best.exists(_.claimSurvived),
      "viewSurfaced" -> best.exists(_.viewSurfaced),
      "ownedCauseLinked" -> best.exists(_.ownedCauseLinked),
      "clusteredCoherent" -> best.exists(_.clusteredCoherent),
      "matchedComparisonIds" -> matches.map(_.comparisonId).distinct.sorted,
      "frameIds" -> matches.flatMap(_.frameIds).distinct.sorted,
      "detailMechanismKinds" -> matches.flatMap(_.detailMechanismKinds).distinct.sortBy(_.toString).map(_.toString),
      "detailSemanticAnchorKeys" -> matches.flatMap(_.detailSemanticAnchorKeys).distinct.sorted,
      "semanticDetailTokens" -> matches.flatMap(_.semanticDetailTokens).distinct.sorted,
      "semanticDetailTokenGroups" -> JsArray(
        matches
          .flatMap(_.semanticDetailTokenGroups)
          .distinct
          .sortBy(_.mkString("\u0000"))
          .map(tokens => JsArray(tokens.map(JsString.apply)))
      ),
      "objectBindingSignatures" -> matches.flatMap(_.objectBindingSignatures).distinct.sorted,
      "sourceEvidenceIds" -> matches.flatMap(_.sourceEvidenceIds).distinct.sorted,
      "causeKinds" -> matches.flatMap(_.causeKinds).distinct.sortBy(_.toString).map(_.toString),
      "primaryRootCauseKinds" -> matches.flatMap(_.primaryRootCauseKinds).distinct.sortBy(_.toString).map(_.toString),
      "primaryRootCauseEvidenceIds" -> matches.flatMap(_.primaryRootCauseEvidenceIds).distinct.sorted,
      "primaryRootArbitrationTiers" -> matches.flatMap(_.primaryRootArbitrationTiers).distinct.sortBy(_.toString).map(_.toString),
      "causeIds" -> matches.flatMap(_.causeIds).distinct.sorted,
      "claimIds" -> matches.flatMap(_.claimIds).distinct.sorted,
      "bestLineageTrace" -> best.map(semanticRubricLineageTraceJson).getOrElse(Json.obj()),
      "lineageTraces" -> JsArray(
        matches
          .sortBy(_.comparisonId)
          .map(semanticRubricLineageTraceJson)
      )
    )

  private def semanticRubricLineageTraceJson(row: SemanticRubricSlotRow): JsObject =
    Json.obj(
      "comparisonId" -> row.comparisonId,
      "unit" -> row.unit.toString,
      "axisKey" -> row.axisKey,
      "terminalStage" -> row.terminalStage,
      "strictLineageTerminalStage" -> row.strictLineageTerminalStage,
      "strictCauseLineageBound" -> row.strictCauseLineageBound,
      "strictPrimaryRootLineageBound" -> row.strictPrimaryRootLineageBound,
      "strictPrimaryRootTierLineageBound" -> row.strictPrimaryRootTierLineageBound,
      "frameIds" -> row.frameIds,
      "sourceEvidenceIds" -> row.sourceEvidenceIds,
      "detailMechanismKinds" -> row.detailMechanismKinds.distinct.sortBy(_.toString).map(_.toString),
      "detailSemanticAnchorKeys" -> row.detailSemanticAnchorKeys.distinct.sorted,
      "semanticDetailTokens" -> row.semanticDetailTokens.distinct.sorted,
      "semanticDetailTokenGroups" -> JsArray(
        row.semanticDetailTokenGroups
          .distinct
          .sortBy(_.mkString("\u0000"))
          .map(tokens => JsArray(tokens.map(JsString.apply)))
      ),
      "objectBindingSignatures" -> row.objectBindingSignatures,
      "causeKinds" -> row.causeKinds.map(_.toString),
      "causeIds" -> row.causeIds,
      "claimIds" -> row.claimIds,
      "primaryRootCauseKinds" -> row.primaryRootCauseKinds.map(_.toString),
      "primaryRootCauseEvidenceIds" -> row.primaryRootCauseEvidenceIds,
      "primaryRootArbitrationTiers" -> row.primaryRootArbitrationTiers.map(_.toString)
    )

  private def tokensSatisfied(requiredTokens: List[String], values: List[String]): Boolean =
    requiredTokens.forall(token => tokenSatisfied(token, values))

  private def tokenSatisfied(requiredToken: String, values: List[String]): Boolean =
    values.exists(value => tokenMatches(requiredToken, value))

  private def semanticRubricStageSatisfied(actual: String, required: String): Boolean =
    actual == required.trim

  private def tokenMatches(requiredToken: String, value: String): Boolean =
    val required = requiredToken.trim.toLowerCase
    val candidate = value.trim.toLowerCase
    required.nonEmpty &&
      (candidate == required || (required.endsWith(":") && candidate.startsWith(required)))

  private def semanticDetailTokensSatisfiedForSlot(
      slot: ExpectedSemanticSlot,
      row: SemanticRubricSlotRow
  ): Boolean =
    expectedSemanticDetailTokens(slot).isEmpty ||
      semanticDetailTokenGroupsForSlot(slot, row).exists(group =>
        tokensSatisfied(expectedSemanticDetailTokens(slot), group)
      )

  private def semanticDetailTokenGroupsForSlot(
      slot: ExpectedSemanticSlot,
      row: SemanticRubricSlotRow
  ): List[List[String]] =
    val unitToken = s"unit:${slot.unit}"
    row.semanticDetailTokenGroups
      .map(_.distinct.sorted)
      .filter(_.contains(unitToken))

  private def coLocatedTokensSatisfied(requiredTokens: List[String], tokenGroups: List[List[String]]): Boolean =
    requiredTokens.isEmpty ||
      tokenGroups.exists(group => tokensSatisfied(requiredTokens, group))

  private def semanticDetailCoLocationTokens(slot: ExpectedSemanticSlot): List[String] =
    if slot.requiredCoLocatedSemanticDetailTokens.nonEmpty then slot.requiredCoLocatedSemanticDetailTokens
    else expectedSemanticDetailTokens(slot)

  private def expectedSemanticDetailTokens(slot: ExpectedSemanticSlot): List[String] =
    (slot.requiredSemanticDetailTokens ++
      slot.requiredTerminalConsequenceKinds.map(kind => s"terminalConsequenceKind:$kind")).distinct

  private def planOptionOwnedCauseExpectation(slot: ExpectedSemanticSlot): Boolean =
    slot.unit == PositionPlanTechniqueUnit.PlanOptionSet &&
      slot.requiredTerminalStage.contains("owned_cause_linked")

  private def viewOnlyOwnedCauseExpectation(
      slot: ExpectedSemanticSlot,
      best: Option[SemanticRubricSlotRow]
  ): Boolean =
    slot.requiredTerminalStage.contains("owned_cause_linked") &&
      best.exists(row => row.viewSurfaced && !row.ownedCauseLinked)

  private def objectBindingTokensCoLocated(
      requiredObjectBindingTokens: List[String],
      requiredSemanticDetailTokens: List[String],
      tokenGroups: List[List[String]]
  ): Boolean =
    requiredObjectBindingTokens.isEmpty ||
      tokenGroups.exists(group =>
        tokensSatisfied(requiredSemanticDetailTokens, group) &&
          requiredObjectBindingTokens.forall(token => objectBindingTokenSatisfied(token, group))
      )

  private def objectBindingTokensSatisfiedForSlot(
      slot: ExpectedSemanticSlot,
      row: SemanticRubricSlotRow
  ): Boolean =
    slot.requiredObjectBindingTokens.isEmpty ||
      slot.requiredObjectBindingTokens.forall(token =>
        semanticDetailTokenGroupsForSlot(slot, row).exists(group => objectBindingTokenSatisfied(token, group))
      )

  private def objectBindingTokenSatisfied(token: String, values: List[String]): Boolean =
    objectBindingSemanticDetailTokens(token).exists(values.contains)

  private def objectBindingSemanticDetailTokens(token: String): List[String] =
    val normalized = token.trim
    val targetValue = roleTokenValue(normalized).filter(_ => normalized.startsWith("target="))
    if roleTokenValue(normalized).exists(_.isEmpty) then Nil
    else
      (
        List(
          roleTokenValue(normalized).filter(_ => normalized.startsWith("actor=")).map(value => s"objectActor:$value"),
          targetValue.map(value => s"objectTarget:$value"),
          roleTokenValue(normalized).filter(_ => normalized.startsWith("mechanism=")).map(value => s"objectMechanism:$value"),
          roleTokenValue(normalized).filter(_ => normalized.startsWith("consequence=")).map(value => s"objectConsequence:$value"),
          roleTokenValue(normalized).filter(_ => normalized.startsWith("witness=")).map(value => s"objectWitness:$value"),
          Some(normalized)
        ).flatten ++ targetValue.toList.flatMap(resourceContestTargetTokens)
      ).distinct

  private def roleTokenValue(token: String): Option[String] =
    List("actor=", "target=", "mechanism=", "consequence=", "witness=")
      .find(token.startsWith)
      .map(token.stripPrefix)

  private def resourceContestTargetTokens(targetValue: String): List[String] =
    if targetValue.startsWith("Square:") then
      List(s"resourceContestSquare:${targetValue.stripPrefix("Square:")}")
    else if targetValue.startsWith("File:") then
      List(s"resourceContestFile:${targetValue.stripPrefix("File:")}")
    else Nil

  private def causeLineageTokenCoLocationRequired(slot: ExpectedSemanticSlot): Boolean =
    slot.requiredCauseKinds.nonEmpty && semanticLineageTokenCoLocationRequired(slot)

  private def rootLineageTokenCoLocationRequired(slot: ExpectedSemanticSlot): Boolean =
    (slot.requiredPrimaryRootCauseKinds.nonEmpty || slot.requiredPrimaryRootArbitrationTiers.nonEmpty) &&
      semanticLineageTokenCoLocationRequired(slot)

  private def rootTierLineageTokenCoLocationRequired(slot: ExpectedSemanticSlot): Boolean =
    slot.requiredPrimaryRootArbitrationTiers.nonEmpty && semanticLineageTokenCoLocationRequired(slot)

  private def semanticLineageTokenCoLocationRequired(slot: ExpectedSemanticSlot): Boolean =
    expectedSemanticDetailTokens(slot).nonEmpty ||
      slot.requiredCoLocatedSemanticDetailTokens.nonEmpty ||
      slot.requiredObjectBindingTokens.nonEmpty

  private def causeLineageTokensCoLocated(slot: ExpectedSemanticSlot, row: SemanticRubricSlotRow): Boolean =
    val causeIds = row.causeIds.toSet
    val causeIdKinds = row.causeIdKinds.toSet
    causeIds.nonEmpty &&
      lineageCandidateTokenGroups(slot, row).exists(group =>
        causeEvidenceIdsFromTokens(group).exists(causeEvidenceId =>
          causeIds.contains(causeEvidenceId) &&
            (
              slot.requiredCauseKinds.isEmpty ||
                slot.requiredCauseKinds.exists(kind =>
                  causeIdKinds.contains(causeEvidenceId -> kind)
                )
            )
        )
      )

  private def rootLineageTokensCoLocated(slot: ExpectedSemanticSlot, row: SemanticRubricSlotRow): Boolean =
    val rootCauseEvidenceIds = row.primaryRootCauseEvidenceIds.toSet
    rootCauseEvidenceIds.nonEmpty &&
      lineageCandidateTokenGroups(slot, row).exists(group =>
        rootLineageEvidenceIdsFromTokens(group).exists(rootCauseEvidenceIds.contains)
      )

  private def rootTierLineageTokensCoLocated(slot: ExpectedSemanticSlot, row: SemanticRubricSlotRow): Boolean =
    val requiredTiers = slot.requiredPrimaryRootArbitrationTiers.toSet
    row.primaryRootCauseEvidenceTiers.nonEmpty &&
      requiredTiers.nonEmpty &&
      lineageCandidateTokenGroups(slot, row).exists(group =>
        val rootCauseEvidenceIds = rootLineageEvidenceIdsFromTokens(group).toSet
        row.primaryRootCauseEvidenceTiers.exists(tier =>
          rootCauseEvidenceIds(tier.causeEvidenceId) && requiredTiers(tier.tier)
        )
      )

  private def semanticRubricCauseLineageBound(
      causeIds: List[String],
      tokenGroups: List[List[String]]
  ): Boolean =
    val ids = causeIds.toSet
    ids.nonEmpty &&
      tokenGroups.exists(group => causeEvidenceIdsFromTokens(group).exists(ids.contains))

  private def semanticRubricPrimaryRootLineageBound(
      primaryRootCauseEvidenceIds: List[String],
      tokenGroups: List[List[String]]
  ): Boolean =
    val ids = primaryRootCauseEvidenceIds.toSet
    ids.nonEmpty &&
      tokenGroups.exists(group => rootLineageEvidenceIdsFromTokens(group).exists(ids.contains))

  private def semanticRubricPrimaryRootTierLineageBound(
      primaryRootCauseEvidenceTiers: List[PrimaryRootCauseEvidenceTier],
      tokenGroups: List[List[String]]
  ): Boolean =
    primaryRootCauseEvidenceTiers.nonEmpty &&
      tokenGroups.exists(group =>
        val rootCauseEvidenceIds = rootLineageEvidenceIdsFromTokens(group).toSet
        primaryRootCauseEvidenceTiers.exists(tier =>
          rootCauseEvidenceIds(tier.causeEvidenceId)
        )
      )

  private def lineageCandidateTokenGroups(slot: ExpectedSemanticSlot, row: SemanticRubricSlotRow): List[List[String]] =
    val requiredSemanticTokens = semanticDetailCoLocationTokens(slot)
    semanticDetailTokenGroupsForSlot(slot, row)
      .filter(group =>
        tokensSatisfied(requiredSemanticTokens, group) &&
          slot.requiredObjectBindingTokens.forall(token => objectBindingTokenSatisfied(token, group))
      )

  private def causeEvidenceIdsFromTokens(tokens: List[String]): List[String] =
    tokenValuesWithPrefix(tokens, "causeEvidenceId:")

  private def rootLineageEvidenceIdsFromTokens(tokens: List[String]): List[String] =
    (tokenValuesWithPrefix(tokens, "causeEvidenceId:") ++
      tokenValuesWithPrefix(tokens, "rootCauseEvidenceId:")).distinct

  private def tokenValuesWithPrefix(tokens: List[String], prefix: String): List[String] =
    tokens
      .filter(_.startsWith(prefix))
      .map(_.stripPrefix(prefix).trim)
      .filter(_.nonEmpty)
      .distinct

  private def bestObjectBindingTokenGroupFor(
      slot: ExpectedSemanticSlot,
      rows: List[SemanticRubricSlotRow]
  ): List[String] =
    val requiredObjectBindingTokens = slot.requiredObjectBindingTokens
    val requiredSemanticDetailTokens = semanticDetailCoLocationTokens(slot)
    if requiredObjectBindingTokens.isEmpty then Nil
    else
      rows
        .flatMap(row => semanticDetailTokenGroupsForSlot(slot, row))
        .filter(group =>
          requiredSemanticDetailTokens.exists(token => tokenSatisfied(token, group)) ||
            requiredObjectBindingTokens.exists(token => objectBindingTokenSatisfied(token, group))
        )
        .sortBy(group =>
          (
            -requiredSemanticDetailTokens.count(token => tokenSatisfied(token, group)),
            -requiredObjectBindingTokens.count(token => objectBindingTokenSatisfied(token, group)),
            group.mkString("\u0000")
          )
        )
        .headOption
        .getOrElse(Nil)

  private def missingObjectBindingTokens(requiredTokens: List[String], values: List[String]): List[String] =
    requiredTokens.filterNot(token => objectBindingTokenSatisfied(token, values))

  private def bestCoLocatedSemanticDetailTokens(
      slot: ExpectedSemanticSlot,
      rows: List[SemanticRubricSlotRow]
  ): List[String] =
    val requiredTokens = slot.requiredCoLocatedSemanticDetailTokens
    if requiredTokens.isEmpty then Nil
    else
      rows
        .flatMap(row => semanticDetailTokenGroupsForSlot(slot, row))
        .filter(group => requiredTokens.exists(token => tokenSatisfied(token, group)))
        .sortBy(group => (-requiredTokens.count(token => tokenSatisfied(token, group)), group.mkString("\u0000")))
        .headOption
        .getOrElse(Nil)

  private def missingRequiredTokens(requiredTokens: List[String], values: List[String]): List[String] =
    requiredTokens.filterNot(token => tokenSatisfied(token, values))

  private def semanticRubricSlotRows(diagnostic: CandidateComparisonDiagnostic): List[SemanticRubricSlotRow] =
    val publicRows =
      diagnostic.moveJudgmentView.publicMoveMeaningClaimDiagnostics.map(claim =>
        semanticRubricSlotRow(diagnostic, claim.unit, claim.axisKey)
      )
    val viewOnlyRows =
      diagnostic.moveJudgmentView.positionPlanTechniqueUnits
        .filterNot(unit => publicRows.exists(_.unit == unit))
        .map(unit => semanticRubricSlotRow(diagnostic, unit, None))
    (publicRows ++ viewOnlyRows)
      .distinctBy(row => (row.comparisonId, row.unit, row.axisKey))
      .sortBy(row => (row.comparisonId, row.unit.toString, row.axisKey.getOrElse("")))

  private def semanticRubricSlotRow(
      diagnostic: CandidateComparisonDiagnostic,
      unit: PositionPlanTechniqueUnit,
      axisKey: Option[String]
  ): SemanticRubricSlotRow =
    val view = diagnostic.moveJudgmentView
    val frameIds = view.positionPlanTechniqueFrameIds
    val frameCauseIds = view.positionPlanTechniqueRelativeCauseEvidenceIds.toSet
    val frameAxisKeys = view.positionPlanTechniqueAxisKeys.toSet
    val detailUnits = view.positionPlanTechniqueSemanticDetailUnits.toSet
    val detailAxisKeys = view.positionPlanTechniqueSemanticDetailAxisKeys.toSet
    val detailMechanismKinds = view.positionPlanTechniqueSemanticDetailMechanismKinds
    val detailSemanticAnchorKeys = view.positionPlanTechniqueSemanticDetailAnchorKeys
    val semanticDetailTokens = view.positionPlanTechniqueSemanticDetailTokens
    val semanticDetailTokenGroups = view.positionPlanTechniqueSemanticDetailTokenGroups
    val objectBindingSignatures = view.positionPlanTechniqueObjectBindingSignatures
    val unitSurfaced = view.positionPlanTechniqueUnits.contains(unit)
    val rootCauseIds = view.primaryRootCauseEvidenceIds.toSet
    val causeIds = frameCauseIds.toList.distinct.sorted
    val frameCauseFlows =
      diagnostic.relativeCauseDiagnostics.causeFlow.filter(flow => frameCauseIds.contains(flow.causeId))
    val decodedDetailSurfaced =
      detailUnits.contains(unit) &&
        (axisKey.forall(detailAxisKeys.contains) || axisKey.isEmpty)
    val exactAxisOrPattern =
      axisKey.exists(frameAxisKeys.contains) ||
        (axisKey.isEmpty && unitSurfaced)
    val objectBound =
      objectBindingSignatures.nonEmpty
    val publicSurfaceClaimDiagnostics =
      view.publicMoveMeaningClaimDiagnostics.filter(diagnostic =>
        semanticRubricSurfaceClaimMatches(diagnostic, unit, axisKey)
      )
    val publicSurfaceClaimWithCarrier =
      publicSurfaceClaimDiagnostics.filter(_.hasCarrier)
    val publicSurfaceStage =
      publicSurfaceClaimWithCarrier.headOption
        .map(_.supportLevel)
        .filter(_.nonEmpty)
        .orElse(publicSurfaceClaimDiagnostics.headOption.map(_ => "no_public_carrier"))
        .getOrElse(if unitSurfaced then "view_only" else "missing_semantic_slot")
    val ownedCauseLinked =
      publicSurfaceClaimWithCarrier.exists(claim =>
        claim.supportLevel == "owned_cause_linked" &&
          claim.causeEvidenceIds.exists(frameCauseIds.contains)
      )
    val causeOwned =
      ownedCauseLinked || frameCauseIds.exists(rootCauseIds.contains)
    val claimSurvived =
      publicSurfaceClaimDiagnostics.nonEmpty
    val viewSurfaced =
      publicSurfaceClaimWithCarrier.nonEmpty &&
        unitSurfaced &&
        axisKey.forall(frameAxisKeys.contains)
    val clusteredCoherent =
      viewSurfaced &&
        decodedDetailSurfaced &&
        frameIds.nonEmpty &&
        claimSurvived &&
        ownedCauseLinked
    val strictCauseLineageBound =
      semanticRubricCauseLineageBound(causeIds, semanticDetailTokenGroups)
    val strictPrimaryRootLineageBound =
      semanticRubricPrimaryRootLineageBound(view.primaryRootCauseEvidenceIds, semanticDetailTokenGroups)
    val strictPrimaryRootTierLineageBound =
      semanticRubricPrimaryRootTierLineageBound(
        view.primaryRootCauseEvidenceTiers,
        semanticDetailTokenGroups
      )
    SemanticRubricSlotRow(
      comparisonId = diagnostic.id,
      unit = unit,
      axisKey = axisKey,
      terminalStage = publicSurfaceStage,
      strictLineageTerminalStage = publicSurfaceStage,
      objectBound = objectBound,
      exactAxisOrPattern = exactAxisOrPattern,
      causeOwned = causeOwned,
      claimSurvived = claimSurvived,
      viewSurfaced = viewSurfaced,
      ownedCauseLinked = ownedCauseLinked,
      clusteredCoherent = clusteredCoherent,
      strictCauseLineageBound = strictCauseLineageBound,
      strictPrimaryRootLineageBound = strictPrimaryRootLineageBound,
      strictPrimaryRootTierLineageBound = strictPrimaryRootTierLineageBound,
      frameIds = frameIds,
      detailMechanismKinds = detailMechanismKinds,
      detailSemanticAnchorKeys = detailSemanticAnchorKeys,
      semanticDetailTokens = semanticDetailTokens,
      semanticDetailTokenGroups = semanticDetailTokenGroups,
      objectBindingSignatures = objectBindingSignatures,
      sourceEvidenceIds = view.positionPlanTechniqueEvidenceIds.distinct.sorted,
      causeKinds = frameCauseFlows.map(_.causeKind).distinct.sortBy(_.toString),
      primaryRootCauseKinds = view.primaryRootCauseKinds.distinct.sortBy(_.toString),
      primaryRootCauseEvidenceIds = view.primaryRootCauseEvidenceIds.distinct.sorted,
      primaryRootArbitrationTiers = view.primaryRootArbitrationTiers.distinct.sortBy(_.toString),
      primaryRootCauseEvidenceTiers = view.primaryRootCauseEvidenceTiers.distinct.sortBy(tier => (tier.causeEvidenceId, tier.tier.toString)),
      causeIds = causeIds,
      causeIdKinds =
        frameCauseFlows
          .map(flow => flow.causeId -> flow.causeKind)
          .distinct
          .sortBy((causeId, kind) => (causeId, kind.toString)),
      claimIds = frameCauseFlows.flatMap(_.claimIds).distinct.sorted
    )

  private def semanticRubricSurfaceClaimMatches(
      diagnostic: PublicMoveMeaningClaimDiagnostic,
      unit: PositionPlanTechniqueUnit,
      axisKey: Option[String]
  ): Boolean =
    diagnostic.unit == unit &&
      axisKey.forall(diagnostic.axisKey.contains)

  private def axisPart(axisKey: String, index: Int): String =
    axisKey.split(":", 3).lift(index).getOrElse("")

  private def planCoherenceAxis(axisKey: String): Boolean =
    axisPart(axisKey, 0) == StrategicAxisKind.PlanCoherence.toString

  private def referenceAxisCanExplainStructuralLoss(axisKey: String): Boolean =
    axisPart(axisKey, 1) match
      case "Loss" | "Release" | "Concede" => false
      case _                              => true

  private def candidateAxisCanExplainStructuralLoss(axisKey: String): Boolean =
    axisPart(axisKey, 1) match
      case "Loss" | "Release" | "Concede" => true
      case _                              => false

  private def diagnosticFlows(
      diagnostics: List[CandidateComparisonDiagnostic]
  ): List[(CandidateComparisonDiagnostic, RelativeCauseFlowDiagnostic)] =
    diagnostics.flatMap(diagnostic => diagnostic.relativeCauseDiagnostics.causeFlow.map(diagnostic -> _))

  private def stageCountsJson(entries: List[(CandidateComparisonDiagnostic, RelativeCauseFlowDiagnostic)]): JsObject =
    Json.obj(
      "causeProduced" -> entries.size,
      "ownedDepthOrAdmissibleProof" -> entries.count((_, flow) =>
        flow.hasOwnedTypedDepth || flow.hasOwnedAdmissibleLongTermProof
      ),
      "ideaCreated" -> entries.count((_, flow) => !flow.causeWithoutIdea),
      "claimCandidateCreated" -> entries.count((_, flow) => flow.claimCandidateIds.nonEmpty),
      "truthCertified" -> entries.count((_, flow) =>
        flow.claimCandidateTruthStatuses.contains(ClaimLifecycleTruthStatus.Certified)
      ),
      "finalClaimIncluded" -> entries.count((_, flow) => flow.claimIds.nonEmpty),
      "eventClusterCreated" -> entries.count((_, flow) => flow.eventClusterIds.nonEmpty),
      "moveJudgmentViewSurfaced" -> entries.count((diagnostic, flow) => flowSurfacedInMoveJudgmentView(diagnostic, flow))
    )

  private def structuralOpportunity(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.relativeCauseDiagnostics.causeFlow.exists(noEventCauseFlow) ||
      diagnostic.decisionTrace.semanticAxisDiagnostics.referenceLeadAxes.nonEmpty ||
      diagnostic.decisionTrace.semanticAxisDiagnostics.referenceOnlyAxes.nonEmpty ||
      diagnostic.decisionTrace.semanticAxisDiagnostics.candidateLeadAxes.nonEmpty ||
      diagnostic.decisionTrace.semanticAxisDiagnostics.candidateOnlyAxes.nonEmpty

  def noEventCauseFlow(flow: RelativeCauseFlowDiagnostic): Boolean =
    ClaimEventCluster.kindForCause(flow.causeKind).isEmpty

  private def flowSurfacedInMoveJudgmentView(
      diagnostic: CandidateComparisonDiagnostic,
      flow: RelativeCauseFlowDiagnostic
  ): Boolean =
    val surfacedIds =
      diagnostic.moveJudgmentView.primaryCauseEvidenceIds ++
        diagnostic.moveJudgmentView.secondaryCauseEvidenceIds ++
        diagnostic.moveJudgmentView.contextCauseEvidenceIds
    surfacedIds.contains(flow.causeId)

  def relativeCauseClaimDroppedComparisonIds(
      diagnostics: List[CandidateComparisonDiagnostic],
      stage: ClaimLifecycleStage
  ): List[String] =
    diagnostics
      .filter(_.relativeCauseDiagnostics.causeFlow.exists(_.claimCandidateDroppedStages.contains(stage)))
      .map(_.id)

  private def stringCountsJson(values: List[String]): JsObject =
    MoveReviewPhase3AuditMetrics.stringCountsJson(values)


