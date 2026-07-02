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
private[qc] final class MoveReviewPhase3AuditFunnelMetrics:
  private final case class SemanticRubricSlotRow(
      comparisonId: String,
      unit: PositionPlanTechniqueUnit,
      axisKey: Option[String],
      supportLevel: String,
      lineRole: String,
      moveUci: String,
      objectBound: Boolean,
      exactAxisOrPattern: Boolean,
      causeOwned: Boolean,
      claimSurvived: Boolean,
      viewSurfaced: Boolean,
      ownedCauseLinked: Boolean,
      clusteredCoherent: Boolean,
      strictCauseLineageBound: Boolean,
      frameIds: List[String],
      sourceEvidenceIds: List[String],
      primaryRootCauseEvidenceIds: List[String],
      causeIds: List[String],
      claimIds: List[String]
  )

  private[qc] def semanticRubricExpectedSlotCoverageJson(
      expectedSlots: List[ExpectedSemanticSlot],
      diagnostics: List[CandidateComparisonDiagnostic],
      expectedQuestionIds: List[String] = Nil
  ): JsObject =
    val rows = diagnostics.flatMap(semanticRubricSlotRows)
    val slotRows =
      expectedSlots.map(slot => expectedSemanticSlotCoverage(slot, rows))
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
    val failedRequiredSupportLevelSlotIds =
      slotRows.filter(row => !(row \ "supportLevelSatisfied").as[Boolean]).map(row => (row \ "id").as[String])
    Json.obj(
      "classification" -> "audit_only",
      "expectedSlotCount" -> slotRows.size,
      "matchedSlotCount" -> slotRows.count(row => (row \ "matched").as[Boolean]),
      "viewSurfacedCount" -> slotRows.count(row => (row \ "viewSurfaced").as[Boolean]),
      "ownedCauseLinkedCount" -> slotRows.count(row => (row \ "ownedCauseLinked").as[Boolean]),
      "clusteredCoherentCount" -> slotRows.count(row => (row \ "clusteredCoherent").as[Boolean]),
      "supportLevelCounts" -> stringCountsJson(slotRows.map(row => (row \ "supportLevel").as[String])),
      "missingSlotIds" -> missingSlotIds,
      "missingUniqueSlotIds" -> missingSlotIds.distinct.sorted,
      "missingSlotIdCounts" -> stringCountsJson(missingSlotIds),
      "failedRequiredSupportLevelSlotIds" -> failedRequiredSupportLevelSlotIds,
      "failedRequiredSupportLevelUniqueSlotIds" -> failedRequiredSupportLevelSlotIds.distinct.sorted,
      "failedRequiredSupportLevelSlotIdCounts" -> stringCountsJson(failedRequiredSupportLevelSlotIds),
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
    val failedRequiredSupportLevelSlotIds =
      slotRows.filter(row => !(row \ "supportLevelSatisfied").as[Boolean]).map(row => (row \ "id").as[String])
    Json.obj(
      "classification" -> "audit_only",
      "coverageRowCount" -> coverages.size,
      "expectedSlotCount" -> slotRows.size,
      "matchedSlotCount" -> slotRows.count(row => (row \ "matched").as[Boolean]),
      "viewSurfacedCount" -> slotRows.count(row => (row \ "viewSurfaced").as[Boolean]),
      "ownedCauseLinkedCount" -> slotRows.count(row => (row \ "ownedCauseLinked").as[Boolean]),
      "clusteredCoherentCount" -> slotRows.count(row => (row \ "clusteredCoherent").as[Boolean]),
      "supportLevelCounts" -> stringCountsJson(slotRows.map(row => (row \ "supportLevel").as[String])),
      "missingSlotIds" -> missingSlotIds,
      "missingUniqueSlotIds" -> missingSlotIds.distinct.sorted,
      "missingSlotIdCounts" -> stringCountsJson(missingSlotIds),
      "failedRequiredSupportLevelSlotIds" -> failedRequiredSupportLevelSlotIds,
      "failedRequiredSupportLevelUniqueSlotIds" -> failedRequiredSupportLevelSlotIds.distinct.sorted,
      "failedRequiredSupportLevelSlotIdCounts" -> stringCountsJson(failedRequiredSupportLevelSlotIds),
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
            "supportLevelCounts" -> stringCountsJson(rows.map(row => (row \ "supportLevel").as[String])),
            "missingSlotIds" -> missingSlotIds,
            "missingUniqueSlotIds" -> missingSlotIds.distinct.sorted,
            "missingSlotIdCounts" -> stringCountsJson(missingSlotIds),
            "slotIds" -> rows.map(row => (row \ "id").as[String]).distinct.sorted,
            "units" -> rows.map(row => (row \ "unit").as[String]).distinct.sorted
          )
        }
    )

  private def expectedSemanticSlotCoverage(
      slot: ExpectedSemanticSlot,
      rows: List[SemanticRubricSlotRow]
  ): JsObject =
    val unitEligibleRows =
      rows.filter(row =>
        row.unit == slot.unit &&
          slot.axisKey.forall(expectedAxis => row.axisKey.contains(expectedAxis))
      )
    val matches = unitEligibleRows
    val best =
      slot.requiredSupportLevel
        .flatMap(required => matches.find(_.supportLevel == required.trim))
        .orElse(matches.headOption)
    val supportLevel = best.map(_.supportLevel).getOrElse("missing_semantic_slot")
    val supportLevelSatisfied =
      slot.requiredSupportLevel.forall(required => supportLevel == required.trim)
    val matched = best.nonEmpty && supportLevelSatisfied
    Json.obj(
      "id" -> slot.id,
      "unit" -> slot.unit.toString,
      "axisKey" -> slot.axisKey,
      "questionId" -> slot.questionId,
      "description" -> slot.description,
      "requiredSupportLevel" -> slot.requiredSupportLevel,
      "matched" -> matched,
      "supportLevel" -> supportLevel,
      "lineRole" -> best.map(_.lineRole),
      "moveUci" -> best.map(_.moveUci),
      "supportLevelSatisfied" -> supportLevelSatisfied,
      "objectBound" -> best.exists(_.objectBound),
      "exactAxisOrPattern" -> best.exists(_.exactAxisOrPattern),
      "causeOwned" -> best.exists(_.causeOwned),
      "claimSurvived" -> best.exists(_.claimSurvived),
      "viewSurfaced" -> best.exists(_.viewSurfaced),
      "ownedCauseLinked" -> best.exists(_.ownedCauseLinked),
      "clusteredCoherent" -> best.exists(_.clusteredCoherent),
      "matchedComparisonIds" -> matches.map(_.comparisonId).distinct.sorted,
      "frameIds" -> matches.flatMap(_.frameIds).distinct.sorted,
      "sourceEvidenceIds" -> matches.flatMap(_.sourceEvidenceIds).distinct.sorted,
      "primaryRootCauseEvidenceIds" -> matches.flatMap(_.primaryRootCauseEvidenceIds).distinct.sorted,
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
      "supportLevel" -> row.supportLevel,
      "lineRole" -> row.lineRole,
      "moveUci" -> row.moveUci,
      "strictCauseLineageBound" -> row.strictCauseLineageBound,
      "frameIds" -> row.frameIds,
      "sourceEvidenceIds" -> row.sourceEvidenceIds,
      "causeIds" -> row.causeIds,
      "claimIds" -> row.claimIds,
      "primaryRootCauseEvidenceIds" -> row.primaryRootCauseEvidenceIds
    )

  private def semanticRubricSlotRows(diagnostic: CandidateComparisonDiagnostic): List[SemanticRubricSlotRow] =
    val publicRows =
      diagnostic.moveJudgmentView.publicMoveMeaningClaimDiagnostics
        .filter(_.hasCarrier)
        .map(claim => semanticRubricSlotRow(diagnostic, claim))
    publicRows
      .distinctBy(row => (row.comparisonId, row.unit, row.axisKey, row.lineRole, row.moveUci))
      .sortBy(row => (row.comparisonId, row.unit.toString, row.axisKey.getOrElse(""), row.lineRole, row.moveUci))

  private def semanticRubricSlotRow(
      diagnostic: CandidateComparisonDiagnostic,
      claim: PublicMoveMeaningClaimDiagnostic
  ): SemanticRubricSlotRow =
    val view = diagnostic.moveJudgmentView
    val frameIds = view.positionPlanTechniqueFrameIds
    val claimCauseIds = claim.causeEvidenceIds.distinct.sorted
    val claimCauseIdSet = claimCauseIds.toSet
    val frameCauseFlows =
      diagnostic.relativeCauseDiagnostics.causeFlow.filter(flow => claimCauseIdSet.contains(flow.causeId))
    val exactAxisOrPattern =
      claim.axisKey.nonEmpty || view.positionPlanTechniqueUnits.contains(claim.unit)
    val objectBound =
      claim.hasBoardCarrier
    val publicSurfaceSupportLevel =
      Option(claim.supportLevel).filter(_.nonEmpty).getOrElse("missing_semantic_slot")
    val ownedCauseLinked =
      publicSurfaceSupportLevel == "owned_cause_linked"
    val causeOwned =
      claimCauseIds.nonEmpty
    val claimSurvived =
      true
    val viewSurfaced =
      true
    val clusteredCoherent =
      publicSurfaceSupportLevel == "clustered_coherent"
    val strictCauseLineageBound =
      claimCauseIds.nonEmpty
    val claimPrimaryRootCauseIds =
      view.primaryRootCauseEvidenceIds.distinct.sorted.filter(claimCauseIdSet)
    SemanticRubricSlotRow(
      comparisonId = diagnostic.id,
      unit = claim.unit,
      axisKey = claim.axisKey,
      supportLevel = publicSurfaceSupportLevel,
      lineRole = claim.lineRole,
      moveUci = claim.moveUci,
      objectBound = objectBound,
      exactAxisOrPattern = exactAxisOrPattern,
      causeOwned = causeOwned,
      claimSurvived = claimSurvived,
      viewSurfaced = viewSurfaced,
      ownedCauseLinked = ownedCauseLinked,
      clusteredCoherent = clusteredCoherent,
      strictCauseLineageBound = strictCauseLineageBound,
      frameIds = frameIds,
      sourceEvidenceIds = claim.sourceEvidenceIds.distinct.sorted,
      primaryRootCauseEvidenceIds = claimPrimaryRootCauseIds,
      causeIds = claimCauseIds,
      claimIds = frameCauseFlows.flatMap(_.claimIds).distinct.sorted
    )

  def noEventCauseFlow(flow: RelativeCauseFlowDiagnostic): Boolean =
    ClaimEventCluster.kindForCause(flow.causeKind).isEmpty

  private def stringCountsJson(values: List[String]): JsObject =
    MoveReviewPhase3AuditMetrics.stringCountsJson(values)
