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
    val boardCarrierTotal =
      publicClaimDiagnostics.map(_.boardCarriers.size).sum
    val boardCarrierSaturatedPublicClaimCount =
      publicClaimDiagnostics.count(_.boardCarriers.size >= 12)
    val targetObjectPublicClaimCount =
      publicClaimDiagnostics.count(diagnostic =>
        diagnostic.targetSquares.nonEmpty || diagnostic.targetFiles.nonEmpty || diagnostic.targetPieces.nonEmpty
      )
    val targetObjectWithoutBoardCarrierPublicClaimCount =
      publicClaimDiagnostics.count(diagnostic =>
        !diagnostic.hasBoardCarrier &&
          (diagnostic.targetSquares.nonEmpty || diagnostic.targetFiles.nonEmpty || diagnostic.targetPieces.nonEmpty)
      )
    Json.obj(
      "publicMoveMeaningClaimCount" -> publicClaimDiagnostics.size,
      "boardCarrierlessPublicMoveMeaningClaimCount" -> boardCarrierlessPublicClaimCount,
      "publicMoveMeaningBoardCarrierTotal" -> boardCarrierTotal,
      "boardCarrierSaturatedPublicMoveMeaningClaimCount" -> boardCarrierSaturatedPublicClaimCount,
      "targetObjectPublicMoveMeaningClaimCount" -> targetObjectPublicClaimCount,
      "targetObjectWithoutBoardCarrierPublicMoveMeaningClaimCount" -> targetObjectWithoutBoardCarrierPublicClaimCount,
      "multiEvidenceInputPublicClaimCount" -> multiInputPublicClaimCount
    )

  private[qc] def moveMeaningSurfaceCorpusDiagnosticsJson(coverages: List[JsValue]): JsObject =
    val publicClaimCount =
      coverages.map(coverage => (coverage \ "publicMoveMeaningClaimCount").as[Int]).sum
    val boardCarrierlessPublicClaimCount =
      coverages.map(coverage => (coverage \ "boardCarrierlessPublicMoveMeaningClaimCount").as[Int]).sum
    val multiInputPublicClaimCount =
      coverages.map(coverage => (coverage \ "multiEvidenceInputPublicClaimCount").as[Int]).sum
    val boardCarrierTotal =
      coverages.map(coverage => (coverage \ "publicMoveMeaningBoardCarrierTotal").asOpt[Int].getOrElse(0)).sum
    val boardCarrierSaturatedPublicClaimCount =
      coverages.map(coverage => (coverage \ "boardCarrierSaturatedPublicMoveMeaningClaimCount").asOpt[Int].getOrElse(0)).sum
    val targetObjectPublicClaimCount =
      coverages.map(coverage => (coverage \ "targetObjectPublicMoveMeaningClaimCount").asOpt[Int].getOrElse(0)).sum
    val targetObjectWithoutBoardCarrierPublicClaimCount =
      coverages
        .map(coverage => (coverage \ "targetObjectWithoutBoardCarrierPublicMoveMeaningClaimCount").asOpt[Int].getOrElse(0))
        .sum
    Json.obj(
      "publicMoveMeaningClaimCount" -> publicClaimCount,
      "boardCarrierlessPublicMoveMeaningClaimCount" -> boardCarrierlessPublicClaimCount,
      "publicMoveMeaningBoardCarrierTotal" -> boardCarrierTotal,
      "boardCarrierSaturatedPublicMoveMeaningClaimCount" -> boardCarrierSaturatedPublicClaimCount,
      "targetObjectPublicMoveMeaningClaimCount" -> targetObjectPublicClaimCount,
      "targetObjectWithoutBoardCarrierPublicMoveMeaningClaimCount" -> targetObjectWithoutBoardCarrierPublicClaimCount,
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
      frameIds: List[String],
      sourceEvidenceIds: List[String],
      primaryRootCauseEvidenceIds: List[String],
      causeIds: List[String],
      claimIds: List[String],
      publicSurface: Boolean
  )

  private[qc] def semanticRubricExpectedSlotCoverageJson(
      expectedSlots: List[ExpectedSemanticSlot],
      diagnostics: List[CandidateComparisonDiagnostic],
      expectedQuestionIds: List[String] = Nil
  ): JsObject =
    val rows = diagnostics.flatMap(semanticRubricSlotRows)
    val slotRows =
      expectedSlots.map(slot => expectedSemanticSlotCoverage(slot, rows, diagnostics))
    val measuredSlotRows = slotRows.filter(row => (row \ "measured").as[Boolean])
    val questionIds = measuredSlotRows.flatMap(row => (row \ "questionId").asOpt[String]).distinct.sorted
    val matchedQuestionIds =
      measuredSlotRows
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
      measuredSlotRows.filterNot(row => (row \ "matched").as[Boolean]).map(row => (row \ "id").as[String])
    val unmeasuredSlotIds =
      slotRows.filterNot(row => (row \ "measured").as[Boolean]).map(row => (row \ "id").as[String])
    val failedRequiredSupportLevelSlotIds =
      measuredSlotRows.filter(row => !(row \ "supportLevelSatisfied").as[Boolean]).map(row => (row \ "id").as[String])
    Json.obj(
      "classification" -> "audit_only",
      "expectedSlotCount" -> slotRows.size,
      "measuredSlotCount" -> measuredSlotRows.size,
      "matchedSlotCount" -> measuredSlotRows.count(row => (row \ "matched").as[Boolean]),
      "supportLevelCounts" -> stringCountsJson(measuredSlotRows.map(row => (row \ "supportLevel").as[String])),
      "unmeasuredSlotIds" -> unmeasuredSlotIds,
      "unmeasuredUniqueSlotIds" -> unmeasuredSlotIds.distinct.sorted,
      "unmeasuredSlotIdCounts" -> stringCountsJson(unmeasuredSlotIds),
      "missingSlotIds" -> missingSlotIds,
      "missingUniqueSlotIds" -> missingSlotIds.distinct.sorted,
      "missingSlotIdCounts" -> stringCountsJson(missingSlotIds),
      "failedRequiredSupportLevelSlotIds" -> failedRequiredSupportLevelSlotIds,
      "failedRequiredSupportLevelUniqueSlotIds" -> failedRequiredSupportLevelSlotIds.distinct.sorted,
      "failedRequiredSupportLevelSlotIdCounts" -> stringCountsJson(failedRequiredSupportLevelSlotIds),
      "survivalFailureClassCounts" -> stringCountsJson(slotRows.map(survivalFailureClass)),
      "survivalFailureSlotIds" -> survivalFailureSlotIdsJson(slotRows),
      "survivalFailureQuestionIds" ->
        survivalFailureQuestionIdsJson(slotRows, unmeasuredExpectedQuestionIds),
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
      coverages.flatMap(coverage => (coverage \ "slots").as[List[JsObject]])
    val measuredSlotRows = slotRows.filter(row => (row \ "measured").as[Boolean])
    val questionIds =
      (measuredSlotRows.flatMap(row => (row \ "questionId").asOpt[String]) ++
        coverages.flatMap(coverage => (coverage \ "questionIds").as[List[String]]))
        .distinct
        .sorted
    val matchedQuestionIds =
      measuredSlotRows
        .filter(row => (row \ "matched").as[Boolean])
        .flatMap(row => (row \ "questionId").asOpt[String])
        .distinct
        .sorted
    val expectedQuestions =
      coverages
        .flatMap(coverage => (coverage \ "expectedQuestionIds").as[List[String]])
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
      measuredSlotRows.filterNot(row => (row \ "matched").as[Boolean]).map(row => (row \ "id").as[String])
    val unmeasuredSlotIds =
      slotRows.filterNot(row => (row \ "measured").as[Boolean]).map(row => (row \ "id").as[String])
    val failedRequiredSupportLevelSlotIds =
      measuredSlotRows.filter(row => !(row \ "supportLevelSatisfied").as[Boolean]).map(row => (row \ "id").as[String])
    Json.obj(
      "classification" -> "audit_only",
      "coverageRowCount" -> coverages.size,
      "expectedSlotCount" -> slotRows.size,
      "measuredSlotCount" -> measuredSlotRows.size,
      "matchedSlotCount" -> measuredSlotRows.count(row => (row \ "matched").as[Boolean]),
      "supportLevelCounts" -> stringCountsJson(measuredSlotRows.map(row => (row \ "supportLevel").as[String])),
      "unmeasuredSlotIds" -> unmeasuredSlotIds,
      "unmeasuredUniqueSlotIds" -> unmeasuredSlotIds.distinct.sorted,
      "unmeasuredSlotIdCounts" -> stringCountsJson(unmeasuredSlotIds),
      "missingSlotIds" -> missingSlotIds,
      "missingUniqueSlotIds" -> missingSlotIds.distinct.sorted,
      "missingSlotIdCounts" -> stringCountsJson(missingSlotIds),
      "failedRequiredSupportLevelSlotIds" -> failedRequiredSupportLevelSlotIds,
      "failedRequiredSupportLevelUniqueSlotIds" -> failedRequiredSupportLevelSlotIds.distinct.sorted,
      "failedRequiredSupportLevelSlotIdCounts" -> stringCountsJson(failedRequiredSupportLevelSlotIds),
      "survivalFailureClassCounts" -> stringCountsJson(slotRows.map(survivalFailureClass)),
      "survivalFailureSlotIds" -> survivalFailureSlotIdsJson(slotRows),
      "survivalFailureQuestionIds" ->
        survivalFailureQuestionIdsJson(slotRows, unmeasuredExpectedQuestionIds),
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
            "supportLevelCounts" -> stringCountsJson(rows.map(row => (row \ "supportLevel").as[String])),
            "survivalFailureClassCounts" -> stringCountsJson(rows.map(survivalFailureClass)),
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
      rows: List[SemanticRubricSlotRow],
      diagnostics: List[CandidateComparisonDiagnostic]
  ): JsObject =
    val diagnosticsById = diagnostics.map(diagnostic => diagnostic.id -> diagnostic).toMap
    val measured = true
    val unitEligibleRows =
      rows.filter(row =>
        row.unit == slot.unit &&
          slot.axisKey.forall(expectedAxis => row.axisKey.contains(expectedAxis)) &&
          slot.lineRole.forall(expectedRole => row.lineRole == expectedRole.trim) &&
          slot.moveUci.forall(expectedMove =>
            JudgmentSubjectBinding.normalizeMove(row.moveUci) == JudgmentSubjectBinding.normalizeMove(expectedMove)
          ) &&
          diagnosticsById.get(row.comparisonId).forall(diagnostic => slotSemanticTokensSatisfied(slot, diagnostic))
      )
    val publicMatches = unitEligibleRows.filter(_.publicSurface)
    val claimMatches = unitEligibleRows.filterNot(_.publicSurface)
    val best =
      slot.requiredSupportLevel
        .flatMap(required => publicMatches.find(row => supportLevelSatisfies(row.supportLevel, required)))
        .orElse(publicMatches.headOption)
    val bestClaim =
      slot.requiredSupportLevel
        .flatMap(required => claimMatches.find(row => supportLevelSatisfies(row.supportLevel, required)))
        .orElse(claimMatches.headOption)
    val supportLevel =
      if measured then best.map(_.supportLevel).getOrElse("missing_semantic_slot")
      else "unmeasured_semantic_slot"
    val claimSupportLevel =
      if measured then bestClaim.map(_.supportLevel).getOrElse("missing_semantic_claim")
      else "unmeasured_semantic_slot"
    val supportLevelSatisfied =
      measured && slot.requiredSupportLevel.forall(required => supportLevelSatisfies(supportLevel, required))
    val claimSupportLevelSatisfied =
      measured && slot.requiredSupportLevel.forall(required => supportLevelSatisfies(claimSupportLevel, required))
    val matched = measured && best.nonEmpty && supportLevelSatisfied
    val requiresOwnedCause =
      slot.requiredSupportLevel.exists(required => supportLevelSatisfies(required, "owned_cause_linked"))
    val ownedExpectationWithoutProof =
      requiresOwnedCause &&
        unitEligibleRows.nonEmpty &&
        unitEligibleRows.forall(_.causeIds.isEmpty)
    val requiredDetailTokenAbsent =
      slot.requiredSemanticDetailTokens.nonEmpty &&
        semanticDetailUnitPresent(slot, diagnostics) &&
        !semanticDetailPresent(slot, diagnostics)
    val counterplayRaceExpectationOverspecified =
      slot.unit == PositionPlanTechniqueUnit.CounterplayRace &&
        !semanticDetailUnitPresent(slot, diagnostics) &&
        diagnostics.exists { diagnostic =>
          val groups = diagnostic.moveJudgmentView.positionPlanTechniqueSemanticDetailTokenGroups
          val hasRace = groups.exists(_.contains("unit:CounterplayRace"))
          val hasCounterplayControl =
            groups.exists(tokens =>
              tokens.contains("unit:SpacePreventionResourceDenial") &&
                tokens.contains("axisKind:Counterplay") &&
                (
                  tokens.contains("counterBreak:true") ||
                    tokens.contains("resourceContestKind:CounterplayRestraint") ||
                    tokens.contains("resourceContestScope:counterplay")
                )
            )
          hasCounterplayControl && !hasRace
        }
    val survivalFailureClass =
      if !measured then "measurement_gap"
      else if matched then "covered"
      else if ownedExpectationWithoutProof then "ownership_proof_gap"
      else if requiredDetailTokenAbsent || counterplayRaceExpectationOverspecified then "rubric_input_gap"
      else if best.isEmpty && claimSupportLevelSatisfied then "public_surface_blocked"
      else if claimMatches.nonEmpty || semanticDetailPresent(slot, diagnostics) then "structural_bottleneck"
      else "coverage_shortage"
    Json.obj(
      "id" -> slot.id,
      "measured" -> measured,
      "unit" -> slot.unit.toString,
      "axisKey" -> slot.axisKey,
      "lineRole" -> slot.lineRole,
      "moveUci" -> slot.moveUci,
      "questionId" -> slot.questionId,
      "description" -> slot.description,
      "requiredSupportLevel" -> slot.requiredSupportLevel,
      "requiredSemanticDetailTokens" -> slot.requiredSemanticDetailTokens,
      "matched" -> matched,
      "supportLevel" -> supportLevel,
      "claimSupportLevel" -> claimSupportLevel,
      "lineRole" -> best.map(_.lineRole),
      "moveUci" -> best.map(_.moveUci),
      "supportLevelSatisfied" -> supportLevelSatisfied,
      "claimSupportLevelSatisfied" -> claimSupportLevelSatisfied,
      "claimPresent" -> claimMatches.nonEmpty,
      "publicSurfacePresent" -> publicMatches.nonEmpty,
      "requiresOwnedCause" -> requiresOwnedCause,
      "ownedExpectationWithoutProof" -> ownedExpectationWithoutProof,
      "requiredDetailTokenAbsent" -> requiredDetailTokenAbsent,
      "counterplayRaceExpectationOverspecified" -> counterplayRaceExpectationOverspecified,
      "survivalFailureClass" -> survivalFailureClass,
      "matchedComparisonIds" -> publicMatches.map(_.comparisonId).distinct.sorted,
      "claimComparisonIds" -> claimMatches.map(_.comparisonId).distinct.sorted,
      "frameIds" -> publicMatches.flatMap(_.frameIds).distinct.sorted,
      "sourceEvidenceIds" -> publicMatches.flatMap(_.sourceEvidenceIds).distinct.sorted,
      "primaryRootCauseEvidenceIds" -> publicMatches.flatMap(_.primaryRootCauseEvidenceIds).distinct.sorted,
      "causeIds" -> publicMatches.flatMap(_.causeIds).distinct.sorted,
      "claimIds" -> publicMatches.flatMap(_.claimIds).distinct.sorted,
      "bestLineageTrace" -> best.map(semanticRubricLineageTraceJson).getOrElse(Json.obj()),
      "bestClaimLineageTrace" -> bestClaim.map(semanticRubricLineageTraceJson).getOrElse(Json.obj()),
      "lineageTraces" -> JsArray(
        publicMatches
          .sortBy(_.comparisonId)
          .map(semanticRubricLineageTraceJson)
      ),
      "claimLineageTraces" -> JsArray(
        claimMatches
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
      "frameIds" -> row.frameIds,
      "sourceEvidenceIds" -> row.sourceEvidenceIds,
      "causeIds" -> row.causeIds,
      "claimIds" -> row.claimIds,
      "primaryRootCauseEvidenceIds" -> row.primaryRootCauseEvidenceIds,
      "publicSurface" -> row.publicSurface
    )

  private def semanticRubricSlotRows(diagnostic: CandidateComparisonDiagnostic): List[SemanticRubricSlotRow] =
    val claimRows =
      diagnostic.moveJudgmentView.moveMeaningClaimDiagnostics
        .map(claim => semanticRubricSlotRow(diagnostic, claim, publicSurface = false))
    val publicRows =
      diagnostic.moveJudgmentView.publicMoveMeaningClaimDiagnostics
        .filter(_.hasCarrier)
        .map(claim => semanticRubricSlotRow(diagnostic, claim, publicSurface = true))
    (claimRows ++ publicRows)
      .distinctBy(row => (row.publicSurface, row.comparisonId, row.unit, row.axisKey, row.lineRole, row.moveUci))
      .sortBy(row =>
        (row.comparisonId, row.unit.toString, row.axisKey.getOrElse(""), row.lineRole, row.moveUci, row.publicSurface)
      )

  private def semanticRubricSlotRow(
      diagnostic: CandidateComparisonDiagnostic,
      claim: PublicMoveMeaningClaimDiagnostic,
      publicSurface: Boolean
  ): SemanticRubricSlotRow =
    val view = diagnostic.moveJudgmentView
    val frameIds = view.positionPlanTechniqueFrameIds
    val claimCauseIds = claim.causeEvidenceIds.distinct.sorted
    val claimCauseIdSet = claimCauseIds.toSet
    val frameCauseFlows =
      diagnostic.relativeCauseDiagnostics.causeFlow.filter(flow => claimCauseIdSet.contains(flow.causeId))
    val publicSurfaceSupportLevel =
      Option(claim.supportLevel).filter(_.nonEmpty).getOrElse("missing_semantic_slot")
    val claimPrimaryRootCauseIds =
      view.primaryRootCauseEvidenceIds.distinct.sorted.filter(claimCauseIdSet)
    SemanticRubricSlotRow(
      comparisonId = diagnostic.id,
      unit = claim.unit,
      axisKey = claim.axisKey,
      supportLevel = publicSurfaceSupportLevel,
      lineRole = claim.lineRole,
      moveUci = claim.moveUci,
      frameIds = frameIds,
      sourceEvidenceIds = claim.sourceEvidenceIds.distinct.sorted,
      primaryRootCauseEvidenceIds = claimPrimaryRootCauseIds,
      causeIds = claimCauseIds,
      claimIds = frameCauseFlows.flatMap(_.claimIds).distinct.sorted,
      publicSurface = publicSurface
    )

  private def semanticDetailPresent(
      slot: ExpectedSemanticSlot,
      diagnostics: List[CandidateComparisonDiagnostic]
  ): Boolean =
    semanticDetailUnitPresent(slot, diagnostics) &&
      diagnostics.exists(diagnostic => slotSemanticTokensSatisfied(slot, diagnostic))

  private def semanticDetailUnitPresent(
      slot: ExpectedSemanticSlot,
      diagnostics: List[CandidateComparisonDiagnostic]
  ): Boolean =
    diagnostics.exists { diagnostic =>
      val view = diagnostic.moveJudgmentView
      view.positionPlanTechniqueSemanticDetailUnits.contains(slot.unit) &&
      slot.axisKey.forall(axis =>
        view.positionPlanTechniqueSemanticDetailAxisKeys.contains(axis) ||
          view.positionPlanTechniqueAxisKeys.contains(axis)
      )
    }

  private def slotSemanticTokensSatisfied(
      slot: ExpectedSemanticSlot,
      diagnostic: CandidateComparisonDiagnostic
  ): Boolean =
    slot.requiredSemanticDetailTokens.isEmpty ||
      diagnostic.moveJudgmentView.positionPlanTechniqueSemanticDetailTokenGroups.exists(tokens =>
        tokens.contains(s"unit:${slot.unit}") &&
          slot.requiredSemanticDetailTokens.forall(requiredSemanticToken(tokens, _))
      )

  private def requiredSemanticToken(tokens: List[String], required: String): Boolean =
    val expected = required.trim
    expected.isEmpty ||
      tokens.contains(expected) ||
      tokens.exists(_.startsWith(s"$expected:")) ||
      (expected.endsWith(":") && tokens.exists(_.startsWith(expected)))

  private def supportLevelSatisfies(actual: String, required: String): Boolean =
    supportLevelRank(actual) >= supportLevelRank(required)

  private def supportLevelRank(level: String): Int =
    level.trim match
      case "owned_cause_linked"  => 3
      case "clustered_coherent"  => 2
      case "view_surfaced"       => 1
      case "missing_semantic_slot" | "missing_semantic_claim" | "unmeasured_semantic_slot" => 0
      case _                    => 0

  private def survivalFailureClass(row: JsObject): String =
    (row \ "survivalFailureClass").as[String]

  private def survivalFailureSlotIdsJson(slotRows: List[JsObject]): JsObject =
    JsObject(
      slotRows
        .groupBy(survivalFailureClass)
        .toList
        .sortBy(_._1)
        .map { case (failureClass, rows) =>
          failureClass -> JsArray(rows.map(row => JsString((row \ "id").as[String])).distinct)
        }
    )

  private def survivalFailureQuestionIdsJson(
      slotRows: List[JsObject],
      unmeasuredExpectedQuestionIds: List[String]
  ): JsObject =
    val missingQuestionIdsByClass =
      slotRows
        .filterNot(row => (row \ "matched").as[Boolean])
        .flatMap(row => (row \ "questionId").asOpt[String].map(survivalFailureClass(row) -> _))
        .groupMap(_._1)(_._2)
    val measurementGapQuestionIds =
      (missingQuestionIdsByClass.getOrElse("measurement_gap", Nil) ++ unmeasuredExpectedQuestionIds).distinct.sorted
    JsObject(
      (missingQuestionIdsByClass + ("measurement_gap" -> measurementGapQuestionIds))
        .toList
        .sortBy(_._1)
        .map((failureClass, questionIds) => failureClass -> Json.toJson(questionIds.distinct.sorted))
    )

  def noEventCauseFlow(flow: RelativeCauseFlowDiagnostic): Boolean =
    ClaimEventCluster.kindForCause(flow.causeKind).isEmpty

  private def stringCountsJson(values: List[String]): JsObject =
    MoveReviewPhase3AuditMetrics.stringCountsJson(values)
