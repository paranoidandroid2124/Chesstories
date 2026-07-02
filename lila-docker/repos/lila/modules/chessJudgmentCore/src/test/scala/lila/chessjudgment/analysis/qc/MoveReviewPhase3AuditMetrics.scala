package lila.chessjudgment.analysis.qc

import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*
import MoveReviewPhase3AuditContract.ExpectedSemanticSlot

private[qc] object MoveReviewPhase3AuditMetrics:
  def rootArbitrationQualitySummaryJson(
      diagnostics: List[CandidateComparisonDiagnostic]
  ): JsObject =
    val fallbackRootDiagnostics =
      diagnostics.filter(_.moveJudgmentView.primaryRootArbitrationTiers.contains(MoveJudgmentCauseRootArbitrationTier.FallbackRoot))
    val broadOwnedDiagnostics =
      diagnostics.filter(_.moveJudgmentView.rootArbitrationTiers.contains(MoveJudgmentCauseRootArbitrationTier.BroadOwnedRoot))
    val contextOnlyDiagnostics =
      diagnostics.filter(_.moveJudgmentView.rootArbitrationTiers.contains(MoveJudgmentCauseRootArbitrationTier.ContextOnly))
    Json.obj(
      "classification" -> "audit_only",
      "comparisonCount" -> diagnostics.size,
      "tierCounts" -> stringCountsJson(diagnostics.flatMap(_.moveJudgmentView.rootArbitrationTiers.map(_.toString))),
      "primaryRootTierCounts" ->
        stringCountsJson(diagnostics.flatMap(_.moveJudgmentView.primaryRootArbitrationTiers.map(_.toString))),
      "fallbackRootComparisonIds" -> fallbackRootDiagnostics.map(_.id).distinct.sorted,
      "fallbackRootComparisonCount" -> fallbackRootDiagnostics.map(_.id).distinct.size,
      "broadOwnedRootComparisonIds" -> broadOwnedDiagnostics.map(_.id).distinct.sorted,
      "broadOwnedRootComparisonCount" -> broadOwnedDiagnostics.map(_.id).distinct.size,
      "contextOnlyComparisonIds" -> contextOnlyDiagnostics.map(_.id).distinct.sorted,
      "contextOnlyComparisonCount" -> contextOnlyDiagnostics.map(_.id).distinct.size
    )

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

  def bindingWidthAuditJson(view: MoveJudgmentView): JsObject =
    bindingWidthAuditJson(Some(view))

  def bindingWidthAuditJson(view: Option[MoveJudgmentView]): JsObject =
    val frames =
      view.toList.flatMap(view => view.causeAudit.primary ++ view.causeAudit.secondary ++ view.causeAudit.context)
    val sideOnlyTargetFrames =
      frames.filter(frame => frame.objectBindingSignatures.exists(sideOnlyTargetSignature))
    val contextSupportOnlyFrames =
      frames.filter(frame =>
        frame.objectBindingSignatures.nonEmpty &&
          frame.objectBindingSignatures.forall(signature => signatureParts(signature).contains("proof=ContextSupport"))
      )
    val playerFacingObjectReadyFrames =
      frames.filter(frame => EvidenceObjectBinding.playerFacingReadySignatures(frame.objectBindingSignatures))
    val targetOnlyObjectBindingFrames =
      frames.filter(frame =>
        !EvidenceObjectBinding.playerFacingReadySignatures(frame.objectBindingSignatures) &&
          frame.objectBindingSignatures.exists(signature =>
            EvidenceObjectBinding.signatureTokens(List(signature), "target=").exists(EvidenceObjectBinding.concreteTargetToken)
          )
      )
    val weakConcreteReadyFrames =
      frames.filter(frame =>
        frame.concreteObjectReady &&
          !EvidenceObjectBinding.playerFacingReadySignatures(frame.objectBindingSignatures)
      )
    val axisObjectFingerprints =
      frames.flatMap(frame =>
        val fingerprints = frame.objectBindingSignatures.map(objectFingerprint).filter(_.nonEmpty).distinct
        frame.proofStrategicAxisKeys.flatMap(axis => fingerprints.map(axis -> _))
      )
    val axisWithMultipleFingerprints =
      axisObjectFingerprints
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.distinct.sorted)
        .toMap
        .filter(_._2.size > 1)
    val witnessFrames =
      frames.filter(_.witnessBindingLevel != MoveJudgmentCauseWitnessBindingLevel.NotWitness)
    val sameComparisonOnlyFrames =
      frames.filter(_.witnessBindingLevel == MoveJudgmentCauseWitnessBindingLevel.SameComparisonOnly)
    Json.obj(
      "classification" -> "audit_only",
      "causeFrameCount" -> frames.size,
      "sideOnlyTargetFrameCount" -> sideOnlyTargetFrames.size,
      "sideOnlyTargetFrameIds" -> sideOnlyTargetFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      "contextSupportOnlyBindingFrameCount" -> contextSupportOnlyFrames.size,
      "contextSupportOnlyBindingFrameIds" -> contextSupportOnlyFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      "playerFacingObjectReadyFrameCount" -> playerFacingObjectReadyFrames.size,
      "targetOnlyObjectBindingFrameCount" -> targetOnlyObjectBindingFrames.size,
      "targetOnlyObjectBindingFrameIds" -> targetOnlyObjectBindingFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      "weakConcreteReadyFrameCount" -> weakConcreteReadyFrames.size,
      "weakConcreteReadyFrameIds" -> weakConcreteReadyFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      "axisWithMultipleObjectFingerprintsCount" -> axisWithMultipleFingerprints.size,
      "axisWithMultipleObjectFingerprints" -> JsObject(
        axisWithMultipleFingerprints.toList.sortBy(_._1).map { case (axis, fingerprints) =>
          axis -> Json.obj(
            "objectFingerprintCount" -> fingerprints.size,
            "objectFingerprintsSample" -> fingerprints.take(5)
          )
        }
      ),
      "witnessBindingFrameCount" -> witnessFrames.size,
      "sameComparisonOnlyBindingFrameCount" -> sameComparisonOnlyFrames.size,
      "sameComparisonOnlyBindingRatio" ->
        (if witnessFrames.isEmpty then 0.0 else sameComparisonOnlyFrames.size.toDouble / witnessFrames.size.toDouble),
      "sameComparisonOnlyBindingFrameIds" -> sameComparisonOnlyFrames.flatMap(_.causeEvidenceIds).distinct.sorted,
      "witnessBindingLevelCounts" -> stringCountsJson(witnessFrames.map(_.witnessBindingLevel.toString)),
      "rootArbitrationTierCounts" -> stringCountsJson(frames.map(_.rootArbitrationTier.toString))
    )

  private def sideOnlyTargetSignature(signature: String): Boolean =
    val targets = signatureTokens(signature, "target")
    targets.nonEmpty && targets.forall(_.startsWith("Side:"))

  private def objectFingerprint(signature: String): String =
    signatureParts(signature)
      .filter(part =>
        part.startsWith("actor=") ||
          part.startsWith("target=") ||
          part.startsWith("mechanism=") ||
          part.startsWith("consequence=") ||
          part.startsWith("witness=")
      )
      .sorted
      .mkString("|")

  private def signatureTokens(signature: String, role: String): List[String] =
    val prefix = s"$role="
    signatureParts(signature).collect { case part if part.startsWith(prefix) => part.stripPrefix(prefix) }

  private[qc] def signatureParts(signature: String): List[String] =
    signature.split("\\|").toList.map(_.trim).filter(_.nonEmpty)

  private[qc] def signatureValues(signature: String, key: String): List[String] =
    signatureParts(signature)
      .collectFirst { case part if part.startsWith(s"$key=") => part.stripPrefix(s"$key=") }
      .toList
      .flatMap(_.split(",").toList)
      .map(_.trim)
      .filter(value => value.nonEmpty && value != "none")

  private def multiOwnerMoveMeaningClaim(signature: String): Boolean =
    signatureValues(signature, "causes").size > 1 ||
      signatureValues(signature, "sources").size > 1

  private[qc] def moveMeaningSurfaceDiagnosticsJson(
      diagnostics: List[CandidateComparisonDiagnostic]
  ): JsObject =
    val internalSignatures =
      diagnostics.flatMap(_.moveJudgmentView.moveMeaningClaimSurfaceSignatures).distinct.sorted
    val publicClaimDiagnostics =
      diagnostics.flatMap(_.moveJudgmentView.publicMoveMeaningClaimDiagnostics).distinct.sortBy(_.signature)
    val publicSignatures = publicClaimDiagnostics.map(_.signature).distinct.sorted
    val suppressedSignatures = internalSignatures.diff(publicSignatures)
    val mergedSignatures = internalSignatures.filter(multiOwnerMoveMeaningClaim)
    val boardCarrierlessPublicSignatures =
      publicClaimDiagnostics.filterNot(_.hasBoardCarrier).map(_.signature).distinct.sorted
    Json.obj(
      "publicMoveMeaningClaimCount" -> publicSignatures.size,
      "boardCarrierlessPublicMoveMeaningClaimCount" -> boardCarrierlessPublicSignatures.size,
      "boardCarrierlessPublicMoveMeaningClaimSignatures" -> boardCarrierlessPublicSignatures,
      "suppressedMoveMeaningClaimCount" -> suppressedSignatures.size,
      "suppressedMoveMeaningClaimSignatures" -> suppressedSignatures,
      "mergedOwnershipClaimCount" -> mergedSignatures.size,
      "mergedOwnershipClaimSignatures" -> mergedSignatures
    )

  private[qc] def moveMeaningSurfaceCorpusDiagnosticsJson(coverages: List[JsValue]): JsObject =
    val publicClaimCount =
      coverages.map(coverage => (coverage \ "publicMoveMeaningClaimCount").asOpt[Int].getOrElse(0)).sum
    val boardCarrierlessPublicSignatures =
      coverages
        .flatMap(coverage =>
          (coverage \ "boardCarrierlessPublicMoveMeaningClaimSignatures").asOpt[List[String]].getOrElse(Nil)
        )
        .distinct
        .sorted
    val suppressedSignatures =
      coverages
        .flatMap(coverage => (coverage \ "suppressedMoveMeaningClaimSignatures").asOpt[List[String]].getOrElse(Nil))
        .distinct
        .sorted
    val mergedSignatures =
      coverages
        .flatMap(coverage => (coverage \ "mergedOwnershipClaimSignatures").asOpt[List[String]].getOrElse(Nil))
        .distinct
        .sorted
    Json.obj(
      "publicMoveMeaningClaimCount" -> publicClaimCount,
      "boardCarrierlessPublicMoveMeaningClaimCount" -> boardCarrierlessPublicSignatures.size,
      "boardCarrierlessPublicMoveMeaningClaimSignatures" -> boardCarrierlessPublicSignatures,
      "suppressedMoveMeaningClaimCount" -> suppressedSignatures.size,
      "suppressedMoveMeaningClaimSignatures" -> suppressedSignatures,
      "mergedOwnershipClaimCount" -> mergedSignatures.size,
      "mergedOwnershipClaimSignatures" -> mergedSignatures
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

  private final case class StructuralOpportunityAxis(
      key: String,
      sourceSide: RelativeCauseSourceSide,
      expectedCauseKind: RelativeCauseKind
  )

  private final case class StructuralOpportunityAxisRow(
      comparisonId: String,
      axisKey: String,
      axisKind: String,
      axisPolarity: String,
      axisLabel: String,
      axisSourceSide: RelativeCauseSourceSide,
      axisSourceIds: List[String],
      axisSourceLayers: List[EvidenceLayer],
      expectedCauseKind: RelativeCauseKind,
      terminalStage: String,
      exactTerminalStage: String,
      causeClass: String,
      exactCauseIds: List[String],
      exactRootCauseEvidenceIds: List[String],
      exactIdeaIds: List[String],
      exactClaimCandidateIds: List[String],
      exactFinalClaimIds: List[String],
      kindOnlyCauseIds: List[String],
      kindOnlyRootCauseEvidenceIds: List[String],
      producedCauseKinds: List[RelativeCauseKind],
      surfacedRootCauseKinds: List[RelativeCauseKind]
  )

  private[qc] def structuralOpportunityGenerationFunnelJson(
      diagnostics: List[CandidateComparisonDiagnostic]
  ): JsObject =
    val rows = diagnostics.filter(primaryActionableStructuralOpportunity).flatMap(structuralOpportunityAxisRows)
    Json.obj(
      "classification" -> "audit_only",
      "axisOpportunityCount" -> rows.size,
      "comparisonCount" -> rows.map(_.comparisonId).distinct.size,
      "comparisonIds" -> rows.map(_.comparisonId).distinct.sorted,
      "terminalStageCounts" -> stringCountsJson(rows.map(_.terminalStage)),
      "causeClassCounts" -> stringCountsJson(rows.map(_.causeClass)),
      "expectedCauseKindCounts" -> stringCountsJson(rows.map(_.expectedCauseKind.toString)),
      "axisExactLineage" -> structuralOpportunityAxisExactLineageJson(rows),
      "fallbackMaskedConcreteAxisComparisonIds" ->
        rows
          .filter(_.terminalStage == "fallback_masked_concrete_axis")
          .map(_.comparisonId)
          .distinct
          .sorted,
      "planCoherenceExactLineage" -> planCoherenceExactLineageFunnelJson(diagnostics),
      "byAxis" -> structuralOpportunityByAxisJson(rows),
      "byTerminalStage" -> structuralOpportunityByTerminalStageJson(rows),
      "outpostDetail" -> outpostDetailFunnelJson(diagnostics, rows),
      "opponentRestrictionDetail" -> opponentRestrictionDetailFunnelJson(rows)
    )

  private final case class PlanCoherenceLineageRow(
      comparisonId: String,
      axisKey: String,
      axisSourceSide: RelativeCauseSourceSide,
      axisSourceIds: List[String],
      terminalStage: String,
      exactCauseIds: List[String],
      exactRootCauseEvidenceIds: List[String],
      kindOnlyCauseIds: List[String],
      kindOnlyRootCauseEvidenceIds: List[String],
      producedPlanCauseKinds: List[RelativeCauseKind],
      surfacedRootCauseKinds: List[RelativeCauseKind]
  )

  private def planCoherenceExactLineageFunnelJson(
      diagnostics: List[CandidateComparisonDiagnostic]
  ): JsObject =
    val actionableDiagnostics =
      diagnostics.filter(diagnostic =>
        diagnostic.comparisonKind == CandidateComparisonKind.PlayedVsBest &&
          badVerdict(diagnostic.verdict) &&
          diagnostic.subjectBinding != SubjectBindingClass.Other
      )
    val rows = actionableDiagnostics.flatMap(planCoherenceLineageRows)
    val planFallbackCauseComparisonIds =
      actionableDiagnostics
        .filter(_.relativeCauseDiagnostics.causeFlow.exists(flow => planFallbackCause(flow.causeKind)))
        .map(_.id)
        .distinct
        .sorted
    val planFallbackRootComparisonIds =
      actionableDiagnostics
        .filter(_.moveJudgmentView.primaryRootCauseKinds.exists(planFallbackCause))
        .map(_.id)
        .distinct
        .sorted
    val exactAxisCauseComparisonIds =
      rows.filter(_.exactCauseIds.nonEmpty).map(_.comparisonId).distinct.sorted
    val exactAxisRootComparisonIds =
      rows.filter(_.exactRootCauseEvidenceIds.nonEmpty).map(_.comparisonId).distinct.sorted
    val planAxisComparisonIds = rows.map(_.comparisonId).distinct.sorted
    val planRootWithoutPlanAxisComparisonIds = planFallbackRootComparisonIds.diff(planAxisComparisonIds)
    val planRootWithoutExactAxisLineageComparisonIds =
      planFallbackRootComparisonIds.diff(exactAxisRootComparisonIds)
    val planFallbackMaskedConcreteAxisComparisonIds =
      actionableDiagnostics
        .filter(diagnostic => planFallbackRootComparisonIds.contains(diagnostic.id))
        .filter(diagnostic =>
          val concreteRows = structuralOpportunityAxisRows(diagnostic).filter(row => concreteAxisCause(row.expectedCauseKind))
          concreteRows.nonEmpty && concreteRows.forall(_.exactRootCauseEvidenceIds.isEmpty)
        )
        .map(_.id)
        .distinct
        .sorted
    Json.obj(
      "classification" -> "audit_only",
      "axisOpportunityCount" -> rows.size,
      "axisOpportunityComparisonCount" -> planAxisComparisonIds.size,
      "axisOpportunityComparisonIds" -> planAxisComparisonIds,
      "planFallbackCauseProducedComparisonCount" -> planFallbackCauseComparisonIds.size,
      "planFallbackCauseProducedComparisonIds" -> planFallbackCauseComparisonIds,
      "planFallbackExactAxisCauseComparisonCount" -> exactAxisCauseComparisonIds.size,
      "planFallbackExactAxisCauseComparisonIds" -> exactAxisCauseComparisonIds,
      "planFallbackRootComparisonCount" -> planFallbackRootComparisonIds.size,
      "planFallbackRootComparisonIds" -> planFallbackRootComparisonIds,
      "planFallbackExactAxisRootComparisonCount" -> exactAxisRootComparisonIds.size,
      "planFallbackExactAxisRootComparisonIds" -> exactAxisRootComparisonIds,
      "planFallbackRootWithoutPlanCoherenceAxisComparisonIds" -> planRootWithoutPlanAxisComparisonIds,
      "planFallbackRootWithoutPlanCoherenceAxisComparisonCount" -> planRootWithoutPlanAxisComparisonIds.size,
      "planFallbackRootWithoutExactAxisLineageComparisonIds" -> planRootWithoutExactAxisLineageComparisonIds,
      "planFallbackRootWithoutExactAxisLineageComparisonCount" -> planRootWithoutExactAxisLineageComparisonIds.size,
      "planFallbackMaskedConcreteAxisComparisonIds" -> planFallbackMaskedConcreteAxisComparisonIds,
      "planFallbackMaskedConcreteAxisComparisonCount" -> planFallbackMaskedConcreteAxisComparisonIds.size,
      "terminalStageCounts" -> stringCountsJson(rows.map(_.terminalStage)),
      "producedPlanCauseKindCounts" -> stringCountsJson(rows.flatMap(_.producedPlanCauseKinds.map(_.toString))),
      "surfacedRootCauseKindCounts" -> stringCountsJson(rows.flatMap(_.surfacedRootCauseKinds.map(_.toString))),
      "byAxis" -> Json.obj(
        rows
          .groupBy(_.axisKey)
          .toSeq
          .sortBy(_._1)
          .map { case (axisKey, axisRows) =>
            axisKey -> Json.obj(
              "axisOpportunityCount" -> axisRows.size,
              "comparisonCount" -> axisRows.map(_.comparisonId).distinct.size,
              "terminalStageCounts" -> stringCountsJson(axisRows.map(_.terminalStage)),
              "exactCauseComparisonCount" -> axisRows.filter(_.exactCauseIds.nonEmpty).map(_.comparisonId).distinct.size,
              "exactRootComparisonCount" -> axisRows.filter(_.exactRootCauseEvidenceIds.nonEmpty).map(_.comparisonId).distinct.size
            )
          }*
      )
    )

  private def planCoherenceLineageRows(diagnostic: CandidateComparisonDiagnostic): List[PlanCoherenceLineageRow] =
    val primaryRootCauseEvidenceIds = diagnostic.moveJudgmentView.primaryRootCauseEvidenceIds.toSet
    val planFlows =
      diagnostic.relativeCauseDiagnostics.causeFlow
        .filter(flow => noEventCauseFlow(flow) && planFallbackCause(flow.causeKind))
    val surfacedRootCauseKinds = diagnostic.moveJudgmentView.primaryRootCauseKinds.filter(planFallbackCause)
    planCoherenceAxisOpportunities(diagnostic).map(axis =>
      val exactFlows = planFlows.filter(_.proofStrategicAxisKeys.contains(axis.key))
      val exactRootCauseIds = exactFlows.map(_.causeId).filter(primaryRootCauseEvidenceIds.contains)
      val kindOnlyCauseIds =
        planFlows
          .filterNot(_.proofStrategicAxisKeys.contains(axis.key))
          .map(_.causeId)
          .distinct
          .sorted
      val kindOnlyRootCauseIds = kindOnlyCauseIds.filter(primaryRootCauseEvidenceIds.contains)
      PlanCoherenceLineageRow(
        comparisonId = diagnostic.id,
        axisKey = axis.key,
        axisSourceSide = axis.sourceSide,
        axisSourceIds = structuralOpportunityAxisSourceIds(diagnostic, axis),
        terminalStage = planCoherenceTerminalStage(exactFlows, exactRootCauseIds, kindOnlyCauseIds, kindOnlyRootCauseIds),
        exactCauseIds = exactFlows.map(_.causeId).distinct.sorted,
        exactRootCauseEvidenceIds = exactRootCauseIds.distinct.sorted,
        kindOnlyCauseIds = kindOnlyCauseIds,
        kindOnlyRootCauseEvidenceIds = kindOnlyRootCauseIds.distinct.sorted,
        producedPlanCauseKinds = planFlows.map(_.causeKind).distinct.sortBy(_.toString),
        surfacedRootCauseKinds = surfacedRootCauseKinds.distinct.sortBy(_.toString)
      )
    )

  private def planCoherenceAxisOpportunities(diagnostic: CandidateComparisonDiagnostic): List[StructuralOpportunityAxis] =
    val axes = diagnostic.decisionTrace.semanticAxisDiagnostics
    val referenceAxes =
      (axes.referenceLeadAxes ++ axes.referenceOnlyAxes)
        .distinct
        .filter(planCoherenceAxis)
        .map(axis =>
          StructuralOpportunityAxis(
            key = axis,
            sourceSide = RelativeCauseSourceSide.Reference,
            expectedCauseKind = RelativeCauseKind.PlanContradiction
          )
        )
    val candidateAxes =
      (axes.candidateLeadAxes ++ axes.candidateOnlyAxes)
        .distinct
        .filter(planCoherenceAxis)
        .map(axis =>
          StructuralOpportunityAxis(
            key = axis,
            sourceSide = RelativeCauseSourceSide.Candidate,
            expectedCauseKind = RelativeCauseKind.PlanContradiction
          )
        )
    (referenceAxes ++ candidateAxes).distinctBy(axis => (axis.key, axis.sourceSide))

  private def planCoherenceTerminalStage(
      exactFlows: List[RelativeCauseFlowDiagnostic],
      exactRootCauseIds: List[String],
      kindOnlyCauseIds: List[String],
      kindOnlyRootCauseIds: List[String]
  ): String =
    if exactRootCauseIds.nonEmpty then "final_plan_root"
    else if kindOnlyRootCauseIds.nonEmpty then "kind_only_plan_root_without_axis"
    else if exactFlows.isEmpty && kindOnlyCauseIds.nonEmpty then "kind_only_plan_cause_without_axis"
    else if exactFlows.isEmpty then "no_exact_plan_cause"
    else if !exactFlows.exists(_.hasOwnedAdmissibleLongTermProof) then "no_owned_admissible_plan_proof"
    else if exactFlows.forall(_.causeWithoutIdea) then "no_idea"
    else if exactFlows.forall(_.claimCandidateIds.isEmpty) then "no_claim_candidate"
    else if exactFlows.forall(_.claimIds.isEmpty) then "no_final_claim"
    else "produced_not_root"

  private def primaryActionableStructuralOpportunity(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.comparisonKind == CandidateComparisonKind.PlayedVsBest &&
      badVerdict(diagnostic.verdict) &&
      diagnostic.subjectBinding != SubjectBindingClass.Other &&
      structuralOpportunityAxisOpportunities(diagnostic).nonEmpty

  private def structuralOpportunityAxisRows(
      diagnostic: CandidateComparisonDiagnostic
  ): List[StructuralOpportunityAxisRow] =
    val producedCauseKinds = diagnostic.relativeCauseDiagnostics.producedCauseKinds.filter(noEventCauseKind)
    val surfacedRootCauseKinds = diagnostic.moveJudgmentView.primaryRootCauseKinds.filter(noEventCauseKind)
    val primaryRootCauseEvidenceIds = diagnostic.moveJudgmentView.primaryRootCauseEvidenceIds.toSet
    val structuralFlows = diagnostic.relativeCauseDiagnostics.causeFlow.filter(noEventCauseFlow)
    structuralOpportunityAxisOpportunities(diagnostic).map(axis =>
      val exactFlows = structuralFlows.filter(flow => flow.proofStrategicAxisKeys.contains(axis.key))
      val exactExpectedFlows = exactFlows.filter(_.causeKind == axis.expectedCauseKind)
      val exactRootCauseIds = exactExpectedFlows.map(_.causeId).filter(primaryRootCauseEvidenceIds.contains)
      val kindOnlyCauseIds =
        structuralFlows
          .filter(flow => flow.causeKind == axis.expectedCauseKind && !flow.proofStrategicAxisKeys.contains(axis.key))
          .map(_.causeId)
          .distinct
          .sorted
      val kindOnlyRootCauseIds = kindOnlyCauseIds.filter(primaryRootCauseEvidenceIds.contains)
      val terminalStage = structuralOpportunityTerminalStage(diagnostic, axis, producedCauseKinds, surfacedRootCauseKinds)
      val exactTerminalStage =
        structuralOpportunityExactTerminalStage(axis, exactExpectedFlows, exactRootCauseIds, kindOnlyCauseIds, kindOnlyRootCauseIds)
      StructuralOpportunityAxisRow(
        comparisonId = diagnostic.id,
        axisKey = axis.key,
        axisKind = axisPart(axis.key, 0),
        axisPolarity = axisPart(axis.key, 1),
        axisLabel = axisPart(axis.key, 2),
        axisSourceSide = axis.sourceSide,
        axisSourceIds = structuralOpportunityAxisSourceIds(diagnostic, axis),
        axisSourceLayers = structuralOpportunityAxisSourceLayers(diagnostic, axis),
        expectedCauseKind = axis.expectedCauseKind,
        terminalStage = terminalStage,
        exactTerminalStage = exactTerminalStage,
        causeClass = structuralOpportunityCauseClass(axis.expectedCauseKind, producedCauseKinds, surfacedRootCauseKinds),
        exactCauseIds = exactExpectedFlows.map(_.causeId).distinct.sorted,
        exactRootCauseEvidenceIds = exactRootCauseIds.distinct.sorted,
        exactIdeaIds = exactExpectedFlows.flatMap(_.ideaIds).distinct.sorted,
        exactClaimCandidateIds = exactExpectedFlows.flatMap(_.claimCandidateIds).distinct.sorted,
        exactFinalClaimIds = exactExpectedFlows.flatMap(_.claimIds).distinct.sorted,
        kindOnlyCauseIds = kindOnlyCauseIds,
        kindOnlyRootCauseEvidenceIds = kindOnlyRootCauseIds.distinct.sorted,
        producedCauseKinds = producedCauseKinds,
        surfacedRootCauseKinds = surfacedRootCauseKinds
      )
    )

  private def structuralOpportunityAxisSourceIds(
      diagnostic: CandidateComparisonDiagnostic,
      axis: StructuralOpportunityAxis
  ): List[String] =
    val axes = diagnostic.decisionTrace.semanticAxisDiagnostics
    val sourceIds =
      axis.sourceSide match
        case RelativeCauseSourceSide.Reference => axes.referenceAxisSourceIds.getOrElse(axis.key, Nil)
        case RelativeCauseSourceSide.Candidate => axes.candidateAxisSourceIds.getOrElse(axis.key, Nil)
        case _ =>
          axes.referenceAxisSourceIds.getOrElse(axis.key, Nil) ++ axes.candidateAxisSourceIds.getOrElse(axis.key, Nil)
    sourceIds.distinct.sorted

  private def structuralOpportunityAxisSourceLayers(
      diagnostic: CandidateComparisonDiagnostic,
      axis: StructuralOpportunityAxis
  ): List[EvidenceLayer] =
    val axes = diagnostic.decisionTrace.semanticAxisDiagnostics
    val sourceLayers =
      axis.sourceSide match
        case RelativeCauseSourceSide.Reference => axes.referenceAxisSourceLayers.getOrElse(axis.key, Nil)
        case RelativeCauseSourceSide.Candidate => axes.candidateAxisSourceLayers.getOrElse(axis.key, Nil)
        case _ =>
          axes.referenceAxisSourceLayers.getOrElse(axis.key, Nil) ++ axes.candidateAxisSourceLayers.getOrElse(axis.key, Nil)
    sourceLayers.distinct.sortBy(_.toString)

  private def structuralOpportunityExactTerminalStage(
      axis: StructuralOpportunityAxis,
      exactExpectedFlows: List[RelativeCauseFlowDiagnostic],
      exactRootCauseIds: List[String],
      kindOnlyCauseIds: List[String],
      kindOnlyRootCauseIds: List[String]
  ): String =
    if exactRootCauseIds.nonEmpty then "final_structural_root"
    else if kindOnlyRootCauseIds.nonEmpty then "kind_only_root_without_axis"
    else if kindOnlyCauseIds.nonEmpty && exactExpectedFlows.isEmpty then "kind_only_cause_without_axis"
    else if exactExpectedFlows.isEmpty then "no_exact_cause_produced"
    else if !exactExpectedFlows.exists(_.hasOwnedAdmissibleLongTermProof) then "no_owned_admissible_long_term_proof"
    else if exactExpectedFlows.forall(_.causeWithoutIdea) then "no_idea"
    else if exactExpectedFlows.forall(_.claimCandidateIds.isEmpty) then "no_claim_candidate"
    else if exactExpectedFlows.forall(_.claimIds.isEmpty) then "no_final_claim"
    else if concreteAxisCause(axis.expectedCauseKind) then "produced_not_root"
    else "produced_not_root"

  private def structuralOpportunityAxisOpportunities(diagnostic: CandidateComparisonDiagnostic): List[StructuralOpportunityAxis] =
    val axes = diagnostic.decisionTrace.semanticAxisDiagnostics
    val referenceAxes =
      (axes.referenceLeadAxes ++ axes.referenceOnlyAxes)
        .distinct
        .filter(referenceAxisCanExplainStructuralLoss)
        .filterNot(planCoherenceAxis)
        .map(axis =>
          StructuralOpportunityAxis(
            key = axis,
            sourceSide = RelativeCauseSourceSide.Reference,
            expectedCauseKind = referenceLeadCauseForAxis(axis)
          )
        )
    val candidateAxes =
      (axes.candidateLeadAxes ++ axes.candidateOnlyAxes)
        .distinct
        .filter(candidateAxisCanExplainStructuralLoss)
        .filterNot(planCoherenceAxis)
        .map(axis =>
          StructuralOpportunityAxis(
            key = axis,
            sourceSide = RelativeCauseSourceSide.Candidate,
            expectedCauseKind = candidateNegativeCauseForAxis(axis)
          )
        )
    (referenceAxes ++ candidateAxes).distinctBy(axis => (axis.key, axis.sourceSide, axis.expectedCauseKind))

  private def structuralOpportunityTerminalStage(
      diagnostic: CandidateComparisonDiagnostic,
      axis: StructuralOpportunityAxis,
      producedCauseKinds: List[RelativeCauseKind],
      surfacedRootCauseKinds: List[RelativeCauseKind]
  ): String =
    val structuralFlows = diagnostic.relativeCauseDiagnostics.causeFlow.filter(noEventCauseFlow)
    val expectedCauseProduced = producedCauseKinds.contains(axis.expectedCauseKind)
    val expectedRootSurfaced = surfacedRootCauseKinds.contains(axis.expectedCauseKind)
    val planFallbackProducedOrSurfaced =
      producedCauseKinds.exists(planFallbackCause) || surfacedRootCauseKinds.exists(planFallbackCause)
    if concreteAxisCause(axis.expectedCauseKind) && planFallbackProducedOrSurfaced && !expectedCauseProduced && !expectedRootSurfaced then
      "fallback_masked_concrete_axis"
    else if structuralFlows.isEmpty && diagnostic.relativeCauseDiagnostics.expectedCauseHints.isEmpty then
      "no_cause_draft"
    else if structuralFlows.isEmpty then
      "no_cause_produced"
    else if !structuralFlows.exists(_.hasOwnedAdmissibleLongTermProof) then
      "no_owned_admissible_long_term_proof"
    else if structuralFlows.forall(_.causeWithoutIdea) then
      "no_idea"
    else if structuralFlows.forall(_.claimCandidateIds.isEmpty) then
      "no_claim_candidate"
    else if structuralFlows.forall(_.claimIds.isEmpty) then
      "no_final_claim"
    else if expectedRootSurfaced then
      "final_structural_root"
    else if expectedCauseProduced then
      "produced_not_root"
    else if surfacedRootCauseKinds.nonEmpty then
      "different_structural_root"
    else
      "produced_not_root"

  private def structuralOpportunityCauseClass(
      expectedCauseKind: RelativeCauseKind,
      producedCauseKinds: List[RelativeCauseKind],
      surfacedRootCauseKinds: List[RelativeCauseKind]
  ): String =
    val kinds = (surfacedRootCauseKinds ++ producedCauseKinds).distinct
    if kinds.contains(expectedCauseKind) && concreteAxisCause(expectedCauseKind) then "concrete_axis_cause"
    else if kinds.exists(planFallbackCause) then "plan_fallback"
    else if kinds.exists(broadStructuralFallbackCause) then "broad_structural_fallback"
    else "no_structural_cause"

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
      primaryRootCauseEvidenceIdTierSignatures: List[String],
      causeIds: List[String],
      causeIdKindSignatures: List[String],
      claimIds: List[String],
      publicSurfaceClaimSignatures: List[String]
  )

  private[qc] def semanticRubricFunnelJson(
      diagnostics: List[CandidateComparisonDiagnostic]
  ): JsObject =
    val rows = diagnostics.filter(primaryActionableStructuralOpportunity).flatMap(semanticRubricSlotRows)
    Json.obj(
      "classification" -> "audit_only",
      "semanticSlotCount" -> rows.size,
      "comparisonCount" -> rows.map(_.comparisonId).distinct.size,
      "comparisonIds" -> rows.map(_.comparisonId).distinct.sorted,
      "stageCounts" -> semanticRubricStrictLineageStageCountsJson(rows),
      "terminalStageCounts" -> stringCountsJson(rows.map(_.strictLineageTerminalStage)),
      "looseStageCounts" -> semanticRubricStageCountsJson(rows),
      "looseTerminalStageCounts" -> stringCountsJson(rows.map(_.terminalStage)),
      "strictLineageStageCounts" -> semanticRubricStrictLineageStageCountsJson(rows),
      "strictLineageTerminalStageCounts" -> stringCountsJson(rows.map(_.strictLineageTerminalStage)),
      "strictCauseLineageBoundCount" -> rows.count(_.strictCauseLineageBound),
      "strictPrimaryRootLineageBoundCount" -> rows.count(_.strictPrimaryRootLineageBound),
      "strictPrimaryRootTierLineageBoundCount" -> rows.count(_.strictPrimaryRootTierLineageBound),
      "byUnit" -> semanticRubricByUnitJson(rows),
      "strictLineageByUnit" -> semanticRubricStrictLineageByUnitJson(rows),
      "byTerminalStage" -> semanticRubricByTerminalStageJson(rows),
      "viewMissingComparisonIds" -> rows.filterNot(_.viewSurfaced).map(_.comparisonId).distinct.sorted
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
      slot.requiredTerminalStage.exists(stage => semanticRubricStageRank(stage) > semanticRubricStageRank("semantic_detected"))

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
    val legacyBest =
      legacyMatches.sortBy(row => -semanticRubricStageRank(row.terminalStage)).headOption
    val legacyTerminalStage = legacyBest.map(_.terminalStage).getOrElse("missing_semantic_slot")
    val legacyMatched =
      legacyBest.nonEmpty &&
        slot.requiredTerminalStage.forall(required =>
          semanticRubricStageRank(legacyTerminalStage) >= semanticRubricStageRank(required)
        )
    val causeBorrowFalsePositive =
      slot.requiredCauseKinds.nonEmpty && legacyMatched && !causeLineageTokenCoLocationSatisfied
    val primaryRootBorrowFalsePositive =
      slot.requiredPrimaryRootCauseKinds.nonEmpty && legacyMatched && !rootLineageTokenCoLocationSatisfied
    val rootTierBorrowFalsePositive =
      slot.requiredPrimaryRootArbitrationTiers.nonEmpty && legacyMatched && !rootTierLineageTokenCoLocationSatisfied
    val best = matches.sortBy(row => -semanticRubricStageRank(expectedSemanticSlotTerminalStage(slot, row))).headOption
    val terminalStage = best.map(row => expectedSemanticSlotTerminalStage(slot, row)).getOrElse("missing_semantic_slot")
    val strictLineageTerminalStage =
      best.map(row => expectedSemanticSlotStrictTerminalStage(slot, row)).getOrElse("missing_semantic_slot")
    val terminalStageSatisfied =
      slot.requiredTerminalStage.forall(required =>
        semanticRubricStageRank(terminalStage) >= semanticRubricStageRank(required)
      )
    val strictLineageTerminalStageSatisfied =
      slot.requiredTerminalStage.forall(required =>
        semanticRubricStageRank(strictLineageTerminalStage) >= semanticRubricStageRank(required)
      )
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
      "primaryRootCauseEvidenceIdTierSignatures" -> matches.flatMap(_.primaryRootCauseEvidenceIdTierSignatures).distinct.sorted,
      "causeIds" -> matches.flatMap(_.causeIds).distinct.sorted,
      "causeIdKindSignatures" -> matches.flatMap(_.causeIdKindSignatures).distinct.sorted,
      "claimIds" -> matches.flatMap(_.claimIds).distinct.sorted,
      "publicSurfaceClaimSignatures" -> matches.flatMap(_.publicSurfaceClaimSignatures).distinct.sorted,
      "bestLineageTrace" -> best.map(semanticRubricLineageTraceJson).getOrElse(Json.obj()),
      "lineageTraces" -> JsArray(
        matches
          .sortBy(row => (-semanticRubricStageRank(row.terminalStage), row.comparisonId))
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
      "causeIdKindSignatures" -> row.causeIdKindSignatures,
      "claimIds" -> row.claimIds,
      "publicSurfaceClaimSignatures" -> row.publicSurfaceClaimSignatures,
      "primaryRootCauseKinds" -> row.primaryRootCauseKinds.map(_.toString),
      "primaryRootCauseEvidenceIds" -> row.primaryRootCauseEvidenceIds,
      "primaryRootArbitrationTiers" -> row.primaryRootArbitrationTiers.map(_.toString),
      "primaryRootCauseEvidenceIdTierSignatures" -> row.primaryRootCauseEvidenceIdTierSignatures
    )

  private def tokensSatisfied(requiredTokens: List[String], values: List[String]): Boolean =
    requiredTokens.forall(token => tokenSatisfied(token, values))

  private def tokenSatisfied(requiredToken: String, values: List[String]): Boolean =
    values.exists(value => tokenMatches(requiredToken, value))

  private def tokenMatches(requiredToken: String, value: String): Boolean =
    val required = requiredToken.trim
    val candidate = value.trim
    val normalizedRequired = required.toLowerCase
    val normalizedCandidate = candidate.toLowerCase
    val acronymToken =
      required.length <= 3 && required.exists(_.isLetter) && required.forall(ch => !ch.isLetter || ch.isUpper)
    normalizedRequired.nonEmpty &&
      (
        normalizedCandidate == normalizedRequired ||
          (
            normalizedRequired.endsWith(":") &&
              normalizedCandidate.startsWith(normalizedRequired)
          ) ||
          (
            normalizedRequired.contains(":") &&
              (
                normalizedCandidate.startsWith(s"$normalizedRequired:") ||
                  normalizedCandidate.contains(s":$normalizedRequired:") ||
                  normalizedCandidate.endsWith(s":$normalizedRequired")
              )
          ) ||
          (
            !normalizedRequired.contains(":") &&
              normalizedCandidate.split(":").lastOption.contains(normalizedRequired)
          ) ||
          (
            !acronymToken &&
              !normalizedRequired.contains(":") &&
              candidate.startsWith(required) &&
              candidate
                .drop(required.length)
                .headOption
                .exists(ch => ch == ':' || ch == '-' || ch == '_' || ch.isUpper)
          )
      )

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
      slot.requiredTerminalStage.exists(stage =>
        semanticRubricStageRank(stage) >= semanticRubricStageRank("owned_cause_linked")
      )

  private def viewOnlyOwnedCauseExpectation(
      slot: ExpectedSemanticSlot,
      best: Option[SemanticRubricSlotRow]
  ): Boolean =
    slot.requiredTerminalStage.exists(stage =>
      semanticRubricStageRank(stage) >= semanticRubricStageRank("owned_cause_linked")
    ) && best.exists(row => row.viewSurfaced && !row.ownedCauseLinked)

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
        objectBindingSignatureTokenSatisfied(token, row.objectBindingSignatures) ||
          semanticDetailTokenGroupsForSlot(slot, row).exists(group => objectBindingTokenSatisfied(token, group))
      )

  private def objectBindingSignatureTokenSatisfied(token: String, signatures: List[String]): Boolean =
    val normalized = token.trim
    normalized.nonEmpty &&
      !roleTokenValue(normalized).exists(_.isEmpty) &&
      signatures.exists(signature => MoveReviewPhase3AuditMetrics.signatureParts(signature).contains(normalized))

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
    val causeIdKindSignatures = row.causeIdKindSignatures.toSet
    causeIds.nonEmpty &&
      lineageCandidateTokenGroups(slot, row).exists(group =>
        causeEvidenceIdsFromTokens(group).exists(causeEvidenceId =>
          causeIds.contains(causeEvidenceId) &&
            (
              slot.requiredCauseKinds.isEmpty ||
                slot.requiredCauseKinds.exists(kind =>
                  causeIdKindSignatures.contains(causeIdKindSignature(causeEvidenceId, kind))
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
    val tierSignatures = row.primaryRootCauseEvidenceIdTierSignatures.toSet
    tierSignatures.nonEmpty &&
      lineageCandidateTokenGroups(slot, row).exists(group =>
        rootLineageEvidenceIdsFromTokens(group).exists(rootCauseEvidenceId =>
          slot.requiredPrimaryRootArbitrationTiers.exists(tier =>
            tierSignatures.contains(primaryRootCauseEvidenceIdTierSignature(rootCauseEvidenceId, tier))
          )
        )
      )

  private def primaryRootCauseEvidenceIdTierSignature(
      causeEvidenceId: String,
      tier: MoveJudgmentCauseRootArbitrationTier
  ): String =
    s"$causeEvidenceId|tier=$tier"

  private def causeIdKindSignature(causeEvidenceId: String, kind: RelativeCauseKind): String =
    s"$causeEvidenceId|kind=$kind"

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
      primaryRootCauseEvidenceIdTierSignatures: List[String],
      tokenGroups: List[List[String]]
  ): Boolean =
    val signatures = primaryRootCauseEvidenceIdTierSignatures.toSet
    signatures.nonEmpty &&
      tokenGroups.exists(group =>
        rootLineageEvidenceIdsFromTokens(group).exists(rootCauseEvidenceId =>
          signatures.exists(_.startsWith(s"$rootCauseEvidenceId|tier="))
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

  private def semanticRubricStageRank(stage: String): Int =
    stage match
      case "missing_semantic_slot"  => 0
      case "semantic_detected"      => 1
      case "object_bound"           => 2
      case "exact_axis_or_pattern"  => 3
      case "cause_owned"            => 4
      case "claim_survived"         => 5
      case "view_surfaced"          => 6
      case "owned_cause_linked"     => 7
      case "clustered_coherent"     => 8
      case _                        => 0

  private def expectedSemanticSlotTerminalStage(
      slot: ExpectedSemanticSlot,
      row: SemanticRubricSlotRow
  ): String =
    if expectedSemanticSlotRecognitionViewMatch(slot, row) then "view_surfaced"
    else row.terminalStage

  private def expectedSemanticSlotStrictTerminalStage(
      slot: ExpectedSemanticSlot,
      row: SemanticRubricSlotRow
  ): String =
    if expectedSemanticSlotRecognitionViewMatch(slot, row) then "view_surfaced"
    else row.strictLineageTerminalStage

  private def expectedSemanticSlotRecognitionViewMatch(
      slot: ExpectedSemanticSlot,
      row: SemanticRubricSlotRow
  ): Boolean =
    slot.requiredTerminalStage.contains("view_surfaced") &&
      slot.requiredCauseKinds.isEmpty &&
      slot.requiredPrimaryRootCauseKinds.isEmpty &&
      slot.requiredPrimaryRootArbitrationTiers.isEmpty &&
      row.viewSurfaced

  private def semanticRubricSlotRows(diagnostic: CandidateComparisonDiagnostic): List[SemanticRubricSlotRow] =
    val axisRows =
      structuralOpportunityAxisOpportunities(diagnostic).flatMap(axis =>
        semanticRubricUnitsForAxis(axis.key).map(unit => semanticRubricSlotRow(diagnostic, unit, Some(axis.key)))
      )
    val viewOnlyRows =
      diagnostic.moveJudgmentView.positionPlanTechniqueUnits
        .filterNot(unit => axisRows.exists(_.unit == unit))
        .map(unit => semanticRubricSlotRow(diagnostic, unit, None))
    (axisRows ++ viewOnlyRows)
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
    val decodedDetailSurfaced =
      detailUnits.contains(unit) &&
        (axisKey.forall(detailAxisKeys.contains) || axisKey.isEmpty)
    val flows =
      diagnostic.relativeCauseDiagnostics.causeFlow.filter(flow =>
        axisKey.exists(flow.proofStrategicAxisKeys.contains) ||
          frameCauseIds.contains(flow.causeId)
      )
    val fallbackFlows =
      Option.when(flows.isEmpty && axisKey.isEmpty)(diagnostic.relativeCauseDiagnostics.causeFlow).getOrElse(flows)
    val exactAxisOrPattern =
      axisKey.exists(axis => flows.exists(_.proofStrategicAxisKeys.contains(axis)) || frameAxisKeys.contains(axis)) ||
        (axisKey.isEmpty && unitSurfaced)
    val objectBound =
      fallbackFlows.exists(_.objectBindingSignatures.nonEmpty) ||
        objectBindingSignatures.nonEmpty
    val causeOwned =
      fallbackFlows.exists(flow => flow.hasOwnedTypedDepth || flow.hasOwnedAdmissibleLongTermProof)
    val claimSurvived =
      fallbackFlows.exists(_.claimIds.nonEmpty)
    val causeIds = fallbackFlows.map(_.causeId).distinct.sorted
    val publicSurfaceClaimDiagnostics =
      view.publicMoveMeaningClaimDiagnostics.filter(diagnostic =>
        semanticRubricSurfaceClaimMatches(diagnostic, unit, axisKey)
      )
    val publicSurfaceClaimSignatures = publicSurfaceClaimDiagnostics.map(_.signature)
    val publicSurfaceClaimSurfaced =
      publicSurfaceClaimDiagnostics.nonEmpty
    val publicSurfaceClaimCauseIds =
      publicSurfaceClaimDiagnostics.flatMap(_.causeEvidenceIds).distinct.sorted
    val ownedCauseLinked =
      fallbackFlows.exists(flow =>
        frameCauseIds.contains(flow.causeId) &&
          (flow.hasOwnedTypedDepth || flow.hasOwnedAdmissibleLongTermProof) &&
          publicSurfaceClaimCauseIds.contains(flow.causeId)
      )
    val viewSurfaced =
      publicSurfaceClaimSurfaced &&
        unitSurfaced &&
        (
          axisKey.forall(frameAxisKeys.contains) ||
            fallbackFlows.exists(flow => frameCauseIds.contains(flow.causeId)) ||
            axisKey.isEmpty
        )
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
        view.primaryRootCauseEvidenceIdTierSignatures,
        semanticDetailTokenGroups
      )
    val strictCauseOwned = causeOwned && strictCauseLineageBound
    val strictClaimSurvived = claimSurvived && strictCauseOwned
    val strictViewSurfaced = viewSurfaced && strictClaimSurvived
    val strictOwnedCauseLinked = ownedCauseLinked && strictViewSurfaced
    val strictClusteredCoherent = clusteredCoherent && strictOwnedCauseLinked
    SemanticRubricSlotRow(
      comparisonId = diagnostic.id,
      unit = unit,
      axisKey = axisKey,
      terminalStage = semanticRubricTerminalStage(
        objectBound = objectBound,
        exactAxisOrPattern = exactAxisOrPattern,
        causeOwned = causeOwned,
        claimSurvived = claimSurvived,
        viewSurfaced = viewSurfaced,
        ownedCauseLinked = ownedCauseLinked,
        clusteredCoherent = clusteredCoherent
      ),
      strictLineageTerminalStage = semanticRubricTerminalStage(
        objectBound = objectBound,
        exactAxisOrPattern = exactAxisOrPattern,
        causeOwned = strictCauseOwned,
        claimSurvived = strictClaimSurvived,
        viewSurfaced = strictViewSurfaced,
        ownedCauseLinked = strictOwnedCauseLinked,
        clusteredCoherent = strictClusteredCoherent
      ),
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
      causeKinds = fallbackFlows.map(_.causeKind).distinct.sortBy(_.toString),
      primaryRootCauseKinds = view.primaryRootCauseKinds.distinct.sortBy(_.toString),
      primaryRootCauseEvidenceIds = view.primaryRootCauseEvidenceIds.distinct.sorted,
      primaryRootArbitrationTiers = view.primaryRootArbitrationTiers.distinct.sortBy(_.toString),
      primaryRootCauseEvidenceIdTierSignatures = view.primaryRootCauseEvidenceIdTierSignatures.distinct.sorted,
      causeIds = causeIds,
      causeIdKindSignatures =
        fallbackFlows
          .map(flow => causeIdKindSignature(flow.causeId, flow.causeKind))
          .distinct
          .sorted,
      claimIds = fallbackFlows.flatMap(_.claimIds).distinct.sorted,
      publicSurfaceClaimSignatures = publicSurfaceClaimSignatures.distinct.sorted
    )

  private def semanticRubricSurfaceClaimMatches(
      diagnostic: PublicMoveMeaningClaimDiagnostic,
      unit: PositionPlanTechniqueUnit,
      axisKey: Option[String]
  ): Boolean =
    diagnostic.unit == unit &&
      axisKey.forall(diagnostic.axisKey.contains)

  private def semanticRubricUnitsForAxis(axisKey: String): List[PositionPlanTechniqueUnit] =
    axisPart(axisKey, 0) match
      case "PawnBreak" =>
        List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute)
      case "PlanCoherence" =>
        List(PositionPlanTechniqueUnit.PlanOptionSet)
      case "Counterplay" =>
        if axisPart(axisKey, 1) == "Restrain" then List(PositionPlanTechniqueUnit.SpacePreventionResourceDenial)
        else List(PositionPlanTechniqueUnit.CounterplayRace)
      case "Activity" =>
        List(PositionPlanTechniqueUnit.PieceRerouteRoute)
      case "SpaceCenter" =>
        List(PositionPlanTechniqueUnit.SpacePreventionResourceDenial)
      case "Target" =>
        List(PositionPlanTechniqueUnit.StructuralTransformation)
      case _ =>
        Nil

  private def semanticRubricTerminalStage(
      objectBound: Boolean,
      exactAxisOrPattern: Boolean,
      causeOwned: Boolean,
      claimSurvived: Boolean,
      viewSurfaced: Boolean,
      ownedCauseLinked: Boolean,
      clusteredCoherent: Boolean
  ): String =
    if !objectBound then "semantic_detected"
    else if !exactAxisOrPattern then "object_bound"
    else if !causeOwned then "exact_axis_or_pattern"
    else if !claimSurvived then "cause_owned"
    else if !viewSurfaced then "claim_survived"
    else if !ownedCauseLinked then "view_surfaced"
    else if !clusteredCoherent then "owned_cause_linked"
    else "clustered_coherent"

  private def semanticRubricStageCountsJson(rows: List[SemanticRubricSlotRow]): JsObject =
    Json.obj(
      "semantic_detected" -> rows.size,
      "object_bound" -> rows.count(_.objectBound),
      "exact_axis_or_pattern" -> rows.count(_.exactAxisOrPattern),
      "cause_owned" -> rows.count(_.causeOwned),
      "claim_survived" -> rows.count(_.claimSurvived),
      "view_surfaced" -> rows.count(_.viewSurfaced),
      "owned_cause_linked" -> rows.count(_.ownedCauseLinked),
      "clustered_coherent" -> rows.count(_.clusteredCoherent)
    )

  private def semanticRubricStrictLineageStageCountsJson(rows: List[SemanticRubricSlotRow]): JsObject =
    semanticRubricRankStageCountsJson(rows.map(_.strictLineageTerminalStage))

  private def semanticRubricRankStageCountsJson(stages: List[String]): JsObject =
    Json.obj(
      "semantic_detected" -> stages.count(stage => semanticRubricStageRank(stage) >= semanticRubricStageRank("semantic_detected")),
      "object_bound" -> stages.count(stage => semanticRubricStageRank(stage) >= semanticRubricStageRank("object_bound")),
      "exact_axis_or_pattern" -> stages.count(stage =>
        semanticRubricStageRank(stage) >= semanticRubricStageRank("exact_axis_or_pattern")
      ),
      "cause_owned" -> stages.count(stage => semanticRubricStageRank(stage) >= semanticRubricStageRank("cause_owned")),
      "claim_survived" -> stages.count(stage => semanticRubricStageRank(stage) >= semanticRubricStageRank("claim_survived")),
      "view_surfaced" -> stages.count(stage => semanticRubricStageRank(stage) >= semanticRubricStageRank("view_surfaced")),
      "owned_cause_linked" -> stages.count(stage =>
        semanticRubricStageRank(stage) >= semanticRubricStageRank("owned_cause_linked")
      ),
      "clustered_coherent" -> stages.count(stage =>
        semanticRubricStageRank(stage) >= semanticRubricStageRank("clustered_coherent")
      )
    )

  private def semanticRubricByUnitJson(rows: List[SemanticRubricSlotRow]): JsObject =
    JsObject(
      rows
        .groupBy(_.unit)
        .toList
        .sortBy(_._1.toString)
        .map { case (unit, unitRows) =>
          unit.toString -> Json.obj(
            "semanticSlotCount" -> unitRows.size,
            "comparisonCount" -> unitRows.map(_.comparisonId).distinct.size,
            "comparisonIds" -> unitRows.map(_.comparisonId).distinct.sorted,
            "stageCounts" -> semanticRubricStrictLineageStageCountsJson(unitRows),
            "terminalStageCounts" -> stringCountsJson(unitRows.map(_.strictLineageTerminalStage)),
            "looseStageCounts" -> semanticRubricStageCountsJson(unitRows),
            "looseTerminalStageCounts" -> stringCountsJson(unitRows.map(_.terminalStage)),
            "axisKeys" -> unitRows.flatMap(_.axisKey).distinct.sorted,
            "frameIds" -> unitRows.flatMap(_.frameIds).distinct.sorted,
            "causeIds" -> unitRows.flatMap(_.causeIds).distinct.sorted,
            "claimIds" -> unitRows.flatMap(_.claimIds).distinct.sorted
          )
        }
    )

  private def semanticRubricStrictLineageByUnitJson(rows: List[SemanticRubricSlotRow]): JsObject =
    JsObject(
      rows
        .groupBy(_.unit)
        .toList
        .sortBy(_._1.toString)
        .map { case (unit, unitRows) =>
          unit.toString -> Json.obj(
            "semanticSlotCount" -> unitRows.size,
            "strictLineageStageCounts" -> semanticRubricStrictLineageStageCountsJson(unitRows),
            "strictLineageTerminalStageCounts" -> stringCountsJson(unitRows.map(_.strictLineageTerminalStage)),
            "strictCauseLineageBoundCount" -> unitRows.count(_.strictCauseLineageBound),
            "strictPrimaryRootLineageBoundCount" -> unitRows.count(_.strictPrimaryRootLineageBound),
            "strictPrimaryRootTierLineageBoundCount" -> unitRows.count(_.strictPrimaryRootTierLineageBound),
            "comparisonIds" -> unitRows.map(_.comparisonId).distinct.sorted
          )
        }
    )

  private def semanticRubricByTerminalStageJson(rows: List[SemanticRubricSlotRow]): JsObject =
    JsObject(
      rows
        .groupBy(_.terminalStage)
        .toList
        .sortBy(_._1)
        .map { case (stage, stageRows) =>
          stage -> Json.obj(
            "semanticSlotCount" -> stageRows.size,
            "comparisonCount" -> stageRows.map(_.comparisonId).distinct.size,
            "comparisonIds" -> stageRows.map(_.comparisonId).distinct.sorted,
            "unitCounts" -> stringCountsJson(stageRows.map(_.unit.toString)),
            "axisKeys" -> stageRows.flatMap(_.axisKey).distinct.sorted,
            "frameIds" -> stageRows.flatMap(_.frameIds).distinct.sorted
          )
        }
    )

  private def structuralOpportunityByAxisJson(rows: List[StructuralOpportunityAxisRow]): JsObject =
    JsObject(
      rows
        .groupBy(_.axisKey)
        .toList
        .sortBy(_._1)
        .map { case (axisKey, axisRows) =>
          axisKey -> Json.obj(
            "axisKind" -> axisRows.headOption.map(_.axisKind).getOrElse(""),
            "axisPolarity" -> axisRows.headOption.map(_.axisPolarity).getOrElse(""),
            "axisLabel" -> axisRows.headOption.map(_.axisLabel).getOrElse(""),
            "axisSourceSideCounts" -> stringCountsJson(axisRows.map(_.axisSourceSide.toString)),
            "expectedCauseKindCounts" -> stringCountsJson(axisRows.map(_.expectedCauseKind.toString)),
            "axisOpportunityCount" -> axisRows.size,
            "comparisonCount" -> axisRows.map(_.comparisonId).distinct.size,
            "comparisonIds" -> axisRows.map(_.comparisonId).distinct.sorted,
            "terminalStageCounts" -> stringCountsJson(axisRows.map(_.terminalStage)),
            "causeClassCounts" -> stringCountsJson(axisRows.map(_.causeClass)),
            "producedCauseKindCounts" -> stringCountsJson(axisRows.flatMap(_.producedCauseKinds.map(_.toString))),
            "surfacedRootCauseKindCounts" -> stringCountsJson(axisRows.flatMap(_.surfacedRootCauseKinds.map(_.toString)))
          )
        }
    )

  private def structuralOpportunityAxisExactLineageJson(rows: List[StructuralOpportunityAxisRow]): JsObject =
    Json.obj(
      "classification" -> "audit_only",
      "axisOpportunityCount" -> rows.size,
      "comparisonCount" -> rows.map(_.comparisonId).distinct.size,
      "exactCauseProducedCount" -> rows.count(_.exactCauseIds.nonEmpty),
      "exactRootCount" -> rows.count(_.exactRootCauseEvidenceIds.nonEmpty),
      "kindOnlyCauseWithoutAxisCount" -> rows.count(_.exactTerminalStage == "kind_only_cause_without_axis"),
      "kindOnlyRootWithoutAxisCount" -> rows.count(_.exactTerminalStage == "kind_only_root_without_axis"),
      "terminalStageCounts" -> stringCountsJson(rows.map(_.exactTerminalStage)),
      "byAxis" -> structuralOpportunityAxisExactByAxisJson(rows),
      "byTerminalStage" -> structuralOpportunityAxisExactByTerminalStageJson(rows)
    )

  private def structuralOpportunityAxisExactByAxisJson(rows: List[StructuralOpportunityAxisRow]): JsObject =
    JsObject(
      rows
        .groupBy(_.axisKey)
        .toList
        .sortBy(_._1)
        .map { case (axisKey, axisRows) =>
          axisKey -> Json.obj(
            "axisKind" -> axisRows.headOption.map(_.axisKind).getOrElse(""),
            "axisPolarity" -> axisRows.headOption.map(_.axisPolarity).getOrElse(""),
            "axisLabel" -> axisRows.headOption.map(_.axisLabel).getOrElse(""),
            "axisSourceSideCounts" -> stringCountsJson(axisRows.map(_.axisSourceSide.toString)),
            "axisSourceIds" -> axisRows.flatMap(_.axisSourceIds).distinct.sorted,
            "axisSourceLayers" -> axisRows.flatMap(_.axisSourceLayers.map(_.toString)).distinct.sorted,
            "expectedCauseKindCounts" -> stringCountsJson(axisRows.map(_.expectedCauseKind.toString)),
            "axisOpportunityCount" -> axisRows.size,
            "comparisonCount" -> axisRows.map(_.comparisonId).distinct.size,
            "comparisonIds" -> axisRows.map(_.comparisonId).distinct.sorted,
            "terminalStageCounts" -> stringCountsJson(axisRows.map(_.exactTerminalStage)),
            "exactCauseIds" -> axisRows.flatMap(_.exactCauseIds).distinct.sorted,
            "exactRootCauseEvidenceIds" -> axisRows.flatMap(_.exactRootCauseEvidenceIds).distinct.sorted,
            "exactIdeaIds" -> axisRows.flatMap(_.exactIdeaIds).distinct.sorted,
            "exactClaimCandidateIds" -> axisRows.flatMap(_.exactClaimCandidateIds).distinct.sorted,
            "exactFinalClaimIds" -> axisRows.flatMap(_.exactFinalClaimIds).distinct.sorted,
            "kindOnlyCauseIds" -> axisRows.flatMap(_.kindOnlyCauseIds).distinct.sorted,
            "kindOnlyRootCauseEvidenceIds" -> axisRows.flatMap(_.kindOnlyRootCauseEvidenceIds).distinct.sorted
          )
        }
    )

  private def structuralOpportunityAxisExactByTerminalStageJson(rows: List[StructuralOpportunityAxisRow]): JsObject =
    JsObject(
      rows
        .groupBy(_.exactTerminalStage)
        .toList
        .sortBy(_._1)
        .map { case (stage, stageRows) =>
          stage -> Json.obj(
            "axisOpportunityCount" -> stageRows.size,
            "comparisonCount" -> stageRows.map(_.comparisonId).distinct.size,
            "comparisonIds" -> stageRows.map(_.comparisonId).distinct.sorted,
            "axisCounts" -> stringCountsJson(stageRows.map(_.axisKey)),
            "expectedCauseKindCounts" -> stringCountsJson(stageRows.map(_.expectedCauseKind.toString)),
            "exactCauseIds" -> stageRows.flatMap(_.exactCauseIds).distinct.sorted,
            "exactRootCauseEvidenceIds" -> stageRows.flatMap(_.exactRootCauseEvidenceIds).distinct.sorted,
            "kindOnlyCauseIds" -> stageRows.flatMap(_.kindOnlyCauseIds).distinct.sorted,
            "kindOnlyRootCauseEvidenceIds" -> stageRows.flatMap(_.kindOnlyRootCauseEvidenceIds).distinct.sorted
          )
        }
    )

  private def structuralOpportunityByTerminalStageJson(rows: List[StructuralOpportunityAxisRow]): JsObject =
    JsObject(
      rows
        .groupBy(_.terminalStage)
        .toList
        .sortBy(_._1)
        .map { case (stage, stageRows) =>
          stage -> Json.obj(
            "axisOpportunityCount" -> stageRows.size,
            "comparisonCount" -> stageRows.map(_.comparisonId).distinct.size,
            "comparisonIds" -> stageRows.map(_.comparisonId).distinct.sorted,
            "axisCounts" -> stringCountsJson(stageRows.map(_.axisKey)),
            "expectedCauseKindCounts" -> stringCountsJson(stageRows.map(_.expectedCauseKind.toString)),
            "causeClassCounts" -> stringCountsJson(stageRows.map(_.causeClass))
          )
        }
    )

  private def outpostDetailFunnelJson(
      diagnostics: List[CandidateComparisonDiagnostic],
      rows: List[StructuralOpportunityAxisRow]
  ): JsObject =
    val outpostEvidenceDiagnostics = diagnostics.filter(outpostEvidenceComparison)
    val outpostAxisComparisonIds =
      outpostEvidenceDiagnostics
        .filter(diagnostic => allSemanticAxes(diagnostic).exists(outpostAxisKey))
        .map(_.id)
        .distinct
        .sorted
    val outpostRows = rows.filter(row => outpostAxisKey(row.axisKey))
    Json.obj(
      "classification" -> "audit_only",
      "evidenceComparisonCount" -> outpostEvidenceDiagnostics.map(_.id).distinct.size,
      "evidenceComparisonIds" -> outpostEvidenceDiagnostics.map(_.id).distinct.sorted,
      "axisPreservedComparisonCount" -> outpostAxisComparisonIds.size,
      "axisPreservedComparisonIds" -> outpostAxisComparisonIds,
      "evidenceWithoutAxisComparisonIds" ->
        outpostEvidenceDiagnostics
          .map(_.id)
          .distinct
          .filterNot(outpostAxisComparisonIds.toSet)
          .sorted,
      "axisOpportunityCount" -> outpostRows.size,
      "axisOpportunityComparisonCount" -> outpostRows.map(_.comparisonId).distinct.size,
      "axisOpportunityComparisonIds" -> outpostRows.map(_.comparisonId).distinct.sorted,
      "terminalStageCounts" -> stringCountsJson(outpostRows.map(_.terminalStage)),
      "causeClassCounts" -> stringCountsJson(outpostRows.map(_.causeClass)),
      "expectedCauseKindCounts" -> stringCountsJson(outpostRows.map(_.expectedCauseKind.toString)),
      "producedCauseKindCounts" -> stringCountsJson(outpostRows.flatMap(_.producedCauseKinds.map(_.toString))),
      "surfacedRootCauseKindCounts" -> stringCountsJson(outpostRows.flatMap(_.surfacedRootCauseKinds.map(_.toString)))
    )

  private def outpostEvidenceComparison(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.comparisonKind == CandidateComparisonKind.PlayedVsBest &&
      badVerdict(diagnostic.verdict) &&
      (
        diagnostic.decisionTrace.referenceStructuralConsequences.exists(outpostConsequence) ||
          diagnostic.decisionTrace.candidateStructuralConsequences.exists(outpostConsequence)
      )

  private def outpostConsequence(kind: TransitionConsequenceKind): Boolean =
    kind == TransitionConsequenceKind.OutpostGain || kind == TransitionConsequenceKind.OutpostConcession

  private def allSemanticAxes(diagnostic: CandidateComparisonDiagnostic): List[String] =
    val axes = diagnostic.decisionTrace.semanticAxisDiagnostics
    (
      axes.referenceAxes ++
        axes.candidateAxes ++
        axes.sharedAxes ++
        axes.referenceOnlyAxes ++
        axes.candidateOnlyAxes ++
        axes.referenceLeadAxes ++
        axes.candidateLeadAxes
    ).distinct

  private def outpostAxisKey(axisKey: String): Boolean =
    axisPart(axisKey, 0) == StrategicAxisKind.Activity.toString &&
      axisPart(axisKey, 2).contains("outpost")

  private def opponentRestrictionDetailFunnelJson(rows: List[StructuralOpportunityAxisRow]): JsObject =
    val restrictionRows = rows.filter(row => opponentRestrictionAxisKey(row.axisKey))
    Json.obj(
      "classification" -> "audit_only",
      "axisOpportunityCount" -> restrictionRows.size,
      "axisOpportunityComparisonCount" -> restrictionRows.map(_.comparisonId).distinct.size,
      "axisOpportunityComparisonIds" -> restrictionRows.map(_.comparisonId).distinct.sorted,
      "terminalStageCounts" -> stringCountsJson(restrictionRows.map(_.terminalStage)),
      "causeClassCounts" -> stringCountsJson(restrictionRows.map(_.causeClass)),
      "expectedCauseKindCounts" -> stringCountsJson(restrictionRows.map(_.expectedCauseKind.toString)),
      "fallbackMaskedConcreteAxisComparisonIds" ->
        restrictionRows
          .filter(_.terminalStage == "fallback_masked_concrete_axis")
          .map(_.comparisonId)
          .distinct
          .sorted,
      "producedCauseKindCounts" -> stringCountsJson(restrictionRows.flatMap(_.producedCauseKinds.map(_.toString))),
      "surfacedRootCauseKindCounts" -> stringCountsJson(restrictionRows.flatMap(_.surfacedRootCauseKinds.map(_.toString)))
    )

  private def opponentRestrictionAxisKey(axisKey: String): Boolean =
    axisPart(axisKey, 0) == StrategicAxisKind.Counterplay.toString &&
      axisPart(axisKey, 1) == StrategicAxisPolarity.Restrain.toString

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

  private def referenceLeadCauseForAxis(axisKey: String): RelativeCauseKind =
    axisPart(axisKey, 0) match
      case "Target" if axisPart(axisKey, 2).contains("weak-pawn") =>
        RelativeCauseKind.PawnWeaknessTarget
      case "Target" =>
        RelativeCauseKind.TargetPressureGain
      case "SpaceCenter" =>
        RelativeCauseKind.CenterControlGain
      case "PawnBreak" =>
        RelativeCauseKind.PawnBreakOpportunity
      case "Activity" =>
        RelativeCauseKind.ActivityGain
      case "Counterplay" if axisPart(axisKey, 1) == "Restrain" =>
        RelativeCauseKind.OpponentRestriction
      case "Counterplay" if axisPart(axisKey, 2).contains("king-safety") =>
        RelativeCauseKind.KingSafetyConcession
      case "PlanCoherence" =>
        RelativeCauseKind.PlanContradiction
      case _ =>
        RelativeCauseKind.MissedStrategicImprovement

  private def candidateNegativeCauseForAxis(axisKey: String): RelativeCauseKind =
    axisPart(axisKey, 0) match
      case "Target" if axisPart(axisKey, 1) == "Release" =>
        RelativeCauseKind.TargetPressureRelease
      case "Target" if axisPart(axisKey, 2).contains("weak-pawn") =>
        RelativeCauseKind.PawnWeaknessTarget
      case "Target" =>
        RelativeCauseKind.TargetPressureGain
      case "SpaceCenter" =>
        RelativeCauseKind.CenterControlGain
      case "PawnBreak" =>
        RelativeCauseKind.PawnBreakOpportunity
      case "Activity" =>
        RelativeCauseKind.ActivityLoss
      case "Counterplay" if axisPart(axisKey, 1) == "Restrain" =>
        RelativeCauseKind.OpponentRestriction
      case "Counterplay" if axisPart(axisKey, 2).contains("king-safety") =>
        RelativeCauseKind.KingSafetyConcession
      case "PlanCoherence" =>
        RelativeCauseKind.PlanContradiction
      case _ =>
        RelativeCauseKind.StrategicConcession

  private def noEventCauseKind(kind: RelativeCauseKind): Boolean =
    ClaimEventCluster.kindForCause(kind).isEmpty

  private def concreteAxisCause(kind: RelativeCauseKind): Boolean =
    kind match
      case RelativeCauseKind.TargetPressureGain | RelativeCauseKind.TargetPressureRelease |
          RelativeCauseKind.CenterControlGain |
          RelativeCauseKind.KingSafetyConcession | RelativeCauseKind.PawnWeaknessTarget |
          RelativeCauseKind.PawnBreakOpportunity | RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss |
          RelativeCauseKind.OpponentRestriction =>
        true
      case _ =>
        false

  private def planFallbackCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.PlanContradiction || kind == RelativeCauseKind.PlanImprovement

  private def broadStructuralFallbackCause(kind: RelativeCauseKind): Boolean =
    kind match
      case RelativeCauseKind.StructuralImprovement | RelativeCauseKind.MissedStrategicImprovement |
          RelativeCauseKind.StrategicConcession =>
        true
      case _ =>
        false

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


