package lila.chessjudgment.analysis.qc

import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

object MoveReviewPhase3AuditViewJson:
  def objectBindingSignatureSample(signatures: List[String]): List[String] =
    signatures.distinct.sortBy(objectBindingSignatureSampleSortKey).take(5)

  def objectBindingSignatureSampleJson(signatures: List[String]): JsObject =
    Json.obj(
      "objectBindingSignatureCount" -> signatures.size,
      "objectBindingSignaturesSample" -> objectBindingSignatureSample(signatures)
    )

  private def objectBindingSignatureSampleSortKey(signature: String): (Int, Int, String) =
    val normalized = signature.toLowerCase
    val targetRank =
      if normalized.contains("target=square:") ||
        normalized.contains("target=file:") ||
        normalized.contains("target=piece:") ||
        normalized.contains("target=pawn:") ||
        normalized.contains("target=plansubject:")
      then 0
      else if normalized.contains("target=") then 1
      else 2
    val moveRank =
      if normalized.contains("actor=move:") || normalized.contains("witness=move:") then 0 else 1
    (targetRank, moveRank, signature)

  def moveMeaningSurfacePayloadJson(view: MoveJudgmentView): JsObject =
    val verdict = view.verdict.map(frame => MoveMeaningSurface.verdict(frame))
    val surfaces = MoveMeaningSurface.from(view)
    Json.obj(
      "verdict" -> verdict.map(moveMeaningSurfaceVerdictJson),
      "move_semantics" -> surfaces.map(moveMeaningSurfaceJson),
      "llm_payload" -> verdict.toList.flatMap(moveMeaningSurfaceLlmPayloadJson(_, surfaces))
    )

  private def moveMeaningSurfaceVerdictJson(verdict: MoveMeaningSurfaceVerdict): JsObject =
    Json.obj(
      "verdict_code" -> verdict.verdictCode,
      "move_quality" -> verdict.moveQuality,
      "played_move" -> verdict.playedMove,
      "reference_move" -> verdict.referenceMove
    )

  private def moveMeaningSurfaceJson(surface: MoveMeaningSurface): JsObject =
    Json.obj(
      "move_uci" -> surface.moveUci,
      "subject" -> surface.subject,
      "line_role" -> surface.lineRole,
      "move_quality" -> surface.moveQuality,
      "idea_type" -> surface.ideaType,
      "idea" -> moveMeaningSurfaceCodeJson(surface.idea),
      "idea_quality" -> surface.ideaQuality,
      "assessment" -> moveMeaningSurfaceAssessmentJson(surface.assessment),
      "failure_family" -> surface.failureFamily,
      "problem" -> surface.problem,
      "target" -> moveMeaningSurfaceTargetJson(surface.target),
      "targets" -> moveMeaningSurfaceTargetJson(surface.target),
      "priority" -> surface.priority,
      "comparison_loss" -> surface.comparisonLostIdeas.map(moveMeaningSurfaceComparisonLossJson),
      "terminal_consequences" -> surface.terminalConsequences.map(moveMeaningSurfaceCodeJson),
      "endgame_technique" -> surface.endgameTechnique.map(moveMeaningSurfaceTechniqueJson),
      "technique" -> surface.endgameTechnique.map(moveMeaningSurfaceTechniqueJson),
      "comparison" -> surface.comparison.map(moveMeaningSurfaceComparisonJson),
      "evidence" -> moveMeaningSurfaceEvidenceJson(surface)
    )

  private def moveMeaningSurfaceCodeJson(code: MoveMeaningSurfaceCode): JsObject =
    Json.obj(
      "code" -> code.code,
      "label" -> code.label
    )

  private def moveMeaningSurfaceAssessmentJson(assessment: MoveMeaningSurfaceAssessment): JsObject =
    Json.obj(
      "move_quality" -> moveMeaningSurfaceCodeJson(assessment.moveQuality),
      "idea_quality" -> moveMeaningSurfaceCodeJson(assessment.ideaQuality),
      "priority" -> moveMeaningSurfaceCodeJson(assessment.priority),
      "is_verdict_reason" -> assessment.verdictReason,
      "is_local_idea" -> assessment.localIdea,
      "failure_family" -> assessment.failureFamily.map(moveMeaningSurfaceCodeJson),
      "problem" -> assessment.problem.map(moveMeaningSurfaceCodeJson)
    )

  private def moveMeaningSurfaceEvidenceJson(surface: MoveMeaningSurface): JsObject =
    val evidence = surface.evidence
    Json.obj(
      "has_carrier" -> evidence.hasCarrier,
      "proof_level" -> evidence.proofLevel,
      "target_bound" -> evidence.targetBound,
      "cause_ids" -> evidence.causeIds,
      "source_ids" -> evidence.sourceIds,
      "board_carriers" -> evidence.boardCarriers.map(moveMeaningSurfaceBoardCarrierJson(_, surface))
    )

  private def moveMeaningSurfaceBoardCarrierJson(
      carrier: MoveMeaningSurfaceBoardCarrier,
      surface: MoveMeaningSurface
  ): JsObject =
    Json.obj(
      "subject" -> surface.subject,
      "line_role" -> surface.lineRole,
      "move_uci" -> surface.moveUci,
      "role" -> carrier.role,
      "kind" -> carrier.kind,
      "value" -> carrier.value,
      "from" -> carrier.from,
      "to" -> carrier.to
    )

  private def moveMeaningSurfaceLlmPayloadJson(
      verdict: MoveMeaningSurfaceVerdict,
      surfaces: List[MoveMeaningSurface]
  ): List[JsObject] =
    val problemMove = verdict.moveQuality == "bad" || verdict.verdictCode == "playable_loss"
    val subjectSpecs =
      if problemMove then List(("reference_move", verdict.referenceMove, "reference_pv_"), ("played_move", verdict.playedMove, "played_pv_"))
      else List(("played_move", verdict.playedMove, "played_pv_"))
    subjectSpecs.flatMap { (subject, subjectMove, pvRolePrefix) =>
      val evidenceSurfaces = surfaces.filter(surface => surface.subject == subject && moveMeaningSurfaceHasLlmCarrier(surface))
      val pv = surfaces
        .filter(_.subject == subject)
        .flatMap(_.comparison.toList.flatMap(_.moves))
        .filter(move => move.uci.nonEmpty && move.role.startsWith(pvRolePrefix))
        .map(_.uci)
        .distinct
      val terminal = evidenceSurfaces.flatMap(_.terminalConsequences).distinct
      val technique = evidenceSurfaces.flatMap(_.endgameTechnique).distinct
      val consequenceCarriers = moveMeaningSurfaceLlmCarrierPairs(evidenceSurfaces)
        .filter((carrier, _) => carrier.role == "target" && moveMeaningSurfaceLlmConsequenceCarrier(carrier))
      val concreteConsequence = consequenceCarriers.exists((carrier, _) => carrier.kind == "PlanSubject" || carrier.kind == "Pawn")
      val ownedRouteCarrier = evidenceSurfaces.exists(surface =>
        surface.evidence.proofLevel == "owned_cause" &&
          surface.evidence.boardCarriers.exists(carrier => carrier.role == "actor" && carrier.kind == "Move" && carrier.value == subjectMove)
      )
      val directStructuralCarrier = evidenceSurfaces.exists { surface =>
        val carriers = surface.evidence.boardCarriers
        surface.evidence.sourceIds.exists(_.contains("structural-delta")) &&
          carriers.exists(carrier => carrier.role == "actor" && carrier.kind == "Move" && carrier.value == subjectMove) &&
          carriers.exists(carrier =>
            carrier.role == "target" &&
              (carrier.kind == "Pawn" || (carrier.kind == "PlanSubject" && carrier.value.startsWith("passed-pawn")))
          )
      }
      if evidenceSurfaces.isEmpty ||
        (terminal.isEmpty && technique.isEmpty && !directStructuralCarrier && (pv.isEmpty || (!concreteConsequence && !ownedRouteCarrier)))
      then Nil
      else
        val carriers = moveMeaningSurfaceLlmCarrierPairs(evidenceSurfaces).filter((carrier, _) =>
          (carrier.role == "actor" && carrier.kind == "Move" && carrier.value == subjectMove) ||
            (carrier.role == "target" && moveMeaningSurfaceLlmConsequenceCarrier(carrier))
        )
        List(
        Json.obj(
          "key" -> "current-move-chain",
          "current_move" -> verdict.playedMove,
          "reference_move" -> verdict.referenceMove,
          "move_quality" -> verdict.moveQuality,
          "subject" -> subject,
          "proof_levels" -> evidenceSurfaces.map(_.evidence.proofLevel).distinct,
          "carriers" -> carriers.map((carrier, surface) => moveMeaningSurfaceBoardCarrierJson(carrier, surface)),
          "pv" -> pv,
          "consequence_carriers" -> consequenceCarriers.map((carrier, surface) => moveMeaningSurfaceBoardCarrierJson(carrier, surface)),
          "terminal_consequences" -> terminal.map(moveMeaningSurfaceCodeJson),
          "technique" -> technique.map(moveMeaningSurfaceTechniqueJson),
          "cause_ids" -> evidenceSurfaces.flatMap(_.evidence.causeIds).distinct,
          "source_ids" -> evidenceSurfaces.flatMap(_.evidence.sourceIds).distinct,
          "player_facing_reason_allowed" -> true
        )
      )
    }

  private def moveMeaningSurfaceHasLlmCarrier(surface: MoveMeaningSurface): Boolean =
    (surface.evidence.proofLevel == "owned_cause" || surface.evidence.proofLevel == "terminal_proof") &&
      surface.evidence.hasCarrier &&
      (surface.evidence.boardCarriers.nonEmpty || surface.terminalConsequences.nonEmpty || surface.endgameTechnique.nonEmpty)

  private def moveMeaningSurfaceLlmConsequenceCarrier(carrier: MoveMeaningSurfaceBoardCarrier): Boolean =
    carrier.kind match
      case "PlanSubject" => !carrier.value.contains(",")
      case "Pawn" | "Square" | "File" => true
      case _ => false

  private def moveMeaningSurfaceLlmCarrierPairs(
      surfaces: List[MoveMeaningSurface]
  ): List[(MoveMeaningSurfaceBoardCarrier, MoveMeaningSurface)] =
    surfaces
      .flatMap(surface => surface.evidence.boardCarriers.map(carrier => carrier -> surface))
      .distinctBy((carrier, surface) => (surface.subject, surface.lineRole, surface.moveUci, carrier.role, carrier.kind, carrier.value, carrier.from, carrier.to))

  private def moveMeaningSurfaceTechniqueJson(technique: MoveMeaningSurfaceEndgameTechnique): JsObject =
    Json.obj(
      "pattern" -> technique.pattern,
      "pattern_label" -> technique.patternLabel,
      "pattern_info" -> technique.pattern.zip(technique.patternLabel).headOption.map { case (code, label) =>
        moveMeaningSurfaceCodeJson(MoveMeaningSurfaceCode(code, label))
      },
      "rook_pattern" -> technique.rookPattern,
      "rook_pattern_label" -> technique.rookPatternLabel,
      "rook_geometry" -> technique.rookPattern.zip(technique.rookPatternLabel).headOption.map { case (code, label) =>
        moveMeaningSurfaceCodeJson(MoveMeaningSurfaceCode(code, label))
      },
      "side" -> technique.side,
      "status" -> technique.horizonStatus,
      "status_label" -> technique.statusLabel,
      "trigger_move" -> technique.triggerMove,
      "status_move" -> technique.triggerMove.map(move => Json.obj("uci" -> move)),
      "entry_ply_offset" -> technique.entryPlyOffset,
      "terminal_ply_offset" -> technique.terminalPlyOffset,
      "required_squares" -> technique.requiredSquares,
      "maintained_squares" -> technique.maintainedSquares,
      "broken_squares" -> technique.brokenSquares,
      "squares" -> Json.obj(
        "required" -> technique.requiredSquares,
        "maintained" -> technique.maintainedSquares,
        "broken" -> technique.brokenSquares
      ),
      "terminal_consequences" -> technique.terminalConsequences.map(moveMeaningSurfaceCodeJson),
      "failure_reason" -> technique.failureReason.map(moveMeaningSurfaceCodeJson)
    )

  private def moveMeaningSurfaceComparisonJson(comparison: MoveMeaningSurfaceComparison): JsObject =
    Json.obj(
      "kind" -> comparison.kind,
      "relation" -> comparison.relation,
      "reference_move" -> comparison.referenceMove,
      "candidate_move" -> comparison.candidateMove,
      "second_move" -> comparison.secondMove,
      "moves" -> comparison.moves.map(moveMeaningSurfaceMoveRefJson),
      "lost_ideas" -> comparison.lostIdeas.map(moveMeaningSurfaceComparisonLossJson)
    )

  private def moveMeaningSurfaceMoveRefJson(move: MoveMeaningSurfaceMoveRef): JsObject =
    Json.obj(
      "role" -> move.role,
      "uci" -> move.uci
    )

  private def moveMeaningSurfaceComparisonLossJson(loss: MoveMeaningSurfaceComparisonLoss): JsObject =
    Json.obj(
      "side" -> loss.side,
      "code" -> loss.code,
      "label" -> loss.label
    )

  private def moveMeaningSurfaceTargetJson(target: MoveMeaningSurfaceTarget): JsObject =
    Json.obj(
      "squares" -> target.squares,
      "files" -> target.files,
      "pieces" -> target.pieces
    )

  def positionPlanTechniqueObjectBindingsSampleJson(
      bindings: List[PositionPlanTechniqueObjectBinding]
  ): JsObject =
    Json.obj(
      "objectBindingCount" -> bindings.size,
      "objectBindingsSample" -> bindings.take(5).map(positionPlanTechniqueObjectBindingJson)
    )

  def positionPlanTechniqueFrameJson(frame: PositionPlanTechniqueFrame, lineRefSummary: LineNodeRef => JsObject): JsObject =
    Json.obj(
      "id" -> frame.id,
      "units" -> frame.units.map(_.toString),
      "positionId" -> frame.position.id,
      "line" -> frame.line.map(lineRefSummary),
      "moveUci" -> frame.moveUci,
      "scope" -> frame.scope.toString,
      "mechanismKinds" -> frame.mechanismKinds.map(_.toString),
      "strategicAxisKeys" -> frame.strategicAxisKeys,
      "semanticAnchors" -> frame.semanticAnchors.map(semanticAnchorJson),
      "semanticDetails" -> frame.semanticDetails.map(positionPlanTechniqueSemanticDetailJson),
      "evidenceIds" -> frame.evidenceIds,
      "mechanismEvidenceIds" -> frame.mechanismEvidenceIds,
      "sourceEvidenceIds" -> frame.sourceEvidenceIds,
      "relativeCauseEvidenceIds" -> frame.relativeCauseEvidenceIds,
      "ideaIds" -> frame.ideaIds,
      "claimIds" -> frame.claimIds,
      "planComparison" -> frame.planComparison.map(planComparisonJson),
      "relationToVerdict" -> frame.relationToVerdict.map(_.toString),
      "confidence" -> frame.confidence.toString,
      "salience" -> frame.salience
    ) ++ objectBindingSignatureSampleJson(frame.objectBindingSignatures) ++
      positionPlanTechniqueObjectBindingsSampleJson(frame.objectBindings)

  private def positionPlanTechniqueObjectBindingJson(binding: PositionPlanTechniqueObjectBinding): JsObject =
    Json.obj(
      "sourceEvidenceId" -> binding.sourceEvidenceId,
      "actor" -> binding.actor,
      "target" -> binding.target,
      "mechanism" -> binding.mechanism,
      "consequence" -> binding.consequence,
      "witness" -> binding.witness,
      "lineId" -> binding.lineId,
      "lineRootMove" -> binding.lineRootMove,
      "horizon" -> binding.horizon,
      "proofRole" -> binding.proofRole.map(_.toString),
      "signature" -> binding.signature
    )

  private def positionPlanTechniqueSemanticDetailJson(detail: PositionPlanTechniqueSemanticDetail): JsObject =
    Json.obj(
      "unit" -> detail.unit.toString,
      "axisKey" -> detail.axisKey,
      "axisKind" -> detail.axisKind.map(_.toString),
      "axisPolarity" -> detail.axisPolarity.map(_.toString),
      "label" -> detail.label,
      "mechanismKinds" -> detail.mechanismKinds.map(_.toString),
      "semanticAnchorKeys" -> detail.semanticAnchorKeys,
      "contrastOutcome" -> detail.contrastOutcome.map(_.toString),
      "referenceStrength" -> detail.referenceStrength,
      "candidateStrength" -> detail.candidateStrength,
      "narrativeHorizon" -> detail.narrativeHorizon.map(_.toString),
      "raceLeadingLineRole" -> detail.raceLeadingLineRole.map(_.toString),
      "raceReferenceRootMove" -> detail.raceReferenceRootMove,
      "raceCandidateRootMove" -> detail.raceCandidateRootMove,
      "referenceEvidenceIds" -> detail.referenceEvidenceIds,
      "candidateEvidenceIds" -> detail.candidateEvidenceIds,
      "referencePlanIds" -> detail.referencePlanIds,
      "candidatePlanIds" -> detail.candidatePlanIds,
      "threatKind" -> detail.threatKind,
      "threatDriver" -> detail.threatDriver,
      "threatSeverity" -> detail.threatSeverity,
      "turnsToImpact" -> detail.turnsToImpact,
      "defenseMove" -> detail.defenseMove,
      "prophylaxisNeeded" -> detail.prophylaxisNeeded,
      "maxWinPercentLossIfIgnored" -> detail.maxWinPercentLossIfIgnored,
      "pawnBreakReady" -> detail.pawnBreakReady,
      "breakFile" -> detail.breakFile,
      "breakImpact" -> detail.breakImpact,
      "advanceOrCapture" -> detail.advanceOrCapture,
      "passedPawnUrgency" -> detail.passedPawnUrgency,
      "passerBlockade" -> detail.passerBlockade,
      "blockadeSquare" -> detail.blockadeSquare,
      "blockadeRole" -> detail.blockadeRole,
      "pusherSupport" -> detail.pusherSupport,
      "minorityAttack" -> detail.minorityAttack,
      "counterBreak" -> detail.counterBreak,
      "tensionPolicy" -> detail.tensionPolicy,
      "tensionSquares" -> detail.tensionSquares,
      "tensionEdges" -> detail.tensionEdges,
      "counterBreakFiles" -> detail.counterBreakFiles,
      "pawnPlayDriver" -> detail.pawnPlayDriver,
      "planAlignmentScore" -> detail.planAlignmentScore,
      "planAlignmentBand" -> detail.planAlignmentBand,
      "matchedPlanIds" -> detail.matchedPlanIds,
      "missingPlanIds" -> detail.missingPlanIds,
      "planAlignmentReasonCodes" -> detail.planAlignmentReasonCodes,
      "planAlignmentReasonWeights" -> detail.planAlignmentReasonWeights,
      "openingPriorLineage" -> detail.openingPriorLineage,
      "openingPriorFamily" -> detail.openingPriorFamily,
      "openingPriorThemes" -> detail.openingPriorThemes,
      "openingPriorTypicalPawnStructures" -> detail.openingPriorTypicalPawnStructures,
      "openingPriorCenterBreaks" -> detail.openingPriorCenterBreaks,
      "openingPriorDevelopmentPriorities" -> detail.openingPriorDevelopmentPriorities,
      "openingPriorGambitCompensation" -> detail.openingPriorGambitCompensation,
      "openingPriorStrategicPlanPriors" -> detail.openingPriorStrategicPlanPriors,
      "openingPriorMatchSource" -> detail.openingPriorMatchSource,
      "openingPriorRequestedLineage" -> detail.openingPriorRequestedLineage,
      "openingPriorCanonicalLineage" -> detail.openingPriorCanonicalLineage,
      "openingPriorOpeningSpecific" -> detail.openingPriorOpeningSpecific,
      "openingPriorCanCertify" -> detail.openingPriorCanCertify,
      "resourceContestActorSide" -> detail.resourceContestActorSide,
      "resourceContestTargetSide" -> detail.resourceContestTargetSide,
      "resourceContestKinds" -> detail.resourceContestKinds,
      "resourceContestSignals" -> detail.resourceContestSignals,
      "resourceContestSquares" -> detail.resourceContestSquares,
      "resourceContestFiles" -> detail.resourceContestFiles,
      "resourceContestScopes" -> detail.resourceContestScopes,
      "resourceContestMagnitude" -> detail.resourceContestMagnitude,
      "structuralRouteMove" -> detail.structuralRouteMove,
      "structuralRouteRole" -> detail.structuralRouteRole,
      "structuralRoutePerspective" -> detail.structuralRoutePerspective,
      "structuralRouteFromPly" -> detail.structuralRouteFromPly,
      "structuralRouteToPly" -> detail.structuralRouteToPly,
      "structuralPurposeConsequences" -> detail.structuralPurposeConsequences,
      "structuralPurposeSubjects" -> detail.structuralPurposeSubjects,
      "structuralPurposeCategories" -> detail.structuralPurposeCategories,
      "structuralPurposePolarities" -> detail.structuralPurposePolarities,
      "structuralPurposeStrength" -> detail.structuralPurposeStrength,
      "structuralMotifTags" -> detail.structuralMotifTags,
      "boardAnchorKinds" -> detail.boardAnchorKinds,
      "boardAnchorSignals" -> detail.boardAnchorSignals,
      "requiredSquares" -> detail.requiredSquares,
      "endgameTechniquePattern" -> detail.endgameTechniquePattern,
      "endgameTechniqueRookPattern" -> detail.endgameTechniqueRookPattern,
      "endgameTechniqueSide" -> detail.endgameTechniqueSide,
      "endgameTechniqueHorizonStatus" -> detail.endgameTechniqueHorizonStatus,
      "endgameTechniqueTriggerMove" -> detail.endgameTechniqueTriggerMove,
      "endgameTechniqueEntryPlyOffset" -> detail.endgameTechniqueEntryPlyOffset,
      "endgameTechniqueTerminalPlyOffset" -> detail.endgameTechniqueTerminalPlyOffset,
      "maintainedSquares" -> detail.maintainedSquares,
      "brokenSquares" -> detail.brokenSquares,
      "terminalConsequenceKinds" -> detail.terminalConsequenceKinds,
      "endgameTechniqueFailureReason" -> detail.endgameTechniqueFailureReason,
      "anchorMagnitude" -> detail.anchorMagnitude,
      "sourceEvidenceIds" -> detail.sourceEvidenceIds,
      "causeEvidenceIds" -> detail.causeEvidenceIds,
      "proofRoles" -> detail.proofRoles.map(_.toString),
      "contextCauseEvidenceIds" -> detail.contextCauseEvidenceIds,
      "contextProofRoles" -> detail.contextProofRoles.map(_.toString),
      "specificityTier" -> detail.specificityTier.toString
    ) ++ objectBindingSignatureSampleJson(detail.objectBindingSignatures)

  private def semanticAnchorJson(anchor: EvidenceSemanticAnchor): JsObject =
    Json.obj(
      "kind" -> anchor.kind.toString,
      "values" -> anchor.values,
      "stableKey" -> anchor.stableKey
    )

  private def planComparisonJson(plan: StrategicPlanComparison): JsObject =
    Json.obj(
      "referencePlanIds" -> plan.referencePlanIds,
      "candidatePlanIds" -> plan.candidatePlanIds,
      "outcome" -> plan.outcome.toString,
      "hasPlanDelta" -> plan.hasPlanDelta
    )
