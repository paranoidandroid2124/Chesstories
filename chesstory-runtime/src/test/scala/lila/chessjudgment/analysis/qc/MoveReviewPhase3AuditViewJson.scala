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
    MoveMeaningSurface.publicPayloadJson(view)

  def endgameTechniqueLineObservationJson(
      observation: Option[EndgameTechniqueLineObservation]
  ): Option[JsObject] =
    observation.map(value =>
      Json.obj(
        "kingRouteMoves" -> value.kingRouteMoves,
        "tempoRecipient" -> value.tempoRecipient,
        "opponentReply" -> value.opponentReply,
        "followUpPawnMove" -> value.followUpPawnMove,
        "replyToPawnMove" -> value.replyToPawnMove,
        "newPassedPawnSquares" -> value.newPassedPawnSquares
      )
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
      "activePlanIds" -> detail.activePlanIds.map(_.toString),
      "displayPlanId" -> detail.displayPlanId.map(_.toString),
      "previousPlanId" -> detail.previousPlanId.map(_.toString),
      "planTransitionType" -> detail.planTransitionType.map(_.toString),
      "planMoveRole" -> detail.planMoveRole.map(_.toString),
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
      "structuralConsequenceKinds" -> detail.structuralConsequenceKinds.map(_.toString),
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
      "endgameTechniqueLineObservation" -> endgameTechniqueLineObservationJson(detail.endgameTechniqueLineObservation),
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
