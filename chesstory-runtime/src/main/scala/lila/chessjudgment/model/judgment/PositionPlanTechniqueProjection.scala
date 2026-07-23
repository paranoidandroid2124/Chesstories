package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.singlePosition.{ PawnPlayAnalysis, ThreatSeverity }
import lila.chessjudgment.model.{ Motif, PlanId, TransitionType }
import lila.chessjudgment.model.structure.PlanAlignment

enum PositionPlanTechniqueUnit:
  case TensionBreakPolicyRoute
  case PlanOptionSet
  case EndgameTechniqueRecipe
  case SpacePreventionResourceDenial
  case CounterplayRace
  case PieceRerouteRoute
  case StructuralTransformation
  case CompensationSource

enum PositionPlanTechniqueSpecificityTier:
  case ExactObjectAxis
  case ConcreteObjectAxis
  case BroadAxis
  case ContextOnly

case class EndgameTechniqueLineObservation(
    kingRouteMoves: List[String],
    tempoRecipient: String,
    opponentReply: Option[String] = None,
    followUpPawnMove: Option[String] = None,
    replyToPawnMove: Option[String] = None,
    newPassedPawnSquares: List[String] = Nil
)

case class PositionPlanTechniqueSemanticDetail(
    unit: PositionPlanTechniqueUnit,
    axisKey: Option[String] = None,
    axisKind: Option[StrategicAxisKind] = None,
    axisPolarity: Option[StrategicAxisPolarity] = None,
    label: Option[String] = None,
    mechanismKinds: List[StrategicMechanismKind] = Nil,
    semanticAnchorKeys: List[String] = Nil,
    contrastOutcome: Option[StrategicAxisComparisonOutcome] = None,
    referenceStrength: Option[Int] = None,
    candidateStrength: Option[Int] = None,
    narrativeHorizon: Option[StrategicSustainabilityHorizon] = None,
    raceLeadingLineRole: Option[LineNodeRole] = None,
    raceReferenceRootMove: Option[String] = None,
    raceCandidateRootMove: Option[String] = None,
    referenceEvidenceIds: List[String] = Nil,
    candidateEvidenceIds: List[String] = Nil,
    referencePlanIds: List[String] = Nil,
    candidatePlanIds: List[String] = Nil,
    activePlanIds: List[PlanId] = Nil,
    displayPlanId: Option[PlanId] = None,
    mainExplanationCandidate: Boolean = true,
    previousPlanId: Option[PlanId] = None,
    planTransitionType: Option[TransitionType] = None,
    planMoveRole: Option[PlanMoveRole] = None,
    threatKind: Option[String] = None,
    threatDriver: Option[String] = None,
    threatSeverity: Option[String] = None,
    turnsToImpact: Option[Int] = None,
    defenseMove: Option[String] = None,
    prophylaxisNeeded: Option[Boolean] = None,
    maxWinPercentLossIfIgnored: Option[Double] = None,
    pawnBreakReady: Option[Boolean] = None,
    breakFile: Option[String] = None,
    breakImpact: Option[Int] = None,
    advanceOrCapture: Option[Boolean] = None,
    passedPawnUrgency: Option[String] = None,
    passerBlockade: Option[Boolean] = None,
    blockadeSquare: Option[String] = None,
    blockadeRole: Option[String] = None,
    pusherSupport: Option[Boolean] = None,
    minorityAttack: Option[Boolean] = None,
    counterBreak: Option[Boolean] = None,
    tensionPolicy: Option[String] = None,
    tensionSquares: List[String] = Nil,
    tensionEdges: List[String] = Nil,
    counterBreakFiles: List[String] = Nil,
    pawnPlayDriver: Option[String] = None,
    planAlignmentScore: Option[Int] = None,
    planAlignmentBand: Option[String] = None,
    matchedPlanIds: List[String] = Nil,
    missingPlanIds: List[String] = Nil,
    planAlignmentReasonCodes: List[String] = Nil,
    planAlignmentReasonWeights: Map[String, Double] = Map.empty,
    openingPriorLineage: Option[String] = None,
    openingPriorFamily: Option[String] = None,
    openingPriorThemes: List[String] = Nil,
    openingPriorTypicalPawnStructures: List[String] = Nil,
    openingPriorCenterBreaks: List[String] = Nil,
    openingPriorDevelopmentPriorities: List[String] = Nil,
    openingPriorGambitCompensation: Option[Boolean] = None,
    openingPriorStrategicPlanPriors: List[String] = Nil,
    openingPriorMatchSource: Option[String] = None,
    openingPriorRequestedLineage: Option[String] = None,
    openingPriorCanonicalLineage: Option[String] = None,
    openingPriorOpeningSpecific: Option[Boolean] = None,
    openingPriorCanCertify: Option[Boolean] = None,
    resourceContestActorSide: Option[String] = None,
    resourceContestTargetSide: Option[String] = None,
    resourceContestKinds: List[String] = Nil,
    resourceContestSignals: List[String] = Nil,
    resourceContestSquares: List[String] = Nil,
    resourceContestFiles: List[String] = Nil,
    resourceContestScopes: List[String] = Nil,
    resourceContestMagnitude: Option[Int] = None,
    structuralRouteMove: Option[String] = None,
    structuralRouteRole: Option[String] = None,
    structuralRoutePerspective: Option[String] = None,
    structuralRouteFromPly: Option[Int] = None,
    structuralRouteToPly: Option[Int] = None,
    structuralPurposeConsequences: List[String] = Nil,
    structuralConsequenceKinds: List[TransitionConsequenceKind] = Nil,
    structuralPurposeSubjects: List[String] = Nil,
    structuralPurposeCategories: List[String] = Nil,
    structuralPurposePolarities: List[String] = Nil,
    structuralPurposeStrength: Option[Int] = None,
    structuralMotifTags: List[String] = Nil,
    boardAnchorKinds: List[String] = Nil,
    boardAnchorSignals: List[String] = Nil,
    requiredSquares: List[String] = Nil,
    endgameTechniquePattern: Option[String] = None,
    endgameTechniqueRookPattern: Option[String] = None,
    endgameTechniqueSide: Option[String] = None,
    endgameTechniqueHorizonStatus: Option[String] = None,
    endgameTechniqueTriggerMove: Option[String] = None,
    endgameTechniqueEntryPlyOffset: Option[Int] = None,
    endgameTechniqueTerminalPlyOffset: Option[Int] = None,
    endgameTechniqueLineObservation: Option[EndgameTechniqueLineObservation] = None,
    endgameZugzwangProof: Option[EndgameZugzwangProof] = None,
    maintainedSquares: List[String] = Nil,
    brokenSquares: List[String] = Nil,
    terminalConsequenceKinds: List[String] = Nil,
    endgameTechniqueFailureReason: Option[String] = None,
    anchorMagnitude: Option[Int] = None,
    sourceEvidenceIds: List[String] = Nil,
    positiveFunctionalProofEvidenceIds: List[String] = Nil,
    resolvedPlanEvent: Option[ResolvedPlanEvent] = None,
    causeEvidenceIds: List[String] = Nil,
    proofRoles: List[RelativeCauseProofRole] = Nil,
    contextCauseEvidenceIds: List[String] = Nil,
    contextProofRoles: List[RelativeCauseProofRole] = Nil,
    objectBindingSignatures: List[String] = Nil,
    specificityTier: PositionPlanTechniqueSpecificityTier = PositionPlanTechniqueSpecificityTier.ContextOnly
):
  def testedContinuation: Option[TestedPlanContinuation] =
    resolvedPlanEvent.flatMap(_.testedContinuation)

object PositionPlanTechniqueSemanticDetail:
  def comparisonLossSides(detail: PositionPlanTechniqueSemanticDetail): List[String] =
    val referenceHasWorseAxis = detail.axisPolarity.exists(comparisonNegativePolarity)
    val candidateHasWorseAxis = referenceHasWorseAxis
    detail.contrastOutcome match
      case Some(StrategicAxisComparisonOutcome.CandidateConcession) =>
        List("candidate")
      case Some(StrategicAxisComparisonOutcome.ReferenceOnly | StrategicAxisComparisonOutcome.ReferenceStronger) =>
        if referenceHasWorseAxis then List("reference") else List("candidate")
      case Some(StrategicAxisComparisonOutcome.ReferencePreservesPlan) =>
        if referenceHasWorseAxis then List("reference") else List("candidate")
      case Some(StrategicAxisComparisonOutcome.CandidateOnly | StrategicAxisComparisonOutcome.CandidateStronger) =>
        if candidateHasWorseAxis then List("candidate") else List("reference")
      case _ =>
        Nil

  def comparisonLossKinds(detail: PositionPlanTechniqueSemanticDetail): List[String] =
    if comparisonLossSides(detail).isEmpty then Nil
    else
      val pawnBreak =
        detail.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute ||
          detail.axisKind.contains(StrategicAxisKind.PawnBreak) ||
          detail.breakFile.exists(_.trim.nonEmpty)
      val tensionRelease =
        detail.axisPolarity.contains(StrategicAxisPolarity.Release) ||
          detail.tensionPolicy.exists(policy => policy.toLowerCase.contains("release")) ||
          detail.structuralPurposeSubjects.exists(subject => subject.toLowerCase.startsWith("resolved-tension:"))
      val tensionMaintain =
        detail.tensionPolicy.exists(policy =>
          val normalized = policy.toLowerCase
          normalized.contains("maintain") || normalized.contains("preserve")
        )
      val counterplay =
        detail.unit == PositionPlanTechniqueUnit.CounterplayRace &&
          detail.breakFile.exists(_.trim.nonEmpty) &&
          detail.counterBreakFiles.exists(_.trim.nonEmpty)
      val outpost =
        detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute &&
          detail.structuralPurposeSubjects.exists(subject =>
            StructuralPurposeSubject.parse(subject).exists(_.isInstanceOf[StructuralPurposeSubject.Outpost])
          )
      val longDiagonal =
        detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute &&
          detail.structuralPurposeSubjects.exists(subject =>
            StructuralPurposeSubject.parse(subject) match
              case Some(StructuralPurposeSubject.Battery(axis, _, _, _)) => axis.equalsIgnoreCase("diagonal")
              case _                                                     => false
          )
      val pieceRoute =
        detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute &&
          detail.structuralPurposeSubjects.exists(subject =>
            StructuralPurposeSubject.parse(subject).exists(_.isInstanceOf[StructuralPurposeSubject.PieceRoute])
          )
      val targetPressure =
        detail.axisKind.contains(StrategicAxisKind.Target) ||
          detail.structuralPurposeConsequences.exists(consequence =>
            consequence.equalsIgnoreCase(TransitionConsequenceKind.TargetPressureGain.toString) ||
              consequence.equalsIgnoreCase(TransitionConsequenceKind.TargetPressureRelease.toString)
          )
      val slowMoveOrder =
        detail.planAlignmentReasonCodes.exists(reason =>
          val normalized = reason.trim.toLowerCase
          normalized == "move_order_too_slow" || normalized == "move-order-too-slow"
        )
      val losses =
        List(
          Option.when(pawnBreak && tensionRelease && !counterplay)("tension_released_early"),
          Option.when(pawnBreak && tensionMaintain && !tensionRelease && !counterplay)("tension_preservation_missed"),
          Option.when(pawnBreak && !tensionRelease && !tensionMaintain && !counterplay)("break_option_missed"),
          Option.when(counterplay)("counter_break_allowed"),
          Option.when(outpost)("outpost_route_missed"),
          Option.when(longDiagonal)("diagonal_pressure_lost"),
          Option.when(pieceRoute && !outpost && !longDiagonal)("piece_route_missed"),
          Option.when(targetPressure)("target_pressure_lost"),
          Option.when(slowMoveOrder)("move_order_too_slow")
        ).flatten.distinct
      if losses.nonEmpty then losses
      else if detail.unit == PositionPlanTechniqueUnit.PlanOptionSet then List("plan_option_missed")
      else Nil

  private def comparisonNegativePolarity(polarity: StrategicAxisPolarity): Boolean =
    polarity == StrategicAxisPolarity.Loss ||
      polarity == StrategicAxisPolarity.Release ||
      polarity == StrategicAxisPolarity.Concede

case class PositionPlanTechniqueObjectBinding(
    sourceEvidenceId: String,
    actor: List[String],
    target: List[String],
    mechanism: List[String],
    consequence: List[String],
    witness: List[String],
    lineId: Option[String],
    lineRootMove: Option[String],
    horizon: Option[String],
    proofRole: Option[RelativeCauseProofRole],
    signature: String
)

case class PositionPlanTechniqueFrame(
    id: String,
    units: List[PositionPlanTechniqueUnit],
    position: PositionNodeRef,
    line: Option[LineNodeRef],
    moveUci: Option[String],
    scope: EvidenceScope,
    mechanismKinds: List[StrategicMechanismKind],
    strategicAxisKeys: List[String],
    semanticAnchors: List[EvidenceSemanticAnchor],
    objectBindingSignatures: List[String],
    objectBindings: List[PositionPlanTechniqueObjectBinding] = Nil,
    semanticDetails: List[PositionPlanTechniqueSemanticDetail] = Nil,
    evidenceIds: List[String],
    mechanismEvidenceIds: List[String],
    sourceEvidenceIds: List[String],
    relativeCauseEvidenceIds: List[String],
    ideaIds: List[String],
    claimIds: List[String],
    planComparison: Option[StrategicPlanComparison],
    relationToVerdict: Option[IdeaVerdictRelation],
    confidence: EvidenceConfidence,
    salience: Int
)

object PositionPlanTechniqueProjection:
  def frames(
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      ideaVerdict: Option[IdeaVerdictSplit]
  ): List[PositionPlanTechniqueFrame] =
    graph.records
      .flatMap {
        case record @ EvidenceRecord(ref, payload: StrategicMechanismEvidence, parents) =>
          mechanismPlanTechniqueFrame(record, ref, payload, parents, graph, ideas, claims, ideaVerdict).toList
        case record @ EvidenceRecord(ref, payload: StrategicMechanismContrastEvidence, parents) =>
          contrastPlanTechniqueFrame(record, ref, payload, parents, graph, ideas, claims, ideaVerdict).toList
        case record @ EvidenceRecord(ref, payload: StructuralDeltaEvidence, parents) =>
          structuralDeltaPlanTechniqueFrame(record, ref, payload, parents, graph, ideas, claims, ideaVerdict).toList
        case EvidenceRecord(ref, payload: ThreatEpisodeEvidence, parents) =>
          threatEpisodePlanTechniqueFrame(ref, payload, parents, graph, ideas, claims, ideaVerdict).toList
        case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) =>
          tacticalCausePlanTechniqueFrame(ref, cause, graph, ideas, claims, ideaVerdict).toList
        case EvidenceRecord(ref, payload: BoardFactEvidence, _) if payload.endgameTechniqueAnchors.nonEmpty =>
          boardEndgameTechniqueFrame(ref, payload, graph, ideas, claims, ideaVerdict).toList
        case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
          linePlanTechniqueFrame(ref, payload, graph, ideas, claims, ideaVerdict).toList
        case _ =>
          Nil
      }
      .filter(_.units.nonEmpty)
      .distinctBy(_.id)
      .sortBy(frame => (-frame.salience, frame.id))

  private def structuralDeltaPlanTechniqueFrame(
      record: EvidenceRecord,
      ref: EvidenceRef,
      payload: StructuralDeltaEvidence,
      parents: List[EvidenceRef],
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      ideaVerdict: Option[IdeaVerdictSplit]
  ): Option[PositionPlanTechniqueFrame] =
    if !payload.hasTypedOutput || payload.moveUci.trim.isEmpty then None
    else
      val refs = (ref :: parents).distinctBy(_.id)
      val anchors = semanticAnchorsFor(graph, record, refs)
      val evidenceIds = List(ref.id)
      val rootMoveMotifSourceIds = positionPlanTechniqueRootMoveMotifSourceIds(graph, payload)
      val structureContextSourceIds = positionPlanTechniqueStructureRouteContextSourceIds(graph, refs)
      val structureContextAnchorKeys =
        structureContextSourceIds
          .flatMap(id => graph.byId.get(id))
          .flatMap(StrategicMechanismEvidence.sourceSemanticAnchors)
          .map(_.stableKey)
          .distinct
          .sorted
      val semanticDetails =
        positionPlanTechniqueEnrichedDetails(
          structuralDeltaPlanTechniqueDetails(
            payload,
            anchors,
            evidenceIds,
            rootMoveMotifSourceIds,
            structureContextSourceIds,
            structureContextAnchorKeys
          ),
          graph
        )
      val relativeCauseEvidenceIds = semanticDetails.flatMap(_.causeEvidenceIds).distinct.sorted
      val linkedEvidenceIds = (evidenceIds ++ relativeCauseEvidenceIds).toSet
      val ideaIds = positionPlanTechniqueIdeaIds(ideas, linkedEvidenceIds)
      val claimIds = positionPlanTechniqueClaimIds(claims, linkedEvidenceIds, ideaIds.toSet)
      val frameUnits = semanticDetails.map(_.unit).distinct.sortBy(_.toString)
      Option.when(frameUnits.nonEmpty) {
        val objectBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, List(ref))
        PositionPlanTechniqueFrame(
          id = s"position-plan-technique:${ref.id}:structural-delta",
          units = frameUnits,
          position = ref.position,
          line = payload.line.orElse(ref.line),
          moveUci = Some(payload.moveUci.trim.toLowerCase),
          scope = ref.scope,
          mechanismKinds = structuralDeltaPlanTechniqueMechanismKinds(frameUnits),
          strategicAxisKeys = Nil,
          semanticAnchors = anchors,
          objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
          objectBindings = positionPlanTechniqueObjectBindings(objectBindings),
          semanticDetails = semanticDetails,
          evidenceIds = evidenceIds,
          mechanismEvidenceIds = Nil,
          sourceEvidenceIds = parents.map(_.id).distinct.sorted,
          relativeCauseEvidenceIds = relativeCauseEvidenceIds,
          ideaIds = ideaIds,
          claimIds = claimIds,
          planComparison = None,
          relationToVerdict = positionPlanTechniqueRelation(ideaVerdict, ideaIds.toSet),
          confidence = ref.confidence,
          salience =
            payload.consequences.map(_.strength).sum +
              payload.signals.size +
              frameUnits.size +
              ideaIds.size +
              claimIds.size +
              relativeCauseEvidenceIds.size
        )
      }

  private def structuralDeltaPlanTechniqueDetails(
      payload: StructuralDeltaEvidence,
      anchors: List[EvidenceSemanticAnchor],
      evidenceIds: List[String],
      rootMoveMotifSourceIds: List[String],
      structureContextSourceIds: List[String],
      structureContextAnchorKeys: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    val anchorKeys = anchors.map(_.stableKey).distinct.sorted
    val structureRouteContext = positionPlanTechniqueStructureRouteContextKeys(structureContextAnchorKeys, Nil)
    positionPlanTechniqueStructuralPurposes(evidenceIds.head, payload)
      .filter(structuralDeltaPlanTechniqueConcretePurpose)
      .flatMap(purpose =>
        val contextApplies = structureRouteContext && positionPlanTechniqueDevelopmentRoutePurpose(purpose)
        val detailEvidenceIds =
          (
            evidenceIds ++
              rootMoveMotifSourceIds ++
              Option.when(contextApplies)(structureContextSourceIds).toList.flatten
          ).distinct.sorted
        val detailAnchorKeys =
          if contextApplies then (anchorKeys ++ structureContextAnchorKeys).distinct.sorted else anchorKeys
        structuralDeltaPlanTechniqueUnits(purpose, structureRouteContext).map(unit =>
          PositionPlanTechniqueSemanticDetail(
            unit = unit,
            semanticAnchorKeys = detailAnchorKeys,
            sourceEvidenceIds = detailEvidenceIds
          ).withStructuralPurpose(Some(purpose))
        )
      )
      .distinctBy(detail =>
        (
          detail.unit,
          detail.structuralRouteMove,
          detail.structuralPurposeSubjects.mkString(","),
          detail.structuralPurposeConsequences.mkString(",")
        )
      )
      .sortBy(detail => (detail.unit.toString, detail.structuralRouteMove.getOrElse(""), detail.structuralPurposeSubjects.mkString(",")))

  private def positionPlanTechniqueRootMoveMotifSourceIds(
      graph: TypedEvidenceGraph,
      delta: StructuralDeltaEvidence
  ): List[String] =
    graph.records
      .collect {
        case record @ EvidenceRecord(ref, motif: MoveMotifEvidence, _)
            if motif.isRootEvent &&
              positionPlanTechniquePublicRouteMotif(motif) &&
              EvidenceRef.sameMove(motif.rootMove, delta.moveUci) &&
              ref.position == delta.transition.from &&
              ref.line == delta.line =>
          motif.proof.kind -> record
      }
      .groupMap(_._1)(_._2)
      .values
      .map(_.minBy(_.ref.id).ref.id)
      .toList
      .sorted

  private def positionPlanTechniquePublicRouteMotif(motif: MoveMotifEvidence): Boolean =
    motif.motif match
      case _: Motif.RookBehindPassedPawn => true
      case _                             => false

  private def structuralDeltaPlanTechniqueConcretePurpose(
      purpose: PositionPlanTechniqueStructuralPurpose
  ): Boolean =
    purpose.routeMove.exists(_.trim.nonEmpty) &&
      (
        purpose.consequenceKinds.nonEmpty ||
        purpose.transitionRouteSubject.exists(_.trim.nonEmpty) ||
          purpose.subjects.exists(positionPlanTechniqueConcreteSubject)
      )

  private def structuralDeltaPlanTechniqueUnits(
      purpose: PositionPlanTechniqueStructuralPurpose,
      structureRouteContext: Boolean
  ): List[PositionPlanTechniqueUnit] =
    (
      Option
        .when(
          purpose.consequenceKinds.nonEmpty ||
            purpose.subjects.exists(positionPlanTechniqueConcreteSubject) ||
            (structureRouteContext && positionPlanTechniqueDevelopmentRoutePurpose(purpose))
        )(
          PositionPlanTechniqueUnit.StructuralTransformation
        )
        .toList ++
        Option.when(positionPlanTechniqueDevelopmentRoutePurpose(purpose))(PositionPlanTechniqueUnit.PieceRerouteRoute).toList ++
        Option.when(structuralDeltaPawnBreakPurpose(purpose))(PositionPlanTechniqueUnit.TensionBreakPolicyRoute).toList
    ).distinct.sortBy(_.toString)

  private def structuralDeltaPawnBreakPurpose(
      purpose: PositionPlanTechniqueStructuralPurpose
  ): Boolean =
    purpose.subjects.exists(subject =>
      val normalized = subject.toLowerCase
      normalized.startsWith("break-file:") ||
        normalized.startsWith("created-tension:") ||
        normalized.startsWith("resolved-tension:")
    ) ||
      purpose.consequenceKinds.exists(positionPlanTechniquePawnTensionConsequence)

  private def structuralDeltaPlanTechniqueMechanismKinds(
      units: List[PositionPlanTechniqueUnit]
  ): List[StrategicMechanismKind] =
    (
      List(StrategicMechanismKind.StructuralImprovement) ++
        Option.when(units.contains(PositionPlanTechniqueUnit.PieceRerouteRoute))(StrategicMechanismKind.Activity).toList ++
        Option.when(units.contains(PositionPlanTechniqueUnit.TensionBreakPolicyRoute))(StrategicMechanismKind.PawnStructure).toList
    ).distinct.sortBy(_.toString)

  private def mechanismPlanTechniqueFrame(
      record: EvidenceRecord,
      ref: EvidenceRef,
      payload: StrategicMechanismEvidence,
      parents: List[EvidenceRef],
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      ideaVerdict: Option[IdeaVerdictSplit]
  ): Option[PositionPlanTechniqueFrame] =
    if !planTechniqueMechanismEligible(payload) then None
    else
      val refs = (ref :: parents ++ payload.signals.map(_.source)).distinctBy(_.id)
      val anchors = semanticAnchorsFor(graph, record, refs)
      val units = positionPlanTechniqueUnits(payload)
      val objectBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, refs)
      val evidenceIds = refs.map(_.id).distinct.sorted
      val semanticDetails =
        positionPlanTechniqueEnrichedDetails(
          mechanismPlanTechniqueDetails(payload, units, anchors, graph, refs),
          graph
        )
      val relativeCauseEvidenceIds = semanticDetails.flatMap(_.causeEvidenceIds).distinct.sorted
      val linkedEvidenceIds = (evidenceIds ++ relativeCauseEvidenceIds).toSet
      val ideaIds = positionPlanTechniqueIdeaIds(ideas, linkedEvidenceIds)
      val claimIds = positionPlanTechniqueClaimIds(claims, linkedEvidenceIds, ideaIds.toSet)
      val frameUnits = (units ++ semanticDetails.map(_.unit)).distinct.sortBy(_.toString)
      Option.when(units.nonEmpty) {
        PositionPlanTechniqueFrame(
          id = s"position-plan-technique:${ref.id}",
          units = frameUnits,
          position = ref.position,
          line = ref.line,
          moveUci = ref.line.map(_.rootMove).filter(_.nonEmpty),
          scope = ref.scope,
          mechanismKinds = List(payload.kind),
          strategicAxisKeys = payload.axisDetails.map(_.stableKey).distinct.sorted,
          semanticAnchors = anchors,
          objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
          objectBindings = positionPlanTechniqueObjectBindings(objectBindings),
          semanticDetails = semanticDetails,
          evidenceIds = evidenceIds,
          mechanismEvidenceIds = List(ref.id),
          sourceEvidenceIds = refs.map(_.id).filterNot(_ == ref.id).distinct.sorted,
          relativeCauseEvidenceIds = relativeCauseEvidenceIds,
          ideaIds = ideaIds,
          claimIds = claimIds,
          planComparison = None,
          relationToVerdict = positionPlanTechniqueRelation(ideaVerdict, ideaIds.toSet),
          confidence = ref.confidence,
          salience = payload.directStrength + frameUnits.size + ideaIds.size + claimIds.size + relativeCauseEvidenceIds.size
        )
      }

  private def contrastPlanTechniqueFrame(
      record: EvidenceRecord,
      ref: EvidenceRef,
      payload: StrategicMechanismContrastEvidence,
      parents: List[EvidenceRef],
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      ideaVerdict: Option[IdeaVerdictSplit]
  ): Option[PositionPlanTechniqueFrame] =
    if !payload.hasActionableContrast then None
    else
      val refs = (ref :: parents ++ payload.sourceRefs).distinctBy(_.id)
      val anchors = semanticAnchorsFor(graph, record, refs)
      val units =
        (payload.axisComparisons.flatMap(axis => positionPlanTechniqueUnits(axis.axis)) ++
          payload.planComparison.toList.flatMap(plan => Option.when(plan.hasPlanDelta)(PositionPlanTechniqueUnit.PlanOptionSet))).distinct
          .sortBy(_.toString)
      val objectBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, refs)
      val evidenceIds = refs.map(_.id).distinct.sorted
      val semanticDetails =
        positionPlanTechniqueEnrichedDetails(
          contrastPlanTechniqueDetails(payload, anchors, graph, refs),
          graph
        )
      val relativeCauseEvidenceIds = semanticDetails.flatMap(_.causeEvidenceIds).distinct.sorted
      val linkedEvidenceIds = (evidenceIds ++ relativeCauseEvidenceIds).toSet
      val ideaIds = positionPlanTechniqueIdeaIds(ideas, linkedEvidenceIds)
      val claimIds = positionPlanTechniqueClaimIds(claims, linkedEvidenceIds, ideaIds.toSet)
      val frameUnits = (units ++ semanticDetails.map(_.unit)).distinct.sortBy(_.toString)
      Option.when(units.nonEmpty) {
        PositionPlanTechniqueFrame(
          id = s"position-plan-technique:${ref.id}",
          units = frameUnits,
          position = ref.position,
          line = Some(payload.candidateLine),
          moveUci = Some(payload.candidateLine.rootMove).filter(_.nonEmpty),
          scope = ref.scope,
          mechanismKinds = Nil,
          strategicAxisKeys = payload.axisKeys,
          semanticAnchors = anchors,
          objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
          objectBindings = positionPlanTechniqueObjectBindings(objectBindings),
          semanticDetails = semanticDetails,
          evidenceIds = evidenceIds,
          mechanismEvidenceIds = List(ref.id),
          sourceEvidenceIds = refs.map(_.id).filterNot(_ == ref.id).distinct.sorted,
          relativeCauseEvidenceIds = relativeCauseEvidenceIds,
          ideaIds = ideaIds,
          claimIds = claimIds,
          planComparison = payload.planComparison,
          relationToVerdict = positionPlanTechniqueRelation(ideaVerdict, ideaIds.toSet),
          confidence = ref.confidence,
          salience = payload.actionableComparisons.size * 3 + frameUnits.size + ideaIds.size + claimIds.size + relativeCauseEvidenceIds.size
        )
      }

  private def threatEpisodePlanTechniqueFrame(
      ref: EvidenceRef,
      payload: ThreatEpisodeEvidence,
      parents: List[EvidenceRef],
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      ideaVerdict: Option[IdeaVerdictSplit]
  ): Option[PositionPlanTechniqueFrame] =
    Option.when(payload.isProofSignalDefensivePressure) {
      val evidenceIds = List(ref.id)
      val enrichmentRefs = (ref :: parents).distinctBy(_.id)
      val enrichmentSourceIds = enrichmentRefs.map(_.id).distinct.sorted
      val objectBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, List(ref))
      val relativeCauseEvidenceIds = relativeCauseEvidenceIdsFor(graph, evidenceIds.toSet, frameLine = ref.line)
      val linkedEvidenceIds = (evidenceIds ++ relativeCauseEvidenceIds).toSet
      val ideaIds = positionPlanTechniqueIdeaIds(ideas, linkedEvidenceIds)
      val claimIds = positionPlanTechniqueClaimIds(claims, linkedEvidenceIds, ideaIds.toSet)
      val units = threatEpisodeUnits(payload)
      val resourceContestBySourceId = positionPlanTechniqueResourceContestBySourceId(graph, enrichmentRefs)
      val resourceContest = positionPlanTechniqueResourceContestForSources(enrichmentSourceIds, resourceContestBySourceId)
      PositionPlanTechniqueFrame(
        id = s"position-plan-technique:${ref.id}",
        units = units,
        position = ref.position,
        line = ref.line,
        moveUci = payload.onlyDefense
          .orElse(payload.episode.bestDefense)
          .orElse(ref.line.map(_.rootMove))
          .filter(_.nonEmpty),
        scope = ref.scope,
        mechanismKinds = Nil,
        strategicAxisKeys = Nil,
        semanticAnchors = threatEpisodeSemanticAnchors(payload),
        objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
        objectBindings = positionPlanTechniqueObjectBindings(objectBindings),
        semanticDetails = positionPlanTechniqueEnrichedDetails(
          threatEpisodePlanTechniqueDetails(payload, units, enrichmentSourceIds)
            .map(_.withResourceContest(resourceContest)),
          graph
        ),
        evidenceIds = evidenceIds,
        mechanismEvidenceIds = Nil,
        sourceEvidenceIds = Nil,
        relativeCauseEvidenceIds = relativeCauseEvidenceIds,
        ideaIds = ideaIds,
        claimIds = claimIds,
        planComparison = None,
        relationToVerdict = positionPlanTechniqueRelation(ideaVerdict, ideaIds.toSet),
        confidence = ref.confidence,
        salience = threatEpisodeSalience(payload) + units.size + ideaIds.size + claimIds.size + relativeCauseEvidenceIds.size
      )
    }

  private def tacticalCausePlanTechniqueFrame(
      ref: EvidenceRef,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      ideaVerdict: Option[IdeaVerdictSplit]
  ): Option[PositionPlanTechniqueFrame] =
    if !tacticalCausePlanTechniqueEligible(cause) then None
    else
      val sourceRefs = tacticalCausePlanTechniqueSourceRefs(cause)
      val evidenceIds = (ref.id :: sourceRefs.map(_.id)).distinct.sorted
      val semanticDetails =
        positionPlanTechniqueEnrichedDetails(
          tacticalCausePlanTechniqueDetails(cause, graph, evidenceIds),
          graph
        )
      val frameUnits = semanticDetails.map(_.unit).distinct.sortBy(_.toString)
      Option.when(semanticDetails.nonEmpty) {
        val relativeCauseEvidenceIds = (ref.id :: semanticDetails.flatMap(_.causeEvidenceIds)).distinct.sorted
        val linkedEvidenceIds = (evidenceIds ++ relativeCauseEvidenceIds).toSet
        val ideaIds = positionPlanTechniqueIdeaIds(ideas, linkedEvidenceIds)
        val claimIds = positionPlanTechniqueClaimIds(claims, linkedEvidenceIds, ideaIds.toSet)
        val objectBindings = EvidenceObjectBinding.fromRelativeCause(cause, graph)
        PositionPlanTechniqueFrame(
          id = s"position-plan-technique:${ref.id}:tactical-proof",
          units = frameUnits,
          position = ref.position,
          line = Some(cause.eventLine),
          moveUci = Some(cause.eventRootMove).filter(_.nonEmpty),
          scope = ref.scope,
          mechanismKinds = Nil,
          strategicAxisKeys = Nil,
          semanticAnchors = tacticalCauseSemanticAnchors(cause),
          objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
          objectBindings = positionPlanTechniqueObjectBindings(objectBindings),
          semanticDetails = semanticDetails,
          evidenceIds = evidenceIds,
          mechanismEvidenceIds = Nil,
          sourceEvidenceIds = sourceRefs.map(_.id).distinct.sorted,
          relativeCauseEvidenceIds = relativeCauseEvidenceIds,
          ideaIds = ideaIds,
          claimIds = claimIds,
          planComparison = None,
          relationToVerdict = positionPlanTechniqueRelation(ideaVerdict, ideaIds.toSet),
          confidence = ref.confidence,
          salience = semanticDetails.size * 3 + ideaIds.size + claimIds.size + relativeCauseEvidenceIds.size
        )
      }

  private def tacticalCausePlanTechniqueEligible(cause: RelativeCauseFact): Boolean =
    cause.attribution.rootMoveMatched &&
      cause.hasOwnedTacticalProof &&
      tacticalCausePlanTechniqueSections(cause).nonEmpty

  private def tacticalCausePlanTechniqueSections(cause: RelativeCauseFact): List[RelativeCauseProofSection] =
    cause.proof.toList
      .flatMap(proof => List(proof.directProof, proof.contrastProof))
      .filter(_.hasTacticalProof)

  private def tacticalCausePlanTechniqueSourceRefs(cause: RelativeCauseFact): List[EvidenceRef] =
    tacticalCausePlanTechniqueSections(cause)
      .flatMap(section =>
        section.sourceRefs ++
          section.tacticalMechanisms.flatMap(_.signals.flatMap(_.source))
      )
      .distinctBy(_.id)

  private def tacticalCausePlanTechniqueDetails(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      evidenceIds: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    val sections = tacticalCausePlanTechniqueSections(cause)
    val terminalDetails =
      cause.proof.toList.flatMap { proof =>
        val mainThreatLineRank = proof.directProof.sourceRefs.flatMap(_.line.collect {
          case line if line.role == LineNodeRole.Threat => line.rank
        }).minOption
        proof.directProof.lineConsequences.filter(consequence =>
          (
            consequence.rootMoveMatched(cause.eventRootMove) ||
              tacticalCauseAllowsOpponentPromotion(cause, consequence)
          ) &&
            consequence.source.line.forall(line =>
              (line == cause.eventLine || line.role == LineNodeRole.Threat) &&
                (line.role != LineNodeRole.Threat || mainThreatLineRank.contains(line.rank))
            )
        )
      }
        .filter(proof => LineConsequenceKind.terminalResultProof(proof.kind) && tacticalCauseTerminalDetailAllowed(cause, proof.kind))
        .map { proof =>
          val terminalTargets = proof.eventMove.toList.flatMap(tacticalCauseMoveTargetSquare)
          PositionPlanTechniqueSemanticDetail(
            unit = PositionPlanTechniqueUnit.StructuralTransformation,
            axisPolarity = Some(tacticalCauseAxisPolarity(cause)),
            label = Some("terminal-proof"),
            semanticAnchorKeys = List(tacticalCauseLineConsequenceAnchor(proof).stableKey),
            structuralRouteMove = Some(cause.eventRootMove),
            structuralPurposeConsequences = List(proof.kind.toString),
            structuralPurposeSubjects =
              if terminalTargets.nonEmpty then terminalTargets
              else tacticalCauseTargetSubjects(cause, graph, Nil),
            structuralPurposeCategories = List("TerminalProof"),
            structuralPurposePolarities = List(lineTerminalProofPolarity(proof.kind, proof.rootSide, proof.beneficiary)),
            terminalConsequenceKinds = List(proof.kind.toString),
            sourceEvidenceIds = List(proof.source.id)
          )
        }
    val defensiveSubjects = tacticalCauseTargetSubjects(cause, graph, Nil)
      .filterNot(_.matches("[a-h][1-8]"))
    val defensiveDetails =
      Option.when(tacticalCauseDefensive(cause))(
        PositionPlanTechniqueSemanticDetail(
          unit = PositionPlanTechniqueUnit.SpacePreventionResourceDenial,
          label = Some("defensive-resource"),
          structuralRouteMove = Some(cause.eventRootMove),
          structuralPurposeConsequences = tacticalCauseConsequenceLabels(sections),
          structuralPurposeSubjects = defensiveSubjects,
          structuralPurposeCategories = List("DefensiveResource"),
          structuralPurposePolarities = List("Preserve"),
          sourceEvidenceIds = evidenceIds
        )
      ).toList
    val relationDetail =
      Option
        .when(!tacticalCauseDefensive(cause) && tacticalCauseConsequenceLabels(sections).nonEmpty)(
          PositionPlanTechniqueSemanticDetail(
            unit = PositionPlanTechniqueUnit.StructuralTransformation,
            axisKind = Some(StrategicAxisKind.Target),
            axisPolarity = Some(tacticalCauseAxisPolarity(cause)),
            label = Some("tactical-proof"),
            structuralRouteMove = Some(cause.eventRootMove),
            structuralPurposeConsequences = tacticalCauseConsequenceLabels(sections),
            structuralPurposeSubjects = tacticalCauseTargetSubjects(cause, graph, Nil),
            structuralPurposeCategories = List("TacticalProof"),
            structuralPurposePolarities = List(tacticalCauseAxisPolarity(cause).toString),
            sourceEvidenceIds = evidenceIds
          )
        )
        .toList
    (terminalDetails ++ defensiveDetails ++ relationDetail)
      .filter(detail => detail.structuralPurposeSubjects.nonEmpty || detail.terminalConsequenceKinds.nonEmpty)
      .distinctBy(detail => (detail.unit, detail.label, detail.terminalConsequenceKinds.mkString(","), detail.structuralPurposeSubjects.mkString(",")))

  private def tacticalCauseAllowsOpponentPromotion(
      cause: RelativeCauseFact,
      proof: LineConsequenceProof
  ): Boolean =
    cause.attribution.kind == CauseAttributionKind.CandidateAllowsLiability &&
      cause.sourceSide == RelativeCauseSourceSide.Candidate &&
      (
        cause.kind == RelativeCauseKind.TacticalRefutationOfPlayed ||
          cause.kind == RelativeCauseKind.CandidateTacticalLiability
      ) &&
      proof.source.line.contains(cause.eventLine) &&
      proof.eventMove.nonEmpty &&
      proof.rootMove.isEmpty &&
      (
        proof.kind == LineConsequenceKind.Promotion ||
          proof.kind == LineConsequenceKind.PromotionRace
      ) &&
      proof.rootSide.zip(proof.beneficiary).exists((root, beneficiary) => root != beneficiary)

  private def tacticalCauseDefensive(cause: RelativeCauseFact): Boolean =
    Set(
      RelativeCauseKind.OnlyDefenseNecessity,
      RelativeCauseKind.DefensiveResource,
      RelativeCauseKind.DrawResource
    ).contains(cause.kind)

  private def tacticalCauseTerminalDetailAllowed(
      cause: RelativeCauseFact,
      kind: LineConsequenceKind
  ): Boolean =
    cause.kind != RelativeCauseKind.SacrificeCompensation &&
      (
        kind != LineConsequenceKind.MaterialLoss ||
          cause.attribution.kind == CauseAttributionKind.CandidateAllowsLiability
      )

  private def tacticalCauseAxisPolarity(cause: RelativeCauseFact): StrategicAxisPolarity =
    cause.attribution.kind match
      case CauseAttributionKind.CandidateAllowsLiability => StrategicAxisPolarity.Loss
      case _ if tacticalCauseDefensive(cause)            => StrategicAxisPolarity.Preserve
      case _                                             => StrategicAxisPolarity.Gain

  private def tacticalCauseConsequenceLabels(sections: List[RelativeCauseProofSection]): List[String] =
    (
      sections.flatMap(_.lineConsequences.map(_.kind.toString)) ++
        sections.flatMap(_.relationProofs.map(_.kind.toString)) ++
        sections.flatMap(_.tacticalMechanisms.map(_.kind.toString)) ++
        sections.flatMap(_.threatEpisodes.map(threat => s"${threat.driver}:${threat.kind}"))
    ).distinct.sorted

  private def tacticalCauseTargetSubjects(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      moveHints: List[String]
  ): List[String] =
    val signatures = EvidenceObjectBinding.objectSignatures(EvidenceObjectBinding.fromRelativeCause(cause, graph))
    val proofTargets =
      EvidenceObjectBinding
        .objectTokens(signatures, "target", Some(RelativeCauseProofRole.DirectProof))
        .toList ++
        EvidenceObjectBinding.objectTokens(signatures, "target", Some(RelativeCauseProofRole.ContrastProof)).toList
    (
      moveHints.flatMap(tacticalCauseMoveTargetSquare) ++
        proofTargets.flatMap(tacticalCauseTargetSubject)
    ).distinct.sorted

  private def tacticalCauseTargetSubject(token: String): List[String] =
    val normalized = token.trim
    val lower = normalized.toLowerCase
    if lower.startsWith("square:") then List(normalized.drop("Square:".length).toLowerCase)
    else if lower.startsWith("file:") then List(normalized.drop("File:".length).toLowerCase)
    else if lower.startsWith("plansubject:") then List(normalized.drop("PlanSubject:".length).toLowerCase)
    else Nil

  private def tacticalCauseMoveTargetSquare(move: String): Option[String] =
    val normalized = JudgmentSubjectBinding.normalizeMove(move)
    Option.when(normalized.length >= 4)(normalized.slice(2, 4))

  private def tacticalCauseSemanticAnchors(cause: RelativeCauseFact): List[EvidenceSemanticAnchor] =
    tacticalCausePlanTechniqueSections(cause)
      .flatMap(section =>
        section.lineConsequences.map(tacticalCauseLineConsequenceAnchor) ++
          section.lineEvents.map(event => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.LineEvent, event.kind.toString, event.moveUci.getOrElse("")))
      )
      .distinctBy(_.stableKey)
      .sortBy(_.stableKey)

  private def tacticalCauseLineConsequenceAnchor(proof: LineConsequenceProof): EvidenceSemanticAnchor =
    EvidenceSemanticAnchor.of(
      EvidenceSemanticAnchorKind.LineConsequence,
      "TacticalProof",
      proof.kind.toString,
      proof.eventMove.getOrElse("")
    )

  private def boardEndgameTechniqueFrame(
      ref: EvidenceRef,
      payload: BoardFactEvidence,
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      ideaVerdict: Option[IdeaVerdictSplit]
  ): Option[PositionPlanTechniqueFrame] =
    val anchors = payload.endgameTechniqueAnchors
    Option.when(anchors.nonEmpty) {
      val evidenceIds = List(ref.id)
      val objectBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, List(ref))
      val relativeCauseEvidenceIds = relativeCauseEvidenceIdsFor(graph, evidenceIds.toSet, frameLine = ref.line)
      val linkedEvidenceIds = (evidenceIds ++ relativeCauseEvidenceIds).toSet
      val ideaIds = positionPlanTechniqueIdeaIds(ideas, linkedEvidenceIds)
      val claimIds = positionPlanTechniqueClaimIds(claims, linkedEvidenceIds, ideaIds.toSet)
      PositionPlanTechniqueFrame(
        id = s"position-plan-technique:${ref.id}",
        units = List(PositionPlanTechniqueUnit.EndgameTechniqueRecipe),
        position = ref.position,
        line = ref.line,
        moveUci = ref.line.map(_.rootMove).filter(_.nonEmpty),
        scope = ref.scope,
        mechanismKinds = List(StrategicMechanismKind.Endgame),
        strategicAxisKeys = Nil,
        semanticAnchors = anchors.map(_.semanticGroupingAnchor).distinctBy(_.stableKey).sortBy(_.stableKey),
        objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
        objectBindings = positionPlanTechniqueObjectBindings(objectBindings),
        semanticDetails = positionPlanTechniqueEnrichedDetails(
          endgameTechniquePlanDetails(anchors, evidenceIds),
          graph
        ),
        evidenceIds = evidenceIds,
        mechanismEvidenceIds = Nil,
        sourceEvidenceIds = Nil,
        relativeCauseEvidenceIds = relativeCauseEvidenceIds,
        ideaIds = ideaIds,
        claimIds = claimIds,
        planComparison = None,
        relationToVerdict = positionPlanTechniqueRelation(ideaVerdict, ideaIds.toSet),
        confidence = ref.confidence,
        salience = anchors.map(_.magnitude).sum + ideaIds.size + claimIds.size + relativeCauseEvidenceIds.size
      )
    }

  private def linePlanTechniqueFrame(
      ref: EvidenceRef,
      payload: LineFactEvidence,
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      ideaVerdict: Option[IdeaVerdictSplit]
  ): Option[PositionPlanTechniqueFrame] =
    val horizons = payload.endgameTechniqueHorizons
    val terminalConsequences = payload.proofSignalConsequences.filter(consequence =>
      lineTerminalProofConsequence(consequence) && payload.rootMove.exists(consequence.rootMoveMatched)
    )
    val evidenceIds = List(ref.id)
    val castlingContinuationDetails = lineCastlingContinuationPlanDetails(payload, evidenceIds)
    Option.when(horizons.nonEmpty || terminalConsequences.nonEmpty || castlingContinuationDetails.nonEmpty) {
      val objectBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, List(ref))
      val relativeCauseEvidenceIds = relativeCauseEvidenceIdsFor(graph, evidenceIds.toSet, frameLine = ref.line)
      val linkedEvidenceIds = (evidenceIds ++ relativeCauseEvidenceIds).toSet
      val ideaIds = positionPlanTechniqueIdeaIds(ideas, linkedEvidenceIds)
      val claimIds = positionPlanTechniqueClaimIds(claims, linkedEvidenceIds, ideaIds.toSet)
      val semanticAnchors =
        (
          horizons.map(lineEndgameTechniqueSemanticAnchor) ++
            terminalConsequences.map(lineTerminalProofSemanticAnchor) ++
            castlingContinuationDetails.flatMap(lineCastlingContinuationSemanticAnchors)
        ).distinctBy(_.stableKey).sortBy(_.stableKey)
      val units =
        (
          Option.when(horizons.nonEmpty)(PositionPlanTechniqueUnit.EndgameTechniqueRecipe).toList ++
            Option.when(terminalConsequences.nonEmpty)(PositionPlanTechniqueUnit.StructuralTransformation).toList ++
            castlingContinuationDetails.map(_.unit)
        ).distinct.sortBy(_.toString)
      PositionPlanTechniqueFrame(
        id =
          if horizons.nonEmpty || terminalConsequences.nonEmpty then s"position-plan-technique:${ref.id}:endgame-horizon"
          else s"position-plan-technique:${ref.id}:line-continuation",
        units = units,
        position = ref.position,
        line = ref.line,
        moveUci = ref.line.map(_.rootMove).filter(_.nonEmpty),
        scope = ref.scope,
        mechanismKinds =
          (
            Option.when(horizons.nonEmpty || terminalConsequences.nonEmpty)(StrategicMechanismKind.Endgame).toList ++
              Option.when(castlingContinuationDetails.nonEmpty)(StrategicMechanismKind.Activity).toList
          ).distinct.sortBy(_.toString),
        strategicAxisKeys = Nil,
        semanticAnchors = semanticAnchors,
        objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
        objectBindings = positionPlanTechniqueObjectBindings(objectBindings),
        semanticDetails = positionPlanTechniqueEnrichedDetails(
          lineEndgameTechniquePlanDetails(payload, evidenceIds) ++
            lineTerminalProofPlanDetails(terminalConsequences, evidenceIds) ++
            castlingContinuationDetails,
          graph
        ),
        evidenceIds = evidenceIds,
        mechanismEvidenceIds = Nil,
        sourceEvidenceIds = Nil,
        relativeCauseEvidenceIds = relativeCauseEvidenceIds,
        ideaIds = ideaIds,
        claimIds = claimIds,
        planComparison = None,
        relationToVerdict = positionPlanTechniqueRelation(ideaVerdict, ideaIds.toSet),
        confidence = ref.confidence,
        salience =
          horizons.size * 2 + terminalConsequences.size * 3 + castlingContinuationDetails.size * 2 +
            ideaIds.size + claimIds.size + relativeCauseEvidenceIds.size
      )
    }

  private def lineCastlingContinuationPlanDetails(
      payload: LineFactEvidence,
      evidenceIds: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    payload.rootMove.toList.flatMap { rootMove =>
      val normalizedRoot = rootMove.trim.toLowerCase
      val rootCastle =
        payload.lineEventsOf(LineEventKind.Castling).exists(event =>
          event.plyOffset == 0 && event.moveUci.trim.equalsIgnoreCase(normalizedRoot)
        )
      if !rootCastle then Nil
      else
        lineCastlingRookDestination(normalizedRoot).toList.flatMap { rookSquare =>
          payload.lineReplaySteps
            .drop(1)
            .collectFirst {
              case step if lineRookContinuationMove(step.moveUci, rookSquare) =>
                val move = step.moveUci.trim.toLowerCase
                val target = move.slice(2, 4)
                PositionPlanTechniqueSemanticDetail(
                  unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
                  label = Some("line-continuation-route"),
                  semanticAnchorKeys = lineCastlingContinuationSemanticAnchorKeys(rookSquare, target),
                  structuralRouteMove = Some(normalizedRoot),
                  structuralPurposeConsequences = List(TransitionConsequenceKind.FileOccupationGain.toString),
                  structuralPurposeSubjects = List(s"rook:$rookSquare-$target:maneuver", s"file:${target.take(1)}:$target"),
                  structuralPurposeCategories = List(
                    TransitionConsequenceCategory.PieceActivity.toString,
                    TransitionConsequenceCategory.StrategicSupport.toString
                  ),
                  structuralPurposePolarities = List(StructuralSignalPolarity.Gain.toString),
                  structuralPurposeStrength = Some(1),
                  sourceEvidenceIds = evidenceIds
                )
            }
            .toList
        }
    }

  private def lineCastlingRookDestination(moveUci: String): Option[String] =
    moveUci.trim.toLowerCase match
      case "e1g1" | "e1h1" => Some("f1")
      case "e1c1" | "e1a1" => Some("d1")
      case "e8g8" | "e8h8" => Some("f8")
      case "e8c8" | "e8a8" => Some("d8")
      case _               => None

  private def lineRookContinuationMove(moveUci: String, from: String): Boolean =
    val move = moveUci.trim.toLowerCase
    move.length >= 4 &&
      move.take(2) == from &&
      Set("a", "h").contains(move.slice(2, 3))

  private def lineCastlingContinuationSemanticAnchorKeys(from: String, to: String): List[String] =
    lineCastlingContinuationSemanticAnchors(from, to).map(_.stableKey).distinct.sorted

  private def lineCastlingContinuationSemanticAnchors(
      detail: PositionPlanTechniqueSemanticDetail
  ): List[EvidenceSemanticAnchor] =
    detail.structuralPurposeSubjects.collectFirst {
      case subject if subject.startsWith("rook:") && subject.contains("-") =>
        val route = subject.stripPrefix("rook:").takeWhile(_ != ':')
        route.split("-").toList match
          case from :: to :: Nil => lineCastlingContinuationSemanticAnchors(from, to)
          case _                 => Nil
    }.getOrElse(Nil)

  private def lineCastlingContinuationSemanticAnchors(from: String, to: String): List[EvidenceSemanticAnchor] =
    List(EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.LineEvent, LineEventKind.Castling.toString, "RookContinuation", s"$from-$to"))

  private def lineTerminalProofConsequence(consequence: LineConsequence): Boolean =
    consequence.eventMove.nonEmpty &&
      LineConsequenceKind.terminalResultProof(consequence.kind)

  private def planTechniqueMechanismEligible(payload: StrategicMechanismEvidence): Boolean =
    payload.canAnchorStrategicIdea ||
      payload.canAnchorPawnStructureIdea ||
      payload.canAnchorOpeningIdea ||
      payload.canAnchorPlanIdea ||
      payload.canSupportCompensation ||
      payload.kind == StrategicMechanismKind.Endgame ||
      payload.hasStrategicAxis

  private def threatEpisodeUnits(payload: ThreatEpisodeEvidence): List[PositionPlanTechniqueUnit] =
    (
      Option.when(payload.summary.counterThreatBetter)(
        PositionPlanTechniqueUnit.CounterplayRace
      ).toList ++
        Option.when(payload.prophylaxisNeeded || payload.defenseRequired)(
          PositionPlanTechniqueUnit.SpacePreventionResourceDenial
        ).toList
    ).distinct.sortBy(_.toString)

  private def threatEpisodeSemanticAnchors(payload: ThreatEpisodeEvidence): List[EvidenceSemanticAnchor] =
    val episode = payload.episode
    List(
      EvidenceSemanticAnchor.of(
        EvidenceSemanticAnchorKind.LineConsequence,
        "ThreatEpisode",
        episode.kind.toString,
        episode.driver.toString,
        episode.severity.toString,
        s"turns:${episode.turnsToImpact}"
      )
    )

  private def threatEpisodeSalience(payload: ThreatEpisodeEvidence): Int =
    val severityScore =
      payload.episode.severity match
        case ThreatSeverity.Urgent    => 5
        case ThreatSeverity.Important => 4
        case ThreatSeverity.Low       => 1
    severityScore +
      Option.when(payload.onlyDefense.nonEmpty)(2).getOrElse(0) +
      Option.when(payload.prophylaxisNeeded)(1).getOrElse(0)

  private def semanticAnchorsFor(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      refs: List[EvidenceRef]
  ): List[EvidenceSemanticAnchor] =
    val directAnchors =
      record.payload match
        case payload: StrategicMechanismEvidence =>
          payload.semanticGroupingAnchors
        case payload: StrategicMechanismContrastEvidence =>
          payload.axisComparisons.map(axis =>
            EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StrategicAxis, axis.axisKey)
          ) ++
            payload.planComparison.toList.flatMap(plan =>
              (plan.referencePlanIds ++ plan.candidatePlanIds).distinct.map(planId =>
                EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.Plan, planId)
              )
            )
        case _ =>
          Nil
    val sourceAnchors =
      refs
        .flatMap(ref => graph.byId.get(ref.id))
        .flatMap(StrategicMechanismEvidence.sourceSemanticAnchors)
    (directAnchors ++ sourceAnchors).distinctBy(_.stableKey).sortBy(_.stableKey)

  private def positionPlanTechniqueObjectBindings(
      bindings: List[EvidenceObjectBinding]
  ): List[PositionPlanTechniqueObjectBinding] =
    bindings
      .filter(_.hasConcreteObject)
      .map(binding =>
        PositionPlanTechniqueObjectBinding(
          sourceEvidenceId = binding.source.id,
          actor = binding.actor.map(_.signaturePart).distinct.sorted,
          target = binding.target.map(_.signaturePart).distinct.sorted,
          mechanism = binding.mechanism.map(_.signaturePart).distinct.sorted,
          consequence = binding.consequence.map(_.signaturePart).distinct.sorted,
          witness = binding.witness.map(_.signaturePart).distinct.sorted,
          lineId = binding.line.map(_.id),
          lineRootMove = binding.line.map(_.rootMove).filter(_.nonEmpty),
          horizon = binding.horizon,
          proofRole = binding.proofRole,
          signature = binding.signature
        )
      )
      .distinctBy(_.signature)
      .sortBy(binding => (binding.sourceEvidenceId, binding.signature))

  private final case class PositionPlanTechniqueDetailCauseLinkage(
      causeEvidenceIds: List[String],
      proofRoles: List[RelativeCauseProofRole],
      contextCauseEvidenceIds: List[String],
      contextProofRoles: List[RelativeCauseProofRole],
      positiveFunctionalPlanId: Option[PlanId],
      positiveFunctionalProofEvidenceIds: List[String],
      positiveFunctionalCausalEvent: Option[PlanCausalEventEvidence],
      positiveFunctionalResolvedEvent: Option[ResolvedPlanEvent],
      objectBindingSignatures: List[String],
      specificityTier: PositionPlanTechniqueSpecificityTier
  )

  private final case class PositionPlanTechniquePositiveFunctionalProof(
      planId: PlanId,
      evidenceIds: List[String],
      bindings: List[EvidenceObjectBinding],
      event: PlanCausalEventEvidence,
      resolvedEvent: ResolvedPlanEvent
  )

  private final case class PositionPlanTechniqueResourceContest(
      actorSide: Option[String],
      targetSide: Option[String],
      kinds: List[String],
      signals: List[String],
      squares: List[String],
      files: List[String],
      scopes: List[String],
      magnitude: Option[Int]
  )

  private final case class PositionPlanTechniqueStructuralPurpose(
      sourceIds: List[String],
      routeMove: Option[String],
      transitionRouteSubject: Option[String],
      routeRole: Option[String],
      routePerspective: Option[String],
      routeFromPly: Option[Int],
      routeToPly: Option[Int],
      consequenceKinds: List[TransitionConsequenceKind],
      consequences: List[String],
      subjects: List[String],
      categories: List[String],
      polarities: List[String],
      strength: Option[Int]
  )

  private final case class PositionPlanTechniqueThreatProjection(
      kind: Option[String],
      driver: Option[String],
      severity: Option[String],
      turnsToImpact: Option[Int],
      defenseMove: Option[String],
      prophylaxisNeeded: Option[Boolean],
      maxWinPercentLossIfIgnored: Option[Double]
  )

  private final case class PositionPlanTechniquePlanEvidence(
      displayPlanId: Option[PlanId],
      activePlanIds: List[PlanId],
      mainExplanationCandidate: Boolean,
      principalCausalEventAvailable: Boolean,
      previousPlanId: Option[PlanId],
      transitionType: Option[TransitionType],
      event: Option[PlanCausalEventEvidence],
      eventEvidenceId: Option[String]
  )

  private def positionPlanTechniqueEnrichedDetails(
      details: List[PositionPlanTechniqueSemanticDetail],
      graph: TypedEvidenceGraph
  ): List[PositionPlanTechniqueSemanticDetail] =
    val rawDetails =
      (
        details ++
          details.flatMap(positionPlanTechniqueMaterialCompensationDetails(_, graph))
      ).distinct
    rawDetails.map { detail =>
      val taggedDetail = positionPlanTechniqueWithStructuralMotifs(detail, graph)
      val planOwnedDetail = taggedDetail.withPlanEvidence(
        positionPlanTechniquePlanEvidenceForSources(graph, taggedDetail.sourceEvidenceIds, taggedDetail)
      )
      val causeLinkage = positionPlanTechniqueDetailCauseLinkage(planOwnedDetail, graph)
      planOwnedDetail.copy(
        displayPlanId = planOwnedDetail.displayPlanId.orElse(causeLinkage.positiveFunctionalPlanId),
        causeEvidenceIds = causeLinkage.causeEvidenceIds,
        proofRoles = causeLinkage.proofRoles,
        contextCauseEvidenceIds = causeLinkage.contextCauseEvidenceIds,
        contextProofRoles = causeLinkage.contextProofRoles,
        positiveFunctionalProofEvidenceIds = causeLinkage.positiveFunctionalProofEvidenceIds,
        resolvedPlanEvent = planOwnedDetail.resolvedPlanEvent.orElse(causeLinkage.positiveFunctionalResolvedEvent),
        objectBindingSignatures = causeLinkage.objectBindingSignatures,
        specificityTier = causeLinkage.specificityTier
      )
    }

  private def positionPlanTechniqueMaterialCompensationDetails(
      detail: PositionPlanTechniqueSemanticDetail,
      graph: TypedEvidenceGraph
  ): List[PositionPlanTechniqueSemanticDetail] =
    if detail.unit == PositionPlanTechniqueUnit.CompensationSource ||
      positionPlanTechniqueNegativeOutcome(detail) ||
      !positionPlanTechniqueConcreteCompensationCarrier(detail)
    then Nil
    else
      val localEvidenceIds =
        (
          detail.sourceEvidenceIds ++
            detail.referenceEvidenceIds ++
            detail.candidateEvidenceIds
        ).distinct.sorted
      val sacrificeEvidenceIds =
        localEvidenceIds.filter(id =>
          graph.byId
            .get(id)
            .exists(record =>
              EvidenceObjectBinding
                .objectSignatures(EvidenceObjectBinding.fromEvidenceRefs(graph, List(record.ref)))
                .exists(positionPlanTechniqueMaterialSacrificeSignature)
            )
        )
      Option
        .when(sacrificeEvidenceIds.nonEmpty)(
          detail.copy(
            unit = PositionPlanTechniqueUnit.CompensationSource,
            axisKey = None,
            axisKind = None,
            axisPolarity = None,
            sourceEvidenceIds = (sacrificeEvidenceIds ++ detail.sourceEvidenceIds).distinct.sorted,
            referenceEvidenceIds = Nil,
            candidateEvidenceIds = Nil
          )
        )
        .toList

  private def positionPlanTechniqueConcreteCompensationCarrier(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute ||
      detail.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute ||
      (
        detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
          detail.structuralConsequenceKinds.exists(kind =>
            kind == TransitionConsequenceKind.TargetPressureGain ||
              kind == TransitionConsequenceKind.TargetPressureRelease ||
              kind == TransitionConsequenceKind.PawnTensionGain ||
              kind == TransitionConsequenceKind.PawnTensionResolution ||
              kind == TransitionConsequenceKind.LineUnlockGain ||
              kind == TransitionConsequenceKind.MobilityGain ||
              kind == TransitionConsequenceKind.MobilityLoss
          )
      )

  private def positionPlanTechniqueMaterialSacrificeSignature(signature: String): Boolean =
    signature.toLowerCase.contains("target=plansubject:material-sacrifice:")

  private def positionPlanTechniqueDetailCauseLinkage(
      detail: PositionPlanTechniqueSemanticDetail,
      graph: TypedEvidenceGraph
  ): PositionPlanTechniqueDetailCauseLinkage =
    val localEvidenceIds =
      (
        detail.sourceEvidenceIds ++
          detail.referenceEvidenceIds ++
          detail.candidateEvidenceIds
      ).distinct.sorted
    val evidenceIds = localEvidenceIds
    val evidenceIdSet = localEvidenceIds.toSet
    val linkedCauseRecords = positionPlanTechniqueRelativeCauseRecordsFor(graph, evidenceIdSet)
    val localCauseRecords = positionPlanTechniqueCauseRecordsForDetail(detail, linkedCauseRecords, evidenceIdSet)
    val causeRecords =
      if positionPlanTechniqueTerminalOverriddenEndgameTechnique(detail) then Nil
      else localCauseRecords.distinctBy(_._1.id)
    val contextCauseRecords =
      if positionPlanTechniqueTerminalOverriddenEndgameTechnique(detail) then Nil
      else positionPlanTechniqueContextCauseRecordsForDetail(detail, linkedCauseRecords, evidenceIdSet)
    val causeBindings =
      causeRecords.flatMap { case (_, cause) => EvidenceObjectBinding.fromRelativeCause(cause, graph) }
    val localCauseBindings =
      causeBindings.filter(binding =>
        evidenceIdSet.contains(binding.source.id) &&
          binding.proofRole.exists(positionPlanTechniqueAdmissibleDetailProofRole)
      )
    val positiveFunctionalProof =
      positionPlanTechniquePositiveFunctionalProof(detail, graph, evidenceIds, localCauseRecords)
    val directEvidenceBindings = EvidenceObjectBinding.fromEvidenceRefs(
      graph,
      evidenceIds.flatMap(id => graph.byId.get(id).map(_.ref))
    )
    val rawDetailBindings =
      if
        detail.unit == PositionPlanTechniqueUnit.CompensationSource &&
          positiveFunctionalProof.exists(_.bindings.nonEmpty)
      then
        positiveFunctionalProof.toList.flatMap(_.bindings)
      else if detail.unit == PositionPlanTechniqueUnit.CompensationSource && localCauseBindings.nonEmpty then
        (
          localCauseBindings ++ directEvidenceBindings
        ).distinctBy(_.signature)
      else if localCauseBindings.nonEmpty then localCauseBindings
      else if positiveFunctionalProof.exists(_.bindings.nonEmpty) then positiveFunctionalProof.toList.flatMap(_.bindings)
      else directEvidenceBindings
    val structuralConsequenceKinds = detail.structuralPurposeConsequences.map(_.toLowerCase).toSet
    val carrierOwnedBindings =
      rawDetailBindings.filter(binding =>
        positionPlanTechniqueOwnsEndgameHorizonBinding(detail, binding) &&
          (
            structuralConsequenceKinds.isEmpty ||
              binding.source.layer != EvidenceLayer.StructuralDelta ||
              binding.mechanism.exists(obj => structuralConsequenceKinds(obj.key.toLowerCase))
          )
      )
    val detailBindings =
      if detail.unit == PositionPlanTechniqueUnit.CompensationSource then
        val sacrificeBindings = carrierOwnedBindings.filter(binding =>
          EvidenceObjectBinding
            .objectSignatures(List(binding))
            .exists(positionPlanTechniqueMaterialSacrificeSignature)
        )
        val sacrificeLines = sacrificeBindings.flatMap(_.line).toSet
        val resultKinds = detail.structuralConsequenceKinds.map(_.toString.toLowerCase).toSet
        val resultBindings = carrierOwnedBindings.filter(binding =>
          binding.source.layer == EvidenceLayer.StructuralDelta &&
            binding.line.exists(sacrificeLines) &&
            detail.structuralRouteMove.exists(move =>
              binding.actor.exists(obj => obj.kind == EvidenceObjectKind.Move && EvidenceRef.sameMove(obj.key, move))
            ) &&
            binding.mechanism.exists(obj => resultKinds(obj.key.toLowerCase))
        )
        if sacrificeBindings.nonEmpty && resultBindings.nonEmpty then
          (sacrificeBindings ++ resultBindings).distinctBy(_.signature)
        else sacrificeBindings
      else carrierOwnedBindings
    val objectBindingSignatures = EvidenceObjectBinding.objectSignatures(detailBindings)
    val proofRoles =
      (
        localCauseBindings.flatMap(_.proofRole) ++
          causeRecords.flatMap { case (_, cause) =>
            positionPlanTechniqueProofRolesForEvidence(cause, evidenceIdSet)
              .filter(positionPlanTechniqueAdmissibleDetailProofRole)
          }
      ).distinct.sortBy(_.toString)
    PositionPlanTechniqueDetailCauseLinkage(
      causeEvidenceIds = causeRecords.map(_._1.id).distinct.sorted,
      proofRoles = proofRoles,
      contextCauseEvidenceIds = contextCauseRecords.map(_._1.id).distinct.sorted,
      contextProofRoles =
        contextCauseRecords
          .flatMap { case (_, cause) =>
            positionPlanTechniqueProofRolesForEvidence(cause, evidenceIdSet).filter(_ == RelativeCauseProofRole.ContextSupport)
          }
          .distinct
          .sortBy(_.toString),
      positiveFunctionalPlanId = positiveFunctionalProof.map(_.planId),
      positiveFunctionalProofEvidenceIds = positiveFunctionalProof.toList.flatMap(_.evidenceIds).distinct.sorted,
      positiveFunctionalCausalEvent = positiveFunctionalProof.map(_.event),
      positiveFunctionalResolvedEvent = positiveFunctionalProof.map(_.resolvedEvent),
      objectBindingSignatures = objectBindingSignatures,
      specificityTier = positionPlanTechniqueSpecificityTier(detail, causeRecords.map(_._2), objectBindingSignatures)
    )

  private def positionPlanTechniqueOwnsEndgameHorizonBinding(
      detail: PositionPlanTechniqueSemanticDetail,
      binding: EvidenceObjectBinding
  ): Boolean =
    val mechanisms = binding.mechanism.map(_.key.trim.toLowerCase).toSet
    val isEndgameHorizon = mechanisms.contains("endgametechniquehorizon")
    val isLineHorizonDetail =
      detail.endgameTechniqueHorizonStatus.nonEmpty &&
        detail.endgameTechniqueEntryPlyOffset.nonEmpty &&
        detail.endgameTechniqueTerminalPlyOffset.nonEmpty
    if !isLineHorizonDetail then !isEndgameHorizon
    else
      detail.endgameTechniquePattern.exists { pattern =>
        val expectedHorizon =
          for
            status <- detail.endgameTechniqueHorizonStatus
            entry <- detail.endgameTechniqueEntryPlyOffset
            terminal <- detail.endgameTechniqueTerminalPlyOffset
          yield s"endgame:$status:entry=$entry:terminal=$terminal"
        isEndgameHorizon &&
          mechanisms.contains(pattern.trim.toLowerCase) &&
          detail.endgameTechniqueHorizonStatus.forall(status =>
            binding.consequence.exists(_.key.equalsIgnoreCase(status))
          ) &&
          expectedHorizon.forall(expected => binding.horizon.exists(_.equalsIgnoreCase(expected)))
      }

  private def positionPlanTechniquePositiveFunctionalProof(
      detail: PositionPlanTechniqueSemanticDetail,
      graph: TypedEvidenceGraph,
      evidenceIds: List[String],
      causeRecords: List[(EvidenceRef, RelativeCauseFact)]
  ): Option[PositionPlanTechniquePositiveFunctionalProof] =
    if !positionPlanTechniquePositiveFunctionalDetail(detail) then None
    else if detail.unit == PositionPlanTechniqueUnit.CompensationSource then
      positionPlanTechniqueMaterialTradeoffProof(detail, graph, evidenceIds, causeRecords)
    else
      for
        routeMove <- detail.structuralRouteMove
        planId <- detail.displayPlanId
        eventRecordAndPayload <- evidenceIds.flatMap(graph.byId.get).collect {
          case record @ EvidenceRecord(ref, payload: PlanCausalEventEvidence, _)
              if payload.planId == planId &&
                EvidenceRef.sameMove(payload.rootMove, routeMove) &&
                ref.confidence != EvidenceConfidence.Heuristic &&
                payload.publicGoalProofReady =>
            record -> payload
        }.distinctBy(_._1.ref.id) match
          case event :: Nil => Some(event)
          case _            => None
        (eventRecord, event) = eventRecordAndPayload
        detailKinds = detail.structuralConsequenceKinds.map(_.toString.toLowerCase).toSet
        eventBindings = EvidenceObjectBinding
          .fromEvidenceRefs(graph, List(eventRecord.ref))
          .filter(binding =>
            binding.target.exists(EvidenceObjectBinding.specificSurfaceTargetObject) &&
              binding.target.exists(obj =>
                obj.kind == EvidenceObjectKind.PlanSubject && obj.key.equalsIgnoreCase(planId.toString)
              ) &&
              (
                detailKinds.isEmpty ||
                  binding.mechanism.exists(obj => detailKinds(obj.key.toLowerCase)) ||
                  binding.horizon.nonEmpty && event.episodePublicProofReady
              )
          )
        if eventBindings.nonEmpty
      yield
        PositionPlanTechniquePositiveFunctionalProof(
          planId = planId,
          evidenceIds = List(eventRecord.ref.id),
          bindings = eventBindings.distinctBy(_.signature),
          event = event,
          resolvedEvent = ResolvedPlanEvent.from(event)
        )

  private def positionPlanTechniqueMaterialTradeoffProof(
      detail: PositionPlanTechniqueSemanticDetail,
      graph: TypedEvidenceGraph,
      evidenceIds: List[String],
      causeRecords: List[(EvidenceRef, RelativeCauseFact)]
  ): Option[PositionPlanTechniquePositiveFunctionalProof] =
    val evidenceRecords = evidenceIds.flatMap(graph.byId.get)
    val detailLines = evidenceRecords.flatMap(_.ref.line).toSet
    val rootMoves = (detail.structuralRouteMove.toList ++ detailLines.map(_.rootMove)).map(EvidenceRef.normalizeMove).toSet
    val admissibleCauses = causeRecords.filter { case (_, cause) =>
      cause.kind == RelativeCauseKind.SacrificeCompensation &&
        cause.sourceSide == RelativeCauseSourceSide.Candidate &&
        cause.attribution.rootMoveMatched &&
        cause.attribution.directProofEligible &&
        !Set(MoveChoiceVerdict.Inaccuracy, MoveChoiceVerdict.Mistake, MoveChoiceVerdict.Blunder)(cause.verdict)
    }
    def causeSacrificeSquares(line: LineNodeRef): Set[String] =
      admissibleCauses
        .filter(_._2.eventLine == line)
        .flatMap { case (_, cause) => EvidenceObjectBinding.fromRelativeCause(cause, graph) }
        .flatMap(_.target)
        .collect {
          case obj
              if obj.kind == EvidenceObjectKind.PlanSubject &&
                obj.key.toLowerCase.startsWith("material-sacrifice:") =>
            obj.key.toLowerCase.stripPrefix("material-sacrifice:")
        }
        .toSet
    val candidates = graph.records.collect {
      case record @ EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
          if ref.confidence != EvidenceConfidence.Heuristic &&
            ref.line.exists(detailLines) &&
            rootMoves(EvidenceRef.normalizeMove(event.rootMove)) &&
            event.materialTradeoffs.nonEmpty &&
            ref.line.exists(line =>
              val provenSquares = causeSacrificeSquares(line)
              event.observedMaterialCosts.nonEmpty &&
                event.observedMaterialCosts.forall(cost => provenSquares(cost.capture.square.key.toLowerCase))
            ) =>
        (record, event, event.materialTradeoffs)
    }.distinctBy(_._1.ref.id)
    val planEvidence = positionPlanTechniquePlanEvidenceForSources(
      graph,
      (evidenceIds ++ candidates.map(_._1.ref.id)).distinct,
      detail
    )
    val selectedEvent =
      planEvidence.event
        .orElse(
          planEvidence.activePlanIds.collectFirst(Function.unlift(planId =>
            candidates.find(_._2.planId == planId).map(_._2)
          ))
        )
        .orElse(candidates match
          case (_, event, _) :: Nil => Some(event)
          case _                    => None
        )
    for
      event <- selectedEvent
      (record, _, tradeoffs) <- candidates.find(_._2 == event)
      bindings = EvidenceObjectBinding.fromPlanMaterialTradeoffs(record.ref, event)
      if bindings.nonEmpty
    yield PositionPlanTechniquePositiveFunctionalProof(
      planId = event.planId,
      evidenceIds = List(record.ref.id),
      bindings = bindings,
      event = event,
      resolvedEvent = ResolvedPlanEvent.fromMaterialTradeoffs(event, tradeoffs)
    )

  private def positionPlanTechniquePositiveFunctionalDetail(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    !positionPlanTechniqueNegativeOutcome(detail) &&
      (
        detail.unit == PositionPlanTechniqueUnit.CompensationSource ||
          detail.unit == PositionPlanTechniqueUnit.PlanOptionSet && detail.planMoveRole.nonEmpty
      )

  private def positionPlanTechniqueNegativeOutcome(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.axisPolarity.exists(polarity =>
      polarity == StrategicAxisPolarity.Loss ||
        polarity == StrategicAxisPolarity.Release ||
        polarity == StrategicAxisPolarity.Concede
    ) ||
      detail.structuralPurposePolarities.exists(_.equalsIgnoreCase(StructuralSignalPolarity.Loss.toString))

  private def positionPlanTechniqueTerminalOverriddenEndgameTechnique(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe &&
      detail.endgameTechniqueHorizonStatus.exists(status =>
        status == "SupersededByTactic" || status == "ContradictedByTerminalProof"
      )

  private def positionPlanTechniqueCauseRecordsForDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      causeRecords: List[(EvidenceRef, RelativeCauseFact)],
      evidenceIds: Set[String]
  ): List[(EvidenceRef, RelativeCauseFact)] =
    val axisMatched = detail.axisKey match
      case Some(axisKey) =>
        causeRecords.filter { case (causeRef, cause) =>
          cause.strategicProofIdentity.axisKeys.contains(axisKey) &&
            positionPlanTechniqueCauseOwnsStructuralRoot(detail, cause) &&
            positionPlanTechniqueCauseKindMatchesDetail(detail, causeRef, cause) &&
            positionPlanTechniqueHasAdmissibleProofForEvidence(cause, evidenceIds)
        }
      case None =>
        causeRecords.filter { case (causeRef, cause) =>
          positionPlanTechniqueCauseOwnsStructuralRoot(detail, cause) &&
            positionPlanTechniqueCauseKindMatchesUnitOnlyDetail(detail, causeRef, cause) &&
            positionPlanTechniqueHasAdmissibleProofForEvidence(cause, evidenceIds)
        }
    val concreteRouteProofMatched =
      if detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute then
        causeRecords.filter { case (_, cause) =>
          cause.attribution.directProofEligible &&
            cause.attribution.rootMoveMatched &&
            detail.structuralRouteMove.exists(move => positionPlanTechniqueSameMove(move, cause.eventRootMove)) &&
            positionPlanTechniqueConcreteRouteCauseKind(detail, cause.kind) &&
            positionPlanTechniqueHasAdmissibleProofForEvidence(cause, evidenceIds)
        }
      else Nil
    (
      axisMatched ++
        concreteRouteProofMatched ++
        positionPlanTechniqueRootMoveStructuralCauseRecords(detail, causeRecords, evidenceIds)
    ).distinctBy(_._1.id)

  private[judgment] def positionPlanTechniqueCauseOwnsStructuralRoot(
      detail: PositionPlanTechniqueSemanticDetail,
      cause: RelativeCauseFact
  ): Boolean =
    detail.unit match
      case PositionPlanTechniqueUnit.StructuralTransformation =>
        detail.structuralRouteMove.exists(move => positionPlanTechniqueSameMove(move, cause.eventRootMove))
      case _ =>
        !positionPlanTechniqueRootMoveStructuralDetail(detail) ||
          detail.structuralRouteMove.exists(move => positionPlanTechniqueSameMove(move, cause.eventRootMove))

  private def positionPlanTechniqueContextCauseRecordsForDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      causeRecords: List[(EvidenceRef, RelativeCauseFact)],
      evidenceIds: Set[String]
  ): List[(EvidenceRef, RelativeCauseFact)] =
    detail.axisKey match
      case Some(axisKey) =>
        causeRecords.filter { case (_, cause) =>
          cause.strategicProofIdentity.axisKeys.contains(axisKey) &&
            positionPlanTechniqueContextCauseKindMatchesDetail(detail, cause.kind) &&
            positionPlanTechniqueProofRolesForEvidence(cause, evidenceIds).contains(RelativeCauseProofRole.ContextSupport)
        }
      case None =>
        Nil

  private def positionPlanTechniqueContextCauseKindMatchesDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      kind: RelativeCauseKind
  ): Boolean =
    detail.unit match
      case PositionPlanTechniqueUnit.SpacePreventionResourceDenial =>
        Set(
          RelativeCauseKind.MissedTacticalResource,
          RelativeCauseKind.TacticalRefutationOfPlayed,
          RelativeCauseKind.CandidateTacticalLiability,
          RelativeCauseKind.RecaptureRecoveryWindow,
          RelativeCauseKind.OnlyDefenseNecessity,
          RelativeCauseKind.DefensiveResource,
          RelativeCauseKind.KingForcing
        ).contains(kind)
      case _ =>
        false

  private def positionPlanTechniqueCauseKindMatchesDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      causeRef: EvidenceRef,
      cause: RelativeCauseFact
  ): Boolean =
    detail.axisKind.exists(axisKind =>
      !(detail.unit == PositionPlanTechniqueUnit.CounterplayRace && axisKind == StrategicAxisKind.PawnBreak) &&
        positionPlanTechniqueCauseKindsForAxis(axisKind, detail.axisPolarity, detail.structuralConsequenceKinds).contains(cause.kind)
    ) ||
      positionPlanTechniqueCounterplayRestraintPlanCauseKind(detail, cause.kind) ||
      positionPlanTechniqueConcreteCounterplayRaceCauseKind(detail, cause.kind) ||
      positionPlanTechniqueConcreteRouteCauseKind(detail, cause.kind) ||
      positionPlanTechniqueConcreteStructuralPlanCauseKind(detail, cause.kind) ||
      positionPlanTechniqueTacticalProofCauseKind(detail, causeRef, cause) ||
      positionPlanTechniqueTerminalProofCauseKind(detail, cause)

  private def positionPlanTechniqueCauseKindMatchesUnitOnlyDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      causeRef: EvidenceRef,
      cause: RelativeCauseFact
  ): Boolean =
    detail.axisKey.isEmpty &&
      positionPlanTechniqueCauseKindMatchesStructuralSourceDetail(detail, causeRef, cause)

  private def positionPlanTechniqueRootMoveStructuralCauseRecords(
      detail: PositionPlanTechniqueSemanticDetail,
      causeRecords: List[(EvidenceRef, RelativeCauseFact)],
      evidenceIds: Set[String]
  ): List[(EvidenceRef, RelativeCauseFact)] =
    if !positionPlanTechniqueRootMoveStructuralDetail(detail) then Nil
    else
      causeRecords.filter { case (causeRef, cause) =>
        cause.attribution.directProofEligible &&
          cause.attribution.rootMoveMatched &&
          detail.structuralRouteMove.exists(move => positionPlanTechniqueSameMove(move, cause.eventRootMove)) &&
          positionPlanTechniqueCauseKindMatchesStructuralSourceDetail(detail, causeRef, cause) &&
          positionPlanTechniqueDirectStructuralTransitionProof(detail, cause, evidenceIds)
      }

  private def positionPlanTechniqueRootMoveStructuralDetail(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.unit match
      case PositionPlanTechniqueUnit.PieceRerouteRoute =>
        positionPlanTechniqueConcretePieceRoute(detail)
      case PositionPlanTechniqueUnit.StructuralTransformation =>
        positionPlanTechniqueConcreteStructuralTransformation(detail)
      case PositionPlanTechniqueUnit.CounterplayRace =>
        positionPlanTechniqueCounterplayPawnBreakRace(detail)
      case PositionPlanTechniqueUnit.SpacePreventionResourceDenial =>
        positionPlanTechniqueCounterplayRestraintPlanCauseKind(detail, RelativeCauseKind.PlanImprovement)
      case _ =>
        false

  private def positionPlanTechniqueCauseKindMatchesStructuralSourceDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      causeRef: EvidenceRef,
      cause: RelativeCauseFact
  ): Boolean =
    positionPlanTechniqueCauseKindsForUnit(detail.unit).contains(cause.kind) ||
      positionPlanTechniqueCounterplayRestraintPlanCauseKind(detail, cause.kind) ||
      positionPlanTechniqueConcreteCounterplayRaceCauseKind(detail, cause.kind) ||
      positionPlanTechniqueConcreteRouteCauseKind(detail, cause.kind) ||
      positionPlanTechniqueConcreteStructuralPlanCauseKind(detail, cause.kind) ||
      positionPlanTechniqueTacticalProofCauseKind(detail, causeRef, cause) ||
      positionPlanTechniqueTerminalProofCauseKind(detail, cause)

  private def positionPlanTechniqueDirectStructuralTransitionProof(
      detail: PositionPlanTechniqueSemanticDetail,
      cause: RelativeCauseFact,
      evidenceIds: Set[String]
  ): Boolean =
    cause.proof.toList.flatMap(_.directProof.transitionConsequences).exists { proof =>
      evidenceIds.contains(proof.source.id) &&
        proof.transition.line.contains(cause.eventLine) &&
        positionPlanTechniqueSameMove(proof.transition.moveUci, cause.eventRootMove) &&
        detail.structuralRouteMove.exists(move => positionPlanTechniqueSameMove(move, proof.transition.moveUci)) &&
        positionPlanTechniqueStructuralConsequenceOverlapsDetail(detail, proof.consequence)
    }

  private def positionPlanTechniqueStructuralConsequenceOverlapsDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      consequence: TransitionConsequence
  ): Boolean =
    val detailSubjects = detail.structuralPurposeSubjects.map(_.trim.toLowerCase).toSet
    consequence.subjects.exists(subject => detailSubjects.contains(subject.trim.toLowerCase))

  private def positionPlanTechniqueSameMove(left: String, right: String): Boolean =
    JudgmentSubjectBinding.normalizeMove(left) == JudgmentSubjectBinding.normalizeMove(right)

  private def positionPlanTechniqueConcreteRouteCauseKind(
      detail: PositionPlanTechniqueSemanticDetail,
      kind: RelativeCauseKind
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute &&
      detail.axisKind.contains(StrategicAxisKind.Activity) &&
      (
        positionPlanTechniqueConcretePieceRoute(detail) ||
          (kind == RelativeCauseKind.DefensiveResource && positionPlanTechniqueDefensiveKingRoute(detail))
      ) &&
      Set(
        RelativeCauseKind.PlanImprovement,
        RelativeCauseKind.PlanContradiction,
        RelativeCauseKind.RecaptureRecoveryWindow,
        RelativeCauseKind.DefensiveResource,
        RelativeCauseKind.KingForcing
      ).contains(kind)

  private def positionPlanTechniqueDefensiveKingRoute(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.structuralRouteMove.nonEmpty &&
      detail.structuralPurposeSubjects.exists(subject =>
        StructuralPurposeSubject.parse(subject) match
          case Some(StructuralPurposeSubject.PieceRoute(piece, _, _)) =>
            piece.equalsIgnoreCase("king")
          case _ =>
            false
      )

  private def positionPlanTechniqueConcreteCounterplayRaceCauseKind(
      detail: PositionPlanTechniqueSemanticDetail,
      kind: RelativeCauseKind
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.CounterplayRace &&
      positionPlanTechniqueCounterplayRaceProof(detail) &&
      (
        detail.axisKind.contains(StrategicAxisKind.Counterplay) &&
          (
            detail.resourceContestSquares.nonEmpty ||
              detail.resourceContestFiles.nonEmpty ||
              detail.structuralPurposeSubjects.exists(positionPlanTechniqueConcreteSubject)
          ) &&
          kind == RelativeCauseKind.StructuralImprovement ||
          detail.axisKind.contains(StrategicAxisKind.PawnBreak) &&
            positionPlanTechniqueCounterplayPawnBreakRace(detail) &&
            kind == RelativeCauseKind.PawnBreakOpportunity
      )

  private def positionPlanTechniqueCounterplayRestraintPlanCauseKind(
      detail: PositionPlanTechniqueSemanticDetail,
      kind: RelativeCauseKind
  ): Boolean =
    kind == RelativeCauseKind.PlanImprovement &&
      detail.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial &&
      detail.axisKind.contains(StrategicAxisKind.Counterplay) &&
      detail.axisPolarity.contains(StrategicAxisPolarity.Restrain) &&
      detail.structuralRouteMove.nonEmpty &&
      (
        detail.resourceContestSquares.nonEmpty ||
          detail.resourceContestFiles.nonEmpty ||
          detail.counterBreakFiles.nonEmpty ||
          detail.breakFile.exists(_.trim.nonEmpty) ||
          detail.structuralPurposeSubjects.exists(positionPlanTechniqueConcreteSubject)
      )

  private def positionPlanTechniqueConcretePieceRoute(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.structuralRouteMove.nonEmpty &&
      detail.structuralPurposeSubjects.exists(positionPlanTechniqueRouteSubject)

  private def positionPlanTechniqueRouteSubject(subject: String): Boolean =
    StructuralPurposeSubject.parse(subject) match
      case Some(StructuralPurposeSubject.PieceRoute(_, _, _)) =>
        positionPlanTechniqueQualifiedRouteSubject(subject)
      case Some(StructuralPurposeSubject.Outpost(_, _))       => true
      case Some(StructuralPurposeSubject.Battery(_, _, _, _)) => true
      case Some(StructuralPurposeSubject.PieceRestriction(_, _, _)) =>
        positionPlanTechniqueStrategicRayRestrictionSubject(subject)
      case Some(StructuralPurposeSubject.PieceSquare(_, _)) =>
        positionPlanTechniqueLineUnlockRouteSubject(subject)
      case _                                                  => false

  private def positionPlanTechniquePieceRouteSubject(subject: String): Boolean =
    StructuralPurposeSubject.parse(subject) match
      case Some(StructuralPurposeSubject.PieceRoute(_, _, _))   => true
      case Some(StructuralPurposeSubject.Outpost(_, _))          => true
      case Some(StructuralPurposeSubject.Battery(_, _, _, _))    => true
      case Some(StructuralPurposeSubject.PieceRestriction(_, _, _)) =>
        positionPlanTechniqueStrategicRayRestrictionSubject(subject)
      case Some(StructuralPurposeSubject.PieceSquare(_, _)) =>
        positionPlanTechniqueLineUnlockRouteSubject(subject)
      case _                                                    => false

  private def positionPlanTechniqueLineUnlockRouteSubject(subject: String): Boolean =
    val normalized = subject.toLowerCase
    normalized.contains(":line-unlock:by:") && normalized.contains(":mobility+")

  private def positionPlanTechniqueQualifiedRouteSubject(subject: String): Boolean =
    val normalized = subject.toLowerCase
    normalized.contains("outpost") ||
      normalized.contains("battery") ||
      normalized.contains("diagonal") ||
      normalized.contains("maneuver") ||
      normalized.contains("filecontrol") ||
      normalized.contains("file-control") ||
      normalized.contains("fileaccess") ||
      normalized.contains("file-access") ||
      normalized.contains("fileoccupation") ||
      normalized.contains("file-occupation") ||
      normalized.contains("weak-square") ||
      normalized.contains("weaksquare") ||
      normalized.contains("rook-lift")

  private def positionPlanTechniqueConcreteStructuralPlanCauseKind(
      detail: PositionPlanTechniqueSemanticDetail,
      kind: RelativeCauseKind
  ): Boolean =
    val causeKinds =
      Set(RelativeCauseKind.PlanImprovement, RelativeCauseKind.PlanContradiction) ++
        Option
          .when(positionPlanTechniqueStructureRouteContext(detail) && positionPlanTechniqueDevelopmentRouteDetail(detail))(
            Set(RelativeCauseKind.ActivityGain, RelativeCauseKind.ActivityLoss)
          )
          .getOrElse(Set.empty)
    detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
      positionPlanTechniqueConcreteStructuralTransformation(detail) &&
      causeKinds.contains(kind)

  private def positionPlanTechniqueConcreteStructuralTransformation(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    val concreteMotifs = Set("iqp", "isolated", "open", "space", "transition")
    detail.structuralRouteMove.nonEmpty &&
      detail.structuralMotifTags.exists(concreteMotifs)

  private[chessjudgment] def positionPlanTechniqueTacticalProofCauseKind(
      detail: PositionPlanTechniqueSemanticDetail,
      causeRef: EvidenceRef,
      cause: RelativeCauseFact
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
      cause.hasOwnedTacticalProof &&
      detail.sourceEvidenceIds.contains(causeRef.id) &&
      detail.structuralRouteMove.nonEmpty &&
      detail.structuralPurposeSubjects.nonEmpty &&
      Set(
        RelativeCauseKind.MissedTacticalResource,
        RelativeCauseKind.TacticalRefutationOfPlayed,
        RelativeCauseKind.CandidateTacticalLiability,
        RelativeCauseKind.KingForcing,
        RelativeCauseKind.MaterialSwing,
        RelativeCauseKind.RecaptureRecoveryWindow,
        RelativeCauseKind.WrongRecapturer,
        RelativeCauseKind.SacrificeCompensation
      ).contains(cause.kind)

  private def positionPlanTechniqueTerminalProofCauseKind(
      detail: PositionPlanTechniqueSemanticDetail,
      cause: RelativeCauseFact
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
      detail.terminalConsequenceKinds.exists(positionPlanTechniqueTerminalProofConsequenceKind) &&
      detail.structuralRouteMove.nonEmpty &&
      detail.terminalConsequenceKinds.exists(positionPlanTechniqueTerminalProofCauseKind(_, detail, cause))

  private def positionPlanTechniqueTerminalProofConsequenceKind(kind: String): Boolean =
    LineConsequenceKind.terminalResultProofName(kind)

  private def positionPlanTechniqueTerminalProofCauseKind(
      consequenceKind: String,
      detail: PositionPlanTechniqueSemanticDetail,
      cause: RelativeCauseFact
  ): Boolean =
    consequenceKind match
      case "Mate" =>
        Set(
          RelativeCauseKind.KingForcing,
          RelativeCauseKind.TacticalRefutationOfPlayed,
          RelativeCauseKind.MissedTacticalResource,
          RelativeCauseKind.CandidateTacticalLiability
        ).contains(cause.kind)
      case "MaterialGain" =>
        Set(RelativeCauseKind.MaterialSwing, RelativeCauseKind.SacrificeCompensation).contains(cause.kind)
      case "MaterialLoss" =>
        Set(RelativeCauseKind.MaterialSwing, RelativeCauseKind.SacrificeCompensation).contains(cause.kind) ||
          positionPlanTechniqueAllowsCandidateMaterialLoss(detail, cause)
      case "Promotion" | "PromotionRace" =>
        Set(
          RelativeCauseKind.ConversionMiss,
          RelativeCauseKind.ConversionSecured
        ).contains(cause.kind) ||
          positionPlanTechniqueAllowsOpponentPromotion(detail, cause)
      case "DrawResource" =>
        cause.kind == RelativeCauseKind.DrawResource
      case _ =>
        false

  private def positionPlanTechniqueAllowsOpponentPromotion(
      detail: PositionPlanTechniqueSemanticDetail,
      cause: RelativeCauseFact
  ): Boolean =
    Set(
      RelativeCauseKind.TacticalRefutationOfPlayed,
      RelativeCauseKind.CandidateTacticalLiability
    ).contains(cause.kind) &&
      detail.axisPolarity.contains(StrategicAxisPolarity.Loss) &&
      cause.proof.exists(proof =>
        proof.directProof.lineConsequences.exists(consequence =>
          detail.sourceEvidenceIds.contains(consequence.source.id) &&
            detail.terminalConsequenceKinds.contains(consequence.kind.toString) &&
            tacticalCauseAllowsOpponentPromotion(cause, consequence)
        )
      )

  private def positionPlanTechniqueAllowsCandidateMaterialLoss(
      detail: PositionPlanTechniqueSemanticDetail,
      cause: RelativeCauseFact
  ): Boolean =
    cause.attribution.kind == CauseAttributionKind.CandidateAllowsLiability &&
      cause.sourceSide == RelativeCauseSourceSide.Candidate &&
      Set(
        RelativeCauseKind.TacticalRefutationOfPlayed,
        RelativeCauseKind.CandidateTacticalLiability
      ).contains(cause.kind) &&
      detail.axisPolarity.contains(StrategicAxisPolarity.Loss) &&
      cause.proof.exists(proof =>
        proof.directProof.lineConsequences.exists(consequence =>
          consequence.kind == LineConsequenceKind.MaterialLoss &&
            consequence.rootMoveMatched(cause.eventRootMove) &&
            detail.sourceEvidenceIds.contains(consequence.source.id) &&
            detail.terminalConsequenceKinds.contains(consequence.kind.toString)
        )
      )

  private def positionPlanTechniqueCauseKindsForUnit(
      unit: PositionPlanTechniqueUnit
  ): Set[RelativeCauseKind] =
    unit match
      case PositionPlanTechniqueUnit.TensionBreakPolicyRoute =>
        Set(RelativeCauseKind.PawnBreakOpportunity)
      case PositionPlanTechniqueUnit.PlanOptionSet =>
        Set(RelativeCauseKind.PlanImprovement, RelativeCauseKind.PlanContradiction)
      case PositionPlanTechniqueUnit.EndgameTechniqueRecipe =>
        Set(RelativeCauseKind.ConversionMiss, RelativeCauseKind.ConversionSecured, RelativeCauseKind.DrawResource)
      case PositionPlanTechniqueUnit.SpacePreventionResourceDenial =>
        Set(
          RelativeCauseKind.OnlyDefenseNecessity,
          RelativeCauseKind.DefensiveResource,
          RelativeCauseKind.DrawResource,
          RelativeCauseKind.OpponentRestriction
        )
      case PositionPlanTechniqueUnit.CounterplayRace =>
        Set(
          RelativeCauseKind.OnlyDefenseNecessity,
          RelativeCauseKind.DefensiveResource,
          RelativeCauseKind.OpponentRestriction,
          RelativeCauseKind.KingSafetyConcession
        )
      case PositionPlanTechniqueUnit.PieceRerouteRoute =>
        Set(RelativeCauseKind.ActivityGain, RelativeCauseKind.ActivityLoss)
      case PositionPlanTechniqueUnit.StructuralTransformation =>
        Set(
          RelativeCauseKind.StructuralImprovement,
          RelativeCauseKind.MissedStrategicImprovement,
          RelativeCauseKind.StrategicConcession,
          RelativeCauseKind.TargetPressureGain,
          RelativeCauseKind.TargetPressureRelease,
          RelativeCauseKind.PawnWeaknessTarget,
          RelativeCauseKind.CenterControlGain
        )
      case PositionPlanTechniqueUnit.CompensationSource =>
        Set(RelativeCauseKind.SacrificeCompensation)

  private def positionPlanTechniqueCauseKindsForAxis(
      axisKind: StrategicAxisKind,
      polarity: Option[StrategicAxisPolarity],
      consequenceKinds: List[TransitionConsequenceKind]
  ): Set[RelativeCauseKind] =
    axisKind match
      case StrategicAxisKind.Target =>
        Set(
          RelativeCauseKind.TargetPressureGain,
          RelativeCauseKind.TargetPressureRelease
        ) ++ Option.when(consequenceKinds.exists(kind =>
          kind == TransitionConsequenceKind.WeakPawnTargetCreated ||
            kind == TransitionConsequenceKind.WeakSquareTargetCreated
        ))(
          RelativeCauseKind.PawnWeaknessTarget
        )
      case StrategicAxisKind.SpaceCenter =>
        Set(RelativeCauseKind.CenterControlGain)
      case StrategicAxisKind.PawnBreak =>
        Set(RelativeCauseKind.PawnBreakOpportunity)
      case StrategicAxisKind.Counterplay =>
        Set(
          Option.when(polarity.contains(StrategicAxisPolarity.Restrain))(RelativeCauseKind.OpponentRestriction),
          Option.when(
            polarity.contains(StrategicAxisPolarity.Concede) &&
              consequenceKinds.exists(kind =>
                kind == TransitionConsequenceKind.KingSafetyConcession ||
                  kind == TransitionConsequenceKind.KingRingPressureConcession
              )
          )(RelativeCauseKind.KingSafetyConcession)
        ).flatten
      case StrategicAxisKind.Activity =>
        Set(
          RelativeCauseKind.ActivityGain,
          RelativeCauseKind.ActivityLoss
        )
      case StrategicAxisKind.PlanCoherence =>
        Set(
          RelativeCauseKind.PlanImprovement,
          RelativeCauseKind.PlanContradiction
        )

  private def positionPlanTechniqueHasAdmissibleProofForEvidence(
      cause: RelativeCauseFact,
      evidenceIds: Set[String]
  ): Boolean =
    positionPlanTechniqueProofRolesForEvidence(cause, evidenceIds).exists(positionPlanTechniqueAdmissibleDetailProofRole)

  private def positionPlanTechniqueAdmissibleDetailProofRole(role: RelativeCauseProofRole): Boolean =
    role == RelativeCauseProofRole.DirectProof || role == RelativeCauseProofRole.ContrastProof

  private def positionPlanTechniqueRelativeCauseRecordsFor(
      graph: TypedEvidenceGraph,
      evidenceIds: Set[String]
  ): List[(EvidenceRef, RelativeCauseFact)] =
    graph.records.collect {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), parents)
          if relativeCauseLinkedEvidenceIds(cause, parents).exists(evidenceIds.contains) =>
        ref -> cause
    }.distinctBy(_._1.id).sortBy(_._1.id)

  private def positionPlanTechniqueProofRolesForEvidence(
      cause: RelativeCauseFact,
      evidenceIds: Set[String]
  ): List[RelativeCauseProofRole] =
    (
      cause.proof.toList
        .flatMap(_.sections)
        .filter(section => positionPlanTechniqueProofSectionEvidenceIds(section).exists(evidenceIds.contains))
        .map(_.role) ++
        positionPlanTechniqueAttributionProofRolesForEvidence(cause, evidenceIds)
    ).distinct.sortBy(_.toString)

  private def positionPlanTechniqueAttributionProofRolesForEvidence(
      cause: RelativeCauseFact,
      evidenceIds: Set[String]
  ): List[RelativeCauseProofRole] =
    List(
      Option.when(cause.attribution.directProofEligible && cause.attribution.ownedEvidence.exists(ref => evidenceIds.contains(ref.id)))(
        RelativeCauseProofRole.DirectProof
      ),
      Option.when(cause.attribution.contrastEvidence.exists(ref => evidenceIds.contains(ref.id)))(
        RelativeCauseProofRole.ContrastProof
      ),
      Option.when(cause.attribution.contextEvidence.exists(ref => evidenceIds.contains(ref.id)))(
        RelativeCauseProofRole.ContextSupport
      )
    ).flatten

  private def positionPlanTechniqueProofSectionEvidenceIds(section: RelativeCauseProofSection): List[String] =
    (
      section.sourceRefs.map(_.id) ++
        section.tacticalMechanisms.flatMap(_.signals.flatMap(_.source.map(_.id))) ++
        section.strategicMechanisms.flatMap(_.signals.map(_.source.id)) ++
        section.strategicMechanismContrasts.flatMap(_.axisComparisons.flatMap(_.sources.map(_.id)))
    ).distinct.sorted

  private def positionPlanTechniqueSpecificityTier(
      detail: PositionPlanTechniqueSemanticDetail,
      causes: List[RelativeCauseFact],
      objectBindingSignatures: List[String]
  ): PositionPlanTechniqueSpecificityTier =
    val exactAxis =
      detail.axisKey.exists(axis => causes.exists(_.strategicProofIdentity.axisKeys.contains(axis)))
    val nonBroadObject =
      objectBindingSignatures.exists(positionPlanTechniqueNonBroadObjectSignature)
    if exactAxis && nonBroadObject then PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    else if nonBroadObject then PositionPlanTechniqueSpecificityTier.ConcreteObjectAxis
    else if detail.axisKey.nonEmpty || objectBindingSignatures.nonEmpty || detail.semanticAnchorKeys.nonEmpty then
      PositionPlanTechniqueSpecificityTier.BroadAxis
    else PositionPlanTechniqueSpecificityTier.ContextOnly

  private def positionPlanTechniqueNonBroadObjectSignature(signature: String): Boolean =
    EvidenceObjectBinding.specificTargetMechanismReadySignatures(List(signature))

  private def mechanismPlanTechniqueDetails(
      payload: StrategicMechanismEvidence,
      units: List[PositionPlanTechniqueUnit],
      anchors: List[EvidenceSemanticAnchor],
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): List[PositionPlanTechniqueSemanticDetail] =
    val anchorKeys = anchors.map(_.stableKey).distinct.sorted
    val pawnPlayBySourceId = positionPlanTechniquePawnPlayBySourceId(graph, refs)
    val planAlignmentBySourceId = positionPlanTechniquePlanAlignmentBySourceId(graph, refs)
    val openingPriorBySourceId = positionPlanTechniqueOpeningPriorBySourceId(graph, refs)
    val resourceContestBySourceId = positionPlanTechniqueResourceContestBySourceId(graph, refs)
    val structuralPurposeBySourceId = positionPlanTechniqueStructuralPurposeBySourceId(graph, refs)
    val routePurposeBySourceId = positionPlanTechniqueRoutePurposeBySourceId(graph, refs)
    val threatBySourceId = positionPlanTechniqueThreatBySourceId(graph, refs)
    val axisDetails =
      payload.axisDetails.flatMap(axis =>
        positionPlanTechniqueUnits(axis).map(unit =>
          val sourceIds = payload.signals.filter(_.axis.exists(_.stableKey == axis.stableKey)).map(_.source.id).distinct.sorted
          val detailSourceIds = positionPlanTechniqueExpandedSourceIds(graph, sourceIds)
          val detail = PositionPlanTechniqueSemanticDetail(
            unit = unit,
            axisKey = Some(axis.stableKey),
            axisKind = Some(axis.kind),
            axisPolarity = Some(axis.polarity),
            label = Some(axis.label),
            mechanismKinds = List(payload.kind),
            semanticAnchorKeys = anchorKeys,
            sourceEvidenceIds = detailSourceIds
          )
          detail
            .withPawnPlay(positionPlanTechniquePawnPlayForSources(sourceIds, pawnPlayBySourceId))
            .withPlanAlignment(positionPlanTechniquePlanAlignmentForSources(sourceIds, planAlignmentBySourceId))
            .withOpeningPrior(positionPlanTechniqueOpeningPriorForSources(sourceIds, openingPriorBySourceId))
            .withResourceContest(positionPlanTechniqueResourceContestForSources(sourceIds, resourceContestBySourceId))
            .withStructuralPurpose(positionPlanTechniqueStructuralPurposeForDetailSources(detail, sourceIds, structuralPurposeBySourceId))
            .withStructuralPurpose(positionPlanTechniqueStructuralPurposeForSources(sourceIds, routePurposeBySourceId))
            .withThreatProjection(positionPlanTechniqueThreatForSources(sourceIds, threatBySourceId))
        )
      )
    val unitOnlyDetails =
      units
        .filterNot(unit => axisDetails.exists(_.unit == unit))
        .map(unit =>
          val sourceIds = payload.signals.map(_.source.id).distinct.sorted
          val detailSourceIds = positionPlanTechniqueExpandedSourceIds(graph, sourceIds)
          val detail = PositionPlanTechniqueSemanticDetail(
            unit = unit,
            mechanismKinds = List(payload.kind),
            semanticAnchorKeys = anchorKeys,
            sourceEvidenceIds = detailSourceIds
          )
          detail
            .withPawnPlay(positionPlanTechniquePawnPlayForSources(sourceIds, pawnPlayBySourceId))
            .withPlanAlignment(positionPlanTechniquePlanAlignmentForSources(sourceIds, planAlignmentBySourceId))
            .withOpeningPrior(positionPlanTechniqueOpeningPriorForSources(sourceIds, openingPriorBySourceId))
            .withResourceContest(positionPlanTechniqueResourceContestForSources(sourceIds, resourceContestBySourceId))
            .withStructuralPurpose(positionPlanTechniqueStructuralPurposeForDetailSources(detail, sourceIds, structuralPurposeBySourceId))
            .withStructuralPurpose(positionPlanTechniqueStructuralPurposeForSources(sourceIds, routePurposeBySourceId))
            .withThreatProjection(positionPlanTechniqueThreatForSources(sourceIds, threatBySourceId))
        )
    positionPlanTechniqueWithPawnBreakRaceDetails(axisDetails ++ unitOnlyDetails)
      .distinctBy(detail => (detail.unit, detail.axisKey, detail.sourceEvidenceIds.mkString(",")))
      .sortBy(detail => (detail.unit.toString, detail.axisKey.getOrElse("")))

  extension (detail: PositionPlanTechniqueSemanticDetail)
    private def withPawnPlay(pawnPlay: Option[PawnPlayAnalysis]): PositionPlanTechniqueSemanticDetail =
      pawnPlay.fold(detail)(play =>
        detail.copy(
          pawnBreakReady = Some(play.pawnBreakReady),
          breakFile = play.breakFile,
          breakImpact = Some(play.breakImpact),
          advanceOrCapture = Some(play.advanceOrCapture),
          passedPawnUrgency = Some(play.passedPawnUrgency.toString),
          passerBlockade = Some(play.passerBlockade),
          blockadeSquare = play.blockadeSquare.map(_.key),
          blockadeRole = play.blockadeRole.map(_.toString),
          pusherSupport = Some(play.pusherSupport),
          minorityAttack = Some(play.minorityAttack),
          counterBreak = Some(play.counterBreak),
          tensionPolicy = Some(play.tensionPolicy.toString),
          tensionSquares = play.tensionSquares.distinct.sorted,
          tensionEdges = play.tensionEdges.distinct.sorted,
          counterBreakFiles = play.counterBreakFiles.distinct.sorted,
          pawnPlayDriver = Some(play.primaryDriver.toString)
        )
      )

    private def withPlanAlignment(alignment: Option[PlanAlignment]): PositionPlanTechniqueSemanticDetail =
      alignment.fold(detail)(plan =>
        detail.copy(
          planAlignmentScore = Some(plan.score),
          planAlignmentBand = Some(plan.band.toString),
          matchedPlanIds = plan.matchedPlanIds.distinct.sorted,
          missingPlanIds = plan.missingPlanIds.distinct.sorted,
          planAlignmentReasonCodes = plan.reasonCodes.distinct.sorted,
          planAlignmentReasonWeights = plan.reasonWeights
        )
      )

    private def withPlanEvidence(plan: PositionPlanTechniquePlanEvidence): PositionPlanTechniqueSemanticDetail =
      val planCanOwnDetail = detail.unit != PositionPlanTechniqueUnit.CompensationSource
      val resolvedDisplayPlanId =
        detail.displayPlanId.orElse(Option.when(planCanOwnDetail)(plan.displayPlanId).flatten)
      val matchingEvent = Option.when(planCanOwnDetail)(plan.event).flatten
        .filter(event => resolvedDisplayPlanId.forall(_ == event.planId))
      val unownedRestriction =
        detail.structuralConsequenceKinds.distinct == List(TransitionConsequenceKind.OpponentMobilityRestriction) &&
          plan.principalCausalEventAvailable &&
          matchingEvent.isEmpty
      val eventOwnedDetail =
        if detail.unit == PositionPlanTechniqueUnit.PlanOptionSet then
          matchingEvent.fold(detail) { event =>
            val consequences = event.ownedGoalConsequences
            detail.copy(
              structuralRouteMove = Some(event.rootMove),
              structuralPurposeConsequences = consequences.map(_.kind.toString).distinct.sorted,
              structuralConsequenceKinds = consequences.map(_.kind).distinct,
              structuralPurposeSubjects = consequences.flatMap(_.subjects).distinct.sorted,
              structuralPurposeCategories = consequences
                .flatMap(consequence => positionPlanTechniqueStructuralPurposeCategories(consequence.kind))
                .distinct
                .sorted,
              structuralPurposePolarities = consequences.map(_.polarity.toString).distinct.sorted,
              structuralPurposeStrength = Option.when(consequences.nonEmpty)(consequences.map(_.strength).sum)
            )
          }
        else detail
      eventOwnedDetail.copy(
        activePlanIds =
          (Option.when(planCanOwnDetail)(plan.activePlanIds).getOrElse(Nil) ++ eventOwnedDetail.activePlanIds).distinct,
        displayPlanId = resolvedDisplayPlanId,
        mainExplanationCandidate =
          (!planCanOwnDetail || plan.mainExplanationCandidate) && eventOwnedDetail.mainExplanationCandidate && !unownedRestriction,
        previousPlanId = eventOwnedDetail.previousPlanId.orElse(Option.when(planCanOwnDetail)(plan.previousPlanId).flatten),
        planTransitionType =
          eventOwnedDetail.planTransitionType.orElse(Option.when(planCanOwnDetail)(plan.transitionType).flatten),
        planMoveRole =
          if eventOwnedDetail.unit == PositionPlanTechniqueUnit.PlanOptionSet then
            matchingEvent.flatMap(event => event.moveRole(event.planSequenceSummary.map(_.transitionType)))
          else None,
        resolvedPlanEvent = eventOwnedDetail.resolvedPlanEvent.orElse(matchingEvent.map(ResolvedPlanEvent.from)),
        sourceEvidenceIds =
          (
            eventOwnedDetail.sourceEvidenceIds ++
              Option
                .when(matchingEvent.nonEmpty && eventOwnedDetail.unit != PositionPlanTechniqueUnit.CompensationSource)(
                  plan.eventEvidenceId
                )
                .flatten
          ).distinct.sorted
      )

    private def withOpeningPrior(selection: Option[OpeningThemePriorSelection]): PositionPlanTechniqueSemanticDetail =
      selection.fold(detail)(selected =>
        val prior = selected.prior
        detail.copy(
          openingPriorLineage = prior.lineage,
          openingPriorFamily = prior.family.map(_.toString),
          openingPriorThemes = prior.themes.map(_.toString).distinct,
          openingPriorTypicalPawnStructures = prior.typicalPawnStructures.distinct,
          openingPriorCenterBreaks = prior.centerBreaks.distinct,
          openingPriorDevelopmentPriorities = prior.developmentPriorities.distinct,
          openingPriorGambitCompensation = Some(prior.gambitCompensation),
          openingPriorStrategicPlanPriors = prior.strategicPlanPriors.distinct,
          openingPriorMatchSource = Some(selected.matchSource.toString),
          openingPriorRequestedLineage = selected.requestedLineage,
          openingPriorCanonicalLineage = selected.canonicalLineage,
          openingPriorOpeningSpecific = Some(selected.openingSpecific),
          openingPriorCanCertify = Some(selected.canCertifyOpeningClaim)
        )
      )

    private def withResourceContest(contest: Option[PositionPlanTechniqueResourceContest]): PositionPlanTechniqueSemanticDetail =
      if positionPlanTechniqueResourceContestApplies(detail) then
        contest.filter(positionPlanTechniqueResourceContestMatchesDetail(detail, _)).fold(detail)(resource =>
          detail.copy(
            resourceContestActorSide = resource.actorSide,
            resourceContestTargetSide = resource.targetSide,
            resourceContestKinds = (detail.resourceContestKinds ++ resource.kinds).distinct.sorted,
            resourceContestSignals = (detail.resourceContestSignals ++ resource.signals).distinct.sorted,
            resourceContestSquares = (detail.resourceContestSquares ++ resource.squares).distinct.sorted,
            resourceContestFiles = (detail.resourceContestFiles ++ resource.files).distinct.sorted,
            resourceContestScopes = (detail.resourceContestScopes ++ resource.scopes).distinct.sorted,
            resourceContestMagnitude = resource.magnitude
          )
        )
      else detail

    private def withStructuralPurpose(purpose: Option[PositionPlanTechniqueStructuralPurpose]): PositionPlanTechniqueSemanticDetail =
      if positionPlanTechniqueStructuralPurposeApplies(detail, purpose.nonEmpty) then
        purpose.fold(detail)(structural =>
          val purposeSubjects =
            if detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute &&
                !structural.subjects.exists(positionPlanTechniquePieceRouteSubject)
            then (structural.subjects ++ structural.transitionRouteSubject).distinct.sorted
            else structural.subjects
          val subjectLabels = purposeSubjects.map(_.trim.toLowerCase)
          val breakLabel = subjectLabels.collectFirst { case subject if subject.startsWith("break-file:") => subject.replace(':', '-') }
          val tensionLabel = subjectLabels.collectFirst {
            case subject if subject.startsWith("created-tension:") || subject.startsWith("resolved-tension:") =>
              subject.replace(':', '-')
          }
          val structuralLabel =
            detail.label.orElse(
              Option.when(breakLabel.nonEmpty)((breakLabel.toList ++ tensionLabel.toList).mkString("-")).orElse(
                structural.consequenceKinds.collectFirst {
                  case TransitionConsequenceKind.WeakPawnTargetCreated        => "weak-pawn-target"
                  case TransitionConsequenceKind.WeakSquareTargetCreated      => "weak-square-target"
                  case TransitionConsequenceKind.TargetPressureGain           => "target-pressure-gain"
                  case TransitionConsequenceKind.TargetPressureRelease        => "target-pressure-release"
                  case TransitionConsequenceKind.LineUnlockGain               => "line-unlock"
                  case TransitionConsequenceKind.PieceExchangeAvailable       => "piece-exchange-available"
                  case TransitionConsequenceKind.PieceExchangeCompleted       => "piece-exchange-completed"
                  case TransitionConsequenceKind.MobilityGain                 => "mobility-gain"
                  case TransitionConsequenceKind.MobilityLoss                 => "mobility-loss"
                  case TransitionConsequenceKind.OpponentMobilityRestriction  => "opponent-mobility-restriction"
                }
              )
            )
          detail.copy(
            label = structuralLabel,
            sourceEvidenceIds = (detail.sourceEvidenceIds ++ structural.sourceIds).distinct.sorted,
            structuralRouteMove = structural.routeMove,
            structuralRouteRole = structural.routeRole,
            structuralRoutePerspective = structural.routePerspective,
            structuralRouteFromPly = structural.routeFromPly,
            structuralRouteToPly = structural.routeToPly,
            structuralPurposeConsequences = structural.consequences,
            structuralConsequenceKinds = structural.consequenceKinds,
            structuralPurposeSubjects = purposeSubjects,
            structuralPurposeCategories = structural.categories,
            structuralPurposePolarities = structural.polarities,
            structuralPurposeStrength = structural.strength,
            breakFile = detail.breakFile.orElse(positionPlanTechniqueStructuralBreakFile(purposeSubjects)),
            tensionEdges = (detail.tensionEdges ++ positionPlanTechniqueStructuralTensionEdges(purposeSubjects)).distinct.sorted
          )
        )
      else detail

    private def withThreatProjection(threat: Option[PositionPlanTechniqueThreatProjection]): PositionPlanTechniqueSemanticDetail =
      if positionPlanTechniqueThreatProjectionApplies(detail) then
        threat.fold(detail)(projection =>
          detail.copy(
            threatKind = detail.threatKind.orElse(projection.kind),
            threatDriver = detail.threatDriver.orElse(projection.driver),
            threatSeverity = detail.threatSeverity.orElse(projection.severity),
            turnsToImpact = detail.turnsToImpact.orElse(projection.turnsToImpact),
            defenseMove = detail.defenseMove.orElse(projection.defenseMove),
            prophylaxisNeeded = detail.prophylaxisNeeded.orElse(projection.prophylaxisNeeded),
            maxWinPercentLossIfIgnored = detail.maxWinPercentLossIfIgnored.orElse(projection.maxWinPercentLossIfIgnored),
            resourceContestScopes =
              (detail.resourceContestScopes ++ Option.when(projection.prophylaxisNeeded.contains(true))("prophylaxis").toList).distinct.sorted
          )
        )
      else detail

  private def positionPlanTechniqueExpandedSourceIds(
      graph: TypedEvidenceGraph,
      sourceIds: List[String]
  ): List[String] =
    sourceIds
      .flatMap(id =>
        graph.byId
          .get(id)
          .fold(List(id))(record => positionPlanTechniqueRecordLineage(graph, record, 2).map(_.ref.id))
      )
      .distinct
      .sorted

  private def positionPlanTechniqueRecordLineage(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      remainingParentDepth: Int
  ): List[EvidenceRecord] =
    def loop(current: EvidenceRecord, remaining: Int, visited: Set[String]): List[EvidenceRecord] =
      if visited.contains(current.ref.id) then Nil
      else
        val nextVisited = visited + current.ref.id
        current ::
          Option
            .when(remaining > 0)(
              current.parents
                .flatMap(parent => graph.byId.get(parent.id))
                .flatMap(parentRecord => loop(parentRecord, remaining - 1, nextVisited))
            )
            .getOrElse(Nil)
    loop(record, remainingParentDepth, Set.empty)

  private[chessjudgment] def principalCausalSelectionPool(
      candidates: List[(PlanCausalEventEvidence, (Int, Int, Int, Int))],
      currentMoveFunctionProofReady: PlanCausalEventEvidence => Boolean
  ): List[(PlanCausalEventEvidence, (Int, Int, Int, Int))] =
    val currentMoveFunctions = candidates.filter { case (event, _) =>
      currentMoveFunctionProofReady(event)
    }
    if currentMoveFunctions.nonEmpty then currentMoveFunctions else candidates

  private[chessjudgment] def principalCausalOwnershipTier(
      event: PlanCausalEventEvidence,
      preparedPawnAdvanceObserved: Boolean
  ): Int =
    val directOrInducedRootFunction =
      event.directPieceRoleProofReady ||
        event.directPawnRoleProofReady ||
        event.goalDependencyProofReady ||
        preparedPawnAdvanceObserved ||
        event.planVerifiedResponseGoalResults.nonEmpty ||
        event.conditionalResponseContinuationResults.nonEmpty ||
        event.observedVacatedSquareRouteProofReady ||
        event.materialTradeoffs.exists(_._1.offerCapture.nonEmpty)
    if directOrInducedRootFunction then 0
    else if event.representativeGoalResultAssessment.exists(_.robustness == PlanCausalRobustness.Robust) then 1
    else if event.directGoalConsequences.exists(consequence => consequence.positive && consequence.strength > 0) then 2
    else if
      event.representativeGoalResultAssessment.exists(_.robustness == PlanCausalRobustness.Conditional) ||
        event.opponentResourceDeterrenceProofReady
    then 3
    else 4

  private def positionPlanTechniquePlanEvidenceForSources(
      graph: TypedEvidenceGraph,
      sourceIds: List[String],
      detail: PositionPlanTechniqueSemanticDetail
  ): PositionPlanTechniquePlanEvidence =
    val directRecords = sourceIds.flatMap(id => graph.byId.get(id))
    val records =
      directRecords
        .flatMap(positionPlanTechniqueRecordLineage(graph, _, 2))
        .distinctBy(_.ref.id)
    val eventRecords = records.collect {
      case record @ EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
          if ref.confidence != EvidenceConfidence.Heuristic && event.publicGoalProofReady =>
        record -> event
    }.distinctBy(_._1.ref.id)
    val sharedEventLine = eventRecords.map(_._2.rootLine).distinct match
      case line :: Nil => Some(line)
      case _           => None
    val sharedEventAfter = eventRecords.map(_._2.rootTransition.to).distinct match
      case position :: Nil => Some(position)
      case _               => None
    val pressureRecord =
      for
        line <- sharedEventLine
        after <- sharedEventAfter
        record <- graph.records.collectFirst {
          case record @ EvidenceRecord(ref, _: PlanPressureEvidence, _)
              if ref.line.contains(line) && ref.position == after =>
            record
        }
      yield record
    val resolvedPressureRecord = pressureRecord.orElse(positionPlanTechniquePressureRecordForSources(graph, sourceIds))
    val pressure = resolvedPressureRecord.collect { case EvidenceRecord(_, payload: PlanPressureEvidence, _) => payload }
    val pressureRootMove = resolvedPressureRecord.flatMap(_.ref.line.map(_.rootMove)).orElse(sharedEventLine.map(_.rootMove))
    def uniqueEvent(planId: PlanId): Option[(EvidenceRecord, PlanCausalEventEvidence)] =
      eventRecords.filter(_._2.planId == planId) match
        case event :: Nil => Some(event)
        case _            => None
    val directEventRecord = eventRecords match
      case event :: Nil => Some(event)
      case _ =>
        pressure.toList
          .flatMap(_.activePlanIds(pressureRootMove))
          .collectFirst(Function.unlift(uniqueEvent))
    val rootMove = directEventRecord.map(_._2.rootMove).orElse(pressureRootMove)
    val pressurePlanIds = pressure.toList.flatMap(_.activePlanIds(rootMove))
    val eventLine =
      directEventRecord
        .map(_._2.rootLine)
        .orElse(sharedEventLine)
        .orElse(resolvedPressureRecord.flatMap(_.ref.line))
    val authoritativeRootEventRecords = rootMove.toList.flatMap(move =>
      graph.records.collect {
        case record @ EvidenceRecord(ref, candidate: PlanCausalEventEvidence, _)
            if ref.confidence != EvidenceConfidence.Heuristic &&
              candidate.publicGoalProofReady &&
              EvidenceRef.sameMove(candidate.rootMove, move) &&
              eventLine.forall(_ == candidate.rootLine) =>
          record -> candidate
      }
    ).distinctBy(_._1.ref.id)
    val authoritativeRootEvents = authoritativeRootEventRecords.map(_._2).distinct
    val authoritativeRootPlanIds = authoritativeRootEvents.map(_.planId).toSet
    val provenEpisodePlanIds = authoritativeRootEvents.filter(_.episodePublicProofReady).map(_.planId).toSet
    val pressureOrder = pressurePlanIds.zipWithIndex.toMap
    def preparedPawnAdvanceObserved(event: PlanCausalEventEvidence): Boolean =
      val preparedMoves = event.directPreparedPawnAdvances.map { case (from, to) => s"$from$to" }
      preparedMoves.nonEmpty &&
        (
          event.episode.exists(_.futureEvents.exists(step =>
            preparedMoves.exists(EvidenceRef.sameMove(_, step.moveUci))
          )) ||
            graph.records.exists {
              case EvidenceRecord(ref, line: LineFactEvidence, _)
                  if ref.line.contains(event.rootLine) =>
                line.lineReplayMoves.drop(1).exists(move => preparedMoves.exists(EvidenceRef.sameMove(_, move)))
              case _ => false
            }
        )
    def currentMoveFunctionProofReady(event: PlanCausalEventEvidence): Boolean =
      event.principalCurrentMoveFunctionProofReady || preparedPawnAdvanceObserved(event)
    val principalCausalCandidates = authoritativeRootEvents
      .flatMap(event => event.principalExplanationSortKey.map(event -> _))
    val principalCausalFrontier = principalCausalCandidates.filterNot { case (candidate, _) =>
      val (strength, salience, depth) = candidate.principalOwnedResultKey.getOrElse((0, 0, 0))
      !currentMoveFunctionProofReady(candidate) &&
        !candidate.materialCounterplayPreventionProofReady &&
        principalCausalCandidates.exists { case (other, _) =>
          val (otherStrength, otherSalience, otherDepth) = other.principalOwnedResultKey.getOrElse((0, 0, 0))
          other != candidate &&
            (candidate.directGoalConsequences.isEmpty || other.directGoalConsequences.nonEmpty) &&
            otherStrength >= strength &&
            otherSalience >= salience &&
            otherDepth >= depth &&
            (otherStrength > strength || otherSalience > salience || otherDepth > depth)
        }
    }
    val principalSelectionPool = principalCausalSelectionPool(principalCausalFrontier, currentMoveFunctionProofReady)
    val principalCausalEventPlanId = principalSelectionPool
      .sortBy { case (candidate, (proofSpecificity, resultStrength, causalDepth, resultSalience)) =>
        val rootActorGoalProofReady = candidate.rootActorGoalProofReady
        val effectiveProofSpecificity =
          if rootActorGoalProofReady then math.max(proofSpecificity, 4)
          else proofSpecificity
        (
          principalCausalOwnershipTier(candidate, preparedPawnAdvanceObserved(candidate)),
          -effectiveProofSpecificity,
          if rootActorGoalProofReady then 0 else 1,
          -resultStrength,
          -causalDepth,
          -resultSalience,
          pressureOrder.getOrElse(candidate.planId, Int.MaxValue),
          candidate.planId.toString
        )
      }
      .headOption
      .map(_._1.planId)
    val dominatedCausalEvents =
      principalCausalCandidates.map(_._1).toSet -- principalCausalFrontier.map(_._1).toSet
    val principalPlanId =
      principalCausalEventPlanId
        .orElse(
          pressurePlanIds
            .find(provenEpisodePlanIds)
            .orElse(pressurePlanIds.find(authoritativeRootPlanIds))
            .orElse(
              authoritativeRootPlanIds.toList match
                case planId :: Nil => Some(planId)
                case _             => None
            )
            .orElse(directEventRecord.map(_._2.planId))
        )
    val matchingAuthoritativeRecords = authoritativeRootEventRecords.filter { case (_, candidate) =>
      positionPlanTechniqueEventOwnsDetail(candidate, detail)
    }
    val inferredEventRecord =
      principalPlanId
        .flatMap(planId =>
          matchingAuthoritativeRecords.filter(_._2.planId == planId) match
            case event :: Nil => Some(event)
            case _            => None
        )
        .orElse(
          matchingAuthoritativeRecords match
            case event :: Nil => Some(event)
            case _            => None
        )
    val eventRecord = directEventRecord.orElse(inferredEventRecord)
    val event = eventRecord.map(_._2)
    val displayPlanId = event.map(_.planId)
    val transition =
      event
        .flatMap(event =>
          graph.records.collect {
            case EvidenceRecord(_, PlanTransitionEvidence(summary), _)
                if event.planSequenceSummary.contains(summary) =>
              summary
          }.distinct match
            case summary :: Nil => Some(summary)
            case _              => None
        )
    PositionPlanTechniquePlanEvidence(
      displayPlanId = displayPlanId,
      activePlanIds = (principalPlanId.toList ++ pressurePlanIds ++ event.map(_.planId).toList).distinct,
      mainExplanationCandidate = event.forall(candidate => !dominatedCausalEvents(candidate)),
      principalCausalEventAvailable = principalPlanId.exists(authoritativeRootPlanIds),
      previousPlanId = transition.flatMap(_.previousPlanId),
      transitionType = transition.map(_.transitionType),
      event = event,
      eventEvidenceId = eventRecord.map(_._1.ref.id)
    )

  private def positionPlanTechniqueEventOwnsDetail(
      event: PlanCausalEventEvidence,
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    val consequenceKinds = detail.structuralConsequenceKinds.distinct
    val detailSubjects = detail.structuralPurposeSubjects.map(_.trim.toLowerCase).filter(_.nonEmpty).toSet
    consequenceKinds match
      case consequenceKind :: Nil
          if detail.structuralRouteMove.exists(EvidenceRef.sameMove(_, event.rootMove)) =>
        event.directGoalConsequences.exists { consequence =>
          val eventSubjects = consequence.subjects.map(_.trim.toLowerCase).filter(_.nonEmpty).toSet
          consequence.kind == consequenceKind &&
          (
            detailSubjects.isEmpty ||
              eventSubjects.nonEmpty &&
                (detailSubjects.subsetOf(eventSubjects) || eventSubjects.subsetOf(detailSubjects))
          )
        }
      case _ => false

  private def positionPlanTechniquePressureRecordForSources(
      graph: TypedEvidenceGraph,
      sourceIds: List[String]
  ): Option[EvidenceRecord] =
    val directRecords = sourceIds.flatMap(id => graph.byId.get(id))
    directRecords
      .collectFirst { case record @ EvidenceRecord(_, _: PlanPressureEvidence, _) => record }
      .orElse {
        val structuralIds = directRecords.collect {
          case EvidenceRecord(ref, _: StructuralDeltaEvidence, _) => ref.id
        }.toSet
        graph.records.collectFirst {
          case record @ EvidenceRecord(_, _: PlanPressureEvidence, parents)
              if parents.exists(parent => structuralIds.contains(parent.id)) =>
            record
        }
      }

  private def positionPlanTechniquePawnPlayBySourceId(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): Map[String, PawnPlayAnalysis] =
    refs.flatMap(ref =>
      graph.byId
        .get(ref.id)
        .flatMap(record =>
          positionPlanTechniqueRecordLineage(graph, record, 2).collectFirst {
            case EvidenceRecord(_, PawnStructureFactEvidence(_, _, Some(pawnPlay)), _) => pawnPlay
          }
        )
        .map(ref.id -> _)
    ).toMap

  private def positionPlanTechniquePawnPlayForSources(
      sourceIds: List[String],
      pawnPlayBySourceId: Map[String, PawnPlayAnalysis]
  ): Option[PawnPlayAnalysis] =
    sourceIds.collectFirst(Function.unlift(pawnPlayBySourceId.get))

  private def positionPlanTechniquePlanAlignmentBySourceId(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): Map[String, PlanAlignment] =
    refs.flatMap(ref =>
      graph.byId
        .get(ref.id)
        .flatMap(record =>
          positionPlanTechniqueRecordLineage(graph, record, 2).collectFirst {
            case EvidenceRecord(_, PawnStructureFactEvidence(_, Some(alignment), _), _) => alignment
          }
        )
        .map(ref.id -> _)
    ).toMap

  private def positionPlanTechniquePlanAlignmentForSources(
      sourceIds: List[String],
      planAlignmentBySourceId: Map[String, PlanAlignment]
  ): Option[PlanAlignment] =
    sourceIds.collectFirst(Function.unlift(planAlignmentBySourceId.get))

  private def positionPlanTechniqueOpeningPriorBySourceId(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): Map[String, OpeningThemePriorSelection] =
    refs.flatMap(ref =>
      graph.byId
        .get(ref.id)
        .flatMap(record =>
          positionPlanTechniqueRecordLineage(graph, record, 2).collectFirst {
            case EvidenceRecord(_, OpeningContextEvidence(_, _, _, Some(selection)), _) => selection
          }
        )
        .map(ref.id -> _)
    ).toMap

  private def positionPlanTechniqueOpeningPriorForSources(
      sourceIds: List[String],
      openingPriorBySourceId: Map[String, OpeningThemePriorSelection]
  ): Option[OpeningThemePriorSelection] =
    sourceIds.collectFirst(Function.unlift(openingPriorBySourceId.get))

  private def positionPlanTechniqueResourceContestBySourceId(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): Map[String, PositionPlanTechniqueResourceContest] =
    refs.flatMap(ref =>
      graph.byId
        .get(ref.id)
        .flatMap(record => positionPlanTechniqueResourceContestForLineage(positionPlanTechniqueRecordLineage(graph, record, 2)))
        .map(ref.id -> _)
    ).toMap

  private def positionPlanTechniqueResourceContestForSources(
      sourceIds: List[String],
      resourceContestBySourceId: Map[String, PositionPlanTechniqueResourceContest]
  ): Option[PositionPlanTechniqueResourceContest] =
    positionPlanTechniqueMergeResourceContests(sourceIds.flatMap(resourceContestBySourceId.get))

  private def positionPlanTechniqueStructuralPurposeBySourceId(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): Map[String, List[PositionPlanTechniqueStructuralPurpose]] =
    refs.flatMap(ref =>
      graph.byId
        .get(ref.id)
        .map(record => ref.id -> positionPlanTechniqueStructuralPurposesForLineage(positionPlanTechniqueRecordLineage(graph, record, 2)))
    ).toMap

  private def positionPlanTechniqueStructuralPurposeForSources(
      sourceIds: List[String],
      structuralPurposeBySourceId: Map[String, List[PositionPlanTechniqueStructuralPurpose]]
  ): Option[PositionPlanTechniqueStructuralPurpose] =
    positionPlanTechniqueMergeStructuralPurposes(sourceIds.flatMap(id => structuralPurposeBySourceId.getOrElse(id, Nil)))

  private def positionPlanTechniqueStructuralPurposeForDetailSources(
      detail: PositionPlanTechniqueSemanticDetail,
      sourceIds: List[String],
      structuralPurposeBySourceId: Map[String, List[PositionPlanTechniqueStructuralPurpose]]
  ): Option[PositionPlanTechniqueStructuralPurpose] =
    positionPlanTechniqueMergeStructuralPurposes(
      sourceIds
        .flatMap(id => structuralPurposeBySourceId.getOrElse(id, Nil))
        .filter(positionPlanTechniqueStructuralPurposeMatchesDetail(detail, _))
    )

  private def positionPlanTechniqueRoutePurposeBySourceId(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): Map[String, List[PositionPlanTechniqueStructuralPurpose]] =
    refs.flatMap(ref =>
      graph.byId.get(ref.id).map(record => ref.id -> positionPlanTechniqueRoutePurposes(record))
    ).toMap

  private def positionPlanTechniqueRoutePurposes(record: EvidenceRecord): List[PositionPlanTechniqueStructuralPurpose] =
    record.payload match
      case payload: MoveMotifEvidence =>
        val move = payload.moveUci.trim.toLowerCase
        payload.motif match
          case Motif.KingStep(_, color, _, _) =>
            positionPlanTechniqueRoutePurpose(record.ref.id, move, color, List(s"king:${move.take(2)}-${move.slice(2, 4)}")).toList
          case Motif.Castling(side, color, _, _) =>
            val rank = if color.white then "1" else "8"
            val (kingTo, rookFrom, rookTo) =
              if side == Motif.CastlingSide.Kingside then (s"g$rank", s"h$rank", s"f$rank")
              else (s"c$rank", s"a$rank", s"d$rank")
            positionPlanTechniqueRoutePurpose(
              record.ref.id,
              move,
              color,
              List(s"king:e$rank-$kingTo", s"rook:$rookFrom-$rookTo")
            ).toList
          case _ =>
            Nil
      case _ =>
        Nil

  private def positionPlanTechniqueRoutePurpose(
      sourceId: String,
      move: String,
      color: chess.Color,
      subjects: List[String]
  ): Option[PositionPlanTechniqueStructuralPurpose] =
    Option.when(move.matches("[a-h][1-8][a-h][1-8].*") && subjects.nonEmpty)(
      PositionPlanTechniqueStructuralPurpose(
        sourceIds = List(sourceId),
        routeMove = Some(move),
        transitionRouteSubject = None,
        routeRole = None,
        routePerspective = Some(if color.white then "White" else "Black"),
        routeFromPly = None,
        routeToPly = None,
        consequenceKinds = List(TransitionConsequenceKind.DevelopmentPieceActivated),
        consequences = List(TransitionConsequenceKind.DevelopmentPieceActivated.toString),
        subjects = subjects.distinct.sorted,
        categories = List(TransitionConsequenceCategory.PieceActivity.toString, TransitionConsequenceCategory.StrategicSupport.toString),
        polarities = List(StructuralSignalPolarity.Gain.toString),
        strength = Some(subjects.size)
      )
    )

  private def positionPlanTechniqueStructureRouteContextSourceIds(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): List[String] =
    val currentPly = refs.map(_.position.ply).minOption
    val currentPositions = refs.map(_.position).filter(position => currentPly.contains(position.ply)).distinct
    graph.records
      .filter(record =>
        record.ref.layer == EvidenceLayer.PawnStructure &&
          positionPlanTechniqueActiveStructureScope(record.ref.scope) &&
          currentPositions.contains(record.ref.position) &&
          positionPlanTechniqueStructureRouteContextKeys(
            StrategicMechanismEvidence.sourceSemanticAnchors(record).map(_.stableKey),
            Nil
          )
      )
      .map(_.ref)
      .map(_.id)
      .distinct
      .sorted

  private def positionPlanTechniqueActiveStructureScope(scope: EvidenceScope): Boolean =
    scope == EvidenceScope.BeforePosition || scope == EvidenceScope.CurrentPosition

  private def positionPlanTechniqueStructureRouteContext(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    positionPlanTechniqueOpenCenterContext(detail) || positionPlanTechniqueIqpContext(detail)

  private def positionPlanTechniqueStructureRouteContextKeys(
      anchorKeys: List[String],
      openingPriorTypicalPawnStructures: List[String]
  ): Boolean =
    positionPlanTechniqueOpenCenterContextKeys(anchorKeys, openingPriorTypicalPawnStructures) ||
      positionPlanTechniqueIqpContextKeys(anchorKeys, openingPriorTypicalPawnStructures)

  private def positionPlanTechniqueOpenCenterContext(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    positionPlanTechniqueOpenCenterContextKeys(detail.semanticAnchorKeys, detail.openingPriorTypicalPawnStructures)

  private def positionPlanTechniqueIqpContext(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    positionPlanTechniqueIqpContextKeys(detail.semanticAnchorKeys, detail.openingPriorTypicalPawnStructures)

  private def positionPlanTechniqueOpenCenterContextKeys(
      anchorKeys: List[String],
      openingPriorTypicalPawnStructures: List[String]
  ): Boolean =
    val normalizedAnchorKeys = anchorKeys.map(_.toLowerCase)
    normalizedAnchorKeys.exists(key =>
      key == "pawnstructure:opencenter" ||
        key == "pawnplay:center-break" ||
        key.contains("openinganchor:pawnstructure:pawnbreakobserved") ||
        key.contains("pawnbreakpreparation")
    ) ||
      openingPriorTypicalPawnStructures.exists(value =>
        val normalized = value.toLowerCase
        normalized.contains("open")
      )

  private def positionPlanTechniqueIqpContextKeys(
      anchorKeys: List[String],
      openingPriorTypicalPawnStructures: List[String]
  ): Boolean =
    val normalizedAnchorKeys = anchorKeys.map(_.toLowerCase)
    normalizedAnchorKeys.exists(key =>
      key == "pawnstructure:iqp" ||
        key == "pawnstructure:iqpwhite" ||
        key == "pawnstructure:iqpblack"
    ) ||
      openingPriorTypicalPawnStructures.exists(value =>
        val normalized = value.toLowerCase
        normalized.contains("iqp")
      )

  private def positionPlanTechniqueDevelopmentRoutePurpose(
      purpose: PositionPlanTechniqueStructuralPurpose
  ): Boolean =
    purpose.routeMove.nonEmpty &&
      (
        purpose.subjects.exists(positionPlanTechniquePieceRouteSubject) ||
          purpose.consequenceKinds.exists(positionPlanTechniquePieceRouteConsequence) ||
          purpose.categories.exists(positionPlanTechniqueDevelopmentRouteCategory) ||
          purpose.consequenceKinds.isEmpty && purpose.transitionRouteSubject.exists(_.trim.nonEmpty)
      )

  private def positionPlanTechniqueDevelopmentRouteDetail(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.structuralRouteMove.nonEmpty &&
      (
        detail.structuralPurposeSubjects.exists(positionPlanTechniquePieceRouteSubject) ||
          detail.structuralPurposeConsequences.exists(value =>
            TransitionConsequenceKind.values.exists(kind =>
              kind.toString.equalsIgnoreCase(value) && positionPlanTechniquePieceRouteConsequence(kind)
            )
          ) ||
          detail.structuralPurposeCategories.exists(positionPlanTechniqueDevelopmentRouteCategory)
      )

  private def positionPlanTechniqueDevelopmentRouteCategory(category: String): Boolean =
    val normalized = category.toLowerCase
    normalized.contains("development") || normalized.contains("pieceactivity")

  private def positionPlanTechniqueStructuralBreakFile(subjects: List[String]): Option[String] =
    subjects.collectFirst {
      case subject if subject.toLowerCase.startsWith("break-file:") =>
        subject.drop("break-file:".length).trim.toLowerCase
    }.filter(_.nonEmpty)

  private def positionPlanTechniqueStructuralTensionEdges(subjects: List[String]): List[String] =
    subjects.flatMap { subject =>
      val normalized = subject.toLowerCase.trim
      List("created-tension:", "resolved-tension:").collect {
        case prefix if normalized.startsWith(prefix) =>
          normalized.drop(prefix.length)
      }
    }.filter(_.nonEmpty).distinct.sorted

  private def positionPlanTechniqueThreatBySourceId(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): Map[String, PositionPlanTechniqueThreatProjection] =
    refs.flatMap(ref =>
      graph.byId
        .get(ref.id)
        .flatMap(record =>
          positionPlanTechniqueRecordLineage(graph, record, 2).collectFirst {
            case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) => positionPlanTechniqueThreatProjection(payload)
          }
        )
        .map(ref.id -> _)
    ).toMap

  private def positionPlanTechniqueThreatForSources(
      sourceIds: List[String],
      threatBySourceId: Map[String, PositionPlanTechniqueThreatProjection]
  ): Option[PositionPlanTechniqueThreatProjection] =
    positionPlanTechniqueMergeThreatProjections(sourceIds.flatMap(threatBySourceId.get))

  private def positionPlanTechniqueThreatProjection(payload: ThreatEpisodeEvidence): PositionPlanTechniqueThreatProjection =
    val episode = payload.episode
    PositionPlanTechniqueThreatProjection(
      kind = Some(episode.kind.toString),
      driver = Some(episode.driver.toString),
      severity = Some(episode.severity.toString),
      turnsToImpact = Some(episode.turnsToImpact),
      defenseMove = payload.onlyDefense.orElse(episode.bestDefense),
      prophylaxisNeeded = Some(payload.prophylaxisNeeded),
      maxWinPercentLossIfIgnored = payload.maxWinPercentLossIfIgnored
    )

  private def positionPlanTechniqueMergeThreatProjections(
      projections: List[PositionPlanTechniqueThreatProjection]
  ): Option[PositionPlanTechniqueThreatProjection] =
    Option.when(projections.nonEmpty) {
      PositionPlanTechniqueThreatProjection(
        kind = positionPlanTechniqueSingleValue(projections.flatMap(_.kind)),
        driver = positionPlanTechniqueSingleValue(projections.flatMap(_.driver)),
        severity = positionPlanTechniqueSingleValue(projections.flatMap(_.severity)),
        turnsToImpact = positionPlanTechniqueSingleInt(projections.flatMap(_.turnsToImpact)),
        defenseMove = positionPlanTechniqueSingleValue(projections.flatMap(_.defenseMove)),
        prophylaxisNeeded = positionPlanTechniqueSingleBoolean(projections.flatMap(_.prophylaxisNeeded)),
        maxWinPercentLossIfIgnored = projections.flatMap(_.maxWinPercentLossIfIgnored).sorted.lastOption
      )
    }

  private def positionPlanTechniqueStructuralPurposesForLineage(
      records: List[EvidenceRecord]
  ): List[PositionPlanTechniqueStructuralPurpose] =
    records.collect { case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) =>
      positionPlanTechniqueStructuralPurposes(ref.id, payload)
    }.flatten

  private def positionPlanTechniqueStructuralPurposes(
      sourceId: String,
      payload: StructuralDeltaEvidence
  ): List[PositionPlanTechniqueStructuralPurpose] =
    val consequences = payload.consequences.filter(_.strength > 0)
    val groupedConsequences =
      if consequences.nonEmpty then consequences.map(List(_))
      else List(Nil)
    groupedConsequences.map(positionPlanTechniqueStructuralPurpose(sourceId, payload, _))

  private def positionPlanTechniqueStructuralPurpose(
      sourceId: String,
      payload: StructuralDeltaEvidence,
      consequences: List[TransitionConsequence]
  ): PositionPlanTechniqueStructuralPurpose =
    PositionPlanTechniqueStructuralPurpose(
      sourceIds = List(sourceId),
      routeMove = Some(payload.transition.moveUci).filter(_.nonEmpty),
      transitionRouteSubject = positionPlanTechniqueTransitionRouteSubject(payload),
      routeRole = Some(payload.transition.role.toString),
      routePerspective = Some(positionPlanTechniqueColorKey(payload.transition.perspective)),
      routeFromPly = Some(payload.transition.from.ply),
      routeToPly = Some(payload.transition.to.ply),
      consequenceKinds = consequences.map(_.kind).distinct,
      consequences = consequences.map(_.kind.toString).distinct.sorted,
      subjects = consequences.flatMap(_.subjects).distinct.sorted,
      categories = consequences.flatMap(consequence => positionPlanTechniqueStructuralPurposeCategories(consequence.kind)).distinct.sorted,
      polarities = consequences.map(_.polarity.toString).distinct.sorted,
      strength = Option.when(consequences.nonEmpty)(consequences.map(_.strength).sum)
    )

  private def positionPlanTechniqueTransitionRouteSubject(payload: StructuralDeltaEvidence): Option[String] =
    for
      move <- Some(payload.transition.moveUci.trim.toLowerCase).filter(_.length >= 4)
      fromKey = move.take(2)
      toKey = move.slice(2, 4)
      from <- _root_.chess.Square.all.find(_.key == fromKey)
      position <- _root_.chess.format.Fen.read(
        _root_.chess.variant.Standard,
        _root_.chess.format.Fen.Full(payload.transition.from.fen)
      )
      piece <- position.board.pieceAt(from)
      role = piece.role.name.trim.toLowerCase
      if piece.color == payload.transition.perspective && role != "pawn" && role != "king"
    yield s"$role:$fromKey-$toKey:maneuver"

  private def positionPlanTechniqueStructuralPurposeMatchesDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      purpose: PositionPlanTechniqueStructuralPurpose
  ): Boolean =
    val opponentMobilityPurpose =
      purpose.consequenceKinds.contains(TransitionConsequenceKind.OpponentMobilityRestriction)
    val kindMatches =
      if detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute then
        purpose.consequenceKinds.exists(positionPlanTechniquePieceRouteConsequence) ||
          purpose.categories.exists(category =>
            category == TransitionConsequenceCategory.PieceActivity.toString ||
              category == TransitionConsequenceCategory.Development.toString ||
              category == TransitionConsequenceCategory.OpeningDevelopment.toString
          )
      else detail.axisKind match
        case Some(StrategicAxisKind.Counterplay)
            if detail.axisPolarity.contains(StrategicAxisPolarity.Restrain) &&
              opponentMobilityPurpose =>
          purpose.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
        case _ if opponentMobilityPurpose =>
          false
        case Some(StrategicAxisKind.Counterplay) if detail.axisPolarity.contains(StrategicAxisPolarity.Restrain) =>
          false
        case Some(StrategicAxisKind.PawnBreak) =>
          if positionPlanTechniquePawnTensionDetail(detail) ||
            purpose.consequenceKinds.exists(positionPlanTechniquePawnTensionConsequence)
          then
            val acceptedKinds =
              detail.axisPolarity match
                case Some(StrategicAxisPolarity.Release) =>
                  Set(TransitionConsequenceKind.PawnTensionResolution)
                case Some(StrategicAxisPolarity.Support | StrategicAxisPolarity.Preserve | StrategicAxisPolarity.Gain) =>
                  Set(TransitionConsequenceKind.PawnTensionGain)
                case _ =>
                  Set(TransitionConsequenceKind.PawnTensionGain, TransitionConsequenceKind.PawnTensionResolution)
            purpose.consequenceKinds.exists(acceptedKinds)
          else true
        case Some(StrategicAxisKind.Target) =>
          val typedTargetKinds = detail.structuralConsequenceKinds.filter(positionPlanTechniqueTargetConsequence).toSet
          if typedTargetKinds.nonEmpty then purpose.consequenceKinds.exists(typedTargetKinds)
          else
            purpose.consequenceKinds.exists(positionPlanTechniqueTargetConsequence) ||
              purpose.categories.contains(TransitionConsequenceCategory.TargetPressure.toString)
        case Some(StrategicAxisKind.SpaceCenter) =>
          purpose.consequenceKinds.exists(positionPlanTechniqueCenterConsequence) ||
            purpose.categories.exists(category =>
              category == TransitionConsequenceCategory.CenterControl.toString ||
                category == TransitionConsequenceCategory.OpeningCenterControl.toString
            )
        case _ =>
          true
    val polarityMatches =
      if detail.unit == PositionPlanTechniqueUnit.PlanOptionSet then true
      else
        detail.axisPolarity match
          case Some(polarity) =>
            val acceptedPolarities = positionPlanTechniqueStructuralPolaritiesForAxis(polarity)
            purpose.polarities.isEmpty || purpose.polarities.exists(acceptedPolarities)
          case None =>
            true
    kindMatches && polarityMatches

  private def positionPlanTechniquePawnTensionDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.tensionEdges.nonEmpty ||
      detail.tensionSquares.nonEmpty ||
      detail.structuralPurposeConsequences.exists(consequence =>
        consequence.equalsIgnoreCase(TransitionConsequenceKind.PawnTensionGain.toString) ||
          consequence.equalsIgnoreCase(TransitionConsequenceKind.PawnTensionResolution.toString)
      )

  private def positionPlanTechniquePawnTensionConsequence(kind: TransitionConsequenceKind): Boolean =
    kind == TransitionConsequenceKind.PawnTensionGain ||
      kind == TransitionConsequenceKind.PawnTensionResolution

  private def positionPlanTechniqueTargetConsequence(kind: TransitionConsequenceKind): Boolean =
    kind == TransitionConsequenceKind.TargetPressureGain ||
      kind == TransitionConsequenceKind.TargetPressureRelease ||
      kind == TransitionConsequenceKind.KingSafetyPressure ||
      kind == TransitionConsequenceKind.KingRingPressureGain ||
      kind == TransitionConsequenceKind.WeakPawnTargetCreated ||
      kind == TransitionConsequenceKind.WeakSquareTargetCreated

  private def positionPlanTechniqueCenterConsequence(kind: TransitionConsequenceKind): Boolean =
    kind == TransitionConsequenceKind.SpaceGain ||
      kind == TransitionConsequenceKind.CenterControlGain ||
      kind == TransitionConsequenceKind.CenterControlLoss ||
      kind == TransitionConsequenceKind.DevelopmentCenterControlGain ||
      kind == TransitionConsequenceKind.DevelopmentCenterControlLoss

  private def positionPlanTechniquePieceRouteConsequence(kind: TransitionConsequenceKind): Boolean =
    kind == TransitionConsequenceKind.DevelopmentLagReduced ||
      kind == TransitionConsequenceKind.DevelopmentLagIncreased ||
      kind == TransitionConsequenceKind.DevelopmentPieceActivated ||
      kind == TransitionConsequenceKind.DevelopmentPieceRetreated ||
      kind == TransitionConsequenceKind.DevelopmentMobilityGain ||
      kind == TransitionConsequenceKind.DevelopmentMobilityLoss ||
      kind == TransitionConsequenceKind.MobilityGain ||
      kind == TransitionConsequenceKind.MobilityLoss ||
      kind == TransitionConsequenceKind.LineUnlockGain ||
      kind == TransitionConsequenceKind.FileAccessGain ||
      kind == TransitionConsequenceKind.FileAccessLoss ||
      kind == TransitionConsequenceKind.OutpostGain ||
      kind == TransitionConsequenceKind.OutpostConcession ||
      kind == TransitionConsequenceKind.RookLiftActivation ||
      kind == TransitionConsequenceKind.BatteryPressureGain ||
      kind == TransitionConsequenceKind.PieceExchangeAvailable ||
      kind == TransitionConsequenceKind.PieceExchangeCompleted

  private def positionPlanTechniqueStructuralPolaritiesForAxis(polarity: StrategicAxisPolarity): Set[String] =
    polarity match
      case StrategicAxisPolarity.Gain | StrategicAxisPolarity.Support | StrategicAxisPolarity.Preserve |
          StrategicAxisPolarity.Restrain =>
        Set(StructuralSignalPolarity.Gain.toString, StructuralSignalPolarity.Neutral.toString)
      case StrategicAxisPolarity.Loss | StrategicAxisPolarity.Release | StrategicAxisPolarity.Concede =>
        Set(StructuralSignalPolarity.Loss.toString, StructuralSignalPolarity.Neutral.toString)

  private def positionPlanTechniqueMergeStructuralPurposes(
      purposes: List[PositionPlanTechniqueStructuralPurpose]
  ): Option[PositionPlanTechniqueStructuralPurpose] =
    Option.when(purposes.nonEmpty) {
      PositionPlanTechniqueStructuralPurpose(
        sourceIds = purposes.flatMap(_.sourceIds).distinct.sorted,
        routeMove = positionPlanTechniqueSingleValue(purposes.flatMap(_.routeMove)),
        transitionRouteSubject = positionPlanTechniqueSingleValue(purposes.flatMap(_.transitionRouteSubject)),
        routeRole = positionPlanTechniqueSingleValue(purposes.flatMap(_.routeRole)),
        routePerspective = positionPlanTechniqueSingleValue(purposes.flatMap(_.routePerspective)),
        routeFromPly = positionPlanTechniqueSingleInt(purposes.flatMap(_.routeFromPly)),
        routeToPly = positionPlanTechniqueSingleInt(purposes.flatMap(_.routeToPly)),
        consequenceKinds = purposes.flatMap(_.consequenceKinds).distinct,
        consequences = purposes.flatMap(_.consequences).distinct.sorted,
        subjects = purposes.flatMap(_.subjects).distinct.sorted,
        categories = purposes.flatMap(_.categories).distinct.sorted,
        polarities = purposes.flatMap(_.polarities).distinct.sorted,
        strength = Option.when(purposes.exists(_.strength.nonEmpty))(purposes.flatMap(_.strength).sum)
      )
    }

  private def positionPlanTechniqueStructuralPurposeCategories(
      kind: TransitionConsequenceKind
  ): List[String] =
    TransitionConsequenceCategory.values.toList
      .filter(category => StructuralDeltaEvidence.hasConsequenceCategory(kind, category))
      .map(_.toString)
      .sorted

  private def positionPlanTechniqueResourceContestForLineage(
      records: List[EvidenceRecord]
  ): Option[PositionPlanTechniqueResourceContest] =
    val anchors =
      records
        .flatMap {
          case EvidenceRecord(_, payload: BoardFactEvidence, _)     => payload.boardAnchors
          case EvidenceRecord(_, payload: StrategicFactEvidence, _) => payload.boardAnchors
          case _                                                    => Nil
        }
        .filter(positionPlanTechniqueResourceAnchor)
    positionPlanTechniqueResourceContestFromAnchors(anchors)

  private def positionPlanTechniqueResourceContestFromAnchors(
      anchors: List[BoardAnchor]
  ): Option[PositionPlanTechniqueResourceContest] =
    Option.when(anchors.nonEmpty) {
      PositionPlanTechniqueResourceContest(
        actorSide = positionPlanTechniqueSingleValue(anchors.map(anchor => positionPlanTechniqueColorKey(anchor.side))),
        targetSide =
          positionPlanTechniqueSingleValue(anchors.flatMap(_.detail.flatMap(_.subjectColor).map(positionPlanTechniqueColorKey))),
        kinds = anchors.map(_.kind.toString).distinct.sorted,
        signals = anchors.map(_.signal.toString).distinct.sorted,
        squares = anchors.flatMap(anchor => (anchor.targetHintSquares ++ anchor.focusSquares).map(_.key)).distinct.sorted,
        files = anchors.flatMap(_.detail.flatMap(_.file).map(_.key)).distinct.sorted,
        scopes = anchors.flatMap(positionPlanTechniqueResourceScopes).distinct.sorted,
        magnitude = anchors.map(_.magnitude).sorted.lastOption
      )
    }

  private def positionPlanTechniqueMergeResourceContests(
      contests: List[PositionPlanTechniqueResourceContest]
  ): Option[PositionPlanTechniqueResourceContest] =
    Option.when(contests.nonEmpty) {
      PositionPlanTechniqueResourceContest(
        actorSide = positionPlanTechniqueSingleValue(contests.flatMap(_.actorSide)),
        targetSide = positionPlanTechniqueSingleValue(contests.flatMap(_.targetSide)),
        kinds = contests.flatMap(_.kinds).distinct.sorted,
        signals = contests.flatMap(_.signals).distinct.sorted,
        squares = contests.flatMap(_.squares).distinct.sorted,
        files = contests.flatMap(_.files).distinct.sorted,
        scopes = contests.flatMap(_.scopes).distinct.sorted,
        magnitude = contests.flatMap(_.magnitude).sorted.lastOption
      )
    }

  private def positionPlanTechniqueResourceAnchor(anchor: BoardAnchor): Boolean =
    anchor.kind match
      case BoardAnchorKind.CounterplayRestraint =>
        positionPlanTechniqueConcreteResourceAnchor(anchor)
      case BoardAnchorKind.FileControl | BoardAnchorKind.WeakSquare | BoardAnchorKind.Outpost | BoardAnchorKind.BatteryPressure =>
        positionPlanTechniqueConcreteResourceAnchor(anchor)
      case _ =>
        false

  private def positionPlanTechniqueConcreteResourceAnchor(anchor: BoardAnchor): Boolean =
    anchor.detail.exists(detail =>
      detail.targetSquare.nonEmpty ||
        detail.file.nonEmpty ||
        detail.relatedSquares.nonEmpty ||
        detail.attackerSquare.nonEmpty ||
        detail.attackerSquares.nonEmpty
    )

  private def positionPlanTechniqueResourceContestApplies(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    (
      detail.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial &&
        (
          detail.axisKind.contains(StrategicAxisKind.Counterplay) &&
            detail.axisPolarity.contains(StrategicAxisPolarity.Restrain) ||
            detail.threatKind.nonEmpty
        )
    ) ||
      (
        detail.unit == PositionPlanTechniqueUnit.CounterplayRace &&
          (detail.threatKind.nonEmpty || positionPlanTechniqueCounterplayRaceProof(detail))
      )

  private def positionPlanTechniqueResourceContestMatchesDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      resource: PositionPlanTechniqueResourceContest
  ): Boolean =
    detail.axisKind match
      case Some(StrategicAxisKind.Counterplay) =>
        resource.kinds.contains(BoardAnchorKind.CounterplayRestraint.toString) ||
          resource.scopes.contains("counterplay")
      case None if detail.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial && detail.threatKind.nonEmpty =>
        resource.kinds.contains(BoardAnchorKind.CounterplayRestraint.toString) ||
          resource.scopes.contains("counterplay")
      case None if detail.unit == PositionPlanTechniqueUnit.CounterplayRace && detail.threatKind.nonEmpty =>
        resource.kinds.contains(BoardAnchorKind.CounterplayRestraint.toString) ||
          resource.scopes.contains("counterplay")
      case _ =>
        true

  private def positionPlanTechniqueThreatProjectionApplies(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.CounterplayRace ||
      detail.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial

  private def positionPlanTechniqueStructuralPurposeApplies(
      detail: PositionPlanTechniqueSemanticDetail,
      hasStructuralPurpose: Boolean
  ): Boolean =
    (detail.axisKey.nonEmpty && (
      detail.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute ||
        detail.unit == PositionPlanTechniqueUnit.StructuralTransformation ||
        detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute ||
        detail.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial ||
        (
          detail.unit == PositionPlanTechniqueUnit.CounterplayRace &&
            positionPlanTechniqueCounterplayRaceProof(detail)
        )
    )) ||
      (detail.axisKey.isEmpty &&
        (detail.unit == PositionPlanTechniqueUnit.StructuralTransformation ||
          detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute) &&
        (
          detail.mechanismKinds.exists(positionPlanTechniqueStructuralPurposeMechanism) ||
            hasStructuralPurpose ||
            detail.semanticAnchorKeys.exists(_.startsWith("StructuralDelta:"))
        )) ||
      (detail.unit == PositionPlanTechniqueUnit.PlanOptionSet &&
        (
          detail.referencePlanIds.nonEmpty ||
            detail.candidatePlanIds.nonEmpty ||
            detail.matchedPlanIds.nonEmpty ||
            detail.missingPlanIds.nonEmpty ||
            detail.semanticAnchorKeys.exists(_.startsWith("Plan"))
        ))

  private def positionPlanTechniqueStructuralPurposeMechanism(kind: StrategicMechanismKind): Boolean =
    kind match
      case StrategicMechanismKind.PawnStructure | StrategicMechanismKind.StructuralImprovement |
          StrategicMechanismKind.StrategicConcession | StrategicMechanismKind.PawnWeakness |
          StrategicMechanismKind.TargetPressure =>
        true
      case _ =>
        false

  private def positionPlanTechniqueCounterplayRaceProof(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.unit == PositionPlanTechniqueUnit.CounterplayRace &&
      (
        detail.raceLeadingLineRole.nonEmpty ||
          (detail.raceCandidateRootMove.nonEmpty && detail.raceReferenceRootMove.nonEmpty) ||
          (
            positionPlanTechniqueCounterplayPawnBreakRace(detail) &&
              positionPlanTechniquePawnBreakRaceLineEvidence(detail)
          )
      )

  private def positionPlanTechniqueCounterplayPawnBreakRace(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.breakFile.exists(_.trim.nonEmpty) &&
      detail.counterBreakFiles.exists(_.trim.nonEmpty) &&
      detail.semanticAnchorKeys.exists(_.startsWith("CounterplayRace:PawnBreak:"))

  private def positionPlanTechniquePawnBreakRaceLineEvidence(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.raceLeadingLineRole.nonEmpty ||
      (detail.raceCandidateRootMove.nonEmpty && detail.raceReferenceRootMove.nonEmpty)

  private def positionPlanTechniqueConcreteSubject(subject: String): Boolean =
    val normalized = subject.toLowerCase
    if normalized.contains("diagonal-denial") then positionPlanTechniqueStrategicRayRestrictionSubject(normalized)
    else
      normalized.matches(".*[a-h][1-8].*") ||
        normalized.contains("file") ||
        normalized.contains("diagonal")

  private def positionPlanTechniqueStrategicRayRestrictionSubject(subject: String): Boolean =
    val normalized = subject.toLowerCase
    normalized.matches(".*bishop:(g7|b7|g2|b2):diagonal-denial:blocked-by:[c-f][45]:locked-center:mobility-[0-9]+-to-[0-9]+.*")

  private def positionPlanTechniqueWithStructuralMotifs(
      detail: PositionPlanTechniqueSemanticDetail,
      graph: TypedEvidenceGraph
  ): PositionPlanTechniqueSemanticDetail =
    val motifTags = (
      positionPlanTechniqueStructuralMotifTags(detail) ++
        positionPlanTechniqueOwnedRouteMotifTags(detail, graph)
    ).distinct.sorted
    val axisAnchors = positionPlanTechniqueStructuralAxisAnchors(detail)
    if motifTags.isEmpty && axisAnchors.isEmpty then detail
    else
      detail.copy(
        semanticAnchorKeys = (detail.semanticAnchorKeys ++ axisAnchors).distinct.sorted,
        structuralMotifTags = (detail.structuralMotifTags ++ motifTags).distinct.sorted
      )

  private def positionPlanTechniqueOwnedRouteMotifTags(
      detail: PositionPlanTechniqueSemanticDetail,
      graph: TypedEvidenceGraph
  ): List[String] =
    if detail.unit != PositionPlanTechniqueUnit.PieceRerouteRoute then Nil
    else
      detail.structuralRouteMove.toList.flatMap { routeMove =>
        detail.sourceEvidenceIds
          .flatMap(graph.byId.get)
          .collect {
            case EvidenceRecord(_, motif: MoveMotifEvidence, _)
                if motif.isRootEvent && EvidenceRef.sameMove(motif.rootMove, routeMove) =>
              motif.motif
          }
          .collect {
            case _: Motif.RookBehindPassedPawn => "RookBehindPassedPawn"
            case _: Motif.Castling             => "Castling"
          }
      }.distinct.sorted

  private def positionPlanTechniqueStructuralAxisAnchors(
      detail: PositionPlanTechniqueSemanticDetail
  ): List[String] =
    detail.structuralPurposeSubjects.flatMap { subject =>
      StructuralPurposeSubject.parse(subject) match
        case Some(StructuralPurposeSubject.Battery(axis, _, _, _)) if axis.equalsIgnoreCase("diagonal") =>
          List("axis:Diagonal")
        case Some(StructuralPurposeSubject.PieceRestriction(_, _, _))
            if positionPlanTechniqueStrategicRayRestrictionSubject(subject) =>
          List("axis:Diagonal")
        case _ =>
          Nil
    }.distinct.sorted

  private def positionPlanTechniqueStructuralMotifTags(
      detail: PositionPlanTechniqueSemanticDetail
  ): List[String] =
    if detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute then
      positionPlanTechniquePieceRouteMotifTags(detail)
    else if detail.unit == PositionPlanTechniqueUnit.StructuralTransformation ||
        detail.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute
    then
      val anchorKeys = detail.semanticAnchorKeys.map(_.toLowerCase)
      val consequences = detail.structuralPurposeConsequences.map(_.toLowerCase)
      val subjects = detail.structuralPurposeSubjects.map(_.toLowerCase)
      val hasOpenCenterRouteContext =
        positionPlanTechniqueOpenCenterContext(detail) &&
          positionPlanTechniqueDevelopmentRouteDetail(detail)
      val hasIqpAnchor =
        positionPlanTechniqueIqpContext(detail)
      val hasStructuralTransition =
        anchorKeys.exists(_.startsWith("structuraldelta:")) ||
          consequences.nonEmpty ||
          subjects.nonEmpty
      val hasOpenSignal =
        anchorKeys.exists(key =>
          key.contains("lineunlock") ||
            key.contains("semiopenfileaccess") ||
            key.contains("openfileaccess")
        ) ||
          consequences.exists(_.contains("lineunlock")) ||
          hasOpenCenterRouteContext
      val hasSpaceSignal =
        anchorKeys.exists(key =>
          key.startsWith("strategicaxis:spacecenter") ||
            key.contains("centercontrol")
        ) ||
          detail.structuralPurposeCategories.exists(_.toLowerCase.contains("center")) ||
          hasOpenCenterRouteContext
      val hasCentralOpenSignal =
        hasOpenSignal && detail.structuralRouteMove.exists(positionPlanTechniqueCentralRouteMove)
      (
        Option.when(hasIqpAnchor)("iqp").toList ++
          Option.when(hasIqpAnchor && hasStructuralTransition)("isolated").toList ++
          Option.when(hasIqpAnchor && hasStructuralTransition)("transition").toList ++
          Option.when(hasOpenSignal)("open").toList ++
          Option.when(hasSpaceSignal || hasCentralOpenSignal)("space").toList
      ).distinct.sorted
    else Nil

  private def positionPlanTechniqueCentralRouteMove(move: String): Boolean =
    val normalized = move.toLowerCase
    normalized.matches("[de][1-8][de][1-8].*")

  private def positionPlanTechniquePieceRouteMotifTags(
      detail: PositionPlanTechniqueSemanticDetail
  ): List[String] =
    val anchorKeys = detail.semanticAnchorKeys.map(_.toLowerCase)
    val consequences = detail.structuralPurposeConsequences.map(_.toLowerCase)
    val categories = detail.structuralPurposeCategories.map(_.toLowerCase)
    val subjects = detail.structuralPurposeSubjects.map(_.toLowerCase)
    val hasPieceRouteSubject =
      subjects.exists(positionPlanTechniquePieceRouteSubject)
    val hasQualifiedRoute =
      subjects.exists(positionPlanTechniqueRouteSubject)
    val hasPieceActivity =
      hasPieceRouteSubject ||
      subjects.exists {
        case subject => subject.contains("piece") || subject.contains("mobility")
      } ||
        consequences.exists(consequence =>
          consequence.contains("developmentpieceactivated") ||
            consequence.contains("developmentmobilitygain") ||
          consequence.contains("mobilitygain")
        ) ||
        categories.exists(category => category.contains("pieceactivity") || category.contains("development")) ||
        anchorKeys.exists(key => key.contains("pieceactivation"))
    val hasOpenCenterRouteContext =
      positionPlanTechniqueOpenCenterContext(detail) &&
        positionPlanTechniqueDevelopmentRouteDetail(detail)
    val hasIqpContext =
      positionPlanTechniqueIqpContext(detail)
    val hasOutpost =
      consequences.exists(_.contains("outpost")) ||
        categories.exists(_.contains("outpost")) ||
        anchorKeys.exists(_.contains("outpost"))
    val hasBattery =
      subjects.exists(_.contains("battery:")) ||
        consequences.exists(_.contains("battery")) ||
        categories.exists(_.contains("battery")) ||
        anchorKeys.exists(_.contains("battery"))
    val hasDiagonal =
      subjects.exists(_.contains("diagonal")) ||
        consequences.exists(_.contains("diagonal")) ||
        categories.exists(_.contains("diagonal")) ||
        anchorKeys.exists(_.contains("diagonal"))
    val hasQualifiedRouteToken =
      subjects.exists(positionPlanTechniqueQualifiedRouteSubject) ||
        consequences.exists(positionPlanTechniqueQualifiedRouteSubject) ||
        categories.exists(positionPlanTechniqueQualifiedRouteSubject) ||
        anchorKeys.exists(positionPlanTechniqueQualifiedRouteSubject)
    val hasFileRoute =
      (subjects ++ consequences ++ categories ++ anchorKeys).exists(positionPlanTechniqueFileRouteToken)
    (
      Option.when(hasPieceActivity)("piece").toList ++
        Option.when(hasQualifiedRoute || hasQualifiedRouteToken)("route").toList ++
        Option.when((hasQualifiedRoute || hasQualifiedRouteToken) && detail.structuralRouteMove.nonEmpty)("reroute").toList ++
        Option.when(hasIqpContext)("iqp").toList ++
        Option.when(hasOpenCenterRouteContext)("open").toList ++
        Option.when(hasOpenCenterRouteContext)("space").toList ++
        Option.when(hasOutpost)("outpost").toList ++
        Option.when(hasBattery)("battery").toList ++
        Option.when(hasDiagonal)("diagonal").toList ++
        Option.when(hasFileRoute)("file")
    ).distinct.sorted

  private def positionPlanTechniqueFileRouteToken(token: String): Boolean =
    val normalized = token.toLowerCase
    normalized.contains("filecontrol") ||
      normalized.contains("file-control") ||
      normalized.contains("fileaccess") ||
      normalized.contains("file-access") ||
      normalized.contains("fileoccupation") ||
      normalized.contains("file-occupation")

  private def positionPlanTechniqueResourceScopes(anchor: BoardAnchor): List[String] =
    val base =
      anchor.kind match
        case BoardAnchorKind.CounterplayRestraint =>
          List("counterplay") ++ Option.when(anchor.signal == BoardAnchorSignal.OpponentLowMobility)("space").toList
        case BoardAnchorKind.Space                => List("space")
        case BoardAnchorKind.FileControl          => List("file")
        case BoardAnchorKind.Activity             => List("piece_activity")
        case BoardAnchorKind.CenterControl        => List("center")
        case BoardAnchorKind.Outpost              => List("outpost")
        case BoardAnchorKind.WeakSquare           => List("entry_square")
        case BoardAnchorKind.KingSafety           => List("king_safety")
        case BoardAnchorKind.PawnStructure        => List("structure")
        case _                                    => Nil
    val files =
      anchor.detail.toList.flatMap(detail =>
        detail.file.toList.map(_.key) ++
          detail.targetSquare.toList.map(_.key.take(1))
      )
    val sectors = files.distinct.flatMap {
      case "a" | "b" | "c" => List("queenside")
      case "d" | "e"       => List("center")
      case "f" | "g" | "h" => List("kingside")
      case _               => Nil
    }
    val sectorScopes =
      sectors.distinct match
        case sector :: Nil => List(sector)
        case _             => Nil
    (base ++ sectorScopes).distinct.sorted

  private def positionPlanTechniqueColorKey(color: chess.Color): String =
    if color.white then "white" else "black"

  private def positionPlanTechniqueSingleValue(values: List[String]): Option[String] =
    values.distinct.sorted match
      case value :: Nil => Some(value)
      case _            => None

  private def positionPlanTechniqueSingleInt(values: List[Int]): Option[Int] =
    values.distinct.sorted match
      case value :: Nil => Some(value)
      case _            => None

  private def positionPlanTechniqueSingleBoolean(values: List[Boolean]): Option[Boolean] =
    values.distinct match
      case value :: Nil => Some(value)
      case _            => None

  private def contrastPlanTechniqueDetails(
      payload: StrategicMechanismContrastEvidence,
      anchors: List[EvidenceSemanticAnchor],
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef]
  ): List[PositionPlanTechniqueSemanticDetail] =
    val anchorKeys = anchors.map(_.stableKey).distinct.sorted
    val pawnPlayBySourceId = positionPlanTechniquePawnPlayBySourceId(graph, refs)
    val openingPriorBySourceId = positionPlanTechniqueOpeningPriorBySourceId(graph, refs)
    val resourceContestBySourceId = positionPlanTechniqueResourceContestBySourceId(graph, refs)
    val structuralPurposeBySourceId = positionPlanTechniqueStructuralPurposeBySourceId(graph, refs)
    val threatBySourceId = positionPlanTechniqueThreatBySourceId(graph, refs)
    val axisDetails =
      payload.axisComparisons.flatMap(axisComparison =>
        positionPlanTechniqueUnits(axisComparison.axis).map(unit =>
          val sourceIds = axisComparison.sources.map(_.id).distinct.sorted
          val candidateSourceIds = axisComparison.candidateSources.map(_.id).distinct.sorted
          val raceLineContext =
            unit == PositionPlanTechniqueUnit.CounterplayRace ||
              axisComparison.axis.kind == StrategicAxisKind.PawnBreak
          val detail = PositionPlanTechniqueSemanticDetail(
            unit = unit,
            axisKey = Some(axisComparison.axisKey),
            axisKind = Some(axisComparison.axis.kind),
            axisPolarity = Some(axisComparison.axis.polarity),
            label = Some(axisComparison.axis.label),
            contrastOutcome = Some(axisComparison.outcome),
            referenceStrength = Some(axisComparison.referenceStrength),
            candidateStrength = Some(axisComparison.candidateStrength),
            narrativeHorizon = Some(payload.sustainability.horizon),
            raceLeadingLineRole =
              Option.when(raceLineContext)(
                positionPlanTechniqueRaceLeadingLineRole(axisComparison, payload)
              ).flatten,
            raceReferenceRootMove =
              Option.when(raceLineContext)(
                positionPlanTechniqueRootMove(payload.referenceLine)
              ).flatten,
            raceCandidateRootMove =
              Option.when(raceLineContext)(
                positionPlanTechniqueRootMove(payload.candidateLine)
              ).flatten,
            semanticAnchorKeys = anchorKeys,
            referenceEvidenceIds = axisComparison.referenceSources.map(_.id).distinct.sorted,
            candidateEvidenceIds = axisComparison.candidateSources.map(_.id).distinct.sorted,
            sourceEvidenceIds = sourceIds
          )
          detail
            .withPawnPlay(positionPlanTechniquePawnPlayForSources(sourceIds, pawnPlayBySourceId))
            .withOpeningPrior(positionPlanTechniqueOpeningPriorForSources(sourceIds, openingPriorBySourceId))
            .withResourceContest(positionPlanTechniqueResourceContestForSources(sourceIds, resourceContestBySourceId))
            .withStructuralPurpose(
              positionPlanTechniqueStructuralPurposeForDetailSources(
                detail,
                candidateSourceIds,
                structuralPurposeBySourceId
              )
            )
            .withThreatProjection(positionPlanTechniqueThreatForSources(sourceIds, threatBySourceId))
        )
      )
    val planDetails =
      payload.planComparison.toList.filter(_.hasPlanDelta).map(plan =>
        val sourceIds = payload.sourceRefs.map(_.id).distinct.sorted
        val detail = PositionPlanTechniqueSemanticDetail(
          unit = PositionPlanTechniqueUnit.PlanOptionSet,
          contrastOutcome = Some(plan.outcome),
          narrativeHorizon = Some(payload.sustainability.horizon),
          semanticAnchorKeys = anchorKeys,
          referencePlanIds = plan.referencePlanIds.distinct.sorted,
          candidatePlanIds = plan.candidatePlanIds.distinct.sorted,
          sourceEvidenceIds = sourceIds
        )
        detail
          .withResourceContest(positionPlanTechniqueResourceContestForSources(sourceIds, resourceContestBySourceId))
          .withStructuralPurpose(positionPlanTechniqueStructuralPurposeForDetailSources(detail, sourceIds, structuralPurposeBySourceId))
          .withThreatProjection(positionPlanTechniqueThreatForSources(sourceIds, threatBySourceId))
      )
    positionPlanTechniqueWithPawnBreakRaceDetails(axisDetails ++ planDetails)
      .distinctBy(detail =>
        (
          detail.unit,
          detail.axisKey,
          detail.referencePlanIds.mkString(","),
          detail.candidatePlanIds.mkString(",")
        )
      )
      .sortBy(detail => (detail.unit.toString, detail.axisKey.getOrElse("")))

  private def positionPlanTechniqueWithPawnBreakRaceDetails(
      details: List[PositionPlanTechniqueSemanticDetail]
  ): List[PositionPlanTechniqueSemanticDetail] =
    val enrichedDetails = details
    enrichedDetails ++ enrichedDetails.flatMap(positionPlanTechniquePawnBreakRaceDetail)

  private def positionPlanTechniquePawnBreakRaceDetail(
      detail: PositionPlanTechniqueSemanticDetail
  ): Option[PositionPlanTechniqueSemanticDetail] =
    Option.when(positionPlanTechniquePawnBreakRaceEligible(detail)) {
      val breakToken = positionPlanTechniqueRaceToken(detail.breakFile.get)
      val counterToken =
        detail.counterBreakFiles
          .map(positionPlanTechniqueRaceToken)
          .filter(token => token.nonEmpty && token != breakToken)
          .distinct
          .sorted
          .mkString("-")
      detail.copy(
        unit = PositionPlanTechniqueUnit.CounterplayRace,
        label = Some(s"counterplay-race-$breakToken-vs-$counterToken"),
        semanticAnchorKeys =
          (detail.semanticAnchorKeys :+ s"CounterplayRace:PawnBreak:$breakToken:$counterToken").distinct.sorted
      )
    }

  private def positionPlanTechniquePawnBreakRaceEligible(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    val breakToken = detail.breakFile.map(positionPlanTechniqueRaceToken)
    val distinctCounterBreak =
      breakToken.exists(token =>
        detail.counterBreakFiles
          .map(positionPlanTechniqueRaceToken)
          .exists(counterToken => counterToken.nonEmpty && counterToken != token)
      )
    detail.unit == PositionPlanTechniqueUnit.TensionBreakPolicyRoute &&
      detail.breakFile.exists(_.trim.nonEmpty) &&
      distinctCounterBreak &&
      positionPlanTechniquePawnBreakRaceLineEvidence(detail) &&
      (
        detail.pawnPlayDriver.exists(driver =>
          val normalized = driver.toLowerCase
          normalized.contains("breakready") ||
            normalized.contains("tensionactive") ||
            normalized.contains("tensioncritical")
        ) || positionPlanTechniquePawnBreakTransitionEvidence(detail)
      ) &&
      positionPlanTechniquePawnBreakTensionCarrier(detail)

  private def positionPlanTechniquePawnBreakTensionCarrier(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.tensionEdges.nonEmpty || detail.tensionSquares.nonEmpty

  private[judgment] def positionPlanTechniquePawnBreakTransitionEvidence(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.structuralConsequenceKinds.exists(positionPlanTechniquePawnTensionConsequence)

  private def positionPlanTechniqueRaceToken(raw: String): String =
    raw.trim.toLowerCase.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")

  private def positionPlanTechniqueRaceLeadingLineRole(
      axisComparison: StrategicAxisComparison,
      payload: StrategicMechanismContrastEvidence
  ): Option[LineNodeRole] =
    if axisComparison.candidateLead then Some(payload.candidateLine.role)
    else if axisComparison.referenceLead then Some(payload.referenceLine.role)
    else None

  private def positionPlanTechniqueRootMove(line: LineNodeRef): Option[String] =
    Some(line.rootMove).filter(_.nonEmpty)

  private def threatEpisodePlanTechniqueDetails(
      payload: ThreatEpisodeEvidence,
      units: List[PositionPlanTechniqueUnit],
      evidenceIds: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    val episode = payload.episode
    units.map(unit =>
      PositionPlanTechniqueSemanticDetail(
        unit = unit,
        label = Option.when(unit == PositionPlanTechniqueUnit.CounterplayRace)("dynamic-counterplay-race"),
        threatKind = Some(episode.kind.toString),
        threatDriver = Some(episode.driver.toString),
        threatSeverity = Some(episode.severity.toString),
        turnsToImpact = Some(episode.turnsToImpact),
        defenseMove = payload.onlyDefense.orElse(episode.bestDefense),
        prophylaxisNeeded = Some(payload.prophylaxisNeeded),
        maxWinPercentLossIfIgnored = payload.maxWinPercentLossIfIgnored,
        resourceContestSquares = episode.attackSquares.map(_.key).distinct.sorted,
        resourceContestScopes =
          Option.when(episode.attackSquares.nonEmpty)("threat").toList ++
            Option.when(payload.prophylaxisNeeded && episode.attackSquares.nonEmpty)("prophylaxis").toList,
        sourceEvidenceIds = evidenceIds
      )
    )

  private def endgameTechniquePlanDetails(
      anchors: List[BoardAnchor],
      evidenceIds: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    anchors
      .groupBy(_.semanticGroupingAnchor.stableKey)
      .values
      .toList
      .map { groupedAnchors =>
        val detailTags = groupedAnchors.flatMap(_.detail.toList.flatMap(_.tags)).distinct
        def tagValue(prefix: String): Option[String] =
          detailTags.collectFirst { case tag if tag.startsWith(prefix) => tag.stripPrefix(prefix) }
        PositionPlanTechniqueSemanticDetail(
          unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
          semanticAnchorKeys = groupedAnchors.map(_.semanticGroupingAnchor.stableKey).distinct.sorted,
          boardAnchorKinds = groupedAnchors.map(_.kind.toString).distinct.sorted,
          boardAnchorSignals = groupedAnchors.map(_.signal.toString).distinct.sorted,
          requiredSquares = groupedAnchors.flatMap(_.focusSquares.map(_.key)).distinct.sorted,
          endgameTechniquePattern = tagValue("pattern:"),
          endgameTechniqueRookPattern = tagValue("rook-pattern:"),
          endgameTechniqueSide = tagValue("technique-side:"),
          anchorMagnitude = groupedAnchors.map(_.magnitude).sorted.lastOption,
          sourceEvidenceIds = evidenceIds
        )
      }
      .sortBy(detail => detail.semanticAnchorKeys.mkString("\u0000"))

  private def lineEndgameTechniquePlanDetails(
      payload: LineFactEvidence,
      evidenceIds: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    payload.endgameTechniqueHorizons
      .map { horizon =>
        PositionPlanTechniqueSemanticDetail(
          unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
          semanticAnchorKeys = List(lineEndgameTechniqueSemanticAnchor(horizon).stableKey),
          endgameTechniquePattern = Some(horizon.pattern),
          endgameTechniqueRookPattern = horizon.rookPattern,
          endgameTechniqueSide = Some(horizon.techniqueSideKey),
          endgameTechniqueHorizonStatus = Some(horizon.status.toString),
          endgameTechniqueTriggerMove = horizon.triggerMove,
          endgameTechniqueEntryPlyOffset = Some(horizon.entryPlyOffset),
          endgameTechniqueTerminalPlyOffset = Some(horizon.terminalPlyOffset),
          endgameTechniqueLineObservation = lineEndgameTechniqueObservation(payload, horizon),
          endgameZugzwangProof = horizon.zugzwangProof,
          requiredSquares = horizon.requiredSquares.distinct.sorted,
          maintainedSquares = horizon.maintainedSquares.distinct.sorted,
          brokenSquares = horizon.brokenSquares.distinct.sorted,
          terminalConsequenceKinds = horizon.terminalConsequenceKinds.map(_.toString).distinct.sorted,
          endgameTechniqueFailureReason = horizon.failureReason,
          sourceEvidenceIds = evidenceIds
        )
      }
      .distinctBy(detail =>
        (
          detail.endgameTechniquePattern,
          detail.endgameTechniqueSide,
          detail.endgameTechniqueEntryPlyOffset,
          detail.endgameTechniqueTerminalPlyOffset
        )
      )
      .sortBy(detail =>
        (
          detail.endgameTechniquePattern.getOrElse(""),
          detail.endgameTechniqueSide.getOrElse(""),
          detail.endgameTechniqueEntryPlyOffset.getOrElse(-1)
        )
      )

  private def lineEndgameTechniqueObservation(
      payload: LineFactEvidence,
      horizon: LineEndgameTechniqueHorizon
  ): Option[EndgameTechniqueLineObservation] =
    val replay = payload.lineReplaySteps
    val entry = horizon.entryPlyOffset
    val terminal = horizon.terminalPlyOffset
    val rootMove = payload.rootMove.map(JudgmentSubjectBinding.normalizeMove)
    val triggerMove = horizon.triggerMove.map(JudgmentSubjectBinding.normalizeMove)
    if
      horizon.pattern != "Triangulation" ||
        horizon.status != LineEndgameTechniqueHorizonStatus.Completed ||
        entry != 0 || terminal != entry + 4
    then None
    else
      for
        root <- rootMove
        trigger <- triggerMove
        if root == trigger
        rootStep <- replay.lift(entry)
        if JudgmentSubjectBinding.normalizeMove(rootStep.moveUci) == root
        route = List(entry, entry + 2, terminal).flatMap(replay.lift).map(step => JudgmentSubjectBinding.normalizeMove(step.moveUci))
        if route.size == 3
      yield
        val reply = replay
          .lift(terminal + 1)
          .map(step => JudgmentSubjectBinding.normalizeMove(step.moveUci))
        val pawnOffset = terminal + 2
        val pawnMove = replay
          .lift(pawnOffset)
          .filter(lineStepMovesPawn(_, horizon.techniqueSide))
          .map(step => JudgmentSubjectBinding.normalizeMove(step.moveUci))
        val pawnReply = pawnMove.flatMap(_ => replay.lift(pawnOffset + 1).map(step => JudgmentSubjectBinding.normalizeMove(step.moveUci)))
        val newPassedPawnSquares = pawnMove.toList.flatMap(_ =>
          payload
            .lineEventsOf(LineEventKind.PassedPawn)
            .filter(event =>
              event.side.contains(horizon.techniqueSide) &&
                event.plyOffset >= pawnOffset &&
                event.plyOffset <= pawnOffset + 1
            )
            .flatMap(_.square.map(_.key))
        ).distinct.sorted
        EndgameTechniqueLineObservation(
          kingRouteMoves = route,
          tempoRecipient = if horizon.techniqueSide.white then "black" else "white",
          opponentReply = reply,
          followUpPawnMove = pawnMove,
          replyToPawnMove = pawnReply,
          newPassedPawnSquares = newPassedPawnSquares
        )

  private def lineStepMovesPawn(step: LineReplayStep, side: _root_.chess.Color): Boolean =
    val move = JudgmentSubjectBinding.normalizeMove(step.moveUci)
    (for
      position <- _root_.chess.format.Fen.read(
        _root_.chess.variant.Standard,
        _root_.chess.format.Fen.Full(step.fenBefore)
      )
      from <- _root_.chess.Square.fromKey(move.take(2))
      piece <- position.board.pieceAt(from)
    yield position.color == side && piece.color == side && piece.role == _root_.chess.Pawn).contains(true)

  private def lineTerminalProofPlanDetails(
      consequences: List[LineConsequence],
      evidenceIds: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    consequences
      .filter(lineTerminalProofConsequence)
      .flatMap { consequence =>
        consequence.rootMove.orElse(consequence.eventMove).map(_.trim.toLowerCase).map { routeMove =>
          val targetSquare =
            consequence.eventMove
              .flatMap(move => _root_.chess.Square.all.find(_.key == move.slice(2, 4)))
              .map(square => s"target:${square.key}")
          PositionPlanTechniqueSemanticDetail(
            unit = PositionPlanTechniqueUnit.StructuralTransformation,
            label = Some("terminal-proof"),
            semanticAnchorKeys = List(lineTerminalProofSemanticAnchor(consequence).stableKey),
            structuralRouteMove = Some(routeMove),
            structuralPurposeConsequences = List(consequence.kind.toString),
            structuralPurposeSubjects = targetSquare.toList,
            structuralPurposeCategories = List("TerminalProof"),
            structuralPurposePolarities = List(lineTerminalProofPolarity(consequence.kind, consequence.rootSide, consequence.beneficiary)),
            terminalConsequenceKinds = List(consequence.kind.toString),
            sourceEvidenceIds = evidenceIds
          )
        }
      }
      .distinctBy(detail => (detail.structuralRouteMove, detail.terminalConsequenceKinds.mkString(",")))
      .sortBy(detail => detail.structuralRouteMove.getOrElse(""))

  private def lineTerminalProofPolarity(
      kind: LineConsequenceKind,
      rootSide: Option[_root_.chess.Color] = None,
      beneficiary: Option[_root_.chess.Color] = None
  ): String =
    kind match
      case LineConsequenceKind.MaterialLoss => "Loss"
      case LineConsequenceKind.DrawResource => "Neutral"
      case _ if rootSide.zip(beneficiary).exists((root, winner) => root != winner) => "Loss"
      case _ if rootSide.zip(beneficiary).exists((root, winner) => root == winner) => "Gain"
      case _ => "Neutral"

  private def lineTerminalProofSemanticAnchor(
      consequence: LineConsequence
  ): EvidenceSemanticAnchor =
    EvidenceSemanticAnchor.of(
      EvidenceSemanticAnchorKind.LineConsequence,
      "TerminalProof",
      consequence.kind.toString,
      consequence.eventMove.getOrElse("")
    )

  private def lineEndgameTechniqueSemanticAnchor(
      horizon: LineEndgameTechniqueHorizon
  ): EvidenceSemanticAnchor =
    EvidenceSemanticAnchor.of(
      EvidenceSemanticAnchorKind.LineConsequence,
      (
        List(
          "EndgameTechniqueHorizon",
          s"pattern:${horizon.pattern}",
          s"horizonStatus:${horizon.status}",
          s"technique-side:${horizon.techniqueSideKey}"
        ) ++ horizon.rookPattern.map(pattern => s"rook-pattern:$pattern")
      )*
    )

  private def positionPlanTechniqueUnits(payload: StrategicMechanismEvidence): List[PositionPlanTechniqueUnit] =
    (
      positionPlanTechniqueUnits(payload.kind) ++
        payload.axisDetails.flatMap(positionPlanTechniqueUnits)
    ).distinct.sortBy(_.toString)

  private def positionPlanTechniqueUnits(kind: StrategicMechanismKind): List[PositionPlanTechniqueUnit] =
    kind match
      case StrategicMechanismKind.PlanPressure | StrategicMechanismKind.OpeningAlignment =>
        List(PositionPlanTechniqueUnit.PlanOptionSet)
      case StrategicMechanismKind.PawnStructure | StrategicMechanismKind.StructuralImprovement |
          StrategicMechanismKind.StrategicConcession | StrategicMechanismKind.PawnWeakness |
          StrategicMechanismKind.TargetPressure =>
        List(PositionPlanTechniqueUnit.StructuralTransformation)
      case StrategicMechanismKind.Endgame =>
        List(PositionPlanTechniqueUnit.EndgameTechniqueRecipe)
      case StrategicMechanismKind.Compensation =>
        List(PositionPlanTechniqueUnit.CompensationSource)
      case StrategicMechanismKind.Activity =>
        List(PositionPlanTechniqueUnit.PieceRerouteRoute)
      case StrategicMechanismKind.CenterControl =>
        List(PositionPlanTechniqueUnit.SpacePreventionResourceDenial)
      case StrategicMechanismKind.KingSafety =>
        List(PositionPlanTechniqueUnit.StructuralTransformation)

  private def positionPlanTechniqueUnits(axis: StrategicAxisDetail): List[PositionPlanTechniqueUnit] =
    axis.kind match
      case StrategicAxisKind.PawnBreak =>
        List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute)
      case StrategicAxisKind.PlanCoherence =>
        List(PositionPlanTechniqueUnit.PlanOptionSet)
      case StrategicAxisKind.Counterplay =>
        if axis.polarity == StrategicAxisPolarity.Restrain then List(PositionPlanTechniqueUnit.SpacePreventionResourceDenial)
        else List(PositionPlanTechniqueUnit.CounterplayRace)
      case StrategicAxisKind.Activity =>
        List(PositionPlanTechniqueUnit.PieceRerouteRoute)
      case StrategicAxisKind.SpaceCenter =>
        List(PositionPlanTechniqueUnit.SpacePreventionResourceDenial)
      case StrategicAxisKind.Target =>
        List(PositionPlanTechniqueUnit.StructuralTransformation)

  private def positionPlanTechniqueIdeaIds(ideas: List[ChessIdea], evidenceIds: Set[String]): List[String] =
    ideas
      .filter(idea => idea.evidence.exists(ref => evidenceIds.contains(ref.id)))
      .map(_.ref.id)
      .distinct
      .sorted

  private def positionPlanTechniqueClaimIds(
      claims: List[ClaimSeed],
      evidenceIds: Set[String],
      ideaIds: Set[String]
  ): List[String] =
    claims
      .filter(claim =>
        claim.evidence.exists(ref => evidenceIds.contains(ref.id)) ||
          claim.ideaRefs.exists(ref => ideaIds.contains(ref.id))
      )
      .map(_.id)
      .distinct
      .sorted

  private def positionPlanTechniqueRelation(
      ideaVerdict: Option[IdeaVerdictSplit],
      ideaIds: Set[String]
  ): Option[IdeaVerdictRelation] =
    ideaVerdict.toList
      .flatMap(_.bindings)
      .filter(binding => ideaIds.contains(binding.idea.id))
      .map(_.relation)
      .distinct
      .sortBy(_.toString)
      .headOption

  private def relativeCauseEvidenceIdsFor(
      graph: TypedEvidenceGraph,
      evidenceIds: Set[String],
      comparisonKind: Option[CandidateComparisonKind] = None,
      referenceLine: Option[LineNodeRef] = None,
      candidateLine: Option[LineNodeRef] = None,
      frameLine: Option[LineNodeRef],
      axisKeys: Set[String] = Set.empty
  ): List[String] =
    graph.records.collect {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), parents)
          if relativeCauseLinkedEvidenceIds(cause, parents).exists(evidenceIds.contains) &&
            comparisonKind.forall(_ == cause.comparisonKind) &&
            referenceLine.forall(_ == cause.referenceLine) &&
            candidateLine.forall(_ == cause.candidateLine) &&
            frameLine.forall(line => causeLineMatches(cause, line)) &&
            axisKeysMatch(cause, axisKeys) =>
        ref.id
    }.distinct.sorted

  private def relativeCauseLinkedEvidenceIds(cause: RelativeCauseFact, parents: List[EvidenceRef]): List[String] =
    (relativeCauseEvidenceIds(cause) ++ parents.map(_.id)).distinct.sorted

  private def causeLineMatches(cause: RelativeCauseFact, line: LineNodeRef): Boolean =
    cause.referenceLine == line ||
      cause.candidateLine == line ||
      cause.eventLine == line ||
      cause.evidenceLines.contains(line)

  private def axisKeysMatch(cause: RelativeCauseFact, axisKeys: Set[String]): Boolean =
    axisKeys.isEmpty ||
      cause.strategicProofIdentity.axisKeys.exists(axisKeys.contains)

  private def relativeCauseEvidenceIds(cause: RelativeCauseFact): List[String] =
    val tacticalSignalSourceIds =
      cause.proof.toList.flatMap(_.sections.flatMap(_.tacticalMechanisms.flatMap(_.signals.flatMap(_.source.map(_.id)))))
    (
      (cause.supportEvidence ++
        cause.attribution.ownedEvidence ++
        cause.attribution.contrastEvidence ++
        cause.attribution.contextEvidence ++
        cause.proof.toList.flatMap(proof =>
          proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs ++ proof.contextSupport.sourceRefs
        )).map(_.id) ++ cause.strategicProofIdentity.signalSourceIds ++ tacticalSignalSourceIds
    ).distinct.sorted
