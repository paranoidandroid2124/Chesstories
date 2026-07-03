package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.singlePosition.{ PawnPlayAnalysis, ThreatSeverity }
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
    maintainedSquares: List[String] = Nil,
    brokenSquares: List[String] = Nil,
    terminalConsequenceKinds: List[String] = Nil,
    endgameTechniqueFailureReason: Option[String] = None,
    anchorMagnitude: Option[Int] = None,
    sourceEvidenceIds: List[String] = Nil,
    causeEvidenceIds: List[String] = Nil,
    proofRoles: List[RelativeCauseProofRole] = Nil,
    contextCauseEvidenceIds: List[String] = Nil,
    contextProofRoles: List[RelativeCauseProofRole] = Nil,
    objectBindingSignatures: List[String] = Nil,
    specificityTier: PositionPlanTechniqueSpecificityTier = PositionPlanTechniqueSpecificityTier.ContextOnly
)

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
        case EvidenceRecord(ref, payload: ThreatEpisodeEvidence, parents) =>
          threatEpisodePlanTechniqueFrame(ref, payload, parents, graph, ideas, claims, ideaVerdict).toList
        case EvidenceRecord(ref, payload: BoardFactEvidence, _) if payload.endgameTechniqueAnchors.nonEmpty =>
          boardEndgameTechniqueFrame(ref, payload, graph, ideas, claims, ideaVerdict).toList
        case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
          lineEndgameTechniqueFrame(ref, payload, graph, ideas, claims, ideaVerdict).toList
        case _ =>
          Nil
      }
      .filter(_.units.nonEmpty)
      .distinctBy(_.id)
      .sortBy(frame => (-frame.salience, frame.id))

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
      val units = positionPlanTechniqueUnits(payload, anchors)
      val objectBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, refs)
      val evidenceIds = refs.map(_.id).distinct.sorted
      val candidateRelativeCauseEvidenceIds =
        relativeCauseEvidenceIdsFor(
          graph,
          evidenceIds.toSet,
          frameLine = ref.line,
          axisKeys = payload.axisDetails.map(_.stableKey).toSet
        )
      val semanticDetails =
        positionPlanTechniqueEnrichedDetails(
          mechanismPlanTechniqueDetails(payload, units, anchors, graph, refs),
          graph,
          (evidenceIds ++ candidateRelativeCauseEvidenceIds).distinct.sorted
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
    if !contrastCanProducePlanTechniqueFrame(payload, graph) then None
    else
      val refs = (ref :: parents ++ payload.sourceRefs).distinctBy(_.id)
      val anchors = semanticAnchorsFor(graph, record, refs)
      val units =
        (payload.axisComparisons.flatMap(axis => positionPlanTechniqueUnits(axis.axis)) ++
          payload.planComparison.toList.flatMap(plan => Option.when(plan.hasPlanDelta)(PositionPlanTechniqueUnit.PlanOptionSet))).distinct
          .sortBy(_.toString)
      val objectBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, refs)
      val evidenceIds = refs.map(_.id).distinct.sorted
      val candidateRelativeCauseEvidenceIds =
        relativeCauseEvidenceIdsFor(
          graph,
          evidenceIds.toSet,
          comparisonKind = Some(payload.comparisonKind),
          referenceLine = Some(payload.referenceLine),
          candidateLine = Some(payload.candidateLine),
          frameLine = Some(payload.candidateLine),
          axisKeys = payload.axisKeys.toSet
        )
      val semanticDetails =
        positionPlanTechniqueEnrichedDetails(
          contrastPlanTechniqueDetails(payload, anchors, graph, refs),
          graph,
          (evidenceIds ++ candidateRelativeCauseEvidenceIds).distinct.sorted
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

  private def contrastCanProducePlanTechniqueFrame(
      payload: StrategicMechanismContrastEvidence,
      graph: TypedEvidenceGraph
  ): Boolean =
    StrategicMechanismContrastEvidence.hasActionableContrastOrSameRootCarrier(
      contrastCandidateComparisonFact(payload, graph),
      payload,
      graph.records
    )

  private def contrastCandidateComparisonFact(
      payload: StrategicMechanismContrastEvidence,
      graph: TypedEvidenceGraph
  ): Option[CandidateComparisonFact] =
    graph.records.collectFirst {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == payload.comparisonKind &&
            fact.referenceLine == payload.referenceLine &&
            fact.candidateLine == payload.candidateLine =>
        fact
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
        moveUci = payload.onlyDefense.orElse(ref.line.map(_.rootMove)).filter(_.nonEmpty),
        scope = ref.scope,
        mechanismKinds = Nil,
        strategicAxisKeys = Nil,
        semanticAnchors = threatEpisodeSemanticAnchors(payload),
        objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
        objectBindings = positionPlanTechniqueObjectBindings(objectBindings),
        semanticDetails = positionPlanTechniqueEnrichedDetails(
          threatEpisodePlanTechniqueDetails(payload, units, enrichmentSourceIds)
            .map(_.withResourceContest(resourceContest)),
          graph,
          evidenceIds
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
          graph,
          (evidenceIds ++ relativeCauseEvidenceIds).distinct.sorted
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

  private def lineEndgameTechniqueFrame(
      ref: EvidenceRef,
      payload: LineFactEvidence,
      graph: TypedEvidenceGraph,
      ideas: List[ChessIdea],
      claims: List[ClaimSeed],
      ideaVerdict: Option[IdeaVerdictSplit]
  ): Option[PositionPlanTechniqueFrame] =
    val horizons = payload.endgameTechniqueHorizons
    val terminalConsequences = payload.proofSignalConsequences.filter(lineTerminalProofConsequence)
    Option.when(horizons.nonEmpty || terminalConsequences.nonEmpty) {
      val evidenceIds = List(ref.id)
      val objectBindings = EvidenceObjectBinding.fromEvidenceRefs(graph, List(ref))
      val relativeCauseEvidenceIds = relativeCauseEvidenceIdsFor(graph, evidenceIds.toSet, frameLine = ref.line)
      val linkedEvidenceIds = (evidenceIds ++ relativeCauseEvidenceIds).toSet
      val ideaIds = positionPlanTechniqueIdeaIds(ideas, linkedEvidenceIds)
      val claimIds = positionPlanTechniqueClaimIds(claims, linkedEvidenceIds, ideaIds.toSet)
      val semanticAnchors =
        (
          horizons.map(lineEndgameTechniqueSemanticAnchor) ++
            terminalConsequences.map(lineTerminalProofSemanticAnchor)
        ).distinctBy(_.stableKey).sortBy(_.stableKey)
      val units =
        (
          Option.when(horizons.nonEmpty)(PositionPlanTechniqueUnit.EndgameTechniqueRecipe).toList ++
            Option.when(terminalConsequences.nonEmpty)(PositionPlanTechniqueUnit.StructuralTransformation).toList
        ).distinct.sortBy(_.toString)
      PositionPlanTechniqueFrame(
        id = s"position-plan-technique:${ref.id}:endgame-horizon",
        units = units,
        position = ref.position,
        line = ref.line,
        moveUci = ref.line.map(_.rootMove).filter(_.nonEmpty),
        scope = ref.scope,
        mechanismKinds = List(StrategicMechanismKind.Endgame),
        strategicAxisKeys = Nil,
        semanticAnchors = semanticAnchors,
        objectBindingSignatures = EvidenceObjectBinding.objectSignatures(objectBindings),
        objectBindings = positionPlanTechniqueObjectBindings(objectBindings),
        semanticDetails = positionPlanTechniqueEnrichedDetails(
          lineEndgameTechniquePlanDetails(horizons, evidenceIds) ++
            lineTerminalProofPlanDetails(terminalConsequences, evidenceIds),
          graph,
          (evidenceIds ++ relativeCauseEvidenceIds).distinct.sorted
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
        salience = horizons.size * 2 + terminalConsequences.size * 3 + ideaIds.size + claimIds.size + relativeCauseEvidenceIds.size
      )
    }

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
      objectBindingSignatures: List[String],
      specificityTier: PositionPlanTechniqueSpecificityTier
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

  private def positionPlanTechniqueEnrichedDetails(
      details: List[PositionPlanTechniqueSemanticDetail],
      graph: TypedEvidenceGraph,
      fallbackEvidenceIds: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    val rawDetails =
      (
        details ++
          details.flatMap(positionPlanTechniqueMaterialCompensationDetails(_, graph, fallbackEvidenceIds))
      ).distinctBy(detail => (detail.unit, detail.axisKey, detail.label, detail.sourceEvidenceIds.mkString(",")))
    val fallbackRefs = fallbackEvidenceIds.flatMap(id => graph.byId.get(id).map(_.ref))
    val fallbackAnchorKeys =
      fallbackRefs
        .flatMap(ref => graph.byId.get(ref.id))
        .flatMap(StrategicMechanismEvidence.sourceSemanticAnchors)
        .map(_.stableKey)
        .distinct
        .sorted
    val fallbackStructureRouteContext =
      positionPlanTechniqueStructureRouteContextKeys(fallbackAnchorKeys, Nil)
    val fallbackStructureRouteSourceIds =
      positionPlanTechniqueStructureRouteContextSourceIds(graph, fallbackRefs)
    val structuralPurposeBySourceId = positionPlanTechniqueStructuralPurposeBySourceId(graph, fallbackRefs)
    val fallbackStructuralPurpose =
      positionPlanTechniqueStructuralPurposeForSources(fallbackEvidenceIds, structuralPurposeBySourceId)
    rawDetails.map { detail =>
      val routedDetail =
        positionPlanTechniqueWithFallbackOpenCenterRoute(
          detail,
          fallbackStructuralPurpose,
          fallbackStructureRouteContext,
          fallbackStructureRouteSourceIds,
          fallbackAnchorKeys
        )
      val taggedDetail = positionPlanTechniqueWithStructuralMotifs(routedDetail)
      val causeLinkage = positionPlanTechniqueDetailCauseLinkage(taggedDetail, graph, fallbackEvidenceIds)
      taggedDetail.copy(
        causeEvidenceIds = causeLinkage.causeEvidenceIds,
        proofRoles = causeLinkage.proofRoles,
        contextCauseEvidenceIds = causeLinkage.contextCauseEvidenceIds,
        contextProofRoles = causeLinkage.contextProofRoles,
        objectBindingSignatures = causeLinkage.objectBindingSignatures,
        specificityTier = causeLinkage.specificityTier
      )
    }

  private def positionPlanTechniqueMaterialCompensationDetails(
      detail: PositionPlanTechniqueSemanticDetail,
      graph: TypedEvidenceGraph,
      fallbackEvidenceIds: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    if detail.unit == PositionPlanTechniqueUnit.CompensationSource ||
      !positionPlanTechniqueConcreteCompensationCarrier(detail)
    then Nil
    else
      val localEvidenceIds =
        (
          detail.sourceEvidenceIds ++
            detail.referenceEvidenceIds ++
            detail.candidateEvidenceIds
        ).distinct.sorted
      val evidenceIds =
        if localEvidenceIds.nonEmpty then localEvidenceIds else fallbackEvidenceIds.distinct.sorted
      val sacrificeEvidenceIds =
        evidenceIds.filter(id =>
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
            sourceEvidenceIds = sacrificeEvidenceIds,
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
          (
            detail.label.exists(label =>
              val normalized = label.toLowerCase
              normalized.contains("target-pressure") ||
                normalized.contains("break-file") ||
                normalized.contains("line-unlock") ||
                normalized.contains("mobility")
            ) ||
              detail.structuralPurposeConsequences.exists(consequence =>
                val normalized = consequence.toLowerCase
                normalized.contains("targetpressure") ||
                  normalized.contains("lineunlock") ||
                  normalized.contains("mobility")
              )
          )
      )

  private def positionPlanTechniqueMaterialSacrificeSignature(signature: String): Boolean =
    signature.toLowerCase.contains("target=plansubject:material-sacrifice:")

  private def positionPlanTechniqueDetailCauseLinkage(
      detail: PositionPlanTechniqueSemanticDetail,
      graph: TypedEvidenceGraph,
      fallbackEvidenceIds: List[String]
  ): PositionPlanTechniqueDetailCauseLinkage =
    val localEvidenceIds =
      (
        detail.sourceEvidenceIds ++
          detail.referenceEvidenceIds ++
          detail.candidateEvidenceIds
      ).distinct.sorted
    val evidenceIds =
      if localEvidenceIds.nonEmpty then localEvidenceIds else fallbackEvidenceIds.distinct.sorted
    val evidenceIdSet = evidenceIds.toSet
    val fallbackCauseRecords =
      fallbackEvidenceIds.flatMap(id =>
        graph.byId.get(id).collect { case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) =>
          ref -> cause
        }
      )
    val linkedCauseRecords = positionPlanTechniqueRelativeCauseRecordsFor(graph, evidenceIdSet)
    val localCauseRecords = positionPlanTechniqueCauseRecordsForDetail(detail, linkedCauseRecords, evidenceIdSet)
    val exactAxisFallbackCauseRecords =
      positionPlanTechniqueExactAxisFallbackCauseRecords(detail, fallbackCauseRecords)
    val causeRecords =
      if positionPlanTechniqueTerminalOverriddenEndgameTechnique(detail) then Nil
      else (localCauseRecords ++ exactAxisFallbackCauseRecords).distinctBy(_._1.id)
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
    val rawDetailBindings =
      if localCauseBindings.nonEmpty then localCauseBindings
      else
        EvidenceObjectBinding.fromEvidenceRefs(
          graph,
          evidenceIds.flatMap(id => graph.byId.get(id).map(_.ref))
        )
    val detailBindings =
      if detail.unit == PositionPlanTechniqueUnit.CompensationSource then
        rawDetailBindings.filter(binding =>
          EvidenceObjectBinding
            .objectSignatures(List(binding))
            .exists(positionPlanTechniqueMaterialSacrificeSignature)
        )
      else rawDetailBindings
    val objectBindingSignatures =
      (
        EvidenceObjectBinding.objectSignatures(detailBindings) ++
          positionPlanTechniqueRouteObjectSignatures(detail) ++
          positionPlanTechniquePawnBreakObjectSignatures(detail)
      ).distinct.sorted
    val proofRoles =
      (
        localCauseBindings.flatMap(_.proofRole) ++
          causeRecords.flatMap { case (_, cause) =>
            positionPlanTechniqueProofRolesForEvidence(cause, evidenceIdSet)
              .filter(positionPlanTechniqueAdmissibleDetailProofRole)
          } ++
          exactAxisFallbackCauseRecords.flatMap { case (_, cause) =>
            positionPlanTechniqueAdmissibleProofRoles(cause)
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
      objectBindingSignatures = objectBindingSignatures,
      specificityTier = positionPlanTechniqueSpecificityTier(detail, causeRecords.map(_._2), objectBindingSignatures, proofRoles)
    )

  private def positionPlanTechniqueTerminalOverriddenEndgameTechnique(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.EndgameTechniqueRecipe &&
      detail.endgameTechniqueHorizonStatus.exists(status =>
        status == "SupersededByTactic" || status == "ContradictedByTerminalProof"
      )

  private def positionPlanTechniqueRouteObjectSignatures(
      detail: PositionPlanTechniqueSemanticDetail
  ): List[String] =
    if detail.unit != PositionPlanTechniqueUnit.PieceRerouteRoute &&
        detail.unit != PositionPlanTechniqueUnit.PlanOptionSet &&
        detail.unit != PositionPlanTechniqueUnit.StructuralTransformation
    then Nil
    else
      detail.structuralPurposeSubjects.flatMap { subject =>
        StructuralPurposeSubject.parse(subject) match
          case Some(StructuralPurposeSubject.PieceRoute(piece, from, to)) =>
            val longDiagonal =
              positionPlanTechniqueLongDiagonalBishopRoute(piece, from, to)
            List(
              (
                detail.structuralRouteMove.map(move => s"actor=Move:${move.toLowerCase}") ::
                  List(
                    Some(s"actor=Piece:${piece.toLowerCase}"),
                    Some(s"actor=Square:${from.toLowerCase}"),
                    Some(s"target=Square:${to.toLowerCase}"),
                    Some("mechanism=Mechanism:developmentchoice"),
                    Option.when(longDiagonal)("mechanism=Mechanism:bishop-long-diagonal"),
                    Some("consequence=Consequence:developmentpieceactivated"),
                    Option.when(longDiagonal)("consequence=Consequence:diagonalpressure")
                  )
              ).flatten.mkString("|")
            )
          case Some(StructuralPurposeSubject.Outpost(piece, square)) =>
            List(
              (
                detail.structuralRouteMove.map(move => s"actor=Move:${move.toLowerCase}") ::
                  List(
                    Some(s"actor=Piece:${piece.toLowerCase}"),
                    Some(s"target=Square:${square.toLowerCase}"),
                    Some("mechanism=Mechanism:outpost"),
                    Some("consequence=Consequence:outpost")
                  )
              ).flatten.mkString("|")
            )
          case Some(StructuralPurposeSubject.Battery(axis, from, to, roles)) =>
            val roleActors = roles.map(role => Some(s"actor=Piece:${role.toLowerCase}"))
            List(
              (
                detail.structuralRouteMove.map(move => s"actor=Move:${move.toLowerCase}") ::
                  (roleActors ++ List(
                    Some(s"target=Square:${from.toLowerCase}"),
                    Some(s"target=Square:${to.toLowerCase}"),
                    Some(s"mechanism=Mechanism:battery-${axis.toLowerCase}"),
                    Some("consequence=Consequence:batteryline")
                  ))
              ).flatten.mkString("|")
            )
          case Some(StructuralPurposeSubject.PieceRestriction(piece, square, blocker))
              if positionPlanTechniqueStrategicRayRestrictionSubject(subject) =>
            List(
              (
                detail.structuralRouteMove.map(move => s"actor=Move:${move.toLowerCase}") ::
                  List(
                    Some(s"actor=Piece:${piece.toLowerCase}"),
                    Some(s"target=Square:${square.toLowerCase}"),
                    Some(s"target=Square:${blocker.toLowerCase}"),
                    Some("mechanism=Mechanism:diagonal-denial"),
                    Some("consequence=Consequence:mobilityloss")
                  )
              ).flatten.mkString("|")
            )
          case Some(StructuralPurposeSubject.PieceSquare(piece, square)) if positionPlanTechniqueLineUnlockRouteSubject(subject) =>
            List(
              (
                detail.structuralRouteMove.map(move => s"actor=Move:${move.toLowerCase}") ::
                  List(
                    Some(s"actor=Piece:${piece.toLowerCase}"),
                    Some(s"actor=Square:${square.toLowerCase}"),
                    Some(s"target=Square:${square.toLowerCase}"),
                    Some("mechanism=Mechanism:line-unlock"),
                    Some("consequence=Consequence:lineunlockgain")
                  )
              ).flatten.mkString("|")
            )
          case _ =>
            Nil
      }.distinct.sorted

  private def positionPlanTechniqueLongDiagonalBishopRoute(piece: String, from: String, to: String): Boolean =
    piece.equalsIgnoreCase("bishop") &&
      positionPlanTechniqueDiagonalDistance(from, to).exists(_ >= 3) &&
      positionPlanTechniqueMainLongDiagonal(from) &&
      positionPlanTechniqueMainLongDiagonal(to)

  private def positionPlanTechniqueDiagonalDistance(from: String, to: String): Option[Int] =
    for
      fromFile <- from.headOption.map(_.toLower - 'a')
      fromRank <- from.drop(1).headOption.map(_ - '1')
      toFile <- to.headOption.map(_.toLower - 'a')
      toRank <- to.drop(1).headOption.map(_ - '1')
      fileDistance = (toFile - fromFile).abs
      rankDistance = (toRank - fromRank).abs
      if fileDistance == rankDistance && fileDistance > 0
    yield fileDistance

  private def positionPlanTechniqueMainLongDiagonal(square: String): Boolean =
    (
      for
        file <- square.headOption.map(_.toLower - 'a')
        rank <- square.drop(1).headOption.map(_ - '1')
      yield file == rank || file + rank == 7
    ).contains(true)

  private def positionPlanTechniquePawnBreakObjectSignatures(
      detail: PositionPlanTechniqueSemanticDetail
  ): List[String] =
    if detail.unit != PositionPlanTechniqueUnit.TensionBreakPolicyRoute &&
        detail.unit != PositionPlanTechniqueUnit.CounterplayRace
    then Nil
    else
      val targets =
        (
          detail.breakFile.map(file => s"target=File:${positionPlanTechniqueRaceToken(file)}").toList ++
            detail.counterBreakFiles.map(file => s"target=File:${positionPlanTechniqueRaceToken(file)}") ++
            detail.tensionSquares.map(square => s"target=Square:${positionPlanTechniqueRaceToken(square)}")
        ).filterNot(_.endsWith(":")).distinct.sorted
      val mechanisms =
        List(
          Some("mechanism=Mechanism:pawnbreak"),
          Option.when(detail.unit == PositionPlanTechniqueUnit.CounterplayRace)("mechanism=Mechanism:counterplayrace")
        ).flatten
      val consequences =
        List(
          detail.breakFile.map(file => s"consequence=Consequence:break-file-${positionPlanTechniqueRaceToken(file)}"),
          Option.when(detail.counterBreakFiles.nonEmpty)("consequence=Consequence:counterbreak-race")
        ).flatten
      val signature = (targets ++ mechanisms ++ consequences).mkString("|")
      Option.when(targets.nonEmpty)(signature).toList

  private def positionPlanTechniqueExactAxisFallbackCauseRecords(
      detail: PositionPlanTechniqueSemanticDetail,
      causeRecords: List[(EvidenceRef, RelativeCauseFact)]
  ): List[(EvidenceRef, RelativeCauseFact)] =
    detail.axisKey match
      case Some(axisKey) =>
        causeRecords.filter { case (_, cause) =>
          cause.strategicProofIdentity.axisKeys.contains(axisKey) &&
            positionPlanTechniqueCauseKindMatchesDetail(detail, cause.kind) &&
            positionPlanTechniqueAdmissibleProofRoles(cause).nonEmpty
        }
      case None =>
        Nil

  private def positionPlanTechniqueCauseRecordsForDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      causeRecords: List[(EvidenceRef, RelativeCauseFact)],
      evidenceIds: Set[String]
  ): List[(EvidenceRef, RelativeCauseFact)] =
    val axisMatched = detail.axisKey match
      case Some(axisKey) =>
        causeRecords.filter { case (_, cause) =>
          cause.strategicProofIdentity.axisKeys.contains(axisKey) &&
            positionPlanTechniqueCauseKindMatchesDetail(detail, cause.kind) &&
            positionPlanTechniqueHasAdmissibleProofForEvidence(cause, evidenceIds)
        }
      case None =>
        causeRecords.filter { case (_, cause) =>
          positionPlanTechniqueCauseKindMatchesUnitOnlyDetail(detail, cause.kind) &&
            positionPlanTechniqueHasAdmissibleProofForEvidence(cause, evidenceIds)
        }
    (
      axisMatched ++
        positionPlanTechniqueRootOwnedStructuralCauseRecords(detail, causeRecords, evidenceIds)
    ).distinctBy(_._1.id)

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
      kind: RelativeCauseKind
  ): Boolean =
    detail.axisKind.exists(axisKind =>
      !(detail.unit == PositionPlanTechniqueUnit.CounterplayRace && axisKind == StrategicAxisKind.PawnBreak) &&
        positionPlanTechniqueCauseKindsForAxis(axisKind, detail.axisPolarity, detail.label).contains(kind)
    ) ||
      positionPlanTechniqueConcreteCounterplayRaceCauseKind(detail, kind) ||
      positionPlanTechniqueConcreteRoutePlanCauseKind(detail, kind) ||
      positionPlanTechniqueConcreteStructuralPlanCauseKind(detail, kind) ||
      positionPlanTechniqueTerminalProofCauseKind(detail, kind)

  private def positionPlanTechniqueCauseKindMatchesUnitOnlyDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      kind: RelativeCauseKind
  ): Boolean =
    detail.axisKey.isEmpty &&
      positionPlanTechniqueCauseKindMatchesStructuralSourceDetail(detail, kind)

  private def positionPlanTechniqueRootOwnedStructuralCauseRecords(
      detail: PositionPlanTechniqueSemanticDetail,
      causeRecords: List[(EvidenceRef, RelativeCauseFact)],
      evidenceIds: Set[String]
  ): List[(EvidenceRef, RelativeCauseFact)] =
    if !positionPlanTechniqueRootOwnedStructuralDetail(detail) then Nil
    else
      causeRecords.filter { case (_, cause) =>
        cause.attribution.directProofEligible &&
          cause.attribution.rootMoveMatched &&
          detail.structuralRouteMove.exists(move => positionPlanTechniqueSameMove(move, cause.eventRootMove)) &&
          positionPlanTechniqueCauseKindMatchesStructuralSourceDetail(detail, cause.kind) &&
          positionPlanTechniqueDirectStructuralTransitionProof(detail, cause, evidenceIds)
      }

  private def positionPlanTechniqueRootOwnedStructuralDetail(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.unit match
      case PositionPlanTechniqueUnit.PieceRerouteRoute =>
        positionPlanTechniqueConcretePieceRoute(detail)
      case PositionPlanTechniqueUnit.StructuralTransformation =>
        positionPlanTechniqueConcreteStructuralTransformation(detail)
      case PositionPlanTechniqueUnit.CounterplayRace =>
        positionPlanTechniqueCounterplayPawnBreakRace(detail)
      case _ =>
        false

  private def positionPlanTechniqueCauseKindMatchesStructuralSourceDetail(
      detail: PositionPlanTechniqueSemanticDetail,
      kind: RelativeCauseKind
  ): Boolean =
    positionPlanTechniqueCauseKindsForUnit(detail.unit).contains(kind) ||
      positionPlanTechniqueConcreteCounterplayRaceCauseKind(detail, kind) ||
      positionPlanTechniqueConcreteRoutePlanCauseKind(detail, kind) ||
      positionPlanTechniqueConcreteStructuralPlanCauseKind(detail, kind) ||
      positionPlanTechniqueTerminalProofCauseKind(detail, kind)

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

  private def positionPlanTechniqueConcreteRoutePlanCauseKind(
      detail: PositionPlanTechniqueSemanticDetail,
      kind: RelativeCauseKind
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute &&
      detail.axisKind.contains(StrategicAxisKind.Activity) &&
      positionPlanTechniqueConcretePieceRoute(detail) &&
      Set(RelativeCauseKind.PlanImprovement, RelativeCauseKind.PlanContradiction).contains(kind)

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
          .when(positionPlanTechniqueOpenCenterContext(detail) && positionPlanTechniqueDevelopmentRouteDetail(detail))(
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

  private def positionPlanTechniqueTerminalProofCauseKind(
      detail: PositionPlanTechniqueSemanticDetail,
      kind: RelativeCauseKind
  ): Boolean =
    detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
      detail.terminalConsequenceKinds.exists(positionPlanTechniqueTerminalProofConsequenceKind) &&
      detail.structuralRouteMove.nonEmpty &&
      detail.terminalConsequenceKinds.exists(positionPlanTechniqueTerminalProofCauseKind(_, kind))

  private def positionPlanTechniqueTerminalProofConsequenceKind(kind: String): Boolean =
    LineConsequenceKind.terminalResultProofName(kind)

  private def positionPlanTechniqueTerminalProofCauseKind(
      consequenceKind: String,
      causeKind: RelativeCauseKind
  ): Boolean =
    consequenceKind match
      case "Mate" =>
        Set(
          RelativeCauseKind.KingForcing,
          RelativeCauseKind.TacticalRefutationOfPlayed,
          RelativeCauseKind.MissedTacticalResource,
          RelativeCauseKind.CandidateTacticalLiability
        ).contains(causeKind)
      case "MaterialGain" | "MaterialLoss" =>
        Set(RelativeCauseKind.MaterialSwing, RelativeCauseKind.SacrificeCompensation).contains(causeKind)
      case "PromotionRace" =>
        Set(RelativeCauseKind.ConversionMiss, RelativeCauseKind.ConversionSecured).contains(causeKind)
      case "DrawResource" =>
        Set(RelativeCauseKind.DrawResource, RelativeCauseKind.DefensiveResource).contains(causeKind)
      case _ =>
        false

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
      label: Option[String]
  ): Set[RelativeCauseKind] =
    val labelLower = label.fold("")(_.toLowerCase)
    axisKind match
      case StrategicAxisKind.Target =>
        Set(
          RelativeCauseKind.TargetPressureGain,
          RelativeCauseKind.TargetPressureRelease
        ) ++ Option.when(labelLower.contains("weak-pawn") || labelLower.contains("weak-square"))(
          RelativeCauseKind.PawnWeaknessTarget
        )
      case StrategicAxisKind.SpaceCenter =>
        Set(RelativeCauseKind.CenterControlGain)
      case StrategicAxisKind.PawnBreak =>
        Set(RelativeCauseKind.PawnBreakOpportunity)
      case StrategicAxisKind.Counterplay =>
        Set(
          Option.when(polarity.contains(StrategicAxisPolarity.Restrain))(RelativeCauseKind.OpponentRestriction),
          Option.when(label.exists(_.contains("king-safety")))(RelativeCauseKind.KingSafetyConcession)
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

  private def positionPlanTechniqueAdmissibleProofRoles(
      cause: RelativeCauseFact
  ): List[RelativeCauseProofRole] =
    cause.proof.toList
      .flatMap(_.sections)
      .map(_.role)
      .filter(positionPlanTechniqueAdmissibleDetailProofRole)
      .distinct
      .sortBy(_.toString)

  private def positionPlanTechniqueProofSectionEvidenceIds(section: RelativeCauseProofSection): List[String] =
    (
      section.sourceRefs.map(_.id) ++
        section.strategicMechanisms.flatMap(_.signals.map(_.source.id)) ++
        section.strategicMechanismContrasts.flatMap(_.axisComparisons.flatMap(_.sources.map(_.id)))
    ).distinct.sorted

  private def positionPlanTechniqueSpecificityTier(
      detail: PositionPlanTechniqueSemanticDetail,
      causes: List[RelativeCauseFact],
      objectBindingSignatures: List[String],
      proofRoles: List[RelativeCauseProofRole]
  ): PositionPlanTechniqueSpecificityTier =
    val exactAxis =
      detail.axisKey.exists(axis => causes.exists(_.strategicProofIdentity.axisKeys.contains(axis)))
    val nonBroadObject =
      objectBindingSignatures.exists(positionPlanTechniqueNonBroadObjectSignature)
    val admissibleProofRole =
      proofRoles.exists(role => role == RelativeCauseProofRole.DirectProof || role == RelativeCauseProofRole.ContrastProof)
    if exactAxis && nonBroadObject && admissibleProofRole then PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    else if nonBroadObject && admissibleProofRole then PositionPlanTechniqueSpecificityTier.ConcreteObjectAxis
    else if detail.axisKey.nonEmpty || objectBindingSignatures.nonEmpty || detail.semanticAnchorKeys.nonEmpty then
      PositionPlanTechniqueSpecificityTier.BroadAxis
    else PositionPlanTechniqueSpecificityTier.ContextOnly

  private def positionPlanTechniqueNonBroadObjectSignature(signature: String): Boolean =
    EvidenceObjectBinding.playerFacingReadySignatures(List(signature))

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
            .withThreatProjection(positionPlanTechniqueThreatForSources(sourceIds, threatBySourceId))
        )
      )
    val unitOnlyDetails =
      units
        .filterNot(unit => axisDetails.exists(_.unit == unit))
        .map(unit =>
          val sourceIds = payload.signals.map(_.source.id).distinct.sorted
          val detailSourceIds = positionPlanTechniqueExpandedSourceIds(graph, sourceIds)
          PositionPlanTechniqueSemanticDetail(
            unit = unit,
            mechanismKinds = List(payload.kind),
            semanticAnchorKeys = anchorKeys,
            sourceEvidenceIds = detailSourceIds
          )
            .withPawnPlay(positionPlanTechniquePawnPlayForSources(sourceIds, pawnPlayBySourceId))
            .withPlanAlignment(positionPlanTechniquePlanAlignmentForSources(sourceIds, planAlignmentBySourceId))
            .withOpeningPrior(positionPlanTechniqueOpeningPriorForSources(sourceIds, openingPriorBySourceId))
            .withResourceContest(positionPlanTechniqueResourceContestForSources(sourceIds, resourceContestBySourceId))
            .withStructuralPurpose(positionPlanTechniqueStructuralPurposeForSources(sourceIds, structuralPurposeBySourceId))
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
      if positionPlanTechniqueStructuralPurposeApplies(detail) then
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

  private def positionPlanTechniqueWithFallbackOpenCenterRoute(
      detail: PositionPlanTechniqueSemanticDetail,
      fallbackPurpose: Option[PositionPlanTechniqueStructuralPurpose],
      fallbackStructureRouteContext: Boolean,
      fallbackStructureRouteSourceIds: List[String],
      fallbackAnchorKeys: List[String]
  ): PositionPlanTechniqueSemanticDetail =
    val withPurpose = fallbackPurpose
      .filter(purpose =>
        detail.structuralRouteMove.isEmpty &&
          (positionPlanTechniqueStructureRouteContext(detail) || fallbackStructureRouteContext) &&
          positionPlanTechniqueDevelopmentRoutePurpose(purpose)
      )
      .fold(detail)(purpose => detail.withStructuralPurpose(Some(purpose)))
    if (fallbackStructureRouteContext || positionPlanTechniqueStructureRouteContext(withPurpose)) &&
        positionPlanTechniqueDevelopmentRouteDetail(withPurpose)
    then
      withPurpose.copy(
        sourceEvidenceIds = (withPurpose.sourceEvidenceIds ++ fallbackStructureRouteSourceIds).distinct.sorted,
        semanticAnchorKeys = (withPurpose.semanticAnchorKeys ++ fallbackAnchorKeys).distinct.sorted
      )
    else withPurpose

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
          purpose.categories.exists(positionPlanTechniqueDevelopmentRouteCategory)
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
    val kindMatches =
      detail.axisKind match
        case Some(StrategicAxisKind.Counterplay)
            if detail.axisPolarity.contains(StrategicAxisPolarity.Restrain) &&
              detail.label.exists(_.equalsIgnoreCase("opponent-diagonal-restriction")) =>
          purpose.consequenceKinds.contains(TransitionConsequenceKind.OpponentMobilityRestriction) &&
            purpose.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
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
          purpose.consequenceKinds.exists(positionPlanTechniqueTargetConsequence) ||
            purpose.categories.contains(TransitionConsequenceCategory.TargetPressure.toString)
        case Some(StrategicAxisKind.SpaceCenter) =>
          purpose.consequenceKinds.exists(positionPlanTechniqueCenterConsequence) ||
            purpose.categories.exists(category =>
              category == TransitionConsequenceCategory.CenterControl.toString ||
                category == TransitionConsequenceCategory.OpeningCenterControl.toString
            )
        case Some(StrategicAxisKind.Activity) if detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute =>
          purpose.consequenceKinds.exists(positionPlanTechniquePieceRouteConsequence) ||
            purpose.categories.exists(category =>
              category == TransitionConsequenceCategory.PieceActivity.toString ||
                category == TransitionConsequenceCategory.Development.toString ||
                category == TransitionConsequenceCategory.OpeningDevelopment.toString
            )
        case _ =>
          true
    val polarityMatches =
      detail.axisPolarity match
        case Some(polarity) =>
          val acceptedPolarities = positionPlanTechniqueStructuralPolaritiesForAxis(polarity)
          purpose.polarities.isEmpty || purpose.polarities.exists(acceptedPolarities)
        case None =>
          true
    kindMatches && polarityMatches

  private def positionPlanTechniquePawnTensionDetail(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    val axisText = (detail.axisKey.toList ++ detail.label.toList).mkString(" ").toLowerCase
    axisText.contains("created-tension") ||
      axisText.contains("resolved-tension") ||
      detail.tensionEdges.nonEmpty ||
      detail.tensionSquares.nonEmpty

  private def positionPlanTechniquePawnTensionConsequence(kind: TransitionConsequenceKind): Boolean =
    kind == TransitionConsequenceKind.PawnTensionGain ||
      kind == TransitionConsequenceKind.PawnTensionResolution

  private def positionPlanTechniqueTargetConsequence(kind: TransitionConsequenceKind): Boolean =
    kind == TransitionConsequenceKind.TargetPressureGain ||
      kind == TransitionConsequenceKind.TargetPressureRelease ||
      kind == TransitionConsequenceKind.WeakPawnTargetCreated ||
      kind == TransitionConsequenceKind.WeakSquareTargetCreated

  private def positionPlanTechniqueCenterConsequence(kind: TransitionConsequenceKind): Boolean =
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
      kind == TransitionConsequenceKind.BatteryPressureGain

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

  private def positionPlanTechniqueStructuralPurposeApplies(detail: PositionPlanTechniqueSemanticDetail): Boolean =
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
        detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
        (
          detail.mechanismKinds.exists(positionPlanTechniqueStructuralPurposeMechanism) ||
            positionPlanTechniqueStructuralPurposeAnchor(detail)
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

  private def positionPlanTechniqueStructuralPurposeAnchor(detail: PositionPlanTechniqueSemanticDetail): Boolean =
    detail.semanticAnchorKeys.exists(_.startsWith("StructuralDelta:")) ||
      detail.sourceEvidenceIds.exists(_.toLowerCase.contains("structural-delta"))

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
      detail: PositionPlanTechniqueSemanticDetail
  ): PositionPlanTechniqueSemanticDetail =
    val motifTags = positionPlanTechniqueStructuralMotifTags(detail)
    val axisAnchors = positionPlanTechniqueStructuralAxisAnchors(detail)
    if motifTags.isEmpty && axisAnchors.isEmpty then detail
    else
      detail.copy(
        semanticAnchorKeys = (detail.semanticAnchorKeys ++ axisAnchors).distinct.sorted,
        structuralMotifTags = (detail.structuralMotifTags ++ motifTags).distinct.sorted
      )

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
      val sourceIds = detail.sourceEvidenceIds.map(_.toLowerCase)
      val consequences = detail.structuralPurposeConsequences.map(_.toLowerCase)
      val hasOpenCenterRouteContext =
        positionPlanTechniqueOpenCenterContext(detail) &&
          positionPlanTechniqueDevelopmentRouteDetail(detail)
      val hasIqpAnchor =
        positionPlanTechniqueIqpContext(detail)
      val hasStructuralTransition =
        anchorKeys.exists(_.startsWith("structuraldelta:")) ||
          sourceIds.exists(_.contains("structural-delta")) ||
          consequences.nonEmpty
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
            .withStructuralPurpose(positionPlanTechniqueStructuralPurposeForDetailSources(detail, sourceIds, structuralPurposeBySourceId))
            .withThreatProjection(positionPlanTechniqueThreatForSources(sourceIds, threatBySourceId))
        )
      )
    val planDetails =
      payload.planComparison.toList.filter(_.hasPlanDelta).map(plan =>
        val sourceIds = payload.sourceRefs.map(_.id).distinct.sorted
        PositionPlanTechniqueSemanticDetail(
          unit = PositionPlanTechniqueUnit.PlanOptionSet,
          contrastOutcome = Some(plan.outcome),
          narrativeHorizon = Some(payload.sustainability.horizon),
          semanticAnchorKeys = anchorKeys,
          referencePlanIds = plan.referencePlanIds.distinct.sorted,
          candidatePlanIds = plan.candidatePlanIds.distinct.sorted,
          sourceEvidenceIds = sourceIds
        )
          .withResourceContest(positionPlanTechniqueResourceContestForSources(sourceIds, resourceContestBySourceId))
          .withStructuralPurpose(positionPlanTechniqueStructuralPurposeForSources(sourceIds, structuralPurposeBySourceId))
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

  private def positionPlanTechniquePawnBreakTransitionEvidence(
      detail: PositionPlanTechniqueSemanticDetail
  ): Boolean =
    detail.sourceEvidenceIds.exists(id =>
      val normalized = id.toLowerCase
      normalized.contains(":played-transition") ||
        normalized.contains(":reference-transition") ||
        normalized.contains(":transition:played") ||
        normalized.contains(":transition:reference")
    )

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
      horizons: List[LineEndgameTechniqueHorizon],
      evidenceIds: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    horizons
      .map { horizon =>
        PositionPlanTechniqueSemanticDetail(
          unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
          semanticAnchorKeys = List(lineEndgameTechniqueSemanticAnchor(horizon).stableKey),
          endgameTechniquePattern = Some(horizon.pattern),
          endgameTechniqueRookPattern = Some(horizon.rookPattern),
          endgameTechniqueSide = Some(horizon.techniqueSideKey),
          endgameTechniqueHorizonStatus = Some(horizon.status.toString),
          endgameTechniqueTriggerMove = horizon.triggerMove,
          endgameTechniqueEntryPlyOffset = Some(horizon.entryPlyOffset),
          endgameTechniqueTerminalPlyOffset = Some(horizon.terminalPlyOffset),
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

  private def lineTerminalProofPlanDetails(
      consequences: List[LineConsequence],
      evidenceIds: List[String]
  ): List[PositionPlanTechniqueSemanticDetail] =
    consequences
      .filter(lineTerminalProofConsequence)
      .flatMap { consequence =>
        consequence.eventMove.map(_.trim.toLowerCase).map { routeMove =>
          val targetSquare =
            _root_.chess.Square.all.find(_.key == routeMove.slice(2, 4)).map(square => s"target:${square.key}")
          PositionPlanTechniqueSemanticDetail(
            unit = PositionPlanTechniqueUnit.StructuralTransformation,
            label = Some("terminal-proof"),
            semanticAnchorKeys = List(lineTerminalProofSemanticAnchor(consequence).stableKey),
            structuralRouteMove = Some(routeMove),
            structuralPurposeConsequences = List(consequence.kind.toString),
            structuralPurposeSubjects = targetSquare.toList,
            structuralPurposeCategories = List("TerminalProof"),
            structuralPurposePolarities = List(lineTerminalProofPolarity(consequence.kind)),
            terminalConsequenceKinds = List(consequence.kind.toString),
            sourceEvidenceIds = evidenceIds
          )
        }
      }
      .distinctBy(detail => (detail.structuralRouteMove, detail.terminalConsequenceKinds.mkString(",")))
      .sortBy(detail => detail.structuralRouteMove.getOrElse(""))

  private def lineTerminalProofPolarity(kind: LineConsequenceKind): String =
    kind match
      case LineConsequenceKind.MaterialLoss => "Loss"
      case LineConsequenceKind.DrawResource => "Preserve"
      case _                                => "Gain"

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
      "EndgameTechniqueHorizon",
      s"pattern:${horizon.pattern}",
      s"rook-pattern:${horizon.rookPattern}",
      s"horizonStatus:${horizon.status}",
      s"technique-side:${horizon.techniqueSideKey}"
    )

  private def positionPlanTechniqueUnits(
      payload: StrategicMechanismEvidence,
      anchors: List[EvidenceSemanticAnchor]
  ): List[PositionPlanTechniqueUnit] =
    (
      positionPlanTechniqueUnits(payload.kind) ++
        payload.axisDetails.flatMap(positionPlanTechniqueUnits) ++
        anchors.flatMap(positionPlanTechniqueUnits)
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

  private def positionPlanTechniqueUnits(anchor: EvidenceSemanticAnchor): List[PositionPlanTechniqueUnit] =
    anchor.kind match
      case EvidenceSemanticAnchorKind.Plan | EvidenceSemanticAnchorKind.PlanPressure |
          EvidenceSemanticAnchorKind.PlanTransition | EvidenceSemanticAnchorKind.OpeningAnchor |
          EvidenceSemanticAnchorKind.OpeningObserved | EvidenceSemanticAnchorKind.OpeningSupported |
          EvidenceSemanticAnchorKind.StructurePlan =>
        List(PositionPlanTechniqueUnit.PlanOptionSet)
      case EvidenceSemanticAnchorKind.PawnStructure | EvidenceSemanticAnchorKind.PawnPlay |
          EvidenceSemanticAnchorKind.StructuralDelta =>
        List(PositionPlanTechniqueUnit.StructuralTransformation)
      case EvidenceSemanticAnchorKind.StrategicAxis =>
        Nil
      case EvidenceSemanticAnchorKind.BoardAnchor =>
        positionPlanTechniqueUnitsForBoardAnchor(anchor.values)
      case EvidenceSemanticAnchorKind.StrategicKind | EvidenceSemanticAnchorKind.StrategicMechanism |
          EvidenceSemanticAnchorKind.CandidateComparison | EvidenceSemanticAnchorKind.LineEvent |
          EvidenceSemanticAnchorKind.LineConsequence =>
        Nil

  private def positionPlanTechniqueUnitsForBoardAnchor(values: List[String]): List[PositionPlanTechniqueUnit] =
    val keys = values.map(_.toLowerCase).toSet
    List(
      Option.when(keys.exists(_.contains("endgametechnique")))(PositionPlanTechniqueUnit.EndgameTechniqueRecipe),
      Option.when(keys.exists(value => value.contains("counterplayrestraint") || value.contains("space")))(
        PositionPlanTechniqueUnit.SpacePreventionResourceDenial
      ),
      Option.when(keys.exists(value => value.contains("outpost") || value.contains("activity") || value.contains("filecontrol")))(
        PositionPlanTechniqueUnit.PieceRerouteRoute
      ),
      Option.when(keys.exists(value => value.contains("pawnstructure") || value.contains("weaksquare")))(
        PositionPlanTechniqueUnit.StructuralTransformation
      )
    ).flatten

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
    (
      (cause.supportEvidence ++
        cause.attribution.ownedEvidence ++
        cause.attribution.contrastEvidence ++
        cause.attribution.contextEvidence ++
        cause.proof.toList.flatMap(proof =>
          proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs ++ proof.contextSupport.sourceRefs
        )).map(_.id) ++ cause.strategicProofIdentity.signalSourceIds
    ).distinct.sorted
