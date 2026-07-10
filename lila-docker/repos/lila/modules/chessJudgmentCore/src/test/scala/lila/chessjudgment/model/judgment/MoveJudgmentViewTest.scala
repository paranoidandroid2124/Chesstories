package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.analysis.position.PositionFactNormalizer
import lila.chessjudgment.model.{ Fact, FactScope, PlanSequenceSummary, TransitionType }
import lila.chessjudgment.analysis.singlePosition.{
  DefenseAssessment,
  PassedPawnUrgency,
  PawnPlayAnalysis,
  PawnPlayDriver,
  Threat,
  ThreatAnalysis,
  ThreatDriver,
  ThreatEvidenceSource,
  ThreatKind,
  ThreatSeverity,
  TensionPolicy
}
import lila.chessjudgment.model.strategic.VariationLine
import lila.chessjudgment.model.structure.*

class MoveJudgmentViewTest extends munit.FunSuite:

  test("board target anchors keep a clear diagonal attack axis"):
    val target = chess.Square.fromKey("h7").get
    val attacker = chess.Square.fromKey("d3").get
    val record = PositionFactNormalizer.fromBoardFacts(
      id = "board:diagonal-target",
      facts = List(Fact.TargetPiece(Color.Black, target, chess.Pawn, List(attacker), Nil, FactScope.Now)),
      features = None,
      position = PositionNodeRef("8/7p/8/8/8/3B4/8/4K3 w - - 0 1", 1, Some(Color.White), Some("root")),
      scope = EvidenceScope.CurrentPosition
    )
    val anchor = record.payload.asInstanceOf[BoardFactEvidence].boardAnchors.head

    assert(anchor.detail.flatMap(_.axis).contains(BoardAnchorAxis.Diagonal), anchor.detail)
    assert(anchor.semanticGroupingAnchor.stableKey.contains("axis:Diagonal"), anchor.semanticGroupingAnchor.stableKey)

  test("does not parse restriction carriers as piece routes"):
    assertEquals(StructuralPurposeSubject.parse("pawn:b5-b4:advance-restricted"), None)
    assertEquals(StructuralPurposeSubject.parse("pawn:a2-a4:color-complex-safe"), None)

  test("does not stamp an after-position mechanism with its matching claim move"):
    val root = PositionNodeRef("8/8/8/8/8/8/7P/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/7P/8/8/4K3 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "h2h4", 1, LineNodeRole.Played)
    val transitionRef = evidenceRef(
      id = "move-transition:played:h2h4",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:after-played:counterplay",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = afterPlayed,
      line = None,
      scope = EvidenceScope.AfterPlayedPosition
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "defensive-counter-break-c")
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.PawnStructure,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.PawnStructure,
          label = "defensive-counter-break-c",
          source = transitionRef,
          strength = 2,
          axis = Some(axis)
        )
      ),
      semanticAnchors = Nil
    )
    val claim = ClaimSeed(
      id = "claim:after-played:counterplay",
      family = ClaimFamily.Strategic,
      idea = None,
      subject = IdeaSubject.PlayedMove,
      primaryPosition = root,
      primaryLine = Some(playedLine),
      subjectMove = Some("h2h4"),
      evidence = List(transitionRef),
      engineComparison = None,
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )

    val frame = PositionPlanTechniqueProjection
      .frames(
        TypedEvidenceGraph(
          List(
            EvidenceRecord(transitionRef, MoveTransitionEvidence("h2h4", root, afterPlayed)),
            EvidenceRecord(mechanismRef, mechanism, parents = List(transitionRef))
          )
        ),
        Nil,
        List(claim),
        None
      )
      .find(_.mechanismEvidenceIds.contains(mechanismRef.id))
      .get

    assertEquals(frame.line, None)
    assertEquals(frame.moveUci, None)
    assertEquals(frame.semanticDetails.find(_.axisKey.contains(axis.stableKey)).flatMap(_.structuralRouteMove), None)

  test("links plan technique frames to ideas and claims through relative cause evidence"):
    val root = PositionNodeRef("8/8/8/8/8/8/2P5/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "c2c4", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "h2h3", 2, LineNodeRole.Played)
    val sourceRef = evidenceRef(
      id = "plan-transition:central-break",
      producer = EvidenceProducer.PlanTransitionProducer,
      layer = EvidenceLayer.PlanTransition,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.ReferenceTransition
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:central-break",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.ReferenceTransition
    )
    val causeRef = evidenceRef(
      id = "relative-cause:central-break",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val unrelatedCauseRef = evidenceRef(
      id = "relative-cause:shared-source-activity",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val sameAxisTacticalCauseRef = evidenceRef(
      id = "relative-cause:same-axis-tactical",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val sameAxisPlanCauseRef = evidenceRef(
      id = "relative-cause:same-axis-plan",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val sameAxisCenterCauseRef = evidenceRef(
      id = "relative-cause:same-axis-center",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "central-break-timing")
    val unrelatedAxis = StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "shared-source-activity")
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.PlanPressure,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.PlanTransition,
          label = "central-break-timing",
          source = sourceRef,
          strength = 3,
          axis = Some(axis)
        )
      ),
      semanticAnchors = List(EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PlanTransition, "CentralBreakthrough"))
    )
    val unrelatedSignal = mechanism.signals.head.copy(label = "shared-source-activity", axis = Some(unrelatedAxis))
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.PawnBreakOpportunity,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 12.0,
      candidateWinPercentDeltaForMover = -12.0,
      supportEvidence = List(mechanismRef),
      evidenceLines = List(referenceLine, playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(mechanismRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            strategicMechanisms = List(
              StrategicMechanismProof(
                source = mechanismRef,
                kind = StrategicMechanismKind.PlanPressure,
                signals = mechanism.signals
              )
            )
          ),
          contextSupport = RelativeCauseProofSection(
            role = RelativeCauseProofRole.ContextSupport,
            strength = RelativeCauseProofStrength.WeakHint,
            strategicMechanisms = List(
              StrategicMechanismProof(
                source = mechanismRef,
                kind = StrategicMechanismKind.PlanPressure,
                signals = mechanism.signals
              )
            )
          )
        )
      )
    )
    val unrelatedCause = cause.copy(
      kind = RelativeCauseKind.ActivityGain,
      attribution = cause.attribution.copy(ownedEvidence = List(mechanismRef))
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            strategicMechanisms = List(
              StrategicMechanismProof(
                source = mechanismRef,
                kind = StrategicMechanismKind.PlanPressure,
                signals = List(unrelatedSignal)
              )
            )
          )
        )
      )
    )
    val sameAxisTacticalCause = cause.copy(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      attribution = cause.attribution.copy(ownedEvidence = List(mechanismRef))
    )(cause.proof)
    val sameAxisPlanCause = cause.copy(
      kind = RelativeCauseKind.PlanContradiction,
      attribution = cause.attribution.copy(ownedEvidence = List(mechanismRef))
    )(cause.proof)
    val sameAxisCenterCause = cause.copy(
      kind = RelativeCauseKind.CenterControlGain,
      attribution = cause.attribution.copy(ownedEvidence = List(mechanismRef))
    )(cause.proof)
    val ideaRef = ChessIdeaRef("idea:central-break-cause-owned", ChessIdeaFamily.Strategic)
    val idea = ChessIdea(
      ref = ideaRef,
      subject = IdeaSubject.Plan,
      primaryPosition = root,
      primaryLine = Some(playedLine),
      moveUci = Some("h2h3"),
      evidence = List(causeRef),
      requiredLayers = List(EvidenceLayer.RelativeCause),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val claim = ClaimSeed(
      id = "claim:central-break-cause-owned",
      family = ClaimFamily.Strategic,
      idea = Some(ideaRef),
      subject = IdeaSubject.PlayedMove,
      primaryPosition = root,
      primaryLine = Some(playedLine),
      subjectMove = Some("h2h3"),
      evidence = List(causeRef),
      engineComparison = None,
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(
              sourceRef,
              PlanTransitionEvidence(
                PlanSequenceSummary(
                  transitionType = TransitionType.Continuation,
                  momentum = 0.8,
                  primaryPlanId = Some("CentralBreakthrough")
                )
              )
            ),
            EvidenceRecord(mechanismRef, mechanism, parents = List(sourceRef)),
            EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(mechanismRef)),
            EvidenceRecord(unrelatedCauseRef, RelativeCauseFactEvidence(unrelatedCause), parents = List(mechanismRef)),
            EvidenceRecord(sameAxisTacticalCauseRef, RelativeCauseFactEvidence(sameAxisTacticalCause), parents = List(mechanismRef)),
            EvidenceRecord(sameAxisPlanCauseRef, RelativeCauseFactEvidence(sameAxisPlanCause), parents = List(mechanismRef)),
            EvidenceRecord(sameAxisCenterCauseRef, RelativeCauseFactEvidence(sameAxisCenterCause), parents = List(mechanismRef))
          )
        ),
        ideas = List(idea),
        claims = List(claim),
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val frame = view.positionPlanTechniqueFrames.head
    assert(frame.relativeCauseEvidenceIds.contains(causeRef.id), frame.relativeCauseEvidenceIds)
    assert(!frame.relativeCauseEvidenceIds.contains(unrelatedCauseRef.id), frame.relativeCauseEvidenceIds)
    assert(!frame.relativeCauseEvidenceIds.contains(sameAxisTacticalCauseRef.id), frame.relativeCauseEvidenceIds)
    assertEquals(frame.ideaIds, List(ideaRef.id))
    assertEquals(frame.claimIds, List(claim.id))
    val detail = frame.semanticDetails.find(_.axisKey.contains(axis.stableKey)).get
    assertEquals(detail.causeEvidenceIds, List(causeRef.id))
    assert(!detail.causeEvidenceIds.contains(sameAxisPlanCauseRef.id), detail.causeEvidenceIds)
    assert(!detail.causeEvidenceIds.contains(sameAxisCenterCauseRef.id), detail.causeEvidenceIds)
    assert(detail.proofRoles.contains(RelativeCauseProofRole.DirectProof), detail.proofRoles)
    assertEquals(detail.specificityTier, PositionPlanTechniqueSpecificityTier.BroadAxis)
    assert(detail.objectBindingSignatures.exists(_.contains("proof=DirectProof")), detail.objectBindingSignatures)
    assert(!detail.objectBindingSignatures.exists(_.contains("proof=ContextSupport")), detail.objectBindingSignatures)
    assert(detail.objectBindingSignatures.exists(_.contains("target=PlanSubject:centralbreakthrough")), detail.objectBindingSignatures)

  test("decodes plan technique contrast and object slots before prose"):
    val root = PositionNodeRef("8/8/8/8/8/8/4P3/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterReference = PositionNodeRef("8/8/8/8/4P3/8/8/4K3 b - - 0 1", 2, Some(Color.Black), Some("after-reference"))
    val referenceLine = LineNodeRef("reference-line", "e2e4", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "h2h3", 1, LineNodeRole.Played)
    val structuralRef = evidenceRef(
      id = "structural-delta:e4-break",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = afterReference,
      line = Some(referenceLine),
      scope = EvidenceScope.BestLine
    )
    val contrastRef = evidenceRef(
      id = "strategic-contrast:e4-break",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.Counterfactual
    )
    val transition = StructuralTransitionBinding(
      moveUci = "e2e4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterReference,
      line = Some(referenceLine),
      perspective = Color.White
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(
        TransitionConsequence(
          TransitionConsequenceKind.CenterControlGain,
          StructuralSignalPolarity.Gain,
          strength = 4,
          subjects = List("e4")
        )
      )
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "central-break-timing")
    val contrast = StrategicMechanismContrastEvidence(
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      axisComparisons = List(
        StrategicAxisComparison(
          axis = axis,
          outcome = StrategicAxisComparisonOutcome.ReferencePreservesPlan,
          referenceStrength = 4,
          candidateStrength = 1,
          referenceSources = List(structuralRef),
          candidateSources = Nil
        )
      ),
      planComparison = Some(
        StrategicPlanComparison(
          referencePlanIds = List("CentralBreakthrough"),
          candidatePlanIds = Nil,
          outcome = StrategicAxisComparisonOutcome.ReferencePreservesPlan
        )
      ),
      sustainability = StrategicSustainabilityAssessment(
        horizon = StrategicSustainabilityHorizon.MediumPv,
        lineMaintained = true,
        pvMaintained = true,
        referencePlyCount = 6,
        candidatePlyCount = 3
      ),
      support = StrategicContrastSupport(
        directSources = List(structuralRef),
        contrastSources = Nil,
        contextSources = Nil
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(structuralRef, structuralDelta),
            EvidenceRecord(contrastRef, contrast)
          )
        ),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val frame = view.positionPlanTechniqueFrames.head
    val axisDetail = frame.semanticDetails.find(_.axisKey.contains(axis.stableKey)).get
    assertEquals(axisDetail.unit, PositionPlanTechniqueUnit.TensionBreakPolicyRoute)
    assertEquals(axisDetail.contrastOutcome, Some(StrategicAxisComparisonOutcome.ReferencePreservesPlan))
    assertEquals(axisDetail.narrativeHorizon, Some(StrategicSustainabilityHorizon.MediumPv))
    assertEquals(axisDetail.referenceStrength, Some(4))
    assertEquals(axisDetail.candidateStrength, Some(1))
    assertEquals(axisDetail.referenceEvidenceIds, List(structuralRef.id))
    assertEquals(axisDetail.structuralRouteMove, Some("e2e4"))
    assertEquals(axisDetail.structuralRouteRole, Some("Played"))
    assertEquals(axisDetail.structuralRoutePerspective, Some("white"))
    assertEquals(axisDetail.structuralRouteFromPly, Some(1))
    assertEquals(axisDetail.structuralRouteToPly, Some(2))
    assertEquals(axisDetail.structuralPurposeConsequences, List("CenterControlGain"))
    assertEquals(axisDetail.structuralPurposeSubjects, List("e4"))
    assertEquals(
      axisDetail.structuralPurposeCategories,
      List("CenterControl", "OpeningCenterControl", "PlanAnchor", "StrategicMove", "StrategicSupport", "StructuralAnchor")
    )
    assertEquals(axisDetail.structuralPurposePolarities, List("Gain"))
    assertEquals(axisDetail.structuralPurposeStrength, Some(4))
    val planDetail = frame.semanticDetails.find(_.unit == PositionPlanTechniqueUnit.PlanOptionSet).get
    assertEquals(planDetail.referencePlanIds, List("CentralBreakthrough"))
    assertEquals(planDetail.candidatePlanIds, Nil)
    assertEquals(planDetail.structuralRouteMove, Some("e2e4"))
    assertEquals(planDetail.structuralRouteRole, Some("Played"))
    assertEquals(planDetail.structuralPurposeSubjects, List("e4"))
    val binding = frame.objectBindings.find(_.mechanism.exists(_.contains("pawnbreak"))).get
    assert(binding.target.contains("Square:e4"), binding)
    assert(binding.mechanism.exists(_.contains("pawnbreak")), binding)
    assert(binding.consequence.exists(_.contains("referencepreservesplan")), binding)

  test("preserves concrete piece route object ownership on semantic details"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/6N1 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/8/5N2/8/8 b - - 1 1", 2, Some(Color.Black), Some("after-played"))
    val referenceLine = LineNodeRef("reference-line", "a2a4", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "g1f3", 1, LineNodeRole.Played)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:g1f3",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:route-plan-contradiction",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:activity:g1f3",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "g1f3",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      TransitionConsequenceKind.DevelopmentPieceActivated,
      StructuralSignalPolarity.Gain,
      strength = 3,
      subjects = List("knight:g1-f3")
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(consequence)
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity-gain")
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.Activity,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.StructuralDelta,
          label = "activity-gain",
          source = structuralRef,
          strength = 3,
          axis = Some(axis)
        )
      ),
      semanticAnchors = Nil
    )
    val planCause = RelativeCauseFact(
      kind = RelativeCauseKind.PlanContradiction,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 8.0,
      candidateWinPercentDeltaForMover = -8.0,
      supportEvidence = List(mechanismRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(mechanismRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            strategicMechanisms = List(
              StrategicMechanismProof(
                source = mechanismRef,
                kind = StrategicMechanismKind.Activity,
                signals = mechanism.signals
              )
            )
          )
        )
      )
    )

    val frame = PositionPlanTechniqueProjection
      .frames(
        TypedEvidenceGraph(
          List(
            EvidenceRecord(structuralRef, structuralDelta),
            EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef)),
            EvidenceRecord(causeRef, RelativeCauseFactEvidence(planCause), parents = List(mechanismRef))
          )
        ),
        Nil,
        Nil,
        None
      )
      .head
    val detail = frame.semanticDetails
      .find(detail =>
        detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute &&
          detail.structuralPurposeSubjects.contains("knight:g1-f3")
      )
      .get

    assertEquals(detail.structuralPurposeSubjects, List("knight:g1-f3"))
    assertEquals(detail.structuralMotifTags, List("piece"))
    assertEquals(detail.causeEvidenceIds, Nil)
    assertEquals(detail.proofRoles, Nil)
    assertEquals(detail.specificityTier, PositionPlanTechniqueSpecificityTier.ConcreteObjectAxis)
    assert(
      detail.objectBindingSignatures.exists(signature =>
        signature.contains("actor=Piece:knight") &&
          signature.contains("actor=Square:g1") &&
          signature.contains("target=Square:f3")
      ),
      detail.objectBindingSignatures
    )

  test("links exact good current move target pressure to owned move meaning"):
    val root = PositionNodeRef("8/8/8/8/8/8/1p6/3QK3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/8/1Q6/1p6/4K3 b - - 1 1", 2, Some(Color.Black), Some("after-played"))
    val referenceLine = LineNodeRef("reference-line", "d1b3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "d1b3", 1, LineNodeRole.Played)
    val referenceLineEvidence = evidenceRef(
      id = "line:reference:d1b3",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.BestLine
    )
    val playedLineEvidence = evidenceRef(
      id = "line:played:d1b3",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val playedTransitionEvidence = evidenceRef(
      id = "move-transition:played:d1b3",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val relativeAssessmentEvidence = evidenceRef(
      id = "relative-assessment:exact-good-target-pressure",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeAssessment,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d1b3:target-b7",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:target-pressure:d1b3",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:target-pressure:d1b3",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val played = MoveTransitionEdge(
      role = TransitionEdgeRole.Played,
      from = root,
      moveUci = "d1b3",
      to = afterPlayed,
      planTransition = None,
      evidence = playedTransitionEvidence
    )
    val reference = CandidateLineNode(
      ref = referenceLine,
      line = VariationLine(List("d1b3"), scoreCp = 30, depth = 16),
      evidence = referenceLineEvidence
    )
    val candidate = CandidateLineNode(
      ref = playedLine,
      line = VariationLine(List("d1b3"), scoreCp = 30, depth = 16),
      evidence = playedLineEvidence
    )
    val playedLinePayload =
      new LineFactEvidence(playedLine, Some("d1b3"), None, List("a2a3"), None, None)(
        events = List(
          LineMoveEvent(
            kind = LineEventKind.PassedPawn,
            moveUci = "a2a3",
            plyOffset = 2,
            side = Some(Color.White),
            square = Some(EvidenceSquare("a3"))
          )
        )
      )
    val assessment = RelativeMoveAssessment(
      played = played,
      referenceTransition = None,
      reference = reference,
      candidate = candidate,
      comparison = EvalComparison(
        mover = Color.White,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        rawCandidateDeltaCpForDiagnostics = 0,
        candidateWinPercentDeltaForMover = 0.0,
        rawCpLossForDiagnostics = 0,
        winPercentLossForMover = 0.0,
        verdict = MoveChoiceVerdict.MatchesReference
      ),
      confidence = EvidenceConfidence.EngineBacked,
      evidence = relativeAssessmentEvidence,
      counterfactualEvidence = Nil,
      relativeCauseEvidence = List(causeRef)
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d1b3",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      TransitionConsequenceKind.TargetPressureGain,
      StructuralSignalPolarity.Gain,
      strength = 3,
      subjects = List("b7")
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(consequence)
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, "target-pressure-gain")
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.TargetPressure,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.StructuralDelta,
          label = "target-pressure-gain",
          source = structuralRef,
          strength = 3,
          axis = Some(axis)
        )
      ),
      semanticAnchors = Nil
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.TargetPressureGain,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.MatchesReference,
      winPercentLossForMover = 0.0,
      candidateWinPercentDeltaForMover = 0.0,
      supportEvidence = List(mechanismRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(mechanismRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            strategicMechanisms = List(
              StrategicMechanismProof(
                source = mechanismRef,
                kind = StrategicMechanismKind.TargetPressure,
                signals = mechanism.signals
              )
            ),
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, consequence))
          )
        )
      )
    )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(structuralRef, structuralDelta),
        EvidenceRecord(playedLineEvidence, playedLinePayload),
        EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef, playedLineEvidence)),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(mechanismRef, structuralRef))
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = List(assessment),
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    assert(view.causeAudit.all.exists(_.causeEvidenceIds.contains(causeRef.id)))

    val claim = view.moveMeaningClaims.find(_.causeEvidenceIds.contains(causeRef.id)).get
    assertEquals(claim.meaningKind, "TargetPressure")
    assertEquals(claim.moveUci, "d1b3")
    assertEquals(claim.supportLevel, "owned_cause_linked")
    assertEquals(claim.surfaceLane, "current_move_owned")
    assertEquals(claim.lineRole, "candidate")
    assertEquals(claim.causeEvidenceIds, List(causeRef.id))
    assert(claim.targetSquares.contains("b7"), claim.targetSquares)
    assert(!claim.boardCarriers.exists(_.value == "passed-pawn:a3"), claim.boardCarriers)

    val surfaceOnlyView = MoveJudgmentView
      .from(
        relativeAssessments = List(assessment.copy(relativeCauseEvidence = Nil)),
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(structuralRef, structuralDelta),
            EvidenceRecord(playedLineEvidence, playedLinePayload),
            EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef, playedLineEvidence))
          )
        ),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get
    val surfaceOnlyClaim = surfaceOnlyView.moveMeaningClaims.find(_.meaningKind == "TargetPressure").get
    assertEquals(surfaceOnlyClaim.supportLevel, "view_surfaced")
    assertEquals(surfaceOnlyClaim.surfaceLane, "current_move_function")
    assertEquals(surfaceOnlyClaim.causeEvidenceIds, Nil)
    assert(surfaceOnlyClaim.targetSquares.contains("b7"), surfaceOnlyClaim.targetSquares)
    assert(MoveMeaningSurface.from(surfaceOnlyView).exists(_.ideaType == "target_pressure"))

  test("links exact quiet piece route through root owned structural proof without axis identity"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/4KN2 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/8/4N3/8/4K3 b - - 1 1", 2, Some(Color.Black), Some("after-played"))
    val referenceLine = LineNodeRef("reference-line", "f1e3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "f1e3", 1, LineNodeRole.Played)
    val referenceLineEvidence = evidenceRef(
      id = "line:reference:f1e3:quiet-route",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.BestLine
    )
    val playedLineEvidence = evidenceRef(
      id = "line:played:f1e3:quiet-route",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val playedTransitionEvidence = evidenceRef(
      id = "move-transition:played:f1e3:quiet-route",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val relativeAssessmentEvidence = evidenceRef(
      id = "relative-assessment:quiet-route",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeAssessment,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val structuralRef = evidenceRef(
      id = "structural-delta:played:f1e3:quiet-route",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:activity:f1e3:quiet-route",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:activity:f1e3:quiet-route",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val played = MoveTransitionEdge(
      role = TransitionEdgeRole.Played,
      from = root,
      moveUci = "f1e3",
      to = afterPlayed,
      planTransition = None,
      evidence = playedTransitionEvidence
    )
    val reference = CandidateLineNode(
      ref = referenceLine,
      line = VariationLine(List("f1e3"), scoreCp = 20, depth = 16),
      evidence = referenceLineEvidence
    )
    val candidate = CandidateLineNode(
      ref = playedLine,
      line = VariationLine(List("f1e3"), scoreCp = 20, depth = 16),
      evidence = playedLineEvidence
    )
    val assessment = RelativeMoveAssessment(
      played = played,
      referenceTransition = None,
      reference = reference,
      candidate = candidate,
      comparison = EvalComparison(
        mover = Color.White,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        rawCandidateDeltaCpForDiagnostics = 0,
        candidateWinPercentDeltaForMover = 0.0,
        rawCpLossForDiagnostics = 0,
        winPercentLossForMover = 0.0,
        verdict = MoveChoiceVerdict.MatchesReference
      ),
      confidence = EvidenceConfidence.EngineBacked,
      evidence = relativeAssessmentEvidence,
      counterfactualEvidence = Nil,
      relativeCauseEvidence = List(causeRef)
    )
    val transition = StructuralTransitionBinding(
      moveUci = "f1e3",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      TransitionConsequenceKind.DevelopmentPieceActivated,
      StructuralSignalPolarity.Gain,
      strength = 3,
      subjects = List("knight:f1-e3:maneuver")
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(consequence)
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity-gain")
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.Activity,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.StructuralDelta,
          label = "activity-gain",
          source = structuralRef,
          strength = 3,
          axis = Some(axis)
        )
      ),
      semanticAnchors = Nil
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.ActivityGain,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.MatchesReference,
      winPercentLossForMover = 0.0,
      candidateWinPercentDeltaForMover = 0.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, consequence))
          )
        )
      )
    )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(structuralRef, structuralDelta),
        EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef)),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(structuralRef))
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = List(assessment),
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val detail = view.positionPlanTechniqueFrames.flatMap(_.semanticDetails).find(_.unit == PositionPlanTechniqueUnit.PieceRerouteRoute).get
    assertEquals(detail.causeEvidenceIds, List(causeRef.id))
    assert(detail.proofRoles.contains(RelativeCauseProofRole.DirectProof), detail.proofRoles)

    val claim = view.moveMeaningClaims.find(_.causeEvidenceIds.contains(causeRef.id)).get
    assertEquals(claim.meaningKind, "PieceRoute")
    assertEquals(claim.moveUci, "f1e3")
    assertEquals(claim.supportLevel, "owned_cause_linked")
    assertEquals(claim.surfaceLane, "current_move_owned")
    assertEquals(claim.lineRole, "candidate")
    assert(claim.routeIdentityParts.contains("piece:knight"), claim.routeIdentityParts)
    assert(claim.routeIdentityParts.contains("from:f1"), claim.routeIdentityParts)
    assert(claim.routeIdentityParts.contains("to:e3"), claim.routeIdentityParts)

  test("links exact good current move pawn break tension gain without borrowing line capture"):
    val root = PositionNodeRef("4k3/8/8/3p4/8/8/4P3/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("4k3/8/8/3p4/4P3/8/8/4K3 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val referenceLine = LineNodeRef("reference-line", "e2e4", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "e2e4", 1, LineNodeRole.Played)
    val referenceLineEvidence = evidenceRef(
      id = "line:reference:e2e4:exact-break",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.BestLine
    )
    val playedLineEvidence = evidenceRef(
      id = "line:played:e2e4:exact-break",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val playedTransitionEvidence = evidenceRef(
      id = "move-transition:played:e2e4:exact-break",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val relativeAssessmentEvidence = evidenceRef(
      id = "relative-assessment:exact-good-pawn-break",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeAssessment,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val structuralRef = evidenceRef(
      id = "structural-delta:played:e2e4:tension",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:pawn-break:e2e4",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:pawn-break:e2e4",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val played = MoveTransitionEdge(
      role = TransitionEdgeRole.Played,
      from = root,
      moveUci = "e2e4",
      to = afterPlayed,
      planTransition = None,
      evidence = playedTransitionEvidence
    )
    val reference = CandidateLineNode(
      ref = referenceLine,
      line = VariationLine(List("e2e4"), scoreCp = 30, depth = 16),
      evidence = referenceLineEvidence
    )
    val candidate = CandidateLineNode(
      ref = playedLine,
      line = VariationLine(List("e2e4"), scoreCp = 30, depth = 16),
      evidence = playedLineEvidence
    )
    val assessment = RelativeMoveAssessment(
      played = played,
      referenceTransition = None,
      reference = reference,
      candidate = candidate,
      comparison = EvalComparison(
        mover = Color.White,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        rawCandidateDeltaCpForDiagnostics = 0,
        candidateWinPercentDeltaForMover = 0.0,
        rawCpLossForDiagnostics = 0,
        winPercentLossForMover = 0.0,
        verdict = MoveChoiceVerdict.MatchesReference
      ),
      confidence = EvidenceConfidence.EngineBacked,
      evidence = relativeAssessmentEvidence,
      counterfactualEvidence = Nil,
      relativeCauseEvidence = List(causeRef)
    )
    val transition = StructuralTransitionBinding(
      moveUci = "e2e4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      TransitionConsequenceKind.PawnTensionGain,
      StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("break-file:e", "created-tension:e4-d5")
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(consequence)
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "break-file-e-created-tension-e4-d5")
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.PawnStructure,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.StructuralDelta,
          label = "break-file-e-created-tension-e4-d5",
          source = structuralRef,
          strength = 3,
          axis = Some(axis)
        )
      ),
      semanticAnchors = Nil
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.PawnBreakOpportunity,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.MatchesReference,
      winPercentLossForMover = 0.0,
      candidateWinPercentDeltaForMover = 0.0,
      supportEvidence = List(mechanismRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(mechanismRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            strategicMechanisms = List(
              StrategicMechanismProof(
                source = mechanismRef,
                kind = StrategicMechanismKind.PawnStructure,
                signals = mechanism.signals
              )
            ),
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, consequence))
          )
        )
      )
    )
    val playedLinePayload =
      new LineFactEvidence(
        playedLine,
        Some("e2e4"),
        None,
        Nil,
        None,
        Some(
          LineMaterialSummary(
            sideToMove = Color.White,
            captures = List(
              LineMaterialCapture(
                moveUci = "e2e4",
                plyOffset = 0,
                side = Color.White,
                attackerRole = EvidencePieceRole("pawn"),
                capturedRole = EvidencePieceRole("pawn"),
                square = EvidenceSquare("e4"),
                valueCp = 100,
                recapture = false
              )
            ),
            netCaptureCpForMover = 100,
            maxGainCpForMover = 100,
            maxLossCpForMover = 0,
            hasRecaptureChain = false,
            hasRecoveryWindow = false,
            promotionGainCpForMover = 0,
            materialWindowComplete = true
          )
        )
      )(
        events = List(
          LineMoveEvent(
            kind = LineEventKind.Capture,
            moveUci = "e2e4",
            plyOffset = 0,
            side = Some(Color.White),
            pieceRole = Some(EvidencePieceRole("pawn")),
            targetRole = Some(EvidencePieceRole("pawn")),
            square = Some(EvidenceSquare("e4"))
          )
        )
      )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(playedLineEvidence, playedLinePayload),
        EvidenceRecord(structuralRef, structuralDelta),
        EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef)),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(mechanismRef, structuralRef))
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = List(assessment),
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    assert(view.causeAudit.all.exists(_.causeEvidenceIds.contains(causeRef.id)))

    val claim = view.moveMeaningClaims.find(_.causeEvidenceIds.contains(causeRef.id)).get
    assertEquals(claim.meaningKind, "PawnBreakTiming")
    assertEquals(claim.role, "PreparesBreak")
    assertEquals(claim.moveUci, "e2e4")
    assertEquals(claim.supportLevel, "owned_cause_linked")
    assertEquals(claim.surfaceLane, "current_move_owned")
    assertEquals(claim.lineRole, "candidate")
    assertEquals(claim.breakFiles, List("e"))
    assert(claim.breakIdentityParts.contains("tensionEdge:e4-d5"), claim.breakIdentityParts)
    assert(claim.targetFiles.contains("e"), claim.targetFiles)
    assert(claim.targetSquares.contains("e4"), claim.targetSquares)
    assert(claim.targetSquares.contains("d5"), claim.targetSquares)
    assert(
      !claim.boardCarriers.exists(carrier =>
        carrier.kind == "PlanSubject" &&
          (carrier.value.startsWith("material-capture:") || carrier.value.startsWith("material-recapture:"))
      ),
      claim.boardCarriers
    )
    assert(!claim.boardCarriers.exists(carrier => carrier.kind == "Piece" && carrier.value == "pawn"), claim.boardCarriers)

  test("links exact good current move diagonal restriction to owned counterplay meaning"):
    val root = PositionNodeRef("4k3/6b1/8/3p4/3PP3/8/8/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("4k3/6b1/8/3pP3/3P4/8/8/4K3 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val referenceLine = LineNodeRef("reference-line", "e4e5", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "e4e5", 1, LineNodeRole.Played)
    val referenceLineEvidence = evidenceRef(
      id = "line:reference:e4e5:diagonal-denial",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.BestLine
    )
    val playedLineEvidence = evidenceRef(
      id = "line:played:e4e5:diagonal-denial",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val playedTransitionEvidence = evidenceRef(
      id = "move-transition:played:e4e5:diagonal-denial",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val relativeAssessmentEvidence = evidenceRef(
      id = "relative-assessment:exact-good-diagonal-denial",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeAssessment,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val structuralRef = evidenceRef(
      id = "structural-delta:played:e4e5:opponent-diagonal-restriction",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val contextualRestraintRef = evidenceRef(
      id = "strategic-fact:contextual-counterplay-restraint",
      producer = EvidenceProducer.StrategicFeatureProducer,
      layer = EvidenceLayer.Strategic,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:counterplay:e4e5:opponent-diagonal-restriction",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:opponent-restriction:e4e5",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val sameAxisKingSafetyCauseRef = evidenceRef(
      id = "relative-cause:played-best:king-safety:e4e5",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val played = MoveTransitionEdge(
      role = TransitionEdgeRole.Played,
      from = root,
      moveUci = "e4e5",
      to = afterPlayed,
      planTransition = None,
      evidence = playedTransitionEvidence
    )
    val reference = CandidateLineNode(
      ref = referenceLine,
      line = VariationLine(List("e4e5"), scoreCp = 30, depth = 16),
      evidence = referenceLineEvidence
    )
    val candidate = CandidateLineNode(
      ref = playedLine,
      line = VariationLine(List("e4e5"), scoreCp = 30, depth = 16),
      evidence = playedLineEvidence
    )
    val assessment = RelativeMoveAssessment(
      played = played,
      referenceTransition = None,
      reference = reference,
      candidate = candidate,
      comparison = EvalComparison(
        mover = Color.White,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        rawCandidateDeltaCpForDiagnostics = 0,
        candidateWinPercentDeltaForMover = 0.0,
        rawCpLossForDiagnostics = 0,
        winPercentLossForMover = 0.0,
        verdict = MoveChoiceVerdict.MatchesReference
      ),
      confidence = EvidenceConfidence.EngineBacked,
      evidence = relativeAssessmentEvidence,
      counterfactualEvidence = Nil,
      relativeCauseEvidence = List(causeRef, sameAxisKingSafetyCauseRef)
    )
    val transition = StructuralTransitionBinding(
      moveUci = "e4e5",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      TransitionConsequenceKind.OpponentMobilityRestriction,
      StructuralSignalPolarity.Gain,
      strength = 1,
      subjects = List("bishop:g7:diagonal-denial:blocked-by:e5:locked-center:mobility-3-to-2")
    )
    val unrelatedLineUnlock = TransitionConsequence(
      TransitionConsequenceKind.LineUnlockGain,
      StructuralSignalPolarity.Gain,
      strength = 1,
      subjects = List("rook:h1:line-unlock:by:e4e5:mobility+1")
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(consequence, unrelatedLineUnlock)
    )
    val contextualRestraint = StrategicFactEvidence(
      kind = StrategicFactKind.CounterplayRestraint,
      facts = Nil,
      relatedPlans = Nil,
      confidence = 0.8
    )(
      boardAnchors = List(
        BoardAnchor(
          kind = BoardAnchorKind.CounterplayRestraint,
          side = Color.White,
          signal = BoardAnchorSignal.OpponentLowMobility,
          magnitude = 2,
          confidence = 0.8,
          detail = Some(BoardAnchorDetail(subjectColor = Some(Color.Black), targetSquare = Some(EvidenceSquare("a1"))))
        )
      )
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "opponent-diagonal-restriction")
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.CenterControl,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.StructuralDelta,
          label = "opponent-diagonal-restriction",
          source = structuralRef,
          strength = 3,
          axis = Some(axis)
        )
      ),
      semanticAnchors = Nil
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.OpponentRestriction,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.MatchesReference,
      winPercentLossForMover = 0.0,
      candidateWinPercentDeltaForMover = 0.0,
      supportEvidence = List(mechanismRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(mechanismRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            strategicMechanisms = List(
              StrategicMechanismProof(
                source = mechanismRef,
                kind = StrategicMechanismKind.CenterControl,
                signals = mechanism.signals
              )
            ),
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, consequence))
          )
        )
      )
    )
    val sameAxisKingSafetyCause =
      cause.copy(kind = RelativeCauseKind.KingSafetyConcession)(cause.proof)
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(contextualRestraintRef, contextualRestraint),
        EvidenceRecord(structuralRef, structuralDelta, parents = List(contextualRestraintRef)),
        EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef)),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(mechanismRef, structuralRef)),
        EvidenceRecord(
          sameAxisKingSafetyCauseRef,
          RelativeCauseFactEvidence(sameAxisKingSafetyCause),
          parents = List(mechanismRef, structuralRef)
        )
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = List(assessment),
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    assert(view.causeAudit.all.exists(_.causeEvidenceIds.contains(causeRef.id)))
    val counterplayDetail = view.positionPlanTechniqueFrames
      .flatMap(_.semanticDetails)
      .find(_.axisKey.contains(axis.stableKey))
      .getOrElse(fail(s"frames=${view.positionPlanTechniqueFrames}"))
    assertEquals(counterplayDetail.causeEvidenceIds, List(causeRef.id))
    assertEquals(counterplayDetail.resourceContestSquares, List("a1"))
    assertEquals(counterplayDetail.structuralPurposeConsequences, List("OpponentMobilityRestriction"))
    assertEquals(
      counterplayDetail.structuralPurposeSubjects,
      List("bishop:g7:diagonal-denial:blocked-by:e5:locked-center:mobility-3-to-2")
    )
    assert(!view.moveMeaningClaims.exists(_.causeEvidenceIds.contains(sameAxisKingSafetyCauseRef.id)), view.moveMeaningClaims)

    val claim = view.moveMeaningClaims
      .find(_.causeEvidenceIds.contains(causeRef.id))
      .getOrElse(fail(s"claims=${view.moveMeaningClaims}; causeAudit=${view.causeAudit}; frames=${view.positionPlanTechniqueFrames}"))
    assertEquals(claim.meaningKind, "CounterplayControl")
    assertEquals(claim.role, "PreventsCounterplay")
    assertEquals(claim.moveUci, "e4e5")
    assertEquals(claim.supportLevel, "owned_cause_linked")
    assertEquals(claim.surfaceLane, "current_move_owned")
    assertEquals(claim.lineRole, "candidate")
    assertEquals(claim.causeEvidenceIds, List(causeRef.id))
    assertEquals(claim.publicIdeaType, Some("ray_denial"))
    assert(claim.targetPieces.contains("bishop"), claim.targetPieces)
    assert(claim.targetSquares.contains("g7"), claim.targetSquares)
    assert(claim.targetSquares.contains("e5"), claim.targetSquares)
    assert(!claim.routeIdentityParts.exists(_.contains("line-unlock")), claim.routeIdentityParts)
    val surface = MoveMeaningSurface.from(view).find(_.ideaType == "ray_denial").getOrElse(fail(MoveMeaningSurface.from(view).toString))
    assertEquals(surface.subject, "played_move")
    assertEquals(surface.priority, "main")
    assertEquals(surface.target.pieces, List("bishop"))
    assertEquals(surface.target.squares, List("e5", "g7"))

  test("preserves central open structural transformation ownership from structural-delta anchors"):
    val root = PositionNodeRef("8/8/4p3/8/8/8/8/8 b - - 0 1", 1, Some(Color.Black), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/4p3/8/8/8/8 w - - 0 2", 2, Some(Color.White), Some("after-played"))
    val referenceLine = LineNodeRef("reference-line", "f8e7", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "e6e5", 2, LineNodeRole.Played)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:e6e5:open-center",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:structural-open:e6e5",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:open-center-plan-contradiction",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val transition = StructuralTransitionBinding(
      moveUci = "e6e5",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.Black
    )
    val consequence = TransitionConsequence(
      kind = TransitionConsequenceKind.LineUnlockGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 3
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(consequence)
    )
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.Activity,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.StructuralDelta,
          label = "line-unlock",
          source = structuralRef,
          strength = 3
        )
      ),
      semanticAnchors = Nil
    )
    val planCause = RelativeCauseFact(
      kind = RelativeCauseKind.PlanContradiction,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 8.0,
      candidateWinPercentDeltaForMover = -8.0,
      supportEvidence = List(mechanismRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(mechanismRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            strategicMechanisms = List(
              StrategicMechanismProof(
                source = mechanismRef,
                kind = StrategicMechanismKind.PawnStructure,
                signals = mechanism.signals
              )
            )
          )
        )
      )
    )

    val details = PositionPlanTechniqueProjection
      .frames(
        TypedEvidenceGraph(
          List(
            EvidenceRecord(structuralRef, structuralDelta),
            EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef)),
            EvidenceRecord(causeRef, RelativeCauseFactEvidence(planCause), parents = List(mechanismRef))
          )
        ),
        Nil,
        Nil,
        None
      )
      .flatMap(_.semanticDetails)
    val detail = details
      .find(detail =>
        detail.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
          detail.structuralRouteMove.contains("e6e5")
      )
      .get

    assertEquals(detail.structuralPurposeConsequences, List("LineUnlockGain"))
    assertEquals(detail.structuralMotifTags, List("open", "space"))
    assertEquals(detail.causeEvidenceIds, List(causeRef.id))
    assert(detail.proofRoles.contains(RelativeCauseProofRole.DirectProof), detail.proofRoles)

  test("keeps active open-center and IQP structure attached to quiet development route"):
    val root = PositionNodeRef("r1b1kb1r/pp3ppp/2nqpn2/8/2BN4/8/PPP2PPP/R1BQ1RK1 b kq - 0 10", 1, Some(Color.Black), Some("root"))
    val afterPlayed = PositionNodeRef("r1b1k2r/pp2bppp/2nqpn2/8/2BN4/8/PPP2PPP/R1BQ1RK1 w kq - 1 11", 2, Some(Color.White), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "f8e7", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "f8e7", 1, LineNodeRole.BestReference)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:f8e7:development",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val pawnRef = evidenceRef(
      id = "pawn-structure:before:open-center",
      producer = EvidenceProducer.PawnStructureProducer,
      layer = EvidenceLayer.PawnStructure,
      position = root,
      line = None,
      scope = EvidenceScope.CurrentPosition
    )
    val openingRef = evidenceRef(
      id = "feature-anchor:pawn-structure:break-observed",
      producer = EvidenceProducer.FeatureAnchorProducer,
      layer = EvidenceLayer.FeatureAnchor,
      position = root,
      line = None,
      scope = EvidenceScope.CurrentPosition
    )
    val contrastRef = evidenceRef(
      id = "strategic-contrast:played-vs-best:f8e7",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val transition = StructuralTransitionBinding(
      moveUci = "f8e7",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.Black
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(
        TransitionConsequence(
          kind = TransitionConsequenceKind.DevelopmentPieceActivated,
          polarity = StructuralSignalPolarity.Gain,
          strength = 2,
          subjects = List("bishop:f8-e7")
        ),
        TransitionConsequence(
          kind = TransitionConsequenceKind.DevelopmentMobilityGain,
          polarity = StructuralSignalPolarity.Gain,
          strength = 1,
          subjects = List("bishop:f8-e7:mobility+1")
        )
      )
    )
    val pawnStructure = PawnStructureFactEvidence(
      StructureProfile(
        primary = StructureId.OpenCenter,
        confidence = 0.75,
        alternatives = Nil,
        centerState = CenterState.Open,
        evidenceCodes = Nil
      ),
      alignment = None,
      pawnPlay = Some(
        PawnPlayAnalysis(
          pawnBreakReady = true,
          breakFile = Some("e"),
          breakImpact = 80,
          advanceOrCapture = false,
          passedPawnUrgency = PassedPawnUrgency.Background,
          passerBlockade = false,
          blockadeSquare = None,
          blockadeRole = None,
          pusherSupport = false,
          minorityAttack = false,
          counterBreak = true,
          tensionPolicy = TensionPolicy.Maintain,
          tensionSquares = Nil,
          primaryDriver = PawnPlayDriver.BreakReady,
          counterBreakFiles = List("c")
        )
      )
    )
    val openingAnchor = FeatureAnchorEvidence(
      FeatureAnchor(
        theme = OpeningTheme.PawnStructure,
        signal = FeatureAnchorSignal.PawnBreakObserved,
        sourceLayer = EvidenceLayer.PawnStructure,
        strength = 0.7
      )
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Support, "TargetFixation")
    val contrast = StrategicMechanismContrastEvidence(
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      axisComparisons = List(
        StrategicAxisComparison(
          axis = axis,
          outcome = StrategicAxisComparisonOutcome.CandidateStronger,
          referenceStrength = 2,
          candidateStrength = 3,
          referenceSources = Nil,
          candidateSources = List(pawnRef)
        )
      ),
      planComparison = Some(
        StrategicPlanComparison(
          referencePlanIds = List("OpeningDevelopment", "PawnBreakPreparation"),
          candidatePlanIds = List("OpeningDevelopment", "PawnBreakPreparation"),
          outcome = StrategicAxisComparisonOutcome.SharedSustained
        )
      ),
      sustainability = StrategicSustainabilityAssessment(
        horizon = StrategicSustainabilityHorizon.MediumPv,
        lineMaintained = true,
        pvMaintained = true,
        referencePlyCount = 6,
        candidatePlyCount = 6
      ),
      support = StrategicContrastSupport(
        directSources = List(pawnRef),
        contrastSources = Nil,
        contextSources = List(structuralRef, openingRef)
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(structuralRef, structuralDelta),
            EvidenceRecord(pawnRef, pawnStructure),
            EvidenceRecord(openingRef, openingAnchor),
            EvidenceRecord(contrastRef, contrast)
          )
        ),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get
    val detail = view.positionPlanTechniqueFrames
      .flatMap(_.semanticDetails)
      .find(detail => detail.unit == PositionPlanTechniqueUnit.StructuralTransformation && detail.axisKey.contains(axis.stableKey))
      .get

    assertEquals(detail.structuralRouteMove, Some("f8e7"))
    assert(detail.sourceEvidenceIds.contains(pawnRef.id), detail.sourceEvidenceIds)
    assert(detail.sourceEvidenceIds.contains(structuralRef.id), detail.sourceEvidenceIds)
    assert(detail.structuralPurposeSubjects.contains("bishop:f8-e7"), detail.structuralPurposeSubjects)
    assert(detail.structuralPurposeConsequences.contains("DevelopmentPieceActivated"), detail.structuralPurposeConsequences)
    assert(detail.structuralMotifTags.contains("open"), detail.structuralMotifTags)
    assert(!detail.structuralMotifTags.contains("iqp"), detail.structuralMotifTags)
    assert(!detail.structuralMotifTags.contains("isolated"), detail.structuralMotifTags)
    assert(!detail.structuralMotifTags.contains("transition"), detail.structuralMotifTags)
    assert(
      detail.objectBindingSignatures.exists(signature =>
        signature.contains("actor=Move:f8e7") &&
          signature.contains("actor=Piece:bishop") &&
          signature.contains("target=Square:e7")
      ),
      detail.objectBindingSignatures
    )
    val fallbackContextContrast = contrast.copy(
      axisComparisons = contrast.axisComparisons.map(_.copy(referenceSources = Nil, candidateSources = Nil)),
      support = StrategicContrastSupport(
        directSources = Nil,
        contrastSources = Nil,
        contextSources = List(structuralRef, openingRef)
      )
    )
    val fallbackContextView = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(structuralRef, structuralDelta),
            EvidenceRecord(pawnRef, pawnStructure),
            EvidenceRecord(openingRef, openingAnchor),
            EvidenceRecord(contrastRef, fallbackContextContrast)
          )
        ),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get
    val fallbackContextDetail = fallbackContextView.positionPlanTechniqueFrames
      .flatMap(_.semanticDetails)
      .find(detail => detail.unit == PositionPlanTechniqueUnit.StructuralTransformation && detail.axisKey.contains(axis.stableKey))
      .get

    assertEquals(fallbackContextDetail.structuralRouteMove, Some("f8e7"))
    assert(fallbackContextDetail.sourceEvidenceIds.contains(structuralRef.id), fallbackContextDetail.sourceEvidenceIds)
    assert(fallbackContextDetail.sourceEvidenceIds.contains(pawnRef.id), fallbackContextDetail.sourceEvidenceIds)

    val iqpPawnRef = evidenceRef(
      id = "pawn-structure:before:iqp-black",
      producer = EvidenceProducer.PawnStructureProducer,
      layer = EvidenceLayer.PawnStructure,
      position = root,
      line = None,
      scope = EvidenceScope.CurrentPosition
    )
    val iqpContrastRef = evidenceRef(
      id = "strategic-contrast:played-vs-best:f8e7:iqp",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val iqpPawnStructure = pawnStructure.copy(
      profile = pawnStructure.profile.copy(primary = StructureId.IQPBlack),
      pawnPlay = None
    )
    val iqpContrast = contrast.copy(
      axisComparisons = contrast.axisComparisons.map(_.copy(candidateSources = List(iqpPawnRef))),
      support = contrast.support.copy(
        directSources = List(iqpPawnRef),
        contextSources = List(structuralRef)
      )
    )
    val iqpView = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(structuralRef, structuralDelta),
            EvidenceRecord(iqpPawnRef, iqpPawnStructure),
            EvidenceRecord(iqpContrastRef, iqpContrast)
          )
        ),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get
    val iqpDetail = iqpView.positionPlanTechniqueFrames
      .flatMap(_.semanticDetails)
      .find(detail => detail.unit == PositionPlanTechniqueUnit.StructuralTransformation && detail.axisKey.contains(axis.stableKey))
      .get

    assertEquals(iqpDetail.structuralRouteMove, Some("f8e7"))
    assert(iqpDetail.sourceEvidenceIds.contains(iqpPawnRef.id), iqpDetail.sourceEvidenceIds)
    assert(iqpDetail.sourceEvidenceIds.contains(structuralRef.id), iqpDetail.sourceEvidenceIds)
    assert(iqpDetail.structuralPurposeSubjects.contains("bishop:f8-e7"), iqpDetail.structuralPurposeSubjects)
    assert(iqpDetail.structuralMotifTags.contains("iqp"), iqpDetail.structuralMotifTags)
    assert(iqpDetail.structuralMotifTags.contains("isolated"), iqpDetail.structuralMotifTags)
    assert(iqpDetail.structuralMotifTags.contains("transition"), iqpDetail.structuralMotifTags)

  test("keeps IQP maneuver carrier when activity consequence has no route subject"):
    val root = PositionNodeRef(
      "3r2k1/pp2npp1/2qrp2p/8/3P1Q2/1B1R2P1/PP3P1P/3R2K1 b - - 10 25",
      1,
      Some(Color.Black),
      Some("root")
    )
    val afterPlayed = PositionNodeRef(
      "3r2k1/pp3pp1/2qrp2p/3n4/3P1Q2/1B1R2P1/PP3P1P/3R2K1 w - - 11 26",
      2,
      Some(Color.White),
      Some("after-played")
    )
    val playedLine = LineNodeRef("played-line", "e7d5", 1, LineNodeRole.Played)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:e7d5:iqp-maneuver",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val pawnRef = evidenceRef(
      id = "pawn-structure:before:iqp-black",
      producer = EvidenceProducer.PawnStructureProducer,
      layer = EvidenceLayer.PawnStructure,
      position = root,
      line = None,
      scope = EvidenceScope.CurrentPosition
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:activity:e7d5:iqp-maneuver",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "e7d5",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.Black
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(
        TransitionConsequence(
          kind = TransitionConsequenceKind.MobilityGain,
          polarity = StructuralSignalPolarity.Gain,
          strength = 1,
          subjects = Nil
        )
      )
    )
    val pawnStructure = PawnStructureFactEvidence(
      StructureProfile(
        primary = StructureId.IQPBlack,
        confidence = 0.8,
        alternatives = Nil,
        centerState = CenterState.Open,
        evidenceCodes = Nil
      ),
      alignment = None,
      pawnPlay = None
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity-gain")
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.Activity,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.StructuralDelta,
          label = "activity-gain",
          source = structuralRef,
          strength = 2,
          axis = Some(axis)
        )
      ),
      semanticAnchors = Nil
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(structuralRef, structuralDelta),
            EvidenceRecord(pawnRef, pawnStructure),
            EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef, pawnRef))
          )
        ),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get
    val detail = view.positionPlanTechniqueFrames
      .flatMap(_.semanticDetails)
      .find(detail => detail.unit == PositionPlanTechniqueUnit.PieceRerouteRoute && detail.axisKey.contains(axis.stableKey))
      .get

    assert(detail.structuralPurposeSubjects.contains("knight:e7-d5:maneuver"), detail.structuralPurposeSubjects)
    assert(
      detail.objectBindingSignatures.exists(signature =>
        signature.contains("actor=Move:e7d5") &&
          signature.contains("actor=Piece:knight") &&
          signature.contains("actor=Square:e7") &&
          signature.contains("target=Square:d5")
      ),
      detail.objectBindingSignatures
    )

  test("preserves diagonal battery piece roles and target squares in piece route detail"):
    val root = PositionNodeRef("8/8/8/8/8/8/6P1/2B1K2Q w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/7Q/8/8/8/6P1/2B1K3 b - - 1 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "h1h6", 1, LineNodeRole.Played)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:h1h6:diagonal-battery",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:diagonal-battery-plan",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:activity:h1h6:diagonal-battery",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "h1h6",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(
        TransitionConsequence(
          kind = TransitionConsequenceKind.BatteryPressureGain,
          polarity = StructuralSignalPolarity.Gain,
          strength = 3,
          subjects = List("battery:diagonal:c1-h6:bishop-queen")
        )
      )
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "diagonal-battery")
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.Activity,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.StructuralDelta,
          label = "diagonal-battery",
          source = structuralRef,
          strength = 3,
          axis = Some(axis)
        )
      ),
      semanticAnchors = Nil
    )
    val planCause = RelativeCauseFact(
      kind = RelativeCauseKind.PlanImprovement,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = LineNodeRef("reference-line", "e1e2", 1, LineNodeRole.BestReference),
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.MatchesReference,
      winPercentLossForMover = 0.0,
      candidateWinPercentDeltaForMover = 0.0,
      supportEvidence = List(mechanismRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(mechanismRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            strategicMechanisms = List(
              StrategicMechanismProof(
                source = mechanismRef,
                kind = StrategicMechanismKind.Activity,
                signals = mechanism.signals
              )
            )
          )
        )
      )
    )

    val frame = PositionPlanTechniqueProjection
      .frames(
        TypedEvidenceGraph(
          List(
            EvidenceRecord(structuralRef, structuralDelta),
            EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef)),
            EvidenceRecord(causeRef, RelativeCauseFactEvidence(planCause), parents = List(mechanismRef))
          )
        ),
        Nil,
        Nil,
        None
      )
      .head
    val detail = frame.semanticDetails.find(_.unit == PositionPlanTechniqueUnit.PieceRerouteRoute).get

    assertEquals(detail.structuralPurposeSubjects, List("battery:diagonal:c1-h6:bishop-queen"))
    assert(detail.semanticAnchorKeys.contains("axis:Diagonal"), detail.semanticAnchorKeys)
    assert(detail.structuralMotifTags.contains("battery"), detail.structuralMotifTags)
    assert(detail.structuralMotifTags.contains("diagonal"), detail.structuralMotifTags)
    assertEquals(detail.causeEvidenceIds, List(causeRef.id))
    assert(detail.proofRoles.contains(RelativeCauseProofRole.DirectProof), detail.proofRoles)
    assertEquals(detail.specificityTier, PositionPlanTechniqueSpecificityTier.ExactObjectAxis)
    assert(
      detail.objectBindingSignatures.exists(signature =>
        signature.contains("actor=Piece:bishop") &&
          signature.contains("actor=Piece:queen") &&
          signature.contains("target=Square:c1") &&
          signature.contains("target=Square:h6") &&
          signature.contains("mechanism=Mechanism:battery-diagonal")
      ),
      detail.objectBindingSignatures
    )

  test("ordinary bishop development does not become main long-diagonal pressure"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/4KB2 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/1B6/8/8/8/4K3 b - - 1 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "f1b5", 1, LineNodeRole.Played)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:f1b5:mobility",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val mechanismRef = evidenceRef(
      id = "strategic-mechanism:activity:f1b5:mobility",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "f1b5",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(
        TransitionConsequence(
          kind = TransitionConsequenceKind.MobilityGain,
          polarity = StructuralSignalPolarity.Gain,
          strength = 4,
          subjects = List("bishop:f1-b5:mobility+4")
        )
      )
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity-gain")
    val mechanism = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.Activity,
      signals = List(
        StrategicMechanismSignal(
          kind = StrategicMechanismSignalKind.StructuralDelta,
          label = "activity-gain",
          source = structuralRef,
          strength = 4,
          axis = Some(axis)
        )
      ),
      semanticAnchors = Nil
    )

    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(structuralRef, structuralDelta),
        EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef))
      )
    )
    val frame = PositionPlanTechniqueProjection
      .frames(
        graph,
        Nil,
        Nil,
        None
      )
      .head
    val detail = frame.semanticDetails.find(_.unit == PositionPlanTechniqueUnit.PieceRerouteRoute).get

    assertEquals(detail.structuralPurposeSubjects, List("bishop:f1-b5:mobility+4"))
    assert(detail.objectBindingSignatures.exists(_.contains("actor=Piece:bishop")), detail.objectBindingSignatures)
    assert(!detail.objectBindingSignatures.exists(_.contains("mechanism=Mechanism:bishop-long-diagonal")), detail.objectBindingSignatures)
    assert(!detail.objectBindingSignatures.exists(_.contains("consequence=Consequence:diagonalpressure")), detail.objectBindingSignatures)
    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get
    assert(!MoveMeaningSurface.from(view).exists(surface => surface.ideaType == "ray_denial" || surface.ideaType == "long_diagonal_pressure"))

  test("decodes counterplay race line lead from strategic contrast"):
    val root = PositionNodeRef("8/8/8/8/8/8/4P3/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "b2b4", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "g7g5", 1, LineNodeRole.Played)
    val sourceRef = evidenceRef(
      id = "strategic-fact:counterplay-race",
      producer = EvidenceProducer.StrategicFeatureProducer,
      layer = EvidenceLayer.Strategic,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition
    )
    val contrastRef = evidenceRef(
      id = "strategic-contrast:counterplay-race",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.Counterfactual
    )
    val threatRef = evidenceRef(
      id = "threat-episode:counterplay-race",
      producer = EvidenceProducer.ThreatPressureProducer,
      layer = EvidenceLayer.ThreatPressure,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.ThreatLine
    )
    val threat = Threat(
      kind = ThreatKind.Positional,
      lossIfIgnoredCp = 80,
      lossIfIgnoredWinPercent = Some(10.0),
      turnsToImpact = 3,
      evidenceSource = ThreatEvidenceSource.MotifAndLineValueDelta,
      motifs = Nil,
      attackSquares = List("g4"),
      targetPieces = List("king"),
      bestDefense = Some("h2h3"),
      defenseCount = 1
    )
    val threatSummary = ThreatAnalysis(
      threats = List(threat),
      defense = DefenseAssessment(
        necessity = ThreatSeverity.Important,
        onlyDefense = Some("h2h3"),
        alternatives = Nil,
        counterIsBetter = false,
        prophylaxisNeeded = true,
        resourceCoverageScore = 80
      ),
      threatSeverity = ThreatSeverity.Important,
      immediateThreat = false,
      strategicThreat = true,
      threatIgnorable = false,
      defenseRequired = true,
      counterThreatBetter = false,
      prophylaxisNeeded = true,
      resourceAvailable = true,
      maxLossIfIgnored = 80,
      maxWinPercentLossIfIgnored = Some(10.0),
      primaryDriver = ThreatDriver.PositionalThreat,
      insufficientData = false
    )
    val episode = ThreatEpisode.fromThreat(Color.White, threat, 0)
    val axis = StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Gain, "queenside-counterplay-race")
    val contrast = StrategicMechanismContrastEvidence(
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      axisComparisons = List(
        StrategicAxisComparison(
          axis = axis,
          outcome = StrategicAxisComparisonOutcome.CandidateStronger,
          referenceStrength = 1,
          candidateStrength = 4,
          referenceSources = Nil,
          candidateSources = List(threatRef)
        )
      ),
      planComparison = None,
      sustainability = StrategicSustainabilityAssessment(
        horizon = StrategicSustainabilityHorizon.ShortPv,
        lineMaintained = true,
        pvMaintained = true,
        referencePlyCount = 3,
        candidatePlyCount = 5
      ),
      support = StrategicContrastSupport(
        directSources = List(threatRef),
        contrastSources = Nil,
        contextSources = Nil
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(sourceRef, StrategicFactEvidence(StrategicFactKind.PlanPressure, Nil, Nil, 0.8)(boardAnchors = Nil)),
            EvidenceRecord(threatRef, ThreatEpisodeEvidence(episode, threatSummary)),
            EvidenceRecord(contrastRef, contrast)
          )
        ),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val raceDetail =
      view.positionPlanTechniqueFrames
        .flatMap(_.semanticDetails)
        .find(detail => detail.unit == PositionPlanTechniqueUnit.CounterplayRace && detail.axisKey.contains(axis.stableKey))
        .get
    assertEquals(raceDetail.axisKey, Some(axis.stableKey))
    assertEquals(raceDetail.narrativeHorizon, Some(StrategicSustainabilityHorizon.ShortPv))
    assertEquals(raceDetail.raceLeadingLineRole, Some(LineNodeRole.Played))
    assertEquals(raceDetail.raceReferenceRootMove, Some("b2b4"))
    assertEquals(raceDetail.raceCandidateRootMove, Some("g7g5"))
    assertEquals(raceDetail.turnsToImpact, Some(3))
    assertEquals(raceDetail.defenseMove, Some("h2h3"))
    assertEquals(raceDetail.prophylaxisNeeded, Some(true))

  test("classifies counterplay restraint contrast as prevention rather than race"):
    val root = PositionNodeRef("8/8/8/8/8/8/4P3/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g2g4", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "h2h3", 1, LineNodeRole.Played)
    val restraintRef = evidenceRef(
      id = "strategic-fact:counterplay-restraint",
      producer = EvidenceProducer.StrategicFeatureProducer,
      layer = EvidenceLayer.Strategic,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.BestLine
    )
    val contrastRef = evidenceRef(
      id = "strategic-contrast:counterplay-restraint",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.Counterfactual
    )
    val tacticalCauseRef = evidenceRef(
      id = "relative-cause:tactical-context-counterplay-restraint",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.Counterfactual
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "opponent-low-mobility")
    val contrast = StrategicMechanismContrastEvidence(
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      axisComparisons = List(
        StrategicAxisComparison(
          axis = axis,
          outcome = StrategicAxisComparisonOutcome.ReferencePreservesPlan,
          referenceStrength = 4,
          candidateStrength = 1,
          referenceSources = List(restraintRef),
          candidateSources = Nil
        )
      ),
      planComparison = None,
      sustainability = StrategicSustainabilityAssessment(
        horizon = StrategicSustainabilityHorizon.MediumPv,
        lineMaintained = true,
        pvMaintained = true,
        referencePlyCount = 6,
        candidatePlyCount = 3
      ),
      support = StrategicContrastSupport(
        directSources = List(restraintRef),
        contrastSources = Nil,
        contextSources = Nil
      )
    )
    val restraintAnchor = BoardAnchor(
      kind = BoardAnchorKind.CounterplayRestraint,
      side = Color.White,
      signal = BoardAnchorSignal.OpponentLowMobility,
      magnitude = 4,
      confidence = 0.9,
      detail = Some(BoardAnchorDetail(subjectColor = Some(Color.Black), targetSquare = Some(EvidenceSquare("g5"))))
    )
    val restraintFact = StrategicFactEvidence(
      kind = StrategicFactKind.CounterplayRestraint,
      facts = Nil,
      relatedPlans = Nil,
      confidence = 0.8
    )(boardAnchors = List(restraintAnchor))
    val tacticalCause = RelativeCauseFact(
      kind = RelativeCauseKind.MissedTacticalResource,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 12.0,
      candidateWinPercentDeltaForMover = -12.0,
      supportEvidence = Nil,
      evidenceLines = List(referenceLine, candidateLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = referenceLine,
      sourceSide = RelativeCauseSourceSide.Reference,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.ReferenceCreatesResource,
        ownedEvidence = Nil,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          contextSupport = RelativeCauseProofSection(
            role = RelativeCauseProofRole.ContextSupport,
            strength = RelativeCauseProofStrength.WeakHint,
            strategicMechanismContrasts = List(
              StrategicMechanismContrastProof(
                source = contrastRef,
                comparisonKind = CandidateComparisonKind.PlayedVsBest,
                referenceLine = referenceLine,
                candidateLine = candidateLine,
                axisComparisons = contrast.axisComparisons,
                sustainability = contrast.sustainability
              )
            )
          )
        )
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(restraintRef, restraintFact),
            EvidenceRecord(contrastRef, contrast),
            EvidenceRecord(tacticalCauseRef, RelativeCauseFactEvidence(tacticalCause), parents = List(contrastRef))
          )
        ),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val frame = view.positionPlanTechniqueFrames.head
    assert(frame.semanticAnchors.exists(_.stableKey.contains("CounterplayRestraint")), frame.semanticAnchors.map(_.stableKey))
    val axisDetail = frame.semanticDetails.find(_.axisKey.contains(axis.stableKey)).get
    assertEquals(axisDetail.unit, PositionPlanTechniqueUnit.SpacePreventionResourceDenial)
    assert(axisDetail.semanticAnchorKeys.exists(_.contains("CounterplayRestraint")), axisDetail.semanticAnchorKeys)
    assertEquals(axisDetail.resourceContestActorSide, Some("white"))
    assertEquals(axisDetail.resourceContestTargetSide, Some("black"))
    assertEquals(axisDetail.resourceContestKinds, List("CounterplayRestraint"))
    assertEquals(axisDetail.resourceContestSignals, List("OpponentLowMobility"))
    assertEquals(axisDetail.resourceContestSquares, List("g5"))
    assertEquals(axisDetail.resourceContestScopes, List("counterplay", "kingside", "space"))
    assertEquals(axisDetail.resourceContestMagnitude, Some(4))
    assertEquals(axisDetail.causeEvidenceIds, Nil)
    assertEquals(axisDetail.proofRoles, Nil)
    assertEquals(axisDetail.contextCauseEvidenceIds, List(tacticalCauseRef.id))
    assertEquals(axisDetail.contextProofRoles, List(RelativeCauseProofRole.ContextSupport))

  test("surfaces threat episode prevention frames before prose"):
    val root = PositionNodeRef("8/8/8/8/8/8/2P5/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "g2g3", 1, LineNodeRole.Played)
    val threatRef = evidenceRef(
      id = "threat-episode:prevent-counterplay",
      producer = EvidenceProducer.ThreatPressureProducer,
      layer = EvidenceLayer.ThreatPressure,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.ThreatLine
    )
    val restraintRef = evidenceRef(
      id = "strategic-fact:prevent-counterplay-restraint",
      producer = EvidenceProducer.StrategicFeatureProducer,
      layer = EvidenceLayer.Strategic,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.ThreatLine
    )
    val threat = Threat(
      kind = ThreatKind.Positional,
      lossIfIgnoredCp = 90,
      lossIfIgnoredWinPercent = Some(12.0),
      turnsToImpact = 3,
      evidenceSource = ThreatEvidenceSource.MotifAndLineValueDelta,
      motifs = Nil,
      attackSquares = List("d4"),
      targetPieces = List("knight"),
      bestDefense = Some("g2g3"),
      defenseCount = 1
    )
    val summary = ThreatAnalysis(
      threats = List(threat),
      defense = DefenseAssessment(
        necessity = ThreatSeverity.Important,
        onlyDefense = Some("g2g3"),
        alternatives = Nil,
        counterIsBetter = false,
        prophylaxisNeeded = true,
        resourceCoverageScore = 80
      ),
      threatSeverity = ThreatSeverity.Important,
      immediateThreat = false,
      strategicThreat = true,
      threatIgnorable = false,
      defenseRequired = true,
      counterThreatBetter = false,
      prophylaxisNeeded = true,
      resourceAvailable = true,
      maxLossIfIgnored = 90,
      maxWinPercentLossIfIgnored = Some(12.0),
      primaryDriver = ThreatDriver.PositionalThreat,
      insufficientData = false
    )
    val episode = ThreatEpisode.fromThreat(Color.White, threat, 0)
    val restraintAnchor = BoardAnchor(
      kind = BoardAnchorKind.CounterplayRestraint,
      side = Color.Black,
      signal = BoardAnchorSignal.OpponentLowMobility,
      magnitude = 3,
      confidence = 0.82,
      detail = Some(BoardAnchorDetail(subjectColor = Some(Color.White), targetSquare = Some(EvidenceSquare("g5"))))
    )
    val restraintFact = StrategicFactEvidence(
      kind = StrategicFactKind.CounterplayRestraint,
      facts = Nil,
      relatedPlans = Nil,
      confidence = 0.8
    )(boardAnchors = List(restraintAnchor))
    val causeRef = evidenceRef(
      id = "relative-cause:only-defense:prevent-counterplay",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = playedLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 12.0,
      candidateWinPercentDeltaForMover = -12.0,
      supportEvidence = List(threatRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Reference,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.ReferenceCreatesResource,
        ownedEvidence = List(threatRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            threatEpisodes = List(
              ThreatEpisodeCauseProof(
                source = threatRef,
                driver = episode.driver,
                kind = episode.kind,
                severity = episode.severity
              )
            )
          )
        )
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(restraintRef, restraintFact),
            EvidenceRecord(threatRef, ThreatEpisodeEvidence(episode, summary), parents = List(restraintRef)),
            EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(threatRef))
          )
        ),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    assertEquals(view.positionPlanTechniqueFrames.size, 1)
    val frame = view.positionPlanTechniqueFrames.head
    assert(frame.units.contains(PositionPlanTechniqueUnit.SpacePreventionResourceDenial), frame.units)
    assert(!frame.units.contains(PositionPlanTechniqueUnit.CounterplayRace), frame.units)
    assertEquals(frame.evidenceIds, List(threatRef.id))
    assert(frame.objectBindingSignatures.nonEmpty, frame)
    val preventionDetail = frame.semanticDetails.find(_.unit == PositionPlanTechniqueUnit.SpacePreventionResourceDenial).get
    assertEquals(preventionDetail.threatKind, Some("Positional"))
    assertEquals(preventionDetail.threatDriver, Some("PositionalThreat"))
    assertEquals(preventionDetail.threatSeverity, Some("Important"))
    assertEquals(preventionDetail.turnsToImpact, Some(3))
    assertEquals(preventionDetail.defenseMove, Some("g2g3"))
    assertEquals(preventionDetail.prophylaxisNeeded, Some(true))
    assertEquals(preventionDetail.maxWinPercentLossIfIgnored, Some(12.0))
    assertEquals(preventionDetail.resourceContestKinds, List("CounterplayRestraint"))
    assertEquals(preventionDetail.resourceContestSignals, List("OpponentLowMobility"))
    assertEquals(preventionDetail.resourceContestSquares, List("d4", "g5"))
    assertEquals(preventionDetail.resourceContestScopes, List("counterplay", "kingside", "prophylaxis", "space", "threat"))
    assertEquals(preventionDetail.causeEvidenceIds, List(causeRef.id))
    assertEquals(preventionDetail.proofRoles, List(RelativeCauseProofRole.DirectProof))

  test("exposes played alternative structural improvement as context cause"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val afterReference = PositionNodeRef("8/8/8/8/8/5N2/3P4/8 b - - 1 1", 2, Some(Color.Black), Some("after-reference"))

    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val alternativeLine = LineNodeRef("alternative-line", "c2c4", 3, LineNodeRole.Alternative)

    val playedLineEvidence = evidenceRef(
      id = "line:played",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = afterPlayed,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedLine
    )
    val referenceLineEvidence = evidenceRef(
      id = "line:reference",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = afterReference,
      line = Some(referenceLine),
      scope = EvidenceScope.BestLine
    )
    val playedTransitionEvidence = evidenceRef(
      id = "transition:played",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val relativeAssessmentEvidence = evidenceRef(
      id = "relative-assessment:played",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeAssessment,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )

    val played = MoveTransitionEdge(
      role = TransitionEdgeRole.Played,
      from = root,
      moveUci = "d2d4",
      to = afterPlayed,
      planTransition = None,
      evidence = playedTransitionEvidence
    )
    val reference = CandidateLineNode(
      ref = referenceLine,
      line = VariationLine(List("g1f3"), scoreCp = 30, depth = 16),
      evidence = referenceLineEvidence
    )
    val candidate = CandidateLineNode(
      ref = playedLine,
      line = VariationLine(List("d2d4"), scoreCp = 20, depth = 16),
      evidence = playedLineEvidence
    )
    val assessment = RelativeMoveAssessment(
      played = played,
      referenceTransition = None,
      reference = reference,
      candidate = candidate,
      comparison = EvalComparison(
        mover = Color.White,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        rawCandidateDeltaCpForDiagnostics = -10,
        candidateWinPercentDeltaForMover = 0.0,
        rawCpLossForDiagnostics = 10,
        winPercentLossForMover = 0.4,
        verdict = MoveChoiceVerdict.PlayableLoss
      ),
      confidence = EvidenceConfidence.EngineBacked,
      evidence = relativeAssessmentEvidence,
      counterfactualEvidence = Nil
    )

    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      kind = TransitionConsequenceKind.CenterControlGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("center")
    )
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val structuralRecord = EvidenceRecord(
      ref = structuralRef,
      payload = StructuralDeltaEvidence(
        transition = transition,
        signals = Nil,
        consequences = List(consequence)
      )
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.StructuralImprovement,
      comparisonKind = CandidateComparisonKind.PlayedVsAlternative,
      referenceLine = alternativeLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.ImprovesOnReference,
      winPercentLossForMover = 0.0,
      candidateWinPercentDeltaForMover = 1.4,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PlayedAlternativeContext,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Context,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      proof = Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, consequence))
          )
        )
      )
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-alt:structural-improvement",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val graph = TypedEvidenceGraph(
      List(
        structuralRecord,
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(structuralRef))
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = List(assessment),
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    assertEquals(view.causeAudit.primary, Nil)
    assertEquals(view.causeAudit.context.map(_.causeKind), List(RelativeCauseKind.StructuralImprovement))
    assertEquals(view.causeAudit.context.head.causeRole, RelativeCauseRole.PlayedAlternativeContext)
    assertEquals(view.causeAudit.context.head.causeSourceSide, RelativeCauseSourceSide.Candidate)
    assertEquals(view.causeAudit.context.head.evidenceIds, List(causeRef.id, structuralRef.id).sorted)

  test("does not expose playable loss relative cause as primary bad cause"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:tactical",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.PlayableLoss,
      winPercentLossForMover = 2.5,
      candidateWinPercentDeltaForMover = 0.0,
      supportEvidence = List(causeRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary
    )(None)
    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(List(EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause)))),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    assertEquals(view.causeAudit.primary, Nil)
    assertEquals(view.causeAudit.secondary.map(_.causeKind), List(RelativeCauseKind.TacticalRefutationOfPlayed))

  test("keeps line-bound tactical witness but demotes same-comparison-only evidence to context"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val floatingLine = LineNodeRef("floating-line", "b1c3", 3, LineNodeRole.Alternative)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      kind = TransitionConsequenceKind.CenterControlGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("d4")
    )
    val structuralCause = RelativeCauseFact(
      kind = RelativeCauseKind.StructuralImprovement,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Inaccuracy,
      winPercentLossForMover = 4.0,
      candidateWinPercentDeltaForMover = -4.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      proof = Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, consequence))
          )
        )
      )
    )
    val structuralCauseRef = evidenceRef(
      id = "relative-cause:played-best:structural",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val tacticalCauseRef = evidenceRef(
      id = "relative-cause:played-best:tactical",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val tacticalCause = RelativeCauseFact(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Inaccuracy,
      winPercentLossForMover = 4.0,
      candidateWinPercentDeltaForMover = -4.0,
      supportEvidence = Nil,
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(None)
    val recaptureCauseRef = evidenceRef(
      id = "relative-cause:played-best:recapture",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.Counterfactual
    )
    val recaptureCause = tacticalCause.copy(
      kind = RelativeCauseKind.RecaptureRecoveryWindow,
      sourceSide = RelativeCauseSourceSide.Reference,
      eventLine = referenceLine,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(None)
    val defensiveCauseRef = evidenceRef(
      id = "relative-cause:played-best:defensive",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.Counterfactual
    )
    val defensiveCause = tacticalCause.copy(
      kind = RelativeCauseKind.DefensiveResource,
      sourceSide = RelativeCauseSourceSide.Reference,
      eventLine = referenceLine,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(None)
    val sameComparisonOnlyCauseRef = evidenceRef(
      id = "relative-cause:played-best:same-comparison-only",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(floatingLine),
      scope = EvidenceScope.Counterfactual
    )
    val sameComparisonOnlyCause = tacticalCause.copy(
      kind = RelativeCauseKind.MaterialSwing,
      evidenceLines = List(floatingLine),
      eventLine = floatingLine
    )(None)
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(transition = transition, signals = Nil, consequences = List(consequence))
        ),
        EvidenceRecord(structuralCauseRef, RelativeCauseFactEvidence(structuralCause), parents = List(structuralRef)),
        EvidenceRecord(tacticalCauseRef, RelativeCauseFactEvidence(tacticalCause)),
        EvidenceRecord(recaptureCauseRef, RelativeCauseFactEvidence(recaptureCause)),
        EvidenceRecord(defensiveCauseRef, RelativeCauseFactEvidence(defensiveCause)),
        EvidenceRecord(sameComparisonOnlyCauseRef, RelativeCauseFactEvidence(sameComparisonOnlyCause))
      )
    )
    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val primaryByKind = view.causeAudit.primary.map(frame => frame.causeKind -> frame).toMap
    val secondaryByKind = view.causeAudit.secondary.map(frame => frame.causeKind -> frame).toMap
    val contextByKind = view.causeAudit.context.map(frame => frame.causeKind -> frame).toMap
    val structuralRoot = primaryByKind(RelativeCauseKind.StructuralImprovement)
    assertEquals(view.causeAudit.primary.map(_.causeKind), List(RelativeCauseKind.StructuralImprovement))
    assertEquals(structuralRoot.narrativeRole, MoveJudgmentCauseNarrativeRole.RootCause)
    assertEquals(
      structuralRoot.tacticalWitnessCauseKinds.toSet,
      Set(RelativeCauseKind.TacticalRefutationOfPlayed)
    )
    assertEquals(structuralRoot.punishmentWitnessCauseKinds, Nil)
    assertEquals(
      structuralRoot.contextualTacticalWitnessCauseKinds.toSet,
      Set(
        RelativeCauseKind.TacticalRefutationOfPlayed
      )
    )
    assertEquals(
      secondaryByKind(RelativeCauseKind.TacticalRefutationOfPlayed).narrativeRole,
      MoveJudgmentCauseNarrativeRole.TacticalWitness
    )
    assertEquals(
      secondaryByKind(RelativeCauseKind.TacticalRefutationOfPlayed).witnessBindingLevel,
      MoveJudgmentCauseWitnessBindingLevel.LineContext
    )
    assert(
      !secondaryByKind(RelativeCauseKind.TacticalRefutationOfPlayed).witnessBindingSignals.contains(
        MoveJudgmentCauseWitnessBindingSignal.SharedDirectConsequence
      )
    )
    assertEquals(
      contextByKind(RelativeCauseKind.RecaptureRecoveryWindow).narrativeRole,
      MoveJudgmentCauseNarrativeRole.ContextCause
    )
    assertEquals(
      contextByKind(RelativeCauseKind.DefensiveResource).narrativeRole,
      MoveJudgmentCauseNarrativeRole.ContextCause
    )
    assertEquals(
      contextByKind(RelativeCauseKind.MaterialSwing).narrativeRole,
      MoveJudgmentCauseNarrativeRole.ContextCause
    )
    assertEquals(
      contextByKind(RelativeCauseKind.MaterialSwing).witnessBindingLevel,
      MoveJudgmentCauseWitnessBindingLevel.SameComparisonOnly
    )
    assertEquals(
      contextByKind(RelativeCauseKind.MaterialSwing).witnessBindingSignals,
      List(MoveJudgmentCauseWitnessBindingSignal.SameComparison)
    )

  test("classifies tactical witness as punishment only when object consequence and event line bind to structural root"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4:punishment",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val tacticalRef = evidenceRef(
      id = "tactical-mechanism:played:d2d4:punishment",
      producer = EvidenceProducer.TacticalMechanismProducer,
      layer = EvidenceLayer.TacticalMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      kind = TransitionConsequenceKind.CenterControlGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("d4")
    )
    val structuralProof = RelativeCauseProof(
      directProof = RelativeCauseProofSection(
        role = RelativeCauseProofRole.DirectProof,
        strength = RelativeCauseProofStrength.Primary,
        transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, consequence))
      )
    )
    val tacticalProof = RelativeCauseProof(
      directProof = structuralProof.directProof.copy(
        tacticalMechanisms = List(
          TacticalMechanismProof(
            source = tacticalRef,
            kind = TacticalMechanismKind.Refutation,
            signals = List(
              TacticalMechanismSignal(
                kind = TacticalMechanismSignalKind.LineConsequence,
                label = "refutation-line",
                sourceLayer = EvidenceLayer.TacticalMechanism,
                source = Some(tacticalRef)
              )
            )
          )
        )
      )
    )
    val structuralCause = RelativeCauseFact(
      kind = RelativeCauseKind.StructuralImprovement,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Blunder,
      winPercentLossForMover = 12.0,
      candidateWinPercentDeltaForMover = -12.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(Some(structuralProof))
    val tacticalCause = structuralCause.copy(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      supportEvidence = Nil,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(Some(tacticalProof))
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(structuralRef, StructuralDeltaEvidence(transition, signals = Nil, consequences = List(consequence))),
        EvidenceRecord(
          tacticalRef,
          TacticalMechanismEvidence(
            kind = TacticalMechanismKind.Refutation,
            moveUci = Some("d2d4"),
            line = Some(playedLine),
            signals = List(
              TacticalMechanismSignal(
                kind = TacticalMechanismSignalKind.LineConsequence,
                label = "refutation-line",
                sourceLayer = EvidenceLayer.TacticalMechanism,
                source = Some(tacticalRef)
              )
            )
          )
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:structural-punishment-root",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(structuralCause),
          parents = List(structuralRef)
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:tactical-punishment-witness",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(tacticalCause),
          parents = List(structuralRef)
        )
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val tacticalFrame = view.causeAudit.secondary.find(_.causeKind == RelativeCauseKind.TacticalRefutationOfPlayed).get
    val structuralFrame = view.causeAudit.primary.find(_.causeKind == RelativeCauseKind.StructuralImprovement).get
    assertEquals(view.causeAudit.primary.map(_.causeKind), List(RelativeCauseKind.StructuralImprovement))
    assertEquals(structuralFrame.tacticalWitnessCauseKinds, List(RelativeCauseKind.TacticalRefutationOfPlayed))
    assertEquals(structuralFrame.punishmentWitnessCauseKinds, List(RelativeCauseKind.TacticalRefutationOfPlayed))
    assertEquals(structuralFrame.contextualTacticalWitnessCauseKinds, Nil)
    assertEquals(tacticalFrame.narrativeRole, MoveJudgmentCauseNarrativeRole.TacticalWitness)
    assertEquals(tacticalFrame.witnessBindingLevel, MoveJudgmentCauseWitnessBindingLevel.Punishment)
    assert(tacticalFrame.witnessBindingSignals.contains(MoveJudgmentCauseWitnessBindingSignal.SameEventLine))
    assert(tacticalFrame.witnessBindingSignals.contains(MoveJudgmentCauseWitnessBindingSignal.SharedDirectConsequence))

  test("demotes plan contradiction when a concrete structural root exists in the same played-vs-best loss"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4:activity-loss",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val activityConsequence = TransitionConsequence(
      kind = TransitionConsequenceKind.MobilityLoss,
      polarity = StructuralSignalPolarity.Loss,
      strength = 3,
      subjects = List("d4")
    )
    val planConsequence = TransitionConsequence(
      kind = TransitionConsequenceKind.DevelopmentUnsafePlacement,
      polarity = StructuralSignalPolarity.Loss,
      strength = 1
    )
    val proof = RelativeCauseProof(
      directProof = RelativeCauseProofSection(
        role = RelativeCauseProofRole.DirectProof,
        strength = RelativeCauseProofStrength.Primary,
        transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, activityConsequence))
      )
    )
    val concreteActivityCause = RelativeCauseFact(
      kind = RelativeCauseKind.ActivityLoss,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 9.0,
      candidateWinPercentDeltaForMover = -9.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(Some(proof))
    val planCause = concreteActivityCause.copy(
      kind = RelativeCauseKind.PlanContradiction,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, planConsequence))
          )
        )
      )
    )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(transition = transition, signals = Nil, consequences = List(activityConsequence, planConsequence))
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:activity-loss-root",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(concreteActivityCause),
          parents = List(structuralRef)
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:plan-fallback",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(planCause),
          parents = List(structuralRef)
        )
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val activityFrame = view.causeAudit.primary.find(_.causeKind == RelativeCauseKind.ActivityLoss).get
    val planFrame = view.causeAudit.context.find(_.causeKind == RelativeCauseKind.PlanContradiction).get
    assertEquals(view.causeAudit.primary.map(_.causeKind), List(RelativeCauseKind.ActivityLoss))
    assertEquals(activityFrame.narrativeRole, MoveJudgmentCauseNarrativeRole.RootCause)
    assertEquals(planFrame.narrativeRole, MoveJudgmentCauseNarrativeRole.ContextCause)

  test("keeps tactical root above broad structural root with only generic activity target"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4:broad-activity",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val broadConsequence = TransitionConsequence(
      kind = TransitionConsequenceKind.MobilityLoss,
      polarity = StructuralSignalPolarity.Loss,
      strength = 2,
      subjects = List("activity")
    )
    val broadProof = RelativeCauseProof(
      directProof = RelativeCauseProofSection(
        role = RelativeCauseProofRole.DirectProof,
        strength = RelativeCauseProofStrength.Primary,
        transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, broadConsequence))
      )
    )
    val broadStructuralCause = RelativeCauseFact(
      kind = RelativeCauseKind.ActivityLoss,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Blunder,
      winPercentLossForMover = 14.0,
      candidateWinPercentDeltaForMover = -14.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(Some(broadProof))
    val tacticalCause = broadStructuralCause.copy(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      supportEvidence = Nil,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(None)
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(transition = transition, signals = Nil, consequences = List(broadConsequence))
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:broad-activity-root",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(broadStructuralCause),
          parents = List(structuralRef)
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:concrete-tactical-root",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(tacticalCause)
        )
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val activityFrame = view.causeAudit.secondary.find(_.causeKind == RelativeCauseKind.ActivityLoss).get
    val tacticalFrame = view.causeAudit.primary.find(_.causeKind == RelativeCauseKind.TacticalRefutationOfPlayed).get
    assertEquals(view.causeAudit.primary.map(_.causeKind), List(RelativeCauseKind.TacticalRefutationOfPlayed))
    assertEquals(activityFrame.concreteObjectReady, false)
    assertEquals(activityFrame.narrativeRole, MoveJudgmentCauseNarrativeRole.SupportingCause)
    assertEquals(activityFrame.rootArbitrationTier, MoveJudgmentCauseRootArbitrationTier.BroadOwnedRoot)
    assertEquals(tacticalFrame.narrativeRole, MoveJudgmentCauseNarrativeRole.RootCause)
    assertEquals(tacticalFrame.rootArbitrationTier, MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot)

  test("does not default generic broad structural signal into root cause when no root is selected"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4:broad-activity-only",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val broadConsequence = TransitionConsequence(
      kind = TransitionConsequenceKind.MobilityLoss,
      polarity = StructuralSignalPolarity.Loss,
      strength = 2,
      subjects = List("activity")
    )
    val broadCause = RelativeCauseFact(
      kind = RelativeCauseKind.ActivityLoss,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Blunder,
      winPercentLossForMover = 14.0,
      candidateWinPercentDeltaForMover = -14.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, broadConsequence))
          )
        )
      )
    )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(transition = transition, signals = Nil, consequences = List(broadConsequence))
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:broad-activity-only",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(broadCause),
          parents = List(structuralRef)
        )
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val activityFrame = view.causeAudit.secondary.find(_.causeKind == RelativeCauseKind.ActivityLoss).get
    assertEquals(view.causeAudit.primary, Nil)
    assertEquals(activityFrame.rootArbitrationTier, MoveJudgmentCauseRootArbitrationTier.BroadOwnedRoot)
    assertEquals(activityFrame.narrativeRole, MoveJudgmentCauseNarrativeRole.SupportingCause)

  test("cluster frames do not mark actor-only proof bindings as concrete object ready"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4:actor-only",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:tactical:actor-only-proof",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      kind = TransitionConsequenceKind.CenterControlGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 8.0,
      candidateWinPercentDeltaForMover = -8.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      proof = Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, consequence))
          )
        )
      )
    )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(transition = transition, signals = Nil, consequences = List(consequence))
        ),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(structuralRef))
      )
    )
    val claim = ClaimSeed(
      id = "claim:tactical:actor-only-proof",
      family = ClaimFamily.Tactical,
      idea = None,
      subject = IdeaSubject.PlayedMove,
      primaryPosition = root,
      primaryLine = Some(playedLine),
      subjectMove = Some("d2d4"),
      evidence = List(causeRef),
      engineComparison = None,
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val clusters = ClaimEventCluster.fromClaims(List(claim), graph)
    assertEquals(clusters.size, 1)
    assert(clusters.head.objectBindingSignatures.nonEmpty, clusters.head.objectBindingSignatures)

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = List(claim),
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = clusters
      )
      .get
    val clusterFrame = view.causeAudit.all.find(_.clusterId.contains(clusters.head.id)).get
    assertEquals(clusterFrame.framed, true)
    assertEquals(clusterFrame.objectBindingSignatures.nonEmpty, true)
    assertEquals(clusterFrame.concreteObjectReady, false)

  test("keeps unbound recapture label below tactical event root when plan fallback is suppressed"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4:plan-fallback-with-events",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val planConsequence = TransitionConsequence(
      kind = TransitionConsequenceKind.DevelopmentUnsafePlacement,
      polarity = StructuralSignalPolarity.Loss,
      strength = 1
    )
    val baseCause = RelativeCauseFact(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Blunder,
      winPercentLossForMover = 12.0,
      candidateWinPercentDeltaForMover = -12.0,
      supportEvidence = Nil,
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(None)
    val planCause = baseCause.copy(
      kind = RelativeCauseKind.PlanContradiction,
      supportEvidence = List(structuralRef),
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, planConsequence))
          )
        )
      )
    )
    val tacticalCause = baseCause
    val wrongRecapturerCause = baseCause.copy(kind = RelativeCauseKind.WrongRecapturer)(None)
    val onlyDefenseCause = baseCause.copy(
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      sourceSide = RelativeCauseSourceSide.Reference,
      eventLine = referenceLine,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(None)
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(transition = transition, signals = Nil, consequences = List(planConsequence))
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:event:tactical-refutation",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(tacticalCause)
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:event:wrong-recapturer",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(wrongRecapturerCause)
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:event:only-defense",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(referenceLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(onlyDefenseCause)
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:plan-fallback-with-events",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(planCause)
        )
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val rolesByKind = (view.causeAudit.primary ++ view.causeAudit.secondary ++ view.causeAudit.context).map(frame => frame.causeKind -> frame.narrativeRole).toMap
    assertEquals(view.causeAudit.primary.map(_.causeKind), List(RelativeCauseKind.TacticalRefutationOfPlayed))
    assertEquals(rolesByKind(RelativeCauseKind.TacticalRefutationOfPlayed), MoveJudgmentCauseNarrativeRole.RootCause)
    assertEquals(rolesByKind(RelativeCauseKind.WrongRecapturer), MoveJudgmentCauseNarrativeRole.SupportingCause)
    assertEquals(rolesByKind(RelativeCauseKind.OnlyDefenseNecessity), MoveJudgmentCauseNarrativeRole.SupportingCause)
    assertEquals(rolesByKind(RelativeCauseKind.PlanContradiction), MoveJudgmentCauseNarrativeRole.SupportingCause)

  test("does not promote unbound wrong recapturer over plan fallback"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4:plan-vs-unbound-recapture",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val planContrastRef = evidenceRef(
      id = "strategic-contrast:played:d2d4:plan-vs-unbound-recapture",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val planConsequence = TransitionConsequence(
      kind = TransitionConsequenceKind.DevelopmentUnsafePlacement,
      polarity = StructuralSignalPolarity.Loss,
      strength = 1
    )
    val planAxis = StrategicAxisDetail(StrategicAxisKind.PlanCoherence, StrategicAxisPolarity.Support, "OpeningDevelopment")
    val planAxisComparison = StrategicAxisComparison(
      axis = planAxis,
      outcome = StrategicAxisComparisonOutcome.ReferenceOnly,
      referenceStrength = 3,
      candidateStrength = 0,
      referenceSources = List(structuralRef),
      candidateSources = Nil
    )
    val planSustainability = StrategicSustainabilityAssessment(
      horizon = StrategicSustainabilityHorizon.MediumPv,
      lineMaintained = true,
      pvMaintained = true,
      referencePlyCount = 6,
      candidatePlyCount = 3
    )
    val planContrast = StrategicMechanismContrastEvidence(
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      axisComparisons = List(planAxisComparison),
      planComparison = Some(
        StrategicPlanComparison(
          referencePlanIds = List("OpeningDevelopment"),
          candidatePlanIds = Nil,
          outcome = StrategicAxisComparisonOutcome.ReferenceOnly
        )
      ),
      sustainability = planSustainability,
      support = StrategicContrastSupport(directSources = List(structuralRef), contrastSources = Nil, contextSources = Nil)
    )
    val planCause = RelativeCauseFact(
      kind = RelativeCauseKind.PlanContradiction,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 8.0,
      candidateWinPercentDeltaForMover = -8.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            strategicMechanismContrasts = List(
              StrategicMechanismContrastProof(
                source = planContrastRef,
                comparisonKind = CandidateComparisonKind.PlayedVsBest,
                referenceLine = referenceLine,
                candidateLine = playedLine,
                axisComparisons = List(planAxisComparison),
                sustainability = planSustainability
              )
            ),
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, planConsequence))
          )
        )
      )
    )
    val wrongRecapturerCause = planCause.copy(
      kind = RelativeCauseKind.WrongRecapturer,
      supportEvidence = Nil,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(None)
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(transition = transition, signals = Nil, consequences = List(planConsequence))
        ),
        EvidenceRecord(planContrastRef, planContrast, parents = List(structuralRef)),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:plan-fallback-vs-unbound-recapture",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(planCause),
          parents = List(structuralRef)
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:unbound-wrong-recapturer",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(wrongRecapturerCause)
        )
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val rolesByKind = (view.causeAudit.primary ++ view.causeAudit.secondary ++ view.causeAudit.context).map(frame => frame.causeKind -> frame.narrativeRole).toMap
    val tiersByKind = (view.causeAudit.primary ++ view.causeAudit.secondary ++ view.causeAudit.context).map(frame => frame.causeKind -> frame.rootArbitrationTier).toMap
    assertEquals(view.causeAudit.primary.map(_.causeKind), List(RelativeCauseKind.PlanContradiction))
    assertEquals(rolesByKind(RelativeCauseKind.PlanContradiction), MoveJudgmentCauseNarrativeRole.RootCause)
    assertEquals(rolesByKind(RelativeCauseKind.WrongRecapturer), MoveJudgmentCauseNarrativeRole.SupportingCause)
    assertEquals(tiersByKind(RelativeCauseKind.PlanContradiction), MoveJudgmentCauseRootArbitrationTier.FallbackRoot)
    assertEquals(tiersByKind(RelativeCauseKind.WrongRecapturer), MoveJudgmentCauseRootArbitrationTier.ContextOnly)

  test("does not promote plan contradiction fallback without plan coherence proof"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4:plan-fallback-without-plan-axis",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val planConsequence = TransitionConsequence(
      kind = TransitionConsequenceKind.DevelopmentUnsafePlacement,
      polarity = StructuralSignalPolarity.Loss,
      strength = 1,
      subjects = List("plan-switch")
    )
    val planCause = RelativeCauseFact(
      kind = RelativeCauseKind.PlanContradiction,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 8.0,
      candidateWinPercentDeltaForMover = -8.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, planConsequence))
          )
        )
      )
    )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(transition = transition, signals = Nil, consequences = List(planConsequence))
        ),
        EvidenceRecord(
          evidenceRef(
            id = "relative-cause:played-best:plan-fallback-without-plan-axis",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeCause,
            position = root,
            line = Some(playedLine),
            scope = EvidenceScope.Counterfactual
          ),
          RelativeCauseFactEvidence(planCause),
          parents = List(structuralRef)
        )
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val planFrame = view.causeAudit.secondary.find(_.causeKind == RelativeCauseKind.PlanContradiction).get
    assertEquals(view.causeAudit.primary, Nil)
    assertEquals(planFrame.narrativeRole, MoveJudgmentCauseNarrativeRole.SupportingCause)
    assertEquals(planFrame.rootArbitrationTier, MoveJudgmentCauseRootArbitrationTier.ContextOnly)

  test("cause audit demotes weak cause frames when concrete same-comparison peers exist"):
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val otherLine = LineNodeRef("other-line", "c2c4", 3, LineNodeRole.Alternative)

    def frame(
        id: String,
        kind: RelativeCauseKind,
        role: MoveJudgmentCauseFrameRole,
        tier: MoveJudgmentCauseRootArbitrationTier,
        concrete: Boolean,
        narrativeRole: MoveJudgmentCauseNarrativeRole,
        candidateLine: LineNodeRef = playedLine
    ): MoveJudgmentCauseFrame =
      MoveJudgmentCauseFrame(
        role = role,
        clusterId = None,
        framed = true,
        causeEvidenceIds = List(id),
        causeKind = kind,
        comparisonKind = CandidateComparisonKind.PlayedVsBest,
        causeRole = RelativeCauseRole.PrimaryPlayedCause,
        causeSourceSide = RelativeCauseSourceSide.Candidate,
        causeImportance = RelativeCauseImportance.Primary,
        attributionKind = CauseAttributionKind.CandidateAllowsLiability,
        attributionRootMoveMatched = true,
        attributionDirectProofEligible = true,
        referenceLine = referenceLine,
        candidateLine = candidateLine,
        eventLine = candidateLine,
        eventRootMove = candidateLine.rootMove,
        causeClaimIds = Nil,
        evaluationClaimIds = Nil,
        witnessClaimIds = Nil,
        ideaIds = Nil,
        supportIdeaIds = Nil,
        claimCandidateIds = Nil,
        finalClaimIds = Nil,
        relatedSupportClusterIds = Nil,
        evidenceIds = Nil,
        proofDirectSourceIds = Nil,
        proofContrastSourceIds = Nil,
        proofContextSupportSourceIds = Nil,
        proofStrategicAxisLineage = Nil,
        proofStrategicAxisKeys = Nil,
        proofStrategicMechanismKinds = Nil,
        proofStrategicMechanismSourceIds = Nil,
        proofStrategicMechanismSignalSourceIds = Nil,
        supportEvidenceSourceIds = Nil,
        objectBindingSignatures =
          if concrete then List("target=Square:d4|mechanism=Mechanism:activity|consequence=Consequence:activity")
          else Nil,
        concreteObjectReady = concrete,
        narrativeRole = narrativeRole,
        rootArbitrationTier = tier
      )

    val concreteRoot =
      frame(
        "concrete-root",
        RelativeCauseKind.PawnBreakOpportunity,
        MoveJudgmentCauseFrameRole.PrimaryCause,
        MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot,
        concrete = true,
        narrativeRole = MoveJudgmentCauseNarrativeRole.RootCause
      )
    val objectlessPrimary =
      frame(
        "objectless-primary",
        RelativeCauseKind.DefensiveResource,
        MoveJudgmentCauseFrameRole.PrimaryCause,
        MoveJudgmentCauseRootArbitrationTier.ContextOnly,
        concrete = false,
        narrativeRole = MoveJudgmentCauseNarrativeRole.SupportingCause
      )
    val objectlessSecondary =
      frame(
        "objectless-secondary",
        RelativeCauseKind.CandidateTacticalLiability,
        MoveJudgmentCauseFrameRole.SecondaryCause,
        MoveJudgmentCauseRootArbitrationTier.ContextOnly,
        concrete = false,
        narrativeRole = MoveJudgmentCauseNarrativeRole.SupportingCause
      )
    val concreteContextOnly =
      frame(
        "concrete-context-only",
        RelativeCauseKind.DefensiveResource,
        MoveJudgmentCauseFrameRole.PrimaryCause,
        MoveJudgmentCauseRootArbitrationTier.ContextOnly,
        concrete = true,
        narrativeRole = MoveJudgmentCauseNarrativeRole.SupportingCause
      )
    val concreteFallback =
      frame(
        "concrete-fallback",
        RelativeCauseKind.PlanContradiction,
        MoveJudgmentCauseFrameRole.PrimaryCause,
        MoveJudgmentCauseRootArbitrationTier.FallbackRoot,
        concrete = true,
        narrativeRole = MoveJudgmentCauseNarrativeRole.RootCause
      )
    val concreteBroad =
      frame(
        "concrete-broad",
        RelativeCauseKind.ActivityGain,
        MoveJudgmentCauseFrameRole.SecondaryCause,
        MoveJudgmentCauseRootArbitrationTier.BroadOwnedRoot,
        concrete = true,
        narrativeRole = MoveJudgmentCauseNarrativeRole.SupportingCause
      )
    val objectlessTacticalRoot =
      frame(
        "objectless-tactical-root",
        RelativeCauseKind.TacticalRefutationOfPlayed,
        MoveJudgmentCauseFrameRole.PrimaryCause,
        MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot,
        concrete = false,
        narrativeRole = MoveJudgmentCauseNarrativeRole.RootCause
      )
    val objectlessWithoutConcretePeer =
      frame(
        "objectless-other-comparison",
        RelativeCauseKind.DefensiveResource,
        MoveJudgmentCauseFrameRole.SecondaryCause,
        MoveJudgmentCauseRootArbitrationTier.ContextOnly,
        concrete = false,
        narrativeRole = MoveJudgmentCauseNarrativeRole.SupportingCause,
        candidateLine = otherLine
      )

    val audit = MoveJudgmentView.causeAuditBuckets(
      List(
        concreteRoot,
        objectlessPrimary,
        objectlessSecondary,
        concreteContextOnly,
        concreteFallback,
        concreteBroad,
        objectlessTacticalRoot,
        objectlessWithoutConcretePeer
      )
    )

    assertEquals(audit.primary.map(_.causeEvidenceIds.head), List("concrete-root", "objectless-tactical-root"))
    assertEquals(audit.secondary.map(_.causeEvidenceIds.head), List("objectless-other-comparison"))
    assertEquals(
      audit.context.map(frame => frame.causeEvidenceIds.head -> frame.role).toSet,
      Set(
        "objectless-primary" -> MoveJudgmentCauseFrameRole.ContextCause,
        "objectless-secondary" -> MoveJudgmentCauseFrameRole.ContextCause,
        "concrete-context-only" -> MoveJudgmentCauseFrameRole.ContextCause,
        "concrete-fallback" -> MoveJudgmentCauseFrameRole.ContextCause,
        "concrete-broad" -> MoveJudgmentCauseFrameRole.ContextCause
      )
    )

  test("links pawn break tension proof to an owned move meaning claim"):
    val root = PositionNodeRef("4k3/8/8/3p4/8/8/4P3/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterReference = PositionNodeRef("4k3/8/8/3p4/4P3/8/8/4K3 b - - 0 1", 2, Some(Color.Black), Some("after-reference"))
    val referenceLine = LineNodeRef("reference-line", "e2e4", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "h2h3", 2, LineNodeRole.Played)
    val structuralRef = evidenceRef(
      id = "structural-delta:reference:e2e4:tension",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.ReferenceTransition
    )
    val contrastRef = evidenceRef(
      id = "strategic-contrast:reference:e2e4:tension",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:pawn-break-tension",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val referenceLineEvidence = evidenceRef(
      id = "line:reference:e2e4",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.BestLine
    )
    val playedLineEvidence = evidenceRef(
      id = "line:played:h2h3",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedLine
    )
    val playedTransitionEvidence = evidenceRef(
      id = "transition:played:h2h3",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val relativeAssessmentEvidence = evidenceRef(
      id = "relative-assessment:played-best:h2h3",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeAssessment,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val played = MoveTransitionEdge(
      role = TransitionEdgeRole.Played,
      from = root,
      moveUci = "h2h3",
      to = root,
      planTransition = None,
      evidence = playedTransitionEvidence
    )
    val reference = CandidateLineNode(
      ref = referenceLine,
      line = VariationLine(List("e2e4"), scoreCp = 30, depth = 16),
      evidence = referenceLineEvidence
    )
    val candidate = CandidateLineNode(
      ref = playedLine,
      line = VariationLine(List("h2h3"), scoreCp = -50, depth = 16),
      evidence = playedLineEvidence
    )
    val transition = StructuralTransitionBinding(
      moveUci = "e2e4",
      role = TransitionEdgeRole.Reference,
      from = root,
      to = afterReference,
      line = Some(referenceLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      kind = TransitionConsequenceKind.PawnTensionGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("break-file:e", "created-tension:e4-d5")
    )
    val structuralDelta = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(consequence)
    )
    val axis = StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "break-file-e-maintain-e4-d5")
    val axisComparison = StrategicAxisComparison(
      axis = axis,
      outcome = StrategicAxisComparisonOutcome.ReferenceOnly,
      referenceStrength = 3,
      candidateStrength = 0,
      referenceSources = List(structuralRef),
      candidateSources = Nil
    )
    val sustainability = StrategicSustainabilityAssessment(
      horizon = StrategicSustainabilityHorizon.ShortPv,
      lineMaintained = true,
      pvMaintained = true,
      referencePlyCount = 3,
      candidatePlyCount = 3
    )
    val contrast = StrategicMechanismContrastEvidence(
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      axisComparisons = List(axisComparison),
      planComparison = None,
      sustainability = sustainability,
      support = StrategicContrastSupport(directSources = List(structuralRef), contrastSources = Nil, contextSources = Nil)
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.PawnBreakOpportunity,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 8.0,
      candidateWinPercentDeltaForMover = -8.0,
      supportEvidence = List(contrastRef, structuralRef),
      evidenceLines = List(referenceLine, playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = referenceLine,
      sourceSide = RelativeCauseSourceSide.Reference,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.ReferenceCreatesResource,
        ownedEvidence = List(contrastRef, structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            strategicMechanismContrasts = List(
              StrategicMechanismContrastProof(
                source = contrastRef,
                comparisonKind = CandidateComparisonKind.PlayedVsBest,
                referenceLine = referenceLine,
                candidateLine = playedLine,
                axisComparisons = List(axisComparison),
                sustainability = sustainability
              )
            ),
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, consequence))
          )
        )
      )
    )
    val assessment = RelativeMoveAssessment(
      played = played,
      referenceTransition = None,
      reference = reference,
      candidate = candidate,
      comparison = EvalComparison(
        mover = Color.White,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        rawCandidateDeltaCpForDiagnostics = -80,
        candidateWinPercentDeltaForMover = -8.0,
        rawCpLossForDiagnostics = 80,
        winPercentLossForMover = 8.0,
        verdict = MoveChoiceVerdict.Mistake
      ),
      confidence = EvidenceConfidence.EngineBacked,
      evidence = relativeAssessmentEvidence,
      counterfactualEvidence = Nil,
      relativeCauseEvidence = List(causeRef)
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = List(assessment),
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(structuralRef, structuralDelta),
            EvidenceRecord(contrastRef, contrast, parents = List(structuralRef)),
            EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(contrastRef, structuralRef))
          )
        ),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    val detail = view.positionPlanTechniqueFrames.flatMap(_.semanticDetails).find(_.axisKey.contains(axis.stableKey)).get
    assertEquals(detail.unit, PositionPlanTechniqueUnit.TensionBreakPolicyRoute)
    assertEquals(detail.structuralPurposeConsequences, List("PawnTensionGain"))
    assertEquals(detail.structuralPurposeSubjects, List("break-file:e", "created-tension:e4-d5"))
    assertEquals(detail.causeEvidenceIds, List(causeRef.id))
    assert(detail.proofRoles.contains(RelativeCauseProofRole.DirectProof), detail.proofRoles)
    val claim = view.moveMeaningClaims.find(_.meaningKind == "PawnBreakTiming").get
    assertEquals(claim.role, "PreparesBreak")
    assertEquals(claim.supportLevel, "owned_cause_linked")
    assertEquals(claim.visibility, "reason_grade")
    assertEquals(claim.surfaceLane, "reference_or_opponent_resource")
    assertEquals(claim.lineRole, "reference")
    assertEquals(claim.moveUci, "e2e4")
    assertEquals(claim.causeEvidenceIds, List(causeRef.id))
    assertEquals(claim.breakFiles, List("e"))
    assert(claim.targetFiles.contains("e"), claim.targetFiles)
    assert(claim.targetSquares.contains("e4"), claim.targetSquares)
    assert(claim.targetSquares.contains("d5"), claim.targetSquares)

  test("keeps pure tactical played blunder as root when no proved structural root exists"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val tacticalCauseRef = evidenceRef(
      id = "relative-cause:played-best:pure-tactical",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val tacticalCause = RelativeCauseFact(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Blunder,
      winPercentLossForMover = 12.0,
      candidateWinPercentDeltaForMover = -12.0,
      supportEvidence = Nil,
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(None)

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(List(EvidenceRecord(tacticalCauseRef, RelativeCauseFactEvidence(tacticalCause)))),
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    assertEquals(view.causeAudit.primary.map(_.causeKind), List(RelativeCauseKind.TacticalRefutationOfPlayed))
    assertEquals(view.causeAudit.primary.head.narrativeRole, MoveJudgmentCauseNarrativeRole.RootCause)
    assertEquals(view.causeAudit.primary.head.rootArbitrationTier, MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot)

  test("draw resource proof outranks exact long-term roots as a terminal result"):
    val playedLine = LineNodeRef("played-line", "h6a6", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "h6a6", 1, LineNodeRole.BestReference)
    def frame(
        id: String,
        kind: RelativeCauseKind,
        hasLongTermProof: Boolean,
        proofLineConsequences: List[LineConsequenceKind] = Nil
    ): MoveJudgmentCauseFrame =
      MoveJudgmentCauseFrame(
        role = MoveJudgmentCauseFrameRole.PrimaryCause,
        clusterId = None,
        framed = true,
        causeEvidenceIds = List(id),
        causeKind = kind,
        comparisonKind = CandidateComparisonKind.PlayedVsBest,
        causeRole = RelativeCauseRole.PrimaryPlayedCause,
        causeSourceSide = RelativeCauseSourceSide.Candidate,
        causeImportance = RelativeCauseImportance.Primary,
        attributionKind = CauseAttributionKind.CandidateCreatesValue,
        attributionRootMoveMatched = true,
        attributionDirectProofEligible = true,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        eventLine = playedLine,
        eventRootMove = playedLine.rootMove,
        causeClaimIds = Nil,
        evaluationClaimIds = Nil,
        witnessClaimIds = Nil,
        ideaIds = Nil,
        supportIdeaIds = Nil,
        claimCandidateIds = Nil,
        finalClaimIds = Nil,
        relatedSupportClusterIds = Nil,
        evidenceIds = Nil,
        proofDirectSourceIds = List(s"line:$id"),
        proofContrastSourceIds = Nil,
        proofContextSupportSourceIds = Nil,
        proofStrategicAxisLineage = Nil,
        proofStrategicAxisKeys = Nil,
        proofStrategicMechanismKinds = Nil,
        proofStrategicMechanismSourceIds = Nil,
        proofStrategicMechanismSignalSourceIds = Nil,
        supportEvidenceSourceIds = Nil,
        proofLineConsequences = proofLineConsequences,
        objectBindingSignatures = List(
          s"actor=Move:${playedLine.rootMove}|target=Square:a6|mechanism=Mechanism:proof|consequence=Consequence:proof|proof=DirectProof"
        ),
        concreteObjectReady = true,
        hasOwnedAdmissibleLongTermProof = hasLongTermProof
      )
    val exactStructural = frame("exact-structural", RelativeCauseKind.StructuralImprovement, hasLongTermProof = true)
    val drawResource =
      frame("draw-resource", RelativeCauseKind.DrawResource, hasLongTermProof = false, List(LineConsequenceKind.DrawResource))

    val narrated = MoveJudgmentCauseNarrativeProjection.withNarrativeRoles(TypedEvidenceGraph.empty, List(exactStructural, drawResource))

    assertEquals(
      narrated.filter(_.narrativeRole == MoveJudgmentCauseNarrativeRole.RootCause).map(_.causeKind),
      List(RelativeCauseKind.DrawResource)
    )
    assertEquals(
      narrated.find(_.causeKind == RelativeCauseKind.StructuralImprovement).map(_.narrativeRole),
      Some(MoveJudgmentCauseNarrativeRole.SupportingCause)
    )

  test("does not demote tactical root when structural cause lacks owned admissible proof"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4:weak",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      kind = TransitionConsequenceKind.CenterControlGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2
    )
    val structuralCause = RelativeCauseFact(
      kind = RelativeCauseKind.StructuralImprovement,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Blunder,
      winPercentLossForMover = 12.0,
      candidateWinPercentDeltaForMover = -12.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = false
      )
    )(None)
    val structuralCauseRef = evidenceRef(
      id = "relative-cause:played-best:weak-structural",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val tacticalCauseRef = evidenceRef(
      id = "relative-cause:played-best:tactical-with-weak-structural",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val tacticalCause = structuralCause.copy(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      supportEvidence = Nil,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(None)
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(transition = transition, signals = Nil, consequences = List(consequence))
        ),
        EvidenceRecord(structuralCauseRef, RelativeCauseFactEvidence(structuralCause), parents = List(structuralRef)),
        EvidenceRecord(tacticalCauseRef, RelativeCauseFactEvidence(tacticalCause))
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = Nil,
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get

    assertEquals(view.causeAudit.primary.map(_.causeKind), List(RelativeCauseKind.TacticalRefutationOfPlayed))
    assertEquals(view.causeAudit.primary.head.narrativeRole, MoveJudgmentCauseNarrativeRole.RootCause)

  test("binds playable loss played-vs-best evidence as context instead of primary cause"):
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val playedMoves = Set("d2d4")
    val playableComparison = CandidateComparisonFact(
      kind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      comparison = EvalComparison(
        mover = Color.White,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        rawCandidateDeltaCpForDiagnostics = -10,
        candidateWinPercentDeltaForMover = 0.0,
        rawCpLossForDiagnostics = 10,
        winPercentLossForMover = 2.5,
        verdict = MoveChoiceVerdict.PlayableLoss
      )
    )
    val inaccuracyComparison = playableComparison.copy(
      comparison = playableComparison.comparison.copy(
        winPercentLossForMover = 4.0,
        verdict = MoveChoiceVerdict.Inaccuracy
      )
    )
    val playableCause = RelativeCauseFact(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.PlayableLoss,
      winPercentLossForMover = 2.5,
      candidateWinPercentDeltaForMover = 0.0,
      supportEvidence = Nil,
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary
    )()

    assertEquals(JudgmentSubjectBinding.comparisonBinding(playableComparison, playedMoves), SubjectBindingClass.ContextPlayed)
    assertEquals(JudgmentSubjectBinding.comparisonBinding(inaccuracyComparison, playedMoves), SubjectBindingClass.PrimaryPlayedCause)
    assertEquals(JudgmentSubjectBinding.relativeCauseBinding(playableCause, playedMoves), SubjectBindingClass.ContextPlayed)

  test("rook technique horizon detail survives into move meaning claims"):
    val root = PositionNodeRef("8/3P1K2/4k3/8/3R4/8/8/2r5 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("3Q4/5K2/4k3/8/3R4/8/8/2r5 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d7d8q", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "d7d8q", 1, LineNodeRole.BestReference)
    val lineEvidenceRef = evidenceRef(
      id = "line:lucena-horizon",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:lucena-horizon",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val relativeAssessmentEvidence = evidenceRef(
      id = "relative-assessment:lucena-horizon",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeAssessment,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val transitionEvidence = evidenceRef(
      id = "move-transition:played:d7d8q",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val horizon = LineEndgameTechniqueHorizon(
      pattern = "Lucena",
      rookPattern = "RookBehindPassedPawn",
      techniqueSide = Color.White,
      entryPlyOffset = 0,
      terminalPlyOffset = 2,
      status = LineEndgameTechniqueHorizonStatus.Transitioned,
      triggerMove = Some("d7d8q"),
      requiredSquares = List("d7", "d8", "e7"),
      maintainedSquares = List("d7", "d8", "e7")
    )
    val linePayload =
      new LineFactEvidence(playedLine, Some("d7d8q"), None, Nil, None, None)(
        endgameHorizons = List(horizon)
      )
    val played = MoveTransitionEdge(
      role = TransitionEdgeRole.Played,
      from = root,
      moveUci = "d7d8q",
      to = afterPlayed,
      planTransition = None,
      evidence = transitionEvidence
    )
    val reference = CandidateLineNode(
      ref = referenceLine,
      line = VariationLine(List("d7d8q"), scoreCp = 100, depth = 16),
      evidence = lineEvidenceRef
    )
    val candidate = CandidateLineNode(
      ref = playedLine,
      line = VariationLine(List("d7d8q"), scoreCp = 100, depth = 16),
      evidence = lineEvidenceRef
    )
    val assessment = RelativeMoveAssessment(
      played = played,
      referenceTransition = None,
      reference = reference,
      candidate = candidate,
      comparison = EvalComparison(
        mover = Color.White,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        rawCandidateDeltaCpForDiagnostics = 0,
        candidateWinPercentDeltaForMover = 0.0,
        rawCpLossForDiagnostics = 0,
        winPercentLossForMover = 0.0,
        verdict = MoveChoiceVerdict.MatchesReference
      ),
      confidence = EvidenceConfidence.EngineBacked,
      evidence = relativeAssessmentEvidence,
      counterfactualEvidence = Nil,
      relativeCauseEvidence = List(causeRef)
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.ConversionSecured,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.MatchesReference,
      winPercentLossForMover = 0.0,
      candidateWinPercentDeltaForMover = 0.0,
      supportEvidence = List(lineEvidenceRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(lineEvidenceRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            lineConsequences = List(
              LineConsequenceProof(
                source = lineEvidenceRef,
                kind = LineConsequenceKind.ForcedTheme,
                eventMove = Some("d7d8q"),
                lineMoves = List("d7d8q")
              )
            )
          )
        )
      )
    )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(lineEvidenceRef, linePayload),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(lineEvidenceRef))
      )
    )

    val view = MoveJudgmentView
      .from(
        relativeAssessments = List(assessment),
        evidenceGraph = graph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get
    val claim = view.moveMeaningClaims
      .find(claim => claim.meaningKind == "TechniqueConversion" && claim.endgameTechniquePattern.contains("Lucena"))
      .getOrElse(fail(view.moveMeaningClaims.toString))

    assertEquals(claim.role, "ReachesTechnique")
    assertEquals(claim.supportLevel, "owned_cause_linked")
    assertEquals(claim.endgameTechniqueHorizonStatus, Some("Transitioned"))
    assertEquals(claim.endgameTechniqueTriggerMove, Some("d7d8q"))
    assert(claim.sourceEvidenceIds.contains("line:lucena-horizon"), claim.sourceEvidenceIds)
    assert(claim.requiredSquares.contains("d8"), claim.requiredSquares)
    assert(claim.maintainedSquares.contains("d8"), claim.maintainedSquares)
    val surfaceTechnique = MoveMeaningSurface.from(view).flatMap(_.endgameTechnique).headOption.getOrElse(fail(MoveMeaningSurface.from(view).toString))
    assertEquals(surfaceTechnique.pattern, Some("lucena"))
    assertEquals(surfaceTechnique.patternLabel, Some("Lucena"))
    assertEquals(surfaceTechnique.rookPattern, Some("rook_behind_passed_pawn"))
    assertEquals(surfaceTechnique.rookPatternLabel, Some("rook behind passed pawn"))
    assertEquals(surfaceTechnique.horizonStatus, Some("converted"))
    assertEquals(surfaceTechnique.triggerMove, Some("d7d8q"))
    assert(surfaceTechnique.requiredSquares.contains("d8"), surfaceTechnique.requiredSquares)
    assert(surfaceTechnique.maintainedSquares.contains("d8"), surfaceTechnique.maintainedSquares)

    val mate = LineConsequence(
      LineConsequenceKind.Mate,
      List("d7d8q", "e6e7", "d8e7"),
      proofSignal = true,
      eventMove = Some("d7d8q")
    )
    val overriddenHorizon =
      horizon.copy(
        status = LineEndgameTechniqueHorizonStatus.SupersededByTactic,
        terminalConsequenceKinds = List(LineConsequenceKind.Mate),
        failureReason = Some("terminal-proof-supersedes-technique")
      )
    val overriddenPayload =
      new LineFactEvidence(playedLine, Some("d7d8q"), None, Nil, None, None)(
        consequences = List(mate),
        endgameHorizons = List(overriddenHorizon)
      )
    val overriddenGraph = TypedEvidenceGraph(
      List(
        EvidenceRecord(lineEvidenceRef, overriddenPayload),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(lineEvidenceRef))
      )
    )
    val overriddenView = MoveJudgmentView
      .from(
        relativeAssessments = List(assessment),
        evidenceGraph = overriddenGraph,
        ideas = Nil,
        claims = Nil,
        claimLifecycle = Nil,
        ideaVerdict = None,
        claimSupportClusters = Nil,
        claimEventClusters = Nil
      )
      .get
    val overriddenDetail = overriddenView.positionPlanTechniqueFrames
      .flatMap(_.semanticDetails)
      .find(detail => detail.endgameTechniquePattern.contains("Lucena") && detail.endgameTechniqueHorizonStatus.contains("SupersededByTactic"))
      .getOrElse(fail(overriddenView.positionPlanTechniqueFrames.toString))

    assertEquals(overriddenDetail.causeEvidenceIds, Nil)
    assertEquals(overriddenDetail.proofRoles, Nil)
    assertEquals(overriddenDetail.terminalConsequenceKinds, List("Mate"))
    assert(!overriddenView.moveMeaningClaims.exists(_.supportLevel == "owned_cause_linked"))
    assert(!overriddenView.moveMeaningClaims.exists(_.causeEvidenceIds.nonEmpty))
    val terminalClaim = overriddenView.moveMeaningClaims
      .find(claim =>
        claim.terminalConsequenceKinds.contains("Mate") &&
          claim.supportLevel == "view_surfaced" &&
          claim.surfaceLane == "current_move_function"
      )
      .getOrElse(fail(overriddenView.moveMeaningClaims.toString))
    assertEquals(terminalClaim.meaningKind, "TerminalProof")
    assertEquals(terminalClaim.role, "ProvesMate")
    assertEquals(terminalClaim.supportLevel, "view_surfaced")
    assertEquals(terminalClaim.surfaceLane, "current_move_function")
    assertEquals(MoveMeaningSurface.from(overriddenView).map(_.ideaType), List("terminal_mate"))

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = producer,
      layer = layer,
      position = position,
      line = line,
      scope = scope,
      confidence = EvidenceConfidence.EngineBacked
    )
