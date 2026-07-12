package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.analysis.position.PositionFactNormalizer
import lila.chessjudgment.model.{ Fact, FactScope }
import lila.chessjudgment.model.strategic.VariationLine

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

    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(structuralRef, structuralDelta),
        EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef)),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(planCause), parents = List(mechanismRef))
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

    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(structuralRef, structuralDelta),
        EvidenceRecord(mechanismRef, mechanism, parents = List(structuralRef)),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(planCause), parents = List(mechanismRef))
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
          signature.contains("mechanism=Mechanism:batterypressuregain")
      ),
      detail.objectBindingSignatures
    )
    val causeFrame = MoveJudgmentCauseFrame(
      role = MoveJudgmentCauseFrameRole.PrimaryCause,
      clusterId = None,
      framed = false,
      causeEvidenceIds = List(causeRef.id),
      causeKind = RelativeCauseKind.PlanImprovement,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      causeRole = RelativeCauseRole.PrimaryPlayedCause,
      causeSourceSide = RelativeCauseSourceSide.Candidate,
      causeImportance = RelativeCauseImportance.Primary,
      attributionKind = CauseAttributionKind.CandidateCreatesValue,
      attributionRootMoveMatched = true,
      attributionDirectProofEligible = true,
      referenceLine = planCause.referenceLine,
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
      evidenceIds = List(causeRef.id),
      proofDirectSourceIds = List(mechanismRef.id),
      proofContrastSourceIds = Nil,
      proofContextSupportSourceIds = Nil,
      proofStrategicAxisLineage = Nil,
      proofStrategicAxisKeys = Nil,
      proofStrategicMechanismKinds = List(StrategicMechanismKind.Activity),
      proofStrategicMechanismSourceIds = List(mechanismRef.id),
      proofStrategicMechanismSignalSourceIds = List(structuralRef.id),
      supportEvidenceSourceIds = List(mechanismRef.id),
      objectBindingSignatures = Nil,
      concreteObjectReady = true,
      hasOwnedAdmissibleLongTermProof = true,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.FallbackRoot
    )
    val planDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      principalPlanId = Some(lila.chessjudgment.model.PlanId.QueensideAttack)
    )
    val exactPlanFrame = causeFrame.copy(
      objectBindingSignatures = List("actor=Move:h1h6|target=PlanSubject:queensideattack|proof=DirectProof")
    )
    val wrongPlanFrame = exactPlanFrame.copy(
      objectBindingSignatures = List("actor=Move:h1h6|target=PlanSubject:kingsideattack|proof=DirectProof")
    )
    assert(MoveMeaningClaim.planCauseFrameOwnsDetailPlan(exactPlanFrame, planDetail))
    assert(!MoveMeaningClaim.planCauseFrameOwnsDetailPlan(wrongPlanFrame, planDetail))

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
    val (tacticalProof, tacticalProofRecord) =
      ownedTacticalProof("line-bound-witness", root, playedLine)
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
    )(Some(tacticalProof))
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
        tacticalProofRecord,
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
    val (tacticalProof, tacticalProofRecord) =
      ownedTacticalProof("broad-structural-peer", root, playedLine)
    val tacticalCause = broadStructuralCause.copy(
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
        tacticalProofRecord,
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
    val (tacticalProof, tacticalProofRecord) =
      ownedTacticalProof("plan-fallback-peer", root, playedLine)
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
    )(Some(tacticalProof))
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
        tacticalProofRecord,
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

  test("does not promote a tactical category without owned tactical proof"):
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

    assertEquals(view.causeAudit.primary, Nil)
    assert(!view.causeAudit.all.exists(frame =>
      frame.causeKind == RelativeCauseKind.TacticalRefutationOfPlayed &&
        (frame.narrativeRole == MoveJudgmentCauseNarrativeRole.RootCause ||
          frame.rootArbitrationTier == MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot)
    ), view.causeAudit)

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
        hasOwnedAdmissibleLongTermProof = hasLongTermProof,
        hasOwnedTacticalProof = proofLineConsequences.exists(LineConsequenceKind.tacticalDriver)
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
    val (tacticalProof, tacticalProofRecord) =
      ownedTacticalProof("weak-structural-peer", root, playedLine)
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
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(transition = transition, signals = Nil, consequences = List(consequence))
        ),
        tacticalProofRecord,
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

  test("future episode results do not redefine the root move function"):
    val routeEvent = PlanEventPublicProof(
      goalTheme = "PieceImprovement",
      goalKind = None,
      moveRole = None,
      transitionType = None,
      rootMove = "e2e3",
      actorRole = Some("knight"),
      actorFrom = Some("e2"),
      actorTo = Some("e3"),
      targets = Nil,
      developmentChoices = Nil,
      results = List(
        PlanEventPublicResult(
          stage = "future",
          kind = TransitionConsequenceKind.MobilityGain,
          polarity = StructuralSignalPolarity.Gain,
          strength = 1,
          subjects = Nil
        )
      ),
      responses = Nil,
      futureCausality = None
    )
    val claim = MoveMeaningClaim(
      meaningKind = "PlanContinuity",
      role = "DevelopsPieceForPlan",
      laneKey = "plan-continuity",
      conflictKey = None,
      supportLevel = "owned_cause_linked",
      visibility = "functional_explanation",
      surfaceLane = "current_move_function",
      lineRole = "candidate",
      moveUci = "e2e3",
      frameId = "plan-frame",
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      axisKey = None,
      axisKind = None,
      axisPolarity = None,
      label = None,
      causeKinds = Nil,
      causeSourceSides = Nil,
      causeEvidenceIds = Nil,
      sourceEvidenceIds = Nil,
      objectBindingSignatures = Nil,
      principalPlanEvent = Some(routeEvent)
    )

    assertEquals(MoveMeaningClaim.principalPlanFunctionType(claim), None)
    assertEquals(
      MoveMeaningClaim.principalPlanFunctionType(
        claim.copy(principalPlanEvent = Some(routeEvent.copy(results = routeEvent.results.map(_.copy(stage = "direct")))))
      ),
      Some("piece_route")
    )
    val fileOpening = routeEvent.results.head.copy(kind = TransitionConsequenceKind.SemiOpenFileGain)
    assertEquals(
      MoveMeaningClaim.principalPlanFunctionType(
        claim.copy(principalPlanEvent = Some(routeEvent.copy(results = List(fileOpening.copy(stage = "direct")))))
      ),
      Some("pawn_break_timing")
    )
    assertEquals(
      MoveMeaningClaim.principalPlanFunctionType(
        claim.copy(principalPlanEvent = Some(routeEvent.copy(results = List(fileOpening))))
      ),
      None
    )

  private def ownedTacticalProof(
      id: String,
      position: PositionNodeRef,
      line: LineNodeRef
  ): (RelativeCauseProof, EvidenceRecord) =
    val ref = evidenceRef(
      id = s"tactical-mechanism:$id",
      producer = EvidenceProducer.TacticalMechanismProducer,
      layer = EvidenceLayer.TacticalMechanism,
      position = position,
      line = Some(line),
      scope = EvidenceScope.Counterfactual
    )
    val signal = TacticalMechanismSignal(
      kind = TacticalMechanismSignalKind.LineConsequence,
      label = "refutation-line",
      sourceLayer = EvidenceLayer.TacticalMechanism,
      source = Some(ref)
    )
    (
      RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          role = RelativeCauseProofRole.DirectProof,
          strength = RelativeCauseProofStrength.Primary,
          tacticalMechanisms = List(TacticalMechanismProof(ref, TacticalMechanismKind.Refutation, List(signal)))
        )
      ),
      EvidenceRecord(
        ref,
        TacticalMechanismEvidence(
          kind = TacticalMechanismKind.Refutation,
          moveUci = Some(line.rootMove),
          line = Some(line),
          signals = List(signal)
        )
      )
    )

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
