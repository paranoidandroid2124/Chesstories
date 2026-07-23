package lila.chessjudgment.model.judgment

import chess.{ Color, King, Pawn, Square }
import lila.chessjudgment.analysis.position.PositionFactNormalizer
import lila.chessjudgment.model.{ Fact, FactScope, Motif, PlanId }
import lila.chessjudgment.model.strategic.VariationLine
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanTheme
import play.api.libs.json.JsObject

class MoveJudgmentViewTest extends munit.FunSuite:

  test("unknown coordinate-shaped plan subjects are not concrete target proof"):
    val unknownSubject =
      "actor=Move:e2e4|target=PlanSubject:foo:e4|mechanism=Mechanism:target-pressure|consequence=Consequence:target-pressure"
    val typedSquare =
      "actor=Move:e2e4|target=Square:e4|mechanism=Mechanism:target-pressure|consequence=Consequence:target-pressure"

    assert(!EvidenceObjectBinding.specificTargetMechanismReadySignatures(List(unknownSubject)))
    assert(EvidenceObjectBinding.specificTargetMechanismReadySignatures(List(typedSquare)))

  test("line root events never cross from another line root"):
    val line = LineNodeRef("threat:6", "c4d5", 6, LineNodeRole.Threat)
    val event = LineMoveEvent(
      kind = LineEventKind.PassedPawn,
      moveUci = "c4d5",
      plyOffset = 0,
      side = Some(Color.White),
      pieceRole = Some(EvidencePieceRole(Pawn.name)),
      square = Some(EvidenceSquare("d5"))
    )
    val payload = new LineFactEvidence(line, Some("c4d5"), None, Nil)(events = List(event))

    assertEquals(payload.eventsForRootMove("d6d5"), Nil)
    assertEquals(payload.eventsForRootMove("c4d5"), List(event))

  test("material gain ownership follows the lasting outcome event rather than every capture in a positive line"):
    val line = LineNodeRef("threat:material", "h7h6", 1, LineNodeRole.Threat)
    val bishopExchange = LineMaterialCapture(
      "g5f6",
      1,
      Color.White,
      EvidencePieceRole("bishop"),
      EvidencePieceRole("knight"),
      EvidenceSquare("f6"),
      300,
      recapture = false
    )
    val recapture = LineMaterialCapture(
      "g7f6",
      2,
      Color.Black,
      EvidencePieceRole("pawn"),
      EvidencePieceRole("bishop"),
      EvidenceSquare("f6"),
      300,
      recapture = true
    )
    val lastingPawnGain = LineMaterialCapture(
      "g4h5",
      5,
      Color.White,
      EvidencePieceRole("bishop"),
      EvidencePieceRole("pawn"),
      EvidenceSquare("h5"),
      100,
      recapture = false
    )
    val summary = LineMaterialSummary(
      sideToMove = Color.White,
      captures = List(bishopExchange, recapture, lastingPawnGain),
      netCaptureCpForMover = 100,
      maxGainCpForMover = 300,
      maxLossCpForMover = 0,
      hasRecaptureChain = true,
      hasRecoveryWindow = false,
      promotionGainCpForMover = 0,
      materialWindowComplete = true
    )
    val lastingOutcome = LineConsequence(
      kind = LineConsequenceKind.MaterialGain,
      lineMoves = List("g4h5"),
      proofSignal = false,
      eventMove = Some("g4h5"),
      beneficiary = Some(Color.White)
    )
    val payload = new LineFactEvidence(
      line,
      Some("h7h6"),
      Some("g5f6"),
      List("g7f6", "g4h5"),
      material = Some(summary)
    )(consequences = List(lastingOutcome))

    assertEquals(payload.materialGainCapturesFor(Color.White), List(lastingPawnGain))
    assertEquals(payload.materialGainCapturesFor(Color.Black), Nil)

  test("a return to the starting square is not an object-state continuation"):
    val root = LineReplayStep(
      1,
      "c3e2",
      "4k3/p7/8/8/8/2N5/8/4K3 w - - 0 1",
      "4k3/p7/8/8/8/8/4N3/4K3 b - - 1 1"
    )
    val reply = LineReplayStep(
      2,
      "a7a6",
      root.fenAfter,
      "4k3/8/p7/8/8/8/4N3/4K3 w - - 0 2"
    )
    val returns = LineReplayStep(
      3,
      "e2c3",
      reply.fenAfter,
      "4k3/8/p7/8/8/2N5/8/4K3 b - - 1 2"
    )

    assertEquals(LineObjectTrajectory.find(root, List(reply, returns)), None)

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

  test("preserves concrete piece-route objects on semantic details"):
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
          subjects = List("battery:diagonal:c1-h6:bishop-queen"),
          targetSubjects = List("pawn:f7")
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

    assert(detail.semanticAnchorKeys.contains("axis:Diagonal"), detail.semanticAnchorKeys)
    assert(detail.structuralMotifTags.contains("battery"), detail.structuralMotifTags)
    assert(detail.structuralMotifTags.contains("diagonal"), detail.structuralMotifTags)
    assert(detail.causeEvidenceIds.contains(causeRef.id), detail.causeEvidenceIds)
    assert(detail.proofRoles.contains(RelativeCauseProofRole.DirectProof), detail.proofRoles)
    assert(
      detail.objectBindingSignatures.exists(signature =>
          signature.contains("actor=Move:h1h6") &&
          signature.contains("actor=Piece:queen") &&
          signature.contains("actor=Square:h1") &&
          signature.contains("target=Piece:pawn") &&
          signature.contains("target=Square:f7") &&
          signature.contains("witness=Piece:bishop") &&
          signature.contains("witness=Piece:queen") &&
          signature.contains("witness=Square:c1") &&
          signature.contains("witness=Square:h6") &&
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
      displayPlanId = Some(lila.chessjudgment.model.PlanId.QueensideAttack)
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

    assert(detail.structuralPurposeSubjects.contains("bishop:f1-b5:mobility+4"), detail.structuralPurposeSubjects)
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
    assert(axisDetail.resourceContestKinds.contains("CounterplayRestraint"), axisDetail.resourceContestKinds)
    assert(axisDetail.resourceContestSignals.contains("OpponentLowMobility"), axisDetail.resourceContestSignals)
    assert(axisDetail.resourceContestSquares.contains("g5"), axisDetail.resourceContestSquares)
    assert(Set("counterplay", "kingside", "space").subsetOf(axisDetail.resourceContestScopes.toSet), axisDetail.resourceContestScopes)
    assertEquals(axisDetail.resourceContestMagnitude, Some(4))
    assert(axisDetail.causeEvidenceIds.isEmpty, axisDetail.causeEvidenceIds)
    assert(axisDetail.contextCauseEvidenceIds.contains(tacticalCauseRef.id), axisDetail.contextCauseEvidenceIds)
    assert(axisDetail.contextProofRoles.contains(RelativeCauseProofRole.ContextSupport), axisDetail.contextProofRoles)

  test("prefers a line-bound tactical event over same-comparison long-term context"):
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

    assert(view.causeAudit.primary.exists(_.causeKind == RelativeCauseKind.TacticalRefutationOfPlayed), view.causeAudit)
    assert(!view.causeAudit.primary.exists(_.causeKind == RelativeCauseKind.StructuralImprovement), view.causeAudit)
    assert(!view.causeAudit.primary.exists(frame => Set(
      RelativeCauseKind.RecaptureRecoveryWindow,
      RelativeCauseKind.DefensiveResource,
      RelativeCauseKind.MaterialSwing
    )(frame.causeKind)), view.causeAudit)

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

    val structuralFrame = view.causeAudit.all.find(_.causeKind == RelativeCauseKind.StructuralImprovement).get
    assert(view.causeAudit.primary.exists(_.causeKind == RelativeCauseKind.TacticalRefutationOfPlayed), view.causeAudit)
    assert(!view.causeAudit.primary.exists(_.causeKind == RelativeCauseKind.StructuralImprovement), view.causeAudit)
    assert(!structuralFrame.tacticalWitnessCauseKinds.contains(RelativeCauseKind.TacticalRefutationOfPlayed))

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

    assert(!view.causeAudit.primary.exists(_.causeKind == RelativeCauseKind.ActivityLoss), view.causeAudit)

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
    assert(!clusterFrame.concreteObjectReady, clusterFrame)

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

    assert(!view.causeAudit.primary.exists(_.causeKind == RelativeCauseKind.PlanContradiction), view.causeAudit)

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

    assert(!view.causeAudit.primary.exists(_.causeKind == RelativeCauseKind.TacticalRefutationOfPlayed), view.causeAudit)

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

    assert(narrated.exists(frame =>
      frame.causeKind == RelativeCauseKind.DrawResource &&
        frame.narrativeRole == MoveJudgmentCauseNarrativeRole.RootCause
    ), narrated)
    assert(!narrated.exists(frame =>
      frame.causeKind == RelativeCauseKind.StructuralImprovement &&
        frame.narrativeRole == MoveJudgmentCauseNarrativeRole.RootCause
    ), narrated)

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

  test("move functions distinguish immediate plan and opponent reply results"):
    val rootSource = NumberedChessMove("e2e3", "Ne3", ChessTurn(12, "white", "12."), "12.Ne3")
    def rootOwned(result: PlanResult): PlanResult =
      result.copy(stage = "direct", source = Some(rootSource), sourcePlyOffset = Some(0))
    val routeEvent = ResolvedPlanEvent(
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
        PlanResult(
          stage = "future",
          kind = TransitionConsequenceKind.MobilityGain,
          polarity = StructuralSignalPolarity.Gain,
          strength = 1,
          subjects = Nil
        )
      ),
      responses = Nil,
      testedContinuation = None
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
      resolvedPlanEvent = Some(routeEvent)
    )

    assertEquals(MoveMeaningClaim.resolvedPlanFunctionType(claim), None)
    assertEquals(
      MoveMeaningClaim.resolvedPlanFunctionType(
        claim.copy(resolvedPlanEvent = Some(routeEvent.copy(results = routeEvent.results.map(rootOwned))))
      ),
      Some("piece_route")
    )
    val fileOpening = routeEvent.results.head.copy(kind = TransitionConsequenceKind.SemiOpenFileGain)
    assertEquals(
      MoveMeaningClaim.resolvedPlanFunctionType(
        claim.copy(resolvedPlanEvent = Some(routeEvent.copy(results = List(rootOwned(fileOpening)))))
      ),
      None
    )
    assertEquals(
      MoveMeaningClaim.resolvedPlanFunctionType(
        claim.copy(resolvedPlanEvent = Some(routeEvent.copy(actorRole = Some("pawn"), results = List(rootOwned(fileOpening)))))
      ),
      Some("pawn_break_timing")
    )
    val conditionalFileOpening = fileOpening.copy(robustness = Some(PlanCausalRobustness.Conditional))
    assertEquals(
      MoveMeaningClaim.resolvedPlanFunctionType(
        claim.copy(resolvedPlanEvent = Some(routeEvent.copy(actorRole = Some("pawn"), results = List(conditionalFileOpening))))
      ),
      Some("plan_continuity")
    )
    assertEquals(
      MoveMeaningClaim.resolvedPlanFunctionType(
        claim.copy(
          resolvedPlanEvent = Some(
            routeEvent.copy(actorRole = Some("pawn"), results = List(conditionalFileOpening.copy(stage = "response")))
          )
        )
      ),
      None
    )
    val prevention = routeEvent.copy(
      moveRole = Some(PlanMoveRole.Prevention),
      results = List(
        rootOwned(routeEvent.results.head).copy(
          kind = TransitionConsequenceKind.OpponentMobilityRestriction,
          subjects = List("pawn:c6-c5:entry-restricted:by:b3b4:exchange-gain:1")
        )
      )
    )
    assertEquals(
      MoveMeaningClaim.resolvedPlanFunctionType(claim.copy(resolvedPlanEvent = Some(prevention))),
      Some("counterplay_control")
    )
    assertEquals(
      MoveMeaningClaim.resolvedPlanFunctionType(
        claim.copy(
          resolvedPlanEvent = Some(
            prevention.copy(results = prevention.results.map(_.copy(subjects = List("pawn:d7-d5:advance-restricted"))))
          )
        )
      ),
      Some("counterplay_control")
    )

  test("dependency-only plan events stay internal until they own a specific public function"):
    val dependencyOnlyEvent = ResolvedPlanEvent(
      goalTheme = "PieceImprovement",
      goalKind = None,
      moveRole = Some(PlanMoveRole.Preparation),
      transitionType = None,
      rootMove = "g5f4",
      actorRole = Some("bishop"),
      actorFrom = Some("g5"),
      actorTo = Some("f4"),
      targets = Nil,
      developmentChoices = Nil,
      results = Nil,
      responses = Nil,
      testedContinuation = None,
      rootDependencyKinds = List(PlanCausalDependencyKind.ObjectStatePrecondition)
    )
    val claim = MoveMeaningClaim(
      meaningKind = "PlanContinuity",
      role = "PreparesCurrentPlan",
      laneKey = "dependency-only-plan",
      conflictKey = None,
      supportLevel = "owned_cause_linked",
      visibility = "reason_grade",
      surfaceLane = "current_move_owned",
      lineRole = "candidate",
      moveUci = "g5f4",
      frameId = "dependency-only-plan-frame",
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      axisKey = None,
      axisKind = None,
      axisPolarity = None,
      label = None,
      causeKinds = Nil,
      causeSourceSides = Nil,
      causeEvidenceIds = Nil,
      sourceEvidenceIds = List("plan-event"),
      objectBindingSignatures = Nil,
      resolvedPlanEvent = Some(dependencyOnlyEvent)
    )

    assert(!MoveMeaningClaim.resolvedPlanSpecificPublicFunction(claim))
    val concretePressure = PlanResult(
      stage = "future",
      kind = TransitionConsequenceKind.TargetPressureGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("d5"),
      robustness = Some(PlanCausalRobustness.Conditional)
    )
    assert(
      MoveMeaningClaim.resolvedPlanSpecificPublicFunction(
        claim.copy(resolvedPlanEvent = Some(dependencyOnlyEvent.copy(results = List(concretePressure))))
      )
    )

  test("resolved plan targets do not inherit sibling king-pressure bindings"):
    val pressureResult = PlanResult(
      stage = "direct",
      kind = TransitionConsequenceKind.TargetPressureGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("d5"),
      source = Some(NumberedChessMove("d3b5", "Bb5", ChessTurn(12, "white", "12."), "12.Bb5")),
      sourcePlyOffset = Some(0)
    )
    val event = ResolvedPlanEvent(
      goalTheme = "WeaknessFixation",
      goalKind = None,
      moveRole = Some(PlanMoveRole.Preparation),
      transitionType = None,
      rootMove = "d3b5",
      actorRole = Some("bishop"),
      actorFrom = Some("d3"),
      actorTo = Some("b5"),
      targets = List("d5"),
      developmentChoices = Nil,
      results = List(pressureResult),
      responses = Nil,
      testedContinuation = None
    )
    val contaminatedClaim = MoveMeaningClaim(
      meaningKind = "PlanContinuity",
      role = "PreparesCurrentPlan",
      laneKey = "owned-plan-target",
      conflictKey = None,
      supportLevel = "owned_cause_linked",
      visibility = "reason_grade",
      surfaceLane = "current_move_owned",
      lineRole = "candidate",
      moveUci = "d3b5",
      frameId = "owned-plan-target-frame",
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      axisKey = None,
      axisKind = None,
      axisPolarity = None,
      label = None,
      causeKinds = Nil,
      causeSourceSides = Nil,
      causeEvidenceIds = Nil,
      sourceEvidenceIds = List("plan-event"),
      objectBindingSignatures = List(
        "actor=Piece:bishop|target=Square:d5|mechanism=Mechanism:targetpressuregain",
        "actor=Piece:queen|target=Piece:king|target=Square:g8|mechanism=Mechanism:kingringpressuregain"
      ),
      resolvedPlanEvent = Some(event)
    )

    assert(!MoveMeaningSurface.kingPressureClaim(contaminatedClaim))
    assert(
      !MoveMeaningSurface.kingPressureClaim(
        contaminatedClaim.copy(
          resolvedPlanEvent = Some(
            event.copy(
              targets = List("d5", "g8"),
              results = List(
                pressureResult,
                pressureResult.copy(kind = TransitionConsequenceKind.KingRingPressureGain, subjects = List("g8"))
              )
            )
          )
        )
      )
    )
    assert(
      MoveMeaningSurface.kingPressureClaim(
        contaminatedClaim.copy(
          resolvedPlanEvent = Some(
            event.copy(
              targets = List("g8"),
              results = List(pressureResult.copy(kind = TransitionConsequenceKind.KingRingPressureGain, subjects = List("g8")))
            )
          )
        )
      )
    )

    val pressureClaim = contaminatedClaim.copy(
      meaningKind = "TargetPressure",
      axisKind = Some(StrategicAxisKind.Target),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      publicSurfaceAdmitted = true,
      publicProofLevel = "owned_function",
      publicTargetBound = true,
      targetSquares = List("d5"),
      boardCarriers = List(
        MoveMeaningSurfaceBoardCarrier("actor", "Move", "d3b5", from = Some("d3"), to = Some("b5")),
        MoveMeaningSurfaceBoardCarrier("target", "Square", "d5", semanticRole = Some("pressure_target"))
      )
    )
    def pressureView(claim: MoveMeaningClaim) = MoveJudgmentView(
      verdict = None,
      verdictCarriers = Nil,
      supportContextClusterIds = Nil,
      overriddenLocalIdeas = Nil,
      preservedLocalIdeas = Nil,
      moveMeaningClaims = List(claim)
    )

    assert(MoveMeaningSurface.unsupportedPositiveTargetPressure(pressureClaim))
    assertEquals(MoveMeaningSurface.publicSurfaces(pressureView(pressureClaim)), Nil)

    val pinPressureClaim = pressureClaim.copy(
      publicProofLevel = "owned_cause",
      proofRelationKinds = List(RelationFactKind.Pin)
    )
    assert(!MoveMeaningSurface.unsupportedPositiveTargetPressure(pinPressureClaim))
    val pinPressureSurface = MoveMeaningSurface.publicSurfaces(pressureView(pinPressureClaim)).headOption
      .getOrElse(fail("expected tactically proved pressure"))
    assertEquals(pinPressureSurface.idea.code, "tactical_pressure")
    assert(!pinPressureSurface.idea.label.contains("bishop"))

    val attackedPieceClaim = pressureClaim.copy(
      boardCarriers = pressureClaim.boardCarriers :+ MoveMeaningSurfaceBoardCarrier(
        "target",
        "Piece",
        "pawn",
        semanticRole = Some(MoveMeaningSurfaceBoardCarrier.AttackedPieceRole)
      )
    )
    assert(!MoveMeaningSurface.unsupportedPositiveTargetPressure(attackedPieceClaim))
    assert(MoveMeaningSurface.publicSurfaces(pressureView(attackedPieceClaim)).nonEmpty)

    val provedPlanClaim = pressureClaim.copy(positiveFunctionalProofEvidenceIds = List("plan-event"))
    val provedPlanSurface = MoveMeaningSurface.publicSurfaces(pressureView(provedPlanClaim)).headOption
      .getOrElse(fail("expected proved plan pressure"))
    assertEquals(provedPlanSurface.idea.label, "pressure on d5")
    assert(!provedPlanSurface.idea.label.contains("bishop"))

    val idOnlyFixationSurface = MoveMeaningSurface
      .publicSurfaces(
        pressureView(
          provedPlanClaim.copy(sourceEvidenceIds = List("opaque:target-fixation"))
        )
      )
      .headOption
      .getOrElse(fail("expected pressure with an inert evidence id"))
    assertEquals(idOnlyFixationSurface.idea.label, "pressure on d5")

    val typedFixationSurface = MoveMeaningSurface
      .publicSurfaces(
        pressureView(
          provedPlanClaim.copy(
            axisPolarity = Some(StrategicAxisPolarity.Support),
            sourceEvidenceIds = List("opaque-source")
          )
        )
      )
      .headOption
      .getOrElse(fail("expected typed target fixation"))
    assertEquals(typedFixationSurface.idea.label, "fixes target on d5")

  test("pawn-break transition evidence ignores evidence id text"):
    val idOnly = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      sourceEvidenceIds = List("opaque:transition:played")
    )
    val typed = idOnly.copy(
      sourceEvidenceIds = List("opaque-source"),
      structuralConsequenceKinds = List(TransitionConsequenceKind.PawnTensionGain)
    )

    assert(!PositionPlanTechniqueProjection.positionPlanTechniquePawnBreakTransitionEvidence(idOnly))
    assert(PositionPlanTechniqueProjection.positionPlanTechniquePawnBreakTransitionEvidence(typed))

  test("tactical structural cause linkage requires owned typed proof and source ownership"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val causeRef = evidenceRef(
      id = "relative-cause:opaque",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val (tacticalProof, _) = ownedTacticalProof("typed-owner", root, playedLine)
    def tacticalCause(proof: Option[RelativeCauseProof]) = RelativeCauseFact(
      kind = RelativeCauseKind.TacticalRefutationOfPlayed,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Mistake,
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
    )(proof)
    val misleadingLabel = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      label = Some("tactical-proof"),
      structuralRouteMove = Some("d2d4"),
      structuralPurposeSubjects = List("square:d4"),
      sourceEvidenceIds = List(causeRef.id)
    )
    val opaqueLabel = misleadingLabel.copy(label = Some("opaque-label"))

    assert(!PositionPlanTechniqueProjection.positionPlanTechniqueTacticalProofCauseKind(
      misleadingLabel,
      causeRef,
      tacticalCause(None)
    ))
    assert(PositionPlanTechniqueProjection.positionPlanTechniqueTacticalProofCauseKind(
      opaqueLabel,
      causeRef,
      tacticalCause(Some(tacticalProof))
    ))
    assert(!PositionPlanTechniqueProjection.positionPlanTechniqueTacticalProofCauseKind(
      opaqueLabel.copy(sourceEvidenceIds = List("relative-cause:other")),
      causeRef,
      tacticalCause(Some(tacticalProof))
    ))

    val rootBoundStructuralDetail = opaqueLabel.copy(
      structuralConsequenceKinds = List(TransitionConsequenceKind.TargetPressureGain),
      structuralPurposeSubjects = List("g2")
    )
    val localCause = tacticalCause(Some(tacticalProof))
    assert(PositionPlanTechniqueProjection.positionPlanTechniqueCauseOwnsStructuralRoot(
      rootBoundStructuralDetail,
      localCause
    ))
    assert(!PositionPlanTechniqueProjection.positionPlanTechniqueCauseOwnsStructuralRoot(
      rootBoundStructuralDetail,
      localCause.copy(eventLine = referenceLine, sourceSide = RelativeCauseSourceSide.Reference)(localCause.proof)
    ))
    assert(!PositionPlanTechniqueProjection.positionPlanTechniqueCauseOwnsStructuralRoot(
      rootBoundStructuralDetail.copy(structuralRouteMove = None),
      localCause
    ))

  test("public compression keeps one concrete carrier for the same human idea"):
    val assessment = MoveMeaningSurfaceAssessment(
      moveQuality = MoveMeaningSurfaceCode("good", "good move"),
      ideaQuality = MoveMeaningSurfaceCode("verified", "verified reason"),
      priority = MoveMeaningSurfaceCode("main", "main reason"),
      verdictReason = true,
      localIdea = true
    )
    def surface(
        lineRole: String,
        target: MoveMeaningSurfaceTarget,
        event: Option[ResolvedPlanEvent] = None,
        representative: Boolean = false
    ) =
      MoveMeaningSurface(
        moveUci = "d8d7",
        subject = "played_move",
        lineRole = lineRole,
        moveQuality = "good",
        unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
        ideaType = "piece_route",
        idea = MoveMeaningSurfaceCode("piece_route", "rook lift to d7"),
        ideaQuality = "verified",
        assessment = assessment,
        failureFamily = None,
        problem = None,
        target = target,
        priority = "main",
        evidence = MoveMeaningSurfaceEvidence(
          publicSurfaceAdmitted = true,
          positiveFunctionalProofIds = event.toList.map(_ => "plan-event")
        ),
        resolvedPlanEvent = event,
        representativePlanEvent = representative
      )
    val planEvent = ResolvedPlanEvent(
      goalTheme = "WingPlay",
      goalKind = None,
      moveRole = Some(PlanMoveRole.Preparation),
      transitionType = None,
      rootMove = "d8d7",
      actorRole = Some("rook"),
      actorFrom = Some("d8"),
      actorTo = Some("d7"),
      targets = List("d7"),
      developmentChoices = List(StructuralDevelopmentChoice("rook", "d8", "d7")),
      results = Nil,
      responses = Nil,
      testedContinuation = None,
      rootDependencyKinds = List(PlanCausalDependencyKind.LineAccessPrecondition)
    )
    val generic = surface("reference", MoveMeaningSurfaceTarget()).copy(
      priority = "main",
      assessment = assessment.copy(priority = MoveMeaningSurfaceCode("main", "main reason"))
    )
    val direct = surface("candidate", MoveMeaningSurfaceTarget(squares = List("d7")))
    val owned = surface(
      "candidate",
      MoveMeaningSurfaceTarget(squares = List("d7")),
      event = Some(planEvent),
      representative = true
    )
    val distinctTarget = surface("candidate", MoveMeaningSurfaceTarget(squares = List("a7"))).copy(
      idea = MoveMeaningSurfaceCode("piece_route", "rook pressure on a7")
    )

    val compressed = MoveMeaningSurface.compressEquivalentPublicIdeas(List(generic, direct, owned, distinctTarget))
    assertEquals(compressed.size, 2)
    assert(compressed.exists(surface =>
      surface.resolvedPlanEvent == owned.resolvedPlanEvent && surface.priority == "main"
    ), compressed)
    assert(compressed.contains(distinctTarget), compressed)

    val competingPlan = surface(
      "candidate",
      MoveMeaningSurfaceTarget(squares = List("c6")),
      event = Some(planEvent.copy(goalTheme = "RestrictionProphylaxis"))
    ).copy(
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      ideaType = "counterplay_control",
      idea = MoveMeaningSurfaceCode("counterplay_control", "restrains c6-c5")
    )
    val displayed = MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(competingPlan, owned))
    assertEquals(displayed.headOption, Some(owned))
    assertEquals(
      MoveMeaningSurface.publicIdeaChainSelectedPlanEvent(List(competingPlan, owned)),
      Some(planEvent)
    )
    val representativeContinuity = owned.copy(
      displayPlanId = Some(PlanId.PawnBreakPreparation),
      ideaType = "plan_continuity",
      idea = MoveMeaningSurfaceCode("plan_continuity", "plan continuity")
    )
    val representativeRoute = owned.copy(
      displayPlanId = Some(PlanId.PawnBreakPreparation),
      ideaType = "piece_route",
      idea = MoveMeaningSurfaceCode("piece_route", "supporting route")
    )
    val independentlyOwnedPlanResult = competingPlan.copy(
      displayPlanId = Some(PlanId.Prophylaxis),
      evidence = competingPlan.evidence.copy(
        proofLevel = "owned_function",
        positiveFunctionalProofIds = List("prophylaxis-plan-event")
      )
    )
    val diversified = MoveMeaningSurface.publicIdeaChainDisplaySemantics(
      List(representativeContinuity, representativeRoute, independentlyOwnedPlanResult)
    )
    assertEquals(
      diversified.count(surface =>
        surface.representativePlanEvent && surface.displayPlanId.contains(PlanId.PawnBreakPreparation)
      ),
      1
    )
    assert(diversified.contains(independentlyOwnedPlanResult), diversified)
    val mismatchedPlan = owned.copy(
      resolvedPlanEvent = Some(planEvent.copy(rootMove = "a2a4")),
      representativePlanEvent = true
    )
    assertEquals(MoveMeaningSurface.publicIdeaChainSelectedPlanEvent(List(mismatchedPlan)), None)

    val emptyRepresentative = owned.copy(
      resolvedPlanEvent = Some(planEvent.copy(
        targets = Nil,
        developmentChoices = Nil,
        rootDependencyKinds = Nil,
        preparedPawnAdvanceFiles = List("d")
      )),
      representativePlanEvent = true
    )
    assertEquals(MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(emptyRepresentative, direct)), List(direct))
    List(List(emptyRepresentative, direct), List(direct, emptyRepresentative)).foreach { equivalentSurfaces =>
      val compressedEquivalent = MoveMeaningSurface.compressEquivalentPublicIdeas(equivalentSurfaces)
      assertEquals(compressedEquivalent.size, 1)
      assert(!compressedEquivalent.head.representativePlanEvent)
      assert(compressedEquivalent.head.representativePlanEvidenceObserved)
      assertEquals(
        MoveMeaningSurface.publicIdeaChainDisplaySemantics(compressedEquivalent).map(_.idea),
        List(direct.idea)
      )
    }
    val targetOnlyRepresentative = emptyRepresentative.copy(
      resolvedPlanEvent = Some(planEvent.copy(
        developmentChoices = Nil,
        rootDependencyKinds = Nil
      ))
    )
    assertEquals(MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(targetOnlyRepresentative, direct)), List(direct))
    val dependencyOnlyRepresentative = emptyRepresentative.copy(
      resolvedPlanEvent = Some(planEvent.copy(targets = Nil, developmentChoices = Nil))
    )
    assertEquals(MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(dependencyOnlyRepresentative, direct)), List(direct))

    val futurePressure = PlanResult(
      stage = "future",
      kind = TransitionConsequenceKind.TargetPressureGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("e7"),
      source = Some(NumberedChessMove("a1e1", "Re1", ChessTurn(12, "white", "12."), "12.Re1")),
      robustness = Some(PlanCausalRobustness.Conditional),
      sourcePlyOffset = Some(4)
    )
    val conditionedPlanEvent = planEvent.copy(
      targets = List("e7"),
      results = List(futurePressure),
      representativeResult = Some(futurePressure)
    )
    val conditionedRepresentative = owned.copy(
      ideaType = "plan_continuity",
      idea = MoveMeaningSurfaceCode("plan_continuity", "future rook pressure"),
      evidence = owned.evidence.copy(
        proofLevel = "owned_function",
        sourceIds = List("played-plan-event"),
        boardCarriers = List(
          MoveMeaningSurfaceBoardCarrier("actor", "Move", "d8d7", from = Some("d8"), to = Some("d7"))
        )
      ),
      resolvedPlanEvent = Some(conditionedPlanEvent),
      representativePlanEvent = true
    )
    val directPressure = direct.copy(
      ideaType = "target_pressure",
      idea = MoveMeaningSurfaceCode("target_pressure", "bishop attacks queen"),
      target = MoveMeaningSurfaceTarget(squares = List("d6"), pieces = List("queen")),
      evidence = MoveMeaningSurfaceEvidence(
        publicSurfaceAdmitted = true,
        proofLevel = "owned_cause",
        targetBound = true,
        causeIds = List("direct-target-pressure"),
        sourceIds = List("played-structural-delta"),
        boardCarriers = List(
          MoveMeaningSurfaceBoardCarrier("actor", "Move", "d8d7", from = Some("d8"), to = Some("d7")),
          MoveMeaningSurfaceBoardCarrier(
            "target",
            "Piece",
            "queen",
            semanticRole = Some(MoveMeaningSurfaceBoardCarrier.AttackedPieceRole)
          ),
          MoveMeaningSurfaceBoardCarrier("target", "Square", "d6")
        )
      )
    )
    assertEquals(
      MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(conditionedRepresentative, directPressure)).headOption,
      Some(directPressure)
    )
    assertEquals(
      MoveMeaningSurface.publicIdeaChainSelectedPlanEvent(List(conditionedRepresentative, directPressure)),
      None
    )
    assertEquals(
      MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(owned, directPressure)).headOption,
      Some(directPressure)
    )
    val exactStructuralPressure = directPressure.copy(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      evidence = directPressure.evidence.copy(
        boardCarriers = directPressure.evidence.boardCarriers.map(_.copy(semanticRole = None))
      )
    )
    assertEquals(
      MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(owned, exactStructuralPressure)).headOption,
      Some(exactStructuralPressure)
    )
    val nonRepresentativeDistractor = owned.copy(
      ideaType = "pawn_break_timing",
      idea = MoveMeaningSurfaceCode("pawn_break_timing", "derived pawn break"),
      resolvedPlanEvent = None,
      representativePlanEvent = false
    )
    assertEquals(
      MoveMeaningSurface
        .publicIdeaChainDisplaySemantics(List(nonRepresentativeDistractor, exactStructuralPressure))
        .headOption,
      Some(exactStructuralPressure)
    )
    val supportingDistractor = nonRepresentativeDistractor.copy(mainExplanationCandidate = false)
    val diversifiedFallback = MoveMeaningSurface.publicIdeaChainDisplaySemantics(
      List(emptyRepresentative, supportingDistractor, direct)
    )
    assert(diversifiedFallback.contains(direct), diversifiedFallback)
    assert(diversifiedFallback.contains(supportingDistractor), diversifiedFallback)
    assert(!diversifiedFallback.contains(emptyRepresentative), diversifiedFallback)
    val compressedDominanceWitness = MoveMeaningSurface.compressEquivalentPublicIdeas(
      List(emptyRepresentative, direct, supportingDistractor)
    )
    val compressedFallback = MoveMeaningSurface.publicIdeaChainDisplaySemantics(compressedDominanceWitness)
    assert(compressedFallback.exists(_.idea == direct.idea), compressedFallback)
    assert(compressedFallback.contains(supportingDistractor), compressedFallback)
    val markedSupporting = supportingDistractor.copy(representativePlanEvidenceObserved = true)
    assertEquals(MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(markedSupporting, direct)), List(direct))
    val fallbackOnly = supportingDistractor
    assertEquals(MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(fallbackOnly)), List(fallbackOnly))
    assert(!MoveMeaningClaim.publicPlanResultOwnsRootFunction(conditionedPlanEvent, futurePressure))
    assertEquals(
      MoveMeaningClaim.resolvedPlanFunctionType(
        MoveMeaningClaim(
          meaningKind = "PlanContinuity",
          role = "ExecutesCurrentPlan",
          laneKey = "future-pressure",
          conflictKey = None,
          supportLevel = "owned_cause_linked",
          visibility = "reason_grade",
          surfaceLane = "current_move_owned",
          lineRole = "candidate",
          moveUci = "d8d7",
          frameId = "future-pressure-frame",
          unit = PositionPlanTechniqueUnit.PlanOptionSet,
          axisKey = None,
          axisKind = None,
          axisPolarity = None,
          label = None,
          causeKinds = Nil,
          causeSourceSides = Nil,
          causeEvidenceIds = Nil,
          sourceEvidenceIds = List("plan-event"),
          objectBindingSignatures = Nil,
          resolvedPlanEvent = Some(conditionedPlanEvent)
        )
      ),
      Some("plan_continuity")
    )

    val directPressureResult = futurePressure.copy(
      stage = "direct",
      source = Some(NumberedChessMove("d8d7", "Rd7", ChessTurn(12, "white", "12."), "12.Rd7")),
      sourcePlyOffset = Some(0),
      robustness = Some(PlanCausalRobustness.Robust)
    )
    assert(MoveMeaningClaim.publicPlanResultOwnsRootFunction(
      conditionedPlanEvent.copy(rootMove = "d8d7"),
      directPressureResult
    ))
    val directPressureEvent = conditionedPlanEvent.copy(rootMove = "d8d7")
    assert(!MoveMeaningClaim.publicPlanResultOwnsRootFunction(
      directPressureEvent,
      directPressureResult.copy(source = None)
    ))
    assert(!MoveMeaningClaim.publicPlanResultOwnsRootFunction(
      directPressureEvent,
      directPressureResult.copy(sourcePlyOffset = None)
    ))
    assert(!MoveMeaningClaim.publicPlanResultOwnsRootFunction(
      directPressureEvent,
      directPressureResult.copy(stage = "future")
    ))
    val rootOwnedDirectPlan = owned.copy(
      ideaType = "pawn_break_timing",
      idea = MoveMeaningSurfaceCode("pawn_break_timing", "direct pawn break result"),
      resolvedPlanEvent = Some(conditionedPlanEvent.copy(
        rootMove = "d8d7",
        results = List(directPressureResult),
        representativeResult = Some(directPressureResult)
      )),
      representativePlanEvent = true
    )
    assertEquals(
      MoveMeaningSurface
        .publicIdeaChainDisplaySemantics(List(rootOwnedDirectPlan, exactStructuralPressure))
        .headOption,
      Some(rootOwnedDirectPlan)
    )
    val exactKingPressure = exactStructuralPressure.copy(
      idea = MoveMeaningSurfaceCode("target_pressure", "rook attacks king"),
      priority = "supporting",
      assessment = exactStructuralPressure.assessment.copy(
        priority = MoveMeaningSurfaceCode("supporting", "supporting reason")
      ),
      target = MoveMeaningSurfaceTarget(squares = List("d6"), pieces = List("king")),
      evidence = exactStructuralPressure.evidence.copy(
        boardCarriers = exactStructuralPressure.evidence.boardCarriers.map {
          case carrier if carrier.role == "target" && carrier.kind == "Piece" => carrier.copy(value = "king")
          case carrier => carrier
        }
      )
    )
    assertEquals(
      MoveMeaningSurface
        .publicIdeaChainDisplaySemantics(List(rootOwnedDirectPlan, exactKingPressure))
        .headOption,
      Some(exactKingPressure)
    )
    val conditionalDirectResult = directPressureResult.copy(
      robustness = Some(PlanCausalRobustness.Conditional)
    )
    val conditionalDirectPlan = rootOwnedDirectPlan.copy(
      resolvedPlanEvent = Some(conditionedPlanEvent.copy(
        rootMove = "d8d7",
        results = List(conditionalDirectResult),
        representativeResult = Some(conditionalDirectResult)
      ))
    )
    assertEquals(
      MoveMeaningSurface
        .publicIdeaChainDisplaySemantics(List(conditionalDirectPlan, exactStructuralPressure))
        .headOption,
      Some(exactStructuralPressure)
    )
    val representativeFutureWithUnrelatedDirectResult = conditionedRepresentative.copy(
      resolvedPlanEvent = Some(conditionedPlanEvent.copy(
        rootMove = "d8d7",
        results = List(futurePressure, directPressureResult),
        representativeResult = Some(futurePressure)
      ))
    )
    assertEquals(
      MoveMeaningSurface
        .publicIdeaChainDisplaySemantics(List(representativeFutureWithUnrelatedDirectResult, exactStructuralPressure))
        .headOption,
      Some(exactStructuralPressure)
    )
    val mainIndependentCurrentFunction = exactStructuralPressure.copy(
      ideaType = "piece_route",
      idea = MoveMeaningSurfaceCode("piece_route", "main independent current function"),
      priority = "main"
    )
    val supportingExactPressure = exactStructuralPressure.copy(
      priority = "supporting",
      assessment = exactStructuralPressure.assessment.copy(
        priority = MoveMeaningSurfaceCode("supporting", "supporting reason")
      )
    )
    assertEquals(
      MoveMeaningSurface
        .publicIdeaChainDisplaySemantics(List(supportingExactPressure, mainIndependentCurrentFunction))
        .headOption,
      Some(mainIndependentCurrentFunction)
    )
    assertEquals(
      MoveMeaningSurface
        .publicIdeaChainDisplaySemantics(List(conditionedRepresentative, mainIndependentCurrentFunction))
        .headOption,
      Some(mainIndependentCurrentFunction)
    )
    assertEquals(
      MoveMeaningSurface
        .publicIdeaChainDisplaySemantics(List(supportingExactPressure, conditionedRepresentative))
        .headOption,
      Some(conditionedRepresentative)
    )
    val nonRepresentativeSelection = MoveMeaningSurface.publicIdeaChainDisplaySemantics(
      List(independentlyOwnedPlanResult, supportingExactPressure)
    )
    assertEquals(nonRepresentativeSelection.headOption, Some(supportingExactPressure))
    assert(nonRepresentativeSelection.contains(independentlyOwnedPlanResult), nonRepresentativeSelection)
    assertEquals(
      MoveMeaningSurface.publicIdeaChainSelectedPlanEvent(nonRepresentativeSelection),
      None
    )
    val replyTestedRepresentative = conditionedRepresentative.copy(
      resolvedPlanEvent = Some(conditionedPlanEvent.copy(
        testedContinuation = Some(TestedPlanContinuation(
          dependencyKind = PlanCausalDependencyKind.ObjectStatePrecondition,
          futureMove = "a1e1",
          targetSquare = Some("e7"),
          plyOffset = 4,
          robustness = PlanCausalRobustness.Conditional,
          realizedReplies = 1,
          exactReplies = 1,
          equivalentReplies = 0,
          testedReplies = 3,
          expectedReplies = 3,
          sequence = Nil,
          representativeResult = Some(futurePressure)
        ))
      ))
    )
    val replyTestedSelection = MoveMeaningSurface.publicIdeaChainDisplaySemantics(
      List(replyTestedRepresentative, supportingExactPressure)
    )
    assertEquals(replyTestedSelection.headOption, Some(supportingExactPressure))
    assert(replyTestedSelection.contains(replyTestedRepresentative), replyTestedSelection)
    val sacrificeAtRoot = MoveMeaningSurfaceBoardCarrier(
      "target",
      "PlanSubject",
      "material-sacrifice:d7",
      semanticRole = Some("root_material_sacrifice")
    )
    val futureSacrifice = sacrificeAtRoot.copy(value = "material-sacrifice:a7")
    val rootOwnedSacrificeCompensation = exactStructuralPressure.copy(
      unit = PositionPlanTechniqueUnit.CompensationSource,
      ideaType = "compensation",
      idea = MoveMeaningSurfaceCode("compensation", "root-owned sacrifice compensation"),
      evidence = exactStructuralPressure.evidence.copy(
        proofLevel = "owned_function",
        positiveFunctionalProofIds = List("compensation-plan-event"),
        boardCarriers = exactStructuralPressure.evidence.boardCarriers :+ sacrificeAtRoot
      )
    )
    assertEquals(
      MoveMeaningSurface
        .publicIdeaChainDisplaySemantics(List(exactStructuralPressure, rootOwnedSacrificeCompensation))
        .headOption,
      Some(rootOwnedSacrificeCompensation)
    )
    assertEquals(
      MoveMeaningClaim.preferredMaterialSacrificeCarriers(
        List(futureSacrifice, sacrificeAtRoot),
        observedMaterialCostSquares = List("a7"),
        claimMove = "d8d7"
      ),
      List(sacrificeAtRoot)
    )
    assertEquals(
      MoveMeaningClaim.preferredMaterialSacrificeCarriers(
        List(futureSacrifice),
        observedMaterialCostSquares = List("a7"),
        claimMove = "d8d7"
      ),
      List(futureSacrifice)
    )
    val unownedSameDestinationSacrifice = sacrificeAtRoot.copy(semanticRole = Some("material_sacrifice"))
    assertEquals(
      MoveMeaningClaim.preferredMaterialSacrificeCarriers(
        List(unownedSameDestinationSacrifice, futureSacrifice),
        observedMaterialCostSquares = List("a7"),
        claimMove = "d8d7"
      ),
      List(futureSacrifice)
    )
    val ambiguousPlanResults = conditionedPlanEvent.copy(
      rootMove = "d8d7",
      results = List(futurePressure, directPressureResult),
      representativeResult = None
    )
    assertEquals(MoveMeaningClaim.publicPlanResultForEvent(ambiguousPlanResults), None)
    assertEquals(
      MoveMeaningClaim.publicPlanResultForEvent(
        ambiguousPlanResults.copy(
          results = List(directPressureResult),
          representativeResult = Some(futurePressure.copy(polarity = StructuralSignalPolarity.Loss))
        )
      ),
      None
    )
    assertEquals(
      MoveMeaningClaim.publicPlanResultForEvent(
        ambiguousPlanResults.copy(
          results = List(directPressureResult.copy(stage = "response")),
          representativeResult = Some(directPressureResult.copy(stage = "response"))
        )
      ),
      None
    )

    val restrictedMove = NumberedChessMove("f7f6", "...f6", ChessTurn(12, "black", "12..."), "12...f6")
    val materialReply = NumberedChessMove("e5f6", "Nxf6+", ChessTurn(13, "white", "13."), "13.Nxf6+")
    val realizedResponse = PlanReplyTest(
      move = restrictedMove.uci,
      moveReference = Some(restrictedMove),
      outcome = PlanCausalBranchOutcome.Realized,
      realizationMove = Some(materialReply.uci),
      realizationReference = Some(materialReply),
      realizationMatch = Some(PlanCausalRealizationMatch.ExactMove),
      observedThrough = Some(materialReply.turn),
      terminalOutcome = None,
      terminalReference = None,
      line = List(restrictedMove, materialReply)
    )
    val conditionalRestriction = PlanResult(
      stage = "direct",
      kind = TransitionConsequenceKind.OpponentMobilityRestriction,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("pawn:f7-f6:advance-restricted"),
      source = conditionedPlanEvent.rootMoveReference,
      robustness = Some(PlanCausalRobustness.Conditional),
      conditions = List(realizedResponse),
      materialCounterplayPreventionProofReady = true
    )
    val responseEvent = conditionedPlanEvent.copy(
      goalTheme = PlanTheme.RestrictionProphylaxis.id,
      results = List(conditionalRestriction),
      responses = List(realizedResponse),
      representativeResult = Some(conditionalRestriction)
    )
    val realizedResponseSurface = competingPlan.copy(
      priority = "main",
      resolvedPlanEvent = Some(responseEvent),
      representativePlanEvent = false,
      idea = MoveMeaningSurfaceCode("counterplay_control", "tested reply deterrence")
    )
    val conditionalContinuitySurface = realizedResponseSurface.copy(
      ideaType = "plan_continuity",
      idea = MoveMeaningSurfaceCode("plan_continuity", "plan continuity"),
      representativePlanEvent = true
    )
    assertEquals(
      MoveMeaningSurface.publicIdeaDisplayLabel(conditionalContinuitySurface),
      "prepares Nxf6+ after f7-f6"
    )
    val refutedResponse = realizedResponse.copy(outcome = PlanCausalBranchOutcome.Refuted)
    val supersededResponse = realizedResponse.copy(
      outcome = PlanCausalBranchOutcome.Diverted,
      terminalOutcome = Some(PlanCausalTerminalOutcome.Draw),
      terminalReference = Some(materialReply),
      terminalPlyOffset = Some(2)
    )
    val mergedResult = ResolvedPlanEvent.mergePlanResults(List(
      conditionalRestriction,
      conditionalRestriction.copy(
        conditions = Nil,
        refutations = List(refutedResponse),
        supersessions = List(supersededResponse)
      )
    )).headOption.getOrElse(fail("expected merged result"))
    assertEquals(mergedResult.conditions, List(realizedResponse))
    assertEquals(mergedResult.refutations, List(refutedResponse))
    assertEquals(mergedResult.supersessions, List(supersededResponse))
    val boundResultJson = MoveMeaningSurface.publicPlanResultJson(conditionalRestriction.copy(
      refutations = List(refutedResponse),
      supersessions = List(supersededResponse)
    ))
    assertEquals(
      (boundResultJson \ "conditions").as[List[JsObject]].map(result => (result \ "reply" \ "uci").as[String]),
      List(restrictedMove.uci)
    )
    assertEquals(
      (boundResultJson \ "refutations").as[List[JsObject]].map(result => (result \ "effect_on_plan").as[String]),
      List("the tested line does not confirm the planned result")
    )
    assertEquals(
      (boundResultJson \ "supersessions").as[List[JsObject]].map(result => (result \ "effect_on_plan").as[String]),
      List("the game takes a different course")
    )
    assertEquals(
      (boundResultJson \ "supersessions").as[List[JsObject]].map(result => (result \ "game_result").as[String]),
      List("draw")
    )
    val unboundResultJson = MoveMeaningSurface.publicPlanResultJson(conditionalRestriction.copy(conditions = Nil))
    assert((unboundResultJson \ "conditions").toOption.isEmpty, unboundResultJson)
    assert((unboundResultJson \ "refutations").toOption.isEmpty, unboundResultJson)
    assert((unboundResultJson \ "supersessions").toOption.isEmpty, unboundResultJson)
    val refutedResponseSurface = realizedResponseSurface.copy(
      target = MoveMeaningSurfaceTarget(squares = List("e6")),
      resolvedPlanEvent = Some(responseEvent.copy(
        results = List(conditionalRestriction.copy(conditions = List(refutedResponse))),
        responses = List(refutedResponse),
        representativeResult = Some(conditionalRestriction.copy(conditions = List(refutedResponse)))
      )),
      idea = MoveMeaningSurfaceCode("counterplay_control", "refuted reply branch")
    )
    val conditionalBehindDirect =
      MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(realizedResponseSurface, supportingExactPressure))
    assertEquals(conditionalBehindDirect.headOption, Some(supportingExactPressure))
    assert(conditionalBehindDirect.contains(realizedResponseSurface), conditionalBehindDirect)
    val conditionalBehindRepresentative =
      MoveMeaningSurface.publicIdeaChainDisplaySemantics(List(realizedResponseSurface, owned))
    assertEquals(conditionalBehindRepresentative.headOption, Some(owned))
    assert(conditionalBehindRepresentative.contains(realizedResponseSurface), conditionalBehindRepresentative)
    List(
      List(refutedResponseSurface, realizedResponseSurface),
      List(realizedResponseSurface, refutedResponseSurface)
    ).foreach { responseOrder =>
      assertEquals(
        MoveMeaningSurface.publicIdeaChainDisplaySemantics(responseOrder).headOption,
        Some(realizedResponseSurface)
      )
    }

    val directPressureSignature =
      "actor=Move:f5h3|actor=Piece:bishop|target=Piece:pawn|target=Square:g2|mechanism=Mechanism:targetpressuregain|consequence=Consequence:targetpressuregain:gain"
    val kingRingSignature =
      "actor=Move:f5h3|actor=Piece:bishop|target=Piece:king|target=Square:g1|mechanism=Mechanism:kingringpressuregain|consequence=Consequence:kingringpressuregain:gain"
    val lookalikePressureSignature =
      directPressureSignature.replace("targetpressuregain", "targetpressuregainspeculative")
    val directTargetDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      structuralRouteMove = Some("f5h3"),
      structuralConsequenceKinds = List(TransitionConsequenceKind.TargetPressureGain)
    )
    assertEquals(
      MoveMeaningClaim.surfaceObjectBindingSignatures(
        directTargetDetail,
        List(kingRingSignature, lookalikePressureSignature, directPressureSignature),
        "f5h3"
      ),
      List(directPressureSignature)
    )
    assertEquals(
      MoveMeaningClaim.surfaceObjectBindingSignatures(
        directTargetDetail,
        List(directPressureSignature),
        "d1a4"
      ),
      Nil
    )

    val pressureRoot = PositionNodeRef(
      "4k3/1p6/8/8/8/8/8/3QK3 w - - 0 1",
      1,
      Some(Color.White),
      Some("pressure-root")
    )
    val pressureAfter = PositionNodeRef(
      "4k3/1p6/8/8/8/1Q6/8/4K3 b - - 1 1",
      2,
      Some(Color.Black),
      Some("pressure-after")
    )
    val pressureLine = LineNodeRef("pressure-line", "d1b3", 1, LineNodeRole.Played)
    val pressureRef = evidenceRef(
      id = "structural-delta:played:d1b3",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = pressureRoot,
      line = Some(pressureLine),
      scope = EvidenceScope.PlayedTransition
    )
    val pressureGraph = TypedEvidenceGraph(List(EvidenceRecord(
      pressureRef,
      StructuralDeltaEvidence(
        transition = StructuralTransitionBinding(
          moveUci = "d1b3",
          role = TransitionEdgeRole.Played,
          from = pressureRoot,
          to = pressureAfter,
          line = Some(pressureLine),
          perspective = Color.White
        ),
        signals = Nil,
        consequences = List(TransitionConsequence(
          kind = TransitionConsequenceKind.TargetPressureGain,
          polarity = StructuralSignalPolarity.Gain,
          strength = 2,
          subjects = List("pawn:b7"),
          targetSubjects = List("pawn:b7")
        ))
      )
    )))
    val pressureDetail = directTargetDetail.copy(
      structuralRouteMove = Some("d1b3"),
      structuralPurposeSubjects = List("pawn:b7"),
      sourceEvidenceIds = List(pressureRef.id)
    )
    val pressureSignature =
      "actor=Move:d1b3|actor=Piece:queen|target=Piece:pawn|target=Square:b7|mechanism=Mechanism:targetpressuregain|consequence=Consequence:targetpressuregain:gain"
    assert(MoveMeaningClaim.directStructuralImmediateTargetPressureReady(
      pressureGraph,
      pressureDetail,
      List(pressureSignature),
      "d1b3"
    ))
    assert(!MoveMeaningClaim.directStructuralImmediateTargetPressureReady(
      pressureGraph,
      pressureDetail,
      List(pressureSignature),
      "d1a4"
    ))

    val repeatedLine = List("h6h7", "e3a7", "h7h6", "a7d4", "h6h7", "d4d7", "h7h6")
    val ownedWithLine = owned.copy(comparison = Some(MoveMeaningSurfaceComparison(
      kind = "played-vs-best",
      relation = "played",
      referenceMove = "d7d6",
      candidateMove = "d8d7",
      moves =
        MoveMeaningSurfaceMoveRef("played_move", "d8d7") ::
          repeatedLine.zipWithIndex.map((move, index) => MoveMeaningSurfaceMoveRef(s"played_pv_${index + 1}", move)) ++
          List(
            MoveMeaningSurfaceMoveRef("played_pv_plan_6", "f6f5"),
            MoveMeaningSurfaceMoveRef("played_pv_1", "c1c2"),
            MoveMeaningSurfaceMoveRef("played_pv_2", "c2c3")
          )
    )))
    val competingWithLine = competingPlan.copy(comparison = Some(MoveMeaningSurfaceComparison(
      kind = "played-vs-best",
      relation = "played",
      referenceMove = "d7d6",
      candidateMove = "d8d7",
      moves = List(
        MoveMeaningSurfaceMoveRef("played_move", "d8d7"),
        MoveMeaningSurfaceMoveRef("played_pv_1", "a2a3"),
        MoveMeaningSurfaceMoveRef("played_pv_2", "a7a6")
      )
    )))
    assertEquals(
      MoveMeaningSurface.publicPvLine(List(ownedWithLine, competingWithLine), "played_move", "played_pv_"),
      "d8d7" :: repeatedLine
    )
    assertEquals(
      MoveMeaningSurface.publicPvLine(List(ownedWithLine.copy(comparison = None), competingWithLine), "played_move", "played_pv_"),
      Nil
    )

  test("root-capture material projection requires an ordinary gain on the root event"):
    val move = "e2f3"
    val captureCarrier = MoveMeaningSurfaceBoardCarrier(
      "target",
      "PlanSubject",
      "material-capture:f3",
      semanticRole = Some("material_capture")
    )
    def terminalDetail(kind: String, eventMove: String) = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      semanticAnchorKeys = List(s"LineConsequence:TerminalProof:$kind:$eventMove"),
      terminalConsequenceKinds = List(kind)
    )

    assert(MoveMeaningClaim.rootCaptureMaterialTimingReady(
      TypedEvidenceGraph.empty,
      terminalDetail("MaterialGain", move),
      move,
      List(captureCarrier)
    ))
    assert(!MoveMeaningClaim.rootCaptureMaterialTimingReady(
      TypedEvidenceGraph.empty,
      terminalDetail("MaterialGain", "f3f7"),
      move,
      List(captureCarrier)
    ))
    assert(!MoveMeaningClaim.rootCaptureMaterialTimingReady(
      TypedEvidenceGraph.empty,
      terminalDetail("MaterialLoss", "f3f7"),
      move,
      List(captureCarrier)
    ))
    assert(!MoveMeaningClaim.rootCaptureMaterialTimingReady(
      TypedEvidenceGraph.empty,
      terminalDetail("MaterialGain", move),
      move,
      List(captureCarrier, captureCarrier.copy(value = "material-sacrifice:f3", semanticRole = Some("material_sacrifice")))
    ))
    assert(MoveMeaningClaim.rootCaptureMaterialTimingReady(
      TypedEvidenceGraph.empty,
      terminalDetail("MaterialLoss", "f3f7"),
      move,
      Nil
    ))

    val position = PositionNodeRef("8/8/8/8/8/5p2/4K3/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val square = Square.fromKey("f3").getOrElse(fail("expected f3"))
    val recapture = Motif.Capture(
      piece = King,
      captured = Pawn,
      square = square,
      captureType = Motif.CaptureType.Recapture,
      color = Color.White,
      plyIndex = 0,
      move = Some(move)
    )
    val recaptureEvent = MoveMotifEvent.fromMotif(
      rootMove = move,
      motif = recapture,
      position = position,
      line = None,
      scope = EvidenceScope.PlayedTransition
    )
    val recaptureRef = evidenceRef(
      id = "move-motif:root-recapture",
      producer = EvidenceProducer.MoveMotifProducer,
      layer = EvidenceLayer.MoveMotif,
      position = position,
      line = None,
      scope = EvidenceScope.PlayedTransition
    )
    val recaptureGraph = TypedEvidenceGraph(List(EvidenceRecord(recaptureRef, MoveMotifEvidence(recaptureEvent))))
    assert(!MoveMeaningClaim.rootCaptureMaterialTimingReady(
      recaptureGraph,
      terminalDetail("MaterialGain", move),
      move,
      List(captureCarrier)
    ))
    assert(!MoveMeaningClaim.rootCaptureMaterialTimingReady(
      recaptureGraph,
      terminalDetail("MaterialGain", move),
      move,
      Nil
    ))

  test("a representative future episode names the plan rather than only the root route"):
    val root = NumberedChessMove("h1d1", "Rd1", ChessTurn(17, "white", "17."), "17.Rd1")
    val middle = NumberedChessMove("d1d3", "Rd3", ChessTurn(19, "white", "19."), "19.Rd3")
    val resultMove = NumberedChessMove("d3h3", "Rh3", ChessTurn(22, "white", "22."), "22.Rh3")
    val sequence = List(
      PlanSequenceMove("h1d1", Some(root), Some("rook"), Some("h1"), Some("d1"), Nil),
      PlanSequenceMove(
        "d1d3",
        Some(middle),
        Some("rook"),
        Some("d1"),
        Some("d3"),
        List(PlanCausalDependencyKind.ObjectStatePrecondition)
      ),
      PlanSequenceMove(
        "d3h3",
        Some(resultMove),
        Some("rook"),
        Some("d3"),
        Some("h3"),
        List(PlanCausalDependencyKind.ObjectStatePrecondition)
      )
    )
    val pressure = PlanResult(
      stage = "future",
      kind = TransitionConsequenceKind.BatteryPressureGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("pawn:h7"),
      source = Some(resultMove),
      robustness = Some(PlanCausalRobustness.Conditional)
    )
    val continuation = TestedPlanContinuation(
      dependencyKind = PlanCausalDependencyKind.ObjectStatePrecondition,
      futureMove = "d1d3",
      targetSquare = Some("h7"),
      plyOffset = 4,
      robustness = PlanCausalRobustness.Conditional,
      realizedReplies = 1,
      exactReplies = 1,
      equivalentReplies = 0,
      testedReplies = 3,
      expectedReplies = 3,
      sequence = sequence,
      representativeResult = Some(pressure)
    )
    val event = ResolvedPlanEvent(
      goalTheme = "PieceRedeployment",
      goalKind = Some("WorstPieceImprovement"),
      moveRole = Some(PlanMoveRole.Preparation),
      transitionType = None,
      rootMove = "h1d1",
      actorRole = Some("rook"),
      actorFrom = Some("h1"),
      actorTo = Some("d1"),
      targets = List("pawn:h7"),
      developmentChoices = Nil,
      results = List(pressure),
      responses = Nil,
      testedContinuation = Some(continuation),
      rootMoveReference = Some(root)
    )
    val claim = MoveMeaningClaim(
      meaningKind = "PieceRoute",
      role = "ImprovesPieceRoute",
      laneKey = "future-rook-route",
      conflictKey = None,
      supportLevel = "owned_cause_linked",
      visibility = "reason_grade",
      surfaceLane = "current_move_owned",
      lineRole = "candidate",
      moveUci = "h1d1",
      frameId = "future-rook-route-frame",
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = None,
      axisKind = None,
      axisPolarity = None,
      label = None,
      causeKinds = Nil,
      causeSourceSides = Nil,
      causeEvidenceIds = Nil,
      sourceEvidenceIds = List("plan-event"),
      objectBindingSignatures = Nil,
      positiveFunctionalProofEvidenceIds = List("plan-event"),
      routeIdentityParts = List("piece:rook", "from:h1", "to:d1"),
      publicSurfaceAdmitted = true,
      publicProofLevel = "owned_function",
      publicTargetBound = true,
      resolvedPlanEvent = Some(event),
      representativePlanEvent = true
    )
    val view = MoveJudgmentView(
      verdict = None,
      verdictCarriers = Nil,
      supportContextClusterIds = Nil,
      overriddenLocalIdeas = Nil,
      preservedLocalIdeas = Nil,
      moveMeaningClaims = List(claim)
    )

    val surface = MoveMeaningSurface.publicSurfaces(view).headOption.getOrElse(fail("expected public plan surface"))
    assertEquals(
      MoveMeaningSurface.publicIdeaDisplayLabel(surface),
      "prepares Rd3 then Rh3 to increase pressure on pawn on h7"
    )
    assertEquals(
      MoveMeaningSurface.publicIdeaDisplayLabel(
        surface.copy(
          moveUci = middle.uci,
          idea = MoveMeaningSurfaceCode("target_pressure", "rook pressure on h7"),
          resolvedPlanEvent = Some(
            event.copy(
              rootMove = middle.uci,
              historySequence = sequence.take(2),
              testedContinuation = None,
              rootMoveReference = Some(middle)
            )
          )
        )
      ),
      "rook pressure on h7"
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
