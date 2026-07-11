package lila.chessjudgment.analysis.assembly

import chess.Color
import lila.chessjudgment.model.strategic.VariationLine
import lila.chessjudgment.model.judgment.*

class ChessIdeaAssemblerTest extends munit.FunSuite:

  test("assembled plan pressure and pawn structure evidence stays acyclic"):
    val graph = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = chess.variant.Standard.initialFen.value,
          playedMoveUci = "d2d4",
          variations = List(
            VariationLine(List("e2e4", "e7e5", "g1f3"), scoreCp = 30, depth = 16),
            VariationLine(List("d2d4", "d7d5", "g1f3"), scoreCp = 20, depth = 16)
          ),
          currentEvalCp = Some(20),
          ply = Some(1)
        )
      )
      .getOrElse(fail("expected assembled evidence graph"))
      .context
      .evidenceGraph

    assert(graph.records.exists(_.payload.isInstanceOf[PlanPressureEvidence]))
    assert(graph.records.exists(_.payload.isInstanceOf[PawnStructureFactEvidence]))
    assertEquals(
      graph.records.filter(record => graph.parentClosure(record).exists(_.ref.id == record.ref.id)).map(_.ref.id),
      Nil
    )

  test("same-root plan function reaches public surface with line-owned proof"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "rnbqkb1r/pp1ppppp/5n2/2pP4/2P5/8/PP2PPPP/RNBQKBNR b KQkq - 0 3",
          playedMoveUci = "b7b5",
          variations = List(
            VariationLine(
              List("b7b5", "c4b5", "a7a6", "b5a6", "g7g6", "b1c3", "f8g7", "e2e4", "e8g8", "g1f3", "d7d6", "f1e2"),
              scoreCp = 0,
              depth = 12
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(5),
          openingContext = Some(RawOpeningContext(name = Some("Benko Gambit / queenside file pressure"))),
          movePrefixUci = List("d2d4", "g8f6", "c2c4", "c7c5", "d4d5")
        )
      )
      .getOrElse(fail("expected judgment result"))
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected move judgment view"))
    val claim = view.moveMeaningClaims
      .find(claim =>
        claim.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
          claim.lineRole == "candidate" &&
          claim.moveUci == "b7b5" &&
          claim.publicSurfaceAdmitted &&
          claim.positiveFunctionalProofEvidenceIds.nonEmpty
      )
      .getOrElse(fail(view.moveMeaningClaims.toString))

    assertEquals(claim.causeEvidenceIds, Nil)
    assertEquals(claim.supportLevel, "owned_cause_linked")
    assertEquals(claim.publicProofLevel, "owned_function")
    assert(claim.publicSurfaceAdmitted)
    assertEquals(claim.positiveFunctionalProofEvidenceIds.size, 3)
    assert(claim.objectBindingSignatures.exists(signature =>
      signature.contains("target=PlanSubject:queensideattack") &&
        signature.contains("target=Square:c4") &&
        signature.contains("actor=Move:b7b5")
    ), claim.objectBindingSignatures)
    val proofRecords = claim.positiveFunctionalProofEvidenceIds.flatMap(result.packet.evidenceGraph.byId.get)
    assertEquals(proofRecords.count(_.payload.isInstanceOf[PlanPressureEvidence]), 1)
    assertEquals(proofRecords.count(_.payload.isInstanceOf[StructuralDeltaEvidence]), 1)
    assertEquals(proofRecords.count(_.payload.isInstanceOf[LineFactEvidence]), 1)
    assert(proofRecords.forall(_.ref.line.exists(_.role == LineNodeRole.Played)), proofRecords)
    assertEquals(
      proofRecords.collectFirst { case EvidenceRecord(_, payload: LineFactEvidence, _) => payload.futureRootPawnAdvance() }.flatten,
      None
    )
    assertEquals(
      proofRecords.collectFirst { case EvidenceRecord(_, payload: LineFactEvidence, _) => payload.futureRootObjectMove() }.flatten,
      None
    )

  test("line trajectory preserves the moved knight across future plies"):
    val graph = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = chess.variant.Standard.initialFen.value,
          playedMoveUci = "g1f3",
          variations = List(VariationLine(List("g1f3", "g8f6", "f3e5"), scoreCp = 0, depth = 12)),
          currentEvalCp = Some(0),
          ply = Some(1)
        )
      )
      .getOrElse(fail("expected assembled evidence graph"))
      .context
      .evidenceGraph
    val trajectory = graph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) =>
        payload.futureRootObjectMove()
    }.flatten.getOrElse(fail("expected future knight trajectory"))

    assertEquals(trajectory.rootStep.moveUci, "g1f3")
    assertEquals(trajectory.futureStep.moveUci, "f3e5")
    assertEquals(trajectory.pieceRole.name.toLowerCase, "knight")
    assertEquals(trajectory.rootFrom.key -> trajectory.rootTo.key, "g1" -> "f3")
    assertEquals(trajectory.futureFrom.key -> trajectory.futureTo.key, "f3" -> "e5")
    assertEquals(trajectory.plyOffset, 2)

  test("Doknjas b5 plan carries its later b4 realization without another analysis pass"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "r1b2rk1/pp2ppbp/5np1/q1nP4/4PB2/2N2P2/PP4PP/2RQKBNR b K - 0 10",
          playedMoveUci = "b7b5",
          variations = List(
            VariationLine(
              List("b7b5", "b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4", "c3b5", "a3a5", "c2c5"),
              scoreCp = 0,
              depth = 12
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(19),
          openingContext = Some(RawOpeningContext(name = Some("Grunfeld / b5-b4 counterplay versus center"))),
          movePrefixUci = List(
            "d2d4", "g8f6", "c2c4", "g7g6", "b1c3", "d7d5", "c1f4", "f8g7", "e2e3", "c7c5",
            "d4c5", "d8a5", "a1c1", "e8h8", "c4d5", "b8d7", "f2f3", "d7c5", "e3e4"
          )
        )
      )
      .getOrElse(fail("expected judgment result"))
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected move judgment view"))
    val claim = view.moveMeaningClaims
      .find(claim =>
        claim.unit == PositionPlanTechniqueUnit.StructuralTransformation &&
          claim.lineRole == "candidate" &&
          claim.moveUci == "b7b5" &&
          claim.supportLevel == "owned_cause_linked" &&
          claim.positiveFunctionalProofEvidenceIds.nonEmpty &&
          claim.publicSurfaceAdmitted
      )
      .getOrElse(fail(view.moveMeaningClaims.toString))

    assertEquals(claim.positiveFunctionalProofEvidenceIds.size, 3)
    assertEquals(claim.publicProofLevel, "owned_function")
    assertEquals(claim.principalPlanId, Some(lila.chessjudgment.model.PlanId.QueensideAttack))
    assert(claim.targetSquares.contains("b4") && claim.targetFiles.contains("b"), claim)
    assert(claim.objectBindingSignatures.exists(signature =>
      signature.contains("actor=Move:b7b5") &&
        signature.contains("target=Square:b4") &&
        signature.contains("target=File:b") &&
        signature.contains("witness=Move:b5b4") &&
        signature.contains("horizon=ply:8")
    ), claim.objectBindingSignatures)
    val publicPlanClaim = view.moveMeaningClaims
      .find(candidate =>
        candidate.unit == PositionPlanTechniqueUnit.PlanOptionSet &&
          candidate.moveUci == "b7b5" &&
          candidate.publicSurfaceAdmitted &&
          candidate.publicProofLevel == "owned_function" &&
          candidate.targetSquares.contains("b4") &&
          candidate.comparisonMoveRefs.exists(_.uci == "b5b4")
      )
      .getOrElse(fail(view.moveMeaningClaims.toString))
    assertEquals(publicPlanClaim.principalPlanId, Some(lila.chessjudgment.model.PlanId.QueensideAttack))
    val proofRecords = claim.positiveFunctionalProofEvidenceIds.flatMap(result.packet.evidenceGraph.byId.get)
    assertEquals(proofRecords.count(_.payload.isInstanceOf[PlanPressureEvidence]), 1)
    assertEquals(proofRecords.count(_.payload.isInstanceOf[StructuralDeltaEvidence]), 1)
    assertEquals(proofRecords.count(_.payload.isInstanceOf[LineFactEvidence]), 1)
    assertEquals(
      proofRecords.collectFirst { case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        payload.futureRootPawnAdvance().map { case (step, offset) => step.moveUci -> offset }
      }.flatten,
      Some("b5b4" -> 8)
    )

  test("structural transition proof can seed a strategic relative-cause idea"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)

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
      strength = 2
    )
    val cause = RelativeCauseFact(
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
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:structural-improvement",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(
            transition = transition,
            signals = Nil,
            consequences = List(consequence)
          )
        ),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(structuralRef))
      )
    )

    val assembly = ChessIdeaAssembler.enrich(
      RelativeAssessmentAssembly(
        input = normalizedInput(root, afterPlayed),
        context = JudgmentAssemblyContext.empty(graph)
      )
    )

    val relativeCauseIdeas = assembly.context.ideas.filter(_.evidence.exists(_.id == causeRef.id))
    assertEquals(relativeCauseIdeas.map(_.ref.family), List(ChessIdeaFamily.Strategic))

  private def normalizedInput(
      root: PositionNodeRef,
      afterPlayed: PositionNodeRef
  ): NormalizedMoveReviewInput =
    NormalizedMoveReviewInput(
      beforeFen = root.fen,
      playedMoveUci = "d2d4",
      beforePly = root.ply,
      sideToMove = Some(Color.White),
      afterPlayedFen = afterPlayed.fen,
      afterReferenceFen = None,
      lines = Nil,
      currentWhitePovEvalCp = 0,
      opening = None
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
