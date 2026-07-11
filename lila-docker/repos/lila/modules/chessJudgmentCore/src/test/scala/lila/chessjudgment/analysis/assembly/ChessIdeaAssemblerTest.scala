package lila.chessjudgment.analysis.assembly

import chess.Color
import lila.chessjudgment.model.ProbeResult
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
    assertEquals(result.packet.probeRequests, Nil)
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected move judgment view"))
    val claim = view.moveMeaningClaims
      .find(claim =>
        claim.unit == PositionPlanTechniqueUnit.PlanOptionSet &&
          claim.meaningKind == "PlanContinuity" &&
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
    assertEquals(claim.positiveFunctionalProofEvidenceIds.size, 1)
    assertEquals(
      view.moveMeaningClaims.count(candidate =>
        candidate.lineRole == "candidate" &&
          candidate.moveUci == "b7b5" &&
          candidate.positiveFunctionalProofEvidenceIds.nonEmpty
      ),
      1
    )
    assert(claim.objectBindingSignatures.exists(signature =>
      signature.contains("target=PlanSubject:queensideattack") &&
        signature.contains("target=Square:c4") &&
        signature.contains("actor=Move:b7b5")
    ), claim.objectBindingSignatures)
    val proofRecords = claim.positiveFunctionalProofEvidenceIds.flatMap(result.packet.evidenceGraph.byId.get)
    assertEquals(proofRecords.count(_.payload.isInstanceOf[PlanCausalEventEvidence]), 1)
    assert(proofRecords.forall(_.ref.line.exists(_.role == LineNodeRole.Played)), proofRecords)
    val eventParents = proofRecords.flatMap(_.parents).flatMap(parent => result.packet.evidenceGraph.byId.get(parent.id))
    assert(eventParents.exists(_.payload.isInstanceOf[PlanPressureEvidence]))
    assert(eventParents.exists(_.payload.isInstanceOf[StructuralDeltaEvidence]))
    assert(eventParents.exists(_.payload.isInstanceOf[LineFactEvidence]))
    assert(eventParents.exists(_.payload.isInstanceOf[MoveTransitionEvidence]))
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

  test("line trajectory does not transfer identity to a same-role replacement"):
    val graph = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "4k3/8/8/8/6b1/8/3N4/4K1N1 w - - 0 1",
          playedMoveUci = "g1f3",
          variations = List(
            VariationLine(List("g1f3", "g4f3", "d2f3", "e8e7", "f3e5"), scoreCp = 0, depth = 12)
          ),
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
        payload.futureRootObjectMove(payload.lineReplayCount)
    }.flatten

    assertEquals(trajectory, None)

  test("future plan event stays internal until reply robustness is observed"):
    val result = MoveReviewJudgmentOrchestrator
      .build(doknjasInput())
      .getOrElse(fail("expected judgment result"))
    val eventRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(payload.rootMove, "b7b5") && payload.futureMove.exists(EvidenceRef.sameMove(_, "b5b4")) =>
        record -> payload
    }.getOrElse(fail("expected causal event"))

    assert(eventRecord._2.counterfactualDependencyProven)
    assertEquals(eventRecord._2.robustness, PlanCausalRobustness.Untested)
    assert(!eventRecord._2.futurePublicProofReady)
    assertEquals(result.packet.probeRequests.flatMap(_.candidateMove), List("b7b5"))
    assert(!EvidenceObjectBinding.fromEvidenceRefs(result.packet.evidenceGraph, List(eventRecord._1.ref)).exists(binding =>
      binding.witness.exists(obj => obj.kind == EvidenceObjectKind.Move && obj.key == "b5b4")
    ))

  test("reply probe certifies a robust future plan event without hard-coded position rules"):
    val input = doknjasInput()
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(input, List(
        VariationLine(List("b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("h2h3", "b5b4"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")

    assert(result.isValid, result.validation.issues)
    assertEquals(event.branchWitnesses.size, 3)
    assert(event.branchWitnesses.forall(_.outcome == PlanCausalBranchOutcome.Realized), event.branchWitnesses)
    assert(event.branchWitnesses.forall(_.realizationMatch.contains(PlanCausalRealizationMatch.ExactMove)), event.branchWitnesses)
    assertEquals(event.robustness, PlanCausalRobustness.Robust)
    assert(event.futurePublicProofReady)
    assertEquals(result.packet.probeRequests, Nil)
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected move judgment view"))
    val publicClaim = view.moveMeaningClaims.find(claim =>
      claim.publicSurfaceAdmitted &&
        claim.moveUci == "b7b5" &&
        claim.futureCausalProof.exists(proof => proof.futureMove == "b5b4" && proof.targetSquare == "b4")
    ).getOrElse(fail(view.moveMeaningClaims.toString))
    assertEquals(publicClaim.unit, PositionPlanTechniqueUnit.PlanOptionSet)
    assertEquals(
      view.moveMeaningClaims.count(claim =>
        claim.lineRole == "candidate" &&
          claim.moveUci == "b7b5" &&
          claim.positiveFunctionalProofEvidenceIds.nonEmpty
      ),
      1
    )
    val publicProof = publicClaim.futureCausalProof.getOrElse(fail("expected public causal proof"))
    assertEquals(publicProof.dependencyKind, PlanCausalDependencyKind.ObjectStatePrecondition)
    assertEquals(publicProof.plyOffset, 8)
    assertEquals(publicProof.robustness, PlanCausalRobustness.Robust)
    assertEquals(publicProof.realizedReplies, 3)
    assertEquals(publicProof.exactReplies, 3)
    assertEquals(publicProof.equivalentReplies, 0)
    assertEquals(publicProof.testedReplies, 3)
    assertEquals(publicClaim.positiveFunctionalProofEvidenceIds.size, 1)
    assert(result.packet.evidenceGraph.byId(publicClaim.positiveFunctionalProofEvidenceIds.head).payload.isInstanceOf[PlanCausalEventEvidence])
    val causalSurfaces = MoveMeaningSurface.publicSurfaces(view).filter(_.causalPlan.nonEmpty)
    assertEquals(causalSurfaces.size, 1)
    assertEquals(causalSurfaces.head.priority, "main")
    assert(causalSurfaces.exists(_.causalPlan.exists(plan =>
      plan.futureMove == "b5b4" &&
        plan.robustness == PlanCausalRobustness.Robust &&
        plan.exactReplies == 3 &&
        plan.equivalentReplies == 0 &&
        plan.testedReplies == 3
    )))
    val publicJson = MoveMeaningSurface.publicPayloadJson(view)
    assert((publicJson \\ "future_move").exists(_.asOpt[String].contains("b5b4")), publicJson)
    assert((publicJson \\ "exact_replies").exists(_.asOpt[Int].contains(3)), publicJson)
    assert((publicJson \\ "equivalent_replies").exists(_.asOpt[Int].contains(0)), publicJson)

  test("future realization accepts a different move only when typed function and subject agree"):
    val expected = TransitionConsequence(
      TransitionConsequenceKind.TargetPressureGain,
      StructuralSignalPolarity.Gain,
      2,
      List("weak-pawn:c4")
    )
    val equivalent = expected.copy(strength = 1)
    val differentTarget = expected.copy(subjects = List("weak-pawn:f4"))

    assertEquals(
      PlanCausalFunctionalMatch.classify("b5b4", List(expected), "b5a4", List(equivalent)),
      Some(PlanCausalRealizationMatch.EquivalentFunction)
    )
    assertEquals(
      PlanCausalFunctionalMatch.classify("b5b4", List(expected), "b5a4", List(differentTarget)),
      None
    )

  test("reply probe preserves conditionality instead of treating an unfinished branch as refutation"):
    val input = doknjasInput()
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(input, List(
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("h2h3", "a7a6"), 0, depth = 16),
        VariationLine(List("g1h3", "e7e6"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")

    assert(result.isValid, result.validation.issues)
    assertEquals(event.branchWitnesses.count(_.outcome == PlanCausalBranchOutcome.Realized), 1)
    assertEquals(event.branchWitnesses.count(_.outcome == PlanCausalBranchOutcome.Deferred), 2)
    assertEquals(event.robustness, PlanCausalRobustness.Conditional)
    assert(event.futurePublicProofReady)

  test("validator rejects a causal event whose branch outcome was re-labeled downstream"):
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(doknjasInput(), List(
        VariationLine(List("b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("h2h3", "b5b4"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val causalRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.futureMove.exists(EvidenceRef.sameMove(_, "b5b4")) =>
        record -> payload
    }.getOrElse(fail("expected causal record"))
    val tamperedPayload = causalRecord._2.copy(
      branchWitnesses = causalRecord._2.branchWitnesses match
        case head :: tail => head.copy(outcome = PlanCausalBranchOutcome.Refuted) :: tail
        case Nil          => fail("expected branch witnesses")
    )
    val tamperedGraph = TypedEvidenceGraph(result.packet.evidenceGraph.records.map { record =>
      if record.ref.id == causalRecord._1.ref.id then causalRecord._1.copy(payload = tamperedPayload)
      else record
    })
    val validation = JudgmentPacketValidator.validate(result.packet.copy(evidenceGraph = tamperedGraph))

    assert(validation.issues.exists(_.kind == JudgmentPacketValidationIssueKind.InvalidPlanCausalBranchProof), validation.issues)

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

  private def doknjasInput(probeResults: List[ProbeResult] = Nil): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r1b2rk1/pp2ppbp/5np1/q1nP4/4PB2/2N2P2/PP4PP/2RQKBNR b K - 0 10",
      playedMoveUci = "b7b5",
      variations = List(
        VariationLine(
          List("b7b5", "b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4", "c3b5", "a3a5", "c2c5"),
          scoreCp = 0,
          depth = 16
        )
      ),
      currentEvalCp = Some(0),
      ply = Some(19),
      openingContext = Some(RawOpeningContext(name = Some("Grunfeld / b5-b4 counterplay versus center"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "c2c4", "g7g6", "b1c3", "d7d5", "c1f4", "f8g7", "e2e3", "c7c5",
        "d4c5", "d8a5", "a1c1", "e8h8", "c4d5", "b8d7", "f2f3", "d7c5", "e3e4"
      ),
      probeResults = probeResults
    )

  private def withReplyProbe(input: RawMoveReviewInput, replyLines: List[VariationLine]): RawMoveReviewInput =
    val request = MoveReviewJudgmentOrchestrator
      .build(input)
      .getOrElse(fail("expected initial probe request"))
      .packet
      .probeRequests
      .find(_.candidateMove.exists(EvidenceRef.sameMove(_, "b7b5")))
      .getOrElse(fail("expected b7b5 reply probe request"))
    input.copy(probeResults = List(
      ProbeResult(
        id = request.id,
        fen = Some(request.fen),
        evalCp = 0,
        replyLines = Some(replyLines),
        deltaVsBaseline = 0,
        purpose = request.purpose,
        probedMove = request.candidateMove,
        depth = Some(request.depth),
        objective = request.objective,
        requiredSignals = request.requiredSignals,
        candidateMove = request.candidateMove,
        depthFloor = request.depthFloor,
        variationHash = request.variationHash
      )
    ))

  private def causalEvent(
      result: MoveReviewJudgmentResult,
      rootMove: String,
      futureMove: String
  ): PlanCausalEventEvidence =
    result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(payload.rootMove, rootMove) && payload.futureMove.exists(EvidenceRef.sameMove(_, futureMove)) =>
        payload
    }.getOrElse(fail("expected causal event"))

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
