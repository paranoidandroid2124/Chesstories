package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.position.{ PositionAnalyzer, PositionRelationExtractor }
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, StructuralDeltaContracts }
import lila.chessjudgment.model.{ ProbeResolution, ProbeResult, ProbeVariant }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  PrincipalVariationEvidence
}
import lila.chessjudgment.model.line.EngineLine
import lila.chessjudgment.model.PassedPawnResultKind

class PassedPawnResultProofTest extends munit.FunSuite:

  test("branch witnesses use the exact branch position move and rank"):
    val fen = "4k3/p7/3p4/8/4P3/8/8/4K1N1 w - - 0 1"
    val root = fixture(fen, List("g1f3"))
    val branchFen = root.structural.to.fen
    val wrongBranchFen = fixture(fen, List("g1h3")).structural.to.fen
    val replay = replayFrom(branchFen, List("a7a6"))
    val evaluation = CandidateLineEvaluation.EngineSearch(
      EngineLine(List("a7a6"), scoreCp = 0, depth = 20)
    )
    val normalized = AdmittedReviewLine(LineNodeRole.BranchReply, 1, evaluation, replay)
    val branch = AdmittedReviewBranchReply(
      sourceProbeId = "exact-branch",
      probedMoveUci = "g1f3",
      branchFen = branchFen,
      branchPly = 1,
      certifiedHorizonPlyOffset = 1,
      lines = List(normalized)
    )
    def candidate(id: String, move: String, positionFen: String): CandidateLineNode =
      val line = LineNodeRef(id, move, 1, LineNodeRole.BranchReply)
      CandidateLineNode(
        line,
        CandidateLineEvaluation.EngineSearch(EngineLine(List(move), scoreCp = 0, depth = 20)),
        EvidenceRef(
          id = s"$id-evidence",
          producer = EvidenceProducer.LegalLineProducer,
          layer = EvidenceLayer.Line,
          position = PositionNodeRef(positionFen, 1, Some(Black)),
          line = Some(line),
          scope = EvidenceScope.BranchReplyLine,
          confidence = EvidenceConfidence.LegalReplayVerified
        )
      )
    val wrongPosition = candidate("wrong-position", "a7a6", wrongBranchFen)
    val wrongMove = candidate("wrong-move", "a7a5", branchFen)
    val exact = candidate("exact", "a7a6", branchFen)
    val context = assemblyContext(root).copy(lines = List(wrongPosition, wrongMove, exact))

    assertEquals(
      PassedPawnResultEventAssembler.exactBranchReplyLineFor(context, branch, normalized).map(_.ref),
      Some(exact.ref)
    )
    assertEquals(
      PassedPawnResultEventAssembler
        .exactBranchReplyLineFor(context.copy(lines = context.lines :+ candidate("duplicate", "a7a6", branchFen)), branch, normalized),
      None,
      "multiple exact candidates must fail closed"
    )

  test("branch predecessor replay selection is complete, semantic, and order invariant"):
    val fen = "4k3/p7/3p4/8/4P3/8/8/4K1N1 w - - 0 1"
    val root = fixture(fen, List("g1f3"))
    val branchFen = root.structural.to.fen
    val continuation = replayFrom(branchFen, List("a7a6"))
    val evaluation = CandidateLineEvaluation.EngineSearch(
      EngineLine(List("a7a6"), scoreCp = 0, depth = 20)
    )
    def line(rank: Int, predecessor: Option[CanonicalLineReplay]) =
      AdmittedReviewLine(
        LineNodeRole.BranchReply,
        rank,
        evaluation,
        continuation,
        predecessor
      )
    def assembleBranch(
        branchPly: Int,
        exactBranchFen: String,
        lines: List[AdmittedReviewLine]
    ) =
      NodeLineTransitionAssembler
        .assemble(
          normalizedInput(root).copy(
            branchReplies = List(
              AdmittedReviewBranchReply(
                sourceProbeId = "predecessor-selection",
                probedMoveUci = "g1f3",
                branchFen = exactBranchFen,
                branchPly = branchPly,
                certifiedHorizonPlyOffset = 1,
                lines = lines
              )
            )
          )
        )
        .getOrElse(fail("expected the root input to assemble"))
    def admittedSummary(context: JudgmentAssemblyContext) =
      val transition = context.transition(TransitionEdgeRole.BranchReply)
      (
        context.position(PositionNodeRole.AfterBranchReply).map(_.ref.fen),
        transition.map(edge => (edge.moveUci, edge.to.fen)),
        transition.flatMap(context.transitionReplay).map(_.replaySteps)
      )

    val exact = root.replay.subset(List(root.replay.replaySteps.head))
      .getOrElse(fail("expected an exact predecessor"))
    val semanticDuplicate = replayFrom(fen, List("g1f3"))
    val firstMissing = List(line(1, None), line(2, Some(exact)), line(3, Some(semanticDuplicate)))
    val reversed = firstMissing.reverse
    val forwardSummary = admittedSummary(assembleBranch(1, branchFen, firstMissing))
    assertEquals(forwardSummary, admittedSummary(assembleBranch(1, branchFen, reversed)))
    assert(forwardSummary._1.nonEmpty, "a later exact predecessor must not be hidden by the first line")
    assert(forwardSummary._2.nonEmpty, "semantic duplicates must still produce one branch-reply transition")

    val initial = Standard.initialFen.value
    val pathA = replayFrom(initial, List("g1f3", "g8f6", "f3g1", "f6g8", "g1f3"))
    val pathB = replayFrom(initial, List("g1f3", "b8c6", "f3g1", "c6b8", "g1f3"))
    assert(PrincipalVariationEvidence.sameBoardState(
      pathA.replaySteps.last.fenAfter,
      pathB.replaySteps.last.fenAfter
    ))
    assert(NodeLineTransitionAssembler.uniqueSemanticPredecessor(List(pathA)).nonEmpty)
    assert(NodeLineTransitionAssembler.uniqueSemanticPredecessor(List(pathB)).nonEmpty)
    assertEquals(NodeLineTransitionAssembler.uniqueSemanticPredecessor(List(pathA, pathB)), None)
    assertEquals(NodeLineTransitionAssembler.uniqueSemanticPredecessor(List(pathB, pathA)), None)

  test("each newly created exact structural fact contributes exactly one result"):
    val pawnTension = analyzedFixture(
      "4k3/8/8/6p1/8/8/7P/4K3 w - - 0 1",
      List("h2h4")
    ).structural.consequences.filter(_.kind == TransitionConsequenceKind.PawnTensionCreated)
    assertEquals(pawnTension.flatMap(_.subjectFacts).size, 1)
    assertEquals(
      pawnTension.flatMap(_.subjectFacts.map(_.label)),
      List("created-tension:white:h4-g5")
    )

    val geometricControl = analyzedFixture(
      "4b1k1/8/8/8/8/8/8/3QK3 w - - 0 1",
      List("d1h5")
    ).structural.relationChanges.filter(change =>
      change.direction == RelationChangeDirection.Established &&
        (change.relation.detail match
          case RelationWitnessDetail.GeometricControl(White, attacker, _, target) =>
            attacker.key == "h5" && target.key == "e8"
          case _ => false)
    )
    assertEquals(geometricControl.size, 1)

  test("passed-pawn progress without an observed result route stays structural"):
    val result = analyzedFixture(
      "7k/8/P7/8/8/8/8/4K3 w - - 0 1",
      List("a6a7")
    )
    assert(result.structural.consequences.exists(
      _.kind == TransitionConsequenceKind.PassedPawnProgress
    ))
    val input = normalizedInput(result)
    assertEquals(
      PassedPawnResultEventAssembler.fromAssembly(
        input,
        assemblyContext(result),
        JudgmentProvenanceAllocator.forInput(input),
        assemblyContext(result).evidenceGraph.records.head
      ),
      Nil
    )

  test("a missing bounded result route remains deferred instead of proving passed-pawn-result refutation"):
    val raw = RawMoveReviewInput(
      fen = "7k/8/8/PP6/8/8/8/4K3 w - - 0 1",
      playedMoveUci = "a5a6",
      variations = List(
        EngineLine(List("e1e2", "h8g8", "e2e3"), scoreCp = 50, depth = 20),
        EngineLine(List("a5a6", "h8g8", "a6a7"), scoreCp = 0, depth = 20)
      )
    )
    val packet = MoveReviewJudgmentOrchestrator
      .execute(raw)
      .getOrElse(fail("expected an exact passed-pawn-result episode"))
    val event = packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, candidate: PassedPawnResultEventEvidence, _)
          if candidate.episode.exists(_.resultRoutes.nonEmpty) => candidate
    }.getOrElse(fail("expected one passed-pawn-result route"))
    val episode = event.episode.getOrElse(fail("expected a proven passed-pawn-result episode"))
    val route = episode.resultRoutes.head
    val duplicateContinuationFailure = intercept[IllegalArgumentException] {
      episode.copy(continuations = episode.continuations :+ episode.continuations.head)
    }
    assertEquals(
      duplicateContinuationFailure.getMessage,
      "requirement failed: duplicate exact passed-pawn-result continuations"
    )
    val duplicateDependencyFailure = intercept[IllegalArgumentException] {
      episode.copy(dependencies = episode.dependencies :+ episode.dependencies.head)
    }
    assertEquals(
      duplicateDependencyFailure.getMessage,
      "requirement failed: duplicate exact passed-pawn-result dependencies"
    )
    val duplicateRouteFailure = intercept[IllegalArgumentException] {
      episode.copy(resultRoutes = episode.resultRoutes :+ route)
    }
    assertEquals(
      duplicateRouteFailure.getMessage,
      "requirement failed: duplicate exact passed-pawn-result routes"
    )
    val sourceOffset = route.sourceEvent.step.ply - episode.root.step.ply
    val witness = PassedPawnReplyBranchWitness(
      sourceProbeId = "bounded-no-match",
      line = LineNodeRef("bounded-no-match", "h8g8", 1, LineNodeRole.BranchReply),
      observedEpisode = None,
      certifiedHorizonPlyOffset = sourceOffset,
      observedThroughPlyOffset = sourceOffset,
      terminalOutcome = None,
      terminalPlyOffset = None,
      terminalStep = None
    )
    val assessment = PassedPawnResultReplyAssessment.fromRoute(
      episode,
      route,
      List(witness),
      branchSetComplete = true
    )

    assertEquals(assessment.observations.map(_.outcome), List(PassedPawnResultBranchOutcome.Deferred))
    assertEquals(assessment.robustness, PassedPawnResultReplyCoverage.IncompleteReplyCoverage)

  test("passed-pawn-result event dispatch requires the exact PlayedVsBest demand and excludes alternatives"):
    val fen = Standard.initialFen.value
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    def normalized(
        role: LineNodeRole,
        rank: Int,
        moves: List[String],
        scoreCp: Int
    ): AdmittedReviewLine =
      AdmittedReviewLine(
        role,
        rank,
        CandidateLineEvaluation.EngineSearch(EngineLine(moves, scoreCp, depth = 20)),
        replayFrom(fen, moves)
      )
    val played = normalized(LineNodeRole.Played, 2, List("g1f3", "g8f6"), -20)
    val reference = normalized(LineNodeRole.BestReference, 1, List("b1c3", "b8c6"), 20)
    val alternative = normalized(LineNodeRole.Alternative, 3, List("e2e4", "e7e5"), 0)
    val input = AdmittedMoveReviewInput(
      beforeFen = fen,
      playedMoveUci = "g1f3",
      beforePly = 0,
      sideToMove = Some(White),
      afterPlayedFen = played.replay.replaySteps.head.fenAfter,
      afterReferenceFen = Some(reference.replay.replaySteps.head.fenAfter),
      lines = List(played, reference, alternative),
      completeCandidateSet = None,
      positionHistory = history
    )
    val facts = NodeLineTransitionAssembler
      .assemble(input)
      .map(EvidenceFactAssembler.enrich)
      .getOrElse(fail("expected the comparison fixture to assemble"))
    assert(!facts.evidenceGraph.records.exists(_.payload.isInstanceOf[PassedPawnResultEventEvidence]))
    val playedNode = facts.line(LineNodeRole.Played).getOrElse(fail("expected a played line"))
    val referenceNode = facts.line(LineNodeRole.BestReference).getOrElse(fail("expected a reference line"))
    val comparison = CandidateComparisonFact
      .fromLines(
        CandidateComparisonKind.PlayedVsBest,
        White,
        referenceNode,
        playedNode
      )
      .getOrElse(fail("expected an exact PlayedVsBest comparison"))
    val root = facts.root.getOrElse(fail("expected a comparison root"))
    def demand(id: String, fact: CandidateComparisonFact): EvidenceRecord =
      EvidenceRecord(
        EvidenceRef(
          id = id,
          producer = EvidenceProducer.RelativeMoveProducer,
          layer = EvidenceLayer.CandidateComparison,
          position = root,
          line = Some(fact.candidateLine),
          scope = EvidenceScope.Counterfactual,
          confidence = EvidenceConfidence.EngineBacked
        ),
        CandidateComparisonEvidence(fact),
        List(referenceNode.evidence, playedNode.evidence)
      )
    val activeDemand = demand("played-vs-best-demand", comparison)
    val demandedContext = facts.withEvidence(activeDemand)
    assertEquals(
      PassedPawnResultEventAssembler.demandedRootLines(demandedContext, activeDemand),
      Set(referenceNode.ref, playedNode.ref)
    )
    assert(!PassedPawnResultEventAssembler
      .demandedRootLines(demandedContext, activeDemand)
      .exists(_.role == LineNodeRole.Alternative))

    val inactiveDemand = demand(
      "best-vs-second-demand",
      comparison.copy(kind = CandidateComparisonKind.BestVsSecond)
    )
    val inactiveContext = demandedContext.withEvidence(inactiveDemand)
    assertEquals(
      PassedPawnResultEventAssembler.demandedRootLines(inactiveContext, inactiveDemand),
      Set.empty
    )

  test("submitted demanded passed-pawn-result probe is replayed through the same comparison demand stage"):
    val fen = "7k/8/8/PP6/8/8/8/4K3 w - - 0 1"
    val raw = RawMoveReviewInput(
      fen = fen,
      playedMoveUci = "a5a6",
      variations = List(
        EngineLine(List("e1e2", "h8g8", "e2e3"), scoreCp = 50, depth = 20),
        EngineLine(List("a5a6", "h8g8", "a6a7"), scoreCp = 0, depth = 20),
        EngineLine(List("b5b6", "h8g8", "b6b7"), scoreCp = -10, depth = 20)
      )
    )
    val initial = MoveReviewJudgmentOrchestrator
      .execute(raw)
      .getOrElse(fail("expected an initial passed-pawn-result review"))
    val comparison = initial.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => record
    }.getOrElse(fail("expected the primary comparison demand"))
    val playedPassedPawnResult = initial.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(ref, _: PassedPawnResultEventEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) => record
    }.getOrElse(fail("expected a demanded played-line passed-pawn-result event"))
    assert(
      playedPassedPawnResult.parents.contains(comparison.ref),
      clues(playedPassedPawnResult.parents, comparison.ref)
    )
    assert(!initial.evidenceGraph.records.exists {
      case EvidenceRecord(ref, _: PassedPawnResultEventEvidence, _) =>
        ref.line.exists(_.role == LineNodeRole.Alternative)
      case _ => false
    })
    assert(!initial.probeRequests.exists(request =>
      EvidenceRef.sameMove(request.candidateMove, "b5b6")
    ))
    val request = initial.probeRequests
      .find(request => EvidenceRef.sameMove(request.candidateMove, "a5a6"))
      .getOrElse(fail("expected the demanded played-line branch probe"))
    val malformed = ProbeResult(
      request.id,
      ProbeResolution.EngineSearch(Nil, request.depth)
    )
    val replayed = MoveReviewJudgmentOrchestrator
      .execute(raw.copy(probeResults = List(malformed)))
      .getOrElse(fail("expected the submitted review to remain reportable"))
    assert(replayed.probeRequests.exists(_.id == request.id))

  test("one root and reply receive only the maximum passed-pawn result horizon"):
    val raw = RawMoveReviewInput(
      fen = "7k/8/8/PP6/8/8/8/4K3 w - - 0 1",
      playedMoveUci = "a5a6",
      variations = List(
        EngineLine(List("e1e2", "h8g8", "e2e3"), scoreCp = 50, depth = 20),
        EngineLine(List("a5a6", "h8g8", "a6a7", "g8h8", "a7a8q"), scoreCp = 0, depth = 20)
      )
    )
    val initial = MoveReviewJudgmentOrchestrator
      .execute(raw)
      .getOrElse(fail("expected a bounded passed-pawn result review"))
    val playedRequests = initial.probeRequests.filter(request =>
      EvidenceRef.sameMove(request.candidateMove, raw.playedMoveUci)
    )
    assert(playedRequests.nonEmpty, "the exact legal replies must still be demanded")
    val horizons = playedRequests.map(_.variant).collect {
      case ProbeVariant.BranchReply(_, horizon) => horizon
    }
    assertEquals(horizons.distinct, List(4))
    assertEquals(playedRequests.map(_.moves).distinct.size, playedRequests.size)

  test("every exact legal reply must realize the passed-pawn result before one typed L2 proof is public"):
    val raw = RawMoveReviewInput(
      fen = "7k/8/8/PP6/8/8/8/4K3 w - - 0 1",
      playedMoveUci = "a5a6",
      variations = List(
        EngineLine(List("e1e2", "h8g8", "e2e3"), scoreCp = 50, depth = 20),
        EngineLine(List("a5a6", "h8g8", "a6a7"), scoreCp = 0, depth = 20)
      )
    )
    val initial = MoveReviewJudgmentOrchestrator
      .execute(raw)
      .getOrElse(fail("expected an initial bounded passed-pawn result review"))
    assert(!initial.evidenceGraph.records.exists(_.payload.isInstanceOf[PassedPawnResultProofEvidence]))

    val playedReplyRequests = initial.probeRequests.filter(request =>
      EvidenceRef.sameMove(request.candidateMove, raw.playedMoveUci)
    )
    assert(playedReplyRequests.nonEmpty, "the closed root inventory must demand every played-line reply")
    val probeResults = playedReplyRequests.map { request =>
      val reply = request.moves match
        case exact :: Nil => exact
        case other        => fail(s"expected one exact reply move, got $other")
      ProbeResult(
        request.id,
        ProbeResolution.EngineSearch(
          List(
            CandidateLineEvaluation.EngineSearch(
              EngineLine(List(reply, "a6a7"), scoreCp = 0, depth = request.depth)
            )
          ),
          request.depth
        )
      )
    }
    val resolved = MoveReviewJudgmentOrchestrator
      .execute(raw.copy(probeResults = probeResults))
      .getOrElse(fail("expected the closed reply review to assemble"))
    val proofRecord = resolved.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, _: PassedPawnResultProofEvidence, _) => record
    }.getOrElse(fail("expected one typed passed-pawn causal proof"))
    val proof = proofRecord.payload.asInstanceOf[PassedPawnResultProofEvidence]
    val comparisonRecord = resolved.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => record
    }.getOrElse(fail("expected the exact comparison demand"))

    assert(resolved.evidenceGraph.proofEligible(proofRecord))
    assertEquals(proof.legalReplyMoves, playedReplyRequests.flatMap(_.moves).map(EvidenceRef.normalizeMove).distinct.sorted)
    assertEquals(proof.replyBranches.size, proof.legalReplyMoves.size)
    assertEquals(proof.proofPaths.map(_.pathOccurrenceId).distinct.size, proof.proofPaths.size)
    assert(proof.proofPaths.size >= proof.replyBranches.size)
    assert(proofRecord.parents.forall(parent => proof.lowerPremiseIds.contains(parent.id)))

    val eventRecord = resolved.evidenceGraph
      .record(proof.eventSource)
      .getOrElse(fail("expected the exact passed-pawn result-event coverage record"))
    val publicInventory = proof.publicClosedReplyInventory
    val inventoryOwner = resolved.evidenceGraph.records
      .find(_.ref.id == publicInventory.issuerEvidenceId)
      .getOrElse(fail("expected the exact StructuralDelta reply-inventory owner"))
    assert(inventoryOwner.payload.isInstanceOf[StructuralDeltaEvidence])
    assert(eventRecord.parents.contains(inventoryOwner.ref))
    assert(proofRecord.parents.contains(inventoryOwner.ref))
    assert(proof.lowerPremiseIds.contains(inventoryOwner.ref.id))
    assertEquals(publicInventory.coverageEvidenceId, eventRecord.ref.id)
    assertNotEquals(publicInventory.issuerEvidenceId, publicInventory.coverageEvidenceId)
    assertEquals(
      PassedPawnResultProofAssembler.existingProofOwnersForExactDependencies(
        resolved.evidenceGraph,
        proof.rootLine,
        eventRecord,
        comparisonRecord,
        inventoryOwner,
        proof.assessment
      ).map(_.ref.id),
      List(proofRecord.ref.id),
      "the exact immutable dependency snapshot must reuse its existing passed-pawn result proof before derivation"
    )
    val changedComparison = comparisonRecord.copy(
      ref = comparisonRecord.ref.copy(id = s"${comparisonRecord.ref.id}-changed")
    )
    assertEquals(
      PassedPawnResultProofAssembler.existingProofOwnersForExactDependencies(
        resolved.evidenceGraph,
        proof.rootLine,
        eventRecord,
        changedComparison,
        inventoryOwner,
        proof.assessment
      ),
      Nil,
      "a stale proof must not be reused for a changed comparison owner"
    )
    val changedInventory = inventoryOwner.copy(
      ref = inventoryOwner.ref.copy(id = s"${inventoryOwner.ref.id}-changed")
    )
    assertEquals(
      PassedPawnResultProofAssembler.existingProofOwnersForExactDependencies(
        resolved.evidenceGraph,
        proof.rootLine,
        eventRecord,
        comparisonRecord,
        changedInventory,
        proof.assessment
      ),
      Nil,
      "a stale proof must not be reused for a changed closed-inventory owner"
    )
    val duplicateProofOwner = proofRecord.copy(
      ref = proofRecord.ref.copy(id = s"${proofRecord.ref.id}-ambiguous")
    )
    val ambiguousProofGraph = resolved.evidenceGraph.add(duplicateProofOwner)
    assert(ambiguousProofGraph.proofEligible(duplicateProofOwner))
    assertEquals(
      PassedPawnResultProofAssembler.existingProofOwnersForExactDependencies(
        ambiguousProofGraph,
        proof.rootLine,
        eventRecord,
        comparisonRecord,
        inventoryOwner,
        proof.assessment
      ).map(_.ref.id).sorted,
      List(proofRecord.ref.id, duplicateProofOwner.ref.id).sorted
    )
    val duplicateProofFailure = intercept[IllegalStateException] {
      PassedPawnResultProofAssembler.fromAssembly(
        resolved.assembly.copy(evidenceGraph = ambiguousProofGraph),
        JudgmentProvenanceAllocator.forInput(resolved.assembly.input),
        comparisonRecord
      )
    }
    assertEquals(
      duplicateProofFailure.getMessage,
      "one exact passed-pawn-result L2 dependency manifest has multiple graph owners"
    )
    assertEquals(
      PassedPawnResultProofAssembler.exactRootReplyInventoryOwner(
        resolved.evidenceGraph,
        eventRecord,
        proof.event
      ),
      Some(inventoryOwner)
    )

    val missingInventoryOwner = eventRecord.copy(
      parents = eventRecord.parents.filterNot(_.id == inventoryOwner.ref.id)
    )
    assertEquals(
      PassedPawnResultProofAssembler.exactRootReplyInventoryOwner(
        resolved.evidenceGraph,
        missingInventoryOwner,
        proof.event
      ),
      None,
      "a missing lower inventory owner must fail closed"
    )
    val duplicateInventoryOwner = inventoryOwner.copy(
      ref = inventoryOwner.ref.copy(id = s"${inventoryOwner.ref.id}-ambiguous")
    )
    val ambiguousInventoryGraph = resolved.evidenceGraph.add(duplicateInventoryOwner)
    val ambiguousInventoryOwner = eventRecord.copy(
      parents = eventRecord.parents :+ duplicateInventoryOwner.ref
    )
    assertEquals(
      PassedPawnResultProofAssembler.exactRootReplyInventoryOwner(
        ambiguousInventoryGraph,
        ambiguousInventoryOwner,
        proof.event
      ),
      None,
      "multiple exact lower inventory owners must fail closed"
    )

    val branchOwnerBindings = proof.event.branchWitnesses.map { witness =>
      val owners = eventRecord.parents.filter(parent =>
        parent.producer == EvidenceProducer.LegalLineProducer && parent.line.contains(witness.line)
      )
      assertEquals(owners.size, 1)
      witness.line -> owners.head.id
    }
    val eventOccurrenceKey = PassedPawnResultEventAssembler.episodeOccurrenceKey(
      proof.event,
      proof.event.continuationSourceLine,
      branchOwnerBindings
    )
    val firstWitness = proof.event.branchWitnesses.head
    val changedProbeEvent = proof.event.copy(branchWitnesses =
      firstWitness.copy(sourceProbeId = s"${firstWitness.sourceProbeId}-sibling") ::
        proof.event.branchWitnesses.tail
    )
    assertNotEquals(
      eventOccurrenceKey,
      PassedPawnResultEventAssembler.episodeOccurrenceKey(
        changedProbeEvent,
        changedProbeEvent.continuationSourceLine,
        branchOwnerBindings
      )
    )
    val changedHorizonEvent = proof.event.copy(branchWitnesses =
      firstWitness.copy(certifiedHorizonPlyOffset = firstWitness.certifiedHorizonPlyOffset + 1) ::
        proof.event.branchWitnesses.tail
    )
    assertNotEquals(
      eventOccurrenceKey,
      PassedPawnResultEventAssembler.episodeOccurrenceKey(
        changedHorizonEvent,
        changedHorizonEvent.continuationSourceLine,
        branchOwnerBindings
      )
    )
    assertNotEquals(
      eventOccurrenceKey,
      PassedPawnResultEventAssembler.episodeOccurrenceKey(
        proof.event,
        proof.event.continuationSourceLine,
        branchOwnerBindings.updated(0, branchOwnerBindings.head._1 -> "sibling-line-owner")
      )
    )
    val siblingLine = firstWitness.line.copy(
      id = s"${firstWitness.line.id}-sibling",
      rootMove = "h8h7"
    )
    val changedReplyMapEvent = proof.event.copy(branchWitnesses =
      firstWitness.copy(line = siblingLine) :: proof.event.branchWitnesses.tail
    )
    assertNotEquals(
      eventOccurrenceKey,
      PassedPawnResultEventAssembler.episodeOccurrenceKey(
        changedReplyMapEvent,
        changedReplyMapEvent.continuationSourceLine,
        (siblingLine -> branchOwnerBindings.head._2) :: branchOwnerBindings.tail
      )
    )

    val rootEvent = proof.event.causalEpisode.root
    val graphRootStructuralOccurrences = resolved.evidenceGraph.records.collect {
      case EvidenceRecord(_, structural: StructuralDeltaEvidence, _)
          if structural.line.contains(proof.rootLine) &&
            EvidenceRef.sameMove(structural.moveUci, rootEvent.moveUci) =>
        structural.replayStructuralOccurrence.map(_.occurrenceId)
    }.flatten.distinct
    assertEquals(
      graphRootStructuralOccurrences,
      List(rootEvent.structuralOccurrence.occurrenceId),
      "the root graph record and passed-pawn result must project the same replay-owned structural occurrence"
    )
    assert(
      proof.event.directResultProofs.forall(_.sourceOccurrenceId == rootEvent.structuralOccurrence.occurrenceId)
    )

    val resultEvent = proof.assessment.sourceEvent
    val resultOwnerReplay = resolved.evidenceGraph
      .record(resultEvent.lineOccurrenceOwner)
      .collect { case EvidenceRecord(_, line: LineFactEvidence, _) => line }
      .flatMap(_.certifiedReplay)
      .getOrElse(fail("expected the exact result line owner replay"))
    val consumedPawnTopologyOccurrences = resultEvent.structuralOccurrence.resultPremiseOccurrences
      .filter(_.contract == VerticalRelationContractKind.PawnTopologyTransition)
    assert(consumedPawnTopologyOccurrences.nonEmpty, "expected a consumed L1 pawn-topology occurrence")
    val exactPawnTopologyOccurrences = resultOwnerReplay
      .verticalRelationOccurrences(
        resultEvent.step,
        List(VerticalRelationContractKind.PawnTopologyTransition)
      )
      .filter(occurrence =>
        consumedPawnTopologyOccurrences.exists(_.occurrenceId == occurrence.occurrenceId)
      )
    assertEquals(
      exactPawnTopologyOccurrences.map(_.occurrenceId).sorted,
      consumedPawnTopologyOccurrences.map(_.occurrenceId).sorted,
      "the structural result must select exact L1 occurrences rather than every same-contract output"
    )
    val exactPawnTopologyPremises = exactPawnTopologyOccurrences
      .flatMap(_.certifiedSourcePremiseIds)
      .distinct
    assert(exactPawnTopologyPremises.nonEmpty, "expected the exact L1 pawn-topology premises")
    assert(
      exactPawnTopologyPremises.forall(resultEvent.structuralOccurrence.sourcePremiseKeys.contains),
      "the passed-pawn result occurrence must retain the exact lower L1 premise keys"
    )
    val publicExpectedResultPremises = proof.publicProofPaths
      .flatMap(_.premises)
      .filter(_.role == "expected_result")
    assert(publicExpectedResultPremises.nonEmpty, "expected a public typed passed-pawn-result premise")
    assert(
      publicExpectedResultPremises.forall(publicPremise =>
        exactPawnTopologyPremises.forall(publicPremise.sourcePremiseIds.contains)
      ),
      "the public passed-pawn-result premise must retain the exact lower L1 source ids"
    )
    val publicDependencyPremises = proof.publicProofPaths
      .flatMap(_.premises)
      .filter(premise => Set("expected_dependency", "observed_dependency")(premise.role))
    assert(publicDependencyPremises.nonEmpty, "expected exact public dependency witnesses")
    assert(publicDependencyPremises.forall(_.dependencyProof.nonEmpty))
    assert(
      publicDependencyPremises.flatMap(_.dependencyProof).exists(_.proofKind == "object_state"),
      "the exact repeated-pawn route must publish its object-state proof"
    )
    val objectStateProof = publicDependencyPremises
      .flatMap(_.dependencyProof)
      .find(_.proofKind == "object_state")
      .getOrElse(fail("expected the exact repeated-pawn object-state proof"))
    assertEquals(
      objectStateProof.squares.map(witness => witness.role -> witness.square),
      List("root_from" -> "a5", "root_to" -> "a6", "future_from" -> "a6", "future_to" -> "a7")
    )
    assertEquals(
      objectStateProof.pieces.map(witness => (witness.role, witness.side, witness.piece)),
      List(
        ("root_before", "white", "pawn"),
        ("tracked", "white", "pawn"),
        ("future_after", "white", "pawn")
      )
    )
    assertEquals(objectStateProof.relationIssuers, Nil)
    assert(
      proof.publicProofPaths
        .flatMap(_.premises)
        .filterNot(premise => Set("expected_dependency", "observed_dependency")(premise.role))
        .forall(_.dependencyProof.isEmpty),
      "non-dependency premises must not acquire a speculative dependency proof"
    )
    publicDependencyPremises.flatMap(_.dependencyProof).foreach { dependencyProof =>
      assert(dependencyProof.squares.nonEmpty)
      assert(dependencyProof.pieces.nonEmpty)
      (dependencyProof.dependencyKind, dependencyProof.proofKind) match
        case ("object_state_precondition", "object_state") =>
          assertEquals(dependencyProof.relationIssuers, Nil)
        case ("line_access_precondition", "line_access") =>
          assertEquals(
            dependencyProof.relationIssuers.map(issuer => issuer.contract -> issuer.relationKind),
            List("slider_reach_delta" -> "slider_reach_delta")
          )
        case ("response_continuation_precondition", "pawn_break_follow_up") =>
          assertEquals(
            dependencyProof.relationIssuers.map(issuer => issuer.contract -> issuer.relationKind),
            List("pawn_topology_transition" -> "pawn_topology_transition")
          )
        case ("response_continuation_precondition", "capture_follow_up") =>
          assertEquals(
            dependencyProof.relationIssuers.map(issuer => issuer.contract -> issuer.relationKind),
            List("capture_recapture_inventory" -> "capture_recapture_inventory")
          )
        case other => fail(s"unexpected public dependency proof $other")
    }

    val missingDemandParent = proofRecord.copy(parents = List(proof.eventSource))
    assert(
      !proof.exactOccurrenceCertified(missingDemandParent),
      "a typed payload must not self-certify after losing its exact comparison demand"
    )

  test("causal trace ownership is exact, missing manifests fail closed, and duplicate producers fail"):
    val exactFixture = fixture(Standard.initialFen.value, List("g1f3", "g8f6", "b1c3"))
    val owner = EvidenceRef(
      "trace-owner",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      exactFixture.structural.from,
      Some(exactFixture.line),
      exactFixture.line.role.scope,
      EvidenceConfidence.LegalReplayVerified
    )
    val ownerRecord = EvidenceRecord(owner, exactFixture.facts)
    val exactSteps = exactFixture.replay.replaySteps.toSet
    val exactTrace = PassedPawnResultEventProof.causalTrace(
      exactFixture.replay,
      List(ownerRecord -> exactFixture.facts),
      Map(owner.id -> exactSteps)
    )
    val rootStep = exactFixture.replay.replaySteps.head
    assertEquals(exactTrace.observations.keySet, exactSteps)
    assertEquals(
      exactTrace.observation(rootStep).map(_.structuralOccurrence.occurrenceId),
      exactFixture.replay.structuralOccurrence(rootStep).map(_.occurrenceId)
    )
    val rootObservation = exactTrace.observation(rootStep).getOrElse(fail("expected the root observation"))
    val duplicateObservationFailure = intercept[IllegalArgumentException] {
      CausalLineTrace.from(
        exactFixture.replay.replaySteps,
        List(rootObservation, rootObservation),
        Some(exactFixture.replay)
      )
    }
    assertEquals(
      duplicateObservationFailure.getMessage,
      "requirement failed: one causal replay step cannot have multiple observation producers"
    )

    val missingTrace = PassedPawnResultEventProof.causalTrace(
      exactFixture.replay,
      List(ownerRecord -> exactFixture.facts),
      Map(owner.id -> exactFixture.replay.replaySteps.drop(1).toSet)
    )
    assertEquals(missingTrace.observations, Map.empty)

    val duplicateLine = exactFixture.line.copy(id = s"${exactFixture.line.id}-duplicate")
    val duplicateFacts = LineFactEvidence.fromCertifiedReplay(duplicateLine, exactFixture.replay)
    val duplicateOwner = owner.copy(
      id = "trace-owner-duplicate",
      line = Some(duplicateLine)
    )
    val duplicateOwnerFailure = intercept[IllegalArgumentException] {
      PassedPawnResultEventProof.causalTrace(
        exactFixture.replay,
        List(ownerRecord -> exactFixture.facts, EvidenceRecord(duplicateOwner, duplicateFacts) -> duplicateFacts),
        Map(owner.id -> exactSteps, duplicateOwner.id -> exactSteps)
      )
    }
    assertEquals(
      duplicateOwnerFailure.getMessage,
      "requirement failed: one causal replay step cannot have multiple observation producers"
    )

  test("a check response is not promoted beside the exact capture continuation"):
    val exactFixture = fixture(
      "5rk1/5p2/8/8/2B5/1Q6/8/4K3 w - - 0 1",
      List("c4f7", "f8f7", "b3f7")
    )
    val lineOwner = EvidenceRef(
      "parallel-path-line-owner",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      exactFixture.structural.from,
      Some(exactFixture.line),
      exactFixture.line.role.scope,
      EvidenceConfidence.LegalReplayVerified
    )
    val lineRecord = EvidenceRecord(lineOwner, exactFixture.facts)
    val trace = PassedPawnResultEventProof.causalTrace(
      exactFixture.replay,
      List(lineRecord -> exactFixture.facts)
    )
    def event(step: LineReplayStep): PassedPawnResultEventNode =
      val observation = trace.observation(step).getOrElse(fail("expected the exact step observation"))
      val transition = exactFixture.replay.transition(step).getOrElse(fail("expected the exact replay transition"))
      PassedPawnResultEventNode(
        PassedPawnResultEventIdentityBuilder.from(
          step.moveUci,
          transition.relationDelta.rootMove,
          PassedPawnResultKind.Creation
        ),
        step,
        observation.transition.perspective,
        observation.consequences,
        observation.lineOccurrenceOwner,
        observation.structuralOccurrence,
        observation.transition,
        Some(transition.legal),
        Some(transition.relationDelta.rootMove)
      )
    val rootStep :: replyStep :: resultStep :: Nil = exactFixture.replay.replaySteps: @unchecked
    val root = event(rootStep)
    val result = event(resultStep)
    val reply = PassedPawnResultReply
      .certified(
        root,
        replyStep,
        exactFixture.replay.transition(rootStep).get,
        exactFixture.replay.transition(replyStep).get,
        exactFixture.replay
      )
      .getOrElse(fail("expected the exact capture-and-check response"))
    val dependencies = trace
      .relation(rootStep, resultStep)
      .toList
      .flatMap(_.continuationsFor(replyStep))
      .map(trajectory =>
        PassedPawnResultDependency(
          root,
          result,
          PassedPawnResultDependencyKind.ResponseContinuationPrecondition,
          PassedPawnResultDependencyProof.ResponseContinuation(trajectory),
          plyOffset = 2
        )
      )
    assertEquals(dependencies.size, 1)
    assert(dependencies.forall(_.enablesContinuation))
    val paths = PassedPawnResultEpisode(
      root,
      List(result),
      dependencies,
      List(reply)
    ).enablingDependencyPathsTo(result)
    assertEquals(paths.size, 1)
    assertEquals(
      paths.flatMap(_.headOption).map(_.proof).collect {
        case PassedPawnResultDependencyProof.ResponseContinuation(_: CaptureResponseFollowUpTrajectory) => "capture"
      }.toSet,
      Set("capture")
    )
    val captureProof = PassedPawnResultDependencyPremiseWitness.from(dependencies.head).publicProof
    assertEquals(captureProof.dependencyKind, "response_continuation_precondition")
    assertEquals(captureProof.proofKind, "capture_follow_up")
    assertEquals(
      captureProof.squares.map(witness => witness.role -> witness.square),
      List("reply_from" -> "f8", "reply_to" -> "f7", "follow_up_from" -> "b3", "follow_up_to" -> "f7")
    )
    assertEquals(
      captureProof.pieces.map(witness => (witness.role, witness.side, witness.piece)),
      List(
        ("trigger_piece", "white", "bishop"),
        ("responder_piece", "black", "rook"),
        ("follow_up_piece", "white", "queen")
      )
    )
    assertEquals(
      captureProof.relationIssuers.map(issuer => issuer.contract -> issuer.relationKind),
      List("capture_recapture_inventory" -> "capture_recapture_inventory")
    )
    assertEquals(captureProof.relationIssuers.map(_.stepKey), List(BoundedCausalIdentity.stepKey(replyStep)))

  test("a line-access dependency publishes its exact slider-reach issuer without recomputing access"):
    val exactFixture = fixture(
      "7k/8/8/8/8/8/P7/R6K w - - 0 1",
      List("a2a3", "h8g8", "a1a2")
    )
    val lineOwner = EvidenceRef(
      "line-access-witness-owner",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      exactFixture.structural.from,
      Some(exactFixture.line),
      exactFixture.line.role.scope,
      EvidenceConfidence.LegalReplayVerified
    )
    val lineRecord = EvidenceRecord(lineOwner, exactFixture.facts)
    val trace = PassedPawnResultEventProof.causalTrace(
      exactFixture.replay,
      List(lineRecord -> exactFixture.facts)
    )
    def event(step: LineReplayStep): PassedPawnResultEventNode =
      val observation = trace.observation(step).getOrElse(fail("expected the exact step observation"))
      val transition = exactFixture.replay.transition(step).getOrElse(fail("expected the exact replay transition"))
      PassedPawnResultEventNode(
        PassedPawnResultEventIdentityBuilder.from(
          step.moveUci,
          transition.relationDelta.rootMove,
          PassedPawnResultKind.Creation
        ),
        step,
        observation.transition.perspective,
        observation.consequences,
        observation.lineOccurrenceOwner,
        observation.structuralOccurrence,
        observation.transition,
        Some(transition.legal),
        Some(transition.relationDelta.rootMove)
      )
    val enablingStep :: interveningStep :: enabledStep :: Nil = exactFixture.replay.replaySteps: @unchecked
    val trajectory = LineAccessTrajectory
      .findRootClearanceBeforeUse(enablingStep, enabledStep, List(interveningStep), exactFixture.replay)
      .getOrElse(fail("expected the exact line-access trajectory"))
    val dependency = PassedPawnResultDependency(
      event(enablingStep),
      event(enabledStep),
      PassedPawnResultDependencyKind.LineAccessPrecondition,
      PassedPawnResultDependencyProof.LineAccess(trajectory),
      plyOffset = 2
    )
    val publicProof = PassedPawnResultDependencyPremiseWitness.from(dependency).publicProof
    assertEquals(publicProof.dependencyKind, "line_access_precondition")
    assertEquals(publicProof.proofKind, "line_access")
    assertEquals(
      publicProof.squares.map(witness => witness.role -> witness.square),
      List("vacated_gate" -> "a2", "enabled_from" -> "a1", "enabled_to" -> "a2")
    )
    assertEquals(
      publicProof.pieces.map(witness => (witness.role, witness.side, witness.piece)),
      List(("enabled_piece", "white", "rook"))
    )
    val issuer = publicProof.relationIssuers match
      case exact :: Nil => exact
      case other        => fail(s"expected one exact slider-reach issuer, got $other")
    assertEquals(issuer.contract, "slider_reach_delta")
    assertEquals(issuer.relationKind, "slider_reach_delta")
    assertEquals(issuer.resultKey, trajectory.relationOccurrenceBinding.result.stableKey)
    assertEquals(issuer.occurrenceId, trajectory.relationOccurrenceBinding.occurrenceId)
    assertEquals(issuer.stepKey, BoundedCausalIdentity.stepKey(enablingStep))
    assertEquals(issuer.sourcePremiseIds, trajectory.relationOccurrenceBinding.certifiedSourcePremiseIds)

  test("a reply-backed passed-pawn result dependency retains its exact L1 occurrence owners"):
    val fen = "4k3/8/3p4/2P5/4P3/8/8/4K3 w - - 0 1"
    val replay = replayFrom(fen, List("e4e5", "d6e5", "c5c6"))
    val breakStep :: replyStep :: followUpStep :: Nil = replay.replaySteps: @unchecked
    val line = LineNodeRef(
      id = "reply-l1-passed-pawn-result-line",
      rootMove = breakStep.moveUci,
      rank = 1,
      role = LineNodeRole.Played
    )
    val lineOwner = EvidenceRef(
      id = "reply-l1-passed-pawn-result-line-owner",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = PositionNodeRef(fen, 0, Some(White)),
      line = Some(line),
      scope = line.role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
    def eventNode(step: LineReplayStep): PassedPawnResultEventNode =
      val replayTransition = replay.transition(step).getOrElse(fail("expected the exact replay transition"))
      val transition = StructuralTransitionBinding(
        moveUci = step.moveUci,
        role = TransitionEdgeRole.Played,
        from = PositionNodeRef(step.fenBefore, step.ply - 1, Some(replayTransition.legal.before.color)),
        to = PositionNodeRef(step.fenAfter, step.ply, Some(replayTransition.legal.after.color)),
        line = Some(line),
        perspective = replayTransition.legal.move.piece.color
      )
      PassedPawnResultEventNode(
        identity = PassedPawnResultEventIdentityBuilder.from(
          step.moveUci,
          replayTransition.relationDelta.rootMove,
          PassedPawnResultKind.Creation
        ),
        step = step,
        perspective = transition.perspective,
        structuralConsequences = Nil,
        lineOccurrenceOwner = lineOwner,
        structuralOccurrence = replay
          .structuralOccurrence(step)
          .getOrElse(fail("expected the replay-owned structural occurrence")),
        structuralTransition = transition,
        canonicalStep = Some(replayTransition.legal),
        canonicalMovement = Some(replayTransition.relationDelta.rootMove)
      )

    val root = eventNode(breakStep)
    val result = eventNode(followUpStep)
    val trajectory = PawnBreakFollowUpTrajectory
      .find(breakStep, replyStep, followUpStep, List(replyStep), replay)
      .getOrElse(fail("expected the exact reply-backed continuation"))
    val dependency = PassedPawnResultDependency(
      root,
      result,
      PassedPawnResultDependencyKind.ResponseContinuationPrecondition,
      PassedPawnResultDependencyProof.ResponseContinuation(trajectory),
      plyOffset = 2
    )
    assert(dependency.enablesContinuation)
    val reply = PassedPawnResultReply
      .certified(
        root,
        replyStep,
        replay.transition(breakStep).getOrElse(fail("expected the trigger transition")),
        replay.transition(replyStep).getOrElse(fail("expected the reply transition")),
        replay
      )
      .getOrElse(fail("expected the certified capture response"))
    val duplicateResponseFailure = intercept[IllegalArgumentException] {
      PassedPawnResultEpisode(root, List(result), List(dependency), List(reply, reply))
    }
    assertEquals(
      duplicateResponseFailure.getMessage,
      "requirement failed: duplicate exact passed-pawn-result responses"
    )
    val occurrence = trajectory.relationOccurrenceBinding
    val premiseWitness = PassedPawnResultDependencyPremiseWitness.from(dependency)
    assert(premiseWitness.relationOwnerIds.contains(occurrence.occurrenceId))
    assert(
      occurrence.certifiedSourcePremiseIds.forall(premiseWitness.relationOwnerIds.contains),
      clues(occurrence.certifiedSourcePremiseIds, premiseWitness.relationOwnerIds)
    )
    val dependencyProof = premiseWitness.publicProof
    assertEquals(dependencyProof.dependencyKind, "response_continuation_precondition")
    assertEquals(dependencyProof.proofKind, "pawn_break_follow_up")
    assertEquals(
      dependencyProof.squares.map(witness => witness.role -> witness.square),
      List(
        "reply_from" -> "d6",
        "reply_to" -> "e5",
        "follow_up_from" -> "c5",
        "follow_up_to" -> "c6",
        "released_passed_pawn" -> "c5"
      )
    )
    assertEquals(
      dependencyProof.pieces.map(witness => (witness.role, witness.side, witness.piece)),
      List(
        ("trigger_pawn", "white", "pawn"),
        ("responder_pawn", "black", "pawn"),
        ("follow_up_pawn", "white", "pawn")
      )
    )
    val issuer = dependencyProof.relationIssuers match
      case exact :: Nil => exact
      case other        => fail(s"expected one exact pawn-topology issuer, got $other")
    assertEquals(issuer.contract, "pawn_topology_transition")
    assertEquals(issuer.relationKind, "pawn_topology_transition")
    assertEquals(issuer.resultKey, occurrence.result.stableKey)
    assertEquals(issuer.occurrenceId, occurrence.occurrenceId)
    assertEquals(issuer.stepKey, BoundedCausalIdentity.stepKey(replyStep))
    assertEquals(issuer.sourcePremiseIds, occurrence.certifiedSourcePremiseIds)

  test("an inactive passed-pawn-result trace computes no event pair until an exact relation is demanded"):
    val quiet = fixture(Standard.initialFen.value, List("g1f3", "g8f6", "b1c3"))
    val rootStep = quiet.replay.replaySteps.head
    val rootTransition = quiet.replay.transition(rootStep).getOrElse(fail("expected the root transition"))
    val rootLineOccurrenceOwner = EvidenceRef(
      "quiet-root-line-owner",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      quiet.structural.from,
      Some(quiet.line),
      quiet.line.role.scope,
      EvidenceConfidence.LegalReplayVerified
    )
    val rootStructuralOccurrence = quiet.replay
      .structuralOccurrence(rootStep)
      .getOrElse(fail("expected the quiet root structural occurrence"))
    val trace = CausalLineTrace.from(
      quiet.replay.replaySteps,
      List(CausalStepObservation(
        rootStep,
        quiet.structural.transition,
        quiet.structural.consequences,
        rootTransition.relationDelta.rootMove,
        rootLineOccurrenceOwner,
        rootStructuralOccurrence
      )),
      Some(quiet.replay)
    )
    val resultKind = PassedPawnResultKind.AdvanceOrPromote
    val root = PassedPawnResultEventNode(
      identity = PassedPawnResultEventIdentityBuilder.from(
        rootStep.moveUci,
        rootTransition.relationDelta.rootMove,
        resultKind
      ),
      step = rootStep,
      perspective = White,
      structuralConsequences = Nil,
      lineOccurrenceOwner = rootLineOccurrenceOwner,
      structuralOccurrence = rootStructuralOccurrence,
      structuralTransition = quiet.structural.transition,
      canonicalStep = Some(rootTransition.legal),
      canonicalMovement = Some(rootTransition.relationDelta.rootMove)
    )

    assertEquals(trace.computedRelationCount, 0)
    val episode = PassedPawnResultEpisodeBuilder.fromContinuation(
      rootLine = quiet.line,
      role = TransitionEdgeRole.Played,
      root = root,
      continuation = quiet.replay.replaySteps.drop(1),
      trace = Some(trace)
    )
    assertEquals(episode.dependencies, Nil)
    assertEquals(episode.resultRoutes, Nil)
    assertEquals(trace.computedRelationCount, 0)

    val lastStep = quiet.replay.replaySteps.last
    trace.relation(rootStep, lastStep)
    assertEquals(trace.computedRelationCount, 1)
    trace.relation(rootStep, lastStep)
    assertEquals(trace.computedRelationCount, 1)

  final private case class Fixture(
      line: LineNodeRef,
      facts: LineFactEvidence,
      structural: StructuralDeltaEvidence,
      replay: CanonicalLineReplay
  )

  private def replayFrom(fen: String, moves: List[String]): CanonicalLineReplay =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val extended = history.extend(moves).toOption.getOrElse(fail("expected a legal replay"))
    CanonicalLineReplay
      .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail("expected a canonical replay"))

  private def fixture(fen: String, moves: List[String]): Fixture =
    val positionHistory = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val lineHistory = positionHistory
      .extend(moves)
      .toOption
      .getOrElse(fail("expected one admitted fixture line"))
    val replay = CanonicalLineReplay
      .fromHistory(lineHistory.segmentReplaySteps.drop(positionHistory.segmentReplaySteps.size))
      .getOrElse(fail("expected one canonical fixture replay"))
    val line = LineNodeRef(
      id = s"line-${moves.head}",
      rootMove = moves.head,
      rank = 1,
      role = LineNodeRole.Played
    )
    val rootStep = replay.legalSteps.head
    val from = PositionNodeRef(fen, 0, Some(rootStep.move.piece.color))
    val transition = StructuralTransitionBinding(
      moveUci = moves.head,
      role = TransitionEdgeRole.Played,
      from = from,
      to = PositionNodeRef(replay.replaySteps.head.fenAfter, 1, Some(rootStep.after.color)),
      line = Some(line),
      perspective = rootStep.move.piece.color
    )
    val rootReplay = replay
      .subset(List(replay.replaySteps.head))
      .getOrElse(fail("expected the admitted root transition"))
    val transitionProof = CanonicalTransitionProof
      .from(transition, rootReplay)
      .getOrElse(fail("expected a certified fixture transition"))
    Fixture(
      line,
      LineFactEvidence.fromCertifiedReplay(line = line, replay = replay),
      StructuralDeltaEvidence(
        transition,
        consequences = Nil,
        relationChanges = Nil,
        resultPremiseSources = Nil,
        canonicalTransitionProof = Some(transitionProof),
        canonicalDeltaProof = None
      ),
      replay
    )

  private def analyzedFixture(fen: String, moves: List[String]): Fixture =
    val base = fixture(fen, moves)
    val before = Fen.read(Standard, Fen.Full(fen)).getOrElse(fail("expected the initial position"))
    val afterFen = base.facts.lineReplaySteps.head.fenAfter
    val after = Fen.read(Standard, Fen.Full(afterFen)).getOrElse(fail("expected the root result position"))
    val beforeRef = PositionNodeRef(fen, 0, Some(before.color))
    val afterRef = PositionNodeRef(afterFen, 1, Some(after.color))
    val rootReplay = base.replay
      .subset(List(base.replay.replaySteps.head))
      .getOrElse(fail("expected the admitted root transition"))
    val replayTransition = rootReplay.onlyTransition.getOrElse(fail("expected one replay transition"))
    val beforeRelations = relationNodes(replayTransition.beforeAnalysis, beforeRef)
    val afterRelations = relationNodes(replayTransition.afterAnalysis, afterRef)
    val delta = StructuralDeltaAnalyzer.bind(
      transition = replayTransition,
      canonicalRelations = CanonicalRelationDelta.bind(
        replayTransition.declared,
        replayTransition.relationDelta,
        beforeRelations,
        afterRelations
      )
    )
    base.copy(structural = base.structural.copy(
      consequences = StructuralDeltaContracts.consequences(delta.structural),
      relationChanges = delta.canonicalRelations.changes
    ))

  private def relationNodes(
      analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      position: PositionNodeRef
  ): CanonicalPositionRelationSnapshot =
    TypedEvidenceGraph.empty
      .addAll(
        PositionRelationExtractor.records(
          analysis.boardRelations,
          position,
          EvidenceScope.CurrentPosition,
          relation => s"fixture:${position.ply}:${relation.semanticId}"
        )
      )
      .relationGraph
      .closedPositionRelationSnapshot(
        position,
        EvidenceScope.CurrentPosition,
        analysis.relationInventory
      )

  private def assemblyContext(fixture: Fixture): JudgmentAssemblyContext =
    val lineRef = EvidenceRef(
      id = s"assembled-line-${fixture.line.rootMove}",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = fixture.structural.from,
      line = Some(fixture.line),
      scope = EvidenceScope.PlayedLine,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
    val structuralRef = EvidenceRef(
      id = s"assembled-structural-${fixture.line.rootMove}",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = fixture.structural.from,
      line = Some(fixture.line),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
    val transitionRef = EvidenceRef(
      id = s"assembled-transition-${fixture.line.rootMove}",
      producer = EvidenceProducer.MoveTransitionProducer,
      layer = EvidenceLayer.MoveTransition,
      position = fixture.structural.from,
      line = None,
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
    val beforeAnalysis = PositionAnalyzer.analyze(
      fixture.replay.legalSteps.head.before,
      fixture.structural.from.fen,
      fixture.structural.from.ply
    )
    val afterAnalysis = PositionAnalyzer.analyzeAfter(
      beforeAnalysis,
      fixture.replay.legalSteps.head,
      fixture.structural.to.fen
    )
    val rootReplay = fixture.replay
      .subset(List(fixture.replay.replaySteps.head))
      .getOrElse(fail("expected the admitted root transition"))
    val moveTransitionProof = CanonicalTransitionProof
      .from(fixture.structural.transition.copy(line = None), rootReplay)
      .getOrElse(fail("expected an unlined move-transition certificate"))
    val input = normalizedInput(fixture)
    val graph = List(
      EvidenceRecord(lineRef, fixture.facts),
      EvidenceRecord(structuralRef, fixture.structural),
      EvidenceRecord(
        transitionRef,
        MoveTransitionEvidence(
          fixture.line.rootMove,
          fixture.structural.from,
          fixture.structural.to,
          Some(moveTransitionProof)
        )
      )
    ).foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    val context = JudgmentAssemblyContext(
      input = input,
      positions = List(
        PositionNode(PositionNodeRole.Before, fixture.structural.from),
        PositionNode(PositionNodeRole.AfterPlayed, fixture.structural.to)
      ),
      positionAnalyses = Map(
        fixture.structural.from -> beforeAnalysis,
        fixture.structural.to -> afterAnalysis
      ),
      lines = List(
        CandidateLineNode(
          fixture.line,
          CandidateLineEvaluation.EngineSearch(
            EngineLine(fixture.facts.lineReplayMoves, scoreCp = 0, depth = 20)
          ),
          lineRef
        )
      ),
      transitions = List(
        MoveTransitionEdge(
          role = TransitionEdgeRole.Played,
          from = fixture.structural.from,
          moveUci = fixture.line.rootMove,
          to = fixture.structural.to,
          evidence = transitionRef
        )
      ),
      lineReplays = Map(fixture.line -> fixture.replay),
      transitionReplays = Map(
        transitionRef.id -> fixture.replay
          .subset(List(fixture.replay.replaySteps.head))
          .getOrElse(fail("expected the fixture root transition replay"))
      ),
      evidenceGraph = graph,
      claims = Nil
    )
    context

  private def normalizedInput(fixture: Fixture): AdmittedMoveReviewInput =
    val fen = fixture.structural.from.fen
    val positionHistory = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    AdmittedMoveReviewInput(
      beforeFen = fen,
      playedMoveUci = fixture.line.rootMove,
      beforePly = 0,
      sideToMove = Some(White),
      afterPlayedFen = fixture.structural.to.fen,
      afterReferenceFen = None,
      lines = List(
        AdmittedReviewLine(
          role = LineNodeRole.Played,
          rank = 1,
          evaluation = CandidateLineEvaluation.EngineSearch(
            EngineLine(
              fixture.facts.lineReplayMoves,
              scoreCp = 0,
              depth = 20
            )
          ),
          replay = fixture.replay
        )
      ),
      completeCandidateSet = None,
      positionHistory = positionHistory,
    )
