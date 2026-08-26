package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.position.{ PositionAnalyzer, PositionRelationExtractor }
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, StructuralDeltaContracts }
import lila.chessjudgment.model.ProbeObjective
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  PrincipalVariationEvidence
}
import lila.chessjudgment.model.strategic.EngineLine
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind

class PlanCausalHypothesisTest extends munit.FunSuite:

  test("a raw development delta cannot own a plan event"):
    val opening = analyzedFixture(Standard.initialFen.value, List("g1f3", "g8f6"))
    val kinds = assembledEvents(opening).collect {
      case EvidenceRecord(_, event: PlanCausalEventEvidence, _) => event.planId
    }

    assert(!kinds.contains(PlanKind.OpeningDevelopment))


  test("a captured restriction mechanism cannot own prophylaxis"):
    val capturedRestrictor = analyzedFixture(
      "4k3/8/3p4/4p3/2Q5/8/4N3/4K3 w - - 0 1",
      List("e2f4", "e5f4")
    )
    val kinds = assembledEvents(capturedRestrictor).collect {
      case EvidenceRecord(_, event: PlanCausalEventEvidence, _) => event.planId
    }

    assert(!kinds.contains(PlanKind.ProphylaxisRestraint))

  test("branch witnesses use the exact branch position move and rank"):
    val fen = "4k3/p7/3p4/8/4P3/8/8/4K1N1 w - - 0 1"
    val root = fixture(fen, List("g1f3"))
    val branchFen = root.structural.to.fen
    val wrongBranchFen = fixture(fen, List("g1h3")).structural.to.fen
    val replay = replayFrom(branchFen, List("a7a6"))
    val evaluation = CandidateLineEvaluation.EngineSearch(
      EngineLine(List("a7a6"), scoreCp = 0, depth = 20)
    )
    val normalized = NormalizedCandidateLine(LineNodeRole.Threat, 1, evaluation, replay)
    val branch = NormalizedThreatBranch(
      sourceProbeId = "exact-branch",
      objective = ProbeObjective.BranchReplyMultiPv,
      probedMoveUci = "g1f3",
      branchFen = branchFen,
      branchPly = 1,
      opponentResourceMove = None,
      certifiedHorizonPlyOffset = Some(1),
      lines = List(normalized)
    )
    def candidate(id: String, move: String, positionFen: String): CandidateLineNode =
      val line = LineNodeRef(id, move, 1, LineNodeRole.Threat)
      CandidateLineNode(
        line,
        CandidateLineEvaluation.EngineSearch(EngineLine(List(move), scoreCp = 0, depth = 20)),
        EvidenceRef(
          id = s"$id-evidence",
          producer = EvidenceProducer.LegalLineProducer,
          layer = EvidenceLayer.Line,
          position = PositionNodeRef(positionFen, 1, Some(Black)),
          line = Some(line),
          scope = EvidenceScope.ThreatLine,
          confidence = EvidenceConfidence.LegalReplayVerified
        )
      )
    val wrongPosition = candidate("wrong-position", "a7a6", wrongBranchFen)
    val wrongMove = candidate("wrong-move", "a7a5", branchFen)
    val exact = candidate("exact", "a7a6", branchFen)
    val context = assemblyContext(root).copy(lines = List(wrongPosition, wrongMove, exact))

    assertEquals(
      PlanCausalEventAssembler.exactThreatLineFor(context, branch, normalized).map(_.ref),
      Some(exact.ref)
    )
    assertEquals(
      PlanCausalEventAssembler
        .exactThreatLineFor(context.copy(lines = context.lines :+ candidate("duplicate", "a7a6", branchFen)), branch, normalized),
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
      NormalizedCandidateLine(
        LineNodeRole.Threat,
        rank,
        evaluation,
        continuation,
        predecessor
      )
    def assembleBranch(
        branchPly: Int,
        exactBranchFen: String,
        lines: List[NormalizedCandidateLine]
    ) =
      NodeLineTransitionAssembler
        .assemble(
          normalizedInput(root).copy(
            threatBranches = List(
              NormalizedThreatBranch(
                sourceProbeId = "predecessor-selection",
                objective = ProbeObjective.BranchReplyMultiPv,
                probedMoveUci = "g1f3",
                branchFen = exactBranchFen,
                branchPly = branchPly,
                opponentResourceMove = None,
                certifiedHorizonPlyOffset = Some(1),
                lines = lines
              )
            )
          )
        )
        .getOrElse(fail("expected the root input to assemble"))
    def admittedSummary(context: JudgmentAssemblyContext) =
      val transition = context.transition(TransitionEdgeRole.Threat)
      (
        context.position(PositionNodeRole.AfterThreat).map(_.ref.fen),
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
    assert(forwardSummary._2.nonEmpty, "semantic duplicates must still produce one threat transition")

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

  test("root clearance discovers a PV-absent rook transfer through the causal trace"):
    val fen = "4k3/7p/8/8/8/1n6/P7/R3K3 w - - 0 1"
    val latent = latentCandidateMoves(fen, "a2b3")

    assert(
      latent.contains("a1a2"),
      "a2b3 must own the rook's newly opened a-file route, not only pawn-support continuations"
    )
    val context = assemblyContext(fixture(fen, List("a2b3", "h7h6")))
    assert(
      ExactCausalContinuationProbePlanner
        .fromAssembly(context)
        .exists(_.continuationMoves == List("h7h6", "a1a2"))
    )

  test("proactive discovery rejects hanging line-clearance fantasies"):
    assert(
      !latentCandidateMoves(Standard.initialFen.value, "d2d4")
        .contains("c1h6")
    )
    assert(
      !latentCandidateMoves(Standard.initialFen.value, "e2e4")
        .contains("f1a6")
    )

  test("counter-resource discovery keeps exact captures and rejects pre-existing contact"):
    val captureFen = "2B1k3/n7/8/8/8/2N5/8/4K3 w - - 0 1"
    val exact = exactResourceMovesAfter(captureFen, "c3b5")
    assert(exact.contains("a7c8"), "the newly attacked knight's capture must remain a resource candidate")
    val consequence = OpponentResourceDeterrenceProof.consequence("a7c8", "knight")
    assertEquals(consequence.subjects, List("knight:a7-c8:resource-deterred"))
    assert(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject(consequence.subjectFacts.head))

    val preExistingContactFen = "2B1k3/n7/8/8/8/8/8/R3K3 w - - 0 1"
    val preExisting = exactResourceMovesAfter(preExistingContactFen, "a1a6")
    assert(
      !preExisting.contains("a7c8"),
      "moving along an already open rook ray must not manufacture a newly contested resource"
    )

    def resourceLine(id: String, move: String, role: LineNodeRole, rank: Int): CandidateLineNode =
      val ref = LineNodeRef(id, move, rank, role)
      val evidence = EvidenceRef(
        id = s"$id-evidence",
        producer = EvidenceProducer.LegalLineProducer,
        layer = EvidenceLayer.Line,
        position = PositionNodeRef(captureFen, 0, Some(White)),
        line = Some(ref),
        scope = role.scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      )
      CandidateLineNode(
        ref,
        CandidateLineEvaluation.EngineSearch(EngineLine(List(move), scoreCp = 0, depth = 20)),
        evidence
      )
    val comparisonFixture = fixture(captureFen, List("c3b5", "a7c8"))
    val comparisonContext = JudgmentAssemblyContext(
      input = normalizedInput(comparisonFixture),
      positions = List(PositionNode(PositionNodeRole.Before, PositionNodeRef(captureFen, 0, Some(White)))),
      lines = List(
        resourceLine("resource-played", "c3b5", LineNodeRole.Played, 1),
        resourceLine("resource-reference", "e1d1", LineNodeRole.BestReference, 2)
      ),
      transitions = Nil,
      evidenceGraph = TypedEvidenceGraph.empty,
      claims = Nil
    )
    assertEquals(
      CounterResourceProbePlanner.fromAssembly(comparisonContext),
      Nil,
      "an exact resource candidate must not schedule work before an exact causal episode demands it"
    )

  test("each newly created exact structural fact contributes exactly one result"):
    val pawnTension = analyzedFixture(
      "4k3/8/8/6p1/8/8/7P/4K3 w - - 0 1",
      List("h2h4")
    ).structural.consequences.filter(_.kind == TransitionConsequenceKind.PawnTensionCreated)
    assertEquals(pawnTension.map(_.strength), List(1))
    assertEquals(
      pawnTension.flatMap(_.subjects),
      List("break-file:h", "created-tension:h4-g5")
    )

    val geometricControl = analyzedFixture(
      "4b1k1/8/8/8/8/8/8/3QK3 w - - 0 1",
      List("d1h5")
    ).structural.relationChanges.filter(change =>
      change.direction == RelationChangeDirection.Established &&
        (change.detail match
          case RelationWitnessDetail.GeometricControl(White, attacker, _, target, _) =>
            attacker.key == "h5" && target.key == "e8"
          case _ => false)
    )
    assertEquals(geometricControl.size, 1)

  test("a pinned geometric attack remains only a canonical relation"):
    val structural = analyzedFixture(
      "k2r4/8/1r3q2/8/5N2/8/8/3K4 w - - 0 1",
      List("f4d5")
    ).structural
    assert(structural.consequences.isEmpty)
    assert(structural.relationChanges.exists(_.kind == RelationFactKind.GeometricControl))

  test("an opened pawn path is not promoted when the pawn advances remain illegal"):
    val structural = analyzedFixture(
      "7k/8/8/b7/8/3N4/3P4/4K3 w - - 0 1",
      List("d3f4")
    ).structural
    assert(!structural.signals.flatMap(_.subjects).exists(_.contains("advance-path-cleared")))
    assert(structural.consequences.isEmpty)

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
        signals = Nil,
        consequences = Nil,
        relationChanges = Nil,
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
      signals = StructuralDeltaContracts.signals(delta.structural),
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
      line = Some(fixture.line),
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
    val input = normalizedInput(fixture)
    val graph = List(
      EvidenceRecord(lineRef, fixture.facts),
      EvidenceRecord(structuralRef, fixture.structural),
      EvidenceRecord(
        transitionRef,
        MoveTransitionEvidence(
          fixture.line.rootMove,
          fixture.structural.from,
          fixture.structural.to
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

  private def assembledEvents(fixture: Fixture): List[EvidenceRecord] =
    val context = assemblyContext(fixture)
    PlanCausalEventAssembler.fromAssembly(
      context.input,
      context,
      JudgmentProvenanceAllocator.forInput(context.input)
    )

  private def candidateKinds(fixture: Fixture): List[PlanKind] =
    PlanCausalEventProof
      .eventCandidatePlans(
        PlanCausalEventProof.causalTrace(
          fixture.line,
          fixture.structural,
          fixture.facts,
          Some(fixture.replay)
        ),
        fixture.structural.perspective
      )
      .map(_.kind)

  private def latentCandidateMoves(fen: String, move: String): List[String] =
    val root = fixture(fen, List(move))
    ExactCausalContinuationProbePlanner.latentCandidateMoves(root.line, root.replay)

  private def exactResourceMovesAfter(fen: String, move: String): List[String] =
    CounterResourceProbePlanner.exactResourceMovesAfter(fixture(fen, List(move)).replay)

  private def normalizedInput(fixture: Fixture): NormalizedMoveReviewInput =
    val fen = fixture.structural.from.fen
    val positionHistory = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    NormalizedMoveReviewInput(
      beforeFen = fen,
      playedMoveUci = fixture.line.rootMove,
      beforePly = 0,
      sideToMove = Some(White),
      afterPlayedFen = fixture.structural.to.fen,
      afterReferenceFen = None,
      lines = List(
        NormalizedCandidateLine(
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
      openingContext = OpeningContextEvidence(None, Nil)
    )
