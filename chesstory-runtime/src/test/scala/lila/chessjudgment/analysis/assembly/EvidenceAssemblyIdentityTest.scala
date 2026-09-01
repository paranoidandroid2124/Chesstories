package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.relation.ClosedRelationEvidence
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.line.EngineLine

class EvidenceAssemblyIdentityTest extends munit.FunSuite:

  private val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  private val position = PositionNodeRef(fen, 0, Some(White), Some("identity-root"))
  private val line = LineNodeRef("identity-line", "e2e4", 1, LineNodeRole.Played)

  private def hasCreatedDoubleCheck(relation: RelationFactEvidence): Boolean =
    relation.detail match
      case RelationWitnessDetail.CreatedCheckResponseInventory(_, _, _, checkers, _, _, _, _) =>
        checkers.size >= 2
      case _ => false

  test("line and transition identities include their exact board occurrence"):
    val afterE4 = PrincipalVariationEvidence
      .legalFenAfter(fen, "e2e4")
      .getOrElse(fail("expected e4"))
    val afterD4 = PrincipalVariationEvidence
      .legalFenAfter(fen, "d2d4")
      .getOrElse(fail("expected d4"))
    val rootA = PositionNodeRef(afterE4, 1, Some(Black), Some("occurrence-root-a"))
    val rootB = PositionNodeRef(afterD4, 1, Some(Black), Some("occurrence-root-b"))
    val move = "g8f6"
    def normalized(root: PositionNodeRef): AdmittedReviewLine =
      val history = CanonicalPositionHistory
        .from(root.fen, Nil, root.fen)
        .getOrElse(fail("expected an occurrence history"))
      val extended = history.extend(List(move)).getOrElse(fail("expected a legal occurrence line"))
      val replay = CanonicalLineReplay
        .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
        .getOrElse(fail("expected a canonical occurrence replay"))
      AdmittedReviewLine(
        LineNodeRole.Alternative,
        rank = 1,
        CandidateLineEvaluation.EngineSearch(EngineLine(List(move), scoreCp = 0, depth = 18)),
        replay
      )
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    val lineA = normalized(rootA)
    val lineB = normalized(rootB)
    val lineKeyA = allocator.lineOccurrenceKey(lineA, rootA)
    val lineKeyB = allocator.lineOccurrenceKey(lineB, rootB)
    val refA = allocator.lineRef(lineA, lineKeyA)
    val refB = allocator.lineRef(lineB, lineKeyB)

    assertNotEquals(lineKeyA, lineKeyB)
    assertNotEquals(refA, refB)
    assertEquals(
      allocator.lineRef(lineA, allocator.lineOccurrenceKey(lineA, rootA)),
      refA
    )

    val toA = PositionNodeRef(
      PrincipalVariationEvidence.legalFenAfter(rootA.fen, move).getOrElse(fail("expected Nf6 after e4")),
      2,
      Some(White),
      Some("occurrence-to-a")
    )
    val toB = PositionNodeRef(
      PrincipalVariationEvidence.legalFenAfter(rootB.fen, move).getOrElse(fail("expected Nf6 after d4")),
      2,
      Some(White),
      Some("occurrence-to-b")
    )
    assertNotEquals(
      allocator.transitionOccurrenceKey(TransitionEdgeRole.Alternative, rootA, move, toA),
      allocator.transitionOccurrenceKey(TransitionEdgeRole.Alternative, rootB, move, toB)
    )

  test("fresh double-check control stays on one transition occurrence"):
    val assembled = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "4k3/8/r7/8/8/8/4B3/4R1K1 w - - 0 1",
          playedMoveUci = "e2d3",
          variations = List(
            EngineLine(List("e2b5"), scoreCp = 100, depth = 20),
            EngineLine(List("e2d3", "e8f7"), scoreCp = 0, depth = 20)
          )
        )
      )
      .getOrElse(fail("expected the double-check review to assemble"))
    val freshChecks = assembled.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if hasCreatedDoubleCheck(relation) => record
    }

    assert(freshChecks.nonEmpty)
    freshChecks.foreach { record =>
      val carriers = record.parents.flatMap(assembled.evidenceGraph.record).collect {
        case EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) => occurrence
      }
      assertEquals(carriers.size, 1)
      assert(assembled.evidenceGraph.proofEligible(record))
    }
    val responseInventories = assembled.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.CreatedCheckResponseInventory => record
    }
    assert(responseInventories.nonEmpty)
    assert(responseInventories.forall(assembled.evidenceGraph.proofEligible))

  test("a root check response binds the transition carrier and chosen legal response once"):
    val assembled = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "k3r3/8/8/8/8/3B4/8/4K3 w - - 0 1",
          playedMoveUci = "d3e2",
          variations = List(
            EngineLine(List("e1d2"), scoreCp = 20, depth = 20),
            EngineLine(List("d3e2", "e8e2"), scoreCp = 0, depth = 20)
          )
        )
      )
      .getOrElse(fail("expected the checked root response to assemble"))
    val response = assembled.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.RootCheckResponse => record
    }.getOrElse(fail("expected the root response relation"))
    val carriers = response.parents.flatMap(assembled.evidenceGraph.record).collect {
      case EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) => occurrence
    }

    assertEquals(carriers.size, 1)
    assertEquals(response.parents.distinct.size, response.parents.size)
    assert(assembled.evidenceGraph.proofEligible(response))

  test("a root-and-absence stalemate proof materializes without a fabricated positive source"):
    val assembled = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "k7/2Q5/2K5/8/8/8/8/8 w - - 0 1",
          playedMoveUci = "c7b6",
          variations = List(
            EngineLine(List("c7b6"), scoreCp = 0, depth = 20)
          )
        )
      )
      .getOrElse(fail("expected the stalemate transition to assemble"))
    val stalemate = assembled.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.StalemateTransition => record
    }.getOrElse(fail("expected the closed stalemate relation"))
    val parents = stalemate.parents.flatMap(assembled.evidenceGraph.record)

    assertEquals(parents.count(_.payload.isInstanceOf[ClosedRelationOccurrenceEvidence]), 1)
    assertEquals(parents.count(_.payload.isInstanceOf[RelationFactEvidence]), 0)
    assert(assembled.evidenceGraph.proofEligible(stalemate))

  test("transposed boards share double-check control semantics but retain both exact occurrences"):
    val rootFen = "4k3/6pp/8/8/8/8/PP2B3/4R1K1 w - - 0 1"
    def endFen(moves: List[String]): String =
      PrincipalVariationEvidence
        .legalMoveReplay(rootFen, moves, startPly = 0)
        .flatMap(CanonicalLineReplay.fromLegalReplay)
        .flatMap(_.replaySteps.lastOption.map(_.fenAfter))
        .getOrElse(fail(s"expected legal transposition path: ${moves.mkString(" ")}"))
    val transposedA = endFen(List("a2a3", "h7h6", "b2b3", "g7g6"))
    val transposedB = endFen(List("b2b3", "g7g6", "a2a3", "h7h6"))
    assert(PrincipalVariationEvidence.sameBoardState(transposedA, transposedB))

    val replay = PrincipalVariationEvidence
      .legalMoveReplay(transposedA, List("e2b5"), startPly = 4)
      .flatMap(CanonicalLineReplay.fromLegalReplay)
      .getOrElse(fail("expected the transposed double-check transition"))
    val transition = replay.onlyTransition.getOrElse(fail("expected one transition"))
    val production = ClosedRelationEvidence.relationProduction(replay, "e2b5")
    def bind(path: String): CanonicalRelationTransitionInventory =
      val beforeRef = PositionNodeRef(transposedA, 4, Some(White), Some(s"$path-before"))
      val afterFen = chess.format.Fen.write(transition.legal.after).value
      val afterRef = PositionNodeRef(afterFen, 5, Some(Black), Some(s"$path-after"))
      def snapshot(
          analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
          position: PositionNodeRef
      ): CanonicalPositionRelationSnapshot =
        TypedEvidenceGraph.empty
          .addAll(PositionRelationExtractor.records(
            analysis.boardRelations,
            position,
            EvidenceScope.CurrentPosition,
            relation => s"$path:${position.ply}:${relation.semanticId}"
          ))
          .relationGraph
          .closedPositionRelationSnapshot(position, EvidenceScope.CurrentPosition, analysis.relationInventory)
      val before = snapshot(transition.beforeAnalysis, beforeRef)
      val after = snapshot(transition.afterAnalysis, afterRef)
      val delta = CanonicalRelationDelta.bind(transition.declared, transition.relationDelta, before, after)
      production.bindClosedOutput(before, after, delta)
    val boundA = bind("transposition-a")
    val boundB = bind("transposition-b")
    val doubleA = boundA.relations.filter(hasCreatedDoubleCheck)
    val doubleB = boundB.relations.filter(hasCreatedDoubleCheck)

    assertEquals(doubleA.size, 1)
    assertEquals(doubleB.size, 1)
    assertEquals(doubleA.head.assertionId, doubleB.head.assertionId)
    assertEquals(doubleA.head.semanticId, doubleB.head.semanticId)
    assertNotEquals(boundA.before.position, boundB.before.position)
    assert(PrincipalVariationEvidence.sameBoardState(boundA.before.position.fen, boundB.before.position.fen))
    assertNotEquals(boundA.after.position, boundB.after.position)

  private def input: AdmittedMoveReviewInput =
    val after = PrincipalVariationEvidence
      .legalFenAfter(fen, line.rootMove)
      .getOrElse(fail("expected legal root move"))
    val history = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .fold(error => fail(error.toString), identity)
    AdmittedMoveReviewInput(
      beforeFen = fen,
      playedMoveUci = line.rootMove,
      beforePly = 0,
      sideToMove = Some(White),
      afterPlayedFen = after,
      afterReferenceFen = None,
      lines = Nil,
      admittedRootRankingPair = None,
      positionHistory = history,
    )
