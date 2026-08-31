package lila.chessjudgment.analysis.position

import chess.Square
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.model.judgment.{
  CanonicalLineReplay,
  LineReplayStep,
  RelationWitnessDetail
}
import lila.chessjudgment.model.line.{ LegalReplayStep, PrincipalVariationEvidence }
import lila.chessjudgment.model.position.BoardGeometry

class PositionCalculationOwnershipTest extends munit.FunSuite:

  test("an analyzed replay accepts only the exact legal-move occurrence it owns"):
    val ownedFen = "4k3/8/8/8/8/8/8/4K2R w - - 0 1"
    val foreignFen = "4k3/8/8/8/8/8/8/4K2R w - - 37 19"
    val owned = PositionAnalyzer.analyze(PrincipalVariationEvidence.readPosition(ownedFen).get, ownedFen, 0)
    val foreign = PositionAnalyzer.analyze(PrincipalVariationEvidence.readPosition(foreignFen).get, foreignFen, 0)
    val ownedMove = owned.actualLegalMoves.find(_.toUci.uci == "h1h2").getOrElse(fail("expected Rh2"))
    val foreignMove = foreign.actualLegalMoves.find(_.toUci.uci == "h1h2").getOrElse(fail("expected foreign Rh2"))
    val step = LineReplayStep(1, "h1h2", ownedFen, Fen.write(ownedMove.after).value)

    assert(CanonicalLineReplay.fromAnalyzedLegalMove(step, owned, foreignMove).isEmpty)
    intercept[IllegalArgumentException](LegalReplayStep.fromMove(1, owned.position, foreignMove))

  test("one targeted legal projection is reused when check responses close the full inventory"):
    val fen = "4r2k/8/8/8/8/8/3B4/4K3 w - - 0 1"
    val position = PrincipalVariationEvidence.readPosition(fen).get
    val analysis = PositionAnalyzer.analyze(position, fen, 0)
    val inventory = analysis.relationInventory
    val targeted = inventory.legalMove("d2e3").getOrElse(fail("expected the interposition"))
    val responses = inventory.kingResponses

    assertEquals(
      responses.legalMoves.map(_.moveUci),
      analysis.actualLegalMoves.map(_.toUci.uci).sorted
    )
    assert(
      targeted.asInstanceOf[AnyRef].eq(
        responses.legalMoves.find(_.moveUci == "d2e3").get.asInstanceOf[AnyRef]
      )
    )
    assertEquals(responses.responses.map(_.move), responses.legalMoves)

  test("one actual legal move effect is reused by its closed resource and transition"):
    val fen = "4k3/8/8/8/8/8/8/4K2R w K - 0 1"
    val position = PrincipalVariationEvidence.readPosition(fen).getOrElse(fail("expected legal FEN"))
    val analysis = PositionAnalyzer.analyze(position, fen, 0)
    val boardEffect = analysis.computation.actualLegalMoveEffects
      .find(_._1.toUci.uci == "e1g1")
      .map(_._2)
      .getOrElse(fail("expected castling effect"))
    val closed = analysis.relationInventory.legalMove("e1g1").getOrElse(fail("expected castling resource"))
    val step = PrincipalVariationEvidence
      .legalMoveReplay(fen, List("e1g1"), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail("expected legal castling replay"))
    val after = PositionAnalyzer.analyzeAfter(analysis, step, Fen.write(step.after).value)

    assert(boardEffect.asInstanceOf[AnyRef].eq(closed.boardEffect.asInstanceOf[AnyRef]))
    assert(after.transitionFootprint.exists(footprint =>
      boardEffect.asInstanceOf[AnyRef].eq(footprint.moveEffect.asInstanceOf[AnyRef])
    ))

  test("incremental geometry matches cold geometry for ordinary and special move footprints"):
    val cases = List(
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" -> "e2e4",
      "4k3/8/8/8/8/8/8/4K2R w K - 0 1" -> "e1g1",
      "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1" -> "e5d6",
      "4k3/P7/8/8/8/8/8/4K3 w - - 0 1" -> "a7a8q"
    )

    cases.foreach { case (fen, moveUci) =>
      val before = PrincipalVariationEvidence.readPosition(fen).getOrElse(fail(s"expected legal FEN: $fen"))
      val beforeAnalysis = PositionAnalyzer.analyze(before, fen, 0)
      val legalStep = PrincipalVariationEvidence
        .legalMoveReplay(fen, List(moveUci), startPly = 0)
        .flatMap(_.headOption)
        .getOrElse(fail(s"expected legal replay for $moveUci"))
      val afterFen = Fen.write(legalStep.after).value
      val footprint = BoardGeometry.transitionFootprint(
        legalStep.move,
        BoardGeometry.moveEffect(legalStep.move)
      )
      val incrementalStatic = StaticBoardGeometryComputation.buildAfter(
        beforeAnalysis.computation.staticBoard,
        legalStep.after.board,
        footprint
      ).geometry
      val coldStatic = StaticBoardGeometryComputation.cold(legalStep.after.board)
      val incrementalGeometry = PositionComputation.from(incrementalStatic, legalStep.after)
      val coldGeometry = PositionComputation.from(coldStatic, legalStep.after)
      val incremental = PositionAnalysis.fresh(
        PositionAnalyzer.computeOccurrence(legalStep.after, afterFen).copy(plyCount = 1),
        legalStep.after,
        incrementalGeometry,
        Some(footprint),
        None
      )
      val cold = PositionAnalysis.fresh(
        PositionAnalyzer.computeOccurrence(legalStep.after, afterFen).copy(plyCount = 1),
        legalStep.after,
        coldGeometry,
        None,
        None
      )

      assertEquals(incremental.occurrence, cold.occurrence, moveUci)
      assertEquals(incremental.pawnTopology, cold.pawnTopology, moveUci)
      val incrementalDetails = incremental.boardRelations.map(_.detail)
      val coldDetails = cold.boardRelations.map(_.detail)
      assert(
        incrementalDetails == coldDetails,
        s"$moveUci: extra=${incrementalDetails.diff(coldDetails)} missing=${coldDetails.diff(incrementalDetails)}"
      )
      assertEquals(incremental.boardRelations, cold.boardRelations, moveUci)
      assertEquals(
        PositionAnalyzer.analyzeAfter(beforeAnalysis, legalStep, afterFen).transitionFootprint,
        Some(footprint)
      )
    }

  test("bishop rook and queen controls are exact first-barrier ray projections"):
    val fen = "4k3/8/2b5/8/3qP3/8/2R5/4K3 w - - 0 1"
    val current = PrincipalVariationEvidence.readPosition(fen).getOrElse(fail("expected legal FEN"))
    val inventory = PositionAnalyzer
      .analyze(current, fen, 0)
      .computation
      .staticBoard
      .geometricControlInventory

    (current.board.bishops | current.board.rooks | current.board.queens).squares.foreach { origin =>
      val piece = current.board.pieceAt(origin).getOrElse(fail(s"expected slider on ${origin.key}"))
      val expected = piece.role match
        case chess.Bishop => origin.bishopAttacks(current.board.occupied)
        case chess.Rook   => origin.rookAttacks(current.board.occupied)
        case chess.Queen  => origin.queenAttacks(current.board.occupied)
        case role         => fail(s"unexpected slider role: $role")
      assertEquals(inventory.byOrigin(origin), expected, origin.key)
    }

  test("an unrelated non-pawn move reuses the immutable pawn topology"):
    val fen = "4k3/8/8/8/8/8/P7/4K2R w - - 0 1"
    val before = PositionAnalyzer.analyze(PrincipalVariationEvidence.readPosition(fen).get, fen, 0)
    val legalStep = PrincipalVariationEvidence
      .legalMoveReplay(fen, List("h1h2"), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail("expected the rook move"))
    val afterFen = Fen.write(legalStep.after).value
    val footprint = BoardGeometry.transitionFootprint(
      legalStep.move,
      BoardGeometry.moveEffect(legalStep.move)
    )
    val build = StaticBoardGeometryComputation.buildAfter(
      before.computation.staticBoard,
      legalStep.after.board,
      footprint
    )
    val updated = build.geometry

    assert(before.pawnTopology.asInstanceOf[AnyRef] eq updated.pawnTopology.asInstanceOf[AnyRef])
    val beforeFacts = before.computation.staticBoard.relationSnapshot.factsByDetail
    val afterFacts = updated.relationSnapshot.factsByDetail
    val unchangedDetails = beforeFacts.keySet.intersect(afterFacts.keySet)
    assert(unchangedDetails.nonEmpty)
    unchangedDetails.foreach(detail =>
      assert(
        beforeFacts(detail).asInstanceOf[AnyRef] eq afterFacts(detail).asInstanceOf[AnyRef],
        RelationWitnessDetail.stableKey(detail)
      )
    )
  test("a pawn move refreshes only its exact topology dependency closure"):
    val fen = Standard.initialFen.value
    val before = PositionAnalyzer.analyze(PrincipalVariationEvidence.readPosition(fen).get, fen, 0)
    val legalStep = replay(fen, "a2a3")
    val afterFen = Fen.write(legalStep.after).value
    val footprint = BoardGeometry.transitionFootprint(
      legalStep.move,
      BoardGeometry.moveEffect(legalStep.move)
    )
    val build = StaticBoardGeometryComputation.buildAfter(
      before.computation.staticBoard,
      legalStep.after.board,
      footprint
    )

    val pawnProduction = before.pawnTopology.afterTransition(
      legalStep.after.board,
      footprint
    )
    assertEquals(pawnProduction.affectedPawnFileKeys, Set(chess.White -> chess.File.A))
    assertEquals(
      pawnProduction.frontStatesProduced.keySet,
      Set(chess.White -> Square.A3)
    )
    assertEquals(
      pawnProduction.passageStatesProduced.keySet,
      Set(chess.White -> Square.A3, chess.Black -> Square.A7, chess.Black -> Square.B7)
    )
    assertEquals(
      build.geometry.pawnTopology,
      StaticBoardGeometryComputation
        .cold(legalStep.after.board)
        .pawnTopology
    )
    assertEquals(
      build.relationTransition,
      before.computation.staticBoard.relationSnapshot.transitionTo(
        build.geometry.relationSnapshot,
        PositionRelationExtractor.staticRelationRefreshFootprint(footprint)
      )
    )

  test("rebasing an occurrence ply shares its legal moves, relations, geometry, and transition calculation"):
    val fen = "4k3/8/8/8/8/8/P7/4K2R w - - 0 1"
    val before = PositionAnalyzer.analyze(PrincipalVariationEvidence.readPosition(fen).get, fen, 0)
    val step = replay(fen, "h1h2")
    val afterFen = Fen.write(step.after).value
    val after = PositionAnalyzer.analyzeAfter(before, step, afterFen)
    val legalMoves = after.actualLegalMoves
    val relations = after.boardRelations
    val inventory = after.relationInventory
    val transition = after.relationTransition.getOrElse(fail("expected the admitted transition"))
    val rebased = after.atPly(23)

    assert(after.atPly(1).asInstanceOf[AnyRef] eq after.asInstanceOf[AnyRef])
    assert(rebased.computation.asInstanceOf[AnyRef] eq after.computation.asInstanceOf[AnyRef])
    assert(rebased.position.asInstanceOf[AnyRef] eq after.position.asInstanceOf[AnyRef])
    assert(rebased.actualLegalMoves.asInstanceOf[AnyRef] eq legalMoves.asInstanceOf[AnyRef])
    assert(rebased.boardRelations.asInstanceOf[AnyRef] eq relations.asInstanceOf[AnyRef])
    assert(rebased.relationInventory.asInstanceOf[AnyRef] eq inventory.asInstanceOf[AnyRef])
    assert(rebased.pawnTopology.asInstanceOf[AnyRef] eq after.pawnTopology.asInstanceOf[AnyRef])
    assert(rebased.relationTransition.get.asInstanceOf[AnyRef] eq transition.asInstanceOf[AnyRef])
    assertEquals(rebased.occurrence.plyCount, 23)
    assertEquals(rebased.occurrence.fen, after.occurrence.fen)
    intercept[IllegalArgumentException](after.atPly(0))
    intercept[IllegalArgumentException](before.atPly(-1))

  test("a cold destination cache hit performs no transition geometry production"):
    val fen = "4k3/8/8/8/8/8/P7/R3K3 w - - 41 77"
    val step = replay(fen, "a1b1")
    val afterFen = Fen.write(step.after).value
    val before = PositionAnalyzer.analyze(step.before, fen, 0)
    val coldDestination = PositionAnalyzer.analyze(step.after, afterFen, 1)
    val after = PositionAnalyzer.analyzeAfter(before, step, afterFen)

    assert(after.computation.asInstanceOf[AnyRef] eq coldDestination.computation.asInstanceOf[AnyRef])
    assertTransitionMatchesBoardDifference(before, after)

  test("one cached destination keeps distinct sparse transitions for different predecessors"):
    val leftFen = "4k3/8/8/8/8/8/8/1N2K3 w - - 0 1"
    val rightFen = "4k3/8/8/8/8/8/8/4KN2 w - - 0 1"
    val leftStep = replay(leftFen, "b1d2")
    val rightStep = replay(rightFen, "f1d2")
    val leftAfterFen = Fen.write(leftStep.after).value
    val rightAfterFen = Fen.write(rightStep.after).value
    assertEquals(
      PrincipalVariationEvidence.semanticBoardStateFen(leftAfterFen),
      PrincipalVariationEvidence.semanticBoardStateFen(rightAfterFen)
    )

    val leftBefore = PositionAnalyzer.analyze(leftStep.before, leftFen, 0)
    val leftAfter = PositionAnalyzer.analyzeAfter(leftBefore, leftStep, leftAfterFen)
    val rightBefore = PositionAnalyzer.analyze(rightStep.before, rightFen, 0)
    val rightAfter = PositionAnalyzer.analyzeAfter(rightBefore, rightStep, rightAfterFen)

    assert(leftAfter.computation.staticBoard.asInstanceOf[AnyRef] eq rightAfter.computation.staticBoard.asInstanceOf[AnyRef])
    assert(!(leftAfter.computation.asInstanceOf[AnyRef] eq rightAfter.computation.asInstanceOf[AnyRef]))
    assertEquals(leftAfter.transitionFootprint.get.changedSquareSet.map(_.key), Set("b1", "d2"))
    assertEquals(rightAfter.transitionFootprint.get.changedSquareSet.map(_.key), Set("d2", "f1"))
    assertTransitionMatchesBoardDifference(leftBefore, leftAfter)
    assertTransitionMatchesBoardDifference(rightBefore, rightAfter)
    assert(
      !(leftAfter.relationTransition.get.asInstanceOf[AnyRef] eq rightAfter.relationTransition.get.asInstanceOf[AnyRef])
    )

  test("slider attack and full occupied-ray invalidation use different exact closures"):
    val secondOccupantMove = replay(
      "4k3/8/8/N7/8/P7/8/R3K3 w - - 0 1",
      "a5b7"
    )
    val secondOccupantFootprint = BoardGeometry.transitionFootprint(
      secondOccupantMove.move,
      BoardGeometry.moveEffect(secondOccupantMove.move)
    )
    assert(secondOccupantFootprint.affectedOccupiedRayOrigins(Square.A1))
    assert(!secondOccupantFootprint.affectedGeometricControlOrigins(Square.A1))

    val openedAttackMove = replay(
      "q3k3/8/8/8/N7/8/8/R3K3 w - - 0 1",
      "a4b6"
    )
    val openedAttackFootprint = BoardGeometry.transitionFootprint(
      openedAttackMove.move,
      BoardGeometry.moveEffect(openedAttackMove.move)
    )
    assert(openedAttackFootprint.affectedOccupiedRayOrigins(Square.A1))
    assert(openedAttackFootprint.affectedGeometricControlOrigins(Square.A1))

  test("a mismatched position and FEN cannot poison the semantic geometry cache"):
    val suppliedFen = "4k3/8/8/8/8/8/P7/4K3 w - - 0 1"
    val differentFen = "4k3/8/8/8/8/8/1P6/4K3 w - - 0 1"
    val differentPosition = PrincipalVariationEvidence.readPosition(differentFen).get

    intercept[IllegalArgumentException] {
      PositionAnalyzer.analyze(differentPosition, suppliedFen, 0)
    }

    val suppliedPosition = PrincipalVariationEvidence.readPosition(suppliedFen).get
    val analysis = PositionAnalyzer.analyze(suppliedPosition, suppliedFen, 0)
    assertEquals(analysis.position.board.pieceAt(chess.Square.A2), Some(chess.Piece(chess.Color.White, chess.Pawn)))
    assertEquals(analysis.position.board.pieceAt(chess.Square.B2), None)

  private def replay(fen: String, uci: String) =
    PrincipalVariationEvidence
      .legalMoveReplay(fen, List(uci), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail(s"expected legal replay: $fen $uci"))

  private def assertTransitionMatchesBoardDifference(
      before: PositionAnalysis,
    after: PositionAnalysis
  ): Unit =
    val transition = after.relationTransition.getOrElse(fail("expected a complete relation transition"))
    val beforeDetails = before.computation.staticBoard.relationSnapshot.factsByDetail.keySet
    val afterDetails = after.computation.staticBoard.relationSnapshot.factsByDetail.keySet
    val actualRemoved = transition.comparableRemoved.map(_.detail).toSet
    val actualEstablished = transition.comparableEstablished.map(_.detail).toSet
    assertEquals(actualRemoved, beforeDetails -- afterDetails)
    assertEquals(actualEstablished, afterDetails -- beforeDetails)
