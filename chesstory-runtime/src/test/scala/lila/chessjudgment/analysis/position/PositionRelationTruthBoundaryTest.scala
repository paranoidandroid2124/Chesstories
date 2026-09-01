package lila.chessjudgment.analysis.position

import chess.{ Color, HalfMoveClock, Square }
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.model.judgment.{
  EvidenceSemanticAnchorKind,
  EvidenceScope,
  EvidenceSquare,
  PositionNodeRef,
  CanonicalLineReplay,
  RelationAxisSignal,
  RelationFactEvidence,
  RelationFactKind,
  RelationRayProjection,
  RelationWitnessDetail,
  TypedEvidenceGraph
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.position.BoardGeometry

class PositionRelationTruthBoundaryTest extends munit.FunSuite:

  private def position(fen: String) =
    Fen.read(Standard, Fen.Full(fen)).get

  private def relations(fen: String): List[RelationFactEvidence] =
    PositionAnalyzer.analyze(position(fen), fen, plyCount = 0).boardRelations

  private def closedSnapshot(
      analysis: PositionAnalysis,
      positionRef: PositionNodeRef,
      scope: EvidenceScope = EvidenceScope.CurrentPosition
  ) =
    val records = PositionRelationExtractor.records(
      analysis.boardRelations,
      positionRef,
      scope,
      relation => s"closed:${positionRef.ply}:${relation.semanticId}"
    )
    TypedEvidenceGraph.empty
      .addAll(records)
      .relationGraph
      .closedPositionRelationSnapshot(positionRef, scope, analysis.relationInventory)

  private def hasAbsoluteKingPinGeometry(relation: RelationFactEvidence): Boolean =
    relation.detail match
      case ray: RelationWitnessDetail.RayBarrier =>
        RelationRayProjection.isAbsoluteKingPinGeometry(ray)
      case _ => false

  test("one board geometry owner supplies rays and geometric controls"):
    val current = position("7k/8/8/6r1/8/8/8/2B3K1 w - - 0 1")
    val bishop = current.board.pieceAt(Square.C1).getOrElse(fail("expected bishop on c1"))
    val analysis = PositionAnalyzer.analyze(current, Fen.write(current).value, 0)

    assertEquals(
      analysis.boardRelations.flatMap(_.detail match
        case RelationWitnessDetail.GeometricControl(_, attacker, _, target)
            if attacker.key == Square.C1.key => Square.fromKey(target.key).toList
        case _ => Nil
      )
        .filter(_.file.value > Square.C1.file.value)
        .toList
        .sortBy(_.file.value)
        .map(_.key),
      List("d2", "e3", "f4", "g5")
    )
    assertEquals(
      analysis.computation.staticBoard.geometricControlInventory.byOrigin(Square.C1),
      Square.C1.bishopAttacks(current.board.occupied)
    )

  test("the initial position exposes exact pawn topology without tactical substitutes"):
    val initial = relations("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    assertEquals(initial.count(_.kind == RelationFactKind.PawnFileGroup), 16)
    assertEquals(initial.count(_.kind == RelationFactKind.PawnFrontOccupancy), 16)
    assertEquals(initial.count(_.kind == RelationFactKind.PawnPassage), 16)
    assert(initial.exists(_.kind == RelationFactKind.GeometricControl))
    assert(initial.exists(_.kind == RelationFactKind.LegalMove))

  test("geometric control and legal capture remain distinct canonical facts"):
    val currentRelations = relations("4k3/8/8/8/3q4/8/3R4/4K3 w - - 0 1")
    val capture = currentRelations.find(_.detail match
      case RelationWitnessDetail.LegalMove(Color.White, from, _, to, moveUci, Some(captured)) =>
        from.key == "d2" && to.key == "d4" && moveUci == "d2d4" &&
          captured.capturedSquare.key == "d4" && captured.capturedSide == Color.Black
      case _ => false
    ).getOrElse(fail("expected the exact legal rook capture on d4"))

    assert(capture.hasConcreteWitness)
    assert(capture.semanticGroupingAnchors.exists(_.kind == EvidenceSemanticAnchorKind.Relation))

    val controls = currentRelations.collect {
      case relation if relation.kind == RelationFactKind.GeometricControl => relation.detail
    }
    assert(controls.exists {
      case RelationWitnessDetail.GeometricControl(
            Color.White,
            attacker,
            _,
            target
          ) =>
        attacker.key == "d2" && target.key == "d4"
      case _ => false
    })
    assert(controls.exists {
      case RelationWitnessDetail.GeometricControl(
            Color.Black,
            attacker,
            _,
            target
          ) =>
        attacker.key == "d4" && target.key == "d2"
      case _ => false
    })
    assertEquals(
      currentRelations.count(_.detail match
        case RelationWitnessDetail.LegalMove(Color.Black, _, _, _, _, _) => true
        case _                                                           => false
      ),
      0,
      "the non-moving side's geometric attack must not become a hypothetical legal move"
    )

  test("en passant legal capture is owned by the captured pawn square without expanding child replies"):
    val beforeFen = "7k/8/8/8/3p4/8/4P3/K3R3 w - - 0 1"
    val advance = PrincipalVariationEvidence
      .legalMoveReplay(beforeFen, List("e2e4"), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail("expected the double pawn advance"))
    val afterFen = Fen.write(advance.after).value
    val beforeAnalysis = PositionAnalyzer.analyze(advance.before, beforeFen, plyCount = 0)
    val afterAnalysis = PositionAnalyzer.analyzeAfter(beforeAnalysis, advance, afterFen)
    val pressure = afterAnalysis.boardRelations
      .find(_.detail match
        case RelationWitnessDetail.LegalMove(Color.Black, from, _, to, moveUci, Some(capture)) =>
          from.key == "d4" && to.key == "e3" && moveUci == "d4e3" && capture.capturedSquare.key == "e4"
        case _ => false
      )
      .getOrElse(fail("expected the en-passant legal capture"))

    pressure.detail match
      case RelationWitnessDetail.LegalMove(_, _, _, _, _, Some(capture)) =>
        assertEquals(capture.capturedSquare.key, "e4")
        assertEquals(capture.capturedSide, Color.White)
      case detail => fail(s"expected LegalMove, got ${detail.detailName}")

  test("every position relation uses the canonical relation payload"):
    val currentRelations = relations("4k3/8/8/8/3q4/8/3R4/4K3 w - - 0 1")
    assert(currentRelations.nonEmpty)
    assert(currentRelations.forall(_.hasConcreteWitness))

  test("closed L0 state, bidirectional controls, and legal effects reuse one inventory"):
    val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val initial = PositionAnalyzer.analyze(position(initialFen), initialFen, plyCount = 0)
    val initialRef = PositionNodeRef(initialFen, 0, Some(Color.White))
    val closedInitial = closedSnapshot(initial, initialRef)
    assertEquals(closedInitial.inventory.stateView.kingSquare(Color.White).map(_.key), Some("e1"))
    assertEquals(
      closedInitial.inventory.stateView.pieces(Color.White, lila.chessjudgment.model.judgment.EvidencePieceRole("pawn")).size,
      8
    )
    val vacantE4 = closedInitial.inventory.certifyState(
      PositionRelationExtractor.ClosedPositionStateQuery.Vacant(EvidenceSquare("e4"))
    ).getOrElse(fail("expected the exact closed vacancy on e4"))
    val cachedVacantE4 = closedInitial.inventory.certifyState(
      PositionRelationExtractor.ClosedPositionStateQuery.Vacant(EvidenceSquare("e4"))
    ).getOrElse(fail("expected the cached exact vacancy on e4"))
    assert(vacantE4.asInstanceOf[AnyRef] eq cachedVacantE4.asInstanceOf[AnyRef])
    assertEquals(
      closedInitial.inventory.certifyState(
        PositionRelationExtractor.ClosedPositionStateQuery.Vacant(EvidenceSquare("e2"))
      ),
      None
    )
    assert(initial.boardRelations.asInstanceOf[AnyRef] eq initial.relationInventory.relations.asInstanceOf[AnyRef])

    val beforeFen = "7k/8/8/8/3p4/8/4P3/K3R3 w - - 0 1"
    val step = PrincipalVariationEvidence
      .legalMoveReplay(beforeFen, List("e2e4"), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail("expected the double pawn advance"))
    val before = PositionAnalyzer.analyze(step.before, beforeFen, plyCount = 0)
    val afterFen = Fen.write(step.after).value
    val after = PositionAnalyzer.analyzeAfter(before, step, afterFen)
    val closedAfter = closedSnapshot(
      after,
      PositionNodeRef(afterFen, after.occurrence.plyCount, Some(Color.Black))
    )
    assertEquals(closedAfter.inventory.stateView.potentialEnPassantSquare.map(_.key), Some("e3"))
    val enPassant = closedAfter.inventory.legalMovesFrom(EvidenceSquare("d4")).find(_.moveUci == "d4e3").getOrElse(
      fail("expected the exact en-passant legal resource")
    )
    assertEquals(enPassant.capture.map(_.capturedSquare.key), Some("e4"))
    assertEquals(enPassant.destination.key, "e3")
    assertEquals(enPassant.boardEffect.cellChanges.map(_.square.key).toSet, Set("d4", "e3", "e4"))
    assertEquals(enPassant.capture.map(_.capturedSquare.key), Some("e4"))
    assertEquals(closedAfter.inventory.legalCapturesOf(EvidenceSquare("e4")).map(_.source.semanticId), List(enPassant.source.semanticId))
    val fromOrigin = closedAfter.inventory.controlsFrom(EvidenceSquare("d4")).find(_.targetSquare.key == "e3")
    val fromTarget = closedAfter.inventory.controlsAt(Color.Black, EvidenceSquare("e3")).find(_.controller.square.key == "d4")
    assertEquals(fromOrigin.map(_.source.semanticId), fromTarget.map(_.source.semanticId))

    val castlingFen = "4k3/8/8/8/8/8/8/4K2R w K - 0 1"
    val castlingAnalysis = PositionAnalyzer.analyze(position(castlingFen), castlingFen, plyCount = 0)
    assertEquals(castlingAnalysis.actualLegalMoves.filter(_.castle.nonEmpty).map(_.toUci.uci), List("e1g1"))
    val castling = closedSnapshot(
      castlingAnalysis,
      PositionNodeRef(castlingFen, 0, Some(Color.White))
    ).inventory.legalMovesFrom(EvidenceSquare("e1")).find(_.moveUci == "e1g1").getOrElse(
      fail("expected the exact castling legal resource")
    )
    assertEquals(castling.mode, PositionRelationExtractor.ClosedLegalMovementMode.Castling)
    assertEquals(
      castling.boardEffect.pieceTransitions.map(movement => movement.from.key -> movement.to.key).toSet,
      Set("e1" -> "g1", "h1" -> "f1")
    )
    assertEquals(castling.boardEffect.cellChanges.map(_.square.key).toSet, Set("e1", "f1", "g1", "h1"))

    val promotionFen = "7k/P7/8/8/8/8/8/4K3 w - - 0 1"
    val promotionAnalysis = PositionAnalyzer.analyze(position(promotionFen), promotionFen, plyCount = 0)
    val promotion = closedSnapshot(
      promotionAnalysis,
      PositionNodeRef(promotionFen, 0, Some(Color.White))
    ).inventory.legalMovesFrom(EvidenceSquare("a7")).find(_.moveUci == "a7a8q").getOrElse(
      fail("expected the exact promotion legal resource")
    )
    assertEquals(promotion.movement.beforeRole.name, "pawn")
    assertEquals(promotion.movement.afterRole.name, "queen")
    assertEquals(promotion.boardEffect.cellChanges.map(_.square.key).toSet, Set("a7", "a8"))

  test("closed king responses classify every legal reply without a reply search"):
    import PositionRelationExtractor.ClosedKingResponseMode.*

    val fen = "4r2k/8/8/8/8/8/3B4/4K3 w - - 0 1"
    val analysis = PositionAnalyzer.analyze(position(fen), fen, plyCount = 0)
    val closed = closedSnapshot(analysis, PositionNodeRef(fen, 0, Some(Color.White)))
    val king = closed.inventory.kingResponses
    assert(king.inCheck)
    assertEquals(king.checkers.map(_.controller.square.key), List("e8"))
    assertEquals(king.responses.map(_.move), king.legalMoves)
    assert(king.responses.exists(_.modes(KingMove)))
    assert(king.responses.exists(response => response.move.moveUci == "d2e3" && response.modes(Interpose)))
    assert(king.interpositionRay.exists(_.detail match
      case RelationWitnessDetail.RayBarrier(Color.Black, attacker, role, occupants, _) =>
        attacker.key == "e8" && role.name.equalsIgnoreCase("rook") &&
          occupants.headOption.exists(_.square.key == "e1")
      case _ => false
    ))
    assertEquals(king.terminal, PositionRelationExtractor.ClosedKingTerminalState.Ongoing)

  test("closed pawn topology is projected from canonical L0 sources"):
    val pawnFen = "4k3/8/8/8/3P4/2P1P3/P7/4K3 w - - 0 1"
    val pawnAnalysis = PositionAnalyzer.analyze(position(pawnFen), pawnFen, plyCount = 0)
    val pawnTopology = closedSnapshot(
      pawnAnalysis,
      PositionNodeRef(pawnFen, 0, Some(Color.White))
    ).inventory.pawnTopologyView
    val d4 = pawnTopology.pawns.find(pawn => pawn.side == Color.White && pawn.square.key == "d4").getOrElse(
      fail("expected the d4 pawn projection")
    )
    val a2 = pawnTopology.pawns.find(pawn => pawn.side == Color.White && pawn.square.key == "a2").getOrElse(
      fail("expected the a2 pawn projection")
    )
    assert(d4.passed && d4.geometricallyProtectedPasser)
    assertEquals(d4.componentPawns.map(_.key), List("c3", "d4", "e3"))
    assert(a2.isolated && !a2.geometricallyProtectedPasser)
  test("an ordered ray retains distant occupants and invalidates when the third occupant moves"):
    val beforeFen = "k2q4/8/3p4/8/3N4/8/8/3R3K b - - 0 1"
    val before = PositionAnalyzer.analyze(position(beforeFen), beforeFen, plyCount = 0)
    val beforeRay = before.boardRelations.find(_.detail match
      case RelationWitnessDetail.RayBarrier(Color.White, attacker, _, occupants, geometry) =>
        geometry.axis == RelationAxisSignal.File &&
          attacker.key == "d1" && occupants.map(_.square.key) == List("d4", "d6", "d8")
      case _ => false
    ).getOrElse(fail("expected the complete d-file occupancy chain"))
    assertEquals(beforeRay.targetSquares.map(_.key), List("d4"))
    assertEquals(
      beforeRay.focusSquares.map(_.key),
      List("d1", "d4", "d6", "d8"),
      "distant occupants remain exact witnesses without becoming immediate tactical targets"
    )

    val step = PrincipalVariationEvidence
      .legalMoveReplay(beforeFen, List("d8e8"), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail("expected the queen move from the third ray occupant"))
    val afterFen = Fen.write(step.after).value
    val incremental = PositionAnalyzer.analyzeAfter(before, step, afterFen).boardRelations
    val cold = PositionComputation
      .from(StaticBoardGeometryComputation.cold(step.after.board), step.after)
      .boardRelations
    assertEquals(incremental, cold)
    assert(incremental.exists(_.detail match
      case RelationWitnessDetail.RayBarrier(Color.White, attacker, _, occupants, geometry) =>
        geometry.axis == RelationAxisSignal.File &&
          attacker.key == "d1" && occupants.map(_.square.key) == List("d4", "d6")
      case _ => false
    ))

  test("Doknjas Bf7 removes one rook support without proving that f6 has no supporter"):
    val beforeFen = "3r1r1k/1p4pp/p2pbp2/4n3/q2BPR2/2PB2Q1/P1P3PP/R6K b - - 7 24"
    val before = PositionAnalyzer.analyze(position(beforeFen), beforeFen, plyCount = 0)
    val step = PrincipalVariationEvidence
      .legalMoveReplay(beforeFen, List("e6f7"), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail("expected Doknjas's ...Bf7"))
    val afterFen = Fen.write(step.after).value
    val after = PositionAnalyzer.analyzeAfter(before, step, afterFen)
    val transition = after.relationTransition.getOrElse(fail("expected the exact relation transition"))

    assert(transition.comparableRemoved.exists(_.detail match
      case RelationWitnessDetail.GeometricControl(
            Color.Black,
            controller,
            _,
            target
          ) => controller.key == "f8" && target.key == "f6"
      case _ => false
    ))
    assert(transition.comparableEstablished.exists(_.detail match
      case RelationWitnessDetail.RayBarrier(
            Color.Black,
            attacker,
            _,
            occupants,
            geometry
          ) =>
        geometry.axis == RelationAxisSignal.File &&
          attacker.key == "f8" && occupants.map(_.square.key) == List("f7", "f6", "f4")
      case _ => false
    ))
    assert(after.boardRelations.exists(_.detail match
      case RelationWitnessDetail.GeometricControl(
            Color.Black,
            controller,
            _,
            target
          ) => controller.key == "g7" && target.key == "f6"
      case _ => false
    ))
  test("one canonical ray inventory preserves ordered occupants and exact absolute king pins"):
    def hasOrderedRay(
        facts: List[RelationFactEvidence],
        attacker: String,
        occupants: List[String]
    ): Boolean =
      facts.exists(_.detail match
        case RelationWitnessDetail.RayBarrier(_, exactAttacker, _, exactOccupants, _) =>
          exactAttacker.key == attacker && exactOccupants.map(_.square.key) == occupants
        case _ => false
      )

    val pin = relations("4k3/4n3/8/8/8/8/8/4R1K1 b - - 0 1")
    val screenedEnemy = relations("k3q3/8/8/4b3/8/8/8/4R1K1 w - - 0 1")
    val supportedSlider = relations("k3q3/8/8/8/8/8/4Q3/4R1K1 w - - 0 1")
    val kingFirst = relations("4q3/8/8/4k3/8/8/8/4R1K1 b - - 0 1")

    assert(hasOrderedRay(pin, "e1", List("e7", "e8")))
    assert(pin.exists(hasAbsoluteKingPinGeometry))
    assert(hasOrderedRay(screenedEnemy, "e1", List("e5", "e8")))
    assert(hasOrderedRay(supportedSlider, "e1", List("e2", "e8")))
    assert(hasOrderedRay(kingFirst, "e1", List("e5", "e8")))
    assert((pin ++ screenedEnemy ++ supportedSlider ++ kingFirst).forall(relation =>
      relation.kind != RelationFactKind.RayBarrier || relation.isPositionRelation
    ))


  test("the canonical relation delta retains the exact established ordered ray"):
    val beforeFen = "4k3/4n3/8/8/8/8/8/R5K1 w - - 0 1"
    val legalReplay = PrincipalVariationEvidence
      .legalMoveReplay(beforeFen, List("a1e1"), startPly = 0)
      .getOrElse(fail("expected a legal pin-creating move"))
    val replay = CanonicalLineReplay
      .fromLegalReplay(legalReplay)
      .getOrElse(fail("expected canonical pin replay"))
    val after = legalReplay.head.after
    val afterFen = Fen.write(after).value
    val canonicalPin = PositionAnalyzer
      .analyze(after, afterFen, plyCount = 1)
      .boardRelations
      .find(hasAbsoluteKingPinGeometry)
      .getOrElse(fail("expected the canonical after-position pin"))
    val establishedPin = replay.onlyTransition
      .getOrElse(fail("expected one replay transition"))
      .relationDelta
      .establishedOf(RelationFactKind.RayBarrier)
      .find(change => hasAbsoluteKingPinGeometry(change.relation))
      .getOrElse(fail("expected the canonical established pin change"))

    assertEquals(establishedPin.relation.detail, canonicalPin.detail)
    assert(establishedPin.proofKeys.nonEmpty)

  test("semantic geometry is shared across clock fields while legal move occurrences remain distinct"):
    val firstFen = "4k3/8/8/8/3q4/8/3R4/4K3 w - - 0 1"
    val secondFen = "4k3/8/8/8/3q4/8/3R4/4K3 w - - 37 92"
    val first = PositionAnalyzer.analyze(position(firstFen), firstFen, plyCount = 1)
    val second = PositionAnalyzer.analyze(position(secondFen), secondFen, plyCount = 183)

    assertEquals(first.occurrence.fen, firstFen)
    assertEquals(second.occurrence.fen, secondFen)
    assertEquals(first.occurrence.plyCount, 1)
    assertEquals(second.occurrence.plyCount, 183)
    assert(!(first.actualLegalMoves.asInstanceOf[AnyRef] eq second.actualLegalMoves.asInstanceOf[AnyRef]))
    assertEquals(first.actualLegalMoves.map(_.toUci.uci), second.actualLegalMoves.map(_.toUci.uci))
    assert(
      first.actualLegalMoves.zip(second.actualLegalMoves).exists { case (left, right) =>
        Fen.write(left.after).value != Fen.write(right.after).value
      }
    )
    assert(first.computation.staticBoard.asInstanceOf[AnyRef] eq second.computation.staticBoard.asInstanceOf[AnyRef])
    assert(!(first.computation.asInstanceOf[AnyRef] eq second.computation.asInstanceOf[AnyRef]))
    assert(!(first.relationInventory.asInstanceOf[AnyRef] eq second.relationInventory.asInstanceOf[AnyRef]))
    assert(first.pawnTopology.asInstanceOf[AnyRef] eq second.pawnTopology.asInstanceOf[AnyRef])
    assertEquals(first.boardRelations.map(_.semanticId), second.boardRelations.map(_.semanticId))
    assert(first.boardRelations != second.boardRelations)
    assert(!(first.boardRelations.asInstanceOf[AnyRef] eq second.boardRelations.asInstanceOf[AnyRef]))

  test("canonical relation producers are unique without defensive deduplication"):
    val current = relations("4k3/3p1p2/2n1b3/1q1P4/2B1P3/2N2N2/3Q1P2/4K3 w - - 0 1")
    assertEquals(current.map(_.semanticId).distinct.size, current.size)

  test("all consumers share one normalized full-FEN parse while clock-distinct positions remain distinct"):
    val fen = "4k3/8/8/8/3q4/8/3R4/4K3 w - - 0 1"
    val first = PrincipalVariationEvidence.readPosition(fen).getOrElse(fail("expected a legal position"))
    val repeated = PrincipalVariationEvidence
      .readPosition(s"  ${fen.replace(" ", "   ")}  ")
      .getOrElse(fail("expected the normalized position"))
    val differentClock = PrincipalVariationEvidence
      .readPosition("4k3/8/8/8/3q4/8/3R4/4K3 w - - 37 92")
      .getOrElse(fail("expected the clock-distinct position"))

    assert(first.asInstanceOf[AnyRef] eq repeated.asInstanceOf[AnyRef])
    assert(!(first.asInstanceOf[AnyRef] eq differentClock.asInstanceOf[AnyRef]))
    assertEquals(first.history.halfMoveClock, HalfMoveClock(0))
    assertEquals(differentClock.history.halfMoveClock, HalfMoveClock(37))
