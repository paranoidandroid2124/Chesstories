package lila.chessjudgment.analysis.tactical

import chess.{ Black, White }
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.relation.ClosedRelationEvidence
import lila.chessjudgment.model.judgment.{
  CanonicalLineReplay,
  EvidencePieceRole,
  EvidenceSquare,
  EvidenceConfidence,
  EvidenceScope,
  PositionNodeRef,
  RelationChangeDirection,
  RelationCheckResponseMode,
  RelationControlTarget,
  RelationFactEvidence,
  RelationFactKind,
  RelationRayPattern,
  RelationPawnConnectionDirection,
  RelationPawnConnectionKind,
  RelationPawnConnectionWitness,
  RelationPawnTopologyFacet,
  RelationPieceWitness,
  RelationWitnessDetail,
  TypedEvidenceGraph,
  VerticalRelationPremiseRole
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class ClosedRelationEvidenceTest extends munit.FunSuite:

  private def production(
      fen: String,
      moves: List[String]
  ): ClosedRelationEvidence.RelationProduction =
    val legalReplay = PrincipalVariationEvidence
      .legalMoveReplay(fen, moves, startPly = 0)
      .getOrElse(fail(s"expected legal replay: $fen ${moves.mkString(" ")}"))
    val replay = CanonicalLineReplay
      .fromLegalReplay(legalReplay)
      .getOrElse(fail("expected canonical legal replay"))
    ClosedRelationEvidence.relationProduction(replay, moves.head)

  private def relations(fen: String, moves: List[String]): List[RelationFactEvidence] =
    production(fen, moves).relations

  private def rayPattern(relation: RelationFactEvidence): Option[RelationRayPattern] =
    relation.detail match
      case RelationWitnessDetail.RayBarrier(owner, _, _, occupants, geometry) =>
        Some(RelationRayPattern.classify(owner, occupants, geometry.axis))
      case RelationWitnessDetail.NamedRayTransition(_, _, _, _, _, _, _, pattern, _, _) =>
        Some(pattern)
      case _ => None

  private def hasCreatedDoubleCheck(relation: RelationFactEvidence): Boolean =
    relation.detail match
      case RelationWitnessDetail.CreatedCheckResponseInventory(_, _, _, checkers, _, _, _, _) =>
        checkers.size >= 2
      case _ => false

  test("control-set deltas certify the last controller of an empty square"):
    val emptySquare = relations(
      "7k/8/8/8/8/8/8/5R1K w - - 0 1",
      List("f1a1")
    ).find(_.detail match
      case RelationWitnessDetail.GeometricControlSetDelta(_, White, target, _, _, _, _, _, _, _) =>
        target.key == "f6"
      case _ => false
    ).getOrElse(fail("expected a closed f6 controller delta"))

    assert(emptySquare.detail match
      case RelationWitnessDetail.GeometricControlSetDelta(
            _,
            White,
            target,
            RelationControlTarget.Empty,
            RelationControlTarget.Empty,
            before,
            after,
            removed,
            established,
            _
          ) =>
        target.key == "f6" && before.map(_.square.key) == List("f1") && after.isEmpty &&
          removed == before && established.isEmpty
      case _ => false
    )

  test("promotion and en-passant preserve exact named-ray transitions"):
    val promotion = relations(
      "2r4k/1P6/B7/8/8/8/8/4K3 w - - 0 1",
      List("b7b8q")
    )
    assert(promotion.exists(relation => relation.detail match
      case RelationWitnessDetail.NamedRayTransition(_, White, attacker, attackerRole, barrier, target, _, pattern, direction, _) =>
        attacker.key == "b8" && barrier.square.key == "c8" && target.exists(_.square.key == "h8") &&
          attackerRole.name.equalsIgnoreCase("queen") && barrier.role.name.equalsIgnoreCase("rook") &&
          target.exists(_.role.name.equalsIgnoreCase("king")) &&
          pattern == RelationRayPattern.AbsoluteKingPin && direction == RelationChangeDirection.Established
      case _ => false
    ))

    val enPassant = relations(
      "8/5k2/4n3/3pP3/4n3/8/B7/4K3 w - d6 0 1",
      List("e5d6")
    )
    assert(enPassant.exists(relation => relation.detail match
      case RelationWitnessDetail.NamedRayTransition(_, White, attacker, _, barrier, target, _, pattern, direction, _) =>
        attacker.key == "a2" && barrier.square.key == "e6" && target.exists(_.square.key == "f7") &&
          pattern == RelationRayPattern.AbsoluteKingPin && direction == RelationChangeDirection.Established
      case _ => false
    ))

  test("an unchanged distant pin is not projected onto an unrelated king move"):
    val unchanged = relations(
      "k7/n7/8/8/8/8/8/R3K3 w - - 0 1",
      List("e1f1")
    )
    assert(!unchanged.exists(rayPattern(_).contains(RelationRayPattern.AbsoluteKingPin)))

  test("moving the king out of a ray records the exact removed pin"):
    val unpinned = relations(
      "k7/n7/8/8/8/8/8/R3K3 b - - 0 1",
      List("a8b8")
    )
    val pinTransitions = unpinned.flatMap { relation =>
      relation.detail match
        case RelationWitnessDetail.NamedRayTransition(
              _,
              White,
              attacker,
              _,
              barrier,
              target,
              _,
              RelationRayPattern.AbsoluteKingPin,
              direction,
              _
            ) if attacker.key == "a1" && barrier.square.key == "a7" && target.exists(_.square.key == "a8") =>
          List(relation -> direction)
        case _ => Nil
    }

    assertEquals(pinTransitions.map(_._2), List(RelationChangeDirection.Removed))

  test("a replay-derived ray projection cannot enter the graph without its occurrence owner"):
    val fen = "4k3/8/2n5/8/2B5/8/8/4K3 w - - 0 1"
    val result = production(fen, List("c4b5"))
    val projected = result.relations.find(relation =>
      relation.kind == RelationFactKind.NamedRayTransition && relation.hasLineProof
    ).getOrElse(fail("expected one legal-replay named-ray transition"))
    val before = PositionNodeRef(fen, 0, Some(White), Some("ray-projection-before"))
    val record = RelationFactEvidence.record(
      id = "unlined-ray-projection",
      payload = projected,
      position = before,
      line = None,
      scope = EvidenceScope.CurrentPosition,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
    intercept[IllegalArgumentException](TypedEvidenceGraph.empty.add(record))

  test("distant ray churn does not recreate an unchanged named relation"):
    val unchangedBattery = relations(
      "4k3/Q7/8/n7/8/R7/8/R6K w - - 0 1",
      List("a7b7")
    )
    assert(!unchangedBattery.exists(rayPattern(_).contains(RelationRayPattern.Battery)))

  test("root ray projection follows the exact board delta, including castling and discovered rays"):
    val castlingPin = relations(
      "5k2/5n2/8/8/8/8/8/4K2R w K - 0 1",
      List("e1g1")
    )
    val discoveredPin = relations(
      "k7/n7/8/8/8/8/B7/R3K3 w - - 0 1",
      List("a2b3")
    )
    val bidirectionalBattery = relations(
      "6k1/8/8/8/b2R3b/8/4R3/6K1 w - - 0 1",
      List("e2e4")
    )

    assert(castlingPin.exists(relation => relation.detail match
      case RelationWitnessDetail.NamedRayTransition(_, White, attacker, _, barrier, target, _, pattern, direction, _) =>
        attacker.key == "f1" && barrier.square.key == "f7" && target.exists(_.square.key == "f8") &&
          pattern == RelationRayPattern.AbsoluteKingPin && direction == RelationChangeDirection.Established
      case _ => false
    ))
    assert(discoveredPin.exists(relation => relation.detail match
      case RelationWitnessDetail.NamedRayTransition(_, White, attacker, _, barrier, target, _, pattern, direction, _) =>
        attacker.key == "a1" && barrier.square.key == "a7" && target.exists(_.square.key == "a8") &&
          pattern == RelationRayPattern.AbsoluteKingPin && direction == RelationChangeDirection.Established
      case _ => false
    ))
    assertEquals(
      bidirectionalBattery.flatMap(_.detail match
        case RelationWitnessDetail.NamedRayTransition(
              _,
              White,
              _,
              _,
              _,
              Some(target),
              _,
              RelationRayPattern.Battery,
              RelationChangeDirection.Established,
              _
            ) => List(target.square.key)
        case _ => Nil
      ).toSet,
      Set("a4", "h4")
    )

  test("fresh double check retains both exact checkers"):
    val freshDoubleChecks = relations(
      "4k3/8/r7/8/8/8/4B3/4R1K1 w - - 0 1",
      List("e2b5")
    ).filter(hasCreatedDoubleCheck)

    assertEquals(freshDoubleChecks.size, 1)
    assertEquals(freshDoubleChecks.head.lineMoves, List("e2b5"))
    assert(freshDoubleChecks.head.detail match
      case RelationWitnessDetail.CreatedCheckResponseInventory(_, Black, kingSquare, checkers, _, _, _, _) =>
        kingSquare.key == "e8" &&
          checkers.map(checker => checker.square.key -> checker.role.name.toLowerCase).toSet ==
            Set("b5" -> "bishop", "e1" -> "rook")
      case _ => false
    )

  test("a pinned geometric recapture records one exact legal restriction and its cause"):
    val result = relations(
      "4k3/4n3/8/3p4/4P3/8/8/4R1K1 w - - 0 1",
      List("e4d5")
    )
    val availability = result.find(_.kind == RelationFactKind.CaptureRecaptureInventory)
      .getOrElse(fail("expected capture/recapture availability"))

    assert(availability.detail match
      case RelationWitnessDetail.CaptureRecaptureInventory(
            mover,
            captured,
            geometric,
            legal,
            restricted,
            proof
          ) =>
        val restrictedExactly =
          restricted match
            case List(restriction) =>
              restriction.piece ==
                RelationPieceWitness(EvidenceSquare("e7"), EvidencePieceRole("knight")) &&
                restriction.resource.destination.key == "d5" &&
                restriction.kingSquare.key == "e8" &&
                restriction.postMoveControllers ==
                  List(RelationPieceWitness(EvidenceSquare("e1"), EvidencePieceRole("rook"))) &&
                restriction.absolutePinPaths.exists(path =>
                  path.pinner == RelationPieceWitness(EvidenceSquare("e1"), EvidencePieceRole("rook")) &&
                    path.pinned == restriction.piece && path.kingSquare == restriction.kingSquare
                )
            case _ => false
        val exactLegalAbsence = proof.absences.exists(_.query match
          case PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(Black, from, to) =>
            from.key == "e7" && to.key == "d5"
          case _ => false
        )
        val exactExposureState = proof.states.exists(_.query match
          case PositionRelationExtractor.ClosedPositionStateQuery.OwnKingExposure(
                Black,
                piece,
                resource,
                kingSquare,
                controllers
              ) =>
            piece.square.key == "e7" && resource.destination.key == "d5" &&
              kingSquare.key == "e8" && controllers.map(_.square.key) == List("e1")
          case _ => false
        )
        mover.from.key == "e4" && mover.to.key == "d5" && captured.square.key == "d5" &&
          captured.side == Black &&
          geometric == List(RelationPieceWitness(EvidenceSquare("e7"), EvidencePieceRole("knight"))) &&
          legal.isEmpty && restrictedExactly && exactLegalAbsence && exactExposureState
      case _ => false
    )

  test("pawn topology preserves supporter direction while component closure stays undirected"):
    val topology = relations(
      "k7/8/8/3P4/2P5/8/8/7K w - - 0 1",
      List("c4c5")
    ).filter(_.kind == RelationFactKind.PawnTopologyTransition)
    val beforeStates = topology.flatMap(_.detail match
      case RelationWitnessDetail.PawnTopologyTransition(_, Some(before), _, _, _) => List(before)
      case _ => Nil
    ).map(state => state.square.key -> state).toMap
    val c4 = beforeStates.getOrElse("c4", fail("expected the moving c4 pawn state"))
    val d5 = beforeStates.getOrElse("d5", fail("expected the supported d5 pawn state"))

    assert(c4.connections.contains(RelationPawnConnectionWitness(
      EvidenceSquare("d5"),
      RelationPawnConnectionKind.GeometricSupport,
      RelationPawnConnectionDirection.Supports
    )))
    assert(d5.connections.contains(RelationPawnConnectionWitness(
      EvidenceSquare("c4"),
      RelationPawnConnectionKind.GeometricSupport,
      RelationPawnConnectionDirection.SupportedBy
    )))
