package lila.chessjudgment.analysis.relation

import chess.{ Black, White }
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.judgment.{
  CanonicalLineReplay,
  EvidencePieceRole,
  EvidenceSquare,
  RelationCheckResponseMode,
  RelationControlTarget,
  RelationFactEvidence,
  RelationFactKind,
  RelationPawnConnectionDirection,
  RelationPawnConnectionKind,
  RelationPawnConnectionWitness,
  RelationPawnTopologyFacet,
  RelationPieceWitness,
  RelationWitnessDetail,
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
