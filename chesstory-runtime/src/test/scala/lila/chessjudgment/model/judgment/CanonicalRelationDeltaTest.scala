package lila.chessjudgment.model.judgment

import chess.{ Bishop, Color, King, Pawn, Piece, Queen, Rook, Square }
import chess.format.Fen

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.structure.{
  CanonicalTransitionStructuralDelta,
  StructuralDeltaAnalyzer,
  StructuralDeltaContracts
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.position.BoardGeometry

class CanonicalRelationDeltaTest extends munit.FunSuite:

  test("a legal move binds a created pin to its exact changed square and mover"):
    val delta = relationDelta("4k3/4n3/8/8/8/8/8/R5K1 w - - 0 1", "a1e1")
    val pin = delta.established
      .find(_.detail match
        case RelationWitnessDetail.RayBarrier(owner, _, _, occupants, geometry) =>
          RelationRayPattern.classify(owner, occupants, geometry.axis) == RelationRayPattern.AbsoluteKingPin
        case _ => false
      )
      .getOrElse(fail("expected the created pin"))

    assert(pin.proofKeys.contains(RelationProofKey.ChangedSquare(EvidenceSquare("e1"))))
    assert(
      pin.proofKeys.contains(
        RelationProofKey.MovedPiece(
          Color.White,
          EvidencePieceRole(Rook.name),
          EvidencePieceRole(Rook.name),
          EvidenceSquare("a1"),
          EvidenceSquare("e1")
        )
      )
    )

  test("one transition delta retains every local relation proof key without pairwise production"):
    val delta = relationDelta(
      "4k3/3p1p2/2n1b3/1q1P4/2B1P3/2N2N2/3Q1P2/4K3 w - - 0 1",
      "c4b5"
    )
    val moverKey = RelationProofKey.MovedPiece(
      Color.White,
      EvidencePieceRole(Bishop.name),
      EvidencePieceRole(Bishop.name),
      EvidenceSquare("c4"),
      EvidenceSquare("b5")
    )
    val moverChanges = delta.changes.filter(_.proofKeys.contains(moverKey))
    val establishedKinds = moverChanges
      .filter(_.direction == RelationChangeDirection.Established)
      .map(_.kind)
      .toSet

    assert(establishedKinds.contains(RelationFactKind.GeometricControl))
    assert(establishedKinds.contains(RelationFactKind.RayBarrier))
    assertEquals(delta.changes.map(_.stableKey).distinct.size, delta.changes.size)
    assert(delta.changes.forall(_.proofKeys.nonEmpty))

  test("piece-transition proof keys retain castling partners and promotion roles"):
    val castling = relationDelta("4k3/8/8/8/8/8/8/4K2R w K - 0 1", "e1g1")
    val promotion = relationDelta("4k3/P7/8/8/8/8/8/4K3 w - - 0 1", "a7a8q")

    val castlingMovers = castling.changes.flatMap(_.proofKeys).collect {
      case mover @ RelationProofKey.MovedPiece(_, _, _, _, _) => mover
    }.toSet
    assert(castlingMovers.contains(RelationProofKey.MovedPiece(
      Color.White,
      EvidencePieceRole(King.name),
      EvidencePieceRole(King.name),
      EvidenceSquare("e1"),
      EvidenceSquare("g1")
    )))
    assert(castlingMovers.contains(RelationProofKey.MovedPiece(
      Color.White,
      EvidencePieceRole(Rook.name),
      EvidencePieceRole(Rook.name),
      EvidenceSquare("h1"),
      EvidenceSquare("f1")
    )))

    val promotionMovers = promotion.changes.flatMap(_.proofKeys).collect {
      case mover @ RelationProofKey.MovedPiece(_, _, _, _, _) => mover
    }.toSet
    assert(promotionMovers.contains(RelationProofKey.MovedPiece(
      Color.White,
      EvidencePieceRole(Pawn.name),
      EvidencePieceRole(Queen.name),
      EvidenceSquare("a7"),
      EvidenceSquare("a8")
    )))

  test("the transition footprint records exact before and after occupants for special moves"):
    def changes(fen: String, move: String) =
      val step = PrincipalVariationEvidence
        .legalMoveReplay(fen, List(move), startPly = 0)
        .flatMap(_.headOption)
        .getOrElse(fail(s"expected legal move $move"))
      BoardGeometry.transitionFootprint(step.move, BoardGeometry.moveEffect(step.move)).cellChanges
        .map(change => change.square -> (change.before -> change.after))
        .toMap

    val castling = changes("4k3/8/8/8/8/8/8/4K2R w K - 0 1", "e1g1")
    assertEquals(castling(Square.E1), Some(Piece(Color.White, King)) -> None)
    assertEquals(castling(Square.F1), None -> Some(Piece(Color.White, Rook)))
    assertEquals(castling(Square.G1), None -> Some(Piece(Color.White, King)))
    assertEquals(castling(Square.H1), Some(Piece(Color.White, Rook)) -> None)

    val enPassant = changes("4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1", "e5d6")
    assertEquals(enPassant(Square.D5), Some(Piece(Color.Black, Pawn)) -> None)
    assertEquals(enPassant(Square.D6), None -> Some(Piece(Color.White, Pawn)))

    val promotion = changes("4k3/P7/8/8/8/8/8/4K3 w - - 0 1", "a7a8q")
    assertEquals(promotion(Square.A7), Some(Piece(Color.White, Pawn)) -> None)
    assertEquals(promotion(Square.A8), None -> Some(Piece(Color.White, Queen)))

    val promotedStep = PrincipalVariationEvidence
      .legalMoveReplay("4k3/P7/8/8/8/8/8/4K3 w - - 0 1", List("a7a8q"), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail("expected legal promotion"))
    assertEquals(
      BoardGeometry.transitionFootprint(
        promotedStep.move,
        BoardGeometry.moveEffect(promotedStep.move)
      ).pieceTransitions,
      List(lila.chessjudgment.model.position.BoardPieceTransition(
        Color.White,
        Pawn,
        Queen,
        Square.A7,
        Square.A8
      ))
    )

  test("same-side hypothetical legality does not promote geometric control into a structural result"):
    val delta = transitionDelta(
      "rn2k3/P7/8/8/8/8/8/K7 w - - 0 1",
      "a1b1"
    )
    assert(
      StructuralDeltaContracts.consequences(delta.structural).flatMap(_.relationKeys)
        .forall(_.kind != RelationFactKind.GeometricControl)
    )

  test("an established geometric control remains a relation without claiming future pressure"):
    val delta = transitionDelta(
      "4k3/8/8/8/3q4/8/8/R3K3 w - - 0 1",
      "a1d1"
    )
    val attack = delta.relationDelta.established
      .find(_.detail match
        case RelationWitnessDetail.GeometricControl(Color.White, attacker, _, target) =>
          attacker.key == "d1" && target.key == "d4"
        case _ => false
      )
      .getOrElse(fail("expected the newly established rook attack on d4"))

    assertEquals(attack.kind, RelationFactKind.GeometricControl)
    assert(
      StructuralDeltaContracts.consequences(delta.structural).flatMap(_.relationKeys)
        .forall(_.kind != RelationFactKind.GeometricControl)
    )

  test("turn-scoped snapshots do not masquerade as move-caused relation changes"):
    val delta = relationDelta(
      "4k3/8/8/8/3q4/8/3R4/4K3 w - - 0 1",
      "d2d1"
    )

    assert(!delta.changes.exists(_.kind == RelationFactKind.LegalMove))

  test("pawn tension is projected only from sparse canonical relation changes"):
    val created = transitionDelta("4k3/8/4p3/8/3P4/8/8/4K3 w - - 0 1", "d4d5")
    val resolved = transitionDelta("4k3/8/4p3/3P4/8/8/8/4K3 w - - 0 1", "d5d6")

    assertEquals(
      created.pawnTopology.createdTensions.map(edge => edge.from.key -> edge.to.key),
      List("d5" -> "e6")
    )
    assertEquals(created.pawnTopology.resolvedTensions, Nil)
    assertEquals(
      resolved.pawnTopology.resolvedTensions.map(edge => edge.from.key -> edge.to.key),
      List("d5" -> "e6")
    )
    assertEquals(resolved.pawnTopology.createdTensions, Nil)

  test("coexisting pawn tensions retain their exact local relation keys"):
    val delta = transitionDelta("4k3/8/8/3p1p2/8/8/4P3/4K3 w - - 0 1", "e2e4")
    val consequence = StructuralDeltaContracts
      .consequences(delta.structural)
      .find(_.kind == TransitionConsequenceKind.PawnTensionCreated)
      .getOrElse(fail("expected both pawn tensions created by e2e4"))
    val tensionBindings = consequence.subjectBindings.collect {
      case binding @ StructuralSubjectBinding(StructuralSubject.PawnTensionCreated(_, _, _), _, _) => binding
    }
    assertEquals(tensionBindings.size, 2)
    assert(tensionBindings.forall(_.relationKeys.size == 1))
    assertEquals(tensionBindings.flatMap(_.relationKeys).distinct.size, 2)

    val selected = consequence.copy(subjectBindings = List(tensionBindings.head))
    assertEquals(selected.relationKeys, tensionBindings.head.relationKeys)
    assertEquals(selected.relationKeys.size, 1)

  test("passed-pawn creation and advance are projected from changed passage facts"):
    val created = transitionDelta("k7/8/4p3/3P4/8/8/8/4R2K w - - 0 1", "e1e6")
    val advanced = transitionDelta("4k3/8/8/3P4/8/8/8/4K3 w - - 0 1", "d5d6")

    assertEquals(created.pawnTopology.passedCreated.map(_.square.key), List("d5"))
    assertEquals(created.pawnTopology.passedAdvanced, Nil)
    assertEquals(
      advanced.pawnTopology.passedAdvanced.map(item => (item.from.key, item.to.key, item.kind.toString)),
      List(("d5", "d6", "ExistingPasserAdvanced"))
    )
    assertEquals(advanced.pawnTopology.passedCreated, Nil)

  test("open and semi-open file changes inspect only pawn-touched files"):
    val opened = transitionDelta("4k3/8/8/8/8/1n6/P7/4K3 w - - 0 1", "a2b3")
    val semiOpened = transitionDelta("4k3/p7/8/8/8/1n6/P7/4K3 w - - 0 1", "a2b3")

    assertEquals(opened.pawnTopology.openedFiles.map(_.file.value), List(0))
    assertEquals(opened.pawnTopology.semiOpenedFiles, Nil)
    assertEquals(semiOpened.pawnTopology.openedFiles, Nil)
    assertEquals(semiOpened.pawnTopology.semiOpenedFiles.map(_.file.value), List(0))

  test("a missing changed relation cannot be certified as an exact delta"):
    intercept[IllegalArgumentException] {
      transitionDelta(
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        "e2e4",
        _.filterNot(_.relation.detail match
          case RelationWitnessDetail.PawnFileGroup(Color.White, file, _) => file.key.equalsIgnoreCase("e")
          case _                                                         => false
        )
      )
    }

  private def relationDelta(fen: String, moveUci: String): CanonicalRelationDelta =
    transitionDelta(fen, moveUci).canonicalRelations

  private def transitionDelta(
      fen: String,
      moveUci: String,
      afterInventory: List[CanonicalRelationNode] => List[CanonicalRelationNode] = identity
  ): CanonicalTransitionStructuralDelta =
    val legalStep = PrincipalVariationEvidence
      .legalMoveReplay(fen, List(moveUci), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail(s"expected legal move $moveUci"))
    val afterFen = Fen.write(legalStep.after).value
    val replay = CanonicalLineReplay
      .fromLegalReplay(List(legalStep))
      .getOrElse(fail("expected one canonical replay transition"))
    val transition = replay.onlyTransition.getOrElse(fail("expected one canonical replay transition"))
    val beforeRef = PositionNodeRef(fen, 0, Some(legalStep.before.color))
    val afterRef = PositionNodeRef(afterFen, 1, Some(legalStep.after.color))
    val beforeRelations = canonicalRelationSnapshot(
      canonicalRelations(transition.beforeAnalysis, beforeRef),
      beforeRef,
      transition.beforeAnalysis.relationInventory
    )
    val afterRelations = canonicalRelationSnapshot(
      afterInventory(canonicalRelations(transition.afterAnalysis, afterRef)),
      afterRef,
      transition.afterAnalysis.relationInventory
    )

    StructuralDeltaAnalyzer.bind(
      transition = transition,
      canonicalRelations = CanonicalRelationDelta.bind(
        transition.declared,
        transition.relationDelta,
        beforeRelations,
        afterRelations
      )
    )

  private def canonicalRelations(
      analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      position: PositionNodeRef
  ): List[CanonicalRelationNode] =
    TypedEvidenceGraph.empty
      .addAll(
        PositionRelationExtractor.records(
          analysis.boardRelations,
          position,
          EvidenceScope.CurrentPosition,
          relation => s"delta:${position.ply}:${relation.semanticId}"
        )
      )
      .relationGraph
      .nodes

  private def canonicalRelationSnapshot(
      nodes: List[CanonicalRelationNode],
      position: PositionNodeRef,
      inventory: PositionRelationExtractor.PositionRelationInventoryCertificate
  ): CanonicalPositionRelationSnapshot =
    TypedEvidenceGraph.empty
      .addAll(nodes.map(_.record))
      .relationGraph
      .closedPositionRelationSnapshot(position, EvidenceScope.CurrentPosition, inventory)
