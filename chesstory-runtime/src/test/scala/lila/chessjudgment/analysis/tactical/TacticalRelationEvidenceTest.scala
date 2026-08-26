package lila.chessjudgment.analysis.tactical

import chess.{ Black, Color, White }
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.judgment.{
  CanonicalRelationDelta,
  CanonicalLineReplay,
  CanonicalPositionRelationSnapshot,
  EvidencePieceRole,
  EvidenceSquare,
  EvidenceConfidence,
  EvidenceScope,
  PositionNodeRef,
  RelationBlockerRemovalMode,
  RelationControlTarget,
  RelationCombinationContractKind,
  RelationFactEvidence,
  RelationFactKind,
  RelationRayPattern,
  RelationMoveTransitionWitness,
  RelationPieceWitness,
  RelationWitnessDetail,
  TypedEvidenceGraph
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class TacticalRelationEvidenceTest extends munit.FunSuite:

  private def production(
      fen: String,
      moves: List[String]
  ): TacticalRelationEvidence.RelationProduction =
    val legalReplay = PrincipalVariationEvidence
      .legalMoveReplay(fen, moves, startPly = 0)
      .getOrElse(fail(s"expected legal replay: $fen ${moves.mkString(" ")}"))
    val replay = CanonicalLineReplay
      .fromLegalReplay(legalReplay)
      .getOrElse(fail("expected canonical legal replay"))
    TacticalRelationEvidence.relationProduction(replay, moves.head)

  private def relations(fen: String, moves: List[String]): List[RelationFactEvidence] =
    production(fen, moves).relations

  private def targets(
      relations: List[RelationFactEvidence],
      kind: RelationFactKind
  ): Set[String] =
    relations.filter(_.kind == kind).flatMap(_.targetSquares.map(_.key)).toSet

  private def accessSignatures(
      relations: List[RelationFactEvidence]
  ): Set[(Color, String, RelationControlTarget, RelationRayPattern, RelationBlockerRemovalMode)] =
    relations.flatMap(_.detail match
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
            _,
            controllerSide,
            _,
            _,
            _,
            _,
            _,
            target,
            targetState,
            barrierPattern,
            removalMode,
            _
          ) =>
        List((controllerSide, target.key, targetState, barrierPattern, removalMode))
      case _ => Nil
    ).toSet

  private def rootMover(detail: RelationWitnessDetail): Option[RelationMoveTransitionWitness] =
    detail match
      case RelationWitnessDetail.GeometricSupporterCapture(mover, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.GeometricControlSetDelta(mover, _, _, _, _, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.GeometricSupportDelta(mover, _, _, _, _, _, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.SliderControlInterference(mover, _, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(mover, _, _, _, _, _, _, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.CheckingEnemyControlBundle(mover, _, _, _) => Some(mover)
      case RelationWitnessDetail.DoubleCheck(mover, _, _, _) => Some(mover)
      case _ => None

  private def rayPattern(relation: RelationFactEvidence): Option[RelationRayPattern] =
    relation.detail match
      case RelationWitnessDetail.RayBarrier(owner, _, _, occupants, axis) =>
        Some(RelationRayPattern.classify(owner, occupants, axis))
      case _ => None

  test("geometric support deltas preserve beneficiary identity and exact added supporters"):
    val doknjasSupport = relations(
      "2rq1rk1/1p2bppp/3pbn2/p3p3/Pn2P3/BPN3P1/2PQNPBP/2R2RK1 b - - 3 14",
      List("f8e8")
    )
    assert(doknjasSupport.exists(_.detail match
      case RelationWitnessDetail.GeometricSupportDelta(
            mover,
            Black,
            supportedBefore,
            supportedBeforeRole,
            supportedAfter,
            supportedAfterRole,
            beforeSupporters,
            afterSupporters,
            removed,
            established,
            _
          ) =>
        mover.from.key == "f8" && mover.to.key == "e8" &&
          supportedBefore.key == "e7" && supportedAfter.key == "e7" &&
          supportedBeforeRole.name.equalsIgnoreCase("bishop") &&
          supportedAfterRole.name.equalsIgnoreCase("bishop") &&
          beforeSupporters.toSet == Set(RelationPieceWitness(EvidenceSquare("d8"), EvidencePieceRole("queen"))) &&
          afterSupporters.toSet == beforeSupporters.toSet ++ established && removed.isEmpty &&
          established == List(RelationPieceWitness(EvidenceSquare("e8"), EvidencePieceRole("rook")))
      case _ => false
    ))

    val occupiedControlledSquare = relations(
      "7k/8/8/8/8/8/1B6/R6K w - - 0 1",
      List("b2a3")
    )
    assert(occupiedControlledSquare.exists(_.detail match
      case RelationWitnessDetail.GeometricSupportDelta(
            _,
            White,
            supportedBefore,
            supportedBeforeRole,
            supportedAfter,
            supportedAfterRole,
            beforeSupporters,
            afterSupporters,
            removed,
            established,
            _
          ) =>
        supportedBefore.key == "b2" && supportedAfter.key == "a3" &&
          supportedBeforeRole.name.equalsIgnoreCase("bishop") &&
          supportedAfterRole.name.equalsIgnoreCase("bishop") && beforeSupporters.isEmpty &&
          afterSupporters == established && removed.isEmpty &&
          established == List(RelationPieceWitness(EvidenceSquare("a1"), EvidencePieceRole("rook")))
      case _ => false
    ))

  test("one support delta closes every changed supporter for one continuing piece"):
    val replacement = relations(
      "7k/8/8/8/8/N7/8/RR5K w - - 0 1",
      List("a3b5")
    ).filter(_.kind == RelationFactKind.GeometricSupportDelta)
    val movedKnight = replacement.find(_.detail match
      case RelationWitnessDetail.GeometricSupportDelta(_, White, before, _, after, _, _, _, _, _, _) =>
        before.key == "a3" && after.key == "b5"
      case _ => false
    ).getOrElse(fail("expected the moved knight's complete support replacement"))
    assert(movedKnight.detail match
      case RelationWitnessDetail.GeometricSupportDelta(
            _,
            White,
            before,
            _,
            after,
            _,
            beforeSupporters,
            afterSupporters,
            removed,
            established,
            _
          ) =>
        before.key == "a3" && after.key == "b5" &&
          beforeSupporters.map(_.square.key).toSet == Set("a1") &&
          afterSupporters.map(_.square.key).toSet == Set("b1") &&
          removed.map(_.square.key).toSet == Set("a1") &&
          established.map(_.square.key).toSet == Set("b1")
      case _ => false
    )
    assert(replacement.exists(_.detail match
      case RelationWitnessDetail.GeometricSupportDelta(
            _,
            White,
            before,
            _,
            after,
            _,
            beforeSupporters,
            afterSupporters,
            removed,
            established,
            _
          ) =>
        before.key == "b1" && after.key == "b1" &&
          beforeSupporters.map(_.square.key).toSet == Set("a1", "a3") &&
          afterSupporters.map(_.square.key).toSet == Set("a1") &&
          removed.map(_.square.key).toSet == Set("a3") && established.isEmpty
      case _ => false
    ))

    val allRemoved = relations(
      "7k/8/8/8/4N3/8/8/1B2R2K w - - 0 1",
      List("e4c5")
    ).filter(_.kind == RelationFactKind.GeometricSupportDelta)
    val knight = allRemoved.find(_.detail match
      case RelationWitnessDetail.GeometricSupportDelta(_, White, before, _, after, _, _, _, _, _, _) =>
        before.key == "e4" && after.key == "c5"
      case _ => false
    ).getOrElse(fail("expected the complete knight support loss"))
    assert(knight.detail match
      case RelationWitnessDetail.GeometricSupportDelta(_, _, _, _, _, _, beforeSupporters, afterSupporters, removed, established, _) =>
        beforeSupporters.map(_.square.key).toSet == Set("b1", "e1") && afterSupporters.isEmpty &&
          removed.map(_.square.key).toSet == Set("b1", "e1") && established.isEmpty
      case _ => false
    )

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

  test("an own blocker clearance is proved by removed and established canonical controls"):
    val witnesses = relations(
      "q6k/6p1/8/6N1/8/8/B7/R3K3 w - - 0 1",
      List("a2b3", "g7g6", "g5f7")
    ).filter(_.kind == RelationFactKind.GeometricLineControlAfterBlockerRemoval)

    assertEquals(
      accessSignatures(witnesses),
      Set(
        (White, "a3", RelationControlTarget.Empty, RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (White, "a4", RelationControlTarget.Empty, RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (White, "a5", RelationControlTarget.Empty, RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (White, "a6", RelationControlTarget.Empty, RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (White, "a7", RelationControlTarget.Empty, RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (White, "a8", RelationControlTarget.Enemy(EvidencePieceRole("queen")), RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (Black, "a1", RelationControlTarget.Enemy(EvidencePieceRole("rook")), RelationRayPattern.XRay, RelationBlockerRemovalMode.Moved)
      )
    )
    assert(witnesses.forall(_.lineMoves == List("a2b3")))

  test("all typed witnesses survive before downstream display selection"):
    val openedLines = relations(
      "3q3k/8/5r2/8/3N4/2B5/8/3R2K1 w - - 0 1",
      List("d4e2")
    )
    assertEquals(
      accessSignatures(openedLines),
      Set(
        (White, "d5", RelationControlTarget.Empty, RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (White, "d6", RelationControlTarget.Empty, RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (White, "d7", RelationControlTarget.Empty, RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (White, "d8", RelationControlTarget.Enemy(EvidencePieceRole("queen")), RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (White, "e5", RelationControlTarget.Empty, RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (White, "f6", RelationControlTarget.Enemy(EvidencePieceRole("rook")), RelationRayPattern.Ordinary, RelationBlockerRemovalMode.Moved),
        (Black, "d3", RelationControlTarget.Empty, RelationRayPattern.XRay, RelationBlockerRemovalMode.Moved),
        (Black, "d2", RelationControlTarget.Empty, RelationRayPattern.XRay, RelationBlockerRemovalMode.Moved),
        (Black, "d1", RelationControlTarget.Enemy(EvidencePieceRole("rook")), RelationRayPattern.XRay, RelationBlockerRemovalMode.Moved)
      )
    )

    assertEquals(RelationFactKind.fromId("slider_control_interference"), Some(RelationFactKind.SliderControlInterference))
    assertEquals(RelationFactKind.fromId("defender_trade"), None)

  test("an enemy-control bundle requires both a new check edge and another enemy-control edge"):
    val geometricMultiAttack = relations(
      "7k/1r1r4/8/8/4N3/8/8/7K w - - 0 1",
      List("e4c5")
    )
    assertEquals(targets(geometricMultiAttack, RelationFactKind.CheckingEnemyControlBundle), Set.empty)

    val checkingFork = relations(
      "4k3/7r/8/8/8/8/8/3QK3 w - - 0 1",
      List("d1h5")
    )
    assertEquals(targets(checkingFork, RelationFactKind.CheckingEnemyControlBundle), Set("e8", "h7"))
    assertEquals(
      checkingFork.filter(_.kind == RelationFactKind.CheckingEnemyControlBundle).flatMap(_.focusSquares.map(_.key)).toSet,
      Set("d1", "e8", "h5", "h7")
    )

    val discovered = relations(
      "2q1k3/8/8/8/8/8/4B3/4R1K1 w - - 0 1",
      List("e2g4")
    ).filter(_.kind == RelationFactKind.CheckingEnemyControlBundle)
    assertEquals(discovered.size, 1)
    assert(discovered.head.detail match
      case RelationWitnessDetail.CheckingEnemyControlBundle(_, kingControls, otherControls, _) =>
        kingControls.exists(control =>
          control.controllerSquare.key == "e1" && control.targetSquare.key == "e8"
        ) && otherControls.exists(control =>
          control.controllerSquare.key == "g4" && control.targetSquare.key == "c8"
        )
      case _ => false
    )

    val pin = relations(
      "4k3/8/2n5/8/2B5/8/8/4K3 w - - 0 1",
      List("c4b5")
    )
    val pinWitnesses = pin.filter(rayPattern(_).contains(RelationRayPattern.Pin))
    assertEquals(pinWitnesses.flatMap(_.targetSquares.map(_.key)).toSet, Set("e8"))
    assertEquals(pinWitnesses.flatMap(_.lineMoves), List("c4b5"))

  test("promotion uses the exact after-role when it gives check and attacks another piece"):
    val promotion = relations(
      "4k3/6P1/6r1/8/8/8/8/K7 w - - 0 1",
      List("g7g8q")
    ).filter(_.kind == RelationFactKind.CheckingEnemyControlBundle)

    assertEquals(targets(promotion, RelationFactKind.CheckingEnemyControlBundle), Set("e8", "g6"))
    assert(promotion.exists(_.detail match
      case RelationWitnessDetail.CheckingEnemyControlBundle(mover, kingControls, otherControls, _) =>
        mover.side == White &&
          mover.from.key == "g7" &&
          mover.to.key == "g8" &&
          mover.beforeRole.name.equalsIgnoreCase("pawn") &&
          mover.afterRole.name.equalsIgnoreCase("queen") &&
          (kingControls ++ otherControls).forall(control =>
            control.controllerSquare.key == "g8" && control.controllerRole.name.equalsIgnoreCase("queen")
          )
      case _ => false
    ))

  test("a promotion capture keeps one exact Pawn-to-Queen root across combined proofs"):
    val combined = relations(
      "rk6/nP6/8/8/8/8/8/1R2K3 w - - 0 1",
      List("b7a8q")
    ).filter(relation =>
      Set(
        RelationFactKind.GeometricSupporterCapture,
        RelationFactKind.CheckingEnemyControlBundle,
        RelationFactKind.DoubleCheck
      )(relation.kind)
    )

    assertEquals(
      combined.map(_.kind).toSet,
      Set(
        RelationFactKind.GeometricSupporterCapture,
        RelationFactKind.CheckingEnemyControlBundle,
        RelationFactKind.DoubleCheck
      )
    )
    assert(combined.forall(relation => rootMover(relation.detail).exists(mover =>
      mover.side == White &&
        mover.from.key == "b7" && mover.to.key == "a8" &&
        mover.beforeRole.name.equalsIgnoreCase("pawn") &&
        mover.afterRole.name.equalsIgnoreCase("queen")
    )))

  test("promotion interference uses the promoted role at the destination"):
    val interference = relations(
      "k3r2r/6P1/8/8/8/8/8/K7 w - - 0 1",
      List("g7g8q")
    ).find(_.kind == RelationFactKind.SliderControlInterference)
      .getOrElse(fail("expected promotion interference"))

    assert(interference.detail match
      case RelationWitnessDetail.SliderControlInterference(mover, _, controller, _, supported, _, _) =>
        mover.from.key == "g7" && mover.to.key == "g8" &&
          mover.beforeRole.name.equalsIgnoreCase("pawn") &&
          mover.afterRole.name.equalsIgnoreCase("queen") &&
          Set(controller.key, supported.key) == Set("e8", "h8")
      case _ => false
    )

  test("a same-side interposer preserves the exact slider support that it blocks"):
    val positionRelations = relations(
      "3r1r1k/1p4pp/p2pbp2/4n3/q2BPR2/2PB2Q1/P1P3PP/R6K b - - 7 24",
      List("e6f7")
    )
    val interference = positionRelations.find(_.kind == RelationFactKind.SliderControlInterference)
      .getOrElse(fail("expected same-side slider support interference"))

    assert(interference.detail match
      case RelationWitnessDetail.SliderControlInterference(
            mover,
            controllerSide,
            controller,
            _,
            supported,
            _,
            _
          ) =>
        mover.side == Black && mover.from.key == "e6" && mover.to.key == "f7" &&
          controllerSide == Black && controller.key == "f8" && supported.key == "f6"
      case _ => false
    )
    assert(positionRelations.exists(_.detail match
      case RelationWitnessDetail.GeometricSupportDelta(
            _,
            Black,
            supportedBefore,
            _,
            supportedAfter,
            _,
            _,
            _,
            _,
            established,
            _
          ) =>
        supportedBefore.key == "e6" && supportedAfter.key == "f7" &&
          established.exists(value =>
            value.square.key == "f8" && value.role.name.equalsIgnoreCase("rook")
          )
      case _ => false
    ))

  test("slider interference covers empty-square control and enemy attack with one contract"):
    val interferences = relations(
      "r6k/8/8/8/8/8/8/RN5K w - - 0 1",
      List("b1a3")
    ).flatMap(_.detail match
      case RelationWitnessDetail.SliderControlInterference(
            interposer,
            controllerSide,
            controller,
            controllerRole,
            target,
            targetState,
            _
          ) =>
        List((interposer, controllerSide, controller, controllerRole, target, targetState))
      case _ => Nil
    )

    assert(interferences.exists { case (interposer, side, controller, role, target, targetState) =>
      interposer.from.key == "b1" && interposer.to.key == "a3" && side == Black &&
        controller.key == "a8" && role.name.equalsIgnoreCase("rook") && target.key == "a2" &&
        targetState == RelationControlTarget.Empty
    })
    assert(interferences.exists { case (_, side, controller, role, target, targetState) =>
      side == Black && controller.key == "a8" && role.name.equalsIgnoreCase("rook") &&
        target.key == "a1" && targetState == RelationControlTarget.Enemy(EvidencePieceRole("rook"))
    })

  test("castling keeps the king clearance and rook attack as separate exact changes"):
    val castling = relations(
      "5k2/8/8/8/8/8/8/n3K2R w K - 0 1",
      List("e1g1")
    )

    assert(castling.exists(_.detail match
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
            mover,
            White,
            attackerBefore,
            attackerAfter,
            attackerRole,
            _,
            _,
            target,
            _,
            _,
            RelationBlockerRemovalMode.Moved,
            _
          ) =>
        mover.from.key == "e1" && mover.to.key == "g1" &&
          mover.beforeRole.name.equalsIgnoreCase("king") &&
          attackerBefore.key == "h1" && attackerAfter.key == "f1" &&
          attackerRole.name.equalsIgnoreCase("rook") && target.key == "a1"
      case _ => false
    ))
    assert(castling.exists(_.detail match
      case RelationWitnessDetail.CheckingEnemyControlBundle(mover, kingControls, otherControls, _) =>
        mover.from.key == "e1" && mover.to.key == "g1" &&
          kingControls.exists(control =>
            control.controllerSquare.key == "f1" &&
              control.controllerRole.name.equalsIgnoreCase("rook") &&
              control.targetSquare.key == "f8"
          ) && otherControls.exists(control => control.targetSquare.key == "a1")
      case _ => false
    ))

  test("castling does not relabel maintained rook-to-king support as newly established"):
    val fen = "4k3/8/8/8/8/8/8/4K2R w K - 0 1"
    val move = "e1g1"
    val legalReplay = PrincipalVariationEvidence
      .legalMoveReplay(fen, List(move), startPly = 0)
      .getOrElse(fail("expected legal castling replay"))
    val replay = CanonicalLineReplay
      .fromLegalReplay(legalReplay)
      .getOrElse(fail("expected canonical castling replay"))
    val transition = replay.onlyTransition.getOrElse(fail("expected one castling transition"))
    val beforeRef = PositionNodeRef(fen, 0, Some(transition.legal.before.color))
    val afterFen = chess.format.Fen.write(transition.legal.after).value
    val afterRef = PositionNodeRef(afterFen, 1, Some(transition.legal.after.color))
    def snapshot(
        analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
        position: PositionNodeRef
    ): CanonicalPositionRelationSnapshot =
      TypedEvidenceGraph.empty
        .addAll(PositionRelationExtractor.records(
          analysis.boardRelations,
          position,
          EvidenceScope.CurrentPosition,
          relation => s"castling:${position.ply}:${relation.semanticId}"
        ))
        .relationGraph
        .closedPositionRelationSnapshot(position, EvidenceScope.CurrentPosition, analysis.relationInventory)
    val before = snapshot(transition.beforeAnalysis, beforeRef)
    val after = snapshot(transition.afterAnalysis, afterRef)
    val canonicalDelta = CanonicalRelationDelta.bind(
      transition.declared,
      transition.relationDelta,
      before,
      after
    )
    val bound = TacticalRelationEvidence
      .relationProduction(replay, move)
      .bindClosedOutput(before, after, canonicalDelta)

    assert(!bound.relations.exists(_.detail match
      case RelationWitnessDetail.GeometricSupportDelta(
            _,
            White,
            supportedBefore,
            _,
            supportedAfter,
            _,
            _,
            _,
            removed,
            established,
            _
          ) =>
        supportedBefore.key == "e1" && supportedAfter.key == "g1" &&
          (removed ++ established).exists(value =>
            Set("h1", "f1")(value.square.key) && value.role.name.equalsIgnoreCase("rook")
          )
      case _ => false
    ))
  test("en passant can expose a stationary slider without reassigning the attack to the pawn"):
    val enPassant = relations(
      "4k3/8/2r5/3pP3/4Q3/8/8/K7 w - d6 0 1",
      List("e5d6")
    ).filter(_.kind == RelationFactKind.CheckingEnemyControlBundle)

    assertEquals(targets(enPassant, RelationFactKind.CheckingEnemyControlBundle), Set("e8", "c6"))
    assert(enPassant.exists(_.detail match
      case RelationWitnessDetail.CheckingEnemyControlBundle(mover, kingControls, otherControls, _) =>
        mover.from.key == "e5" && mover.to.key == "d6" &&
          mover.beforeRole.name.equalsIgnoreCase("pawn") &&
          mover.afterRole.name.equalsIgnoreCase("pawn") &&
          (kingControls ++ otherControls).forall(control =>
            control.controllerSquare.key == "e4" && control.controllerRole.name.equalsIgnoreCase("queen")
          )
      case _ => false
    ))

  test("promotion clearance and en-passant capture use their distinct changed cells"):
    val promotion = relations(
      "2r4k/1P6/B7/8/8/8/8/4K3 w - - 0 1",
      List("b7b8q")
    )
    assert(promotion.exists(_.detail match
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
            mover,
            White,
            _,
            _,
            _,
            blocker,
            _,
            target,
            RelationControlTarget.Enemy(role),
            _,
            RelationBlockerRemovalMode.Moved,
            _
          ) =>
        mover.from.key == "b7" && mover.to.key == "b8" &&
          mover.beforeRole.name.equalsIgnoreCase("pawn") &&
          mover.afterRole.name.equalsIgnoreCase("queen") &&
          blocker.key == "b7" && target.key == "c8" && role.name.equalsIgnoreCase("rook")
      case _ => false
    ))
    assert(promotion.exists(relation => relation.detail match
      case RelationWitnessDetail.RayBarrier(White, attacker, attackerRole, occupants, axis) =>
        attacker.key == "b8" && occupants.head.square.key == "c8" && occupants.lift(1).exists(_.square.key == "h8") &&
          attackerRole.name.equalsIgnoreCase("queen") && occupants.head.role.name.equalsIgnoreCase("rook") &&
          occupants.lift(1).exists(_.role.name.equalsIgnoreCase("king")) &&
          RelationRayPattern.classify(White, occupants, axis) == RelationRayPattern.Pin
      case _ => false
    ))

    val enPassant = relations(
      "8/5k2/4n3/3pP3/4n3/8/B7/4K3 w - d6 0 1",
      List("e5d6")
    )
    assert(enPassant.exists(_.kind == RelationFactKind.GeometricSupporterCapture))
    assert(enPassant.exists(_.detail match
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
            mover,
            White,
            _,
            _,
            _,
            blocker,
            _,
            target,
            RelationControlTarget.Enemy(role),
            _,
            RelationBlockerRemovalMode.Captured,
            _
          ) =>
        mover.from.key == "e5" && mover.to.key == "d6" &&
          blocker.key == "d5" && target.key == "e6" && role.name.equalsIgnoreCase("knight")
      case _ => false
    ))
    assert(enPassant.exists(relation => relation.detail match
      case RelationWitnessDetail.RayBarrier(White, attacker, _, occupants, axis) =>
        attacker.key == "a2" && occupants.head.square.key == "e6" && occupants.lift(1).exists(_.square.key == "f7") &&
          RelationRayPattern.classify(White, occupants, axis) == RelationRayPattern.Pin
      case _ => false
    ))

    val selfExposure = relations(
      "8/5k2/4N3/3pP3/4n3/8/b7/4K3 w - d6 0 1",
      List("e5d6")
    )
    assert(selfExposure.exists(_.detail match
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
            mover,
            Black,
            _,
            _,
            _,
            blocker,
            _,
            target,
            RelationControlTarget.Enemy(role),
            _,
            RelationBlockerRemovalMode.Captured,
            _
          ) =>
        mover.side == White && mover.from.key == "e5" && mover.to.key == "d6" &&
          blocker.key == "d5" && target.key == "e6" && role.name.equalsIgnoreCase("knight")
      case _ => false
    ))

  test("an unchanged distant pin is not projected onto an unrelated king move"):
    val unchanged = relations(
      "k7/n7/8/8/8/8/8/R3K3 w - - 0 1",
      List("e1f1")
    )
    assert(!unchanged.exists(rayPattern(_).contains(RelationRayPattern.Pin)))

  test("an unlined after-position ray projection cannot enter the before-position inventory"):
    val fen = "4k3/8/2n5/8/2B5/8/8/4K3 w - - 0 1"
    val result = production(fen, List("c4b5"))
    val projected = result.relations.find(relation =>
      relation.kind == RelationFactKind.RayBarrier && relation.hasLineProof
    ).getOrElse(fail("expected one legal-replay ray projection"))
    val before = PositionNodeRef(fen, 0, Some(White), Some("ray-projection-before"))
    val record = RelationFactEvidence.record(
      id = "unlined-ray-projection",
      payload = projected,
      position = before,
      line = None,
      scope = EvidenceScope.CurrentPosition,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
    val graph = TypedEvidenceGraph.empty.add(record).relationGraph

    assertEquals(graph.at(before).map(_.record), List(record))
    assertEquals(graph.positionRelationsAt(before), Nil)

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

    assert(castlingPin.exists(relation => relation.detail match
      case RelationWitnessDetail.RayBarrier(White, attacker, _, occupants, axis) =>
        attacker.key == "f1" && occupants.head.square.key == "f7" && occupants.lift(1).exists(_.square.key == "f8") &&
          RelationRayPattern.classify(White, occupants, axis) == RelationRayPattern.Pin
      case _ => false
    ))
    assert(discoveredPin.exists(relation => relation.detail match
      case RelationWitnessDetail.RayBarrier(White, attacker, _, occupants, axis) =>
        attacker.key == "a1" && occupants.head.square.key == "a7" && occupants.lift(1).exists(_.square.key == "a8") &&
          RelationRayPattern.classify(White, occupants, axis) == RelationRayPattern.Pin
      case _ => false
    ))

  test("a maintained non-king control is not relabeled as newly established beside double check"):
    val combined = relations(
      "4k3/8/r7/8/8/8/4B3/4R1K1 w - - 0 1",
      List("e2b5")
    ).filter(relation =>
      Set(RelationFactKind.DoubleCheck, RelationFactKind.CheckingEnemyControlBundle)(relation.kind)
    )

    assertEquals(
      combined.map(_.kind).toSet,
      Set(RelationFactKind.DoubleCheck)
    )
    assertEquals(combined.size, 1)
    assert(combined.forall(_.lineMoves == List("e2b5")))
    assert(combined.forall(_.detail match
      case RelationWitnessDetail.DoubleCheck(_, _, _, proof) => proof.premises.size >= 3
      case _ => false
    ))
