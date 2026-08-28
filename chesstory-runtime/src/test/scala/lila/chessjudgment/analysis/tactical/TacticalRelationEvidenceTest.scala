package lila.chessjudgment.analysis.tactical

import chess.{ Black, Color, White }
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.relation.ClosedRelationEvidence
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
  RelationChangeDirection,
  RelationCheckResponseMode,
  RelationControlReachWitness,
  RelationControlTarget,
  RelationCombinationContractKind,
  RelationFactEvidence,
  RelationFactKind,
  RelationRayPattern,
  RelationMoveTransitionWitness,
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
            openedTargets,
            barrierPattern,
            removalMode,
            _
          ) =>
        openedTargets.map(target =>
          (controllerSide, target.square.key, target.target, barrierPattern, removalMode)
        )
      case _ => Nil
    ).toSet

  private def rootMover(detail: RelationWitnessDetail): Option[RelationMoveTransitionWitness] =
    detail match
      case RelationWitnessDetail.GeometricSupporterCapture(mover, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.GeometricControlSetDelta(mover, _, _, _, _, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.GeometricSupportDelta(mover, _, _, _, _, _, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.SliderLineInterruption(mover, _, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(mover, _, _, _, _, _, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.NamedRayTransition(mover, _, _, _, _, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.CaptureRecaptureInventory(mover, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.CreatedCheckResponseInventory(mover, _, _, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.RootCheckResponse(mover, _, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.PawnTopologyTransition(mover, _, _, _, _) => Some(mover)
      case RelationWitnessDetail.StalemateTransition(mover, _, _, _) => Some(mover)
      case _ => None

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

    assertEquals(witnesses.size, 2, "two opened rays must not expand into one result per controlled square")
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

  test("a promotion capture keeps one exact Pawn-to-Queen root across source and combined proofs"):
    val combined = relations(
      "rk6/nP6/8/8/8/8/8/1R2K3 w - - 0 1",
      List("b7a8q")
    ).filter(relation =>
      relation.kind == RelationFactKind.GeometricSupporterCapture ||
        hasCreatedDoubleCheck(relation)
    )

    assertEquals(
      combined.map(_.kind).toSet,
      Set(
        RelationFactKind.GeometricSupporterCapture,
        RelationFactKind.GeometricControlSetDelta
      )
    )
    assert(combined.forall(relation => rootMover(relation.detail).exists(mover =>
      mover.side == White &&
        mover.from.key == "b7" && mover.to.key == "a8" &&
        mover.beforeRole.name.equalsIgnoreCase("pawn") &&
        mover.afterRole.name.equalsIgnoreCase("queen")
    )))

  test("promotion line interruption uses the promoted role at the destination"):
    val interruption = relations(
      "k3r2r/6P1/8/8/8/8/8/K7 w - - 0 1",
      List("g7g8q")
    ).find(_.detail match
      case RelationWitnessDetail.SliderLineInterruption(_, _, controllerBefore, controllerAfter, _, targets, _) =>
        controllerBefore.key == "e8" && controllerAfter.key == "e8" &&
          targets.map(_.square.key).toSet == Set("h8")
      case _ => false
    )
      .getOrElse(fail("expected promotion line interruption"))

    assert(interruption.detail match
      case RelationWitnessDetail.SliderLineInterruption(mover, _, controllerBefore, controllerAfter, _, targets, _) =>
        mover.from.key == "g7" && mover.to.key == "g8" &&
          mover.beforeRole.name.equalsIgnoreCase("pawn") &&
          mover.afterRole.name.equalsIgnoreCase("queen") &&
          controllerBefore.key == "e8" && controllerAfter.key == "e8" &&
          targets.map(_.square.key).toSet == Set("h8")
      case _ => false
    )

  test("a same-side interposer preserves the exact slider support that it blocks"):
    val positionRelations = relations(
      "3r1r1k/1p4pp/p2pbp2/4n3/q2BPR2/2PB2Q1/P1P3PP/R6K b - - 7 24",
      List("e6f7")
    )
    val interruption = positionRelations.find(_.kind == RelationFactKind.SliderLineInterruption)
      .getOrElse(fail("expected same-side slider support line interruption"))

    assert(interruption.detail match
      case RelationWitnessDetail.SliderLineInterruption(
            mover,
            controllerSide,
            controllerBefore,
            controllerAfter,
            _,
            targets,
            _,
          ) =>
        mover.side == Black && mover.from.key == "e6" && mover.to.key == "f7" &&
          controllerSide == Black && controllerBefore.key == "f8" && controllerAfter.key == "f8" &&
          targets.exists(_.square.key == "f6")
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

  test("slider line interruption covers empty-square control and enemy attack without semantic overclaim"):
    val interruptions = relations(
      "r6k/8/8/8/8/8/8/RN5K w - - 0 1",
      List("b1a3")
    ).flatMap(_.detail match
      case RelationWitnessDetail.SliderLineInterruption(
            interposer,
            controllerSide,
            controllerBefore,
            controllerAfter,
            controllerRole,
            targets,
            _
          ) =>
        List((interposer, controllerSide, controllerBefore, controllerAfter, controllerRole, targets))
      case _ => Nil
    )

    assert(interruptions.exists { case (interposer, side, controllerBefore, controllerAfter, role, targets) =>
      interposer.from.key == "b1" && interposer.to.key == "a3" && side == Black &&
        controllerBefore.key == "a8" && controllerAfter.key == "a8" &&
        role.name.equalsIgnoreCase("rook") &&
        targets.toSet == Set(
          RelationControlReachWitness(EvidenceSquare("a2"), RelationControlTarget.Empty),
          RelationControlReachWitness(
            EvidenceSquare("a1"),
            RelationControlTarget.Enemy(EvidencePieceRole("rook"))
          )
        )
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
            openedTargets,
            _,
            RelationBlockerRemovalMode.Moved,
            _
          ) =>
        mover.from.key == "e1" && mover.to.key == "g1" &&
          mover.beforeRole.name.equalsIgnoreCase("king") &&
          attackerBefore.key == "h1" && attackerAfter.key == "f1" &&
          attackerRole.name.equalsIgnoreCase("rook") && openedTargets.exists(_.square.key == "a1")
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
    val bound = ClosedRelationEvidence
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
            openedTargets,
            _,
            RelationBlockerRemovalMode.Moved,
            _
          ) =>
        mover.from.key == "b7" && mover.to.key == "b8" &&
          mover.beforeRole.name.equalsIgnoreCase("pawn") &&
          mover.afterRole.name.equalsIgnoreCase("queen") &&
          blocker.key == "b7" && openedTargets.exists(reach =>
            reach.square.key == "c8" && (reach.target match
              case RelationControlTarget.Enemy(role) => role.name.equalsIgnoreCase("rook")
              case _                                 => false
            )
          )
      case _ => false
    ))
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
            openedTargets,
            _,
            RelationBlockerRemovalMode.Captured,
            _
          ) =>
        mover.from.key == "e5" && mover.to.key == "d6" &&
          blocker.key == "d5" && openedTargets.exists(reach =>
            reach.square.key == "e6" && (reach.target match
              case RelationControlTarget.Enemy(role) => role.name.equalsIgnoreCase("knight")
              case _                                 => false
            )
          )
      case _ => false
    ))
    assert(enPassant.exists(relation => relation.detail match
      case RelationWitnessDetail.NamedRayTransition(_, White, attacker, _, barrier, target, _, pattern, direction, _) =>
        attacker.key == "a2" && barrier.square.key == "e6" && target.exists(_.square.key == "f7") &&
          pattern == RelationRayPattern.AbsoluteKingPin && direction == RelationChangeDirection.Established
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
            openedTargets,
            _,
            RelationBlockerRemovalMode.Captured,
            _
          ) =>
        mover.side == White && mover.from.key == "e5" && mover.to.key == "d6" &&
          blocker.key == "d5" && openedTargets.exists(reach =>
            reach.square.key == "e6" && (reach.target match
              case RelationControlTarget.Enemy(role) => role.name.equalsIgnoreCase("knight")
              case _                                 => false
            )
          )
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
