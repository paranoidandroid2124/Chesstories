package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.analysis.relation.ClosedRelationEvidence
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class RelationFactEvidenceLawTest extends munit.FunSuite:

  private val knight = EvidencePieceRole("knight")
  private val pawn = EvidencePieceRole("pawn")

  private def producedRelations(fen: String, moveUci: String): List[RelationFactEvidence] =
    val legalReplay = PrincipalVariationEvidence
      .legalMoveReplay(fen, List(moveUci), startPly = 0)
      .getOrElse(throw new AssertionError(s"expected legal mapping fixture: $fen $moveUci"))
    val replay = CanonicalLineReplay
      .fromLegalReplay(legalReplay)
      .getOrElse(throw new AssertionError(s"expected canonical mapping fixture: $fen $moveUci"))
    ClosedRelationEvidence.relationProduction(replay, moveUci).relations

  test("legal response production and witness validation use the canonical knight promotion suffix"):
    val producedCheckResponses = producedRelations(
      "7K/8/8/8/8/8/R2p4/7k w - - 0 1",
      "a2a1"
    ).filter(_.kind == RelationFactKind.CreatedCheckResponseInventory)
    assertEquals(producedCheckResponses.size, 1)
    val producedResponses = producedCheckResponses.head.detail match
      case RelationWitnessDetail.CreatedCheckResponseInventory(_, _, _, _, responses, _, _, _) => responses
      case _ => fail("expected check responses with a promotion interposition")
    val producedKnightPromotion = producedResponses
      .find(_.resource.moveUci == "d2d1n")
      .getOrElse(fail("expected the exact knight-promotion interposition"))
    assertEquals(producedKnightPromotion.resource.movement.beforeRole, pawn)
    assertEquals(producedKnightPromotion.resource.movement.afterRole, knight)
    assert(producedKnightPromotion.modes.contains(RelationCheckResponseMode.Interpose))

    val promotion = RelationMoveTransitionWitness(
      Color.White,
      EvidenceSquare("a7"),
      EvidenceSquare("a8"),
      pawn,
      knight
    )
    intercept[IllegalArgumentException](
      RelationLegalMoveResourceWitness(promotion, "a7a8k", None)
    )

  test("an intrinsically invalid proof source cannot regain authority through a typed child"):
    val beforeFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val afterFen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
    val before = PositionNodeRef(beforeFen, 0, Some(Color.White))
    val after = PositionNodeRef(afterFen, 1, Some(Color.Black))
    val invalidStructural = EvidenceRecord(
      EvidenceRef(
        "invalid-structural-parent",
        EvidenceProducer.StructuralDeltaProducer,
        EvidenceLayer.StructuralDelta,
        before,
        None,
        EvidenceScope.PlayedTransition,
        EvidenceConfidence.BoardDerived
      ),
      StructuralDeltaEvidence(
        StructuralTransitionBinding(
          "e2e4",
          TransitionEdgeRole.Played,
          before,
          after,
          None,
          Color.White
        ),
        signals = Nil,
        consequences = Nil,
        relationChanges = Nil,
        derivedRelationSources = Nil,
        canonicalTransitionProof = None,
        canonicalDeltaProof = None
      )
    )
    val typedChild = EvidenceRecord(
      EvidenceRef(
        "typed-child",
        EvidenceProducer.MoveTransitionProducer,
        EvidenceLayer.MoveTransition,
        before,
        None,
        EvidenceScope.PlayedTransition,
        EvidenceConfidence.BoardDerived
      ),
      MoveTransitionEvidence("e2e4", before, after, canonicalTransitionProof = None),
      parents = List(invalidStructural.ref)
    )
    val graph = TypedEvidenceGraph.empty.add(invalidStructural).add(typedChild)
    val orphan = typedChild.copy(
      ref = typedChild.ref.copy(id = "missing-parent-child"),
      parents = List(invalidStructural.ref.copy(id = "missing-parent"))
    )
    val orphanGraph = TypedEvidenceGraph.empty.add(orphan)
    val forgedTransition = typedChild.copy(
      ref = typedChild.ref.copy(
        id = "forged-legal-transition",
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      parents = Nil
    )
    val forgedGraph = TypedEvidenceGraph.empty.add(forgedTransition)

    assert(!graph.proofEligible(invalidStructural))
    assert(!graph.proofEligible(typedChild))
    assert(!orphanGraph.proofEligible(orphan))
    assert(!forgedGraph.proofEligible(forgedTransition))
