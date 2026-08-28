package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.relation.ClosedRelationEvidence
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class RelationFactEvidenceLawTest extends munit.FunSuite:

  private val a1 = EvidenceSquare("a1")
  private val b2 = EvidenceSquare("b2")
  private val c3 = EvidenceSquare("c3")
  private val d4 = EvidenceSquare("d4")
  private val rook = EvidencePieceRole("rook")
  private val bishop = EvidencePieceRole("bishop")
  private val knight = EvidencePieceRole("knight")
  private val pawn = EvidencePieceRole("pawn")

  private def move(
      from: EvidenceSquare,
      to: EvidenceSquare,
      role: EvidencePieceRole
  ): RelationMoveTransitionWitness =
    RelationMoveTransitionWitness(
      Color.White,
      from,
      to,
      role,
      role
    )

  private def verticalPremise(
      role: VerticalRelationPremiseRole,
      kind: RelationFactKind,
      digit: Char
  ): VerticalRelationPremise =
    VerticalRelationPremise(
      role,
      VerticalRelationPremiseSource.Derived,
      RelationProofStage.TransitionFact,
      kind,
      digit.toString * 64,
      digit.toUpper.toString * 64
    )

  private val supportChangeProof = VerticalRelationDerivationProof.fromPremises(
    VerticalRelationContractKind.GeometricSupportCausalTransition,
    List(
      verticalPremise(
        VerticalRelationPremiseRole.SupportSetDelta(
          Color.White,
          RelationPieceWitness(d4, bishop),
          RelationPieceWitness(d4, bishop),
          List(RelationPieceWitness(b2, rook)),
          Nil,
          List(RelationPieceWitness(b2, rook)),
          Nil
        ),
        RelationFactKind.GeometricSupportDelta,
        '7'
      ),
      verticalPremise(
        VerticalRelationPremiseRole.SupportRemovalCause(
          RelationPieceWitness(d4, bishop),
          RelationPieceWitness(b2, rook),
          RelationSupportChangeCause.SupporterCaptured
        ),
        RelationFactKind.GeometricSupporterCapture,
        '8'
      )
    ).sortBy(_.stableKey),
    List(
      ClosedRelationAbsencePremise(
        RelationSnapshotOccurrence.After,
        PositionRelationExtractor.ClosedRelationAbsenceQuery.GeometricFriendlySupportOf(Color.White, d4)
      )
    ),
    Nil
  )
  private def producedRelations(fen: String, moveUci: String): List[RelationFactEvidence] =
    val legalReplay = PrincipalVariationEvidence
      .legalMoveReplay(fen, List(moveUci), startPly = 0)
      .getOrElse(throw new AssertionError(s"expected legal mapping fixture: $fen $moveUci"))
    val replay = CanonicalLineReplay
      .fromLegalReplay(legalReplay)
      .getOrElse(throw new AssertionError(s"expected canonical mapping fixture: $fen $moveUci"))
    ClosedRelationEvidence.relationProduction(replay, moveUci).relations

  test("one assertion preserves distinct exact derivation paths"):
    def supportChange(
        proof: VerticalRelationDerivationProof,
        lineMoves: List[String] = List("a1c3")
    ): RelationFactEvidence =
      RelationFactEvidence.from(
        RelationWitnessDetail.GeometricSupportCausalTransition(
          move(a1, c3, knight),
          Color.White,
          d4,
          bishop,
          d4,
          bishop,
          List(RelationPieceWitness(b2, rook)),
          Nil,
          List(RelationSupportRemovalWitness(
            RelationPieceWitness(b2, rook),
            List(RelationSupportChangeCause.SupporterCaptured)
          )),
          Nil,
          proof
        ),
        lineMoves
      )
    val alternateProof = VerticalRelationDerivationProof.fromPremises(
      VerticalRelationContractKind.GeometricSupportCausalTransition,
      List(
        verticalPremise(
          VerticalRelationPremiseRole.SupportSetDelta(
            Color.White,
            RelationPieceWitness(d4, bishop),
            RelationPieceWitness(d4, bishop),
            List(RelationPieceWitness(b2, rook)),
            Nil,
            List(RelationPieceWitness(b2, rook)),
            Nil
          ),
          RelationFactKind.GeometricSupportDelta,
          '1'
        ),
        verticalPremise(
          VerticalRelationPremiseRole.SupportRemovalCause(
            RelationPieceWitness(d4, bishop),
            RelationPieceWitness(b2, rook),
            RelationSupportChangeCause.SupporterCaptured
          ),
          RelationFactKind.GeometricSupporterCapture,
          '2'
        )
      ).sortBy(_.stableKey),
      supportChangeProof.absences,
      Nil
    )
    val first = supportChange(supportChangeProof)
    val second = supportChange(alternateProof)
    val continuationProof = supportChange(supportChangeProof, List("a1c3", "e8e7"))
    assertEquals(first.assertionId, second.assertionId)
    assertEquals(first.assertionId, continuationProof.assertionId)
    assertNotEquals(first.semanticId, second.semanticId)
    assertNotEquals(first.semanticId, continuationProof.semanticId)

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

  test("semantic relation ids preserve the exact order and multiplicity of a proof line"):
    val detail = RelationWitnessDetail.GeometricSupportCausalTransition(
      move(a1, c3, knight),
      Color.White,
      d4,
      bishop,
      d4,
      bishop,
      List(RelationPieceWitness(b2, rook)),
      Nil,
      List(RelationSupportRemovalWitness(
        RelationPieceWitness(b2, rook),
        List(RelationSupportChangeCause.SupporterCaptured)
      )),
      Nil,
      supportChangeProof
    )
    val forward = RelationFactEvidence.from(detail, List("a1b3", "e8e7", "b3a1"))
    val reversed = RelationFactEvidence.from(detail, List("b3a1", "e8e7", "a1b3"))
    val shortened = RelationFactEvidence.from(detail, List("a1b3", "e8e7"))

    assertNotEquals(forward.semanticId, reversed.semanticId)
    assertNotEquals(forward.semanticId, shortened.semanticId)

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
