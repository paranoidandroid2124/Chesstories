package lila.chessjudgment.model.judgment

import chess.Color
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.position.PositionAnalyzer

class RelationFactEvidenceLawTest extends munit.FunSuite:

  private val a1 = EvidenceSquare("a1")
  private val b2 = EvidenceSquare("b2")
  private val c3 = EvidenceSquare("c3")
  private val d4 = EvidenceSquare("d4")
  private val e5 = EvidenceSquare("e5")
  private val king = EvidencePieceRole("king")
  private val queen = EvidencePieceRole("queen")
  private val rook = EvidencePieceRole("rook")
  private val bishop = EvidencePieceRole("bishop")
  private val knight = EvidencePieceRole("knight")

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

  private def premise(
      occurrence: RelationPremiseOccurrence,
      kind: RelationFactKind,
      digit: Char
  ): RelationCombinationPremise =
    RelationCombinationPremise(occurrence, kind, digit.toString * 64)

  private val legalAndRemovedControl = RelationCombinationProof(
    RelationCombinationContractKind.GeometricSupporterCapture,
    List(
      premise(RelationPremiseOccurrence.Before, RelationFactKind.LegalMove, '1'),
      premise(RelationPremiseOccurrence.Removed, RelationFactKind.GeometricControl, '2')
    ).sortBy(_.stableKey),
    List(RelationProofKey.ChangedSquare(d4))
  )
  private val controlSetDeltaProof = RelationCombinationProof(
    RelationCombinationContractKind.GeometricControlSetDelta,
    List(
      premise(RelationPremiseOccurrence.Before, RelationFactKind.LegalMove, 'f'),
      premise(RelationPremiseOccurrence.Established, RelationFactKind.GeometricControl, '0')
    ).sortBy(_.stableKey),
    List(RelationProofKey.ChangedSquare(d4))
  )
  private val supportDeltaProof = RelationCombinationProof(
    RelationCombinationContractKind.GeometricSupportDelta,
    List(
      premise(RelationPremiseOccurrence.Before, RelationFactKind.LegalMove, 'd'),
      premise(RelationPremiseOccurrence.Established, RelationFactKind.GeometricControl, 'e')
    ).sortBy(_.stableKey),
    List(RelationProofKey.ChangedSquare(e5))
  )
  private val interferenceProof = RelationCombinationProof(
    RelationCombinationContractKind.SliderControlInterference,
    (
      legalAndRemovedControl.premises :+
        premise(RelationPremiseOccurrence.Established, RelationFactKind.RayBarrier, '3')
    ).sortBy(_.stableKey),
    List(RelationProofKey.ChangedSquare(c3))
  )
  private val blockerProof = RelationCombinationProof(
    RelationCombinationContractKind.GeometricLineControlAfterBlockerRemoval,
    List(
      premise(RelationPremiseOccurrence.Before, RelationFactKind.LegalMove, '4'),
      premise(RelationPremiseOccurrence.Removed, RelationFactKind.RayBarrier, '5'),
      premise(RelationPremiseOccurrence.Established, RelationFactKind.GeometricControl, '6')
    ).sortBy(_.stableKey),
    List(RelationProofKey.ChangedSquare(b2))
  )
  private val checkingBundleProof = RelationCombinationProof(
    RelationCombinationContractKind.CheckingEnemyControlBundle,
    List(
      premise(RelationPremiseOccurrence.Before, RelationFactKind.LegalMove, '7'),
      premise(RelationPremiseOccurrence.Established, RelationFactKind.GeometricControl, '8'),
      premise(RelationPremiseOccurrence.Established, RelationFactKind.GeometricControl, '9')
    ).sortBy(_.stableKey),
    List(RelationProofKey.ChangedSquare(c3))
  )
  private val doubleCheckProof = RelationCombinationProof(
    RelationCombinationContractKind.DoubleCheck,
    List(
      premise(RelationPremiseOccurrence.Before, RelationFactKind.LegalMove, 'a'),
      premise(RelationPremiseOccurrence.Established, RelationFactKind.GeometricControl, 'b'),
      premise(RelationPremiseOccurrence.Established, RelationFactKind.GeometricControl, 'c')
    ).sortBy(_.stableKey),
    List(RelationProofKey.ChangedSquare(c3))
  )

  private def doubleCheck(checkers: List[RelationPieceWitness]): RelationWitnessDetail =
    RelationWitnessDetail.DoubleCheck(
      move(e5, c3, knight),
      d4,
      checkers,
      doubleCheckProof
    )

  private val canonicalCases: List[(RelationFactKind, RelationWitnessDetail)] = List(
    RelationFactKind.GeometricControl -> RelationWitnessDetail.GeometricControl(
      Color.White,
      a1,
      rook,
      d4,
      RelationControlTarget.Enemy(queen)
    ),
    RelationFactKind.LegalMove -> RelationWitnessDetail.LegalMove(
      Color.White,
      a1,
      rook,
      d4,
      "a1d4",
      Some(RelationLegalCaptureWitness(d4, queen, Color.Black))
    ),
    RelationFactKind.GeometricControlSetDelta -> RelationWitnessDetail.GeometricControlSetDelta(
      move(a1, c3, rook),
      Color.White,
      d4,
      RelationControlTarget.Empty,
      RelationControlTarget.Empty,
      Nil,
      List(RelationPieceWitness(c3, rook)),
      Nil,
      List(RelationPieceWitness(c3, rook)),
      controlSetDeltaProof
    ),
    RelationFactKind.GeometricSupporterCapture -> RelationWitnessDetail.GeometricSupporterCapture(
      move(a1, d4, rook), d4, queen, e5, bishop, legalAndRemovedControl
    ),
    RelationFactKind.GeometricSupportDelta -> RelationWitnessDetail.GeometricSupportDelta(
      move(a1, c3, rook),
      Color.White,
      d4,
      bishop,
      d4,
      bishop,
      Nil,
      List(RelationPieceWitness(c3, rook)),
      Nil,
      List(RelationPieceWitness(c3, rook)),
      supportDeltaProof
    ),
    RelationFactKind.SliderControlInterference -> RelationWitnessDetail.SliderControlInterference(
      move(a1, c3, rook), Color.White, b2, bishop, d4, RelationControlTarget.Enemy(queen), interferenceProof
    ),
    RelationFactKind.GeometricLineControlAfterBlockerRemoval -> RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
      move(a1, b2, rook),
      Color.White,
      a1,
      b2,
      rook,
      b2,
      bishop,
      d4,
      RelationControlTarget.Enemy(queen),
      RelationRayPattern.XRay,
      RelationBlockerRemovalMode.Captured,
      blockerProof
    ),
    RelationFactKind.CheckingEnemyControlBundle -> RelationWitnessDetail.CheckingEnemyControlBundle(
      move(a1, c3, knight),
      List(RelationEnemyControlWitness(c3, knight, d4, king)),
      List(RelationEnemyControlWitness(c3, knight, e5, queen)),
      checkingBundleProof
    ),
    RelationFactKind.PawnFileGroup -> RelationWitnessDetail.PawnFileGroup(
      Color.White,
      EvidenceFile("d"),
      List(d4)
    ),
    RelationFactKind.PawnTension -> RelationWitnessDetail.PawnTension(d4, e5),
    RelationFactKind.PawnFrontOccupancy -> RelationWitnessDetail.PawnFrontOccupancy(
      Color.White,
      d4,
      Some(d4.copy(key = "d5")),
      Some(RelationColoredPieceWitness(d4.copy(key = "d5"), knight, Color.Black))
    ),
    RelationFactKind.PawnPassage -> RelationWitnessDetail.PawnPassage(Color.White, d4, List(e5)),
    RelationFactKind.MajorPieceFileOccupancy -> RelationWitnessDetail.MajorPieceFileOccupancy(
      Color.White,
      EvidenceFile("d"),
      List(RelationPieceWitness(d4, rook)),
      open = true
    ),
    RelationFactKind.DoubleCheck -> doubleCheck(
      List(RelationPieceWitness(a1, rook), RelationPieceWitness(b2, bishop))
    ),
    RelationFactKind.RayBarrier -> RelationWitnessDetail.RayBarrier(
      Color.White,
      a1,
      bishop,
      List(
        RelationColoredPieceWitness(b2, knight, Color.Black),
        RelationColoredPieceWitness(d4, king, Color.Black)
      ),
      RelationAxisSignal.Diagonal
    )
  )

  test("every relation kind has one total typed mapping and a stable id round trip"):
    assertEquals(canonicalCases.map(_._1).toSet, RelationFactKind.values.toSet)
    canonicalCases.foreach { case (expectedKind, detail) =>
      val payload = RelationFactEvidence.from(detail, List("a1a8"))
      assertEquals(payload.kind, expectedKind)
      assertEquals(RelationFactKind.fromId(RelationFactKind.id(expectedKind)), Some(expectedKind))
      assert(payload.hasConcreteWitness, clues(expectedKind, payload.participants, payload.focusSquares, payload.files))
    }

  test("verification requires a canonical producer certificate bound to the exact board"):
    val emptyPosition = PositionNodeRef("4k3/8/8/8/8/8/8/4K3 w - - 0 1", 0, Some(Color.White))
    val unverifiedStatic = RelationFactEvidence.from(
      RelationWitnessDetail.MajorPieceFileOccupancy(
        Color.White,
        EvidenceFile("d"),
        List(RelationPieceWitness(d4, rook)),
        open = true
      ),
      Nil
    )
    val unverifiedTactical = RelationFactEvidence.from(
      doubleCheck(List(RelationPieceWitness(a1, rook), RelationPieceWitness(b2, bishop))),
      List("a1d1")
    )
    val emptyBoardRef = EvidenceRef(
      "relation:position:file-access",
      EvidenceProducer.RelationProducer,
      EvidenceLayer.Relation,
      emptyPosition,
      None,
      EvidenceScope.CurrentPosition,
      EvidenceConfidence.BoardDerived
    )
    val replayRef = emptyBoardRef.copy(
      id = "relation:line:double-check",
      scope = EvidenceScope.BestLine,
      confidence = EvidenceConfidence.LegalReplayVerified
    )

    assert(!RelationFactEvidence.verified(emptyBoardRef, unverifiedStatic))
    assert(!RelationFactEvidence.verified(replayRef, unverifiedTactical))

    val rookFen = "4k3/8/8/8/8/8/8/3RK3 w - - 0 1"
    val rookPosition = Fen.read(Standard, Fen.Full(rookFen)).get
    val certified = PositionAnalyzer
      .analyze(rookPosition, rookFen, plyCount = 0)
      .boardRelations
      .find(_.kind == RelationFactKind.MajorPieceFileOccupancy)
      .getOrElse(fail("expected a certified major-piece file occupancy"))
    val certifiedRef = emptyBoardRef.copy(
      id = s"relation:position:${certified.semanticId}",
      position = PositionNodeRef(rookFen, 0, Some(Color.White))
    )
    assert(RelationFactEvidence.verified(certifiedRef, certified))
    assert(!RelationFactEvidence.verified(emptyBoardRef, certified))
    assert(!RelationFactEvidence.verified(certifiedRef.copy(confidence = EvidenceConfidence.Mixed), certified))

  test("semantic relation ids are independent of detector ordering"):
    val detail = doubleCheck(List(RelationPieceWitness(a1, rook), RelationPieceWitness(b2, bishop)))
    val first = RelationFactEvidence.from(detail, List("a1b3", "e8e7"))
    val second = RelationFactEvidence.from(detail, List("a1b3", "e8e7"))
    assertEquals(first.semanticId, second.semanticId)

    val reversedTargets = RelationFactEvidence.from(
      doubleCheck(List(RelationPieceWitness(b2, bishop), RelationPieceWitness(a1, rook))),
      List("a1b3", "e8e7")
    )
    assertEquals(first.semanticId, reversedTargets.semanticId)

  test("semantic relation ids preserve the exact order and multiplicity of a proof line"):
    val detail = doubleCheck(List(RelationPieceWitness(a1, rook), RelationPieceWitness(b2, bishop)))
    val forward = RelationFactEvidence.from(detail, List("a1b3", "e8e7", "b3a1"))
    val reversed = RelationFactEvidence.from(detail, List("b3a1", "e8e7", "a1b3"))
    val shortened = RelationFactEvidence.from(detail, List("a1b3", "e8e7"))

    assertNotEquals(forward.semanticId, reversed.semanticId)
    assertNotEquals(forward.semanticId, shortened.semanticId)

  test("the canonical graph owns relation shape, occurrence identity, and deduplication"):
    val rookFen = "4k3/8/8/8/8/8/8/3RK3 w - - 0 1"
    val position = Fen.read(Standard, Fen.Full(rookFen)).get
    val positionRef = PositionNodeRef(rookFen, 0, Some(Color.White), Some("first-id"))
    val relation = PositionAnalyzer
      .analyze(position, rookFen, plyCount = 0)
      .boardRelations
      .find(_.kind == RelationFactKind.MajorPieceFileOccupancy)
      .getOrElse(fail("expected a certified major-piece file occupancy"))
    val first = RelationFactEvidence.record(
      "canonical-relation-1",
      relation,
      positionRef,
      None,
      EvidenceScope.CurrentPosition,
      EvidenceConfidence.BoardDerived
    )
    val graph = TypedEvidenceGraph.empty.add(first)
    val differentRuleOccurrence = PositionNodeRef(
      "4k3/8/8/8/8/8/8/3RK3 w - - 17 42",
      0,
      Some(Color.White),
      Some("first-id")
    )
    val differentOccurrence = positionRef.copy(id = Some("different-allocation-id"))

    assertEquals(graph.relationGraph.at(positionRef).map(_.record), List(first))
    assertEquals(graph.relationGraph.at(differentRuleOccurrence), Nil)
    assertEquals(graph.relationGraph.at(differentOccurrence), Nil)
    assert(graph.relationGraph.touching(EvidenceSquare("d1")).exists(_.record == first))

    val duplicate = first.copy(ref = first.ref.copy(id = "canonical-relation-2"))
    intercept[IllegalArgumentException](graph.add(duplicate))

    val wrongLayer = first.copy(
      ref = first.ref.copy(
        id = "wrong-relation-layer",
        producer = EvidenceProducer.TacticalMechanismProducer,
        layer = EvidenceLayer.TacticalMechanism
      )
    )
    intercept[IllegalArgumentException](TypedEvidenceGraph.empty.add(wrongLayer))

  test("an intrinsically invalid proof source cannot regain authority through a typed child"):
    val beforeFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val afterFen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
    val before = PositionNodeRef(beforeFen, 0, Some(Color.White))
    val after = PositionNodeRef(afterFen, 1, Some(Color.Black))
    val invalidRestriction = EvidenceRecord(
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
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.OpponentMobilityRestriction,
            strength = 1,
            subjectBindings = List(
              StructuralSubjectBinding.unbound(StructuralSubject.OpponentResourceDeterred(
                EvidencePieceRole("pawn"),
                EvidenceSquare("a7"),
                EvidenceSquare("a6")
              ))
            )
          )
        ),
        relationChanges = Nil,
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
      MoveTransitionEvidence("e2e4", before, after),
      parents = List(invalidRestriction.ref)
    )
    val graph = TypedEvidenceGraph.empty.add(invalidRestriction).add(typedChild)
    val orphan = typedChild.copy(
      ref = typedChild.ref.copy(id = "missing-parent-child"),
      parents = List(invalidRestriction.ref.copy(id = "missing-parent"))
    )
    val orphanGraph = TypedEvidenceGraph.empty.add(orphan)

    assert(!graph.proofEligible(invalidRestriction))
    assert(!graph.proofEligible(typedChild))
    assert(!orphanGraph.proofEligible(orphan))
