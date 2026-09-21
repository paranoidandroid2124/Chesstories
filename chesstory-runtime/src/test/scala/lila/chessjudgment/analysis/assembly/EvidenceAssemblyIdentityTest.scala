package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.line.EngineLine

class EvidenceAssemblyIdentityTest extends munit.FunSuite:

  private val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  private val position = PositionNodeRef(fen, 0, Some(White), Some("identity-root"))
  private val line = LineNodeRef("identity-line", "e2e4")

  private def hasCreatedDoubleCheck(relation: RelationFactEvidence): Boolean =
    relation.detail match
      case RelationWitnessDetail.CreatedCheckResponseInventory(_, _, _, checkers, _, _, _, _) =>
        checkers.size >= 2
      case _ => false

  test("nested capture-order branches share their prefix but retain distinct transposed proof paths"):
    // Doknjas, Najdorf Game 18, PDF page 157: after 13.Nf3.
    val rootFen = "rn2nrk1/2qbbppp/p2pp2B/1p6/4P3/1BN2NQ1/PPP2PPP/3R1RK1 b - - 11 13"
    val prefix = List("b8c6", "h6f4", "b5b4", "e4e5")
    val movesA = prefix ++ List("c6e5", "f3e5", "d6e5", "f4e5", "c7b7")
    val movesB = prefix ++ List("d6e5", "f3e5", "c6e5", "f4e5", "c7b7")
    def assemble(moves: List[List[String]]) =
      MoveReviewInputAdmission.admit(RawMoveReviewInput(
        rootFen, prefix.head, moves.map(line => EngineLine(line, scoreCp = 0, depth = 20))
      )).map(MoveReviewAssembler.assemble).getOrElse(fail("expected both legal nested branches"))
    val assembled = assemble(List(movesA, movesB))
    def path(moves: List[String]): List[MoveTransitionEdge] =
      val line = assembled.lineReplays.find(_._2.replaySteps.map(_.moveUci) == moves).get._1
      assembled.lineTransitionIds(line).map(id => assembled.transitions.find(_.evidence.id == id).get)
    val a = path(movesA)
    val b = path(movesB)
    assertEquals(a.take(prefix.size), b.take(prefix.size))
    assertEquals(assembled.transitions.size, movesA.size + movesB.size - prefix.size)
    assertEquals(assembled.positions.size, assembled.transitions.size + 1)
    assertEquals(a.last.to.fen, b.last.to.fen)
    assertNotEquals(a.last.to, b.last.to)
    assertNotEquals(a.last.evidence.id, b.last.evidence.id)
    def capturedByBishop(edges: List[MoveTransitionEdge]): String =
      val bishop = edges.find(_.moveUci == "f4e5").get
      assembled.evidenceGraph.record(bishop.evidence).get.payload match
        case move: MoveTransitionEvidence => move.canonicalTransitionProof.get.rootMovement.capture.get.capturedRole.name
        case _ => fail("expected the bishop capture")
    assertEquals(capturedByBishop(a), "pawn")
    assertEquals(capturedByBishop(b), "knight")
    a.zip(b).take(prefix.size).foreach { (left, right) =>
      assert(assembled.transitionReplay(left).get eq assembled.transitionReplay(right).get)
    }
    val reversed = assemble(List(movesB, movesA))
    assertEquals(assembled.transitions.map(_.evidence.id).toSet, reversed.transitions.map(_.evidence.id).toSet)
    // The observations and complete lines may disappear without invalidating
    // independently certified occurrences or their lower facts.
    val independent = assembled.evidenceGraph.records.filterNot(record =>
      Set(EvidenceLayer.Line, EvidenceLayer.Eval, EvidenceLayer.CandidateComparison, EvidenceLayer.RelativeAssessment)(record.ref.layer)
    )
    val withoutLines = TypedEvidenceGraph.empty.addAll(independent)
    assert(independent.nonEmpty)
    assert(independent.forall(withoutLines.proofEligible))

  test("long legal lines keep bounded identifiers without dropping predecessor ownership"):
    val moves = List(
      "e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5a4", "g8f6",
      "e1g1", "f8e7", "f1e1", "b7b5", "a4b3", "d7d6", "c2c3", "e8g8",
      "h2h3", "c6b8", "d2d4", "b8d7", "b1d2", "c7c5"
    )
    val admitted = MoveReviewInputAdmission.admit(
      RawMoveReviewInput(fen, moves.head, List(EngineLine(moves, scoreCp = 0, depth = 20)))
    ).getOrElse(fail("expected a legal long line"))
    val allocator = JudgmentProvenanceAllocator.forInput(admitted)
    val root = allocator.positionRef(PositionNodeRole.Before, fen, 0, Some(White))
    val legalLine = admitted.legalLines.head
    val owner = allocator.lineRef(legalLine, allocator.lineOccurrenceKey(legalLine, root))
    assert(owner.id.length <= 512)
    var predecessor = root
    val positions = legalLine.replay.replaySteps.zipWithIndex.map { (step, offset) =>
      val side = if step.ply % 2 == 0 then White else Black
      val next = allocator.successorPositionRef(predecessor, step, side)
      // Check before building another generation so a regression cannot exhaust memory.
      assert(next.id.exists(_.length <= 512))
      val transitionKey = allocator.transitionOccurrenceKey(predecessor, step.moveUci, next)
      assert(allocator.evidenceId(s"transition:$transitionKey").length <= 512)
      if offset > 0 then
        val otherPath = predecessor.copy(id = Some("other-predecessor"))
        assertNotEquals(
          next,
          allocator.successorPositionRef(otherPath, step, side)
        )
      predecessor = next
      next
    }
    assertEquals(positions.size, moves.size)
    assertEquals(positions.distinct.size, moves.size)

  test("line and transition identities include their exact board occurrence"):
    val afterE4 = PrincipalVariationEvidence
      .legalFenAfter(fen, "e2e4")
      .getOrElse(fail("expected e4"))
    val afterD4 = PrincipalVariationEvidence
      .legalFenAfter(fen, "d2d4")
      .getOrElse(fail("expected d4"))
    val rootA = PositionNodeRef(afterE4, 1, Some(Black), Some("occurrence-root-a"))
    val rootB = PositionNodeRef(afterD4, 1, Some(Black), Some("occurrence-root-b"))
    val move = "g8f6"
    def normalized(root: PositionNodeRef): AdmittedLegalLine =
      val history = CanonicalPositionHistory
        .from(root.fen, Nil, root.fen)
        .getOrElse(fail("expected an occurrence history"))
      val extended = history.extend(List(move)).getOrElse(fail("expected a legal occurrence line"))
      val replay = CanonicalLineReplay
        .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
        .getOrElse(fail("expected a canonical occurrence replay"))
      AdmittedLegalLine(replay)
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    val lineA = normalized(rootA)
    val lineB = normalized(rootB)
    val lineKeyA = allocator.lineOccurrenceKey(lineA, rootA)
    val lineKeyB = allocator.lineOccurrenceKey(lineB, rootB)
    val refA = allocator.lineRef(lineA, lineKeyA)
    val refB = allocator.lineRef(lineB, lineKeyB)

    assertNotEquals(lineKeyA, lineKeyB)
    assertNotEquals(refA, refB)
    assertEquals(
      allocator.lineRef(lineA, allocator.lineOccurrenceKey(lineA, rootA)),
      refA
    )

    val toA = PositionNodeRef(
      PrincipalVariationEvidence.legalFenAfter(rootA.fen, move).getOrElse(fail("expected Nf6 after e4")),
      2,
      Some(White),
      Some("occurrence-to-a")
    )
    val toB = PositionNodeRef(
      PrincipalVariationEvidence.legalFenAfter(rootB.fen, move).getOrElse(fail("expected Nf6 after d4")),
      2,
      Some(White),
      Some("occurrence-to-b")
    )
    assertNotEquals(
      allocator.transitionOccurrenceKey(rootA, move, toA),
      allocator.transitionOccurrenceKey(rootB, move, toB)
    )

  test("a repeated move through a cycle keeps distinct graph occurrences"):
    val cycle = MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = "r2qrbk1/1bpn1p1p/p2p1np1/Pp2p3/3PP3/2P2NNP/1PB2PP1/R1BQR1K1 w - - 1 17",
          playedMoveUci = "f3h2",
          variations = List(
            EngineLine(List("f3h2", "f6h5", "h2f3", "h5f6", "f3h2"), scoreCp = 0, depth = 20)
          )
        )
      )
      .map(MoveReviewAssembler.assemble)
      .getOrElse(fail("expected the repeated cycle to assemble"))
    val repeated = cycle.transitions.filter(edge => EvidenceRef.sameMove(edge.moveUci, "f3h2"))

    assertEquals(repeated.size, 2)
    val List(first, second) = repeated.sortBy(_.to.ply): @unchecked
    assert(PrincipalVariationEvidence.sameBoardState(first.from.fen, second.from.fen))
    assert(PrincipalVariationEvidence.sameBoardState(first.to.fen, second.to.fen))
    assertNotEquals(first.from, second.from)
    assertNotEquals(first.to, second.to)
    assertNotEquals(first.evidence.id, second.evidence.id)
    val occurrenceIds = List(first.from, second.from).map { position =>
      cycle.evidenceGraph.records.collect {
        case EvidenceRecord(ref, _: PositionOccurrenceEvidence, _) if ref.position == position => ref.id
      } match
        case id :: Nil => id
        case ids       => fail(s"expected one position occurrence for ${position.id}, found ${ids.size}")
    }
    assertEquals(occurrenceIds.distinct.size, 2)

  test("transposed histories keep distinct owners for the same following move"):
    val rootFen = "4k3/6pp/8/8/8/8/PP2B3/4R1K1 w - - 0 1"
    val lineA = List("a2a3", "h7h6", "b2b3", "g7g6", "e2d3")
    val lineB = List("b2b3", "g7g6", "a2a3", "h7h6", "e2d3")
    val transposed = MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = lineA.head,
          variations = List(
            EngineLine(lineA, scoreCp = 0, depth = 20),
            EngineLine(lineB, scoreCp = 20, depth = 20)
          )
        )
      )
      .map(MoveReviewAssembler.assemble)
      .getOrElse(fail("expected both transposed histories to assemble"))
    val finalEdges = transposed.transitions
      .filter(edge => EvidenceRef.sameMove(edge.moveUci, "e2d3"))
      .sortBy(_.evidence.line.map(_.id).getOrElse(""))

    val replayPlyCount = transposed.lineReplays.valuesIterator.map(_.replaySteps.size).sum
    assertEquals(transposed.transitions.size, replayPlyCount)
    assertEquals(transposed.positions.size, replayPlyCount + 1)
    assertEquals(finalEdges.size, 2)
    val List(finalA, finalB) = finalEdges: @unchecked
    assert(PrincipalVariationEvidence.sameBoardState(finalA.from.fen, finalB.from.fen))
    assert(PrincipalVariationEvidence.sameBoardState(finalA.to.fen, finalB.to.fen))
    assertNotEquals(finalA.from, finalB.from)
    assertNotEquals(finalA.to, finalB.to)
    assertNotEquals(finalA.evidence.id, finalB.evidence.id)
    def destinationOccurrence(edge: MoveTransitionEdge): EvidenceRecord =
      transposed.evidenceGraph.records.collect {
        case record @ EvidenceRecord(ref, _: PositionOccurrenceEvidence, _)
            if ref.position == edge.to => record
      } match
        case record :: Nil => record
        case records       => fail(s"expected one destination occurrence, found ${records.size}")
    val occurrenceA = destinationOccurrence(finalA)
    val occurrenceB = destinationOccurrence(finalB)
    assertEquals(occurrenceA.parents, List(finalA.evidence))
    assertEquals(occurrenceB.parents, List(finalB.evidence))

    def outputsFor(edge: MoveTransitionEdge): List[EvidenceRecord] =
      transposed.evidenceGraph.records.collect {
        case record @ EvidenceRecord(_, relation: RelationFactEvidence, parents)
            if relation.proofStage != RelationProofStage.PositionFact &&
              parents.flatMap(transposed.evidenceGraph.record).exists {
                case EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) =>
                  occurrence.edge.evidence.id == edge.evidence.id
                case _ => false
              } =>
          record
      }
    val outputsA = outputsFor(finalA)
    val outputsB = outputsFor(finalB)
    val semanticsA = outputsA.collect { case EvidenceRecord(_, relation: RelationFactEvidence, _) => relation.semanticId }
    val semanticsB = outputsB.collect { case EvidenceRecord(_, relation: RelationFactEvidence, _) => relation.semanticId }

    assert(semanticsA.nonEmpty)
    assertEquals(semanticsA.toSet, semanticsB.toSet)
    assertEquals((outputsA ++ outputsB).map(_.ref.id).distinct.size, outputsA.size + outputsB.size)

  test("fresh double-check control stays on one transition occurrence"):
    val assembled = MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = "4k3/8/r7/8/8/8/4B3/4R1K1 w - - 0 1",
          playedMoveUci = "e2d3",
          variations = List(
            EngineLine(List("e2b5"), scoreCp = 100, depth = 20),
            EngineLine(List("e2d3", "e8f7"), scoreCp = 0, depth = 20)
          )
        )
      )
      .map(MoveReviewAssembler.assemble)
      .getOrElse(fail("expected the double-check review to assemble"))
    val freshChecks = assembled.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if hasCreatedDoubleCheck(relation) => record
    }

    assert(freshChecks.nonEmpty)
    freshChecks.foreach { record =>
      record.payload match
        case relation: RelationFactEvidence => relation.detail match
          case RelationWitnessDetail.CreatedCheckResponseInventory(_, Black, kingSquare, checkers, _, _, _, _) =>
            assertEquals(kingSquare.key, "e8")
            assertEquals(checkers.map(checker => checker.square.key -> checker.role.name.toLowerCase).toSet,
              Set("b5" -> "bishop", "e1" -> "rook"))
          case _ => fail("expected both exact checkers on the black king")
        case _ => fail("expected the selected double-check relation")
      val carriers = record.parents.flatMap(assembled.evidenceGraph.record).collect {
        case EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) => occurrence
      }
      assertEquals(carriers.size, 1)
      assert(assembled.evidenceGraph.proofEligible(record))
    }
    val responseInventories = assembled.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.CreatedCheckResponseInventory => record
    }
    assert(responseInventories.nonEmpty)
    assert(responseInventories.forall(assembled.evidenceGraph.proofEligible))

    val removed = freshChecks.head
    val carrierRecord = removed.parents.flatMap(assembled.evidenceGraph.record).collect {
      case record @ EvidenceRecord(_, _: ClosedRelationOccurrenceEvidence, _) => record
    } match
      case carrier :: Nil => carrier
      case carriers       => fail(s"expected one closed relation occurrence, found ${carriers.size}")
    val carrierOutputs = assembled.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, parents)
          if parents.contains(carrierRecord.ref) => record -> relation
    }
    val lower = carrierOutputs.collectFirst {
      case (record, relation) if relation.proofStage == RelationProofStage.TransitionFact => record
    }.getOrElse(fail("expected a transition fact on the double-check occurrence"))
    assert(!assembled.evidenceGraph.parentClosure(lower).exists(_.ref == removed.ref))

    val withoutDerived = TypedEvidenceGraph.empty.addAll(
      assembled.evidenceGraph.records.filterNot(_.ref == removed.ref)
    )
    val independentSiblings = carrierOutputs
      .map(_._1)
      .filterNot(_.ref == removed.ref)
      .flatMap(record => withoutDerived.record(record.ref))
      .filterNot(_.parents.contains(removed.ref))

    assert(independentSiblings.exists(_.ref == lower.ref))
    assert(withoutDerived.proofEligible(withoutDerived.record(carrierRecord.ref).getOrElse(
      fail("expected the transition occurrence to survive removal of one output")
    )))
    assert(independentSiblings.forall(withoutDerived.proofEligible))

  test("a root check response binds the transition carrier and chosen legal response once"):
    val assembled = MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = "k3r3/8/8/8/8/3B4/8/4K3 w - - 0 1",
          playedMoveUci = "d3e2",
          variations = List(
            EngineLine(List("e1d2"), scoreCp = 20, depth = 20),
            EngineLine(List("d3e2", "e8e2"), scoreCp = 0, depth = 20)
          )
        )
      )
      .map(MoveReviewAssembler.assemble)
      .getOrElse(fail("expected the checked root response to assemble"))
    val response = assembled.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.RootCheckResponse => record
    }.getOrElse(fail("expected the root response relation"))
    val carriers = response.parents.flatMap(assembled.evidenceGraph.record).collect {
      case EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) => occurrence
    }

    assertEquals(carriers.size, 1)
    assertEquals(response.parents.distinct.size, response.parents.size)
    assert(assembled.evidenceGraph.proofEligible(response))

  test("a root-and-absence stalemate proof materializes without a fabricated positive source"):
    val assembled = MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = "k7/2Q5/2K5/8/8/8/8/8 w - - 0 1",
          playedMoveUci = "c7b6",
          variations = List(
            EngineLine(List("c7b6"), scoreCp = 0, depth = 20)
          )
        )
      )
      .map(MoveReviewAssembler.assemble)
      .getOrElse(fail("expected the stalemate transition to assemble"))
    val stalemate = assembled.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.StalemateTransition => record
    }.getOrElse(fail("expected the closed stalemate relation"))
    val parents = stalemate.parents.flatMap(assembled.evidenceGraph.record)

    assertEquals(parents.count(_.payload.isInstanceOf[ClosedRelationOccurrenceEvidence]), 1)
    assertEquals(parents.count(_.payload.isInstanceOf[RelationFactEvidence]), 0)
    assert(assembled.evidenceGraph.proofEligible(stalemate))

  private def input: AdmittedMoveReviewInput =
    MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = fen,
          playedMoveUci = line.rootMove,
          variations = List(
            EngineLine(List("e2e4", "e7e5"), scoreCp = 20, depth = 18),
            EngineLine(List("d2d4", "d7d5"), scoreCp = 10, depth = 18)
          )
        )
      )
      .getOrElse(fail("expected a canonical allocator input"))
