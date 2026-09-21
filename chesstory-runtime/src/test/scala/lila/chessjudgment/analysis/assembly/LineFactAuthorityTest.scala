package lila.chessjudgment.analysis.assembly

import chess.Black

import lila.chessjudgment.analysis.line.LineFactNormalizer
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

class LineFactAuthorityTest extends munit.FunSuite:
  test("repeated promotion UCI retains both exact observed occurrences"):
    val fen = "8/P7/P6k/8/8/8/8/7K w - - 0 1"
    val payload = normalizedLine(fen, Nil, fen,
      List("a7a8q", "h6h5", "a8b8", "h5h4", "a6a7", "h4h3", "a7a8q"))
    assertEquals(payload.lineEventsOf(LineEventKind.Promotion).map(e => e.plyOffset -> e.moveUci),
      List(0 -> "a7a8q", 6 -> "a7a8q"))
    assertEquals(payload.material.get.events.map(_.promotionGainCp), List(800, 800))
    assertEquals(payload.material.get.runningBalancesForMover, List(800, 1600))

  test("repeated capture UCI retains both material events without a lasting-gain projection"):
    val payload = normalizedLine("7k/8/5KQ1/8/1r6/8/r7/R7 b - - 0 1", List("b4b3"),
      "7k/8/5KQ1/8/8/1r6/r7/R7 w - - 1 2",
      List("a1a2", "b3b2", "a2a1", "b2a2", "a1a2"))
    assertEquals(payload.materialCaptures.map(c => c.plyOffset -> c.moveUci),
      List(0 -> "a1a2", 4 -> "a1a2"))
    assertEquals(payload.material.get.runningBalancesForMover, List(500, 1000))

  test("observed recovery includes zero and a promotion-only recovery without requiring game termination"):
    val zeroFen = "7k/2p2Kp1/3P3P/8/1Q6/8/8/8 w - - 0 1"
    val zero = normalizedLine(zeroFen, Nil, zeroFen,
      List("b4c5", "c7d6", "c5d6", "g7h6", "d6h6")).material.get
    assertEquals(zero.runningBalancesForMover, List(-100, 0, -100, 0))
    assert(zero.hasObservedRecovery)
    val promotionFen = "7k/Pp3Kp1/7P/8/5Q2/8/8/8 w - - 0 1"
    val promotion = normalizedLine(promotionFen, Nil, promotionFen,
      List("f4g5", "g7h6", "a7a8n", "b7b6", "g5h6")).material.get
    assertEquals(promotion.runningBalancesForMover, List(-100, 100, 200))
    assertEquals(promotion.events.find(_.plyOffset == 2).get.capture, None)
    assert(promotion.hasObservedRecovery)

  test("queen-for-pawn loss remains an observed capture and recapture sequence"):
    val payload = normalizedLine("3r4/3p3k/8/8/8/8/8/3Q3K b - - 0 1", List("h7h8"),
      "3r3k/3p4/8/8/8/8/8/3Q3K w - - 1 2", List("d1d7", "d8d7"))
    assertEquals(payload.materialCaptures.map(c =>
      (c.moveUci, c.plyOffset, c.attackerRole.name, c.capturedRole.name, c.recapture)),
      List(("d1d7", 0, "queen", "pawn", false), ("d8d7", 1, "rook", "queen", true)))
    assertEquals(payload.material.get.runningBalancesForMover, List(100, -800))
    assertEquals(payload.lineEventsOf(LineEventKind.Recapture).map(e => e.plyOffset -> e.moveUci),
      List(1 -> "d8d7"))

  test("an immediate reply check uses the canonical event while a completed quiet material scan is zero"):
    val fen = "1r5k/8/8/8/8/8/8/K6N w - - 0 1"
    val payload = normalizedLine(fen, Nil, fen, List("h1f2", "b8b1"))
    val event = payload.lineEventsOf(LineEventKind.Check).head
    assertEquals((event.moveUci, event.plyOffset, event.side), ("b8b1", 1, Some(Black)))
    assertEquals(payload.material.get.events, Nil)
    assertEquals(payload.material.get.observedNetCpForMover, 0)

  test("a different replay's ledger fails the existing graph boundary while None remains uncomputed"):
    val initial = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
    val a = List("e2e4", "d7d5", "e4d5")
    val b = List("d2d4", "e7e5", "d4e5")
    val ctx = MoveReviewAssembler.assembleOccurrences(MoveReviewInputAdmission.admitIndependent(history,
      a.head, suppliedLines = List(a, b).map(SuppliedCommentaryLine(CommentaryLineSource.Analysis, _))).get)
    def factFor(moves: List[String]) = ctx.legalLines.iterator.flatMap(line =>
      ctx.evidenceGraph.uniqueProofEligibleLineFactRecordFor(line.ref)).find(_._2.lineReplayMoves == moves).get
    val (record, fact) = factFor(a)
    val other = factFor(b)._2.material
    val detached = record.copy(payload = fact.copy(material = other))
    val wrongGraph = TypedEvidenceGraph.empty.addAll(ctx.evidenceGraph.records.map(value =>
      if value.ref == record.ref then detached else value))
    assert(!wrongGraph.proofEligible(detached))
    assertEquals(wrongGraph.uniqueProofEligibleLineFactRecordFor(fact.line), None)
    intercept[IllegalArgumentException] {
      LineFactEvidence.fromCertifiedReplay(fact.line, fact.certifiedReplay.get, other)
    }
    val uncomputed = record.copy(payload = fact.copy(material = None))
    val uncomputedGraph = TypedEvidenceGraph.empty.addAll(ctx.evidenceGraph.records.map(value =>
      if value.ref == record.ref then uncomputed else value))
    assert(uncomputedGraph.proofEligible(uncomputed))
    assertEquals(uncomputed.payload.asInstanceOf[LineFactEvidence].material, None)
    assert(fact.material.get.events eq fact.material.get.events)
    assert(fact.lineEvents eq fact.lineEvents)

  test("a ledger rejects a mismatched root history and unrelated predecessor without replaying a board"):
    val initial = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val root = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
    val afterE4 = root.extend(List("e2e4")).toOption.get
    val black = CanonicalLineReplay.fromHistory(afterE4.extend(List("d7d5")).toOption.get.segmentReplaySteps.drop(1)).get
    intercept[IllegalArgumentException] { LineMaterialSummary(black, root, None) }
    val unrelated = CanonicalLineReplay.fromHistory(root.extend(List("d2d4")).toOption.get.segmentReplaySteps).get
    intercept[IllegalArgumentException] { LineMaterialSummary(black, afterE4, Some(unrelated)) }
    assertEquals(LegalLineAssembler.lineMaterialSummary(afterE4, root.extend(List("d2d4", "d7d5")).toOption.get), None)

  private def normalizedLine(initialFen: String, historyMoves: List[String], rootFen: String,
      moves: List[String]): LineFactEvidence =
    val history = CanonicalPositionHistory.from(initialFen, historyMoves, rootFen).toOption.get
    val end = history.extend(moves).toOption.get
    val replay = CanonicalLineReplay.fromHistory(end.segmentReplaySteps.drop(history.segmentReplaySteps.size)).get
    val predecessor = CanonicalLineReplay.fromHistory(history.segmentReplaySteps.takeRight(1))
    val material = LineMaterialSummary(replay, history, predecessor)
    LineFactNormalizer.fromValidatedLine("line-test-evidence", LineNodeRef("line-test", moves.head), replay,
      PositionNodeRef(rootFen, history.currentPly, Some(material.sideToMove)), EvidenceScope.LegalLine,
      Some(material)).payload.asInstanceOf[LineFactEvidence]
