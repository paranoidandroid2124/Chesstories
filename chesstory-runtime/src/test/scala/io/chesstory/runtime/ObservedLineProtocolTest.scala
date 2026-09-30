package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{ MoveReviewAssembler, MoveReviewInputAdmission }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

class ObservedLineProtocolTest extends munit.FunSuite:
  private val initial = IndependentCommentaryExamples.initial
  private val exchange = List("e2e4", "d7d5", "e4d5", "d8d5")
  private val recovery = List("e2e4", "d7d5", "b1c3", "d5e4", "c3e4")

  private def context(fen: String, lines: List[List[String]]): JudgmentAssemblyContext =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, lines.head.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Analysis, _))).get
    MoveReviewAssembler.assembleOccurrences(input)

  private def lineFor(ctx: JudgmentAssemblyContext, moves: List[String]): LegalLineNode =
    ctx.legalLines.find(line => ctx.evidenceGraph.record(line.evidence).exists(_.payload match
      case fact: LineFactEvidence => fact.lineReplayMoves == moves
      case _ => false)).get

  private def summaryFor(ctx: JudgmentAssemblyContext, moves: List[String]): JsObject =
    val line = lineFor(ctx, moves)
    val result = ObservedLineProtocol.summaryJson(ctx, line).get
    val ids = ctx.lineTransitionIds(line.ref)
    val events = (result \ "events").as[List[JsObject]] ++
      (result \ "material" \ "events").asOpt[List[JsObject]].getOrElse(Nil)
    events.foreach { event =>
      val offset = (event \ "ply_offset").as[Int]
      assertEquals((event \ "occurrence_id").as[String], ids(offset))
      assertEquals((event \ "move_uci").as[String], moves(offset))
    }
    assertEquals((result \ "premise_ids").as[List[String]], List(line.evidence.id))
    assertEquals((result \ "ending" \ "occurrence_id").as[String], ids.last)
    assertEquals((result \ "ending" \ "ply_offset").as[Int], moves.size - 1)
    assertEquals((result \ "scope").as[String], "observed_line")
    assertEquals(EvidenceBackedJudgmentPacket.fromAssembly(ctx).get.primary, None)
    result

  test("finite exchange, loss and recovery expose observed balances without closed gain claims"):
    val laterLoss = recovery ++ List("d8d5", "e4c3", "d5d2")
    val cases = List(
      (exchange, List(100, 0), false),
      (recovery.take(4), List(-100), false),
      (recovery, List(-100, 0), true),
      (laterLoss, List(-100, 0, -100), true))
    val ctx = context(initial, cases.map(_._1))
    for (moves, balances, recovered) <- cases do
      val result = summaryFor(ctx, moves)
      assertEquals((result \ "perspective").as[String], "white")
      val material = (result \ "material").as[JsObject]
      assertEquals((material \ "events").as[List[JsObject]].map(e => (e \ "balance_cp").as[Int]), balances)
      assertEquals((material \ "net_cp").as[Int], balances.last)
      assertEquals((material \ "max_gain_cp").as[Int], (0 :: balances).max)
      assertEquals((material \ "max_loss_cp").as[Int], (0 :: balances).min)
      assertEquals((material \ "recovered_after_loss").as[Boolean], recovered)
      assertEquals((result \ "ending" \ "kind").as[String], "line_end")
      assertEquals(result.keys, Set("scope", "perspective", "premise_ids", "events", "ending", "material"))
    val exchanges = (summaryFor(ctx, exchange) \ "material" \ "events").as[List[JsObject]]
    assertEquals(exchanges.map(e => (e \ "capture" \ "recapture").as[String]), List("excluded", "proven"))

  test("mate, stalemate and an ordinary path ending remain different without any evaluation"):
    val fen = "7k/5K2/8/6Q1/8/8/8/8 w - - 0 1"
    val ctx = context(fen, List(List("g5g7"), List("g5g6")))
    for (move, ending, event) <- List(("g5g7", "checkmate", "mate"), ("g5g6", "stalemate", "stalemate")) do
      val summary = summaryFor(ctx, List(move))
      assertEquals((summary \ "ending" \ "kind").as[String], ending)
      assert((summary \ "events").as[List[JsObject]].exists(e => (e \ "kind").as[String] == event))
    val quiet = context(initial, List(List("g1f3", "g8f6")))
    val summary = summaryFor(quiet, List("g1f3", "g8f6"))
    assertEquals((summary \ "events").as[List[JsObject]], Nil)
    assertEquals((summary \ "material").as[JsObject], Json.obj(
      "events" -> Json.arr(), "net_cp" -> 0, "max_gain_cp" -> 0, "max_loss_cp" -> 0,
      "recovered_after_loss" -> false))
    assertEquals((summary \ "ending" \ "kind").as[String], "line_end")

  test("check and check evasion stay tied to their own event occurrence"):
    val moves = List("e2b5", "e8d8")
    val ctx = context("4k3/8/r7/8/8/8/4B3/4R1K1 w - - 0 1", List(moves))
    val events = (summaryFor(ctx, moves) \ "events").as[List[JsObject]]
    assertEquals(events.map(e => ((e \ "kind").as[String], (e \ "ply_offset").as[Int])),
      List("check" -> 0, "check_evasion" -> 1))
    assertEquals(events.map(e => (e \ "side").as[String]), List("white", "black"))

  test("capture promotion and en passant reuse exact movement, captured square and material values"):
    val cases = List(
      ("1r5k/P7/8/8/8/8/7p/7K w - - 0 1", "a7b8n", 700, 200, "b8"),
      ("7k/8/8/3pP3/8/8/8/7K w - d6 0 1", "e5d6", 100, 0, "d5"),
      ("7k/8/8/8/8/8/r7/R6K b - - 0 1", "a2a1", 500, 0, "a1"))
    for (fen, move, delta, promotion, capturedSquare) <- cases do
      val ctx = context(fen, List(List(move)))
      val summary = summaryFor(ctx, List(move))
      val material = (summary \ "material").as[JsObject]
      val event = (material \ "events").as[List[JsObject]].head
      assertEquals((event \ "delta_cp").as[Int], delta)
      assertEquals((event \ "promotion_gain_cp").as[Int], promotion)
      assertEquals((event \ "capture" \ "square").as[String], capturedSquare)
      assertEquals((material \ "net_cp").as[Int], delta)
      assertEquals((summary \ "perspective").as[String], if fen.contains(" b ") then "black" else "white")
      if promotion > 0 then
        assertEquals((event \ "before_role").as[String], "pawn")
        assertEquals((event \ "after_role").as[String], "knight")
        assert((summary \ "events").as[List[JsObject]].exists(e => (e \ "kind").as[String] == "promotion"))

  test("an illegal suffix and a missing line proof cannot erase an independently certified sibling"):
    val ctx = context(initial, List(exchange :+ "a1a8", recovery))
    assertEquals(ctx.input.suppliedLines.head.acceptedMoves, exchange)
    val expected = summaryFor(ctx, recovery)
    assertEquals((summaryFor(ctx, exchange) \ "ending" \ "kind").as[String], "line_end")
    val excluded = lineFor(ctx, exchange)
    val damaged = ctx.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(
      ctx.evidenceGraph.records.filterNot(_.ref == excluded.evidence)))
    assertEquals(ObservedLineProtocol.summaryJson(damaged, excluded), None)
    assertEquals(ObservedLineProtocol.summaryJson(damaged, lineFor(ctx, recovery)), Some(expected))
    val missingId = ctx.lineTransitionIds(excluded.ref).last
    val noLastEdge = ctx.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(
      ctx.evidenceGraph.records.filterNot(_.ref.id == missingId)))
    assertEquals(ObservedLineProtocol.summaryJson(noLastEdge, excluded), None)
    assertEquals(ObservedLineProtocol.summaryJson(noLastEdge, lineFor(ctx, recovery)), Some(expected))

  test("equal final FENs preserve different capture histories and cannot exchange proof ownership"):
    import SuppliedCommentaryExamples.{ history, pawnCapture, knightCapture }
    val ctx = MoveReviewAssembler.assembleOccurrences(MoveReviewInputAdmission.admitIndependent(history,
      pawnCapture.head, suppliedLines = List(pawnCapture, knightCapture)
        .map(SuppliedCommentaryLine(CommentaryLineSource.Analysis, _))).get)
    val a = lineFor(ctx, pawnCapture)
    val b = lineFor(ctx, knightCapture)
    val aIds = ctx.lineTransitionIds(a.ref)
    val bIds = ctx.lineTransitionIds(b.ref)
    val aSummary = summaryFor(ctx, pawnCapture)
    val bSummary = summaryFor(ctx, knightCapture)
    assertEquals(history.extend(pawnCapture).toOption.get.currentFen, history.extend(knightCapture).toOption.get.currentFen)
    assertEquals(aIds.take(4), bIds.take(4))
    assert(aIds.last != bIds.last)
    assert((aSummary \ "premise_ids") != (bSummary \ "premise_ids"))
    def captureAt7(summary: JsObject) = (summary \ "material" \ "events").as[List[JsObject]]
      .find(e => (e \ "ply_offset").as[Int] == 7).get
    assertEquals((captureAt7(aSummary) \ "capture" \ "role").as[String], "pawn")
    assertEquals((captureAt7(bSummary) \ "capture" \ "role").as[String], "knight")
    val switched = ctx.copy(lineTransitionIds = ctx.lineTransitionIds.updated(a.ref, aIds.init :+ bIds.last))
    assertEquals(ObservedLineProtocol.summaryJson(switched, a), None)
    assertEquals(ObservedLineProtocol.summaryJson(ctx, a.copy(evidence = b.evidence)), None)
