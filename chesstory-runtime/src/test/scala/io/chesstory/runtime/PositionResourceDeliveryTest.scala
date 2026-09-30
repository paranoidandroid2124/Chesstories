package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

class PositionResourceDeliveryTest extends munit.FunSuite:
  private def assemble(fen: String, move: String, paths: List[List[String]] = Nil): JudgmentAssemblyContext =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    MoveReviewAssembler.assembleOccurrences(MoveReviewInputAdmission.admitIndependent(history, move, paths).get)

  private def wire(ctx: JudgmentAssemblyContext): JsObject =
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(ctx).get
    assertEquals(packet.primary, None)
    RuntimeProtocol.observationsJson(packet)

  private def at(body: JsObject, position: PositionNodeRef): JsObject =
    (body \ "positions").as[List[JsObject]].find(p => (p \ "id").as[String] == position.id.get).get

  private def controllers(position: JsObject, square: String): Set[String] =
    (position \ "controls" \ "pieces").as[List[JsObject]].filter(p =>
      (p \ "squares").as[List[String]].contains(square)).map(p => (p \ "piece" \ "square").as[String]).toSet

  test("owned legal moves retain checking consequences including a noncapturing discovered check"):
    val danish = "rnbQ1b1r/ppp2kpp/5n2/8/8/8/PB3PPP/RN2K1NR b KQ - 0 8"
    val ctx = assemble(danish, "f8b4")
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(ctx).get
    val before = ctx.playedTransition.get.from
    val view = packet.resources.find(_.position == before).get
    assert(view.legalRows.get.forall(_.capture.isEmpty))
    assertEquals(view.checkingMoves.get.map(_.move), List("f8b4"))
    val rows = (at(wire(ctx), before) \ "legal_moves" \ "moves").as[List[JsObject]]
    assertEquals(rows.filter(row => (row \ "gives_check").as[Boolean]).map(row => (row \ "uci").as[String]),
      List("f8b4"))
    assertEquals(rows.find(row => (row \ "uci").as[String] == "h8g8")
      .map(row => (row \ "gives_check").as[Boolean]), Some(false))

    val discovered = assemble("4k3/8/8/8/8/8/4B3/4R2K w - - 0 1", "e2a6")
    val facts = (at(wire(discovered), discovered.playedTransition.get.from) \ "legal_moves" \ "moves")
      .as[List[JsObject]]
    assertEquals(facts.find(row => (row \ "uci").as[String] == "e2a6")
      .map(row => (row \ "gives_check").as[Boolean]), Some(true)) // Re1 checks, not the bishop on a6.
    assertEquals(facts.find(row => (row \ "uci").as[String] == "e1f1")
      .map(row => (row \ "gives_check").as[Boolean]), Some(false))

    val missing = ctx.evidenceGraph.records.find(record => record.ref.position == before && (record.payload match
      case relation: RelationFactEvidence => relation.detail match
        case move: RelationWitnessDetail.LegalMove => move.moveUci == "h8g8"
        case _ => false
      case _ => false)).get
    val partial = ctx.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(ctx.evidenceGraph.records.filterNot(_ == missing)))
    val unknown = (at(wire(partial), before) \ "legal_moves" \ "moves").as[List[JsObject]]
    assert(unknown.exists(row => (row \ "uci").as[String] == "f8b4"))
    assert(unknown.forall(row => (row \ "gives_check").toOption.isEmpty))

  test("empty-square controls preserve a lost pawn control alongside a retained knight control"):
    val ctx = assemble("7k/7p/8/8/4P3/4N3/8/7K w - - 0 1", "e4e5")
    val edge = ctx.playedTransition.get
    val recordCount = ctx.evidenceGraph.records.size
    val body = wire(ctx)
    assertEquals(controllers(at(body, edge.from), "d5"), Set("e4", "e3"))
    assertEquals(controllers(at(body, edge.to), "d5"), Set("e3"))
    assertEquals((at(body, edge.to) \ "controls" \ "complete").as[Boolean], true)
    assertEquals(ctx.evidenceGraph.records.size, recordCount)
    val path = (body \ "paths").as[List[JsObject]].head
    assertEquals((path \ "position_ids").as[List[String]], List(edge.from.id.get, edge.to.id.get))

  test("the queen ray identifies the departed bishop and the new first occupied square"):
    val ctx = assemble("2r1r1k1/1b3ppp/p2b4/1p1n1P2/6Pq/PNP5/1PQ3BP/R1B2R1K w - - 3 22", "g2f3")
    val edge = ctx.playedTransition.get
    val body = wire(ctx)
    def occupants(position: PositionNodeRef): List[String] =
      val ray = (at(body, position) \ "rays" \ "lines").as[List[JsObject]].find(row =>
        (row \ "piece" \ "square").as[String] == "c2" &&
          (row \ "squares").as[List[String]].contains("h2")).get
      (ray \ "occupants").as[List[JsObject]].map(row => (row \ "square").as[String])
    assertEquals(occupants(edge.from), List("g2", "h2"))
    assertEquals(occupants(edge.to), List("h2"))
    assert(!controllers(at(body, edge.from), "h2")("c2"))
    assert(controllers(at(body, edge.to), "h2")("c2"))

  test("clear pawn paths keep geometry separate from the actual side-to-move legal catalogue"):
    val ctx = assemble("r6k/8/8/8/8/8/5P2/4K3 b - - 0 1", "a8e8")
    val edge = ctx.playedTransition.get
    val body = wire(ctx)
    def paths(position: PositionNodeRef): List[JsObject] =
      (at(body, position) \ "pawn_advances" \ "paths").as[List[JsObject]]
    assertEquals((at(body, edge.from) \ "fen").as[String].split(" ")(1), "b")
    assertEquals((at(body, edge.to) \ "fen").as[String].split(" ")(1), "w")
    assertEquals(paths(edge.to).map(p => (p \ "traversed_squares").as[List[String]]),
      List(List("f3"), List("f3", "f4")))
    for position <- List(edge.from, edge.to) do
      val legal = (at(body, position) \ "legal_moves").as[JsObject]
      assertEquals((legal \ "complete").as[Boolean], true)
      assert(!(legal \ "moves").as[List[JsObject]].exists(p => (p \ "uci").as[String].startsWith("f2")))
      assert(paths(position).forall(p => (p \ "legality").toOption.isEmpty && (p \ "legal_moves").toOption.isEmpty))

  test("missing control evidence marks a partial family and preserves other certified resources"):
    val ctx = assemble(chess.variant.Standard.initialFen.value, "g1f3")
    val after = ctx.playedTransition.get.to
    val missing = ctx.evidenceGraph.records.find(record => record.ref.position == after && (record.payload match
      case relation: RelationFactEvidence => relation.detail match
        case RelationWitnessDetail.GeometricControl(_, from, _, to) => from.key == "f3" && to.key == "e5"
        case _ => false
      case _ => false)).get
    val broken = ctx.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(ctx.evidenceGraph.records.filterNot(_ == missing)))
    val position = at(wire(broken), after)
    assertEquals((position \ "controls" \ "complete").as[Boolean], false)
    assert(controllers(position, "d4")("f3"))
    assert(!controllers(position, "e5")("f3"))
    assertEquals((position \ "rays" \ "complete").as[Boolean], true)
    assertEquals((position \ "pawn_advances" \ "complete").as[Boolean], true)
    assertEquals((position \ "pawn_states" \ "complete").as[Boolean], true)
    assertEquals((position \ "pawn_files" \ "complete").as[Boolean], true)

  test("a missing legal-move premise marks the catalogue partial without losing other certified moves or paths"):
    val ctx = assemble(chess.variant.Standard.initialFen.value, "g1f3")
    val before = ctx.playedTransition.get.from
    val missing = ctx.evidenceGraph.records.find(record => record.ref.position == before && (record.payload match
      case relation: RelationFactEvidence => relation.detail match
        case move: RelationWitnessDetail.LegalMove => move.moveUci == "f2f3"
        case _ => false
      case _ => false)).get
    val broken = ctx.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(ctx.evidenceGraph.records.filterNot(_ == missing)))
    val position = at(wire(broken), before)
    val paths = (position \ "pawn_advances" \ "paths").as[List[JsObject]]
    assertEquals((paths.find(row => (row \ "piece" \ "square").as[String] == "f2" &&
      (row \ "destination").as[String] == "f3").get \ "traversed_squares").as[List[String]], List("f3"))
    val legal = (position \ "legal_moves" \ "moves").as[List[JsObject]].map(p => (p \ "uci").as[String])
    assertEquals((position \ "legal_moves" \ "complete").as[Boolean], false)
    assert(!legal.contains("f2f3"))
    assert(legal.contains("f2f4"))
    assert(legal.contains("g1f3"))
    assertEquals((position \ "pawn_advances" \ "complete").as[Boolean], true)
    assertEquals((position \ "pawn_states" \ "complete").as[Boolean], true)
    assertEquals((position \ "pawn_files" \ "complete").as[Boolean], true)

  test("the same knight controls expose different actual legal catalogues after retaining or removing the pin"):
    val fen = "4k3/4n3/8/8/8/8/7P/4R1K1 w - - 0 1"
    def after(move: String): JsObject =
      val ctx = assemble(fen, move)
      at(wire(ctx), ctx.playedTransition.get.to)
    val pinned = after("h2h3")
    val released = after("e1a1")
    def knightControls(p: JsObject) = (p \ "controls" \ "pieces").as[List[JsObject]]
      .find(row => (row \ "piece" \ "square").as[String] == "e7").get
    assertEquals(knightControls(pinned), knightControls(released))
    def knightMoves(p: JsObject) = (p \ "legal_moves" \ "moves").as[List[JsObject]]
      .map(row => (row \ "uci").as[String]).filter(_.startsWith("e7"))
    assertEquals(knightMoves(pinned), Nil)
    assertEquals(knightMoves(released), List("e7c6", "e7c8", "e7d5", "e7f5", "e7g6", "e7g8"))
    assertEquals((pinned \ "legal_moves" \ "complete").as[Boolean], true)

  test("castling keeps its actual destination and promotion choices remain distinct legal moves"):
    val castle = assemble("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", "e1g1")
    val castleMoves = (at(wire(castle), castle.playedTransition.get.from) \ "legal_moves" \ "moves").as[List[JsObject]]
    assert(castleMoves.exists(row => Set("e1g1", "e1h1")((row \ "uci").as[String]) &&
      (row \ "destination").as[String] == "g1"))
    val promotion = assemble("1r5k/P7/8/8/8/8/8/7K w - - 0 1", "a7a8n")
    val position = at(wire(promotion), promotion.playedTransition.get.from)
    val moves = (position \ "legal_moves" \ "moves").as[List[JsObject]].filter(row =>
      (row \ "uci").as[String].startsWith("a7"))
    assertEquals(moves.map(row => (row \ "uci").as[String]).toSet,
      Set("a7a8q", "a7a8r", "a7a8b", "a7a8n", "a7b8q", "a7b8r", "a7b8b", "a7b8n"))
    assertEquals(moves.map(row => (row \ "source_id").as[String]).distinct.size, 8)
    moves.foreach { row =>
      if (row \ "destination").as[String] == "b8" then
        assertEquals((row \ "captured_square").as[String], "b8")
      else assert((row \ "captured_square").toOption.isEmpty)
    }
    assert((position \ "pawn_advances" \ "paths").as[List[JsObject]].forall(row =>
      row.keys == Set("piece", "destination", "traversed_squares", "source_id")))

  private def pawnState(position: JsObject, square: String): JsObject =
    (position \ "pawn_states" \ "pawns").as[List[JsObject]].find(p => (p \ "square").as[String] == square).get

  private def without(ctx: JudgmentAssemblyContext, position: PositionNodeRef)(
      predicate: RelationWitnessDetail => Boolean
  ): JudgmentAssemblyContext =
    val missing = ctx.evidenceGraph.records.find(record => record.ref.position == position && (record.payload match
      case fact: RelationFactEvidence => predicate(fact.detail)
      case _ => false)).get
    ctx.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(ctx.evidenceGraph.records.filterNot(_ == missing)))

  test("already open and semi-open files remain current facts when a rook enters them"):
    val cases = List(
      "7k/8/8/8/8/8/P7/R6K w - - 0 1" -> "open",
      "7k/4p3/8/8/8/8/P7/R6K w - - 0 1" -> "black_pawns_only",
      "7k/8/8/8/8/8/P3P3/R6K w - - 0 1" -> "white_pawns_only",
      "7k/4p3/8/8/8/8/P3P3/R6K w - - 0 1" -> "both_sides")
    cases.foreach { case (fen, occupancy) =>
      val ctx = assemble(fen, "a1e1")
      val body = wire(ctx)
      val edge = ctx.playedTransition.get
      assertEquals(at(body, edge.from) \ "pawn_files", at(body, edge.to) \ "pawn_files")
      val files = (at(body, edge.to) \ "pawn_files" \ "files").as[List[JsObject]]
      assertEquals(files.size, 8)
      val file = files.find(row => (row \ "file").as[String] == "e").get
      assertEquals((file \ "occupancy").as[String], occupancy)
      assertEquals((file \ "source_ids").as[List[String]].distinct.size, 2)
    }

  test("a missing pawn passage affects that passed claim only"):
    val ctx = assemble("k7/8/8/3P4/2P5/8/8/7K w - - 0 1", "h1g1")
    val body = wire(ctx)
    val edge = ctx.playedTransition.get
    assert(!(body \ "statements").as[List[JsObject]].exists(row => (row \ "kind").as[String] == "pawn_structure"))
    assertEquals(at(body, edge.from) \ "pawn_states", at(body, edge.to) \ "pawn_states")
    assertEquals((pawnState(at(body, edge.to), "d5") \ "passed").as[Boolean], true)
    assertEquals((at(body, edge.to) \ "pawn_states" \ "complete").as[Boolean], true)
    val after = ctx.playedTransition.get.to
    val broken = without(ctx, after) {
      case RelationWitnessDetail.PawnPassage(_, square, _) => square.key == "d5"
      case _ => false
    }
    val position = at(wire(broken), after)
    val pawn = pawnState(position, "d5")
    assertEquals(pawn \ "passed", JsDefined(JsNull))
    assertEquals((pawn \ "isolated").as[Boolean], false)
    assertEquals((pawn \ "doubled").as[Boolean], false)
    assertEquals((pawnState(position, "c4") \ "passed").as[Boolean], true)
    assertEquals((position \ "pawn_states" \ "complete").as[Boolean], false)
    assertEquals((position \ "pawn_files" \ "complete").as[Boolean], true)

  test("a missing file source cannot erase independent pawn facets or other file classifications"):
    val ctx = assemble("k7/8/8/3P4/2P5/8/8/7K w - - 0 1", "h1g1")
    val after = ctx.playedTransition.get.to
    val broken = without(ctx, after) {
      case RelationWitnessDetail.PawnFileGroup(chess.Color.White, file, _) => file.key == "c"
      case _ => false
    }
    val position = at(wire(broken), after)
    val pawn = pawnState(position, "d5")
    assertEquals(pawn \ "isolated", JsDefined(JsNull))
    assertEquals((pawn \ "passed").as[Boolean], true)
    assertEquals((pawn \ "doubled").as[Boolean], false)
    assertEquals(pawnState(position, "c4") \ "doubled", JsDefined(JsNull))
    val files = (position \ "pawn_files" \ "files").as[List[JsObject]]
    assertEquals(files.size, 7)
    assert(!files.exists(row => (row \ "file").as[String] == "c"))
    assertEquals((position \ "pawn_files" \ "complete").as[Boolean], false)
    assertEquals((position \ "controls" \ "complete").as[Boolean], true)

  test("one surviving occupied adjacent file certifies non-isolation despite a missing other file"):
    val ctx = assemble("k7/8/8/3P4/2P1P3/8/8/7K w - - 0 1", "h1g1")
    val after = ctx.playedTransition.get.to
    val broken = without(ctx, after) {
      case RelationWitnessDetail.PawnFileGroup(chess.Color.White, file, _) => file.key == "c"
      case _ => false
    }
    val pawn = pawnState(at(wire(broken), after), "d5")
    assertEquals((pawn \ "isolated").as[Boolean], false)
    assertEquals((pawn \ "passed").as[Boolean], true)
    assertEquals((pawn \ "doubled").as[Boolean], false)

  test("doubled isolated pawns and a known empty pawn catalogue keep affirmative state and exact absence"):
    val doubled = assemble("7k/8/8/8/P7/P7/8/7K w - - 0 1", "h1g1")
    val position = at(wire(doubled), doubled.playedTransition.get.to)
    for square <- List("a3", "a4") do
      val pawn = pawnState(position, square)
      assertEquals((pawn \ "doubled").as[Boolean], true)
      assertEquals((pawn \ "isolated").as[Boolean], true)
      assertEquals((pawn \ "passed").as[Boolean], true)
    val removed = assemble("7k/8/8/8/8/8/p7/K7 w - - 0 1", "a1a2")
    val empty = at(wire(removed), removed.playedTransition.get.to)
    assertEquals((empty \ "pawn_states" \ "pawns").as[List[JsObject]], Nil)
    assertEquals((empty \ "pawn_states" \ "complete").as[Boolean], true)
    assert((empty \ "pawn_files" \ "files").as[List[JsObject]].forall(row =>
      (row \ "occupancy").as[String] == "open"))

  test("legal capture targets come from the certified witness including the remote en-passant pawn"):
    val ctx = assemble("1k6/8/8/r2pP2R/8/8/8/6K1 w - d6 0 2", "e5d6")
    val position = at(wire(ctx), ctx.playedTransition.get.from)
    val moves = (position \ "legal_moves" \ "moves").as[List[JsObject]]
    val capture = moves.find(row => (row \ "uci").as[String] == "e5d6").get
    assertEquals((capture \ "destination").as[String], "d6")
    assertEquals((capture \ "captured_square").as[String], "d5")
    val quiet = moves.find(row => (row \ "uci").as[String] == "e5e6").get
    assert((quiet \ "captured_square").toOption.isEmpty)
