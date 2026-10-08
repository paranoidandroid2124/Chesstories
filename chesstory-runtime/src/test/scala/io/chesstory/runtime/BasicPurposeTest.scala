package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.*

/** Legal open examples test explanation scope, not an engine ranking of moves. */
class BasicPurposeTest extends munit.FunSuite:
  private case class Position(fen: String, prefix: String = "", mates: List[Option[Int]] = Nil)
  private val pirc = Position("rnbqkb1r/ppp1pp1p/3p1np1/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 4",
    "e2e4 d7d6 d2d4 g8f6 b1c3 g7g6")
  private val files = Position("r4rk1/pp3ppp/2p5/8/8/2P2N2/PP3PPP/R4RK1 w - - 0 1")

  private def prepare(position: Position, lines: String*) =
    val rows = lines.toList.map(_.split(" ").toList)
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(position.fen, rows.head.head,
      rows.zipWithIndex.map((moves, index) => EngineLine(moves, 0, position.mates.lift(index).flatten, 16)),
      movePrefixUci = position.prefix.split(" ").toList.filter(_.nonEmpty))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    (packet, paths, pair)

  private def summary(position: Position, lines: String*): JsObject =
    val (packet, _, _) = prepare(position, lines*)
    val response = RuntimeProtocol.encodeMoveMeaningResult(None, packet)
    assertEquals(response.httpStatus, 200)
    (response.body \ "move_commentary" \ "observations" \ "selection" \ "candidates")
      .as[List[JsObject]].map(value => (value \ "detail").as[JsObject])
      .find(value => (value \ "type").as[String] == "root_comparison").get

  test("open-file placement is a sufficient current purpose without an invented use of an empty square"):
    val detail = summary(files, "f1e1 h7h6", "f1d1 h7h6")
    val text = (detail \ "summary").as[String]
    List("Rfe1", "Rfd1", "open e-file", "open d-file").foreach(value => assert(text.contains(value), text))
    List("controls e5", "controls d4", "also supported", "neither side loses", "safe", "better")
      .foreach(value => assert(!text.contains(value), text))
    val expanded = (detail \ "text").as[String]
    assert(expanded.contains("controls e5") && expanded.contains("controls d4"), expanded)
    val question = (detail \ "summary_question").as[JsObject]
    assert((question \ "detail_units").as[List[JsObject]].nonEmpty)
    assert((question \ "units").as[List[JsObject]].forall(unit => !(unit \ "focus").isDefined),
      "a file-purpose click must not highlight the omitted shared empty square or its helper")

  test("convergence keeps the basic actions that explain how the two opening choices get there"):
    val detail = summary(Position("r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 1 5",
      "e2e4 e7e5 g1f3 b8c6 f1c4 f8c5 d2d3 g8f6"),
      "e1g1 d7d6 b1c3", "b1c3 d7d6 e1g1")
    val text = (detail \ "summary").as[String]
    List("O-O", "castles", "rook from h1 to f1", "Nc3 develops the knight", "same recorded position")
      .foreach(value => assert(text.contains(value), text))
    assert(!text.contains("king is safe"), text)

  test("later development in the same plan keeps the root purposes ahead of their convergence"):
    val detail = summary(pirc, "g1f3 f8g7 f1e2 e8g8 e1g1 a7a6 c1e3 b7b5",
      "f1e2 f8g7 c1e3 e8g8 g1f3 a7a6 e1g1 b7b6")
    val text = (detail \ "summary").as[String]
    assert(text.contains("Nf3 develops") && text.contains("Be2 develops"), text)
    assert(text.indexOf("Nf3 develops") < text.indexOf("same recorded position"), text)
    assert(!text.contains("keeps supporting") && !text.contains("still present"), text)
    assert((detail \ "summary_question" \ "detail_units").as[List[JsObject]].nonEmpty)

  test("a plain development explanation does not need a later use and is not repeated"):
    val detail = summary(pirc, "g1f3 f8g7", "f1e2 f8g7")
    val text = (detail \ "summary").as[String]
    assertEquals(text.sliding("Nf3 develops".length).count(_ == "Nf3 develops"), 1)
    assertEquals(text.sliding("Be2 develops".length).count(_ == "Be2 develops"), 1)
    assert(!text.contains("keeps supporting") && !text.contains("still present"), text)

  test("a development label cannot displace the observed pawn loss or the other move's defence"):
    val detail = summary(Position("rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2",
      "e2e4 e7e5 g1f3"),
      "f8c5 f3e5", "b8c6 b1c3")
    val text = (detail \ "summary").as[String]
    List("Nxe5", "captures the black pawn", "Nc6", "support").foreach(value => assert(text.contains(value), text))

  test("current purpose never turns a root-only line into an observed response"):
    val detail = summary(pirc, "g1f3", "f1e2")
    val text = (detail \ "summary").as[String]
    assert(text.contains("Nf3 develops") && text.contains("Be2 develops"), text)
    assertEquals(text.sliding("no following reply is recorded".length).count(_ == "no following reply is recorded"), 2)

  test("an ordinary-looking recapture retains the check that explains the selected king response"):
    val detail = summary(Position("r1bqkb1r/ppp2ppp/2n2n2/3Pp1N1/2B5/8/PPPP1PPP/RNBQK2R b KQkq - 0 5",
      "e2e4 e7e5 g1f3 b8c6 f1c4 g8f6 f3g5 d7d5 e4d5"),
      "f6d5 g5f7 e8f7 d1f3 f7e6 b1c3", "c6a5 c4b5 c7c6 d5c6 b7c6")
    val text = (detail \ "summary").as[String]
    List("Nxf7", "Kxf7", "Qf3+", "Ke6", "gives check").foreach(value => assert(text.contains(value), text))
    assert(!text.contains("h7"), "an unrelated support contrast belongs in detail: " + text)

  test("open-file placement cannot hide an observed capture with checkmate"):
    val detail = summary(Position("4r1k1/pp3ppp/2p5/8/8/2P5/PP3PPP/5RK1 w - - 0 1",
      mates = List(Some(-1), None)), "f1e1 e8e1", "f1a1 h7h6")
    val text = (detail \ "summary").as[String]
    List("Re1", "Rxe1#", "Black wins by checkmate").foreach(value => assert(text.contains(value), text))
    assert(!text.contains("open e-file"), text)

  test("a familiar opening board without its history cannot certify first development"):
    val detail = summary(pirc.copy(prefix = ""), "g1f3 f8g7", "f1e2 f8g7")
    val text = (detail \ "summary").as[String]
    assert(!text.contains("develops"), text)

  test("purpose selection reuses exact occurrence evidence and leaves missing facts unresolved"):
    import CommentaryAnswer.*
    val (packet, paths, pair) = prepare(files, "f1e1 h7h6", "f1d1 h7h6")
    val original = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    assertEquals(MoveWorkProjection.withRootActions(pair, Nil, original), original)
    val notes = MoveWorkProjection.select(packet, paths).local
    val unrelated = notes.filterNot(note => pair.exists(_.ids.head == note.edge.evidence.id))
    assertEquals(MoveWorkProjection.withRootActions(pair, unrelated, original), original)
    val selected = MoveWorkProjection.withRootActions(pair, notes, original)
    assert(selected.evidence.forall(id => packet.evidenceGraph.byId.get(id).exists(packet.evidenceGraph.proofEligible)))
    val fileEvidence = notes.filter(note => pair.exists(_.ids.head == note.edge.evidence.id) &&
      note.note.detail.isInstanceOf[MoveWorkProjection.Detail.RookFile]).flatMap(_.evidence)
    assert(fileEvidence.nonEmpty && fileEvidence.forall(selected.evidence.contains),
      "narrowing the displayed highlight retains the full admitted file-purpose proof")
    assert(selected.core.flatMap(_.statements).forall(_.isInstanceOf[Action]))
    assert(original.core.forall(selected.details.contains))
    assert(RootQuestionSummary.realize(paths, pair, selected).nonEmpty)
