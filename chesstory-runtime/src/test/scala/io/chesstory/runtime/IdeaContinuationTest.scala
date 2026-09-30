package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** Book continuations exercise reading and delivery with fixed, synthetic reports.
  * Their scores are not measurements of engine strength or of the author's evaluation.
  */
class IdeaContinuationTest extends munit.FunSuite:
  private val profile = CommentaryEngineProfile.ServerAdmitted

  private def project(fen: String, moves: String, alternatives: List[String], supplied: List[String] = Nil):
      (CommentaryJobState, JsObject) =
    val owner = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val rows = moves.split(" ").toList :: alternatives.map(_.split(" ").toList)
    rows.foreach(line => assert(owner.extend(line).isRight, line.mkString(" ")))
    def submit(state: CommentaryJobState, lines: List[List[String]]): CommentaryJobState =
      val active = state.asInstanceOf[AwaitingEngineWork]
      val work = active.issuedWork
      CommentaryJobReducer.submitIssuedWorkReport(active,
        EngineWorkReport.Completed(profile, work.workId, work.executionKeySha256,
          lines.zipWithIndex.map((line, rank) => ReportedEngineLineSuffix(line,
            ReportedWhiteEngineScore.Centipawns(-rank * 20), work.requiredDepth))), 1L).toOption.get
    val initial = CommentaryJobReducer.startJob(owner.initialFen, owner.movePrefixUci, fen, rows.head.head,
      owner.extend(rows.head.take(1)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L,
      inputLines = supplied.map(line => SuppliedCommentaryLine(CommentaryLineSource.Study, line.split(" ").toList)))
      .toOption.get
    val state = submit(submit(initial, rows.map(_.take(1))), rows)
    state -> (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("i" * 32, "fixed-book-continuation", 10000L, state)) \ "observations").as[JsObject]

  private def mainText(observations: JsObject): String =
    val current = (observations \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "detail" \ "type").asOpt[String].contains("root_comparison")).get
    (current \ "detail" \ "text").as[String]

  private def compose(state: CommentaryJobState, observations: JsObject, line: String, through: Int,
      contacts: List[(CommentaryFact.Piece, CommentaryFact.Piece)],
      lineOverride: Option[String] = None, reservedText: Option[String] = None,
      cause: Option[CommentaryFact.Piece] = None): JsObject =
    val packet = state.observationsPacket.get
    val projection = MoveWorkProjection.select(packet)
    val base = (projection.baseJson \ "candidates").as[List[JsObject]]
      .find(c => (c \ "detail" \ "type").asOpt[String].contains("root_comparison")).get
    val current = reservedText.fold(base)(text => base + ("detail" ->
      ((base \ "detail").as[JsObject] + ("text" -> JsString(text)))))
    val path = (observations \ "paths").as[List[JsObject]]
      .find(p => (p \ "moves").as[List[String]] == line.split(" ").toList).get
    val anchor = MoveWorkProjection.AttackContext(lineOverride.getOrElse((path \ "id").as[String]),
      line.split(" ").take(through).toList, contacts, cause)
    val result = projection.withMoveEffects(current, context = List(MoveWorkProjection.Context(
      "The selected attack keeps its original bounded description.",
      Some("These concrete attacks do not by themselves establish a forced gain."), Some(anchor))))
    def strings(value: JsValue): Iterator[String] = value match
      case JsString(s) => Iterator.single(s)
      case JsArray(values) => values.iterator.flatMap(strings)
      case obj: JsObject => obj.values.iterator.flatMap(strings)
      case _ => Iterator.empty
    val published = strings(observations - "selection").toSet
    (result \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil).foreach(effect =>
      (effect \ "evidence_ids").as[List[String]].foreach(id => assert(published(id), id)))
    result

  test("Ruy Lopez 13 reading baseline"):
    val cases = List(
      ("18.Nh2", "r2qrbk1/1b3p1p/p2p1np1/1pnPp1B1/P1p1P3/2P2NNP/1PB2PP1/R2QR1K1 w - - 2 18",
        "f3h2 h7h6 g5e3 h6h5 d1f3 f6h7 f3e2 d8c7 g3f1 a8b8 f1d2 b7c8 a4b5 a6b5 f2f4 e5f4 e3f4 h7f6",
        List("d1d2", "g5e3")),
      ("22.Ngf1", "r3rbk1/1bq2p1n/p2p2p1/1pnPp2p/P1p1P3/2P1B1NP/1PB1QPPN/R3R1K1 w - - 4 22",
        "g3f1 a8b8 f1d2 b7c8 a4b5 a6b5 f2f4 e5f4 e3f4 h7f6 e2f1 f6d7 h2f3 f8g7 f1f2 b8b6 f4e3 b6a6",
        List("a1a3", "a1a2")),
      ("34.g4", "4r1k1/2qn1p2/b2p2pb/1pnP3p/2pBP3/2P2N1P/1PBN1QP1/5RK1 w - - 4 34",
        "g2g4 h5g4 f2h4 g4f3 h4h6 d7e5 d2f3 c5d7 f3g5 d7f8 g5f7 c7f7 f1f7 g8f7 c2d1 e8e7 d1g4",
        List("g1h1", "f2h4")))
    cases.foreach { (name, fen, line, alternatives) =>
      val (_, observations) = project(fen, line, alternatives)
      val text = mainText(observations)
      name match
        case "18.Nh2" =>
          assert(text.contains("Nh2 clears f3") && text.contains("Qf3") &&
            text.contains("Nh7: moves the initially attacked Black knight on f6"), text)
        case "22.Ngf1" =>
          assert(text.contains("gives up that knight's support of White pawn on e4") &&
            text.contains("Nd2, White knight on d2 adds support to White pawn on e4"), text)
        case _ => assert(text.contains("Nxf3 actually recaptures"), text)
    }

  private val ruyFen = "4r1k1/2qn1p2/b2p2pb/1pnP3p/2pBP3/2P2N1P/1PBN1QP1/5RK1 w - - 4 34"
  private val ruyLine = "g2g4 h5g4 f2h4 g4f3 h4h6 d7e5 d2f3"
  private val queenContact = CommentaryFact.Piece("white", "queen", "f2") ->
    CommentaryFact.Piece("black", "bishop", "h6")

  test("a selected later attack carries the bishop gain alongside the earlier knight loss"):
    val (state, obs) = project(ruyFen, ruyLine, List("g1h1", "f2h4"))
    val candidate = compose(state, obs, ruyLine, 3, List(queenContact))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Qxh6: captures the initially attacked Black bishop on h6"), text)
    assert(text.contains("captures White knight on f3") && text.contains("Nxf3"), text)
    assertEquals("Qxh6:".r.findAllIn(text).length, 1, text)
    assert(text.contains("selected attack keeps"), text)
    assert(text.contains("do not by themselves establish a forced gain"), text)

  private val d4Fen = "rn2kb1r/ppp1qppp/3pbn2/8/8/2N1N3/PPPPQPPP/R1B1KB1R w KQkq - 6 8"
  private val d4Line = "d2d4 d6d5 g2g4 h7h6 f1g2 b8c6 e3d5 e6d5 c3d5 e7e2 e1e2 e8c8 d5f6 c6d4 e2d1 g7f6 c1d2 h6h5 c2c3 d4f3 g2f3 f8h6 d1c2 d8d2 c2b3 h5h4 h1f1 h6f4"
  private val d4Contacts = List(
    CommentaryFact.Piece("black", "rook", "a8") -> CommentaryFact.Piece("white", "bishop", "c1"),
    CommentaryFact.Piece("black", "knight", "b8") -> CommentaryFact.Piece("white", "bishop", "c1"))

  test("an opened rook attack preserves the other attacker's removal and actual check reply"):
    val (state, obs) = project(d4Fen, "g2g3", List(d4Line, "d2d3"))
    val candidate = compose(state, obs, d4Line, 20, d4Contacts)
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Bxf3: captures the original attacker Black knight on f3"), text)
    assert(text.contains("Rxd2+") && text.contains("captures the initially attacked White bishop on d2"), text)
    assert(text.contains("Kb3") && text.toLowerCase.contains("check"), text)
    assertEquals(text.sliding("Rxd2+:".length).count(_ == "Rxd2+:"), 1, text)
    assertEquals(text.sliding("captures the initially attacked White bishop on d2".length)
      .count(_ == "captures the initially attacked White bishop on d2"), 1, text)
    assert(!text.contains("recaptures White bishop on d2") && !text.contains("forced capture"), text)

  test("the clearing piece's immediate loss stays with the selected stationary attack"):
    val (state, obs) = project(d4Fen, "g2g3", List(d4Line, "d2d3"))
    val candidate = compose(state, obs, d4Line, 20, d4Contacts.take(1), cause = Some(d4Contacts(1)._1))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Bxf3: White bishop on g2 captures Black knight on f3"), text)
    assert(text.contains("The captured knight played Nf3"), text)
    assert(text.contains("Rxd2+") && text.contains("Kb3"), text)
    val unrelated = compose(state, obs, d4Line, 20, d4Contacts.take(1),
      cause = Some(CommentaryFact.Piece("white", "bishop", "f1")))
    assert(!(unrelated \ "detail" \ "text").as[String].contains("The captured bishop played Nf3"))
    val (ruyState, ruyObs) = project(ruyFen, ruyLine, List("g1h1", "f2h4"))
    val otherLoss = compose(ruyState, ruyObs, ruyLine, 3, List(queenContact),
      cause = Some(CommentaryFact.Piece("white", "knight", "f3")))
    assert(!(otherLoss \ "detail" \ "text").as[String].contains("The captured knight played Qh4"))

  test("selected contacts share one transaction without crowding out the later attack's gain and cost"):
    val g3Line = "g2g3 d6d5 f1g2 c7c6 d2d4 h7h5 h2h4 g7g6 e1g1 f8g7 e2d1 e8g8 c3e2 b8d7 e2f4 f8e8 c2c3 f6e4 d1b3 g7h6 e3d5 c6d5"
    val (_, obs) = project(d4Fen, g3Line, List(d4Line, "d2d3"))
    val comparison = (obs \ "selection" \ "candidates").as[List[JsObject]].find(c =>
      (c \ "detail" \ "type").asOpt[String].contains("question_comparison") &&
        (c \ "detail" \ "prefix_moves_uci").asOpt[List[String]].contains(Nil) &&
        (c \ "detail" \ "text").as[String].contains("Nf3 (ply 20)")).get
    val text = (comparison \ "detail" \ "text").as[String]
    assert(text.contains("Bxf3: White bishop on g2 captures Black knight on f3"), text)
    assert(text.contains("Rxd2+") && text.contains("Kb3"), text)
    assertEquals(text.sliding("Rxd2+:".length).count(_ == "Rxd2+:"), 1, text)
    assert(text.contains("plies 10–12") && text.contains("plies 14–19"), text)
    assert(text.length <= 4000, text.length.toString)

  test("a short owned continuation cannot borrow the capture or king reply from a supplied longer line"):
    List(23, 24).foreach { count =>
      val short = d4Line.split(" ").take(count).mkString(" ")
      val (state, obs) = project(d4Fen, "g2g3", List(short, "d2d3"), supplied = List(d4Line))
      val text = (compose(state, obs, short, 20, d4Contacts) \ "detail" \ "text").as[String]
      assert(!text.contains("Kb3"), text)
      assertEquals(text.contains("Rxd2+"), count == 24, text)
    }

  test("a contact must match the physical target and exact owned path"):
    val (state, obs) = project(ruyFen, ruyLine, List("g1h1", "f2h4"))
    val wrongTarget = queenContact._1 -> CommentaryFact.Piece("black", "bishop", "a6")
    List(
      compose(state, obs, ruyLine, 3, List(wrongTarget)),
      compose(state, obs, ruyLine, 3, List(queenContact), Some("another-answer-line")),
      compose(state, obs, ruyLine, 3, List(queenContact, wrongTarget))
    ).foreach { candidate =>
      val text = (candidate \ "detail" \ "text").as[String]
      assert(text.contains("selected attack keeps its original bounded description"), text)
      assert(!text.contains("Qxh6:"), text)
    }

  test("a full continuation cannot evict the shorter already admitted explanation at the text limit"):
    val line = "a1a2 h8h7 f2h4 h7g7 h4h6"
    val (state, obs) = project("7k/8/7b/8/8/8/5Q2/K7 w - - 0 1", line, List("a1b1", "a1b2"))
    val candidate = compose(state, obs, line, 3, List(queenContact), reservedText = Some("x" * 3820))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("selected attack keeps its original bounded description"), text)
    assert(text.length <= 4000, text.length.toString)
    assert(!text.contains("Qxh6"), text)
    assertEquals(text.sliding("These concrete attacks".length).count(_ == "These concrete attacks"), 1, text)
