package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.*

/** Fixed continuations test explanation contracts, not engine strength or release coverage. */
class ReferenceMeaningTest extends munit.FunSuite:
  private val re1 = "r2qkb1r/pppb1ppp/2np1n2/1B2p3/4P3/3P1N2/PPP2PPP/RNBQ1RK1 w kq - 2 6"
  private val bg5 = "r2qrbk1/pppb1ppp/2np1n2/1B2p3/P3P3/2PP1N1P/1P3PP1/RNBQR1K1 w - - 1 10"
  private val qxd4 = "3rr1k1/pppq1pb1/5nnp/3P2p1/P2p4/2P1NNBP/1P3PP1/R2QR1K1 w - - 0 19"
  private val e5 = "r4r1k/2qbnppp/3ppn2/8/ppB1PN1Q/5N2/PPP2PPP/3RR1K1 b - - 1 20"
  private val kh8 = "rn2nrk1/2qbbppp/3pp2B/p7/1p2PN2/1B3NQ1/PPP2PPP/3R1RK1 b - - 1 15"
  private def project(fen: String, lines: String*): JsObject =
    val rows = lines.toList.map(_.split(" ").toList)
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen, playedMoveUci = rows.head.head,
      variations = rows.map(EngineLine(_, 0, None, 8)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    RuntimeProtocol.observationsJson(packet)
  private def selected(out: JsObject) = (out \ "selection" \ "candidates").as[List[JsObject]]
    .filter(c => (c \ "selection").as[String] == "selected" &&
      (c \ "detail" \ "type").as[String] == "root_comparison")
  private def effects(out: JsObject, scope: String) = selected(out).flatMap(c =>
    (c \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil)).filter(e =>
      (e \ "scope").as[String] == scope)
  private def text(out: JsObject) = selected(out).map(c => (c \ "detail" \ "text").as[String]).mkString(" ")

  test("Re1 preserves the same c-pawn's two-square option and links its actual support after d4-d5"):
    val out = project(re1, "f1e1 g7g6 d3d4 f8g7 d4d5 c6e7 b5d7 f6d7 c1e3 e8g8 c2c4",
      "c2c3 g7g6 f1e1 f8g7 b1d2 e8g8")
    val row = effects(out, "pawn_option").headOption.getOrElse(fail(text(out)))
    assertEquals((row \ "actor" \ "square").as[String], "c2")
    assertEquals((row \ "target" \ "square").as[String], "d3")
    assertEquals((row \ "through_plies").as[List[Int]], List(11, 1))
    assert(text(out).contains("d4–d5") && text(out).contains("one-square advance"), text(out))

  test("a short Re1 PV preserves the option without inventing its use or peer impossibility"):
    val out = project(re1, "f1e1 g7g6", "c2c3 g7g6")
    val row = effects(out, "pawn_option").head
    assertEquals((row \ "through_plies").as[List[Int]], List(1, 1))
    assertEquals(row \ "target", JsDefined(JsNull))
    assert(!text(out).contains("uses that two-square"), text(out))
    assert(text(out).contains("also has a clear route"), text(out))

  test("a clear pawn route is not promoted to a legal move when that pawn now screens its king"):
    val out = project("6k1/8/8/8/1b6/8/2P5/4K3 w - - 0 1", "e1e2", "c2c3")
    assert(effects(out, "pawn_option").nonEmpty, text(out))
    assert(text(out).contains("also has a clear route"), text(out))
    assert(!text(out).contains("c4 remains available"), text(out))

  test("two choices preserving the same pawn do not create a pawn-option difference"):
    val out = project(re1, "f1e1 g7g6", "b1d2 g7g6")
    assertEquals(effects(out, "pawn_option"), Nil)

  test("Qxd4 and cxd4 use different capturers of the same victim and preserve c3 support only on one arm"):
    val out = project(qxd4, "d1d4 f6e4", "c3d4 f6d5")
    val row = effects(out, "capture_choice").headOption.getOrElse(fail(text(out)))
    assertEquals((row \ "actor" \ "square").as[String], "c3")
    assertEquals((row \ "target" \ "square").as[String], "d4")
    assert(text(out).contains("with the queen") && text(out).contains("support it on d4"), text(out))
    assert(!text(out).toLowerCase.contains("recaptures the pawn"), text(out))

  test("the relative queen pin keeps the same three pieces and first breaks at g5"):
    val out = project(bg5, "c1g5 h7h6 g5h4 g7g5 h4g3 c6e7 b5d7 d8d7 b1d2 e7g6", "b5c4 h7h6")
    val row = effects(out, "relative_pin").headOption.getOrElse(fail(text(out)))
    assertEquals((row \ "through_plies").as[List[Int]], List(4))
    assertEquals((row \ "target" \ "square").as[String], "f6")
    assertEquals((row \ "square").as[String], "d8")
    assert(text(out).contains("remains after Bh4") && text(out).contains("g5 breaks"), text(out))
    assert(text(out).contains("Ne7 (Black knight from c6)") &&
      text(out).contains("Bxd7 (White bishop from b5)") &&
      text(out).contains("Qxd7 (Black queen from d8)"), text(out))
    assert(!text(out).contains("cannot move"), text(out))

  test("missing the rear queen removes the relative-pin explanation"):
    val out = project(bg5.replace("r2qrbk1", "r3rbk1"), "c1g5 h7h6 g5h4", "b5c4 h7h6")
    assertEquals(effects(out, "relative_pin"), Nil)

  test("a truncated pin continuation never invents its dissolution"):
    val out = project(bg5, "c1g5 h7h6 g5h4", "b5c4 h7h6")
    assert(effects(out, "relative_pin").nonEmpty, text(out))
    assert(!text(out).contains("breaks that alignment"), text(out))

  test("e5 compares the same enemy pawn task and retains the bishop price and opened counterattack"):
    val out = project(e5, "e6e5 f4d5 e7d5 c4d5", "c7c4 e4e5")
    val row = effects(out, "advance_choice").headOption.getOrElse(fail(text(out)))
    assertEquals((row \ "actor" \ "square").as[String], "e4")
    assertEquals((row \ "target" \ "square").as[String], "e6")
    assert(text(out).contains("Captures the bishop on c4") && text(out).contains("Black knight on f6") &&
      text(out).contains("White knight on f4"), text(out))

  test("a short alternative does not prove the missing pawn advance happened"):
    val out = project(e5, "e6e5 f4d5", "c7c4")
    assertEquals(effects(out, "advance_choice"), Nil)

  test("Kh8 certifies only the exact frozen capture option and keeps White's actual reply"):
    val out = project(kh8, "g8h8 h6g5", "a5a4 h6g7 e8g7 f4h5")
    val row = effects(out, "capture_release").headOption.getOrElse(fail(text(out)))
    assertEquals((row \ "actor" \ "square").as[String], "g7")
    assertEquals((row \ "target" \ "square").as[String], "h6")
    assertEquals((row \ "through_plies").as[List[Int]], List(2))
    assert(text(out).contains("opponent moves first") && text(out).contains("Bg5"), text(out))

  test("Kh8 does not release gxh6 when the other diagonal still exposes the king"):
    val out = project(kh8.replace("3pp2B", "3ppB1B").replace("1B3NQ1", "5NQ1"),
      "g8h8 h6g5", "a5a4 h6g7 e8g7 f4h5")
    assert(!effects(out, "capture_release").exists(e => (e \ "target" \ "square").as[String] == "h6"), text(out))
