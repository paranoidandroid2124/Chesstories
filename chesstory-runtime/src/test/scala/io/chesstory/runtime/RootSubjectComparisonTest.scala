package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.*

/** Previously opened recorded lines, without an engine or corpus discovery. */
class RootSubjectComparisonTest extends munit.FunSuite:
  import CommentaryAnswer.*
  private case class Example(fen: String, rows: List[String])
  private val knight = Example("2rq1rk1/1bp1bppp/p3pn2/8/Qn1P4/2B2NP1/1P1NPPBP/R4RK1 b - - 1 16", List(
    "b4d5 c3a5 d8d7 a4c4 c7c5 f3e5 d7d6 c4b3 d6b8 e2e4 d5b4 d4c5 e7c5 a5b4",
    "b7c6 a4a5 c8b8 f1c1 f6e4 g2f1 e4d2 f3d2 h7h6 e2e3 d8d7 a5h5 c6b5 d2c4 b5c4"))
  private val support = Example("1rb1rbk1/1pq2ppp/p1nppn2/8/P2NPP2/1PN2B2/2PQ1BPP/3R1R1K b - - 4 16", List(
    "f6d7 f2g3 c6d4 d2d4 b7b5 e4e5 b5a4 e5d6 f8d6 d4d6 c7c3 b3a4 c3c2 f4f5 e6e5 d1c1 c2a4",
    "c8d7 d4e2 e8d8 g2g4 b7b5 f2h4 f8e7 g4g5 f6e8 f4f5 c6e5 f3g2 b8c8 a4b5 a6b5 h4f2 b5b4 f5e6 f7e6 c3a2 c7c2"))
  private val bishop = Example("r2q1rk1/3bbppp/p1np1n2/1ppBp3/4P3/P2PNN1P/1PP2PP1/R1BQR1K1 w - - 8 16", List(
    "d5b3 c6d4 f3d4 e5d4 e3d5 d7e6 d5e7 d8e7 c1g5 h7h6 g5h4 e6b3 c2b3 e7e6 f2f4",
    "c2c3 f6d5 e3d5 d7e6 d3d4 c5d4 d5e7 c6e7 c3d4 e7g6 c1e3 h7h6 a1c1 a8c8 d1d2 f8e8 d4d5 e6d7 d2b4"))

  private def prepare(example: Example) =
    val lines = example.rows.map(row => EngineLine(row.split(" ").toList, 0, None, 16))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = example.fen,
      playedMoveUci = lines.head.moves.head, variations = lines)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = lines.map(line => paths.paths.find(_.moves == line.moves).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    (packet, paths, pair, positions)

  private def completed(example: Example) =
    val (packet, paths, pair, positions) = prepare(example)
    val selected = RootSubjectComparison.answer(packet, paths, pair, positions).getOrElse(fail("no comparison"))
    val answer = MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, Nil, selected)
      .getOrElse(fail("comparison prices could not complete"))
    assertEquals(answer.comparison, selected.comparison, "completion retains the selected question scope")
    val delivered = RootQuestionSummary.realize(paths, pair, answer).getOrElse(fail("no delivered comparison"))
    val covered = answer.core.flatMap(_.covered)
    assertEquals(covered.distinct.size, covered.size, "each actual capture has one core owner")
    (pair, answer, delivered)

  test("protecting a root use does not promote an unrelated end-position fact from its bundle") {
    val example = Example("7k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1",
      List("f1e1 h8h7 c3b5 f6e4 e1e4", "g1h1 h8h7"))
    val (packet, paths, pair, positions) = prepare(example)
    val selected = RootSubjectComparison.answer(packet, paths, pair, positions).get
    val path = pair.head
    val rook = CommentaryFact.Piece("white", "rook", "f1").witness
    val pawn = CommentaryFact.Piece("white", "pawn", "e4").witness
    val use = LineContinuationInvestigation.directSupportUse(packet, path, paths, path.moves.size, rook, pawn).uses.head
    val rootUse = SupportedCapture(RootQuestionSummary.MoveRef(path, 0), use.provider, pawn, use, true, None, None)
    val background = PiecePresence(RootQuestionSummary.MoveRef(path, 4),
      CommentaryFact.Piece("black", "king", "h8").witness, CommentaryFact.Piece("black", "king", "h7").witness)
    val protectedAnswer = RootSubjectComparison.protect(selected, List(Section(List(rootUse, background))))
    assert(protectedAnswer.core.flatMap(_.statements).contains(rootUse))
    assert(!protectedAnswer.core.flatMap(_.statements).contains(background))
    assert(protectedAnswer.details.flatMap(_.statements).contains(background))
    val notes = MoveWorkProjection.select(packet, paths).local
    val completed = MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, protectedAnswer).get
    assert(Set(3, 4).subsetOf(completed.core.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet))
    assert(RootQuestionSummary.realize(paths, pair, completed).nonEmpty)
  }

  test("root evasion compares the original threatened knight with its actual loss and retention") {
    val (pair, answer, delivered) = completed(knight)
    val subjects = answer.core.flatMap(_.statements.collect { case value: ComparisonSubject => value.originals }).flatten
    assert(subjects.exists(piece => piece.role.name == "knight" && piece.square.key == "b4"))
    assert(answer.core.flatMap(_.covered).contains(RootQuestionSummary.MoveRef(pair.head, 13)))
    assert(!answer.core.flatMap(_.covered).exists(ref => (ref.path eq pair.last) && Set(6, 7, 14)(ref.offset)),
      "another knight's exchanges do not become the b4 knight's price")
    assert(delivered.text.contains("originally on b4"))
  }

  test("central knight use retains the queen's supporting departure and the other knight's loss") {
    val (pair, answer, _) = completed(support)
    val covered = answer.core.flatMap(_.covered).filter(_.path eq pair.head).map(_.offset).toSet
    assert(Set(2, 3, 7, 8, 9, 10).subsetOf(covered), "the selected use retains its actual price")
    assert(!covered(6) && !covered(11), "an independent pawn trade does not widen this price interval")
    assert(answer.core.flatMap(_.statements).exists {
      case value: ControlState => value.move == RootQuestionSummary.MoveRef(pair.head, 9) && !value.present
      case _ => false
    })
    val reference = answer.core.flatMap(_.covered).filter(_.path eq pair.last).map(_.offset).toSet
    assert(Set(17, 18, 20).subsetOf(reference), "the delayed evasion and its opened capture ray retain their prices")
  }

  test("bishop comparison retains the first reply's fork and the used recapturer's later loss in both arms") {
    val (pair, answer, _) = completed(bishop)
    val covered = answer.core.flatMap(_.covered)
    assert(Set(2, 3, 6, 7, 11, 12).subsetOf(covered.filter(_.path eq pair.head).map(_.offset).toSet))
    assert(Set(1, 2, 6, 7).subsetOf(covered.filter(_.path eq pair.last).map(_.offset).toSet))
    assert(!covered.contains(RootQuestionSummary.MoveRef(pair.last, 8)), "a neighbouring pawn capture is a separate question")
  }

  test("a root-only alternative cannot be counted as an observed successful response") {
    val (packet, paths, pair, positions) = prepare(knight.copy(rows = List(knight.rows.head, "b7c6")))
    assertEquals(RootSubjectComparison.answer(packet, paths, pair, positions), None)
  }

  test("identical victim and price do not differ merely because the root capturer differs") {
    val example = Example("4k3/8/8/8/8/3n4/8/3Q1BK1 w - - 0 1", List("d1d3 e8e7", "f1d3 e8e7"))
    val (packet, paths, pair, positions) = prepare(example)
    assertEquals(RootSubjectComparison.answer(packet, paths, pair, positions), None)
  }

  test("equal first exchanges retain the initial contact's later knight loss and other-square compensation") {
    val example = Example("r6k/3n2pp/p1Np1r1b/1p1Pp3/PQ2BB2/5P2/1PP1q3/1K1R2R1 b - - 0 24", List(
      "e5f4 g1e1 e2c4 b4a5 b5a4 a5c7 d7e5 e1h1 a8f8 c7e7 e5g6 e4g6 h7g6 c6d8 a4a3 d8e6 f6f7 e7d6 f8c8 d1d2 a3b2",
      "h6f4 b4a5 f4e3 g1e1 e2f2 a5c7 d7c5 e1g1 e3h6 d1f1 f2e2 c6b8 f6f8 c7d6 a8b8 d6c5 b5a4 d5d6 e2b5 c5b5 a6b5 f1d1 b5b4 e4c6"))
    val (pair, answer, _) = completed(example)
    val covered = answer.core.flatMap(_.covered)
    assert(Set(11, 12).subsetOf(covered.filter(_.path eq pair.head).map(_.offset).toSet))
    assert(Set(14, 15).subsetOf(covered.filter(_.path eq pair.last).map(_.offset).toSet),
      "the actual Raxb8 and Qxc5 are part of the same initial defensive comparison")
  }

  test("sparse prices coexist with a protected root use without becoming a false interval total") {
    val examples = List(
      Example("r3qrk1/pp3pbp/3Q2p1/8/4p1b1/2N1B3/PPPR1PPP/2K4R b - - 3 16", List(
        "a8c8 c3d5 c8d8 d5f6 g7f6 d6f6 e8e6 f6e6 g4e6 h1d1 d8d2 d1d2 f7f6 e3a7 f8a8 d2d6 g8f7 a7d4 a8a2 d6b6 e6d5 b6f6 f7e7 d4c3",
        "g7c3 b2c3 a8d8 d6f6 e8e6 f6e6 g4e6 h1d1 d8d2 d1d2 f7f5 e3a7 e6a2 d2d7 f8f7 d7d1 a2e6 a7e3")),
      Example("r2qk2r/pb2bppp/1np1p3/4P3/3P4/1QNBPN2/P5PP/R4RK1 b kq - 0 16", List(
        "e8g8 c3e4 c6c5 e4c5 e7c5 d4c5 b7f3 f1f3 b6d7 a1d1 d8c7 d3h7 g8h7 b3d3 h7g8 f3h3 g7g6 d3d7 c7d7 d1d7 f8c8 d7d4 c8c5",
        "a8b8 a1b1 e8g8 b3c2 g7g6 c3e4 b6d7 c2a4")))
    examples.zip(List(Set(13), Set(3, 4, 5, 11, 12))).foreach { (example, needed) =>
      val (packet, paths, pair, _) = prepare(example)
      val root = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]]
        .find(value => (value \ "detail" \ "type").as[String] == "root_comparison").get
      val question = (root \ "detail" \ "summary_question").asOpt[JsObject]
        .getOrElse(fail("a verified sparse price must still deliver"))
      val covered = (question \ "units").as[List[JsObject]].flatMap(unit => (unit \ "covered_moves").as[List[JsObject]])
      assertEquals(covered.distinct.size, covered.size, "protection and price completion keep one capture owner")
      needed.foreach(offset => assert(covered.contains(RootQuestionSummary.MoveRef(pair.head, offset).json),
        "a selected later use retains the observed loss or earlier clearance exchange: " + offset))
      assert(!(root \ "detail" \ "summary_unavailable").isDefined)
    }
  }

  test("the later loss of a knight using a vacated square remains a basic price") {
    val example = Example("2rq1rk1/pp1b1p2/2n1pn2/2b3p1/4P2B/2N3P1/PP2QPBP/R4RK1 w - - 0 16", List(
      "h4g5 e6e5 e2d2 c5e7 g5h6 g8h7 h6f8 d8f8 a1c1 c6d4 g1h1 e7d6 c3e2 d6c5 e2d4 c5d4 c1c8 d7c8 d2g5 f8g7 f2f4 g7g5 f4g5",
      "e4e5 c6d4 e2d1 g5h4 e5f6 d8f6 c3e4 f6g7 g3h4 d7c6 g1h1 f7f5 a1c1 f5e4 c1c5"))
    val (packet, paths, pair, _) = prepare(example)
    val detail = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "type").as[String] == "root_comparison").get.apply("detail")
    val text = (detail \ "summary").as[String]
    List("e5 clears e4", "Ne4", "fxe4", "white knight on e4").foreach(value => assert(text.contains(value), text))
    val covered = (detail \ "summary_question" \ "units").as[List[JsObject]]
      .flatMap(unit => (unit \ "covered_moves").as[List[JsObject]])
    assert(covered.contains(RootQuestionSummary.MoveRef(pair.last, 13).json))
  }
