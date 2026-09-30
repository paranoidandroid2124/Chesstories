package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{ MoveReviewAssembler, MoveReviewInputAdmission }
import lila.chessjudgment.model.judgment.EvidenceBackedJudgmentPacket
import lila.chessjudgment.model.line.CanonicalPositionHistory

class CommentaryNotationTest extends munit.FunSuite:
  private def notation(fen: String, lines: List[List[String]]) =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, lines.head.head, lines).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    new CommentaryNotation(packet)

  test("SAN renders legal special moves and the required piece disambiguation"):
    val samples = List(
      ("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", List("e1g1", "e8c8"), List("O-O", "O-O-O")),
      ("7k/5K2/6Q1/8/8/8/8/8 w - - 0 1", List("g6g7"), List("Qg7#")),
      ("7k/8/5KQ1/8/8/8/8/8 w - - 0 1", List("g6h6"), List("Qh6+")),
      ("7k/P7/8/8/8/8/8/7K w - - 0 1", List("a7a8q"), List("a8=Q+")),
      ("7k/8/8/3pP3/8/8/8/7K w - d6 0 1", List("e5d6"), List("exd6")),
      ("7k/8/8/8/8/R7/8/R6K w - - 0 1", List("a1a2"), List("R1a2")),
      ("7k/8/8/8/8/1N6/8/1N3N1K w - - 0 1", List("b1d2"), List("Nb1d2"))
    )
    samples.foreach { (fen, moves, expected) =>
      assertEquals(notation(fen, List(moves)).line(moves), expected)
    }
    val castling = notation(samples.head._1, List(List("e1d1")))
    assertEquals(castling.move(Nil, "e1g1"), "O-O", "a castling alternative needs no returned PV")

  test("the same UCI owns a different SAN after a different exact prefix"):
    val fen = "7k/8/8/8/8/8/8/1N3N1K w - - 0 1"
    val direct = List("b1d2")
    val later = List("f1e3", "h8g8", "b1d2")
    val labels = notation(fen, List(direct, later))
    assertEquals(labels.line(direct), List("Nbd2"))
    assertEquals(labels.line(later), List("Ne3", "Kg8", "Nd2"))
    assertEquals(labels.move(Nil, "f1d2"), "Nfd2", "a legal choice needs no returned PV to have SAN")
    assertEquals(labels.move(List("f1e3", "h8g8"), "b1d2"), "Nd2")
    assertEquals(labels.move(Nil, "b1b8"), "?", "invalid moves never acquire invented SAN")
