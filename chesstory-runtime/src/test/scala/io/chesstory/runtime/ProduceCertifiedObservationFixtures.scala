package io.chesstory.runtime

import java.nio.file.{Files, Paths}
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Runtime-produced input for the actual browser decoder and view regression tests. */
object ProduceCertifiedObservationFixtures:
  def main(args: Array[String]): Unit =
    val cases = List(
      ("pinned-recapture", "4k3/4n3/8/3p4/4P3/8/8/4R1K1 w - - 0 1", "e4d5"),
      ("double-check", "4k3/8/r7/8/8/8/4B3/4R1K1 w - - 0 1", "e2b5"),
      ("root-response", "4k3/8/8/8/8/8/4r3/4K3 w - - 0 1", "e1e2"),
      ("pawn-connections", "k7/8/8/3P4/2P5/8/8/7K w - - 0 1", "c4c5"),
      ("mate", "7k/8/5KQ1/8/8/8/8/8 w - - 0 1", "g6g7"),
      ("stalemate", "7k/5K2/8/6Q1/8/8/8/8 w - - 0 1", "g5g6"),
      ("castle", "4k3/8/8/8/8/8/8/4K2R w K - 0 1", "e1g1"),
      ("underpromotion", "1r5k/P7/8/8/8/8/8/7K w - - 0 1", "a7b8n"),
      ("moving-target", "7k/r7/8/8/8/8/8/R6K b - - 0 1", "a7a6"),
      ("Bf3", "2r1r1k1/1b3ppp/p2b4/1p1n1P2/6Pq/PNP5/1PQ3BP/R1B2R1K w - - 3 22", "g2f3"),
      ("Bc3", "2r3k1/1p1b2pp/p2p4/3P1p2/1PPbr3/P4Q2/3BBqPP/2R1R2K w - - 7 25", "d2c3"),
      ("Bc4", "8/2q4p/B1Pp1kp1/3P4/1P3p2/p6P/6P1/2R4K w - - 0 41", "a6c4"))
    val fixtures = cases.map { case (name, fen, move) =>
      val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
      Json.obj("name" -> name, "fen" -> fen, "move" -> move,
        "response" -> IndependentCommentaryExamples.wire(IndependentCommentaryExamples.start(history, move)))
    }
    Files.writeString(Paths.get("..", "judgment-evaluation", "fixtures", "public-commentary-v6",
      "certified-observations-produced.json"), Json.prettyPrint(JsArray(fixtures)) + "\n")
