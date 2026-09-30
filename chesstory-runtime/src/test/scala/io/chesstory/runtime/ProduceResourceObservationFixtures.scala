package io.chesstory.runtime

import java.nio.file.{Files, Paths}
import lila.chessjudgment.model.line.CanonicalPositionHistory
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

object ProduceResourceObservationFixtures:
  def main(args: Array[String]): Unit =
    val input = scala.util.Using.resource(getClass.getResourceAsStream("/resource-cases.json"))(stream => Json.parse(stream))
    val sourceCases = (input \ "cases").as[List[JsObject]]
    val resources = sourceCases.map(row =>
      ((row \ "id").as[String], (row \ "before_fen").as[String], (row \ "move").as[String], List.empty[List[String]]))
    val initial = chess.variant.Standard.initialFen.value
    val recovery = List("e2e4", "d7d5", "b1c3", "d5e4", "c3e4")
    val lines = List(
      ("unchanged-pawn-states", "k7/8/8/3P4/2P5/8/8/7K w - - 0 1", "h1g1", List.empty[List[String]]),
      ("rook-enters-open-file", "7k/8/8/8/8/8/P7/R6K w - - 0 1", "a1e1", List.empty[List[String]]),
      ("rook-enters-black-pawns-only-file", "7k/4p3/8/8/8/8/P7/R6K w - - 0 1", "a1e1", List.empty[List[String]]),
      ("rook-enters-white-pawns-only-file", "7k/8/8/8/8/8/P3P3/R6K w - - 0 1", "a1e1", List.empty[List[String]]),
      ("rook-enters-both-sides-file", "7k/4p3/8/8/8/8/P3P3/R6K w - - 0 1", "a1e1", List.empty[List[String]]),
      ("no-pawns-after-capture", "7k/8/8/8/8/8/p7/K7 w - - 0 1", "a1a2", List.empty[List[String]]),
      ("knight-pinned", "4k3/4n3/8/8/8/8/7P/4R1K1 w - - 0 1", "h2h3", List.empty[List[String]]),
      ("knight-released", "4k3/4n3/8/8/8/8/7P/4R1K1 w - - 0 1", "e1a1", List.empty[List[String]]),
      ("finite-material-branches", initial, "e2e4", List(
        List("e2e4", "d7d5", "e4d5", "d8d5"), recovery.take(4), recovery,
        recovery ++ List("d8d5", "e4c3", "d5d2"))),
      ("quiet-line", initial, "g1f3", List(List("g1f3", "g8f6"))))
    val fixtures = (resources ++ lines).map { case (name, fen, move, paths) =>
      val source = sourceCases.find(row => (row \ "id").as[String] == name)
      val initialFen = source.flatMap(row => (row \ "initial_fen").asOpt[String]).getOrElse(fen)
      val prefix = source.flatMap(row => (row \ "move_prefix").asOpt[List[String]]).getOrElse(Nil)
      val currentFen = source.flatMap(row => (row \ "runtime_fen").asOpt[String]).getOrElse(fen)
      val history = CanonicalPositionHistory.from(initialFen, prefix, currentFen)
        .fold(error => throw IllegalArgumentException(s"$name history: $error"), identity)
      val state = CommentaryJobReducer.startJob(history.initialFen, history.movePrefixUci, history.currentFen, move,
        history.extend(List(move)).fold(error => throw IllegalArgumentException(s"$name move: $error"), identity).currentFen,
        CommentaryJobPolicy(16, 10000L, IndependentCommentaryExamples.profile), 0L,
        paths.map(SuppliedCommentaryLine(CommentaryLineSource.Analysis, _)))
        .fold(error => throw IllegalArgumentException(s"$name job: $error"), identity)
      Json.obj("name" -> name, "fen" -> history.currentFen, "move" -> move,
        "response" -> IndependentCommentaryExamples.wire(CommentaryJobReducer.cancelJob(state)))
    }
    val json = Json.prettyPrint(JsArray(fixtures)) + "\n"
    Files.writeString(Paths.get("..", "judgment-evaluation", "fixtures", "public-commentary-v6", "resources-produced.json"), json)
