package io.chesstory.runtime

import java.nio.file.{Files, Paths}
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Synthetic completed reports exercise the public contract, not engine quality. */
object ProduceCommentarySelectionFixtures:
  def main(args: Array[String]): Unit =
    val fen = chess.variant.Standard.initialFen.value
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val cases = List(
      ("material-counts-differ", List("e2e4", "d7d5", "e4d5", "d8d5"), "different"),
      ("material-counts-equal", List("e2e4", "e7e5"), "equal"))
    val fixtures = cases.zipWithIndex.map { case ((name, played, relation), index) =>
      val policy = CommentaryJobPolicy(16, Long.MaxValue, CommentaryEngineProfile.ServerAdmitted)
      val root = CommentaryJobReducer.startJob(fen, Nil, fen, played.head,
        history.extend(List(played.head)).toOption.get.currentFen, policy, nowEpochMs = 0L)
        .toOption.collect { case value: AwaitingEngineWork => value }.get
      val work = root.issuedWork
      // The existing unrestricted root contract requires three reported heads.
      val lines = List(List("d2d4", "d7d5") -> 80, played -> 0, List("g1f3", "g8f6") -> -10)
      val report = EngineWorkReport.Completed(work.engineProfile, work.workId, work.executionKeySha256,
        lines.map { case (moves, score) =>
          ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(score), work.requiredDepth)
        })
      val completed = CommentaryJobReducer.submitIssuedWorkReport(root, report, nowEpochMs = 1L)
        .toOption.collect { case value: CompletedCommentaryJob => value }.get
      val response = RuntimeProtocol.encodePositionCommentaryResponse(
        CommentaryJobSnapshot((index + 1).toString * 32, name, Long.MaxValue, completed), completed)
      val selection = (response \ "observations" \ "selection").as[JsObject]
      assert((selection \ "version").as[Int] == 2)
      val comparison = (selection \ "candidates").as[List[JsObject]]
        .find(row => (row \ "kind").as[String] == "comparison").get
      assert((comparison \ "state").toOption.isEmpty)
      assert((comparison \ "detail" \ "type").as[String] == "root_comparison")
      assert((comparison \ "detail" \ "material_relation").as[String] == relation)
      assert((comparison \ "detail" \ "material_unavailable_reason").get == JsNull)
      Json.obj("name" -> name, "fen" -> fen, "move" -> played.head, "response" -> response,
        "note" -> "synthetic engine reports; contract only")
    }
    Files.writeString(Paths.get("..", "judgment-evaluation", "fixtures", "public-commentary-v6",
      "commentary-selection-produced.json"), Json.prettyPrint(JsArray(fixtures)) + "\n")
