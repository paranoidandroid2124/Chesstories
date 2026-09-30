package io.chesstory.runtime

import java.nio.file.{ Files, Paths }
import lila.chessjudgment.model.judgment.{ CommentaryLineSource, SuppliedCommentaryLine }
import play.api.libs.json.*

private[runtime] object SuppliedCommentaryExamples:
  val prefix = "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4 g8f6 b1c3 a7a6 f1c4 e7e6 c4b3 b7b5 e1g1 f8e7 d1f3 d8c7 f3g3 e8g8 c1h6 f6e8 a1d1 c8d7 d4f3".split(" ").toList
  val history = IndependentCommentaryExamples.history.extend(prefix).toOption.get
  val shared = List("b8c6", "h6f4", "b5b4", "e4e5")
  val pawnCapture = shared ++ List("c6e5", "f3e5", "d6e5", "f4e5", "c7b7")
  val knightCapture = shared ++ List("d6e5", "f3e5", "c6e5", "f4e5", "c7b7")
  val lines = List(pawnCapture, knightCapture).map(SuppliedCommentaryLine(CommentaryLineSource.Analysis, _))

  def start(inputs: List[SuppliedCommentaryLine]): CommentaryJobState =
    CommentaryJobReducer.startJob(history.initialFen, history.movePrefixUci, history.currentFen, "b8c6",
      history.extend(List("b8c6")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, IndependentCommentaryExamples.profile), 0L, inputs).toOption.get

  def wire(state: CommentaryJobState): JsObject = PositionCommentaryJobProtocol.projectJobSnapshot(
    CommentaryJobSnapshot("p" * 32, "request-supplied-paths", 10000L, state))

class SuppliedCommentaryLinesTest extends munit.FunSuite:
  import SuppliedCommentaryExamples.*

  test("supplied transpositions preserve shared history and different Bxe5 targets through cancellation"):
    val active = start(lines)
    val json = wire(active)
    val paths = (json \ "observations" \ "paths").as[List[JsObject]]
    def path(moves: List[String]) = paths.find(p => (p \ "moves").as[List[String]] == moves).get
    val a = (path(pawnCapture) \ "occurrence_ids").as[List[String]]
    val b = (path(knightCapture) \ "occurrence_ids").as[List[String]]
    assertEquals(a.take(4), b.take(4))
    assert(a.drop(4).zip(b.drop(4)).forall { case (left, right) => left != right })
    assertEquals(history.extend(pawnCapture).toOption.get.currentFen, history.extend(knightCapture).toOption.get.currentFen)
    val facts = (json \ "observations" \ "statements").as[List[JsObject]]
    def captured(id: String) = (facts.find(f => (f \ "occurrence_id").as[String] == id).get \ "captured_role").as[String]
    assertEquals(captured(a(7)), "pawn")
    assertEquals(captured(b(7)), "knight")
    assertEquals((path(pawnCapture) \ "sources").as[List[String]], List("analysis"))
    CommentaryDeliveryPreservationAssertions.assertRootStopPreservesObservations(
      (json \ "observations").as[JsObject],
      (wire(CommentaryJobReducer.cancelJob(active)) \ "observations").as[JsObject],
      active.asInstanceOf[AwaitingEngineWork].issuedWork.workId, "cancelled")

  test("invalid tail preserves its admitted prefix and the other complete branch with a scoped issue"):
    val broken = SuppliedCommentaryLine(CommentaryLineSource.Study, pawnCapture.take(6) :+ "a1a8")
    val json = wire(start(lines :+ broken))
    val observations = (json \ "observations").as[JsObject]
    assertEquals((observations \ "input_issues").as[List[JsObject]], List(Json.obj(
      "input_line_index" -> 2, "accepted_ply_count" -> 6, "reason" -> "invalid_continuation")))
    val paths = (observations \ "paths").as[List[JsObject]]
    assert(paths.exists(p => (p \ "moves").as[List[String]] == broken.moves.take(6)))
    assert(paths.exists(p => (p \ "moves").as[List[String]] == knightCapture))

  test("PGN origin and later moves survive engine assessment assembly without becoming engine premises"):
    import IndependentCommentaryExamples.{ history as initialHistory, profile }
    val record = SuppliedCommentaryLine(CommentaryLineSource.PgnRecord, List("e2e4", "d7d5", "e4d5", "d8d5"))
    val active = CommentaryJobReducer.startJob(initialHistory.initialFen, Nil, initialHistory.currentFen,
      "e2e4", initialHistory.extend(List("e2e4")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile), 0L, List(record)).toOption.get.asInstanceOf[AwaitingEngineWork]
    val report = List(List("e2e4", "e7e5") -> 30, List("d2d4", "d7d5") -> 20, List("g1f3", "g8f6") -> 10)
      .map { case (moves, score) => ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(score), 16) }
    val completed = CommentaryJobReducer.submitIssuedWorkReport(active,
      EngineWorkReport.Completed(profile, active.issuedWork.workId, active.issuedWork.executionKeySha256, report), 1L).toOption.get
    val before = (wire(active) \ "observations" \ "paths").as[List[JsObject]]
      .find(p => (p \ "moves").as[List[String]] == record.moves).get
    val after = (wire(completed) \ "observations" \ "paths").as[List[JsObject]]
      .find(p => (p \ "moves").as[List[String]] == record.moves).get
    assertEquals(after, before)
    assertEquals((after \ "sources").as[List[String]], List("pgn_record"))

  test("an automatic terminal closes only its supplied branch and preserves a different continuation"):
    import IndependentCommentaryExamples.{ history as initialHistory, profile }
    val ended = SuppliedCommentaryLine(CommentaryLineSource.PgnRecord,
      List("f2f3", "e7e5", "g2g4", "d8h4", "b1c3"))
    val other = SuppliedCommentaryLine(CommentaryLineSource.Analysis, List("f2f3", "e7e5", "e2e4"))
    val active = CommentaryJobReducer.startJob(initialHistory.initialFen, Nil, initialHistory.currentFen,
      "f2f3", initialHistory.extend(List("f2f3")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile), 0L, List(ended, other)).toOption.get
    val observations = (wire(active) \ "observations").as[JsObject]
    assertEquals((observations \ "input_issues").as[List[JsObject]], List(Json.obj(
      "input_line_index" -> 0, "accepted_ply_count" -> 4, "reason" -> "automatic_terminal")))
    val moves = (observations \ "paths").as[List[JsObject]].map(p => (p \ "moves").as[List[String]])
    assert(moves.contains(ended.moves.take(4)))
    assert(moves.contains(other.moves))

object ProduceSuppliedCommentaryFixtures:
  def main(args: Array[String]): Unit =
    import SuppliedCommentaryExamples.*
    val destination = Paths.get("..", "judgment-evaluation", "fixtures", "public-commentary-v6")
    Files.writeString(destination.resolve("transposed-captures-produced.json"), Json.prettyPrint(wire(start(lines))) + "\n")
    val broken = SuppliedCommentaryLine(CommentaryLineSource.Study, pawnCapture.take(6) :+ "a1a8")
    Files.writeString(destination.resolve("partial-input-produced.json"), Json.prettyPrint(wire(start(lines :+ broken))) + "\n")
