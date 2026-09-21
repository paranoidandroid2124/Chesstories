package io.chesstory.runtime

import java.nio.file.{ Files, Paths }
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

private[runtime] object IndependentCommentaryExamples:
  val initial = chess.variant.Standard.initialFen.value
  val profile = CommentaryEngineProfile.ServerAdmitted
  val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
  val recaptureHistory = history.extend(List("e2e4", "d7d5", "e4d5")).toOption.get

  def start(source: CanonicalPositionHistory, move: String): CommentaryJobState =
    CommentaryJobReducer.startJob(source.initialFen, source.movePrefixUci, source.currentFen, move,
      source.extend(List(move)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile), 0L).toOption.get

  def wire(state: CommentaryJobState): JsObject = PositionCommentaryJobProtocol.projectJobSnapshot(
    CommentaryJobSnapshot("i" * 32, "request-independent", 10000L, state))

  def statements(wire: JsObject): List[JsObject] = (wire \ "observations" \ "statements").as[List[JsObject]]

  def examples: List[(String, JsObject)] =
    val development = start(history, "g1f3")
    val recapture = start(recaptureHistory, "d8d5")
    val unknown = CanonicalPositionHistory.from(recaptureHistory.currentFen, Nil, recaptureHistory.currentFen).toOption.get
    val preparationFailures = List("engine_unavailable" -> EngineWorkFailure.EngineUnavailable,
      "browser_unsupported" -> EngineWorkFailure.BrowserUnsupported).map { case (name, failure) =>
      val awaiting = development.asInstanceOf[AwaitingEngineWork]
      val stopped = CommentaryJobReducer.submitIssuedWorkReport(awaiting, EngineWorkReport.ExecutorFailed(
        profile, awaiting.issuedWork.workId, awaiting.issuedWork.executionKeySha256, failure), 1L).toOption.get
      s"development-$name-produced" -> wire(stopped)
    }
    List(
      "development-awaiting-produced" -> wire(development),
      "development-cancelled-produced" -> wire(CommentaryJobReducer.cancelJob(development)),
      "recapture-stopped-produced" -> wire(CommentaryJobReducer.cancelJob(recapture)),
      "capture-unknown-stopped-produced" -> wire(CommentaryJobReducer.cancelJob(start(unknown, "d8d5")))
    ) ++ preparationFailures

class IndependentCommentaryTest extends munit.FunSuite:
  import IndependentCommentaryExamples.*

  test("direct review returns independent development with no engine input"):
    val result = RuntimeProtocol.evaluate(Json.obj("schema_version" -> RuntimeProtocol.RequestSchema,
      "input" -> Json.obj("fen" -> initial, "playedMoveUci" -> "g1f3")))
    assertEquals(result.httpStatus, 200)
    val commentary = (result.body \ "move_commentary").as[JsObject]
    assertEquals((commentary \ "primary").toOption, None)
    val fact = statements(commentary).head
    assertEquals((fact \ "kind").as[String], "development")
    assertEquals((fact \ "central_squares").as[List[String]], List("d4", "e5"))

  test("search failure and absent prehistory preserve capture while keeping recapture knowledge distinct"):
    val root = start(recaptureHistory, "d8d5").asInstanceOf[AwaitingEngineWork]
    val before = wire(root)
    assertEquals((statements(before).head \ "recapture").as[String], "proven")
    val stopped = CommentaryJobReducer.submitIssuedWorkReport(root,
      EngineWorkReport.ExecutorFailed(profile, root.issuedWork.workId, root.issuedWork.executionKeySha256,
        EngineWorkFailure.ExecutionFailed), 1L).toOption.get
    CommentaryDeliveryPreservationAssertions.assertRootStopPreservesObservations(
      (before \ "observations").as[JsObject], (wire(stopped) \ "observations").as[JsObject],
      root.issuedWork.workId, "engine_execution_failed")
    val unknownHistory = CanonicalPositionHistory.from(recaptureHistory.currentFen, Nil, recaptureHistory.currentFen).toOption.get
    val unknown = wire(CommentaryJobReducer.cancelJob(start(unknownHistory, "d8d5")))
    val capture = statements(unknown).head
    assertEquals((capture \ "kind").as[String], "capture")
    assertEquals((capture \ "captured_role").as[String], "pawn")
    assertEquals((capture \ "recapture").as[String], "unknown")
    assertEquals((before \ "observations" \ "paths").as[List[JsObject]].find(p =>
      (p \ "kind").as[String] == "history").map(p => (p \ "moves").as[List[String]]),
      Some(List("e4d5", "d8d5")))

/** Rebuilds public examples from the runtime reducer, never from hand-authored statements. */
object ProduceIndependentCommentaryFixtures:
  def main(args: Array[String]): Unit =
    import IndependentCommentaryExamples.*
    val destination = Paths.get("..", "judgment-evaluation", "fixtures", "public-commentary-v6")
    examples.foreach { case (name, body) =>
      Files.writeString(destination.resolve(s"$name.json"), Json.prettyPrint(body) + "\n")
    }
    val root = start(history, "e2e4").asInstanceOf[AwaitingEngineWork]
    val lines = List(List("e2e4", "e7e5", "g1f3") -> 30, List("d2d4", "d7d5", "g1f3") -> 20,
      List("g1f3", "g8f6", "d2d4") -> 10).map { case (moves, score) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(score), 16)
      }
    val completed = CommentaryJobReducer.submitIssuedWorkReport(root,
      EngineWorkReport.Completed(profile, root.issuedWork.workId, root.issuedWork.executionKeySha256, lines), 1L).toOption.get
    val best = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("b" * 32, "request-best-choice", Long.MaxValue, completed))
    Files.writeString(destination.resolve("best-choice-produced.json"), Json.prettyPrint(best) + "\n")
