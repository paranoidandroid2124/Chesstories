package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import play.api.libs.json.*

/** Real reports from the two preregistered positions. Replay checks task
  * identity and ownership, never the ten-second screen time. */
class ExchangeActivityTest extends munit.FunSuite:
  override def munitTimeout = 2.minutes
  private lazy val records =
    val name = "judgment-evaluation/fixtures/public-commentary-v6/strategic-function-browser.json.gz"
    val file = List(Path.of(name), Path.of("..", name)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(file))
    val json = try Json.parse(stream.readAllBytes()) finally stream.close()
    (json \ "cases").as[List[JsObject]]

  private def read(san: String): (MovePurpose.Reading, ObservedLineProtocol.PreparedPaths) =
    val row = records.find(value => (value \ "san").as[String] == san).get
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((row \ "request").get).getBytes(UTF_8)).toOption.get
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(12, 10000L, request.engineProfile, investigateChoices = true, shortJob = true),
      1L, request.inputLines, request.proposals).toOption.get
    val serializer = PositionCommentaryJobProtocol.getClass.getDeclaredMethods.find(_.getName == "issuedEngineWorkJson").get
    serializer.setAccessible(true)
    (row \ "saved_reports").as[List[JsObject]].zipWithIndex.foreach { (saved, index) =>
      val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
      val encoded = serializer.invoke(PositionCommentaryJobProtocol, work).asInstanceOf[JsObject]
      def search(value: JsObject) = value - "work_id" - "execution_key_sha256" - "max_search_elapsed_ms"
      assertEquals(search(encoded), search((saved \ "issued_work").as[JsObject]))
      val raw = (saved \ "report").as[JsObject] ++ Json.obj("work_id" -> work.workId,
        "execution_key_sha256" -> work.executionKeySha256)
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(Json.stringify(raw).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val value = MovePurpose.read(packet, paths, state.responseSearches, MoveWorkProjection.select(packet, paths).local).get
    assertEquals(value.question.kind, "exchange_activity")
    assert(!value.belongsTo(ObservedLineProtocol.prepare(packet)))
    (value, paths)

  test("bishop exchange tracks the remaining central activity and the same displaced opponent"):
    val (value, _) = read("Bd5")
    assert(value.question.activity.nonEmpty)
    val states = value.branches.flatMap(_.activity)
    assertEquals(states.size, 2)
    assert(states.forall(_.mover.isEmpty), "the purpose must outlive the moved bishop")
    assert(states.forall(s => s.central.nonEmpty && s.retreats.nonEmpty))
    assert(value.activityContinuity.exists {
      case ExchangeActivity.Continuity.Deployment(p) => p.square.key == "e4"
      case _ => false
    })

  test("the same pawn plan does not imply the same queen counterplay"):
    val (value, paths) = read("Qc2")
    val states = value.branches.flatMap(_.activity)
    assertEquals(states.map(_.queensGone).toSet, Set(true, false))
    assert(value.activityContinuity.exists {
      case ExchangeActivity.Continuity.PawnPressure(square, targets, next) =>
        square == "f4" && targets.map(_.square.key) == List("g5") && next == "f5"
      case _ => false
    })
    val continued = value.branches.find(_.activity.get.queensGone).get.path
    assert(continued.moves.contains("g5f4") && continued.moves.contains("e3f4"))
    val lastOwnMove = (continued.moves.size - 1) / 2 * 2
    val later = ExchangeActivity.state(paths, continued, value.question.actor,
      value.question.activity.get, lastOwnMove).get
    assert(!ExchangeActivity.continuity(states.head, later, value.question.actor.side).exists {
      case ExchangeActivity.Continuity.PawnPressure("f4", _, _) => true
      case _ => false
    }, "after gxf4 exf4 another white pawn on f4 cannot restore pressure on the removed g5 pawn")
