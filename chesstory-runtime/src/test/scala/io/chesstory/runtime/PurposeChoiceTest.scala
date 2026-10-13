package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import play.api.libs.json.*

/** Replay the two actual screen reports. This checks meaning and ownership;
  * screen latency is measured by the fresh browser requests in the fixture. */
class PurposeChoiceTest extends munit.FunSuite:
  override def munitTimeout = 2.minutes
  private lazy val records =
    val name = "judgment-evaluation/fixtures/public-commentary-v6/choice-neutralization-browser.json.gz"
    val file = List(Path.of(name), Path.of("..", name)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(file))
    val json = try Json.parse(stream.readAllBytes()) finally stream.close()
    (json \ "cases").as[List[JsObject]]

  private def read(id: String): (MovePurpose.Reading, ObservedLineProtocol.PreparedPaths) =
    val row = records.find(value => (value \ "id").as[String] == id).get
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
    val value = MovePurpose.read(packet, paths, state.responseSearches, MoveWorkProjection.select(packet, paths).local,
      state.rootSearches.filter(_.stopReason.isEmpty).flatMap(_.receipt)).get
    assert(value.choice.nonEmpty)
    assert(PurposeChoice.read(packet, paths, Nil).isEmpty, "board geometry cannot substitute a completed comparison")
    val foreign = ObservedLineProtocol.prepare(packet)
    assert(!value.choice.get.belongsTo(foreign))
    assert(MovePurpose.answer(packet, foreign, value.choice.get.arms.map(_.path), value, Some(_)).isEmpty)
    (value, paths)

  test("Nd5 can neutralize the pressure by exchanging the c5 guard; Bd5 retains a different cost"):
    val (value, paths) = read("existing")
    val alternative = value.choice.get.arms(1)
    assertEquals(alternative.path.moves.take(4), List("c3d5", "f6d4", "f3d4", "e4c5"))
    assert(alternative.through <= 7, "an incidental much later retreat is not the first neutralization")
    val difference = value.neutralization.get
    assertEquals((difference.target.square.key, difference.post, difference.guard.square.key), ("e4", "c5", "d4"))
    assertEquals(difference.retainedGuard.square.key, "d4")
    assertEquals(difference.opponent.square.key, "b7")
    assert(alternative.state.targets.exists((original, current) => original.square.key == "e4" && current.exists(_.square.key == "c5")))
    assert(alternative.entry.isEmpty, "the returned Nf4 line has not exchanged away the two rook targets")
    assert(PurposeChoice.neutralization(value.choice.get, Nil, paths).isEmpty,
      "one continuation alone cannot establish a contrast with the played move")

  test("the new position connects the blocked bishop route to its actual loss, not d5 occupancy"):
    val (value, _) = read("new")
    val choice = value.choice.get
    assertEquals(choice.arms.map(_.path.moves.head), List("b3d5", "b6d5"))
    assert(choice.escaped.exists(p => p.role.name == "queen" && p.square.key == "b1"))
    val alternative = choice.arms(1)
    assert(alternative.blocksRoute)
    assert(alternative.constrained.exists { (_, route) =>
      route.open.isEmpty && route.contested.nonEmpty &&
        route.blocked.exists((square, piece) => square == "d5" && piece.role.name == "knight")
    })
    assert(alternative.retained.current.isEmpty, "the original b3 bishop is actually lost in this line")
    assert(alternative.path.moves.take(alternative.through + 1).sliding(2).exists(_ == List("b3a4", "a1a4")))
    assert(value.neutralization.isEmpty, "this route restriction is not the old c5-guard mechanism")
