package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import play.api.libs.json.*

/** Two development failures from the same preregistered Ne7 position.
  * Replaying reports checks meaning and ownership, never wall-clock latency. */
class RedeploymentPurposeTest extends munit.FunSuite:
  override def munitTimeout = 2.minutes
  private lazy val records =
    val name = "judgment-evaluation/fixtures/public-commentary-v6/redeployment-purpose-browser.json.gz"
    val file = List(Path.of(name), Path.of("..", name)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(file))
    val json = try Json.parse(stream.readAllBytes()) finally stream.close()
    (json \ "regressions").as[Map[String, JsObject]]

  private def read(name: String): MovePurpose.Reading =
    val row = records(name)
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((row \ "request").get).getBytes(UTF_8)).toOption.get
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(12, 10001L, request.engineProfile, investigateChoices = true),
      1L, request.inputLines, request.proposals).toOption.get
    val serializer = PositionCommentaryJobProtocol.getClass.getDeclaredMethods.find(_.getName == "issuedEngineWorkJson").get
    serializer.setAccessible(true)
    (row \ "saved_reports").as[List[JsObject]].zipWithIndex.foreach { (saved, index) =>
      val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
      val encoded = serializer.invoke(PositionCommentaryJobProtocol, work).asInstanceOf[JsObject]
      val previous = (saved \ "issued_work").as[JsObject]
      // Only the new replay job's identity/lease changes. Position, history,
      // restriction, depth, nodes and search time must be the actual search.
      def search(value: JsObject) = value - "work_id" - "execution_key_sha256" - "max_search_elapsed_ms"
      assertEquals(search(encoded), search(previous))
      val raw = (saved \ "report").as[JsObject] ++ Json.obj("work_id" -> work.workId,
        "execution_key_sha256" -> work.executionKeySha256)
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(Json.stringify(raw).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val value = MovePurpose.read(packet, paths, state.responseSearches, MoveWorkProjection.select(packet, paths).local).get
    assertEquals(value.question.plan.get.targets.map(_.square.key), List("e4"), "d5 is an immediate attack, not this long task")
    val foreign = ObservedLineProtocol.prepare(packet)
    assert(!value.belongsTo(foreign))
    assertEquals(paths.purposePosition(foreign.paths.find(_.line.ref == value.question.path.line.ref).get, 1),
      None, "a cached board cannot license a foreign path")
    value

  test("leaving and returning to e4 does not resume the original exchange task"):
    val value = read("interrupted")
    val branch = value.branches.find(_.path.moves(1) == "e4d2").get
    assert(branch.path.moves.contains("d2e4"), "the saved counterexample returns to the same square")
    assertEquals(branch.use, 2)
    assertEquals(branch.relocated.map(_._2.map(_.square.key)), List(Some("d2")))
    assertEquals(branch.pressure, None)

  test("queen pressure on the same knight is not the planned knight exchange"):
    val value = read("affordance")
    assertEquals(value.question.exchangeTarget.map(_.square.key), Some("e4"))
    val pressure = value.branches.flatMap(_.pressure).find(_.actor.role.name == "queen").get
    assertEquals(pressure.targets.map(_.square.key), List("e4"))
    assert(!pressure.sameTask)
