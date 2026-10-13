package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import play.api.libs.json.*

/** Real reports from the two preregistered positions. Replay checks task
  * identity and ownership, never the ten-second screen time. */
class SupportPurposeTest extends munit.FunSuite:
  override def munitTimeout = 2.minutes
  private lazy val records =
    val name = "judgment-evaluation/fixtures/public-commentary-v6/strategic-purpose-browser.json.gz"
    val file = List(Path.of(name), Path.of("..", name)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(file))
    val json = try Json.parse(stream.readAllBytes()) finally stream.close()
    (json \ "semantic_controls").as[List[JsObject]]

  private def read(san: String): MovePurpose.Reading =
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
    assertEquals(value.question.kind, "support_release")
    assert(!value.belongsTo(ObservedLineProtocol.prepare(packet)))
    val task = value.question.support.get
    assert(paths.purposePosition(value.question.path, 0).get.view.legalRows.get.exists(_.move == task.release.move),
      "the root move shares the support duty; it does not make the rook transfer newly legal")
    value

  test("Bd3 preserves the pawn duty through a new bishop post and through a different defender"):
    val value = read("Bd3")
    assertEquals(value.question.support.get.asset.square.key, "e2")
    val kept = value.branches.find(_.path.moves(1) == "b3c2").get.support.get
    assert(kept.sameDuty && kept.blockaderRemains)
    assertEquals(kept.provider.map(_.square.key), Some("c4"), "the bishop moved, but kept its task")
    val changed = value.branches.find(_.path.moves(1) == "c3c4").get.support.get
    assert(changed.sameDuty && changed.blockaderRemains)
    assertEquals(changed.provider, None, "the bishop no longer performs this duty")
    assertEquals(changed.guards.map(_.square.key), List("e8"), "the other rook continues the duty")

  test("Ke6 preparation can become king support or remain pending while the blockade changes"):
    val value = read("Ke6")
    assertEquals(value.question.square, "d5")
    assertEquals(value.question.support.get.asset.square.key, "d4")
    val realized = value.branches.find(_.path.moves(1) == "d2d1").get.support.get
    assert(realized.sameDuty && realized.blockaderRemains)
    assertEquals(realized.provider.map(_.square.key), Some("d5"))
    assertEquals(realized.activity.map(_.role.name), Some("rook"))
    val checked = value.branches.find(_.path.moves(1) == "f4f5").get.support.get
    assert(checked.sameDuty)
    assert(!checked.blockaderRemains, "the same supported pawn does not imply the same restriction on White's rook")
    assertEquals(checked.provider, None)
    assertEquals(checked.guards.map(_.square.key), List("d5"))
    assertEquals(checked.activity.map(_.role.name), Some("queen"))
