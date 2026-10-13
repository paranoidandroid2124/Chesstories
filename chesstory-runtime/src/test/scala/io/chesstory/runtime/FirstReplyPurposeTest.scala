package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import play.api.libs.json.*

/** Two automatic requests outside Nb1/Nc4; all receipts keep their actual work owners and depths. */
class FirstReplyPurposeTest extends munit.FunSuite:
  override def munitTimeout = 2.minutes
  private lazy val records =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/first-reply-purpose-produced.json.gz"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    val data = try Json.parse(stream.readAllBytes()) finally stream.close()
    (data \ "cases").as[List[JsObject]].map(value => (value \ "game_id").as[String] -> value).toMap

  private def summary(id: String): JsObject =
    val record = records(id)
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get
    assertEquals(request.proposals, Nil)
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 180000L, request.engineProfile, investigateChoices = true,
        previewDepth = Some(8)), 1L, request.inputLines, request.proposals).toOption.get
    val serializer = PositionCommentaryJobProtocol.getClass.getDeclaredMethods.find(
      _.getName == "issuedEngineWorkJson").get
    serializer.setAccessible(true)
    (record \ "saved_reports").as[List[JsObject]].zipWithIndex.foreach { (saved, index) =>
      val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
      assert(work.requiredDepth <= 18)
      assertEquals(serializer.invoke(PositionCommentaryJobProtocol, work), (saved \ "issued_work").get,
        s"receipt $id/$index must belong to the entire issued work descriptor")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    assert(state.isInstanceOf[CompletedCommentaryJob])
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("f" * 32, request.requestId, 180000L, state))
    val primary = (wire \ "result" \ "selected_move_reviews").as[List[JsObject]].find(value =>
      (value \ "selection" \ "roles").as[List[String]].contains("played")).get
      .apply("commentary").as[JsObject].apply("primary")
    assertEquals(primary, (record \ "baseline_primary").get)
    val detail = (wire \ "observations" \ "selection" \ "candidates").as[List[JsObject]].filter(value =>
      (value \ "selection").as[String] == "selected").map(value => (value \ "detail").as[JsObject])
      .find(value => (value \ "summary_question" \ "question").asOpt[String].contains("first_target_response")).get
    assert((detail \ "summary_question" \ "first_target_response").asOpt[JsObject].nonEmpty)
    List("units", "detail_units").foreach { field =>
      val units = (detail \ "summary_question" \ field).as[List[JsObject]]
      assert(units.size <= 8)
      assert(units.flatMap(unit => (unit \ "proof_moves").as[List[JsObject]])
        .map(ref => (ref \ "line_id").as[String]).distinct.size <= 7)
    }
    val output = Path.of("target/research-runs/first-reply-purpose-20261011")
    Files.createDirectories(output)
    Files.writeString(output.resolve(id + "-verified.json"), Json.prettyPrint(detail), UTF_8)
    detail

  test("Ne4 explains the knight attack, the preferred exchange and the declined capture's reinforcement"):
    val text = (summary("FJ-SG35-LATE-NE4") \ "summary").as[String]
    assert(text.contains("knight on e4 now targets White's knight on g3"), text)
    assert(text.contains("Bxe4 is the stronger tested reply than Qe2"), text)
    assert(text.contains("Bxe4 captures the black knight on e4") &&
      text.contains("fxe4 recaptures the white bishop on e4"), text)
    assert(text.contains("After fxe4, Black's pawn on e4 now targets White's queen on d3"), text)
    assert(text.contains("Qe2 Ncd6") && text.contains("knight on d6 supports Black's knight on e4"), text)
    assert(!text.contains("The recorded continuation is Nxe4"), "the old third response belongs in detail")

  test("Qxd7 explains the rook recapture, escaping with a bishop capture and the inferior queen exchange"):
    val text = (summary("FJ-SG17-QXD7") \ "summary").as[String]
    assert(text.contains("Qxd7 recaptures the white rook on d7") &&
      text.contains("queen on d7 now targets White's queen on b5"), text)
    assert(text.contains("Qxe5 is the stronger tested reply than Qxd7+"), text)
    assert(text.contains("Qxe5 captures the black bishop on e5") &&
      text.contains("Qxe5 moves the queen out of the attack from Black's queen on d7"), text)
    assert(text.contains("Qxd7+ recaptures the black queen on d7") &&
      text.contains("Bxd7 recaptures the white queen on d7"), text)
    assert(text.indexOf("stronger tested reply") < text.indexOf("gives check"), text)
