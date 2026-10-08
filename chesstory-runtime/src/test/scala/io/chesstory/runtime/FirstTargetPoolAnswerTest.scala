package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import play.api.libs.json.*

/** An unchanged, actually executed first-child pool must answer its owned
  * target pair even when the explicit prefix and default prefix serialize
  * the same child. A ready receipt does not answer an omitted legal reply.
  */
class FirstTargetPoolAnswerTest extends munit.FunSuite:
  private def replay(): CommentaryJobState =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/first-target-full-pool-produced.json.gz"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    val fixture = try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()
    val record = (fixture \ "cases").as[List[JsObject]].head
    assertEquals((record \ "actual_engine_execution").as[Boolean], true)
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get
    val saved = (record \ "saved_reports").as[List[JsObject]]
    assertEquals(saved.size, 5)
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 180001L, request.engineProfile,
        investigateChoices = true, previewDepth = Some(8)), 1L,
      request.inputLines, request.proposals).toOption.get
    saved.zipWithIndex.foreach { (entry, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"the original report $index requires its exact work")
      val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("e" * 32, request.requestId, 180001L, state))
      assertEquals((wire \ "issued_engine_work").as[JsObject], (entry \ "issued_work").as[JsObject],
        "the entire work/history/profile/key owner must match before accepting an actual receipt")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((entry \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    state

  test("actual full first pool answers its Nd5 capture and decline at the effective owned child"):
    val state = replay()
    val inquiry = state.responseInquiries.find(inquiry => inquiry.question.rootMove == "c3d5" &&
      inquiry.question.originalTarget.square.key == "c3").get
    assertEquals(inquiry.state, "answered")
    assertEquals(inquiry.stopReason, None)
    val answer = inquiry.answer.get
    assertEquals(answer.pair.workId, "work:4")
    assertEquals(answer.pair.requiredDepth, 16)
    assertEquals(answer.searchPositionId, inquiry.question.childPositionId)
    assertEquals(answer.plyOffset, 1)
    val search = state.responseSearches.find(_.work.workId == answer.pair.workId).get
    val defaultAnchor = EngineSearchAnchor(inquiry.question.rootMove,
      inquiry.question.rootMoveOccurrenceId, inquiry.question.childPositionId)
    assert(search.anchor != defaultAnchor, "the original structural equality must fail on this unchanged actual input")
    assertEquals(search.anchor.prefixMoves, defaultAnchor.prefixMoves)
    assertEquals(search.work.executionKeySha256, answer.pair.executionKeySha256)
    assertEquals(Set(inquiry.question.referenceMove, inquiry.question.alternativeMove), Set("e6d5", "c5c6"))
    assertEquals(search.pairAnswer(state.observationsPacket.get,
      inquiry.question.referenceMove, inquiry.question.alternativeMove), Some(answer.pair))
    assertEquals(inquiry.readingOwner, state.observationsPacket)
    assert(inquiry.reading.exists(reading => reading.complete &&
      reading.lineIds == List(answer.pair.referenceLineId, answer.pair.alternativeLineId)))

  test("a legal omitted target reply cannot borrow the completed pool pair"):
    val state = replay()
    val inquiry = state.responseInquiries.find(inquiry => inquiry.question.rootMove == "c3d5" &&
      inquiry.question.originalTarget.square.key == "c3").get
    assert(inquiry.question.captureMoves.contains("c5d5"), "the original rook capture remains legal")
    val search = state.responseSearches.find(_.work.workId == "work:4").get
    assert(!search.pathsByMove.contains("c5d5"), "the unrestricted MultiPV pool did not return every legal reply")
    assertEquals(search.pairAnswer(state.observationsPacket.get, "e6d5", "c5d5"), None)
