package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.*

/** The two work-order tests replay unchanged actual root/focus receipts.
  * The composed development control uses synthetic scores only to admit
  * legal board facts; it cannot establish an engine preference or causation.
  */
class FirstTargetResponseTest extends munit.FunSuite:
  private lazy val records =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/sg41-first-target-prefixes-produced.json.gz"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    val fixture = try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()
    (fixture \ "cases").as[List[JsObject]].map(value => (value \ "game_id").as[String] -> value).toMap

  private def prefix(id: String): AwaitingEngineWork =
    val record = records(id)
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get
    val savedReports = (record \ "saved_reports").as[List[JsObject]]
    val count = if id.endsWith("HELD") then 4 else 3
    assertEquals(savedReports.size, count)
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 180001L, request.engineProfile,
        investigateChoices = true, previewDepth = Some(8)), 1L,
      request.inputLines, request.proposals).toOption.get
    savedReports.zipWithIndex.foreach { (saved, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"no issued work for actual root receipt $index")
      val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("d" * 32, request.requestId, 180001L, state))
      assertEquals((wire \ "issued_engine_work").as[JsObject], (saved \ "issued_work").as[JsObject],
        "all owner/history/profile/key fields must match before an actual report is submitted")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    state match
      case active: AwaitingEngineWork => active
      case other => fail(s"the already discovered played-mover response remains unfinished: ${other.getClass.getSimpleName}")

  private def firstTarget(id: String, capturingMoves: Set[String], observedReply: String): Unit =
    val next = prefix(id)
    val packet = next.observationsPacket.get
    val prepared = next.preparedPaths.get
    val work = next.issuedWork
    val questions = next.responseInquiries.filter(_.question.rootMove == "c3d5")
    val mover = questions.find(_.question.originalTarget.square.key == "c3").get.question
    assertEquals(mover.originalTarget.role.name, "knight")
    assertEquals(mover.target.square.key, "d5")
    assertEquals(mover.captureMoves.toSet, capturingMoves)
    assert((mover.captureMoves ++ mover.declineMoves).contains(observedReply))
    assert(prepared.paths.exists(path => path.certified.nonEmpty && path.ids == path.declared &&
      path.moves.startsWith(List("c3d5", observedReply))),
      "the first response is witnessed by this actual owned root, rather than invented for the schedule")
    assert(TargetResponseInvestigation.reviewedMover(packet, prepared, mover),
      "the promoted subject must be the exact actual original mover at its certified root occurrence")
    assert(questions.exists(_.question.originalTarget.square.key == "c2"),
      "the same child also contains an unrelated attacked pawn")
    assert(questions.filterNot(_.question.id == mover.id).forall(inquiry =>
      !TargetResponseInvestigation.reviewedMover(packet, prepared, inquiry.question)),
      "an unrelated target does not become a required moved-piece judgment")
    assertEquals(work.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(work.rootRestriction, EngineRootRestriction.Unrestricted,
      "a named capture versus a PV hint cannot supply the strongest owned reply")
    assertEquals(work.searchAnchor.map(_.prefixMoves), Some(List("c3d5")),
      "the actual root target pool must precede optional prefix14/18 exchange refinements")
    assertEquals(work.searchAnchor.map(_.rootOccurrenceId), Some(mover.rootMoveOccurrenceId))
    assertEquals(work.searchAnchor.map(_.positionId), Some(mover.childPositionId))
    assertEquals(work.requiredDepth, 16)
    assertEquals(work.multiPv, 3)
    assertEquals(work.enginePositionMovesUci, packet.assembly.input.positionHistory.movePrefixUci :+ "c3d5")
    assert(next.responseInquiries.find(_.question.id == mover.id).exists(inquiry =>
      inquiry.state == "awaiting_engine" && inquiry.answer.isEmpty && inquiry.stopReason.isEmpty))

  test("actual Nd5 after a5 receives its owned first target pool before the optional Ne2 exchange"):
    firstTarget("FC-SG41-ND5-HELD", Set("e6d5", "c5d5"), "e6d5")

  test("actual Nd5 after exf5 receives its owned first target pool before a later Rxf5 inquiry"):
    firstTarget("FC-SG41-ND5-MOVED", Set("c5d5"), "f5e4")

  test("quiet development does not promote every geometrical relation to a first mover obligation"):
    val fen = "rnbqkb1r/ppp1pp1p/3p1np1/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 4"
    val history = List("e2e4", "d7d6", "d2d4", "g8f6", "b1c3", "g7g6")
    val lines = List("g1f3 f8g7 c1e3", "c1e3 f8g7 g1f3").map(row =>
      EngineLine(row.split(" ").toList, 0, None, 16))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen, playedMoveUci = "g1f3",
      variations = lines, movePrefixUci = history)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    val questions = TargetResponseInvestigation.discover(packet, "c1e3", prepared)
      .filter(_.rootMove == "g1f3")
    assert(questions.exists(question => question.originalTarget.square.key == "e4" &&
      question.captureMoves.contains("f6e4")), "the existing unrelated pawn capture is still a legal question")
    assert(questions.forall(question => !TargetResponseInvestigation.reviewedMover(packet, prepared, question)),
      "Ng1's quiet development must not be treated as a threatened mover merely because Pe4 is attacked")
