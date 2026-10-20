package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import play.api.libs.json.*

/** Only the two live positions used for this bounded goal. */
class ReferencePurposeReadingTest extends munit.FunSuite:
  override def munitTimeout = 2.minutes
  private lazy val fixture =
    val name = "judgment-evaluation/fixtures/public-commentary-v6/evaluation-reference-browser.json.gz"
    val file = List(Path.of(name), Path.of("..", name)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(file))
    try Json.parse(stream.readAllBytes()) finally stream.close()

  private def start(row: JsValue): CommentaryJobState =
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((row \ "request").get).getBytes(UTF_8)).toOption.get
    CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(12, 10000L, request.engineProfile, investigateChoices = true, shortJob = true),
      1L, request.inputLines, request.proposals).toOption.get

  private def submit(state: CommentaryJobState, saved: JsValue, index: Int): CommentaryJobState =
    val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
    val raw = (saved \ "report").as[JsObject] ++ Json.obj("work_id" -> work.workId,
      "execution_key_sha256" -> work.executionKeySha256)
    val report = PositionCommentaryJobProtocol.parseEngineWorkReport(Json.stringify(raw).getBytes(UTF_8)).toOption.get
    CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get

  private def read(id: String) =
    val row = (fixture \ "cases").as[List[JsObject]].find(r => (r \ "id").as[String] == id).get
    val state = (row \ "saved_reports").as[List[JsObject]].zipWithIndex.foldLeft(start(row)) {
      case (state, (saved, index)) => submit(state, saved, index)
    }
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val reading = ReferencePurposeReading.read(packet, paths, pair).get
    val notes = MoveWorkProjection.select(packet, paths).local
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val answer = ReferencePurposeReading.answer(packet, paths, pair, reading,
      value => MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, value)).get
    assert(RootQuestionSummary.realize(paths, pair, answer).nonEmpty)
    val foreign = ObservedLineProtocol.prepare(packet)
    assert(!reading.belongsTo(foreign))
    assert(ReferencePurposeReading.answer(packet, foreign, pair, reading, Some(_)).isEmpty)
    assert(ReferencePurposeReading.read(packet, paths, pair.take(1)).isEmpty)
    val unpaid = answer.copy(core = answer.core.map(section => section.copy(statements = section.statements.filterNot {
      case _: CommentaryAnswer.Material | _: CommentaryAnswer.MaterialEvents => true
      case _ => false
    })))
    assert(RootQuestionSummary.realize(paths, pair, unpaid).isEmpty, "selected captures still owe their complete price")
    reading

  test("the actual c6/Bd5 comparison keeps the pawn-backed post and closes the whole exchange"):
    val reading = read("existing")
    assertEquals(reading.arms.map(_.path.moves.head), List("c7c6", "b3d5"))
    assertEquals(reading.supportedPost, Some("d5"))
    assert(reading.reference.shields.exists((attacker, target) =>
      attacker.square.key == "g2" && target.square.key == "b7"))
    assert(reading.reference.remainingGuards.exists(_.square.key == "c6"))
    assert(reading.played.remainingGuards.isEmpty)
    assertEquals(reading.played.through, 6)
    assert(reading.played.occupant.exists(_.role.name == "queen"),
      "the intermediate knight is gone after the selected exchange closes")
    assert(reading.played.retainedMover.isEmpty)

    // The earlier rank flip must not substitute the supplementary Nd5 choice
    // for the actual assessment pair. A completed pair needs no extra search.
    val flipped = (fixture \ "rank_flip").get
    val next = submit(start(flipped), (flipped \ "saved_reports").as[List[JsObject]].head, 0)
    next match
      case pending: AwaitingEngineWork => pending.issuedWork.rootRestriction match
        case EngineRootRestriction.Restricted(moves) => assert(!moves.contains("b6d5"))
        case _ => ()
      case _ => ()

  test("Nd4/e4 connects loss of the supporting pawn to the opponent's resulting central knight"):
    val reading = read("new")
    assertEquals(reading.arms.map(_.path.moves.head), List("c6d4", "e5e4"))
    assertEquals(reading.supportedPost, Some("d4"))
    assert(reading.reference.remainingGuards.exists(_.square.key == "e5"))
    assert(reading.reference.occupant.exists(_.role.name == "knight"))
    assert(reading.retainedChoice.exists(_.square.key == "e5"))
    assert(reading.played.retainedMover.isEmpty)
    assert(reading.played.replyAt.exists(p => p.role.name == "knight" && p.square.key == "e4"))
    assert(reading.played.replyGuards.exists(_.square.key == "d3"))
    val captures = reading.played.path.certified.get._2.material.get.captures.filter(_.plyOffset <= reading.played.through)
    assertEquals(captures.size, 2)
    assert(captures.forall(_.capturedRole.name == "pawn"), "the returned line is a pawn exchange, not the book's rook capture")
