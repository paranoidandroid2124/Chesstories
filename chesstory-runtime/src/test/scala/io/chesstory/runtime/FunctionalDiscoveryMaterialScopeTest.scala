package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path as NioPath}
import java.util.zip.GZIPInputStream
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** The six original NB1 work descriptors and reports are replayed unchanged.
  * This verifies owned conditional material evidence and numerical preservation;
  * it does not establish the strongest defence, utility, or the root score cause.
  */
class FunctionalDiscoveryMaterialScopeTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryAnswer.*
  import ObservedLineProtocol.PreparedPath

  private lazy val record =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/functional-discovery-material-nb1-produced.json.gz"
    val path = List(NioPath.of(relative), NioPath.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()

  private lazy val request = PositionCommentaryJobProtocol.parseJobRequest(
    Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get

  private lazy val current: CommentaryJobState =
    var state: CommentaryJobState = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 180001L, request.engineProfile,
        investigateChoices = true, previewDepth = Some(8)), 1L,
      request.inputLines, request.proposals).toOption.get
    val reports = (record \ "saved_reports").as[List[JsObject]]
    assertEquals(reports.size, 6)
    reports.zipWithIndex.foreach { (saved, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"no issued owner for original NB1 report $index")
      val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("s" * 32, request.requestId, 180001L, state))
      assertEquals((wire \ "issued_engine_work").as[JsObject], (saved \ "issued_work").as[JsObject],
        "compare the entire original work before admitting its unchanged report")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    assert(state.isInstanceOf[CompletedCommentaryJob],
      "conditional material access must not turn a generated witness into another investigation")
    state

  private lazy val packet = current.observationsPacket.get
  private lazy val paths = current.preparedPaths.getOrElse(ObservedLineProtocol.prepare(packet))
  private lazy val references = (record \ "expected_quiet_reference_moves").as[List[List[String]]]
  private val provider = Piece("white", "knight", "d2")
  private val enemy = Piece("black", "queen", "c6")
  private val friendly = Piece("white", "rook", "d1")
  private val control = Predicate("control", Some(provider), square = Some("d6"))
  private val capture = Predicate("legal_capture", Some(provider), Some(enemy))

  private lazy val proofs = FunctionalReplyReading.read(current.proposalEntries).filter(proof =>
    references.contains(proof.reference) && proof.condition == control &&
      proof.effect == capture && proof.price.isEmpty)

  private def path(moves: List[String]): PreparedPath = paths.paths.filter(_.moves.startsWith(moves))
    .minByOption(_.moves.size).getOrElse(fail(s"missing original owned witness $moves"))

  test("actual generated NB1 recapture proofs read discovery-only material without widening independent seeds"):
    assertEquals(proofs.map(_.reference).toSet, references.toSet)
    assertEquals(proofs.size, 2)
    val seedsBefore = paths.investigationPaths.map(_.line.ref.id).toSet
    val readingsBefore = paths.continuationReadings.map(_.path.line.ref.id).toSet
    proofs.foreach { proof =>
      val statement = LegalReplyOption(path(proof.reference), path(proof.alternative), path(proof.option), proof)
      assert(packet.assembly.input.legalLines.exists(line =>
        line.moves == statement.alternative.sourceMoves && line.discoveryOnly))
      assert(!seedsBefore(statement.alternative.line.ref.id))
      assert(!readingsBefore(statement.alternative.line.ref.id))
      val reading = paths.continuationReadingFor(statement.alternative).get
      val loss = reading.events.find(_.plyOffset == proof.alternative.size - 1).get
      assertEquals(loss.originalActor, Some(enemy.witness))
      assertEquals(loss.originalVictim, Some(friendly.witness))
      assert(FunctionalReplyReading.valid(paths, statement),
        "supported board facts must not fail solely because a declared material witness is excluded from scheduling")
      assertEquals(proof.owner.answer.map(_.workId), Some("work:3"),
        "the original completed numeric receipt remains distinct from the new board-only material access")
    }
    assertEquals(paths.investigationPaths.map(_.line.ref.id).toSet, seedsBefore)
    assertEquals(paths.continuationReadings.map(_.path.line.ref.id).toSet, readingsBefore)
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("s" * 32, request.requestId, 180001L, current))
    val played = (wire \ "result" \ "selected_move_reviews").as[List[JsObject]].find(value =>
      (value \ "move_uci").as[String] == request.playedMoveUci).get
    assertEquals((played \ "commentary" \ "primary").as[JsObject],
      (record \ "expected_root_primary").as[JsObject],
      "a conditional material reading cannot replace or reinterpret the actual numerical inaccuracy")

  test("an owned material event cannot repair a truncated path or stale root receipt"):
    assertEquals(proofs.size, 2)
    proofs.foreach { proof =>
      val statement = LegalReplyOption(path(proof.reference), path(proof.alternative), path(proof.option), proof)
      val rootOnly = paths.paths.filter(_.moves.headOption.contains(proof.alternative.head))
        .minByOption(_.moves.size).get
      assert(!FunctionalReplyReading.valid(paths, statement.copy(alternative = rootOnly)))
      val receipt = proof.owner.answer.get
      val stale = proof.copy(owner = proof.owner.copy(answer = Some(receipt.copy(
        alternativeOccurrenceId = statement.reference.ids.head))))
      assert(!FunctionalReplyReading.valid(paths, statement.copy(proof = stale)),
        "exact board material cannot authorize a receipt with another root transition owner")
    }
