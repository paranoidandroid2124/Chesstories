package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** Only the existing Ne4 and the preregistered Gelfand Nf3 position. These
  * replay actual reports; no book continuation or evaluation enters the job. */
class ExchangeFunctionReadingTest extends munit.FunSuite:
  override def munitTimeout = 2.minutes
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}

  private lazy val records =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/exchange-function-produced.json.gz"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    val json = try Json.parse(stream.readAllBytes()) finally stream.close()
    (json \ "cases").as[List[JsObject]].map(row => (row \ "game_id").as[String] -> row).toMap

  private def primary(wire: JsValue): JsValue =
    (wire \ "result" \ "selected_move_reviews").as[List[JsObject]].find(row =>
      (row \ "selection" \ "roles").as[List[String]].contains("played")).get
      .apply("commentary").as[JsObject].apply("primary")

  private case class Fixture(id: String, state: CommentaryJobState, packet: EvidenceBackedJudgmentPacket,
      paths: PreparedPaths, pair: List[PreparedPath], proof: TargetResponseInvestigation.FirstTarget,
      wire: JsValue):
    val notes = MoveWorkProjection.select(packet, paths).local
    val value = ExchangeFunctionReading.read(packet, paths, pair, proof, notes).get
    val detail = (wire \ "observations" \ "selection" \ "candidates").as[List[JsObject]].filter(row =>
      (row \ "selection").as[String] == "selected" &&
        (row \ "detail" \ "type").asOpt[String].contains("root_comparison"))
      .map(row => (row \ "detail").as[JsObject]).find(row =>
        (row \ "summary_question" \ "question").asOpt[String].contains("exchange_function")).get
    val units = (detail \ "summary_question" \ "units").as[List[JsObject]]
    def covered(path: PreparedPath, offsets: Set[Int]): Boolean =
      val ids = units.flatMap(unit => (unit \ "covered_moves").as[List[JsObject]])
        .map(row => ((row \ "line_id").as[String], (row \ "occurrence_id").as[String])).toSet
      offsets.forall(offset => ids(path.line.ref.id -> path.ids(offset)))

  private def load(id: String): Fixture =
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
      val pending = state.asInstanceOf[AwaitingEngineWork].issuedWork
      assert(pending.requiredDepth <= 18)
      assertEquals(serializer.invoke(PositionCommentaryJobProtocol, pending), (saved \ "issued_work").get)
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    assert(state.isInstanceOf[CompletedCommentaryJob])
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val baseline = (record \ "baseline_primary").get
    val keys = if (baseline \ "kind").as[String] == "best_choice" then
      List("best_endpoint", "runner_up_endpoint") else List("played_endpoint", "reference_endpoint")
    val pair = keys.map(key => paths.certifiedPath(packet, (baseline \ key \ "moves").as[List[String]]).get)
    val proofs = TargetResponseInvestigation.firstTargets(packet, paths, state.responseInquiries, state.responseSearches)
    assertEquals(proofs.size, 1)
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("f" * 32, request.requestId, 180000L, state))
    assertEquals(primary(wire), baseline, "the explanation cannot rewrite the original judgment or its endpoints")
    Fixture(id, state, packet, paths, pair, proofs.head, wire)

  /** Remove one required board inventory while keeping the actual response
    * receipts. The function must disappear, not borrow another path's board. */
  private def missingPosition(f: Fixture, missing: PositionNodeRef): Unit =
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(f.packet.assembly.copy(
      positionAnalyses = f.packet.assembly.positionAnalyses - missing)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val searches = f.state.responseSearches.flatMap { search =>
      val child = packet.assembly.input.positionHistory.extend(search.anchor.prefixMoves).toOption.get
      TargetResponseInvestigation.readSearch(packet,
        CommentaryInvestigation.EngineCandidates(search.work, search.lines), child, paths)
    }
    val inquiry = f.proof.inquiry
    val pairAnswer = inquiry.answer.get.pair
    val reading = TargetResponseInvestigation.read(packet, inquiry.question,
      pairAnswer.referenceLineId, pairAnswer.alternativeLineId, paths).get
    val proofs = TargetResponseInvestigation.firstTargets(packet, paths,
      List(inquiry.copy(reading = Some(reading), readingOwner = Some(packet))), searches)
    assertEquals(proofs.size, 1, "the first reply comparison remains valid; the later function is unknown")
    val pair = f.pair.map(path => paths.certifiedPath(packet, path.moves).get)
    val notes = MoveWorkProjection.select(packet, paths).local
    assertEquals(ExchangeFunctionReading.read(packet, paths, pair, proofs.head, notes), None)
    assert(!f.value.belongsTo(paths), "a checked function cannot be transplanted to a changed evidence owner")

  test("Ne4 loses the f6 knight while Nd6 uses the other knight and retains pressure on e3"):
    val f = load("FJ-SG35-LATE-NE4")
    val loss = f.value.closed.get
    val kept = f.value.kept.get
    assertEquals((loss.slider.square.key, loss.blocker.square.key, loss.target.square.key), ("e6", "e4", "e3"))
    assertEquals((kept.exchanged.square.key, kept.retained.square.key, kept.arrived.square.key), ("c4", "f6", "d5"))
    assertEquals(kept.otherTargets.map(_.square.key), List("f4"))
    assertEquals(f.paths.trackedPieceAfter(f.value.capture, kept.retained, 3), Some(None))
    assertEquals(f.paths.trackedPieceAfter(kept.path, kept.retained, kept.use + 1), Some(Some(kept.arrived)))
    assertEquals(kept.path.moves.take(kept.use + 1), List("c4d6", "d3b1", "d6e4", "g3e4", "d5e4", "g2f1", "f6d5"))
    assert(f.covered(f.value.capture, Set(1, 2)))
    assert(f.covered(kept.path, Set(3, 4)))
    assertEquals(f.value.capture.moves(1), f.proof.leaderMove, "the strongest capture must own its own recapture price")
    assert(f.units.size <= 8)
    missingPosition(f, kept.path.positions(kept.use + 1))

  test("new Nf3 explains accepted and declined exchanges without turning a legal rook transfer into a scored line"):
    val f = load("GF-1975-24-NF3")
    assert(f.value.closed.isEmpty && f.value.kept.isEmpty)
    val access = f.value.access.get
    assertEquals((access.rook.square.key, access.removed.square.key, access.square, access.target.square.key),
      ("f3", "e5", "d3", "d6"))
    val position = new MoveWorkProjection.Position(f.paths.positionView(f.value.capture, 4).get)
    assert(position.view.legalRows.get.exists(_.move == "f3d3"))
    assertEquals(position.controllers("d3", chess.Black), Nil)
    assert(!f.value.references.exists(move => move.path.moves(move.offset) == "f3d3"),
      "the unplayed option cannot acquire a scored/observed action reference")
    val decline = f.proof.captureDecline.pathsByMove(f.proof.question.alternativeMove)
    assertEquals(decline.moves.take(6), List("d4f3", "f8e8", "a1d1", "d8c7", "f3e5", "e8e5"))
    assert(f.covered(f.value.capture, Set(1, 2)))
    assert(f.covered(decline, Set(4, 5)))
    assert(f.units.size <= 8)
    assertEquals(ExchangeFunctionReading.read(f.packet, f.paths, f.pair, f.proof, Nil), None,
      "a legal transfer alone does not select a useful pawn target without an observed role")
    missingPosition(f, f.value.capture.positions(4))
