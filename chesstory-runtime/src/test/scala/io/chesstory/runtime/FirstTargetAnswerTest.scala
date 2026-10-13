package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CandidateLineEvaluation
import play.api.libs.json.*

/** Unchanged actual product reports own the response/value data. Selecting
  * an additional legal root use in one test is explicit test content, never
  * a claim that its geometry explains the engine's original grade. */
class FirstTargetAnswerTest extends munit.FunSuite:
  override def munitTimeout = 2.minutes
  import CommentaryAnswer.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef
  import TargetResponseInvestigation.{FirstTarget, firstTargets}

  private lazy val records =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/first-target-answers-produced.json.gz"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    val fixture = try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()
    (fixture \ "cases").as[List[JsObject]].map(value => (value \ "game_id").as[String] -> value).toMap

  private lazy val states: Map[String, CommentaryJobState] = records.map { (id, record) =>
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 180001L, request.engineProfile,
        investigateChoices = true, previewDepth = Some(8)), 1L,
      request.inputLines, request.proposals).toOption.get
    val reports = (record \ "saved_reports").as[List[JsObject]]
    assertEquals(reports.size, if id.endsWith("HELD") then 10 else 7)
    reports.zipWithIndex.foreach { (saved, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"no issued work for actual receipt $id/$index")
      val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("f" * 32, request.requestId, 180001L, state))
      assertEquals((wire \ "issued_engine_work").as[JsObject], (saved \ "issued_work").as[JsObject],
        "the complete actual work descriptor must match before every receipt is replayed")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    assert(state.isInstanceOf[CompletedCommentaryJob])
    id -> state
  }

  private case class Fixture(id: String, state: CommentaryJobState, packet: EvidenceBackedJudgmentPacket,
      paths: PreparedPaths, pair: List[PreparedPath], proof: FirstTarget):
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(packet, paths).local
    def previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    def close(answer: Answer) = MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, answer)
    def protect(answer: Answer, sections: List[Section]) =
      MoveWorkProjection.protectRootUses(packet, paths, pair, positions, notes, answer, sections)
    def read(answer: Answer = previous): Answer = TargetResponseInvestigation.withAnswer(packet, paths, pair,
      List(proof), notes, answer, close, protect)
    def wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("f" * 32, (records(id) \ "request" \ "request_id").as[String], 180001L, state))
    def summary =
      val candidates = (wire \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      candidates.filter(value => (value \ "selection").as[String] == "selected" &&
        (value \ "detail" \ "type").asOpt[String].contains("root_comparison"))
        .flatMap(value => (value \ "detail" \ "summary_question").asOpt[JsObject])
        .find(value => (value \ "question").asOpt[String].contains("first_target_response"))
        .getOrElse(fail("the actual root has no delivered first-target answer"))
    def ref(value: JsObject): MoveRef =
      val path = paths.path(packet, (value \ "line_id").as[String]).get
      val offset = path.ids.indexOf((value \ "occurrence_id").as[String])
      assert(offset >= 0)
      MoveRef(path, offset)
    def covered(field: String): List[MoveRef] = (summary \ field).asOpt[List[JsObject]].getOrElse(Nil)
      .flatMap(unit => (unit \ "covered_moves").as[List[JsObject]]).map(ref)

  private def actual(suffix: String): Fixture =
    val id = "FC-SG41-ND5-" + suffix
    val state = states(id)
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val proofs = firstTargets(packet, paths, state.responseInquiries, state.responseSearches)
    assertEquals(proofs.size, 1, "only the answered exact actual Nc3 mover is the root-target proof")
    val primary = records(id) \ "baseline_primary"
    val pair = List("played_endpoint", "reference_endpoint").map(key =>
      paths.certifiedPath(packet, (primary \ key \ "moves").as[List[String]]).get)
    Fixture(id, state, packet, paths, pair, proofs.head)

  private def statements(answer: Answer) = answer.core.flatMap(_.statements)
  private def white(role: String, square: String) = CommentaryFact.Piece("white", role, square).witness

  private def rootGradeUnchanged(f: Fixture): Unit =
    val review = (f.wire \ "result" \ "selected_move_reviews").as[List[JsObject]]
      .find(value => (value \ "move_uci").as[String] == "c3d5").get
    assertEquals((review \ "commentary" \ "primary").as[JsObject],
      (records(f.id) \ "baseline_primary").as[JsObject],
      "first-reply ranking and defender geometry must not rewrite any original root assessment field")

  test("MOVED begins with actual Nd5 and closes both compared first replies"):
    val f = actual("MOVED")
    val answer = f.read()
    assertEquals(answer.question, "first_target_response")
    assertEquals(f.proof.leaderMove, "f5e4")
    assertEquals(f.proof.fullPool.work.workId, "work:3")
    assertEquals(f.proof.captureDecline.work.workId, "work:5")
    val result = RootQuestionSummary.realize(f.paths, f.pair, answer).get
    assert(result.text.startsWith("After Nd5"), result.text)
    val body = f.pair.head
    assertEquals(body.moves(2), "d4e4")
    assertEquals(f.paths.pieceAfter(f.packet, body, white("rook", "d4"), 3),
      Some(Some(white("rook", "e4"))))
    val recaptureSan = f.paths.notation.line(body.moves)(2)
    assertEquals(recaptureSan, "Rxe4")
    assert(result.text.contains("fxe4") && result.text.contains(recaptureSan), result.text)
    val controls = statements(answer).collect { case value: ControlState => value }
    assert(controls.exists(value => value.actor == white("rook", "e4") && !value.present &&
      value.target.contains(white("knight", "d5")) && value.remaining.isEmpty))
    assert(controls.exists(value => value.actor == white("rook", "d1") && value.present &&
      value.target.contains(white("knight", "d5"))))
    assertEquals(statements(answer).flatMap(_.covered).filter(move =>
      (move.path eq body) && Set(1, 2)(move.offset)).map(_.offset).toSet, Set(1, 2))
    val capture = f.proof.captureDecline.pathsByMove("c5d5")
    assertEquals(statements(answer).flatMap(_.covered).filter(move =>
      (move.path eq capture) && Set(1, 2)(move.offset)).map(_.offset).toSet, Set(1, 2))
    assertEquals((f.summary \ "first_target_response").as[JsObject], f.proof.ownershipJson)
    assertEquals(f.covered("units").filter(move => (move.path eq body) && Set(1, 2)(move.offset))
      .map(_.offset).toSet, Set(1, 2))
    assertEquals(f.covered("units").filter(move => (move.path eq capture) && Set(1, 2)(move.offset))
      .map(_.offset).toSet, Set(1, 2))
    rootGradeUnchanged(f)

  test("HELD keeps all four immediate capture prices and never substitutes a rook for captured Nc3"):
    val f = actual("HELD")
    assertEquals(f.proof.inquiry.state, "answered")
    assertEquals(f.proof.leaderMove, "e6d5")
    assertEquals(f.proof.fullPool.work.workId, "work:4")
    assertEquals(f.proof.captureDecline.work.workId, "work:4")
    val answer = f.read()
    assertEquals(answer.question, "first_target_response")
    val body = f.pair.head
    assertEquals(body.moves.take(5), List("c3d5", "e6d5", "d4d5", "c5d5", "d1d5"))
    assertEquals(statements(answer).flatMap(_.covered).filter(move =>
      (move.path eq body) && (1 to 4).contains(move.offset)).map(_.offset).toSet, Set(1, 2, 3, 4))
    assert(!statements(answer).exists {
      case value: ControlState => value.move.offset == 2 && value.target.contains(white("knight", "d5"))
      case _ => false
    })
    assertEquals(f.paths.pieceAfter(f.packet, body, f.proof.question.originalTarget, 3), Some(None))
    val result = RootQuestionSummary.realize(f.paths, f.pair, answer).get
    assert(result.text.startsWith("After Nd5"), result.text)
    assertEquals(body.moves(4), "d1d5")
    assertEquals(f.paths.pieceAfter(f.packet, body, white("rook", "d1"), 5),
      Some(Some(white("rook", "d5"))))
    assertEquals(f.paths.pieceAfter(f.packet, body, white("rook", "d4"), 4), Some(None))
    val recaptureSan = f.paths.notation.line(body.moves)(4)
    assertEquals(recaptureSan, "Rxd5")
    assert(result.text.contains("exd5") && result.text.contains(recaptureSan), result.text)
    assertEquals(f.covered("units").filter(move =>
      (move.path eq body) && (1 to 4).contains(move.offset)).map(_.offset).toSet, Set(1, 2, 3, 4))
    rootGradeUnchanged(f)

  test("unanswered, stopped, missing-pool or missing-pair evidence supplies no compact proof"):
    val f = actual("MOVED")
    val inquiry = f.proof.inquiry
    List(inquiry.copy(state = "observed", answer = None),
      inquiry.copy(state = "stopped", answer = None, stopReason = Some("search_budget_exhausted")),
      inquiry.copy(reading = None), inquiry.copy(readingOwner = None)).foreach { unready =>
      assertEquals(firstTargets(f.packet, f.paths, List(unready), f.state.responseSearches), Nil)
    }
    List(f.proof.fullPool, f.proof.captureDecline).foreach { absent =>
      assertEquals(firstTargets(f.packet, f.paths, List(inquiry),
        f.state.responseSearches.filterNot(_ eq absent)), Nil)
    }
    assertEquals(TargetResponseInvestigation.withAnswer(f.packet, f.paths, f.pair, Nil, f.notes,
      f.previous, f.close, f.protect), f.previous)

  test("stale selection, foreign reading/search ownership and an unrelated target fail closed"):
    val f = actual("MOVED")
    val inquiry = f.proof.inquiry
    val question = inquiry.question
    val alien = question.copy(alternativeSelection = question.alternativeSelection.copy(workId = Some("foreign-pool")))
    val wrongLine = question.copy(alternativeSelection = question.alternativeSelection.copy(lineId = Some("foreign-line")))
    val unrelated = question.copy(originalTarget = white("pawn", "c2"))
    val later = question.copy(rootMoveOccurrenceId = "foreign-root")
    List(alien, wrongLine, unrelated, later).foreach { changed =>
      assertEquals(firstTargets(f.packet, f.paths, List(inquiry.copy(question = changed)), f.state.responseSearches), Nil)
    }
    val external = EvidenceBackedJudgmentPacket.fromAssembly(f.packet.assembly).get
    assert(!(external eq f.packet))
    val foreignPaths = ObservedLineProtocol.prepare(external)
    assertEquals(firstTargets(external, foreignPaths, List(inquiry), f.state.responseSearches), Nil)
    assertEquals(firstTargets(f.packet, f.paths, List(inquiry.copy(readingOwner = Some(external))),
      f.state.responseSearches), Nil)
    val unowned = inquiry.copy(answer = inquiry.answer.map(value => value.copy(pair =
      value.pair.copy(workId = "foreign-comparison"))))
    assertEquals(firstTargets(f.packet, f.paths, List(unowned), f.state.responseSearches), Nil)

  test("an underdepth unrestricted receipt cannot become a completed first-reply pool"):
    val f = actual("MOVED")
    val pool = f.proof.fullPool
    val child = f.packet.assembly.input.positionHistory.extend(pool.anchor.prefixMoves).toOption.get
    val saved = (records(f.id) \ "saved_reports").as[List[JsObject]].find(value =>
      (value \ "issued_work" \ "work_id").as[String] == pool.work.workId).get
    val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
      Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
    val suffixes = report match
      case EngineWorkReport.Completed(_, _, _, rows) => rows.map(_.copy(depth = pool.work.requiredDepth - 1))
      case _ => fail("the sealed actual pool was not completed")
    assertEquals(EngineLineAdmission.admitReportedLineSuffixes(pool.work, suffixes, Some(child)), None,
      "the public report admission already rejects an underdepth completed claim")
    val lines = pool.lines.map { original =>
      val continuation = child.admitToFirstAutomaticTerminal(original.evaluation.moves).toOption.get
      val evaluation = CandidateLineEvaluation.EngineSearch(original.evaluation.engineLine.get.copy(
        depth = pool.work.requiredDepth - 1))
      continuation.bind(evaluation).get
    }
    assertEquals(TargetResponseInvestigation.readSearch(f.packet,
      CommentaryInvestigation.EngineCandidates(pool.work, lines), child, f.paths), None)

  test("missing first-own-action defense resources retain the previous complete answer"):
    val f = actual("MOVED")
    val leader = f.proof.captureDecline.pathsByMove(f.proof.leaderMove)
    val missing = EvidenceBackedJudgmentPacket.fromAssembly(f.packet.assembly.copy(
      positionAnalyses = f.packet.assembly.positionAnalyses - leader.positions(3))).get
    val paths = ObservedLineProtocol.prepare(missing)
    val searches = f.state.responseSearches.flatMap { search =>
      val child = missing.assembly.input.positionHistory.extend(search.anchor.prefixMoves).toOption.get
      TargetResponseInvestigation.readSearch(missing,
        CommentaryInvestigation.EngineCandidates(search.work, search.lines), child, paths)
    }
    val question = f.proof.question
    val answer = f.proof.inquiry.answer.get
    val reading = TargetResponseInvestigation.read(missing, question,
      answer.pair.referenceLineId, answer.pair.alternativeLineId, paths).get
    assert(reading.complete, "the existing capture/decline receipt remains ready; the own defense is separately unknown")
    val inquiry = f.proof.inquiry.copy(reading = Some(reading), readingOwner = Some(missing))
    val proof = firstTargets(missing, paths, List(inquiry), searches)
    assertEquals(proof.size, 1)
    val pair = f.pair.map(path => paths.certifiedPath(missing, path.moves).get)
    val previous = RootQuestionSummary.completePairAnswer(missing, paths, pair, "selected_comparison", Nil).get
    val positions = missing.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(missing, paths).local
    val selected = TargetResponseInvestigation.withAnswer(missing, paths, pair, proof, notes, previous,
      value => MoveWorkProjection.completeSelectedConditions(missing, paths, pair, positions, notes, value),
      (value, sections) => MoveWorkProjection.protectRootUses(missing, paths, pair, positions, notes, value, sections))
    assertEquals(selected, previous)

  test("an independently selected explicit defense question is preserved"):
    val f = actual("MOVED")
    val explicit = f.previous.copy(question = "current_defense")
    assertEquals(f.read(explicit), explicit)

  test("an independent root vacancy/use stays together in detail without reseeding the first-reply answer"):
    val f = actual("MOVED")
    val path = f.proof.captureDecline.pathsByMove(f.proof.leaderMove)
    val offset = path.moves.indexOf("c2c3")
    assert(offset > 2)
    val pawn = white("pawn", "c2")
    val entrant = f.paths.pieceAfter(f.packet, path, pawn, offset + 1).flatten.get
    assertEquals(entrant, white("pawn", "c3"))
    assert((1 to offset).forall(count => f.paths.positionView(path, count)
      .exists(view => view.occupantAt(EvidenceSquare("c3")).isEmpty)))
    val use = SquareReoccupation(MoveRef(path, 0), f.proof.question.originalTarget, f.proof.question.target,
      "c3", MoveRef(path, offset), pawn, entrant)
    val previous = f.previous.copy(core = f.previous.core :+ Section(List(use)))
    val selected = f.read(previous)
    assertEquals(selected.question, "first_target_response")
    assert(selected.details.flatMap(_.statements).contains(use))
    assert(selected.details.flatMap(_.references).contains(MoveRef(path, offset)))
    assert(!selected.completionSources.get.contains(use))
    val repeated = f.close(selected).get
    assert(repeated.details.flatMap(_.statements).contains(use))
    assertEquals(repeated.required.toSet, selected.required.toSet,
      "already closed prices cannot grow into another selected history on the second pass")
    assert(RootQuestionSummary.realize(f.paths, f.pair, repeated).nonEmpty)
