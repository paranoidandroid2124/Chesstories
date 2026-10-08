package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** These are unchanged public product receipts, not supplied evaluations or
  * edited PVs. A first reply's meaning must come from its exact first own
  * action; a later escape or an unrelated arm cannot supply that source. */
class FirstReplyMeaningTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import MoveWorkProjection.{Detail, LocalCandidate}
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef
  import TargetResponseInvestigation.{FirstTarget, firstTargets}

  private def load(relative: String): List[JsObject] =
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    val fixture = try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()
    (fixture \ "cases").as[List[JsObject]]

  private lazy val records = (load(
    "judgment-evaluation/fixtures/public-commentary-v6/first-reply-meaning-produced.json.gz") ++
    load("judgment-evaluation/fixtures/public-commentary-v6/first-target-answers-produced.json.gz")
      .filter(value => (value \ "game_id").as[String] == "FC-SG41-ND5-MOVED"))
    .map(value => (value \ "game_id").as[String] -> value).toMap

  private def replayState(id: String): CommentaryJobState =
    val record = records(id)
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 180001L, request.engineProfile,
        investigateChoices = true, previewDepth = Some(8)), 1L,
      request.inputLines, request.proposals).toOption.get
    val reports = (record \ "saved_reports").as[List[JsObject]]
    assertEquals(reports.size, if id == "TAT25-10" then 6 else 7)
    reports.zipWithIndex.foreach { (saved, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"no issued actual work for $id/$index")
      val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("f" * 32, request.requestId, 180001L, state))
      assertEquals((wire \ "issued_engine_work").as[JsObject], (saved \ "issued_work").as[JsObject],
        "the whole actual issued descriptor must match before its unchanged report is submitted")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    assert(state.isInstanceOf[CompletedCommentaryJob])
    state

  private lazy val tatState = replayState("TAT25-10")
  private lazy val movedState = replayState("FC-SG41-ND5-MOVED")

  private case class Fixture(id: String, state: CommentaryJobState, packet: EvidenceBackedJudgmentPacket,
      paths: PreparedPaths, pair: List[PreparedPath], proof: FirstTarget):
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(packet, paths).local
    def previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    def close(answer: Answer, local: List[LocalCandidate] = notes) =
      MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, local, answer)
    def read(answer: Answer = previous, local: List[LocalCandidate] = notes): Answer =
      TargetResponseInvestigation.withAnswer(packet, paths, pair, List(proof), local, answer,
        value => close(value, local),
        (value, sections) => MoveWorkProjection.protectRootUses(packet, paths, pair,
          positions, local, value, sections))
    def wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("f" * 32, (records(id) \ "request" \ "request_id").as[String], 180001L, state))
    def summary = (wire \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .filter(value => (value \ "selection").as[String] == "selected" &&
        (value \ "detail" \ "type").asOpt[String].contains("root_comparison"))
      .flatMap(value => (value \ "detail" \ "summary_question").asOpt[JsObject])
      .find(value => (value \ "question").asOpt[String].contains("first_target_response")).get
    def ref(value: JsObject): MoveRef =
      val path = paths.path(packet, (value \ "line_id").as[String]).get
      val offset = path.ids.indexOf((value \ "occurrence_id").as[String])
      assert(offset >= 0)
      MoveRef(path, offset)
    def unitText(unit: JsObject): String = (unit \ "en").as[List[JsValue]].map {
      case JsString(text) => text
      case value: JsObject =>
        val move = ref(value)
        paths.notation.line(move.path.moves)(move.offset)
      case other => fail(s"unexpected inline part $other")
    }.mkString
    def covered(field: String): List[MoveRef] = (summary \ field).as[List[JsObject]]
      .flatMap(unit => (unit \ "covered_moves").as[List[JsObject]]).map(ref)

  private def actual(id: String): Fixture =
    val state = id match
      case "TAT25-10" => tatState
      case "FC-SG41-ND5-MOVED" => movedState
      case other => fail(s"unknown actual fixture $other")
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val proofs = firstTargets(packet, paths, state.responseInquiries, state.responseSearches)
    assertEquals(proofs.size, 1, "one answered exact actual mover owns the first-target answer")
    val primary = records(id) \ "baseline_primary"
    val pair = List("played_endpoint", "reference_endpoint").map(key =>
      paths.certifiedPath(packet, (primary \ key \ "moves").as[List[String]]).get)
    Fixture(id, state, packet, paths, pair, proofs.head)

  private def white(role: String, square: String) = CommentaryFact.Piece("white", role, square).witness
  private def black(role: String, square: String) = CommentaryFact.Piece("black", role, square).witness
  private def statements(answer: Answer): List[Statement] = answer.core.flatMap(_.statements)
  private def rootGradeAndOwnersUnchanged(f: Fixture): Unit =
    val played = f.pair.head.moves.head
    val review = (f.wire \ "result" \ "selected_move_reviews").as[List[JsObject]]
      .find(value => (value \ "move_uci").as[String] == played).get
    assertEquals((review \ "commentary" \ "primary").as[JsObject],
      (records(f.id) \ "baseline_primary").as[JsObject], "no root assessment field may be rewritten")
    assertEquals((f.summary \ "first_target_response").as[JsObject], f.proof.ownershipJson)
    if f.id == "TAT25-10" then
      assertEquals(f.proof.ownershipJson, (records(f.id) \ "baseline_first_target_response").as[JsObject])

  private def assertLegacyExchangeDetail(f: Fixture, answer: Answer): Unit =
    val path = f.pair.head
    assertEquals(path.moves.slice(16, 18), List("f3d4", "c5d4"))
    val originalKnight = white("knight", "f3")
    assertEquals(f.paths.pieceAfter(f.packet, path, originalKnight, 17), Some(Some(white("knight", "d4"))))
    assertEquals(f.paths.pieceAfter(f.packet, path, originalKnight, 18), Some(None))
    assert(!statements(answer).flatMap(_.covered).exists(move =>
      (move.path eq path) && Set(16, 17)(move.offset)))
    assertEquals(answer.details.flatMap(_.covered).filter(move =>
      (move.path eq path) && Set(16, 17)(move.offset)).map(_.offset).toSet, Set(16, 17))
    assert(!answer.completionSources.get.flatMap(_.actionReferences).exists(move =>
      (move.path eq path) && Set(16, 17)(move.offset)), "detail prices cannot become new own-action sources")

  test("actual f6 attacks original Ne5 and Ng4 evades it in basic without selecting the late Nf3 exchange"):
    val f = actual("TAT25-10")
    val body = f.pair.head
    assertEquals(body.moves.take(3), List("e3e4", "f7f6", "e5g4"))
    assertEquals(f.proof.leaderMove, "f7f6")
    assertEquals(f.proof.fullPool.work.workId, "work:4")
    assertEquals(f.proof.captureDecline.work.workId, "work:5")
    assertEquals(f.paths.pieceAfter(f.packet, body, white("knight", "e5"), 2),
      Some(Some(white("knight", "e5"))))
    assertEquals(f.paths.pieceAfter(f.packet, body, white("knight", "e5"), 3),
      Some(Some(white("knight", "g4"))))
    val evasion = Action(MoveRef(body, 2), Detail.Evades(white("knight", "g4"), black("pawn", "f6")))
    val incoming = Action(MoveRef(body, 1), Detail.Attack(black("pawn", "f6"), white("knight", "e5"), true))
    assert(f.notes.exists(note => note.edge.evidence.id == body.ids(2) && note.note.detail == evasion.detail),
      "the actual first-own evasion is already an observed fact, independent of selection")
    assertEquals(f.paths.pieceAfter(f.packet, body, white("knight", "e5"), 7),
      Some(Some(white("knight", "e3"))))
    val use = SquareReoccupation(MoveRef(body, 0), white("pawn", "e3"), white("pawn", "e4"),
      "e3", MoveRef(body, 6), white("knight", "e5"), white("knight", "e3"))
    val answer = f.read(f.previous.copy(core = f.previous.core :+ Section(List(use))))
    assertEquals(answer.question, "first_target_response")
    assert(statements(answer).contains(evasion), "the current first-own escape must be selected explicitly")
    assert(statements(answer).contains(incoming), "common condition closure must recover the first reply's target")
    assertEquals(answer.completionSources.get.count(_ == evasion), 1)
    assert(answer.completionSources.get.contains(use), "a separate actual root use remains owned")
    assert(answer.required.contains(MoveRef(body, 6)))
    assert(!answer.completionSources.get.exists {
      case Action(move, _: Detail.Evades) => (move.path eq body) && move.offset == 18
      case _ => false
    })
    assertLegacyExchangeDetail(f, answer)
    val closedAgain = f.close(answer).get
    assertEquals(closedAgain.completionSources, answer.completionSources)
    assertLegacyExchangeDetail(f, closedAgain)
    val basic = RootQuestionSummary.realize(f.paths, f.pair, answer).get.text
    assert(basic.contains("After f6, Black's pawn on f6 now targets White's knight on e5."), basic)
    assert(basic.contains("Ng4 moves the knight out of the attack from Black's pawn on f6."), basic)
    val publicBasic = (f.summary \ "units").as[List[JsObject]].map(f.unitText).mkString(" ")
    assert(publicBasic.contains("After f6, Black's pawn on f6 now targets White's knight on e5."), publicBasic)
    assert(publicBasic.contains("Ng4 moves the knight out of the attack from Black's pawn on f6."), publicBasic)
    assert(!f.covered("units").exists(move => (move.path eq body) && Set(16, 17)(move.offset)))
    assertEquals(f.covered("detail_units").filter(move =>
      (move.path eq body) && Set(16, 17)(move.offset)).map(_.offset).toSet, Set(16, 17))
    rootGradeAndOwnersUnchanged(f)

  test("real later Nc2 and other-arm Ng4 notes cannot substitute for missing exact first-own notes"):
    val f = actual("TAT25-10")
    val body = f.pair.head
    val local = f.notes.filterNot(_.edge.evidence.id == body.ids(2))
    assert(local.exists(note => note.edge.evidence.id == body.ids(18) && (note.note.detail match
      case Detail.Evades(target, _) => target == white("knight", "c2")
      case _ => false)), "the real later Nc2 escape remains available as a negative witness")
    assert(local.exists(note => note.edge.evidence.id != body.ids(2) && (note.note.detail match
      case Detail.Evades(target, attacker) => target == white("knight", "g4") && attacker == black("pawn", "f6")
      case _ => false)), "an unchanged other-arm Ng4 escape also remains available")
    val answer = f.read(local = local)
    assertEquals(answer.question, "first_target_response")
    assert(!answer.completionSources.get.exists {
      case Action(_, _: Detail.Evades) => true
      case _ => false
    }, "no later or other-arm note can be reanchored as the first own reply")
    assert(!statements(answer).exists {
      case Action(move, _: Detail.Evades) => move.path eq body
      case Action(move, Detail.Attack(actor, target, true)) => (move.path eq body) && move.offset == 1 &&
        actor == black("pawn", "f6") && target == white("knight", "e5")
      case _ => false
    })
    assertLegacyExchangeDetail(f, answer)
    rootGradeAndOwnersUnchanged(f)

  test("actual MOVED Rxe4 keeps its capture prices and defense handoff without inventing an evasion"):
    val f = actual("FC-SG41-ND5-MOVED")
    val body = f.pair.head
    assertEquals(body.moves.take(3), List("c3d5", "f5e4", "d4e4"))
    val before = new MoveWorkProjection.Position(f.paths.positionView(body, 2).get)
    assertEquals(before.controllers("d4", chess.Black), Nil, "original Rd4 was not escaping an attack")
    assert(!f.notes.exists(note => note.edge.evidence.id == body.ids(2) && (note.note.detail match
      case _: Detail.Evades => true
      case _ => false)))
    val answer = f.read()
    assertEquals(answer.question, "first_target_response")
    assert(!statements(answer).exists {
      case Action(move, _: Detail.Evades) => (move.path eq body) && move.offset == 2
      case _ => false
    })
    assert(!answer.completionSources.get.exists {
      case Action(move, _: Detail.Evades) => (move.path eq body) && move.offset == 2
      case _ => false
    })
    assertEquals(statements(answer).flatMap(_.covered).filter(move =>
      (move.path eq body) && Set(1, 2)(move.offset)).map(_.offset).toSet, Set(1, 2))
    val controls = statements(answer).collect { case value: ControlState => value }
    assert(controls.exists(value => value.actor == white("rook", "e4") && !value.present &&
      value.target.contains(white("knight", "d5")) && value.remaining.isEmpty))
    assert(controls.exists(value => value.actor == white("rook", "d1") && value.present &&
      value.target.contains(white("knight", "d5"))))
    rootGradeAndOwnersUnchanged(f)
