package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import play.api.libs.json.*
import CommentaryEnglish.at

/** Exact reports from the three real-screen jobs. No engine is run here. */
class MoveIntentTest extends munit.FunSuite:
  private lazy val fixture =
    val name = "judgment-evaluation/fixtures/public-commentary-v6/core-idea-browser.json.gz"
    val file = List(Path.of(name), Path.of("..", name)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(file))
    try Json.parse(stream.readAllBytes()) finally stream.close()

  private lazy val rows = (fixture \ "cases").as[List[JsObject]]

  private def replay(row: JsObject, investigate: Boolean = true, beforeReplies: Boolean = false): CommentaryJobState =
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((row \ "request").get).getBytes(UTF_8)).toOption.get
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(12, 10001L, request.engineProfile, investigateChoices = investigate,
        shortJob = true), 1L, request.inputLines, request.proposals).toOption.get
    val saved = (row \ "saved_reports").as[List[JsObject]]
    (if beforeReplies then saved.take(3) else if investigate then saved else saved.take(2)).zipWithIndex.foreach { (item, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork])
      val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
      assertEquals(work.executionKeySha256, (item \ "issued_work" \ "execution_key_sha256").as[String])
      val raw = (item \ "report").as[JsObject] ++ Json.obj("work_id" -> work.workId)
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(Json.stringify(raw).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    if !beforeReplies then
      assert(!state.isInstanceOf[AwaitingEngineWork], "Saved evidence must answer the work actually issued")
    state

  private lazy val states = rows.map(row => (row \ "id").as[String] -> replay(row)).toMap
  private def data(id: String) =
    val state = states(id)
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val reading = MoveIntent.read(packet, paths, state.responseSearches).get
    (state, packet, paths, reading)

  test("root preparation selects a useful action with an owned causal bridge in all three cases"):
    val expected = Map("kg2" -> MoveIntent.Aim.CheckingTempo,
      "rf3" -> MoveIntent.Aim.KingAttack, "re2" -> MoveIntent.Aim.RookCoordination)
    expected.foreach { (id, aim) =>
      val (state, packet, paths, value) = data(id)
      assertEquals(value.question.aim, aim)
      assert(value.complete)
      assertEquals(value.pair.toSet, ReferencePurposeReading.assessmentPair(packet, paths).toSet)
      assertEquals(value.question.rootMove, packet.assembly.input.playedMoveUci)
      assert(value.examples.forall(p => p.certified.nonEmpty && p.ids == p.declared))
      assert(value.choices.nonEmpty && value.replies.nonEmpty)
      assert(MoveIntent.probes(packet, paths, value.question, state.responseSearches).isEmpty)
      val answer = MoveIntent.summary(packet, paths, state.responseSearches).get
      val units = (answer.json \ "units").as[List[JsObject]]
      assert(!units.exists(u => (u \ "english_only").asOpt[Boolean].contains(true)))
      assert(units.exists(u => (u \ "covered_moves").as[JsArray].value.nonEmpty))
      assert(units.flatMap(u => (u \ "ko").as[List[JsValue]]).forall {
        case JsString(s) => s.nonEmpty
        case _ => true
      })
    }

  test("a legal conditional checking example is not promoted to a best-defense or evaluation claim"):
    val (state, packet, paths, value) = data("kg2")
    val q = value.question
    assertEquals(q.actor.role.name, "king")
    assertEquals(value.examples.size, 2)
    assert(value.examples.zip(q.checkOffsets).zipWithIndex.forall { case ((path, offset), i) =>
      paths.purposePosition(path, offset + 1).exists(p =>
        p.view.currentCheckSources(q.actor.side).nonEmpty == (i == 1))
    })
    val text = MoveIntent.summary(packet, paths, state.responseSearches).get.text
    assert(text.contains("conditional") && text.contains("not a claim of best defense"), text)
    assert(!text.contains("does not remain after this continuation"), text)

  test("prepared content cannot be transferred to another packet reader"):
    val (_, packet, paths, value) = data("rf3")
    val foreign = ObservedLineProtocol.prepare(packet)
    val answer = CommentaryAnswer.Answer("functional_response",
      List(CommentaryAnswer.Section(List(CommentaryAnswer.PreparedIdea(value)))), Nil, Nil)
    assert(RootQuestionSummary.realize(foreign, value.pair, answer).isEmpty)
    assert(value.belongsTo(paths) && !value.belongsTo(foreign))

  test("identical rook placement is not identical pressure when an own bishop blocks the file"):
    // The old root and action reports still have exactly the same work keys.
    // Stop before the replaced reply survey; do not transplant its old report.
    val state = replay((fixture \ "blocked_path").as[JsObject], beforeReplies = true)
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val value = MoveIntent.read(packet, paths, state.responseSearches).get
    val branch = value.root
    assertEquals(branch.moves(4), value.question.action)
    val position = paths.purposePosition(branch, 5).get
    val front = paths.trackedPieceAfter(branch, value.question.partner, 5).flatten.get
    val target = paths.trackedPieceAfter(branch, value.question.target, 5).flatten.get
    assert(!position.controlledBy(front)(target.at))
    assert(position.blockedRay(front, target).exists(_._2.exists(p => p.at == "e5" && p.role.name == "bishop")))
    val reopened = paths.purposePosition(branch, 7).get
    val afterExchange = paths.trackedPieceAfter(branch, value.question.partner, 7).flatten.get
    assert(reopened.controlledBy(afterExchange)(target.at), "the bishop exchange opens the same rook's pressure later")

  test("without admitted preparation examples the old unknown-purpose boundary remains"):
    val state = replay(rows.find(r => (r \ "id").as[String] == "rf3").get, investigate = false)
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.getOrElse(ObservedLineProtocol.prepare(packet))
    assert(paths.moveIntent.nonEmpty)
    assert(MoveIntent.read(packet, paths, state.responseSearches).isEmpty)

  test("a sparse played PV does not force a weak later move into the first-reply comparison"):
    val state = replay((fixture \ "reply_gap").as[JsObject], investigate = false)
    val packet = state.observationsPacket.get
    val paths = ObservedLineProtocol.prepare(packet)
    val q = paths.moveIntent.get
    val child = packet.assembly.input.positionHistory.extend(List(q.rootMove)).toOption.get
    assert(!q.root.moves.zipWithIndex.exists((m, n) => n >= 3 && n % 2 == 1 &&
      m != q.root.moves(1) && child.currentLegalMoveUcis.contains(m)))
    val reply = MoveIntent.probes(packet, paths, q, Nil).find(_._1.prefixMoves == List(q.rootMove)).get
    assertEquals(reply._2.toSet, child.currentLegalMoveUcis.toSet)
    val (_, surveyedPacket, surveyedPaths, surveyed) = data("re2")
    val search = surveyed.replies.get
    val childMoves = surveyedPacket.assembly.input.positionHistory.extend(List(surveyed.question.rootMove)).toOption.get.currentLegalMoveUcis
    assertEquals(search.work.rootRestriction, EngineRootRestriction.Restricted(childMoves.sorted))
    assertEquals(search.lines.size, 3)
    assert(MoveIntent.probes(surveyedPacket, surveyedPaths, surveyed.question, List(search))
      .forall(_._1.prefixMoves.size != 1), "one completed survey must not trigger another survey")

  test("naming an already explained capturing alternative does not create a duplicate price paragraph"):
    val state = replay((fixture \ "capture_comparison").as[JsObject], beforeReplies = true)
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val value = MoveIntent.read(packet, paths, state.responseSearches).get
    assertEquals(value.root.moves(2), "d4c6")
    val tested = value.choices.get.pathsByMove(value.root.moves(2))
    assert(tested ne value.root)
    assertEquals(tested.ids.take(3), value.root.ids.take(3))
    val result = MoveIntent.summary(packet, paths, state.responseSearches).get
    val units = (result.json \ "units").as[List[JsObject]]
    assert(!units.exists(u => (u \ "english_only").asOpt[Boolean].contains(true)))
    val actions = units.flatMap(u => (u \ "action_moves").as[List[JsObject]])
    assert(!actions.exists(r => (r \ "line_id").as[String] == tested.line.ref.id))
    val costs = units.flatMap(u => (u \ "covered_moves").as[List[JsObject]])
    val actualCaptures = value.root.certified.get._2.material.get.captures.filter(_.plyOffset <= 5)
    assert(actualCaptures.forall(c => costs.contains(RootQuestionSummary.MoveRef(value.root, c.plyOffset).json)))
