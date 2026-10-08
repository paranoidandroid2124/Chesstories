package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Actual root receipts establish the source of the first own action.
  * The legal supplied controls have no score or best-reply claim. Provider
  * geometry alone never establishes capture safety or the root's value.
  */
class TargetSupportThroughOwnActionTest extends munit.FunSuite:
  import CommentaryAnswer.ControlState
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import TargetResponseInvestigation.{Question, supportThroughOwnAction}

  private lazy val records =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/sg41-first-target-prefixes-produced.json.gz"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    val fixture = try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()
    (fixture \ "cases").as[List[JsObject]].map(value => (value \ "game_id").as[String] -> value).toMap

  private def request(id: String) = PositionCommentaryJobProtocol.parseJobRequest(
    Json.stringify((records(id) \ "request").get).getBytes(UTF_8)).toOption.get

  private def originalMover(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths): Question =
    TargetResponseInvestigation.discover(packet, "c3d5", paths)
      .find(question => question.rootMove == "c3d5" && question.originalTarget.square.key == "c3").get

  private def actual(id: String): (EvidenceBackedJudgmentPacket, PreparedPaths, Question, PreparedPath) =
    val input = request(id)
    val saved = (records(id) \ "saved_reports").as[List[JsObject]]
    var state = CommentaryJobReducer.startJob(input.initialFen, input.movePrefixUci,
      input.currentFen, input.playedMoveUci, input.resultingFen,
      CommentaryJobPolicy(16, 180001L, input.engineProfile,
        investigateChoices = true, previewDepth = Some(8)), 1L,
      input.inputLines, input.proposals).toOption.get
    saved.zipWithIndex.foreach { (value, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork])
      val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("e" * 32, input.requestId, 180001L, state))
      assertEquals((wire \ "issued_engine_work").as[JsObject], (value \ "issued_work").as[JsObject],
        "every actual report must match its complete issued descriptor before admission")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((value \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val fullMoves = (saved.last \ "report" \ "outcome" \ "line_suffixes").as[List[JsObject]]
      .map(row => (row \ "moves").as[List[String]]).find(_.headOption.contains("c3d5")).get
    val path = paths.certifiedPath(packet, fullMoves).get
    assert(path.certified.nonEmpty && path.ids == path.declared && path.moves == path.sourceMoves)
    (packet, paths, originalMover(packet, paths), path)

  private def supplied(moves: List[String]): (EvidenceBackedJudgmentPacket, PreparedPaths, Question, PreparedPath) =
    val input = request("FC-SG41-ND5-MOVED")
    val history = CanonicalPositionHistory.from(input.initialFen, input.movePrefixUci, input.currentFen).toOption.get
    val admitted = MoveReviewInputAdmission.admitIndependent(history, "c3d5",
      suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, moves))).get
    assertEquals(admitted.suppliedLines.map(_.acceptedMoves), List(moves))
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(admitted)).get
    val paths = ObservedLineProtocol.prepare(packet)
    (packet, paths, originalMover(packet, paths), paths.certifiedPath(packet, moves).get)

  private def white(role: String, square: String) = CommentaryFact.Piece("white", role, square).witness

  test("the same actual Nd5 target keeps original Rd1 when R4xe4 relocates its previous defender"):
    val (packet, paths, question, path) = actual("FC-SG41-ND5-MOVED")
    assertEquals(path.moves.take(3), List("c3d5", "f5e4", "d4e4"))
    assertEquals(paths.pieceAfter(packet, path, question.originalTarget, 2), Some(Some(white("knight", "d5"))))
    assertEquals(paths.pieceAfter(packet, path, question.originalTarget, 3), Some(Some(white("knight", "d5"))))
    assert(paths.positionView(path, 2).get.completeFacts(RelationFactKind.GeometricControl).nonEmpty)
    val before = new MoveWorkProjection.Position(paths.positionView(path, 2).get)
    val after = paths.positionView(path, 3).get
    val afterControls = new MoveWorkProjection.Position(after)
    assertEquals(before.controllers("d5", chess.White), List(white("rook", "d4")))
    assertEquals(afterControls.controllers("d5", chess.White), List(white("rook", "d1")))
    assert(after.completeFacts(RelationFactKind.GeometricControl).get.exists(_.semanticId ==
      "06acb0125a8dea3e623879be44a16efa6a64bbd0f152bf282f3e3a7aefac828b"),
      "the incoming defender has the canonical actual Rd1-to-d5 source, not a new target selected for the recapturer")
    val states: List[ControlState] = supportThroughOwnAction(packet, paths, question, path).get
    val loss = states.find(value => value.actor == white("rook", "e4") && !value.present).get
    assert(loss.move.path eq path)
    assertEquals(loss.move.offset, 2)
    assertEquals(loss.target, Some(white("knight", "d5")))
    assertEquals(loss.square, "d5")
    assertEquals(loss.remaining, None, "newly exposed Rd1 must not be worded as a retained defender")
    val gained = states.find(value => value.actor == white("rook", "d1") && value.present).get
    assert(gained.move.path eq path)
    assertEquals(gained.move.offset, 2)
    assertEquals(gained.target, Some(white("knight", "d5")))
    assert(states.forall(value => (value.move.path eq path) && value.move.offset <= 2 &&
      value.target.contains(white("knight", "d5"))), "all returned states retain the same surviving original target")

  test("HELD cannot replace the captured original knight with the rook that recaptures on its square"):
    val (packet, paths, question, path) = actual("FC-SG41-ND5-HELD")
    assertEquals(path.moves.take(3), List("c3d5", "e6d5", "d4d5"))
    assertEquals(paths.pieceAfter(packet, path, question.originalTarget, 2), Some(None))
    assertEquals(paths.pieceAfter(packet, path, question.originalTarget, 3), Some(None))
    assertEquals(supportThroughOwnAction(packet, paths, question, path), None,
      "an occupied d5 square is not the surviving original Nc3 target")

  test("an ordinary legal a4 does not fabricate loss of the stationary Rd4 support"):
    val (packet, paths, question, path) = supplied(List("c3d5", "f5e4", "a3a4"))
    val original = white("rook", "d4")
    val target = white("knight", "d5")
    (2 to 3).foreach { count =>
      val position = new MoveWorkProjection.Position(paths.positionView(path, count).get)
      assert(position.complete(RelationFactKind.GeometricControl))
      assert(position.controlledBy(original)("d5"))
      assert(!position.controlledBy(white("rook", "d1"))("d5"))
    }
    val states = supportThroughOwnAction(packet, paths, question, path).get
    assert(!states.exists(value => !value.present && value.actor == original && value.target.contains(target)))
    assert(!states.exists(value => value.remaining.exists(_.contains(white("rook", "d1"))) ||
      value.present && value.actor == white("rook", "d1")),
      "a different own move cannot borrow the Rd1 ray opened by R4xe4")

  test("a genuinely retained defender is distinct from the Rd1 provider that can be newly exposed"):
    // This composed board adds Nf4 and a d3 blocker. No engine scores or
    // historical receipt from the actual SG41 position are attributed to it.
    val fen = "3k3r/3bbp2/p2p1p2/1pr2p1p/3RPN1P/P1NP1B2/1PP3P1/1K1R4 w - - 0 21"
    val moves = List("c3d5", "f5e4", "d4e4")
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val admitted = MoveReviewInputAdmission.admitIndependent(history, "c3d5",
      suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, moves))).get
    assertEquals(admitted.suppliedLines.map(_.acceptedMoves), List(moves))
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(admitted)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val path = paths.certifiedPath(packet, moves).get
    val question = originalMover(packet, paths)
    val before = new MoveWorkProjection.Position(paths.positionView(path, 2).get)
    val after = new MoveWorkProjection.Position(paths.positionView(path, 3).get)
    assert(before.complete(RelationFactKind.GeometricControl) && after.complete(RelationFactKind.GeometricControl))
    assertEquals(before.controllers("d5", chess.White).toSet, Set(white("rook", "d4"), white("knight", "f4")))
    assertEquals(after.controllers("d5", chess.White), List(white("knight", "f4")))
    val states = supportThroughOwnAction(packet, paths, question, path).get
    val loss = states.find(value => value.actor == white("rook", "e4") && !value.present).get
    assertEquals(loss.remaining, Some(List(white("knight", "f4"))))
    assert(!states.exists(value => value.actor == white("rook", "d1") && value.present))

  test("a short or foreign exact-looking path supplies no completed support change"):
    val (packet, paths, question, path) = actual("FC-SG41-ND5-MOVED")
    val foreign = ObservedLineProtocol.prepare(packet).certifiedPath(packet, path.moves).get
    assertEquals(foreign.ids, path.ids)
    assertEquals(foreign.positions, path.positions)
    assert(!(foreign eq path))
    assertEquals(supportThroughOwnAction(packet, paths, question, foreign), None)
    val (shortPacket, shortPaths, shortQuestion, shortPath) = supplied(List("c3d5", "f5e4"))
    assertEquals(supportThroughOwnAction(shortPacket, shortPaths, shortQuestion, shortPath), None)
    assertEquals(supportThroughOwnAction(packet, paths,
      question.copy(rootMoveOccurrenceId = "external-root"), path), None)

  test("unknown before or after control resources never become an empty defender inventory"):
    val (packet, paths, _, path) = actual("FC-SG41-ND5-MOVED")
    List(2, 3).foreach { count =>
      val missing = EvidenceBackedJudgmentPacket.fromAssembly(packet.assembly.copy(
        positionAnalyses = packet.assembly.positionAnalyses - path.positions(count))).get
      val prepared = ObservedLineProtocol.prepare(missing)
      val owned = prepared.certifiedPath(missing, path.moves).get
      val question = originalMover(missing, prepared)
      assertEquals(supportThroughOwnAction(missing, prepared, question, owned), None,
        s"missing count$count resources leave the condition unconfirmed")
    }
