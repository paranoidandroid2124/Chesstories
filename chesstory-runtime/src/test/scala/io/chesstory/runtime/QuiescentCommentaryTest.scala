package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import play.api.libs.json.*
import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import CommentaryAnswer.*
import RootQuestionSummary.MoveRef

class QuiescentCommentaryTest extends munit.FunSuite:
  private def prepare(input: RawMoveReviewInput) =
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(
      MoveReviewAssembler.assemble(MoveReviewInputAdmission.admit(input).get)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    (packet, paths, pair, positions)

  test("actual pair summaries share physical costs while preserving check replies, root support and unpaid rejection"):
    val name = "judgment-evaluation/fixtures/public-commentary-v6/quiescent-browser.json.gz"
    val file = List(Path.of(name), Path.of("..", name)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(file))
    val rows = try (Json.parse(stream.readAllBytes()) \ "positions").as[List[JsValue]] finally stream.close()
    rows.filter(row => Set("existing", "new")((row \ "id").as[String])).foreach { row =>
      val request = PositionCommentaryJobProtocol.parseJobRequest(
        Json.stringify((row \ "request").get).getBytes(UTF_8)).toOption.get
      val lines = (row \ "report" \ "outcome" \ "line_suffixes").as[List[JsValue]].map(v =>
        EngineLine((v \ "moves").as[List[String]], (v \ "white_score" \ "value").as[Int], None,
          (v \ "depth").as[Int]))
      val (packet, paths, pair, positions) = prepare(RawMoveReviewInput(fen = request.currentFen,
        playedMoveUci = request.playedMoveUci, variations = lines, movePrefixUci = request.movePrefixUci))
      val reading = ReferencePurposeReading.read(packet, paths, pair).getOrElse(fail("actual pair has no reading"))
      val notes = MoveWorkProjection.select(packet, paths).local
      def complete(a: Answer) = MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, a)
      val answer = ReferencePurposeReading.answer(packet, paths, pair, reading, complete).get
      val result = RootQuestionSummary.realize(paths, pair, answer).get
      val units = (result.json \ "units").as[List[JsValue]]
      assert(!units.exists(u => (u \ "english_only").asOpt[Boolean].contains(true)), result.text)
      assert(!result.text.contains("recorded continuation is"), result.text)
      assert(!result.text.contains("exchanged away"), result.text)
      assertEquals(answer.core.flatMap(_.covered).toSet, reading.arms.flatMap(arm =>
        arm.path.certified.get._2.material.get.captures.filter(_.plyOffset <= arm.through)
          .map(e => MoveRef(arm.path, e.plyOffset))).toSet)
      assert(RootQuestionSummary.realize(paths, pair, answer.copy(core = answer.core.map(s =>
        s.copy(statements = s.statements.filterNot(_.isInstanceOf[Material]))))).isEmpty)
      assertEquals(complete(answer).get.required.toSet, answer.required.toSet)
      if (row \ "id").as[String] == "existing" then
        assert(result.text.contains("in both lines"), result.text)
        assert(result.text.contains("pair of bishops") && result.text.contains("pair of knights"), result.text)
        assert(reading.arms.map(_.losses.toSet).distinct.size == 2, "shared cost must retain the different extra exchange")
      else
        assert(reading.reference.supportUse.exists(use => use.provider.square.key == "h4" &&
          use.target.square.key == "g5" && use.recapture.plyOffset == 4))
        val checks = answer.core.flatMap(_.statements).collect { case c: CheckSequence => c.events }.flatten
        assert(checks.exists { case c: CheckReply => c.move.offset == 3; case _ => false },
          "Qd3+ must retain the quiet g6 interposition")
        assert(result.text.contains("then recaptures with the pawn"), result.text)
        assert(reading.played.losses.exists(p => p.square.key == "d3" && p.role.name == "bishop"))
        assert(reading.played.losses.exists(p => p.square.key == "h7" && p.role.name == "pawn"))
    }

  test("the same new position keeps root clearance or supported arrivals when the automatic PV changes"):
    val name = "judgment-evaluation/fixtures/public-commentary-v6/quiescent-browser.json.gz"
    val file = List(Path.of(name), Path.of("..", name)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(file))
    val rows = try (Json.parse(stream.readAllBytes()) \ "positions").as[List[JsValue]] finally stream.close()
    rows.filter(row => Set("new_route", "new_advance")((row \ "id").as[String])).foreach { row =>
      val request = PositionCommentaryJobProtocol.parseJobRequest(
        Json.stringify((row \ "request").get).getBytes(UTF_8)).toOption.get
      val lines = (row \ "report" \ "outcome" \ "line_suffixes").as[List[JsValue]].map(v =>
        EngineLine((v \ "moves").as[List[String]], (v \ "white_score" \ "value").as[Int], None,
          (v \ "depth").as[Int]))
      val (packet, paths, _, _) = prepare(RawMoveReviewInput(fen = request.currentFen,
        playedMoveUci = request.playedMoveUci, variations = lines, movePrefixUci = request.movePrefixUci))
      val candidates = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsValue]]
      val question = candidates.flatMap(c => (c \ "detail" \ "summary_question").asOpt[JsObject]).headOption.get
      val units = (question \ "units").as[List[JsValue]]
      assert(!units.exists(u => (u \ "english_only").asOpt[Boolean].contains(true)), Json.stringify(question))
      val summary = candidates.flatMap(c => (c \ "detail" \ "summary").asOpt[String]).mkString(" ")
      if (row \ "id").as[String] == "new_route" then
        assert(summary.contains("clears h2"), summary)
        assert(summary.contains("Rh3"), summary)
        assert(summary.contains("g7"), summary)
      else assert(summary.contains("supports the pawn's arrival on g5"), summary)
      assert(summary.contains("Qd3+"), summary)
      assert(summary.contains("original d3 bishop"), summary)
      assert(!summary.contains("recorded continuation is"), summary)
    }

  test("only complete direct exchanges and quiet check-king sequences are abbreviated"):
    def line(fen: String, moves: String, alternative: String) =
      val rows = List(moves, alternative).map(v => EngineLine(v.split(" ").toList, 0, None, 8))
      val (packet, paths, _, positions) = prepare(RawMoveReviewInput(fen = fen,
        playedMoveUci = rows.head.moves.head, variations = rows))
      val pair = rows.map(row => paths.paths.find(_.moves == row.moves).get)
      (paths, pair, RootQuestionSummary.prepare(packet, paths, pair, positions).get.head)
    val (paths, pair, arm) = line("2r1r2k/p2n1qp1/1p3pRp/3P4/2P5/P2Q4/1B4PP/5RK1 w - - 5 31",
      "b2f6 d7f6 f1f6 g7f6", "d3d1")
    val physical = arm.captures
    val events = arm.material.events
    val text = CommentarySequenceText.exchange(pair.head, events, ko = false, Some(physical)).get
    assertEquals(text.collect { case m: MoveRef => m.offset }, List(0, 3))
    val words = text.collect { case s: String => s }.mkString
    List("b2 bishop", "f1 rook", "d7 knight", "f6 pawn").foreach(s =>
      assert(words.contains(s), words))
    assert(CommentarySequenceText.exchange(pair.head, List(events.head, events.last), ko = false).isEmpty)
    assert(CommentarySequenceText.exchange(pair.head, events.take(2), ko = false).isEmpty,
      "do not present an unfinished recapture prefix as the local exchange outcome")
    assert(CommentarySequenceText.exchange(pair.head, events, ko = false,
      Some(physical.updated(1, physical(1).copy(originalVictim = physical.last.originalVictim)))).isEmpty)
    val (checkPaths, checkPair, checkArm) = line("7k/8/8/8/8/8/8/R5K1 w - - 0 1",
      "a1a8 h8h7 a8a7 h7h6", "g1f1")
    val sequence = CheckSequence(MoveRef(checkPair.head, 0), checkArm.checkEvents(List(0, 2)))
    val summary = CommentarySequenceText.checks(sequence, ko = false).get
    assertEquals(summary.collect { case m: MoveRef => m.offset }, List(0, 3))
    assertEquals(CommentarySequenceText.checks(sequence, ko = true).get.collect { case m: MoveRef => m },
      summary.collect { case m: MoveRef => m })
    assert(CommentarySequenceText.checks(sequence.copy(events = sequence.events.dropRight(1)), ko = false).isEmpty,
      "a finite checked endpoint is not a completed local sequence")
