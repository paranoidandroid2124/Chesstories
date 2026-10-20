package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import play.api.libs.json.*
import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.EvidenceBackedJudgmentPacket
import lila.chessjudgment.model.line.EngineLine

/** Same two saved screen reports, so a different PV cannot masquerade as editing. */
class CoreSelectionTest extends munit.FunSuite:
  private val root = List(Path.of("."), Path.of("..")).find(p =>
    Files.isDirectory(p.resolve("judgment-evaluation"))).get
  private val output = Files.createDirectories(root.resolve("target/research-runs/core-selection-20261013"))
  private lazy val rows =
    val stream = new GZIPInputStream(Files.newInputStream(root.resolve(
      "judgment-evaluation/fixtures/public-commentary-v6/core-selection-browser.json.gz")))
    try (Json.parse(stream.readAllBytes()) \ "positions").as[List[JsValue]] finally stream.close()

  private def prepare(row: JsValue) =
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((row \ "request").get).getBytes(UTF_8)).toOption.get
    val lines = (row \ "report" \ "outcome" \ "line_suffixes").as[List[JsValue]].map(v =>
      EngineLine((v \ "moves").as[List[String]], (v \ "white_score" \ "value").as[Int], None,
        (v \ "depth").as[Int]))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = request.currentFen,
      playedMoveUci = request.playedMoveUci, variations = lines, movePrefixUci = request.movePrefixUci)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    (packet, paths)

  for id <- List("existing", "new") do test(s"$id: keep the actual question, costs and reply while selecting its core") {
    val row = rows.find(r => (r \ "id").as[String] == id).get
    val (packet, paths) = prepare(row)
    val projection = MoveWorkProjection.select(packet, paths)
    val candidates = (projection.json \ "candidates").as[List[JsValue]]
    val question = candidates.flatMap(c => (c \ "detail" \ "summary_question").asOpt[JsObject]).headOption.get
    val units = (question \ "units").as[List[JsValue]]
    assert(!units.exists(u => (u \ "english_only").asOpt[Boolean].contains(true)))
    val before = (row \ "before" \ "units").as[List[JsValue]]
    def receipts(values: List[JsValue], key: String) = values.flatMap(v =>
      (v \ key).asOpt[List[JsValue]].getOrElse(Nil)).toSet
    List("covered_moves", "required_moves", "proof_moves", "recorded_end_moves").foreach(key =>
      assertEquals(receipts(units, key), receipts(before, key), s"$id lost $key"))
    def prose(values: List[JsValue], language: String) = values.map(u =>
      (u \ language).as[List[JsValue]].map {
        case JsString(text) => text
        case move =>
          val path = paths.paths.find(_.line.ref.id == (move \ "line_id").as[String]).get
          val n = path.ids.indexOf((move \ "occurrence_id").as[String])
          paths.notation.move(path.moves.take(n), path.moves(n))
      }.mkString).mkString("\n\n")
    val ko = prose(units, "ko")
    val en = prose(units, "en")
    assertEquals((question \ "question").as[String], "selected_comparison")
    if id == "existing" then
      List("c6", "Bd5", "Nge4", "백 b5 폰", "비숍 둘", "나이트 둘", "고립된 더블폰", "열린 c 파일", "흑 c6 폰의 지원", "흑 b7 폰을 지킵니다").foreach(s =>
        assert(ko.contains(s), ko))
      assertEquals(ko.sliding("백 g2 비숍에서 흑 b7 폰".length).count(_ == "백 g2 비숍에서 흑 b7 폰"), 1)
      assert(!ko.contains("그 칸을 차지합니다") && !ko.contains("원래 b6의 나이트"), ko)
      val pair = ReferencePurposeReading.assessmentPair(packet, paths)
      val reading = ReferencePurposeReading.read(packet, paths, pair).get
      val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
      val answer = ReferencePurposeReading.answer(packet, paths, pair, reading, a =>
        MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, projection.local, a)).get
      val statements = answer.core.head.statements
      val joined = statements.collect { case a: CommentaryAnswer.Action => a }.flatMap(a =>
        CommentarySequenceText.pursuit(statements, a, ko = true)).find(r =>
          val offsets = r.expressed.flatMap(_.references.map(_.offset))
          offsets.last - offsets.head > 1).get
      val first = joined.expressed.head.asInstanceOf[CommentaryAnswer.Action]
      assert(CommentarySequenceText.pursuit(statements.filterNot(_.isInstanceOf[CommentaryAnswer.Material]), first,
        ko = true).isEmpty, "an intervening capture cannot be silently folded without its price")
    else
      List("g5에 진출하는 나이트를 지원", "폰이 되잡", "f3 나이트", "h2 폰", "e7 비숍", "d3 비숍", "h7 폰", "Kxh7", "Qd3+", "g6", "체크를 막", "d3 칸을 비우", "백 f3 나이트를 남깁니다").foreach(s =>
        assert(ko.contains(s), ko))
      assertEquals(ko.sliding("Kxh7".length).count(_ == "Kxh7"), 1)
      assert(!ko.contains("d1 퀸을 남깁니다"), ko)
    Files.writeString(output.resolve(s"$id-rendered.json"), Json.prettyPrint(question ++ Json.obj("display_ko" -> ko, "display_en" -> en,
      "before_ko" -> prose(before, "ko"), "before_en" -> prose(before, "en"))), UTF_8)
  }

  test("a finite live line ending with an opposing occupant is an attack, not allied support"):
    val (packet, paths) = prepare(rows.find(r => (r \ "id").as[String] == "existing_hostile").get)
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val reading = ReferencePurposeReading.read(packet, paths, pair).get
    assert(reading.played.occupant.exists(_.side != reading.played.actor.side))
    assert(reading.played.remainingGuards.nonEmpty)
    val ko = ReferencePurposeText.position(reading, reading.played, ko = true).collect { case s: String => s }.mkString
    val en = ReferencePurposeText.position(reading, reading.played, ko = false).collect { case s: String => s }.mkString
    assert(ko.contains("백 d5 나이트는 이제 흑 c6 폰의 공격을 받습니다"), ko)
    assert(en.contains("attacked by"), en)
    assert(!ko.contains("흑 c6 폰의 지원"), ko)
    val lead = ReferencePurposeText.purpose(reading, ko = true).collect { case s: String => s }.mkString
    assert(lead.contains("d5에 진출하는 비숍을 지원"), lead)
    assert(!lead.contains("c6 폰이 d5 진출을 받칩니다"), lead)

  test("a quiet preparation reaches its actual late support use without reciting every waiting move"):
    val (packet, paths) = prepare(rows.find(r => (r \ "id").as[String] == "new_quiet_setup").get)
    val candidates = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsValue]]
    val detail = candidates.flatMap(c => (c \ "detail").asOpt[JsObject])
      .find(d => (d \ "summary_question").toOption.nonEmpty).get
    val question = (detail \ "summary_question").as[JsObject]
    val units = (question \ "units").as[List[JsValue]]
    val text = (detail \ "summary").as[String]
    assertEquals((question \ "question").as[String], "selected_comparison")
    assert(!units.exists(u => (u \ "english_only").asOpt[Boolean].contains(true)), text)
    assert(text.contains("supports the knight's arrival on g5"), text)
    List("Qa5+", "Bd2", "Ng5", "pawn on e6", "pawn on h7").foreach(s => assert(text.contains(s), text))
    assert(text.contains("original d3 bishop") && text.contains("original h7 pawn"), text)
    assert(text.contains("g6") && text.contains("No further reply is recorded"), text)
    assert(!text.contains("recorded continuation is"), text)
    val end = ReferencePurposeReading.assessmentPair(packet, paths).find(_.moves.head == "h2h4").get
    assert(units.flatMap(u => (u \ "recorded_end_moves").asOpt[List[JsValue]].getOrElse(Nil))
      .contains(RootQuestionSummary.MoveRef(end, 8).json))
    assertEquals(units.flatMap(u => (u \ "covered_moves").as[List[JsValue]]).distinct.size, 2)
    Files.writeString(output.resolve("quiet-setup-rendered.json"),
      Json.prettyPrint(question ++ Json.obj("display_en" -> text)), UTF_8)

  test("unchanged support of an empty square belongs to the purpose only"):
    val (packet, paths) = prepare(rows.find(r => (r \ "id").as[String] == "existing_empty_post").get)
    val reading = ReferencePurposeReading.read(packet, paths,
      ReferencePurposeReading.assessmentPair(packet, paths)).get
    val arm = reading.reference
    assert(arm.occupant.isEmpty)
    assertEquals(arm.guards.toSet, arm.remainingGuards.toSet)
    val lead = ReferencePurposeText.purpose(reading, ko = true).collect { case s: String => s }.mkString
    val after = ReferencePurposeText.position(reading, arm, ko = true).collect { case s: String => s }.mkString
    assert(lead.contains("흑 c6 폰이 d5 진출을 받칩니다"), lead)
    assert(!after.contains("d5 칸은 흑 c6 폰의 지원"), after)
    List("흑 b6 나이트를 남깁니다", "더블폰", "통과폰").foreach(s => assert(after.contains(s), after))

  test("a located purpose user is not a second positional fact without a remaining task or contrast"):
    val (packet, paths) = prepare(rows.find(r => (r \ "id").as[String] == "new_pawn").get)
    val candidates = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsValue]]
    val detail = candidates.flatMap(c => (c \ "detail").asOpt[JsObject])
      .find(d => (d \ "summary_question").toOption.nonEmpty).get
    val text = (detail \ "summary").as[String]
    assertEquals((detail \ "summary_question" \ "question").as[String], "selected_comparison")
    assert(text.contains("supports the pawn's arrival on g5"), text)
    assert(text.contains("clears d3") && text.contains("Qd3+") && text.contains("Kg8"), text)
    assert(text.contains("original d3 bishop") && text.contains("original h7 pawn"), text)
    assert(!text.contains("White's queen on d3 remains"), text)
