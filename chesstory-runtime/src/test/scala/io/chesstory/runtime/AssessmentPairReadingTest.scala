package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import play.api.libs.json.*
import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.EvidenceBackedJudgmentPacket
import lila.chessjudgment.model.line.EngineLine

/** The two actual screen requests, with their original engine reports. */
class AssessmentPairReadingTest extends munit.FunSuite:
  override def munitTimeout = 2.minutes
  private lazy val fixture =
    val name = "judgment-evaluation/fixtures/public-commentary-v6/assessment-pair-browser.json.gz"
    val file = List(Path.of(name), Path.of("..", name)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(file))
    try Json.parse(stream.readAllBytes()) finally stream.close()

  private def read(id: String, reportIndex: Int = 2) =
    val row = (fixture \ "baseline").as[List[JsObject]].find(r => (r \ "id").as[String] == id).get
    val http = (row \ "http").as[List[JsObject]]
    prepare((http.head \ "request").get, (http(reportIndex) \ "request").get)

  private def prepare(rawRequest: JsValue, report: JsValue) =
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify(rawRequest).getBytes(UTF_8)).toOption.get
    // Rebuild the two actual assessed engine lines. This tests their meaning,
    // not a replay of a scheduler whose unnecessary third-root job is removed.
    val lines = (report \ "outcome" \ "line_suffixes").as[List[JsValue]].take(2).map(row =>
      EngineLine((row \ "moves").as[List[String]], (row \ "white_score" \ "value").as[Int], None,
        (row \ "depth").as[Int]))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = request.currentFen,
      playedMoveUci = request.playedMoveUci, variations = lines, movePrefixUci = request.movePrefixUci)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val reading = ReferencePurposeReading.read(packet, paths, pair).get
    val notes = MoveWorkProjection.select(packet, paths).local
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val answer = ReferencePurposeReading.answer(packet, paths, pair, reading,
      value => MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, value)).get
    val result = RootQuestionSummary.realize(paths, pair, answer).get
    assert(!(result.json \ "units").as[List[JsValue]].exists(u => (u \ "english_only").asOpt[Boolean].contains(true)), result.text)
    val foreign = ObservedLineProtocol.prepare(packet)
    assert(ReferencePurposeReading.answer(packet, foreign, pair, reading, Some(_)).isEmpty)
    assert(ReferencePurposeReading.read(packet, paths, pair.take(1)).isEmpty)
    val unpaid = answer.copy(core = answer.core.map(s => s.copy(statements = s.statements.filterNot {
      case _: CommentaryAnswer.Material | _: CommentaryAnswer.MaterialEvents => true
      case _ => false
    })))
    assert(RootQuestionSummary.realize(paths, pair, unpaid).isEmpty)
    for arm <- reading.arms if arm.through == arm.path.moves.size - 1 do
      assert(answer.core.flatMap(_.statements).contains(CommentaryAnswer.RecordedEnd(RootQuestionSummary.MoveRef(arm.path, arm.through))))
    reading

  test("Rb8/Bd5: b7 duty survives the complete exchange through different pieces"):
    val reading = read("existing")
    assertEquals(reading.arms.map(_.path.moves.head), List("a8b8", "b3d5"))
    assert(reading.reference.protections.exists(_.target.square.key == "b7"))
    assert(reading.played.shields.exists((attacker, target) =>
      attacker.square.key == "g2" && target.square.key == "b7"))
    assert(reading.reference.functions.exists(f => f.original.square.key == "b6" &&
      f.current.exists(_.square.key == "d5") && f.supports.exists(_.square.key == "c3")))
    assert(reading.played.functions.exists(f => f.original.square.key == "b6" && f.current.isEmpty))
    assert(reading.contrasts.exists(c => c.reference.original.square.key == "b6" &&
      c.reference.supports.exists(_.square.key == "c3") && c.played.current.isEmpty))
    assert(reading.arms.forall(_.functions.exists(f => f.original.square.key == "b3" && f.current.isEmpty)),
      "Rb8 does not preserve the bishop after Qxb3")
    assertEquals(reading.played.through, 6)
    assert(reading.played.pawns.exists(p => p.original.square.key == "e5" && p.current.square.key == "d4"))
    assert(reading.reference.pawns.exists(p => p.original.square.key == "e5" && p.current.square.key == "c3" &&
      p.passed.contains(true)))
    val declined = read("existing", reportIndex = 1)
    assert(declined.played.replyShields.exists(_.square.key == "g2"),
      "Nge4 interposes against the bishop's attack instead of exchanging it immediately")
    val endpoint = (fixture \ "short_endpoint").get
    val finite = prepare((endpoint \ "request").get, (endpoint \ "report").get)
    assertEquals(finite.played.through, finite.played.path.moves.size - 1)
    assertEquals(finite.arms.map(_.path.moves.head), List("c7c6", "b3d5"))

  test("Nd6/Bxf3: the same physical exchange can be delayed to activate the rook first"):
    val reading = read("new")
    assertEquals(reading.arms.map(_.path.moves.head), List("e4d6", "g4f3"))
    assert(reading.sharedCapture.exists(s => s.actor.square.key == "g4" && s.victim.square.key == "f3" &&
      s.referenceOffset == 4 && s.playedOffset == 0))
    assert(reading.reference.functions.exists(f => f.original.square.key == "d8" && f.openFile.contains("c")))
    assertEquals(reading.reference.through, reading.played.through,
      "Do not compare the developed rook against a deliberately earlier checkpoint")
    assert(reading.contrasts.exists(c => c.reference.original.square.key == "d8" &&
      c.reference.openFile.contains("c") && c.played.openFile.isEmpty))
    assert(reading.played.functions.exists(f => f.original.square.key == "d8" && f.current.exists(_.square.key == "d8") &&
      f.openFile.isEmpty))
    assert(reading.arms.forall(_.pawns.exists(p => p.original.square.key == "g2" && p.current.square.key == "f3" &&
      p.doubled.exists(_.square.key == "f2"))))
    val changed = (fixture \ "different_exchange").get
    val different = prepare((changed \ "request").get, (changed \ "report").get)
    assert(different.sharedCapture.isEmpty, "Bxf5/Nxf5 is not the same physical exchange as Bxf3/gxf3")
    assert(different.contrasts.exists(c => c.reference.original.square.key == "d8" &&
      c.reference.openFile.contains("c") && c.played.openFile.isEmpty))
    assert(different.reference.pawns.exists(p => p.original.square.key == "g2" &&
      p.current.square.key == "g2" && p.doubled.isEmpty))
    assert(different.played.pawns.exists(p => p.original.square.key == "g2" &&
      p.current.square.key == "f3" && p.doubled.nonEmpty))
