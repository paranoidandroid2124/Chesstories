package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

class ReserveBreakTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val fen = "8/8/5pp1/4k2p/2P4P/4K1P1/8/8 b - - 0 47"
  private val actual = List("g6g5", "g3g4", "h5g4", "h4h5", "f6f5")
  private val alternative = List("e5e6", "e3d4", "e6d6")
  private val held = List("e5e6", "g3g4", "h5g4", "e3e4")
  private val decoy = List("e5e6", "g3g4", "h5g4", "h4h5", "g6h5")

  private def fixture(line: List[String] = actual, position: String = fen,
      alternativeLine: List[String] = alternative, heldLine: List[String] = held) =
    val lines = List(line, alternativeLine, heldLine, decoy)
    val history = CanonicalPositionHistory.from(position, Nil, position).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(moves => paths.certifiedPath(packet, moves).get)
    val batch = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, alternativeLine.head)),
      lines.map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "test-line"))), Nil, pvGuided = true)
    val generated = batch.additions.find(entry => entry.request.origin.question.producer == "C-reserve-response" &&
      entry.request.requirements.exists {
        case pair: CompareMoves => pair.id == "reserve_root" && pair.alternativeMove == alternativeLine.head
        case _ => false
      }).get
    def answer(left: Int, right: Int, offset: Int, relation: String) = CommentaryInvestigation.Answer(
      "synthetic-break-" + offset, "synthetic-break-key-" + offset,
      CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), relation,
      found(left).line.ref.id, found(right).line.ref.id, Json.obj(), Json.obj(),
      found(left).ids(offset), found(right).ids(offset), Some(PrincipalVariationComparison.QualityBand.Separated))
    val entry = CommentaryFactReader.read(packet, paths, List(generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered", answer = Some(answer(0, 1, 0, "better")))
      case reading if reading.requirementId == "reserve_response" => reading.copy(state = "answered", answer = Some(answer(1, 2, 1, "worse")))
      case reading => reading
    }))).head
    val proof = CommentaryIdea.reserveResponses(List(entry)).head
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    val selected = RootSubjectComparison.withReserveBreak(packet, paths, found.take(2), List(proof), previous)
    (packet, paths, found, proof, previous, selected)

  test("the immediate pawn sacrifice and new passer explain the lost guard before a distant reserve use"):
    val (_, paths, found, _, _, selected) = fixture()
    assertEquals(selected.question, "investigated_mechanism")
    val result = RootQuestionSummary.realize(paths, found.take(2), selected).get
    assert(result.text.contains("g4 hxg4 h5 continuation gives White a passed pawn on h5"), result.text)
    assert(result.text.contains("pawn on g6"), result.text)
    assert(result.text.contains("g4 evaluates below Kd4"), result.text)
    assert(result.text.contains("captures the white pawn on g4"), result.text)
    assert(!result.text.contains("forced") && !result.text.contains("must"), result.text)

  test("a different opposing pawn reply can let the break pawn capture on the same unguarded square"):
    val (_, paths, found, _, _, selected) = fixture(List("g6g5", "g3g4", "f6f5", "g4h5", "e5f6"))
    assertEquals(selected.question, "investigated_mechanism")
    val result = RootQuestionSummary.realize(paths, found.take(2), selected).get
    assert(result.text.contains("g4 f5 gxh5 continuation gives White a passed pawn on h5"), result.text)
    assert(result.text.contains("after the same g4, it still guards h5"), result.text)
    assert(result.text.contains("captures the black pawn on h5"), result.text)

  test("the lost guard persists when the advanced pawn itself captures the other pawn"):
    val (_, paths, found, _, _, selected) = fixture(List("g6g5", "g3g4", "g5h4", "g4h5", "h4h3"))
    assertEquals(selected.question, "investigated_mechanism")
    val result = RootQuestionSummary.realize(paths, found.take(2), selected).get
    assert(result.text.contains("g4 gxh4 gxh5 continuation gives White a passed pawn on h5"), result.text)
    assert(result.text.contains("captures the white pawn on h4"), result.text)
    assert(result.text.contains("captures the black pawn on h5"), result.text)

  test("an intervening quiet king move leaves the same lost pawn guard and counterbreak"):
    val (_, paths, found, _, _, selected) = fixture(List("g6g5", "g3g4", "e5e6", "g4h5", "g5h4"))
    assertEquals(selected.question, "investigated_mechanism")
    val result = RootQuestionSummary.realize(paths, found.take(2), selected).get
    assert(result.text.contains("g4 Ke6 gxh5 continuation gives White a passed pawn on h5"), result.text)
    assert(result.text.contains("after the same g4, it still guards h5"), result.text)
    assert(result.text.contains("captures the black pawn on h5"), result.text)
    assert(result.text.contains("captures the white pawn on h4"), result.text)
    assert(result.text.indexOf("captures the black pawn on h5") <
      result.text.indexOf("captures the white pawn on h4"), result.text)

  test("a captured pawn in the alternative reply does not hide the counterbreak question or its trade"):
    val (_, paths, found, _, _, selected) = fixture(position =
      "8/8/5pp1/p1p1k2p/P1P1P2P/4K1P1/8/8 b - - 0 44",
      alternativeLine = List("f6f5", "e4f5", "g6f5", "e3f3"),
      heldLine = List("f6f5", "g3g4", "f5g4", "e3d3"))
    assertEquals(selected.question, "investigated_mechanism")
    val result = RootQuestionSummary.realize(paths, found.take(2), selected).get
    assert(result.text.contains("g4 hxg4 h5 continuation gives White a passed pawn on h5"), result.text)
    assert(result.text.contains("g4 evaluates below exf5"), result.text)
    val actions = selected.core.flatMap(_.statements.flatMap(_.actionReferences))
    assert(Set(1, 2).subsetOf(actions.filter(_.path eq found(1)).map(_.offset).toSet),
      "the alternative's exf5 and gxf5 trade remains part of the basic answer")

  test("a verified counterbreak accompanies the terminal result without hiding its exchange prices"):
    val (packet, paths, found, proof, previous, _) = fixture(
      alternativeLine = List("e5d6", "e3d4", "d6c6", "c4c5", "g6g5", "h4g5", "f6g5", "d4e4",
        "c6c5", "e4f5", "h5h4", "g3h4", "g5h4", "f5g4", "c5d4", "g4h4"),
      heldLine = List("e5d6", "g3g4", "h5g4", "e3e4"))
    val terminal = previous.copy(question = "observed_terminal_comparison")
    assert(terminal.core.flatMap(_.statements).exists(_.isInstanceOf[CommentaryAnswer.Terminal]))
    val selected = RootSubjectComparison.withReserveBreak(packet, paths, found.take(2), List(proof), terminal)
    assertEquals(selected.question, terminal.question)
    assert(terminal.core.flatMap(_.statements).forall(selected.core.flatMap(_.statements).contains))
    assert(terminal.required.forall(selected.required.contains))
    val text = RootQuestionSummary.realize(paths, found.take(2), selected).get.text
    assert(text.contains("passed pawn on h5") && text.contains("game is drawn"), text)
    assert(text.indexOf("passed pawn on h5") < text.indexOf("game is drawn"), text)

  test("guard identity, new passer, exact reply path and occurrence all belong to the same proof"):
    val (_, paths, found, _, _, selected) = fixture()
    val value = selected.core.flatMap(_.statements).collectFirst { case value: CommentaryAnswer.ReserveBreak => value }.get
    for forged <- List(value.copy(guard = Piece("black", "pawn", "f6").witness),
        value.copy(passer = Piece("white", "pawn", "c4").witness), value.copy(offset = 1),
        value.copy(offset = 5), value.copy(held = found(3)), value.copy(otherReply = found(2)),
        value.copy(proof = value.proof.copy(answer = value.proof.answer.map(_.copy(alternativeOccurrenceId = "foreign"))))) do
      assert(!RootSubjectComparison.validReserveBreak(paths, forged))

  test("a pawn guard change without an inferior tested counterbreak does not establish importance"):
    val (packet, paths, found, proof, previous, _) = fixture()
    for relation <- List("equal", "better", "unknown") do
      val weaker = proof.copy(answer = proof.answer.map(_.copy(evaluationRelation = relation)))
      assertEquals(RootSubjectComparison.withReserveBreak(packet, paths, found.take(2), List(weaker), previous), previous)
    val close = proof.copy(answer = proof.answer.map(_.copy(qualityBand = Some(PrincipalVariationComparison.QualityBand.Close))))
    assertEquals(RootSubjectComparison.withReserveBreak(packet, paths, found.take(2), List(close), previous), previous)
    val existing = previous.copy(question = "selected_target_response")
    assertEquals(RootSubjectComparison.withReserveBreak(packet, paths, found.take(2), List(proof), existing), existing)
