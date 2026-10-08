package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

class ReserveTimingTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val fen = "8/5p2/8/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35"
  private val spent = List("g3g4", "f7f6", "f3e3")
  private val kept = List("f3e3", "f7f6", "e3f3", "e5d6", "f3e2", "d6e5", "e2e3")

  private def fixture(lines: List[List[String]]) =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, spent.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(moves => paths.certifiedPath(packet, moves).get)
    val batch = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, kept.head)),
      lines.map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "test-line"))), Nil, pvGuided = true)
    (packet, paths, found, batch)

  test("the first own decision tests the retained move against the observed king move after a common reply"):
    val (packet, paths, found, batch) = fixture(List(spent, kept))
    val generated = batch.additions.find(_.request.origin.question.producer == "C-reserve-timing").get
    val pair = generated.request.requirements.collectFirst { case value: CompareMoves if value.id == "reserve_timing" => value }.get
    assertEquals(pair, CompareMoves("reserve_timing", kept.take(2), spent.head, kept(2)))
    val answer = CommentaryInvestigation.Answer("synthetic-root", "synthetic-root-key",
      CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), "better",
      found(0).line.ref.id, found(1).line.ref.id, Json.obj(), Json.obj(), found(0).ids.head, found(1).ids.head,
      Some(PrincipalVariationComparison.QualityBand.Separated))
    val answered = generated.copy(readings = generated.readings.map(reading =>
      if reading.requirementId == "reserve_root" then reading.copy(state = "answered", answer = Some(answer)) else reading))
    val entry = CommentaryFactReader.read(packet, paths, List(answered)).head
    val timing = CommentaryIdea.reserveTimings(List(entry)).head
    assertEquals(timing.test, pair)
    assertEquals(timing.answer, None)
    // A missing/currently changed king continuation cannot retain this priority.
    val stale = entry.copy(readings = entry.readings.map(reading =>
      if reading.requirementId == "timing_current" then reading.copy(state = "not_applicable", result = None) else reading))
    assertEquals(CommentaryIdea.reserveTimings(List(stale)), Nil)

  test("a different opponent reply is first a comparison hypothesis, not the common timing decision"):
    val (_, _, _, batch) = fixture(List(spent, List("f3e3", "e5f6", "e3f4")))
    assert(batch.additions.exists(_.request.origin.question.producer == "C-reserve-response"))
    assert(!batch.additions.exists(_.request.origin.question.producer == "C-reserve-timing"))

  test("a later king move cannot replace the first decision's actual move"):
    val (_, _, _, batch) = fixture(List(spent, kept))
    val pairs = batch.additions.filter(_.request.origin.question.producer == "C-reserve-timing")
      .flatMap(_.request.requirements.collect { case value: CompareMoves if value.id == "reserve_timing" => value })
    assertEquals(pairs.distinct, List(CompareMoves("reserve_timing", kept.take(2), spent.head, kept(2))))

  private def meaningFixture() =
    val earlySpend = kept.take(2) :+ spent.head
    val otherRoute = List("f3e3", "f7f6", "e3e2", "e5d6", "e2d2", "d6e5", "d2e3")
    val (packet, paths, found, batch) = fixture(List(spent, kept, earlySpend, otherRoute))
    val generated = batch.additions.find(entry => entry.request.origin.question.producer == "C-reserve-timing" &&
      entry.request.requirements.exists {
        case pair: CompareMoves => pair.id == "reserve_timing" && pair.alternativeMove == "e3f3"
        case _ => false
      }).get
    def answer(left: Int, right: Int, offset: Int) = CommentaryInvestigation.Answer(
      "synthetic-tempo-" + offset, "synthetic-tempo-key-" + offset,
      CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), "better",
      found(left).line.ref.id, found(right).line.ref.id, Json.obj(), Json.obj(),
      found(left).ids(offset), found(right).ids(offset), Some(PrincipalVariationComparison.QualityBand.Separated))
    val entry = CommentaryFactReader.read(packet, paths, List(generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered", answer = Some(answer(0, 1, 0)))
      case reading if reading.requirementId == "reserve_timing" => reading.copy(state = "answered", answer = Some(answer(2, 1, 2)))
      case reading => reading
    }))).head
    val proof = CommentaryIdea.reserveTimings(List(entry)).head
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    val selected = RootSubjectComparison.withReserveTempo(packet, paths, found.take(2), List(proof), previous)
    (packet, paths, found, proof, previous, selected)

  test("a witnessed king manoeuvre passes the turn without spending the pawn, with a separate value test"):
    val (_, paths, found, _, _, selected) = meaningFixture()
    assertEquals(selected.question, "investigated_mechanism")
    val result = RootQuestionSummary.realize(paths, found.take(2), selected).get
    assert(result.text.contains("Kf3 evaluates above an immediate g4"), result.text)
    assert(result.text.contains("Kf3 Kd6 Ke2 Ke5 Ke3 restores the same piece placement with Black to move"), result.text)
    assert(result.text.contains("pawn stays on g3"), result.text)
    assert(!result.text.contains("forced") && !result.text.contains("must"), result.text)

  test("neither an arbitrary route nor the same side's turn can certify the tempo explanation"):
    val (_, paths, found, _, _, selected) = meaningFixture()
    val value = selected.core.head.statements.head.asInstanceOf[CommentaryAnswer.ReserveTempo]
    for forged <- List(value.copy(route = found(3)), value.copy(through = 6), value.copy(through = 5),
        value.copy(pawn = Piece("black", "pawn", "f7").witness), value.copy(spent = found(0))) do
      assert(!RootSubjectComparison.validReserveTempo(paths, forged))

  test("a quiet king cycle alone is not an explanation of importance"):
    val (packet, paths, found, proof, previous, _) = meaningFixture()
    for relation <- List("equal", "worse", "unknown") do
      val weakened = proof.copy(answer = proof.answer.map(_.copy(evaluationRelation = relation)))
      assertEquals(RootSubjectComparison.withReserveTempo(packet, paths, found.take(2), List(weakened), previous), previous)
    val close = proof.copy(answer = proof.answer.map(_.copy(qualityBand = Some(PrincipalVariationComparison.QualityBand.Close))))
    assertEquals(RootSubjectComparison.withReserveTempo(packet, paths, found.take(2), List(close), previous), previous)
