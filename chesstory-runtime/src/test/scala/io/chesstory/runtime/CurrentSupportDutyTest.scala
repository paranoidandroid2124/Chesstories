package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

class CurrentSupportDutyTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val fen = "r1b1r1k1/pp1nqpbp/3p1np1/2pP4/4PP2/2N2N2/PPQ1B1PP/R1B2RK1 w - - 9 12"
  private val actual = List("f3d2", "d7b6", "e2b5", "c8d7", "a2a4")
  private val alternative = List("f1e1", "d7b6", "e2b5", "c8d7", "a2a4")

  private def fixture(played: List[String] = actual, other: List[String] = alternative) =
    val lines = List(played, other)
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, played.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(moves => paths.certifiedPath(packet, moves).get)
    val batch = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, alternative.head)),
      lines.map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "test-line"))), Nil, pvGuided = true)
    (packet, paths, found, batch)

  test("lost support asks about the current target departure before following its later plan"):
    val (packet, paths, found, batch) = fixture()
    val generated = batch.additions.find(_.request.origin.question.producer == "C-current-support-duty").get
    val root = CommentaryInvestigation.Answer("synthetic-root", "synthetic-root-key",
      CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), "better",
      found(0).line.ref.id, found(1).line.ref.id, Json.obj(), Json.obj(), found(0).ids.head, found(1).ids.head,
      Some(PrincipalVariationComparison.QualityBand.Close))
    val filled = generated.copy(readings = generated.readings.map(reading =>
      if reading.requirementId == "duty_root" then reading.copy(state = "answered", answer = Some(root)) else reading))
    val entry = CommentaryFactReader.read(packet, paths, List(filled)).head
    val duty = CommentaryIdea.currentDuties(List(entry)).head
    assertEquals(duty.actual, CompareMoves("duty_current_choice", actual.take(2), "a2a4", "e2b5"))
    assertEquals(duty.alternative, CompareMoves("duty_other_choice", alternative.take(2), "a2a4", "e2b5"))
    assertEquals(duty.actualAnswer, None)
    val support = entry.request.requirements.collectFirst { case value: Fact if value.id == "duty_before" => value.predicate }.get
    assertEquals(support, Predicate("support", Some(Piece("white", "queen", "c2")), Some(Piece("white", "bishop", "e2"))))
    for id <- List("duty_current", "duty_current_reply", "duty_after_root", "duty_other_support", "duty_observed_plan") do
      val stale = entry.copy(readings = entry.readings.map(reading =>
        if reading.requirementId == id then reading.copy(state = "not_applicable", result = None) else reading))
      assertEquals(CommentaryIdea.currentDuties(List(stale)), Nil)

  test("a target that does not leave on the first own response cannot infer an unseen defensive duty"):
    val (_, _, _, batch) = fixture(List("f3d2", "d7b6", "a2a4", "f6d5"))
    assert(!batch.additions.exists(_.request.origin.question.producer == "C-current-support-duty"))

  test("restoring support is also a current adjustment; the target need not retreat"):
    val (_, _, _, batch) = fixture(List("f3d2", "d7b6", "f1e1", "c8g4", "a2a4"))
    val entries = batch.additions.filter(_.request.origin.question.producer == "C-current-support-duty")
    assert(entries.exists(_.request.requirements.exists {
      case value: CompareMoves => value == CompareMoves("duty_current_choice", actual.take(2), "a2a4", "f1e1")
      case _ => false
    }))
    assert(entries.exists(_.request.requirements.exists {
      case value: Fact => value.id == "duty_adjustment" && value.predicate ==
        Predicate("support", Some(Piece("white", "rook", "f1")), Some(Piece("white", "bishop", "e2")))
      case _ => false
    }))

  test("a refreshed root PV does not assign the old plan occurrence to a different arm"):
    val (_, _, _, first) = fixture(actual.take(3))
    val (packet, paths, _, _) = fixture()
    val changed = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, alternative.head)),
      List(actual, alternative).map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "test-line"))),
      first.additions, pvGuided = true)
    val oldIds = first.additions.filter(_.request.origin.question.producer == "C-current-support-duty").map(_.request.id).toSet
    assert(oldIds.nonEmpty)
    assert(changed.additions.exists(entry => entry.request.origin.question.producer == "C-current-support-duty" &&
      !oldIds(entry.request.id)))

  test("a different reply in the other root PV does not hide the current same-reply question"):
    val (_, _, _, batch) = fixture(List("f3d2", "d7b6", "f1e1", "c8g4", "a2a4"),
      List("f1e1", "f6e4", "e2d3", "f7f5", "a2a4"))
    val generated = batch.additions.find(_.request.origin.question.producer == "C-current-support-duty").get
    assert(generated.request.requirements.exists {
      case value: CompareMoves => value == CompareMoves("duty_other_choice", List("f1e1", "d7b6"), "a2a4", "e2d3")
      case _ => false
    })
    assert(generated.request.requirements.exists {
      case value: Fact => value.id == "duty_other_response" && value.path.source == Declared(List("f1e1", "d7b6")) &&
        value.predicate == Predicate("legal_move", Some(Piece("white", "bishop", "e2")), square = Some("d3"))
      case _ => false
    })
    assert(batch.replayNeeds.contains(List("f1e1", "d7b6")))
