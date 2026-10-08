package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Legal book position; the receipt scores are synthetic test data. */
class ReserveHoldTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import PrincipalVariationComparison.QualityBand

  private val fen = "8/p7/8/1p1p1k1p/1P1P1P1P/5K2/P7/8 b - - 0 39"
  private val actual = "a7a6 f3e3 f5e6 f4f5 e6f5 e3f3 f5e6 f3f4 e6f6 a2a3 f6g6 f4e5 g6f7 e5d5".split(" ").toList
  private val prefix = "f5g6 f3f2 g6f5 f2e3 f5g4 f4f5 g4f5 e3f3 f5e6 f3f4 e6f6 a2a3".split(" ").toList
  private val waited = prefix ++ List("a7a6", "f4e3", "f6f5")
  private val retreat = prefix ++ List("f6g6", "f4e5", "g6f7", "e5d5")

  private def fixture() =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val lines = List(actual, waited, retreat)
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(moves => paths.certifiedPath(packet, moves).get)
    def answer(left: Int, right: Int, offset: Int) =
      CommentaryInvestigation.Answer("synthetic-hold-" + offset, "synthetic-hold-key-" + offset,
        CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), "better",
        found(left).line.ref.id, found(right).line.ref.id, Json.obj(), Json.obj(),
        found(left).ids(offset), found(right).ids(offset), Some(QualityBand.Separated))
    val batch = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, waited.head)),
      lines.take(2).map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "test-line"))) :+
        CommentaryDiscovery.SourcePath(retreat, Source("generator", "synthetic-king-class"), Some(prefix -> 0)), Nil,
      pvGuided = true)
    val generated = batch.additions.find(_.request.origin.question.producer == "C-reserve-hold").get
    val answered = generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered", answer = Some(answer(0, 1, 0)))
      case reading if reading.requirementId == "reserve_hold" => reading.copy(state = "answered", answer = Some(answer(2, 1, prefix.size)))
      case reading => reading
    })
    val entry = CommentaryFactReader.read(packet, paths, List(answered)).head
    (packet, paths, found, entry)

  test("a later retained pawn move holds the king's guard against the tested retreat and entry"):
    val (packet, paths, found, entry) = fixture()
    val proof = CommentaryIdea.reserveHolds(List(entry)).head
    assertEquals(proof.test.referenceMove, "f6g6")
    assert(proof.verified)
    val prior = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    val answer = RootSubjectComparison.withReserveHold(packet, paths, found.take(2), List(proof), prior)
    assertEquals(answer.question, "investigated_mechanism")
    val result = RootQuestionSummary.realize(paths, found.take(2), answer).get
    assert(result.text.contains("a6 spends a pawn move that Kg6 keeps in reserve"), result.text)
    assert(result.text.contains("keeps the king on f6 and e5 guarded"), result.text)
    assert(result.text.contains("allowing Ke5 to enter"), result.text)
    assert(result.text.contains("Kxd5"), result.text)
    assert(!result.text.contains("forced") && !result.text.contains("must"), result.text)

  test("a common quiet king reply is a relevant challenge too; repeated search provenance cannot change a question identity"):
    val (packet, paths, _, _) = fixture()
    def sources(producer: String) = List(actual, waited).map(moves => CommentaryDiscovery.SourcePath(moves,
      Source("generator", producer, sourceRefs = List("synthetic-" + producer))))
    val first = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, waited.head)),
      sources("root_search"), Nil, pvGuided = true)
    val response = first.additions.find(_.request.origin.question.producer == "C-reserve-response").get
    val test = response.request.requirements.collectFirst { case value: CompareMoves if value.id == "reserve_response" => value }.get
    assertEquals(test.prefixMovesUci, List("f5g6"))
    assertEquals(Set(test.referenceMove, test.alternativeMove), Set("f3e3", "f3f2"))
    val second = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, waited.head)),
      sources("focus_comparison"), first.additions, pvGuided = true)
    val before = first.additions.filter(_.request.origin.question.producer == "C-reserve-hold").map(_.request.id).toSet
    assert(second.additions.filter(_.request.origin.question.producer == "C-reserve-hold").forall(entry => before(entry.request.id)))
    val updated = second.additions.find(_.request.origin.question.producer == "C-reserve-hold").get
    assert(updated.generatedArms.forall(arm => Set("synthetic-root_search", "synthetic-focus_comparison")
      .subsetOf(arm.sourceRefs.toSet)))

  test("a king move that also holds the result does not prove the wait necessary"):
    val (packet, paths, found, entry) = fixture()
    val prior = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    for relation <- List("equal", "worse", "unknown") do
      val changed = entry.copy(readings = entry.readings.map { reading =>
        if reading.requirementId == "reserve_hold" then reading.copy(answer = reading.answer.map(_.copy(evaluationRelation = relation)))
        else reading
      })
      val proof = CommentaryIdea.reserveHolds(List(changed)).head
      assert(!proof.verified)
      assertEquals(RootSubjectComparison.withReserveHold(packet, paths, found.take(2), List(proof), prior), prior)

  test("the best legal king alternative must be included; a weaker sample cannot stand for the class"):
    val (_, _, _, entry) = fixture()
    val seed = CommentaryIdea.reserveHolds(List(entry)).head
    val betterKing = prefix ++ List("f6f7", "f4e5")
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = List(actual, waited, retreat, betterKing)
        .map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val at = history.extend(prefix).toOption.get
    val root = paths.certifiedPath(packet, waited).get
    def search(restriction: EngineRootRestriction, kingScore: Int) =
      val rows = List(waited.drop(prefix.size) -> 0, retreat.drop(prefix.size) -> 700,
        betterKing.drop(prefix.size) -> kingScore).filter((moves, _) => restriction match
          case EngineRootRestriction.Unrestricted => true
          case EngineRootRestriction.Restricted(pool) => pool.contains(moves.head))
      val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, "synthetic-king-class",
        EngineWorkPurpose.ResponseComparison, at.initialFen, at.movePrefixUci, at.currentFen,
        restriction, at.currentFen, Nil, EngineSearchLimits(16, 2_000_000, 2_500, rows.size), 3_500,
        Some(EngineSearchAnchor(prefix.head, root.ids.head, root.positions(prefix.size).id.get, prefix)))
      val reported = rows.map((moves, cp) => ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16))
      val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(at)).get
      val receipt = CommentaryInvestigation.EngineCandidates(work, EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)
      TargetResponseInvestigation.readSearch(packet, receipt, at, paths).get
    val full = search(EngineRootRestriction.Unrestricted, 0)
    val weaker = seed.copy(test = seed.test.copy(referenceMove = "f6g6"),
      testAnswer = full.pairAnswer(packet, "f6g6", "a7a6"))
    assert(weaker.verified)
    assert(!RootSubjectComparison.strongestReserveHold(packet, paths, weaker, List(full)))
    val restricted = search(EngineRootRestriction.Restricted(List("a7a6", "f6g6")), 0)
    assert(!RootSubjectComparison.strongestReserveHold(packet, paths,
      weaker.copy(testAnswer = restricted.pairAnswer(packet, "f6g6", "a7a6")), List(restricted)))
    val losingKings = search(EngineRootRestriction.Unrestricted, 600)
    val best = seed.copy(test = seed.test.copy(referenceMove = "f6f7"),
      testAnswer = losingKings.pairAnswer(packet, "f6f7", "a7a6"))
    assert(best.verified)
    assert(RootSubjectComparison.strongestReserveHold(packet, paths, best, List(losingKings)))
    assert(!RootSubjectComparison.strongestReserveHold(packet, paths,
      best.copy(testAnswer = best.testAnswer.map(_.copy(workId = "foreign"))), List(losingKings)))

  test("the entry, physical pawn and exact child receipt remain part of the selected meaning"):
    val (packet, paths, found, entry) = fixture()
    val prior = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    val proof = CommentaryIdea.reserveHolds(List(entry)).head
    val answer = RootSubjectComparison.withReserveHold(packet, paths, found.take(2), List(proof), prior)
    val value = answer.core.head.statements.head.asInstanceOf[CommentaryAnswer.ReserveHold]
    val changed = List(value.copy(square = "f5"),
      value.copy(proof = proof.copy(branch = proof.branch.updated(1, "f3e2").updated(3, "e2e3"))),
      value.copy(proof = proof.copy(pawn = Piece("black", "pawn", "h5"))),
      value.copy(proof = proof.copy(testAnswer = proof.testAnswer.map(_.copy(referenceOccurrenceId = found(0).ids.head)))),
      value.copy(waited = found(0)))
    changed.foreach(item => assert(!RootSubjectComparison.validReserveHold(paths, item)))
    val stale = entry.copy(readings = entry.readings.map(reading =>
      if reading.requirementId == "hold_current" then reading.copy(state = "not_applicable", result = None) else reading))
    assertEquals(CommentaryIdea.reserveHolds(List(stale)), Nil)

  test("the later use follows both answered choices; deleting or weakening its ancestor cannot borrow a root PV"):
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val rootKept = List("f5g6", "f3e3", "g6f5", "e3f3")
    val earlyWait = prefix.take(8) ++ List("a7a6", "a2a3", "f5g6")
    val lines = List(actual, rootKept, waited, retreat, earlyWait)
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(moves => paths.certifiedPath(packet, moves).get)
    def receipt(left: Int, right: Int, offset: Int, relation: String) =
      CommentaryInvestigation.Answer("synthetic-chain-" + offset, "synthetic-chain-key-" + offset,
        CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), relation,
        found(left).line.ref.id, found(right).line.ref.id, Json.obj(), Json.obj(),
        found(left).ids(offset), found(right).ids(offset), Some(QualityBand.Separated))
    def owner(pair: CompareMoves, answer: CommentaryInvestigation.Answer) =
      CommentaryIdea.AnsweredComparison("synthetic-chain-owner", pair, answer)
    val first = owner(CompareMoves("hold_ancestor_0", List("f5g6"), "f3e3", "f3f2"), receipt(1, 2, 1, "equal"))
    val second = owner(CompareMoves("hold_source", prefix.take(8), "a7a6", "f5e6"), receipt(4, 2, 8, "equal"))
    val proof = CommentaryIdea.ReserveHold(
      owner(CompareMoves("reserve_root", Nil, actual.head, waited.head), receipt(0, 1, 0, "better")),
      CompareMoves("reserve_hold", prefix, "f6g6", "a7a6"), Some(receipt(3, 2, 12, "better")),
      prefix :+ "a7a6", Piece("black", "pawn", "a7"), Piece("black", "king", "f5"), Nil,
      Some(second), "alternative", List(first -> "alternative"))
    val value = CommentaryAnswer.ReserveHold(found(0), found(1), found(2), found(3), "e5", proof)
    assert(RootSubjectComparison.validReserveHold(paths, value))
    val closeReply = first.copy(answer = first.answer.copy(evaluationRelation = "worse", qualityBand = Some(QualityBand.Close)))
    assert(RootSubjectComparison.validReserveHold(paths,
      value.copy(proof = proof.copy(ancestors = List(closeReply -> "alternative")))))
    assert(!RootSubjectComparison.validReserveHold(paths,
      value.copy(proof = proof.copy(ancestors = List(closeReply.copy(answer = closeReply.answer.copy(
        evaluationRelation = "unknown")) -> "alternative")))))
    val invalid = List(proof.copy(ancestors = Nil),
      proof.copy(ancestors = List(first -> "reference")),
      proof.copy(ancestors = List(first.copy(answer = first.answer.copy(evaluationRelation = "worse")) -> "alternative")),
      proof.copy(ancestors = List(first -> "alternative", first -> "alternative")),
      proof.copy(ancestors = List(first.copy(answer = first.answer.copy(alternativeOccurrenceId = found(0).ids.head)) -> "alternative")))
    invalid.foreach(changed => assert(!RootSubjectComparison.validReserveHold(paths, value.copy(proof = changed))))
