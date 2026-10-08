package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Actual legal positions; comparison receipts here are deliberately synthetic.
  * Separate recorded product runs test the hypothesis with the real engine. */
class ReserveEntryTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import PrincipalVariationComparison.QualityBand

  private val fen = "8/5p2/8/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35"
  private val actual = ("g3g4 f7f6 f3e3 b4b3 e3f3 e5d6 f3f4 d6e6 " +
    "e4e5 f6e5 f4g5 e5e4 g5f4 e6d5 g4g5 e4e3 f4e3 d5e5 g5g6 e5f6 g6g7 f6g7").split(" ").toList
  private val retained = "f3e3 e5e6 e3f4 e6f6 e4e5 f6e7 f4e4 e7e6 g3g4 b4b3 g4g5 e6d7 e4d5".split(" ").toList
  private val challenged = retained.take(11) ++ List("f7f5", "e5f6")

  private def lastWaitFixture(extraPawn: Boolean = false, finalKingMove: String = "f4f5", counterCapture: Boolean = false) =
    val start = if extraPawn then "8/5p2/7p/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35" else fen
    val actual = "g3g4 f7f6 f3e3 b4b3 e3f3 e5d6".split(" ").toList
    val prefix = "f3e3 f7f6 e3f3 e5d6 f3e2 d6e5 e2e3 f6f5 e4f5 e5f5 e3f3 f5e5 g3g4".split(" ").toList
    val waited = prefix ++ List("e5d6", "f3e4", "d6e7")
    val counter = prefix ++ (if counterCapture then "b4a3 b2a3 e5f6 f3f4 f6g6 g4g5 g6h7"
      else "b4b3 f3e3 e5f6 e3f4 f6g6 g4g5 g6h7").split(" ").toList :+ finalKingMove
    val history = CanonicalPositionHistory.from(start, Nil, start).toOption.get
    val lines = List(actual, waited, counter)
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(moves => paths.certifiedPath(packet, moves).get)
    def answer(left: Int, right: Int, offset: Int, relation: String) =
      CommentaryInvestigation.Answer("synthetic-last-wait-" + offset, "synthetic-last-wait-key-" + offset,
        CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), relation,
        found(left).line.ref.id, found(right).line.ref.id, Json.obj(), Json.obj(),
        found(left).ids(offset), found(right).ids(offset), Some(QualityBand.Separated))
    val generated = CommentaryDiscovery.discover(packet, paths,
      List(CommentaryDiscovery.Anchor(Nil, waited.head)),
      lines.take(2).map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "synthetic-last-wait"))),
      Nil, pvGuided = true).additions.find(entry => entry.request.origin.question.producer == "C-reserve-entry" &&
        entry.request.requirements.exists {
          case value: CompareMoves => value.id == "reserve_counter" && value.alternativeMove == counter(prefix.size)
          case _ => false
        }).get
    val entry = CommentaryFactReader.read(packet, paths, List(generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered", answer = Some(answer(0, 1, 0, "better")))
      case reading if reading.requirementId == "reserve_counter" => reading.copy(state = "answered", answer = Some(answer(1, 2, prefix.size, "equal")))
      case reading => reading
    }))).head
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    val selected = RootSubjectComparison.withReserveEntry(packet, paths, found.take(2),
      CommentaryIdea.reserveEntries(List(entry)), previous)
    (packet, paths, found, previous, selected)

  test("the opponent's last pawn wait can lead to a later forced release of another entry square"):
    val (_, paths, found, _, selected) = lastWaitFixture()
    assertEquals(selected.question, "investigated_mechanism")
    val value = selected.core.head.statements.head.asInstanceOf[CommentaryAnswer.ReserveEntry]
    assertEquals(value.reply, CommentaryAnswer.ReserveReply.LastPawnWait(18, "f5"))
    val text = RootQuestionSummary.realize(paths, found.take(2), selected).get.text
    assert(text.contains("b3 keeps that square guarded"), text)
    assert(text.contains("g5 holds the king's position"), text)
    assert(text.contains("no legal pawn move left"), text)
    assert(text.contains("Every legal king reply, including Kh7, gives up f5; the king can enter that square"), text)
    for forged <- List(CommentaryAnswer.ReserveReply.LastPawnWait(16, "f5"),
        CommentaryAnswer.ReserveReply.LastPawnWait(18, "e4")) do
      assert(!RootSubjectComparison.validReserveEntry(paths, value.copy(reply = forged)))

  test("one remaining legal pawn move refutes the claim that every reply must release the entry"):
    val (_, _, _, previous, selected) = lastWaitFixture(extraPawn = true)
    assertEquals(selected, previous)

  test("a pawn counter exchange keeps its recapture and both material costs before the later last wait"):
    val (_, paths, found, _, selected) = lastWaitFixture(counterCapture = true)
    assertEquals(selected.question, "investigated_mechanism")
    val value = selected.core.head.statements.head.asInstanceOf[CommentaryAnswer.ReserveEntry]
    assertEquals(value.reply, CommentaryAnswer.ReserveReply.LastPawnWait(18, "f5", true))
    val result = RootQuestionSummary.realize(paths, found.take(2), selected).get
    assert(result.text.contains("bxa3 recaptures that pawn"), result.text)
    assert(result.text.contains("Every legal king reply, including Kh7, gives up f5"), result.text)
    assert(List(13, 14).forall(offset => selected.core.flatMap(_.covered)
      .exists(ref => (ref.path eq found(2)) && ref.offset == offset)), "both sides of the counter trade must be covered")
    assert(!RootSubjectComparison.validReserveEntry(paths, value.copy(
      reply = CommentaryAnswer.ReserveReply.LastPawnWait(18, "f5", false))))
    val (_, _, _, previous, unsafe) = lastWaitFixture(extraPawn = true, counterCapture = true)
    assertEquals(unsafe, previous, "the capture cannot hide another available pawn move")

  test("an available entry is not invented as a played move when the tested PV chooses another square"):
    val (_, paths, found, _, selected) = lastWaitFixture(finalKingMove = "f4e4")
    val value = selected.core.head.statements.head.asInstanceOf[CommentaryAnswer.ReserveEntry]
    assertEquals(value.reply, CommentaryAnswer.ReserveReply.LastPawnWait(18, "f5"))
    assert(!value.references.exists(ref => (ref.path eq found(2)) && ref.offset == 20))
    val text = RootQuestionSummary.realize(paths, found.take(2), selected).get.text
    assert(text.contains("gives up f5; the king can enter that square"), text)
    assert(!text.contains("Kf5"), text)

  private def fixture() =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val lines = List(actual, retained, challenged)
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(moves => paths.certifiedPath(packet, moves).get)
    def answer(left: Int, right: Int, offset: Int, relation: String) =
      CommentaryInvestigation.Answer("synthetic-reserve-" + offset, "synthetic-key-" + offset,
        CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), relation,
        found(left).line.ref.id, found(right).line.ref.id, Json.obj(), Json.obj(),
        found(left).ids(offset), found(right).ids(offset), Some(QualityBand.Separated))
    val batch = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, retained.head)),
      lines.take(2).map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "test-line"))), Nil,
      pvGuided = true)
    val generated = batch.additions.find(_.request.origin.question.producer == "C-reserve-entry").get
    val answered = generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered", answer = Some(answer(0, 1, 0, "better")))
      case reading if reading.requirementId == "reserve_counter" => reading.copy(state = "answered", answer = Some(answer(1, 2, 11, "worse")))
      case reading => reading
    })
    val entry = CommentaryFactReader.read(packet, paths, List(answered)).head
    (packet, paths, found, batch, entry)

  test("the retained pawn can supply two waits while the king holds its entry position"):
    val (_, _, _, _, entry) = fixture()
    val proof = CommentaryIdea.reserveEntries(List(entry)).head
    assertEquals(proof.first, 8)
    assertEquals(proof.last, 10)
    assertEquals(proof.square, "d5")
    assertEquals(proof.pawn, Piece("white", "pawn", "g3"))
    assertEquals(proof.test.alternativeMove, "f7f5")
    assert(proof.verified)

  test("the pawn concession is independent of the ranking between the two tested defenses"):
    val (packet, paths, found, _, entry) = fixture()
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    for relation <- List("better", "equal", "worse") do
      val changed = entry.copy(readings = entry.readings.map { reading =>
        if reading.requirementId == "reserve_counter" then reading.copy(answer = reading.answer.map(_.copy(
          evaluationRelation = relation, qualityBand = None)))
        else reading
      })
      val answer = RootSubjectComparison.withReserveEntry(packet, paths, found.take(2),
        CommentaryIdea.reserveEntries(List(changed)), previous)
      val text = RootQuestionSummary.realize(paths, found.take(2), answer).get.text
      assert(text.contains("leaves a passed pawn on f6, supported by the pawn kept in reserve"), text)
      assert(!text.contains("evaluates"), text)
    val unknown = entry.copy(readings = entry.readings.map { reading =>
      if reading.requirementId == "reserve_counter" then reading.copy(answer = None) else reading
    })
    assert(!CommentaryIdea.reserveEntries(List(unknown)).head.verified)

  test("an unrelated later pawn move cannot keep a withdrawn current use alive"):
    val (_, _, _, _, entry) = fixture()
    val changed = entry.copy(readings = entry.readings.map { reading =>
      if reading.requirementId == "reserve_current_entry" then reading.copy(state = "not_applicable", result = None)
      else reading
    })
    assertEquals(CommentaryIdea.reserveEntries(List(changed)), Nil)

  test("the basic answer retains the waiting sequence, tested pawn reply and used pawn's later price"):
    val (packet, paths, found, _, entry) = fixture()
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    val selected = RootSubjectComparison.withReserveEntry(packet, paths, found.take(2),
      CommentaryIdea.reserveEntries(List(entry)), previous)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val answer = MoveWorkProjection.completeSelectedConditions(packet, paths, found.take(2), positions,
      MoveWorkProjection.select(packet, paths).local, selected).get
    assertEquals(answer.question, "investigated_mechanism")
    val result = RootQuestionSummary.realize(paths, found.take(2), answer).get
    assert(result.text.startsWith("g4 spends a pawn move that Ke3 keeps in reserve."), result.text)
    assert(result.text.contains("g4 b3 g5 keeps the king on e4"), result.text)
    assert(result.text.contains("Kd5 enters that square"), result.text)
    assert(result.text.contains("Against f5+") && result.text.contains("exf6 leaves a passed pawn on f6"), result.text)
    assert(result.text.contains("Kxg7"), result.text)
    assert(!result.text.contains("forced") && !result.text.contains("only winning") && !result.text.contains("must"), result.text)
    assert(answer.core.flatMap(_.covered).exists(ref => (ref.path eq found(2)) && ref.offset == 12),
      "the selected child reply's capture must be covered as a price, not only mentioned in prose")

  test("realization rejects an unrelated pawn, a different king entry and a counter receipt from another occurrence"):
    val (packet, paths, found, _, entry) = fixture()
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    val answer = RootSubjectComparison.withReserveEntry(packet, paths, found.take(2),
      CommentaryIdea.reserveEntries(List(entry)), previous)
    val original = answer.core.head.statements.head.asInstanceOf[CommentaryAnswer.ReserveEntry]
    val forged = List(original.copy(proof = original.proof.copy(pawn = Piece("white", "pawn", "e4"))),
      original.copy(proof = original.proof.copy(square = "f5")),
      original.copy(reply = CommentaryAnswer.ReserveReply.PassedPawn(Piece("white", "pawn", "f5").witness, true)),
      original.copy(reply = CommentaryAnswer.ReserveReply.PassedPawn(Piece("white", "pawn", "f6").witness, false)),
      original.copy(proof = original.proof.copy(testAnswer = original.proof.testAnswer.map(_.copy(
        alternativeOccurrenceId = found(0).ids.head)))))
    forged.foreach(value => assert(!RootSubjectComparison.validReserveEntry(paths, value)))

  test("a separately tested first reply owns its continuation without borrowing the root PV's different suffix"):
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val rootKept = "f3e3 e5e6 e3f4 e6f6 e4e5 f6g6".split(" ").toList
    val otherReply = "f3e3 f7f6 e3f3 b4b3".split(" ").toList
    val lines = List(actual, rootKept, retained, challenged, otherReply)
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(moves => paths.certifiedPath(packet, moves).get)
    def answer(left: Int, right: Int, offset: Int, relation: String) =
      CommentaryInvestigation.Answer("synthetic-reply-" + offset, "synthetic-key-" + offset,
        CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), relation,
        found(left).line.ref.id, found(right).line.ref.id, Json.obj(), Json.obj(),
        found(left).ids(offset), found(right).ids(offset), Some(QualityBand.Separated))
    val rootSources = lines.take(2).map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "test-root")))
    val initial = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, retained.head)),
      rootSources, Nil, pvGuided = true)
    val response = initial.additions.find(_.request.origin.question.producer == "C-reserve-response").get
    val readResponse = CommentaryFactReader.read(packet, paths, List(response.copy(readings = response.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered", answer = Some(answer(0, 1, 0, "better")))
      case reading if reading.requirementId == "reserve_response" => reading.copy(state = "answered", answer = Some(answer(2, 4, 1, "worse")))
      case reading => reading
    }))).head
    assertEquals(CommentaryIdea.reserveResponses(List(readResponse)).size, 1)
    val source = CommentaryDiscovery.SourcePath(retained, Source("generator", "response_comparison",
      sourceRefs = List("synthetic-reply-1")))
    val next = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, retained.head)),
      rootSources :+ source, List(readResponse), pvGuided = true)
    val generated = next.additions.find(entry => entry.request.origin.question.producer == "C-reserve-entry" &&
      entry.request.requirements.exists(_.id == "reserve_source")).get
    val owned = CommentaryFactReader.read(packet, paths, List(generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered", answer = Some(answer(0, 1, 0, "better")))
      case reading if reading.requirementId == "reserve_source" => reading.copy(state = "answered", answer = Some(answer(2, 4, 1, "worse")))
      case reading if reading.requirementId == "reserve_counter" => reading.copy(state = "answered", answer = Some(answer(2, 3, 11, "equal")))
      case reading => reading
    }))).head
    val proof = CommentaryIdea.reserveEntries(List(owned)).head
    assert(proof.source.nonEmpty)
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    val selected = RootSubjectComparison.withReserveEntry(packet, paths, found.take(2), List(proof), previous)
    assertEquals(selected.question, "investigated_mechanism")
    val value = selected.core.head.statements.head.asInstanceOf[CommentaryAnswer.ReserveEntry]
    assert(!RootSubjectComparison.validReserveEntry(paths, value.copy(proof = proof.copy(source = None))),
      "the tested continuation cannot be attributed to the unrelated current root suffix")
    for relation <- List("better", "unknown") do
      val changed = proof.copy(source = proof.source.map(source => source.copy(answer = source.answer.copy(evaluationRelation = relation))))
      assert(!RootSubjectComparison.validReserveEntry(paths, value.copy(proof = changed)))
    val stale = owned.copy(readings = owned.readings.map(reading =>
      if reading.requirementId == "reserve_source" then reading.copy(state = "deferred", answer = None) else reading))
    assertEquals(CommentaryIdea.reserveEntries(List(stale)), Nil)

  test("the selected counter gets its bounded allowance before optional work, retaining position and owners"):
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val profile = CommentaryEngineProfile.ServerAdmitted
    val start = CommentaryJobReducer.startJob(fen, Nil, fen, actual.head,
      history.extend(List(actual.head)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 30000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L)
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    def submit(state: AwaitingEngineWork, now: Long) = CommentaryJobReducer.submitIssuedWorkReport(state,
      EngineWorkReport.Completed(profile, state.issuedWork.workId, state.issuedWork.executionKeySha256,
        List(retained -> 240, actual -> 0, List("f3e2", "b4a3", "b2a3") -> -500).map { (moves, score) =>
          ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(score), state.issuedWork.requiredDepth)
        }), now).toOption.get.asInstanceOf[AwaitingEngineWork]
    val firstReply = submit(submit(start, 1L), 2L)
    assertEquals(firstReply.issuedWork.searchAnchor.map(_.prefixMoves), Some(retained.take(1)))
    val selected = CommentaryJobReducer.submitIssuedWorkReport(firstReply,
      EngineWorkReport.Completed(profile, firstReply.issuedWork.workId, firstReply.issuedWork.executionKeySha256,
        List(retained.drop(1) -> 240, List("f7f6", "e3f3", "e5d6") -> 500).map { (moves, score) =>
          ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(score), firstReply.issuedWork.requiredDepth)
        }), 3L).toOption.get.asInstanceOf[AwaitingEngineWork]
    assertEquals(selected.issuedWork.searchAnchor.map(_.prefixMoves), Some(retained.take(11)), clues(selected.issuedWork))
    assertEquals(selected.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("e6d7", "f7f5", "f7f6")))
    assertEquals(selected.issuedWork.searchLimits, EngineSearchLimits(16, 20000000, 5000, 3))
    assertEquals(selected.issuedWork.maxSearchElapsedMs, 6000)
    assert(selected.proposalEntries.exists(_.readings.exists(reading => reading.requirementId == "reserve_counter" &&
      reading.state == "awaiting_engine" && reading.workId.contains(selected.issuedWork.workId))))
    def fail(state: AwaitingEngineWork, now: Long) = CommentaryJobReducer.submitIssuedWorkReport(state,
      EngineWorkReport.ExecutorFailed(profile, state.issuedWork.workId, state.issuedWork.executionKeySha256,
        EngineWorkFailure.BudgetExhausted), now).toOption.get
    val stopped = fail(selected, 5002L)
    val counters = stopped.proposalEntries.flatMap(entry => entry.request.requirements.collect {
      case pair: CompareMoves if pair.id == "reserve_counter" && pair.prefixMovesUci == retained.take(11) =>
        entry -> pair
    })
    assertEquals(counters.map(_._2.alternativeMove).toSet, Set("f7f5", "f7f6"))
    assertEquals(counters.size, 4)
    for (entry, pair) <- counters do
      val reading = entry.readings.find(_.requirementId == pair.id).get
      assertEquals(reading.state, "stopped")
      assertEquals(reading.stopReason, Some("search_budget_exhausted"))
      assertEquals(reading.answer, None)
      assert(selected.issuedWork.proposalRefs.contains(entry.request.id -> pair.id))
      assertEquals(reading.workId, Some(selected.issuedWork.workId))
    stopped match
      case awaiting: AwaitingEngineWork =>
        val work = awaiting.issuedWork
        assert(!CommentaryJobReducer.sameProposalSearch(selected.issuedWork, work), "the failed full pool cannot be reissued")
        assertEquals(work.searchAnchor.map(_.prefixMoves), Some(retained.take(8)))
        assertEquals(work.enginePositionInitialFen, fen)
        assertEquals(work.enginePositionMovesUci, retained.take(8))
        assertEquals(work.searchFen, history.extend(retained.take(8)).toOption.get.currentFen)
        assertEquals(work.rootRestriction, EngineRootRestriction.Restricted(List("g3g4", "e4e3", "e4f3", "e4f4")))
        val holds = stopped.proposalEntries.flatMap(entry => entry.request.requirements.collect {
          case pair: CompareMoves if pair.id == "reserve_hold" && pair.prefixMovesUci == retained.take(8) &&
              Set(pair.referenceMove, pair.alternativeMove) == Set("e4e3", "g3g4") => entry -> pair
        })
        assert(holds.nonEmpty)
        assertEquals(work.proposalRefs.toSet, holds.map((entry, pair) => entry.request.id -> pair.id).toSet)
        for (entry, pair) <- holds do
          val reading = entry.readings.find(_.requirementId == pair.id).get
          assertEquals(reading.state, "awaiting_engine")
          assertEquals(reading.workId, Some(work.workId))
      case _ => ()
    assertEquals(stopped.rootSearches.flatMap(_.receipt).map(_.lines.map(_.evaluation)),
      selected.rootSearches.flatMap(_.receipt).map(_.lines.map(_.evaluation)),
      "an unfinished child comparison cannot alter the numeric root assessment")
    assert(CommentaryIdea.reserveEntries(stopped.proposalEntries).forall(_.testAnswer.isEmpty),
      "an exhausted counter is not a verified functional entry")
    assert(!fail(selected, 25001L).isInstanceOf[AwaitingEngineWork], "a failed comparison cannot replenish the job deadline")
