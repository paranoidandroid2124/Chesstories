package io.chesstory.runtime

import lila.chessjudgment.model.judgment.EvaluationSource
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Supplied opening paths and invented scores exercise the reducer; no engine is run. */
class CommentaryFactJobTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val initial = chess.variant.Standard.initialFen.value
  private val owner = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val origin = Origin(Source("user", "fact-job-question"), Source("user", "fact-job-path"),
    Source("none", "fact-job-priority"), Source("none", "fact-job-evidence"))
  private val prefix = List("e2e4", "e7e5")
  private val rootRows = List(prefix -> 30, List("d2d4", "d7d5") -> 20, List("g1f3", "d7d5") -> 10)
  private val future = Path("future", Declared(prefix), extend = true)
  private val knight = Piece("white", "knight", "g1")

  private def request(requirements: Requirement*): Request = Request("facts", origin, requirements.toList)
  private def development(id: String = "developed", at: Int = 5): Fact =
    Fact(id, future, Predicate("location", Some(knight)), Ply(at), expected = Some("f3"))

  private def start(requests: List[Request] = Nil, limit: Int = 600, discover: Boolean = false): AwaitingEngineWork =
    CommentaryJobReducer.startJob(initial, Nil, initial, "d2d4",
      owner.extend(List("d2d4")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = discover,
        maximumLocalExpansions = limit), 0L, proposals = requests).toOption.get
      .asInstanceOf[AwaitingEngineWork]

  private def submit(state: AwaitingEngineWork, rows: List[(List[String], Int)]): CommentaryJobState =
    val work = state.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, rows.map { (moves, cp) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16)
      }), 1L).toOption.get

  private def completed(state: CommentaryJobState): CompletedCommentaryJob = state match
    case value: CompletedCommentaryJob => value
    case other => fail(s"expected a completed job, got ${other.getClass.getSimpleName}")

  private def awaiting(state: CommentaryJobState): AwaitingEngineWork = state match
    case value: AwaitingEngineWork => value
    case other => fail(s"expected an outstanding fact continuation, got ${other.getClass.getSimpleName}")

  private def reading(state: CommentaryJobState, id: String): Reading =
    state.proposalEntries.find(_.request.id == "facts").get.readings.find(_.requirementId == id).get

  private def result(state: CommentaryJobState, id: String): Result = reading(state, id).result.get

  private def dump(state: CommentaryJobState, name: String): Unit =
    sys.props.get("chesstory.factWireDump").foreach { target =>
      val wire = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("3" * 32, "fact-delivery", 10000L, state))
      java.nio.file.Files.writeString(java.nio.file.Path.of(target).resolveSibling(name), Json.prettyPrint(wire))
    }

  private def review(state: CompletedCommentaryJob): CompletedMoveReview = state.result match
    case CompletedPositionCommentary.FocusedMoveReview(_, value) => value
    case _ => fail("expected the retained root review")

  private def rootDecision(state: CompletedCommentaryJob): JsObject =
    val record = review(state)
    val selected = (RuntimeProtocol.completedResultJson(state) \ "selected_move_reviews").as[List[JsObject]]
    val played = selected.find(value => (value \ "move_uci").as[String] == record.moveUci).get
    Json.obj("move_uci" -> record.moveUci, "best_move_uci" -> record.bestMoveUci,
      "legal_move_index" -> record.legalMoveIndex, "best_legal_move_index" -> record.bestLegalMoveIndex,
      "played_root_rank" -> record.playedRootRank, "best_root_rank" -> record.bestRootRank,
      "primary" -> (played \ "commentary" \ "primary").as[JsObject])

  test("a declared path is canonically replayed and its fact reaches the job snapshot without another search"):
    val moves = List("g2g3", "g7g6", "f1g2")
    val supplied = request(Fact("bishop", Path("plan", Declared(moves)),
      Predicate("location", Some(Piece("white", "bishop", "f1"))), Ply(3), expected = Some("g2")))
    val baseline = completed(submit(start(), rootRows))
    val done = completed(submit(start(List(supplied), limit = 3), rootRows))
    val fact = result(done, "bishop")
    assertEquals(reading(done, "bishop").state, "supported")
    assertEquals(fact.state, "supported")
    assertEquals(fact.samples.flatMap(_.value).distinct, List("g2"))
    assert(fact.samples.filter(_.value.nonEmpty).forall(_.throughPly == 3))
    val packet = done.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    fact.samples.filter(_.value.nonEmpty).foreach { sample =>
      val observed = prepared.path(packet, sample.lineId).get
      assertEquals(observed.moves.take(3), moves)
      assertEquals(sample.moves, moves, "the compact sample binds the exact admitted prefix")
      assertEquals(sample.throughPly, 3)
    }
    assertEquals(done.localExpansionsUsed, 3)
    assertEquals(done.discoveryState, CommentaryDiscovery.State())
    assertEquals(done.progress.physicalWorksIssued, 1)
    assertEquals(rootDecision(done), rootDecision(baseline))
    assertEquals(done.proposalEntries.head.request, supplied)
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("3" * 32, "fact-delivery", 10000L, done))
    val proposals = (wire \ "observations" \ "selection" \ "proposals").as[List[JsObject]]
    val row = (proposals.head \ "requirements").as[List[JsObject]].head
    assertEquals((row \ "state").as[String], "supported")
    assertEquals((row \ "result" \ "state").as[String], "supported")
    assertEquals((row \ "result" \ "samples").as[List[JsObject]]
      .flatMap(value => (value \ "value").asOpt[String]).distinct, List("g2"))
    sys.props.get("chesstory.factWireDump").foreach { target =>
      java.nio.file.Files.writeString(java.nio.file.Path.of(target), Json.prettyPrint(wire))
    }

  test("automatic discovery shares replay capacity and registers only a returned child search"):
    val supplied = request(ObserveMove("local", Nil, "g2g3"),
      CompareMoves("child", List("e2e4"), "e7e5", "c7c5"))
    val pending = awaiting(submit(start(List(supplied), limit = 1, discover = true), rootRows))
    assertEquals(pending.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(pending.issuedWork.searchAnchor.get.prefixMoves, List("e2e4"))
    assertEquals(reading(pending, "local").state, "observed")
    assertEquals(pending.localExpansionsUsed, 1)
    assertEquals(pending.proposalEntries.head.request, supplied)
    assertEquals(pending.proposalEntries.head.admission, "submitted")
    assert(pending.proposalEntries.tail.forall(_.admission == "generated"))
    val scan = pending.discoveryState.scans.head
    assertEquals(scan.anchor.prefixMoves, Nil)
    assertEquals(scan.legalMoves.toSet, owner.currentLegalMoveUcis.toSet)
    assert(scan.examinedMoves.contains("g2g3"))
    assertEquals(scan.state, "partial")
    assertEquals(scan.reason, Some("local_expansion_limit"))
    assertEquals(pending.discoveryState.scans.size, 1)

    val cancelled = completed(CommentaryJobReducer.cancelJob(pending))
    assertEquals(cancelled.discoveryState, pending.discoveryState)
    assertEquals(cancelled.localExpansionsUsed, 1)
    assertEquals(reading(cancelled, "local").state, "observed")
    assertEquals(reading(cancelled, "child").stopReason, Some("cancelled"))
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("3" * 32, "discovery-delivery", 10000L, cancelled))
    val discovery = wire \ "observations" \ "selection" \ "discovery"
    assertEquals((discovery \ "input_key").as[String], pending.discoveryState.inputKey)
    assertEquals((discovery \ "scans").as[List[JsObject]].map(value => (value \ "state").as[String]), List("partial"))

    val after = submit(pending, List(List("e7e5") -> 0, List("c7c5") -> -10))
    assertNotEquals(after.discoveryState.inputKey, pending.discoveryState.inputKey)
    assertEquals(after.discoveryState.scans.map(_.anchor.prefixMoves).toSet, Set(Nil, List("e2e4")))
    assertEquals(after.localExpansionsUsed, 1)
    val retained = after match
      case done: CompletedCommentaryJob => done
      case active: AwaitingEngineWork => active.assessment.get
      case other => fail(s"expected retained discovery observations, got ${other.getClass.getSimpleName}")
    assertEquals(rootDecision(retained), rootDecision(pending.assessment.get))

  test("one owned continuation resolves a shared comparison context without closing another branch"):
    val shared = request(development(),
      Fact("king", future, Predicate("presence", Some(Piece("white", "king", "e1"))), Ply(5), expected = Some("present")),
      FactGroup("ready", "all", List("developed", "king")),
      CompareMoves("sicilian", List("e2e4"), "e7e5", "c7c5"),
      CompareMoves("french", List("e2e4"), "e7e5", "e7e6"))
    val other = Request("other", origin, List(Fact("other_development",
      Path("other", Declared(List("g1f3", "d7d5")), extend = true),
      Predicate("location", Some(knight)), Ply(5), expected = Some("f3"))))
    val arms = List("sicilian", "french").map(id => GeneratedArm(s"arm_$id", Nil, "e2e4", None,
      List("ready", id), shared.requirements.map(_.id), origin.question, Nil))
    val first = awaiting(submit(start(List(shared, other), limit = 0), rootRows))
    assertEquals(first.issuedWork.searchAnchor.map(_.prefixMoves), Some(prefix),
      "a needed result on the current PV precedes an unrelated third-candidate continuation")
    assertEquals(first.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val waiting = first.proposalEntries.find(_.request.id == "facts").get.copy(generatedArms = arms)
    assert(!neededRequirementIds(waiting)("sicilian") && !neededRequirementIds(waiting)("french"))
    val next = submit(first, List(
      List("g1f3", "b8c6", "f1c4") -> 30,
      List("f1c4", "b8c6", "g1f3") -> 20,
      List("d2d3", "b8c6", "g1f3") -> 10))
    val answered = next.proposalEntries.find(_.request.id == "facts").get.copy(generatedArms = arms)
    List("developed", "king", "ready").foreach(id => assertEquals(reading(next, id).state, "supported"))
    assert(neededRequirementIds(answered)("sicilian") && neededRequirementIds(answered)("french"),
      "both independent comparisons now have their own supported shared prerequisite")
    assert(!neededRequirementIds(answered)("developed"))
    val untouched = next.proposalEntries.find(_.request.id == "other").get.readings.head
    assert(!isResolved(untouched), "a same-piece observation on e4 cannot answer the Nf3 branch")
    assert(!awaiting(next).issuedWork.searchAnchor.exists(_.prefixMoves == prefix))
    assertEquals(next.progress.physicalReportsAccepted, 2)
    assertEquals(next.progress.physicalWorksIssued, 3, "one root and at most the two existing general slots")

  test("explicit extension facts share an owned response search even when automatic investigations are disabled"):
    val supplied = request(development(),
      Fact("unasserted", future, Predicate("location", Some(knight)), Ply(5)),
      Fact("king", future, Predicate("presence", Some(Piece("black", "king", "e8"))), Ply(5),
        expected = Some("present")),
      FactGroup("both", "all", List("developed", "king")))
    val active = awaiting(submit(start(List(supplied), limit = 0), rootRows))
    dump(active, "fact-wire-pending.json")
    val baseline = rootDecision(active.assessment.get)
    val work = active.issuedWork
    assertEquals(work.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(work.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(work.multiPv, 3)
    assertEquals(work.enginePositionInitialFen, initial)
    assertEquals(work.enginePositionMovesUci, prefix)
    assertEquals(work.searchFen, owner.extend(prefix).toOption.get.currentFen)
    assertEquals(work.searchAnchor.get.prefixMoves, prefix)
    assertEquals(work.searchAnchor.get.plyOffset, 2)
    List("developed", "king").foreach { id =>
      assertEquals(reading(active, id).state, "awaiting_engine")
      assertEquals(reading(active, id).workId, Some(work.workId))
      assertEquals(result(active, id).state, "partial")
      assert(result(active, id).needs.exists(need => need.kind == "continuation" &&
        need.prefixMoves == prefix && need.horizon.contains(5)))
    }
    val done = completed(submit(active, List(
      List("g1f3", "b8c6", "f1c4") -> 900,
      List("f1c4", "b8c6", "g1f3") -> -800,
      List("b1c3", "b8c6", "g1f3") -> 700)))
    assertEquals(result(done, "developed").state, "supported")
    assertEquals(result(done, "unasserted").state, "observed")
    assertEquals(result(done, "unasserted").needs, Nil)
    assertEquals(result(done, "developed").samples.flatMap(_.value).distinct, List("f3"))
    assertEquals(result(done, "king").state, "supported")
    assertEquals(result(done, "both").state, "supported")
    result(done, "developed").samples.filter(_.value.nonEmpty).foreach { sample =>
      assert(sample.moves.startsWith(prefix))
      assertEquals(sample.throughPly, 5)
    }
    assertEquals(done.progress.physicalWorksIssued, 2)
    assertEquals(done.progress.physicalReportsAccepted, 2)
    assertEquals(done.localExpansionsUsed, 0)
    assertEquals(done.responseSearches.size, 1)
    assertEquals(done.responseSearches.head.work.workId, work.workId)
    assert(done.observationsPacket.get.assembly.input.evaluationObservations
      .forall(_.source != EvaluationSource.ResponseComparison))
    assertEquals(rootDecision(done), baseline)
    assertEquals(done.assessmentNeedsRefresh, false)
    dump(done, "fact-wire-group.json")

  test("cancel and deadline preserve the partial result beside an already supported fact"):
    val supplied = request(
      Fact("known", Path("known", Declared(prefix)),
        Predicate("presence", Some(Piece("white", "pawn", "e2"))), Ply(2), expected = Some("present")),
      development())
    val active = awaiting(submit(start(List(supplied), limit = 0), rootRows))
    val known = reading(active, "known")
    val partial = result(active, "developed")
    val baseline = rootDecision(active.assessment.get)
    assertEquals(known.state, "supported")
    assertEquals(partial.state, "partial")
    assert(partial.needs.nonEmpty)
    val stopped = List(
      CommentaryJobReducer.cancelJob(active) -> "cancelled",
      CommentaryJobReducer.expireJob(active, 10000L) -> "deadline_exceeded")
    stopped.foreach { (state, reason) =>
      val done = completed(state)
      assertEquals(reading(done, "known"), known)
      assertEquals(reading(done, "developed").state, "stopped")
      assertEquals(reading(done, "developed").stopReason, Some(reason))
      assertEquals(result(done, "developed"), partial)
      assertEquals(done.proposalEntries.head.request, supplied)
      assertEquals(done.progress.physicalReportsAccepted, 1)
      assertEquals(rootDecision(done), baseline)
    }

  test("a closed declared path leaves a later point partial instead of requesting an extension"):
    val supplied = request(Fact("later", Path("closed", Declared(prefix)),
      Predicate("location", Some(knight)), Ply(5), expected = Some("f3")))
    val done = completed(submit(start(List(supplied), limit = 0), rootRows))
    assertEquals(result(done, "later").state, "partial")
    assert(result(done, "later").samples.forall(_.value.isEmpty))
    assertEquals(reading(done, "later").answer, None)
    assertEquals(done.progress.physicalWorksIssued, 1)
    assertEquals(done.progress.physicalReportsAccepted, 1)
    assertEquals(done.localExpansionsUsed, 0)
    assertEquals(done.responseSearches, Nil)

  test("a fact uses the exact two paths returned to its preceding comparison requirement"):
    val pawn = Predicate("location", Some(Piece("white", "pawn", "e2")))
    val supplied = request(CompareMoves("pair", Nil, "e2e4", "d2d4"),
      Fact("reference", Path("reference", Comparison("pair", "reference")), pawn, Ply(2)),
      Fact("alternative", Path("alternative", Comparison("pair", "alternative")), pawn, Ply(2)),
      FactComparison("different", "reference", "alternative", "different"))
    val done = completed(submit(start(List(supplied), limit = 0), rootRows))
    val answer = reading(done, "pair").answer.get
    assertEquals(result(done, "reference").samples.map(_.lineId).distinct, List(answer.referenceLineId))
    assertEquals(result(done, "alternative").samples.map(_.lineId).distinct, List(answer.alternativeLineId))
    assertEquals(result(done, "reference").samples.flatMap(_.value), List("e4"))
    assertEquals(result(done, "alternative").samples.flatMap(_.value), List("e2"))
    assertEquals(result(done, "different").state, "supported")
    assertEquals(result(done, "different").samples, Nil)
    assertEquals(done.progress.physicalWorksIssued, 1)

  test("incomplete extension reports consume only the two shared extra searches and retain their frontier"):
    val supplied = request(Fact("later", future, Predicate("presence", Some(knight)), Ply(7),
      expected = Some("present")))
    val first = awaiting(submit(start(List(supplied), limit = 0), rootRows))
    assertEquals(first.issuedWork.enginePositionMovesUci, prefix)
    val second = awaiting(submit(first, List(List("g1f3") -> 30, List("f1c4") -> 20, List("b1c3") -> 10)))
    val secondPrefix = second.issuedWork.enginePositionMovesUci
    assert(secondPrefix.startsWith(prefix))
    assertEquals(secondPrefix.size, 3)
    assert(Set("g1f3", "f1c4", "b1c3")(secondPrefix.last))
    assertEquals(second.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(second.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val done = completed(submit(second, List(List("b8c6") -> 300, List("g8f6") -> -200, List("d7d6") -> 100)))
    assertEquals(done.progress.physicalWorksIssued, 3)
    assertEquals(done.progress.physicalReportsAccepted, 3)
    assertEquals(done.responseSearches.size, 2)
    assertEquals(done.localExpansionsUsed, 0)
    assertEquals(result(done, "later").state, "partial")
    assert(result(done, "later").needs.exists(_.horizon.contains(7)))
    assertEquals(reading(done, "later").stopReason, Some("investigation_limit"))
    assertEquals(reading(done, "later").answer, None)
    assertEquals(done.proposalEntries.head.request, supplied)
    assertEquals(review(done).bestMoveUci, "e2e4")

  test("an already violated interval does not spend a search extending that exact history"):
    val captured = List("e2e4", "d7d5", "e4d5", "d8d5")
    val supplied = request(Fact("survives", Path("survival", Declared(List("e2e4")), extend = true),
      Predicate("presence", Some(Piece("white", "pawn", "e2"))), Ply(1), Some(Ply(6)), Some("present")))
    val done = completed(submit(start(List(supplied), limit = 0), rootRows.updated(0, captured -> 30)))
    val fact = result(done, "survives")
    assert(fact.samples.exists(sample => sample.throughPly == 4 && sample.value.contains("removed")))
    assertEquals(fact.state, "partial", "a failed observed arm does not refute every possible extension of e4")
    assert(!fact.needs.exists(_.kind == "continuation"))
    assert(fact.needs.exists(_.reason == "observed_extensions_not_exhaustive"))
    assertEquals(done.progress.physicalWorksIssued, 1)
    assertEquals(done.responseSearches, Nil)

  test("missing outcome data precedes an independent optional pair and is not investigated again after its answer"):
    val supplied = request(development(), CompareMoves("other-reply", List("d2d4"), "d7d5", "g8f6"))
    val first = awaiting(submit(start(List(supplied), limit = 0), rootRows))
    assertEquals(first.issuedWork.searchAnchor.get.prefixMoves, prefix)
    assertEquals(first.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val second = awaiting(submit(first, List(List("g1f3", "b8c6", "f1c4") -> 30,
      List("f1c4", "g8f6", "g1f3") -> 20, List("b1c3", "g8f6", "g1f3") -> 10)))
    assertEquals(reading(second, "developed").state, "supported")
    assertEquals(second.issuedWork.searchAnchor.get.prefixMoves, List("d2d4"))
    assertEquals(second.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("d7d5", "g8f6")))
    val done = completed(submit(second, List(List("d7d5") -> 20, List("g8f6") -> 30)))
    assertEquals(done.progress.physicalWorksIssued, 3)
    assertEquals(done.progress.physicalReportsAccepted, 3)
    assertEquals(done.responseSearches.count(_.anchor.prefixMoves == prefix), 1)
    assertEquals(result(done, "developed").needs, Nil)
    assertEquals(reading(done, "other-reply").state, "answered")

  test("complete path data does not turn an uninterpreted claim into an executable engine question"):
    val supplied = request(Uninterpreted("plan", "Is this a lasting plan for coordinating all the pieces?"),
      Fact("known", Path("closed", Declared(prefix)), Predicate("location", Some(knight)), Ply(2)))
    val done = completed(submit(start(List(supplied), limit = 0), rootRows))
    assertEquals(reading(done, "plan").state, "unsupported_reading")
    assertEquals(reading(done, "plan").stopReason, Some("uninterpreted_hypothesis"))
    assertEquals(result(done, "known").state, "observed")
    assertEquals(result(done, "known").samples.flatMap(_.value), List("g1"))
    assertEquals(done.progress.physicalWorksIssued, 1)
    assertEquals(done.responseSearches, Nil)

  test("a failed interval arm preserves its evidence without blocking a supported sibling"):
    val captured = List("e2e4", "d7d5", "e4d5", "d8d5")
    val supplied = request(CompareMoves("reply", List("e2e4"), "d7d5", "e7e5"),
      Fact("survives", Path("survival", Declared(List("e2e4")), extend = true),
        Predicate("presence", Some(Piece("white", "pawn", "e2"))), Ply(1), Some(Ply(6)), Some("present")))
    val pending = awaiting(submit(start(List(supplied), limit = 0), rootRows.updated(0, captured -> 30)))
    assertEquals(pending.issuedWork.searchAnchor.get.prefixMoves, List("e2e4"))
    assertEquals(pending.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("d7d5", "e7e5")))
    val done = completed(submit(pending, List(captured.tail -> 30,
      List("e7e5", "g1f3", "b8c6", "f1c4", "g8f6") -> 20)))
    val fact = result(done, "survives")
    assertEquals(fact.state, "supported")
    assert(fact.samples.exists(_.value.contains("removed")), "the failed arm remains available for inspection")
    assert(fact.proofs.nonEmpty)
    assertEquals(done.progress.physicalWorksIssued, 2)
    assertEquals(done.progress.physicalReportsAccepted, 2)

  test("event and direct-use evidence survive real result serialization beside a refuted support claim"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val moves = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3", "e4f3")
    val declared = Path("line", Declared(moves))
    val bishop = Piece("white", "bishop", "f1")
    val target = Piece("white", "knight", "f3")
    val supplied = request(
      Fact("not-yet-used", declared, Predicate("direct_support_use", Some(bishop), Some(target)), Ply(6), expected = Some("false")),
      Fact("used", declared, Predicate("direct_support_use", Some(bishop), Some(target)), Ply(7), expected = Some("true")),
      Fact("retained", declared, Predicate("support", Some(bishop), Some(target)),
        Event("capture", 1, "after", 5, 7, target = Some(target)), expected = Some("present")),
      FactGroup("both", "all", List("used", "retained")))
    val active = CommentaryJobReducer.startJob(fen, Nil, fen, moves.head,
      history.extend(List(moves.head)).toOption.get.currentFen, CommentaryJobPolicy(16, 10000L, profile,
        investigateChoices = false), 0L, proposals = List(supplied)).toOption.get.asInstanceOf[AwaitingEngineWork]
    val done = completed(submit(active, List(moves -> 30, List("h1g1") -> 20, List("d2d3") -> 10)))
    assertEquals(result(done, "used").state, "supported")
    assertEquals(result(done, "not-yet-used").state, "supported")
    assertEquals(result(done, "retained").state, "refuted")
    assertEquals(result(done, "retained").samples.flatMap(_.value), List("target_removed"))
    assertEquals(result(done, "both").state, "refuted")
    dump(done, "fact-wire-events.json")
