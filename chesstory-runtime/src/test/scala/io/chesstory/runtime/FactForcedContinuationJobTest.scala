package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Exposed legal exchange and synthetic reports test exact blocked owners,
  * not engine strength, a new numerical explanation, or a changed depth policy.
  */
class FactForcedContinuationJobTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val initial = chess.variant.Standard.initialFen.value
  private val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(List(
    "e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")).toOption.get
  private val checked = List("f3e5", "d8d4", "e5f3", "d4e4", "d1e2", "e4e2")
  private val recovered = checked :+ "e1e2"
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val rows = List(checked -> 0, List("e1g1") -> -10, List("d2d4") -> -20)

  private def request(id: String, facts: List[Requirement]) = Request(id,
    Origin(Source("user", id), Source("user", id + "-path"),
      Source("none", "forced-fact-priority"), Source("none", "forced-fact-evidence")), facts)

  private val supplied = request("forced-facts", List(
    Fact("checked", Path("exchange", Declared(checked)), Predicate("check", side = Some("white")),
      Ply(6), expected = Some("present")),
    Fact("recovered", Path("exchange", Declared(checked), extend = true),
      Predicate("location", Some(Piece("white", "king", "e1"))), Ply(7), expected = Some("e2"))))

  private def run(limit: Int, proposals: List[Request]): CommentaryJobState =
    val started = CommentaryJobReducer.startJob(history.initialFen, history.movePrefixUci, history.currentFen,
      checked.head, history.extend(List(checked.head)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = false, maximumLocalExpansions = limit),
      0L, proposals = proposals).toOption.get.asInstanceOf[AwaitingEngineWork]
    val work = started.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(started, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, rows.map { (moves, cp) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16)
      }), 1L).toOption.get

  private def completed(state: CommentaryJobState): CompletedCommentaryJob = state match
    case done: CompletedCommentaryJob => done
    case other => fail("expected finite local completion, got " + other.getClass.getSimpleName)

  private def reading(state: CommentaryJobState, owner: String, fact: String): Reading =
    state.proposalEntries.find(_.request.id == owner).get.readings.find(_.requirementId == fact).get

  test("the attempted exact fact reports its local cap while keeping its partial unknown result"):
    assertEquals(history.extend(checked).toOption.get.currentLegalMoveUcis, List("e1e2"))
    for limit <- List(600, 1, 0) do
      val done = completed(run(limit, List(supplied)))
      val known = reading(done, supplied.id, "checked")
      val later = reading(done, supplied.id, "recovered")
      val result = later.result.get
      val prepared = ObservedLineProtocol.prepare(done.observationsPacket.get)
      assertEquals(known.state, "supported")
      assertEquals(known.stopReason, None)
      assertEquals(done.progress.physicalWorksIssued, 1)
      assertEquals(done.progress.physicalReportsAccepted, 1)
      assertEquals(done.responseSearches, Nil)
      assertEquals(done.continuationInquiries, Nil)
      assert(prepared.paths.forall(_.moves.size <= 7))
      if limit > 0 then
        assertEquals(done.localExpansionsUsed, 1)
        assertEquals(later.state, "supported")
        assertEquals(later.stopReason, None)
        assertEquals(result.state, "supported")
        assertEquals(result.needs, Nil)
        assertEquals(result.samples.flatMap(_.value).distinct, List("e2"))
        assert(prepared.paths.exists(_.moves == recovered))
      else
        assertEquals(done.localExpansionsUsed, 0)
        assertEquals(later.state, "stopped")
        assertEquals(later.stopReason, Some("local_expansion_limit"))
        assertEquals(result.state, "partial")
        assert(result.samples.forall(_.value.isEmpty))
        assert(result.needs.exists(need => need.kind == "continuation" &&
          need.prefixMoves == checked && need.horizon.contains(7)))
        assert(!prepared.paths.exists(_.moves.startsWith(recovered)))

  test("a shorter satisfied or refuted owner on the same line is not labeled cap blocked"):
    val shorter = request("shorter-owners", List(
      Fact("location-now", Path("exchange", Declared(checked), extend = true),
        Predicate("location", Some(Piece("white", "king", "e1"))), Ply(6), expected = Some("e1")),
      Fact("wrong-check", Path("exchange", Declared(checked)),
        Predicate("check", side = Some("white")), Ply(6), expected = Some("absent"))))
    val done = completed(run(0, List(supplied, shorter)))
    assertEquals(reading(done, shorter.id, "location-now").state, "supported")
    assertEquals(reading(done, shorter.id, "wrong-check").state, "refuted")
    shorter.requirements.foreach(fact =>
      assert(!reading(done, shorter.id, fact.id).stopReason.contains("local_expansion_limit")))
    assert(reading(done, shorter.id, "location-now").result.get.needs.isEmpty)
    assert(reading(done, shorter.id, "wrong-check").result.get.needs.isEmpty)

  test("cap exhaustion does not label an unattempted nonunique fact at another prefix"):
    val quiet = List("e1g1")
    assert(history.extend(quiet).toOption.get.currentLegalMoveUcis.size > 1)
    val outside = request("unattempted-other-prefix", List(
      Fact("later-location", Path("quiet", Declared(quiet), extend = true),
        Predicate("location", Some(Piece("white", "king", "e1"))), Ply(2), expected = Some("g1"))))
    val state = run(0, List(supplied, outside))
    val untouched = reading(state, outside.id, "later-location")
    assert(!untouched.stopReason.contains("local_expansion_limit"))
    assertEquals(untouched.result.get.state, "partial")
    assert(untouched.result.get.needs.exists(need => need.kind == "continuation" &&
      need.prefixMoves == quiet && need.horizon.contains(2)))
    assertEquals(state.localExpansionsUsed, 0)
    assertEquals(state.progress.physicalReportsAccepted, 1)

  test("an unavailable original actor does not borrow another fact's cap reason"):
    val foreign = request("wrong-original-role", List(
      Fact("foreign-actor", Path("exchange", Declared(checked), extend = true),
        Predicate("location", Some(Piece("white", "bishop", "e1"))), Ply(6), expected = Some("e1"))))
    val done = completed(run(0, List(supplied, foreign)))
    val untouched = reading(done, foreign.id, "foreign-actor")
    assert(!untouched.stopReason.contains("local_expansion_limit"))
    assert(untouched.result.get.samples.forall(_.value.isEmpty))
    assert(!untouched.result.get.needs.exists(_.kind == "continuation"))

  test("two pending owners sharing the failed edge retain their own exact unknown needs"):
    val other = request("other-pending-owner", List(
      Fact("check-after", Path("same-exchange", Declared(checked), extend = true),
        Predicate("check", side = Some("white")), Ply(7), expected = Some("absent"))))
    val done = completed(run(0, List(supplied, other)))
    List(supplied.id -> "recovered", other.id -> "check-after").foreach { (owner, fact) =>
      val value = reading(done, owner, fact)
      assertEquals(value.state, "stopped")
      assertEquals(value.stopReason, Some("local_expansion_limit"))
      assertEquals(value.result.get.state, "partial")
      assert(value.result.get.samples.forall(_.value.isEmpty))
      assert(value.result.get.needs.exists(need => need.kind == "continuation" &&
        need.prefixMoves == checked && need.horizon.contains(7)))
    }
    assertEquals(reading(done, supplied.id, "checked").state, "supported")

  test("cap reason publication changes neither the root judgment nor the existing physical capture prices"):
    val done = completed(run(0, List(supplied)))
    val control = completed(run(0, Nil))
    val packet = done.observationsPacket.get
    val baseline = control.observationsPacket.get
    assertEquals(packet.primary, baseline.primary)
    assertEquals(packet.assembly.input.evaluationObservations, baseline.assembly.input.evaluationObservations)
    val beforePaths = ObservedLineProtocol.prepare(baseline)
    val currentPaths = ObservedLineProtocol.prepare(packet)
    def canonicalPaths(paths: ObservedLineProtocol.PreparedPaths) = paths.paths.map(path =>
      (path.line.ref.id, path.moves, path.ids, path.positions.map(_.id)))
    assertEquals(canonicalPaths(currentPaths), canonicalPaths(beforePaths))
    def captureLedger(paths: ObservedLineProtocol.PreparedPaths) = paths.continuationReadings.map(reading =>
      reading.path.moves -> reading.events.map(_.json(reading.path.positions.head.id.get)))
    assertEquals(captureLedger(currentPaths), captureLedger(beforePaths))
    assertEquals(done.rootSearches.map(_.work), control.rootSearches.map(_.work))
    assertEquals(done.assessmentNeedsRefresh, control.assessmentNeedsRefresh)
    val prepared = ObservedLineProtocol.prepare(packet)
    assert(prepared.continuationReadings.flatMap(_.events).exists(event =>
      event.originalVictim.exists(piece => piece.side.white && piece.role.name == "queen")))
    assert(!prepared.paths.exists(_.moves.startsWith(recovered)))
    def pricedSelection(value: CompletedCommentaryJob): JsValue =
      val observations = RuntimeProtocol.observationsJson(value.observationsPacket.get, Nil, Nil,
        Nil, value.preparedPaths, Nil)
      observations \ "selection" \ "candidates" match
        case result: JsDefined => result.value
        case _ => fail("the retained root selection must be available")
    assertEquals(pricedSelection(done), pricedSelection(control))

  test("a partially advanced forced path keeps the cap at its exact still unknown horizon"):
    // Composed legal position, scoped identity begins at this FEN.
    // Qb7+ Qxb7+ Ka5 has two forced reply edges; no chess value is claimed.
    val fen = "k7/5Q2/K7/7R/8/8/6q1/8 w - - 0 1"
    val local = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val prefix = List("f7b7")
    val through = prefix :+ "g2b7"
    val full = through :+ "a6a5"
    assertEquals(local.extend(prefix).toOption.get.currentLegalMoveUcis, List("g2b7"))
    assertEquals(local.extend(through).toOption.get.currentLegalMoveUcis, List("a6a5"))
    val fact = request("partially-advanced-owner", List(
      Fact("king-after", Path("forced-three", Declared(prefix), extend = true),
        Predicate("location", Some(Piece("white", "king", "a6"))), Ply(3), expected = Some("a5"))))
    val started = CommentaryJobReducer.startJob(fen, Nil, fen, prefix.head,
      local.extend(prefix).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = false, maximumLocalExpansions = 1),
      0L, proposals = List(fact)).toOption.get.asInstanceOf[AwaitingEngineWork]
    val work = started.issuedWork
    val done = completed(CommentaryJobReducer.submitIssuedWorkReport(started,
      EngineWorkReport.Completed(profile, work.workId, work.executionKeySha256, List(
        ReportedEngineLineSuffix(prefix, ReportedWhiteEngineScore.Centipawns(100), 16),
        ReportedEngineLineSuffix(List("a6b5"), ReportedWhiteEngineScore.Centipawns(90), 16),
        ReportedEngineLineSuffix(List("h5h4"), ReportedWhiteEngineScore.Centipawns(80), 16))), 1L).toOption.get)
    val later = reading(done, fact.id, "king-after")
    assertEquals(later.state, "stopped")
    assertEquals(later.stopReason, Some("local_expansion_limit"))
    assertEquals(later.result.get.state, "partial")
    assert(later.result.get.samples.forall(_.value.isEmpty))
    assert(later.result.get.needs.exists(need => need.kind == "continuation" &&
      need.prefixMoves == through && need.horizon.contains(3)))
    val prepared = ObservedLineProtocol.prepare(done.observationsPacket.get)
    assert(prepared.paths.exists(_.moves == through))
    assert(!prepared.paths.exists(_.moves.startsWith(full)))
    assert(prepared.continuationReadings.exists(reading => reading.path.moves == through &&
      reading.events.exists(event => event.originalVictim.exists(piece =>
        piece.side.white && piece.role.name == "queen") &&
        reading.path.ids.lift(1).contains(event.occurrenceId))))
    assertEquals(done.localExpansionsUsed, 1)
    assertEquals(done.progress.physicalWorksIssued, 1)
    assertEquals(done.progress.physicalReportsAccepted, 1)
