package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory

/** The legal retained-pawn fixture owns the real reducer's question and pool.
  * All reports here are invented scheduling receipts, not an engine judgment
  * of the endgame or a proof of which counter is strongest. */
class FailedProposalPoolTest extends munit.FunSuite:
  import CommentaryProposal.*

  private val fen = "8/5p2/8/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35"
  private val actual = ("g3g4 f7f6 f3e3 b4b3 e3f3 e5d6 f3f4 d6e6 " +
    "e4e5 f6e5 f4g5 e5e4 g5f4 e6d5 g4g5 e4e3 f4e3 d5e5 g5g6 e5f6 g6g7 f6g7").split(" ").toList
  private val retained = "f3e3 e5e6 e3f4 e6f6 e4e5 f6e7 f4e4 e7e6 g3g4 b4b3 g4g5 e6d7 e4d5".split(" ").toList
  private val profile = CommentaryEngineProfile.ServerAdmitted

  private def selectedPool(): AwaitingEngineWork =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
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
    val selected = CommentaryJobReducer.submitIssuedWorkReport(firstReply,
      EngineWorkReport.Completed(profile, firstReply.issuedWork.workId, firstReply.issuedWork.executionKeySha256,
        List(retained.drop(1) -> 240, List("f7f6", "e3f3", "e5d6") -> 500).map { (moves, score) =>
          ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(score), firstReply.issuedWork.requiredDepth)
        }), 3L).toOption.get.asInstanceOf[AwaitingEngineWork]
    assertEquals(selected.issuedWork.searchAnchor.map(_.prefixMoves), Some(retained.take(11)))
    assertEquals(selected.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("e6d7", "f7f5", "f7f6")))
    selected

  test("a failed entire pawn pool leaves both counters unknown without reissuing it through the other pair"):
    val selected = selectedPool()
    val failed = selected.issuedWork
    val stopped = CommentaryJobReducer.submitIssuedWorkReport(selected,
      EngineWorkReport.ExecutorFailed(profile, failed.workId, failed.executionKeySha256,
        EngineWorkFailure.BudgetExhausted), 5002L).toOption.get
    stopped match
      case awaiting: AwaitingEngineWork =>
        val work = awaiting.issuedWork
        assert(!CommentaryJobReducer.sameProposalSearch(failed, work), "f6 cannot reissue the same failed f5/f6 pool")
        assertEquals(work.searchAnchor.map(_.prefixMoves), Some(retained.take(8)))
        assertEquals(work.enginePositionInitialFen, fen)
        assertEquals(work.enginePositionMovesUci, retained.take(8))
        val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
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
    assertEquals(stopped.progress.physicalWorksIssued,
      selected.progress.physicalWorksIssued + (if stopped.isInstanceOf[AwaitingEngineWork] then 1 else 0))
    assertEquals(stopped.progress.physicalReportsAccepted, selected.progress.physicalReportsAccepted + 1)
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
      assertEquals(reading.workId,
        Option.when(failed.proposalRefs.contains(entry.request.id -> pair.id))(failed.workId))
    assert(CommentaryIdea.reserveEntries(stopped.proposalEntries).forall(_.testAnswer.isEmpty),
      "an exhausted counter is not a verified functional entry")
    assertEquals(stopped.rootSearches.flatMap(_.receipt).map(_.lines.map(_.evaluation)),
      selected.rootSearches.flatMap(_.receipt).map(_.lines.map(_.evaluation)),
      "an unfinished child comparison cannot alter the numeric root assessment")

  test("an unissued alias inherits the unknown scope without borrowing the failed work's owner or stopping another question"):
    val selected = selectedPool()
    val failed = selected.issuedWork
    val (original, pair) = selected.proposalEntries.flatMap(entry => entry.request.requirements.collect {
      case pair: CompareMoves if pair.id == "reserve_counter" && pair.alternativeMove == "f7f6" => entry -> pair
    }).head
    val alias = original.copy(request = original.request.copy(id = "unissued-pool-alias"),
      readings = original.readings.map(reading => if reading.requirementId == pair.id then
        reading.copy(state = "deferred", workId = None, answer = None) else reading))
    val independentPair = CompareMoves("independent", List(actual.head), "f7f6", "e5d6")
    val independent = CommentaryProposal.entries(List(Request("independent-root-reply",
      original.request.origin, List(independentPair)))).head
    val attempted = failed.copy(workId = "not-issued", executionKeySha256 = "not-issued-key",
      proposalRefs = failed.proposalRefs :+ (alias.request.id -> pair.id))
    val entries = CommentaryJobReducer.exhaustedProposalEntries(
      selected.proposalEntries ++ List(alias, independent), pair, attempted, failed)
    val unknown = entries.find(_.request.id == alias.request.id).get.readings.find(_.requirementId == pair.id).get
    assertEquals(unknown.state, "stopped")
    assertEquals(unknown.stopReason, Some("search_budget_exhausted"))
    assertEquals(unknown.answer, None)
    assertEquals(unknown.workId, None, "a prospective alias was never an actual issued owner")
    assertEquals(entries.find(_.request.id == independent.request.id).get, independent)

  test("pool identity keeps exact history, position ownership and allowance while excluding only issuance and alias names"):
    val work = selectedPool().issuedWork
    val renamed = work.copy(workId = "new-work", executionKeySha256 = "new-key", proposalRefs = Nil,
      rootRestriction = EngineRootRestriction.Restricted(List("f7f6", "e6d7", "f7f5")))
    assert(CommentaryJobReducer.sameProposalSearch(work, renamed))
    val distinct = List(
      renamed.copy(enginePositionMovesUci = renamed.enginePositionMovesUci.dropRight(2)),
      renamed.copy(enginePositionInitialFen = renamed.searchFen),
      renamed.copy(searchAnchor = renamed.searchAnchor.map(_.copy(rootOccurrenceId = "another-owner"))),
      renamed.copy(rootRestriction = EngineRootRestriction.Restricted(List("e6d7", "f7f6"))),
      renamed.copy(rootRestriction = EngineRootRestriction.Unrestricted),
      renamed.copy(searchLimits = renamed.searchLimits.copy(depth = 20)),
      renamed.copy(searchLimits = renamed.searchLimits.copy(nodes = 40000000, movetimeMs = 8000)),
      renamed.copy(searchLimits = renamed.searchLimits.copy(multiPv = 2)),
      renamed.copy(maxSearchElapsedMs = renamed.maxSearchElapsedMs + 1000))
    for other <- distinct do assert(!CommentaryJobReducer.sameProposalSearch(work, other), clues(other))
    // Only one profile is server-admitted today. The complete case-class
    // comparison also includes that profile; no second profile is invented.
