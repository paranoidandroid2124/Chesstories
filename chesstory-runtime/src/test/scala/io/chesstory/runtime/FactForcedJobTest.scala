package io.chesstory.runtime

import lila.chessjudgment.model.judgment.EvaluationSource
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** The exposed Ruy exchange and invented reports test finite local replay, without an engine. */
class FactForcedJobTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  test("an explicit fact replays the unique reply only to its horizon and retains a local-cap partial"):
    val initial = chess.variant.Standard.initialFen.value
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(List(
      "e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")).toOption.get
    // The same queen-exchange frontier is exercised in CheckResponseInvestigationTest.
    val checked = List("f3e5", "d8d4", "e5f3", "d4e4", "d1e2", "e4e2")
    val recovered = checked :+ "e1e2"
    assertEquals(history.extend(checked).toOption.get.currentLegalMoveUcis, List("e1e2"))
    val origin = Origin(Source("user", "forced-fact-question"), Source("user", "forced-fact-path"),
      Source("none", "forced-fact-priority"), Source("none", "forced-fact-evidence"))
    val supplied = Request("forced-facts", origin, List(
      Fact("checked", Path("exchange", Declared(checked)), Predicate("check", side = Some("white")),
        Ply(6), expected = Some("present")),
      Fact("recovered", Path("exchange", Declared(checked), extend = true),
        Predicate("location", Some(Piece("white", "king", "e1"))), Ply(7), expected = Some("e2"))))
    val profile = CommentaryEngineProfile.ServerAdmitted
    val rows = List(checked -> 0, List("e1g1") -> -10, List("d2d4") -> -20)

    for limit <- List(600, 1, 0) do
      val started = CommentaryJobReducer.startJob(history.initialFen, history.movePrefixUci, history.currentFen,
        checked.head, history.extend(List(checked.head)).toOption.get.currentFen,
        CommentaryJobPolicy(16, 10000L, profile, investigateChoices = false, maximumLocalExpansions = limit),
        0L, proposals = List(supplied)).toOption.get.asInstanceOf[AwaitingEngineWork]
      val work = started.issuedWork
      val state = CommentaryJobReducer.submitIssuedWorkReport(started, EngineWorkReport.Completed(profile,
        work.workId, work.executionKeySha256, rows.map { (moves, cp) =>
          ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16)
        }), 1L).toOption.get
      val done = state match
        case value: CompletedCommentaryJob => value
        case other => fail(s"expected local completion at limit $limit, got ${other.getClass.getSimpleName}")
      val entry = done.proposalEntries.find(_.request.id == supplied.id).get
      val known = entry.readings.find(_.requirementId == "checked").get
      val reading = entry.readings.find(_.requirementId == "recovered").get
      val result = reading.result.get
      val packet = done.observationsPacket.get
      val prepared = ObservedLineProtocol.prepare(packet)
      assertEquals(entry.request, supplied)
      assertEquals(known.state, "supported")
      assertEquals(done.progress.physicalWorksIssued, 1)
      assertEquals(done.progress.physicalReportsAccepted, 1)
      assertEquals(done.responseSearches, Nil)
      assertEquals(done.continuationInquiries, Nil)
      assert(packet.assembly.input.evaluationObservations.forall(_.source != EvaluationSource.ResponseComparison))
      assert(prepared.paths.forall(_.moves.size <= 7))

      if limit > 0 then
        assertEquals(done.localExpansionsUsed, 1)
        assertEquals(reading.state, "supported")
        assertEquals(result.state, "supported")
        assertEquals(result.needs, Nil)
        assertEquals(result.samples.flatMap(_.value).distinct, List("e2"))
        assert(result.samples.filter(_.value.nonEmpty).forall(_.throughPly == 7))
        assert(prepared.paths.exists(_.moves == recovered))
      else
        assertEquals(done.localExpansionsUsed, 0)
        assertEquals(reading.state, "partial")
        assertEquals(reading.stopReason, Some("local_expansion_limit"))
        assertEquals(result.state, "partial")
        assert(result.samples.forall(_.value.isEmpty))
        assert(result.needs.exists(need => need.kind == "continuation" &&
          need.prefixMoves == checked && need.horizon.contains(7)))
        assert(!prepared.paths.exists(_.moves.startsWith(recovered)))
