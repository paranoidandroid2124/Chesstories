package io.chesstory.runtime

import java.nio.file.{ Files, Path }
import java.nio.charset.StandardCharsets.UTF_8
import play.api.libs.json.*

/** Recorded reports verify scheduling and ownership, never engine strength or causal sufficiency. */
class CriticalComparisonSchedulingTest extends munit.FunSuite:
  private def afterPrefix(remainingMs: Option[Long] = None,
      fixtureName: String = "selected-dependency-prefix-produced.json",
      reportLimit: Int = Int.MaxValue): CommentaryJobState =
    val relative = s"judgment-evaluation/fixtures/public-commentary-v6/$fixtureName"
    val fixturePath = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val fixture = Json.parse(Files.readString(fixturePath, UTF_8))
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((fixture \ "request").get).getBytes(UTF_8)).toOption.get
    var state: CommentaryJobState = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 30001L, request.engineProfile, investigateChoices = true, previewDepth = Some(8)),
      1L).toOption.get
    val reports = (fixture \ "saved_reports").as[List[JsObject]].take(reportLimit)
    reports.zipWithIndex.foreach { (saved, index) =>
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      val awaiting = state.asInstanceOf[AwaitingEngineWork]
      assertEquals(report.reportedWorkId, awaiting.issuedWork.workId)
      assertEquals(report.reportedExecutionKeySha256, awaiting.issuedWork.executionKeySha256)
      val now = if index == reports.size - 1 then remainingMs.fold(index.toLong + 2L)(30001L - _) else index.toLong + 2L
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, now).toOption.get
    }
    state

  test("a ready root dependency precedes a downstream fact after the stronger reply changes"):
    val state = afterPrefix()
    val next = state.asInstanceOf[AwaitingEngineWork]
    val timing = CommentaryIdea.reserveTimings(next.proposalEntries).find(value =>
      value.owner.comparison.referenceMove == next.playedMoveUci).get
    assert(timing.source.exists(source => source.answer.evaluationRelation == "better" &&
      timing.sourceArm == "alternative"), "the returned alternative reply is stronger than the root PV reply")
    assertEquals(next.issuedWork.searchAnchor.map(_.prefixMoves), Some(timing.test.prefixMovesUci))
    assertEquals(next.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List(timing.test.referenceMove, timing.test.alternativeMove)))
    assert(next.proposalEntries.exists(_.readings.exists(reading => reading.requirementId == timing.test.id &&
      reading.state == "awaiting_engine" && reading.workId.contains(next.issuedWork.workId))))

  test("a ready comparison can use the short node allowance while keeping reading time and its exact owner"):
    List(7000L, 5500L).foreach { remaining =>
      val next = afterPrefix(Some(remaining)).asInstanceOf[AwaitingEngineWork]
      val timing = CommentaryIdea.reserveTimings(next.proposalEntries).head
      assertEquals(next.issuedWork.searchAnchor.map(_.prefixMoves), Some(timing.test.prefixMovesUci))
      assertEquals(next.issuedWork.searchLimits, EngineSearchLimits(16, 20000000, 2500, 2))
      assertEquals(next.issuedWork.maxSearchElapsedMs, 3500)
      assert(next.proposalEntries.exists(_.readings.exists(reading => reading.requirementId == timing.test.id &&
        reading.state == "awaiting_engine" && reading.workId.contains(next.issuedWork.workId))))
    }
    assert(afterPrefix(Some(5499L)).isInstanceOf[CompletedCommentaryJob],
      "a completed prefix does not replenish the request or consume the reserved reading time")

  test("a recorded node-exhausted mandatory focus can retry once with a short allowance"):
    List(7000L, 5500L).foreach { remaining =>
      val next = afterPrefix(Some(remaining), "node-exhausted-focus-prefix-produced.json")
        .asInstanceOf[AwaitingEngineWork]
      assertEquals(next.issuedWork.purpose, EngineWorkPurpose.FocusComparison)
      assertEquals(next.issuedWork.searchLimits, EngineSearchLimits(16, 20000000, 2500, 2))
      assertEquals(next.issuedWork.maxSearchElapsedMs, 3500)
      val stopped = next.rootSearches.find(_.stopReason.contains(CommentaryJobStopCondition.SearchBudgetExhausted)).get
      assertEquals(next.issuedWork.rootRestriction, stopped.work.rootRestriction)
      assertEquals(next.issuedWork.enginePositionInitialFen, stopped.work.enginePositionInitialFen)
      assertEquals(next.issuedWork.enginePositionMovesUci, stopped.work.enginePositionMovesUci)
      assertEquals(next.verification.get.completedDepth, None)
      val failed = CommentaryJobReducer.submitIssuedWorkReport(next, EngineWorkReport.ExecutorFailed(
        next.engineProfile, next.issuedWork.workId, next.issuedWork.executionKeySha256,
        EngineWorkFailure.BudgetExhausted), 30001L - remaining + 1L).toOption.get
      assert(failed.isInstanceOf[CompletedCommentaryJob])
      assertEquals(failed.verification.get.state, "stopped")
    }
    assert(afterPrefix(Some(5499L), "node-exhausted-focus-prefix-produced.json").isInstanceOf[CompletedCommentaryJob])

  test("a historical root report cannot answer the currently owned first-opponent counter"):
    // The actual five-report recording remains intact. Only its first two root
    // works match this agenda; failed-refresh behavior has a separate controlled test.
    val name = "failed-assessment-refresh-prefix-produced.json"
    val next = afterPrefix(fixtureName = name, reportLimit = 2).asInstanceOf[AwaitingEngineWork]
    val work = next.issuedWork
    assertEquals(work.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(work.searchAnchor.map(_.prefixMoves), Some(List("g3g4")))
    assertEquals(work.enginePositionMovesUci, next.rootSearches.head.work.enginePositionMovesUci :+ "g3g4")
    assertEquals(work.rootRestriction, EngineRootRestriction.Unrestricted)
    val entry = next.proposalEntries.find(value => value.request.origin.question.producer == "C-plan-counter" &&
      value.request.requirements.exists {
        case pair: CommentaryProposal.CompareMoves => pair.id == "contrast_strength" &&
          pair.prefixMovesUci == List("g3g4") && pair.referenceMove == "e6e7" && pair.alternativeMove == "f6f5"
        case _ => false
      }).get
    assert(CommentaryIdea.currentQuestions(next.proposalEntries).exists(_.request.id == entry.request.id))
    assert(CommentaryProposal.neededRequirementIds(entry)("contrast_strength"))
    assertEquals(entry.readings.find(_.requirementId == "contrast_context").get.state, "supported")
    val pending = entry.readings.find(_.requirementId == "contrast_strength").get
    assertEquals(pending.state, "awaiting_engine")
    assertEquals(pending.workId, Some(work.workId))
    assertEquals(pending.answer, None)
    assert(work.proposalRefs.contains(entry.request.id -> "contrast_strength"))

    val relative = s"judgment-evaluation/fixtures/public-commentary-v6/$name"
    val fixturePath = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val fixture = Json.parse(Files.readString(fixturePath, UTF_8))
    val reports = (fixture \ "saved_reports").as[List[JsObject]]
    assertEquals(reports.size, 5, "retain the complete historical actual-work recording")
    assertEquals((reports(2) \ "issued_work" \ "purpose").as[String], "investigation_comparison")
    assertEquals((reports(2) \ "issued_work" \ "root_restriction" \ "moves_uci").as[List[String]],
      List("f4g4", "f4f3"))
    val recorded = PositionCommentaryJobProtocol.parseEngineWorkReport(
      Json.stringify((reports(2) \ "report").get).getBytes(UTF_8)).toOption.get
    assertEquals(recorded.reportedExecutionKeySha256,
      (reports(2) \ "issued_work" \ "execution_key_sha256").as[String])
    assertEquals(recorded.reportedWorkId, work.workId, "a work ordinal alone is not ownership")
    assertNotEquals(recorded.reportedExecutionKeySha256, work.executionKeySha256)
    assertEquals(CommentaryJobReducer.submitIssuedWorkReport(next, recorded, 4L),
      Left(EngineWorkReportRejection.WorkNotOutstanding))
    val inquiry = next.inquiries.find(value => value.question.alternativeMove == "f4f3").get
    assertEquals(inquiry.question.referenceMove, "f4g4")
    assertEquals(inquiry.state, "deferred")
    assertEquals(inquiry.answer, None, "the rejected historical pair is not an observed current answer")
    assertEquals(inquiry.assessmentEffect, None)
    assertEquals(next.assessmentNeedsRefresh, false, "no owned root rebound has occurred")
    assertEquals(next.progress.physicalReportsAccepted, 2)

  test("an available entry finishes its one missing value comparison after four explorations"):
    val fixture = "available-entry-dependency-prefix-produced.json"
    val next = afterPrefix(fixtureName = fixture).asInstanceOf[AwaitingEngineWork]
    val entry = CommentaryIdea.reserveEntries(next.proposalEntries).find(_.optionTest.nonEmpty).get
    val comparison = entry.optionTest.get
    assertEquals(comparison.referenceMove, "f4f5")
    assertEquals(comparison.alternativeMove, "e4e5")
    assertEquals(next.issuedWork.searchAnchor.map(_.prefixMoves), Some(comparison.prefixMovesUci))
    assertEquals(next.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List(comparison.referenceMove, comparison.alternativeMove)))
    assertEquals(next.issuedWork.workId, "work:6", "two root searches and four comparisons precede this dependency")
    val requirements = next.proposalEntries.map(_.request.requirements.size).sum
    assert(requirements > 580 && requirements <= 600,
      s"one comparison must fit near the requirement cap without copying its parent's facts: $requirements")
    assertEquals(next.proposalEntries.count(_.request.requirements.exists(_.id == comparison.id)), 1)
    assert(afterPrefix(Some(5499L), fixture).isInstanceOf[CompletedCommentaryJob],
      "a selected dependency still cannot consume the delivery reserve or extend the deadline")
