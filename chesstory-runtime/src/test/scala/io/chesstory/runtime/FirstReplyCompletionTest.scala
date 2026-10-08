package io.chesstory.runtime

import java.nio.file.{ Files, Path }
import java.nio.charset.StandardCharsets.UTF_8
import lila.chessjudgment.model.judgment.{EvidenceRecord, RelativeAssessmentEvidence}
import play.api.libs.json.*

/** The prefix contains actual reports. Later invented scores test ownership and completion,
  * never the strongest reply, the cause of an evaluation, or a chess explanation's truth.
  */
class FirstReplyCompletionTest extends munit.FunSuite:
  import CommentaryProposal.*

  private lazy val fixture =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/first-reply-dependency-prefix-produced.json"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    Json.parse(Files.readString(path, UTF_8))
  private lazy val request = PositionCommentaryJobProtocol.parseJobRequest(
    Json.stringify((fixture \ "request").get).getBytes(UTF_8)).toOption.get
  private lazy val reports = (fixture \ "saved_reports").as[List[JsObject]].map(saved =>
    PositionCommentaryJobProtocol.parseEngineWorkReport(
      Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get)

  private val root = "e8d7"
  private val actual = "c6d5"
  private val observed = "f6h4"
  private val counter = "g6g5"
  private val otherCounter = "g7f8"
  private val firstPair = List(root) -> Set(observed, counter)

  private def awaiting(state: CommentaryJobState): AwaitingEngineWork = state match
    case value: AwaitingEngineWork => value
    case other => fail(s"expected another owned work, got ${other.getClass.getSimpleName}")

  private def prefix(count: Int = 4, proposals: List[Request] = Nil): CommentaryJobState =
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 30001L, request.engineProfile, investigateChoices = true,
        previewDepth = Some(8)), 1L, proposals = proposals).toOption.get
    reports.take(count).zipWithIndex.foreach { (report, index) =>
      val active = awaiting(state)
      assertEquals(report.reportedWorkId, active.issuedWork.workId)
      assertEquals(report.reportedExecutionKeySha256, active.issuedWork.executionKeySha256)
      state = CommentaryJobReducer.submitIssuedWorkReport(active, report, index.toLong + 2L).toOption.get
    }
    state

  private def suffixes(index: Int): List[ReportedEngineLineSuffix] = reports(index) match
    case EngineWorkReport.Completed(_, _, _, lines) => lines
    case _ => fail("the recorded prefix contains completed reports")

  private def submit(state: AwaitingEngineWork, lines: List[ReportedEngineLineSuffix],
      now: Long = 10L): CommentaryJobState =
    val work = state.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(state.engineProfile,
      work.workId, work.executionKeySha256, lines), now).toOption.get

  private def failed(state: AwaitingEngineWork, now: Long = 10L): CommentaryJobState =
    val work = state.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.ExecutorFailed(state.engineProfile,
      work.workId, work.executionKeySha256, EngineWorkFailure.BudgetExhausted), now).toOption.get

  private def issuedPair(state: CommentaryJobState): Option[(List[String], Set[String])] = state match
    case active: AwaitingEngineWork => active.issuedWork.rootRestriction match
      case EngineRootRestriction.Restricted(moves) => Some(
        active.issuedWork.searchAnchor.fold(List.empty[String])(_.prefixMoves) -> moves.toSet)
      case _ => None
    case _ => None

  private def ownedFirstPair(state: CommentaryJobState): Boolean = state match
    case next: AwaitingEngineWork => comparisons(next, firstPair).exists((entry, value, reading) =>
      reading.state == "awaiting_engine" && reading.workId.contains(next.issuedWork.workId) &&
        next.issuedWork.proposalRefs.contains(entry.request.id -> value.id))
    case _ => false

  private def assertFirstSurvey(state: AwaitingEngineWork): Unit =
    assertEquals(state.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(state.issuedWork.searchAnchor.map(_.prefixMoves), Some(List(root)))
    assertEquals(state.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(state.issuedWork.searchLimits, EngineSearchLimits(16, 20000000, 5000, 3))
    assertEquals(state.issuedWork.maxSearchElapsedMs, 6000)
    assert(ownedFirstPair(state), "the broad survey retains the exact named comparison's ready owner")

  private def rankedReplies: List[ReportedEngineLineSuffix] =
    // The best returned reply is observed; the named quiet counter is absent.
    // One-ply legal lines and close synthetic scores isolate scheduling from root re-verification.
    List(observed -> 307, otherCounter -> 308, "f3f4" -> 309).map { (move, score) =>
      ReportedEngineLineSuffix(List(move), ReportedWhiteEngineScore.Centipawns(score), 16)
    }

  private def submitted(id: String, value: CompareMoves): Request =
    val source = Source("user", id)
    Request(id, Origin(source, source, source, source), List(value))

  private def comparisons(state: CommentaryJobState, pair: (List[String], Set[String])) =
    state.proposalEntries.flatMap(entry => entry.request.requirements.collect {
      case value: CompareMoves if CommentaryIdea.pair(value) == pair =>
        (entry, value, entry.readings.find(_.requirementId == value.id).get)
    })

  private def basicReview(state: CommentaryJobState): CompletedMoveReview =
    val result = state match
      case done: CompletedCommentaryJob => done.result
      case _ => state.assessment.get.result
    result match
      case CompletedPositionCommentary.FocusedMoveReview(_, review) => review
      case _ => fail("expected the preserved move review")

  private def rootAssessment(state: CommentaryJobState) =
    basicReview(state).judgmentPacket.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeAssessmentEvidence(value), _) => value
    }

  private def readyFirstPairs(survey: AwaitingEngineWork): Set[(List[String], Set[String])] =
    val current = CommentaryIdea.currentQuestions(survey.proposalEntries).map(_.request.id).toSet
    val legal = survey.observationsPacket.get.assembly.input.positionHistory
      .extend(List(root)).toOption.get.currentLegalMoveUcis.toSet
    val ready = survey.proposalEntries.filter(entry => entry.admission == "generated" && current(entry.request.id))
      .flatMap { entry => entry.request.requirements.collect {
        case value: CompareMoves if value.prefixMovesUci == List(root) && value.expectedRelation.nonEmpty &&
            Set(value.referenceMove, value.alternativeMove)(observed) &&
            Set(value.referenceMove, value.alternativeMove).size == 2 &&
            List(value.referenceMove, value.alternativeMove).forall(legal) &&
            neededRequirementIds(entry)(value.id) &&
            comparisonContexts(entry, value, readyOnly = true).nonEmpty &&
            entry.readings.exists(reading => reading.requirementId == value.id &&
              reading.state == "awaiting_engine" && reading.answer.isEmpty &&
              reading.workId.contains(survey.issuedWork.workId)) &&
            survey.issuedWork.proposalRefs.contains(entry.request.id -> value.id) => CommentaryIdea.pair(value)
      }}.toSet
    assert(ready(firstPair), "the original generated g5 condition remains a ready shared survey owner")
    ready

  private def selectedReadyPair(exact: AwaitingEngineWork, ready: Set[(List[String], Set[String])]) =
    assertEquals(exact.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(exact.issuedWork.searchAnchor.map(_.prefixMoves), Some(List(root)))
    val moves = exact.issuedWork.rootRestriction match
      case EngineRootRestriction.Restricted(values) => values
      case _ => fail("the missing named arm requires an exact restricted pair, rather than another full survey")
    assertEquals(moves.size, 2)
    assertEquals(moves.distinct.size, 2)
    assert(moves.contains(observed), "the pair retains the owning root's observed first response")
    val pair = List(root) -> moves.toSet
    assert(ready(pair), "the selected pair was current, legal, condition-ready and owned before the survey")
    val legal = exact.observationsPacket.get.assembly.input.positionHistory
      .extend(List(root)).toOption.get.currentLegalMoveUcis.toSet
    assert(moves.forall(legal))
    val history = exact.observationsPacket.get.assembly.input.positionHistory
    assertEquals(exact.issuedWork.enginePositionInitialFen, history.initialFen)
    assertEquals(exact.issuedWork.enginePositionMovesUci, history.movePrefixUci :+ root)
    assert(comparisons(exact, pair).exists((entry, value, reading) =>
      entry.admission == "generated" && comparisonContexts(entry, value, readyOnly = true).nonEmpty &&
        reading.state == "awaiting_engine" && reading.workId.contains(exact.issuedWork.workId) &&
        exact.issuedWork.proposalRefs.contains(entry.request.id -> value.id)))
    assert(exact.issuedWork.proposalRefs.nonEmpty)
    assert(exact.issuedWork.proposalRefs.forall { (proposalId, requirementId) =>
      comparisons(exact, pair).exists((entry, value, _) =>
        entry.request.id == proposalId && value.id == requirementId)
    }, "each exact-work owner belongs to this selected pair")
    pair -> moves.find(_ != observed).get

  private def assertOnlySelectedPair(state: CommentaryJobState, workId: String,
      pair: (List[String], Set[String]), ready: Set[(List[String], Set[String])]): Unit =
    ready.filterNot(_ == pair).foreach { other =>
      assert(comparisons(state, other).forall((_, _, reading) => !reading.answer.exists(_.workId == workId)),
        "the exact pair's receipt cannot answer another missing counter from the broad pool")
    }

  test("a ready first-reply counter is owned by the Focus pair, even outside the latest root bundle"):
    val next = awaiting(prefix())
    val latestRoot = next.rootSearches.filter(_.work.purpose == EngineWorkPurpose.RootSearch).last
    assertEquals(latestRoot.work.workId, "work:2")
    assert(!latestRoot.receipt.get.lines.exists(_.evaluation.moves.head == actual),
      "the unrestricted bundle cannot certify the played/reference pair")
    val focus = next.rootSearches.find(_.work.workId == "work:3").get
    val answer = CommentaryInvestigation.pairedAnswer(next.observationsPacket.get, actual, root,
      focus.work, focus.receipt.get.lines, next.preparedPaths).get
    assertEquals(answer.workId, "work:3")
    val paths = next.preparedPaths.get.paths
    assert(List(answer.referenceLineId, answer.alternativeLineId).exists(id => paths.exists(path =>
      path.line.ref.id == id && path.certified.nonEmpty && path.ids == path.declared &&
        path.moves.startsWith(List(root, observed)))))
    assertEquals(next.issuedWork.workId, "work:4")
    assertFirstSurvey(next)
    val owners = comparisons(next, firstPair).filter(_._3.state == "awaiting_engine")
    assert(owners.exists((entry, value, _) => entry.request.origin.question.producer == "C-plan-counter" &&
      value.id == "contrast_strength" && CommentaryProposal.comparisonContexts(entry, value, readyOnly = true).nonEmpty))
    assert(owners.forall((entry, value, reading) =>
      next.issuedWork.proposalRefs.contains(entry.request.id -> value.id) &&
        reading.workId.contains(next.issuedWork.workId)))

  test("two ordinary comparisons and a first survey leave the last completion slot for the exact named pair"):
    val questions = List("d8-first" -> "e8d8", "e7-second" -> "e8e7").map { (id, move) =>
      submitted(id, CompareMoves("pair", Nil, root, move))
    }
    var state = awaiting(prefix(proposals = questions))
    Set("e8d8", "e8e7").foreach { _ =>
      assertEquals(state.issuedWork.purpose, EngineWorkPurpose.InvestigationComparison)
      assertEquals(state.issuedWork.searchAnchor, None)
      val alternative = state.issuedWork.rootRestriction match
        case EngineRootRestriction.Restricted(moves) => moves.find(_ != root).get
        case _ => fail("the independent submitted query is an exact pair")
      assert(Set("e8d8", "e8e7")(alternative))
      state = awaiting(submit(state, List(suffixes(3).find(_.moves.head == root).get,
        ReportedEngineLineSuffix(List(alternative), ReportedWhiteEngineScore.Centipawns(0), 16))))
    }
    assertEquals(state.issuedWork.workId, "work:6")
    assertFirstSurvey(state)
    val ready = readyFirstPairs(state)
    val assessment = rootAssessment(state)
    val surveyId = state.issuedWork.workId
    state = awaiting(submit(state, rankedReplies))
    assertEquals(state.issuedWork.workId, "work:7", "four root works and three investigations precede the last slot")
    val survey = state.responseSearches.find(_.work.workId == surveyId).get
    assertEquals(survey.lines.head.evaluation.moves.head, observed)
    assert(!survey.lines.exists(_.evaluation.moves.head == counter))
    val (pair, selectedCounter) = selectedReadyPair(state, ready)
    assert(!survey.lines.exists(_.evaluation.moves.head == selectedCounter),
      "the selected legal counter is genuinely absent from the returned top three")
    assert(comparisons(state, pair).forall(_._3.answer.isEmpty))
    val comparisonWorkId = state.issuedWork.workId
    // White scores intentionally make the quiet Black reply worse. Completing a test is not confirming its hypothesis.
    val read = submit(state, List(
      ReportedEngineLineSuffix(List(observed), ReportedWhiteEngineScore.Centipawns(307), 16),
      ReportedEngineLineSuffix(List(selectedCounter), ReportedWhiteEngineScore.Centipawns(347), 16)))
    val answered = comparisons(read, pair).filter(_._3.answer.exists(_.workId == comparisonWorkId))
    assert(answered.nonEmpty)
    assert(answered.forall((_, _, reading) => reading.answer.get.evaluationRelation == "worse"))
    assert(answered.exists((entry, value, reading) => entry.request.origin.question.producer == "C-plan-counter" &&
      value.expectedRelation.contains("better") && reading.state == "refuted"))
    assertOnlySelectedPair(read, comparisonWorkId, pair, ready)
    assertEquals(basicReview(read).bestMoveUci, root)
    assertEquals(rootAssessment(read), assessment, "a named child answer cannot rewrite the root assessment")
    assertNotEquals(issuedPair(read), Some(pair), "a completed named pair is never reissued under an alias")

  test("an old first reply is not promoted after the owning Focus line chooses a different reply"):
    val focus = awaiting(prefix(count = 3))
    val changed = suffixes(3).map { line =>
      if line.moves.head == root then line.copy(moves = suffixes(2).find(_.moves.head == root).get.moves)
      else line
    }
    assertEquals(changed.find(_.moves.head == root).get.moves.take(2), List(root, otherCounter))
    val read = submit(focus, changed)
    assert(!ownedFirstPair(read),
      "an old or off-PV Bxh4 observation does not acquire a current root dependency")

  test("an under-depth Focus pair cannot promote a current first-reply dependency"):
    val focus = awaiting(prefix(count = 3))
    assertEquals(focus.issuedWork.requiredDepth, 16)
    val shallow = submit(focus, suffixes(3).map(_.copy(depth = 15)))
    assert(!ownedFirstPair(shallow))
    assert(comparisons(shallow, firstPair).forall((_, _, reading) => reading.answer.isEmpty))

  test("a report for an unowned work cannot supply the Focus witness"):
    val focus = awaiting(prefix(count = 3))
    val work = focus.issuedWork
    val report = EngineWorkReport.Completed(focus.engineProfile, "work:unowned",
      work.executionKeySha256, suffixes(3))
    assertEquals(CommentaryJobReducer.submitIssuedWorkReport(focus, report, 10L),
      Left(EngineWorkReportRejection.WorkNotOutstanding))
    assertEquals(focus.issuedWork, work)
    assertEquals(focus.progress.physicalReportsAccepted, 3)

  test("a changed condition's unready arm cannot request its strength comparison"):
    val state = prefix()
    val (entry, value, _) = comparisons(state, firstPair).find((entry, _, _) =>
      entry.request.origin.question.producer == "C-plan-counter").get
    assert(neededRequirementIds(entry)(value.id))
    assert(comparisonContexts(entry, value, readyOnly = true).nonEmpty)
    val unready = entry.copy(readings = entry.readings.map(reading =>
      if reading.requirementId == "contrast_context" then reading.copy(state = "deferred", workId = None)
      else if reading.requirementId == value.id then reading.copy(state = "deferred", workId = None)
      else reading))
    assert(!neededRequirementIds(unready)(value.id))
    assertEquals(comparisonContexts(unready, value, readyOnly = true), Nil)

  test("one exhausted comparison leaves its pair unknown and continues an independent question"):
    val aliases = List(
      submitted("same-pair", CompareMoves("a-first", List(root), observed, counter, Some("better"))),
      submitted("reversed-alias", CompareMoves("z-alias", List(root), counter, observed, Some("worse"))),
      submitted("independent-counter", CompareMoves("other", List(root), observed, otherCounter)))
    val survey = awaiting(prefix(proposals = aliases))
    assertFirstSurvey(survey)
    val surveyId = survey.issuedWork.workId
    val selected = awaiting(failed(survey))
    assertEquals(issuedPair(selected), Some(firstPair), "failed broad ranking still leaves the exact pair executable")
    assert(!selected.responseSearches.exists(_.work.workId == surveyId),
      "failed ranking cannot manufacture a completed response search")
    assert(comparisons(selected, firstPair).forall((_, _, reading) =>
      reading.answer.isEmpty && reading.state != "stopped"))
    val failedId = selected.issuedWork.workId
    val next = awaiting(failed(selected, 11L))
    assertEquals(issuedPair(next), Some(List(root) -> Set(observed, otherCounter)))
    assertEquals(basicReview(next).bestMoveUci, root)
    assertEquals(next.progress.selectedCommentariesCompleted, 1)
    assert(!next.rootSearches.exists(_.work.workId == failedId),
      "a failed child comparison cannot become a root search receipt")
    val stopped = comparisons(next, firstPair)
    assert(stopped.exists(_._1.request.id == "same-pair"))
    assert(stopped.exists(_._1.request.id == "reversed-alias"))
    assert(stopped.exists(_._1.request.origin.question.producer == "C-plan-counter"))
    assert(stopped.forall((_, _, reading) => reading.state == "stopped" && reading.answer.isEmpty &&
      reading.stopReason.contains("search_budget_exhausted")))
    stopped.foreach { (entry, value, reading) =>
      val issuedOwner = selected.issuedWork.proposalRefs.contains(entry.request.id -> value.id)
      assertEquals(reading.workId, Option.when(issuedOwner)(failedId),
        "unknown aliases retain the failure's scope without acquiring an unissued work owner")
    }
    val independentId = next.issuedWork.workId
    val read = submit(next, List(
      ReportedEngineLineSuffix(List(observed), ReportedWhiteEngineScore.Centipawns(307), 16),
      ReportedEngineLineSuffix(List(otherCounter), ReportedWhiteEngineScore.Centipawns(307), 16)), 20L)
    assert(comparisons(read, List(root) -> Set(observed, otherCounter)).exists((_, _, reading) =>
      reading.answer.exists(_.workId == independentId)))
    assertNotEquals(issuedPair(read), Some(firstPair),
      "regenerating discovery after another answer cannot reissue an exhausted pair under an alias")
    assert(comparisons(read, firstPair).forall((_, _, reading) => reading.answer.isEmpty && reading.state == "stopped"))
    assertEquals(basicReview(read).bestMoveUci, root)

  test("a current completed all-reply survey supplements a missing named arm exactly once"):
    val survey = awaiting(prefix())
    assertFirstSurvey(survey)
    val ready = readyFirstPairs(survey)
    val assessment = rootAssessment(survey)
    val surveyId = survey.issuedWork.workId
    val exact = awaiting(submit(survey, rankedReplies))
    val recorded = exact.responseSearches.find(_.work.workId == surveyId).get
    assertEquals(recorded.work.rootRestriction, EngineRootRestriction.Unrestricted)
    assertEquals(recorded.lines.head.evaluation.moves.head, observed)
    assert(!recorded.lines.exists(_.evaluation.moves.head == counter),
      "a missing top-three reply has no returned score")
    val (pair, selectedCounter) = selectedReadyPair(exact, ready)
    assert(!recorded.lines.exists(_.evaluation.moves.head == selectedCounter))
    assert(comparisons(exact, pair).forall((_, _, reading) => reading.answer.isEmpty),
      "survey ownership does not claim that the absent branch was compared")
    val exactId = exact.issuedWork.workId
    val read = submit(exact, List(
      ReportedEngineLineSuffix(List(observed), ReportedWhiteEngineScore.Centipawns(307), 16),
      ReportedEngineLineSuffix(List(selectedCounter), ReportedWhiteEngineScore.Centipawns(347), 16)), 20L)
    assert(comparisons(read, pair).exists((_, _, reading) => reading.answer.exists(_.workId == exactId)))
    assert(comparisons(read, pair).exists((_, value, reading) =>
      value.expectedRelation.contains("better") && reading.state == "refuted" &&
        reading.answer.exists(_.workId == exactId)), "completing the selected hypothesis can refute it")
    assertOnlySelectedPair(read, exactId, pair, ready)
    assertNotEquals(issuedPair(read), Some(pair))
    assertEquals(read.responseSearches.count(search => search.anchor.prefixMoves == List(root) &&
      search.work.rootRestriction == EngineRootRestriction.Unrestricted), 1)
    assertEquals(basicReview(read).bestMoveUci, root)
    assertEquals(rootAssessment(read), assessment)

  test("a failed first survey is attempted once and the named pair can still complete"):
    val survey = awaiting(prefix())
    assertFirstSurvey(survey)
    val ready = readyFirstPairs(survey)
    val assessment = rootAssessment(survey)
    val surveyId = survey.issuedWork.workId
    val exact = awaiting(failed(survey))
    val (pair, selectedCounter) = selectedReadyPair(exact, ready)
    assert(!exact.responseSearches.exists(_.work.workId == surveyId))
    assert(comparisons(exact, pair).forall((_, _, reading) =>
      reading.answer.isEmpty && reading.state != "stopped"))
    val exactId = exact.issuedWork.workId
    val read = submit(exact, List(
      ReportedEngineLineSuffix(List(observed), ReportedWhiteEngineScore.Centipawns(307), 16),
      ReportedEngineLineSuffix(List(selectedCounter), ReportedWhiteEngineScore.Centipawns(347), 16)), 20L)
    assert(comparisons(read, pair).exists((_, _, reading) => reading.answer.exists(_.workId == exactId)))
    assert(comparisons(read, pair).exists((_, value, reading) =>
      value.expectedRelation.contains("better") && reading.state == "refuted" &&
        reading.answer.exists(_.workId == exactId)))
    assertOnlySelectedPair(read, exactId, pair, ready)
    assertNotEquals(issuedPair(read), Some(pair))
    read match
      case next: AwaitingEngineWork => assert(!(next.issuedWork.searchAnchor.exists(_.prefixMoves == List(root)) &&
        next.issuedWork.rootRestriction == EngineRootRestriction.Unrestricted),
        "a new ready pair at the same prefix cannot repeat the failed full survey")
      case _ => ()
    assertEquals(basicReview(read).bestMoveUci, root)
    assertEquals(rootAssessment(read), assessment)

  test("an existing typed critical timing dependency keeps priority over general first-reply questions"):
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/selected-dependency-prefix-produced.json"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val data = Json.parse(Files.readString(path, UTF_8))
    val parsed = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((data \ "request").get).getBytes(UTF_8)).toOption.get
    var state = CommentaryJobReducer.startJob(parsed.initialFen, parsed.movePrefixUci,
      parsed.currentFen, parsed.playedMoveUci, parsed.resultingFen,
      CommentaryJobPolicy(16, 30001L, parsed.engineProfile, investigateChoices = true,
        previewDepth = Some(8)), 1L).toOption.get
    (data \ "saved_reports").as[List[JsObject]].zipWithIndex.foreach { (saved, index) =>
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      val next = awaiting(state)
      assertEquals(next.issuedWork.workId, report.reportedWorkId)
      assertEquals(next.issuedWork.executionKeySha256, report.reportedExecutionKeySha256)
      state = CommentaryJobReducer.submitIssuedWorkReport(next, report, index.toLong + 2L).toOption.get
    }
    val next = awaiting(state)
    val timing = CommentaryIdea.reserveTimings(next.proposalEntries).find(value =>
      value.owner.comparison.referenceMove == next.playedMoveUci).get
    assert(timing.source.exists(_.answer.evaluationRelation == "better"))
    assertEquals(next.issuedWork.searchAnchor.map(_.prefixMoves), Some(timing.test.prefixMovesUci))
    assertEquals(next.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List(timing.test.referenceMove, timing.test.alternativeMove)))
    assert(next.issuedWork.proposalRefs.exists((proposalId, requirementId) =>
      requirementId == timing.test.id && next.proposalEntries.exists(entry =>
        entry.request.id == proposalId && entry.readings.exists(reading =>
          reading.requirementId == requirementId && reading.state == "awaiting_engine" &&
            reading.workId.contains(next.issuedWork.workId)))))
