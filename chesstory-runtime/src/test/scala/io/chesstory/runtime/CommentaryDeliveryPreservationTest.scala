package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{ MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput }
import lila.chessjudgment.model.judgment.{ CommentaryLineSource, EvidenceBackedJudgmentPacket, LineNodeRole, SuppliedCommentaryLine }
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, EngineLine }
import play.api.libs.json.*

private[runtime] object CommentaryDeliveryPreservationAssertions extends munit.Assertions:
  def assertRootStopPreservesObservations(
      before: JsObject,
      after: JsObject,
      workId: String,
      stopReason: String
  ): Unit =
    val previous = (before \ "selection" \ "root_searches").as[List[JsObject]]
    val retained = (after \ "selection" \ "root_searches").as[List[JsObject]]
    val pending = previous.filter(search => (search \ "work_id").as[String] == workId)
    assertEquals(pending.size, 1)
    assertEquals((pending.head \ "state").as[String], "awaiting_engine")
    assertEquals((pending.head \ "stop_reason").get, JsNull)
    // Work identity, exact root restriction, admitted candidate bindings and prior receipts are immutable.
    assertEquals(retained.map(_ - "state" - "stop_reason"), previous.map(_ - "state" - "stop_reason"))
    val expected = previous.map { search =>
      if (search \ "work_id").as[String] == workId then
        search ++ Json.obj("state" -> "stopped", "stop_reason" -> stopReason)
      else search
    }
    assertEquals(retained, expected)
    // Equality still covers every independent chess observation and all remaining selection metadata.
    assertEquals(after, before + ("selection" ->
      ((before \ "selection").as[JsObject] + ("root_searches" -> Json.toJson(expected)))))

class CommentaryDeliveryPreservationTest extends munit.FunSuite:
  import IndependentCommentaryExamples.{ history, initial, profile, wire }
  import CommentaryDeliveryPreservationAssertions.assertRootStopPreservesObservations

  private val supplied = List(
    SuppliedCommentaryLine(CommentaryLineSource.Study, List("g1h3", "d7d5", "h3g5", "a1a8")),
    SuppliedCommentaryLine(CommentaryLineSource.PgnRecord, List("e2e4", "d7d5", "e4d5", "d8d5"))
  )
  private val rootMoves = List(
    List("e2e4", "e7e5", "g1f3"), List("d2d4", "d7d5", "c1f4"), List("g1f3", "g8f6")
  )

  private def start(move: String = "g1h3", depth: Int = 16, source: CanonicalPositionHistory = history) =
    CommentaryJobReducer.startJob(source.initialFen, source.movePrefixUci, source.currentFen, move,
      source.extend(List(move)).toOption.get.currentFen, CommentaryJobPolicy(depth, 10000L, profile), 0L,
      if source == history then supplied else Nil).toOption.get.asInstanceOf[AwaitingEngineWork]

  private def report(state: AwaitingEngineWork, moves: List[List[String]]): EngineWorkReport =
    EngineWorkReport.Completed(profile, state.issuedWork.workId, state.issuedWork.executionKeySha256,
      moves.zipWithIndex.map { case (line, index) =>
        ReportedEngineLineSuffix(line, ReportedWhiteEngineScore.Centipawns(30 - index * 10), state.issuedWork.requiredDepth)
      })

  private def submit(state: AwaitingEngineWork, moves: List[List[String]]): CommentaryJobState =
    CommentaryJobReducer.submitIssuedWorkReport(state, report(state, moves), 1L).toOption.get

  private def observations(state: CommentaryJobState): JsObject = (wire(state) \ "observations").as[JsObject]

  private def assertPathsRetained(before: CommentaryJobState, after: CommentaryJobState): Unit =
    val later = (observations(after) \ "paths").as[List[JsObject]]
    (observations(before) \ "paths").as[List[JsObject]].foreach { path =>
      val current = later.find(value => (value \ "id").get == (path \ "id").get).getOrElse(fail("missing path"))
      val receipts = Set("evaluations", "conditional_evaluations")
      assertEquals(JsObject(current.fields.filterNot(field => receipts(field._1))),
        JsObject(path.fields.filterNot(field => receipts(field._1))))
      // New same-path evaluations may append evidence; every previous score and source must survive.
      receipts.foreach { key =>
        val previous = (path \ key).asOpt[List[JsObject]].getOrElse(Nil)
        val retained = (current \ key).asOpt[List[JsObject]].getOrElse(Nil)
        assert(previous.forall(retained.contains), s"lost prior $key")
      }
    }
    val laterPositions = (observations(after) \ "positions").as[List[JsObject]]
    (observations(before) \ "positions").as[List[JsObject]].foreach(position => assert(laterPositions.contains(position)))
    assertEquals((observations(after) \ "input_issues").toOption, (observations(before) \ "input_issues").toOption)

  test("direct evaluation rejection retains legal prefixes and independently valid sibling scores"):
    val prefix = List("e2e4", "d7d5", "e4d5", "d8d5")
    val sibling = List("d2d4", "d7d5", "c1f4")
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(initial, "g1f3",
      List(EngineLine(prefix :+ "a1a8", 30, None, 16), EngineLine(sibling, 20, None, 16)))).get
    assertEquals(input.legalLines.map(_.moves).toSet, Set(List("g1f3"), prefix, sibling))
    assert(input.assessmentCandidates.isEmpty)
    assertEquals(input.evaluationObservations.map(_.evaluation.moves), List(sibling))
    val result = RuntimeProtocol.evaluate(Json.obj("schema_version" -> RuntimeProtocol.RequestSchema,
      "input" -> Json.obj("fen" -> initial, "playedMoveUci" -> "g1f3",
        "variations" -> Json.arr(Json.obj("moves" -> (prefix :+ "a1a8"), "scoreCp" -> 30, "depth" -> 16),
          Json.obj("moves" -> sibling, "scoreCp" -> 20, "depth" -> 16)))))
    assertEquals(result.httpStatus, 200)
    val paths = (result.body \ "move_commentary" \ "observations" \ "paths").as[List[JsObject]]
    val retained = paths.find(path => (path \ "moves").as[List[String]] == prefix).get
    val summary = (retained \ "line_summary").as[JsObject]
    assertEquals((summary \ "material" \ "net_cp").as[Int], 0)
    assertEquals((summary \ "ending" \ "occurrence_id").as[String],
      (retained \ "occurrence_ids").as[List[String]].last)

  test("a contradictory terminal score cannot erase the separately admitted physical replay"):
    val source = history.extend(List("f2f3", "e7e5", "g2g4")).toOption.get
    val active = start("h7h6", source = source)
    val paths = List(List("d8h4"), List("g8f6", "b1c3"), List("b8c6", "b1c3"))
    val stopped = submit(active, paths).asInstanceOf[StoppedCommentaryJob]
    assertEquals(stopped.stopCondition, CommentaryJobStopCondition.InvalidEngineWorkReport)
    assertEquals(stopped.progress.physicalReportsAccepted, 0)
    val input = stopped.observationsPacket.get.assembly.input
    assertEquals(input.legalLines.map(_.moves).toSet,
      Set(List("h7h6"), List("d8h4"), paths(1), paths(2)))
    assert(input.assessmentCandidates.isEmpty)
    assert(input.evaluationObservations.isEmpty)
    assertPathsRetained(active, stopped)

  test("focus failure categories retain supplied prefixes and accepted root observations exactly"):
    val root = start()
    val focus = submit(root, rootMoves).asInstanceOf[AwaitingEngineWork]
    assertPathsRetained(root, focus)
    val stopped = CommentaryJobReducer.submitIssuedWorkReport(focus,
      EngineWorkReport.ExecutorFailed(profile, focus.issuedWork.workId, focus.issuedWork.executionKeySha256,
        EngineWorkFailure.ExecutionFailed), 2L).toOption.get
    assertEquals((wire(stopped) \ "stop_condition").as[String], "engine_execution_failed")
    assertRootStopPreservesObservations(observations(focus), observations(stopped), focus.issuedWork.workId,
      "engine_execution_failed")
    assertEquals(stopped.progress.physicalReportsAccepted, 2)
    val invalid = submit(focus, List(List("e2e4", "e7e5"), List("g1h3", "a1a8")))
    assertEquals((wire(invalid) \ "stop_condition").as[String], "invalid_engine_work_report")
    assertRootStopPreservesObservations(observations(focus), observations(invalid),
      focus.issuedWork.workId, "invalid_engine_work_report")
    assertEquals(invalid.progress.physicalReportsAccepted, 1)
    val wrongKey = EngineWorkReport.ExecutorFailed(profile, focus.issuedWork.workId, "0" * 64,
      EngineWorkFailure.ExecutionFailed)
    assertEquals(CommentaryJobReducer.submitIssuedWorkReport(focus, wrongKey, 2L),
      Left(EngineWorkReportRejection.WorkNotOutstanding))
    for (stopped, code) <- List(CommentaryJobReducer.cancelJob(focus) -> "cancelled",
      CommentaryJobReducer.expireJob(focus, 10000L) -> "deadline_exceeded") do
      assertRootStopPreservesObservations(observations(focus), observations(stopped), focus.issuedWork.workId, code)

  test("registry reads and rejected late reports preserve the terminal packet until retention expires"):
    val registry = CommentaryJobRegistry(4, 1000L)
    val created = registry.createJob("delivery-preservation", initial, Nil, initial, "g1h3",
      history.extend(List("g1h3")).toOption.get.currentFen, CommentaryJobPolicy(16, 10000L, profile), 0L,
      supplied).toOption.get
    val root = created.asInstanceOf[CommentaryJobSnapshot].state.asInstanceOf[AwaitingEngineWork]
    val focused = registry.submitEngineWorkReportAt(created.jobId, report(root, rootMoves), 1L).toOption.get
    val stopped = registry.cancelJobAt(created.jobId, 2L).get
    val focus = focused.asInstanceOf[CommentaryJobSnapshot]
    val expected = PositionCommentaryJobProtocol.projectJobSnapshot(
      focus.copy(state = CommentaryJobReducer.cancelJob(focus.state)))
    assert(stopped.isInstanceOf[RetainedCommentaryJobResult])
    assertEquals(PositionCommentaryJobProtocol.projectJobSnapshot(stopped), expected)
    assertRootStopPreservesObservations(observations(focus.state), (expected \ "observations").as[JsObject],
      focus.state.asInstanceOf[AwaitingEngineWork].issuedWork.workId, "cancelled")
    assertEquals(registry.submitEngineWorkReportAt(created.jobId, report(root, rootMoves), 3L),
      Left(EngineWorkReportSubmissionRejection.WorkNotOutstanding))
    assertEquals(registry.expireAndReadJobAt(created.jobId, 1001L).get, stopped)
    assertEquals(registry.expireAndReadJobAt(created.jobId, 1002L), None)

  test("a completed registry result preserves every public field and never extends its retention on read or cancel"):
    val registry = CommentaryJobRegistry(1, 1000L)
    val created = registry.createJob("completed-retention", initial, Nil, initial, "g1h3",
      history.extend(List("g1h3")).toOption.get.currentFen, CommentaryJobPolicy(16, 10000L, profile), 0L,
      supplied).toOption.get.asInstanceOf[CommentaryJobSnapshot]
    val root = created.state.asInstanceOf[AwaitingEngineWork]
    val focused = registry.submitEngineWorkReportAt(created.jobId, report(root, rootMoves), 1L)
      .toOption.get.asInstanceOf[CommentaryJobSnapshot]
    val focus = focused.state.asInstanceOf[AwaitingEngineWork]
    val receipt = report(focus, List(List("e2e4", "e7e5", "g1f3"), List("g1h3", "d7d5", "h3g5")))
    val expectedState = CommentaryJobReducer.submitIssuedWorkReport(focus, receipt, 2L).toOption.get
    assert(expectedState.isInstanceOf[CompletedCommentaryJob])
    val expected = PositionCommentaryJobProtocol.projectJobSnapshot(focused.copy(state = expectedState))
    val completed = registry.submitEngineWorkReportAt(created.jobId, receipt, 2L).toOption.get
    assert(completed.isInstanceOf[RetainedCommentaryJobResult])
    assertEquals(PositionCommentaryJobProtocol.projectJobSnapshot(completed), expected)
    assertEquals(registry.expireAndReadJobAt(created.jobId, 999L).get, completed)
    assertEquals(registry.cancelJobAt(created.jobId, 1001L).get, completed)
    assertEquals(registry.submitEngineWorkReportAt(created.jobId, receipt, 1001L),
      Left(EngineWorkReportSubmissionRejection.WorkNotOutstanding))
    assertEquals(registry.expireAndReadJobAt(created.jobId, 1002L), None)

  test("explicit release frees active and terminal jobs without retaining a stopped projection"):
    val registry = CommentaryJobRegistry(1, 1000L)
    def create() = registry.createJob("release", initial, Nil, initial, "g1h3",
      history.extend(List("g1h3")).toOption.get.currentFen, CommentaryJobPolicy(16, 10000L, profile), 0L,
      supplied).toOption.get
    val active = create().asInstanceOf[CommentaryJobSnapshot]
    assert(registry.releaseJob(active.jobId))
    assertEquals(registry.expireAndReadJobAt(active.jobId, 1L), None)
    assertEquals(registry.submitEngineWorkReportAt(active.jobId,
      report(active.state.asInstanceOf[AwaitingEngineWork], rootMoves), 1L),
      Left(EngineWorkReportSubmissionRejection.JobNotFound))
    assert(!registry.releaseJob(active.jobId))
    val next = create()
    registry.cancelJobAt(next.jobId, 1L)
    assert(registry.releaseJob(next.jobId))
    assertEquals(registry.expireAndReadJobAt(next.jobId, 2L), None)
    assert(create().jobId != next.jobId)

  test("retained delivery reproduces exact UTF-8 bytes without keeping mutable delivery buffers"):
    val expected = "{\"answer\":\"미확인: 준비수의 보상 ♞\",\"reason\":\"investigation_limit\"}"
      .getBytes(java.nio.charset.StandardCharsets.UTF_8)
    val source = expected.clone()
    val retained = RetainedCommentaryJobResult("compressed-result", source)
    source(0) = 0
    val first = retained.jsonBytes
    assertEquals(first.toList, expected.toList)
    first(0) = 0
    assertEquals(retained.jsonBytes.toList, expected.toList)

  test("a delayed expiry preserves its original deadline and an immediately stopped creation also freezes"):
    val registry = CommentaryJobRegistry(2, 1000L)
    def create(id: String, now: Long) = registry.createJob(id, initial, Nil, initial, "g1h3",
      history.extend(List("g1h3")).toOption.get.currentFen, CommentaryJobPolicy(16, 100L, profile), now,
      supplied).toOption.get
    val created = create("delayed-expiry", 0L).asInstanceOf[CommentaryJobSnapshot]
    val expired = registry.expireAndReadJobAt(created.jobId, 500L).get
    val expected = PositionCommentaryJobProtocol.projectJobSnapshot(
      created.copy(state = CommentaryJobReducer.expireJob(created.state, 500L)))
    assert(expired.isInstanceOf[RetainedCommentaryJobResult])
    assertEquals(PositionCommentaryJobProtocol.projectJobSnapshot(expired), expected)
    val immediate = create("already-expired", 500L)
    assert(immediate.isInstanceOf[RetainedCommentaryJobResult])
    assertEquals((PositionCommentaryJobProtocol.projectJobSnapshot(immediate) \ "stop_condition").as[String],
      "deadline_exceeded")
    assertEquals(registry.cancelJobAt(created.jobId, 1099L).get, expired)
    assertEquals(registry.expireAndReadJobAt(created.jobId, 1100L), None)
    assertEquals(registry.expireAndReadJobAt(immediate.jobId, 1100L), None)

  test("successful focus and one-ply scored lines both retain independent observations"):
    val root = start()
    val focus = submit(root, rootMoves).asInstanceOf[AwaitingEngineWork]
    val completed = submit(focus, List(List("e2e4", "e7e5", "g1f3"), List("g1h3", "d7d5", "h3g5")))
    assert(completed.isInstanceOf[CompletedCommentaryJob])
    assertPathsRetained(focus, completed)
    assert(completed.observationsPacket.get.assembly.input.evaluationObservations.size >= 5)
    val shortLines = submit(start("g1f3"), rootMoves.map(_.take(1)))
    assert(shortLines.isInstanceOf[CompletedCommentaryJob])
    // A valid score comparison projects a verdict even without a longer continuation.
    assert(shortLines.observationsPacket.get.primary.nonEmpty)
    assert((observations(shortLines) \ "statements").as[List[JsObject]].nonEmpty)
    assertPathsRetained(start("g1f3"), shortLines)

  test("preparation failure retains admitted lines and remains distinct from construction failure"):
    val root = start("g1f3", depth = 1)
    val stopped = submit(root, rootMoves).asInstanceOf[StoppedCommentaryJob]
    assertEquals(stopped.stopCondition, CommentaryJobStopCondition.MoveReviewPreparationFailed)
    assertPathsRetained(root, stopped)
    assert(rootMoves.forall(stopped.observationsPacket.get.assembly.input.legalLines.map(_.moves).contains))
    assertEquals(stopped.progress.physicalReportsAccepted, 1)

  test("retained review assembly failure returns no replacement and leaves its independent packet intact"):
    val retainedInput = MoveReviewInputAdmission.admitIndependent(history, "g1f3", suppliedLines = supplied).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(retainedInput)).get
    val before = RuntimeProtocol.observationsJson(packet)
    val differentFocus = MoveReviewInputAdmission.admitIndependent(history, "e2e4").get
    intercept[IllegalArgumentException](MoveReviewAssembler.assemble(differentFocus, Some(packet.assembly)))
    assertEquals(CommentaryJobReducer.assembleRetainedPacket(differentFocus, packet), None)
    assertEquals(RuntimeProtocol.observationsJson(packet), before)
    assert(CommentaryJobReducer.assembleRetainedPacket(retainedInput, packet).nonEmpty)

  test("upper assessment failure retains newly assembled branches and returns an explicit stop condition"):
    val newBranch = List("g1f3", "d7d5", "d2d4")
    val prepared = MoveReviewInputAdmission.admit(RawMoveReviewInput(initial, "g1f3", List(
      EngineLine(List("e2e4", "e7e5", "g1f3"), 50, None, 16),
      EngineLine(newBranch, 0, None, 16)))).get
    val previous = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(
      MoveReviewInputAdmission.admitIndependent(prepared.positionHistory, "g1f3").get)).get
    assert(!previous.assembly.input.legalLines.exists(_.moves == newBranch))
    val lower = MoveReviewAssembler.assembleOccurrences(prepared, Some(previous.assembly))
    val reference = lower.lines.find(_.role == LineNodeRole.BestReference).get
    // A malformed assessment endpoint claims the reference's line evidence.
    // Its relative assessment then has duplicate parents; legal occurrences are intact.
    val malformed = lower.copy(lines = lower.lines.map { candidate =>
      if candidate.role == LineNodeRole.Played then candidate.copy(lineEvidence = reference.lineEvidence)
      else candidate
    })
    val lowerPacket = EvidenceBackedJudgmentPacket.fromAssembly(malformed).get
    intercept[IllegalArgumentException](MoveReviewAssembler.withAssessment(malformed))
    val (preserved, failure) = CommentaryJobReducer.assessOccurrences(malformed).get
    assertEquals(failure, Some(CommentaryJobStopCondition.ReviewConstructionFailed))
    assert(preserved.primary.isEmpty)
    assert(preserved.assembly.input.legalLines.exists(_.moves == newBranch))
    assertEquals(RuntimeProtocol.observationsJson(preserved), RuntimeProtocol.observationsJson(lowerPacket))
    assertEquals(preserved.assembly.transitions, lower.transitions)
    assertEquals(previous.assembly.input.legalLines.exists(_.moves == newBranch), false)
    assertEquals(CommentaryJobReducer.assessOccurrences(lower).get._2, None)
