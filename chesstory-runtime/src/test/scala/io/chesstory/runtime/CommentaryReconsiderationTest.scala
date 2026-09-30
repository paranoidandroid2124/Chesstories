package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Invented reports test withdrawal and evidence ownership, not sacrifice discovery or engine strength. */
class CommentaryReconsiderationTest extends munit.FunSuite:
  import CommentaryProposal.*
  import CommentaryFact.{Comparison, Declared, Path, Piece, Ply, Predicate}

  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val source = Source("user", "reconsideration-test")
  private val origin = Origin(source, source, source, Source("none", "reconsideration-test"))
  private val board = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
  private val roots = List(List("d2d3") -> 50, List("f1d3") -> 40, List("e4d5") -> 30)
  private val pawn = Piece("white", "pawn", "d2")
  private val rootQuestion = Request("root-question", origin, List(
    CompareMoves("pair", Nil, "f1d3", "d2d3", Some("better")),
    Fact("returned", Path("returned", Comparison("pair", "alternative")),
      Predicate("location", Some(pawn)), Ply(1), expected = Some("d3")),
    Fact("declared", Path("declared", Declared(List("d2d3"))),
      Predicate("location", Some(pawn)), Ply(1), expected = Some("d3")),
    FactGroup("returned-group", "all", List("returned", "declared"))))
  private val childQuestion = Request("child-question", origin,
    List(CompareMoves("reply", List("d2d3"), "d5e4", "d8d7")))

  private def start(fen: String = board, actual: String = "d2d3",
      requests: List[Request] = List(rootQuestion, childQuestion), discover: Boolean = true,
      preview: Boolean = true): AwaitingEngineWork =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    CommentaryJobReducer.startJob(fen, Nil, fen, actual, history.extend(List(actual)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = discover,
        previewDepth = Option.when(preview)(8)),
      0L, proposals = requests).toOption.get.asInstanceOf[AwaitingEngineWork]

  private def submit(active: AwaitingEngineWork, rows: List[(List[String], Int)],
      depth: Option[Int] = None, mates: Set[List[String]] = Set.empty): CommentaryJobState =
    val work = active.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(active, EngineWorkReport.Completed(profile, work.workId,
      work.executionKeySha256, rows.map { (moves, cp) =>
        val score = if mates(moves) then ReportedWhiteEngineScore.Mate(cp) else ReportedWhiteEngineScore.Centipawns(cp)
        ReportedEngineLineSuffix(moves, score, depth.getOrElse(work.requiredDepth))
      }), 1L).toOption.get

  private def awaiting(state: CommentaryJobState): AwaitingEngineWork = state match
    case value: AwaitingEngineWork => value
    case _ =>
      val readings = state.proposalEntries.flatMap(entry => entry.readings.map(reading =>
        s"${entry.request.id}/${reading.requirementId}:${reading.state}:${reading.stopReason}"))
      fail(s"expected an outstanding owned work; ${state.getClass.getSimpleName}; " +
        s"verification=${state.verification}; readings=${readings.mkString(",")}")

  private def finish(state: CommentaryJobState): CompletedCommentaryJob = state match
    case value: CompletedCommentaryJob => value
    case active: AwaitingEngineWork => CommentaryJobReducer.cancelJob(active).asInstanceOf[CompletedCommentaryJob]
    case _ => fail("expected a retained assessment")

  private def reading(state: CommentaryJobState, id: String, entryId: String = "root-question"): Reading =
    state.proposalEntries.find(_.request.id == entryId).get.readings.find(_.requirementId == id).get

  private def review(state: CommentaryJobState): CompletedMoveReview =
    val completed = state match
      case value: CompletedCommentaryJob => value
      case _ => state.assessment.get
    completed.result match
      case CompletedPositionCommentary.FocusedMoveReview(_, value) => value
      case _ => fail("expected a move review")

  private def childWork(): AwaitingEngineWork =
    val verification = awaiting(submit(start(), roots))
    val child = awaiting(submit(verification, roots))
    assertEquals(child.issuedWork.searchAnchor.map(_.prefixMoves), Some(List("d2d3")))
    assertEquals(child.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List("d5e4", "d8d7")))
    assertEquals(reading(child, "pair").state, "supported")
    assertEquals(reading(child, "returned-group").state, "supported")
    child

  private def challenge(child: AwaitingEngineWork): AwaitingEngineWork =
    val pending = awaiting(submit(child, List(List("d5e4", "d3e4") -> -300, List("d8d7") -> 200)))
    assertEquals(pending.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(pending.issuedWork.requiredDepth, 17)
    assertEquals(pending.verification.map(_.reason), Some("child_challenge"))
    pending

  // Test only the existing selection adapter: the metadata does not claim automatic question discovery.
  private def comparisonCandidates(state: CommentaryJobState): List[JsObject] =
    val entry = state.proposalEntries.find(_.request.id == "root-question").get
    val arm = GeneratedArm("test-arm", Nil, "f1d3", Some("d2d3"),
      List("pair", "returned-group"), entry.request.requirements.map(_.id), source, Nil)
    val packet = state.observationsPacket.get
    CommentaryProposal.comparisonCandidates(packet, ObservedLineProtocol.prepare(packet),
      List(entry.copy(generatedArms = List(arm))))

  private def assertWithdrawn(state: CommentaryJobState): Unit =
    assertEquals(reading(state, "pair").answer, None)
    assert(!Set("answered", "supported", "refuted")(reading(state, "pair").state))
    List("returned", "returned-group").foreach { id =>
      val result = reading(state, id).result.get
      assertEquals(result.state, "partial")
      assertEquals(result.samples, Nil)
      assertEquals(result.proofs, Nil)
      assert(result.needs.exists(need => need.kind == "comparison" && need.requirementId.contains("pair")))
    }
    assertEquals(reading(state, "declared").state, "supported")
    assert(reading(state, "declared").result.get.samples.nonEmpty)
    assertEquals(comparisonCandidates(state), Nil)

  test("a child challenge withdraws a shallow answer, its returned facts and selected explanation immediately"):
    val child = childWork()
    val oldAnswer = reading(child, "pair").answer.get
    assert(comparisonCandidates(child).exists(candidate => (candidate \ "selection").as[String] == "selected"))
    val priorReview = review(child).judgmentPacket
    val pending = challenge(child)
    assertWithdrawn(pending)
    assertEquals(review(pending).judgmentPacket, priorReview)
    assertEquals(pending.assessmentNeedsRefresh, true)
    assert(pending.pvComparisons.forall(reading =>
      List(reading.answer.referenceDepth, reading.answer.alternativeDepth).flatten.forall(_ >= 17)))
    val packet = pending.observationsPacket.get
    val paths = ObservedLineProtocol.prepare(packet)
    assert(paths.path(packet, oldAnswer.referenceLineId).nonEmpty)
    assert(paths.path(packet, oldAnswer.alternativeLineId).nonEmpty)
    assert(pending.rootSearches.exists(_.work.workId == oldAnswer.workId))
    assert(pending.responseSearches.exists(_.work.workId == child.issuedWork.workId))

  test("a root recheck omitting actual keeps answers withdrawn through focus waiting, failure and expiry"):
    val pending = challenge(childWork())
    val focus = awaiting(submit(pending, List(List("f1d3") -> 40, List("e4d5") -> 30, List("f3g5") -> 20)))
    assertEquals(focus.issuedWork.purpose, EngineWorkPurpose.FocusComparison)
    assertEquals(focus.issuedWork.requiredDepth, 17)
    assertWithdrawn(focus)
    val work = focus.issuedWork
    val failed = CommentaryJobReducer.submitIssuedWorkReport(focus, EngineWorkReport.ExecutorFailed(profile,
      work.workId, work.executionKeySha256, EngineWorkFailure.ExecutionFailed), 2L).toOption.get
    List(failed, CommentaryJobReducer.expireJob(focus, 10000L), CommentaryJobReducer.cancelJob(focus)).foreach { stopped =>
      assertWithdrawn(stopped)
      assertEquals(stopped.assessmentNeedsRefresh, true)
      assertEquals(stopped.verification.map(_.state), Some("stopped"))
      assertEquals(review(stopped).judgmentPacket.assembly.input.assessmentCandidates
        .flatMap(_.evaluation.engineLine.map(_.depth)).toSet, Set(16))
    }

  test("a deeper result replaces returned fact ownership and does not revive an omitted automatic inquiry"):
    val child = childWork()
    val obsolete = child.inquiries.find(_.question.alternativeMove == "e4d5").get
    assert(obsolete.answer.nonEmpty)
    val oldAnswer = reading(child, "pair").answer.get
    val pending = challenge(child)
    assertEquals(pending.inquiries.find(_.question.id == obsolete.question.id).get.answer, None)
    val actual = List("d2d3", "d5e4", "d3e4")
    val done = finish(submit(pending, List(List("f1d3") -> 40, actual -> -300, List("f3g5") -> 20)))
    assertEquals(done.verification.map(_.state), Some("complete"))
    assertEquals(done.assessmentNeedsRefresh, false)
    val renewed = reading(done, "pair")
    assertEquals(renewed.state, "refuted")
    assertEquals(renewed.answer.map(_.workId), Some(pending.issuedWork.workId))
    assertEquals(renewed.answer.flatMap(_.alternativeDepth), Some(17))
    assertNotEquals(renewed.answer.get.alternativeLineId, oldAnswer.alternativeLineId)
    val returned = reading(done, "returned")
    assertEquals(returned.state, "supported")
    assert(returned.result.get.samples.forall(_.lineId == renewed.answer.get.alternativeLineId))
    assertEquals(reading(done, "returned-group").state, "supported")
    assert(comparisonCandidates(done).exists(candidate =>
      (candidate \ "detail" \ "evaluation_relation").as[String] == "worse"))
    val retired = done.inquiries.find(_.question.id == obsolete.question.id).get
    assertEquals(retired.answer, None)
    assertNotEquals(retired.state, "answered")
    assertEquals(retired.assessmentEffect, None)
    assert(done.rootSearches.exists(_.work.workId == obsolete.answer.get.workId))

  test("complete terminal comparisons and their exact facts survive a higher root requirement"):
    // Only a move that immediately ends the game has an exact root outcome.
    val initial = "r6k/4Q3/6K1/8/8/8/8/8 w - - 0 1"
    val terminalQuestion = Request("terminal-question", origin, List(
      CompareMoves("terminal-pair", Nil, "e7g7", "e7h7", Some("equal")),
      Fact("mate-path", Path("terminal", Comparison("terminal-pair", "alternative")),
        Predicate("location", Some(Piece("white", "queen", "e7"))), Ply(1), expected = Some("h7"))))
    val reply = Request("terminal-trigger", origin,
      List(CompareMoves("reply", List("e7e4"), "a8a7", "a8a6")))
    val terminalRows = List(List("e7e4") -> 30, List("e7g7") -> 1, List("e7h7") -> 1)
    val mates = Set(List("e7g7"), List("e7h7"))
    val verify = awaiting(submit(start(initial, "e7e4", List(terminalQuestion, reply), discover = false),
      terminalRows, mates = mates))
    val child = awaiting(submit(verify, terminalRows, mates = mates))
    assertEquals(child.issuedWork.searchAnchor.map(_.prefixMoves), Some(List("e7e4")))
    val previous = reading(child, "terminal-pair", "terminal-question")
    assertEquals(previous.state, "supported")
    assertEquals(previous.answer.get.referenceDepth, None)
    assertEquals(previous.answer.get.alternativeDepth, None)
    val pending = awaiting(submit(child, List(List("a8a7") -> -300, List("a8a6") -> 20)))
    assertEquals(pending.issuedWork.requiredDepth, 17)
    assertEquals(reading(pending, "terminal-pair", "terminal-question"), previous)
    assertEquals(reading(pending, "mate-path", "terminal-question").state, "supported")
    assertEquals(reading(finish(pending), "terminal-pair", "terminal-question"), previous)

  test("a restricted deeper refresh also retires legacy inquiry answers outside its new pool"):
    val initial = chess.variant.Standard.initialFen.value
    val rootRows = List(List("e2e4") -> 30, List("d2d4") -> 20, List("g1f3") -> 10)
    val probe = awaiting(submit(start(initial, "g1f3", Nil, preview = false), rootRows))
    assertEquals(probe.issuedWork.purpose, EngineWorkPurpose.InvestigationComparison)
    val old = probe.inquiries.find(_.question.alternativeMove == "d2d4").get
    assert(old.answer.nonEmpty)
    val verify = awaiting(submit(probe, List(List("e2e4") -> 30, List("g1h3") -> 80), Some(24)))
    assertEquals(verify.issuedWork.requiredDepth, 24)
    assertEquals(verify.inquiries.find(_.question.id == old.question.id).get.answer, None)
    val done = finish(submit(verify, List(List("e2e4") -> 20, List("g1f3") -> 0, List("g1h3") -> 80)))
    assertEquals(review(done).bestMoveUci, "g1h3")
    assertEquals(done.assessmentNeedsRefresh, false)
    val retired = done.inquiries.find(_.question.id == old.question.id).get
    assertEquals(retired.answer, None)
    assertNotEquals(retired.state, "answered")
    assertEquals(retired.assessmentEffect, None)
    assert(done.rootSearches.exists(search => search.work.workId == old.answer.get.workId && search.receipt.nonEmpty))
