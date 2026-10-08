package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path as NioPath}
import java.util.zip.GZIPInputStream
import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Four unchanged actual Current34 reports establish the current prefix.
  * No new engine report is supplied: the positive result is issuance of the
  * still-unanswered counter. Negative mutations below are ownership evidence,
  * never new evaluations, completed chess results, or the book-root cause.
  */
class ReserveSourceScopeTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import PrincipalVariationComparison.QualityBand

  private lazy val record =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/reserve-source-scope-h05-produced.json.gz"
    val path = List(NioPath.of(relative), NioPath.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()

  private lazy val request = PositionCommentaryJobProtocol.parseJobRequest(
    Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get

  private lazy val current: AwaitingEngineWork =
    var state: CommentaryJobState = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 180001L, request.engineProfile,
        investigateChoices = true, previewDepth = Some(8)), 1L,
      request.inputLines, request.proposals).toOption.get
    val reports = (record \ "saved_reports").as[List[JsObject]]
    assertEquals(reports.size, 4)
    reports.zipWithIndex.foreach { (saved, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"no issued owner for actual report $index")
      val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("s" * 32, request.requestId, 180001L, state))
      assertEquals((wire \ "issued_engine_work").as[JsObject], (saved \ "issued_work").as[JsObject],
        "the complete actual work object must match before each unchanged report is admitted")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    state match
      case active: AwaitingEngineWork => active
      case other => fail(s"the owned entry counter remains to be issued, got ${other.getClass.getSimpleName}")

  private val wait = ("f3e3 f7f6 e3f3 e5d6 f3f2 d6c6 f2e3 c6c7 e3d2 " +
    "b4c3 b2c3 c7c6 d2e3 c6d6 e3f4 d6e6 g3g4").split(" ").toList

  private lazy val proof = CommentaryIdea.reserveEntries(current.proposalEntries).find(value =>
    value.test.prefixMovesUci == wait && value.test.referenceMove == "e6d7" &&
      value.test.alternativeMove == "f6f5" && value.square == "f5").getOrElse(
    fail("the supported current wait must retain its exact unanswered guarding-pawn counter"))

  private lazy val packet = current.observationsPacket.get
  private lazy val paths = current.preparedPaths.get
  private lazy val source = proof.source.get
  private lazy val sourcePath = paths.paths.find(_.line.ref.id == source.answer.alternativeLineId).get
  private lazy val entry = current.proposalEntries.find(_.request.id == proof.owner.proposalId).get
  private lazy val witness = paths.paths.find(path =>
    entry.request.origin.evidence.sourceRefs.contains(path.line.ref.id)).get
  private lazy val actors = List(proof.pawn.witness, proof.king.witness)
  private lazy val pair = List(proof.owner.answer.referenceLineId, proof.owner.answer.alternativeLineId)
    .map(id => paths.paths.find(_.line.ref.id == id).get)

  test("the changed actual descendant issues its own counter without borrowing a future value"):
    assertEquals(current.issuedWork.workId, "work:4")
    assertEquals(current.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(current.issuedWork.searchAnchor.map(_.prefixMoves), Some(wait))
    current.issuedWork.rootRestriction match
      case EngineRootRestriction.Restricted(moves) => assertEquals(moves.toSet, Set("e6d7", "f6f5"))
      case other => fail(s"expected the exact king/pawn counter pair, got $other")
    assertEquals(current.issuedWork.searchAnchor.map(_.positionId), sourcePath.positions(17).id)
    assert(current.issuedWork.proposalRefs.contains(entry.request.id -> "reserve_counter"))

    assertEquals(proof.owner.answer.workId, "work:1")
    assertEquals(source.answer.workId, "work:2")
    assertEquals(source.comparison.id, "reserve_source")
    assertEquals(source.comparison.prefixMovesUci, List("f3e3", "f7f6"))
    assertEquals(source.answer.evaluationRelation, "better")
    assertEquals(source.answer.qualityBand, Some(QualityBand.Separated))
    assertEquals(proof.sourceArm, "alternative")
    assert(CommentaryIdea.comparableReserveArm(source.answer, proof.sourceArm))
    assert(entry.request.origin.evidence.sourceRefs.contains("work:3"))
    assertEquals(sourcePath.moves.take(17), witness.moves.take(17))
    assertEquals(sourcePath.ids.take(17), witness.ids.take(17))
    assertEquals(sourcePath.positions.take(18), witness.positions.take(18))
    assert(RootSubjectComparison.currentReserveEntry(paths, proof))
    assert(entry.readings.find(_.requirementId == "reserve_current").exists(_.state == "supported"))
    assert(entry.readings.find(_.requirementId == "reserve_context").exists(_.state == "supported"))
    assertEquals(proof.testAnswer, None)
    assertEquals(proof.optionAnswer, None)
    assert(!proof.verified)
    assert(!current.responseSearches.exists(_.anchor.prefixMoves == wait),
      "a pending work supplies neither a completed counter search nor an entry-value answer")

    val work3 = CommentaryIdea.reserveHolds(current.proposalEntries).flatMap(_.testAnswer).find(answer =>
      answer.workId == "work:3" &&
        (answer.referenceEvaluation \ "white_score" \ "value").asOpt[Int].contains(887) &&
        (answer.alternativeEvaluation \ "white_score" \ "value").asOpt[Int].contains(603)).get
    assertEquals(work3.evaluationRelation, "worse")
    assertEquals(work3.qualityBand, Some(QualityBand.Separated))
    assert(!CommentaryIdea.comparableReserveArm(work3, "alternative"))

    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("s" * 32, request.requestId, 180001L, current))
    val played = (wire \ "assessment" \ "selected_move_reviews").as[List[JsObject]].find(value =>
      (value \ "move_uci").as[String] == "g3g4").get
    val verdict = (played \ "commentary" \ "primary").as[JsObject]
    assertEquals((verdict \ "verdict_code").as[String], "mistake")
    assertEquals((verdict \ "delta" \ "candidate_win_percent_delta_for_mover").as[Double],
      -11.830322457665872)

  test("an earlier certified prefix owns the wait despite a different later forecast"):
    assertEquals(sourcePath.moves.size, 18)
    assert(witness.moves.size > sourcePath.moves.size)
    assert(!sourcePath.moves.startsWith(proof.branch))
    assert(CommentaryDiscovery.sameReserveDecision(paths, sourcePath, witness, 17, actors))
    assert(!CommentaryDiscovery.sameReserveDecision(paths, sourcePath, witness, 19, actors),
      "the shorter source cannot own the later entry occurrence")
    val rootForecast = paths.paths.find(_.line.ref.id == proof.owner.answer.alternativeLineId).get
    assertEquals(rootForecast.moves.last, "b4b5")
    assert(!CommentaryDiscovery.sameReserveDecision(paths, rootForecast, witness, 17, actors),
      "the original root's changed intermediate history cannot own this descendant")

  test("removed original actors and another canonical packet cannot authorize the current prefix"):
    val removedOriginal = Piece("black", "pawn", "b4").witness
    assertEquals(paths.trackedPieceAfter(sourcePath, removedOriginal, 17).flatten, None)
    assert(!CommentaryDiscovery.sameReserveDecision(paths, sourcePath, witness, 17, actors :+ removedOriginal))
    val foreignFen = request.currentFen.replace("0 35", "1 35")
    assertNotEquals(foreignFen, request.currentFen)
    val history = CanonicalPositionHistory.from(foreignFen, Nil, foreignFen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, request.playedMoveUci,
      suppliedLines = List(sourcePath.moves, witness.moves).map(
        SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val foreignPacket = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val foreignPaths = ObservedLineProtocol.prepare(foreignPacket)
    val foreignSource = foreignPaths.certifiedPath(foreignPacket, sourcePath.moves).get
    val foreignWitness = foreignPaths.certifiedPath(foreignPacket, witness.moves).get
    assert(CommentaryDiscovery.sameReserveDecision(foreignPaths, foreignSource, foreignWitness, 17, actors))
    assertEquals(foreignSource.moves.take(17), witness.moves.take(17))
    assert(!CommentaryDiscovery.sameReserveDecision(paths, foreignSource, witness, 17, actors),
      "same legal UCI moves in another root history do not confer ownership in this packet")

  test("a stale source realization or source occurrence cannot retain the new wait"):
    val rootLine = proof.owner.answer.alternativeLineId
    val oldForecast = source.copy(answer = source.answer.copy(alternativeLineId = rootLine))
    assert(!RootSubjectComparison.currentReserveEntry(paths, proof.copy(source = Some(oldForecast))))
    val wrongOccurrence = source.copy(answer = source.answer.copy(
      alternativeOccurrenceId = proof.owner.answer.alternativeOccurrenceId))
    assert(!RootSubjectComparison.currentReserveEntry(paths, proof.copy(source = Some(wrongOccurrence))))

  test("an older branch's completed value and future pawn loss cannot answer the new counter"):
    val olderSpent = paths.paths.find(_.line.ref.id == source.answer.referenceLineId).get
    assert(paths.continuationReadings.find(_.path eq olderSpent).exists(_.events.exists(event =>
      event.originalVictim.contains(proof.pawn.witness))), "the older immediate-g4 branch really contains the pawn price")
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    assertEquals(RootSubjectComparison.withReserveEntry(packet, paths, pair, List(proof), previous), previous)
    // Explicitly corrupted ownership: a complete early Kf3/g4 answer is not
    // a late Black Kd7/f5 receipt, even when its path contains a pawn price.
    val borrowed = proof.copy(testAnswer = Some(source.answer))
    assertEquals(RootSubjectComparison.withReserveEntry(packet, paths, pair, List(borrowed), previous), previous)
