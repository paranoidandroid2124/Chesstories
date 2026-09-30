package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory
import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*

/** Invented search reports isolate backend discovery, not engine strength.
  * The first case supplies no recovery line; the second explicitly supplies one
  * and tests extraction of its unanswered premise rather than donor discovery.
  */
class ContinuationDiscoveryTest extends munit.FunSuite:
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private def history =
    val initial = chess.variant.Standard.initialFen.value
    CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(List(
      "e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")).toOption.get

  private def start(supplied: List[List[String]] = Nil): AwaitingEngineWork =
    val h = history
    CommentaryJobReducer.startJob(h.initialFen, h.movePrefixUci, h.currentFen, "f3e5",
      h.extend(List("f3e5")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true), 0L,
      supplied.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).toOption.get.asInstanceOf[AwaitingEngineWork]

  private def submit(state: AwaitingEngineWork, rows: List[(List[String], Int)]): CommentaryJobState =
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile,
      state.issuedWork.workId, state.issuedWork.executionKeySha256,
      rows.map((moves, cp) => ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16))), 1L)
      .toOption.get

  test("Danish queen capture retains the checking reply question and the cost of a passive block"):
    val initial = chess.variant.Standard.initialFen.value
    val h = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(List(
      "e2e4", "e7e5", "d2d4", "e5d4", "c2c3", "d4c3", "f1c4", "c3b2",
      "c1b2", "d7d5", "c4d5", "g8f6")).toOption.get
    val queen = List("d5f7", "e8f7", "d1d8")
    val check = queen :+ "f8b4"
    val returned = check ++ List("d8d2", "b4d2", "b1d2")
    val passive = check ++ List("b1c3", "h8d8")
    assert(h.extend(returned).isRight)
    assert(h.extend(passive).isRight)
    assert(h.extend(check).toOption.get.currentLegalMoveUcis.contains("d8d2"))
    assert(h.extend(check).toOption.get.currentLegalMoveUcis.contains("b1c3"))
    val begun = CommentaryJobReducer.startJob(h.initialFen, h.movePrefixUci, h.currentFen, queen.head,
      h.extend(List(queen.head)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true), 0L).toOption.get
      .asInstanceOf[AwaitingEngineWork]
    // The root report supplies Qxd8, not the checking recovery or its answer.
    val choice = submit(begun, List(queen -> 0, List("b1c3") -> -10, List("g1f3") -> -20))
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(choice.issuedWork.searchAnchor.get.prefixMoves, queen.take(1))
    val reply = submit(choice, List(queen.tail -> 0, List("e8e7") -> 10))
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(reply.issuedWork.searchAnchor.get.prefixMoves, queen)
    assertEquals(reply.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assert(reply.continuationInquiries.exists(inquiry => inquiry.question.prefixMoves == queen &&
      inquiry.question.kinds.contains("material_followup") && inquiry.question.legalMoves.contains("f8b4")))
    // Supply the independently proposed checking reply. The next White choice
    // must remain a real question even when this request's two slots are spent.
    val done = submit(reply, List(List("f8b4") -> 0, List("f8e7") -> 20, List("h8g8") -> 100))
      .asInstanceOf[CompletedCommentaryJob]
    val blocked = done.continuationInquiries.find(_.question.prefixMoves == check).get
    assert(blocked.question.kinds.contains("capturer_reply"))
    assert(Set("d8d2", "b1c3").subsetOf(blocked.question.legalMoves.toSet))
    assertEquals(blocked.state, "deferred")
    assertEquals(blocked.stopReason, Some("investigation_limit"))
    // Exact supplied continuations establish the queen's different physical
    // takers and the later bishop recovery; they do not prove forced best play.
    val input = MoveReviewInputAdmission.admitIndependent(h, queen.head,
      suppliedLines = List(returned, passive).map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    assertEquals(input.suppliedLines.map(_.acceptedMoves), List(returned, passive))
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val readings = ObservedLineProtocol.prepare(packet).continuationReadings
    val givenBack = readings.find(_.path.moves == returned).get
    val abandoned = readings.find(_.path.moves == passive).get
    def queenLoss(value: LineContinuationInvestigation.Reading) = value.events.find(event =>
      event.originalVictim.exists(piece => piece.side.white && piece.role.name == "queen")).get
    val bishopTakes = queenLoss(givenBack)
    val rookTakes = queenLoss(abandoned)
    assertEquals(bishopTakes.originalActor.map(_.square.key), Some("f8"))
    assertEquals(rookTakes.originalActor.map(_.square.key), Some("h8"))
    assert(givenBack.captureLinks.exists(_.lossEventId == bishopTakes.id))
    assert(!abandoned.captureLinks.exists(_.lossEventId == rookTakes.id))
    assert(abandoned.openLossEventIds.nonEmpty)

  test("a bare pawn capture discovers a later legal gain from short responses without a donor recovery"):
    val reply = submit(start(), List(List("f3e5") -> 50, List("e1g1") -> 40, List("d2d4") -> 30))
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(reply.issuedWork.searchAnchor.get.prefixMoves, List("f3e5"))
    val next = submit(reply, List(List("d8d4", "e5f3") -> -20,
      List("d8e7", "b1c3") -> 0, List("f8d6") -> 10)).asInstanceOf[AwaitingEngineWork]
    val prefix = List("f3e5", "d8d4", "e5f3")
    val discovered = next.continuationInquiries.find(_.question.prefixMoves == prefix).get
    assert(discovered.question.kinds.contains("material_followup"))
    assert(discovered.question.comparisonSources.isEmpty)
    assert(discovered.question.legalMoves.contains("d4e4"))
    assert(discovered.question.legalMoves.contains("d4d6")) // Quiet replies compete with the legal gain.
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, prefix)
    assertEquals(next.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assert(next.continuationReadings.forall(_.otherTargetGains.isEmpty))
    val threatened = next.continuationInquiries.find(_.question.prefixMoves == List("f3e5", "f8d6")).get
    assert(threatened.question.kinds.contains("capturer_reply"))
    assert(threatened.question.legalMoves.contains("e5f3"))
    assert(threatened.question.legalMoves.contains("b1c3"))

    val done = submit(next, List(List("d4e4", "d1e2", "e4e2", "e1e2") -> 0,
      List("d4d6") -> 40, List("d4d8") -> 50)).asInstanceOf[CompletedCommentaryJob]
    val observed = done.continuationReadings.find(_.path.moves == prefix ++
      List("d4e4", "d1e2", "e4e2", "e1e2")).get
    assert(observed.otherTargetGains.exists { gain =>
      observed.events.find(_.id == gain.gainEventId).flatMap(_.originalVictim).exists(_.square.key == "e4")
    })
    assertEquals(observed.events.head.actorEnd.state, "present")
    assertEquals(done.progress.physicalWorksIssued, 3)
    val packet = done.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val known = LineContinuationInvestigation.observedMaterialContinuations(packet, prefix,
      discovered.question.triggerOccurrenceIds, prepared).get
    assert(known.contains(observed.path.line.ref.id))
    val oldShortSearch = done.responseSearches.find(_.anchor.prefixMoves == List("f3e5")).get
    assert(!LineContinuationInvestigation.unobservedContinuations(packet, oldShortSearch, prepared)
      .exists { (_, point) => point.prefixMovesUci == prefix && point.kinds.contains("material_followup") })

    // An observed recovery does not settle the parent question when the
    // recovering queen is lost later in the very same returned continuation.
    val badSuffix = List("d4e4", "e1f1", "e4e2", "f1e2")
    val relost = submit(next, List(badSuffix -> 900, List("d4d6") -> 40, List("d4d8") -> 50))
      .asInstanceOf[CompletedCommentaryJob]
    val bad = relost.continuationReadings.find(_.path.moves == prefix ++ badSuffix).get
    assert(bad.recoveryLosses.exists(link => link.lossEventId == bad.events.head.id &&
      bad.events.find(_.id == link.laterLossEventId).exists(_.originalVictim.exists(_.role.name == "queen"))))
    assert(bad.openLossEventIds.nonEmpty)
    val stillOpen = relost.continuationInquiries.find(_.question.prefixMoves == prefix).get
    assertEquals(stillOpen.state, "surveyed")
    assert(stillOpen.unresolvedSourceLineIds.contains(bad.path.line.ref.id))
    val parent = relost.continuationInquiries.find(_.question.prefixMoves == List("f3e5")).get
    assert(parent.unresolvedSourceLineIds.contains(bad.path.line.ref.id))
    assert(parent.followupQuestionIds.contains(stillOpen.question.id))
    assertEquals(relost.progress.physicalWorksIssued, 3)

  test("a checking counterreply can open a material inquiry when no capture is currently legal"):
    // Constructed Danish sibling with Pe4 absent. It isolates the accidental
    // dependence on unrelated ...Nxe4; it is not a second published game.
    val fen = "rnbqkb1r/ppp2ppp/5n2/3B4/8/8/PB3PPP/RN1QK1NR w KQkq - 0 7"
    val h = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val queen = List("d5f7", "e8f7", "d1d8")
    assert(h.extend(queen :+ "f8b4").toOption.get.currentPosition.check.yes)
    val started = CommentaryJobReducer.startJob(h.initialFen, Nil, h.currentFen, queen.head,
      h.extend(List(queen.head)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true), 0L).toOption.get
      .asInstanceOf[AwaitingEngineWork]
    val first = submit(started, List(queen -> 0, List("b1c3") -> -10, List("g1f3") -> -20))
      .asInstanceOf[AwaitingEngineWork]
    val next = submit(first, List(queen.tail -> 0, List("e8e7") -> 10))
      .asInstanceOf[AwaitingEngineWork]
    val reading = next.continuationReadings.find(_.path.moves == queen).get
    val view = next.preparedPaths.get.resourceViews(next.observationsPacket.get)(reading.path.positions.last.id.get)
    assert(view.legalRows.get.forall(_.capture.isEmpty))
    assert(next.continuationInquiries.exists(inquiry => inquiry.question.prefixMoves == queen &&
      inquiry.question.kinds.contains("material_followup") && inquiry.question.legalMoves.contains("f8b4")))

  test("the other-target gain itself discovers the preceding choice without a donor capturer removal"):
    val donor = List("f3e5", "d8d4", "e5f3", "d4e4")
    def afterReply(supplied: List[List[String]]): AwaitingEngineWork =
      val reply = submit(start(supplied), List(List("f3e5") -> 50,
        List("e1g1") -> 40, List("d2d4") -> 30)).asInstanceOf[AwaitingEngineWork]
      submit(reply, List(donor.tail -> -20, List("d8e7") -> 0, List("f8d6") -> 10))
        .asInstanceOf[AwaitingEngineWork]
    def checkUnobservedReply(): Unit =
      val checked = afterReply(List(donor))
      // The gain still discovers its earlier choice, but an unanswered actual
      // check on the current path precedes reconsidering that observed gain.
      assertEquals(checked.issuedWork.searchAnchor.get.prefixMoves, donor)
      assert(checked.continuationInquiries.exists(q => q.question.prefixMoves == donor.take(2) &&
        q.question.kinds.contains("other_target_gain") && q.state == "deferred"))
    // These are independent jobs. Preserve their assertions without retaining every old packet together.
    checkUnobservedReply()
    // An already certified reply removes that observation gap. It must not
    // erase the earlier choice or require another search for the known check.
    val observed = donor ++ List("d1e2", "e4e2", "e1e2")
    val next = afterReply(List(donor, observed))
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, donor.take(2))
    assert(next.continuationInquiries.exists(q => q.question.prefixMoves == donor.take(2) &&
      q.question.kinds.contains("other_target_gain")))
    val done = submit(next, List(List("e5f3", "d4e4") -> 0, List("b1c3") -> -100,
      List("c2c3") -> -90)).asInstanceOf[CompletedCommentaryJob]
    val short = done.continuationInquiries.find(_.question.prefixMoves == (donor.take(2) :+ "b1c3")).get
    assert(short.question.kinds.contains("continuation_not_observed"))
    assert(short.question.kinds.contains("material_followup"))
    assert(short.question.legalMoves.contains("d4e5"))
    assert(short.question.comparisonSources.nonEmpty)
    assertEquals(short.state, "deferred")

  test("vacating a blocker cannot hide a later queen loss outside the root capture chain"):
    // Independently noticed in a deliberately weak supplied support-use example.
    // These legal lines provide evidence; no engine quality is asserted.
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val h = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val pawn = List("d2d3", "d5e4", "d3e4", "d8d1")
    val bishop = List("f1d3", "d5e4", "d3e4")
    val input = MoveReviewInputAdmission.admitIndependent(h, pawn.head,
      suppliedLines = List(pawn, bishop).map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    assertEquals(input.suppliedLines.map(_.acceptedMoves), List(pawn, bishop))
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    val exposed = prepared.continuationReadings.find(_.path.moves == pawn).get
    val blocked = prepared.continuationReadings.find(_.path.moves == bishop).get
    def legalAt(reading: LineContinuationInvestigation.Reading) =
      prepared.resourceViews(packet)(reading.path.positions(3).id.get).legalRows.get.map(_.move)
    assert(legalAt(exposed).contains("d8d1"))
    assert(!legalAt(blocked).contains("d8d1"))
    val lostQueen = exposed.events.find(_.originalVictim.exists(_.role.name == "queen")).get
    assert(exposed.openLossEventIds(lostQueen.id))
    assertEquals(blocked.openLossEventIds, Set.empty[String])
    val opportunities = LineContinuationInvestigation.readExposures(packet, exposed.path, prepared)
    assertEquals(opportunities.map(_.prefixMoves), List(pawn.take(3)))
    assertEquals(opportunities.head.captures.map(_.move), List("d8d1"))
    assertEquals(opportunities.head.originalTarget.square.key, "d1")
    assert(exposed.choicePoints.exists(point => point.prefixMovesUci == pawn.take(3) &&
      point.kinds.contains("capture_exposure_response")))
    assert(exposed.choicePoints.exists(point => point.prefixMovesUci == pawn.take(2) &&
      point.kinds.contains("exposure_prevention_choice")))
    assertEquals(LineContinuationInvestigation.unresolvedExposures(packet, pawn.take(3),
      List(exposed.path.ids(2)), List(exposed.path.line.ref.id), List(exposed.path), prepared),
      List(exposed.path.line.ref.id))
    assertEquals(opportunities.head.blockers.map((original, current) => original.square.key -> current.square.key),
      List("d2" -> "d3"))
    val pawnExposure = LineContinuationInvestigation.readExposures(packet, blocked.path, prepared)
    assertEquals(pawnExposure.flatMap(_.captures.map(_.move)), List("d8d2"))
    assertEquals(pawnExposure.head.originalTarget.square.key, "d2")
    // Pd2 blocks access to the queen; it can itself be captured. The queen can
    // then take that rook, so a legal exposure is not a proven material loss.
    assert(h.extend(bishop :+ "d8d2").toOption.get.currentLegalMoveUcis.contains("d1d2"))

    def provided(board: String, moves: List[String]) =
      val initial = CanonicalPositionHistory.from(board, Nil, board).toOption.get
      val admitted = MoveReviewInputAdmission.admitIndependent(initial, moves.head,
        suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, moves))).get
      assertEquals(admitted.suppliedLines.head.acceptedMoves, moves)
      val next = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(admitted)).get
      val paths = ObservedLineProtocol.prepare(next)
      val path = paths.certifiedPath(next, moves).get
      (next, paths, path)
    val (declined, declinedPaths, declinedPath) = provided(fen, pawn.take(3) :+ "d8d3")
    assertEquals(LineContinuationInvestigation.readExposures(declined, declinedPath, declinedPaths)
      .flatMap(_.captures.map(_.move)), List("d8d1")) // The author's weak reply cannot hide the legal resource.
    assert(!declinedPaths.continuationReadings.find(_.path eq declinedPath).get.choicePoints
      .exists(_.kinds.contains("exposure_prevention_choice")))

    val pinnedFen = "R2r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val (pinned, pinnedPaths, pinnedPath) = provided(pinnedFen, pawn.take(3))
    val end = pinnedPaths.resourceViews(pinned)(pinnedPath.positions.last.id.get)
    val identities = pinnedPaths.identitiesAfter(pinned, pinnedPath, 3)
    val rook = identities.find(_._1.square.key == "d8").get._2.flatten.get
    assert(end.controlSources(rook, EvidenceSquare("d1")).nonEmpty)
    assert(!end.legalRows.get.exists(_.move == "d8d1"))
    assertEquals(LineContinuationInvestigation.readExposures(pinned, pinnedPath, pinnedPaths), Nil)

  test("an en passant departure can expose another piece through both vacated blockers"):
    val fen = "k7/4p3/8/r2P3Q/8/8/8/7K b - - 0 1"
    val h = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get.extend(List("e7e5")).toOption.get
    val line = List("d5e6")
    val input = MoveReviewInputAdmission.admitIndependent(h, line.head,
      suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, line))).get
    assertEquals(input.suppliedLines.head.acceptedMoves, line)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    val exposed = LineContinuationInvestigation.readExposures(packet, prepared.certifiedPath(packet, line).get, prepared)
    assertEquals(exposed.size, 1)
    assertEquals(exposed.head.blockers.map(_._2.square.key), List("d5", "e5"))
    assertEquals(exposed.head.originalTarget.square.key, "h5")
    assertEquals(exposed.head.captures.map(_.move), List("a5h5"))

  test("a newly opened legal capture requests replies before reconsidering the preceding recapture"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val h = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val opening = List("d2d3", "d5e4", "d3e4")
    val initial = CommentaryJobReducer.startJob(h.initialFen, h.movePrefixUci, h.currentFen, opening.head,
      h.extend(List(opening.head)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true), 0L).toOption.get
      .asInstanceOf[AwaitingEngineWork]
    // Synthetic root results supply a path, but no exposure question or Rxd1 line.
    val response = submit(initial, List((opening :+ "d8d3") -> 100,
      List("f1d3") -> 90, List("d1e2") -> 80)).asInstanceOf[AwaitingEngineWork]
    assertEquals(response.issuedWork.searchAnchor.get.prefixMoves, opening)
    val question = response.continuationInquiries.find(_.question.prefixMoves == opening).get.question
    assert(question.kinds.contains("capture_exposure_response"))
    assert(question.legalMoves.contains("d8d1"))
    assert(question.legalMoves.contains("d8d7"))
    assert(!response.continuationInquiries.exists(_.question.kinds.contains("exposure_prevention_choice")))

    val prevention = submit(response, List(List("d8d1") -> -900,
      List("d8d7") -> 0, List("d8d6") -> 10)).asInstanceOf[AwaitingEngineWork]
    assertEquals(prevention.issuedWork.searchAnchor.get.prefixMoves, opening.take(2))
    assertEquals(prevention.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    val actualLoss = prevention.continuationReadings.find(_.path.moves == (opening :+ "d8d1")).get
    assert(prevention.continuationInquiries.find(_.question.id == question.id).get
      .unresolvedSourceLineIds.contains(actualLoss.path.line.ref.id))
    val done = submit(prevention, List(List("d1e2") -> 20,
      List("f3g5", "e4d3", "f1d3") -> 0, List("d3e4", "d8d1") -> -900))
      .asInstanceOf[CompletedCommentaryJob]
    assert(done.continuationInquiries.exists(inquiry => inquiry.question.prefixMoves == opening.take(2) &&
      inquiry.question.kinds.contains("exposure_prevention_choice") && inquiry.state == "surveyed"))
    assertEquals(done.progress.physicalWorksIssued, 3)

  test("one quiet move opening the queen's file discovers a reply without a material event or supplied continuation"):
    val fen = "3r2k1/8/8/8/8/8/3N4/3Q3K w - - 0 1"
    val h = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val initial = CommentaryJobReducer.startJob(h.initialFen, h.movePrefixUci, h.currentFen, "d2c4",
      h.extend(List("d2c4")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true), 0L).toOption.get
      .asInstanceOf[AwaitingEngineWork]
    val next = submit(initial, List(List("d2c4") -> 0, List("d2f3") -> -10, List("d2b3") -> -20))
      .asInstanceOf[AwaitingEngineWork]
    val observed = next.continuationReadings.find(_.path.moves == List("d2c4")).get
    assertEquals(observed.events, Nil)
    assertEquals(observed.captureExposures.flatMap(_.captures.map(_.move)), List("d8d1"))
    assertEquals(next.issuedWork.searchAnchor.get.prefixMoves, List("d2c4"))
    assertEquals(next.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
