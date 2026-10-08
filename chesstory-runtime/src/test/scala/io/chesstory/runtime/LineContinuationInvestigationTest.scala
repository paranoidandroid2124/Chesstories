package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Supplied legal variations test the reader, never engine discovery or strength. */
class LineContinuationInvestigationTest extends munit.FunSuite:
  private val initial = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  private def packet(fen: String, historyMoves: List[String], line: List[String], others: List[List[String]] = Nil) =
    val start = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val history = start.extend(historyMoves).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, line.head,
      suppliedLines = (line :: others).map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    assertEquals(input.suppliedLines.map(_.acceptedMoves), line :: others)
    EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get

  private def read(source: EvidenceBackedJudgmentPacket, line: List[String]) =
    val prepared = ObservedLineProtocol.prepare(source)
    val reading = prepared.continuationReadings.find(_.path.moves == line).get
    (prepared, reading)

  private def pending(source: EvidenceBackedJudgmentPacket, prepared: ObservedLineProtocol.PreparedPaths,
      path: ObservedLineProtocol.PreparedPath, point: LineContinuationInvestigation.ChoicePoint,
      searches: List[TargetResponseInvestigation.Search] = Nil): CommentaryContinuationInquiry =
    val count = point.prefixMovesUci.size
    val rows = prepared.resourceViews(source)(point.positionId).legalRows.get
    val question = CommentaryContinuationQuestion(s"condition_${path.line.ref.id}_$count", path.ids.head,
      path.positions.head.id.get, point.positionId, point.prefixMovesUci, path.ids.take(count),
      path.moves.lift(count).toList, List(path.line.ref.id), rows.map(_.move), rows.map(row => row.move -> row.sourceId).toMap,
      point.premises.map(premise => CommentaryContinuationPremise(premise, List(path.line.ref.id))))
    CommentaryJobReducer.refreshContinuationInquiries(source, prepared, List(question), Nil, searches).head

  private def returned(fen: String, roots: List[(List[String], Int)], requirements: List[CommentaryProposal.Requirement],
      children: List[(List[String], Int)]) =
    import CommentaryProposal.*
    val profile = CommentaryEngineProfile.ServerAdmitted
    val origin = Origin(Source("user", "inline-condition-test"), Source("none", "none"),
      Source("none", "none"), Source("none", "none"))
    val owner = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val start = CommentaryJobReducer.startJob(fen, Nil, fen, roots.head._1.head,
      owner.extend(roots.head._1.take(1)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = false), 0L,
      proposals = List(Request("condition_test", origin, requirements))).toOption.get.asInstanceOf[AwaitingEngineWork]
    def submit(state: AwaitingEngineWork, rows: List[(List[String], Int)]) =
      val work = state.issuedWork
      CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile, work.workId,
        work.executionKeySha256, rows.map((moves, cp) =>
          ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), work.requiredDepth))), 1L).toOption.get
    val child = submit(start, roots).asInstanceOf[AwaitingEngineWork]
    assertEquals(child.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    val result = submit(child, children)
    val source = result.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(source)
    val retained = result.responseSearches.find(_.work.workId == child.issuedWork.workId).get
    val search = TargetResponseInvestigation.readSearch(source,
      CommentaryInvestigation.EngineCandidates(retained.work, retained.lines),
      owner.extend(retained.anchor.prefixMoves).toOption.get, prepared).get
    (source, prepared, search, origin)

  test("Anand supplied line links delayed original capturer removal and reads later victims through the end"):
    // New in Chess, 9215.pdf, printed pp. 18-19: author-supplied position and line.
    val fen = "2rq1rk1/p3bppp/bpn1p3/1P1n4/3P4/P1N2N2/1BB2PPP/R2QR1K1 b - - 0 15"
    val line = List("d5c3", "d1d3", "g7g6", "b2c3", "d8c7", "b5a6", "c6a5", "c3a5")
    val source = packet(fen, Nil, line)
    val (prepared, value) = read(source, line)
    assert(value.complete)
    assertEquals(value.events.map(_.plyOffset), List(0, 3, 5, 7))
    val first = value.events.head
    val recovery = value.events(1)
    assertEquals(first.originalActor.map(_.square.key), Some("d5"))
    assertEquals(first.originalVictim.map(_.square.key), Some("c3"))
    assertEquals(first.actorEnd.state, "captured")
    assertEquals(first.actorEnd.removedByEventId, Some(recovery.id))
    assertEquals(value.captureLinks, List(LineContinuationInvestigation.CaptureLink(first.id, recovery.id, 3)))
    assertEquals(value.events(2).originalVictim.map(_.square.key), Some("a6"))
    assertEquals(value.events(3).originalVictim.map(_.square.key), Some("c6"))
    assertEquals(recovery.actorEnd.piece.map(_.square.key), Some("a5"))
    assert(value.choicePoints.exists(_.prefixMovesUci == line.take(1)))
    assert(value.choicePoints.exists(_.prefixMovesUci == line.take(2)))
    assert(value.choicePoints.filter(_.prefixMovesUci.size <= 2).forall(_.kinds.contains("delayed_capturer_removal")))
    // Bc3xa5 also opens Qc7's file to Bc2. It is a legal reply candidate,
    // not a free bishop: Qd3 can recapture that queen on c2.
    assertEquals(value.captureExposures.map(exposure => exposure.prefixMoves -> exposure.captures.map(_.move)),
      List(line -> List("c7c2")))
    val afterQueenCapture = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
      .extend(line :+ "c7c2").toOption.get
    assert(afterQueenCapture.currentLegalMoveUcis.contains("d3c2"))
    val replacement = value.supportComparisons.find(comparison => comparison.kind == "replacement_after_capture" &&
      comparison.beforeActor.square.key == "c3" && comparison.afterActor.square.key == "b2").get
    val d4 = replacement.targets.find(_.originalTarget.square.key == "d4").get
    assertEquals(d4.before.state, "absent")
    assertEquals(d4.after.state, "present")
    assert(d4.after.sourceIds.nonEmpty)
    assert(prepared.continuationReadings eq prepared.continuationReadings)
    assert(LineContinuationInvestigation.readAll(source, prepared).find(_.id == value.id).get eq value)
    val rootOnly = prepared.certifiedPath(source, line.take(1)).get
    assert(prepared.identitiesAfter(source, value.path, 1) eq prepared.identitiesAfter(source, rootOnly, 1))
    val atEnd = prepared.identitiesAfter(source, value.path, line.size)
    assert(atEnd eq prepared.identitiesAfter(source, value.path, line.size))
    val bishop = prepared.identitiesAfter(source, value.path, 0).find(_._1.square.key == "b2").get._1
    assertEquals(prepared.pieceAfter(source, value.path, bishop, 4).flatten.map(_.square.key), Some("c3"))
    assertEquals(prepared.pieceAfter(source, value.path, bishop, 8).flatten.map(_.square.key), Some("a5"))
    assertEquals(prepared.pieceAfter(source, value.path, bishop, 9), None)
    val detail = value.candidateJson(source) \ "detail"
    assertEquals((detail \ "type").as[String], "line_continuation")
    assert((detail \ "net_cp").toOption.isEmpty)
    assert((detail \ "events").as[List[JsObject]].forall(row => (row \ "delta_cp").toOption.isEmpty))

  test("Ruy pawn recovery elsewhere proposes the loss response while the original capturer survives"):
    val history = List("e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")
    val line = List("f3e5", "d8d4", "e5f3", "d4e4")
    val source = packet(initial, history, line)
    val (_, value) = read(source, line)
    assertEquals(value.events.map(_.plyOffset), List(0, 3))
    assertEquals(value.captureLinks, Nil)
    assertEquals(value.events.head.actorEnd.state, "present")
    assertEquals(value.events.head.actorEnd.piece.map(_.square.key), Some("f3"))
    assertEquals(value.otherTargetGains, List(LineContinuationInvestigation.OtherTargetGain(
      value.events.head.id, value.events(1).id)))
    assertEquals(value.choicePoints.map(_.prefixMovesUci), List(line.take(1), line.take(2)))
    assertEquals(value.choicePoints.head.kinds, List("other_target_gain", "root_capture_response"))
    assertEquals(value.choicePoints.head.premises.map(premise => premise.kind -> premise.triggerIds.toSet).toMap,
      Map("root_capture_response" -> Set(value.events.head.occurrenceId),
        "other_target_gain" -> value.events.map(_.occurrenceId).toSet))
    assertEquals(value.choicePoints(1).kinds, List("other_target_gain"))
    assertEquals(value.events.head.originalVictim.map(_.square.key), Some("e5"))
    assertEquals(value.events(1).originalVictim.map(_.square.key), Some("e4"))

  test("a balanced Scandinavian exchange does not acquire a compensation claim from an unrelated later pawn gain"):
    val line = List("e4d5", "d8d5", "b1c3", "d5a5", "d2d4", "e7e5", "d4e5")
    val source = packet(initial, List("e2e4", "d7d5"), line)
    val (_, value) = read(source, line)
    assertEquals(value.events.map(_.plyOffset), List(0, 1, 6))
    assertEquals(value.captureLinks.map(_.elapsedPlies), List(1))
    assertEquals(value.otherTargetGains, Nil)
    assertEquals(value.choicePoints, Nil)

  test("one comparison source retains a witnessed recovery while its other recovery remains missing"):
    val fen = "1r5k/8/8/q7/8/8/1PP5/R6K w - - 0 1"
    val donor = List("a1a5", "b8b7", "h1g1", "b7b2", "g1h1", "b2c2")
    val peer = List("a1a5", "b8c8")
    val partial = peer ++ List("h1g1", "c8b8", "g1h1", "b8b2")
    val complete = partial ++ List("h1g1", "b2c2")
    def observations(extra: List[List[String]]) =
      val source = packet(fen, Nil, donor, peer :: extra)
      val prepared = ObservedLineProtocol.prepare(source)
      val donorPath = prepared.continuationReadings.find(_.path.moves == donor).get.path
      val peerPath = prepared.continuationReadings.find(_.path.moves == peer).get.path
      val ref = LineContinuationInvestigation.ComparisonSource("supplied-path-comparison",
        donorPath.line.ref.id, peerPath.line.ref.id)
      val requirements = LineContinuationInvestigation.continuationRequirements(source, peer, List(ref), prepared).get
      (source, prepared, ref, requirements)
    val (source, prepared, ref, requirements) = observations(List(partial))
    assertEquals(requirements.size, 2)
    assertEquals(requirements.map(_.id).distinct.size, 2)
    assertEquals(requirements.count(_.witnessLineIds.nonEmpty), 1)
    assertEquals(requirements.count(_.witnessLineIds.isEmpty), 1)
    assertEquals(LineContinuationInvestigation.observedContinuations(source, peer, List(ref), prepared), None)
    val (unobservedSource, unobservedPrepared, _, _) = observations(Nil)
    val peerReading = unobservedPrepared.continuationReadings.find(_.path.moves == peer).get
    val lossTriggers = List(peerReading.events.head.occurrenceId)
    val unobservedMaterial = LineContinuationInvestigation.materialContinuationRequirements(
      unobservedSource, peer, lossTriggers, unobservedPrepared).get
    assertEquals(unobservedMaterial.size, 1)
    assertEquals(unobservedMaterial.head.witnessLineIds, Nil)
    assertEquals(LineContinuationInvestigation.materialContinuationRequirements(unobservedSource, peer,
      List(peerReading.path.ids.last), unobservedPrepared), None) // Rc8 is not the open loss.
    val partialMaterial = LineContinuationInvestigation.materialContinuationRequirements(
      source, peer, lossTriggers, prepared).get
    assertEquals(partialMaterial.map(_.id), unobservedMaterial.map(_.id))
    assert(partialMaterial.forall(_.witnessLineIds.nonEmpty)) // A partial gain answers this local probe only.
    val (finished, finishedPrepared, finishedRef, answered) = observations(List(partial, complete))
    assertEquals(answered.map(_.id).toSet, requirements.map(_.id).toSet)
    assert(answered.forall(_.witnessLineIds.nonEmpty))
    assert(LineContinuationInvestigation.observedContinuations(finished, peer, List(finishedRef), finishedPrepared).nonEmpty)

  test("support requirements retain separate targets and partial exact peer witnesses"):
    val fen = "7k/8/8/2Pp2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val donor = List("g1h1", "d5e4", "d2e4")
    val observation = donor ++ List("h8g7", "h1g2", "g7g6", "g2h2", "g6f7", "h2g2",
      "f7e7", "g2h2", "e7d7", "h2g2", "d7c6", "g2h2")
    val peer = List("g1h1", "d5e4", "f3e4")
    val partial = peer ++ List("h8g7", "h1g2", "g7g6", "g2h2")
    val source = packet(fen, Nil, observation, List(donor, peer, partial))
    val prepared = ObservedLineProtocol.prepare(source)
    def path(moves: List[String]) = prepared.continuationReadings.find(_.path.moves == moves).get.path
    val ref = LineContinuationInvestigation.ComparisonSource("supplied-support-comparison",
      path(donor).line.ref.id, path(peer).line.ref.id, "support_application", Some(path(observation).line.ref.id))
    val requirements = LineContinuationInvestigation.continuationRequirements(source, peer, List(ref), prepared).get
    assertEquals(requirements.size, 2)
    assertEquals(requirements.map(_.id).distinct.size, 2)
    val witnessed = requirements.find(_.witnessLineIds.nonEmpty).get
    val missing = requirements.find(_.witnessLineIds.isEmpty).get
    assertEquals(witnessed.witnessLineIds, List(path(partial).line.ref.id))
    assertEquals(LineContinuationInvestigation.observedContinuations(source, peer, List(ref), prepared), None)
    val choices = prepared.continuationReadings.find(_.path.moves == observation).get.replacementFunctionChoices
    assertEquals(choices.flatMap(_.point.premises.flatMap(_.requirementId)).toSet, requirements.map(_.id).toSet)
    val applications = (prepared.continuationReadings.find(_.path.moves == observation).get
      .candidateJson(source) \ "detail" \ "support_applications").as[List[JsObject]]
    assertEquals(applications.flatMap(row => (row \ "requirement_id").asOpt[String]).toSet, requirements.map(_.id).toSet)
    assertEquals(LineContinuationInvestigation.unresolvedSupportContinuations(source, peer, List(ref),
      List(path(partial)), prepared, Some(witnessed.id)), Nil)
    assertEquals(LineContinuationInvestigation.unresolvedSupportContinuations(source, peer, List(ref),
      List(path(partial)), prepared, Some(missing.id)), List(path(partial).line.ref.id))

  test("a recovered pawn does not hide a subsequent loss of the piece that recovered it"):
    val history = List("e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")
    // Deliberately poor Black play: a supplied counterexample, not an engine PV.
    val line = List("f3e5", "d8d4", "e5f3", "d4e4", "e1f1", "e4e2", "f1e2")
    val source = packet(initial, history, line)
    val (_, value) = read(source, line)
    assert(value.otherTargetGains.exists(_.lossEventId == value.events.head.id))
    val queenLoss = value.events.find(_.plyOffset == 6).get
    assertEquals(queenLoss.originalVictim.map(_.square.key), Some("d8"))
    assert(value.openLossEventIds(queenLoss.id))
    assert(!value.openLossEventIds(value.events.head.id))
    assert(value.lossOrigins(queenLoss.occurrenceId).contains(value.events.head.occurrenceId))
    assert(value.choicePoints.exists(_.triggerIds.contains(queenLoss.occurrenceId)))

  test("a delayed exact descendant answers the physical premise while a sibling line cannot"):
    val fen = "2rq1rk1/p3bppp/bpn1p3/1P1n4/3P4/P1N2N2/1BB2PPP/R2QR1K1 b - - 0 15"
    val donor = List("d5c3", "d1d3", "g7g6", "b2c3")
    val peer = List("d5c3", "d1d3", "f7f5", "d3d2")
    val extension = peer ++ List("g7g6", "b2c3")
    def observed(extra: List[List[String]]) =
      val source = packet(fen, Nil, donor, peer :: extra)
      val prepared = ObservedLineProtocol.prepare(source)
      val sourceRef = LineContinuationInvestigation.ComparisonSource("supplied-path-comparison",
        prepared.continuationReadings.find(_.path.moves == donor).get.path.line.ref.id,
        prepared.continuationReadings.find(_.path.moves == peer).get.path.line.ref.id)
      LineContinuationInvestigation.observedContinuations(source, peer, List(sourceRef), prepared)
    assertEquals(observed(Nil), None)
    assert(observed(List(extension)).nonEmpty)

  test("a physical expected victim remains observed after its original accounting interval has closed"):
    val history = List("e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")
    val donor = List("f3e5", "d8d4", "e5f3", "d4e4")
    val peer = List("f3e5", "d8d4", "b1c3")
    // Another supplied poor-play counterexample: ...Qxe5 closes the original
    // interval, ...Qxe4 still captures the expected pawn, then Black loses Q.
    val extension = peer ++ List("d4e5", "d2d4", "e5e4", "c1e3", "e4e3", "f2e3")
    val source = packet(initial, history, donor, List(peer, extension))
    val prepared = ObservedLineProtocol.prepare(source)
    val ref = LineContinuationInvestigation.ComparisonSource("supplied-path-comparison",
      prepared.continuationReadings.find(_.path.moves == donor).get.path.line.ref.id,
      prepared.continuationReadings.find(_.path.moves == peer).get.path.line.ref.id)
    val observed = LineContinuationInvestigation.observedContinuations(source, peer, List(ref), prepared).get
    assert(observed.contains(prepared.continuationReadings.find(_.path.moves == extension).get.path.line.ref.id))

  test("an intermediate other gain does not answer an expected capturer removal after a later loss"):
    val history = List("e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")
    val donor = List("f3e5", "d8d4", "b1c3", "d4e5")
    val peer = List("f3e5", "d8d4", "e5f3")
    val extension = peer ++ List("d4e4", "e1f1", "e4e2", "f1e2")
    val source = packet(initial, history, donor, List(peer, extension))
    val prepared = ObservedLineProtocol.prepare(source)
    val ref = LineContinuationInvestigation.ComparisonSource("supplied-path-comparison",
      prepared.continuationReadings.find(_.path.moves == donor).get.path.line.ref.id,
      prepared.continuationReadings.find(_.path.moves == peer).get.path.line.ref.id)
    assertEquals(LineContinuationInvestigation.observedContinuations(source, peer, List(ref), prepared), None)
    val loss = prepared.continuationReadings.find(_.path.moves == peer).get.events.head
    assert(LineContinuationInvestigation.observedMaterialContinuations(source, peer,
      List(loss.occurrenceId), prepared).nonEmpty) // The pawn gain remains a true, different observation.

  test("ordinary immediate Scotch exchanges preserve every capture link without creating a continuation inquiry"):
    val line = List("d2d4", "e5d4", "f3d4", "c6d4", "d1d4")
    val source = packet(initial, List("e2e4", "e7e5", "g1f3", "b8c6"), line)
    val (_, value) = read(source, line)
    assertEquals(value.events.map(_.plyOffset), List(1, 2, 3, 4))
    assertEquals(value.captureLinks.map(_.elapsedPlies), List(1, 1, 1))
    assertEquals(value.events.take(3).map(_.actorEnd.state), List("captured", "captured", "captured"))
    assertEquals(value.events.last.actorEnd.state, "present")
    assertEquals(value.otherTargetGains, Nil)
    assertEquals(value.choicePoints, Nil)
    assertEquals(value.supportComparisons.count(_.kind == "replacement_after_capture"), 3)

  test("promotion remains the same original physical piece and a supplied path end is not stability"):
    val line = List("e7e8q", "h8h7", "e8h5")
    val source = packet("7k/4P3/8/8/8/8/8/6K1 w - - 0 1", Nil, line)
    val (prepared, value) = read(source, line)
    val event = value.events.head
    assertEquals(event.originalActor.map(_.role.name), Some("pawn"))
    assertEquals(event.originalActor.map(_.square.key), Some("e7"))
    assertEquals(event.originalVictim, None)
    assertEquals(event.actorEnd.piece.map(_.role.name), Some("queen"))
    assertEquals(event.actorEnd.piece.map(_.square.key), Some("h5"))
    assertEquals(value.choicePoints, Nil)
    assertEquals((ObservedLineProtocol.summaryJson(value.path).get \ "ending" \ "kind").as[String], "line_end")
    val original = event.originalActor.get
    assertEquals(prepared.pieceAfter(source, value.path, original, 0), Some(Some(original)))
    val other = packet("7k/4P3/8/8/8/8/8/6K1 w - - 0 1", Nil, line)
    intercept[IllegalArgumentException](value.candidateJson(other))
    intercept[IllegalArgumentException](LineContinuationInvestigation.readAll(other, prepared))

  test("a bare root capture asks for replies before a supplied compensation line exists, while quiet paths add no template"):
    val history = List("e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")
    val capture = List("f3e5")
    val source = packet(initial, history, capture)
    val (_, value) = read(source, capture)
    assertEquals(value.captureLinks, Nil)
    assertEquals(value.otherTargetGains, Nil)
    assertEquals(value.choicePoints.map(_.prefixMovesUci), List(capture))
    assertEquals(value.choicePoints.head.kinds, List("root_capture_response"))
    val quiet = packet(initial, List("e2e4", "e7e5", "g1f3", "b8c6"), List("f1b5"))
    val prepared = ObservedLineProtocol.prepare(quiet)
    val quietReading = prepared.continuationReadings.find(_.path.moves == List("f1b5")).get
    assert(quietReading.complete, "a certified quiet root remains readable as a comparison arm")
    assertEquals(quietReading.events, Nil)
    assertEquals(quietReading.choicePoints, Nil, "reading a quiet move does not invent a reply or an investigation")
    assert(prepared.continuationReadings eq prepared.continuationReadings)

  test("a delayed removal retains its immediate capture-chain ancestor choice point"):
    // Author-supplied Orton-Shafi line; every position uses its exact retained history.
    val fen = "1rr3k1/4q1pp/p2pp3/4p1N1/BPn2nQ1/P5PP/1P3P2/1R2R1K1 b - - 0 27"
    val line = List("h7h5", "g4h4", "e7g5", "h4g5", "f4h3", "g1g2", "h3g5")
    val source = packet(fen, Nil, line)
    val (prepared, value) = read(source, line)
    assertEquals(value.events.map(_.plyOffset), List(2, 3, 4, 6))
    val delayed = value.captureLinks.find(link => link.lossEventId == value.events(1).id &&
      link.captureEventId == value.events(3).id).get
    assertEquals(delayed.elapsedPlies, 3)
    val ancestor = value.choicePoints.find(_.prefixMovesUci == line.take(3)).get
    assert(ancestor.triggerIds.contains(value.events.head.occurrenceId))
    assert(ancestor.triggerIds.contains(value.events(1).occurrenceId))
    assert(ancestor.triggerIds.contains(value.events.last.occurrenceId))
    assert(ancestor.kinds.contains("delayed_capturer_removal"))
    val counterexamples = LineContinuationInvestigation.refutingMoves(source, ancestor.prefixMovesUci,
      List(value.path.line.ref.id), ancestor.triggerIds, prepared)
    assert(!counterexamples.contains("g3f4")) // After h5 Qh4 this pawn is pinned by Qg5 against Kg1.
    val directLine = List("e7g5", "g4g5", "f4h3", "g1g2", "h3g5")
    val directSource = packet(fen, Nil, directLine)
    val (directPrepared, direct) = read(directSource, directLine)
    val directPoint = direct.choicePoints.find(_.prefixMovesUci == directLine.take(1)).get
    assert(LineContinuationInvestigation.refutingMoves(directSource, directPoint.prefixMovesUci,
      List(direct.path.line.ref.id), directPoint.triggerIds, directPrepared).contains("g3f4"))

  test("a replacement's stationary support is actually used by a supplied recapture, without a forced-defense claim"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val line = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3", "e4f3")
    val source = packet(fen, Nil, line)
    val (prepared, value) = read(source, line)
    val applications = LineContinuationInvestigation.readDirectSupportRecaptures(source, value, prepared)
    assertEquals(applications.size, 1)
    val application = applications.head
    assertEquals(application.comparison.kind, "replacement_after_capture")
    assertEquals(application.comparison.beforeActor.square.key, "e4")
    assertEquals(application.comparison.afterActor.square.key, "f1")
    assertEquals(application.originalTarget.square.key, "f3")
    assertEquals(application.originalCapturer.square.key, "d8")
    assertEquals(application.provider.square.key, "e4")
    assertEquals(application.target.square.key, "f3")
    assertEquals(application.supportCount, 5)
    assertEquals(application.supportPositionId, value.path.positions(5).id.get)
    assertEquals(application.support.state, "present")
    assert(application.support.sourceIds.nonEmpty)
    assertEquals(application.targetCapture.plyOffset, 5)
    assertEquals(application.recapture.plyOffset, 6)
    assert(value.captureLinks.exists(link => link.lossEventId == application.targetCapture.id &&
      link.captureEventId == application.recapture.id))
    val views = prepared.resourceViews(source)
    def legalAfter(count: Int) = views(value.path.positions(count).id.get).legalRows.get.map(_.move).toSet
    // This is deliberately weak supplied play: White already had Bxd3, and
    // Qxf3/Kxf3 also recapture later. No uniqueness, force, or strength is tested.
    assert(legalAfter(4)("e4d3"))
    assert(Set("e4f3", "d1f3", "g2f3").subsetOf(legalAfter(6)))
    val json = application.json(value.path.positions.head.id.get)
    assertEquals((json \ "kind").as[String], "direct_support_recapture")
    assertEquals((json \ "loss_event_id").as[String], application.targetCapture.id)
    assertEquals((json \ "recapture_event_id").as[String], application.recapture.id)
    assertEquals((json \ "elapsed_plies").as[Int], 1)
    val delayedLine = line.dropRight(1) ++ List("g2h2", "g8h8", "e4f3")
    val delayedSource = packet(fen, Nil, delayedLine)
    val (delayedPrepared, delayedReading) = read(delayedSource, delayedLine)
    val delayed = LineContinuationInvestigation.readDirectSupportRecaptures(delayedSource, delayedReading, delayedPrepared)
    assertEquals(delayed.size, 1)
    assertEquals(delayed.head.recapture.plyOffset - delayed.head.targetCapture.plyOffset, 3)
    assertEquals((delayed.head.json(delayedReading.path.positions.head.id.get) \ "requirement_id").as[String],
      (json \ "requirement_id").as[String])

  test("comparison conditions retain the defended target and exact history of a support use"):
    import CommentaryFact.*
    import CommentaryProposal.*
    val fen = "3r2k1/8/8/3p4/4P3/2N2N2/3P4/3Q1B1K w - - 0 1"
    val line = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3", "e4f3")
    val loop = line.take(2) ++ List("h1g1", "d8d7", "g1h1", "d7d8") ++ line.drop(2)
    val sibling = line.take(2) ++ List("h1g2", "d8d7", "g2h1", "d7d8") ++ line.drop(2)
    val source = packet(fen, Nil, line, List(loop, sibling))
    val prepared = ObservedLineProtocol.prepare(source)
    def inquiry(moves: List[String], count: Int): CommentaryContinuationInquiry =
      val reading = prepared.continuationReadings.find(_.path.moves == moves).get
      val point = reading.replacementFunctionChoices.find(_.point.prefixMovesUci.size == count).get.point
      pending(source, prepared, reading.path, point)
    val first = inquiry(line, 2)
    val second = inquiry(loop, 6)
    val third = inquiry(sibling, 6)
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    assertEquals(history.extend(loop.take(6)).toOption.get.currentFen,
      history.extend(sibling.take(6)).toOption.get.currentFen)
    assertEquals(first.question.premises.flatMap(_.origin.requirementId), second.question.premises.flatMap(_.origin.requirementId),
      "the same physical support ID is possible after a different history")
    val provider = Piece("white", "bishop", "f1")
    val defended = Piece("white", "knight", "f3")
    val capturer = Piece("black", "rook", "d8")
    val event = Event("capture", 1, "after", 5, 7, Some(provider), Some(capturer))
    val used = Fact("used", Path("line", Declared(line)), Predicate("direct_support_use", Some(provider), Some(defended)),
      event, expected = Some("true"))
    val later = used.copy(id = "later", path = Path("loop", Declared(loop)), at = event.copy(fromPly = 9, throughPly = 11))
    val otherHistory = later.copy(id = "other_history", path = Path("sibling", Declared(sibling)))
    val material = used.copy(id = "material", predicate = Predicate("presence", Some(capturer)), expected = Some("removed"))
    val origin = Origin(Source("author", "inline-support-test"), Source("none", "none"),
      Source("none", "none"), Source("none", "none"))
    val entry = CommentaryFactReader.read(source, prepared, entries(List(Request("conditions", origin,
      List(used, later, otherHistory, material))))).head
    def result(fact: Fact) = entry.readings.find(_.requirementId == fact.id).get.result.get
    List(used, later, otherHistory, material).foreach(fact => assertEquals(result(fact).state, "supported"))
    val tests = LineContinuationInvestigation.conditionTests(source, prepared, List(first, second, third))
    assert(tests(first.question.id)(List(used -> result(used))))
    assert(tests(second.question.id)(List(later -> result(later))))
    assert(!tests(second.question.id)(List(used -> result(used))), "a shared support ID cannot borrow another history's recapture")
    assert(tests(third.question.id)(List(otherHistory -> result(otherHistory))))
    assert(!tests(third.question.id)(List(later -> result(later))), "equal FEN and ply do not transfer another exact history's fact")
    assert(!tests(first.question.id)(List(material -> result(material))),
      "capturing the rook is not by itself the named use of support for the knight")

  test("king access links the same original king, target, provider and observed point within one arm"):
    import CommentaryFact.*
    import CommentaryProposal.*
    val fen = "7k/8/8/2Pp2P1/4P2B/5P2/3N4/6K1 w - - 0 1"
    val line = List("g1h1", "d5e4", "d2e4", "h8g7", "h1g2", "g7g6", "g2h2", "g6f7", "h2g2",
      "f7e7", "g2h2", "e7d7", "h2g2", "d7c6", "g2h2")
    val source = packet(fen, Nil, line)
    val (prepared, reading) = read(source, line)
    val king = Piece("black", "king", "h8")
    val provider = Piece("white", "knight", "d2")
    val replacement = reading.supportComparisons.find(comparison => comparison.kind == "replacement_after_capture" &&
      comparison.afterActor == provider.witness).get
    val lateTarget = Piece("white", "pawn", "c5").witness
    assertEquals(reading.kingCaptureOpportunities(replacement, lateTarget, 7), Nil)
    val wider = reading.kingCaptureOpportunities(replacement, lateTarget, 15)
    assert(wider.exists(_.count == 15), "a wider request can extend the same owned observation")
    assertEquals(reading.kingCaptureOpportunities(replacement, lateTarget, 9), Nil,
      "a shorter horizon cannot borrow the later cached king approach")
    val origin = Origin(Source("author", "inline-king-condition"), Source("none", "none"),
      Source("none", "none"), Source("none", "none"))
    def facts(name: String, target: String, count: Int, defender: Piece = provider): List[Fact] = List(
      Fact(s"${name}_capture", Path("line", Declared(line)),
        Predicate("legal_capture", Some(king), Some(Piece("white", "pawn", target))), Ply(count)),
      Fact(s"${name}_support", Path("line", Declared(line)),
        Predicate("support", Some(defender), Some(Piece("white", "pawn", target))), Ply(count)))
    val first = facts("first", "g5", 7)
    val second = facts("second", "c5", 15)
    val distant = facts("distant", "c5", 7)
    val later = facts("later", "g5", 9)
    val wrongTurn = facts("turn", "g5", 6)
    val otherProvider = facts("other", "g5", 7, Piece("white", "bishop", "h4"))
    val removedProvider = facts("removed", "g5", 7, Piece("white", "pawn", "e4"))
    val movedKing = first.head.copy(id = "moved_king", predicate = first.head.predicate.copy(
      subject = Some(Piece("black", "king", "g6"))))
    val eventFacts = first.map(fact => fact.copy(id = s"event_${fact.id}",
      at = Event("move", 1, "after", 6, 7, Some(Piece("white", "king", "g1")), square = Some("h2"))))
    val interval = first.last.copy(id = "interval", at = Ply(3), through = Some(Ply(7)), expected = Some("present"))
    val unfinished = interval.copy(id = "unfinished", through = Some(Ply(16)))
    val all = first ++ second ++ distant ++ later ++ wrongTurn ++ otherProvider ++ removedProvider ++ eventFacts ++
      List(movedKing, interval, unfinished)
    val entry = CommentaryFactReader.read(source, prepared, entries(List(Request("conditions", origin, all)))).head
    def result(fact: Fact) = entry.readings.find(_.requirementId == fact.id).get.result.get
    def evidence(values: List[Fact]) = values.map(fact => fact -> result(fact))
    def testAt(count: Int) =
      val point = reading.replacementFunctionChoices.find(_.point.triggerIds.contains(reading.path.ids(count - 1))).get.point
      val inquiry = pending(source, prepared, reading.path, point)
      assertEquals(inquiry.state, "deferred")
      LineContinuationInvestigation.conditionTests(source, prepared, List(inquiry))(inquiry.question.id)
    val firstTest = testAt(7)
    val secondTest = testAt(15)
    assert(firstTest(evidence(first)))
    assert(secondTest(evidence(second)))
    assert(firstTest(evidence(eventFacts)), "a real preceding event can select the same observation as Ply")
    assert(firstTest(evidence(first.head :: List(interval))), "a fully proved interval includes its witnessed points")
    assert(!firstTest(evidence(first.head :: List(unfinished))), "an unfinished interval is still an unmet prerequisite")
    assert(result(first.head).samples.exists(sample => sample.throughPly == 7 && sample.moves.size == 15),
      "the declared suffix length is not the observation time")
    assert(!firstTest(evidence(first.take(1))), "illegal alone cannot name the provider")
    assert(!firstTest(evidence(first.drop(1))), "support alone cannot name the king's approach")
    assert(!firstTest(evidence(first.take(1) ++ otherProvider.drop(1))), "another actual defender cannot donate its support")
    assert(!firstTest(evidence(first.take(1) ++ removedProvider.drop(1))), "the replaced pawn is not the new provider")
    assert(!firstTest(evidence(movedKing :: first.drop(1))), "current squares do not replace original king identity")
    assert(!firstTest(evidence(second)))
    assert(!secondTest(evidence(distant)), "a distant illegal king capture is not the later approach")
    assert(!firstTest(evidence(later)), "the same predicates at a later point do not replace the named observation")
    assert(!firstTest(evidence(first.take(1) ++ later.drop(1))), "the atoms must coexist at one time")
    assert(!firstTest(evidence(wrongTurn)))
    assertEquals(result(wrongTurn.head).samples.flatMap(_.value).distinct, List("not_turn"))
    assertEquals(result(otherProvider.last).samples.flatMap(_.value).distinct, List("present"))
    assert(!firstTest(List(first.head -> result(first.head), first.last -> result(first.last).copy(proofs = Nil))),
      "an unproved sample retained beside other evidence is not a supported condition")

    val comparison = CompareMoves("recapture", line.take(2), "d2e4", "f3e4")
    def arm(name: String, heads: List[String]) = GeneratedArm(name, line.take(2), "d2e4", Some("f3e4"),
      heads :+ comparison.id, all.map(_.id) :+ comparison.id, origin.question, Nil)
    val split = entry.copy(request = entry.request.copy(requirements = entry.request.requirements :+ comparison),
      readings = entry.readings :+ CommentaryProposal.Reading(comparison.id),
      generatedArms = List(arm("capture", List(first.head.id)), arm("support", List(first.last.id))))
    def linked(value: Entry) = comparisonContexts(value, comparison, readyOnly = true).exists(context =>
      firstTest(context.map(fact => fact -> value.readings.find(_.requirementId == fact.id).get.result.get)))
    assert(!linked(split), "separately ready arms must not be merged into a new explanation")
    assert(linked(split.copy(generatedArms = List(arm("together", first.map(_.id))))))

  test("king support conditions retain exact history and do not survive removal of the named provider"):
    import CommentaryFact.*
    import CommentaryProposal.*
    val fen = "7k/8/8/2Pp2P1/4P2B/5P2/3N4/6K1 w - - 0 1"
    val base = List("g1h1", "d5e4", "d2e4", "h8g7", "h1g2", "g7g6", "g2h2")
    val first = base.take(2) ++ List("h1g1", "h8g8", "g1h1", "g8h8") ++ base.drop(2)
    val second = base.take(2) ++ List("h1g2", "h8g8", "g2h1", "g8h8") ++ base.drop(2)
    val removed = base.take(4) ++ List("f3f4", "g7g6", "h1g2", "g6f5", "g2h2", "f5e4", "h2h1", "e4f5", "h1h2")
    val source = packet(fen, Nil, first, List(second, removed))
    val prepared = ObservedLineProtocol.prepare(source)
    val owner = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    assertEquals(owner.extend(first).toOption.get.currentFen, owner.extend(second).toOption.get.currentFen)
    def inquiry(moves: List[String], prefix: Int, observedAt: Int) =
      val reading = prepared.continuationReadings.find(_.path.moves == moves).get
      pending(source, prepared, reading.path, reading.replacementFunctionChoices.find(choice =>
        choice.point.prefixMovesUci.size == prefix && choice.point.triggerIds.contains(reading.path.ids(observedAt - 1))).get.point)
    val a = inquiry(first, 6, 11)
    val b = inquiry(second, 6, 11)
    val c = inquiry(removed, 2, 9)
    assertEquals(a.question.premises.flatMap(_.origin.requirementId), b.question.premises.flatMap(_.origin.requirementId))
    val tests = LineContinuationInvestigation.conditionTests(source, prepared, List(a, b, c))
    def facts(name: String, moves: List[String], count: Int) = List(
      Fact(s"${name}_capture", Path(name, Declared(moves)),
        Predicate("legal_capture", Some(Piece("black", "king", "h8")), Some(Piece("white", "pawn", "g5"))), Ply(count)),
      Fact(s"${name}_support", Path(name, Declared(moves)),
        Predicate("support", Some(Piece("white", "knight", "d2")), Some(Piece("white", "pawn", "g5"))), Ply(count)))
    val af = facts("first", first, 11)
    val bf = facts("second", second, 11)
    val before = facts("before", removed, 9)
    val after = facts("after", removed, 13)
    val origin = Origin(Source("author", "inline-king-history"), Source("none", "none"),
      Source("none", "none"), Source("none", "none"))
    val entry = CommentaryFactReader.read(source, prepared, entries(List(Request("conditions", origin, af ++ bf ++ before ++ after)))).head
    def result(fact: Fact) = entry.readings.find(_.requirementId == fact.id).get.result.get
    def evidence(facts: List[Fact]) = facts.map(fact => fact -> result(fact))
    assert(tests(a.question.id)(evidence(af)))
    assert(tests(b.question.id)(evidence(bf)))
    assert(!tests(a.question.id)(evidence(bf)), "identical FEN, ply and support ID do not authorize another history")
    assert(!tests(a.question.id)(evidence(af.take(1) ++ bf.drop(1))), "two histories cannot jointly supply one condition")
    assert(tests(c.question.id)(evidence(before)))
    assertEquals(result(after.head).samples.flatMap(_.value).distinct, List("illegal"))
    assertEquals(result(after.last).samples.flatMap(_.value).distinct, List("actor_removed"))
    assert(!tests(c.question.id)(evidence(after)), "the bishop still defends the pawn after the named knight is captured")

  test("owned king support conditions link a short recapture peer without answering its missing result"):
    import CommentaryFact.*
    import CommentaryProposal.*
    val fen = "7k/8/8/2Pp2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val prefix = List("g1h1", "d5e4")
    val donor = prefix ++ List("d2e4", "h8g7", "h1g2", "g7g6", "g2h2")
    val peer = prefix :+ "f3e4"
    val need = Fact("next", Path("next", Declared(prefix), extend = true),
      Predicate("presence", Some(Piece("white", "pawn", "e4"))), Ply(3), expected = Some("removed"))
    val (source, prepared, search, origin) = returned(fen,
      List(prefix -> 100, List("g1g2") -> 0, List("d2b3") -> -10), List(need),
      List(List("f3e4") -> 100, donor.drop(2) -> 90, List("h1g1") -> 80))
    val (reading, point) = LineContinuationInvestigation.unobservedContinuations(source, search, prepared)
      .find((_, point) => point.prefixMovesUci == peer && point.kinds.contains("support_application_not_observed")).get
    val scoped = point.copy(premises = point.premises.filter(_.kind == "support_application_not_observed"))
    val inquiry = pending(source, prepared, reading.path, scoped, List(search))
    val target = Piece("white", "pawn", "g5")
    val facts = List(
      Fact("capture", Path("donor", Declared(donor)),
        Predicate("legal_capture", Some(Piece("black", "king", "h8")), Some(target)), Ply(7)),
      Fact("support", Path("donor", Declared(donor)),
        Predicate("support", Some(Piece("white", "knight", "d2")), Some(target)), Ply(7)))
    val entry = CommentaryFactReader.read(source, prepared, entries(List(Request("conditions", origin, facts)))).head
    val evidence = facts.map(fact => fact -> entry.readings.find(_.requirementId == fact.id).get.result.get)
    assert(inquiry.premiseObservations.forall(_.observationState == "unobserved"))
    assert(LineContinuationInvestigation.conditionTests(source, prepared, List(inquiry), List(search))(inquiry.question.id)(evidence))
    assert(!LineContinuationInvestigation.conditionTests(source, prepared, List(inquiry))(inquiry.question.id)(evidence),
      "the actual returned comparison owns this transfer")
    val observed = inquiry.copy(premiseObservations = inquiry.premiseObservations.map(_.copy(observationState = "observed")))
    assert(!LineContinuationInvestigation.conditionTests(source, prepared, List(observed), List(search))(observed.question.id)(evidence))
    assertEquals(inquiry.state, "deferred")
    assert(inquiry.premiseObservations.forall(_.observationState == "unobserved"),
      "the donor's simultaneous facts do not supply the peer's missing king approach")

    val sources = prepared.paths.map(_.moves).distinct.map(moves =>
      CommentaryDiscovery.SourcePath(moves, Source("engine", "inline-owned-report")))
    def discover(existing: List[Entry]) = CommentaryDiscovery.discover(source, prepared,
      List(CommentaryDiscovery.Anchor(Nil, prefix.head)), sources, existing, pvGuided = true)
    val batch = discover(Nil)
    val generated = batch.additions.find(entry => entry.request.origin.question.producer == "C-opponent-choice" &&
      entry.request.requirements.exists {
        case value: CompareMoves => value.alternativeMove == "g1g2"
        case _ => false
      }).get
    def links(entry: Entry): Boolean =
      val observed = CommentaryFactReader.read(source, prepared, List(entry)).head
      val comparison = observed.request.requirements.collectFirst { case value: CompareMoves => value }.get
      val readings = observed.readings.flatMap(reading => reading.result.map(reading.requirementId -> _)).toMap
      comparisonContexts(observed, comparison, readyOnly = true).exists(context =>
        LineContinuationInvestigation.conditionTests(source, prepared, List(inquiry), List(search))(inquiry.question.id)(
          context.flatMap(fact => readings.get(fact.id).map(fact -> _))))
    assert(links(generated), "the real producer must place both atoms in one ready comparison context")
    assertEquals(generated.request.requirements.count(_.isInstanceOf[CompareMoves]), 1,
      "the witnessed application shares the existing pair")
    val others = batch.additions.filterNot(_.request.id == generated.request.id)
    val remaining = MaximumRequirements - others.map(_.request.requirements.size).sum - 5
    assert(remaining >= 0)
    val filler = entries(List(Request("occupied", origin,
      (1 to remaining).map(n => Uninterpreted(s"held_$n", "existing unrelated question")).toList)))
    val bare = discover(others ++ filler).additions.find(_.request.id == generated.request.id).get
    assertEquals(bare.request.requirements.size, 4)
    assertEquals(bare.generatedArms.size, 1, "an unadmitted bundle cannot leave dangling or partial condition heads")
    assert(!links(bare))
    val extended = discover(others :+ bare).additions.find(_.request.id == generated.request.id).get
    assert(links(extended), "freed capacity admits the same complete condition without another pair")
    assertEquals(extended.request.requirements.count(_.isInstanceOf[CompareMoves]), 1)
    assert(bare.request.requirements.forall(extended.request.requirements.contains))
    assert(validRequests(List(extended.request)))

  test("a later-ranked donor links its exact recovery to the earlier short owned peer"):
    import CommentaryFact.*
    import CommentaryProposal.*
    val fen = "1r5k/8/8/q7/8/8/1PP5/R6K w - - 0 1"
    val donor = List("a1a5", "b8b7", "h1g1", "b7b2", "g1h1", "b2c2")
    val peer = List("a1a5", "b8c8")
    val (source, prepared, search, origin) = returned(fen,
      List(peer -> 0, List("h1g1") -> -100, List("a1b1") -> -200),
      List(CompareMoves("reply", peer.take(1), "b8c8", "b8b7")),
      List(peer.drop(1) -> 0, donor.drop(1) -> 10))
    val expected = prepared.continuationReadings.find(_.path.moves == donor).get.events.find(_.plyOffset == 5).get
    val (reading, point) = LineContinuationInvestigation.unobservedContinuations(source, search, prepared)
      .find((_, point) => point.prefixMovesUci == peer && point.premises.exists(premise =>
        premise.kind == "continuation_not_observed" && premise.triggerIds.contains(expected.occurrenceId))).get
    val scoped = point.copy(premises = point.premises.filter(premise => premise.kind == "continuation_not_observed" &&
      premise.triggerIds.contains(expected.occurrenceId)))
    val inquiry = pending(source, prepared, reading.path, scoped, List(search))
    val owner = inquiry.question.comparisonSources.head
    assert(!search.selectedLinePairs.exists((left, right) =>
      left.line.ref.id == owner.donorLineId && right.line.ref.id == owner.peerLineId))
    val fact = Fact("gain", Path("donor", Declared(donor)), Predicate("presence", Some(Piece("white", "pawn", "c2"))),
      Event("capture", 1, "after", 5, 6, Some(Piece("black", "rook", "b8")), Some(Piece("white", "pawn", "c2"))),
      expected = Some("removed"))
    val result = CommentaryFactReader.read(source, prepared, entries(List(Request("gain", origin, List(fact)))))
      .head.readings.head.result.get
    assertEquals(result.state, "supported")
    assert(inquiry.premiseObservations.forall(_.observationState == "unobserved"))
    assert(LineContinuationInvestigation.conditionTests(source, prepared, List(inquiry), List(search))(inquiry.question.id)(List(fact -> result)))
    assert(!LineContinuationInvestigation.conditionTests(source, prepared, List(inquiry))(inquiry.question.id)(List(fact -> result)),
      "a named comparison without its owning returned work cannot supply the link")

  test("a support-family pair retains its condition when the whole report leads with a different move"):
    import CommentaryFact.*
    import CommentaryProposal.*
    val fen = "3r2k1/8/8/3p4/4P3/2N2N2/3P4/3Q1B1K w - - 0 1"
    val prefix = List("f1d3", "d5e4")
    val donor = prefix ++ List("d3e4", "d8d3", "h1g2", "d3f3", "e4f3")
    val peer = prefix :+ "c3e4"
    val need = Fact("next", Path("next", Declared(prefix), extend = true),
      Predicate("presence", Some(Piece("white", "pawn", "e4"))), Ply(3), expected = Some("removed"))
    val (source, prepared, search, origin) = returned(fen,
      List(prefix -> 100, List("h1g1") -> 0, List("d2d3") -> -10), List(need),
      List(List("h1g1") -> 100, List("c3e4") -> 90, donor.drop(2) -> 80))
    val (reading, point) = LineContinuationInvestigation.unobservedContinuations(source, search, prepared)
      .find((_, point) => point.prefixMovesUci == peer && point.kinds.contains("support_application_not_observed")).get
    val scoped = point.copy(premises = point.premises.filter(_.kind == "support_application_not_observed"))
    val inquiry = pending(source, prepared, reading.path, scoped, List(search))
    val owner = inquiry.question.comparisonSources.head
    assert(!search.selectedLinePairs.exists((left, right) =>
      Set(left.line.ref.id, right.line.ref.id) == Set(owner.donorLineId, owner.peerLineId)))
    val provider = Piece("white", "bishop", "f1")
    val capturer = Piece("black", "rook", "d8")
    val fact = Fact("used", Path("donor", Declared(donor)),
      Predicate("direct_support_use", Some(provider), Some(Piece("white", "knight", "f3"))),
      Event("capture", 1, "after", 5, 7, Some(provider), Some(capturer)), expected = Some("true"))
    val result = CommentaryFactReader.read(source, prepared, entries(List(Request("used", origin, List(fact)))))
      .head.readings.head.result.get
    assertEquals(result.state, "supported")
    assert(inquiry.premiseObservations.exists(_.observationState == "unobserved"))
    assert(LineContinuationInvestigation.conditionTests(source, prepared, List(inquiry), List(search))(inquiry.question.id)(List(fact -> result)))

  test("a missing recapture or another provider does not prove the replacement used its support"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val throughLoss = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3")
    List(throughLoss, throughLoss :+ "d1f3", throughLoss :+ "g2f3").foreach { line =>
      val source = packet(fen, Nil, line)
      val (prepared, value) = read(source, line)
      assertEquals(LineContinuationInvestigation.readDirectSupportRecaptures(source, value, prepared), Nil)
    }

  test("a provider or capturer moving before a later physical recapture is not stationary direct support use"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val throughLoss = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3")
    val extensions = List(
      List("e4d5", "g8h7", "d5f3"),
      List("g2h2", "f3f6", "h2h1", "f6g6", "e4g6"))
    extensions.foreach { extension =>
      val line = throughLoss ++ extension
      val source = packet(fen, Nil, line)
      val (prepared, value) = read(source, line)
      val loss = value.events.find(_.plyOffset == 5).get
      val recovery = value.events.find(_.plyOffset == line.size - 1).get
      assert(value.captureLinks.exists(link => link.lossEventId == loss.id && link.captureEventId == recovery.id))
      assertEquals(LineContinuationInvestigation.readDirectSupportRecaptures(source, value, prepared), Nil)
    }

  test("an en passant recapture link does not use support of the pawn's different capture square"):
    val fen = "7k/8/8/8/3p4/8/4Pb2/3RQ2K b - - 0 1"
    val line = List("f2e1", "d1e1", "h8h7", "e2e4", "d4e3", "e1e3")
    val source = packet(fen, Nil, line)
    val (prepared, value) = read(source, line)
    val comparison = value.supportComparisons.find(comparison => comparison.kind == "replacement_after_capture" &&
      comparison.beforeActor.square.key == "e1" && comparison.afterActor.square.key == "d1").get
    val pawn = comparison.targets.find(_.originalTarget.square.key == "e2").get.originalTarget
    assertEquals(value.supportAt(4, comparison.afterActor, pawn).state, "present")
    val loss = value.events.find(_.plyOffset == 4).get
    val recovery = value.events.find(_.plyOffset == 5).get
    assert(value.captureLinks.exists(link => link.lossEventId == loss.id && link.captureEventId == recovery.id))
    assertEquals(prepared.pieceAfter(source, value.path, pawn, 4).flatten.map(_.square.key), Some("e4"))
    assertEquals(prepared.pieceAfter(source, value.path, loss.originalActor.get, 5).flatten.map(_.square.key), Some("e3"))
    assertEquals(LineContinuationInvestigation.readDirectSupportRecaptures(source, value, prepared), Nil)
