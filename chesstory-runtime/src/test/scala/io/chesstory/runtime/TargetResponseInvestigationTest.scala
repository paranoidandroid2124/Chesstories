package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ AutomaticTerminal, CanonicalPositionHistory }
import play.api.libs.json.*

/** Exact synthetic positions and supplied legal lines, never engine-quality claims. */
class TargetResponseInvestigationTest extends munit.FunSuite:
  import TargetResponseInvestigation.*

  private val support = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
  private val pinned = "4r2k/8/8/2b5/3N4/8/4R3/4K3 w - - 0 1"

  private def packet(fen: String, actual: String, lines: List[List[String]]): EvidenceBackedJudgmentPacket =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, actual,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    assertEquals(input.suppliedLines.map(_.acceptedMoves), lines)
    EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get

  private def path(source: EvidenceBackedJudgmentPacket, moves: List[String]) =
    ObservedLineProtocol.prepare(source).certifiedPath(source, moves).get

  private def receipt(source: EvidenceBackedJudgmentPacket, root: String, rows: List[(List[String], Int)],
      id: String, restriction: EngineRootRestriction = EngineRootRestriction.Unrestricted,
      depths: List[Int] = Nil): CommentaryInvestigation.EngineCandidates =
    val history = source.assembly.input.positionHistory.extend(List(root)).toOption.get
    val rootPath = ObservedLineProtocol.prepare(source).paths.find(_.moves.headOption.contains(root)).get
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, id,
      EngineWorkPurpose.ResponseComparison, history.initialFen, history.movePrefixUci, history.currentFen,
      restriction, history.currentFen, Nil, EngineSearchLimits(16, 2_000_000, 2_500, rows.size), 3_500,
      Some(EngineSearchAnchor(root, rootPath.ids.head, rootPath.positions(1).id.get)))
    val reported = rows.zipWithIndex.map { case ((moves, cp), index) =>
      ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), depths.lift(index).getOrElse(16))
    }
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(history)).get
    CommentaryInvestigation.EngineCandidates(work, EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)

  private def search(source: EvidenceBackedJudgmentPacket, value: CommentaryInvestigation.EngineCandidates): Search =
    val child = source.assembly.input.positionHistory.extend(List(value.work.searchAnchor.get.rootMove)).toOption.get
    readSearch(source, value, child, ObservedLineProtocol.prepare(source)).get

  // An explicit test request selects two members of the producer's full reply classes.
  private def requestedPair(source: EvidenceBackedJudgmentPacket, root: String, capture: String, decline: String): Question =
    val prepared = ObservedLineProtocol.prepare(source)
    val q = discover(source, root, prepared).find(_.rootMove == root).get
    assert(q.captureMoves.contains(capture))
    assert(q.declineMoves.contains(decline))
    q.copy(referenceMove = capture, alternativeMove = decline)

  private def reading(source: EvidenceBackedJudgmentPacket, q: Question,
      capture: List[String], decline: List[String]): Reading =
    val prepared = ObservedLineProtocol.prepare(source)
    read(source, q, prepared.certifiedPath(source, capture).get.line.ref.id,
      prepared.certifiedPath(source, decline).get.line.ref.id, prepared).get

  private def branches(value: Reading, source: EvidenceBackedJudgmentPacket): List[JsObject] =
    val candidate = value.candidateJson(source)
    assert((candidate \ "state").toOption.isEmpty)
    (candidate \ "detail" \ "branches").as[List[JsObject]]

  test("a recorded alternative to recapturing is complete, but a missing own continuation is not"):
    val decline = List("d2d3", "d8d7", "f3g5")
    for own <- List(Nil, List("f3g5")) do
      val capture = List("d2d3", "d5e4") ++ own
      val source = packet(support, "d2d3", List(capture, decline))
      val q = requestedPair(source, "d2d3", "d5e4", "d8d7")
      val result = reading(source, q, capture, decline)
      val first = branches(result, source).head
      assert((first \ "recaptures" \ "moves").as[List[JsObject]].nonEmpty)
      assertEquals((first \ "observed_recapture" \ "state").as[String], "not_observed")
      assertEquals(result.complete, own.nonEmpty)

  test("owned child answers use the child mover and calibrate reply prose without changing raw answer states"):
    import CommentaryProposal.*
    import PrincipalVariationComparison.QualityBand
    val cases = List(
      (support, "d2d3", "d2d4", CommentaryFact.Piece("white", "king", "h1"),
        List("d5e4", "d3e4"), List("d8d7")),
      ("7k/7p/8/8/8/8/P7/K7 b - - 0 1", "h8g8", "h8g7", CommentaryFact.Piece("black", "king", "h8"),
        List("a1b1"), List("a1b2")))
    for
      (fen, root, rootAlternative, king, referenceLine, alternativeLine) <- cases
      (difference, expectedBand) <- List(10 -> QualityBand.Close, 600 -> QualityBand.Separated)
    do
      val source = packet(fen, root, List(root :: referenceLine, root :: alternativeLine))
      val child = source.assembly.input.positionHistory.extend(List(root)).toOption.get
      assert(child.currentPosition.color != source.assembly.input.positionHistory.currentPosition.color)
      val cp = if child.currentPosition.color.white then difference else -difference
      val supplied = receipt(source, root, List(referenceLine -> 0, alternativeLine -> cp),
        s"child-band-${king.side}-$difference")
      val owned = search(source, supplied)
      val forward = owned.pairAnswer(source, referenceLine.head, alternativeLine.head).get
      val reverse = owned.pairAnswer(source, alternativeLine.head, referenceLine.head).get
      assertEquals(forward.qualityBand, Some(expectedBand))
      assertEquals(reverse.qualityBand, Some(expectedBand))
      assertEquals(forward.evaluationRelation, "better")
      assertEquals(reverse.evaluationRelation, "worse")
      assertEquals(forward.workId, supplied.work.workId)
      assertEquals(reverse.executionKeySha256, forward.executionKeySha256)
      assertEquals(reverse.referenceLineId, forward.alternativeLineId)
      assertEquals(reverse.alternativeLineId, forward.referenceLineId)
      assertEquals(owned.anchor.plyOffset, 1)
      assert((CommentaryInvestigation.answerJson(forward) \ "quality_band").toOption.isEmpty)

      val prepared = ObservedLineProtocol.prepare(source)
      val origin = Source("generator", "C-tactical-counter", Some("inline-band"))
      val context = Fact("tactical_context", CommentaryFact.Path("current", CommentaryFact.Declared(List(root))),
        CommentaryFact.Predicate("presence", subject = Some(king)), CommentaryFact.Ply(1), expected = Some("present"))
      val rootChoice = CompareMoves("contrast_opponent_choice", Nil, root, rootAlternative)
      for (answer, reference, alternative) <- List(
        (forward, referenceLine.head, alternativeLine.head), (reverse, alternativeLine.head, referenceLine.head))
      do
        val comparison = CompareMoves("tactical_strength", List(root), reference, alternative)
        val request = Request(s"reply-prose-${king.side}-$difference-$reference", Origin(origin, origin, origin, origin),
          List(context, rootChoice, comparison))
        assert(validRequests(List(request)))
        val entry = entries(List(request)).head.copy(admission = "generated",
          generatedArms = List(GeneratedArm("current-choice", Nil, root, Some(rootAlternative),
            List(context.id, rootChoice.id), List(context.id, rootChoice.id, comparison.id), origin, Nil)))
        val answered = entry.copy(readings = entry.readings.map(reading =>
          if reading.requirementId == comparison.id then pairReading(comparison, answer) else reading))
        val current = CommentaryFactReader.read(source, prepared, List(answered)).head
        assertEquals(current.readings.find(_.requirementId == context.id).get.state, "supported")
        assertEquals(current.readings.find(_.requirementId == comparison.id).get.state, "answered")
        assertEquals(current.readings.find(_.requirementId == rootChoice.id).get.state, "deferred",
          "a child score and its wording do not answer the different root question")
        val candidates = comparisonCandidates(source, prepared, List(current))
        assertEquals(candidates.size, 1)
        val detail = (candidates.head \ "detail").as[JsObject]
        assertEquals((detail \ "evaluation_relation").toOption, Some(JsNull))
        val text = (detail \ "text").as[String]
        val relative = if answer.evaluationRelation == "better" then "above" else "below"
        assert(text.contains(s"comparison after ${prepared.notation.move(Nil, root)}, ${prepared.notation.move(List(root), alternative)} evaluates $relative ${prepared.notation.move(List(root), reference)}"), text)
        if expectedBand == QualityBand.Close then
          assert(text.contains("The engine evaluations are close"), text)
          assert(text.contains("do not establish a stronger refutation"), text)
          assert(!text.contains("Revise the explanation") && !text.contains("weaker reply"), text)
        else if answer.evaluationRelation == "better" then
          assert(text.contains("Revise the explanation that relies on the weaker reply"), text)
          assert(!text.contains("evaluations are close"), text)
        else
          assert(text.contains("This reply is not rated higher"), text)
          assert(!text.contains("Revise the explanation"), text)
        assert(!text.contains("assessment policy") && !text.contains("close band") &&
          !text.contains("practical separation"), text)

  test("shared immediate identity snapshots preserve captured and unknown states with exact owners"):
    val capture = List("d2d3", "d5e4", "d3e4")
    val source = packet(support, "d2d3", List(capture))
    val prepared = ObservedLineProtocol.prepare(source)
    val full = prepared.certifiedPath(source, capture).get
    val atReply = prepared.identitiesAfter(source, full, 2)
    assert(atReply eq prepared.identitiesAfter(source, full, 2))
    val originalTarget = atReply.find(_._1.square.key == "e4").get._1
    val originalRecapturer = atReply.find(_._1.square.key == "d2").get._1
    val byOriginal = atReply.toMap
    assertEquals(byOriginal(originalTarget), Some(None))
    assert(byOriginal(originalTarget) eq prepared.pieceAfter(source, full, originalTarget, 2))
    assertEquals(byOriginal(originalRecapturer).flatten.map(_.square.key), Some("d3"))
    val afterRecapture = prepared.identitiesAfter(source, full, 3).toMap
    assertEquals(afterRecapture(originalTarget), Some(None))
    assertEquals(afterRecapture(originalRecapturer).flatten.map(_.square.key), Some("e4"))
    val short = prepared.certifiedPath(source, List("d2d3")).get
    assert(prepared.identitiesAfter(source, short, 3).forall(_._2.isEmpty))
    assert(prepared.resourceViews(source) eq prepared.resourceViews(source))
    assert(prepared.attackedRootTargets(source) eq prepared.attackedRootTargets(source))
    val anotherPrepared = ObservedLineProtocol.prepare(source)
    intercept[IllegalArgumentException](prepared.identitiesAfter(source,
      anotherPrepared.certifiedPath(source, capture).get, 2))
    val replacement = packet(support, "d2d3", List(capture))
    intercept[IllegalArgumentException](prepared.identitiesAfter(replacement, full, 2))

  test("a legal seed and a complete observed contrast remain distinct from child search selection"):
    val initial = packet(support, "d2d3", Nil)
    val seed = discover(initial, "d2d3", ObservedLineProtocol.prepare(initial)).head
    assertEquals(seed.referenceMove, seed.captureMoves.sorted.head)
    assertEquals(seed.alternativeMove, seed.declineMoves.sorted.head)
    assertEquals(seed.referenceSelection, Selection())
    assertEquals(seed.alternativeSelection, Selection())
    val capture = List("d2d3", "d5e4", "d3e4")
    val oldDecline = List("d2d3", "d8d7")
    val quietReply = List("d2d3", "d8e8")
    val source = packet(support, "d2d3", List(capture, oldDecline, quietReply))
    val prepared = ObservedLineProtocol.prepare(source)
    val observed = discover(source, "d2d3", prepared).head
    val oldReading = inspectExisting(source, observed, prepared).get
    assert(oldReading.complete)
    assertEquals(observed.alternativeSelection.source, "observed_line")
    val owned = search(source, receipt(source, "d2d3", List(
      (capture.tail, 200), (oldDecline.tail, 300), (quietReply.tail, -400)), "quiet-reply"))
    val selected = discover(source, "d2d3", prepared, List(owned)).head
    assertEquals(selected.id, observed.id)
    assertEquals(selected.alternativeMove, "d8e8") // Synthetic Black score, not the largest White score.
    assertEquals(selected.alternativeSelection, Selection("child_search", Some("quiet-reply"),
      Some(path(source, quietReply).line.ref.id)))
    assertEquals((owned.json \ "root_restriction" \ "kind").as[String], "unrestricted")
    assertEquals((owned.json \ "candidates").as[List[JsObject]].map(row => (row \ "move_uci").as[String]),
      List("d8e8", "d5e4", "d8d7"))
    assertEquals((snapshot(selected, "observed", None, None) \ "alternative_selection" \ "source").as[String], "child_search")
    assertEquals(oldReading.lineIds, List(path(source, capture).line.ref.id, path(source, oldDecline).line.ref.id))
    assert(read(source, oldReading.question, oldReading.lineIds.head, oldReading.lineIds(1), prepared).exists(_.complete))

  test("whole-class search scope outranks a newer partial sample without comparing scores across works"):
    val capture = List("d2d3", "d5e4")
    val olderReply = List("d2d3", "d8e8")
    val newerReply = List("d2d3", "d8d7")
    val source = packet(support, "d2d3", List(capture, olderReply, newerReply))
    val prepared = ObservedLineProtocol.prepare(source)
    val whole = search(source, receipt(source, "d2d3", List((capture.tail, 0), (olderReply.tail, 20)), "whole"))
    val partial = search(source, receipt(source, "d2d3", List((capture.tail, 0), (newerReply.tail, -900)), "partial",
      EngineRootRestriction.Restricted(List("d5e4", "d8d7"))))
    val q = discover(source, "d2d3", prepared, List(whole, partial)).head
    assertEquals(q.referenceSelection.workId, Some("partial")) // The capture class contains just d5e4.
    assertEquals(q.alternativeMove, "d8e8")
    assertEquals(q.alternativeSelection.workId, Some("whole"))
    val newerWhole = search(source, receipt(source, "d2d3", List(
      (capture.tail, 0), (newerReply.tail, 500), (olderReply.tail, 500)), "newer-whole"))
    val tied = discover(source, "d2d3", prepared, List(whole, partial, newerWhole)).head
    assertEquals(tied.alternativeMove, "d8d7") // Equal returned scores use UCI only as a display tie break.
    assertEquals(tied.alternativeSelection.workId, Some("newer-whole"))
    assertEquals(newerWhole.lines.count(_.evaluation.engineLine.exists(_.scoreCp == 500)), 2)

  test("a root PV reply is an explicit hint that a child search can replace"):
    val capture = List("d2d3", "d5e4")
    val observedReply = List("d2d3", "d8e8")
    val rootPvReply = List("d2d3", "d8d7")
    val supplied = packet(support, "d2d3", List(capture, observedReply, rootPvReply))
    val history = supplied.assembly.input.positionHistory
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, "root-hint",
      EngineWorkPurpose.RootSearch, history.initialFen, history.movePrefixUci, history.currentFen,
      EngineRootRestriction.Unrestricted, history.currentFen, Nil, EngineSearchLimits(16, 5_000_000, 5_000, 1), 6_000)
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work,
      List(ReportedEngineLineSuffix(rootPvReply, ReportedWhiteEngineScore.Centipawns(20), 16)), Some(history)).get
    val lines = EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get
    val input = MoveReviewInputAdmission.retainAdmittedLines(supplied.assembly.input, lines, EvaluationSource.RootSearch)
    val source = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val prepared = ObservedLineProtocol.prepare(source)
    val hint = discover(source, "d2d3", prepared).head
    assertEquals(hint.alternativeMove, "d8d7")
    assertEquals(hint.alternativeSelection, Selection("root_pv_hint", lineId = Some(path(source, rootPvReply).line.ref.id)))
    val own = search(source, receipt(source, "d2d3", List((capture.tail, 50), (observedReply.tail, -50)), "child"))
    val chosen = discover(source, "d2d3", prepared, List(own)).head
    assertEquals(chosen.alternativeMove, "d8e8")
    assertEquals(chosen.alternativeSelection.source, "child_search")

  test("a class search covers every legal capture without requiring each searched move to be returned"):
    val fen = "7k/8/8/8/3p4/3b4/4PB2/7K w - - 0 1"
    val bishop = List("e2e4", "d3e4")
    val ep = List("e2e4", "d4e3")
    val decline = List("e2e4", "h8g8")
    val source = packet(fen, "e2e4", List(bishop, ep, decline))
    val prepared = ObservedLineProtocol.prepare(source)
    val q = discover(source, "e2e4", prepared).head
    assertEquals(q.captureMoves.toSet, Set("d3e4", "d4e3"))
    val original = search(source, receipt(source, "e2e4", List((bishop.tail, -100), (decline.tail, -200)), "initial"))
    val narrowed = search(source, receipt(source, "e2e4", List((ep.tail, 100)), "capture-class",
      EngineRootRestriction.Restricted(q.captureMoves)))
    val chosen = discover(source, "e2e4", prepared, List(original, narrowed)).head
    assertEquals(chosen.referenceMove, "d4e3")
    assertEquals(chosen.referenceSelection.workId, Some("capture-class"))
    assertEquals(chosen.alternativeSelection.workId, Some("initial"))
    assertEquals((narrowed.json \ "multi_pv").as[Int], 1)
    assertEquals((narrowed.json \ "root_restriction" \ "moves_uci").as[List[String]].toSet, q.captureMoves.toSet)
    val noCaptureReturned = search(source, receipt(source, "e2e4", List((decline.tail, -300)), "later-survey"))
    val retained = discover(source, "e2e4", prepared, List(original, narrowed, noCaptureReturned)).head
    assertEquals(retained.referenceMove, "d4e3")
    assertEquals(retained.alternativeSelection.workId, Some("later-survey"))

  test("child search reading rejects mixed depths, mismatched history and an unrelated exact packet"):
    val capture = List("d2d3", "d5e4")
    val decline = List("d2d3", "d8d7")
    val source = packet(support, "d2d3", List(capture, decline))
    val prepared = ObservedLineProtocol.prepare(source)
    val child = source.assembly.input.positionHistory.extend(List("d2d3")).toOption.get
    val mismatched = receipt(source, "d2d3", List((capture.tail, 0), (decline.tail, 20)), "unequal", depths = List(16, 17))
    assertEquals(readSearch(source, mismatched, child, prepared), None)
    val valid = receipt(source, "d2d3", List((capture.tail, 0), (decline.tail, 20)), "owned")
    assertEquals(readSearch(source, valid, source.assembly.input.positionHistory, prepared), None)
    val wrongAnchor = valid.copy(work = valid.work.copy(searchAnchor = valid.work.searchAnchor.map(
      _.copy(rootOccurrenceId = "not-the-root-transition"))))
    assertEquals(readSearch(source, wrongAnchor, child, prepared), None)
    val owned = search(source, valid)
    val replacement = packet(support, "d2d3", List(capture, decline))
    assert(!owned.belongsTo(replacement))
    assert(discover(replacement, "d2d3", ObservedLineProtocol.prepare(replacement), List(owned))
      .forall(_.referenceSelection.source != "child_search"))
    intercept[IllegalArgumentException](readSearch(replacement, valid, child, prepared))

  // New in Chess sample, printed pp. 18–19: Anand–Adams 1987.
  // https://www.newinchess.com/media/wysiwyg/product_pdf/9215.pdf
  // The author supplies the position and variations; every numeric report below is invented.
  test("Anand–Adams quiet Qd3 replaces a supplied reply only after its synthetic child report is admitted"):
    val fen = "2rq1rk1/p3bppp/bpn1p3/1P1n4/3P4/P1N2N2/1BB2PPP/R2QR1K1 b - - 0 15"
    val move = "d5c3"
    val immediateRecapture = List(move, "b2c3", "a6b5")
    val delayed = List(move, "d1d3", "g7g6", "b2c3", "d8c7", "b5a6", "c6a5", "c3a5")
    val takeBishop = List(move, "b5a6")
    val initial = packet(fen, move, List(immediateRecapture))
    val initialPrepared = ObservedLineProtocol.prepare(initial)
    val before = discover(initial, move, initialPrepared)
    val targets = before.filter(q => Set("c6", "a6")(q.originalTarget.square.key))
    assertEquals(targets.map(_.originalTarget.square.key).toSet, Set("c6", "a6"))
    assert(targets.forall(q => q.declineMoves.contains("d1d3") && !q.captureMoves.contains("d1d3")))
    assert(targets.forall(q => q.alternativeMove == "b2c3" && q.alternativeSelection.source == "observed_line"))
    assert(initialPrepared.paths.forall(!_.moves.contains("d1d3")))

    val history = initial.assembly.input.positionHistory
    val child = history.extend(List(move)).toOption.get
    val anchor = targets.head
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, "anand-child-survey",
      EngineWorkPurpose.ResponseComparison, child.initialFen, child.movePrefixUci, child.currentFen,
      EngineRootRestriction.Unrestricted, child.currentFen, Nil, EngineSearchLimits(16, 2_000_000, 2_500, 3), 3_500,
      Some(EngineSearchAnchor(move, anchor.rootMoveOccurrenceId, anchor.childPositionId)))
    val rows = List((delayed.tail, 500), (immediateRecapture.tail, -20), (takeBishop.tail, 20))
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, rows.map { (moves, cp) =>
      ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16)
    }, Some(child)).get
    val lines = EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get
    val batch = CommentaryInvestigation.EngineCandidates(work, lines)
    assertEquals(readSearch(initial, batch, child, initialPrepared), None) // Unadmitted paths cannot select a reply.
    val input = MoveReviewInputAdmission.retainLegalContinuations(initial.assembly.input, physical.map(_.continuation))
    val source = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val prepared = ObservedLineProtocol.prepare(source)
    val own = readSearch(source, batch, child, prepared).get
    val after = discover(source, move, prepared, List(own))
      .filter(q => Set("c6", "a6")(q.originalTarget.square.key))
    assertEquals(after.map(_.id).toSet, targets.map(_.id).toSet)
    assert(after.forall(q => q.alternativeMove == "d1d3" && q.alternativeSelection.workId.contains(work.workId)))
    assert(source.assembly.input.evaluationObservations.forall(_.source != EvaluationSource.ResponseComparison))

    val bishop = after.find(_.originalTarget.square.key == "a6").get
    val observation = read(source, bishop, path(source, takeBishop).line.ref.id,
      path(source, delayed).line.ref.id, prepared).get
    assert(observation.complete)
    assertEquals((branches(observation, source)(1) \ "target_state").as[String], "present")
    assertEquals((observation.candidateJson(source) \ "detail" \ "scope").as[String], "observed_lines")
    val replay = source.assembly.lineReplays(path(source, delayed).line.ref)
    assertEquals(OccurrenceResources.followPiece(replay, bishop.originalTarget, 6), Some(None))
    // Complete immediate observations do not explain the delayed capture of that same bishop at ply 6.
    val mate = history.admitToFirstAutomaticTerminal(List(move, "d1d3", "a6b5", "d3h7")).toOption.get
    assertEquals(mate.automaticTerminal, Some(AutomaticTerminal.Checkmate(chess.White)))

  // Same source, printed p. 7: Orton–Shafi. This checks legal move order, not a score claim.
  test("Orton–Shafi Qxg5 has different legal countercaptures after h5 Qh4 on the exact history"):
    val fen = "1rr3k1/4q1pp/p2pp3/4p1N1/BPn2nQ1/P5PP/1P3P2/1R2R1K1 b - - 0 27"
    val move = "e7g5"
    val countercapture = "g3f4"
    val immediate = packet(fen, move, List(List(move, countercapture), List(move, "g4f4")))
    val immediatePrepared = ObservedLineProtocol.prepare(immediate)
    val now = discover(immediate, move, immediatePrepared).find(_.originalTarget.square.key == "f4").get
    assert(now.captureMoves.contains(countercapture))
    assert(immediate.assembly.input.positionHistory.extend(List(move, countercapture)).isRight)

    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
      .extend(List("h7h5", "g4h4")).toOption.get
    val delayedInput = MoveReviewInputAdmission.admitIndependent(history, move,
      suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, List(move, "h4f4")))).get
    assertEquals(delayedInput.suppliedLines.head.acceptedMoves, List(move, "h4f4"))
    val delayed = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(delayedInput)).get
    val delayedPrepared = ObservedLineProtocol.prepare(delayed)
    val later = discover(delayed, move, delayedPrepared).find(_.originalTarget.square.key == "f4").get
    assertEquals(now.rootMove, later.rootMove)
    assertEquals(now.originalTarget, later.originalTarget)
    assertNotEquals(now.rootPositionId, later.rootPositionId)
    assertNotEquals(now.childPositionId, later.childPositionId)
    assertNotEquals(now.id, later.id)
    assert(!later.captureMoves.contains(countercapture))
    assert(later.captureMoves.contains("h4f4"))
    val replies = delayed.resources.find(_.position.id.contains(later.childPositionId)).get.legalRows.get
    assert(!replies.exists(_.move == countercapture))
    assert(history.extend(List(move, countercapture)).isLeft) // Moving Pg3 exposes Kg1 to Qg5.

  test("root pressure connects other-piece support, pawn relocation and attacker capture without inventing a shared purpose"):
    val source = packet(support, "d2d3", Nil)
    val questions = CommentaryInvestigation.discover(source, "d2d3")
    def reasons(move: String) = questions.find(_.alternativeMove == move).toList.flatMap(_.reasons)
    assert(reasons("f1d3").exists(_.kind == CommentaryInvestigation.Kind.TargetSupport))
    assert(reasons("e4e5").exists(_.kind == CommentaryInvestigation.Kind.TargetRelocation))
    assert(reasons("e4d5").exists(_.kind == CommentaryInvestigation.Kind.AttackerCapture))
    assert(!reasons("d1a1").exists(_.kind == CommentaryInvestigation.Kind.TargetSupport))
    val view = source.resources.find(_.position == source.assembly.playedTransition.get.from).get
    val pressureIds = view.attackedTargets(chess.White).flatMap(_._2.map(_.semanticId)).toSet
    val targeted = questions.flatMap(_.reasons).filter(_.sourceName == "attacked_target")
    assert(targeted.nonEmpty)
    assert(targeted.forall(r => r.target.exists(_.square.key == "e4") && r.pressureSourceIds.nonEmpty &&
      r.pressureSourceIds.forall(pressureIds)))
    assertEquals(CommentaryInvestigation.discover(source, "d2d3"), questions)

  test("conditional recapture follows the replacement pawn or bishop and records different later support"):
    val pawnCapture = List("d2d3", "d5e4", "d3e4")
    val pawnDecline = List("d2d3", "d8d7")
    val bishopCapture = List("f1d3", "d5e4", "d3e4")
    val bishopDecline = List("f1d3", "d8d7")
    val source = packet(support, "d2d3", List(pawnCapture, pawnDecline, bishopCapture, bishopDecline))
    val pawnQ = requestedPair(source, "d2d3", "d5e4", "d8d7")
    val bishopQ = requestedPair(source, "f1d3", "d5e4", "d8d7")
    val values = List(reading(source, pawnQ, pawnCapture, pawnDecline),
      reading(source, bishopQ, bishopCapture, bishopDecline))
    assert(values.forall(_.complete))
    val captureRows = values.map(branches(_, source).head)
    captureRows.foreach { row =>
      assertEquals((row \ "target_state").as[String], "captured")
      assertEquals((row \ "target").get, JsNull)
      assertEquals((row \ "recaptures" \ "moves").as[List[JsObject]].map(r => (r \ "move_uci").as[String]), List("d3e4"))
    }
    assertEquals(captureRows.map(row => (row \ "observed_recapture" \ "actor" \ "original" \ "square").as[String]),
      List("d2", "f1"))
    assertEquals(captureRows.map(row => (row \ "observed_recapture" \ "actor" \ "current" \ "role").as[String]),
      List("pawn", "bishop"))
    val targets = captureRows.map(row => (row \ "observed_recapture" \ "supported_targets").as[List[JsObject]]
      .map(target => (target \ "original" \ "square").as[String]))
    assert(!targets.head.contains("f3"))
    assert(targets(1).contains("f3"))
    values.foreach { value =>
      assertEquals((branches(value, source)(1) \ "target_state").as[String], "present")
      assert(hasExistingContrast(source, value.question, ObservedLineProtocol.prepare(source)))
    }
    val replay = source.assembly.lineReplays(path(source, pawnCapture).line.ref)
    assertEquals(OccurrenceResources.followPiece(replay, pawnQ.originalTarget, 3), Some(None))
    assertEquals((snapshot(pawnQ, "observed", None, None) \ "answer").get, JsNull)

  test("complete reply catalogs do not complete an unobserved recapture's later function"):
    val capture = List("d2d3", "d5e4")
    val decline = List("d2d3", "d8d7")
    val source = packet(support, "d2d3", List(capture, decline))
    val q = requestedPair(source, "d2d3", "d5e4", "d8d7")
    val value = reading(source, q, capture, decline)
    assert(!value.complete)
    assertEquals((branches(value, source).head \ "recaptures" \ "state").as[String], "complete")
    assertEquals((branches(value, source).head \ "observed_recapture" \ "state").as[String], "not_observed")
    assert(!hasExistingContrast(source, q, ObservedLineProtocol.prepare(source)))
    val retained = inspectExisting(source, q, ObservedLineProtocol.prepare(source)).get
    assert(!retained.complete)
    val retainedCapture = branches(retained, source).head
    assertEquals((retainedCapture \ "target_state").as[String], "captured")
    assertEquals((retainedCapture \ "providers" \ "state").as[String], "not_applicable")
    assertEquals((retainedCapture \ "recaptures" \ "state").as[String], "complete")
    assert((retainedCapture \ "recaptures" \ "moves").as[List[JsObject]]
      .exists(row => (row \ "move_uci").as[String] == "d3e4"))
    assertEquals((retainedCapture \ "observed_recapture" \ "state").as[String], "not_observed")

  test("existing paths prefer a completed branch without replacing a fixed partial read or mixing paths"):
    val shortCapture = List("d2d3", "d5e4")
    val fullCapture = shortCapture :+ "d3e4"
    val decline = List("d2d3", "d8d7")
    val longerDecline = decline :+ "h1g1"
    val source = packet(support, "d2d3", List(shortCapture, decline, fullCapture, longerDecline))
    val q = requestedPair(source, "d2d3", "d5e4", "d8d7")
    val prepared = ObservedLineProtocol.prepare(source)
    val fixed = reading(source, q, shortCapture, decline)
    assert(!fixed.complete)
    val existing = inspectExisting(source, q, prepared).get
    assert(existing.complete)
    assertEquals((branches(existing, source).head \ "line_id").as[String],
      prepared.certifiedPath(source, fullCapture).get.line.ref.id)
    assertEquals((branches(existing, source).head \ "observed_recapture" \ "actor" \ "original" \ "square").as[String], "d2")
    assertEquals((branches(fixed, source).head \ "observed_recapture" \ "state").as[String], "not_observed")

  test("a supporting pinned rook is not a legal recapture and an illegal support proposal is absent"):
    val capture = List("e2e4", "c5d4")
    val decline = List("e2e4", "h8g8")
    val source = packet(pinned, "e2e4", List(capture, decline))
    val view = source.resources.find(_.position == source.assembly.playedTransition.get.from).get
    val target = view.occupantAt(EvidenceSquare("d4")).get
    val supportMoves = view.supportCandidates(target).flatMap(_.detail match
      case RelationWitnessDetail.LegalMove(_, _, _, _, move, _) => Some(move)
      case _ => None)
    assert(supportMoves.contains("e2e4"))
    assert(!supportMoves.contains("e2d2"))
    val q = requestedPair(source, "e2e4", "c5d4", "h8g8")
    val value = reading(source, q, capture, decline)
    assert(value.complete)
    val row = branches(value, source).head
    assertEquals((row \ "recaptures" \ "state").as[String], "complete")
    assertEquals((row \ "recaptures" \ "moves").as[List[JsObject]], Nil)
    assertEquals((row \ "observed_recapture" \ "state").as[String], "not_observed")

  test("en passant partitions by the physical captured square and preserves the recapturer's identity"):
    val fen = "7k/8/8/8/3p4/3b4/4PB2/7K w - - 0 1"
    val capture = List("e2e4", "d4e3", "f2e3")
    val decline = List("e2e4", "h8g8")
    val source = packet(fen, "e2e4", List(capture, decline))
    val q = requestedPair(source, "e2e4", "d4e3", "h8g8")
    assertEquals(q.target.square.key, "e4")
    assertEquals(q.captureMoves.toSet, Set("d4e3", "d3e4"))
    val value = reading(source, q, capture, decline)
    assert(value.complete)
    val row = branches(value, source).head
    assertEquals((row \ "target_state").as[String], "captured")
    assertEquals((row \ "capturer" \ "square").as[String], "e3")
    assertEquals((row \ "observed_recapture" \ "actor" \ "original" \ "square").as[String], "f2")
    assertEquals((row \ "recaptures" \ "moves").as[List[JsObject]].map(r => (r \ "move_uci").as[String]), List("f2e3"))

  List(
    ("white", "7k/8/8/8/3p4/8/4PB2/7K w - - 0 1",
      List("e2e4", "d4e3", "f2e3"), List("e2e4", "h8g8")),
    ("black", "7k/4pb2/8/3P4/8/8/8/7K b - - 0 1",
      List("e7e5", "d5e6", "f7e6"), List("e7e5", "h1g1"))
  ).foreach { (side, fen, capture, decline) =>
    test(s"$side en passant alone discovers and reads the exact physical victim without geometric pressure"):
      val source = packet(fen, capture.head, List(capture, decline))
      val prepared = ObservedLineProtocol.prepare(source)
      val root = source.assembly.playedTransition.get
      val capturePath = prepared.certifiedPath(source, capture).get
      val declinePath = prepared.certifiedPath(source, decline).get
      val views = prepared.resourceViews(source)
      val child = views(capturePath.positions(1).id.get)
      val original = views(root.from.id.get).occupantAt(EvidenceSquare(capture.head.take(2))).get
      val target = child.occupantAt(EvidenceSquare(capture.head.drop(2))).get
      val replies = child.legalRows.get
      val ep = replies.find(_.move == capture(1)).get
      assert(ep.captures(target))
      assertNotEquals(ep.to, target.square)
      assertEquals(prepared.attackedRootTargets(source).find(_._1 == original), None)
      assertEquals(child.attackedTargets(original.side).find(_._1 == target), None)

      // The independently assembled owned question also diagnoses the reader when discovery is absent.
      val owned = Question("ep-only-question", root.evidence.id, root.from.id.get, capture.head,
        capturePath.ids.head, child.position.id.get, original, target, Nil,
        List(capture(1)), replies.filterNot(_.captures(target)).map(_.move),
        replies.map(row => row.move -> row.sourceId).toMap, capture(1), decline(1))
      val discovered = discover(source, capture.head, prepared).find(_.originalTarget == original)
      val direct = read(source, owned, capturePath.line.ref.id, declinePath.line.ref.id, prepared)
      val existing = inspectExisting(source, owned, prepared)
      assertEquals((discovered.nonEmpty, direct.nonEmpty, existing.nonEmpty), (true, true, true))
      assert(direct.exists(_.complete))
      assert(existing.exists(_.complete))
      val q = discovered.get
      assertEquals(q.target, target)
      assertEquals(q.pressureSourceIds, Nil)
      assertEquals(q.captureMoves, List(capture(1)))
      assertEquals(q.referenceMove, capture(1))
      assertEquals(q.alternativeMove, decline(1))
      val value = read(source, q, capturePath.line.ref.id, declinePath.line.ref.id, prepared).get
      assert(value.complete)
      assertEquals(branches(value, source).map(row => (row \ "target_state").as[String]),
        List("captured", "present"))
      val taken = branches(value, source).head
      assertEquals((taken \ "capturer" \ "square").as[String], capture(1).drop(2))
      assertEquals((taken \ "observed_recapture" \ "actor" \ "original" \ "square").as[String],
        capture(2).take(2))
      assertEquals((taken \ "recaptures" \ "moves").as[List[JsObject]].map(row =>
        (row \ "move_uci").as[String]), List(capture(2)))
      assert(inspectExisting(source, q, prepared).exists(_.complete))

      val wire = RuntimeProtocol.observationsJson(source,
        responseInquiries = List(CommentaryResponseInquiry(q, reading = Some(value))), prepared = Some(prepared))
      val inquiry = (wire \ "selection" \ "response_inquiries").as[List[JsObject]].head
      assertEquals((inquiry \ "pressure_source_ids").as[List[String]], Nil)
      assertEquals((inquiry \ "move_source_ids" \ capture(1)).as[String], ep.sourceId)
      val delivered = (wire \ "selection" \ "candidates").as[List[JsObject]]
        .find(row => (row \ "id").as[String] == value.id).get
      assertEquals((delivered \ "detail" \ "branches").as[List[JsObject]], branches(value, source))

      val recapturer = views(root.from.id.get).occupantAt(EvidenceSquare(capture(2).take(2))).get
      val unrelated = child.completeFacts(RelationFactKind.GeometricControl).get.head.semanticId
      val invalid = List(
        q.copy(pressureSourceIds = List(ep.sourceId)),
        q.copy(pressureSourceIds = List(unrelated)),
        q.copy(rootOccurrenceId = "foreign-root"),
        q.copy(rootMoveOccurrenceId = "foreign-root-move"),
        q.copy(rootPositionId = q.childPositionId),
        q.copy(childPositionId = capturePath.positions(2).id.get),
        q.copy(originalTarget = recapturer),
        q.copy(target = target.copy(square = ep.to)),
        q.copy(captureMoves = Nil),
        q.copy(declineMoves = q.declineMoves.tail),
        q.copy(referenceMove = decline(1), alternativeMove = capture(1)),
        q.copy(moveSourceIds = q.moveSourceIds - capture(1)),
        q.copy(moveSourceIds = q.moveSourceIds.updated(capture(1), unrelated)))
      invalid.foreach { altered =>
        assertEquals(read(source, altered, capturePath.line.ref.id, declinePath.line.ref.id, prepared), None)
        assertEquals(inspectExisting(source, altered, prepared), None)
      }
  }

  test("a pinned pawn's illegal en passant does not seed a pressure-free victim"):
    val fen = "3k4/8/8/8/3p4/8/4PB2/3R3K w - - 0 1"
    val source = packet(fen, "e2e4", List(List("e2e4", "d8c8")))
    val prepared = ObservedLineProtocol.prepare(source)
    val child = prepared.resourceViews(source)(prepared.certifiedPath(source, List("e2e4")).get.positions(1).id.get)
    val target = child.occupantAt(EvidenceSquare("e4")).get
    assertEquals(child.attackedTargets(chess.White).find(_._1 == target), None)
    assert(!child.legalRows.get.exists(_.move == "d4e3"))
    assertEquals(discover(source, "e2e4", prepared), Nil)

  List(
    ("moved target", "7k/8/8/8/3p4/3b4/4PB2/7K w - - 0 1",
      List("e2e4", "d4e3", "f2e3"), List("e2e4", "h8g8"), "e4", 2),
    ("newly exposed target", "7k/8/8/8/1b6/2N5/8/4Q2K w - - 0 1",
      List("c3e4", "b4e1"), List("c3e4", "h8g8"), "e1", 1),
    ("unchanged pressure", support,
      List("d2d3", "d5e4", "d3e4"), List("d2d3", "d8d7"), "e4", 1)
  ).foreach { (name, fen, capture, decline, target, pressureCount) =>
    test(s"$name delivers the discovered response contrast and rejects incomplete or foreign pressure scope"):
      val source = packet(fen, capture.head, List(capture, decline))
      val prepared = ObservedLineProtocol.prepare(source)
      val q = discover(source, capture.head, prepared).find(_.target.square.key == target).get
      assertEquals(q.referenceMove, capture(1))
      assertEquals(q.alternativeMove, decline(1))
      assertEquals(q.pressureSourceIds.size, pressureCount)
      val captureId = prepared.certifiedPath(source, capture).get.line.ref.id
      val declineId = prepared.certifiedPath(source, decline).get.line.ref.id
      val value = read(source, q, captureId, declineId, prepared).get
      assert(value.complete)
      assertEquals(branches(value, source).map(row => (row \ "target_state").as[String]),
        List("captured", "present"))
      val existing = inspectExisting(source, q, prepared).get
      assert(existing.complete)
      assertEquals(existing.lineIds, value.lineIds)
      val wire = RuntimeProtocol.observationsJson(source,
        responseInquiries = List(CommentaryResponseInquiry(q, reading = Some(existing))), prepared = Some(prepared))
      val delivered = (wire \ "selection" \ "candidates").as[List[JsObject]]
        .find(row => (row \ "id").as[String] == existing.id).get
      assertEquals((delivered \ "detail" \ "response_inquiry_id").as[String], q.id)
      assertEquals((delivered \ "line_ids").as[List[String]], List(captureId, declineId))
      assertEquals((delivered \ "detail" \ "branches").as[List[JsObject]], branches(existing, source))

      val unrelated = prepared.resourceViews(source)(q.childPositionId)
        .completeFacts(RelationFactKind.GeometricControl).get
        .find(fact => !q.pressureSourceIds.contains(fact.semanticId)).get.semanticId
      val invalid = List(
        q.copy(pressureSourceIds = Nil),
        q.copy(pressureSourceIds = (q.pressureSourceIds :+ unrelated).sorted),
        q.copy(childPositionId = q.rootPositionId),
        q.copy(rootMoveOccurrenceId = "foreign-root-occurrence"),
        q.copy(target = q.target.copy(square = EvidenceSquare("a1"))),
        q.copy(captureMoves = Nil)) ++ q.pressureSourceIds.map(id =>
          q.copy(pressureSourceIds = q.pressureSourceIds.filterNot(_ == id)))
      invalid.distinct.foreach { altered =>
        assertEquals(read(source, altered, captureId, declineId, prepared), None)
        assertEquals(inspectExisting(source, altered, prepared), None)
      }
  }

  test("ordinary development without a positive target contact does not acquire a response question"):
    val fen = "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
    val source = packet(fen, "f1b5", List(List("f1b5", "a7a6"), List("f1c4", "g8f6")))
    assertEquals(discover(source, "f1c4", ObservedLineProtocol.prepare(source)), Nil)
    assert(!CommentaryInvestigation.discover(source, "f1c4").flatMap(_.reasons).exists(_.sourceName == "attacked_target"))

  test("same-FEN endpoints cannot replace an exact response path and forged child target catalogs are rejected"):
    val capture = List("e2e4", "c5d4")
    val decline = List("e2e4", "h8g8", "d4f3", "g8h8")
    val transposed = List("d4f3", "h8g8", "e2e4", "g8h8")
    val source = packet(pinned, "e2e4", List(capture, decline, transposed))
    val prepared = ObservedLineProtocol.prepare(source)
    val cap = prepared.certifiedPath(source, capture).get
    val dec = prepared.certifiedPath(source, decline).get
    val other = prepared.certifiedPath(source, transposed).get
    assertEquals(dec.positions.last.fen, other.positions.last.fen)
    assertNotEquals(dec.positions.last.id, other.positions.last.id)
    val q = requestedPair(source, "e2e4", "c5d4", "h8g8")
    val value = read(source, q, cap.line.ref.id, dec.line.ref.id, prepared).get
    assertEquals(read(source, q, cap.line.ref.id, other.line.ref.id, prepared), None)
    assertEquals(read(source, q.copy(captureMoves = Nil), cap.line.ref.id, dec.line.ref.id, prepared), None)
    assertEquals(read(source, q.copy(target = q.target.copy(square = EvidenceSquare("f3"))), cap.line.ref.id, dec.line.ref.id, prepared), None)
    val replacement = packet(pinned, "e2e4", List(capture, decline, transposed))
    assert(!value.belongsTo(replacement))
    intercept[IllegalArgumentException](value.candidateJson(replacement))
    intercept[IllegalArgumentException](read(replacement, q, cap.line.ref.id, dec.line.ref.id, prepared))

  test("missing after-reply resources remain unknown rather than empty legal recapture sets"):
    val capture = List("d2d3", "d5e4", "d3e4")
    val decline = List("d2d3", "d8d7")
    val source = packet(support, "d2d3", List(capture, decline))
    val missingPosition = path(source, capture).positions(2)
    val ctx = source.assembly.copy(positionAnalyses = source.assembly.positionAnalyses - missingPosition)
    val missing = EvidenceBackedJudgmentPacket.fromAssembly(ctx).get
    val q = requestedPair(missing, "d2d3", "d5e4", "d8d7")
    val value = reading(missing, q, capture, decline)
    assert(!value.complete)
    val captureBranch = branches(value, missing).head
    assertEquals((captureBranch \ "target_state").as[String], "captured")
    assertEquals((captureBranch \ "providers" \ "state").as[String], "not_applicable")
    assertEquals((captureBranch \ "recaptures" \ "state").as[String], "unknown")
    assertEquals((captureBranch \ "recaptures" \ "moves").as[List[JsObject]], Nil)
    assertEquals((captureBranch \ "observed_recapture" \ "state").as[String], "unknown")
    assert(!hasExistingContrast(missing, q, ObservedLineProtocol.prepare(missing)))

  test("related capture facts survive an incomplete or branching decline without completing that decline"):
    val pawn = List("d2d3", "d5e4", "d3e4")
    val bishop = List("f1d3", "d5e4", "d3e4")
    val decline = List("d2d3", "d8d7")
    val bishopDecline = List("f1d3", "d8d7")
    val first = decline :+ "h1g1"
    val second = decline :+ "e4e5"
    val original = packet(support, pawn.head, List(pawn, pawn.take(2), bishop, bishop.take(2),
      decline, bishopDecline, first, second))
    val missing = EvidenceBackedJudgmentPacket.fromAssembly(original.assembly.copy(
      positionAnalyses = original.assembly.positionAnalyses - path(original, decline).positions(2))).get
    List(original -> true, missing -> false).foreach { (source, complete) =>
      val prepared = ObservedLineProtocol.prepare(source)
      val q = requestedPair(source, pawn.head, "d5e4", "d8d7")
      val bq = requestedPair(source, bishop.head, "d5e4", "d8d7")
      val current = reading(source, q, pawn.take(2), decline)
      val retained = List(first, second).map(reading(source, q, pawn, _))
      assert(retained.forall(_.complete == complete))
      assert(retained.forall(value => (branches(value, source).head \ "observed_recapture" \ "state")
        .as[String] == "complete"))
      val otherCurrent = reading(source, bq, bishop.take(2), bishopDecline)
      val otherFull = reading(source, bq, bishop, bishopDecline)
      val wire = RuntimeProtocol.observationsJson(source, responseInquiries = List(
        CommentaryResponseInquiry(q, reading = Some(current), retainedReadings = retained),
        CommentaryResponseInquiry(bq, reading = Some(otherCurrent), retainedReadings = List(otherFull))),
        prepared = Some(prepared))
      val selection = (wire \ "selection").as[JsObject]
      assertEquals((selection \ "version").as[Int], 2)
      val candidates = (selection \ "candidates").as[List[JsObject]]
      assert(candidates.forall(candidate => (candidate \ "state").toOption.isEmpty))
      assert((selection \ "scenes").as[List[JsObject]].forall(scene => (scene \ "state").toOption.isEmpty))
      val pairIds = Set(path(source, pawn).line.ref.id, path(source, bishop).line.ref.id)
      assert(candidates.exists(candidate => (candidate \ "line_ids").as[List[String]].toSet == pairIds &&
        (candidate \ "detail" \ "differences").asOpt[List[JsObject]].exists(_.exists(row =>
          (row \ "kind").as[String] == "replacement_support" &&
            (row \ "original_target" \ "square").as[String] == "f3"))))
      retained.foreach { value =>
        val candidate = candidates.find(candidate => (candidate \ "id").as[String] == value.id).get
        assertEquals((candidate \ "line_ids").as[List[String]], value.lineIds)
        val deliveredBranches = (candidate \ "detail" \ "branches").as[List[JsObject]]
        assertEquals(deliveredBranches.map(row => (row \ "line_id").as[String]), value.lineIds)
        assertEquals((deliveredBranches.head \ "observed_recapture" \ "state").as[String], "complete")
        assertEquals((deliveredBranches.head \ "observed_recapture" \ "actor" \ "original" \ "square").as[String], "d2")
        val deliveredDecline = deliveredBranches(1)
        assertEquals((deliveredDecline \ "target_state").as[String], "present")
        assertEquals((deliveredDecline \ "providers" \ "state").as[String], if complete then "complete" else "unknown")
        assertEquals((deliveredDecline \ "recaptures" \ "state").as[String], "not_applicable")
      }
      // Incomparable decline continuations remain separate paths and do not
      // become an arbitrary additional comparison with the other root arm.
      val declineIds = Set(path(source, first).line.ref.id, path(source, second).line.ref.id)
      val otherDeclineId = path(source, bishopDecline).line.ref.id
      assert(!candidates.exists(candidate =>
        (candidate \ "detail" \ "type").asOpt[String].contains("line_continuation_comparison") &&
          (candidate \ "line_ids").as[List[String]].contains(otherDeclineId) &&
          (candidate \ "line_ids").as[List[String]].exists(declineIds)))
    }
