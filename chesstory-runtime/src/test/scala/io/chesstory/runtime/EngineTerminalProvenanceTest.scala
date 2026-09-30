package io.chesstory.runtime

import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, CanonicalPositionHistory }
import lila.chessjudgment.analysis.assembly.{ MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput }
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

class EngineTerminalProvenanceTest extends munit.FunSuite:

  private def summary(input: AdmittedMoveReviewInput, moves: List[String]): JsObject =
    val ctx = MoveReviewAssembler.assembleOccurrences(input)
    val line = ctx.legalLines.find(line => ctx.evidenceGraph.record(line.evidence).exists(_.payload match
      case fact: LineFactEvidence => fact.lineReplayMoves == moves
      case _ => false)).get
    val result = ObservedLineProtocol.summaryJson(ctx, line).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(ctx).get
    val path = ObservedLineProtocol.prepare(packet).certifiedPath(packet, moves).get
    assertEquals(ObservedLineProtocol.summaryJson(path), Some(result))
    assertEquals((result \ "ending" \ "ply_offset").as[Int], moves.size - 1)
    assertEquals((result \ "ending" \ "occurrence_id").as[String], ctx.lineTransitionIds(line.ref).last)
    assertEquals((result \ "premise_ids").as[List[String]], List(line.evidence.id))
    result

  test("a terminal search suffix does not make its multi-ply commentary prefix an exact draw"):
    val fen = "8/k1P5/2K5/8/8/8/8/8 w - - 0 1"
    val prefix = List("c7c8r", "a7a6", "c8b8", "a6a5", "b8b4")
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
      .extend(prefix).toOption.get
    val work = IssuedEngineWork.create(
      CommentaryEngineProfile.ServerAdmitted,
      "terminal-suffix",
      EngineWorkPurpose.RootSearch,
      fen,
      prefix,
      history.currentFen,
      EngineRootRestriction.Unrestricted,
      fen,
      prefix,
      EngineSearchLimits(16, 5_000_000, 5_000, 1),
      6_000
    )
    val report = ReportedEngineLineSuffix(List("a5b4"), ReportedWhiteEngineScore.Centipawns(0), 16)
    for moves <- List(List("a5b4"), List("a5b4", "c6d6")) do
      val physical = EngineLineAdmission.admitReportedLineSuffixes(work,
        List(report.copy(moves = moves, whiteScore = ReportedWhiteEngineScore.Centipawns(20)))).get
      assertEquals(physical.head.continuation.moves, List("a5b4"))
      assertEquals(physical.head.continuation.automaticTerminal, Some(AutomaticTerminal.InsufficientMaterial))
      assertEquals(EngineLineAdmission.bindAdmittedLineSuffixes(work, physical), None,
        "neither the commentary prefix nor the raw tail can excuse a nonzero immediate draw score")
    assertEquals(EngineLineAdmission.admitReportedLineSuffixes(work,
      List(report.copy(moves = List("a5b4", "c6c8")))), None,
      "a terminal endpoint does not excuse an illegal reported tail")
    assertEquals(EngineLineAdmission.admitReportedLineSuffixes(work,
      List(report.copy(moves = List("a5b4", "c6d6")))).get.head.continuation.moves, List("a5b4"))
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, List(report)).get
    assertEquals(physical.head.continuation.moves, List("a5b4"))
    val bound = EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get.head
    assertEquals(bound.automaticTerminal, Some(AutomaticTerminal.InsufficientMaterial))
    assertEquals(bound.evaluation.moves, prefix :+ "a5b4")
    assert(bound.evaluation.isInstanceOf[CandidateLineEvaluation.EngineSearch])
    assertEquals(bound.evaluation.engineLine.map(line => (line.scoreCp, line.depth)), Some((0, 16)))

  test("a one-move automatic terminal still supplies an exact outcome"):
    val fen = "8/k1P5/2K5/8/8/8/8/8 w - - 0 1"
    val work = IssuedEngineWork.create(
      CommentaryEngineProfile.ServerAdmitted, "terminal-root", EngineWorkPurpose.RootSearch,
      fen, Nil, fen, EngineRootRestriction.Unrestricted, fen, Nil,
      EngineSearchLimits(16, 5_000_000, 5_000, 1), 6_000
    )
    def bind(score: Int) =
      EngineLineAdmission.admitReportedLineSuffixes(work,
        List(ReportedEngineLineSuffix(List("c7c8q"), ReportedWhiteEngineScore.Centipawns(score), 16)))
        .flatMap(EngineLineAdmission.bindAdmittedLineSuffixes(work, _))
    assertEquals(bind(0).get.map(_.evaluation), List(
      CandidateLineEvaluation.ExactAutomaticTerminal(List("c7c8q"), AutomaticTerminal.Stalemate)))
    assertEquals(bind(20), None)

  test("a later drawn PV retains its nonzero engine estimate without certifying the root as drawn"):
    // Public Ding-Gukesh 2024 game 14, before 55.Rf2. This is the actual
    // returned alternative PV and score; its last capture leaves K+B versus K.
    val fen = "B7/8/4b3/4kp2/5Rp1/6P1/1r6/6K1 w - - 16 55"
    val moves = "a8g2 e6a2 f4f2 b2b1 f2f1 b1f1 g2f1 a2c4 f1c4 f5f4 g1f2 f4g3 f2g3 e5e4 g3g4"
      .split(" ").toList
    val work = IssuedEngineWork.create(
      CommentaryEngineProfile.ServerAdmitted, "later-draw", EngineWorkPurpose.RootSearch,
      fen, Nil, fen, EngineRootRestriction.Unrestricted, fen, Nil,
      EngineSearchLimits(16, 2_000_000, 2_500, 1), 3_500
    )
    def bind(score: ReportedWhiteEngineScore) =
      EngineLineAdmission.admitReportedLineSuffixes(work,
        List(ReportedEngineLineSuffix(moves, score, 16)))
        .flatMap(EngineLineAdmission.bindAdmittedLineSuffixes(work, _))
    val line = bind(ReportedWhiteEngineScore.Centipawns(-33)).get.head
    assertEquals(line.automaticTerminal, Some(AutomaticTerminal.InsufficientMaterial))
    assert(line.evaluation.isInstanceOf[CandidateLineEvaluation.EngineSearch])
    assertEquals(line.evaluation.engineLine.map(value => (value.moves, value.scoreCp, value.depth)),
      Some((moves, -33, 16)))
    assertEquals(bind(ReportedWhiteEngineScore.Mate(-8)), None,
      "an explicit mate score still contradicts a drawn endpoint")
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.retainAdmittedLines(
      MoveReviewInputAdmission.admitIndependent(history, moves.head).get,
      List(line), EvaluationSource.RootSearch)
    assertEquals((summary(input, moves) \ "ending" \ "kind").as[String], "insufficient_material")
    assertEquals(input.evaluationObservations.head.evaluation, line.evaluation)
    assertEquals((summary(input, moves.take(1)) \ "ending" \ "kind").as[String], "line_end",
      "the conditional endpoint must not migrate to the same first move")

  test("unscored supplied lines retain all observed automatic endings at the exact first terminal"):
    val mateFen = "7k/5K2/8/6Q1/8/8/8/8 w - - 0 1"
    val materialFen = "7k/8/8/8/K7/1p6/8/8 w - - 0 1"
    val clockFen = "7k/8/8/8/8/8/8/R6K w - - 149 76"
    val initial = chess.variant.Standard.initialFen.value
    val cycle = List("g1f3", "g8f6", "f3g1", "f6g8")
    val cases = List(
      (mateFen, List("g5g7"), "checkmate"),
      (mateFen, List("g5g6"), "stalemate"),
      (materialFen, List("a4b3"), "insufficient_material"),
      (clockFen, List("a1a2"), "seventy_five_move_rule"),
      (initial, List.fill(4)(cycle).flatten, "fivefold_repetition"))
    for (fen, moves, kind) <- cases do
      val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
      val input = MoveReviewInputAdmission.admitIndependent(history, moves.head,
        suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Analysis, moves :+ "a1a2"))).get
      assertEquals(input.suppliedLines.head.acceptedMoves, moves)
      assertEquals(input.suppliedLines.head.stop, Some(CommentaryLineStop.AutomaticTerminal))
      assertEquals(input.evaluationObservations, Nil)
      assertEquals((summary(input, moves) \ "ending" \ "kind").as[String], kind)

  test("draw claims and the same FEN without the repetition history are not terminal endpoints"):
    val initial = chess.variant.Standard.initialFen.value
    val cycle = List("g1f3", "g8f6", "f3g1", "f6g8")
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
    val moves = List.fill(4)(cycle).flatten
    val before = history.extend(moves.dropRight(1)).toOption.get
    val known = MoveReviewInputAdmission.admitIndependent(before, moves.last).get
    val unknown = CanonicalPositionHistory.from(before.currentFen, Nil, before.currentFen).toOption.get
    val sameFen = MoveReviewInputAdmission.admitIndependent(unknown, moves.last).get
    assertEquals((summary(known, List(moves.last)) \ "ending" \ "kind").as[String], "fivefold_repetition")
    assertEquals((summary(sameFen, List(moves.last)) \ "ending" \ "kind").as[String], "line_end")
    val otherHistory = before.admitToFirstAutomaticTerminal(List(moves.last)).toOption.get
    assertEquals(MoveReviewInputAdmission.retainLegalContinuations(sameFen, List(otherHistory)), sameFen,
      "a terminal from the same FEN with another repetition history is not this line's fact")
    val threefold = moves.take(8)
    val claims = MoveReviewInputAdmission.admitIndependent(history, moves.head, List(threefold)).get
    assertEquals((summary(claims, threefold) \ "ending" \ "kind").as[String], "line_end")
    val clockFen = "7k/8/8/8/8/8/8/R6K w - - 99 51"
    val clock = CanonicalPositionHistory.from(clockFen, Nil, clockFen).toOption.get
    val fifty = MoveReviewInputAdmission.admitIndependent(clock, "a1a2").get
    assertEquals((summary(fifty, List("a1a2")) \ "ending" \ "kind").as[String], "line_end")

  test("retaining an exact legal continuation keeps its terminal even when its score is rejected"):
    val fen = "8/k1P5/2K5/8/8/8/8/8 w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val moves = List("c7c8r", "a7a6", "c8a8")
    val rejectedScore = lila.chessjudgment.model.line.EngineLine(moves, -10000, Some(-2), 16)
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen, moves.head, List(rejectedScore))).get
    assertEquals(input.evaluationObservations, Nil)
    assertEquals((summary(input, moves) \ "ending" \ "kind").as[String], "checkmate")
    val draw = List("c7c8r", "a7a6", "c8b8", "a6a5", "b8b4", "a5b4")
    val root = MoveReviewInputAdmission.admitIndependent(history, draw.head).get
    val continuation = history.admitToFirstAutomaticTerminal(draw).toOption.get
    val retained = MoveReviewInputAdmission.retainLegalContinuations(root, List(continuation))
    assertEquals((summary(retained, draw) \ "ending" \ "kind").as[String], "insufficient_material")

  test("nonzero scores for the two immediate bishop draws preserve only physical observations"):
    val cases = List(
      ("8/6B1/1k5p/8/3K4/8/8/8 w - - 0 58", "g7h6", -15),
      ("6B1/8/7K/8/8/2k5/p7/8 w - - 0 60", "g8a2", -25))
    for (fen, move, cp) <- cases do
      val work = IssuedEngineWork.create(
        CommentaryEngineProfile.ServerAdmitted, s"bishop-draw-$move", EngineWorkPurpose.RootSearch,
        fen, Nil, fen, EngineRootRestriction.Unrestricted, fen, Nil,
        EngineSearchLimits(16, 5_000_000, 5_000, 1), 6_000)
      val physical = EngineLineAdmission.admitReportedLineSuffixes(work,
        List(ReportedEngineLineSuffix(List(move), ReportedWhiteEngineScore.Centipawns(cp), 16))).get
      assertEquals(physical.head.continuation.moves, List(move))
      assertEquals(physical.head.continuation.automaticTerminal, Some(AutomaticTerminal.InsufficientMaterial))
      assertEquals(EngineLineAdmission.bindAdmittedLineSuffixes(work, physical), None,
        "the observed draw must not turn a contradictory reported score into an accepted evaluation")
      val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
      val input = MoveReviewInputAdmission.retainLegalContinuations(
        MoveReviewInputAdmission.admitIndependent(history, move).get, physical.map(_.continuation))
      assertEquals(input.evaluationObservations, Nil)
      assertEquals((summary(input, List(move)) \ "ending" \ "kind").as[String], "insufficient_material")

  test("a contradictory terminal report stops evaluation without erasing its certified physical paths"):
    val fen = "8/k1P5/2K5/8/8/8/8/8 w - - 0 1"
    val mateMoves = List("c7c8r", "a7a6", "c8a8")
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val resultingFen = history.extend(mateMoves.take(1)).toOption.get.currentFen
    for score <- List(ReportedWhiteEngineScore.Centipawns(20), ReportedWhiteEngineScore.Mate(-2)) do
      val awaiting = CommentaryJobReducer.startJob(
        fen, Nil, fen, mateMoves.head, resultingFen,
        CommentaryJobPolicy(16, 100_000L, CommentaryEngineProfile.ServerAdmitted, previewDepth = Some(8)),
        0L).toOption.get.asInstanceOf[AwaitingEngineWork]
      val work = awaiting.issuedWork
      assertEquals(work.multiPv, 3)
      val lines = List(
        ReportedEngineLineSuffix(mateMoves, score, work.requiredDepth),
        ReportedEngineLineSuffix(List("c7c8q"), ReportedWhiteEngineScore.Centipawns(0), work.requiredDepth),
        ReportedEngineLineSuffix(List("c7c8b"), ReportedWhiteEngineScore.Centipawns(0), work.requiredDepth))
      val stopped = CommentaryJobReducer.submitIssuedWorkReport(awaiting,
        EngineWorkReport.Completed(work.engineProfile, work.workId, work.executionKeySha256, lines),
        1L).toOption.get.asInstanceOf[StoppedCommentaryJob]
      assertEquals(stopped.stopCondition, CommentaryJobStopCondition.InvalidEngineWorkReport)
      assertEquals(stopped.progress.physicalReportsAccepted, 0)
      assertEquals(stopped.progress.rootCandidateLinesAdmitted, 0)
      assertEquals(stopped.assessment, None)
      assertEquals(stopped.verification.map(_.state), Some("stopped"))
      assertEquals(stopped.verification.flatMap(_.completedDepth), None)
      assertEquals(stopped.verification.flatMap(_.stopReason), Some("invalid_engine_work_report"))
      val input = stopped.observationsPacket.get.assembly.input
      assertEquals(input.evaluationObservations, Nil)
      assertEquals((summary(input, mateMoves) \ "ending" \ "kind").as[String], "checkmate")
      assertEquals((summary(input, List("c7c8q")) \ "ending" \ "kind").as[String], "stalemate")
      assertEquals((summary(input, List("c7c8b")) \ "ending" \ "kind").as[String], "insufficient_material")
