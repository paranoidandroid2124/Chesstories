package lila.chessjudgment.analysis.assembly

import chess.Pawn
import chess.variant.Standard
import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  PositionRuleAssessment,
  PreInitialHistoryKnowledge
}
import lila.chessjudgment.model.line.EngineLine
import lila.chessjudgment.model.judgment.{
  EvidenceRecord,
  EvidenceRef,
  EvidenceConfidence,
  EvidenceLayer,
  EvidenceProducer,
  LineEventKind,
  LineFactEvidence,
  LineMaterialRecaptureStatus
}

class HistoryTerminalContinuityTest extends munit.FunSuite:

  test("terminal PV endpoints retain engine provenance rather than certifying the root outcome"):
    val fen = "8/k1P5/2K5/8/8/8/8/8 w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val lines = List(
      EngineLine(List("c7c8r", "a7a6", "c8a8"), 10000, Some(2), 16) ->
        AutomaticTerminal.Checkmate(chess.White),
      EngineLine(List("c7c8r", "a7a6", "c8b8", "a6a5", "b8b4", "a5b4"), 0, None, 1) ->
        AutomaticTerminal.InsufficientMaterial
    )
    lines.foreach { case (line, terminal) =>
      val continuation = history.admitToFirstAutomaticTerminal(line.moves).toOption.get
      assertEquals(continuation.automaticTerminal, Some(terminal))
      val admitted = continuation.bind(CandidateLineEvaluation.EngineSearch(line)).get
      assertEquals(admitted.automaticTerminal, Some(terminal))
      val exact = CandidateLineEvaluation.ExactAutomaticTerminal(line.moves, terminal)
      assertEquals(continuation.bind(exact), None)
      assert(!exact.comparisonReady)

      val alternative = EngineLine(List("c6d6", "a7a6"), 20, None, 16)
      val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen, "c7c8r", List(line, alternative))).get
      val candidate = input.assessmentCandidates.find(_.rootMove.contains("c7c8r")).get
      assertEquals(candidate.evaluation, CandidateLineEvaluation.EngineSearch(line))
      assertEquals(candidate.evaluation.comparisonReady, line.mate.nonEmpty)
      assert(input.legalLines.exists(_.moves == line.moves))
      val evaluations = MoveReviewAssembler.assemble(input).evidenceGraph.records
        .filter(record => record.ref.layer == EvidenceLayer.Eval && record.ref.line.exists(_.rootMove == "c7c8r"))
      if candidate.evaluation.comparisonReady then
        assertEquals(evaluations.size, 1)
        assertEquals(evaluations.head.ref.producer, EvidenceProducer.EngineEvalProducer)
        assertEquals(evaluations.head.ref.confidence, EvidenceConfidence.EngineBacked)
      else
        assertEquals(evaluations, Nil)
        assert(input.evaluationObservations.exists(_.evaluation == CandidateLineEvaluation.EngineSearch(line)))
    }

    val supplied = MoveReviewInputAdmission.admitIndependent(history, "c7c8r", List(lines.last._1.moves)).get
    assertEquals(supplied.assessmentCandidates, Nil)
    assertEquals(supplied.evaluationObservations, Nil)
    assert(supplied.legalLines.exists(_.moves == lines.last._1.moves))

  test("a terminal score mismatch retains the legal conditional line without admitting its evaluation"):
    val fen = "8/k1P5/2K5/8/8/8/8/8 w - - 0 1"
    val line = EngineLine(List("c7c8r", "a7a6", "c8a8"), -10000, Some(-2), 16)
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen, "c7c8r", List(line))).get
    assertEquals(input.assessmentCandidates, Nil)
    assertEquals(input.evaluationObservations, Nil)
    assert(input.legalLines.exists(_.moves == line.moves))

  test("engine lines stop at the history-owned automatic terminal boundary"):
    val fen = "7k/8/8/8/K7/1p6/8/8 w - - 0 1"
    val prepared = MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = fen,
          playedMoveUci = "a4b3",
          variations = List(
            EngineLine(List("a4a3", "h8g8"), 20, None, 16),
            EngineLine(List("a4b3", "h8h7", "b3c3"), 0, None, 16)
          )
        )
      )
      .getOrElse(fail("expected the automatic-terminal line to normalize"))

    assertEquals(
      prepared.assessmentCandidates
        .find(candidate => candidate.rootMove.exists(_ == prepared.playedMoveUci))
        .map(_.evaluation),
      Some(
        CandidateLineEvaluation.ExactAutomaticTerminal(
          List("a4b3"),
          AutomaticTerminal.InsufficientMaterial
        )
      )
    )

  test("canonical history retains custom-root replay metadata, including captures and castling"):
    val customInitial = "4k3/8/8/8/3p4/4P3/5P2/4K3 b - - 0 40"
    val customCurrent = "4k3/8/8/8/8/4p3/5P2/4K3 w - - 0 41"
    val customHistory = CanonicalPositionHistory
      .from(customInitial, List("d4e3"), customCurrent)
      .toOption
      .getOrElse(fail("expected the custom-root capture to replay"))
    val capture = customHistory.segmentReplaySteps match
      case step :: Nil => step
      case other       => fail(s"expected one replay step, got ${other.size}")

    assertEquals(customHistory.preInitialHistoryKnowledge, PreInitialHistoryKnowledge.Unknown)
    assertEquals(customHistory.currentPly, 80)
    assertEquals(capture.ply, 80)
    assertEquals(capture.uci, "d4e3")
    assertEquals(capture.beforeFen, customInitial)
    assertEquals(capture.afterFen, customCurrent)
    assertEquals(capture.capturedRole, Some(Pawn))

    val castleInitial = "4k2r/8/8/8/8/8/8/4K2R w Kk - 0 1"
    val castleCurrent = "4k2r/8/8/8/8/8/8/5RK1 b k - 1 1"
    val castleHistory = CanonicalPositionHistory
      .from(castleInitial, List("e1g1"), castleCurrent)
      .toOption
      .getOrElse(fail("expected the custom-root castle to replay"))
    val castle = castleHistory.segmentReplaySteps.headOption
      .getOrElse(fail("expected the castle replay step"))

    assertEquals(castle.ply, 1)
    assertEquals(castle.beforeFen, castleInitial)
    assertEquals(castle.afterFen, castleCurrent)
    assertEquals(castle.move.castle.map(_.kingTo.key), Some("g1"))

  test("material summaries preserve capture when only recapture history is unknown"):
    val customInitial = "4k3/8/8/8/3p4/4P3/5P2/4K3 b - - 0 40"
    val customCurrent = "4k3/8/8/8/8/4p3/5P2/4K3 w - - 0 41"
    val capturedPredecessor = CanonicalPositionHistory
      .from(customInitial, List("d4e3"), customCurrent)
      .toOption
      .getOrElse(fail("expected the stored predecessor"))
    val recaptureLine = capturedPredecessor
      .extend(List("f2e3"))
      .toOption
      .getOrElse(fail("expected the candidate recapture"))
    val recaptureSummary = LegalLineAssembler
      .lineMaterialSummary(capturedPredecessor, recaptureLine)
      .getOrElse(fail("expected material knowledge from the stored predecessor"))

    assert(recaptureSummary.captures.headOption.exists(_.recapture))
    assert(recaptureSummary.captures.exists(_.recapture))

    val unknownPrehistory = CanonicalPositionHistory
      .from(customCurrent, Nil, customCurrent)
      .toOption
      .getOrElse(fail("expected an empty custom-root history"))
    val unknownFirstCapture = unknownPrehistory
      .extend(List("f2e3"))
      .toOption
      .getOrElse(fail("expected the legal first capture"))

    assertEquals(unknownPrehistory.preInitialHistoryKnowledge, PreInitialHistoryKnowledge.Unknown)
    val unknownSummary = LegalLineAssembler.lineMaterialSummary(unknownPrehistory, unknownFirstCapture)
      .getOrElse(fail("a missing predecessor must not erase the observed capture"))
    val unknownCapture = unknownSummary.captures.head
    assertEquals(unknownCapture.recaptureStatus, LineMaterialRecaptureStatus.Unknown)
    assertEquals(unknownCapture.capturedRole.name, "pawn")
    assertEquals(unknownCapture.square.key, "e3")
    assertEquals(unknownSummary.observedNetCpForMover, recaptureSummary.observedNetCpForMover)

    val standardCurrent = "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"
    val knownPredecessor = CanonicalPositionHistory
      .from(Standard.initialFen.value, List("e2e4", "d7d5", "e4d5"), standardCurrent)
      .toOption
      .getOrElse(fail("expected the standard-root predecessor"))
    val immediateRecapture = knownPredecessor
      .extend(List("d8d5"))
      .toOption
      .getOrElse(fail("expected the standard-root immediate recapture"))
    val immediateSummary = LegalLineAssembler
      .lineMaterialSummary(knownPredecessor, immediateRecapture)
      .getOrElse(fail("expected the known predecessor material summary"))

    assertEquals(knownPredecessor.preInitialHistoryKnowledge, PreInitialHistoryKnowledge.KnownEmpty)
    assert(immediateSummary.captures.headOption.exists(_.recapture))

  test("focused production preserves an immediate recapture from nonempty history"):
    val currentFen = "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"
    val history = CanonicalPositionHistory
      .from(Standard.initialFen.value, List("e2e4", "d7d5", "e4d5"), currentFen)
      .toOption
      .getOrElse(fail("expected the exact played occurrence history"))
    val rootEvaluations = List(
      EngineLine(List("d8d5", "b1c3"), -30, None, 16),
      EngineLine(List("g8f6", "b1c3"), 20, None, 16)
    ).map { line =>
      val evaluation = CandidateLineEvaluation.EngineSearch(line)
      history
        .admitToFirstAutomaticTerminal(line.moves)
        .toOption
        .flatMap(_.bind(evaluation))
        .getOrElse(fail(s"expected focused admission for ${line.moves.head}"))
    }
    val resultingFen = history
      .extend(List("d8d5"))
      .toOption
      .map(_.currentFen)
      .getOrElse(fail("expected the played recapture position"))
    val review = MoveReviewInputAdmission
      .prepareFocusedMoveReview(
        history,
        "d8d5",
        resultingFen,
        PositionRuleAssessment.assess(history),
        rootEvaluations,
        Nil
      ) match
      case Some(PreparedFocusedPositionCommentary.MoveReview(value)) => value
      case other => fail(s"expected a focused move review, got $other")
    val assembly = MoveReviewAssembler.assemble(review.preparedInput)
    val lineFact = assembly.evidenceGraph.records.collect {
      case EvidenceRecord(_, payload: LineFactEvidence, _)
          if payload.rootMove.exists(EvidenceRef.sameMove(_, "d8d5")) => payload
    } match
      case exact :: Nil => exact
      case other        => fail(s"expected one recapture line fact, got ${other.size}")
    val recapture = lineFact.materialCaptures
      .find(capture => capture.plyOffset == 0 && EvidenceRef.sameMove(capture.moveUci, "d8d5"))
      .getOrElse(fail("expected the root capture material fact"))

    assert(recapture.recaptureStatus.isInstanceOf[LineMaterialRecaptureStatus.Proven])

  test("assembly retains a definite capture and unknown recapture without predecessor input"):
    val currentFen = "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"
    val admitted = MoveReviewInputAdmission.admit(RawMoveReviewInput(
      currentFen,
      "d8d5",
      List(EngineLine(List("d8d5", "b1c3"), -30, None, 16))
    )).getOrElse(fail("the legal capture needs no evaluation comparison or predecessor"))
    val assembly = MoveReviewAssembler.assemble(admitted)
    val records = assembly.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, payload: LineFactEvidence, _) => record -> payload
    }
    assertEquals(records.size, 1)
    val (record, line) = records.head
    assert(assembly.evidenceGraph.proofEligible(record))
    val capture = line.materialCaptures.head
    assertEquals(capture.recaptureStatus, LineMaterialRecaptureStatus.Unknown)
    assertEquals(capture.capturedRole.name, "pawn")
    assertEquals(capture.square.key, "d5")
    assert(line.lineEvents.exists(event => event.kind == LineEventKind.Capture && event.plyOffset == 0))
    assert(!line.lineEvents.exists(_.kind == LineEventKind.Recapture))
