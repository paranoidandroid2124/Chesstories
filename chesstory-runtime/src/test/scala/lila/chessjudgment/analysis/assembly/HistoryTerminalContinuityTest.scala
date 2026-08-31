package lila.chessjudgment.analysis.assembly

import chess.Pawn
import chess.variant.Standard
import lila.chessjudgment.model.{
  BranchReplyProbeBinding,
  ProbeRequest,
  ProbeResolution,
  ProbeResult,
  ProbeVariant
}
import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  PreInitialHistoryKnowledge
}
import lila.chessjudgment.model.line.EngineLine

class HistoryTerminalContinuityTest extends munit.FunSuite:

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
      prepared.playedLine.map(_.evaluation),
      Some(
        CandidateLineEvaluation.ExactAutomaticTerminal(
          List("a4b3"),
          AutomaticTerminal.InsufficientMaterial
        )
      )
    )

  test("branch reply horizon closes on a history-owned automatic terminal"):
    val fen = "k7/6b1/8/8/8/8/3R4/7K w - - 0 1"
    val playedMove = "d2d4"
    val replyMove = "g7d4"
    val prepared = MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = fen,
          playedMoveUci = playedMove,
          variations = List(
            EngineLine(List("d2d3", "g7f8", "h1g1"), 20, None, 16),
            EngineLine(List(playedMove, "g7f8", "h1g1"), 0, None, 16)
          )
        )
      )
      .getOrElse(fail("expected the branch root to normalize"))
    val played = prepared.playedLine.getOrElse(fail("expected the played root line"))
    val engineLine = played.evaluation.engineLine.getOrElse(fail("expected a nonterminal root engine line"))
    val branchFen = played.replay.replaySteps.headOption
      .map(_.fenAfter)
      .getOrElse(fail("expected the branch position"))
    val horizon = 3
    val variationHash = BranchReplyProbeBinding.variationHash(
      rootFen = fen,
      role = played.role,
      rootMove = playedMove,
      whitePovEvalCp = engineLine.scoreCp,
      mate = engineLine.mate,
      depth = engineLine.depth,
      moves = engineLine.moves,
      replyMove = replyMove,
      certifiedHorizonPlyOffset = horizon
    )
    val request = ProbeRequest(
      id = "insufficient-material-reply",
      fen = branchFen,
      depth = 16,
      candidateMove = playedMove,
      depthFloor = 12,
      variationHash = variationHash,
      variant = ProbeVariant.BranchReply(replyMove, horizon)
    )
    val result = ProbeResult(
      id = request.id,
      resolution = ProbeResolution.ExactAutomaticTerminal(
        CandidateLineEvaluation.ExactAutomaticTerminal(
          List(replyMove),
          AutomaticTerminal.InsufficientMaterial
        )
      )
    )
    val admission = MoveReviewInputAdmission
      .admitIssuedProbeResult(prepared, request, result, prepared.lines.map(_.rank).max + 1)
      .getOrElse(fail("expected the exact automatic terminal probe to be admitted"))

    assertEquals(admission.branch.certifiedHorizonPlyOffset, horizon)
    assertEquals(
      admission.branch.lines.map(_.evaluation),
      List(
        CandidateLineEvaluation.ExactAutomaticTerminal(
          List(replyMove),
          AutomaticTerminal.InsufficientMaterial
        )
      )
    )

  test("trusted prepared probe pairs fail loudly before the shared executor on impossible metadata"):
    val prepared = MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = Standard.initialFen.value,
          playedMoveUci = "d2d4",
          variations = List(
            EngineLine(List("e2e4", "e7e5", "g1f3"), 20, None, 16),
            EngineLine(List("d2d4", "d7d5", "g1f3"), 0, None, 16)
          )
        )
      )
      .getOrElse(fail("expected a normalized prepared review"))
    val request = ProbeRequest(
      id = "trusted-request",
      fen = Standard.initialFen.value,
      depth = 16,
      candidateMove = "d2d4",
      depthFloor = 12,
      variationHash = "trusted-variation",
      variant = ProbeVariant.BranchReply("d7d5", 1)
    )
    val mismatched = ProbeResult(
      id = "different-result",
      resolution = ProbeResolution.EngineSearch(
        List(CandidateLineEvaluation.EngineSearch(EngineLine(List("e2e4"), 0, None, 16))),
        16
      )
    )

    intercept[IllegalArgumentException] {
      MoveReviewJudgmentOrchestrator.executePreparedReview(prepared, List(request -> mismatched))
    }

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
    assertEquals(capture.index, 0)
    assertEquals(capture.ply, 80)
    assertEquals(capture.uci, "d4e3")
    assertEquals(capture.beforeFen, customInitial)
    assertEquals(capture.afterFen, customCurrent)
    assertEquals(capture.capturedRole, Some(Pawn))
    assertEquals(capture.castle, None)

    val castleInitial = "4k2r/8/8/8/8/8/8/4K2R w Kk - 0 1"
    val castleCurrent = "4k2r/8/8/8/8/8/8/5RK1 b k - 1 1"
    val castleHistory = CanonicalPositionHistory
      .from(castleInitial, List("e1g1"), castleCurrent)
      .toOption
      .getOrElse(fail("expected the custom-root castle to replay"))
    val castle = castleHistory.segmentReplaySteps.headOption
      .getOrElse(fail("expected the castle replay step"))

    assertEquals(castle.index, 0)
    assertEquals(castle.ply, 1)
    assertEquals(castle.beforeFen, castleInitial)
    assertEquals(castle.afterFen, castleCurrent)
    assert(castle.castle.nonEmpty)

  test("material summaries use stored predecessor knowledge and reject unknown prehistory"):
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
    val recaptureSummary = CandidateLineAssembler
      .lineMaterialSummary(capturedPredecessor, recaptureLine)
      .getOrElse(fail("expected material knowledge from the stored predecessor"))

    assert(recaptureSummary.captures.headOption.exists(_.recapture))
    assert(recaptureSummary.hasRecaptureChain)

    val unknownPrehistory = CanonicalPositionHistory
      .from(customCurrent, Nil, customCurrent)
      .toOption
      .getOrElse(fail("expected an empty custom-root history"))
    val unknownFirstCapture = unknownPrehistory
      .extend(List("f2e3"))
      .toOption
      .getOrElse(fail("expected the legal first capture"))

    assertEquals(unknownPrehistory.preInitialHistoryKnowledge, PreInitialHistoryKnowledge.Unknown)
    assertEquals(CandidateLineAssembler.lineMaterialSummary(unknownPrehistory, unknownFirstCapture), None)

    val standardCurrent = "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"
    val knownPredecessor = CanonicalPositionHistory
      .from(Standard.initialFen.value, List("e2e4", "d7d5", "e4d5"), standardCurrent)
      .toOption
      .getOrElse(fail("expected the standard-root predecessor"))
    val immediateRecapture = knownPredecessor
      .extend(List("d8d5"))
      .toOption
      .getOrElse(fail("expected the standard-root immediate recapture"))
    val immediateSummary = CandidateLineAssembler
      .lineMaterialSummary(knownPredecessor, immediateRecapture)
      .getOrElse(fail("expected the known predecessor material summary"))

    assertEquals(knownPredecessor.preInitialHistoryKnowledge, PreInitialHistoryKnowledge.KnownEmpty)
    assert(immediateSummary.captures.headOption.exists(_.recapture))
