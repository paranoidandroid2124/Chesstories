package lila.chessjudgment.analysis.line

import chess.{ Black, White }
import chess.variant.Standard
import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CanonicalPositionHistory,
  CanonicalPositionHistoryFailure,
  PrincipalVariationEvidence
}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.EngineLine

class TruthBoundaryTest extends munit.FunSuite:

  test("boundary inspection replays a full legal tail but retains only its first automatic terminal"):
    val initialFen = "7k/8/8/8/K7/1p6/8/8 w - - 0 1"
    val suffix = List("a4b3", "h8h7", "b3c3")
    val history = CanonicalPositionHistory
      .from(initialFen, Nil, initialFen)
      .getOrElse(fail("expected the K+N versus K position to be canonical"))

    assertEquals(
      history.inspectAutomaticTerminalBoundary(suffix),
      Right(Some(List("a4b3") -> AutomaticTerminal.InsufficientMaterial))
    )
    assertEquals(
      history.extend(suffix),
      Left(CanonicalPositionHistoryFailure.IllegalMovePrefix)
    )

  test("lasting material outcome separates a queen target from the retained exchange gain"):
    val fen = "r2q3k/8/8/8/8/8/8/3R3K w - - 0 1"
    val queenCapture = LineMaterialCapture(
      moveUci = "d1d8",
      plyOffset = 0,
      side = White,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("queen"),
      square = EvidenceSquare("d8"),
      valueCp = 900,
      recapture = false
    )
    val rookRecapture = LineMaterialCapture(
      moveUci = "a8d8",
      plyOffset = 1,
      side = Black,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("rook"),
      square = EvidenceSquare("d8"),
      valueCp = 500,
      recapture = true
    )
    val line = normalizedMaterialLine(
      id = "queen-exchange",
      fen = fen,
      moves = List("d1d8", "a8d8"),
      summary = LineMaterialSummary(
        sideToMove = White,
        captures = List(queenCapture, rookRecapture),
        netCaptureCpForMover = 400,
        maxGainCpForMover = 900,
        maxLossCpForMover = 0,
        hasRecaptureChain = true,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
    )
    val outcome = exactMaterialOutcome(line, LineConsequenceKind.MaterialGain)

    assertEquals(outcome.beneficiary, White)
    assertEquals(outcome.durableNetCp, 400)
    assertEquals(outcome.event.capturedRole, EvidencePieceRole("queen"))
    assertEquals(outcome.event.square, EvidenceSquare("d8"))
    assertEquals(outcome.event.targetValueCp, 900)
    assertEquals(outcome.event.plyOffset, 0)

  test("a free rook capture retains its full material outcome"):
    val capture = LineMaterialCapture(
      moveUci = "d1d8",
      plyOffset = 0,
      side = White,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("rook"),
      square = EvidenceSquare("d8"),
      valueCp = 500,
      recapture = false
    )
    val line = normalizedMaterialLine(
      id = "free-rook",
      fen = "3r3k/8/8/8/8/8/8/3R3K w - - 0 1",
      moves = List("d1d8"),
      summary = LineMaterialSummary(
        sideToMove = White,
        captures = List(capture),
        netCaptureCpForMover = 500,
        maxGainCpForMover = 500,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
    )
    val outcome = exactMaterialOutcome(line, LineConsequenceKind.MaterialGain)

    assertEquals(outcome.durableNetCp, 500)
    assertEquals(outcome.event.targetValueCp, 500)

  test("lasting material outcome normalizes a loss to the beneficiary side"):
    val capture = LineMaterialCapture(
      moveUci = "d8d4",
      plyOffset = 1,
      side = Black,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("rook"),
      square = EvidenceSquare("d4"),
      valueCp = 500,
      recapture = false
    )
    val line = normalizedMaterialLine(
      id = "rook-loss",
      fen = "3r3k/8/8/8/8/8/8/3R3K w - - 0 1",
      moves = List("d1d4", "d8d4"),
      summary = LineMaterialSummary(
        sideToMove = White,
        captures = List(capture),
        netCaptureCpForMover = -500,
        maxGainCpForMover = 0,
        maxLossCpForMover = -500,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
    )
    val outcome = exactMaterialOutcome(line, LineConsequenceKind.MaterialLoss)

    assertEquals(outcome.beneficiary, Black)
    assertEquals(outcome.durableNetCp, 500)
    assertEquals(outcome.event.plyOffset, 1)

  test("a later independent capture cannot inflate the first lasting material outcome"):
    val pawnCapture = LineMaterialCapture(
      moveUci = "a1a7",
      plyOffset = 0,
      side = White,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("pawn"),
      square = EvidenceSquare("a7"),
      valueCp = 100,
      recapture = false
    )
    val laterRookCapture = LineMaterialCapture(
      moveUci = "c1g5",
      plyOffset = 2,
      side = White,
      attackerRole = EvidencePieceRole("bishop"),
      capturedRole = EvidencePieceRole("rook"),
      square = EvidenceSquare("g5"),
      valueCp = 500,
      recapture = false
    )
    val line = normalizedMaterialLine(
      id = "later-independent-gain",
      fen = "7k/p7/8/6r1/8/8/8/R1B4K w - - 0 1",
      moves = List("a1a7", "h8g8", "c1g5"),
      summary = LineMaterialSummary(
        sideToMove = White,
        captures = List(pawnCapture, laterRookCapture),
        netCaptureCpForMover = 600,
        maxGainCpForMover = 600,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
    )
    val outcome = exactMaterialOutcome(line, LineConsequenceKind.MaterialGain)

    assertEquals(outcome.event.moveUci, "a1a7")
    assertEquals(outcome.event.targetValueCp, 100)
    assertEquals(outcome.durableNetCp, 100)

  test("an incomplete material window cannot publish a durable outcome"):
    val capture = LineMaterialCapture(
      moveUci = "d1d8",
      plyOffset = 0,
      side = White,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("rook"),
      square = EvidenceSquare("d8"),
      valueCp = 500,
      recapture = false
    )
    val line = normalizedMaterialLine(
      id = "incomplete-rook-capture",
      fen = "3r3k/8/8/8/8/8/8/3R3K w - - 0 1",
      moves = List("d1d8"),
      summary = LineMaterialSummary(
        sideToMove = White,
        captures = List(capture),
        netCaptureCpForMover = 500,
        maxGainCpForMover = 500,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = false
      )
    )

    assert(
      line.lineConsequences
        .filter(consequence =>
          consequence.kind == LineConsequenceKind.MaterialGain ||
            consequence.kind == LineConsequenceKind.MaterialLoss
        )
        .forall(_.materialOutcome.isEmpty)
    )

  test("a zero material balance cannot publish a positive durable outcome"):
    val capture = LineMaterialCapture(
      moveUci = "d1d8",
      plyOffset = 0,
      side = White,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("rook"),
      square = EvidenceSquare("d8"),
      valueCp = 500,
      recapture = false
    )
    val recapture = LineMaterialCapture(
      moveUci = "a8d8",
      plyOffset = 1,
      side = Black,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("rook"),
      square = EvidenceSquare("d8"),
      valueCp = 500,
      recapture = true
    )
    val line = normalizedMaterialLine(
      id = "equal-rook-exchange",
      fen = "r2r3k/8/8/8/8/8/8/3R3K w - - 0 1",
      moves = List("d1d8", "a8d8"),
      summary = LineMaterialSummary(
        sideToMove = White,
        captures = List(capture, recapture),
        netCaptureCpForMover = 0,
        maxGainCpForMover = 500,
        maxLossCpForMover = 0,
        hasRecaptureChain = true,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
    )

    assert(line.lineConsequences.forall(_.materialOutcome.isEmpty))

  test("ambiguous capture ownership cannot publish a durable outcome"):
    val capture = LineMaterialCapture(
      moveUci = "d1d8",
      plyOffset = 0,
      side = White,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("rook"),
      square = EvidenceSquare("d8"),
      valueCp = 500,
      recapture = false
    )
    val line = normalizedMaterialLine(
      id = "ambiguous-rook-capture",
      fen = "3r3k/8/8/8/8/8/8/3R3K w - - 0 1",
      moves = List("d1d8"),
      summary = LineMaterialSummary(
        sideToMove = White,
        captures = List(capture, capture),
        netCaptureCpForMover = 500,
        maxGainCpForMover = 500,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
    )

    assert(
      line.lineConsequences
        .filter(_.kind == LineConsequenceKind.MaterialGain)
        .forall(_.materialOutcome.isEmpty)
    )

  test("an exact named opening sequence is not itself semantic proof"):
    val afterE4 = PrincipalVariationEvidence
      .legalFenAfter(Standard.initialFen.value, "e2e4")
      .getOrElse(fail("expected 1. e4 to be legal"))
    val beforeQueenMove = PrincipalVariationEvidence
      .legalFenAfter(afterE4, "e7e5")
      .getOrElse(fail("expected 1... e5 to be legal"))
    val declaredLine = EngineLine(
      moves = List("d1h5", "b8c6", "f1c4", "g8f6", "h5f7"),
      scoreCp = 900,
      depth = 18
    )
    val history = CanonicalPositionHistory
      .from(beforeQueenMove, Nil, beforeQueenMove)
      .getOrElse(fail("expected the queen-line root to be canonical"))
    val extended = history
      .extend(declaredLine.moves)
      .getOrElse(fail("expected the declared queen line to be legal"))
    val replay = CanonicalLineReplay
      .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail("expected one canonical queen-line replay"))

    assertEquals(
      ForcedLineTruth.detect(replay, List(declaredLine)),
      None
    )

  test("conversion cause polarity is owned by exactly one comparison side"):
    assert(
      RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.ConversionMiss,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateAllowsLiability
      )
    )
    assert(
      !RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.ConversionMiss,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      )
    )
    assert(
      RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.ConversionSecured,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      )
    )
    assert(
      !RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.ConversionSecured,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.CandidateAllowsLiability
      )
    )
    assert(
      RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.MaterialSwing,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateAllowsLiability
      )
    )
    assert(
      !RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.MaterialSwing,
        RelativeCauseSourceSide.Mixed,
        CauseAttributionKind.CandidateAllowsLiability
      )
    )

  private def exactMaterialOutcome(
      line: LineFactEvidence,
      kind: LineConsequenceKind
  ): RootOwnedMaterialOutcome =
    line.lineConsequences
      .find(_.kind == kind)
      .flatMap(_.materialOutcome)
      .getOrElse(fail(s"expected exact $kind material outcome"))

  private def normalizedMaterialLine(
      id: String,
      fen: String,
      moves: List[String],
      summary: LineMaterialSummary
  ): LineFactEvidence =
    val lineHistory = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .flatMap(_.extend(moves))
      .getOrElse(fail("expected canonical material line history"))
    val canonicalReplay = CanonicalLineReplay
      .fromHistory(lineHistory.segmentReplaySteps)
      .getOrElse(fail("expected canonical material line replay"))
    val moveRefs = canonicalReplay.replaySteps.map(step =>
      PrincipalVariationEvidence.LineMoveRef(step.ply, step.moveUci, step.fenAfter)
    )
    val first = moveRefs.headOption.getOrElse(fail("expected a non-empty material line"))
    val lineRef = LineNodeRef(id, first.uci, 1, LineNodeRole.BestReference)
    val facts = PrincipalVariationEvidence.LineFacts(
      line = PrincipalVariationEvidence.LineVariationRef(moveRefs),
      first = first,
      reply = moveRefs.lift(1),
      continuation = moveRefs.lift(2),
      continuationTail = moveRefs.drop(3)
    )
    LineFactNormalizer
      .fromValidatedLine(
        id = s"$id-evidence",
        lineRef = lineRef,
        facts = facts,
        replay = canonicalReplay,
        position = PositionNodeRef(fen, 0, Some(summary.sideToMove)),
        scope = lineRef.role.scope,
        materialSummary = Some(summary)
      )
      .payload match
      case payload: LineFactEvidence => payload
      case _ => fail("expected normalized line evidence")
