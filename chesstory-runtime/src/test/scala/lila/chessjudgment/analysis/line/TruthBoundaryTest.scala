package lila.chessjudgment.analysis.line

import chess.{ Black, Queen, Rook, Square, White }
import chess.variant.Standard
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.analysis.position.PositionAnalyzer
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.EngineLine

class TruthBoundaryTest extends munit.FunSuite:

  test("material imbalance comes from board piece counts"):
    val balanced = PositionAnalyzer
      .extractFeatures(Standard.initialFen.value, plyCount = 0)
      .getOrElse(fail("expected initial-position features"))
    val extraWhiteKnight = PositionAnalyzer
      .extractFeatures("4k3/8/8/8/8/8/8/1N2K3 w - - 0 1", plyCount = 0)
      .getOrElse(fail("expected imbalanced-position features"))

    assertEquals(balanced.imbalance.whiteKnights, balanced.imbalance.blackKnights)
    assertEquals(extraWhiteKnight.imbalance.whiteKnights, 1)
    assertEquals(extraWhiteKnight.imbalance.blackKnights, 0)

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

    assertEquals(
      ForcedLineTruth.detect(beforeQueenMove, "d1h5", List(declaredLine)),
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

  test("a ready specific cause dominates a generic fallback only in the same causal channel"):
    val decisions = dominanceDecisions(
      fallbackTarget = "e8",
      specificTarget = "e8",
      specificSourceSide = RelativeCauseSourceSide.Reference
    )

    assertEquals(
      decisions("fallback").status,
      RelativeCauseDominanceStatus.DominatedFallback
    )
    assertEquals(decisions("fallback").dominatingCauseEvidenceIds, List("specific"))
    assert(decisions("specific").retained)

  test("a specific cause on another target cannot suppress an independent fallback"):
    val decisions = dominanceDecisions(
      fallbackTarget = "e8",
      specificTarget = "a8",
      specificSourceSide = RelativeCauseSourceSide.Reference
    )

    assert(decisions("fallback").retained)
    assert(decisions("specific").retained)

  test("source and attribution polarity separate otherwise identical fallback channels"):
    val decisions = dominanceDecisions(
      fallbackTarget = "e8",
      specificTarget = "e8",
      specificSourceSide = RelativeCauseSourceSide.Candidate
    )

    assert(decisions("fallback").retained)
    assert(decisions("specific").retained)

  test("a generic tactical liability yields to its exact material outcome"):
    val decisions = dominanceDecisions(
      fallbackTarget = "e4",
      specificTarget = "e4",
      specificSourceSide = RelativeCauseSourceSide.Candidate,
      fallbackKind = RelativeCauseKind.CandidateTacticalLiability,
      fallbackSourceSide = RelativeCauseSourceSide.Candidate,
      specificKind = RelativeCauseKind.MaterialSwing,
      materialProof = true
    )

    assertEquals(
      decisions("fallback").status,
      RelativeCauseDominanceStatus.DominatedFallback
    )
    assertEquals(decisions("fallback").dominatingCauseEvidenceIds, List("specific"))

  test("an object-unready specific cause cannot suppress a ready fallback"):
    val decisions = dominanceDecisions(
      fallbackTarget = "e8",
      specificTarget = "e8",
      specificSourceSide = RelativeCauseSourceSide.Reference,
      specificReady = false
    )

    assert(decisions("fallback").retained)
    assert(decisions("specific").retained)

  private def dominanceDecisions(
      fallbackTarget: String,
      specificTarget: String,
      specificSourceSide: RelativeCauseSourceSide,
      fallbackKind: RelativeCauseKind = RelativeCauseKind.MissedTacticalResource,
      fallbackSourceSide: RelativeCauseSourceSide = RelativeCauseSourceSide.Reference,
      specificReady: Boolean = true,
      specificKind: RelativeCauseKind = RelativeCauseKind.KingForcing,
      materialProof: Boolean = false
  ): Map[String, RelativeCauseDominanceDecision] =
    val position = PositionNodeRef(
      fen = "4k3/8/8/8/1r2r3/8/4Q3/K7 w - - 0 1",
      ply = 0,
      sideToMove = Some(White)
    )
    val referenceLine = LineNodeRef("reference", "e2h5", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate", "e2e4", 2, LineNodeRole.Played)
    val comparisonRef = EvidenceRef(
      id = "comparison",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.CandidateComparison,
      position = position,
      line = Some(candidateLine),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val comparison = CandidateComparisonFact(
      kind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      comparison = EvalComparison.fromLines(
        mover = White,
        reference = CandidateLineNode(
          referenceLine,
          EngineLine(List(referenceLine.rootMove), scoreCp = 300, depth = 18),
          lineEvidenceRef("reference-line", referenceLine, position)
        ),
        candidate = CandidateLineNode(
          candidateLine,
          EngineLine(List(candidateLine.rootMove), scoreCp = -300, depth = 18),
          lineEvidenceRef("candidate-line", candidateLine, position)
        )
      )
    )
    val fallbackLine =
      if fallbackSourceSide == RelativeCauseSourceSide.Reference then referenceLine else candidateLine
    val specificLine =
      if specificSourceSide == RelativeCauseSourceSide.Reference then referenceLine else candidateLine
    val fallbackProofRef =
      if materialProof && fallbackLine.role == LineNodeRole.Played then
        lineEvidenceRef("fallback-proof", fallbackLine, position)
      else motifRef("fallback-proof", fallbackLine, position)
    val specificProofRef =
      if specificReady then
        if materialProof && specificLine.role == LineNodeRole.Played then
          lineEvidenceRef("specific-proof", specificLine, position)
        else motifRef("specific-proof", specificLine, position)
      else
        motifRef("specific-proof", specificLine, position).copy(
          producer = EvidenceProducer.TacticalMechanismProducer,
          layer = EvidenceLayer.TacticalMechanism
        )
    val fallbackCause = cause(
      kind = fallbackKind,
      comparisonRef = comparisonRef,
      proofRef = fallbackProofRef,
      sourceSide = fallbackSourceSide
    )
    val specificCause = cause(
      kind = specificKind,
      comparisonRef = comparisonRef,
      proofRef = specificProofRef,
      sourceSide = specificSourceSide
    )
    val fallbackRef = causeRef("fallback", fallbackLine, position)
    val specificRef = causeRef(
      "specific",
      specificLine,
      position
    )
    val proofRecords = List(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      EvidenceRecord(
        fallbackProofRef,
        if materialProof && fallbackLine.role == LineNodeRole.Played then
          materialLiabilityEvidence(fallbackLine, position)
        else if materialProof then materialMotifEvidence(fallbackLine.rootMove, fallbackTarget)
        else motifEvidence(fallbackLine.rootMove, fallbackTarget)
      ),
      EvidenceRecord(
        specificProofRef,
        if specificReady then
          if materialProof && specificLine.role == LineNodeRole.Played then
            materialLiabilityEvidence(specificLine, position)
          else if materialProof then materialMotifEvidence(specificLine.rootMove, specificTarget)
          else motifEvidence(specificLine.rootMove, specificTarget)
        else
          TacticalMechanismEvidence(
            kind = TacticalMechanismKind.KingForcing,
            moveUci = Some(specificLine.rootMove),
            line = Some(specificLine),
            signals = Nil
          )
      )
    )
    val rawGraph = proofRecords.foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    def admit(cause: RelativeCauseFact): RelativeCauseFact =
      val signatures = EvidenceObjectBinding
        .rawDirectSentenceChannelsForProjection(cause, rawGraph)
        .map(_.causalSignature)
        .toSet
      cause.copy(directEffectAdmission = DirectEffectAdmission.Restricted(signatures))

    val admittedFallbackCause = admit(fallbackCause)
    val admittedSpecificCause = admit(specificCause)
    val causes = List(
      admittedFallbackCause -> fallbackRef,
      admittedSpecificCause -> specificRef
    )
    val graph = (
      proofRecords ++ List(
        EvidenceRecord(fallbackRef, RelativeCauseFactEvidence(admittedFallbackCause)),
        EvidenceRecord(specificRef, RelativeCauseFactEvidence(admittedSpecificCause))
      )
    ).foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))

    val directChannels = causes.map { case (cause, ref) =>
      ref.id -> RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
    }.toMap
    val dominanceEligibility = CrossComparisonCauseExposurePolicy.resolveEligibilityForDominance(
      causes,
      graph,
      Set(candidateLine.rootMove)
    )
    RelativeCauseDominancePolicy
      .resolve(causes, dominanceEligibility, directChannels, graph)
      .decisions
      .map(decision => decision.causeEvidenceId -> decision)
      .toMap

  private def motifRef(
      id: String,
      line: LineNodeRef,
      position: PositionNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.MoveMotifProducer,
      layer = EvidenceLayer.MoveMotif,
      position = position,
      line = Some(line),
      scope = line.role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )

  private def lineEvidenceRef(
      id: String,
      line: LineNodeRef,
      position: PositionNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = position,
      line = Some(line),
      scope = line.role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )

  private def causeRef(
      id: String,
      line: LineNodeRef,
      position: PositionNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = position,
      line = Some(line),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )

  private def motifEvidence(rootMove: String, target: String): MoveMotifEvidence =
    val square = Square.fromKey(target).getOrElse(fail(s"invalid test square '$target'"))
    MoveMotifEvidence(
      MoveMotifEvent.fromMotif(
        rootMove,
        Motif.Check(
          piece = Queen,
          targetSquare = square,
          checkType = Motif.CheckType.Normal,
          color = White,
          plyIndex = 0,
          move = Some(rootMove)
        )
      )
    )

  private def materialMotifEvidence(rootMove: String, target: String): MoveMotifEvidence =
    val square = Square.fromKey(target).getOrElse(fail(s"invalid test square '$target'"))
    MoveMotifEvidence(
      MoveMotifEvent.fromMotif(
        rootMove,
        Motif.Capture(
          piece = Queen,
          captured = Rook,
          square = square,
          captureType = Motif.CaptureType.Winning,
          color = White,
          plyIndex = 0,
          move = Some(rootMove)
        )
      )
    )

  private def materialLiabilityEvidence(
      line: LineNodeRef,
      position: PositionNodeRef
  ): LineFactEvidence =
    val rootAfter = PrincipalVariationEvidence
      .legalFenAfter(position.fen, line.rootMove)
      .getOrElse(fail(s"expected legal liability root ${line.rootMove}"))
    val replyMove = "b4e4"
    val replyAfter = PrincipalVariationEvidence
      .legalFenAfter(rootAfter, replyMove)
      .getOrElse(fail(s"expected legal liability reply $replyMove"))
    val rootCapture = LineMaterialCapture(
      moveUci = line.rootMove,
      plyOffset = 0,
      side = White,
      attackerRole = EvidencePieceRole("queen"),
      capturedRole = EvidencePieceRole("rook"),
      square = EvidenceSquare("e4"),
      valueCp = 500,
      recapture = false
    )
    val replyCapture = LineMaterialCapture(
      moveUci = replyMove,
      plyOffset = 1,
      side = Black,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("queen"),
      square = EvidenceSquare("e4"),
      valueCp = 900,
      recapture = true
    )
    val consequence = LineConsequence(
      kind = LineConsequenceKind.MaterialLoss,
      lineMoves = List(line.rootMove, replyMove),
      proofSignal = true,
      eventMove = Some(replyMove),
      rootMove = Some(line.rootMove),
      rootSide = Some(White),
      beneficiary = Some(Black),
      materialOutcome = Some(RootOwnedMaterialOutcome(
        event = RootOwnedMaterialEventSalience(
          moveUci = replyCapture.moveUci,
          plyOffset = replyCapture.plyOffset,
          capturedRole = replyCapture.capturedRole,
          square = replyCapture.square,
          targetValueCp = replyCapture.valueCp
        ),
        beneficiary = Black,
        durableNetCp = 400
      ))
    )
    LineFactEvidence(
      line = line,
      material = Some(LineMaterialSummary(
        sideToMove = White,
        captures = List(rootCapture, replyCapture),
        netCaptureCpForMover = -400,
        maxGainCpForMover = 500,
        maxLossCpForMover = 900,
        hasRecaptureChain = true,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = List(
        LineReplayStep(0, line.rootMove, position.fen, rootAfter),
        LineReplayStep(1, replyMove, rootAfter, replyAfter)
      ),
      events = List(
        LineMoveEvent(LineEventKind.Capture, line.rootMove, 0, Some(White), Some(EvidencePieceRole("queen")), Some(EvidencePieceRole("rook")), Some(EvidenceSquare("e4"))),
        LineMoveEvent(LineEventKind.Recapture, replyMove, 1, Some(Black), Some(EvidencePieceRole("rook")), Some(EvidencePieceRole("queen")), Some(EvidenceSquare("e4")))
      ),
      consequences = List(consequence)
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
    val moveRefs = moves.zipWithIndex.foldLeft(fen -> List.empty[PrincipalVariationEvidence.LineMoveRef]) {
      case ((before, refs), (move, ply)) =>
        val after = PrincipalVariationEvidence
          .legalFenAfter(before, move)
          .getOrElse(fail(s"expected legal material move $move"))
        after -> (refs :+ PrincipalVariationEvidence.LineMoveRef(ply, move, after))
    }._2
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
        position = PositionNodeRef(fen, 0, Some(summary.sideToMove)),
        scope = lineRef.role.scope,
        materialSummary = Some(summary)
      )
      .payload match
      case payload: LineFactEvidence => payload
      case _ => fail("expected normalized line evidence")

  private def cause(
      kind: RelativeCauseKind,
      comparisonRef: EvidenceRef,
      proofRef: EvidenceRef,
      sourceSide: RelativeCauseSourceSide
  ): RelativeCauseFact =
    val attributionKind =
      if sourceSide == RelativeCauseSourceSide.Reference then CauseAttributionKind.ReferenceCreatesResource
      else CauseAttributionKind.CandidateAllowsLiability
    RelativeCauseFact(
      kind = kind,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(proofRef),
      sourceSide = sourceSide,
      attribution = CauseAttribution(
        kind = attributionKind,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            sourceRefs = List(proofRef)
          )
        )
      )
    )
