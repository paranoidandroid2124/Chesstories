package lila.chessjudgment.analysis.qc

import lila.chessjudgment.analysis.line.{ LineFactNormalizer, PrincipalVariationEvidence }
import lila.chessjudgment.analysis.move.MoveAnalyzer
import lila.chessjudgment.analysis.position.{ FactExtractor, PositionAnalyzer, PositionFactNormalizer }
import lila.chessjudgment.analysis.singlePosition.*
import lila.chessjudgment.analysis.strategic.EndgamePatternOracle
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, StructuralDeltaContracts }
import lila.chessjudgment.model.{
  Fact,
  FactScope,
  Motif,
  PlanId,
  ProbeAdmissionDiagnostic,
  ProbeAdmissionStatus,
  ProbePurpose,
  ProbeRequest,
  TransitionType
}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.{ RookEndgameGeometry, RookEndgamePattern }
import lila.chessjudgment.model.structure.*
import chess.{ Color, File }
import chess.format.Fen
import chess.variant.Standard
class MoveReviewPhase3AuditRunnerTest extends munit.FunSuite:

  test("resolved branch probes are no longer pending regardless of admission outcome"):
    val requests = List("admitted", "rejected", "ignored", "pending").map(id =>
      ProbeRequest(
        id = id,
        fen = "8/8/8/8/8/8/8/K6k w - - 0 1",
        moves = List("a1a2"),
        depth = 16,
        purpose = Some(ProbePurpose.ReplyMultipv)
      )
    )
    val diagnostics = List(
      ProbeAdmissionDiagnostic("admitted", ProbeAdmissionStatus.Admitted, Nil, Some(ProbePurpose.ReplyMultipv)),
      ProbeAdmissionDiagnostic("rejected", ProbeAdmissionStatus.Rejected, List("BAD_RESULT"), Some(ProbePurpose.ReplyMultipv)),
      ProbeAdmissionDiagnostic("ignored", ProbeAdmissionStatus.Ignored, List("STALE_RESULT"), Some(ProbePurpose.ReplyMultipv))
    )

    assertEquals(
      SemanticCoverageMetrics.pendingBranchReplyProbeRequests(requests, diagnostics).map(_.id),
      List("pending")
    )

  test("semantic rubric marks expected questions without slots as unmeasured and incomplete"):
    val coverage = MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
      expectedSlots = Nil,
      diagnostics = Nil,
      expectedQuestionIds = List("terminal_mate", "terminal_promotion_race")
    )
    val corpusCoverage = MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCorpusCoverageJson(List(coverage))

    assertEquals((coverage \ "missingExpectedQuestionIds").as[List[String]], List.empty[String])
    assertEquals((coverage \ "unmeasuredExpectedQuestionIds").as[List[String]], List("terminal_mate", "terminal_promotion_race"))
    assertEquals((coverage \ "expectedQuestionCoverageComplete").as[Boolean], false)
    assertEquals((corpusCoverage \ "expectedQuestionCoverageComplete").as[Boolean], false)

  test("endgame projection preserves distinct meanings from one board record"):
    val root = PositionNodeRef("8/3k4/8/3K4/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val ref =
      EvidenceRef(
        id = "board:endgame-meanings",
        producer = EvidenceProducer.BoardFactProducer,
        layer = EvidenceLayer.Board,
        position = root,
        line = None,
        scope = EvidenceScope.CurrentPosition,
        confidence = EvidenceConfidence.BoardDerived
      )
    val anchors =
      List(
        BoardAnchor(
          BoardAnchorKind.EndgameTechnique,
          chess.Color.White,
          BoardAnchorSignal.EndgameKingActivity,
          magnitude = 2,
          confidence = 0.68,
          detail = Some(BoardAnchorDetail(subjectSquare = Some(EvidenceSquare("d5"))))
        ),
        BoardAnchor(
          BoardAnchorKind.EndgameTechnique,
          chess.Color.White,
          BoardAnchorSignal.EndgameOpposition,
          magnitude = 3,
          confidence = 0.76,
          detail = Some(
            BoardAnchorDetail(
              subjectSquare = Some(EvidenceSquare("d5")),
              targetSquare = Some(EvidenceSquare("d7"))
            )
          )
        )
      )
    val payload = new BoardFactEvidence(Nil, None)(anchors = anchors)
    val details =
      PositionPlanTechniqueProjection
        .frames(TypedEvidenceGraph(List(EvidenceRecord(ref, payload))), Nil, Nil, None)
        .flatMap(_.semanticDetails)

    assertEquals(details.size, 2)
    assertEquals(
      details.flatMap(_.boardAnchorSignals).toSet,
      Set(BoardAnchorSignal.EndgameKingActivity.toString, BoardAnchorSignal.EndgameOpposition.toString)
    )

  test("endgame king facts preserve their side and opposition has one owner"):
    val fen = "8/8/3k4/8/3K4/8/8/8 w - - 0 1"
    val position = Fen.read(Standard, Fen.Full(fen)).get
    val facts =
      (
        FactExtractor.fromMotifs(position.board, MoveAnalyzer.detectStateMotifs(position, 0), FactScope.Now) ++
          List(chess.Color.White, chess.Color.Black).flatMap { side =>
            FactExtractor.extractEndgameFacts(
              position.board,
              side,
              EndgamePatternOracle.analyze(position.board, side)
            )
          }
      ).distinct
    val record =
      PositionFactNormalizer.fromBoardFacts(
        id = "board:endgame-side-ownership",
        facts = facts,
        features = None,
        position = PositionNodeRef(fen, 0, Some(position.color), Some("root")),
        scope = EvidenceScope.CurrentPosition
      )
    val anchors = record.payload.asInstanceOf[BoardFactEvidence].boardAnchors
    val opposition = anchors.filter(_.signal == BoardAnchorSignal.EndgameOpposition)
    val activity = anchors.filter(_.signal == BoardAnchorSignal.EndgameKingActivity)

    assertEquals(opposition.map(_.side), List(chess.Color.Black))
    assertEquals(opposition.flatMap(_.detail.flatMap(_.subjectSquare.map(_.key))), List("d6"))
    assertEquals(opposition.flatMap(_.detail.flatMap(_.targetSquare.map(_.key))), List("d4"))
    assertEquals(
      activity.map(anchor => anchor.side -> anchor.detail.flatMap(_.subjectSquare.map(_.key))).toSet,
      Set[(chess.Color, Option[String])](chess.Color.White -> Some("d4"), chess.Color.Black -> Some("d6"))
    )

  test("line endgame horizons surface through EndgameTechniqueRecipe details"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val line = lineRef("lucena-line", "d7d8q", 1, LineNodeRole.BestReference)
    val ref =
      EvidenceRef(
        id = "line:lucena-horizon",
        producer = EvidenceProducer.LegalLineProducer,
        layer = EvidenceLayer.Line,
        position = root,
        line = Some(line),
        scope = EvidenceScope.BestLine,
        confidence = EvidenceConfidence.LegalReplayVerified
      )
    val horizon =
      LineEndgameTechniqueHorizon(
        pattern = "Lucena",
        rookPattern = Some("RookBehindPassedPawn"),
        techniqueSide = chess.Color.White,
        entryPlyOffset = 0,
        terminalPlyOffset = 2,
        status = LineEndgameTechniqueHorizonStatus.Transitioned,
        triggerMove = Some("d7d8q"),
        requiredSquares = List("d8", "d7", "e7"),
        maintainedSquares = List("d8", "d7", "e7")
      )
    val payload =
      new LineFactEvidence(line, Some("d7d8q"), None, List("e8e7"), None, None)(
        endgameHorizons = List(horizon)
      )

    val frames = PositionPlanTechniqueProjection.frames(TypedEvidenceGraph(List(EvidenceRecord(ref, payload))), Nil, Nil, None)

    assertEquals(frames.size, 1)
    val frame = frames.head
    assertEquals(frame.units, List(PositionPlanTechniqueUnit.EndgameTechniqueRecipe))
    assert(frame.objectBindingSignatures.exists(_.contains("target=Square:d8")), frame.objectBindingSignatures)
    val detail = frame.semanticDetails.head
    assertEquals(detail.endgameTechniquePattern, Some("Lucena"))
    assertEquals(detail.endgameTechniqueRookPattern, Some("RookBehindPassedPawn"))
    assertEquals(detail.endgameTechniqueHorizonStatus, Some("Transitioned"))
    assertEquals(detail.endgameTechniqueTriggerMove, Some("d7d8q"))
    assertEquals(detail.endgameTechniqueEntryPlyOffset, Some(0))
    assertEquals(detail.endgameTechniqueTerminalPlyOffset, Some(2))
    assertEquals(detail.requiredSquares, List("d7", "d8", "e7"))
    assertEquals(detail.maintainedSquares, List("d7", "d8", "e7"))

  test("line normalizer proves triangulation only from a completed tempo-transfer cycle"):
    val startFen = "8/p2k4/8/8/3K4/8/P7/8 w - - 0 1"

    def recordFor(id: String, moves: List[String]): EvidenceRecord =
      val lineMoves = moves.zipWithIndex.foldLeft((List.empty[PrincipalVariationEvidence.LineMoveRef], startFen)) {
        case ((built, fen), (move, index)) =>
          val fenAfter = PrincipalVariationEvidence.legalFenAfter(fen, move).get
          (built :+ PrincipalVariationEvidence.LineMoveRef(index + 1, move, fenAfter), fenAfter)
      }._1
      val validated =
        PrincipalVariationEvidence
          .validatedLine(
            startFen,
            PrincipalVariationEvidence.LineVariationRef(lineMoves),
            moves.head
          )
          .get
      val facts =
        PrincipalVariationEvidence.LineFacts(
          line = validated.line,
          first = validated.moves.head,
          reply = validated.reply,
          continuation = validated.continuation,
          continuationTail = validated.moves.drop(3)
        )
      LineFactNormalizer.fromValidatedLine(
        id = s"line:$id",
        lineRef = lineRef(id, moves.head, 1, LineNodeRole.Played),
        facts = facts,
        position = PositionNodeRef(startFen, 1, Some(chess.Color.White), Some("root")),
        scope = EvidenceScope.PlayedLine
      )

    def lineFacts(record: EvidenceRecord): LineFactEvidence =
      record.payload match
        case payload: LineFactEvidence => payload
        case _                         => fail("expected line fact evidence")

    val completed = recordFor("triangulation", List("d4c4", "d7e7", "c4c5", "e7d7", "c5d4"))
    val simpleReturn = recordFor("king-return", List("d4c4", "d7e7", "c4d4", "e7d7"))
    val horizon = lineFacts(completed).endgameTechniqueHorizons.find(_.pattern == "Triangulation").get
    val detail =
      PositionPlanTechniqueProjection
        .frames(TypedEvidenceGraph(List(completed)), Nil, Nil, None)
        .flatMap(_.semanticDetails)
        .find(_.endgameTechniquePattern.contains("Triangulation"))
        .get

    assertEquals(horizon.rookPattern, None)
    assertEquals(horizon.status, LineEndgameTechniqueHorizonStatus.Completed)
    assertEquals(horizon.triggerMove, Some("d4c4"))
    assertEquals(horizon.entryPlyOffset, 0)
    assertEquals(horizon.terminalPlyOffset, 4)
    assertEquals(horizon.requiredSquares, List("c4", "c5", "d4"))
    assertEquals(horizon.maintainedSquares, Nil)
    assertEquals(horizon.brokenSquares, Nil)
    assertEquals(detail.endgameTechniqueRookPattern, None)
    assertEquals(detail.endgameTechniqueHorizonStatus, Some("Completed"))
    assertEquals(
      detail.endgameTechniqueLineObservation,
      Some(
        EndgameTechniqueLineObservation(
          kingRouteMoves = List("d4c4", "c4c5", "c5d4"),
          tempoRecipient = "black"
        )
      )
    )
    assert(!lineFacts(simpleReturn).endgameTechniqueHorizons.exists(_.pattern == "Triangulation"))

  test("terminal result takes precedence over the rook-technique cause"):
    val line = lineRef("mate-line", "d7d8q", 1, LineNodeRole.BestReference)
    val mate = LineConsequence(
      LineConsequenceKind.Mate,
      List("d7d8q", "e8e7", "d8d1"),
      proofSignal = true,
      eventMove = Some("d7d8q"),
      rootMove = Some("d7d8q")
    )
    val lucenaOverride =
      LineEndgameTechniqueHorizon(
        pattern = "Lucena",
        rookPattern = Some("RookBehindPassedPawn"),
        techniqueSide = chess.Color.White,
        entryPlyOffset = 0,
        terminalPlyOffset = 2,
        status = LineEndgameTechniqueHorizonStatus.SupersededByTactic,
        triggerMove = Some("d7d8q"),
        requiredSquares = List("d8"),
        maintainedSquares = List("d8"),
        terminalConsequenceKinds = List(LineConsequenceKind.Mate),
        failureReason = Some("terminal-proof-supersedes-technique")
      )
    val philidorOverride =
      lucenaOverride.copy(
        pattern = "PhilidorDefense",
        rookPattern = Some("KingCutOff"),
        status = LineEndgameTechniqueHorizonStatus.ContradictedByTerminalProof,
        failureReason = Some("terminal-proof-overrides-technique")
      )
    val payload =
      new LineFactEvidence(line, Some("d7d8q"), None, List("e8e7", "d8d1"), None, None)(
        consequences = List(mate),
        endgameHorizons = List(lucenaOverride, philidorOverride)
      )

    assert(payload.hasTerminalEndgameTechniqueOverride)
    assertEquals(payload.maintainedWinningEndgameTechniqueHorizons, Nil)
    assertEquals(payload.maintainedDefensiveEndgameTechniqueHorizons, Nil)
    assert(payload.endgameTechniquesTriggeredByRootMove("d7d8q", RelativeCauseKind.ConversionSecured).isEmpty)
    assert(payload.endgameTechniquesTriggeredByRootMove("d7d8q", RelativeCauseKind.DrawResource).isEmpty)
    val details =
      PositionPlanTechniqueProjection
        .frames(
          TypedEvidenceGraph(
            List(
              EvidenceRecord(
                EvidenceRef(
                  id = "line:terminal-overrides-technique",
                  producer = EvidenceProducer.LegalLineProducer,
                  layer = EvidenceLayer.Line,
                  position = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root")),
                  line = Some(line),
                  scope = EvidenceScope.BestLine,
                  confidence = EvidenceConfidence.LegalReplayVerified
                ),
                payload
              )
            )
          ),
          Nil,
          Nil,
          None
        )
        .flatMap(_.semanticDetails)
    val terminalDetail =
      details.find(detail => detail.terminalConsequenceKinds.contains("Mate") && detail.endgameTechniquePattern.isEmpty).get
    val techniqueDetails = details.filter(_.endgameTechniquePattern.nonEmpty)

    assert(!terminalDetail.objectBindingSignatures.exists(_.toLowerCase.contains("endgametechniquehorizon")))
    assert(techniqueDetails.nonEmpty)
    assert(
      techniqueDetails.forall(detail =>
        detail.objectBindingSignatures.nonEmpty &&
          detail.objectBindingSignatures.forall(_.toLowerCase.contains("endgametechniquehorizon"))
      )
    )

  test("line terminal proof requires explicit matching root ownership"):
    val line = lineRef("unowned-terminal", "d7d8q", 1, LineNodeRole.BestReference)
    val evidenceRef = EvidenceRef(
      id = "line:unowned-terminal",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root")),
      line = Some(line),
      scope = EvidenceScope.BestLine,
      confidence = EvidenceConfidence.LegalReplayVerified
    )

    List(None, Some("e8e7")).foreach { consequenceRootMove =>
      val payload = new LineFactEvidence(line, Some("d7d8q"), None, List("e8e7", "d8d1"), None, None)(
        consequences = List(LineConsequence(
          LineConsequenceKind.Mate,
          List("d7d8q", "e8e7", "d8d1"),
          proofSignal = true,
          eventMove = Some("d8d1"),
          rootMove = consequenceRootMove
        ))
      )
      val frames = PositionPlanTechniqueProjection.frames(
        TypedEvidenceGraph(List(EvidenceRecord(evidenceRef, payload))),
        Nil,
        Nil,
        None
      )

      assertEquals(frames, Nil)
    }

  test("line normalizer marks broken squares when a rook technique horizon fails"):
    val startFen = "6K1/3k1P2/8/8/R7/8/8/7r w - - 0 1"
    val afterFen = PrincipalVariationEvidence.legalFenAfter(startFen, "f7f8q").get
    val rawLine =
      PrincipalVariationEvidence.LineVariationRef(
        List(PrincipalVariationEvidence.LineMoveRef(1, "f7f8q", afterFen))
      )
    val validated = PrincipalVariationEvidence.validatedLine(startFen, rawLine, "f7f8q").get
    val facts =
      PrincipalVariationEvidence.LineFacts(
        line = validated.line,
        first = validated.moves.head,
        reply = validated.reply,
        continuation = validated.continuation,
        continuationTail = validated.moves.drop(3)
      )
    val position = PositionNodeRef(startFen, 1, Some(chess.Color.White), Some("root"))
    val record =
      LineFactNormalizer.fromValidatedLine(
        id = "line:lucena-fails",
        lineRef = lineRef("lucena-fails", "f7f8q", 1, LineNodeRole.BestReference),
        facts = facts,
        position = position,
        scope = EvidenceScope.BestLine
      )
    val payload =
      record.payload match
        case payload: LineFactEvidence => payload
        case _                         => fail("expected line fact evidence")
    val horizons = payload.endgameTechniqueHorizons
    val lucena = horizons.find(_.pattern == "Lucena").get

    assertEquals(lucena.status, LineEndgameTechniqueHorizonStatus.Failed)
    assertEquals(lucena.triggerMove, Some("f7f8q"))
    assert(lucena.requiredSquares.nonEmpty)
    assertEquals(lucena.brokenSquares, lucena.requiredSquares)
    assert(payload.endgameTechniquesTriggeredByRootMove("f7f8q", RelativeCauseKind.ConversionMiss).nonEmpty)

  test("line normalizer uses breaking move as trigger for mid-line rook technique failure"):
    val startFen = "8/2KP4/4k3/8/4R3/8/8/7r w - - 0 1"
    val afterBridge = PrincipalVariationEvidence.legalFenAfter(startFen, "e4d4").get
    val afterWaitingMove = PrincipalVariationEvidence.legalFenAfter(afterBridge, "h1h2").get
    val afterBreak = PrincipalVariationEvidence.legalFenAfter(afterWaitingMove, "d4d3").get
    val rawLine =
      PrincipalVariationEvidence.LineVariationRef(
        List(
          PrincipalVariationEvidence.LineMoveRef(1, "e4d4", afterBridge),
          PrincipalVariationEvidence.LineMoveRef(2, "h1h2", afterWaitingMove),
          PrincipalVariationEvidence.LineMoveRef(3, "d4d3", afterBreak)
        )
      )
    val validated = PrincipalVariationEvidence.validatedLine(startFen, rawLine, "e4d4").get
    val facts =
      PrincipalVariationEvidence.LineFacts(
        line = validated.line,
        first = validated.moves.head,
        reply = validated.reply,
        continuation = validated.continuation,
        continuationTail = validated.moves.drop(3)
      )
    val position = PositionNodeRef(startFen, 1, Some(chess.Color.White), Some("root"))
    val record =
      LineFactNormalizer.fromValidatedLine(
        id = "line:lucena-mid-line-fails",
        lineRef = lineRef("lucena-mid-line-fails", "e4d4", 1, LineNodeRole.BestReference),
        facts = facts,
        position = position,
        scope = EvidenceScope.BestLine
      )
    val payload =
      record.payload match
        case payload: LineFactEvidence => payload
        case _                         => fail("expected line fact evidence")
    val lucena = payload.endgameTechniqueHorizons.find(_.pattern == "Lucena").get

    assertEquals(lucena.entryPlyOffset, 0)
    assertEquals(lucena.status, LineEndgameTechniqueHorizonStatus.Failed)
    assertEquals(lucena.triggerMove, Some("d4d3"))
    assert(payload.endgameTechniquesTriggeredByRootMove("e4d4", RelativeCauseKind.ConversionMiss).isEmpty)
    assert(payload.endgameTechniquesTriggeredByRootMove("d4d3", RelativeCauseKind.ConversionMiss).nonEmpty)

  test("line normalizer keeps Vancura technique without inventing terminal draw proof"):
    val startFen = "8/8/k7/P7/K7/r7/8/7R b - - 0 1"
    val afterFen = PrincipalVariationEvidence.legalFenAfter(startFen, "a3g3").get
    val rawLine =
      PrincipalVariationEvidence.LineVariationRef(
        List(PrincipalVariationEvidence.LineMoveRef(1, "a3g3", afterFen))
      )
    val validated = PrincipalVariationEvidence.validatedLine(startFen, rawLine, "a3g3").get
    val facts =
      PrincipalVariationEvidence.LineFacts(
        line = validated.line,
        first = validated.moves.head,
        reply = validated.reply,
        continuation = validated.continuation,
        continuationTail = validated.moves.drop(3)
      )
    val position = PositionNodeRef(startFen, 1, Some(chess.Color.Black), Some("root"))
    val record =
      LineFactNormalizer.fromValidatedLine(
        id = "line:vancura-draw-resource",
        lineRef = lineRef("vancura-draw-resource", "a3g3", 1, LineNodeRole.BestReference),
        facts = facts,
        position = position,
        scope = EvidenceScope.BestLine
      )
    val payload =
      record.payload match
        case payload: LineFactEvidence => payload
        case _                         => fail("expected line fact evidence")
    val vancura = payload.endgameTechniqueHorizons.find(_.pattern == "VancuraDefense").get
    val detail =
      PositionPlanTechniqueProjection
        .frames(TypedEvidenceGraph(List(record)), Nil, Nil, None)
        .flatMap(_.semanticDetails)
        .find(_.endgameTechniquePattern.contains("VancuraDefense"))
        .get

    assertEquals(vancura.rookPattern, Some(RookEndgamePattern.RookBehindPassedPawn.toString))
    assertEquals(vancura.status, LineEndgameTechniqueHorizonStatus.Transitioned)
    assertEquals(vancura.triggerMove, Some("a3g3"))
    assert(vancura.requiredSquares.nonEmpty)
    assert(vancura.maintainedSquares.nonEmpty)
    assertEquals(vancura.brokenSquares, Nil)
    assert(payload.endgameTechniquesTriggeredByRootMove("a3g3", RelativeCauseKind.DrawResource).nonEmpty)
    assertEquals(payload.proofSignalConsequencesOf(LineConsequenceKind.DrawResource), Nil)
    assertEquals(detail.endgameTechniqueRookPattern, Some(RookEndgamePattern.RookBehindPassedPawn.toString))
    assertEquals(detail.endgameTechniqueHorizonStatus, Some(LineEndgameTechniqueHorizonStatus.Transitioned.toString))
    assertEquals(detail.endgameTechniqueTriggerMove, Some("a3g3"))
    assert(detail.requiredSquares.nonEmpty)

  test("line normalizer does not bind a later material capture to a quiet root move"):
    val startFen = "r3k3/8/8/8/8/8/7P/R3K3 w - - 0 1"
    val afterRoot = PrincipalVariationEvidence.legalFenAfter(startFen, "h2h3").get
    val afterReply = PrincipalVariationEvidence.legalFenAfter(afterRoot, "a8a7").get
    val afterCapture = PrincipalVariationEvidence.legalFenAfter(afterReply, "a1a7").get
    val rawLine =
      PrincipalVariationEvidence.LineVariationRef(
        List(
          PrincipalVariationEvidence.LineMoveRef(1, "h2h3", afterRoot),
          PrincipalVariationEvidence.LineMoveRef(2, "a8a7", afterReply),
          PrincipalVariationEvidence.LineMoveRef(3, "a1a7", afterCapture)
        )
      )
    val validated = PrincipalVariationEvidence.validatedLine(startFen, rawLine, "h2h3").get
    val facts =
      PrincipalVariationEvidence.LineFacts(
        line = validated.line,
        first = validated.moves.head,
        reply = validated.reply,
        continuation = validated.continuation,
        continuationTail = validated.moves.drop(3)
      )
    val materialSummary =
      LineMaterialSummary(
        sideToMove = chess.Color.White,
        captures = List(
          LineMaterialCapture(
            moveUci = "a1a7",
            plyOffset = 2,
            side = chess.Color.White,
            attackerRole = EvidencePieceRole("Rook"),
            capturedRole = EvidencePieceRole("Rook"),
            square = EvidenceSquare("a7"),
            valueCp = 500,
            recapture = false
          )
        ),
        netCaptureCpForMover = 500,
        maxGainCpForMover = 500,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
    val position = PositionNodeRef(startFen, 1, Some(chess.Color.White), Some("root"))
    val record =
      LineFactNormalizer.fromValidatedLine(
        id = "line:quiet-root-later-material",
        lineRef = lineRef("quiet-root-later-material", "h2h3", 1, LineNodeRole.BestReference),
        facts = facts,
        position = position,
        scope = EvidenceScope.BestLine,
        materialSummary = Some(materialSummary)
      )
    val payload =
      record.payload match
        case payload: LineFactEvidence => payload
        case _                         => fail("expected line fact evidence")
    val materialProof = payload.proofSignalConsequencesOf(LineConsequenceKind.MaterialGain).head

    assertEquals(materialProof.eventMove, Some("a1a7"))
    assertEquals(materialProof.rootMove, None)
    assertEquals(materialProof.lineMoves.headOption, Some("a1a7"))
    assert(!materialProof.rootMoveMatched("h2h3"), materialProof)
    assertEquals(
      payload.consequencesForRootMove("h2h3").filter(_.kind == LineConsequenceKind.MaterialGain),
      Nil
    )

  test("line normalizer gives material gain to the first capture whose gain survives every reply"):
    def payload(
        id: String,
        startFen: String,
        moves: List[String],
        captures: List[LineMaterialCapture],
        netCaptureCp: Int
    ): LineFactEvidence =
      val (_, moveRefs) = moves.zipWithIndex.foldLeft(startFen -> List.empty[PrincipalVariationEvidence.LineMoveRef]) {
        case ((fenBefore, accumulated), (move, index)) =>
          val fenAfter = PrincipalVariationEvidence.legalFenAfter(fenBefore, move).getOrElse(fail(s"illegal $move"))
          fenAfter -> (accumulated :+ PrincipalVariationEvidence.LineMoveRef(index + 1, move, fenAfter))
      }
      val rawLine = PrincipalVariationEvidence.LineVariationRef(moveRefs)
      val validated = PrincipalVariationEvidence.validatedLine(startFen, rawLine, moves.head).get
      val facts = PrincipalVariationEvidence.LineFacts(
        line = validated.line,
        first = validated.moves.head,
        reply = validated.reply,
        continuation = validated.continuation,
        continuationTail = validated.moves.drop(3)
      )
      val running = captures
        .map(capture => if capture.side == captures.head.side then capture.valueCp else -capture.valueCp)
        .scanLeft(0)(_ + _)
        .tail
      val summary = LineMaterialSummary(
        sideToMove = captures.head.side,
        captures = captures,
        netCaptureCpForMover = netCaptureCp,
        maxGainCpForMover = (0 :: running).max,
        maxLossCpForMover = (0 :: running).min,
        hasRecaptureChain = captures.exists(_.recapture),
        hasRecoveryWindow = running.exists(_ < 0) && running.exists(_ >= 0),
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
      LineFactNormalizer
        .fromValidatedLine(
          id = id,
          lineRef = lineRef(id, moves.head, 1, LineNodeRole.Played),
          facts = facts,
          position = PositionNodeRef(startFen, 1, Some(captures.head.side), Some("root")),
          scope = EvidenceScope.PlayedTransition,
          materialSummary = Some(summary)
        )
        .payload
        .asInstanceOf[LineFactEvidence]

    val recoveredRoot = payload(
      id = "line:recovered-root-later-gain",
      startFen = "1brr2k1/3q1pp1/pp2p2p/4P3/P2P3P/1N2Q1P1/1P1R1PK1/2R5 b - - 0 30",
      moves = List("c8c1", "b3c1", "d7c6", "d4d5", "c6c1"),
      captures = List(
        LineMaterialCapture("c8c1", 0, Color.Black, EvidencePieceRole("Rook"), EvidencePieceRole("Rook"), EvidenceSquare("c1"), 500, recapture = false),
        LineMaterialCapture("b3c1", 1, Color.White, EvidencePieceRole("Knight"), EvidencePieceRole("Rook"), EvidenceSquare("c1"), 500, recapture = true),
        LineMaterialCapture("c6c1", 4, Color.Black, EvidencePieceRole("Queen"), EvidencePieceRole("Knight"), EvidenceSquare("c1"), 300, recapture = false)
      ),
      netCaptureCp = 300
    )
    val recoveredGain = recoveredRoot.proofSignalConsequencesOf(LineConsequenceKind.MaterialGain).head
    assertEquals(recoveredGain.eventMove, Some("c6c1"))
    assertEquals(recoveredGain.rootMove, None)
    assertEquals(recoveredGain.lineMoves.headOption, Some("c6c1"))
    assertEquals(
      recoveredRoot.consequencesForRootMove("c8c1").filter(_.kind == LineConsequenceKind.MaterialGain),
      Nil
    )

    val declinedOffer = payload(
      id = "line:declined-exchange-offer",
      startFen = "4k3/8/8/8/8/2n5/1b6/2R1K3 w - - 0 1",
      moves = List("c1c3"),
      captures = List(
        LineMaterialCapture("c1c3", 0, Color.White, EvidencePieceRole("Rook"), EvidencePieceRole("Knight"), EvidenceSquare("c3"), 300, recapture = false)
      ),
      netCaptureCp = 300
    )
    val declinedGain = declinedOffer.proofSignalConsequencesOf(LineConsequenceKind.MaterialGain).head
    assertEquals(declinedGain.eventMove, None)
    assertEquals(declinedGain.rootMove, None)
    assertEquals(declinedGain.lineMoves, Nil)
    assertEquals(
      declinedOffer.consequencesForRootMove("c1c3").filter(_.kind == LineConsequenceKind.MaterialGain),
      Nil
    )
    val declinedSacrifice = declinedOffer.proofSignalConsequencesOf(LineConsequenceKind.Sacrifice).head
    assertEquals(declinedSacrifice.eventMove, Some("c1c3"))
    assertEquals(declinedSacrifice.rootMove, Some("c1c3"))
    assert(declinedSacrifice.rootMoveMatched("c1c3"), declinedSacrifice)
    assertEquals(
      declinedOffer.consequencesForRootMove("c1c3").filter(_.kind == LineConsequenceKind.Sacrifice),
      List(declinedSacrifice)
    )

    val equalExchange = payload(
      id = "line:declined-equal-exchange",
      startFen = "4k3/8/8/8/8/2r5/1b6/2R1K3 w - - 0 1",
      moves = List("c1c3"),
      captures = List(
        LineMaterialCapture("c1c3", 0, Color.White, EvidencePieceRole("Rook"), EvidencePieceRole("Rook"), EvidenceSquare("c3"), 500, recapture = false)
      ),
      netCaptureCp = 500
    )
    val equalGain = equalExchange.proofSignalConsequencesOf(LineConsequenceKind.MaterialGain).head
    assertEquals(equalGain.eventMove, None)
    assertEquals(equalGain.rootMove, None)
    assertEquals(equalExchange.proofSignalConsequencesOf(LineConsequenceKind.Sacrifice), Nil)

    val profitableAcceptance = payload(
      id = "line:profitable-accepted-capture",
      startFen = "4k3/6p1/7r/8/8/8/8/2B1K3 w - - 0 1",
      moves = List("c1h6"),
      captures = List(
        LineMaterialCapture("c1h6", 0, Color.White, EvidencePieceRole("Bishop"), EvidencePieceRole("Rook"), EvidenceSquare("h6"), 500, recapture = false)
      ),
      netCaptureCp = 500
    )
    val profitableGain = profitableAcceptance.proofSignalConsequencesOf(LineConsequenceKind.MaterialGain).head
    assertEquals(profitableGain.eventMove, Some("c1h6"))
    assertEquals(profitableGain.rootMove, Some("c1h6"))
    assert(profitableGain.rootMoveMatched("c1h6"))
    assertEquals(profitableAcceptance.proofSignalConsequencesOf(LineConsequenceKind.Sacrifice), Nil)

  test("material and promotion consequences keep explicit event ownership"):
    val quietPromotionSummary =
      LineMaterialSummary(
        sideToMove = Color.White,
        captures = Nil,
        netCaptureCpForMover = 800,
        maxGainCpForMover = 800,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 800,
        materialWindowComplete = true
      )
    val (_, quietPromotion) = normalizedLinePayload(
      id = "line:quiet-root-later-promotion",
      startFen = "7k/5P2/8/8/8/8/8/K7 w - - 0 1",
      moves = List("a1a2", "h8h7", "f7f8q"),
      summary = quietPromotionSummary
    )
    val quietGain = quietPromotion.proofSignalConsequencesOf(LineConsequenceKind.MaterialGain).head
    val quietPromotionProof = quietPromotion.proofSignalConsequencesOf(LineConsequenceKind.Promotion).head

    assertEquals(quietGain.eventMove, Some("f7f8q"))
    assertEquals(quietGain.rootMove, None)
    assertEquals(quietPromotionProof.eventMove, Some("f7f8q"))
    assertEquals(quietPromotionProof.rootMove, None)
    assertEquals(
      quietPromotion.consequencesForRootMove("a1a2").filter(consequence =>
        consequence.kind == LineConsequenceKind.MaterialGain || consequence.kind == LineConsequenceKind.Promotion
      ),
      Nil
    )

    val raceSummary = quietPromotionSummary.copy(
      netCaptureCpForMover = 600,
      maxGainCpForMover = 800,
      promotionGainCpForMover = 600
    )
    val (_, race) = normalizedLinePayload(
      id = "line:promotion-race-beneficiary",
      startFen = "8/5P1k/8/8/8/8/K1p5/8 w - - 0 1",
      moves = List("f7f8q", "c2c1n"),
      summary = raceSummary
    )
    val raceGain = race.proofSignalConsequencesOf(LineConsequenceKind.MaterialGain).head
    val raceProof = race.proofSignalConsequencesOf(LineConsequenceKind.PromotionRace).head

    assertEquals(
      race.lineEventsOf(LineEventKind.Promotion).map(event => (event.moveUci, event.plyOffset, event.side)),
      List(("f7f8q", 0, Some(Color.White)), ("c2c1n", 1, Some(Color.Black)))
    )
    assertEquals(raceGain.eventMove, Some("f7f8q"))
    assertEquals(raceGain.beneficiary, Some(Color.White))
    assertEquals(raceProof.eventMove, Some("f7f8q"))
    assertEquals(raceProof.rootMove, Some("f7f8q"))
    assertEquals(raceProof.beneficiary, Some(Color.White))

  test("later loss and hanging capture use their own lasting material event"):
    val lossSummary =
      LineMaterialSummary(
        sideToMove = Color.White,
        captures = List(
          LineMaterialCapture("a1a7", 0, Color.White, EvidencePieceRole("Rook"), EvidencePieceRole("Pawn"), EvidenceSquare("a7"), 100, recapture = false),
          LineMaterialCapture("h8h1", 3, Color.Black, EvidencePieceRole("Rook"), EvidencePieceRole("Queen"), EvidenceSquare("h1"), 900, recapture = false)
        ),
        netCaptureCpForMover = -800,
        maxGainCpForMover = 100,
        maxLossCpForMover = -800,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
    val (lossRecord, loss) = normalizedLinePayload(
      id = "line:root-capture-unrelated-later-loss",
      startFen = "4k2r/p7/8/8/8/8/8/R3K2Q w - - 0 1",
      moves = List("a1a7", "e8f8", "e1f2", "h8h1"),
      summary = lossSummary
    )
    val lossProof = loss.proofSignalConsequencesOf(LineConsequenceKind.MaterialLoss).head

    assertEquals(lossProof.eventMove, Some("h8h1"))
    assertEquals(lossProof.rootMove, None)
    assertEquals(lossProof.lineMoves.headOption, Some("h8h1"))
    assertEquals(loss.consequencesForRootMove("a1a7").filter(_.kind == LineConsequenceKind.MaterialLoss), Nil)
    assert(
      !LineConsequenceProof(
        source = lossRecord.ref,
        kind = LineConsequenceKind.MaterialLoss,
        eventMove = Some("h8h1"),
        lineMoves = List("a1a7", "h8h1")
      ).rootMoveMatched("a1a7")
    )

    val hangingSummary =
      LineMaterialSummary(
        sideToMove = Color.White,
        captures = List(
          LineMaterialCapture("a1a7", 2, Color.White, EvidencePieceRole("Rook"), EvidencePieceRole("Rook"), EvidenceSquare("a7"), 500, recapture = false)
        ),
        netCaptureCpForMover = 500,
        maxGainCpForMover = 500,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )
    val (_, hanging) = normalizedLinePayload(
      id = "line:later-hanging-capture",
      startFen = "q3k3/r7/8/8/8/8/7P/R3K3 w - - 0 1",
      moves = List("h2h3", "e8f8", "a1a7"),
      summary = hangingSummary
    )
    val hangingGain = hanging.proofSignalConsequencesOf(LineConsequenceKind.MaterialGain).head

    assertEquals(hangingGain.eventMove, None)
    assertEquals(hangingGain.rootMove, None)
    assertEquals(hangingGain.lineMoves, Nil)
    assertEquals(hanging.consequencesForRootMove("h2h3").filter(_.kind == LineConsequenceKind.MaterialGain), Nil)

  test("non-capture promotion terminal proof owns root move before suppressing rook technique"):
    val startFen = "6K1/3k1P2/8/8/R7/8/8/7r w - - 0 1"
    val afterFen = PrincipalVariationEvidence.legalFenAfter(startFen, "f7f8q").get
    val rawLine =
      PrincipalVariationEvidence.LineVariationRef(
        List(PrincipalVariationEvidence.LineMoveRef(1, "f7f8q", afterFen))
      )
    val validated = PrincipalVariationEvidence.validatedLine(startFen, rawLine, "f7f8q").get
    val facts =
      PrincipalVariationEvidence.LineFacts(
        line = validated.line,
        first = validated.moves.head,
        reply = validated.reply,
        continuation = validated.continuation,
        continuationTail = validated.moves.drop(3)
      )
    val materialSummary =
      LineMaterialSummary(
        sideToMove = chess.Color.White,
        captures = Nil,
        netCaptureCpForMover = 800,
        maxGainCpForMover = 800,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 800,
        materialWindowComplete = true
      )
    val position = PositionNodeRef(startFen, 1, Some(chess.Color.White), Some("root"))
    val record =
      LineFactNormalizer.fromValidatedLine(
        id = "line:lucena-promotion-terminal",
        lineRef = lineRef("lucena-promotion-terminal", "f7f8q", 1, LineNodeRole.BestReference),
        facts = facts,
        position = position,
        scope = EvidenceScope.BestLine,
        materialSummary = Some(materialSummary)
      )
    val payload =
      record.payload match
        case payload: LineFactEvidence => payload
        case _                         => fail("expected line fact evidence")
    val lucena = payload.endgameTechniqueHorizons.find(_.pattern == "Lucena").get
    val promotionProof = payload.proofSignalConsequencesOf(LineConsequenceKind.Promotion).head
    val materialProof = payload.proofSignalConsequencesOf(LineConsequenceKind.MaterialGain).head

    assertEquals(lucena.status, LineEndgameTechniqueHorizonStatus.SupersededByTactic)
    assert(promotionProof.rootMoveMatched("f7f8q"), promotionProof)
    assert(materialProof.rootMoveMatched("f7f8q"), materialProof)
    assert(payload.endgameTechniquesTriggeredByRootMove("f7f8q", RelativeCauseKind.ConversionSecured).isEmpty)

  test("recognition view slots require a public carrier signature"):
    val diagnostic =
      comparisonDiagnostic(
        id = "cmp-route-recognition-carrierless",
        referenceLeadAxes = Nil,
        producedKinds = Nil,
        flows = Nil,
        primaryRootKinds = Nil,
        primaryRootIds = Nil,
        positionPlanTechniqueFrameIds = List("frame-route-recognition-carrierless"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.PieceRerouteRoute),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.PieceRerouteRoute),
        positionPlanTechniqueSemanticDetailTokens = List("unit:PieceRerouteRoute"),
        positionPlanTechniqueSemanticDetailTokenGroups = List(List("unit:PieceRerouteRoute")),
        publicMoveMeaningClaimDiagnostics = Nil
      )
    val recognitionSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "route-recognition-carrierless",
        unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
        requiredSupportLevel = Some("view_surfaced")
      )

    val coverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(recognitionSlot), List(diagnostic))

    assertEquals((coverage \ "slots" \ 0 \ "matched").as[Boolean], false)

  test("threat pressure marks active pawn counterplay as race only with root motif"):
    val fen = "4k3/8/8/8/2P5/8/8/4K3 w - - 0 1"
    val activePv =
      List(
        PvLine(List("c4c5"), sideRelativeEvalCp = 320, mate = None, depth = 20),
        PvLine(List("e1e2"), sideRelativeEvalCp = -320, mate = None, depth = 20)
      )
    val quietPv =
      List(
        PvLine(List("e1e2"), sideRelativeEvalCp = 320, mate = None, depth = 20),
        PvLine(List("c4c5"), sideRelativeEvalCp = -320, mate = None, depth = 20)
      )
    val motifs =
      List(
        Motif.Initiative(Color.Black, score = 100, plyIndex = 0, move = None),
        Motif.PawnAdvance(File.C, fromRank = 4, toRank = 5, Color.White, plyIndex = 0, move = Some("c4c5"))
      )

    val activeSummary =
      ThreatPressureAssessor.analyze(fen, motifs, activePv, dynamicAssessment, Color.White)
    val quietSummary =
      ThreatPressureAssessor.analyze(fen, motifs, quietPv, dynamicAssessment, Color.White)

    assertEquals(activeSummary.counterThreatBetter, true)
    assertEquals(activeSummary.defense.counterIsBetter, true)
    assertEquals(quietSummary.counterThreatBetter, false)

  test("pawn play strategic axis label preserves break file tension policy and squares"):
    val position = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val record =
      EvidenceRecord(
        evidenceRef("pawn-structure", position),
        PawnStructureFactEvidence(
          StructureProfile(
            primary = StructureId.FluidCenter,
            confidence = 0.8,
            alternatives = Nil,
            centerState = CenterState.Fluid,
            evidenceCodes = Nil
          ),
          alignment = None,
          pawnPlay = Some(
            PawnPlayAnalysis(
              pawnBreakReady = true,
              breakFile = Some("e"),
              breakImpact = 80,
              advanceOrCapture = false,
              passedPawnUrgency = PassedPawnUrgency.Background,
              passerBlockade = false,
              blockadeSquare = None,
              blockadeRole = None,
              pusherSupport = false,
              minorityAttack = false,
              counterBreak = true,
              tensionPolicy = TensionPolicy.Maintain,
              tensionSquares = List("d4", "e5"),
              tensionEdges = List("d4-e5"),
              counterBreakFiles = List("c"),
              primaryDriver = PawnPlayDriver.BreakReady
            )
          )
        )
      )

    val axisLabels =
      StrategicMechanismEvidence
        .sourceMechanisms(record)
        .flatMap { case (_, signal) => signal.axis.map(_.label) }

    assert(axisLabels.contains("break-file-e-maintain-d4-e5"), axisLabels)
    val signatures =
      EvidenceObjectBinding.objectSignatures(EvidenceObjectBinding.fromEvidenceRefs(TypedEvidenceGraph(List(record)), List(record.ref)))
    assert(signatures.exists(_.contains("target=File:e")), signatures)
    assert(signatures.exists(signature => signature.contains("target=Square:d4") && signature.contains("target=Square:e5")), signatures)
    assert(signatures.exists(_.contains("target=File:c")), signatures)

  test("structural pawn tension delta uses concrete break and tension subjects as axis label"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/4P3/8/8/8 b - - 0 1", 2, Some(chess.Color.Black), Some("after"))
    val transition = StructuralTransitionBinding(
      moveUci = "e2e4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = after,
      line = Some(candidateLine),
      perspective = chess.Color.White
    )
    val record =
      EvidenceRecord(
        evidenceRef("structural-delta:pawn-tension", root),
        StructuralDeltaEvidence(
          transition = transition,
          signals = Nil,
          consequences = List(
            TransitionConsequence(
              TransitionConsequenceKind.PawnTensionGain,
              StructuralSignalPolarity.Gain,
              strength = 3,
              subjects = List("break-file:e", "created-tension:e4-d5")
            )
          )
        )
      )
    val axisLabels =
      StrategicMechanismEvidence
        .sourceMechanisms(record)
        .flatMap { case (_, signal) => signal.axis.map(_.label) }

    assert(axisLabels.contains("break-file-e-created-tension-e4-d5"), axisLabels)
    assert(!axisLabels.contains("pawn-structure-delta"), axisLabels)

  test("structural pawn tension resolution emits release axis instead of positive break support"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/8/8/8 b - - 0 1", 2, Some(chess.Color.Black), Some("after"))
    val transition = StructuralTransitionBinding(
      moveUci = "d4e5",
      role = TransitionEdgeRole.Played,
      from = root,
      to = after,
      line = Some(candidateLine),
      perspective = chess.Color.White
    )
    val record =
      EvidenceRecord(
        evidenceRef("structural-delta:pawn-tension-resolution", root),
        StructuralDeltaEvidence(
          transition = transition,
          signals = Nil,
          consequences = List(
            TransitionConsequence(
              TransitionConsequenceKind.PawnTensionResolution,
              StructuralSignalPolarity.Loss,
              strength = 3,
              subjects = List("resolved-tension:d4-e5")
            )
          )
        )
      )
    val axes =
      StrategicMechanismEvidence
        .sourceMechanisms(record)
        .flatMap { case (_, signal) => signal.axis }

    assert(axes.exists(axis => axis.kind == StrategicAxisKind.PawnBreak && axis.polarity == StrategicAxisPolarity.Release), axes)
    assert(axes.exists(_.label == "resolved-tension-d4-e5"), axes)
    assert(!axes.exists(axis => axis.polarity == StrategicAxisPolarity.Support && axis.label == "resolved-tension-d4-e5"), axes)

  test("non-tension pawn structure deltas do not emit pawn break axes"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/8/8/8 b - - 0 1", 2, Some(chess.Color.Black), Some("after"))
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = after,
      line = Some(candidateLine),
      perspective = chess.Color.White
    )
    val record =
      EvidenceRecord(
        evidenceRef("structural-delta:open-file-only", root),
        StructuralDeltaEvidence(
          transition = transition,
          signals = Nil,
          consequences = List(
            TransitionConsequence(
              TransitionConsequenceKind.OpenFileGain,
              StructuralSignalPolarity.Gain,
              strength = 2,
              subjects = List("file:d")
            )
          )
        )
      )
    val axes =
      StrategicMechanismEvidence
        .sourceMechanisms(record)
        .flatMap { case (_, signal) => signal.axis }

    assert(!axes.exists(_.kind == StrategicAxisKind.PawnBreak), axes)

  test("pawn play assessor recognizes central advance break candidates"):
    val fen = "rnbq1rk1/pp3ppp/4pn2/8/1bBP4/2N2N2/PP3PPP/R1BQ1RK1 b - - 0 9"
    val features = PositionAnalyzer.extractFeatures(fen, 17).get
    val assessment =
      SinglePositionAssessor.classify(
        features = features,
        multiPv = Nil,
        currentWhitePovEvalCp = 25,
        sideToMove = chess.Color.Black
      )
    val pawnPlay =
      PawnPlayAssessor.analyze(
        features = features,
        motifs = Nil,
        positionAssessment = assessment,
        sideToMove = chess.Color.Black
      ).get

    assert(pawnPlay.pawnBreakReady)
    assertEquals(pawnPlay.breakFile, Some("e"))
    assertEquals(pawnPlay.primaryDriver, PawnPlayDriver.BreakReady)

  test("pawn play assessor recognizes contested central advance break candidates"):
    val fen = "rn1qk2r/1b3ppp/p3pn2/1pp5/8/3BPN2/PP2QPPP/RNB2RK1 w kq - 3 10"
    val features = PositionAnalyzer.extractFeatures(fen, 19).get
    val assessment =
      SinglePositionAssessor.classify(
        features = features,
        multiPv = Nil,
        currentWhitePovEvalCp = 33,
        sideToMove = chess.Color.White
      )
    val pawnPlay =
      PawnPlayAssessor.analyze(
        features = features,
        motifs = Nil,
        positionAssessment = assessment,
        sideToMove = chess.Color.White
      ).get

    assert(pawnPlay.pawnBreakReady)
    assertEquals(pawnPlay.breakFile, Some("e"))
    assertEquals(pawnPlay.primaryDriver, PawnPlayDriver.BreakReady)

  test("pawn play assessor keeps opponent counter-break as defensive driver when no stronger pawn action exists"):
    val fen = "4k3/8/8/8/8/8/4K3/8 w - - 0 1"
    val features = PositionAnalyzer.extractFeatures(fen, 1).get
    val assessment =
      SinglePositionAssessor.classify(
        features = features,
        multiPv = Nil,
        currentWhitePovEvalCp = 0,
        sideToMove = chess.Color.White
      )
    val pawnPlay =
      PawnPlayAssessor.analyze(
        features = features,
        motifs = List(Motif.PawnBreak(File.C, File.D, Color.Black, plyIndex = 0, move = Some("c7d6"))),
        positionAssessment = assessment,
        sideToMove = chess.Color.White
      ).get

    assertEquals(pawnPlay.pawnBreakReady, false)
    assertEquals(pawnPlay.counterBreak, true)
    assertEquals(pawnPlay.counterBreakFiles, List("c"))
    assertEquals(pawnPlay.primaryDriver, PawnPlayDriver.Defensive)

  test("a newly attacked defended pawn does not become a weak pawn or adjacent weak squares"):
    val beforeFen = "4k3/8/3p4/4p3/8/8/3P4/4K3 w - - 0 1"
    val afterFen = "4k3/8/3p4/4p3/3P4/8/8/4K3 b - - 0 1"
    val before = Fen.read(Standard, Fen.Full(beforeFen)).getOrElse(fail("expected before position"))
    val after = Fen.read(Standard, Fen.Full(afterFen)).getOrElse(fail("expected after position"))
    val delta = StructuralDeltaAnalyzer
      .delta(
        beforeFen = beforeFen,
        beforeBoard = before.board,
        afterFen = afterFen,
        afterBoard = after.board,
        side = Color.White,
        files = ('a' to 'h').toList,
        targets = List("e5"),
        createdTensionFrom = Some("d4"),
        moveUci = Some("d2d4")
      )
      .getOrElse(fail("expected structural delta"))

    assertEquals(delta.newWeakPawns, Nil)
    assertEquals(delta.newWeakSquares, Nil)

  test("capturing the last pawn defender creates only the attacked structural hole"):
    val beforeFen = "7k/8/3p4/4P3/8/1N6/8/7K w - - 0 1"
    val afterFen = "7k/8/3P4/8/8/1N6/8/7K b - - 0 1"
    val before = Fen.read(Standard, Fen.Full(beforeFen)).getOrElse(fail("expected before position"))
    val after = Fen.read(Standard, Fen.Full(afterFen)).getOrElse(fail("expected after position"))
    val delta = StructuralDeltaAnalyzer
      .delta(
        beforeFen = beforeFen,
        beforeBoard = before.board,
        afterFen = afterFen,
        afterBoard = after.board,
        side = Color.White,
        files = ('a' to 'h').toList,
        targets = Nil,
        createdTensionFrom = Some("d6"),
        moveUci = Some("e5d6")
      )
      .getOrElse(fail("expected structural delta"))

    assertEquals(delta.newWeakSquares, List("c5"))
    assert(
      StructuralDeltaContracts
        .consequences(delta)
        .exists(consequence =>
          consequence.kind == TransitionConsequenceKind.WeakSquareTargetCreated &&
            consequence.subjects == List("weak-square:c5")
        )
    )

  test("board fact anchors preserve concrete target squares for outposts and weak squares"):
    val position = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val e5 = chess.Square.fromKey("e5").get
    val record =
      PositionFactNormalizer.fromBoardFacts(
        id = "board-concrete-square",
        facts = List(
          Fact.Outpost(e5, chess.Knight, FactScope.Now),
          Fact.WeakSquare(e5, chess.Color.Black, Fact.WeakSquareReason.StructuralHole, FactScope.Now)
        ),
        features = None,
        position = position,
        scope = EvidenceScope.CurrentPosition
      )
    val anchors =
      record.payload match
        case payload: BoardFactEvidence => payload.boardAnchors
        case _                          => Nil

    val outpostTarget = anchors.find(_.kind == BoardAnchorKind.Outpost).flatMap(_.detail.flatMap(_.targetSquare.map(_.key)))
    val weakSquareTarget =
      anchors.find(_.kind == BoardAnchorKind.WeakSquare).flatMap(_.detail.flatMap(_.targetSquare.map(_.key)))

    assertEquals(outpostTarget, Some("e5"))
    assertEquals(weakSquareTarget, Some("e5"))

  test("rook endgame pattern anchors preserve technique squares for required-square projection"):
    val position = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val d7 = chess.Square.fromKey("d7").get
    val d8 = chess.Square.fromKey("d8").get
    val record =
      PositionFactNormalizer.fromBoardFacts(
        id = "board-rook-technique",
        facts = List(
          Fact.RookEndgamePattern(
            RookEndgamePattern.RookBehindPassedPawn,
            FactScope.Now,
            primaryPattern = Some("Lucena"),
            anchorSquares = List(d7, d8)
          )
        ),
        features = None,
        position = position,
        scope = EvidenceScope.CurrentPosition
      )
    val anchors =
      record.payload match
        case payload: BoardFactEvidence => payload.boardAnchors
        case _                          => Nil
    val rookAnchor = anchors.find(_.signal == BoardAnchorSignal.EndgameRookPattern).toList
    val focusSquares = rookAnchor.flatMap(_.focusSquares.map(_.key)).distinct.sorted
    val semanticKeys = rookAnchor.map(_.semanticGroupingAnchor.stableKey)

    assertEquals(focusSquares, List("d7", "d8"))
    assert(semanticKeys.exists(_.contains("pattern:Lucena")), semanticKeys)

    val projectedDetail =
      PositionPlanTechniqueProjection
        .frames(TypedEvidenceGraph(List(record)), Nil, Nil, None)
        .flatMap(_.semanticDetails)
        .find(_.boardAnchorSignals.contains(BoardAnchorSignal.EndgameRookPattern.toString))
        .get
    assertEquals(projectedDetail.endgameTechniquePattern, Some("Lucena"))
    assertEquals(projectedDetail.endgameTechniqueRookPattern, Some("RookBehindPassedPawn"))
    assertEquals(projectedDetail.requiredSquares, List("d7", "d8"))
    assert(projectedDetail.objectBindingSignatures.nonEmpty)

  test("rook endgame pattern anchors preserve typed Philidor geometry"):
    val position = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val geometry =
      RookEndgameGeometry(
        techniqueSide = chess.Color.Black,
        strongSide = chess.Color.White,
        defendingSide = chess.Color.Black,
        passedPawn = chess.Square.fromKey("e5").get,
        promotionSquare = chess.Square.fromKey("e8").get,
        attackingKing = chess.Square.fromKey("d5"),
        defendingKing = chess.Square.fromKey("e7"),
        attackingRook = chess.Square.fromKey("a8"),
        defendingRook = chess.Square.fromKey("h6"),
        barrierRank = Some(chess.Rank.Sixth)
      )
    val record =
      PositionFactNormalizer.fromBoardFacts(
        id = "board-philidor-technique",
        facts = List(
          Fact.RookEndgamePattern(
            RookEndgamePattern.KingCutOff,
            FactScope.Now,
            primaryPattern = Some("PhilidorDefense"),
            anchorSquares = geometry.anchorSquares,
            geometry = Some(geometry)
          )
        ),
        features = None,
        position = position,
        scope = EvidenceScope.CurrentPosition
      )
    val rookAnchor =
      record.payload match
        case payload: BoardFactEvidence => payload.boardAnchors.find(_.signal == BoardAnchorSignal.EndgameRookPattern)
        case _                          => None
    val anchor = rookAnchor.get
    val detail = anchor.detail.get
    val semanticKey = anchor.semanticGroupingAnchor.stableKey

    assertEquals(anchor.side, chess.Color.Black)
    assertEquals(detail.subjectColor, Some(chess.Color.Black))
    assertEquals(detail.subjectSquare.map(_.key), Some("e7"))
    assertEquals(detail.targetSquare.map(_.key), Some("e8"))
    assertEquals(detail.attackerColor, Some(chess.Color.White))
    assertEquals(detail.attackerSquare.map(_.key), Some("a8"))
    assertEquals(detail.defenderSquares.map(_.key).sorted, List("e7", "h6"))
    assertEquals(detail.file.map(_.key), Some("e"))
    assert(detail.tags.contains("pattern:PhilidorDefense"), detail.tags)
    assert(detail.tags.contains("barrier-rank:6"), detail.tags)
    assert(semanticKey.contains("technique-side:black"), semanticKey)
    assert(semanticKey.contains("passed-pawn:e5"), semanticKey)

    val projectedDetail =
      PositionPlanTechniqueProjection
        .frames(TypedEvidenceGraph(List(record)), Nil, Nil, None)
        .flatMap(_.semanticDetails)
        .find(_.boardAnchorSignals.contains(BoardAnchorSignal.EndgameRookPattern.toString))
        .get
    assertEquals(projectedDetail.endgameTechniquePattern, Some("PhilidorDefense"))
    assertEquals(projectedDetail.endgameTechniqueRookPattern, Some("KingCutOff"))
    assertEquals(projectedDetail.endgameTechniqueSide, Some("black"))
    assert(projectedDetail.requiredSquares.contains("e8"), projectedDetail.requiredSquares)

  test("rook endgame oracle attaches technique patterns and geometry from study positions"):
    def board(fen: String) = Fen.read(Standard, Fen.Full(fen)).get.board
    def feature(fen: String, color: chess.Color) = EndgamePatternOracle.analyze(board(fen), color).get
    def anchorKeys(fen: String, color: chess.Color) =
      feature(fen, color).rookEndgameAnchorSquares.map(_.key).distinct.sorted

    val lucenaFen = "6K1/3k1P2/8/8/R7/8/8/7r w - - 0 1"
    val lucena = feature(lucenaFen, chess.Color.White)
    assertEquals(lucena.primaryPattern, Some("Lucena"))
    assertEquals(lucena.rookEndgamePattern, RookEndgamePattern.RookBehindPassedPawn)
    assertEquals(anchorKeys(lucenaFen, chess.Color.White), List("a4", "d7", "f7", "f8", "g8", "h1"))

    val lucenaFacts =
      FactExtractor.extractEndgameFacts(board(lucenaFen), chess.Color.White, Some(lucena))
    val lucenaFactAnchors =
      lucenaFacts.collectFirst { case Fact.RookEndgamePattern(_, _, _, anchors, _) => anchors.map(_.key).distinct.sorted }
    assertEquals(lucenaFactAnchors, Some(List("a4", "d7", "f7", "f8", "g8", "h1")))
    val lucenaGeometry = lucena.rookEndgameGeometry.get
    assertEquals(lucenaGeometry.techniqueSide, chess.Color.White)
    assertEquals(lucenaGeometry.strongSide, chess.Color.White)
    assertEquals(lucenaGeometry.defendingSide, chess.Color.Black)
    assertEquals(lucenaGeometry.passedPawn.key, "f7")
    assertEquals(lucenaGeometry.promotionSquare.key, "f8")
    assertEquals(lucenaGeometry.techniqueKing.map(_.key), Some("g8"))

    val sameFileBridgeLucenaFen = "8/2KP4/4k3/8/3R4/8/8/2r5 w - - 0 1"
    val sameFileBridgeLucena = feature(sameFileBridgeLucenaFen, chess.Color.White)
    assertEquals(sameFileBridgeLucena.primaryPattern, Some("Lucena"))
    assertEquals(sameFileBridgeLucena.rookEndgamePattern, RookEndgamePattern.RookBehindPassedPawn)
    assertEquals(anchorKeys(sameFileBridgeLucenaFen, chess.Color.White), List("c1", "c7", "d4", "d7", "d8", "e6"))

    val philidorFen = "R7/4k3/7r/3KP3/8/8/8/8 w - - 0 1"
    val philidor = feature(philidorFen, chess.Color.Black)
    assertEquals(philidor.primaryPattern, Some("PhilidorDefense"))
    assertEquals(philidor.rookEndgamePattern, RookEndgamePattern.KingCutOff)
    assertEquals(anchorKeys(philidorFen, chess.Color.Black), List("a8", "d5", "e5", "e7", "e8", "h6"))
    val philidorGeometry = philidor.rookEndgameGeometry.get
    assertEquals(philidorGeometry.techniqueSide, chess.Color.Black)
    assertEquals(philidorGeometry.strongSide, chess.Color.White)
    assertEquals(philidorGeometry.defendingSide, chess.Color.Black)
    assertEquals(philidorGeometry.passedPawn.key, "e5")
    assertEquals(philidorGeometry.promotionSquare.key, "e8")
    assertEquals(philidorGeometry.techniqueKing.map(_.key), Some("e7"))
    assertEquals(philidorGeometry.barrierRank.map(_.value + 1), Some(6))

    val postAdvancePhilidorFen = "8/6k1/8/r3P3/4K3/8/8/7R b - - 0 1"
    val postAdvancePhilidor = feature(postAdvancePhilidorFen, chess.Color.Black)
    assertEquals(postAdvancePhilidor.primaryPattern, Some("PhilidorDefense"))
    assertEquals(postAdvancePhilidor.rookEndgamePattern, RookEndgamePattern.KingCutOff)
    assertEquals(anchorKeys(postAdvancePhilidorFen, chess.Color.Black), List("a5", "e4", "e5", "e8", "g7", "h1"))
    assertEquals(postAdvancePhilidor.rookEndgameGeometry.flatMap(_.barrierRank), None)

    val vancuraFen = "6k1/7R/P1r3K1/8/8/8/8/8 w - - 0 1"
    val vancura = feature(vancuraFen, chess.Color.Black)
    assertEquals(vancura.primaryPattern, Some("VancuraDefense"))
    assertEquals(vancura.rookEndgamePattern, RookEndgamePattern.RookBehindPassedPawn)
    assertEquals(anchorKeys(vancuraFen, chess.Color.Black), List("a6", "a8", "c6", "g6", "g8", "h7"))

    val shortSideFen = "k7/8/8/1P5r/2K5/8/8/7R b - - 0 1"
    val shortSide = feature(shortSideFen, chess.Color.Black)
    assertEquals(shortSide.primaryPattern, Some("ShortSideDefense"))
    assertEquals(shortSide.rookEndgamePattern, RookEndgamePattern.KingCutOff)
    assertEquals(anchorKeys(shortSideFen, chess.Color.Black), List("a8", "b5", "b8", "c4", "h1", "h5"))

    val tarraschFen = "8/7k/8/4P3/4K3/8/8/R3r3 w - - 0 1"
    val tarrasch = feature(tarraschFen, chess.Color.Black)
    assertEquals(tarrasch.primaryPattern, Some("TarraschDefenseActive"))
    assertEquals(tarrasch.rookEndgamePattern, RookEndgamePattern.TarraschDefenseActive)
    assertEquals(anchorKeys(tarraschFen, chess.Color.Black), List("a1", "e1", "e4", "e5", "e8", "h7"))

    val passiveFen = "7k/8/8/8/4p3/4r3/8/K6R b - - 0 1"
    val passive = feature(passiveFen, chess.Color.Black)
    assertEquals(passive.primaryPattern, Some("PassiveRookDefense"))
    assertEquals(passive.rookEndgamePattern, RookEndgamePattern.PassiveRookDefense)
    assertEquals(anchorKeys(passiveFen, chess.Color.Black), List("a1", "e1", "e3", "e4", "h1", "h8"))

  test("rook endgame oracle rejects nearby non-technique contrast positions"):
    def feature(fen: String, color: chess.Color) =
      val board = Fen.read(Standard, Fen.Full(fen)).get.board
      EndgamePatternOracle.analyze(board, color).get

    val sixthRankBridgeLike = feature("6K1/3k4/5P2/8/R7/8/8/7r w - - 0 1", chess.Color.White)
    assertNotEquals(sixthRankBridgeLike.primaryPattern, Some("Lucena"))
    assertNotEquals(sixthRankBridgeLike.rookEndgamePattern, RookEndgamePattern.RookBehindPassedPawn)

    val adjacentFileBridgeLike = feature("8/2KP4/4k3/8/4R3/8/8/2r5 w - - 0 1", chess.Color.White)
    assertNotEquals(adjacentFileBridgeLike.primaryPattern, Some("Lucena"))
    assertNotEquals(adjacentFileBridgeLike.rookEndgamePattern, RookEndgamePattern.RookBehindPassedPawn)

    val earlyThirdRankDefenseLike = feature("R7/4k3/7r/8/3KP3/8/8/8 w - - 0 1", chess.Color.Black)
    assertNotEquals(earlyThirdRankDefenseLike.primaryPattern, Some("PhilidorDefense"))

  test("move meaning claims do not surface file-only pawn break background as current move function"):
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:break-file-d"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("break-file-d"),
      breakFile = Some("d"),
      tensionPolicy = Some("Ignore"),
      sourceEvidenceIds = List(s"played-transition:${candidateLine.rootMove}"),
      objectBindingSignatures = List("target=File:d|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )

    assert(!view.moveMeaningClaims.exists(claim => claim.meaningKind == "PawnBreakTiming"), view.moveMeaningClaims)

  test("move meaning claims do not surface pawn-shaped non-pawn moves as the current pawn break"):
    val queenPosition =
      PositionNodeRef("8/8/8/8/8/8/4Q3/4K3 w - - 0 1", 1, Some(chess.Color.White), Some("queen-root"))
    val ownedCause = causeFrame(
      causeId = "cause-queen-shaped-break",
      axisKeys = List("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
      objectSignatures = List("actor=Move:e2e3|target=Square:e3|target=Square:d4|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    ).copy(
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("break-file-e-created-tension-e3-d4"),
      breakFile = Some("e"),
      tensionSquares = List("e3", "d4"),
      tensionEdges = List("e3-d4"),
      structuralPurposeSubjects = List("created-tension:e3-d4"),
      candidateEvidenceIds = List("structural-delta:played:e2e3:tension"),
      sourceEvidenceIds = List("structural-delta:played:e2e3:tension"),
      causeEvidenceIds = List("cause-queen-shaped-break"),
      objectBindingSignatures =
        List("actor=Move:e2e3|target=Square:e3|target=Square:d4|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val base = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(ownedCause),
      details = List(detail)
    )
    val view =
      meaningClaimViewWithClaims(
        base.copy(
          positionPlanTechniqueFrames =
            List(meaningClaimPlanTechniqueFrame(List(detail)).copy(position = queenPosition))
        )
      )

    assert(!view.moveMeaningClaims.exists(claim => claim.meaningKind == "PawnBreakTiming"), view.moveMeaningClaims)

  test("move meaning claims suppress view-only route copies when an owned same-route carrier exists"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:developmentchoice|proof=DirectProof"
    val routeCause = causeFrame(
      causeId = "cause-owned-route",
      axisKeys = List("Activity:Gain:activity-gain"),
      objectSignatures = List(routeSignature),
      causeKind = RelativeCauseKind.ActivityGain,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    ).copy(hasOwnedAdmissibleLongTermProof = true)
    val ownedDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      candidateEvidenceIds = List("structural-delta:played:e2e3"),
      sourceEvidenceIds = List("structural-delta:played:e2e3"),
      causeEvidenceIds = List("cause-owned-route"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralRouteRole = Some("Played"),
      structuralPurposeSubjects = List("knight:e2-e3:maneuver"),
      structuralPurposeConsequences = List("DevelopmentPieceActivated"),
      structuralPurposeCategories = List("PieceActivity", "Development"),
      structuralMotifTags = List("piece", "reroute", "route")
    )
    val viewOnlyDetail = ownedDetail.copy(
      axisKey = Some("Activity:Loss:activity-loss"),
      axisPolarity = Some(StrategicAxisPolarity.Loss),
      label = Some("activity-loss"),
      causeEvidenceIds = Nil,
      proofRoles = Nil,
      structuralPurposeConsequences = List("MobilityLoss"),
      structuralPurposePolarities = List("Loss")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(routeCause),
      details = List(ownedDetail, viewOnlyDetail)
    )
    val routeClaims = view.moveMeaningClaims.filter(_.meaningKind == "PieceRoute")

    assertEquals(routeClaims.size, 1)
    assert(routeClaims.head.publicSurfaceAdmitted, routeClaims)

  test("move meaning claims keep only the current root route from a mixed comparison detail"):
    val currentRouteSignature =
      "actor=Move:e2e3|actor=Piece:queen|actor=Square:d1|target=Square:d1|mechanism=Mechanism:lineunlockgain|consequence=Consequence:lineunlockgain:gain|proof=DirectProof"
    val routeCause = causeFrame(
      causeId = "cause-mixed-comparison-route",
      axisKeys = List("Activity:Gain:activity-gain"),
      objectSignatures = List(currentRouteSignature),
      causeKind = RelativeCauseKind.ActivityGain,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot
    ).copy(hasOwnedAdmissibleLongTermProof = true)
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.SharedSustained),
      candidateEvidenceIds = List("structural-delta:played:e2e3"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "structural-delta:reference:e2e4"),
      causeEvidenceIds = List("cause-mixed-comparison-route"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(currentRouteSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = None,
      structuralPurposeSubjects = List(
        "battery:file:c1-c2:rook-queen",
        "bishop:d3:line-unlock:by:e2e4:mobility+1",
        "queen:d1:line-unlock:by:e2e3:mobility+2"
      ),
      structuralPurposeConsequences = List("BatteryPressureGain", "LineUnlockGain"),
      structuralPurposeCategories = List("PieceActivity"),
      structuralPurposePolarities = List("Gain")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(routeCause),
      details = List(detail)
    )
    val routeClaims = view.moveMeaningClaims.filter(claim =>
      claim.meaningKind == "PieceRoute" && claim.publicSurfaceAdmitted
    )

    assert(routeClaims.nonEmpty, view.moveMeaningClaims)
    assert(routeClaims.forall(_.routeIdentityParts.contains("subject:queen:d1:line-unlock:by:e2e3:mobility+2")), routeClaims)
    assert(routeClaims.forall(!_.routeIdentityParts.exists(part => part.contains("e2e4") || part.contains("battery:file:c1-c2"))), routeClaims)

  test("public meaning keeps a view-only line unlock as a result of the current piece route"):
    val movedPieceSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:maneuver|consequence=Consequence:mobilitygain:gain|proof=DirectProof"
    val lineUnlockSignature =
      "actor=Move:e2e3|actor=Piece:queen|actor=Square:d1|target=Square:d1|mechanism=Mechanism:lineunlockgain|consequence=Consequence:lineunlockgain:gain|proof=DirectProof"
    val routeCause = causeFrame(
      causeId = "cause-current-piece-route",
      axisKeys = List("Activity:Gain:activity-gain"),
      objectSignatures = List(movedPieceSignature),
      causeKind = RelativeCauseKind.ActivityGain,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot
    ).copy(hasOwnedAdmissibleLongTermProof = true)
    val movedPieceDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      candidateEvidenceIds = List("structural-delta:played:e2e3:route"),
      sourceEvidenceIds = List("structural-delta:played:e2e3:route"),
      causeEvidenceIds = List("cause-current-piece-route"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(movedPieceSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("knight:e2-e3:maneuver"),
      structuralPurposeConsequences = List("MobilityGain"),
      structuralPurposeCategories = List("PieceActivity"),
      structuralPurposePolarities = List("Gain")
    )
    val lineUnlockDetail = movedPieceDetail.copy(
      candidateEvidenceIds = List("structural-delta:played:e2e3:line-unlock"),
      sourceEvidenceIds = List("structural-delta:played:e2e3:line-unlock"),
      causeEvidenceIds = Nil,
      proofRoles = Nil,
      objectBindingSignatures = List(lineUnlockSignature),
      structuralPurposeSubjects = List("queen:d1:line-unlock:by:e2e3:mobility+2"),
      structuralPurposeConsequences = List("LineUnlockGain")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(routeCause),
      details = List(movedPieceDetail, lineUnlockDetail)
    )
    val publicRoutes = view.moveMeaningClaims.filter(claim =>
      claim.meaningKind == "PieceRoute" && claim.publicSurfaceAdmitted
    )

    assertEquals(publicRoutes.size, 1, view.moveMeaningClaims)
    assert(publicRoutes.head.routeIdentityParts.contains("from:e2"), publicRoutes)
    assert(publicRoutes.head.routeIdentityParts.contains("to:e3"), publicRoutes)

  test("a line beneficiary cannot relabel a king move as a rook lift"):
    val kingMove = lineRef("played-king-move", "g1g2", 2, LineNodeRole.Played)
    val lineUnlockSignature =
      "actor=Move:g1g2|actor=Piece:king|actor=Square:g1|target=Piece:rook|target=Square:d1|mechanism=Mechanism:lineunlockgain|consequence=Consequence:lineunlockgain:gain|proof=DirectProof"
    val cause = causeFrame(
      causeId = "cause-king-clears-rook",
      axisKeys = List("Activity:Gain:line-unlock"),
      objectSignatures = List(lineUnlockSignature),
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot
    ).copy(
      candidateLine = kingMove,
      eventLine = kingMove,
      eventRootMove = kingMove.rootMove,
      hasOwnedAdmissibleLongTermProof = true
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:line-unlock"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("line-unlock"),
      candidateEvidenceIds = List("structural-delta:played:g1g2"),
      sourceEvidenceIds = List("structural-delta:played:g1g2"),
      causeEvidenceIds = List("cause-king-clears-rook"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(lineUnlockSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(kingMove.rootMove),
      structuralPurposeSubjects = List("rook:d1:line-unlock:by:g1g2:mobility+2"),
      structuralPurposeConsequences = List("LineUnlockGain")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail),
      candidate = kingMove
    )

    val routes = MoveMeaningSurface.from(view).filter(_.ideaType == "piece_route")
    assert(routes.exists(_.evidence.boardCarriers.exists(carrier =>
      carrier.role == "actor" && carrier.kind == "Move" && carrier.value == kingMove.rootMove
    )), routes)
    assert(!routes.exists(_.evidence.boardCarriers.exists(carrier =>
      carrier.role == "actor" && carrier.kind == "Piece" && carrier.value == "rook"
    )), routes)

  test("move meaning claims compress an owned route concession with its duplicate activity loss"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Piece:knight|target=Square:e3|mechanism=Mechanism:rerouting|consequence=Consequence:mobilityloss:loss|proof=DirectProof"
    val activitySignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Piece:knight|target=Square:e3|mechanism=Mechanism:activity|consequence=Consequence:mobilityloss:loss|proof=DirectProof"
    val cause = causeFrame(
      causeId = "cause-owned-route-loss",
      axisKeys = List("Activity:Loss:activity-loss"),
      objectSignatures = List(routeSignature, activitySignature),
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot,
      causeKind = RelativeCauseKind.ActivityLoss
    ).copy(hasOwnedAdmissibleLongTermProof = true)
    val routeDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Loss:activity-loss"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Loss),
      label = Some("activity-loss"),
      candidateEvidenceIds = List("route-loss"),
      sourceEvidenceIds = List("route-loss"),
      causeEvidenceIds = List("cause-owned-route-loss"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("knight:e2-e3:maneuver"),
      structuralPurposeConsequences = List("MobilityLoss"),
      structuralPurposePolarities = List("Loss"),
      structuralMotifTags = List("piece", "reroute", "route")
    )
    val activityDetail = routeDetail.copy(
      candidateEvidenceIds = List("activity-loss"),
      sourceEvidenceIds = List("activity-loss"),
      objectBindingSignatures = List(activitySignature),
      structuralPurposeSubjects = Nil,
      structuralMotifTags = Nil
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.Inaccuracy,
      auditCauses = List(cause),
      details = List(routeDetail, activityDetail)
    )
    val admitted = view.moveMeaningClaims.filter(_.publicSurfaceAdmitted)

    assertEquals(admitted.count(_.meaningKind == "PieceRoute"), 1, view.moveMeaningClaims)
    assert(!admitted.exists(_.meaningKind == "PieceActivity"), admitted)
    val surfaces = MoveMeaningSurface.publicSurfaces(view)
    assert(surfaces.exists(surface =>
      surface.ideaType == "piece_route" && surface.problem.contains("piece_activity_lost")
    ), surfaces)

  test("move meaning public surface does not use rendered target as claim proof"):
    val proofOnlyClaim = MoveMeaningClaim(
      meaningKind = "PieceActivity",
      role = "ImprovesPieceActivity",
      laneKey = "kind=PieceActivity|axis=Activity:Gain:activity-gain|object=none",
      conflictKey = None,
      supportLevel = "owned_cause_linked",
      visibility = "reason_grade",
      surfaceLane = "current_move_owned",
      lineRole = "candidate",
      moveUci = "e2e3",
      frameId = "frame-proof-only",
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      causeKinds = List(RelativeCauseKind.ActivityGain),
      causeSourceSides = List(RelativeCauseSourceSide.Candidate),
      causeEvidenceIds = List("cause-proof-only"),
      sourceEvidenceIds = Nil,
      objectBindingSignatures = Nil,
      reasonTokens = Nil,
      targetSquares = List("e4")
    )
    val surface = MoveMeaningSurface.from(
      meaningClaimView(
        verdict = MoveChoiceVerdict.MatchesReference,
        auditCauses = Nil,
        details = Nil
      ).copy(moveMeaningClaims = List(proofOnlyClaim))
    )

    assert(!surface.exists(_.ideaType == "piece_activity"), surface)
    assertEquals(MoveMeaningSurface.evidenceForClaim(proofOnlyClaim).targetBound, false)

  test("move meaning public surface does not use target-only objects as terminal or technique carrier"):
    val terminalTargetOnlyClaim = MoveMeaningClaim(
      meaningKind = "TerminalProof",
      role = "ForcesTerminalResult",
      laneKey = "kind=TerminalProof|axis=none|object=none",
      conflictKey = None,
      supportLevel = "view_surfaced",
      visibility = "functional_explanation",
      surfaceLane = "current_move_function",
      lineRole = "candidate",
      moveUci = "e2e3",
      frameId = "frame-terminal-target-only",
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      axisKey = None,
      axisKind = None,
      axisPolarity = None,
      label = Some("mate"),
      causeKinds = Nil,
      causeSourceSides = Nil,
      causeEvidenceIds = Nil,
      sourceEvidenceIds = List("line-terminal"),
      objectBindingSignatures = List("target=Square:e8"),
      reasonTokens = List("terminalConsequenceKind:Mate"),
      targetSquares = List("e8")
    )
    val techniqueTargetOnlyClaim = terminalTargetOnlyClaim.copy(
      meaningKind = "RookEndgameTechnique",
      role = "HoldsTechnique",
      laneKey = "kind=RookEndgameTechnique|axis=none|object=none",
      unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
      label = Some("rook-endgame-technique"),
      sourceEvidenceIds = List("line-technique"),
      reasonTokens = List("horizonStatus:Active", "requiredSquare:e8")
    )
    val surface = MoveMeaningSurface.from(
      meaningClaimView(
        verdict = MoveChoiceVerdict.MatchesReference,
        auditCauses = Nil,
        details = Nil
      ).copy(moveMeaningClaims = List(terminalTargetOnlyClaim, techniqueTargetOnlyClaim))
    )

    assert(!surface.exists(item => Set(
      PositionPlanTechniqueUnit.StructuralTransformation,
      PositionPlanTechniqueUnit.EndgameTechniqueRecipe
    )(item.unit)), surface)
    assertEquals(MoveMeaningSurface.evidenceForClaim(terminalTargetOnlyClaim).hasCarrier, false)
    assertEquals(MoveMeaningSurface.evidenceForClaim(techniqueTargetOnlyClaim).hasCarrier, false)

  test("move meaning claims do not surface unsafe current-move route as positive route"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:outpost|consequence=Consequence:outpost"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("outpost:knight:e3", "knight:e2-e3:mobility+2"),
      structuralPurposeConsequences = List("DevelopmentUnsafePlacement"),
      structuralPurposeCategories = List("PieceActivity"),
      structuralMotifTags = List("piece", "route", "outpost")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.Mistake,
      auditCauses = Nil,
      details = List(detail)
    )

    assert(!view.moveMeaningClaims.exists(claim =>
      claim.moveUci == candidateLine.rootMove && claim.meaningKind == "PieceRoute"
    ), view.moveMeaningClaims)

  test("a publicly admitted bad-move function remains visible without inventing a plan event"):
    val planPressureSignature =
      "actor=Move:e2e3|actor=Square:e2|target=PlanSubject:queensideattack|mechanism=Mechanism:plan-pressure|consequence=Consequence:queensideattack|witness=Move:e2e3"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      axisKey = Some("PlanCoherence:Support:QueensideAttack"),
      axisKind = Some(StrategicAxisKind.PlanCoherence),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("QueensideAttack"),
      activePlanIds = List(PlanId.QueensideAttack),
      displayPlanId = Some(PlanId.QueensideAttack),
      planMoveRole = Some(PlanMoveRole.Preparation),
      structuralRouteMove = Some(candidateLine.rootMove),
      sourceEvidenceIds = List("plan-pressure:queenside-attack"),
      objectBindingSignatures = List(planPressureSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.Mistake,
      auditCauses = Nil,
      details = List(
        detail,
        detail.copy(
          axisKey = Some("PlanCoherence:Support:QueensideAttack:alternate-axis"),
          label = Some("QueensideAttack alternate"),
          sourceEvidenceIds = List("plan-pressure:queenside-attack:alternate")
        )
      )
    )
    val claims = view.moveMeaningClaims.filter(_.meaningKind == "PlanContinuity")
    assertEquals(claims.size, 1)
    assertEquals(claims.head.displayPlanId, Some(PlanId.QueensideAttack))
    assert(claims.head.boardCarriers.exists(carrier => carrier.kind == "PlanSubject" && carrier.value == "queensideattack"))
    val admittedClaim = claims.head.copy(
      supportLevel = "view_surfaced",
      visibility = "functional_explanation",
      surfaceLane = "current_move_function",
      publicSurfaceAdmitted = true,
      publicProofLevel = "surface_evidence",
      publicTargetBound = true
    )
    val publicView = view.copy(moveMeaningClaims = List(admittedClaim))
    val surfaces =
      MoveMeaningSurface
        .publicSurfaces(publicView)
        .filter(_.displayPlanId.contains(PlanId.QueensideAttack))
    assertEquals(surfaces.size, 1)
    assertEquals(surfaces.head.displayPlanId, Some(PlanId.QueensideAttack))
    val publicPayload = MoveMeaningSurface.publicPayloadJson(publicView)
    assert((publicPayload \\ "principal_plan_id").isEmpty)
    val publicExplanation = (publicPayload \ "explanations")
      .as[List[play.api.libs.json.JsObject]].head
    assertEquals((publicExplanation \ "plan").toOption, Some(play.api.libs.json.JsNull))
    val existingPurposes = (publicExplanation \ "purposes").as[List[String]]
    assert(!existingPurposes.contains("queensideattack"))
    def explanationFor(surface: MoveMeaningSurface) =
      MoveMeaningSurface
        .publicExplanations(MoveMeaningSurface.verdict(publicView.verdict.get), List(surface))
        .head

    val carrierBoundarySurface = surfaces.head.copy(
      target = MoveMeaningSurfaceTarget(squares = List("e4")),
      evidence = surfaces.head.evidence.copy(
        boardCarriers = surfaces.head.evidence.boardCarriers ++ List(
          MoveMeaningSurfaceBoardCarrier("target", "Square", "e4", semanticRole = Some("pressure_target")),
          MoveMeaningSurfaceBoardCarrier(
            "target",
            "PlanSubject",
            "knight:c3-c7:entry-restricted:route:c3-b5-c7",
            semanticRole = Some("restricted_reply_route_proof")
          )
        )
      )
    )
    val carrierBoundaryExplanation = explanationFor(carrierBoundarySurface)
    assertEquals((carrierBoundaryExplanation \ "purposes").as[List[String]], existingPurposes)
    assert((carrierBoundaryExplanation \ "results").toOption.isEmpty)
    val carrierBoundaryIdeas = (carrierBoundaryExplanation \ "ideas").as[List[play.api.libs.json.JsObject]]
    assert(carrierBoundaryIdeas.forall(idea => (idea \ "results").toOption.isEmpty))
    assert(carrierBoundaryIdeas.flatMap(idea => (idea \ "target" \ "squares").as[List[String]]).contains("e4"))
    assert(!play.api.libs.json.Json.stringify(carrierBoundaryExplanation).contains("restricted_reply_route_proof"))
    val targetDistinctSurfaces = List(
      surfaces.head.copy(target = MoveMeaningSurfaceTarget(squares = List("e4"))),
      surfaces.head.copy(target = MoveMeaningSurfaceTarget(squares = List("h4")))
    )
    val targetDistinctIdeas =
      (MoveMeaningSurface
        .publicExplanations(MoveMeaningSurface.verdict(publicView.verdict.get), targetDistinctSurfaces)
        .head \ "ideas")
        .as[List[play.api.libs.json.JsObject]]
    assertEquals(targetDistinctIdeas.size, 2)
    assertEquals(
      targetDistinctIdeas.flatMap(idea => (idea \ "target" \ "squares").as[List[String]]).sorted,
      List("e4", "h4")
    )

  test("move meaning claims downgrade positive roles when cause polarity conflicts"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:g1|target=Square:f3|mechanism=Mechanism:developmentchoice|proof=DirectProof"
    val cause = causeFrame(
      causeId = "cause-activity-loss",
      axisKeys = List("Activity:Loss:activity-loss"),
      objectSignatures = List(routeSignature),
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot
    ).copy(
      causeKind = RelativeCauseKind.ActivityLoss,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateStronger),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("played-transition"),
      causeEvidenceIds = List("cause-activity-loss"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("piece:knight:g1-f3")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail)
    )

    assert(!view.moveMeaningClaims.exists(claim =>
      claim.moveUci == candidateLine.rootMove &&
        claim.meaningKind == "PieceRoute" &&
        claim.role.startsWith("Improves")
    ), view.moveMeaningClaims)

  test("public target keeps the exact board target instead of a stale flat piece list"):
    val cause = causeFrame(
      causeId = "cause-pawn-target",
      axisKeys = List("Target:Gain:pawn-target"),
      objectSignatures = List("target=Pawn:weak-pawn:d4|mechanism=Mechanism:target|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnWeaknessTarget
    ).copy(
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    )
    val planEvent = ResolvedPlanEvent(
      goalTheme = "piece redeployment",
      goalKind = Some("target pressure"),
      moveRole = Some(PlanMoveRole.Execution),
      transitionType = Some(TransitionType.Continuation),
      rootMove = candidateLine.rootMove,
      actorRole = Some("pawn"),
      actorFrom = Some("e2"),
      actorTo = Some("e3"),
      targets = List("square:e8"),
      developmentChoices = Nil,
      results = Nil,
      responses = Nil,
      testedContinuation = None
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      axisKey = Some("Target:Gain:pawn-target"),
      axisKind = Some(StrategicAxisKind.Target),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("played-transition"),
      causeEvidenceIds = List("cause-pawn-target"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures =
        List("target=Pawn:weak-pawn:d4|mechanism=Mechanism:target|consequence=Consequence:targetpressure|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("weak-pawn:d4"),
      resolvedPlanEvent = Some(planEvent)
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail)
    )

    val surface = MoveMeaningSurface.from(view)
    assertEquals(view.moveMeaningClaims.flatMap(_.targetPieces).distinct, List("pawn"))
    assertEquals(view.moveMeaningClaims.flatMap(_.resolvedPlanEvent.toList.flatMap(_.targets)).distinct, List("square:e8"))
    assertEquals(surface.flatMap(_.target.squares), List("d4"))
    assertEquals(surface.flatMap(_.target.pieces), List("pawn"))
    val stalePieceView = view.copy(moveMeaningClaims = view.moveMeaningClaims.map(
      _.copy(targetPieces = List("king", "knight", "pawn", "rook"))
    ))
    assertEquals(MoveMeaningSurface.from(stalePieceView).flatMap(_.target.pieces), List("pawn"))

  test("a mixed plan keeps its authoritative goal while retaining conditional deterrence as a result"):
    val resourceMove = NumberedChessMove(
      "d7d5",
      "d5",
      ChessTurn(1, "black", "1..."),
      "1...d5"
    )
    val materialReply = NumberedChessMove(
      "e4d5",
      "exd5",
      ChessTurn(2, "white", "2."),
      "2.exd5"
    )
    val replyTest = PlanReplyTest(
      move = resourceMove.uci,
      moveReference = Some(resourceMove),
      outcome = PlanCausalBranchOutcome.Realized,
      realizationMove = Some(materialReply.uci),
      realizationReference = Some(materialReply),
      realizationMatch = Some(PlanCausalRealizationMatch.ExactMove),
      observedThrough = Some(materialReply.turn),
      terminalOutcome = None,
      terminalReference = None,
      line = List(resourceMove, materialReply)
    )
    val conditionalDeterrence = PlanResult(
      stage = "direct",
      kind = TransitionConsequenceKind.OpponentMobilityRestriction,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("pawn:d7-d5:advance-restricted"),
      robustness = Some(PlanCausalRobustness.Conditional),
      conditions = List(replyTest),
      materialCounterplayPreventionProofReady = true
    )
    val independentFunction = PlanResult(
      stage = "direct",
      kind = TransitionConsequenceKind.OpponentMobilityRestriction,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("knight:c6-e2:entry-restricted:route:c6-d4-e2")
    )
    val planEvent = ResolvedPlanEvent(
      goalTheme = "restriction prophylaxis",
      goalKind = Some("prophylaxis restraint"),
      moveRole = Some(PlanMoveRole.Prevention),
      transitionType = Some(TransitionType.Continuation),
      rootMove = candidateLine.rootMove,
      actorRole = Some("pawn"),
      actorFrom = Some("e2"),
      actorTo = Some("e3"),
      targets = List("king:g8", "pawn:d7-d5:advance-restricted"),
      developmentChoices = Nil,
      results = List(conditionalDeterrence, independentFunction),
      responses = List(replyTest),
      testedContinuation = None,
      representativeResult = Some(conditionalDeterrence)
    )
    val signature =
      "actor=Move:e2e3|actor=Piece:pawn|actor=Square:e2|target=PlanSubject:pawn:d7-d5:advance-restricted|mechanism=Mechanism:opponentmobilityrestriction|consequence=Consequence:opponentmobilityrestriction:gain|proof=DirectProof"
    val cause = causeFrame(
      causeId = "cause-mixed-plan-deterrence",
      axisKeys = List("Counterplay:Restrain:opponent-mobility-restriction"),
      objectSignatures = List(signature),
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot,
      causeKind = RelativeCauseKind.OpponentRestriction
    ).copy(hasOwnedAdmissibleLongTermProof = true)
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.SpacePreventionResourceDenial,
      axisKey = Some("Counterplay:Restrain:opponent-mobility-restriction"),
      axisKind = Some(StrategicAxisKind.Counterplay),
      axisPolarity = Some(StrategicAxisPolarity.Restrain),
      label = Some("opponent-mobility-restriction"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("played-transition"),
      causeEvidenceIds = List("cause-mixed-plan-deterrence"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(signature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("pawn:d7-d5:advance-restricted"),
      structuralPurposeConsequences = List("OpponentMobilityRestriction"),
      resolvedPlanEvent = Some(planEvent)
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail)
    )
    val payload = MoveMeaningSurface.publicPayloadJson(view)
    val explanation = (payload \ "explanations")
      .as[List[play.api.libs.json.JsObject]]
      .find(explanation => (explanation \ "move").asOpt[String].contains(candidateLine.rootMove))
      .getOrElse(fail(s"expected played explanation: $payload"))
    val plan = (explanation \ "plan").as[play.api.libs.json.JsObject]
    val ideaNames = (explanation \ "ideas").as[List[play.api.libs.json.JsObject]]
      .flatMap(idea => (idea \ "name").asOpt[String])

    assertEquals((plan \ "goal" \ "theme").asOpt[String], Some("restriction prophylaxis"), plan)
    assertEquals((plan \ "goal" \ "kind").asOpt[String], Some("prophylaxis restraint"), plan)
    assertEquals((plan \ "move_role").asOpt[String], Some("prevents counterplay"), plan)
    assert((plan \ "results").as[List[play.api.libs.json.JsObject]].exists(result =>
      (result \ "when").asOpt[String].contains("if the opponent plays d7-d5") &&
        (result \ "change").asOpt[String].contains("exd5 produces a material gain in the tested line")
    ), plan)
    assert(!ideaNames.exists(_.startsWith("tests d7-d5 with")), ideaNames)

    val unprovenDeterrence = conditionalDeterrence.copy(materialCounterplayPreventionProofReady = false)
    val unprovenEvent = planEvent.copy(
      results = List(unprovenDeterrence, independentFunction),
      representativeResult = Some(unprovenDeterrence)
    )
    val unprovenPlan = (MoveMeaningSurface.publicPayloadJson(meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail.copy(resolvedPlanEvent = Some(unprovenEvent)))
    )) \ "explanations" \ 0 \ "plan").as[play.api.libs.json.JsObject]
    assert(!(unprovenPlan \ "results").as[List[play.api.libs.json.JsObject]].exists(result =>
      (result \ "change").asOpt[String].exists(_.contains("material gain"))
    ), unprovenPlan)

  test("a plan target follows its representative chess result instead of the event target union"):
    val representativeResult = PlanResult(
      stage = "direct",
      kind = TransitionConsequenceKind.TargetPressureGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("d4"),
      sourcePlyOffset = Some(0)
    )
    val planEvent = ResolvedPlanEvent(
      goalTheme = "wing play",
      goalKind = Some("hook creation"),
      moveRole = Some(PlanMoveRole.Execution),
      transitionType = Some(TransitionType.Continuation),
      rootMove = candidateLine.rootMove,
      actorRole = Some("pawn"),
      actorFrom = Some("e2"),
      actorTo = Some("e3"),
      targets = List("d4", "square:e8", "square:f6"),
      developmentChoices = Nil,
      results = List(
        representativeResult,
        PlanResult(
          stage = "direct",
          kind = TransitionConsequenceKind.TargetPressureGain,
          polarity = StructuralSignalPolarity.Gain,
          strength = 1,
          subjects = List("f6"),
          sourcePlyOffset = Some(0)
        )
      ),
      responses = Nil,
      testedContinuation = None,
      representativeResult = Some(representativeResult)
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      axisKey = Some("PlanCoherence:Support:QueensideAttack"),
      axisKind = Some(StrategicAxisKind.PlanCoherence),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("QueensideAttack"),
      activePlanIds = List(PlanId.QueensideAttack),
      displayPlanId = Some(PlanId.QueensideAttack),
      planMoveRole = Some(PlanMoveRole.Execution),
      structuralRouteMove = Some(candidateLine.rootMove),
      sourceEvidenceIds = List("played-transition"),
      objectBindingSignatures = List(
        "actor=Move:e2e3|actor=Piece:pawn|actor=Square:e2|target=PlanSubject:queensideattack|target=Square:e8|target=Square:f6|mechanism=Mechanism:plan-continuity|consequence=Consequence:targetpressure"
      ),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      resolvedPlanEvent = Some(planEvent)
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )
    val planClaim = view.moveMeaningClaims.find(claim =>
      claim.unit == PositionPlanTechniqueUnit.PlanOptionSet &&
        claim.displayPlanId.contains(PlanId.QueensideAttack)
    ).getOrElse(fail("expected the typed plan claim"))
    val publicTargetSquares = planClaim.boardCarriers.collect {
      case carrier if carrier.role == "target" && carrier.kind == "Square" => carrier.value
    }

    assert(publicTargetSquares.contains("d4"))
    assert(!publicTargetSquares.exists(Set("e8", "f6")))

    val mismatchedView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail.copy(resolvedPlanEvent = Some(planEvent.copy(rootMove = "a2a4"))))
    )
    val mismatchedPlanClaims = mismatchedView.moveMeaningClaims.filter(claim =>
      claim.unit == PositionPlanTechniqueUnit.PlanOptionSet &&
        claim.displayPlanId.contains(PlanId.QueensideAttack)
    )
    assert(mismatchedPlanClaims.nonEmpty)
    assert(mismatchedPlanClaims.forall(!_.publicSurfaceAdmitted), mismatchedPlanClaims)
    assert(!MoveMeaningSurface.publicSurfaces(mismatchedView).exists(_.displayPlanId.contains(PlanId.QueensideAttack)))

  test("move meaning public surface does not call file battery pressure long diagonal"):
    val cause = causeFrame(
      causeId = "cause-file-battery",
      axisKeys = List("Activity:Gain:battery-pressure-gain"),
      objectSignatures = List(
        "actor=Move:e2e3|actor=Piece:rook|actor=Piece:queen|target=Square:a5|target=Square:h5|mechanism=Mechanism:battery-file|consequence=Consequence:filepressure"
      ),
      causeKind = RelativeCauseKind.ActivityGain
    ).copy(
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:battery-pressure-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("battery-pressure-gain"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("played-transition"),
      causeEvidenceIds = List("cause-file-battery"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("battery:file:a5-h5:rook-queen"),
      structuralPurposeConsequences = List("BatteryPressureGain"),
      objectBindingSignatures = List(
        "actor=Move:e2e3|actor=Piece:rook|actor=Piece:queen|target=Square:a5|target=Square:h5|mechanism=Mechanism:battery-file|consequence=Consequence:filepressure"
      ),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail)
    )

    assert(!MoveMeaningSurface.from(view).exists(_.ideaType == "long_diagonal_pressure"))

  test("deduplication selects one owned claim without borrowing sibling proof"):
    val owned = MoveMeaningClaim(
      meaningKind = "TargetPressure",
      role = "CreatesTargetPressure",
      laneKey = "target-pressure:e4",
      conflictKey = None,
      supportLevel = "owned_cause_linked",
      visibility = "reason_grade",
      surfaceLane = "current_move_owned",
      lineRole = "candidate",
      moveUci = candidateLine.rootMove,
      frameId = "owned-frame",
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      axisKey = Some("Target:Gain:e4"),
      axisKind = Some(StrategicAxisKind.Target),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("target-pressure"),
      causeKinds = List(RelativeCauseKind.TargetPressureGain),
      causeSourceSides = List(RelativeCauseSourceSide.Candidate),
      causeEvidenceIds = List("owned-cause"),
      sourceEvidenceIds = List("owned-source"),
      objectBindingSignatures = List("actor=Move:e2e3|target=Square:e4|mechanism=Mechanism:targetpressure"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val sibling = owned.copy(
      supportLevel = "view_surfaced",
      frameId = "sibling-frame",
      causeKinds = Nil,
      causeSourceSides = Nil,
      causeEvidenceIds = Nil,
      sourceEvidenceIds = List("sibling-source"),
      objectBindingSignatures = List("actor=Move:e2e3|target=Square:h4|mechanism=Mechanism:targetpressure"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ContextOnly
    )

    assertEquals(MoveMeaningClaim.selectMeaningClaim(List(sibling, owned)), Some(owned))

  test("move meaning claims let cross-comparison reference roots own reference resources"):
    val referenceAlternativeCause = causeFrame(
      causeId = "cause-cross-reference-break",
      axisKeys = List("PawnBreak:Support:reference-alternative-break"),
      objectSignatures = List("target=File:e|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    ).copy(
      comparisonKind = CandidateComparisonKind.ReferenceVsAlternative,
      causeRole = RelativeCauseRole.AlternativeDiagnostic,
      causeSourceSide = RelativeCauseSourceSide.Reference,
      eventLine = referenceLine,
      eventRootMove = referenceLine.rootMove,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:reference-alternative-break"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceStronger),
      breakFile = Some("e"),
      tensionSquares = List("e4", "d5"),
      referenceEvidenceIds = List("reference-alternative-transition"),
      sourceEvidenceIds = List("reference-alternative-transition"),
      causeEvidenceIds = List("cause-cross-reference-break"),
      proofRoles = List(RelativeCauseProofRole.DirectProof, RelativeCauseProofRole.ContrastProof),
      objectBindingSignatures = List("target=File:e|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      auditCauses = List(referenceAlternativeCause),
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims.count(claim =>
      claim.lineRole == "reference" && claim.moveUci == referenceLine.rootMove
    ), 1, view.moveMeaningClaims)
    assert(!view.moveMeaningClaims.exists(_.moveUci == candidateLine.rootMove), view.moveMeaningClaims)
    val explanations = MoveMeaningSurface.publicExplanations(
      MoveMeaningSurfaceVerdict(
        verdictCode = "mistake",
        moveQuality = "bad",
        playedMove = candidateLine.rootMove,
        referenceMove = referenceLine.rootMove
      ),
      MoveMeaningSurface.publicSurfaces(view)
    )
    assertEquals(explanations.size, 1)
    assertEquals((explanations.head \ "move").as[String], referenceLine.rootMove)
    assert((explanations.head \ "ideas").as[List[play.api.libs.json.JsObject]].nonEmpty)

    val referenceSurface = MoveMeaningSurface.publicSurfaces(view).head
    val playableExplanations = MoveMeaningSurface.publicExplanations(
      MoveMeaningSurfaceVerdict(
        verdictCode = "playable_loss",
        moveQuality = "playable",
        playedMove = candidateLine.rootMove,
        referenceMove = referenceLine.rootMove
      ),
      List(
        referenceSurface.copy(subject = "played_move", lineRole = "candidate", moveUci = candidateLine.rootMove),
        referenceSurface
      )
    )
    assertEquals(
      playableExplanations.map(explanation => (explanation \ "move").as[String]),
      List(candidateLine.rootMove, referenceLine.rootMove)
    )
    val badExplanations = MoveMeaningSurface.publicExplanations(
      MoveMeaningSurfaceVerdict(
        verdictCode = "mistake",
        moveQuality = "bad",
        playedMove = candidateLine.rootMove,
        referenceMove = referenceLine.rootMove
      ),
      List(
        referenceSurface.copy(subject = "played_move", lineRole = "candidate", moveUci = candidateLine.rootMove),
        referenceSurface
      )
    )
    assertEquals(
      badExplanations.map(explanation => (explanation \ "move").as[String]),
      List(candidateLine.rootMove, referenceLine.rootMove)
    )

    val referenceClaimWithThreatWitness = view.moveMeaningClaims.head.copy(
      comparisonMoveRefs = List(
        MoveMeaningSurfaceMoveRef("threat_pv_1", "e7e5"),
        MoveMeaningSurfaceMoveRef("threat_pv_2", "g1f3")
      )
    )
    assertEquals(
      MoveMeaningSurface.subject(referenceClaimWithThreatWitness),
      "reference_move"
    )

    val sameRootVerdict = view.verdict.map(frame =>
      frame.copy(referenceLine = frame.referenceLine.copy(rootMove = frame.candidateLine.rootMove))
    )
    assertEquals(
      MoveMeaningSurface.subject(referenceClaimWithThreatWitness.copy(moveUci = candidateLine.rootMove)),
      "reference_move"
    )
    val resolvedPlan = ResolvedPlanEvent(
      goalTheme = "piece redeployment",
      goalKind = Some("worst piece improvement"),
      moveRole = Some(PlanMoveRole.Preparation),
      transitionType = Some(TransitionType.Continuation),
      rootMove = candidateLine.rootMove,
      actorRole = Some("knight"),
      actorFrom = Some("e2"),
      actorTo = Some("g3"),
      targets = List("square:f6"),
      developmentChoices = Nil,
      results = Nil,
      responses = Nil,
      testedContinuation = None
    )
    val sameRootPlanClaim = referenceClaimWithThreatWitness.copy(
      moveUci = candidateLine.rootMove,
      resolvedPlanEvent = Some(resolvedPlan)
    )
    assertEquals(
      MoveMeaningSurface.publicSubject(sameRootVerdict, sameRootPlanClaim),
      "played_move"
    )
    assertEquals(
      MoveMeaningSurface.publicSubject(
        view.verdict,
        sameRootPlanClaim.copy(moveUci = referenceLine.rootMove)
      ),
      "reference_move"
    )

  test("cross-comparison candidate causes cannot become the current played move"):
    val alternativeLine = lineRef("second", "c2c4", 3, LineNodeRole.Alternative)
    val routeSignature =
      "actor=Move:c2c4|actor=Piece:knight|actor=Square:c2|target=Square:c4|mechanism=Mechanism:mobilitygain|proof=DirectProof"
    val foreignCandidateCause = causeFrame(
      causeId = "cause-cross-candidate-route",
      axisKeys = List("Activity:Gain:cross-candidate-route"),
      objectSignatures = List(routeSignature),
      causeKind = RelativeCauseKind.ActivityGain,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    ).copy(
      comparisonKind = CandidateComparisonKind.BestVsSecond,
      causeRole = RelativeCauseRole.CandidateSetConstraint,
      causeSourceSide = RelativeCauseSourceSide.Candidate,
      candidateLine = alternativeLine,
      eventLine = alternativeLine,
      eventRootMove = alternativeLine.rootMove,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:cross-candidate-route"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("played-transition"),
      causeEvidenceIds = List("cause-cross-candidate-route"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("knight:e2-e3:maneuver"),
      structuralPurposeConsequences = List("MobilityGain"),
      structuralMotifTags = List("piece", "route")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(foreignCandidateCause),
      details = List(detail)
    )

    assertEquals(
      MoveMeaningClaim.lineRoles(
        TypedEvidenceGraph(Nil),
        meaningClaimPlanTechniqueFrame(List(detail)),
        detail,
        view.verdict.get,
        List(foreignCandidateCause)
      ),
      List("contrast")
    )
    assert(!view.moveMeaningClaims.exists(claim =>
      claim.lineRole == "candidate" &&
        claim.moveUci == candidateLine.rootMove &&
        claim.causeEvidenceIds.contains("cause-cross-candidate-route")
    ), view.moveMeaningClaims)

  private def normalizedLinePayload(
      id: String,
      startFen: String,
      moves: List[String],
      summary: LineMaterialSummary
  ): (EvidenceRecord, LineFactEvidence) =
    val (_, moveRefs) = moves.zipWithIndex.foldLeft(startFen -> List.empty[PrincipalVariationEvidence.LineMoveRef]) {
      case ((fenBefore, accumulated), (move, index)) =>
        val fenAfter = PrincipalVariationEvidence.legalFenAfter(fenBefore, move).getOrElse(fail(s"illegal $move"))
        fenAfter -> (accumulated :+ PrincipalVariationEvidence.LineMoveRef(index + 1, move, fenAfter))
    }
    val validated = PrincipalVariationEvidence
      .validatedLine(startFen, PrincipalVariationEvidence.LineVariationRef(moveRefs), moves.head)
      .get
    val facts = PrincipalVariationEvidence.LineFacts(
      line = validated.line,
      first = validated.moves.head,
      reply = validated.reply,
      continuation = validated.continuation,
      continuationTail = validated.moves.drop(3)
    )
    val record = LineFactNormalizer.fromValidatedLine(
      id = id,
      lineRef = lineRef(id, moves.head, 1, LineNodeRole.Played),
      facts = facts,
      position = PositionNodeRef(startFen, 1, Some(summary.sideToMove), Some("root")),
      scope = EvidenceScope.PlayedTransition,
      materialSummary = Some(summary)
    )
    record -> record.payload.asInstanceOf[LineFactEvidence]

  private def lineRef(id: String, rootMove: String, rank: Int, role: LineNodeRole): LineNodeRef =
    LineNodeRef(id, rootMove, rank, role)

  private val referenceLine = lineRef("best", "e2e4", 1, LineNodeRole.BestReference)
  private val candidateLine = lineRef("played", "e2e3", 2, LineNodeRole.Played)
  private val meaningClaimPosition =
    PositionNodeRef("8/8/8/8/8/8/4P3/4K3 w - - 0 1", 1, Some(chess.Color.White), Some("root"))

  private def lineDiagnostic(ref: LineNodeRef): CandidateLineDiagnostic =
    CandidateLineDiagnostic(
      id = ref.id,
      rootMove = ref.rootMove,
      role = ref.role,
      rank = ref.rank,
      moves = List(ref.rootMove),
      whitePovEvalCp = Some(20),
      mate = None,
      depth = Some(16)
    )

  private def evidenceRef(id: String, position: PositionNodeRef): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = position,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )

  private def semanticAxes(referenceLeadAxes: List[String]): SemanticAxisDiagnostics =
    SemanticAxisDiagnostics(
      referenceAxisStrengths = referenceLeadAxes.map(_ -> 2).toMap,
      candidateAxisStrengths = Map.empty,
      referenceAxisSourceIds = referenceLeadAxes.map(_ -> List("axis-src")).toMap,
      candidateAxisSourceIds = Map.empty,
      referenceAxisSourceLayers = referenceLeadAxes.map(_ -> List(EvidenceLayer.StrategicMechanism)).toMap,
      candidateAxisSourceLayers = Map.empty,
      referenceAxes = referenceLeadAxes,
      candidateAxes = Nil,
      sharedAxes = Nil,
      referenceOnlyAxes = referenceLeadAxes,
      candidateOnlyAxes = Nil,
      referenceLeadAxes = referenceLeadAxes,
      candidateLeadAxes = Nil
    )

  private def causeFlow(
      causeId: String,
      kind: RelativeCauseKind,
      proofAxisKeys: List[String],
      claimIds: List[String]
  ): RelativeCauseFlowDiagnostic =
    RelativeCauseFlowDiagnostic(
      causeId = causeId,
      causeKind = kind,
      causeRole = RelativeCauseRole.PrimaryPlayedCause,
      causeComparisonKind = CandidateComparisonKind.PlayedVsBest,
      causeSourceSide = RelativeCauseSourceSide.Candidate,
      causeEventLine = candidateLine,
      proofStrategicAxisKeys = proofAxisKeys,
      claimCandidateFamilies = Set(ClaimFamily.Strategic),
      finalClaimFamilies = Set(ClaimFamily.Strategic),
      directProofSourceIds = List("direct-proof"),
      contrastProofSourceIds = Nil,
      contextSupportSourceIds = Nil,
      directProofKinds = proofAxisKeys.map(axis => s"StrategicAxis:$axis"),
      contrastProofKinds = Nil,
      contextSupportKinds = Nil,
      hasOwnedTypedDepth = true,
      hasOwnedAdmissibleLongTermProof = true,
      eventClusterExpected = false,
      ideaIds = List("idea-plan"),
      claimCandidateIds = List("claim-candidate-plan"),
      claimCandidateStages = Set(ClaimLifecycleStage.CandidateCreated, ClaimLifecycleStage.TruthCertified),
      claimCandidateTruthStatuses = Set(ClaimLifecycleTruthStatus.Certified),
      claimCandidateDroppedStages = Set.empty,
      claimIds = claimIds,
      eventClusterIds = Nil,
      eventClusterMissingSupportEvidenceIds = Nil,
      causeWithoutIdea = false,
      ideaWithoutClaimCandidate = false,
      ideaWithoutFinalClaim = false,
      claimWithoutEventCluster = false,
      strategicCauseWithoutContrast = false,
      contextSupportUsedAsDirectProof = false,
      contextOnlyAttribution = false,
      unattributedCause = false,
      rootMismatchedAttribution = false,
      supportPromotedToDirectProof = false,
      objectBindingSignatures = List("axis:outpost"),
      relativeCauseWithoutObjectSignature = false,
      objectLostBetweenEvidenceAndCause = false,
      objectLostBetweenCauseAndClaim = false
    )

  private def causeFrame(
      causeId: String,
      axisKeys: List[String],
      objectSignatures: List[String],
      witnessBindingLevel: MoveJudgmentCauseWitnessBindingLevel = MoveJudgmentCauseWitnessBindingLevel.NotWitness,
      rootArbitrationTier: MoveJudgmentCauseRootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ContextOnly,
      causeKind: RelativeCauseKind = RelativeCauseKind.ActivityGain
  ): MoveJudgmentCauseFrame =
    MoveJudgmentCauseFrame(
      role = MoveJudgmentCauseFrameRole.PrimaryCause,
      clusterId = None,
      framed = false,
      causeEvidenceIds = List(causeId),
      causeKind = causeKind,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      causeRole = RelativeCauseRole.PrimaryPlayedCause,
      causeSourceSide = RelativeCauseSourceSide.Candidate,
      causeImportance = RelativeCauseImportance.Primary,
      attributionKind = CauseAttributionKind.CandidateAllowsLiability,
      attributionRootMoveMatched = true,
      attributionDirectProofEligible = true,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      eventLine = candidateLine,
      eventRootMove = candidateLine.rootMove,
      causeClaimIds = Nil,
      evaluationClaimIds = Nil,
      witnessClaimIds = Nil,
      ideaIds = Nil,
      supportIdeaIds = Nil,
      claimCandidateIds = Nil,
      finalClaimIds = Nil,
      relatedSupportClusterIds = Nil,
      evidenceIds = List(causeId),
      proofDirectSourceIds = Nil,
      proofContrastSourceIds = Nil,
      proofContextSupportSourceIds = Nil,
      proofStrategicAxisLineage = Nil,
      proofStrategicAxisKeys = axisKeys,
      proofStrategicMechanismKinds = Nil,
      proofStrategicMechanismSourceIds = Nil,
      proofStrategicMechanismSignalSourceIds = Nil,
      supportEvidenceSourceIds = Nil,
      objectBindingSignatures = objectSignatures,
      concreteObjectReady = objectSignatures.nonEmpty,
      rootArbitrationTier = rootArbitrationTier,
      witnessBindingLevel = witnessBindingLevel,
      witnessBindingSignals =
        if witnessBindingLevel == MoveJudgmentCauseWitnessBindingLevel.SameComparisonOnly then
          List(MoveJudgmentCauseWitnessBindingSignal.SameComparison)
        else Nil
    )

  private def meaningClaimView(
      verdict: MoveChoiceVerdict,
      comparisonKind: CandidateComparisonKind = CandidateComparisonKind.PlayedVsBest,
      auditCauses: List[MoveJudgmentCauseFrame],
      details: List[PositionPlanTechniqueSemanticDetail],
      reference: LineNodeRef = referenceLine,
      candidate: LineNodeRef = candidateLine
  ): MoveJudgmentView =
    val base =
      MoveJudgmentView(
      verdict = Some(
        MoveJudgmentVerdictFrame(
          verdict = verdict,
          winPercentLossForMover = 0.0,
          candidateWinPercentDeltaForMover = 0.0,
          relativeAssessmentEvidenceId = "relative-assessment",
          verdictCertificationEvidenceId = None,
          comparisonKind = comparisonKind,
          referenceLine = reference,
          candidateLine = candidate
        )
      ),
      verdictCarriers = Nil,
      causeAudit = MoveJudgmentCauseAudit(context = auditCauses),
      positionPlanTechniqueFrames = List(meaningClaimPlanTechniqueFrame(details, candidate)),
      supportContextClusterIds = Nil,
      overriddenLocalIdeas = Nil,
      preservedLocalIdeas = Nil
    )
    meaningClaimViewWithClaims(base)

  private def meaningClaimViewWithClaims(view: MoveJudgmentView): MoveJudgmentView =
    val cleared = view.copy(moveMeaningClaims = Nil)
    val claims = MoveMeaningClaim.from(TypedEvidenceGraph(Nil), cleared, cleared.causeAudit.all)
    cleared.copy(moveMeaningClaims = claims)

  private def meaningClaimPlanTechniqueFrame(
      details: List[PositionPlanTechniqueSemanticDetail],
      candidate: LineNodeRef = candidateLine
  ): PositionPlanTechniqueFrame =
    PositionPlanTechniqueFrame(
      id = "frame-move-meaning",
      units = details.map(_.unit).distinct.sortBy(_.toString),
      position = meaningClaimPosition,
      line = Some(candidate),
      moveUci = Some(candidate.rootMove),
      scope = EvidenceScope.PlayedTransition,
      mechanismKinds = details.flatMap(_.mechanismKinds).distinct.sortBy(_.toString),
      strategicAxisKeys = details.flatMap(_.axisKey).distinct.sorted,
      semanticAnchors = Nil,
      objectBindingSignatures = details.flatMap(_.objectBindingSignatures).distinct.sorted,
      semanticDetails = details,
      evidenceIds = details.flatMap(_.sourceEvidenceIds).distinct.sorted,
      mechanismEvidenceIds = details.flatMap(_.sourceEvidenceIds).distinct.sorted,
      sourceEvidenceIds = details.flatMap(_.sourceEvidenceIds).distinct.sorted,
      relativeCauseEvidenceIds = details.flatMap(_.causeEvidenceIds).distinct.sorted,
      ideaIds = Nil,
      claimIds = Nil,
      planComparison = None,
      relationToVerdict = None,
      confidence = EvidenceConfidence.EngineBacked,
      salience = 10
    )

  private def dynamicAssessment: SinglePositionAssessment =
    SinglePositionAssessment(
      nature = NatureResult(NatureType.Dynamic, tensionScore = 2, openFilesCount = 1, mobilityDiff = 0, lockedCenter = false),
      criticality = CriticalityResult(
        CriticalityType.Normal,
        rawSideRelativeEvalDeltaCpForDiagnostics = Some(120),
        evalDeltaWinPercent = Some(6.0),
        mateDistance = None,
        forcingMovesInPv = 0
      ),
      candidateSet = CandidateSetTopology(
        CandidateSetType.NarrowChoice,
        bestLineSideRelativeEvalCp = Some(320),
        secondLineSideRelativeEvalCp = Some(-320),
        thirdLineSideRelativeEvalCp = None,
        gapBestToSecondWp = Some(8.0),
        spreadTop3Wp = None,
        secondCandidateFailure = Some(CandidateFailureMode.SignificantDisadvantage)
      ),
      gamePhase = GamePhaseResult(GamePhaseType.Middlegame, totalMaterial = 24, queensOnBoard = true, minorPiecesCount = 4),
      simplifyBias = SimplifyBiasResult(
        isSimplificationWindow = false,
        rawEvalAdvantageCpForDiagnostics = 0,
        evalAdvantageWinPercent = 0.0,
        isEndgameNear = false,
        exchangeAvailable = false
      ),
      drawBias = DrawBiasResult(
        isDrawish = false,
        materialSymmetry = false,
        oppositeColorBishops = false,
        fortressLikely = false,
        insufficientMaterial = false
      ),
      riskProfile = RiskProfileResult(RiskLevel.Medium, evalVolatility = 80, tacticalMotifsCount = 1, kingExposureSum = 1),
      judgmentFocus = JudgmentFocusResult(JudgmentFocusType.Plan, JudgmentDriver.DynamicPosition)
    )

  private def comparisonDiagnostic(
      id: String,
      referenceLeadAxes: List[String],
      referenceStructuralConsequences: List[TransitionConsequenceKind] = Nil,
      candidateStructuralConsequences: List[TransitionConsequenceKind] = Nil,
      producedKinds: List[RelativeCauseKind],
      flows: List[RelativeCauseFlowDiagnostic],
      primaryRootKinds: List[RelativeCauseKind],
      primaryRootIds: List[String],
      positionPlanTechniqueFrameIds: List[String],
      positionPlanTechniqueUnits: List[PositionPlanTechniqueUnit],
      positionPlanTechniqueAxisKeys: List[String] = Nil,
      positionPlanTechniqueSemanticDetailUnits: List[PositionPlanTechniqueUnit] = Nil,
      positionPlanTechniqueSemanticDetailAxisKeys: List[String] = Nil,
      positionPlanTechniqueSemanticDetailMechanismKinds: List[StrategicMechanismKind] = Nil,
      positionPlanTechniqueSemanticDetailAnchorKeys: List[String] = Nil,
      positionPlanTechniqueSemanticDetailTokens: List[String] = Nil,
      positionPlanTechniqueSemanticDetailTokenGroups: List[List[String]] = Nil,
      positionPlanTechniqueObjectBindingSignatures: List[String] = Nil,
      positionPlanTechniqueEvidenceIds: List[String] = Nil,
      positionPlanTechniqueRelativeCauseEvidenceIds: List[String] = Nil,
      publicMoveMeaningClaimDiagnostics: List[PublicMoveMeaningClaimDiagnostic] = Nil,
      rootArbitrationTiers: List[MoveJudgmentCauseRootArbitrationTier] = Nil,
      primaryRootArbitrationTiers: List[MoveJudgmentCauseRootArbitrationTier] = Nil,
      verdict: MoveChoiceVerdict = MoveChoiceVerdict.Mistake,
      relationKinds: List[RelationFactKind] = Nil
  ): CandidateComparisonDiagnostic =
    val axes = semanticAxes(referenceLeadAxes)
    val rootCauseEvidenceTiers =
      primaryRootIds
        .zip(primaryRootArbitrationTiers)
        .map { case (causeEvidenceId, tier) => PrimaryRootCauseEvidenceTier(causeEvidenceId, tier) }
    CandidateComparisonDiagnostic(
      id = id,
      comparisonFingerprint = id,
      dedupeKey = id,
      dedupeClass = CandidateComparisonDedupeClass.Unique,
      subjectBinding = SubjectBindingClass.PrimaryPlayedCause,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = lineDiagnostic(referenceLine),
      candidateLine = lineDiagnostic(candidateLine),
      verdict = verdict,
      mover = "White",
      rawCpLossForDiagnostics = 120,
      rawCandidateDeltaCpForDiagnostics = -120,
      winPercentLossForMover = 18.0,
      candidateWinPercentDeltaForMover = -18.0,
      causeKinds = producedKinds,
      causeRoles = List(RelativeCauseRole.PrimaryPlayedCause),
      causeSourceSides = List(RelativeCauseSourceSide.Candidate),
      causeImportances = Nil,
      causeEventLines = List(candidateLine),
      causeSupport = Nil,
      relativeCauseDiagnostics = ComparisonRelativeCauseDiagnostics(
        expectedCauseHints = producedKinds,
        missingExpectedCauseHints = Nil,
        producedCauseIds = flows.map(_.causeId),
        producedCauseKinds = producedKinds,
        producedCauseRoles = List(RelativeCauseRole.PrimaryPlayedCause),
        producedCauseSourceSides = List(RelativeCauseSourceSide.Candidate),
        producedCauseEventLines = List(candidateLine),
        missingCause = false,
        shallowProofCauseIds = Nil,
        ownedTypedDepthCauseIds = flows.map(_.causeId),
        unboundEvidenceIds = Nil,
        wrongRoleCauseIds = Nil,
        wrongSourceSideCauseIds = Nil,
        wrongEventLineCauseIds = Nil,
        wrongImportanceCauseIds = Nil,
        causeWithoutIdeaIds = Nil,
        ideaWithoutClaimCandidateCauseIds = Nil,
        ideaWithoutFinalClaimCauseIds = Nil,
        ideaWithoutClaimCauseIds = Nil,
        claimWithoutEventClusterCauseIds = Nil,
        eventClusterSupportMissingCauseIds = Nil,
        strategicCauseWithoutContrastIds = Nil,
        strategicClaimWithoutComparativeCauseIds = Nil,
        genericStructuralImprovementWithoutStrategicContrastIds = Nil,
        contextSupportUsedAsDirectProofIds = Nil,
        contextOnlyCauseIds = Nil,
        unattributedCauseIds = Nil,
        rootMismatchedCauseIds = Nil,
        supportPromotedToDirectProofCauseIds = Nil,
        relativeCauseWithoutObjectSignatureIds = Nil,
        objectLostBetweenEvidenceAndCauseIds = Nil,
        objectLostBetweenCauseAndClaimIds = Nil,
        causeFlow = flows
      ),
      moveJudgmentView = ComparisonMoveJudgmentViewDiagnostics(
        primaryCauseKinds = primaryRootKinds,
        secondaryCauseKinds = Nil,
        contextCauseKinds = Nil,
        primaryCauseEvidenceIds = primaryRootIds,
        primaryIdeaFamilies = Set(ChessIdeaFamily.Strategic),
        primaryClaimCandidateFamilies = Set(ClaimFamily.Strategic),
        primaryFinalClaimFamilies = Set(ClaimFamily.Strategic),
        primaryFramedCauseKinds = Nil,
        primaryUnframedCauseKinds = primaryRootKinds,
        primaryRootCauseKinds = primaryRootKinds,
        primaryTacticalWitnessCauseKinds = Nil,
        primaryPunishmentWitnessCauseKinds = Nil,
        primaryContextualTacticalWitnessCauseKinds = Nil,
        primaryRootCauseEvidenceIds = primaryRootIds,
        primaryTacticalWitnessCauseEvidenceIds = Nil,
        primaryPunishmentWitnessCauseEvidenceIds = Nil,
        primaryContextualTacticalWitnessCauseEvidenceIds = Nil,
        rootArbitrationTiers = rootArbitrationTiers,
        primaryRootArbitrationTiers = primaryRootArbitrationTiers,
        primaryRootCauseEvidenceTiers = rootCauseEvidenceTiers,
        secondaryCauseEvidenceIds = Nil,
        contextCauseEvidenceIds = Nil,
        projectedContextCauseNoViewIds = Nil,
        playableLossPrimaryCauseEvidenceIds = Nil,
        objectlessPrimaryCauseEvidenceIds = Nil,
        objectlessSecondaryCauseEvidenceIds = Nil,
        objectlessContextCauseEvidenceIds = Nil,
        positionPlanTechniqueFrameIds = positionPlanTechniqueFrameIds,
        positionPlanTechniqueUnits = positionPlanTechniqueUnits,
        positionPlanTechniqueAxisKeys = positionPlanTechniqueAxisKeys,
        positionPlanTechniqueSemanticDetailUnits = positionPlanTechniqueSemanticDetailUnits,
        positionPlanTechniqueSemanticDetailAxisKeys = positionPlanTechniqueSemanticDetailAxisKeys,
        positionPlanTechniqueSemanticDetailMechanismKinds = positionPlanTechniqueSemanticDetailMechanismKinds,
        positionPlanTechniqueSemanticDetailAnchorKeys = positionPlanTechniqueSemanticDetailAnchorKeys,
        positionPlanTechniqueSemanticDetailTokens = positionPlanTechniqueSemanticDetailTokens,
        positionPlanTechniqueSemanticDetailTokenGroups = positionPlanTechniqueSemanticDetailTokenGroups,
        positionPlanTechniqueObjectBindingSignatures = positionPlanTechniqueObjectBindingSignatures,
        positionPlanTechniqueEvidenceIds = positionPlanTechniqueEvidenceIds,
        positionPlanTechniqueRelativeCauseEvidenceIds = positionPlanTechniqueRelativeCauseEvidenceIds,
        publicMoveMeaningClaimDiagnostics = publicMoveMeaningClaimDiagnostics
      ),
      advisoryCauseHints = Nil,
      significanceReasons = List(CandidateComparisonSignificanceReason.PlayedLoss),
      lowSignalReasons = Nil,
      comparisonConfidence = EvidenceConfidence.EngineBacked,
      causeConfidences = List(EvidenceConfidence.EngineBacked),
      hasLowDepthCause = false,
      hasUnexplainedEngineGap = false,
      hasSecondaryContextEngineGap = false,
      evidenceLayers = ComparisonEvidenceLayerDiagnostics(
        reference = EvidenceLayerNeighborhood(Set.empty, Map.empty, Map.empty),
        candidate = EvidenceLayerNeighborhood(Set.empty, Map.empty, Map.empty)
      ),
      decisionTrace = CandidateCauseDecisionTrace(
        badLoss = true,
        tacticalLoss = false,
        majorLoss = true,
        candidateBetter = false,
        requiresExplanatoryCause = true,
        positiveContextAlternative = false,
        referenceTacticalMechanismKinds = Nil,
        candidateTacticalMechanismKinds = Nil,
        referenceConcreteLine = true,
        candidateConcreteLine = true,
        candidateTacticalRefutationBridge = false,
        referenceTacticalRisk = false,
        hasOnlyDefense = false,
        hasThreatResource = false,
        referenceProphylacticResource = false,
        referenceKingStepResource = false,
        candidateKingStepResource = false,
        referenceCastlingResource = false,
        candidateCastlingResource = false,
        referencePreventivePawnResource = false,
        candidatePreventivePawnResource = false,
        hasConversionWindow = false,
        referenceConversionWindow = false,
        candidateConversionWindow = false,
        referenceStructuralTargetRelease = false,
        referenceReleasedTargets = Nil,
        candidateReleasedTargets = Nil,
        referenceCreatedTargets = Nil,
        candidateCreatedTargets = Nil,
        referenceStructuralImprovement = true,
        candidateStructuralImprovement = false,
        candidatePawnStructureImprovement = false,
        candidateTargetPressureGain = false,
        candidateCenterControlGain = false,
        candidateDevelopmentActivation = false,
        candidatePieceActivityGain = false,
        sameDestinationCaptureChoice = false,
        referenceStructuralSignals = referenceLeadAxes,
        candidateStructuralSignals = Nil,
        referenceStructuralConsequences = referenceStructuralConsequences,
        candidateStructuralConsequences = candidateStructuralConsequences,
        candidatePawnStructureSignals = Nil,
        referenceSemanticAxisCount = referenceLeadAxes.size,
        candidateSemanticAxisCount = 0,
        referenceSemanticAxisLead = referenceLeadAxes.nonEmpty,
        semanticAxisDiagnostics = axes,
        candidatePlanEvidence = false,
        candidateStrategicEvidence = false,
        candidateStrategicConcessionEvidence = true,
        referencePassedPawnResource = false,
        candidatePassedPawnResource = false,
        referenceEndgameResource = false,
        candidateEndgameResource = false,
        referenceLooseMaterialExploit = false,
        candidateLooseMaterialExploit = false,
        materialSwingEvidence = false,
        referenceMaterialNetCp = 0,
        candidateMaterialNetCp = 0,
        referenceMaterialMaxGainCp = 0,
        candidateMaterialMaxGainCp = 0,
        referenceMaterialPromotionGainCp = 0,
        candidateMaterialPromotionGainCp = 0,
        referenceMaterialRecapture = false,
        candidateMaterialRecapture = false,
        referenceMaterialRecovery = false,
        candidateMaterialRecovery = false,
        referenceMaterialComplete = false,
        candidateMaterialComplete = false,
        referenceRelationKinds = relationKinds,
        candidateRelationKinds = relationKinds,
        relationKinds = relationKinds,
        referenceMotifs = Nil,
        candidateMotifs = Nil
      ),
      tacticalLossTrace = TacticalLossTrace(false, 0.0, None, None, Nil),
      failureClass = CandidateComparisonFailureClass.NoFailure,
      failureReasons = Nil
    )

  private def publicMoveMeaningClaimDiagnostic(
      unit: PositionPlanTechniqueUnit,
      meaningKind: String,
      supportLevel: String,
      surfaceLane: String,
      axisKey: Option[String] = None,
      lineRole: String,
      moveUci: String,
      causeEvidenceIds: List[String] = Nil,
      sourceEvidenceIds: List[String] = Nil,
      hasCarrier: Option[Boolean] = None,
      hasBoardCarrier: Option[Boolean] = None,
      proofLevel: String = ""
  ): PublicMoveMeaningClaimDiagnostic =
    val graphCarrier = causeEvidenceIds.nonEmpty || sourceEvidenceIds.nonEmpty
    val lineMoveCarrier = lineRole.trim.nonEmpty && moveUci.trim.nonEmpty
    val effectiveHasCarrier = hasCarrier.getOrElse(graphCarrier && lineMoveCarrier)
    val effectiveHasBoardCarrier = hasBoardCarrier.getOrElse(effectiveHasCarrier)
    val effectiveProofLevel =
      Option(proofLevel).map(_.trim).filter(_.nonEmpty).getOrElse {
        if !effectiveHasCarrier then "none"
        else if causeEvidenceIds.nonEmpty && supportLevel == "owned_cause_linked" then "owned_cause"
        else if causeEvidenceIds.nonEmpty then "cause_linked"
        else "surface_evidence"
      }
    PublicMoveMeaningClaimDiagnostic(
      unit = unit,
      axisKey = axisKey,
      meaningKind = meaningKind,
      supportLevel = supportLevel,
      surfaceLane = surfaceLane,
      lineRole = lineRole,
      moveUci = moveUci,
      causeEvidenceIds = causeEvidenceIds,
      sourceEvidenceIds = sourceEvidenceIds,
      publicSurfaceAdmitted = effectiveHasCarrier,
      hasBoardCarrier = effectiveHasBoardCarrier,
      proofLevel = effectiveProofLevel
    )
