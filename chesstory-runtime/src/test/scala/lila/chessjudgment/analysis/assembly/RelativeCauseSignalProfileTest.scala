package lila.chessjudgment.analysis.assembly

import chess.Color
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, StructuralDeltaContracts }
import lila.chessjudgment.model.judgment.*

class RelativeCauseSignalProfileTest extends munit.FunSuite:

  test("does not mark contrast-only context attribution as root mismatch"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val line = LineNodeRef("line", "g1f3", 1, LineNodeRole.BestReference)
    val contrastRef = EvidenceRef(
      id = "line:contrast",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = root,
      line = Some(line),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )

    val attribution = CauseAttribution(
      kind = CauseAttributionKind.ContextOnly,
      contrastEvidence = List(contrastRef),
      rootMoveMatched = false,
      directProofEligible = false,
      reason = Some("no-owned-direct-proof")
    )

    assertEquals(attribution.rootMismatch, false)

  test("candidate loose material liability binds mover, piece, and square"):
    val root = PositionNodeRef("8/8/8/8/8/8/7P/R3K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val candidateLine = LineNodeRef("candidate-line", "h2h3", 2, LineNodeRole.Played)
    val threatLine = LineNodeRef("threat-line", "a8a7", 1, LineNodeRole.Threat)
    def materialLossRecord(
        id: String,
        line: LineNodeRef,
        sideToMove: Color,
        captureSide: Color,
        capturedRole: EvidencePieceRole,
        square: EvidenceSquare
    ): EvidenceRecord =
      val summary =
        LineMaterialSummary(
          sideToMove = sideToMove,
          captures = List(
            LineMaterialCapture(
              moveUci = "b8a7",
              plyOffset = 1,
              side = captureSide,
              attackerRole = EvidencePieceRole("Bishop"),
              capturedRole = capturedRole,
              square = square,
              valueCp = 500,
              recapture = false
            )
          ),
          netCaptureCpForMover = -500,
          maxGainCpForMover = 0,
          maxLossCpForMover = 500,
          hasRecaptureChain = false,
          hasRecoveryWindow = false,
          promotionGainCpForMover = 0,
          materialWindowComplete = true
        )
      EvidenceRecord(
        ref = EvidenceRef(
          id = id,
          producer = EvidenceProducer.LegalLineProducer,
          layer = EvidenceLayer.Line,
          position = root,
          line = Some(line),
          scope = line.role.scope,
          confidence = EvidenceConfidence.LegalReplayVerified
        ),
        payload = new LineFactEvidence(line, Some(line.rootMove), None, Nil, None, Some(summary))()
      )
    val matchingLoss =
      materialLossRecord(
        "line:matching-loss",
        candidateLine,
        Color.White,
        Color.Black,
        EvidencePieceRole("Rook"),
        EvidenceSquare("a7")
      )
    val wrongDirection =
      materialLossRecord(
        "line:wrong-direction",
        threatLine,
        Color.Black,
        Color.White,
        EvidencePieceRole("Rook"),
        EvidenceSquare("a7")
      )
    val wrongPiece =
      materialLossRecord(
        "line:wrong-piece",
        candidateLine,
        Color.White,
        Color.Black,
        EvidencePieceRole("Pawn"),
        EvidenceSquare("a7")
      )
    val wrongSquare =
      materialLossRecord(
        "line:wrong-square",
        candidateLine,
        Color.White,
        Color.Black,
        EvidencePieceRole("Rook"),
        EvidenceSquare("b7")
      )
    val anchorRecord =
      EvidenceRecord(
        ref = EvidenceRef(
          id = "board:loose-white-rook-a7",
          producer = EvidenceProducer.BoardFactProducer,
          layer = EvidenceLayer.Board,
          position = root,
          line = None,
          scope = EvidenceScope.CurrentPosition,
          confidence = EvidenceConfidence.BoardDerived
        ),
        payload = new BoardFactEvidence(Nil, None)(
          anchors = List(
            BoardAnchor(
              kind = BoardAnchorKind.LooseMaterial,
              side = Color.Black,
              signal = BoardAnchorSignal.AttackedTarget,
              magnitude = 5,
              confidence = 1.0,
              detail = Some(
                BoardAnchorDetail(
                  subjectColor = Some(Color.White),
                  subjectSquare = Some(EvidenceSquare("a7")),
                  subjectRole = Some(EvidencePieceRole("Rook"))
                )
              )
            )
          )
        )
      )

    val matched =
      RelativeCauseSignalProfile.looseMaterialLiabilityRecords(
        Color.White,
        List(matchingLoss, wrongDirection, wrongPiece, wrongSquare),
        List(anchorRecord)
      )

    assertEquals(matched.map(_.ref.id), List("line:matching-loss"))

  test("structural delta records pawn tension from the moved pawn landing square"):
    val beforeFen = "4k3/8/8/3p4/8/8/4P3/4K3 w - - 0 1"
    val afterFen = "4k3/8/8/3p4/4P3/8/8/4K3 b - - 0 1"
    val beforeBoard = Fen.read(Standard, Fen.Full(beforeFen)).get.board
    val afterBoard = Fen.read(Standard, Fen.Full(afterFen)).get.board
    val delta =
      StructuralDeltaAnalyzer
        .delta(
          beforeFen = beforeFen,
          beforeBoard = beforeBoard,
          afterFen = afterFen,
          afterBoard = afterBoard,
          side = Color.White,
          files = ('a' to 'h').toList,
          targets = Nil,
          createdTensionFrom = Some("e4"),
          moveUci = Some("e2e4")
        )
        .get
    val consequences = StructuralDeltaContracts.consequences(delta)

    assert(delta.createdTension.contains("e4-d5"), delta.createdTension)
    assert(
      consequences.exists(consequence =>
        consequence.kind == TransitionConsequenceKind.PawnTensionGain &&
          consequence.subjects.contains("created-tension:e4-d5") &&
          consequence.subjects.contains("break-file:e")
      ),
      consequences
    )

  test("structural pawn break axes keep created and resolved tension separate"):
    val root = PositionNodeRef("8/8/4p3/3p4/2P5/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("8/8/4p3/3P4/8/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after"))
    val playedLine = LineNodeRef("played-line", "c4d5", 1, LineNodeRole.Played)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:c4d5:mixed-tension",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val structural = StructuralDeltaEvidence(
      transition = StructuralTransitionBinding(
        moveUci = "c4d5",
        role = TransitionEdgeRole.Played,
        from = root,
        to = after,
        line = Some(playedLine),
        perspective = Color.White
      ),
      signals = Nil,
      consequences = List(
        TransitionConsequence(
          TransitionConsequenceKind.PawnTensionResolution,
          StructuralSignalPolarity.Neutral,
          strength = 1,
          subjects = List("break-file:c", "resolved-tension:c4-d5")
        ),
        TransitionConsequence(
          TransitionConsequenceKind.PawnTensionGain,
          StructuralSignalPolarity.Gain,
          strength = 1,
          subjects = List("break-file:d", "created-tension:d5-e6")
        )
      )
    )
    val record = EvidenceRecord(structuralRef, structural)
    val axes =
      StrategicMechanismEvidence
        .sourceMechanisms(record)
        .collect { case (StrategicMechanismKind.PawnStructure, signal) => signal.axis.map(_.stableKey) }
        .flatten

    assert(axes.contains("PawnBreak:Release:break-file-c-resolved-tension-c4-d5"), axes)
    assert(axes.contains("PawnBreak:Support:break-file-d-created-tension-d5-e6"), axes)
    assert(!axes.exists(axis => axis.contains("created-tension") && axis.contains("resolved-tension")), axes)

  test("structural delta records current pawn move restricting opponent fianchetto bishop"):
    val beforeFen = "4k3/6b1/8/3p4/3PP3/8/8/4K3 w - - 0 1"
    val afterFen = "4k3/6b1/8/3pP3/3P4/8/8/4K3 b - - 0 1"
    val beforeBoard = Fen.read(Standard, Fen.Full(beforeFen)).get.board
    val afterBoard = Fen.read(Standard, Fen.Full(afterFen)).get.board
    val delta =
      StructuralDeltaAnalyzer
        .delta(
          beforeFen = beforeFen,
          beforeBoard = beforeBoard,
          afterFen = afterFen,
          afterBoard = afterBoard,
          side = Color.White,
          files = ('a' to 'h').toList,
          targets = Nil,
          moveUci = Some("e4e5")
        )
        .get
    val consequences = StructuralDeltaContracts.consequences(delta)

    assert(
      delta.opponentMobilityRestrictions.exists(_.startsWith("bishop:g7:diagonal-denial:blocked-by:e5:")),
      delta.opponentMobilityRestrictions
    )
    assert(
      consequences.exists(consequence =>
        consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
          consequence.subjects.exists(_.startsWith("bishop:g7:diagonal-denial:blocked-by:e5:"))
      ),
      consequences
    )

  test("does not record diagonal restriction without a locked center"):
    val beforeFen = "4k3/6b1/3p4/8/3PP3/8/8/4K3 w - - 0 1"
    val afterFen = "4k3/6b1/3p4/4P3/3P4/8/8/4K3 b - - 0 1"
    val beforeBoard = Fen.read(Standard, Fen.Full(beforeFen)).get.board
    val afterBoard = Fen.read(Standard, Fen.Full(afterFen)).get.board
    val delta =
      StructuralDeltaAnalyzer
        .delta(
          beforeFen = beforeFen,
          beforeBoard = beforeBoard,
          afterFen = afterFen,
          afterBoard = afterBoard,
          side = Color.White,
          files = ('a' to 'h').toList,
          targets = Nil,
          moveUci = Some("e4e5")
        )
        .get

    assert(!delta.opponentMobilityRestrictions.exists(_.contains(":diagonal-denial:")), delta.opponentMobilityRestrictions)
    assert(
      delta.opponentMobilityRestrictions.exists(_.startsWith("bishop:g7-f6:entry-restricted:by:e4e5:exchange-gain:")),
      delta.opponentMobilityRestrictions
    )
    assert(
      !StructuralDeltaContracts
        .consequences(delta)
        .exists(consequence =>
          consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
            consequence.subjects.exists(_.contains(":diagonal-denial:"))
        )
    )

  test("resource entry restriction requires a favorable full recapture chain"):
    def restrictions(beforeFen: String, afterFen: String) =
      StructuralDeltaAnalyzer
        .delta(
          beforeFen = beforeFen,
          beforeBoard = Fen.read(Standard, Fen.Full(beforeFen)).get.board,
          afterFen = afterFen,
          afterBoard = Fen.read(Standard, Fen.Full(afterFen)).get.board,
          side = Color.White,
          files = ('a' to 'h').toList,
          targets = Nil,
          moveUci = Some("b3b4")
        )
        .get
        .opponentMobilityRestrictions

    val favorableResource = restrictions(
      "4k3/8/2p5/8/3P4/1P6/8/4K3 w - - 0 1",
      "4k3/8/2p5/8/1P1P4/8/8/4K3 b - - 0 1"
    )
    val fullyRecaptured = restrictions(
      "3rk3/8/2pp4/8/3P4/1P6/8/3K4 w - - 0 1",
      "3rk3/8/2pp4/8/1P1P4/8/8/3K4 b - - 0 1"
    )
    val purposelessEntry = restrictions(
      "4k3/8/3b4/8/8/1P6/8/4K3 w - - 0 1",
      "4k3/8/3b4/8/1P6/8/8/4K3 b - - 0 1"
    )

    assert(
      favorableResource.exists(_.startsWith("pawn:c6-c5:entry-restricted:by:b3b4:exchange-gain:1")),
      favorableResource
    )
    assert(!fullyRecaptured.exists(_.startsWith("pawn:c6-c5:entry-restricted:")), fullyRecaptured)
    assert(!purposelessEntry.exists(_.startsWith("bishop:d6-c5:entry-restricted:")), purposelessEntry)

  test("records a pawn advance removed by the current move occupying its route"):
    def restrictions(beforeFen: String, afterFen: String, side: Color, moveUci: String) =
      StructuralDeltaAnalyzer
        .delta(
          beforeFen = beforeFen,
          beforeBoard = Fen.read(Standard, Fen.Full(beforeFen)).get.board,
          afterFen = afterFen,
          afterBoard = Fen.read(Standard, Fen.Full(afterFen)).get.board,
          side = side,
          files = ('a' to 'h').toList,
          targets = Nil,
          moveUci = Some(moveUci)
        )
        .get
        .opponentMobilityRestrictions

    val twoStep = restrictions(
      "4k3/3p4/8/8/8/8/8/3RK3 w - - 0 1",
      "4k3/3p4/3R4/8/8/8/8/4K3 b - - 1 1",
      Color.White,
      "d1d6"
    )
    val oneStep = restrictions(
      "4k3/8/8/8/2p5/8/3B4/4K3 w - - 0 1",
      "4k3/8/8/8/2p5/2B5/8/4K3 b - - 1 1",
      Color.White,
      "d2c3"
    )
    val ownPawnAdvance = restrictions(
      "4k3/7p/8/8/7P/8/8/4K3 w - - 0 1",
      "4k3/7p/8/7P/8/8/8/4K3 b - - 0 1",
      Color.White,
      "h4h5"
    )

    assertEquals(twoStep, List("pawn:d7-d5:advance-restricted"))
    assertEquals(oneStep, List("pawn:c4-c3:advance-restricted"))
    assertEquals(ownPawnAdvance, Nil)
    assertEquals(
      StructuralDeltaEvidence.restrictedOpponentEntry("pawn:d7-d5:advance-restricted"),
      Some(("pawn", "d7", "d5"))
    )

  test("records a current move that makes an opponent resource tactically costly"):
    def restrictions(beforeFen: String, afterFen: String, side: Color, moveUci: String) =
      StructuralDeltaAnalyzer
        .delta(
          beforeFen = beforeFen,
          beforeBoard = Fen.read(Standard, Fen.Full(beforeFen)).get.board,
          afterFen = afterFen,
          afterBoard = Fen.read(Standard, Fen.Full(afterFen)).get.board,
          side = side,
          files = ('a' to 'h').toList,
          targets = Nil,
          moveUci = Some(moveUci)
        )
        .get
        .opponentMobilityRestrictions

    val breakRestraint = restrictions(
      "r1bqr1k1/pp1n1ppp/2pb1n2/3p4/3P4/2NBPN2/PPQB1PPP/R4RK1 b - - 3 10",
      "r1b1r1k1/pp1nqppp/2pb1n2/3p4/3P4/2NBPN2/PPQB1PPP/R4RK1 w - - 4 11",
      Color.Black,
      "d8e7"
    )
    val regroupRestraint = restrictions(
      "r3b1k1/1p3pb1/p1n1p1p1/1r2P3/2RPB3/4BN2/P2K1PP1/7R w - - 2 21",
      "r3b1k1/1p3pb1/p1n1p1p1/1r2P1B1/2RPB3/5N2/P2K1PP1/7R b - - 3 21",
      Color.White,
      "e3g5"
    )
    val invasionRestraint = restrictions(
      "q4rk1/1p3ppp/4p3/pQ1nN3/P4P2/4P2P/1P3P2/2R3K1 b - - 1 25",
      "q2r2k1/1p3ppp/4p3/pQ1nN3/P4P2/4P2P/1P3P2/2R3K1 w - - 2 26",
      Color.Black,
      "f8d8"
    )
    val kingCaptureAccess = restrictions(
      "6k1/6pp/8/8/1P6/8/2r3PP/4R2K w - - 0 1",
      "6k1/6pp/8/8/1P6/8/2r3PP/4R1K1 b - - 1 1",
      Color.White,
      "h1g1"
    )

    assert(breakRestraint.exists(_.startsWith("pawn:e3-e4:entry-restricted:by:d8e7:")), breakRestraint)
    assert(regroupRestraint.exists(_.startsWith("knight:c6-e7:entry-restricted:by:e3g5:")), regroupRestraint)
    assert(invasionRestraint.exists(_.startsWith("queen:b5-d7:entry-restricted:by:f8d8:")), invasionRestraint)
    assert(!kingCaptureAccess.exists(_.contains(":entry-restricted:")), kingCaptureAccess)

  test("does not attribute a distant exchange change without root actor participation"):
    val beforeFen = "b1qrr1k1/p6p/P4ppP/Npp1p2n/3n4/2QP1NP1/1PP2PB1/R3R1K1 w - - 2 26"
    val afterFen = "b1qrr1k1/pN5p/P4ppP/1pp1p2n/3n4/2QP1NP1/1PP2PB1/R3R1K1 b - - 1 1"
    val restrictions = StructuralDeltaAnalyzer
      .delta(
        beforeFen = beforeFen,
        beforeBoard = Fen.read(Standard, Fen.Full(beforeFen)).get.board,
        afterFen = afterFen,
        afterBoard = Fen.read(Standard, Fen.Full(afterFen)).get.board,
        side = Color.White,
        files = ('a' to 'h').toList,
        targets = Nil,
        moveUci = Some("a5b7")
      )
      .get
      .opponentMobilityRestrictions

    assert(!restrictions.exists(_.startsWith("pawn:e5-e4:entry-restricted:by:a5b7:")), restrictions)

  test("records a strategically functional opponent knight route denied two moves ahead"):
    def restrictions(beforeFen: String, afterFen: String, side: Color, moveUci: String) =
      StructuralDeltaAnalyzer
        .delta(
          beforeFen = beforeFen,
          beforeBoard = Fen.read(Standard, Fen.Full(beforeFen)).get.board,
          afterFen = afterFen,
          afterBoard = Fen.read(Standard, Fen.Full(afterFen)).get.board,
          side = side,
          files = ('a' to 'h').toList,
          targets = Nil,
          moveUci = Some(moveUci)
        )
        .get
        .opponentMobilityRestrictions

    val tacticalRoute = restrictions(
      "3rq1k1/4rppp/2n3b1/pp2P3/2pP1QB1/P1P1R3/1B4PP/5RK1 w - - 2 25",
      "3rq1k1/4rppp/2n3b1/pp2P3/2pP1Q2/P1P1RB2/1B4PP/5RK1 b - - 3 25",
      Color.White,
      "g4f3"
    )
    val outpostRoute = restrictions(
      "1r2kb1r/1qn2pp1/bpn1p2p/p2pP2P/P2P1N2/2P2NR1/3B1PP1/R2QKB2 w k - 7 22",
      "1r2kb1r/1qn2pp1/bpn1p2p/p2pP2P/P2P1N2/2P2NR1/5PP1/R1BQKB2 b k - 8 22",
      Color.White,
      "d2c1"
    )
    val futurePawnRoute = restrictions(
      "2r3k1/3b1pp1/2p2n2/p2p4/1r5p/1P2P1P1/P3NPBP/R1R3K1 w - - 0 20",
      "2r3k1/3b1pp1/2p2n2/p2p4/1r3N1p/1P2P1P1/P4PBP/R1R3K1 b - - 1 20",
      Color.White,
      "e2f4"
    )

    assert(tacticalRoute.exists(subject =>
      StructuralDeltaEvidence.restrictedOpponentRoute(subject).contains(List("c6", "e7", "d5"))
    ), tacticalRoute)
    assert(outpostRoute.exists(subject =>
      StructuralDeltaEvidence.restrictedOpponentRoute(subject).contains(List("c7", "b5", "a3")) &&
        subject.contains(":goal:c4")
    ), outpostRoute)
    assert(futurePawnRoute.exists(_.startsWith("pawn:h4-h3:entry-restricted:by:e2f4:")), futurePawnRoute)

  test("maps concrete opponent diagonal restriction in a distinct-root comparison"):
    val root = PositionNodeRef("4k3/6b1/8/3p4/3PP3/8/8/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("4k3/6b1/8/3pP3/3P4/8/8/4K3 b - - 0 1", 2, Some(Color.Black), Some("after"))
    val playedLine = LineNodeRef("played-line", "e4e5", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:e4e5:opponent-diagonal-restriction",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val structuralRecord = EvidenceRecord(
      structuralRef,
      StructuralDeltaEvidence(
        transition = StructuralTransitionBinding(
          moveUci = "e4e5",
          role = TransitionEdgeRole.Played,
          from = root,
          to = after,
          line = Some(playedLine),
          perspective = Color.White
        ),
        signals = Nil,
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.OpponentMobilityRestriction,
            StructuralSignalPolarity.Gain,
            strength = 1,
            subjects = List("bishop:g7:diagonal-denial:blocked-by:e5:locked-center:mobility-5-to-3")
          )
        )
      )
    )
    val mechanismRecord = EvidenceRecord(
      EvidenceRef(
        id = "strategic-mechanism:counterplay:e4e5:opponent-diagonal-restriction",
        producer = EvidenceProducer.StrategicMechanismProducer,
        layer = EvidenceLayer.StrategicMechanism,
        position = root,
        line = Some(playedLine),
        scope = EvidenceScope.PlayedTransition,
        confidence = EvidenceConfidence.EngineBacked
      ),
      StrategicMechanismEvidence(
        kind = StrategicMechanismKind.CenterControl,
        signals = List(
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.StructuralDelta,
            label = "opaque-axis",
            source = structuralRef,
            strength = 3,
            axis = Some(
              StrategicAxisDetail(
                StrategicAxisKind.Counterplay,
                StrategicAxisPolarity.Restrain,
                "opaque-axis"
              )
            )
          )
        ),
        semanticAnchors = Nil
      ),
      parents = List(structuralRef)
    )
    val graph = TypedEvidenceGraph(List(structuralRecord, mechanismRecord))
    val signatures = EvidenceObjectBinding.objectSignatures(EvidenceObjectBinding.fromEvidenceRefs(graph, List(structuralRef)))
    val profile = exactPlayedProfile(referenceLine, playedLine, List(structuralRecord, mechanismRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assert(drafts.exists(draft =>
      draft.kind == RelativeCauseKind.OpponentRestriction &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Candidate) &&
        draft.attributionKind == CauseAttributionKind.CandidateCreatesValue
    ))
    assert(signatures.exists(signature => signature.contains("target=Piece:bishop") && signature.contains("target=Square:g7")), signatures)

  test("ignores strategic contrast records from other comparison identities"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val bestLine = LineNodeRef("best-line", "g1f3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val alternativeLine = LineNodeRef("alternative-line", "c2c4", 3, LineNodeRole.Alternative)
    val contrastRef = EvidenceRef(
      id = "strategic-contrast:reference-vs-alternative",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(alternativeLine),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val contrastRecord = EvidenceRecord(
      ref = contrastRef,
      payload = StrategicMechanismContrastEvidence(
        comparisonKind = CandidateComparisonKind.ReferenceVsAlternative,
        referenceLine = bestLine,
        candidateLine = alternativeLine,
        axisComparisons = List(
          StrategicAxisComparison(
            axis = StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, "target-pressure-gain"),
            outcome = StrategicAxisComparisonOutcome.ReferenceOnly,
            referenceStrength = 2,
            candidateStrength = 0,
            referenceSources = Nil,
            candidateSources = Nil
          )
        ),
        planComparison = None,
        sustainability = StrategicSustainabilityAssessment(
          horizon = StrategicSustainabilityHorizon.MediumPv,
          lineMaintained = true,
          pvMaintained = true,
          referencePlyCount = 6,
          candidatePlyCount = 6
        ),
        support = StrategicContrastSupport(Nil, Nil, Nil)
      )
    )
    val profile = RelativeCauseSignalProfile.from(
      fact = CandidateComparisonFact(
        kind = CandidateComparisonKind.PlayedVsBest,
        referenceLine = bestLine,
        candidateLine = playedLine,
        comparison = EvalComparison(
          mover = Color.White,
          referenceLine = bestLine,
          candidateLine = playedLine,
          rawCandidateDeltaCpForDiagnostics = -80,
          candidateWinPercentDeltaForMover = -7.0,
          rawCpLossForDiagnostics = 80,
          winPercentLossForMover = 7.0,
          verdict = MoveChoiceVerdict.Inaccuracy
        )
      ),
      referenceRecords = Nil,
      candidateRecords = Nil,
      sharedRecords = List(contrastRecord)
    )

    assert(!RelativeCauseDraftPlanner.drafts(profile).exists(_.kind == RelativeCauseKind.TargetPressureGain))

  test("does not draft current move pawn break from broad pawn structure delta"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "e2e4", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "e2e4", 1, LineNodeRole.BestReference)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:e2e4:broad-structure",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = "strategic-mechanism:pawn-structure:e2e4",
        producer = EvidenceProducer.StrategicMechanismProducer,
        layer = EvidenceLayer.StrategicMechanism,
        position = root,
        line = Some(playedLine),
        scope = EvidenceScope.PlayedTransition,
        confidence = EvidenceConfidence.EngineBacked
      ),
      payload = StrategicMechanismEvidence(
        kind = StrategicMechanismKind.PawnStructure,
        signals = List(
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.StructuralDelta,
            label = "pawn-structure-delta",
            source = structuralRef,
            strength = 2,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "pawn-structure-delta"))
          )
        ),
        semanticAnchors = Nil
      )
    )
    val profile = exactPlayedProfile(referenceLine, playedLine, List(mechanismRecord))

    assert(!RelativeCauseDraftPlanner.drafts(profile).exists(_.kind == RelativeCauseKind.PawnBreakOpportunity))

  test("pawn break proof signals keep tension gain and resolution polarities separate"):
    val root = PositionNodeRef("8/8/8/3p4/4P3/8/8/8 b - - 0 1", 1, Some(Color.Black), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after"))
    val playedLine = LineNodeRef("played-line", "e4d5", 1, LineNodeRole.Played)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:e4d5:resolved-tension",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val structural = StructuralDeltaEvidence(
      transition = StructuralTransitionBinding(
        moveUci = "e4d5",
        role = TransitionEdgeRole.Played,
        from = root,
        to = after,
        line = Some(playedLine),
        perspective = Color.White
      ),
      signals = Nil,
      consequences = List(
        TransitionConsequence(
          TransitionConsequenceKind.PawnTensionResolution,
          StructuralSignalPolarity.Neutral,
          strength = 2,
          subjects = List("break-file:e", "resolved-tension:e4-d5")
        )
      )
    )
    def payload(axis: StrategicAxisDetail) =
      StrategicMechanismEvidence(
        kind = StrategicMechanismKind.PawnStructure,
        signals = List(
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.StructuralDelta,
            label = axis.label,
            source = structuralRef,
            strength = 2,
            axis = Some(axis)
          )
        ),
        semanticAnchors = Nil
      )
    val graph = TypedEvidenceGraph(List(EvidenceRecord(structuralRef, structural)))
    val supportSignals = RelativeAssessmentAssembler.strategicMechanismProofSignals(
      graph,
      RelativeCauseKind.PawnBreakOpportunity,
      payload(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "opaque-axis")),
      RelativeCauseSourceSide.Candidate,
      selectedStructuralSourceIds = Set(structuralRef.id)
    )
    val mixedReleaseSignals = RelativeAssessmentAssembler.strategicMechanismProofSignals(
      graph,
      RelativeCauseKind.PawnBreakOpportunity,
      payload(
        StrategicAxisDetail(
          StrategicAxisKind.PawnBreak,
          StrategicAxisPolarity.Support,
          "resolved-tension-looking-axis"
        )
      ),
      RelativeCauseSourceSide.Candidate,
      selectedStructuralSourceIds = Set(structuralRef.id)
    )
    val releaseSignals = RelativeAssessmentAssembler.strategicMechanismProofSignals(
      graph,
      RelativeCauseKind.PawnBreakOpportunity,
      payload(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Release, "opaque-axis")),
      RelativeCauseSourceSide.Candidate,
      selectedStructuralSourceIds = Set(structuralRef.id)
    )

    assertEquals(supportSignals, Nil)
    assertEquals(mixedReleaseSignals, Nil)
    assertEquals(releaseSignals.map(_.source.id), List(structuralRef.id))

  test("uses typed target consequences instead of axis wording"):
    val root = PositionNodeRef("8/8/8/8/2p5/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/2p5/8/8/8 b - - 1 1", 2, Some(Color.Black), Some("after"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "d1a4", 2, LineNodeRole.Played)

    def profileFor(
        consequenceKind: TransitionConsequenceKind,
        subject: String,
        axisLabel: String
    ): RelativeCauseSignalProfile =
      val suffix = consequenceKind.toString.toLowerCase
      val structuralRef = EvidenceRef(
        id = s"structural-delta:played:d1a4:$suffix",
        producer = EvidenceProducer.StructuralDeltaProducer,
        layer = EvidenceLayer.StructuralDelta,
        position = root,
        line = Some(candidateLine),
        scope = EvidenceScope.PlayedTransition,
        confidence = EvidenceConfidence.EngineBacked
      )
      val structuralRecord = EvidenceRecord(
        structuralRef,
        StructuralDeltaEvidence(
          transition = StructuralTransitionBinding(
            moveUci = "d1a4",
            role = TransitionEdgeRole.Played,
            from = root,
            to = after,
            line = Some(candidateLine),
            perspective = Color.White
          ),
          signals = Nil,
          consequences = List(
            TransitionConsequence(
              consequenceKind,
              StructuralSignalPolarity.Gain,
              strength = 2,
              subjects = List(subject)
            )
          )
        )
      )
      val mechanismRecord = EvidenceRecord(
        EvidenceRef(
          id = s"strategic-mechanism:target:d1a4:$suffix",
          producer = EvidenceProducer.StrategicMechanismProducer,
          layer = EvidenceLayer.StrategicMechanism,
          position = root,
          line = Some(candidateLine),
          scope = EvidenceScope.PlayedTransition,
          confidence = EvidenceConfidence.EngineBacked
        ),
        StrategicMechanismEvidence(
          kind = StrategicMechanismKind.PawnWeakness,
          signals = List(
            StrategicMechanismSignal(
              kind = StrategicMechanismSignalKind.StructuralDelta,
              label = axisLabel,
              source = structuralRef,
              strength = 2,
              axis = Some(StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, axisLabel))
            )
          ),
          semanticAnchors = Nil
        ),
        parents = List(structuralRef)
      )
      exactPlayedProfile(referenceLine, candidateLine, List(structuralRecord, mechanismRecord))

    val opaqueWeakTarget = RelativeCauseDraftPlanner.drafts(
      profileFor(TransitionConsequenceKind.WeakPawnTargetCreated, "weak-pawn:c4", "opaque-axis")
    )
    val misleadingLabel = RelativeCauseDraftPlanner.drafts(
      profileFor(TransitionConsequenceKind.TargetPressureGain, "pawn:c4", "weak-pawn-target")
    )

    assert(opaqueWeakTarget.exists(_.kind == RelativeCauseKind.PawnWeaknessTarget), opaqueWeakTarget)
    assert(!misleadingLabel.exists(_.kind == RelativeCauseKind.PawnWeaknessTarget), misleadingLabel)

  test("requires a typed king concession instead of counterplay wording"):
    val root = PositionNodeRef("6k1/8/8/8/8/8/8/6K1 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("6k1/8/8/8/8/8/7K/8 b - - 1 1", 2, Some(Color.Black), Some("after"))
    val referenceLine = LineNodeRef("reference-line", "g1f1", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "g1h2", 2, LineNodeRole.Played)

    def profileFor(consequenceKind: TransitionConsequenceKind, axisLabel: String): RelativeCauseSignalProfile =
      val structuralRef = EvidenceRef(
        id = s"structural-delta:played:g1h2:${consequenceKind.toString.toLowerCase}",
        producer = EvidenceProducer.StructuralDeltaProducer,
        layer = EvidenceLayer.StructuralDelta,
        position = root,
        line = Some(candidateLine),
        scope = EvidenceScope.PlayedTransition,
        confidence = EvidenceConfidence.EngineBacked
      )
      val structuralRecord = EvidenceRecord(
        structuralRef,
        StructuralDeltaEvidence(
          transition = StructuralTransitionBinding(
            moveUci = "g1h2",
            role = TransitionEdgeRole.Played,
            from = root,
            to = after,
            line = Some(candidateLine),
            perspective = Color.White
          ),
          signals = Nil,
          consequences = List(
            TransitionConsequence(consequenceKind, StructuralSignalPolarity.Loss, strength = 2, subjects = List("king:g1"))
          )
        )
      )
      val contrastRecord = strategicContrastRecord(
        root = root,
        referenceLine = referenceLine,
        candidateLine = candidateLine,
        comparisons = List(
          strategicAxisComparison(
            StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Concede, axisLabel),
            StrategicAxisComparisonOutcome.CandidateConcession,
            candidateSources = List(structuralRef)
          )
        ),
        planComparison = None
      )
      playedVsBestProfile(referenceLine, candidateLine, List(structuralRecord, contrastRecord))

    val typedConcession = RelativeCauseDraftPlanner.drafts(
      profileFor(TransitionConsequenceKind.KingSafetyConcession, "opaque-axis")
    )
    val misleadingLabel = RelativeCauseDraftPlanner.drafts(
      profileFor(TransitionConsequenceKind.TargetPressureGain, "king-safety")
    )

    assert(typedConcession.exists(_.kind == RelativeCauseKind.KingSafetyConcession), typedConcession)
    assert(!misleadingLabel.exists(_.kind == RelativeCauseKind.KingSafetyConcession), misleadingLabel)

  test("does not duplicate current move support across played-vs-alternative comparisons"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val alternativeLine = LineNodeRef("alternative-line", "g1f3", 1, LineNodeRole.Alternative)
    val playedLine = LineNodeRef("played-line", "d1b3", 1, LineNodeRole.Played)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:d1b3:target-b7",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = "strategic-mechanism:target-pressure:d1b3",
        producer = EvidenceProducer.StrategicMechanismProducer,
        layer = EvidenceLayer.StrategicMechanism,
        position = root,
        line = Some(playedLine),
        scope = EvidenceScope.PlayedTransition,
        confidence = EvidenceConfidence.EngineBacked
      ),
      payload = StrategicMechanismEvidence(
        kind = StrategicMechanismKind.TargetPressure,
        signals = List(
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.StructuralDelta,
            label = "target-pressure-gain",
            source = structuralRef,
            strength = 3,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, "target-pressure-gain"))
          )
        ),
        semanticAnchors = Nil
      )
    )
    val profile = candidateBetterProfile(alternativeLine, playedLine, List(mechanismRecord))

    assert(!RelativeCauseDraftPlanner.drafts(profile).exists(_.kind == RelativeCauseKind.TargetPressureGain))

  test("does not relabel reference target pressure release as target pressure gain"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val contrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Release, "target-pressure-release"),
          StrategicAxisComparisonOutcome.ReferenceOnly
        )
      ),
      planComparison = None
    )
    val profile = playedVsBestProfile(referenceLine, playedLine, List(contrastRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assert(!drafts.exists(_.kind == RelativeCauseKind.TargetPressureGain))

  test("lets current move development support activity value in a distinct-root comparison"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/4N3/3P4/8 b - - 1 1", 2, Some(Color.Black), Some("after"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "f1e3", 2, LineNodeRole.Played)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:f1e3:development-route",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRef = EvidenceRef(
      id = "strategic-mechanism:activity:f1e3:development-route",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val transition = StructuralTransitionBinding(
      moveUci = "f1e3",
      role = TransitionEdgeRole.Played,
      from = root,
      to = after,
      line = Some(candidateLine),
      perspective = Color.White
    )
    val structuralRecord = EvidenceRecord(
      structuralRef,
      StructuralDeltaEvidence(
        transition = transition,
        signals = Nil,
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.DevelopmentPieceActivated,
            StructuralSignalPolarity.Gain,
            strength = 2,
            subjects = List("knight:f1-e3", "knight:f1-e3:center+1")
          )
        )
      )
    )
    val mechanismRecord = EvidenceRecord(
      mechanismRef,
      StrategicMechanismEvidence(
        kind = StrategicMechanismKind.Activity,
        signals = List(
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.StructuralDelta,
            label = "Activity",
            source = structuralRef,
            strength = 2,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Support, "Activity"))
          )
        ),
        semanticAnchors = Nil
      ),
      parents = List(structuralRef)
    )
    val profile = exactPlayedProfile(referenceLine, candidateLine, List(structuralRecord, mechanismRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assert(drafts.exists(draft =>
      draft.kind == RelativeCauseKind.ActivityGain &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Candidate)
    ))

  test("does not let file battery pressure own long diagonal candidate activity value"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/R6Q/8/8/3P4/8 b - - 1 1", 2, Some(Color.Black), Some("after"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "h1h5", 2, LineNodeRole.Alternative)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:h1h5:file-battery",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRef = EvidenceRef(
      id = "strategic-mechanism:activity:h1h5:file-battery",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val structuralRecord = EvidenceRecord(
      structuralRef,
      StructuralDeltaEvidence(
        transition = StructuralTransitionBinding(
          moveUci = "h1h5",
          role = TransitionEdgeRole.Played,
          from = root,
          to = after,
          line = Some(candidateLine),
          perspective = Color.White
        ),
        signals = Nil,
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.BatteryPressureGain,
            StructuralSignalPolarity.Gain,
            strength = 2,
            subjects = List("battery:file:a5-h5:rook-queen")
          )
        )
      )
    )
    val mechanismRecord = EvidenceRecord(
      mechanismRef,
      StrategicMechanismEvidence(
        kind = StrategicMechanismKind.Activity,
        signals = List(
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.StructuralDelta,
            label = "battery-pressure-gain",
            source = structuralRef,
            strength = 3,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "battery-pressure-gain"))
          )
        ),
        semanticAnchors = Nil
      ),
      parents = List(structuralRef)
    )
    val profile = candidateBetterProfile(referenceLine, candidateLine, List(structuralRecord, mechanismRecord))

    assert(!RelativeCauseDraftPlanner.drafts(profile).exists(_.kind == RelativeCauseKind.ActivityGain))

  test("does not borrow diagonal battery proof from another signal in the same activity payload"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/5N2/3P4/8 b - - 1 1", 2, Some(Color.Black), Some("after"))
    val referenceLine = LineNodeRef("reference-line", "d2d4", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "g1f3", 2, LineNodeRole.Alternative)
    val currentStructuralRef = EvidenceRef(
      id = "structural-delta:played:g1f3:mobility",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val staleBatteryRef = EvidenceRef(
      id = "structural-delta:reference:h1h6:diagonal-battery",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRef = EvidenceRef(
      id = "strategic-mechanism:activity:mixed-signals",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val currentStructuralRecord = EvidenceRecord(
      currentStructuralRef,
      StructuralDeltaEvidence(
        transition = StructuralTransitionBinding(
          moveUci = "g1f3",
          role = TransitionEdgeRole.Played,
          from = root,
          to = after,
          line = Some(candidateLine),
          perspective = Color.White
        ),
        signals = Nil,
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.DevelopmentMobilityGain,
            StructuralSignalPolarity.Gain,
            strength = 1,
            subjects = List("knight:g1-f3:mobility+2")
          )
        )
      )
    )
    val staleBatteryRecord = EvidenceRecord(
      staleBatteryRef,
      StructuralDeltaEvidence(
        transition = StructuralTransitionBinding(
          moveUci = "h1h6",
          role = TransitionEdgeRole.Reference,
          from = root,
          to = after,
          line = Some(referenceLine),
          perspective = Color.White
        ),
        signals = Nil,
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.BatteryPressureGain,
            StructuralSignalPolarity.Gain,
            strength = 2,
            subjects = List("battery:diagonal:c1-h6:bishop-queen")
          )
        )
      )
    )
    val mechanismRecord = EvidenceRecord(
      mechanismRef,
      StrategicMechanismEvidence(
        kind = StrategicMechanismKind.Activity,
        signals = List(
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.StructuralDelta,
            label = "activity-gain",
            source = currentStructuralRef,
            strength = 2,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity-gain"))
          ),
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.StructuralDelta,
            label = "battery-pressure-gain",
            source = staleBatteryRef,
            strength = 3,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "battery-pressure-gain"))
          )
        ),
        semanticAnchors = Nil
      ),
      parents = List(currentStructuralRef, staleBatteryRef)
    )
    val profile =
      candidateBetterProfile(referenceLine, candidateLine, List(currentStructuralRecord, staleBatteryRecord, mechanismRecord))

    assert(!RelativeCauseDraftPlanner.drafts(profile).exists(_.kind == RelativeCauseKind.ActivityGain))

  test("keeps counter-break causes for distinct roots and suppresses same-root variants"):
    val root = PositionNodeRef("8/8/8/3p4/3P4/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/3P4/8/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "d4d5", 2, LineNodeRole.Alternative)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:d4d5:tension",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRef = EvidenceRef(
      id = "strategic-mechanism:counter-break:d4d5",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val pawnRef = EvidenceRef(
      id = "pawn-structure:played:d4d5:counter-break",
      producer = EvidenceProducer.PawnStructureProducer,
      layer = EvidenceLayer.PawnStructure,
      position = after,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val structuralRecord = EvidenceRecord(
      structuralRef,
      StructuralDeltaEvidence(
        transition = StructuralTransitionBinding(
          moveUci = "d4d5",
          role = TransitionEdgeRole.Played,
          from = root,
          to = after,
          line = Some(candidateLine),
          perspective = Color.White
        ),
        signals = Nil,
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.PawnTensionResolution,
            StructuralSignalPolarity.Gain,
            strength = 2,
            subjects = List("resolved-tension:d4-c5")
          )
        )
      )
    )
    val mechanismRecord = EvidenceRecord(
      mechanismRef,
      StrategicMechanismEvidence(
        kind = StrategicMechanismKind.PawnStructure,
        signals = List(
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.PawnStructure,
            label = "opaque-axis",
            source = pawnRef,
            strength = 2,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "opaque-axis"))
          )
        ),
        semanticAnchors = Nil
      ),
      parents = List(structuralRef)
    )
    val exactReferenceLine = LineNodeRef("exact-reference-line", "d4d5", 1, LineNodeRole.BestReference)
    val malformedStructuralRecord = structuralRecord.copy(
      ref = structuralRef.copy(id = "structural-delta:played:d4d5:not-a-carrier"),
      payload = structuralRecord.payload.asInstanceOf[StructuralDeltaEvidence].copy(
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.PawnTensionResolution,
            StructuralSignalPolarity.Gain,
            strength = 2,
            subjects = List("not-a-carrier")
          )
        )
      )
    )
    val sharedContrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = exactReferenceLine,
      candidateLine = candidateLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "opaque-axis"),
          StrategicAxisComparisonOutcome.SharedSustained,
          referenceSources = List(pawnRef),
          candidateSources = List(pawnRef)
        )
      ),
      planComparison = None
    )
    val sharedContrastWithSupport = sharedContrastRecord.copy(
      payload = sharedContrastRecord.payload.asInstanceOf[StrategicMechanismContrastEvidence].copy(
        support = StrategicContrastSupport(directSources = List(structuralRef), contrastSources = Nil, contextSources = Nil)
      )
    )
    val profile = candidateBetterProfile(referenceLine, candidateLine, List(structuralRecord, mechanismRecord))
    val exactProfile = exactPlayedProfile(exactReferenceLine, candidateLine, Nil, List(structuralRecord, mechanismRecord))
    val exactContrastProfile = exactPlayedProfile(exactReferenceLine, candidateLine, Nil, List(structuralRecord, sharedContrastRecord))
    val exactContrastRefProfile = exactPlayedProfile(exactReferenceLine, candidateLine, Nil, List(sharedContrastWithSupport))
    val malformedExactProfile = exactPlayedProfile(exactReferenceLine, candidateLine, Nil, List(malformedStructuralRecord, sharedContrastRecord))

    assert(RelativeCauseDraftPlanner.drafts(profile).exists(_.kind == RelativeCauseKind.OpponentRestriction))
    List(
      candidateBetterProfile(referenceLine, candidateLine, List(mechanismRecord)),
      exactProfile,
      exactContrastProfile,
      exactContrastRefProfile,
      malformedExactProfile
    ).foreach(profile =>
      assert(!RelativeCauseDraftPlanner.drafts(profile).exists(_.kind == RelativeCauseKind.OpponentRestriction))
    )

  test("does not turn an exact same-root activity carrier into a comparative cause"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/5B2 b - - 0 1", 1, Some(Color.Black), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/8/3Pb3/8 w - - 1 2", 2, Some(Color.White), Some("after"))
    val referenceLine = LineNodeRef("reference-line", "f8e7", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "f8e7", 2, LineNodeRole.Played)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:f8e7:development-route",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val structuralRecord = EvidenceRecord(
      structuralRef,
      StructuralDeltaEvidence(
        transition = StructuralTransitionBinding(
          moveUci = "f8e7",
          role = TransitionEdgeRole.Played,
          from = root,
          to = after,
          line = Some(candidateLine),
          perspective = Color.Black
        ),
        signals = Nil,
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.DevelopmentPieceActivated,
            StructuralSignalPolarity.Gain,
            strength = 2,
            subjects = List("bishop:f8-e7", "bishop:f8-e7:mobility+1")
          )
        )
      )
    )
    val sharedContrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Support, "Activity"),
          StrategicAxisComparisonOutcome.SharedSustained,
          referenceSources = List(structuralRef),
          candidateSources = List(structuralRef)
        )
      ),
      planComparison = None
    )
    val malformedStructuralRecord = structuralRecord.copy(
      ref = structuralRef.copy(id = "structural-delta:played:f8e7:generic-mobility"),
      payload = structuralRecord.payload.asInstanceOf[StructuralDeltaEvidence].copy(
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.DevelopmentMobilityGain,
            StructuralSignalPolarity.Gain,
            strength = 2,
            subjects = List("mobility+1")
          )
        )
      )
    )
    val exactContrastProfile = exactPlayedProfile(referenceLine, candidateLine, Nil, List(structuralRecord, sharedContrastRecord))
    val malformedProfile = exactPlayedProfile(referenceLine, candidateLine, Nil, List(malformedStructuralRecord, sharedContrastRecord))

    List(exactContrastProfile, malformedProfile).foreach(profile =>
      assert(!RelativeCauseDraftPlanner.drafts(profile).exists(_.kind == RelativeCauseKind.ActivityGain))
    )

  test("maps candidate positive counterplay restraint axis to opponent restriction"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "h2h3", 2, LineNodeRole.Alternative)
    val restraintSource = EvidenceRef(
      id = "strategic-mechanism:counterplay-restraint:candidate",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val contrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "opponent-low-mobility"),
          StrategicAxisComparisonOutcome.CandidateOnly,
          candidateSources = List(restraintSource)
        )
      ),
      planComparison = None,
      comparisonKind = CandidateComparisonKind.PlayedVsAlternative
    )
    val profile = candidateBetterProfile(referenceLine, candidateLine, List(contrastRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assert(drafts.exists(draft =>
      draft.kind == RelativeCauseKind.OpponentRestriction &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Candidate)
    ))

  test("uses plan comparison as the single source for plan contradiction"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val contrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(
            StrategicAxisKind.PlanCoherence,
            StrategicAxisPolarity.Support,
            "OpeningDevelopment,PieceActivation"
          ),
          StrategicAxisComparisonOutcome.ReferenceOnly
        )
      ),
      planComparison = Some(
        StrategicPlanComparison(
          referencePlanIds = List("OpeningDevelopment", "PieceActivation"),
          candidatePlanIds = Nil,
          outcome = StrategicAxisComparisonOutcome.ReferenceOnly
        )
      )
    )
    val profile = playedVsBestProfile(referenceLine, playedLine, List(contrastRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assert(drafts.exists(draft =>
      draft.kind == RelativeCauseKind.PlanContradiction &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Candidate)
    ))

  test("prefers pawn break opportunity root over plan contradiction when pawn break axis is concrete"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "c2c4", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "g1f3", 2, LineNodeRole.Played)
    val contrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "BreakReady"),
          StrategicAxisComparisonOutcome.ReferenceOnly
        ),
        strategicAxisComparison(
          StrategicAxisDetail(
            StrategicAxisKind.PlanCoherence,
            StrategicAxisPolarity.Support,
            "PawnBreakPreparation,PieceActivation"
          ),
          StrategicAxisComparisonOutcome.ReferenceOnly
        )
      ),
      planComparison = Some(
        StrategicPlanComparison(
          referencePlanIds = List("PawnBreakPreparation", "PieceActivation"),
          candidatePlanIds = Nil,
          outcome = StrategicAxisComparisonOutcome.ReferenceOnly
        )
      )
    )
    val profile = playedVsBestProfile(referenceLine, playedLine, List(contrastRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assert(drafts.exists(draft =>
      draft.kind == RelativeCauseKind.PawnBreakOpportunity &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Reference)
    ))

  test("does not promote a proof-flagged forced theme to king forcing"):
    val root = PositionNodeRef("6k1/7p/8/6N1/8/3B4/8/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "d3h7", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "e1f1", 2, LineNodeRole.Played)
    val lineRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = "line:geometry-only-forced-theme",
        producer = EvidenceProducer.LegalLineProducer,
        layer = EvidenceLayer.Line,
        position = root,
        line = Some(referenceLine),
        scope = referenceLine.role.scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      payload = new LineFactEvidence(referenceLine, Some(referenceLine.rootMove), None, Nil)(
        consequences = List(
          LineConsequence(
            kind = LineConsequenceKind.ForcedTheme,
            lineMoves = Nil,
            proofSignal = true,
            rootMove = Some(referenceLine.rootMove)
          )
        )
      )
    )

    assertEquals(RelativeCauseSignalProfile.forcingLineResourceRecords(List(lineRecord)), Nil)
    assert(!RelativeCauseDraftPlanner.drafts(playedVsBestProfile(referenceLine, playedLine, List(lineRecord))).exists(
      _.kind == RelativeCauseKind.KingForcing
    ))

  test("candidate-only adverse promotion creates liability and reference parity suppresses it"):
    val root = PositionNodeRef("8/8/8/8/8/8/6p1/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "e1f1", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "e1d1", 2, LineNodeRole.Played)
    def promotionRecord(id: String, line: LineNodeRef, kind: LineConsequenceKind): EvidenceRecord =
      EvidenceRecord(
        ref = EvidenceRef(
          id = id,
          producer = EvidenceProducer.LegalLineProducer,
          layer = EvidenceLayer.Line,
          position = root,
          line = Some(line),
          scope = line.role.scope,
          confidence = EvidenceConfidence.LegalReplayVerified
        ),
        payload = new LineFactEvidence(line, Some(line.rootMove), None, Nil)(
          consequences = List(LineConsequence(
            kind = kind,
            lineMoves = List(line.rootMove, "g2g1q"),
            proofSignal = true,
            eventMove = Some("g2g1q"),
            rootMove = None,
            rootSide = Some(Color.White),
            beneficiary = Some(Color.Black)
          ))
        )
      )
    def lossProfile(
        referenceRecords: List[EvidenceRecord],
        candidateRecords: List[EvidenceRecord]
    ): RelativeCauseSignalProfile =
      RelativeCauseSignalProfile.from(
        fact = CandidateComparisonFact(
          kind = CandidateComparisonKind.PlayedVsBest,
          referenceLine = referenceLine,
          candidateLine = playedLine,
          comparison = EvalComparison(
            mover = Color.White,
            referenceLine = referenceLine,
            candidateLine = playedLine,
            rawCandidateDeltaCpForDiagnostics = -100,
            candidateWinPercentDeltaForMover = -6.0,
            rawCpLossForDiagnostics = 100,
            winPercentLossForMover = 6.0,
            verdict = MoveChoiceVerdict.Mistake
          )
        ),
        referenceRecords = referenceRecords,
        candidateRecords = candidateRecords,
        sharedRecords = Nil
      )

    List(LineConsequenceKind.Promotion, LineConsequenceKind.PromotionRace).foreach { kind =>
      val candidateRecord = promotionRecord(s"candidate-${kind.toString}", playedLine, kind)
      val referenceRecord = promotionRecord(s"reference-${kind.toString}", referenceLine, kind)
      val liability = RelativeCauseDraftPlanner
        .drafts(lossProfile(Nil, List(candidateRecord)))
        .find(draft =>
          draft.kind == RelativeCauseKind.TacticalRefutationOfPlayed &&
            draft.attributionKind == CauseAttributionKind.CandidateAllowsLiability
        )
        .getOrElse(fail(s"expected candidate promotion liability for $kind"))

      assertEquals(liability.sourceSide, Some(RelativeCauseSourceSide.Candidate))
      assertEquals(liability.support.map(_.ref.id), List(candidateRecord.ref.id))
      assert(!RelativeCauseDraftPlanner
        .drafts(lossProfile(List(referenceRecord), List(candidateRecord)))
        .exists(draft =>
          draft.kind == RelativeCauseKind.TacticalRefutationOfPlayed &&
            draft.attributionKind == CauseAttributionKind.CandidateAllowsLiability
        ))
    }

  private def playedVsBestProfile(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): RelativeCauseSignalProfile =
    RelativeCauseSignalProfile.from(
      fact = CandidateComparisonFact(
        kind = CandidateComparisonKind.PlayedVsBest,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        comparison = EvalComparison(
          mover = Color.White,
          referenceLine = referenceLine,
          candidateLine = playedLine,
          rawCandidateDeltaCpForDiagnostics = -35,
          candidateWinPercentDeltaForMover = -3.5,
          rawCpLossForDiagnostics = 35,
          winPercentLossForMover = 3.5,
          verdict = MoveChoiceVerdict.Inaccuracy
        )
      ),
      referenceRecords = records,
      candidateRecords = Nil,
      sharedRecords = Nil
    )

  private def candidateBetterProfile(
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): RelativeCauseSignalProfile =
    RelativeCauseSignalProfile.from(
      fact = CandidateComparisonFact(
        kind = CandidateComparisonKind.PlayedVsAlternative,
        referenceLine = referenceLine,
        candidateLine = candidateLine,
        comparison = EvalComparison(
          mover = Color.White,
          referenceLine = referenceLine,
          candidateLine = candidateLine,
          rawCandidateDeltaCpForDiagnostics = 35,
          candidateWinPercentDeltaForMover = 3.5,
          rawCpLossForDiagnostics = -35,
          winPercentLossForMover = -3.5,
          verdict = MoveChoiceVerdict.ImprovesOnReference
        )
      ),
      referenceRecords = Nil,
      candidateRecords = records,
      sharedRecords = Nil
    )

  private def exactPlayedProfile(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      candidateRecords: List[EvidenceRecord],
      sharedRecords: List[EvidenceRecord] = Nil
  ): RelativeCauseSignalProfile =
    RelativeCauseSignalProfile.from(
      fact = CandidateComparisonFact(
        kind = CandidateComparisonKind.PlayedVsBest,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        comparison = EvalComparison(
          mover = Color.White,
          referenceLine = referenceLine,
          candidateLine = playedLine,
          rawCandidateDeltaCpForDiagnostics = 0,
          candidateWinPercentDeltaForMover = 0.0,
          rawCpLossForDiagnostics = 0,
          winPercentLossForMover = 0.0,
          verdict = MoveChoiceVerdict.MatchesReference
        )
      ),
      referenceRecords = Nil,
      candidateRecords = candidateRecords,
      sharedRecords = sharedRecords
    )

  private def strategicContrastRecord(
      root: PositionNodeRef,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      comparisons: List[StrategicAxisComparison],
      planComparison: Option[StrategicPlanComparison],
      comparisonKind: CandidateComparisonKind = CandidateComparisonKind.PlayedVsBest
  ): EvidenceRecord =
    EvidenceRecord(
      ref = EvidenceRef(
        id = "strategic-contrast:played-vs-best",
        producer = EvidenceProducer.StrategicMechanismProducer,
        layer = EvidenceLayer.StrategicMechanism,
        position = root,
        line = Some(candidateLine),
        scope = EvidenceScope.Counterfactual,
        confidence = EvidenceConfidence.EngineBacked
      ),
      payload = StrategicMechanismContrastEvidence(
        comparisonKind = comparisonKind,
        referenceLine = referenceLine,
        candidateLine = candidateLine,
        axisComparisons = comparisons,
        planComparison = planComparison,
        sustainability = StrategicSustainabilityAssessment(
          horizon = StrategicSustainabilityHorizon.MediumPv,
          lineMaintained = true,
          pvMaintained = true,
          referencePlyCount = 6,
          candidatePlyCount = 6
        ),
        support = StrategicContrastSupport(Nil, Nil, Nil)
      )
    )

  private def strategicAxisComparison(
      axis: StrategicAxisDetail,
      outcome: StrategicAxisComparisonOutcome,
      referenceSources: List[EvidenceRef] = Nil,
      candidateSources: List[EvidenceRef] = Nil
  ): StrategicAxisComparison =
    StrategicAxisComparison(
      axis = axis,
      outcome = outcome,
      referenceStrength = if referenceSources.nonEmpty || outcome == StrategicAxisComparisonOutcome.ReferenceOnly then 2 else 0,
      candidateStrength = if candidateSources.nonEmpty || outcome == StrategicAxisComparisonOutcome.CandidateConcession then 2 else 0,
      referenceSources = referenceSources,
      candidateSources = candidateSources
    )
