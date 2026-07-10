package lila.chessjudgment.analysis.qc

import lila.chessjudgment.analysis.line.{ LineFactNormalizer, PrincipalVariationEvidence }
import lila.chessjudgment.analysis.position.{ FactExtractor, PositionAnalyzer, PositionFactNormalizer }
import lila.chessjudgment.analysis.singlePosition.*
import lila.chessjudgment.analysis.strategic.EndgamePatternOracle
import lila.chessjudgment.model.{ Fact, FactScope, Motif }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.{ RookEndgameGeometry, RookEndgamePattern }
import lila.chessjudgment.model.structure.*
import chess.{ Color, File }
import chess.format.Fen
import chess.variant.Standard
class MoveReviewPhase3AuditRunnerTest extends munit.FunSuite:

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

  test("semantic rubric measures terminal proof questions through explicit carrier slots"):
    def terminalSlot(
        id: String,
        questionId: String,
        stage: String
    ) =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = id,
        unit = PositionPlanTechniqueUnit.StructuralTransformation,
        questionId = Some(questionId),
        requiredSupportLevel = Some(stage)
      )
    def terminalDiagnostic(
        id: String,
        move: String,
        consequence: String,
        objectSignature: String,
        support: String,
        lane: String,
        causeKind: Option[RelativeCauseKind] = None,
        causeId: String = "none"
    ) =
      val detailTokens =
        List(
          "unit:StructuralTransformation",
          s"terminalConsequenceKind:$consequence",
          "proofRole:DirectProof",
          s"structuralConsequence:$consequence",
          "structuralCategory:TerminalProof"
        ) ++
          Option.when(causeId != "none")(s"causeEvidenceId:$causeId").toList ++
          objectSignature.split("\\|").toList.flatMap {
            case token if token.startsWith("actor=")       => List(s"objectActor:${token.stripPrefix("actor=")}")
            case token if token.startsWith("target=")      => List(s"objectTarget:${token.stripPrefix("target=")}")
            case token if token.startsWith("mechanism=")   => List(s"objectMechanism:${token.stripPrefix("mechanism=")}")
            case token if token.startsWith("consequence=") => List(s"objectConsequence:${token.stripPrefix("consequence=")}")
            case token if token.startsWith("witness=")     => List(s"objectWitness:${token.stripPrefix("witness=")}")
            case _                                         => Nil
          }
      val causes = causeKind.toList
      val flows =
        causeKind
          .filter(_ => causeId != "none")
          .map(kind => causeFlow(causeId, kind, proofAxisKeys = Nil, claimIds = List(s"claim-$id")))
          .toList
      comparisonDiagnostic(
        id = id,
        referenceLeadAxes = Nil,
        producedKinds = causes,
        flows = flows,
        primaryRootKinds = causes,
        primaryRootIds = if causes.nonEmpty then List(causeId) else Nil,
        positionPlanTechniqueFrameIds = List(s"frame-$id"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.StructuralTransformation),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.StructuralTransformation),
        positionPlanTechniqueSemanticDetailTokens = detailTokens,
        positionPlanTechniqueSemanticDetailTokenGroups = List(detailTokens),
        positionPlanTechniqueObjectBindingSignatures = List(objectSignature),
        positionPlanTechniqueRelativeCauseEvidenceIds = if causes.nonEmpty then List(causeId) else Nil,
        publicMoveMeaningClaimDiagnostics =
          List(
            publicMoveMeaningClaimDiagnostic(
              unit = PositionPlanTechniqueUnit.StructuralTransformation,
              meaningKind = "TerminalProof",
              supportLevel = support,
              surfaceLane = lane,
              lineRole = "candidate",
              moveUci = move,
              causeEvidenceIds = if causeId == "none" then Nil else List(causeId),
              sourceEvidenceIds = List(s"line:$id"),
              proofLevel = "DirectProof"
            )
          )
      )

    val mateSlot =
      terminalSlot(
        id = "terminal-mate-view",
        questionId = "terminal_mate",
        stage = "view_surfaced"
      )
    val promotionRaceSlot =
      terminalSlot(
        id = "terminal-promotion-race-owned",
        questionId = "terminal_promotion_race",
        stage = "owned_cause_linked"
      )
    val coverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        expectedSlots = List(mateSlot, promotionRaceSlot),
        diagnostics = List(
          terminalDiagnostic(
            id = "terminal-mate",
            move = "g6g7",
            consequence = "Mate",
            objectSignature = "actor=Move:g6g7|target=Square:h7|mechanism=Mechanism:Mate|consequence=Consequence:Mate|proof=DirectProof",
            support = "view_surfaced",
            lane = "current_move_function"
          ),
          terminalDiagnostic(
            id = "terminal-promotion-race",
            move = "a7a8q",
            consequence = "PromotionRace",
            objectSignature =
              "actor=Move:a7a8q|target=Square:a8|mechanism=Mechanism:promotionrace|consequence=Consequence:promotionrace|proof=DirectProof",
            support = "owned_cause_linked",
            lane = "current_move_owned",
            causeKind = Some(RelativeCauseKind.ConversionSecured),
            causeId = "cause-promotion-race"
          )
        ),
        expectedQuestionIds = List("terminal_mate", "terminal_promotion_race")
      )
    val corpusCoverage = MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCorpusCoverageJson(List(coverage))

    assertEquals((coverage \ "matchedSlotCount").as[Int], 2)
    assertEquals((coverage \ "measuredExpectedQuestionIds").as[List[String]], List("terminal_mate", "terminal_promotion_race"))
    assertEquals((coverage \ "unmeasuredExpectedQuestionIds").as[List[String]], Nil)
    assertEquals((coverage \ "coveredExpectedQuestionIds").as[List[String]], List("terminal_mate", "terminal_promotion_race"))
    assertEquals((coverage \ "expectedQuestionCoverageComplete").as[Boolean], true)
    assertEquals((coverage \ "byQuestionId" \ "terminal_mate" \ "supportLevelCounts" \ "view_surfaced").as[Int], 1)
    assertEquals((coverage \ "byQuestionId" \ "terminal_promotion_race" \ "supportLevelCounts" \ "owned_cause_linked").as[Int], 1)
    assertEquals((corpusCoverage \ "expectedQuestionCoverageComplete").as[Boolean], true)

  test("comparison diagnostics keep contrast plan technique frames on their owning comparison"):
    val root = PositionNodeRef("8/8/8/8/8/8/4P3/4K3 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val referenceLineA = LineNodeRef("reference-line-a", "e2e4", 1, LineNodeRole.BestReference)
    val referenceLineB = LineNodeRef("reference-line-b", "d2d4", 1, LineNodeRole.BestReference)
    val sharedCandidateLine = LineNodeRef("played-line", "g1f3", 2, LineNodeRole.Played)
    def comparison(referenceLine: LineNodeRef) =
      CandidateComparisonFact(
        kind = CandidateComparisonKind.PlayedVsBest,
        referenceLine = referenceLine,
        candidateLine = sharedCandidateLine,
        comparison = EvalComparison(
          mover = chess.Color.White,
          referenceLine = referenceLine,
          candidateLine = sharedCandidateLine,
          rawCandidateDeltaCpForDiagnostics = -90,
          candidateWinPercentDeltaForMover = -9.0,
          rawCpLossForDiagnostics = 90,
          winPercentLossForMover = 9.0,
          verdict = MoveChoiceVerdict.Inaccuracy
        )
      )
    val comparisonA = comparison(referenceLineA)
    val comparisonB = comparison(referenceLineB)
    def comparisonRef(id: String, fact: CandidateComparisonFact) =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.RelativeMoveProducer,
        layer = EvidenceLayer.CandidateComparison,
        position = root,
        line = Some(fact.candidateLine),
        scope = EvidenceScope.Counterfactual,
        confidence = EvidenceConfidence.EngineBacked
      )
    val contrastRef =
      EvidenceRef(
        id = "strategic-contrast:central-break-owner",
        producer = EvidenceProducer.StrategicMechanismProducer,
        layer = EvidenceLayer.StrategicMechanism,
        position = root,
        line = Some(sharedCandidateLine),
        scope = EvidenceScope.Counterfactual,
        confidence = EvidenceConfidence.Mixed
      )
    val axis = StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "central-break-timing")
    val contrast =
      StrategicMechanismContrastEvidence(
        comparisonKind = CandidateComparisonKind.PlayedVsBest,
        referenceLine = referenceLineA,
        candidateLine = sharedCandidateLine,
        axisComparisons = List(
          StrategicAxisComparison(
            axis = axis,
            outcome = StrategicAxisComparisonOutcome.ReferenceStronger,
            referenceStrength = 2,
            candidateStrength = 0,
            referenceSources = Nil,
            candidateSources = Nil
          )
        ),
        planComparison = None,
        sustainability = StrategicSustainabilityAssessment(
          horizon = StrategicSustainabilityHorizon.ShortPv,
          lineMaintained = true,
          pvMaintained = true,
          referencePlyCount = 4,
          candidatePlyCount = 4
        ),
        support = StrategicContrastSupport(Nil, Nil, Nil)
      )
    val contrastFrame =
      PositionPlanTechniqueFrame(
        id = "position-plan-technique:strategic-contrast:central-break-owner",
        units = List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute),
        position = root,
        line = Some(sharedCandidateLine),
        moveUci = Some("g1f3"),
        scope = EvidenceScope.Counterfactual,
        mechanismKinds = Nil,
        strategicAxisKeys = List(axis.stableKey),
        semanticAnchors = Nil,
        objectBindingSignatures = Nil,
        semanticDetails = List(
          PositionPlanTechniqueSemanticDetail(
            unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
            axisKey = Some(axis.stableKey),
            axisKind = Some(axis.kind),
            axisPolarity = Some(axis.polarity),
            label = Some(axis.label),
            contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceStronger),
            referenceStrength = Some(2),
            candidateStrength = Some(0),
            sourceEvidenceIds = List(contrastRef.id)
          )
        ),
        evidenceIds = List(contrastRef.id),
        mechanismEvidenceIds = List(contrastRef.id),
        sourceEvidenceIds = Nil,
        relativeCauseEvidenceIds = Nil,
        ideaIds = Nil,
        claimIds = Nil,
        planComparison = None,
        relationToVerdict = None,
        confidence = EvidenceConfidence.Mixed,
        salience = 3
      )
    val packet =
      EvidenceBackedJudgmentPacket(
        root = root,
        positions = List(PositionNode(PositionNodeRole.Before, root, facts = Nil, features = None, assessment = None, evidence = Nil)),
        candidateLines = Nil,
        transitions = Nil,
        relativeAssessments = Nil,
        evidenceGraph = TypedEvidenceGraph(
          List(
            EvidenceRecord(comparisonRef("candidate-comparison:owner", comparisonA), CandidateComparisonEvidence(comparisonA)),
            EvidenceRecord(comparisonRef("candidate-comparison:shared-candidate-other-reference", comparisonB), CandidateComparisonEvidence(comparisonB)),
            EvidenceRecord(contrastRef, contrast)
          )
        ),
        ideas = Nil,
        claims = Nil,
        ideaVerdict = None,
        moveJudgmentView = Some(
          MoveJudgmentView(
            verdict = None,
            verdictCarriers = Nil,
            causeAudit = MoveJudgmentCauseAudit(),
            positionPlanTechniqueFrames = List(contrastFrame),
            supportContextClusterIds = Nil,
            overriddenLocalIdeas = Nil,
            preservedLocalIdeas = Nil
          )
        )
      )

    val diagnostics = CandidateComparisonDiagnostic.fromPacket(packet).map(diagnostic => diagnostic.id -> diagnostic).toMap

    assertEquals(
      diagnostics("candidate-comparison:owner").moveJudgmentView.positionPlanTechniqueFrameIds,
      List(contrastFrame.id)
    )
    assertEquals(
      diagnostics("candidate-comparison:shared-candidate-other-reference").moveJudgmentView.positionPlanTechniqueFrameIds,
      Nil
    )

  test("expected semantic slots match explicit rook endgame technique probes outside structural funnel"):
    def endgameDiagnostic(
        id: String,
        causeId: String,
        causeKind: RelativeCauseKind,
        anchors: List[String]
    ): CandidateComparisonDiagnostic =
      val detailTokens =
        List(
          "unit:EndgameTechniqueRecipe",
          "boardAnchorKind:EndgameTechnique",
          "boardAnchorSignal:EndgameRookPattern",
          s"causeEvidenceId:$causeId"
        ) ++ anchors.map(anchor => s"semanticAnchor:$anchor")
      comparisonDiagnostic(
        id = id,
        referenceLeadAxes = Nil,
        producedKinds = List(causeKind),
        flows = List(causeFlow(causeId, causeKind, proofAxisKeys = Nil, claimIds = List(s"claim-$id"))),
        primaryRootKinds = List(causeKind),
        primaryRootIds = List(causeId),
        positionPlanTechniqueFrameIds = List(s"frame-$id"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.EndgameTechniqueRecipe),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.EndgameTechniqueRecipe),
        positionPlanTechniqueSemanticDetailMechanismKinds = List(StrategicMechanismKind.Endgame),
        positionPlanTechniqueSemanticDetailAnchorKeys = anchors,
        positionPlanTechniqueSemanticDetailTokens = detailTokens,
        positionPlanTechniqueSemanticDetailTokenGroups = List(detailTokens),
        positionPlanTechniqueObjectBindingSignatures =
          List("target=Square:d8|mechanism=Mechanism:EndgameTechnique|proof=DirectProof"),
        positionPlanTechniqueRelativeCauseEvidenceIds = List(causeId),
        publicMoveMeaningClaimDiagnostics =
          List(
            publicMoveMeaningClaimDiagnostic(
              unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
              meaningKind = "RookEndgameTechnique",
              supportLevel = "owned_cause_linked",
              surfaceLane = "current_move_owned",
              lineRole = "candidate",
              moveUci = candidateLine.rootMove,
              causeEvidenceIds = List(causeId)
            )
          ),
        primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot)
      )

    val lucenaSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "rook-endgame-lucena-recognition",
        unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
        questionId = Some("rook_endgame.conversion_recipe"),
        requiredSupportLevel = Some("owned_cause_linked")
      )
    val philidorSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "rook-endgame-philidor-recognition",
        unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
        questionId = Some("rook_endgame.draw_resource"),
        requiredSupportLevel = Some("owned_cause_linked")
      )

    val coverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(lucenaSlot, philidorSlot),
        List(
          endgameDiagnostic(
            "cmp-lucena",
            "cause-lucena",
            RelativeCauseKind.ConversionSecured,
            List("pattern:Lucena", "rook-pattern:RookBehindPassedPawn")
          ),
          endgameDiagnostic(
            "cmp-philidor",
            "cause-philidor",
            RelativeCauseKind.DrawResource,
            List("pattern:PhilidorDefense", "rook-pattern:KingCutOff")
          )
        )
      )

    assertEquals((coverage \ "matchedSlotCount").as[Int], 2)
    assertEquals((coverage \ "missingSlotIds").as[List[String]], Nil)

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
        rookPattern = "RookBehindPassedPawn",
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

  test("terminal proof overrides rook technique cause ownership"):
    val line = lineRef("mate-line", "d7d8q", 1, LineNodeRole.BestReference)
    val mate = LineConsequence(LineConsequenceKind.Mate, List("d7d8q", "e8e7", "d8d1"), proofSignal = true, eventMove = Some("d7d8q"))
    val lucenaOverride =
      LineEndgameTechniqueHorizon(
        pattern = "Lucena",
        rookPattern = "RookBehindPassedPawn",
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
        rookPattern = "KingCutOff",
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
    assert(payload.rootOwnedEndgameTechniqueHorizons("d7d8q", RelativeCauseKind.ConversionSecured).isEmpty)
    assert(payload.rootOwnedEndgameTechniqueHorizons("d7d8q", RelativeCauseKind.DrawResource).isEmpty)

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
    assert(payload.rootOwnedEndgameTechniqueHorizons("f7f8q", RelativeCauseKind.ConversionMiss).nonEmpty)

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
    assert(payload.rootOwnedEndgameTechniqueHorizons("e4d4", RelativeCauseKind.ConversionMiss).isEmpty)
    assert(payload.rootOwnedEndgameTechniqueHorizons("d4d3", RelativeCauseKind.ConversionMiss).nonEmpty)

  test("line normalizer marks Vancura transition as draw resource proof"):
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

    assertEquals(vancura.rookPattern, RookEndgamePattern.RookBehindPassedPawn.toString)
    assertEquals(vancura.status, LineEndgameTechniqueHorizonStatus.Transitioned)
    assertEquals(vancura.triggerMove, Some("a3g3"))
    assert(vancura.requiredSquares.nonEmpty)
    assert(vancura.maintainedSquares.nonEmpty)
    assertEquals(vancura.brokenSquares, Nil)
    assert(payload.rootOwnedEndgameTechniqueHorizons("a3g3", RelativeCauseKind.DrawResource).nonEmpty)
    assert(payload.proofSignalConsequencesOf(LineConsequenceKind.DrawResource).exists(_.rootMoveMatched("a3g3")))
    assertEquals(detail.endgameTechniqueRookPattern, Some(RookEndgamePattern.RookBehindPassedPawn.toString))
    assertEquals(detail.endgameTechniqueHorizonStatus, Some(LineEndgameTechniqueHorizonStatus.Transitioned.toString))
    assertEquals(detail.endgameTechniqueTriggerMove, Some("a3g3"))
    assert(detail.requiredSquares.nonEmpty)

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
    val promotionProof = payload.proofSignalConsequencesOf(LineConsequenceKind.PromotionRace).head
    val materialProof = payload.proofSignalConsequencesOf(LineConsequenceKind.MaterialGain).head

    assertEquals(lucena.status, LineEndgameTechniqueHorizonStatus.SupersededByTactic)
    assert(promotionProof.rootMoveMatched("f7f8q"), promotionProof)
    assert(materialProof.rootMoveMatched("f7f8q"), materialProof)
    assert(payload.rootOwnedEndgameTechniqueHorizons("f7f8q", RelativeCauseKind.ConversionSecured).isEmpty)

  test("semantic rubric does not treat a frame id alone as object-bound"):
    val viewOnlyContext =
      comparisonDiagnostic(
        id = "cmp-view-only-context",
        referenceLeadAxes = Nil,
        producedKinds = Nil,
        flows = Nil,
        primaryRootKinds = Nil,
        primaryRootIds = Nil,
        positionPlanTechniqueFrameIds = List("frame-root-context"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.PlanOptionSet)
      )
    val slot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "view-only-plan-context",
        unit = PositionPlanTechniqueUnit.PlanOptionSet
      )

    val coverage = MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(slot), List(viewOnlyContext))

    assertEquals((coverage \ "slots" \ 0 \ "matched").as[Boolean], false)
    assertEquals((coverage \ "slots" \ 0 \ "supportLevel").as[String], "missing_semantic_slot")

  test("recognition view slots require semantic details to reach public surface"):
    val detailTokens =
      List(
        "unit:EndgameTechniqueRecipe",
        "pattern:Lucena",
        "rook-pattern:RookBehindPassedPawn",
        "requiredSquare:f8"
      )
    val diagnostic =
      comparisonDiagnostic(
        id = "cmp-rook-recognition-view",
        referenceLeadAxes = Nil,
        producedKinds = Nil,
        flows = Nil,
        primaryRootKinds = Nil,
        primaryRootIds = Nil,
        positionPlanTechniqueFrameIds = List("frame-rook-recognition-view"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.EndgameTechniqueRecipe),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.EndgameTechniqueRecipe),
        positionPlanTechniqueSemanticDetailTokens = detailTokens,
        positionPlanTechniqueSemanticDetailTokenGroups = List(detailTokens),
        publicMoveMeaningClaimDiagnostics =
          List(
            publicMoveMeaningClaimDiagnostic(
              unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
              meaningKind = "RookEndgameTechnique",
              supportLevel = "view_surfaced",
              surfaceLane = "current_move_function",
              lineRole = "candidate",
              moveUci = candidateLine.rootMove,
              sourceEvidenceIds = List("line:rook-horizon"),
              proofLevel = "DirectProof"
            )
          )
      )
    val detailOnlyDiagnostic =
      diagnostic.copy(
        id = "cmp-rook-recognition-detail-only",
        moveJudgmentView = diagnostic.moveJudgmentView.copy(
          publicMoveMeaningClaimDiagnostics = Nil
        )
      )
    val recognitionSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "rook-recognition-view",
        unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
        requiredSupportLevel = Some("view_surfaced"),
      )
    val ownedSlot =
      recognitionSlot.copy(
        id = "rook-recognition-owned",
        requiredSupportLevel = Some("owned_cause_linked")
      )

    val coverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(recognitionSlot, ownedSlot), List(diagnostic))
    val detailOnlyCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(recognitionSlot), List(detailOnlyDiagnostic))

    assertEquals((coverage \ "matchedSlotCount").as[Int], 1)
    assertEquals((coverage \ "slots" \ 0 \ "supportLevel").as[String], "view_surfaced")
    assertEquals((coverage \ "slots" \ 0 \ "matched").as[Boolean], true)
    assertEquals((coverage \ "slots" \ 1 \ "matched").as[Boolean], false)
    assertEquals((detailOnlyCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((detailOnlyCoverage \ "slots" \ 0 \ "supportLevel").as[String], "missing_semantic_slot")

  test("recognition view slots without token probes match public current move surface"):
    val diagnostic =
      comparisonDiagnostic(
        id = "cmp-route-recognition-view",
        referenceLeadAxes = Nil,
        producedKinds = Nil,
        flows = Nil,
        primaryRootKinds = Nil,
        primaryRootIds = Nil,
        positionPlanTechniqueFrameIds = List("frame-route-recognition-view"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.PieceRerouteRoute),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.PieceRerouteRoute),
        positionPlanTechniqueSemanticDetailTokens = List("unit:PieceRerouteRoute"),
        positionPlanTechniqueSemanticDetailTokenGroups = List(List("unit:PieceRerouteRoute")),
        publicMoveMeaningClaimDiagnostics =
          List(
            publicMoveMeaningClaimDiagnostic(
              unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
              meaningKind = "PieceRoute",
              supportLevel = "view_surfaced",
              surfaceLane = "current_move_function",
              lineRole = "candidate",
              moveUci = candidateLine.rootMove,
              sourceEvidenceIds = List("route-src")
            )
          )
      )
    val recognitionSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "route-recognition-view",
        unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
        requiredSupportLevel = Some("view_surfaced")
      )
    val ownedSlot =
      recognitionSlot.copy(
        id = "route-recognition-owned",
        requiredSupportLevel = Some("owned_cause_linked")
      )

    val coverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(recognitionSlot, ownedSlot), List(diagnostic))

    assertEquals((coverage \ "slots" \ 0 \ "supportLevel").as[String], "view_surfaced")
    assertEquals((coverage \ "slots" \ 0 \ "matched").as[Boolean], true)
    assertEquals((coverage \ "slots" \ 1 \ "matched").as[Boolean], false)

  test("semantic rubric flags plan option owned-cause expectations without upgrading recognition"):
    val diagnostic =
      comparisonDiagnostic(
        id = "cmp-plan-option-view",
        referenceLeadAxes = Nil,
        producedKinds = List(RelativeCauseKind.ActivityGain),
        flows = List(causeFlow("cause-activity", RelativeCauseKind.ActivityGain, Nil, List("claim-activity"))),
        primaryRootKinds = List(RelativeCauseKind.ActivityGain),
        primaryRootIds = List("cause-activity"),
        positionPlanTechniqueFrameIds = List("frame-plan-option-view"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.PlanOptionSet),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.PlanOptionSet),
        positionPlanTechniqueSemanticDetailTokens = List("unit:PlanOptionSet", "minorityAttack:true"),
        positionPlanTechniqueSemanticDetailTokenGroups = List(List("unit:PlanOptionSet", "minorityAttack:true")),
        positionPlanTechniqueObjectBindingSignatures =
          List("target=PlanSubject:pawnbreakpreparation|mechanism=Mechanism:plan-pressure"),
        publicMoveMeaningClaimDiagnostics = Nil
      )
    val slot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "minority-plan-owned",
        unit = PositionPlanTechniqueUnit.PlanOptionSet,
        requiredSupportLevel = Some("owned_cause_linked"),
      )

    val coverage = MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(slot), List(diagnostic))

    assertEquals((coverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((coverage \ "slots" \ 0 \ "supportLevel").as[String], "missing_semantic_slot")

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
    assertEquals((coverage \ "slots" \ 0 \ "supportLevel").as[String], "missing_semantic_slot")

  test("ownership slots accept reference-owned public pawn break without current-move overclaim"):
    val axis = "PawnBreak:Support:break-file-e-created-tension-e5-d4"
    val causeId = "cause-reference-break"
    val detailTokens =
      List("unit:TensionBreakPolicyRoute", "axisKind:PawnBreak", "breakFile:e", s"causeEvidenceId:$causeId")
    val diagnostic =
      comparisonDiagnostic(
        id = "cmp-reference-owned-pawn-break",
        referenceLeadAxes = List(axis),
        producedKinds = List(RelativeCauseKind.PawnBreakOpportunity),
        flows = List(causeFlow(causeId, RelativeCauseKind.PawnBreakOpportunity, List(axis), List("claim-reference-break"))),
        primaryRootKinds = List(RelativeCauseKind.PawnBreakOpportunity),
        primaryRootIds = List(causeId),
        positionPlanTechniqueFrameIds = List("frame-reference-owned-pawn-break"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute),
        positionPlanTechniqueAxisKeys = List(axis),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute),
        positionPlanTechniqueSemanticDetailAxisKeys = List(axis),
        positionPlanTechniqueSemanticDetailTokens = detailTokens,
        positionPlanTechniqueSemanticDetailTokenGroups = List(detailTokens),
        positionPlanTechniqueObjectBindingSignatures = List("target=File:e|mechanism=Mechanism:pawnbreak|proof=DirectProof"),
        positionPlanTechniqueRelativeCauseEvidenceIds = List(causeId),
        publicMoveMeaningClaimDiagnostics =
          List(
            publicMoveMeaningClaimDiagnostic(
              unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
              axisKey = Some(axis),
              meaningKind = "PawnBreakTiming",
              supportLevel = "owned_cause_linked",
              surfaceLane = "reference_or_opponent_resource",
              lineRole = "reference",
              moveUci = "e6e5",
              causeEvidenceIds = List(causeId)
            )
          ),
        primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot)
      )
    val softReferenceDiagnostic =
      diagnostic.copy(
        id = "cmp-reference-soft-pawn-break",
        moveJudgmentView = diagnostic.moveJudgmentView.copy(
          publicMoveMeaningClaimDiagnostics = Nil
        )
      )
    val slot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "reference-owned-pawn-break",
        unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
        axisKey = Some(axis),
        requiredSupportLevel = Some("owned_cause_linked"),
      )

    val coverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(slot), List(diagnostic))
    val softCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(slot), List(softReferenceDiagnostic))

    assertEquals((coverage \ "matchedSlotCount").as[Int], 1)
    assertEquals((coverage \ "slots" \ 0 \ "supportLevel").as[String], "owned_cause_linked")
    assertEquals((softCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((softCoverage \ "slots" \ 0 \ "supportLevel").as[String], "missing_semantic_slot")

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

  test("rook endgame pattern anchors preserve typed Philidor ownership geometry"):
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

  test("move meaning claims do not surface non-pawn geometry as current pawn break ownership"):
    val nonPawnLine = lineRef("non-pawn-created-tension", "d4c6", 4, LineNodeRole.Played)
    val ownedCause = causeFrame(
      causeId = "cause-non-pawn-break",
      axisKeys = List("PawnBreak:Support:break-file-c-created-tension-c6-d7"),
      objectSignatures = List("target=Square:c6|target=Square:d7|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    ).copy(
      eventLine = nonPawnLine,
      eventRootMove = nonPawnLine.rootMove,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:break-file-c-created-tension-c6-d7"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("break-file-c-created-tension-c6-d7"),
      breakFile = Some("c"),
      tensionSquares = List("c6", "d7"),
      tensionEdges = List("c6-d7"),
      structuralPurposeSubjects = List("created-tension:c6-d7"),
      candidateEvidenceIds = List("structural-delta:played:d4c6:tension"),
      sourceEvidenceIds = List("structural-delta:played:d4c6:tension"),
      causeEvidenceIds = List("cause-non-pawn-break"),
      objectBindingSignatures =
        List("actor=Move:d4c6|target=Square:c6|target=Square:d7|mechanism=Mechanism:pawn-break|proof=DirectProof"),
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
          verdict = base.verdict.map(_.copy(candidateLine = nonPawnLine)),
          positionPlanTechniqueFrames =
            List(meaningClaimPlanTechniqueFrame(List(detail)).copy(line = Some(nonPawnLine), moveUci = Some(nonPawnLine.rootMove)))
        )
      )

    assert(!view.moveMeaningClaims.exists(claim => claim.meaningKind == "PawnBreakTiming"), view.moveMeaningClaims)

  test("move meaning claims do not surface pawn-shaped non-pawn moves as current pawn break ownership"):
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

  test("move meaning claims do not surface consequence-only pawn break as current move timing"):
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:break-file-e"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("break-file-e"),
      breakFile = Some("e"),
      structuralPurposeConsequences = List("PawnTensionGain"),
      candidateEvidenceIds = List("structural-delta:played:e2e3:tension-kind-only"),
      sourceEvidenceIds = List("structural-delta:played:e2e3:tension-kind-only"),
      objectBindingSignatures =
        List("actor=Move:e2e3|target=File:e|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )
    val claims = view.moveMeaningClaims.filter(_.meaningKind == "PawnBreakTiming")

    assert(!claims.exists(_.surfaceLane.startsWith("current_move")), claims)

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

    assertEquals(routeClaims.map(_.supportLevel), List("owned_cause_linked"))
    assertEquals(routeClaims.map(_.surfaceLane), List("current_move_owned"))
    assertEquals(routeClaims.map(_.role), List("ImprovesPieceRoute"))

  test("move meaning claims surface current-move structural route without cause ownership"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:developmentchoice|consequence=Consequence:developmentpieceactivated"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      proofRoles = Nil,
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.BroadAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("knight:e2-e3", "knight:e2-e3:mobility+2"),
      structuralPurposeConsequences = List("DevelopmentPieceActivated"),
      structuralPurposeCategories = List("PieceActivity"),
      structuralMotifTags = List("piece")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims, Nil)

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

    assertEquals(surface, Nil)
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

    assertEquals(surface, Nil)
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

    assertEquals(view.moveMeaningClaims, Nil)

  test("move meaning claims do not surface plan-pressure-only break option as current move function"):
    val planPressureSignature =
      "actor=Move:e2e3|actor=Square:e2|target=PlanSubject:spaceadvantage|mechanism=Mechanism:plan-pressure|consequence=Consequence:plancoherence:support:pawnbreakpreparation,spaceadvantage|witness=PlanSubject:evidenceatom(pawnadvance(4,2,3,white,0,some(e2e3)),0.16)"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      axisKey = Some("PlanCoherence:Support:PawnBreakPreparation,SpaceAdvantage"),
      axisKind = Some(StrategicAxisKind.PlanCoherence),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("PawnBreakPreparation,SpaceAdvantage"),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceOnly),
      referenceEvidenceIds = List("strategic-mechanism:plan-pressure:e2e3:before-position"),
      sourceEvidenceIds = List("strategic-mechanism:plan-pressure:e2e3:before-position"),
      breakFile = Some("e"),
      objectBindingSignatures = List(planPressureSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.BroadAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )

    assertEquals(
      view.moveMeaningClaims.exists(claim =>
        claim.meaningKind == "PlanContinuity" &&
          claim.role == "PreparesBreakOption" &&
          claim.surfaceLane == "current_move_function"
      ),
      false
    )

  test("move meaning claims do not surface generic structure shift from current-move route alone"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:developmentchoice|consequence=Consequence:developmentpieceactivated"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      axisKey = None,
      axisKind = None,
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.BroadAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("knight:e2-e3", "knight:e2-e3:mobility+2"),
      structuralPurposeConsequences = List("DevelopmentPieceActivated"),
      structuralPurposeCategories = List("PieceActivity"),
      structuralMotifTags = List("space")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims, Nil)

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

    assertEquals(view.moveMeaningClaims, Nil)

  test("move meaning public surface normalizes pawn target signatures"):
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
      structuralPurposeSubjects = List("weak-pawn:d4")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail)
    )

    val surface = MoveMeaningSurface.from(view)
    assertEquals(surface.flatMap(_.target.squares), List("d4"))
    assertEquals(surface.flatMap(_.target.pieces), List("pawn"))

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

  test("move meaning claims do not call engine candidate comparisons current move lanes"):
    val candidateSetCause = causeFrame(
      causeId = "cause-candidate-set-break",
      axisKeys = List("PawnBreak:Support:candidate-set-break"),
      objectSignatures = List("target=File:e|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    ).copy(
      comparisonKind = CandidateComparisonKind.BestVsSecond,
      causeRole = RelativeCauseRole.CandidateSetConstraint,
      causeSourceSide = RelativeCauseSourceSide.Candidate,
      eventLine = candidateLine,
      eventRootMove = candidateLine.rootMove,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:candidate-set-break"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      breakFile = Some("e"),
      tensionSquares = List("e3", "d4"),
      candidateEvidenceIds = List("candidate-set-transition"),
      sourceEvidenceIds = List("candidate-set-transition"),
      causeEvidenceIds = List("cause-candidate-set-break"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List("target=File:e|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      comparisonKind = CandidateComparisonKind.BestVsSecond,
      auditCauses = List(candidateSetCause),
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims.map(_.supportLevel), List("owned_cause_linked"))
    assertEquals(view.moveMeaningClaims.map(_.surfaceLane), List("pv_or_line_witness"))
    assert(!view.moveMeaningClaims.exists(_.surfaceLane.startsWith("current_move")))

  test("move meaning claims keep reference-vs-alternative resources out of current move lanes"):
    val referenceAlternativeCause = causeFrame(
      causeId = "cause-reference-alternative-break",
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
      causeEvidenceIds = List("cause-reference-alternative-break"),
      proofRoles = List(RelativeCauseProofRole.DirectProof, RelativeCauseProofRole.ContrastProof),
      objectBindingSignatures = List("target=File:e|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      comparisonKind = CandidateComparisonKind.ReferenceVsAlternative,
      auditCauses = List(referenceAlternativeCause),
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims.map(_.lineRole), List("reference"))
    assertEquals(view.moveMeaningClaims.map(_.supportLevel), List("owned_cause_linked"))
    assertEquals(view.moveMeaningClaims.map(_.surfaceLane), List("reference_or_opponent_resource"))
    assert(!view.moveMeaningClaims.exists(_.surfaceLane.startsWith("current_move")))

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

    assertEquals(
      view.moveMeaningClaims.map(c => (c.lineRole, c.moveUci, c.supportLevel, c.surfaceLane)),
      List(("reference", referenceLine.rootMove, "owned_cause_linked", "reference_or_opponent_resource"))
    )
    assert(!view.moveMeaningClaims.exists(_.surfaceLane.startsWith("current_move")))

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
