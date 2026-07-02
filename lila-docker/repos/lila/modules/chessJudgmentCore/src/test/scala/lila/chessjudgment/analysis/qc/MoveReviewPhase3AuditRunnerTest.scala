package lila.chessjudgment.analysis.qc

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }

import lila.chessjudgment.analysis.assembly.{ RawMoveReviewInput, RawOpeningContext }
import lila.chessjudgment.analysis.line.{ LineFactNormalizer, PrincipalVariationEvidence }
import lila.chessjudgment.analysis.position.{ FactExtractor, PositionAnalyzer, PositionFactNormalizer }
import lila.chessjudgment.analysis.singlePosition.*
import lila.chessjudgment.analysis.strategic.EndgamePatternOracle
import lila.chessjudgment.model.{ Fact, FactScope, Motif }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.{ RookEndgameGeometry, RookEndgamePattern, VariationLine }
import lila.chessjudgment.model.structure.*
import chess.{ Color, File }
import chess.format.Fen
import chess.variant.Standard
import play.api.libs.json.Json

import scala.jdk.CollectionConverters.*

class MoveReviewPhase3AuditRunnerTest extends munit.FunSuite:

  test("main archives replay input when output path is provided"):
    val dir = Files.createTempDirectory("phase3-audit-runner-main")
    try
      val input = dir.resolve("input.jsonl")
      val output = dir.resolve("phase3_audit_output_current_chunk01_rows001-001.jsonl")
      val row =
        Json.obj(
          "sampleId" -> "sample-main",
          "input" -> Json.obj(
            "fen" -> "8/8/8/8/8/8/4P3/4K3 w - - 0 1",
            "playedMoveUci" -> "e2e4",
            "variations" -> Json.arr(
              Json.obj(
                "moves" -> Json.arr("e2e4"),
                "scoreCp" -> 20,
                "depth" -> 16
              )
            ),
            "currentEvalCp" -> 20,
            "ply" -> 1,
            "movePrefixUci" -> Json.arr("g1f3")
          ),
          "opening" -> "Test Opening",
          "targetPly" -> 1,
          "playedSan" -> "e4"
        )
      Files.writeString(input, Json.stringify(row), StandardCharsets.UTF_8)

      MoveReviewPhase3AuditRunner.main(Array(input.toString, output.toString))

      val archive = dir.resolve("phase3_audit_input_replay_current_chunk01_rows001-001.jsonl")
      val summary = dir.resolve("phase3_audit_summary_current_chunk01_rows001-001.json")
      assert(Files.exists(output))
      assert(Files.exists(archive))
      assert(Files.exists(summary))
      val replay = Json.parse(Files.readString(archive, StandardCharsets.UTF_8))
      assertEquals((replay \ "sampleId").as[String], "sample-main")
      assertEquals((replay \ "input" \ "playedMoveUci").as[String], "e2e4")
      val summaryJson = Json.parse(Files.readString(summary, StandardCharsets.UTF_8))
      assertEquals((summaryJson \ "schemaVersion").as[String], "move_review_phase3_audit_summary.v1")
    finally deleteRecursively(dir)

  test("main can write slim audit rows without full view diagnostics"):
    val dir = Files.createTempDirectory("phase3-audit-runner-slim")
    try
      val input = dir.resolve("input.jsonl")
      val output = dir.resolve("phase3_audit_output_current_chunk01_rows001-001.jsonl")
      val row =
        Json.obj(
          "sampleId" -> "sample-slim",
          "input" -> Json.obj(
            "fen" -> "8/8/8/8/8/8/4P3/4K3 w - - 0 1",
            "playedMoveUci" -> "e2e4",
            "variations" -> Json.arr(
              Json.obj(
                "moves" -> Json.arr("e2e4"),
                "scoreCp" -> 20,
                "depth" -> 16
              )
            ),
            "currentEvalCp" -> 20,
            "ply" -> 1,
            "movePrefixUci" -> Json.arr("g1f3")
          )
        )
      Files.writeString(input, Json.stringify(row), StandardCharsets.UTF_8)

      MoveReviewPhase3AuditRunner.main(Array(input.toString, output.toString, "--slim"))

      val auditRow = Json.parse(Files.readString(output, StandardCharsets.UTF_8))
      assertEquals((auditRow \ "auditMode").as[String], "slim")
      assert((auditRow \ "semanticCoverage").toOption.nonEmpty)
      assert((auditRow \ "semanticRubricExpectedSlotCoverage").toOption.nonEmpty)
      assert((auditRow \ "moveJudgmentView").toOption.isEmpty)
      assert((auditRow \ "claimSupportClusters").toOption.isEmpty)
      assert((auditRow \ "claimEventClusters").toOption.isEmpty)
      assert(Files.exists(dir.resolve("phase3_audit_summary_current_chunk01_rows001-001.json")))
    finally deleteRecursively(dir)

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
        stage: String,
        consequence: LineConsequenceKind,
        objectTokens: List[String],
        causeKinds: List[RelativeCauseKind] = Nil
    ) =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = id,
        unit = PositionPlanTechniqueUnit.StructuralTransformation,
        questionId = Some(questionId),
        requiredTerminalStage = Some(stage),
        requiredCauseKinds = causeKinds,
        requiredTerminalConsequenceKinds = List(consequence),
        requiredCoLocatedSemanticDetailTokens = List("proofRole:DirectProof"),
        requiredObjectBindingTokens = objectTokens
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
        moveMeaningClaimSurfaceSignatures =
          List(
            s"unit=StructuralTransformation|axis=none|kind=TerminalProof|support=$support|lane=$lane|lineRole=candidate|move=$move|causes=$causeId|sources=line:$id|proof=DirectProof|carrier=true|boardCarrier=true|objects=${objectSignature.replace('|', ';')}"
          ),
        publicMoveMeaningClaimSurfaceSignatures =
          List(
            s"unit=StructuralTransformation|axis=none|kind=TerminalProof|support=$support|lane=$lane|lineRole=candidate|move=$move|causes=$causeId|sources=line:$id|proof=DirectProof|carrier=true|boardCarrier=true|objects=${objectSignature.replace('|', ';')}"
          )
      )

    val mateSlot =
      terminalSlot(
        id = "terminal-mate-view",
        questionId = "terminal_mate",
        stage = "view_surfaced",
        consequence = LineConsequenceKind.Mate,
        objectTokens = List("target=Square:h7", "mechanism=Mechanism:Mate")
      )
    val promotionRaceSlot =
      terminalSlot(
        id = "terminal-promotion-race-owned",
        questionId = "terminal_promotion_race",
        stage = "owned_cause_linked",
        consequence = LineConsequenceKind.PromotionRace,
        objectTokens = List("target=Square:a8", "mechanism=Mechanism:promotionrace"),
        causeKinds = List(RelativeCauseKind.ConversionSecured)
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
    assertEquals((coverage \ "byQuestionId" \ "terminal_mate" \ "terminalStageCounts" \ "view_surfaced").as[Int], 1)
    assertEquals((coverage \ "byQuestionId" \ "terminal_promotion_race" \ "ownedCauseLinkedCount").as[Int], 1)
    assertEquals((coverage \ "byQuestionId" \ "terminal_promotion_race" \ "terminalStageCounts" \ "clustered_coherent").as[Int], 1)
    assertEquals((corpusCoverage \ "expectedQuestionCoverageComplete").as[Boolean], true)

  test("semantic rubric reports suppressed and merged move meaning claims"):
    val publicSignature =
      "unit=PieceRerouteRoute|axis=none|kind=PieceRoute|support=view_surfaced|lane=current_move_function|lineRole=played|move=g1f3|causes=none|sources=route-src|proof=none|carrier=true|boardCarrier=true|objects=target=Piece:knight;mechanism=Mechanism:route;witness=Move:g1f3"
    val suppressedSignature =
      "unit=PieceRerouteRoute|axis=none|kind=PieceRoute|support=owned_cause_linked|lane=current_move_owned|lineRole=played|move=g1f3|causes=cause-route|sources=route-src|proof=DirectProof|objects=target=Piece:knight"
    val mergedSignature =
      "unit=PieceRerouteRoute|axis=none|kind=PieceRoute|support=owned_cause_linked|lane=current_move_owned|lineRole=played|move=g1f3|causes=cause-route,cause-structure|sources=route-src,structure-src|proof=DirectProof|carrier=true|boardCarrier=true|objects=target=Piece:knight"
    val carrierlessPublicSignature =
      "unit=PlanOptionSet|axis=none|kind=PlanContinuity|support=view_surfaced|lane=current_move_function|lineRole=played|move=g1f3|causes=none|sources=plan-src|proof=DirectProof|carrier=false|boardCarrier=false|objects=mechanism=Mechanism:activity"
    val diagnostic =
      comparisonDiagnostic(
        id = "surface-diagnostic",
        referenceLeadAxes = Nil,
        producedKinds = List(RelativeCauseKind.ActivityGain),
        flows = List(
          causeFlow(
            "cause-route",
            RelativeCauseKind.ActivityGain,
            proofAxisKeys = Nil,
            claimIds = List("claim-route")
          )
        ),
        primaryRootKinds = List(RelativeCauseKind.ActivityGain),
        primaryRootIds = List("cause-route"),
        moveMeaningClaimSurfaceSignatures =
          List(publicSignature, suppressedSignature, mergedSignature, carrierlessPublicSignature),
        publicMoveMeaningClaimSurfaceSignatures = List(publicSignature, mergedSignature, carrierlessPublicSignature)
      )

    val coverage = MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(Nil, List(diagnostic))
    val corpusCoverage = MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCorpusCoverageJson(List(coverage))

    assertEquals((coverage \ "publicMoveMeaningClaimCount").as[Int], 3)
    assertEquals((coverage \ "boardCarrierlessPublicMoveMeaningClaimCount").as[Int], 1)
    assertEquals(
      (coverage \ "boardCarrierlessPublicMoveMeaningClaimSignatures").as[List[String]],
      List(carrierlessPublicSignature)
    )
    assertEquals((coverage \ "suppressedMoveMeaningClaimCount").as[Int], 1)
    assertEquals((coverage \ "suppressedMoveMeaningClaimSignatures").as[List[String]], List(suppressedSignature))
    assertEquals((coverage \ "mergedOwnershipClaimCount").as[Int], 1)
    assertEquals((coverage \ "mergedOwnershipClaimSignatures").as[List[String]], List(mergedSignature))
    assertEquals((corpusCoverage \ "publicMoveMeaningClaimCount").as[Int], 3)
    assertEquals((corpusCoverage \ "boardCarrierlessPublicMoveMeaningClaimCount").as[Int], 1)
    assertEquals((corpusCoverage \ "suppressedMoveMeaningClaimCount").as[Int], 1)
    assertEquals((corpusCoverage \ "mergedOwnershipClaimCount").as[Int], 1)

  test("writes replay input archive next to audit output"):
    val dir = Files.createTempDirectory("phase3-audit-runner")
    try
      val output = dir.resolve("phase3_audit_output_current_chunk01_rows001-001.jsonl")
      val raw =
        RawMoveReviewInput(
          fen = "8/8/8/8/8/8/4P3/4K3 w - - 0 1",
          playedMoveUci = "e2e4",
          variations = List(VariationLine(List("e2e4"), scoreCp = 20, depth = 16)),
          currentEvalCp = Some(20),
          ply = Some(1),
          openingContext = Some(RawOpeningContext(eco = Some("A00"), name = Some("Test Opening"), family = Some("A"))),
          movePrefixUci = List("g1f3")
        )
      val sample =
        MoveReviewPhase3AuditRunner.AuditInputSample(
          sampleId = "sample-1",
          raw = raw,
          opening = Some("Test Opening"),
          sliceKind = Some("eco"),
          targetPly = Some(1),
          playedSan = Some("e4")
        )

      val archive = MoveReviewPhase3AuditRunner.writeReplayInputArchive(output, List(sample))

      assertEquals(archive.getFileName.toString, "phase3_audit_input_replay_current_chunk01_rows001-001.jsonl")
      val rows = Files.readAllLines(archive, StandardCharsets.UTF_8).asScala.toList
      assertEquals(rows.size, 1)
      val json = Json.parse(rows.head)
      assertEquals((json \ "schemaVersion").as[String], "move_review_phase3_replay_input.v1")
      assertEquals((json \ "sampleId").as[String], "sample-1")
      assertEquals((json \ "input" \ "playedMoveUci").as[String], "e2e4")
      assertEquals((json \ "input" \ "variations" \ 0 \ "moves" \ 0).as[String], "e2e4")
      assertEquals((json \ "input" \ "movePrefixUci" \ 0).as[String], "g1f3")
      assertEquals((json \ "opening").as[String], "Test Opening")
    finally deleteRecursively(dir)

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

  test("root arbitration quality summary separates fallback broad and context-only tiers"):
    val exact =
      comparisonDiagnostic(
        id = "cmp-exact-root",
        referenceLeadAxes = List("Activity:Gain:outpost-gain"),
        producedKinds = List(RelativeCauseKind.ActivityGain),
        flows = List(
          causeFlow(
            causeId = "cause-exact",
            kind = RelativeCauseKind.ActivityGain,
            proofAxisKeys = List("Activity:Gain:outpost-gain"),
            claimIds = List("claim-exact")
          )
        ),
        primaryRootKinds = List(RelativeCauseKind.ActivityGain),
        primaryRootIds = List("cause-exact"),
        rootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot),
        primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot)
      )
    val fallback =
      comparisonDiagnostic(
        id = "cmp-plan-fallback",
        referenceLeadAxes = List("PlanCoherence:Preserve:main-plan"),
        producedKinds = List(RelativeCauseKind.PlanContradiction),
        flows = List(
          causeFlow(
            causeId = "cause-plan",
            kind = RelativeCauseKind.PlanContradiction,
            proofAxisKeys = List("PlanCoherence:Preserve:main-plan"),
            claimIds = List("claim-plan")
          )
        ),
        primaryRootKinds = List(RelativeCauseKind.PlanContradiction),
        primaryRootIds = List("cause-plan"),
        rootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.FallbackRoot),
        primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.FallbackRoot)
      )
    val broadAndContext =
      comparisonDiagnostic(
        id = "cmp-broad-context",
        referenceLeadAxes = List("Activity:Gain:mobility-gain"),
        producedKinds = List(RelativeCauseKind.ActivityGain, RelativeCauseKind.TacticalRefutationOfPlayed),
        flows = List(
          causeFlow(
            causeId = "cause-broad",
            kind = RelativeCauseKind.ActivityGain,
            proofAxisKeys = List("Activity:Gain:mobility-gain"),
            claimIds = List("claim-broad")
          )
        ),
        primaryRootKinds = List(RelativeCauseKind.TacticalRefutationOfPlayed),
        primaryRootIds = List("cause-tactical"),
        rootArbitrationTiers = List(
          MoveJudgmentCauseRootArbitrationTier.BroadOwnedRoot,
          MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot,
          MoveJudgmentCauseRootArbitrationTier.ContextOnly
        ),
        primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot)
      )

    val summary = MoveReviewPhase3AuditRunner.rootArbitrationQualitySummaryJson(List(exact, fallback, broadAndContext))

    assertEquals((summary \ "classification").as[String], "audit_only")
    assertEquals((summary \ "comparisonCount").as[Int], 3)
    assertEquals((summary \ "tierCounts" \ "FallbackRoot").as[Int], 1)
    assertEquals((summary \ "tierCounts" \ "BroadOwnedRoot").as[Int], 1)
    assertEquals((summary \ "tierCounts" \ "ContextOnly").as[Int], 1)
    assertEquals((summary \ "primaryRootTierCounts" \ "ExactOwnedRoot").as[Int], 1)
    assertEquals((summary \ "primaryRootTierCounts" \ "FallbackRoot").as[Int], 1)
    assertEquals((summary \ "primaryRootTierCounts" \ "ConcreteOwnedRoot").as[Int], 1)
    assertEquals((summary \ "fallbackRootComparisonIds").as[List[String]], List("cmp-plan-fallback"))
    assertEquals((summary \ "broadOwnedRootComparisonIds").as[List[String]], List("cmp-broad-context"))
    assertEquals((summary \ "contextOnlyComparisonIds").as[List[String]], List("cmp-broad-context"))

  test("structural opportunity funnel counts concrete axis masked by plan fallback"):
    val diagnostic =
      comparisonDiagnostic(
        id = "cmp-outpost-plan-fallback",
        referenceLeadAxes = List("Target:Gain:target-pressure-gain"),
        producedKinds = List(RelativeCauseKind.PlanContradiction),
        flows = List(
          causeFlow(
            causeId = "cause-plan",
            kind = RelativeCauseKind.PlanContradiction,
            proofAxisKeys = List("Target:Gain:target-pressure-gain", "PlanCoherence:Preserve:main-plan"),
            claimIds = List("claim-plan")
          )
        ),
        primaryRootKinds = List(RelativeCauseKind.PlanContradiction),
        primaryRootIds = List("cause-plan")
      )

    val funnel = MoveReviewPhase3AuditRunner.structuralOpportunityGenerationFunnelJson(List(diagnostic))

    assertEquals((funnel \ "axisOpportunityCount").as[Int], 1)
    assertEquals((funnel \ "terminalStageCounts" \ "fallback_masked_concrete_axis").as[Int], 1)
    assertEquals(
      (funnel \ "fallbackMaskedConcreteAxisComparisonIds").as[List[String]],
      List("cmp-outpost-plan-fallback")
    )
    assertEquals((funnel \ "causeClassCounts" \ "plan_fallback").as[Int], 1)
    assertEquals((funnel \ "expectedCauseKindCounts" \ "TargetPressureGain").as[Int], 1)
    assertEquals((funnel \ "byAxis" \ "Target:Gain:target-pressure-gain" \ "terminalStageCounts" \ "fallback_masked_concrete_axis").as[Int], 1)

  test("structural opportunity funnel tracks exact axis lineage instead of same-kind roots"):
    val diagnostic =
      comparisonDiagnostic(
        id = "cmp-same-kind-different-axis",
        referenceLeadAxes = List("Activity:Gain:outpost-gain", "Activity:Gain:mobility-gain"),
        producedKinds = List(RelativeCauseKind.ActivityGain),
        flows = List(
          causeFlow(
            causeId = "cause-activity-mobility",
            kind = RelativeCauseKind.ActivityGain,
            proofAxisKeys = List("Activity:Gain:mobility-gain"),
            claimIds = List("claim-activity-mobility")
          )
        ),
        primaryRootKinds = List(RelativeCauseKind.ActivityGain),
        primaryRootIds = List("cause-activity-mobility")
      )

    val funnel = MoveReviewPhase3AuditRunner.structuralOpportunityGenerationFunnelJson(List(diagnostic))
    val exact = funnel \ "axisExactLineage"

    assertEquals((exact \ "axisOpportunityCount").as[Int], 2)
    assertEquals((exact \ "exactRootCount").as[Int], 1)
    assertEquals((exact \ "kindOnlyRootWithoutAxisCount").as[Int], 1)
    assertEquals(
      (exact \ "byAxis" \ "Activity:Gain:mobility-gain" \ "terminalStageCounts" \ "final_structural_root").as[Int],
      1
    )
    assertEquals(
      (exact \ "byAxis" \ "Activity:Gain:outpost-gain" \ "terminalStageCounts" \ "kind_only_root_without_axis").as[Int],
      1
    )

  test("plan technique eligibility funnel separates playable tactical and structural no-axis bad rows"):
    val structuralAxis =
      comparisonDiagnostic(
        id = "cmp-structural-axis",
        referenceLeadAxes = List("Activity:Gain:outpost-gain"),
        producedKinds = List(RelativeCauseKind.ActivityGain),
        flows = List(
          causeFlow(
            causeId = "cause-activity",
            kind = RelativeCauseKind.ActivityGain,
            proofAxisKeys = List("Activity:Gain:outpost-gain"),
            claimIds = List("claim-activity")
          )
        ),
        primaryRootKinds = List(RelativeCauseKind.ActivityGain),
        primaryRootIds = List("cause-activity")
      )
    val playableNoAxis =
      comparisonDiagnostic(
        id = "cmp-playable-no-axis",
        referenceLeadAxes = Nil,
        producedKinds = Nil,
        flows = Nil,
        primaryRootKinds = Nil,
        primaryRootIds = Nil,
        verdict = MoveChoiceVerdict.PlayableLoss
      )
    val tacticalNoAxis =
      comparisonDiagnostic(
        id = "cmp-tactical-no-axis",
        referenceLeadAxes = Nil,
        producedKinds = Nil,
        flows = Nil,
        primaryRootKinds = Nil,
        primaryRootIds = Nil,
        relationKinds = List(RelationFactKind.Pin)
      )
    val structuralMissingAxis =
      comparisonDiagnostic(
        id = "cmp-structural-missing-axis",
        referenceLeadAxes = Nil,
        producedKinds = Nil,
        flows = Nil,
        primaryRootKinds = Nil,
        primaryRootIds = Nil
      )

    val summary =
      MoveReviewPhase3AuditRunner.relativeCauseQualitySummaryJson(
        List(structuralAxis, playableNoAxis, tacticalNoAxis, structuralMissingAxis)
      )
    val funnel = summary \ "planTechniqueEligibilityFunnel"

    assertEquals((funnel \ "comparisonCount").as[Int], 4)
    assertEquals((funnel \ "structuralAxisPresentCount").as[Int], 1)
    assertEquals((funnel \ "playableLossNoAxisCount").as[Int], 1)
    assertEquals((funnel \ "tacticalOnlyNoAxisCount").as[Int], 1)
    assertEquals((funnel \ "structuralAxisMissingCandidateCount").as[Int], 1)
    assertEquals((funnel \ "classificationCounts" \ "structural_axis_present").as[Int], 1)
    assertEquals((funnel \ "classificationCounts" \ "playable_loss_no_axis").as[Int], 1)
    assertEquals((funnel \ "classificationCounts" \ "tactical_only_no_axis").as[Int], 1)
    assertEquals((funnel \ "classificationCounts" \ "structural_axis_missing_candidate").as[Int], 1)
    assertEquals(
      (funnel \ "structuralAxisMissingCandidateComparisonIds").as[List[String]],
      List("cmp-structural-missing-axis")
    )

  test("plan technique anchor eligibility only escalates axisless inventory when a structural missing candidate exists"):
    val playableNoAxis =
      comparisonDiagnostic(
        id = "cmp-playable-no-axis",
        referenceLeadAxes = Nil,
        producedKinds = Nil,
        flows = Nil,
        primaryRootKinds = Nil,
        primaryRootIds = Nil,
        verdict = MoveChoiceVerdict.PlayableLoss
      )
    val structuralMissingAxis =
      comparisonDiagnostic(
        id = "cmp-structural-missing-axis",
        referenceLeadAxes = Nil,
        producedKinds = Nil,
        flows = Nil,
        primaryRootKinds = Nil,
        primaryRootIds = Nil
      )
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/8/8/8 b - - 1 1", 2, Some(chess.Color.Black), Some("after"))
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = after,
      line = Some(candidateLine),
      perspective = chess.Color.White
    )
    val axislessGraph =
      TypedEvidenceGraph(
        List(
          EvidenceRecord(
            evidenceRef("structural-delta:axisless", root),
            StructuralDeltaEvidence(
              transition = transition,
              signals = Nil,
              consequences = List(
                TransitionConsequence(TransitionConsequenceKind.PassedPawnProgress, StructuralSignalPolarity.Gain, 3),
                TransitionConsequence(TransitionConsequenceKind.PromotionPressureGain, StructuralSignalPolarity.Gain, 3)
              )
            )
          )
        )
      )

    val inventoryOnly =
      MoveReviewPhase3AuditRunner.planTechniqueAnchorEligibilityJson(List(playableNoAxis), axislessGraph)
    val missingWithInventory =
      MoveReviewPhase3AuditRunner.planTechniqueAnchorEligibilityJson(List(playableNoAxis, structuralMissingAxis), axislessGraph)

    assertEquals((inventoryOnly \ "resolution").as[String], "inventory_only_no_structural_missing_candidate")
    assertEquals((inventoryOnly \ "axislessStructuralAnchorSignalCount").as[Int], 2)
    assertEquals((inventoryOnly \ "axislessStructuralAnchorPlanTechniqueUnitCandidateCounts" \ "StructuralTransformation").as[Int], 2)
    assertEquals((inventoryOnly \ "structuralAxisMissingCandidateCount").as[Int], 0)
    assertEquals((inventoryOnly \ "structuralAxisMissingCandidateWithAnyPacketAxislessAnchorCount").as[Int], 0)
    assertEquals((missingWithInventory \ "resolution").as[String], "upstream_axis_generation_candidate")
    assertEquals((missingWithInventory \ "structuralAxisMissingCandidateCount").as[Int], 1)
    assertEquals((missingWithInventory \ "structuralAxisMissingCandidateWithAnyPacketAxislessAnchorCount").as[Int], 1)
    assertEquals(
      (missingWithInventory \ "axislessStructuralAnchorPlanTechniqueUnitCandidateLabels" \ "StructuralTransformation").as[List[String]],
      List("passed-pawn-progress", "promotion-pressure-gain")
    )
    assertEquals((missingWithInventory \ "axislessStructuralAnchorUnmappedLabelCounts").as[play.api.libs.json.JsObject].value.size, 0)
    assertEquals(
      (missingWithInventory \ "structuralAxisMissingCandidateWithAnyPacketAxislessAnchorComparisonIds").as[List[String]],
      List("cmp-structural-missing-axis")
    )

  test("plan technique anchor eligibility separates broad activity inventory from object-bound reroute candidates"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/8/8/8 b - - 1 1", 2, Some(chess.Color.Black), Some("after"))
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = after,
      line = Some(candidateLine),
      perspective = chess.Color.White
    )
    val graph =
      TypedEvidenceGraph(
        List(
          EvidenceRecord(
            evidenceRef("structural-delta:activity-axisless", root),
            StructuralDeltaEvidence(
              transition = transition,
              signals = Nil,
              consequences = List(
                TransitionConsequence(TransitionConsequenceKind.MobilityGain, StructuralSignalPolarity.Gain, 2),
                TransitionConsequence(TransitionConsequenceKind.MobilityLoss, StructuralSignalPolarity.Loss, 2)
              )
            )
          )
        )
      )

    val summary = MoveReviewPhase3AuditRunner.planTechniqueAnchorEligibilityJson(Nil, graph)

    assertEquals((summary \ "axislessStructuralAnchorPlanTechniqueUnitCandidateCounts" \ "PieceRerouteRoute").as[Int], 2)
    assertEquals((summary \ "axislessStructuralAnchorPlanTechniqueUnitObjectBoundCandidateCounts").as[play.api.libs.json.JsObject].value.size, 0)
    assertEquals((summary \ "axislessStructuralAnchorPlanTechniqueUnitBroadCandidateCounts" \ "PieceRerouteRoute").as[Int], 2)
    assertEquals(
      (summary \ "axislessStructuralAnchorPlanTechniqueUnitBroadLabels" \ "PieceRerouteRoute").as[List[String]],
      List("activity-gain", "activity-loss")
    )

  test("plan technique anchor eligibility does not borrow object subjects from a different structural consequence"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/8/8/8 b - - 1 1", 2, Some(chess.Color.Black), Some("after"))
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = after,
      line = Some(candidateLine),
      perspective = chess.Color.White
    )
    val graph =
      TypedEvidenceGraph(
        List(
          EvidenceRecord(
            evidenceRef("structural-delta:mixed-subjects", root),
            StructuralDeltaEvidence(
              transition = transition,
              signals = Nil,
              consequences = List(
                TransitionConsequence(TransitionConsequenceKind.PassedPawnProgress, StructuralSignalPolarity.Gain, 3),
                TransitionConsequence(
                  TransitionConsequenceKind.OutpostGain,
                  StructuralSignalPolarity.Gain,
                  3,
                  subjects = List("outpost:knight:e5")
                )
              )
            )
          )
        )
      )

    val summary = MoveReviewPhase3AuditRunner.planTechniqueAnchorEligibilityJson(Nil, graph)

    assertEquals((summary \ "axislessStructuralAnchorPlanTechniqueUnitCandidateCounts" \ "StructuralTransformation").as[Int], 1)
    assertEquals((summary \ "axislessStructuralAnchorPlanTechniqueUnitObjectBoundCandidateCounts").as[play.api.libs.json.JsObject].value.size, 0)
    assertEquals((summary \ "axislessStructuralAnchorPlanTechniqueUnitBroadCandidateCounts" \ "StructuralTransformation").as[Int], 1)
    assertEquals(
      (summary \ "axislessStructuralAnchorPlanTechniqueUnitBroadLabels" \ "StructuralTransformation").as[List[String]],
      List("passed-pawn-progress")
    )

  test("semantic rubric funnel strict lineage does not treat row-level cause ownership as detail-owned"):
    def diagnosticWithDetailTokens(
        id: String,
        detailTokens: List[String]
    ): CandidateComparisonDiagnostic =
      comparisonDiagnostic(
        id = id,
        referenceLeadAxes = List("PawnBreak:Support:central-break-timing"),
        producedKinds = List(RelativeCauseKind.PawnBreakOpportunity),
        flows = List(
          causeFlow(
            causeId = "cause-central-break",
            kind = RelativeCauseKind.PawnBreakOpportunity,
            proofAxisKeys = List("PawnBreak:Support:central-break-timing"),
            claimIds = List("claim-central-break")
          )
        ),
        primaryRootKinds = List(RelativeCauseKind.PawnBreakOpportunity),
        primaryRootIds = List("cause-central-break"),
        positionPlanTechniqueFrameIds = List(s"frame-$id"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute),
        positionPlanTechniqueAxisKeys = List("PawnBreak:Support:central-break-timing"),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute),
        positionPlanTechniqueSemanticDetailAxisKeys = List("PawnBreak:Support:central-break-timing"),
        positionPlanTechniqueSemanticDetailTokens = detailTokens,
        positionPlanTechniqueSemanticDetailTokenGroups = List(detailTokens),
        positionPlanTechniqueObjectBindingSignatures = List("target=Pawn:e4|mechanism=Mechanism:pawn-break|proof=DirectProof"),
        positionPlanTechniqueRelativeCauseEvidenceIds = List("cause-central-break"),
        moveMeaningClaimSurfaceSignatures =
          List("unit=TensionBreakPolicyRoute|axis=PawnBreak:Support:central-break-timing|kind=PawnBreakTiming|support=owned_cause_linked|lane=current_move_owned|causes=cause-central-break|carrier=true|boardCarrier=true|objects=target=Pawn:e4;mechanism=Mechanism:pawn-break"),
        publicMoveMeaningClaimSurfaceSignatures =
          List("unit=TensionBreakPolicyRoute|axis=PawnBreak:Support:central-break-timing|kind=PawnBreakTiming|support=owned_cause_linked|lane=current_move_owned|causes=cause-central-break|carrier=true|boardCarrier=true|objects=target=Pawn:e4;mechanism=Mechanism:pawn-break"),
        primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot)
      )
    val borrowed =
      diagnosticWithDetailTokens("borrowed-row-cause", List("pawnBreakReady:true", "objectTarget:Pawn:e4"))
    val coalesced =
      diagnosticWithDetailTokens(
        "coalesced-row-cause",
        List("pawnBreakReady:true", "objectTarget:Pawn:e4", "causeEvidenceId:cause-central-break")
      )

    val borrowedFunnel = MoveReviewPhase3AuditRunner.semanticRubricFunnelJson(List(borrowed))
    val coalescedFunnel = MoveReviewPhase3AuditRunner.semanticRubricFunnelJson(List(coalesced))

    assertEquals((borrowedFunnel \ "looseStageCounts" \ "clustered_coherent").as[Int], 1)
    assertEquals((borrowedFunnel \ "stageCounts" \ "clustered_coherent").as[Int], 0)
    assertEquals((borrowedFunnel \ "strictLineageStageCounts" \ "clustered_coherent").as[Int], 0)
    assertEquals((borrowedFunnel \ "strictLineageTerminalStageCounts" \ "exact_axis_or_pattern").as[Int], 1)
    assertEquals((borrowedFunnel \ "strictCauseLineageBoundCount").as[Int], 0)
    assertEquals((coalescedFunnel \ "strictLineageStageCounts" \ "clustered_coherent").as[Int], 1)
    assertEquals((coalescedFunnel \ "strictCauseLineageBoundCount").as[Int], 1)

  test("expected semantic ownership slots require cause evidence co-located with semantic detail"):
    def diagnosticWithDetailTokens(
        id: String,
        detailTokens: List[String],
        flowDiagnostics: List[RelativeCauseFlowDiagnostic] = List(
          causeFlow(
            causeId = "cause-central-break",
            kind = RelativeCauseKind.PawnBreakOpportunity,
            proofAxisKeys = List("PawnBreak:Support:central-break-timing"),
            claimIds = List("claim-central-break")
          )
        ),
        relativeCauseEvidenceIds: List[String] = List("cause-central-break")
    ): CandidateComparisonDiagnostic =
      comparisonDiagnostic(
        id = id,
        referenceLeadAxes = List("PawnBreak:Support:central-break-timing"),
        producedKinds = flowDiagnostics.map(_.causeKind).distinct,
        flows = flowDiagnostics,
        primaryRootKinds = List(RelativeCauseKind.PawnBreakOpportunity),
        primaryRootIds = List("cause-central-break"),
        positionPlanTechniqueFrameIds = List(s"frame-$id"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute),
        positionPlanTechniqueAxisKeys = List("PawnBreak:Support:central-break-timing"),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute),
        positionPlanTechniqueSemanticDetailAxisKeys = List("PawnBreak:Support:central-break-timing"),
        positionPlanTechniqueSemanticDetailTokens = detailTokens,
        positionPlanTechniqueSemanticDetailTokenGroups = List(detailTokens),
        positionPlanTechniqueObjectBindingSignatures = List("target=Pawn:e4|mechanism=Mechanism:pawn-break|proof=DirectProof"),
        positionPlanTechniqueRelativeCauseEvidenceIds = relativeCauseEvidenceIds,
        moveMeaningClaimSurfaceSignatures =
          List("unit=TensionBreakPolicyRoute|axis=PawnBreak:Support:central-break-timing|kind=PawnBreakTiming|support=owned_cause_linked|lane=current_move_owned|causes=cause-central-break|carrier=true|boardCarrier=true|objects=target=Pawn:e4;mechanism=Mechanism:pawn-break"),
        publicMoveMeaningClaimSurfaceSignatures =
          List("unit=TensionBreakPolicyRoute|axis=PawnBreak:Support:central-break-timing|kind=PawnBreakTiming|support=owned_cause_linked|lane=current_move_owned|causes=cause-central-break|carrier=true|boardCarrier=true|objects=target=Pawn:e4;mechanism=Mechanism:pawn-break"),
        primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot)
      )
    val slot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "owned-pawn-break-slot",
        unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
        questionId = Some("pawn_break_timing_ownership"),
        requiredTerminalStage = Some("owned_cause_linked"),
        requiredCauseKinds = List(RelativeCauseKind.PawnBreakOpportunity, RelativeCauseKind.PlanImprovement),
        requiredSemanticDetailTokens = List("pawnBreakReady:true")
      )
    val borrowedCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(slot),
        List(
          diagnosticWithDetailTokens(
            "borrowed-row-cause",
            List("unit:TensionBreakPolicyRoute", "pawnBreakReady:true", "objectTarget:Pawn:e4")
          )
        )
      )
    val ownedCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(slot),
        List(
          diagnosticWithDetailTokens(
            "co-located-cause",
            List(
              "unit:TensionBreakPolicyRoute",
              "pawnBreakReady:true",
              "objectTarget:Pawn:e4",
              "causeEvidenceId:cause-central-break"
            )
          )
        )
      )
    val pawnBreakOnlySlot = slot.copy(
      id = "owned-pawn-break-slot-id-kind-bound",
      requiredCauseKinds = List(RelativeCauseKind.PawnBreakOpportunity)
    )
    val wrongKindCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(pawnBreakOnlySlot),
        List(
          diagnosticWithDetailTokens(
            "wrong-kind-cause-id",
            List(
              "unit:TensionBreakPolicyRoute",
              "pawnBreakReady:true",
              "objectTarget:Pawn:e4",
              "causeEvidenceId:cause-defensive-context"
            ),
            flowDiagnostics = List(
              causeFlow(
                causeId = "cause-central-break",
                kind = RelativeCauseKind.PawnBreakOpportunity,
                proofAxisKeys = List("PawnBreak:Support:central-break-timing"),
                claimIds = List("claim-central-break")
              ),
              causeFlow(
                causeId = "cause-defensive-context",
                kind = RelativeCauseKind.DefensiveResource,
                proofAxisKeys = List("PawnBreak:Support:central-break-timing"),
                claimIds = List("claim-defensive-context")
              )
            ),
            relativeCauseEvidenceIds = List("cause-central-break", "cause-defensive-context")
          )
        )
      )

    assertEquals((borrowedCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((borrowedCoverage \ "causeBorrowFalsePositiveCount").as[Int], 1)
    assertEquals((borrowedCoverage \ "slots" \ 0 \ "causeLineageTokenCoLocationSatisfied").as[Boolean], false)
    assertEquals((borrowedCoverage \ "slots" \ 0 \ "legacyMatchedBeforeStrictLineage").as[Boolean], true)
    assertEquals((ownedCoverage \ "matchedSlotCount").as[Int], 1)
    assertEquals((ownedCoverage \ "slots" \ 0 \ "terminalStageSatisfied").as[Boolean], true)
    assertEquals((ownedCoverage \ "slots" \ 0 \ "causeLineageTokenCoLocationSatisfied").as[Boolean], true)
    assert((ownedCoverage \ "slots" \ 0 \ "terminalStage").as[String] == "clustered_coherent")
    assertEquals((wrongKindCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((wrongKindCoverage \ "causeBorrowFalsePositiveCount").as[Int], 1)
    assertEquals((wrongKindCoverage \ "slots" \ 0 \ "causeLineageTokenCoLocationSatisfied").as[Boolean], false)
    assertEquals((wrongKindCoverage \ "slots" \ 0 \ "legacyMatchedBeforeStrictLineage").as[Boolean], true)

  test("structure compensation slots do not borrow center control ownership from IQP transition detail"):
    val causeId = "move-review:demo:evidence:relative-cause:played-vs-best:center-control-gain:a:b:0"
    def diagnosticWithStructuralTokens(
        id: String,
        detailTokens: List[String]
    ): CandidateComparisonDiagnostic =
      comparisonDiagnostic(
        id = id,
        referenceLeadAxes = List("Target:Gain:weak-pawn-target"),
        producedKinds = List(RelativeCauseKind.CenterControlGain),
        flows = List(
          causeFlow(
            causeId = causeId,
            kind = RelativeCauseKind.CenterControlGain,
            proofAxisKeys = List("Target:Gain:weak-pawn-target"),
            claimIds = List("claim-structural-compensation")
          )
        ),
        primaryRootKinds = List(RelativeCauseKind.CenterControlGain),
        primaryRootIds = List(causeId),
        positionPlanTechniqueFrameIds = List(s"frame-$id"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.StructuralTransformation),
        positionPlanTechniqueAxisKeys = List("Target:Gain:weak-pawn-target"),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.StructuralTransformation),
        positionPlanTechniqueSemanticDetailAxisKeys = List("Target:Gain:weak-pawn-target"),
        positionPlanTechniqueSemanticDetailTokens = detailTokens,
        positionPlanTechniqueSemanticDetailTokenGroups = List(detailTokens),
        positionPlanTechniqueObjectBindingSignatures = List("target=Pawn:weak-pawn:d4|mechanism=Mechanism:WeakPawnTargetCreated|proof=DirectProof"),
        positionPlanTechniqueRelativeCauseEvidenceIds = List(causeId),
        primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot)
      )
    val slot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "iqp-structure-compensation-owned",
        unit = PositionPlanTechniqueUnit.StructuralTransformation,
        questionId = Some("structure_compensation"),
        requiredTerminalStage = Some("clustered_coherent"),
        requiredCauseKinds = List(
          RelativeCauseKind.StructuralImprovement,
          RelativeCauseKind.PawnWeaknessTarget,
          RelativeCauseKind.TargetPressureGain,
          RelativeCauseKind.PlanContradiction
        ),
        requiredSemanticDetailTokens = List("IQP", "isolated", "transition")
      )
    val ownedCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(slot),
        List(
          diagnosticWithStructuralTokens(
            "owned-iqp-transition",
            List(
              "unit:StructuralTransformation",
              "axisKey:Target:Gain:weak-pawn-target",
              "iqp",
              "isolated",
              "transition",
              s"causeEvidenceId:$causeId"
            )
          )
        )
      )
    val borrowedCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(slot),
        List(
          diagnosticWithStructuralTokens(
            "borrowed-iqp-transition",
            List("unit:StructuralTransformation", "axisKey:Target:Gain:weak-pawn-target", "iqp", "isolated", "transition")
          )
        )
      )

    assertEquals((ownedCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((ownedCoverage \ "slots" \ 0 \ "semanticDetailTokensSatisfied").as[Boolean], true)
    assertEquals((ownedCoverage \ "slots" \ 0 \ "causeKinds").as[List[String]], Nil)
    assertEquals((ownedCoverage \ "slots" \ 0 \ "terminalStage").as[String], "missing_semantic_slot")
    assertEquals((ownedCoverage \ "slots" \ 0 \ "causeLineageTokenCoLocationSatisfied").as[Boolean], false)
    assertEquals((borrowedCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((borrowedCoverage \ "slots" \ 0 \ "causeLineageTokenCoLocationSatisfied").as[Boolean], false)

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
        moveMeaningClaimSurfaceSignatures =
          List(s"unit=EndgameTechniqueRecipe|axis=none|kind=RookEndgameTechnique|support=owned_cause_linked|lane=current_move_owned|causes=$causeId|carrier=true|boardCarrier=true|objects=target=Square:d8;mechanism=Mechanism:EndgameTechnique"),
        publicMoveMeaningClaimSurfaceSignatures =
          List(s"unit=EndgameTechniqueRecipe|axis=none|kind=RookEndgameTechnique|support=owned_cause_linked|lane=current_move_owned|causes=$causeId|carrier=true|boardCarrier=true|objects=target=Square:d8;mechanism=Mechanism:EndgameTechnique"),
        primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot)
      )

    val lucenaSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "rook-endgame-lucena-recognition",
        unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
        questionId = Some("rook_endgame.conversion_recipe"),
        requiredTerminalStage = Some("view_surfaced"),
        requiredMechanismKinds = List(StrategicMechanismKind.Endgame),
        requiredCauseKinds = List(RelativeCauseKind.ConversionSecured),
        requiredSemanticDetailTokens = List("boardAnchorSignal:EndgameRookPattern"),
        requiredSemanticAnchorTokens = List("pattern:Lucena", "rook-pattern:RookBehindPassedPawn")
      )
    val philidorSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "rook-endgame-philidor-recognition",
        unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
        questionId = Some("rook_endgame.draw_resource"),
        requiredTerminalStage = Some("view_surfaced"),
        requiredMechanismKinds = List(StrategicMechanismKind.Endgame),
        requiredCauseKinds = List(RelativeCauseKind.DrawResource),
        requiredSemanticDetailTokens = List("boardAnchorSignal:EndgameRookPattern"),
        requiredSemanticAnchorTokens = List("pattern:PhilidorDefense", "rook-pattern:KingCutOff")
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
    assertEquals((coverage \ "viewSurfacedCount").as[Int], 2)
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

    val json = MoveReviewPhase3AuditViewJson.positionPlanTechniqueFrameJson(frame, ref => Json.obj("id" -> ref.id))
    val jsonDetail = json \ "semanticDetails" \ 0
    assertEquals((jsonDetail \ "endgameTechniquePattern").as[String], "Lucena")
    assertEquals((jsonDetail \ "endgameTechniqueHorizonStatus").as[String], "Transitioned")
    assertEquals((jsonDetail \ "endgameTechniqueTriggerMove").as[String], "d7d8q")
    assertEquals((jsonDetail \ "maintainedSquares").as[List[String]], List("d7", "d8", "e7"))

  test("owned rook horizon slots require semantic anchors squares and line evidence"):
    val slot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "owned-line-lucena-horizon",
        unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
        questionId = Some("rook_endgame_technique"),
        requiredTerminalStage = Some("owned_cause_linked"),
        requiredCauseKinds = List(RelativeCauseKind.ConversionSecured),
        requiredSemanticDetailTokens = List(
          "pattern:Lucena",
          "rook-pattern:RookBehindPassedPawn",
          "horizonStatus:Transitioned",
          "requiredSquare:d8",
          "sourceEvidenceId:line:lucena-horizon"
        )
      )
    val patternOnlyTokens =
      List(
        "unit:EndgameTechniqueRecipe",
        "pattern:Lucena",
        "rook-pattern:RookBehindPassedPawn",
        "horizonStatus:Transitioned",
        "causeEvidenceId:cause-lucena"
      )
    val ownedTokens =
      patternOnlyTokens ++
        List(
          "requiredSquare:d8",
          "maintainedSquare:d8",
          "sourceEvidenceId:line:lucena-horizon"
        )
    val patternOnlyCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(slot),
        List(
          diagnosticWithDetailTokens(
            "pattern-only-lucena",
            patternOnlyTokens,
            causeKind = RelativeCauseKind.ConversionSecured
          )
        )
      )
    val ownedCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(slot),
        List(
          diagnosticWithDetailTokens(
            "owned-lucena",
            ownedTokens,
            causeKind = RelativeCauseKind.ConversionSecured
          )
        )
      )

    assertEquals((patternOnlyCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((patternOnlyCoverage \ "slots" \ 0 \ "semanticDetailTokensSatisfied").as[Boolean], false)
    assertEquals((ownedCoverage \ "matchedSlotCount").as[Int], 1)
    assertEquals((ownedCoverage \ "slots" \ 0 \ "causeLineageTokenCoLocationSatisfied").as[Boolean], true)

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

  test("line normalizer marks maintained defensive rook horizon as draw resource proof"):
    val startFen = "R7/4k3/7r/3KP3/8/8/8/8 b - - 0 1"
    val afterFen = PrincipalVariationEvidence.legalFenAfter(startFen, "h6a6").get
    val rawLine =
      PrincipalVariationEvidence.LineVariationRef(
        List(PrincipalVariationEvidence.LineMoveRef(1, "h6a6", afterFen))
      )
    val validated = PrincipalVariationEvidence.validatedLine(startFen, rawLine, "h6a6").get
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
        id = "line:philidor-draw-resource",
        lineRef = lineRef("philidor-draw-resource", "h6a6", 1, LineNodeRole.BestReference),
        facts = facts,
        position = position,
        scope = EvidenceScope.BestLine
      )
    val payload =
      record.payload match
        case payload: LineFactEvidence => payload
        case _                         => fail("expected line fact evidence")
    val philidor = payload.endgameTechniqueHorizons.find(_.pattern == "PhilidorDefense").get
    val drawResource = payload.proofSignalConsequencesOf(LineConsequenceKind.DrawResource).head

    assertEquals(philidor.status, LineEndgameTechniqueHorizonStatus.Active)
    assert(drawResource.rootMoveMatched("h6a6"), drawResource)
    assert(payload.rootOwnedEndgameTechniqueHorizons("h6a6", RelativeCauseKind.DrawResource).nonEmpty)

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

    assertEquals((coverage \ "slots" \ 0 \ "objectBound").as[Boolean], false)
    assertEquals((coverage \ "slots" \ 0 \ "terminalStage").as[String], "missing_semantic_slot")

  test("recognition view slots require semantic details to reach public surface"):
    val detailTokens =
      List(
        "unit:EndgameTechniqueRecipe",
        "pattern:Lucena",
        "rook-pattern:RookBehindPassedPawn",
        "requiredSquare:f8"
      )
    val publicSurfaceSignature =
      "unit=EndgameTechniqueRecipe|axis=none|kind=RookEndgameTechnique|support=view_surfaced|lane=current_move_function|causes=none|sources=line:rook-horizon|proof=DirectProof|carrier=true|boardCarrier=true|objects=target=Square:f8"
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
        moveMeaningClaimSurfaceSignatures = List(publicSurfaceSignature),
        publicMoveMeaningClaimSurfaceSignatures = List(publicSurfaceSignature)
      )
    val detailOnlyDiagnostic =
      diagnostic.copy(
        id = "cmp-rook-recognition-detail-only",
        moveJudgmentView = diagnostic.moveJudgmentView.copy(
          moveMeaningClaimSurfaceSignatures = Nil,
          publicMoveMeaningClaimDiagnostics = Nil,
          publicMoveMeaningClaimSurfaceSignatures = Nil
        )
      )
    val recognitionSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "rook-recognition-view",
        unit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe,
        requiredTerminalStage = Some("view_surfaced"),
        requiredSemanticDetailTokens = List("pattern:Lucena", "requiredSquare:f8")
      )
    val ownedSlot =
      recognitionSlot.copy(
        id = "rook-recognition-owned",
        requiredCauseKinds = List(RelativeCauseKind.ConversionSecured)
      )

    val coverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(recognitionSlot, ownedSlot), List(diagnostic))
    val detailOnlyCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(recognitionSlot), List(detailOnlyDiagnostic))

    assertEquals((coverage \ "matchedSlotCount").as[Int], 1)
    assertEquals((coverage \ "slots" \ 0 \ "terminalStage").as[String], "view_surfaced")
    assertEquals((coverage \ "slots" \ 0 \ "matched").as[Boolean], true)
    assertEquals((coverage \ "slots" \ 1 \ "matched").as[Boolean], false)
    assertEquals((detailOnlyCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((detailOnlyCoverage \ "slots" \ 0 \ "terminalStage").as[String], "semantic_detected")

  test("recognition view slots without token probes match public current move surface"):
    val publicSurfaceSignature =
      "unit=PieceRerouteRoute|axis=none|kind=PieceRoute|support=view_surfaced|lane=current_move_function|causes=none|sources=route-src|proof=none|carrier=true|boardCarrier=true|objects=target=Piece:knight"
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
        moveMeaningClaimSurfaceSignatures = List(publicSurfaceSignature),
        publicMoveMeaningClaimSurfaceSignatures = List(publicSurfaceSignature)
      )
    val recognitionSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "route-recognition-view",
        unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
        requiredTerminalStage = Some("view_surfaced")
      )
    val ownedSlot =
      recognitionSlot.copy(
        id = "route-recognition-owned",
        requiredTerminalStage = Some("owned_cause_linked")
      )

    val coverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(recognitionSlot, ownedSlot), List(diagnostic))

    assertEquals((coverage \ "slots" \ 0 \ "terminalStage").as[String], "view_surfaced")
    assertEquals((coverage \ "slots" \ 0 \ "matched").as[Boolean], true)
    assertEquals((coverage \ "slots" \ 1 \ "matched").as[Boolean], false)

  test("semantic rubric flags plan option owned-cause expectations without upgrading recognition"):
    val publicPlanSignature =
      "unit=PlanOptionSet|axis=none|kind=PlanContinuity|support=view_surfaced|lane=current_move_function|causes=none|sources=plan-src|proof=none|carrier=false|boardCarrier=false|objects=target=PlanSubject:pawnbreakpreparation"
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
        moveMeaningClaimSurfaceSignatures = List(publicPlanSignature),
        publicMoveMeaningClaimSurfaceSignatures = List(publicPlanSignature)
      )
    val slot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "minority-plan-owned",
        unit = PositionPlanTechniqueUnit.PlanOptionSet,
        requiredTerminalStage = Some("owned_cause_linked"),
        requiredSemanticDetailTokens = List("minorityAttack:true")
      )

    val coverage = MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(slot), List(diagnostic))

    assertEquals((coverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((coverage \ "planOptionOwnedCauseExpectationSlotIds").as[List[String]], List("minority-plan-owned"))
    assertEquals((coverage \ "viewOnlyOwnedCauseExpectationSlotIds").as[List[String]], Nil)
    assertEquals((coverage \ "classifiedMissingSlotIds").as[List[String]], List("minority-plan-owned"))
    assertEquals((coverage \ "unclassifiedMissingSlotIds").as[List[String]], Nil)
    assertEquals((coverage \ "slots" \ 0 \ "terminalStage").as[String], "claim_survived")
    assertEquals((coverage \ "slots" \ 0 \ "planOptionOwnedCauseExpectation").as[Boolean], true)
    assertEquals((coverage \ "slots" \ 0 \ "viewOnlyOwnedCauseExpectation").as[Boolean], false)

  test("recognition view slots require a public carrier signature"):
    val carrierlessPublicSurfaceSignature =
      "unit=PieceRerouteRoute|axis=none|kind=PieceRoute|support=view_surfaced|lane=current_move_function|lineRole=played|move=g1f3|causes=none|sources=none|proof=none|carrier=false|boardCarrier=false|objects=none"
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
        moveMeaningClaimSurfaceSignatures = List(carrierlessPublicSurfaceSignature),
        publicMoveMeaningClaimSurfaceSignatures = List(carrierlessPublicSurfaceSignature)
      )
    val recognitionSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "route-recognition-carrierless",
        unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
        requiredTerminalStage = Some("view_surfaced")
      )

    val coverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(recognitionSlot), List(diagnostic))

    assertEquals((coverage \ "slots" \ 0 \ "matched").as[Boolean], false)
    assertEquals((coverage \ "slots" \ 0 \ "viewSurfaced").as[Boolean], false)

  test("ownership slots accept reference-owned public pawn break without current-move overclaim"):
    val axis = "PawnBreak:Support:break-file-e-created-tension-e5-d4"
    val causeId = "cause-reference-break"
    val detailTokens =
      List("unit:TensionBreakPolicyRoute", "axisKind:PawnBreak", "breakFile:e", s"causeEvidenceId:$causeId")
    val publicReferenceOwnedSignature =
      s"unit=TensionBreakPolicyRoute|axis=$axis|kind=PawnBreakTiming|support=owned_cause_linked|lane=reference_or_opponent_resource|lineRole=reference|move=e6e5|causes=$causeId|carrier=true|boardCarrier=true|objects=target=File:e;mechanism=Mechanism:pawnbreak"
    val publicReferenceSoftSignature =
      s"unit=TensionBreakPolicyRoute|axis=$axis|kind=PawnBreakTiming|support=view_surfaced|lane=reference_or_opponent_resource|lineRole=reference|move=e6e5|causes=none|carrier=false|boardCarrier=false|objects=none"
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
        moveMeaningClaimSurfaceSignatures = List(publicReferenceOwnedSignature),
        publicMoveMeaningClaimSurfaceSignatures = List(publicReferenceOwnedSignature),
        primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot)
      )
    val softReferenceDiagnostic =
      diagnostic.copy(
        id = "cmp-reference-soft-pawn-break",
        moveJudgmentView = diagnostic.moveJudgmentView.copy(
          moveMeaningClaimSurfaceSignatures = List(publicReferenceSoftSignature),
          publicMoveMeaningClaimDiagnostics = publicMoveMeaningClaimDiagnostic(publicReferenceSoftSignature).toList,
          publicMoveMeaningClaimSurfaceSignatures = List(publicReferenceSoftSignature)
        )
      )
    val slot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "reference-owned-pawn-break",
        unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
        requiredTerminalStage = Some("owned_cause_linked"),
        requiredCauseKinds = List(RelativeCauseKind.PawnBreakOpportunity),
        requiredSemanticDetailTokens = List("axisKind:PawnBreak", "breakFile:e")
      )

    val coverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(slot), List(diagnostic))
    val softCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(slot), List(softReferenceDiagnostic))

    assertEquals((coverage \ "matchedSlotCount").as[Int], 1)
    assertEquals((coverage \ "slots" \ 0 \ "terminalStage").as[String], "clustered_coherent")
    assertEquals((coverage \ "slots" \ 0 \ "publicSurfaceClaimSignatures").as[List[String]], List(publicReferenceOwnedSignature))
    assertEquals((softCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((softCoverage \ "slots" \ 0 \ "terminalStage").as[String], "claim_survived")

  test("resource contest view slots require concrete target binding"):
    val semanticTokens =
      List(
        "unit:SpacePreventionResourceDenial",
        "space",
        "queenside",
        "resourceContestKind:CounterplayRestraint",
        "resourceContestSquare:a1"
      )
    val concreteDiagnostic =
      comparisonDiagnostic(
        id = "cmp-resource-contest-concrete",
        referenceLeadAxes = Nil,
        producedKinds = Nil,
        flows = Nil,
        primaryRootKinds = Nil,
        primaryRootIds = Nil,
        positionPlanTechniqueFrameIds = List("frame-resource-contest-concrete"),
        positionPlanTechniqueUnits = List(PositionPlanTechniqueUnit.SpacePreventionResourceDenial),
        positionPlanTechniqueSemanticDetailUnits = List(PositionPlanTechniqueUnit.SpacePreventionResourceDenial),
        positionPlanTechniqueSemanticDetailTokens = semanticTokens,
        positionPlanTechniqueSemanticDetailTokenGroups = List(semanticTokens),
        moveMeaningClaimSurfaceSignatures =
          List(
            "unit=SpacePreventionResourceDenial|axis=none|kind=CounterplayControl|support=view_surfaced|lane=current_move_function|causes=none|sources=resource-src|proof=DirectProof|carrier=true|boardCarrier=true|objects=target=Square:a1;mechanism=Mechanism:counterplayrestraint"
          ),
        publicMoveMeaningClaimSurfaceSignatures =
          List(
            "unit=SpacePreventionResourceDenial|axis=none|kind=CounterplayControl|support=view_surfaced|lane=current_move_function|causes=none|sources=resource-src|proof=DirectProof|carrier=true|boardCarrier=true|objects=target=Square:a1;mechanism=Mechanism:counterplayrestraint"
          )
      )
    val sideOnlyDiagnostic =
      concreteDiagnostic.copy(
        id = "cmp-resource-contest-side-only",
        moveJudgmentView = concreteDiagnostic.moveJudgmentView.copy(
          positionPlanTechniqueSemanticDetailTokens =
            semanticTokens.filterNot(_.startsWith("resourceContestSquare:")),
          positionPlanTechniqueSemanticDetailTokenGroups =
            List(semanticTokens.filterNot(_.startsWith("resourceContestSquare:"))),
          positionPlanTechniqueObjectBindingSignatures =
            List("target=Side:white|mechanism=Mechanism:counterplayrestraint")
        )
      )
    val partialTokenDiagnostic =
      concreteDiagnostic.copy(
        id = "cmp-resource-contest-partial-token",
        moveJudgmentView = concreteDiagnostic.moveJudgmentView.copy(
          positionPlanTechniqueSemanticDetailTokens =
            semanticTokens.filterNot(_.startsWith("resourceContestSquare:")) :+ "resourceContestSquare:a10",
          positionPlanTechniqueSemanticDetailTokenGroups =
            List(semanticTokens.filterNot(_.startsWith("resourceContestSquare:")) :+ "resourceContestSquare:a10"),
          positionPlanTechniqueObjectBindingSignatures =
            List("target=Square:a10|mechanism=Mechanism:counterplayrestraint")
        )
      )
    val concreteSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "queenside-space-resource-view",
        unit = PositionPlanTechniqueUnit.SpacePreventionResourceDenial,
        requiredTerminalStage = Some("view_surfaced"),
        requiredSemanticDetailTokens = List("queenside", "space", "resourceContest"),
        requiredObjectBindingTokens = List("target=Square:a1")
      )
    val emptyTargetSlot =
      concreteSlot.copy(
        id = "queenside-space-resource-view-empty-target",
        requiredObjectBindingTokens = List("target=")
      )

    val concreteCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(concreteSlot), List(concreteDiagnostic))
    val sideOnlyCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(concreteSlot), List(sideOnlyDiagnostic))
    val partialTokenCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(concreteSlot), List(partialTokenDiagnostic))
    val emptyTargetCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(List(emptyTargetSlot), List(concreteDiagnostic))

    assertEquals((concreteCoverage \ "matchedSlotCount").as[Int], 1)
    assertEquals((sideOnlyCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((partialTokenCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((emptyTargetCoverage \ "matchedSlotCount").as[Int], 0)

  test("semantic rubric detail tokens match role values and declared prefixes, not substrings"):
    val iqpSlot =
      MoveReviewPhase3AuditRunner.ExpectedSemanticSlot(
        id = "iqp-motif",
        unit = PositionPlanTechniqueUnit.StructuralTransformation,
        requiredTerminalStage = Some("view_surfaced"),
        requiredSemanticDetailTokens = List("IQP")
      )
    val routeSubjectSlot =
      iqpSlot.copy(
        id = "bishop-route-subject",
        requiredSemanticDetailTokens = List("structuralPurposeSubject:bishop")
      )
    val exactIqpCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(iqpSlot),
        List(
          diagnosticWithDetailTokens(
            "structural-motif-iqp",
            List("unit:StructuralTransformation", "structuralMotif:iqp"),
            RelativeCauseKind.StructuralImprovement,
            unit = PositionPlanTechniqueUnit.StructuralTransformation
          )
        )
      )
    val substringCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(iqpSlot),
        List(
          diagnosticWithDetailTokens(
            "substring-iqp",
            List("unit:StructuralTransformation", "notIQP:true", "iqpTransition:present"),
            RelativeCauseKind.StructuralImprovement,
            unit = PositionPlanTechniqueUnit.StructuralTransformation
          )
        )
      )
    val subjectPrefixCoverage =
      MoveReviewPhase3AuditRunner.semanticRubricExpectedSlotCoverageJson(
        List(routeSubjectSlot),
        List(
          diagnosticWithDetailTokens(
            "bishop-route-subject",
            List("unit:StructuralTransformation", "structuralPurposeSubject:bishop:f8-e7"),
            RelativeCauseKind.StructuralImprovement,
            unit = PositionPlanTechniqueUnit.StructuralTransformation
          )
        )
      )

    assertEquals((exactIqpCoverage \ "matchedSlotCount").as[Int], 1)
    assertEquals((substringCoverage \ "matchedSlotCount").as[Int], 0)
    assertEquals((substringCoverage \ "semanticTokenExpectationMissSlotIds").as[List[String]], List("iqp-motif"))
    assertEquals((substringCoverage \ "classifiedMissingSlotIds").as[List[String]], List("iqp-motif"))
    assertEquals((substringCoverage \ "unclassifiedMissingSlotIds").as[List[String]], Nil)
    assertEquals((substringCoverage \ "slots" \ 0 \ "semanticTokenExpectationMiss").as[Boolean], true)
    assertEquals((subjectPrefixCoverage \ "matchedSlotCount").as[Int], 1)

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

  test("audit view json exposes pawn tension edges and counter-break files"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val frame =
      PositionPlanTechniqueFrame(
        id = "frame-pawn-break-detail",
        units = List(PositionPlanTechniqueUnit.TensionBreakPolicyRoute),
        position = root,
        line = Some(candidateLine),
        moveUci = Some("e2e4"),
        scope = EvidenceScope.PlayedTransition,
        mechanismKinds = List(StrategicMechanismKind.PawnStructure),
        strategicAxisKeys = List("PawnBreak:Support:break-file-e-maintain-d4-e5"),
        semanticAnchors = Nil,
        objectBindingSignatures = Nil,
        semanticDetails = List(
          PositionPlanTechniqueSemanticDetail(
            unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
            axisKey = Some("PawnBreak:Support:break-file-e-maintain-d4-e5"),
            axisKind = Some(StrategicAxisKind.PawnBreak),
            axisPolarity = Some(StrategicAxisPolarity.Support),
            label = Some("break-file-e-maintain-d4-e5"),
            breakFile = Some("e"),
            tensionPolicy = Some("Maintain"),
            tensionSquares = List("d4", "e5"),
            tensionEdges = List("d4-e5"),
            counterBreakFiles = List("c")
          )
        ),
        evidenceIds = List("pawn-structure"),
        mechanismEvidenceIds = List("pawn-structure"),
        sourceEvidenceIds = List("pawn-structure"),
        relativeCauseEvidenceIds = Nil,
        ideaIds = Nil,
        claimIds = Nil,
        planComparison = None,
        relationToVerdict = None,
        confidence = EvidenceConfidence.Mixed,
        salience = 1
      )

    val json = MoveReviewPhase3AuditViewJson.positionPlanTechniqueFrameJson(frame, ref => Json.obj("id" -> ref.id))
    val detail = json \ "semanticDetails" \ 0

    assertEquals((detail \ "tensionEdges").as[List[String]], List("d4-e5"))
    assertEquals((detail \ "counterBreakFiles").as[List[String]], List("c"))

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

  test("binding width audit reports broad and colocated graph bindings"):
    val view = MoveJudgmentView(
      verdict = None,
      verdictCarriers = Nil,
      causeAudit = MoveJudgmentCauseAudit(
        primary = List(
          causeFrame(
            causeId = "cause-side-only",
            axisKeys = List("Counterplay:Restrain:opponent-low-mobility"),
            objectSignatures = List(
              "target=Side:black|mechanism=Mechanism:counterplayrestraint|consequence=Consequence:counterplayrestraint|proof=DirectProof"
            ),
            rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.FallbackRoot
          ),
          causeFrame(
            causeId = "cause-context-only",
            axisKeys = List("Activity:Gain:outpost-gain"),
            objectSignatures = List(
              "target=Square:e5|mechanism=Mechanism:activity|consequence=Consequence:activity|proof=ContextSupport"
            ),
            rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ContextOnly
          ),
          causeFrame(
            causeId = "cause-outpost-direct",
            axisKeys = List("Activity:Gain:outpost-gain"),
            objectSignatures = List(
              "target=Square:d5|mechanism=Mechanism:activity|consequence=Consequence:activity|proof=DirectProof"
            ),
            rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
          ),
          causeFrame(
            causeId = "cause-colocated",
            axisKeys = Nil,
            objectSignatures = Nil,
            witnessBindingLevel = MoveJudgmentCauseWitnessBindingLevel.SameComparisonOnly
          )
        )
      ),
      supportContextClusterIds = Nil,
      overriddenLocalIdeas = Nil,
      preservedLocalIdeas = Nil
    )

    val audit = MoveReviewPhase3AuditRunner.bindingWidthAuditJson(view)

    assertEquals((audit \ "sideOnlyTargetFrameCount").as[Int], 1)
    assertEquals((audit \ "contextSupportOnlyBindingFrameCount").as[Int], 1)
    assertEquals((audit \ "playerFacingObjectReadyFrameCount").as[Int], 1)
    assertEquals((audit \ "weakConcreteReadyFrameCount").as[Int], 2)
    assertEquals((audit \ "axisWithMultipleObjectFingerprintsCount").as[Int], 1)
    assertEquals((audit \ "sameComparisonOnlyBindingFrameCount").as[Int], 1)
    assertEquals((audit \ "witnessBindingLevelCounts" \ "SameComparisonOnly").as[Int], 1)
    assertEquals((audit \ "rootArbitrationTierCounts" \ "ExactOwnedRoot").as[Int], 1)
    assertEquals((audit \ "rootArbitrationTierCounts" \ "FallbackRoot").as[Int], 1)
    assertEquals((audit \ "rootArbitrationTierCounts" \ "ContextOnly").as[Int], 2)

  test("move meaning claims surface concrete played-move reasons without praise wording"):
    val cause = causeFrame(
      causeId = "cause-break",
      axisKeys = List("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
      objectSignatures = List("target=Square:e3|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    ).copy(
      role = MoveJudgmentCauseFrameRole.ContextCause,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("break-file-e-created-tension-e3-d4"),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateStronger),
      breakFile = Some("e"),
      tensionPolicy = Some("maintain"),
      tensionSquares = List("d4", "e3"),
      tensionEdges = List("e3-d4"),
      structuralPurposeSubjects = List("created-tension:e3-d4"),
      structuralPurposeConsequences = List("PawnTensionGain"),
      candidateEvidenceIds = List("structural-delta:played:e2e3:tension"),
      sourceEvidenceIds = List("structural-delta:played:e2e3:tension"),
      causeEvidenceIds = List("cause-break"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List("target=Square:e3|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail)
    )

    val claims = view.moveMeaningClaims
    assertEquals(claims.map(_.meaningKind), List("PawnBreakTiming"))
    assertEquals(claims.head.role, "PreparesBreak")
    assertEquals(claims.head.supportLevel, "owned_cause_linked")
    assertEquals(claims.head.visibility, "reason_grade")
    assertEquals(claims.head.surfaceLane, "current_move_owned")
    assert(claims.head.laneKey.contains("breakFile=e"), claims.head.laneKey)
    assert(claims.head.reasonTokens.contains("breakFile:e"))
    assert(claims.head.reasonTokens.contains("tensionSquare:d4"))
    assert(claims.head.reasonTokens.contains("tensionSquare:e3"))
    assert(claims.head.reasonTokens.contains("contrastOutcome:CandidateStronger"))

  test("move meaning claims use candidate evidence as current-move carrier source"):
    val cause = causeFrame(
      causeId = "cause-break-candidate-source",
      axisKeys = List("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
      objectSignatures = List("target=Square:e3|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    ).copy(
      role = MoveJudgmentCauseFrameRole.ContextCause,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("break-file-e-created-tension-e3-d4"),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateStronger),
      breakFile = Some("e"),
      tensionPolicy = Some("maintain"),
      tensionSquares = List("d4", "e3"),
      tensionEdges = List("e3-d4"),
      structuralPurposeSubjects = List("created-tension:e3-d4"),
      structuralPurposeConsequences = List("PawnTensionGain"),
      candidateEvidenceIds = List("structural-delta:played:e2e3:tension"),
      sourceEvidenceIds = List("pawn-structure:before"),
      causeEvidenceIds = List("cause-break-candidate-source"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List("target=Square:e3|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims.map(_.meaningKind), List("PawnBreakTiming"))
    assertEquals(view.moveMeaningClaims.map(_.supportLevel), List("owned_cause_linked"))
    assertEquals(view.moveMeaningClaims.map(_.surfaceLane), List("current_move_owned"))
    assertEquals(view.moveMeaningClaims.head.sourceEvidenceIds, List("structural-delta:played:e2e3:tension"))

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

  test("move meaning claims classify newly created pawn tension as preparation"):
    val createdTensionPosition =
      PositionNodeRef("8/8/8/8/3p4/8/4P3/4K3 w - - 0 1", 1, Some(chess.Color.White), Some("created-tension-root"))
    def detail(policy: String) =
      PositionPlanTechniqueSemanticDetail(
        unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
        axisKey = Some("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
        axisKind = Some(StrategicAxisKind.PawnBreak),
        axisPolarity = Some(StrategicAxisPolarity.Support),
        label = Some("break-file-e-created-tension-e3-d4"),
        breakFile = Some("e"),
        tensionPolicy = Some(policy),
        tensionSquares = List("e3", "d4"),
        tensionEdges = List("e3-d4"),
        structuralPurposeSubjects = List("created-tension:e3-d4"),
        candidateEvidenceIds = List("structural-delta:played:e2e3:tension"),
        sourceEvidenceIds = List("structural-delta:played:e2e3:tension"),
        objectBindingSignatures =
          List("actor=Move:e2e3|target=Square:e3|target=Square:d4|mechanism=Mechanism:pawn-break|proof=DirectProof"),
        specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
      )
    val details = List(detail("release"), detail("maintain"))
    val base = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = details
    )
    val view =
      meaningClaimViewWithClaims(
        base.copy(
          positionPlanTechniqueFrames =
            List(meaningClaimPlanTechniqueFrame(details).copy(position = createdTensionPosition))
        )
      )
    val claims = view.moveMeaningClaims.filter(_.meaningKind == "PawnBreakTiming")

    assert(claims.nonEmpty, view.moveMeaningClaims)
    assert(claims.forall(_.role == "PreparesBreak"), claims)

  test("move meaning claims do not borrow another file's pawn break as current move timing"):
    def detail(breakFile: String, axisKey: String) =
      PositionPlanTechniqueSemanticDetail(
        unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
        axisKey = Some(axisKey),
        axisKind = Some(StrategicAxisKind.PawnBreak),
        axisPolarity = Some(StrategicAxisPolarity.Support),
        label = Some(axisKey.stripPrefix("PawnBreak:Support:")),
        breakFile = Some(breakFile),
        tensionPolicy = Some("prepare"),
        tensionSquares = List("e3", "d4"),
        tensionEdges = List("e3-d4"),
        structuralPurposeSubjects = List("created-tension:e3-d4"),
        candidateEvidenceIds = List(s"structural-delta:played:e2e3:$breakFile-break"),
        sourceEvidenceIds = List(s"structural-delta:played:e2e3:$breakFile-break"),
        objectBindingSignatures =
          List(s"actor=Move:e2e3|target=File:$breakFile|target=Square:e3|target=Square:d4|mechanism=Mechanism:pawn-break|proof=DirectProof"),
        specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
      )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(
        detail("e", "PawnBreak:Support:break-file-e-created-tension-e3-d4"),
        detail("d", "PawnBreak:Support:break-file-d-maintain-d4-e3")
      )
    )
    val claims = view.moveMeaningClaims.filter(_.meaningKind == "PawnBreakTiming")

    assert(claims.exists(_.reasonTokens.contains("breakFile:e")), claims)
    assert(!claims.exists(claim => claim.surfaceLane.startsWith("current_move") && claim.reasonTokens.contains("breakFile:d")), claims)

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

  test("move meaning claims do not borrow alternative pawn break detail as current timing"):
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:break-file-e-release-e2-e3-e3-e2"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("break-file-e-release-e2-e3-e3-e2"),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceOnly),
      breakFile = Some("e"),
      tensionSquares = List("e2", "e3"),
      tensionEdges = List("e2-e3", "e3-e2"),
      counterBreakFiles = List("d"),
      candidateEvidenceIds = List("structural-delta:alternative:g1f3"),
      sourceEvidenceIds = List("pawn-structure:after-alternative:g1f3"),
      causeEvidenceIds = List("cause-alternative-break"),
      objectBindingSignatures =
        List(
          "target=File:d|target=File:e|target=Square:e2|target=Square:e3|mechanism=Mechanism:pawnbreak|witness=Move:e2e3|proof=DirectProof"
        ),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(
        causeFrame(
          causeId = "cause-alternative-break",
          axisKeys = List("PawnBreak:Support:break-file-e-release-e2-e3-e3-e2"),
          objectSignatures = List("target=File:e|target=Square:e2|target=Square:e3"),
          causeKind = RelativeCauseKind.PawnBreakOpportunity,
          rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
        ).copy(hasOwnedAdmissibleLongTermProof = true)
      ),
      details = List(detail)
    )
    val claims = view.moveMeaningClaims.filter(_.meaningKind == "PawnBreakTiming")

    assert(!claims.exists(_.surfaceLane.startsWith("current_move")), claims)

  test("move meaning claims keep pawn break provenance instead of hiding it through object dedupe"):
    def detail(causeId: String, sourceId: String) =
      PositionPlanTechniqueSemanticDetail(
        unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
        axisKey = Some("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
        axisKind = Some(StrategicAxisKind.PawnBreak),
        axisPolarity = Some(StrategicAxisPolarity.Support),
        label = Some("break-file-e-created-tension-e3-d4"),
        breakFile = Some("e"),
        tensionSquares = List("e3", "d4"),
        tensionEdges = List("e3-d4"),
        structuralPurposeSubjects = List("created-tension:e3-d4"),
        candidateEvidenceIds = List(sourceId),
        sourceEvidenceIds = List(sourceId),
        causeEvidenceIds = List(causeId),
        objectBindingSignatures =
          List("actor=Move:e2e3|target=File:e|target=Square:e3|target=Square:d4|mechanism=Mechanism:pawn-break|proof=DirectProof"),
        specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
      )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(
        causeFrame(
          causeId = "cause-pawn-break-a",
          axisKeys = List("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
          objectSignatures = List("actor=Move:e2e3|target=Square:e3|target=Square:d4"),
          causeKind = RelativeCauseKind.PawnBreakOpportunity,
          rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
        ).copy(hasOwnedAdmissibleLongTermProof = true),
        causeFrame(
          causeId = "cause-pawn-break-b",
          axisKeys = List("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
          objectSignatures = List("actor=Move:e2e3|target=Square:e3|target=Square:d4"),
          causeKind = RelativeCauseKind.PawnBreakOpportunity,
          rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
        ).copy(hasOwnedAdmissibleLongTermProof = true)
      ),
      details = List(
        detail("cause-pawn-break-a", "structural-delta:played:e2e3:tension-a"),
        detail("cause-pawn-break-b", "structural-delta:played:e2e3:tension-b")
      )
    )
    val claims = view.moveMeaningClaims.filter(_.meaningKind == "PawnBreakTiming")

    assertEquals(claims.size, 1)
    assertEquals(claims.head.causeEvidenceIds.toSet, Set("cause-pawn-break-a", "cause-pawn-break-b"))
    assertEquals(
      claims.head.sourceEvidenceIds.toSet,
      Set("structural-delta:played:e2e3:tension-a", "structural-delta:played:e2e3:tension-b")
    )

  test("move meaning claims classify captured existing pawn tension as release"):
    val releaseLine = lineRef("played-release", "e2d3", 2, LineNodeRole.Played)
    val releasePosition =
      PositionNodeRef("8/8/8/8/8/3p4/4P3/4K3 w - - 0 1", 1, Some(chess.Color.White), Some("release-root"))
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Release:break-file-e-resolved-tension-e2-d3"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Release),
      label = Some("break-file-e-resolved-tension-e2-d3"),
      breakFile = Some("e"),
      tensionPolicy = Some("release"),
      tensionSquares = List("e2", "d3"),
      tensionEdges = List("e2-d3"),
      structuralPurposeSubjects = List("resolved-tension:e2-d3"),
      candidateEvidenceIds = List("structural-delta:played:e2d3:tension"),
      sourceEvidenceIds = List("structural-delta:played:e2d3:tension"),
      objectBindingSignatures =
        List("actor=Move:e2d3|target=Square:e2|target=Square:d3|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val base = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )
    val view =
      meaningClaimViewWithClaims(
        base.copy(
          verdict = base.verdict.map(_.copy(candidateLine = releaseLine)),
          positionPlanTechniqueFrames =
            List(meaningClaimPlanTechniqueFrame(List(detail)).copy(position = releasePosition, line = Some(releaseLine), moveUci = Some(releaseLine.rootMove)))
        )
      )
    val claims = view.moveMeaningClaims.filter(_.meaningKind == "PawnBreakTiming")

    assert(claims.exists(_.role == "ReleasesPawnTension"), claims)

  test("move meaning claims classify pawn advances out of existing tension as release"):
    val releaseLine = lineRef("played-advance-release", "c4c5", 2, LineNodeRole.Played)
    val releasePosition =
      PositionNodeRef("8/8/8/3p4/2P5/8/8/4K3 w - - 0 1", 1, Some(chess.Color.White), Some("advance-release-root"))
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Release:break-file-c-resolved-tension-c4-d5"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Release),
      label = Some("break-file-c-resolved-tension-c4-d5"),
      breakFile = Some("c"),
      tensionPolicy = Some("release"),
      tensionSquares = List("c4", "d5"),
      tensionEdges = List("c4-d5"),
      structuralPurposeSubjects = List("resolved-tension:c4-d5"),
      candidateEvidenceIds = List("structural-delta:played:c4c5:tension"),
      sourceEvidenceIds = List("structural-delta:played:c4c5:tension"),
      objectBindingSignatures =
        List("actor=Move:c4c5|target=Square:c4|target=Square:d5|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val base = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )
    val view =
      meaningClaimViewWithClaims(
        base.copy(
          verdict = base.verdict.map(_.copy(candidateLine = releaseLine)),
          positionPlanTechniqueFrames =
            List(meaningClaimPlanTechniqueFrame(List(detail)).copy(position = releasePosition, line = Some(releaseLine), moveUci = Some(releaseLine.rootMove)))
        )
      )
    val claims = view.moveMeaningClaims.filter(_.meaningKind == "PawnBreakTiming")

    assert(claims.exists(_.role == "ReleasesPawnTension"), claims)

  test("move meaning claims preserve compatible pawn break lanes"):
    val eCause = causeFrame(
      causeId = "cause-e-break",
      axisKeys = List("PawnBreak:Support:e-break"),
      objectSignatures = List("target=File:e|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    ).copy(
      role = MoveJudgmentCauseFrameRole.ContextCause,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true
    )
    val cCause = causeFrame(
      causeId = "cause-c-break",
      axisKeys = List("PawnBreak:Support:c-break"),
      objectSignatures = List("target=File:c|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    ).copy(
      role = MoveJudgmentCauseFrameRole.ContextCause,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true
    )
    def breakDetail(causeId: String, edgeSquare: String) =
      val axisKey = s"PawnBreak:Support:break-file-e-created-tension-e3-$edgeSquare"
      PositionPlanTechniqueSemanticDetail(
        unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
        axisKey = Some(axisKey),
        axisKind = Some(StrategicAxisKind.PawnBreak),
        axisPolarity = Some(StrategicAxisPolarity.Support),
        label = Some(axisKey.stripPrefix("PawnBreak:Support:")),
        contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateStronger),
        breakFile = Some("e"),
        tensionPolicy = Some("prepare"),
        tensionSquares = List("e3", edgeSquare),
        tensionEdges = List(s"e3-$edgeSquare"),
        structuralPurposeSubjects = List(s"created-tension:e3-$edgeSquare"),
        structuralPurposeConsequences = List("PawnTensionGain"),
        candidateEvidenceIds = List(s"structural-delta:played:e2e3:$edgeSquare-break"),
        sourceEvidenceIds = List(s"structural-delta:played:e2e3:$edgeSquare-break"),
        causeEvidenceIds = List(causeId),
        proofRoles = List(RelativeCauseProofRole.DirectProof),
        objectBindingSignatures = List(s"actor=Move:e2e3|target=Square:e3|target=Square:$edgeSquare|mechanism=Mechanism:pawn-break|proof=DirectProof"),
        specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
      )

    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(eCause, cCause),
      details = List(
        breakDetail("cause-e-break", "d4"),
        breakDetail("cause-c-break", "f4")
      )
    )

    assertEquals(view.moveMeaningClaims.map(_.role), List("PreparesBreak", "PreparesBreak"))
    assertEquals(view.moveMeaningClaims.map(_.laneKey).distinct.size, 2)
    assertEquals(view.moveMeaningClaims.map(_.meaningKind), List("PawnBreakTiming", "PawnBreakTiming"))

  test("move meaning claims can use owned contrast support for exact good moves"):
    val alternativeLine = lineRef("alt", "d2d4", 3, LineNodeRole.Alternative)
    val cause = causeFrame(
      causeId = "cause-alt-activity",
      axisKeys = List("Activity:Gain:activity-gain"),
      objectSignatures =
        List(
          "actor=Piece:knight|actor=Square:g1|target=Square:f3|mechanism=Mechanism:developmentchoice|proof=DirectProof"
        )
    ).copy(
      role = MoveJudgmentCauseFrameRole.ContextCause,
      causeKind = RelativeCauseKind.ActivityGain,
      comparisonKind = CandidateComparisonKind.PlayedVsAlternative,
      causeRole = RelativeCauseRole.PlayedAlternativeContext,
      causeSourceSide = RelativeCauseSourceSide.Candidate,
      causeImportance = RelativeCauseImportance.Context,
      attributionKind = CauseAttributionKind.CandidateCreatesValue,
      referenceLine = alternativeLine,
      candidateLine = candidateLine,
      eventLine = candidateLine,
      eventRootMove = candidateLine.rootMove,
      hasOwnedAdmissibleLongTermProof = true,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot,
      narrativeRole = MoveJudgmentCauseNarrativeRole.ContextCause
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
      causeEvidenceIds = List("cause-alt-activity"),
      proofRoles = List(RelativeCauseProofRole.DirectProof, RelativeCauseProofRole.ContrastProof),
      objectBindingSignatures =
        List(
          "actor=Piece:knight|actor=Square:g1|target=Square:f3|mechanism=Mechanism:developmentchoice|proof=DirectProof"
        ),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("piece:knight:g1-f3")
    )

    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims.map(_.meaningKind), List("PieceActivity"))
    assertEquals(view.moveMeaningClaims.head.supportLevel, "view_surfaced")
    assertEquals(view.moveMeaningClaims.head.visibility, "functional_explanation")
    assertEquals(view.moveMeaningClaims.head.causeEvidenceIds, Nil)
    assert(!view.moveMeaningClaims.head.reasonTokens.contains("causeEvidenceId:cause-alt-activity"))
    assert(!view.moveMeaningClaims.head.reasonTokens.exists(_.startsWith("routeCarrier:")))

  test("move meaning claims preserve route tagged current-move piece routes"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:developmentchoice|consequence=Consequence:developmentpieceactivated|proof=DirectProof"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      candidateEvidenceIds = List("played-transition"),
      referenceEvidenceIds = List("reference-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "structural-delta:reference:e2e3", "played-transition", "reference-transition"),
      proofRoles = List(RelativeCauseProofRole.DirectProof, RelativeCauseProofRole.ContrastProof),
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralRouteRole = Some("Played"),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.SharedSustained),
      structuralPurposeSubjects = List("knight:e2-e3", "knight:e2-e3:center+1", "knight:e2-e3:mobility+2"),
      structuralPurposeConsequences = List("DevelopmentPieceActivated", "DevelopmentMobilityGain"),
      structuralPurposeCategories = List("PieceActivity", "Development"),
      structuralMotifTags = List("piece", "reroute", "route")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims.map(_.meaningKind), List("PieceRoute"))
    assertEquals(view.moveMeaningClaims.map(_.supportLevel), List("view_surfaced"))
    assertEquals(view.moveMeaningClaims.map(_.surfaceLane), List("current_move_function"))
    assertEquals(view.moveMeaningClaims.map(_.lineRole), List("candidate"))
    assertEquals(view.moveMeaningClaims.head.causeEvidenceIds, Nil)
    assert(view.moveMeaningClaims.head.reasonTokens.contains("routePiece:knight"))
    assert(view.moveMeaningClaims.head.reasonTokens.contains("routeFrom:e2"))
    assert(view.moveMeaningClaims.head.reasonTokens.contains("routeTo:e3"))
    assert(view.moveMeaningClaims.head.reasonTokens.contains("routeCarrier:reroute"))

  test("move meaning claims own same-root subject route carriers without route object signatures"):
    val routeSubjectOnlySignature =
      "actor=Move:e2e3|target=Square:e3|mechanism=Mechanism:developmentchoice|proof=DirectProof"
    val routeCause = causeFrame(
      causeId = "cause-subject-route",
      axisKeys = List("Activity:Gain:activity-gain"),
      objectSignatures = List(routeSubjectOnlySignature),
      causeKind = RelativeCauseKind.ActivityGain,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    ).copy(hasOwnedAdmissibleLongTermProof = true)
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      candidateEvidenceIds = List("structural-delta:played:e2e3"),
      sourceEvidenceIds = List("structural-delta:played:e2e3"),
      causeEvidenceIds = List("cause-subject-route"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(routeSubjectOnlySignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralRouteRole = Some("Played"),
      structuralPurposeSubjects = List("knight:e2-e3:maneuver"),
      structuralPurposeConsequences = List("DevelopmentPieceActivated", "MobilityLoss"),
      structuralPurposePolarities = List("Gain", "Loss"),
      structuralPurposeCategories = List("PieceActivity", "Development"),
      structuralMotifTags = List("piece", "reroute", "route")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(routeCause),
      details = List(detail)
    )
    val claim = view.moveMeaningClaims.head

    assertEquals(claim.meaningKind, "PieceRoute")
    assertEquals(claim.supportLevel, "owned_cause_linked")
    assertEquals(claim.surfaceLane, "current_move_owned")
    assertEquals(claim.role, "ImprovesPieceRoute")
    assertEquals(claim.causeEvidenceIds, List("cause-subject-route"))
    assert(claim.reasonTokens.contains("routePiece:knight"))
    assert(claim.reasonTokens.contains("routeFrom:e2"))
    assert(claim.reasonTokens.contains("routeTo:e3"))

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

  test("move meaning claims preserve same-root break, counter-break, and line-unlock siblings"):
    val breakSignature =
      "actor=Move:e2e3|target=File:e|target=Square:e3|target=Square:d4|mechanism=Mechanism:pawn-break|proof=DirectProof"
    val counterBreakSignature =
      "actor=Move:e2e3|target=File:e|target=File:c|mechanism=Mechanism:pawnbreak|consequence=Consequence:counterbreak-race|proof=DirectProof"
    val lineUnlockSignature =
      "actor=Move:e2e3|actor=Piece:queen|actor=Square:d1|target=Square:d1|mechanism=Mechanism:line-unlock|consequence=Consequence:lineunlockgain|proof=DirectProof"
    val breakCause = causeFrame(
      causeId = "cause-e-break",
      axisKeys = List("PawnBreak:Release:break-file-e-resolved-tension-e3-d4"),
      objectSignatures = List(breakSignature),
      causeKind = RelativeCauseKind.PawnBreakOpportunity,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    ).copy(hasOwnedAdmissibleLongTermProof = true)
    val counterBreakCause = causeFrame(
      causeId = "cause-counter-break",
      axisKeys = List("Counterplay:Restrain:defensive-counter-break-c"),
      objectSignatures = List("mechanism=Mechanism:counterplayrestraint|proof=DirectProof"),
      causeKind = RelativeCauseKind.OpponentRestriction,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    ).copy(hasOwnedAdmissibleLongTermProof = true)
    val lineUnlockCause = causeFrame(
      causeId = "cause-line-unlock",
      axisKeys = List("Activity:Gain:activity-gain"),
      objectSignatures = List(lineUnlockSignature),
      causeKind = RelativeCauseKind.ActivityGain,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    ).copy(hasOwnedAdmissibleLongTermProof = true)
    val breakDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Release:break-file-e-resolved-tension-e3-d4"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Release),
      label = Some("break-file-e-resolved-tension-e3-d4"),
      breakFile = Some("e"),
      tensionPolicy = Some("release"),
      tensionSquares = List("e3", "d4"),
      tensionEdges = List("e3-d4"),
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("break-file:e", "resolved-tension:e3-d4"),
      structuralPurposeConsequences = List("PawnTensionResolution"),
      candidateEvidenceIds = List("structural-delta:played:e2e3"),
      sourceEvidenceIds = List("structural-delta:played:e2e3"),
      causeEvidenceIds = List("cause-e-break"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(breakSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val counterBreakDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.SpacePreventionResourceDenial,
      axisKey = Some("Counterplay:Restrain:defensive-counter-break-c"),
      axisKind = Some(StrategicAxisKind.Counterplay),
      axisPolarity = Some(StrategicAxisPolarity.Restrain),
      label = Some("defensive-counter-break-c"),
      breakFile = Some("e"),
      counterBreakFiles = List("c"),
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("break-file:e", "resolved-tension:e3-d4"),
      structuralPurposeConsequences = List("PawnTensionResolution"),
      candidateEvidenceIds = List("structural-delta:played:e2e3"),
      sourceEvidenceIds = List("structural-delta:played:e2e3"),
      causeEvidenceIds = List("cause-counter-break"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(counterBreakSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val lineUnlockDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("queen:d1:line-unlock:by:e2e3:mobility+1"),
      structuralPurposeConsequences = List("LineUnlockGain"),
      structuralMotifTags = List("piece", "reroute", "route"),
      candidateEvidenceIds = List("structural-delta:played:e2e3"),
      sourceEvidenceIds = List("structural-delta:played:e2e3"),
      causeEvidenceIds = List("cause-line-unlock"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(lineUnlockSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )

    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(breakCause, counterBreakCause, lineUnlockCause),
      details = List(breakDetail, counterBreakDetail, lineUnlockDetail)
    )
    val claims = view.moveMeaningClaims

    assertEquals(claims.map(_.meaningKind).toSet, Set("PawnBreakTiming", "CounterplayControl", "PieceRoute"))
    assert(claims.forall(_.moveUci == candidateLine.rootMove), claims)
    assert(claims.forall(_.surfaceLane == "current_move_owned"), claims)
    assert(claims.exists(claim => claim.meaningKind == "CounterplayControl" && claim.reasonTokens.contains("counterBreakFile:c")))
    assert(claims.exists(claim => claim.meaningKind == "PieceRoute" && claim.reasonTokens.exists(_.contains("line-unlock"))))

  test("move meaning claims keep rook-lift subjects as concrete route carriers"):
    val rookLiftSignature =
      "actor=Move:e2e3|actor=Piece:rook|actor=Square:e2|target=Square:e3|mechanism=Mechanism:developmentchoice|consequence=Consequence:developmentpieceactivated|proof=DirectProof"
    val mixedActivityLossSignature =
      "actor=Move:e2e3|actor=Side:white|actor=Square:e2|mechanism=Mechanism:mobilityloss|consequence=Consequence:mobilityloss:loss|proof=DirectProof"
    val rookLiftCause = causeFrame(
      causeId = "cause-rook-lift",
      axisKeys = List("Activity:Gain:activity-gain"),
      objectSignatures = List(rookLiftSignature),
      causeKind = RelativeCauseKind.ActivityGain,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    ).copy(hasOwnedAdmissibleLongTermProof = true)
    val rookLiftDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("rook-lift:e2-e3:rank-3"),
      structuralPurposeConsequences = List("RookLiftActivation", "FileOccupationGain"),
      structuralMotifTags = List("file", "iqp", "piece", "reroute", "route"),
      candidateEvidenceIds = List("structural-delta:played:e2e3"),
      sourceEvidenceIds = List("structural-delta:played:e2e3"),
      causeEvidenceIds = List("cause-rook-lift"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(rookLiftSignature, mixedActivityLossSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )

    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(rookLiftCause),
      details = List(rookLiftDetail)
    )
    val claim = view.moveMeaningClaims.head

    assertEquals(claim.meaningKind, "PieceRoute")
    assertEquals(claim.role, "ImprovesPieceRoute")
    assertEquals(claim.surfaceLane, "current_move_owned")
    assert(claim.reasonTokens.contains("routePiece:rook"))
    assert(claim.reasonTokens.contains("routeFrom:e2"))
    assert(claim.reasonTokens.contains("routeTo:e3"))
    assert(claim.reasonTokens.contains("routeCarrier:rook-lift"))

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

  test("move meaning public surface keeps generic piece activity separate from piece route"):
    val signature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:developmentchoice|consequence=Consequence:developmentpieceactivated"
    val claim = MoveMeaningClaim(
      meaningKind = "PieceActivity",
      role = "ImprovesPieceActivity",
      laneKey = "kind=PieceActivity|axis=Activity:Gain:activity-gain|object=target=Square:e3",
      conflictKey = None,
      supportLevel = "view_surfaced",
      visibility = "functional_explanation",
      surfaceLane = "current_move_function",
      lineRole = "candidate",
      moveUci = "e2e3",
      frameId = "frame-generic-development",
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      causeKinds = Nil,
      causeSourceSides = Nil,
      causeEvidenceIds = Nil,
      sourceEvidenceIds = List("played-transition"),
      objectBindingSignatures = List(signature),
      reasonTokens = List(s"objectBinding:$signature", "structuralCategory:PieceActivity"),
      targetSquares = List("e3")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = Nil
    ).copy(moveMeaningClaims = List(claim))

    val surface = MoveMeaningSurface.from(view)
    assertEquals(surface.map(_.ideaType), List("piece_activity"))
    assertEquals(surface.map(_.lineRole), List("candidate"))
    assertEquals(surface.map(_.ideaQuality), List("supported"))
    assertEquals(surface.map(_.priority), List("supporting"))
    assertEquals(surface.map(_.evidence.hasCarrier), List(true))
    assertEquals(surface.map(_.evidence.proofLevel), List("surface_evidence"))
    assertEquals(surface.flatMap(_.evidence.sourceIds), List("played-transition"))
    assert(surface.head.evidence.boardCarriers.contains(MoveMeaningSurfaceBoardCarrier("actor", "Move", "e2e3", Some("e2"), Some("e3"))))
    assert(surface.head.evidence.boardCarriers.contains(MoveMeaningSurfaceBoardCarrier("actor", "Piece", "knight")))
    assert(surface.head.evidence.boardCarriers.contains(MoveMeaningSurfaceBoardCarrier("target", "Square", "e3")))
    assert(!claim.reasonTokens.exists(_.startsWith("routeCarrier:")))
    val badSurface = MoveMeaningSurface.from(
      meaningClaimView(
        verdict = MoveChoiceVerdict.Mistake,
        auditCauses = Nil,
        details = Nil
      ).copy(moveMeaningClaims = List(claim.copy(causeKinds = List(RelativeCauseKind.TempoLoss))))
    )
    assertEquals(badSurface, Nil)

  test("move meaning public surface uses rendered target carrier for claim proof"):
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

    assertEquals(surface.map(_.evidence.proofLevel), List("owned_cause"))
    assertEquals(surface.flatMap(_.evidence.causeIds), List("cause-proof-only"))
    assert(surface.head.evidence.boardCarriers.contains(MoveMeaningSurfaceBoardCarrier("target", "Square", "e4")))

  test("move meaning public evidence scans all object signatures before capping board carriers"):
    val fillerSignatures =
      List(
        "mechanism=Mechanism:context|consequence=Consequence:context",
        "actor=Side:white|mechanism=Mechanism:context|consequence=Consequence:context",
        "target=Side:black|mechanism=Mechanism:context|consequence=Consequence:context",
        "mechanism=Mechanism:activity|consequence=Consequence:activity"
      )
    val carrierSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:developmentchoice|consequence=Consequence:developmentpieceactivated"
    val claim = MoveMeaningClaim(
      meaningKind = "PieceActivity",
      role = "ImprovesPieceActivity",
      laneKey = "kind=PieceActivity|axis=Activity:Gain:activity-gain|object=target=Square:e3",
      conflictKey = None,
      supportLevel = "view_surfaced",
      visibility = "functional_explanation",
      surfaceLane = "current_move_function",
      lineRole = "candidate",
      moveUci = "e2e3",
      frameId = "frame-late-carrier",
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      causeKinds = Nil,
      causeSourceSides = Nil,
      causeEvidenceIds = Nil,
      sourceEvidenceIds = List("played-transition"),
      objectBindingSignatures = fillerSignatures :+ carrierSignature,
      reasonTokens = List(s"objectBinding:$carrierSignature")
    )
    val surface = MoveMeaningSurface.from(
      meaningClaimView(
        verdict = MoveChoiceVerdict.MatchesReference,
        auditCauses = Nil,
        details = Nil
      ).copy(moveMeaningClaims = List(claim))
    )

    assertEquals(surface.head.evidence.hasCarrier, true)
    assertEquals(surface.head.evidence.proofLevel, "surface_evidence")
    assert(surface.head.evidence.boardCarriers.contains(MoveMeaningSurfaceBoardCarrier("actor", "Move", "e2e3", Some("e2"), Some("e3"))))
    assert(surface.head.evidence.boardCarriers.contains(MoveMeaningSurfaceBoardCarrier("target", "Square", "e3")))

  test("move meaning public surface ranks terminal carriers before generic activity"):
    val mateSignature =
      "actor=Move:e2e3|actor=Piece:queen|actor=Square:e2|target=Square:h7|mechanism=Mechanism:mate|consequence=Consequence:mate"
    val activitySignature =
      "actor=Move:e2e3|actor=Piece:queen|actor=Square:e2|target=Square:e3|mechanism=Mechanism:activity|consequence=Consequence:activity"
    val mateClaim = MoveMeaningClaim(
      meaningKind = "PieceActivity",
      role = "ImprovesPieceActivity",
      laneKey = "kind=PieceActivity|axis=Activity:Gain:activity-gain|object=mate",
      conflictKey = None,
      supportLevel = "owned_cause_linked",
      visibility = "reason_grade",
      surfaceLane = "current_move_owned",
      lineRole = "candidate",
      moveUci = "e2e3",
      frameId = "frame-terminal-mate",
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      causeKinds = List(RelativeCauseKind.ActivityGain),
      causeSourceSides = List(RelativeCauseSourceSide.Candidate),
      causeEvidenceIds = List("cause-terminal"),
      sourceEvidenceIds = List("played-transition"),
      objectBindingSignatures = List(mateSignature),
      reasonTokens = List(s"objectBinding:$mateSignature"),
      targetSquares = List("h7")
    )
    val activityClaim = mateClaim.copy(
      laneKey = "kind=PieceActivity|axis=Activity:Gain:activity-gain|object=activity",
      frameId = "frame-activity",
      objectBindingSignatures = List(activitySignature),
      reasonTokens = List(s"objectBinding:$activitySignature"),
      targetSquares = List("e3")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = Nil
    ).copy(moveMeaningClaims = List(activityClaim, mateClaim))

    val surface = MoveMeaningSurface.from(view)
    assertEquals(surface.map(_.ideaType).take(2), List("terminal_mate", "piece_activity"))
    assertEquals(surface.head.terminalConsequences.map(_.code), List("mate"))
    assertEquals(surface.head.evidence.hasCarrier, true)
    assertEquals(surface.head.evidence.proofLevel, "terminal_proof")

  test("move meaning claims surface root-owned terminal proof without a strategic axis"):
    val mateSignature =
      "actor=Move:e2e3|target=Square:h7|mechanism=Mechanism:Mate|consequence=Consequence:Mate|proof=DirectProof"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      label = Some("terminal-proof"),
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeConsequences = List("Mate"),
      structuralPurposeCategories = List("TerminalProof"),
      terminalConsequenceKinds = List("Mate"),
      sourceEvidenceIds = List("line:mate-proof", s"played-transition:${candidateLine.rootMove}"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(mateSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )

    val claim = view.moveMeaningClaims.headOption.getOrElse(fail(view.moveMeaningClaims.toString))
    assertEquals(claim.meaningKind, "TerminalProof")
    assertEquals(claim.role, "ProvesMate")
    assertEquals(claim.moveUci, candidateLine.rootMove)
    assertEquals(claim.supportLevel, "view_surfaced")
    assertEquals(claim.surfaceLane, "current_move_function")
    assert(claim.reasonTokens.contains("terminalConsequenceKind:Mate"))
    val surface = MoveMeaningSurface.from(view)
    assertEquals(surface.map(_.ideaType), List("terminal_mate"))
    assertEquals(surface.head.terminalConsequences.map(_.code), List("mate"))
    val drawResourceView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(
        detail.copy(
          structuralPurposeConsequences = List("DrawResource"),
          terminalConsequenceKinds = List("DrawResource"),
          objectBindingSignatures = List(
            "actor=Move:e2e3|target=Square:e3|mechanism=Mechanism:DrawResource|consequence=Consequence:DrawResource|proof=DirectProof"
          )
        )
      )
    )
    val drawSurface = MoveMeaningSurface.from(drawResourceView)
    assertEquals(drawResourceView.moveMeaningClaims.map(_.meaningKind), List("TerminalProof"))
    assertEquals(drawResourceView.moveMeaningClaims.map(_.role), List("ProvesDrawResource"))
    assertEquals(drawSurface.map(_.ideaType), List("draw_resource"))
    assertEquals(drawSurface.head.terminalConsequences.map(_.code), List("draw_resource"))
    val forcedMateView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(
        detail.copy(
          structuralRouteMove = Some("h7h8"),
          objectBindingSignatures = List(
            "actor=Move:h7h8|target=Square:h8|mechanism=Mechanism:Mate|consequence=Consequence:Mate|witness=Move:e2e3|proof=DirectProof"
          )
        )
      )
    )
    assertEquals(forcedMateView.moveMeaningClaims.map(_.meaningKind), List("TerminalProof"))
    assertEquals(forcedMateView.moveMeaningClaims.map(_.surfaceLane), List("current_move_function"))
    val forcedMateSurface = MoveMeaningSurface.from(forcedMateView)
    assertEquals(forcedMateSurface.map(_.ideaType), List("terminal_mate"))
    assert(forcedMateSurface.head.evidence.boardCarriers.contains(MoveMeaningSurfaceBoardCarrier("witness", "Move", "e2e3", Some("e2"), Some("e3"))))

    val targetlessSurface =
      MoveMeaningSurface.from(
        view.copy(
          moveMeaningClaims = List(
            claim.copy(
              causeEvidenceIds = Nil,
              sourceEvidenceIds = Nil,
              objectBindingSignatures = Nil,
              targetSquares = Nil,
              targetFiles = Nil,
              targetPieces = Nil,
              reasonTokens = claim.reasonTokens.filterNot(token =>
                token.startsWith("object") || token.startsWith("sourceEvidenceId:")
              )
            )
          )
        )
      )
    assertEquals(targetlessSurface, Nil)

  test("move meaning claims do not call bare weak-square target pressure a piece route"):
    val targetSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:WeakSquareTargetCreated|consequence=Consequence:WeakSquareTargetCreated"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      axisKey = Some("Target:Gain:weak-square-target"),
      axisKind = Some(StrategicAxisKind.Target),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("weak-square-target"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(targetSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("weak-square:e3"),
      structuralPurposeConsequences = List("WeakSquareTargetCreated"),
      structuralPurposeCategories = List("TargetPressure"),
      structuralMotifTags = List("weak-square")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims.map(_.meaningKind), List("TargetPressure"))
    assertEquals(view.moveMeaningClaims.head.supportLevel, "view_surfaced")
    assert(!view.moveMeaningClaims.exists(_.meaningKind == "PieceRoute"))
    assert(!view.moveMeaningClaims.head.reasonTokens.exists(_.startsWith("routeCarrier:")))

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

  test("move meaning claims keep concrete bad-move route as weak local idea"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:maneuver|consequence=Consequence:developmentpieceactivated"
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
      structuralPurposeSubjects = List("knight:e2-e3:mobility+2"),
      structuralPurposeConsequences = List("DevelopmentPieceActivated", "DevelopmentMobilityGain"),
      structuralPurposeCategories = List("PieceActivity"),
      structuralMotifTags = List("piece", "reroute", "route")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.Mistake,
      auditCauses = Nil,
      details = List(detail)
    )

    val claim = view.moveMeaningClaims.headOption.getOrElse(fail(view.moveMeaningClaims.toString))
    assertEquals(claim.meaningKind, "PieceRoute")
    assertEquals(claim.supportLevel, "view_surfaced")
    assertEquals(claim.surfaceLane, "current_move_function")
    assertEquals(claim.causeEvidenceIds, Nil)
    assert(claim.sourceEvidenceIds.contains("played-transition"), claim.sourceEvidenceIds)

    val surface = MoveMeaningSurface.from(view)
    assertEquals(surface.map(_.ideaType), List("piece_route"))
    assertEquals(surface.head.assessment.localIdea, true)
    assertEquals(surface.head.assessment.verdictReason, false)
    assertEquals(surface.head.ideaQuality, "weak")
    assertEquals(surface.head.evidence.hasCarrier, true)

  test("move meaning claims surface explicit negative current-move route carriers as route loss"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:maneuver|consequence=Consequence:mobilityloss"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Loss:activity-loss"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Loss),
      label = Some("activity-loss"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("knight:e2-e3:maneuver"),
      structuralPurposeConsequences = List("MobilityLoss"),
      structuralPurposeCategories = List("PieceActivity"),
      structuralMotifTags = List("piece", "reroute", "route")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )
    val claim = view.moveMeaningClaims.headOption.getOrElse(fail(view.moveMeaningClaims.toString))

    assertEquals(claim.meaningKind, "PieceRoute")
    assertEquals(claim.role, "ConcedesPieceRoute")
    assertEquals(claim.supportLevel, "view_surfaced")
    assertEquals(claim.surfaceLane, "current_move_function")
    assert(claim.reasonTokens.contains("routePiece:knight"), claim.reasonTokens)
    assert(claim.reasonTokens.contains("routeFrom:e2"), claim.reasonTokens)
    assert(claim.reasonTokens.contains("routeTo:e3"), claim.reasonTokens)

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

  test("move meaning claims surface open-center structural route without cause ownership"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:developmentchoice|consequence=Consequence:developmentpieceactivated"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      axisKey = None,
      axisKind = None,
      contrastOutcome = Some(StrategicAxisComparisonOutcome.SharedSustained),
      candidateEvidenceIds = List("played-transition"),
      referenceEvidenceIds = List("reference-transition"),
      sourceEvidenceIds = List("pawn-structure:before", "structural-delta:played:e2e3"),
      semanticAnchorKeys = List(
        "OpeningAnchor:PawnStructure:PawnBreakObserved",
        "PawnPlay:break-file-e",
        "PawnPlay:center-break",
        "PawnStructure:OpenCenter"
      ),
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.BroadAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("knight:e2-e3", "knight:e2-e3:mobility+2"),
      structuralPurposeConsequences = List("DevelopmentPieceActivated", "DevelopmentMobilityGain"),
      structuralPurposeCategories = List("Development", "OpeningDevelopment", "StrategicSupport"),
      structuralMotifTags = List("open", "space")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )

    val claim = view.moveMeaningClaims.headOption.getOrElse(fail(view.moveMeaningClaims.toString))
    assertEquals(claim.meaningKind, "PieceActivity")
    assertEquals(claim.role, "ImprovesPieceActivity")
    assertEquals(claim.supportLevel, "view_surfaced")
    assertEquals(claim.surfaceLane, "current_move_function")
    assertEquals(claim.causeEvidenceIds, Nil)
    assertEquals(claim.sourceEvidenceIds, List("pawn-structure:before", "played-transition", "structural-delta:played:e2e3"))
    assert(claim.reasonTokens.contains("structuralMotif:open"), claim.reasonTokens)
    assert(claim.reasonTokens.contains("structuralSubject:knight:e2-e3"), claim.reasonTokens)

  test("move meaning claims surface open-center development route as route recognition without cause ownership"):
    val routeSignature =
      "actor=Move:e2e3|actor=Piece:knight|actor=Square:e2|target=Square:e3|mechanism=Mechanism:developmentchoice|consequence=Consequence:developmentpieceactivated"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = None,
      axisKind = None,
      contrastOutcome = Some(StrategicAxisComparisonOutcome.SharedSustained),
      candidateEvidenceIds = List("played-transition"),
      referenceEvidenceIds = List("reference-transition"),
      sourceEvidenceIds = List("pawn-structure:before", "structural-delta:played:e2e3"),
      semanticAnchorKeys = List(
        "OpeningAnchor:PawnStructure:PawnBreakObserved",
        "PawnPlay:center-break",
        "PawnStructure:OpenCenter"
      ),
      objectBindingSignatures = List(routeSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.BroadAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("knight:e2-e3", "knight:e2-e3:mobility+2"),
      structuralPurposeConsequences = List("DevelopmentPieceActivated", "DevelopmentMobilityGain"),
      structuralPurposeCategories = List("Development", "OpeningDevelopment", "StrategicSupport"),
      structuralMotifTags = List("open", "route", "reroute", "space")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )

    val claim = view.moveMeaningClaims.headOption.getOrElse(fail(view.moveMeaningClaims.toString))
    assertEquals(claim.meaningKind, "PieceRoute")
    assertEquals(claim.role, "ImprovesPieceRoute")
    assertEquals(claim.supportLevel, "view_surfaced")
    assertEquals(claim.surfaceLane, "current_move_function")
    assertEquals(claim.causeEvidenceIds, Nil)
    assertEquals(claim.sourceEvidenceIds, List("pawn-structure:before", "played-transition", "structural-delta:played:e2e3"))
    assert(claim.reasonTokens.contains("structuralMotif:open"), claim.reasonTokens)
    assert(claim.reasonTokens.contains("structuralSubject:knight:e2-e3"), claim.reasonTokens)

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

  test("move meaning claims preserve concrete counterplay resource without side-only promotion"):
    val resourceSignature =
      "actor=Move:e2e3|target=Square:e4|mechanism=Mechanism:counterplayrestraint|consequence=Consequence:counterplayrestraint"
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.SpacePreventionResourceDenial,
      axisKey = Some("Counterplay:Restrain:opponent-low-mobility"),
      axisKind = Some(StrategicAxisKind.Counterplay),
      axisPolarity = Some(StrategicAxisPolarity.Restrain),
      label = Some("opponent-low-mobility"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      objectBindingSignatures = List(resourceSignature),
      specificityTier = PositionPlanTechniqueSpecificityTier.BroadAxis,
      resourceContestSquares = List("e4"),
      resourceContestKinds = List("CounterplayRestraint"),
      resourceContestSignals = List("OpponentLowMobility"),
      resourceContestScopes = List("counterplay")
    )
    val sideOnly = detail.copy(
      objectBindingSignatures = List("target=Side:black|mechanism=Mechanism:counterplayrestraint"),
      resourceContestSquares = Nil,
      resourceContestKinds = Nil,
      resourceContestSignals = Nil,
      resourceContestScopes = Nil
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail, sideOnly)
    )

    assertEquals(view.moveMeaningClaims.map(_.meaningKind), List("CounterplayControl"))
    assertEquals(view.moveMeaningClaims.head.supportLevel, "view_surfaced")
    assertEquals(view.moveMeaningClaims.head.role, "PreventsCounterplay")
    assert(view.moveMeaningClaims.head.reasonTokens.contains("resourceContestSquare:e4"))
    assert(view.moveMeaningClaims.head.reasonTokens.contains("resourceContestKind:CounterplayRestraint"))
    assert(view.moveMeaningClaims.head.reasonTokens.contains("resourceContestScope:counterplay"))

  test("move meaning prophylaxis requires concrete threat carrier beyond defense move"):
    val defenseOnly = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.SpacePreventionResourceDenial,
      axisKey = Some("Counterplay:Restrain:prophylaxis-needed"),
      axisKind = Some(StrategicAxisKind.Counterplay),
      axisPolarity = Some(StrategicAxisPolarity.Restrain),
      label = Some("prophylaxis-needed"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("played-transition"),
      objectBindingSignatures =
        List("actor=Move:e2e3|target=Side:black|mechanism=Mechanism:PositionalThreat|consequence=Consequence:Important"),
      specificityTier = PositionPlanTechniqueSpecificityTier.BroadAxis,
      defenseMove = Some(candidateLine.rootMove),
      prophylaxisNeeded = Some(true),
      threatKind = Some("Positional"),
      threatDriver = Some("PositionalThreat"),
      threatSeverity = Some("Important"),
      turnsToImpact = Some(3)
    )
    val concreteThreat = defenseOnly.copy(
      objectBindingSignatures =
        List("actor=Move:e2e3|target=Square:g4|mechanism=Mechanism:PositionalThreat|consequence=Consequence:Important"),
      resourceContestSquares = List("g4"),
      resourceContestScopes = List("prophylaxis", "threat")
    )
    val borrowedAnchorSquare = defenseOnly.copy(
      defenseMove = None,
      objectBindingSignatures =
        List("target=Square:e3|target=Square:g4|mechanism=Mechanism:PositionalThreat|consequence=Consequence:Important"),
      resourceContestSquares = List("e3", "g4"),
      resourceContestScopes = List("counterplay", "prophylaxis", "threat")
    )
    val defenseOnlyView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(defenseOnly)
    )
    val concreteThreatView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(concreteThreat)
    )
    val borrowedAnchorSquareView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(borrowedAnchorSquare)
    )

    assertEquals(defenseOnlyView.moveMeaningClaims, Nil)
    assertEquals(borrowedAnchorSquareView.moveMeaningClaims, Nil)
    assertEquals(concreteThreatView.moveMeaningClaims.map(_.meaningKind), List("CounterplayControl"))
    assertEquals(concreteThreatView.moveMeaningClaims.head.surfaceLane, "current_move_function")
    assert(concreteThreatView.moveMeaningClaims.head.reasonTokens.contains("resourceContestSquare:g4"))
    assert(concreteThreatView.moveMeaningClaims.head.reasonTokens.contains("resourceContestScope:prophylaxis"))
    assert(concreteThreatView.moveMeaningClaims.head.reasonTokens.contains("resourceContestScope:threat"))

  test("move meaning claims do not label center control or concessions as positive counterplay"):
    val centerDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.SpacePreventionResourceDenial,
      axisKey = Some("SpaceCenter:Gain:center-control-gain"),
      axisKind = Some(StrategicAxisKind.SpaceCenter),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("center-control-gain"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      objectBindingSignatures =
        List("actor=Move:e2e3|target=Square:e4|mechanism=Mechanism:centercontrolgain"),
      specificityTier = PositionPlanTechniqueSpecificityTier.BroadAxis,
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("center:e4")
    )
    val concededRace = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.CounterplayRace,
      axisKey = Some("Counterplay:Concede:king-safety-concession"),
      axisKind = Some(StrategicAxisKind.Counterplay),
      axisPolarity = Some(StrategicAxisPolarity.Concede),
      label = Some("king-safety-concession"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      objectBindingSignatures =
        List("actor=Move:e2e3|target=Square:e4|mechanism=Mechanism:kingsafety"),
      specificityTier = PositionPlanTechniqueSpecificityTier.BroadAxis,
      resourceContestSquares = List("e4")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(centerDetail, concededRace)
    )

    assertEquals(view.moveMeaningClaims.map(_.meaningKind), List("CenterControl"))
    assertEquals(view.moveMeaningClaims.map(_.role), List("ExplainsMoveFunction"))
    assert(!view.moveMeaningClaims.exists(claim => claim.meaningKind == "CounterplayControl" && claim.axisKey.contains("SpaceCenter")))
    assert(!view.moveMeaningClaims.exists(claim => claim.meaningKind == "CounterplayRace"))

  test("move meaning claims keep counterplay race distinct and require concrete resource ownership"):
    val raceDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.CounterplayRace,
      axisKey = Some("Counterplay:Gain:counterplay-race"),
      axisKind = Some(StrategicAxisKind.Counterplay),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("counterplay-race"),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      causeEvidenceIds = List("cause-counter-race"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures =
        List("actor=Move:e2e3|target=Square:e4|mechanism=Mechanism:counterplayrace"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ConcreteObjectAxis,
      resourceContestSquares = List("e4"),
      raceLeadingLineRole = Some(LineNodeRole.Played),
      raceReferenceRootMove = Some(referenceLine.rootMove),
      raceCandidateRootMove = Some(candidateLine.rootMove)
    )
    val restrictionCause = causeFrame(
      causeId = "cause-counter-race",
      axisKeys = List("Counterplay:Gain:counterplay-race"),
      objectSignatures = raceDetail.objectBindingSignatures,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot,
      causeKind = RelativeCauseKind.OpponentRestriction
    )
    val actorOnlyRace = raceDetail.copy(
      causeEvidenceIds = Nil,
      objectBindingSignatures = List("actor=Move:e2e3|mechanism=Mechanism:counterplayrace"),
      specificityTier = PositionPlanTechniqueSpecificityTier.BroadAxis,
      resourceContestSquares = Nil
    )
    val restraintOnlyRace = raceDetail.copy(
      causeEvidenceIds = Nil,
      proofRoles = Nil,
      axisKey = Some("Counterplay:Support:opponent-low-mobility"),
      label = Some("opponent-low-mobility"),
      raceCandidateRootMove = None,
      resourceContestSquares = List("e4"),
      resourceContestKinds = List("CounterplayRestraint"),
      resourceContestSignals = List("OpponentLowMobility")
    )
    val counterBreakOnlyRace = raceDetail.copy(
      causeEvidenceIds = Nil,
      proofRoles = Nil,
      raceCandidateRootMove = None,
      resourceContestSquares = Nil,
      counterBreakFiles = List("c")
    )
    val rootlessDynamicRace = raceDetail.copy(
      axisKey = None,
      axisKind = None,
      axisPolarity = None,
      label = Some("dynamic-counterplay-race"),
      causeEvidenceIds = Nil,
      proofRoles = Nil,
      threatKind = Some("Positional"),
      turnsToImpact = Some(2),
      defenseMove = Some("h2h3"),
      raceLeadingLineRole = None,
      raceReferenceRootMove = None,
      raceCandidateRootMove = None
    )
    val pawnBreakRace = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.CounterplayRace,
      axisKey = Some("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      label = Some("counterplay-race-e-vs-c"),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateOnly),
      raceLeadingLineRole = Some(LineNodeRole.Played),
      raceReferenceRootMove = Some(referenceLine.rootMove),
      raceCandidateRootMove = Some(candidateLine.rootMove),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      causeEvidenceIds = List("cause-pawn-race"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures =
        List("actor=Move:e2e3|target=File:e|mechanism=Mechanism:pawnbreak|mechanism=Mechanism:counterplayrace"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ConcreteObjectAxis,
      breakFile = Some("e"),
      tensionEdges = List("e3-d4"),
      counterBreakFiles = List("c")
    )
    val pawnBreakRaceCause = causeFrame(
      causeId = "cause-pawn-race",
      axisKeys = List("PawnBreak:Support:break-file-e-created-tension-e3-d4"),
      objectSignatures = pawnBreakRace.objectBindingSignatures,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot,
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    )
    val pawnBreakRaceSourceDetail =
      pawnBreakRace.copy(
        unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
        label = Some("break-file-e-created-tension-e3-d4")
      )
    val offFilePawnBreakRace = pawnBreakRace.copy(
      axisKey = Some("PawnBreak:Support:break-file-a-created-tension-e3-d4"),
      label = Some("counterplay-race-a-vs-c"),
      breakFile = Some("a"),
      objectBindingSignatures =
        List("actor=Move:e2e3|target=File:a|mechanism=Mechanism:pawnbreak|mechanism=Mechanism:counterplayrace")
    )
    val sameFilePawnBreakRace = pawnBreakRace.copy(
      label = Some("counterplay-race-e-vs-e"),
      counterBreakFiles = List("e")
    )
    val captureCandidateLine = lineRef("played-capture", "e2d3", 2, LineNodeRole.Played)
    val captureReferenceLine = lineRef("best-capture", "g1f3", 1, LineNodeRole.BestReference)
    val captureToFilePawnBreakRace = pawnBreakRace.copy(
      axisKey = Some("PawnBreak:Support:break-file-d-created-tension-e2-d3"),
      label = Some("counterplay-race-d-vs-e"),
      raceCandidateRootMove = Some(captureCandidateLine.rootMove),
      raceReferenceRootMove = Some(captureReferenceLine.rootMove),
      sourceEvidenceIds = List("structural-delta:played:e2d3", "played-transition"),
      objectBindingSignatures =
        List("actor=Move:e2d3|target=File:d|mechanism=Mechanism:pawnbreak|mechanism=Mechanism:counterplayrace"),
      breakFile = Some("d"),
      tensionEdges = List("e2-d3"),
      counterBreakFiles = List("e")
    )
    val captureFromFilePawnBreakRace = captureToFilePawnBreakRace.copy(
      axisKey = Some("PawnBreak:Support:break-file-e-created-tension-e2-d3"),
      label = Some("counterplay-race-e-vs-d"),
      objectBindingSignatures =
        List("actor=Move:e2d3|target=File:e|mechanism=Mechanism:pawnbreak|mechanism=Mechanism:counterplayrace"),
      breakFile = Some("e"),
      counterBreakFiles = List("d")
    )
    val delaysOpponentRace = raceDetail.copy(
      causeEvidenceIds = Nil,
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      resourceContestActorSide = Some("white"),
      resourceContestTargetSide = Some("black"),
      resourceContestKinds = List("CounterplayRestraint"),
      resourceContestSignals = List("OpponentLowMobility"),
      resourceContestScopes = List("counterplay")
    )
    val allowedCounterplayRace = pawnBreakRace.copy(
      axisPolarity = Some(StrategicAxisPolarity.Concede),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateConcession),
      raceLeadingLineRole = Some(LineNodeRole.Played),
      raceCandidateRootMove = Some(candidateLine.rootMove),
      raceReferenceRootMove = Some(referenceLine.rootMove),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("structural-delta:played:e2e3", "played-transition"),
      causeEvidenceIds = Nil,
      proofRoles = List(RelativeCauseProofRole.DirectProof)
    )
    val linkedView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(restrictionCause),
      details = List(raceDetail)
    )
    val pawnBreakRaceView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(pawnBreakRaceCause),
      details = List(pawnBreakRace)
    )
    val pawnBreakRaceWithSourceView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(pawnBreakRaceCause),
      details = List(pawnBreakRaceSourceDetail, pawnBreakRace)
    )
    val actorOnlyView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(actorOnlyRace)
    )
    val restraintOnlyView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(restraintOnlyRace)
    )
    val counterBreakOnlyView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(counterBreakOnlyRace)
    )
    val rootlessDynamicView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(rootlessDynamicRace)
    )
    val offFilePawnBreakRaceView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(pawnBreakRaceCause),
      details = List(offFilePawnBreakRace)
    )
    val sameFilePawnBreakRaceView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(pawnBreakRaceCause),
      details = List(sameFilePawnBreakRace)
    )
    val allowedCounterplayRaceView = meaningClaimView(
      verdict = MoveChoiceVerdict.PlayableLoss,
      auditCauses = Nil,
      details = List(allowedCounterplayRace)
    )
    val captureToFilePawnBreakRaceView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(captureToFilePawnBreakRace),
      reference = captureReferenceLine,
      candidate = captureCandidateLine
    )
    val captureFromFilePawnBreakRaceView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(captureFromFilePawnBreakRace),
      reference = captureReferenceLine,
      candidate = captureCandidateLine
    )
    val delaysOpponentRaceView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(delaysOpponentRace)
    )

    assertEquals(linkedView.moveMeaningClaims.map(_.meaningKind), List("CounterplayRace"))
    assertEquals(linkedView.moveMeaningClaims.map(_.role), List("StartsOwnCounterplayRace"))
    assertEquals(linkedView.moveMeaningClaims.head.supportLevel, "owned_cause_linked")
    assert(linkedView.moveMeaningClaims.head.reasonTokens.contains("raceRole:starts_own_race"))
    assert(linkedView.moveMeaningClaims.head.reasonTokens.contains("raceTempoOrder:played_starts_first"))
    assert(linkedView.moveMeaningClaims.head.reasonTokens.contains("raceCarrier:square"))
    assertEquals(delaysOpponentRaceView.moveMeaningClaims.map(_.role), List("DelaysOpponentCounterplayRace"))
    assert(delaysOpponentRaceView.moveMeaningClaims.head.reasonTokens.contains("raceRole:delays_opponent_counterplay"))
    assert(delaysOpponentRaceView.moveMeaningClaims.head.reasonTokens.contains("resourceContestActorSide:white"))
    assert(delaysOpponentRaceView.moveMeaningClaims.head.reasonTokens.contains("resourceContestTargetSide:black"))
    assertEquals(pawnBreakRaceView.moveMeaningClaims.map(_.meaningKind), List("CounterplayRace"))
    assertEquals(pawnBreakRaceView.moveMeaningClaims.map(_.role), List("StartsOwnCounterplayRace"))
    assertEquals(pawnBreakRaceView.moveMeaningClaims.head.supportLevel, "owned_cause_linked")
    assert(pawnBreakRaceView.moveMeaningClaims.head.reasonTokens.contains("breakFile:e"))
    assert(pawnBreakRaceView.moveMeaningClaims.head.reasonTokens.contains("counterBreakFile:c"))
    assert(pawnBreakRaceView.moveMeaningClaims.head.reasonTokens.contains("raceRole:starts_own_race"))
    assert(pawnBreakRaceView.moveMeaningClaims.head.reasonTokens.contains("raceBreakFile:e"))
    assert(pawnBreakRaceView.moveMeaningClaims.head.reasonTokens.contains("raceCounterBreakFile:c"))
    assert(pawnBreakRaceView.moveMeaningClaims.head.reasonTokens.contains("raceBreakMove:e2e3"))
    assertEquals(pawnBreakRaceWithSourceView.moveMeaningClaims.map(_.meaningKind), List("CounterplayRace"))
    assertEquals(allowedCounterplayRaceView.moveMeaningClaims.map(_.meaningKind), List("CounterplayRace"))
    assertEquals(allowedCounterplayRaceView.moveMeaningClaims.map(_.role), List("AllowsOpponentCounterplayRace"))
    assertEquals(allowedCounterplayRaceView.moveMeaningClaims.head.surfaceLane, "current_move_function")
    assert(allowedCounterplayRaceView.moveMeaningClaims.head.reasonTokens.contains("raceRole:allows_opponent_race"))
    assert(allowedCounterplayRaceView.moveMeaningClaims.head.reasonTokens.contains("raceTempoOrder:opponent_arrives_first"))
    assertEquals(captureToFilePawnBreakRaceView.moveMeaningClaims.map(_.meaningKind), List("CounterplayRace"))
    assert(captureToFilePawnBreakRaceView.moveMeaningClaims.head.reasonTokens.contains("raceBreakFile:d"))
    assertEquals(captureFromFilePawnBreakRaceView.moveMeaningClaims, Nil)
    assertEquals(actorOnlyView.moveMeaningClaims, Nil)
    assertEquals(restraintOnlyView.moveMeaningClaims, Nil)
    assertEquals(counterBreakOnlyView.moveMeaningClaims, Nil)
    assertEquals(rootlessDynamicView.moveMeaningClaims, Nil)
    assertEquals(offFilePawnBreakRaceView.moveMeaningClaims, Nil)
    assertEquals(sameFilePawnBreakRaceView.moveMeaningClaims, Nil)

  test("move meaning claims reject broad or mismatched contrast support"):
    val alternativeLine = lineRef("alt", "d2d4", 3, LineNodeRole.Alternative)
    val cause = causeFrame(
      causeId = "cause-alt-activity",
      axisKeys = List("Activity:Gain:activity-gain"),
      objectSignatures =
        List(
          "actor=Piece:knight|actor=Square:g1|target=Square:f3|mechanism=Mechanism:developmentchoice|proof=DirectProof"
        )
    ).copy(
      role = MoveJudgmentCauseFrameRole.ContextCause,
      comparisonKind = CandidateComparisonKind.PlayedVsAlternative,
      causeRole = RelativeCauseRole.PlayedAlternativeContext,
      causeSourceSide = RelativeCauseSourceSide.Candidate,
      causeImportance = RelativeCauseImportance.Context,
      attributionKind = CauseAttributionKind.CandidateCreatesValue,
      referenceLine = alternativeLine,
      candidateLine = candidateLine,
      eventLine = candidateLine,
      eventRootMove = candidateLine.rootMove,
      hasOwnedAdmissibleLongTermProof = true,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot,
      narrativeRole = MoveJudgmentCauseNarrativeRole.ContextCause
    )
    val exactDetail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateStronger),
      candidateEvidenceIds = List("played-transition"),
      sourceEvidenceIds = List("played-transition"),
      causeEvidenceIds = List("cause-alt-activity"),
      proofRoles = List(RelativeCauseProofRole.DirectProof, RelativeCauseProofRole.ContrastProof),
      objectBindingSignatures =
        List(
          "actor=Piece:knight|actor=Square:g1|target=Square:f3|mechanism=Mechanism:developmentchoice|proof=DirectProof"
        ),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis,
      structuralRouteMove = Some(candidateLine.rootMove)
    )
    val broadPlan = exactDetail.copy(
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      axisKind = Some(StrategicAxisKind.PlanCoherence),
      axisPolarity = Some(StrategicAxisPolarity.Preserve),
      specificityTier = PositionPlanTechniqueSpecificityTier.ContextOnly
    )
    val broadActivitySupport = exactDetail.copy(
      structuralRouteMove = None,
      structuralPurposeSubjects = Nil,
      objectBindingSignatures =
        List(
          "actor=Side:black|actor=Side:white|target=Square:a1|target=Square:f1|mechanism=Mechanism:activity|mechanism=Mechanism:counterplayrestraint|consequence=Consequence:counterplayrestraint|proof=DirectProof"
        )
    )
    val pawnActivityAsRoute = exactDetail.copy(
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("piece"),
      objectBindingSignatures =
        List(
          "actor=Move:e2e3|actor=Side:white|actor=Square:e2|target=Square:d4|mechanism=Mechanism:activity|mechanism=Mechanism:centercontrolchanged|consequence=Consequence:activity:gain:activity-gain|proof=DirectProof"
        )
    )
    val routeMotifWithoutPieceObject = exactDetail.copy(
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("piece:knight:e2-d4"),
      structuralMotifTags = List("route", "outpost"),
      objectBindingSignatures =
        List(
          "actor=Move:e2e3|actor=Side:white|actor=Square:e2|target=Square:d4|mechanism=Mechanism:activity|consequence=Consequence:activity:gain:activity-gain|proof=DirectProof"
        )
    )
    val broadView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(broadPlan)
    )
    val broadActivitySupportView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(broadActivitySupport)
    )
    val pawnActivityAsRouteView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause.copy(objectBindingSignatures = pawnActivityAsRoute.objectBindingSignatures)),
      details = List(pawnActivityAsRoute)
    )
    val routeMotifWithoutPieceObjectView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause.copy(objectBindingSignatures = routeMotifWithoutPieceObject.objectBindingSignatures)),
      details = List(routeMotifWithoutPieceObject)
    )
    val alternativeMoveObject = exactDetail.copy(
      structuralRouteMove = Some(alternativeLine.rootMove),
      structuralPurposeSubjects = List("piece:knight:b1-d2"),
      objectBindingSignatures =
        List(
          "actor=Move:d2d4|actor=Piece:knight|actor=Square:b1|target=Square:d2|mechanism=Mechanism:developmentchoice|proof=DirectProof"
        )
    )
    val alternativeMoveObjectView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause.copy(objectBindingSignatures = alternativeMoveObject.objectBindingSignatures)),
      details = List(alternativeMoveObject)
    )
    val mismatchedCauseUnitView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(exactDetail.copy(unit = PositionPlanTechniqueUnit.StructuralTransformation))
    )
    val fallbackRootView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause.copy(rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.FallbackRoot)),
      details = List(exactDetail)
    )
    val mismatchedSideView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause.copy(causeSourceSide = RelativeCauseSourceSide.Reference)),
      details = List(exactDetail)
    )
    val mismatchedObjectView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses =
        List(cause.copy(objectBindingSignatures = List("target=Square:h7|mechanism=Mechanism:attack|proof=DirectProof"))),
      details = List(exactDetail)
    )
    val broadActorOnlyView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses =
        List(
          cause.copy(
            objectBindingSignatures = List("actor=Piece:knight|mechanism=Mechanism:developmentchoice|proof=DirectProof")
          )
        ),
      details = List(exactDetail)
    )

    assertEquals(broadView.moveMeaningClaims.headOption.map(_.supportLevel), Some("contextual"))
    assertEquals(MoveMeaningSurface.from(broadView), Nil)
    assertEquals(broadActivitySupportView.moveMeaningClaims, Nil)
    assertEquals(pawnActivityAsRouteView.moveMeaningClaims.headOption.map(_.supportLevel), Some("view_surfaced"))
    assertEquals(routeMotifWithoutPieceObjectView.moveMeaningClaims.headOption.map(_.supportLevel), Some("view_surfaced"))
    assertEquals(alternativeMoveObjectView.moveMeaningClaims, Nil)
    assertEquals(mismatchedCauseUnitView.moveMeaningClaims.headOption.map(_.supportLevel), Some("view_surfaced"))
    assertEquals(fallbackRootView.moveMeaningClaims.headOption.map(_.supportLevel), Some("view_surfaced"))
    assertEquals(mismatchedSideView.moveMeaningClaims.headOption.map(_.supportLevel), Some("view_surfaced"))
    assertEquals(mismatchedObjectView.moveMeaningClaims.headOption.map(_.supportLevel), Some("view_surfaced"))
    assertEquals(broadActorOnlyView.moveMeaningClaims.headOption.map(_.supportLevel), Some("view_surfaced"))
    assertEquals(broadActivitySupportView.moveMeaningClaims.flatMap(_.causeEvidenceIds), Nil)
    assertEquals(pawnActivityAsRouteView.moveMeaningClaims.flatMap(_.causeEvidenceIds), Nil)
    assertEquals(routeMotifWithoutPieceObjectView.moveMeaningClaims.flatMap(_.causeEvidenceIds), Nil)
    assertEquals(alternativeMoveObjectView.moveMeaningClaims.flatMap(_.causeEvidenceIds), Nil)
    assertEquals(mismatchedCauseUnitView.moveMeaningClaims.flatMap(_.causeEvidenceIds), Nil)
    assertEquals(fallbackRootView.moveMeaningClaims.flatMap(_.causeEvidenceIds), Nil)
    assertEquals(mismatchedSideView.moveMeaningClaims.flatMap(_.causeEvidenceIds), Nil)
    assertEquals(mismatchedObjectView.moveMeaningClaims.flatMap(_.causeEvidenceIds), Nil)
    assertEquals(broadActorOnlyView.moveMeaningClaims.flatMap(_.causeEvidenceIds), Nil)

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

  test("move meaning claims keep non-binary axes and reject borrowed objects"):
    val ownedCause = causeFrame(
      causeId = "cause-break",
      axisKeys = List("PawnBreak:Support:central-break-timing"),
      objectSignatures = List("target=Square:e3|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    ).copy(
      role = MoveJudgmentCauseFrameRole.ContextCause,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true
    )
    val broadPlan = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      axisKind = Some(StrategicAxisKind.PlanCoherence),
      axisPolarity = Some(StrategicAxisPolarity.Preserve),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.SharedSustained),
      candidateEvidenceIds = List("mechanism-break"),
      candidatePlanIds = List("hold-center"),
      sourceEvidenceIds = List("plan-comparison"),
      planAlignmentScore = Some(2),
      planAlignmentReasonCodes = List("preserve-flexibility"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ContextOnly
    )
    val validBreakDetail =
      broadPlan.copy(
        unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
        axisKind = Some(StrategicAxisKind.PawnBreak),
        axisPolarity = Some(StrategicAxisPolarity.Support),
        contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateStronger),
        breakFile = Some("e"),
        tensionSquares = List("d4", "e3"),
        candidatePlanIds = Nil,
        planAlignmentScore = None,
        planAlignmentReasonCodes = Nil,
        candidateEvidenceIds = List("structural-delta:played:e2e3:tension"),
        sourceEvidenceIds = List("structural-delta:played:e2e3:tension"),
        causeEvidenceIds = List("cause-break"),
        proofRoles = List(RelativeCauseProofRole.DirectProof),
        objectBindingSignatures = List("target=Square:e3|mechanism=Mechanism:pawn-break|proof=DirectProof"),
        specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
      )
    val broadView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(ownedCause),
      details = List(broadPlan)
    )
    val playableView = meaningClaimView(
      verdict = MoveChoiceVerdict.PlayableLoss,
      auditCauses = List(ownedCause),
      details = List(validBreakDetail)
    )
    val negativeView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(ownedCause),
      details = List(
        validBreakDetail.copy(
          axisPolarity = Some(StrategicAxisPolarity.Release),
          contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateStronger)
        )
      )
    )
    val negativeOutcomeView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(ownedCause),
      details = List(
        validBreakDetail.copy(
          axisPolarity = Some(StrategicAxisPolarity.Support),
          contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateConcession)
        )
      )
    )
    val lineOnlyDetail = validBreakDetail.copy(
      axisPolarity = Some(StrategicAxisPolarity.Support),
    )
    val lineOnlyView =
      meaningClaimViewWithClaims(
        meaningClaimView(
        verdict = MoveChoiceVerdict.MatchesReference,
        auditCauses = List(ownedCause),
        details = List(lineOnlyDetail)
        ).copy(positionPlanTechniqueFrames = List(meaningClaimPlanTechniqueFrame(List(lineOnlyDetail)).copy(moveUci = None)))
      )
    val planCoherenceView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(ownedCause),
      details = List(
        broadPlan.copy(
          axisKind = Some(StrategicAxisKind.PlanCoherence),
          axisPolarity = Some(StrategicAxisPolarity.Preserve)
        )
      )
    )
    val sideOnlyCause = causeFrame(
      causeId = "cause-side-only",
      axisKeys = List("Activity:Gain:piece-activity"),
      objectSignatures = List("target=Side:black|mechanism=Mechanism:activity|proof=DirectProof")
    ).copy(role = MoveJudgmentCauseFrameRole.ContextCause)
    val sideOnlyView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(sideOnlyCause),
      details = List(
        validBreakDetail.copy(
          unit = PositionPlanTechniqueUnit.StructuralTransformation,
          axisKind = Some(StrategicAxisKind.Activity),
          causeEvidenceIds = List("cause-side-only"),
          breakFile = None,
          tensionSquares = Nil,
          objectBindingSignatures = List("target=Side:black|mechanism=Mechanism:activity|proof=DirectProof")
        )
      )
    )
    val mismatchedReferenceCause =
      ownedCause.copy(referenceLine = lineRef("other-best", "d2d4", 3, LineNodeRole.BestReference))
    val mismatchedReferenceView = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(mismatchedReferenceCause),
      details = List(validBreakDetail)
    )
    val referenceBreakDetail =
      validBreakDetail.copy(
        referenceEvidenceIds = List("structural-delta:reference:e2e4:tension"),
        candidateEvidenceIds = Nil,
        sourceEvidenceIds = List("structural-delta:reference:e2e4:tension"),
        tensionSquares = List("d5", "e4"),
        objectBindingSignatures = List("target=Square:e4|mechanism=Mechanism:pawn-break|proof=DirectProof"),
        raceReferenceRootMove = Some(referenceLine.rootMove),
        contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceStronger)
      )
    val referenceCause =
      ownedCause.copy(
        causeSourceSide = RelativeCauseSourceSide.Reference,
        eventLine = referenceLine,
        eventRootMove = referenceLine.rootMove,
        objectBindingSignatures = List("target=Square:e4|mechanism=Mechanism:pawn-break|proof=DirectProof")
      )
    val referenceLineView =
      meaningClaimViewWithClaims(
        meaningClaimView(
        verdict = MoveChoiceVerdict.MatchesReference,
        auditCauses = List(referenceCause),
        details = List(referenceBreakDetail)
        ).copy(
        positionPlanTechniqueFrames =
          List(
            meaningClaimPlanTechniqueFrame(List(referenceBreakDetail))
              .copy(line = Some(referenceLine), moveUci = Some(referenceLine.rootMove))
          )
      )
      )
    val frameBorrowedObjectDetail =
      validBreakDetail.copy(
        unit = PositionPlanTechniqueUnit.StructuralTransformation,
        axisKind = Some(StrategicAxisKind.Activity),
        breakFile = None,
        tensionSquares = Nil,
        objectBindingSignatures = Nil
    )
    val frameBorrowedObjectView =
      meaningClaimViewWithClaims(
        meaningClaimView(
        verdict = MoveChoiceVerdict.MatchesReference,
        auditCauses = List(ownedCause),
        details = List(frameBorrowedObjectDetail)
        ).copy(
        positionPlanTechniqueFrames = List(
          meaningClaimPlanTechniqueFrame(List(frameBorrowedObjectDetail)).copy(
            objectBindingSignatures = List("target=Square:e4|mechanism=Mechanism:activity|proof=DirectProof")
          )
        )
      )
      )

    assertEquals(broadView.moveMeaningClaims.headOption.map(_.supportLevel), Some("contextual"))
    assertEquals(playableView.moveMeaningClaims.headOption.map(_.meaningKind), Some("PawnBreakTiming"))
    assertEquals(negativeView.moveMeaningClaims.headOption.map(_.role), Some("ReleasesPawnTension"))
    assertEquals(negativeOutcomeView.moveMeaningClaims.headOption.map(_.role), Some("ReleasesPawnTension"))
    assertEquals(lineOnlyView.moveMeaningClaims, Nil)
    assertEquals(planCoherenceView.moveMeaningClaims.headOption.map(_.meaningKind), Some("PlanContinuity"))
    assertEquals(planCoherenceView.moveMeaningClaims.headOption.map(_.role), Some("SharedCompatiblePlan"))
    assertEquals(planCoherenceView.moveMeaningClaims.headOption.map(_.supportLevel), Some("contextual"))
    assertEquals(planCoherenceView.moveMeaningClaims.headOption.map(_.visibility), Some("soft_context"))
    assertEquals(sideOnlyView.moveMeaningClaims, Nil)
    assertEquals(
      mismatchedReferenceView.moveMeaningClaims.headOption.map(_.supportLevel),
      Some("view_surfaced")
    )
    assertEquals(
      mismatchedReferenceView.moveMeaningClaims.flatMap(_.reasonTokens)
        .exists(_.startsWith("causeEvidenceId:")),
      false
    )
    assertEquals(referenceLineView.moveMeaningClaims.headOption.map(_.lineRole), Some("reference"))
    assertEquals(referenceLineView.moveMeaningClaims.headOption.map(_.moveUci), Some(referenceLine.rootMove))
    assertEquals(referenceLineView.moveMeaningClaims.headOption.map(_.surfaceLane), Some("reference_or_opponent_resource"))
    assertEquals(frameBorrowedObjectView.moveMeaningClaims, Nil)

  test("move meaning claims keep reference resources out of current move reason lane"):
    val referenceCause = causeFrame(
      causeId = "cause-reference-break",
      axisKeys = List("PawnBreak:Support:reference-break"),
      objectSignatures = List("target=File:e|mechanism=Mechanism:pawn-break|proof=DirectProof"),
      causeKind = RelativeCauseKind.PawnBreakOpportunity
    ).copy(
      role = MoveJudgmentCauseFrameRole.ContextCause,
      causeSourceSide = RelativeCauseSourceSide.Reference,
      eventLine = referenceLine,
      eventRootMove = referenceLine.rootMove,
      hasOwnedAdmissibleLongTermProof = true,
      attributionDirectProofEligible = true,
      rootArbitrationTier = MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
    )
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:reference-break"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceStronger),
      breakFile = Some("e"),
      tensionPolicy = Some("prepare"),
      tensionSquares = List("e4", "d5"),
      candidateEvidenceIds = List("played-transition"),
      referenceEvidenceIds = List("reference-transition"),
      sourceEvidenceIds = List("played-transition", "reference-transition"),
      causeEvidenceIds = List("cause-reference-break"),
      proofRoles = List(RelativeCauseProofRole.DirectProof, RelativeCauseProofRole.ContrastProof),
      objectBindingSignatures =
        List(
          "actor=Move:e2e3|target=File:e|mechanism=Mechanism:pawn-break|proof=DirectProof",
          "actor=Move:e2e4|target=File:e|mechanism=Mechanism:pawn-break|proof=DirectProof"
        ),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.Mistake,
      auditCauses = List(referenceCause),
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims.map(_.lineRole), List("reference"))
    assertEquals(view.moveMeaningClaims.map(_.moveUci), List(referenceLine.rootMove))
    assertEquals(view.moveMeaningClaims.map(_.supportLevel), List("owned_cause_linked"))
    assertEquals(view.moveMeaningClaims.map(_.surfaceLane), List("reference_or_opponent_resource"))
    assert(!view.moveMeaningClaims.exists(claim => claim.moveUci == candidateLine.rootMove && claim.supportLevel == "owned_cause_linked"))
    assert(view.moveMeaningClaims.exists(_.reasonTokens.contains("comparisonLossSide:candidate")))
    assert(view.moveMeaningClaims.exists(_.reasonTokens.contains("comparisonLoss:break_option_missed")))

    val surface = MoveMeaningSurface.from(view)
    assertEquals(surface.map(_.subject), List("reference_move"))
    assertEquals(surface.map(_.lineRole), List("reference"))
    assertEquals(surface.map(_.moveQuality), List("not_applicable"))
    assertEquals(surface.map(_.priority), List("comparison"))
    assertEquals(surface.flatMap(_.failureFamily), Nil)
    assertEquals(surface.flatMap(_.problem), Nil)
    assertEquals(surface.flatMap(_.comparisonLossSides), List("candidate"))
    assertEquals(surface.flatMap(_.comparisonLosses), List("break_option_missed"))
    assertEquals(surface.flatMap(_.comparisonLostIdeas.map(_.side)), List("played_move"))
    assertEquals(surface.flatMap(_.comparisonLostIdeas.map(_.label)), List("missed pawn break option"))
    assertEquals(surface.flatMap(_.comparison.toList.flatMap(_.moves.map(_.role))).distinct, List("played_move", "best_move"))
    assertEquals(surface.flatMap(_.comparison.toList.flatMap(_.lostIdeas.map(_.side))), List("played_move"))
    assertEquals(surface.flatMap(_.target.files), List("e"))

  test("move meaning comparison loss tokens require a distinct compared move"):
    val sameRootReferenceLine = lineRef("same-best", candidateLine.rootMove, 1, LineNodeRole.BestReference)
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKey = Some("PawnBreak:Support:same-root-break"),
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceOnly),
      breakFile = Some("e"),
      tensionPolicy = Some("maintain"),
      tensionSquares = List("e3", "d4"),
      candidateEvidenceIds = List("structural-delta:played:e2e3:tension"),
      referenceEvidenceIds = List("structural-delta:reference:e2e3:tension"),
      sourceEvidenceIds = List("structural-delta:played:e2e3:tension", "structural-delta:reference:e2e3:tension"),
      proofRoles = List(RelativeCauseProofRole.ContrastProof),
      objectBindingSignatures = List("actor=Move:e2e3|target=File:e|mechanism=Mechanism:pawn-break|proof=ContrastProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val base =
      MoveJudgmentView(
        verdict = Some(
          MoveJudgmentVerdictFrame(
            verdict = MoveChoiceVerdict.MatchesReference,
            winPercentLossForMover = 0.0,
            candidateWinPercentDeltaForMover = 0.0,
            relativeAssessmentEvidenceId = "relative-assessment",
            verdictCertificationEvidenceId = None,
            comparisonKind = CandidateComparisonKind.PlayedVsBest,
            referenceLine = sameRootReferenceLine,
            candidateLine = candidateLine
          )
        ),
        verdictCarriers = Nil,
        causeAudit = MoveJudgmentCauseAudit(),
        positionPlanTechniqueFrames = List(meaningClaimPlanTechniqueFrame(List(detail))),
        supportContextClusterIds = Nil,
        overriddenLocalIdeas = Nil,
        preservedLocalIdeas = Nil
      )
    val view = meaningClaimViewWithClaims(base)

    assert(view.moveMeaningClaims.nonEmpty)
    assert(!view.moveMeaningClaims.exists(_.reasonTokens.exists(_.startsWith("comparisonLoss:"))))
    assert(!view.moveMeaningClaims.exists(_.reasonTokens.exists(_.startsWith("comparisonLossSide:"))))
    assertEquals(MoveMeaningSurface.from(view).flatMap(_.comparisonLosses), Nil)

  test("position plan comparison loss kinds map concrete chess carriers"):
    val pawnRelease = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
      axisKind = Some(StrategicAxisKind.PawnBreak),
      axisPolarity = Some(StrategicAxisPolarity.Release),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceStronger),
      breakFile = Some("d"),
      tensionEdges = List("d4-e5")
    )
    val candidateRelease = pawnRelease.copy(contrastOutcome = Some(StrategicAxisComparisonOutcome.CandidateConcession))
    val counterRace = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.CounterplayRace,
      axisKind = Some(StrategicAxisKind.Counterplay),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceStronger),
      breakFile = Some("e"),
      counterBreakFiles = List("c"),
      resourceContestFiles = List("c")
    )
    val genericCounterRace =
      counterRace.copy(breakFile = None, counterBreakFiles = Nil, resourceContestFiles = List("c"))
    val outpostRoute = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceStronger),
      structuralPurposeSubjects = List("outpost:knight:d5")
    )
    val diagonalPressure = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceStronger),
      objectBindingSignatures = List(
        "actor=Piece:bishop|target=Square:h6|mechanism=Mechanism:bishop-long-diagonal|consequence=Consequence:diagonalpressure"
      )
    )
    val slowOrder = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.PlanOptionSet,
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceStronger),
      planAlignmentReasonCodes = List("move-order-too-slow")
    )
    val tempoOnly = slowOrder.copy(planAlignmentReasonCodes = List("tempo-lag"))
    val pawnBreakWithTargetContext =
      pawnRelease.copy(
        axisPolarity = Some(StrategicAxisPolarity.Support),
        structuralRouteMove = Some("e6e5"),
        structuralPurposeSubjects = List("weak-pawn:d4", "weak-square:d4")
      )

    assertEquals(PositionPlanTechniqueSemanticDetail.comparisonLossSides(pawnRelease), List("reference"))
    assertEquals(PositionPlanTechniqueSemanticDetail.comparisonLossSides(candidateRelease), List("candidate"))
    assert(PositionPlanTechniqueSemanticDetail.comparisonLossKinds(pawnRelease).contains("tension_released_early"))
    assertEquals(PositionPlanTechniqueSemanticDetail.comparisonLossKinds(pawnBreakWithTargetContext), List("break_option_missed"))
    assertEquals(PositionPlanTechniqueSemanticDetail.comparisonLossKinds(counterRace), List("counter_break_allowed"))
    assert(!PositionPlanTechniqueSemanticDetail.comparisonLossKinds(genericCounterRace).contains("counter_break_allowed"))
    assert(PositionPlanTechniqueSemanticDetail.comparisonLossKinds(outpostRoute).contains("outpost_route_missed"))
    assert(PositionPlanTechniqueSemanticDetail.comparisonLossKinds(diagonalPressure).contains("diagonal_pressure_lost"))
    assert(PositionPlanTechniqueSemanticDetail.comparisonLossKinds(slowOrder).contains("move_order_too_slow"))
    assert(!PositionPlanTechniqueSemanticDetail.comparisonLossKinds(tempoOnly).contains("move_order_too_slow"))

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
      objectBindingSignatures = List("target=Pawn:weak-pawn:d4|mechanism=Mechanism:target|proof=DirectProof"),
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

  test("move meaning public surface recognizes diagonal battery pressure without a new motif"):
    val cause = causeFrame(
      causeId = "cause-diagonal-battery",
      axisKeys = List("Activity:Gain:battery-pressure-gain"),
      objectSignatures = List(
        "actor=Move:e2e3|actor=Piece:bishop|actor=Piece:queen|target=Square:c1|target=Square:h6|mechanism=Mechanism:battery-diagonal"
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
      causeEvidenceIds = List("cause-diagonal-battery"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      structuralRouteMove = Some(candidateLine.rootMove),
      structuralPurposeSubjects = List("battery:diagonal:c1-h6:bishop-queen"),
      structuralPurposeConsequences = List("BatteryPressureGain"),
      objectBindingSignatures = List(
        "actor=Move:e2e3|actor=Piece:bishop|actor=Piece:queen|target=Square:c1|target=Square:h6|mechanism=Mechanism:battery-diagonal"
      ),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = List(cause),
      details = List(detail)
    )

    val surface = MoveMeaningSurface.from(view)
    assertEquals(surface.map(_.ideaType), List("long_diagonal_pressure"))
    assertEquals(surface.map(_.subject), List("played_move"))
    assertEquals(surface.flatMap(_.target.squares), List("c1", "h6"))

  test("move meaning public surface recognizes long bishop diagonal route without a battery unit"):
    val signature =
      "actor=Move:b7e4|actor=Piece:bishop|actor=Square:b7|target=Square:e4|mechanism=Mechanism:developmentchoice|mechanism=Mechanism:bishop-long-diagonal|consequence=Consequence:diagonalpressure"
    val claim = MoveMeaningClaim(
      meaningKind = "PieceRoute",
      role = "ImprovesPieceRoute",
      laneKey = "kind=PieceRoute|axis=Activity:Gain:activity-gain|object=target=Square:e4",
      conflictKey = None,
      supportLevel = "owned_cause_linked",
      visibility = "reason_grade",
      surfaceLane = "current_move_owned",
      lineRole = "candidate",
      moveUci = "b7e4",
      frameId = "frame-bishop-long-diagonal",
      unit = PositionPlanTechniqueUnit.PieceRerouteRoute,
      axisKey = Some("Activity:Gain:activity-gain"),
      axisKind = Some(StrategicAxisKind.Activity),
      axisPolarity = Some(StrategicAxisPolarity.Gain),
      label = Some("activity-gain"),
      causeKinds = List(RelativeCauseKind.ActivityGain),
      causeSourceSides = List(RelativeCauseSourceSide.Candidate),
      causeEvidenceIds = List("cause-bishop-long-diagonal"),
      sourceEvidenceIds = List("played-transition"),
      objectBindingSignatures = List(signature),
      reasonTokens = List(s"objectBinding:$signature", "structuralMotif:diagonal"),
      targetSquares = List("e4")
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = Nil
    ).copy(moveMeaningClaims = List(claim))

    val surface = MoveMeaningSurface.from(view)
    assertEquals(surface.map(_.ideaType), List("long_diagonal_pressure"))
    assertEquals(surface.flatMap(_.target.squares), List("e4"))

  test("move meaning public surface does not call file battery pressure long diagonal"):
    val cause = causeFrame(
      causeId = "cause-file-battery",
      axisKeys = List("Activity:Gain:battery-pressure-gain"),
      objectSignatures = List(
        "actor=Move:e2e3|actor=Piece:rook|actor=Piece:queen|target=Square:a5|target=Square:h5|mechanism=Mechanism:battery-file"
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
        "actor=Move:e2e3|actor=Piece:rook|actor=Piece:queen|target=Square:a5|target=Square:h5|mechanism=Mechanism:battery-file"
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

  test("move meaning claims keep mixed contrast details line-neutral"):
    val detail = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      axisKey = Some("Target:Support:mixed-contrast-target"),
      axisKind = Some(StrategicAxisKind.Target),
      axisPolarity = Some(StrategicAxisPolarity.Support),
      contrastOutcome = Some(StrategicAxisComparisonOutcome.ReferenceStronger),
      structuralPurposeSubjects = List("e4"),
      referenceEvidenceIds = List("reference-transition"),
      candidateEvidenceIds = List("candidate-transition"),
      sourceEvidenceIds = List("reference-transition", "candidate-transition"),
      proofRoles = List(RelativeCauseProofRole.DirectProof),
      objectBindingSignatures = List("target=Square:e4|mechanism=Mechanism:target-pressure|proof=DirectProof"),
      specificityTier = PositionPlanTechniqueSpecificityTier.ExactObjectAxis
    )
    val view = meaningClaimView(
      verdict = MoveChoiceVerdict.MatchesReference,
      auditCauses = Nil,
      details = List(detail)
    )

    assertEquals(view.moveMeaningClaims.map(_.lineRole), List("contrast"))
    assertEquals(view.moveMeaningClaims.map(_.moveUci), List(""))
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

  test("axisless structural anchor inventory reports structural signals that cannot enter axis lineage"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(chess.Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/8/8/8 b - - 1 1", 2, Some(chess.Color.Black), Some("after"))
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = after,
      line = Some(candidateLine),
      perspective = chess.Color.White
    )
    val axislessRef = evidenceRef("structural-delta:axisless", root)
    val axislessPayload = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(
        TransitionConsequence(TransitionConsequenceKind.PassedPawnProgress, StructuralSignalPolarity.Gain, 3),
        TransitionConsequence(TransitionConsequenceKind.PromotionPressureGain, StructuralSignalPolarity.Gain, 3)
      )
    )
    val axisRef = evidenceRef("structural-delta:outpost", root)
    val axisPayload = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(
        TransitionConsequence(
          TransitionConsequenceKind.OutpostGain,
          StructuralSignalPolarity.Gain,
          3,
          subjects = List("outpost:knight:e5")
        )
      )
    )
    val inventory = MoveReviewPhase3AuditRunner.axislessStructuralAnchorInventoryJson(
      TypedEvidenceGraph(
        List(
          EvidenceRecord(axislessRef, axislessPayload),
          EvidenceRecord(axisRef, axisPayload)
        )
      )
    )

    assertEquals((inventory \ "axislessSignalCount").as[Int], 2)
    assertEquals((inventory \ "axislessRecordCount").as[Int], 1)
    assertEquals((inventory \ "axislessSignalLabelCounts" \ "passed-pawn-progress").as[Int], 1)
    assertEquals((inventory \ "axislessSignalLabelCounts" \ "promotion-pressure-gain").as[Int], 1)
    assertEquals((inventory \ "axislessRecordIds").as[List[String]], List("structural-delta:axisless"))

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      Files
        .walk(path)
        .iterator()
        .asScala
        .toList
        .sortWith((left, right) => left.getNameCount > right.getNameCount)
        .foreach(Files.deleteIfExists)

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
      familyMismatchKinds = Set.empty,
      expectedIdeaFamilies = Set(ChessIdeaFamily.Strategic),
      actualIdeaFamilies = Set(ChessIdeaFamily.Strategic),
      expectedClaimFamilies = Set(ClaimFamily.Strategic),
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

  private def diagnosticWithDetailTokens(
      id: String,
      detailTokens: List[String],
      causeKind: RelativeCauseKind,
      causeId: String = "cause-lucena",
      unit: PositionPlanTechniqueUnit = PositionPlanTechniqueUnit.EndgameTechniqueRecipe
  ): CandidateComparisonDiagnostic =
    comparisonDiagnostic(
      id = id,
      referenceLeadAxes = Nil,
      producedKinds = List(causeKind),
      flows = List(causeFlow(causeId, causeKind, proofAxisKeys = Nil, claimIds = List(s"claim-$id"))),
      primaryRootKinds = List(causeKind),
      primaryRootIds = List(causeId),
      positionPlanTechniqueFrameIds = List(s"frame-$id"),
      positionPlanTechniqueUnits = List(unit),
      positionPlanTechniqueSemanticDetailUnits = List(unit),
      positionPlanTechniqueSemanticDetailMechanismKinds = List(StrategicMechanismKind.Endgame),
      positionPlanTechniqueSemanticDetailTokens = detailTokens,
      positionPlanTechniqueSemanticDetailTokenGroups = List(detailTokens),
      positionPlanTechniqueObjectBindingSignatures =
        List("target=Square:d8|mechanism=Mechanism:EndgameTechniqueHorizon|proof=DirectProof"),
      positionPlanTechniqueRelativeCauseEvidenceIds = List(causeId),
      moveMeaningClaimSurfaceSignatures =
        List(s"unit=$unit|axis=none|kind=RookEndgameTechnique|support=owned_cause_linked|lane=current_move_owned|causes=$causeId|carrier=true|boardCarrier=true|objects=target=Square:d8;mechanism=Mechanism:EndgameTechniqueHorizon"),
      publicMoveMeaningClaimSurfaceSignatures =
        List(s"unit=$unit|axis=none|kind=RookEndgameTechnique|support=owned_cause_linked|lane=current_move_owned|causes=$causeId|carrier=true|boardCarrier=true|objects=target=Square:d8;mechanism=Mechanism:EndgameTechniqueHorizon"),
      primaryRootArbitrationTiers = List(MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot)
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
      positionPlanTechniqueFrameIds: List[String] = Nil,
      positionPlanTechniqueUnits: List[PositionPlanTechniqueUnit] = Nil,
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
      moveMeaningClaimSurfaceSignatures: List[String] = Nil,
      publicMoveMeaningClaimSurfaceSignatures: List[String] = Nil,
      rootArbitrationTiers: List[MoveJudgmentCauseRootArbitrationTier] = Nil,
      primaryRootArbitrationTiers: List[MoveJudgmentCauseRootArbitrationTier] = Nil,
      primaryRootCauseEvidenceIdTierSignatures: List[String] = Nil,
      verdict: MoveChoiceVerdict = MoveChoiceVerdict.Mistake,
      relationKinds: List[RelationFactKind] = Nil
  ): CandidateComparisonDiagnostic =
    val axes = semanticAxes(referenceLeadAxes)
    val publicMoveMeaningClaimDiagnostics =
      publicMoveMeaningClaimSurfaceSignatures.flatMap(publicMoveMeaningClaimDiagnostic)
    val rootCauseEvidenceIdTierSignatures =
      if primaryRootCauseEvidenceIdTierSignatures.nonEmpty then primaryRootCauseEvidenceIdTierSignatures
      else
        primaryRootIds
          .zip(primaryRootArbitrationTiers)
          .map { case (causeEvidenceId, tier) => s"$causeEvidenceId|tier=$tier" }
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
        genericCauseIds = Nil,
        ownedTypedDepthCauseIds = flows.map(_.causeId),
        nonGenericCauseIds = flows.map(_.causeId),
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
        primaryRootCauseEvidenceIdTierSignatures = rootCauseEvidenceIdTierSignatures,
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
        moveMeaningClaimSurfaceSignatures = moveMeaningClaimSurfaceSignatures,
        publicMoveMeaningClaimDiagnostics = publicMoveMeaningClaimDiagnostics,
        publicMoveMeaningClaimSurfaceSignatures = publicMoveMeaningClaimSurfaceSignatures
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
  private def publicMoveMeaningClaimDiagnostic(signature: String): Option[PublicMoveMeaningClaimDiagnostic] =
    val parts = signature.split("\\|").toList.map(_.trim).filter(_.nonEmpty)
    def value(key: String): Option[String] =
      parts.collectFirst { case part if part.startsWith(s"$key=") => part.stripPrefix(s"$key=") }
    def values(key: String): List[String] =
      value(key).toList
        .flatMap(_.split(",").toList)
        .map(_.trim)
        .filter(value => value.nonEmpty && value != "none")
    value("unit").flatMap(unitName => PositionPlanTechniqueUnit.values.find(_.toString == unitName)).map { unit =>
      PublicMoveMeaningClaimDiagnostic(
        unit = unit,
        axisKey = value("axis").filter(_ != "none"),
        meaningKind = value("kind").getOrElse(""),
        supportLevel = value("support").getOrElse(""),
        surfaceLane = value("lane").getOrElse(""),
        lineRole = value("lineRole").getOrElse(""),
        moveUci = value("move").getOrElse(""),
        causeEvidenceIds = values("causes"),
        hasCarrier = value("carrier").contains("true"),
        hasBoardCarrier = value("boardCarrier").contains("true"),
        proofLevel = value("proof").getOrElse("none"),
        signature = signature
      )
    }
