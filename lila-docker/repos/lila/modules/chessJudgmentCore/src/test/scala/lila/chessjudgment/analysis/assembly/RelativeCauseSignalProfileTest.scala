package lila.chessjudgment.analysis.assembly

import chess.Color
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, StructuralDeltaContracts }
import lila.chessjudgment.model.judgment.*

class RelativeCauseSignalProfileTest extends munit.FunSuite:

  test("scores plan contradiction below concrete structural causes"):
    val plan = relativeCauseWithLongTermProof(RelativeCauseKind.PlanContradiction)
    val activity = relativeCauseWithLongTermProof(RelativeCauseKind.ActivityGain)

    assert(
      ClaimArbitrator.relativeCauseSalienceForAudit(activity) >
        ClaimArbitrator.relativeCauseSalienceForAudit(plan)
    )

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

    assertEquals(delta.opponentMobilityRestrictions, Nil)
    assert(!StructuralDeltaContracts.consequences(delta).exists(_.kind == TransitionConsequenceKind.OpponentMobilityRestriction))

  test("maps concrete opponent diagonal restriction to current move opponent restriction"):
    val root = PositionNodeRef("4k3/6b1/8/3p4/3PP3/8/8/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("4k3/6b1/8/3pP3/3P4/8/8/4K3 b - - 0 1", 2, Some(Color.Black), Some("after"))
    val playedLine = LineNodeRef("played-line", "e4e5", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "e4e5", 1, LineNodeRole.BestReference)
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
            label = "opponent-diagonal-restriction",
            source = structuralRef,
            strength = 3,
            axis = Some(
              StrategicAxisDetail(
                StrategicAxisKind.Counterplay,
                StrategicAxisPolarity.Restrain,
                "opponent-diagonal-restriction"
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
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.OpponentRestriction))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Candidate)))
    assertEquals(drafts.map(_.attributionKind), List(CauseAttributionKind.CandidateCreatesValue))
    assert(signatures.exists(signature => signature.contains("target=Piece:bishop") && signature.contains("target=Square:g7")), signatures)

  test("does not draft current move opponent restriction from side-only restraint"):
    val root = PositionNodeRef("4k3/6b1/8/3p4/3PP3/8/8/4K3 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("4k3/6b1/8/3pP3/3P4/8/8/4K3 b - - 0 1", 2, Some(Color.Black), Some("after"))
    val playedLine = LineNodeRef("played-line", "e4e5", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "e4e5", 1, LineNodeRole.BestReference)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:e4e5:side-only-restraint",
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
            subjects = List("side:black")
          )
        )
      )
    )
    val mechanismRecord = EvidenceRecord(
      EvidenceRef(
        id = "strategic-mechanism:counterplay:e4e5:side-only-restraint",
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
            label = "opponent-diagonal-restriction",
            source = structuralRef,
            strength = 3,
            axis = Some(
              StrategicAxisDetail(
                StrategicAxisKind.Counterplay,
                StrategicAxisPolarity.Restrain,
                "opponent-diagonal-restriction"
              )
            )
          )
        ),
        semanticAnchors = Nil
      ),
      parents = List(structuralRef)
    )
    val profile = exactPlayedProfile(referenceLine, playedLine, List(structuralRecord, mechanismRecord))

    assertEquals(RelativeCauseDraftPlanner.drafts(profile), Nil)

  private def relativeCauseWithLongTermProof(kind: RelativeCauseKind): RelativeCauseFact =
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/8/8/8 b - - 1 1", 2, Some(Color.Black), Some("after"))
    val referenceLine = LineNodeRef("best-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val proofRef = EvidenceRef(
      id = s"transition-proof:${kind.toString}",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val transition = StructuralTransitionBinding(
      moveUci = candidateLine.rootMove,
      role = TransitionEdgeRole.Played,
      from = root,
      to = after,
      line = Some(candidateLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      kind = TransitionConsequenceKind.DevelopmentMobilityGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("activity")
    )
    RelativeCauseFact(
      kind = kind,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      verdict = MoveChoiceVerdict.Mistake,
      winPercentLossForMover = 12.0,
      candidateWinPercentDeltaForMover = -12.0,
      supportEvidence = Nil,
      evidenceLines = List(referenceLine, candidateLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = candidateLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateAllowsLiability,
        ownedEvidence = List(proofRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            transitionConsequences = List(TransitionConsequenceProof(proofRef, transition, consequence))
          )
        )
      )
    )

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

    assertEquals(RelativeCauseDraftPlanner.drafts(profile).map(_.kind), Nil)

  test("does not draft played bad-cause activity loss for playable loss"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val activityLossRef = EvidenceRef(
      id = "structural-delta:played:d2d4",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val activityLossRecord = EvidenceRecord(
      ref = activityLossRef,
      payload = StructuralDeltaEvidence(
        transition = StructuralTransitionBinding(
          moveUci = "d2d4",
          role = TransitionEdgeRole.Played,
          from = root,
          to = afterPlayed,
          line = Some(playedLine),
          perspective = Color.White
        ),
        signals = Nil,
        consequences = List(
          TransitionConsequence(
            kind = TransitionConsequenceKind.DevelopmentLagIncreased,
            polarity = StructuralSignalPolarity.Loss,
            strength = 2
          )
        )
      )
    )
    val tacticalRef = EvidenceRef(
      id = "tactical-mechanism:played:d2d4",
      producer = EvidenceProducer.TacticalMechanismProducer,
      layer = EvidenceLayer.TacticalMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedLine,
      confidence = EvidenceConfidence.EngineBacked
    )
    val tacticalRecord = EvidenceRecord(
      ref = tacticalRef,
      payload = TacticalMechanismEvidence(
        kind = TacticalMechanismKind.MaterialGain,
        moveUci = Some("d2d4"),
        line = Some(playedLine),
        signals = List(
          TacticalMechanismSignal(
            kind = TacticalMechanismSignalKind.LineConsequence,
            label = "material-gain",
            sourceLayer = EvidenceLayer.Line
          )
        )
      )
    )
    val profile = RelativeCauseSignalProfile.from(
      fact = CandidateComparisonFact(
        kind = CandidateComparisonKind.PlayedVsBest,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        comparison = EvalComparison(
          mover = Color.White,
          referenceLine = referenceLine,
          candidateLine = playedLine,
          rawCandidateDeltaCpForDiagnostics = -20,
          candidateWinPercentDeltaForMover = 0.0,
          rawCpLossForDiagnostics = 20,
          winPercentLossForMover = 2.5,
          verdict = MoveChoiceVerdict.PlayableLoss
        )
      ),
      referenceRecords = Nil,
      candidateRecords = List(activityLossRecord, tacticalRecord),
      sharedRecords = Nil
    )

    assertEquals(RelativeCauseDraftPlanner.drafts(profile).map(_.kind), Nil)

  test("drafts played-vs-best strategic contrast for near-threshold inaccuracy"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val contrastRef = EvidenceRef(
      id = "strategic-contrast:played-vs-best",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val contrastRecord = EvidenceRecord(
      ref = contrastRef,
      payload = StrategicMechanismContrastEvidence(
        comparisonKind = CandidateComparisonKind.PlayedVsBest,
        referenceLine = referenceLine,
        candidateLine = playedLine,
        axisComparisons = List(
          StrategicAxisComparison(
            axis = StrategicAxisDetail(
              StrategicAxisKind.PlanCoherence,
              StrategicAxisPolarity.Support,
              "OpeningDevelopment,PieceActivation"
            ),
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
      referenceRecords = List(contrastRecord),
      candidateRecords = Nil,
      sharedRecords = Nil
    )

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.PlanContradiction))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Reference)))

  test("prefers reference target pressure root over plan contradiction when axis proof is concrete"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val targetSource = EvidenceRef(
      id = "strategic-mechanism:target-pressure:reference",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val contrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, "target-pressure-gain"),
          StrategicAxisComparisonOutcome.ReferenceOnly,
          referenceSources = List(targetSource)
        ),
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
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.TargetPressureGain))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Reference)))

  test("prefers candidate activity loss root over plan contradiction when axis proof is concrete"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val activitySource = EvidenceRef(
      id = "strategic-mechanism:activity-loss:played",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val contrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Loss, "activity-loss"),
          StrategicAxisComparisonOutcome.CandidateConcession,
          candidateSources = List(activitySource)
        ),
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
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.ActivityLoss))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Candidate)))

  test("maps reference activity gain to a concrete activity gain root"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val contrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity-gain"),
          StrategicAxisComparisonOutcome.ReferenceOnly
        )
      ),
      planComparison = None
    )
    val profile = playedVsBestProfile(referenceLine, playedLine, List(contrastRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.ActivityGain))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Reference)))

  test("preserves outpost transitions as activity axis details"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterReference = PositionNodeRef("8/8/8/3N4/8/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-reference"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val transition =
      StructuralTransitionBinding(
        moveUci = "g1f3",
        role = TransitionEdgeRole.Reference,
        from = root,
        to = afterReference,
        line = Some(referenceLine),
        perspective = Color.White
      )
    def outpostRecord(
        id: String,
        kind: TransitionConsequenceKind,
        polarity: StructuralSignalPolarity
    ): EvidenceRecord =
      EvidenceRecord(
        ref = EvidenceRef(
          id = id,
          producer = EvidenceProducer.StructuralDeltaProducer,
          layer = EvidenceLayer.StructuralDelta,
          position = root,
          line = Some(referenceLine),
          scope = EvidenceScope.Counterfactual,
          confidence = EvidenceConfidence.EngineBacked
        ),
        payload = StructuralDeltaEvidence(
          transition = transition,
          signals = Nil,
          consequences = List(TransitionConsequence(kind, polarity, 1, List("d5")))
        )
      )

    val gainSignals =
      StrategicMechanismEvidence
        .sourceMechanisms(outpostRecord("structural-delta:outpost-gain", TransitionConsequenceKind.OutpostGain, StructuralSignalPolarity.Gain))
        .collect { case (StrategicMechanismKind.Activity, signal) => signal }
    val concessionSignals =
      StrategicMechanismEvidence
        .sourceMechanisms(outpostRecord("structural-delta:outpost-concession", TransitionConsequenceKind.OutpostConcession, StructuralSignalPolarity.Loss))
        .collect { case (StrategicMechanismKind.Activity, signal) => signal }

    assertEquals(gainSignals.map(_.label), List("outpost-gain"))
    assertEquals(gainSignals.flatMap(_.axisKey), List("Activity:Gain:outpost-gain"))
    assertEquals(concessionSignals.map(_.label), List("outpost-concession"))
    assertEquals(concessionSignals.flatMap(_.axisKey), List("Activity:Loss:outpost-concession"))

  test("preserves opponent restriction target and axis detail"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val ref =
      EvidenceRef(
        id = "strategic:counterplay-restraint",
        producer = EvidenceProducer.StrategicFeatureProducer,
        layer = EvidenceLayer.Strategic,
        position = root,
        line = None,
        scope = EvidenceScope.BeforePosition,
        confidence = EvidenceConfidence.Heuristic
      )
    val anchor =
      BoardAnchor(
        kind = BoardAnchorKind.CounterplayRestraint,
        side = Color.White,
        signal = BoardAnchorSignal.OpponentLowMobility,
        magnitude = 4,
        confidence = 0.72,
        detail = Some(BoardAnchorDetail(subjectColor = Some(Color.Black)))
      )
    val record =
      EvidenceRecord(
        ref = ref,
        payload = StrategicFactEvidence(
          kind = StrategicFactKind.CounterplayRestraint,
          facts = Nil,
          relatedPlans = Nil,
          confidence = 0.72
        )(boardAnchors = List(anchor))
      )

    val signals = StrategicMechanismEvidence.sourceMechanisms(record).map(_._2)
    val signatures = EvidenceObjectBinding.objectSignatures(EvidenceObjectBinding.fromEvidenceRefs(TypedEvidenceGraph(List(record)), List(ref)))

    assertEquals(signals.map(_.label), List("opponent-low-mobility"))
    assertEquals(signals.flatMap(_.axisKey), List("Counterplay:Restrain:opponent-low-mobility"))
    assert(signatures.exists(_.contains("target=Side:black")))
    assert(signatures.exists(_.contains("mechanism=Mechanism:opponentlowmobility")))

  test("drafts current move target pressure from structural strategic support without eval gap"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "d1b3", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "d1b3", 1, LineNodeRole.BestReference)
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
    val profile = exactPlayedProfile(referenceLine, playedLine, List(mechanismRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.TargetPressureGain))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Candidate)))
    assertEquals(drafts.map(_.attributionKind), List(CauseAttributionKind.CandidateCreatesValue))

  test("drafts current move pawn break from structural tension gain without eval gap"):
    val root = PositionNodeRef("8/8/8/3p4/8/8/4P3/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "e2e4", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "e2e4", 1, LineNodeRole.BestReference)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:e2e4:tension",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = "strategic-mechanism:pawn-break:e2e4",
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
            label = "break-file-e-created-tension-e4-d5",
            source = structuralRef,
            strength = 3,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "break-file-e-created-tension-e4-d5"))
          )
        ),
        semanticAnchors = Nil
      )
    )
    val profile = exactPlayedProfile(referenceLine, playedLine, List(mechanismRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.PawnBreakOpportunity))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Candidate)))
    assertEquals(drafts.map(_.attributionKind), List(CauseAttributionKind.CandidateCreatesValue))

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

    assertEquals(RelativeCauseDraftPlanner.drafts(profile), Nil)

  test("does not draft current move pawn break from file-only break-ready axis"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "d2d3", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "d2d3", 1, LineNodeRole.BestReference)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:d2d3:file-only-break",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = "strategic-mechanism:pawn-break:d2d3:file-only",
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
            label = "break-file-d",
            source = structuralRef,
            strength = 2,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "break-file-d"))
          )
        ),
        semanticAnchors = Nil
      )
    )
    val profile = exactPlayedProfile(referenceLine, playedLine, List(mechanismRecord))

    assertEquals(RelativeCauseDraftPlanner.drafts(profile), Nil)

  test("does not draft current move pawn break preparation from a mixed release axis"):
    val root = PositionNodeRef("8/8/8/3p4/2P5/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "c4d5", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "c4d5", 1, LineNodeRole.BestReference)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:c4d5:mixed-release",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = "strategic-mechanism:pawn-break:c4d5:mixed-release",
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
            label = "break-file-c-break-file-d-created-tension-d5-e6-resolved-tension-c4-d5",
            source = structuralRef,
            strength = 3,
            axis = Some(
              StrategicAxisDetail(
                StrategicAxisKind.PawnBreak,
                StrategicAxisPolarity.Support,
                "break-file-c-break-file-d-created-tension-d5-e6-resolved-tension-c4-d5"
              )
            )
          ),
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.StructuralDelta,
            label = "break-file-c-release-c4-d5",
            source = structuralRef,
            strength = 3,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "break-file-c-release-c4-d5"))
          )
        ),
        semanticAnchors = Nil
      )
    )
    val profile = exactPlayedProfile(referenceLine, playedLine, List(mechanismRecord))

    assertEquals(RelativeCauseDraftPlanner.drafts(profile), Nil)

  test("does not draft current move pawn break from another played line with the same root move"):
    val root = PositionNodeRef("8/8/8/3p4/8/8/4P3/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "e2e4", 1, LineNodeRole.Played)
    val otherPlayedLine = LineNodeRef("other-played-line", "e2e4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "e2e4", 1, LineNodeRole.BestReference)
    val structuralRef = EvidenceRef(
      id = "structural-delta:other-played:e2e4:tension",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(otherPlayedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = "strategic-mechanism:pawn-break:e2e4:mismatched-line",
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
            label = "break-file-e-created-tension-e4-d5",
            source = structuralRef,
            strength = 3,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "break-file-e-created-tension-e4-d5"))
          )
        ),
        semanticAnchors = Nil
      )
    )
    val profile = exactPlayedProfile(referenceLine, playedLine, List(mechanismRecord))

    assertEquals(RelativeCauseDraftPlanner.drafts(profile), Nil)

  test("does not draft current move support from a reference line mechanism"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "g1f3", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "d1b3", 1, LineNodeRole.BestReference)
    val structuralRef = EvidenceRef(
      id = "structural-delta:reference:d1b3:target-b7",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.ReferenceTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = "strategic-mechanism:target-pressure:d1b3",
        producer = EvidenceProducer.StrategicMechanismProducer,
        layer = EvidenceLayer.StrategicMechanism,
        position = root,
        line = Some(referenceLine),
        scope = EvidenceScope.ReferenceTransition,
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
    val profile = exactPlayedProfile(referenceLine, playedLine, List(mechanismRecord))

    assertEquals(RelativeCauseDraftPlanner.drafts(profile), Nil)

  test("does not draft current move support from a payload-wide gain on another signal"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "d1b3", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "d1b3", 1, LineNodeRole.BestReference)
    val playedStructuralRef = EvidenceRef(
      id = "structural-delta:played:d1b3:release-b7",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val referenceStructuralRef = EvidenceRef(
      id = "structural-delta:reference:d1b3:gain-b7",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.ReferenceTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = "strategic-mechanism:mixed-target-pressure:d1b3",
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
            label = "target-pressure-release",
            source = playedStructuralRef,
            strength = 2,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Release, "target-pressure-release"))
          ),
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.StructuralDelta,
            label = "target-pressure-gain",
            source = referenceStructuralRef,
            strength = 3,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, "target-pressure-gain"))
          )
        ),
        semanticAnchors = Nil
      )
    )
    val profile = exactPlayedProfile(referenceLine, playedLine, List(mechanismRecord))

    assertEquals(RelativeCauseDraftPlanner.drafts(profile), Nil)

  test("strategic proof signals keep only the selected current structural source"):
    val root = PositionNodeRef("8/8/8/8/8/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val playedLine = LineNodeRef("played-line", "d1b3", 1, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "d1b3", 1, LineNodeRole.BestReference)
    val after = PositionNodeRef("8/8/8/8/8/1Q6/1p6/8 b - - 1 1", 2, Some(Color.Black), Some("after"))
    val playedStructuralRef = EvidenceRef(
      id = "structural-delta:played:d1b3:gain-b7",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val referenceStructuralRef = EvidenceRef(
      id = "structural-delta:reference:d1b3:gain-h7",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(referenceLine),
      scope = EvidenceScope.ReferenceTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    def structural(line: LineNodeRef, target: String) =
      StructuralDeltaEvidence(
        transition = StructuralTransitionBinding(
          moveUci = "d1b3",
          role = TransitionEdgeRole.Played,
          from = root,
          to = after,
          line = Some(line),
          perspective = Color.White
        ),
        signals = Nil,
        consequences = List(
          TransitionConsequence(
            TransitionConsequenceKind.TargetPressureGain,
            StructuralSignalPolarity.Gain,
            strength = 3,
            subjects = List(target)
          )
        )
      )
    val playedSignal = StrategicMechanismSignal(
      kind = StrategicMechanismSignalKind.StructuralDelta,
      label = "target-pressure-gain-b7",
      source = playedStructuralRef,
      strength = 3,
      axis = Some(StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, "target-pressure-gain-b7"))
    )
    val referenceSignal = StrategicMechanismSignal(
      kind = StrategicMechanismSignalKind.StructuralDelta,
      label = "target-pressure-gain-h7",
      source = referenceStructuralRef,
      strength = 3,
      axis = Some(StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, "target-pressure-gain-h7"))
    )
    val payload = StrategicMechanismEvidence(
      kind = StrategicMechanismKind.TargetPressure,
      signals = List(playedSignal, referenceSignal),
      semanticAnchors = Nil
    )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(playedStructuralRef, structural(playedLine, "b7")),
        EvidenceRecord(referenceStructuralRef, structural(referenceLine, "h7"))
      )
    )

    val signals = RelativeAssessmentAssembler.strategicMechanismProofSignals(
      graph,
      RelativeCauseKind.TargetPressureGain,
      payload,
      RelativeCauseSourceSide.Candidate,
      selectedStructuralSourceIds = Set(playedStructuralRef.id)
    )

    assertEquals(signals.map(_.source.id), List(playedStructuralRef.id))
    assertEquals(signals.map(_.label), List("target-pressure-gain-b7"))

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
      payload(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, "break-file-e-created-tension-e4-d5")),
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
          "break-file-e-break-file-d-created-tension-d5-e6-resolved-tension-e4-d5"
        )
      ),
      RelativeCauseSourceSide.Candidate,
      selectedStructuralSourceIds = Set(structuralRef.id)
    )
    val releaseSignals = RelativeAssessmentAssembler.strategicMechanismProofSignals(
      graph,
      RelativeCauseKind.PawnBreakOpportunity,
      payload(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Release, "break-file-e-resolved-tension-e4-d5")),
      RelativeCauseSourceSide.Candidate,
      selectedStructuralSourceIds = Set(structuralRef.id)
    )

    assertEquals(supportSignals, Nil)
    assertEquals(mixedReleaseSignals, Nil)
    assertEquals(releaseSignals.map(_.source.id), List(structuralRef.id))

  test("same-root counter-break proof signals can use the played break carrier"):
    val root = PositionNodeRef("8/8/8/2p5/3P4/8/8/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/3P4/8/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after"))
    val playedLine = LineNodeRef("played-line", "d4d5", 1, LineNodeRole.Played)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:d4d5:counter-break-carrier",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val structural = StructuralDeltaEvidence(
      transition = StructuralTransitionBinding(
        moveUci = "d4d5",
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
          subjects = List("break-file:d", "resolved-tension:d4-c5")
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
    val counterBreakSignals = RelativeAssessmentAssembler.strategicMechanismProofSignals(
      graph,
      RelativeCauseKind.OpponentRestriction,
      payload(StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "defensive-counter-break-c")),
      RelativeCauseSourceSide.Candidate,
      selectedStructuralSourceIds = Set(structuralRef.id)
    )
    val genericSignals = RelativeAssessmentAssembler.strategicMechanismProofSignals(
      graph,
      RelativeCauseKind.OpponentRestriction,
      payload(StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "opponent-low-mobility")),
      RelativeCauseSourceSide.Candidate,
      selectedStructuralSourceIds = Set(structuralRef.id)
    )

    assertEquals(counterBreakSignals.map(_.source.id), List(structuralRef.id))
    assertEquals(genericSignals, Nil)

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

    assertEquals(RelativeCauseDraftPlanner.drafts(profile), Nil)

  test("prefers opponent restriction root over plan contradiction when counterplay restraint axis is concrete"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "h2h3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "g1f3", 2, LineNodeRole.Played)
    val contrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "opponent-low-mobility"),
          StrategicAxisComparisonOutcome.ReferenceOnly
        ),
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.PlanCoherence, StrategicAxisPolarity.Support, "ProphylaxisRestraint"),
          StrategicAxisComparisonOutcome.ReferenceOnly
        )
      ),
      planComparison = Some(
        StrategicPlanComparison(
          referencePlanIds = List("ProphylaxisRestraint"),
          candidatePlanIds = Nil,
          outcome = StrategicAxisComparisonOutcome.ReferenceOnly
        )
      )
    )
    val profile = playedVsBestProfile(referenceLine, playedLine, List(contrastRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.OpponentRestriction))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Reference)))

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
    assertEquals(drafts.map(_.kind), Nil)

  test("prefers candidate target pressure release root over plan contradiction when release axis is concrete"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val releaseSource = EvidenceRef(
      id = "strategic-mechanism:target-pressure-release:played",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val contrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Release, "target-pressure-release"),
          StrategicAxisComparisonOutcome.CandidateOnly,
          candidateSources = List(releaseSource)
        ),
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
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.TargetPressureRelease))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Candidate)))

  test("does not treat candidate target pressure release as positive target pressure gain"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "d2d4", 2, LineNodeRole.Alternative)
    val releaseSource = EvidenceRef(
      id = "strategic-mechanism:target-pressure-release:candidate",
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
          StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Release, "target-pressure-release"),
          StrategicAxisComparisonOutcome.CandidateOnly,
          candidateSources = List(releaseSource)
        )
      ),
      planComparison = None,
      comparisonKind = CandidateComparisonKind.PlayedVsAlternative
    )
    val profile = RelativeCauseSignalProfile.from(
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
      candidateRecords = List(contrastRecord),
      sharedRecords = Nil
    )

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(drafts.map(_.kind), Nil)

  test("maps candidate positive activity axis to concrete activity gain"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "d2d4", 2, LineNodeRole.Alternative)
    val activitySource = EvidenceRef(
      id = "strategic-mechanism:activity-gain:candidate",
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
          StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity-gain"),
          StrategicAxisComparisonOutcome.CandidateOnly,
          candidateSources = List(activitySource)
        )
      ),
      planComparison = None,
      comparisonKind = CandidateComparisonKind.PlayedVsAlternative
    )
    val profile = candidateBetterProfile(referenceLine, candidateLine, List(contrastRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.ActivityGain))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Candidate)))

  test("lets concrete current move battery pressure own candidate activity value"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("8/8/7Q/8/8/8/3P4/2B5 b - - 1 1", 2, Some(Color.Black), Some("after"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "h1h6", 2, LineNodeRole.Alternative)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:h1h6:battery",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRef = EvidenceRef(
      id = "strategic-mechanism:activity:h1h6:battery",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val transition = StructuralTransitionBinding(
      moveUci = "h1h6",
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

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.ActivityGain))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Candidate)))

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

    assertEquals(RelativeCauseDraftPlanner.drafts(profile).map(_.kind), Nil)

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

    assertEquals(RelativeCauseDraftPlanner.drafts(profile).map(_.kind), Nil)

  test("does not let generic current move mobility own candidate activity value"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val after = PositionNodeRef("8/8/8/8/8/5N2/3P4/8 b - - 1 1", 2, Some(Color.Black), Some("after"))
    val referenceLine = LineNodeRef("reference-line", "d2d4", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "g1f3", 2, LineNodeRole.Alternative)
    val structuralRef = EvidenceRef(
      id = "structural-delta:played:g1f3:mobility",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val mechanismRef = EvidenceRef(
      id = "strategic-mechanism:activity:g1f3:mobility",
      producer = EvidenceProducer.StrategicMechanismProducer,
      layer = EvidenceLayer.StrategicMechanism,
      position = root,
      line = Some(candidateLine),
      scope = EvidenceScope.PlayedTransition,
      confidence = EvidenceConfidence.EngineBacked
    )
    val transition = StructuralTransitionBinding(
      moveUci = "g1f3",
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
            TransitionConsequenceKind.DevelopmentMobilityGain,
            StructuralSignalPolarity.Gain,
            strength = 1,
            subjects = List("knight:g1-f3:mobility+2")
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
            source = structuralRef,
            strength = 2,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity-gain"))
          )
        ),
        semanticAnchors = Nil
      ),
      parents = List(structuralRef)
    )
    val profile = candidateBetterProfile(referenceLine, candidateLine, List(structuralRecord, mechanismRecord))

    assertEquals(RelativeCauseDraftPlanner.drafts(profile).map(_.kind), Nil)

  test("drafts same-root counter-break support only with a concrete break carrier"):
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
            label = "defensive-counter-break-c",
            source = pawnRef,
            strength = 2,
            axis = Some(StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "defensive-counter-break-c"))
          )
        ),
        semanticAnchors = Nil
      ),
      parents = List(structuralRef)
    )
    val exactReferenceLine = LineNodeRef("exact-reference-line", "d4d5", 1, LineNodeRole.BestReference)
    val sharedContrastRecord = strategicContrastRecord(
      root = root,
      referenceLine = exactReferenceLine,
      candidateLine = candidateLine,
      comparisons = List(
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "defensive-counter-break-c"),
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

    val exactContrastDrafts = RelativeCauseDraftPlanner.drafts(exactContrastProfile)

    assertEquals(RelativeCauseDraftPlanner.drafts(profile).map(_.kind), List(RelativeCauseKind.OpponentRestriction))
    assertEquals(RelativeCauseDraftPlanner.drafts(candidateBetterProfile(referenceLine, candidateLine, List(mechanismRecord))).map(_.kind), Nil)
    assertEquals(RelativeCauseDraftPlanner.drafts(exactProfile).map(_.kind), List(RelativeCauseKind.OpponentRestriction))
    assertEquals(exactContrastDrafts.map(_.kind), List(RelativeCauseKind.OpponentRestriction))
    assert(exactContrastDrafts.exists(_.support.exists(_.ref.id == structuralRef.id)), exactContrastDrafts)
    assertEquals(RelativeCauseDraftPlanner.drafts(exactContrastRefProfile).map(_.kind), List(RelativeCauseKind.OpponentRestriction))

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
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.OpponentRestriction))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Candidate)))

  test("maps candidate positive counterplay support axis without requiring plan improvement"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "h2h3", 2, LineNodeRole.Alternative)
    val counterplaySource = EvidenceRef(
      id = "strategic-mechanism:counterplay-race:candidate",
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
          StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Support, "counterplay-race"),
          StrategicAxisComparisonOutcome.CandidateOnly,
          candidateSources = List(counterplaySource)
        )
      ),
      planComparison = None,
      comparisonKind = CandidateComparisonKind.PlayedVsAlternative
    )
    val profile = candidateBetterProfile(referenceLine, candidateLine, List(contrastRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.StructuralImprovement))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Candidate)))

  test("keeps plan improvement as a companion only when counterplay support has a plan delta"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate-line", "h2h3", 2, LineNodeRole.Alternative)
    val counterplaySource = EvidenceRef(
      id = "strategic-mechanism:counterplay-race:candidate-plan",
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
          StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Support, "counterplay-race"),
          StrategicAxisComparisonOutcome.CandidateOnly,
          candidateSources = List(counterplaySource)
        ),
        strategicAxisComparison(
          StrategicAxisDetail(StrategicAxisKind.PlanCoherence, StrategicAxisPolarity.Support, "FlankCounterplay"),
          StrategicAxisComparisonOutcome.CandidateOnly,
          candidateSources = List(counterplaySource)
        )
      ),
      planComparison = Some(
        StrategicPlanComparison(
          referencePlanIds = Nil,
          candidatePlanIds = List("FlankCounterplay"),
          outcome = StrategicAxisComparisonOutcome.CandidateOnly
        )
      ),
      comparisonKind = CandidateComparisonKind.PlayedVsAlternative
    )
    val profile = candidateBetterProfile(referenceLine, candidateLine, List(contrastRecord))

    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(drafts.map(_.kind).toSet, Set(RelativeCauseKind.StructuralImprovement, RelativeCauseKind.PlanImprovement))
    assertEquals(drafts.flatMap(_.sourceSide).toSet, Set(RelativeCauseSourceSide.Candidate))

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
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.PlanContradiction))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Candidate)))

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
    assertEquals(drafts.map(_.kind), List(RelativeCauseKind.PawnBreakOpportunity))
    assertEquals(drafts.map(_.sourceSide), List(Some(RelativeCauseSourceSide.Reference)))

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
