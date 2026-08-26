package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }
import lila.chessjudgment.analysis.line.LineFactNormalizer
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  PrincipalVariationEvidence
}
import lila.chessjudgment.model.strategic.EngineLine

class SacrificeCompensationDetectionTest extends munit.FunSuite:

  private val fixture = compensationFixture()

  test("a raw structural return cannot manufacture sacrifice compensation"):
    val sacrificeEpisodes = fixture.line
      .rootOwnedCausalEpisodes(fixture.candidateLine.rootMove)
      .filter(_.consequence.kind == LineConsequenceKind.Sacrifice)
    assert(sacrificeEpisodes.nonEmpty)

    val support = RelativeCauseSignalProfile.sacrificeCompensationSupportRecords(
      fixture.records,
      fixture.candidateLine
    )
    assertEquals(support, Nil)
    assertEquals(StrategicMechanismEvidence.sourceMechanisms(fixture.structuralRecord), Nil)

    val profileGraph = fixture.records.foldLeft(TypedEvidenceGraph.empty)((graph, record) => graph.add(record))
    val profile = RelativeCauseSignalProfile.from(
      fact = fixture.fact,
      graph = profileGraph,
      referenceRecords = Nil,
      candidateRecords = fixture.records.filterNot(_.ref == fixture.comparisonRecord.ref),
      sharedRecords = Nil
    )
    val compensationDrafts = RelativeCauseDraftPlanner
      .drafts(profile, fixture.comparisonRecord)
      .filter(draft =>
        draft.kind == RelativeCauseKind.SacrificeCompensation &&
          draft.sourceSide.contains(RelativeCauseSourceSide.Candidate)
      )
    assertEquals(compensationDrafts, Nil)

  test("a strategic wrapper cannot restore authority to an unproved structural leaf"):
    val graph = fixture.records.foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    val binding = RelativeCauseBinding(
      role = RelativeCauseRole.PrimaryPlayedCause,
      sourceSide = RelativeCauseSourceSide.Candidate,
      eventLine = fixture.candidateLine,
      evidenceLines = List(fixture.referenceLine, fixture.candidateLine),
      bindingTier = RelativeCauseBindingTier.Primary
    )
    val direct = RelativeAssessmentAssembler.ownedCauseDirectProofRecords(
      graph = graph,
      fact = fixture.fact,
      kind = RelativeCauseKind.SacrificeCompensation,
      binding = binding,
      attributionKind = CauseAttributionKind.CandidateCreatesValue,
      support = List(fixture.lineRecord, fixture.contrastRecord)
    )

    assertEquals(direct, Nil)
    assertEquals(
      RelativeAssessmentAssembler.ownedCauseDirectProofRecords(
        graph = graph,
        fact = fixture.fact,
        kind = RelativeCauseKind.SacrificeCompensation,
        binding = binding,
        attributionKind = CauseAttributionKind.CandidateCreatesValue,
        support = List(fixture.structuralRecord, fixture.mechanismRecord)
      ),
      Nil
    )

  private final case class CompensationFixture(
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      line: LineFactEvidence,
      lineRecord: EvidenceRecord,
      returnConsequence: TransitionConsequence,
      structuralRecord: EvidenceRecord,
      mechanismRecord: EvidenceRecord,
      axis: StrategicAxisDetail,
      axisComparison: StrategicAxisComparison,
      contrast: StrategicMechanismContrastEvidence,
      contrastRecord: EvidenceRecord,
      fact: CandidateComparisonFact,
      comparisonRecord: EvidenceRecord,
      records: List[EvidenceRecord]
  )

  private def compensationFixture(): CompensationFixture =
    val fen = "6k1/8/8/6p1/5p2/8/4N3/4R1K1 w - - 0 1"
    val position = PositionNodeRef(fen, 0, Some(White))
    val referenceLine = LineNodeRef("safe-reference", "e2g3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("synthetic-sacrifice", "e2f4", 2, LineNodeRole.Played)
    val line = normalizedLine(position, candidateLine, List("e2f4", "g5f4"))
    val lineRecord = EvidenceRecord(
      evidenceRef(
        "sacrifice-line",
        EvidenceProducer.LegalLineProducer,
        EvidenceLayer.Line,
        position,
        Some(candidateLine),
        candidateLine.role.scope,
        EvidenceConfidence.LegalReplayVerified
      ),
      line
    )
    val afterRoot = PrincipalVariationEvidence
      .legalFenAfter(fen, candidateLine.rootMove)
      .getOrElse(fail("expected legal synthetic sacrifice"))
    val returnConsequence = TransitionConsequence(
      kind = TransitionConsequenceKind.FileOccupationEstablished,
      strength = 2,
      subjectBindings = List(
        StructuralSubjectBinding.unbound(
          StructuralSubject.FileOccupation(
            EvidenceFile("e"),
            EvidenceSquare("e7"),
            EvidencePieceRole("rook")
          )
        )
      )
    )
    val transition = StructuralTransitionBinding(
        moveUci = candidateLine.rootMove,
        role = TransitionEdgeRole.Played,
        from = position,
        to = PositionNodeRef(afterRoot, 1, Some(Black)),
        line = Some(candidateLine),
        perspective = White
      )
    val rootReplay = line.certifiedReplay
      .flatMap(_.subset(line.lineReplaySteps.take(1)))
      .getOrElse(fail("expected the normalized sacrifice root replay"))
    val transitionProof = CanonicalTransitionProof
      .from(transition, rootReplay)
      .getOrElse(fail("expected the sacrifice transition certificate"))
    val structural = StructuralDeltaEvidence(
      transition = transition,
      signals = Nil,
      consequences = List(returnConsequence),
      relationChanges = Nil,
      canonicalTransitionProof = Some(transitionProof),
      canonicalDeltaProof = None
    )
    val structuralRecord = EvidenceRecord(
      evidenceRef(
        "activity-return",
        EvidenceProducer.StructuralDeltaProducer,
        EvidenceLayer.StructuralDelta,
        position,
        Some(candidateLine),
        EvidenceScope.PlayedTransition,
        EvidenceConfidence.LegalReplayVerified
      ),
      structural
    )
    val axis = StrategicAxisDetail(
      StrategicAxisKind.Counterplay,
      StrategicAxisPolarity.Restrain,
      "unproved-counterplay-return"
    )
    val mechanismRecord = EvidenceRecord(
      evidenceRef(
        "activity-mechanism",
        EvidenceProducer.StrategicMechanismProducer,
        EvidenceLayer.StrategicMechanism,
        position,
        Some(candidateLine),
        EvidenceScope.PlayedTransition,
        EvidenceConfidence.LegalReplayVerified
      ),
      StrategicMechanismEvidence(
        kind = StrategicMechanismKind.PlanPressure,
        signals = List(
          StrategicMechanismSignal(
            kind = StrategicMechanismSignalKind.PlanPressure,
            label = axis.label,
            source = structuralRecord.ref,
            axis = Some(axis)
          )
        ),
        semanticAnchors = Nil
      ),
      parents = List(structuralRecord.ref)
    )
    val axisComparison = StrategicAxisComparison(
      axis = axis,
      outcome = StrategicAxisComparisonOutcome.CandidateOnly,
      referenceSources = Nil,
      candidateSources = List(mechanismRecord.ref, structuralRecord.ref)
    )
    val contrast = StrategicMechanismContrastEvidence(
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      axisComparisons = List(axisComparison),
      planComparison = None,
      sustainability = StrategicSustainabilityAssessment(
        horizon = StrategicSustainabilityHorizon.ShortPv,
        lineMaintained = true,
        pvMaintained = true,
        referencePlyCount = 2,
        candidatePlyCount = 2,
        sustainedAxisKeys = List(axis.stableKey)
      ),
      support = StrategicContrastSupport(
        directSources = List(mechanismRecord.ref, structuralRecord.ref),
        contrastSources = Nil,
        contextSources = Nil
      )
    )
    val fact = comparison(position, referenceLine, candidateLine)
    val comparisonRecord = EvidenceRecord(
      evidenceRef(
        "sacrifice-comparison",
        EvidenceProducer.RelativeMoveProducer,
        EvidenceLayer.CandidateComparison,
        position,
        Some(candidateLine),
        EvidenceScope.Counterfactual,
        EvidenceConfidence.EngineBacked
      ),
      CandidateComparisonEvidence(fact),
      parents = List(lineRecord.ref)
    )
    val contrastRecord = EvidenceRecord(
      evidenceRef(
        "sacrifice-return-contrast",
        EvidenceProducer.StrategicMechanismProducer,
        EvidenceLayer.StrategicMechanism,
        position,
        Some(candidateLine),
        EvidenceScope.Counterfactual,
        EvidenceConfidence.EngineBacked
      ),
      contrast,
      parents = List(comparisonRecord.ref, mechanismRecord.ref, structuralRecord.ref)
    )
    val records = List(
      comparisonRecord,
      lineRecord,
      structuralRecord,
      mechanismRecord,
      contrastRecord
    )
    CompensationFixture(
      referenceLine,
      candidateLine,
      line,
      lineRecord,
      returnConsequence,
      structuralRecord,
      mechanismRecord,
      axis,
      axisComparison,
      contrast,
      contrastRecord,
      fact,
      comparisonRecord,
      records
    )

  private def normalizedLine(
      position: PositionNodeRef,
      line: LineNodeRef,
      moves: List[String]
  ): LineFactEvidence =
    val lineHistory = CanonicalPositionHistory
      .from(position.fen, Nil, position.fen)
      .flatMap(_.extend(moves))
      .getOrElse(fail("expected canonical sacrifice line history"))
    val canonicalReplay = CanonicalLineReplay
      .fromHistory(lineHistory.segmentReplaySteps)
      .getOrElse(fail("expected canonical sacrifice line replay"))
    val moveRefs = canonicalReplay.replaySteps.map(step =>
      PrincipalVariationEvidence.LineMoveRef(step.ply, step.moveUci, step.fenAfter)
    )
    val facts = PrincipalVariationEvidence.LineFacts(
      line = PrincipalVariationEvidence.LineVariationRef(moveRefs),
      first = moveRefs.head,
      reply = moveRefs.lift(1),
      continuation = moveRefs.lift(2),
      continuationTail = moveRefs.drop(3)
    )
    val material = LineMaterialSummary(
      sideToMove = White,
      captures = List(
        LineMaterialCapture(
          moveUci = "e2f4",
          plyOffset = 0,
          side = White,
          attackerRole = EvidencePieceRole("knight"),
          capturedRole = EvidencePieceRole("pawn"),
          square = EvidenceSquare("f4"),
          valueCp = 100,
          recapture = false
        ),
        LineMaterialCapture(
          moveUci = "g5f4",
          plyOffset = 1,
          side = Black,
          attackerRole = EvidencePieceRole("pawn"),
          capturedRole = EvidencePieceRole("knight"),
          square = EvidenceSquare("f4"),
          valueCp = 300,
          recapture = true
        )
      ),
      netCaptureCpForMover = -200,
      maxGainCpForMover = 100,
      maxLossCpForMover = 200,
      hasRecaptureChain = true,
      hasRecoveryWindow = false,
      promotionGainCpForMover = 0,
      materialWindowComplete = true
    )
    LineFactNormalizer
      .fromValidatedLine(
        id = "synthetic-sacrifice-evidence",
        lineRef = line,
        facts = facts,
        replay = canonicalReplay,
        position = position,
        scope = line.role.scope,
        materialSummary = Some(material)
      )
      .payload match
      case payload: LineFactEvidence => payload
      case _                         => fail("expected normalized line fact")

  private def comparison(
      position: PositionNodeRef,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef
  ): CandidateComparisonFact =
    CandidateComparisonFact(
      kind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      comparison = EvalComparison.fromLines(
        White,
        CandidateLineNode(
          referenceLine,
          CandidateLineEvaluation.EngineSearch(EngineLine(List(referenceLine.rootMove), 400, depth = 20)),
          evalRef("reference-eval", position, referenceLine)
        ),
        CandidateLineNode(
          candidateLine,
          CandidateLineEvaluation.EngineSearch(EngineLine(List(candidateLine.rootMove), -600, depth = 20)),
          evalRef("candidate-eval", position, candidateLine)
        )
      ).getOrElse(fail("expected comparison")),
      verdictConfidence = VerdictConfidence.EngineBacked
    )

  private def evalRef(
      id: String,
      position: PositionNodeRef,
      line: LineNodeRef
  ): EvidenceRef =
    evidenceRef(
      id,
      EvidenceProducer.EngineEvalProducer,
      EvidenceLayer.Eval,
      position,
      Some(line),
      line.role.scope,
      EvidenceConfidence.EngineBacked
    )

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence
  ): EvidenceRef =
    EvidenceRef(id, producer, layer, position, line, scope, confidence)
