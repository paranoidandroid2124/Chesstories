package lila.chessjudgment.model.judgment

import chess.{ Queen, Rook, White, Black }
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.EngineLine

class ComparisonEndpointEffectObservationPolicyTest extends munit.FunSuite:

  private val rootFen = "4k3/8/8/8/8/8/8/3QK3 w - - 0 1"
  private val root = PositionNodeRef(rootFen, 0, Some(White))
  private val reference = LineNodeRef("reference", "d1d2", 1, LineNodeRole.BestReference)
  private val candidate = LineNodeRef("candidate", "d1h5", 2, LineNodeRole.Played)

  test("strategic endpoint outcome ignores which root piece realizes the shared axis"):
    val axis = StrategicAxisDetail(
      StrategicAxisKind.Target,
      StrategicAxisPolarity.Gain,
      "king-target"
    )
    val referenceObservation = ComparisonEndpointEffectObservationPolicy
      .fromStrategicAxis(root, reference, axis, strength = 3)
      .getOrElse(fail("expected reference observation"))
    val candidateObservation = ComparisonEndpointEffectObservationPolicy
      .fromStrategicAxis(root, candidate, axis, strength = 3)
      .getOrElse(fail("expected candidate observation"))
    val kingLine = LineNodeRef("king-candidate", "e1f1", 3, LineNodeRole.Alternative)
    val differentActor = ComparisonEndpointEffectObservationPolicy
      .fromStrategicAxis(root, kingLine, axis, strength = 3)
      .getOrElse(fail("expected different-actor observation"))

    assertEquals(referenceObservation.scope, candidateObservation.scope)
    assertEquals(referenceObservation.scope, differentActor.scope)
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.compareMagnitude(
        referenceObservation.magnitude,
        candidateObservation.magnitude
      ),
      ComparisonEndpointEffectObservationPolicy.MagnitudeRelation.Equal
    )

  test("material timing belongs to typed magnitude and Pareto trade-offs fail closed"):
    val earlier = materialObservation(
      reference,
      eventPly = 2,
      valueCp = 500,
      LineConsequenceKind.MaterialGain,
      beneficiary = White,
      captureSide = White
    )
    val later = materialObservation(
      candidate,
      eventPly = 4,
      valueCp = 500,
      LineConsequenceKind.MaterialGain,
      beneficiary = White,
      captureSide = White
    )

    assertEquals(earlier.scope, later.scope, "ply timing must not be duplicated in scope.horizon")
    assertEquals(earlier.scope.horizon, None)
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.compareMagnitude(earlier.magnitude, later.magnitude),
      ComparisonEndpointEffectObservationPolicy.MagnitudeRelation.LeftStrictlyStronger
    )
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.compareMagnitude(
        ComparisonEndpointEffectMagnitude.Exact(
          DirectCauseImportanceMeasure.MaterialOutcome(500, 5)
        ),
        ComparisonEndpointEffectMagnitude.Exact(
          DirectCauseImportanceMeasure.MaterialOutcome(300, 3)
        )
      ),
      ComparisonEndpointEffectObservationPolicy.MagnitudeRelation.Incomparable
    )

  test("material endpoint compares durable outcome instead of captured-target salience"):
    val queenExchange = materialObservation(
      reference,
      eventPly = 2,
      valueCp = 900,
      LineConsequenceKind.MaterialGain,
      beneficiary = White,
      captureSide = White,
      durableNetCp = Some(400)
    )
    val freeRook = materialObservation(
      candidate,
      eventPly = 2,
      valueCp = 500,
      LineConsequenceKind.MaterialGain,
      beneficiary = White,
      captureSide = White,
      durableNetCp = Some(500)
    )

    assertEquals(queenExchange.scope, freeRook.scope)
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.compareMagnitude(
        queenExchange.magnitude,
        freeRook.magnitude
      ),
      ComparisonEndpointEffectObservationPolicy.MagnitudeRelation.RightStrictlyStronger
    )

  test("material gain for the opponent and material loss share one liability scope"):
    val opponentGain = materialObservation(
      reference,
      eventPly = 2,
      valueCp = 500,
      LineConsequenceKind.MaterialGain,
      beneficiary = Black,
      captureSide = Black
    )
    val moverLoss = materialObservation(
      candidate,
      eventPly = 2,
      valueCp = 500,
      LineConsequenceKind.MaterialLoss,
      beneficiary = Black,
      captureSide = Black
    )

    assertEquals(opponentGain.scope.stake, RootOwnedEffectStake.ActorLiability)
    assertEquals(opponentGain.scope, moverLoss.scope)
    assertEquals(
      opponentGain.scope.consequenceSignatures,
      List("consequence:material-transfer")
    )

  test("complete inventories require a unique observation per semantic scope"):
    val observation = ComparisonEndpointEffectObservationPolicy
      .fromStrategicAxis(
        root,
        reference,
        StrategicAxisDetail(
          StrategicAxisKind.Activity,
          StrategicAxisPolarity.Gain,
          "activity"
        ),
        3
      )
      .getOrElse(fail("expected observation"))
    val ambiguous = observation.copy(
      magnitude = ComparisonEndpointEffectMagnitude.Exact(
        DirectCauseImportanceMeasure.StrategicStrength(5)
      )
    )

    assertEquals(
      EvidenceObjectBinding.completeEndpointObservations(
        List(Some(observation), Some(observation))
      ),
      Some(Set(observation)),
      "support-id duplicates must canonicalize to one semantic observation"
    )
    assertEquals(
      EvidenceObjectBinding.completeEndpointObservations(
        List(Some(observation), Some(ambiguous))
      ),
      None,
      "one scope with conflicting typed magnitudes must fail closed"
    )

    assertEquals(
      ComparisonEndpointEffectObservationPolicy.uniqueObservationFor(
        ComparisonEndpointEffectInventory.Complete(Set(observation)),
        observation.scope
      ),
      Some(Some(observation))
    )
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.uniqueObservationFor(
        ComparisonEndpointEffectInventory.Complete(Set(observation, ambiguous)),
        observation.scope
      ),
      None
    )
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.uniqueObservationFor(
        ComparisonEndpointEffectInventory.Incomplete,
        observation.scope
      ),
      None
    )

  test("retained played value is PVB-only and proof segment ambiguity is presentation-only"):
    val fixture = retainedValueFixture()
    val raw = EvidenceObjectBinding
      .directCauseChannelsForProjection(fixture.cause, fixture.graph)
      .headOption
      .getOrElse(fail("expected candidate-owned relation channel"))
    val segmentAmbiguous = raw.copy(proofSegmentAmbiguous = true)

    assert(
      ComparisonEndpointEffectObservationPolicy.retainedPlayedValueReady(
        fixture.cause,
        segmentAmbiguous,
        fixture.comparison,
        fixture.graph
      )
    )
    assertEquals(segmentAmbiguous.proofSegment, None)
    assert(
      RootOwnedEffectTruthView.from(List(segmentAmbiguous)).certifiedExplanatoryAgreement.nonEmpty
    )

    val alternativeComparison = fixture.comparison.copy(
      kind = CandidateComparisonKind.PlayedVsAlternative
    )
    assert(
      !ComparisonEndpointEffectObservationPolicy.retainedPlayedValueReady(
        fixture.cause,
        segmentAmbiguous,
        alternativeComparison,
        fixture.graph
      )
    )
    val mismatchedPlayedVsBest = fixture.comparison.copy(
      candidateLine = fixture.comparison.referenceLine
    )
    assert(
      !ComparisonEndpointEffectObservationPolicy.retainedPlayedValueReady(
        fixture.cause,
        segmentAmbiguous,
        mismatchedPlayedVsBest,
        fixture.graph
      )
    )

  test("proof segment variants merge as one causal representative"):
    val fixture = retainedValueFixture()
    val raw = EvidenceObjectBinding
      .directCauseChannelsForProjection(fixture.cause, fixture.graph)
      .headOption
      .getOrElse(fail("expected relation channel"))
    val canonical = EvidenceObjectBinding.canonicalCauseChannels(
      List(raw, raw.copy(proofSegmentAmbiguous = true))
    )

    assertEquals(canonical.size, 1)
    assert(canonical.head.proofSegmentAmbiguous)
    assertEquals(canonical.head.proofSegment, None)
    assert(
      RootOwnedEffectTruthView.from(canonical).agreesCausallyWith(
        RootOwnedEffectTruthView.from(List(raw))
      )
    )

  test("complete line inventory carries mate magnitude and qualitative endgame conversion"):
    val mateFen = "7k/8/5KQ1/8/8/8/8/8 w - - 0 1"
    val matePosition = PositionNodeRef(mateFen, 0, Some(White))
    val mateLine = LineNodeRef("mate-line", "g6g7", 1, LineNodeRole.BestReference)
    val mateAfter = PrincipalVariationEvidence
      .legalFenAfter(mateFen, mateLine.rootMove)
      .getOrElse(fail("expected legal mate"))
    val mateConsequence = LineConsequence(
      LineConsequenceKind.Mate,
      List(mateLine.rootMove),
      proofSignal = true,
      eventMove = Some(mateLine.rootMove),
      rootMove = Some(mateLine.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White)
    )
    val matePayload = LineFactEvidence(
      line = mateLine,
      material = Some(emptyMaterial),
      replay = List(LineReplayStep(0, mateLine.rootMove, mateFen, mateAfter)),
      events = List(
        LineMoveEvent(
          kind = LineEventKind.Check,
          moveUci = mateLine.rootMove,
          plyOffset = 0,
          side = Some(White),
          pieceRole = Some(EvidencePieceRole("queen")),
          targetRole = Some(EvidencePieceRole("king")),
          square = Some(EvidenceSquare("h8"))
        ),
        LineMoveEvent(
          kind = LineEventKind.Mate,
          moveUci = mateLine.rootMove,
          plyOffset = 0,
          side = Some(White),
          pieceRole = Some(EvidencePieceRole("queen")),
          targetRole = Some(EvidencePieceRole("king")),
          square = Some(EvidenceSquare("h8"))
        )
      ),
      consequences = List(mateConsequence)
    )
    val mateRef = EvidenceRef(
      "mate-line-fact",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      matePosition,
      Some(mateLine),
      EvidenceScope.BestLine,
      EvidenceConfidence.LegalReplayVerified
    )
    val mateObservations = EvidenceObjectBinding
      .comparisonEndpointLineObservations(mateRef, matePayload, matePosition)
      .mate match
        case ComparisonEndpointEffectInventory.Complete(observations) => observations
        case ComparisonEndpointEffectInventory.Incomplete => fail("expected complete mate inventory")
    assertEquals(
      mateObservations.size,
      1,
      "the exact Mate episode must subsume only its duplicate qualitative Mate event"
    )
    assert(
      mateObservations.exists(
        _.magnitude == ComparisonEndpointEffectMagnitude.Exact(
          DirectCauseImportanceMeasure.MateArrival(0)
        )
      )
    )
    val mateQualitative = EvidenceObjectBinding
      .comparisonEndpointLineObservations(mateRef, matePayload, matePosition)
      .qualitative match
        case ComparisonEndpointEffectInventory.Complete(observations) => observations
        case ComparisonEndpointEffectInventory.Incomplete => fail("expected complete qualitative inventory")
    assertEquals(mateQualitative.size, 1, "the distinct Check event must remain observable")
    assert(mateQualitative.exists(
      _.scope.effectIdentity.primitiveKind == RootOwnedEffectPrimitiveKind.RootLineEvent
    ))

    val conversionFen = "8/1p1k4/1P6/2PK4/8/8/8/8 w - - 0 1"
    val conversionPosition = PositionNodeRef(conversionFen, 0, Some(White))
    val conversionLine = LineNodeRef("conversion-line", "d5d4", 1, LineNodeRole.BestReference)
    val conversionMoves = List("d5d4", "d7d8", "d4e5", "d8d7", "e5d5")
    val conversionReplay = legalReplay(conversionFen, conversionMoves)
    val conversionHorizon = LineEndgameTechniqueHorizon(
      pattern = "Lucena",
      rookPattern = Some("Lucena"),
      techniqueSide = White,
      entryPlyOffset = 0,
      terminalPlyOffset = 4,
      status = LineEndgameTechniqueHorizonStatus.Completed,
      triggerMove = Some(conversionLine.rootMove),
      requiredSquares = List("d4", "d5", "e5")
    )
    val conversionPayload = LineFactEvidence(
      line = conversionLine,
      material = Some(emptyMaterial),
      replay = conversionReplay,
      endgameHorizons = List(conversionHorizon)
    )
    val conversionRef = EvidenceRef(
      "conversion-line-fact",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      conversionPosition,
      Some(conversionLine),
      EvidenceScope.BestLine,
      EvidenceConfidence.LegalReplayVerified
    )
    val conversionObservations = EvidenceObjectBinding
      .comparisonEndpointLineObservations(
        conversionRef,
        conversionPayload,
        conversionPosition
      )
      .qualitative match
        case ComparisonEndpointEffectInventory.Complete(observations) => observations
        case ComparisonEndpointEffectInventory.Incomplete => fail("expected complete conversion inventory")
    assert(conversionObservations.exists(observation =>
      observation.scope.effectIdentity.primitiveKind == RootOwnedEffectPrimitiveKind.EndgameHorizon &&
        observation.magnitude == ComparisonEndpointEffectMagnitude.QualitativePresence
    ))

  test("eligible root event projection failure makes only qualitative inventory incomplete"):
    val replay = legalReplay(rootFen, List(reference.rootMove))
    val payload = LineFactEvidence(
      line = reference,
      material = Some(emptyMaterial),
      replay = replay,
      events = List(LineMoveEvent(
        kind = LineEventKind.Tempo,
        moveUci = reference.rootMove,
        plyOffset = 0,
        side = Some(White)
      ))
    )
    val inventories = EvidenceObjectBinding.comparisonEndpointLineObservations(
      evidenceRef(
        "sparse-root-event",
        reference,
        EvidenceLayer.Line,
        EvidenceProducer.LegalLineProducer,
        EvidenceScope.BestLine
      ),
      payload,
      root
    )

    assertEquals(inventories.material, ComparisonEndpointEffectInventory.Complete(Set.empty))
    assertEquals(inventories.mate, ComparisonEndpointEffectInventory.Complete(Set.empty))
    assertEquals(inventories.qualitative, ComparisonEndpointEffectInventory.Incomplete)

  test("eligible endgame horizon projection failure makes qualitative inventory incomplete"):
    val replay = legalReplay(rootFen, List(reference.rootMove))
    val payload = LineFactEvidence(
      line = reference,
      material = Some(emptyMaterial),
      replay = replay,
      endgameHorizons = List(LineEndgameTechniqueHorizon(
        pattern = "Lucena",
        rookPattern = Some("Lucena"),
        techniqueSide = White,
        entryPlyOffset = 0,
        terminalPlyOffset = 0,
        status = LineEndgameTechniqueHorizonStatus.Failed,
        triggerMove = Some(reference.rootMove),
        requiredSquares = Nil
      ))
    )
    val inventories = EvidenceObjectBinding.comparisonEndpointLineObservations(
      evidenceRef(
        "sparse-endgame-horizon",
        reference,
        EvidenceLayer.Line,
        EvidenceProducer.LegalLineProducer,
        EvidenceScope.BestLine
      ),
      payload,
      root
    )

    assertEquals(inventories.material, ComparisonEndpointEffectInventory.Complete(Set.empty))
    assertEquals(inventories.mate, ComparisonEndpointEffectInventory.Complete(Set.empty))
    assertEquals(inventories.qualitative, ComparisonEndpointEffectInventory.Incomplete)

  test("strategic Release has one exact stake and change table"):
    val pawnBreak = StrategicAxisDetail(
      StrategicAxisKind.PawnBreak,
      StrategicAxisPolarity.Release,
      "central-break"
    )
    val target = StrategicAxisDetail(
      StrategicAxisKind.Target,
      StrategicAxisPolarity.Release,
      "weak-target"
    )
    val activity = StrategicAxisDetail(
      StrategicAxisKind.Activity,
      StrategicAxisPolarity.Release,
      "piece-activity"
    )
    val generic = StrategicAxisDetail(
      StrategicAxisKind.SpaceCenter,
      StrategicAxisPolarity.Release,
      "space"
    )

    assertEquals(
      RootOwnedEffectPolicy.strategicAxisStake(pawnBreak),
      Some(RootOwnedEffectStake.ActorValue)
    )
    assertEquals(
      RootOwnedEffectPolicy.strategicAxisChange(pawnBreak),
      Some(DirectCausalChange.Occurred)
    )
    List(target, activity).foreach { axis =>
      assertEquals(
        RootOwnedEffectPolicy.strategicAxisStake(axis),
        Some(RootOwnedEffectStake.ActorLiability)
      )
      assertEquals(
        RootOwnedEffectPolicy.strategicAxisChange(axis),
        Some(DirectCausalChange.Lost)
      )
    }
    assertEquals(RootOwnedEffectPolicy.strategicAxisStake(generic), None)
    assertEquals(RootOwnedEffectPolicy.strategicAxisChange(generic), None)

  private final case class RetainedFixture(
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      graph: TypedEvidenceGraph
  )

  private def retainedValueFixture(): RetainedFixture =
    val relationRef = evidenceRef(
      "candidate-relation",
      candidate,
      EvidenceLayer.Relation,
      EvidenceProducer.TacticalRelationProducer,
      EvidenceScope.PlayedTransition
    )
    val relation = RelationFactEvidence
      .from(
        RelationWitnessDetail.HangingPiece(
          EvidenceSquare("h5"),
          EvidenceSquare("e8"),
          EvidencePieceRole(Queen.name),
          EvidencePieceRole("king")
        ),
        List(candidate.rootMove)
      )
      .getOrElse(fail("expected relation"))
    val comparison = comparisonFact(CandidateComparisonKind.PlayedVsBest, equalScore = true)
    val comparisonRef = EvidenceRef(
      "retained-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      root,
      Some(candidate),
      EvidenceScope.Counterfactual,
      EvidenceConfidence.EngineBacked
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.MaterialSwing,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(relationRef),
      sourceSide = RelativeCauseSourceSide.Candidate,
      attribution = CauseAttribution(
        CauseAttributionKind.CandidateCreatesValue,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(relationRef)
        )
      ))
    )
    val graph = List(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      EvidenceRecord(relationRef, relation)
    ).foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    RetainedFixture(cause, comparison, graph)

  private def comparisonFact(
      kind: CandidateComparisonKind,
      equalScore: Boolean
  ): CandidateComparisonFact =
    val referenceScore = 300
    val candidateScore = if equalScore then 300 else -300
    CandidateComparisonFact(
      kind,
      reference,
      candidate,
      EvalComparison.fromLines(
        White,
        CandidateLineNode(
          reference,
          EngineLine(List(reference.rootMove), referenceScore, depth = 18),
          evidenceRef(
            "reference-eval",
            reference,
            EvidenceLayer.Eval,
            EvidenceProducer.EngineEvalProducer,
            EvidenceScope.BestLine
          )
        ),
        CandidateLineNode(
          candidate,
          EngineLine(List(candidate.rootMove), candidateScore, depth = 18),
          evidenceRef(
            "candidate-eval",
            candidate,
            EvidenceLayer.Eval,
            EvidenceProducer.EngineEvalProducer,
            EvidenceScope.PlayedLine
          )
        )
      )
    )

  private def materialObservation(
      line: LineNodeRef,
      eventPly: Int,
      valueCp: Int,
      consequenceKind: LineConsequenceKind,
      beneficiary: chess.Color,
      captureSide: chess.Color,
      durableNetCp: Option[Int] = None
  ): ComparisonEndpointEffectObservation =
    val actor = RootCausalActor
      .fromPosition(root, line.rootMove)
      .getOrElse(fail("expected root actor"))
    val eventMove = if line == reference then "d2e8" else "h5e8"
    val capture = LineMaterialCapture(
      eventMove,
      eventPly,
      captureSide,
      EvidencePieceRole(Queen.name),
      EvidencePieceRole(Rook.name),
      EvidenceSquare("e8"),
      valueCp,
      recapture = false
    )
    val consequence = LineConsequence(
      consequenceKind,
      lineMoves = List(eventMove),
      proofSignal = true,
      eventMove = Some(eventMove),
      rootMove = Some(line.rootMove),
      rootSide = Some(White),
      beneficiary = Some(beneficiary),
      materialOutcome = Some(RootOwnedMaterialOutcome(
        event = RootOwnedMaterialEventSalience(
          moveUci = eventMove,
          plyOffset = eventPly,
          capturedRole = capture.capturedRole,
          square = capture.square,
          targetValueCp = capture.valueCp
        ),
        beneficiary = beneficiary,
        durableNetCp = durableNetCp.getOrElse(valueCp)
      ))
    )
    val replay = (0 to eventPly).toList.map { ply =>
      val move =
        if ply == 0 then line.rootMove
        else if ply == eventPly then eventMove
        else "e8e7"
      LineReplayStep(ply, move, rootFen, rootFen)
    }
    val payload = LineFactEvidence(
      line = line,
      material = Some(LineMaterialSummary(
        sideToMove = White,
        captures = List(capture),
        netCaptureCpForMover = if captureSide == White then valueCp else -valueCp,
        maxGainCpForMover = valueCp.max(0),
        maxLossCpForMover = valueCp.max(0),
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = replay,
      consequences = List(consequence)
    )
    val episode = RootOwnedCausalEpisode(
      line = line,
      actor = actor,
      target = EvidenceSquare("e8"),
      links = List(RootCausalLink(
        RootCausalLinkKind.ContinuousLineAccess,
        line.rootMove,
        eventMove,
        EvidenceSquare("e8")
      )),
      consequence = consequence,
      eventPlyOffset = eventPly,
      chainMoves = replay.map(_.moveUci)
    )
    val source = evidenceRef(
      s"${line.id}-material",
      line,
      EvidenceLayer.Line,
      EvidenceProducer.LegalLineProducer,
      line.role.scope
    )
    val binding = EvidenceObjectBinding(
      source = source,
      actor = List(
        ConcreteChessObject(EvidenceObjectKind.Side, "white"),
        ConcreteChessObject(EvidenceObjectKind.Piece, Queen.name),
        ConcreteChessObject(EvidenceObjectKind.Square, "d1")
      ),
      target = List(
        ConcreteChessObject(EvidenceObjectKind.Square, "e8"),
        ConcreteChessObject(EvidenceObjectKind.Piece, Rook.name)
      ),
      mechanism = List(ConcreteChessObject(EvidenceObjectKind.Mechanism, "line-access")),
      consequence = List(ConcreteChessObject(EvidenceObjectKind.Consequence, consequenceKind.toString)),
      line = Some(line),
      horizon = Some(s"ply:$eventPly")
    )
    val proof = RootOwnedEffectProof.LineEpisode(source, payload, episode)
    ComparisonEndpointEffectObservationPolicy
      .fromLineEpisode(root, line, binding, proof, episode)
      .getOrElse(fail("expected material observation"))

  private val emptyMaterial = LineMaterialSummary(
    sideToMove = White,
    captures = Nil,
    netCaptureCpForMover = 0,
    maxGainCpForMover = 0,
    maxLossCpForMover = 0,
    hasRecaptureChain = false,
    hasRecoveryWindow = false,
    promotionGainCpForMover = 0,
    materialWindowComplete = true
  )

  private def legalReplay(startFen: String, moves: List[String]): List[LineReplayStep] =
    moves.zipWithIndex.foldLeft(startFen -> List.empty[LineReplayStep]) {
      case ((before, steps), (move, index)) =>
        val after = PrincipalVariationEvidence
          .legalFenAfter(before, move)
          .getOrElse(fail(s"expected legal test move $move"))
        after -> (steps :+ LineReplayStep(index, move, before, after))
    }._2

  private def evidenceRef(
      id: String,
      line: LineNodeRef,
      layer: EvidenceLayer,
      producer: EvidenceProducer,
      scope: EvidenceScope
  ): EvidenceRef =
    EvidenceRef(
      id,
      producer,
      layer,
      root,
      Some(line),
      scope,
      EvidenceConfidence.LegalReplayVerified
    )
