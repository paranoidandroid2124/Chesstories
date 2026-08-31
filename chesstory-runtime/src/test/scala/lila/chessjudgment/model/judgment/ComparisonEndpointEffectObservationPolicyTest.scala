package lila.chessjudgment.model.judgment

import chess.{ White, Black }
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.line.EngineLine

class ComparisonEndpointEffectObservationPolicyTest extends munit.FunSuite:

  private val rootFen = "4k3/7r/8/8/8/8/8/3QK3 w - - 0 1"
  private val root = PositionNodeRef(rootFen, 0, Some(White))
  private val reference = LineNodeRef("reference", "d1d2", 1, LineNodeRole.BestReference)
  private val candidate = LineNodeRef("candidate", "d1h5", 2, LineNodeRole.Played)

  test("material timing belongs to typed magnitude and Pareto trade-offs fail closed"):
    val earlier = exactMaterialObservation(
      reference,
      eventPly = 2,
      valueCp = 500,
      LineConsequenceKind.MaterialGain,
      beneficiary = White
    )
    val later = exactMaterialObservation(
      candidate,
      eventPly = 4,
      valueCp = 500,
      LineConsequenceKind.MaterialGain,
      beneficiary = White
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
    val queenExchange = exactMaterialObservation(
      reference,
      eventPly = 2,
      valueCp = 900,
      LineConsequenceKind.MaterialGain,
      beneficiary = White,
      durableNetCp = Some(400)
    )
    val freeRook = exactMaterialObservation(
      candidate,
      eventPly = 2,
      valueCp = 500,
      LineConsequenceKind.MaterialGain,
      beneficiary = White,
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
    val opponentGain = exactMaterialObservation(
      reference,
      eventPly = 2,
      valueCp = 500,
      LineConsequenceKind.MaterialGain,
      beneficiary = Black
    )
    val moverLoss = exactMaterialObservation(
      candidate,
      eventPly = 2,
      valueCp = 500,
      LineConsequenceKind.MaterialLoss,
      beneficiary = Black
    )

    assertEquals(opponentGain.scope.stake, RootOwnedEffectStake.ActorLiability)
    assertEquals(opponentGain.scope, moverLoss.scope)
    assertEquals(
      opponentGain.scope.consequenceSignatures,
      List("consequence:material-transfer")
    )

  test("complete line inventory carries mate magnitude and qualitative endgame conversion"):
    val mateFen = "7k/8/5KQ1/8/8/8/8/8 w - - 0 1"
    val matePosition = PositionNodeRef(mateFen, 0, Some(White))
    val mateLine = LineNodeRef("mate-line", "g6g7", 1, LineNodeRole.BestReference)
    val mateReplay = certifiedReplay(mateFen, List(mateLine.rootMove))
    val mateConsequence = LineConsequence(
      LineConsequenceKind.Mate,
      List(LineMoveOccurrence(mateLine.rootMove, 0)),
      directCauseProjectionEligible = true,
      eventOccurrence = Some(LineMoveOccurrence(mateLine.rootMove, 0)),
      rootMove = Some(mateLine.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White)
    )
    val matePayload = LineFactEvidence.fromCertifiedReplay(
      line = mateLine,
      replay = mateReplay,
      material = None,
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
    val episode = matePayload.rootOwnedCausalEpisodes(mateLine.rootMove) match
      case exact :: Nil => exact
      case _ => fail("expected one root-owned mate episode")
    val binding = EvidenceObjectBinding(
      source = mateRef,
      actor = List(ConcreteChessObject(EvidenceObjectKind.Piece, "white-queen")),
      target = List(ConcreteChessObject(EvidenceObjectKind.Piece, "black-king")),
      mechanism = List(ConcreteChessObject(EvidenceObjectKind.Mechanism, "mate")),
      consequence = List(ConcreteChessObject(EvidenceObjectKind.Consequence, "mate")),
      line = Some(mateLine),
      horizon = Some("ply:0")
    )
    val mateProof = RootOwnedEffectProof.LineEpisode(mateRef, matePayload, episode)
    val mateObservation = ComparisonEndpointEffectObservationPolicy
      .fromLineEpisode(matePosition, mateLine, binding, mateProof, episode)
      .getOrElse(fail("expected exact mate observation"))
    assertEquals(
      mateObservation.magnitude,
      ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.MateArrival(0))
    )

    val checkEvent = matePayload.eventsForRootMove(mateLine.rootMove).find(_.kind == LineEventKind.Check)
      .getOrElse(fail("expected root check event"))
    val checkProof = RootOwnedEffectProof.RootLineEvent(mateRef, matePayload, checkEvent)
    val checkObservation = ComparisonEndpointEffectObservationPolicy
      .fromRootLineEvent(matePosition, mateLine, binding, checkProof, checkEvent)
      .getOrElse(fail("expected qualitative root-event observation"))
    assertEquals(
      checkObservation.scope.effectIdentity.primitiveKind,
      RootOwnedEffectPrimitiveKind.RootLineEvent
    )
    assertEquals(checkObservation.magnitude, ComparisonEndpointEffectMagnitude.QualitativePresence)

    val channel = DirectCauseChannel(
      binding = binding,
      rootActor = episode.actor,
      directChange = DirectCausalChange.Occurred,
      rootOwnedProof = Some(mateProof)
    )
    val comparisonRef = mateRef.copy(
      id = "comparison",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.CandidateComparison,
      line = None,
      scope = EvidenceScope.Counterfactual
    )
    val admittedCause = RelativeCauseFact(
      kind = RelativeCauseKind.KingForcing,
      comparisonEvidence = comparisonRef,
      supportEvidence = Nil,
      sourceSide = RelativeCauseSourceSide.Candidate,
      attribution = CauseAttribution(CauseAttributionKind.CandidateCreatesValue),
      directEffectAdmission = DirectEffectAdmission.Restricted(
        Map(channel.exactOccurrenceFingerprint -> Some(mateObservation))
      )
    )
    assertEquals(admittedCause.admittedEndpointObservation(channel), Some(mateObservation))
    assertEquals(
      admittedCause.admittedEndpointObservation(channel.copy(directChange = DirectCausalChange.Lost)),
      None,
      "a different occurrence must not borrow the transported endpoint observation"
    )
    assertEquals(
      admittedCause
        .copy(attribution = CauseAttribution(CauseAttributionKind.CandidateAllowsLiability))
        .admittedEndpointObservation(channel),
      None,
      "transported endpoint stake must still agree with Cause attribution"
    )
    val membership = ComparisonEndpointWitnessDemandEntry
      .fromChannel(
        RelativeCauseSourceSide.Reference,
        ComparisonEndpointWitnessDemandMode.MembershipOnly,
        channel
      )
      .getOrElse(fail("expected exact membership demand"))
    val closure = ComparisonEndpointWitnessDemandEntry
      .fromChannel(
        RelativeCauseSourceSide.Reference,
        ComparisonEndpointWitnessDemandMode.DifferentialClosure,
        channel
      )
      .getOrElse(fail("expected exact closure demand"))
    val membershipDemand = ComparisonEndpointWitnessDemand.fromEntries(List(membership, membership))
    assertEquals(membershipDemand.entries.size, 1)
    assertEquals(membershipDemand.entriesForEndpoint(RelativeCauseSourceSide.Candidate), Nil)
    assertEquals(
      ComparisonEndpointWitnessDemand.fromEntries(List(closure))
        .entriesForEndpoint(RelativeCauseSourceSide.Candidate),
      List(closure)
    )
    val secondCarrier = ComparisonEndpointWitnessDemandEntry
      .fromChannel(
        RelativeCauseSourceSide.Reference,
        ComparisonEndpointWitnessDemandMode.MembershipOnly,
        channel.copy(binding = binding.copy(source = mateRef.copy(id = "second-carrier")))
      )
      .getOrElse(fail("expected second carrier occurrence"))
    assertEquals(
      ComparisonEndpointWitnessDemand.fromEntries(List(membership, secondCarrier)).entries.size,
      2
    )

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
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(reference.rootMove), referenceScore, depth = 18)),
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
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(candidate.rootMove), candidateScore, depth = 18)),
          evidenceRef(
            "candidate-eval",
            candidate,
            EvidenceLayer.Eval,
            EvidenceProducer.EngineEvalProducer,
            EvidenceScope.PlayedLine
          )
        )
      ).get,
      VerdictConfidence.EngineBacked
    )

  private def exactMaterialObservation(
      line: LineNodeRef,
      eventPly: Int,
      valueCp: Int,
      consequenceKind: LineConsequenceKind,
      beneficiary: chess.Color,
      durableNetCp: Option[Int] = None
  ): ComparisonEndpointEffectObservation =
    val _ = line
    val stake =
      if beneficiary == White then RootOwnedEffectStake.ActorValue
      else RootOwnedEffectStake.ActorLiability
    val change =
      if consequenceKind == LineConsequenceKind.MaterialGain && stake == RootOwnedEffectStake.ActorValue then
        DirectCausalChange.Occurred
      else DirectCausalChange.Lost
    val materialTransfer = List("outcome:material-transfer")
    ComparisonEndpointEffectObservation(
      scope = ComparisonEndpointEffectScopeKey(
        rootBoardState = PrincipalVariationEvidence
          .semanticBoardStateFen(rootFen)
          .getOrElse(fail("expected canonical root board")),
        mover = ComparisonEndpointMoverIdentity(White),
        targetSignatures = materialTransfer,
        mechanismSignatures = materialTransfer,
        consequenceSignatures = List("consequence:material-transfer"),
        horizon = None,
        directChange = change,
        effectIdentity = RootOwnedEffectIdentity(
          primitiveKind = RootOwnedEffectPrimitiveKind.LineEpisode,
          targetSignatures = materialTransfer,
          passedPawnResultKindIds = Nil
        ),
        stake = stake
      ),
      magnitude = ComparisonEndpointEffectMagnitude.Exact(
        DirectCauseImportanceMeasure.MaterialOutcome(durableNetCp.getOrElse(valueCp), eventPly)
      )
    )

  private def certifiedReplay(startFen: String, moves: List[String]): CanonicalLineReplay =
    (for
      history <- CanonicalPositionHistory.from(startFen, Nil, startFen).toOption
      extended <- history.extend(moves).toOption
      replay <- CanonicalLineReplay.fromHistory(
        extended.segmentReplaySteps.drop(history.segmentReplaySteps.size)
      )
    yield replay).getOrElse(fail(s"expected one certified replay for ${moves.mkString(" ")}"))

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
