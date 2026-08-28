package lila.chessjudgment.model.judgment

import chess.{ White, Black }
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.strategic.EngineLine

class ComparisonEndpointEffectObservationPolicyTest extends munit.FunSuite:

  private val rootFen = "4k3/7r/8/8/8/8/8/3QK3 w - - 0 1"
  private val root = PositionNodeRef(rootFen, 0, Some(White))
  private val reference = LineNodeRef("reference", "d1d2", 1, LineNodeRole.BestReference)
  private val candidate = LineNodeRef("candidate", "d1h5", 2, LineNodeRole.Played)

  private def rootActor(moveUci: String, role: String): RootCausalActor =
    RootCausalActor(
      moveUci,
      EvidencePieceRole(role),
      White,
      EvidenceSquare(moveUci.take(2)),
      EvidenceSquare(moveUci.slice(2, 4))
    )

  test("strategic endpoint outcome ignores which root piece realizes the shared axis"):
    val axis = StrategicAxisDetail(
      StrategicAxisKind.PlanCoherence,
      StrategicAxisPolarity.Gain,
      "plan-a"
    )
    val referenceObservation = ComparisonEndpointEffectObservationPolicy
      .fromStrategicAxis(root, rootActor(reference.rootMove, "queen"), axis)
      .getOrElse(fail("expected reference observation"))
    val candidateObservation = ComparisonEndpointEffectObservationPolicy
      .fromStrategicAxis(root, rootActor(candidate.rootMove, "queen"), axis)
      .getOrElse(fail("expected candidate observation"))
    val kingLine = LineNodeRef("king-candidate", "e1f1", 3, LineNodeRole.Alternative)
    val differentActor = ComparisonEndpointEffectObservationPolicy
      .fromStrategicAxis(root, rootActor(kingLine.rootMove, "king"), axis)
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

  test("complete inventories require a unique observation per semantic scope"):
    val observation = ComparisonEndpointEffectObservationPolicy
      .fromStrategicAxis(
        root,
        rootActor(reference.rootMove, "queen"),
        StrategicAxisDetail(
          StrategicAxisKind.PlanCoherence,
          StrategicAxisPolarity.Gain,
          "plan-a"
        )
      )
      .getOrElse(fail("expected observation"))
    val ambiguous = observation.copy(
      magnitude = ComparisonEndpointEffectMagnitude.Exact(
        DirectCauseImportanceMeasure.StructuralStrength(5)
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

  test("complete line inventory carries mate magnitude and qualitative endgame conversion"):
    val mateFen = "7k/8/5KQ1/8/8/8/8/8 w - - 0 1"
    val matePosition = PositionNodeRef(mateFen, 0, Some(White))
    val mateLine = LineNodeRef("mate-line", "g6g7", 1, LineNodeRole.BestReference)
    val mateReplay = certifiedReplay(mateFen, List(mateLine.rootMove))
    val mateConsequence = LineConsequence(
      LineConsequenceKind.Mate,
      List(mateLine.rootMove),
      proofSignal = true,
      eventMove = Some(mateLine.rootMove),
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
    val mateObservations = EvidenceObjectBinding
      .comparisonEndpointLineObservations(mateRef, matePayload, matePosition)
      .mate match
        case ComparisonEndpointEffectInventory.Complete(observations) => observations
        case ComparisonEndpointEffectInventory.Incomplete => fail("expected complete mate inventory")
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
    assert(mateQualitative.exists(
      _.scope.effectIdentity.primitiveKind == RootOwnedEffectPrimitiveKind.RootLineEvent
    ))

  test("eligible root event projection failure makes only qualitative inventory incomplete"):
    val replay = certifiedReplay(rootFen, List(reference.rootMove))
    val payload = LineFactEvidence.fromCertifiedReplay(
      line = reference,
      replay = replay,
      material = None,
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
          planIds = Nil,
          strategicAxes = Nil
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
