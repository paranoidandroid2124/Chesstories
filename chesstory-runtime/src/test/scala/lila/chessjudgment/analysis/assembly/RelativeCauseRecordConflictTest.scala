package lila.chessjudgment.analysis.assembly

import chess.White
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import lila.chessjudgment.model.line.EngineLine

class RelativeCauseRecordConflictTest extends munit.FunSuite:

  test("mate completeness requires engine winner polarity to match the owned beneficiary"):
    val mateFen = "7k/8/5KQ1/8/8/8/8/8 w - - 0 1"
    val matePosition = PositionNodeRef(mateFen, 0, Some(White))
    val mateLine = LineNodeRef("mate-polarity", "g6g7", 1, LineNodeRole.BestReference)
    val history = CanonicalPositionHistory
      .from(mateFen, Nil, mateFen)
      .getOrElse(fail("expected a canonical mate root"))
    val extended = history
      .extend(List(mateLine.rootMove))
      .getOrElse(fail("expected legal mate"))
    val replay = CanonicalLineReplay
      .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail("expected one certified mate replay"))
    val payload = LineFactEvidence.fromCertifiedReplay(
      line = mateLine,
      replay = replay,
      events = List(LineMoveEvent(
        kind = LineEventKind.Mate,
        moveUci = mateLine.rootMove,
        plyOffset = 0,
        side = Some(White),
        pieceRole = Some(EvidencePieceRole("queen")),
        targetRole = Some(EvidencePieceRole("king")),
        square = Some(EvidenceSquare("h8"))
      )),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.Mate,
        proofOccurrences = List(LineMoveOccurrence(mateLine.rootMove, 0)),
        directCauseProjectionEligible = true,
        eventOccurrence = Some(LineMoveOccurrence(mateLine.rootMove, 0)),
        rootMove = Some(mateLine.rootMove),
        rootSide = Some(White),
        beneficiary = Some(White)
      ))
    )
    val source = EvidenceRef(
      "mate-polarity-line",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      matePosition,
      Some(mateLine),
      EvidenceScope.BestLine,
      EvidenceConfidence.LegalReplayVerified
    )
    val mateInventory = ComparisonEndpointEffectInventory.Complete(
      Set(
        ComparisonEndpointEffectObservation(
          ComparisonEndpointEffectScopeKey(
            rootBoardState = matePosition.fen,
            mover = ComparisonEndpointMoverIdentity(White),
            targetSignatures = Nil,
            mechanismSignatures = Nil,
            consequenceSignatures = Nil,
            horizon = Some("ply:0"),
            directChange = DirectCausalChange.Occurred,
            effectIdentity = RootOwnedEffectIdentity(
              RootOwnedEffectPrimitiveKind.LineEpisode,
              Nil,
              Nil
            ),
            stake = RootOwnedEffectStake.ActorValue
          ),
          ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.MateArrival(0))
        )
      )
    )

    val correct = RelativeAssessmentAssembler.enforceLineFamilyCompleteness(
      mateInventory,
      payload,
      Some(EngineLine(List(mateLine.rootMove), 900, mate = Some(1), depth = 20)),
      ComparisonEndpointLineProofFamily.LineEpisodeMate
    )
    val reversed = RelativeAssessmentAssembler.enforceLineFamilyCompleteness(
      mateInventory,
      payload,
      Some(EngineLine(List(mateLine.rootMove), 900, mate = Some(-1), depth = 20)),
      ComparisonEndpointLineProofFamily.LineEpisodeMate
    )

    assert(correct.isInstanceOf[ComparisonEndpointEffectInventory.Complete])
    assertEquals(reversed, ComparisonEndpointEffectInventory.Incomplete)

  test("one endpoint fact cannot hide multiple evidence owners"):
    val position = PositionNodeRef("8/8/8/8/8/8/8/K6k w - - 0 1", 0, Some(White))
    val line = LineNodeRef("unique-carrier-line", "a1a2", 1, LineNodeRole.BestReference)
    def carrier(id: String): EvidenceRef =
      EvidenceRef(
        id,
        EvidenceProducer.LegalLineProducer,
        EvidenceLayer.Line,
        position,
        Some(line),
        EvidenceScope.BestLine,
        EvidenceConfidence.LegalReplayVerified
      )

    val sole = carrier("sole-owner") -> "exact-payload"
    assertEquals(RelativeAssessmentAssembler.uniqueCarrier(List(sole)), Some(sole))
    assertEquals(RelativeAssessmentAssembler.uniqueCarrier(Nil), None)

    val conflict = intercept[IllegalStateException] {
      RelativeAssessmentAssembler.uniqueCarrier(
        List(sole, carrier("duplicate-owner") -> "exact-payload")
      )
    }
    assert(conflict.getMessage.contains("multiple evidence owners"))

  test("one exact claim projection cannot hide duplicate producers"):
    val position = PositionNodeRef("8/8/8/8/8/8/8/K6k w - - 0 1", 0, Some(White))
    def carrier(id: String): EvidenceRef =
      EvidenceRef(
        id,
        EvidenceProducer.PositionOccurrenceProducer,
        EvidenceLayer.PositionOccurrence,
        position,
        None,
        EvidenceScope.CurrentPosition,
        EvidenceConfidence.BoardDerived
      )
    def claim(id: String, evidence: List[EvidenceRef] = Nil): JudgmentClaim =
      JudgmentClaim(
        id,
        ClaimFamily.Tactical,
        ClaimSubject.PlayedMove,
        position,
        None,
        None,
        evidence,
        EvidenceScope.CurrentPosition,
        EvidenceConfidence.BoardDerived
      )

    intercept[IllegalArgumentException] {
      ClaimProducerOwnership.validate(List(claim("same-id"), claim("same-id")), TypedEvidenceGraph.empty)
    }
    intercept[IllegalArgumentException] {
      claim("duplicate-evidence", List(carrier("same-carrier"), carrier("same-carrier")))
    }
    intercept[IllegalArgumentException] {
      RelativeCauseProofSection(
        RelativeCauseProofRole.DirectProof,
        RelativeCauseProofStrength.Primary,
        List(carrier("same-source"), carrier("same-source"))
      )
    }
    intercept[IllegalArgumentException] {
      ClaimProducerOwnership.validate(List(claim("owner-a"), claim("owner-b")), TypedEvidenceGraph.empty)
    }
    ClaimProducerOwnership.validate(
      List(
        claim("independent-a", List(carrier("carrier-a"))),
        claim("independent-b", List(carrier("carrier-b")))
      ),
      TypedEvidenceGraph.empty
    )

  test("one ready relative Cause cannot hide duplicate producers"):
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1",
          playedMoveUci = "d1d8",
          variations = List(
            EngineLine(List("c4f7", "f8f7", "d1d8"), scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(List("d1d8", "f8d8"), scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected an exact relative Cause fixture"))
    val graph = packet.evidenceGraph
    val owner = graph.records.collectFirst {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if RelativeCauseSemanticKey.from(cause, record.ref.id, graph).sentenceReady =>
        record
    }.getOrElse(fail("expected one sentence-ready relative Cause"))
    val duplicate = owner.copy(ref = owner.ref.copy(id = s"${owner.ref.id}:duplicate-owner"))

    val conflict = intercept[IllegalArgumentException] {
      RelativeAssessmentAssembler.validateRelativeCauseOwnership(
        List(owner, duplicate),
        graph
      )
    }
    assert(conflict.getMessage.contains("multiple evidence owners"))
