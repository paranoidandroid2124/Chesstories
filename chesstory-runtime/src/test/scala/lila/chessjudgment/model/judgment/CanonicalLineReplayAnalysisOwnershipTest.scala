package lila.chessjudgment.model.judgment

import chess.variant.Standard
import lila.chessjudgment.model.line.CanonicalPositionHistory

class CanonicalLineReplayAnalysisOwnershipTest extends munit.FunSuite:

  private def replayFrom(fen: String, moves: List[String]): CanonicalLineReplay =
    val history = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .flatMap(_.extend(moves))
      .getOrElse(fail(s"expected a legal replay for ${moves.mkString(" ")}"))
    CanonicalLineReplay
      .fromHistory(history.segmentReplaySteps)
      .getOrElse(fail("expected a canonical replay"))

  private def twoPlyReplay: CanonicalLineReplay =
    val history = CanonicalPositionHistory
      .from(Standard.initialFen.value, Nil, Standard.initialFen.value)
      .flatMap(_.extend(List("e2e4", "e7e5")))
      .getOrElse(fail("expected a legal two-ply history"))
    CanonicalLineReplay
      .fromHistory(history.segmentReplaySteps)
      .getOrElse(fail("expected a canonical replay"))

  test("one replay shares each occurrence analysis across before and after queries"):
    val replay = twoPlyReplay
    val first :: second :: Nil = replay.replaySteps: @unchecked

    val firstBefore = replay.analysisBefore(first).getOrElse(fail("expected the root occurrence"))
    val firstAfter = replay.analysisAfter(first).getOrElse(fail("expected the first destination"))
    val secondBefore = replay.analysisBefore(second).getOrElse(fail("expected the second source"))
    val secondAfter = replay.analysisAfter(second).getOrElse(fail("expected the second destination"))

    assert(firstBefore eq replay.analysisBefore(first).getOrElse(fail("expected the cached root occurrence")))
    assert(firstAfter eq replay.analysisAfter(first).getOrElse(fail("expected the cached first destination")))
    assert(firstAfter eq secondBefore)
    assert(secondAfter eq replay.analysisAfter(second).getOrElse(fail("expected the cached second destination")))
    assertEquals(firstBefore.occurrence.plyCount, first.ply - 1)
    assertEquals(firstAfter.occurrence.plyCount, first.ply)
    assertEquals(secondAfter.occurrence.plyCount, second.ply)

  test("rebasing changes only occurrence plies and shares the owned calculations"):
    val replay = twoPlyReplay
    assertEquals(replay.rebased(0), None)

    val rebased = replay.rebased(4).getOrElse(fail("expected a positive occurrence rebase"))
    val originalFirst = replay.replaySteps.head
    val rebasedFirst = rebased.replaySteps.head
    val originalBefore = replay.analysisBefore(originalFirst).getOrElse(fail("expected original source"))
    val originalAfter = replay.analysisAfter(originalFirst).getOrElse(fail("expected original destination"))
    val rebasedBefore = rebased.analysisBefore(rebasedFirst).getOrElse(fail("expected rebased source"))
    val rebasedAfter = rebased.analysisAfter(rebasedFirst).getOrElse(fail("expected rebased destination"))

    assertEquals(rebasedFirst.ply, 4)
    assertEquals(rebasedBefore.occurrence.plyCount, 3)
    assertEquals(rebasedAfter.occurrence.plyCount, 4)
    assert(rebasedBefore.position eq originalBefore.position)
    assert(rebasedAfter.position eq originalAfter.position)
    assert(rebasedBefore.actualLegalMoves eq originalBefore.actualLegalMoves)
    assert(rebasedAfter.boardRelations eq originalAfter.boardRelations)

  test("transposed replay destinations share semantic occurrence identity without sharing steps"):
    val first = replayFrom(
      Standard.initialFen.value,
      List("g1f3", "g8f6", "g2g3", "g7g6")
    )
    val second = replayFrom(
      Standard.initialFen.value,
      List("g2g3", "g7g6", "g1f3", "g8f6")
    )
    val firstStep = first.replaySteps.last
    val secondStep = second.replaySteps.last
    val firstOccurrence = first.positionAfter(firstStep).getOrElse(fail("missing first destination"))
    val secondOccurrence = second.positionAfter(secondStep).getOrElse(fail("missing transposed destination"))

    assertNotEquals(firstStep, secondStep)
    assert(
      lila.chessjudgment.model.line.PrincipalVariationEvidence.sameBoardState(
        firstOccurrence.position.fen,
        secondOccurrence.position.fen
      )
    )
    assertEquals(firstOccurrence.position.ply, secondOccurrence.position.ply)
    assertEquals(firstOccurrence.occurrenceId, secondOccurrence.occurrenceId)

  test("exact recapture membership returns its occurrence-owned L1 key and resource"):
    val replay = replayFrom(
      "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1",
      List("d1d8", "f8d8")
    )
    val capture :: recapture :: Nil = replay.replaySteps: @unchecked

    val (key, resource) = replay
      .exactRecaptureMembership(capture, recapture)
      .getOrElse(fail("expected the exact recapture membership"))

    assertEquals(key.kind, RelationFactKind.CaptureRecaptureInventory)
    assert(EvidenceRef.sameMove(resource.moveUci, "f8d8"))
    assertEquals(resource.movement, replay.transition(recapture).map(_.relationDelta.rootMove.witness).get)

    val captureReplay = replay.subset(List(capture)).getOrElse(fail("expected the capture occurrence"))
    val recaptureReplay = replay.subset(List(recapture)).getOrElse(fail("expected the reply occurrence"))
    assertEquals(
      captureReplay.exactRecaptureMembership(capture, recaptureReplay, recapture),
      Some(key -> resource)
    )

  test("exact check-response membership rejects the same semantic move from another occurrence"):
    val replay = replayFrom(
      "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1",
      List("c4f7", "f8f7")
    )
    val check :: response :: Nil = replay.replaySteps: @unchecked
    val (key, witness) = replay
      .exactCheckResponseMembership(check, response)
      .getOrElse(fail("expected the exact check-response membership"))

    assertEquals(key.kind, RelationFactKind.CreatedCheckResponseInventory)
    assert(EvidenceRef.sameMove(witness.resource.moveUci, "f8f7"))

    val foreignResponse = replay
      .rebased(4)
      .getOrElse(fail("expected a rebased occurrence"))
      .replaySteps(1)
    assertEquals(replay.exactCheckResponseMembership(check, foreignResponse), None)

  test("a pawn-break follow-up consumes the reply occurrence's L1 passed-state transition"):
    val fen = "4k3/8/3p4/2P5/4P3/8/8/4K3 w - - 0 1"
    val history = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .flatMap(_.extend(List("e4e5", "d6e5", "c5c6")))
      .getOrElse(fail("expected a legal pawn-break continuation"))
    val replay = CanonicalLineReplay
      .fromHistory(history.segmentReplaySteps)
      .getOrElse(fail("expected a canonical pawn-break replay"))
    val breakStep :: replyStep :: followUpStep :: Nil = replay.replaySteps: @unchecked
    val line = LineNodeRef(
      "pawn-break-follow-up-owner",
      breakStep.moveUci,
      1,
      LineNodeRole.BestReference
    )
    val lineRecord = EvidenceRecord(
      EvidenceRef(
        "pawn-break-follow-up-owner-evidence",
        EvidenceProducer.LegalLineProducer,
        EvidenceLayer.Line,
        PositionNodeRef(fen, 0, replay.legalSteps.headOption.map(_.move.piece.color)),
        Some(line),
        line.role.scope,
        EvidenceConfidence.LegalReplayVerified
      ),
      LineFactEvidence.fromCertifiedReplay(line, replay)
    )
    val releaseOccurrences = replay.verticalRelationOccurrences(
      replyStep,
      List(VerticalRelationContractKind.PawnTopologyTransition)
    ).filter { occurrence =>
      occurrence.relation.detail match
        case detail: RelationWitnessDetail.PawnTopologyTransition =>
          detail.before.exists(_.square.key.equalsIgnoreCase("c5")) &&
            detail.changedFacets.contains(RelationPawnTopologyFacet.Passed)
        case _ => false
    }
    val releaseOccurrence = releaseOccurrences match
      case exact :: Nil => exact
      case found        => fail(s"expected one L1 release occurrence, found ${found.size}")
    val trajectory = PawnBreakFollowUpTrajectory
      .find(
        breakStep,
        replyStep,
        followUpStep,
        List(replyStep),
        replay,
        _ => Some(lineRecord)
      )
      .getOrElse(fail("expected the L1-certified pawn-break follow-up"))

    assertEquals(trajectory.releasedPassedPawn, EvidenceSquare("c5"))
    assertEquals(
      trajectory.pawnTopologyTransitionKey,
      DerivedRelationResultKey.from(releaseOccurrence.relation)
    )
    assertEquals(trajectory.relationOccurrenceAuthority.step, replyStep)
    assertEquals(
      trajectory.relationOccurrenceAuthority.contract,
      VerticalRelationContractKind.PawnTopologyTransition
    )
    assertEquals(
      trajectory.relationOccurrenceAuthority.occurrenceId,
      releaseOccurrence.occurrenceId
    )
    assertEquals(
      trajectory.relationOccurrenceAuthority.certifiedSourcePremiseIds,
      releaseOccurrence.certifiedSourcePremiseIds
    )
    assertEquals(
      trajectory.pawnTopologyTransitionKey.kind,
      RelationFactKind.PawnTopologyTransition
    )
    releaseOccurrence.relation.detail match
      case detail: RelationWitnessDetail.PawnTopologyTransition =>
        assert(detail.before.exists(state => !state.passed))
        assert(detail.after.exists(_.passed))
      case _ => fail("expected the exact L1 pawn-topology detail")

    val functionProof = CausalDependencyFunctionProof.PawnBreakFollowUp(
      trajectory.color,
      replyStep.moveUci,
      trajectory.replyFrom,
      trajectory.replyTo,
      trajectory.followUpFrom,
      trajectory.followUpTo,
      trajectory.releasedPassedPawn,
      trajectory.pawnTopologyTransitionKey.stableKey
    )
    assertEquals(
      functionProof.pawnTopologyTransitionKey,
      trajectory.pawnTopologyTransitionKey.stableKey
    )
    intercept[IllegalArgumentException] {
      functionProof.copy(pawnTopologyTransitionKey = s"pawn_passage:${List.fill(64)("0").mkString}")
    }
