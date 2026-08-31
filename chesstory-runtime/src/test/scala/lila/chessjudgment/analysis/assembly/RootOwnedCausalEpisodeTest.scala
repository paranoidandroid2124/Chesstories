package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }

import lila.chessjudgment.analysis.line.LineFactNormalizer
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }

class RootOwnedCausalEpisodeTest extends munit.FunSuite:

  test("an immediate recapture is owned through the canonical relation inventory"):
    val initialFen = "r1b2rk1/5ppp/1qp2n2/p2p4/1b6/N3P1P1/PPQB1PBP/R1R3K1 w - - 0 13"
    val rootFen = "r1b2rk1/5ppp/1qp2n2/p2p4/1b6/4P1P1/PPQB1PBP/RNR3K1 b - - 1 13"
    val moves = List("b4d2", "b1d2")
    val history = CanonicalPositionHistory
      .from(initialFen, List("a3b1"), rootFen)
      .toOption
      .getOrElse(fail("expected the exact predecessor history"))
    val lineHistory = history
      .extend(moves)
      .toOption
      .getOrElse(fail("expected the exact recapture line"))
    val replay = CanonicalLineReplay
      .fromHistory(lineHistory.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail("expected one canonical replay"))
    val moveRefs = replay.replaySteps.map(step =>
      PrincipalVariationEvidence.LineMoveRef(step.ply, step.moveUci, step.fenAfter)
    )
    val first = moveRefs.head
    val line = LineNodeRef("exact-recapture", first.uci, 1, LineNodeRole.BestReference)
    val facts = PrincipalVariationEvidence.LineFacts(
      line = PrincipalVariationEvidence.LineVariationRef(moveRefs),
      first = first,
      reply = moveRefs.lift(1),
      continuation = None,
      continuationTail = Nil
    )
    val material = CandidateLineAssembler
      .lineMaterialSummary(history, lineHistory)
      .getOrElse(fail("expected the canonical material ledger"))
    val payload = LineFactNormalizer
      .fromValidatedLine(
        id = "exact-recapture-evidence",
        lineRef = line,
        facts = facts,
        replay = replay,
        position = PositionNodeRef(rootFen, 0, Some(Black)),
        scope = line.role.scope,
        materialSummary = Some(material)
      )
      .payload match
      case value: LineFactEvidence => value
      case _                       => fail("expected normalized line evidence")

    val recapture = payload.materialCaptures
      .find(capture => EvidenceRef.sameMove(capture.moveUci, "b1d2"))
      .getOrElse(fail("expected the canonical recapture occurrence"))
    assert(recapture.recaptureStatus.isInstanceOf[LineMaterialRecaptureStatus.Proven])

    val episode = payload
      .rootOwnedCausalEpisodes(line.rootMove)
      .find(_.consequence.kind == LineConsequenceKind.RecaptureSequence)
      .getOrElse(fail("expected the exact root-owned recapture sequence"))
    assertEquals(episode.actor.moveUci, "b4d2")
    assertEquals(episode.eventOccurrence.plyOffset, 1)
    assert(episode.links.exists(_.kind == RootCausalLinkKind.RootActorCaptured))
    assert(RootOwnedEffectPolicy.admitsLineEpisode(payload, episode))

  test("repeated promotion UCI retains two exact consequence occurrences"):
    val rootFen = "8/P7/P6k/8/8/8/8/7K w - - 0 1"
    val moves = List("a7a8q", "h6h5", "a8b8", "h5h4", "a6a7", "h4h3", "a7a8q")
    val (payload, line) = normalizedLine(
      id = "repeated-promotion",
      initialFen = rootFen,
      historyMoves = Nil,
      rootFen = rootFen,
      moves = moves
    )

    val promotions = payload.lineConsequences.filter(_.kind == LineConsequenceKind.Promotion)
    assertEquals(promotions.flatMap(_.eventOccurrence.map(_.plyOffset)), List(0, 6))
    assertEquals(
      promotions.map(_.proofOccurrences.map(occurrence => occurrence.plyOffset -> occurrence.moveUci)),
      List(
        List(0 -> "a7a8q"),
        List(0 -> "a7a8q", 6 -> "a7a8q")
      )
    )
    assertEquals(
      payload
        .rootOwnedCausalEpisodes(line.rootMove)
        .filter(_.consequence.kind == LineConsequenceKind.Promotion)
        .map(_.eventOccurrence.plyOffset),
      List(0)
    )

  test("lasting material gain does not absorb a later repeated UCI capture"):
    val initialFen = "7k/8/5KQ1/8/1r6/8/r7/R7 b - - 0 1"
    val rootFen = "7k/8/5KQ1/8/8/1r6/r7/R7 w - - 1 2"
    val (payload, _) = normalizedLine(
      id = "repeated-material-capture",
      initialFen = initialFen,
      historyMoves = List("b4b3"),
      rootFen = rootFen,
      moves = List("a1a2", "b3b2", "a2a1", "b2a2", "a1a2")
    )

    assertEquals(payload.materialGainCapturesFor(White).map(_.plyOffset), List(0))

  test("a queen-for-pawn blunder remains an exact capture sequence without an upper idea label"):
    val initialFen = "3r4/3p3k/8/8/8/8/8/3Q3K b - - 0 1"
    val rootFen = "3r3k/3p4/8/8/8/8/8/3Q3K w - - 1 2"
    val (payload, _) = normalizedLine(
      id = "queen-for-pawn-blunder",
      initialFen = initialFen,
      historyMoves = List("h7h8"),
      rootFen = rootFen,
      moves = List("d1d7", "d8d7")
    )

    assertEquals(
      payload.materialCaptures.map(capture =>
        (capture.moveUci, capture.plyOffset, capture.attackerRole.name, capture.capturedRole.name, capture.recapture)
      ),
      List(
        ("d1d7", 0, "queen", "pawn", false),
        ("d8d7", 1, "rook", "queen", true)
      )
    )
    assertEquals(payload.lineConsequences.map(_.kind), List(LineConsequenceKind.RecaptureSequence))

  test("an immediate reply check is projected from its exact L1 event but is not causal without a link"):
    val rootFen = "1r5k/8/8/8/8/8/8/K6N w - - 0 1"
    val (payload, line) = normalizedLine(
      id = "unlinked-immediate-reply-check",
      initialFen = rootFen,
      historyMoves = Nil,
      rootFen = rootFen,
      moves = List("h1f2", "b8b1"),
      requireMaterial = false
    )

    val checkEvent = payload.lineEvents.filter(_.kind == LineEventKind.Check) match
      case exact :: Nil => exact
      case other        => fail(s"expected one exact reply check event, got $other")
    assertEquals(checkEvent.moveUci, "b8b1")
    assertEquals(checkEvent.plyOffset, 1)
    assertEquals(checkEvent.side, Some(Black))

    val liability = payload.lineConsequences.filter(_.kind == LineConsequenceKind.ImmediateReplyCheck) match
      case exact :: Nil => exact
      case other        => fail(s"expected one exact immediate-reply-check projection, got $other")
    assertEquals(
      liability.proofOccurrences.map(occurrence => occurrence.plyOffset -> occurrence.moveUci),
      List(0 -> "h1f2", 1 -> "b8b1")
    )
    assertEquals(liability.eventOccurrence.map(_.plyOffset), Some(1))
    assertEquals(liability.beneficiary, Some(Black))
    assert(!liability.directCauseProjectionEligible)
    assertEquals(payload.rootOwnedCausalEpisodes(line.rootMove), Nil)

  test("exact slider reach does not become a general causal episode without a bounded proof"):
    val rootFen = "r6k/8/6Kp/8/8/8/B7/R7 w - - 0 1"
    val (payload, line) = normalizedLine(
      id = "line-clearance-without-bounded-proof",
      initialFen = rootFen,
      historyMoves = Nil,
      rootFen = rootFen,
      moves = List("a2b3", "h6h5", "a1a8"),
      whitePovMate = Some(1)
    )
    val replay = payload.certifiedReplay.getOrElse(fail("expected the certified clearance replay"))
    val steps = payload.lineReplaySteps
    val trajectory = LineAccessTrajectory
      .findRootClearanceBeforeUse(steps.head, steps(2), List(steps(1)), replay)
      .getOrElse(fail("expected the exact slider-reach occurrence"))

    assertEquals(trajectory.vacatedSquares, List(EvidenceSquare("a2")))
    assertEquals(trajectory.enabledFrom, EvidenceSquare("a1"))
    assertEquals(trajectory.enabledTo, EvidenceSquare("a8"))
    assertEquals(
      trajectory.relationOccurrenceBinding.contract,
      VerticalRelationContractKind.SliderReachDelta
    )
    assert(trajectory.relationOccurrenceBinding.occurrenceId.matches("[0-9a-f]{64}"))
    assert(trajectory.relationOccurrenceBinding.certifiedSourcePremiseIds.nonEmpty)
    assert(payload.lineConsequences.exists(_.kind == LineConsequenceKind.Mate))
    assertEquals(payload.rootOwnedCausalEpisodes(line.rootMove), Nil)

  test("capture and recapture keep their exact material path without a duplicate response seed"):
    val initialFen = "r5k1/p7/1B6/8/8/8/8/R3K3 b - - 0 1"
    val rootFen = "r6k/p7/1B6/8/8/8/8/R3K3 w - - 1 2"
    val (payload, line) = normalizedLine(
      id = "capture-recapture-material-path",
      initialFen = initialFen,
      historyMoves = List("g8h8"),
      rootFen = rootFen,
      moves = List("a1a7", "a8a7", "b6a7")
    )
    val replay = payload.certifiedReplay.getOrElse(fail("expected the certified recapture replay"))
    val steps = payload.lineReplaySteps
    val trajectory = CaptureResponseFollowUpTrajectory
      .find(steps.head, steps(1), steps(2), List(steps(1)), replay)
      .getOrElse(fail("expected the exact capture-response continuation"))
    val episode = payload
      .rootOwnedCausalEpisodes(line.rootMove)
      .find(_.eventOccurrence.sameOccurrence(2, "b6a7"))
      .getOrElse(fail("expected the transported material episode"))

    assertEquals(
      trajectory.relationOccurrenceBinding.contract,
      VerticalRelationContractKind.CaptureRecaptureInventory
    )
    assert(trajectory.relationOccurrenceBinding.occurrenceId.matches("[0-9a-f]{64}"))
    assert(trajectory.relationOccurrenceBinding.certifiedSourcePremiseIds.nonEmpty)
    assertEquals(
      episode.links.map(_.kind).toSet,
      Set(RootCausalLinkKind.RootActorCaptured, RootCausalLinkKind.MaterialCaptureResponse)
    )
    assert(episode.links.exists(link =>
      link.kind == RootCausalLinkKind.RootActorCaptured &&
        link.cause.sameOccurrence(0, "a1a7") &&
        link.effect.sameOccurrence(1, "a8a7")
    ))
    assert(episode.links.exists(link =>
      link.kind == RootCausalLinkKind.MaterialCaptureResponse &&
        link.cause.sameOccurrence(1, "a8a7") &&
        link.effect.sameOccurrence(2, "b6a7")
    ))
    assert(RootOwnedEffectPolicy.admitsLineEpisode(payload, episode))

  private def normalizedLine(
      id: String,
      initialFen: String,
      historyMoves: List[String],
      rootFen: String,
      moves: List[String],
      requireMaterial: Boolean = true,
      whitePovMate: Option[Int] = None
  ): (LineFactEvidence, LineNodeRef) =
    val history = CanonicalPositionHistory
      .from(initialFen, historyMoves, rootFen)
      .toOption
      .getOrElse(fail(s"expected canonical history for $id"))
    val lineHistory = history
      .extend(moves)
      .toOption
      .getOrElse(fail(s"expected canonical line for $id"))
    val replay = CanonicalLineReplay
      .fromHistory(lineHistory.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail(s"expected canonical replay for $id"))
    val moveRefs = replay.replaySteps.map(step =>
      PrincipalVariationEvidence.LineMoveRef(step.ply, step.moveUci, step.fenAfter)
    )
    val first = moveRefs.headOption.getOrElse(fail(s"expected a root move for $id"))
    val line = LineNodeRef(id, first.uci, 1, LineNodeRole.BestReference)
    val facts = PrincipalVariationEvidence.LineFacts(
      line = PrincipalVariationEvidence.LineVariationRef(moveRefs),
      first = first,
      reply = moveRefs.lift(1),
      continuation = moveRefs.lift(2),
      continuationTail = moveRefs.drop(3)
    )
    val material = CandidateLineAssembler
      .lineMaterialSummary(history, lineHistory)
    if requireMaterial && material.isEmpty then fail(s"expected a canonical material ledger for $id")
    val payload = LineFactNormalizer
      .fromValidatedLine(
        id = s"$id-evidence",
        lineRef = line,
        facts = facts,
        replay = replay,
        position = PositionNodeRef(rootFen, 0, replay.legalSteps.headOption.map(_.move.piece.color)),
        scope = line.role.scope,
        materialSummary = material,
        whitePovMate = whitePovMate
      )
      .payload match
      case value: LineFactEvidence => value
      case _                       => fail(s"expected normalized line evidence for $id")
    payload -> line
