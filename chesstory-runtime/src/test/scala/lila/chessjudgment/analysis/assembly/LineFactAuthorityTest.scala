package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }

import lila.chessjudgment.analysis.line.LineFactNormalizer
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }

class LineFactAuthorityTest extends munit.FunSuite:

  private object LineAccessBranchRole extends CausalBranchRole:
    val stableKey = "line-access-reference"

  private object LineAccessStateRole extends CausalStateRole:
    val stableKey = "intervening-slider-reach"

  test("an immediate recapture stays an exact lower line fact"):
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
    val line = LineNodeRef("exact-recapture", first.uci)
    val facts = PrincipalVariationEvidence.LineFacts(
      line = PrincipalVariationEvidence.LineVariationRef(moveRefs),
      first = first,
      reply = moveRefs.lift(1),
      continuation = None,
      continuationTail = Nil
    )
    val material = LegalLineAssembler
      .lineMaterialSummary(history, lineHistory)
      .getOrElse(fail("expected the canonical material ledger"))
    val payload = LineFactNormalizer
      .fromValidatedLine(
        id = "exact-recapture-evidence",
        lineRef = line,
        facts = facts,
        replay = replay,
        position = PositionNodeRef(rootFen, 0, Some(Black)),
        scope = EvidenceScope.LegalLine,
        materialSummary = Some(material)
      )
      .payload match
      case value: LineFactEvidence => value
      case _                       => fail("expected normalized line evidence")

    val recapture = payload.materialCaptures
      .find(capture => EvidenceRef.sameMove(capture.moveUci, "b1d2"))
      .getOrElse(fail("expected the canonical recapture occurrence"))
    assert(recapture.recaptureStatus.isInstanceOf[LineMaterialRecaptureStatus.Proven])

    assertEquals(
      payload.lineConsequences
        .filter(_.kind == LineConsequenceKind.RecaptureSequence)
        .flatMap(_.eventOccurrence)
        .map(occurrence => occurrence.plyOffset -> occurrence.moveUci),
      List(1 -> "b1d2")
    )

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

    assertEquals(
      payload.lineConsequences
        .filter(_.kind == LineConsequenceKind.MaterialGain)
        .flatMap(_.eventOccurrence)
        .map(_.plyOffset),
      List(0)
    )

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
    assertEquals(liability.rootMove, Some(line.rootMove))

  test("exact slider reach reuses its certified relation occurrence"):
    val rootFen = "r6k/8/6Kp/8/8/8/B7/R7 w - - 0 1"
    val (payload, line) = normalizedLine(
      id = "line-clearance-without-bounded-proof",
      initialFen = rootFen,
      historyMoves = Nil,
      rootFen = rootFen,
      moves = List("a2b3", "h6h5", "a1a8")
    )
    val replay = payload.certifiedReplay.getOrElse(fail("expected the certified clearance replay"))
    val steps = payload.lineReplaySteps
    val lineRecord = EvidenceRecord(
      EvidenceRef(
        "line-access-state-owner",
        EvidenceProducer.LegalLineProducer,
        EvidenceLayer.Line,
        PositionNodeRef(rootFen, 0, Some(White)),
        Some(line),
        EvidenceScope.LegalLine,
        EvidenceConfidence.LegalReplayVerified
      ),
      payload
    )
    val trajectories = LineAccessTrajectory
      .findAllRootClearancesBeforeUse(
        steps.head,
        steps(2),
        List(steps(1)),
        replay,
        _ => Some(lineRecord)
      )
    val trajectory = trajectories match
      case exact :: Nil => exact
      case other        => fail(s"expected one exact slider-reach occurrence, found ${other.size}")

    assertEquals(trajectory.vacatedSquares, List(EvidenceSquare("a2")))
    assertEquals(trajectory.enabledFrom, EvidenceSquare("a1"))
    assertEquals(trajectory.enabledTo, EvidenceSquare("a8"))
    assertEquals(
      trajectory.relationOccurrenceAuthority.contract,
      VerticalRelationContractKind.SliderReachDelta
    )
    assert(trajectory.relationOccurrenceAuthority.occurrenceId.matches("[0-9a-f]{64}"))
    assert(trajectory.relationOccurrenceAuthority.certifiedSourcePremiseIds.nonEmpty)
    assertEquals(trajectory.persistenceStates.map(_.step), List(steps(1)))
    assert(
      trajectory.persistenceStates.forall(state =>
        state.occurrence.certifies(state.proof, EvidenceScope.LegalLine)
      )
    )
    val persistence = trajectory.persistenceStates.head
    val inventory = replay
      .analysisAfter(steps(1))
      .getOrElse(fail("expected the intervening position analysis"))
      .relationInventory
    val firstCertificate = inventory
      .certifyState(persistence.proof.query)
      .getOrElse(fail("expected the cached slider-reach state"))
    val repeatedCertificate = inventory
      .certifyState(persistence.proof.query)
      .getOrElse(fail("expected the repeated slider-reach state"))
    assert(firstCertificate eq repeatedCertificate)

    val (foreignPayload, _) = normalizedLine(
      id = "foreign-line-clearance-owner",
      initialFen = rootFen,
      historyMoves = Nil,
      rootFen = rootFen,
      moves = List("a2b3", "h6h5", "a1a8")
    )
    val foreignReplay = foreignPayload.certifiedReplay.getOrElse(fail("expected the foreign replay"))
    val foreignOccurrence = foreignReplay
      .positionAfter(foreignReplay.replaySteps(1))
      .getOrElse(fail("expected the foreign intervening occurrence"))
    val foreignInventory = foreignReplay
      .analysisAfter(foreignReplay.replaySteps(1))
      .getOrElse(fail("expected the foreign intervening analysis"))
      .relationInventory
    assertEquals(
      foreignInventory.bindState(firstCertificate, foreignOccurrence.position, EvidenceScope.LegalLine),
      None
    )

    val rootOccurrence = CertifiedRootOccurrenceTestFixture.from(
      line,
      lineRecord,
      replay,
      CausalRootProvenance.CounterfactualAnalyzedRoot
    )
    val branch = CausalBranchOccurrence.fromRootOccurrence(
      LineAccessBranchRole,
      rootOccurrence,
      steps.size
    )
    val binding = CausalClosedStateBinding.afterStep(
      LineAccessStateRole,
      persistence,
      branch,
      1
    )
    assertEquals(binding.issuerOccurrenceId, persistence.occurrence.occurrenceId)
    assertEquals(binding.queryKey, persistence.proof.query.stableKey)
    assertEquals(binding.afterStepIndex, 1)
    assert(binding.semanticProofId.matches("[0-9a-f]{64}"))
    assert(payload.lineConsequences.exists(_.kind == LineConsequenceKind.Mate))

  test("capture and recapture keep one exact relation occurrence authority"):
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
    val lineRecord = EvidenceRecord(
      EvidenceRef(
        "capture-response-occurrence-owner",
        EvidenceProducer.LegalLineProducer,
        EvidenceLayer.Line,
        PositionNodeRef(rootFen, 1, Some(White)),
        Some(line),
        EvidenceScope.LegalLine,
        EvidenceConfidence.LegalReplayVerified
      ),
      payload
    )
    val trajectory = CaptureResponseFollowUpTrajectory
      .find(steps.head, steps(1), steps(2), List(steps(1)), replay, _ => Some(lineRecord))
      .getOrElse(fail("expected the exact capture-response continuation"))
    assertEquals(
      trajectory.relationOccurrenceAuthority.contract,
      VerticalRelationContractKind.CaptureRecaptureInventory
    )
    assert(trajectory.relationOccurrenceAuthority.occurrenceId.matches("[0-9a-f]{64}"))
    assert(trajectory.relationOccurrenceAuthority.certifiedSourcePremiseIds.nonEmpty)
    assertEquals(trajectory.triggerStep, steps.head)
    assertEquals(trajectory.replyStep, steps(1))
    assertEquals(trajectory.followUpStep, steps(2))

  private def normalizedLine(
      id: String,
      initialFen: String,
      historyMoves: List[String],
      rootFen: String,
      moves: List[String],
      requireMaterial: Boolean = true
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
    val line = LineNodeRef(id, first.uci)
    val facts = PrincipalVariationEvidence.LineFacts(
      line = PrincipalVariationEvidence.LineVariationRef(moveRefs),
      first = first,
      reply = moveRefs.lift(1),
      continuation = moveRefs.lift(2),
      continuationTail = moveRefs.drop(3)
    )
    val material = LegalLineAssembler
      .lineMaterialSummary(history, lineHistory)
    if requireMaterial && material.isEmpty then fail(s"expected a canonical material ledger for $id")
    val payload = LineFactNormalizer
      .fromValidatedLine(
        id = s"$id-evidence",
        lineRef = line,
        facts = facts,
        replay = replay,
        position = PositionNodeRef(rootFen, 0, replay.legalSteps.headOption.map(_.move.piece.color)),
        scope = EvidenceScope.LegalLine,
        materialSummary = material
      )
      .payload match
      case value: LineFactEvidence => value
      case _                       => fail(s"expected normalized line evidence for $id")
    payload -> line
