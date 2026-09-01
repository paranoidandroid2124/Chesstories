package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  JudgmentProvenanceAllocator,
  MoveReviewJudgmentOrchestrator,
  RawMoveReviewInput,
  RelationCausalProofTestDispatch,
  RelativeAssessmentAssembler,
  RelativeCauseDraftPlanner,
  RelativeCauseSignalProfile
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.line.EngineLine

class UniqueCheckReplyDefenderDisplacementBeforeCaptureTest extends munit.FunSuite:

  private val rootFen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1"
  private val referenceMoves = List("c4f7", "f8f7", "d1d8")
  private val playedMoves = List("d1d8", "f8d8")

  test("forced reply proof consumes exact L1 inventories and a closed recapture absence"):
    val referenceLine = LineNodeRef("reference-order", referenceMoves.head, 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-order", playedMoves.head, 1, LineNodeRole.Played)
    val referenceReplay = certifiedReplay(referenceMoves)
    val playedReplay = certifiedReplay(playedMoves)

    val proofs = deriveProofs(
      "exact",
      referenceLine,
      playedLine,
      referenceReplay,
      playedReplay
    )
    val exact = proofs.headOption
      .getOrElse(fail("expected one unique-check-reply defender-displacement proof"))

    assertEquals(exact.occurrence.referenceSteps.map(_.moveUci), referenceMoves)
    assertEquals(exact.occurrence.playedSteps.map(_.moveUci), playedMoves)
    assertEquals(exact.semantic.forcedReply.moveUci, "f8f7")
    assertEquals(exact.semantic.disabledDefender.side, chess.Black)
    assertEquals(exact.semantic.disabledDefender.role.name.toLowerCase, "rook")
    assertEquals(exact.semantic.disabledDefender.square.key.toLowerCase, "f8")
    assertEquals(exact.semantic.realizer.from.key.toLowerCase, "d1")
    assertEquals(exact.semantic.realizer.to.key.toLowerCase, "d8")
    assertEquals(exact.semantic.playedDefense.moveUci, "f8d8")
    val path = exact.occurrence.proofPaths match
      case one :: Nil => one
      case other      => fail(s"expected one exact proof path, found ${other.size}")
    assertEquals(
      path.premiseUses.map(_.role),
      List(
        UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.CreatedCheckResponse,
        UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.ReferenceCaptureRecapture,
        UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.PlayedCaptureRecapture
      )
    )
    assert(path.premiseUses.head.result.stableKey.startsWith("created_check_response_inventory:"))
    assertEquals(
      path.premiseUses.count(_.result.stableKey.startsWith("capture_recapture_inventory:")),
      2
    )
    val absenceUse = path.closedAbsenceUses match
      case one :: Nil => one
      case other      => fail(s"expected one exact closed-absence use, found ${other.size}")
    assertEquals(path.closedStateUses, Nil)
    assertEquals(absenceUse.binding.queryKey, "legal-capture:black:d8")
    assertEquals(absenceUse.binding.afterStepIndex, 2)
    assertEquals(absenceUse.binding.branchId, exact.occurrence.referenceBranch.branchId)
    assertEquals(absenceUse.binding.issuerEvidenceId, "reference-source-exact")
    val issuerOccurrence = referenceReplay
      .positionAfter(referenceReplay.replaySteps(2))
      .getOrElse(fail("expected the exact absence issuer occurrence"))
    assertEquals(absenceUse.binding.issuerOccurrenceId, issuerOccurrence.occurrenceId)
    assert(absenceUse.binding.semanticProofId.matches("[0-9a-f]{64}"))
    assert(exact.dependency.value.matches("[0-9a-f]{64}"))

  test("the exact replay position owns one cached absence while line paths remain distinct"):
    val replay = certifiedReplay(referenceMoves)
    val captureStep = replay.replaySteps.lift(2).getOrElse(fail("missing reference capture"))
    val occurrence = replay.positionAfter(captureStep).getOrElse(fail("missing capture destination"))
    val exactQuery = lila.chessjudgment.analysis.position.PositionRelationExtractor
      .ClosedRelationAbsenceQuery
      .LegalCaptureOf(chess.Black, EvidenceSquare("d8"))
    val absenceScope = LineNodeRole.BestReference.scope
    val bound = occurrence
      .closedAbsence(exactQuery, absenceScope)
      .getOrElse(fail("expected exact after-occurrence binding"))
    assertEquals(bound.query, exactQuery)
    assert(occurrence.certifies(bound, absenceScope))

    val repeated = replay.positionAfter(captureStep).getOrElse(fail("missing repeated destination"))
    assertEquals(repeated.occurrenceId, occurrence.occurrenceId)
    assert(repeated.sameOwner(occurrence))

    val inventory = replay
      .analysisAfter(captureStep)
      .getOrElse(fail("missing capture analysis"))
      .relationInventory
    val firstCertificate = inventory.certifyAbsence(exactQuery).getOrElse(fail("missing cached absence"))
    val secondCertificate = inventory.certifyAbsence(exactQuery).getOrElse(fail("missing repeated absence"))
    assert(firstCertificate eq secondCertificate)

    val line = LineNodeRef("position-owner", referenceMoves.head, 1, LineNodeRole.BestReference)
    val branch = CausalBranchOccurrence.certifiedCounterfactual(
      UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.CounterfactualReference,
      line,
      replay,
      referenceMoves.size
    )
    val record = EvidenceRecord(
      lineRef("position-owner-record", line, rootFen),
      LineFactEvidence.fromCertifiedReplay(line, replay)
    )
    val authority = ClosedRelationAbsenceAuthority
      .certified(record, occurrence, bound)
      .getOrElse(fail("expected graph-owned absence authority"))
    val binding = CausalClosedAbsenceBinding.afterStep(
      UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole.ReferenceRecaptureAbsent,
      authority,
      branch,
      2
    )
    val foreignLine = line.copy(id = "foreign-position-owner")
    val foreignBranch = CausalBranchOccurrence.certifiedCounterfactual(
      UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.CounterfactualReference,
      foreignLine,
      replay,
      referenceMoves.size
    )
    val foreignRecord = EvidenceRecord(
      lineRef("foreign-position-owner-record", foreignLine, rootFen),
      LineFactEvidence.fromCertifiedReplay(foreignLine, replay)
    )
    val foreignAuthority = ClosedRelationAbsenceAuthority
      .certified(foreignRecord, occurrence, bound)
      .getOrElse(fail("expected foreign graph-owned absence authority"))
    val foreignBinding = CausalClosedAbsenceBinding.afterStep(
      UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole.ReferenceRecaptureAbsent,
      foreignAuthority,
      foreignBranch,
      2
    )
    assertEquals(binding.issuerOccurrenceId, foreignBinding.issuerOccurrenceId)
    assertNotEquals(binding.branchId, foreignBinding.branchId)
    assertNotEquals(binding.issuerEvidenceId, foreignBinding.issuerEvidenceId)
    intercept[IllegalArgumentException] {
      CausalClosedAbsenceBinding.afterStep(
        UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole.ReferenceRecaptureAbsent,
        foreignAuthority,
        branch,
        2
      )
    }

    assertEquals(
      occurrence.closedAbsence(
        lila.chessjudgment.analysis.position.PositionRelationExtractor
          .ClosedRelationAbsenceQuery
          .GeometricControlOf(chess.Black, EvidenceSquare("f7")),
        absenceScope
      ),
      None
    )

  test("semantic proof survives line transposition identity while occurrences remain distinct"):
    val referenceReplay = certifiedReplay(referenceMoves)
    val playedReplay = certifiedReplay(playedMoves)
    def derive(path: String): CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture =
      val referenceLine = LineNodeRef(s"reference-$path", referenceMoves.head, 1, LineNodeRole.BestReference)
      val playedLine = LineNodeRef(s"played-$path", playedMoves.head, 1, LineNodeRole.Played)
      deriveProofs(path, referenceLine, playedLine, referenceReplay, playedReplay)
        .headOption
        .getOrElse(fail(s"expected exact proof for $path"))

    val first = derive("first")
    val second = derive("second")
    assertEquals(first.semantic.semanticId, second.semantic.semanticId)
    assertNotEquals(first.occurrence.occurrenceId, second.occurrence.occurrenceId)
    assertNotEquals(first.dependency.value, second.dependency.value)

  test("dependency fingerprint retains exact demand ownership"):
    val referenceLine = LineNodeRef("reference-demand", referenceMoves.head, 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-demand", playedMoves.head, 1, LineNodeRole.Played)
    val referenceReplay = certifiedReplay(referenceMoves)
    val playedReplay = certifiedReplay(playedMoves)
    val first = deriveProofs(
      "same-path",
      referenceLine,
      playedLine,
      referenceReplay,
      playedReplay,
      demandId = "demand-owner-a"
    ).head
    val second = deriveProofs(
      "same-path",
      referenceLine,
      playedLine,
      referenceReplay,
      playedReplay,
      demandId = "demand-owner-b"
    ).head

    assertEquals(first.semantic.semanticId, second.semantic.semanticId)
    assertEquals(first.occurrence.occurrenceId, second.occurrence.occurrenceId)
    assertNotEquals(first.dependency.value, second.dependency.value)

  test("a changed exact demand dispatches beside stale proof state and only its proof is consumed"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = playedMoves.head,
          variations = List(
            EngineLine(referenceMoves, scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(playedMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected exact lower facts"))
    val initial = RelativeAssessmentAssembler.enrichFacts(lowerFacts)
    val oldProof = initial.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) => record
    }.getOrElse(fail("expected the first dependency-owned proof"))
    val oldDemand = initial.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => record
    }.getOrElse(fail("expected the first exact comparison demand"))
    val oldFact = oldDemand.payload.asInstanceOf[CandidateComparisonEvidence].comparison
    val changedFact = oldFact
    val changedDemand = oldDemand.copy(
      ref = oldDemand.ref.copy(id = s"${oldDemand.ref.id}:changed")
    )
    val staleContext = initial.withEvidence(changedDemand)
    val dispatched = dispatchForcedReply(
      staleContext,
      JudgmentProvenanceAllocator.forInput(lowerFacts.input),
      changedDemand
    ) match
      case exact :: Nil => exact
      case other        => fail(s"expected one changed-dependency dispatch, found ${other.size}")
    val oldPayload = oldProof.payload.asInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence]
    val newPayload = dispatched.payload.asInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence]
    assertEquals(newPayload.semanticId, oldPayload.semanticId)
    assertEquals(newPayload.occurrenceId, oldPayload.occurrenceId)
    assertNotEquals(newPayload.dependencyId, oldPayload.dependencyId)
    assertNotEquals(dispatched.ref.id, oldProof.ref.id)
    assert(dispatched.ref.id.contains(newPayload.semanticId))
    assert(dispatched.ref.id.contains(newPayload.occurrenceId))
    assert(dispatched.ref.id.contains(newPayload.dependencyId))

    val current = staleContext.withEvidence(dispatched)
    assertEquals(
      dispatchForcedReply(
        current,
        JudgmentProvenanceAllocator.forInput(lowerFacts.input),
        changedDemand
      ).map(_.ref.id),
      List(dispatched.ref.id)
    )
    assertEquals(
      dispatchForcedReply(
        current,
        JudgmentProvenanceAllocator.forInput(lowerFacts.input),
        oldDemand
      ).map(_.ref.id),
      List(oldProof.ref.id)
    )
    val referenceRecords = current.evidenceGraph.records.filter(_.referencesLine(changedFact.referenceLine))
    val candidateRecords = current.evidenceGraph.records.filter(_.referencesLine(changedFact.candidateLine))
    val profile = RelativeCauseSignalProfile.from(
      changedFact,
      changedDemand,
      current.evidenceGraph,
      referenceRecords,
      candidateRecords
    )
    assertEquals(profile.referenceMoveOrderProofs.map(_.ref.id), List(dispatched.ref.id))

  test("missing sibling defense fails closed"):
    val referenceLine = LineNodeRef("reference-closed", referenceMoves.head, 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-open", playedMoves.head, 1, LineNodeRole.Played)
    val incompletePlayed = certifiedReplay(playedMoves.take(1))

    assertEquals(
      deriveProofs(
        "missing-defense",
        referenceLine,
        playedLine,
        certifiedReplay(referenceMoves),
        incompletePlayed
      ),
      Nil
    )

  test("multiple sibling recapturers fail closed instead of over-attributing one forced displacement"):
    val multipleDefenseFen = "3qkb2/4pn2/8/8/2B5/8/8/3R2K1 w - - 0 1"
    val multipleReferenceMoves = List("c4f7", "e8f7", "d1d8")
    val multiplePlayedMoves = List("d1d8", "e8d8")
    val referenceLine =
      LineNodeRef("reference-multiple-defense", multipleReferenceMoves.head, 1, LineNodeRole.BestReference)
    val playedLine =
      LineNodeRef("played-multiple-defense", multiplePlayedMoves.head, 1, LineNodeRole.Played)
    val referenceReplay = certifiedReplay(multipleDefenseFen, multipleReferenceMoves)
    val playedReplay = certifiedReplay(multipleDefenseFen, multiplePlayedMoves)
    val playedCapture = playedReplay
      .verticalRelationOccurrences(
        playedReplay.replaySteps.head,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      )
      .flatMap(_.relation.detail match
        case exact: RelationWitnessDetail.CaptureRecaptureInventory => List(exact)
        case _                                                       => Nil
      ) match
      case exact :: Nil => exact
      case other        => fail(s"expected one played recapture inventory, found ${other.size}")

    assertEquals(
      playedCapture.legalRecaptures.map(_.moveUci).sorted,
      List("e8d8", "f7d8")
    )
    assertEquals(
      deriveProofs(
        "multiple-defense",
        referenceLine,
        playedLine,
        referenceReplay,
        playedReplay,
        root = multipleDefenseFen
      ),
      Nil
    )

  test("a delayed realizer is not attributed without intervening dependency proofs"):
    val delayedMoves = List("c4f7", "f8f7", "g1h2", "h7h6", "d1d8")
    val referenceLine = LineNodeRef("reference-delayed", delayedMoves.head, 1, LineNodeRole.BestReference)
    val playedLine = LineNodeRef("played-immediate", playedMoves.head, 1, LineNodeRole.Played)

    assertEquals(
      deriveProofs(
        "delayed-realizer",
        referenceLine,
        playedLine,
        certifiedReplay(delayedMoves),
        certifiedReplay(playedMoves)
      ),
      Nil
    )

  test("a canonical non-demand comparison does not dispatch the L2 producer"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = playedMoves.head,
          variations = List(
            EngineLine(referenceMoves, scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(playedMoves, scoreCp = 0, depth = 24),
            EngineLine(List("b3b7"), scoreCp = -100, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected the lower facts to assemble"))
    val quietContext = RelativeAssessmentAssembler.enrichFacts(lowerFacts)
    val quietRecord = quietContext.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.ReferenceVsAlternative => record
    }.getOrElse(fail("expected the canonical non-demand comparison"))
    val quietComparison = quietRecord.payload.asInstanceOf[CandidateComparisonEvidence].comparison
    assert(quietContext.evidenceGraph.proofEligible(quietRecord))
    assert(!ActionablePlayedVsBestCausalProofDemand.accepts(quietComparison))

    assertEquals(
      dispatchForcedReply(
        quietContext,
        JudgmentProvenanceAllocator.forInput(lowerFacts.input),
        quietRecord
      ),
      Nil
    )

  test("the exact L2 occurrence is the only WrongMoveOrder direct proof consumer"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = playedMoves.head,
          variations = List(
            EngineLine(referenceMoves, scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(playedMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected the comparison to assemble"))
    assertEquals(
      lowerFacts.evidenceGraph.records.count(_.payload.isInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence]),
      0
    )
    val relativeFacts = RelativeAssessmentAssembler.enrichFacts(lowerFacts)
    val causalRecords = relativeFacts.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) => record
    }
    assertEquals(causalRecords.size, 1)
    assert(relativeFacts.evidenceGraph.proofEligible(causalRecords.head))
    val reentered = RelativeAssessmentAssembler.enrichFacts(relativeFacts)
    val reenteredCausal = reentered.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) => record
    }
    assertEquals(reenteredCausal.map(_.ref.id), causalRecords.map(_.ref.id))
    assertEquals(
      reenteredCausal.map(_.payload.asInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence].dependencyId),
      causalRecords.map(_.payload.asInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence].dependencyId)
    )

    val (comparisonRecord, comparisonRef, comparison) = relativeFacts.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(ref, CandidateComparisonEvidence(value), _) =>
        (record, ref, value)
    }.getOrElse(fail("expected the exact candidate comparison"))
    val duplicateOwner = causalRecords.head.copy(
      ref = causalRecords.head.ref.copy(id = s"${causalRecords.head.ref.id}:duplicate")
    )
    val duplicated = relativeFacts.withEvidence(duplicateOwner)
    assert(duplicated.evidenceGraph.proofEligible(duplicateOwner))
    val duplicateFailure = intercept[IllegalStateException] {
      dispatchForcedReply(
        duplicated,
        JudgmentProvenanceAllocator.forInput(lowerFacts.input),
        comparisonRecord
      )
    }
    assertEquals(
      duplicateFailure.getMessage,
      "one exact unique-check-reply-defender-displacement-before-capture dependency has multiple graph owners"
    )
    val referenceRecords = relativeFacts.evidenceGraph.records.filter(_.referencesLine(comparison.referenceLine))
    val candidateRecords = relativeFacts.evidenceGraph.records.filter(_.referencesLine(comparison.candidateLine))
    val profile = RelativeCauseSignalProfile.from(
      comparison,
      comparisonRecord,
      relativeFacts.evidenceGraph,
      referenceRecords,
      candidateRecords
    )
    assertEquals(profile.referenceMoveOrderProofs.map(_.ref.id), causalRecords.map(_.ref.id))
    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(
      drafts.filter(_.kind == RelativeCauseKind.MissedTacticalResource),
      Nil,
      "a root check/capture resource without an exact L2 family must not become a missed tactic"
    )
    val moveOrderDraft = drafts
      .find(_.kind == RelativeCauseKind.WrongMoveOrder)
      .getOrElse(fail("expected the L2-backed move-order draft"))
    val binding = RelativeCauseFact.binding(
      comparisonKind = comparison.kind,
      referenceLine = comparison.referenceLine,
      candidateLine = comparison.candidateLine,
      sourceSide = moveOrderDraft.sourceSide
    )
    val provisionalCause = RelativeCauseFact(
      kind = RelativeCauseKind.WrongMoveOrder,
      comparisonEvidence = comparisonRef,
      sourceSide = RelativeCauseSourceSide.Reference,
      proofSources = moveOrderDraft.support.map(_.ref)
    )
    val rawChannels = DirectCauseChannel.certifiedForCause(
      provisionalCause,
      relativeFacts.evidenceGraph
    )
    assertEquals(
      causalRecords.head.ref.position,
      comparisonRef.position,
      clues(causalRecords.head.ref.position, comparisonRef.position)
    )
    assertEquals(
      relativeFacts.evidenceGraph.relativeCauseBinding(provisionalCause).map(_.eventLine),
      Some(comparison.referenceLine)
    )
    assert(
      relativeFacts.evidenceGraph.certifiedRootActorFor(comparison.referenceLine).nonEmpty,
      clues(comparison.referenceLine)
    )
    assert(relativeFacts.evidenceGraph.proofEligible(causalRecords.head))
    val rootActor = relativeFacts.evidenceGraph.certifiedRootActorFor(comparison.referenceLine).get
    val causalPayload = causalRecords.head.payload.asInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence]
    assertEquals(causalPayload.semantic.trigger.side, rootActor.color)
    assertEquals(causalPayload.semantic.trigger.beforeRole, rootActor.role)
    assertEquals(causalPayload.semantic.trigger.from, rootActor.from)
    assertEquals(causalPayload.semantic.trigger.to, rootActor.to)
    assert(rawChannels.nonEmpty)
    assert(
      rawChannels.forall(channel =>
        channel.familyMetadata == DirectCauseFamilyMetadata(moveOrderDraft.kind, moveOrderDraft.sourceSide)
      )
    )
    assert(rawChannels.forall(channel => channel.rootOwnedProof match
      case _: RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture => true
      case _                                                       => false
    ))
    assertEquals(
      DirectCauseChannel.certifiedForCause(
        provisionalCause.copy(kind = RelativeCauseKind.MissedTacticalResource),
        relativeFacts.evidenceGraph
      ),
      Nil,
      "the typed direct-channel family must reject a second Cause-kind mapping"
    )
    intercept[IllegalArgumentException] {
      DirectCauseChannel.validateAndOrder(rawChannels ++ rawChannels.take(1))
    }

    val withCauses = RelativeAssessmentAssembler.enrichCauses(relativeFacts)
    val moveOrderCauses = withCauses.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder => cause
    }
    assertEquals(moveOrderCauses.size, 1)
    val cause = moveOrderCauses.head
    val directChannels = DirectCauseChannel.certifiedForCause(cause, withCauses.evidenceGraph)
    assert(directChannels.nonEmpty)
    assert(directChannels.forall(channel => channel.rootOwnedProof match
      case _: RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture => true
      case _                                                       => false
    ))
    assert(cause.proofSources.exists(_.id == causalRecords.head.ref.id))

  test("the certified player-facing Cause retains the exact L2 occurrence"):
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = playedMoves.head,
          variations = List(
            EngineLine(referenceMoves, scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(playedMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected an evidence-backed judgment packet"))

    val selected = packet.causeExposureResolution.certifiedCauses
      .find { idea =>
        packet.evidenceGraph.record(idea.selection.causeEvidence).exists {
          case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
            cause.kind == RelativeCauseKind.WrongMoveOrder
          case _ => false
        }
      }
      .getOrElse(fail("expected the L2-backed cause in the selected commentary"))

    assert(packet.playerFacingClaimDecisions.exists(_.claimId == selected.ownerClaimId))
    assert(selected.directChannels.nonEmpty)
    assert(selected.directChannels.forall(channel => channel.rootOwnedProof match
      case _: RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture => true
      case _                                                       => false
    ))
    assert(selected.directChannels.forall(channel => channel.rootOwnedProof match
      case RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture(_, result) =>
        result.proofPaths.nonEmpty
      case _ => false
    ))

  private def certifiedReplay(moves: List[String]): CanonicalLineReplay =
    certifiedReplay(rootFen, moves)

  private def certifiedReplay(fen: String, moves: List[String]): CanonicalLineReplay =
    PrincipalVariationEvidence
      .legalMoveReplay(fen, moves, startPly = 0)
      .flatMap(CanonicalLineReplay.fromLegalReplay)
      .getOrElse(fail(s"expected legal replay for ${moves.mkString(" ")}"))

  private def deriveProofs(
      path: String,
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      demandId: String = "",
      root: String = rootFen
  ): List[CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture] =
    val referenceRecord = EvidenceRecord(
      lineRef(s"reference-source-$path", referenceLine, root),
      LineFactEvidence.fromCertifiedReplay(referenceLine, referenceReplay)
    )
    val playedRecord = EvidenceRecord(
      lineRef(s"played-source-$path", playedLine, root),
      LineFactEvidence.fromCertifiedReplay(playedLine, playedReplay)
    )
    val comparison = CandidateComparisonFact(
      kind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      comparison = EvalComparison(
        mover = White,
        candidateWinPercentDeltaForMover = -25.0,
        verdict = MoveChoiceVerdict.Blunder,
        detail = CandidateComparisonDeltaDetail.EngineEvaluation(0, None)
      ),
      verdictConfidence = VerdictConfidence.EngineBacked
    )
    val demandRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = Option(demandId).filter(_.nonEmpty).getOrElse(s"comparison-demand-$path"),
        producer = EvidenceProducer.RelativeMoveProducer,
        layer = EvidenceLayer.CandidateComparison,
        position = referenceRecord.ref.position,
        line = Some(playedLine),
        scope = EvidenceScope.Counterfactual,
        confidence = EvidenceConfidence.EngineBacked
      ),
      payload = CandidateComparisonEvidence(comparison),
      parents = List(
        referenceRecord.ref,
        playedRecord.ref,
        evaluationRef(s"reference-eval-$path", referenceLine, referenceRecord.ref.position),
        evaluationRef(s"played-eval-$path", playedLine, playedRecord.ref.position)
      ).sortBy(_.id)
    )
    val changedSeed = for
      trigger <- referenceReplay.replaySteps.headOption
      forcedReplyStep <- referenceReplay.replaySteps.lift(1)
      (checkOccurrence, forcedReply) <-
        referenceReplay.exactCheckResponseOccurrenceMembership(trigger, forcedReplyStep)
      referenceExploitStep <- referenceReplay.replaySteps.lift(2)
      referenceExploitOccurrence <- referenceReplay.verticalRelationOccurrences(
        referenceExploitStep,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      ) match
        case exact :: Nil => Some(exact)
        case _            => None
      playedExploitStep <- playedReplay.replaySteps.headOption
      playedReplyStep <- playedReplay.replaySteps.lift(1)
      (playedExploitOccurrence, playedRecapture) <-
        playedReplay.exactRecaptureOccurrenceMembership(playedExploitStep, playedReplyStep)
    yield UniqueCheckReplyDefenderDisplacementBeforeCaptureChangedSeed(
      checkOccurrence,
      forcedReply,
      referenceExploitOccurrence,
      playedExploitOccurrence,
      playedRecapture
    )
    changedSeed.toList.flatMap(seed =>
      UniqueCheckReplyDefenderDisplacementBeforeCaptureProof.deriveImmediate(
        referenceLine,
        playedLine,
        referenceRecord,
        playedRecord,
        demandRecord,
        comparison,
        referenceReplay,
        playedReplay,
        seed
      )
    )

  private def dispatchForcedReply(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      demandSource: EvidenceRecord
  ): List[EvidenceRecord] =
    RelationCausalProofTestDispatch.forced(context, allocator, demandSource)

  private def lineRef(id: String, line: LineNodeRef, root: String): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = PositionNodeRef(root, 0, Some(White), Some("unique-check-reply-root")),
      line = Some(line),
      scope = line.role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )

  private def evaluationRef(
      id: String,
      line: LineNodeRef,
      root: PositionNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.EngineEvalProducer,
      layer = EvidenceLayer.Eval,
      position = root,
      line = Some(line),
      scope = line.role.scope,
      confidence = EvidenceConfidence.EngineBacked
    )
