package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  ForcedReplyResourceDifferentialAssembler,
  JudgmentProvenanceAllocator,
  MoveReviewJudgmentOrchestrator,
  RawMoveReviewInput,
  RelativeAssessmentAssembler,
  RelativeCauseDraftPlanner,
  RelativeCauseSignalProfile
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.line.EngineLine

class ForcedReplyResourceDifferentialTest extends munit.FunSuite:

  private val rootFen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1"
  private val referenceMoves = List("c4f7", "f8f7", "d1d8")
  private val playedMoves = List("d1d8", "f8d8")
  private val removalRootFen = "7k/4p3/5n2/3r2B1/8/8/2B5/K2Q2R1 w - - 0 1"
  private val removalReferenceMoves = List("g5f6", "e7f6", "d1d5")
  private val removalPlayedMoves = List("d1d5", "f6d5")

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
    assertEquals(proofs.map(_.semantic.mechanism), List(ForcedReplyResourceMechanism.ForcedDisplacement))
    val exact = proofs.headOption
      .getOrElse(fail("expected one exact resource differential"))

    assertEquals(exact.occurrence.referenceSteps.map(_.moveUci), referenceMoves)
    assertEquals(exact.occurrence.playedSteps.map(_.moveUci), playedMoves)
    assertEquals(exact.semantic.forcedReply.moveUci, "f8f7")
    assertEquals(exact.semantic.mechanism, ForcedReplyResourceMechanism.ForcedDisplacement)
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
        ForcedReplyResourcePremiseRole.CreatedCheckResponse,
        ForcedReplyResourcePremiseRole.ReferenceCaptureRecapture,
        ForcedReplyResourcePremiseRole.PlayedCaptureRecapture
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
    assertEquals(absenceUse.binding.queryKey, "legal-capture:black:d8")
    assertEquals(absenceUse.binding.afterStepIndex, 2)
    assertEquals(absenceUse.binding.branchId, exact.occurrence.referenceBranch.branchId)
    assertEquals(absenceUse.binding.issuerEvidenceId, "reference-source-exact")
    val issuerOccurrence = referenceReplay.verticalRelationOccurrences(
      referenceReplay.replaySteps(2),
      List(VerticalRelationContractKind.CaptureRecaptureInventory)
    ) match
      case exact :: Nil => exact
      case other        => fail(s"expected one exact absence issuer occurrence, found ${other.size}")
    assertEquals(absenceUse.binding.issuerOccurrenceId, issuerOccurrence.occurrenceId)
    assert(absenceUse.binding.semanticProofId.matches("[0-9a-f]{64}"))
    assert(exact.dependency.value.matches("[0-9a-f]{64}"))

  test("forced recapturer removal consumes the exact root capture before the later target capture"):
    val referenceLine = LineNodeRef(
      "reference-removal",
      removalReferenceMoves.head,
      1,
      LineNodeRole.BestReference
    )
    val playedLine = LineNodeRef(
      "played-removal",
      removalPlayedMoves.head,
      1,
      LineNodeRole.Played
    )
    val proofs = deriveProofs(
      "removal",
      referenceLine,
      playedLine,
      certifiedReplay(removalRootFen, removalReferenceMoves),
      certifiedReplay(removalRootFen, removalPlayedMoves),
      root = removalRootFen
    )
    assertEquals(
      proofs.map(_.semantic.mechanism),
      List(ForcedReplyResourceMechanism.ForcedRecapturerRemoval)
    )
    val exact = proofs.head
    assertEquals(exact.semantic.trigger.from.key.toLowerCase, "g5")
    assertEquals(exact.semantic.trigger.to.key.toLowerCase, "f6")
    assertEquals(exact.semantic.forcedReply.moveUci, "e7f6")
    assertEquals(exact.semantic.realizer.from.key.toLowerCase, "d1")
    assertEquals(exact.semantic.realizer.to.key.toLowerCase, "d5")
    assertEquals(exact.semantic.playedDefense.moveUci, "f6d5")
    assertEquals(exact.semantic.disabledDefender.side, chess.Black)
    assertEquals(exact.semantic.disabledDefender.role.name.toLowerCase, "knight")
    assertEquals(exact.semantic.disabledDefender.square.key.toLowerCase, "f6")
    val path = exact.occurrence.proofPaths match
      case one :: Nil => one
      case other      => fail(s"expected one exact removal proof path, found ${other.size}")
    assertEquals(
      path.premiseUses.map(_.role),
      List(
        ForcedReplyResourcePremiseRole.ReferenceRootCapture,
        ForcedReplyResourcePremiseRole.CreatedCheckResponse,
        ForcedReplyResourcePremiseRole.ReferenceCaptureRecapture,
        ForcedReplyResourcePremiseRole.PlayedCaptureRecapture
      )
    )
    assertEquals(path.premiseUses.take(2).map(_.stepIndex), List(0, 0))
    assertEquals(path.premiseUses(2).stepIndex, 2)
    assertEquals(path.premiseUses(3).stepIndex, 0)
    assertEquals(
      path.closedAbsenceUses.map(_.binding.queryKey),
      List("legal-capture:black:d5")
    )

    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = removalRootFen,
          playedMoveUci = removalPlayedMoves.head,
          variations = List(
            EngineLine(removalReferenceMoves, scoreCp = 600, depth = 24),
            EngineLine(removalPlayedMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected the removal proof to reach the judgment packet"))
    val causalResults = packet.evidenceGraph.records.collect {
      case EvidenceRecord(_, payload: ForcedReplyResourceDifferentialEvidence, _) => payload
    }
    assertEquals(
      causalResults.map(_.semantic.mechanism),
      List(ForcedReplyResourceMechanism.ForcedRecapturerRemoval)
    )
    val selectedResults = packet.causeExposureResolution.narrativeIdeas
      .flatMap(_.facets)
      .flatMap(_.directChannels)
      .flatMap(_.rootOwnedProof)
      .collect {
        case RootOwnedEffectProof.ForcedReplyResourceDifferential(_, result) => result
      }
    assertEquals(
      selectedResults.map(_.semantic.mechanism).distinct,
      List(ForcedReplyResourceMechanism.ForcedRecapturerRemoval)
    )

  test("the exact L1 absence capability binds once and mismatched premises fail closed"):
    val replay = certifiedReplay(referenceMoves)
    val captureStep = replay.replaySteps.lift(2).getOrElse(fail("missing reference capture"))
    val occurrence = replay
      .verticalRelationOccurrences(
        captureStep,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      ) match
      case exact :: Nil => exact
      case other        => fail(s"expected one capture inventory, found ${other.size}")
    val capture = occurrence.relation.detail match
      case exact: RelationWitnessDetail.CaptureRecaptureInventory => exact
      case _ => fail("expected the capture-recapture detail")
    val exactQuery = lila.chessjudgment.analysis.position.PositionRelationExtractor
      .ClosedRelationAbsenceQuery
      .LegalCaptureOf(capture.captured.side, capture.mover.to)
    val exactPremise = ClosedRelationAbsencePremise(
      RelationSnapshotOccurrence.After,
      exactQuery
    )
    val capability = occurrence
      .absenceCapability(exactPremise)
      .getOrElse(fail("expected the L1-owned recapture-absence capability"))
    val after = replay.after(captureStep).getOrElse(fail("missing capture destination"))
    val position = PositionNodeRef(captureStep.fenAfter, captureStep.ply, Some(after.color))
    val bound = capability
      .bind(position, EvidenceScope.Counterfactual)
      .getOrElse(fail("expected exact after-occurrence binding"))
    assertEquals(bound.query, exactQuery)

    assertEquals(
      occurrence.absenceCapability(exactPremise.copy(occurrence = RelationSnapshotOccurrence.Before)),
      None
    )
    val missingPremise = ClosedRelationAbsencePremise(
      RelationSnapshotOccurrence.After,
      lila.chessjudgment.analysis.position.PositionRelationExtractor
        .ClosedRelationAbsenceQuery
        .AnyLegalMove(capture.captured.side)
    )
    assertEquals(occurrence.absenceCapability(missingPremise), None)

  test("semantic proof survives line transposition identity while occurrences remain distinct"):
    val referenceReplay = certifiedReplay(referenceMoves)
    val playedReplay = certifiedReplay(playedMoves)
    def derive(path: String): CertifiedForcedReplyResourceDifferential =
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
      case record @ EvidenceRecord(_, _: ForcedReplyResourceDifferentialEvidence, _) => record
    }.getOrElse(fail("expected the first dependency-owned proof"))
    val oldDemand = initial.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => record
    }.getOrElse(fail("expected the first exact comparison demand"))
    val oldFact = oldDemand.payload.asInstanceOf[CandidateComparisonEvidence].comparison
    val changedFact = oldFact.copy(
      comparison = oldFact.comparison.copy(
        candidateWinPercentDeltaForMover = oldFact.comparison.candidateWinPercentDeltaForMover - 1.0
      )
    )
    val changedDemand = oldDemand.copy(
      ref = oldDemand.ref.copy(id = s"${oldDemand.ref.id}:changed"),
      payload = CandidateComparisonEvidence(changedFact)
    )
    val staleContext = initial.withEvidence(changedDemand)
    val dispatched = ForcedReplyResourceDifferentialAssembler.fromAssembly(
      staleContext,
      JudgmentProvenanceAllocator.forInput(lowerFacts.input),
      changedDemand
    ) match
      case exact :: Nil => exact
      case other        => fail(s"expected one changed-dependency dispatch, found ${other.size}")
    val oldPayload = oldProof.payload.asInstanceOf[ForcedReplyResourceDifferentialEvidence]
    val newPayload = dispatched.payload.asInstanceOf[ForcedReplyResourceDifferentialEvidence]
    assertEquals(newPayload.semanticId, oldPayload.semanticId)
    assertEquals(newPayload.occurrenceId, oldPayload.occurrenceId)
    assertNotEquals(newPayload.dependencyId, oldPayload.dependencyId)
    assertNotEquals(dispatched.ref.id, oldProof.ref.id)
    assert(dispatched.ref.id.contains(newPayload.semanticId))
    assert(dispatched.ref.id.contains(newPayload.occurrenceId))
    assert(dispatched.ref.id.contains(newPayload.dependencyId))

    val current = staleContext.withEvidence(dispatched)
    assertEquals(
      ForcedReplyResourceDifferentialAssembler.fromAssembly(
        current,
        JudgmentProvenanceAllocator.forInput(lowerFacts.input),
        changedDemand
      ).map(_.ref.id),
      List(dispatched.ref.id)
    )
    assertEquals(
      ForcedReplyResourceDifferentialAssembler.fromAssembly(
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
      candidateRecords,
      Nil
    )
    assertEquals(profile.referenceMoveOrderResource.map(_.ref.id), List(dispatched.ref.id))

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

  test("an inactive public consumer does not dispatch the L2 producer"):
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
      .getOrElse(fail("expected the lower facts to assemble"))
    val reference = lowerFacts.line(LineNodeRole.BestReference).getOrElse(fail("missing reference"))
    val candidate = lowerFacts.line(LineNodeRole.Played).getOrElse(fail("missing candidate"))
    val quietComparison = CandidateComparisonFact(
      kind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = reference.ref,
      candidateLine = candidate.ref,
      comparison = EvalComparison(
        mover = White,
        candidateWinPercentDeltaForMover = 0.0,
        verdict = MoveChoiceVerdict.MatchesReference,
        detail = CandidateComparisonDeltaDetail.EngineEvaluation(0, None)
      ),
      verdictConfidence = VerdictConfidence.EngineBacked
    )
    val quietRecord = EvidenceRecord(
      ref = EvidenceRef(
        id = "quiet-comparison-demand",
        producer = EvidenceProducer.RelativeMoveProducer,
        layer = EvidenceLayer.CandidateComparison,
        position = lowerFacts.root.getOrElse(fail("missing root")),
        line = Some(candidate.ref),
        scope = EvidenceScope.Counterfactual,
        confidence = EvidenceConfidence.EngineBacked
      ),
      payload = CandidateComparisonEvidence(quietComparison)
    )
    val quietContext = lowerFacts.withEvidence(List(quietRecord))
    assert(quietContext.evidenceGraph.proofEligible(quietRecord))
    assert(!ForcedReplyResourceDifferentialDemand.accepts(quietComparison))

    assertEquals(
      ForcedReplyResourceDifferentialAssembler.fromAssembly(
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
      lowerFacts.evidenceGraph.records.count(_.payload.isInstanceOf[ForcedReplyResourceDifferentialEvidence]),
      0
    )
    val relativeFacts = RelativeAssessmentAssembler.enrichFacts(lowerFacts)
    val causalRecords = relativeFacts.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: ForcedReplyResourceDifferentialEvidence, _) => record
    }
    assertEquals(causalRecords.size, 1)
    assert(relativeFacts.evidenceGraph.proofEligible(causalRecords.head))
    val reentered = RelativeAssessmentAssembler.enrichFacts(relativeFacts)
    val reenteredCausal = reentered.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: ForcedReplyResourceDifferentialEvidence, _) => record
    }
    assertEquals(reenteredCausal.map(_.ref.id), causalRecords.map(_.ref.id))
    assertEquals(
      reenteredCausal.map(_.payload.asInstanceOf[ForcedReplyResourceDifferentialEvidence].dependencyId),
      causalRecords.map(_.payload.asInstanceOf[ForcedReplyResourceDifferentialEvidence].dependencyId)
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
      ForcedReplyResourceDifferentialAssembler.fromAssembly(
        duplicated,
        JudgmentProvenanceAllocator.forInput(lowerFacts.input),
        comparisonRecord
      )
    }
    assertEquals(
      duplicateFailure.getMessage,
      "one exact L2 proof dependency identity has multiple graph owners"
    )
    val referenceRecords = relativeFacts.evidenceGraph.records.filter(_.referencesLine(comparison.referenceLine))
    val candidateRecords = relativeFacts.evidenceGraph.records.filter(_.referencesLine(comparison.candidateLine))
    val profile = RelativeCauseSignalProfile.from(
      comparison,
      comparisonRecord,
      relativeFacts.evidenceGraph,
      referenceRecords,
      candidateRecords,
      Nil
    )
    assertEquals(profile.referenceMoveOrderResource.map(_.ref.id), causalRecords.map(_.ref.id))
    val drafts = RelativeCauseDraftPlanner.drafts(profile)
    assertEquals(
      drafts.count(draft =>
        draft.kind == RelativeCauseKind.KingForcing &&
          draft.sourceSide.contains(RelativeCauseSourceSide.Reference)
      ),
      1,
      "the endpoint-positive planner must combine one reference KingForcing meaning before draft creation"
    )
    val moveOrderDraft = drafts
      .find(_.kind == RelativeCauseKind.WrongMoveOrder)
      .getOrElse(fail("expected the L2-backed move-order draft"))
    val binding = RelativeCauseFact.binding(
      kind = moveOrderDraft.kind,
      comparisonKind = comparison.kind,
      referenceLine = comparison.referenceLine,
      candidateLine = comparison.candidateLine,
      supportEvidence = moveOrderDraft.support.map(_.ref),
      explicitSourceSide = moveOrderDraft.sourceSide
    )
    val ownedDirect = RelativeAssessmentAssembler.ownedCauseDirectProofRecords(
      relativeFacts.evidenceGraph,
      comparison,
      moveOrderDraft.kind,
      binding,
      moveOrderDraft.attributionKind,
      moveOrderDraft.support
    )
    assertEquals(ownedDirect.map(_.ref.id), causalRecords.map(_.ref.id))
    val provisionalCause = RelativeCauseFact(
      kind = RelativeCauseKind.WrongMoveOrder,
      comparisonEvidence = comparisonRef,
      supportEvidence = moveOrderDraft.support.map(_.ref),
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = CauseAttribution(
        CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            RelativeCauseProofRole.DirectProof,
            RelativeCauseProofStrength.Primary,
            ownedDirect.map(_.ref)
          )
        )
      )
    )
    val rawChannels = EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(
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
    val causalPayload = causalRecords.head.payload.asInstanceOf[ForcedReplyResourceDifferentialEvidence]
    assertEquals(causalPayload.semantic.trigger.side, rootActor.color)
    assertEquals(causalPayload.semantic.trigger.beforeRole, rootActor.role)
    assertEquals(causalPayload.semantic.trigger.from, rootActor.from)
    assertEquals(causalPayload.semantic.trigger.to, rootActor.to)
    assert(rawChannels.nonEmpty)
    assert(rawChannels.forall(_.rootOwnedProof.exists {
      case _: RootOwnedEffectProof.ForcedReplyResourceDifferential => true
      case _                                                       => false
    }))

    val withCauses = RelativeAssessmentAssembler.enrichCauses(relativeFacts)
    val moveOrderCauses = withCauses.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder => cause
    }
    assertEquals(moveOrderCauses.size, 1)
    val cause = moveOrderCauses.head
    val directChannels = EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(cause, withCauses.evidenceGraph)
    assert(directChannels.nonEmpty)
    assert(directChannels.forall(_.rootOwnedProof.exists {
      case _: RootOwnedEffectProof.ForcedReplyResourceDifferential => true
      case _                                                       => false
    }))
    assert(cause.supportEvidence.exists(_.id == causalRecords.head.ref.id))

  test("the selected player-facing idea retains the exact L2 occurrence"):
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

    val selected = packet.causeExposureResolution.narrativeIdeas
      .flatMap(_.facets)
      .find { facet =>
        packet.evidenceGraph.record(facet.selection.causeEvidence).exists {
          case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
            cause.kind == RelativeCauseKind.WrongMoveOrder
          case _ => false
        }
      }
      .getOrElse(fail("expected the L2-backed cause in the selected commentary"))

    assert(packet.playerFacingClaimDecisions.exists(_.claimId == selected.ownerClaimId))
    assert(selected.directChannels.nonEmpty)
    assert(selected.directChannels.forall(_.binding.horizon.contains("ply:2")))
    assert(selected.directChannels.forall(_.rootOwnedProof.exists {
      case _: RootOwnedEffectProof.ForcedReplyResourceDifferential => true
      case _                                                       => false
    }))
    assert(selected.directChannels.forall(_.proofSegment.isEmpty))
    assert(selected.directChannels.forall(_.rootOwnedProof.exists {
      case RootOwnedEffectProof.ForcedReplyResourceDifferential(_, result) =>
        result.proofPaths.nonEmpty
      case _ => false
    }))

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
  ): List[CertifiedForcedReplyResourceDifferential] =
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
      payload = CandidateComparisonEvidence(comparison)
    )
    ForcedReplyResourceProof.deriveImmediate(
      referenceLine,
      playedLine,
      referenceRecord,
      playedRecord,
      demandRecord,
      comparison,
      referenceReplay,
      playedReplay
    )

  private def lineRef(id: String, line: LineNodeRef, root: String): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = PositionNodeRef(root, 0, Some(White), Some("forced-reply-root")),
      line = Some(line),
      scope = line.role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
