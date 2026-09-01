package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  JudgmentProvenanceAllocator,
  MoveReviewJudgmentOrchestrator,
  OccurrenceExplanationAssembler,
  OccurrenceExplanationDemand,
  ExplanationRequest,
  RawMoveReviewInput,
  RelationCausalProofTestDispatch,
  RelativeAssessmentAssembler
}
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.line.EngineLine

class UniqueCheckReplyDefenderDisplacementBeforeCaptureTest extends munit.FunSuite:

  private val rootFen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1"
  private val displacementMoves = List("c4f7", "f8f7", "d1d8")
  private val immediateMoves = List("d1d8", "f8d8")

  test("forced reply proof consumes exact L1 inventories and a closed recapture absence"):
    val displacementLine = LineNodeRef("displacement-order", displacementMoves.head)
    val immediateLine = LineNodeRef("immediate-order", immediateMoves.head)
    val displacementReplay = certifiedReplay(displacementMoves)
    val immediateReplay = certifiedReplay(immediateMoves)

    val proofs = deriveProofs(
      "exact",
      displacementLine,
      immediateLine,
      displacementReplay,
      immediateReplay
    )
    val exact = proofs.headOption
      .getOrElse(fail("expected one unique-check-reply defender-displacement proof"))

    assertEquals(exact.occurrence.displacementSteps.map(_.moveUci), displacementMoves)
    assertEquals(exact.occurrence.immediateCaptureSteps.map(_.moveUci), immediateMoves)
    assertEquals(exact.semantic.forcedReply.moveUci, "f8f7")
    assertEquals(exact.semantic.disabledDefender.side, chess.Black)
    assertEquals(exact.semantic.disabledDefender.role.name.toLowerCase, "rook")
    assertEquals(exact.semantic.disabledDefender.square.key.toLowerCase, "f8")
    assertEquals(exact.semantic.realizer.from.key.toLowerCase, "d1")
    assertEquals(exact.semantic.realizer.to.key.toLowerCase, "d8")
    assertEquals(exact.semantic.immediateDefense.moveUci, "f8d8")
    val path = exact.occurrence.proofPaths match
      case one :: Nil => one
      case other      => fail(s"expected one exact proof path, found ${other.size}")
    assertEquals(
      path.premiseUses.map(_.role),
      List(
        UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.DisplacementCheckResponse,
        UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.DelayedCaptureRecapture,
        UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.ImmediateCaptureRecapture
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
    assertEquals(
      path.closedStateUses.map(_.binding.role),
      List(
        UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedCaptureActorPresent,
        UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedTargetPresent,
        UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedCaptureActorPresent,
        UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedTargetPresent
      )
    )
    assertEquals(path.closedStateUses.map(_.binding.afterStepIndex), List(0, 0, 1, 1))
    assertEquals(
      path.closedStateUses.map(_.binding.queryKey),
      List(
        "occupied-by:white:rook@d1",
        "occupied-by:black:queen@d8",
        "occupied-by:white:rook@d1",
        "occupied-by:black:queen@d8"
      )
    )
    assertEquals(absenceUse.binding.queryKey, "legal-capture:black:d8")
    assertEquals(absenceUse.binding.afterStepIndex, 2)
    assertEquals(absenceUse.binding.branchId, exact.occurrence.displacementBranch.branchId)
    assertEquals(absenceUse.binding.issuerEvidenceId, "displacement-source-exact")
    val issuerOccurrence = displacementReplay
      .positionAfter(displacementReplay.replaySteps(2))
      .getOrElse(fail("expected the exact absence issuer occurrence"))
    assertEquals(absenceUse.binding.issuerOccurrenceId, issuerOccurrence.occurrenceId)
    assert(absenceUse.binding.semanticProofId.matches("[0-9a-f]{64}"))
    assert(exact.dependency.value.matches("[0-9a-f]{64}"))
    assertEquals(
      exact.proof.parentSources.map(_.id).toSet,
      Set(
        "displacement-source-exact",
        "displacement-source-exact:transition",
        "immediate-source-exact",
        "immediate-source-exact:transition"
      )
    )
    assertEquals(
      exact.proof.parentSources.map(_.layer).toSet,
      Set(EvidenceLayer.Line, EvidenceLayer.MoveTransition)
    )
    assertEquals(
      exact.proof.lowerIssuerRecords.map(_.ref).sortBy(_.id),
      exact.proof.parentSources
    )
    assert(exact.proof.remainsCertified)

  test("missing or role-tampered intermediate occupancy cannot form the forced-reply manifest"):
    val displacementLine = LineNodeRef("displacement-state-adversary", displacementMoves.head)
    val immediateLine = LineNodeRef("immediate-state-adversary", immediateMoves.head)
    val exact = deriveProofs(
      "state-adversary",
      displacementLine,
      immediateLine,
      certifiedReplay(displacementMoves),
      certifiedReplay(immediateMoves)
    ).headOption.getOrElse(fail("expected the exact forced-reply proof"))
    val path = exact.occurrence.proofPaths.head
    val states = path.manifest.stateBindings
    val realizer = RelationColoredPieceWitness(
      exact.semantic.realizer.from,
      exact.semantic.realizer.beforeRole,
      exact.semantic.realizer.side
    )
    def rebuild(candidateStates: List[CausalClosedStateBinding]) =
      UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest.exact(
        path.premiseUses(0),
        path.premiseUses(1),
        path.premiseUses(2),
        path.closedAbsenceUses.head.binding,
        realizer,
        exact.semantic.capturedTarget,
        candidateStates
      )

    intercept[IllegalArgumentException] {
      rebuild(states.dropRight(1))
    }
    val actorAtRoot = states.head
    val targetRoleWithActorState = CausalClosedStateBinding.afterStep(
      UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedTargetPresent,
      actorAtRoot.authority,
      exact.occurrence.displacementBranch,
      actorAtRoot.afterStepIndex
    )
    intercept[IllegalArgumentException] {
      rebuild(states.updated(1, targetRoleWithActorState))
    }

  test("the exact replay position owns one cached absence while line paths remain distinct"):
    val replay = certifiedReplay(displacementMoves)
    val captureStep = replay.replaySteps.lift(2).getOrElse(fail("missing delayed capture"))
    val occurrence = replay.positionAfter(captureStep).getOrElse(fail("missing capture destination"))
    val exactQuery = lila.chessjudgment.analysis.position.PositionRelationExtractor
      .ClosedRelationAbsenceQuery
      .LegalCaptureOf(chess.Black, EvidenceSquare("d8"))
    val absenceScope = EvidenceScope.LegalLine
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

    val line = LineNodeRef("position-owner", displacementMoves.head)
    val record = EvidenceRecord(
      lineRef("position-owner-record", line, rootFen),
      LineFactEvidence.fromCertifiedReplay(line, replay)
    )
    val branch = CausalBranchOccurrence.fromRootOccurrence(
      UniqueCheckReplyBranchRole.DisplacementThenCapture,
      CertifiedRootOccurrenceTestFixture.from(
        line,
        record,
        replay,
        CausalRootProvenance.CounterfactualAnalyzedRoot
      ),
      displacementMoves.size
    )
    val authority = ClosedRelationAbsenceAuthority
      .certified(record, occurrence, bound)
      .getOrElse(fail("expected graph-owned absence authority"))
    val binding = CausalClosedAbsenceBinding.afterStep(
      UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole.DelayedCaptureRecaptureAbsent,
      authority,
      branch,
      2
    )
    val foreignLine = line.copy(id = "foreign-position-owner")
    val foreignRecord = EvidenceRecord(
      lineRef("foreign-position-owner-record", foreignLine, rootFen),
      LineFactEvidence.fromCertifiedReplay(foreignLine, replay)
    )
    val foreignBranch = CausalBranchOccurrence.fromRootOccurrence(
      UniqueCheckReplyBranchRole.DisplacementThenCapture,
      CertifiedRootOccurrenceTestFixture.from(
        foreignLine,
        foreignRecord,
        replay,
        CausalRootProvenance.CounterfactualAnalyzedRoot
      ),
      displacementMoves.size
    )
    val foreignAuthority = ClosedRelationAbsenceAuthority
      .certified(foreignRecord, occurrence, bound)
      .getOrElse(fail("expected foreign graph-owned absence authority"))
    val foreignBinding = CausalClosedAbsenceBinding.afterStep(
      UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole.DelayedCaptureRecaptureAbsent,
      foreignAuthority,
      foreignBranch,
      2
    )
    assertEquals(binding.issuerOccurrenceId, foreignBinding.issuerOccurrenceId)
    assertNotEquals(binding.branchId, foreignBinding.branchId)
    assertNotEquals(binding.issuerEvidenceId, foreignBinding.issuerEvidenceId)
    intercept[IllegalArgumentException] {
      CausalClosedAbsenceBinding.afterStep(
        UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole.DelayedCaptureRecaptureAbsent,
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
    val displacementReplay = certifiedReplay(displacementMoves)
    val immediateReplay = certifiedReplay(immediateMoves)
    def derive(path: String): CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture =
      val displacementLine = LineNodeRef(s"displacement-$path", displacementMoves.head)
      val immediateLine = LineNodeRef(s"immediate-$path", immediateMoves.head)
      deriveProofs(path, displacementLine, immediateLine, displacementReplay, immediateReplay)
        .headOption
        .getOrElse(fail(s"expected exact proof for $path"))

    val first = derive("first")
    val second = derive("second")
    assertEquals(first.semantic.semanticId, second.semantic.semanticId)
    assertNotEquals(first.occurrence.occurrenceId, second.occurrence.occurrenceId)
    assertNotEquals(first.dependency.value, second.dependency.value)

  test("assessment records do not own the occurrence-demanded proof identity"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = immediateMoves.head,
          variations = List(
            EngineLine(displacementMoves, scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(immediateMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected exact lower facts"))
    val assessed = RelativeAssessmentAssembler.enrichComparisonEvidence(lowerFacts)
    assertEquals(
      assessed.evidenceGraph.records.count(
        _.payload.isInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence]
      ),
      0
    )
    val initial = produceExplanations(assessed)
    val oldProof = initial.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) => record
    }.getOrElse(fail("expected the first dependency-owned proof"))
    val assessmentRecord = initial.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => record
    }.getOrElse(fail("expected the first exact Assessment comparison"))
    val changedAssessmentRecord = assessmentRecord.copy(
      ref = assessmentRecord.ref.copy(id = s"${assessmentRecord.ref.id}:changed")
    )
    val assessmentChangedContext = initial.withEvidence(changedAssessmentRecord)
    val dispatched = dispatchForcedReply(
      assessmentChangedContext,
      JudgmentProvenanceAllocator.forInput(lowerFacts.input)
    ) match
      case exact :: Nil => exact
      case other        => fail(s"expected one changed-dependency dispatch, found ${other.size}")
    val oldPayload = oldProof.payload.asInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence]
    val newPayload = dispatched.payload.asInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence]
    assertEquals(newPayload.semanticId, oldPayload.semanticId)
    assertEquals(newPayload.occurrenceId, oldPayload.occurrenceId)
    assertEquals(newPayload.dependencyId, oldPayload.dependencyId)
    assertEquals(dispatched.ref.id, oldProof.ref.id)
    assertEquals(
      dispatched.parents.map(_.layer).toSet,
      Set(EvidenceLayer.Line, EvidenceLayer.MoveTransition)
    )

    val current = assessmentChangedContext
    assertEquals(
      dispatchForcedReply(
        current,
        JudgmentProvenanceAllocator.forInput(lowerFacts.input)
      ).map(_.ref.id),
      List(oldProof.ref.id)
    )

  test("missing sibling defense fails closed"):
    val displacementLine = LineNodeRef("displacement-closed", displacementMoves.head)
    val immediateLine = LineNodeRef("immediate-open", immediateMoves.head)
    val incompleteImmediate = certifiedReplay(immediateMoves.take(1))

    assertEquals(
      deriveProofs(
        "missing-defense",
        displacementLine,
        immediateLine,
        certifiedReplay(displacementMoves),
        incompleteImmediate
      ),
      Nil
    )

  test("multiple sibling recapturers fail closed instead of over-attributing one forced displacement"):
    val multipleDefenseFen = "3qkb2/4pn2/8/8/2B5/8/8/3R2K1 w - - 0 1"
    val multipleDisplacementMoves = List("c4f7", "e8f7", "d1d8")
    val multipleImmediateMoves = List("d1d8", "e8d8")
    val displacementLine =
      LineNodeRef("displacement-multiple-defense", multipleDisplacementMoves.head)
    val immediateLine =
      LineNodeRef("immediate-multiple-defense", multipleImmediateMoves.head)
    val displacementReplay = certifiedReplay(multipleDefenseFen, multipleDisplacementMoves)
    val immediateReplay = certifiedReplay(multipleDefenseFen, multipleImmediateMoves)
    val immediateCapture = immediateReplay
      .verticalRelationOccurrences(
        immediateReplay.replaySteps.head,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      )
      .flatMap(_.relation.detail match
        case exact: RelationWitnessDetail.CaptureRecaptureInventory => List(exact)
        case _                                                       => Nil
      ) match
      case exact :: Nil => exact
      case other        => fail(s"expected one immediate recapture inventory, found ${other.size}")

    assertEquals(
      immediateCapture.legalRecaptures.map(_.moveUci).sorted,
      List("e8d8", "f7d8")
    )
    assertEquals(
      deriveProofs(
        "multiple-defense",
        displacementLine,
        immediateLine,
        displacementReplay,
        immediateReplay,
        root = multipleDefenseFen
      ),
      Nil
    )

  test("a delayed realizer is not attributed without intervening dependency proofs"):
    val delayedMoves = List("c4f7", "f8f7", "g1h2", "h7h6", "d1d8")
    val displacementLine = LineNodeRef("displacement-delayed", delayedMoves.head)
    val immediateLine = LineNodeRef("immediate-capture", immediateMoves.head)

    assertEquals(
      deriveProofs(
        "delayed-realizer",
        displacementLine,
        immediateLine,
        certifiedReplay(delayedMoves),
        certifiedReplay(immediateMoves)
      ),
      Nil
    )

  test("no explanation request performs no typed proof production"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = immediateMoves.head,
          variations = List(
            EngineLine(displacementMoves, scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(immediateMoves, scoreCp = 0, depth = 24),
            EngineLine(List("b3b7"), scoreCp = -100, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected the lower facts to assemble"))
    assertEquals(
      lowerFacts.evidenceGraph.records.count(
        _.payload.isInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence]
      ),
      0
    )

  test("the exact L2 occurrence owns one requested Cause directly"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = immediateMoves.head,
          variations = List(
            EngineLine(displacementMoves, scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(immediateMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected the comparison to assemble"))
    assertEquals(
      lowerFacts.evidenceGraph.records.count(_.payload.isInstanceOf[UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence]),
      0
    )
    val facts = produceExplanations(RelativeAssessmentAssembler.enrichComparisonEvidence(lowerFacts))
    val causalRecords = facts.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) => record
    }
    assertEquals(causalRecords.size, 1)
    assert(facts.evidenceGraph.proofEligible(causalRecords.head))

    val duplicateOwner = causalRecords.head.copy(
      ref = causalRecords.head.ref.copy(id = s"${causalRecords.head.ref.id}:duplicate")
    )
    val duplicated = facts.withEvidence(duplicateOwner)
    assert(duplicated.evidenceGraph.proofEligible(duplicateOwner))
    val duplicateFailure = intercept[IllegalArgumentException] {
      OccurrenceExplanationAssembler.enrichCauses(
        duplicated,
        JudgmentProvenanceAllocator.forInput(lowerFacts.input),
        exactDemand(duplicated)
      )
    }
    assertEquals(
      duplicateFailure.getMessage,
      "requirement failed: one demanded typed proof may own only one occurrence Cause"
    )
    val withCauses = OccurrenceExplanationAssembler.enrichCauses(
      facts,
      JudgmentProvenanceAllocator.forInput(facts.input),
      exactDemand(facts)
    )
    val causes = withCauses.evidenceGraph.records.collect {
      case EvidenceRecord(_, OccurrenceExplanationCauseEvidence(cause), _) => cause
    }
    assertEquals(causes.size, 1)
    assertEquals(causes.head.proofSource, causalRecords.head.ref)
    assertEquals(causes.head.subject, exactDemand(facts).subject.publicOccurrence)

  test("the certified player-facing Cause retains the exact L2 occurrence"):
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = immediateMoves.head,
          variations = List(
            EngineLine(displacementMoves, scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(immediateMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected an evidence-backed judgment packet"))

    val selected = packet.occurrenceExplanations match
      case exact :: Nil => exact
      case other        => fail(s"expected one requested occurrence explanation, found ${other.size}")
    assertEquals(selected.subject.moveUci, immediateMoves.head)
    assert(selected.rootOwnedProof match
      case RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture(_, result) =>
        result.subjectOccurrence == selected.subject && result.proofPaths.nonEmpty
      case _ => false
    )

  private def produceExplanations(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    OccurrenceExplanationAssembler.enrichProofs(
      context,
      JudgmentProvenanceAllocator.forInput(context.input),
      exactDemand(context)
    )

  private def exactDemand(context: JudgmentAssemblyContext): OccurrenceExplanationDemand =
    OccurrenceExplanationDemand
      .resolve(context, ExplanationRequest.forObservedMove(context.input))
      .getOrElse(fail("expected the exact observed occurrence demand"))

  private def certifiedReplay(moves: List[String]): CanonicalLineReplay =
    certifiedReplay(rootFen, moves)

  private def certifiedReplay(fen: String, moves: List[String]): CanonicalLineReplay =
    PrincipalVariationEvidence
      .legalMoveReplay(fen, moves, startPly = 0)
      .flatMap(CanonicalLineReplay.fromLegalReplay)
      .getOrElse(fail(s"expected legal replay for ${moves.mkString(" ")}"))

  private def deriveProofs(
      path: String,
      displacementLine: LineNodeRef,
      immediateLine: LineNodeRef,
      displacementReplay: CanonicalLineReplay,
      immediateReplay: CanonicalLineReplay,
      root: String = rootFen
  ): List[CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture] =
    val displacementRecord = EvidenceRecord(
      lineRef(s"displacement-source-$path", displacementLine, root),
      LineFactEvidence.fromCertifiedReplay(displacementLine, displacementReplay)
    )
    val immediateRecord = EvidenceRecord(
      lineRef(s"immediate-source-$path", immediateLine, root),
      LineFactEvidence.fromCertifiedReplay(immediateLine, immediateReplay)
    )
    val displacementRoot = CertifiedRootOccurrenceTestFixture.from(
      displacementLine,
      displacementRecord,
      displacementReplay,
      CausalRootProvenance.CounterfactualAnalyzedRoot
    )
    val immediateCaptureRoot = CertifiedRootOccurrenceTestFixture.from(
      immediateLine,
      immediateRecord,
      immediateReplay,
      CausalRootProvenance.ObservedGameRoot
    )
    val demand = for
      trigger <- displacementReplay.replaySteps.headOption
      forcedReplyStep <- displacementReplay.replaySteps.lift(1)
      (checkOccurrence, forcedReply) <-
        displacementReplay.exactCheckResponseOccurrenceMembership(trigger, forcedReplyStep)
      delayedCaptureStep <- displacementReplay.replaySteps.lift(2)
      delayedCaptureOccurrence <- displacementReplay.verticalRelationOccurrences(
        delayedCaptureStep,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      ) match
        case exact :: Nil => Some(exact)
        case _            => None
      immediateCaptureStep <- immediateReplay.replaySteps.headOption
      immediateReplyStep <- immediateReplay.replaySteps.lift(1)
      (immediateCaptureOccurrence, immediateRecapture) <-
        immediateReplay.exactRecaptureOccurrenceMembership(immediateCaptureStep, immediateReplyStep)
    yield UniqueCheckReplyDefenderDisplacementBeforeCaptureDemand(
      checkOccurrence,
      forcedReply,
      delayedCaptureOccurrence,
      immediateCaptureOccurrence,
      immediateRecapture
    )
    demand.toList.flatMap(exactDemand =>
      UniqueCheckReplyDefenderDisplacementBeforeCaptureProof.certifyDemanded(
        immediateCaptureRoot,
        displacementRoot,
        immediateCaptureRoot,
        exactDemand
      )
    )

  private def dispatchForcedReply(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    RelationCausalProofTestDispatch.forced(context, allocator)

  private def lineRef(id: String, line: LineNodeRef, root: String): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = PositionNodeRef(root, 0, Some(White), Some("unique-check-reply-root")),
      line = Some(line),
      scope = EvidenceScope.LegalLine,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
