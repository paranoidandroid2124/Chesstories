package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  ExplanationRequest,
  JudgmentProvenanceAllocator,
  OccurrenceExplanationAssembler,
  OccurrenceExplanationDemand,
  RawMoveReviewInput,
  RelationCausalProofTestDispatch,
  RelativeAssessmentAssembler
}
import lila.chessjudgment.model.line.{ EngineLine, PrincipalVariationEvidence }

class SoleRecapturerRemovalBeforeTargetCaptureTest extends munit.FunSuite:

  private val positiveFen = "8/4p2k/5n2/3r2B1/8/8/8/K2Q4 w - - 0 1"
  private val negativeFen = "6b1/4p2k/5n2/3r2B1/8/8/8/K2Q4 w - - 0 1"
  private val replacementNegativeFen = "4r3/4p2k/5n2/6B1/4b3/8/8/K3Q3 w - - 0 1"
  private val replacementRemovalMoves = List("g5f6", "e7f6", "e1e4")
  private val replacementImmediateMoves = List("e1e4", "f6e4")
  private val removalMoves = List("g5f6", "e7f6", "d1d5")
  private val immediateMoves = List("d1d5", "f6d5")

  test("a non-check removal captures the immediate branch's sole exact recapturer"):
    val removalReplay = certifiedReplay(positiveFen, removalMoves)
    val immediateReplay = certifiedReplay(positiveFen, immediateMoves)
    val removalStep = removalReplay.replaySteps.head
    assertEquals(
      removalReplay.verticalRelationOccurrences(
        removalStep,
        List(VerticalRelationContractKind.CreatedCheckResponseInventory)
      ),
      Nil,
      "the family must not depend on check-forcing promotion"
    )

    val exact = deriveProofs("positive", positiveFen, removalReplay, immediateReplay) match
      case one :: Nil => one
      case other      => fail(s"expected one sole-recapturer-removal proof, found ${other.size}")

    assertEquals(exact.semantic.remover.stableKey, "white:bishop>bishop:g5-f6")
    assertEquals(exact.semantic.removedDefender.side, chess.Black)
    assertEquals(exact.semantic.removedDefender.role.name.toLowerCase, "knight")
    assertEquals(exact.semantic.removedDefender.square.key.toLowerCase, "f6")
    assertEquals(exact.semantic.removalRecapture.moveUci, "e7f6")
    assertEquals(exact.semantic.exploit.stableKey, "white:queen>queen:d1-d5")
    assertEquals(exact.semantic.capturedTarget.role.name.toLowerCase, "rook")
    assertEquals(exact.semantic.capturedTarget.square.key.toLowerCase, "d5")
    assertEquals(exact.semantic.immediateRecapture.moveUci, "f6d5")
    assertEquals(exact.occurrence.removalSteps.map(_.moveUci), removalMoves)
    assertEquals(exact.occurrence.immediateSteps.map(_.moveUci), immediateMoves)

    val path = exact.occurrence.proofPaths match
      case one :: Nil => one
      case other      => fail(s"expected one independent proof path, found ${other.size}")
    assertEquals(
      path.premiseUses.map(_.role),
      List(
        SoleRecapturerRemovalBeforeTargetCapturePremiseRole.DefenderRemoval,
        SoleRecapturerRemovalBeforeTargetCapturePremiseRole.PostRemovalTargetCaptureInventory,
        SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ImmediateTargetCaptureInventory
      )
    )
    assertEquals(path.premiseUses.map(_.stepIndex), List(0, 2, 0))
    assert(path.premiseUses.forall(_.sourcePremiseIds.nonEmpty))
    assertEquals(
      path.closedStateUses.map(_.binding.role),
      List(
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalExploitActorPresent,
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalTargetPresent,
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalExploitActorPresent,
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalTargetPresent
      )
    )
    assertEquals(path.closedStateUses.map(_.binding.afterStepIndex), List(0, 0, 1, 1))
    assertEquals(
      path.closedStateUses.map(_.binding.queryKey),
      List(
        "occupied-by:white:queen@d1",
        "occupied-by:black:rook@d5",
        "occupied-by:white:queen@d1",
        "occupied-by:black:rook@d5"
      )
    )
    val absence = path.closedAbsenceUses match
      case one :: Nil => one.binding
      case other      => fail(s"expected one exact closed absence, found ${other.size}")
    assertEquals(absence.queryKey, "legal-capture:black:d5")
    assertEquals(absence.afterStepIndex, 2)
    assertEquals(absence.branchId, exact.occurrence.removalBranch.branchId)
    assertEquals(absence.issuerEvidenceId, "removal-source-positive")
    assert(absence.issuerOccurrenceId.matches("[0-9a-f]{64}"))
    assert(exact.semantic.semanticId.matches("[0-9a-f]{64}"))
    assert(exact.occurrence.occurrenceId.matches("[0-9a-f]{64}"))
    assert(exact.dependency.value.matches("[0-9a-f]{64}"))
    assert(exact.remainsCertified)
    assertEquals(
      exact.parentSources.map(_.id).toSet,
      Set(
        "removal-source-positive",
        "removal-source-positive:transition",
        "immediate-source-positive",
        "immediate-source-positive:transition"
      )
    )
    assertEquals(exact.parentSources.map(_.layer).toSet, Set(EvidenceLayer.Line, EvidenceLayer.MoveTransition))
    assertEquals(exact.lowerIssuerRecords.map(_.ref).sortBy(_.id), exact.parentSources)

  test("missing or query-tampered intermediate occupancy cannot form the sole-recapturer manifest"):
    val exact = deriveProofs(
      "state-adversary",
      positiveFen,
      certifiedReplay(positiveFen, removalMoves),
      certifiedReplay(positiveFen, immediateMoves)
    ).headOption.getOrElse(fail("expected the exact sole-recapturer proof"))
    val path = exact.occurrence.proofPaths.head
    val states = path.manifest.stateBindings
    val exploitActor = RelationColoredPieceWitness(
      exact.semantic.exploit.from,
      exact.semantic.exploit.beforeRole,
      exact.semantic.exploit.side
    )
    def rebuild(candidateStates: List[CausalClosedStateBinding]) =
      SoleRecapturerRemovalBeforeTargetCaptureManifest.exact(
        path.premiseUses(0),
        path.premiseUses(1),
        path.premiseUses(2),
        path.closedAbsenceUses.head.binding,
        exploitActor,
        exact.semantic.capturedTarget,
        candidateStates
      )

    intercept[IllegalArgumentException] {
      rebuild(states.dropRight(1))
    }
    val actorAtRoot = states.head
    val targetRoleWithActorState = CausalClosedStateBinding.afterStep(
      SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalTargetPresent,
      actorAtRoot.authority,
      exact.occurrence.removalBranch,
      actorAtRoot.afterStepIndex
    )
    intercept[IllegalArgumentException] {
      rebuild(states.updated(1, targetRoleWithActorState))
    }

  test("the specified bishop alternative prevents a sole-recapturer certificate"):
    val removalReplay = certifiedReplay(negativeFen, removalMoves)
    val exploitStep = removalReplay.replaySteps(2)
    val inventory = removalReplay.verticalRelationOccurrences(
      exploitStep,
      List(VerticalRelationContractKind.CaptureRecaptureInventory)
    ) match
      case one :: Nil => one.relation.detail.asInstanceOf[RelationWitnessDetail.CaptureRecaptureInventory]
      case other      => fail(s"expected one post-removal capture inventory, found ${other.size}")
    assertEquals(inventory.legalRecaptures.map(_.moveUci), List("g8d5"))

    assertEquals(
      deriveProofs(
        "alternative-recapture",
        negativeFen,
        removalReplay,
        certifiedReplay(negativeFen, immediateMoves)
      ),
      Nil
    )

  test("a replacement rook recapture fails only the post-removal closed absence"):
    val removalReplay = certifiedReplay(replacementNegativeFen, replacementRemovalMoves)
    val immediateReplay = certifiedReplay(replacementNegativeFen, replacementImmediateMoves)
    val immediateInventory = captureInventory(immediateReplay, immediateReplay.replaySteps.head)
    val postRemovalInventory = captureInventory(removalReplay, removalReplay.replaySteps(2))
    assertEquals(immediateInventory.legalRecaptures.map(_.moveUci), List("f6e4"))
    assertEquals(postRemovalInventory.legalRecaptures.map(_.moveUci), List("e8e4"))

    assertEquals(
      deriveProofs(
        "replacement-recapture",
        replacementNegativeFen,
        removalReplay,
        immediateReplay,
        replacementRemovalMoves,
        replacementImmediateMoves
      ),
      Nil
    )

  test("assessment owner changes leave the occurrence proof identity unchanged"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = positiveFen,
          playedMoveUci = immediateMoves.head,
          variations = List(
            EngineLine(removalMoves, scoreCp = 600, depth = 24),
            EngineLine(immediateMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected exact lower facts"))
    val assessed = RelativeAssessmentAssembler.enrichComparisonEvidence(lowerFacts)
    val context = produceExplanations(assessed)
    val assessmentRecord = context.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => record
    }.getOrElse(fail("expected the exact PlayedVsBest Assessment record"))

    val first = dispatchDefense(context) match
      case one :: Nil => one
      case other      => fail(s"expected one demanded proof, found ${other.size}")
    val changedAssessmentRecord = assessmentRecord.copy(
      ref = assessmentRecord.ref.copy(id = s"${assessmentRecord.ref.id}:changed-owner")
    )
    val second = dispatchDefense(context.withEvidence(changedAssessmentRecord)) match
      case one :: Nil => one
      case other      => fail(s"expected one proof after an Assessment-owner change, found ${other.size}")

    assertEquals(first.semantic.semanticId, second.semantic.semanticId)
    assertEquals(first.occurrence.occurrenceId, second.occurrence.occurrenceId)
    assertEquals(first.dependency.value, second.dependency.value)
    assert(first.consumesDependencies(
      context.certifiedRootOccurrence(first.occurrence.subjectOccurrence.line).get,
      context.certifiedRootOccurrence(first.occurrence.removalLine).get,
      context.certifiedRootOccurrence(first.occurrence.immediateCaptureLine).get
    ))

  test("the certified family reaches one occurrence Cause directly"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = positiveFen,
          playedMoveUci = immediateMoves.head,
          variations = List(
            EngineLine(removalMoves, scoreCp = 600, depth = 24),
            EngineLine(immediateMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected exact lower facts"))
    val facts = produceExplanations(RelativeAssessmentAssembler.enrichComparisonEvidence(lowerFacts))
    val proofs = facts.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: SoleRecapturerRemovalBeforeTargetCaptureEvidence, _) => record
    }
    assertEquals(proofs.size, 1)
    val proofRecord = proofs.head
    val proofPayload = proofRecord.payload.asInstanceOf[SoleRecapturerRemovalBeforeTargetCaptureEvidence]
    assert(proofPayload.exactOccurrenceCertified(proofRecord))
    assert(!proofPayload.exactOccurrenceCertified(
      proofRecord.copy(ref = proofRecord.ref.copy(scope = EvidenceScope.Counterfactual))
    ))
    assert(!proofPayload.exactOccurrenceCertified(
      proofRecord.copy(ref = proofRecord.ref.copy(line = None))
    ))

    val withCauses = OccurrenceExplanationAssembler.enrichCauses(
      facts,
      JudgmentProvenanceAllocator.forInput(facts.input),
      exactDemand(facts)
    )
    val causes = withCauses.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, OccurrenceExplanationCauseEvidence(cause), _)
          if cause.proofSource == proofRecord.ref => record
    }
    assertEquals(causes.size, 1)

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

  private def certifiedReplay(fen: String, moves: List[String]): CanonicalLineReplay =
    PrincipalVariationEvidence
      .legalMoveReplay(fen, moves, startPly = 0)
      .flatMap(CanonicalLineReplay.fromLegalReplay)
      .getOrElse(fail(s"expected legal replay for ${moves.mkString(" ")}"))

  private def deriveProofs(
      path: String,
      rootFen: String,
      removalReplay: CanonicalLineReplay,
      immediateReplay: CanonicalLineReplay,
      exactRemovalMoves: List[String] = removalMoves,
      exactImmediateMoves: List[String] = immediateMoves
  ): List[CertifiedSoleRecapturerRemovalBeforeTargetCapture] =
    val removalLine = LineNodeRef(s"removal-$path", exactRemovalMoves.head)
    val immediateLine = LineNodeRef(s"immediate-$path", exactImmediateMoves.head)
    val removalRecord = EvidenceRecord(
      lineRef(s"removal-source-$path", removalLine, rootFen),
      LineFactEvidence.fromCertifiedReplay(removalLine, removalReplay)
    )
    val immediateRecord = EvidenceRecord(
      lineRef(s"immediate-source-$path", immediateLine, rootFen),
      LineFactEvidence.fromCertifiedReplay(immediateLine, immediateReplay)
    )
    val removalRoot = CertifiedRootOccurrenceTestFixture.from(
      removalLine,
      removalRecord,
      removalReplay,
      CausalRootProvenance.CounterfactualAnalyzedRoot
    )
    val immediateCaptureRoot = CertifiedRootOccurrenceTestFixture.from(
      immediateLine,
      immediateRecord,
      immediateReplay,
      CausalRootProvenance.ObservedGameRoot
    )
    val demand = for
      removalStep <- removalReplay.replaySteps.headOption
      removalReplyStep <- removalReplay.replaySteps.lift(1)
      (removalOccurrence, removalRecapture) <-
        removalReplay.exactRecaptureOccurrenceMembership(removalStep, removalReplyStep)
      postRemovalCaptureStep <- removalReplay.replaySteps.lift(2)
      postRemovalCaptureOccurrence <- removalReplay.verticalRelationOccurrences(
        postRemovalCaptureStep,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      ) match
        case exact :: Nil => Some(exact)
        case _            => None
      immediateCaptureStep <- immediateReplay.replaySteps.headOption
      immediateReplyStep <- immediateReplay.replaySteps.lift(1)
      (immediateCaptureOccurrence, immediateRecapture) <-
        immediateReplay.exactRecaptureOccurrenceMembership(immediateCaptureStep, immediateReplyStep)
    yield SoleRecapturerRemovalBeforeTargetCaptureDemand(
      removalOccurrence,
      removalRecapture,
      postRemovalCaptureOccurrence,
      immediateCaptureOccurrence,
      immediateRecapture
    )
    demand.toList.flatMap(exactDemand =>
      SoleRecapturerRemovalBeforeTargetCaptureProof.certifyDemanded(
        immediateCaptureRoot,
        removalRoot,
        immediateCaptureRoot,
        exactDemand
      )
    )

  private def dispatchDefense(
      context: JudgmentAssemblyContext
  ): List[CertifiedSoleRecapturerRemovalBeforeTargetCapture] =
    RelationCausalProofTestDispatch.defense(context)

  private def captureInventory(
      replay: CanonicalLineReplay,
      step: LineReplayStep
  ): RelationWitnessDetail.CaptureRecaptureInventory =
    replay.verticalRelationOccurrences(
      step,
      List(VerticalRelationContractKind.CaptureRecaptureInventory)
    ) match
      case one :: Nil => one.relation.detail.asInstanceOf[RelationWitnessDetail.CaptureRecaptureInventory]
      case other      => fail(s"expected one capture inventory, found ${other.size}")

  private def lineRef(id: String, line: LineNodeRef, rootFen: String): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = PositionNodeRef(rootFen, 0, Some(White), Some("sole-recapturer-removal-root")),
      line = Some(line),
      scope = EvidenceScope.LegalLine,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
