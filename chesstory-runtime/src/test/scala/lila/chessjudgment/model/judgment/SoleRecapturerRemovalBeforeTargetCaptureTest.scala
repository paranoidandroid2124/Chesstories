package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  RawMoveReviewInput,
  RelationCausalProofTestDispatch,
  RelativeAssessmentAssembler,
  RelativeCauseSignalProfile
}
import lila.chessjudgment.model.line.{ EngineLine, PrincipalVariationEvidence }

class SoleRecapturerRemovalBeforeTargetCaptureTest extends munit.FunSuite:

  private val positiveFen = "8/4p2k/5n2/3r2B1/8/8/8/K2Q4 w - - 0 1"
  private val negativeFen = "6b1/4p2k/5n2/3r2B1/8/8/8/K2Q4 w - - 0 1"
  private val replacementNegativeFen = "4r3/4p2k/5n2/6B1/4b3/8/8/K3Q3 w - - 0 1"
  private val replacementReferenceMoves = List("g5f6", "e7f6", "e1e4")
  private val replacementPlayedMoves = List("e1e4", "f6e4")
  private val referenceMoves = List("g5f6", "e7f6", "d1d5")
  private val playedMoves = List("d1d5", "f6d5")

  test("a non-check reference capture removes the played branch's sole exact recapturer"):
    val referenceReplay = certifiedReplay(positiveFen, referenceMoves)
    val playedReplay = certifiedReplay(positiveFen, playedMoves)
    val removalStep = referenceReplay.replaySteps.head
    assertEquals(
      referenceReplay.verticalRelationOccurrences(
        removalStep,
        List(VerticalRelationContractKind.CreatedCheckResponseInventory)
      ),
      Nil,
      "the family must not depend on check-forcing promotion"
    )

    val exact = deriveProofs("positive", positiveFen, referenceReplay, playedReplay) match
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
    assertEquals(exact.semantic.playedRecapture.moveUci, "f6d5")
    assertEquals(exact.occurrence.referenceSteps.map(_.moveUci), referenceMoves)
    assertEquals(exact.occurrence.playedSteps.map(_.moveUci), playedMoves)

    val path = exact.occurrence.proofPaths match
      case one :: Nil => one
      case other      => fail(s"expected one independent proof path, found ${other.size}")
    assertEquals(
      path.premiseUses.map(_.role),
      List(
        SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ReferenceDefenderRemoval,
        SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ReferenceLaterExploitInventory,
        SoleRecapturerRemovalBeforeTargetCapturePremiseRole.PlayedImmediateExploitInventory
      )
    )
    assertEquals(path.premiseUses.map(_.stepIndex), List(0, 2, 0))
    assert(path.premiseUses.forall(_.sourcePremiseIds.nonEmpty))
    assertEquals(
      path.closedStateUses.map(_.binding.role),
      List(
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceExploitActorPresent,
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceTargetPresent,
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceExploitActorPresent,
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceTargetPresent
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
    assertEquals(absence.branchId, exact.occurrence.referenceBranch.branchId)
    assertEquals(absence.issuerEvidenceId, "reference-source-positive")
    assert(absence.issuerOccurrenceId.matches("[0-9a-f]{64}"))
    assert(exact.semantic.semanticId.matches("[0-9a-f]{64}"))
    assert(exact.occurrence.occurrenceId.matches("[0-9a-f]{64}"))
    assert(exact.dependency.value.matches("[0-9a-f]{64}"))
    assert(exact.remainsCertified)
    assertEquals(
      exact.parentSources.map(_.id).toSet,
      Set("reference-source-positive", "played-source-positive")
    )
    assert(exact.parentSources.forall(_.layer == EvidenceLayer.Line))
    assert(exact.lowerIssuerRecords.forall(_.ref.layer == EvidenceLayer.Line))

  test("missing or query-tampered intermediate occupancy cannot form the sole-recapturer manifest"):
    val exact = deriveProofs(
      "state-adversary",
      positiveFen,
      certifiedReplay(positiveFen, referenceMoves),
      certifiedReplay(positiveFen, playedMoves)
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
      SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceTargetPresent,
      actorAtRoot.authority,
      exact.occurrence.referenceBranch,
      actorAtRoot.afterStepIndex
    )
    intercept[IllegalArgumentException] {
      rebuild(states.updated(1, targetRoleWithActorState))
    }

  test("the specified bishop alternative prevents a sole-recapturer certificate"):
    val referenceReplay = certifiedReplay(negativeFen, referenceMoves)
    val exploitStep = referenceReplay.replaySteps(2)
    val inventory = referenceReplay.verticalRelationOccurrences(
      exploitStep,
      List(VerticalRelationContractKind.CaptureRecaptureInventory)
    ) match
      case one :: Nil => one.relation.detail.asInstanceOf[RelationWitnessDetail.CaptureRecaptureInventory]
      case other      => fail(s"expected one reference capture inventory, found ${other.size}")
    assertEquals(inventory.legalRecaptures.map(_.moveUci), List("g8d5"))

    assertEquals(
      deriveProofs(
        "alternative-recapture",
        negativeFen,
        referenceReplay,
        certifiedReplay(negativeFen, playedMoves)
      ),
      Nil
    )

  test("a replacement rook recapture fails only the reference closed absence"):
    val referenceReplay = certifiedReplay(replacementNegativeFen, replacementReferenceMoves)
    val playedReplay = certifiedReplay(replacementNegativeFen, replacementPlayedMoves)
    val playedInventory = captureInventory(playedReplay, playedReplay.replaySteps.head)
    val referenceInventory = captureInventory(referenceReplay, referenceReplay.replaySteps(2))
    assertEquals(playedInventory.legalRecaptures.map(_.moveUci), List("f6e4"))
    assertEquals(referenceInventory.legalRecaptures.map(_.moveUci), List("e8e4"))

    assertEquals(
      deriveProofs(
        "replacement-recapture",
        replacementNegativeFen,
        referenceReplay,
        playedReplay,
        replacementReferenceMoves,
        replacementPlayedMoves
      ),
      Nil
    )

  test("evaluation-only demand ownership leaves the structural proof identity unchanged"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = positiveFen,
          playedMoveUci = playedMoves.head,
          variations = List(
            EngineLine(referenceMoves, scoreCp = 600, depth = 24),
            EngineLine(playedMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected exact lower facts"))
    val context = RelativeAssessmentAssembler.enrichFacts(lowerFacts)
    val demand = context.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => record
    }.getOrElse(fail("expected the exact PlayedVsBest demand"))

    val first = dispatchDefense(context, demand) match
      case one :: Nil => one
      case other      => fail(s"expected one demanded proof, found ${other.size}")
    val changedDemand = demand.copy(ref = demand.ref.copy(id = s"${demand.ref.id}:changed-owner"))
    val second = dispatchDefense(
      context.withEvidence(changedDemand),
      changedDemand
    ) match
      case one :: Nil => one
      case other      => fail(s"expected one changed-demand proof, found ${other.size}")

    assertEquals(first.semantic.semanticId, second.semantic.semanticId)
    assertEquals(first.occurrence.occurrenceId, second.occurrence.occurrenceId)
    assertEquals(first.dependency.value, second.dependency.value)
    assert(first.consumesDependencies(
      context.evidenceGraph.uniqueProofEligibleLineFactRecordFor(first.occurrence.referenceLine).get._1,
      context.evidenceGraph.uniqueProofEligibleLineFactRecordFor(first.occurrence.playedLine).get._1
    ))

  test("forged comparison confidence, scope, or ancestry cannot open an L2 demand"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = positiveFen,
          playedMoveUci = playedMoves.head,
          variations = List(
            EngineLine(referenceMoves, scoreCp = 600, depth = 24),
            EngineLine(playedMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected exact lower facts"))
    val context = RelativeAssessmentAssembler.enrichFacts(lowerFacts)
    val demand = context.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => record
    }.getOrElse(fail("expected the exact PlayedVsBest demand"))
    val fact = demand.payload.asInstanceOf[CandidateComparisonEvidence].comparison
    val referenceSource = context.evidenceGraph
      .uniqueProofEligibleLineFactRecordFor(fact.referenceLine)
      .map(_._1)
      .getOrElse(fail("missing exact reference line source"))
    val playedSource = context.evidenceGraph
      .uniqueProofEligibleLineFactRecordFor(fact.candidateLine)
      .map(_._1)
      .getOrElse(fail("missing exact played line source"))
    val root = referenceSource.ref.position
    assert(context.evidenceGraph.proofEligible(demand))
    assert(ActionablePlayedVsBestCausalProofDemand.acceptsRecord(
      demand,
      fact,
      root,
      referenceSource,
      playedSource
    ))

    val forged = List(
      demand.copy(
        ref = demand.ref.copy(id = s"${demand.ref.id}:wrong-confidence", confidence = EvidenceConfidence.BoardDerived)
      ),
      demand.copy(
        ref = demand.ref.copy(id = s"${demand.ref.id}:wrong-scope", scope = EvidenceScope.CurrentPosition)
      ),
      demand.copy(
        ref = demand.ref.copy(id = s"${demand.ref.id}:missing-parent"),
        parents = demand.parents.filterNot(_.layer == EvidenceLayer.Eval)
      )
    )
    forged.foreach { carrier =>
      assert(!context.withEvidence(carrier).evidenceGraph.proofEligible(carrier))
      assert(!ActionablePlayedVsBestCausalProofDemand.acceptsRecord(
        carrier,
        fact,
        root,
        referenceSource,
        playedSource
      ))
    }

  test("the certified family reaches the exact WrongMoveOrder consumer"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = positiveFen,
          playedMoveUci = playedMoves.head,
          variations = List(
            EngineLine(referenceMoves, scoreCp = 600, depth = 24),
            EngineLine(playedMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected exact lower facts"))
    val facts = RelativeAssessmentAssembler.enrichFacts(lowerFacts)
    val demand = facts.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => record
    }.getOrElse(fail("expected the exact PlayedVsBest demand"))
    val comparison = demand.payload.asInstanceOf[CandidateComparisonEvidence].comparison
    val proofs = RelativeCauseSignalProfile.referenceDirectCausalProofRecords(
      facts.evidenceGraph,
      facts.evidenceGraph.recordsFor(comparison.referenceLine),
      comparison,
      demand
    )
    assertEquals(proofs.size, 1)
    val proofRecord = proofs.head
    val proofPayload = proofRecord.payload.asInstanceOf[SoleRecapturerRemovalBeforeTargetCaptureEvidence]
    assert(proofPayload.exactOccurrenceCertified(proofRecord))
    assert(!proofPayload.exactOccurrenceCertified(
      proofRecord.copy(ref = proofRecord.ref.copy(scope = EvidenceScope.Counterfactual))
    ))
    assert(!proofPayload.exactOccurrenceCertified(
      proofRecord.copy(ref = proofRecord.ref.copy(line = Some(comparison.candidateLine)))
    ))

    val withCauses = RelativeAssessmentAssembler.enrichCauses(facts)
    val causes = withCauses.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder => record
    }
    assertEquals(causes.size, 1)

  private def certifiedReplay(fen: String, moves: List[String]): CanonicalLineReplay =
    PrincipalVariationEvidence
      .legalMoveReplay(fen, moves, startPly = 0)
      .flatMap(CanonicalLineReplay.fromLegalReplay)
      .getOrElse(fail(s"expected legal replay for ${moves.mkString(" ")}"))

  private def deriveProofs(
      path: String,
      rootFen: String,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      exactReferenceMoves: List[String] = referenceMoves,
      exactPlayedMoves: List[String] = playedMoves
  ): List[CertifiedSoleRecapturerRemovalBeforeTargetCapture] =
    val referenceLine = LineNodeRef(
      s"reference-$path",
      exactReferenceMoves.head,
      1,
      LineNodeRole.BestReference
    )
    val playedLine = LineNodeRef(s"played-$path", exactPlayedMoves.head, 1, LineNodeRole.Played)
    val referenceRecord = EvidenceRecord(
      lineRef(s"reference-source-$path", referenceLine, rootFen),
      LineFactEvidence.fromCertifiedReplay(referenceLine, referenceReplay)
    )
    val playedRecord = EvidenceRecord(
      lineRef(s"played-source-$path", playedLine, rootFen),
      LineFactEvidence.fromCertifiedReplay(playedLine, playedReplay)
    )
    val demand = for
      removalStep <- referenceReplay.replaySteps.headOption
      removalReplyStep <- referenceReplay.replaySteps.lift(1)
      (removalOccurrence, removalRecapture) <-
        referenceReplay.exactRecaptureOccurrenceMembership(removalStep, removalReplyStep)
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
    yield SoleRecapturerRemovalBeforeTargetCaptureDemand(
      removalOccurrence,
      removalRecapture,
      referenceExploitOccurrence,
      playedExploitOccurrence,
      playedRecapture
    )
    demand.toList.flatMap(exactDemand =>
      SoleRecapturerRemovalBeforeTargetCaptureProof.certifyDemanded(
        referenceLine,
        playedLine,
        referenceRecord,
        playedRecord,
        referenceReplay,
        playedReplay,
        exactDemand
      )
    )

  private def dispatchDefense(
      context: JudgmentAssemblyContext,
      demandSource: EvidenceRecord
  ): List[CertifiedSoleRecapturerRemovalBeforeTargetCapture] =
    RelationCausalProofTestDispatch.defense(context, demandSource)

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
      scope = line.role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
