package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  ExplanationRequest,
  JudgmentProvenanceAllocator,
  OccurrenceExplanationAssembler,
  OccurrenceExplanationDemand,
  RawMoveReviewInput,
  RelativeAssessmentAssembler
}
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.{ EngineLine, PrincipalVariationEvidence }

class VacatedGateEnablesUnrecapturableSliderCaptureTest extends munit.FunSuite:

  private val rootFen = "7k/q7/8/8/8/8/N7/R6K w - - 0 1"
  private val vacatedGateMoves = List("a2b4", "h8g8", "a1a7")
  private val retainedGateMoves = List("h1h2", "h8g8")

  test("a vacated gate is consumed by the same slider's later exact capture"):
    val exact = deriveProofs(
      "positive",
      rootFen,
      certifiedReplay(rootFen, vacatedGateMoves),
      certifiedReplay(rootFen, retainedGateMoves)
    ) match
      case one :: Nil => one
      case other      =>
        fail(s"expected one vacated-gate-enables-unrecapturable-slider-capture proof, found ${other.size}")

    assertEquals(exact.semantic.enabler.stableKey, "white:knight>knight:a2-b4")
    assertEquals(exact.semantic.slider.square.key.toLowerCase, "a1")
    assertEquals(exact.semantic.slider.role.name.toLowerCase, "rook")
    assertEquals(exact.semantic.gateBlocker.square.key.toLowerCase, "a2")
    assertEquals(exact.semantic.exploit.stableKey, "white:rook>rook:a1-a7")
    assertEquals(exact.semantic.capturedTarget.square.key.toLowerCase, "a7")
    assertEquals(exact.semantic.capturedTarget.role.name.toLowerCase, "queen")
    assertEquals(exact.occurrence.vacatedGateSteps.map(_.moveUci), vacatedGateMoves)
    assertEquals(exact.occurrence.retainedGateSteps.map(_.moveUci), retainedGateMoves)

    val path = exact.occurrence.proofPaths match
      case one :: Nil => one
      case other      => fail(s"expected one physical line-access path, found ${other.size}")
    assertEquals(
      path.premiseUses.map(_.role),
      List(
        VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.GateVacatingSliderReach,
        VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.LaterSliderCapture
      )
    )
    assertEquals(path.premiseUses.map(_.stepIndex), List(0, 2))
    assertEquals(
      path.closedAbsenceUses.map(_.binding.role),
      List(
        VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.LaterCaptureImmediateRecaptureAbsent,
        VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.RetainedGateExploitMoveAbsent,
        VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.RetainedGateReplacementCaptureAbsent
      )
    )
    assertEquals(
      path.closedAbsenceUses.map(_.binding.queryKey),
      List("legal-capture:black:a7", "legal-move-from-to:white:a1:a7", "legal-capture:white:a7")
    )
    assertEquals(
      path.closedStateUses.map(_.binding.role),
      List(
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.VacatedGateInterveningSliderReach,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.VacatedGateTargetPersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.VacatedGateTargetPersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateSliderPersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateTargetPersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateBlockerPersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateSliderPersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateTargetPersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateBlockerPersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateBlockedSliderReach
      )
    )
    assertEquals(path.closedStateUses.map(_.binding.afterStepIndex), List(1, 0, 1, 0, 0, 0, 1, 1, 1, 1))
    assert(path.closedStateUses.forall(_.binding.issuerOccurrenceId.matches("[0-9a-f]{64}")))
    assertEquals(
      path.closedStateUses.map(_.binding.queryKey).filter(_.startsWith("occupied-by:")),
      List(
        "occupied-by:black:queen@a7",
        "occupied-by:black:queen@a7",
        "occupied-by:white:rook@a1",
        "occupied-by:black:queen@a7",
        "occupied-by:white:knight@a2",
        "occupied-by:white:rook@a1",
        "occupied-by:black:queen@a7",
        "occupied-by:white:knight@a2"
      )
    )
    assert(path.closedStateUses.last.binding.queryKey.startsWith("slider-reach:white:rook@a1:"))
    assert(!path.closedStateUses.last.binding.queryKey.contains("a7:enemy"))

    val projected = BoundedCausalPublicProjection.verticalRelationPaths(exact.occurrence.proofPaths).head
    assertEquals(projected.pathOccurrenceId, path.pathOccurrenceId)
    assertEquals(projected.closedStateUses.map(_.useId), path.closedStateUses.map(_.useId))
    assertEquals(
      projected.closedStateUses.map(_.issuerOccurrenceId),
      path.closedStateUses.map(_.binding.issuerOccurrenceId)
    )
    assert(exact.semantic.semanticId.matches("[0-9a-f]{64}"))
    assert(exact.occurrence.occurrenceId.matches("[0-9a-f]{64}"))
    assert(exact.dependency.value.matches("[0-9a-f]{64}"))
    assert(exact.remainsCertified)
    assertEquals(
      exact.parentSources.map(_.id).toSet,
      Set(
        "vacated-gate-source-positive",
        "vacated-gate-source-positive:transition",
        "retained-gate-source-positive",
        "retained-gate-source-positive:transition"
      )
    )
    assertEquals(exact.parentSources.map(_.layer).toSet, Set(EvidenceLayer.Line, EvidenceLayer.MoveTransition))
    assertEquals(exact.lowerIssuerRecords.map(_.ref).sortBy(_.id), exact.parentSources)

  test("replacement recapture, changed reply, already-open line, and opened retained-gate sibling fail closed"):
    val replacementFen = "7k/q3r3/8/8/8/8/N7/R6K w - - 0 1"
    assertEquals(
      deriveProofs(
        "replacement-recapture",
        replacementFen,
        certifiedReplay(replacementFen, vacatedGateMoves),
        certifiedReplay(replacementFen, retainedGateMoves)
      ),
      Nil
    )

    val changedRetainedGate = List("h1h2", "h8g7")
    assertEquals(
      deriveProofs(
        "changed-reply",
        rootFen,
        certifiedReplay(rootFen, vacatedGateMoves),
        certifiedReplay(rootFen, changedRetainedGate),
        exactRetainedGateMoves = changedRetainedGate
      ),
      Nil
    )

    val alreadyOpenFen = "7k/q7/8/8/8/8/8/R6K w - - 0 1"
    val alreadyOpenVacatedGate = List("h1h2", "h8g8", "a1a7")
    val alreadyOpenRetainedGate = List("h1g2", "h8g8")
    assertEquals(
      deriveProofs(
        "already-open",
        alreadyOpenFen,
        certifiedReplay(alreadyOpenFen, alreadyOpenVacatedGate),
        certifiedReplay(alreadyOpenFen, alreadyOpenRetainedGate),
        exactVacatedGateMoves = alreadyOpenVacatedGate,
        exactRetainedGateMoves = alreadyOpenRetainedGate
      ),
      Nil
    )

    val retainedGateAlsoOpens = List("a2c3", "h8g8")
    assertEquals(
      deriveProofs(
        "retained-gate-opens",
        rootFen,
        certifiedReplay(rootFen, vacatedGateMoves),
        certifiedReplay(rootFen, retainedGateAlsoOpens),
        exactRetainedGateMoves = retainedGateAlsoOpens
      ),
      Nil
    )

  test("the retained-gate continuation certifies exact occupants and rejects false state and absence claims"):
    val replay = certifiedReplay(rootFen, retainedGateMoves)
    val occurrence = replay
      .positionAfter(replay.replaySteps.last)
      .getOrElse(fail("expected the retained-gate pre-exploit occurrence"))
    val scope = EvidenceScope.LegalLine
    val exactPieces = List(
      RelationColoredPieceWitness(EvidenceSquare("a1"), EvidencePieceRole("rook"), chess.White),
      RelationColoredPieceWitness(EvidenceSquare("a7"), EvidencePieceRole("queen"), chess.Black),
      RelationColoredPieceWitness(EvidenceSquare("a2"), EvidencePieceRole("knight"), chess.White)
    )
    exactPieces.foreach(piece =>
      assert(occurrence.closedState(
        PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece),
        scope
      ).nonEmpty)
    )
    val falseOccupants = List(
      RelationColoredPieceWitness(EvidenceSquare("a1"), EvidencePieceRole("queen"), chess.White),
      RelationColoredPieceWitness(EvidenceSquare("a7"), EvidencePieceRole("rook"), chess.Black),
      RelationColoredPieceWitness(EvidenceSquare("a2"), EvidencePieceRole("knight"), chess.Black)
    )
    falseOccupants.foreach(piece =>
      assertEquals(
        occurrence.closedState(
          PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece),
          scope
        ),
        None
      )
    )
    assertEquals(
      occurrence.closedAbsence(
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
          chess.Black,
          EvidenceSquare("g8"),
          EvidenceSquare("f7")
        ),
        scope
      ),
      None,
      "a White-to-move inventory must not certify a foreign-side legal-move absence"
    )

    val replacementFen = "7k/q3r3/8/8/8/8/N7/R6K w - - 0 1"
    val replacementReplay = certifiedReplay(replacementFen, vacatedGateMoves)
    val replacementAfterExploit = replacementReplay
      .positionAfter(replacementReplay.replaySteps.last)
      .getOrElse(fail("expected the replacement-recapture occurrence"))
    assertEquals(
      replacementAfterExploit.closedAbsence(
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
          chess.Black,
          EvidenceSquare("a7")
        ),
        EvidenceScope.BestLine
      ),
      None,
      "the e7 rook must defeat the vacated-gate no-recapture closure"
    )

    val openedMoves = List("a2c3", "h8g8")
    val openedReplay = certifiedReplay(rootFen, openedMoves)
    val openedOccurrence = openedReplay
      .positionAfter(openedReplay.replaySteps.last)
      .getOrElse(fail("expected the opened retained-gate occurrence"))
    assertEquals(
      openedOccurrence.closedAbsence(
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
          chess.White,
          EvidenceSquare("a1"),
          EvidenceSquare("a7")
        ),
        EvidenceScope.LegalLine
      ),
      None,
      "an opened sibling must defeat the exact from-to absence"
    )
    assertEquals(
      openedOccurrence.closedAbsence(
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
          chess.White,
          EvidenceSquare("a7")
        ),
        EvidenceScope.LegalLine
      ),
      None,
      "an opened sibling must defeat the target-capture absence"
    )

  test("semantic transpositions preserve distinct occurrence and path ownership"):
    val vacatedGateReplay = certifiedReplay(rootFen, vacatedGateMoves)
    val retainedGateReplay = certifiedReplay(rootFen, retainedGateMoves)
    val first = deriveProofs("route-a", rootFen, vacatedGateReplay, retainedGateReplay).head
    val second = deriveProofs("route-b", rootFen, vacatedGateReplay, retainedGateReplay).head

    assertEquals(first.semantic.semanticId, second.semantic.semanticId)
    assertNotEquals(first.occurrence.occurrenceId, second.occurrence.occurrenceId)
    assertNotEquals(
      first.occurrence.proofPaths.head.pathOccurrenceId,
      second.occurrence.proofPaths.head.pathOccurrenceId
    )
    assertNotEquals(first.dependency.value, second.dependency.value)

  test("the exploit occurrence stays bounded when its certified source PV continues"):
    val sourceMoves = vacatedGateMoves :+ "g8f8"
    val exact = deriveProofs(
      "trailing-source-pv",
      rootFen,
      certifiedReplay(rootFen, sourceMoves),
      certifiedReplay(rootFen, retainedGateMoves),
      exactVacatedGateMoves = sourceMoves
    ) match
      case one :: Nil => one
      case other      => fail(s"expected one bounded gate-release proof, found ${other.size}")

    assertEquals(exact.occurrence.vacatedGateSteps.map(_.moveUci), vacatedGateMoves)
    assertEquals(exact.occurrence.exploitStep.moveUci, "a1a7")
    assertEquals(exact.occurrence.proofPaths.head.premiseUses.last.stepIndex, 2)

  test("the exact gate-release proof reaches its occurrence Cause directly"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = retainedGateMoves.head,
          variations = List(
            EngineLine(vacatedGateMoves, scoreCp = 600, depth = 24),
            EngineLine(retainedGateMoves, scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected exact lower facts"))
    val facts = produceExplanations(RelativeAssessmentAssembler.enrichComparisonEvidence(lowerFacts))
    val causalRecords = facts.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: VacatedGateEnablesUnrecapturableSliderCaptureEvidence, _) => record
    }
    assertEquals(causalRecords.size, 1)
    val causalPayload = causalRecords.head.payload.asInstanceOf[VacatedGateEnablesUnrecapturableSliderCaptureEvidence]
    assert(causalPayload.hasCompleteProofPaths)
    assert(causalPayload.exactOccurrenceCertified(causalRecords.head))

    val withCauses = OccurrenceExplanationAssembler.enrichCauses(
      facts,
      JudgmentProvenanceAllocator.forInput(facts.input),
      exactDemand(facts)
    )
    val causes = withCauses.evidenceGraph.records.collect {
      case EvidenceRecord(_, OccurrenceExplanationCauseEvidence(cause), _)
          if cause.proofSource == causalRecords.head.ref => cause
    }
    assertEquals(causes.size, 1)
    assertEquals(causes.head.subject, causalPayload.subjectOccurrence)

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
      fen: String,
      vacatedGateReplay: CanonicalLineReplay,
      retainedGateReplay: CanonicalLineReplay,
      exactVacatedGateMoves: List[String] = vacatedGateMoves,
      exactRetainedGateMoves: List[String] = retainedGateMoves
  ): List[CertifiedVacatedGateEnablesUnrecapturableSliderCapture] =
    val vacatedGateLine = LineNodeRef(s"vacated-gate-$path", exactVacatedGateMoves.head)
    val retainedGateLine = LineNodeRef(s"retained-gate-$path", exactRetainedGateMoves.head)
    val vacatedGateRecord = EvidenceRecord(
      lineRef(s"vacated-gate-source-$path", vacatedGateLine, fen),
      LineFactEvidence.fromCertifiedReplay(vacatedGateLine, vacatedGateReplay)
    )
    val retainedGateRecord = EvidenceRecord(
      lineRef(s"retained-gate-source-$path", retainedGateLine, fen),
      LineFactEvidence.fromCertifiedReplay(retainedGateLine, retainedGateReplay)
    )
    val vacatedGateRoot = CertifiedRootOccurrenceTestFixture.from(
      vacatedGateLine,
      vacatedGateRecord,
      vacatedGateReplay,
      CausalRootProvenance.CounterfactualAnalyzedRoot
    )
    val retainedGateRoot = CertifiedRootOccurrenceTestFixture.from(
      retainedGateLine,
      retainedGateRecord,
      retainedGateReplay,
      CausalRootProvenance.ObservedGameRoot
    )
    val demands = for
      enablingStep <- vacatedGateReplay.replaySteps.headOption.toList
      rootReachOccurrence <- vacatedGateReplay.verticalRelationOccurrences(
        enablingStep,
        List(VerticalRelationContractKind.SliderReachDelta)
      )
      exploitStep <- vacatedGateReplay.replaySteps.lift(2).toList
      exploitOccurrence <- (vacatedGateReplay.verticalRelationOccurrences(
        exploitStep,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      ) match
        case exact :: Nil => List(exact)
        case _            => Nil)
    yield VacatedGateEnablesUnrecapturableSliderCaptureDemand(rootReachOccurrence, exploitOccurrence, 2)
    VacatedGateEnablesUnrecapturableSliderCaptureProof.certifyDemanded(
      retainedGateRoot,
      vacatedGateRoot,
      retainedGateRoot,
      demands.sortBy(_.stableKey)
    )

  private def lineRef(id: String, line: LineNodeRef, fen: String): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = PositionNodeRef(fen, 0, Some(White), Some("vacated-gate-root")),
      line = Some(line),
      scope = EvidenceScope.LegalLine,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
