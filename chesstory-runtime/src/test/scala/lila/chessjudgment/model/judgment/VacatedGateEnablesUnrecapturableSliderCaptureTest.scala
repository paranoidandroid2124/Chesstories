package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  RawMoveReviewInput,
  RelativeAssessmentAssembler,
  RelativeCauseSignalProfile
}
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.{ EngineLine, PrincipalVariationEvidence }

class VacatedGateEnablesUnrecapturableSliderCaptureTest extends munit.FunSuite:

  private val rootFen = "7k/q7/8/8/8/8/N7/R6K w - - 0 1"
  private val referenceMoves = List("a2b4", "h8g8", "a1a7")
  private val playedMoves = List("h1h2", "h8g8")

  test("a vacated gate is consumed by the same slider's later exact capture"):
    val exact = deriveProofs(
      "positive",
      rootFen,
      certifiedReplay(rootFen, referenceMoves),
      certifiedReplay(rootFen, playedMoves)
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
    assertEquals(exact.occurrence.referenceSteps.map(_.moveUci), referenceMoves)
    assertEquals(exact.occurrence.playedSteps.map(_.moveUci), playedMoves)

    val path = exact.occurrence.proofPaths match
      case one :: Nil => one
      case other      => fail(s"expected one physical line-access path, found ${other.size}")
    assertEquals(
      path.premiseUses.map(_.role),
      List(
        VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.ReferenceRootSliderReach,
        VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.ReferenceExploitCapture
      )
    )
    assertEquals(path.premiseUses.map(_.stepIndex), List(0, 2))
    assertEquals(
      path.closedAbsenceUses.map(_.binding.role),
      List(
        VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.ReferenceImmediateRecaptureAbsent,
        VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.PlayedExploitMoveAbsent,
        VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.PlayedReplacementCaptureAbsent
      )
    )
    assertEquals(
      path.closedAbsenceUses.map(_.binding.queryKey),
      List("legal-capture:black:a7", "legal-move-from-to:white:a1:a7", "legal-capture:white:a7")
    )
    assertEquals(
      path.closedStateUses.map(_.binding.role),
      List(
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.ReferenceInterveningSliderReach,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedSliderOccupied,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedTargetOccupied,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedGateBlockerOccupied,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedBlockedSliderReach
      )
    )
    assertEquals(path.closedStateUses.map(_.binding.afterStepIndex), List(1, 1, 1, 1, 1))
    assert(path.closedStateUses.forall(_.binding.issuerOccurrenceId.matches("[0-9a-f]{64}")))
    assertEquals(
      path.closedStateUses.map(_.binding.queryKey).filter(_.startsWith("occupied-by:")),
      List("occupied-by:white:rook@a1", "occupied-by:black:queen@a7", "occupied-by:white:knight@a2")
    )
    assert(path.closedStateUses.last.binding.queryKey.startsWith("slider-reach:white:rook@a1:"))
    assert(!path.closedStateUses.last.binding.queryKey.contains("a7:enemy"))

    val projected = BoundedCausalPublicProjection.paths(exact.occurrence.proofPaths).head
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
      Set("reference-source-positive", "played-source-positive")
    )
    assert(exact.parentSources.forall(_.layer == EvidenceLayer.Line))
    assert(exact.lowerIssuerRecords.forall(_.ref.layer == EvidenceLayer.Line))

  test("replacement recapture, changed reply, already-open line, and opened Played sibling fail closed"):
    val replacementFen = "7k/q3r3/8/8/8/8/N7/R6K w - - 0 1"
    assertEquals(
      deriveProofs(
        "replacement-recapture",
        replacementFen,
        certifiedReplay(replacementFen, referenceMoves),
        certifiedReplay(replacementFen, playedMoves)
      ),
      Nil
    )

    val changedPlayed = List("h1h2", "h8g7")
    assertEquals(
      deriveProofs(
        "changed-reply",
        rootFen,
        certifiedReplay(rootFen, referenceMoves),
        certifiedReplay(rootFen, changedPlayed),
        exactPlayedMoves = changedPlayed
      ),
      Nil
    )

    val alreadyOpenFen = "7k/q7/8/8/8/8/8/R6K w - - 0 1"
    val alreadyOpenReference = List("h1h2", "h8g8", "a1a7")
    val alreadyOpenPlayed = List("h1g2", "h8g8")
    assertEquals(
      deriveProofs(
        "already-open",
        alreadyOpenFen,
        certifiedReplay(alreadyOpenFen, alreadyOpenReference),
        certifiedReplay(alreadyOpenFen, alreadyOpenPlayed),
        exactReferenceMoves = alreadyOpenReference,
        exactPlayedMoves = alreadyOpenPlayed
      ),
      Nil
    )

    val playedAlsoOpens = List("a2c3", "h8g8")
    assertEquals(
      deriveProofs(
        "played-opens",
        rootFen,
        certifiedReplay(rootFen, referenceMoves),
        certifiedReplay(rootFen, playedAlsoOpens),
        exactPlayedMoves = playedAlsoOpens
      ),
      Nil
    )

  test("the played-root analysis continuation certifies exact occupants and rejects false state and absence claims"):
    val replay = certifiedReplay(rootFen, playedMoves)
    val occurrence = replay
      .positionAfter(replay.replaySteps.last)
      .getOrElse(fail("expected the played-root analysis-continuation pre-exploit occurrence"))
    val scope = EvidenceScope.PlayedLine
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
    val replacementReplay = certifiedReplay(replacementFen, referenceMoves)
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
      "the e7 rook must defeat the reference no-recapture closure"
    )

    val openedMoves = List("a2c3", "h8g8")
    val openedReplay = certifiedReplay(rootFen, openedMoves)
    val openedOccurrence = openedReplay
      .positionAfter(openedReplay.replaySteps.last)
      .getOrElse(fail("expected the opened played-root analysis-continuation occurrence"))
    assertEquals(
      openedOccurrence.closedAbsence(
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
          chess.White,
          EvidenceSquare("a1"),
          EvidenceSquare("a7")
        ),
        EvidenceScope.PlayedLine
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
        EvidenceScope.PlayedLine
      ),
      None,
      "an opened sibling must defeat the target-capture absence"
    )

  test("semantic transpositions preserve distinct occurrence and path ownership"):
    val referenceReplay = certifiedReplay(rootFen, referenceMoves)
    val playedReplay = certifiedReplay(rootFen, playedMoves)
    val first = deriveProofs("route-a", rootFen, referenceReplay, playedReplay).head
    val second = deriveProofs("route-b", rootFen, referenceReplay, playedReplay).head

    assertEquals(first.semantic.semanticId, second.semantic.semanticId)
    assertNotEquals(first.occurrence.occurrenceId, second.occurrence.occurrenceId)
    assertNotEquals(
      first.occurrence.proofPaths.head.pathOccurrenceId,
      second.occurrence.proofPaths.head.pathOccurrenceId
    )
    assertNotEquals(first.dependency.value, second.dependency.value)

  test("the exploit occurrence stays bounded when its certified source PV continues"):
    val sourceMoves = referenceMoves :+ "g8f8"
    val exact = deriveProofs(
      "trailing-source-pv",
      rootFen,
      certifiedReplay(rootFen, sourceMoves),
      certifiedReplay(rootFen, playedMoves),
      exactReferenceMoves = sourceMoves
    ) match
      case one :: Nil => one
      case other      => fail(s"expected one bounded preparation, found ${other.size}")

    assertEquals(exact.occurrence.referenceSteps.map(_.moveUci), referenceMoves)
    assertEquals(exact.occurrence.exploitStep.moveUci, "a1a7")
    assertEquals(exact.occurrence.proofPaths.head.premiseUses.last.stepIndex, 2)

  test("the exact preparation reaches its own MissedTacticalResource proof consumer"):
    val lowerFacts = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = rootFen,
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
    val causalRecords = RelativeCauseSignalProfile.vacatedGateEnablesUnrecapturableSliderCaptureRecords(
      facts.evidenceGraph,
      facts.evidenceGraph.recordsFor(comparison.referenceLine),
      comparison,
      demand
    )
    assertEquals(causalRecords.size, 1)
    val causalPayload = causalRecords.head.payload.asInstanceOf[VacatedGateEnablesUnrecapturableSliderCaptureEvidence]
    assert(causalPayload.hasCompleteProofPaths)
    assert(causalPayload.exactOccurrenceCertified(causalRecords.head))

    val withCauses = RelativeAssessmentAssembler.enrichCauses(facts)
    val allMissedCauses = withCauses.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.MissedTacticalResource => cause
    }
    assertEquals(allMissedCauses.size, 1)
    val exactCauses = allMissedCauses.filter(
      _.proofSources.exists(_.id == causalRecords.head.ref.id)
    )
    assertEquals(exactCauses.size, 1)
    val unrelatedGenericMissed = withCauses.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.MissedTacticalResource &&
            !cause.proofSources.exists(_.id == causalRecords.head.ref.id) => cause
    }
    assertEquals(unrelatedGenericMissed, Nil)
    val channels = DirectCauseChannel.certifiedForCause(
      exactCauses.head,
      withCauses.evidenceGraph
    )
    assert(channels.nonEmpty)
    assert(channels.forall(channel => channel.rootOwnedProof match
      case RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture(_, result) =>
        result.semanticId == causalPayload.semanticId && result.hasCompleteProofPaths
      case _ => false
    ))

  private def certifiedReplay(fen: String, moves: List[String]): CanonicalLineReplay =
    PrincipalVariationEvidence
      .legalMoveReplay(fen, moves, startPly = 0)
      .flatMap(CanonicalLineReplay.fromLegalReplay)
      .getOrElse(fail(s"expected legal replay for ${moves.mkString(" ")}"))

  private def deriveProofs(
      path: String,
      fen: String,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      exactReferenceMoves: List[String] = referenceMoves,
      exactPlayedMoves: List[String] = playedMoves
  ): List[CertifiedVacatedGateEnablesUnrecapturableSliderCapture] =
    val referenceLine = LineNodeRef(
      s"reference-$path",
      exactReferenceMoves.head,
      1,
      LineNodeRole.BestReference
    )
    val playedLine = LineNodeRef(s"played-$path", exactPlayedMoves.head, 1, LineNodeRole.Played)
    val referenceRecord = EvidenceRecord(
      lineRef(s"reference-source-$path", referenceLine, fen),
      LineFactEvidence.fromCertifiedReplay(referenceLine, referenceReplay)
    )
    val playedRecord = EvidenceRecord(
      lineRef(s"played-source-$path", playedLine, fen),
      LineFactEvidence.fromCertifiedReplay(playedLine, playedReplay)
    )
    val changedSeeds = for
      enablingStep <- referenceReplay.replaySteps.headOption.toList
      rootReachOccurrence <- referenceReplay.verticalRelationOccurrences(
        enablingStep,
        List(VerticalRelationContractKind.SliderReachDelta)
      )
      exploitStep <- referenceReplay.replaySteps.lift(2).toList
      exploitOccurrence <- (referenceReplay.verticalRelationOccurrences(
        exploitStep,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      ) match
        case exact :: Nil => List(exact)
        case _            => Nil)
    yield VacatedGateEnablesUnrecapturableSliderCaptureChangedSeed(rootReachOccurrence, exploitOccurrence, 2)
    VacatedGateEnablesUnrecapturableSliderCaptureProof.deriveChangedDependencies(
      referenceLine,
      playedLine,
      referenceRecord,
      playedRecord,
      referenceReplay,
      playedReplay,
      changedSeeds.sortBy(_.stableKey)
    )

  private def lineRef(id: String, line: LineNodeRef, fen: String): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = PositionNodeRef(fen, 0, Some(White), Some("direct-line-access-root")),
      line = Some(line),
      scope = line.role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
