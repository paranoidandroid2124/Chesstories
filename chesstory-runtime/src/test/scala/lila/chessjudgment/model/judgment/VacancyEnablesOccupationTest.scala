package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  RawMoveReviewInput,
  RelativeAssessmentAssembler
}
import lila.chessjudgment.model.line.{ EngineLine, PrincipalVariationEvidence }

class VacancyEnablesOccupationTest extends munit.FunSuite:

  private val rootFen =
    "1r4k1/p1q2p1p/2BRbp1B/4p3/P1p4P/6P1/1P2PP1K/3R4 w - - 0 30"
  private val referenceMoves = List("c6g2", "e6f5", "d6c6")
  private val playedMoves = List("d1d2", "e6f5")

  test("canonical replay binds the exact position-owned LegalMove occurrence"):
    val replay = certifiedReplay(rootFen, referenceMoves)
    val line = LineNodeRef("reference-legal-owner", referenceMoves.head, 1, LineNodeRole.BestReference)
    val record = lineRecord("reference-legal-owner-record", line, rootFen, replay)
    val occurrence = replay.legalMoveOccurrence(replay.replaySteps.head)
      .getOrElse(fail("expected the exact root LegalMove occurrence"))
    val bound = RecordBoundLegalMoveOccurrence.certified(record, occurrence)
      .getOrElse(fail("expected the LegalLine-bound movement occurrence"))

    assertEquals(bound.movement.stableKey, "white:bishop>bishop:c6-g2")
    assertEquals(bound.moveUci, "c6g2")
    assertEquals(bound.capture, None)
    assert(bound.legalMoveSemanticId.matches("[0-9a-f]{64}"))
    assert(bound.occurrenceId.matches("[0-9a-f]{64}"))
    assertEquals(
      bound.causalSourcePremiseIds,
      List(
        bound.issuerEvidenceId,
        bound.occurrenceId,
        s"legal-move:${bound.legalMoveSemanticId}"
      ).sorted
    )

  test("Doknjas sibling closes exact release, later occupation, and Played absence"):
    val exact = deriveProofs(
      "doknjas",
      certifiedReplay(rootFen, referenceMoves),
      certifiedReplay(rootFen, playedMoves)
    ) match
      case one :: Nil => one
      case other      => fail(s"expected one vacancy-enables-occupation proof, found ${other.size}")

    assertEquals(exact.semantic.release.stableKey, "white:bishop>bishop:c6-g2")
    assertEquals(exact.semantic.blocker, colored("c6", "bishop", chess.White))
    assertEquals(exact.semantic.occupation.stableKey, "white:rook>rook:d6-c6")
    assertEquals(exact.semantic.occupier, colored("d6", "rook", chess.White))
    assertEquals(exact.occurrence.referenceSteps.map(_.moveUci), referenceMoves)
    assertEquals(exact.occurrence.playedSteps.map(_.moveUci), playedMoves)

    val path = exact.occurrence.proofPaths.head
    val manifest = path.manifest.asInstanceOf[VacancyOccupationManifest]
    assertEquals(
      manifest.supplementalPremiseUses.map(_.asInstanceOf[CausalLegalMovePremiseUse].role),
      List(
        VacancyOccupationPremiseRole.ReferenceReleaseMove,
        VacancyOccupationPremiseRole.ReferenceOccupationMove
      )
    )
    assertEquals(
      manifest.supplementalPremiseUses.map(_.asInstanceOf[CausalLegalMovePremiseUse].stepIndex),
      List(0, 2)
    )
    assertEquals(
      path.closedAbsenceUses.map(_.binding.queryKey),
      List("legal-move-from-to:white:d6:c6")
    )
    assertEquals(
      path.closedStateUses.map(_.binding.queryKey),
      List(
        "vacant:c6",
        "vacant:c6",
        "occupied-by:white:rook@c6",
        "occupied-by:white:bishop@c6",
        "occupied-by:white:rook@d6"
      )
    )
    assertEquals(path.closedStateUses.map(_.binding.afterStepIndex), List(0, 1, 2, 1, 1))
    assert(exact.semantic.semanticId.matches("[0-9a-f]{64}"))
    assert(exact.occurrence.occurrenceId.matches("[0-9a-f]{64}"))
    assert(exact.dependency.value.matches("[0-9a-f]{64}"))
    assert(exact.remainsCertified)

  test("changed blocker, missing occupier, and capture occupation fail closed"):
    val openedPlayed = List("c6d5", "e6f5")
    assertEquals(
      deriveProofs(
        "opened-played",
        certifiedReplay(rootFen, referenceMoves),
        certifiedReplay(rootFen, openedPlayed),
        exactPlayedMoves = openedPlayed
      ),
      Nil
    )

    val movedOccupier = List("d6d2", "e6f5")
    assertEquals(
      deriveProofs(
        "moved-occupier",
        certifiedReplay(rootFen, referenceMoves),
        certifiedReplay(rootFen, movedOccupier),
        exactPlayedMoves = movedOccupier
      ),
      Nil
    )

    val captureOccupation = List("c6g2", "c7c6", "d6c6")
    assertEquals(
      deriveProofs(
        "capture-occupation",
        certifiedReplay(rootFen, captureOccupation),
        certifiedReplay(rootFen, playedMoves),
        exactReferenceMoves = captureOccupation
      ),
      Nil
    )

  test("bounded occurrence ignores a trailing certified PV move"):
    val extended = referenceMoves :+ "c7e7"
    val exact = deriveProofs(
      "trailing",
      certifiedReplay(rootFen, extended),
      certifiedReplay(rootFen, playedMoves),
      exactReferenceMoves = extended
    ).head

    assertEquals(exact.occurrence.referenceSteps.map(_.moveUci), referenceMoves)
    assertEquals(exact.occurrence.occupationStepIndex, 2)

  test("a delayed occupation retains every intervening exact vacancy without a horizon cap"):
    val delayedReference = List("c6g2", "e6f5", "b2b3", "g8h8", "d6c6")
    val delayedPlayed = List("d1d2", "e6f5", "b2b3", "g8h8")
    val exact = deriveProofs(
      "delayed-vacancy",
      certifiedReplay(rootFen, delayedReference),
      certifiedReplay(rootFen, delayedPlayed),
      exactReferenceMoves = delayedReference,
      exactPlayedMoves = delayedPlayed
    ) match
      case one :: Nil => one
      case other      => fail(s"expected one delayed vacancy proof, found ${other.size}")

    assertEquals(exact.occurrence.occupationStepIndex, 4)
    assertEquals(
      exact.occurrence.proofPaths.head.closedStateUses
        .take(4)
        .map(use => use.binding.afterStepIndex -> use.binding.queryKey),
      List(0 -> "vacant:c6", 1 -> "vacant:c6", 2 -> "vacant:c6", 3 -> "vacant:c6")
    )

  test("a different certified reply still closes the exact blocker obstruction"):
    val differentReply = List("d1d2", "g8h8")
    val exact = deriveProofs(
      "different-reply",
      certifiedReplay(rootFen, referenceMoves),
      certifiedReplay(rootFen, differentReply),
      exactPlayedMoves = differentReply
    ).headOption.getOrElse(fail("expected the exact sibling obstruction without a same-reply heuristic"))

    assertEquals(exact.occurrence.playedSteps.map(_.moveUci), differentReply)
    assertEquals(
      exact.occurrence.proofPaths.head.closedAbsenceUses.map(_.binding.queryKey),
      List("legal-move-from-to:white:d6:c6")
    )

  test("same semantic proof preserves distinct line occurrences, paths, and dependencies"):
    val referenceReplay = certifiedReplay(rootFen, referenceMoves)
    val playedReplay = certifiedReplay(rootFen, playedMoves)
    val first = deriveProofs("route-a", referenceReplay, playedReplay).head
    val second = deriveProofs("route-b", referenceReplay, playedReplay).head

    assertEquals(first.semantic.semanticId, second.semantic.semanticId)
    assertNotEquals(first.occurrence.occurrenceId, second.occurrence.occurrenceId)
    assertNotEquals(
      first.occurrence.proofPaths.head.pathOccurrenceId,
      second.occurrence.proofPaths.head.pathOccurrenceId
    )
    assertNotEquals(first.dependency.value, second.dependency.value)

  test("a reoccupation and independent revacating move cannot be attributed to the root release"):
    val repeatedReference = List(
      "c6g2",
      "e6f5",
      "d6c6",
      "g8h8",
      "c6d6",
      "h8g8",
      "d6c6"
    )
    val extendedPlayed = List("d1d2", "e6f5", "b2b3", "g8h8", "b3b4", "h8g8")
    val lower = EvidenceFactAssembler.assemble(
      RawMoveReviewInput(
        fen = rootFen,
        playedMoveUci = extendedPlayed.head,
        variations = List(
          EngineLine(repeatedReference, scoreCp = 600, depth = 24),
          EngineLine(extendedPlayed, scoreCp = 0, depth = 24)
        )
      )
    ).getOrElse(fail("expected both legal replay routes"))
    val facts = RelativeAssessmentAssembler.enrichFacts(lower)
    val proofs = facts.evidenceGraph.records.collect {
      case EvidenceRecord(_, payload: VacancyEnablesOccupationEvidence, _) => payload
    }

    assertEquals(proofs.size, 1)
    assertEquals(proofs.map(_.semanticId).distinct.size, 1)
    assertEquals(proofs.map(_.occurrenceId).distinct.size, 1)
    assertEquals(proofs.map(_.dependencyId).distinct.size, 1)
    assertEquals(proofs.flatMap(_.proofPaths.map(_.pathOccurrenceId)).distinct.size, 1)
    assertEquals(proofs.head.occurrenceProof.map(_.occurrence.occupationStepIndex), Some(2))
    assert(proofs.forall(_.resultSet.exactlyOwns(proofs.map(_.dependencyId))))
    val reused = RelativeAssessmentAssembler.enrichFacts(facts).evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: VacancyEnablesOccupationEvidence, _) => record
    }
    val original = facts.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: VacancyEnablesOccupationEvidence, _) => record
    }
    assertEquals(reused.map(_.ref.id), original.map(_.ref.id))

  test("the exact proof reaches MissedSquareRelease Cause and no broader family"):
    val lower = EvidenceFactAssembler.assemble(
      RawMoveReviewInput(
        fen = rootFen,
        playedMoveUci = playedMoves.head,
        variations = List(
          EngineLine(referenceMoves, scoreCp = 600, depth = 24),
          EngineLine(playedMoves, scoreCp = 0, depth = 24)
        )
      )
    ).getOrElse(fail("expected exact lower facts"))
    val facts = RelativeAssessmentAssembler.enrichFacts(lower)
    val proofRecords = facts.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: VacancyEnablesOccupationEvidence, _) => record
    }
    assertEquals(proofRecords.size, 1)
    assert(facts.evidenceGraph.proofEligible(proofRecords.head))
    val proofPayload = proofRecords.head.payload.asInstanceOf[VacancyEnablesOccupationEvidence]
    assert(proofRecords.head.referencesLine(proofPayload.occurrence.referenceLine))
    assert(proofRecords.head.referencesLine(proofPayload.occurrence.playedLine))

    val withCauses = RelativeAssessmentAssembler.enrichCauses(facts)
    val exactCauses = withCauses.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.MissedSquareRelease => cause
    }
    assertEquals(exactCauses.size, 1)
    assertEquals(exactCauses.head.sourceSide, RelativeCauseSourceSide.Reference)
    assertEquals(ClaimFamily.fromCause(exactCauses.head.kind), ClaimFamily.BoundedCausal)
    assertEquals(
      withCauses.evidenceGraph.records.collect {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
            if cause.proofSources.exists(_.id == proofRecords.head.ref.id) => cause.kind
      },
      List(RelativeCauseKind.MissedSquareRelease)
    )

  private def deriveProofs(
      route: String,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      exactReferenceMoves: List[String] = referenceMoves,
      exactPlayedMoves: List[String] = playedMoves
  ): List[CertifiedVacancyEnablesOccupation] =
    val referenceLine = LineNodeRef(
      s"reference-$route",
      exactReferenceMoves.head,
      1,
      LineNodeRole.BestReference
    )
    val playedLine = LineNodeRef(
      s"played-$route",
      exactPlayedMoves.head,
      1,
      LineNodeRole.Played
    )
    val referenceRecord = lineRecord(
      s"reference-source-$route",
      referenceLine,
      rootFen,
      referenceReplay
    )
    val playedRecord = lineRecord(
      s"played-source-$route",
      playedLine,
      rootFen,
      playedReplay
    )
    val seeds = for
      releaseStep <- referenceReplay.replaySteps.headOption.toList
      release <- referenceReplay.legalMoveOccurrence(releaseStep).toList
      (occupationStep, index) <- referenceReplay.replaySteps.zipWithIndex.drop(2)
      occupation <- referenceReplay.legalMoveOccurrence(occupationStep).toList
      if occupation.movement.capture.isEmpty &&
        occupation.movement.side == release.movement.side &&
        occupation.movement.to == release.movement.from
    yield VacancyEnablesOccupationChangedSeed(release, occupation, index)
    VacancyEnablesOccupationProof.deriveChangedDependencies(
      referenceLine,
      playedLine,
      referenceRecord,
      playedRecord,
      referenceReplay,
      playedReplay,
      seeds.sortBy(_.stableKey)
    )

  private def certifiedReplay(fen: String, moves: List[String]): CanonicalLineReplay =
    PrincipalVariationEvidence.legalMoveReplay(fen, moves, startPly = 0)
      .flatMap(CanonicalLineReplay.fromLegalReplay)
      .getOrElse(fail(s"expected legal replay for ${moves.mkString(" ")}"))

  private def lineRecord(
      id: String,
      line: LineNodeRef,
      fen: String,
      replay: CanonicalLineReplay
  ): EvidenceRecord =
    EvidenceRecord(
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.LegalLineProducer,
        layer = EvidenceLayer.Line,
        position = PositionNodeRef(fen, 0, Some(White), Some("vacancy-root")),
        line = Some(line),
        scope = line.role.scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      LineFactEvidence.fromCertifiedReplay(line, replay)
    )

  private def colored(square: String, role: String, side: chess.Color): RelationColoredPieceWitness =
    RelationColoredPieceWitness(EvidenceSquare(square), EvidencePieceRole(role), side)
