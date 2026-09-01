package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  RawMoveReviewInput,
  RelativeAssessmentAssembler
}
import lila.chessjudgment.model.line.{ EngineLine, PrincipalVariationEvidence }

class SquareReleaseRouteTest extends munit.FunSuite:

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
      case other      => fail(s"expected one square-release-route proof, found ${other.size}")

    assertEquals(exact.semantic.release.stableKey, "white:bishop>bishop:c6-g2")
    assertEquals(exact.semantic.blocker, colored("c6", "bishop", chess.White))
    assertEquals(exact.semantic.route.map(_.stableKey), List("white:rook>rook:d6-c6"))
    assertEquals(exact.semantic.routePiece, colored("d6", "rook", chess.White))
    assertEquals(exact.semantic.terminal, SquareReleaseRouteTerminal.Occupation)
    assertEquals(exact.occurrence.referenceSteps.map(_.moveUci), referenceMoves)
    assertEquals(exact.occurrence.playedSteps.map(_.moveUci), playedMoves)

    val path = exact.occurrence.proofPaths.head
    val manifest = path.manifest.asInstanceOf[SquareReleaseRouteManifest]
    assertEquals(
      manifest.supplementalPremiseUses.map(_.asInstanceOf[CausalLegalMovePremiseUse].role),
      List(
        SquareReleaseRoutePremiseRole.ReferenceReleaseMove,
        SquareReleaseRoutePremiseRole.ReferenceRouteMove(0)
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
        "occupied-by:white:bishop@c6",
        "occupied-by:white:rook@d6",
        "occupied-by:white:rook@d6"
      )
    )
    assertEquals(path.closedStateUses.map(_.binding.afterStepIndex), List(0, 1, 2, 0, 1, 0, 1))
    assert(exact.semantic.semanticId.matches("[0-9a-f]{64}"))
    assert(exact.occurrence.occurrenceId.matches("[0-9a-f]{64}"))
    assert(exact.dependency.value.matches("[0-9a-f]{64}"))
    assert(exact.remainsCertified)

  test("Doknjas route retains one knight through four occurrences and its exact capture reply"):
    val doknjasFen = "r5k1/2pq1ppp/br2p3/p1Qn4/3P4/2N2PP1/PP1BPK1P/1R5R b - - 4 17"
    val doknjasReference = List(
      "b6c6",
      "c5a3",
      "d5b6",
      "d2e3",
      "b6c4",
      "a3b3",
      "c6b6",
      "b3c2",
      "a8b8",
      "b2b3",
      "c4a3",
      "c2d2",
      "a3b1",
      "h1b1"
    )
    val doknjasSibling = List("a5a4", "c5a3")
    val exact = deriveTerminalProofs(
      route = "doknjas-goryachkina-dubov",
      root = doknjasFen,
      referenceMoves = doknjasReference,
      playedMoves = doknjasSibling,
      routeIndices = List(2, 4, 10, 12),
      terminalContract = VerticalRelationContractKind.CaptureRecaptureInventory,
      terminalReplyIndex = Some(13),
      rootSide = chess.Black
    ) match
      case one :: Nil => one
      case other      => fail(s"expected one exact Doknjas terminal route, found ${other.size}")

    assertEquals(exact.semantic.release.stableKey, "black:rook>rook:b6-c6")
    assertEquals(
      exact.semantic.route.map(_.stableKey),
      List(
        "black:knight>knight:d5-b6",
        "black:knight>knight:b6-c4",
        "black:knight>knight:c4-a3",
        "black:knight>knight:a3-b1"
      )
    )
    exact.semantic.terminal match
      case SquareReleaseRouteTerminal.Capture(_, mover, captured, _, legalRecaptures, _) =>
        assertEquals(mover.stableKey, "black:knight>knight:a3-b1")
        assertEquals(captured, colored("b1", "rook", chess.White))
        assertEquals(legalRecaptures.map(_.moveUci), List("c3b1", "h1b1"))
      case other => fail(s"expected the exact terminal capture, found $other")
    assertEquals(exact.occurrence.routeStepIndices, List(2, 4, 10, 12))
    assertEquals(exact.occurrence.terminalReplyStep.map(_.moveUci), Some("h1b1"))
    assertEquals(exact.occurrence.referenceSteps.map(_.moveUci), doknjasReference)
    assertEquals(exact.occurrence.playedSteps.map(_.moveUci), doknjasSibling)
    assertEquals(exact.occurrence.proofPaths.head.closedStateUses.size, 17)
    assert(exact.remainsCertified)

  test("Silicon Road route retains four knight occurrences and does not turn five replies into force"):
    val preRoot = "1r3n2/pp6/2p1p1k1/N3R2p/3PP1p1/1P2K1P1/P6P/8 w - - 0 30"
    val prefix = certifiedReplay(preRoot, List("a2a3", "g6h6"))
    val siliconFen = prefix.replaySteps.last.fenAfter
    val siliconReference = List(
      "b3b4",
      "h6g6",
      "a5b3",
      "b7b6",
      "b3c1",
      "b8e8",
      "c1d3",
      "f8d7",
      "d3f4",
      "g6f7"
    )
    val siliconSibling = List("a3a4", "h6g6")
    val exact = deriveTerminalProofs(
      route = "silicon-road-knight-route",
      root = siliconFen,
      referenceMoves = siliconReference,
      playedMoves = siliconSibling,
      routeIndices = List(2, 4, 6, 8),
      terminalContract = VerticalRelationContractKind.CreatedCheckResponseInventory,
      terminalReplyIndex = Some(9)
    ) match
      case one :: Nil => one
      case other      => fail(s"expected one exact Silicon Road terminal route, found ${other.size}")

    assertEquals(exact.semantic.release.stableKey, "white:pawn>pawn:b3-b4")
    assertEquals(
      exact.semantic.route.map(_.stableKey),
      List(
        "white:knight>knight:a5-b3",
        "white:knight>knight:b3-c1",
        "white:knight>knight:c1-d3",
        "white:knight>knight:d3-f4"
      )
    )
    exact.semantic.terminal match
      case SquareReleaseRouteTerminal.CreatedCheck(_, mover, checkedSide, _, _, responses, _, terminal) =>
        assertEquals(mover.stableKey, "white:knight>knight:d3-f4")
        assertEquals(checkedSide, chess.Black)
        assertEquals(responses.size, 5)
        assertEquals(responses.map(_.resource.moveUci).contains("g6f7"), true)
        assertEquals(terminal, RelationCheckTerminalState.Ongoing)
      case other => fail(s"expected the exact created-check terminal, found $other")
    assertEquals(exact.occurrence.routeStepIndices, List(2, 4, 6, 8))
    assertEquals(exact.occurrence.terminalReplyStep.map(_.moveUci), Some("g6f7"))
    assertEquals(exact.occurrence.proofPaths.head.closedStateUses.size, 13)
    assert(exact.remainsCertified)

    val facts = EvidenceFactAssembler.assemble(
      RawMoveReviewInput(
        fen = siliconFen,
        playedMoveUci = siliconSibling.head,
        variations = List(
          EngineLine(siliconReference, scoreCp = 600, depth = 24),
          EngineLine(siliconSibling, scoreCp = 0, depth = 24)
        )
      )
    ).map(RelativeAssessmentAssembler.enrichFacts)
      .getOrElse(fail("expected the exact Silicon Road lower facts"))
    val produced = facts.evidenceGraph.records.collect {
      case EvidenceRecord(_, payload: SquareReleaseRouteEvidence, _) => payload
    }
    assertEquals(produced.count(_.publicTerminal == SquareReleaseRoutePublicTerminal.Occupation), 1)
    assertEquals(
      produced.count(_.publicTerminal.isInstanceOf[SquareReleaseRoutePublicTerminal.CreatedCheck]),
      1
    )
    assertEquals(produced.map(_.occurrenceId).distinct.size, 2)
    assert(produced.forall(_.proofPaths.nonEmpty))

  test("a skipped same-object leg and a missing ongoing reply fail closed"):
    val preRoot = "1r3n2/pp6/2p1p1k1/N3R2p/3PP1p1/1P2K1P1/P6P/8 w - - 0 30"
    val prefix = certifiedReplay(preRoot, List("a2a3", "g6h6"))
    val siliconFen = prefix.replaySteps.last.fenAfter
    val siliconReference = List(
      "b3b4",
      "h6g6",
      "a5b3",
      "b7b6",
      "b3c1",
      "b8e8",
      "c1d3",
      "f8d7",
      "d3f4",
      "g6f7"
    )
    val siliconSibling = List("a3a4", "h6g6")

    assertEquals(
      deriveTerminalProofs(
        "skipped-leg",
        siliconFen,
        siliconReference,
        siliconSibling,
        List(2, 6, 8),
        VerticalRelationContractKind.CreatedCheckResponseInventory,
        Some(9)
      ),
      Nil
    )
    assertEquals(
      deriveTerminalProofs(
        "missing-reply",
        siliconFen,
        siliconReference,
        siliconSibling,
        List(2, 4, 6, 8),
        VerticalRelationContractKind.CreatedCheckResponseInventory,
        None
      ),
      Nil
    )

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
    assertEquals(exact.occurrence.firstRouteStepIndex, 2)

  test("a delayed occupation retains every intervening exact vacancy without a horizon cap"):
    val delayedReference = List("c6g2", "e6f5", "b2b3", "g8h8", "d6c6")
    val delayedPlayed = List("d1d2", "e6f5", "b2b3", "g8h8")
    val exact = deriveProofs(
      "delayed-vacancy",
      certifiedReplay(rootFen, delayedReference),
      certifiedReplay(rootFen, delayedPlayed),
      exactReferenceMoves = delayedReference,
      exactPlayedMoves = delayedPlayed,
      firstRouteIndex = 4
    ) match
      case one :: Nil => one
      case other      => fail(s"expected one delayed vacancy proof, found ${other.size}")

    assertEquals(exact.occurrence.firstRouteStepIndex, 4)
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

  test("endpoint restoration cannot replace continuous sibling persistence"):
    val delayedReference = List("c6g2", "e6f5", "b2b3", "g8h8", "d6c6")
    val leaveAndReturn = List("c6d5", "e6f5", "d5c6", "g8h8")
    assertEquals(
      deriveProofs(
        "leave-and-return",
        certifiedReplay(rootFen, delayedReference),
        certifiedReplay(rootFen, leaveAndReturn),
        exactReferenceMoves = delayedReference,
        exactPlayedMoves = leaveAndReturn,
        firstRouteIndex = 4
      ),
      Nil
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
      case EvidenceRecord(_, payload: SquareReleaseRouteEvidence, _) => payload
    }

    assertEquals(proofs.size, 1)
    assertEquals(proofs.map(_.semanticId).distinct.size, 1)
    assertEquals(proofs.map(_.occurrenceId).distinct.size, 1)
    assertEquals(proofs.map(_.dependencyId).distinct.size, 1)
    assertEquals(proofs.flatMap(_.proofPaths.map(_.pathOccurrenceId)).distinct.size, 1)
    assertEquals(proofs.head.occurrenceProof.map(_.occurrence.firstRouteStepIndex), Some(2))
    assert(proofs.forall(_.resultSet.exactlyOwns(proofs.map(_.dependencyId))))
    val reused = RelativeAssessmentAssembler.enrichFacts(facts).evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: SquareReleaseRouteEvidence, _) => record
    }
    val original = facts.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: SquareReleaseRouteEvidence, _) => record
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
      case record @ EvidenceRecord(_, _: SquareReleaseRouteEvidence, _) => record
    }
    assertEquals(proofRecords.size, 1)
    assert(facts.evidenceGraph.proofEligible(proofRecords.head))
    val proofPayload = proofRecords.head.payload.asInstanceOf[SquareReleaseRouteEvidence]
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
      exactPlayedMoves: List[String] = playedMoves,
      firstRouteIndex: Int = 2
  ): List[CertifiedSquareReleaseRoute] =
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
      firstRouteStep <- referenceReplay.replaySteps.lift(firstRouteIndex).toList
      firstRouteLeg <- referenceReplay.legalMoveOccurrence(firstRouteStep).toList
    yield SquareReleaseRouteChangedSeed.occupation(release, firstRouteLeg, firstRouteIndex)
    SquareReleaseRouteProof.deriveChangedDependencies(
      referenceLine,
      playedLine,
      referenceRecord,
      playedRecord,
      referenceReplay,
      playedReplay,
      seeds.sortBy(_.stableKey)
    )

  private def deriveTerminalProofs(
      route: String,
      root: String,
      referenceMoves: List[String],
      playedMoves: List[String],
      routeIndices: List[Int],
      terminalContract: VerticalRelationContractKind,
      terminalReplyIndex: Option[Int],
      rootSide: chess.Color = chess.White
  ): List[CertifiedSquareReleaseRoute] =
    val referenceReplay = certifiedReplay(root, referenceMoves)
    val playedReplay = certifiedReplay(root, playedMoves)
    val referenceLine = LineNodeRef(
      s"reference-$route",
      referenceMoves.head,
      1,
      LineNodeRole.BestReference
    )
    val playedLine = LineNodeRef(
      s"played-$route",
      playedMoves.head,
      1,
      LineNodeRole.Played
    )
    val referenceRecord = lineRecord(
      s"reference-source-$route",
      referenceLine,
      root,
      referenceReplay,
      rootSide
    )
    val playedRecord = lineRecord(
      s"played-source-$route",
      playedLine,
      root,
      playedReplay,
      rootSide
    )
    val routeOccurrences = routeIndices.flatMap(index =>
      referenceReplay.replaySteps.lift(index).flatMap(referenceReplay.legalMoveOccurrence)
    )
    val seeds = for
      releaseStep <- referenceReplay.replaySteps.headOption.toList
      release <- referenceReplay.legalMoveOccurrence(releaseStep).toList
      if routeOccurrences.size == routeIndices.size
      terminalStep <- referenceReplay.replaySteps.lift(routeIndices.last).toList
      terminal <- referenceReplay.verticalRelationOccurrences(terminalStep, List(terminalContract))
      reply = terminalReplyIndex.flatMap(index =>
        referenceReplay.replaySteps.lift(index).flatMap(referenceReplay.legalMoveOccurrence)
      )
    yield SquareReleaseRouteChangedSeed.terminal(
      release,
      routeOccurrences,
      routeIndices,
      terminal,
      reply
    )
    SquareReleaseRouteProof.deriveChangedDependencies(
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
      replay: CanonicalLineReplay,
      side: chess.Color = White
  ): EvidenceRecord =
    EvidenceRecord(
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.LegalLineProducer,
        layer = EvidenceLayer.Line,
        position = PositionNodeRef(fen, 0, Some(side), Some("square-release-root")),
        line = Some(line),
        scope = line.role.scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      LineFactEvidence.fromCertifiedReplay(line, replay)
    )

  private def colored(square: String, role: String, side: chess.Color): RelationColoredPieceWitness =
    RelationColoredPieceWitness(EvidenceSquare(square), EvidencePieceRole(role), side)
