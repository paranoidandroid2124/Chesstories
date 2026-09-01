package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  RawMoveReviewInput,
  RelativeAssessmentAssembler
}
import lila.chessjudgment.model.line.EngineLine

class CaptureExclusionMoveOrderTest extends munit.FunSuite:

  private val rootFen =
    "r2qrbk1/1bpn1p1p/p2p1np1/Pp2p3/3PP3/2P2NNP/1PB2PP1/R1BQR1K1 w - - 1 17"
  private val referenceMoves = List("d4d5", "c7c6", "d5c6", "b7c6", "b2b4")
  private val playedMoves = List("b2b4", "e5d4")

  test("Doknjas Ruy move order closes the exact capture reply before the same deferred move"):
    val context = enriched(rootFen, referenceMoves, playedMoves)
    val record = captureRecord(context)
    val proof = record.payload.asInstanceOf[CaptureExclusionMoveOrderEvidence]

    assertEquals(proof.vacatingMove.stableKey, "white:pawn>pawn:d4-d5")
    assertEquals(proof.deferredMove.stableKey, "white:pawn>pawn:b2-b4")
    assertEquals(proof.captureReply.stableKey, "black:pawn>pawn:e5-d4")
    assertEquals(proof.capturedTarget, colored("d4", "pawn", chess.White))
    assertEquals(proof.referenceDeferredStepIndex, 4)
    assertEquals(proof.occurrence.referenceSteps.map(_.moveUci), referenceMoves)
    assertEquals(proof.occurrence.playedSteps.map(_.moveUci), playedMoves)

    val path = proof.proofPaths match
      case one :: Nil => one
      case other      => fail(s"expected one independent proof path, found ${other.size}")
    assertEquals(
      path.manifest.supplementalPremiseUses.map(_.asInstanceOf[CausalLegalMovePremiseUse].role),
      List(
        CaptureExclusionMoveOrderPremiseRole.ReferenceVacatingMove,
        CaptureExclusionMoveOrderPremiseRole.PlayedDeferredMove,
        CaptureExclusionMoveOrderPremiseRole.PlayedCaptureReply,
        CaptureExclusionMoveOrderPremiseRole.ReferenceDeferredMove
      )
    )
    assertEquals(
      path.manifest.supplementalPremiseUses.map(_.asInstanceOf[CausalLegalMovePremiseUse].stepIndex),
      List(0, 0, 1, 4)
    )
    val legalUses = path.manifest.supplementalPremiseUses.map(
      _.asInstanceOf[CausalLegalMovePremiseUse]
    )
    assertEquals(legalUses(1).legalMoveSemanticId, legalUses(3).legalMoveSemanticId)
    assertNotEquals(legalUses(1).issuerOccurrenceId, legalUses(3).issuerOccurrenceId)
    assertEquals(
      path.closedAbsenceUses.map(_.binding.afterStepIndex),
      List(0, 4)
    )
    assertEquals(
      path.closedAbsenceUses.map(_.binding.queryKey),
      List.fill(2)("legal-move-from-to:black:e5:d4")
    )
    assertEquals(path.closedStateUses.size, 14)
    assertEquals(
      path.closedStateUses.count(
        _.binding.role == CaptureExclusionMoveOrderStateRole.ReferenceVacatedTarget
      ),
      5
    )
    assertEquals(
      path.closedStateUses.count(
        _.binding.role == CaptureExclusionMoveOrderStateRole.ReferenceReplyActor
      ),
      5
    )
    assertEquals(
      path.closedStateUses.count(
        _.binding.role == CaptureExclusionMoveOrderStateRole.ReferenceDeferredActor
      ),
      4
    )
    assert(proof.hasCompleteProofPaths)
    assert(context.evidenceGraph.proofEligible(record))
    assert(proof.semanticId.matches("[0-9a-f]{64}"))
    assert(proof.occurrenceId.matches("[0-9a-f]{64}"))
    assert(proof.dependencyId.matches("[0-9a-f]{64}"))

  test("the shortest later occurrence retains both endpoint absences and the complete state interval"):
    val proof = captureRecord(
      enriched(rootFen, List("d4d5", "c7c6", "b2b4"), playedMoves)
    ).payload.asInstanceOf[CaptureExclusionMoveOrderEvidence]
    val path = proof.proofPaths.head

    assertEquals(proof.referenceDeferredStepIndex, 2)
    assertEquals(path.closedAbsenceUses.map(_.binding.afterStepIndex), List(0, 2))
    assertEquals(path.closedStateUses.size, 8)
    assert(proof.hasCompleteProofPaths)

  test("endpoint, state, and branch tampering cannot form the exact manifest"):
    val proof = captureRecord(
      enriched(rootFen, referenceMoves, playedMoves)
    ).payload.asInstanceOf[CaptureExclusionMoveOrderEvidence]
    val path = proof.proofPaths.head
    val legalMoves = path.manifest.supplementalPremiseUses.map(
      _.asInstanceOf[CausalLegalMovePremiseUse]
    )
    val absences = path.manifest.absenceBindings
    val states = path.manifest.stateBindings

    def rebuild(
        candidateAbsences: List[CausalClosedAbsenceBinding],
        candidateStates: List[CausalClosedStateBinding]
    ) =
      CaptureExclusionMoveOrderManifest.exact(
        legalMoves(0),
        legalMoves(1),
        legalMoves(2),
        legalMoves(3),
        proof.capturedTarget,
        candidateAbsences,
        candidateStates
      )

    intercept[IllegalArgumentException] {
      rebuild(absences.reverse, states)
    }
    intercept[IllegalArgumentException] {
      rebuild(absences, states.dropRight(1))
    }
    intercept[IllegalArgumentException] {
      CausalClosedStateBinding.afterStep(
        states.head.role,
        states.head.authority,
        proof.playedBranch,
        0
      )
    }

  test("the family is demand-bounded and reuses its sole graph owner"):
    val first = enriched(rootFen, referenceMoves, playedMoves)
    val original = first.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: CaptureExclusionMoveOrderEvidence, _) => record
    }
    val repeated = RelativeAssessmentAssembler.enrichFacts(first).evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: CaptureExclusionMoveOrderEvidence, _) => record
    }
    assertEquals(original.size, 1)
    assertEquals(repeated.map(_.ref.id), original.map(_.ref.id))

    val noReply = enriched(rootFen, referenceMoves, List("b2b4", "f8g7"))
    assertEquals(
      noReply.evidenceGraph.records.count(_.payload.isInstanceOf[CaptureExclusionMoveOrderEvidence]),
      0
    )

  test("transposed reference histories share semantics without losing occurrences"):
    val kingFirst = List(
      "d4d5",
      "g8h8",
      "a1b1",
      "f6h5",
      "b1a1",
      "h8g8",
      "b2b4"
    )
    val knightFirst = List(
      "d4d5",
      "f6h5",
      "a1b1",
      "g8h8",
      "b1a1",
      "h8g8",
      "b2b4"
    )
    val first = captureRecord(enriched(rootFen, kingFirst, playedMoves)).payload
      .asInstanceOf[CaptureExclusionMoveOrderEvidence]
    val second = captureRecord(enriched(rootFen, knightFirst, playedMoves)).payload
      .asInstanceOf[CaptureExclusionMoveOrderEvidence]

    assertEquals(
      first.referenceBranch.replaySteps.last.fenAfter,
      second.referenceBranch.replaySteps.last.fenAfter
    )
    assertEquals(first.semanticId, second.semanticId)
    assertNotEquals(first.occurrenceId, second.occurrenceId)
    assertNotEquals(first.dependencyId, second.dependencyId)
    assertNotEquals(first.referenceBranch.branchId, second.referenceBranch.branchId)

  test("the typed proof reaches the exact reference-side WrongMoveOrder Cause"):
    val facts = enriched(rootFen, referenceMoves, playedMoves)
    val proofRecord = captureRecord(facts)
    val withCauses = RelativeAssessmentAssembler.enrichCauses(facts)
    val causes = withCauses.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder => cause
    }

    assertEquals(causes.size, 1)
    assertEquals(causes.head.sourceSide, RelativeCauseSourceSide.Reference)
    assert(causes.head.proofSources.exists(_.id == proofRecord.ref.id))

  private def enriched(
      fen: String,
      reference: List[String],
      played: List[String]
  ): JudgmentAssemblyContext =
    EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = fen,
          playedMoveUci = played.head,
          variations = List(
            EngineLine(reference, scoreCp = 600, depth = 24),
            EngineLine(played, scoreCp = 0, depth = 24)
          )
        )
      )
      .map(RelativeAssessmentAssembler.enrichFacts)
      .getOrElse(fail("expected exact certified line facts"))

  private def colored(
      square: String,
      role: String,
      side: chess.Color
  ): RelationColoredPieceWitness =
    RelationColoredPieceWitness(EvidenceSquare(square), EvidencePieceRole(role), side)

  private def captureRecord(context: JudgmentAssemblyContext): EvidenceRecord =
    context.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: CaptureExclusionMoveOrderEvidence, _) => record
    } match
      case one :: Nil => one
      case other      => fail(s"expected one capture-exclusion move-order proof, found ${other.size}")
