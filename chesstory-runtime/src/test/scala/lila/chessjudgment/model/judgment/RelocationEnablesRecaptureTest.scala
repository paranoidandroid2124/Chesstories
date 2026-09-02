package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.assembly.{
  EvidenceFactAssembler,
  ExplanationRequest,
  JudgmentProvenanceAllocator,
  OccurrenceExplanationAssembler,
  OccurrenceExplanationDemand,
  RawMoveReviewInput
}
import lila.chessjudgment.model.line.EngineLine

class RelocationEnablesRecaptureTest extends munit.FunSuite:

  private final case class Fixture(rootFen: String, relocated: List[String], retained: List[String])

  private val najdorf = Fixture(
    "1nb3k1/8/1q6/8/3N4/8/7P/K7 b - - 0 1",
    List("c8b7", "h2h3", "b8c6", "d4c6", "b7c6"),
    List("b8c6", "d4c6", "b6c6")
  )

  private val aiRevolution = Fixture(
    "7k/2b5/2b5/8/5P2/5N2/6B1/K1Q5 b - - 0 1",
    List("c7f4", "c1f4", "c6f3", "f4f3"),
    List("c6f3", "g2f3")
  )

  private val szuhanek = Fixture(
    "1n2k3/4b3/5n2/6B1/8/8/P7/K7 w - - 0 1",
    List("a2a3", "b8d7", "g5f6", "d7f6"),
    List("g5f6", "e7f6")
  )

  private val movedAttackerMultiLegTargetAndSecondaryResponder = Fixture(
    "r3k2r/6p1/8/8/8/8/P2N4/K7 b k - 0 1",
    List("e8g8", "d2e4", "a8a6", "a2a3", "a6f6", "e4f6", "f8f6"),
    List("a8a6", "d2e4", "a6f6", "e4f6", "g7f6")
  )

  private val enPassant = Fixture(
    "k6n/2pp4/8/4P3/8/8/P7/K7 b - - 0 1",
    List("h8f7", "a2a3", "d7d5", "e5d6", "f7d6"),
    List("d7d5", "e5d6", "c7d6")
  )

  private val promotedResponder = Fixture(
    "7k/8/8/8/4N2b/1K6/P4rp1/8 b - - 0 1",
    List("g2g1q", "e4f2", "g1f2"),
    List("h8g8", "e4f2", "h4f2")
  )

  private val differentPhysicalTarget = Fixture(
    "1k5r/6p1/r4r2/8/4N3/8/P7/K7 b - - 0 1",
    List("h8f8", "e4f6", "f8f6"),
    List("f6f5", "a2a3", "a6f6", "e4f6", "g7f6")
  )

  private val differentPhysicalAttacker = Fixture(
    "1k5r/1p5p/8/6b1/R4r2/8/8/K3R3 b - - 0 1",
    List("h8f8", "e1e4", "b7b6", "e4f4", "f8f4"),
    List("b7b6", "a4e4", "h7h6", "e4f4", "g5f4")
  )

  private val replacedResponder = Fixture(
    "1nb4k/8/P7/8/3Nb3/8/7P/K7 b - - 0 1",
    List("c8b7", "a6b7", "e4b7", "h2h3", "b8c6", "d4c6", "b7c6"),
    List("b8c6", "d4c6", "e4c6")
  )

  test("the three reference topologies retain exact participants and ordered occurrences"):
    val examples = List(
      (najdorf, "c8b7", "d4c6", "b7c6", "b6c6", "b8", "knight"),
      (aiRevolution, "c1f4", "c6f3", "f4f3", "g2f3", "f3", "knight"),
      (szuhanek, "b8d7", "g5f6", "d7f6", "e7f6", "f6", "knight")
    )

    examples.foreach { case (fixture, relocation, capture, relocatedResponderRecapture,
          retainedOtherRecapture, rootTargetSquare, rootTargetRole) =>
      val (context, record) = produced(fixture)
      val proof = record.payload.asInstanceOf[RelocationEnablesRecaptureEvidence]

      assertEquals(proof.relocationMoveUci, relocation)
      assertEquals(proof.targetCaptureMoveUci, capture)
      assertEquals(proof.relocatedResponderRecaptureMoveUci, relocatedResponderRecapture)
      assertEquals(proof.retainedOtherRecaptureMoveUci, retainedOtherRecapture)
      assertEquals(proof.targetAtCommonRoot.square.key, rootTargetSquare)
      assertEquals(proof.targetAtCommonRoot.role.name, rootTargetRole)
      assertEquals(proof.proofPaths.size, 1)
      assert(proof.occurrenceProof.remainsCertified)
      assert(proof.occurrenceProof.proves(record, proof))
      assert(proof.lowerRecordsAreCanonical(context.evidenceGraph.byId))
      assert(context.evidenceGraph.proofEligible(record))
      assertEquals(proof.relocatedRecaptureStepIndex, proof.relocatedCaptureStepIndex + 1)
      assertEquals(proof.retainedRecaptureStepIndex, proof.retainedCaptureStepIndex + 1)
    }

  test("continuity retains a moved attacker, a multi-leg target, and a castling-secondary responder"):
    val (_, record) = produced(movedAttackerMultiLegTargetAndSecondaryResponder)
    val proof = record.payload.asInstanceOf[RelocationEnablesRecaptureEvidence]
    val uses = continuityUses(proof)

    assertEquals(proof.targetAtCommonRoot, colored("a8", "rook", chess.Black))
    assertEquals(proof.capturedTarget, colored("f6", "rook", chess.Black))
    assertEquals(proof.attackerAtCommonRoot, colored("d2", "knight", chess.White))
    assertEquals(proof.attackerAtCapture, colored("e4", "knight", chess.White))
    assertEquals(proof.trackedResponderAtSeed, colored("h8", "rook", chess.Black))
    assertEquals(proof.trackedResponderAtStaging, colored("f8", "rook", chess.Black))
    assertEquals(proof.relocationMoveUci, "e8g8")
    assertEquals(proof.relocationMove.from.key, "h8")
    assertEquals(proof.relocationMove.to.key, "f8")
    assertEquals(
      movedUses(uses, RelocationEnablesRecaptureContinuityRole.RelocatedBranchTarget)
        .map(use => use.selectedTransition.get.from.key -> use.selectedTransition.get.to.key),
      List("a8" -> "a6", "a6" -> "f6")
    )
    assertEquals(
      movedUses(uses, RelocationEnablesRecaptureContinuityRole.RelocatedBranchAttacker)
        .map(use => use.selectedTransition.get.from.key -> use.selectedTransition.get.to.key),
      List("d2" -> "e4")
    )
    val secondary = movedUses(uses, RelocationEnablesRecaptureContinuityRole.RelocatedResponder)
    assertEquals(secondary.map(_.transitionKind), List(ObjectContinuityStepKind.Secondary))
    assertEquals(secondary.map(_.overallMoveUci), List("e8g8"))

  test("en-passant keeps the captured victim square distinct from the recapture landing square"):
    val (_, record) = produced(enPassant)
    val proof = record.payload.asInstanceOf[RelocationEnablesRecaptureEvidence]

    assertEquals(proof.capturedTarget, colored("d5", "pawn", chess.Black))
    assertEquals(proof.recaptureSquare.key, "d6")
    assertEquals(proof.targetCaptureMoveUci, "e5d6")
    assertEquals(proof.relocatedResponderRecaptureMoveUci, "f7d6")
    assertEquals(proof.retainedOtherRecaptureMoveUci, "c7d6")

  test("promotion preserves responder identity while changing its exact role"):
    val (_, record) = produced(promotedResponder)
    val proof = record.payload.asInstanceOf[RelocationEnablesRecaptureEvidence]

    assertEquals(proof.trackedResponderAtSeed, colored("g2", "pawn", chess.Black))
    assertEquals(proof.trackedResponderAtStaging, colored("g1", "queen", chess.Black))
    assertEquals(proof.relocationMove.beforeRole.name, "pawn")
    assertEquals(proof.relocationMove.afterRole.name, "queen")
    assertEquals(proof.relocatedResponderRecaptureMoveUci, "g1f2")

  test("a capture at the branch root needs no fabricated target or attacker retention step"):
    val (_, record) = produced(szuhanek)
    val proof = record.payload.asInstanceOf[RelocationEnablesRecaptureEvidence]
    val retainedBranchId = proof.retainedBranch.branchId
    val uses = continuityUses(proof)

    assertEquals(
      uses.count(use =>
        use.branchId == retainedBranchId &&
          (use.role == RelocationEnablesRecaptureContinuityRole.RetainedBranchTarget ||
            use.role == RelocationEnablesRecaptureContinuityRole.RetainedBranchAttacker)
      ),
      0
    )

  test("the sibling must close the exact seed-to-recapture-square move absence"):
    val bothSeedMovesAreLegal = Fixture(
      "7k/7p/8/8/8/2b5/1N6/QR5K b - - 0 1",
      List("h7h6", "a1a2", "c3b2", "a2b2"),
      List("c3b2", "b1b2")
    )
    assertEquals(proofRecords(enriched(bothSeedMovesAreLegal)).size, 0)

  test("same move tokens do not hide a responder that moves away and returns"):
    val returnedResponder = najdorf.copy(
      relocated = List(
        "c8b7", "h2h3", "b7a6", "h3h4", "a6b7", "h4h5", "b8c6", "d4c6", "b7c6"
      )
    )
    assertEquals(proofRecords(enriched(returnedResponder)).size, 0)

  test("a captured responder cannot be replaced by another same-role object at staging"):
    assertEquals(proofRecords(enriched(replacedResponder)).size, 0)

  test("same capture tokens do not merge different common-root target or attacker objects"):
    assertEquals(proofRecords(enriched(differentPhysicalTarget)).size, 0)
    assertEquals(proofRecords(enriched(differentPhysicalAttacker)).size, 0)

  test("a delayed recapture and a different target capture are not inferred"):
    val delayed = najdorf.copy(
      relocated = List("c8b7", "h2h3", "b8c6", "d4c6", "b6b5", "a1a2", "b7c6")
    )
    val differentCapture = najdorf.copy(retained = List("b8c6", "d4b5", "b6b5"))

    assertEquals(proofRecords(enriched(delayed)).size, 0)
    assertEquals(proofRecords(enriched(differentCapture)).size, 0)

  test("the family is demand bounded and requires both exact branches"):
    val facts = EvidenceFactAssembler.assemble(input(najdorf)).getOrElse(fail("expected line facts"))
    assertEquals(
      facts.evidenceGraph.records.count(_.payload.isInstanceOf[RelocationEnablesRecaptureEvidence]),
      0
    )
    val oneBranch = najdorf.copy(retained = List("b8c6", "d4c6", "b6c6"))
    val oneLineFacts = EvidenceFactAssembler.assemble(
      RawMoveReviewInput(
        fen = oneBranch.rootFen,
        playedMoveUci = oneBranch.retained.head,
        variations = List(EngineLine(oneBranch.retained, scoreCp = 0, depth = 24))
      )
    ).getOrElse(fail("expected one admitted line"))
    val demand = OccurrenceExplanationDemand.resolve(
      oneLineFacts,
      ExplanationRequest.forObservedMove(oneLineFacts.input)
    ).getOrElse(fail("expected the observed demand"))
    val produced = OccurrenceExplanationAssembler.enrichProofs(
      oneLineFacts,
      JudgmentProvenanceAllocator.forInput(oneLineFacts.input),
      demand
    )
    assertEquals(proofRecords(produced).size, 0)

  test("either semantic branch may own the exact observed subject occurrence"):
    val (_, retainedRecord) = produced(najdorf)
    val (_, relocatedRecord) = produced(najdorf, najdorf.relocated.head)
    val retainedProof = retainedRecord.payload.asInstanceOf[RelocationEnablesRecaptureEvidence]
    val relocatedProof = relocatedRecord.payload.asInstanceOf[RelocationEnablesRecaptureEvidence]

    assertEquals(retainedProof.semanticId, relocatedProof.semanticId)
    assertNotEquals(retainedProof.subjectOccurrence.occurrenceId, relocatedProof.subjectOccurrence.occurrenceId)
    assertEquals(retainedProof.subjectOccurrence.moveUci, najdorf.retained.head)
    assertEquals(relocatedProof.subjectOccurrence.moveUci, najdorf.relocated.head)
    assert(retainedProof.occurrenceProof.proves(retainedRecord, retainedProof))
    assert(relocatedProof.occurrenceProof.proves(relocatedRecord, relocatedProof))

  test("transposed relocated histories retain distinct occurrences, dependencies, paths, and Causes"):
    val rootFen = "1nb3k1/8/1q6/8/3N4/8/P6P/K7 b - - 0 1"
    val firstOrder = List("c8b7", "a2a3", "g8h8", "h2h3", "b8c6", "d4c6", "b7c6")
    val secondOrder = List("g8h8", "h2h3", "c8b7", "a2a3", "b8c6", "d4c6", "b7c6")
    val retained = List("b8c6", "d4c6", "b6c6")
    val raw = RawMoveReviewInput(
      rootFen,
      retained.head,
      List(
        EngineLine(firstOrder, scoreCp = 30, depth = 24),
        EngineLine(secondOrder, scoreCp = 25, depth = 24),
        EngineLine(retained, scoreCp = 20, depth = 24)
      )
    )
    val facts = EvidenceFactAssembler.assemble(raw).getOrElse(fail("expected transposed line facts"))
    val allocator = JudgmentProvenanceAllocator.forInput(facts.input)
    val demand = OccurrenceExplanationDemand.resolve(
      facts,
      ExplanationRequest.forObservedMove(facts.input)
    ).getOrElse(fail("expected the observed transposition demand"))
    val proofContext = OccurrenceExplanationAssembler.enrichProofs(facts, allocator, demand)
    val proofs = proofRecords(proofContext).map(_.payload.asInstanceOf[RelocationEnablesRecaptureEvidence])

    assertEquals(proofs.size, 2)
    assertEquals(proofs.map(_.semanticId).distinct.size, 1)
    assertEquals(proofs.map(_.occurrenceId).distinct.size, 2)
    assertEquals(proofs.map(_.dependencyId).distinct.size, 2)
    assertEquals(proofs.flatMap(_.proofPaths.map(_.pathOccurrenceId)).distinct.size, 2)
    assert(proofs.forall(_.proofPaths.size == 1))
    assertEquals(proofs.map(_.relocatedBranch.lineOwnerEvidenceId).distinct.size, 2)

    val causeContext = OccurrenceExplanationAssembler.enrichCauses(proofContext, allocator, demand)
    val causes = causeContext.evidenceGraph.records.collect {
      case EvidenceRecord(_, OccurrenceExplanationCauseEvidence(cause), _)
          if cause.proofOccurrence.family == BoundedCausalContractKind.RelocationEnablesRecapture => cause
    }
    assertEquals(causes.size, 2)
    assertEquals(causes.map(_.proofOccurrence.occurrenceId).distinct.size, 2)

  test("an unrelated observed root cannot borrow two sibling proof branches"):
    val facts = EvidenceFactAssembler.assemble(
      RawMoveReviewInput(
        fen = najdorf.rootFen,
        playedMoveUci = "b6b5",
        variations = List(
          EngineLine(najdorf.relocated, scoreCp = 30, depth = 24),
          EngineLine(najdorf.retained, scoreCp = 20, depth = 24),
          EngineLine(List("b6b5", "h2h3"), scoreCp = 10, depth = 24)
        )
      )
    ).getOrElse(fail("expected an admitted unrelated subject"))
    val demand = OccurrenceExplanationDemand.resolve(
      facts,
      ExplanationRequest.forObservedMove(facts.input)
    ).getOrElse(fail("expected an unrelated observed demand"))
    val produced = OccurrenceExplanationAssembler.enrichProofs(
      facts,
      JudgmentProvenanceAllocator.forInput(facts.input),
      demand
    )
    assertEquals(proofRecords(produced).size, 0)

  private def produced(
      fixture: Fixture,
      playedMoveUci: String = ""
  ): (JudgmentAssemblyContext, EvidenceRecord) =
    val context = enriched(fixture, playedMoveUci)
    proofRecords(context) match
      case record :: Nil => context -> record
      case other         => fail(s"expected one relocation-recapture proof, found ${other.size}")

  private def enriched(fixture: Fixture, playedMoveUci: String = ""): JudgmentAssemblyContext =
    val facts = EvidenceFactAssembler.assemble(input(fixture, playedMoveUci))
      .getOrElse(fail("expected certified line facts"))
    val demand = OccurrenceExplanationDemand.resolve(
      facts,
      ExplanationRequest.forObservedMove(facts.input)
    ).getOrElse(fail("expected the observed occurrence demand"))
    OccurrenceExplanationAssembler.enrichProofs(
      facts,
      JudgmentProvenanceAllocator.forInput(facts.input),
      demand
    )

  private def input(fixture: Fixture, playedMoveUci: String = "") = RawMoveReviewInput(
    fen = fixture.rootFen,
    playedMoveUci = Option(playedMoveUci).filter(_.nonEmpty).getOrElse(fixture.retained.head),
    variations = List(
      EngineLine(fixture.relocated, scoreCp = 30, depth = 24),
      EngineLine(fixture.retained, scoreCp = 20, depth = 24)
    )
  )

  private def proofRecords(context: JudgmentAssemblyContext) = context.evidenceGraph.records.collect {
    case record @ EvidenceRecord(_, _: RelocationEnablesRecaptureEvidence, _) => record
  }

  private def continuityUses(
      proof: RelocationEnablesRecaptureEvidence
  ): List[CausalObjectContinuityStepUse] = proof.proofPaths.flatMap(
    _.manifest.supplementalPremiseUses.collect { case use: CausalObjectContinuityStepUse => use }
  )

  private def movedUses(
      uses: List[CausalObjectContinuityStepUse],
      role: RelocationEnablesRecaptureContinuityRole
  ): List[CausalObjectContinuityStepUse] =
    uses.filter(use => use.role == role && use.selectedTransition.nonEmpty)

  private def colored(square: String, role: String, side: chess.Color) =
    RelationColoredPieceWitness(EvidenceSquare(square), EvidencePieceRole(role), side)
