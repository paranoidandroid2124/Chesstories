package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class PassedPawnProgressRealizedAfterOnlyLegalReplyProofTest extends munit.FunSuite:

  private val singletonAnalysisContinuation = RawMoveReviewInput(
    fen = "7k/5K2/6P1/8/8/8/8/8 w - - 0 1",
    playedMoveUci = "g6g7",
    variations = List(
      EngineLine(List("f7e7", "h8g8", "e7e8"), scoreCp = 600, depth = 20),
      EngineLine(List("g6g7", "h8h7", "g7g8q"), scoreCp = -600, depth = 20)
    )
  )

  test("one played route plus the exact singleton reply closes one typed L2 occurrence"):
    val resolved = MoveReviewJudgmentOrchestrator
      .execute(singletonAnalysisContinuation)
      .getOrElse(fail("expected the singleton analysis continuation to assemble"))
    val proofRecord = resolved.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, _: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence, _) => record
    }.getOrElse(fail("expected one exact passed-pawn L2 proof"))
    val proof = proofRecord.payload.asInstanceOf[PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence]
    val eventRecord = resolved.evidenceGraph
      .record(proof.eventSource)
      .getOrElse(fail("expected the exact lower passed-pawn event"))
    val event = eventRecord.payload.asInstanceOf[PassedPawnResultEventEvidence]

    assert(resolved.evidenceGraph.proofEligible(eventRecord))
    assert(resolved.evidenceGraph.proofEligible(proofRecord))
    assert(event.onlyLegalReplyClosed)
    assertEquals(event.onlyLegalReplyMove, Some("h8h7"))
    assertEquals(event.exactResultRoutes, event.causalEpisode.resultRoutes.sortBy(_.stableKey))

    val branch = proof.publicBranches match
      case exact :: Nil => exact
      case other        => fail(s"expected one analysis-continuation branch, found ${other.size}")
    assertEquals(branch.role, "played_root_analysis_continuation")
    assertEquals(branch.rootProvenance, "observed_game_root")
    assertEquals(branch.replyMove, "h8h7")
    assertEquals(branch.steps.map(_.moveUci), List("g6g7", "h8h7", "g7g8q"))
    assertEquals(branch.steps.map(_.index), List(0, 1, 2))
    assertEquals(branch.steps.map(_.ply), List(1, 2, 3))
    assertEquals(
      branch.steps.map(_.provenance),
      List("observed_game_move", "certified_analysis_move", "certified_analysis_move")
    )

    assert(proof.publicProofPaths.nonEmpty)
    assertEquals(
      proof.publicProofPaths.map(_.pathOccurrenceId).distinct.size,
      proof.publicProofPaths.size
    )
    assert(proof.publicProofPaths.forall(_.analysisContinuationBranchId == branch.branchId))
    assertEquals(
      proof.publicProofPaths.map(_.realizationMove).distinct,
      List("g7g8q")
    )
    assert(
      proof.publicProofPaths.forall(path =>
        path.premises.map(_.role).toSet == Set("dependency", "result") &&
          path.premises.forall(_.branchId == branch.branchId) &&
          path.premises.forall(_.branchRole == "played_root_analysis_continuation")
      )
    )
    assert(!proof.publicProofPaths.flatMap(_.premises).exists(premise =>
      premise.role.contains("expected") || premise.role.contains("observed") ||
        premise.role.contains("functional") || premise.lowerKind.contains("functional")
    ))

    val closure = proof.publicClosedReplyInventory
    assertEquals(closure.legalReplyMove, "h8h7")
    assertEquals(closure.analysisContinuationBranchId, branch.branchId)
    assertEquals(closure.rootAfterFen, branch.steps.head.fenAfter)
    assert(proofRecord.parents.forall(parent => proof.lowerPremiseIds.contains(parent.id)))
    val evaluationOwnedIds = resolved.evidenceGraph.records.collect {
      case EvidenceRecord(ref, _: CandidateComparisonEvidence, _)     => ref.id
      case EvidenceRecord(ref, _: CandidateLineEvaluationEvidence, _) => ref.id
    }.toSet
    assert(
      evaluationOwnedIds.intersect(
        (eventRecord.parents.map(_.id) ++ proofRecord.parents.map(_.id) ++ proof.lowerPremiseIds).toSet
      ).isEmpty
    )
    assertEquals(
      proofRecord.parents.map(_.id).toSet,
      Set(eventRecord.ref.id, closure.issuerEvidenceId)
    )

  test("the exact dependency manifest reuses one owner and rejects a second owner"):
    val resolved = MoveReviewJudgmentOrchestrator
      .execute(singletonAnalysisContinuation)
      .getOrElse(fail("expected the singleton analysis continuation to assemble"))
    val proofRecord = resolved.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, _: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence, _) => record
    }.getOrElse(fail("expected one exact passed-pawn L2 proof"))
    val proof = proofRecord.payload.asInstanceOf[PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence]
    val eventRecord = resolved.evidenceGraph.record(proof.eventSource).get
    val comparisonRecord = resolved.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => record
    }.getOrElse(fail("expected the exact PlayedVsBest dispatch demand"))
    val inventoryRecord = proofRecord.parents
      .flatMap(resolved.evidenceGraph.record)
      .collectFirst { case record @ EvidenceRecord(_, _: StructuralDeltaEvidence, _) => record }
      .getOrElse(fail("expected the exact StructuralDelta reply inventory"))

    assertEquals(
      PassedPawnProgressRealizedAfterOnlyLegalReplyProofAssembler
        .existingProofOwnersForExactDependencies(
          resolved.evidenceGraph,
          proof.rootLine,
          eventRecord,
          inventoryRecord,
          proof.resultRoutes
        )
        .map(_.ref.id),
      List(proofRecord.ref.id)
    )

    val duplicate = proofRecord.copy(ref = proofRecord.ref.copy(id = s"${proofRecord.ref.id}-duplicate"))
    val ambiguous = resolved.evidenceGraph.add(duplicate)
    val ambiguousContext = resolved.assembly.copy(evidenceGraph = ambiguous)
    val exactInput = ExactPlayedVsBestCausalInput
      .from(ambiguousContext, comparisonRecord)
      .getOrElse(fail("expected the exact played-vs-best demand"))
    val demand = PassedPawnResultEventAssembler
      .changedDependencyDemand(ambiguousContext, exactInput)
      .getOrElse(fail("expected the exact passed-pawn changed-dependency demand"))
    val thrown = intercept[IllegalStateException] {
      PassedPawnProgressRealizedAfterOnlyLegalReplyProofAssembler.fromDemand(
        ambiguousContext,
        JudgmentProvenanceAllocator.forInput(ambiguousContext.input),
        demand
      )
    }
    assertEquals(
      thrown.getMessage,
      "one exact passed-pawn analysis-continuation dependency manifest has multiple graph owners"
    )

  test("evaluation-only changes do not change persisted event or proof identity"):
    def identities(input: RawMoveReviewInput): (String, List[String]) =
      val resolved = MoveReviewJudgmentOrchestrator.execute(input).getOrElse(fail("expected a review packet"))
      val comparisonKey = resolved.evidenceGraph.records.collectFirst {
        case EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
            if fact.kind == CandidateComparisonKind.PlayedVsBest =>
          CandidateComparisonSemanticKey.from(fact).stableKey
      }.getOrElse(fail("expected a PlayedVsBest comparison"))
      val eventRecord = resolved.evidenceGraph.records.collectFirst {
        case record @ EvidenceRecord(_, _: PassedPawnResultEventEvidence, _) => record
      }.getOrElse(fail("expected a passed-pawn result event"))
      val proofRecord = resolved.evidenceGraph.records.collectFirst {
        case record @ EvidenceRecord(_, proof: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence, _) =>
          record -> proof
      }.getOrElse(fail("expected a passed-pawn typed proof"))
      val (record, proof) = proofRecord
      comparisonKey -> List(
        eventRecord.ref.id,
        eventRecord.parents.map(_.id).sorted.mkString("|"),
        record.ref.id,
        proof.semanticId,
        proof.occurrenceId,
        proof.dependencyFingerprint,
        record.parents.map(_.id).sorted.mkString("|"),
        proof.lowerPremiseIds.mkString("|")
      )

    val changedEvaluation = singletonAnalysisContinuation.copy(
      variations = List(
        EngineLine(List("f7e7", "h8g8", "e7e8"), scoreCp = 700, depth = 20),
        EngineLine(List("g6g7", "h8h7", "g7g8q"), scoreCp = -300, depth = 20)
      )
    )
    val baseline = identities(singletonAnalysisContinuation)
    val changed = identities(changedEvaluation)
    assertNotEquals(changed._1, baseline._1)
    assertEquals(changed._2, baseline._2)

  test("a non-actionable comparison does not dispatch passed-pawn event or proof production"):
    val quietInput = singletonAnalysisContinuation.copy(
      variations = List(
        EngineLine(List("f7e7", "h8g8", "e7e8"), scoreCp = 20, depth = 20),
        EngineLine(List("g6g7", "h8h7", "g7g8q"), scoreCp = 10, depth = 20)
      )
    )
    val resolved = MoveReviewJudgmentOrchestrator.execute(quietInput).getOrElse(fail("expected a review packet"))
    val comparison = resolved.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if fact.kind == CandidateComparisonKind.PlayedVsBest => fact
    }.getOrElse(fail("expected a quiet PlayedVsBest comparison"))
    assert(!ActionablePlayedVsBestCausalProofDemand.accepts(comparison))
    assert(!resolved.evidenceGraph.records.exists(_.payload.isInstanceOf[PassedPawnResultEventEvidence]))
    assert(!resolved.evidenceGraph.records.exists(
      _.payload.isInstanceOf[PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence]
    ))

  test("more than one legal root reply remains fail closed"):
    val raw = RawMoveReviewInput(
      fen = "7k/8/8/PP6/8/8/8/4K3 w - - 0 1",
      playedMoveUci = "a5a6",
      variations = List(
        EngineLine(List("e1e2", "h8g8", "e2e3"), scoreCp = 600, depth = 20),
        EngineLine(List("a5a6", "h8g8", "a6a7"), scoreCp = -600, depth = 20)
      )
    )
    val resolved = MoveReviewJudgmentOrchestrator.execute(raw).getOrElse(fail("expected a review packet"))
    assert(!resolved.evidenceGraph.records.exists(_.payload.isInstanceOf[PassedPawnResultEventEvidence]))
    assert(!resolved.evidenceGraph.records.exists(
      _.payload.isInstanceOf[PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence]
    ))

  test("a structural passed-pawn change without a later certified analysis result publishes no L2 name"):
    val raw = RawMoveReviewInput(
      fen = "7k/8/P7/8/8/8/8/4K3 w - - 0 1",
      playedMoveUci = "a6a7",
      variations = List(
        EngineLine(List("e1e2", "h8g8", "e2e3"), scoreCp = 600, depth = 20),
        EngineLine(List("a6a7", "h8g8"), scoreCp = -600, depth = 20)
      )
    )
    val resolved = MoveReviewJudgmentOrchestrator.execute(raw).getOrElse(fail("expected a review packet"))
    assert(!resolved.evidenceGraph.records.exists(
      _.payload.isInstanceOf[PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence]
    ))
