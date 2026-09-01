package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{
  AdmittedCandidateLine,
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  EngineLine,
  PositionRuleAssessment
}

class OccurrenceExplanationProductionTest extends munit.FunSuite:

  private val rootFen =
    "r2qrbk1/1bpn1p1p/p2p1np1/Pp2p3/3PP3/2P2NNP/1PB2PP1/R1BQR1K1 w - - 1 17"
  private val vacatingMoves = List("d4d5", "c7c6", "d5c6", "b7c6", "b2b4")
  private val immediateMoves = List("b2b4", "e5d4")

  test("actionable and quiet assessments share the same occurrence-demanded proof identity"):
    val actionable = execute(
      played = "b2b4",
      vacatingScore = 600,
      immediateScore = 0
    )
    val quiet = execute(
      played = "b2b4",
      vacatingScore = 20,
      immediateScore = 10
    )

    val actionableProof = captureProof(actionable)
    val quietProof = captureProof(quiet)
    assertEquals(actionableProof.semanticId, quietProof.semanticId)
    assertEquals(actionableProof.occurrenceId, quietProof.occurrenceId)
    assertEquals(actionableProof.dependencyId, quietProof.dependencyId)
    assertEquals(actionableProof.subjectOccurrence.moveUci, "b2b4")
    assertEquals(actionableProof.immediateBranch.rootProvenance, CausalRootProvenance.ObservedGameRoot)
    assertEquals(actionableProof.vacatingBranch.rootProvenance, CausalRootProvenance.CounterfactualAnalyzedRoot)
    assertEquals(actionable.occurrenceExplanations.size, 1)
    assertEquals(quiet.occurrenceExplanations.size, 1)

  test("played best owns the observed vacating branch without a duplicate Played line"):
    val packet = execute(
      played = "d4d5",
      vacatingScore = 600,
      immediateScore = 0
    )
    val proof = captureProof(packet)

    assert(packet.primary.isInstanceOf[PlayerFacingPrimary.BestChoice])
    assertEquals(packet.candidateLines.count(_.role == LineNodeRole.Played), 0)
    assertEquals(assessmentRole(packet, proof.subjectOccurrence.line), LineNodeRole.BestReference)
    assertEquals(proof.subjectOccurrence.moveUci, "d4d5")
    assertEquals(proof.vacatingBranch.rootProvenance, CausalRootProvenance.ObservedGameRoot)
    assertEquals(proof.immediateBranch.rootProvenance, CausalRootProvenance.CounterfactualAnalyzedRoot)

  test("root and focused production retain four LegalLine owners and all four assessment projections"):
    val prepared = focusedPrepared()
    val legalRoots = prepared.legalLines.map(_.rootMove)
    val assessmentRoots = prepared.assessmentCandidates.map(_.lineRootMove)

    assertEquals(legalRoots.size, 4)
    assertEquals(legalRoots.distinct.size, 4)
    assertEquals(assessmentRoots.size, 4)
    assertEquals(assessmentRoots.toSet, legalRoots.toSet)
    assertEquals(assessmentRoots.count(EvidenceRef.sameMove(_, immediateMoves.head)), 1)

    val packet = MoveReviewJudgmentOrchestrator
      .executePreparedReview(
        prepared,
        Some(ExplanationRequest.forObservedMove(prepared))
      )
      .getOrElse(fail("expected the real root-plus-focus producer path"))
      .packet
    val proof = captureProof(packet)

    assertEquals(packet.assembly.legalLines.size, 4)
    assertEquals(packet.assembly.legalLines.map(_.ref).distinct.size, 4)
    assertEquals(packet.candidateLines.size, 4)
    assertEquals(proof.subjectOccurrence.moveUci, immediateMoves.head)
    assertSubjectBranchOwnership(proof)

  test("assessment rank and selection roles do not enter causal proof identity"):
    val vacatingBest = execute(
      played = "b2b4",
      vacatingScore = 600,
      immediateScore = 0
    )
    val immediateBest = execute(
      played = "b2b4",
      vacatingScore = 0,
      immediateScore = 600
    )
    val before = captureProof(vacatingBest)
    val after = captureProof(immediateBest)

    assert(vacatingBest.primary.isInstanceOf[PlayerFacingPrimary.MoveVerdict])
    assert(immediateBest.primary.isInstanceOf[PlayerFacingPrimary.BestChoice])
    assertEquals(assessmentRole(vacatingBest, before.subjectOccurrence.line), LineNodeRole.Played)
    assertEquals(assessmentRole(immediateBest, after.subjectOccurrence.line), LineNodeRole.BestReference)
    assertNotEquals(
      assessmentRole(vacatingBest, before.vacatingBranch.line),
      assessmentRole(immediateBest, after.vacatingBranch.line)
    )
    assertEquals(before.subjectOccurrence.lineOwnerEvidenceId, after.subjectOccurrence.lineOwnerEvidenceId)
    assertEquals(before.subjectOccurrence.transitionEvidenceId, after.subjectOccurrence.transitionEvidenceId)
    assertEquals(before.vacatingBranch.lineOwnerEvidenceId, after.vacatingBranch.lineOwnerEvidenceId)
    assertEquals(before.vacatingBranch.rootTransitionEvidenceId, after.vacatingBranch.rootTransitionEvidenceId)
    assertEquals(before.immediateBranch.lineOwnerEvidenceId, after.immediateBranch.lineOwnerEvidenceId)
    assertEquals(before.immediateBranch.rootTransitionEvidenceId, after.immediateBranch.rootTransitionEvidenceId)
    assertNotEquals(
      assessmentEvidenceId(vacatingBest, before.vacatingBranch.line),
      assessmentEvidenceId(immediateBest, after.vacatingBranch.line)
    )
    assertSubjectBranchOwnership(before)
    assertSubjectBranchOwnership(after)
    assertEquals(before.semanticId, after.semanticId)
    assertEquals(before.occurrenceId, after.occurrenceId)
    assertEquals(before.dependencyId, after.dependencyId)
    assertEquals(before.proofPaths.map(_.pathOccurrenceId), after.proofPaths.map(_.pathOccurrenceId))

  test("duplicate root owners with different full replays fail closed at admission"):
    assertEquals(
      MoveReviewInputAdmission.admit(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = vacatingMoves.head,
          variations = List(
            EngineLine(vacatingMoves, scoreCp = 600, depth = 24),
            EngineLine(vacatingMoves.take(1), scoreCp = 500, depth = 24)
          )
        )
      ),
      None
    )

  test("non-comparison-ready root evaluation is not admitted through a test-only LegalLine path"):
    val history = rootHistory
    val rootMoves = additionalRootMoves(history)
    val result = MoveReviewInputAdmission.prepareFocusedMoveReview(
      history,
      immediateMoves.head,
      resultingFen(history, immediateMoves.head),
      PositionRuleAssessment.assess(history),
      List(
        admittedLine(history, vacatingMoves, 600),
        admittedLine(history, List(rootMoves.head), -100, depth = 7),
        admittedLine(history, List(rootMoves(1)), -200)
      ),
      List(
        admittedLine(history, vacatingMoves, 600),
        admittedLine(history, immediateMoves, 0)
      )
    )

    assertEquals(result, None)

  test("one missing LegalLine root binding closes the entire explanation demand"):
    val context = EvidenceFactAssembler
      .assemble(review("b2b4", 600, 0))
      .getOrElse(fail("expected the exact LegalLine inventory"))
    val missing = context.certifiedRootOccurrences
      .flatMap(_.find(!_.isObserved))
      .getOrElse(fail("expected one analyzed sibling root occurrence"))
    val broken = context.copy(
      transitionReplays = context.transitionReplays - missing.transition.evidence.id,
      lineRootOccurrences = context.lineRootOccurrences - missing.line
    )

    assertEquals(broken.certifiedRootOccurrences, None)
    assertEquals(
      OccurrenceExplanationDemand.resolve(
        broken,
        ExplanationRequest.forObservedMove(broken.input)
      ),
      None
    )
    assertEquals(
      broken.evidenceGraph.records.count(_.payload.isInstanceOf[OccurrenceExplanationCauseEvidence]),
      0
    )

  test("no explanation demand performs no typed family or occurrence Cause production"):
    val raw = review("b2b4", 600, 0)
    val prepared = MoveReviewInputAdmission.admit(raw).getOrElse(fail("expected admitted review"))
    val packet = MoveReviewJudgmentOrchestrator
      .executePreparedReview(prepared, None)
      .getOrElse(fail("assessment should close without an explanation demand"))
      .packet

    assertEquals(captureProofRecords(packet).size, 0)
    assertEquals(packet.occurrenceExplanations.size, 0)

  test("an available line without the exact reply/absence join is not guessed into a proof"):
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = "b2b4",
          variations = List(
            EngineLine(vacatingMoves, scoreCp = 600, depth = 24),
            EngineLine(List("b2b4", "f8g7"), scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("assessment should survive a missing exact L2 join"))

    assertEquals(captureProofRecords(packet).size, 0)
    assertEquals(packet.occurrenceExplanations.size, 0)

  test("transposed histories share one proposition without merging occurrences or proof paths"):
    val sharedRoot = rootFen.replace(" 1 17", " 4 17")
    val directHistory = CanonicalPositionHistory
      .from(sharedRoot, Nil, sharedRoot)
      .toOption
      .getOrElse(fail("expected the direct root occurrence"))
    val transpositionStart = rootFen.replace(" 1 17", " 0 15")
    val transposedHistory = CanonicalPositionHistory
      .from(
        transpositionStart,
        List("f3h2", "f6h5", "h2f3", "h5f6"),
        sharedRoot
      )
      .toOption
      .getOrElse(fail("expected the repeated board through its distinct history"))

    assertEquals(directHistory.currentFen, transposedHistory.currentFen)
    assertEquals(directHistory.currentPly, transposedHistory.currentPly)
    val direct = captureProof(executeHistory(directHistory))
    val transposed = captureProof(executeHistory(transposedHistory))

    assertEquals(direct.semanticId, transposed.semanticId)
    assertNotEquals(direct.subjectOccurrence.occurrenceId, transposed.subjectOccurrence.occurrenceId)
    assertNotEquals(
      direct.subjectOccurrence.lineOwnerEvidenceId,
      transposed.subjectOccurrence.lineOwnerEvidenceId
    )
    assertNotEquals(
      direct.subjectOccurrence.transitionEvidenceId,
      transposed.subjectOccurrence.transitionEvidenceId
    )
    assertNotEquals(direct.occurrenceId, transposed.occurrenceId)
    assertNotEquals(direct.dependencyId, transposed.dependencyId)
    assertEquals(direct.proofPaths.size, transposed.proofPaths.size)
    assert(
      direct.proofPaths.map(_.pathOccurrenceId).toSet
        .intersect(transposed.proofPaths.map(_.pathOccurrenceId).toSet)
        .isEmpty
    )
    assertSubjectBranchOwnership(direct)
    assertSubjectBranchOwnership(transposed)

  private def execute(
      played: String,
      vacatingScore: Int,
      immediateScore: Int
  ): EvidenceBackedJudgmentPacket =
    val admitted = MoveReviewInputAdmission
      .admit(review(played, vacatingScore, immediateScore))
      .getOrElse(fail("expected admitted review"))
    val rankingPair = AdmittedRootRankingPair
      .fromAdmittedRanking(admitted.assessmentCandidates.sortBy(_.rank).flatMap(_.rootMove))
      .getOrElse(fail("expected admitted root ranking"))
    val prepared = admitted.copy(admittedRootRankingPair = Some(rankingPair))

    MoveReviewJudgmentOrchestrator
      .executePreparedReview(
        prepared,
        Some(ExplanationRequest.forObservedMove(prepared))
      )
      .getOrElse(fail("expected the producer pipeline to close"))
      .packet

  private def review(
      played: String,
      vacatingScore: Int,
      immediateScore: Int
  ): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = rootFen,
      playedMoveUci = played,
      variations = List(
        EngineLine(vacatingMoves, scoreCp = vacatingScore, depth = 24),
        EngineLine(immediateMoves, scoreCp = immediateScore, depth = 24)
      )
    )

  private def executeHistory(
      history: CanonicalPositionHistory
  ): EvidenceBackedJudgmentPacket =
    val reserved = Set(vacatingMoves.head, immediateMoves.head)
    val thirdMove = history.currentLegalMoveUcis
      .find(move => !reserved(EvidenceRef.normalizeMove(move)))
      .getOrElse(fail("expected a third legal root move"))
    val rootLines = List(
      admittedLine(history, vacatingMoves, 600),
      admittedLine(history, immediateMoves, 0),
      admittedLine(history, List(thirdMove), -600)
    )
    val resultingFen = history
      .admitToFirstAutomaticTerminal(List(immediateMoves.head))
      .toOption
      .map(_.resultingHistory.currentFen)
      .getOrElse(fail("expected the played continuation"))
    val prepared = MoveReviewInputAdmission
      .prepareFocusedMoveReview(
        history,
        immediateMoves.head,
        resultingFen,
        PositionRuleAssessment.assess(history),
        rootLines,
        Nil
      ) match
      case Some(PreparedFocusedPositionCommentary.MoveReview(review)) => review.preparedInput
      case other => fail(s"expected a focused move review, found $other")

    MoveReviewJudgmentOrchestrator
      .executePreparedReview(
        prepared,
        Some(ExplanationRequest.forObservedMove(prepared))
      )
      .map(_.packet)
      .getOrElse(fail("expected the transposition-aware producer pipeline"))

  private def focusedPrepared(): AdmittedMoveReviewInput =
    val history = rootHistory
    val rootMoves = additionalRootMoves(history)
    MoveReviewInputAdmission
      .prepareFocusedMoveReview(
        history,
        immediateMoves.head,
        resultingFen(history, immediateMoves.head),
        PositionRuleAssessment.assess(history),
        List(
          admittedLine(history, vacatingMoves, 600),
          admittedLine(history, List(rootMoves.head), -100),
          admittedLine(history, List(rootMoves(1)), -200)
        ),
        List(
          admittedLine(history, vacatingMoves, 600),
          admittedLine(history, immediateMoves, 0)
        )
      ) match
      case Some(PreparedFocusedPositionCommentary.MoveReview(review)) => review.preparedInput
      case other => fail(s"expected a root-plus-focused-played review, found $other")

  private def rootHistory: CanonicalPositionHistory =
    CanonicalPositionHistory
      .from(rootFen, Nil, rootFen)
      .toOption
      .getOrElse(fail("expected the canonical root history"))

  private def additionalRootMoves(history: CanonicalPositionHistory): List[String] =
    val reserved = Set(vacatingMoves.head, immediateMoves.head)
    val exact = history.currentLegalMoveUcis.filterNot(move => reserved(EvidenceRef.normalizeMove(move)))
    if exact.size >= 2 then exact.take(2)
    else fail("expected two additional legal root moves")

  private def resultingFen(history: CanonicalPositionHistory, move: String): String =
    history
      .admitToFirstAutomaticTerminal(List(move))
      .toOption
      .map(_.resultingHistory.currentFen)
      .getOrElse(fail(s"expected the legal root move $move"))

  private def admittedLine(
      history: CanonicalPositionHistory,
      moves: List[String],
      scoreCp: Int,
      depth: Int = 24
  ): AdmittedCandidateLine =
    val evaluation = CandidateLineEvaluation.EngineSearch(
      EngineLine(moves, scoreCp = scoreCp, depth = depth)
    )
    history
      .admitToFirstAutomaticTerminal(moves)
      .toOption
      .flatMap(_.bind(evaluation))
      .getOrElse(fail(s"expected one admitted candidate line for ${moves.mkString(" ")}"))

  private def captureProof(
      packet: EvidenceBackedJudgmentPacket
  ): CaptureExclusionMoveOrderEvidence =
    captureProofRecords(packet) match
      case record :: Nil => record.payload.asInstanceOf[CaptureExclusionMoveOrderEvidence]
      case other         => fail(s"expected one capture-exclusion proof, found ${other.size}")

  private def captureProofRecords(
      packet: EvidenceBackedJudgmentPacket
  ): List[EvidenceRecord] =
    packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: CaptureExclusionMoveOrderEvidence, _) => record
    }

  private def assertSubjectBranchOwnership(
      proof: CaptureExclusionMoveOrderEvidence
  ): Unit =
    val exact = List(proof.vacatingBranch, proof.immediateBranch).filter(branch =>
      branch.lineOwnerEvidenceId == proof.subjectOccurrence.lineOwnerEvidenceId &&
        branch.rootTransitionEvidenceId == proof.subjectOccurrence.transitionEvidenceId
    )
    assertEquals(exact.size, 1)

  private def assessmentRole(
      packet: EvidenceBackedJudgmentPacket,
      line: LineNodeRef
  ): LineNodeRole =
    packet.candidateLines.filter(_.ref == line) match
      case candidate :: Nil => candidate.role
      case other            => fail(s"expected one assessment projection for ${line.id}, found ${other.size}")

  private def assessmentEvidenceId(
      packet: EvidenceBackedJudgmentPacket,
      line: LineNodeRef
  ): String =
    packet.evidenceGraph.records.collect {
      case EvidenceRecord(ref, CandidateLineEvaluationEvidence(payloadLine, _), _)
          if payloadLine == line => ref.id
    } match
      case id :: Nil => id
      case other     => fail(s"expected one assessment fact for ${line.id}, found ${other.size}")
