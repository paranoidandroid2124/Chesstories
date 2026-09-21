package lila.chessjudgment.analysis.assembly

import chess.variant.Standard
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, CanonicalPositionHistory, PositionRuleAssessment }

class BestChoiceProductionTest extends munit.FunSuite:

  private val ranking = List("e2e4", "d2d4", "g1f3")

  test("a one-ply nonterminal played PV retains its role and produces a comparison"):
    val initial = Standard.initialFen.value
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
    val lines = List(
      EngineLine(List("e2e4"), 30, None, 16),
      EngineLine(List("d2d4"), 20, None, 16)
    )
    val direct = MoveReviewInputAdmission.admit(RawMoveReviewInput(initial, "d2d4", lines)).get
    val roots = lines.map(line => history.admitToFirstAutomaticTerminal(line.moves).toOption.get
      .bind(CandidateLineEvaluation.EngineSearch(line)).get)
    val focused = MoveReviewInputAdmission.prepareFocusedMoveReview(
      history, "d2d4", history.extend(List("d2d4")).toOption.get.currentFen,
      PositionRuleAssessment.assess(history), roots, Nil
    ).get match
      case PreparedFocusedPositionCommentary.MoveReview(review) => review.preparedInput
      case other => fail(s"expected a move review, got $other")

    List(direct, focused).foreach { input =>
      val played = input.assessmentCandidates.filter(_.rootMove.contains("d2d4"))
      assertEquals(played.map(_.role), List(LineNodeRole.Played))
      val assembly = MoveReviewAssembler.assemble(input)
      assert(assembly.line(LineNodeRole.Played).nonEmpty)
      val packet = EvidenceBackedJudgmentPacket.fromAssembly(assembly).get
      assertEquals(soleComparison(packet).kind, CandidateComparisonKind.PlayedVsBest)
    }

  test("draw-claim preference uses accepted focused scores without discarding other root observations"):
    val initial = Standard.initialFen.value
    val cycle = List("g1f3", "g8f6", "f3g1", "f6g8")
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
      .extend(cycle ++ cycle).toOption.get
    def admit(move: String, score: Int) =
      history.admitToFirstAutomaticTerminal(List(move)).toOption.get
        .bind(CandidateLineEvaluation.EngineSearch(EngineLine(List(move), score, None, 16))).get
    // The played move can overtake the reference in Focus. Conversely, a
    // non-repeated root move can still provide a non-losing observation.
    List(
      (-30, -40, 31, 10, false),
      (-30, -40, -31, 10, false),
      (30, 20, -31, -40, false),
      (-30, -40, -31, -20, true)
    ).foreach { case (rootBest, rootOther, focusedBest, focusedPlayed, expectDraw) =>
      val result = MoveReviewInputAdmission.prepareFocusedMoveReview(
        history, "a2a3", history.extend(List("a2a3")).toOption.get.currentFen,
        PositionRuleAssessment.assess(history),
        List(admit("e2e4", rootBest), admit("d2d4", rootOther)),
        List(admit("e2e4", focusedBest), admit("a2a3", focusedPlayed))
      ).get
      result match
        case PreparedFocusedPositionCommentary.PositionAction(PlayerFacingPositionAction.DominantDrawClaim(claims)) =>
          assert(expectDraw)
          assert(claims.nonEmpty)
        case PreparedFocusedPositionCommentary.MoveReview(review) =>
          assert(!expectDraw)
          assert(review.drawClaims.nonEmpty)
          assertEquals(review.preparedInput.evaluationObservations.size, 4)
          assertEquals(review.preparedInput.referenceCandidate.get.evaluation.engineLine.get.scoreCp, focusedBest)
        case other => fail(s"unexpected position action: $other")
    }

  test("focused preparation accepts reconstructed exact history but rejects a different path to the same FEN"):
    val initial = Standard.initialFen.value
    val prefix = List("g1f3", "g8f6", "f3g1", "f6g8")
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(prefix).toOption.get
    val reconstructed = CanonicalPositionHistory.from(initial, prefix, history.currentFen).toOption.get
    val roots = List(
      EngineLine(List("e2e4", "e7e5", "g1f3"), 30, None, 16),
      EngineLine(List("d2d4", "d7d5", "g1f3"), 20, None, 16)
    ).map(line => reconstructed.admitToFirstAutomaticTerminal(line.moves).toOption.get
      .bind(CandidateLineEvaluation.EngineSearch(line)).get)
    val played = ranking.head
    val resultingFen = history.extend(List(played)).toOption.get.currentFen
    def prepare(owner: CanonicalPositionHistory) = MoveReviewInputAdmission.prepareFocusedMoveReview(
      owner, played, resultingFen, PositionRuleAssessment.assess(owner), roots, Nil)

    val review = prepare(history) match
      case Some(PreparedFocusedPositionCommentary.MoveReview(value)) => value
      case other => fail(s"expected preparation across independent exact-history owners, got $other")
    assertEquals(review.bestMoveUci, played)
    assertEquals(review.playedRootRank, Some(1))
    assertEquals(review.preparedInput.positionHistory.movePrefixUci, prefix)
    assertEquals(review.preparedInput.legalLines.map(_.moves), roots.map(_.evaluation.moves))

    val otherPrefix = List("b1c3", "b8c6", "c3b1", "c6b8")
    val otherHistory = CanonicalPositionHistory.from(initial, otherPrefix, history.currentFen).toOption.get
    assertEquals(otherHistory.currentFen, history.currentFen)
    assertEquals(otherHistory.extend(List(played)).toOption.get.currentFen, resultingFen)
    assertEquals(prepare(otherHistory), None)

  test("equal replay paths retain separate root and focused evaluation observations"):
    val history = CanonicalPositionHistory.from(Standard.initialFen.value, Nil, Standard.initialFen.value).toOption.get
    def admit(moves: List[String], score: Int) =
      history.admitToFirstAutomaticTerminal(moves).toOption.get
        .bind(CandidateLineEvaluation.EngineSearch(EngineLine(moves, score, None, 16))).get
    val bestMoves = List("e2e4", "e7e5", "g1f3")
    val roots = List(admit(bestMoves, 30), admit(List("d2d4", "d7d5", "g1f3"), 20))
    val focus = List(admit(bestMoves, 31), admit(List("a2a3", "e7e5", "g1f3"), 40))
    val prepared = MoveReviewInputAdmission.prepareFocusedMoveReview(
      history, "a2a3", history.extend(List("a2a3")).toOption.get.currentFen,
      PositionRuleAssessment.assess(history), roots, focus
    ).get match
      case PreparedFocusedPositionCommentary.MoveReview(review) => review.preparedInput
      case _ => fail("expected an ordinary focused review")
    assertEquals(prepared.legalLines.size, 3)
    assertEquals(prepared.evaluationObservations.size, 4)
    val repeated = prepared.evaluationObservations.filter(_.evaluation.moves == bestMoves)
    assertEquals(repeated.map(observation => observation.source -> observation.evaluation.engineLine.get.scoreCp),
      List(EvaluationSource.RootSearch -> 30, EvaluationSource.FocusComparison -> 31))
    val assembled = MoveReviewAssembler.assemble(prepared)
    val reference = assembled.line(LineNodeRole.BestReference).get
    assertEquals(reference.ref.rootMove, "e2e4")
    assertEquals(reference.evaluation.engineLine.get.scoreCp, 31)
    // A focused observation can reverse the earlier ranking without erasing
    // either report or rejecting the independent occurrence assembly.
    assert(EvidenceBackedJudgmentPacket.fromAssembly(assembled).nonEmpty)

  private def admittedReview(playedMove: String): AdmittedMoveReviewInput =
    MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = Standard.initialFen.value,
          playedMoveUci = playedMove,
          variations = List(
            EngineLine(List("e2e4", "e7e5", "g1f3"), 30, None, 16),
            EngineLine(List("d2d4", "d7d5", "g1f3"), 20, None, 16),
            EngineLine(List("g1f3", "g8f6", "d2d4"), 10, None, 16)
          )
        )
      )
      .getOrElse(fail("expected exact input admission"))

  private def producedPacket(playedMove: String): EvidenceBackedJudgmentPacket =
    val admitted = admittedReview(playedMove)
    val rankingPair = AdmittedRootRankingPair
      .fromAdmittedRanking(ranking)
      .getOrElse(fail("expected the admitted root ranking"))
    val prepared = admitted.copy(admittedRootRankingPair = Some(rankingPair))

    assertEquals(prepared.assessmentCandidates.flatMap(_.rootMove).toSet, ranking.toSet)
    EvidenceBackedJudgmentPacket
      .fromAssembly(MoveReviewAssembler.assemble(prepared))
      .getOrElse(fail("expected the lower assembly to project BestChoice"))

  private val assessmentLayers = Set(
    EvidenceLayer.Eval,
    EvidenceLayer.CandidateComparison,
    EvidenceLayer.RelativeAssessment
  )

  private def lowerEvidenceShape(
      assembly: JudgmentAssemblyContext
  ): List[(EvidenceRef, List[EvidenceRef])] =
    assembly.evidenceGraph.records
      .filterNot(record => assessmentLayers(record.ref.layer))
      .map(record => record.ref -> record.parents)

  private def assertSameLowerAssembly(
      actual: JudgmentAssemblyContext,
      expected: JudgmentAssemblyContext
  ): Unit =
    assertEquals(actual.positions, expected.positions)
    assertEquals(actual.legalLines, expected.legalLines)
    assertEquals(actual.transitions, expected.transitions)
    assertEquals(actual.lineTransitionIds, expected.lineTransitionIds)
    assertEquals(
      actual.lineReplays.view.mapValues(_.replaySteps).toMap,
      expected.lineReplays.view.mapValues(_.replaySteps).toMap
    )
    assertEquals(
      actual.transitionReplays.view.mapValues(_.replaySteps).toMap,
      expected.transitionReplays.view.mapValues(_.replaySteps).toMap
    )
    assertEquals(lowerEvidenceShape(actual), lowerEvidenceShape(expected))

  test("an admitted line cannot be rebound from another root occurrence"):
    val input = admittedReview(ranking.head)
    val foreign = MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 2",
          playedMoveUci = ranking.head,
          variations = List(EngineLine(List("e2e4", "e7e5"), 30, None, 16))
        )
      )
      .getOrElse(fail("expected a second exact root occurrence"))
      .legalLines.head
    val rebound = input.legalLines.map(line =>
      if EvidenceRef.sameMove(line.rootMove, foreign.rootMove) then foreign else line
    )

    intercept[IllegalArgumentException] {
      input.copy(
        legalLines = rebound,
        assessmentCandidates = Nil,
        admittedRootRankingPair = None
      )
    }

  test("removing assessment candidates leaves the lower occurrence ledger unchanged"):
    val input = admittedReview(ranking.last)
    val complete = MoveReviewAssembler.assemble(input)
    val lowerOnly = MoveReviewAssembler.assemble(
      input.copy(assessmentCandidates = Nil, admittedRootRankingPair = None)
    )

    assert(EvidenceBackedJudgmentPacket.fromAssembly(complete).nonEmpty)
    assertEquals(EvidenceBackedJudgmentPacket.fromAssembly(lowerOnly).map(_.primary), Some(None))
    assertSameLowerAssembly(lowerOnly, complete)

  test("a partial best-choice projection cannot suppress lower occurrences"):
    val rankingPair = AdmittedRootRankingPair
      .fromAdmittedRanking(ranking)
      .getOrElse(fail("expected the admitted root ranking"))
    val input = admittedReview(ranking.head).copy(admittedRootRankingPair = Some(rankingPair))
    val complete = MoveReviewAssembler.assemble(input)
    val partial = MoveReviewAssembler
      .assemble(
        input.copy(
          assessmentCandidates = input.assessmentCandidates.filter(_.role == LineNodeRole.BestReference)
        )
      )

    assert(EvidenceBackedJudgmentPacket.fromAssembly(complete).nonEmpty)
    assertEquals(EvidenceBackedJudgmentPacket.fromAssembly(partial).map(_.primary), Some(None))
    assertSameLowerAssembly(partial, complete)

  private def soleAssessmentRecord(
      packet: EvidenceBackedJudgmentPacket
  ): (RelativeMoveAssessment, List[EvidenceRef]) =
    packet.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeAssessmentEvidence(assessment), parents) => assessment -> parents
    } match
      case record :: Nil => record
      case records       => fail(s"expected one produced relative assessment, found ${records.size}")

  private def soleComparison(packet: EvidenceBackedJudgmentPacket): CandidateComparisonFact =
    packet.evidenceGraph.records.collect {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) => fact
    } match
      case fact :: Nil => fact
      case facts       => fail(s"expected one quantitative primary comparison, found ${facts.size}")

  test("the observed best move reuses its sole LegalLine owner through packet production"):
    val packet = producedPacket(ranking.head)
    val played = packet.assembly.playedTransition.getOrElse(fail("expected the observed transition"))
    val owner = packet.candidateLines
      .find(line => EvidenceRef.sameMove(line.ref.rootMove, played.moveUci))
      .getOrElse(fail("expected the observed move's unique LegalLine owner"))
    val (assessment, parents) = soleAssessmentRecord(packet)

    assert(packet.primary.exists(_.isInstanceOf[PlayerFacingPrimary.BestChoice]))
    assertEquals(assessment.referenceTransition, None)
    assertEquals(packet.assembly.lineRootOccurrence(owner.ref), Some(played))
    assertEquals(assessment.reference, assessment.candidate)
    assertEquals(soleComparison(packet).kind, CandidateComparisonKind.BestVsSecond)
    assertEquals(parents.count(_ == owner.lineEvidence), 1)
    assertEquals(parents.map(_.id).distinct.size, parents.size)

  test("every admitted replay ply owns one graph position and transition occurrence"):
    val packet = producedPacket(ranking.head)
    val assembly = packet.assembly
    val continuationEdges = assembly.transitions.filter(_.role == TransitionEdgeRole.LineContinuation)
    val continuationPositions = assembly.positions.filter(_.role == PositionNodeRole.LineOccurrence)
    val positionOccurrenceRecords = packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, _: PositionOccurrenceEvidence, _) => record
    }
    val replayPlyCount = assembly.lineReplays.valuesIterator.map(_.replaySteps.size).sum
    val continuationPlyCount = replayPlyCount - assembly.legalLines.size

    assertEquals(assembly.transitions.size, replayPlyCount)
    assertEquals(assembly.transitionReplays.size, replayPlyCount)
    assertEquals(continuationEdges.size, continuationPlyCount)
    assertEquals(continuationPositions.size, continuationPlyCount)
    continuationEdges.foreach { edge =>
      val record = packet.evidenceGraph.record(edge.evidence)
        .getOrElse(fail("expected the continuation transition evidence"))
      val predecessor = positionOccurrenceRecords.find(_.ref.position == edge.from)
        .getOrElse(fail("expected the historical predecessor"))
      assertEquals(record.parents, List(predecessor.ref))
      assert(packet.evidenceGraph.proofEligible(record))
      val destination = positionOccurrenceRecords.filter(_.ref.position == edge.to) match
        case exact :: Nil => exact
        case records      => fail(s"expected one destination occurrence, found ${records.size}")
      assertEquals(destination.ref.line, edge.evidence.line)
      assertEquals(destination.parents, List(edge.evidence))
      assert(packet.evidenceGraph.proofEligible(destination))
    }

  test("an unrelated malformed suffix occurrence is only locally proof-ineligible"):
    val packet = producedPacket(ranking.head)
    val line = packet.assembly.legalLines.head
    val continuations = packet.assembly.transitions
      .filter(edge => packet.assembly.lineTransitionIds(line.ref).contains(edge.evidence.id) &&
        edge.role == TransitionEdgeRole.LineContinuation)
      .sortBy(_.to.ply)
    val List(first, second) = continuations: @unchecked
    val occurrence = packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(ref, _: PositionOccurrenceEvidence, _)
          if ref.position == first.to => record
    } match
      case exact :: Nil => exact
      case records      => fail(s"expected one suffix position occurrence, found ${records.size}")
    val forged = occurrence.copy(parents = List(second.evidence))
    val graph = TypedEvidenceGraph.empty.addAll(
      packet.evidenceGraph.records.map(record =>
        if record.ref == forged.ref then forged else record
      )
    )

    assert(!graph.proofEligible(forged))
    val rebuilt = EvidenceBackedJudgmentPacket
      .fromAssembly(packet.assembly.copy(evidenceGraph = graph))
      .getOrElse(fail("expected the selected assessment chain to remain projectable"))
    assertEquals(rebuilt.primary, packet.primary)

  test("assessment siblings stay local and the selected transition remains exact"):
    val packet = producedPacket(ranking.head)
    val (assessmentRecord, assessment) = packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, RelativeAssessmentEvidence(value), _) => record -> value
    } match
      case record :: Nil => record
      case records       => fail(s"expected one produced assessment, found ${records.size}")
    val siblingCandidate = packet.candidateLines
      .find(_.rank == 3)
      .getOrElse(fail("expected an unrelated third-ranked candidate"))
    val siblingEvidence = assessment.evidence.copy(
      id = s"${assessment.evidence.id}:candidate-sibling",
      line = Some(siblingCandidate.ref)
    )
    val siblingAssessment = assessment.copy(
      candidate = siblingCandidate,
      evidence = siblingEvidence
    )
    val sibling = assessmentRecord.copy(
      ref = siblingEvidence,
      payload = RelativeAssessmentEvidence(siblingAssessment),
      parents = siblingAssessment.canonicalParents.getOrElse(fail("expected canonical sibling parents"))
    )
    val graph = packet.evidenceGraph.add(sibling)

    assert(!graph.proofEligible(sibling))
    val rebuilt = EvidenceBackedJudgmentPacket
      .fromAssembly(packet.assembly.copy(evidenceGraph = graph))
      .getOrElse(fail("expected the selected assessment to survive beside its sibling"))
    assertEquals(rebuilt.primary, packet.primary)

    val otherDestination = packet.assembly.transitions
      .find(edge => edge.role != TransitionEdgeRole.LineContinuation && edge.to != assessment.played.to)
      .map(_.to)
      .getOrElse(fail("expected another root transition destination"))
    val alteredAssessment = assessment.copy(
      played = assessment.played.copy(to = otherDestination)
    )
    val alteredRecord = assessmentRecord.copy(
      payload = RelativeAssessmentEvidence(alteredAssessment),
      parents = alteredAssessment.canonicalParents.getOrElse(fail("expected canonical selected parents"))
    )
    val alteredGraph = TypedEvidenceGraph.empty.addAll(
      packet.evidenceGraph.records.map(record =>
        if record.ref == alteredRecord.ref then alteredRecord else record
      )
    )
    val alteredAssembly = packet.assembly.copy(
      transitions = packet.assembly.transitions.map(edge =>
        if edge == assessment.played then alteredAssessment.played else edge
      ),
      evidenceGraph = alteredGraph
    )
    val playedRecord = alteredGraph
      .record(alteredAssessment.played.evidence)
      .getOrElse(fail("expected the selected transition evidence"))

    assert(alteredGraph.proofEligible(playedRecord))
    assert(!alteredAssessment.played.matches(playedRecord))
    assert(!alteredGraph.proofEligible(alteredRecord))
    assertEquals(EvidenceBackedJudgmentPacket.fromAssembly(alteredAssembly), None)

  test("a comparison belongs to the mover at its root occurrence"):
    val packet = producedPacket(ranking.head)
    val comparisonRecord = packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _) => record
    } match
      case record :: Nil => record
      case records       => fail(s"expected one comparison, found ${records.size}")
    val fact = comparisonRecord.payload match
      case CandidateComparisonEvidence(value) => value
      case _                                  => fail("expected comparison evidence")
    val reference = packet.candidateLines.find(_.ref == fact.referenceLine)
      .getOrElse(fail("expected the reference endpoint"))
    val candidate = packet.candidateLines.find(_.ref == fact.candidateLine)
      .getOrElse(fail("expected the candidate endpoint"))
    val wrongMover = if fact.comparison.mover.white then chess.Black else chess.White
    val forgedFact = CandidateComparisonFact
      .fromLines(fact.kind, wrongMover, reference, candidate)
      .getOrElse(fail("expected a self-consistent opposite-mover comparison"))
    val forged = comparisonRecord.copy(payload = CandidateComparisonEvidence(forgedFact))
    val graph = TypedEvidenceGraph.empty.addAll(
      packet.evidenceGraph.records.map(record =>
        if record.ref == forged.ref then forged else record
      )
    )

    assert(!graph.proofEligible(forged))

  test("a continuation structural delta cannot lose its historical position parents"):
    val packet = producedPacket(ranking.head)
    val structural = packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: StructuralDeltaEvidence, _)
          if payload.role == TransitionEdgeRole.LineContinuation => record
    }.getOrElse(fail("expected one line-continuation structural delta"))
    val forged = structural.copy(
      parents = structural.parents.filterNot(_.layer == EvidenceLayer.PositionOccurrence)
    )
    val graph = TypedEvidenceGraph.empty.addAll(
      packet.evidenceGraph.records.map(record =>
        if record.ref == forged.ref then forged else record
      )
    )

    assert(!graph.proofEligible(forged))

  test("a third-ranked played move produces only its primary assessment comparison"):
    val packet = producedPacket(ranking(2))
    val (assessment, parents) = soleAssessmentRecord(packet)

    assertNotEquals(assessment.reference, assessment.candidate)
    assertEquals(soleComparison(packet).kind, CandidateComparisonKind.PlayedVsBest)
    assertEquals(parents.count(_ == assessment.reference.lineEvidence), 1)
    assertEquals(parents.count(_ == assessment.candidate.lineEvidence), 1)
    assertEquals(parents.map(_.id).distinct.size, parents.size)
