package lila.chessjudgment.analysis.assembly

import chess.variant.Standard
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class BestChoiceProductionTest extends munit.FunSuite:

  private val ranking = List("e2e4", "d2d4", "g1f3")

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
    MoveReviewAssembler
      .assemble(prepared)
      .flatMap(EvidenceBackedJudgmentPacket.fromAssembly)
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
    assertEquals(actual.lineRootOccurrences, expected.lineRootOccurrences)
    assertEquals(
      actual.lineReplays.view.mapValues(_.replaySteps).toMap,
      expected.lineReplays.view.mapValues(_.replaySteps).toMap
    )
    assertEquals(
      actual.transitionReplays.view.mapValues(_.replaySteps).toMap,
      expected.transitionReplays.view.mapValues(_.replaySteps).toMap
    )
    assertEquals(lowerEvidenceShape(actual), lowerEvidenceShape(expected))

  test("review assembly preserves per-ply lower facts without an assessment packet"):
    val assembly = MoveReviewAssembler
      .assemble(admittedReview(ranking.head))
      .getOrElse(fail("expected the admitted lower review to assemble"))

    assertEquals(EvidenceBackedJudgmentPacket.fromAssembly(assembly), None)
    assert(assembly.lineReplays.valuesIterator.exists(_.replaySteps.nonEmpty))
    assert(assembly.positions.exists(_.role == PositionNodeRole.LineOccurrence))

  test("removing assessment candidates leaves the lower occurrence ledger unchanged"):
    val input = admittedReview(ranking.last)
    val complete = MoveReviewAssembler
      .assemble(input)
      .getOrElse(fail("expected the complete review to assemble"))
    val lowerOnly = MoveReviewAssembler
      .assemble(input.copy(assessmentCandidates = Nil, admittedRootRankingPair = None))
      .getOrElse(fail("expected the lower-only review to assemble"))

    assert(EvidenceBackedJudgmentPacket.fromAssembly(complete).nonEmpty)
    assertEquals(EvidenceBackedJudgmentPacket.fromAssembly(lowerOnly), None)
    assertSameLowerAssembly(lowerOnly, complete)

  test("a partial best-choice projection cannot suppress lower occurrences"):
    val rankingPair = AdmittedRootRankingPair
      .fromAdmittedRanking(ranking)
      .getOrElse(fail("expected the admitted root ranking"))
    val input = admittedReview(ranking.head).copy(admittedRootRankingPair = Some(rankingPair))
    val complete = MoveReviewAssembler
      .assemble(input)
      .getOrElse(fail("expected the complete best-choice review to assemble"))
    val partial = MoveReviewAssembler
      .assemble(
        input.copy(
          assessmentCandidates = input.assessmentCandidates.filter(_.role == LineNodeRole.BestReference)
        )
      )
      .getOrElse(fail("expected the partial assessment review to preserve its lower assembly"))

    assert(EvidenceBackedJudgmentPacket.fromAssembly(complete).nonEmpty)
    assertEquals(EvidenceBackedJudgmentPacket.fromAssembly(partial), None)
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

    assert(packet.primary.isInstanceOf[PlayerFacingPrimary.BestChoice])
    assertEquals(assessment.referenceTransition, None)
    assertEquals(packet.assembly.lineForRootTransition(played).map(_.ref), Some(owner.ref))
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
      val owner = assembly.legalLines.find(line => edge.evidence.line.contains(line.ref))
        .getOrElse(fail("expected the continuation's LegalLine owner"))
      val record = packet.evidenceGraph.record(edge.evidence)
        .getOrElse(fail("expected the continuation transition evidence"))
      assertEquals(record.parents, List(owner.evidence))
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
      .filter(_.evidence.line.contains(line.ref))
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

  test("a continuation structural delta cannot lose its exact line parent"):
    val packet = producedPacket(ranking.head)
    val structural = packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: StructuralDeltaEvidence, _)
          if payload.role == TransitionEdgeRole.LineContinuation => record
    }.getOrElse(fail("expected one line-continuation structural delta"))
    val forged = structural.copy(
      parents = structural.parents.filterNot(_.layer == EvidenceLayer.Line)
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
