package lila.chessjudgment.analysis.assembly

import chess.variant.Standard
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class BestChoiceProductionTest extends munit.FunSuite:

  private val ranking = List("e2e4", "d2d4", "g1f3")

  private def producedPacket(playedMove: String): EvidenceBackedJudgmentPacket =
    val admitted = MoveReviewInputAdmission
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
    val rankingPair = AdmittedRootRankingPair
      .fromAdmittedRanking(ranking)
      .getOrElse(fail("expected the admitted root ranking"))
    val prepared = admitted.copy(admittedRootRankingPair = Some(rankingPair))

    assertEquals(prepared.assessmentCandidates.flatMap(_.rootMove).toSet, ranking.toSet)
    MoveReviewJudgmentOrchestrator
      .executePreparedReview(prepared)
      .getOrElse(fail("expected the sole production pipeline to close BestChoice"))
      .packet

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
    assertEquals(packet.assembly.referenceTransition, None)
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

  test("a continuation transition cannot lose its certified line parent"):
    val packet = producedPacket(ranking.head)
    val continuationEdge = packet.assembly.transitions
      .find(_.role == TransitionEdgeRole.LineContinuation)
      .getOrElse(fail("expected one line-continuation transition"))
    val continuation = packet.evidenceGraph.record(continuationEdge.evidence)
      .getOrElse(fail("expected its transition record"))
    val forged = continuation.copy(parents = Nil)
    val forgedGraph = TypedEvidenceGraph.empty.addAll(
      packet.evidenceGraph.records.map(record =>
        if record.ref == forged.ref then forged else record
      )
    )

    assert(!forgedGraph.proofEligible(forged))
    assertEquals(
      EvidenceBackedJudgmentPacket.fromAssembly(
        packet.assembly.copy(evidenceGraph = forgedGraph)
      ),
      None
    )

  test("a suffix position cannot borrow another continuation from the same line"):
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
    assertEquals(
      EvidenceBackedJudgmentPacket.fromAssembly(
        packet.assembly.copy(evidenceGraph = graph)
      ),
      None
    )

  test("an independent lower transition cannot suppress an otherwise closed packet"):
    val packet = producedPacket(ranking.head)
    val continuationEdge = packet.assembly.transitions
      .find(_.role == TransitionEdgeRole.LineContinuation)
      .getOrElse(fail("expected one line-continuation transition"))
    val continuation = packet.evidenceGraph.record(continuationEdge.evidence)
      .getOrElse(fail("expected its transition record"))
    val duplicate = continuation.copy(
      ref = continuation.ref.copy(id = s"${continuation.ref.id}:orphan-duplicate")
    )
    val graph = packet.evidenceGraph.add(duplicate)

    assert(graph.proofEligible(duplicate))
    val rebuilt = EvidenceBackedJudgmentPacket
      .fromAssembly(packet.assembly.copy(evidenceGraph = graph))
      .getOrElse(fail("expected the independent lower transition to survive beside the packet"))
    assertEquals(rebuilt.primary, packet.primary)

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

  test("packet admission rejects an extra CandidateComparison owner"):
    val packet = producedPacket(ranking(2))
    val comparisonRecord = packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _) => record
    } match
      case record :: Nil => record
      case records       => fail(s"expected one quantitative primary comparison, found ${records.size}")
    val extra = comparisonRecord.copy(
      ref = comparisonRecord.ref.copy(id = s"${comparisonRecord.ref.id}:unconsumed")
    )

    assertEquals(
      EvidenceBackedJudgmentPacket.fromAssembly(
        packet.assembly.withEvidence(extra)
      ),
      None
    )
