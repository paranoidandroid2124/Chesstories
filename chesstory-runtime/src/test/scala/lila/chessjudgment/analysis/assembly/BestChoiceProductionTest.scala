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
      .executePreparedReview(
        prepared,
        Some(ExplanationRequest.forObservedMove(prepared))
      )
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
