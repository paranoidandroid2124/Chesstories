package lila.chessjudgment.analysis.assembly

import chess.variant.Standard
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
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

    assertEquals(prepared.lines.flatMap(_.rootMove), ranking)
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
    assertEquals(parents.count(_ == owner.evidence), 1)
    assertEquals(parents.map(_.id).distinct.size, parents.size)

  test("a non-best played move preserves separate reference and candidate line parents"):
    val packet = producedPacket(ranking(1))
    val (assessment, parents) = soleAssessmentRecord(packet)

    assertNotEquals(assessment.reference, assessment.candidate)
    assertEquals(parents.count(_ == assessment.reference.evidence), 1)
    assertEquals(parents.count(_ == assessment.candidate.evidence), 1)
    assertEquals(parents.map(_.id).distinct.size, parents.size)

  test("relative assessment production rejects every non-owner duplicate or id collision"):
    val (assessment, _) = soleAssessmentRecord(producedPacket(ranking.head))
    val duplicate = assessment.copy(relativeCauseEvidence = List(assessment.primaryComparisonEvidence))
    val collision = assessment.copy(
      relativeCauseEvidence = List(
        assessment.primaryComparisonEvidence.copy(scope = EvidenceScope.PlayedTransition)
      )
    )

    intercept[IllegalArgumentException](TransitionFactNormalizer.fromRelativeAssessment(duplicate))
    intercept[IllegalArgumentException](TransitionFactNormalizer.fromRelativeAssessment(collision))
