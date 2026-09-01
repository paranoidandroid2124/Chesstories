package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class OccurrenceExplanationOwnerConflictTest extends munit.FunSuite:

  test("one ready occurrence Cause cannot hide duplicate producers"):
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1",
          playedMoveUci = "d1d8",
          variations = List(
            EngineLine(List("c4f7", "f8f7", "d1d8"), scoreCp = 600, mate = Some(1), depth = 24),
            EngineLine(List("d1d8", "f8d8"), scoreCp = 0, depth = 24)
          )
        )
      )
      .getOrElse(fail("expected an exact occurrence Cause fixture"))
    val graph = packet.evidenceGraph
    val owner = graph.records.collectFirst {
      case record @ EvidenceRecord(_, OccurrenceExplanationCauseEvidence(_), _) => record
    }.getOrElse(fail("expected one proof-admitted occurrence Cause"))
    val duplicate = owner.copy(ref = owner.ref.copy(id = s"${owner.ref.id}:duplicate-owner"))

    val conflict = intercept[IllegalArgumentException] {
      EvidenceBackedJudgmentPacket.fromAssembly(
        packet.assembly.withEvidence(duplicate)
      )
    }
    assert(conflict.getMessage.contains("each requested proof occurrence"))

  test("comparison and occurrence Cause owner ids are stable across admitted input order"):
    val lines = List(
      EngineLine(List("c4f7", "f8f7", "d1d8"), scoreCp = 600, mate = Some(1), depth = 24),
      EngineLine(List("d1d8", "f8d8"), scoreCp = 0, depth = 24)
    )
    def ownerIds(variations: List[EngineLine]): (List[String], List[String]) =
      val packet = MoveReviewJudgmentOrchestrator
        .execute(
          RawMoveReviewInput(
            fen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1",
            playedMoveUci = "d1d8",
            variations = variations
          )
        )
        .getOrElse(fail("expected an exact occurrence Cause fixture"))
      val comparisons = packet.evidenceGraph.records.collect {
        case EvidenceRecord(ref, CandidateComparisonEvidence(_), _) => ref.id
      }.sorted
      val causes = packet.evidenceGraph.records.collect {
        case EvidenceRecord(ref, OccurrenceExplanationCauseEvidence(_), _) => ref.id
      }.sorted
      comparisons -> causes

    val forward = ownerIds(lines)
    val reversed = ownerIds(lines.reverse)
    assertEquals(reversed, forward)
    assert(forward._1.forall(!_.matches(".*:\\d+$")))
    assert(forward._2.forall(!_.matches(".*:\\d+$")))

  test("independent typed proof records remain independent occurrence Causes"):
    val uniquePacket = resolved(
      RawMoveReviewInput(
        fen = "3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1",
        playedMoveUci = "d1d8",
        variations = List(
          EngineLine(List("c4f7", "f8f7", "d1d8"), scoreCp = 600, mate = Some(1), depth = 24),
          EngineLine(List("d1d8", "f8d8"), scoreCp = 0, depth = 24)
        )
      )
    )
    val solePacket = resolved(
      RawMoveReviewInput(
        fen = "8/4p2k/5n2/3r2B1/8/8/8/K2Q4 w - - 0 1",
        playedMoveUci = "d1d5",
        variations = List(
          EngineLine(List("g5f6", "e7f6", "d1d5"), scoreCp = 600, depth = 24),
          EngineLine(List("d1d5", "f6d5"), scoreCp = 0, depth = 24)
        )
      )
    )
    val proofRecords = List(
      uniquePacket.evidenceGraph.records.collectFirst {
        case record @ EvidenceRecord(_, _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) => record
      }.getOrElse(fail("expected the unique-check proof")),
      solePacket.evidenceGraph.records.collectFirst {
        case record @ EvidenceRecord(_, _: SoleRecapturerRemovalBeforeTargetCaptureEvidence, _) => record
      }.getOrElse(fail("expected the sole-recapturer proof"))
    )

    val causes = List(uniquePacket, solePacket).flatMap(_.evidenceGraph.records.collect {
      case EvidenceRecord(_, OccurrenceExplanationCauseEvidence(cause), _) => cause
    })
    assertEquals(causes.size, 2)
    assertEquals(
      causes.map(_.proofSource.id).toSet,
      proofRecords.map(_.ref.id).toSet
    )

  private def resolved(input: RawMoveReviewInput): EvidenceBackedJudgmentPacket =
    MoveReviewJudgmentOrchestrator
      .execute(input)
      .getOrElse(fail("expected an exact occurrence Cause fixture"))
