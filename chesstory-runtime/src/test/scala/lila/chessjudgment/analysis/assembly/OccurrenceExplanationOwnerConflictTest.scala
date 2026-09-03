package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class OccurrenceExplanationOwnerConflictTest extends munit.FunSuite:

  test("an invalid or unregistered typed relation cannot remove a certified sibling or primary"):
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
      .getOrElse(fail("expected an exact occurrence relation fixture"))
    val graph = packet.evidenceGraph
    val valid = graph.records.collectFirst {
      case record @ EvidenceRecord(_, _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) => record
    }.getOrElse(fail("expected one certified occurrence relation proof"))
    val invalid = valid.copy(
      ref = valid.ref.copy(
        id = s"${valid.ref.id}:invalid-owner",
        producer = EvidenceProducer.EngineEvalProducer
      )
    )

    assertEquals(OccurrenceRelationProof.fromRecord(invalid, graph), None)
    val withInvalid = packet.assembly.withEvidence(invalid)
    assert(!withInvalid.evidenceGraph.proofEligible(invalid))
    val rebuilt = EvidenceBackedJudgmentPacket
      .fromAssembly(withInvalid)
      .getOrElse(fail("an invalid relation must not remove the primary packet"))
    assertEquals(rebuilt.primary, packet.primary)
    assertEquals(
      rebuilt.occurrenceRelations.map(_.source),
      packet.occurrenceRelations.map(_.source)
    )

  test("comparison and occurrence relation owner ids are stable across admitted input order"):
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
        .getOrElse(fail("expected an exact occurrence relation fixture"))
      val comparisons = packet.evidenceGraph.records.collect {
        case EvidenceRecord(ref, CandidateComparisonEvidence(_), _) => ref.id
      }.sorted
      val relations = packet.occurrenceRelations.map(_.source.id).sorted
      comparisons -> relations

    val forward = ownerIds(lines)
    val reversed = ownerIds(lines.reverse)
    assertEquals(reversed, forward)
    assert(forward._1.forall(!_.matches(".*:\\d+$")))
    assert(forward._2.forall(!_.matches(".*:\\d+$")))

  test("independent typed proof records remain independent occurrence relations"):
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

    val relations = List(uniquePacket, solePacket).flatMap(_.occurrenceRelations)
    assertEquals(relations.size, 2)
    assertEquals(
      relations.map(_.source.id).toSet,
      proofRecords.map(_.ref.id).toSet
    )

  private def resolved(input: RawMoveReviewInput): EvidenceBackedJudgmentPacket =
    MoveReviewJudgmentOrchestrator
      .execute(input)
      .getOrElse(fail("expected an exact occurrence relation fixture"))
