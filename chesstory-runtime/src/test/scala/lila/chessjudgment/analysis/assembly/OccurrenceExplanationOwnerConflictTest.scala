package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class OccurrenceExplanationOwnerConflictTest extends munit.FunSuite:

  private val rootFen =
    "r2qrbk1/1bpn1p1p/p2p1np1/Pp2p3/3PP3/2P2NNP/1PB2PP1/R1BQR1K1 w - - 1 17"
  private val lines = List(
    EngineLine(List("d4d5", "c7c6", "d5c6", "b7c6", "b2b4"), scoreCp = 600, depth = 24),
    EngineLine(List("b2b4", "e5d4"), scoreCp = 0, depth = 24)
  )

  test("an invalid or unregistered typed relation cannot remove a certified sibling or primary"):
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = rootFen,
          playedMoveUci = "b2b4",
          variations = lines
        )
      )
      .getOrElse(fail("expected an exact occurrence relation fixture"))
    val graph = packet.evidenceGraph
    val valid = graph.records.collectFirst {
      case record @ EvidenceRecord(_, _: CaptureExclusionMoveOrderEvidence, _) => record
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
    def ownerIds(variations: List[EngineLine]): (List[String], List[String]) =
      val packet = MoveReviewJudgmentOrchestrator
        .execute(
          RawMoveReviewInput(
            fen = rootFen,
            playedMoveUci = "b2b4",
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
