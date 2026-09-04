package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class PassedPawnOccurrencePreservationTest extends munit.FunSuite:

  test("move occurrences and passed-pawn facts survive independently"):
    val assembly = MoveReviewInputAdmission
      .admit(
        RawMoveReviewInput(
          fen = "7k/5K2/6P1/8/8/8/8/8 w - - 0 1",
          playedMoveUci = "g6g7",
          variations = List(
            EngineLine(List("f7e7", "h8g8", "e7e8"), scoreCp = 600, depth = 20),
            EngineLine(List("g6g7", "h8h7", "g7g8q"), scoreCp = -600, depth = 20)
          )
        )
      )
      .map(MoveReviewAssembler.assemble)
      .getOrElse(fail("expected the passed-pawn line to assemble"))
    val line = assembly.legalLines
      .find(line => EvidenceRef.sameMove(line.ref.rootMove, "g6g7"))
      .getOrElse(fail("expected the observed LegalLine"))
    val root = assembly.lineRootOccurrence(line.ref)
      .getOrElse(fail("expected the observed root occurrence"))
    val withoutMiddleStructural = assembly.evidenceGraph.records.filterNot {
      case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) =>
        ref.line.contains(line.ref) && EvidenceRef.sameMove(payload.moveUci, "h8h7")
      case _ => false
    }
    assertEquals(assembly.evidenceGraph.records.size - withoutMiddleStructural.size, 1)
    val graph = TypedEvidenceGraph.empty.addAll(withoutMiddleStructural)
    val moveRecords = graph.records.collect {
      case record @ EvidenceRecord(ref, payload: MoveTransitionEvidence, _)
          if ref.id == root.transitionEvidenceId || ref.line.contains(line.ref) =>
        record -> payload
    }.sortBy(_._2.to.ply)

    assertEquals(moveRecords.map(_._2.moveUci), List("g6g7", "h8h7", "g7g8q"))
    assertEquals(moveRecords.map(_._1.ref.id).distinct.size, 3)
    assert(moveRecords.forall { case (record, payload) =>
      graph.proofEligible(record) && payload.movement.nonEmpty && payload.moveEffect.nonEmpty
    })

    val promotion = graph.records.collect {
      case record @ EvidenceRecord(_, payload: StructuralDeltaEvidence, _)
          if payload.line.contains(line.ref) && EvidenceRef.sameMove(payload.moveUci, "g7g8q") =>
        record -> payload
    } match
      case exact :: Nil => exact
      case records      => fail(s"expected one promotion structural occurrence, found ${records.size}")

    assert(graph.proofEligible(promotion._1))
    assert(
      promotion._2.consequences
        .filter(_.kind == TransitionConsequenceKind.PassedPawnProgress)
        .flatMap(_.resultSubjectFacts.map(_.label))
        .contains("passed-pawn-promoted:white:g7-g8")
    )
