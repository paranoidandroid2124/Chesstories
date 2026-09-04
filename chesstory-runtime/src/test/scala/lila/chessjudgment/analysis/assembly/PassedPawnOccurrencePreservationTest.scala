package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class PassedPawnOccurrencePreservationTest extends munit.FunSuite:

  test("passed-pawn facts survive per occurrence without a combined causal contract"):
    val packet = MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = "7k/5K2/6P1/8/8/8/8/8 w - - 0 1",
          playedMoveUci = "g6g7",
          variations = List(
            EngineLine(List("f7e7", "h8g8", "e7e8"), scoreCp = 600, depth = 20),
            EngineLine(List("g6g7", "h8h7", "g7g8q"), scoreCp = -600, depth = 20)
          )
        )
      )
      .getOrElse(fail("expected the passed-pawn line to assemble"))
    val line = packet.assembly.legalLines
      .find(line => EvidenceRef.sameMove(line.ref.rootMove, "g6g7"))
      .getOrElse(fail("expected the observed LegalLine"))
    val root = packet.assembly.lineRootOccurrence(line.ref)
      .getOrElse(fail("expected the observed root occurrence"))
    val occurrences = packet.evidenceGraph
      .lineOccurrences(line.ref, root.transitionEvidenceId)
      .getOrElse(fail("expected graph-owned line occurrences"))

    assertEquals(occurrences.map(_.step.moveUci), List("g6g7", "h8h7", "g7g8q"))
    assertEquals(occurrences.map(_.structuralOccurrenceOwner.id).distinct.size, 3)
    assertEquals(occurrences.head.structural.certifiedRootResponseMoves, Some(List("h8h7")))

    val passedPawnFacts = occurrences.map(occurrence =>
      occurrence.step.moveUci -> occurrence.consequences
        .filter(_.kind == TransitionConsequenceKind.PassedPawnProgress)
        .flatMap(_.resultSubjectFacts.map(_.label))
    ).toMap
    assert(passedPawnFacts("g6g7").contains("passed-pawn-advanced:white:g6-g7:rank-7"))
    assert(passedPawnFacts("g7g8q").contains("passed-pawn-promoted:white:g7-g8"))

    val withoutMiddleStructural = packet.evidenceGraph.records.filterNot {
      case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) =>
        ref.line.contains(line.ref) && EvidenceRef.sameMove(payload.moveUci, "h8h7")
      case _ => false
    }
    assertEquals(packet.evidenceGraph.records.size - withoutMiddleStructural.size, 1)
    val surviving = TypedEvidenceGraph.empty
      .addAll(withoutMiddleStructural)
      .lineOccurrences(line.ref, root.transitionEvidenceId)
      .getOrElse(fail("expected the certified line after one unresolved occurrence"))

    assertEquals(surviving.map(_.step.moveUci), List("g6g7", "g7g8q"))
    assertEquals(surviving.map(_.structuralOccurrenceOwner.id).distinct.size, 2)
    assert(
      surviving.last.consequences
        .filter(_.kind == TransitionConsequenceKind.PassedPawnProgress)
        .flatMap(_.resultSubjectFacts.map(_.label))
        .contains("passed-pawn-promoted:white:g7-g8")
    )
