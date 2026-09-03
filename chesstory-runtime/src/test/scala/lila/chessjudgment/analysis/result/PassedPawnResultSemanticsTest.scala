package lila.chessjudgment.analysis.result

import lila.chessjudgment.analysis.assembly.{ EvidenceFactAssembler, RawMoveReviewInput }
import lila.chessjudgment.model.PassedPawnResultKind
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class PassedPawnResultSemanticsTest extends munit.FunSuite:

  test("graph-owned transformation kinds cannot borrow a sibling result"):
    assertEquals(
      resultKinds(transition("4k3/8/P7/8/8/8/8/4K3 w - - 0 1", "a6a7")),
      Set(PassedPawnResultKind.AdvanceOrPromote)
    )
    assertEquals(
      resultKinds(transition("4k3/8/1p6/P7/8/8/8/4K3 w - - 0 1", "a5a6")),
      Set(PassedPawnResultKind.Creation)
    )
    assertEquals(
      resultKinds(transition("4k3/P7/8/8/8/8/8/4K3 w - - 0 1", "a7a8q")),
      Set(PassedPawnResultKind.AdvanceOrPromote)
    )

  private def resultKinds(
      occurrence: GraphOwnedLineTransitionOccurrence
  ): Set[PassedPawnResultKind] =
    occurrence.consequences
      .flatMap(PassedPawnResultTransitionProof.directCandidates(occurrence, _))
      .map(_.resultKind)
      .toSet

  private def transition(fen: String, move: String): GraphOwnedLineTransitionOccurrence =
    val context = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = fen,
          playedMoveUci = move,
          variations = List(
            EngineLine(List("e1e2"), scoreCp = 100, depth = 20),
            EngineLine(List(move, "e8e7"), scoreCp = 0, depth = 20)
          )
        )
      )
      .getOrElse(fail(s"expected graph-owned transition $move"))
    val line = context.legalLines
      .find(line => EvidenceRef.sameMove(line.ref.rootMove, move))
      .getOrElse(fail(s"expected the LegalLine for $move"))
    val root = context.lineRootOccurrence(line.ref).getOrElse(
      fail("expected the line's root occurrence")
    )
    context.evidenceGraph
      .lineTransitionOccurrencePrefix(line.ref, root.transitionEvidenceId)
      .flatMap(_.headOption)
      .getOrElse(fail("expected one graph-owned root transition"))
