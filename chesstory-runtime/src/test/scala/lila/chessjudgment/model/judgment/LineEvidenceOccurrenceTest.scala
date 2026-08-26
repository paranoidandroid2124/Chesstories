package lila.chessjudgment.model.judgment

import chess.White
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class LineEvidenceOccurrenceTest extends munit.FunSuite:

  private val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  test("line projection retains every continuation occurrence, including repeated moves"):
    val moves = List(
      "g1f3",
      "g8f6",
      "f3g1",
      "f6g8",
      "g1f3",
      "g8f6",
      "f3g1",
      "f6g8",
      "g1f3",
      "g8f6"
    )
    val replay = PrincipalVariationEvidence
      .legalMoveReplay(initialFen, moves, startPly = 0)
      .flatMap(CanonicalLineReplay.fromLegalReplay)
      .getOrElse(fail("expected a legal repeated-move replay"))
    val line = LineNodeRef("complete-line-occurrences", moves.head, 1, LineNodeRole.BestReference)
    val position = PositionNodeRef(initialFen, 0, Some(White), Some("complete-line-root"))
    val ref = EvidenceRef(
      id = "complete-line-evidence",
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = position,
      line = Some(line),
      scope = EvidenceScope.BestLine,
      confidence = EvidenceConfidence.LegalReplayVerified
    )
    val record = EvidenceRecord(
      ref,
      LineFactEvidence.fromCertifiedReplay(line, replay)
    )
    val graph = TypedEvidenceGraph.empty.add(record)

    val continuationBindings = EvidenceObjectBinding
      .fromEvidenceRefs(graph, List(ref))
      .filter(_.lineOccurrence.nonEmpty)
    val occurrences = continuationBindings.flatMap(_.lineOccurrence)

    assertEquals(occurrences.map(_.ply), (2 to moves.size).toList)
    assertEquals(occurrences.map(_.moveUci), moves.tail)
    assertEquals(continuationBindings.map(_.signature).distinct.size, moves.size - 1)
    assertEquals(occurrences.count(_.moveUci == "g8f6"), 3)
