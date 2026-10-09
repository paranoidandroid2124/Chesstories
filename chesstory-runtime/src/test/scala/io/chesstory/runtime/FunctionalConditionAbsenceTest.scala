package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

class FunctionalConditionAbsenceTest extends munit.FunSuite:
  private val fen = "7k/8/8/1p6/8/8/3N4/3R3K w - - 0 1"
  private val line = List("d2c4", "b5c4", "d1d6", "h8g8")
  private val guard = CommentaryFact.Piece("white", "knight", "d2")
  private val control = CommentaryFact.Predicate("control", Some(guard), square = Some("d6"))

  private def prepared() =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, line.head,
      suppliedLines = List(line).map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    paths -> paths.certifiedPath(packet, line).get

  test("capturing the original supporter proves absent control rather than unknown evidence"):
    val (paths, path) = prepared()
    assertEquals(FunctionalReplyReading.conditionAt(paths, path, 1, control), Some("present"))
    assertEquals(paths.trackedPieceAfter(path, guard.witness, 2), Some(None))
    assertEquals(FunctionalReplyReading.conditionAt(paths, path, 2, control), Some("absent"))

  test("foreign paths and an unavailable future position remain unknown"):
    val (paths, path) = prepared()
    val (_, foreign) = prepared()
    assertEquals(FunctionalReplyReading.conditionAt(paths, foreign, 2, control), None)
    assertEquals(FunctionalReplyReading.conditionAt(paths, path, line.size + 1, control), None)
