package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Real legal paths test delivery budgets, without assigning move values. */
class CommentaryWitnessScopeTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import RootQuestionSummary.MoveRef

  private val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  private val rootRows = List(List("g1f3", "h7h6"), List("e2e4", "h7h6"))
  private val witnessRows = List("a7a6", "a7a5", "b7b6", "b7b5", "c7c6", "c7c5",
    "d7d6", "d7d5", "e7e6", "e7e5").map(reply => List("g1f3", reply))
  private lazy val admitted =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, "g1f3", suppliedLines =
      (rootRows ++ witnessRows).map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = rootRows.map(row => paths.certifiedPath(packet, row).get)
    val witnesses = witnessRows.map(row => paths.certifiedPath(packet, row).get)
    (paths, pair, witnesses)

  private def answer(coreCount: Int, detailStart: Int, detailCount: Int): Answer =
    val (_, pair, witnesses) = admitted
    def continuation(refs: List[MoveRef]) = Section(List(Continuation(refs)))
    Answer("functional_response", List(
      continuation(pair.map(MoveRef(_, 0))),
      continuation(witnesses.take(coreCount).map(MoveRef(_, 1)))),
      List(continuation(witnesses.slice(detailStart, detailStart + detailCount).map(MoveRef(_, 1)))), Nil)

  test("a complete core keeps its own five witnesses alongside five different detail witnesses"):
    val (paths, pair, witnesses) = admitted
    val delivered = RootQuestionSummary.realize(paths, pair, answer(5, 5, 5)).get
    assertEquals((delivered.json \ "witness_line_ids").as[List[String]].toSet,
      witnesses.map(_.line.ref.id).toSet)
    assertEquals((delivered.json \ "units").as[List[JsObject]].size, 2)
    assertEquals((delivered.json \ "detail_units").as[List[JsObject]].size, 1)

  test("unused detail capacity cannot admit a sixth core witness"):
    val (paths, pair, _) = admitted
    assert(RootQuestionSummary.realize(paths, pair, answer(6, 6, 1)).isEmpty)

  test("unused core capacity cannot admit a sixth detail witness"):
    val (paths, pair, _) = admitted
    assert(RootQuestionSummary.realize(paths, pair, answer(1, 1, 6)).isEmpty)

  test("a shared witness consumes capacity in each layer and appears only once in the union"):
    val (paths, pair, witnesses) = admitted
    val delivered = RootQuestionSummary.realize(paths, pair, answer(5, 0, 5)).get
    assertEquals((delivered.json \ "witness_line_ids").as[List[String]].toSet,
      witnesses.take(5).map(_.line.ref.id).toSet)

  test("a supplementary capture remains unpriced even when witness capacity is available"):
    val captureFen = "r6k/8/8/8/8/8/8/R6K w - - 0 1"
    val history = CanonicalPositionHistory.from(captureFen, Nil, captureFen).toOption.get
    val rows = List(List("a1a3", "a8b8"), List("a1a4", "a8b8"), List("a1a3", "a8a3"))
    val input = MoveReviewInputAdmission.admitIndependent(history, "a1a3", suppliedLines =
      rows.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = rows.take(2).map(row => paths.certifiedPath(packet, row).get)
    val witness = paths.certifiedPath(packet, rows.last).get
    val selected = Answer("functional_response", List(Section(List(Continuation(pair.map(MoveRef(_, 0)))))),
      List(Section(List(Continuation(List(MoveRef(witness, 1)))))), Nil)
    assert(RootQuestionSummary.realize(paths, pair, selected).isEmpty,
      "extra detail capacity never supplies a missing capture or recapture account")
