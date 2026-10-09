package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** A selected quiet rook use loses the rook but wins its attacked queen on
  * another square. The price is an observed reply, not a forced tactic. */
class UsedPieceCounterPriceTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import RootQuestionSummary.MoveRef

  private def piece(side: String, role: String, square: String) = CommentaryFact.Piece(side, role, square).witness
  private val rook = piece("white", "rook", "d1")
  private val queen = piece("black", "queen", "b6")
  private val knight = piece("white", "knight", "d2")
  private val alternative = List("d2b1", "h8g8")

  private def prepare(line: List[String], pawn: Boolean = false) =
    val fen = if pawn then "7k/8/1q6/1n6/5p2/4B3/3N4/3R3K w - - 0 1"
      else "7k/8/1q6/1n6/8/4B3/3N4/3R3K w - - 0 1"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, "d2c4", suppliedLines =
      List(line, alternative).map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = List(line, alternative).map(row => paths.certifiedPath(packet, row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val arm = RootQuestionSummary.prepare(packet, paths, pair, positions).get.head
    (packet, paths, pair, positions, arm)

  test("the quiet user's loss includes the immediate capture of its originally selected target"):
    val (packet, paths, pair, positions, arm) = prepare(
      List("d2c4", "h8g8", "d1d6", "b5d6", "e3b6", "g8f8"))
    val loss = arm.captures.find(_.originalVictim.contains(rook)).get
    val account = arm.usedPieceLossAccount(loss, rook, Set(queen))
    assertEquals(account.events.map(_.plyOffset), List(3, 4))
    assertEquals(arm.usedPieceLossAccount(loss, rook).events.map(_.plyOffset), List(3))
    val used = MoveRef(pair.head, 2)
    val route = ClearedRoute(MoveRef(pair.head, 0), knight, arm.at(knight, 1).get,
      rook, used, arm.at(rook, 3).get, Nil, true, None)
    val attack = Action(used, MoveWorkProjection.Detail.Attack(arm.at(rook, 3).get, queen, true))
    val section = Section(List(route, attack))
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison",
      List(Section(List(Continuation(pair.map(MoveRef(_, 0))))))).get
    val answer = MoveWorkProjection.protectRootUses(packet, paths, pair, positions,
      MoveWorkProjection.select(packet, paths).local, previous, List(section))
    val covered = answer.core.flatMap(_.covered)
    assert(covered.contains(MoveRef(pair.head, 3)), "retain the used rook's actual loss")
    assert(covered.contains(MoveRef(pair.head, 4)), "retain the queen recovery on its different square")
    assert(RootQuestionSummary.realize(paths, pair, answer).nonEmpty)

  test("a different immediate captured piece does not become the selected target's price"):
    val (_, _, _, _, arm) = prepare(
      List("d2c4", "h8g8", "d1d6", "b5d6", "e3f4", "g8f8"), pawn = true)
    val loss = arm.captures.find(_.originalVictim.contains(rook)).get
    assertEquals(arm.usedPieceLossAccount(loss, rook, Set(queen)).events.map(_.plyOffset), List(3))

  test("a later target capture after an intervening quiet answer is not this immediate counter-price"):
    val (_, _, _, _, arm) = prepare(
      List("d2c4", "h8g8", "d1d6", "b5d6", "e3f2", "g8f8", "f2b6", "f8g8"))
    val loss = arm.captures.find(_.originalVictim.contains(rook)).get
    assertEquals(arm.usedPieceLossAccount(loss, rook, Set(queen)).events.map(_.plyOffset), List(3))
