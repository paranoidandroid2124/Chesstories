package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class CommentaryMaterialAccountTest extends munit.FunSuite:
  private def admittedMaterial(fen: String, moves: List[String]): LineMaterialSummary =
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen,
      playedMoveUci = moves.head, variations = List(EngineLine(moves, 0, None, 16)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    ObservedLineProtocol.prepare(packet).paths.find(_.moves == moves).get.certified.flatMap(_._2.material).get

  // These are the existing immediate-exchange and en-passant continuations in PracticalDecisionTest.
  test("a capture interval keeps both sides' losses and rejects incomplete or repeated material events"):
    val moves = "b2f6 d7f6 f1f6 g7f6".split(" ").toList
    val material = admittedMaterial("2r1r2k/p2n1qp1/1p3pRp/3P4/2P5/P2Q4/1B4PP/5RK1 w - - 5 31", moves)
    val offsets = moves.indices.toList
    def calculate(selected: List[Int], shown: List[Int] = offsets) =
      CommentaryMaterialAccount.calculate(material, selected, shown, moves.size)
    val cost = calculate(offsets).get
    assertEquals(cost.commonExchanges, Nil)
    assertEquals(cost.whiteAdditionalLosses, List(EvidencePieceRole("bishop") -> 1, EvidencePieceRole("rook") -> 1))
    assertEquals(cost.blackAdditionalLosses, List(EvidencePieceRole("pawn") -> 1, EvidencePieceRole("knight") -> 1))
    assertEquals(cost.offsets, offsets)
    assert(cost.events.zip(material.events).forall((included, original) => included eq original))
    assertEquals(cost.events.size, material.events.size)

    assert(calculate(List(0, 2, 3)).isEmpty, "the knight's intervening capture cannot disappear from the total")
    assert(calculate(0 :: offsets).isEmpty, "one occurrence cannot be counted twice")
    assert(calculate(offsets, List(0, 1, 3)).isEmpty, "every included capture needs its move reference")
    assert(calculate(offsets, -1 :: offsets).isEmpty)
    assert(calculate(offsets, offsets :+ moves.size).isEmpty)
    assert(calculate(offsets :+ moves.size, offsets :+ moves.size).isEmpty)
    assert(calculate(Nil).isEmpty)
    assertEquals(calculate(offsets, offsets.reverse ++ offsets).get.offsets, offsets)
    val middle = calculate(List(1, 2), List(1, 2)).get
    assertEquals(middle.events.map(_.plyOffset), List(1, 2), "a bounded account need not own outside captures")
    assertEquals(middle.whiteAdditionalLosses, List(EvidencePieceRole("bishop") -> 1))
    assertEquals(middle.blackAdditionalLosses, List(EvidencePieceRole("knight") -> 1))

  test("an en-passant exchange preserves the actual victim square and cancels one pawn loss per side"):
    val moves = "a2a4 b4a3 a1a3".split(" ").toList
    val material = admittedMaterial("1r3rk1/5ppp/1q2b3/8/1p6/1P3BP1/PQ3P2/R2R2K1 w - - 0 26", moves)
    val cost = CommentaryMaterialAccount.calculate(material, List(1, 2), moves.indices.toList, moves.size).get
    assertEquals(cost.commonExchanges, List(EvidencePieceRole("pawn") -> 1))
    assertEquals(cost.whiteAdditionalLosses, Nil)
    assertEquals(cost.blackAdditionalLosses, Nil)
    assertEquals(cost.offsets, List(0, 1, 2), "the initial quiet advance remains available as context")
    assertEquals(cost.events.map(_.plyOffset), List(1, 2))
    assert(cost.events.head eq material.events.head)
    assertEquals(cost.events.head.movement.to, EvidenceSquare("a3"))
    assertEquals(cost.events.head.capture.get.square, EvidenceSquare("a4"))
    assertEquals(cost.events.last.capture.get.square, EvidenceSquare("a3"))
    assert(CommentaryMaterialAccount.calculate(material, List(0, 1, 2), List(0, 1, 2), moves.size).isEmpty,
      "a quiet move is context, not a material event")
