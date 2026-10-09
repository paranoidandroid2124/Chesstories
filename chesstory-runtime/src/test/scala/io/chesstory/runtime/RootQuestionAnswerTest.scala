package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.Json

/** Existing opened master rows from ReferenceMeaningDeliveryTest, inline.
  * These checks exercise admitted facts and delivery, without an engine,
  * fixture loading, a new position, or an assessment of move quality.
  */
class RootQuestionAnswerTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import RootQuestionSummary.MoveRef

  private case class Example(name: String, question: String, fen: String, rows: List[String])
  private val examples = List(
    Example("support recapturers", "paired_support_use",
      "r3b1k1/r4ppp/3np3/3p4/p2P4/1P1NPP2/R2N1KPP/2R5 w - - 0 24", List(
        "c1a1 d6c8 b3a4 a7a4 a2a4 a8a4 a1a4 e8a4 e3e4 a4b5 d3c5 c8a7 d2b1 a7c6 b1a3 c6d4 f2e3",
        "d3c5 a4b3 a2a7 a8a7 c5b3 a7a2 c1a1 a2c2 a1c1")),
    Example("interposition", "support_and_interposition",
      "5r2/pp1rbppk/2n5/8/3p3P/3N4/PP1BPP2/2R2K1R b - - 1 21", List(
        "f8e8 h1g1 f7f6 g1g4 h7g8 a2a4 g8f7 b2b4 e8h8 g4f4 h8h5 b4b5 c6e5 d3e5 h5e5",
        "d7d5 d2g5 f7f6 g5f4 f8e8 f1g2 h7g8 c1b1 d5a5 a2a3 g8f7")),
    Example("support transfer", "provider_transfer",
      "r2qnrk1/p2p2pp/b1n5/2p2p2/2P2P2/P1PBB3/4N1PP/R2Q1RK1 b - - 1 14", List(
        "d7d6 e3f2 g7g6 a1b1 d8c7 f1e1 e8f6 d1a4 a6b7 f2h4 a8e8 e2g3 e8e1 b1e1 c6d8 h4f6",
        "d8a5 f1e1 g7g6 e2c1 d7d6 c1b3 a5c7 e3f2 e8f6 f2h4 g8g7 a1a2 a8e8 a2e2 c6d8")),
    Example("capturing evasion", "attack_or_immediate_exchange",
      "2rq1rk1/3n1ppp/p2bp3/1pp5/3P4/1N2Q1P1/PP1NPPKP/R1R5 w - - 0 20", List(
        "d2e4 c5d4 b3d4 d7c5 d4f3 c5e4 e3e4 d8b6 c1c2 c8c2 e4c2 d6e7 a1c1 b6a5 c2c7 a5c7 c1c7 e7f6 b2b3 f8d8 c7c1 g7g5",
        "d4c5 d7c5 c1d1 c8c7 a1c1 d8a8 d2f3 c5b3 c1c7 d6c7 e3b3 f8d8")),
    Example("counterattacking evasion", "attack_or_immediate_exchange",
      "r3rbk1/1p1bqppp/8/pB1p4/7P/P3PP2/1PQ2P2/2KRB2R w - - 2 18", List(
        "d1d5 e8c8 e1c3 d7e6 d5d3 c8c5 a3a4 h7h5 c1b1 g7g6 c2d2 f8g7 c3g7 g8g7 b5d7 g7h7",
        "b5d7 e7d7 e1c3 e8c8 c1d2 d5d4 d2e1 d7c7 d1d4 b7b5 c2e4 b5b4 c3d2 c7c2 e4c2 c8c2 e1e2 c2b2 a3b4 a5b4 h1c1 a8b8 e2d3")),
    Example("retained knight and check reply", "escape_or_recapture",
      "2kr3r/ppp2p1p/2nn2p1/4p3/2N1P3/2P2BP1/PP3P1P/3RR1K1 w - - 3 16", List(
        "c4e3 c6b8 f3g2 h7h5 g2h3 b8d7 e3d5 h5h4 g1g2 c8b8 h3d7 d8d7 d5f6 d7e7 g3g4",
        "f3g4 c8b8 g4e2 b8c8 f2f4 d6c4 e2c4 d8d1 e1d1 e5f4 g3f4 h8d8 d1d8 c6d8 g1f2 c8d7 e4e5 d7e7")),
    Example("same queen occurrence", "capture_now_or_after_attack",
      "5rk1/p4rb1/3q2p1/2p1p1Bp/2pn4/P1NP1PP1/4P1K1/1R1Q2R1 w - - 0 27", List(
        "d3c4 d6e6 d1d3 f7d7 g1d1 f8f7 c3e4",
        "c3e4 d6a6 d3c4 a6c4 b1b2 c4a6 d1d2 c5c4 d2c3 f7c7 g1d1 g8h7 d1a1 a6a4 a1b1 a4c6 b1a1")),
    Example("interleaved neutral trades", "occupied_pawn_destination",
      "r1b4r/p3kn2/2p1p3/2N2pp1/2P4p/4P3/PP2B1PP/2KR3R w - - 0 20", List(
        "g2g4 f7e5 h1g1 h8g8 c1c2 c8d7 c2c3 a8f8 h2h3 d7c8 c3d4 e5f7 d4c3",
        "h2h3 a7a5 e3e4 f5f4 c1c2 e6e5 b2b3 a8a7 c2c3 e7f6 c5d3 g5g4 e2g4 c8g4 h3g4 a7d7 d3f2 d7d1 h1d1 f6e7 a2a3 e7f6 d1h1 c6c5 f2d3 f6g5 d3c5 g5g4")),
    Example("different-target countercapture", "counter_capture_comparison",
      "r4rk1/p1p3pp/1b6/2PpN3/4b1Pq/4B2P/PP3P1K/R2Q1R2 b - - 0 19", List(
        "h4e7 e5c6 e7f6 c5b6 f6c6 b6a7 f8f3 f1g1 c6d6 g1g3 f3e3 f2e3 a8a7 d1b3 d6f6 b3b8 g8f7",
        "b6a5 f2f4 a8e8 d1a4 a5d2 e3d2 e8e5 a4d4 e5e6 d4f2 h4e7 d2c3 h7h5 g4h5 f8f7 f1e1")),
    Example("checking exchange before evasion", "opening_counterplay",
      "1r2r1k1/p1pq1p2/1p1b1pp1/8/2QPB3/7P/PP3PP1/R3R1K1 b - - 3 22", List(
        "b6b5 c4c2 d6f8 e1d1 e8d8 c2c6 f6f5 e4f3",
        "g8g7 e4c6 e8e1 a1e1 d7f5 g2g3 b8h8 c6e8 h8f8 g1g2 f5c8 e8c6 a7a5 h3h4 f6f5 f2f4 c8d8 b2b3 d6e7 e1e3 e7a3")),
    Example("legal controlled destination", "controlled_destination",
      "2r2rk1/pp1n1pp1/2n1p2p/1q1pP2P/3P2P1/4QN2/PP1N1P2/1KR4R w - - 3 19", List(
        "g4g5 c6b4 c1g1 g8h7 d2b3 b5d3 e3d3 b4d3 h1h2 c8c7 a2a3 d7b6 g1d1 d3f4 b3c5 b6c4 h2h4 f4e2 h4g4 f8c8 d1e1 e2d4 g5g6 f7g6 f3d4 c7c5 h5g6 h7g8 d4e6",
        "a2a3 c6e7 h1g1 d7b6 c1c5 b5a4 e3c3 a4a6 c3e3 b6a4 c5c1 c8c1 g1c1 f8c8 c1c8 e7c8 g4g5 a6b5 e3b3 b5b3 d2b3 h6g5 f3g5 c8e7")),
    Example("same arrangement", "observed_arrangement_convergence",
      "4r1k1/5pb1/2p4p/2Pp1Q2/q7/1N4P1/2P2P2/2R3K1 w - - 1 28", List(
        "g1g2 g7c3 c1h1 g8g7 f5f3 a4c4 h1d1 e8e6 d1d3 c3b2 f3d1 c4e4 d1f3",
        "f5d3 a4b4 g1g2 g7c3 c1h1 g8g7 d3f3 b4c4 h1d1 e8e6 d1d3 c3b2 f3d1 c4e4 d1f3")),
    Example("move order with castling", "observed_move_order",
      "r3k2r/pppbnppp/2np1q2/5P2/4P3/1N1B4/P1PB2PP/R2Q1R1K w kq - 2 15", List(
        "a2a4 c6e5 d1c1 h7h6 d2c3 e7c6 b3d2 f6e7 a1b1 f7f6 b1b7 d7c8 b7b1 e8g8 h2h3 c8d7",
        "d1c1 h7h6 a2a4 e8g8 f1f3 c6e5 f3g3 g8h7 g3h3 d6d5 b3c5 d7c8 d3e2 b7b6")),
    Example("check versus unrecorded next reply", "check_status_comparison",
      "r3qr1k/1pp1bp1p/p1n4P/4pnp1/PP2N3/1BP2P2/1B1Q1PK1/R2R4 w - - 4 24", List(
        "d2d7 f5h4 g2f1 f7f5 e4g3 h4f3 b4b5 c6a5 b3a2 a6b5 a4b5 g5g4 g3f5 f3h2 f1g1 h2f3 g1f1",
        "g2f1 a8d8 d2e2 d8d1 a1d1 e8c8 b4b5 a6b5 a4b5 c6a5 b3a2 f7f6 e4g3 f5h4")),
    Example("promotion and immediate removal", "deferred_pawn_attack",
      "r4rk1/2p1bppp/p1n5/1p1qPb2/3B4/1PPp1N2/1P3PPP/RB1QR1K1 b - - 6 17", List(
        "d3d2 e1e2 c6d4 f3d4 f5b1 a1b1 e7g5 h2h4 g5f4 g2g3 f4e5 e2d2 e5d4 d2d4 d5c6 b1a1 a8e8 b3b4 e8e6 d4d7 f8e8 d1d5",
        "f8d8 h2h4 d3d2 b1f5 d2e1q d1e1 d5b3 e5e6 d8d6 f3e5 c6d4 e6f7 g8f8 c3d4 g7g6 e1e4 a8d8 a1e1 g6f5 e4f5 d6h6")),
    Example("route with complete separate trades", "observed_route_use",
      "r2qr1k1/p4ppp/1b6/n1Q1p3/4N3/BP1b1N2/P2P1PPP/R3R1K1 w - - 3 19", List(
        "c5d6 a5b7 d6d8 a8d8 g2g4 f7f6 a1c1 d8d5 e4c3 d5d7 c3e4 h7h6 c1c3 b6a5 c3c6 e8d8 h2h4 a5b6 c6c3",
        "c5c3 d3e4 e1e4 a8c8 c3b4 a5c6 b4d6 c6d4 d6d8 e8d8 a1c1 d4f3 g2f3 c8c1 a3c1 b6d4 c1a3 d8c8 a3b4")),
    Example("specific retained knight", "contested_pair_response",
      "r1bqr1k1/1pp2pbp/6p1/1P2pn2/3n4/2NP1NP1/2P2PBP/1RBQR1K1 w - - 1 15", List(
        "f3d4 f5d4 c1e3 h7h5 d1c1 d4f5 e3d2 f5d4 d2g5 f7f6 g5e3 d4f5 b5b6 c7c6 c3e4 f5e3 c1e3 g8h7 e3c5",
        "f3d2 a8a7 c3d5 d4b5 b1b5 c7c6 b5c5 c6d5 c5d5 d8c7 c1b2 c8e6 b2e5 c7c8 d5b5 e6d7 b5d5")),
    Example("blocker already present", "opening_counterplay",
      "r3r1k1/ppp2p2/2np1qp1/2n4p/4P3/1P3B2/P4PPP/1QBRR1K1 w - - 0 21", List(
        "c1b2 c6e5 f3e2 h5h4 b1c1 c5e6 g2g3 g6g5 e2g4 e8e7 e1e3 a8e8 g3h4 f6f4 e3g3",
        "e1e3 c6d4 c1b2 c5e6 f3e2 c7c5 e2c4 h5h4 b1a1 a8c8 h2h3 a7a6 c4e6 e8e6 a2a4 c8e8")),
    Example("earlier retreat changes recapture", "opening_counterplay",
      "r2q1rk1/pb3ppp/1pn1p3/8/2BPP3/5N2/P2Q1PPP/3R1RK1 w - - 2 14", List(
        "f1e1 c6a5 c4f1 d8d6 d4d5 e6d5 e4e5 d6e7 f3d4 b7c8 f2f4 c8g4 d1c1 a8c8 f4f5 c8c1 d2c1 e7c5",
        "d2f4 d8f6 f4e3 h7h6 f1e1 a8c8 e4e5 f6e7 d4d5 e6d5 c4d5 c8d8 a2a3 c6a5 d5a2 d8d1 e1d1 f8d8 d1d8 e7d8 h2h4")))

  private def prepare(example: Example) =
    val lines = example.rows.map(row => EngineLine(row.split(" ").toList, 0, None, 16))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = example.fen,
      playedMoveUci = lines.head.moves.head, variations = lines)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = lines.map(line => paths.paths.find(_.moves == line.moves).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val chosen = RootQuestionSummary.choose(packet, paths, pair, positions)
    (paths, pair, chosen)

  private def materials(answer: Answer): List[Material] = answer.core.flatMap(_.statements.flatMap {
    case value: Material => List(value)
    case value: TargetResponse => value.material.toList
    case _ => Nil
  })

  examples.foreach { example =>
    test(s"typed ${example.question}: ${example.name}") {
      val (paths, pair, selected) = prepare(example)
      val chosen = selected.getOrElse(fail(s"no selected facts for ${example.name}"))
      assertEquals(chosen.question, example.question)
      val answer = chosen.confirmedFacts
      val sections = answer.core ++ answer.details
      sections.foreach { section =>
        section.required.foreach(ref => assert(section.references.contains(ref), s"unrepresented obligation: $ref"))
        section.references.foreach { ref =>
          assert(pair.exists(_ eq ref.path))
          assert(ref.offset >= 0 && ref.offset < ref.path.moves.size)
          assert(chosen.evidence.contains(ref.path.ids(ref.offset)))
        }
      }
      val coreCovered = answer.core.flatMap(_.covered)
      assertEquals(coreCovered.distinct.size, coreCovered.size, "one capture has one core cost owner")
      materials(answer).foreach { value =>
        val original = value.path.certified.get._2.material.get
        assert(value.cost.events.forall(event => original.events.exists(_ eq event)))
        val first = value.cost.events.head.plyOffset
        val last = value.cost.events.last.plyOffset
        assertEquals(value.cost.events, original.events.filter(event => event.plyOffset >= first && event.plyOffset <= last),
          "a claimed material interval cannot omit an interleaved capture")
        assert(value.covered.forall(ref => answer.details.exists(_.covered.contains(ref))),
          "detail must expand the same selected cost")
      }
      val delivered = chosen.realize(paths, pair)
      assert(delivered.nonEmpty, s"typed facts should deliver for ${example.name}")
      assertEquals((delivered.get.json \ "schema_version").as[Int], 2)
    }
  }

  test("interleaved captures and the recapturing pawn's eventual loss share one original ledger") {
    val (_, pair, selected) = prepare(examples.find(_.question == "occupied_pawn_destination").get)
    val cost = materials(selected.get.confirmedFacts).find(_.path eq pair.last).get.cost
    assertEquals(cost.events.map(_.plyOffset), List(12, 13, 14, 17, 18, 26, 27))
    assertEquals(cost.whiteAdditionalLosses, Nil)
    assertEquals(cost.blackAdditionalLosses.map((role, count) => role.name -> count), List("pawn" -> 1))
  }

  test("the first queen evasion keeps its occurrence instead of a later identical SAN") {
    val (_, pair, selected) = prepare(examples.find(_.question == "capture_now_or_after_attack").get)
    val responses = selected.get.confirmedFacts.core.flatMap(_.statements.collect { case value: TargetResponse => value })
    val queen = responses.find(_.path eq pair.last).get
    assertEquals(queen.escape, Some(1))
    assert(queen.references.contains(MoveRef(pair.last, 1)))
    assert(!queen.references.contains(MoveRef(pair.last, 5)))
  }

  test("missing the supporting recapturer's loss cannot keep the balanced paired answer") {
    val example = examples.head
    val shortened = example.copy(rows = example.rows.head.split(" ").take(7).mkString(" ") :: example.rows.tail)
    assert(prepare(shortened)._3.forall(_.question != "paired_support_use"))
  }

  test("an unrelated quiet move stays in detail without losing an intervening capture") {
    val (_, pair, selected) = prepare(examples.head)
    val answer = selected.get.confirmedFacts
    val cost = materials(answer).find(_.path eq pair.head).get.cost
    assertEquals(cost.events.map(_.plyOffset), List(2, 3, 4, 5, 6, 7))
    assertEquals(cost.offsets, List(0, 2, 3, 4, 5, 6, 7))
    val quiet = MoveRef(pair.head, 1)
    assert(!answer.core.exists(_.references.contains(quiet)))
    assert(answer.details.exists(_.references.contains(quiet)))
  }

  test("route attacks, evasions and the arrival reply stay with their material account") {
    val (_, pair, selected) = prepare(examples.find(_.question == "observed_route_use").get)
    val answer = selected.get.confirmedFacts
    val cost = materials(answer).find(_.path eq pair.last).get.cost
    assertEquals(cost.events.map(_.plyOffset), List(1, 2, 8, 9))
    List(3, 4, 5, 6, 7).foreach { offset =>
      val ref = MoveRef(pair.last, offset)
      assert(answer.core.exists(section => section.references.contains(ref) && section.required.contains(ref)),
        s"the actual route reply at offset $offset cannot be reduced to an optional detail")
    }
    assert((0 to 9).forall(offset => answer.details.exists(_.references.contains(MoveRef(pair.last, offset)))))
  }

  test("a recapture keeps its original ledger meaning without repeating the preceding capture") {
    val example = examples.find(_.name == "specific retained knight").get
    val lines = example.rows.map(row => EngineLine(row.split(" ").toList, 0, None, 16))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = example.fen,
      playedMoveUci = lines.head.moves.head, variations = lines)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = lines.map(line => paths.paths.find(_.moves == line.moves).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val arm = RootQuestionSummary.prepare(packet, paths, pair, positions).get.head
    val recorded = arm.material.captures.find(_.recapture).get
    val recapture = arm.captures.find(_.plyOffset == recorded.plyOffset).get
    val preceding = arm.captures.find(_.plyOffset == recapture.plyOffset - 1).get
    assertEquals(recapture.originalVictim, preceding.originalActor)
    val together = arm.pieceExchange(List(preceding, recapture))
    val alone = arm.pieceExchange(List(recapture))
    assertEquals(together.recaptureOffsets, Set(recorded.plyOffset))
    assertEquals(alone.recaptureOffsets, together.recaptureOffsets)
    assertEquals(arm.pieceExchange(List(preceding)).recaptureOffsets, Set.empty[Int])
    assert(alone.events.head eq recapture, "retain the original event and physical identities")
    assertEquals(alone.references, List(MoveRef(arm.path, recorded.plyOffset)))
    assertEquals(alone.covered, alone.references)
    def words(value: PieceExchange): String = CommentaryEnglish.statement(value).map {
      case text: String => text
      case ref: MoveRef => paths.notation.line(ref.path.moves)(ref.offset)
    }.mkString
    assert(words(alone).contains(" recaptures "))
    assertEquals(CommentaryEnglish.statement(together).collect { case ref: MoveRef => ref }, together.references)
    assertEquals(CommentaryEnglish.statement(alone).collect { case ref: MoveRef => ref }, alone.references)
    assertEquals(together.events.last.originalVictim, alone.events.head.originalVictim)
    assertEquals(together.events.last.originalActor, alone.events.head.originalActor)
  }

  test("a recorded move order retains the same original knight's root attack and actual release") {
    // Already opened WCC 2016 game 8. The g4-knight and g6-knight are distinct identities.
    val example = Example("earlier knight escape", "observed_move_order",
      "3r2k1/4qppp/1p2p1n1/pN6/P1PR2n1/4P3/5PPP/4QBK1 b - - 0 24", List(
        "g4f6 e1d1 d8b8 g2g3 g6e5 f1g2 h7h5 h2h3 e7c5 b5d6 c5b4 g1h2 b8f8 f2f4 e5d7 d6e4 f6e4 g2e4",
        "g6e5 e1d2 d8b8 d4d6 g4f6 d2d4 e5d7 b5a7 e7e8 g2g4 e6e5 d4d1 d7c5 a7c6 g7g6"))
    val lines = example.rows.map(row => EngineLine(row.split(" ").toList, 0, None, 16))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = example.fen,
      playedMoveUci = lines.head.moves.head, variations = lines)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = lines.map(line => paths.paths.find(_.moves == line.moves).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val chosen = RootQuestionSummary.choose(packet, paths, pair, positions).get
    assertEquals(chosen.question, example.question)
    val content = chosen.confirmedFacts.core.flatMap(_.statements)
    val contrast = content.collectFirst { case v: ControlContrast => v }.get
    assertEquals(contrast.move, MoveRef(pair.head, 0))
    assertEquals(contrast.alternative, MoveRef(pair.last, 0))
    assertEquals(contrast.target.map(_.square.key), Some("f6"))
    assertEquals(contrast.alternativeTarget.map(_.square.key), Some("g4"))
    assert(!contrast.present && contrast.alternativePresent)
    val knight = CommentaryFact.Piece("black", "knight", "g4").witness
    val arms = RootQuestionSummary.prepare(packet, paths, pair, positions).get
    assertEquals(arms.head.at(knight, 1), contrast.target)
    assertEquals(arms.last.at(knight, 1), contrast.alternativeTarget)
    val withdrawal = content.collectFirst { case v: AttackWithdrawal => v }.get
    assertEquals(withdrawal.move, MoveRef(pair.last, 3))
    assertEquals(withdrawal.before.square.key, "d4")
    assertEquals(withdrawal.after.square.key, "d6")
    assertEquals(withdrawal.target, knight)
    val text = chosen.realize(paths, pair).get.text
    List("Nf6", "N6e5", "rook on d4", "knight on g4", "Rd6", "ending its attack").foreach(s =>
      assert(text.contains(s), text))
    assert(!text.toLowerCase.contains("safe") && !text.toLowerCase.contains("forced"))
    val unknownRoot = RootQuestionSummary.choose(packet, paths, pair, positions - pair.head.positions(1)).get
    assert(!unknownRoot.confirmedFacts.core.flatMap(_.statements).exists(_.isInstanceOf[ControlContrast]),
      "unknown root control cannot become an observed absence")
    val unknownRelease = RootQuestionSummary.choose(packet, paths, pair, positions - pair.last.positions(3)).get
    assert(!unknownRelease.confirmedFacts.core.flatMap(_.statements).exists(_.isInstanceOf[AttackWithdrawal]),
      "the attacker's departure needs its own complete preceding geometry")
    assertEquals(unknownRoot.question, chosen.question)
    assertEquals(unknownRelease.question, chosen.question)
  }

  test("delivery failure does not erase the selected question or its factual answer") {
    val (paths, pair, selected) = prepare(examples.find(_.question == "observed_arrangement_convergence").get)
    val chosen = selected.get
    val oversized = RootQuestionSummary.SelectedQuestion(chosen.confirmedFacts.copy(
      // Whole sections may be packed into the delivery containers. One
      // unsplittable statement exceeding the part bound must still fail.
      core = Section(List(Continuation(List.fill(81)(MoveRef(pair.head, 0))))) ::
        chosen.confirmedFacts.core))
    assertEquals(oversized.realize(paths, pair), None)
    assertEquals(oversized.question, chosen.question)
    assertEquals(oversized.evidence, chosen.evidence)
    assert(oversized.answer.nonEmpty)
    val failed = MoveWorkProjection.unavailableAnswer(Some(oversized.question), oversized.realize(paths, pair))
    assertEquals((failed \ "summary_unavailable" \ "question").as[String], chosen.question)
    assertEquals((failed \ "summary_unavailable" \ "reason").as[String], "answer_not_delivered")
    assertEquals(MoveWorkProjection.unavailableAnswer(None, None), Json.obj())
    assertEquals(MoveWorkProjection.unavailableAnswer(Some(chosen.question), chosen.realize(paths, pair)), Json.obj())
  }

  test("repeated completion of an identical detail account retains each price once") {
    val (paths, pair, selected) = prepare(examples.find(_.name == "interleaved neutral trades").get)
    val answer = selected.get.confirmedFacts
    assert(answer.details.exists(_.covered.nonEmpty), "the example must own real detail prices")
    val baseline = RootQuestionSummary.realize(paths, pair, answer).get
    val repeated = answer.copy(details = List.fill(4)(answer.details).flatten)
    val delivered = RootQuestionSummary.realize(paths, pair, repeated).get
    assertEquals(delivered.text, baseline.text)
    assertEquals((delivered.json \ "units").as[List[play.api.libs.json.JsObject]],
      (baseline.json \ "units").as[List[play.api.libs.json.JsObject]])
    assertEquals((delivered.json \ "detail_units").as[List[play.api.libs.json.JsObject]],
      (baseline.json \ "detail_units").as[List[play.api.libs.json.JsObject]],
      "sharing an exact section must retain all its moves, captures and focus without repeating the account")
    assertEquals(repeated.required, answer.required)
    assertEquals(repeated.details.flatMap(_.covered).toSet, answer.details.flatMap(_.covered).toSet)
  }
