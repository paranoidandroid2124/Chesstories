package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

/** Legal open examples test meaning and ownership, not the quality of synthetic scores. */
class MaterialContrastTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import RootQuestionSummary.MoveRef

  private def prepare(fen: String, lines: List[String]) =
    val rows = lines.map(_.split(" ").toList)
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen,
      playedMoveUci = rows.head.head, variations = rows.map(EngineLine(_, 0, None, 16)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    (packet, paths, pair)

  private def fullAnswer(paths: ObservedLineProtocol.PreparedPaths,
      pair: List[ObservedLineProtocol.PreparedPath]): Answer =
    val subject = pair.flatMap(path => paths.continuationReadings.find(_.path eq path).get.events)
      .flatMap(_.originalVictim).head
    val heading = Section(List(ComparisonSubject(pair.map(MoveRef(_, 0)), List(subject))))
    val arms = pair.map { path =>
      val events = paths.continuationReadings.find(_.path eq path).get.events.filter(_.originalVictim.nonEmpty)
      val recaptures = path.certified.flatMap(_._2.material).get.captures.filter(_.recapture).map(_.plyOffset).toSet
      Section(Option.when(!events.exists(_.plyOffset == 0))(Continuation(List(MoveRef(path, 0)))).toList ++
        Option.when(events.nonEmpty)(PieceExchange(path, events, recaptures)).toList)
    }
    Answer("selected_comparison", heading :: arms, Nil, Nil, Some(ComparisonScope(Set(subject))))

  private val recaptureFen = "r1bqkbnr/pppp1ppp/2n5/8/3pP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 0 4"

  test("a complete recapture comparison leads with the knight exchange and queen price without owning a capture twice"):
    val (_, paths, pair) = prepare(recaptureFen, List("f3d4 c6d4 d1d4", "d1d4 c6d4"))
    val original = fullAnswer(paths, pair)
    val answer = RootSubjectComparison.withMaterialContrast(original, pair)
    assertEquals(answer.core.flatMap(_.covered), original.core.flatMap(_.covered))
    assertEquals(answer.core.head.statements.size, 2)
    assert(answer.core.head.statements.forall(_.isInstanceOf[MaterialBalance]))
    assertEquals(answer.comparison, original.comparison)
    assert(answer.details.flatMap(_.statements).exists(_.isInstanceOf[ComparisonSubject]))
    val delivered = RootQuestionSummary.realize(paths, pair, answer).get
    assert(delivered.text.startsWith("In the recorded Nxd4 line, both sides exchange one knight; Black also loses one pawn."), delivered.text)
    assert(delivered.text.contains("In the recorded Qxd4 line, White gives up one queen for one pawn."), delivered.text)
    assert(delivered.text.contains("recaptures that queen"), delivered.text)
    assertEquals(RootSubjectComparison.withMaterialContrast(answer, pair), answer)
    val playedQueen = RootSubjectComparison.withMaterialContrast(original, pair, Some(pair.last.moves.head))
    assert(RootQuestionSummary.realize(paths, pair, playedQueen).get.text.startsWith(
      "In the recorded Qxd4 line, White gives up one queen for one pawn."))
    assertEquals(playedQueen.core.flatMap(_.covered), original.core.flatMap(_.covered))

  test("certified no captures remains a finite observation, alongside the other arm's actual pawn loss"):
    val (_, paths, pair) = prepare("rnbqkb1r/pppp1ppp/5n2/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 2 2",
      List("f8c5 f3e5", "b8c6 b1c3"))
    val original = fullAnswer(paths, pair)
    val answer = RootSubjectComparison.withMaterialContrast(original, pair)
    val text = RootQuestionSummary.realize(paths, pair, answer).get.text
    assert(text.startsWith("In the recorded Bc5 line, Black loses one pawn."), text)
    assert(text.contains("In the recorded Nc6 line, no capture or promotion is recorded."), text)
    List("safe", "forced", "blunder", "better", "equal", "compensation").foreach(word => assert(!text.contains(word), text))

  test("a closed root exchange names its endpoint instead of absorbing a later unrelated pawn trade"):
    val (_, paths, pair) = prepare(recaptureFen,
      List("f3d4 g8f6 d4c6 b7c6 f1d3 d7d6 e1g1 d6d5 e4d5 c6d5", "d1d4 c6d4"))
    val full = fullAnswer(paths, pair)
    val later = pair.head.certified.flatMap(_._2.material).get.events.filter(_.plyOffset >= 8)
    val selected = full.copy(core = full.core.map(section => section.copy(statements = section.statements.map {
      case exchange: PieceExchange if exchange.path eq pair.head =>
        exchange.copy(events = exchange.events.filter(_.plyOffset < 8))
      case other => other
    })), details = List(Section(List(MaterialEvents(pair.head, later)))))
    val answer = RootSubjectComparison.withMaterialContrast(selected, pair)
    val balance = answer.core.head.statements.head.asInstanceOf[MaterialBalance]
    assertEquals(balance.through, Some(3))
    assertEquals(answer.core.flatMap(_.covered), selected.core.flatMap(_.covered))
    assert(answer.details.flatMap(_.statements).contains(MaterialEvents(pair.head, later)))
    val text = RootQuestionSummary.realize(paths, pair, answer).get.text
    assert(text.startsWith("Through bxc6 in the recorded Nxd4 line, both sides exchange one knight; Black also loses one pawn."), text)
    assertEquals(RootQuestionSummary.realize(paths, pair, answer.copy(core =
      answer.core.head.copy(statements = balance.copy(through = None) :: answer.core.head.statements.tail) :: answer.core.tail)), None,
      "a prefix cannot claim to price the whole line")
    assertEquals(RootQuestionSummary.realize(paths, pair, answer.copy(core =
      answer.core :+ Section(List(MaterialEvents(pair.head, later))))), None,
      "a selected later price cannot be hidden outside the overview")

  test("equal complete transactions do not acquire a differing-material overview"):
    val (_, paths, pair) = prepare("4k3/8/8/8/8/3n4/8/3Q1BK1 w - - 0 1",
      List("d1d3 e8e7", "f1d3 e8e7"))
    val original = fullAnswer(paths, pair)
    assertEquals(RootSubjectComparison.withMaterialContrast(original, pair), original)
    val (_, openingPaths, openingPair) = prepare("rnbqkb1r/pppp1ppp/5n2/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3",
      List("b1c3 b8c6", "d2d4 e5d4 f3d4"))
    val opening = fullAnswer(openingPaths, openingPair)
    assertEquals(RootSubjectComparison.withMaterialContrast(opening, openingPair), opening,
      "an ordinary equal pawn exchange is not an extra material loss")

  test("a partial selected exchange cannot hide the recapturer's later loss inside a whole-line balance"):
    val (_, paths, pair) = prepare("4r2k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1",
      List("f1e1 h8h7 c3b5 f6e4 e1e4 e8e4", "g1h1 h8h7"))
    val original = fullAnswer(paths, pair)
    val partial = original.copy(core = original.core.map(section => section.copy(statements = section.statements.map {
      case exchange: PieceExchange => exchange.copy(events = exchange.events.filterNot(_.plyOffset == 5))
      case other => other
    })))
    assertEquals(RootSubjectComparison.withMaterialContrast(partial, pair), partial)
    val complete = RootSubjectComparison.withMaterialContrast(original, pair)
    assert(complete.core.head.statements.exists(_.isInstanceOf[MaterialBalance]))
    assert(complete.core.flatMap(_.covered).contains(MoveRef(pair.head, 5)))

  test("promotion, absent replies and an unscoped answer are not admitted as simple material contrasts"):
    val (_, paths, pair) = prepare("7k/8/8/8/8/5R2/1p5K/R7 w - - 0 1",
      List("f3a3 b2a1q a3a1", "h2g2 h8h7"))
    val promotion = fullAnswer(paths, pair)
    assertEquals(RootSubjectComparison.withMaterialContrast(promotion, pair), promotion)
    val (_, shortPaths, shortPair) = prepare(recaptureFen, List("f3d4 c6d4 d1d4", "d1d4"))
    val short = fullAnswer(shortPaths, shortPair)
    assertEquals(RootSubjectComparison.withMaterialContrast(short, shortPair), short)
    assertEquals(RootSubjectComparison.withMaterialContrast(short.copy(comparison = None), shortPair), short.copy(comparison = None))

  test("a detached overview, forged empty ledger or omitted event is rejected at realization"):
    val (_, paths, pair) = prepare(recaptureFen, List("f3d4 c6d4 d1d4", "d1d4 c6d4"))
    val original = fullAnswer(paths, pair)
    val complete = RootSubjectComparison.withMaterialContrast(original, pair)
    val balances = complete.core.head
    assertEquals(RootQuestionSummary.realize(paths, pair, complete.copy(core = List(balances))), None)
    assertEquals(RootQuestionSummary.realize(paths, pair, original.copy(core = List(original.core.head))), None,
      "naming a capture question does not discharge either root's actual capture")
    assertEquals(RootQuestionSummary.realize(paths, pair, complete.copy(core =
      Section(List(MaterialBalance(pair.head, None))) :: complete.core.tail)), None)
    val left = balances.statements.head.asInstanceOf[MaterialBalance]
    val material = pair.head.certified.flatMap(_._2.material).get
    val partial = CommentaryMaterialAccount.calculate(material, List(0), List(0), pair.head.moves.size).get
    assertEquals(RootQuestionSummary.realize(paths, pair, complete.copy(core =
      balances.copy(statements = left.copy(cost = Some(partial)) :: balances.statements.tail) :: complete.core.tail)), None)
