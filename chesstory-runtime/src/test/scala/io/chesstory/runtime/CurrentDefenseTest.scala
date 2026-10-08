package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.*

class CurrentDefenseTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import RootQuestionSummary.MoveRef

  private val fen = "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
  private val exposed = "f8c5 f3e5 b8c6 e5c6 d7c6 d2d3 f7f5 d1h5 g7g6 h5e2 g8f6 e4f5 e8f7 f5g6 h7g6"
  private val defended = "b8c6 b1c3 g8f6 f1b5 f8d6 b5c6 d7c6 d2d4 e5d4 d1d4 e8g8"

  private def prepare(position: String = fen, rows: List[String] = List(exposed, defended)) =
    val lines = rows.map(row => EngineLine(row.split(" ").toList, 0, None, 16))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = position,
      playedMoveUci = lines.head.moves.head, variations = lines)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = lines.map(line => paths.paths.find(_.moves == line.moves).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val previous = RootSubjectComparison.answer(packet, paths, pair, positions).getOrElse(
      RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get)
    (packet, paths, pair, positions, previous)

  test("the current unprotected target and actual reply lead before development and distant history"):
    val (packet, paths, pair, positions, previous) = prepare()
    val selected = RootSubjectComparison.withImmediateDefense(packet, paths, pair, positions, previous)
    assertEquals(selected.question, "current_defense")
    val notes = MoveWorkProjection.select(packet, paths).local
    val answer = MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, selected).get
    val result = RootQuestionSummary.realize(paths, pair, answer).get
    assert(result.text.startsWith("After Bc5, Black's pawn on e5 is under attack"), result.text)
    assert(result.text.contains("has no defender") && result.text.contains("Nxe5"), result.text)
    assert(result.text.indexOf("Nxe5") < result.text.indexOf("After Nc6"), result.text)
    assert(result.text.contains("supported by Black's knight on c6"), result.text)
    assert(!result.text.contains("still present") && !result.text.contains("must") && !result.text.contains("forced"), result.text)
    assert(previous.core.flatMap(_.covered).forall(answer.core.flatMap(_.covered).contains))
    assertEquals(answer.core.flatMap(_.covered).distinct.size, answer.core.flatMap(_.covered).size)
    assert(previous.core.flatMap(_.statements).forall(answer.details.flatMap(_.statements).contains))

  test("the played supporting move stays first when the weaker alternative has the immediate capture"):
    val (packet, paths, pair, positions, previous) = prepare(rows = List(defended, exposed))
    val answer = RootSubjectComparison.withImmediateDefense(packet, paths, pair, positions, previous)
    val text = RootQuestionSummary.realize(paths, pair, answer).get.text
    assert(text.startsWith("After Nc6"), text)
    assert(text.indexOf("After Nc6") < text.indexOf("After Bc5"), text)

  test("a defended pawn capture cannot be described as an unanswered undefended attack"):
    val position = "rnbqkbnr/ppp2ppp/3p4/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
    val (packet, paths, pair, positions, previous) = prepare(position,
      List("f8e7 f3e5 d6e5", "b8c6 b1c3"))
    assertEquals(RootSubjectComparison.withImmediateDefense(packet, paths, pair, positions, previous), previous)

  test("two different immediate defensive questions do not pick an arbitrary single pawn"):
    val (packet, paths, pair, positions, previous) = prepare(
      "6k1/8/5n2/7P/4P3/8/1K6/R7 w - - 0 1", List("a1e1 f6h5", "a1h1 f6e4"))
    assertEquals(RootSubjectComparison.withImmediateDefense(packet, paths, pair, positions, previous), previous)

  test("no first-reply capture means no inferred failure of current defence"):
    val (packet, paths, pair, positions, previous) = prepare(rows =
      List("f8c5 d2d3 b8c6", "b8c6 b1c3"))
    assertEquals(RootSubjectComparison.withImmediateDefense(packet, paths, pair, positions, previous), previous)

  test("a root-only alternative cannot complete a current defence comparison"):
    val (packet, paths, pair, positions, previous) = prepare(rows = List(exposed, "b8c6"))
    assertEquals(RootSubjectComparison.withImmediateDefense(packet, paths, pair, positions, previous), previous)

  test("realization rejects a forged missing defender on the exact supported board"):
    val (packet, paths, pair, positions, previous) = prepare()
    val answer = RootSubjectComparison.withImmediateDefense(packet, paths, pair, positions, previous)
    val forged = answer.copy(core = answer.core.map(section => section.copy(statements = section.statements.map {
      case value: TargetDefense if value.defenders.nonEmpty => value.copy(defenders = Nil)
      case value => value
    })))
    assertEquals(RootQuestionSummary.realize(paths, pair, forged), None)

  test("a current defence does not break an already protected root-use and price episode"):
    val (packet, paths, pair, positions, previous) = prepare("7k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1",
      List("f1e1 h8h7 c3b5 f6e4 e1e4", "g1h1 f6e4 f1f7 h8g8"))
    val path = pair.head
    val rook = CommentaryFact.Piece("white", "rook", "f1").witness
    val pawn = CommentaryFact.Piece("white", "pawn", "e4").witness
    val use = LineContinuationInvestigation.directSupportUse(packet, path, paths, path.moves.size, rook, pawn).uses.head
    val receipt = SupportedCapture(MoveRef(path, 0), use.provider, pawn, use, true, None, None)
    val episode = Section(receipt :: previous.core.flatMap(_.statements).filter(_.covered.nonEmpty))
    val protectedPrevious = previous.copy(core = List(episode))
    val answer = RootSubjectComparison.withImmediateDefense(packet, paths, pair, positions, protectedPrevious)
    assert(answer.core.exists(_.statements == episode.statements))

  test("counterplay against another pawn retains its actual use and the attacking knight's later loss"):
    val petrov = "g8f6 f3e5 d7d6 e5f3 f6e4 d2d4 d6d5 f1d3 f8e7 e1g1 e8g8 c2c4 b8c6 f1e1 c8f5 b1c3 e4c3 b2c3 f5d3 d1d3 d5c4 d3c4 e7d6"
    val (packet, paths, pair, positions, previous) = prepare(rows = List(petrov, defended))
    val answer = RootSubjectComparison.withImmediateDefense(packet, paths, pair, positions, previous)
    val completed = MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, Nil, answer).get
    val result = RootQuestionSummary.realize(paths, pair, completed).get
    assert(result.text.contains("targets White's pawn on e4") && result.text.contains("Nxe4"), result.text)
    val covered = completed.core.flatMap(_.covered).filter(_.path eq pair.head).map(_.offset).toSet
    assert(Set(1, 4, 16, 17).subsetOf(covered), "the offered pawn, countercapture and used knight's price stay visible")
    assert(!result.text.contains("uncompensated") && !result.text.contains("wins a pawn"), result.text)

  test("moving the threatened pawn out of attack is a current answer alongside adding support"):
    val (packet, paths, pair, positions, previous) = prepare(
      "6k1/5p2/5n2/8/4P3/8/1P6/1N3BK1 w - - 0 1",
      List("f1c4 f6e4 b1c3 e4c3 b2c3", "e4e5 f6d7 f1c4 g8h8"))
    val answer = RootSubjectComparison.withImmediateDefense(packet, paths, pair, positions, previous)
    assertEquals(answer.question, "current_defense")
    val text = RootQuestionSummary.realize(paths, pair, answer).get.text
    assert(text.contains("After Bc4") && text.contains("Nxe4"), text)
    assert(text.contains("e5 moves the pawn out of the attack from Black's knight on f6"), text)
    assert(!text.contains("e5 is safe") && !text.contains("must"), text)
