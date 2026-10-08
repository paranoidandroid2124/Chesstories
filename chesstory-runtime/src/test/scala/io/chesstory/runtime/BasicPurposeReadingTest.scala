package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

class BasicPurposeReadingTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import RootQuestionSummary.MoveRef

  private val pirc = "rnbqkb1r/ppp1pp1p/3p1np1/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 4"
  private val prefix = List("e2e4", "d7d6", "d2d4", "g8f6", "b1c3", "g7g6")
  private val quiet = List("g1f3 f8g7 c1e3 e8g8 h2h3 b7b6 e4e5 f6e8 f1d3 c8b7",
    "c1e3 f8g7 d1d2 e8g8 e1c1 c7c6 h2h4 b7b5 f2f3 d8a5 c1b1 e7e5 g2g4 e5d4 e3d4 b5b4 h4h5 b4c3 d4c3 f6e4 c3a5 e4d2 a5d2")

  private def prepare(fen: String, rows: List[String], moves: List[String] = Nil,
      scores: List[Int] = List(0, 0), mates: List[Option[Int]] = List(None, None)) =
    val lines = rows.zipWithIndex.map((row, index) => EngineLine(row.split(" ").toList,
      scores(index), mates(index), 16))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen, playedMoveUci = lines.head.moves.head,
      variations = lines, movePrefixUci = moves)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = lines.map(line => paths.paths.find(_.moves == line.moves).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(packet, paths).local
    (packet, paths, pair, positions, notes)

  private def prior(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths,
      pair: List[ObservedLineProtocol.PreparedPath]) =
    RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get

  test("a quiet close development comparison is answered by current purposes, with its future exchanges retained in detail"):
    val (packet, paths, pair, positions, notes) = prepare(pirc, quiet, prefix)
    val bishop = CommentaryFact.Piece("white", "bishop", "c1").witness
    val pawn = CommentaryFact.Piece("white", "pawn", "d4").witness
    val use = LineContinuationInvestigation.directSupportUse(packet, pair.last, paths,
      pair.last.moves.size, bishop, pawn).uses.head
    val receipt = SupportedCapture(MoveRef(pair.last, 0), use.provider, pawn, use, true, None, None)
    val lead = List(Section(List(Continuation(List(MoveRef(pair.head, 0))))), Section(List(receipt)))
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", lead).get
    val answer = BasicPurposeReading.simplify(packet, paths, pair, positions, notes, previous)
    assert(answer != previous)
    assert(answer.core.flatMap(_.statements).forall {
      case Action(move, _: MoveWorkProjection.Detail.Development) => move.offset == 0
      case _ => false
    })
    assert(answer.details.flatMap(_.statements).contains(receipt))
    assert(previous.core.flatMap(_.covered).forall(answer.details.flatMap(_.covered).contains))
    val delivered = RootQuestionSummary.realize(paths, pair, answer).get
    assert(delivered.text.contains("Nf3 develops the knight") && delivered.text.contains("Be3 develops the bishop"), delivered.text)
    assert(delivered.text.startsWith("Nf3 develops the knight"), delivered.text)
    assert(!delivered.text.contains("exchange") && !delivered.text.contains("safe"), delivered.text)
    assert(delivered.json.toString.contains("Bxd4") == false, "notation is linked by exact occurrence, not copied into the contract")
    assertEquals(BasicPurposeReading.simplify(packet, paths, pair, positions, notes, answer).core, answer.core)

  test("development that supplies an attacked pawn's defence keeps the current obligation and its observed prices"):
    val fen = "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
    val (packet, paths, pair, positions, notes) = prepare(fen, List("b8c6 b1c3", "f8c5 f3e5"),
      List("e2e4", "e7e5", "g1f3"))
    val previous = prior(packet, paths, pair)
    assertEquals(BasicPurposeReading.simplify(packet, paths, pair, positions, notes, previous), previous)

  test("a certified played purpose can precede a fuller comparison when the alternative has no such purpose"):
    val rows = List("g1f3 d7d5", "e2e4 e7e5")
    val (packet, paths, pair, positions, notes) = prepare("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", rows)
    val previous = prior(packet, paths, pair)
    val answer = BasicPurposeReading.simplify(packet, paths, pair, positions, notes, previous)
    assertEquals(answer.comparison, previous.comparison)
    assertEquals(answer.details, previous.details)
    assert(previous.core.flatMap(_.statements).forall(answer.core.flatMap(_.statements).contains))
    assert(previous.required.forall(answer.required.contains))
    val delivered = RootQuestionSummary.realize(paths, pair, answer).get
    assert(delivered.text.startsWith("Nf3 develops the knight"), delivered.text)
    assert(delivered.text.contains("e4"), delivered.text)
    assertEquals(BasicPurposeReading.simplify(packet, paths, pair, positions, notes, answer), answer)
    val (reverse, reversedPaths, reversedPair, reversedPositions, reversedNotes) = prepare(
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", rows.reverse)
    val original = prior(reverse, reversedPaths, reversedPair)
    assertEquals(BasicPurposeReading.simplify(reverse, reversedPaths, reversedPair,
      reversedPositions, reversedNotes, original), original, "the alternative's familiar purpose cannot displace the played move")

  test("an existing played purpose is read before its arm's later continuation without changing the selected content"):
    val rows = List("g1f3 d7d5", "e2e4 e7e5")
    val (packet, paths, pair, positions, notes) = prepare("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", rows)
    val root = MoveRef(pair.head, 0)
    val note = notes.find(note => note.edge.evidence.id == pair.head.ids.head &&
      note.note.detail.isInstanceOf[MoveWorkProjection.Detail.Development]).get
    val action = Action(root, note.note.detail)
    val base = prior(packet, paths, pair)
    val owner = base.core.indexWhere(_.references.contains(root))
    val section = base.core(owner)
    val previous = base.copy(core = base.core.updated(owner,
      section.copy(statements = section.statements :+ action)))
    val answer = BasicPurposeReading.simplify(packet, paths, pair, positions, notes, previous)
    assertEquals(answer.core(owner).statements.head, action)
    assertEquals(answer.core.flatMap(_.statements).toSet, previous.core.flatMap(_.statements).toSet)
    assertEquals(answer.core.flatMap(_.covered), previous.core.flatMap(_.covered))
    assertEquals(answer.required, previous.required)
    assertEquals(answer.comparison, previous.comparison)
    assertEquals(answer.details, previous.details)
    assert(RootQuestionSummary.realize(paths, pair, answer).get.text.startsWith("Nf3 develops the knight"))
    assertEquals(BasicPurposeReading.simplify(packet, paths, pair, positions, notes, answer), answer)

  test("a rook's loss of an attacked ally's existing support cannot be hidden by an open-file purpose"):
    val (packet, paths, pair, positions, notes) = prepare("3n2k1/8/8/2b5/8/8/5P2/5RK1 w - - 0 1",
      List("f1e1 g8h8", "f1a1 g8h8"))
    val previous = prior(packet, paths, pair)
    assert(notes.exists(_.note.detail.isInstanceOf[MoveWorkProjection.Detail.RookFile]))
    assertEquals(BasicPurposeReading.simplify(packet, paths, pair, positions, notes, previous), previous)

  test("a close development comparison names its present attack without importing a remote exchange"):
    val fen = "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
    val (packet, paths, pair, positions, notes) = prepare(fen, List("f1b5 a7a6", "f1e2 g8f6"),
      List("e2e4", "e7e5", "g1f3", "b8c6"))
    val previous = prior(packet, paths, pair)
    val answer = BasicPurposeReading.simplify(packet, paths, pair, positions, notes, previous)
    assert(answer != previous)
    val text = RootQuestionSummary.realize(paths, pair, answer).get.text
    assert(text.contains("Bb5 develops the bishop") && text.contains("attacks Black's knight on c6"), text)
    assert(!text.contains("wins") && !text.contains("better") && !text.contains("exchange"), text)
    assert(previous.core.flatMap(_.covered).forall(answer.details.flatMap(_.covered).contains))

  test("a short purpose preserves the actual first capture and its recapture in basic text"):
    val fen = "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/1P6/P2PPPPP/RNBQKBNR w KQkq - 1 3"
    val (packet, paths, pair, positions, notes) = prepare(fen,
      List("b1c3 d5c4 b3c4", "c1b2 e7e6"), List("c2c4", "d7d5", "b2b3", "g8f6"))
    val previous = prior(packet, paths, pair)
    val answer = BasicPurposeReading.simplify(packet, paths, pair, positions, notes, previous)
    val text = RootQuestionSummary.realize(paths, pair, answer).get.text
    assert(text.contains("dxc4") && text.contains("bxc4"), text)
    assert(answer.core.flatMap(_.covered).contains(MoveRef(pair.head, 1)))
    assert(answer.core.flatMap(_.covered).contains(MoveRef(pair.head, 2)))

  test("an error grade, mate forecast, explicit other question, absent board or unobserved reply retains the fuller answer"):
    val (packet, paths, pair, positions, notes) = prepare(pirc, quiet, prefix, List(-500, 0))
    val previous = prior(packet, paths, pair)
    assertEquals(BasicPurposeReading.simplify(packet, paths, pair, positions, notes, previous), previous)
    val (matePacket, matePaths, matePair, matePositions, mateNotes) = prepare(pirc, quiet, prefix,
      List(0, 0), List(Some(5), Some(4)))
    val mate = prior(matePacket, matePaths, matePair)
    assertEquals(BasicPurposeReading.simplify(matePacket, matePaths, matePair, matePositions, mateNotes, mate), mate)
    val (goodPacket, goodPaths, goodPair, goodPositions, goodNotes) = prepare(pirc, quiet, prefix)
    val good = prior(goodPacket, goodPaths, goodPair)
    val explicit = good.copy(question = "investigated_mechanism")
    assertEquals(BasicPurposeReading.simplify(goodPacket, goodPaths, goodPair, goodPositions, goodNotes, explicit), explicit)
    assertEquals(BasicPurposeReading.simplify(goodPacket, goodPaths, goodPair, Map.empty, goodNotes, good), good)
    val (shortPacket, shortPaths, shortPair, shortPositions, shortNotes) = prepare(pirc,
      List("g1f3", "c1e3 f8g7"), prefix)
    val short = prior(shortPacket, shortPaths, shortPair)
    assertEquals(BasicPurposeReading.simplify(shortPacket, shortPaths, shortPair, shortPositions, shortNotes, short), short)
