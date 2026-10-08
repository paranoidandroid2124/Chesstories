package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

/** Recorded NB1/Nc4 rows supply legal facts and the original numerical gap.
  * The local Rd6 counterexample is a declared conditional example, not a
  * replacement engine receipt or a proof of the cause of Nb1's grade.
  * These tests integrate the real functional/protection/completion seam;
  * production of the original physical bundle boundaries needs a real replay.
  */
class PhysicalEpisodeBoundaryTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import CommentaryFact.{Piece as FactPiece, Predicate}
  import CommentaryProposal.CompareMoves
  import MoveWorkProjection.Position
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef

  private def moves(value: String) = value.split(" ").toList
  private def piece(side: String, role: String, square: String) = FactPiece(side, role, square).witness
  private def statements(answer: Answer) = answer.core.flatMap(_.statements)
  private def captures(answer: Answer, path: PreparedPath) = answer.core.flatMap(_.covered)
    .filter(_.path eq path).map(_.offset).toSet

  private val fen = "3rnrk1/3n1pbp/1pq3p1/2p1p3/4P3/4B1PP/1PPNQPB1/R2R2K1 w - - 2 19"
  private val reference = moves("d2c4 e8f6 d1d6 c6c7 a1d1 b6b5 c4a3 b5b4 a3c4 f8e8 e2f3 h7h6 g2f1 g7f8 d6a6 g8g7 h3h4 d8a8 d1a1 a8a6 a1a6")
  private val played = moves("d2b1 e8c7 e2d3 c5c4 d3d2 c7e6 b1c3 d7f6 c3d5 f6d5 e4d5 c6b5 d5e6 d8d2 d1d2 f7e6 a1a7 b5b2 d2d7 g7f6 e3h6 b2c2 h6f8")
  private val left = moves("d2c4 e8f6 d1d6 c6d6")
  private val right = moves("d2b1 e8f6 d1d6 c6d6")
  private val option = left :+ "c4d6"

  private case class Fixture(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], positions: Map[PositionNodeRef, Position],
      notes: List[MoveWorkProjection.LocalCandidate], proof: FunctionalReplyReading.Proof,
      previous: Answer, queenUse: Statement, rookUse: Statement,
      queenBackground: Statement, rookRoute: Statement):
    def answer(at: Map[PositionNodeRef, Position] = positions): Answer =
      FunctionalReplyReading.withAnswer(packet, paths, pair, List(proof), previous,
        (answer, sections) => MoveWorkProjection.protectRootUses(packet, paths, pair, at, notes, answer, sections))
    def close(answer: Answer): Answer = MoveWorkProjection.completeSelectedConditions(
      packet, paths, pair, positions, notes, answer).getOrElse(fail("the separated episodes did not close"))

  private def fixture(): Fixture =
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen,
      playedMoveUci = played.head, variations = List(
        EngineLine(reference, 175, None, 16), EngineLine(played, 109, None, 16)))).get
    val local = List(left, right, option).map { line =>
      val history = input.positionHistory.admitToFirstAutomaticTerminal(line).toOption.get
      assertEquals(history.stepsAfter(input.positionHistory).get.map(_.uci), line)
      history
    }
    val retained = MoveReviewInputAdmission.retainLegalContinuations(input, local)
    // The original scored comparison needs the public relative assessment.
    // The added legal conditional branches themselves have no engine score.
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(retained)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = List(reference, played).map(line => paths.certifiedPath(packet, line).get)
    val positions = packet.resources.map(view => view.position -> new Position(view)).toMap
    val notes = MoveWorkProjection.select(packet, paths).local
    val rook = pair.head
    val queen = pair.last
    val knight = piece("white", "knight", "d2")
    val queenUse = SquareReoccupation(MoveRef(queen, 0), knight, piece("white", "knight", "b1"),
      "d2", MoveRef(queen, 4), piece("white", "queen", "e2"), piece("white", "queen", "d2"))
    val rookUse = ClearedRoute(MoveRef(rook, 0), knight, piece("white", "knight", "c4"),
      piece("white", "rook", "d1"), MoveRef(rook, 2), piece("white", "rook", "d6"),
      List(piece("black", "queen", "c6")), passage = true, development = None)
    val queenBackground = PiecePresence(MoveRef(queen, 22), piece("black", "queen", "c6"),
      piece("black", "queen", "c2"))
    val rookRoute = PieceRoute(rook, piece("white", "rook", "d1"), List(2, 14))
    def material(path: PreparedPath, offsets: Set[Int]) = MaterialEvents(path,
      path.certified.flatMap(_._2.material).get.events.filter(event => offsets(event.plyOffset)))
    val recorded = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    val previous = recorded.copy(core = List(
      Section(List(queenUse, material(queen, Set(13, 14)), queenBackground)),
      Section(List(rookUse, rookRoute, material(rook, Set(19, 20))))),
      comparison = Some(ComparisonScope(Set(knight, piece("white", "queen", "e2"), piece("white", "rook", "d1")))))
    val actor = FactPiece("white", "knight", "d2")
    val capture = Predicate("legal_capture", Some(actor), Some(FactPiece("black", "queen", "c6")))
    val proof = FunctionalReplyReading.Proof(FunctionalReplyReading.RootOwner("declared-nb1-rd6-example",
      CompareMoves("function_root", Nil, left.head, right.head)),
      Predicate("control", Some(actor), square = Some("d6")), left, right, option, capture, capture, Nil)
    Fixture(packet, paths, pair, positions, notes, proof, previous, queenUse, rookUse, queenBackground, rookRoute)

  test("one relocated quiet user keeps only its own episode while a second root question closes its actual queen price"):
    val f = fixture()
    val closed = f.close(f.answer())
    assertEquals(closed.question, "functional_response")
    assert(statements(closed).exists(_.isInstanceOf[LegalReplyOption]))
    assert(List(f.queenUse, f.rookUse, f.rookRoute).forall(statements(closed).contains),
      "the independent root uses survive; Rd6's relocated user does not receive a fabricated stationary account")
    assert(!statements(closed).contains(f.queenBackground),
      "the other arm's Qc2 endpoint does not become core because the Rd6 account cannot narrow")
    assert(closed.details.flatMap(_.statements).contains(f.queenBackground))
    assertEquals(captures(closed, f.pair.last), Set(13, 14),
      "the original Qd2 user is actually lost and its capturer is actually recovered")
    assertEquals(captures(closed, f.pair.head), Set(19, 20),
      "the conservative Rd6/Ra6 episode keeps its observed rook loss and recovery")
    assert(!closed.completionSources.get.contains(f.queenBackground))
    assert(!closed.completionSources.get.flatMap(_.covered).exists(ref =>
      (ref.path eq f.pair.last) && Set(13, 14)(ref.offset)),
      "a price created by the independently closed Qd2 use cannot become a new idea seed")
    val owned = closed.core.flatMap(_.covered)
    assertEquals(owned.distinct.size, owned.size, "shared captures retain one core owner")
    val repeated = f.close(closed)
    assertEquals(repeated.completionSources, closed.completionSources)
    assertEquals(captures(repeated, f.pair.last), Set(13, 14))
    assertEquals(captures(repeated, f.pair.head), Set(19, 20))
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("missing identity retains the unclosed episode and conditional purpose does not replace the actual mistake assessment"):
    val f = fixture()
    val primary = f.packet.primary.map {
      case PlayerFacingPrimary.MoveVerdict(ref) => ref
      case PlayerFacingPrimary.BestChoice(ref) => ref
    }.get
    val numerical = f.packet.evidenceGraph.candidateComparisonRecord(primary).get.payload match
      case CandidateComparisonEvidence(value) => value
      case _ => fail("the recorded comparison did not retain its numerical assessment")
    assertEquals(numerical.comparison.verdict, MoveChoiceVerdict.Mistake,
      "the declared Rd6 example cannot reclassify the actual engine gap as a routine close comparison")
    assertEquals(BasicPurposeReading.simplify(f.packet, f.paths, f.pair,
      f.positions, f.notes, f.previous), f.previous)
    val answer = f.answer(f.positions - f.pair.last.positions(10))
    assert(statements(answer).contains(f.queenUse) && statements(answer).contains(f.queenBackground),
      "without the stationary user's intermediate identity, its complete old episode remains")
    assert(Set(13, 14).subsetOf(captures(answer, f.pair.last)))
    assert(statements(answer).contains(f.rookUse) && statements(answer).contains(f.rookRoute))
    val finalAnswer = f.close(answer)
    assertEquals(finalAnswer.question, "functional_response")
    assertEquals(f.packet.evidenceGraph.candidateComparisonRecord(primary).get.payload,
      CandidateComparisonEvidence(numerical), "neither closure nor conditional purpose changes a numerical receipt")
    val delivered = RootQuestionSummary.realize(f.paths, f.pair, finalAnswer).get
    List("forced", "compensates", "winning", "best move", "evaluation").foreach(word =>
      assert(!delivered.text.toLowerCase.contains(word), delivered.text))

  test("repeated protection keeps independently closed prices separate from the unclosed relocated user"):
    val f = fixture()
    val numerical = CommentarySelection.primaryComparisonRecords(f.packet).head._2
    val once = f.close(f.answer())
    val protectedAgain = FunctionalReplyReading.withAnswer(f.packet, f.paths, f.pair,
      List(f.proof), once, (answer, sections) => MoveWorkProjection.protectRootUses(
        f.packet, f.paths, f.pair, f.positions, f.notes, answer, sections))
    val repeated = f.close(protectedAgain)
    assertEquals(repeated.question, "functional_response")
    assert(List(f.queenUse, f.rookUse, f.rookRoute).forall(statements(repeated).contains))
    assert(!repeated.core.exists(section =>
      section.statements.contains(f.queenUse) && section.statements.contains(f.rookUse)),
      "a later protection pass must retain the two independently owned physical episodes")
    assert(!statements(repeated).contains(f.queenBackground),
      "Rd6's unresolved relocated user cannot promote Qd2's Qc2 background again")
    assert(repeated.details.flatMap(_.statements).contains(f.queenBackground))
    assertEquals(captures(repeated, f.pair.last), Set(13, 14))
    assertEquals(captures(repeated, f.pair.head), Set(19, 20))
    val sources = repeated.completionSources.get
    assert(!sources.contains(f.queenBackground))
    assert(!sources.flatMap(_.covered).exists(ref =>
      (ref.path eq f.pair.last) && Set(13, 14)(ref.offset)),
      "regenerated Qd2 prices remain completion obligations and never become original idea seeds")
    val owned = repeated.core.flatMap(_.covered)
    assertEquals(owned.distinct.size, owned.size)
    assertEquals(CommentarySelection.primaryComparisonRecords(f.packet).head._2, numerical)
    assert(RootQuestionSummary.realize(f.paths, f.pair, repeated).nonEmpty)
