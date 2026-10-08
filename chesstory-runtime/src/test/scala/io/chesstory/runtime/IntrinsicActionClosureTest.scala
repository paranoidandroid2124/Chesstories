package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

/** Intrinsic duties of already selected actual actions. Legal recorded rows
  * supply facts; these regressions do not run an engine or assert move value.
  */
class IntrinsicActionClosureTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import MoveWorkProjection.{Detail, Position}
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef

  private def piece(side: String, role: String, square: String) = CommentaryFact.Piece(side, role, square).witness
  private def statements(answer: Answer) = answer.core.flatMap(_.statements)
  private def captures(answer: Answer, path: PreparedPath) =
    answer.core.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet
  private def checks(answer: Answer) = statements(answer).flatMap {
    case value: CheckSequence => value.events
    case value: CheckEvent => List(value)
    case _ => Nil
  }
  private def oneCaptureOwner(answer: Answer): Unit =
    val owned = answer.core.flatMap(_.covered)
    assertEquals(owned.distinct.size, owned.size, "each actual capture has one selected owner")

  private case class Fixture(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], positions: Map[PositionNodeRef, Position],
      notes: List[MoveWorkProjection.LocalCandidate]):
    def arms = RootQuestionSummary.prepare(packet, paths, pair, positions).get
    def answer(content: List[Statement], scope: Option[ComparisonScope] = None): Answer =
      val recorded = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
      recorded.copy(core = List(Section(Continuation(pair.map(MoveRef(_, 0))) :: content)), comparison = scope)
    def allMaterial(path: PreparedPath) = MaterialEvents(path, path.certified.flatMap(_._2.material).get.events)
    def protect(answer: Answer, episodes: List[Section]): Answer =
      MoveWorkProjection.protectRootUses(packet, paths, pair, positions, notes, answer, episodes)
    def close(answer: Answer): Answer =
      MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, answer)
        .getOrElse(fail("the selected actual actions did not pass common closure"))

  private def prepare(fen: String, rows: String*): Fixture =
    val lines = rows.toList.map(row => EngineLine(row.split(" ").toList, 0, None, 16))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen,
      playedMoveUci = lines.head.moves.head, variations = lines))
      .getOrElse(fail("the supplied root position was not admitted"))
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input))
      .getOrElse(fail("the legal rows did not produce an evidence-backed packet"))
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = lines.map(line => paths.paths.find(_.moves == line.moves)
      .getOrElse(fail(s"the complete legal row was not certified: ${line.moves.mkString(" ")}")))
    Fixture(packet, paths, pair, packet.resources.map(view => view.position -> new Position(view)).toMap,
      MoveWorkProjection.select(packet, paths).local)

  test("an already selected capturing check and its capturing reply close their intrinsic trade without new price sources"):
    // Pa2 survives Bxg2 so the independent checking reply remains before an automatic ending.
    val f = prepare("8/8/5n2/5b2/4P2k/8/P5P1/5RK1 w - - 0 1",
      "f1e1 f6e4 e1e4 f5e4 g1h1 e4g2 h1g1 h4g5", "g1h1 h4g5")
    val path = f.pair.head
    val events = f.arms.head.checkEvents(List(2, 3))
    assert(events.exists { case value: CheckingMove => value.move == MoveRef(path, 2); case _ => false })
    assert(events.exists { case value: CheckReply => value.move == MoveRef(path, 3); case _ => false })
    val sequence = CheckSequence(MoveRef(path, 2), events)
    val selected = f.answer(List(sequence))
    assertEquals(captures(selected, path), Set.empty[Int], "typed check facts do not already own the captures")
    val sources = statements(selected)
    val closed = f.close(selected)
    assert(statements(closed).contains(sequence))
    assertEquals(captures(closed, path), Set(1, 2, 3),
      "Nxe4, Rxe4+ and Bxe4 are the actual immediate trade of the selected check")
    assertEquals(f.arms.head.captures.find(_.plyOffset == 3).get.originalVictim,
      Some(piece("white", "rook", "f1")))
    assert(closed.required.contains(MoveRef(path, 3)))
    Set(5, 6).foreach(offset => assert(!closed.required.contains(MoveRef(path, offset)),
      "the later Bxg2+ is a separate capture and checking episode"))
    assert(closed.details.flatMap(_.covered).contains(MoveRef(path, 5)))
    assertEquals(closed.completionSources, Some(sources))
    assertEquals(closed.completionSources.get.flatMap(_.covered), Nil,
      "the added intrinsic trade is an obligation, not a newly selected price story")
    val again = f.close(closed)
    assertEquals(captures(again, path), Set(1, 2, 3))
    assertEquals(again.completionSources, closed.completionSources)
    oneCaptureOwner(closed)
    oneCaptureOwner(again)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("a quiet vacancy user closes the checking root reply before narrowing away an unrelated king route"):
    val f = prepare("1k6/8/8/8/8/8/R7/R5K1 w - - 0 1",
      "a1b1 b8c8 a2a1 c8d8", "g1h1 b8c8")
    val path = f.pair.head
    val root = piece("white", "rook", "a1")
    val user = piece("white", "rook", "a2")
    val rootEvents = f.arms.head.checkEvents(List(0, 1))
    assert(rootEvents.exists { case value: CheckingMove => value.move == MoveRef(path, 0); case _ => false })
    assert(rootEvents.exists { case value: CheckReply => value.move == MoveRef(path, 1); case _ => false })
    val sequence = CheckSequence(MoveRef(path, 0), rootEvents)
    val vacancy = SquareReoccupation(MoveRef(path, 0), root, piece("white", "rook", "b1"),
      "a1", MoveRef(path, 2), user, piece("white", "rook", "a1"))
    val background = PieceRoute(path, piece("black", "king", "b8"), List(3))
    val selected = f.answer(List(sequence), Some(ComparisonScope(Set(root, user))))
    val protectedAnswer = f.protect(selected, List(Section(List(vacancy, background))))
    assert(statements(protectedAnswer).contains(vacancy))
    assert(!statements(protectedAnswer).contains(background),
      "the complete quiet account must not fall back merely because the checking root needs its reply")
    assert(protectedAnswer.details.flatMap(_.statements).contains(background))
    val closed = f.close(protectedAnswer)
    assert(statements(closed).contains(sequence) && statements(closed).contains(vacancy))
    Set(0, 1, 2).foreach(offset => assert(closed.required.contains(MoveRef(path, offset))))
    assert(!closed.required.contains(MoveRef(path, 3)))
    assertEquals(captures(closed, path), Set.empty[Int])
    assert(closed.completionSources.get.contains(vacancy))
    assert(!closed.completionSources.get.contains(background))
    assert(checks(closed).contains(rootEvents.head))
    assertEquals(f.close(closed).required.toSet, closed.required.toSet)
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("an exact quiet attack keeps its capturing target arrival without selecting the target's later lifetime"):
    val f = prepare("6k1/8/2b5/8/P7/8/R6R/5NK1 w - - 0 1",
      "a2a1 c6a4 h2a2 a4b5 g1h1 b5f1 a1f1 g8h7", "g1f2 g8f7")
    val path = f.pair.head
    val root = piece("white", "rook", "a2")
    val user = piece("white", "rook", "h2")
    val bishop = piece("black", "bishop", "c6")
    val vacancy = SquareReoccupation(MoveRef(path, 0), root, piece("white", "rook", "a1"),
      "a2", MoveRef(path, 2), user, piece("white", "rook", "a2"))
    val attackNote = f.notes.find(note => note.edge.evidence.id == path.ids(2) && (note.note.detail match
      case Detail.Attack(actor, target, true) => actor == piece("white", "rook", "a2") &&
        target == piece("black", "bishop", "a4")
      case _ => false)).getOrElse(fail("the exact quiet user's actual attack was not observed"))
    val attack = Action(MoveRef(path, 2), attackNote.note.detail)
    val background = PieceRoute(path, bishop, List(1, 3, 5))
    val episode = Section(List(vacancy, attack, f.allMaterial(path), background),
      focus = List(Focus(MoveRef(path, 2), attackNote)))
    val protectedAnswer = f.protect(f.answer(Nil, Some(ComparisonScope(Set(root, user)))), List(episode))
    assertEquals(captures(protectedAnswer, path), Set(1),
      "Bxa4 supplied the attacked bishop and actually cost the pawn on a4")
    val closed = f.close(protectedAnswer)
    assert(statements(closed).contains(vacancy) && statements(closed).contains(attack))
    assertEquals(f.arms.head.captures.find(_.plyOffset == 1).get.originalActor, Some(bishop))
    assertEquals(f.arms.head.captures.find(_.plyOffset == 1).get.originalVictim,
      Some(piece("white", "pawn", "a4")))
    assertEquals(captures(closed, path), Set(1))
    assert(closed.required.contains(MoveRef(path, 1)))
    Set(5, 6).foreach(offset => assert(!closed.required.contains(MoveRef(path, offset)),
      "Bxf1 and Rxf1 belong to the target's later fight, not the selected Ra2 attack"))
    assert(Set(5, 6).subsetOf(closed.details.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet))
    assert(!statements(closed).contains(background))
    assert(closed.details.flatMap(_.statements).contains(background))
    assert(List(vacancy, attack).forall(closed.completionSources.get.contains))
    assert(!closed.completionSources.get.flatMap(_.covered).contains(MoveRef(path, 1)))
    assertEquals(captures(f.close(closed), path), Set(1))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)
