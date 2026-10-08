package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

/** Integration of the real protection seam with final common completion.
  * Saved rows and legal counterexamples supply facts; these tests run no engine.
  */
class ClosedRootProtectionTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import MoveWorkProjection.{Detail, Position}
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef

  private def piece(side: String, role: String, square: String) = CommentaryFact.Piece(side, role, square).witness
  private def statements(answer: Answer) = answer.core.flatMap(_.statements)
  private def captures(answer: Answer, path: PreparedPath) =
    statements(answer).flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet
  private def checks(answer: Answer) = statements(answer).flatMap {
    case value: CheckSequence => value.events
    case value: CheckEvent => List(value)
    case _ => Nil
  }
  private def oneCaptureOwner(answer: Answer): Unit =
    val owned = statements(answer).flatMap(_.covered)
    assertEquals(owned.distinct.size, owned.size, "each selected actual capture has one statement owner")

  private case class Fixture(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], positions: Map[PositionNodeRef, Position],
      notes: List[MoveWorkProjection.LocalCandidate]):
    def arms = RootQuestionSummary.prepare(packet, paths, pair, positions).get
    def material(path: PreparedPath, offsets: Set[Int]) =
      MaterialEvents(path, path.certified.flatMap(_._2.material).get.events.filter(event => offsets(event.plyOffset)))
    def allMaterial(path: PreparedPath) = MaterialEvents(path, path.certified.flatMap(_._2.material).get.events)
    def answer(content: List[Statement], scope: Option[ComparisonScope] = None): Answer =
      val recorded = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
      recorded.copy(core = List(Section(Continuation(pair.map(MoveRef(_, 0))) :: content)), comparison = scope)
    def protect(answer: Answer, bundles: List[Section], at: Map[PositionNodeRef, Position] = positions): Answer =
      MoveWorkProjection.protectRootUses(packet, paths, pair, at, notes, answer, bundles)
    def close(answer: Answer): Answer =
      MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, answer)
        .getOrElse(fail("the protected root-use answer did not pass final common completion"))
    def support(provider: MoveWorkProjection.Piece, target: MoveWorkProjection.Piece,
        path: PreparedPath = pair.head) =
      LineContinuationInvestigation.directSupportUse(packet, path, paths, path.moves.size, provider, target).uses.head

  private def prepare(fen: String, rows: String*): Fixture =
    val lines = rows.toList.map(row => EngineLine(row.split(" ").toList, 0, None, 16))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen,
      playedMoveUci = lines.head.moves.head, variations = lines)).getOrElse(fail("the supplied root position was not admitted"))
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input))
      .getOrElse(fail("the legal rows did not produce an evidence-backed packet"))
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = lines.map(line => paths.paths.find(_.moves == line.moves)
      .getOrElse(fail(s"the whole legal row was not certified: ${line.moves.mkString(" ")}")))
    Fixture(packet, paths, pair, packet.resources.map(view => view.position -> new Position(view)).toMap,
      MoveWorkProjection.select(packet, paths).local)

  private def checkingSupport(kingMove: String = "g1h1") =
    prepare("4r3/8/5n2/8/4P2k/8/8/5RK1 w - - 0 1",
      s"f1e1 f6e4 e1e4 h4g5 $kingMove e8e4", "g1h1 h4g5")

  private def stationaryPawnSupport() =
    prepare("3r1rk1/pbq1ppbp/1p1n2p1/n1p1N3/2P2P2/3PPN2/PBQ1B1PP/R4RK1 w - - 7 16",
      "e3e4 f7f6 e5g4 d6f7 f4f5 g6f5 e4f5 f7h6 g4e3 c7f4 f1f2 f4e3 b2c1 e3f2 g1f2 h6f5 g2g4 f5h6 h2h3 a5c6 c1b2 h6f7 a1d1 e7e5 f3h4 f6f5 h4f5 f7d6 f2g1 d6f5 g4f5 f8f5 e2g4 f5g5",
      "a1d1 d6f5 c2c1 a5c6 a2a3 b7a8 g2g4 c6e5 f3e5 f5h4 e3e4 a8b7 c1c2 f7f6 e5f3 h4f3 f1f3")

  private def pawnReceipt(f: Fixture) =
    val pawn = piece("white", "pawn", "e3")
    val target = piece("white", "pawn", "f4")
    SupportedCapture(MoveRef(f.pair.head, 0), piece("white", "pawn", "e4"), target,
      f.support(pawn, target), false, Some(MoveRef(f.pair.head, 4)), None)

  private def knightVacancy(f: Fixture) =
    SquareReoccupation(MoveRef(f.pair.head, 0), piece("white", "pawn", "e3"), piece("white", "pawn", "e4"),
      "e3", MoveRef(f.pair.head, 8), piece("white", "knight", "e5"), piece("white", "knight", "e3"))

  private def pawnBackground(f: Fixture) =
    val path = f.pair.head
    val attack = f.notes.find(note => note.edge.evidence.id == path.ids(16) && (note.note.detail match
      case Detail.Attack(actor, victim, true) => actor == piece("white", "pawn", "g4") &&
        victim == piece("black", "knight", "f5")
      case _ => false)).get
    Section(List(Action(MoveRef(path, 16), attack.note.detail),
      PieceRoute(path, piece("white", "pawn", "g2"), List(16)),
      Continuation(List(MoveRef(path, 16), MoveRef(path, 17)))),
      List(MoveRef(path, 16), MoveRef(path, 17)), List(Focus(MoveRef(path, 16), attack)))

  test("the actual protection seam narrows a support episode while keeping its own check reply and price"):
    val f = checkingSupport()
    val path = f.pair.head
    val rook = piece("white", "rook", "f1")
    val pawn = piece("white", "pawn", "e4")
    val support = SupportRecapture(MoveRef(path, 0), path, f.support(rook, pawn))
    val background = List[Statement](PieceRoute(path, piece("white", "king", "g1"), List(4)),
      Continuation(List(MoveRef(path, 4))))
    val supportNote = f.notes.find(note => note.edge.evidence.id == path.ids.head && (note.note.detail match
      case Detail.Support(p, t, _, _) => p == piece("white", "rook", "e1") && t == pawn
      case _ => false)).get
    val episode = Section(support :: f.allMaterial(path) :: background,
      List(MoveRef(path, 0), MoveRef(path, 2), MoveRef(path, 3), MoveRef(path, 4), MoveRef(path, 5)),
      List(Focus(MoveRef(path, 0), supportNote)))
    val protectedAnswer = f.protect(f.answer(Nil, Some(ComparisonScope(Set(rook, pawn)))), List(episode))
    assert(background.forall(statement => !statements(protectedAnswer).contains(statement)))
    assert(background.forall(protectedAnswer.details.flatMap(_.statements).contains))
    assertEquals(captures(protectedAnswer, path), Set(1, 2, 5))
    assert(checks(protectedAnswer).exists { case value: CheckingMove => value.move == MoveRef(path, 2); case _ => false })
    assert(checks(protectedAnswer).exists { case value: CheckReply => value.move == MoveRef(path, 3); case _ => false })
    val closed = f.close(protectedAnswer)
    assertEquals(captures(closed, path), Set(1, 2, 5))
    assert(closed.required.contains(MoveRef(path, 3)))
    assert(!closed.required.contains(MoveRef(path, 4)))
    assert(closed.core.flatMap(_.focus).contains(Focus(MoveRef(path, 0), supportNote)))
    assert(closed.comparison.get.protectedActions.contains(MoveRef(path, 2)))
    assert(!closed.comparison.get.protectedActions.contains(MoveRef(path, 4)))
    assert(!closed.completionSources.get.flatMap(_.covered).contains(MoveRef(path, 5)))
    assertEquals(f.close(closed).required.toSet, closed.required.toSet)
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("closing one episode retains both independent root uses and the previously protected actions"):
    val f = checkingSupport("g1f1")
    val path = f.pair.head
    val rook = piece("white", "rook", "f1")
    val pawn = piece("white", "pawn", "e4")
    val king = piece("white", "king", "g1")
    val support = SupportRecapture(MoveRef(path, 0), path, f.support(rook, pawn))
    val vacancy = SquareReoccupation(MoveRef(path, 0), rook, piece("white", "rook", "e1"),
      "f1", MoveRef(path, 4), king, piece("white", "king", "f1"))
    val unrelated = PiecePresence(MoveRef(path, 5), piece("black", "rook", "e8"), piece("black", "rook", "e4"))
    val rootUses = List[Statement](support, vacancy)
    val base = f.answer(Nil, Some(ComparisonScope(Set(rook, pawn, king), Set(MoveRef(f.pair.last, 0)))))
    val protectedAnswer = f.protect(base, List(Section(rootUses ++ List(f.allMaterial(path), unrelated))))
    val closed = f.close(protectedAnswer)
    assert(rootUses.forall(statements(closed).contains))
    assertEquals(statements(closed).flatMap(conditionUses).filter(_._1.offset == 0).toSet,
      Set(MoveRef(path, 0) -> MoveRef(path, 2), MoveRef(path, 0) -> MoveRef(path, 4)))
    assertEquals(captures(closed, path), Set(1, 2, 5))
    assert(closed.comparison.get.protectedActions.contains(MoveRef(f.pair.last, 0)))
    assert(closed.comparison.get.protectedActions.contains(MoveRef(path, 4)))
    assert(!statements(closed).contains(unrelated))
    assert(closed.details.flatMap(_.statements).contains(unrelated))
    assert(rootUses.forall(closed.completionSources.get.contains))
    assertEquals(captures(f.close(closed), path), captures(closed, path))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("support only owns the stationary provider loss without reselecting the intervening queen rook fight"):
    val f = stationaryPawnSupport()
    val path = f.pair.head
    val receipt = pawnReceipt(f)
    val background = pawnBackground(f)
    val episode = Section(receipt :: f.allMaterial(path) :: background.statements,
      background.required, background.focus)
    val base = f.answer(Nil, Some(ComparisonScope(Set(piece("white", "pawn", "e3"), piece("white", "pawn", "f4")))))
    val protectedAnswer = f.protect(base, List(episode))
    val closed = f.close(protectedAnswer)
    assertEquals(captures(closed, path), Set(5, 6, 15))
    Set(11, 13, 14, 16, 17).foreach(offset => assert(!closed.required.contains(MoveRef(path, offset))))
    assert(background.statements.forall(statement => !statements(closed).contains(statement)))
    assert(background.statements.forall(closed.details.flatMap(_.statements).contains))
    assert(closed.details.flatMap(_.focus).contains(background.focus.head))
    assert(closed.details.flatMap(_.covered).contains(MoveRef(path, 11)))
    assert(!closed.completionSources.get.flatMap(_.covered).contains(MoveRef(path, 15)))
    assertEquals(captures(f.close(closed), path), Set(5, 6, 15))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("selecting Ne3 independently owns its actual loss and the connected queen rook checking exchange"):
    val f = stationaryPawnSupport()
    val path = f.pair.head
    val support = pawnReceipt(f)
    val vacancy = knightVacancy(f)
    val background = pawnBackground(f)
    val episode = Section(List(support, vacancy, f.allMaterial(path)) ++ background.statements,
      background.required, background.focus)
    val base = f.answer(Nil, Some(ComparisonScope(Set(piece("white", "pawn", "e3"),
      piece("white", "pawn", "f4"), piece("white", "knight", "e5")))))
    val protectedAnswer = f.protect(base, List(episode))
    assertEquals(captures(protectedAnswer, path), Set(5, 6, 11, 13, 14, 15))
    assert(checks(protectedAnswer).exists { case value: CheckingMove => value.move == MoveRef(path, 13); case _ => false })
    assert(checks(protectedAnswer).exists { case value: CheckReply => value.move == MoveRef(path, 14); case _ => false })
    val closed = f.close(protectedAnswer)
    assertEquals(captures(closed, path), Set(5, 6, 11, 13, 14, 15))
    assert(List(support, vacancy).forall(statements(closed).contains))
    val arm = f.arms.head
    assertEquals(arm.captures.find(_.plyOffset == 11).get.originalVictim, Some(piece("white", "knight", "e5")))
    assertEquals(arm.captures.find(_.plyOffset == 13).get.originalVictim, Some(piece("white", "rook", "f1")))
    assertEquals(arm.captures.find(_.plyOffset == 14).get.originalVictim, Some(piece("black", "queen", "c7")))
    Set(8, 11, 13, 14, 15).foreach(offset => assert(closed.required.contains(MoveRef(path, offset))))
    assert(!closed.required.contains(MoveRef(path, 16)))
    assert(background.statements.forall(closed.details.flatMap(_.statements).contains))
    Set(11, 13, 14, 15).foreach(offset =>
      assert(!closed.completionSources.get.flatMap(_.covered).contains(MoveRef(path, offset))))
    assertEquals(captures(f.close(closed), path), captures(closed, path))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("missing quiet identity evidence falls back for that episode while the independent other arm still closes"):
    val f = prepare("r5k1/8/8/7n/5P2/8/P6R/6K1 w - - 0 1",
      "a2a3 a8a7 h2a2 a7a3 g1h1 a3a2", "h2f2 h5f4 f2f4 a8f8 g1h1 f8f4")
    val actual = f.pair.head
    val other = f.pair.last
    val rook = piece("white", "rook", "h2")
    val pawn = piece("white", "pawn", "a2")
    val target = piece("white", "pawn", "f4")
    val vacancy = SquareReoccupation(MoveRef(actual, 0), pawn, piece("white", "pawn", "a3"),
      "a2", MoveRef(actual, 2), rook, piece("white", "rook", "a2"))
    val support = SupportRecapture(MoveRef(other, 0), other, f.support(rook, target, other))
    val actualBackground = PieceRoute(actual, piece("white", "king", "g1"), List(4))
    val otherBackground = PieceRoute(other, piece("white", "king", "g1"), List(4))
    val actualEpisode = Section(List(vacancy, f.allMaterial(actual), actualBackground))
    val otherEpisode = Section(List(support, f.allMaterial(other), otherBackground))
    val base = f.answer(Nil, Some(ComparisonScope(Set(rook, pawn, target))))
    val protectedAnswer = f.protect(base, List(actualEpisode, otherEpisode), f.positions - actual.positions(4))
    assert(statements(protectedAnswer).contains(actualBackground), "the unverified quiet account retains its old episode")
    assert(!statements(protectedAnswer).contains(otherBackground), "the complete independent account can be narrowed")
    assert(protectedAnswer.details.flatMap(_.statements).contains(otherBackground))
    assert(List(vacancy, support).forall(statements(protectedAnswer).contains))
    assertEquals(captures(protectedAnswer, actual), Set(3, 5))
    assertEquals(captures(protectedAnswer, other), Set(1, 2, 5))
    val closed = f.close(protectedAnswer)
    assert(Set(3, 5).subsetOf(captures(closed, actual)))
    assertEquals(captures(closed, other), Set(1, 2, 5))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("a capturing promotion root keeps its identity context and the quiet user's actual loss without duplicate root price"):
    val f = prepare("k4r1r/6P1/8/8/8/8/8/K5R1 w - - 0 1",
      "g7h8q f8h8 g1g7 h8h7 a1b1 h7g7", "a1b1 a8b7")
    val path = f.pair.head
    val pawn = piece("white", "pawn", "g7")
    val rook = piece("white", "rook", "g1")
    val vacancy = SquareReoccupation(MoveRef(path, 0), pawn, piece("white", "queen", "h8"),
      "g7", MoveRef(path, 2), rook, piece("white", "rook", "g7"))
    val background = PieceRoute(path, piece("white", "king", "a1"), List(4))
    val protectedAnswer = f.protect(f.answer(Nil, Some(ComparisonScope(Set(pawn, rook)))),
      List(Section(List(vacancy, f.allMaterial(path), background))))
    val closed = f.close(protectedAnswer)
    assertEquals(captures(closed, path), Set(0, 1, 5))
    assertEquals(f.arms.head.captures.find(_.plyOffset == 0).get.originalVictim, Some(piece("black", "rook", "h8")))
    assertEquals(f.arms.head.captures.find(_.plyOffset == 1).get.originalVictim, Some(pawn))
    assertEquals(f.arms.head.captures.find(_.plyOffset == 5).get.originalVictim, Some(rook))
    val context = statements(closed).collect { case value: MaterialEvents if value.path eq path => value.events }.flatten
    assertEquals(context.count(event => event.plyOffset == 0 && event.promotionGainCp > 0), 1)
    assert(!statements(closed).contains(background))
    assert(closed.details.flatMap(_.statements).contains(background))
    assert(!closed.required.contains(MoveRef(path, 4)))
    assert(!closed.completionSources.get.flatMap(_.covered).contains(MoveRef(path, 5)))
    assertEquals(captures(f.close(closed), path), captures(closed, path))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("a quiet user that leaves and returns keeps the old price episode instead of a fabricated stationary account"):
    val f = prepare("1r4k1/8/8/8/8/8/P6R/6K1 w - - 0 1",
      "a2a3 b8b7 h2a2 b7a7 a2b2 a7a6 b2a2 a6a3 g1h1 a3a2", "g1h1 g8f8")
    val path = f.pair.head
    val pawn = piece("white", "pawn", "a2")
    val rook = piece("white", "rook", "h2")
    val vacancy = SquareReoccupation(MoveRef(path, 0), pawn, piece("white", "pawn", "a3"),
      "a2", MoveRef(path, 2), rook, piece("white", "rook", "a2"))
    val route = PieceRoute(path, rook, List(2, 4, 6))
    val episode = Section(List(vacancy, f.material(path, Set(7, 9)), route), List(MoveRef(path, 9)))
    val protectedAnswer = f.protect(f.answer(Nil, Some(ComparisonScope(Set(pawn, rook)))), List(episode))
    assert(episode.statements.forall(statements(protectedAnswer).contains))
    assertEquals(protectedAnswer.completionSources, None, "a fallback is not an independently closed narrower account")
    val closed = f.close(protectedAnswer)
    assert(Set(7, 9).subsetOf(captures(closed, path)))
    assert(statements(closed).contains(vacancy) && statements(closed).contains(route))
    assert(closed.required.contains(MoveRef(path, 9)))
    assertEquals(f.arms.head.captures.find(_.plyOffset == 9).get.originalVictim, Some(rook))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("the saved Nc4 support price preserves the actual rook exchange and its exact selected recorded check ending"):
    // The v5 failure class: a comparison capture account cannot shrink the
    // already verified price of the original support user into its first loss.
    val f = prepare("b5r1/4rk2/1nppNp2/1p3Pp1/1P2P1P1/2N2K1R/2P5/3R4 b - - 1 32",
      "b6c4 f3g3 f7e8 e4e5 f6e5 c3e4 e7f7 h3h6 c6c5 e4d6 c4d6 d1d6 f7d7 d6d7 e8d7 h6h7",
      "e7d7 f3g3")
    val path = f.pair.head
    val knight = piece("black", "knight", "b6")
    val pawn = piece("black", "pawn", "d6")
    val support = SupportRecapture(MoveRef(path, 0), path, f.support(knight, pawn))
    val ending = RecordedEnd(MoveRef(path, 15))
    val background = PieceRoute(path, piece("white", "pawn", "e4"), List(3))
    val base = f.answer(Nil, Some(ComparisonScope(Set(knight, pawn))))
    val protectedAnswer = f.protect(base, List(Section(List(support, f.allMaterial(path), ending, background))))
    assertEquals(captures(protectedAnswer, path), Set(9, 10, 11, 13, 14),
      "the root support user's verified loss account includes the actual rook exchange")
    val closed = f.close(protectedAnswer)
    assertEquals(captures(closed, path), Set(9, 10, 11, 13, 14))
    assert(statements(closed).contains(support) && statements(closed).contains(ending))
    assert(checks(closed).exists { case value: CheckingMove => value.move == ending.move; case _ => false })
    assert(!checks(closed).exists { case value: CheckReply => value.move == MoveRef(path, 16); case _ => false })
    assert(closed.required.contains(ending.move))
    assert(!closed.required.contains(MoveRef(path, 3)) && !closed.required.contains(MoveRef(path, 4)))
    assert(!statements(closed).contains(background))
    assert(closed.details.flatMap(_.statements).contains(background))
    assert(!closed.completionSources.get.flatMap(_.covered).contains(MoveRef(path, 11)))
    assertEquals(captures(f.close(closed), path), captures(closed, path))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("replacing a preclosed comparison with immediate defence registers the new sources and removes discarded generic ones"):
    val f = prepare("7k/8/5n2/8/4P3/8/8/1N3RK1 w - - 0 1",
      "f1e1 h8h7 b1c3 f6e4 e1e4", "g1h1 f6e4 f1f7 h8g8")
    val path = f.pair.head
    val rook = piece("white", "rook", "f1")
    val pawn = piece("white", "pawn", "e4")
    val support = SupportRecapture(MoveRef(path, 0), path, f.support(rook, pawn))
    val generic = PieceRoute(path, piece("white", "knight", "b1"), List(2))
    val base = f.answer(List(generic, f.material(f.pair.last, Set(1))), Some(ComparisonScope(Set(rook, pawn))))
    val protectedAnswer = f.close(f.protect(base, List(Section(List(support, f.allMaterial(path))))))
    assert(protectedAnswer.completionSources.get.contains(generic))
    val selected = RootSubjectComparison.withImmediateDefense(f.packet, f.paths, f.pair, f.positions, protectedAnswer)
    assertEquals(selected.question, "current_defense")
    assert(!statements(selected).contains(generic))
    assert(!selected.completionSources.get.contains(generic))
    val defences = statements(selected).collect { case value: TargetDefense => value }
    assertEquals(defences.size, 2)
    assert(defences.forall(selected.completionSources.get.contains))
    assert(selected.completionSources.get.contains(support), "the independent root use remains an original selection")
    assert(selected.details.flatMap(_.statements).contains(generic))
    val closed = f.close(selected)
    assert(statements(closed).contains(support))
    assert(protectedAnswer.core.flatMap(_.covered).forall(closed.core.flatMap(_.covered).contains))
    assert(!closed.required.contains(MoveRef(path, 2)), "a removed generic route no longer opens its old use dependencies")
    assertEquals(f.close(closed).completionSources, closed.completionSources)
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)
