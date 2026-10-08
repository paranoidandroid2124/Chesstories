package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

/** Closed root-use candidates constructed before changing the legacy protection
  * policy. The recorded rows certify chess facts; no engine is invoked. */
class RootUseClosureTest extends munit.FunSuite:
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
    val owned = statements(answer).flatMap(_.covered)
    assertEquals(owned.distinct.size, owned.size, "an actual capture has one selected statement owner")

  private case class Fixture(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], positions: Map[PositionNodeRef, Position],
      notes: List[MoveWorkProjection.LocalCandidate]):
    def arms(at: Map[PositionNodeRef, Position] = positions) = RootQuestionSummary.prepare(packet, paths, pair, at).get
    def answer(content: List[Statement], scope: Option[ComparisonScope] = None): Answer =
      val recorded = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
      recorded.copy(core = List(Section(Continuation(pair.map(MoveRef(_, 0))) :: content)), comparison = scope)
    def close(answer: Answer): Answer =
      MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, answer)
        .getOrElse(fail("the selected root-use account did not close"))
    def directSupport(provider: MoveWorkProjection.Piece, target: MoveWorkProjection.Piece) =
      val path = pair.head
      LineContinuationInvestigation.directSupportUse(packet, path, paths, path.moves.size, provider, target).uses.head

  private def prepare(fen: String, rows: String*): Fixture =
    val lines = rows.toList.map(row => EngineLine(row.split(" ").toList, 0, None, 16))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen,
      playedMoveUci = lines.head.moves.head, variations = lines)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = lines.map(line => paths.paths.find(_.moves == line.moves).get)
    Fixture(packet, paths, pair, packet.resources.map(view => view.position -> new Position(view)).toMap,
      MoveWorkProjection.select(packet, paths).local)

  private def checkingSupport(kingMove: String = "g1h1") =
    prepare("4r3/8/5n2/8/4P2k/8/8/5RK1 w - - 0 1",
      s"f1e1 f6e4 e1e4 h4g5 $kingMove e8e4", "g1h1 h4g5")

  private def stationaryPawnSupport() =
    prepare("3r1rk1/pbq1ppbp/1p1n2p1/n1p1N3/2P2P2/3PPN2/PBQ1B1PP/R4RK1 w - - 7 16",
      "e3e4 f7f6 e5g4 d6f7 f4f5 g6f5 e4f5 f7h6 g4e3 c7f4 f1f2 f4e3 b2c1 e3f2 g1f2 h6f5 g2g4 f5h6 h2h3 a5c6 c1b2 h6f7 a1d1 e7e5 f3h4 f6f5 h4f5 f7d6 f2g1 d6f5 g4f5 f8f5 e2g4 f5g5",
      "a1d1 d6f5 c2c1 a5c6 a2a3 b7a8 g2g4 c6e5 f3e5 f5h4 e3e4 a8b7 c1c2 f7f6 e5f3 h4f3 f1f3")

  test("a closed support use keeps its own checking reply and loss without selecting a neighbouring quiet route"):
    val f = checkingSupport()
    val path = f.pair.head
    val provider = piece("white", "rook", "f1")
    val target = piece("white", "pawn", "e4")
    val use = f.directSupport(provider, target)
    val receipt = SupportRecapture(MoveRef(path, 0), path, use)
    val loss = f.arms().head.pieceExchange(f.arms().head.captures.filter(_.plyOffset == 5))
    val background = List[Statement](PieceRoute(path, piece("white", "king", "g1"), List(4)),
      Continuation(List(MoveRef(path, 4))))
    val supportNote = f.notes.find(note => note.edge.evidence.id == path.ids.head && (note.note.detail match
      case Detail.Support(p, t, _, _) => p == piece("white", "rook", "e1") && t == target
      case _ => false)).get
    val selectedUse = Section(List(receipt, loss), receipt.references ++ loss.references,
      List(Focus(MoveRef(path, 0), supportNote)))
    val base = f.answer(Nil, Some(ComparisonScope(Set(provider, target))))
    val oldEpisode = RootSubjectComparison.protect(base,
      List(selectedUse.copy(statements = selectedUse.statements ++ background)))
    // This is the candidate the caller can validate before altering protect.
    // Moving only text to detail would leave the route's action selected.
    val narrow = RootSubjectComparison.protect(base, List(selectedUse)).copy(details =
      base.details :+ Section(background, background.flatMap(_.references)))
    val closed = f.close(narrow)
    val oldClosed = f.close(oldEpisode)
    assertEquals(captures(closed, path), Set(1, 2, 5))
    assertEquals(captures(closed, path), captures(oldClosed, path))
    assert(checks(closed).exists { case value: CheckingMove => value.move == MoveRef(path, 2); case _ => false })
    assert(checks(closed).exists { case value: CheckReply => value.move == MoveRef(path, 3); case _ => false })
    assert(closed.required.contains(MoveRef(path, 3)))
    assert(!closed.required.contains(MoveRef(path, 4)), "the quiet king route is not a price/check obligation")
    assert(background.forall(closed.details.flatMap(_.statements).contains))
    assert(closed.core.flatMap(_.focus).contains(Focus(MoveRef(path, 0), supportNote)))
    assert(closed.comparison.get.protectedActions.contains(MoveRef(path, 2)))
    assert(!closed.comparison.get.protectedActions.contains(MoveRef(path, 4)))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("two independent root uses both survive a closed candidate even when the second use is quiet"):
    val f = checkingSupport("g1f1")
    val path = f.pair.head
    val rook = piece("white", "rook", "f1")
    val pawn = piece("white", "pawn", "e4")
    val king = piece("white", "king", "g1")
    val support = SupportRecapture(MoveRef(path, 0), path, f.directSupport(rook, pawn))
    val vacancy = SquareReoccupation(MoveRef(path, 0), rook, piece("white", "rook", "e1"),
      "f1", MoveRef(path, 4), king, piece("white", "king", "f1"))
    val rootUses = List[Statement](support, vacancy)
    val scope = ComparisonScope(Set(rook, pawn, king), rootUses.flatMap(_.actionReferences).toSet)
    val base = f.answer(Nil, Some(scope))
    val selected = RootSubjectComparison.protect(base, List(Section(rootUses, rootUses.flatMap(_.references))))
    val closed = f.close(selected)
    val obligations = statements(closed).flatMap(conditionUses).toSet
    assert(obligations(MoveRef(path, 0) -> MoveRef(path, 2)))
    assert(obligations(MoveRef(path, 0) -> MoveRef(path, 4)))
    assert(rootUses.forall(statements(closed).contains), "one concise use does not replace the other question")
    assertEquals(captures(closed, path), Set(1, 2, 5))
    assert(closed.required.contains(MoveRef(path, 4)))
    assert(closed.comparison.get.protectedActions.contains(MoveRef(path, 4)))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("the used stationary pawn loss is owned without the intervening queen rook and knight trades"):
    // Previously opened Tata 2025 game 10: this is regression evidence, not a new reader assessment.
    val f = stationaryPawnSupport()
    val path = f.pair.head
    val provider = piece("white", "pawn", "e3")
    val target = piece("white", "pawn", "f4")
    val use = f.directSupport(provider, target)
    val receipt = SupportedCapture(MoveRef(path, 0), piece("white", "pawn", "e4"), target,
      use, false, Some(MoveRef(path, 4)), None)
    val laterAttack = f.notes.find(note => note.edge.evidence.id == path.ids(16) && (note.note.detail match
      case Detail.Attack(actor, victim, true) => actor == piece("white", "pawn", "g4") &&
        victim == piece("black", "knight", "f5")
      case _ => false)).get
    val background = Section(List(Action(MoveRef(path, 16), laterAttack.note.detail),
      PieceRoute(path, piece("white", "pawn", "g2"), List(16)),
      Continuation(List(MoveRef(path, 16), MoveRef(path, 17)))),
      List(MoveRef(path, 16), MoveRef(path, 17)), List(Focus(MoveRef(path, 16), laterAttack)))
    val base = f.answer(List(receipt), Some(ComparisonScope(Set(provider, target), receipt.actionReferences.toSet)))
    val selected = base.copy(details = base.details :+ background,
      evidence = (base.evidence ++ laterAttack.evidence).distinct)
    val closed = f.close(selected)
    assertEquals(captures(closed, path), Set(5, 6, 15))
    val loss = statements(closed).collectFirst { case value: PieceExchange if (value.path eq path) &&
      value.events.map(_.plyOffset) == List(15) => value }.get
    assertEquals(loss.events.head.originalVictim, Some(provider))
    Set(11, 13, 14, 16, 17).foreach(offset => assert(!closed.required.contains(MoveRef(path, offset))))
    assert(!statements(closed).contains(Action(MoveRef(path, 16), laterAttack.note.detail)),
      "g4 attacks the later capturer in a new fight; sharing a history is not ownership of that fight")
    assert(closed.details.contains(background), "the later attack retains its actual frame and order in detail")
    assert(closed.details.flatMap(_.covered).contains(MoveRef(path, 11)))
    assert(closed.details.flatMap(_.covered).contains(MoveRef(path, 13)))
    assert(!statements(closed).exists { case value: Material if value.path eq path => true; case _ => false })
    val closedAgain = f.close(closed)
    assertEquals(captures(closedAgain, path), captures(closed, path), "the added loss does not seed another price")
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).get.text.contains("Nxf5"))

  test("an own checking use does not select the preceding unrelated pawn capture as its anchor"):
    val f = prepare("4k3/8/8/8/8/1p6/P7/R1K4R w - - 0 1",
      "a1b1 b3a2 h1e1 e8f7", "h1h2 b3a2")
    val path = f.pair.head
    val checking = f.arms().head.checkEvents(List(2)).collectFirst { case value: CheckingMove => value }.get
    val selected = f.answer(List(checking))
    val closed = f.close(selected)
    assert(checks(closed).contains(checking))
    assert(checks(closed).exists { case value: CheckReply => value.move == MoveRef(path, 3); case _ => false })
    assert(closed.required.contains(MoveRef(path, 3)))
    assert(!closed.required.contains(MoveRef(path, 1)), "the check at2 does not need the pawn capture at1")
    assert(!captures(closed, path)(1))
    assert(closed.details.flatMap(_.covered).contains(MoveRef(path, 1)))
    assertEquals(f.close(closed).required.toSet, closed.required.toSet)
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("a new independent root use extends the original sources without reselecting the completed support price"):
    val f = stationaryPawnSupport()
    val path = f.pair.head
    val pawn = piece("white", "pawn", "e3")
    val target = piece("white", "pawn", "f4")
    val knight = piece("white", "knight", "e5")
    val support = SupportedCapture(MoveRef(path, 0), piece("white", "pawn", "e4"), target,
      f.directSupport(pawn, target), false, Some(MoveRef(path, 4)), None)
    val original = f.close(f.answer(List(support),
      Some(ComparisonScope(Set(pawn, target), support.actionReferences.toSet))))
    assertEquals(captures(original, path), Set(5, 6, 15))
    val sources = original.completionSources.get
    assert(!sources.flatMap(_.covered).contains(MoveRef(path, 15)))
    val vacancy = SquareReoccupation(MoveRef(path, 0), pawn, piece("white", "pawn", "e4"),
      "e3", MoveRef(path, 8), knight, piece("white", "knight", "e3"))
    // Register the new independent selection, rather than taking all of the
    // already completed core as fresh source content.
    val appended = RootSubjectComparison.protect(original, List(Section(List(vacancy))))
      .copy(completionSources = Some((sources :+ vacancy).distinct),
        comparison = original.comparison.map(scope => scope.copy(subjects = scope.subjects + knight,
          protectedActions = scope.protectedActions ++ vacancy.actionReferences)))
    val closed = f.close(appended)
    assert(statements(closed).contains(support))
    assert(statements(closed).contains(vacancy))
    assert(closed.required.contains(MoveRef(path, 8)))
    assert(statements(closed).contains(Action(MoveRef(path, 7),
      Detail.Attack(piece("black", "knight", "h6"), piece("white", "knight", "g4"), true))),
      "the newly selected Ne3 use must inspect the actual attack it evades")
    assert(captures(closed, path)(15), "the support provider's already owned loss remains a price")
    assert(!captures(closed, path)(13) && !captures(closed, path)(14))
    assertEquals(closed.completionSources, Some((sources :+ vacancy).distinct))
    assert(!closed.completionSources.get.flatMap(_.covered).contains(MoveRef(path, 15)))
    assert(closed.comparison.get.protectedActions.contains(MoveRef(path, 6)))
    assert(closed.comparison.get.protectedActions.contains(MoveRef(path, 8)))
    assertEquals(captures(f.close(closed), path), captures(closed, path))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("the C18 other-square reply keeps both actual knight losses without the queen's later new fight"):
    val f = prepare("r6k/3n2pp/p1Np1r1b/1p1Pp3/PQ2BB2/5P2/1PP1q3/1K1R2R1 b - - 0 24",
      "h6f4 b4a5 f4e3 g1e1 e2f2 a5c7 d7c5 e1g1 e3h6 d1f1 f2e2 c6b8 f6f8 c7d6 a8b8 d6c5 b5a4 d5d6 e2b5 c5b5 a6b5 f1d1 b5b4 e4c6",
      "e5f4 g1e1 e2c4 b4a5 b5a4 a5c7 d7e5 e1h1 a8f8 c7e7 e5g6 e4g6 h7g6 c6d8 a4a3 d8e6 f6f7 e7d6 f8c8 d1d2 a3b2")
    val path = f.pair.head
    val source = path.certified.flatMap(_._2.material).get
    val selected = f.answer(List(MaterialEvents(path, source.events.filter(_.plyOffset == 15))))
    val closed = f.close(selected)
    assert(Set(14, 15).subsetOf(captures(closed, path)))
    val events = f.arms().head.captures
    assertEquals(events.find(_.plyOffset == 14).get.originalVictim, Some(piece("white", "knight", "c6")))
    assertEquals(events.find(_.plyOffset == 15).get.originalVictim, Some(piece("black", "knight", "d7")))
    assert(!captures(closed, path)(20), "Qxc5 does not select the queen's later return to b5 and capture there")
    assert(!closed.required.contains(MoveRef(path, 20)))
    assert(closed.details.flatMap(_.covered).contains(MoveRef(path, 20)))
    assertEquals(closed.core.flatMap(_.actionReferences.map(_.path)).toSet, f.pair.toSet,
      "the unplayed legal Qxb8 alternative is proof, not another selected path")
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("promotion identity context closes the sibling root capture before an unchanged second completion"):
    val f = prepare("1Q6/2P1n2k/8/8/1b6/8/P7/6K1 w - - 0 1",
      "c7c8q h7g6 a2a3 b4a3 g1h1 e7c8 b8c8", "b8b4 h7g6")
    val path = f.pair.head
    val source = path.certified.flatMap(_._2.material).get
    val narrow = f.answer(List(MaterialEvents(path, source.events.filter(event => Set(0, 5, 6)(event.plyOffset)))))
    // An already contextualized source does not bypass initial common admission.
    val admitted = RootQuestionSummary.completePairAnswer(f.packet, f.paths, f.pair,
      narrow.question, narrow.core).get.copy(details = narrow.details)
    val closed = f.close(admitted)
    val closedAgain = f.close(closed)
    assertEquals(captures(closed, path), Set(5, 6))
    assert(captures(closed, f.pair.last)(0), "the actual sibling Qxb4 root capture remains owned")
    assert(!captures(closed, path)(3), "identity context does not select the unrelated Bxa3 trade")
    val contexts = statements(closed).collect { case value: MaterialEvents if value.path eq path => value.events }
    assert(contexts.exists(_.map(_.plyOffset) == List(0, 5, 6)))
    assertEquals(contexts.flatten.count(_.plyOffset == 0), 1)
    assertEquals(captures(closedAgain, path), captures(closed, path))
    assertEquals(captures(closedAgain, f.pair.last), captures(closed, f.pair.last))
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("a selected capture price cannot use a detail or interior ending to hide its actual unanswered check"):
    val f = prepare("2r4k/2p5/2Q5/8/8/8/2K5/8 w - - 0 1",
      "c6c7 c8c7", "c2b2 h8g8")
    val path = f.pair.head
    val first = MaterialEvents(path, path.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 0))
    val ending = RecordedEnd(MoveRef(path, 1))
    val selected = f.answer(List(first, ending))
    val closed = f.close(selected)
    assertEquals(captures(closed, path), Set(0, 1))
    assert(statements(closed).contains(ending))
    assert(checks(closed).exists { case value: CheckingMove => value.move == ending.move; case _ => false })
    assert(!checks(closed).exists(_.isInstanceOf[CheckReply]), "the recorded boundary does not invent a response")
    assert(!closed.required.exists(ref => (ref.path eq path) && ref.offset == 2))
    List(f.answer(List(first)), f.answer(List(first, RecordedEnd(MoveRef(path, 0)))),
      f.answer(List(first, RecordedEnd(MoveRef(f.pair.last, 1)))),
      f.answer(List(first)).copy(details = List(Section(List(ending))))).foreach { invalid =>
      assert(MoveWorkProjection.completeSelectedConditions(f.packet, f.paths, f.pair,
        f.positions, f.notes, invalid).isEmpty)
    }
    oneCaptureOwner(closed)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)
