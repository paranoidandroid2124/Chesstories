package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

/** Small legal boards exercise selected-use dependencies, without engine answers or fixtures. */
class CommentaryDependencyTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import MoveWorkProjection.{Detail, Position}
  import RootQuestionSummary.MoveRef

  private def piece(side: String, role: String, square: String) = CommentaryFact.Piece(side, role, square).witness

  private case class Prepared(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths,
      pair: List[ObservedLineProtocol.PreparedPath], positions: Map[PositionNodeRef, Position],
      notes: List[MoveWorkProjection.LocalCandidate]):
    def answer(statements: List[Statement]): Answer = Answer("selected_comparison",
      List(Section(Continuation(pair.map(MoveRef(_, 0))) :: statements)),
      List(Section(List(Continuation(pair.head.moves.indices.map(MoveRef(pair.head, _)).toList),
        MaterialEvents(pair.head, pair.head.certified.flatMap(_._2.material).get.events)))), List(pair.head.ids.head))
    def arms(at: Map[PositionNodeRef, Position] = positions) = pair.map(path =>
      new RootQuestionSummary.Arm(packet, paths, path, at, paths.continuationReadings.find(_.path eq path).get))
    def capture(offset: Int): PieceExchange =
      val path = pair.head
      val event = paths.continuationReadings.find(_.path eq path).get.events.find(_.plyOffset == offset).get
      val recaptures = path.certified.flatMap(_._2.material).get.captures.filter(_.recapture).map(_.plyOffset).toSet
      PieceExchange(path, List(event), recaptures)
    def close(selected: Answer): Answer =
      val result = MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, selected).get
      result.evidence.foreach(id => assert(packet.evidenceGraph.byId.get(id).exists(packet.evidenceGraph.proofEligible), id))
      result

  private def prepare(fen: String, lines: String*): Prepared =
    val rows = lines.toList.map(_.split(" ").toList)
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen,
      playedMoveUci = rows.head.head, variations = rows.map(EngineLine(_, 0, None, 8)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = rows.take(2).map(row => paths.paths.find(_.moves == row).get)
    Prepared(packet, paths, pair, packet.resources.map(view => view.position -> new Position(view)).toMap,
      MoveWorkProjection.select(packet, paths).local)

  private def statements(answer: Answer) = answer.core.flatMap(_.statements)

  test("a selected later move out of check retains its exact checking predecessor"):
    val f = prepare("4r2k/8/8/8/8/8/8/5K2 w - - 0 1",
      "f1g1 e8e1 g1h2", "f1f2 h8h7")
    val path = f.pair.head
    val selected = f.answer(List(PieceRoute(path, piece("white", "king", "f1"), List(2))))
    val result = f.close(selected)
    val checking = statements(result).collect { case value: CheckSequence => value.events }.flatten
    assert(checking.exists { case value: CheckingMove => value.move == MoveRef(path, 1); case _ => false })
    assert(checking.exists { case value: CheckReply => value.move == MoveRef(path, 2); case _ => false })
    assert(RootQuestionSummary.realize(f.paths, f.pair, result).nonEmpty)
  private def words(answer: Answer, paths: ObservedLineProtocol.PreparedPaths) = answer.core.map(section =>
    CommentaryEnglish.section(section).map {
      case text: String => text
      case move: MoveRef => paths.notation.line(move.path.moves)(move.offset)
    }.mkString).mkString(" ")

  test("a required preceding capture is explained before its selected recapture in the same section"):
    val f = prepare("7k/8/8/8/4P3/3R4/2b5/6K1 w - - 0 1",
      "d3e3 c2e4 e3e4", "g1h1")
    val path = f.pair.head
    val material = path.certified.flatMap(_._2.material).get
    val recapture = material.events.find(_.plyOffset == 2).get
    assert(recapture.capture.get.recapture)
    val selected = f.answer(List(MaterialEvents(path, List(recapture))))
    val closed = RootQuestionSummary.completePairAnswer(f.packet, f.paths, f.pair,
      selected.question, selected.core).get
    val events = closed.core.head.statements.collect { case value: MaterialEvents => value.events }.flatten
    assertEquals(events.map(_.plyOffset), List(1, 2))
    assertEquals(events.map(_.capture.get.capturedRole.name), List("pawn", "bishop"))
    assertEquals(events.map(_.capture.get.side.white), List(false, true))
    val parts = CommentaryEnglish.section(closed.core.head).collect { case move: MoveRef => move }
    assert(parts.indexOf(MoveRef(path, 1)) < parts.indexOf(MoveRef(path, 2)))
    assert(!closed.core.drop(1).flatMap(_.covered).contains(MoveRef(path, 1)))
    assertEquals(closed.core.flatMap(_.covered).toSet, Set(MoveRef(path, 1), MoveRef(path, 2)))
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

    val longer = prepare("4r2k/8/8/8/4P3/3R4/2b5/6K1 w - - 0 1",
      "d3e3 c2e4 e3e4 e8e4", "g1h1")
    val longerPath = longer.pair.head
    val last = longerPath.certified.flatMap(_._2.material).get.events.last
    assertEquals(last.plyOffset, 3)
    val lastOnly = longer.answer(List(MaterialEvents(longerPath, List(last))))
    val longerClosed = RootQuestionSummary.completePairAnswer(longer.packet, longer.paths, longer.pair,
      lastOnly.question, lastOnly.core).get
    val chain = longerClosed.core.head.statements.collect { case value: MaterialEvents => value.events }.flatten
    assertEquals(chain.map(_.plyOffset), List(1, 2, 3), "the initial capture stays before both recaptures")
    assertEquals(chain.map(_.capture.get.capturedRole.name), List("pawn", "bishop", "rook"))
    assert(!longerClosed.core.drop(1).flatMap(_.covered).exists(_.path eq longerPath))
    assert(RootQuestionSummary.realize(longer.paths, longer.pair, longerClosed).nonEmpty)

  test("a selected rook capture retains the opponent's capturing clearance and its actual exchange/check reply"):
    val f = prepare("3q3k/Q7/2np4/2p5/8/8/8/3R3K w - - 0 1",
      "a7c5 d6c5 d1d8 h8h7", "a7a6 d6d5")
    val path = f.pair.head
    val selected = f.answer(List(f.capture(2)))
    val result = f.close(selected)
    val opening = statements(result).collect { case value: ClearedRoute => value }.head
    assertEquals(opening.move, MoveRef(path, 1))
    assertEquals(opening.use, MoveRef(path, 2))
    assertEquals(opening.before, piece("black", "pawn", "d6"))
    assertEquals(opening.originalUser, piece("white", "rook", "d1"))
    assertEquals(opening.targets, List(piece("black", "queen", "d8")))
    assert(!opening.before.equals(piece("black", "knight", "c6")), "an off-ray knight is not the blocker")
    assert((0 to 3).forall(offset => result.required.contains(MoveRef(path, offset))))
    assert((0 to 2).forall(offset => result.core.flatMap(_.covered).contains(MoveRef(path, offset))))
    assertEquals(result.question, selected.question)
    assert(selected.details.forall(result.details.contains))
    assert(result.evidence.contains(path.ids.head))
    val text = words(result, f.paths)
    List("dxc5", "Rxd8+", "d6", "passes through", "Kh7").foreach(value => assert(text.contains(value), text))

  test("same-side capturing clearance is scoped to the exact selected path, not matching SAN in its sibling"):
    val f = prepare("7k/8/8/8/2pp4/3P4/8/3R3K w - - 0 1",
      "h1g1 h8g8 d3c4 g8h8 d1d4", "h1h2 h8g8 d3c4 g8h8 d1d4")
    val path = f.pair.head
    val selected = f.answer(List(f.capture(4)))
    val own = MoveWorkProjection.selectedConditionSections(f.arms(), selected, f.notes).map(_._2)
    val opening = own.flatMap(_.statements).collect { case value: ClearedRoute => value }.head
    assertEquals(opening.move, MoveRef(path, 2))
    assertEquals(opening.use, MoveRef(path, 4))
    assertEquals(opening.before, piece("white", "pawn", "d3"))
    assertEquals(opening.mover, piece("white", "pawn", "c4"))
    assert(path.ids(2) != f.pair.last.ids(2))
    val siblingNotes = f.notes.filter(note => f.pair.last.ids.contains(note.edge.evidence.id))
    val foreign = MoveWorkProjection.selectedConditionSections(f.arms(), selected, siblingNotes).map(_._2)
    assert(!foreign.flatMap(_.statements).exists(_.isInstanceOf[ClearedRoute]))
    val result = f.close(selected)
    assert(result.core.flatMap(_.covered).contains(MoveRef(path, 2)), "the prerequisite's actual capture is retained")
    assert(!result.required.contains(MoveRef(path, 3)), "an unrelated intervening king move remains detail")
    val unknown = f.positions - path.positions(3)
    assert(!MoveWorkProjection.selectedConditionSections(f.arms(unknown), selected, f.notes).map(_._2)
      .flatMap(_.statements).exists(_.isInstanceOf[ClearedRoute]))

  test("a selected square use carries the same rook's incoming attack and quiet escape"):
    val f = prepare("7k/4b3/8/6P1/8/2N5/8/2R4K w - - 0 1",
      "c3a4 e7g5 c1c3", "c3d5 h8g8")
    val path = f.pair.head
    val square = SquareReoccupation(MoveRef(path, 0), piece("white", "knight", "c3"),
      piece("white", "knight", "a4"), "c3", MoveRef(path, 2),
      piece("white", "rook", "c1"), piece("white", "rook", "c3"))
    val incoming = Action(MoveRef(path, 1), Detail.Attack(
      piece("black", "bishop", "g5"), piece("white", "rook", "c1"), true))
    val selected = f.answer(List(square, incoming))
    val additions = MoveWorkProjection.selectedConditionSections(f.arms(), selected, f.notes).map(_._2)
    assert(additions.flatMap(_.focus).nonEmpty)
    additions.foreach(section => assert(section.focus.forall(focus => section.references.contains(focus.move)),
      "a deduplicated attack cannot donate stale focus to the new escape-only section"))
    val result = f.close(selected)
    val actions = statements(result).collect { case action: Action => action }
    assert(actions.contains(Action(MoveRef(path, 1), Detail.Attack(
      piece("black", "bishop", "g5"), piece("white", "rook", "c1"), true))))
    assert(actions.contains(Action(MoveRef(path, 2), Detail.Evades(
      piece("white", "rook", "c3"), piece("black", "bishop", "g5")))))
    assertEquals(statements(result).count(_.isInstanceOf[SquareReoccupation]), 1)
    assertEquals(statements(result).count(_ == incoming), 1)
    assert(result.core.flatMap(_.covered).contains(MoveRef(path, 1)))
    assert(RootQuestionSummary.realize(f.paths, f.pair, result).nonEmpty)
    val text = words(result, f.paths)
    List("Rc3", "Bxg5", "out of the attack", "bishop on g5").foreach(value => assert(text.contains(value), text))

  test("a later reoccupation cannot borrow an earlier vacancy from the same original piece"):
    val f = prepare("7k/4b3/8/6P1/8/2N5/8/2R4K w - - 0 1",
      "c3a4 h8g8 c1c3 g8h8 c3b3 h8g8 a4c3", "c3d5 h8g8")
    val path = f.pair.head
    val selected = f.answer(List(PieceRoute(path, piece("white", "knight", "c3"), List(6))))
    val result = MoveWorkProjection.selectedConditionSections(f.arms(), selected, f.notes).map(_._2)
    val vacancies = result.flatMap(_.statements).collect { case value: SquareReoccupation => value }
    assert(vacancies.exists(value => value.move == MoveRef(path, 4) && value.use == MoveRef(path, 6) &&
      value.originalActor == piece("white", "rook", "c1") && value.originalEntry == piece("white", "knight", "c3")))
    assert(!vacancies.exists(_.move == MoveRef(path, 0)))
    val presence = f.answer(List(PiecePresence(MoveRef(path, 6), piece("white", "knight", "c3"),
      piece("white", "knight", "c3"))))
    assertEquals(MoveWorkProjection.selectedConditionSections(f.arms(), presence, f.notes), Nil)

  test("an exact selected target response owns its attack and escape while retaining proof, focus and capture coverage"):
    val f = prepare("7k/4b3/8/6P1/8/2N5/8/2R4K w - - 0 1",
      "c3a4 e7g5 c1c3", "c3d5 h8g8")
    val path = f.pair.head
    val target = piece("white", "rook", "c1")
    val attacker = piece("black", "bishop", "e7")
    val response = TargetResponse(path, 1, target, attacker, target, Some(2), None, survives = false)
    val route = PieceRoute(path, target, List(2))
    val original = f.answer(List(response, route, f.capture(1)))
    val attack = Action(MoveRef(path, 1), Detail.Attack(piece("black", "bishop", "g5"), target, true))
    val escape = Action(MoveRef(path, 2), Detail.Evades(piece("white", "rook", "c3"),
      piece("black", "bishop", "g5")))
    val attackNote = f.notes.find(note => note.edge.evidence.id == path.ids(1) &&
      note.note.detail == attack.detail).get
    val existingFocus = Focus(MoveRef(path, 1), attackNote)
    val selected = original.copy(core = List(original.core.head.copy(
      required = List(MoveRef(path, 1), MoveRef(path, 2)), focus = List(existingFocus))))
    val additions = MoveWorkProjection.selectedConditionSections(f.arms(), selected, f.notes).map(_._2)
    assert(!additions.flatMap(_.statements).exists(statement => statement == attack || statement == escape))
    assert(additions.exists(section => section.statements.isEmpty && section.required.contains(MoveRef(path, 2))))
    val result = f.close(selected)
    assert(statements(result).contains(response))
    assert(!statements(result).contains(route))
    assert(!statements(result).exists(statement => statement == attack || statement == escape))
    assert(selected.required.forall(result.required.contains))
    assert(selected.core.flatMap(_.covered).forall(result.core.flatMap(_.covered).contains))
    assert(selected.evidence.forall(result.evidence.contains))
    assertEquals(result.details, selected.details)
    assert(result.core.flatMap(_.focus).contains(existingFocus))
    assert(result.core.flatMap(_.focus).exists(_.move == MoveRef(path, 2)),
      "the retained response still receives the escape's focus metadata")
    assert(RootQuestionSummary.realize(f.paths, f.pair, result).nonEmpty)

    // A route in a different section is retained. Metadata belongs to the
    // response section, rather than to the first section mentioning the escape.
    val split = selected.copy(core = List(Section(List(route)),
      selected.core.head.copy(statements = selected.core.head.statements.filterNot(_ == route))))
    val splitResult = f.close(split)
    assert(statements(splitResult).contains(route))
    val responseSection = splitResult.core.find(_.statements.contains(response)).get
    assert(responseSection.focus.exists(_.move == MoveRef(path, 2)))
    assert(!splitResult.core.head.focus.exists(_.move == MoveRef(path, 1)))
    assert(RootQuestionSummary.realize(f.paths, f.pair, splitResult).nonEmpty)

  test("a response cannot replace another attacker, a sibling occurrence, or a quiet route-only condition"):
    val f = prepare("7k/4b3/8/6P1/8/2N5/8/2R4K w - - 0 1",
      "c3a4 e7g5 c1c3", "c3b5 e7g5 c1c3")
    val path = f.pair.head
    val sibling = f.pair.last
    val target = piece("white", "rook", "c1")
    val response = TargetResponse(path, 1, target, piece("black", "bishop", "e7"), target,
      Some(2), None, survives = false)
    def actions(selected: Answer, prepared: Prepared) =
      MoveWorkProjection.selectedConditionSections(prepared.arms(), selected, prepared.notes)
        .flatMap(_._2.statements).collect { case action: Action => action }
    val siblingRoute = PieceRoute(sibling, target, List(2))
    val siblingAnswer = f.answer(List(response, siblingRoute))
    val siblingActions = actions(siblingAnswer, f)
    assert(siblingActions.contains(Action(MoveRef(sibling, 1), Detail.Attack(
      piece("black", "bishop", "g5"), target, true))))
    assert(siblingActions.contains(Action(MoveRef(sibling, 2), Detail.Evades(
      piece("white", "rook", "c3"), piece("black", "bishop", "g5")))))
    assert(path.ids(2) != sibling.ids(2))
    val routeOnly = f.answer(List(PieceRoute(path, target, List(2))))
    val routeActions = actions(routeOnly, f)
    assert(routeActions.contains(Action(MoveRef(path, 1), Detail.Attack(
      piece("black", "bishop", "g5"), target, true))))
    assert(routeActions.contains(Action(MoveRef(path, 2), Detail.Evades(
      piece("white", "rook", "c3"), piece("black", "bishop", "g5")))))

    val opposed = prepare("7k/4b3/8/6P1/8/2N5/7K/2R3r1 w - - 0 1",
      "c3a4 e7g5 c1c3", "c3b5 e7g5 c1c3")
    val opposedPath = opposed.pair.head
    val opposedResponse = response.copy(path = opposedPath)
    val opposedActions = actions(opposed.answer(List(opposedResponse)), opposed)
    assert(opposedActions.contains(Action(MoveRef(opposedPath, 2), Detail.Evades(
      piece("white", "rook", "c3"), piece("black", "rook", "g1")))))
    assert(!opposedActions.contains(Action(MoveRef(opposedPath, 2), Detail.Evades(
      piece("white", "rook", "c3"), piece("black", "bishop", "g5")))))

  test("a delayed root target escape retains the first capture and recapture before that escape"):
    val f = prepare("7k/r7/8/2b5/8/8/5P2/Q4R1K w - - 0 1",
      "a1a2 c5f2 f1f2 a7b7", "a1b1 h8g8")
    val path = f.pair.head
    val target = piece("black", "rook", "a7")
    val response = TargetResponse(path, 0, target, piece("white", "queen", "a1"), target,
      Some(3), None, survives = false)
    val result = f.close(f.answer(List(response)))
    assertEquals(result.core.flatMap(_.covered).toSet, Set(MoveRef(path, 1), MoveRef(path, 2)))
    val text = words(result, f.paths)
    assert(text.contains("Bxf2") && text.contains("Rxf2"), text)
    assert(RootQuestionSummary.realize(f.paths, f.pair, result).nonEmpty)

    val routeOnly = f.close(f.answer(List(PieceRoute(path, target, List(3)))))
    assertEquals(routeOnly.core.flatMap(_.covered), Nil,
      "an available escape route alone does not activate an earlier independent trade")

    val unknown = f.arms(f.positions - path.positions(2))
    val unproved = MoveWorkProjection.completeSelectedPrices(unknown, f.answer(List(response)), List(response)).get
    assertEquals(unproved.core.flatMap(_.covered), Nil,
      "missing control cannot establish that the root attack persists through the first reply")

  test("an independent capture after the root target escapes stays outside that response"):
    val f = prepare("7k/r7/8/2b5/8/8/5P2/Q4R1K w - - 0 1",
      "a1a2 h8h7 h1g1 a7b7 a2e2 c5f2 f1f2", "a1b1 h8g8")
    val path = f.pair.head
    val target = piece("black", "rook", "a7")
    val response = TargetResponse(path, 0, target, piece("white", "queen", "a1"), target,
      Some(3), None, survives = false)
    val result = f.close(f.answer(List(response)))
    assertEquals(result.core.flatMap(_.covered), Nil)
    assert(RootQuestionSummary.realize(f.paths, f.pair, result).nonEmpty)

  test("selected en passant owns the immediately preceding original pawn and both distinct squares"):
    val f = prepare("7k/1p6/8/P7/8/8/8/7K b - - 0 1",
      "b7b5 a5b6 h8g8", "h8g8 h1g1")
    val path = f.pair.head
    val result = f.close(f.answer(List(f.capture(1))))
    val ep = statements(result).collect { case value: EnPassantCapture => value }.head
    assertEquals(ep.advance, MoveRef(path, 0))
    assertEquals(ep.capture, MoveRef(path, 1))
    assertEquals(ep.originalPawn, piece("black", "pawn", "b7"))
    assertEquals(ep.originalCapturer, piece("white", "pawn", "a5"))
    assertEquals(ep.captured, piece("black", "pawn", "b5"))
    assertEquals(ep.destination, "b6")
    assertEquals(ep.covered, List(MoveRef(path, 1)))
    val parts = CommentaryEnglish.statement(ep)
    assertEquals(parts.collect { case move: MoveRef => move }.toSet, ep.references.toSet)
    val text = words(result, f.paths)
    List("en passant", "originally on b7", "originally on a5", "on b5", "on b6").foreach(value =>
      assert(text.contains(value), text))
    val other = prepare("7k/1p6/8/P7/8/8/8/7K b - - 0 1",
      "b7b5 a5b6 h8g8", "h8g8 h1g1")
    assert(MoveWorkProjection.completeSelectedConditions(f.packet, f.paths, f.pair, f.positions, f.notes,
      other.answer(List(other.capture(1)))).isEmpty)

  test("ordinary pawn capture and an en-passant root without an owned predecessor do not invent a double push"):
    val ordinary = prepare("7k/8/1p6/8/P7/8/8/7K b - - 0 1",
      "b6b5 a4b5", "h8g8 h1g1")
    val selected = ordinary.answer(List(ordinary.capture(1)))
    assert(!statements(ordinary.close(selected)).exists(_.isInstanceOf[EnPassantCapture]))
    val earlierAdvance = prepare("7k/1p6/8/8/P7/8/8/7K b - - 0 1",
      "b7b5 h1g1 h8g8 a4b5", "h8g8 h1g1")
    assert(!statements(earlierAdvance.close(earlierAdvance.answer(List(earlierAdvance.capture(3)))))
      .exists(_.isInstanceOf[EnPassantCapture]), "a non-immediate double push is not an en-passant premise")
    val rootCapture = prepare("7k/8/8/Pp6/8/8/8/7K w - b6 0 2",
      "a5b6 h8g8", "h1g1 h8g8")
    val root = rootCapture.answer(List(rootCapture.capture(0)))
    assert(!statements(rootCapture.close(root)).exists(_.isInstanceOf[EnPassantCapture]))
    assert(root.core.flatMap(_.covered).contains(MoveRef(rootCapture.pair.head, 0)))

  test("an admitted same-root support witness remains owned when it is outside the comparison pair"):
    val f = prepare("7k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1",
      "f1e1 h8h7 c3b5", "g1h1 h8h7", "f1e1 f6e4 e1e4")
    val witness = f.paths.paths.find(_.moves == List("f1e1", "f6e4", "e1e4")).get
    assert(!f.pair.exists(_ eq witness))
    assertEquals(witness.ids.head, f.pair.head.ids.head)
    val use = LineContinuationInvestigation.directSupportUse(f.packet, witness, f.paths, witness.moves.size,
      piece("white", "rook", "f1"), piece("white", "pawn", "e4")).uses.head
    val support = SupportRecapture(MoveRef(f.pair.head, 0), witness, use)
    val material = MaterialEvents(witness, witness.certified.flatMap(_._2.material).get.events)
    val selected = f.answer(List(support, material))
    val result = f.close(selected)
    assert(statements(result).contains(support) && statements(result).contains(material))
    assert(result.required.exists(_.path eq witness))
    assertEquals(result.question, selected.question)
    assertEquals(result.details, selected.details)
    assert(RootQuestionSummary.realize(f.paths, f.pair, result).nonEmpty,
      "the existing final witness ownership contract still admits the answer")

  test("escaping one target retains the same attacker's actual other-target exchange"):
    // Already opened Fundamentals Game 9: the same e6-knight attacks Nd4 and Bf4.
    val f = prepare("2r5/1b2kp2/p3n1p1/1p1p4/3N1B1P/4PP2/PP4P1/3R2K1 w - - 3 27",
      "d4b3 e6f4 e3f4 c8c2 d1c1 c2c4 c1e1 e7d6 b3a5 c4c7 g1f2 b7c8 g2g4 c8d7 f2g3",
      "d4e6 e7e6 g2g4 f7f6 h4h5 g6g5 f4g3 b5b4 d1d2 e6f7 g1f2 a6a5 f3f4 c8c1 b2b3 f7e6")
    val chosen = RootQuestionSummary.choose(f.packet, f.paths, f.pair, f.positions).get.confirmedFacts
    val closed = f.close(chosen)
    val path = f.pair.head
    val price = statements(closed).collectFirst { case value: Material if value.path eq path => value }.get
    assertEquals(price.cost.whiteAdditionalLosses, List(EvidencePieceRole("bishop") -> 1))
    assertEquals(price.cost.blackAdditionalLosses, List(EvidencePieceRole("knight") -> 1))
    assertEquals(price.cost.events.map(_.plyOffset), List(1, 2))
    assert(!closed.core.flatMap(_.covered).contains(MoveRef(path, 3)),
      "the following rook's independent pawn capture is outside the selected attack episode")
    assertEquals(closed.question, chosen.question)
    val delivered = RootQuestionSummary.realize(f.paths, f.pair, closed).get
    assert(delivered.text.contains("one bishop for one knight"), delivered.text)
    val sparse = f.answer(List(MaterialEvents(path, path.certified.flatMap(_._2.material).get.events
      .filter(event => Set(1, 3)(event.plyOffset)))))
    val completed = MoveWorkProjection.completeSelectedPrices(f.arms(), sparse,
      sparse.core.flatMap(_.statements)).get
    val prices = statements(completed).collect { case value: Material if value.path eq path => value }
    assertEquals(prices.map(_.cost.events.map(_.plyOffset)), List(List(1, 2)),
      "the selected knight's continuously attacked recapture owns its bishop-for-knight price")
    assert(!completed.core.flatMap(_.covered).exists(ref => (ref.path eq path) && ref.offset >= 3),
      "the next rook move does not enlarge the selected knight's capture account")
    assertEquals(completed.question, sparse.question)
    assertEquals(completed.details, sparse.details)

  test("a selected supporting piece's first-reply loss retains the root gain and actual recovery"):
    // Already opened Fundamentals Game 4. The supporting rook is not the root pawn.
    val f = prepare("k1r1rq2/p4p1p/2p2p2/N1Pn4/2P1pP2/3R2P1/P6P/2KR2Q1 w - - 0 30",
      "c4d5 e4d3 a5c6 e8e4 d1d3 e4c4 c1d2 c4c5 g1d4 f8d6 d2e3 c8c7 d4e4 c7c8 e4d4",
      "d3b3 e4e3 a5b7 d5c3 b3c3 e3e2 d1e1 a8b7 g1f2 c8d8 c3c2 e8e4 c2e2 e4c4 e2c2 d8d4 f2e2 f8c5 e2e7 b7b6 e7c5 b6c5 e1e7 c4c2 c1c2 d4a4 c2b2 a4d4 e7f7")
    val path = f.pair.head
    val support = f.notes.find(note => note.edge.evidence.id == path.ids.head && (note.note.detail match
      case Detail.Support(provider, target, _, _) => provider == piece("white", "rook", "d3") &&
        target == piece("white", "pawn", "d5")
      case _ => false)).get
    val selected = f.answer(List(Action(MoveRef(path, 0), support.note.detail)))
    val closed = f.close(selected)
    val price = statements(closed).collectFirst { case value: Material if value.path eq path => value }.get
    assertEquals(price.cost.whiteAdditionalLosses, List(EvidencePieceRole("rook") -> 1))
    assertEquals(price.cost.commonExchanges, List(EvidencePieceRole("pawn") -> 1))
    assertEquals(price.cost.blackAdditionalLosses,
      List(EvidencePieceRole("pawn") -> 1, EvidencePieceRole("knight") -> 1))
    assertEquals(price.cost.events.map(_.plyOffset), List(0, 1, 2, 4, 7),
      "the actual recovery and the following rook-check response's pawn loss both remain in the price")
    assert(price.references.contains(MoveRef(path, 5)) && price.references.contains(MoveRef(path, 6)),
      "the price keeps the actual countercheck and king response as proof without calling the quiet check a capture")
    assert(!closed.core.flatMap(_.covered).contains(MoveRef(path, 5)))
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("a selected supported target keeps its other-square price in the alternative arm"):
    // Already opened Fundamentals Game 1; the same original e5-knight is the subject in both arms.
    val f = prepare("1r4k1/p4ppp/P1b5/2QpNq2/8/2P1P3/5PPP/R5K1 w - - 2 24",
      "f2f4 b8b1 a1b1 f5b1 g1f2 c6e8 c5a7 h7h5 a7e7 h5h4 e7e8 g8h7 e8a4 b1b2 f2f3 h4h3 g2h3 b2h2",
      "h2h3 f5e5 c5c6 e5e6 c6c7 e6b6 c7d7 g7g6 a1a4 b8d8 d7b7 b6c5 a4f4 d8f8 g1h2 g8g7 b7d7 c5b6 d7d5 b6a6")
    val supported = f.pair.head
    val peer = f.pair.last
    val support = f.notes.find(note => note.edge.evidence.id == supported.ids.head && (note.note.detail match
      case Detail.Support(provider, target, _, _) => provider == piece("white", "pawn", "f4") &&
        target == piece("white", "knight", "e5")
      case _ => false)).get
    val chosen = f.answer(List(Action(MoveRef(supported, 0), support.note.detail)))
    val closed = f.close(chosen)
    val price = statements(closed).collectFirst { case value: Material if value.path eq peer => value }.get
    assertEquals(price.cost.whiteAdditionalLosses, List(EvidencePieceRole("knight") -> 1))
    assertEquals(price.cost.blackAdditionalLosses, List(EvidencePieceRole("bishop") -> 1))
    assertEquals(price.cost.events.map(_.plyOffset), List(1, 2))
    assert(!closed.core.flatMap(_.covered).contains(MoveRef(peer, 19)),
      "the queen's much later pawn capture does not belong to this original threat response")
    assertEquals(closed.question, chosen.question)
    val delivered = RootQuestionSummary.realize(f.paths, f.pair, closed).get
    assert(delivered.text.contains("one knight for one bishop"), delivered.text)
    assert(!delivered.text.contains("fxe5") && !delivered.text.toLowerCase.contains("forced"),
      "a legal but unprovided counterfactual is not promoted into the observed comparison")

  test("a selected capture retains the promotion of its same original victim before the exchange"):
    // Already opened Fundamentals Game 6: the new c-pawn queen and original g2-queen are different pieces.
    val f = prepare("5k2/1p6/5n2/p7/b1P5/8/6Q1/6K1 w - - 1 60",
      "g2b2 f8f7 b2b7 f7f8 c4c5 a4e8 c5c6 f6d5 c6c7 d5e7 c7c8q e7c8 b7c8 a5a4 c8c7 e8g6 c7c6 g6e8 c6f6 f8g8 f6e7 e8g6 e7d8 g8f7",
      "g2b7 a4e8 c4c5 f6g8 b7f3 f8g7 f3g4 g7f7 g4c4 f7f8 c5c6 g8e7 c4f4 f8g7 c6c7 a5a4 f4e5 g7f7")
    val path = f.pair.head
    val reading = f.paths.continuationReadings.find(_.path eq path).get
    val source = path.certified.flatMap(_._2.material).get
    val exchange = source.events.filter(event => Set(11, 12)(event.plyOffset))
    assertEquals(reading.events.find(_.plyOffset == 10).get.originalActor,
      reading.events.find(_.plyOffset == 11).get.originalVictim)
    assertEquals(reading.events.find(_.plyOffset == 12).get.originalActor,
      Some(piece("white", "queen", "g2")))
    val closed = f.close(f.answer(List(MaterialEvents(path, exchange))))
    val grouped = statements(closed).collect { case value: MaterialEvents if value.path eq path => value.events }
    assert(grouped.exists(_.map(_.plyOffset) == List(10, 11, 12)),
      "the original pawn's role change remains before its queen loss and the original queen's recapture")
    assertEquals(grouped.flatten.count(_.plyOffset == 10), 1,
      "a role change is owned once even though it is not capture coverage")
    assertEquals(closed.core.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet, Set(11, 12))
    assert(closed.core.flatMap(_.covered).contains(MoveRef(f.pair.last, 0)),
      "new context re-enters common completion and retains the alternative's real root capture")
    assert(!statements(closed).exists {
      case value: Material if value.path eq path => true
      case _ => false
    }, "a promotion context is not silently converted to an ordinary queen-for-knight account")
    assert(CommentaryMaterialAccount.calculate(source, List(10, 11, 12), List(10, 11, 12), path.moves.size).isEmpty)
    val delivered = RootQuestionSummary.realize(f.paths, f.pair, closed).get
    assert(delivered.text.indexOf("c8=Q") < delivered.text.indexOf("Nxc8"), delivered.text)
    val onlyLoss = f.close(f.answer(List(MaterialEvents(path, exchange.take(1)))))
    assert(statements(onlyLoss).exists {
      case value: MaterialEvents if value.path eq path => value.events.map(_.plyOffset) == List(10, 11, 12)
      case _ => false
    }, "adding an owned recapture cannot detach the same victim's promotion from its price")
    assert(!statements(onlyLoss).exists {
      case value: Material if value.path eq path => true
      case _ => false
    })
    val unrelated = f.close(f.answer(List(MaterialEvents(path, source.events.filter(_.plyOffset == 2)))))
    assert(!statements(unrelated).flatMap(_.references).contains(MoveRef(path, 10)),
      "the original queen's earlier pawn capture does not pull in another piece's later promotion")

  test("a selected root support retains a delayed provider price only during the same verified relations"):
    // Synthetic relation safety checks, not additional master-game quality results.
    def selected(f: Prepared) =
      val path = f.pair.head
      val support = f.notes.find(note => note.edge.evidence.id == path.ids.head && (note.note.detail match
        case Detail.Support(provider, target, _, _) => provider == piece("white", "rook", "d3") &&
          target == piece("white", "pawn", "d5")
        case _ => false)).get
      f.answer(List(Action(MoveRef(path, 0), support.note.detail)))
    def prices(f: Prepared, at: Map[PositionNodeRef, Position]) =
      val answer = selected(f)
      MoveWorkProjection.completeSelectedPrices(f.arms(at), answer, statements(answer)).get

    val continuous = prepare("7k/8/8/3p4/2P1p3/3R4/8/K7 w - - 0 1",
      "c4d5 h8g8 a1b1 g8h8 b1a1 e4d3", "d3d2 h8g8")
    val path = continuous.pair.head
    val closed = prices(continuous, continuous.positions)
    val price = statements(closed).collectFirst { case value: Material if value.path eq path => value }.get
    assertEquals(price.cost.events.map(_.plyOffset), List(0, 5))
    assertEquals(price.cost.whiteAdditionalLosses, List(EvidencePieceRole("rook") -> 1))
    assertEquals(price.cost.blackAdditionalLosses, List(EvidencePieceRole("pawn") -> 1))
    assertEquals(closed.question, selected(continuous).question)
    assertEquals(closed.details, selected(continuous).details)
    assertEquals(prices(continuous, continuous.positions - path.positions(3)), selected(continuous),
      "an unknown intermediate attack/support position cannot certify the episode")

    val lostSupport = prepare("7k/8/2p5/3p4/2P1p3/3R4/8/K7 w - - 0 1",
      "c4d5 h8g8 d5c6 g8h8 a1b1 e4d3", "d3d2 h8g8")
    assertEquals(prices(lostSupport, lostSupport.positions), selected(lostSupport),
      "the original target moving out of support ends this episode before the provider loss")
    val lostAttack = prepare("7k/8/8/3p4/2P5/3R4/4b3/K7 w - - 0 1",
      "c4d5 e2f3 a1b1 f3e2 b1a1 e2d3", "d3d2 h8g8")
    assertEquals(prices(lostAttack, lostAttack.positions), selected(lostAttack),
      "the same eventual capturer cannot reconnect an attack that had stopped")

  test("a support use keeps its continuous price and a stationary provider's separate observed loss"):
    // Synthetic legal lines isolate the selected use from a root-support statement.
    def selected(f: Prepared) =
      val path = f.pair.head
      val use = LineContinuationInvestigation.directSupportUse(f.packet, path, f.paths, path.moves.size,
        piece("white", "rook", "f1"), piece("white", "pawn", "e4")).uses.head
      assert(f.arms().head.recovery(use.recapture).exists(_.plyOffset == 7))
      f.answer(List(SupportRecapture(MoveRef(path, 0), path, use)))
    def prices(f: Prepared, at: Map[PositionNodeRef, Position]) =
      val answer = selected(f)
      MoveWorkProjection.completeSelectedPrices(f.arms(at), answer, statements(answer)).get

    val continuous = prepare("4r2k/8/5n2/8/4P3/8/8/5RK1 w - - 0 1",
      "f1e1 f6e4 e1e4 h8h7 g1h1 h7h8 h1g1 e8e4", "g1h1 h8h7")
    val path = continuous.pair.head
    val price = statements(prices(continuous, continuous.positions)).collectFirst {
      case value: Material if value.path eq path => value }.get
    assertEquals(price.cost.events.map(_.plyOffset), List(1, 2, 7))
    assertEquals(price.cost.whiteAdditionalLosses,
      List(EvidencePieceRole("pawn") -> 1, EvidencePieceRole("rook") -> 1))
    assertEquals(price.cost.blackAdditionalLosses, List(EvidencePieceRole("knight") -> 1))
    assertEquals(prices(continuous, continuous.positions - path.positions(5)), selected(continuous),
      "a linked removal still needs verified attack throughout the owned post-use episode")
    val broken = prepare("4r2k/8/5n2/8/4P3/8/8/5RK1 w - - 0 1",
      "f1e1 f6e4 e1e4 e8d8 g1h1 d8e8 h1g1 e8e4", "g1h1 h8h7")
    val observed = prices(broken, broken.positions)
    assert(statements(observed).exists {
      case value: PieceExchange => (value.path eq broken.pair.head) && value.events.map(_.plyOffset) == List(7)
      case _ => false
    }, "the used rook stayed on e4; its recorded loss remains an outcome despite the interrupted attack")
    assert(!statements(observed).exists(_.isInstanceOf[Material]),
      "the separate loss cannot reconnect that interrupted attack into a continuous price interval")
    val departed = prepare("4r2k/8/5n2/8/4P3/8/8/5RK1 w - - 0 1",
      "f1e1 f6e4 e1e4 e8d8 e4d4 d8e8 d4e4 e8e4", "g1h1 h8h7")
    assertEquals(prices(departed, departed.positions), selected(departed),
      "leaving and returning to the use square ends the bounded stationary outcome")

  test("a shared support use keeps its verified root obstruction and each arm's complete price together"):
    // Opened Candidates 2018 game 7. Both recaptures take the same original b6-knight.
    val f = prepare("r4k1r/ppp2pp1/1n2b3/2p1P1Bp/2P5/P5PP/2P2PK1/3RRN2 w - - 1 24",
      "f1e3 b6c4 e3c4 e6c4 g3g4 b7b5 f2f4 b5b4 a3b4 c5b4 f4f5 h5g4 h3g4 c4b5 g2g3 a7a5 e5e6 f7e6 e1e6 a8e8 d1d5 e8e6 f5e6 b5e8 d5f5 f8g8 f5a5 h8h1",
      "f1d2 b6a4 f2f3 f8e8 g3g4 e8d7 g2g3 a4b2 d1b1 b2c4 d2c4 e6c4 b1b7 h5g4 h3g4 h8b8 b7b1 b8b1 e1b1 c4d5")
    val uses = f.pair.map { path =>
      val use = LineContinuationInvestigation.directSupportUse(f.packet, path, f.paths, path.moves.size,
        piece("white", "knight", "f1"), piece("white", "pawn", "c4")).uses.head
      SupportedCapture(MoveRef(path, 0), f.paths.pieceAfter(f.packet, path,
        piece("white", "knight", "f1"), 1).flatten.get, piece("white", "pawn", "c4"),
        use, fromStart = true, None, None)
    }
    assertEquals(uses.map(_.use.originalCapturer).distinct, List(piece("black", "knight", "b6")))
    val rook = piece("white", "rook", "e1")
    val pawn = piece("white", "pawn", "e5")
    val contrast = ControlContrast(MoveRef(f.pair.head, 0), rook, Some(pawn), "e5", false,
      MoveRef(f.pair.last, 0), rook, Some(pawn), "e5", true)
    val otherTrade = MaterialEvents(f.pair.head, f.pair.head.certified.flatMap(_._2.material).get.events
      .filter(event => Set(8, 9)(event.plyOffset)))
    val selected = List(Section(uses ++ List(contrast, otherTrade)))
    val scoped = RootQuestionSummary.pairedSupportAnswer(f.packet, f.paths, f.pair, f.positions, selected).get
    assertEquals(scoped.question, "paired_support_use")
    assert(scoped.core.head.statements.head == contrast, "the actual difference precedes both support uses")
    assert(scoped.core.flatMap(_.statements).exists {
      case value: ControlState => value.move == MoveRef(f.pair.head, 2) && value.actor == rook &&
        value.target.contains(pawn) && value.present
      case _ => false
    }, "the knight's actual use restores the rook relation that its root move blocked")
    assert(scoped.details.flatMap(_.statements).contains(otherTrade))
    val closed = f.close(scoped)
    // In the Nd2 arm, the selected Nxc4 departure also clears b2 for the
    // actual Rxb7+. Claiming that connected use owns its check reply and
    // ensuing recapture; it is not the Ne3 arm's independent pawn trades.
    f.pair.zip(uses).zip(List(List(1, 2, 3), List(9, 10, 11, 12, 13, 14))).foreach { case ((path, use), expected) =>
      val owner = closed.core.find(_.statements.contains(use)).get
      assertEquals(owner.covered.filter(_.path eq path).map(_.offset).distinct.sorted, expected)
      assert(owner.covered.forall(_.path eq path), "the sibling root reference cannot steal this use's price")
    }
    assert(closed.core.flatMap(_.statements).exists {
      case route: ClearedRoute => route.move == MoveRef(f.pair.last, 9) &&
        route.use == MoveRef(f.pair.last, 12) && route.before == piece("black", "knight", "b2")
      case _ => false
    }, "the extra price has its actual opening and use, not a later ledger gain alone")
    Set(8, 9, 17, 18).foreach(offset =>
      assert(!closed.core.flatMap(_.covered).contains(MoveRef(f.pair.head, offset))))
    assertEquals(closed.details, scoped.details)
    val text = RootQuestionSummary.realize(f.paths, f.pair, closed).get.text
    List("Ne3", "Nd2", "c4", "pawn on e5", "rook on e1").foreach(s => assert(text.contains(s), text))
    assert(RootQuestionSummary.pairedSupportAnswer(f.packet, f.paths, f.pair, f.positions,
      List(Section(uses))).isEmpty, "equal support outcomes alone cannot replace an independent comparison")
    assert(RootQuestionSummary.pairedSupportAnswer(f.packet, f.paths, f.pair,
      f.positions - f.pair.head.positions(3), selected).isEmpty,
      "unknown restoration cannot certify a root obstruction/use comparison")
    assert(RootQuestionSummary.pairedSupportAnswer(f.packet, f.paths, f.pair, f.positions,
      List(Section(List(uses.head, contrast)))).isEmpty, "one core use cannot borrow its counterpart from detail")

  test("a quiet vacancy answer owns both actual evasions while independent trades stay in detail"):
    val f = prepare("r2r2k1/1p1bppbp/npp3p1/6N1/3PPP2/2P3PP/P5B1/R1B2RK1 w - - 1 16",
      "e4e5 a6c7 a2a4 a8a6 c1d2 d8a8 a1b1 f7f6 g5e4 d7e6 f1e1 f6f5 e4g5 e6c4 g2f1 c4f1 g1f1 a8a7 f1e2 g7h6 g5f3 a6a4 b1b6 c7d5 b6b2",
      "g3g4 a6c7 a2a4 c6c5 c1e3 h7h6 g5f3 d7a4 f1c1 a4b5 a1a8 d8a8 c3c4 a8a3 c4b5 a3e3 d4c5 b6c5 c1c5 c7e6 c5c8 g8h7 c8e8")
    val path = f.pair.head
    val pawn = piece("white", "pawn", "e4")
    val knight = piece("white", "knight", "g5")
    val route = SquareReoccupation(MoveRef(path, 0), pawn, piece("white", "pawn", "e5"), "e4",
      MoveRef(path, 8), knight, piece("white", "knight", "e4"))
    val extra = f.capture(21)
    val selected = List(Section(List(route, extra)))
    val answer = RootQuestionSummary.vacancyResponseAnswer(f.packet, f.paths, f.pair, f.positions, selected).get
    assertEquals(answer.question, "observed_route_use")
    assertEquals(answer.core.flatMap(_.covered), Nil)
    assert(answer.details.flatMap(_.statements).contains(extra))
    val responses = answer.core.flatMap(_.statements).collect { case value: Action => value }
    assertEquals(responses.map(v => v.move.path -> v.move.offset).toSet,
      Set(path -> 7, path -> 8, f.pair.last -> 5, f.pair.last -> 6))
    val closed = f.close(answer)
    val text = RootQuestionSummary.realize(f.paths, f.pair, closed).get.text
    List("e5", "g4", "f6", "Ne4", "h6", "Nf3", "pawn originally on e4").foreach(s => assert(text.contains(s), text))
    assert(!text.contains("Rxa4") && !text.contains("safe") && !text.contains("forced"), text)
    assertEquals(closed.details, answer.details)
    assert(RootQuestionSummary.vacancyResponseAnswer(f.packet, f.paths, f.pair,
      f.positions - f.pair.last.positions(6), selected).isEmpty,
      "the counterpart's actual attack cannot be borrowed from the chosen line")
    val forged = route.copy(originalEntry = piece("white", "bishop", "g2"))
    assert(RootQuestionSummary.vacancyResponseAnswer(f.packet, f.paths, f.pair, f.positions,
      List(Section(List(forged)))).isEmpty, "the named ally must be the original actual mover")
    val unrelated = SquareReoccupation(MoveRef(f.pair.last, 0), piece("white", "pawn", "g3"),
      piece("white", "pawn", "g4"), "g3", MoveRef(f.pair.last, 6), knight, piece("white", "knight", "f3"))
    assert(RootQuestionSummary.vacancyResponseAnswer(f.packet, f.paths, f.pair, f.positions,
      List(Section(List(route, unrelated)))).isEmpty, "a different already selected root/use keeps its scope")

  test("an observed other-square countercapture closes only the originally selected economic reply"):
    // Opened Corus 2006 game 1. cxb5 is answered by Rxe3, not a legal recapture on b5.
    val f = prepare("r2r2k1/1p1bppbp/npp3p1/6N1/3PPP2/2P3PP/P5B1/R1B2RK1 w - - 1 16",
      "g3g4 a6c7 a2a4 c6c5 c1e3 h7h6 g5f3 d7a4 f1c1 a4b5 a1a8 d8a8 c3c4 a8a3 c4b5 a3e3 d4c5 b6c5 c1c5 c7e6 c5c8 g8h7 c8e8",
      "e4e5 a6c7 a2a4 a8a6 c1d2 d8a8 a1b1 f7f6 g5e4 d7e6 f1e1 f6f5 e4g5 e6c4 g2f1 c4f1 g1f1 a8a7 f1e2 g7h6 g5f3 a6a4 b1b6 c7d5 b6b2")
    val path = f.pair.head
    val selected = f.answer(List(MaterialEvents(path, path.certified.flatMap(_._2.material).get.events
      .filter(_.plyOffset == 14))))
    val closed = f.close(selected)
    val price = statements(closed).collectFirst { case v: PieceExchange if v.path eq path => v }.get
    assertEquals(price.events.map(_.plyOffset), List(14, 15))
    assertEquals(price.events.map(_.originalVictim.get.role.name), List("bishop", "bishop"))
    assertEquals(closed.core.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet, Set(14, 15),
      "the generated reply is not a new seed for the following dxc5/captures")
    assertEquals(closed.question, selected.question)
    assertEquals(closed.details, selected.details)
    val delivered = RootQuestionSummary.realize(f.paths, f.pair, closed).get.text
    List("cxb5", "Rxe3").foreach(san => assert(delivered.contains(san), delivered))
    val proofOnly = selected.copy(core = List(Section(List(Continuation(f.pair.map(MoveRef(_, 0)))))),
      details = selected.core ++ selected.details)
    val bounded = f.close(proofOnly)
    assert(!bounded.core.flatMap(_.covered).contains(MoveRef(path, 15)),
      "a capture in detail cannot create a new basic economic answer")

  test("a selected gain keeps its exact chasing reply, escape and other-square countercapture"):
    // Opened Corus 2006 game 4. Qxb7 Rb8 Qa6 Rxb2 is one recorded response episode.
    val f = prepare("r3kb1r/pppqp2p/1n2p1p1/n7/1Q1P4/2N1BP2/PP2N1PP/R2R2K1 b kq - 3 16",
      "a5c6 b4b3 b6d5 e3f2 f8g7 b3b7 a8b8 b7a6 b8b2 a6a3 b2b6 c3e4 e8g8 e4c5 d7d6 g1h1 f8b8 e2c3 b6b2",
      "a5c4 e3f4 a7a5 b4b5 f8g7 f4c7 d7b5 c3b5 b6d5 b2b3 c4e3 d1c1 e8g8 c7e5 a5a4 e5g7 g8g7 b5c3 a4b3 a2b3 a8a1 c1a1")
    val path = f.pair.head
    val selected = f.answer(List(MaterialEvents(path, path.certified.flatMap(_._2.material).get.events
      .filter(_.plyOffset == 5))))
    val closed = f.close(selected)
    val price = statements(closed).collectFirst { case v: PieceExchange if v.path eq path => v }.get
    assertEquals(price.events.map(_.plyOffset), List(5, 8))
    assertEquals(price.events.map(_.originalVictim.get.role.name), List("pawn", "pawn"))
    assert((5 to 8).forall(n => closed.required.contains(MoveRef(path, n))))
    assertEquals(closed.core.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet, Set(5, 8))
    assertEquals(closed.details, selected.details)
    val rendered = RootQuestionSummary.realize(f.paths, f.pair, closed).get.text
    List("Qxb7", "Rb8", "Qa6", "Rxb2").foreach(san => assert(rendered.contains(san), rendered))
    assert(!rendered.toLowerCase.contains("forced"), "recorded timing does not prove a forced response")
    val unknown = MoveWorkProjection.completeSelectedPrices(f.arms(f.positions - path.positions(7)),
      selected, statements(selected)).get
    assertEquals(unknown, selected, "unknown chasing geometry cannot certify this quiet reply episode")

  test("prepared support retains its used original pawn's stationary loss without intervening trades"):
    // Opened Tata 2025 game 10. e4 prepares f5; exf5 is used and that same e3-pawn later falls to Nxf5.
    val f = prepare("3r1rk1/pbq1ppbp/1p1n2p1/n1p1N3/2P2P2/3PPN2/PBQ1B1PP/R4RK1 w - - 7 16",
      "e3e4 f7f6 e5g4 d6f7 f4f5 g6f5 e4f5 f7h6 g4e3 c7f4 f1f2 f4e3 b2c1 e3f2 g1f2 h6f5 g2g4 f5h6 h2h3 a5c6 c1b2 h6f7 a1d1 e7e5 f3h4 f6f5 h4f5 f7d6 f2g1 d6f5 g4f5 f8f5 e2g4 f5g5",
      "a1d1 d6f5 c2c1 a5c6 a2a3 b7a8 g2g4 c6e5 f3e5 f5h4 e3e4 a8b7 c1c2 f7f6 e5f3 h4f3 f1f3")
    val path = f.pair.head
    val use = LineContinuationInvestigation.directSupportUse(f.packet, path, f.paths, path.moves.size,
      piece("white", "pawn", "e3"), piece("white", "pawn", "f4")).uses.head
    assertEquals(use.supportCount, 5)
    val receipt = SupportedCapture(MoveRef(path, 0), piece("white", "pawn", "e4"),
      piece("white", "pawn", "f4"), use, fromStart = false, Some(MoveRef(path, 4)), None)
    val selected = f.answer(List(receipt))
    val closed = f.close(selected)
    val loss = statements(closed).collectFirst { case value: PieceExchange if (value.path eq path) &&
      value.events.map(_.plyOffset) == List(15) => value }.get
    assertEquals(loss.events.head.originalVictim, Some(piece("white", "pawn", "e3")))
    assertEquals(closed.core.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet, Set(5, 6, 15))
    assert(!statements(closed).exists(_.isInstanceOf[Material]),
      "the stationary outcome is not an interval through the unselected queen/rook/knight exchange")
    assertEquals(closed.details, selected.details)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).get.text.contains("Nxf5"))
    val unknown = MoveWorkProjection.completeSelectedPrices(f.arms(f.positions - path.positions(10)),
      selected, statements(selected)).get
    assertEquals(unknown, selected, "the stationary outcome needs the whole owned identity prefix")

  test("an opened master line does not extend a selected support into the provider's later lifetime"):
    // Fundamentals Game 7, ply 56: Red8 supports Bd6; that bishop blocks support of Nd5.
    val f = prepare("r3rk2/2p2p2/p2b4/3npN2/P7/8/1P2BPPP/R2R2K1 b - - 1 28",
      "e8d8 d1d5 d8b8 a1b1 b8b4 b2b3 a8b8 g2g3 b4b3 b1b3 b8b3 e2a6 b3b6",
      "d5f4 e2c4 e8d8 a1c1 f4e6 g2g3 e6g7 f5h6 g7e6 c1c3 e5e4 c4e6 f7e6 h6g4 d8b8 d1d4 b8b2 d4e4 a6a5 e4e6 b2b4 c3f3 f8g7 g4h6 b4a4")
    val path = f.pair.head
    val support = f.notes.find(note => note.edge.evidence.id == path.ids.head && (note.note.detail match
      case Detail.Support(provider, target, _, _) => provider == piece("black", "rook", "d8") &&
        target == piece("black", "bishop", "d6")
      case _ => false)).get
    val selected = f.answer(List(Action(MoveRef(path, 0), support.note.detail)))
    val closed = MoveWorkProjection.completeSelectedPrices(f.arms(), selected, statements(selected)).get
    assertEquals(closed, selected,
      "Rdb8 ends the selected Bd6 support before the same rook's later capture")
    assert(!closed.core.flatMap(_.covered).contains(MoveRef(path, 9)),
      "its supporting rook's later loss after the target has gone is outside this episode")

  test("a verified price keeps an exact selected recorded check ending without inventing a reply"):
    // Fundamentals Game 5, ply 64: saved Nc4 line ends at Rh7+, after the actual rook exchange.
    val f = prepare("b5r1/4rk2/1nppNp2/1p3Pp1/1P2P1P1/2N2K1R/2P5/3R4 b - - 1 32",
      "b6c4 f3g3 f7e8 e4e5 f6e5 c3e4 e7f7 h3h6 c6c5 e4d6 c4d6 d1d6 f7d7 d6d7 e8d7 h6h7",
      "e7d7 f3g3")
    val path = f.pair.head
    val use = LineContinuationInvestigation.directSupportUse(f.packet, path, f.paths, path.moves.size,
      piece("black", "knight", "b6"), piece("black", "pawn", "d6")).uses.head
    val arm = f.arms().head
    val loss = arm.captures.find(_.originalVictim.contains(piece("black", "knight", "b6"))).get
    val account = arm.account(loss, piece("black", "knight", "b6"))
    assert(!account.complete)
    assertEquals(account.offsets.filter(n => arm.checks(n) && !account.offsets.contains(n + 1)), List(15))
    val support = SupportRecapture(MoveRef(path, 0), path, use)
    val ending = RecordedEnd(MoveRef(path, path.moves.size - 1))
    val selected = f.answer(List(support, ending))
    val closed = MoveWorkProjection.completeSelectedPrices(f.arms(), selected, statements(selected)).get
    val price = statements(closed).collectFirst { case value: Material if value.path eq path => value }.get
    assertEquals(price.cost.events.map(_.plyOffset), List(9, 10, 11, 13, 14))
    assert(price.references.contains(ending.move))
    assert(statements(closed).contains(ending) && !account.complete)
    assert(!closed.required.exists(ref => (ref.path eq path) && ref.offset == path.moves.size))
    val delivery = closed.copy(details = closed.details.map(section =>
      section.copy(statements = section.statements :+ ending)))
    assert(RootQuestionSummary.realize(f.paths, f.pair, delivery).nonEmpty)
    List(f.answer(List(support)),
      f.answer(List(support, RecordedEnd(MoveRef(path, 9)))),
      f.answer(List(support, RecordedEnd(MoveRef(f.pair.last, f.pair.last.moves.size - 1))))).foreach { invalid =>
      assert(MoveWorkProjection.completeSelectedPrices(f.arms(), invalid, statements(invalid)).isEmpty,
        "missing, interior or sibling recorded ends cannot bound this price")
    }
    val detailOnly = f.answer(List(support)).copy(details = List(Section(List(ending))))
    assert(MoveWorkProjection.completeSelectedPrices(f.arms(), detailOnly, statements(detailOnly)).isEmpty,
      "a detail-only recorded ending is not owned by the selected basic answer")

  test("a selected master queen capture owns its continuously attacked removal and intervening check reply"):
    // Opened WCC 2016 game 9, before White's 24th move. No new engine analysis.
    val f = prepare("1r1r2k1/5ppp/Rb1q4/7n/3P4/1B2BP2/5P1P/3Q1R1K w - - 2 24",
      "f1g1 d6d7 g1g5 g7g6 h1g2 b6d4 g5d5 d7d5 b3d5 d8d5 e3h6 b8d8 h2h4 d4g7 d1d5 h5f4 h6f4 d8d5 a6a8 g7f8 a8e8",
      "d1d3 d6d7 f1b1 b6c7 h1g2 h5f4 e3f4 c7f4 b1e1 d7b5 d3b5 b8b5 b3c4 b5g5 g2f1 f4d6 a6a7")
    val path = f.pair.head
    val source = path.certified.flatMap(_._2.material).get
    val selected = f.answer(List(MaterialEvents(path, source.events.filter(_.plyOffset == 14))))
    val result = f.close(selected)
    val price = statements(result).collectFirst { case value: Material if value.path eq path => value }.get
    assertEquals(price.cost.events.map(_.plyOffset), List(14, 16, 17))
    assertEquals(price.cost.whiteAdditionalLosses, List(EvidencePieceRole("queen") -> 1))
    assertEquals(price.cost.blackAdditionalLosses,
      List(EvidencePieceRole("knight") -> 1, EvidencePieceRole("rook") -> 1))
    assert(price.references.contains(MoveRef(path, 15)) && price.references.contains(MoveRef(path, 16)))
    assert(!price.cost.events.exists(_.plyOffset == 7), "an earlier queen exchange is a different episode")
    assertEquals(result.question, selected.question)
    assertEquals(result.details, selected.details)
    assert(RootQuestionSummary.realize(f.paths, f.pair, result).nonEmpty)

    val uncertain = MoveWorkProjection.completeSelectedPrices(f.arms(f.positions - path.positions(16)),
      selected, statements(selected)).get
    assertEquals(uncertain, selected, "a physical link does not replace an unknown intermediate attack")

    val gain = MaterialEvents(path, source.events.filter(_.plyOffset == 16))
    val split = selected.copy(core = selected.core :+ Section(List(gain), gain.references))
    val joined = f.close(split)
    assert(joined.required.contains(MoveRef(path, 16)), "moving a price cannot discard its required reference")
    assertEquals(statements(joined).collect { case value: Material if value.path eq path =>
      value.cost.events.map(_.plyOffset) }, List(List(14, 16, 17)))
    assert(!statements(joined).exists {
      case value: MaterialEvents if value.path eq path => value.events.exists(_.plyOffset == 16)
      case _ => false
    }, "the transferred capture is not repeated as a separate price")
    assert(RootQuestionSummary.realize(f.paths, f.pair, joined).nonEmpty)

  test("a capturing clearance owns only its actual exposed-queen reply and observed recovery"):
    // Opened WCC 2024 game 12, before Black's 20th move.
    val line = "e7f6 g1e2 g6h7 c3c2 d8d7 h3h4 c6e7 d4d5 b6d5 c2c1 h7f5 b5c3 d5e3 d1d7 e3g2 h2g2 f5d7 c3e4"
    val other = "f7f5 f2f3 e7f6 c3c2 a5a4 d4d5 c6e5 e3b6 c7b6 f3e4 b8c8 e4f5 c8c4 c2b1 c4c5"
    val fen = "1r1qr1k1/1pp1bpp1/1nn3bp/pN6/2PPp3/P1Q1B1PP/1P3PBK/3RR1N1 b - - 3 20"
    val f = prepare(fen, line, other)
    val path = f.pair.head
    val source = path.certified.flatMap(_._2.material).get
    val selected = f.answer(List(MaterialEvents(path, source.events.filter(_.plyOffset == 12))))
    val result = f.close(selected)
    val route = statements(result).collectFirst { case value: ClearedRoute if value.use.offset == 13 => value }.get
    assertEquals(route.move, MoveRef(path, 12))
    assertEquals(route.before, piece("black", "knight", "d5"))
    assertEquals(route.originalUser, piece("white", "rook", "d1"))
    assertEquals(route.targets, List(piece("black", "queen", "d7")))
    val price = statements(result).collectFirst { case value: Material if value.path eq path => value }.get
    assertEquals(price.cost.events.map(_.plyOffset), List(12, 13, 14, 15, 16))
    assertEquals(price.cost.whiteAdditionalLosses,
      List(EvidencePieceRole("bishop") -> 2, EvidencePieceRole("rook") -> 1))
    assertEquals(price.cost.blackAdditionalLosses,
      List(EvidencePieceRole("knight") -> 1, EvidencePieceRole("queen") -> 1))
    assert(!price.cost.events.exists(_.plyOffset == 17), "the next pawn capture does not start a new price")
    assertEquals(result.details, selected.details)
    assert(RootQuestionSummary.realize(f.paths, f.pair, result).nonEmpty)

    val horizon = prepare(fen, line.split(" ").take(13).mkString(" "), other)
    val last = horizon.pair.head
    assert(horizon.arms().head.reading.captureExposures.exists(_.prefixMoves.size == 13))
    val only = horizon.answer(List(MaterialEvents(last,
      last.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 12))))
    val unchanged = MoveWorkProjection.completeSelectedPrices(horizon.arms(), only, statements(only)).get
    assertEquals(unchanged, only, "an available response without actual use is not an observed queen loss")

  test("a selected master capture does not own its capturer's later return into a new attack"):
    // Opened Candidates 2018 game 10. Same physical queen, different attack episodes.
    val f = prepare("r6k/3n2pp/p1Np1r1b/1p1Pp3/PQ2BB2/5P2/1PP1q3/1K1R2R1 b - - 0 24",
      "h6f4 b4a5 f4e3 g1e1 e2f2 a5c7 d7c5 e1g1 e3h6 d1f1 f2e2 c6b8 f6f8 c7d6 a8b8 d6c5 b5a4 d5d6 e2b5 c5b5 a6b5 f1d1 b5b4 e4c6",
      "e5f4 g1e1 e2c4 b4a5 b5a4 a5c7 d7e5 e1h1 a8f8 c7e7 e5g6 e4g6 h7g6 c6d8 a4a3 d8e6 f6f7 e7d6 f8c8 d1d2 a3b2")
    val path = f.pair.head
    val arm = f.arms().head
    val source = path.certified.flatMap(_._2.material).get
    List(13, 15).foreach { offset =>
      val event = arm.captures.find(_.plyOffset == offset).get
      assert(arm.recovery(event).exists(_.plyOffset == 20))
      assertEquals(arm.continuousRecovery(event), None)
      val selected = f.answer(List(MaterialEvents(path, source.events.filter(_.plyOffset == offset))))
      val completed = MoveWorkProjection.completeSelectedPrices(f.arms(), selected, statements(selected)).get
      if offset == 13 then assert(completed == selected)
      else
        // The selected Qxc5 answers the supported knight's immediate loss.
        // Its legal alternative recapture is proof, not the queen's entire lifetime.
        val prices = statements(completed).collect { case value: Material if value.path eq path =>
          value.cost.events.map(_.plyOffset) }
        assertEquals(prices, List(List(14, 15)))
        assertEquals(completed.core.flatMap(_.covered).map(_.offset), List(14, 15))
        assert(!completed.core.flatMap(_.covered).contains(MoveRef(path, 20)),
          "the actual countercapture price cannot own the queen's later return into another attack")
    }

  test("a removal ending in check uses only an exact owned recorded boundary"):
    // Synthetic safety case; this is not a master-game generalization result.
    val f = prepare("2r4k/2p5/2Q5/8/8/8/2K5/8 w - - 0 1",
      "c6c7 c8c7", "c2b2 h8g8")
    val path = f.pair.head
    val first = MaterialEvents(path, path.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 0))
    val end = RecordedEnd(MoveRef(path, 1))
    val selected = f.answer(List(first, end))
    val closed = MoveWorkProjection.completeSelectedPrices(f.arms(), selected, statements(selected)).get
    assertEquals(statements(closed).collectFirst { case value: Material if value.path eq path =>
      value.cost.events.map(_.plyOffset) }.get, List(0, 1))
    assert(statements(closed).contains(end))
    val checking = statements(closed).collect { case value: CheckSequence => value.events }.flatten
    assert(checking.exists { case value: CheckingMove => value.move == end.move; case _ => false })
    assert(!checking.exists(_.isInstanceOf[CheckReply]), "the recorded end does not invent a legal response")
    assert(!closed.required.exists(ref => (ref.path eq path) && ref.offset == 2))
    val delivery = closed.copy(details = closed.details.map(section =>
      section.copy(statements = section.statements :+ end)))
    assert(RootQuestionSummary.realize(f.paths, f.pair, delivery).nonEmpty)
    List(f.answer(List(first)), f.answer(List(first, RecordedEnd(MoveRef(path, 0)))),
      f.answer(List(first, RecordedEnd(MoveRef(f.pair.last, 1)))),
      f.answer(List(first)).copy(details = List(Section(List(end))))).foreach { invalid =>
      assert(MoveWorkProjection.completeSelectedPrices(f.arms(), invalid, statements(invalid)).isEmpty,
        "missing, interior, sibling and detail-only ends cannot close this basic price")
    }

  test("a selected attack's actual use keeps the alternative's reversed first-reply price"):
    // Opened WCC 2016 game 11: the same c3/d4 pawns capture each other in the two actual arms.
    val f = prepare("4r1k1/6pp/p3q3/1p4Q1/3pp3/P1PP3P/2P3P1/5RK1 w - - 0 24",
      "f1e1 d4c3 e1e4 e6b6 g1h1 e8e4 d3e4 b6d6 e4e5 d6a3 e5e6 a3d6 g5f5 d6d1 h1h2 d1d6 h2g1 d6d4 g1h2",
      "c3d4 e4e3 f1e1 h7h6 g5c5 e6f6 e1e2 e8f8 c5d5 f8f7 d5a8 f7f8")
    val path = f.pair.head
    val other = f.pair.last
    val attack = f.notes.find(note => note.edge.evidence.id == path.ids.head && (note.note.detail match
      case Detail.Attack(actor, target, _) => actor == piece("white", "rook", "e1") &&
        target == piece("black", "pawn", "e4")
      case _ => false)).get
    val gain = MaterialEvents(path, path.certified.flatMap(_._2.material).get.events
      .filter(event => Set(2, 5, 6)(event.plyOffset)))
    val counterpart = MaterialEvents(other, other.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 0))
    val selected = f.answer(List(Action(MoveRef(path, 0), attack.note.detail), gain, counterpart))
    val arm = f.arms().head
    val use = arm.captures.find(_.plyOffset == 2).get
    assert(arm.recovery(use).exists(_.plyOffset == 5) && arm.continuousRecovery(use).isEmpty,
      "the queen initially screens the recovering rook, so a physical link alone cannot own this new attack")
    val closed = f.close(selected)
    val price = statements(closed).collectFirst { case value: Material if value.path eq path => value }.get
    assertEquals(price.cost.events.map(_.plyOffset), List(1, 2, 5, 6))
    assertEquals(price.cost.commonExchanges,
      List(EvidencePieceRole("pawn") -> 1, EvidencePieceRole("rook") -> 1))
    assertEquals(price.cost.whiteAdditionalLosses, Nil)
    assertEquals(price.cost.blackAdditionalLosses, Nil)
    assert(!price.covered.contains(MoveRef(path, 9)), "a different pawn's later loss is not this attack's price")
    assertEquals(closed.question, selected.question)
    assertEquals(closed.details, selected.details)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

    val withoutCounterpart = selected.copy(core = selected.core.map(section =>
      section.copy(statements = section.statements.filterNot(_ == counterpart))))
    List(withoutCounterpart, selected.copy(core = selected.core.map(section =>
      section.copy(statements = section.statements.filterNot(_ == gain))))).foreach { invalid =>
      val bounded = MoveWorkProjection.completeSelectedPrices(f.arms(), invalid, statements(invalid)).get
      assert(!bounded.core.flatMap(_.covered).contains(MoveRef(path, 1)),
        "both originally selected sides of the reply/use relation are needed")
    }
    val unknown = MoveWorkProjection.completeSelectedPrices(f.arms(f.positions - path.positions(1)),
      selected, statements(selected)).get
    assert(!unknown.core.flatMap(_.covered).contains(MoveRef(path, 1)), "unknown attack continuity cannot add a price")

  test("an automatic draw after a checking removal preserves the actual terminal without a reply"):
    // Synthetic boundary: Nxb6+ leaves only kings and a knight.
    val f = prepare("2n4k/8/1pQ5/8/K7/8/8/8 w - - 0 1", "c6b6 c8b6", "a4a3 h8g8")
    val path = f.pair.head
    val terminal = Terminal(MoveRef(path, 1), path.automaticTerminal.get)
    val first = MaterialEvents(path, path.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 0))
    val selected = f.answer(List(first, terminal))
    val closed = MoveWorkProjection.completeSelectedPrices(f.arms(), selected, statements(selected)).get
    assert(statements(closed).contains(terminal))
    assertEquals(closed.core.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet, Set(0, 1))
    assert(!closed.required.exists(ref => (ref.path eq path) && ref.offset == 2))
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("selected support departure keeps its exact gain and actual supported-target loss together"):
    // Opened Candidates 2018 game 9; support begins within the line, not at its root.
    val f = prepare("r1b1k1r1/ppp1qp1p/8/2p4n/3QP1pP/2P3P1/PP3P2/RNB2R1K w q - 0 16",
      "d4d3 c8d7 c3c4 e8c8 b1c3 g8e8 f1e1 e7e5 h1h2 d7e6 c3d5 e6d5 c4d5 f7f5 c1g5 f5e4 d3a3 d8d5 a3a7 e5b2 h2g1 b2b6 a7a8 c8d7 a8a3 b6c6",
      "d4c4 c8e6 c4d3 a8d8 d3c2 f7f5 e4f5 e6c4 f1g1 d8d3 h1h2 d3f3 c1g5 g8g5 h4g5 h5f4 g3f4 f3h3 h2g2 c4d3 c2a4 b7b5")
    val path = f.pair.head
    val support = f.notes.find(note => note.edge.evidence.id == path.ids(16) && (note.note.detail match
      case Detail.Support(provider, target, _, _) => provider == piece("white", "queen", "a3") &&
        target == piece("white", "pawn", "b2")
      case _ => false)).get
    val action = Action(MoveRef(path, 16), support.note.detail)
    val gain = MaterialEvents(path, path.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 18))
    val selected = f.answer(Nil).copy(core = List(Section(List(Continuation(f.pair.map(MoveRef(_, 0))), gain), gain.references),
      Section(List(action), action.references, List(Focus(action.move, support)))))
    val closed = f.close(selected)
    val owner = closed.core.find(_.statements.contains(action)).get
    assert(owner.statements.exists {
      case value: ControlState => value.move == MoveRef(path, 18) && !value.present &&
        value.target.contains(piece("white", "pawn", "b2"))
      case _ => false
    })
    val receipt = owner.statements.collectFirst { case value: PieceExchange if value.path eq path => value }.get
    assertEquals(receipt.events.map(_.plyOffset), List(18, 19))
    assertEquals(receipt.events(1).originalVictim, Some(piece("white", "pawn", "b2")))
    assert(owner.required.contains(MoveRef(path, 18)) && owner.focus.exists(_.move == action.move))
    assert(!closed.core.flatMap(_.covered).contains(MoveRef(path, 17)),
      "the support anchor is not a material interval start and cannot pull in the other pawn loss")
    assert(!statements(closed).exists {
      case value: Material if value.path eq path => true
      case _ => false
    }, "an identity-bearing complete price is not repeated as the same role/count aggregate")
    assertEquals(closed.details, selected.details)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)
    val source = path.certified.flatMap(_._2.material).get
    val bill = Material(path, CommentaryMaterialAccount.calculate(source, List(18, 19),
      List(0, 18, 19, 20), path.moves.size).get)
    val withQuietProof = selected.copy(core = List(selected.core.head.copy(
      statements = selected.core.head.statements.filterNot(_ == gain) :+ bill,
      required = bill.references), selected.core.last))
    val transferred = f.close(withQuietProof)
    val quiet = MoveRef(path, 20)
    assert(transferred.required.contains(quiet) && transferred.core.flatMap(_.references).contains(quiet),
      "a quiet proof held only by a replaced account is preserved with its required metadata")
    assert(RootQuestionSummary.realize(f.paths, f.pair, transferred).nonEmpty)
    val siblingProof = withQuietProof.copy(core = withQuietProof.core :+ Section(List(Continuation(List(quiet)))))
    val retained = f.close(siblingProof)
    val quietOwner = retained.core.find(_.statements.contains(Continuation(List(quiet)))).get
    assert(quietOwner.required.contains(quiet), "metadata follows an existing sibling owner of the proof")
    assertEquals(retained.core.flatMap(_.statements).count(_ == Continuation(List(quiet))), 1,
      "retaining a sibling proof does not duplicate its bare continuation")
    assert(RootQuestionSummary.realize(f.paths, f.pair, retained).nonEmpty)
    List(selected.copy(core = List(selected.core.head)), selected.copy(core = List(
      selected.core.head.copy(statements = selected.core.head.statements.filterNot(_ == gain)), selected.core.last))).foreach { invalid =>
      val bounded = MoveWorkProjection.completeSelectedPrices(f.arms(), invalid, statements(invalid)).get
      assert(!bounded.core.flatMap(_.covered).contains(MoveRef(path, 19)),
        "detail/proof-only support or departure cannot become a new basic answer")
    }
    val unknown = MoveWorkProjection.completeSelectedPrices(f.arms(f.positions - path.positions(19)),
      selected, statements(selected)).get
    assert(!unknown.core.flatMap(_.covered).contains(MoveRef(path, 19)), "unknown ended support is not absent support")

  test("a promotion identity witness does not import intervening unrelated trades into a later price"):
    // Synthetic long-gap safety case; the intervening bishop takes a different pawn.
    val f = prepare("1Q6/2P1n2k/8/8/1b6/8/P7/6K1 w - - 0 1",
      "c7c8q h7g6 a2a3 b4a3 g1h1 e7c8 b8c8", "b8b7 h7g6")
    val path = f.pair.head
    val source = path.certified.flatMap(_._2.material).get
    val selected = f.answer(List(MaterialEvents(path, source.events.filter(_.plyOffset == 5))))
    val closed = f.close(selected)
    val groups = statements(closed).collect { case value: MaterialEvents if value.path eq path => value.events }
    assert(groups.exists(_.map(_.plyOffset) == List(0, 5, 6)),
      "the victim's promotion stays with its loss and the original queen's actual recapture")
    assert(!closed.core.flatMap(_.covered).contains(MoveRef(path, 3)),
      "preserving a past identity is not ownership of every trade since its role change")
    assertEquals(closed.details, selected.details)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("a newly selected support price uses current roles when its target was already promoted"):
    // Synthetic role boundary; a promoted queen is not described as a pawn capture victim.
    val f = prepare("1Q6/2P1n2k/1p6/8/8/8/8/6K1 w - - 0 1",
      "c7c8q h7g7 b8b6 e7c8", "b8a8 h7g7")
    val path = f.pair.head
    val support = f.notes.find(note => note.edge.evidence.id == path.ids.head && (note.note.detail match
      case Detail.Support(provider, target, _, _) => provider == piece("white", "queen", "b8") &&
        target == piece("white", "queen", "c8")
      case _ => false)).get
    val loss = f.arms().head.captures.find(_.plyOffset == 3).get
    assertEquals(loss.originalVictim, Some(piece("white", "pawn", "c7")))
    assert(!f.arms().head.identityRoles(List(loss)))
    val source = path.certified.flatMap(_._2.material).get
    val selected = f.answer(List(Action(MoveRef(path, 0), support.note.detail),
      MaterialEvents(path, source.events.filter(_.plyOffset == 2))))
    val closed = f.close(selected)
    assert(statements(closed).exists {
      case value: MaterialEvents if value.path eq path => value.events.map(_.plyOffset) == List(0, 2, 3) &&
        value.events.last.capture.exists(_.capturedRole.name == "queen")
      case _ => false
    })
    assert(!statements(closed).exists {
      case value: PieceExchange if value.path eq path => value.events.contains(loss)
      case _ => false
    }, "an original-role receipt must not call the promoted victim a pawn")
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("a promoted target's actual reply retains its identity inside a larger complete price"):
    // Synthetic safety boundary: the supporter is captured after the promoted target.
    val f = prepare("1Q6/2P1n2k/1p5r/8/8/8/8/6K1 w - - 0 1",
      "c7c8q h7g7 b8b6 e7c8 g1f1 h6b6", "b8a8 h7g7")
    val path = f.pair.head
    val support = f.notes.find(note => note.edge.evidence.id == path.ids.head && (note.note.detail match
      case Detail.Support(provider, target, _, _) => provider == piece("white", "queen", "b8") &&
        target == piece("white", "queen", "c8")
      case _ => false)).get
    val source = path.certified.flatMap(_._2.material).get
    val selected = f.answer(List(Action(MoveRef(path, 0), support.note.detail),
      MaterialEvents(path, source.events.filter(_.plyOffset == 2))))
    val closed = f.close(selected)
    val delivered = statements(closed)
    val whole = delivered.collectFirst { case value: Material if value.path eq path => value }.get
    assertEquals(whole.cost.events.map(_.plyOffset), List(2, 3, 5))
    assert(delivered.exists {
      case value: MaterialEvents if value.path eq path => value.events.map(_.plyOffset) == List(0, 2, 3) &&
        value.events.last.capture.exists(_.capturedRole.name == "queen")
      case _ => false
    }, "the full price cannot replace the specific promoted victim and its role-change context")
    assertEquals(closed.details, selected.details)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)

  test("continuous attack alone does not make unselected intervening exchanges an initial price"):
    // Saved balanced master comparison, also exercised by public delivery tests.
    val f = prepare("2rq1rk1/3n1ppp/p2bp3/1pp5/3P4/1N2Q1P1/PP1NPPKP/R1R5 w - - 0 20",
      "d2e4 c5d4 b3d4 d7c5 d4f3 c5e4 e3e4 d8b6 c1c2 c8c2 e4c2 d6e7 a1c1 b6a5 c2c7 a5c7 c1c7 e7f6 b2b3 f8d8 c7c1 g7g5",
      "d4c5 d7c5 c1d1 c8c7 a1c1 d8a8 d2f3 c5b3 c1c7 d6c7 e3b3 f8d8")
    val path = f.pair.last
    val arm = f.arms().last
    val firstReply = arm.captures.find(_.plyOffset == 1).get
    assert(arm.continuousRecovery(firstReply).exists(_.plyOffset == 10),
      "the same knight is physically removed while the queen's attack persists")
    val source = path.certified.flatMap(_._2.material).get
    val firstPrice = MaterialEvents(path, source.events.filter(event => Set(0, 1)(event.plyOffset)))
    val selected = f.answer(List(firstPrice))
    val bounded = MoveWorkProjection.completeSelectedPrices(f.arms(), selected, statements(selected)).get
    assertEquals(bounded.core.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet, Set(0, 1),
      "unowned knight and rook trades separate the later removal from this chosen pawn exchange")
    assertEquals(bounded.details, selected.details)

  test("a pinned recapturer uses its exact admitted legal capture along the pin ray"):
    // Synthetic legality boundary, not a quality/generalization position.
    val f = prepare("1r1r2k1/8/8/8/8/8/8/1R4K1 w - - 0 1",
      "b1b8 d8b8", "g1f1 b8b2")
    val path = f.pair.head
    val arm = f.arms().head
    val first = arm.captures.find(_.plyOffset == 0).get
    val recovering = arm.captures.find(_.plyOffset == 1).get.originalActor.get
    assert(!arm.covers(1, recovering, first.originalActor.get),
      "unpinned geometric cover does not establish this legal pin-ray recapture")
    assert(arm.continuousRecovery(first).exists(_.plyOffset == 1),
      "the exact legal inventory proves the pinned rook can take the checking-line attacker")
    val source = path.certified.flatMap(_._2.material).get
    val selected = f.answer(List(MaterialEvents(path, source.events.filter(_.plyOffset == 0))))
    val closed = f.close(selected)
    assertEquals(statements(closed).collectFirst { case value: Material if value.path eq path =>
      value.cost.events.map(_.plyOffset) }.get, List(0, 1))
    assertEquals(closed.details, selected.details)
    assert(RootQuestionSummary.realize(f.paths, f.pair, closed).nonEmpty)
