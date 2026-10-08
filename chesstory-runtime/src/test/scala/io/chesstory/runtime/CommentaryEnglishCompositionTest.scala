package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveObservationMeaning, MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{AutomaticTerminal, EngineLine}

/** Fixed legal continuations test realization, not engine strength or selection. */
class CommentaryEnglishCompositionTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import CommentaryEnglish.*
  import MoveWorkProjection.Detail
  import RootQuestionSummary.MoveRef

  private def piece(side: String, role: String, square: String) = CommentaryFact.Piece(side, role, square).witness

  private def prepare(fen: String, lines: List[String]) =
    val rows = lines.map(_.split(" ").toList)
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen,
      playedMoveUci = rows.head.head, variations = rows.map(EngineLine(_, 0, None, 8)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    (packet, paths, rows.map(row => paths.paths.find(_.moves == row).get))

  private def words(parts: List[Part], paths: ObservedLineProtocol.PreparedPaths): String = parts.map {
    case value: String => value
    case move: MoveRef => paths.notation.line(move.path.moves)(move.offset)
  }.mkString

  private def assertReferences(parts: List[Part], section: Section): Unit =
    val actual = parts.collect { case move: MoveRef => move }
    (section.required ++ section.statements.flatMap(_.references)).foreach(ref => assert(actual.exists(value =>
      (value.path eq ref.path) && value.offset == ref.offset), s"missing exact reference $ref"))

  test("mutually exclusive root choices retain their exact references without becoming a recorded sequence"):
    val (_, paths, pair) = prepare(chess.variant.Standard.initialFen.value,
      List("e2e4 e7e5", "d2d4 d7d5"))
    val roots = pair.map(MoveRef(_, 0))
    val selected = Section(List(Continuation(roots)), roots)
    val rendered = renderedSection(selected)
    assertReferences(rendered.parts, selected)
    assertEquals(rendered.expressed, selected.statements)
    assertEquals(words(rendered.parts, paths), "The compared root choices are e4 and d4.")

  test("successive moves on one exact path keep their actual continuation"):
    val (_, paths, pair) = prepare(chess.variant.Standard.initialFen.value,
      List("e2e4 e7e5", "d2d4 d7d5"))
    val moves = List(0, 1).map(MoveRef(pair.head, _))
    val selected = Section(List(Continuation(moves)), moves)
    val rendered = renderedSection(selected)
    assertReferences(rendered.parts, selected)
    assertEquals(words(rendered.parts, paths), "The recorded continuation is e4 e5.")

  test("roots from different starts cannot acquire a common comparison merely from zero offsets"):
    val (_, _, first) = prepare(chess.variant.Standard.initialFen.value,
      List("e2e4 e7e5", "d2d4 d7d5"))
    val (_, _, second) = prepare("rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1",
      List("d7d5 d2d4", "e7e5 e2e4"))
    val roots = List(MoveRef(first.head, 0), MoveRef(second.head, 0))
    val selected = Section(List(Continuation(roots)), roots)
    val rendered = renderedSection(selected)
    assertReferences(rendered.parts, selected)
    assert(!rendered.parts.collect { case value: String => value }.mkString.contains("compared root choices"))

  test("one actor's attack and support share a subject without dropping either target or the opposing attack"):
    val (packet, paths, pair) = prepare("7k/8/8/8/3q1P2/8/8/2B3K1 w - - 0 1",
      List("c1e3", "g1h1"))
    val move = MoveRef(pair.head, 0)
    val actor = piece("white", "bishop", "e3")
    val actions = MoveWorkProjection.select(packet, paths).local.collect {
      case note if note.edge.evidence.id == pair.head.ids.head && (note.note.detail match
        case Detail.Attack(same, _, _) => same == actor
        case Detail.Support(same, _, _, _) => same == actor
        case _ => false) => Action(move, note.note.detail)
    }.distinct
    assert(actions.exists(_.detail.isInstanceOf[Detail.Attack]))
    assert(actions.exists(_.detail.isInstanceOf[Detail.Support]))
    val selected = Section(actions)
    val parts = section(selected)
    assertReferences(parts, selected)
    val text = words(parts, paths)
    List("White's bishop on e3", "Black's queen on d4", "White's pawn on f4", "under attack").foreach(value =>
      assert(text.contains(value), text))
    assertEquals(text.sliding(actor.possessiveName.length).count(_ == actor.possessiveName), 1)
    assertEquals(selected.statements, actions)

  test("a capture of the exact attacker expresses its evasion once, retaining every selected receipt and price"):
    val (_, paths, pair) = prepare("rnbqkb1r/pppp1ppp/5n2/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3",
      List("d2d4 e5d4 f3d4", "b1c3 b8c6"))
    val path = pair.head
    val material = path.certified.flatMap(_._2.material).get
    val evade = Action(MoveRef(path, 1), Detail.Evades(piece("black", "pawn", "d4"), piece("white", "pawn", "d4")))
    val events = paths.continuationReadings.find(_.path eq path).get.events.filter(_.originalVictim.nonEmpty)
    val captures = List[Statement](MaterialEvents(path, material.events),
      PieceExchange(path, events, material.captures.filter(_.recapture).map(_.plyOffset).toSet))
    captures.foreach { capture =>
      // The evasion may precede the physical capture in the selected facts.
      val selected = Section(List(evade, capture))
      val rendered = renderedSection(selected)
      assertReferences(rendered.parts, selected)
      assertEquals(rendered.expressed.toSet, selected.statements.toSet)
      assertEquals(rendered.expressed.flatMap(_.covered).toSet, selected.covered.toSet)
      val text = words(rendered.parts, paths)
      assert(text.contains("captures the attacking white pawn"), text)
      assert(!text.contains("moves the pawn out of the attack"), text)
      assert(text.contains("Nxd4"), text)
    }

  test("another real attacker and mismatched path, victim or moving piece keep independent evasion descriptions"):
    val fen = "7k/8/8/4p3/3P2N1/8/8/7K b - - 0 1"
    val (_, paths, pair) = prepare(fen, List("e5d4 h1g1", "h8g8 h1g1"))
    val path = pair.head
    val move = MoveRef(path, 0)
    val pawn = piece("black", "pawn", "d4")
    val removed = Action(move, Detail.Evades(pawn, piece("white", "pawn", "d4")))
    val other = Action(move, Detail.Evades(pawn, piece("white", "knight", "g4")))
    val capture = MaterialEvents(path, path.certified.flatMap(_._2.material).get.events)
    val selected = Section(List(removed, other, capture))
    val rendered = renderedSection(selected)
    assertReferences(rendered.parts, selected)
    assertEquals(rendered.expressed.toSet, selected.statements.toSet)
    val text = words(rendered.parts, paths)
    assert(text.contains("captures the attacking white pawn"), text)
    assert(text.contains("out of the attack from White's knight on g4"), text)
    assert(!text.contains("out of the attack from White's pawn on d4"), text)
    val (_, _, separate) = prepare(fen, List("e5d4 h1g1", "h8g8 h1g1"))
    val mismatches = List(removed.copy(move = MoveRef(separate.head, 0)),
      Action(move, Detail.Evades(pawn, piece("black", "pawn", "d4"))),
      Action(move, Detail.Evades(pawn, piece("white", "pawn", "e4"))),
      Action(move, Detail.Evades(piece("black", "knight", "d4"), piece("white", "pawn", "d4"))),
      Action(move, Detail.Evades(piece("white", "pawn", "d4"), piece("white", "pawn", "d4"))),
      Action(move, Detail.Evades(piece("black", "pawn", "e5"), piece("white", "pawn", "d4"))))
    mismatches.foreach { mismatch =>
      val independent = Section(List(mismatch, capture))
      val rendered = renderedSection(independent)
      assertReferences(rendered.parts, independent)
      assertEquals(rendered.expressed.toSet, independent.statements.toSet)
      assert(!words(rendered.parts, paths).contains("attacking white pawn"))
    }

  test("a support introduction connects to its actual capture and recapture without borrowing another line's context"):
    val (packet, paths, pair) = prepare("7k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1",
      List("f1e1 h8h7 c3b5 f6e4 e1e4", "g1h1"))
    val path = pair.head
    val move = MoveRef(path, 0)
    val target = piece("white", "pawn", "e4")
    val use = LineContinuationInvestigation.directSupportUse(packet, path, paths, path.moves.size,
      piece("white", "rook", "f1"), target).uses.head
    val action = Action(move, Detail.Support(use.provider, target, List(use.capturer), true))
    val continuation = SupportedCapture(move, use.provider, target, use, true, None, None)
    val selected = Section(List(action, continuation, Continuation(List(MoveRef(pair.last, 0)))))
    val parts = section(selected)
    assertReferences(parts, selected)
    val text = words(parts, paths)
    List("under attack", "That support continues", "Nxe4", "Rxe4", "recaptures its capturer", "Kh1").foreach(value =>
      assert(text.contains(value), text))
    assertEquals(text.sliding(use.provider.possessiveName.length).count(_ == use.provider.possessiveName), 1)
    val standalone = words(statement(continuation), paths)
    assert(standalone.contains(use.provider.possessiveName) && standalone.contains(target.possessiveName), standalone)
    assert(!standalone.startsWith("That support"), standalone)

  test("capture chains abbreviate only the identified preceding capturer and retain every recapture reference"):
    val (packet, paths, pair) = prepare("2r1r2k/p2n1qp1/1p3pRp/3P4/2P5/P2Q4/1B4PP/5RK1 w - - 5 31",
      List("b2f6 d7f6 f1f6 g7f6", "d3d1"))
    val path = pair.head
    val reading = paths.continuationReadings.find(_.path eq path).get
    val events = reading.events.filter(_.originalVictim.nonEmpty)
    val ledger = path.certified.flatMap(_._2.material).get
    val recaptures = ledger.captures.filter(_.recapture).map(_.plyOffset).toSet
    val exchange = PieceExchange(path, events, recaptures)
    val selected = Section(List(exchange))
    val parts = section(selected)
    assertReferences(parts, selected)
    val text = words(parts, paths)
    assert(text.contains("recaptures that bishop") && text.contains("recaptures that knight") &&
      text.contains("recaptures that rook"), text)
    assertEquals(exchange.covered.toSet, parts.collect { case ref: MoveRef => ref }.toSet)
    events.foreach(event => assert(text.contains(originalDescription(event.originalActor.get)), text))
    // Same role, different original piece: it must be introduced explicitly.
    val changedIdentity = events(1).copy(originalVictim = Some(piece("white", "bishop", "c1")))
    val independent = words(statement(PieceExchange(path, List(events.head, changedIdentity), recaptures)), paths)
    assert(independent.contains("white bishop originally on c1"), independent)
    assert(!independent.contains("that piece"), independent)
    val alone = words(statement(PieceExchange(path, List(events(1)), recaptures)), paths)
    assert(alone.contains("recaptures") && alone.contains(originalDescription(events(1).originalVictim.get)), alone)

  test("an actual support use owns its captures once, while the supporting piece's later loss remains explicit"):
    val (packet, paths, pair) = prepare("4r2k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1",
      List("f1e1 h8h7 c3b5 f6e4 e1e4 e8e4", "g1h1"))
    val path = pair.head
    val move = MoveRef(path, 0)
    val target = piece("white", "pawn", "e4")
    val use = LineContinuationInvestigation.directSupportUse(packet, path, paths, path.moves.size,
      piece("white", "rook", "f1"), target).uses.head
    val support = SupportedCapture(move, use.provider, target, use, true, None, None)
    val captures = List(MoveRef(path, 3), MoveRef(path, 4))
    assertEquals(support.covered, captures)
    assertEquals(support.copy(fromStart = false, arrival = Some(MoveRef(path, 5)),
      arrivalCapture = Some(Detail.Capture("rook", "e4", LineMaterialRecaptureStatus.Excluded))).covered, captures,
      "an arrival label does not establish that this statement expresses its capture")
    assertEquals(SupportRecapture(move, path, use).covered, captures)
    assertEquals(SupportTransfer(move, use.provider, target, use.provider, piece("white", "rook", "f1"),
      MoveRef(path, 2), use).covered, captures)
    val lead = List(Section(List(support)), Section(List(Continuation(List(MoveRef(pair.last, 0))))))
    val answer = RootQuestionSummary.completePairAnswer(packet, paths, pair, "support_use", lead).get
    val added = answer.core.flatMap(_.statements).collect { case events: MaterialEvents => events }
    assertEquals(added.flatMap(_.covered), List(MoveRef(path, 5)))
    assertEquals(answer.core.flatMap(_.covered).toSet, (captures :+ MoveRef(path, 5)).toSet)
    assert(answer.details.flatMap(_.covered).contains(MoveRef(path, 5)))
    val realized = RootQuestionSummary.realize(paths, pair, answer).get
    assert(realized.text.contains("captures the supported pawn") &&
      realized.text.contains("recaptures its capturer") && realized.text.contains("captures the white rook on e4"), realized.text)
    assert(!realized.text.contains("captures the white pawn on e4") &&
      !realized.text.contains("recaptures the black knight on e4"), realized.text)
    val (promotionPacket, promotionPaths, promotionPair) = prepare("7k/8/8/8/8/5R2/1p5K/R7 w - - 0 1",
      List("f3a3 b2a1q a3a1", "h2g2"))
    val promotedPath = promotionPair.head
    val promotedUse = LineContinuationInvestigation.directSupportUse(promotionPacket, promotedPath,
      promotionPaths, promotedPath.moves.size, piece("white", "rook", "f3"), piece("white", "rook", "a1")).uses.head
    val promotedSupport = SupportedCapture(MoveRef(promotedPath, 0), promotedUse.provider, promotedUse.target,
      promotedUse, true, None, None)
    assertEquals(promotedSupport.covered, List(MoveRef(promotedPath, 2)))
    val promotedAnswer = RootQuestionSummary.completePairAnswer(promotionPacket, promotionPaths, promotionPair,
      "support_use", List(Section(List(promotedSupport)), Section(List(Continuation(List(MoveRef(promotionPair.last, 0))))))).get
    assert(RootQuestionSummary.realize(promotionPaths, promotionPair, promotedAnswer).get.text.contains("promotes to a queen"))

  private def originalDescription(value: MoveWorkProjection.Piece) =
    value.sideName + " " + value.roleName + " originally on " + value.at

  test("a shared control contrast retains both choices and the actual reply, while different actors remain explicit"):
    val (_, paths, pair) = prepare("7k/8/8/8/4P3/3R4/2b5/6K1 w - - 0 1",
      List("d3e3 c2e4 e3e4", "g1h1"))
    val actor = piece("black", "bishop", "c2")
    val target = piece("white", "pawn", "e4")
    val contrast = ControlContrast(MoveRef(pair.head, 0), actor, Some(target), "e4", true,
      MoveRef(pair.last, 0), actor, Some(target), "e4", false, reply = Some(MoveRef(pair.head, 1)))
    val selected = Section(List(contrast))
    val parts = section(selected)
    assertReferences(parts, selected)
    val text = words(parts, paths)
    assert(text.contains("attacks White's pawn on e4, but not after Kh1") && text.contains("Bxe4"), text)
    val distinctActor = contrast.copy(alternativeActor = piece("black", "bishop", "g2"))
    val distinctText = words(statement(distinctActor), paths)
    assert(distinctText.contains("Black's bishop on c2") && distinctText.contains("Black's bishop on g2"), distinctText)

  test("connected prose retains material losses and the exact terminal result rather than implying an advantage"):
    val (_, paths, pair) = prepare("4k3/8/8/3q4/4P3/8/8/4K3 w - - 0 31",
      List("e4d5 e8e7 e1d2 e7d6 d2c3 d6d5", "e1e2"))
    val path = pair.head
    assertEquals(path.automaticTerminal, Some(AutomaticTerminal.InsufficientMaterial))
    val material = path.certified.flatMap(_._2.material).get
    val offsets = material.events.map(_.plyOffset)
    val cost = CommentaryMaterialAccount.calculate(material, offsets, offsets, path.moves.size).get
    val selected = Section(List(Material(path, cost), Terminal(MoveRef(path, path.moves.size - 1),
      path.automaticTerminal.get)))
    val parts = section(selected)
    assertReferences(parts, selected)
    val text = words(parts, paths)
    List("White gives up one pawn for one queen", "Kxd5", "drawn", "neither side can give checkmate").foreach(value =>
      assert(text.contains(value), text))
    assert(!text.contains("wins"), text)

  test("a typed route-use clause keeps its move and square when the caller already owns the introduction"):
    val (_, paths, pair) = prepare("7k/8/8/8/4P3/3R4/2b5/6K1 w - - 0 1",
      List("d3e3 c2e4 e3e4", "g1h1"))
    val route = ClearedRoute(MoveRef(pair.head, 0), piece("white", "rook", "d3"), piece("white", "rook", "e3"),
      piece("black", "bishop", "c2"), MoveRef(pair.head, 1), piece("black", "bishop", "e4"), Nil, true, None)
    val whole = statement(route)
    assertReferences(whole, Section(List(route)))
    val use = clearedRouteUse(route)
    assertEquals(use.collect { case ref: MoveRef => ref }, List(route.use))
    assertEquals(words(use, paths), "Bxe4 passes through the vacated d3 square in this line.")
    assert(words(whole, paths).contains("clears d3; later Bxe4 passes through that square"), words(whole, paths))

  private def checkingRecapture(fen: String, line: String, alternative: String) =
    val (packet, paths, pair) = prepare(fen, List(line, alternative))
    val path = pair.head
    val capture = path.certified.flatMap(_._2.material).get.events.find(_.plyOffset == 1).get
    val detail = packet.observations.collectFirst {
      case observation if observation.transition.evidence.id == path.ids(1) && (observation.meaning match
        case MoveObservationMeaning.CertifiedRelation(_: RelationWitnessDetail.RootCheckResponse) => true
        case _ => false) => observation.meaning match
          case MoveObservationMeaning.CertifiedRelation(value: RelationWitnessDetail.RootCheckResponse) => value
          case _ => fail("expected the admitted response")
    }.get
    (paths, path, MaterialEvents(path, List(capture)), CheckReply(MoveRef(path, 1), detail))

  private lazy val knightRecapture = checkingRecapture("2r4k/8/8/8/8/1N6/8/2R1K3 b - - 0 1",
    "c8c1 b3c1", "c8c7")

  test("adjacent capture and checker-capture descriptions become one sentence with their exact sequence anchor"):
    val (paths, path, material, reply) = knightRecapture
    assert(material.events.head.capture.get.recapture)
    List[Statement](reply, CheckSequence(MoveRef(path, 0), List(reply))).foreach { check =>
      val selected = Section(List(material, check))
      val before = selected.statements
      val parts = section(selected)
      assertReferences(parts, selected)
      assertEquals(selected.statements, before)
      assertEquals(selected.covered, material.covered)
      assertEquals(parts.collect { case ref: MoveRef if ref.offset == 1 => ref }.size, 1)
      val text = words(parts, paths)
      assert(text.contains("Nxc1 recaptures the checking black rook on c1."), text)
      assert(!text.contains("Nxc1 captures the checking rook"), text)
    }

  test("matching notation does not combine a different path, capture identity, movement or promotion"):
    val (paths, path, material, reply) = knightRecapture
    val (_, otherPath, _, _) = checkingRecapture("2r4k/8/8/8/8/1N6/8/2R1K3 b - - 0 1",
      "c8c1 b3c1", "c8c7")
    val resource = reply.detail.response.resource
    val capture = resource.capture.get
    def withResource(value: RelationLegalMoveResourceWitness) =
      reply.copy(detail = reply.detail.copy(response = reply.detail.response.copy(resource = value)))
    val first = path.certified.flatMap(_._2.material).get.events.head
    val changedCaptures = List(capture.copy(capturedSide = material.events.head.movement.side),
      capture.copy(capturedRole = EvidencePieceRole("bishop")), capture.copy(capturedSquare = EvidenceSquare("d1")))
    val mismatches: List[(MaterialEvents, Statement)] =
      List(material -> reply.copy(move = MoveRef(otherPath, 1)),
        material -> CheckSequence(MoveRef(otherPath, 0), List(reply)),
        material -> withResource(resource.copy(movement = first.movement, moveUci = first.moveUci)),
        material.copy(events = first :: material.events) -> reply) ++
        changedCaptures.map(value => material -> withResource(resource.copy(capture = Some(value))))
    mismatches.foreach { (captures, check) =>
      val selected = Section(List(captures, check))
      val parts = section(selected)
      assertReferences(parts, selected)
      val text = words(parts, paths)
      assert(text.contains("recaptures the black rook on c1"), text)
      assert(text.contains("captures the checking"), text)
      assert(!text.contains("recaptures the checking"), text)
    }
    val separated = Section(List(material, Continuation(List(MoveRef(path, 0))), reply))
    assertReferences(section(separated), separated)
    assert(!words(section(separated), paths).contains("recaptures the checking"))

    val (promotionPaths, _, promoted, promotionReply) = checkingRecapture("7K/Pr6/7k/8/8/8/8/8 b - - 0 1",
      "b7b8 a7b8n", "b7b6")
    assert(promoted.events.head.promotionGainCp > 0)
    val promotion = Section(List(promoted, promotionReply))
    val promotionParts = section(promotion)
    assertReferences(promotionParts, promotion)
    val promotionText = words(promotionParts, promotionPaths)
    assert(promotionText.contains("promotes to a knight") && promotionText.contains("captures the checking rook"), promotionText)

  test("a capture with another check-response mode keeps both descriptions and every selected reference"):
    val (paths, _, material, reply) = checkingRecapture("4r2k/8/8/8/8/8/4P3/4K3 b - - 0 1",
      "e8e2 e1e2", "e8e7")
    assertEquals(reply.detail.response.modes.toSet,
      Set(RelationCheckResponseMode.CaptureChecker, RelationCheckResponseMode.KingMove))
    val selected = Section(List(material, reply))
    val parts = section(selected)
    assertReferences(parts, selected)
    val text = words(parts, paths)
    List("recaptures the black rook on e2", "moves the king to e2", "captures the checking rook on e2").foreach(value =>
      assert(text.contains(value), text))
