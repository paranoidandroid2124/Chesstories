package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{AutomaticTerminal, EngineLine}
import play.api.libs.json.*

/** Existing public FEN/UCI cases from PracticalDecisionTest and
  * ReferenceMeaningDeliveryTest; no external fixtures or engine answers.
  */
class CommentaryAnswerTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import RootQuestionSummary.MoveRef

  private val rookFen = "r6k/8/8/8/8/8/8/R6K w - - 0 1"
  private val capture = List("a1a2", "a8a2")
  private val evade = List("a1a3", "a8b8")
  private val attacker = CommentaryFact.Piece("white", "rook", "a1").witness
  private val target = CommentaryFact.Piece("black", "rook", "a8").witness

  private def prepare(fen: String, rows: List[List[String]], mates: List[Int] = Nil) =
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen,
      playedMoveUci = rows.head.head, variations = rows.zipWithIndex.map((moves, index) =>
        EngineLine(moves, 0, mates.lift(index), 8)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    (packet, ObservedLineProtocol.prepare(packet))

  private lazy val rookPair = prepare(rookFen, List(capture, evade))

  private def select(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths,
      rows: List[List[String]]) =
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val local = MoveWorkProjection.select(packet, paths).local
    val contacts = pair.flatMap(path => local.filter(note => note.edge.evidence.id == path.ids.head &&
      (note.note.detail match
        case MoveWorkProjection.Detail.Attack(actor, victim, _) => actor.side.white && victim == target
        case _ => false)).map(path -> _))
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    (pair, RootQuestionSummary.selectedAnswer(packet, paths, pair, positions, contacts))

  private def responses(answer: Answer) = answer.core.flatMap(_.statements).collect {
    case response: TargetResponse => response
  }

  private def render(parts: List[String | MoveRef], paths: ObservedLineProtocol.PreparedPaths): String =
    parts.map {
      case words: String => words
      case move: MoveRef => paths.notation.line(move.path.moves)(move.offset)
    }.mkString

  test("a root capture compares the same victim and retains its gain before delayed removal"):
    val rows = List(List("f8f3", "h1h2"), List("d5d4", "f3f8", "e6f8", "h1h2"))
    val (packet, paths) = prepare("5rk1/8/4n3/3p4/8/5Q2/8/7K b - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val answer = RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions, Nil).get
    val outcomes = answer.core.flatMap(_.statements).collect { case value: TargetOutcome => value }
    val queen = CommentaryFact.Piece("white", "queen", "f3").witness
    assertEquals(outcomes.map(_.original).toSet, Set(queen))
    assertEquals(outcomes.map(_.removal.get.events.head.plyOffset), List(0, 2))
    val prices = answer.core.flatMap(_.statements).collect { case value: Material => value }
    assertEquals(prices.size, 1)
    assertEquals(prices.head.path, pair(1))
    assertEquals(prices.head.cost.commonExchanges, Nil)
    assertEquals(prices.head.cost.whiteAdditionalLosses.map((role, n) => role.name -> n), List("queen" -> 1))
    assertEquals(prices.head.cost.blackAdditionalLosses.map((role, n) => role.name -> n), List("rook" -> 1))
    assertEquals(prices.head.cost.events.map(_.plyOffset), List(1, 2))
    val complete = MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions,
      MoveWorkProjection.select(packet, paths).local, answer).get
    val reading = RootQuestionSummary.realize(paths, pair, complete).get
    val units = (reading.json \ "units").as[List[JsObject]]
    val covered = units.flatMap(unit => (unit \ "covered_moves").as[List[JsObject]]).toSet
    val required = units.flatMap(unit => (unit \ "required_moves").as[List[JsObject]]).toSet
    assert(answer.core.flatMap(_.covered).map(_.json).toSet.subsetOf(covered))
    assert(prices.head.references.map(_.json).forall(required),
      "the captured rook and checking capture/reply remain exact proof without a SAN checklist")

  test("capture versus survival owns the original target while unrelated end history remains detail"):
    val rows = List(List("a4a8", "h8h7"), List("a4d1", "a8a7", "h1g1", "h8g8"))
    val (packet, paths) = prepare("r6k/8/8/8/Q7/8/8/7K w - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val rook = CommentaryFact.Piece("black", "rook", "a8").witness
    val king = CommentaryFact.Piece("black", "king", "h8").witness
    val incidental = PiecePresence(MoveRef(pair.head, 1), king,
      paths.pieceAfter(packet, pair.head, king, pair.head.moves.size).flatten.get)
    val answer = RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions,
      List(Section(List(incidental)))).get
    val outcomes = answer.core.flatMap(_.statements).collect { case value: TargetOutcome => value }
    assertEquals(outcomes.map(_.original).toSet, Set(rook))
    val retained = outcomes.find(_.root.path eq pair(1)).get
    assertEquals(retained.departure, Some(MoveRef(pair(1), 1)))
    assertEquals(retained.retained.get.original, rook)
    assertEquals(retained.retained.get.current.square.key, "a7")
    assert(!answer.core.flatMap(_.statements).contains(incidental))
    assert(answer.details.flatMap(_.statements).contains(incidental))
    val reading = RootQuestionSummary.realize(paths, pair, answer).get
    val end = retained.retained.get.after
    val units = (reading.json \ "units").as[List[JsObject]]
    assert(units.flatMap(unit => (unit \ "required_moves").as[List[JsObject]]).contains(end.json))
    assert(!units.flatMap(unit => (unit \ "en").as[List[JsValue]]).contains(end.json),
      "recorded survival owns the last-position proof without narrating its unrelated king move")
    val forged = retained.copy(original = king)
    assert(RootQuestionSummary.realize(paths, pair,
      answer.copy(core = List(Section(List(forged))))).isEmpty)
    val falseRole = retained.copy(retained = retained.retained.map(_.copy(current = king)))
    assert(RootQuestionSummary.realize(paths, pair,
      answer.copy(core = List(Section(List(falseRole))))).isEmpty)

  test("different root capture victims do not become the first target question"):
    val rows = List(List("d4d8", "h8h7"), List("d4e4", "d8d1", "h1h2"))
    val (packet, paths) = prepare("3q3k/8/8/8/3Qr3/8/8/7K w - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    assert(RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions, Nil).isEmpty)

  test("equal target outcomes and equal prices do not define the choice's core answer"):
    val rows = List(List("a4a8", "h8h7"), List("a4d1", "a8a7", "d1a4", "a7a6", "a4a6", "h8g8"))
    val (packet, paths) = prepare("r6k/8/8/8/Q7/8/8/7K w - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    assert(RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions, Nil).isEmpty,
      "the same rook removal at a different recorded time is not enough to replace another question")

  test("a retained target's final position proof does not select an unrelated final capture"):
    val rows = List(List("a4a8", "h8h7"), List("a4d1", "a8a7", "c3c4", "b5c4"))
    val (packet, paths) = prepare("r6k/8/8/1p6/Q7/2P5/8/7K w - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val answer = RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions, Nil).get
    val incidental = MoveRef(pair(1), 3)
    assert(answer.core.flatMap(_.references).contains(incidental), "the survival proof owns the exact end position")
    assert(!answer.core.flatMap(_.actionReferences).contains(incidental))
    assert(!answer.core.flatMap(_.covered).contains(incidental))
    assert(answer.details.flatMap(_.covered).contains(incidental), "the full exchange ledger remains available")
    val complete = MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions,
      MoveWorkProjection.select(packet, paths).local, answer).get
    assert(!complete.core.flatMap(_.covered).contains(incidental), "proof-only positions cannot seed core prices")
    assert(RootQuestionSummary.realize(paths, pair, complete).nonEmpty)
    val unpricedAction = complete.copy(core = complete.core :+ Section(List(Continuation(List(incidental)))))
    assert(RootQuestionSummary.realize(paths, pair, unpricedAction).isEmpty,
      "an actual selected capture still requires its material explanation")
    val original = CommentaryFact.Piece("black", "rook", "a8").witness
    val current = paths.pieceAfter(packet, pair(1), original, pair(1).moves.size).flatten.get
    val linkedState = PiecePresence(incidental, original, current)
    val stateAnswer = complete.copy(core = complete.core :+ Section(List(linkedState), linkedState.references))
    val stateReading = RootQuestionSummary.realize(paths, pair, stateAnswer).get
    val stateUnits = (stateReading.json \ "units").as[List[JsObject]]
    val stateUnit = stateUnits.find(unit => (unit \ "en").as[List[JsValue]].contains(incidental.json)).get
    assert((stateUnit \ "proof_moves").as[List[JsObject]].contains(incidental.json),
      "the displayed endpoint SAN remains an exact board proof")
    assert(!(stateUnit \ "action_moves").as[List[JsObject]].contains(incidental.json),
      "a linked endpoint does not select its unrelated capture")
    assert(!stateUnits.flatMap(unit => (unit \ "covered_moves").as[List[JsObject]]).contains(incidental.json))

  test("a surviving target's actual gain belongs to its outcome price"):
    val rows = List(List("a4a8", "h8h7"), List("a4d1", "a8a2", "h1g1"))
    val (packet, paths) = prepare("r6k/8/8/8/Q7/8/P7/7K w - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val answer = RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions, Nil).get
    val outcome = answer.core.flatMap(_.statements).collect {
      case value: TargetOutcome if value.root.path eq pair(1) => value
    }.head
    assert(outcome.removal.isEmpty && outcome.retained.nonEmpty)
    val gain = MoveRef(pair(1), 1)
    assert(answer.core.flatMap(_.covered).contains(gain))
    assert(answer.core.flatMap(_.statements).collect { case value: Material if value.path eq pair(1) => value }
      .exists(_.cost.whiteAdditionalLosses == List(EvidencePieceRole("pawn") -> 1)))
    assert(RootQuestionSummary.realize(paths, pair, answer).nonEmpty)

  test("a capture outcome does not evict a different closed root condition and its actual use"):
    val rows = List(List("a4a8", "h8h7"), List("a4d1", "a8a7", "a1a4", "h8h7"))
    val (packet, paths) = prepare("r6k/8/8/8/Q7/8/8/R6K w - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val queen = CommentaryFact.Piece("white", "queen", "a4").witness
    val rook = CommentaryFact.Piece("white", "rook", "a1").witness
    val use = SquareReoccupation(MoveRef(pair(1), 0), queen,
      paths.pieceAfter(packet, pair(1), queen, 1).flatten.get, "a4", MoveRef(pair(1), 2), rook,
      paths.pieceAfter(packet, pair(1), rook, 3).flatten.get)
    assert(RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions, Nil).nonEmpty)
    assert(RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions,
      List(Section(List(use), use.references))).isEmpty,
      "rook survival is not an importance verdict on the separately verified vacancy/use answer")

  test("a dependent capturer's later independent lifetime is not the selected queen's price"):
    // Already-open World Championship 2016 pair; exact provided lines, no engine.
    val rows = List(
      "f8f3 h1g2 f3f7 e3g4 h6h5 g4h2 g6f8 h2f3 f8d7 b2b4 b7b6",
      "d5d4 f3f8 g6f8 c3d4 e5d4 e3c4 f8d7 f2f4 a8b8 c4e5 d7e5 f4e5"
    ).map(_.split(" ").toList)
    val (packet, paths) = prepare("r4rk1/1p4p1/2p1p1np/p2pp3/P3P2P/2PPNQP1/1P3P2/R4R1K b - - 0 24", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val queen = CommentaryFact.Piece("white", "queen", "f3").witness
    val arm = RootQuestionSummary.prepare(packet, paths, pair, positions).get(1)
    val removal = arm.captures.find(_.plyOffset == 2).get
    assertEquals(arm.recovery(removal).map(_.plyOffset), Some(11))
    assertEquals(arm.continuousRecovery(removal), None)
    assertEquals(arm.targetAccount(removal, queen).events.map(_.plyOffset), List(1, 2))
    val answer = RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions, Nil).get
    val prices = answer.core.flatMap(_.statements).collect { case value: Material => value }
    assertEquals(prices.head.cost.events.map(_.plyOffset), List(1, 2))
    assert(!answer.core.flatMap(_.covered).contains(MoveRef(pair(1), 10)))
    assert(answer.details.flatMap(_.covered).contains(MoveRef(pair(1), 10)))
    val complete = MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions,
      MoveWorkProjection.select(packet, paths).local, answer).get
    assert(RootQuestionSummary.realize(paths, pair, complete).nonEmpty)

  test("a narrower target question cannot preserve a selected root use while dropping that user's price"):
    // Opened WCC 2016 game 11: Re1's actual Rxe4 use also owns the rook trade.
    val rows = List(
      "f1e1 d4c3 e1e4 e6b6 g1h1 e8e4 d3e4 b6d6 e4e5 d6a3 e5e6 a3d6 g5f5 d6d1 h1h2 d1d6 h2g1 d6d4 g1h2",
      "c3d4 e4e3 f1e1 h7h6 g5c5 e6f6 e1e2 e8f8 c5d5 f8f7 d5a8 f7f8"
    ).map(_.split(" ").toList)
    val (packet, paths) = prepare("4r1k1/6pp/p3q3/1p4Q1/3pp3/P1PP3P/2P3P1/5RK1 w - - 0 24", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val use = RouteUse(MoveRef(pair.head, 0), MoveRef(pair.head, 2), "rook")
    assert(RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions, Nil).nonEmpty)
    assert(RootQuestionSummary.captureTargetAnswer(packet, paths, pair, positions,
      List(Section(List(use), use.references))).isEmpty,
      "keeping Rxe4 without its selected rook-loss and recapture price is not content preservation")

  test("a stationary dependent capturer's later new attack cannot reopen a target outcome price"):
    val rows = List(List("g8g6", "f1g2", "h8h7", "g2e4", "h7h8", "e4g6"), List("g8f8", "h1g1"))
    val (packet, paths) = prepare("6rk/8/6Q1/8/8/8/P7/5B1K b - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val arm = RootQuestionSummary.prepare(packet, paths, pair, positions).get.head
    val queen = CommentaryFact.Piece("white", "queen", "g6").witness
    val gain = arm.captures.find(_.plyOffset == 0).get
    assertEquals(arm.recovery(gain).map(_.plyOffset), Some(5))
    assertEquals(arm.continuousRecovery(gain), None)
    assertEquals(arm.targetAccount(gain, queen).events.map(_.plyOffset), List(0))
    assertEquals(arm.account(gain, queen).events.map(_.plyOffset), List(0, 5),
      "the existing broader selected account retains its original scope")

  test("a selected supported countercapture retains the actual ally loss without inventing its legal recapture"):
    val rows = List(List("h1g1", "a8b8", "d6c5"), List("h1g2", "a8a7"))
    val (packet, paths) = prepare("rN3r1k/8/3Q4/2n5/8/8/8/7K w - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val arms = RootQuestionSummary.prepare(packet, paths, pair, positions).get
    val gain = MaterialEvents(pair.head, arms.head.material.events.filter(_.plyOffset == 2))
    val roots = Continuation(pair.map(MoveRef(_, 0)))
    val original = Answer("selected_comparison", List(Section(List(roots, gain))), Nil, Nil)
    val completed = MoveWorkProjection.completeSelectedPrices(arms, original, original.core.flatMap(_.statements)).get
    val price = completed.core.flatMap(_.statements).collect { case value: Material => value }.head
    assertEquals(price.cost.events.map(_.plyOffset), List(1, 2))
    assertEquals(price.cost.commonExchanges, List(EvidencePieceRole("knight") -> 1))
    assertEquals(price.cost.whiteAdditionalLosses, Nil)
    assertEquals(price.cost.blackAdditionalLosses, Nil)
    val recapturePosition = positions(pair.head.positions(2))
    val recapture = recapturePosition.view.legalRows.get.find(_.move == "d6b8").get
    val proof = recapturePosition.sourceRefs(List(recapture.sourceId))
    assert(proof.nonEmpty && proof.forall(arms.head.proofs),
      "the exact available recapture's owned source is retained as evidence")
    assert(completed.core.flatMap(_.references).forall(ref => pair.exists(_ eq ref.path)),
      "the unplayed Qxb8 alternative must not become a move on either recorded path")
    assert(RootQuestionSummary.realize(paths, pair, completed).nonEmpty)

  test("opposite adjacent captures without selected actor support are not one response price"):
    val rows = List(List("h1g1", "a8b8", "f5c5"), List("h1g2", "a8a7"))
    val (packet, paths) = prepare("rN3r1k/8/8/2n2Q2/8/8/8/7K w - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val arms = RootQuestionSummary.prepare(packet, paths, pair, positions).get
    val gain = MaterialEvents(pair.head, arms.head.material.events.filter(_.plyOffset == 2))
    val original = Answer("selected_comparison", List(Section(List(Continuation(pair.map(MoveRef(_, 0))), gain))), Nil, Nil)
    val completed = MoveWorkProjection.completeSelectedPrices(arms, original, original.core.flatMap(_.statements)).get
    assertEquals(completed.core.flatMap(_.covered).map(_.offset), List(2))
    assertEquals(completed.core.flatMap(_.statements).collect { case value: Material => value }, Nil)

  test("geometric support with an illegal recapture cannot join a selected countercapture price"):
    val rows = List(List("d1d2", "a8b8", "d6d8"), List("d1e1", "a8a7"))
    val (packet, paths) = prepare("rN1r4/7k/3Q4/8/8/8/8/3K4 w - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val arms = RootQuestionSummary.prepare(packet, paths, pair, positions).get
    val queen = CommentaryFact.Piece("white", "queen", "d6").witness
    val knight = CommentaryFact.Piece("white", "knight", "b8").witness
    assertEquals(arms.head.control(1, queen, knight), Some(true))
    assert(!arms.head.position(2).get.view.legalRows.get.exists(_.move == "d6b8"),
      "the queen is pinned to its king although it geometrically supports b8")
    val gain = MaterialEvents(pair.head, arms.head.material.events.filter(_.plyOffset == 2))
    val original = Answer("selected_comparison", List(Section(List(Continuation(pair.map(MoveRef(_, 0))), gain))), Nil, Nil)
    val completed = MoveWorkProjection.completeSelectedPrices(arms, original, original.core.flatMap(_.statements)).get
    assertEquals(completed.core.flatMap(_.covered).map(_.offset), List(2))
    assertEquals(completed.core.flatMap(_.statements).collect { case value: Material => value }, Nil)

  test("selected contacts keep original pieces, quiet replies and material cost before wording"):
    val (packet, paths) = rookPair
    val (pair, selected) = select(packet, paths, List(capture, evade))
    val answer = selected.getOrElse(fail("both observed responses form a complete target comparison"))
    val content = responses(answer)
    assertEquals(content.map(_.originalAttacker).toSet, Set(attacker))
    assertEquals(content.map(_.originalTarget).toSet, Set(target))
    assertEquals(content.map(_.target).toSet, Set(target))
    assertEquals(content.map(_.path).toSet, pair.toSet)
    assertEquals(answer.required.toSet, pair.flatMap(path => List(MoveRef(path, 0), MoveRef(path, 1))).toSet)
    answer.core.foreach { section =>
      val response = section.statements.collectFirst { case value: TargetResponse => value }.get
      assertEquals(section.required.toSet, Set(MoveRef(response.path, 0), MoveRef(response.path, 1)))
      assertEquals(section.focus.map(_.move), List(MoveRef(response.path, 0)))
      section.focus.foreach { focus =>
        assertEquals(focus.note.edge.evidence.id, focus.move.path.ids(focus.move.offset))
        assert(focus.note.evidence.forall(answer.evidence.contains))
      }
    }
    val taken = content.find(_.path.moves == capture).get
    val escaped = content.find(_.path.moves == evade).get
    assertEquals(taken.escape, None)
    assertEquals(escaped.escape, Some(1))
    assert(escaped.survives && escaped.material.isEmpty)
    val cost = taken.material.get
    assertEquals(cost.covered, List(MoveRef(taken.path, 1)))
    assertEquals(cost.cost.events.map(_.moveUci), List("a8a2"))
    assertEquals(cost.cost.commonExchanges, Nil)
    assertEquals(cost.cost.whiteAdditionalLosses, List(EvidencePieceRole("rook") -> 1))
    assertEquals(cost.cost.blackAdditionalLosses, Nil)
    assertEquals(paths.pieceAfter(packet, taken.path, attacker, 2), Some(None))
    assertEquals(paths.pieceAfter(packet, escaped.path, target, 2),
      Some(Some(CommentaryFact.Piece("black", "rook", "b8").witness)))
    val removal = taken.attackerRemoval.get
    assertEquals(removal.events.map(_.originalVictim), List(Some(attacker)))
    assertEquals(removal.events.map(_.originalActor), List(Some(target)))
    assertEquals(removal.references, List(MoveRef(taken.path, 1)))
    assertEquals(taken.targetCapture, None)
    assertEquals(escaped.attackerRemoval, None)
    assertEquals(taken.targetEnd, Some(PiecePresence(MoveRef(taken.path, 1), target,
      CommentaryFact.Piece("black", "rook", "a2").witness)))
    assertEquals(escaped.targetEnd, Some(PiecePresence(MoveRef(escaped.path, 1), target,
      CommentaryFact.Piece("black", "rook", "b8").witness)))
    val realized = RootQuestionSummary.realize(paths, pair, answer).get
    List("Rxa2", "white rook originally on a1", "black rook originally on a8", "captures",
      "still present on a2", "Rb8", "out of the attack", "still present on b8").foreach(words =>
      assert(realized.text.contains(words), realized.text))
    val delivered = (realized.json \ "units").as[List[JsObject]]
    delivered.foreach { unit =>
      val enRefs = (unit \ "en").as[List[JsValue]].collect { case ref: JsObject => ref }
      val koRefs = (unit \ "ko").as[List[JsValue]].collect { case ref: JsObject => ref }
      assertEquals(koRefs, enRefs)
      if (unit \ "english_only").asOpt[Boolean].contains(true) then
        assertEquals(unit("ko"), unit("en"))
    }
    assert(answer.evidence.nonEmpty)
    val graph = packet.evidenceGraph
    val ownedPositions = pair.flatMap(_.positions).toSet
    // Internal relation IDs resolve at their owning position before delivery.
    // The complete answer uses the same graph-record contract as local notes.
    answer.evidence.foreach { id =>
      val record = graph.byId.getOrElse(id, fail(s"unowned source: $id"))
      assert(ownedPositions(record.ref.position), s"source belongs to another position: $id")
      assert(graph.proofEligible(record), id)
    }

  test("a selected response cannot borrow a longer sibling's ending or another packet's paths"):
    val (packet, paths) = prepare(rookFen, List(capture.take(1), evade, capture))
    assert(select(packet, paths, List(capture.take(1), evade))._2.isEmpty,
      "the short returned arm does not own the supplied Rxa2")
    assert(select(packet, paths, List(capture, evade))._2.nonEmpty)
    val (otherPacket, otherPaths) = prepare(rookFen, List(capture, evade))
    intercept[IllegalArgumentException] {
      select(otherPacket, paths, List(capture, evade))
    }
    val foreignPair = List(capture, evade).map(row => otherPaths.paths.find(_.moves == row).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    assert(RootQuestionSummary.selectedAnswer(packet, paths, foreignPair, positions, Nil).isEmpty)

  test("wording preserves selected proof and prices without requiring every proof move inline"):
    val (packet, paths) = rookPair
    val (pair, selected) = select(packet, paths, List(capture, evade))
    val answer = selected.get
    val realized = RootQuestionSummary.realize(paths, pair, answer).get
    def parts(value: JsValue): List[String | MoveRef] = value.as[List[JsValue]].map {
      case JsString(words) => words
      case ref: JsObject => answer.required.find(_.json == ref).getOrElse(fail(s"unselected visible reference: $ref"))
      case other => fail(s"unexpected realization part: $other")
    }
    val units = (realized.json \ "units").as[List[JsObject]].zip(answer.core).map { (unit, section) =>
      RootQuestionSummary.Unit((unit \ "role").as[String], parts(unit("en")), parts(unit("ko")),
        parts(unit("covered_moves")).collect { case move: MoveRef => move }, section.required, section.focus,
        englishOnly = (unit \ "english_only").asOpt[Boolean].contains(true),
        expressed = CommentaryEnglish.renderedSection(section).expressed)
    }
    assertEquals(units.flatMap(_.focus).map(_.move).toSet, pair.map(MoveRef(_, 0)).toSet)
    for language <- List("en", "ko") do
      val visible = units.flatMap(unit => if language == "en" then unit.en else unit.ko)
        .collect { case move: MoveRef => move }
      assert(visible.forall(ref => units.exists(_.proofReferences.contains(ref))))
      assert(answer.required.forall(ref => units.exists(_.contentReferences.contains(ref))))
      val text = units.map(unit => render(if language == "en" then unit.en else unit.ko, paths)).mkString(" ")
      List("Rxa2", "Rb8").foreach(move => assert(text.contains(move), text))
      if language == "en" || units.filter(_.covered.nonEmpty).forall(_.englishOnly) then
        assert(text.contains("White loses one rook"), text)
      else
        assert(text.contains("백은") && text.contains("룩 한 개") && text.contains("내줍니다"), text)
    def reword(parts: List[String | MoveRef]) = parts.map {
      case words: String => words.replace(" line,", " continuation,")
      case move: MoveRef => move
    }
    val reworded = units.map(unit => unit.copy(en = reword(unit.en),
      ko = if unit.englishOnly then reword(unit.ko) else unit.ko))
    val alternate = RootQuestionSummary.fromUnits(paths, pair, answer.question, reworded, answer.evidence).get
    assertNotEquals(alternate.text, realized.text)
    assertEquals(reworded.flatMap(_.refs), units.flatMap(_.refs))
    assertEquals(reworded.flatMap(_.covered), units.flatMap(_.covered))
    assertEquals(reworded.flatMap(_.required), units.flatMap(_.required))
    assertEquals(reworded.flatMap(_.focus), units.flatMap(_.focus))
    assertEquals(reworded.flatMap(_.expressed), units.flatMap(_.expressed))
    assertEquals(alternate.evidence.toSet, realized.evidence.toSet)
    for missing <- List("cost", "quiet reply", "capture coverage") do
      val incomplete = answer.copy(core = answer.core.map(section => section.copy(statements = section.statements.map {
        case value: TargetResponse if missing == "cost" && value.material.nonEmpty => value.copy(material = None)
        case value: TargetResponse if missing == "quiet reply" && value.escape.nonEmpty =>
          // The same occurrence now also owns the target's final presence.
          // Remove both uses to test an actual missing reply reference.
          value.copy(escape = None, targetEnd = None)
        case value: TargetResponse if missing == "capture coverage" => value.copy(material = value.material.map(material =>
          material.copy(cost = material.cost.copy(events = Nil))))
        case value => value
      })))
      assert(RootQuestionSummary.realize(paths, pair, incomplete).isEmpty, missing)

  test("one material interval includes every capture and distinguishes the pieces lost by each side"):
    val line = List("b2f6", "d7f6", "f1f6", "g7f6")
    val quiet = List("d3d1")
    val (packet, paths) = prepare("2r1r2k/p2n1qp1/1p3pRp/3P4/2P5/P2Q4/1B4PP/5RK1 w - - 5 31", List(line, quiet))
    val path = paths.paths.find(_.moves == line).get
    val ledger = path.certified.flatMap(_._2.material).get
    val offsets = List(0, 1, 2, 3)
    val cost = CommentaryMaterialAccount.calculate(ledger, offsets, offsets, line.size).get
    assertEquals(cost.commonExchanges, Nil)
    assertEquals(cost.whiteAdditionalLosses, List(EvidencePieceRole("bishop") -> 1, EvidencePieceRole("rook") -> 1))
    assertEquals(cost.blackAdditionalLosses, List(EvidencePieceRole("pawn") -> 1, EvidencePieceRole("knight") -> 1))
    assertEquals(Material(path, cost).covered, offsets.map(MoveRef(path, _)))
    List(List(0, 2, 3), List(0, 1, 1, 2, 3)).foreach(events =>
      assert(CommentaryMaterialAccount.calculate(ledger, events, offsets, line.size).isEmpty))
    assert(CommentaryMaterialAccount.calculate(ledger, offsets, offsets.dropRight(1), line.size).isEmpty,
      "the last recapturer's loss cannot be covered without its move reference")
    val pair = List(line, quiet).map(row => paths.paths.find(_.moves == row).get)
    val answer = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    assertEquals(answer.core.flatMap(_.statements).collect { case value: Material => value }, List(Material(path, cost)))
    assertEquals(answer.details.flatMap(_.statements).collect { case value: MaterialEvents => value.events }, List(ledger.events))
    assertEquals(answer.core.flatMap(_.covered).toSet, offsets.map(MoveRef(path, _)).toSet)
    assert(answer.core.flatMap(_.statements).exists {
      case NoMaterialChange(root, end) => root.path == pair(1) && end == root
      case _ => false
    })
    val realized = RootQuestionSummary.realize(paths, pair, answer).get
    assert(realized.text.contains("one bishop and one rook") &&
      realized.text.contains("one pawn and one knight"), realized.text)
    val basic = (realized.json \ "units").as[List[JsObject]]
    val proof = basic.flatMap(unit => (unit \ "proof_moves").as[List[JsObject]])
    assert(offsets.map(MoveRef(path, _).json).forall(proof.contains))
    val inline = basic.flatMap(unit => (unit \ "en").as[List[JsValue]].collect { case move: JsObject => move })
    assert(!inline.contains(MoveRef(path, 3).json),
      "a role-count price does not need to repeat the last recapture notation")
    val roots = pair.map(MoveRef(_, 0))
    val scoped = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison",
      List(Section(List(Continuation(roots)), roots))).get
    assertEquals(scoped.core.flatMap(_.covered).toSet, offsets.map(MoveRef(path, _)).toSet,
      "the first capture owns the whole actual immediate-recapture chain")
    assert(scoped.core.flatMap(_.statements).forall(!_.isInstanceOf[Material]),
      "a selected answer does not add a separate full-line total")
    assert(RootQuestionSummary.realize(paths, pair, scoped).nonEmpty)

  test("a final-position witness does not bring independent preceding trades into the selected answer"):
    // The already opened interleaved-trade master pair from RootQuestionAnswerTest.
    val lines = List(
      "g2g4 f7e5 h1g1 h8g8 c1c2 c8d7 c2c3 a8f8 h2h3 d7c8 c3d4 e5f7 d4c3",
      "h2h3 a7a5 e3e4 f5f4 c1c2 e6e5 b2b3 a8a7 c2c3 e7f6 c5d3 g5g4 e2g4 c8g4 h3g4 a7d7 d3f2 d7d1 h1d1 f6e7 a2a3 e7f6 d1h1 c6c5 f2d3 f6g5 d3c5 g5g4"
    ).map(_.split(" ").toList)
    val (packet, paths) = prepare("r1b4r/p3kn2/2p1p3/2N2pp1/2P4p/4P3/PP2B1PP/2KR3R w - - 0 20", lines)
    val pair = lines.map(row => paths.paths.find(_.moves == row).get)
    val roots = pair.map(MoveRef(_, 0))
    val end = MoveRef(pair.last, pair.last.moves.size - 1)
    val original = CommentaryFact.Piece("white", "king", "c1").witness
    val current = paths.pieceAfter(packet, pair.last, original, pair.last.moves.size).flatten.get
    val lead = Section(List(Continuation(roots), PiecePresence(end, original, current)), roots :+ end)
    val answer = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", List(lead)).get
    val material = pair.last.certified.flatMap(_._2.material).get
    val finalEvent = material.events.filter(_.plyOffset == end.offset)
    assertEquals(finalEvent.size, 1)
    assert(material.events.size > finalEvent.size)
    assertEquals(answer.core.flatMap(_.covered), Nil,
      "a position witness does not select the unrelated final capture")
    assertEquals(answer.core.flatMap(_.statements).collect { case value: MaterialEvents => value.events }, Nil)
    assertEquals(answer.details.flatMap(_.statements).collect { case value: MaterialEvents => value.events }, List(material.events))
    assert(answer.core.flatMap(_.statements).contains(PiecePresence(end, original, current)))
    assert(RootQuestionSummary.realize(paths, pair, answer).nonEmpty)

  test("proof-only root positions do not excuse an actual root capture or check reply"):
    val rows = List(List("a4a8", "h8h7"), List("a4d1", "a8a7"))
    val (packet, paths) = prepare("r6k/8/8/8/Q7/8/8/7K w - - 0 1", rows)
    val pair = rows.map(row => paths.paths.find(_.moves == row).get)
    val king = CommentaryFact.Piece("black", "king", "h8").witness
    val core = pair.map(path => Section(List(PiecePresence(MoveRef(path, 0), king, king))))
    val answer = Answer("selected_comparison", core, Nil, Nil)
    assert(RootQuestionSummary.realize(paths, pair, answer).isEmpty,
      "the compared Qa8+ still requires its captured rook and actual check reply")

  test("Action wording supplies a subject for relational clauses and a verb after castling"):
    val (packet, paths) = rookPair
    val path = paths.paths.find(_.moves == capture).get
    val attack = MoveWorkProjection.select(packet, paths).local.find(note => note.edge.evidence.id == path.ids.head &&
      (note.note.detail match
        case MoveWorkProjection.Detail.Attack(actor, victim, _) => actor.side.white && victim == target
        case _ => false)).get.note.detail
    val relation = CommentaryEnglish.statement(Action(MoveRef(path, 0), attack))
    assertEquals(relation.collect { case move: MoveRef => move }, List(MoveRef(path, 0)))
    assert(render(relation, paths).startsWith("After Ra2, White's rook on a2"))
    val (castlePacket, castlePaths) = prepare("4k3/8/8/8/8/8/8/4K2R w K - 0 1", List(List("e1g1")))
    val castlePath = castlePaths.paths.find(_.moves == List("e1g1")).get
    val castle = MoveWorkProjection.select(castlePacket, castlePaths).local.find(_.note.kind == "castling").get.note.detail
    assertEquals(render(CommentaryEnglish.statement(Action(MoveRef(castlePath, 0), castle)), castlePaths),
      "O-O castles, bringing the rook from h1 to f1.")

  test("wording groups one actual move's targets and repeated presence without changing the selected facts"):
    import CommentaryEnglish.*
    // The already opened countercapture master position from RootQuestionAnswerTest.
    val (packet, paths) = prepare("r4rk1/p1p3pp/1b6/2PpN3/4b1Pq/4B2P/PP3P1K/R2Q1R2 b - - 0 19",
      List(List("h4e7"), List("b6a5")))
    val path = paths.paths.find(_.moves == List("h4e7")).get
    val attacks = MoveWorkProjection.select(packet, paths).local.collect {
      case note if note.edge.evidence.id == path.ids.head && (note.note.detail match
        case MoveWorkProjection.Detail.Attack(actor, _, _) => actor.roleName == "queen" && actor.at == "e7"
        case _ => false) => Action(MoveRef(path, 0), note.note.detail)
    }.distinct
    assert(attacks.size >= 2, "the original observation contains separate actual queen targets")
    val selected = Section(attacks)
    val parts = CommentaryEnglish.section(selected)
    assertEquals(parts.collect { case ref: MoveRef => ref }, List(MoveRef(path, 0)))
    val words = render(parts, paths)
    attacks.foreach { action => action.detail match
      case MoveWorkProjection.Detail.Attack(_, target, _) =>
        assert(words.contains(target.possessiveName), words)
      case _ => fail("unexpected non-attack in the actual target group")
    }
    assertEquals(selected.statements, attacks)
    val (rookPacket, rookPaths) = rookPair
    val quiet = rookPaths.paths.find(_.moves == evade).get
    val current = rookPaths.pieceAfter(rookPacket, quiet, attacker, 1).flatten.get
    val presences = List(0, 1).map(offset => PiecePresence(MoveRef(quiet, offset), attacker, current))
    val grouped = CommentaryEnglish.section(Section(presences))
    assertEquals(grouped.collect { case ref: MoveRef => ref }.toSet, presences.map(_.after).toSet)
    assertEquals(render(grouped, rookPaths).sliding("is still present".length).count(_ == "is still present"), 1)

  test("every question keeps declared captures and immediate replies within each delivered layer"):
    val line = List("b2f6", "d7f6", "f1f6", "g7f6")
    val quiet = List("d3d1")
    val (_, paths) = prepare("2r1r2k/p2n1qp1/1p3pRp/3P4/2P5/P2Q4/1B4PP/5RK1 w - - 5 31", List(line, quiet))
    val pair = List(line, quiet).map(row => paths.paths.find(_.moves == row).get)
    val captures = line.indices.map(MoveRef(pair.head, _)).toList
    val reply = List(MoveRef(pair.last, 0))
    val cost = RootQuestionSummary.Unit("cost", captures, captures, captures)
    val peer = RootQuestionSummary.Unit("reply", reply, reply)
    val core = List(cost, peer)
    def deliver(units: List[RootQuestionSummary.Unit], details: List[RootQuestionSummary.Unit] = Nil) =
      RootQuestionSummary.fromUnits(paths, pair, "selected_comparison", units, Nil, detailUnits = details)
    assert(deliver(core, List(cost)).nonEmpty)
    assert(deliver(cost :: core, List(cost, cost)).nonEmpty,
      "one actual capture may support several meanings in the same layer")
    val uncovered = cost.copy(covered = captures.dropRight(1))
    assert(deliver(List(uncovered, peer)).isEmpty)
    assert(deliver(core, List(uncovered)).isEmpty,
      "core coverage cannot replace missing detail coverage")
    val missingReply = captures.filterNot(_.offset == 1)
    val interrupted = cost.copy(en = missingReply, ko = missingReply, covered = missingReply)
    assert(deliver(List(interrupted, peer)).isEmpty)
    assert(deliver(core, List(interrupted)).isEmpty,
      "an omitted immediate recapture cannot be borrowed from the other layer")
    val invisible = cost.copy(en = captures.dropRight(1), ko = captures.dropRight(1))
    assert(deliver(List(invisible, peer)).isEmpty)

  test("an actual checking reply is required unless the finite record explicitly ends there"):
    val fen = "2kr3r/ppp2p1p/2nn2p1/4p3/2N1P3/2P2BP1/PP3P1P/3RR1K1 w - - 3 16"
    val checked = List("f3g4", "c8b8")
    val finite = checked.take(1)
    val quiet = List("c4e3")
    val (packet, paths) = prepare(fen, List(checked, finite, quiet))
    val full = paths.paths.find(_.moves == checked).get
    val stopped = paths.paths.find(_.moves == finite).get
    val peer = paths.paths.find(_.moves == quiet).get
    assert(full.certified.exists(_._2.lineEvents.exists(event => event.kind == LineEventKind.Check && event.plyOffset == 0)))
    assert(full.certified.exists(_._2.lineEvents.exists(event => event.kind == LineEventKind.CheckEvasion && event.plyOffset == 1)))
    def unit(path: ObservedLineProtocol.PreparedPath, offsets: List[Int], ends: List[Int] = Nil) =
      val refs = offsets.map(MoveRef(path, _))
      RootQuestionSummary.Unit("reply", refs, refs, recordedEnds = ends.map(MoveRef(path, _)))
    val core = List(unit(full, List(0, 1)), unit(peer, List(0)))
    def deliver(units: List[RootQuestionSummary.Unit], details: List[RootQuestionSummary.Unit] = Nil) =
      RootQuestionSummary.fromUnits(paths, List(full, peer), "selected_comparison", units, Nil, detailUnits = details)
    assert(deliver(core).nonEmpty)
    assert(deliver(List(unit(full, List(0)), core.last)).isEmpty)
    assert(deliver(core, List(unit(full, List(0)))).isEmpty)
    assert(deliver(List(unit(full, List(0, 1), List(0)), core.last)).isEmpty,
      "a recorded end cannot hide an existing reply on the same path")
    val pair = List(stopped, peer)
    val bounded = List(unit(stopped, List(0), List(0)), unit(peer, List(0)))
    assert(RootQuestionSummary.fromUnits(paths, pair, "selected_comparison", bounded, Nil).nonEmpty)
    assert(RootQuestionSummary.fromUnits(paths, pair, "selected_comparison",
      bounded.map(_.copy(recordedEnds = Nil)), Nil).isEmpty)
    val answer = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    val delivered = RootQuestionSummary.realize(paths, pair, answer).get
    val ends = (delivered.json \ "units").as[List[JsObject]].flatMap(unit =>
      (unit \ "recorded_end_moves").asOpt[List[JsObject]].getOrElse(Nil))
    assert(ends.contains(MoveRef(stopped, 0).json))

  test("a played move's actual checkmate consequence precedes the alternative's finite material account"):
    val (packet, paths) = prepare("4r1k1/pp3ppp/2p5/8/8/2P5/PP3PPP/5RK1 w - - 0 1",
      List(List("f1e1", "e8e1"), List("f1a1", "h7h6")), List(-1))
    val root = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    val text = (root \ "detail" \ "summary").as[String]
    assert(text.startsWith("In the Re1 line, White loses one rook. After Rxe1#, Black wins by checkmate."), text)
    assertEquals(text.sliding("Black wins by checkmate".length).count(_ == "Black wins by checkmate"), 1)
    assert(text.contains("Ra1"), text)

  test("a quiet losing move retains the first checking reply and its capture before the final mate"):
    val fen = "5rk1/8/5q2/8/4P3/2N5/8/5RK1 w - - 0 1"
    val rows = List("f1e1 f6f2 g1h1 f2e1 h1h2 f8f2 h2h3 e1h1 h3g4 h1g1 g4h5 f2h2",
      "f1f6 f8f6").map(_.split(" ").toList)
    val (packet, paths) = prepare(fen, rows, List(-6))
    val root = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    val text = (root \ "detail" \ "summary").as[String]
    List("Re1", "f2", "Qf2+", "Kh1", "Qxe1+", "Kh2", "Rh2#", "Rxf6").foreach(move =>
      assert(text.contains(move), text))
    assert(text.indexOf("Qf2+") < text.indexOf("Black wins by checkmate"), text)
    assert(text.contains("does not control f2"), text)
    assert(!text.contains("forced") && !text.contains("only legal"), text)
    val units = (root \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
    val visible = units.flatMap(unit => (unit \ "en").as[List[JsValue]].collect { case ref: JsObject => ref }).toSet
    val actual = paths.paths.find(_.moves == rows.head).get
    assert((1 to 4).forall(offset => visible(MoveRef(actual, offset).json)))

  test("the actual terminal occurrence remains distinct from a finite observed prefix"):
    val line = "g7g8q h4h3 f6f5 h3h2 f5e4 h2h3 e4f3 h3h2 g8g2".split(" ").toList
    val later = "f6f5 h4g3 f5e4 g3g2 e4e3 g2g3 g7g8q g3h3 e3e4 h3h2 e4f4 h2h1 f4f3 h1h2 g8g2".split(" ").toList
    // The existing ReferenceMeaningDelivery case reports mate 5 / mate 8.
    // A cp-only report ending in mate is rejected as an assessment input;
    // shortening the second observed record does not create its terminal.
    val (packet, paths) = prepare("8/6P1/5K2/8/7k/8/8/8 w - - 0 51", List(line, later.dropRight(1)), List(5, 8))
    val finished = paths.paths.find(_.moves == line).get
    val prefix = paths.paths.find(_.moves == later.dropRight(1)).get
    assertEquals(finished.automaticTerminal, Some(AutomaticTerminal.Checkmate(chess.White)))
    assertEquals(prefix.automaticTerminal, None)
    val pair = List(finished, prefix)
    assert(packet.primary.nonEmpty, "consistent reported evaluations must admit the existing root comparison")
    val comparisons = CommentarySelection.primaryComparisonRecords(packet)
    assertEquals(comparisons.size, 1)
    assertEquals(comparisons.flatMap((_, fact, _) => List(fact.referenceLine, fact.candidateLine)).toSet,
      pair.map(_.line.ref).toSet)
    val answer = RootQuestionSummary.completePairAnswer(packet, paths, pair, "observed_terminal_comparison", Nil).get
    val endings = answer.core.flatMap(_.statements).collect { case value: Terminal => value }
    assertEquals(endings, List(Terminal(MoveRef(finished, 8), AutomaticTerminal.Checkmate(chess.White))))
    val promotions = answer.core.flatMap(_.statements).collect { case value: MaterialEvents => value }
    assertEquals(promotions.map(value => value.path -> value.events.map(_.plyOffset)).toMap,
      Map(finished -> List(0), prefix -> List(6)))
    assert(promotions.forall(_.events.forall(event => event.movement.afterRole == EvidencePieceRole("queen") &&
      event.promotionGainCp > 0 && event.capture.isEmpty)))
    assert(Set(MoveRef(finished, 0), MoveRef(finished, 8), MoveRef(prefix, 0), MoveRef(prefix, 6))
      .subsetOf(answer.required.toSet))
    val realized = RootQuestionSummary.realize(paths, pair, answer).get
    assert(realized.text.contains("g8=Q promotes to a queen") && realized.text.contains("Qg2#") &&
      realized.text.contains("White wins by checkmate"), realized.text)
    val completedAgain = RootQuestionSummary.completePairAnswer(packet, paths, pair,
      answer.question, answer.core).get
    assertEquals(completedAgain.core.flatMap(_.statements).collect { case value: Terminal => value }, endings)
    assert(answer.required.forall(completedAgain.required.contains))
    val again = RootQuestionSummary.realize(paths, pair, completedAgain).get.text
    assertEquals(again.sliding("White wins by checkmate".length).count(_ == "White wins by checkmate"), 1)
    val root = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    assertEquals((root \ "detail" \ "summary_kind").as[String], "question_answer")
    assertEquals((root \ "detail" \ "summary_question" \ "question").as[String], "observed_terminal_comparison")
    val displayed = (root \ "detail" \ "summary").as[String]
    List("g8=Q", "Qg2#", "White wins by checkmate").foreach(required => assert(displayed.contains(required), displayed))
    assertEquals(displayed.sliding("White wins by checkmate".length).count(_ == "White wins by checkmate"), 1)
    val visible = (root \ "detail" \ "summary_question" \ "units").as[List[JsObject]].flatMap(unit =>
      (unit \ "en").as[List[JsValue]].collect { case ref: JsObject => ref }).toSet
    assert(answer.required.map(_.json).toSet.subsetOf(visible))
    val promotion = finished.certified.flatMap(_._2.material).get
    assert(CommentaryMaterialAccount.calculate(promotion, List(0), List(0), line.size).isEmpty,
      "promotion gain cannot be mislabeled as an ordinary capture exchange")
