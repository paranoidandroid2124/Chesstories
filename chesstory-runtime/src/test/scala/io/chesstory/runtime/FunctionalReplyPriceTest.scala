package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.*

/** SG14's fixed book board and the saved Qb3/Bc5 root paths provide legal
  * inputs. Scores/receipts here are synthetic and confer no engine-strength
  * authority. The question is an already legal action's specific later price.
  */
class FunctionalReplyPriceTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import CommentaryAnswer.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import PrincipalVariationComparison.QualityBand

  private def moves(value: String): List[String] = value.split(" ").toList
  private val fen = "r2qk2r/pp1b1pbp/2n1p1p1/1B6/Q2P4/B1P2N2/P4PPP/R3K2R w KQkq - 7 14"
  private val current = List(
    moves("a4b3 c6e7 f3e5 g7e5 d4e5 d7b5 b3b5 d8d7 b5b4 e8c8 e1g1 e7f5 a1b1 b7b6 c3c4"),
    moves("a3c5 a7a6 b5c6 d7c6 a4b4 a6a5 b4b2 g7f8 c5b6 d8c8 e1g1 c6f3 g2f3 f8d6 b2e2 a8a6"))
  private val reference = moves("a4b3 a7a6 b5e2 c6d4 f3d4")
  private val alternative = moves("a3c5 a7a6 b5e2 c6d4 f3d4")
  private val price = alternative :+ "d7a4"
  private val queen = Piece("white", "queen", "a4")
  private val knight = Piece("white", "knight", "f3")
  private val capturedKnight = Piece("black", "knight", "c6")
  private val priceBishop = Piece("black", "bishop", "d7")
  private val otherBishop = Piece("black", "bishop", "g7")
  private val condition = Predicate("location", Some(queen))
  private val action = Predicate("legal_capture", Some(knight), Some(capturedKnight))
  private val effect = Predicate("legal_capture", Some(priceBishop), Some(queen))

  private case class Boards(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath]):
    def path(prefix: List[String]): PreparedPath = paths.paths.filter(_.moves.startsWith(prefix))
      .minByOption(_.moves.size).getOrElse(fail(s"missing owned price path $prefix"))

  private case class Fixture(boards: Boards, entry: Entry, proof: FunctionalReplyReading.Proof,
      previous: Answer):
    def statement: LegalReplyOption = LegalReplyOption(boards.path(proof.reference),
      boards.path(proof.alternative), boards.path(proof.option), proof, proof.price.map(boards.path))
    def answer: Answer = FunctionalReplyReading.withAnswer(boards.packet, boards.paths,
      boards.pair, List(proof), previous)

  private def boards(rootFen: String, rootLines: List[List[String]], local: List[List[String]],
      playedMove: Option[String] = None): Boards =
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(rootFen, playedMove.getOrElse(rootLines.head.head),
      rootLines.map(line => EngineLine(line, 0, None, 16)))).get
    val retained = MoveReviewInputAdmission.retainLegalContinuations(input,
      local.distinct.filterNot(rootLines.contains).map { line =>
        val continuation = input.positionHistory.admitToFirstAutomaticTerminal(line).toOption
          .getOrElse(fail(s"illegal price fixture $line"))
        assertEquals(continuation.stepsAfter(input.positionHistory).get.map(_.uci), line,
          "a legally admitted prefix does not certify a missing or illegal suffix")
        continuation
      })
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(retained)).get
    val paths = ObservedLineProtocol.prepare(packet)
    Boards(packet, paths, rootLines.map(line => paths.certifiedPath(packet, line).get))

  private def receipt(value: Boards): CommentaryInvestigation.Answer =
    CommentaryInvestigation.Answer("synthetic-price-root", "synthetic-price-root-key",
      CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), "equal",
      value.pair.head.line.ref.id, value.pair(1).line.ref.id, Json.obj(), Json.obj(),
      value.pair.head.ids.head, value.pair(1).ids.head, Some(QualityBand.Close))

  private def fixture(rootFen: String = fen, rootLines: List[List[String]] = current,
      stored: List[List[String]] = Nil): Fixture =
    val local = if stored.nonEmpty then stored else List(reference, alternative, price, reference :+ "d7a4")
    val value = boards(rootFen, rootLines, local)
    val batch = CommentaryDiscovery.discover(value.packet, value.paths,
      List(CommentaryDiscovery.Anchor(Nil, rootLines.head.head)),
      rootLines.map(line => CommentaryDiscovery.SourcePath(line, Source("user", "synthetic-price-current"))),
      Nil, pvGuided = true)
    val generated = batch.additions.find(entry =>
      entry.request.origin.question.producer == "C-reply-function" && entry.request.requirements.exists {
        case fact: Fact => fact.id == "function_effect_reference" && fact.path.source == Declared(reference)
        case _ => false
      } && entry.request.requirements.exists {
        case fact: Fact => fact.id == "function_price" && fact.path.source == Declared(price)
        case _ => false
      }).getOrElse(fail("missing generated both-legal option with its exact alternative queen price"))
    val entry = CommentaryFactReader.read(value.packet, value.paths, List(generated.copy(
      readings = generated.readings.map {
        case reading if reading.requirementId == "function_root" =>
          reading.copy(state = "answered", answer = Some(receipt(value)))
        case reading => reading
      }))).head
    val proof = FunctionalReplyReading.read(List(entry)).find(proof =>
      proof.reference == reference && proof.alternative == alternative && proof.price.contains(price))
      .getOrElse(fail("the complete price contract must retain its explicit both-legal mode"))
    val previous = RootQuestionSummary.completePairAnswer(value.packet, value.paths, value.pair,
      "selected_comparison", Nil).get
    Fixture(value, entry, proof, previous)

  private def english(value: Fixture): String = CommentaryEnglish.statement(value.statement).map {
    case text: String => text
    case move: RootQuestionSummary.MoveRef => value.boards.paths.notation.line(move.path.moves)(move.offset)
  }.mkString

  private def exactOwnCapture(value: Fixture, path: PreparedPath): Boolean =
    val actor = value.boards.paths.trackedPieceAfter(path, knight.witness, 4).flatten
    val target = value.boards.paths.trackedPieceAfter(path, capturedKnight.witness, 4).flatten
    value.boards.paths.positionView(path, 4).flatMap(_.legalRows).exists(_.exists(row =>
      row.move == "f3d4" && actor.contains(row.actor) && row.capture == target))

  test("the same original knight capture is legal in both arms while only one leaves the original queen price"):
    val value = fixture()
    assertEquals(value.proof.condition, condition)
    assertEquals(value.proof.action, action)
    assertEquals(value.proof.effect, effect)
    assertEquals(value.proof.option, reference)
    assertEquals(value.proof.price, Some(price))
    assertEquals(value.entry.readings.find(_.requirementId == "function_context").get.state, "supported")
    assert(exactOwnCapture(value, value.statement.reference))
    assert(exactOwnCapture(value, value.statement.alternative))
    assert(FunctionalReplyReading.valid(value.boards.paths, value.statement))
    assertEquals(value.statement.priceMove, Some(RootQuestionSummary.MoveRef(value.boards.path(price), 5)))
    assertEquals(value.statement.actionReferences, Nil)
    assertEquals(value.statement.covered, Nil,
      "a conditional price witness does not own the actual selected root's material ledger")

  test("the basic answer names the conditional queen price without claiming a forced or free gain"):
    val value = fixture()
    assertEquals(value.answer.question, "functional_response")
    val delivered = RootQuestionSummary.realize(value.boards.paths, value.boards.pair, value.answer).get
    val lead = english(value)
    assert(lead.contains("Qb3") && lead.contains("Bc5") && lead.contains("Nxd4") && lead.contains("Bxa4"), lead)
    assert(lead.toLowerCase.contains("queen"), lead)
    List("forced", "must", "best", "winning", "free", "safe", "evaluates", "blunder").foreach { word =>
      assert(!lead.toLowerCase.contains(word), lead)
    }
    assert(delivered.text.contains(lead), delivered.text)
    val recordedCaptures = value.previous.core.flatMap(_.statements).flatMap(_.covered).toSet
    assert(recordedCaptures.subsetOf((value.answer.core ++ value.answer.details)
      .flatMap(_.statements).flatMap(_.covered).toSet),
      "all actual captures remain available; an unrelated later exchange need not own the conditional lead")
    val currentKnight = value.boards.paths.trackedPieceAfter(value.statement.reference, knight.witness, 5).flatten.get
    val originalOtherBishop = value.boards.paths.trackedPieceAfter(value.statement.reference, otherBishop.witness, 5).flatten.get
    assert(value.boards.paths.positionView(value.statement.reference, 5).flatMap(_.legalRows).exists(_.exists(row =>
      row.move == "g7d4" && row.actor == originalOtherBishop && row.capture.contains(currentKnight))),
      "Bg7xd4 still takes the original knight used for the recapture; this is no free knight")

  test("both-legal actions require the explicit price mode and cannot weaken the old legality-difference contract"):
    val value = fixture()
    val legacy = value.statement.copy(proof = value.proof.copy(price = None), price = None)
    assert(!FunctionalReplyReading.valid(value.boards.paths, legacy))
    assertEquals(FunctionalReplyReading.withAnswer(value.boards.packet, value.boards.paths,
      value.boards.pair, List(legacy.proof), value.previous), value.previous)

  test("same enemy UCI can remain legal without capturing the named original queen"):
    val value = fixture()
    val rows = List(value.statement.reference, value.statement.alternative).map(path =>
      value.boards.paths.positionView(path, 5).flatMap(_.legalRows).get.find(_.move == "d7a4").get)
    assert(rows.head.capture.isEmpty, "after Qb3 the move is Ba4, not a capture of the queen")
    val queenAtA4 = value.boards.paths.trackedPieceAfter(value.statement.alternative, queen.witness, 5).flatten.get
    assert(rows(1).capture.contains(queenAtA4), "after Bc5 the same UCI is Bxa4 of original Qa4")
    val quietPrice = value.statement.copy(price = Some(value.boards.path(reference :+ "d7a4")))
    assert(!FunctionalReplyReading.valid(value.boards.paths, quietPrice))

  test("another reference capture of the protected queen defeats central usability while the specific fact stays true"):
    val rootFen = "r2qk2r/pp1b1pbp/2n1p1p1/1B6/Q2P4/B1P2N2/Pr3PPP/R3K2R w KQkq - 7 14"
    val value = boards(rootFen, current, List(reference, alternative, price))
    val root = CompareMoves("function_root", Nil, "a4b3", "a3c5")
    val proof = FunctionalReplyReading.Proof(CommentaryIdea.AnsweredComparison("extra-queen-capture-counter", root,
      receipt(value)), condition, reference, alternative, reference, effect, action, Nil, Some(price))
    val statement = LegalReplyOption(value.path(reference), value.path(alternative), value.path(reference), proof,
      Some(value.path(price)))
    assert(FunctionalReplyReading.valid(value.paths, statement),
      "the Bd7-specific difference is still factually correct")
    assert(value.paths.positionView(statement.reference, 5).flatMap(_.legalRows)
      .exists(_.exists(_.move == "b2b3")), "Rb2xb3 can still capture the protected original queen")
    val previous = RootQuestionSummary.completePairAnswer(value.packet, value.paths, value.pair,
      "selected_comparison", Nil).get
    assertEquals(FunctionalReplyReading.withAnswer(value.packet, value.paths, value.pair, List(proof), previous), previous,
      "avoiding one bishop cannot establish a usable queen-safe recapture when another capture remains")

  test("equal immediate prices after Qb3 and Qc2 do not become a usefulness proof from different locations"):
    val peer = "a4c2" :: alternative.tail
    val peerQuiet = peer :+ "d7a4"
    val rootLines = List(moves("a4b3 c6e7"), moves("a4c2 a7a6"))
    val value = boards(fen, rootLines, List(reference, peer, peerQuiet))
    val root = CompareMoves("function_root", Nil, "a4b3", "a4c2")
    val proof = FunctionalReplyReading.Proof(CommentaryIdea.AnsweredComparison("equal-price-counter", root,
      receipt(value)), condition, reference, peer, reference, effect, action, Nil, Some(peerQuiet))
    val statement = LegalReplyOption(value.path(reference), value.path(peer), value.path(reference), proof,
      Some(value.path(peerQuiet)))
    assert(!FunctionalReplyReading.valid(value.paths, statement),
      "both roots avoid the specific Bd7 capture; later Qa5/loose-bishop tactics need their own proof")

  test("missing or illegal sixth moves cannot borrow a legal prefix as the claimed realized price"):
    val value = fixture()
    val missing = value.statement.copy(price = None)
    val tooShort = value.statement.copy(price = Some(value.boards.path(alternative)))
    val illegal = value.statement.copy(proof = value.proof.copy(price = Some(alternative :+ "d7b3")))
    List(missing, tooShort, illegal).foreach(statement =>
      assert(!FunctionalReplyReading.valid(value.boards.paths, statement)))
    assertEquals(RootQuestionSummary.realize(value.boards.paths, value.boards.pair,
      value.answer.copy(core = List(Section(List(missing))))), None)

  test("a different legitimate capture cannot realize the named queen price"):
    val otherPrice = alternative :+ "g7d4"
    val value = fixture(stored = List(reference, alternative, price, otherPrice))
    val wrong = value.statement.copy(proof = value.proof.copy(price = Some(otherPrice)),
      price = Some(value.boards.path(otherPrice)))
    assert(!FunctionalReplyReading.valid(value.boards.paths, wrong),
      "Bg7xd4 captures original Nf3, not original Qa4, and cannot fulfill Bd7's price")
    val wrongTarget = value.statement.copy(proof = value.proof.copy(
      effect = effect.copy(target = Some(Piece("white", "queen", "d1")))))
    val wrongAction = value.statement.copy(proof = value.proof.copy(
      action = action.copy(target = Some(otherBishop))))
    assert(!FunctionalReplyReading.valid(value.boards.paths, wrongTarget))
    assert(!FunctionalReplyReading.valid(value.boards.paths, wrongAction))

  test("foreign price paths and mismatched root receipts cannot import an equal-looking realization"):
    val value = fixture()
    val foreign = fixture()
    assert(!FunctionalReplyReading.valid(value.boards.paths,
      value.statement.copy(price = foreign.statement.price)))
    val wrongReceipt = value.proof.copy(owner = value.proof.owner.copy(answer = Some(
      value.proof.owner.answer.get.copy(referenceOccurrenceId = value.boards.pair(1).ids.head))))
    assert(!FunctionalReplyReading.valid(value.boards.paths, value.statement.copy(proof = wrongReceipt)))
    val wrongCarrier = value.proof.copy(owner = value.proof.owner.copy(answer = Some(
      value.proof.owner.answer.get.copy(referenceLineId = value.boards.pair(1).line.ref.id))))
    assertEquals(FunctionalReplyReading.withAnswer(value.boards.packet, value.boards.paths,
      value.boards.pair, List(wrongCarrier), value.previous), value.previous)

  test("declared prices cannot keep a current root purpose after its live condition is lost or unknown"):
    val value = fixture()
    def changed(state: String) = value.entry.copy(readings = value.entry.readings.map {
      case reading if reading.requirementId == "function_live" || reading.requirementId == "function_context" =>
        reading.copy(state = state, result = None)
      case reading => reading
    })
    assertEquals(value.entry.readings.find(_.requirementId == "function_price").get.state, "supported")
    assertEquals(FunctionalReplyReading.read(List(changed("refuted"))), Nil)
    assertEquals(FunctionalReplyReading.read(List(changed("partial"))), Nil)

  test("current PV changes and longer certified price suffixes preserve only the bounded conditional fact"):
    val rootLines = List(moves("a4b3 a7a6 b5e2 b7b5"), moves("a3c5 a7a6 b5c6 d7c6"))
    val left = reference ++ moves("d7a4 d4c6")
    val right = price :+ "d4e6"
    val value = fixture(rootLines = rootLines, stored = List(left, right))
    assert(!value.boards.paths.paths.exists(_.moves == price))
    assert(FunctionalReplyReading.valid(value.boards.paths, value.statement))
    assertEquals(value.statement.referenceSequence.size, 5)
    assertEquals(value.statement.alternativeSequence.size, 5)
    assertEquals(value.statement.priceMove.map(_.offset), Some(5))
    val text = english(value)
    assert(!text.contains("Nc6") && !text.contains("Nxe6"), text)
    assertEquals(value.answer.question, "functional_response")

  test("same UCI, same current queen role and square do not identify the same original price victim"):
    val rootFen = "6k1/3b4/8/8/p7/8/8/Q2Q2K1 w - - 0 1"
    val rootLines = List(moves("d1a4 d7a4"), moves("a1a4 d7a4"))
    val value = boards(rootFen, rootLines, Nil)
    val originalQueen = Piece("white", "queen", "d1")
    val predicate = Predicate("legal_capture", Some(priceBishop), Some(originalQueen))
    val origin = Origin(Source("user", "price-identity-question"), Source("user", "price-identity-path"),
      Source("none", "price-identity-priority"), Source("none", "price-identity-evidence"))
    val requirements = List[Requirement](
      Fact("reference_capture", Path("reference", Declared(rootLines.head)), predicate, Ply(1), expected = Some("legal")),
      Fact("other_capture", Path("alternative", Declared(rootLines(1))), predicate, Ply(1), expected = Some("illegal")),
      FactComparison("physical_target_difference", "reference_capture", "other_capture", "different"),
      Fact("reference_original_removed", Path("reference", Declared(rootLines.head)),
        Predicate("presence", Some(originalQueen)), Ply(2), expected = Some("removed")),
      Fact("other_original_retained", Path("alternative", Declared(rootLines(1))),
        Predicate("presence", Some(originalQueen)), Ply(2), expected = Some("present")))
    val readings = CommentaryFactReader.read(value.packet, value.paths,
      CommentaryProposal.entries(List(Request("physical-price-identity", origin, requirements)))).head.readings
    assert(readings.forall(_.state == "supported"), readings.toString)
    val rows = value.pair.map(path => value.paths.positionView(path, 1).flatMap(_.legalRows).get
      .find(_.move == "d7a4").get)
    assertEquals(rows.head.capture, rows(1).capture,
      "both rows currently capture a white queen on a4; this current witness alone is ambiguous")
    assertEquals(value.paths.trackedPieceAfter(value.pair.head, originalQueen.witness, 2), Some(None))
    assert(value.paths.trackedPieceAfter(value.pair(1), originalQueen.witness, 2).flatten
      .exists(_.square.key == "d1"))

  test("the actual first capture, recapture and used knight's following loss remain in the basic answer"):
    val known = fixture()
    val rootLines = List(current.head, moves("a3c5 c6d4 f3d4 g7d4"))
    val observed = boards(fen, rootLines, List(reference, alternative, price))
    // The earlier question keeps its independently replayed ...a6 variation.
    // Refresh its current root receipt instead of requiring that quiet first
    // challenge to be rediscovered from the changed ...Nxd4 main continuation.
    val entry = CommentaryFactReader.read(observed.packet, observed.paths, List(known.entry.copy(
      readings = known.entry.readings.map {
        case reading if reading.requirementId == "function_root" =>
          reading.copy(state = "answered", answer = Some(receipt(observed)))
        case reading => reading
      }))).head
    val proof = FunctionalReplyReading.read(List(entry)).find(_.price.contains(price)).get
    val previous = RootQuestionSummary.completePairAnswer(observed.packet, observed.paths, observed.pair,
      "selected_comparison", Nil).get
    val value = Fixture(observed, entry, proof, previous)
    assertEquals(value.answer.question, "functional_response")
    val actual = value.boards.pair(1)
    val captured = actual.certified.flatMap(_._2.material).get.events.filter(_.capture.nonEmpty)
    assertEquals(captured.map(_.plyOffset), List(1, 2, 3))
    val basic = value.answer.core.flatMap(_.covered).toSet
    captured.foreach(event => assert(basic(RootQuestionSummary.MoveRef(actual, event.plyOffset)),
      s"actual ...Nxd4 Nxd4 Bxd4 event ${event.plyOffset} is a required price, not detail-only history"))
    assertEquals(value.boards.paths.trackedPieceAfter(actual, knight.witness, 4), Some(None),
      "the same original Nf3 used for the actual recapture is then lost to Bg7")
    assert(RootQuestionSummary.realize(value.boards.paths, value.boards.pair, value.answer).nonEmpty)

  test("a matching primary root condition admits only the named off-pair price example while an unrelated condition rejects it"):
    val known = fixture()
    def reread(rootLines: List[List[String]], playedMove: Option[String] = None): Fixture =
      val observed = boards(fen, rootLines, List(reference, alternative, price), playedMove)
      val entry = CommentaryFactReader.read(observed.packet, observed.paths, List(known.entry.copy(
        readings = known.entry.readings.map {
          case reading if reading.requirementId == "function_root" =>
            reading.copy(state = "deferred", stopReason = Some("comparison_not_current"), answer = None, result = None)
          case reading => reading
        }))).head
      val proof = FunctionalReplyReading.read(List(entry)).find(_.price.contains(price)).get
      val previous = RootQuestionSummary.completePairAnswer(observed.packet, observed.paths, observed.pair,
        "selected_comparison", Nil).get
      Fixture(observed, entry, proof, previous)

    val sameCondition = reread(List(moves("a4b3 a7a6 b5e2 b7b5"), moves("a1b1 a7a6 b5c6 d7c6")))
    assertEquals(sameCondition.proof.owner.answer, None)
    assertEquals(sameCondition.proof.owner.comparison.alternativeMove, "a3c5")
    assertEquals(sameCondition.boards.pair(1).moves.head, "a1b1")
    assert(FunctionalReplyReading.valid(sameCondition.boards.paths, sameCondition.statement))
    assertEquals(sameCondition.answer.question, "functional_response")
    val lead = english(sameCondition)
    assert(lead.contains("Qb3") && lead.contains("Bc5") && lead.contains("Bxa4"), lead)
    assert(!lead.contains("Rb1") && !lead.toLowerCase.contains("evaluates"),
      "the Bc5 witness cannot be relabelled as the new Rb1 numerical comparison's cause")
    assert(RootQuestionSummary.realize(sameCondition.boards.paths, sameCondition.boards.pair,
      sameCondition.answer).exists(_.text.contains(lead)))
    val actualCaptures = sameCondition.previous.core.flatMap(_.statements).flatMap(_.covered).toSet
    assert(actualCaptures.subsetOf((sameCondition.answer.core ++ sameCondition.answer.details)
      .flatMap(_.statements).flatMap(_.covered).toSet))

    val unrelated = reread(List(moves("a4b3 a7a6 b5e2 b7b5"), moves("a4b4 a7a6 b5e2")))
    assert(FunctionalReplyReading.valid(unrelated.boards.paths, unrelated.statement),
      "the old Bc5 conditional fact is still true in this canonical board")
    assertEquals(unrelated.answer, unrelated.previous,
      "Qb4 also changes the original queen's location, so a different primary condition cannot borrow Bc5's price")

    val foreignPlayed = reread(sameCondition.boards.pair.map(_.moves), playedMove = Some("a1b1"))
    assertEquals(foreignPlayed.boards.packet.assembly.input.playedMoveUci, "a1b1")
    assert(FunctionalReplyReading.valid(foreignPlayed.boards.paths, foreignPlayed.statement),
      "the Qb3/Bc5 conditional fact remains true but owns neither the actual Rb1 choice nor its numerical judgement")
    assertEquals(foreignPlayed.answer, foreignPlayed.previous,
      "a matching queen-location vector cannot import a question whose two named roots exclude the actual played move")
