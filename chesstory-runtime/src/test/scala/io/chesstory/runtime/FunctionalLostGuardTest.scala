package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.*

/** Exposed book-derived development controls. Full SG41 history is retained;
  * all scores and receipts are synthetic admission input, never book/engine
  * values or proof of utility, compensation, best defense or root causality.
  */
class FunctionalLostGuardTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import CommentaryAnswer.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import PrincipalVariationComparison.QualityBand

  private def moves(value: String): List[String] = value.split(" ").toList
  private val sg41Fen = "3k3r/3bbp2/p2ppp2/1pr2P1p/3RP2P/P1N2B2/1PP3P1/1K1R4 b - - 2 20"
  private val history = moves("e2e4 c7c5 g1f3 b8c6 d2d4 c5d4 f3d4 g8f6 b1c3 d7d6 c1g5 e7e6 f2f4 f8e7 d1d2 a7a6 e1c1 c8d7 f1e2 b7b5 e2f3 a8c8 c1b1 d8b6 h1e1 h7h6 g5f6 g7f6 f4f5 b6d4 d2d4 c6d4 d1d4 h6h5 h2h4 c8c5 a2a3 e8d8 e1d1")
  private val current = List(moves("e6f5 a3a4"), moves("a6a5 c3e2"))
  private val reference = moves("e6f5 c3d5")
  private val alternative = moves("a6a5 c3d5")
  private val price = alternative :+ "e6d5"
  private val guard = Piece("black", "pawn", "e6")
  private val knight = Piece("white", "knight", "c3")
  private val rook = Piece("black", "rook", "c5")
  private val condition = Predicate("location", Some(guard))
  private val action = Predicate("legal_move", Some(knight), square = Some("d5"))
  private val effect = Predicate("legal_capture", Some(guard), Some(knight))
  private val local = List(reference, alternative, price, reference :+ "c5d5", alternative :+ "c5d5")

  private case class Boards(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], currentLines: List[List[String]]):
    def path(prefix: List[String]): PreparedPath = paths.paths.filter(_.moves.startsWith(prefix))
      .minByOption(_.moves.size).getOrElse(fail(s"missing owned legal witness $prefix"))
    def previous: Answer = RootQuestionSummary.completePairAnswer(packet, paths, pair,
      "selected_comparison", Nil).get

  private def boards(fen: String = sg41Fen, prefix: List[String] = history,
      lines: List[List[String]] = current, retained: List[List[String]] = local): Boards =
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen, lines.head.head,
      lines.map(line => EngineLine(line, 0, None, 16)), movePrefixUci = prefix)).get
    val continuations = retained.distinct.filterNot(lines.contains).map { line =>
      val continuation = input.positionHistory.admitToFirstAutomaticTerminal(line).toOption
        .getOrElse(fail(s"illegal fixture path $line"))
      assertEquals(continuation.stepsAfter(input.positionHistory).get.map(_.uci), line,
        "a legal prefix does not certify an illegal or omitted suffix")
      continuation
    }
    val admitted = MoveReviewInputAdmission.retainLegalContinuations(input, continuations)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(admitted)).get
    val paths = ObservedLineProtocol.prepare(packet)
    Boards(packet, paths, lines.map(line => paths.certifiedPath(packet, line).get), lines)

  private def discover(value: Boards): CommentaryDiscovery.Batch = CommentaryDiscovery.discover(
    value.packet, value.paths, List(CommentaryDiscovery.Anchor(Nil, value.currentLines.head.head)),
    value.currentLines.map(line => CommentaryDiscovery.SourcePath(line,
      Source("user", "synthetic-lost-guard-current"))), Nil, pvGuided = true)

  private def exactEntry(value: Boards): Entry = discover(value).additions.find(entry =>
    entry.request.origin.question.producer == "C-reply-function" && entry.request.requirements.exists {
      case fact: Fact => fact.id == "function_effect_reference" && fact.predicate == effect &&
        fact.path.source == Declared(reference)
      case _ => false
    } && entry.request.requirements.exists {
      case fact: Fact => fact.id == "function_price" && fact.path.source == Declared(price)
      case _ => false
    }).getOrElse(fail("missing automatically discovered original-guard capture difference"))

  private def proof(value: Boards): FunctionalReplyReading.Proof = FunctionalReplyReading.read(
    CommentaryFactReader.read(value.packet, value.paths, List(exactEntry(value)))).find(proof =>
    proof.reference == reference && proof.alternative == alternative && proof.option == reference &&
      proof.condition == condition && proof.action == action && proof.effect == effect &&
      proof.price.contains(price)).getOrElse(fail("the independently replayed facts must form a guard-as-capturer proof"))

  private def statement(value: Boards, proof: FunctionalReplyReading.Proof): LegalReplyOption =
    FunctionalReplyReading.withRemainingCaptures(value.paths,
      LegalReplyOption(value.path(proof.reference), value.path(proof.alternative),
        value.path(proof.option), proof, proof.price.map(value.path)))

  private def allCapturesOf(value: Boards, path: PreparedPath, at: Int, target: Piece): Set[String] =
    val original = value.paths.trackedPieceAfter(path, target.witness, at).flatten
      .getOrElse(fail("the original captured target must still exist at this legal inventory"))
    value.paths.positionView(path, at).flatMap(_.legalRows).getOrElse(fail("missing complete legal inventory"))
      .filter(_.capture.contains(original)).map(_.move).toSet

  private def english(value: Boards, statement: LegalReplyOption): String =
    CommentaryEnglish.statement(statement).map {
      case text: String => text
      case move: RootQuestionSummary.MoveRef => value.paths.notation.line(move.path.moves)(move.offset)
    }.mkString

  private def syntheticReceipt(value: Boards): CommentaryInvestigation.Answer =
    CommentaryInvestigation.Answer("synthetic-lost-guard-root", "synthetic-lost-guard-key",
      CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), "equal",
      value.pair.head.line.ref.id, value.pair(1).line.ref.id, Json.obj(), Json.obj(),
      value.pair.head.ids.head, value.pair(1).ids.head, Some(QualityBand.Close))

  test("off-PV same legal arrival requests its bounded witnesses without requiring a forecasted Nd5"):
    val value = boards(retained = Nil)
    assertEquals(value.packet.assembly.input.positionHistory.currentPly, 39)
    assert(!current.exists(_.contains("c3d5")))
    assert(!value.paths.paths.exists(_.moves.startsWith(reference)))
    val batch = discover(value)
    assert(batch.functionalReplayNeeds.contains(reference),
      "a missing reference board is replay work, not absence of a useful question")
    assert(batch.functionalReplayNeeds.contains(alternative),
      "the same off-PV action must be tested in the matched other root context")
  
  test("same original guard location and same original opposing action certify only the lost capture fact"):
    val value = boards()
    val recorded = proof(value)
    val fact = statement(value, recorded)
    assert(FunctionalReplyReading.valid(value.paths, fact))
    assertEquals(recorded.owner.answer, None,
      "a completed board fact does not invent a root numerical receipt")
    assertEquals(recorded.condition.subject, recorded.effect.subject)
    assertEquals(recorded.effect.target, recorded.action.subject)
    List(1, 2).foreach { at =>
      assertEquals(value.paths.trackedPieceAfter(fact.reference, guard.witness, at).flatten.map(_.square.key), Some("f5"))
      assertEquals(value.paths.trackedPieceAfter(fact.alternative, guard.witness, at).flatten.map(_.square.key), Some("e6"))
    }
    List(fact.reference, fact.alternative).foreach { path =>
      assertEquals(value.paths.trackedPieceAfter(path, knight.witness, 2).flatten.map(_.square.key), Some("d5"))
    }
    assertEquals(value.paths.trackedPieceAfter(fact.price.get, knight.witness, 3), Some(None))
    assertEquals(fact.actionReferences, Nil)
    assertEquals(fact.covered, Nil, "the conditional capture cannot own actual root captures")

  test("remaining rook capture retains a factual detail while leaving the current core judgment intact"):
    val value = boards()
    val recorded = proof(value)
    val fact = statement(value, recorded)
    assertEquals(allCapturesOf(value, fact.reference, 2, knight), Set("c5d5"))
    assertEquals(allCapturesOf(value, fact.alternative, 2, knight), Set("c5d5", "e6d5"))
    val previous = value.previous
    val answer = FunctionalReplyReading.withAnswer(value.packet, value.paths, value.pair, List(recorded), previous)
    assertEquals(answer.core, previous.core, "a specific lost pawn capture cannot replace a root judgment with utility")
    assertEquals(answer.comparison, previous.comparison)
    assert(!answer.core.flatMap(_.statements).exists(_.isInstanceOf[LegalReplyOption]))
    assert(answer.details.flatMap(_.statements).exists {
      case value: LegalReplyOption => value.proof == recorded
      case _ => false
    }, "the valid difference must remain readable rather than be discarded as redundant")
    val text = english(value, fact)
    assert(text.contains("exf5") && text.contains("Nd5") && text.contains("exd5"), text)
    assert(text.contains("Rxd5") || (text.toLowerCase.contains("rook") && text.contains("c5")), text)
    assert(!text.contains("moves the knight away"), "the guard moves, not the arriving knight")
    List("safe", "useful", "best", "winning", "forced", "cheaper", "evaluates").foreach(word =>
      assert(!text.toLowerCase.contains(word), text))
    assert(RootQuestionSummary.realize(value.paths, value.pair, answer).nonEmpty,
      "a detail-only conditional witness must survive the actual delivery boundary")

  test("all immediate pawn and rook recaptures are known without promoting nominal exchange price to utility"):
    val value = boards()
    assertEquals(allCapturesOf(value, value.path(price), 3, guard), Set("d4d5", "e4d5"))
    List(reference, alternative).foreach { prefix =>
      assertEquals(allCapturesOf(value, value.path(prefix :+ "c5d5"), 3, rook), Set("d4d5", "e4d5"),
        "a voluntary rook-only recapture chain cannot erase the legal pawn recapture")
    }
    assertEquals(value.packet.assembly.input.evaluationObservations.size, 2,
      "retained legal price witnesses are not independently evaluated response receipts")

  test("SG48 moved bishop still captures the same Nf6 so rook capture and missing rook recapture do not imply safety"):
    val fen = "rn4k1/pp2p1b1/3p2P1/5rpQ/2PRN3/4P2B/qP5P/2K4R b - - 1 1"
    val left = moves("g7d4 e4f6")
    val right = moves("b8c6 e4f6")
    val value = boards(fen, Nil, List(List("g7d4"), List("b8c6")),
      List(left, right, left :+ "d4f6", right :+ "g7f6", left :+ "f5f6", right :+ "f5f6"))
    val bishop = Piece("black", "bishop", "g7")
    val target = Piece("white", "knight", "e4")
    List(left, right).foreach { line =>
      val path = value.path(line)
      val b = value.paths.trackedPieceAfter(path, bishop.witness, 2).flatten.get
      val n = value.paths.trackedPieceAfter(path, target.witness, 2).flatten.get
      assert(value.paths.positionView(path, 2).flatMap(_.legalRows).exists(_.exists(row =>
        row.actor == b && row.capture.contains(n))), "both original bishop captures are legal")
      assertEquals(allCapturesOf(value, value.path(line :+ "f5f6"), 3,
        Piece("black", "rook", "f5")), Set.empty[String],
        "complete empty immediate recaptures do not certify safety against forcing checks")
    }
    val capture = Predicate("legal_capture", Some(bishop), Some(target))
    val proofs = FunctionalReplyReading.read(CommentaryFactReader.read(value.packet, value.paths,
      discover(value).additions))
    assert(!proofs.exists(proof => proof.condition == Predicate("location", Some(bishop)) && proof.effect == capture),
      "changing the guard square is insufficient when the same guard still takes the same target")

  test("wrong turn, original actor, illegal price and foreign root receipt cannot certify the conditional fact"):
    val value = boards()
    val recorded = proof(value)
    val fact = statement(value, recorded)
    assertEquals(CommentaryFactAtoms.read(value.packet, value.paths, fact.alternative, 1, effect).value, Some("not_turn"))
    val otherOwner = boards(prefix = Nil)
    val foreignReceipt = syntheticReceipt(otherOwner)
    assertNotEquals(foreignReceipt.referenceOccurrenceId, value.pair.head.ids.head)
    val invalid = List(
      fact.copy(proof = recorded.copy(condition = Predicate("location", Some(Piece("black", "pawn", "d6"))))),
      fact.copy(proof = recorded.copy(effect = effect.copy(target = Some(Piece("white", "knight", "f3"))))),
      fact.copy(proof = recorded.copy(action = action.copy(subject = Some(guard)))),
      fact.copy(proof = recorded.copy(price = Some(alternative :+ "e6e5"))),
      fact.copy(proof = recorded.copy(owner = recorded.owner.copy(answer = Some(foreignReceipt)))),
      fact.copy(alternative = otherOwner.path(alternative)))
    invalid.foreach(fact => assert(!FunctionalReplyReading.valid(value.paths, fact)))

  test("the earlier Black-root fact and synthetic receipt cannot own a currently played White Nd5 judgment"):
    val earlier = boards()
    val recorded = proof(earlier)
    val whiteFen = "3k3r/3bbp2/p2p1p2/1pr2p1p/3RP2P/P1N2B2/1PP3P1/1K1R4 w - - 0 21"
    val own = boards(whiteFen, history :+ "e6f5", List(List("c3d5"), List("c3e2")), Nil)
    val borrowed = recorded.copy(owner = recorded.owner.copy(answer = Some(syntheticReceipt(earlier))))
    assertEquals(FunctionalReplyReading.withAnswer(own.packet, own.paths, own.pair, List(borrowed), own.previous), own.previous,
      "its own legal history and current root pair must own the White assessment")
    assert(!FunctionalReplyReading.valid(own.paths, statement(earlier, borrowed)))

  test("a pinned pawn's geometric attack cannot supply a legal vanished denial"):
    val fen = "4k3/p7/4p3/5P2/8/2N5/8/4R2K b - - 0 1"
    val left = moves("e6e5 c3d5")
    val right = moves("a7a6 c3d5")
    val value = boards(fen, Nil, List(List("e6e5"), List("a7a6")), List(left, right))
    val held = value.path(right)
    val pawn = value.paths.trackedPieceAfter(held, guard.witness, 2).flatten.get
    assert(value.paths.positionView(held, 2).exists(view =>
      new MoveWorkProjection.Position(view).controlledBy(pawn)("d5")))
    assert(!allCapturesOf(value, held, 2, knight).contains("e6d5"),
      "moving the pinned e-pawn would expose its own king on e8")
    val proofs = FunctionalReplyReading.read(CommentaryFactReader.read(value.packet, value.paths,
      discover(value).additions))
    assert(!proofs.exists(_.effect == effect), "a geometric contact is not a legal capture response")

  test("an equal-priced remaining pawn preserves the specific fact without making the original guard indispensable"):
    val fen = "4k3/8/2p1pp2/5P2/4P3/2N5/8/3R3K b - - 0 1"
    val left = moves("e6f5 c3d5")
    val right = moves("e8f8 c3d5")
    val denied = right :+ "e6d5"
    val replacement = left :+ "c6d5"
    val value = boards(fen, Nil, List(moves("e6f5 d1d2"), moves("e8f8 d1d2")),
      List(left, right, denied, replacement))
    val recorded = FunctionalReplyReading.Proof(FunctionalReplyReading.RootOwner(
      "synthetic-equal-priced-replacement", CompareMoves("function_root", Nil, "e6f5", "e8f8")),
      condition, left, right, left, effect, action, Nil, Some(denied))
    val fact = statement(value, recorded)
    assert(FunctionalReplyReading.valid(value.paths, fact))
    assertEquals(allCapturesOf(value, fact.reference, 2, knight), Set("c6d5"))
    List(denied -> guard, replacement -> Piece("black", "pawn", "c6")).foreach { (line, target) =>
      assertEquals(allCapturesOf(value, value.path(line), 3, target), Set("d1d5", "e4d5"))
    }
    val previous = value.previous
    val answer = FunctionalReplyReading.withAnswer(value.packet, value.paths, value.pair, List(recorded), previous)
    assertEquals(answer.core, previous.core, "a second pawn can still make the same immediate knight-for-pawn trade")
    assert(!answer.core.flatMap(_.statements).exists(_.isInstanceOf[LegalReplyOption]))
    assert(answer.details.flatMap(_.statements).exists {
      case value: LegalReplyOption => value.proof == recorded
      case _ => false
    })

  test("missing, truncated, duplicate and foreign capturer snapshots cannot certify the factual caveat"):
    val value = boards()
    val fact = statement(value, proof(value))
    assertEquals(fact.remainingCapturers, Some(List(rook.witness)))
    val forged = List(
      None,
      Some(Nil),
      Some(List(rook.witness, rook.witness)),
      Some(List(Piece("black", "rook", "c6").witness)),
      Some(List(Piece("white", "rook", "d4").witness)),
      Some(List(guard.witness)))
    forged.foreach(snapshot => assert(!FunctionalReplyReading.valid(value.paths,
      fact.copy(remainingCapturers = snapshot)), "the checked exact complete reply set must survive realization"))

  test("a genuinely complete empty capture inventory is distinct from a missing inventory"):
    val fen = "4k3/p7/4p3/5P2/8/2N5/8/7K b - - 0 1"
    val left = moves("e6f5 c3d5")
    val right = moves("a7a6 c3d5")
    val denied = right :+ "e6d5"
    val value = boards(fen, Nil, List(List("e6f5"), List("a7a6")), List(left, right, denied))
    val recorded = FunctionalReplyReading.Proof(FunctionalReplyReading.RootOwner(
      "synthetic-no-remaining-capturer", CompareMoves("function_root", Nil, "e6f5", "a7a6")),
      condition, left, right, left, effect, action, Nil, Some(denied))
    val fact = statement(value, recorded)
    assertEquals(allCapturesOf(value, fact.reference, 2, knight), Set.empty[String])
    assertEquals(fact.remainingCapturers, Some(Nil))
    assert(FunctionalReplyReading.valid(value.paths, fact), "a complete empty set is usable factual evidence")
    assert(!FunctionalReplyReading.valid(value.paths, fact.copy(remainingCapturers = None)),
      "unknown must not masquerade as the certified empty reply set")

  test("a capturing arrival cannot be certified as the same quiet opposing action"):
    val fen = "4k3/p7/4p3/3p1P2/8/2N5/8/7K b - - 0 1"
    val left = moves("e6f5 c3d5")
    val right = moves("a7a6 c3d5")
    val denied = right :+ "e6d5"
    val value = boards(fen, Nil, List(List("e6f5"), List("a7a6")), List(left, right, denied))
    List(left, right).foreach { line =>
      val path = value.path(line)
      val actualKnight = value.paths.trackedPieceAfter(path, knight.witness, 1).flatten.get
      assert(value.paths.positionView(path, 1).flatMap(_.legalRows).exists(_.exists(row =>
        row.move == "c3d5" && row.actor == actualKnight &&
          row.capture.contains(Piece("black", "pawn", "d5").witness))),
        "both real arrivals capture the original pawn on d5")
    }
    val recorded = FunctionalReplyReading.Proof(FunctionalReplyReading.RootOwner(
      "synthetic-capturing-arrival", CompareMoves("function_root", Nil, "e6f5", "a7a6")),
      condition, left, right, left, effect, action, Nil, Some(denied))
    assert(!FunctionalReplyReading.valid(value.paths, statement(value, recorded)),
      "legal_move alone cannot convert an actual capture into the quiet conditional action")
