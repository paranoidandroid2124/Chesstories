package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.*

/** Legally replayed book positions and local counterfactual paths test the
  * scope of a usable response. Scores and receipts are explicitly synthetic:
  * these tests cannot establish best defence, compensation or score causality.
  */
class FunctionalReplyReadingTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import CommentaryAnswer.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import PrincipalVariationComparison.QualityBand

  private def moves(value: String): List[String] = value.split(" ").toList
  private val sg13Fen = "4Q3/5pkp/2Bp1bp1/4p3/1P2P2P/5qP1/5P2/6K1 w - - 2 30"
  private val sg13Current = List(
    moves("e8d7 f3h5"),
    moves("c6d5 f6h4 e8f7 f3f7 d5f7 g7f7"))
  private val sg13Reference = moves("e8d7 f6h4 g3h4 f3g4")
  private val sg13Alternative = moves("c6d5 f6h4 g3h4 f3g4")
  private val sg13Option = sg13Reference :+ "d7g4"
  private val queen = Piece("white", "queen", "e8")
  private val checker = Piece("black", "queen", "f3")
  private val queenControl = Predicate("control", Some(queen), square = Some("g4"))
  private val queenCapture = Predicate("legal_capture", Some(queen), Some(checker))

  private case class Boards(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], byMoves: Map[List[String], PreparedPath]):
    def path(prefix: List[String]): PreparedPath = paths.paths.filter(_.moves.startsWith(prefix))
      .minByOption(_.moves.size).getOrElse(fail(s"missing certified local path for $prefix"))

  private case class Fixture(boards: Boards, entry: Entry, proof: FunctionalReplyReading.Proof,
      previous: Answer):
    def statement: LegalReplyOption = LegalReplyOption(boards.path(proof.reference),
      boards.path(proof.alternative), boards.path(proof.option), proof)
    def answer: Answer = FunctionalReplyReading.withAnswer(boards.packet, boards.paths,
      boards.pair, List(proof), previous)

  private def boards(fen: String, current: List[List[String]], local: List[List[String]],
      close: Boolean = false, playedMove: Option[String] = None): Boards =
    assertEquals(current.size, 2)
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen, playedMove.getOrElse(current.head.head),
      current.zipWithIndex.map { (line, index) =>
        EngineLine(line, if close || index == 1 then 0 else 100, None, 16)
      })).get
    val continuations = local.distinct.filterNot(current.contains).map { line =>
      val continuation = input.positionHistory.admitToFirstAutomaticTerminal(line).toOption
        .getOrElse(fail(s"illegal test path $line"))
      assertEquals(continuation.stepsAfter(input.positionHistory).get.map(_.uci), line,
        "an accepted legal prefix cannot certify an illegal or terminal suffix")
      continuation
    }
    val retained = MoveReviewInputAdmission.retainLegalContinuations(input, continuations)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(retained)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = (current ++ local).distinct.map(line => line -> paths.certifiedPath(packet, line).get).toMap
    Boards(packet, paths, current.map(found), found)

  private def receipt(value: Boards, close: Boolean = false,
      referenceMove: Option[String] = None): CommentaryInvestigation.Answer =
    val reference = referenceMove.flatMap(move => value.pair.find(_.moves.head == move)).getOrElse(value.pair.head)
    val alternative = value.pair.find(_ ne reference).get
    CommentaryInvestigation.Answer("synthetic-functional-root", "synthetic-functional-root-key",
      CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16),
      if close then "equal" else "worse", reference.line.ref.id, alternative.line.ref.id,
      Json.obj(), Json.obj(), reference.ids.head, alternative.ids.head,
      Some(if close then QualityBand.Close else QualityBand.Separated))

  private def fixture(fen: String = sg13Fen, current: List[List[String]] = sg13Current,
      reference: List[String] = sg13Reference, alternative: List[String] = sg13Alternative,
      option: List[String] = sg13Option, stored: List[List[String]] = Nil,
      close: Boolean = false, playedMove: Option[String] = None): Fixture =
    val value = boards(fen, current, if stored.nonEmpty then stored else List(reference, alternative, option),
      close, playedMove)
    val batch = CommentaryDiscovery.discover(value.packet, value.paths,
      List(CommentaryDiscovery.Anchor(Nil, current.head.head)),
      current.map(line => CommentaryDiscovery.SourcePath(line, Source("user", "synthetic-functional-current"))),
      Nil, pvGuided = true)
    val generated = batch.additions.find(entry =>
      entry.request.origin.question.producer == "C-reply-function" && entry.request.requirements.exists {
        case fact: Fact => fact.id == "function_effect_reference" && fact.path.source == Declared(reference)
        case _ => false
      } && entry.request.requirements.exists {
        case fact: Fact => fact.id == "function_option" && fact.path.source == Declared(option)
        case _ => false
      }).getOrElse(fail(s"missing generated conditional function for $reference / $option"))
    val comparison = generated.request.requirements.collectFirst {
      case comparison: CompareMoves if comparison.id == "function_root" => comparison
    }.get
    val answered = generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "function_root" =>
        reading.copy(state = "answered", answer = Some(receipt(value, close, Some(comparison.referenceMove))))
      case reading => reading
    })
    val entry = CommentaryFactReader.read(value.packet, value.paths, List(answered)).head
    val proof = FunctionalReplyReading.read(List(entry)).find(proof =>
      proof.reference == reference && proof.alternative == alternative && proof.option == option)
      .getOrElse(fail("all named local facts and the separate current root condition must be supported"))
    val previous = RootQuestionSummary.completePairAnswer(value.packet, value.paths, value.pair,
      "selected_comparison", Nil).get
    Fixture(value, entry, proof, previous)

  private def english(value: Boards, statement: LegalReplyOption): String =
    CommentaryEnglish.statement(statement).map {
      case text: String => text
      case move: RootQuestionSummary.MoveRef => value.paths.notation.line(move.path.moves)(move.offset)
    }.mkString

  test("SG13 independently replays the natural recapture even when neither current PV uses it"):
    val value = fixture()
    assert(!sg13Current.exists(_.contains("g3h4")))
    assertEquals(value.entry.readings.find(_.requirementId == "function_context").get.state, "supported")
    assertEquals(value.proof.condition, queenControl)
    assertEquals(value.proof.effect, queenCapture)
    assert(FunctionalReplyReading.valid(value.boards.paths, value.statement))
    assertEquals(value.statement.actionReferences, Nil)
    assertEquals(value.statement.covered, Nil,
      "a conditional legal option cannot own the captures on an actual root continuation")
    assertEquals(value.answer.question, "functional_response")
    val delivered = RootQuestionSummary.realize(value.boards.paths, value.boards.pair, value.answer).get
    assert(delivered.text.contains("legal variation Qd7 Bxh4 gxh4 Qg4+"), delivered.text)
    assert(delivered.text.contains("can answer the check with Qxg4"), delivered.text)
    assert(delivered.text.contains("that capture with the same queen is unavailable"), delivered.text)
    assert(value.answer.core.flatMap(_.statements).exists(_.isInstanceOf[LegalReplyOption]))
    assert(value.answer.core.flatMap(_.statements).flatMap(_.covered)
      .contains(RootQuestionSummary.MoveRef(value.boards.pair(1), 1)),
      "the actual Bd5 Bxh4 material price must survive the conditional lead")

  test("the conditional explanation does not turn a legal resource into best defence or a forced line"):
    val value = fixture()
    val text = english(value.boards, value.statement)
    assert(text.contains("In the legal variation") && text.contains("can answer"), text)
    List("forced", "must", "best", "winning", "evaluates", "compensates", "blunder").foreach { forbidden =>
      assert(!text.toLowerCase.contains(forbidden), text)
    }
    assert(text.contains("Qg4+"), "Qg4 remains a legal checking move in both arms")

  test("a different current first defence preserves the independently verified local function"):
    val current = List(moves("e8d7 g6g5"), moves("c6d5 f6h4 b4b5"))
    val value = fixture(current = current)
    assertEquals(value.entry.readings.find(_.requirementId == "function_live").get.state, "supported")
    assert(FunctionalReplyReading.valid(value.boards.paths, value.statement))
    assertEquals(value.answer.question, "functional_response")
    assertEquals(value.proof.reference, sg13Reference)
    assertNotEquals(value.proof.owner.answer.get.referenceLineId, value.statement.reference.line.ref.id,
      "the current root receipt and the declared conditional realization keep different line owners")

  test("a longer certified PV can supply the exact conditional prefix without importing its suffix"):
    val left = sg13Reference ++ moves("g1f1 h7h6")
    val right = sg13Alternative ++ moves("g1f1 h7h6")
    val option = sg13Option :+ "h7h6"
    val value = fixture(stored = List(left, right, option))
    assert(!value.boards.paths.paths.exists(_.moves == sg13Alternative))
    assert(FunctionalReplyReading.valid(value.boards.paths, value.statement))
    assertEquals(value.statement.referenceSequence.size, sg13Reference.size)
    assertEquals(value.statement.alternativeSequence.size, sg13Alternative.size)
    assertEquals(value.statement.option.offset, sg13Option.size - 1)
    assertEquals(value.statement.references.size, sg13Reference.size + sg13Alternative.size + 1)
    assert(value.statement.references.filter(_.path eq value.statement.alternative).forall(_.offset < 4))
    val text = english(value.boards, value.statement)
    assert(!text.contains("Kf1") && !text.contains("h6"), text)
    assert(text.contains("Qg4+") && text.contains("Qxg4"), text)
    assertEquals(value.answer.question, "functional_response")

  test("another available capture of the checker leaves the queen-specific fact true but cannot own the lead"):
    val fen = "4Q3/5pkp/2Bp1bp1/4p3/1P2P2P/5qP1/4BP2/6K1 w - - 2 30"
    val value = fixture(fen = fen)
    assert(FunctionalReplyReading.valid(value.boards.paths, value.statement))
    assert(value.boards.paths.positionView(value.statement.alternative, 4).flatMap(_.legalRows)
      .exists(_.exists(_.move == "e2g4")), "Bishop e2 can also capture the checker after Bd5")
    assertEquals(value.answer, value.previous,
      "the same-queen option is redundant when the alternative retains Bxg4")

  test("an absent current root condition rejects an old declaration even while its local path was supported"):
    val value = fixture()
    val changed = value.entry.copy(readings = value.entry.readings.map {
      case reading if reading.requirementId == "function_live" || reading.requirementId == "function_context" =>
        reading.copy(state = "refuted", result = None)
      case reading => reading
    })
    assertEquals(changed.readings.find(_.requirementId == "function_effect_reference").get.state, "supported")
    assertEquals(FunctionalReplyReading.read(List(changed)), Nil)
    val unknown = value.entry.copy(readings = value.entry.readings.map {
      case reading if reading.requirementId == "function_context" => reading.copy(state = "partial", result = None)
      case reading => reading
    })
    assertEquals(FunctionalReplyReading.read(List(unknown)), Nil,
      "an unfinished or unavailable condition is not successful proof")

  test("endpoint control restored by a later pawn capture is not continuous root-owned support"):
    val fen = "4Q3/5pkp/2Bp1bp1/4p3/1P2PP1P/5qP1/5P2/6K1 w - - 2 30"
    val tail = moves("f6h4 g3h4 f3h5 f4f5 h5h6 f5g6 h6h5 b4b5 h5g4")
    val reference = "e8d7" :: tail
    val alternative = "c6d5" :: tail
    val option = reference :+ "d7g4"
    val current = List(moves("e8d7 f3h5"), moves("c6d5 f6h4"))
    val value = boards(fen, current, List(reference, alternative, option))
    val root = CompareMoves("function_root", Nil, "e8d7", "c6d5")
    val proof = FunctionalReplyReading.Proof(CommentaryIdea.AnsweredComparison("phase-counter", root,
      receipt(value)), queenControl, reference, alternative, option, queenCapture, queenCapture, Nil)
    val statement = LegalReplyOption(value.path(reference), value.path(alternative), value.path(option), proof)
    assert(value.paths.positionView(statement.reference, 4).exists(view =>
      new MoveWorkProjection.Position(view).controlledBy(
        value.paths.trackedPieceAfter(statement.reference, queen.witness, 4).flatten.get)("g4")))
    assert(value.paths.positionView(statement.reference, 5).exists(view =>
      !new MoveWorkProjection.Position(view).controlledBy(
        value.paths.trackedPieceAfter(statement.reference, queen.witness, 5).flatten.get)("g4")))
    assert(value.paths.positionView(statement.reference, 10).flatMap(_.legalRows).exists(_.exists(_.move == "d7g4")))
    assert(!FunctionalReplyReading.valid(value.paths, statement),
      "later fxg6 reopens the queen ray; matching endpoints cannot attribute that restoration to Qd7")

  test("a legal but different option, wrong original pieces, short proof and foreign path cannot pass"):
    val value = fixture(stored = List(sg13Reference, sg13Alternative, sg13Option,
      sg13Reference :+ "g1f1"))
    val original = value.statement
    val wrongOption = original.copy(available = value.boards.path(sg13Reference :+ "g1f1"),
      proof = value.proof.copy(option = sg13Reference :+ "g1f1"))
    val wrongTarget = queenCapture.copy(target = Some(Piece("black", "queen", "d8")))
    val wrongActor = queenCapture.copy(subject = Some(Piece("white", "bishop", "c6")))
    val foreign = fixture()
    val forged = List(wrongOption,
      original.copy(proof = value.proof.copy(effect = wrongTarget, action = wrongTarget)),
      original.copy(proof = value.proof.copy(effect = wrongActor, action = wrongActor)),
      original.copy(reference = value.boards.pair.head),
      original.copy(alternative = foreign.statement.alternative),
      original.copy(proof = value.proof.copy(owner = value.proof.owner.copy(answer = Some(
        value.proof.owner.answer.get.copy(referenceOccurrenceId = value.boards.pair(1).ids.head))))))
    forged.foreach(statement => assert(!FunctionalReplyReading.valid(value.boards.paths, statement)))

  test("a root receipt from another pair cannot import the same legal local story"):
    val value = fixture()
    val wrong = value.proof.copy(owner = value.proof.owner.copy(answer = Some(
      value.proof.owner.answer.get.copy(referenceLineId = value.boards.pair(1).line.ref.id))))
    assertEquals(FunctionalReplyReading.withAnswer(value.boards.packet, value.boards.paths,
      value.boards.pair, List(wrong), value.previous), value.previous)

  test("Kh2 preserves an already legal Ne5 against the same Qa7 despite a close equal root comparison"):
    val fen = "3Q1nk1/1q1p2pp/p3p3/1p6/4P1N1/5PP1/PPr3P1/3R2K1 w - - 0 1"
    val current = List(moves("g1h2 b7c7 d8c7 c2c7 g4e5"), moves("d8g5 b7c8"))
    val reference = moves("g1h2 b7a7")
    val alternative = moves("d8g5 b7a7")
    val option = reference :+ "g4e5"
    val value = fixture(fen, current, reference, alternative, option, close = true)
    assertEquals(value.proof.owner.answer.get.qualityBand, Some(QualityBand.Close))
    assertEquals(value.proof.owner.answer.get.evaluationRelation, "equal")
    assertEquals(value.proof.effect, Predicate("check", side = Some("white")))
    assertEquals(value.proof.action, Predicate("legal_move", Some(Piece("white", "knight", "g4")), square = Some("e5")))
    val originalActor = value.proof.action.subject.get.witness
    assertEquals(value.boards.paths.trackedPieceAfter(value.statement.reference, originalActor, 2),
      value.boards.paths.trackedPieceAfter(value.statement.alternative, originalActor, 2),
      "the same original knight must still start from the same square; the check is what changes its immediate availability")
    assert(FunctionalReplyReading.valid(value.boards.paths, value.statement))
    assertEquals(value.answer.question, "functional_response")
    val text = english(value.boards, value.statement)
    assert(text.contains("against Qa7") && text.contains("can continue with Ne5"), text)
    assert(text.contains("After Qg5 Qa7+") && text.contains("cannot be played immediately"), text)
    assert(!text.contains("evaluates") && !text.contains("best") && !text.contains("forced"), text)
    val wrongSide = value.statement.copy(proof = value.proof.copy(effect = Predicate("check", side = Some("black"))))
    assert(!FunctionalReplyReading.valid(value.boards.paths, wrongSide))

  test("an alternative played root keeps its independent purpose and its own exact reversed receipt"):
    val fen = "3Q1nk1/1q1p2pp/p3p3/1p6/4P1N1/5PP1/PPr3P1/3R2K1 w - - 0 1"
    val current = List(moves("d8g5 b7c8"), moves("g1h2 b7c7 d8c7 c2c7 g4e5"))
    val reference = moves("g1h2 b7a7")
    val alternative = moves("d8g5 b7a7")
    val value = fixture(fen, current, reference, alternative, reference :+ "g4e5",
      close = true, playedMove = Some("g1h2"))
    assertEquals(value.boards.pair.head.moves.head, "d8g5", "the primary comparison still starts with Qg5")
    assertEquals(value.boards.packet.assembly.input.playedMoveUci, "g1h2")
    assertEquals(value.proof.owner.comparison.referenceMove, "g1h2")
    assertEquals(value.proof.owner.comparison.alternativeMove, "d8g5")
    assertEquals(value.proof.owner.answer.get.referenceLineId, value.boards.pair(1).line.ref.id)
    assertEquals(value.proof.owner.answer.get.referenceOccurrenceId, value.boards.pair(1).ids.head)
    assertEquals(value.proof.owner.answer.get.alternativeLineId, value.boards.pair.head.line.ref.id)
    assertEquals(value.proof.owner.answer.get.alternativeOccurrenceId, value.boards.pair.head.ids.head)
    assertEquals(value.proof.owner.answer.get.evaluationRelation, "equal")
    assert(FunctionalReplyReading.valid(value.boards.paths, value.statement))
    assertEquals(value.answer.question, "functional_response")
    val delivered = RootQuestionSummary.realize(value.boards.paths, value.boards.pair, value.answer).get
    assert(delivered.text.startsWith("Kh2 changes the king's position"), delivered.text)
    assert(delivered.text.contains("can continue with Ne5"), delivered.text)
    assert(!delivered.text.contains("evaluates above") && !delivered.text.contains("winning"), delivered.text)

  test("a goal already performed by the alternative root cannot own a check-tempo explanation"):
    val fen = "3Q1nk1/1q1p2pp/p3p3/1p6/4P1N1/5PP1/PPr3P1/3R2K1 w - - 0 1"
    val current = List(moves("g1h2 b7c7 d8g5"), moves("d8g5 b7c8"))
    val reference = moves("g1h2 b7a7")
    val alternative = moves("d8g5 b7a7")
    val option = reference :+ "d8g5"
    val value = boards(fen, current, List(reference, alternative, option), close = true)
    val root = CompareMoves("function_root", Nil, "g1h2", "d8g5")
    val actor = Piece("white", "queen", "d8")
    val proof = FunctionalReplyReading.Proof(CommentaryIdea.AnsweredComparison("already-performed-counter", root,
      receipt(value, close = true)), Predicate("location", Some(Piece("white", "king", "g1"))),
      reference, alternative, option, Predicate("check", side = Some("white")),
      Predicate("legal_move", Some(actor), square = Some("g5")), Nil)
    val statement = LegalReplyOption(value.path(reference), value.path(alternative), value.path(option), proof)
    assertEquals(value.paths.trackedPieceAfter(statement.reference, actor.witness, 2).flatten.get.square.key, "d8")
    assertEquals(value.paths.trackedPieceAfter(statement.alternative, actor.witness, 2).flatten.get.square.key, "g5")
    assert(value.paths.positionView(statement.reference, 2).flatMap(_.legalRows)
      .exists(_.exists(_.move == "d8g5")))
    assert(!value.paths.positionView(statement.alternative, 2).flatMap(_.legalRows)
      .exists(_.exists(_.move == "d8g5")),
      "Qg5 is already achieved in that branch as well as the king being in check")
    assert(!FunctionalReplyReading.valid(value.paths, statement),
      "different actor locations cannot attribute the missing same goal solely to check tempo")
    val previous = RootQuestionSummary.completePairAnswer(value.packet, value.paths, value.pair,
      "selected_comparison", Nil).get
    assertEquals(FunctionalReplyReading.withAnswer(value.packet, value.paths, value.pair, List(proof), previous), previous)
    val batch = CommentaryDiscovery.discover(value.packet, value.paths,
      List(CommentaryDiscovery.Anchor(Nil, current.head.head)),
      current.map(line => CommentaryDiscovery.SourcePath(line, Source("user", "already-performed-counter"))),
      Nil, pvGuided = true)
    assert(!batch.additions.exists(entry => entry.request.origin.question.producer == "C-reply-function" &&
      entry.request.requirements.exists {
        case fact: Fact => fact.id == "function_option" && fact.path.source == Declared(option)
        case _ => false
      }), "discovery must not offer the already-performed queen goal as Kh2's check-tempo purpose")

  test("a deferred numeric comparison preserves a fully reread board purpose without answering that comparison"):
    val value = fixture()
    val entry = CommentaryFactReader.read(value.boards.packet, value.boards.paths, List(value.entry.copy(
      readings = value.entry.readings.map {
        case reading if reading.requirementId == "function_root" =>
          reading.copy(state = "deferred", stopReason = Some("comparison_not_current"), answer = None, result = None)
        case reading => reading
      }))).head
    val numeric = entry.readings.find(_.requirementId == "function_root").get
    assertEquals(numeric.state, "deferred")
    assertEquals(numeric.answer, None)
    assertEquals(entry.readings.find(_.requirementId == "function_context").get.state, "supported")
    val live = entry.request.requirements.collectFirst { case fact: Fact if fact.id == "function_live" => fact }.get
    assertEquals(live.path.source, Declared(List("e8d7")))
    assert(entry.generatedArms.exists(_.headRequirementIds == List("function_context")),
      "only the independently reread board conditions own this conditional purpose")
    val proof = FunctionalReplyReading.read(List(entry)).find(_.option == sg13Option).get
    assertEquals(proof.owner.answer, None)
    val statement = value.statement.copy(proof = proof)
    assert(FunctionalReplyReading.valid(value.boards.paths, statement))
    val answer = FunctionalReplyReading.withAnswer(value.boards.packet, value.boards.paths,
      value.boards.pair, List(proof), value.previous)
    assertEquals(answer.question, "functional_response")
    assert(RootQuestionSummary.realize(value.boards.paths, value.boards.pair, answer).nonEmpty)
    assertEquals(entry.readings.find(_.requirementId == "function_root").get, numeric,
      "a legal-purpose answer must not manufacture a completed numerical receipt")

    val wrongArm = entry.copy(generatedArms = entry.generatedArms.map(_.copy(
      referenceMove = "c6d5", alternativeMove = Some("e8d7"))))
    assertEquals(FunctionalReplyReading.read(List(wrongArm)), Nil,
      "supported facts cannot borrow a generated arm belonging to different roots")
    val foreignRoot = entry.copy(request = entry.request.copy(requirements = entry.request.requirements.map {
      case fact: Fact if fact.id == "function_live" => fact.copy(path = Path("function_live", Declared(List("c6d5"))))
      case requirement => requirement
    }))
    assertEquals(FunctionalReplyReading.read(List(foreignRoot)), Nil,
      "the live declaration must be the same canonical reference root, even with no numeric owner")
    val foreign = fixture()
    assert(!FunctionalReplyReading.valid(value.boards.paths, statement.copy(alternative = foreign.statement.alternative)),
      "equal-looking declarations from a foreign prepared packet remain unowned")

  test("changed current replies and PV identities preserve the same reread purpose while stale optional receipts stay invalid"):
    val known = fixture()
    val current = List(moves("e8d7 g6g5"), moves("c6d5 f6h4 b4b5"))
    val observed = boards(sg13Fen, current, List(sg13Reference, sg13Alternative, sg13Option))
    val entry = CommentaryFactReader.read(observed.packet, observed.paths, List(known.entry.copy(
      readings = known.entry.readings.map {
        case reading if reading.requirementId == "function_root" =>
          reading.copy(state = "deferred", stopReason = Some("comparison_not_current"), answer = None, result = None)
        case reading => reading
      }))).head
    assertNotEquals(known.proof.owner.answer.get.referenceLineId, observed.pair.head.line.ref.id)
    assertEquals(entry.readings.find(_.requirementId == "function_live").get.state, "supported")
    val proof = FunctionalReplyReading.read(List(entry)).find(_.option == sg13Option).get
    assertEquals(proof.owner.answer, None)
    val previous = RootQuestionSummary.completePairAnswer(observed.packet, observed.paths, observed.pair,
      "selected_comparison", Nil).get
    val value = Fixture(observed, entry, proof, previous)
    assert(FunctionalReplyReading.valid(observed.paths, value.statement))
    assertEquals(value.answer.question, "functional_response")
    val text = english(observed, value.statement)
    assert(text.contains("Qd7 Bxh4 gxh4 Qg4+") && text.contains("Qxg4"), text)
    assert(!FunctionalReplyReading.valid(observed.paths, value.statement.copy(proof = known.proof)),
      "a present optional receipt must retain its actual prepared line owner; it cannot be silently ignored")
