package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveObservationMeaning, MoveReviewAssembler, MoveReviewInputAdmission, PieceControlState}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Exposed book positions check chess facts. Invented, owned reports check interpretation
  * and delivery; their scores are not an evaluation of the book or discovery quality.
  */
class PracticalDecisionTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val profile = CommentaryEngineProfile.ServerAdmitted
  private def history(fen: String) = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
  private def najdorfOwner = history(chess.variant.Standard.initialFen.value).extend(
    ("e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4 g8f6 b1c3 a7a6 f1c4 e7e6 a2a4 f8e7 e1g1 e8g8 " +
      "c1e3 b8c6 g1h1 c8d7 d1e2 a8c8 c4b3 c6a5 b3a2 d8c7 a1d1 a5c4 e3c1 f8d8 f2f4 d7e8 " +
      "d1d3 c7b6 b2b3 c4a5 c1b2 a5c6 d4c6 e8c6 d3g3").split(" ").toList).toOption.get
  private val najdorfGame = "b6b4 c3d5 e6d5 e4d5 d8e8 d5c6 b7c6 e2a6".split(" ").toList
  private def najdorf16Owner = history(chess.variant.Standard.initialFen.value).extend(
    ("e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4 g8f6 b1c3 a7a6 f1c4 e7e6 c4b3 b7b5 c1g5 f8e7 " +
      "d1f3 d8b6 e1c1 e8g8 h1e1 c8b7 f3g3 b8c6 d4c6 b6c6 g5h6 f6e8 f2f4 g8h8 h6g5 e7g5 " +
      "f4g5 b5b4 c3e2").split(" ").toList).toOption.get
  private def awaiting(state: CommentaryJobState): AwaitingEngineWork = state match
    case active: AwaitingEngineWork => active
    case _ => fail(s"Expected pending work; verification=${state.verification}; scans=${state.discoveryState.scans}; " +
      state.proposalEntries.filter(_.request.origin.question.producer == "C-tactical-counter").map(entry =>
        entry.readings.map(reading => reading.requirementId + ":" + reading.state + ":" + reading.stopReason).mkString(",")).mkString(" / "))
  private def deliveredComparisons(state: CommentaryJobState): String =
    val response = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("q" * 32, "practical-choice", 10000L, state))
    val candidates = (response \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .filter(value => (value \ "detail" \ "type").asOpt[String].contains("question_comparison"))
    candidates.foreach { candidate =>
      val detail = (candidate \ "detail").as[JsObject]
      (detail \ "question_refs").as[List[JsObject]].foreach { reference =>
        val entry = state.proposalEntries.find(_.request.id == (reference \ "proposal_id").as[String]).get
        val question = entry.request.requirements.collectFirst {
          case value: CompareMoves if value.id == (reference \ "requirement_id").as[String] => value
        }.get
        assertEquals(question.prefixMovesUci, (detail \ "prefix_moves_uci").as[List[String]])
        assertEquals(question.referenceMove, (detail \ "reference_move").as[String])
        assertEquals(question.alternativeMove, (detail \ "alternative_move").as[String],
          "delivered question references retain the comparison orientation required by the receiver")
      }
    }
    candidates.flatMap(value => (value \ "detail" \ "text").asOpt[String]).mkString(" ")

  private def submit(state: AwaitingEngineWork, rows: List[(List[String], Int)]): CommentaryJobState =
    val work = state.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile, work.workId,
      work.executionKeySha256, rows.map { (moves, cp) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), work.requiredDepth)
      }), 1L).toOption.get

  private def begin(fen: String, played: String, rows: List[(List[String], Int)],
      supplied: List[List[String]] = Nil, prefixOwner: Option[CanonicalPositionHistory] = None,
      proposals: List[Request] = Nil): CommentaryJobState =
    val owner = prefixOwner.getOrElse(history(fen))
    val start = CommentaryJobReducer.startJob(owner.initialFen, owner.movePrefixUci, fen, played,
      owner.extend(List(played)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L,
      inputLines = supplied.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _)), proposals = proposals)
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    val preview = submit(start, rows.map((moves, cp) => moves.take(1) -> cp)).asInstanceOf[AwaitingEngineWork]
    submit(preview, rows)

  private def read(owner: CanonicalPositionHistory, lines: List[List[String]], previous: List[Entry] = Nil): List[Entry] =
    val input = MoveReviewInputAdmission.admitIndependent(owner, lines.head.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    assertEquals(input.suppliedLines.map(_.acceptedMoves), lines)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    val batch = CommentaryDiscovery.discover(packet, prepared,
      List(CommentaryDiscovery.Anchor(Nil, lines.head.head)),
      lines.map(CommentaryDiscovery.SourcePath(_, Source("author", "exposed-study-branches"))), previous,
      pvGuided = true)
    CommentaryFactReader.read(packet, prepared, (batch.additions ++ previous).distinctBy(_.request.id))

  test("Najdorf 16 delivers the severed cooperation and opened opposing line from existing observations"):
    val owner = najdorf16Owner
    val line = List("e6e5", "b3d5", "c6c7", "d5b7", "c7b7")
    val expected = "Blocks the line from White queen on g3 to Black pawn on d6; White rook on d1 still attacks that target. " +
      "Opens the line from White bishop on b3 to Black pawn on f7."
    List(line.take(1), line).foreach { moves =>
      val input = MoveReviewInputAdmission.admitIndependent(owner, moves.head,
        suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, moves))).get
      val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
      val edge = packet.assembly.playedTransition.get
      val changes = packet.observationsFor(Set(edge.evidence.id)).collect {
        case observation if observation.meaning.isInstanceOf[MoveObservationMeaning.PieceControl] =>
          val MoveObservationMeaning.PieceControl(before, after) = observation.meaning: @unchecked
          (before.controller.map(_.square.key), before.target.map(_.square.key), before.state, after.state)
      }
      assert(changes.contains((Some("g3"), Some("d6"), PieceControlState.Present, PieceControlState.Absent)),
        "the reader already has the queen's interrupted attack")
      assert(changes.contains((Some("b3"), Some("f7"), PieceControlState.Absent, PieceControlState.Present)),
        "the reader already has the opposing bishop's opened attack")
      val highlights = (RuntimeProtocol.observationsJson(packet) \ "highlights").as[List[JsObject]]
        .filter(value => (value \ "occurrence_id").as[String] == edge.evidence.id)
      val note = highlights.find(value => (value \ "text").as[String] == expected).getOrElse(
        fail(highlights.map(value => (value \ "text").as[String]).mkString(" / ")))
      assertEquals(highlights.size, 3, "the existing display cap is unchanged")
      val views = packet.resources.map(view => view.position.id.get -> view).toMap
      (note \ "frames").as[List[JsObject]].foreach { frame =>
        val view = views((frame \ "position_id").as[String])
        val sources = (frame \ "source_ids").as[List[String]]
        assert(sources.nonEmpty)
        sources.foreach { id =>
          assert(view.records.exists {
            case record @ EvidenceRecord(_, fact: RelationFactEvidence, _) =>
              fact.semanticId == id && packet.evidenceGraph.proofEligible(record)
            case _ => false
          }, s"unproven or wrong-position source: $id")
        }
      }
      (note \ "premise_ids").as[List[String]].foreach(id =>
        assert(packet.evidenceGraph.byId.get(id).exists(packet.evidenceGraph.proofEligible)))
    }
    // The normal job receives fixed, owned reports too; the score is synthetic.
    val state = begin(owner.currentFen, line.head,
      List(line -> 0, List("h7h6") -> 20, List("a6a5") -> 30), prefixOwner = Some(owner))
    val response = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("n" * 32, "najdorf-line-effects", 10000L, state))
    val root = state.observationsPacket.get.assembly.playedTransition.get.evidence.id
    val delivered = (response \ "observations" \ "highlights").as[List[JsObject]]
    assert(delivered.exists(value => (value \ "occurrence_id").as[String] == root &&
      (value \ "text").as[String] == expected), "the owned report reaches the normal job response")
    val comparison = (response \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("best_vs_second")).get
    val explanation = (comparison \ "detail" \ "text").as[String]
    assert(explanation.contains("White queen on g3") && explanation.contains("Black pawn on d6") &&
      explanation.contains("White bishop on b3") && explanation.contains("Black pawn on f7") &&
      explanation.contains("h7h6"), explanation)
    (comparison \ "evidence_ids").as[List[String]].foreach(id =>
      assert(state.observationsPacket.get.evidenceGraph.byId.get(id)
        .exists(state.observationsPacket.get.evidenceGraph.proofEligible), id))

  test("Najdorf 16 connects own obstruction and the returned reply square to the selected pair"):
    val owner = najdorf16Owner.extend(List("e6e5", "b3d5", "c6c7", "d5b7", "c7b7", "g3h4",
      "a8c8", "d1d3", "f7f5", "e4f5", "f8f5")).toOption.get
    val line = List("g2g4", "b7e4", "d3d2", "f5f8", "e2d4", "e4d5")
    val source = Source("user", "same-root-effects")
    val compare = Request("same-root-effects", Origin(source, source, source, source),
      List(CompareMoves("pair", Nil, "g2g4", "h4g4")))
    val state = begin(owner.currentFen, line.head,
      List(List("h4g4") -> 20, line -> 0, List("h4g3") -> -10), prefixOwner = Some(owner), proposals = List(compare))
    val response = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("o" * 32, "najdorf-own-line", 10000L, state))
    val root = state.observationsPacket.get.assembly.playedTransition.get.evidence.id
    val local = (response \ "observations" \ "highlights").as[List[JsObject]]
      .filter(value => (value \ "occurrence_id").as[String] == root)
      .map(value => (value \ "text").as[String]).mkString(" ")
    assert(local.contains("Black rook on f5"), local)
    assert(local.contains("Blocks the line from White queen on h4 toward e4"), local)
    assert(!local.contains("Black pawn on b4"), "the same blocked ray is represented by the used reply square")
    val comparison = (response \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("played_vs_best")).get
    val text = (comparison \ "detail" \ "text").as[String]
    assert(text.contains("White queen on h4") && text.contains("White queen on g4") && text.contains("e4"), text)
    assert(text.contains("b7e4"), text)
    assert(text.contains("d3d2") && text.contains("f5f8") && text.contains("initially attacked Black rook on f5"),
      "the supplied counterattack and reply must be connected to the original rook's later evasion: " + text)
    assert(!text.contains("Black pawn on b4"), text)
    assert(!text.contains("an explanation beyond this material comparison is still missing"), text)
    assert(!text.contains("safe") && !text.contains("strongest"), text)
    (comparison \ "evidence_ids").as[List[String]].foreach(id =>
      assert(state.observationsPacket.get.evidenceGraph.byId.get(id)
        .exists(state.observationsPacket.get.evidenceGraph.proofEligible), id))
    val questionText = deliveredComparisons(state)
    assert(questionText.contains("White queen on h4") && questionText.contains("White queen on g4") &&
      questionText.contains("b7e4"), questionText)
    assert(questionText.contains("d3d2") && questionText.contains("f5f8") &&
      questionText.contains("remains on f8 at this line's end"), questionText)

  test("an attack's later evasion cannot leak from a longer supplied branch into a short returned line"):
    val owner = najdorf16Owner.extend(List("e6e5", "b3d5", "c6c7", "d5b7", "c7b7", "g3h4",
      "a8c8", "d1d3", "f7f5", "e4f5", "f8f5")).toOption.get
    val short = List("g2g4", "b7e4", "d3d2")
    val state = begin(owner.currentFen, short.head,
      List(short -> 0, List("h4g4") -> 20, List("h4g3") -> -10),
      supplied = List(short ++ List("f5f8", "e2d4", "e4d5")), prefixOwner = Some(owner))
    val candidates = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("t" * 32, "short-attack", 10000L, state)) \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
    val comparison = candidates.find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("played_vs_best")).get
    val text = (comparison \ "detail" \ "text").as[String]
    assert(text.contains("b7e4"), text)
    assert(!text.contains("f5f8") && !text.contains("initially attacked Black rook on f5"), text)

  private def rookAttackExplanation(fen: String, line: List[String]): String =
    val state = begin(fen, line.head, List(line -> 0, List("h1g1") -> 20, List("h1h2") -> 10))
    val candidates = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("r" * 32, "rook-attack", 10000L, state)) \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
    val candidate = candidates.find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("played_vs_best")).get
    (candidate \ "evidence_ids").as[List[String]].foreach(id =>
      assert(state.observationsPacket.get.evidenceGraph.byId.get(id).exists(state.observationsPacket.get.evidenceGraph.proofEligible)))
    (candidate \ "detail" \ "text").as[String]

  test("an initial evasion retains the same target's renewed attack and later capture"):
    val fen = "r6k/8/8/8/8/8/8/R6K w - - 0 1"
    val renewed = List("a1a2", "a8b8", "a2b2", "h8g8")
    val surviving = rookAttackExplanation(fen, renewed)
    assert(surviving.contains("a8b8") && surviving.contains("remains on b8 at this line's end"), surviving)
    assert(surviving.contains("original attacker attacks it again"), surviving)
    val captured = rookAttackExplanation(fen, renewed :+ "b2b8")
    assert(captured.contains("a8b8") && captured.contains("b2b8: captures the initially attacked Black rook on a8"), captured)
    assert(!captured.contains("remains on b8"), captured)

  test("moving along the same attack line is not an evasion"):
    val text = rookAttackExplanation("r6k/8/8/8/8/8/8/R6K w - - 0 1",
      List("a1a2", "a8a7", "h1g1", "h8g8"))
    assert(!text.contains("out of White rook") && !text.contains("initially attacked"), text)

  test("capturing another rook on the original square does not capture the original target"):
    val text = rookAttackExplanation("r6r/7k/8/8/8/8/8/R6K w - - 0 1",
      List("a1a2", "a8b8", "a2b2", "b8b6", "b2a2", "h8a8", "a2a8"))
    assert(text.contains("initially attacked Black rook on a8") && text.contains("remains on b6 at this line's end"), text)
    assert(!text.contains("captures the initially attacked") && !text.contains("original target is captured"), text)

  test("evasion from another attacker cannot settle the initially observed attack"):
    val text = rookAttackExplanation("r6k/8/8/8/8/8/8/RR5K w - - 0 1",
      List("a1a2", "h8g8", "b1b8", "g8f7", "a2c2", "a8a7"))
    assert(!text.contains("initially attacked"), text)

  test("an unrelated evasion is not presented as the answer to an intermediate counterattack"):
    val text = rookAttackExplanation("r6k/2B3b1/4q3/8/8/2N5/8/R6K w - - 0 1",
      List("a1a2", "e6e7", "c3b1", "a8b8"))
    assert(text.contains("a8b8") && text.contains("remains on b8 at this line's end"), text)
    assert(!text.contains("e6e7") && !text.contains("c3b1"),
      "the knight evades the old bishop attack, not the queen's new attack on c7: " + text)
    assert(!text.contains("safe"), "the rook on b8 remains attacked by the bishop on c7: " + text)

  test("capturing the actual attacker explains why the original target can stay put"):
    val fen = "r6k/8/8/8/2b5/8/6B1/R6K w - - 0 1"
    val text = rookAttackExplanation(fen, List("a1a2", "c4a2"))
    assert(text.contains("c4a2: captures the original attacker White rook on a2"), text)
    assert(text.contains("initially attacked Black rook on a8 stays on a8"), text)
    assert(text.contains("White bishop on g2 attacks that target after this move"), text)
    assert(!text.contains("safe") && !text.contains("captures the initially attacked Black rook"), text)
    val captured = rookAttackExplanation(fen, List("a1a2", "c4a2", "g2a8"))
    assert(captured.contains("c4a2: captures the original attacker") &&
      captured.contains("g2a8: captures the initially attacked Black rook on a8"), captured)
    assert(!captured.contains("remains on a8"), captured)

  test("interposition connects the stopped attack and its later reopening to the same stationary target"):
    val fen = "r6k/8/8/1b6/8/8/8/R6K w - - 0 1"
    val blocked = List("a1a2", "b5a6")
    val first = rookAttackExplanation(fen, blocked)
    assert(first.contains("b5a6: blocks White rook on a2's line to the initially attacked Black rook on a8"), first)
    assert(first.contains("stays on a8") && first.contains("remains on a8 at this line's end"), first)
    val reopened = blocked ++ List("h1g1", "a6b5")
    val again = rookAttackExplanation(fen, reopened)
    assert(again.contains("a6b5: restores White rook on a2's attack on the initially attacked Black rook on a8"), again)
    assert(again.contains("original attacker attacks it again"), again)
    val captured = rookAttackExplanation(fen, reopened :+ "a2a8")
    assert(captured.contains("a6b5: restores") && captured.contains("a2a8: captures the initially attacked Black rook on a8"), captured)
    assert(!captured.contains("remains on a8"), captured)

  test("removing the interposed piece restores the same physical attack before the target is captured"):
    val fen = "r6k/8/2B5/1b6/8/8/8/R6K w - - 0 1"
    val text = rookAttackExplanation(fen, List("a1a2", "b5a6", "a2a6", "h8g8", "a6a8"))
    assert(text.contains("b5a6: blocks") && text.contains("White bishop on c6 attacks that target after this move"), text)
    assert(text.contains("a2a6: restores White rook on a6's attack on the initially attacked Black rook on a8"), text)
    assert(text.contains("a6a8: captures the initially attacked Black rook on a8"), text)

  test("capturing an already blocked attacker does not resolve the same attack twice"):
    val text = rookAttackExplanation("r6k/8/8/1b6/8/1b6/8/R6K w - - 0 1",
      List("a1a2", "b5a6", "h1g1", "b3a2"))
    assert(text.contains("b5a6: blocks") && text.contains("remains on a8 at this line's end"), text)
    assert(!text.contains("captures the original attacker"), text)

  test("capturing a different rook is not removal of the tracked attacker"):
    val text = rookAttackExplanation("r6k/8/8/8/8/4b3/5R2/R6K w - - 0 1", List("a1a2", "e3f2"))
    assert(!text.contains("captures the original attacker") && !text.contains("initially attacked Black rook on a8"), text)

  test("en passant removes the attacking pawn on its actual capture square"):
    val text = rookAttackExplanation("7k/8/8/3r4/3p4/8/4P3/7K w - - 0 1", List("e2e4", "d4e3"))
    assert(text.contains("d4e3: captures the original attacker White pawn on e4"), text)
    assert(text.contains("initially attacked Black rook on d5 stays on d5"), text)

  test("an absent attack witness cannot become a claimed blocked line"):
    val input = MoveReviewInputAdmission.admitIndependent(najdorf16Owner, "e6e5").get
    val assembly = MoveReviewAssembler.assembleOccurrences(input)
    val edge = assembly.playedTransition.get
    val records = assembly.evidenceGraph.records
    val withoutQueenAttack = records.filterNot { record =>
      record.ref.position == edge.from && (record.payload match
        case fact: RelationFactEvidence => fact.detail match
          case RelationWitnessDetail.GeometricControl(_, from, _, to) => from.key == "g3" && to.key == "d6"
          case _ => false
        case _ => false)
    }
    assert(withoutQueenAttack.size < records.size)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(assembly.copy(
      evidenceGraph = TypedEvidenceGraph.empty.addAll(withoutQueenAttack))).get
    val text = MoveWorkProjection.fromPacket(packet).map(value => (value \ "text").as[String]).mkString(" ")
    assert(!text.contains("Blocks the line from White queen on g3 to Black pawn on d6"), text)

  test("lost empty-square control belongs to one provider and requires complete negative evidence"):
    val owner = history("k7/8/5n2/8/7Q/8/6P1/KB6 w - - 0 1")
    val input = MoveReviewInputAdmission.admitIndependent(owner, "g2g4",
      suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, List("g2g4", "f6e4")))).get
    assertEquals(input.suppliedLines.head.acceptedMoves, List("g2g4", "f6e4"))
    val assembly = MoveReviewAssembler.assembleOccurrences(input)
    val root = assembly.playedTransition.get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(assembly).get
    val after = new MoveWorkProjection.Position(packet.resources.find(_.position == root.to).get)
    assert(after.controllers("e4", chess.White).exists(_.square.key == "b1"),
      "the bishop still controls e4 even though the queen's line is blocked")
    def text(value: EvidenceBackedJudgmentPacket) = MoveWorkProjection.fromPacket(value)
      .filter(note => (note \ "occurrence_id").as[String] == root.evidence.id)
      .map(note => (note \ "text").as[String]).mkString(" ")
    assert(text(packet).contains("Blocks the line from White queen on h4 toward e4"), text(packet))
    val records = assembly.evidenceGraph.records.filterNot { record =>
      record.ref.position == root.to && (record.payload match
        case fact: RelationFactEvidence => fact.detail match
          case RelationWitnessDetail.GeometricControl(_, from, _, to) => from.key == "b1" && to.key == "c2"
          case _ => false
        case _ => false)
    }
    assert(records.size < assembly.evidenceGraph.records.size)
    val incomplete = EvidenceBackedJudgmentPacket.fromAssembly(assembly.copy(
      evidenceGraph = TypedEvidenceGraph.empty.addAll(records))).get
    assert(!text(incomplete).contains("toward e4"), "missing control records cannot prove absence")

  test("blocking an own support line preserves a different friendly provider"):
    val owner = history("7k/Nr6/8/2B5/8/8/7K/RB6 w - - 0 1")
    val input = MoveReviewInputAdmission.admitIndependent(owner, "b1a2").get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val text = MoveWorkProjection.fromPacket(packet).map(value => (value \ "text").as[String]).mkString(" ")
    assert(text.contains("Blocks the line from White rook on a1 to White knight on a7; " +
      "White bishop on c5 still supports that target."), text)
    assert(!text.contains("Black rook on b7 still"), text)

  test("a returned castling reply uses the admitted king destination for empty-square relevance"):
    val owner = history("4k2r/8/8/8/8/8/4N3/K5R1 w k - 0 1")
    val input = MoveReviewInputAdmission.admitIndependent(owner, "e2g3",
      suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, List("e2g3", "e8g8")))).get
    assertEquals(input.suppliedLines.head.acceptedPlyCount, 2)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val root = packet.assembly.playedTransition.get.evidence.id
    val text = MoveWorkProjection.fromPacket(packet).filter(value => (value \ "occurrence_id").as[String] == root)
      .map(value => (value \ "text").as[String]).mkString(" ")
    assert(text.contains("Blocks the line from White rook on g1 toward g8"), text)

  test("a current PV investigation shares the remaining slot with optional comparison questions"):
    val owner = najdorfOwner
    val checked = List("d8d7", "g3g7")
    val first = awaiting(begin(owner.currentFen, "b6b4", List(najdorfGame -> 200, checked -> 0,
      List("b6c7") -> 300), prefixOwner = Some(owner)))
    assertEquals(first.issuedWork.searchAnchor.map(_.prefixMoves), Some(najdorfGame.take(6)))
    assertEquals(first.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("b7c6", "e7d8", "e7f8")))
    val unexplainedLoss = CommentaryIdea.read(first.proposalEntries).filter(idea =>
      idea.key.loss && idea.key.branch == checked)
    assert(unexplainedLoss.nonEmpty)
    assert(unexplainedLoss.forall(!_.observedReturn),
      "the missing return must be read before this loss alone grants its acceptance comparison essential priority")
    def rootChoice(state: CompletedCommentaryJob) = state.result match
      case CompletedPositionCommentary.FocusedMoveReview(_, review) =>
        (review.moveUci, review.bestMoveUci, review.playedRootRank, review.bestRootRank)
      case _ => fail("expected a move review")
    val root = rootChoice(first.assessment.get)
    val second = awaiting(submit(first, List(List("b7c6", "e2a6") -> 200,
      List("e7d8") -> 220, List("e7f8") -> 230)))
    val continuation = najdorfGame.take(4)
    assertEquals(second.issuedWork.searchAnchor.map(_.prefixMoves), Some(continuation),
      "after the known defense is answered, the current PV recovery question outranks a new Bb1 comparison")
    assertEquals(second.issuedWork.rootRestriction, EngineRootRestriction.Unrestricted)
    assert(second.proposalEntries.exists(_.request.requirements.exists {
      case value: CompareMoves => value.prefixMovesUci == List("d8d7") &&
        Set(value.referenceMove, value.alternativeMove) == Set("g3g7", "a2b1")
      case _ => false
    }), "the optional comparison remains represented, rather than being removed from the question pool")
    val done = submit(second, List(najdorfGame.drop(4) -> 200, List("b4b6") -> 220, List("c8c7") -> 240))
    assert(done.isInstanceOf[CompletedCommentaryJob])
    assertEquals(done.progress.physicalWorksIssued, 4, "two root reports and two shared investigations")
    assertEquals(done.progress.physicalReportsAccepted, 4)
    assertEquals(rootChoice(done.asInstanceOf[CompletedCommentaryJob]), root)
    val followed = done.continuationInquiries.find(_.question.prefixMoves == continuation).get
    assert(followed.searchWorkIds.contains(second.issuedWork.workId))
    assertEquals(done.responseSearches.count(_.anchor.prefixMoves == continuation), 1)
    val check = done.continuationInquiries.find(_.question.prefixMoves == checked).get
    assert(!check.premiseObservations.exists(_.observationState == "observed"),
      "another unobserved arm stays unresolved; sharing priority does not promise to close the whole pool")

  test("a submitted alias shares the existing tactical comparison and preserves its three-move bound"):
    val owner = najdorfOwner
    val source = Source("user", "inline-defense-alias")
    val alias = Request("defense-alias", Origin(source, source, source, source),
      List(CompareMoves("alias", najdorfGame.take(6), "b7c6", "e7f8")))
    val first = awaiting(begin(owner.currentFen, "b6b4", List(najdorfGame -> 200,
      List("d8d7", "c3d5") -> 0, List("b6c7") -> 300), prefixOwner = Some(owner), proposals = List(alias)))
    first.issuedWork.rootRestriction match
      case EngineRootRestriction.Restricted(moves) =>
        assertEquals(moves.toSet, Set("b7c6", "e7d8", "e7f8"))
        assertEquals(moves.size, 3)
      case _ => fail("the alias must share the bounded tactical comparison")
    val next = submit(first, List(List("b7c6", "e2a6") -> 200, List("e7d8") -> 220, List("e7f8") -> 230))
    val relevant = next.proposalEntries.flatMap(entry => entry.request.requirements.collect {
      case value: CompareMoves if value.prefixMovesUci == najdorfGame.take(6) &&
        Set(value.referenceMove, value.alternativeMove) == Set("b7c6", "e7f8") =>
        entry.readings.find(_.requirementId == value.id).get
    })
    assert(relevant.size >= 2)
    assert(relevant.forall(_.answer.exists(_.workId == first.issuedWork.workId)))
    assertEquals(next.responseSearches.count(_.anchor.prefixMoves == najdorfGame.take(6)), 1)
    next match
      case pending: AwaitingEngineWork => assertNotEquals(pending.issuedWork.searchAnchor.map(_.prefixMoves), Some(najdorfGame.take(6)))
      case _ => ()

  test("a current source choice is investigated without a capture or supplied tactical motif"):
    val owner = history("7k/8/8/8/8/8/8/R6K w - - 0 1")
    val lines = List(List("h1h2"), List("h1g1"), List("a1a2"))
    val found = read(owner, lines).filter(_.request.origin.question.producer == "C-opponent-choice")
    assertEquals(found.size, 2, "one reference has one pair per distinct candidate, not all candidate combinations")
    assert(found.forall(_.request.requirements.size == 4), "the choice does not require an invented tactical observation")
    val state = begin(owner.currentFen, "h1h2", List(List("h1h2") -> 10, List("h1g1") -> 30, List("a1a2") -> 20))
    val packet = state.observationsPacket.get
    val comparisons = CommentaryProposal.comparisonCandidates(packet, ObservedLineProtocol.prepare(packet), state.proposalEntries)
    assert(comparisons.exists(value => (value \ "selection").as[String] == "selected" &&
      Set((value \ "detail" \ "reference_move").as[String], (value \ "detail" \ "alternative_move").as[String]) == Set("h1g1", "h1h2")))

  test("Ruy Lopez Game 12 finds the central capture option without being told the preparatory goal"):
    // L'Ami–Van den Doel 2018, before 17 d5: book PDF 110, read in its whole-game context.
    val fen = "r2qrbk1/1bpn1p1p/p2p1np1/Pp2p3/3PP3/2P2NNP/1PB2PP1/R1BQR1K1 w - - 1 17"
    val prefix = ("e2e4 e7e5 g1f3 b8c6 f1b5 a7a6 b5a4 g8f6 e1g1 f8e7 f1e1 b7b5 a4b3 d7d6 c2c3 e8g8 " +
      "h2h3 c6b8 d2d4 b8d7 b1d2 c8b7 b3c2 f8e8 d2f1 e7f8 f1g3 g7g6 a2a4 d7b6 a4a5 b6d7").split(" ").toList
    val owner = history(chess.variant.Standard.initialFen.value).extend(prefix).toOption.get
    assertEquals(owner.currentFen, fen)
    val found = read(owner, List(List("d4d5"), List("b2b4")))
    val predicate = Predicate("legal_capture", Some(Piece("black", "pawn", "e5")), Some(Piece("white", "pawn", "d4")))
    val entry = found.find(entry => entry.request.origin.question.producer == "C-opponent-choice" &&
      entry.request.requirements.exists {
        case fact: Fact => fact.predicate == predicate
        case _ => false
      }).getOrElse(fail("17 d5 versus 17 b4 must ask about ...exd4 using the actual opposing turn"))
    def value(id: String) = entry.request.requirements.collectFirst {
      case fact: Fact if fact.id.startsWith(id) && fact.predicate == predicate =>
        entry.readings.find(_.requirementId == fact.id).get.result.get.samples.flatMap(_.value).toSet
    }.get
    assertEquals(value("opponent_reference"), Set("illegal"))
    assertEquals(value("opponent_alternative"), Set("legal"))
    assertEquals(entry.readings.find(_.requirementId == "opponent_context").get.state, "supported")
    assertEquals(entry.readings.find(_.requirementId == "contrast_opponent_choice").get.answer, None)
    assert(entry.questionText.get.contains("does not establish a threat or a loss"))
    assert((CommentaryProposal.snapshot(entry) \ "answer_text").as[String].contains("remain unverified"))
    assert(entry.request.requirements.collect { case fact: Fact => fact }.forall(_.at.horizon <= 1),
      "the current choice discovers this question without a future action or supplied goal")

  test("Najdorf Game 15 discovers the quiet double attack and keeps it separate from a forced gain"):
    // PDF 122–132, whole Game 15. Rd7–Nd5 is a legal counterfactual, not an author-provided verdict.
    val prefix = ("e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4 g8f6 b1c3 a7a6 f1c4 e7e6 a2a4 f8e7 e1g1 e8g8 " +
      "c1e3 b8c6 g1h1 c8d7 d1e2 a8c8 c4b3 c6a5 b3a2 d8c7 a1d1 a5c4 e3c1 f8d8 f2f4 d7e8 " +
      "d1d3 c7b6 b2b3 c4a5 c1b2 a5c6 d4c6 e8c6 d3g3").split(" ").toList
    val owner = history(chess.variant.Standard.initialFen.value).extend(prefix).toOption.get
    assertEquals(owner.currentFen, "2rr2k1/1p2bppp/pqbppn2/8/P3PP2/1PN3R1/BBP1Q1PP/5R1K b - - 1 21")
    val played = "b6b4"
    val game = "b6b4 c3d5 e6d5 e4d5 d8e8 d5c6 b7c6 e2a6".split(" ").toList
    val alternative = List("d8d7", "c3d5")
    val found = read(owner, List(game, alternative))
    val choices = found.filter(_.request.origin.question.producer == "C-opponent-choice")
    assertEquals(choices.size, 1, "one current pair owns its capture and quiet-reply observations")
    val entry = choices.head
    assertEquals(entry.request.requirements.count(_.isInstanceOf[CompareMoves]), 1)
    val targets = entry.request.requirements.collect {
      case fact: Fact if fact.id.startsWith("reply_reference_") && fact.id.endsWith("_target") => fact.predicate.subject.get
    }.toSet
    assertEquals(targets, Set(Piece("black", "queen", "b6"), Piece("black", "bishop", "e7"), Piece("black", "knight", "f6")))
    assert(entry.readings.filter(reading => reading.requirementId.startsWith("reply_reference_") &&
      !reading.requirementId.endsWith("_before")).forall(_.state == "supported"))
    assertEquals(entry.readings.find(_.requirementId == "contrast_opponent_choice").get.answer, None)

    // Synthetic scores exercise normal delivery, not the book's evaluation or tactical soundness.
    val state = begin(owner.currentFen, played, List(game -> 100, alternative -> 0, List("b6c7") -> 200), prefixOwner = Some(owner))
    def comparisonText(state: CommentaryJobState): String =
      val response = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("q" * 32, "quiet-reply", 10000L, state))
      (response \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
        .filter(value => (value \ "detail" \ "type").asOpt[String].contains("question_comparison"))
        .flatMap(value => (value \ "detail" \ "text").asOpt[String]).mkString(" ")
    val text = comparisonText(state)
    assert(text.contains("c3d5") && text.contains("simultaneously attacks"), text)
    assert(text.contains("queen originally on b6") && text.contains("bishop originally on e7"), text)
    assert(text.contains("does not establish a forced gain"), text)
    val changed = begin(owner.currentFen, played, List(List(played, "g3h3") -> 100, alternative -> 0, List("b6c7") -> 200),
      supplied = List(game), prefixOwner = Some(owner))
    assert(!comparisonText(changed).contains(s"After $played, the returned reply c3d5"),
      "a historical fork must not remain the explanation when the current compared line chooses another reply")
    assert(comparisonText(changed).contains("After d8d7, the returned reply c3d5"),
      "the other arm's still-current observation remains available")

  test("Najdorf exchanges connect the new attackers and both departures that open the e-file"):
    val owner = najdorfOwner
    val defense = najdorfGame.take(6) :+ "e7f8"
    val found = read(owner, List(najdorfGame, List("d8d7", "c3d5"), defense))
    val choice = found.find(_.request.origin.question.producer == "C-opponent-choice").get
    val transition = choice.request.requirements.collect {
      case fact: Fact if fact.id.startsWith("tactic_") && fact.path.source == Declared(najdorfGame.take(4)) => fact
    }
    assert(transition.nonEmpty)
    assert(transition.forall(fact => choice.readings.find(_.requirementId == fact.id).get.state == "supported"))
    assert(transition.exists(fact => fact.predicate == Predicate("control", Some(Piece("white", "pawn", "e4")), square = Some("c6")) &&
      fact.expected.contains("present")))
    assert(transition.exists(fact => fact.predicate == Predicate("control", Some(Piece("white", "queen", "e2")), square = Some("e7")) &&
      fact.expected.contains("present")))
    val departures = transition.filter(_.id.endsWith("_left"))
    assertEquals(departures.flatMap(_.predicate.subject).toSet, Set(Piece("black", "pawn", "e6"), Piece("white", "pawn", "e4")))
    assertEquals(departures.map(_.at.horizon).toSet, Set(3, 4))
    assert(transition.exists(fact => fact.id.endsWith("_exchange") && fact.predicate.subject.contains(Piece("white", "knight", "c3"))))
    val opened = choice.request.requirements.collect {
      case fact: Fact if fact.id.startsWith("tactic_") && fact.path.source == Declared(defense) => fact
    }
    assert(opened.exists(fact => fact.predicate == Predicate("control", Some(Piece("black", "rook", "d8")), square = Some("e2")) &&
      fact.expected.contains("present")))
    assertEquals(opened.filter(_.id.endsWith("_left")).flatMap(_.predicate.subject).toSet, Set(Piece("black", "bishop", "e7")),
      "the captured c6 bishop did not open the rook's e-file")

    val pending = awaiting(begin(owner.currentFen, "b6b4", List(najdorfGame -> 200, List("d8d7", "c3d5") -> 0, List("b6c7") -> 300),
      prefixOwner = Some(owner)))
    val text = deliveredComparisons(pending)
    assert(text.contains("white knight originally on c3") && text.contains("removes"), text)
    assert(text.contains("white pawn originally on e4 attacks the black bishop originally on c6"), text)
    assert(text.contains("white queen originally on e2 attacks the black bishop originally on e7"), text)
    assert(text.contains("leaves e6 with e6d5") && text.contains("leaves e4 with e4d5"), text)
    assert(text.contains("do not by themselves establish a forced gain"), text)
    assertEquals(pending.issuedWork.searchAnchor.map(_.prefixMoves), Some(najdorfGame.take(6)),
      "the remaining bishop defense is discovered without supplying Bf8 as a source")
    assertEquals(pending.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("b7c6", "e7d8", "e7f8")))
    assert(text.contains("check e7f8 instead of b7c6"), text)

    val changed = begin(owner.currentFen, "b6b4", List(List("b6b4", "g3h3") -> 200, List("d8d7", "c3d5") -> 0, List("b6c7") -> 300),
      supplied = List(najdorfGame, defense), prefixOwner = Some(owner))
    assert(!deliveredComparisons(changed).contains("leaves e6 with e6d5"), "a historical exchange cannot explain the changed current branch")
    assert(!deliveredComparisons(changed).contains("check e7f8 instead of b7c6"), "a counter to the withdrawn prefix is not a current decision")

  test("a stronger conditional defense requires root verification and cannot become the root score"):
    val owner = najdorfOwner
    val pending = awaiting(begin(owner.currentFen, "b6b4", List(najdorfGame -> 200, List("d8d7", "c3d5") -> 0, List("b6c7") -> 300),
      prefixOwner = Some(owner)))
    assertEquals(pending.issuedWork.searchAnchor.map(_.prefixMoves), Some(najdorfGame.take(6)))
    val defense = "e7f8 g3e3 e8e3 e2e3 c8e8 e3d3 b7c6 b2f6 g7f6 c2c4".split(" ").toList
    val after = awaiting(submit(pending, List(defense -> -100, List("b7c6", "e2a6") -> 200, List("e7d8") -> 180)))
    assertEquals(after.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(after.verification.map(_.reason), Some("child_challenge"))
    assertEquals(after.verification.flatMap(_.completedDepth), Some(16))
    assert(after.responseSearches.exists(_.work.workId == pending.issuedWork.workId))
    assertEquals(after.assessment.map(_.result), pending.assessment.map(_.result), "a child score cannot replace the root judgment")
    val entry = after.proposalEntries.find(entry => entry.request.origin.question.producer == "C-tactical-counter" &&
      entry.request.requirements.exists {
        case value: CompareMoves => value.id == "tactical_strength" && value.alternativeMove == "e7f8"
        case _ => false
      }).get
    val reading = entry.readings.find(_.requirementId == "tactical_strength").get
    assertEquals(reading.answer, None)
    assertEquals(reading.stopReason, Some("comparison_not_current"))
    val search = after.responseSearches.find(_.work.workId == pending.issuedWork.workId).get
    val answer = search.pairAnswer(after.observationsPacket.get, "b7c6", "e7f8").get
    assertEquals(answer.evaluationRelation, "better", "the conditional answer remains with its exact owned report")
    assert(!deliveredComparisons(after).contains("e7f8 is stronger than b7c6"), "deeper verification must not present an obsolete answer as current")
    val comparison = entry.request.requirements.collectFirst { case value: CompareMoves if value.id == "tactical_strength" => value }.get
    val answered = entry.copy(readings = entry.readings.map(value =>
      if value.requirementId == comparison.id then pairReading(comparison, answer) else value))
    val challenge = CommentaryComparisonConflict.unansweredChallenge(after.observationsPacket.get, List(search), List(answered))
    assertEquals(challenge.map(_.prefix), Some(najdorfGame.take(6)))
    assertEquals(challenge.map(_.move), Some("b7c6"), "a root line keeping the disproved reply does not settle this counter")
    val refreshed = submit(after, List((najdorfGame.take(6) ++ defense) -> 100,
      List("d8d7", "c3d5") -> 0, List("b6c7") -> 300))
    val withdrawn = refreshed.proposalEntries.find(_.request.id == entry.request.id).get
    assert(withdrawn.readings.filter(_.requirementId.startsWith("tactical_current_")).exists(_.result.exists(_.reason.contains("event_absent"))))
    val response = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("q" * 32, "changed-reply", 10000L, refreshed))
    assert((response \ "observations" \ "selection" \ "proposals").as[List[JsObject]].exists(value =>
      (value \ "id").as[String] == entry.request.id && (value \ "answer_text").as[String].contains("Withdraw this earlier reply explanation")))
    assert(!deliveredComparisons(refreshed).contains("check e7f8 instead of b7c6"),
      "once the root selects Bf8, the earlier b7c6 explanation must not stay current merely because its prefix still matches")

  test("tactical probes follow the current capture and do not spend replay beyond admission room"):
    val owner = najdorfOwner
    val delayed = "b6b4 c3d5 e6d5 e4d5 d8e8 g3h3 b4b6 d5c6 b7c6 e2a6".split(" ").toList
    val rows = List(delayed -> 200, List("d8d7", "c3d5") -> 0, List("b6c7") -> 300)
    val root = CommentaryJobReducer.startJob(owner.initialFen, owner.movePrefixUci, owner.currentFen, "b6b4",
      owner.extend(List("b6b4")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L,
      inputLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, najdorfGame)))
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    val preview = submit(root, rows)
    assert(!preview.proposalEntries.exists(_.admission == "generated"))
    val packet = preview.observationsPacket.get
    val source = Source("generator", "inline-current-decisions")
    val sources = (rows.map(_._1) :+ najdorfGame).map(CommentaryDiscovery.SourcePath(_, source))
    def discover(previous: List[Entry]) = CommentaryDiscovery.discover(packet, ObservedLineProtocol.prepare(packet),
      List(CommentaryDiscovery.Anchor(Nil, "b6b4")), sources, previous, pvGuided = true)
    val pending = discover(Nil)
    assert(pending.replayNeeds.contains(delayed.take(8) :+ "e7f8"),
      "the shorter historical capture cannot hide the current capture's defense")
    assert(!pending.replayNeeds.exists(_.startsWith(najdorfGame.take(6))),
      "historical decisions do not consume the current tactical replay budget")
    val full = entries(List(Request("full-tactical-budget", Origin(source, source, source, source),
      (1 until MaximumRequirements).map(n => Uninterpreted(s"held_$n", "existing question")).toList)))
    val bounded = discover(full)
    assertEquals(bounded.replayNeeds, Nil)
    assert(bounded.scans.exists(_.reason.contains("requirement_limit")))

    val changedReply = najdorfGame.take(6) ++ List("c8c6", "e2a6")
    val state = awaiting(begin(owner.currentFen, "b6b4", rows.updated(0, changedReply -> 200),
      supplied = List(najdorfGame), prefixOwner = Some(owner)))
    assertEquals(state.issuedWork.searchAnchor.map(_.prefixMoves), Some(najdorfGame.take(6)))
    assertEquals(state.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("c8c6", "e7d8", "e7f8")),
      "the current reply owns the comparison even if an older reply sorts first")

  test("a tactical decision retains two direct replies and bounded distinct conditions after their answers settle"):
    // A synthetic rook on e7 gives more retreats than the book's bishop. This
    // checks a scheduling bound, not the strength of the sampled defenses.
    val fen = "2rr2k1/1p2rppp/pqbppn2/8/P3PP2/1PN3R1/BBP1Q1PP/5R1K b - - 1 21"
    val rows = List(najdorfGame -> 200, List("d8d7", "c3d5") -> 0, List("b6c7") -> 300)
    val state = begin(fen, "b6b4", rows)
    val packet = state.observationsPacket.get
    val questions = state.proposalEntries.flatMap(_.request.requirements.collect {
      case value: CompareMoves if value.id == "tactical_strength" && value.prefixMovesUci == najdorfGame.take(6) => value
    })
    val selected = questions.map(_.alternativeMove).toSet
    assertEquals(selected, Set("e7c7", "e7d7", "b4e4"), "one extra move covers both the blocking and support condition")
    assertEquals(awaiting(state).issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("b7c6", "e7c7", "e7d7")),
      "new conditions cannot displace the existing two direct replies in the first work")
    // Resolution releases the frontier but must not reset this decision's width.
    val settled = state.proposalEntries.map(entry => entry.copy(readings = entry.readings.map(reading =>
      reading.copy(state = "observed", stopReason = None))))
    val batch = CommentaryDiscovery.discover(packet, ObservedLineProtocol.prepare(packet),
      List(CommentaryDiscovery.Anchor(Nil, "b6b4")),
      rows.map((moves, _) => CommentaryDiscovery.SourcePath(moves, Source("generator", "inline-defense-width"))),
      settled, pvGuided = true)
    assert(!batch.replayNeeds.exists(_.startsWith(najdorfGame.take(6))),
      "settled or non-contrasting probes must not open another pair of retreats at this decision")
    assert(batch.additions.flatMap(_.request.requirements.collect {
      case value: CompareMoves if value.id == "tactical_strength" && value.prefixMovesUci == najdorfGame.take(6) => value.alternativeMove
    }).forall(selected))

  test("geometric support of a checked king is not a recapture defense"):
    val owner = history("7k/8/8/8/5n2/8/8/R3K3 w Q - 0 1")
    val found = read(owner, List(List("a1b1", "f4d3"), List("a1a2", "f4d3")))
    assert(!found.flatMap(_.request.requirements).exists {
      case value: Fact => value.id.startsWith("defense_") && value.predicate.target.exists(_.role == "king")
      case _ => false
    })

  test("adding a defender is investigated while the original capture remains legal"):
    // Constructed mechanism case: remove the black queen from the exposed position,
    // keeping the exchange and the two remaining bishop targets. No engine verdict is implied.
    val fen = "2rr2k1/1p2bppp/p1bppn2/8/P3PP2/1PN3R1/BBP1Q1PP/5R1K b - - 1 21"
    val line = "g8h8 c3d5 e6d5 e4d5 c8b8 d5c6 b7c6 e2e7".split(" ").toList
    val state = begin(fen, "g8h8", List(line -> 100, List("g8f8") -> 0, List("d8d7") -> 200))
    val entry = state.proposalEntries.find(entry => entry.request.requirements.exists {
      case value: CompareMoves => value.id == "tactical_strength" && value.prefixMovesUci == line.take(6) && value.alternativeMove == "d8d7"
      case _ => false
    }).getOrElse(fail("a new rook defender must survive alongside direct bishop retreats"))
    val support = Predicate("support", Some(Piece("black", "rook", "d8")), Some(Piece("black", "bishop", "e7")))
    val facts = entry.request.requirements.collect { case fact: Fact => fact }
    assertEquals(facts.filter(_.predicate == support).flatMap(_.expected).toSet, Set("absent", "present"))
    val capture = Predicate("legal_capture", Some(Piece("white", "queen", "e2")), Some(Piece("black", "bishop", "e7")))
    assertEquals(facts.filter(_.predicate == capture).flatMap(_.expected), List("legal", "legal"))
    assertEquals(entry.readings.find(_.requirementId == "tactical_context").get.state, "supported")
    val text = deliveredComparisons(state)
    assert(text.contains("support by the black rook originally on d8") && text.contains("remains legal in both replies"), text)
    assert(text.contains("relative strength of these replies remains unverified"), text)

  test("a checking counter is discovered without supplying it as a continuation"):
    // Same local exchange mechanism, with a constructed white king on h2:
    // ...Ng4+ answers a different condition from moving the attacked bishop.
    val fen = "2rr2k1/1p2bppp/pqbppn2/8/P3PP2/1PN3RP/BBP1Q1PK/5R2 b - - 1 21"
    val state = begin(fen, "b6b4", List(najdorfGame -> 200, List("d8d7", "c3d5") -> 0, List("b6c7") -> 300))
    val entry = state.proposalEntries.find(_.request.requirements.exists {
      case value: CompareMoves => value.id == "tactical_strength" && value.alternativeMove == "f6g4" && value.prefixMovesUci == najdorfGame.take(6)
      case _ => false
    }).getOrElse(fail("the intermediate check must be sampled in addition to the direct retreats"))
    val facts = entry.request.requirements.collect { case fact: Fact if fact.predicate.kind == "check" => fact }
    assertEquals(facts.flatMap(_.expected).toSet, Set("absent", "present"))
    assertEquals(entry.readings.find(_.requirementId == "tactical_context").get.state, "supported")
    assert(deliveredComparisons(state).contains("check against white changes from absent to present"))

  test("a defensive line opening admits interposition and withdraws its old reply when the PV changes"):
    val owner = najdorfOwner
    val prefix = najdorfGame.take(6) :+ "e7f8"
    val current = prefix ++ List("c6b7", "b4b7", "e2d1")
    val blocked = prefix ++ List("g3e3", "e8e3", "e2e3")
    val rows = List(current -> 100, List("d8d7", "c3d5") -> 0, List("b6c7") -> 200)
    val state = begin(owner.currentFen, "b6b4", rows, supplied = List(blocked, prefix :+ "b2f6"), prefixOwner = Some(owner))
    val entry = state.proposalEntries.find(_.request.requirements.exists {
      case value: CompareMoves => value.id == "tactical_strength" && value.prefixMovesUci == prefix && value.alternativeMove == "g3e3"
      case _ => false
    }).getOrElse(fail("Re3 should test the newly opened Re8–Qe2 attack even without a compound capture at this decision"))
    assertEquals(entry.readings.find(_.requirementId == "tactical_context").get.state, "supported")
    assert(deliveredComparisons(state).contains("check g3e3 instead of c6b7"))
    val countercapture = state.proposalEntries.find(_.request.requirements.exists {
      case value: CompareMoves => value.id == "tactical_strength" && value.prefixMovesUci == prefix && value.alternativeMove == "b2f6"
      case _ => false
    }).getOrElse(fail("a capture elsewhere tests a different condition from interposing against the queen attack"))
    assert(countercapture.request.requirements.exists {
      case fact: Fact => fact.predicate == Predicate("presence", Some(Piece("black", "knight", "f6"))) && fact.expected.contains("removed")
      case _ => false
    })
    val changed = begin(owner.currentFen, "b6b4", rows.updated(0, blocked -> 100),
      supplied = List(current), prefixOwner = Some(owner))
    assert(!deliveredComparisons(changed).contains("check g3e3 instead of c6b7"))

  test("weaker and equal replies keep matched consequences within the exact returned horizon"):
    val owner = najdorfOwner
    // Scores only exercise weaker/equal receipt handling. The book's Bf8 is not
    // claimed weaker, and a returned line is not evidence that every reply is forced.
    val defense = "e7f8 g3e3 e8e3 e2e3 c8e8 e3d3 b7c6 b2f6 g7f6 c2c4".split(" ").toList
    List((230, "worse", defense), (200, "equal", defense), (230, "worse", defense.take(3))).foreach { (score, relation, returned) =>
      val pending = awaiting(begin(owner.currentFen, "b6b4", List(najdorfGame -> 200, List("d8d7", "c3d5") -> 0, List("b6c7") -> 300),
        supplied = List(najdorfGame.take(6) ++ defense), prefixOwner = Some(owner)))
      assertEquals(pending.issuedWork.searchAnchor.map(_.prefixMoves), Some(najdorfGame.take(6)))
      val requested = pending.issuedWork.rootRestriction.asInstanceOf[EngineRootRestriction.Restricted].movesUci
      assert(Set("b7c6", "e7f8").subsetOf(requested.toSet))
      val after = submit(pending, requested.map {
        case "b7c6" => List("b7c6", "e2a6") -> 200
        case "e7f8" => returned -> score
        case other => List(other) -> 220
      })
      val entry = after.proposalEntries.find(_.request.requirements.exists {
        case value: CompareMoves => value.id == "tactical_strength" && value.alternativeMove == "e7f8"
        case _ => false
      }).get
      assertEquals(entry.readings.find(_.requirementId == "tactical_strength").flatMap(_.answer).map(_.evaluationRelation), Some(relation),
        s"returned=${returned.size}, readings=${entry.readings.map(reading => (reading.requirementId, reading.state, reading.stopReason))}, verification=${after.verification}")
      val text = deliveredComparisons(after)
      assert(text.contains(s"e7f8 is ${if relation == "equal" then "equal to" else "weaker than"} b7c6"), text)
      assert(text.contains("whether the same explanation still applies depends on its recorded continuation"), text)
      assert(text.contains("beginning e7f8") && text.contains("beginning b7c6"), text)
      assert(text.contains("white loses 1 rook"), text)
      assertEquals(text.contains("black loses 1 rook"), returned.size == defense.size,
        "the longer study line cannot donate a recapture to the answer's shorter engine line")
      assertEquals(text.contains("the capturer on e8e3 is itself captured on e2e3"), returned.size == defense.size)
      assert(text.contains("not a forced result or the cause of the entire root evaluation"), text)
    }

  test("the same Nd5 distinguishes defensive support and its actual recapture use"):
    val owner = najdorfOwner
    // Legal mechanism contrast supplied by this study; no author or engine verdict is invented.
    val bare = "b6b4 c3d5 e6d5 e4d5 c6d5 e2e7".split(" ").toList
    val defended = "d8d7 c3d5 e6d5 e4d5 c6d5 e2e7 d7e7".split(" ").toList
    val state = begin(owner.currentFen, "b6b4", List(bare -> 100, defended -> 0, List("b6c7") -> 200), prefixOwner = Some(owner))
    val text = deliveredComparisons(state)
    assert(text.contains("Against the common reply c3d5"), text)
    assert(text.contains("black rook originally on d8") && text.contains("black bishop originally on e7"), text)
    assert(text.contains("actually uses that defense to recapture the attacker"), text)
    assert(text.contains("does not make the common reply illegal"), text)
    val beforeUse = begin(owner.currentFen, "b6b4", List(bare.take(2) -> 100, defended.take(2) -> 0, List("b6c7") -> 200),
      supplied = List(bare, defended), prefixOwner = Some(owner))
    val currentText = deliveredComparisons(beforeUse)
    assert(currentText.contains("Use of this support in a recapture has not yet been established"), currentText)
    assert(!currentText.contains("actually uses that defense to recapture the attacker"),
      "a historical recapture is not use in the current compared continuation")

    val interrupted = "d8d7 c3d5 e6d5 e4d5 d7d8 c2c4 d8e8 e2e7 e8e7".split(" ").toList
    val recovered = begin(owner.currentFen, "b6b4", List(bare -> 100, interrupted -> 0, List("b6c7") -> 200), prefixOwner = Some(owner))
    val recoveredText = deliveredComparisons(recovered)
    assert(recoveredText.contains("Against the common reply c3d5"), recoveredText)
    assert(!recoveredText.contains("actually uses that defense to recapture the attacker"),
      "support broken by Rd8 and recreated by Re8 is not the continuous benefit of Rd7")

    val otherArm = "b6b4 c3d5 e6d5 e4d5 d8e8 e2e7 e8e7".split(" ").toList
    val recreated = begin(owner.currentFen, "b6b4", List(otherArm -> 100, defended.take(2) -> 0, List("b6c7") -> 200), prefixOwner = Some(owner))
    assert(!deliveredComparisons(recreated).contains("actually uses that defense to recapture the attacker"),
      "a later recapture in the Qb4 arm does not use the Rd7 arm's initial support")

    val previous = read(owner, List(bare, defended))
    val refreshed = read(owner, List(bare, defended, interrupted), previous)
    val entry = refreshed.find(_.request.origin.question.producer == "C-opponent-choice").get
    val prior = previous.find(_.request.id == entry.request.id).get
    assert(prior.request.requirements.forall(value => entry.request.requirements.contains(value)),
      "later uses get new time-specific IDs; an existing question never changes its event range")
    assert(entry.request.requirements.exists {
      case fact: Fact => fact.id.startsWith("defense_") && fact.predicate.kind == "direct_support_use" && fact.at.horizon == 9
      case _ => false
    })

  test("line opening uses the last real exit and includes a capture's replacement blocker"):
    val cases = List(
      ("q6k/8/8/8/8/8/R6K/R7 w - - 0 1", "a2a3 h8g8 a3b3", Set((Piece("white", "rook", "a2"), "a3", 3))),
      ("q6k/8/8/8/8/8/R6K/R7 w - - 0 1", "a2b2 h8g8 b2a2 g8h8 a2c2", Set((Piece("white", "rook", "a2"), "a2", 5))),
      ("q6k/8/8/r7/8/2B5/7K/R7 w - - 0 1", "c3a5 h8g8 a5c7",
        Set((Piece("black", "rook", "a5"), "a5", 1), (Piece("white", "bishop", "c3"), "a5", 3))))
    cases.foreach { (fen, moves, expected) =>
      val line = moves.split(" ").toList
      val entries = read(history(fen), List(line, List("h2g1")))
      val entry = entries.find(_.request.origin.question.producer == "C-opponent-choice").get
      val facts = entry.request.requirements.collect {
        case fact: Fact if fact.id.startsWith("tactic_") && fact.path.source == Declared(line) => fact
      }
      val exits = facts.filter(_.id.endsWith("_left"))
      assert(exits.nonEmpty)
      assertEquals(exits.map(fact => (fact.predicate.subject.get,
        facts.find(_.id == fact.id.stripSuffix("_left") + "_before").get.expected.get, fact.at.horizon)).toSet, expected)
      assert(exits.forall(fact => entry.readings.find(_.requirementId == fact.id).get.state == "supported"))
      val state = begin(fen, line.head, List(line -> 30, List("h2g1") -> 10, List("h2g3") -> 0))
      val text = deliveredComparisons(state)
      if moves.startsWith("a2a3") then
        assert(text.contains("leaves a3 with a3b3"), text)
        assert(!text.contains("leaves a2 with a2a3"), text)
      if moves.startsWith("c3a5") then
        assert(text.contains("black rook originally on a5 on a5 is removed by c3a5"), text)
        assert(text.contains("white bishop originally on c3 leaves a5 with a5c7"), text)
    }

  test("losing a recapture is a concession, even when the tested whole line prefers the preparation"):
    val fen = "6k1/8/5p1p/6p1/8/4B3/8/K7 b - - 0 1"
    val plan = List("h6h5", "a1a2", "f6f5")
    val state = begin(fen, "h6h5", List(plan -> -50, List("f6f5") -> -30, List("g8f8") -> -10),
      List(plan :+ "e3g5", List("f6f5", "e3g5", "h6g5")))
    val idea = CommentaryIdea.read(state.proposalEntries).find(idea => !idea.key.loss && idea.key.branch == plan).get
    val lost = idea.members.find(_.entry.request.requirements.exists {
      case fact: Fact => fact.id == "reference_response" && fact.predicate.subject.contains(Piece("black", "pawn", "h6"))
      case _ => false
    }).get
    assertEquals(lost.values("reference_response"), List("illegal"))
    assertEquals(lost.values(lost.requirements.keys.find(_.startsWith("alternative_response")).get), List("legal"))
    assert(!lost.mechanism, "the changed response is a lost option, not a preparatory benefit")
    assert(idea.text.contains("gives up the recapture"), idea.text)
    assert(!idea.text.contains("may help prepare"), idea.text)
    val packet = state.observationsPacket.get
    val comparison = CommentaryProposal.comparisonCandidates(packet, ObservedLineProtocol.prepare(packet), state.proposalEntries)
      .map(value => (value \ "detail" \ "text").as[String]).find(_.contains("gives up the recapture")).get
    assert(comparison.indexOf("gives up the recapture") < comparison.indexOf("The observed line"),
      "the actual comparison explains the concession before the conditional idea")

  test("a changed king location plus a favorable score does not establish a preparation mechanism"):
    val fen = "7k/8/8/8/8/n7/8/R6K w - - 0 1"
    val plan = List("h1h2", "h8g8", "a1a3")
    val state = begin(fen, "h1h2", List(plan -> 30, List("a1a3") -> 20, List("h1g1") -> 10))
    val entry = state.proposalEntries.find(entry => entry.request.origin.question.producer == "C-preparation-effect" &&
      entry.request.requirements.exists {
        case value: CompareMoves => value.referenceMove == "h1h2" && value.alternativeMove == "a1a3"
        case _ => false
      }).get
    val idea = CommentaryIdea.read(List(entry)).head
    assert(idea.contribution, "the king really moved in this line")
    assert(idea.tracking.forall(_.currentConfirmed), "the current returned line really contains the capture")
    assert(idea.essential.nonEmpty && idea.essential.forall(_._3.answer.nonEmpty), "the comparison is answered")
    assert(!idea.mechanism)
    assertEquals(idea.state, "candidate")
    assert(idea.text.contains("have not yet established how this move helps"))

  test("moving the action's knight away is not evidence that an unrelated king move enables it"):
    val fen = "k3r3/7p/8/6b1/8/2p2N2/4R3/4K3 w - - 0 1"
    val plan = List("e1f1", "h7h6", "f3d2")
    val found = read(history(fen), List(plan, List("f3h4", "h7h6")))
    assert(!found.exists(entry => entry.request.origin.question.producer == "C-preparation-alternative" &&
      entry.request.requirements.exists {
        case value: CompareMoves => value.referenceMove == "e1f1" && value.alternativeMove == "f3h4"
        case _ => false
      }), "disabling Nd2 by moving its knight is not a controlled explanation of Kf1's contribution")

  test("a second visit is not kept current by the first visit in a different returned line"):
    val fen = "7k/8/8/8/8/8/8/R6K w - - 0 1"
    val plan = "h1h2 h8g8 a1a3 g8f8 a3a1 f8g8 a1a3".split(" ").toList
    val current = "h1h2 h8g8 a1a3 g8f8 a3f3 f8g8 f3f4".split(" ").toList
    var published = List.empty[Entry]
    // Advance the existing discovery frontier only; these completion markers are not scores.
    (0 until 3).foreach { _ =>
      published = read(history(fen), List(plan, List("a1a3")), published).map(entry =>
        entry.copy(readings = entry.readings.map(_.copy(state = "supported"))))
    }
    val entry = published.find(entry => entry.request.origin.question.producer == "C-preparation-effect" &&
      entry.request.requirements.exists {
        case fact: Fact => fact.id == "reference_action" && fact.path.source == Declared(plan)
        case _ => false
      }).get
    val tracking = entry.request.requirements.collectFirst {
      case fact @ Fact(_, Path(_, Comparison("contrast_immediate", "reference"), _), _, _: Event, _, _) => fact
    }.get
    assertEquals(tracking.at.asInstanceOf[Event].ordinal, 2)
    val state = begin(fen, "h1h2", List(current -> 30, List("a1a3") -> 20, List("h1g1") -> 10), List(plan))
    val answer = state.proposalEntries.flatMap(_.readings).find(reading =>
      reading.requirementId == "contrast_immediate" && reading.answer.nonEmpty).get.answer.get
    val bound = entry.copy(readings = entry.readings.map(reading =>
      if reading.requirementId == "contrast_immediate" then pairReading(
        entry.request.requirements.collectFirst { case value: CompareMoves if value.id == reading.requirementId => value }.get, answer)
      else reading.copy(state = "deferred", result = None)))
    val packet = state.observationsPacket.get
    val reread = CommentaryFactReader.read(packet, ObservedLineProtocol.prepare(packet), List(bound)).head
    assert(reread.readings.find(_.requirementId == tracking.id).get.result.exists(_.reason.contains("event_absent")))
    assert(CommentaryIdea.read(List(reread)).head.withdrawn)

  test("quiet reply observations exclude promotion role changes"):
    val fen = "7k/6N1/8/8/8/8/p7/1R5K w - - 0 1"
    val found = read(history(fen), List(List("h1h2", "a2a1q"), List("h1g1", "a2a1n")))
    assert(!found.flatMap(_.request.requirements).exists(_.id.startsWith("reply_")),
      "same physical pawn and destination do not identify the same promotion or its attacks")

  test("long comparison explanations keep the decision and whole observations within the receiver limit"):
    val decision = "At this position, the tested alternative is stronger."
    val observations = (1 to 30).map(n => s"Observation $n: " + "named conditional capture " * 12 + ".").toList
    val text = CommentaryProposal.comparisonText(decision :: observations)
    assert(text.startsWith(decision))
    assert(text.length <= 4000)
    assert(text.contains("further explanations remain in the linked questions."))
    observations.foreach(observation =>
      assert(!text.contains(observation.take(16)) || text.contains(observation), "never cut a claim or its qualification in half"))

  test("a tight requirement budget keeps one complete opposing option and later extends the same pair"):
    val owner = history("7k/8/8/8/8/n7/2P5/RN5K w - - 0 1")
    val lines = List(List("h1h2"), List("a1a3"))
    val input = MoveReviewInputAdmission.admitIndependent(owner, lines.head.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    val source = Source("author", "inline-budget-case")
    val origin = Origin(source, source, source, source)
    val filler = entries(List(Request("existing-budget", origin,
      (1 to (MaximumRequirements - 9)).map(n => Uninterpreted(s"prior_$n", "existing unrelated question")).toList)))
    def discover(previous: List[Entry]) = CommentaryDiscovery.discover(packet, prepared,
      List(CommentaryDiscovery.Anchor(Nil, "h1h2")), lines.map(CommentaryDiscovery.SourcePath(_, source)), previous, pvGuided = true)
    val first = discover(filler)
    val choice = first.additions.find(_.request.origin.question.producer == "C-opponent-choice").get
    assertEquals(choice.request.requirements.count(_.isInstanceOf[CompareMoves]), 1)
    assertEquals(choice.request.requirements.count(_.isInstanceOf[FactComparison]), 1)
    assert(first.additions.map(_.request.requirements.size).sum <= 8)
    assert(first.scans.exists(scan => scan.reason.contains("requirement_limit") && scan.deferredRequestIds.nonEmpty))
    assert(first.scans.flatMap(_.deferredRequestIds).forall(id => id.length <= 128 && RuntimeProtocol.acceptsRequestId(id)),
      "deferred capture observations obey the receiver's ID limit too")
    val extended = discover(List(choice)).additions.find(_.request.id == choice.request.id).get
    assertEquals(extended.request.requirements.count(_.isInstanceOf[CompareMoves]), 1)
    assertEquals(extended.request.requirements.count(_.isInstanceOf[FactComparison]), 2)
    assert(choice.request.requirements.forall(extended.request.requirements.contains))
    assert(validRequests(List(extended.request)))
    val tight = entries(List(Request("bare-choice-budget", origin,
      (1 to (MaximumRequirements - 5)).map(n => Uninterpreted(s"prior_$n", "existing unrelated question")).toList)))
    val bare = discover(tight).additions.find(_.request.origin.question.producer == "C-opponent-choice").get
    assertEquals(bare.request.requirements.size, 4, "the legal current choice survives even when no observation bundle fits")

  test("a stronger current choice revises a locally favorable plan and leads the delivered comparisons"):
    val fen = "6k1/7p/5p2/6p1/8/4B3/8/K7 b - - 0 1"
    val plan = List("h7h6", "a1a2", "f6f5")
    val state = begin(fen, "h7h6", List(List("g5g4") -> -200, plan -> -100, List("f6f5") -> 100),
      List(plan ++ List("e3g5", "h6g5"), List("f6f5", "e3g5")))
    val idea = CommentaryIdea.read(state.proposalEntries).find(idea =>
      idea.key.move == "h7h6" && idea.key.branch == plan).get
    assertEquals(idea.strongerChoices, List("g5g4"))
    assertEquals(idea.state, "revised")
    assert(idea.text.startsWith("Revise this idea"))
    assert(idea.text.indexOf("g5g4 is stronger") < idea.text.indexOf("may help prepare"))
    val packet = state.observationsPacket.get
    val candidates = CommentaryProposal.comparisonCandidates(packet, ObservedLineProtocol.prepare(packet), state.proposalEntries)
    val first = candidates.find(candidate => (candidate \ "selection").as[String] == "selected").get
    assertEquals((first \ "detail" \ "prefix_moves_uci").as[List[String]], Nil)
    assertEquals(Set((first \ "detail" \ "reference_move").as[String], (first \ "detail" \ "alternative_move").as[String]),
      Set("h7h6", "g5g4"))
    assert((first \ "detail" \ "text").as[String].contains("identifies an opposing option"),
      "the selected comparison must explain the changed choice as well as its score")
    assertEquals(CommentaryProposal.comparisonCandidates(packet, ObservedLineProtocol.prepare(packet), state.proposalEntries.reverse)
      .map(candidate => (candidate \ "id").as[String]), candidates.map(candidate => (candidate \ "id").as[String]))
    val changed = begin(fen, "h7h6", List(List("g5g4") -> -200,
      List("h7h6", "a1a2", "g8f8") -> -100, List("f6f5") -> 100), List(plan))
    assert(CommentaryIdea.read(changed.proposalEntries).exists(idea => idea.key.branch == plan && idea.withdrawn))
    val changedPacket = changed.observationsPacket.get
    val changedCandidates = CommentaryProposal.comparisonCandidates(changedPacket, ObservedLineProtocol.prepare(changedPacket), changed.proposalEntries)
    assert(changedCandidates.exists(candidate => (candidate \ "selection").as[String] == "selected" &&
      Set((candidate \ "detail" \ "reference_move").as[String], (candidate \ "detail" \ "alternative_move").as[String]) == Set("h7h6", "g5g4")),
      "withdrawing the old plan must not hide a still answered current opposing-option comparison")

  test("a tested reply can change the capturing piece without silently changing the physical target"):
    List(false, true).foreach { differentTarget =>
      val fen = if differentTarget then "7k/8/8/8/2p5/n7/8/R4B1K w - - 0 1"
        else "7k/8/8/8/8/n7/8/R4B1K w - - 0 1"
      val plan = List("h1h2", "h8g8", "a1a3")
      val following = if differentTarget then "f1c4" else "f1b5"
      val changed = List("h1h2", "a3b5", following)
      val first = begin(fen, "h1h2", List(plan -> 0, List("a1a3") -> -10, List("h1g1") -> -20),
        List(changed)).asInstanceOf[AwaitingEngineWork]
      val pending = if differentTarget then
        // Capturing the other pawn is already legal now, so the existing agenda correctly
        // asks whether to do that immediately before comparing the later reply.
        assertEquals(first.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("h1h2", "f1c4")))
        submit(first, List(plan -> 0, List("f1c4") -> -10)).asInstanceOf[AwaitingEngineWork]
      else first
      assertEquals(pending.issuedWork.searchAnchor.map(_.prefixMoves), Some(List("h1h2")),
        s"${pending.issuedWork.purpose} ${pending.issuedWork.rootRestriction} depth ${pending.issuedWork.requiredDepth}")
      pending.issuedWork.rootRestriction match
        case EngineRootRestriction.Restricted(moves) => assertEquals(moves.toSet, Set("a3b5", "h8g8"))
        case _ => fail("the response comparison must restrict the same two replies")
      val after = submit(pending, List(List("a3b5", following) -> 0, List("h8g8", "a1a3") -> 0))
      val idea = CommentaryIdea.read(after.proposalEntries).find(_.key.branch == plan).get
      val packet = after.observationsPacket.get
      val prepared = ObservedLineProtocol.prepare(packet)
      val text = idea.continuationText(packet, prepared)
      if differentTarget then
        assert(text.contains("original action is not observed"), text)
        assert(!text.contains("same physical target"), text)
      else
        assert(text.contains("same physical target is captured by a different piece"), text)
        assert(text.contains("white bishop originally on f1 capturing black knight originally on a3"), text)
      assert(text.contains(s"a3b5 $following"), text)
      val public = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("p" * 32, "practical-decision", 10000L, after))
      val proposals = (public \ "observations" \ "selection" \ "proposals").as[List[JsObject]]
      assert(proposals.exists(value => (value \ "answer_text").asOpt[String].exists(_.contains(text))),
        "continuation explanation must reach the normal response, not only an internal helper")
    }
