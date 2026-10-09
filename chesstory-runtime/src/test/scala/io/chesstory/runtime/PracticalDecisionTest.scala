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
    case stopped: StoppedCommentaryJob => fail(s"Expected pending work; stopped=${stopped.stopCondition}")
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
    val start = awaiting(CommentaryJobReducer.startJob(owner.initialFen, owner.movePrefixUci, fen, played,
      owner.extend(List(played)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L,
      inputLines = supplied.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _)), proposals = proposals)
      .toOption.get)
    val preview = awaiting(submit(start, rows.map((moves, cp) => moves.take(1) -> cp)))
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
    def lineNote(packet: EvidenceBackedJudgmentPacket): MoveWorkProjection.LocalCandidate =
      val root = packet.assembly.playedTransition.get.evidence.id
      MoveWorkProjection.select(packet).local.find(value => value.edge.evidence.id == root && (value.note.detail match
        case MoveWorkProjection.Detail.ChangedLines(closed, opened, _) =>
          closed.exists((actor, target, remaining) => actor == Piece("white", "queen", "g3").witness &&
            target == Piece("black", "pawn", "d6").witness && remaining.contains(Piece("white", "rook", "d1").witness)) &&
            opened.contains(Piece("white", "bishop", "b3").witness -> Piece("black", "pawn", "f7").witness)
        case _ => false)).getOrElse(fail("missing the owned obstruction and opened attack note"))
    def sameNoteReferences(value: JsObject, source: MoveWorkProjection.LocalCandidate): Boolean =
      List("occurrence_id", "squares", "premise_ids", "frames").forall(field => value(field) == source.highlight(field))
    def assertVisibleRelations(value: JsObject): Unit =
      val words = (value \ "text").as[String]
      List("Blocks the line", "queen on g3", "pawn on d6", "rook on d1", "still attacks that target",
        "Opens the line", "bishop on b3", "pawn on f7").foreach(clause => assert(words.contains(clause), words))
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
      val source = lineNote(packet)
      val note = highlights.find(sameNoteReferences(_, source)).getOrElse(
        fail("the displayed note lost its exact occurrence and before/after proof references"))
      assertVisibleRelations(note)
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
    val source = lineNote(state.observationsPacket.get)
    val note = delivered.find(value => (value \ "occurrence_id").as[String] == root && sameNoteReferences(value, source))
      .getOrElse(fail("the owned report reaches the normal job response with its original proof references"))
    assertVisibleRelations(note)
    val comparison = (response \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("best_vs_second")).get
    val explanation = (comparison \ "detail" \ "text").as[String]
    val differences = (comparison \ "detail" \ "effects").as[List[JsObject]].filter(effect =>
      (effect \ "scope").as[String] == "pair_difference")
    List("g3" -> "d6", "b3" -> "f7").foreach { (actor, target) =>
      assert(differences.exists(effect => (effect \ "actor" \ "square").asOpt[String].contains(actor) &&
        (effect \ "target" \ "square").asOpt[String].contains(target)), explanation)
    }
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
    assert(local.contains("Black's rook on f5"), local)
    assert(local.contains("Blocks the line from White queen on h4 toward e4"), local)
    assert(!local.contains("Black pawn on b4"), "the same blocked ray is represented by the used reply square")
    val comparison = (response \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("played_vs_best")).get
    val text = (comparison \ "detail" \ "text").as[String]
    val vacancy = (comparison \ "detail" \ "effects").as[List[JsObject]].find(effect =>
      (effect \ "scope").as[String] == "pair_difference" && (effect \ "square").asOpt[String].contains("e4")).get
    assertEquals((vacancy \ "actor" \ "square").as[String], "h4")
    assertEquals((vacancy \ "target").asOpt[JsObject], None)
    assert(text.contains("Qe4"), text)
    assert(text.contains("Rd2") && text.contains("Rf8") && text.contains("initially attacked Black rook on f5"),
      "the supplied counterattack and reply must be connected to the original rook's later evasion: " + text)
    assert(!text.contains("Black pawn on b4"), text)
    assert(!text.contains("an explanation beyond this material comparison is still missing"), text)
    assert(!text.contains("safe") && !text.contains("strongest"), text)
    assertEquals((comparison \ "detail" \ "type").as[String], "root_comparison")
    assertEquals((comparison \ "detail" \ "material_relation").as[String], "equal")
    assertEquals((comparison \ "detail" \ "material_unavailable_reason").get, JsNull)
    val effects = (comparison \ "detail" \ "effects").as[List[JsObject]]
    val control = effects.find(value => (value \ "scope").as[String] == "pair_difference" &&
      (value \ "square").as[String] == "e4").get
    assertEquals((control \ "actor" \ "square").as[String], "h4")
    assertEquals((control \ "target").get, JsNull)
    assertEquals((control \ "through_plies").as[List[Int]].sorted, List(1, 2),
      "the short Qg4 arm must not borrow the longer arm's reply")
    val history = effects.find(value => (value \ "scope").as[String] == "line_history" &&
      (value \ "target" \ "square").asOpt[String].contains("f5")).get
    assertEquals((history \ "through_plies").as[List[Int]], List(line.size))
    val packet = state.observationsPacket.get
    val paths = ObservedLineProtocol.prepare(packet)
    effects.foreach { effect =>
      val bounds = (effect \ "line_ids").as[List[String]].zip((effect \ "through_plies").as[List[Int]])
      val occurrences = bounds.flatMap((id, through) => paths.path(packet, id).get.ids.take(through)).toSet
      assert((effect \ "occurrence_ids").as[List[String]].forall(occurrences))
      (effect \ "evidence_ids").as[List[String]].foreach(id =>
        assert(packet.evidenceGraph.byId.get(id).exists(packet.evidenceGraph.proofEligible) ||
          packet.evidenceGraph.byId.values.exists {
            case record @ EvidenceRecord(_, fact: RelationFactEvidence, _) =>
              fact.semanticId == id && packet.evidenceGraph.proofEligible(record)
            case _ => false
          }, id))
    }
    (comparison \ "evidence_ids").as[List[String]].foreach(id =>
      assert(state.observationsPacket.get.evidenceGraph.byId.get(id)
        .exists(state.observationsPacket.get.evidenceGraph.proofEligible), id))
    val questionText = deliveredComparisons(state)
    assert(text.contains("At this line's end, these original targets remain:") &&
      text.contains("Black rook on f8 (originally the rook on f5)"), text)
    assert(!questionText.contains("initially attacked Black rook on f5"),
      "the root comparison already owns this exact path's history: " + questionText)

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
    assert(text.contains("Qe4"), text)
    assert(!text.contains("Rf8") && !text.contains("initially attacked Black rook on f5"), text)
    assertEquals((comparison \ "detail" \ "material_unavailable_reason").get, JsNull,
      "a short valid returned line does not make the material evidence unavailable")
    assert(!(comparison \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil).exists(value =>
      (value \ "scope").as[String] == "line_history" &&
        (value \ "target" \ "square").asOpt[String].contains("f5")),
      "no endpoint reference may leak from the longer supplied branch")

  test("a display-capped attack still connects its observed outcome in the selected comparison"):
    val fen = "6k1/8/8/4p3/2N3r1/8/8/7K w - - 0 1"
    val line = List("c4e5", "g4g3")
    val state = begin(fen, line.head, List(line -> 0, List("c4d6") -> 20, List("c4b6") -> 10))
    val packet = state.observationsPacket.get
    val root = packet.assembly.playedTransition.get.evidence.id
    val notes = MoveWorkProjection.select(packet).local.filter(_.edge.evidence.id == root)
    assertEquals(notes.count(_.selection == CommentarySelection.Display.Selected), 3)
    val attack = notes.find(_.note.detail match
      case MoveWorkProjection.Detail.Attack(actor, target, _) =>
        actor.square.key == "e5" && target.square.key == "g4"
      case _ => false).get
    assertEquals(attack.selection, CommentarySelection.Display.DisplayCap)
    val response = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("c" * 32, "capped-attack", 10000L, state))
    val comparison = (response \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("played_vs_best")).get
    val text = (comparison \ "detail" \ "text").as[String]
    assert(text.contains("Rg3 moves the initially attacked Black rook on g4 out of White knight on e5's attack"), text)
    assert(text.contains("remains on g3 at this line's end"), text)
    (comparison \ "evidence_ids").as[List[String]].foreach(id =>
      assert(packet.evidenceGraph.byId.get(id).exists(packet.evidenceGraph.proofEligible), id))

  test("a display-capped counterattack still explains the response before the original target evades"):
    val fen = "6rk/8/8/2n5/4P1R1/8/8/6K1 w - - 0 1"
    val line = List("g4g5", "c5e4", "g5g6", "g8f8")
    val state = begin(fen, line.head, List(line -> 0, List("g1f1") -> 20, List("g1h1") -> 10))
    val packet = state.observationsPacket.get
    val reply = packet.assembly.transitions.find(_.moveUci == "c5e4").get.evidence.id
    val notes = MoveWorkProjection.select(packet).local.filter(_.edge.evidence.id == reply)
    val counterattack = notes.find(_.note.detail match
      case MoveWorkProjection.Detail.Attack(actor, target, _) =>
        actor.square.key == "e4" && target.square.key == "g5"
      case _ => false).get
    assertEquals(counterattack.selection, CommentarySelection.Display.DisplayCap)
    val response = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("c" * 32, "capped-counterattack", 10000L, state))
    val comparison = (response \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("played_vs_best")).get
    val text = (comparison \ "detail" \ "text").as[String]
    assert(text.contains("Nxe4: Black's knight on e4 now targets White's rook on g5."), text)
    assert(text.contains("Rg6: Moves the rook out of the attack from Black's knight on e4."), text)
    assert(text.contains("Rf8 moves the initially attacked Black rook on g8 out of White rook on g6's attack"), text)
    assert(text.contains("At this line's end, these original targets remain:") &&
      text.contains("Black rook on f8 (originally the rook on g8)"), text)

  private def rookAttackExplanation(fen: String, line: List[String]): String =
    val state = begin(fen, line.head, List(line -> 0, List("h1g1") -> 20, List("h1h2") -> 10))
    val observations = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("r" * 32, "rook-attack", 10000L, state)) \ "observations").as[JsObject]
    val candidates = (observations \ "selection" \ "candidates").as[List[JsObject]]
    val candidate = candidates.find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("played_vs_best")).get
    assertPublishedEffectSources(observations, candidate)
    (candidate \ "evidence_ids").as[List[String]].foreach(id =>
      assert(state.observationsPacket.get.evidenceGraph.byId.get(id).exists(state.observationsPacket.get.evidenceGraph.proofEligible)))
    (candidate \ "detail" \ "text").as[String]

  private def assertPublishedEffectSources(observations: JsObject, candidate: JsObject): Unit =
    def strings(value: JsValue): Iterator[String] = value match
      case JsString(text) => Iterator.single(text)
      case JsArray(values) => values.iterator.flatMap(strings)
      case value: JsObject => value.values.iterator.flatMap(strings)
      case _ => Iterator.empty
    val published = strings(observations - "selection").toSet
    (candidate \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil).foreach { effect =>
      (effect \ "evidence_ids").as[List[String]].foreach(id =>
        assert(published(id), s"explanation source must be published outside its own references: $id"))
    }

  test("shared counterplay is told once while every threatened piece keeps its outcome"):
    val fen = "r1b2rk1/ppp2pbp/5np1/3q4/3N4/4B3/PPPQBPPP/R4RK1 b - - 1 12"
    val line = "f6g4 c2c4 d5d6 e2g4 c8g4 h2h3 c7c5 h3g4 c5d4".split(" ").toList
    val state = begin(fen, line.head, List(line -> -40, List("f8d8") -> -20, List("f6e4") -> 0))
    val obs = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("u" * 32, "shared-counterplay", 10000L, state)) \ "observations").as[JsObject]
    val selected = (obs \ "selection" \ "candidates").as[List[JsObject]]
      .filter(value => (value \ "selection").as[String] == "selected")
    val primary = selected.find(value => (value \ "detail" \ "type").as[String] == "root_comparison").get
    val text = (primary \ "detail" \ "text").as[String]
    assertEquals(text.sliding("c4: White's pawn".length).count(_ == "c4: White's pawn"), 1, text)
    val sameEvasion = List("Qd6: Moves the queen out of", "Qd6 moves the initially attacked Black queen")
    assertEquals(sameEvasion.map(value => text.sliding(value.length).count(_ == value)).sum, 1, text)
    assert(!text.contains("after c4, White pawn on c4 attacks Black queen on d5"),
      "the same counterattack already belongs to the initial threat's observed response: " + text)
    assertEquals(text.sliding("captures the original attacker Black knight on g4".length)
      .count(_ == "captures the original attacker Black knight on g4"), 1, text)
    List("White bishop on e3", "White pawn on f2", "White pawn on h2",
      "cxd4: Black pawn on c5 captures the initially attacked White knight on d4").foreach(value => assert(text.contains(value), text))
    selected.foreach(assertPublishedEffectSources(obs, _))
    val effects = selected.flatMap(value => (value \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil))
    assertEquals(effects.distinct.size, effects.size, "visible comparisons share exact effects instead of repeating them")

  test("the main comparison retains the current capture and its certified immediate recaptures"):
    val fen = "2r1r2k/p2n1qp1/1p3pRp/3P4/2P5/P2Q4/1B4PP/5RK1 w - - 5 31"
    val line = "b2f6 d7f6 f1f6 g7f6".split(" ").toList
    def text(returned: List[String], supplied: List[List[String]] = Nil): String =
      val state = begin(fen, line.head,
        List(returned -> 0, List("d3d1") -> -40, List("h2h3") -> -60), supplied)
      val response = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("x" * 32, "immediate-exchange", 10000L, state))
      (response \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
        .filter(value => (value \ "detail" \ "type").as[String] == "root_comparison")
        .map(value => (value \ "detail" \ "text").as[String]).mkString(" ")
    val full = text(line)
    val opening = "the opening exchange is: White bishop captures the black pawn on f6 with Bxf6; " +
      "Black knight recaptures the white bishop on f6 with Nxf6; " +
      "White rook recaptures the black knight on f6 with Rfxf6; " +
      "Black pawn recaptures the white rook on f6 with gxf6"
    assert(full.contains(opening), full)
    assert(!full.contains("Bxf6 captures Black pawn on f6") &&
      !full.contains("gxf6 recaptures White rook on f6"),
      "the same main body already owns those exact capture steps in its opening exchange")
    val short = text(line.take(2), List(line))
    assert(short.contains("Black knight recaptures the white bishop on f6 with Nxf6"), short)
    assert(!short.contains("Black pawn recaptures the white rook on f6 with gxf6"),
      "a longer supplied line must not complete the returned line's exchange")
    val interrupted = text("b2f6 h8h7 d3d1 f7f6".split(" ").toList)
    assert(!interrupted.contains("the opening exchange is:"),
      "a later capture after intervening moves is not an immediate recapture chain")

  test("the main exchange keeps the en-passant victim and recapture squares distinct"):
    val fen = "1r3rk1/5ppp/1q2b3/8/1p6/1P3BP1/PQ3P2/R2R2K1 w - - 0 26"
    val line = "a2a4 b4a3 a1a3".split(" ").toList
    val state = begin(fen, line.head,
      List(line -> 0, List("a2a3") -> -40, List("d1d2") -> -60))
    val response = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("y" * 32, "ep-opening-exchange", 10000L, state))
    val text = (response \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .filter(value => (value \ "detail" \ "type").as[String] == "root_comparison")
      .map(value => (value \ "detail" \ "text").as[String]).mkString(" ")
    assert(text.contains("Black pawn captures the white pawn on a4 with bxa3; " +
      "White rook recaptures the black pawn on a3 with Rxa3"), text)

  test("an observed target capture includes its capturer's recapture and the opened file"):
    val fen = "3r4/5k2/1p3b1p/2p2p2/p1b2B2/P1P3PP/1P3K2/2N1R3 b - - 0 31"
    val line = "f6g5 e1e5 f7f6 f2e1 g5f4 g3f4".split(" ").toList
    def response(returned: List[String], supplied: List[List[String]] = Nil): JsObject =
      val state = begin(fen, line.head, List(returned -> -40, List("b6b5") -> -20, List("f7g6") -> 0), supplied)
      val observations = (PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("v" * 32, "capturer-recapture", 10000L, state)) \ "observations").as[JsObject]
      val graph = state.observationsPacket.get.evidenceGraph
      (observations \ "selection" \ "candidates").as[List[JsObject]].filter(candidate =>
        (candidate \ "detail" \ "type").as[String] == "root_comparison").foreach(candidate =>
        (candidate \ "evidence_ids").as[List[String]].foreach(id =>
          assert(graph.byId.get(id).exists(graph.proofEligible), id)))
      observations
    def primary(obs: JsObject) = (obs \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "type").as[String] == "root_comparison").get
    val obs = response(line)
    val candidate = primary(obs)
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Bxf4 captures the initially attacked White bishop on f4"), text)
    assert(text.contains("gxf4 recaptures Black bishop on f4") && text.contains("opens the g-file"), text)
    assertPublishedEffectSources(obs, candidate)
    val path = (obs \ "paths").as[List[JsObject]].find(value => (value \ "moves").as[List[String]] == line).get
    val effect = (candidate \ "detail" \ "effects").as[List[JsObject]].find(value =>
      (value \ "scope").as[String] == "line_history" &&
        (value \ "line_ids").as[List[String]] == List((path \ "id").as[String]) &&
        (value \ "actor" \ "square").as[String] == "g5" &&
        (value \ "actor" \ "role").as[String] == "bishop" &&
        (value \ "actor" \ "side").as[String] == "black" &&
        (value \ "target" \ "square").asOpt[String].contains("f4") &&
        (value \ "target" \ "role").asOpt[String].contains("bishop") &&
        (value \ "target" \ "side").asOpt[String].contains("white")).get
    assertEquals((effect \ "through_plies").as[List[Int]], List(line.size))
    val occurrences = (path \ "occurrence_ids").as[List[String]]
    List(4, 5).foreach(offset =>
      assert((effect \ "occurrence_ids").as[List[String]].contains(occurrences(offset))))
    val short = primary(response(line.take(5), List(line)))
    val shortText = (short \ "detail" \ "text").as[String]
    assert(shortText.contains("Bxf4 captures the initially attacked"), shortText)
    assert(!shortText.contains("gxf4") && !shortText.contains("opens the g-file"), shortText)
    val delayed = line.take(5) ++ List("e5e2", "h6h5", "g3f4")
    val delayedText = (primary(response(delayed)) \ "detail" \ "text").as[String]
    assert(delayedText.contains("gxf4 captures the original attacker Black bishop on f4") &&
      delayedText.contains("The captured bishop played Bxf4"), delayedText)
    assert(!delayedText.contains("gxf4 recaptures"), "a later capture is not an immediate recapture")

  List(
    ("the attacked bishop's exchange includes what it captured before its loss",
      "5rk1/1b1rppbp/p4n2/2q2p2/P2N1B1Q/2PR4/4N1PP/3R2K1 b - - 1 31", "e7e5",
      List("e7e5 f4h6 f6e8 h6g7 e8g7".split(" ").toList -> -40,
        List("g8h8") -> -20, List("f8d8") -> 0),
      List("Bh6 moves the initially attacked White bishop on f4",
        "captures Black bishop on g7", "Nxg7 recaptures White bishop on g7",
        "The captured bishop played Bxg7"),
      ("h6g7", "bishop", "g7")),
    ("an evading knight's immediate exchange includes the pawn it captured",
      "2r3k1/pp2ppbp/q2pbnp1/8/2rBP1P1/1NN2P2/PPPQ3P/1K1R3R w - - 5 18", "g4g5",
      List("g4g5 f6e4 f3e4".split(" ").toList -> 0,
        List("d2g5") -> 20, List("d2e2") -> 10),
      List("Black knight captures the white pawn on e4 with Nxe4",
        "Nxe4 moves the initially attacked Black knight on f6", "fxe4: White pawn on f3 captures the initially attacked Black knight on e4"),
      ("f6e4", "pawn", "e4")),
    ("a delayed capturer removal includes the rest of its immediate exchange",
      "5rk1/1b1rppbp/p4np1/2q2P2/P2N1B2/2PR4/4NQPP/3R2K1 b - - 0 30", "g6f5",
      List("b7e4 d3d2 f6g4 f2h4 e4f5 h2h3 g4f6 g1h2 d7d5 d4f5 d5f5".split(" ").toList -> -40,
        List("g6f5") -> -20, List("f8d8") -> 0),
      List("Bxf5 captures the initially attacked White pawn on f5",
        "Nxf5 later captures Black bishop on f5", "Rxf5 recaptures White knight on f5"),
      ("d5f5", "knight", "f5"))
  ).foreach { (title, fen, played, rows, expected, gain) =>
    test(title):
      val state = begin(fen, played, rows)
      val obs = (PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("j" * 32, "observed-exchange", 10000L, state)) \ "observations").as[JsObject]
      val candidate = (obs \ "selection" \ "candidates").as[List[JsObject]]
        .find(value => (value \ "detail" \ "type").as[String] == "root_comparison").get
      val path = (obs \ "paths").as[List[JsObject]].find(value =>
        (value \ "moves").as[List[String]] == rows.head._1).get
      val captures = (path \ "line_summary" \ "events").as[List[JsObject]].filter(event =>
        Set("capture", "recapture")((event \ "kind").as[String]))
      assert(captures.exists(event => (event \ "move_uci").as[String] == gain._1 &&
        (event \ "target_role").as[String] == gain._2 && (event \ "square").as[String] == gain._3),
        "the missing gain already belongs to this exact certified line")
      val text = (candidate \ "detail" \ "text").as[String]
      expected.foreach(value => assert(text.contains(value), text))
      assert(text.length <= 4000, text)
      assertPublishedEffectSources(obs, candidate)
      val shortMoves = rows.head._1.dropRight(1)
      val shortState = begin(fen, played, (shortMoves -> rows.head._2) :: rows.tail,
        supplied = List(rows.head._1))
      val shortObs = (PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("k" * 32, "bounded-observed-exchange", 10000L, shortState)) \ "observations").as[JsObject]
      val shortCandidate = (shortObs \ "selection" \ "candidates").as[List[JsObject]]
        .find(value => (value \ "detail" \ "type").as[String] == "root_comparison").get
      val shortText = (shortCandidate \ "detail" \ "text").as[String]
      assert(!shortText.contains(expected.last), "the longer supplied path must not finish this exchange: " + shortText)
      val longerIds = (shortObs \ "paths").as[List[JsObject]].filter(value =>
        (value \ "moves").as[List[String]] == rows.head._1).map(value => (value \ "id").as[String]).toSet
      val effectLines = (shortCandidate \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil)
        .flatMap(value => (value \ "line_ids").as[List[String]])
      assert(!effectLines.exists(longerIds), "effect ownership must not borrow the longer supplied arm")
      assertPublishedEffectSources(shortObs, shortCandidate)
  }

  test("a capture made while escaping stays visible across quiet moves without becoming an immediate recapture"):
    val fen = "2r3k1/pp2ppbp/q2pbnp1/8/2rBP1P1/1NN2P2/PPPQ3P/1K1R3R w - - 5 18"
    val line = "g4g5 f6e4 d2g2 g8f8 f3e4".split(" ").toList
    val state = begin(fen, line.head, List(line -> 0, List("d2g5") -> 20, List("d2e2") -> 10),
      supplied = List("g4g5 f6e4 f3e4".split(" ").toList))
    val obs = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("l" * 32, "quiet-exchange-boundary", 10000L, state)) \ "observations").as[JsObject]
    val candidate = (obs \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "type").as[String] == "root_comparison").get
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("fxe4: White pawn on f3 captures the initially attacked Black knight on e4"), text)
    assert(text.contains("Black knight on f6 captures White pawn on e4"), text)
    assert(!text.contains("fxe4 recaptures"), "quiet moves separate the removal from the earlier capture: " + text)
    assertPublishedEffectSources(obs, candidate)

  test("an exchange extension does not turn a capture promotion into an unchanged-role trade"):
    val fen = "7k/8/8/8/8/7R/1p6/NK6 w - - 0 1"
    val line = List("h3b3", "b2a1q", "b1a1")
    val state = begin(fen, line.head, List(line -> 0, List("h3h4") -> 20, List("b1c2") -> 10))
    val obs = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("m" * 32, "promotion-exchange-boundary", 10000L, state)) \ "observations").as[JsObject]
    val candidate = (obs \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "type").as[String] == "root_comparison").get
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Kxa1: White king on b1 captures the initially attacked Black queen on a1"), text)
    assert(text.contains("Black pawn on b2 captures White knight on a1") &&
      text.contains("Black pawn on b2 promotes to a queen"), text)
    assertPublishedEffectSources(obs, candidate)

  test("a later capture connects only the intervening check and response in its own certified line"):
    val fen = "2rr2k1/p3qp1p/5np1/2b1p3/2R5/1N4P1/P1Q2PNP/R5K1 b - - 1 26"
    val line = "c5f2 g1f2 e7e6 c4c8 f6g4 f2g1 d8c8".split(" ").toList
    def response(returned: List[String], supplied: List[List[String]] = Nil): JsObject =
      val state = begin(fen, line.head,
        List(returned -> -40, List("f6d7") -> -20, List("f6d5") -> 0), supplied)
      val obs = (PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("z" * 32, "intervening-check", 10000L, state)) \ "observations").as[JsObject]
      val graph = state.observationsPacket.get.evidenceGraph
      (obs \ "selection" \ "candidates").as[List[JsObject]].filter(candidate =>
        (candidate \ "detail" \ "type").as[String] == "root_comparison").foreach(candidate =>
        (candidate \ "evidence_ids").as[List[String]].foreach(id =>
          assert(graph.byId.get(id).exists(graph.proofEligible), id)))
      obs
    def primary(obs: JsObject) = (obs \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "type").as[String] == "root_comparison").get
    val obs = response(line)
    val candidate = primary(obs)
    val text = (candidate \ "detail" \ "text").as[String]
    val sequence = List("Rxc8 captures the original attacker Black rook on c8",
      "Ng4+ checks the White king on f2 before the later capture.",
      "Kg1 moves the White king from f2 to g1 in reply to that check.",
      "Rxc8: Black rook on d8 captures the initially attacked White rook on c8")
    val recovery = sequence.last
    sequence.foreach(value => assert(text.contains(value), text))
    assert(!text.contains("Bxf2+ checks"), "the earlier check is outside this capture-to-recovery interval")
    assert(text.length <= 4000, text)
    assertPublishedEffectSources(obs, candidate)
    val path = (obs \ "paths").as[List[JsObject]].find(value => (value \ "moves").as[List[String]] == line).get
    val checks = (path \ "line_summary" \ "events").as[List[JsObject]].filter(event =>
      (event \ "kind").as[String] == "check")
    assert(checks.exists(event => (event \ "ply_offset").as[Int] == 4 &&
      (event \ "move_uci").as[String] == "f6g4" && (event \ "square").as[String] == "f2"))
    val occurrences = (path \ "occurrence_ids").as[List[String]]
    val effect = (candidate \ "detail" \ "effects").as[List[JsObject]].find(value =>
      (value \ "scope").as[String] == "line_history" &&
        (value \ "occurrence_ids").as[List[String]].contains(occurrences(4))).get
    List(3, 4, 5, 6).foreach(offset =>
      assert((effect \ "occurrence_ids").as[List[String]].contains(occurrences(offset))))
    assertEquals((effect \ "line_ids").as[List[String]], List((path \ "id").as[String]))
    assertEquals((effect \ "through_plies").as[List[Int]], List(line.size))
    val short = primary(response(line.take(6), List(line)))
    val shortText = (short \ "detail" \ "text").as[String]
    assert(!shortText.contains("before the later capture") && !shortText.contains(recovery), shortText)
    val quiet = line.take(4) ++ List("g8g7", "f2g1", "d8c8")
    val quietText = (primary(response(quiet, List(line))) \ "detail" \ "text").as[String]
    assert(quietText.contains(recovery), quietText)
    assert(!quietText.contains("Ng4+ checks") && !quietText.contains("in reply to that check"), quietText)
    val checkAfterRecovery = line.take(4) ++ List("d8c8", "c2e2", "f6g4", "f2g1")
    val laterText = (primary(response(checkAfterRecovery)) \ "detail" \ "text").as[String]
    assert(!laterText.contains("before the later capture") && !laterText.contains("in reply to that check"), laterText)
    val captureFen = "6k1/p4p1p/6p1/2R5/3qp3/2R3P1/P2nK2P/4N3 b - - 1 35"
    val captureLine = "d4d6 c5c6 d6d5 c6c5 d2b1 c5d5 b1c3 e2e3 c3d5".split(" ").toList
    val captureState = begin(captureFen, captureLine.head,
      List(captureLine -> -40, List("d2b1") -> -20, List("e4e3") -> 0))
    val captureObs = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("w" * 32, "intervening-capture-check", 10000L, captureState)) \ "observations").as[JsObject]
    val captureCandidate = primary(captureObs)
    val captureText = (captureCandidate \ "detail" \ "text").as[String]
    val captureSequence = List("Rxd5 captures the original attacker Black queen on d5",
      "Nxc3+ captures the White rook on c3 with check against the White king on e2 before the later capture.",
      "Ke3 moves the White king from e2 to e3 in reply to that check.",
      "Nxd5+: Black knight on c3 captures the initially attacked White rook on d5")
    captureSequence.foreach(value => assert(captureText.contains(value), captureText))
    assertPublishedEffectSources(captureObs, captureCandidate)
    val capturePath = (captureObs \ "paths").as[List[JsObject]].find(value =>
      (value \ "moves").as[List[String]] == captureLine).get
    val simultaneous = (capturePath \ "line_summary" \ "events").as[List[JsObject]].filter(event =>
      (event \ "ply_offset").as[Int] == 6)
    assert(simultaneous.exists(event => (event \ "kind").as[String] == "capture" &&
      (event \ "target_role").as[String] == "rook" && (event \ "square").as[String] == "c3"))
    assert(simultaneous.exists(event => (event \ "kind").as[String] == "check" &&
      (event \ "target_role").as[String] == "king" && (event \ "square").as[String] == "e2"))

  test("the response to a counterattack can also withdraw the original attack on the same move"):
    val fen = "6rk/8/8/2n5/4P1R1/8/8/7K w - - 0 1"
    val line = List("g4g5", "c5e4", "g5f5")
    val state = begin(fen, line.head, List(line -> 0, List("h1g1") -> 20, List("h1h2") -> 10))
    assert(ObservedLineProtocol.prepare(state.observationsPacket.get).paths.exists(path =>
      path.moves == line && path.certified.nonEmpty), "the whole counterattack and withdrawal must be legal and observed")
    val comparison = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("c" * 32, "counterattack-withdrawal", 10000L, state)) \ "observations" \ "selection" \ "candidates")
      .as[List[JsObject]].find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("played_vs_best")).get
    val text = (comparison \ "detail" \ "text").as[String]
    assert(text.contains("Nxe4: Black's knight on e4 now targets White's rook on g5"), text)
    assert(text.contains("Rf5: Moves the rook out of the attack from Black's knight on e4"), text)
    assert(text.contains("White rook on g5 moves to f5, ending its attack on Black rook on g8"), text)

  test("an initial evasion retains the same target's renewed attack and later capture"):
    val fen = "r6k/8/8/8/8/8/8/R6K w - - 0 1"
    val renewed = List("a1a2", "a8b8", "a2b2", "h8g8")
    val surviving = rookAttackExplanation(fen, renewed)
    assert(surviving.contains("Rb8") && surviving.contains("remains on b8 at this line's end"), surviving)
    assert(surviving.contains("original attacker attacks it again"), surviving)
    val captured = rookAttackExplanation(fen, renewed :+ "b2b8")
    assert(captured.contains("Rb8") && captured.contains("Rxb8+ captures the initially attacked Black rook on b8"), captured)
    assert(!captured.contains("remains on b8"), captured)

  test("moving along the same attack line is not an evasion"):
    val text = rookAttackExplanation("r6k/8/8/8/8/8/8/R6K w - - 0 1",
      List("a1a2", "a8a7", "h1g1", "h8g8"))
    assert(!text.contains("out of White rook"), text)
    assert(text.contains("to a7; White rook on a2 still attacks it there"), text)

  test("capturing another rook on the original square does not capture the original target"):
    val text = rookAttackExplanation("r6r/7k/8/8/8/8/8/R6K w - - 0 1",
      List("a1a2", "a8b8", "a2b2", "b8b6", "b2a2", "h8a8", "a2a8"))
    assert(text.contains("initially attacked Black rook on a8") && text.contains("remains on b6 at this line's end"), text)
    assert(!text.contains("captures the initially attacked") && !text.contains("original target is captured"), text)

  test("evasion from another attacker cannot settle the initially observed attack"):
    val text = rookAttackExplanation("r6k/8/8/8/8/8/8/RR5K w - - 0 1",
      List("a1a2", "h8g8", "b1b8", "g8f7", "a2c2", "a8a7"))
    assert(text.contains("Rc2: White rook on a2 moves to c2, ending its attack on Black rook on a8") &&
      text.contains("White rook on b8 attacks Black rook on a8 after this move"), text)
    assert(!text.contains("Ra7 moves the initially attacked"),
      "the later evasion is from the other rook, not the original attacker: " + text)

  test("an unrelated evasion is not presented as the answer to an intermediate counterattack"):
    val text = rookAttackExplanation("r6k/2B3b1/4q3/8/8/2N5/8/R6K w - - 0 1",
      List("a1a2", "e6e7", "c3b1", "a8b8"))
    assert(text.contains("Rb8") && text.contains("remains on b8 at this line's end"), text)
    assert(!text.contains("Qe7") && !text.contains("Nb1"),
      "the knight evades the old bishop attack, not the queen's new attack on c7: " + text)
    assert(!text.contains("safe"), "the rook on b8 remains attacked by the bishop on c7: " + text)

  test("capturing the actual attacker explains why the original target can stay put"):
    val fen = "r6k/8/8/8/2b5/8/6B1/R6K w - - 0 1"
    val text = rookAttackExplanation(fen, List("a1a2", "c4a2"))
    assert(text.contains("Bxa2 captures the original attacker White rook on a2"), text)
    assert(text.contains("initially attacked Black rook on a8 stays on a8"), text)
    assert(text.contains("White bishop on g2 attacks that target after this move"), text)
    assert(!text.contains("safe") && !text.contains("captures the initially attacked Black rook"), text)
    val captured = rookAttackExplanation(fen, List("a1a2", "c4a2", "g2a8"))
    assert(captured.contains("Bxa2 captures the original attacker") &&
      captured.contains("Bxa8: White bishop on g2 captures the initially attacked Black rook on a8"), captured)
    assert(!captured.contains("remains on a8"), captured)

  test("the original target captures its attacker while moving and keeps its later fate"):
    val fen = "r6k/8/8/8/8/8/8/R6K w - - 0 1"
    val text = rookAttackExplanation(fen, List("a1a2", "a8a2", "h1g1"))
    assert(text.contains("Rxa2 captures the original attacker White rook on a2"), text)
    assert(text.contains("initially attacked Black rook on a8 moves to a2"), text)
    assert(text.contains("remains on a2 at this line's end"), text)
    assert(!text.contains("safe") && !text.contains("stays on a8"), text)

  test("the original attacker leaves its attack and can restore it later in the same line"):
    val fen = "r6k/8/8/8/8/8/8/R6K w - - 0 1"
    val retreat = List("a1a2", "h8g8", "a2b2")
    val text = rookAttackExplanation(fen, retreat)
    assert(text.contains("Rb2: White rook on a2 moves to b2, ending its attack on Black rook on a8"), text)
    assert(text.contains("remains on a8 at this line's end"), text)
    val reopened = rookAttackExplanation(fen, retreat ++ List("g8h8", "b2a2", "a8a2"))
    assert(reopened.contains("Ra2 restores") && reopened.contains("Rxa2 captures the original attacker"), reopened)
    val asymmetric = (attackPair("7k/8/8/8/6r1/5N2/8/7K w - - 0 1",
      List("f3e5", "h8g8", "e5c4"), List("f3d2")) \ "detail" \ "text").as[String]
    assert(asymmetric.contains("Nc4: White knight on e5 moves to c4, ending its attack on Black rook on g4"),
      "a knight's withdrawal must not require the rook to have attacked it back: " + asymmetric)

  private def attackPair(fen: String, first: List[String], second: List[String],
      supplied: List[List[String]] = Nil): JsObject =
    val state = begin(fen, first.head, List(second -> 20, first -> 0, List("h1h2") -> -10), supplied)
    val observations = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("d" * 32, "paired-attack-history", 10000L, state)) \ "observations").as[JsObject]
    val candidate = (observations \ "selection" \ "candidates")
      .as[List[JsObject]].find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("played_vs_best")).get
    assertPublishedEffectSources(observations, candidate)
    candidate

  test("equal material still distinguishes the target's own capture from another defender's capture"):
    val candidate = attackPair("r6k/8/8/2b5/8/8/6B1/R6K w - - 0 1",
      List("a1a2", "a8a2"), List("a1a3", "c5a3"))
    val detail = (candidate \ "detail").as[JsObject]
    val text = (detail \ "text").as[String]
    assertEquals((detail \ "material_relation").as[String], "equal")
    assert(text.contains("Compared continuations for the same Black rook originally on a8"), text)
    assert(text.contains("Rxa2 captures the original attacker") && text.contains("moves to a2"), text)
    assert(text.contains("Bxa3 captures the original attacker") && text.contains("stays on a8"), text)
    assert(text.contains("White bishop on g2 attacks that target after this move"), text)
    val effects = (detail \ "effects").as[List[JsObject]].filter(effect =>
      (effect \ "scope").as[String] == "line_history" && (effect \ "target" \ "square").as[String] == "a8")
    assertEquals(effects.size, 2)
    assertEquals(effects.flatMap(effect => (effect \ "line_ids").as[List[String]]).toSet,
      (candidate \ "line_ids").as[List[String]].toSet)
    assert(effects.forall(effect => (effect \ "through_plies").as[List[Int]] == List(2)))

  test("a withdrawn attack compares with witnessed maintained pressure without treating survival as resolution"):
    val fen = "r6k/8/8/8/8/8/8/R6K w - - 0 1"
    val withdrawn = List("a1a2", "h8g8", "a2b2")
    val maintained = List("a1a3", "h8g8", "a3a4")
    val text = (attackPair(fen, withdrawn, maintained) \ "detail" \ "text").as[String]
    assert(text.contains("Compared continuations for the same Black rook originally on a8"), text)
    assert(text.contains("ending its attack on Black rook on a8") && text.contains("original attacker still attacks it there"), text)
    val short = (attackPair(fen, withdrawn.take(2), maintained, List(withdrawn)) \ "detail" \ "text").as[String]
    assert(!short.contains("Compared continuations") && !short.contains("Rb2"), short)
    val uneven = (attackPair(fen, withdrawn, maintained.take(2)) \ "detail" \ "text").as[String]
    assert(uneven.contains("Rb2") && !uneven.contains("Compared continuations"),
      "the longer arm's withdrawal remains its own observation beyond the common horizon: " + uneven)

  test("interposition connects the stopped attack and its later reopening to the same stationary target"):
    val fen = "r6k/8/8/1b6/8/8/8/R6K w - - 0 1"
    val blocked = List("a1a2", "b5a6")
    val first = rookAttackExplanation(fen, blocked)
    assert(first.contains("Ba6 blocks White rook on a2's line to the initially attacked Black rook on a8"), first)
    assert(first.contains("stays on a8") && first.contains("remains on a8 at this line's end"), first)
    val reopened = blocked ++ List("h1g1", "a6b5")
    val again = rookAttackExplanation(fen, reopened)
    assert(again.contains("Bb5 restores White rook on a2's attack on the initially attacked Black rook on a8"), again)
    assert(again.contains("original attacker attacks it again"), again)
    val captured = rookAttackExplanation(fen, reopened :+ "a2a8")
    assert(captured.contains("Bb5 restores") && captured.contains("Rxa8+ captures the initially attacked Black rook on a8"), captured)
    assert(!captured.contains("remains on a8"), captured)

  test("removing the interposed piece restores the same physical attack before the target is captured"):
    val fen = "r6k/8/2B5/1b6/8/8/8/R6K w - - 0 1"
    val text = rookAttackExplanation(fen, List("a1a2", "b5a6", "a2a6", "h8g8", "a6a8"))
    assert(text.contains("Ba6 blocks") && text.contains("White bishop on c6 attacks that target after this move"), text)
    assert(text.contains("Rxa6 restores White rook on a6's attack on the initially attacked Black rook on a8"), text)
    assert(text.contains("Rxa8+ captures the initially attacked Black rook on a8"), text)

  test("capturing an already blocked attacker does not resolve the same attack twice"):
    val text = rookAttackExplanation("r6k/8/8/1b6/8/1b6/8/R6K w - - 0 1",
      List("a1a2", "b5a6", "h1g1", "b3a2"))
    assert(text.contains("Ba6 blocks") && text.contains("remains on a8 at this line's end"), text)
    assert(!text.contains("captures the original attacker"), text)

  test("capturing a different rook is not removal of the tracked attacker"):
    val text = rookAttackExplanation("r6k/8/8/8/8/4b3/5R2/R6K w - - 0 1", List("a1a2", "e3f2"))
    assert(!text.contains("captures the original attacker") && !text.contains("initially attacked Black rook on a8"), text)

  test("en passant removes the attacking pawn on its actual capture square"):
    val text = rookAttackExplanation("7k/8/8/3r4/3p4/8/4P3/7K w - - 0 1", List("e2e4", "d4e3"))
    assert(text.contains("dxe3 captures the original attacker White pawn on e4"), text)
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
    assert(text.contains("Nd5") && text.contains("simultaneously attacks"), text)
    assert(text.contains("queen originally on b6") && text.contains("bishop originally on e7"), text)
    assert(text.contains("does not establish a forced gain"), text)
    val changed = begin(owner.currentFen, played, List(List(played, "g3h3") -> 100, alternative -> 0, List("b6c7") -> 200),
      supplied = List(game), prefixOwner = Some(owner))
    assert(!comparisonText(changed).contains("After Qb4, the returned reply Nd5"),
      "a historical fork must not remain the explanation when the current compared line chooses another reply")
    assert(comparisonText(changed).contains("After Rd7, the returned reply Nd5"),
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
    assert(text.contains("leaves e6 with exd5") && text.contains("leaves e4 with exd5"), text)
    assert(text.contains("do not by themselves establish a forced gain"), text)
    assertEquals(pending.issuedWork.searchAnchor.map(_.prefixMoves), Some(najdorfGame.take(6)),
      "the remaining bishop defense is discovered without supplying Bf8 as a source")
    assertEquals(pending.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("b7c6", "e7d8", "e7f8")))
    val entry = pending.proposalEntries.find(_.request.requirements.exists {
      case value: CompareMoves => value.id == "tactical_strength" && value.prefixMovesUci == najdorfGame.take(6) &&
        value.referenceMove == "b7c6" && value.alternativeMove == "e7f8"
      case _ => false
    }).get
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("q" * 32, "pending-bishop-defense", 10000L, pending))
    val selection = (wire \ "observations" \ "selection").as[JsObject]
    val proposal = (selection \ "proposals").as[List[JsObject]].find(value => (value \ "id").as[String] == entry.request.id).get
    val conditions = (proposal \ "requirements").as[List[JsObject]]
    val strength = conditions.find(value => (value \ "id").as[String] == "tactical_strength").get
    assertEquals((strength \ "prefix_moves_uci").as[List[String]], najdorfGame.take(6))
    assertEquals((strength \ "reference_move").as[String], "b7c6")
    assertEquals((strength \ "alternative_move").as[String], "e7f8")
    assertEquals((strength \ "state").as[String], "awaiting_engine")
    assertEquals((strength \ "answer").get, JsNull)
    val location = entry.request.requirements.collect {
      case fact: Fact if fact.predicate == Predicate("location", Some(Piece("black", "bishop", "e7"))) => fact
    }
    assertEquals(location.flatMap(_.expected).toSet, Set("e7", "f8"))
    location.foreach { fact =>
      val row = conditions.find(value => (value \ "id").as[String] == fact.id).get
      assertEquals((row \ "state").as[String], "supported")
      assertEquals((row \ "result").get, entry.readings.find(_.requirementId == fact.id).get.result.get.json)
      assertEquals((row \ "result" \ "samples").as[List[JsObject]].flatMap(value =>
        (value \ "value").asOpt[String]).distinct, fact.expected.toList)
    }
    val question = (selection \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "question_refs").asOpt[List[JsObject]].exists(_.exists(ref =>
        (ref \ "proposal_id").as[String] == entry.request.id && (ref \ "requirement_id").as[String] == "tactical_strength"))).get
    assertEquals((question \ "detail" \ "prefix_moves_uci").as[List[String]], najdorfGame.take(6))
    assertEquals((question \ "detail" \ "reference_move").as[String], "b7c6")
    assertEquals((question \ "detail" \ "alternative_move").as[String], "e7f8")
    assertEquals((question \ "detail" \ "evaluation_relation").get, JsNull)
    assertEquals((question \ "selection").as[String], "context")
    assert((proposal \ "question_text").as[String].contains("Bf8"))
    assert(!text.contains("check Bf8 instead of bxc6"), "the pending defense remains with its own question")

    val changed = begin(owner.currentFen, "b6b4", List(List("b6b4", "g3h3") -> 200, List("d8d7", "c3d5") -> 0, List("b6c7") -> 300),
      supplied = List(najdorfGame, defense), prefixOwner = Some(owner))
    assert(!deliveredComparisons(changed).contains("leaves e6 with exd5"), "a historical exchange cannot explain the changed current branch")
    val revised = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("q" * 32, "withdrawn-bishop-defense", 10000L, changed))
    assert(!(revised \ "observations" \ "selection" \ "candidates").as[List[JsObject]].exists(value =>
      (value \ "detail" \ "prefix_moves_uci").asOpt[List[String]].contains(najdorfGame.take(6)) &&
        (value \ "detail" \ "reference_move").asOpt[String].contains("b7c6") &&
        (value \ "detail" \ "alternative_move").asOpt[String].contains("e7f8")),
      "a counter to the withdrawn prefix has no current pending comparison")

  test("a stronger conditional child16 defense preserves the root while required22 verification is depth limited"):
    val owner = najdorfOwner
    val pending = awaiting(begin(owner.currentFen, "b6b4", List(najdorfGame -> 200, List("d8d7", "c3d5") -> 0, List("b6c7") -> 300),
      prefixOwner = Some(owner)))
    assertEquals(pending.issuedWork.searchAnchor.map(_.prefixMoves), Some(najdorfGame.take(6)))
    val defense = "e7f8 g3e3 e8e3 e2e3 c8e8 e3d3 b7c6 b2f6 g7f6 c2c4".split(" ").toList
    val after = submit(pending, List(defense -> -100, List("b7c6", "e2a6") -> 200, List("e7d8") -> 180))
      .asInstanceOf[CompletedCommentaryJob]
    assertEquals(after.verification.map(_.reason), Some("child_challenge"))
    assertEquals(after.verification.flatMap(_.completedDepth), Some(16))
    assertEquals(after.verification.map(_.targetDepth), Some(22))
    assertEquals(after.verification.map(_.state), Some("stopped"))
    assertEquals(after.verification.flatMap(_.stopReason), Some("depth_limit"))
    assert(after.assessmentNeedsRefresh)
    assertEquals(after.progress.physicalWorksIssued, pending.progress.physicalWorksIssued)
    assert(after.rootSearches.forall(_.work.requiredDepth <= 18))
    assert(after.responseSearches.exists(_.work.workId == pending.issuedWork.workId))
    assertEquals(RuntimeProtocol.completedResultJson(after), RuntimeProtocol.completedResultJson(pending.assessment.get),
      "a child score cannot replace the whole original public root judgment")
    assertEquals(after.rootSearches, pending.rootSearches, "the exact original root work and receipts remain the numerical owners")
    def rootValues(packet: EvidenceBackedJudgmentPacket) = packet.assembly.input.assessmentCandidates.map { candidate =>
      (candidate.rootMove, candidate.evaluation.moves,
        candidate.evaluation.engineLine.map(line => (line.scoreCp, line.mate, line.depth)))
    }
    val originalReview = pending.assessment.get.result.asInstanceOf[CompletedPositionCommentary.FocusedMoveReview].review
    val retainedReview = after.result.asInstanceOf[CompletedPositionCommentary.FocusedMoveReview].review
    assertEquals(rootValues(retainedReview.judgmentPacket), rootValues(originalReview.judgmentPacket),
      "completed child values remain separate from every original root candidate value")
    val entry = after.proposalEntries.find(entry => entry.request.origin.question.producer == "C-tactical-counter" &&
      entry.request.requirements.exists {
        case value: CompareMoves => value.id == "tactical_strength" && value.alternativeMove == "e7f8"
        case _ => false
      }).get
    val reading = entry.readings.find(_.requirementId == "tactical_strength").get
    assertEquals(reading.answer, None)
    val currentFacts = entry.readings.filter(_.requirementId.startsWith("tactical_current_"))
    assertEquals(currentFacts.size, 7)
    currentFacts.foreach { value =>
      assertEquals(value.state, "stopped")
      assertEquals(value.stopReason, Some("depth_limit"), "the unverified root context cannot authorize its strength comparison")
    }
    val context = entry.readings.find(_.requirementId == "tactical_context").get
    assertEquals(context.state, "stopped")
    assertEquals(context.stopReason, Some("depth_limit"))
    assertEquals(entry.readings.filter(_.requirementId.startsWith("tactical_direct_condition:")).map(_.state),
      List.fill(4)("supported"), "the independent legal board facts survive the numerical verification limit")
    assertEquals(reading.stopReason, Some("prerequisite_unresolved"))
    val search = after.responseSearches.find(_.work.workId == pending.issuedWork.workId).get
    val answer = search.pairAnswer(after.observationsPacket.get, "b7c6", "e7f8").get
    assertEquals(answer.evaluationRelation, "better", "the conditional answer remains with its exact owned report")
    assert(!deliveredComparisons(after).contains("Bf8 is stronger than bxc6"), "deeper verification must not present an obsolete answer as current")
    val comparison = entry.request.requirements.collectFirst { case value: CompareMoves if value.id == "tactical_strength" => value }.get
    val answered = entry.copy(readings = entry.readings.map(value =>
      if value.requirementId == comparison.id then pairReading(comparison, answer) else value))
    val challenge = CommentaryComparisonConflict.unansweredChallenge(after.observationsPacket.get, List(search), List(answered))
    assertEquals(challenge.map(_.prefix), Some(najdorfGame.take(6)))
    assertEquals(challenge.map(_.move), Some("b7c6"), "a root line keeping the disproved reply does not settle this counter")
    // The policy stops before a new root22 work exists. Do not submit a child
    // value as a made-up root receipt. The preceding changed-root test retains
    // the independent historical-prefix withdrawal assertions.

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
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("q" * 32, "pending-added-defender", 10000L, state))
    val selection = (wire \ "observations" \ "selection").as[JsObject]
    val proposal = (selection \ "proposals").as[List[JsObject]].find(value => (value \ "id").as[String] == entry.request.id).get
    val conditions = (proposal \ "requirements").as[List[JsObject]]
    val strength = conditions.find(value => (value \ "id").as[String] == "tactical_strength").get
    assertEquals((strength \ "prefix_moves_uci").as[List[String]], line.take(6))
    assertEquals((strength \ "reference_move").as[String], "b7c6")
    assertEquals((strength \ "alternative_move").as[String], "d8d7")
    assertEquals((strength \ "state").as[String], "deferred")
    assertEquals((strength \ "answer").get, JsNull)
    facts.filter(fact => fact.predicate == support || fact.predicate == capture).foreach { fact =>
      val row = conditions.find(value => (value \ "id").as[String] == fact.id).get
      assertEquals((row \ "state").as[String], "supported")
      assertEquals((row \ "result").get, entry.readings.find(_.requirementId == fact.id).get.result.get.json)
      assertEquals((row \ "result" \ "samples").as[List[JsObject]].flatMap(value =>
        (value \ "value").asOpt[String]).distinct, fact.expected.toList)
    }
    val question = (selection \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "question_refs").asOpt[List[JsObject]].exists(_.exists(ref =>
        (ref \ "proposal_id").as[String] == entry.request.id && (ref \ "requirement_id").as[String] == "tactical_strength"))).get
    assertEquals((question \ "detail" \ "prefix_moves_uci").as[List[String]], line.take(6))
    assertEquals((question \ "detail" \ "reference_move").as[String], "b7c6")
    assertEquals((question \ "detail" \ "alternative_move").as[String], "d8d7")
    assertEquals((question \ "detail" \ "evaluation_relation").get, JsNull)
    assertEquals((question \ "selection").as[String], "context")
    assert((proposal \ "question_text").as[String].contains("Rd7"))

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
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("q" * 32, "pending-checking-reply", 10000L, state))
    val selection = (wire \ "observations" \ "selection").as[JsObject]
    val proposal = (selection \ "proposals").as[List[JsObject]].find(value => (value \ "id").as[String] == entry.request.id).get
    val conditions = (proposal \ "requirements").as[List[JsObject]]
    val strength = conditions.find(value => (value \ "id").as[String] == "tactical_strength").get
    assertEquals((strength \ "prefix_moves_uci").as[List[String]], najdorfGame.take(6))
    assertEquals((strength \ "reference_move").as[String], "b7c6")
    assertEquals((strength \ "alternative_move").as[String], "f6g4")
    assertEquals((strength \ "state").as[String], "deferred")
    assertEquals((strength \ "answer").get, JsNull)
    facts.foreach { fact =>
      val row = conditions.find(value => (value \ "id").as[String] == fact.id).get
      assertEquals((row \ "state").as[String], "supported")
      assertEquals((row \ "result").get, entry.readings.find(_.requirementId == fact.id).get.result.get.json)
      assertEquals((row \ "result" \ "samples").as[List[JsObject]].flatMap(value =>
        (value \ "value").asOpt[String]).distinct, fact.expected.toList)
    }
    val question = (selection \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "question_refs").asOpt[List[JsObject]].exists(_.exists(ref =>
        (ref \ "proposal_id").as[String] == entry.request.id && (ref \ "requirement_id").as[String] == "tactical_strength"))).get
    assertEquals((question \ "detail" \ "prefix_moves_uci").as[List[String]], najdorfGame.take(6))
    assertEquals((question \ "detail" \ "reference_move").as[String], "b7c6")
    assertEquals((question \ "detail" \ "alternative_move").as[String], "f6g4")
    assertEquals((question \ "detail" \ "evaluation_relation").get, JsNull)
    assertEquals((question \ "selection").as[String], "context")
    assert((proposal \ "question_text").as[String].contains("Ng4+"))

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
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("q" * 32, "pending-interposition", 10000L, state))
    val selection = (wire \ "observations" \ "selection").as[JsObject]
    val proposal = (selection \ "proposals").as[List[JsObject]].find(value => (value \ "id").as[String] == entry.request.id).get
    val conditions = (proposal \ "requirements").as[List[JsObject]]
    val strength = conditions.find(value => (value \ "id").as[String] == "tactical_strength").get
    val strengthReading = entry.readings.find(_.requirementId == "tactical_strength").get
    assert(Set("deferred", "awaiting_engine")(strengthReading.state))
    assertEquals((strength \ "prefix_moves_uci").as[List[String]], prefix)
    assertEquals((strength \ "reference_move").as[String], "c6b7")
    assertEquals((strength \ "alternative_move").as[String], "g3e3")
    assertEquals((strength \ "state").as[String], strengthReading.state)
    assertEquals((strength \ "answer").get, JsNull)
    val obstruction = entry.request.requirements.collect {
      case fact: Fact if fact.predicate == Predicate("control", Some(Piece("black", "rook", "d8")), square = Some("e2")) => fact
    }
    assertEquals(obstruction.flatMap(_.expected).toSet, Set("absent", "present"))
    obstruction.foreach { fact =>
      val row = conditions.find(value => (value \ "id").as[String] == fact.id).get
      assertEquals((row \ "state").as[String], "supported")
      assertEquals((row \ "result").get, entry.readings.find(_.requirementId == fact.id).get.result.get.json)
      assertEquals((row \ "result" \ "samples").as[List[JsObject]].flatMap(value =>
        (value \ "value").asOpt[String]).distinct, fact.expected.toList)
    }
    val question = (selection \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "question_refs").asOpt[List[JsObject]].exists(_.exists(ref =>
        (ref \ "proposal_id").as[String] == entry.request.id && (ref \ "requirement_id").as[String] == "tactical_strength"))).get
    assertEquals((question \ "detail" \ "prefix_moves_uci").as[List[String]], prefix)
    assertEquals((question \ "detail" \ "reference_move").as[String], "c6b7")
    assertEquals((question \ "detail" \ "alternative_move").as[String], "g3e3")
    assertEquals((question \ "detail" \ "evaluation_relation").get, JsNull)
    assertEquals((question \ "selection").as[String], "context")
    assert((proposal \ "question_text").as[String].contains("Re3"))
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
    val revised = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("q" * 32, "withdrawn-interposition", 10000L, changed))
    assert(!(revised \ "observations" \ "selection" \ "candidates").as[List[JsObject]].exists(value =>
      (value \ "detail" \ "prefix_moves_uci").asOpt[List[String]].contains(prefix) &&
        (value \ "detail" \ "reference_move").asOpt[String].contains("c6b7") &&
        (value \ "detail" \ "alternative_move").asOpt[String].contains("g3e3")),
      "the old reply cannot keep an unverified comparison current after the PV selects the interposition")

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
      assertEquals(entry.readings.find(_.requirementId == "tactical_strength").flatMap(_.answer).flatMap(_.qualityBand),
        Some(PrincipalVariationComparison.QualityBand.Close), "the raw ordering is retained within the close band")
      assert(text.contains(s"Bf8 evaluates ${if relation == "equal" then "equal to" else "below"} bxc6"), text)
      assert(!text.contains("Bf8 is weaker than bxc6"), text)
      assert(text.contains("beginning Bf8") && text.contains("beginning bxc6"), text)
      assert(text.contains("white loses 1 rook"), text)
      assertEquals(text.contains("black loses 1 rook"), returned.size == defense.size,
        "the longer study line cannot donate a recapture to the answer's shorter engine line")
      assertEquals(text.contains("the capturer on Rxe3 is itself captured on Qxe3"), returned.size == defense.size)
      assert(text.contains("not a forced result or the cause of the entire root evaluation"), text)
    }

  test("the same Nd5 distinguishes defensive support and its actual recapture use"):
    val owner = najdorfOwner
    // Legal mechanism contrast supplied by this study; no author or engine verdict is invented.
    val bare = "b6b4 c3d5 e6d5 e4d5 c6d5 e2e7".split(" ").toList
    val defended = "d8d7 c3d5 e6d5 e4d5 c6d5 e2e7 d7e7".split(" ").toList
    val state = begin(owner.currentFen, "b6b4", List(bare -> 100, defended -> 0, List("b6c7") -> 200), prefixOwner = Some(owner))
    val text = deliveredComparisons(state)
    assert(text.contains("Against the common reply Nd5"), text)
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
    assert(recoveredText.contains("Against the common reply Nd5"), recoveredText)
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
        assert(text.contains("leaves a3 with Rb3"), text)
        assert(!text.contains("leaves a2 with Ra3"), text)
      if moves.startsWith("c3a5") then
        assert(text.contains("Black rook on a5 is removed by Bxa5"), text)
        assert(text.contains("White bishop on a5 leaves a5 with Bc7"), text)
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
    val packet = state.observationsPacket.get
    val comparisons = CommentaryProposal.comparisonCandidates(packet, ObservedLineProtocol.prepare(packet), state.proposalEntries)
      .map(value => (value \ "detail" \ "text").as[String])
    assert(comparisons.exists(_.contains("gives up the recapture")),
      "the actual comparison must display the lost recapture: " + comparisons.mkString(" "))
    assert(idea.questionFocus(packet, ObservedLineProtocol.prepare(packet), idea.decisions.flatMap { (member, value, reading) =>
      reading.answer.map(CommentaryIdea.AnsweredComparison(member.entry.request.id, value, _))
    }.toSet).isEmpty, "a lost recapture cannot become a beneficial preparation dependency")

  private lazy val focusedPreparation =
    val fen = "6k1/7p/5p2/6p1/8/4B3/8/K7 b - - 0 1"
    val plan = List("h7h6", "a1a2", "f6f5")
    val state = begin(fen, "h7h6", List(plan -> -100, List("f6f5") -> -50, List("g5g4") -> -20),
      List(plan ++ List("e3g5", "h6g5"), List("f6f5", "e3g5")))
    val packet = state.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val drafts = scala.collection.mutable.ListBuffer.empty[(JsObject, List[MoveWorkProjection.Context])]
    CommentaryProposal.comparisonCandidates(packet, prepared, state.proposalEntries,
      Some((candidate, context) =>
        drafts += ((candidate, context))
        candidate))
    val (candidate, context) = drafts.find(_._2.exists(_.question.nonEmpty)).getOrElse(fail("missing preparation dependency"))
    (state, prepared, MoveWorkProjection.select(packet, prepared), candidate, context)

  test("the answered preparation keeps its exact condition and test with their evidence ownership"):
    val (state, prepared, projection, candidate, context) = focusedPreparation
    val anchored = context.find(_.question.nonEmpty).get
    val focus = anchored.question.get
    assertEquals(focus.key.action.subject, Some(Piece("black", "pawn", "f6")))
    assertEquals(focus.mechanism.after.predicate, Predicate("control", Some(Piece("black", "pawn", "h7")), square = Some("g5")))
    assertEquals(focus.owner.comparison.alternativeMove, "f6f5")
    val result = projection.withMoveEffects(candidate, context = context)
    val text = (result \ "detail" \ "text").as[String]
    val test = anchored.questionTest.get
    val retention = (result \ "detail" \ "effects").as[List[JsObject]].find(effect =>
      (effect \ "scope").as[String] == "support_retention").get
    assertEquals((retention \ "actor" \ "square").as[String], "h6")
    assertEquals((retention \ "target" \ "square").as[String], "g5")
    assertEquals((retention \ "through_plies").as[List[Int]], List(3))
    assert(text.contains(test) && !text.contains("may help prepare") && !text.contains("absent versus present"), text)
    val baseline = projection.withMoveEffects(candidate, context = context.map(_.copy(question = None)))
    val before = (baseline \ "detail" \ "text").as[String]
    assert(before.contains(anchored.text), "unowned context cannot have its purpose or condition rewritten")
    assertEquals(result - "detail" - "evidence_ids", baseline - "detail" - "evidence_ids")
    assertEquals((result \ "evidence_ids").as[List[String]].toSet, (baseline \ "evidence_ids").as[List[String]].toSet,
      "integrating the matched explanation preserves the same evidence ownership")
    val reworded = projection.withMoveEffects(candidate, context = context.map(value =>
      if value.question.nonEmpty then value.copy(text = "The same preparation has different introductory wording.") else value))
    assertEquals((reworded \ "detail" \ "effects").as[List[JsObject]].toSet,
      (result \ "detail" \ "effects").as[List[JsObject]].toSet)
    assertEquals((reworded \ "evidence_ids").as[List[String]].toSet, (result \ "evidence_ids").as[List[String]].toSet)
    assert((reworded \ "detail" \ "text").as[String].contains(test),
      "the verified condition test is retained independently of introductory wording")
    val actualEffects = (result \ "detail" \ "effects").as[List[JsObject]].toSet
    val baselineEffects = (baseline \ "detail" \ "effects").as[List[JsObject]].toSet
    val absorbed = baselineEffects.filter(effect => (effect \ "scope").as[String] == "line_history" &&
      (effect \ "actor" \ "square").asOpt[String].contains("h6") &&
      (effect \ "target" \ "square").asOpt[String].contains("g5") &&
      (effect \ "through_plies").as[List[Int]] == List(1))
    assertEquals(absorbed.size, 1, "only the integrated immediate support is absorbed")
    assertEquals(actualEffects, baselineEffects -- absorbed, "independent h5 and other meanings remain")
    val retained = actualEffects.find(effect => (effect \ "scope").as[String] == "support_retention").get
    assert(absorbed.flatMap(effect => (effect \ "evidence_ids").as[List[String]])
      .subsetOf((retained \ "evidence_ids").as[List[String]].toSet), "the included support keeps its complete proof")
    val returned = prepared.paths.find(_.line.ref.id == focus.owner.answer.referenceLineId).get
    assertEquals(returned.moves, List("h7h6", "a1a2", "f6f5"))
    assert(!(result \ "detail" \ "effects").as[List[JsObject]].exists(effect =>
      (effect \ "through_plies").as[List[Int]].exists(_ > returned.moves.size)),
      "the supplied Bxg5 hxg5 condition test is not an event in the returned PV")
    val normal = RuntimeProtocol.observationsJson(state.observationsPacket.get, prepared = Some(prepared),
      proposalEntries = state.proposalEntries)
    val delivered = (normal \ "selection" \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").asOpt[String].contains("question_comparison") &&
      (value \ "detail" \ "alternative_move").asOpt[String].contains("f6f5")).get
    val normalText = (delivered \ "detail" \ "text").as[String]
    assert(normalText.contains(test) && !normalText.contains("may help prepare"),
      "the earlier root body's ownership cannot remove this independent answer's complete condition: " + normalText)
    assert((delivered \ "detail" \ "effects").as[List[JsObject]].exists(effect =>
      (effect \ "scope").as[String] == "support_retention" && (effect \ "through_plies").as[List[Int]] == List(3)))
    assert(!(normal \ "selection" \ "candidates").as[List[JsObject]].exists(value =>
      (value \ "detail" \ "alternative_move").asOpt[String].contains("g5g4") &&
        (value \ "detail" \ "question_refs").asOpt[List[JsObject]].getOrElse(Nil).exists(ref =>
          (ref \ "proposal_id").asOpt[String].contains(focus.owner.proposalId) &&
            (ref \ "requirement_id").asOpt[String].contains(focus.owner.comparison.id))),
      "the condition test retains its exact question receipt rather than moving to the g4 question")
    sys.props.get("chesstory.reference-output").foreach { directory =>
      val out = java.nio.file.Path.of(directory)
      java.nio.file.Files.createDirectories(out)
      java.nio.file.Files.writeString(out.resolve("normal-preparation.json"),
        Json.stringify(normal))
      java.nio.file.Files.writeString(out.resolve("preparation-order.json"), Json.stringify(Json.obj("before" -> baseline, "after" -> result)))
    }

  test("a verified question keeps its exact condition and use in the root answer, with independent content in detail"):
    val (state, prepared, projection, candidate, context) = focusedPreparation
    val focus = context.find(_.question.nonEmpty).get.question.get
    val root = candidate + ("detail" -> ((candidate \ "detail").as[JsObject] ++ Json.obj(
      "type" -> "root_comparison", "reference_line_id" -> focus.owner.answer.referenceLineId,
      "candidate_line_id" -> focus.owner.answer.alternativeLineId)))
    def render(parts: List[MoveWorkProjection.Context]) =
      projection.withSharedMoveEffects(List(root -> Nil, candidate -> parts)).head
    val result = render(context)
    val answer = (result \ "detail" \ "summary_question").as[JsObject]
    sys.props.get("chesstory.reference-output").foreach { directory =>
      val out = java.nio.file.Path.of(directory)
      java.nio.file.Files.createDirectories(out)
      java.nio.file.Files.writeString(out.resolve("selected-question.json"), Json.stringify(Json.obj(
        "without_verified_focus" -> render(context.map(_.copy(question = None))), "with_verified_focus" -> result,
        "standalone" -> projection.withMoveEffects(root, context = context),
        "branch" -> focus.key.branch, "condition" -> focus.mechanism.after.predicate.toString,
        "action" -> focus.key.action.toString, "reference" -> focus.owner.answer.referenceLineId)))
    }
    assertEquals((answer \ "question").as[String], "investigated_mechanism")
    val path = prepared.paths.find(_.line.ref.id == focus.owner.answer.referenceLineId).get
    def references(field: String) = (answer \ field).as[List[JsObject]].flatMap(unit =>
      (unit \ "en").as[List[JsValue]].collect { case ref: JsObject => ref }).map(ref =>
        (ref \ "line_id").as[String] -> (ref \ "occurrence_id").as[String])
    assert(references("units").contains(path.line.ref.id -> path.ids(0)))
    assert(references("units").contains(path.line.ref.id -> path.ids(2)),
      "the actual f-pawn departure remains with the new h-pawn support")
    val words = (answer \ "units").as[List[JsObject]].flatMap(unit =>
      (unit \ "en").as[List[JsValue]].collect { case JsString(s) => s }).mkString
    assert(words.contains("pawn on h6") && words.contains("pawn on g5"), words)
    assert(words.contains("pawn on f5"), "the alternative's exact old-provider duty is part of this comparison: " + words)
    assert(!words.contains("h5"), "independent control of h5 does not answer the g5 support question")
    assert((answer \ "detail_units").as[List[JsObject]].nonEmpty, "independent facts remain expandable")
    val reworded = render(context.map(part => if part.question.nonEmpty then part.copy(text = "Changed introduction.") else part))
    assertEquals((reworded \ "detail" \ "summary_question").as[JsObject], answer,
      "content selection does not depend on introductory wording")
    val stale = context.map(part => part.copy(question = part.question.map(value =>
      value.copy(key = value.key.copy(action = value.key.action.copy(subject = Some(Piece("black", "pawn", "g5"))))))))
    assertEquals(render(stale), render(context.map(_.copy(question = None))),
      "an unrelated physical action cannot acquire this answer")
    val normal = RuntimeProtocol.observationsJson(state.observationsPacket.get, prepared = Some(prepared),
      proposalEntries = state.proposalEntries)
    val published = (normal \ "selection" \ "candidates").as[List[JsObject]]
    val ids = published.map(value => (value \ "id").as[String]).toSet
    val quietOneMove = prepared.continuationReadings.filter(reading => reading.path.moves.size == 1 &&
      reading.events.isEmpty && reading.captureExposures.isEmpty).map(_.id).toSet
    assert(quietOneMove.nonEmpty && ids.intersect(quietOneMove).isEmpty,
      "evidence reading does not add empty one-move trajectory cards")
    published.foreach(value => assert((value \ "detail" \ "continuation_candidate_ids").asOpt[List[String]]
      .getOrElse(Nil).forall(ids), "all public trajectory links name published candidates"))

  test("a question dependency cannot move to another question, branch or physical action"):
    val (_, _, projection, candidate, context) = focusedPreparation
    val anchored = context.find(_.question.nonEmpty).get
    val focus = anchored.question.get
    def render(value: CommentaryIdea.QuestionFocus) = projection.withMoveEffects(candidate,
      context = context.map(part => if part == anchored then part.copy(question = Some(value)) else part))
    val fallback = projection.withMoveEffects(candidate, context = context.map(_.copy(question = None)))
    val variants = List(
      focus.copy(owner = focus.owner.copy(proposalId = "another-question")),
      focus.copy(owner = focus.owner.copy(answer = focus.owner.answer.copy(referenceLineId = "another-line"))),
      focus.copy(key = focus.key.copy(anchor = List("h7h6"))),
      focus.copy(key = focus.key.copy(action = focus.key.action.copy(subject = Some(Piece("black", "pawn", "g5")))))
    )
    variants.foreach(value => assertEquals(render(value), fallback))
    val owner = focus.owner
    val idea = CommentaryIdea.read(focusedPreparation._1.proposalEntries).find(_.key == focus.key).get
    assertEquals(idea.questionFocus(focusedPreparation._1.observationsPacket.get, focusedPreparation._2,
      Set(owner.copy(answer = owner.answer.copy(executionKeySha256 = "stale-receipt")))), Nil)

  test("the preparation condition belongs to the response target's current square, not a same-id unrelated control"):
    val (state, prepared, _, _, context) = focusedPreparation
    val focus = context.find(_.question.nonEmpty).get.question.get
    val idea = CommentaryIdea.read(state.proposalEntries).find(_.key == focus.key).get
    assertEquals(focus.mechanism.action.predicate.target, Some(Piece("white", "bishop", "e3")))
    assertEquals(focus.mechanism.after.predicate.square, Some("g5"), "the bishop actually moves from e3 to g5")
    val changed = idea.copy(members = idea.members.map { member =>
      val facts = member.entry.request.requirements.map {
        case fact: Fact if fact == focus.mechanism.before || fact == focus.mechanism.after =>
          fact.copy(predicate = fact.predicate.copy(square = Some("f5")))
        case other => other
      }
      member.copy(entry = member.entry.copy(request = member.entry.request.copy(requirements = facts)))
    })
    assert(changed.mechanism, "the old existential verdict alone cannot identify the exact condition")
    assertEquals(changed.questionFocus(state.observationsPacket.get, prepared, Set(focus.owner)), Nil)

  test("a rejected complete question explanation cannot consume a later fitting explanation"):
    val (_, _, projection, candidate, context) = focusedPreparation
    val anchored = context.find(_.question.nonEmpty).get
    val oversized = anchored.copy(additional = List("Independent qualification. " * 200))
    val complete = projection.withMoveEffects(candidate, context = List(anchored))
    assertEquals(projection.withMoveEffects(candidate, context = List(oversized, anchored)), complete)
    val rejected = projection.withMoveEffects(candidate, context = List(oversized))
    assert(!(rejected \ "detail" \ "text").as[String].contains("may help prepare"),
      "a rejected dependency keeps neither a bare purpose nor a partial condition")
    val fallback = projection.withMoveEffects(candidate)
    assertEquals((rejected \ "detail" \ "effects").as[List[JsObject]].toSet,
      (fallback \ "detail" \ "effects").as[List[JsObject]].toSet,
      "a rejected question bundle preserves the existing physical explanations, including their costs and replies")
    assertEquals((rejected \ "evidence_ids").as[List[String]].toSet, (fallback \ "evidence_ids").as[List[String]].toSet)
    assert((complete \ "detail" \ "text").as[String].length <= 4000)

  test("a question about the bishop's use of a cleared square does not promote a later independent rook use"):
    val plan = List("d2a5", "h8h7", "c1e3", "h7h8", "d1d7")
    val state = begin("7k/8/8/8/8/8/3B4/2BR3K w - - 0 1", "d2a5",
      List(plan -> 100, List("d2e3", "h8h7") -> 50, List("h1h2") -> 0))
    val packet = state.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val projection = MoveWorkProjection.select(packet, prepared)
    val rendered = scala.collection.mutable.ListBuffer.empty[(JsObject, List[MoveWorkProjection.Context])]
    CommentaryProposal.comparisonCandidates(packet, prepared, state.proposalEntries,
      Some((candidate, context) =>
        val result = projection.withMoveEffects(candidate, context = context)
        rendered += ((result, context))
        result))
    val (candidate, context) = rendered.find(_._2.exists(_.question.exists(focus =>
      focus.key.action.subject.contains(Piece("white", "bishop", "c1"))))).getOrElse(fail("missing bishop-use question"))
    val paragraph = context.find(_.question.nonEmpty).get.text
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains(paragraph) && text.contains("Rd7"), text)

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
    assert(!idea.text.contains("may help prepare"), "a changed location alone still cannot establish a mechanism")

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
    assert(idea.text.contains("g5g4 is stronger"), "the delivered revision identifies the tested stronger choice")
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
      assert(text.contains(s"Nb5 ${if differentTarget then "Bxc4" else "Bxb5"}"), text)
      val public = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("p" * 32, "practical-decision", 10000L, after))
      val proposals = (public \ "observations" \ "selection" \ "proposals").as[List[JsObject]]
      assert(proposals.exists(value => (value \ "answer_text").asOpt[String].exists(_.contains(text))),
        "continuation explanation must reach the normal response, not only an internal helper")
    }

  test("an already explained withdrawal still supports the new arm's maintained attack"):
    val fen = "r6k/8/8/8/8/8/8/R6K w - - 0 1"
    val withdrawn = List("a1a2", "h8g8", "a2b2")
    val maintained = List("a1a3", "h8g8", "a3a4")
    val state = begin(fen, withdrawn.head,
      List(maintained -> 20, withdrawn -> 0, List("h1h2") -> -10))
    val packet = state.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val candidate = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("w" * 32, "fresh-maintained-attack", 10000L, state)) \ "observations" \ "selection" \ "candidates")
      .as[List[JsObject]].find(value => (value \ "detail" \ "comparison_kind").asOpt[String].contains("played_vs_best")).get
    val lineIds = (candidate \ "line_ids").as[List[String]]
    def ownedLine(moves: List[String]): String =
      val path = prepared.paths.find(path => path.moves == moves && lineIds.contains(path.line.ref.id)).get
      assert(path.certified.nonEmpty, "both complete histories must belong to the observed comparison")
      path.line.ref.id
    val withdrawnId = ownedLine(withdrawn)
    val maintainedId = ownedLine(maintained)
    val detail = (candidate \ "detail").as[JsObject]
    assertEquals((detail \ "material_relation").as[String], "equal")
    // Exercise effect emission independently of which visible comparison first owns the withdrawal.
    val base = candidate + ("detail" -> ((detail - "effects") +
      ("text" -> JsString("The observed material balance is equal."))))
    val projection = MoveWorkProjection.select(packet)
    val complete = projection.withMoveEffects(base, Set.empty)
    val completeEffects = (complete \ "detail" \ "effects").as[List[JsObject]]
    def historyEffect(lineId: String): JsObject = completeEffects.find(effect =>
      (effect \ "scope").as[String] == "line_history" &&
        (effect \ "target" \ "square").as[String] == "a8" &&
        (effect \ "line_ids").as[List[String]] == List(lineId)).get
    val explained = historyEffect(withdrawnId)
    val expectedFresh = historyEffect(maintainedId)
    val fresh = projection.withMoveEffects(base, Set(explained))
    val text = (fresh \ "detail" \ "text").as[String]
    val effects = (fresh \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil)
    assert(text.contains("The other continuation for the compared Black rook originally on a8"), text)
    assert(text.contains("original attacker still attacks it there"), text)
    assert(!text.contains("Rb2:") && !text.contains("ends its attack on"),
      "the earlier owner retains the withdrawal's body: " + text)
    assertEquals(effects.count(_ == expectedFresh), 1, "the newly explained maintained attack keeps its exact evidence")
    assert(!effects.contains(explained), "an already owned history is not emitted a second time")
    assertEquals((expectedFresh \ "through_plies").as[List[Int]], List(maintained.size))

  private def currentEffectResponse(fen: String, played: String, rows: List[(List[String], Int)],
      supplied: List[List[String]] = Nil, prefixOwner: Option[CanonicalPositionHistory] = None,
      output: Option[String] = None): (CommentaryJobState, JsObject, JsObject) =
    val owner = prefixOwner.getOrElse(history(fen))
    (rows.map(_._1) ++ supplied).foreach(moves =>
      assert(owner.extend(moves).isRight, s"the entire inline line must be legal: ${moves.mkString(" ")}"))
    val state = begin(fen, played, rows, supplied, prefixOwner)
    val obs = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("v" * 32, "current-move-use", 10000L, state)) \ "observations").as[JsObject]
    val candidate = (obs \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "type").as[String] == "root_comparison").get
    for directory <- sys.props.get("chesstory.reference-output"); name <- output do
      val out = java.nio.file.Path.of(directory)
      java.nio.file.Files.createDirectories(out)
      java.nio.file.Files.writeString(out.resolve(name + ".json"), Json.stringify(obs))
    (state, obs, candidate)

  private def summaryCoreRefs(candidate: JsObject, paths: ObservedLineProtocol.PreparedPaths):
      Set[(ObservedLineProtocol.PreparedPath, Int)] =
    val owners = (candidate \ "line_ids").as[List[String]].toSet
    val units = (candidate \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
    def refs(value: JsValue) = value.as[List[JsValue]].collect { case ref: JsObject => ref }
    units.foreach { unit =>
      val visible = refs(unit("en"))
      assertEquals(visible.toSet, refs(unit("ko")).toSet)
      (unit \ "required_moves").asOpt[List[JsObject]].getOrElse(Nil).foreach(ref =>
        assert(visible.contains(ref), "the chosen answer must display each required move"))
      refs(unit("covered_moves")).foreach(ref => assert(visible.contains(ref), "a covered cost must be visible"))
    }
    units.flatMap(unit => refs(unit("en"))).map { ref =>
      val line = (ref \ "line_id").as[String]
      assert(owners(line), "summary references must remain on the compared paths")
      val path = paths.paths.find(_.line.ref.id == line).get
      val occurrence = (ref \ "occurrence_id").as[String]
      val offset = path.ids.indexOf(occurrence)
      assert(offset >= 0 && (candidate \ "evidence_ids").as[List[String]].contains(occurrence))
      path -> offset
    }.toSet

  test("a supplied countercheck trade keeps both colors' captures and stops at its owned horizon"):
    val cases = List(
      ("6rk/1b6/8/8/8/8/6P1/R2Q2K1 w - - 0 1",
        "d1d5 g8g2 g1g2 b7d5 g2h3", "d1d2", "g1f1", 20,
        List("Qd5", "Rxg2+", "Kxg2", "Bxd5+", "Kh3"), "White", "Black"),
      ("r2q2k1/6p1/8/8/8/8/1B6/6RK b - - 0 1",
        "d8d4 g1g7 g8g7 b2d4 g7h6", "d8d7", "g8f8", -20,
        List("Qd4", "Rxg7+", "Kxg7", "Bxd4+", "Kh6"), "Black", "White")
    )
    cases.foreach { (fen, moves, alternative, third, score, events, mover, opponent) =>
      val line = moves.split(" ").toList
      val (state, obs, candidate) = currentEffectResponse(fen, line.head,
        List(line -> score, List(alternative) -> 0, List(third) -> -score))
      val text = (candidate \ "detail" \ "text").as[String]
      events.foreach(event => assert(text.contains(event), text))
      assert(text.contains(s"$mover loses one pawn and one queen") &&
        text.contains(s"$opponent loses one rook"), text)
      assert(text.contains("not a forced outcome") && text.length <= 4000, text)
      assert(text.contains("\n\n"), "independent explanation units remain separate paragraphs")
      val summary = (candidate \ "detail" \ "summary").as[String]
      events.foreach(event => assert(summary.contains(event), summary))
      val paths = ObservedLineProtocol.prepare(state.observationsPacket.get)
      val path = paths.paths.find(_.moves == line).get
      val visible = summaryCoreRefs(candidate, paths)
      line.indices.foreach(offset => assert(visible(path -> offset), "both checking captures keep their actual king responses"))
      val material = path.certified.flatMap(_._2.material).get
      def lostBy(side: String) = material.captures.filter(_.side.white != (side == "White"))
        .map(_.capturedRole.name).sorted
      assertEquals(lostBy(mover), List("pawn", "queen"))
      assertEquals(lostBy(opponent), List("rook"))
      val units = (candidate \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
      val covered = units.flatMap(unit => unit("covered_moves").as[List[JsObject]]).toSet
      assertEquals(covered, List(1, 2, 3).map(RootQuestionSummary.MoveRef(path, _).json).toSet,
        "all three actual costs belong to this displayed countercheck sequence")
      val reading = paths.continuationReadings.find(_.path eq path).get
      val expected = List(
        (1, Piece(opponent.toLowerCase, "rook", line(1).take(2)),
          Piece(mover.toLowerCase, "pawn", line(1).slice(2, 4)), false),
        (2, Piece(mover.toLowerCase, "king", line(2).take(2)),
          Piece(opponent.toLowerCase, "rook", line(1).take(2)), true),
        (3, Piece(opponent.toLowerCase, "bishop", line(3).take(2)),
          Piece(mover.toLowerCase, "queen", line.head.take(2)), false))
      expected.foreach { (offset, actor, victim, recapture) =>
        val event = reading.events.find(_.plyOffset == offset).get
        val capture = material.captures.find(_.plyOffset == offset).get
        assertEquals(event.originalActor, Some(actor.witness))
        assertEquals(event.originalVictim, Some(victim.witness))
        assertEquals(capture.attackerRole.name, actor.role)
        assertEquals(capture.capturedRole.name, victim.role)
        assertEquals(capture.recapture, recapture)
        val ref = RootQuestionSummary.MoveRef(path, offset).json
        val priceProof = units.filter(unit => unit("covered_moves").as[List[JsObject]].contains(ref))
          .flatMap(unit => (unit \ "proof_moves").asOpt[List[JsObject]].getOrElse(
            unit("en").as[List[JsValue]].collect { case move: JsObject => move }))
        assert(priceProof.contains(ref), "the summarized price retains the exact physical capture: " + summary)
      }
      val priceWords = units.filter(unit => unit("role").as[String] == "cost")
        .flatMap(unit => unit("en").as[List[JsValue]].collect { case JsString(words) => words }).mkString
      List("one pawn", "one queen", "one rook").foreach(price => assert(priceWords.contains(price), priceWords))
      assert(summary.length < text.length, summary)
      assertPublishedEffectSources(obs, candidate)
      val (_, shortObs, short) = currentEffectResponse(fen, line.head,
        List(line.take(2) -> score, List(alternative) -> 0, List(third) -> -score), supplied = List(line))
      val shortText = (short \ "detail" \ "text").as[String]
      events.drop(2).foreach(event => assert(!shortText.contains(event), shortText))
      (short \ "detail" \ "summary").asOpt[String].foreach(summary =>
        events.drop(2).foreach(event => assert(!summary.contains(event), summary)))
      assertPublishedEffectSources(shortObs, short)
    }

  test("the comparison retains the observed bishop development and alternative pawn option"):
    val (_, obs, candidate) = currentEffectResponse("7k/8/8/8/8/8/3P2P1/5BK1 w - - 0 1", "g2g3",
      List(List("g2g3", "h8h7", "f1g2") -> 20,
        List("d2d4", "h8h7", "g2g4") -> 0, List("d2d3") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("g3 clears g2") && text.contains("Bg2"), text)
    assert(text.contains("route to g4 clear"), "the witnessed alternative remains available: " + text)
    assertPublishedEffectSources(obs, candidate)

  test("choice wording establishes the two current movers' common target with both witnesses"):
    val (state, obs, candidate) = currentEffectResponse("1r5k/8/3p4/8/8/1N6/2Q5/4R1K1 w - - 0 1", "c2d2",
      List(List("e1d1", "h8g8") -> 20, List("c2d2", "h8g8") -> 0, List("e1b1") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    List("Rd1", "Qd2", "pawn on d6").foreach(value => assert(text.contains(value), text))
    assert(text.contains("Qd2 gives up that queen's support of White knight on b3; " +
      "after Rd1, White queen on c2 still supports White knight on b3."), text)
    val contrast = (candidate \ "detail" \ "effects").as[List[JsObject]].find(effect =>
      (effect \ "scope").as[String] == "pair_difference" &&
        (effect \ "actor" \ "square").as[String] == "c2" &&
        (effect \ "target" \ "square").asOpt[String].contains("b3")).get
    assertEquals((contrast \ "line_ids").as[List[String]].toSet, (candidate \ "line_ids").as[List[String]].toSet)
    assertEquals((contrast \ "through_plies").as[List[Int]], List(1, 1))
    assertEquals((contrast \ "occurrence_ids").as[List[String]].size, 2)
    val projection = MoveWorkProjection.select(state.observationsPacket.get)
    val notes = projection.local.filter(note =>
      Set("e1d1", "c2d2")(note.edge.moveUci) && (note.note.detail match
        case MoveWorkProjection.Detail.Attack(actor, target, _) =>
          Set("d1", "d2")(actor.square.key) && target.square.key == "d6"
        case _ => false))
    assertEquals(notes.map(_.edge.moveUci).toSet, Set("e1d1", "c2d2"))
    val evidence = (candidate \ "evidence_ids").as[List[String]].toSet
    notes.foreach(note => assert(note.note.afterSources.toSet.subsetOf(evidence),
      "each current attack needs its own positive control witnesses"))
    assertPublishedEffectSources(obs, candidate)

  test("a support contrast needs the same provider and complete evidence of its loss"):
    val fen = "1r5k/8/3p4/8/2B5/1N6/2Q5/4R1K1 w - - 0 1"
    val (state, _, candidate) = currentEffectResponse(fen, "c2d2",
      List(List("e1d1") -> 20, List("c2d2") -> 0, List("e1b1") -> -20))
    val packet = state.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val afterLoss = prepared.paths.find(_.moves == List("c2d2")).get.positions(1)
    val afterPeer = prepared.paths.find(_.moves == List("e1d1")).get.positions(1)
    val base = candidate + ("detail" -> (((candidate \ "detail").as[JsObject] - "effects") +
      ("text" -> JsString("The observed material balance is equal."))))
    // Keep the bishop's support: another provider cannot stand in for the queen.
    List(afterLoss -> "d2", afterPeer -> "c2").foreach { (position, square) =>
      val records = packet.assembly.evidenceGraph.records.filterNot(record => record.ref.position == position &&
        (record.payload match
          case fact: RelationFactEvidence => fact.detail match
            case RelationWitnessDetail.GeometricControl(_, from, _, _) => from.key == square
            case _ => false
          case _ => false))
      assert(records.size < packet.assembly.evidenceGraph.records.size)
      val incomplete = EvidenceBackedJudgmentPacket.fromAssembly(packet.assembly.copy(
        evidenceGraph = TypedEvidenceGraph.empty.addAll(records))).get
      val rendered = MoveWorkProjection.select(incomplete).withMoveEffects(base)
      assert(!(rendered \ "detail" \ "text").as[String].contains("after Rd1, White queen on c2 still supports"))
    }
    val (_, _, bothLose) = currentEffectResponse(fen, "c2d2",
      List(List("c2e2") -> 20, List("c2d2") -> 0, List("e1d1") -> -20))
    assert(!(bothLose \ "detail" \ "text").as[String].contains("still supports White knight on b3"),
      "an unselected Rd1 arm or the bishop cannot supply the queen's retained support")

  test("choice wording cannot borrow a common target from a later move"):
    val (_, obs, candidate) = currentEffectResponse("7k/8/3p4/8/8/1N6/2Q5/4R1K1 w - - 0 1", "c2d2",
      List(List("e1e2", "h8g8", "e2d2") -> 20,
        List("c2d2", "h8g8") -> 0, List("e1b1") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(!text.contains("Both Re2 and Qd2 attack"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 a current attack remains explainable without a later capture or evasion"):
    val fen = "r2qkb1r/pp1bnppp/2n1p3/3pP3/3P4/N4N2/PP2BPPP/R1BQK2R b KQkq - 0 8"
    val (_, obs, candidate) = currentEffectResponse(fen, "e7f5",
      List(List("e7f5", "a3c2", "f8e7") -> -20, List("f7f6") -> 0, List("a8c8") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Black's knight on f5 now targets White's pawn on d4"), text)
    assert(!text.contains("captures the initially attacked White pawn on d4"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 quiet development and new support reach the current comparison"):
    val owner = history(chess.variant.Standard.initialFen.value).extend(List("e2e4", "e7e5")).toOption.get
    val (_, obs, candidate) = currentEffectResponse(owner.currentFen, "g1f3",
      List(List("g1f3", "b8c6") -> 20, List("b1c3", "g8f6") -> 0, List("d2d4") -> -20),
      prefixOwner = Some(owner))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Nf3 develops the knight from g1 to f3, controlling d4 and e5"), text)
    assert(text.contains("White's knight on f3 adds support to White's pawn on d2 and White's pawn on h2."), text)
    assert(!text.contains("under attack from ."), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 a single current recapture retains its proven history and doubled pawn consequence"):
    val owner = history("4k3/8/8/8/3b4/4N3/4PP2/4K3 b - - 0 1").extend(List("d4e3")).toOption.get
    val (_, obs, candidate) = currentEffectResponse(owner.currentFen, "f2e3",
      List(List("f2e3", "e8d7") -> 20, List("e1f1", "e8d7") -> 0, List("e1d1") -> -20),
      prefixOwner = Some(owner))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("fxe3 recaptures the bishop on e3"), text)
    assert(text.contains("fxe3 leaves doubled white pawns on e2 and e3"), text)
    assert(!text.contains("the opening exchange is"), text)
    assertPublishedEffectSources(obs, candidate)

  test("two observed clearances retain the directly compared pawn options without quiet filler"):
    val (_, obs, candidate) = currentEffectResponse(chess.variant.Standard.initialFen.value, "g2g3",
      List(List("g2g3", "d7d5", "f1g2") -> 20,
        List("d2d3", "d7d5", "b1d2") -> 0, List("b1c3") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("g3 clears g2") && text.contains("d3 clears d2"), text)
    assert(text.contains("d3 keeps the g2-pawn on its starting square") &&
      text.contains("g3 commits that same pawn to g3"), text)
    assert(!text.contains("After g3, White pawn on f2 and White pawn on h2 add support to White pawn on g3"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 quiet development retains the opponent's complete opening exchange"):
    val owner = history(chess.variant.Standard.initialFen.value).extend(
      List("e2e4", "e7e6", "d2d4", "d7d5", "e4e5", "c7c5", "c2c3", "b8c6", "g1f3", "c8d7", "f1e2", "g8e7"))
      .toOption.get
    val (_, obs, candidate) = currentEffectResponse(owner.currentFen, "b1a3",
      List(List("b1a3", "c5d4", "c3d4", "e7f5", "a3c2") -> 20,
        List("e1g1", "e7g6") -> 0, List("b1d2") -> -20), prefixOwner = Some(owner))
    val text = (candidate \ "detail" \ "text").as[String]
    List("Na3 gives up that knight's support of White pawn on c3",
      "White knight on b1 still supports White pawn on c3", "same knight next moves with Nc2")
      .foreach(value => assert(text.contains(value), text))
    assert(!text.contains("O-O gives up"), "quiet castling duties must not fill the comparison: " + text)
    assert(text.contains("White's knight on c2 adds support to White's pawn on d4") &&
      text.contains("Black's knight on f5"), text)
    assert(text.contains("The supported pawn is the same one from c3"), text)
    assert(functionScopes(candidate)("piece_route"), text)
    val route = (candidate \ "detail" \ "effects").as[List[JsObject]].find(effect =>
      (effect \ "scope").as[String] == "piece_route").get
    assertEquals((route \ "target" \ "square").as[String], "c3", "the d4 pawn came from c3, not the captured original d4 pawn")
    assert(text.contains("Black pawn captures the white pawn on d4 with cxd4; White pawn recaptures the black pawn on d4 with cxd4"),
      "leading with the quiet action must preserve the complete response transaction: " + text)
    assert(text.contains("O-O castles, bringing the rook from h1 to f1"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 a newly controlled empty square supports the same arriving pawn's actual recapture"):
    val fen = "8/pppb1p2/5k1p/3p2p1/3P1PP1/2PB1KP1/PP6/8 b - - 0 27"
    val line = "b7b6 d3e2 f6e7 f3f2 f7f6 f2e3 e7d6 e3d2 d7e8 e2d3 a7a5 d2e3 c7c5 a2a3 e8d7 d4c5 b6c5".split(" ").toList
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("a7a5") -> 0, List("h6h5") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(functionScopes(candidate)("support_use"), text)
    List("empty c5 square", "c5", "dxc5+", "bxc5").foreach(value => assert(text.contains(value), text))
    assert(text.contains("dxc5+ gives check") && text.contains("bxc5 captures the checking pawn on c5"), text)
    val use = (candidate \ "detail" \ "effects").as[List[JsObject]].find(effect =>
      (effect \ "scope").as[String] == "support_use").get
    assertEquals((use \ "target" \ "square").as[String], "c7")
    assertEquals((use \ "square").as[String], "c5")
    assertPublishedEffectSources(obs, candidate)
    val (_, _, short) = currentEffectResponse(fen, line.head,
      List(line.dropRight(1) -> -20, List("a7a5") -> 0, List("h6h5") -> 20), supplied = List(line))
    assert(!functionScopes(short)("support_use"), "the longer supplied arm cannot donate the recapture")

  test("PU3 new rook support follows the same moving ally to its actual recapture"):
    val fen = "2b5/3r2pk/p1r1p2p/R7/8/BP2NP1P/2P1K1P1/8 b - - 12 41"
    val owner = history(chess.variant.Standard.initialFen.value).extend(
      ("e2e4 e7e5 g1f3 b8c6 d2d4 e5d4 f3d4 g8f6 d4c6 b7c6 f1d3 d7d5 e4d5 c6d5 e1g1 f8e7 " +
        "h2h3 e8g8 f1e1 c7c5 b2b3 c8b7 b1c3 c5c4 d3f1 d5d4 c3b5 e7b4 c1d2 b4c5 e1e5 f6e4 " +
        "e5c5 d8f6 d2e1 e4c5 d1d4 f6d4 b5d4 f8d8 d4f5 c4b3 a2b3 a7a6 f1c4 c5e6 c4e6 f7e6 " +
        "f5e3 a8c8 a1a4 c8c5 e1b4 c5e5 b4c3 e5e4 a4a2 e4f4 a2a5 h7h6 g1f1 b7c6 f1g1 c6b7 " +
        "g1f1 g8h7 f1e2 f4f7 f2f3 f7c7 c3e1 c7d7 e1b4 d8b8 b4c3 b8c8 c3b4 c8c6 b4c5 b7c8 c5a3")
        .split(" ").toList).toOption.get
    assertEquals(owner.currentFen, fen)
    val line = List("d7c7", "a3b4", "c6c2", "e3c2", "c7c2", "b4d2")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("d7b7") -> 0, List("h6h5") -> 20), prefixOwner = Some(owner))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(functionScopes(candidate)("support_use"), text)
    val use = (candidate \ "detail" \ "effects").as[List[JsObject]].find(effect =>
      (effect \ "scope").as[String] == "support_use").get
    assertEquals((use \ "actor" \ "square").as[String], "c7")
    assertEquals((use \ "target" \ "square").as[String], "c6")
    List("Rdc7", "Rxc2+", "Nxc2", "Bd2").foreach(value => assert(text.contains(value), text))
    assert(!text.contains("control of the empty c2 square"), "c6 blocked c2 at the anchor: " + text)
    assertPublishedEffectSources(obs, candidate)
    val played = "d7b7 a3b2 c8d7 a5a3 d7e8 b2d4 c6d6 d4e5 d6b6 e5d4".split(" ").toList
    val (_, comparisonObs, comparison) = currentEffectResponse(fen, played.head,
      List(line -> -20, played -> 0, List("h6h5") -> 20), prefixOwner = Some(owner))
    val comparisonText = (comparison \ "detail" \ "text").as[String]
    assert(comparisonText.contains("Rb7 clears d7") && functionScopes(comparison)("support_use"), comparisonText)
    assert(comparisonText.contains("Black rook on b7 attacks White pawn on b3") &&
      comparisonText.contains("Rdb6 blocks Black rook on b7's line"), comparisonText)
    assert(!comparisonText.contains("Uses the semi-open b-file"),
      "the complete b-file attack history already explains this independent file observation: " + comparisonText)
    assert(comparisonText.contains("Bd2 blocks the check") && comparisonText.contains("Nxc2"), comparisonText)
    assert(comparisonText.length <= 4000)
    assertPublishedEffectSources(comparisonObs, comparison)

  test("a knight route retains its observed attack and the separate support cost"):
    val fen = "r1b2rk1/1pp1qpb1/p1np1npp/P3p3/2B1P3/1QPP1N2/1P1N1PPP/R1B1R1K1 b - - 7 11"
    val played = "c6d8 d3d4 f6h5 c4f1 f8e8 h2h3 d8e6 d4e5 d6e5 d2c4 e6c5 b3a3".split(" ").toList
    val alternative = "f6d7 b3d1 b7b5 a5b6 d7b6 c4b3 a6a5 d2f1 c8e6 b3e6 e7e6".split(" ").toList
    val (_, obs, candidate) = currentEffectResponse(fen, played.head,
      List(alternative -> -20, played -> 0, List("h6h5") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    List("same knight next moves with Ne6", "Ne6:")
      .foreach(value => assert(text.contains(value), text))
    val cost = (candidate \ "detail" \ "effects").as[List[JsObject]].find(effect =>
      (effect \ "scope").as[String] == "pair_difference" &&
        (effect \ "actor" \ "square").asOpt[String].contains("f8") &&
        (effect \ "target" \ "square").asOpt[String].contains("c8")).get
    assertEquals((cost \ "actor" \ "role").as[String], "rook")
    assertEquals((cost \ "target" \ "role").as[String], "bishop")
    assert(text.contains("dxe5 moves the initially attacked White pawn on d4") && text.contains("dxe5 recaptures White pawn on e5"), text)
    assertPublishedEffectSources(obs, candidate)

  test("the selected PVs can reach a common board and split later without sharing their histories"):
    val fen = "5k2/pp3p1p/6p1/4q3/6b1/2N1P1P1/PPPR3P/2K1R3 b - - 0 23"
    val played = "g4e6 e3e4 b7b5 a2a3 g6g5 e1e3 e5c5 d2d3 e6c4 b2b4 c5e5 d3d7".split(" ").toList
    val alternative = "b7b5 a2a3 g4e6 e3e4 g6g5 d2d8 f8g7 d8d3".split(" ").toList
    val (state, obs, candidate) = currentEffectResponse(fen, played.head,
      List(alternative -> 66, played -> 68, List("g4f5") -> 80))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Be6 e4 b5 a3") && text.contains("b5 a3 Be6 e4"), text)
    assert(text.contains("Bc4") && text.contains("b4:") && text.contains("Qe5") && text.contains("Rd7"),
      "the common board does not erase the later attack's actual counterattack and escape: " + text)
    val prepared = ObservedLineProtocol.prepare(state.observationsPacket.get)
    val pair = (candidate \ "line_ids").as[List[String]].map(id => prepared.path(state.observationsPacket.get, id).get)
    assertEquals(pair.head.positions(4).fen, pair.last.positions(4).fen)
    assertNotEquals(pair.head.positions(4).id, pair.last.positions(4).id)
    assertNotEquals(pair.head.ids(3), pair.last.ids(3))
    val summary = (candidate \ "detail" \ "summary_question").as[JsObject]
    assertEquals((summary \ "question").as[String], "selected_comparison")
    val basicUnits = (summary \ "units").as[List[JsObject]]
    val proof = (basicUnits ++ (summary \ "detail_units").asOpt[List[JsObject]].getOrElse(Nil)).flatMap(unit =>
      (unit \ "proof_moves").asOpt[List[JsObject]].getOrElse(
        (unit \ "en").as[List[JsValue]].collect { case ref: JsObject => ref })).toSet
    pair.foreach(path => List(0, 1, 2, 3).foreach(offset =>
      assert(proof(RootQuestionSummary.MoveRef(path, offset).json),
        "both approaches to the claimed common board keep their own exact history")))
    assertPublishedEffectSources(obs, candidate)
    val projection = MoveWorkProjection.select(state.observationsPacket.get)
    def base(kind: String, body: String) = candidate + ("detail" -> (((candidate \ "detail").as[JsObject] - "effects") ++
      Json.obj("type" -> kind, "text" -> body)))
    val full = projection.withMoveEffects(base("root_comparison", "x" * 3990))
    assertEquals((full \ "detail" \ "text").as[String], "x" * 3990, "convergence cannot overrun the existing verdict budget")
    val question = projection.withMoveEffects(base("question_comparison", "An answer to a separate question."))
    assert(!(question \ "detail" \ "summary_question" \ "question").asOpt[String].contains("selected_comparison"),
      "the root owns this context; separate question and PV paragraphs do not repeat it")
    val original = "Existing comparison."
    val compact = projection.withMoveEffects(base("root_comparison", original))
    val compactText = (compact \ "detail" \ "text").as[String]
    val firstAction = compactText.indexOf("In the provided Be6 line,")
    assert(firstAction > 0, compactText)
    val withoutContext = compactText.drop(firstAction)
    val padding = "x" * (4000 - withoutContext.length + original.length)
    val bounded = projection.withMoveEffects(base("root_comparison", padding))
    val boundedEffects = (bounded \ "detail" \ "effects").as[List[JsObject]]
    val mandatory = (compact \ "detail" \ "effects").as[List[JsObject]].filter(effect =>
      Set("line_history", "piece_route", "check_sequence", "support_use")((effect \ "scope").as[String]))
    assert(mandatory.nonEmpty && mandatory.forall(boundedEffects.contains),
      "the convergence context must not displace an otherwise fitting complete action/reply bundle")
    val boundedText = (bounded \ "detail" \ "text").as[String]
    assert(boundedText.contains("b4:") && boundedText.contains("Rd7") && boundedText.length <= 4000, boundedText)

  test("PV convergence cannot borrow a longer line or mistake a missing continuation for a split"):
    val fen = "5k2/pp3p1p/6p1/4q3/6b1/2N1P1P1/PPPR3P/2K1R3 b - - 0 23"
    val played = "g4e6 e3e4 b7b5 a2a3 g6g5 e1e3".split(" ").toList
    val alternative = "b7b5 a2a3 g4e6 e3e4 g6g5 d2d8".split(" ").toList
    List(3, 4, 5).foreach { count =>
      val (state, obs, candidate) = currentEffectResponse(fen, played.head,
        List(alternative -> 66, played.take(count) -> 68, List("g4f5") -> 80), supplied = List(played))
      val text = (candidate \ "detail" \ "text").as[String]
      val paths = ObservedLineProtocol.prepare(state.observationsPacket.get)
      val visible = summaryCoreRefs(candidate, paths)
      val pair = (candidate \ "line_ids").as[List[String]].map(id => paths.paths.find(_.line.ref.id == id).get)
      assertEquals(pair.map(_.moves.size).sorted, List(count, alternative.size).sorted,
        "the root must not replace its short returned path with the longer supplied one")
      val summary = (candidate \ "detail" \ "summary").as[String]
      if count >= 4 then
        assertEquals(pair.map(_.positions(4).fen).distinct.size, 1)
        pair.foreach(path => (0 until 4).foreach(offset => assert(visible(path -> offset))))
      else
        assert(!summary.contains("same recorded position") && !summary.contains("same arrangement"),
          "a different local comparison cannot claim the unobserved convergence: " + summary)
      assert(!(text + summary).contains("line instead continues"),
        "both continuations must be observed to describe a split: " + summary)
      assertPublishedEffectSources(obs, candidate)
    }

  test("matching piece placement with different castling rights is not PV convergence"):
    val fen = "4k3/8/8/8/8/8/8/R3K2R w KQ - 0 1"
    val left = List("h1g1", "e8e7", "g1h1", "e7e8")
    val right = List("a1b1", "e8e7", "b1a1", "e7e8")
    val leftFen = history(fen).extend(left).toOption.get.currentFen
    val rightFen = history(fen).extend(right).toOption.get.currentFen
    assertEquals(leftFen.split(" ").head, rightFen.split(" ").head)
    assertNotEquals(leftFen, rightFen)
    val (state, obs, candidate) = currentEffectResponse(fen, left.head,
      List(left -> 20, right -> 0, List("e1d1") -> -20))
    summaryCoreRefs(candidate, ObservedLineProtocol.prepare(state.observationsPacket.get))
    val summary = (candidate \ "detail" \ "summary").as[String]
    assert(!summary.contains("same recorded position") && !summary.contains("same arrangement"),
      "a completed local comparison must not erase different castling rights: " + summary)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 support use rejects restored control, a later unpin and en passant's different landing square"):
    val cases = List(
      ("7k/8/5b2/8/8/8/1P6/RN5K w - - 0 1",
        List("a1a3", "h8h7", "b1c3", "h7h8", "b2b3", "h8h7", "b3b4", "f6c3", "a3c3"), "h1g1", "h1h2"),
      ("4r2k/8/6b1/8/8/3N4/4R3/4K3 w - - 0 1",
        List("e2e3", "g6d3", "e1f2", "h8g8", "e3d3"), "e1d1", "e1f1"),
      ("7k/4p3/8/3P4/8/r7/8/7K b - - 0 1",
        List("a3e3", "h1g1", "e7e5", "d5e6", "e3e6"), "h8g8", "h8h7"))
    cases.foreach { (fen, line, second, third) =>
      val (_, obs, candidate) = currentEffectResponse(fen, line.head,
        List(line -> 20, List(second) -> 0, List(third) -> -20))
      assert(!functionScopes(candidate)("support_use"), (candidate \ "detail" \ "text").as[String])
      assertPublishedEffectSources(obs, candidate)
    }

  test("PU3 a bishop route binds the later attack to its actual target and escape"):
    val fen = "5k2/pp3p1p/6p1/4q3/6b1/2N1P1P1/PPPR3P/2K1R3 b - - 0 23"
    val line = "g4e6 e3e4 b7b5 a2a3 g6g5 e1e3 e5c5 d2d3 e6c4 b2b4 c5e5 d3d7".split(" ").toList
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("g4f5") -> 0, List("h7h5") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(functionScopes(candidate)("piece_route"), text)
    assert(text.contains("same bishop next moves with Bc4 to c4") && text.contains("White rook on d3"), text)
    assert(text.contains("Rd7") && !text.contains("captures the initially attacked White rook on d3"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 a route that restores support keeps the current loss of that same duty"):
    val fen = "1r4k1/p4q1p/3p2p1/2p1b3/8/1N4P1/PPQ2P1P/4R1K1 w - - 2 25"
    val line = "c2d2 b8b6 h2h4 h7h5 e1e4 g8h7 d2e3 f7d5 b3c5 b6b2 c5d3 b2a2 d3e5 a2a1 g1h2 d6e5".split(" ").toList
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 20, List("g1f1") -> 0, List("c2d3") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(functionScopes(candidate)("piece_route"), text)
    assert(text.contains("Qd2 gives up that queen's support of White knight on b3") &&
      text.contains("White's queen on e3 adds support to White's knight on b3"), text)
    assert(text.contains("Qe3: White queen on d2 moves to e3"), text)
    assertEquals("White's queen on e3 adds support to White's knight on b3".r.findAllIn(text).length, 1, text)
    assert(!text.contains("the same queen next moves with Qe3"),
      "the actual movement and its restored support already explain the same-piece route: " + text)
    assert(text.contains("Nxe5: White knight on d3 also captures Black bishop on e5") &&
      text.contains("This precedes that knight's removal with dxe5") &&
      text.contains("dxe5 later captures White knight on e5"), text)
    val events = List("Nxc5:", "Rxb2:", "Nd3:", "Rxa2:", "Nxe5:", "Ra1+ checks", "Kh2 moves", "dxe5 later captures")
    events.foreach(event => assertEquals(text.sliding(event.length).count(_ == event), 1, text))
    List("White knight on b3 moves out of Black rook on b6's attack",
      "Black rook on b6 captures White pawn on b2", "attack line opened when White knight on b3 moved away",
      "Moves the rook out of the attack from White's knight on d3", "Black rook on b2 captures White pawn on a2",
      "checks the White king on g1", "moves the White king from g1 to h2").foreach(value => assert(text.contains(value), text))
    assertPublishedEffectSources(obs, candidate)

  test("one comparison card joins shared rook actions while retaining each target and question answer"):
    val fen = "1r4k1/p4q1p/3p2p1/2p1b3/8/1N4P1/PPQ2P1P/4R1K1 w - - 2 25"
    val played = ("c2d2 b8b6 h2h4 h7h5 e1e4 g8h7 d2e3 f7d5 b3c5 b6b2 c5d3 b2a2 " +
      "d3e5 a2a1 g1h2 d6e5 e3g5 d5f7 g5e3 f7b7 e3f3 a1c1").split(" ").toList
    val alternative = "e1d1 f7f3 d1e1 f3d5 e1d1 d5c6 c2c4 g8g7 d1e1 c6c7 e1e2".split(" ").toList
    val (state, obs, root) = currentEffectResponse(fen, played.head,
      List(alternative -> 20, played -> 18, List("g1f1") -> 0), output = Some("shared-queen-restoration"))
    val pair = (root \ "line_ids").as[List[String]].toSet
    val card = (obs \ "selection" \ "candidates").as[List[JsObject]].filter(value =>
      (value \ "selection").as[String] == "selected" && (value \ "line_ids").as[List[String]].toSet == pair)
    assertEquals(card.size, 2)
    val texts = card.map(value => (value \ "detail" \ "text").as[String])
    val text = texts.mkString(" ")
    val summary = card.map(value => (value \ "detail" \ "summary").asOpt[String]
      .getOrElse((value \ "detail" \ "text").as[String])).mkString(" ")
    assert(summary.length < text.length, "the normal shared card should have a shorter reading")
    val packet = state.observationsPacket.get
    val paths = ObservedLineProtocol.prepare(packet)
    val playedPath = paths.paths.find(_.moves == played).get
    val alternativePath = paths.paths.find(_.moves == alternative).get
    val queen = Piece("white", "queen", "c2").witness
    val knight = Piece("white", "knight", "b3").witness
    assertEquals(paths.supportAt(packet, playedPath, 1, queen, knight).state, "absent")
    assertEquals(paths.supportAt(packet, alternativePath, 1, queen, knight).state, "present")
    assertEquals(paths.supportAt(packet, playedPath, 7, queen, knight).state, "present",
      "Qe3 restores the same queen's support before the later knight loss")
    val visible = summaryCoreRefs(root, paths)
    List(0, 6, 8, 9, 11, 12, 13, 14, 15).foreach(offset => assert(visible(playedPath -> offset),
      s"the support contrast lost offset $offset (${paths.notation.line(played)(offset)}): " + summary))
    List("Qd2", "Rd1", "Qe3", "Nxc5", "Rxb2", "Rxa2", "Nxe5", "dxe5", "Ra1+", "Kh2",
      "At this position, Qd2 evaluates below Rd1", "evaluations are close")
      .foreach(clause => assert(summary.contains(clause), summary))
    assert(summary.contains("queen on e3") && summary.contains("adds support to White's knight on b3"),
      "the default reading must not leave the earlier loss of support apparently unchanged: " + summary)
    assert(summary.contains("Nxe5 captures the black bishop on e5") &&
      summary.contains("dxe5 captures the white knight on e5"),
      "the selected attacker's gain and later loss must be explained, not only listed as SAN: " + summary)
    assert(texts.forall(value => value.nonEmpty && value.length <= 4000), texts.mkString("\n"))
    assert(texts.flatMap(_.split("\n\n")).forall(_.length <= 1000),
      "the two long histories should be readable in complete-event paragraphs: " + text)
    val queenResponse = texts.flatMap(_.split("\n\n")).find(_.contains("Qe3: White queen on d2 moves to e3")).get
    assert(queenResponse.contains("White's queen on e3 adds support to White's knight on b3"), queenResponse)
    assert(text.contains("At this position, Qd2 evaluates below Rd1"), text)
    assert(!text.contains("After Qd2, White's knight on b3 adds support to White's queen on d2"),
      "an already complete action/reply explanation need not fill the card with an independent quiet support: " + text)
    assert(text.contains("White rook on e1 attacks Black bishop on e5"), text)
    assert(text.contains("White's pawn on f2") && text.contains("White's pawn on g3"), text)
    assert(text.contains("Qd5: Black queen on f3 moves to d5, ending its attacks on White pawn on f2 and White pawn on g3."), text)
    assert(text.contains("Black bishop on e5 attacks White pawn on g3 after this move."), text)
    assert(!text.contains("Black bishop on e5 attacks White pawn on f2 after this move."), text)
    assert(!text.contains("At Re1, Moves the rook out of"), "the shared evasion is already in the same event: " + text)
    assertEquals(text.sliding("Re1:".length).count(_ == "Re1:"), 2,
      "two different visits in the same PV remain two events: " + text)
    List("Rxb2:", "Rxa2:", "Nxe5:", "Ra1+ checks", "Kh2 moves", "dxe5 later captures").foreach(event =>
      assertEquals(text.sliding(event.length).count(_ == event), 1, text))
    card.foreach(assertPublishedEffectSources(obs, _))
    val projection = MoveWorkProjection.select(packet)
    val base = (projection.baseJson \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    // A question at a later decision, or a different owned line, is independently composed.
    val separateScenes = RuntimeProtocol.observationsJson(packet, proposalEntries = state.proposalEntries)
    val separateRoot = (separateScenes \ "selection" \ "candidates").as[List[JsObject]].find(value =>
      (value \ "id") == (base \ "id")).get
    assertEquals(separateRoot, projection.withMoveEffects(base),
      "without the shared PV card, root and question remain independent scenes")
    val reversed = PrincipalVariationComparison.read(packet, played.head, state.rootSearches.flatMap(_.receipt))
      .filter(reading => Set(reading.answer.referenceLineId, reading.answer.alternativeLineId) == pair)
    assert(reversed.nonEmpty)
    val separateDirection = RuntimeProtocol.observationsJson(packet, proposalEntries = state.proposalEntries,
      pvComparisons = state.pvComparisons ++ reversed)
    val independent = (separateDirection \ "selection" \ "candidates").as[List[JsObject]].find(value =>
      (value \ "id") == (base \ "id")).get
    assertEquals(independent, projection.withMoveEffects(base),
      "a reversed card may consume the question without this oriented root; keep both explanations self-contained")
    val question = card.find(value => (value \ "detail" \ "type").as[String] == "question_comparison").get
    val plain = question + ("detail" -> (((question \ "detail").as[JsObject] - "effects") +
      ("text" -> JsString("This question retains its own answer."))))
    val later = plain + ("detail" -> ((plain \ "detail").as[JsObject] +
      ("prefix_moves_uci" -> Json.arr("e1d1", "f7f3"))))
    val different = plain + ("line_ids" -> Json.toJson(pair.toList.take(1)))
    List(later, different).foreach { other =>
      assertEquals(projection.withSharedMoveEffects(List(base -> Nil, other -> Nil)),
        List(projection.withMoveEffects(base), projection.withMoveEffects(other)))
    }

  test("shared text fields retain a complete function and reply when the first field is full"):
    val fen = "5k2/pp3p1p/6p1/4q3/6b1/2N1P1P1/PPPR3P/2K1R3 b - - 0 23"
    val line = "g4e6 e3e4 b7b5 a2a3 g6g5 e1e3 e5c5 d2d3 e6c4 b2b4 c5e5 d3d7".split(" ").toList
    val (state, obs, _) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("g4f5") -> 0, List("h7h5") -> 20))
    val projection = MoveWorkProjection.select(state.observationsPacket.get)
    val base = (projection.baseJson \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    val full = base + ("detail" -> ((base \ "detail").as[JsObject] + ("text" -> JsString("x" * 3990))))
    val question = base ++ Json.obj("id" -> "shared-question", "detail" -> Json.obj(
      "type" -> "question_comparison", "prefix_moves_uci" -> Json.arr(), "evaluation_relation" -> "equal",
      "text" -> "The engine answer stays with its question."))
    val shared = projection.withSharedMoveEffects(List(full -> Nil,
      question -> List(MoveWorkProjection.Context("Separate question context."))))
    assertEquals((shared.head \ "detail" \ "text").as[String], "x" * 3990)
    val text = (shared.last \ "detail" \ "text").as[String]
    assert(text.contains("same bishop next moves with Bc4") && text.contains("b4:") &&
      text.contains("Qe5") && text.contains("Rd7"), text)
    assert(text.contains("Separate question context.") && text.length <= 4000, text)
    assertPublishedEffectSources(obs, shared.last)
    val effects = (shared.last \ "detail" \ "effects").as[List[JsObject]]
    assert(effects.exists(effect => (effect \ "scope").as[String] == "piece_route"))
    assert(effects.exists(effect => (effect \ "attack_ply").asOpt[Int].nonEmpty),
      "the receiving candidate keeps both the later attack and its functional proof")
    val scope = "These observed attacks alone do not establish a forced gain."
    val contexts = List("First owned event.", "Second owned event.").map(text =>
      MoveWorkProjection.Context(text, Some(scope)))
    val qualified = projection.withSharedMoveEffects(List(full -> Nil, question -> contexts))
    val qualifiedText = (qualified.last \ "detail" \ "text").as[String]
    assertEquals(qualifiedText.sliding(scope.length).count(_ == scope), 1)
    assert(qualifiedText.contains("First owned event.") && qualifiedText.contains("Second owned event."))
    val filledQuestion = question + ("detail" -> ((question \ "detail").as[JsObject] +
      ("text" -> JsString("y" * 3990))))
    val omitted = projection.withSharedMoveEffects(List(full -> Nil, filledQuestion -> contexts))
    assertEquals((omitted.last \ "detail" \ "text").as[String], "y" * 3990,
      "an unadmitted context owns no qualification and cannot consume the mandatory answer's space")

  test("a shared timeline may retain independent explanations across two small remaining fields"):
    val fen = "1r4k1/p4q1p/3p2p1/2p1b3/8/1N4P1/PPQ2P1P/4R1K1 w - - 2 25"
    val line = "e1d1 f7f3 d1e1 f3d5 e1d1 d5c6 c2c4 g8g7 d1e1 c6c7 e1e2".split(" ").toList
    val (state, obs, _) = currentEffectResponse(fen, line.head,
      List(line -> 20, List("g1f1") -> 0, List("g1h1") -> -20))
    val projection = MoveWorkProjection.select(state.observationsPacket.get)
    val base = (projection.baseJson \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    val root = base + ("detail" -> ((base \ "detail").as[JsObject] + ("text" -> JsString("x" * 2800))))
    val question = base ++ Json.obj("id" -> "shared-question", "detail" -> Json.obj(
      "type" -> "question_comparison", "prefix_moves_uci" -> Json.arr(), "evaluation_relation" -> "equal",
      "text" -> ("y" * 2800)))
    val shared = projection.withSharedMoveEffects(List(root -> Nil, question -> Nil))
    val texts = shared.map(value => (value \ "detail" \ "text").as[String])
    val text = texts.mkString(" ")
    assert(texts.forall(_.length <= 4000))
    assert(text.contains("White rook on e1 attacks Black bishop on e5"), text)
    assert(text.contains("White's pawn on f2") && text.contains("White's pawn on g3"), text)
    assert(text.contains("initially attacked Black pawn on d6"), text)
    shared.foreach(assertPublishedEffectSources(obs, _))

  test("co-owned tactical events retain their distinct ply and opened lines without repeated full prefixes"):
    val fen = "rn2kb1r/ppp1qppp/3pbn2/8/8/2N1N3/PPPPQPPP/R1B1KB1R w KQkq - 6 8"
    val played = ("g2g3 d6d5 f1g2 c7c6 d2d4 h7h5 h2h4 g7g6 e1g1 f8g7 e2d1 e8g8 " +
      "c3e2 b8d7 e2f4 f8e8 c2c3 f6e4 d1b3 g7h6 e3d5 c6d5").split(" ").toList
    val alternative = ("d2d4 d6d5 g2g4 h7h6 f1g2 b8c6 e3d5 e6d5 c3d5 e7e2 e1e2 e8c8 " +
      "d5f6 c6d4 e2d1 g7f6 c1d2 h6h5 c2c3 d4f3 g2f3 f8h6 d1c2 d8d2 c2b3 h5h4 h1f1 h6f4").split(" ").toList
    val (state, obs, root) = currentEffectResponse(fen, played.head,
      List(played -> 30, alternative -> 0, List("b2b3") -> -20))
    val question = (obs \ "selection" \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "question_comparison" &&
        (value \ "line_ids").as[List[String]].toSet == (root \ "line_ids").as[List[String]].toSet).get
    val text = (question \ "detail" \ "text").as[String]
    assertEquals(text.sliding("These concrete attacks".length).count(_ == "These concrete attacks"), 1, text)
    List("Nexd5", "Bxd5", "Nxd5", "Nxf6", "Nf3")
      .foreach(event => assert(text.contains(event), text))
    assert(text.contains("Bxd5 moves the initially attacked"), text)
    assert(!text.contains("In the returned continuation d4 d5 g4"), text)
    assertEquals(text.sliding("Qxe2+ Kxe2 O-O-O".length).count(_ == "Qxe2+ Kxe2 O-O-O"), 1, text)
    assert(text.contains("Qxe2+ Kxe2 O-O-O") &&
      text.contains("Nxd4+ Kd1 gxf6 Bd2 h5 c3"), text)
    assert(text.contains("White queen on e2 attacks the Black bishop on e6") &&
      text.contains("Black rook on d8 attacks the White bishop on d2"), text)
    assert(text.contains("Nxf6 clears d5 after the captures Nexd5, Bxd5, Nxd5"), text)
    List("Nexd5 removes the Black pawn on d5", "Black bishop on e6 captures White knight on d5",
      "Nxd5: White knight on c3 captures the initially attacked Black bishop on d5",
      "Nxf6: White knight on d5 also captures Black knight on f6").foreach(loss => assert(text.contains(loss), text))
    val packet = state.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val contexts = scala.collection.mutable.ListBuffer.empty[MoveWorkProjection.Context]
    CommentaryProposal.comparisonCandidates(packet, prepared, state.proposalEntries, Some((value, parts) =>
      if (value \ "line_ids").as[List[String]].toSet == (root \ "line_ids").as[List[String]].toSet then contexts ++= parts
      value))
    val isolated = contexts.find(_.tactic.exists(_.line == alternative.take(13))).get
    val tactic = isolated.tactic.get
    val standalone = tactic.text(prepared.notation, isolated.tacticScope, includeQualification = false)
    List("white knight originally on e3 on d5 is removed by Bxd5",
      "black bishop originally on e6 on d5 is removed by Nxd5",
      "white knight originally on c3 leaves d5 with Nxf6")
      .foreach(condition => assert(standalone.contains(condition), standalone))
    assert(!standalone.contains("clears d5 after the captures"),
      "an independent explanation must still name the losses that its card has not described: " + standalone)
    List("Rxd2+ captures", "Kb3 moves").foreach(reply => assert(text.contains(reply), text))
    assertEquals(text.sliding("with Black queen on e2".length).count(_ == "with Black queen on e2"), 1, text)
    assert(!text.contains("with check against the White king on e1 before the later capture"), text)
    val rootText = (root \ "detail" \ "text").as[String]
    assert(rootText.contains("Ne4") && rootText.contains("cxd5") && rootText.contains("h5"), rootText)
    assert(!rootText.contains("After g3, White pawn on f2 and White pawn on h2 add support to White pawn on g3"),
      "the observed Bg2 use and its full reply/capture history are the explanation, not a quota of extra facts: " + rootText)
    val projection = MoveWorkProjection.select(packet)
    val base = (projection.baseJson \ "candidates").as[List[JsObject]].find(value => (value \ "id") == (root \ "id")).get
    val alreadyShown = (root \ "detail" \ "effects").as[List[JsObject]].toSet
    val following = (projection.withMoveEffects(base, alreadyShown) \ "detail" \ "text").as[String]
    assert(!following.contains("After g3, White pawn on f2 and White pawn on h2 add support to White pawn on g3"),
      "an already displayed exact use must not cause another card to fill the same move with quiet supplements: " + following)
    assertPublishedEffectSources(obs, root)

  test("an attacker's next capture on another ray is not attributed to the target's departure"):
    val fen = "8/7k/1r6/P7/8/1N6/8/7K w - - 0 1"
    val line = List("h1g1", "b6b5", "b3c5", "b5a5")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 0, List("h1h2") -> 20, List("h1g2") -> 10))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Nc5") && text.contains("Rxa5"), text)
    assert(!text.contains("This uses the attack line opened"), text)
    assertPublishedEffectSources(obs, candidate)

  test("a rook's recapture and its new attack share one event without merging two captured knights"):
    val fen = "r1bqkb1r/pppp1ppp/2n5/1B2p3/4n3/5N2/PPPP1PPP/RNBQ1RK1 w kq - 0 5"
    val line = ("f1e1 e4d6 f3e5 f8e7 b5f1 c6e5 e1e5 e8g8 d2d4 e7f6 e5e1 f8e8 " +
      "c1f4 e8e1 d1e1 f6d4 f4d6 c7d6 b1c3 g7g6 a1d1 d4e5 g2g3").split(" ").toList
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 44, List("d2d4") -> 16, List("d1e2") -> 13))
    val text = (candidate \ "detail" \ "text").as[String]
    assertEquals(text.sliding("Rxe5 recaptures".length).count(_ == "Rxe5 recaptures"), 1, text)
    List("Rxe5 recaptures Black knight on e5", "White rook on e5 attacks Black bishop on e7",
      "Black's bishop on f6 now targets White's rook on e5", "Re1: Moves the rook out of",
      "Bxd6: White bishop on f4 captures the initially attacked Black knight on d6").foreach(value => assert(text.contains(value), text))
    val events = List("Nxe5: White knight on f3 captures", "Nxe5 later captures White knight on e5", "Rxe5 recaptures", "Bf6 moves", "Re1: Moves the rook out of")
    events.foreach(event => assert(text.contains(event), text))
    assertPublishedEffectSources(obs, candidate)

  test("two attacks on a castling rook retain that rook as the evader rather than the king"):
    val fen = "k7/8/8/3b4/4q3/8/4PP2/4K2R b K - 0 1"
    val line = "e4h4 e1g1 h4f4 f1d1 f4d4 g1h2 d4d1".split(" ").toList
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("e4a4") -> 0, List("a8b8") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("White rook on h1 moves out of") && text.contains("Qxd1:"), text)
    assert(!text.contains("White king on e1 moves out of"), text)
    assertEquals(text.sliding("O-O moves".length).count(_ == "O-O moves"), 1, text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 an evasion and next-move route share one introduction without losing the response and trade"):
    val fen = "8/p4p2/1p3Nbk/4Pq2/P5pP/1P6/2P3R1/1K4R1 w - - 10 63"
    val owner = history("8/p3Np2/1p5k/4P2b/P3q1pP/1P6/K1PR2R1/8 w - - 0 58").extend(
      "e7g8 h6g6 g8f6 e4f3 g2g1 g6h6 d2g2 f3f5 a2b1 h5g6".split(" ").toList).toOption.get
    assertEquals(owner.currentFen, fen)
    val line = "f6g8 h6g7 g8e7 f5f3 e7g6 g7g6 g2g3 f3f2".split(" ").toList
    val (_, obs, candidate) = currentEffectResponse(fen, "h4h5",
      List(List("h4h5", "g6h5", "f6g4", "h5g4", "g2g4") -> 20, line -> 0, List("g2f2") -> -20),
      prefixOwner = Some(owner))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("the same knight next moves with Ne7 to e7") &&
      text.contains("Ng8+ moves the knight out of the attack from Black's queen on f5"), text)
    assert(!text.contains("that knight continues Ne7") &&
      !text.contains("White's knight on e7 now targets Black's queen on f5"), text)
    assert(text.contains("Qf3") && text.contains("ends its support of Black bishop on g6") &&
      text.contains("Nxg6") && text.contains("Kxg6 recaptures White knight on g6"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 a bare next movement and an unowned later role are not functional routes"):
    val fen = "7k/8/8/4p3/8/8/8/1N5K w - - 0 1"
    val line = List("b1a3", "e5e4", "a3c2")
    val (_, _, bare) = currentEffectResponse(fen, line.head,
      List(line -> 20, List("b1d2") -> 0, List("h1g1") -> -20))
    assert(!functionScopes(bare)("piece_route"), (bare \ "detail" \ "text").as[String])
    val owner = history(chess.variant.Standard.initialFen.value).extend(
      "e2e4 e7e6 d2d4 d7d5 e4e5 c7c5 c2c3 b8c6 g1f3 c8d7 f1e2 g8e7".split(" ").toList).toOption.get
    val supplied = List("b1a3", "c5d4", "c3d4", "e7f5", "a3c2")
    val (_, _, short) = currentEffectResponse(owner.currentFen, "b1a3",
      List(supplied.take(4) -> 20, List("e1g1") -> 0, List("b1d2") -> -20),
      supplied = List(supplied), prefixOwner = Some(owner))
    assert(!functionScopes(short)("piece_route"), (short \ "detail" \ "text").as[String])

  test("PU3 grouping quiet support preserves all current targets when no future use is observed"):
    val (_, obs, candidate) = currentEffectResponse("5k2/7p/6p1/8/8/8/P7/6K1 b - - 0 1", "f8g7",
      List(List("f8g7", "g1f1") -> -20, List("f8e7") -> 0, List("f8g8") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Black's king on g7 adds support to Black's pawn on g6 and Black's pawn on h7"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 an attacked defender's departure is connected to the actual capture of its former target"):
    val fen = "b1N3k1/5ppp/4p3/4P3/3N1P2/3n2P1/7P/6K1 b - - 0 28"
    val line = List("g7g5", "f4g5", "d3e5")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("g8f8") -> 0, List("g7g6") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("fxg5") && text.contains("ends its support of White pawn on e5") &&
      text.contains("Nxe5"), text)
    assert(!text.contains("enables") && !text.contains("undefended"), text)
    assertPublishedEffectSources(obs, candidate)
    val (_, _, short) = currentEffectResponse(fen, line.head,
      List(line.take(2) -> -20, List("g8f8") -> 0, List("g7g6") -> 20), supplied = List(line))
    assert(!(short \ "detail" \ "text").as[String].contains("Nxe5"))

  test("PU3 a direct reply opening the same rook's second attack preserves the actual capture"):
    val fen = "1r3rk1/p1q2pbp/2ppb1p1/3P2B1/8/1N6/PP3PPP/2RQR1K1 b - - 0 16"
    val line = List("b8b5", "d5e6", "b5g5")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("h7h6") -> 0, List("f8e8") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("dxe6") && text.contains("Opens the line from Black rook on b5 to White bishop on g5") &&
      text.contains("Rxg5"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 the same pawn escapes and attacks its original attacker on its next turn"):
    val fen = "2kr1b1r/pppq1ppp/2npbn2/8/8/2N1N1P1/PPPPQPBP/R1B2RK1 w - - 5 11"
    val line = List("b2b4", "d6d5", "b4b5", "c6e7")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 20, List("d2d4") -> 0, List("a2a3") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("b5 ") && text.contains("Black knight on c6") && text.contains("Ne7"), text)
    assertPublishedEffectSources(obs, candidate)
    val extended = line ++ List("d2d4", "d7b5")
    val (_, _, afterCapture) = currentEffectResponse(fen, line.head,
      List(extended -> 20, List("d2d4") -> 0, List("a2a3") -> -20))
    val capturedText = (afterCapture \ "detail" \ "text").as[String]
    assert(capturedText.contains("Qxb5: Black queen on d7 captures the initially attacked White pawn on b5"), capturedText)
    assert(!capturedText.contains("for White pawn on b4:  That original target is captured"), capturedText)
    val (_, _, short) = currentEffectResponse(fen, line.head,
      List(line.take(2) -> 20, List("d2d4") -> 0, List("a2a3") -> -20), supplied = List(line))
    assert(!(short \ "detail" \ "text").as[String].contains("b5:") && !(short \ "detail" \ "text").as[String].contains("next moves with b5"))

  test("PU3 an already explained target capture keeps its check and the observed king response"):
    val fen = "8/4B1pk/7p/3rN3/r4PP1/4K2P/8/8 b - - 3 62"
    val line = List("g7g5", "e5c6", "g5f4", "e3e2")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("h6h5") -> 0, List("a4a5") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("gxf4+") && text.toLowerCase.contains("gives check") &&
      text.contains("Ke2 moves the king to e2"), text)
    assertPublishedEffectSources(obs, candidate)
    val (_, shortObs, short) = currentEffectResponse(fen, line.head,
      List(line.take(3) -> -20, List("h6h5") -> 0, List("a4a5") -> 20), supplied = List(line))
    val shortText = (short \ "detail" \ "text").as[String]
    assert(shortText.contains("gxf4+") && shortText.toLowerCase.contains("gives check"), shortText)
    assert(!shortText.contains("Ke2"), "an unowned longer line cannot answer the final check: " + shortText)
    assertPublishedEffectSources(shortObs, short)

  test("PU3 restored support cannot be treated as still absent at a later capture"):
    val fen = "3r3k/8/2n5/4P3/8/3N4/8/7K b - - 0 1"
    val line = List("d8d4", "d3f4", "h8g8", "f4d3", "c6e5", "d3e5")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("h8g8") -> 0, List("c6a5") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Nf4"), text)
    assert(!text.contains("ends its support of White pawn on e5") &&
      !text.contains("captures White pawn on e5 after that support has ended"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU3 an opened rook line cannot borrow another rook's capture as its own"):
    val fen = "1r4rk/8/4b3/3P2B1/8/8/8/7K b - - 0 1"
    val line = List("b8b5", "d5e6", "g8g5")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("b8b7") -> 0, List("h8h7") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Opens the line from Black rook on b5 to White bishop on g5"), text)
    assert(text.contains("dxe6"), text)
    assert(text.contains("Rgxg5: Black rook on g8 captures the initially attacked White bishop on g5"), text)
    assert(!text.contains("Rbxg5:"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU1 a new defender takes over the departing defender's actual recapture duty"):
    val (_, obs, candidate) = currentEffectResponse("7k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1", "f1e1",
      List(List("f1e1", "h8h7", "c3b5", "f6e4", "e1e4") -> 20,
        List("f1f2", "h8h7") -> 0, List("g1h1") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(functionScopes(candidate)("support_handoff"), text)
    List("Nb5", "Nxe4", "Rxe4").foreach(value => assert(text.contains(value), text))
    assertPublishedEffectSources(obs, candidate)

  test("PU1 a new blocker maintains an already blocked line after the old blocker leaves"):
    val (_, obs, candidate) = currentEffectResponse(
      "r2qr1k1/pp3pbp/3p1np1/2pPn3/4P1b1/1PN5/P1QNBPPP/R1B2RK1 w - - 1 13", "f2f3",
      List(List("f2f3", "g4d7", "c1b2", "f6h5") -> 20,
        List("e2g4", "f6g4") -> 0, List("h2h3") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("keeps the line") && text.contains("Bd7") && text.contains("Nh5"), text)
    assert(!text.contains("safe") && !text.contains("best plan"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU1 a cleared corridor is used by a rook passing through rather than stopping on the square"):
    val (_, obs, candidate) = currentEffectResponse("7k/8/8/8/8/8/4B3/4R1K1 w - - 0 1", "e2f3",
      List(List("e2f3", "h8h7", "e1e7") -> 20, List("e2d1", "h8h7") -> 0, List("g1h1") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("clears e2") && text.contains("Re7+") && text.contains("through that square"), text)
    assertPublishedEffectSources(obs, candidate)

  test("clearance prose keeps development and the observed passage without a friendly king target"):
    val line = List("c1e3", "e8e7", "b1c3", "e7e8", "d1b1")
    val (_, obs, candidate) = currentEffectResponse("4k3/pp6/8/8/8/8/PP6/RNBQK2R w KQ - 0 1", line.head,
      List(line -> 20, List("e1g1", "e8e7") -> 0, List("d1d2") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    val summary = (candidate \ "detail" \ "summary").asOpt[String].getOrElse(text)
    assert(text.contains("Be3 brings the bishop to e3") && text.contains("Qb1 passes through the vacated c1 square"), text)
    assert(!text.contains("toward White king") && !text.contains("to target White's king"), text)
    assert(!"(?i)\\b(ply|plies|half-move)\\b".r.findFirstIn(text + summary).nonEmpty, summary)
    assertPublishedEffectSources(obs, candidate)

  private def functionScopes(candidate: JsObject): Set[String] =
    (candidate \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil)
      .map(value => (value \ "scope").as[String]).toSet

  test("PU2 a corridor's actual queen attack and the rook's response stay with the preparation"):
    val fen = "2r1nr1k/6pp/p2p4/3qp1P1/1p1N2PQ/8/PPPR3P/2K1R3 w - - 5 27"
    val line = List("g5g6", "h7h6", "h4e7", "f8f6", "e7d7")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 20, List("c1b1") -> 0, List("h2h3") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("clears g5") && text.contains("Qe7"), text)
    assert(text.contains("Rf6") && text.contains("Black rook on f8"),
      "the same queen's attack uses a different ray from its passage: " + text)
    assert(text.contains("to f6; White queen on e7 still attacks it there"), text)
    assert(!text.contains("Rf6 moves the initially attacked Black rook on f8 out of"), text)
    assertPublishedEffectSources(obs, candidate)

  test("PU2 a used corridor retains the pawn cost and the actual checking queen and response"):
    val fen = "2r1n2k/3Q2p1/p2p1rPp/4p3/1pqN2P1/8/PPPR3P/2K1R3 w - - 4 30"
    val line = List("g4g5", "h6g5", "d7h3", "h8g8")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 20, List("c1b1") -> 0, List("h2h3") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("clears g4") && text.contains("hxg5"), text)
    assert(text.contains("Qh3+ gives check") && text.contains("White's queen on h3") &&
      text.contains("Kg8 moves the king to g8"), text)
    assert(!text.contains("g5 gives check") && !text.contains("with White's pawn on g5"), text)
    assertPublishedEffectSources(obs, candidate)
    val (_, shortObs, short) = currentEffectResponse(fen, line.head,
      List(line.take(2) -> 20, List("c1b1") -> 0, List("h2h3") -> -20), supplied = List(line))
    assert(!(short \ "detail" \ "text").as[String].contains("Qh3+"))
    assertPublishedEffectSources(shortObs, short)

  test("PU2 a repeated corridor passage keeps the same bishop's new target on another diagonal"):
    val fen = "7k/p7/8/5B2/8/8/2P3r1/K7 w - - 0 1"
    val line = List("c2c3", "a7a6", "f5b1", "a6a5", "b1e4")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 20, List("a1b1") -> 0, List("f5e4") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Bb1") && text.contains("Be4") && text.contains("Black's rook on g2"), text)
    assert(!text.contains("captures the initially attacked Black rook on g2"), text)
    assertPublishedEffectSources(obs, candidate)
    val (_, shortObs, short) = currentEffectResponse(fen, line.head,
      List(line.take(4) -> 20, List("a1b1") -> 0, List("f5e4") -> -20), supplied = List(line))
    val shortText = (short \ "detail" \ "text").as[String]
    assert(shortText.contains("Bb1") && !shortText.contains("Be4"), shortText)
    assertPublishedEffectSources(shortObs, short)

  test("PU2 a used function cannot fit without the observed response to its attack"):
    val fen = "2r1nr1k/6pp/p2p4/3qp1P1/1p1N2PQ/8/PPPR3P/2K1R3 w - - 5 27"
    val line = List("g5g6", "h7h6", "h4e7", "f8f6", "e7d7")
    val (state, _, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 20, List("c1b1") -> 0, List("h2h3") -> -20))
    val projection = MoveWorkProjection.select(state.observationsPacket.get)
    List(200, 800, 1600, 3000).foreach { available =>
      val base = candidate + ("detail" -> (((candidate \ "detail").as[JsObject] - "effects") ++
        Json.obj("type" -> "question_comparison", "text" -> ("x" * (4000 - available)))))
      val delivered = projection.withMoveEffects(base)
      val text = (delivered \ "detail" \ "text").as[String]
      assert(text.length <= 4000)
      if text.contains("clears g5") then
        assert(text.contains("Rf6 moves the initially attacked Black rook on f8 to f6") &&
          text.contains("still attacks it there") && text.contains("Qd7:"), text)
      else assert(!(delivered \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil)
        .exists(effect => (effect \ "attack_ply").asOpt[Int].contains(3)),
        "the later attack cannot be emitted without its owned functional premise")
    }

  test("PU2 an evading bishop keeps its later attack when another piece opens its line"):
    val line = List("c1b2", "c8d8", "b1c3", "d8d4", "c3b1", "d4d5")
    val (_, obs, candidate) = currentEffectResponse("2r5/7k/8/8/8/8/8/KNB5 w - - 0 1", line.head,
      List(line -> 20, List("a1a2") -> 0, List("b1a3") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Bb2 moves the bishop out of") && text.contains("Nb1") && text.contains("Rd5"), text)
    val effect = (candidate \ "detail" \ "effects").as[List[JsObject]]
      .find(effect => (effect \ "attack_ply").asOpt[Int].contains(5)).get
    assertEquals((effect \ "actor" \ "role").as[String], "bishop")
    assertEquals((effect \ "actor" \ "square").as[String], "b2")
    assertPublishedEffectSources(obs, candidate)

  test("PU2 a bishop's first passage is not repeated as a later bare occupation of the same clearance"):
    val line = List("e7e6", "g1f3", "g8f6", "g2g3", "f8b4", "c1d2", "b4e7")
    val (_, obs, candidate) = currentEffectResponse(
      "rnbqkbnr/ppp1pppp/8/3p4/2PP4/8/PP2PPPP/RNBQKBNR b KQkq - 0 2", line.head,
      List(line -> -20, List("c7c6", "e2e3") -> 0, List("g8f6") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Bb4+"), text)
    assertEquals("clears e7".r.findAllIn(text).length, 1, text)
    assertPublishedEffectSources(obs, candidate)

  test("PU2 an already owned check preserves the question's reply and its complete exchange"):
    val first = List("f3g4", "g1h1", "g4f3", "h1g1")
    val other = List("b8d7", "c2e4", "f3e4", "c3e4", "a7a5", "f1c1", "f8c8", "c4e2",
      "g8f8", "h2h4", "a8b8")
    val (state, obs, candidate) = currentEffectResponse(
      "rn3rk1/p4pp1/4p2p/8/1bBP4/2N1Pq2/1PQ2P1P/R4RK1 b - - 1 15", first.head,
      List(first -> -20, other -> 0, List("f3h3") -> 20))
    val questions = (obs \ "selection" \ "candidates").as[List[JsObject]].filter(value =>
      (value \ "detail" \ "alternative_move").asOpt[String].contains("b8d7"))
    assert(questions.nonEmpty, "the normal question comparison must carry this observed reply")
    questions.foreach { question =>
      val text = (question \ "detail" \ "text").as[String]
      assert(text.contains("returned reply Qe4"), text)
    }
    val owned = (candidate \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil)
      .filter(effect => (effect \ "scope").as[String] == "check_sequence").toSet
    val base = candidate + ("detail" -> (((candidate \ "detail").as[JsObject] - "effects") ++
      Json.obj("type" -> "question_comparison", "prefix_moves_uci" -> List.empty[String],
        "text" -> "The engine prefers Qg4+. After Nd7 the returned reply Qe4 attacks the queen and rook.")))
    val result = MoveWorkProjection.select(state.observationsPacket.get).withMoveEffects(base, owned)
    val text = (result \ "detail" \ "text").as[String]
    assert(text.contains("Qxe4") && text.contains("Nxe4"), text)
    assertPublishedEffectSources(obs, result)

  test("PU1 clearing a future bishop diagonal is used even when that bishop starts elsewhere"):
    val (_, obs, candidate) = currentEffectResponse("7k/5p1p/8/8/8/8/B1P5/6K1 w - - 0 1", "c2c3",
      List(List("c2c3", "h8g8", "a2b1") -> 20, List("g1f1") -> 0, List("g1h1") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("clears c2") && text.contains("Bb1") && text.contains("uses the cleared line") &&
      text.contains("Black's pawn on h7"), text)
    assert(functionScopes(candidate)("clearance_use"))
    assertPublishedEffectSources(obs, candidate)

  test("PU1 moving the old defender can leave the actual duty with an existing helper"):
    val (_, obs, candidate) = currentEffectResponse("7k/8/5n2/8/4P3/2N5/8/4R1K1 w - - 0 1", "c3b5",
      List(List("c3b5", "f6e4", "e1e4") -> 20, List("g1f1") -> 0, List("g1h1") -> -20))
    assert(functionScopes(candidate)("support_handoff"), (candidate \ "detail" \ "text").as[String])
    assertPublishedEffectSources(obs, candidate)

  test("PU1 the old defender may move while retaining support before its actual departure"):
    val (_, obs, candidate) = currentEffectResponse("7k/8/5n2/8/4P3/8/2B5/5RK1 w - - 0 1", "f1e1",
      List(List("f1e1", "h8h7", "c2d3", "h7h6", "d3f1", "f6e4", "e1e4") -> 20,
        List("g1f2") -> 0, List("g1h1") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(functionScopes(candidate)("support_handoff"), text)
    List("Bf1", "bishop on c2", "pawn on e4").foreach(value => assert(text.contains(value), text))
    assertPublishedEffectSources(obs, candidate)

  test("PU1 a later defender move cannot end support already lost when the target moved"):
    val (_, obs, candidate) = currentEffectResponse("4r2k/8/8/8/4P3/2N5/8/3R3K w - - 0 1", "d1e1",
      List(List("d1e1", "h8h7", "e4e5", "h7h6", "c3b5", "e8e5", "e1e5") -> 20,
        List("h1g1") -> 0, List("h1g2") -> -20))
    assert(!functionScopes(candidate)("support_handoff"), (candidate \ "detail" \ "text").as[String])
    assertPublishedEffectSources(obs, candidate)

  test("PU1 a later unpinning or a different recapturer cannot establish the root support's use"):
    val cases = List(
      ("7k/8/5n2/8/4P3/2N5/8/r4R1K w - - 0 1",
        List("f1e1", "h8h7", "c3b5", "f6e4", "h1h2", "h7h6", "e1e4"), "h1h2", "f1g1"),
      ("7k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1",
        List("f1e1", "h8h7", "c3b5", "f6e4", "b5c3", "h7h6", "c3e4"), "g1f2", "g1h1"))
    cases.foreach { (fen, line, second, third) =>
      val (_, obs, candidate) = currentEffectResponse(fen, line.head,
        List(line -> 20, List(second) -> 0, List(third) -> -20))
      assert(!functionScopes(candidate)("support_handoff"), (candidate \ "detail" \ "text").as[String])
      assertPublishedEffectSources(obs, candidate)
    }

  test("PU1 blocker identity, departure and continued obstruction are all required"):
    val fen = "r2qr1k1/pp3pbp/3p1np1/2pPn3/4P1b1/1PN5/P1QNBPPP/R1B2RK1 w - - 1 13"
    val lines = List(
      List("f2f3", "g4d7", "f3f4", "f6h5"),
      List("f2f3", "g8h8", "c1b2", "f6h5"),
      List("f2f3", "e5f3", "g2f3", "g4d7", "c1b2", "f6h5"))
    lines.foreach { line =>
      val (_, obs, candidate) = currentEffectResponse(fen, line.head,
        List(line -> 20, List("e2g4", "f6g4") -> 0, List("h2h3") -> -20))
      val effects = (candidate \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil)
      assert(!effects.exists(effect => (effect \ "scope").as[String] == "blocker_handoff" &&
        (effect \ "square").as[String] == "h5"), (candidate \ "detail" \ "text").as[String])
      assertPublishedEffectSources(obs, candidate)
    }

  test("PU1 another obstruction of the corridor ends the original clearance's continuous contribution"):
    val (_, obs, candidate) = currentEffectResponse("7k/8/8/8/8/8/4B3/4R1K1 w - - 0 1", "e2f3",
      List(List("e2f3", "h8g8", "f3e4", "g8h8", "e4f5", "h8g8", "e1e7") -> 20,
        List("e2d1") -> 0, List("g1h1") -> -20))
    assert(!functionScopes(candidate)("clearance_use"), (candidate \ "detail" \ "text").as[String])
    assertPublishedEffectSources(obs, candidate)

  test("PU1 the piece clearing the corridor may be captured elsewhere before the corridor is used"):
    val (_, obs, candidate) = currentEffectResponse("7k/8/8/8/6b1/8/4B3/4R1K1 w - - 0 1", "e2f3",
      List(List("e2f3", "g4f3", "e1e7") -> 20, List("e2d1") -> 0, List("g1h1") -> -20))
    assert(functionScopes(candidate)("clearance_use"), (candidate \ "detail" \ "text").as[String])
    assertPublishedEffectSources(obs, candidate)

  test("PU1 a longer supplied branch cannot donate a functional use to a shorter compared line"):
    val cases = List(
      ("7k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1",
        List("f1e1", "h8h7", "c3b5", "f6e4", "e1e4"), "g1f2", "g1h1"),
      ("r2qr1k1/pp3pbp/3p1np1/2pPn3/4P1b1/1PN5/P1QNBPPP/R1B2RK1 w - - 1 13",
        List("f2f3", "g4d7", "c1b2", "f6h5"), "e2g4", "h2h3"),
      ("7k/8/8/8/8/8/4B3/4R1K1 w - - 0 1",
        List("e2f3", "h8h7", "e1e7"), "e2d1", "g1h1"))
    cases.foreach { (fen, line, second, third) =>
      val (_, obs, candidate) = currentEffectResponse(fen, line.head,
        List(line.take(1) -> 20, List(second) -> 0, List(third) -> -20), supplied = List(line))
      assert(functionScopes(candidate).intersect(Set("support_handoff", "blocker_handoff", "clearance_use")).isEmpty,
        (candidate \ "detail" \ "text").as[String])
      assertPublishedEffectSources(obs, candidate)
    }

  test("the current rook support reaches the comparison without inventing an unrelated preparation"):
    val fen = "r2qkb1r/pb1n1ppp/3ppn2/1pp5/4P3/2PP1NP1/PP3PBP/RNBQ1RK1 w kq - 1 8"
    val line = List("f1e1", "f8e7", "a2a4")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 0, List("a2a4", "a7a6") -> 20, List("h2h3") -> -20))
    val support = "White's rook on e1 adds support to White's pawn on e4"
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains(support), text)
    assert(!text.contains("prepare") && !text.contains("enables"),
      "the observed e4 support does not establish a contribution to a4: " + text)
    assertPublishedEffectSources(obs, candidate)

  test("a current check and its actual king response reach the comparison without an ordinary attack seed"):
    val fen = "1r6/6pp/4k3/p1rpP3/R3pKP1/7P/R7/8 b - - 3 40"
    val (_, obs, candidate) = currentEffectResponse(fen, "b8f8",
      List(List("b8f8", "f4g3", "f8f3") -> -20, List("b8b7", "h3h4") -> 0, List("c5c3") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Rf8+ gives check to the White king on f4 with Black's rook on f8"), text)
    assert(text.contains("Kg3 moves the king to g3"), text)
    val effect = (candidate \ "detail" \ "effects").as[List[JsObject]]
      .find(value => (value \ "scope").as[String] == "check_sequence").get
    assert(text.contains("Rf3+ gives check to the White king on g3"), text)
    assertEquals((effect \ "through_plies").as[List[Int]], List(3))
    assertPublishedEffectSources(obs, candidate)

  test("an immediate pawn interposition answers the actual queen check"):
    val fen = "r2qk2r/ppp1b1pp/2n1b3/5p2/4p3/2PP2P1/P1P2PBP/R1BQNRK1 w kq - 1 11"
    val (_, obs, candidate) = currentEffectResponse(fen, "d1h5",
      List(List("d1h5", "g7g6", "h5e2") -> 0, List("d1e2", "c6e5") -> 20, List("a2a4") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Qh5+ gives check to the Black king on e8 with White's queen on h5"), text)
    assert(text.contains("g6 blocks the check on g6"), text)
    assertPublishedEffectSources(obs, candidate)

  test("a check-blocking rook move retains the queen capture and the rook cost in the opening transaction"):
    val fen = "7r/1pr3k1/p3Q1b1/3pR3/P7/6N1/5qPK/5R2 w - - 6 41"
    val (_, obs, candidate) = currentEffectResponse(fen, "e5h5",
      List(List("e5h5", "f2g3", "h2g3", "h8h5") -> 0, List("g3h5", "g6h5", "f1f2") -> 20,
        List("e6h3") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Rh5 blocks the check on h5"), text)
    assert(text.contains("Qxg3+ gives check"), text)
    assert(text.contains("Kxg3 moves the king to g3 and captures the checking queen on g3"), text)
    assert(text.contains("White king recaptures the black queen on g3 with Kxg3"), text)
    assert(text.contains("Black rook captures the white rook on h5 with Rxh5"), text)
    assertPublishedEffectSources(obs, candidate)

  test("a recapturing pawn opens the rook check rather than becoming the checking piece"):
    val fen = "1r5r/ppp1bpp1/4k3/2p1P3/2N2n1p/1P5P/PBP2PP1/3RR1K1 w - - 4 23"
    val (_, obs, candidate) = currentEffectResponse(fen, "d1d6",
      List(List("d1d6", "c7d6", "e5d6", "e6d5") -> 20, List("c4d6", "h8d8") -> 0,
        List("b2c1") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("cxd6 captures the checking rook on d6"), text)
    assert(text.contains("exd6+ opens White's rook on e1's line, giving check to the Black king on e6"), text)
    assert(text.contains("Kd5 moves the king to d5"), text)
    assert(!text.contains("with White's pawn on d6"), text)
    assertPublishedEffectSources(obs, candidate)

  test("a discovered double check distinguishes the opened rook line from the knight's direct check"):
    val (_, obs, candidate) = currentEffectResponse("4k3/8/8/8/4N3/8/8/4R1K1 w - - 0 1", "e4f6",
      List(List("e4f6", "e8d8") -> 20, List("e4g5", "e8d8") -> 0, List("e1e2") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("Nf6+ opens White's rook on e1's line"), text)
    assert(text.contains("White's knight on f6 also gives check"), text)
    assert(text.contains("Kd8 moves the king to d8"), text)
    assert(!text.contains("opens White's knight on f6"), text)
    assertPublishedEffectSources(obs, candidate)

  test("a current pawn support is delivered without claiming a new king restriction or successful defense"):
    val fen = "6k1/p4p1p/6p1/2R5/3qp3/2R3P1/P2nK2P/4N3 b - - 1 35"
    val line = List("e4e3", "c5c7", "d2e4")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 0, List("d2b1", "c3c2") -> -20, List("d4d6") -> 20))
    val support = "Black's pawn on e3 adds support to Black's knight on d2"
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains(support), text)
    assert(!text.contains("cannot enter") && !text.contains("prevents the king") && !text.contains("saves"),
      "Qd4 already controls d2/f2 and the knight actually leaves d2: " + text)
    assertPublishedEffectSources(obs, candidate)

  test("the current quiet move and the alternative's complete opening exchange are both delivered"):
    val fen = "r2qkb1r/pb1n1ppp/3ppn2/1pp5/4P3/2PP1NP1/PP3PBP/RNBQ1RK1 w kq - 1 8"
    val (_, obs, candidate) = currentEffectResponse(fen, "f1e1",
      List(List("f1e1", "f8e7") -> 20,
        List("a2a4", "b5a4", "a1a4") -> 0, List("h2h3") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("White's rook on e1 adds support to White's pawn on e4"), text)
    assert(text.contains("Black pawn captures the white pawn on a4 with bxa4") &&
      text.contains("White rook recaptures the black pawn on a4 with Rxa4"), text)
    assertPublishedEffectSources(obs, candidate)

  private val knightReturnFen = "1r1qr1k1/1ppnbpp1/2n3bp/pN6/2PPp3/P3BNPP/1P1Q1PBK/3RR3 w - - 0 19"
  private val knightReturnLine = List("f3g1", "d7f6", "g1e2", "d8d7", "e2c3")

  test("the evading knight's actual route reaches an attack on the same original pawn"):
    val (_, obs, candidate) = currentEffectResponse(knightReturnFen, "f3g1",
      List(knightReturnLine -> 20, List("e3f4", "e4f3") -> 0, List("f3h4") -> -20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("out of the attack from Black's pawn on e4"), text)
    assert(text.contains("Ne2") && text.contains("Nec3") && text.contains("White's knight on c3"), text)
    val line = (obs \ "paths").as[List[JsObject]].find(value =>
      (value \ "moves").as[List[String]] == knightReturnLine).get
    val occurrences = (line \ "occurrence_ids").as[List[String]]
    assert((candidate \ "detail" \ "effects").as[List[JsObject]].exists(effect =>
      (effect \ "line_ids").as[List[String]] == List((line \ "id").as[String]) &&
      (effect \ "through_plies").as[List[Int]] == List(5) &&
      List(0, 2, 4).forall(offset => (effect \ "occurrence_ids").as[List[String]].contains(occurrences(offset)))),
      "the route and its actual attack retain their exact owned observations")
    assert(!text.contains("best route") && !text.contains("must prepare"), text)
    assertPublishedEffectSources(obs, candidate)

  test("a short line or another knight's attack cannot complete the original knight's evasion"):
    List(knightReturnLine.take(3), List("f3g1", "d7f6", "b5c3")).foreach { line =>
      val (_, obs, candidate) = currentEffectResponse(knightReturnFen, "f3g1",
        List(line -> 20, List("e3f4", "e4f3") -> 0, List("f3h4") -> -20),
        supplied = List(knightReturnLine))
      val text = (candidate \ "detail" \ "text").as[String]
      assert(text.contains("out of the attack from Black's pawn on e4"), text)
      assert(!text.contains("Nec3") && !text.contains("White knight on c3"),
        "neither a longer source nor the b5 knight owns this evasion's later use: " + text)
      assertPublishedEffectSources(obs, candidate)
    }

  test("a vacated square is connected to another allied piece actually entering it"):
    val fen = "4kb2/6p1/8/8/8/8/8/7K b - - 0 1"
    val line = List("g7g6", "h1h2", "f8g7")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("e8d8") -> 0, List("e8d7") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(text.contains("g6 clears g7") && text.contains("Bg7") && text.contains("Bg7 later occupies that square"), text)
    assert(!text.contains("best plan") && !text.contains("must prepare"), text)
    val path = (obs \ "paths").as[List[JsObject]].find(value =>
      (value \ "moves").as[List[String]] == line).get
    assert((candidate \ "detail" \ "effects").as[List[JsObject]].exists(effect =>
      (effect \ "line_ids").as[List[String]] == List((path \ "id").as[String]) &&
      (effect \ "through_plies").as[List[Int]] == List(3)),
      "the actual entry is bounded by its own third ply")
    assertPublishedEffectSources(obs, candidate)

  test("the vacated-square use cannot be borrowed from an unowned longer line"):
    val fen = "4kb2/6p1/8/8/8/8/8/7K b - - 0 1"
    val line = List("g7g6", "h1h2", "f8g7")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line.take(1) -> -20, List("e8d8") -> 0, List("e8d7") -> 20), supplied = List(line))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(!text.contains("Bg7") && !text.contains("Bg7 later occupies that square"), text)
    assertPublishedEffectSources(obs, candidate)

  test("an intervening occupation ends the original clearance's ownership of a later entry"):
    val fen = "4kb2/6p1/4n3/8/8/8/8/7K b - - 0 1"
    val line = List("g7g6", "h1h2", "e6g7", "h2h1", "g7e6", "h1h2", "f8g7")
    val (_, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -20, List("e8d8") -> 0, List("e8d7") -> 20))
    val text = (candidate \ "detail" \ "text").as[String]
    assert(!text.contains("Bg7") && !text.contains("Black bishop on g7"),
      "the knight's later departure, not the original pawn move, owns the bishop's eventual entry: " + text)
    assertPublishedEffectSources(obs, candidate)

  test("a return observed in the other compared arm cannot extend the short current evasion"):
    val short = List("f3g1")
    val other = List("f3h4", "d7f6", "h4f3", "d8d7", "f3g1", "d7d8", "g1e2", "d8d7", "e2c3")
    val (_, obs, candidate) = currentEffectResponse(knightReturnFen, short.head,
      List(short -> 20, other -> 0, List("e3f4", "e4f3") -> -20))
    val paths = (obs \ "paths").as[List[JsObject]]
    def path(moves: List[String]) = paths.find(value => (value \ "moves").as[List[String]] == moves).get
    val currentPath = path(short)
    val otherPath = path(other)
    val effects = (candidate \ "detail" \ "effects").as[List[JsObject]]
    val current = effects.filter(effect =>
      (effect \ "line_ids").as[List[String]] == List((currentPath \ "id").as[String]))
    assert(current.nonEmpty, "the current evasion still has its own explanation")
    current.foreach { effect =>
      assertEquals((effect \ "through_plies").as[List[Int]], List(1))
      assert((effect \ "occurrence_ids").as[List[String]].toSet.subsetOf(
        (currentPath \ "occurrence_ids").as[List[String]].toSet))
    }
    assert(effects.exists(effect =>
      (effect \ "line_ids").as[List[String]] == List((otherPath \ "id").as[String]) &&
      (effect \ "through_plies").as[List[Int]] == List(9) &&
      (effect \ "target" \ "square").asOpt[String].contains("e4")),
      "the other arm can explain its actual return, with its own evidence and horizon")
    assertPublishedEffectSources(obs, candidate)

  test("a current support already owned by the body is not repeated in another comparison"):
    val fen = "r2qkb1r/pb1n1ppp/3ppn2/1pp5/4P3/2PP1NP1/PP3PBP/RNBQ1RK1 w kq - 1 8"
    val line = List("f1e1", "f8e7", "a2a4")
    val (state, _, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 0, List("a2a4", "a7a6") -> 20, List("h2h3") -> -20))
    val support = "White's rook on e1 adds support to White's pawn on e4"
    val owned = (candidate \ "detail" \ "effects").as[List[JsObject]].find(effect =>
      (effect \ "actor" \ "square").as[String] == "e1" &&
        (effect \ "target" \ "square").asOpt[String].contains("e4")).get
    val base = candidate + ("detail" -> (((candidate \ "detail").as[JsObject] - "effects") +
      ("text" -> JsString("The observed material balance is equal."))))
    val projected = MoveWorkProjection.select(state.observationsPacket.get).withMoveEffects(base, Set(owned))
    assert(!(projected \ "detail" \ "text").as[String].contains(support))
    assert(!(projected \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil).contains(owned))

  test("a quiet current effect cannot displace a complete attack history at either text limit"):
    val fen = "rr5k/8/8/8/8/8/8/R6K w - - 0 1"
    val line = List("a1b1", "b8b7", "b1c1")
    val (state, _, candidate) = currentEffectResponse(fen, line.head,
      List(line -> 0, List("h1h2", "b8b7") -> 20, List("h1g1") -> -20))
    val projection = MoveWorkProjection.select(state.observationsPacket.get)
    val quiet = (candidate \ "detail" \ "effects").as[List[JsObject]].find(effect =>
      (effect \ "actor" \ "square").as[String] == "b1" &&
        (effect \ "target" \ "square").asOpt[String].contains("a8")).get
    List("root_comparison" -> 4000, "pv_comparison" -> 1000).foreach { (kind, limit) =>
      def base(text: String) = candidate + ("detail" -> (((candidate \ "detail").as[JsObject] - "effects") ++
        Json.obj("type" -> kind, "text" -> text)))
      val original = "Existing comparison."
      val core = projection.withMoveEffects(base(original), Set(quiet))
      val coreText = (core \ "detail" \ "text").as[String]
      assert(coreText.contains("Rc1:") && coreText.contains("White rook on b1 moves to c1, ending its attack on Black rook on b7"), coreText)
      val coreEffects = (core \ "detail" \ "effects").as[List[JsObject]]
      val padding = "x" * (limit - (coreText.length - original.length) - 16)
      val bounded = projection.withMoveEffects(base(padding), Set.empty)
      val text = (bounded \ "detail" \ "text").as[String]
      assert(text.length <= limit)
      assert(text.contains("Rc1:") && text.contains("White rook on b1 moves to c1, ending its attack on Black rook on b7"), text)
      assert(!text.contains("out of the attack from Black's rook on a8"),
        "the new current effect yields its place when the existing complete history fills the limit")
      assertEquals((bounded \ "detail" \ "effects").as[List[JsObject]], coreEffects,
        "only the effects whose complete explanation remains in the body are published")
    }

  test("an oversized route bundle cannot consume a capture needed by a smaller admitted history"):
    val fen = "3r4/5k2/1p3b1p/2p2p2/p1b2B2/P1P3PP/1P3K2/2N1R3 b - - 0 31"
    val line = "f6g5 e1e5 f7f6 f2e1 g5f4 g3f4".split(" ").toList
    val (state, obs, candidate) = currentEffectResponse(fen, line.head,
      List(line -> -40, List("b6b5") -> -20, List("f7g6") -> 0))
    assert(functionScopes(candidate)("piece_route"))
    val base = candidate + ("detail" -> (((candidate \ "detail").as[JsObject] - "effects") +
      ("text" -> JsString("x" * 3450))))
    val bounded = MoveWorkProjection.select(state.observationsPacket.get).withMoveEffects(base, Set.empty)
    val text = (bounded \ "detail" \ "text").as[String]
    assert(!functionScopes(bounded)("piece_route"), text)
    assert(text.contains("Bxf4 captures the initially attacked White bishop on f4") &&
      text.contains("gxf4 recaptures Black bishop on f4") && text.contains("opens the g-file"), text)
    assertEquals(text.sliding("gxf4 recaptures".length).count(_ == "gxf4 recaptures"), 1, text)
    assert(text.length <= 4000)
    assertPublishedEffectSources(obs, bounded)

  test("passed-turn work cannot grade a real move and only proposes legal reply comparisons"):
    val fen = "r4k1r/pb1n1ppp/1p1P1b2/2p1N3/1n6/2N3P1/PP3PBP/R1B1R1K1 b - - 6 18"
    val owner = history(fen)
    val played = "b7g2"
    val start = awaiting(CommentaryJobReducer.startJob(fen, Nil, fen, played,
      owner.extend(List(played)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, probeThreats = true), 0L).toOption.get)
    // Owned, legal stub reports exercise the transport boundary; these scores make no chess-quality claim.
    val probe = awaiting(submit(start, List(
      List("d7e5", "g2b7", "a8d8") -> 14,
      List(played, "e1d1") -> 300,
      List("a8d8", "e5d7", "d8d7") -> 14)))
    val packet = probe.observationsPacket.get
    val beforeEvaluations = packet.assembly.input.assessmentCandidates.map(_.evaluation)
    val work = probe.issuedWork
    assertEquals(work.purpose, EngineWorkPurpose.ThreatProbe)
    assertEquals(work.threatOriginPrefixUci, Some(Nil))
    assertEquals(work.searchFen, "r4k1r/pb1n1ppp/1p1P1b2/2p1N3/1n6/2N3P1/PP3PBP/R1B1R1K1 w - - 6 19")
    assertEquals(work.enginePositionMovesUci, Nil)
    assert(probe.assessment.nonEmpty)
    val hints = List(List("e5d7", "f8g8", "d7f6", "g7f6"), List("e5c4"))
      .map(moves => ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(99999), 8))
    val admitted = EngineLineAdmission.admitReportedLineSuffixes(work, hints).get
    assert(EngineLineAdmission.bindAdmittedLineSuffixes(work, admitted).isEmpty,
      "even a valid hypothetical PV cannot bind to commentary evidence")
    val next = awaiting(CommentaryJobReducer.submitIssuedWorkReport(probe,
      EngineWorkReport.Completed(profile, work.workId, work.executionKeySha256, hints), 1L).toOption.get)
    assert(next.observationsPacket.get eq packet)
    assertEquals(next.observationsPacket.get.assembly.input.assessmentCandidates.map(_.evaluation), beforeEvaluations)
    assertEquals(next.issuedWork.threatOriginPrefixUci, Some(List(played)))
    assert(!next.rootSearches.exists(_.work.purpose == EngineWorkPurpose.ThreatProbe))
    val questions = next.proposalEntries.filter(_.request.origin.question.producer == "pass-probe")
    assertEquals(questions.flatMap(_.request.requirements.collect { case pair: CompareMoves =>
      (pair.prefixMovesUci, pair.referenceMove, pair.alternativeMove)
    }), List((List(played), "e1d1", "e5d7")))
    assert(questions.flatMap(_.readings).forall(_.answer.isEmpty), "a suggestion has no real-position strength yet")
    List(EngineWorkFailure.Cancelled, EngineWorkFailure.BudgetExhausted, EngineWorkFailure.ExecutionFailed).foreach { failure =>
      val pending = next.issuedWork
      val finished = CommentaryJobReducer.submitIssuedWorkReport(next, EngineWorkReport.ExecutorFailed(
        profile, pending.workId, pending.executionKeySha256, failure), 2L).toOption.get
      assert(finished.isInstanceOf[CompletedCommentaryJob], "optional work must preserve the usable assessment")
      assertEquals(finished.observationsPacket.get.assembly.input.assessmentCandidates.map(_.evaluation), beforeEvaluations)
      assertEquals(finished.verification, next.verification)
    }
    val malformed = CommentaryJobReducer.submitIssuedWorkReport(probe, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, hints.updated(0, hints.head.copy(moves = List("e5e6")))), 2L).toOption.get
    assert(malformed.isInstanceOf[CompletedCommentaryJob])
    assertEquals(malformed.observationsPacket.get.assembly.input.assessmentCandidates.map(_.evaluation), beforeEvaluations)
    assertEquals(owner.currentFen, fen)
    val verifiedReplyQuestion = awaiting(submit(next, List(List("f6e7") -> 0, List("b4c6") -> 0)))
    assertEquals(verifiedReplyQuestion.issuedWork.purpose, EngineWorkPurpose.ResponseComparison)
    assertEquals(verifiedReplyQuestion.issuedWork.searchAnchor.map(_.prefixMoves), Some(List(played)))
    assertEquals(verifiedReplyQuestion.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List("e1d1", "e5d7")))

  test("passed-turn origins clear en-passant and reject checked or terminal positions"):
    val initial = history(chess.variant.Standard.initialFen.value)
    val afterE4 = initial.extend(List("e2e4")).toOption.get
    val skippedBlack = CommentaryThreat.hypothesis(afterE4).get
    assertEquals(skippedBlack.currentFen.split(" ").toList.drop(1), List("w", "KQkq", "-", "0", "2"))
    assertEquals(skippedBlack.movePrefixUci, Nil)
    val afterD5 = afterE4.extend(List("d7d5")).toOption.get
    assertEquals(CommentaryThreat.hypothesis(afterD5).get.currentFen.split(" ").toList.drop(1),
      List("b", "KQkq", "-", "0", "2"))
    val checked = history("r4k1r/pb1N1ppp/1p1P1b2/2p5/1n6/2N3P1/PP3PBP/R1B1R1K1 b - - 0 19")
    assert(checked.currentPosition.check.yes)
    assert(CommentaryThreat.hypothesis(checked).isEmpty)
    assert(CommentaryThreat.hypothesis(history("4k3/8/8/8/8/8/8/4K3 w - - 0 1")).isEmpty)
