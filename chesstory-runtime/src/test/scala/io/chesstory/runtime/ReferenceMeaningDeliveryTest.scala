package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, PositionResourceView, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{CanonicalPositionHistory, EngineLine}
import play.api.libs.json.*

/** A contract check with fixed book continuations, not an engine or quality measurement.
  * The reducer issues every work; the test returns synthetic scores at that work's depth.
  * Kh1 is only the legal one-ply filler required by the root MultiPV=3 contract. Its
  * comparison and chess interpretation are not assessed. The two book PVs stay unchanged.
  */
class ReferenceMeaningDeliveryTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val fen = "3rr1k1/pppq1pb1/5nnp/3P2p1/P2p4/2P1NNBP/1P3PP1/R2QR1K1 w - - 0 19"
  private val reference = "d1d4 f6e4 g3e5 g7e5 f3e5 e8e5 e3g4 e5d5 d4e4".split(" ").toList
  private val alternative = "c3d4 f6d5 e3d5 e8e1 d1e1 d7d5 g3c7 d8c8 a1c1 g7d4 e1d2 d5d7 f3d4 c8c7 c1c7 d7c7 d4f5".split(" ").toList
  private val pairMoves = Set(reference.head, alternative.head)
  private val knight = Piece("white", "knight", "e3")
  private val rook = Piece("white", "rook", "e1")
  private val enemyKnight = Piece("black", "knight", "f6")
  private val enemyRook = Piece("black", "rook", "e8")
  private def history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get

  private def refs(value: JsValue): List[JsObject] =
    value.as[List[JsValue]].collect { case move: JsObject => move }

  private def locateRef(paths: ObservedLineProtocol.PreparedPaths, ref: JsObject) =
    val path = paths.paths.find(_.line.ref.id == (ref \ "line_id").as[String])
      .getOrElse(fail(s"unowned line reference: $ref"))
    val offset = path.ids.indexOf((ref \ "occurrence_id").as[String])
    assert(offset >= 0, s"move reference belongs to a different path: $ref")
    (path, offset)

  private def assertOwnedBilingualUnits(units: List[JsObject], paths: ObservedLineProtocol.PreparedPaths,
      root: Option[JsObject] = None): Unit =
    units.foreach { unit =>
      val visible = refs(unit("en"))
      val korean = refs(unit("ko"))
      val required = (unit \ "required_moves").asOpt[List[JsObject]].getOrElse(Nil)
      assertEquals(visible.toSet, korean.toSet, "both realizations name the same owned events")
      required.foreach(ref => assert(visible.contains(ref) && korean.contains(ref),
        s"a required move must be displayed in both languages: $ref"))
      refs(unit("covered_moves")).foreach(ref => assert(visible.contains(ref) && korean.contains(ref),
        s"a claimed material cost must be displayed in both languages: $ref"))
      (visible ++ refs(unit("covered_moves")) ++ required).foreach { ref =>
        locateRef(paths, ref)
        root.foreach(candidate => assert((candidate \ "evidence_ids").as[List[String]]
          .contains((ref \ "occurrence_id").as[String]), s"unpublished occurrence: $ref"))
      }
    }

  private def coreUnits(value: JsObject): List[JsObject] = (value \ "units").as[List[JsObject]]
  private def detailUnits(value: JsObject): List[JsObject] =
    (value \ "detail_units").asOpt[List[JsObject]].getOrElse(Nil)
  private def detailRefs(value: JsObject): List[JsObject] =
    ((value \ "detail_moves").asOpt[JsValue].toList.flatMap(refs) ++
      detailUnits(value).flatMap(unit => refs(unit("en")))).distinct

  private def selectedFacts(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths,
      root: JsObject): CommentaryAnswer.Answer =
    val pair = (root \ "line_ids").as[List[String]].map(id => paths.paths.find(_.line.ref.id == id).get)
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val answer = RootQuestionSummary.choose(packet, paths, pair, positions).getOrElse(
      fail("the selected question needs its own confirmed facts")).confirmedFacts
    assertEquals(answer.question, (root \ "detail" \ "summary_question" \ "question").as[String])
    answer

  private def selectedSequences(answer: CommentaryAnswer.Answer): List[RootQuestionSummary.MoveRef] =
    (answer.core ++ answer.details).flatMap(_.statements).flatMap {
      case CommentaryAnswer.Continuation(moves) => moves
      case CommentaryAnswer.Convergence(left, right, _) => left ++ right
      case _ => Nil
    }.distinct

  private def assertConvergenceDelivery(root: JsObject, summary: JsObject,
      paths: ObservedLineProtocol.PreparedPaths, meeting: Int, divergence: Int): Unit =
    val pair = (root \ "line_ids").as[List[String]].map(id => paths.paths.find(_.line.ref.id == id).get)
    val core = coreUnits(summary).flatMap(unit => refs(unit("en"))).toSet
    val displayed = core ++ detailRefs(summary)
    assertEquals(pair.map(_.positions(meeting).fen).distinct.size, 1,
      "both owned approaches reach the same recorded state")
    assertEquals(pair.map(_.ids(meeting - 1)).distinct.size, 2,
      "the common board does not merge the two approach occurrences")
    pair.foreach { path =>
      ((0 until meeting).toList :+ divergence).foreach(offset =>
        assert(core(RootQuestionSummary.MoveRef(path, offset).json),
          "both approaches and each actual later choice remain in the core"))
    }
    assert(pair.exists(path => (meeting until divergence).forall(offset =>
      displayed(RootQuestionSummary.MoveRef(path, offset).json))),
      "the shared continuation remains complete in one owning line, in core or detail")

  private def assertPhysicalRecapture(paths: ObservedLineProtocol.PreparedPaths,
      path: ObservedLineProtocol.PreparedPath, offsets: List[Int]): Set[MoveWorkProjection.Piece] =
    assertEquals(offsets.size, 2)
    assertEquals(offsets.last, offsets.head + 1)
    val events = paths.continuationReadings.find(_.path eq path).get.events
    val loss = events.find(_.plyOffset == offsets.head).get
    val recovery = events.find(_.plyOffset == offsets.last).get
    assertEquals(recovery.originalVictim, loss.originalActor,
      "the second capture takes the same physical piece that made the first")
    assert(path.certified.flatMap(_._2.material).get.captures
      .find(_.plyOffset == offsets.last).exists(_.recapture))
    (loss.originalVictim.toList ++ recovery.originalVictim).toSet

  // Reordering input lines may change sentences and unit grouping. It must not
  // change the displayed events, cost coverage, required replies or path owners.
  private def summaryMeaning(value: JsObject) =
    val units = coreUnits(value)
    val details = detailUnits(value)
    val visible = (for
      language <- List("en", "ko")
      unit <- units
      ref <- refs(unit(language))
    yield (language, ref)).toSet
    (visible, units.flatMap(unit => refs(unit("covered_moves"))).toSet,
      units.flatMap(unit => (unit \ "required_moves").asOpt[List[JsObject]].getOrElse(Nil)).toSet,
      detailRefs(value).toSet, details.flatMap(unit => refs(unit("covered_moves"))).toSet,
      (value \ "witness_line_ids").asOpt[List[String]].getOrElse(Nil).toSet)

  private def projectRoot(start: String, played: String, lines: List[EngineLine]) =
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = start,
      playedMoveUci = played, variations = lines)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val root = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    (packet, paths, root)

  private def completePairSummary(root: JsObject, paths: ObservedLineProtocol.PreparedPaths, question: String): JsObject =
    val detail = (root \ "detail").as[JsObject]
    assertEquals((detail \ "summary_kind").as[String], "question_answer")
    val summary = (detail \ "summary_question").as[JsObject]
    assertEquals((summary \ "question").as[String], question)
    val units = (summary \ "units").as[List[JsObject]]
    val details = (summary \ "detail_units").asOpt[List[JsObject]].getOrElse(Nil)
    assertOwnedBilingualUnits(units ++ details, paths, Some(root))
    val pair = (root \ "line_ids").as[List[String]].map(id => paths.paths.find(_.line.ref.id == id).get)
    val captures = pair.flatMap(path => path.certified.get._2.material.get.captures
      .map(event => RootQuestionSummary.MoveRef(path, event.plyOffset).json)).toSet
    def assertLayer(layer: List[JsObject], wholeLedger: Boolean): Unit =
      val visible = layer.flatMap(unit => refs(unit("en"))).toSet
      val covered = layer.flatMap(unit => refs(unit("covered_moves"))).toSet
      val recordedEnds = layer.flatMap(unit =>
        (unit \ "recorded_end_moves").asOpt[List[JsObject]].getOrElse(Nil)).toSet
      assertEquals(covered, captures.intersect(visible),
        "coverage names exactly the original ledger captures actually displayed in this layer")
      if wholeLedger then assertEquals(covered, captures, "detail retains the entire original capture ledger")
      visible.foreach { ref =>
        val (path, _) = locateRef(paths, ref)
        assert(pair.exists(_ eq path), "a layer cannot borrow a sibling line's occurrence")
      }
      recordedEnds.foreach { ref =>
        val (path, offset) = locateRef(paths, ref)
        assert(visible(ref), "the recorded end must be visible in its own layer")
        assertEquals(offset, path.moves.size - 1)
        assert(path.automaticTerminal.isEmpty, "a finite record cannot replace an actual terminal result")
      }
      pair.foreach { path =>
        def at(offset: Int) = RootQuestionSummary.MoveRef(path, offset).json
        val fact = path.certified.get._2
        val material = fact.material.get
        val checks = fact.lineEvents.filter(_.kind == LineEventKind.Check).map(_.plyOffset).toSet
        assert(visible(at(0)), "both root choices retain their exact occurrence")
        checks.filter(offset => visible(at(offset))).foreach { offset =>
          if offset + 1 < path.moves.size then
            assert(visible(at(offset + 1)), "a displayed actual check retains its recorded response")
          else assert(path.automaticTerminal.nonEmpty || recordedEnds(at(offset)),
            "a final check needs either its actual terminal result or the explicit finite boundary")
        }
        material.captures.filter(_.recapture).foreach { recovery =>
          material.captures.find(_.plyOffset == recovery.plyOffset - 1).foreach { loss =>
            if visible(at(loss.plyOffset)) then assert(visible(at(recovery.plyOffset)),
              "a displayed capture cannot omit its original ledger's immediate recapture")
          }
        }
        val end = at(path.moves.size - 1)
        if path.automaticTerminal.nonEmpty then
          assert(visible(end), "the actual terminal move remains in each delivered layer")
          assert(!recordedEnds(end), "an automatic terminal is not an unrecorded next reply")
        else if path.moves.size == 1 then
          assert(recordedEnds(end), "a one-move finite record keeps its explicit horizon")
        if wholeLedger then
          val required = material.events.map(_.plyOffset) ++ checks ++
            checks.filter(_ + 1 < path.moves.size).map(_ + 1)
          required.foreach(offset => assert(visible(at(offset)),
            s"detail lost an original material event or check response: ${path.moves(offset)}"))
      }
    assertLayer(units, wholeLedger = false)
    assertLayer(details, wholeLedger = true)
    summary

  private def rejectUnansweredQuestion(root: JsObject, paths: ObservedLineProtocol.PreparedPaths,
      question: String, reason: String): Unit =
    (root \ "detail" \ "summary_question").asOpt[JsObject].foreach { summary =>
      assertNotEquals((summary \ "question").as[String], question, reason)
      val units = (summary \ "units").as[List[JsObject]] ++
        (summary \ "detail_units").asOpt[List[JsObject]].getOrElse(Nil)
      assertOwnedBilingualUnits(units, paths, Some(root))
      units.flatMap(unit => refs(unit("covered_moves"))).foreach { ref =>
        val (path, offset) = locateRef(paths, ref)
        assert(path.certified.flatMap(_._2.material).get.captures.exists(_.plyOffset == offset),
          "a different answer still needs its own observed cost")
      }
    }

  private def awaiting(state: CommentaryJobState): AwaitingEngineWork = state match
    case active: AwaitingEngineWork => active
    case stopped: StoppedCommentaryJob => fail(s"fixed response stopped: ${stopped.stopCondition}")
    case _ => fail(s"expected the reducer's issued work, got ${state.getClass.getSimpleName}")

  private def submit(state: AwaitingEngineWork, rows: List[(List[String], Int)]): CommentaryJobState =
    val work = state.issuedWork
    assertEquals(work.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(work.multiPv, 3)
    CommentaryJobReducer.submitIssuedWorkReport(state,
      EngineWorkReport.Completed(profile, work.workId, work.executionKeySha256,
        rows.map((moves, cp) => ReportedEngineLineSuffix(moves,
          ReportedWhiteEngineScore.Centipawns(cp), work.requiredDepth))), 1L)
      .fold(rejection => fail(rejection.toString), identity)

  private lazy val fixedJob: (CommentaryJobState, IssuedEngineWork, JsObject) =
    val owner = history
    val rows = List(reference -> 0, alternative -> 0, List("g1h1") -> -1000)
    rows.foreach((moves, _) => assert(owner.extend(moves).isRight, moves.mkString(" ")))
    val initial = CommentaryJobReducer.startJob(owner.initialFen, owner.movePrefixUci, fen, reference.head,
      owner.extend(reference.take(1)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L)
      .fold(rejection => fail(rejection.toString), identity)
    val preview = awaiting(initial)
    assertEquals(preview.issuedWork.requiredDepth, 8)
    val full = awaiting(submit(preview, rows.map((moves, cp) => moves.take(1) -> cp)))
    assertEquals(full.issuedWork.requiredDepth, 16)
    val state = submit(full, rows)
    assert(!state.isInstanceOf[StoppedCommentaryJob], state.verification.toString)
    val observations = (PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("r" * 32, "fixed-book-meaning-contract", 10000L, state)) \ "observations").as[JsObject]
    sys.props.get("chesstory.reference-output").foreach { directory =>
      val out = java.nio.file.Path.of(directory)
      java.nio.file.Files.createDirectories(out)
      java.nio.file.Files.writeString(out.resolve("normal-ng4.json"), Json.stringify(observations))
    }
    (state, full.issuedWork, observations)

  private def currentCause(entries: List[Entry]): (Entry, Fact, Reading, CompareMoves, String) =
    entries.flatMap { entry =>
      entry.request.requirements.collect {
        case fact: Fact if fact.id.startsWith("current_tactic_") && fact.id.endsWith("_cause") &&
            fact.predicate == Predicate("location", Some(knight)) && fact.expected.contains("g4") &&
            fact.at == Event("move", 1, "after", 6, 7, actor = Some(knight), square = Some("g4")) => fact
      }.flatMap { fact =>
        fact.path.source match
          case Comparison(id, arm) => entry.request.requirements.collect {
            case question: CompareMoves if question.id == id && question.prefixMovesUci.isEmpty &&
                Set(question.referenceMove, question.alternativeMove) == pairMoves =>
              (entry, fact, entry.readings.find(_.requirementId == fact.id).get, question, arm)
          }
          case _ => Nil
      }
    }.headOption.getOrElse(fail("Ng4's generated current cause is missing from the original book pair"))

  private def selectedBookPair(observations: JsObject, ids: Set[String]): List[JsObject] =
    (observations \ "selection" \ "candidates").as[List[JsObject]].filter(candidate =>
      (candidate \ "selection").as[String] == "selected" &&
        Set("root_comparison", "question_comparison")((candidate \ "detail" \ "type").as[String]) &&
        (candidate \ "line_ids").as[List[String]].toSet == ids &&
        (candidate \ "detail" \ "prefix_moves_uci").asOpt[List[String]].forall(_.isEmpty))

  test("direct book input does not invent the comparison receipt needed by the current Ng4 cause"):
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen, playedMoveUci = reference.head,
      variations = List(reference, alternative).map(moves => EngineLine(moves, 0, None, 8)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    val batch = CommentaryDiscovery.discover(packet, prepared,
      List(CommentaryDiscovery.Anchor(Nil, reference.head)),
      List(reference, alternative).map(moves => CommentaryDiscovery.SourcePath(moves,
        Source("author", "fixed-book-meaning-contract"))), Nil, pvGuided = true)
    val entries = CommentaryFactReader.read(packet, prepared, batch.additions)
    val (entry, fact, reading, comparison, _) = currentCause(entries)
    val declared = entry.readings.find(_.requirementId == fact.id.stripPrefix("current_")).get
    assertEquals(declared.state, "supported")
    assert(entry.readings.find(_.requirementId == comparison.id).get.answer.isEmpty)
    assertEquals(reading.state, "partial")
    assert(reading.result.exists(_.needs.exists(_.reason == "comparison_not_answered")))

  test("issued fixed reports bind Ng4's current cause to the exact book arm and physical pieces"):
    val (state, work, observations) = fixedJob
    val packet = state.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val refPath = prepared.paths.find(_.moves == reference).getOrElse(fail("the Qxd4 book PV changed"))
    val altPath = prepared.paths.find(_.moves == alternative).getOrElse(fail("the cxd4 book PV changed"))
    assert(refPath.certified.nonEmpty && refPath.ids == refPath.declared)
    assert(altPath.certified.nonEmpty && altPath.ids == altPath.declared)
    val (entry, fact, reading, comparison, arm) = currentCause(state.proposalEntries)
    val answer = entry.readings.find(_.requirementId == comparison.id).flatMap(_.answer)
      .getOrElse(fail("normal reducer did not bind the book comparison"))
    assertEquals(answer.workId, work.workId)
    assertEquals(answer.executionKeySha256, work.executionKeySha256)
    assertEquals(answer.requiredDepth, work.requiredDepth)
    assertEquals(answer.referenceDepth, Some(work.requiredDepth))
    assertEquals(answer.alternativeDepth, Some(work.requiredDepth))
    assertEquals(Set(answer.referenceLineId, answer.alternativeLineId), Set(refPath.line.ref.id, altPath.line.ref.id))
    val ownedLine = if arm == "reference" then answer.referenceLineId else answer.alternativeLineId
    assertEquals(ownedLine, refPath.line.ref.id)
    assertEquals(reading.state, "supported")
    val result = reading.result.get
    assert(result.samples.exists(sample => sample.lineId == ownedLine && sample.state == "known" &&
      sample.moves.startsWith(reference.take(7)) && reference.startsWith(sample.moves) &&
      sample.fromPly == 7 && sample.throughPly == 7 && sample.value.contains("g4")))
    assert(result.proofs.exists(proof => proof.requirementIds.contains(fact.id) &&
      proof.bindings.values.exists(moves => moves.startsWith(reference.take(7)) && reference.startsWith(moves))))

    assertEquals(prepared.pieceAfter(packet, refPath, knight.witness, 7),
      Some(Some(Piece("white", "knight", "g4").witness)))
    assertEquals(prepared.pieceAfter(packet, refPath, Piece("white", "knight", "f3").witness, 7), Some(None))
    assertEquals(prepared.pieceAfter(packet, refPath, enemyRook.witness, 8),
      Some(Some(Piece("black", "rook", "d5").witness)))
    assertEquals(prepared.pieceAfter(packet, refPath, enemyKnight.witness, 9), Some(None))
    val local = MoveWorkProjection.select(packet, prepared).local
    val attacks = local.filter(_.edge.evidence.id == refPath.ids(6)).flatMap(_.note.detail match
      case MoveWorkProjection.Detail.Attack(actor, target, true) => Some(actor -> target)
      case _ => None).toSet
    assert(attacks(Piece("white", "knight", "g4").witness -> Piece("black", "rook", "e5").witness))
    assert(attacks(rook.witness -> Piece("black", "knight", "e4").witness))
    val captures = local.flatMap(note => note.note.detail match
      case MoveWorkProjection.Detail.Capture(role, at, _) => Some((note.edge.evidence.id, role, at))
      case _ => None).toSet
    assert(captures((refPath.ids(7), "pawn", "d5")), "Rxd5 really captures the original white d5 pawn")
    assert(captures((refPath.ids(8), "knight", "e4")), "Qxe4 captures the original black f6 knight")
    assert(selectedBookPair(observations, Set(refPath.line.ref.id, altPath.line.ref.id)).nonEmpty)

  test("the normal book pair delivers both Ng4 attacks with the observed Rxd5 cost and Qxe4 recovery"):
    val (state, _, observations) = fixedJob
    val packet = state.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val refPath = prepared.paths.find(_.moves == reference).get
    val altPath = prepared.paths.find(_.moves == alternative).get
    val candidates = selectedBookPair(observations, Set(refPath.line.ref.id, altPath.line.ref.id))
    val text = candidates.map(candidate => (candidate \ "detail" \ "text").as[String]).mkString(" ")
    val lower = text.toLowerCase
    assert(text.contains("Ng4"), text)
    assert(lower.contains("white rook originally on e1 attacks the black knight originally on f6 on e4") ||
      lower.contains("white rook on e1 attacks black knight on e4") ||
      lower.contains("white rook on e1 attacks the black knight on e4"), text)
    assert(lower.contains("white knight originally on e3 attacks the black rook originally on e8 on e5") ||
      lower.contains("white knight on g4 attacks black rook on e5") ||
      lower.contains("white knight on g4 attacks the black rook on e5") ||
      lower.contains("white knight on g4 now targets black rook on e5"), text)
    assert(lower.contains("leaves e3 with ng4") || lower.contains("ng4 clears e3"), text)
    assert(text.contains("Rxd5") && text.contains("Qxe4"), text)
    assert(lower.contains("captures the initially attacked black knight on e4") ||
      lower.contains("captures black knight on e4"), text)
    assert(lower.contains("captures white pawn on d5") || lower.contains("captures the white pawn on d5") ||
      lower.contains("captures the pawn on d5"), text)
    val effects = candidates.flatMap(candidate => (candidate \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil))
    val opened = effects.filter(effect => (effect \ "attack_ply").asOpt[Int].contains(7) &&
      (effect \ "actor" \ "role").asOpt[String].contains("rook") &&
      (effect \ "actor" \ "square").asOpt[String].contains("e1") &&
      (effect \ "target" \ "role").asOpt[String].contains("knight") &&
      (effect \ "target" \ "square").asOpt[String].contains("f6"))
    assert(opened.nonEmpty, "Ng4's opened rook attack must retain its own history witness: " + text)
    opened.foreach { effect =>
      assertEquals((effect \ "line_ids").as[List[String]], List(refPath.line.ref.id))
      assertEquals((effect \ "through_plies").as[List[Int]], List(reference.size))
      val occurrences = (effect \ "occurrence_ids").as[List[String]]
      assert(occurrences.contains(refPath.ids(6)) && occurrences.contains(refPath.ids(8)))
      assert(occurrences.forall(refPath.ids.contains))
    }

  test("the same book comparison keeps the failed knight attack alongside the later defender departure"):
    val (state, _, observations) = fixedJob
    val prepared = ObservedLineProtocol.prepare(state.observationsPacket.get)
    val pair = prepared.paths.filter(p => p.moves == reference || p.moves == alternative).map(_.line.ref.id).toSet
    val candidates = selectedBookPair(observations, pair)
    val question = candidates.find(c => (c \ "detail" \ "type").as[String] == "question_comparison").get
    val text = (question \ "detail" \ "text").as[String]
    assert(text.contains("Nxe5: the White knight on e5") &&
      (text.contains("Rxe5: The captured white knight on e5 was the attacker.") ||
        text.contains("Rxe5 captures the original attacker White knight on e5.")), text)
    assert(text.contains("Black queen on d7; Black knight on g6"), text)
    assert(text.contains("Ng4") && text.contains("Rxd5") && text.contains("captures White pawn on d5") &&
      (text.contains("Qxe4: The captured black knight on e4 was the target.") ||
        text.contains("Qxe4: White queen on d4 captures Black knight on e4.")), text)
    val refPath = prepared.paths.find(_.moves == reference).get
    val originalKnight = Piece("white", "knight", "f3").witness
    val originalQueen = Piece("white", "queen", "d1").witness
    assertEquals(prepared.pieceAfter(state.observationsPacket.get, refPath, originalKnight, 5),
      Some(Some(Piece("white", "knight", "e5").witness)))
    assertEquals(prepared.pieceAfter(state.observationsPacket.get, refPath, originalKnight, 6), Some(None))
    assertEquals(prepared.pieceAfter(state.observationsPacket.get, refPath, enemyKnight.witness, 8),
      Some(Some(Piece("black", "knight", "e4").witness)))
    assertEquals(prepared.pieceAfter(state.observationsPacket.get, refPath, enemyKnight.witness, 9), Some(None))
    assertEquals(prepared.pieceAfter(state.observationsPacket.get, refPath, originalQueen, 8),
      Some(Some(Piece("white", "queen", "d4").witness)))
    assertEquals(prepared.pieceAfter(state.observationsPacket.get, refPath, originalQueen, 9),
      Some(Some(Piece("white", "queen", "e4").witness)))
    assert(text.length <= 4000, text.length.toString)
    val whole = candidates.map(c => (c \ "detail" \ "text").as[String]).mkString(" ")
    assert(whole.contains("At Qxe4, White's queen on e4 now targets Black's knight on g6"), whole)
    assert(whole.contains("White's queen on e4 adds support to White's knight on g4, which is under attack from Black's queen on d7"), whole)

  private def positionViews(start: String, moves: List[String]): List[PositionResourceView] =
    val owner = CanonicalPositionHistory.from(start, Nil, start).toOption.get
    assert(owner.extend(moves).isRight, moves.mkString(" "))
    val input = MoveReviewInputAdmission.admitIndependent(owner, moves.head,
      suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, moves))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val path = ObservedLineProtocol.prepare(packet).paths.find(_.moves == moves).get
    val resources = packet.resources.map(view => view.position -> view).toMap
    path.positions.map(resources)

  test("frozen ordinary capture safety can change across Kh8 without changing the actual turn or legal rows"):
    val views = positionViews("rn2nrk1/2qbbppp/3pp2B/p7/1p2PN2/1B3NQ1/PPP2PPP/3R1RK1 b - - 1 15",
      List("g8h8", "h6g5"))
    val actor = Piece("black", "pawn", "g7").witness
    val target = Piece("white", "bishop", "h6").witness
    val legalBefore = views.map(_.legalRows.map(_.map(_.move)))
    assertEquals(views(0).ordinaryCaptureKeepsKingSafe(actor, target), Some(false))
    assertEquals(views(1).ordinaryCaptureKeepsKingSafe(actor, target), Some(true))
    assertEquals(views(1).ordinaryCaptureKeepsKingSafe(actor, target), Some(true), "repeat the exact cached query")
    assertEquals(views(1).position.sideToMove, Some(chess.White))
    assert(!views(1).legalRows.toList.flatten.exists(_.move == "g7h6"), "the off-turn query is not a legal row")
    assertEquals(views(2).ordinaryCaptureKeepsKingSafe(actor, target), None, "Bg5 removed the old h6 target")
    assertEquals(views.map(_.legalRows.map(_.map(_.move))), legalBefore)

  test("Kh8 does not certify gxh6 when a second bishop still exposes the king after the pawn leaves"):
    val views = positionViews("rn2nrk1/2qbbppp/3ppB1B/p7/1p2PN2/5NQ1/PPP2PPP/3R1RK1 b - - 1 15",
      List("g8h8"))
    val actor = Piece("black", "pawn", "g7").witness
    val target = Piece("white", "bishop", "h6").witness
    assertEquals(views(0).ordinaryCaptureKeepsKingSafe(actor, target), Some(false))
    assertEquals(views(1).ordinaryCaptureKeepsKingSafe(actor, target), Some(false),
      "the f6 bishop attacks h8 after g7h6 even though the g-file pin ended")

  test("the frozen query excludes en passant, promotion, king actors and king targets"):
    val ep = positionViews("7k/8/8/3pP3/8/8/8/K7 w - d6 0 1", List("e5d6")).head
    val pawn = Piece("white", "pawn", "e5").witness
    assertEquals(ep.ordinaryCaptureKeepsKingSafe(pawn, Piece("black", "pawn", "d5").witness), None)
    assertEquals(ep.ordinaryCaptureKeepsKingSafe(pawn, Piece("black", "pawn", "d6").witness), None)
    val promotion = positionViews("4k2r/6P1/8/8/8/8/8/4K3 w - - 0 1", List("g7h8q"))
    assertEquals(promotion.head.ordinaryCaptureKeepsKingSafe(
      Piece("white", "pawn", "g7").witness, Piece("black", "rook", "h8").witness), None)
    assertEquals(promotion(1).ordinaryCaptureKeepsKingSafe(
      Piece("white", "queen", "h8").witness, Piece("black", "king", "e8").witness), None)
    val king = positionViews("7k/8/8/8/8/8/1p6/K7 w - - 0 1", List("a1b2")).head
    assertEquals(king.ordinaryCaptureKeepsKingSafe(
      Piece("white", "king", "a1").witness, Piece("black", "pawn", "b2").witness), None)

  // These equal-score, one-ply inputs inspect root explanation composition only.
  // They do not stand in for an issued engine answer or test move preference.
  private def directCaptureChoice(start: String):
      (ObservedLineProtocol.PreparedPaths, CommentarySelection.Result, JsObject) =
    val rows = List(List("d2d4"), List("c3d4"))
    val owner = CanonicalPositionHistory.from(start, Nil, start).toOption.get
    rows.foreach(moves => assert(owner.extend(moves).isRight, moves.mkString(" ")))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = start,
      playedMoveUci = rows.head.head, variations = rows.map(moves => EngineLine(moves, 0, None, 8)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    rows.foreach { moves =>
      val path = prepared.paths.find(_.moves == moves).get
      assertEquals(path.ids, path.declared)
      assertEquals(path.ids.size, 1)
      assert(path.certified.nonEmpty)
    }
    val projection = MoveWorkProjection.select(packet, prepared)
    val root = (projection.json \ "candidates").as[List[JsObject]].find(candidate =>
      (candidate \ "selection").as[String] == "selected" &&
        (candidate \ "detail" \ "type").asOpt[String].contains("root_comparison"))
      .getOrElse(fail("the directly admitted one-ply alternatives need a root comparison"))
    (prepared, projection, root)

  test("a capture choice preserves the second arm's opened attack on the queen"):
    val (prepared, _, root) = directCaptureChoice("6k1/8/8/8/1b1p4/2P5/3Q4/6K1 w - - 0 1")
    val queenPath = prepared.paths.find(_.moves == List("d2d4")).get
    val pawnPath = prepared.paths.find(_.moves == List("c3d4")).get
    val text = (root \ "detail" \ "text").as[String]
    val effects = (root \ "detail" \ "effects").as[List[JsObject]]
    val choice = effects.find(effect => (effect \ "scope").as[String] == "capture_choice")
      .getOrElse(fail(text))
    assertEquals((choice \ "line_ids").as[List[String]], List(queenPath.line.ref.id, pawnPath.line.ref.id))
    assertEquals((choice \ "through_plies").as[List[Int]], List(1, 1))
    assert(text.contains("Qxd4 captures the pawn on d4 with the queen"), text)
    assert(text.contains("cxd4 captures that same pawn with the c3-pawn instead"), text)
    assert(text.contains("After cxd4, Black bishop on b4 attacks White queen on d2"), text)
    val cost = effects.find(effect => (effect \ "scope").as[String] == "pair_difference" &&
      (effect \ "actor" \ "role").asOpt[String].contains("bishop") &&
      (effect \ "actor" \ "square").asOpt[String].contains("b4") &&
      (effect \ "target" \ "role").asOpt[String].contains("queen") &&
      (effect \ "target" \ "square").asOpt[String].contains("d2"))
      .getOrElse(fail("the second arm's queen exposure needs its own comparison witness: " + text))
    assertEquals((cost \ "line_ids").as[List[String]], List(pawnPath.line.ref.id, queenPath.line.ref.id))
    assertEquals((cost \ "through_plies").as[List[Int]], List(1, 1))
    assertEquals((cost \ "occurrence_ids").as[List[String]].toSet, (queenPath.ids ++ pawnPath.ids).toSet)
    assert((cost \ "evidence_ids").as[List[String]].nonEmpty)

  test("absorbing pawn support into a capture choice retains the rook attack and its evidence"):
    val (prepared, projection, root) = directCaptureChoice("3r2k1/8/8/8/1b1p4/2P5/3Q4/6K1 w - - 0 1")
    val queenPath = prepared.paths.find(_.moves == List("d2d4")).get
    val support = projection.local.find(note => note.edge.evidence.id == queenPath.ids.head &&
      (note.note.detail match
        case MoveWorkProjection.Detail.Support(actor, target, opposition, _) =>
          actor == Piece("white", "pawn", "c3").witness &&
            target == Piece("white", "queen", "d4").witness &&
            opposition.contains(Piece("black", "rook", "d8").witness)
        case _ => false)).getOrElse(fail("Qxd4 needs the observed c3 support and d8 opposition"))
    val text = (root \ "detail" \ "text").as[String]
    val choice = (root \ "detail" \ "effects").as[List[JsObject]]
      .find(effect => (effect \ "scope").as[String] == "capture_choice").getOrElse(fail(text))
    List("keeping the pawn on c3 to support it on d4", "under attack from Black's rook on d8")
      .foreach(required => assert(text.contains(required), text))
    val evidence = (choice \ "evidence_ids").as[List[String]].toSet
    assert(support.note.afterSources.nonEmpty, "the supported queen's attacker needs positive control evidence")
    assert(support.note.afterSources.toSet.subsetOf(evidence),
      "the absorbed support's opposition evidence must accompany the capture choice")
    assert(evidence(queenPath.ids.head))
    assert((choice \ "occurrence_ids").as[List[String]].contains(queenPath.ids.head))

  test("the exact book card shares its prior loss without losing the Ng4 reply and recovery"):
    val (state, _, observations) = fixedJob
    val prepared = ObservedLineProtocol.prepare(state.observationsPacket.get)
    val pair = prepared.paths.filter(p => p.moves == reference || p.moves == alternative).map(_.line.ref.id).toSet
    val candidates = selectedBookPair(observations, pair)
    assertEquals(candidates.map(c => (c \ "detail" \ "type").as[String]).toSet,
      Set("root_comparison", "question_comparison"))
    val root = candidates.find(c => (c \ "detail" \ "type").as[String] == "root_comparison").get
    val question = candidates.find(c => (c \ "detail" \ "type").as[String] == "question_comparison").get
    val rootText = (root \ "detail" \ "text").as[String]
    val text = (question \ "detail" \ "text").as[String]
    val priorLoss = "Bxe5 removes the White bishop on e5"
    assert(rootText.contains("Black bishop on g7 captures the initially attacked White bishop on e5"), rootText)
    assert(!text.contains(priorLoss), "the same card already identifies this exact captured bishop: " + text)
    assert(text.contains("Nxe5: the White knight on e5"), text)
    assert(text.contains("Black queen on d7; Black knight on g6"), text)
    List("Ng4", "Rxd5", "Qxe4").foreach(move => assert(text.contains(move), text))
    assert(text.contains("the White knight on e3 leaves e3 with Ng4"), text)
    assert(text.contains("moves the initially attacked Black rook on e5 out of White knight on g4's attack"), text)
    assert(text.contains("Black rook on e5 ends its support of Black knight on e4"), text)
    assert(text.contains("Black rook on e5 captures White pawn on d5"), text)
    assert(text.contains("This follows the end of Black rook on d5's support"), text)
    val card = rootText + " " + text
    List("Black's rook on d5 now targets White's queen on d4",
      "White queen on d4 moves out of Black rook on d5's attack").foreach { sharedResponse =>
      assertEquals(occurrencesOf(card, sharedResponse), 1, card)
    }
    assert(text.length <= 4000 && rootText.length <= 4000)

  test("the separate Kh1 question retains its own prior loss and Nxe5 introduction"):
    val (state, _, observations) = fixedJob
    val prepared = ObservedLineProtocol.prepare(state.observationsPacket.get)
    val refPath = prepared.paths.find(_.moves == reference).get
    val kh1Path = prepared.paths.find(_.moves == List("g1h1")).get
    val candidates = selectedBookPair(observations, Set(refPath.line.ref.id, kh1Path.line.ref.id))
    assertEquals(candidates.size, 1, "a shared Qxd4 PV does not give Kh1 the book pair's root body")
    val question = candidates.head
    assertEquals((question \ "detail" \ "type").as[String], "question_comparison")
    val text = (question \ "detail" \ "text").as[String]
    // This checks the independent explanation, not the filler move's chess value.
    assert(text.contains("Bxe5 removes the White bishop on e5"), text)
    assert(text.contains("Nxe5: the White knight on e5"), text)
    assert(text.contains("the White knight on e5 attacks the Black queen on d7 and the Black knight on g6"), text)
    assert(text.contains("Ng4:"), text)
    assert(text.contains("the White knight on e3 leaves e3 with Ng4"), text)
    val refs = (question \ "detail" \ "question_refs").as[List[JsObject]]
    assertEquals(refs.map(ref => (ref \ "requirement_id").as[String]).sorted,
      List("contrast_opponent_choice", "sacrifice_decline"))

  test("shared event prose preserves each question receipt and the four exact attack witnesses"):
    val (state, work, observations) = fixedJob
    val packet = state.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val refPath = prepared.paths.find(_.moves == reference).get
    val altPath = prepared.paths.find(_.moves == alternative).get
    val kh1Path = prepared.paths.find(_.moves == List("g1h1")).get
    val question = selectedBookPair(observations, Set(refPath.line.ref.id, altPath.line.ref.id))
      .find(c => (c \ "detail" \ "type").as[String] == "question_comparison").get
    val kh1 = selectedBookPair(observations, Set(refPath.line.ref.id, kh1Path.line.ref.id)).head
    def questionRefs(candidate: JsObject): List[(String, String)] =
      (candidate \ "detail" \ "question_refs").as[List[JsObject]].map(ref =>
        (ref \ "proposal_id").as[String] -> (ref \ "requirement_id").as[String])
    assertEquals(questionRefs(question).map(_._2).sorted,
      List("contrast_opponent_choice", "sacrifice_value", "sacrifice_value"))
    List(question, kh1).foreach { candidate =>
      val detail = (candidate \ "detail").as[JsObject]
      questionRefs(candidate).foreach { (proposalId, requirementId) =>
        val entry = state.proposalEntries.find(_.request.id == proposalId).get
        val comparison = entry.request.requirements.collectFirst {
          case value: CompareMoves if value.id == requirementId => value
        }.get
        val answer = entry.readings.find(_.requirementId == requirementId).flatMap(_.answer).get
        assertEquals(comparison.prefixMovesUci, Nil)
        assertEquals(comparison.referenceMove, (detail \ "reference_move").as[String])
        assertEquals(comparison.alternativeMove, (detail \ "alternative_move").as[String])
        assertEquals(Set(answer.referenceLineId, answer.alternativeLineId), (candidate \ "line_ids").as[List[String]].toSet)
        assertEquals(answer.workId, work.workId)
        assertEquals(answer.executionKeySha256, work.executionKeySha256)
        assertEquals(answer.requiredDepth, work.requiredDepth)
      }
    }
    val (entry, _, reading, comparison, _) = currentCause(state.proposalEntries)
    assert(questionRefs(question).contains(entry.request.id -> comparison.id))
    assertEquals(reading.state, "supported")
    val effects = (question \ "detail" \ "effects").as[List[JsObject]]
    val firstAttacker = Piece("white", "knight", "f3")
    val queen = Piece("black", "queen", "d7")
    val survivingKnight = Piece("black", "knight", "g6")
    def piece(value: JsLookupResult): Piece =
      Piece((value \ "side").as[String], (value \ "role").as[String], (value \ "square").as[String])
    List((5, firstAttacker, queen, 6), (5, firstAttacker, survivingKnight, 6),
      (7, knight, enemyRook, 8), (7, rook, enemyKnight, 9)).foreach { (ply, actor, target, responsePly) =>
      val effect = effects.find(effect => (effect \ "scope").as[String] == "line_history" &&
        (effect \ "attack_ply").asOpt[Int].contains(ply) &&
        piece(effect \ "actor") == actor && piece(effect \ "target") == target)
        .getOrElse(fail(s"missing independently owned attack at ply $ply: $actor -> $target"))
      assertEquals((effect \ "prefix_moves_uci").as[List[String]], Nil)
      assertEquals((effect \ "line_ids").as[List[String]], List(refPath.line.ref.id))
      assertEquals((effect \ "through_plies").as[List[Int]], List(reference.size))
      val occurrences = (effect \ "occurrence_ids").as[List[String]].toSet
      assert(occurrences(refPath.ids(ply - 1)) && occurrences(refPath.ids(responsePly - 1)))
      assert(occurrences.subsetOf(refPath.ids.toSet))
      assert(occurrences.subsetOf((effect \ "evidence_ids").as[List[String]].toSet))
    }
    assertEquals(prepared.pieceAfter(packet, refPath, firstAttacker.witness, 6), Some(None))
    List(queen, survivingKnight).foreach { original =>
      assertEquals(prepared.pieceAfter(packet, refPath, original.witness, reference.size), Some(Some(original.witness)))
    }

  private lazy val typedBookComposition =
    val (state, _, _) = fixedJob
    val packet = state.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val pair = prepared.paths.filter(p => p.moves == reference || p.moves == alternative).map(_.line.ref.id).toSet
    val drafts = scala.collection.mutable.ListBuffer.empty[(JsObject, List[MoveWorkProjection.Context])]
    CommentaryProposal.comparisonCandidates(packet, prepared, state.proposalEntries,
      Some((candidate, contexts) =>
        if (candidate \ "selection").as[String] == "selected" &&
            (candidate \ "line_ids").as[List[String]].toSet == pair &&
            (candidate \ "detail" \ "prefix_moves_uci").as[List[String]].isEmpty then
          drafts += ((candidate, contexts))
        candidate))
    assertEquals(drafts.size, 1)
    val (candidate, contexts) = drafts.head
    val projection = MoveWorkProjection.select(packet, prepared)
    val baseline = projection.withMoveEffects(candidate)
    val explained = (baseline \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil).toSet
    (prepared, projection, candidate, contexts, explained)

  private def revisedContext(context: MoveWorkProjection.Context, tactic: TacticStep): MoveWorkProjection.Context =
    context.copy(tactic = Some(tactic), attack = context.attack.map(_.copy(moves = tactic.line,
        contacts = tactic.contacts.map((actor, target, _) => actor -> target), cause = Some(tactic.actor))))

  private def composedContexts(contexts: List[MoveWorkProjection.Context]): JsObject =
    val (_, projection, candidate, _, explained) = typedBookComposition
    projection.withMoveEffects(candidate, explained = explained, context = contexts)

  // Keep prose-only delivery checks until admitted contexts expose their clauses:
  // published effect IDs alone cannot prove that a reply or qualification is visible.
  private def occurrencesOf(text: String, phrase: String): Int = text.sliding(phrase.length).count(_ == phrase)

  test("a question's owned attack keeps its observed reply and recovery within the field limit"):
    val (_, projection, candidate, contexts, _) = typedBookComposition
    val current = contexts.find(_.tactic.exists(_.line == reference.take(7))).get
    // Reserve part of the unchanged wire limit for the question's existing answer.
    // The book continuation and every physical observation stay unchanged.
    val detail = (candidate \ "detail").as[JsObject] - "effects"
    val reserved = (detail \ "text").as[String].padTo(1600, 'x')
    val constrained = candidate + ("detail" -> (detail + ("text" -> JsString(reserved))))
    val result = projection.withMoveEffects(constrained, context = List(current))
    val text = (result \ "detail" \ "text").as[String]
    List("Ng4", "moves the initially attacked Black rook on e5 out of White knight on g4's attack",
      "Black rook on e5 captures White pawn on d5", "Black rook on e5 ends its support of Black knight on e4",
      "Qxe4:", "This follows the end of Black rook on d5's support").foreach(phrase =>
      assert(text.contains(phrase), text.drop(reserved.length)))
    assert(text.length <= 4000)
    assert(text.contains(reserved), "the engine answer remains intact")

  test("same-line context cannot gain answer priority from a different question or comparison"):
    val (_, projection, candidate, contexts, _) = typedBookComposition
    val current = contexts.find(_.tactic.exists(_.line == reference.take(7))).get
    val tactic = current.tactic.get
    val (question, lineId) = tactic.owner.get
    val comparison = question.comparison
    val detail = (candidate \ "detail").as[JsObject] - "effects"
    val reserved = (detail \ "text").as[String].padTo(1600, 'x')
    val constrained = candidate + ("detail" -> (detail + ("text" -> JsString(reserved))))
    val otherOwners = List(
      question.copy(proposalId = question.proposalId + "-different"),
      question.copy(comparison = comparison.copy(id = comparison.id + "-different")),
      question.copy(comparison = comparison.copy(referenceMove = comparison.alternativeMove,
        alternativeMove = comparison.referenceMove)),
      question.copy(answer = question.answer.copy(referenceOccurrenceId = question.answer.alternativeOccurrenceId)),
      question.copy(answer = question.answer.copy(evaluationRelation = "better")))
    val invalid = otherOwners.map(owner => revisedContext(current, tactic.copy(owner = Some(owner -> lineId))))
    invalid.foreach { context =>
      val result = projection.withMoveEffects(constrained, context = List(context))
      val text = (result \ "detail" \ "text").as[String]
      assert(!text.contains("Ng4:"), "a same-line fact without this question's ownership must not displace its existing explanations: " + text.drop(reserved.length))
      assert(text.contains(reserved) && text.length <= 4000)
    }

  test("context wording cannot change the bound question, physical replies or published evidence"):
    val current = typedBookComposition._4.find(_.tactic.exists(_.line == reference.take(7))).get
    val original = composedContexts(List(current))
    List("This wording describes the same selected knight attack.", current.text.replace("the ", "that ")).foreach { wording =>
      val revised = composedContexts(List(current.copy(text = wording)))
      assertEquals((revised \ "detail" \ "question_refs"), (original \ "detail" \ "question_refs"))
      assertEquals((revised \ "detail" \ "effects").as[List[JsObject]].toSet,
        (original \ "detail" \ "effects").as[List[JsObject]].toSet)
      assertEquals((revised \ "evidence_ids").as[List[String]].toSet, (original \ "evidence_ids").as[List[String]].toSet)
      val text = (revised \ "detail" \ "text").as[String]
      List("Ng4", "Rxd5", "Qxe4").foreach(move => assert(text.contains(move), text))
    }

  test("partial then complete Nxe5 contexts share only the common contact"):
    val original = typedBookComposition._4.find(_.tactic.exists(_.line == reference.take(5))).get
    val tactic = original.tactic.get
    assertEquals(tactic.contacts.size, 2)
    val partial = revisedContext(original, tactic.copy(contacts = tactic.contacts.take(1)))
    val result = composedContexts(List(partial, original))
    val text = (result \ "detail" \ "text").as[String]
    List("the White knight on e5 attacks the Black queen on d7",
      "the White knight on e5 attacks the Black knight on g6").foreach { contact =>
      assertEquals(occurrencesOf(text, contact), 1, text)
    }
    val effects = (result \ "detail" \ "effects").as[List[JsObject]].filter(effect =>
      (effect \ "attack_ply").asOpt[Int].contains(5))
    assert(Set("d7", "g6").subsetOf(effects.map(effect => (effect \ "target" \ "square").as[String]).toSet))

  test("later loss and departure clauses preserve their complete contact relationships"):
    val original = typedBookComposition._4.find(_.tactic.exists(_.line == reference.take(5))).get
    assert(original.tactic.get.loss.nonEmpty)
    val withoutLoss = revisedContext(original, original.tactic.get.copy(loss = None))
    val lossText = (composedContexts(List(withoutLoss, original)) \ "detail" \ "text").as[String]
    assert(lossText.contains("Bxe5 removes the White bishop on e5"), lossText)
    val opened = typedBookComposition._4.find(_.tactic.exists(_.line == alternative.take(3))).get
    val tactic = opened.tactic.get
    assertEquals(tactic.contacts.size, 2)
    assert(tactic.departures.nonEmpty)
    val partial = revisedContext(opened, tactic.copy(contacts = tactic.contacts.take(1), departures = Nil))
    val text = (composedContexts(List(partial, opened)) \ "detail" \ "text").as[String]
    val contact = "the White rook on e1 attacks the Black rook on e8"
    val reciprocal = "the Black rook on e8 attacks the White rook on e1"
    val mutual = List("the White rook on e1 and the Black rook on e8 attack each other",
      "the Black rook on e8 and the White rook on e1 attack each other").find(text.contains).getOrElse(fail(text))
    // The prior loss was already retained by the first context; the new line
    // condition must retain both contacts without repeating that separate loss.
    val complete = text.split("\n\n").find(_.contains("The line opens after")).get
    assert(complete.contains(mutual),
      "the new departure must stay attached to both original contacts: " + text)
    assertEquals(occurrencesOf(text, "Nxd5 removes the White pawn on d5"), 1, text)
    val firstContact = tactic.contacts.head
    val repeated = if firstContact._1 == rook then contact else reciprocal
    assertEquals(occurrencesOf(text, repeated), 1, text)
    assertEquals(occurrencesOf(text, mutual), 1, text)

  test("a different receipt or full prefix cannot consume the same attack introduction"):
    val original = typedBookComposition._4.find(_.tactic.exists(_.line == reference.take(5))).get
    val tactic = original.tactic.get
    val (question, lineId) = tactic.owner.get
    val receipt = question.answer
    val otherReceipt = revisedContext(original, tactic.copy(owner =
      Some((question.copy(answer = receipt.copy(workId = receipt.workId + "-different-owner")), lineId))))
    val otherPrefix = revisedContext(original, tactic.copy(line = alternative.head :: tactic.line.tail))
    val contact = "the white knight originally on f3 attacks the black queen originally on d7 on d7"
    List(otherReceipt -> 2, otherPrefix -> 1).foreach { (different, resolved) =>
      val text = (composedContexts(List(original, different)) \ "detail" \ "text").as[String]
      assertEquals(occurrencesOf(text, contact), 2 - resolved,
        "a mismatched prefix keeps its original wording: " + text)
      assertEquals(occurrencesOf(text, "the White knight on e5 attacks the Black queen on d7"), resolved,
        "another receipt cannot consume the independently owned contact: " + text)
    }

  test("an explicit additional condition survives sharing of the typed attack"):
    val original = typedBookComposition._4.find(_.tactic.exists(_.line == reference.take(5))).get
    val later = typedBookComposition._4.find(_.tactic.exists(_.line == reference.take(7))).get
    List("This separate condition must also remain.", later.text).foreach { additional =>
      val qualified = original.copy(additional = List(additional))
      val result = composedContexts(List(qualified, original))
      val text = (result \ "detail" \ "text").as[String]
      assert(text.contains(additional), "sharing the contact cannot discard a separate required condition: " + text)
      assert(text.contains("Nxe5") && text.contains("Black queen on d7"), text)
    }

  test("a context rejected by the real text limit cannot consume the following complete context"):
    val original = typedBookComposition._4.find(_.tactic.exists(_.line == reference.take(5))).get
    val (_, _, observations) = fixedJob
    val candidate = typedBookComposition._3
    val card = (observations \ "selection" \ "candidates").as[List[JsObject]].filter(value =>
      Set("root_comparison", "question_comparison")((value \ "detail" \ "type").as[String]) &&
        (value \ "line_ids").as[List[String]].toSet == (candidate \ "line_ids").as[List[String]].toSet)
    val oversized = original.copy(additional = List(card.map(value => (value \ "detail" \ "text").as[String]).mkString(" ")))
    val baseline = composedContexts(Nil)
    assert(oversized.additional.mkString.length > 4000, "use the actual card's prose to exceed one field even before optional supplements")
    assertEquals(composedContexts(List(oversized)), baseline)
    val complete = composedContexts(List(original))
    assert((complete \ "detail" \ "text").as[String].contains("Nxe5"))
    assertEquals(composedContexts(List(oversized, original)), complete,
      "the omitted predecessor must leave neither missing clauses nor orphaned effect ownership")

  test("co-displayed questions share wording while independently composed questions keep their own explanation"):
    val (_, projection, candidate, contexts, explained) = typedBookComposition
    val original = contexts.find(_.tactic.exists(_.line == reference.take(5))).get
    val root = (projection.baseJson \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison" &&
        (value \ "line_ids").as[List[String]].toSet == (candidate \ "line_ids").as[List[String]].toSet).get
    // Both recipients retain the actual owned question refs. Distinct IDs exercise
    // the compositor's card boundary without generating another report or PV.
    val second = candidate + ("id" -> JsString((candidate \ "id").as[String] + ":second-recipient"))
    val together = projection.withSharedMoveEffects(List(root -> Nil,
      candidate -> List(original), second -> List(original)))
    assertEquals(together.size, 3)
    val sharedText = together.map(value => (value \ "detail" \ "text").as[String]).mkString(" ")
    val contact = "the White knight on e5 attacks the Black queen on d7"
    assertEquals(occurrencesOf(sharedText, contact), 1, sharedText)
    together.tail.foreach { value =>
      assertEquals(value \ "detail" \ "question_refs", candidate \ "detail" \ "question_refs")
      assert((value \ "detail" \ "text").as[String].contains((candidate \ "detail" \ "text").as[String]))
      assert((value \ "detail" \ "text").as[String].contains("do not by themselves establish a forced gain"))
    }
    val separate = List(candidate, second).map(value =>
      projection.withMoveEffects(value, explained, List(original)))
    separate.foreach { value =>
      val text = (value \ "detail" \ "text").as[String]
      assertEquals(occurrencesOf(text, contact), 1, text)
      assert(text.contains("Bxe5 removes the White bishop on e5"), text)
    }

  test("a common-position answer survives a full detailed record"):
    // Mishra–Ivanchuk, Grand Swiss 2023, before 15...exd5; recorded depth-19 lines.
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(
      fen = "r2qk2r/1b2bp2/p3pp2/3B3p/8/2N1Q3/PPP2PPP/2KRR3 b kq - 0 15",
      playedMoveUci = "e6d5",
      variations = List(
        "e6d5 d1d5 b7d5 c3d5 e8f8 d5e7 d8a5 e7c6 a5a2 e3f4 a2a1 c1d2 a1b2 e1e3 h5h4 e3b3 b2a1 c6e7",
        "b7d5 d1d5 e6d5 c3d5 e8f8 d5e7 d8a5 e7c6 a5a2 e3e7 f8g7 e1e3 h8e8 e3g3 g7h7 e7f6 a2a1 c1d2 a8d8"
      ).map(moves => EngineLine(moves.split(" ").toList, 0, None, 19)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val root = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    val summary = completePairSummary(root, paths, "selected_comparison")
    val detail = (root \ "detail").as[JsObject]
    val answer = (detail \ "summary").as[String]
    assertConvergenceDelivery(root, summary, paths, meeting = 4, divergence = 9)
    List("exd5", "Rxd5", "Bxd5", "Nxd5", "Qf4", "Qe7+")
      .foreach(required => assert(answer.contains(required), answer))
    val detailed = detailRefs(summary).toSet
    (root \ "line_ids").as[List[String]].foreach { id =>
      val path = paths.paths.find(_.line.ref.id == id).get
      val events = paths.continuationReadings.find(_.path eq path).get.events
      List((5, Piece("white", "knight", "c3"), Piece("black", "bishop", "e7")),
        (8, Piece("black", "queen", "d8"), Piece("white", "pawn", "a2"))).foreach { (offset, actor, victim) =>
        val event = events.find(_.plyOffset == offset).get
        assertEquals(event.originalActor, Some(actor.witness))
        assertEquals(event.originalVictim, Some(victim.witness))
        assert(detailed(RootQuestionSummary.MoveRef(path, offset).json),
          "the shared continuation keeps each original capture in the independent detail")
      }
    }
    val full = (detail \ "text").as[String]
    List("White knight recaptures the black pawn on d5 with Nxd5",
      "White knight recaptures the black bishop on d5 with Nxd5")
      .foreach(required => assert(full.contains(required), full))
    assert(full.length <= 4000)

  test("a certified move-order answer keeps convergence and the later divergence together"):
    // Ding–Ju Wenjun, Tata Steel Masters 2024, after 21...Kf8; recorded depth-21 lines.
    val start = "3rrk2/1pq2p2/2pb1npp/p7/P1B4P/2P2NP1/1PQ2PK1/4RR2 w - - 0 22"
    val lines = List(
      "c2d2 f8g7 e1e8 d8e8 f1e1 e8e1 d2e1 c7d7 c4b3 d7e8 e1e8 f6e8 f3e1 g7f6 e1d3 f6e7 f2f4 f7f5 b3d1 e8f6 d1b3",
      "e1e8 d8e8 c2d2 f8g7 f1e1 e8e1 d2e1 c7d7 c4b3 d7e8 e1e8 f6e8 f3d2 d6c7 f2f4 f7f5 d2f3 c7b8 f3d2 b8c7")
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = start, playedMoveUci = "e1e8",
      variations = lines.map(moves => EngineLine(moves.split(" ").toList, 0, None, 21)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val root = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    val summary = completePairSummary(root, paths, "selected_comparison")
    val detail = (root \ "detail").as[JsObject]
    val answer = (detail \ "summary").as[String]
    assertConvergenceDelivery(root, summary, paths, meeting = 6, divergence = 12)
    List("Qxe1", "Ne1", "Nd2")
      .foreach(required => assert(answer.contains(required), answer))
    val detailed = detailRefs(summary).toSet
    (root \ "line_ids").as[List[String]].foreach { id =>
      val path = paths.paths.find(_.line.ref.id == id).get
      assertEquals(assertPhysicalRecapture(paths, path, List(10, 11)),
        Set(Piece("black", "queen", "c7").witness, Piece("white", "queen", "c2").witness))
      List(10, 11).foreach(offset => assert(detailed(RootQuestionSummary.MoveRef(path, offset).json),
        "the shared queen exchange remains whole in the independent detail"))
    }
    assert((detail \ "text").as[String].length > answer.length, "the longer record remains available")

  test("a common continuation states when only one supplied record continues"):
    // Abdusattorov–Vachier-Lagrave, Sinquefield Cup 2025, before 15...a5; measured depth-16 reports.
    val start = "1r3k1r/p2qbpp1/1pnp1n1p/2pNp3/P1P1P3/3P1NPb/1P1B1PBP/R2Q1RK1 b - - 3 15"
    val rows = List(
      "h3g4 d5e3 a7a5 d1e1 g7g5 g1h1 g4e6 f3g1 f6g4 e3g4 e6g4 f2f3 g4e6 f3f4 f7f6 f4g5 h6g5 d2g5 f8g7 g5d2" -> 45,
      "a7a5 d5e3 h3g4 d1e1 g7g5 g1h1 g4e6 f3g1 f6g4 e3g4 e6g4 f2f3 g4e6 f3f4 f7f6 f4g5 h6g5 d2g5 f8g7 g5d2 c6d4 e1f2 d4c2" -> 47)
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = start, playedMoveUci = "a7a5",
      variations = rows.map((moves, cp) => EngineLine(moves.split(" ").toList, cp, None, 16)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val root = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    val summary = completePairSummary(root, paths, "selected_comparison")
    val detail = (root \ "detail").as[JsObject]
    val answer = (detail \ "summary").as[String]
    val pair = rows.map((moves, _) => paths.paths.find(_.moves == moves.split(" ").toList).get)
    assertEquals(pair.head.moves.drop(3), pair.last.moves.drop(3).take(pair.head.moves.size - 3))
    val next = pair.head.moves.size
    assertEquals(pair.last.moves(next), "c6d4")
    val visible = (summary \ "units").as[List[JsObject]].flatMap(unit => refs(unit("en"))).toSet
    assert(visible(RootQuestionSummary.MoveRef(pair.last, next).json))
    assert(answer.contains("Bg4 line ends") && answer.contains("Nd4"),
      "the default reading must distinguish an ended record from an observed different reply: " + answer)

  test("removing an attacker retains the price of the defending capture"):
    // Vidit–Nepomniachtchi, Tata Steel Masters 2024, after 18.Rac1.
    // The recorded Stockfish 18 depth-17 lines expose Bxe5 dxe5 in the Qd7 alternative.
    val start = "2rq1rk1/p1n2ppp/1ppb4/8/3P4/PQ2BN2/1P3PPP/2R1R1K1 b - - 1 18"
    val lines = List(
      "c7d5 f3e5 d5e3 e1e3 c6c5 d4c5 c8c5 c1c5 d6c5 e3e2 d8c7 b3f3 f8d8" -> -3,
      "d8d7 f3e5 d6e5 d4e5 h7h6 b3a4 c7d5 c1d1 d7e6 f2f4 d5e3 e1e3 f8d8 h2h3 d8d1 a4d1 c8e8 e3e1 e6f5" -> 7)
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = start, playedMoveUci = "c7d5",
      variations = lines.map((moves, cp) => EngineLine(moves.split(" ").toList, cp, None, 17)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val detail = (MoveWorkProjection.select(packet).json \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get.apply("detail").as[JsObject]
    val text = (detail \ "text").as[String]
    assert(text.contains("Bxe5 captures the original attacker White knight on e5"), text)
    assert(text.contains("dxe5 recaptures Black bishop on e5"),
      "saving the queen by taking the knight must retain the immediate loss of that bishop: " + text)

  test("root summaries preserve admitted priorities and concrete material outcomes"):
    // Fixed legal continuations and controlled scores test composition, not move quality.
    def detail(start: String, continuations: List[String], mates: List[Int] = Nil)
        : (EvidenceBackedJudgmentPacket, ObservedLineProtocol.PreparedPaths, JsObject) =
      val rows = continuations.map(_.split(" ").toList)
      val (packet, paths, root) = projectRoot(start, rows.head.head,
        rows.zipWithIndex.map((moves, index) => EngineLine(moves, 0, mates.lift(index), 8)))
      val detail = (root \ "detail").as[JsObject]
      (detail \ "summary_question" \ "question").asOpt[String].foreach { question =>
        if Set("selected_comparison", "observed_terminal_comparison", "observed_exchange")(question) then
          completePairSummary(root, paths, question)
      }
      sys.props.get("chesstory.reference-output").foreach { directory =>
        val out = java.nio.file.Path.of(directory)
        java.nio.file.Files.createDirectories(out)
        java.nio.file.Files.writeString(out.resolve("priority-" + rows.head.head + ".json"), Json.stringify(root))
      }
      (packet, paths, detail)

    val (_, _, capture) = detail("4k3/8/8/3q4/4P3/8/8/4K3 w - - 0 31", List(
      "e4d5 e8e7 e1d2 e7d6 d2c3 d6d5", "e1e2 d5e4 e2d2 e8e7 d2c3 e7d6 c3b3 e4e2 b3b4 d6d5 b4b3 e2d2 b3a3 d5c6"))
    val captureSummary = (capture \ "summary").as[String]
    assert((capture \ "text").as[String].contains("passed pawn"), "the supplementary fact remains in detail")
    assert(!captureSummary.contains("passed pawn"), captureSummary)
    List("exd5", "Kxd5", "Qxe4+", "Kd2", "one pawn", "one queen", "drawn")
      .foreach(value => assert(captureSummary.contains(value), captureSummary))

    val (_, exchangePaths, exchange) = detail("r1bqk2r/pppp1ppp/2n2n2/8/1b1NP3/2N5/PPP2PPP/R1BQKB1R w KQkq - 3 6", List(
      "d4c6 b7c6 f1d3 d7d5 e4d5 e8g8 e1g1 c6d5 d1f3 c8e6 h2h3 c7c5 c1g5 h7h6 g5h4 c5c4 d3e2 f8e8 a1d1 b4c3 f3c3",
      "f2f3 e8g8 d4c6 b7c6 f1d3 f8e8 e1g1 d7d5 c1g5 d8d6 g5h4 b4c3 b2c3 d5e4 f3e4 f6g4 h4g3 d6c5 g1h1 g4e3 g3f2 e3d1 f2c5 d1c3 a1e1"))
    val exchangeSummary = (exchange \ "summary").as[String]
    // Evaluate both delivery regressions before assertions so the opt-in test
    // projections retain each selected answer even when the first one fails.
    val (supportPacket, supportPaths, support) = detail("rn1qkbnr/pp3ppp/4p3/2ppPb2/3P4/5N2/PPP1BPPP/RNBQK2R w KQkq - 0 6", List(
      "c2c3 b8c6 e1g1 f5g6 c1e3 d8b6 b1d2 g8h6 f3h4 h6f5 h4f5 g6f5 g2g4 f5g6 a1c1 c5d4 c3d4 b6b2 f2f4 f8a3 d1a4 b2b4",
      "c1e3 d8b6 c2c4 b6b2 b1d2 g8e7 e1g1 b8c6 d2b3 d5c4 e2c4 a8d8 d1c1 b2c1 f1c1 e7d5"))
    assert((exchange \ "effects").as[List[JsObject]].exists(value => (value \ "scope").as[String] == "pawn_option"))
    assert((exchange \ "text").as[String].contains("route to f4"))
    val exchangeText = (exchange \ "text").as[String]
    assert(exchangeText.contains("Black knight on e3 captures White queen on d1."), exchangeText)
    assert(exchangeText.contains("The black knight originally on f6 remains on c3 at this line's end."), exchangeText)
    assert(!exchangeSummary.contains("route to f4"), exchangeSummary)
    assert(exchangeSummary.contains("maintains support"), exchangeSummary)
    List("fxe4", "Nxd1", "Bxc5").foreach(value => assert(exchangeSummary.contains(value), exchangeSummary))
    val f3 = exchangePaths.paths.find(_.moves.headOption.contains("f2f3")).get
    val f3Reading = exchangePaths.continuationReadings.find(_.path eq f3).get
    val exchangeQuestion = (exchange \ "summary_question").as[JsObject]
    val exchangeCore = coreUnits(exchangeQuestion).flatMap(unit => refs(unit("en"))).toSet
    val exchangeCovered = coreUnits(exchangeQuestion).flatMap(unit => refs(unit("covered_moves"))).toSet
    List(0, 11, 12, 13, 14, 20, 21, 22).foreach(offset =>
      assert(exchangeCore(RootQuestionSummary.MoveRef(f3, offset).json),
        "the support, its actual recapture and both capturing evasions stay in the core"))
    List((21, Piece("black", "knight", "f6"), Piece("white", "queen", "d1")),
      (22, Piece("white", "bishop", "c1"), Piece("black", "queen", "d8"))).foreach { (offset, actor, victim) =>
      val event = f3Reading.events.find(_.plyOffset == offset).get
      assertEquals(event.originalActor, Some(actor.witness))
      assertEquals(event.originalVictim, Some(victim.witness))
      assert(exchangeCovered(RootQuestionSummary.MoveRef(f3, offset).json))
    }
    // This later pawn is not the e4 support target, and its capture is neither
    // a recapture nor a check evasion after Bxc5. Core claims no whole-line total.
    val later = f3Reading.events.find(_.plyOffset == 23).get
    assertEquals(later.originalActor, Some(Piece("black", "knight", "f6").witness))
    assertEquals(later.originalVictim, Some(Piece("white", "pawn", "b2").witness))
    assertEquals(f3Reading.events.find(_.plyOffset == 13).get.originalVictim,
      Some(Piece("white", "pawn", "e4").witness))
    val f3Fact = f3.certified.get._2
    assert(!f3Fact.material.get.captures.find(_.plyOffset == 23).get.recapture)
    assert(!f3Fact.lineEvents.exists(event => event.kind == LineEventKind.Check && event.plyOffset == 22))
    val laterRef = RootQuestionSummary.MoveRef(f3, 23).json
    assert(detailRefs(exchangeQuestion).contains(laterRef))
    assert(detailUnits(exchangeQuestion).flatMap(unit => refs(unit("covered_moves"))).contains(laterRef))
    val laterWords = detailUnits(exchangeQuestion).flatMap(unit => unit("en").as[List[JsValue]].sliding(2).collect {
      case List(move: JsObject, JsString(words)) if move == laterRef => words
    })
    assert(laterWords.exists(_.contains("captures the white pawn on c3")),
      "the independent later capture is still actually explained in detail")

    List((support \ "text").as[String], (support \ "summary").as[String]).foreach { reading =>
      assert(reading.contains("cxd4 by the supporting pawn on c3 actually recaptures its capturer"), reading)
      assert(reading.contains("pawn on d4 adds support to White's pawn on e5"), reading)
      assert(reading.contains("queen on a4 now targets"), reading)
      List("bishop on a3", "knight on c6", "pawn on a7").foreach(target =>
        assert(reading.contains(s"Black's $target"), reading))
    }
    val supportEffects = (support \ "effects").as[List[JsObject]]
    def pawnEffect(scope: String, target: String): JsObject = supportEffects.find(value =>
      (value \ "scope").as[String] == scope &&
        (value \ "actor" \ "square").asOpt[String].contains("c3") &&
        (value \ "actor" \ "role").asOpt[String].contains("pawn") &&
        (value \ "target" \ "square").asOpt[String].contains(target))
      .getOrElse(fail(s"the same c3-pawn needs its selected $scope proof for $target"))
    val supportUse = pawnEffect("support_use", "d4")
    val route = pawnEffect("piece_route", "e5")
    assertEquals(route \ "line_ids", supportUse \ "line_ids")
    assertEquals((route \ "through_plies").as[List[Int]], List(17))
    assertEquals(route \ "through_plies", supportUse \ "through_plies")
    val visible = coreUnits((support \ "summary_question").as[JsObject])
      .flatMap(unit => refs(unit("en"))).map(ref => (ref \ "occurrence_id").as[String]).toSet
    // SupportUse already explains the pawn's arrival. Its route introduction may
    // be absorbed in prose, but the route proof and the new e5 duty must survive.
    val owned = (route \ "occurrence_ids").as[List[String]]
    List(0, 15, 16).foreach(offset => assert(visible(owned(offset)),
      "the original support, target capture and actual recapture stay in the core"))
    val supportPath = supportPaths.paths.find(_.line.ref.id == (route \ "line_ids").as[List[String]].head).get
    val queenRef = RootQuestionSummary.MoveRef(supportPath, 20)
    assertEquals(supportPath.moves(20), "d1a4")
    val targets = Set(Piece("black", "bishop", "a3").witness,
      Piece("black", "knight", "c6").witness, Piece("black", "pawn", "a7").witness)
    val actualTargets = MoveWorkProjection.select(supportPacket, supportPaths).local.collect {
      case note if note.edge.evidence.id == supportPath.ids(20) => note.note.detail
    }.collect {
      case MoveWorkProjection.Detail.Attack(actor, target, true)
          if actor == Piece("white", "queen", "a4").witness => target
    }.toSet
    assertEquals(actualTargets, targets, "all grouped targets belong to the same actual queen move")
    val queenWords = coreUnits((support \ "summary_question").as[JsObject]).flatMap(unit =>
      unit("en").as[List[JsValue]].sliding(2).collect {
        case List(move: JsObject, JsString(words)) if move == queenRef.json => words
      })
    assert(queenWords.exists(words => words.contains("queen on a4") &&
      List("bishop on a3", "knight on c6", "pawn on a7").forall(words.contains)),
      "the grouped core sentence keeps the actor and all targets at that exact occurrence")

    val (_, _, castle) = detail("4k3/8/8/8/8/8/8/4K2R w K - 0 1", List("e1g1", "e1e2"))
    val castleText = (castle \ "text").as[String]
    val reading = (castle \ "summary").asOpt[String].getOrElse(castleText)
    assert(reading.contains("O-O castles, bringing the rook from h1 to f1."), reading)
    assert(reading.length <= 4000, reading)

    val (_, _, promotion) = detail("8/6P1/5K2/8/7k/8/8/8 w - - 0 51", List(
      "g7g8q h4h3 f6f5 h3h2 f5e4 h2h3 e4f3 h3h2 g8g2",
      "f6f5 h4g3 f5e4 g3g2 e4e3 g2g3 g7g8q g3h3 e3e4 h3h2 e4f4 h2h1 f4f3 h1h2 g8g2"), List(5, 8))
    assert((promotion \ "summary").as[String].contains("g8=Q promotes to a queen"),
      "the default reading must preserve the actual promotion")


  test("master lines bind an immediate exchange and retained pieces to exact occurrences"):
    // Tata Steel Masters 2023, before 17...Bf5 and 14.Qd6; saved Stockfish 18 reports.
    // Shortened prefixes below test refusal to hide an unfinished capture, not move quality.
    val examples = List(
      ("r1bq2k1/pp3pnp/2p2bp1/3p4/3P1B2/2PB4/PP1NQPPP/4R1K1 b - - 1 17", "bishop", "knight", 12, List(
        ("c8f5 d3f5 g7f5 d2b3 f5g7 b3c5 b7b6 c5d3 d8d7 e2f3 a8e8 e1e8 g7e8 h2h3 e8d6 g2g4 f6e7 a2a4 e7f8 f3e2 f7f6 d3b4 d6c4 b2b3", 12, 21),
        ("c8d7 d2b3 a7a5 f4h6 g7e6 b3d2 d8b6 d2f3 a8e8 f3e5", 16, 18))),
      ("r4rk1/pp2q1pp/2n1pn2/2p5/4P3/2N2P2/PPNQ2PP/2KR3R w - - 0 14", "queen", "rook", 5, List(
        ("d2d6 e7d6 d1d6 a8d8 d6d8 f8d8 h1d1 d8d1 c1d1 g7g5 d1d2 g8f7 c2e1 f6d7 e1d3 b7b6 c3b5 f7f6 g2g3", -3, 17),
        ("d2e3 a8d8 d1d8 f8d8 h1d1 d8d1 c3d1 a7a6 g2g3 b7b5 e3a3 a6a5 a3e3 h7h6 f3f4 c6b4 c2b4 a5b4", -20, 17))))
    examples.foreach { (start, role, recapturer, unfinishedTail, reports) =>
      val lines = reports.map((moves, cp, depth) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) =
        projectRoot(start, lines.head.moves.head, rows)
      val (packet, paths, root) = project(lines)
      val summary = completePairSummary(root, paths, "observed_exchange")
      val exchange = paths.paths.find(_.moves == lines.head.moves).get
      val retained = paths.paths.find(_.moves == lines(1).moves).get
      val reading = paths.continuationReadings.find(_.path eq exchange).get
      val loss = reading.events.find(_.plyOffset == 1).get
      val recovery = reading.events.find(_.plyOffset == 2).get
      val actor = loss.originalVictim.get
      val target = loss.originalActor.get
      assertEquals(Set(actor.role.name, target.role.name), Set(role))
      assertEquals(recovery.originalVictim, Some(target))
      assertEquals(recovery.originalActor.map(_.role.name), Some(recapturer))
      List(actor, target).foreach(piece =>
        assert(paths.pieceAfter(packet, retained, piece, retained.moves.size).flatten.nonEmpty))
      val visible = (summary \ "units").as[List[JsObject]].flatMap(unit => refs(unit("en"))).toSet
      (List(0, 1, 2).map(RootQuestionSummary.MoveRef(exchange, _)) ++
        List(0, retained.moves.size - 1).map(RootQuestionSummary.MoveRef(retained, _)))
        .foreach(ref => assert(visible(ref.json)))
      assert(exchange.ids.forall((root \ "evidence_ids").as[List[String]].contains))
      List(2, unfinishedTail).foreach { length =>
        val (_, _, incomplete) = project(lines.head.copy(moves = lines.head.moves.take(length)) :: lines.tail)
        assert(!(incomplete \ "detail" \ "summary_question" \ "question").asOpt[String].contains("observed_exchange"),
          "a pending loss must keep the ordinary full reading")
      }
    }


  test("master root questions keep bilingual replies and recapture costs in complete units"):
    // Saved balanced Tata Steel Masters 2022/2023 positions; no engine invocation.
    val examples = List(
      ("r3b1k1/r4ppp/3np3/3p4/p2P4/1P1NPP2/R2N1KPP/2R5 w - - 0 24", "paired_support_use", "c1a1", 7, List(
        ("c1a1 d6c8 b3a4 a7a4 a2a4 a8a4 a1a4 e8a4 e3e4 a4b5 d3c5 c8a7 d2b1 a7c6 b1a3 c6d4 f2e3", 10, 19),
        ("d3c5 a4b3 a2a7 a8a7 c5b3 a7a2 c1a1 a2c2 a1c1", 0, 19))),
      ("5r2/pp1rbppk/2n5/8/3p3P/3N4/PP1BPP2/2R2K1R b - - 1 21", "support_and_interposition", "d7d5", 3, List(
        ("f8e8 h1g1 f7f6 g1g4 h7g8 a2a4 g8f7 b2b4 e8h8 g4f4 h8h5 b4b5 c6e5 d3e5 h5e5", -15, 17),
        ("d7d5 d2g5 f7f6 g5f4 f8e8 f1g2 h7g8 c1b1 d5a5 a2a3 g8f7", -13, 17))),
      ("r2qnrk1/p2p2pp/b1n5/2p2p2/2P2P2/P1PBB3/4N1PP/R2Q1RK1 b - - 1 14", "provider_transfer", "d8a5", 6, List(
        ("d7d6 e3f2 g7g6 a1b1 d8c7 f1e1 e8f6 d1a4 a6b7 f2h4 a8e8 e2g3 e8e1 b1e1 c6d8 h4f6", -1, 16),
        ("d8a5 f1e1 g7g6 e2c1 d7d6 c1b3 a5c7 e3f2 e8f6 f2h4 g8g7 a1a2 a8e8 a2e2 c6d8", 4, 16))))
    examples.foreach { (start, question, shortenedRoot, length, reports) =>
      val lines = reports.map((moves, cp, depth) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) = projectRoot(start, lines.head.moves.head, rows)
      val (packet, paths, root) = project(lines)
      val detail = (root \ "detail").as[JsObject]
      assertEquals((detail \ "summary_kind").as[String], "question_answer")
      val meaning = (detail \ "summary_question").as[JsObject]
      assertEquals((meaning \ "question").as[String], question)
      val units = (meaning \ "units").as[List[JsObject]]
      assertOwnedBilingualUnits(units, paths, Some(root))
      if question == "paired_support_use" then
        val costs = units.filter(unit => (unit \ "role").as[String] == "cost")
        val covered = costs.flatMap(unit => refs(unit("covered_moves"))).map(locateRef(paths, _)).toSet
        val expected = Map("c1a1" -> Set(2, 3, 4, 5, 6, 7), "d3c5" -> Set(1, 2, 3, 4))
        paths.paths.filter(path => expected.contains(path.moves.head)).foreach(path =>
          assertEquals(covered.filter(_._1 eq path).map(_._2), expected(path.moves.head)))
        val facts = selectedFacts(packet, paths, root).core.flatMap(_.statements)
        val uses = facts.collect { case value: CommentaryAnswer.SupportRecapture => value }
        assertEquals(uses.map(_.witness.moves.head).toSet, expected.keySet)
        uses.foreach(use => assertEquals(use.use.recapture.originalVictim, use.use.targetCapture.originalActor))
        val exchanged = paths.paths.find(_.moves.head == "c1a1").get
        val losses = paths.continuationReadings.find(_.path eq exchanged).get.events
        assertEquals(losses.find(_.plyOffset == 7).get.originalVictim,
          losses.find(_.plyOffset == 6).get.originalActor, "the supporting recapturer is itself captured")
        assert(facts.collect { case value: CommentaryAnswer.MaterialEvents => value }
          .exists(value => (value.path eq exchanged) && value.events.exists(_.plyOffset == 7)),
          "the recapturer's loss remains explicit before wording")
        for language <- List("en", "ko"); offset <- List(3, 4, 5, 6, 7) do
          assert(units.flatMap(unit => refs(unit(language))).contains(RootQuestionSummary.MoveRef(exchanged, offset).json),
            "the interleaved rook exchange and final recapturer loss remain visible")
      val (_, _, reordered) = project(lines.reverse)
      assertEquals(summaryMeaning((reordered \ "detail" \ "summary_question").as[JsObject]), summaryMeaning(meaning))
      val (_, shortPaths, incomplete) = project(lines.map(line =>
        if line.moves.head == shortenedRoot then line.copy(moves = line.moves.take(length)) else line))
      rejectUnansweredQuestion(incomplete, shortPaths, question,
        "a missing reply or capture must not be hidden by the short reading")
    }


  test("master capture questions retain check replies and the actual recapturer"):
    // Four saved balanced master comparisons; product selection, no engine.
    val examples = List(
      ("2rq1rk1/3n1ppp/p2bp3/1pp5/3P4/1N2Q1P1/PP1NPPKP/R1R5 w - - 0 20", "attack_or_immediate_exchange", "d2e4", 2, 4, List(
        ("d2e4 c5d4 b3d4 d7c5 d4f3 c5e4 e3e4 d8b6 c1c2 c8c2 e4c2 d6e7 a1c1 b6a5 c2c7 a5c7 c1c7 e7f6 b2b3 f8d8 c7c1 g7g5", 32, 18),
        ("d4c5 d7c5 c1d1 c8c7 a1c1 d8a8 d2f3 c5b3 c1c7 d6c7 e3b3 f8d8", 20, 18))),
      ("r3rbk1/1p1bqppp/8/pB1p4/7P/P3PP2/1PQ2P2/2KRB2R w - - 2 18", "attack_or_immediate_exchange", "d1d5", 4, 3, List(
        ("d1d5 e8c8 e1c3 d7e6 d5d3 c8c5 a3a4 h7h5 c1b1 g7g6 c2d2 f8g7 c3g7 g8g7 b5d7 g7h7", -18, 16),
        ("b5d7 e7d7 e1c3 e8c8 c1d2 d5d4 d2e1 d7c7 d1d4 b7b5 c2e4 b5b4 c3d2 c7c2 e4c2 c8c2 e1e2 c2b2 a3b4 a5b4 h1c1 a8b8 e2d3", -91, 16))),
      ("2kr3r/ppp2p1p/2nn2p1/4p3/2N1P3/2P2BP1/PP3P1P/3RR1K1 w - - 3 16", "escape_or_recapture", "f3g4", 6, 2, List(
        ("c4e3 c6b8 f3g2 h7h5 g2h3 b8d7 e3d5 h5h4 g1g2 c8b8 h3d7 d8d7 d5f6 d7e7 g3g4", 47, 16),
        ("f3g4 c8b8 g4e2 b8c8 f2f4 d6c4 e2c4 d8d1 e1d1 e5f4 g3f4 h8d8 d1d8 c6d8 g1f2 c8d7 e4e5 d7e7", 28, 16))),
      ("5rk1/p4rb1/3q2p1/2p1p1Bp/2pn4/P1NP1PP1/4P1K1/1R1Q2R1 w - - 0 27", "capture_now_or_after_attack", "c3e4", 3, 3, List(
        ("d3c4 d6e6 d1d3 f7d7 g1d1 f8f7 c3e4", 64, 18),
        ("c3e4 d6a6 d3c4 a6c4 b1b2 c4a6 d1d2 c5c4 d2c3 f7c7 g1d1 g8h7 d1a1 a6a4 a1b1 a4c6 b1a1", 0, 18))))
    examples.foreach { (start, question, shortenedRoot, length, captureCount, reports) =>
      val lines = reports.map((moves, cp, depth) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) =
        val (_, paths, root) = projectRoot(start, lines.head.moves.head, rows)
        (paths, root)
      val (paths, root) = project(lines)
      val meaning = (root \ "detail" \ "summary_question").as[JsObject]
      assertEquals((meaning \ "question").as[String], question)
      val units = (meaning \ "units").as[List[JsObject]]
      def occurrence(ref: JsObject) = locateRef(paths, ref)
      val covered = units.flatMap(unit => refs(unit("covered_moves")))
      assertEquals(covered.size, captureCount)
      assertOwnedBilingualUnits(units, paths, Some(root))
      covered.foreach { ref =>
        val (path, offset) = occurrence(ref)
        assert(path.certified.flatMap(_._2.material).get.captures.exists(_.plyOffset == offset))
      }
      val core = units.flatMap(unit => refs(unit("en"))).map(occurrence)
      if question == "escape_or_recapture" then
        val path = paths.paths.find(_.moves.head == "f3g4").get
        List(0, 1, 2, 5, 6).foreach(offset => assert(core.contains(path -> offset),
          "check response, approach, capture and recapture stay in the answer"))
        assertPhysicalRecapture(paths, path, List(5, 6))
      if question == "capture_now_or_after_attack" then
        val alternative = paths.paths.find(_.moves.head == "c3e4").get
        assert(core.contains(alternative -> 1), "Qa6 is the first queen evasion")
        assert(!core.contains(alternative -> 5), "the repeated Qa6 is not substituted")
      val (_, reordered) = project(lines.reverse)
      assertEquals(summaryMeaning((reordered \ "detail" \ "summary_question").as[JsObject]), summaryMeaning(meaning))
      val (shortPaths, incomplete) = project(lines.map(line =>
        if line.moves.head == shortenedRoot then line.copy(moves = line.moves.take(length)) else line))
      rejectUnansweredQuestion(incomplete, shortPaths, question,
        "a missing reply or recapture must not be hidden by the short reading")
    }


  test("master mixed accounts keep delayed losses and distinguish countercapture from recapture"):
    // Balanced Tata Steel Masters 2022 records, with prefix-removal controls.
    val examples = List(
      ("r1b4r/p3kn2/2p1p3/2N2pp1/2P4p/4P3/PP2B1PP/2KR3R w - - 0 20", "occupied_pawn_destination", List(("h2h3", 14), ("h2h3", 27)), List(
        ("g2g4 f7e5 h1g1 h8g8 c1c2 c8d7 c2c3 a8f8 h2h3 d7c8 c3d4 e5f7 d4c3", 0, 18),
        ("h2h3 a7a5 e3e4 f5f4 c1c2 e6e5 b2b3 a8a7 c2c3 e7f6 c5d3 g5g4 e2g4 c8g4 h3g4 a7d7 d3f2 d7d1 h1d1 f6e7 a2a3 e7f6 d1h1 c6c5 f2d3 f6g5 d3c5 g5g4", -12, 18))),
      ("r4rk1/p1p3pp/1b6/2PpN3/4b1Pq/4B2P/PP3P1K/R2Q1R2 b - - 0 19", "counter_capture_comparison", List(("h4e7", 11), ("h4e7", 12), ("b6a5", 8)), List(
        ("h4e7 e5c6 e7f6 c5b6 f6c6 b6a7 f8f3 f1g1 c6d6 g1g3 f3e3 f2e3 a8a7 d1b3 d6f6 b3b8 g8f7", 31, 17),
        ("b6a5 f2f4 a8e8 d1a4 a5d2 e3d2 e8e5 a4d4 e5e6 d4f2 h4e7 d2c3 h7h5 g4h5 f8f7 f1e1", 91, 17))))
    examples.foreach { (start, question, cuts, reports) =>
      val lines = reports.map((moves, cp, depth) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) =
        val (_, paths, root) = projectRoot(start, lines.head.moves.head, rows)
        (paths, root)
      val (paths, root) = project(lines)
      val meaning = (root \ "detail" \ "summary_question").as[JsObject]
      assertEquals((meaning \ "question").as[String], question)
      val units = (meaning \ "units").as[List[JsObject]]
      def occurrence(ref: JsObject) = locateRef(paths, ref)
      assertOwnedBilingualUnits(units, paths, Some(root))
      val coverage = units.flatMap(unit => refs(unit("covered_moves"))).map(occurrence)
      assertEquals(coverage.distinct.size, coverage.size)
      assertOwnedBilingualUnits(detailUnits(meaning), paths, Some(root))
      val displayed = (units.flatMap(unit => refs(unit("en"))) ++ detailRefs(meaning)).map(occurrence).distinct
      if question == "counter_capture_comparison" then
        val attack = paths.paths.find(_.moves.head == "h4e7").get
        val escape = paths.paths.find(_.moves.head == "b6a5").get
        assertEquals(coverage.filter(_._1 eq attack).map(_._2).toSet, Set(3, 4, 5, 10, 11, 12))
        assertEquals(coverage.filter(_._1 eq escape).map(_._2).toSet, Set(5, 6))
        List(8, 9).foreach(offset => assert(displayed.contains(attack -> offset),
          "the later check stays with its actual response"))
        val events = paths.continuationReadings.find(_.path eq attack).get.events
        assertNotEquals(events.find(_.plyOffset == 4).get.originalVictim,
          events.find(_.plyOffset == 3).get.originalActor, "taking a different target is not a recapture")
        val trade = events.filter(event => Set(10, 11)(event.plyOffset))
        assertEquals(trade.flatMap(_.originalVictim).map(_.role.name).toSet, Set("bishop", "rook"))
        assertPhysicalRecapture(paths, attack, List(10, 11))
        val reading = (root \ "detail" \ "summary").as[String]
        List("Rxe3", "fxe3", "bishop", "rook").foreach(value => assert(reading.contains(value), reading))
      else
        val open = paths.paths.find(_.moves.head == "h2h3").get
        assertEquals(coverage.map(_._2).toSet, Set(12, 13, 14, 17, 18, 26, 27))
        assert(coverage.forall(_._1 eq open))
        List(12, 13, 17, 18).foreach(offset => assert(displayed.contains(open -> offset)))
        val repeated = displayed.filter((path, offset) => paths.notation.line(path.moves)(offset) == "Bxg4")
        assertEquals(repeated.map(_._2).toSet, Set(12, 13), "both bishops have their own occurrence")
      val (_, reordered) = project(lines.reverse)
      assertEquals(summaryMeaning((reordered \ "detail" \ "summary_question").as[JsObject]), summaryMeaning(meaning))
      cuts.foreach { (rootMove, length) =>
        val (shortPaths, incomplete) = project(lines.map(line =>
          if line.moves.head == rootMove then line.copy(moves = line.moves.take(length)) else line))
        rejectUnansweredQuestion(incomplete, shortPaths, question,
          "missing delayed loss, recapture or the capturer's escape cannot retain the original answer")
      }
    }


  test("master reply questions keep checking trades and do not forbid controlled destinations"):
    // Two saved balanced Tata Steel master records; existing board rules only.
    val examples = List(
      ("1r2r1k1/p1pq1p2/1p1b1pp1/8/2QPB3/7P/PP3PP1/R3R1K1 b - - 3 22", "opening_counterplay", List(("g8g7", 3), ("g8g7", 4)), List(
        ("b6b5 c4c2 d6f8 e1d1 e8d8 c2c6 f6f5 e4f3", -28, 18),
        ("g8g7 e4c6 e8e1 a1e1 d7f5 g2g3 b8h8 c6e8 h8f8 g1g2 f5c8 e8c6 a7a5 h3h4 f6f5 f2f4 c8d8 b2b3 d6e7 e1e3 e7a3", -12, 18))),
      ("2r2rk1/pp1n1pp1/2n1p2p/1q1pP2P/3P2P1/4QN2/PP1N1P2/1KR4R w - - 3 19", "controlled_destination", List(("a2a3", 1), ("g4g5", 1)), List(
        ("g4g5 c6b4 c1g1 g8h7 d2b3 b5d3 e3d3 b4d3 h1h2 c8c7 a2a3 d7b6 g1d1 d3f4 b3c5 b6c4 h2h4 f4e2 h4g4 f8c8 d1e1 e2d4 g5g6 f7g6 f3d4 c7c5 h5g6 h7g8 d4e6", 64, 16),
        ("a2a3 c6e7 h1g1 d7b6 c1c5 b5a4 e3c3 a4a6 c3e3 b6a4 c5c1 c8c1 g1c1 f8c8 c1c8 e7c8 g4g5 a6b5 e3b3 b5b3 d2b3 h6g5 f3g5 c8e7", -2, 20))))
    examples.foreach { (start, question, cuts, reports) =>
      val lines = reports.map((moves, cp, depth) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) = projectRoot(start, lines.head.moves.head, rows)
      val (packet, paths, root) = project(lines)
      val meaning = (root \ "detail" \ "summary_question").as[JsObject]
      assertEquals((meaning \ "question").as[String], question)
      val units = (meaning \ "units").as[List[JsObject]]
      def occurrence(ref: JsObject) = locateRef(paths, ref)
      assertOwnedBilingualUnits(units, paths, Some(root))
      assertOwnedBilingualUnits(detailUnits(meaning), paths, Some(root))
      val coverage = units.flatMap(unit => refs(unit("covered_moves"))).map(occurrence)
      val core = units.flatMap(unit => refs(unit("en"))).map(occurrence).toSet
      if question == "opening_counterplay" then
        val path = paths.paths.find(_.moves.head == "g8g7").get
        List(0, 1, 2, 3, 4).foreach(offset => assert(core(path -> offset),
          "root, attack, checking capture, recapture and queen evasion stay in the answer"))
        assertEquals(coverage.toSet, Set(path -> 2, path -> 3))
        assertPhysicalRecapture(paths, path, List(2, 3))
        assertEquals(paths.notation.line(path.moves)(2), "Rxe1+")
        assertEquals(paths.notation.line(path.moves)(3), "Rxe1")
      else
        assertEquals(coverage, Nil)
        paths.paths.filter(path => Set("a2a3", "g4g5")(path.moves.head)).foreach(path =>
          List(0, 1).foreach(offset => assert(core(path -> offset))))
        val controlled = paths.paths.find(_.moves.head == "a2a3").get
        val afterRoot = packet.assembly.lineReplays(controlled.line.ref).legalSteps.head.after
        val knightMove = afterRoot.legalMoves.find(_.toUci.uci == "c6b4")
        assert(knightMove.nonEmpty, "control of b4 does not forbid Nb4")
        assert(knightMove.get.after.legalMoves.exists(_.toUci.uci == "a3b4"), "the controlling pawn can legally capture there")
      val (_, _, reordered) = project(lines.reverse)
      assertEquals(summaryMeaning((reordered \ "detail" \ "summary_question").as[JsObject]), summaryMeaning(meaning))
      cuts.foreach { (rootMove, length) =>
        val (_, shortPaths, incomplete) = project(lines.map(line =>
          if line.moves.head == rootMove then line.copy(moves = line.moves.take(length)) else line))
        rejectUnansweredQuestion(incomplete, shortPaths, question,
          "missing the actual reply, checking recapture or queen evasion cannot retain the original answer")
      }
    }


  test("master move-order comparisons keep separate histories and complete observed replies"):
    // Five saved balanced master positions, including Black to move and castling.
    val examples = List(
      ("4r1k1/5pb1/2p4p/2Pp1Q2/q7/1N4P1/2P2P2/2R3K1 w - - 1 28", "observed_arrangement_convergence", List(
        ("g1g2 g7c3 c1h1 g8g7 f5f3 a4c4 h1d1 e8e6 d1d3 c3b2 f3d1 c4e4 d1f3", 0, 24),
        ("f5d3 a4b4 g1g2 g7c3 c1h1 g8g7 d3f3 b4c4 h1d1 e8e6 d1d3 c3b2 f3d1 c4e4 d1f3", 0, 24))),
      ("r3k2r/pppbnppp/2np1q2/5P2/4P3/1N1B4/P1PB2PP/R2Q1R1K w kq - 2 15", "observed_move_order", List(
        ("a2a4 c6e5 d1c1 h7h6 d2c3 e7c6 b3d2 f6e7 a1b1 f7f6 b1b7 d7c8 b7b1 e8g8 h2h3 c8d7", 39, 16),
        ("d1c1 h7h6 a2a4 e8g8 f1f3 c6e5 f3g3 g8h7 g3h3 d6d5 b3c5 d7c8 d3e2 b7b6", 39, 16))),
      ("1r4k1/3b1p1p/1q2p1p1/p2p4/2pP1P2/PrN3P1/1P1RRPKP/2Q5 b - - 10 28", "observed_arrangement_convergence", List(
        ("g8g7 c1g1 h7h5 h2h4 b6d6 f2f3 b8a8 g1e1 a8h8 e1f2 h8g8 f2e1", 0, 18),
        ("b6d6 h2h4 h7h5 f2f3 g8f8 d2c2 f8g7 c1e3 b8g8 c2d2 g8h8 e3f2 h8g8 f2e1 g8h8 e1f2", 0, 18))),
      ("r4rk1/pp2qpb1/3pbnp1/2p1n1Np/2P1PB1P/2N5/PP1QBPP1/R4RK1 b - - 5 14", "observed_move_order", List(
        ("b7b6 b2b3 a8d8 a1d1 f6h7 f4e3 f8e8 f2f3 e5c6 e2d3 h7g5 h4g5 e7f8", 75, 18),
        ("a8d8 b2b3 e5c6 a1e1 b7b6 f2f3 e6c8 f4g3 c6d4 e2d3 d8e8 g1h1 f6h7 f3f4", 76, 18))),
      ("rq3rk1/5pbp/2ppbnp1/4p3/P3P3/1NB2N2/1PP2PPP/R2QR1K1 w - - 3 17", "observed_move_order", List(
        ("a4a5 h7h6 b3d2 f6d7 d1e2 b8b5 e2e3 a8a6 b2b3 f8a8 e1b1 g7f8 h2h3 g8h7 e3e1", 78, 16),
        ("b3d2 f6h5 a4a5 h5f4 b2b3 f8d8 d2c4 h7h6 d1c1 a8a7 c3d2 g6g5 h2h3 d6d5 d2f4 e5f4 e4d5 e6d5", 75, 19))))
    examples.foreach { (start, question, reports) =>
      val lines = reports.map((moves, cp, depth) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) = projectRoot(start, lines.head.moves.head, rows)
      val (packet, paths, root) = project(lines)
      val meaning = (root \ "detail" \ "summary_question").as[JsObject]
      assertEquals((meaning \ "question").as[String], question)
      val units = (meaning \ "units").as[List[JsObject]]
      def occurrence(ref: JsObject) = locateRef(paths, ref)
      val sequences = selectedSequences(selectedFacts(packet, paths, root))
      val detailed = sequences.map(ref => ref.path -> ref.offset)
      val core = units.flatMap(unit => refs(unit("en"))).map(occurrence)
      assertOwnedBilingualUnits(units ++ detailUnits(meaning), paths, Some(root))
      val displayed = (units.flatMap(unit => refs(unit("en"))) ++ detailRefs(meaning)).toSet
      sequences.foreach(ref => assert(displayed(ref.json), "the selected sequence remains fully displayed"))
      assert(units.forall(unit => refs(unit("covered_moves")).isEmpty))
      val ends = detailed.groupMap(_._1)(_._2).toList.map { (path, offsets) =>
        val end = offsets.max
        assertEquals(offsets.toSet, (0 to end).toSet)
        assert(core.contains(path -> 0))
        val prefix = path.certified.flatMap(_._2.material).get.events.filter(_.plyOffset <= end)
        assert(!prefix.exists(_.promotionGainCp != 0))
        assert(!path.certified.flatMap(_._2.material).get.captures.exists(_.plyOffset <= end))
        assert(!paths.notation.line(path.moves).take(end + 1).exists(s => s.endsWith("+") || s.endsWith("#")))
        path -> end
      }
      assertEquals(ends.size, 2)
      if question == "observed_arrangement_convergence" then
        val fens = ends.map((path, end) => path.positions(end + 1).fen)
        assertEquals(fens.map(_.split(' ').take(4).toList).distinct.size, 1)
        assertEquals(fens.distinct.size, 2, "move counters remain different")
        assertEquals(ends.map((path, end) => path.positions(end + 1).id).distinct.size, 2,
          "matching board fields do not merge occurrences or repetition history")
        assert(ends.forall((path, end) => core.contains(path -> end)))
      else
        ends.foreach { (path, end) =>
          val peer = ends.find(_._1 != path).get._1
          val other = path.moves.indexOf(peer.moves.head, 1)
          assertEquals(end, other + 1, "the actual reply after both actions is recorded")
          assert(core.contains(path -> other))
          assert(core.exists((owner, offset) => owner == path && offset % 2 == 1))
          ends.foreach { (own, _) =>
            val original = paths.identitiesAfter(packet, own, 0).map(_._1).find(_.square.key == own.moves.head.take(2)).get
            assertEquals(paths.pieceAfter(packet, path, original, end + 1), paths.pieceAfter(packet, own, original, 1))
          }
        }
      val (_, _, reordered) = project(lines.reverse)
      // Tied engine rows can reverse presentation order; the owned question,
      // core moves and detailed prefix must remain the same.
      val reorderedMeaning = (reordered \ "detail" \ "summary_question").as[JsObject]
      assertEquals(summaryMeaning(reorderedMeaning), summaryMeaning(meaning))
      ends.foreach { (path, end) =>
        val peer = ends.find(_._1 != path).get._1
        val cut = if question == "observed_arrangement_convergence" then
          path.positions.indices.drop(1).find(count => peer.positions.drop(1).exists(position =>
            path.positions(count).fen.split(' ').take(4).sameElements(position.fen.split(' ').take(4)))).get - 1
        else end
        val (_, _, incomplete) = project(lines.map(line =>
          if line.moves.head == path.moves.head then line.copy(moves = line.moves.take(cut)) else line))
        assert((incomplete \ "detail" \ "summary_question" \ "question").asOpt[String] != Some(question),
          "missing every observed meeting or the required reply cannot retain the complete comparison")
      }
    }
    val competing = List(
      ("r2q1rk1/p2b3p/1pn1ppp1/3p4/P2P1b2/1NPB1N2/1P2QPPP/R3R1K1 w - - 0 16", "shared attack and actual bishop evasion", List(
        ("d3b5 f8e8 g2g3 f4d6 b3d2 g8g7 d2f1 d8c8 b5a6 c8c7 f1e3 a8d8 h2h4 h7h5 a1c1 c7b8 a6b5 e8e7", 29, 16),
        ("g2g3 f4d6 d3b5 d8c8 b5a6 c8c7 a1c1 f8e8 b3d2 d6f8 d2f1 c6a5 a6b5 f8h6 c1a1 d7b5 a4b5 c7c4 g1g2 c4e2 e1e2 a5c4 b2b3", 15, 16))),
      ("2rq1rk1/pb3ppp/2nppn2/2p5/2P1P3/P1NBBP2/1PQ3PP/R3K2R w KQ - 2 14", "different original rooks use d1 after castling", List(
        ("e1g1 a7a5 c2d2 e6e5 a1b1 c8a8 f1d1 c6d4 b2b4 a5b4 a3b4 b7c6 c3b5 d8e7 b5d4 e5d4", 50, 16),
        ("a1b1 f6d7 c2d2 c6d4 e1g1 d7e5 b2b4 b7a8 b1d1 d8d7 f3f4 e5g4", 47, 16))),
      ("5rk1/p5pp/1p2R3/2rp4/2p5/2P2P2/P1P3PP/3R2K1 b - - 0 20", "later convergence changes the early route comparison", List(
        ("c5a5 a2a4 f8d8 e6e7 a5a4 d1e1 a4a2 e7b7 g8h8 e1e7 d8g8 e7e2 h7h6 h2h4 a2a3 g1h2 h8h7 h2g3 a3c3 b7a7 d5d4 a7b7 d4d3 c2d3 c4d3", -17, 18),
        ("f8d8 e6e7 c5a5 e7b7 a5a2 d1e1 g8h8 e1e7 d8g8 e7e2 a2a3 g1f1 h7h6 f1e1 a3c3 b7a7 d5d4 a7d7 g8a8 d7d4 a8a2", -12, 18))))
    competing.foreach { (start, reason, reports) =>
      val lines = reports.map((moves, cp, depth) => EngineLine(moves.split(" ").toList, cp, None, depth))
      val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = start,
        playedMoveUci = lines.head.moves.head, variations = lines)).get
      val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
      val paths = ObservedLineProtocol.prepare(packet)
      val selected = MoveWorkProjection.select(packet, paths).json
      val root = (selected \ "candidates").as[List[JsObject]].find(value =>
        (value \ "detail" \ "type").as[String] == "root_comparison").get
      List("observed_move_order", "observed_arrangement_convergence").foreach(question =>
        rejectUnansweredQuestion(root, paths, question, reason))
    }


  test("master check and delayed-pawn comparisons retain neutral exchanges and promotion removal"):
    // Two opened balanced master positions; saved legal lines, no engine search.
    val examples = List(
      ("r3qr1k/1pp1bp1p/p1n4P/4pnp1/PP2N3/1BP2P2/1B1Q1PK1/R2R4 w - - 4 24", "check_status_comparison", List(
        ("d2d7 f5h4 g2f1 f7f5 e4g3 h4f3 b4b5 c6a5 b3a2 a6b5 a4b5 g5g4 g3f5 f3h2 f1g1 h2f3 g1f1", 0, 16),
        ("g2f1 a8d8 d2e2 d8d1 a1d1 e8c8 b4b5 a6b5 a4b5 c6a5 b3a2 f7f6 e4g3 f5h4", -36, 16))),
      ("r4rk1/2p1bppp/p1n5/1p1qPb2/3B4/1PPp1N2/1P3PPP/RB1QR1K1 b - - 6 17", "deferred_pawn_attack", List(
        ("d3d2 e1e2 c6d4 f3d4 f5b1 a1b1 e7g5 h2h4 g5f4 g2g3 f4e5 e2d2 e5d4 d2d4 d5c6 b1a1 a8e8 b3b4 e8e6 d4d7 f8e8 d1d5", 29, 16),
        ("f8d8 h2h4 d3d2 b1f5 d2e1q d1e1 d5b3 e5e6 d8d6 f3e5 c6d4 e6f7 g8f8 c3d4 g7g6 e1e4 a8d8 a1e1 g6f5 e4f5 d6h6", 29, 16))))
    examples.foreach { (start, question, reports) =>
      val lines = reports.map((moves, cp, depth) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) = projectRoot(start, lines.head.moves.head, rows)
      val (packet, paths, root) = project(lines)
      val meaning = (root \ "detail" \ "summary_question").as[JsObject]
      assertEquals((meaning \ "question").as[String], question)
      val units = (meaning \ "units").as[List[JsObject]]
      def occurrence(ref: JsObject) = locateRef(paths, ref)
      val core = units.flatMap(unit => refs(unit("en"))).map(occurrence)
      val covered = units.flatMap(unit => refs(unit("covered_moves"))).map(occurrence)
      assertOwnedBilingualUnits(units ++ detailUnits(meaning), paths, Some(root))
      val arms = core.map(_._1).distinct
      assertEquals(arms.size, 2)
      arms.foreach { path =>
        val end = (core ++ covered).filter(_._1 == path).map(_._2).max
        val captures = path.certified.flatMap(_._2.material).get.captures.filter(_.plyOffset <= end)
        assertEquals(covered.filter(_._1 == path).map(_._2).sorted, captures.map(_.plyOffset).sorted)
      }
      val cuts = if question == "check_status_comparison" then
        val quiet = arms.find(path => paths.identitiesAfter(packet, path, 0).exists((original, _) =>
          original.role.name == "king" && original.square.key == path.moves.head.take(2))).get
        val checked = arms.find(_ != quiet).get
        val sans = paths.notation.line(checked.moves)
        val check = sans.indexWhere(_.endsWith("+"))
        val later = quiet.moves.indexOf(checked.moves(check))
        val gain = checked.certified.flatMap(_._2.material).get.captures.head.plyOffset
        assertEquals(later, quiet.moves.size - 1)
        assert(!paths.notation.line(quiet.moves)(later).endsWith("+"))
        List(check, check + 1, gain).foreach(offset => assert(core.contains(checked -> offset)))
        val originalKing = paths.identitiesAfter(packet, quiet, 0).map(_._1).find(p =>
          p.role.name == "king" && p.square.key == quiet.moves.head.take(2)).get
        val relocated = paths.pieceAfter(packet, quiet, originalKing, 1).flatten.get
        assert(relocated.square != originalKing.square)
        assertEquals(paths.pieceAfter(packet, quiet, originalKing, later + 1), Some(Some(relocated)))
        val trades = quiet.certified.flatMap(_._2.material).get.captures
        assertEquals(trades.size, 4)
        trades.grouped(2).foreach { pair =>
          assertEquals(pair.last.plyOffset, pair.head.plyOffset + 1)
          assert(pair.head.side != pair.last.side)
          assertEquals(pair.head.capturedRole, pair.last.capturedRole)
          assert(pair.last.recapture)
        }
        List(checked -> gain, quiet -> later)
      else
        val later = arms.find(_.certified.flatMap(_._2.material).get.events.exists(_.promotionGainCp > 0)).get
        val now = arms.find(_ != later).get
        val promotion = later.certified.flatMap(_._2.material).get.events.find(_.promotionGainCp > 0).get
        val at = promotion.plyOffset
        val pawn = paths.identitiesAfter(packet, now, 0).map(_._1).find(_.square.key == now.moves.head.take(2)).get
        assertEquals(pawn.role.name, "pawn")
        assertEquals(paths.pieceAfter(packet, later, pawn, at + 1).flatten.map(_.role), Some(promotion.movement.afterRole))
        assertEquals(paths.pieceAfter(packet, later, pawn, at + 2), Some(None))
        assert(core.contains(later -> (at - 1)), "the intervening bishop loss remains visible")
        List(at, at + 1, at + 2).foreach(offset => assert(core.contains(later -> offset),
          "promotion, removal and immediate pawn countercapture stay in the selected answer"))
        val captures = now.certified.flatMap(_._2.material).get.captures
        assertEquals(captures.size, 8)
        assert(captures.groupMapReduce(_.capturedRole)(capture => if capture.side.white then 1 else -1)(_ + _).values.forall(_ == 0))
        assertEquals(covered.filter(_._1 == now).map(_._2).sorted, captures.map(_.plyOffset).sorted)
        val loss = paths.continuationReadings.find(_.path eq now).get.events.find(_.originalVictim.contains(pawn)).get
        List(later -> (at + 1), now -> loss.plyOffset)
      val (_, _, reordered) = project(lines.reverse)
      val reorderedMeaning = (reordered \ "detail" \ "summary_question").as[JsObject]
      assertEquals(summaryMeaning(reorderedMeaning), summaryMeaning(meaning))
      cuts.foreach { (path, cut) =>
        val (_, _, incomplete) = project(lines.map(line =>
          if line.moves.head == path.moves.head then line.copy(moves = line.moves.take(cut)) else line))
        assert((incomplete \ "detail" \ "summary_question" \ "question").asOpt[String] != Some(question),
          "missing the observed capture, check comparison or promotion removal cannot retain the complete answer")
      }
    }


  test("master pair questions close recaptures and same-count convergence without conflating pieces"):
    // Opened master games from four events; fixed rows, no engine search.
    val examples = List(
      ("r1bq2k1/1p1n1r1p/2p1pb2/p2p1pp1/2PPn3/4BNP1/PPR1PPBP/1NQR2K1 w - - 2 15", "b1c3", "contested_pair_response", List(
        ("b1c3 h7h6 c3e4 f5e4 f3d2 f6g7 f2f3 d7f6 f3e4 f6g4 d2f1 d5e4 h2h3 g4e3 c1e3 c6c5 e3e4 a8a6 c2d2 c5d4 e2e3 e6e5 g3g4 c8e6 e4d3 a6d6", 71, 18, List(0, 1, 2, 3), 3),
        ("b1d2 e4d6 f3e5 f7g7 d2f3 a5a4 b2b3 d7f8 e5d3 a4b3 a2b3 c8d7 h2h3 d7e8 f3e5 h7h5 g1h2 d6e4 f2f3", 44, 18, List(0, 1), 1))),
      ("r1bqr1k1/1pp2pbp/6p1/1P2pn2/3n4/2NP1NP1/2P2PBP/1RBQR1K1 w - - 1 15", "f3d2", "contested_pair_response", List(
        ("f3d4 f5d4 c1e3 h7h5 d1c1 d4f5 e3d2 f5d4 d2g5 f7f6 g5e3 d4f5 b5b6 c7c6 c3e4 f5e3 c1e3 g8h7 e3c5", 36, 17, List(0, 1), 1),
        ("f3d2 a8a7 c3d5 d4b5 b1b5 c7c6 b5c5 c6d5 c5d5 d8c7 c1b2 c8e6 b2e5 c7c8 d5b5 e6d7 b5d5", 0, 21, List(0), 1))),
      ("4r1k1/pp1q2pp/2p2p2/3p1n2/3P1B2/2PQ4/PP3PPP/4R1K1 w - - 1 22", "e1e2", "contested_pair_response", List(
        ("e1e8 d7e8 g1f1 e8c8 g2g4 f5e7 h2h3 e7g6 f4h2 g6f8 d3g3 a7a6 b2b3", -9, 18, List(0, 1), 1),
        ("e1e2 g7g5 f4c1 g8g7 h2h3 f5d6 g1f1 e8e2 d3e2 d7e8 b2b3", -18, 18, List(0, 7, 8), 8))),
      ("2q2rk1/pb1nbppp/1p2p3/3p2Bn/3P4/1QN2NP1/PP2PPBP/R5K1 b - - 3 15", "e7g5", "contested_pair_response", List(
        ("e7g5 f3g5 h7h6 g5f3 h5f6 a1c1 c8c7 e2e3 f8c8 g2f1 c7d8 b3a4 a7a6 a4d1 f6e8 c1c2 e8d6 a2a3", 23, 18, List(0, 1), 1),
        ("c8d8 g5e7 d8e7 a1c1 f8c8 b3d1 d7b8 f3e5 h5f6 d1a4 a7a5 g2f1 e7b4 e5d3 b4e7 e2e3 b8c6 d3f4 e7d8 f1d3 h7h6 f4e2 g7g5", 27, 18, List(0, 1, 2), 2))),
      ("3r2k1/1pp1qpp1/2n1b2p/p7/4P3/Pr2PNP1/1PR1P1BP/2Q2RK1 b - - 10 24", "a5a4", "observed_arrangement_convergence", List(
        ("b3b6 c2c5 a5a4 h2h3 d8d7 g1h2 d7d8", 0, 21, List(0, 2), 2),
        ("a5a4 c2c5 b3b6 c1c3 d8d7 c3c1", 0, 32, List(0, 2), 2))),
      ("r3r1k1/pp3ppp/2n2q2/2P5/3pn1b1/P3PN2/1PQBBPPP/R4RK1 w - - 0 15", "a1d1", "contested_pair_response", List(
        ("a1d1 e4d2 d1d2 d4e3 d2d6 f6f4 c2c3 h7h6 h2h3 g4h5 b2b4 a8d8 f2e3 f4e3 c3e3 e8e3 d6d8 c6d8", 38, 17, List(0, 1, 2), 2),
        ("d2e1 d4d3 c2d3 f6b2 a1b1 b2a2 b1c1 a8d8 c1c2 d8d3 c2a2 d3d5 h2h3 g4e6 a2c2 d5c5 c2c5 e4c5 e1c3 h7h6 f1c1 e8c8", 27, 17, List(0), 1))),
      ("3rr1k1/1p2qp1p/2p2np1/p2b4/1n6/1P1PNNPP/1Q2PPB1/R1R3K1 b - - 4 26", "b7b6", "contested_pair_response", List(
        ("b7b6 c1d1 h7h5 e3d5 d8d5 d1d2 c6c5 a1d1 e8d8 b2c3 b4c6 h3h4 c6d4 e2e3 d4f3 g2f3 d5d6", -20, 17, List(0, 3, 4), 4),
        ("d8a8 e3d5 f6d5 g2f1 c6c5 c1c4 b7b6 d3d4 c5d4 f3d4 a8c8 a1c1 c8c4 c1c4 e7e5", 7, 17, List(0, 1, 2), 2))),
      ("3r1r1k/pb2q2p/1p1pppp1/n1n5/3NP3/1BN3QP/PPPR1PP1/R5K1 w - - 6 20", "a1e1", "contested_pair_response", List(
        ("a1d1 a7a6 d4e2 b6b5 a2a3 a5b3 c2b3 e6e5 c3d5 e7g7 g3e3 f6f5 f2f3 f5f4 e3c3 d8c8 c3b4", 7, 17, List(0, 4, 5, 6), 6),
        ("a1e1 e6e5 c3d5 e7d7 d4f3 b6b5 f3h2 f6f5 f2f3 a7a6 d5c3 a5b3 a2b3 b5b4 c3d5 d7f7", -46, 20, List(0, 11, 12), 12))))
    examples.foreach { (start, played, question, reports) =>
      val lines = reports.map((moves, cp, depth, _, _) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) = projectRoot(start, played, rows)
      val (packet, paths, root) = project(lines)
      val meaning = (root \ "detail" \ "summary_question").as[JsObject]
      assertEquals((meaning \ "question").as[String], question)
      val units = (meaning \ "units").as[List[JsObject]]
      if played == "b7b6" then
        val controls = selectedFacts(packet, paths, root).core.flatMap(_.statements).collect {
          case value: CommentaryAnswer.ControlState if value.actor.role.name == "rook" &&
              value.target.exists(piece => piece.role.name == "bishop" && piece.square.key == "d5") => value
        }
        assertEquals(controls.map(value => value.move.path.moves.head -> value.present).toSet,
          Set("b7b6" -> true, "d8a8" -> false), "the same bishop keeps or loses the d8-rook's support")
      if played == "a1e1" then
        val facts = selectedFacts(packet, paths, root).core.flatMap(_.statements)
        assert(facts.exists {
          case value: CommentaryAnswer.ControlState => value.move.path.moves.head == "a1d1" &&
            value.move.offset == 4 && value.actor.square.key == "a3" && !value.present &&
            value.target.exists(piece => piece.role.name == "bishop" && piece.square.key == "b3")
          case _ => false
        }, "a3, rather than Rad1, withdraws the a-pawn's support of the same bishop")
        val recapturers = facts.collect { case value: CommentaryAnswer.PieceExchange =>
          value.events.last.originalActor.get.square.key }.toSet
        assertEquals(recapturers, Set("c2", "a2"), "the two original pawns are different recapturers")
      def locate(ref: JsObject) = locateRef(paths, ref)
      val core = units.flatMap(unit => refs(unit("en"))).map(locate)
      val covered = units.flatMap(unit => refs(unit("covered_moves"))).map(locate)
      assertOwnedBilingualUnits(units ++ detailUnits(meaning), paths, Some(root))
      reports.foreach { (moves, _, _, required, _) =>
        val path = paths.paths.find(_.moves == moves.split(" ").toList).get
        required.foreach(offset => assert(core.contains(path -> offset), "required reply or recapture stays visible"))
        if question == "contested_pair_response" then
          val events = covered.filter(_._1 == path).map(_._2).distinct.sorted
          // The survival anchor may be the supplied line's end. It does not
          // make independent later exchanges part of this selected pair.
          val expected = path.certified.flatMap(_._2.material).get.captures
            .filter(event => required.contains(event.plyOffset)).map(_.plyOffset)
          assertEquals(events, expected)
          if events.nonEmpty then
            assertPhysicalRecapture(paths, path, events)
            events.foreach(offset => assert(core.contains(path -> offset),
              "capture and its physical recapture belong to the same selected answer"))
      }
      if question == "observed_arrangement_convergence" then
        val sequences = selectedSequences(selectedFacts(packet, paths, root))
        val displayed = (units.flatMap(unit => refs(unit("en"))) ++ detailRefs(meaning)).toSet
        sequences.foreach(ref => assert(displayed(ref.json)))
        val ends = sequences.map(ref => ref.path -> ref.offset).groupMap(_._1)(_._2).toList
        assert(ends.forall((_, offsets) => offsets.toSet == Set(0, 1, 2)))
        assertEquals(ends.map((path, _) => path.positions(3).fen.split(' ').take(4).toList).distinct.size, 1)
        assertEquals(ends.map((path, _) => path.ids(2)).distinct.size, 2, "equal boards keep separate occurrences")
        assertEquals(ends.map((path, _) => paths.notation.line(path.moves)(2)).distinct.size, 2,
          "the same board may be reached by different SAN at the same count")
      else if covered.size == 4 then
        val pairs = covered.groupMap(_._1)(_._2).toList.map { (path, offsets) =>
          val reading = paths.continuationReadings.find(_.path.line.ref == path.line.ref).get
          reading.events.filter(e => offsets.contains(e.plyOffset)).flatMap(_.originalVictim).toSet
        }
        assertEquals(pairs.distinct.size, 1, "the two accounts concern the same original pieces")
      if played == "f3d2" then
        val retreat = paths.paths.find(_.moves.head == played).get
        val knights = paths.identitiesAfter(packet, retreat, 0).map(_._1).filter(p => p.side.white && p.role.name == "knight")
        assertEquals(knights.size, 2)
        assert(paths.pieceAfter(packet, retreat, knights.find(_.square.key == "f3").get, retreat.moves.size).flatten.nonEmpty)
        assertEquals(paths.pieceAfter(packet, retreat, knights.find(_.square.key == "c3").get, retreat.moves.size), Some(None),
          "saving the named knight does not mean all knight trades are avoided")
      val (_, _, reordered) = project(lines.reverse)
      val other = (reordered \ "detail" \ "summary_question").as[JsObject]
      assertEquals((other \ "question").as[String], question)
      assertEquals(summaryMeaning(other), summaryMeaning(meaning))
      reports.foreach { (moves, _, _, _, cut) =>
        val rootMove = moves.take(4)
        val (_, _, incomplete) = project(lines.map(line => if line.moves.head == rootMove then line.copy(moves = line.moves.take(cut)) else line))
        assert((incomplete \ "detail" \ "summary_question" \ "question").asOpt[String] != Some(question),
          "a missing recapture, evasion, response or meeting cannot retain this complete answer")
      }
    }

  test("master route questions retain incoming attacks, capturing replies and their actual costs"):
    // Six opened master games, with their recorded rows; no new engine search.
    val examples = List(
      ("r2qr1k1/p4ppp/1b6/n1Q1p3/4N3/BP1b1N2/P2P1PPP/R3R1K1 w - - 3 19", "c5c3", "c5", "d6", List(
        ("c5d6 a5b7 d6d8 a8d8 g2g4 f7f6 a1c1 d8d5 e4c3 d5d7 c3e4 h7h6 c1c3 b6a5 c3c6 e8d8 h2h4 a5b6 c6c3", -3, 16, 0, 3, List(0, 1, 2, 3), 3),
        ("c5c3 d3e4 e1e4 a8c8 c3b4 a5c6 b4d6 c6d4 d6d8 e8d8 a1c1 d4f3 g2f3 c8c1 a3c1 b6d4 c1a3 d8c8 a3b4", -10, 16, 6, 9, List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 2))),
      ("2k2b1r/2p2pp1/r3b2p/2pnP2N/pp4P1/P1P2NKP/1P1B1P2/R3R3 b - - 0 22", "b4c3", "g7", "g5", List(
        ("g7g6 h5f4 d5f4 g3f4 b4b3 f4g3 f8e7 a1d1 g6g5 d2e3 e6d7 d1d5 d7c6 d5d1 c6b7 h3h4 g5h4 f3h4 e7h4 g3h4 b7f3 d1d3 h8g8 e1g1 a6g6 e3c5 g6g4 g1g4 g8g4 h4h3", -68, 16, 8, 8, List(0, 1, 2, 3, 8), 3),
        ("b4c3 d2c3 a6b6 f3d2 d5c3 b2c3 c5c4 h5f4 e6d7 a1d1 g7g5 f4d5 b6b3 d2b3 c4b3", -67, 16, 10, 11, List(0, 1, 4, 5, 10, 11), 5))),
      ("r1b1q1k1/pp2rpp1/2nR3p/8/8/1Q3NP1/PP3PBP/R5K1 b - - 2 17", "c8e6", "c8", "e6", List(
        ("c8e6 b3d1 e7d7 d6d7 e6d7 d1b3 e8e7 a2a3 d7e6 b3c3 e7d7 h2h4 a8d8 b2b4", -9, 17, 0, 1, List(0, 1), -1),
        ("c8g4 a2a3 a8c8 h2h3 g4e6 b3b5 c8d8 a1d1 d8d6 d1d6 e7d7 d6d7 e8d7 h3h4 b7b6 f3e5 c6e5 b5e5 e6b3 e5b8 g8h7", 0, 17, 4, 5, List(0, 3, 4, 5), 4))),
      ("r5k1/1p1n1p2/2p2npp/p3q3/P7/1PN1PBP1/2Q2P1P/3R2K1 b - - 1 27", "h6h5", "d7", "c5", List(
        ("d7c5 g1g2 a8e8 h2h4 h6h5 c3e2 e8e7 d1d8 g8g7 e2f4", 10, 18, 0, 1, List(0, 1), -1),
        ("h6h5 c3e2 e5c5 c2b2 a8e8 d1d4 d7e5 f3g2 e8e7 b2d2 c5a3 d2c3 e5d7 e2c1 d7c5", 16, 21, 14, 14, List(0, 6, 7, 12, 14), 14))),
      ("4r3/p5R1/2p1kpp1/8/4PPpP/2r5/PPP5/1K5R b - - 0 25", "c3f3", "c3", "g4", List(
        ("c3f3 g7g6 f3f4 h1g1 f4e4 g6g4 e4g4 g1g4 e6f5 g4g1 e8h8 b2b3 h8h4 g1g7 a7a5 g7a7 f5g4 a7a5 f6f5 a2a4", 0, 18, 6, 7, List(0, 1, 2, 4, 5, 6, 7), 7),
        ("c3h3 h1g1 h3h4 g7g6 e6f7 g6g4 h4g4 g1g4 e8e4 b2b3 a7a5 b1b2 a5a4 b3a4 e4a4 b2b3 a4a8 a2a4 f7e6 g4h4 e6d5 a4a5 a8b8 b3c3 b8a8 h4h6 f6f5 c3b4 a8b8 b4a3 b8b5", 37, 18, 6, 8, List(0, 1, 2, 3, 5, 6, 7, 8), 8))),
      ("2q1r1k1/1p2bpp1/p1rpbnnp/P3p3/4P3/2N1BBNP/1PPR1PP1/R3Q1K1 b - - 12 20", "g6h4", "g6", "f4", List(
        ("c6c4 d2d1 c4c6 d1d3 g6f4 d3d2 g7g6 e3f4 e5f4", 36, 16, 4, 8, List(0, 4, 5, 7, 8), 8),
        ("g6h4 f3e2 c6c3 b2c3 d6d5 f2f3 c8c3 d2d3 c3c8 e3f2 h4g6 d3c3 c8d7 e4d5 e6d5 a1b1 d5c6 b1d1 d7c8 c3b3 g6f4", 49, 16, 20, 20, List(0, 1, 2, 3, 6, 10, 20), 20))))
    examples.foreach { (start, played, origin, destination, reports) =>
      val lines = reports.map((moves, cp, depth, _, _, _, _) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) = projectRoot(start, played, rows)
      val (packet, paths, root) = project(lines)
      val question = (root \ "detail" \ "summary_question").as[JsObject]
      assertEquals((question \ "question").as[String], "observed_route_use")
      val units = (question \ "units").as[List[JsObject]]
      def locate(ref: JsObject) = locateRef(paths, ref)
      val core = units.flatMap(unit => refs(unit("en"))).map(locate)
      val details = detailRefs(question).map(locate)
      val covered = units.flatMap(unit => refs(unit("covered_moves"))).map(locate)
      assertOwnedBilingualUnits(units ++ detailUnits(question), paths, Some(root))
      reports.foreach { (moves, _, _, arrival, through, required, _) =>
        val path = paths.paths.find(_.moves == moves.split(" ").toList).get
        val focus = paths.identitiesAfter(packet, path, 0).map(_._1).find(_.square.key == origin).get
        assertEquals(paths.pieceAfter(packet, path, focus, arrival + 1).flatten.map(_.square.key), Some(destination))
        required.foreach(offset => assert(core.contains(path -> offset), "the observed condition, evasion and cost stay visible"))
        assertEquals(details.filter(_._1 == path).map(_._2).toSet, (0 to through).toSet)
        val captures = path.certified.flatMap(_._2.material).get.captures.filter(_.plyOffset <= through)
        assertEquals(covered.filter(_._1 == path).map(_._2).sorted, captures.map(_.plyOffset))
        val exchanges = if played == "c3f3" then
          val expected = if moves.startsWith("c3f3") then List(1, 2, 4, 5, 6, 7) else List(2, 3, 5, 6, 7, 8)
          assertEquals(captures.map(_.plyOffset), expected)
          captures.foreach(event => assert(core.contains(path -> event.plyOffset),
            "the pawn gains, rook exchange and immediate countergain stay in the selected account"))
          if moves.startsWith("c3h3") then
            val otherRook = paths.identitiesAfter(packet, path, 0).map(_._1).find(_.square.key == "e8").get
            assertEquals(paths.pieceAfter(packet, path, focus, 8), Some(None))
            assertEquals(paths.pieceAfter(packet, path, otherRook, 9).flatten.map(_.square.key), Some("e4"))
          captures.filter(event => event.plyOffset == arrival || event.plyOffset == arrival + 1).grouped(2)
        else if played == "g6h4" && moves.startsWith("g6h4") then
          assertEquals(captures.map(_.plyOffset), List(2, 3, 6, 13, 14))
          required.foreach(offset => assert(core.contains(path -> offset)))
          List(13, 14).foreach(offset => assert(details.contains(path -> offset) && core.contains(path -> offset),
            "the original closed interval includes this intervening pawn exchange in its material account"))
          List(2, 3, 6).foreach(offset => assert(core.contains(path -> offset),
            "the delayed capture of the recapturing pawn completes the rook-for-knight-and-pawn account"))
          Iterator.empty
        else captures.grouped(2)
        exchanges.foreach { pair =>
          assertEquals(pair.size, 2)
          assertEquals(pair.last.plyOffset, pair.head.plyOffset + 1)
          assert(pair.last.recapture)
          assert(units.exists(unit => pair.forall(event => refs(unit("en")).map(locate).contains(path -> event.plyOffset))),
            "a capturing reply and its recapture are one visible account")
        }
      }
      val english = (root \ "detail" \ "summary").as[String]
      val facts = selectedFacts(packet, paths, root).core.flatMap(_.statements)
      val accounts = facts.collect { case value: CommentaryAnswer.Material => value }
      def account(first: String) = accounts.find(_.path.moves.head == first).get.cost
      def losses(rows: List[(EvidencePieceRole, Int)]) = rows.map((role, count) => role.name -> count).toMap
      def assertTrade(first: String, white: Map[String, Int], black: Map[String, Int]): Unit =
        val cost = account(first)
        assertEquals(losses(cost.whiteAdditionalLosses), white)
        assertEquals(losses(cost.blackAdditionalLosses), black)
      if played == "c5c3" then
        assertTrade("c5c3", Map("knight" -> 1), Map("bishop" -> 1))
        assertEquals(accounts.map(_.path).distinct.size, 2)
        accounts.foreach(value => assertEquals(losses(value.cost.commonExchanges).get("queen"), Some(1),
          "each branch exchanges both queens as well as retaining its different material cost"))
      if played == "b4c3" then
        assertTrade("b4c3", Map("bishop" -> 1), Map("knight" -> 1))
        val quiet = paths.paths.find(_.moves.head == "g7g6").get
        assert(!core.contains(quiet -> 9), "unrelated Be3 after the quiet arrival remains in detail")
      if Set("h6h5", "g6h4")(played) then
        val last = paths.paths.find(_.moves.head == played).get
        assert(facts.exists {
          case CommentaryAnswer.RecordedEnd(move) => (move.path eq last) && move.offset == last.moves.size - 1
          case _ => false
        }, "an unrecorded reply is an explicit finite horizon, not a terminal result")
        assert(!facts.exists(_.isInstanceOf[CommentaryAnswer.Terminal]))
      if played == "c3f3" then
        val escapes = facts.collect {
          case CommentaryAnswer.Action(move, MoveWorkProjection.Detail.Evades(actor, attacker))
              if actor.role.name == "rook" && !actor.side.white && attacker.side.white &&
                attacker.role.name == "pawn" && attacker.square.key == "b2" => move
        }
        assertEquals(escapes.map(_.path.moves.head).toSet, Set("c3f3", "c3h3"))
        assert(escapes.forall(_.offset == 0))
        assert(facts.exists {
          case value: CommentaryAnswer.TargetResponse => value.path.moves.head == "c3h3" &&
            value.originalTarget == Piece("white", "rook", "h1").witness && value.escape.contains(1)
          case _ => false
        }, "Rg1 answers the attack on the same original h1-rook")
        val follow = paths.paths.find(_.moves.head == "c3h3").get
        val countergain = paths.continuationReadings.find(_.path eq follow).get.events.find(_.plyOffset == 8).get
        assertEquals(countergain.originalActor, Some(Piece("black", "rook", "e8").witness))
        assertEquals(countergain.originalVictim, Some(Piece("white", "pawn", "e4").witness))
        assert(account("c3h3").events.exists(_.plyOffset == 8))
        assert(english.contains("Rg1") && english.contains("Rxe4"), english)
        assert(!english.toLowerCase.contains("forced"))
      if played == "g6h4" then
        assertTrade("c6c4", Map("bishop" -> 1), Map("knight" -> 1))
        assertTrade("g6h4", Map("knight" -> 1, "pawn" -> 1), Map("rook" -> 1))
        assert(english.contains("exd5") && english.contains("Bxd5"),
          "the accounted intervening pawn exchange remains visible: " + english)
      val (_, _, reordered) = project(lines.reverse)
      val reorderedQuestion = (reordered \ "detail" \ "summary_question").as[JsObject]
      assertEquals(summaryMeaning(reorderedQuestion), summaryMeaning(question))
      reports.filter(_._7 >= 0).foreach { (moves, _, _, _, _, _, cut) =>
        val (_, _, incomplete) = project(lines.map(line => if line.moves.head == moves.take(4) then line.copy(moves = line.moves.take(cut)) else line))
        assert((incomplete \ "detail" \ "summary_question" \ "question").asOpt[String] != Some("observed_route_use"),
          "a missing recapture or common arrival cannot retain the closed route account")
      }
    }

  test("a stationary recapturing pawn's later loss remains part of a closed answer"):
    // Opened Tata Steel 2023 counterexample to stopping at fxg5 fxg5.
    val start = "r5nr/1b2k3/p1npppp1/1p4Pp/4P3/2N1NP2/PPP1B1P1/2KR3R w - - 0 17"
    val lines = List(
      EngineLine("g5f6 g8f6 a2a4 b5b4 c3b1 a8d8 b1d2 g6g5 h1e1 h5h4 d2c4 a6a5 e2f1 g5g4 e3g4 f6g4 f3g4 h8g8 f1e2 c6e5 c4e5 d6e5 e1h1 d8d1 e2d1".split(" ").toList, 6, None, 18),
      EngineLine("f3f4 f6g5 f4g5 c6e5 d1d4 e5f7 h1d1 a8d8 a2a4 f7g5 a4b5 a6b5 e4e5 g5f7 c3b5 d6e5 d4c4 d8c8 c4c8".split(" ").toList, 0, None, 18))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = start, playedMoveUci = "f3f4", variations = lines)).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val path = paths.paths.find(_.moves == lines.last.moves).get
    val pawn = paths.identitiesAfter(packet, path, 0).map(_._1).find(_.square.key == "f3").get
    assert((3 to 9).forall(count => paths.pieceAfter(packet, path, pawn, count).flatten.exists(_.square.key == "g5")))
    assertEquals(paths.pieceAfter(packet, path, pawn, 10), Some(None))
    val root = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "root_comparison").get
    (root \ "detail" \ "summary_question").asOpt[JsObject] match
      case Some(question) =>
        val core = (question \ "units").as[List[JsObject]].flatMap(unit => (unit \ "en").as[List[JsValue]]).collect { case move: JsObject => move }
        assert(core.exists(ref => (ref \ "line_id").as[String] == path.line.ref.id &&
          (ref \ "occurrence_id").as[String] == path.ids(9)), "a complete exchange answer must retain Nxg5")
      case None => assert((root \ "detail" \ "text").as[String].contains("Nxg5"), "unresolved comparison retains the observed cost in detail")

  test("shared exchange questions preserve both branch accounts and reject an unexplained root connection"):
    // Opened master-game reports; this is a structural regression, not a new quality sample.
    val examples = List(
      ("3r1r1k/1b4pp/pp4n1/2pPNp1q/8/1B6/PP3PPP/R2QR1K1 b - - 3 24", "h5g5", true, List(
        ("h5g5 e5g6 h7g6 d1f3 b6b5 a1d1 d8d6 e1e5 c5c4 b3c2 g5f6 f3e3 h8g8 b2b3", 22, 16),
        ("h5h6 e5g6 h6g6 e1e6 g6f7 a1c1 f7c7 d5d6 c7c6 f2f3 b6b5 d6d7 c6d7 d1d7 d8d7 c1c5 g7g6 c5e5 f8d8 g1f2", 56, 16),
        ("h5d1 a1d1 g6e5 e1e5 b6b5 d5d6 c5c4 b3c2 g7g6 e5e7 b7c6 e7c7 c6d7 b2b3 c4b3 c2b3 f8f6", 75, 16))),
      ("r3k3/p2nnpp1/2pqp1p1/1p1p2Pr/3P3P/2P1PQ2/PP1NBP2/R3K2R b KQq - 4 16", "d7b6", true, List(
        ("e6e5 e1c1 h5h8 e3e4 e8c8 c1b1 c8b8 e4d5 d6d5 f3h3 e7f5 c3c4 d5d4 c4b5", 74, 16),
        ("a8b8 d2b3 h5h8 e1c1 c6c5 d4c5 d7c5 b3c5 d6c5 f3f4 b8c8 c1b1 a7a5 e3e4 d5e4", 75, 16),
        ("d7b6 e1c1 e7f5 e3e4 d5e4 f3e4 h5h8 h4h5 g6h5 e2h5 g7g6 h5f3 e8d7 d2b3 d7c7 b3c5 a7a5 c5d3 b6d7 c1b1 h8h1 d1h1 b5b4", 82, 16))),
      ("3r2k1/1qnRbp2/p5pp/2p1PQ2/1p3B1P/5N2/1PP2PP1/6K1 w - - 0 26", "f5d3", true, List(
        ("f5d3 b7c8 d7d8 c8d8 d3d8 e7d8 f4h6 a6a5 h6e3 c5c4 b2b3 c4b3 c2b3 c7d5 e3d4 d5c3 g1f1 a5a4 b3a4 c3a4 f1e2 b4b3 e2d3 d8a5 f3g5 g8f8 f2f4 a5b6 g5f3 b6d4 f3d4 b3b2 d3c2", 64, 18),
        ("d7d8 e7d8 f5d3 b7d5 d3d5 c7d5 f4h6 a6a5 b2b3 c5c4 g1f1 d5c3 h6d2 c4b3 c2b3 c3e4 d2e3 e4c3 e3d4 a5a4 b3a4 c3a4 f1e2 b4b3 e2d3 d8a5 f3g5 g8f8 f2f4 f8e7 g5f3 a5b4", 60, 18),
        ("f5g4 h6h5 g4h3 b7c8 d7e7 c8h3 g2h3 c7d5 e7b7 d5f4 f3g5 d8d1 g1h2 f4e6 g5f7 d1e1 h2g3 a6a5 c2c3 a5a4 c3b4 c5b4 f7g5 e6g5 b7b8 g8f7 b8b7", -7, 18))),
      ("r1b1k2r/pp1n1p1p/3qpp2/1B6/3NP2P/8/P4PP1/1R1Q1K1R w kq - 3 15", "h1h3", true, List(
        ("h1h3 a7a6 b5d7 c8d7 h3d3 a8d8 f1g1 d7b5 d3d2 e6e5 d4f3 d6e7 f3h2 h8g8 h2f1 d8d2 d1d2 e7d7 d2h6 b5f1 b1f1 d7h3 g2g3 g8g3 f2g3 h3g3 g1h1 g3h3 h1g1 h3g3", 0, 16),
        ("f1g1 a7a6 b5d7 d6d7 h1h3 e6e5 d4f5 d7d1 b1d1 c8f5 e4f5 e8e7 d1b1 b7b5 a2a4 h8b8 h3b3 b8b6 a4b5 a8b8 b1c1 e7f8", -23, 16),
        ("d4b3 d6d1 b1d1 a7a6 b5e2 b7b6 h1h3 c8b7 h3d3 a8d8 d3c3 b7e4 e2a6 e8g8 f2f3 e4d5 a2a4 f6f5 f1e1 d7e5", -71, 16))),
      ("r5nr/1b2k3/p1npppp1/1p4Pp/4P3/2N1NP2/PPP1B1P1/2KR3R w - - 0 17", "f3f4", false, List(
        ("g5f6 g8f6 a2a4 b5b4 c3b1 a8d8 b1d2 g6g5 h1e1 h5h4 d2c4 a6a5 e2f1 g5g4 e3g4 f6g4 f3g4 h8g8 f1e2 c6e5 c4e5 d6e5 e1h1 d8d1 e2d1", 6, 18),
        ("f3f4 f6g5 f4g5 c6e5 d1d4 e5f7 h1d1 a8d8 a2a4 f7g5 a4b5 a6b5 e4e5 g5f7 c3b5 d6e5 d4c4 d8c8 c4c8", 0, 18),
        ("a2a4 f6g5 a4b5 a6b5 e2b5 c6e5 b2b3 g8f6 c1b2 g5g4 f3f4 e5f7 d1d4 a8b8 f4f5 g6f5 e4f5 e6e5 d4a4", -1, 18))),
      ("b2qk2r/5ppp/2n1p3/1Nb1P3/8/1BP5/5PPP/2BQ1RK1 b k - 0 16", "e8g8", false, List(
        ("c6e5 d1d8 e8d8 c1g5 f7f6 g5e3 c5e3 f2e3 d8e7 f1a1 a8c6 a1a7 c6d7 b5d4 h8c8 b3e6 c8c3 e6d7 e5d7 d4f5 e7d8 a7a8 d8c7 f5g7 c3e3 h2h3 e3e7", 15, 18),
        ("d8d1 f1d1 c6e5 b5c7 e8e7 c1g5 f7f6 c7e6 c5f2 g1f2 f6g5 d1a1 h8b8 e6d4 e5d3 f2g3 h7h6 a1a7 e7f6 a7a6 f6e7 b3c2 d3f4 h2h4 a8g2", 21, 18),
        ("e8g8 d1d8 f8d8 c1f4 h7h6 h2h4 c6e7 f1d1 d8b8 c3c4 e7f5 h4h5 f5h4 b3c2 h4g2 f4g3 c5b4 g1f1 a8f3 d1a1 b8a8 a1a8 f3a8 b5d6 g2e1 c2e4", 28, 18))),
      ("r4rk1/1pp3p1/p1n1q2p/4pp2/P7/1PQPP2P/2PN2P1/R4RK1 w - - 0 17", "c3c4", false, List(
        ("f1f2 b7b6 a1f1 a8d8 c3c4 e6c4 d2c4 e5e4 d3d4 c6e7 c4e5 f8e8 h3h4 c7c5 d4c5 b6c5 e5c4 g7g6", 19, 17),
        ("b3b4 g8h7 a1b1 a8e8 c3c4 e6c4 d2c4 b7b5 a4b5 a6b5 c4a3 c6a7 c2c4 e8d8", 16, 17),
        ("a1b1 g8h7 b3b4 a8e8 c3c4 e6c4 d2c4 b7b6 b4b5 a6b5 a4b5 c6d8 f1f2", 14, 17),
        ("c3c4 e6c4 b3c4 c6b4 a1c1 a6a5 g2g4 f5f4 f1f3 b4a2 c1b1 b7b6 d2e4 a2b4 e3f4 e5f4 f3f2 g7g5", -3, 21))))
    examples.foreach { (start, played, accepted, reports) =>
      val lines = reports.map((moves, cp, depth) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) =
        val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = start, playedMoveUci = played, variations = rows)).get
        val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
        val paths = ObservedLineProtocol.prepare(packet)
        val root = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].find(value =>
          (value \ "detail" \ "type").as[String] == "root_comparison").get
        (paths, (root \ "detail" \ "summary_question").asOpt[JsObject])
      val (paths, question) = project(lines)
      assertEquals(question.exists(q => (q \ "question").as[String] == "contested_pair_response"), accepted)
      question.filter(_ => accepted).foreach { q =>
        val units = (q \ "units").as[List[JsObject]]
        def locate(ref: JsObject) = locateRef(paths, ref)
        val core = units.flatMap(u => refs(u("en"))).map(locate)
        val covered = units.flatMap(u => refs(u("covered_moves"))).map(locate)
        assertOwnedBilingualUnits(units, paths)
        val trades = covered.distinct.groupMap(_._1)(_._2).toList
        assertEquals(trades.size, 2, "both compared branches retain their entire selected account")
        val subjects = trades.map { (path, offsets) =>
          assertEquals(offsets.size % 2, 0)
          offsets.sorted.grouped(2).map(pair => assertPhysicalRecapture(paths, path, pair)).toSet
        }
        assertEquals(subjects.head, subjects.last, "both accounts exchange the same original physical pairs")
        core.map(_._1).distinct.foreach { p =>
          val end = covered.filter(_._1 == p).map(_._2).max
          assertEquals(covered.filter(_._1 == p).map(_._2).distinct.sorted,
            p.certified.flatMap(_._2.material).get.captures.filter(_.plyOffset <= end).map(_.plyOffset))
          val sans = paths.notation.line(p.moves)
          (0 to end).filter(n => sans(n).endsWith("+")).foreach(n =>
            List(n, n + 1).foreach(offset => assert(core.contains(p -> offset),
              "each checking exchange retains its actual reply on that branch")))
        }
        val (_, reversed) = project(lines.reverse)
        assertEquals(reversed.map(summaryMeaning), Some(summaryMeaning(q)))
        // Removing the observed recapture cannot leave the complete exchange answer.
        core.map(_._1).distinct.foreach { p =>
          val cut = covered.filter(_._1 == p).map(_._2).max
          val (_, incomplete) = project(lines.map(line => if line.moves.head == p.moves.head then line.copy(moves = line.moves.take(cut)) else line))
          assert(!incomplete.exists(q => (q \ "question").as[String] == "contested_pair_response"))
        }
      }
    }

  test("different first exchanges retain preceding trades, arrivals and recaptures in their own order"):
    // Opened master-game inputs; expected moves are the independently reviewed answer obligations.
    val examples = List(
      ("1r4k1/1p1n1pbp/p3p1p1/3pP3/P1qP4/1R1Q1P2/1P1BN1PP/6K1 w - - 2 26", "a4a5", Map("Qxc4" -> List(0, 1, 2, 5, 6, 7), "a5" -> List(0, 3, 4, 5, 6, 11, 12)), List(
        ("d3c4 d5c4 b3a3 g7f8 a3a1 b7b5 a4b5 a6b5 e2c3 f7f5 e5f6 d7f6 d2f4 b8b7 f4e5 f8g7 a1a8 g8f7 c3b5 b7b5 a8a7 f7f8 e5d6 f8e8 a7g7 b5b2", -9, 17),
        ("d2b4 g7f8 b4f8 g8f8 d3c4 d5c4 b3b4 b7b5 a4b5 b8b5 b4c4 b5b2 c4c8 f8g7 c8c7 b2e2 c7d7 e2e1", -12, 17),
        ("a4a5 g7f8 g1f2 b7b5 a5b6 d7b6 d3c3 g8g7 g2g4 f8e7 f3f4 c4c3 d2c3 b6d7 e2c1 b8b3 c1b3 d7b8 b3a5", -17, 17))),
      ("4rrk1/pppq2pp/2nb2n1/3p4/1P1P2p1/2PB1N2/P4PPP/R1BQR1K1 w - - 0 14", "d3g6", Map("Rxe8" -> List(0, 1), "Bxg6" -> List(0, 1, 2, 3)), List(
        ("e1e8 d7e8 f3e1 g6h4 c1d2 e8f7 d1e2 f7h5 g2g3 f8e8 d2e3 h4f3 e1f3 g4f3 e2f1 c6e7 e3d2 e7f5 d3b5 c7c6 b5d3", 53, 17),
        ("f3g5 e8e1 d1e1 f8f6 f2f3 h7h6 f3g4 h6g5 c1g5 f6e6 e1f1 g6e7 f1f2 d7e8 a1f1 a7a6", -4, 17),
        ("d3g6 h7g6 e1e8 f8e8 f3d2 g8h7 d2f1 c6d8 c1g5 d8f7 g5d2 e8e4 a2a4 c7c6", -8, 17))))
    examples.foreach { (start, played, expected, reports) =>
      val lines = reports.map((moves, cp, depth) => EngineLine(moves.split(" ").toList, cp, None, depth))
      def project(rows: List[EngineLine]) =
        val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = start, playedMoveUci = played, variations = rows)).get
        val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
        val paths = ObservedLineProtocol.prepare(packet)
        val detail = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].map(_ \ "detail").find(d =>
          (d \ "type").as[String] == "root_comparison").get
        (paths, (detail \ "summary_question").asOpt[JsObject])
      val (paths, question) = project(lines)
      assertEquals(question.map(q => (q \ "question").as[String]), Some("contested_pair_response"))
      val units = (question.get \ "units").as[List[JsObject]]
      def locate(ref: JsObject) = locateRef(paths, ref)
      val core = units.flatMap(u => refs(u("en"))).map(locate)
      val covered = units.flatMap(u => refs(u("covered_moves"))).map(locate)
      assertOwnedBilingualUnits(units, paths)
      covered.distinct.groupMap(_._1)(_._2).foreach { (path, offsets) =>
        assertEquals(offsets.size % 2, 0)
        offsets.sorted.grouped(2).foreach(pair => assertPhysicalRecapture(paths, path, pair))
      }
      core.map(_._1).distinct.foreach { p =>
        val root = paths.notation.line(p.moves).head
        assert(expected(root).forall(offset => core.contains(p -> offset)))
        val end = expected(root).max
        assertEquals(covered.filter(_._1 == p).map(_._2).distinct.sorted,
          p.certified.flatMap(_._2.material).get.captures.filter(_.plyOffset <= end).map(_.plyOffset))
        // A record cut before the first recapture cannot justify this complete exchange comparison.
        val firstRemoval = covered.filter(_._1 == p).map(_._2).sorted.apply(1)
        val (_, incomplete) = project(lines.map(line => if line.moves.head == p.moves.head then line.copy(moves = line.moves.take(firstRemoval)) else line))
        assert(!incomplete.exists(q => (q \ "question").as[String] == "contested_pair_response"))
      }
      val (_, reversed) = project(lines.reverse)
      assertEquals(reversed.map(summaryMeaning), question.map(summaryMeaning))
    }

  test("an early blockade keeps counterattacks and distinguishes local evasion from remaining support"):
    val examples = List(
      ("r3r1k1/ppp2p2/2np1qp1/2n4p/4P3/1P3B2/P4PPP/1QBRR1K1 w - - 0 21", "c1b2", List(
        ("c1b2 c6e5 f3e2 h5h4 b1c1 c5e6 g2g3 g6g5 e2g4 e8e7 e1e3 a8e8 g3h4 f6f4 e3g3", 15, 16),
        ("e1e3 c6d4 c1b2 c5e6 f3e2 c7c5 e2c4 h5h4 b1a1 a8c8 h2h3 a7a6 c4e6 e8e6 a2a4 c8e8", -13, 16),
        ("b3b4 c5e6 c1b2 c6d4 f3e2 c7c5 e2c4 e6f4 c4f1 h5h4 b1c1 a8c8 c1e3 g6g5 b2d4 c5d4 e3d4", -17, 16))),
      ("3r1rk1/5ppp/p1q2b2/5P2/2p1p3/P1P5/BP3PPP/R3QRK1 w - - 1 22", "e1e2", List(
        ("a1d1 d8d3 a2b1 f8e8 e1e2 g7g6 b1d3 c4d3 e2e3 g6f5", 28, 16),
        ("e1e2 d8d3 a1e1 f8e8 e2g4 f6d8 a2b1 c6d5 b1d3 c4d3 e1e3 d8b6 e3h3 d5e5 f1d1 a6a5 d1e1 a5a4 g1f1 b6f2", 9, 16),
        ("e1e3 d8d3 e3f4 f8e8 a1e1 f6d8 a2b1 h7h6 b1c2 c6d5 e1e2 d8g5 f4g4 e4e3 f2e3 d3e3 e2e3 g5e3 g1h1", 1, 16))))
    def project(example: (String, String, List[(String, Int, Int)]), reverse: Boolean = false, short: Boolean = false) =
      val (fen, played, reports) = example
      val lines = reports.map { (moves, cp, depth) =>
        val original = moves.split(" ").toList
        EngineLine(if short && original.head == "c1b2" then original.take(2) else original, cp, None, depth)
      }
      val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen, playedMoveUci = played, variations = if reverse then lines.reverse else lines)).get
      val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
      val paths = ObservedLineProtocol.prepare(packet)
      val detail = (MoveWorkProjection.select(packet, paths).json \ "candidates").as[List[JsObject]].map(_ \ "detail").find(d =>
        (d \ "type").as[String] == "root_comparison").get
      (paths, (detail \ "summary_question").asOpt[JsObject])
    val (paths, question) = project(examples.head)
    assertEquals(question.map(q => (q \ "question").as[String]), Some("opening_counterplay"))
    val units = (question.get \ "units").as[List[JsObject]]
    val expected = Map("Bb2" -> List(0, 1, 2), "Re3" -> List(0, 1, 2))
    assertOwnedBilingualUnits(units, paths)
    units.foreach { unit =>
      val located = refs(unit("en")).map(ref => locateRef(paths, ref))
      assertEquals(located.map(_._1).distinct.size, 1)
      val p = located.head._1
      val root = paths.notation.line(p.moves).head
      assertEquals(located.map(_._2).distinct.sorted, expected(root))
      assertEquals(unit("covered_moves").as[List[JsObject]], Nil)
      if root == "Re3" then
        val text = unit("en").as[List[JsValue]].collect { case JsString(s) => s }.mkString
        assert(text.contains("b3") && text.contains("f3") && text.contains("support"))
        // Be2 at offset 4 stays under Nd4 attack; it cannot be called the escape here.
        assert(!located.exists(_._2 == 4))
    }
    assertEquals(project(examples.head, reverse = true)._2.map(summaryMeaning), question.map(summaryMeaning))
    assert(!project(examples.head, short = true)._2.exists(q => (q \ "question").as[String] == "opening_counterplay"))
    // The other master's c3 counterattack is answered by a sacrifice account, not an evasion.
    assert(!project(examples.last)._2.exists(q => (q \ "question").as[String] == "opening_counterplay"))

  test("a shared action keeps recovery conditions and the order of a shared counterattack"):
    val examples = List(
      ("r2q1rk1/pb3ppp/1pn1p3/8/2BPP3/5N2/P2Q1PPP/3R1RK1 w - - 2 14", "d2f4", List(
        ("f1e1 c6a5 c4f1 d8d6 d4d5 e6d5 e4e5 d6e7 f3d4 b7c8 f2f4 c8g4 d1c1 a8c8 f4f5 c8c1 d2c1 e7c5", 70, 16),
        ("f1e1 h7h6", 70, 16),
        ("h2h4 a8c8 f1e1 h7h6 d2f4 d8c7 f4g4 c8d8 c4b3 c6a5 f3e5 a5b3 a2b3 a7a5 d1d3 f7f5 e4f5 e6f5 g4f4 b7d5 d3g3", 43, 16),
        ("d2f4 d8f6 f4e3 h7h6 f1e1 a8c8 e4e5 f6e7 d4d5 e6d5 c4d5 c8d8 a2a3 c6a5 d5a2 d8d1 e1d1 f8d8 d1d8 e7d8 h2h4", 30, 20))),
      ("r4rk1/3qbpp1/p1bp3p/n3p2n/p2PP2B/5N2/1P1N1PPP/RB1QR1K1 w - - 2 18", "h4e7", List(
        ("b2b4 a5b3 d2b3 a4b3 d4e5 f8d8 h4e7 d7e7 e5e6 f7e6 f3d4 c6e8 d1b3", 53, 16),
        ("b1c2 c6b5 h4e7 d7e7 c2a4 h5f4 e1e3 a5c6 a4b5 a6b5 a1a8 f8a8 d4d5 c6d4 g2g3 d4f3 e3f3 f4h3 g1g2 e7d7 f3a3 a8a3 b2a3 h3g5 h2h4 g5h7", 25, 16),
        ("h4e7 d7e7 b2b4 a5b3 d2b3 a4b3 d1b3 c6b5 g2g3 h5f6 f3h4 f8e8 h4f5 e7d7 b3f3 g7g6 d4e5 d6e5", 10, 16))),
      ("r3r3/qpp2pk1/3p1nnp/p3p1pb/P2PP3/1RP1RNBP/1PB2PP1/3Q2K1 w - - 5 22", "d1d2", List(
        ("d1d2 e8e7 h3h4 f6h7 f3h2 f7f6 h4g5 h6g5 f2f3 g6f4 g3f2 h5e8 b3a3 e8d7", 38, 16),
        ("d1f1 g6f4 h3h4 h5f3 e3f3 c7c6 f1d1 b7b6 g3h2 a8d8 c3c4 c6c5 d4d5 a7d7 f3g3", 22, 16),
        ("d1d3 e8e7 b3b5 a8e8 b2b4 b7b6 b4a5 b6a5 d3c4 a7a6 f3d2", 17, 16))))
    def project(example: (String, String, List[(String, Int, Int)]), reverse: Boolean = false, cut: String = "") =
      val (fen, played, reports) = example
      val lines = reports.map { (moves, cp, depth) =>
        val original = moves.split(" ").toList
        val short = if cut == "reply" && original.head == "f1e1" then original.take(7)
          else if cut == "recapture" && original.head == "d2f4" then original.take(10)
          else original
        EngineLine(short, cp, None, depth)
      }
      val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen, playedMoveUci = played, variations = lines)).get
      val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
      val paths = ObservedLineProtocol.prepare(packet)
      val projection = MoveWorkProjection.select(packet, paths)
      val base = (projection.baseJson \ "candidates").as[List[JsObject]].find(c =>
        (c \ "detail" \ "type").as[String] == "root_comparison").get
      // Engine report order can choose a different same-root variation. Only
      // reverse the unordered pair enumeration while keeping its owned lines.
      val candidate = if reverse then base + ("line_ids" -> JsArray((base \ "line_ids").as[List[JsValue]].reverse)) else base
      val detail = projection.withMoveEffects(candidate) \ "detail"
      (paths, (detail \ "summary_question").asOpt[JsObject])
    val (paths, question) = project(examples.head)
    assertEquals(question.map(q => (q \ "question").as[String]), Some("opening_counterplay"))
    val units = (question.get \ "units").as[List[JsObject]]
    val expected = Map("Rfe1" -> List(0, 1, 2, 4, 5, 6, 7), "Qf4" -> List(0, 6, 7, 8, 9, 10))
    val costs = Map("Rfe1" -> List(5), "Qf4" -> List(9, 10))
    val owned = units.flatMap(u => u("en").as[List[JsValue]].collect { case r: JsObject => (r \ "line_id").as[String] }).distinct
    val selected = paths.paths.filter(p => owned.contains(p.line.ref.id))
    assertEquals(selected.map(p => paths.notation.line(p.moves).head).toSet, expected.keySet)
    assertEquals(selected.size, 2)
    selected.foreach { p =>
      val root = paths.notation.line(p.moves).head
      def offsets(refs: List[JsObject]) = refs.filter(r => (r \ "line_id").as[String] == p.line.ref.id)
        .map(r => p.ids.indexOf((r \ "occurrence_id").as[String])).distinct.sorted
      val mentioned = units.flatMap(u => u("en").as[List[JsValue]].collect { case r: JsObject => r })
      assertEquals(offsets(mentioned), expected(root))
      assertEquals(offsets(units.flatMap(_("covered_moves").as[List[JsObject]])), costs(root))
    }
    assertOwnedBilingualUnits(units, paths)
    val words = units.flatMap(_("en").as[List[JsValue]].collect { case JsString(s) => s }).mkString
    assert(words.contains("bishop originally on c4") && words.contains("recaptures"), words)
    val recovered = paths.paths.find(_.moves.head == "d2f4").get
    val recovery = paths.continuationReadings.find(_.path eq recovered).get.events.find(_.plyOffset == 10).get
    assertEquals(recovery.originalActor.map(piece => (piece.role.name, piece.square.key)), Some("bishop" -> "c4"),
      "the stationary c4-bishop, rather than the retreated bishop in the other line, recaptures")
    assertEquals(project(examples.head, reverse = true)._2.map(summaryMeaning), question.map(summaryMeaning))
    for cut <- List("reply", "recapture") do
      assert(!project(examples.head, cut = cut)._2.exists(q => (q \ "question").as[String] == "opening_counterplay"))
    // A delayed Qxb3 is still recovery; h4's Bxf3 response has a different subject.
    examples.tail.foreach(example => assert(!project(example)._2.exists(q => (q \ "question").as[String] == "opening_counterplay")))

  test("a threatened subject can evade or be exchanged without owning the root move"):
    val examples = List(
      (("r3r3/qpp2pk1/3p1nnp/p3p1pb/P2PP3/1RP1RNBP/1PB2PP1/3Q2K1 w - - 5 22", "d1d2", List(
        ("d1d2 e8e7 h3h4 f6h7 f3h2 f7f6 h4g5 h6g5 f2f3 g6f4 g3f2 h5e8 b3a3 e8d7", 38, 16),
        ("d1f1 g6f4 h3h4 h5f3 e3f3 c7c6 f1d1 b7b6 g3h2 a8d8 c3c4 c6c5 d4d5 a7d7 f3g3", 22, 16),
        ("d1d3 e8e7 b3b5 a8e8 b2b4 b7b6 b4a5 b6a5 d3c4 a7a6 f3d2", 17, 16))), Map("Qd2" -> List(0, 4), "Qf1" -> List(0, 3, 4)), Map("Qd2" -> List(), "Qf1" -> List(3, 4))),
      (("r4rk1/p2q3p/2pbpp2/2p1p3/4N3/3P1Q2/PPP3PP/R4R1K b - - 1 17", "f6f5", List(
        ("d6e7 a1e1 g8h8 e4d2 f8g8 e1e4 d7d5 f3e2 g8g7 d2f3 a8g8", 9, 16),
        ("f6f5 e4d6 d7d6 f1e1 g8h8 f3e3 c5c4 a1d1 a8d8 h1g1 c4d3 d1d3 d6c7 e3e5 c7e5 e1e5 d8d3 c2d3 f8d8 g1f2 d8d3 e5e6", 33, 16),
        ("d7e7 e4f6 g8h8 f3c6 a8c8 c6a6 e5e4 f6e4 d6e5 c2c3 c8b8 f1f8 b8f8 a6c6 f8b8 c6c5 e7c5 e4c5 b8b2 d3d4 e5d6 c5e6 b2e2 e6g5 h8g7 g5f3 h7h5", 249, 16))), Map("Be7" -> List(0), "f5" -> List(0, 1, 2)), Map("Be7" -> List(), "f5" -> List(1, 2))),
      (("r1bq1rk1/1p1nbppp/3p4/p2Pp3/P4P2/1N1Q4/1PP1B1PP/R1B2RK1 w - - 1 14", "g1h1", List(
        ("f4f5 e7g5 c1g5 d8g5 a1a3 b7b6 d3d2 g5h4 d2e3 d7f6 c2c4 h4e4 b3d2 e4e3 a3e3 c8d7", 27, 16),
        ("g1h1 e7f6 c2c3 d7b6 d3e4 g7g6 e2d3 e5f4 c1f4 f6e5 f4e5 d6e5 e4e5 d8d5 e5d5 b6d5 d3b5 d5e3", 22, 16),
        ("f4f5 e7g5", 27, 16))), Map("f5" -> List(0), "Kh1" -> List(0, 7, 8)), Map("f5" -> List(), "Kh1" -> List(7, 8))))
    def project(example: (String, String, List[(String, Int, Int)]), reverse: Boolean = false,
        cut: Option[(String, Int)] = None) =
      val (fen, played, reports) = example
      val lines = reports.map { (text, cp, depth) =>
        val moves = text.split(" ").toList
        EngineLine(cut.filter(_._1 == moves.head).fold(moves)(c => moves.take(c._2)), cp, None, depth)
      }
      val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen, playedMoveUci = played, variations = lines)).get
      val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
      val paths = ObservedLineProtocol.prepare(packet)
      val projection = MoveWorkProjection.select(packet, paths)
      val base = (projection.baseJson \ "candidates").as[List[JsObject]].find(c =>
        (c \ "detail" \ "type").as[String] == "root_comparison").get
      val candidate = if reverse then base + ("line_ids" -> JsArray((base \ "line_ids").as[List[JsValue]].reverse)) else base
      val rendered = projection.withMoveEffects(candidate)
      (paths, (rendered \ "detail" \ "summary_question").asOpt[JsObject], packet -> rendered)
    examples.foreach { (example, expected, costs) =>
      val (paths, result, (packet, root)) = project(example)
      assertEquals(result.map(q => (q \ "question").as[String]), Some("contested_pair_response"))
      val units = (result.get \ "units").as[List[JsObject]]
      val mentioned = units.flatMap(unit => refs(unit("en")))
      val owned = mentioned.map(r => (r \ "line_id").as[String]).distinct
      assertEquals(owned.size, 2)
      assertOwnedBilingualUnits(units, paths)
      val presence = selectedFacts(packet, paths, root).core.flatMap(_.statements).collect {
        case value: CommentaryAnswer.PiecePresence => value
      }
      paths.paths.filter(p => owned.contains(p.line.ref.id)).foreach { p =>
        val root = paths.notation.line(p.moves).head
        def offsets(rs: List[JsObject]) = rs.filter(r => (r \ "line_id").as[String] == p.line.ref.id)
          .map(r => p.ids.indexOf((r \ "occurrence_id").as[String])).distinct.sorted
        assert(expected(root).forall(offsets(mentioned).contains), "the original evasion and recapture remain visible")
        val extra = offsets(mentioned).filterNot(expected(root).contains)
        extra.foreach { offset =>
          val fact = presence.find(value => (value.after.path eq p) && value.after.offset == offset)
            .getOrElse(fail("an additional core move must own the observed survival fact"))
          assertEquals(offset, p.moves.size - 1)
          assertEquals(paths.pieceAfter(packet, p, fact.original, offset + 1), Some(Some(fact.current)),
            "the end reference preserves the same physical subject, without absorbing unrelated trades")
        }
        assertEquals(offsets(units.flatMap(unit => refs(unit("covered_moves")))), costs(root))
        // A stopped record cannot assert the missing recapture or an unobserved reply to the evasion.
        val cutAt = if costs(root).nonEmpty then costs(root).max else expected(root).max + 1
        assert(!project(example, cut = Some(p.moves.head -> cutAt))._2.exists(q =>
          (q \ "question").as[String] == "contested_pair_response"))
      }
      assertEquals(project(example, reverse = true)._2.map(summaryMeaning), result.map(summaryMeaning))
    }
    val negatives = List(
      ("2r4r/1p2N1k1/p1bp2p1/7p/2PR1P2/6n1/PP2B1P1/3K1R2 w - - 2 26", "f1e1", List(
        ("e7c8 g3f1 e2f1 h8c8 b2b4 c8d8 b4b5 c6e8 a2a4 a6b5 c4b5 b7b6 d4c4 g7f6 f1d3 e8f7", 43, 16),
        ("f1f2 c8c7 e7c6 b7c6 e2d3 h8d8 f2f3 h5h4 f4f5 g6g5 f5f6 g7f7 b2b4 d6d5 a2a3", 17, 16),
        ("f1e1 g3f5 e7f5 g6f5 d4d6 h5h4 e2f1 c6e4 e1e3 g7f7 d6d7 f7f6 d1e1 c8g8 d7d6 f6f7 c4c5 g8g4", 7, 16))),
      ("2r3k1/2q1rppp/4p3/1p2N3/p2PQ3/1B4RP/P4PP1/6K1 w - - 0 24", "b3d1", List(
        ("b3d1 f7f6 e5d3 c7d6 e4h4 c8c3 d1e2 b5b4 h4e4 e7a7 d3b4 f6f5 e4e5 d6e5 d4e5 c3g3 f2g3 a7b7", 53, 17),
        ("e4h4 a4b3 h4f6 g7g6 a2b3 e7d7 e5g4 c7c1 g1h2 c1c7", 0, 17),
        ("e4f3 f7f6 f3f6 a4b3 a2b3 c7d6 f6g5 c8f8 e5g4 e7c7 g5b5 d6d4 b5e2 c7c1 g1h2 g8h8 e2e6", -78, 17))),
      ("1qr2rk1/1b1nppbp/pp1p1np1/8/2P5/1PN1PN2/PBR1BPPP/Q4RK1 w - - 0 16", "c2d2", List(
        ("f1d1 f8e8 f3e1 g7f8 e1d3 f6e4 f2f3 e4c3 b2c3 b7c6 a1c1 b6b5 d3b4 b5c4 e2c4 c6b5 c4b5 b8b5", 19, 19),
        ("f1c1 b8a8 f3e1 f8d8 c3d5 f6d5 b2g7", 16, 19),
        ("f1e1 b8a8 c2d2 c8c7 c3d5 b7d5 d2d5 f6d5 b2g7 d5f6 g7f8 a8f8 f3d4 f8c8 h2h3 d7c5 e1d1 c5e4", 8, 19),
        ("c2d2 c8c7 f1d1 f8c8 f3e1 f6e8 d2c2 d7c5 h2h3 e7e6 e2f1 b7c6 d1c1 c5d7 e1f3 c6f3 g2f3", 3, 22))))
    // Earlier material, delayed recovery, and a subject that leaves and returns are different questions.
    negatives.foreach(example => assert(!project(example)._2.exists(q =>
      (q \ "question").as[String] == "contested_pair_response")))
    val bothExchange = ("3r1r1k/1b4pp/pp4n1/2pPNp1q/8/1B6/PP3PPP/R2QR1K1 b - - 3 24", "h5g5", List(
        ("h5g5 e5g6 h7g6 d1f3 b6b5 a1d1 d8d6 e1e5 c5c4 b3c2 g5f6 f3e3 h8g8 b2b3", 22, 16),
        ("h5h6 e5g6 h6g6 e1e6 g6f7 a1c1 f7c7 d5d6 c7c6 f2f3 b6b5 d6d7 c6d7 d1d7 d8d7 c1c5 g7g6 c5e5 f8d8 g1f2", 56, 16),
        ("h5d1 a1d1 g6e5 e1e5 b6b5 d5d6 c5c4 b3c2 g7g6 e5e7 b7c6 e7c7 c6d7 b2b3 c4b3 c2b3 f8f6", 75, 16)))
    val (pairedPaths, unchanged, (pairedPacket, pairedRoot)) = project(bothExchange)
    val answer = selectedFacts(pairedPacket, pairedPaths, pairedRoot)
    val evasions = answer.core.flatMap(_.statements).collect {
      case CommentaryAnswer.Action(move, MoveWorkProjection.Detail.Evades(queen, attacker)) =>
        (move, queen, attacker)
    }
    assertEquals(evasions.map(_._1.path.moves.head).toSet, Set("h5g5", "h5h6"),
      "both root moves preserve their queen-evasion condition before the later shared exchange")
    evasions.foreach { (move, queen, attacker) =>
      assertEquals(move.offset, 0)
      assertEquals(attacker, Piece("white", "queen", "d1").witness)
      assertEquals(pairedPaths.pieceAfter(pairedPacket, move.path, Piece("black", "queen", "h5").witness, 1),
        Some(Some(queen)))
      assert(coreUnits(unchanged.get).flatMap(unit => refs(unit("en"))).contains(move.json))
    }
    assert((pairedRoot \ "detail" \ "summary").as[String].contains("queen on d1"),
      "the opposing queen remains an actual displayed condition")
