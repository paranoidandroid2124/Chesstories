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
    assert(text.contains("keeping the pawn on c3 to support it on d4, under attack from Black rook on d8"), text)
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
    val departure = text.indexOf("Ng4")
    val reply = text.indexOf("Rxd5 ", departure)
    val recovery = text.indexOf("Qxe4:", reply)
    assert(departure >= 0 && reply > departure && recovery > reply, text)
    assert(text.contains("the White knight on e3 leaves e3 with Ng4"), text)
    val consequences = text.substring(reply)
    assert(consequences.contains("moves the initially attacked Black rook on e5 out of White knight on g4's attack"), text)
    assert(consequences.contains("Black rook on e5 ends its support of Black knight on e4"), text)
    assert(consequences.contains("Black rook on e5 captures White pawn on d5"), text)
    assert(consequences.contains("This follows the end of Black rook on d5's support"), text)
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
    context.copy(text = tactic.text(typedBookComposition._1.notation, context.tacticScope, includeQualification = false),
      tactic = Some(tactic), attack = context.attack.map(_.copy(moves = tactic.line,
        contacts = tactic.contacts.map((actor, target, _) => actor -> target), cause = Some(tactic.actor))))

  private def composedContexts(contexts: List[MoveWorkProjection.Context]): JsObject =
    val (_, projection, candidate, _, explained) = typedBookComposition
    projection.withMoveEffects(candidate, explained = explained, context = contexts)

  private def occurrencesOf(text: String, phrase: String): Int = text.sliding(phrase.length).count(_ == phrase)

  test("a question's owned attack keeps its observed reply and recovery ahead of independent move effects"):
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
    assert(text.startsWith(reserved), "the engine answer remains intact")

  test("same-line context cannot gain answer priority from a different question, comparison or opaque wording"):
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
    val invalid = otherOwners.map(owner => revisedContext(current, tactic.copy(owner = Some(owner -> lineId)))) :+
      current.copy(text = current.text + " This separate condition must also remain.")
    invalid.foreach { context =>
      val result = projection.withMoveEffects(constrained, context = List(context))
      val text = (result \ "detail" \ "text").as[String]
      assert(!text.contains("Ng4:"), "a same-line fact without this question's ownership must not displace its existing explanations: " + text.drop(reserved.length))
      assert(text.startsWith(reserved) && text.length <= 4000)
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

  test("an opaque extra condition is preserved instead of being rewritten as a typed duplicate"):
    val original = typedBookComposition._4.find(_.tactic.exists(_.line == reference.take(5))).get
    val later = typedBookComposition._4.find(_.tactic.exists(_.line == reference.take(7))).get
    val opaque = original.copy(text = original.text + " " + later.text)
    val result = composedContexts(List(opaque, original))
    val text = (result \ "detail" \ "text").as[String]
    assert(text.contains(opaque.text), text)
    assertEquals(occurrencesOf(text,
      "the white knight originally on f3 attacks the black queen originally on d7 on d7"), 1, text)
    assertEquals(occurrencesOf(text, "the White knight on e5 attacks the Black queen on d7"), 1, text)

  test("a context rejected by the real text limit cannot consume the following complete context"):
    val original = typedBookComposition._4.find(_.tactic.exists(_.line == reference.take(5))).get
    val (_, _, observations) = fixedJob
    val candidate = typedBookComposition._3
    val card = (observations \ "selection" \ "candidates").as[List[JsObject]].filter(value =>
      Set("root_comparison", "question_comparison")((value \ "detail" \ "type").as[String]) &&
        (value \ "line_ids").as[List[String]].toSet == (candidate \ "line_ids").as[List[String]].toSet)
    val oversized = original.copy(text = card.map(value => (value \ "detail" \ "text").as[String]).mkString(" "))
    val baseline = composedContexts(Nil)
    assert(oversized.text.length > 4000, "use the actual card's prose to exceed one field even before optional supplements")
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
