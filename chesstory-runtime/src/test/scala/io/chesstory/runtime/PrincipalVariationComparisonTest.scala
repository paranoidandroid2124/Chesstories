package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{ MoveObservationMeaning, MoveReviewAssembler, MoveReviewInputAdmission }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.*
import play.api.libs.json.*

/** Synthetic matched reports test reading and provenance; no engine is executed. */
class PrincipalVariationComparisonTest extends munit.FunSuite:
  import PrincipalVariationComparison.*
  import CommentaryInvestigation.EngineCandidates

  private val initial = chess.variant.Standard.initialFen.value
  private val opening = "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"

  private def batch(fen: String, actual: String, rows: List[(List[String], Int, Int)],
      id: String = "pv-reader", start: String = initial, prefix: List[String] = Nil)
      : (EvidenceBackedJudgmentPacket, EngineCandidates) =
    scoredBatch(fen, actual, rows.map { (moves, cp, depth) =>
      (moves, ReportedWhiteEngineScore.Centipawns(cp), depth)
    }, id, start, prefix)

  private def scoredBatch(fen: String, actual: String, rows: List[(List[String], ReportedWhiteEngineScore, Int)],
      id: String = "pv-reader", start: String = initial, prefix: List[String] = Nil)
      : (EvidenceBackedJudgmentPacket, EngineCandidates) =
    val history = CanonicalPositionHistory.from(start, prefix, fen).toOption.get
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, id,
      EngineWorkPurpose.RootSearch, start, prefix, fen, EngineRootRestriction.Unrestricted,
      fen, Nil, EngineSearchLimits(16, 5_000_000, 5_000, rows.size), 6_000)
    val reports = rows.map { (moves, score, depth) =>
      ReportedEngineLineSuffix(moves, score, depth)
    }
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reports).get
    val lines = EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get
    val input = MoveReviewInputAdmission.retainAdmittedLines(
      MoveReviewInputAdmission.admitIndependent(history, actual).get, lines, EvaluationSource.RootSearch)
    EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get -> EngineCandidates(work, lines)

  private def openingBatch(id: String = "opening", bishopScore: Int = 80, knightDepth: Int = 16) =
    batch(opening, "f1b5", List(
      (List("f1c4", "g8f6"), bishopScore, 16),
      (List("f1b5", "a7a6"), 70, 16),
      (List("b1c3", "g8f6"), 60, knightDepth)), id,
      prefix = List("e2e4", "e7e5", "g1f3", "b8c6"))

  private def deliveredPair(packet: EvidenceBackedJudgmentPacket, receipt: EngineCandidates, reference: String): JsObject =
    val readings = read(packet, reference, List(receipt))
    assertEquals(readings.size, 1)
    val before = readings.head.candidateJson(packet)
    val selection = (RuntimeProtocol.observationsJson(packet, pvComparisons = readings) \ "selection").as[JsObject]
    assertEquals((selection \ "version").as[Int], 2)
    val candidates = (selection \ "candidates").as[List[JsObject]]
    assert(candidates.forall(candidate => (candidate \ "state").toOption.isEmpty))
    assert((selection \ "scenes").as[List[JsObject]].forall(scene => (scene \ "state").toOption.isEmpty))
    val delivered = candidates.find(value => (value \ "id").as[String] == readings.head.id).get
    assertEquals(delivered - "detail" - "evidence_ids", before - "detail" - "evidence_ids",
      "presentation cannot change pair ownership, selection or engine sources")
    assertEquals((delivered \ "detail").as[JsObject] - "text" - "effects",
      (before \ "detail").as[JsObject] - "text" - "effects",
      "the investigation relation, participants and priority inputs remain unchanged")
    assertEquals((delivered \ "evidence_ids").as[List[String]], (before \ "evidence_ids").as[List[String]],
      "the existing PV receiver requires exactly the two line-summary proof IDs")
    assert((delivered \ "detail" \ "text").as[String].length <= 1000)
    (delivered \ "evidence_ids").as[List[String]].foreach { id =>
      assert(packet.evidenceGraph.byId.get(id).exists(packet.evidenceGraph.proofEligible), id)
    }
    val prepared = ObservedLineProtocol.prepare(packet)
    val candidateLines = (delivered \ "line_ids").as[List[String]].toSet
    val relationSources = packet.evidenceGraph.byId.valuesIterator.collect {
      case record @ EvidenceRecord(_, fact: RelationFactEvidence, _) if packet.evidenceGraph.proofEligible(record) =>
        fact.semanticId
    }.toSet
    (delivered \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil).foreach { effect =>
      val lines = (effect \ "line_ids").as[List[String]]
      val through = (effect \ "through_plies").as[List[Int]]
      val prefix = (effect \ "prefix_moves_uci").as[List[String]]
      assert(lines.nonEmpty && lines.forall(candidateLines))
      assertEquals(lines.size, through.size)
      assertEquals(prefix, (before \ "detail" \ "prefix_moves_uci").asOpt[List[String]].getOrElse(Nil))
      val occurrences = lines.zip(through).flatMap { (id, count) =>
        val path = prepared.path(packet, id).get
        assert(count > prefix.size && count <= path.moves.size)
        assertEquals(path.moves.take(prefix.size), prefix)
        path.ids.take(count)
      }.toSet
      assert((effect \ "occurrence_ids").as[List[String]].forall(occurrences))
      (effect \ "evidence_ids").as[List[String]].foreach { id =>
        assert(packet.evidenceGraph.byId.get(id).exists(packet.evidenceGraph.proofEligible) || relationSources(id), id)
      }
    }
    delivered

  test("the current capture includes a consecutive countercapture on another piece in its opening exchange"):
    val fen = "2rr4/1p2q1pk/3Rp3/p1PnNbp1/P1B5/4R2P/3Q1PP1/6K1 w - - 3 43"
    val moves = List("c4d5", "d8d6", "c5d6", "e7d6", "g2g4", "d6d5", "d2d5", "e6d5",
      "g4f5", "d5d4", "e3d3", "c8c5", "e5f3", "c5f5", "g1f1")
    def observation(returned: List[String], supplied: List[String] = Nil) =
      val (base, receipt) = batch(fen, "c4d5", List(
        (returned, 314, 16), (List("e3e1", "d8d6", "c5d6", "e7d6"), 208, 16)), start = fen)
      val packet = if supplied.isEmpty then base else
        val admitted = MoveReviewInputAdmission.admitIndependent(base.assembly.input.positionHistory, "c4d5",
          suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, supplied))).get
        val input = MoveReviewInputAdmission.retainAdmittedLines(admitted, receipt.lines, EvaluationSource.RootSearch)
        EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
      val path = ObservedLineProtocol.prepare(packet).certifiedPath(packet, returned).get
      assertEquals(path.moves, returned)
      path.certified.get._2.material.get
    val actions = List(
      "White bishop captures the black knight on d5 with c4d5",
      "Black rook captures the white rook on d6 with d8d6",
      "White pawn recaptures the black rook on d6 with c5d6",
      "Black queen recaptures the white pawn on d6 with e7d6")
    def opening(actions: List[String]) =
      s"the opening exchange is: ${actions.mkString("; ")}. " +
        "This is the observed sequence, not a forced line."
    val material = observation(moves)
    assertEquals(material.captures.take(4).map(_.moveUci), moves.take(4))
    assertEquals(material.captures.take(4).map(_.recapture), List(false, false, true, true))
    assertEquals(material.captures.drop(4).head.plyOffset, 5, "g2g4 separates the next transaction")
    val exchange = CommentarySelection.openingExchange(material, moves)
    assertEquals(exchange.map(_._2), Some(opening(actions)))
    assertEquals(exchange.map(_._1.map(_.plyOffset)), Some(List(0, 1, 2, 3)))
    assert(exchange.forall(_._2.length <= 1400))
    val shortMaterial = observation(moves.take(2), supplied = moves)
    val shortExchange = CommentarySelection.openingExchange(shortMaterial, moves.take(2))
    assertEquals(shortExchange.map(_._2), Some(opening(actions.take(2))))
    assertEquals(shortExchange.map(_._1.map(_.plyOffset)), Some(List(0, 1)))

  test("the same third-piece obstruction in both candidates is not a comparison difference"):
    val fen = "q6k/8/8/8/2N5/8/7K/R7 w - - 0 1"
    val (packet, receipt) = batch(fen, "c4a3", List(
      (List("c4a3"), 40, 16), (List("c4a5"), 35, 16)), start = fen)
    val local = MoveWorkProjection.fromPacket(packet).map(value => (value \ "text").as[String])
    assertEquals(local.count(_.contains("Blocks the line from White rook on a1 to Black queen on a8")), 2)
    val delivered = deliveredPair(packet, receipt, "c4a3")
    val text = (delivered \ "detail" \ "text").as[String]
    val effects = (delivered \ "detail" \ "effects").as[List[JsObject]]
    assertEquals(effects.filter(effect => (effect \ "scope").as[String] == "pair_difference"), Nil,
      "the identical obstruction must not become a difference between the arms")
    assert(!text.contains("Blocks the line") && !text.contains("does not attack"), text)
    assertEquals(effects.size, 2)
    List("a3", "a5").foreach { square =>
      val effect = effects.find(value => (value \ "target" \ "square").as[String] == square).get
      val path = ObservedLineProtocol.prepare(packet).path(packet, (effect \ "line_ids").as[List[String]].head).get
      assertEquals((effect \ "scope").as[String], "line_history")
      assertEquals((effect \ "actor").as[JsObject] - "position_id",
        Json.obj("side" -> "white", "role" -> "rook", "square" -> "a1"))
      assertEquals((effect \ "target").as[JsObject] - "position_id",
        Json.obj("side" -> "white", "role" -> "knight", "square" -> square))
      assertEquals(path.moves, List(s"c4$square"))
      assertEquals((effect \ "through_plies").as[List[Int]], List(1))
      assertEquals((effect \ "occurrence_ids").as[List[String]], path.ids)
      assert(text.contains(s"After N$square, White rook on a1 adds support to White knight on $square"), text)
    }

  test("a third-piece comparison keeps its proof links and the existing text limit"):
    val fen = "q6k/8/8/8/2N5/8/7K/R7 w - - 0 1"
    val (packet, receipt) = batch(fen, "c4a3", List(
      (List("c4a3"), 40, 16), (List("c4b6"), 35, 16)), start = fen)
    val delivered = deliveredPair(packet, receipt, "c4a3")
    val text = (delivered \ "detail" \ "text").as[String]
    assert(text.contains("After Na3, White rook on a1 does not attack Black queen on a8"), text)
    assert(text.contains("after Nb6, White rook on a1 attacks Black queen on a8"), text)
    assert(text.contains("After Na3, Black queen on a8 does not attack White rook on a1"), text)
    assert(text.contains("after Nb6, Black queen on a8 attacks White rook on a1"), text)
    val effects = (delivered \ "detail" \ "effects").as[List[JsObject]]
    val differences = effects.filter(effect => (effect \ "scope").as[String] == "pair_difference")
    assertEquals(differences.size, 2)
    List(("white", "rook", "a1", "black", "queen", "a8"),
      ("black", "queen", "a8", "white", "rook", "a1")).foreach { (side, role, square, targetSide, targetRole, targetSquare) =>
      val effect = differences.find(value => (value \ "actor" \ "side").as[String] == side).get
      assertEquals((effect \ "line_ids").as[List[String]].toSet, (delivered \ "line_ids").as[List[String]].toSet)
      assertEquals((effect \ "through_plies").as[List[Int]], List(1, 1))
      assertEquals((effect \ "actor" \ "role").as[String], role)
      assertEquals((effect \ "actor" \ "square").as[String], square)
      assertEquals((effect \ "target" \ "side").as[String], targetSide)
      assertEquals((effect \ "target" \ "role").as[String], targetRole)
      assertEquals((effect \ "target" \ "square").as[String], targetSquare)
    }
    val supports = effects.filterNot(differences.contains)
    assertEquals(supports.size, 1)
    val support = supports.head
    assertEquals((support \ "scope").as[String], "line_history")
    assertEquals((support \ "actor").as[JsObject] - "position_id",
      Json.obj("side" -> "white", "role" -> "rook", "square" -> "a1"))
    assertEquals((support \ "target").as[JsObject] - "position_id",
      Json.obj("side" -> "white", "role" -> "knight", "square" -> "a3"))
    val supportPath = ObservedLineProtocol.prepare(packet).path(packet, (support \ "line_ids").as[List[String]].head).get
    assertEquals(supportPath.moves, List("c4a3"))
    assertEquals((support \ "through_plies").as[List[Int]], List(1))
    assertEquals((support \ "occurrence_ids").as[List[String]], supportPath.ids)
    val raw = read(packet, "c4a3", List(receipt)).head.candidateJson(packet)
    val full = raw + ("detail" -> ((raw \ "detail").as[JsObject] + ("text" -> JsString("x" * 3900))))
    assertEquals(MoveWorkProjection.select(packet).withMoveEffects(full), full,
      "keep the existing verdict rather than cut one arm")
    val selection = MoveWorkProjection.select(packet)
    val pvLimit = raw + ("detail" -> ((raw \ "detail").as[JsObject] + ("text" -> JsString("x" * 900))))
    assertEquals(selection.withMoveEffects(pvLimit), pvLimit, "the PV text contract is 1000 characters")
    val questionLimit = raw + ("detail" -> ((full \ "detail").as[JsObject] + ("type" -> JsString("question_comparison"))))
    assertEquals(selection.withMoveEffects(questionLimit), questionLimit, "other comparison text is bounded at 4000 characters")

  test("a shared occurrence cannot borrow an empty-square reply from another supplied branch"):
    val fen = "k7/8/8/4n3/7Q/8/6P1/1K6 w - - 0 1"
    val (base, receipt) = batch(fen, "g2g4", List(
      (List("g2g4", "e5c6"), 35, 16), (List("h4g4"), 40, 16)), start = fen)
    // e5c4 uses a square whose queen control g4 blocks; it belongs only to this extra branch.
    val supplied = MoveReviewInputAdmission.admitIndependent(base.assembly.input.positionHistory, "g2g4",
      suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, List("g2g4", "e5c4")))).get
    val input = MoveReviewInputAdmission.retainAdmittedLines(supplied, receipt.lines, EvaluationSource.RootSearch)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    assert(MoveWorkProjection.fromPacket(packet).exists(value => (value \ "text").as[String].contains("toward c4")))
    val text = (deliveredPair(packet, receipt, "g2g4") \ "detail" \ "text").as[String]
    assert(!text.contains("e5c4") && !text.contains("c4"), text)

  test("the comparison text limit retains a complete attack outcome when both cannot fit"):
    val fen = "6k1/5r2/8/4p3/2N3r1/8/8/7K w - - 0 1"
    val (packet, receipt) = batch(fen, "c4e5", List(
      (List("c4e5", "g4g3", "h1h2", "f7f6"), 35, 16), (List("c4d6"), 40, 16)), start = fen)
    val raw = read(packet, "c4e5", List(receipt)).head.candidateJson(packet)
    val selection = MoveWorkProjection.select(packet)
    val fullText = (selection.withMoveEffects(raw) \ "detail" \ "text").as[String]
    assert(fullText.contains("Rg3: moves the initially attacked Black rook on g4"), fullText)
    assert(fullText.contains("Rf6: moves the initially attacked Black rook on f7"), fullText)
    List("pv_comparison" -> 1000, "question_comparison" -> 4000).foreach { (kind, limit) =>
      val limited = raw + ("detail" -> ((raw \ "detail").as[JsObject] ++ Json.obj(
        "type" -> kind, "text" -> ("x" * (limit - 300)))))
      val delivered = selection.withMoveEffects(limited)
      assertEquals(delivered - "detail" - "evidence_ids", limited - "detail" - "evidence_ids")
      assertEquals((delivered \ "detail").as[JsObject] - "text" - "effects",
        (limited \ "detail").as[JsObject] - "text" - "effects")
      val text = (delivered \ "detail" \ "text").as[String]
      val keepsF7 = text.contains("Rf6:")
      assert(keepsF7 != text.contains("Rg3:"), text)
      assert(text.contains(s"That original target remains on ${if keepsF7 then "f6" else "g3"} at this line's end."), text)
      assert(text.length <= limit)
      val keptMove = if keepsF7 then "f7f6" else "g4g3"
      val skippedMove = if keepsF7 then "g4g3" else "f7f6"
      val effects = (delivered \ "detail" \ "effects").as[List[JsObject]]
      val histories = effects.filter(effect => (effect \ "scope").as[String] == "line_history" &&
        (effect \ "through_plies").as[List[Int]] == List(4))
      assertEquals(histories.size, 1, "exactly one complete attack outcome fits")
      val history = histories.head
      assertEquals((history \ "actor").as[JsObject] - "position_id",
        Json.obj("side" -> "white", "role" -> "knight", "square" -> "e5"))
      assertEquals((history \ "target" \ "side").as[String], "black")
      assertEquals((history \ "target" \ "role").as[String], "rook")
      assertEquals((history \ "target" \ "square").as[String], if keepsF7 then "f7" else "g4")
      assertEquals((history \ "through_plies").as[List[Int]], List(4))
      val prepared = ObservedLineProtocol.prepare(packet)
      val historyPath = prepared.path(packet, (history \ "line_ids").as[List[String]].head).get
      assertEquals(historyPath.moves, List("c4e5", "g4g3", "h1h2", "f7f6"))
      effects.filterNot(histories.contains).foreach { current =>
        val path = prepared.path(packet, (current \ "line_ids").as[List[String]].head).get
        assertEquals((current \ "scope").as[String], "line_history")
        assertEquals((current \ "through_plies").as[List[Int]], List(1))
        assertEquals((current \ "actor").as[JsObject] - "position_id",
          Json.obj("side" -> "white", "role" -> "knight", "square" -> path.moves.head.slice(2, 4)))
        assertEquals((current \ "target").as[JsObject] - "position_id",
          Json.obj("side" -> "black", "role" -> "rook", "square" -> "g4"))
        assertEquals((current \ "occurrence_ids").as[List[String]], path.ids.take(1))
        assert(text.contains(s"${if path.moves.head == "c4e5" then "Nxe5" else "Nd6"}: Moves the knight away from the attack of Black rook on g4."), text)
      }
      val effectOccurrences = (history \ "occurrence_ids").as[List[String]]
      val effectEvidence = (history \ "evidence_ids").as[List[String]]
      assert(packet.assembly.transitions.filter(_.moveUci == keptMove).exists(edge =>
        effectOccurrences.contains(edge.evidence.id) && effectEvidence.contains(edge.evidence.id)))
      assert(!packet.assembly.transitions.filter(_.moveUci == skippedMove).exists(edge =>
        effectOccurrences.contains(edge.evidence.id) || effectEvidence.contains(edge.evidence.id)))
      val evidence = (delivered \ "evidence_ids").as[List[String]]
      if kind == "pv_comparison" then assertEquals(evidence, (raw \ "evidence_ids").as[List[String]])
      else
        assert(packet.assembly.transitions.filter(_.moveUci == keptMove).exists(edge => evidence.contains(edge.evidence.id)))
        assert(!packet.assembly.transitions.filter(_.moveUci == skippedMove).exists(edge => evidence.contains(edge.evidence.id)))
    }

  test("attack outcomes preserve PV ownership and use the attacker's location at each evasion"):
    val fen = "r6k/8/8/8/8/8/8/R6K w - - 0 1"
    val (packet, receipt) = batch(fen, "a1a2", List(
      (List("a1a2", "a8b8", "a2b2", "b8c8"), 35, 16), (List("h1g1"), 40, 16)), start = fen)
    val delivered = deliveredPair(packet, receipt, "a1a2")
    val text = (delivered \ "detail" \ "text").as[String]
    assert(text.contains("Rb8: moves the initially attacked Black rook on a8 out of White rook on a2's attack"), text)
    assert(text.contains("Rc8: moves the initially attacked Black rook on a8 out of White rook on b2's attack"), text)
    assert(text.contains("remains on c8 at this line's end"), text)
    val raw = read(packet, "a1a2", List(receipt)).head.candidateJson(packet)
    val limited = raw + ("detail" -> ((raw \ "detail").as[JsObject] + ("text" -> JsString("x" * 900))))
    val bounded = MoveWorkProjection.select(packet).withMoveEffects(limited)
    val boundedText = (bounded \ "detail" \ "text").as[String]
    assert(!boundedText.contains("Rb8:") && !boundedText.contains("Rc8:") &&
      !boundedText.contains("at this line's end"),
      "an outcome's evasion and end state must fit together; independent current facts may still fit")
    assert(!(bounded \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil)
      .exists(effect => (effect \ "through_plies").as[List[Int]] == List(4)))

  test("prepared lookup binds the entire certified line and rejects another packet owner"):
    val (packet, receipt) = openingBatch()
    val prepared = ObservedLineProtocol.prepare(packet)
    val moves = receipt.lines.head.evaluation.moves
    val path = prepared.certifiedPath(packet, moves).get
    assertEquals(prepared.path(packet, path.line.ref.id), Some(path))
    assertEquals(path.moves, moves)
    assertEquals(prepared.certifiedPath(packet, moves.take(1)), None)
    assertEquals(prepared.certifiedPath(packet, moves.drop(1)), None)
    val (other, _) = openingBatch()
    intercept[IllegalArgumentException](prepared.certifiedPath(other, moves))
    intercept[IllegalArgumentException](read(other, "f1c4", List(receipt), prepared = Some(prepared)))
    intercept[IllegalArgumentException](CommentaryInvestigation.pairedAnswer(other, "f1c4", "f1b5",
      receipt.work, receipt.lines, Some(prepared)))

  test("shared multiple-reference readings preserve each ordered pair and its evidence"):
    val (packet, receipt) = openingBatch()
    val prepared = ObservedLineProtocol.prepare(packet)
    val references = List("f1c4", "f1b5")
    val independent = references.flatMap(move => read(packet, move, List(receipt)))
      .distinctBy(_.id).map(_.candidateJson(packet))
    val shared = readAll(packet, references, List(receipt), prepared = Some(prepared))
    assertEquals(shared.map(_.candidateJson(packet)), independent)
    assert(shared.exists(value => value.referenceMove == "f1c4" && value.alternativeMove == "f1b5"))
    assert(shared.exists(value => value.referenceMove == "f1b5" && value.alternativeMove == "f1c4"))

  test("close quiet PV pairs preserve actual-first order and exact positive root participants"):
    val (packet, receipt) = openingBatch()
    val readings = read(packet, "f1c4", List(receipt))
    assertEquals(readings.size, 2)
    assertEquals(readings.head.answer.alternativeOccurrenceId, packet.assembly.playedTransition.get.evidence.id)
    readings.foreach { reading =>
      assertEquals(reading.materialRelation, MaterialRelation.Equal)
      assertEquals(reading.eventSequenceRelation, Relation.Same)
      assertEquals(reading.rootObservationRelation, Relation.Different)
      assertEquals(reading.qualityBand, QualityBand.Close)
      assertEquals(reading.answer.qualityBand, Some(reading.qualityBand))
      assert(reading.eligibleQuietFocus)
      assertEquals(reading.rootDifferenceOccurrences,
        List(reading.answer.referenceOccurrenceId, reading.answer.alternativeOccurrenceId))
      val observations = packet.observations.filter(value => reading.rootDifferenceOccurrences.contains(value.transition.evidence.id))
      assert(observations.exists(_.meaning.isInstanceOf[MoveObservationMeaning.Development]))
      assert(observations.exists(_.meaning.isInstanceOf[MoveObservationMeaning.PieceControl]))
      assert(reading.participants.forall(piece => Set("f1", "b1")(piece.square.key)))
      val json = reading.candidateJson(packet)
      assertEquals((json \ "selection").as[String], "context")
      assertEquals((json \ "detail" \ "answer").as[JsObject], CommentaryInvestigation.answerJson(reading.answer))
      assertEquals((json \ "detail" \ "root_participants").as[List[JsObject]].map(value =>
        (value \ "position_id").as[String]).distinct, List(packet.assembly.playedTransition.get.from.id.get))
      val sources = (json \ "source_refs").as[List[JsObject]]
      assertEquals(sources.map(value => (value \ "source").as[String]).distinct, List("root_search"))
      assert((json \ "detail" \ "text").as[String].contains("not a forced result"))
    }

  test("existing symmetric verdict thresholds separate distant scores without completing an explanation"):
    val (packet, receipt) = openingBatch(bishopScore = 600)
    val readings = read(packet, "f1c4", List(receipt))
    assert(readings.forall(_.qualityBand == QualityBand.Separated))
    assert(readings.forall(_.answer.qualityBand.contains(QualityBand.Separated)))
    assert(readings.forall(!_.eligibleQuietFocus))
    assert(readings.forall(_.materialRelation == MaterialRelation.Equal))
    assert(readings.forall(_.rootObservationRelation == Relation.Different))

  test("the latest usable receipt wins and retained IDs are rebuilt against the replacement packet"):
    val (oldPacket, older) = openingBatch(id = "older")
    val oldReadings = read(oldPacket, "f1c4", List(older))
    val (newPacket, newer) = openingBatch(id = "newer", bishopScore = 85)
    val combinedInput = MoveReviewInputAdmission.retainAdmittedLines(newPacket.assembly.input, older.lines, EvaluationSource.RootSearch)
    val combined = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(combinedInput)).get
    val latest = read(combined, "f1c4", List(older, newer))
    assertEquals(latest.map(_.answer.workId).distinct, List("newer"))
    val retained = read(combined, "f1c4", List(older, newer), oldReadings.map(_.id).toSet)
    assertEquals(retained.map(_.answer.workId).toSet, Set("older", "newer"))
    assert(retained.forall(value => value.candidateJson(combined).value.nonEmpty))
    intercept[IllegalArgumentException](oldReadings.head.candidateJson(combined))
    assert(!oldReadings.head.belongsTo(combined))

  test("mismatched search levels and roots do not manufacture pair readings"):
    val (packet, receipt) = openingBatch(knightDepth = 17)
    assertEquals(read(packet, "f1c4", List(receipt)).size, 1)
    assertEquals(read(packet, "f1c4", List(receipt.copy(work = receipt.work.copy(searchFen = initial)))), Nil)

  test("selection preserves inquiry lifecycle and exact answer links without a general next-evidence directive"):
    val (packet, receipt) = openingBatch()
    val readings = read(packet, "f1c4", List(receipt))
    val reading = readings.find(_.alternativeMove == "b1c3").get
    val question = CommentaryInvestigation.discover(packet, "f1c4", Some(receipt), readings)
      .find(_.alternativeMove == "b1c3").get
    def delivered(answer: CommentaryInvestigation.Answer): JsObject =
      val inquiry = CommentaryInquiry(question, "answered", Some(answer), assessmentEffect = Some("needs_refresh"))
      val selection = (RuntimeProtocol.observationsJson(packet, inquiries = List(inquiry), pvComparisons = readings) \ "selection")
        .as[JsObject]
      assertEquals((selection \ "version").as[Int], 2)
      val row = (selection \ "inquiries").as[List[JsObject]].head
      assertEquals((row \ "state").as[String], "answered")
      assertEquals((row \ "assessment_effect").as[String], "needs_refresh")
      assertEquals((row \ "answer").as[JsObject], CommentaryInvestigation.answerJson(answer))
      assert((row \ "next_evidence").toOption.isEmpty)
      row
    assertEquals((delivered(reading.answer) \ "explanation_candidate_ids").as[List[String]], List(reading.id))
    List(reading.answer.copy(workId = "other-work"),
      reading.answer.copy(executionKeySha256 = "0" * 64),
      reading.answer.copy(alternativeLineId = reading.answer.referenceLineId)).foreach { answer =>
      assertEquals((delivered(answer) \ "explanation_candidate_ids").as[List[String]], Nil)
    }

  test("child-only material differences do not create root participants or quiet priority"):
    val fen = "7k/2p5/8/8/8/3R4/8/K7 w - - 0 1"
    val (packet, receipt) = batch(fen, "a1b1", List(
      (List("a1b2", "c7c6", "d3d5", "h8g8", "d5c5"), 40, 16),
      (List("a1b1", "c7c5", "d3d5", "h8g8", "d5c5"), 35, 16)), start = fen)
    val reading = read(packet, "a1b2", List(receipt)).head
    assertEquals(reading.materialRelation, MaterialRelation.Different)
    assertEquals(reading.eventSequenceRelation, Relation.Different)
    assertEquals(reading.rootObservationRelation, Relation.Unavailable)
    assertEquals(reading.rootDifferenceOccurrences, Nil)
    assertEquals(reading.participants, Nil)
    assert(!reading.eligibleQuietFocus)
    val detail = (reading.candidateJson(packet) \ "detail").as[JsObject]
    assertEquals((detail \ "material_relation").as[String], "different")
    assertEquals((detail \ "root_observation_relation").as[String], "unavailable")

  test("equal role totals can have different capture and recapture timelines without proving forced replies"):
    val (packet, receipt) = batch(initial, "e2e4", List(
      (List("d2d4", "e7e5", "d4e5", "b8c6", "g1f3", "c6e5"), 50, 16),
      (List("e2e4", "d7d5", "e4d5", "d8d5"), 40, 16)))
    val reading = read(packet, "d2d4", List(receipt)).head
    assertEquals(reading.materialRelation, MaterialRelation.Equal)
    assertEquals(reading.eventSequenceRelation, Relation.Different)
    val materials = packet.assembly.legalLines.flatMap(line => ObservedLineProtocol.certifiedFact(packet.assembly, line)
      .flatMap(_._2.material))
    assert(materials.exists(_.captures.exists(_.recapture)))
    assert((reading.candidateJson(packet) \ "detail" \ "text").as[String].contains("not a forced result"))

  test("same-FEN endpoints retain distinct exact paths and do not erase root development differences"):
    val (packet, receipt) = batch(initial, "g1f3", List(
      (List("b1c3", "b8c6", "g1f3", "g8f6"), 40, 16),
      (List("g1f3", "g8f6", "b1c3", "b8c6"), 35, 16)))
    val reading = read(packet, "b1c3", List(receipt)).head
    val lines = List(reading.answer.referenceLineId, reading.answer.alternativeLineId)
      .map(id => packet.assembly.legalLines.find(_.ref.id == id).get)
    assertEquals(lines.map(line => packet.assembly.lineReplays(line.ref).replaySteps.last.fenAfter).distinct.size, 1)
    assertEquals(lines.map(_.ref.id).distinct.size, 2)
    assertEquals(reading.eventSequenceRelation, Relation.Same)
    assertEquals(reading.rootObservationRelation, Relation.Different)
    assertEquals((reading.candidateJson(packet) \ "line_ids").as[List[String]], lines.map(_.ref.id))

  test("moving the same bishop to two squares with the same lost provider is not a functional contrast"):
    val fen = "7k/8/8/8/8/8/P7/4KB2 w - - 0 1"
    val (packet, receipt) = batch(fen, "f1b5", List(
      (List("f1a6", "h8g8"), 40, 16), (List("f1b5", "h8g8"), 35, 16)), start = fen)
    val reading = read(packet, "f1a6", List(receipt)).head
    assertEquals(reading.rootObservationRelation, Relation.Same)
    assertEquals(reading.materialRelation, MaterialRelation.Equal)
    assertEquals(reading.eventSequenceRelation, Relation.Same)
    assertEquals(reading.participants, Nil)
    assert(!reading.eligibleQuietFocus)
    val delivered = deliveredPair(packet, receipt, "f1a6")
    assertEquals((delivered \ "detail" \ "root_observation_relation").as[String], "same")
    assertEquals((delivered \ "detail" \ "material_relation").as[String], "equal")

  test("owned root answers preserve mover-relative ordering and share symmetric close or separated bands"):
    val cases = List(
      ("7k/7p/8/8/8/8/P7/K7 w - - 0 1", chess.White, "a1b1", "a1b2", "h8g8"),
      ("7k/7p/8/8/8/8/P7/K7 b - - 0 1", chess.Black, "h8g8", "h8g7", "a1b1"))
    for
      (fen, mover, reference, alternative, reply) <- cases
      (difference, expectedBand) <- List(10 -> QualityBand.Close, 600 -> QualityBand.Separated)
    do
      val cp = if mover.white then difference else -difference
      val (packet, receipt) = batch(fen, alternative, List(
        (List(reference, reply), 0, 16), (List(alternative, reply), cp, 16)),
        id = s"root-band-${if mover.white then "white" else "black"}-$difference", start = fen)
      assertEquals(packet.assembly.input.positionHistory.currentPosition.color, mover)
      val forward = read(packet, reference, List(receipt)).head
      val reverse = read(packet, alternative, List(receipt)).head
      assertEquals(forward.qualityBand, expectedBand)
      assertEquals(reverse.qualityBand, expectedBand)
      assertEquals(forward.answer.qualityBand, Some(expectedBand))
      assertEquals(reverse.answer.qualityBand, Some(expectedBand))
      assertEquals(forward.answer.evaluationRelation, "better")
      assertEquals(reverse.answer.evaluationRelation, "worse")
      assertEquals(forward.answer.workId, receipt.work.workId)
      assertEquals(reverse.answer.executionKeySha256, forward.answer.executionKeySha256)
      assertEquals(reverse.answer.referenceLineId, forward.answer.alternativeLineId)
      assertEquals(reverse.answer.alternativeLineId, forward.answer.referenceLineId)
      val comparison = CommentaryProposal.CompareMoves("root-band", Nil, reference, alternative, Some("better"))
      assertEquals(CommentaryProposal.pairReading(comparison, forward.answer).state, "supported")
      assertEquals(CommentaryProposal.pairReading(comparison.copy(referenceMove = alternative,
        alternativeMove = reference), reverse.answer).state, "refuted")
      assert((CommentaryInvestigation.answerJson(forward.answer) \ "quality_band").toOption.isEmpty,
        "the shared band is internal; raw answer and public ownership contracts stay unchanged")

  test("reported mate-distance differences retain the existing playable-loss quality band"):
    val (packet, receipt) = scoredBatch(opening, "f1b5", List(
      (List("f1c4", "g8f6"), ReportedWhiteEngineScore.Mate(3), 16),
      (List("f1b5", "a7a6"), ReportedWhiteEngineScore.Mate(5), 16)),
      prefix = List("e2e4", "e7e5", "g1f3", "b8c6"))
    val forward = read(packet, "f1c4", List(receipt)).head
    val reverse = read(packet, "f1b5", List(receipt)).head
    assertEquals(forward.qualityBand, QualityBand.Close)
    assertEquals(reverse.qualityBand, QualityBand.Close)
    assertEquals(forward.answer.qualityBand, Some(QualityBand.Close))
    assertEquals(reverse.answer.qualityBand, Some(QualityBand.Close))
    assertEquals(forward.answer.evaluationRelation, "worse")
    assertEquals(reverse.answer.evaluationRelation, "better")
    // These are synthetic reported mates; the finite legal PV does not itself prove mate.
    assertEquals((forward.answer.referenceEvaluation \ "kind").as[String], "engine_search")

  test("saturated winning scores still honor the existing 500cp degradation exception"):
    val (packet, receipt) = batch(opening, "f1b5", List(
      (List("f1c4", "g8f6"), 1500, 16), (List("f1b5", "a7a6"), 1000, 16)),
      prefix = List("e2e4", "e7e5", "g1f3", "b8c6"))
    val forward = read(packet, "f1c4", List(receipt)).head
    val reverse = read(packet, "f1b5", List(receipt)).head
    assertEquals(forward.qualityBand, QualityBand.Separated)
    assertEquals(reverse.qualityBand, QualityBand.Separated)
    assertEquals(forward.answer.qualityBand, Some(QualityBand.Separated))
    assertEquals(reverse.answer.qualityBand, Some(QualityBand.Separated))
    assert(!forward.eligibleQuietFocus)
