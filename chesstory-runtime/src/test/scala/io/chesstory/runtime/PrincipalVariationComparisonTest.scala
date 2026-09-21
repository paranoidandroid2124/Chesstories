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
    val delivered = (RuntimeProtocol.observationsJson(packet, pvComparisons = readings) \ "selection" \ "candidates")
      .as[List[JsObject]].find(value => (value \ "id").as[String] == readings.head.id).get
    assertEquals(delivered - "detail" - "evidence_ids", before - "detail" - "evidence_ids",
      "presentation cannot change pair ownership, state, selection or engine sources")
    assertEquals((delivered \ "detail").as[JsObject] - "text", (before \ "detail").as[JsObject] - "text",
      "the investigation relation, participants and priority inputs remain unchanged")
    assertEquals((delivered \ "evidence_ids").as[List[String]], (before \ "evidence_ids").as[List[String]],
      "the existing PV receiver requires exactly the two line-summary proof IDs")
    assert((delivered \ "detail" \ "text").as[String].length <= 1000)
    (delivered \ "evidence_ids").as[List[String]].foreach { id =>
      assert(packet.evidenceGraph.byId.get(id).exists(packet.evidenceGraph.proofEligible), id)
    }
    delivered

  test("the same third-piece obstruction in both candidates is not a comparison difference"):
    val fen = "q6k/8/8/8/2N5/8/7K/R7 w - - 0 1"
    val (packet, receipt) = batch(fen, "c4a3", List(
      (List("c4a3"), 40, 16), (List("c4a5"), 35, 16)), start = fen)
    val local = MoveWorkProjection.fromPacket(packet).map(value => (value \ "text").as[String])
    assertEquals(local.count(_.contains("Blocks the line from White rook on a1 to Black queen on a8")), 2)
    val text = (deliveredPair(packet, receipt, "c4a3") \ "detail" \ "text").as[String]
    assert(!text.contains("White rook on a1"), text)

  test("a third-piece comparison keeps its proof links and the existing text limit"):
    val fen = "q6k/8/8/8/2N5/8/7K/R7 w - - 0 1"
    val (packet, receipt) = batch(fen, "c4a3", List(
      (List("c4a3"), 40, 16), (List("c4b6"), 35, 16)), start = fen)
    val delivered = deliveredPair(packet, receipt, "c4a3")
    val text = (delivered \ "detail" \ "text").as[String]
    assert(text.contains("After c4a3, White rook on a1 does not attack Black queen on a8"), text)
    assert(text.contains("after c4b6, White rook on a1 attacks Black queen on a8"), text)
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

  test("attack outcomes preserve PV ownership and use the attacker's location at each evasion"):
    val fen = "r6k/8/8/8/8/8/8/R6K w - - 0 1"
    val (packet, receipt) = batch(fen, "a1a2", List(
      (List("a1a2", "a8b8", "a2b2", "b8c8"), 35, 16), (List("h1g1"), 40, 16)), start = fen)
    val delivered = deliveredPair(packet, receipt, "a1a2")
    val text = (delivered \ "detail" \ "text").as[String]
    assert(text.contains("a8b8: moves the initially attacked Black rook on a8 out of White rook on a2's attack"), text)
    assert(text.contains("b8c8: moves the initially attacked Black rook on a8 out of White rook on b2's attack"), text)
    assert(text.contains("remains on c8 at this line's end"), text)
    val raw = read(packet, "a1a2", List(receipt)).head.candidateJson(packet)
    val limited = raw + ("detail" -> ((raw \ "detail").as[JsObject] + ("text" -> JsString("x" * 900))))
    assertEquals(MoveWorkProjection.select(packet).withMoveEffects(limited), limited,
      "an outcome's evasion and end state must fit together, not be cut midway")

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
    assertEquals(reading.state, "ready") // Only the bounded exchange observation is ready.

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
    assertEquals(reading.state, "needs_explanation")

  test("Black mover quality bands are symmetric while the evaluation relation reverses"):
    val fen = "7k/7p/8/8/8/8/P7/K7 b - - 0 1"
    val (packet, receipt) = batch(fen, "h8g7", List(
      (List("h8g8", "a1b1"), -600, 16), (List("h8g7", "a1b1"), -50, 16)), start = fen)
    val forward = read(packet, "h8g8", List(receipt)).head
    val reverse = read(packet, "h8g7", List(receipt)).head
    assertEquals(forward.qualityBand, QualityBand.Separated)
    assertEquals(reverse.qualityBand, forward.qualityBand)
    assertEquals(forward.answer.evaluationRelation, "worse")
    assertEquals(reverse.answer.evaluationRelation, "better")

  test("reported mate-distance differences retain the existing playable-loss quality band"):
    val (packet, receipt) = scoredBatch(opening, "f1b5", List(
      (List("f1c4", "g8f6"), ReportedWhiteEngineScore.Mate(3), 16),
      (List("f1b5", "a7a6"), ReportedWhiteEngineScore.Mate(5), 16)),
      prefix = List("e2e4", "e7e5", "g1f3", "b8c6"))
    val forward = read(packet, "f1c4", List(receipt)).head
    val reverse = read(packet, "f1b5", List(receipt)).head
    assertEquals(forward.qualityBand, QualityBand.Close)
    assertEquals(reverse.qualityBand, QualityBand.Close)
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
    assert(!forward.eligibleQuietFocus)
