package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Supplied exact lines exercise physical comparisons, not engine quality or automatic move discovery. */
class LineContinuationComparisonTest extends munit.FunSuite:
  private def comparison(fen: String, left: List[String], right: List[String]) =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, left.head,
      suppliedLines = List(left, right).map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    assertEquals(input.suppliedLines.map(_.acceptedMoves), List(left, right))
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    val readings = prepared.continuationReadings
    val l = readings.find(_.path.moves == left).get
    val r = readings.find(_.path.moves == right).get
    (packet, prepared, l, r, prepared.continuationComparison(packet, l, r))

  test("the same Qxg5 capture has a different timing and exact gxf4 availability after h5 Qh4"):
    val fen = "1rr3k1/4q1pp/p2pp3/4p1N1/BPn2nQ1/P5PP/1P3P2/1R2R1K1 b - - 0 27"
    val (packet, prepared, left, right, result) = comparison(fen,
      List("e7g5"), List("h7h5", "g4h4", "e7g5"))
    val detail = result.get.candidateJson(packet) \ "detail"
    val differences = (detail \ "differences").as[List[JsObject]]
    assert(!differences.exists(row => (row \ "kind").as[String] == "loss_followup"))
    val timing = differences.find(row => (row \ "kind").as[String] == "capture_timing").get
    assertEquals((timing \ "left" \ "ply_count").as[Int], 1)
    assertEquals((timing \ "right" \ "ply_count").as[Int], 3)
    val pawnCapture = differences.find(row => (row \ "kind").as[String] == "legal_capture_availability" &&
      (row \ "original_actor" \ "square").as[String] == "g3" &&
      (row \ "original_victim" \ "square").as[String] == "f4").get
    assertEquals((pawnCapture \ "left" \ "moves_uci").as[List[String]], List("g3f4"))
    assertEquals((pawnCapture \ "right" \ "available").as[Boolean], false)
    assertEquals((pawnCapture \ "right" \ "legal_inventory_complete").as[Boolean], true)
    assert((pawnCapture \ "left" \ "source_ids").as[List[String]].nonEmpty)
    assertEquals((detail \ "continuation_candidate_ids").as[List[String]], List(left.id, right.id))
    assert(prepared.continuationComparison(packet, left, right).get eq result.get)
    assertEquals(LineContinuationComparison.compare(packet, left, left, prepared), None)

  test("equal captured roles can replace the same lost pawn with a different support provider"):
    val fen = "7k/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val (packet, _, _, _, result) = comparison(fen,
      List("g1h1", "d5e4", "f3e4"), List("g1h1", "d5e4", "d2e4"))
    val row = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]].find(row =>
      (row \ "kind").as[String] == "replacement_support" &&
        (row \ "original_subject" \ "square").as[String] == "e4" &&
        (row \ "original_target" \ "square").as[String] == "g5").get
    assertEquals((row \ "left" \ "original_provider" \ "square").as[String], "f3")
    assertEquals((row \ "right" \ "original_provider" \ "square").as[String], "d2")
    assertEquals((row \ "left" \ "support" \ "state").as[String], "absent")
    assertEquals((row \ "right" \ "support" \ "state").as[String], "present")
    assertEquals((row \ "left" \ "ply_count").as[Int], 3)
    assertEquals((row \ "right" \ "ply_count").as[Int], 3)

  test("a fixed recapturer loses an old pawn support while its peer retains support through that pawn's relocation"):
    val fen = "6rk/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    // Supplied alternatives: this tests their observed support, not discovery
    // or strength of ...Rf8. The root already permits ...Rxg5 after Kh1.
    val knight = List("g1h1", "d5e4", "d2e4", "g8f8")
    val pawn = List("g1h1", "d5e4", "f3e4", "g8f8")
    val (packet, prepared, left, right, result) = comparison(fen, knight, pawn)
    val rows = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]]
      .filter(row => (row \ "kind").as[String] == "recapturer_support")
    val row = rows.find(row => (row \ "original_subject" \ "square").as[String] == "d2" &&
      (row \ "original_target" \ "square").as[String] == "f3").get
    assertEquals((row \ "matched_support_comparison_ids").as[List[String]],
      List(left, right).map(_.supportComparisons.find(_.kind == "replacement_after_capture").get.id))
    assertEquals((row \ "left" \ "before" \ "support" \ "state").as[String], "present")
    assertEquals((row \ "left" \ "after" \ "support" \ "state").as[String], "absent")
    assertEquals((row \ "right" \ "before" \ "support" \ "state").as[String], "present")
    assertEquals((row \ "right" \ "after" \ "support" \ "state").as[String], "present")
    List("left" -> left, "right" -> right).foreach { (side, reading) =>
      List("before" -> 2, "after" -> 3).foreach { (phase, count) =>
        assertEquals((row \ side \ phase \ "ply_count").as[Int], count)
        assertEquals((row \ side \ phase \ "position_id").as[String], reading.path.positions(count).id.get)
        assertEquals((row \ side \ phase \ "preceding_occurrence_id").as[String], reading.path.ids(count - 1))
      }
    }
    val target = prepared.identitiesAfter(packet, left.path, 0).find(_._1.square.key == "f3").get._1
    assertEquals(prepared.pieceAfter(packet, left.path, target, 3).flatten.map(_.square.key), Some("f3"))
    assertEquals(prepared.pieceAfter(packet, right.path, target, 3).flatten.map(_.square.key), Some("e4"))
    assert((row \ "right" \ "after" \ "support" \ "source_ids").as[List[String]].nonEmpty)
    assert((row \ "evaluation").toOption.isEmpty)

  test("the same recapturer support change in two paths produces no new contrast"):
    val fen = "6rk/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val prefix = List("g1h1", "d5e4", "d2e4")
    val (packet, _, _, _, result) = comparison(fen, prefix :+ "g8f8", prefix :+ "g8b8")
    val rows = result.toList.flatMap(value => (value.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]])
    assert(!rows.exists(row => (row \ "kind").as[String] == "recapturer_support"))

  test("a peer ending before recapture on the opposite turn has no invented recapturer after point"):
    val fen = "6rk/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val prefix = List("g1h1", "d5e4")
    val (packet, _, left, right, result) = comparison(fen, prefix :+ "d2e4", prefix)
    assert(left.path.positions.last.sideToMove != right.path.positions.last.sideToMove)
    val rows = result.toList.flatMap(value => (value.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]])
    assert(!rows.exists(row => (row \ "kind").as[String] == "recapturer_support"))

  test("missing prior support evidence cannot become a recapturer's support-loss claim"):
    val fen = "6rk/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val knight = List("g1h1", "d5e4", "d2e4")
    val pawn = List("g1h1", "d5e4", "f3e4")
    val (original, _, left, _, _) = comparison(fen, knight, pawn)
    val before = left.path.positions(2)
    val missing = original.resources.find(_.position == before).get.records.find(_.payload match
      case fact: RelationFactEvidence => fact.detail match
        case RelationWitnessDetail.GeometricControl(chess.Color.White, from, _, to) =>
          from.key == "d2" && to.key == "f3"
        case _ => false
      case _ => false).get
    val assembly = original.assembly.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(
      original.evidenceGraph.records.filterNot(_ == missing)))
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(assembly).get
    val prepared = ObservedLineProtocol.prepare(packet)
    val l = prepared.continuationReadings.find(_.path.moves == knight).get
    val r = prepared.continuationReadings.find(_.path.moves == pawn).get
    val row = l.supportComparisons.find(_.kind == "replacement_after_capture").get
    val target = prepared.identitiesAfter(packet, l.path, 0).find(_._1.square.key == "f3").get._1
    assertEquals(l.supportAt(2, row.afterActor, target).state, "unknown")
    val result = prepared.continuationComparison(packet, l, r)
    val rows = result.toList.flatMap(value => (value.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]])
    assert(!rows.exists(row => (row \ "kind").as[String] == "recapturer_support" &&
      (row \ "original_subject" \ "square").as[String] == "d2" &&
      (row \ "original_target" \ "square").as[String] == "f3"))

  test("same root actor endpoint support is compared only with the same side to move"):
    val fen = "k7/7p/8/8/8/5N2/7P/6K1 w - - 0 1"
    val left = List("f3h4", "h7h6", "h4f3")
    val right = List("f3h4", "h7h6", "h4g6")
    val (packet, _, _, _, result) = comparison(fen, left, right)
    val rows = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]]
    assert(rows.exists(row => (row \ "kind").as[String] == "root_actor_support" &&
      (row \ "original_target" \ "square").as[String] == "h2" &&
      (row \ "left" \ "support" \ "state").as[String] == "present" &&
      (row \ "right" \ "support" \ "state").as[String] == "absent"))
    assertEquals(comparison(fen, left, right.take(2))._5, None)

  test("different recapturers expose the queen or the remaining pawn at the matched replacement point"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val (packet, _, left, right, result) = comparison(fen,
      List("d2d3", "d5e4", "d3e4"), List("f1d3", "d5e4", "d3e4"))
    val rows = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]]
      .filter(row => (row \ "kind").as[String] == "legal_capture_availability" &&
        (row \ "matched_support_comparison_ids").toOption.nonEmpty)
    assertEquals(rows.size, 2)
    val queen = rows.find(row => (row \ "original_victim" \ "square").as[String] == "d1").get
    assertEquals((queen \ "left" \ "moves_uci").as[List[String]], List("d8d1"))
    assertEquals((queen \ "right" \ "available").as[Boolean], false)
    assertEquals((queen \ "left" \ "ply_count").as[Int], 3)
    assertEquals((queen \ "right" \ "ply_count").as[Int], 3)
    assert((queen \ "matched_capture_event_ids").toOption.isEmpty)
    assertEquals((queen \ "matched_support_comparison_ids").as[List[String]],
      List(left, right).map(_.supportComparisons.find(_.kind == "replacement_after_capture").get.id))
    val pawn = rows.find(row => (row \ "original_victim" \ "square").as[String] == "d2").get
    assertEquals((pawn \ "left" \ "available").as[Boolean], false)
    assertEquals((pawn \ "right" \ "moves_uci").as[List[String]], List("d8d2"))

  test("a replacement's later recapture is linked to the actual target attack on each exact branch"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val pawn = List("d2d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3", "d1f3")
    val bishop = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3", "e4f3")
    val (packet, _, left, right, result) = comparison(fen, pawn, bishop)
    val rows = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]]
    val row = rows.find(row => (row \ "right" \ "direct_recapture").toOption.nonEmpty).get
    assertEquals((row \ "kind").as[String], "replacement_support")
    assertEquals((row \ "original_target" \ "square").as[String], "f3")
    assertEquals((row \ "left" \ "ply_count").as[Int], 5)
    assertEquals((row \ "right" \ "ply_count").as[Int], 5)
    assertEquals((row \ "left" \ "support" \ "state").as[String], "absent")
    assertEquals((row \ "right" \ "support" \ "state").as[String], "present")
    assert((row \ "left" \ "direct_recapture").toOption.isEmpty)
    assertEquals((row \ "right" \ "direct_recapture" \ "recapture_event_id").as[String],
      right.directSupportRecaptures.head.recapture.id)
    List("left" -> left, "right" -> right).foreach { (side, reading) =>
      assertEquals((row \ side \ "target_capture_event_id").as[String],
        reading.events.find(_.plyOffset == 5).get.id)
    }
    val (shortPacket, _, _, _, short) = comparison(fen, pawn.take(5), bishop)
    assert(!(short.get.candidateJson(shortPacket) \ "detail" \ "differences").as[List[JsObject]]
      .exists(row => (row \ "right" \ "direct_recapture").toOption.nonEmpty))

  test("replacement support is used when the same opposing king can actually capture the original pawn"):
    val fen = "7k/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val prefix = List("g1h1", "d5e4")
    val suffix = List("h8g7", "h1g2", "g7g6", "g2h2")
    val pawn = prefix ++ List("f3e4") ++ suffix
    val knight = prefix ++ List("d2e4") ++ suffix
    val (packet, _, _, _, result) = comparison(fen, pawn, knight)
    val rows = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]]
    val applied = rows.filter(row => (row \ "left" \ "application").asOpt[JsObject].nonEmpty)
    assertEquals(applied.size, 1)
    val row = applied.head
    assertEquals((row \ "kind").as[String], "replacement_support")
    assertEquals((row \ "left" \ "ply_count").as[Int], 7)
    assertEquals((row \ "right" \ "ply_count").as[Int], 7)
    assertEquals((row \ "left" \ "application" \ "original_attacker" \ "square").as[String], "h8")
    assertEquals((row \ "left" \ "application" \ "attacker" \ "square").as[String], "g6")
    assertEquals((row \ "left" \ "application" \ "target" \ "square").as[String], "g5")
    assertEquals((row \ "left" \ "application" \ "moves_uci").as[List[String]], List("g6g5"))
    assertEquals((row \ "right" \ "application" \ "moves_uci").as[List[String]], Nil)
    assertEquals((row \ "right" \ "support" \ "state").as[String], "present")
    assert((row \ "right" \ "application" \ "contact_source_ids").as[List[String]].nonEmpty)
    assertEquals((row \ "right" \ "application" \ "legal_inventory_complete").as[Boolean], true)
    assert(rows.exists(value => (value \ "kind").as[String] == "replacement_support" &&
      (value \ "left" \ "ply_count").as[Int] == 3)) // Preserve the original replacement snapshot.
    val (shortPacket, _, _, _, short) = comparison(fen, pawn.take(6), knight.take(6))
    assert(!(short.get.candidateJson(shortPacket) \ "detail" \ "differences").as[List[JsObject]].exists(value =>
      (value \ "left" \ "application").asOpt[JsObject].nonEmpty)) // White's turn after ...Kg6.

  test("the same replacement knight can lose support later without an initial provider difference"):
    val fen = "7k/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val base = List("g1h1", "d5e4", "d2e4", "h8g7")
    val retained = base ++ List("h1g2", "g7g6", "g2h2")
    val moved = base ++ List("e4f6", "g7g6", "h1h2")
    val (packet, _, _, _, result) = comparison(fen, retained, moved)
    val rows = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]]
    val row = rows.find(value => (value \ "left" \ "application").asOpt[JsObject].nonEmpty).get
    assertEquals((row \ "left" \ "original_provider").as[JsObject], (row \ "right" \ "original_provider").as[JsObject])
    assertEquals((row \ "left" \ "original_provider" \ "square").as[String], "d2")
    assertEquals((row \ "left" \ "support" \ "state").as[String], "present")
    assertEquals((row \ "right" \ "support" \ "state").as[String], "absent")
    assertEquals((row \ "left" \ "application" \ "moves_uci").as[List[String]], Nil)
    assertEquals((row \ "right" \ "application" \ "moves_uci").as[List[String]], List("g6g5"))
    assert(!rows.exists(value => (value \ "kind").as[String] == "replacement_support" &&
      (value \ "left" \ "ply_count").as[Int] == 3))

  test("a repeated king opportunity preserves a later support change and deduplicates its unchanged visits"):
    val fen = "7k/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val shared = List("g1h1", "d5e4", "d2e4", "h8g7", "h1g2", "g7g6", "g2h2", "g6h7")
    val (packet, _, _, _, result) = comparison(fen,
      shared ++ List("h2g2", "h7g6", "g2h2"), shared ++ List("e4f6", "h7g6", "h2g2"))
    val rows = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]].filter(value =>
      (value \ "left" \ "application" \ "attacker" \ "square").asOpt[String].contains("g6"))
    assertEquals(rows.size, 1)
    assertEquals((rows.head \ "left" \ "ply_count").as[Int], 7) // First unchanged state, not both visits.
    assertEquals((rows.head \ "right" \ "ply_count").as[Int], 11) // First later changed state survives.
    assertEquals((rows.head \ "left" \ "application" \ "moves_uci").as[List[String]], Nil)
    assertEquals((rows.head \ "right" \ "application" \ "moves_uci").as[List[String]], List("g6g5"))

  test("a shared pawn loss distinguishes delayed capturer removal from a different pawn gain"):
    val fen = "r1bqkbnr/1pp2ppp/p1p5/4p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 5"
    val recapture = List("f3e5", "d8d4", "b1c3", "d4e5")
    val elsewhere = List("f3e5", "d8d4", "e5f3", "d4e4")
    val (packet, _, left, right, result) = comparison(fen, recapture, elsewhere)
    val row = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]].find(row =>
      (row \ "kind").as[String] == "loss_followup").get
    assertEquals((row \ "original_actor" \ "square").as[String], "f3")
    assertEquals((row \ "original_victim" \ "square").as[String], "e5")
    assertEquals((row \ "left" \ "capturer_removal_state").as[String], "observed")
    assertEquals((row \ "left" \ "capturer_removal_event_id").as[String], left.events.last.id)
    assertEquals((row \ "right" \ "capturer_removal_state").as[String], "not_observed_through_line_end")
    assertEquals((row \ "right" \ "other_target_gain_event_ids").as[List[String]], List(right.events.last.id))
    assertEquals((row \ "right" \ "capturer_end" \ "state").as[String], "present")
    val shared = (row \ "shared_attackers").as[List[JsObject]]
    assertEquals(shared.size, 1)
    assertEquals((shared.head \ "actor" \ "square").as[String], "d4")
    assertEquals((shared.head \ "original_actor" \ "square").as[String], "d8")
    assertEquals((shared.head \ "ply_count").as[Int], 2)
    assertEquals((shared.head \ "left" \ "target" \ "square").as[String], "e5")
    assertEquals((shared.head \ "right" \ "target" \ "square").as[String], "e4")
    assert((shared.head \ "left" \ "source_ids").as[List[String]].nonEmpty)
    assert((shared.head \ "right" \ "source_ids").as[List[String]].nonEmpty)
    val (shortPacket, _, _, _, shorter) = comparison(fen, elsewhere, elsewhere.take(3))
    val shortRow = (shorter.get.candidateJson(shortPacket) \ "detail" \ "differences").as[List[JsObject]].find(row =>
      (row \ "kind").as[String] == "loss_followup").get
    assertEquals((shortRow \ "right" \ "other_target_gain_state").as[String], "not_observed_through_line_end")
    assertEquals((shortRow \ "right" \ "ply_count").as[Int], 3)
    assertEquals((shortRow \ "right" \ "observed_followup_plies").as[Int], 2)
    assertEquals((shortRow \ "shared_attackers").as[List[JsObject]], Nil)

  test("later captures by the same queen do not invent simultaneous targets before the queen moved"):
    val fen = "r1bqkbnr/1pp2ppp/p1p5/4p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 5"
    val recapture = List("f3e5", "d8d4", "b1c3", "d4e5")
    val laterPawn = List("f3e5", "d8d4", "e5f3", "d4c5", "d2d3", "c5c2")
    val (packet, _, _, _, result) = comparison(fen, recapture, laterPawn)
    val row = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]].find(row =>
      (row \ "kind").as[String] == "loss_followup").get
    assertEquals((row \ "shared_attackers").as[List[JsObject]], Nil)

  test("shared geometric attacks can precede a move that unpins the attacker"):
    // Critic-supplied synthetic board: no engine judgment or automatic move discovery.
    val fen = "4k1n1/8/8/2b5/4q2N/5p2/2P5/4R2K w - - 0 1"
    val knight = List("h4f3", "c5e7", "h1h2", "e4f3")
    val pawn = List("h4f3", "g8e7", "h1h2", "e4c2")
    val (packet, prepared, left, right, result) = comparison(fen, knight, pawn)
    val common = prepared.resourceViews(packet)(left.path.positions(1).id.get)
    assert(!common.legalRows.get.exists(row => Set("e4f3", "e4c2")(row.move)))
    List(left -> "e4f3", right -> "e4c2").foreach { (reading, move) =>
      assert(prepared.resourceViews(packet)(reading.path.positions(3).id.get).legalRows.get.exists(_.move == move))
    }
    val row = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]].find(row =>
      (row \ "kind").as[String] == "loss_followup").get
    val shared = (row \ "shared_attackers").as[List[JsObject]]
    assertEquals(shared.size, 1)
    assertEquals((shared.head \ "actor" \ "square").as[String], "e4")
    assertEquals((shared.head \ "ply_count").as[Int], 1)
    assertEquals((shared.head \ "left" \ "target" \ "square").as[String], "f3")
    assertEquals((shared.head \ "right" \ "target" \ "square").as[String], "c2")

  test("the same physical recapture at a later time is not a different loss follow-up"):
    val fen = "2rq1rk1/p3bppp/bpn1p3/1P1n4/3P4/P1N2N2/1BB2PPP/R2QR1K1 b - - 0 15"
    val (packet, _, _, _, result) = comparison(fen,
      List("d5c3", "b2c3"), List("d5c3", "d1d3", "g7g6", "b2c3"))
    val rows = (result.get.candidateJson(packet) \ "detail" \ "differences").as[List[JsObject]]
    assert(rows.exists(row => (row \ "kind").as[String] == "capture_timing"))
    assert(!rows.exists(row => (row \ "kind").as[String] == "loss_followup"))

  test("retaining a third root target question preserves every declared peer's functional comparison"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val roots = List("d2d3", "f1d3", "d1e2") // actual, old reference, new reference
    val captures = roots.zip(List("d3e4", "d3e4", "e2e4")).map((root, recapture) =>
      List(root, "d5e4", recapture))
    val declines = roots.map(root => List(root, "d8e8"))
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val lines = captures ++ declines
    val input = MoveReviewInputAdmission.admitIndependent(history, roots.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    assertEquals(input.suppliedLines.map(_.acceptedMoves), lines)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    val inquiries = roots.indices.toList.map { index =>
      val question = TargetResponseInvestigation.discover(packet, roots(index), prepared)
        .find(question => question.rootMove == roots(index) && question.originalTarget.square.key == "e4").get
      val reading = TargetResponseInvestigation.read(packet, question,
        prepared.certifiedPath(packet, captures(index)).get.line.ref.id,
        prepared.certifiedPath(packet, declines(index)).get.line.ref.id, prepared).get
      CommentaryResponseInquiry(question, state = "observed", reading = Some(reading), readingOwner = Some(packet))
    }
    def output(values: List[CommentaryResponseInquiry]) = RuntimeProtocol.observationsJson(packet,
      responseInquiries = values, prepared = Some(prepared), continuationReadings = prepared.continuationReadings)
    def functionalPairs(value: JsObject) = (value \ "selection" \ "candidates").as[List[JsObject]]
      .filter(row => (row \ "detail" \ "type").asOpt[String].contains("line_continuation_comparison"))
      .map(row => (row \ "line_ids").as[List[String]].toSet).toSet
    val two = functionalPairs(output(inquiries.take(2)))
    val wire = output(inquiries)
    val three = functionalPairs(wire)
    val captureIds = captures.map(moves => prepared.certifiedPath(packet, moves).get.line.ref.id)
    val expected = captureIds.combinations(2).map(_.toSet).toSet
    assert(expected.subsetOf(three))
    assert(two.subsetOf(three))
    val related = (wire \ "selection" \ "response_inquiries").as[List[JsObject]]
    assertEquals(related.size, 3)
    related.foreach(row => assertEquals((row \ "related_inquiry_ids").as[List[String]].toSet,
      inquiries.map(_.question.id).toSet - (row \ "id").as[String]))
    val allowed = inquiries.map(_.reading.get.lineIds.toSet).toSet ++
      inquiries.combinations(2).flatMap(pair => pair.head.reading.get.lineIds.zip(pair(1).reading.get.lineIds)
        .map((left, right) => Set(left, right))).toSet
    assert(three.subsetOf(allowed)) // No capture-versus-decline cross product between different questions.

  test("opposite parent directions share one functional comparison without reversing their PV answers"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val lines = List(List("d2d3", "d5e4", "d3e4"), List("f1d3", "d5e4", "d3e4"))
    val roots = lines.map(_.head)
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, "opposite-pair-directions",
      EngineWorkPurpose.RootSearch, history.initialFen, history.movePrefixUci, history.currentFen,
      EngineRootRestriction.Unrestricted, history.currentFen, Nil, EngineSearchLimits(16, 2000000, 2500, 2), 3500)
    // Invented equal scores supply a matched receipt; no engine is run by this test.
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, lines.map(moves =>
      ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(0), 16)), Some(history)).get
    val admitted = EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get
    val base = MoveReviewInputAdmission.admitIndependent(history, roots.head).get
    val input = MoveReviewInputAdmission.retainAdmittedLines(base, admitted, EvaluationSource.RootSearch)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    val comparisons = PrincipalVariationComparison.readAll(packet, roots,
      List(CommentaryInvestigation.EngineCandidates(work, admitted)), prepared = Some(prepared))
    assertEquals(comparisons.size, 2)
    val wire = RuntimeProtocol.observationsJson(packet, pvComparisons = comparisons, prepared = Some(prepared),
      continuationReadings = prepared.continuationReadings)
    val candidates = (wire \ "selection" \ "candidates").as[List[JsObject]]
    val parents = candidates.filter(row => (row \ "detail" \ "type").asOpt[String].contains("pv_comparison"))
    val functional = candidates.filter(row =>
      (row \ "detail" \ "type").asOpt[String].contains("line_continuation_comparison"))
    assertEquals(functional.size, 1)
    val ids = lines.map(moves => prepared.certifiedPath(packet, moves).get.line.ref.id)
    assertEquals(parents.map(row => (row \ "line_ids").as[List[String]]).toSet, Set(ids, ids.reverse))
    assertEquals((functional.head \ "line_ids").as[List[String]], (parents.head \ "line_ids").as[List[String]])
    parents.foreach(parent => assertEquals((parent \ "detail" \ "continuation_comparison_ids").as[List[String]],
      List((functional.head \ "id").as[String])))
