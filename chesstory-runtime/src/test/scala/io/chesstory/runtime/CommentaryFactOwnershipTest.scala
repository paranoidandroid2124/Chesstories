package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

class CommentaryFactOwnershipTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  private val origin = Origin(Source("user", "ownership-test"), Source("user", "ownership-test"),
    Source("none", "ownership-test"), Source("none", "ownership-test"))
  private def packet(fen: String, lines: List[List[String]]): EvidenceBackedJudgmentPacket =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, lines.head.head).get
    val retained = MoveReviewInputAdmission.retainLegalContinuations(input,
      lines.map(moves => history.admitToFirstAutomaticTerminal(moves).toOption.get))
    EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(retained)).get
  private def read(packet: EvidenceBackedJudgmentPacket, requirements: Requirement*): Map[String, Result] =
    val requests = List(Request("ownership", origin, requirements.toList))
    assert(validRequests(requests))
    CommentaryFactReader.read(packet, ObservedLineProtocol.prepare(packet), entries(requests)).head.readings
      .map(reading => reading.requirementId -> reading.result.get).toMap

  test("a shared physical prefix cannot lend another line's material ledger to direct use or event selection"):
    val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    val loss = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3")
    val full = loss :+ "e4f3"
    val missing = loss :+ "d1f3"
    val source = packet(fen, List(full, missing))
    val missingId = ObservedLineProtocol.prepare(source).paths.find(_.moves == missing).get.line.ref.id
    val incomplete = lila.chessjudgment.testing.CheckResponseEvidenceFixture.withoutMaterial(source, Some(missingId))
    val bishop = Piece("white", "bishop", "f1")
    val knight = Piece("white", "knight", "f3")
    val predicate = Predicate("direct_support_use", Some(bishop), Some(knight))
    val good = Fact("full", Path("full", Declared(full)), predicate, Ply(6))
    val absent = Fact("missing", Path("missing", Declared(missing)), predicate, Ply(6))
    List(List(good, absent), List(absent, good)).foreach { requirements =>
      val values = read(incomplete, requirements*)
      assertEquals(values("full").state, "observed")
      assertEquals(values("full").samples.flatMap(_.value), List("false"))
      assertEquals(values("missing").state, "partial")
      assert(values("missing").samples.forall(_.value.isEmpty))
      assert(values("missing").needs.exists(_.reason == "missing_material_ledger"))
    }
    val values = read(incomplete,
      Fact("capture", absent.path, Predicate("presence", Some(knight)), Event("capture", 1, "after", 5, 6)),
      Fact("move", absent.path, Predicate("presence", Some(knight)), Event("move", 1, "after", 5, 6)))
    assertEquals(values("capture").state, "partial")
    assert(values("capture").needs.exists(_.reason == "event_material_unavailable"))
    assertEquals(values("move").samples.flatMap(_.value), List("removed"))

  test("compact samples name the exact path and preserve an event edge beyond the observed board"):
    val moves = List("e2e4", "e7e5", "g1f3")
    val source = packet(chess.variant.Standard.initialFen.value, List(moves))
    val path = ObservedLineProtocol.prepare(source).paths.find(_.moves == moves).get
    val pawn = Piece("white", "pawn", "e2")
    val fact = Fact("before_move", Path("line", Declared(moves)), Predicate("presence", Some(pawn)),
      Event("move", 1, "before", 0, 1, actor = Some(pawn), square = Some("e4")))
    val result = read(source, fact)(fact.id)
    assertEquals(result.state, "observed")
    val sample = result.samples.head
    val json = sample.json
    assertEquals((json \ "line_id").as[String], path.line.ref.id)
    assertEquals((json \ "prefix_ply_count").as[Int], sample.moves.size)
    assert(sample.moves.size > sample.throughPly, "the event edge is not the earlier board's ply")
    assertEquals(path.moves.take(sample.moves.size), sample.moves)
    assertEquals((json \ "from_ply").as[Int], 0)
    assertEquals((json \ "through_ply").as[Int], 0)
    assertEquals(Set("moves_uci", "position_ids", "occurrence_ids").intersect(json.keys), Set.empty[String])

  test("an unadmitted declared future remains open even when its present fact is known"):
    val source = packet(chess.variant.Standard.initialFen.value, List(List("g1f3")))
    val knight = Predicate("presence", Some(Piece("white", "knight", "g1")))
    val values = read(source,
      Fact("known", Path("known", Declared(List("g1f3"))), knight, Ply(0)),
      Fact("future", Path("future", Declared(List("g1f3", "g7g6"))), knight, Ply(0), expected = Some("present")),
      FactComparison("equal", "known", "future", "same"))
    assertEquals(values("known").state, "observed")
    assertEquals(values("future").state, "partial")
    assert(values("future").needs.exists(_.reason == "declared_path_unavailable"))
    assertEquals(values("future").proofs, Nil)
    assertEquals(values("equal").state, "partial")
    assertEquals(values("equal").proofs, Nil)

  test("a longer incomplete ledger cannot erase a shorter complete event or known interval samples"):
    val moves = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3", "e4f3")
    val source = packet("3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1", List(moves.take(3), moves))
    val paths = ObservedLineProtocol.prepare(source).paths
    val longId = paths.find(_.moves == moves).get.line.ref.id
    val shortId = paths.find(_.moves == moves.take(3)).get.line.ref.id
    val incomplete = lila.chessjudgment.testing.CheckResponseEvidenceFixture.withoutMaterial(source, Some(longId))
    val declared = Path("one", Declared(moves.take(1)), extend = true)
    val values = read(incomplete,
      Fact("event", declared, Predicate("presence", Some(Piece("white", "pawn", "e4"))),
        Event("capture", 1, "after", 0, 7), expected = Some("removed")),
      Fact("interval", declared, Predicate("direct_support_use", Some(Piece("white", "bishop", "f1")),
        Some(Piece("white", "knight", "f3"))), Ply(0), Some(Ply(7)), Some("false")))
    assertEquals(values("event").state, "supported")
    assert(values("event").proofs.exists(proof => proof.sampleIds.exists(id =>
      values("event").samples.exists(sample => sample.id == id && sample.lineId == shortId))))
    assertEquals(values("interval").state, "partial")
    assert(values("interval").samples.exists(sample => sample.lineId == shortId && sample.value.contains("false")))
    assert(values("interval").samples.filter(_.lineId == longId).forall(_.value.isEmpty))
    assert(values("interval").needs.exists(_.reason == "missing_material_ledger"))

  test("an observed continuation premise supplies a path but does not certify its proposed support"):
    val moves = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3", "e4f3")
    val source = packet("3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1", List(moves))
    val prepared = ObservedLineProtocol.prepare(source)
    val path = prepared.paths.find(_.moves == moves).get
    val premise = CommentaryContinuationPremise(LineContinuationInvestigation.ChoicePremise(
      "support_observation", List(path.ids(5)), requirementId = Some("same-local-requirement")), List(path.line.ref.id))
    val question = CommentaryContinuationQuestion("owned-inquiry", path.ids.head, path.positions.head.id.get,
      path.positions(5).id.get, moves.take(5), path.ids.take(5), List(moves(5)), List(path.line.ref.id),
      Nil, Map.empty, List(premise))
    val inquiry = CommentaryContinuationInquiry(question, premiseObservations = List(
      CommentaryPremiseObservation(premise, "observed", List(path.line.ref.id), Nil)))
    val target = Piece("white", "knight", "f3")
    val fact = Fact("support", Path("line", Premise(question.id, premise.id(question.id))),
      Predicate("support", Some(Piece("white", "bishop", "f1")), Some(target)), Ply(6), expected = Some("present"))
    val foreign = fact.copy(id = "foreign", path = Path("other", Premise("other-inquiry", premise.id(question.id))))
    val requests = List(Request("ownership", origin, List(fact, foreign)))
    assert(validRequests(requests))
    val readings = CommentaryFactReader.read(source, prepared, entries(requests), List(inquiry)).head.readings
    assertEquals(readings.head.state, "refuted")
    assertEquals(readings.head.result.get.samples.flatMap(_.value), List("target_removed"))
    assertEquals(readings(1).state, "partial")
    assert(readings(1).result.get.needs.exists(_.reason == "premise_owner_unavailable"))
    val missing = inquiry.copy(premiseObservations = List(
      CommentaryPremiseObservation(premise, "unknown", Nil, List(path.line.ref.id))))
    val point = fact.copy(id = "past", path = fact.path.copy(extend = true), at = Ply(0))
    val future = point.copy(id = "future", at = Ply(7))
    val missingRequests = List(Request("missing", origin, List(point, future)))
    val unresolved = CommentaryFactReader.read(source, prepared, entries(missingRequests), List(missing)).head.readings
    assertEquals(unresolved.head.result.get.needs.map(_.kind), List("input"))
    assertEquals(unresolved(1).result.get.needs.map(_.kind), List("continuation"))
    assert(unresolved(1).result.get.needs.forall(need => need.horizon.exists(_ > need.prefixMoves.size)))
