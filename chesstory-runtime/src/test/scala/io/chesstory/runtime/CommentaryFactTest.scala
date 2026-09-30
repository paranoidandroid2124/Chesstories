package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Previously exposed test boards and supplied paths exercise scoped facts, never engine judgment. */
class CommentaryFactTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val supportFen = "7k/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
  private val rookSupportFen = "6rk/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
  private val directFen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
  private val loss = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3")
  private val bishopRecovery = loss :+ "e4f3"
  private val queenRecovery = loss :+ "d1f3"
  private val origin = Origin(Source("user", "fact-question-test"), Source("user", "fact-path-test"),
    Source("none", "fact-priority-test"), Source("none", "fact-evidence-test"))

  private def white(role: String, square: String): Piece = Piece("white", role, square)
  private def black(role: String, square: String): Piece = Piece("black", role, square)
  private def path(binding: String, moves: List[String], extend: Boolean = false): Path =
    Path(binding, Declared(moves), extend)
  private def support(actor: Piece, target: Piece): Predicate = Predicate("support", Some(actor), Some(target))
  private def capture(actor: Piece, target: Piece): Predicate = Predicate("legal_capture", Some(actor), Some(target))
  private def presence(actor: Piece): Predicate = Predicate("presence", Some(actor))
  private def location(actor: Piece): Predicate = Predicate("location", Some(actor))
  private def direct(actor: Piece, target: Piece): Predicate = Predicate("direct_support_use", Some(actor), Some(target))

  private def packet(fen: String, lines: List[List[String]]): EvidenceBackedJudgmentPacket =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val admitted = lines.map { moves =>
      val continuation = history.admitToFirstAutomaticTerminal(moves).toOption.get
      // A legal prefix is insufficient: every supplied move must survive canonical admission.
      assertEquals(continuation.stepsAfter(history).get.map(_.uci), moves)
      continuation
    }
    val input = MoveReviewInputAdmission.admitIndependent(history, lines.head.head).get
    val retained = MoveReviewInputAdmission.retainLegalContinuations(input, admitted)
    EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(retained)).get

  private def read(source: EvidenceBackedJudgmentPacket, requirements: Requirement*): List[Reading] =
    val requests = List(Request("facts", origin, requirements.toList))
    assert(CommentaryProposal.validRequests(requests))
    CommentaryFactReader.read(source, ObservedLineProtocol.prepare(source), CommentaryProposal.entries(requests))
      .head.readings

  private def result(readings: List[Reading], id: String): Result =
    readings.find(_.requirementId == id).get.result.get

  private def assertValue(readings: List[Reading], id: String, expected: String): Unit =
    val value = result(readings, id)
    assertEquals(value.state, "observed")
    assertEquals(value.samples.flatMap(_.value).distinct, List(expected))

  test("returning to the same support does not prove support throughout the intervening path"):
    val fen = "k7/7p/8/8/8/5N2/7P/6K1 w - - 0 1"
    val moves = List("f3h4", "h7h6", "h4f3")
    val declared = path("trip", moves)
    val predicate = support(white("knight", "f3"), white("pawn", "h2"))
    val requirements = List(
      Fact("before", declared, predicate, Ply(0)),
      Fact("away", declared, predicate, Ply(1)),
      Fact("returned", declared, predicate, Ply(3)),
      Fact("maintained", declared, predicate, Ply(0), Some(Ply(3)), Some("present")),
      FactComparison("same-endpoints", "before", "returned", "same"),
      FactComparison("before-return", "away", "returned", "before"))
    val values = read(packet(fen, List(moves)), requirements*)
    assertValue(values, "before", "present")
    assertValue(values, "away", "absent")
    assertValue(values, "returned", "present")
    assertEquals(result(values, "same-endpoints").state, "supported")
    assertEquals(result(values, "before-return").state, "supported")
    assertEquals(result(values, "maintained").state, "refuted")
    val base = packet(fen, List(moves.take(1)))
    val history = base.assembly.input.positionHistory
    val probes = OccurrenceResources.probePaths(base, List(history.extend(moves).toOption.get))
    val entries = CommentaryProposal.entries(List(Request("facts", origin, requirements)))
    val inspected = CommentaryFactReader.inspect(base, ObservedLineProtocol.prepare(base), entries, probes)("facts")
    values.foreach { reading =>
      assertEquals(inspected(reading.requirementId).state, reading.result.get.state)
      assertEquals(inspected(reading.requirementId).witnesses.flatMap(_.value), reading.result.get.samples.flatMap(_.value))
    }
    assert(inspected("maintained").promotionNeeds.contains(moves))
    assertEquals(entries.head.readings.flatMap(_.result), Nil, "inspection must not publish a fact result")

  test("support follows the original target through relocation and distinguishes the moving provider"):
    val knight = List("g1h1", "d5e4", "d2e4", "g8f8")
    val pawn = List("g1h1", "d5e4", "f3e4", "g8f8")
    val predicate = support(white("knight", "d2"), white("pawn", "f3"))
    val left = path("knight", knight)
    val right = path("pawn", pawn)
    val values = read(packet(rookSupportFen, List(knight, pawn)),
      Fact("left-before", left, predicate, Ply(2)),
      Fact("left-after", left, predicate, Ply(3)),
      Fact("right-before", right, predicate, Ply(2)),
      Fact("right-after", right, predicate, Ply(3)),
      Fact("target-after", right, location(white("pawn", "f3")), Ply(3)),
      Fact("old-pawn-removed", right, presence(white("pawn", "e4")), Ply(3)),
      Fact("removed-provider", right, support(white("pawn", "e4"), white("knight", "d2")), Ply(3)),
      FactComparison("different-after", "left-after", "right-after", "different"))
    assertValue(values, "left-before", "present")
    assertValue(values, "left-after", "absent")
    assertValue(values, "right-before", "present")
    assertValue(values, "right-after", "present")
    assertValue(values, "target-after", "e4")
    assertValue(values, "old-pawn-removed", "removed")
    assertValue(values, "removed-provider", "actor_removed")
    assertEquals(result(values, "different-after").state, "supported")

  test("replacement support and a king's legal capture retain their different subjects and turns"):
    val common = List("g1h1", "d5e4")
    val contact = List("h8g7", "h1g2", "g7g6", "g2h2")
    val knight = common ++ List("d2e4") ++ contact
    val pawn = common ++ List("f3e4") ++ contact
    val target = white("pawn", "g5")
    val kingCapture = capture(black("king", "h8"), target)
    val left = path("knight", knight)
    val right = path("pawn", pawn)
    val values = read(packet(supportFen, List(knight, pawn)),
      Fact("knight-support", left, support(white("knight", "d2"), target), Ply(3)),
      Fact("pawn-support", right, support(white("pawn", "f3"), target), Ply(3)),
      Fact("white-turn-left", left, kingCapture, Ply(6)),
      Fact("white-turn-right", right, kingCapture, Ply(6)),
      Fact("king-left", left, kingCapture, Ply(7)),
      Fact("king-right", right, kingCapture, Ply(7)))
    assertValue(values, "knight-support", "present")
    assertValue(values, "pawn-support", "absent")
    assertValue(values, "white-turn-left", "not_turn")
    assertValue(values, "white-turn-right", "not_turn")
    assertValue(values, "king-left", "illegal")
    assertValue(values, "king-right", "legal")

  test("geometric support becomes direct use only for the actual stationary provider's recapture"):
    val movedProvider = loss ++ List("e4d5", "g8h7", "d5f3")
    val source = packet(directFen, List(bishopRecovery, queenRecovery, movedProvider))
    val provider = white("bishop", "f1")
    val target = white("knight", "f3")
    val left = path("bishop", bishopRecovery)
    val values = read(source,
      Fact("before-loss", left, support(provider, target), Ply(5)),
      Fact("after-loss", left, support(provider, target), Ply(6)),
      Fact("captured-target", left, presence(target), Ply(6)),
      Fact("bishop-used", left, direct(provider, target), Ply(7)),
      Fact("queen-used", path("queen", queenRecovery), direct(provider, target), Ply(7)),
      Fact("provider-moved", path("moved", movedProvider), direct(provider, target), Ply(9)),
      Fact("physical-removal", path("moved", movedProvider), presence(black("rook", "d8")), Ply(9)))
    assertValue(values, "before-loss", "present")
    assertValue(values, "after-loss", "target_removed")
    assertValue(values, "captured-target", "removed")
    assertValue(values, "bishop-used", "true")
    assertValue(values, "queen-used", "false")
    assertValue(values, "provider-moved", "false")
    assertValue(values, "physical-removal", "removed")

  test("en passant preserves physical removal without inventing same-square direct support use"):
    val fen = "7k/8/8/8/3p4/8/4Pb2/3RQ2K b - - 0 1"
    val moves = List("f2e1", "d1e1", "h8h7", "e2e4", "d4e3", "e1e3")
    val declared = path("en-passant", moves)
    val provider = white("rook", "d1")
    val target = white("pawn", "e2")
    val capturer = black("pawn", "d4")
    val removed = Event("capture", 1, "after", 0, 6, Some(provider), Some(capturer))
    val values = read(packet(fen, List(moves)),
      Fact("support", declared, support(provider, target), Ply(4)),
      Fact("pawn-square", declared, location(target), Ply(4)),
      Fact("capturer-square", declared, location(capturer), Ply(5)),
      Fact("physical-removal", declared, presence(capturer), removed, expected = Some("removed")),
      Fact("direct-use", declared, direct(provider, target), Ply(6)))
    assertValue(values, "support", "present")
    assertValue(values, "pawn-square", "e4")
    assertValue(values, "capturer-square", "e3")
    assertEquals(result(values, "physical-removal").state, "supported")
    assertValue(values, "direct-use", "false")

  test("a pinned queen's geometric contacts precede its legal capture after an intervening blocker"):
    val fen = "4k1n1/8/8/2b5/4q2N/5p2/2P5/4R2K w - - 0 1"
    val knight = List("h4f3", "c5e7", "h1h2", "e4f3")
    val pawn = List("h4f3", "g8e7", "h1h2", "e4c2")
    val queen = black("queen", "e4")
    val left = path("knight", knight)
    val right = path("pawn", pawn)
    val takeKnight = capture(queen, white("knight", "h4"))
    val takePawn = capture(queen, white("pawn", "c2"))
    val values = read(packet(fen, List(knight, pawn)),
      Fact("contact-knight", left, Predicate("control", Some(queen), square = Some("f3")), Ply(1)),
      Fact("contact-pawn", right, Predicate("control", Some(queen), square = Some("c2")), Ply(1)),
      Fact("pinned-knight", left, takeKnight, Ply(1)),
      Fact("pinned-pawn", right, takePawn, Ply(1)),
      Fact("unblocked-knight", left, takeKnight, Ply(3)),
      Fact("unblocked-pawn", right, takePawn, Ply(3)))
    assertValue(values, "contact-knight", "present")
    assertValue(values, "contact-pawn", "present")
    assertValue(values, "pinned-knight", "illegal")
    assertValue(values, "pinned-pawn", "illegal")
    assertValue(values, "unblocked-knight", "legal")
    assertValue(values, "unblocked-pawn", "legal")

  test("an exact declared endpoint cannot borrow a longer path and Any preserves its unknown member"):
    val full = List("g1h1", "d5e4", "d2e4", "h8g7", "h1g2", "g7g6", "g2h2", "g6f5", "h2h1")
    val short = full.take(7)
    val declared = path("one", short)
    val predicate = support(white("knight", "d2"), white("pawn", "g5"))
    val values = read(packet(supportFen, List(full)),
      Fact("known", declared, predicate, Ply(7), expected = Some("present")),
      Fact("contradicted", declared, predicate, Ply(7), expected = Some("absent")),
      Fact("unknown", declared, predicate, Ply(9), expected = Some("present")),
      Fact("extended", path("extended", short, extend = true), predicate, Ply(9), expected = Some("present")),
      FactGroup("any", "any", List("known", "unknown")),
      FactGroup("all", "all", List("known", "unknown")),
      FactGroup("false-and-unknown", "all", List("contradicted", "unknown")),
      FactGroup("false-or-unknown", "any", List("contradicted", "unknown")))
    assertEquals(result(values, "known").state, "supported")
    assertEquals(result(values, "unknown").state, "partial")
    assert(result(values, "unknown").needs.nonEmpty)
    assert(result(values, "unknown").samples.forall(_.value.isEmpty))
    assertEquals(result(values, "extended").state, "supported")
    assertEquals(result(values, "any").state, "supported")
    assertEquals(result(values, "all").state, "partial")
    assertEquals(result(values, "false-and-unknown").state, "refuted")
    assertEquals(result(values, "false-or-unknown").state, "partial")

  test("All cannot join conflicting executions while independent bindings can describe two alternatives"):
    val source = packet(directFen, List(bishopRecovery, queenRecovery))
    val bishop = white("bishop", "f1")
    val queen = white("queen", "d1")
    val rook = black("rook", "d8")
    val bishopEvent = Event("capture", 1, "after", 0, 7, Some(bishop), Some(rook))
    val queenEvent = Event("capture", 1, "after", 0, 7, Some(queen), Some(rook))
    val values = read(source,
      Fact("bishop", path("same", bishopRecovery), location(bishop), bishopEvent, expected = Some("f3")),
      Fact("queen", path("same", queenRecovery), location(queen), queenEvent, expected = Some("f3")),
      Fact("bishop-root", path("same", bishopRecovery), presence(bishop), Ply(0), expected = Some("present")),
      FactGroup("conflicting-all", "all", List("bishop", "queen")),
      FactGroup("either", "any", List("bishop", "queen")),
      FactGroup("root-conflict", "all", List("bishop-root", "queen")),
      FactGroup("root-or-queen", "any", List("bishop-root", "queen")),
      FactGroup("compatible-nested", "all", List("root-or-queen", "queen")),
      Fact("independent-bishop", path("left", bishopRecovery), location(bishop), bishopEvent, expected = Some("f3")),
      Fact("independent-queen", path("right", queenRecovery), location(queen), queenEvent, expected = Some("f3")),
      FactGroup("two-alternatives", "all", List("independent-bishop", "independent-queen")))
    assertEquals(result(values, "bishop").state, "supported")
    assertEquals(result(values, "queen").state, "supported")
    assertEquals(result(values, "conflicting-all").state, "refuted")
    assertEquals(result(values, "conflicting-all").reason, Some("incompatible_paths"))
    assertEquals(result(values, "either").state, "supported")
    assertEquals(result(values, "bishop-root").state, "supported")
    assertEquals(result(values, "bishop-root").samples.map(_.moves).distinct, List(bishopRecovery))
    assertEquals(result(values, "root-conflict").state, "refuted")
    assertEquals(result(values, "root-conflict").reason, Some("incompatible_paths"))
    assertEquals(result(values, "root-or-queen").state, "supported")
    val nested = result(values, "compatible-nested")
    assertEquals(nested.state, "supported")
    assert(nested.proofs.nonEmpty)
    assert(nested.proofs.forall(_.bindings.get("same").contains(queenRecovery)))
    assertEquals(result(values, "two-alternatives").state, "supported")
    assert(result(values, "two-alternatives").proofs.exists(proof =>
      proof.bindings.get("left").contains(bishopRecovery) && proof.bindings.get("right").contains(queenRecovery)))

  test("capture event phases retain the actual loss-before-recovery order"):
    val declared = path("one", bishopRecovery)
    val target = white("knight", "f3")
    val rook = black("rook", "d8")
    val bishop = white("bishop", "f1")
    val beforeLoss = Event("capture", 1, "before", 0, 7, Some(rook), Some(target))
    val afterLoss = Event("capture", 1, "after", 0, 7, Some(rook), Some(target))
    val afterRecovery = Event("capture", 1, "after", 0, 7, Some(bishop), Some(rook))
    val destination = Event("move", 1, "before", 0, 7, Some(bishop), square = Some("f3"))
    val values = read(packet(directFen, List(bishopRecovery)),
      Fact("before-loss", declared, presence(target), beforeLoss),
      Fact("after-loss", declared, presence(target), afterLoss),
      Fact("after-recovery", declared, presence(rook), afterRecovery),
      Fact("before-arrival", declared, Predicate("location", Some(bishop)), destination),
      Fact("after-arrival", declared, Predicate("location", Some(bishop)), destination.copy(phase = "after")),
      Fact("unobserved-arrival", declared, presence(bishop), destination.copy(square = Some("h7"))),
      FactComparison("loss-before-recovery", "after-loss", "after-recovery", "before"),
      FactComparison("reverse-order", "after-loss", "after-recovery", "after"),
      FactComparison("not-simultaneous", "after-loss", "after-recovery", "same_time"))
    assertValue(values, "before-loss", "present")
    assertValue(values, "after-loss", "removed")
    assertValue(values, "after-recovery", "removed")
    assertEquals(result(values, "before-loss").samples.map(_.fromPly).distinct, List(5))
    assertEquals(result(values, "after-loss").samples.map(_.fromPly).distinct, List(6))
    assertEquals(result(values, "after-recovery").samples.map(_.fromPly).distinct, List(7))
    assertValue(values, "before-arrival", "e4")
    assertValue(values, "after-arrival", "f3")
    assertEquals(result(values, "before-arrival").samples.map(_.fromPly).distinct, List(6))
    assertEquals(result(values, "after-arrival").samples.map(_.fromPly).distinct, List(7))
    assertEquals(result(values, "unobserved-arrival").state, "not_applicable")
    assert(!CommentaryFact.validPoint(destination.copy(kind = "capture")))
    assertEquals(result(values, "loss-before-recovery").state, "supported")
    assertEquals(result(values, "reverse-order").state, "refuted")
    assertEquals(result(values, "not-simultaneous").state, "refuted")
    val base = packet(directFen, List(bishopRecovery.take(1)))
    val history = base.assembly.input.positionHistory
    val probes = OccurrenceResources.probePaths(base, List(history.extend(bishopRecovery).toOption.get))
    val entries = CommentaryProposal.entries(List(Request("events", origin, List(
      Fact("before", path("one", bishopRecovery.take(1), true), presence(target), beforeLoss),
      Fact("absent", path("one", bishopRecovery.take(1), true), presence(bishop), destination.copy(square = Some("h7"))),
      Fact("use", declared, direct(bishop, target), Ply(7))))))
    val inspected = CommentaryFactReader.inspect(base, ObservedLineProtocol.prepare(base), entries, probes)("events")
    assertEquals(inspected("before").witnesses.map(_.fromPly).distinct, List(5))
    assert(inspected("before").promotionNeeds.contains(bishopRecovery.take(6)),
      "an event-before reading must retain the move witnessing the event")
    assertEquals(inspected("absent").state, "not_applicable")
    assert(inspected("absent").promotionNeeds.contains(bishopRecovery))
    assert(inspected("use").needs.exists(_.reason == "probe_requires_admitted_line"))
    assert(inspected("use").promotionNeeds.contains(bishopRecovery))

  test("missing geometric evidence is partial and cannot manufacture a supported loss comparison"):
    val moves = List("g1h1", "d5e4", "d2e4")
    val original = packet(rookSupportFen, List(moves))
    val prepared = ObservedLineProtocol.prepare(original)
    val originalPath = prepared.paths.find(_.moves == moves).get
    val before = originalPath.positions(2)
    // The same deliberately missing relation is exercised in LineContinuationComparisonTest.
    val missing = original.resources.find(_.position == before).get.records.find(_.payload match
      case fact: RelationFactEvidence => fact.detail match
        case RelationWitnessDetail.GeometricControl(chess.Color.White, from, _, to) =>
          from.key == "d2" && to.key == "f3"
        case _ => false
      case _ => false).get
    val assembly = original.assembly.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(
      original.evidenceGraph.records.filterNot(_ == missing)))
    val incomplete = EvidenceBackedJudgmentPacket.fromAssembly(assembly).get
    val declared = path("one", moves)
    val predicate = support(white("knight", "d2"), white("pawn", "f3"))
    val requirements = List(
      Fact("before", declared, predicate, Ply(2)),
      Fact("after", declared, predicate, Ply(3)),
      FactComparison("change", "before", "after", "different"))
    val values = read(incomplete, requirements*)
    assertEquals(result(values, "before").state, "partial")
    assert(result(values, "before").samples.forall(_.value.isEmpty))
    assertValue(values, "after", "absent")
    assertEquals(result(values, "change").state, "partial")
    val probes = OccurrenceResources.probePaths(incomplete,
      List(incomplete.assembly.input.positionHistory.extend(moves).toOption.get))
    val inspected = CommentaryFactReader.inspect(incomplete, ObservedLineProtocol.prepare(incomplete),
      CommentaryProposal.entries(List(Request("facts", origin, requirements))), probes)("facts")
    assertEquals(inspected("before").state, "partial")
    assert(inspected("before").witnesses.forall(_.value.isEmpty))
    assertEquals(inspected("change").state, "partial", "a raw replay cannot repair an admitted source silently")

    // The probe is shorter than the admitted line but covers the same closed condition.
    // Its complete raw inventory must not replace the missing admitted support witness.
    val prefix = moves.take(2)
    val shortProbes = OccurrenceResources.probePaths(incomplete,
      List(incomplete.assembly.input.positionHistory.extend(prefix).toOption.get))
    assertEquals(CommentaryFactAtoms.readProbe(shortProbes.head.at(2).get, predicate).value, Some("present"))
    val shortFact = Fact("short-before", path("short", prefix), predicate, Ply(2))
    val admittedReading = result(read(incomplete, shortFact), shortFact.id)
    val shortInspection = CommentaryFactReader.inspect(incomplete, ObservedLineProtocol.prepare(incomplete),
      CommentaryProposal.entries(List(Request("short", origin, List(shortFact)))), shortProbes)("short")(shortFact.id)
    assertEquals(shortInspection.state, "partial")
    assertEquals(shortInspection.needs, admittedReading.needs)
    assert(shortInspection.witnesses.forall(_.value.isEmpty))
    assertEquals(shortInspection.conditionPrefixes, List(prefix))
    assertEquals(shortInspection.promotionNeeds, Nil, "the existing admitted source must remain authoritative")

    // Both an uncertified prepared path and a registered path omitted by prepare retain
    // ownership of their old prefixes. New suffixes and different replies remain readable.
    // Keep the reviewed move's required edge intact while damaging a separate path.
    val ownershipPacket = packet(rookSupportFen, List(List("g1h2"), moves))
    val ownershipPath = ObservedLineProtocol.prepare(ownershipPacket).paths.find(_.moves == moves).get
    val certificate = ownershipPath.certified.get._1
    val firstEdge = ownershipPacket.evidenceGraph.records.find(_.ref.id == ownershipPath.ids.head).get
    List("uncertified" -> certificate, "missing-first-edge" -> firstEdge).foreach { (label, removed) =>
      val unavailable = EvidenceBackedJudgmentPacket.fromAssembly(ownershipPacket.assembly.copy(
        evidenceGraph = TypedEvidenceGraph.empty.addAll(ownershipPacket.evidenceGraph.records.filterNot(_ == removed)))).get
      val unavailablePaths = ObservedLineProtocol.prepare(unavailable)
      assert(unavailable.assembly.input.legalLines.exists(_.moves == moves), label)
      if label == "uncertified" then
        assert(unavailablePaths.paths.exists(p => p.sourceMoves == moves && p.certified.isEmpty), label)
      else assert(!unavailablePaths.paths.exists(_.sourceMoves == moves), label)
      val future = moves :+ "g8f8"
      val branch = List("g1h1", "g8f8")
      val history = unavailable.assembly.input.positionHistory
      val raw = OccurrenceResources.probePaths(unavailable,
        List(prefix, future, branch).map(ms => history.extend(ms).toOption.get))
      val rook = black("rook", "g8")
      val oldCapture = Event("capture", 1, "after", 1, 2,
        Some(black("pawn", "d5")), Some(white("pawn", "e4")))
      val newMove = Event("move", 1, "after", 3, 4, Some(rook), square = Some("f8"))
      val checks = List(
        shortFact,
        Fact("past-in-future", path("future", future), predicate, Ply(2)),
        Fact("old-event", path("short", prefix), presence(white("pawn", "e4")), oldCapture),
        Fact("old-event-in-future", path("future", future), presence(white("pawn", "e4")), oldCapture),
        Fact("new-point", path("future", future), location(rook), Ply(4)),
        Fact("new-event", path("future", future), location(rook), newMove),
        Fact("before-new-event", path("future", future), location(rook), newMove.copy(phase = "before")),
        Fact("new-branch", path("branch", branch), location(rook), Ply(2)))
      val request = Request("ownership", origin, checks)
      assert(CommentaryProposal.validRequests(List(request)), label)
      val inspected = CommentaryFactReader.inspect(unavailable, unavailablePaths,
        CommentaryProposal.entries(List(request)), raw)("ownership")
      List(shortFact.id, "past-in-future", "before-new-event").foreach { id =>
        assertEquals(inspected(id).state, "partial", s"$label/$id")
        assert(inspected(id).witnesses.forall(_.value.isEmpty), s"$label/$id")
        assert(inspected(id).needs.exists(_.reason == "known_prefix_unavailable"), s"$label/$id")
      }
      List("old-event", "old-event-in-future").foreach { id =>
        assertEquals(inspected(id).state, "partial", s"$label/$id")
        assert(inspected(id).witnesses.isEmpty, s"$label/$id")
        assert(inspected(id).needs.exists(_.reason == "event_source_unavailable"), s"$label/$id")
      }
      List("new-point", "new-event", "new-branch").foreach { id =>
        assertEquals(inspected(id).state, "observed", s"$label/$id")
        assertEquals(inspected(id).witnesses.flatMap(_.value).distinct, List("f8"), s"$label/$id")
      }
      assertEquals(inspected(shortFact.id).promotionNeeds, Nil, label)
      assertEquals(inspected("old-event").promotionNeeds, Nil, label)
      assertEquals(inspected("new-point").promotionNeeds, List(future), label)
      assertEquals(inspected("new-event").promotionNeeds, List(future), label)
      assertEquals(inspected("before-new-event").witnesses.map(_.fromPly), List(3), label)
      assertEquals(inspected("before-new-event").promotionNeeds, List(future), label)
      assertEquals(inspected("new-branch").promotionNeeds, List(branch), label)
    }

  test("pawn properties and empty advance passages retain identity without claiming legal moves"):
    val fen = "7k/2p5/8/3p4/2P5/P7/P7/7K w - - 0 1"
    val moves = List("c4d5", "c7c6", "d5c6")
    val declared = path("pawn-files", moves)
    val runner = white("pawn", "c4")
    def pawn(actor: Piece, property: String) = Predicate("pawn_property", Some(actor), property = Some(property))
    def advance(actor: Piece, square: String) = Predicate("pawn_advance", Some(actor), square = Some(square))
    def file(name: String) = Predicate("file_occupancy", property = Some(name))
    val values = read(packet(fen, List(moves)),
      Fact("doubled", declared, pawn(white("pawn", "a2"), "doubled"), Ply(0)),
      Fact("isolated", declared, pawn(white("pawn", "a3"), "isolated"), Ply(0)),
      Fact("passed-a", declared, pawn(white("pawn", "a3"), "passed"), Ply(0)),
      Fact("single-c", declared, pawn(runner, "doubled"), Ply(0)),
      Fact("blocked-c", declared, pawn(runner, "passed"), Ply(0)),
      Fact("passed-c", declared, pawn(runner, "passed"), Ply(3)),
      Fact("removed-pawn", declared, pawn(black("pawn", "d5"), "passed"), Ply(1)),
      Fact("king-not-pawn", declared, pawn(white("king", "h1"), "isolated"), Ply(0)),
      Fact("removed-advance", declared, advance(black("pawn", "d5"), "d4"), Ply(1)),
      Fact("king-not-advance", declared, advance(white("king", "h1"), "h2"), Ply(0)),
      Fact("a-white", declared, file("a"), Ply(0)),
      Fact("c-both", declared, file("c"), Ply(0)),
      Fact("d-black", declared, file("d"), Ply(0)),
      Fact("e-open", declared, file("e"), Ply(0)),
      Fact("c-black-after", declared, file("c"), Ply(1)),
      Fact("d-white-after", declared, file("d"), Ply(1)),
      Fact("c-white-final", declared, file("c"), Ply(3)))
    List("doubled", "isolated", "passed-a", "passed-c").foreach(assertValue(values, _, "true"))
    List("single-c", "blocked-c").foreach(assertValue(values, _, "false"))
    assertValue(values, "removed-pawn", "actor_removed")
    assertValue(values, "king-not-pawn", "not_pawn")
    assertValue(values, "removed-advance", "actor_removed")
    assertValue(values, "king-not-advance", "not_pawn")
    assertValue(values, "a-white", "white_pawns_only")
    assertValue(values, "c-both", "both_sides")
    assertValue(values, "d-black", "black_pawns_only")
    assertValue(values, "e-open", "open")
    assertValue(values, "c-black-after", "black_pawns_only")
    assertValue(values, "d-white-after", "white_pawns_only")
    assertValue(values, "c-white-final", "white_pawns_only")

    // Nc3 blocks c2-c3 and the transit square of c2-c4; Nb1 restores both passages.
    // The c2 pawn also shields Kb2 from Rh2, so the initially empty passage is not a legal move.
    val passageMoves = List("b1c3", "a8b8", "c3b1")
    val passageSource = packet("k7/8/8/8/8/8/1KP4r/1N6 w - - 0 1", List(passageMoves))
    val passagePath = path("passage", passageMoves)
    val cPawn = white("pawn", "c2")
    val passage = advance(cPawn, "c3")
    val passageValues = read(passageSource,
      Fact("open", passagePath, passage, Ply(0)),
      Fact("pinned", passagePath, Predicate("legal_move", Some(cPawn), square = Some("c3")), Ply(0)),
      Fact("blocked", passagePath, passage, Ply(1)),
      Fact("double-blocked", passagePath, advance(cPawn, "c4"), Ply(1)),
      Fact("reopened", passagePath, passage, Ply(3)),
      Fact("other-turn", passagePath, Predicate("legal_move", Some(cPawn), square = Some("c3")), Ply(3)))
    assertValue(passageValues, "open", "present")
    assertValue(passageValues, "pinned", "illegal")
    assertValue(passageValues, "blocked", "absent")
    assertValue(passageValues, "double-blocked", "absent")
    assertValue(passageValues, "reopened", "present")
    assertValue(passageValues, "other-turn", "not_turn")

    val preparedPassage = ObservedLineProtocol.prepare(passageSource)
    val root = preparedPassage.paths.find(_.moves == passageMoves).get.positions.head
    val missing = passageSource.resources.find(_.position == root).get.records.find(_.payload match
      case fact: RelationFactEvidence => fact.detail match
        case RelationWitnessDetail.PawnAdvanceAffordance(chess.White, pawn, destination, _) =>
          pawn.key == "c2" && destination.key == "c3"
        case _ => false
      case _ => false).get
    val incomplete = EvidenceBackedJudgmentPacket.fromAssembly(passageSource.assembly.copy(
      evidenceGraph = TypedEvidenceGraph.empty.addAll(passageSource.evidenceGraph.records.filterNot(_ == missing)))).get
    val unknown = result(read(incomplete, Fact("missing", passagePath, passage, Ply(0))), "missing")
    assertEquals(unknown.state, "partial")
    assert(unknown.samples.forall(_.value.isEmpty))

  test("promotion preserves the original actor while changing role control check and legal moves"):
    val fen = "7k/P7/8/8/8/8/8/1R5K w - - 0 1"
    val queenMoves = List("a7a8q", "h8h7")
    val knightMoves = List("a7a8n", "h8h7")
    val queen = path("queen-promotion", queenMoves)
    val knight = path("knight-promotion", knightMoves)
    val actor = white("pawn", "a7")
    val role = Predicate("role", Some(actor))
    val check = Predicate("check", side = Some("black"))
    val promoteQueen = Predicate("legal_move", Some(actor), square = Some("a8"), property = Some("queen"))
    val promoteKnight = Predicate("legal_move", Some(actor), square = Some("a8"), property = Some("knight"))
    val knightMove = Predicate("legal_move", Some(actor), square = Some("b6"))
    val promotion = Event("promotion", 1, "after", 0, 2, Some(actor))
    val values = read(packet(fen, List(queenMoves, knightMoves)),
      Fact("original-role", queen, role, Ply(0)),
      Fact("queen-role", queen, role, promotion),
      Fact("knight-role", knight, role, promotion),
      Fact("promoted-location", knight, location(actor), Ply(1)),
      Fact("promoted-not-pawn", knight, Predicate("pawn_property", Some(actor), property = Some("passed")), Ply(1)),
      Fact("promoted-no-advance", knight, Predicate("pawn_advance", Some(actor), square = Some("a8")), Ply(1)),
      Fact("queen-legal", queen, promoteQueen, Ply(0)),
      Fact("knight-legal", knight, promoteKnight, Ply(0)),
      Fact("wrong-turn", knight, knightMove, Ply(1)),
      Fact("new-role-move", knight, knightMove, Ply(2)),
      Fact("old-role-move", knight, Predicate("legal_move", Some(actor), square = Some("a7")), Ply(2)),
      Fact("knight-control", knight, Predicate("control", Some(actor), square = Some("b6")), Ply(2)),
      Fact("not-rook-control", knight, Predicate("control", Some(actor), square = Some("b8")), Ply(2)),
      Fact("before-check", queen, check, Ply(0)),
      Fact("queen-check", queen, check, Ply(1)),
      Fact("escaped-check", queen, check, Ply(2)),
      Fact("knight-no-check", knight, check, Ply(1)))
    assertValue(values, "original-role", "pawn")
    assertValue(values, "queen-role", "queen")
    assertValue(values, "knight-role", "knight")
    assertValue(values, "promoted-location", "a8")
    assertValue(values, "promoted-not-pawn", "not_pawn")
    assertValue(values, "promoted-no-advance", "not_pawn")
    List("queen-legal", "knight-legal", "new-role-move").foreach(assertValue(values, _, "legal"))
    assertValue(values, "wrong-turn", "not_turn")
    assertValue(values, "old-role-move", "illegal")
    assertValue(values, "knight-control", "present")
    assertValue(values, "not-rook-control", "absent")
    assertValue(values, "before-check", "absent")
    assertValue(values, "queen-check", "present")
    assertValue(values, "escaped-check", "absent")
    assertValue(values, "knight-no-check", "absent")

  test("a large shared dependency DAG remains closed and rejects an incompatible final path"):
    val source = packet(directFen, List(bishopRecovery, queenRecovery))
    val atoms: List[Requirement] = List(
      Fact("a", path("same", bishopRecovery), location(white("bishop", "f1")), Ply(7),
        expected = Some("f3")),
      Fact("b", path("same", queenRecovery), location(white("queen", "d1")), Ply(7),
        expected = Some("f3")))
    val groups = (1 to 500).map { index =>
      val members = if index == 1 then List("a")
        else if index == 2 then List("a", "g1")
        else List(s"g${index - 2}", s"g${index - 1}")
      FactGroup(s"g$index", "all", members)
    }.toList
    val requirements = atoms ++ groups :+ FactGroup("conflict", "all", List("g500", "b"))
    val values = read(source, requirements*)
    assertEquals(values.size, 503)
    assertEquals(result(values, "g500").state, "supported")
    assertEquals(result(values, "conflict").state, "refuted")
    assertEquals(result(values, "conflict").reason, Some("incompatible_paths"))
