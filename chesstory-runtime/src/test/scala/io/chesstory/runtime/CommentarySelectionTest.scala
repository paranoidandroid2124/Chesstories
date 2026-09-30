package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, EngineLine }
import play.api.libs.json.*

/** Small synthetic inputs exercise selection contracts, not engine quality. */
class CommentarySelectionTest extends munit.FunSuite:
  private val initial = chess.variant.Standard.initialFen.value
  private def packet(ctx: JudgmentAssemblyContext): EvidenceBackedJudgmentPacket =
    EvidenceBackedJudgmentPacket.fromAssembly(ctx).get
  private def assemble(history: CanonicalPositionHistory, move: String, paths: List[List[String]] = Nil): JudgmentAssemblyContext =
    MoveReviewAssembler.assembleOccurrences(MoveReviewInputAdmission.admitIndependent(history, move, paths).get)
  private def fromFen(fen: String, move: String): JudgmentAssemblyContext =
    assemble(CanonicalPositionHistory.from(fen, Nil, fen).toOption.get, move)
  private def rows(selection: JsObject): List[JsObject] = (selection \ "candidates").as[List[JsObject]]
  private def scenes(selection: JsObject): List[JsObject] = (selection \ "scenes").as[List[JsObject]]
  private def facts(selection: JsObject): List[JsObject] =
    rows(selection).filter(row => (row \ "kind").as[String] == "local_fact")
  private def comparisons(selection: JsObject): List[JsObject] =
    rows(selection).filter(row => (row \ "kind").as[String] == "comparison")
  private def assertSelectionV2(selection: JsObject): Unit =
    assertEquals((selection \ "version").as[Int], 2)
    rows(selection).foreach { row =>
      assert((row \ "state").toOption.isEmpty)
      assert((row \ "detail" \ "explanation_gap").toOption.isEmpty)
    }
    assert(scenes(selection).forall(row => (row \ "state").toOption.isEmpty))
  private def lineFor(ctx: JudgmentAssemblyContext, row: JsObject): LegalLineNode =
    val id = (row \ "line_ids").as[List[String]].head
    ctx.legalLines.find(_.ref.id == id).get
  private def moves(ctx: JudgmentAssemblyContext, row: JsObject): List[String] =
    ctx.lineReplays(lineFor(ctx, row).ref).replaySteps.map(_.moveUci)

  private def reviewed(played: List[String], best: List[String]): JudgmentAssemblyContext =
    // Scores only select the pre-existing comparison. They are not search results.
    val input = RawMoveReviewInput(initial, played.head,
      List(EngineLine(best, 80, None, 16), EngineLine(played, 0, None, 16)))
    MoveReviewAssembler.assemble(MoveReviewInputAdmission.admit(input).get)

  test("public selection does not construct unconsumed local wrappers or discard underlying providers"):
    val ctx = fromFen("3r3k/8/8/8/8/8/8/1N1R1N1K w - - 0 1", "d1d2")
    val source = packet(ctx)
    val projection = MoveWorkProjection.projectFromPacket(source)
    val support = MoveWorkProjection.relationsFromPacket(source).filter(row => (row \ "relation" \ "kind").as[String] == "support")
    assertEquals(support.size, 2)
    assertEquals(support.map(row => (row \ "relation" \ "actor" \ "square").as[String]).toSet, Set("b1", "f1"))
    assertEquals(support.map(row => (row \ "selection").as[String]).toSet, Set("selected", "purpose_dedup"))
    assertEquals(facts(projection.selection), Nil)
    val selected = support.find(row => (row \ "selection").as[String] == "selected").get
    val highlight = Seq("relation", "candidate_index", "selected_index", "selection").foldLeft(selected)(_ - _)
    assert(projection.highlights.contains(highlight))
    assertEquals(scenes(projection.selection), Nil) // An ordinary support fact is not automatically a key scene.

  test("an attack lost to display purpose cannot erase the remaining opened-ray candidate"):
    val ctx = fromFen("7k/8/2p5/3p4/8/3N4/8/3R3K w - - 0 1", "d3b4")
    val source = packet(ctx)
    val projection = MoveWorkProjection.projectFromPacket(source)
    val rookAttack = MoveWorkProjection.relationsFromPacket(source).find(row =>
      (row \ "relation" \ "kind").as[String] == "attack" &&
      (row \ "relation" \ "actor" \ "square").as[String] == "d1").get
    assertEquals((rookAttack \ "selection").as[String], "purpose_dedup")
    assert(projection.highlights.exists(row => (row \ "text").as[String].startsWith("Clears d3,")))

  test("the shared capture candidate preserves unknown versus proven recapture history"):
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
      .extend(List("e2e4", "d7d5", "e4d5")).toOption.get
    val known = packet(assemble(history, "d8d5"))
    val unknown = packet(fromFen(history.currentFen, "d8d5"))
    def capture(source: EvidenceBackedJudgmentPacket): List[String] =
      MoveWorkProjection.fromPacket(source).map(row => (row \ "text").as[String])
    assert(capture(known).contains("Recaptures the pawn on d5."))
    assert(capture(unknown).contains("Captures the pawn on d5."))

  test("a completed material contrast precedes optional facts and retains both complete conditional ledgers"):
    val ctx = reviewed(List("e2e4", "d7d5", "e4d5", "d8d5"), List("d2d4", "d7d5"))
    val source = packet(ctx)
    val prepared = ObservedLineProtocol.prepare(source)
    val selection = MoveWorkProjection.projectFromPacket(source, prepared).selection
    assertSelectionV2(selection)
    val comparison = comparisons(selection).head
    assertEquals(rows(selection).head, comparison)
    assertEquals((comparison \ "detail" \ "type").as[String], "root_comparison")
    assertEquals((comparison \ "detail" \ "scope").as[String], "observed_lines")
    assertEquals((comparison \ "detail" \ "material_relation").as[String], "different")
    assertEquals((comparison \ "detail" \ "material_unavailable_reason").get, JsNull)
    // Same net balance need not have the same captured-role counts.
    assertEquals((comparison \ "detail" \ "reference_net_cp").as[Int], 0)
    assertEquals((comparison \ "detail" \ "candidate_net_cp").as[Int], 0)
    val context = rows(selection).filter(row => (row \ "kind").as[String] == "conditional_line")
    assert(context.forall(row => (row \ "selection").as[String] == "context"))
    val played = context.find(row => moves(ctx, row) ==
      List("e2e4", "d7d5", "e4d5", "d8d5")).get
    assertEquals((played \ "detail").as[JsObject], Json.obj("type" -> "observed_line", "scope" -> "observed_line"))
    val summary = ObservedLineProtocol.summaryJson(ctx, lineFor(ctx, played)).get
    assertEquals((summary \ "material" \ "events").as[List[JsObject]].map(event =>
      (event \ "move_uci").as[String]), List("e4d5", "d8d5"))
    val path = prepared.paths.find(_.line.ref == lineFor(ctx, played).ref).get
    val peer = prepared.paths.find(_.moves == List("d2d4", "d7d5")).get
    val ledger = path.certified.flatMap(_._2.material).get
    assertEquals(ledger.captures.map(capture =>
      (capture.plyOffset, capture.side.white, capture.capturedRole.name, capture.recapture)),
      List((2, true, "pawn", false), (3, false, "pawn", true)))
    assertEquals(peer.certified.flatMap(_._2.material).get.events, Nil,
      "the equal net balance in the other arm has no pawn exchange")
    val captures = prepared.continuationReadings.find(_.path eq path).get.events
      .filter(_.originalVictim.nonEmpty)
    def piece(side: String, role: String, square: String) = CommentaryFact.Piece(side, role, square).witness
    assertEquals(captures.map(event => (event.plyOffset, event.originalActor, event.originalVictim)), List(
      (2, Some(piece("white", "pawn", "e2")), Some(piece("black", "pawn", "d7"))),
      (3, Some(piece("black", "queen", "d8")), Some(piece("white", "pawn", "e2")))))
    // The whole ledger belongs in detail even when the basic answer owns a
    // narrower question. Check its actual losses, not an old total's wording.
    val detailUnits = (comparison \ "detail" \ "summary_question" \ "detail_units").as[List[JsObject]]
    val expected = List(2, 3).map(offset => RootQuestionSummary.MoveRef(path, offset).json).toSet
    assertEquals(detailUnits.flatMap(unit => (unit \ "covered_moves").as[List[JsObject]]).toSet, expected)
    for language <- List("en", "ko") do
      val shown = detailUnits.flatMap(unit => (unit \ language).as[List[JsValue]].collect {
        case reference: JsObject => reference
      }).toSet
      assert(expected.subsetOf(shown), "each loss keeps its actual occurrence in both delivered languages")
    List(2 -> "black pawn", 3 -> "white pawn").foreach { (offset, victim) =>
      val reference = RootQuestionSummary.MoveRef(path, offset).json
      val clauses = detailUnits.flatMap(unit => (unit \ "en").as[List[JsValue]].sliding(2).collect {
        case List(value: JsObject, JsString(words)) if value == reference => words.toLowerCase(java.util.Locale.ROOT)
      })
      assert(clauses.exists(_.contains(victim)), s"the actual $victim loss must be expressed beside its own move")
    }
    val evidence = (comparison \ "evidence_ids").as[List[String]].toSet
    assert((path.ids ++ peer.ids ++ List(path, peer).flatMap(_.certified.map(_._1.ref.id)))
      .forall(evidence), "both owned continuations and their certified ledgers retain their original evidence")
    assertEquals((played \ "occurrence_ids").as[List[String]], path.ids)
    assertEquals((played \ "occurrence_ids").as[List[String]].size, 4)
    assertEquals(scenes(selection).size, 1)
    val scene = scenes(selection).head
    assertEquals((scene \ "root_occurrence_id").as[String], ctx.playedTransition.get.evidence.id)
    assertEquals((scene \ "candidate_ids").as[List[String]].head, (comparison \ "id").as[String])
    assertEquals((scene \ "line_ids").as[List[String]].toSet, (comparison \ "line_ids").as[List[String]].toSet)
    val ids = rows(selection).map(row => (row \ "id").as[String]).toSet
    assert((scene \ "candidate_ids").as[List[String]].forall(ids))

  test("equal material counts preserve the evaluated comparison and separate local facts"):
    val source = packet(reviewed(List("e2e4", "e7e5"), List("d2d4", "d7d5")))
    assert(source.primary.nonEmpty)
    val projection = MoveWorkProjection.projectFromPacket(source)
    assertSelectionV2(projection.selection)
    val comparison = comparisons(projection.selection).head
    assertEquals((comparison \ "detail" \ "type").as[String], "root_comparison")
    assertEquals((comparison \ "detail" \ "material_relation").as[String], "equal")
    assertEquals((comparison \ "detail" \ "material_unavailable_reason").get, JsNull)
    assert((comparison \ "detail" \ "text").as[String].contains("same counts of captured and promoted piece roles"))
    assert((scenes(projection.selection).head \ "candidate_ids").as[List[String]]
      .contains((comparison \ "id").as[String]))
    assert(projection.highlights.nonEmpty)
    assertEquals(facts(projection.selection), Nil, "local facts are delivered through highlights without duplicate wrappers")

  test("an invalid supplied continuation retains its accepted prefix and stop without creating a scene"):
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
    val source = SuppliedCommentaryLine(CommentaryLineSource.Study, List("e2e4", "e7e5", "e4e6"))
    val ctx = MoveReviewAssembler.assembleOccurrences(
      MoveReviewInputAdmission.admitIndependent(history, "e2e4", suppliedLines = List(source)).get)
    val evidence = packet(ctx)
    val selection = MoveWorkProjection.selectionFromPacket(evidence)
    assertEquals(rows(selection), Nil, "no scene consumes a second wrapper of this supplied path")
    assert(ObservedLineProtocol.prepare(evidence).paths.exists(_.moves == List("e2e4", "e7e5")))
    val admitted = ctx.input.suppliedLines.head
    assertEquals(admitted.input.source, CommentaryLineSource.Study)
    assertEquals(admitted.acceptedPlyCount, 2)
    assertEquals(admitted.stop, Some(CommentaryLineStop.InvalidContinuation))
    assertEquals(scenes(selection), Nil)

  test("missing comparison path metadata keeps material unavailable and preserves local facts"):
    val ctx = reviewed(List("e2e4", "d7d5", "e4d5", "d8d5"), List("d2d4", "d7d5"))
    val reference = ctx.line(LineNodeRole.BestReference).get
    // The assessment remains certified; only this consumer's path linkage is absent.
    val changed = ctx.copy(lineTransitionIds = ctx.lineTransitionIds - reference.ref)
    val source = packet(changed)
    assert(source.primary.nonEmpty)
    val projection = MoveWorkProjection.projectFromPacket(source)
    assertSelectionV2(projection.selection)
    val comparison = comparisons(projection.selection).head
    assertEquals((comparison \ "detail" \ "type").as[String], "root_comparison")
    assertEquals((comparison \ "detail" \ "material_relation").as[String], "unavailable")
    assertEquals((comparison \ "detail" \ "material_unavailable_reason").as[String], "missing_path")
    assert((comparison \ "detail" \ "reference_net_cp").toOption.isEmpty)
    assertEquals((comparison \ "detail" \ "reference_line_id").get, JsNull)
    assertEquals((comparison \ "detail" \ "missing_line_ids").as[List[String]], List(reference.ref.id))
    assert(!(comparison \ "line_ids").as[List[String]].contains(reference.ref.id))
    assert(projection.highlights.nonEmpty)
    val available = rows(projection.selection).map(row => (row \ "id").as[String]).toSet
    assert((scenes(projection.selection).head \ "candidate_ids").as[List[String]].forall(available))

  test("same-FEN endpoints keep distinct exact paths and local occurrence anchors"):
    val prefix = "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4 g8f6 b1c3 a7a6 f1c4 e7e6 c4b3 b7b5 e1g1 f8e7 d1f3 d8c7 f3g3 e8g8 c1h6 f6e8 a1d1 c8d7 d4f3".split(" ").toList
    val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(prefix).toOption.get
    val shared = List("b8c6", "h6f4", "b5b4", "e4e5")
    val pawnCapture = shared ++ List("c6e5", "f3e5", "d6e5", "f4e5", "c7b7")
    val knightCapture = shared ++ List("d6e5", "f3e5", "c6e5", "f4e5", "c7b7")
    val ctx = assemble(history, "b8c6", List(pawnCapture, knightCapture))
    val source = packet(ctx)
    val selection = MoveWorkProjection.selectionFromPacket(source)
    val paths = ObservedLineProtocol.prepare(source).paths.filter(_.moves.size == 9)
    assertEquals(paths.size, 2)
    assertEquals(paths.map(_.positions.last.id).distinct.size, 2)
    assertEquals(rows(selection), Nil)

  test("prepared paths preserve summaries and selection but cannot cross packet owners"):
    val ctx = reviewed(List("e2e4", "d7d5", "e4d5", "d8d5"), List("d2d4", "d7d5"))
    val reference = ctx.line(LineNodeRole.BestReference).get
    List(ctx, ctx.copy(lineTransitionIds = ctx.lineTransitionIds - reference.ref)).foreach { context =>
      val source = packet(context)
      val prepared = ObservedLineProtocol.prepare(source)
      prepared.paths.foreach { path =>
        assertEquals(ObservedLineProtocol.summaryJson(path), ObservedLineProtocol.summaryJson(context, path.line))
      }
      assertEquals(MoveWorkProjection.projectFromPacket(source, prepared), MoveWorkProjection.projectFromPacket(source))
      intercept[IllegalArgumentException] {
        MoveWorkProjection.projectFromPacket(packet(context), prepared)
      }
    }
