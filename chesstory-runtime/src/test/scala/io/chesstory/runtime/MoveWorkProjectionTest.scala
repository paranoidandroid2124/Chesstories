package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

class MoveWorkProjectionTest extends munit.FunSuite:
  import MoveWorkMigrationFixtures.*

  private lazy val kingEntry = example("ai-game-26-ply71")
  private lazy val queenSupport = example("ai-game-27-ply31")
  private lazy val rookFile = example("heldout-najdorf-game-33-ply75")
  private lazy val development = example("ai-game-26-ply9")
  private lazy val pawnBlock = resource("empty-d5-loss-and-maintenance")
  private lazy val openedRay = resource("ray-first-occupant-leaves")
  private lazy val advancingPawn = resource("pawn-pinned-but-geometric-routes-open")
  private lazy val transposed = assemble(SuppliedCommentaryExamples.history, "b8c6",
    List(SuppliedCommentaryExamples.pawnCapture, SuppliedCommentaryExamples.knightCapture))

  private val kingText = "White pawn on g3 now controls f4 next to the opposing king; the king cannot enter this square."
  private val supportText = "White queen on e2 adds support to White pawn on e4, under attack from Black bishop on b7, Black knight on c5, Black knight on f6."
  private val attackText = "White queen on e2 now targets Black pawn on b5."

  private def packet(ctx: JudgmentAssemblyContext): EvidenceBackedJudgmentPacket =
    val result = EvidenceBackedJudgmentPacket.fromAssembly(ctx).get
    assertEquals(result.primary, None)
    result

  private def text(note: JsObject): String = (note \ "text").as[String]
  private def occurrence(note: JsObject): String = (note \ "occurrence_id").as[String]
  private def squares(note: JsObject): List[String] = (note \ "squares").as[List[String]]
  private def frames(note: JsObject): List[JsObject] = (note \ "frames").as[List[JsObject]]

  private def notes(ctx: JudgmentAssemblyContext): List[JsObject] =
    MoveWorkProjection.fromPacket(packet(ctx))

  private def played(ctx: JudgmentAssemblyContext): List[JsObject] =
    notes(ctx).filter(occurrence(_) == ctx.playedTransition.get.evidence.id)

  private def texts(ctx: JudgmentAssemblyContext): List[String] = played(ctx).map(text)

  private def expect(ctx: JudgmentAssemblyContext, expected: String, expectedSquares: List[String]): JsObject =
    val selected = played(ctx).filter(text(_) == expected)
    assertEquals(selected.size, 1, expected)
    assertEquals(squares(selected.head).toSet, expectedSquares.toSet)
    selected.head

  // Resolve semantic source IDs inside each frame's position, never globally or by FEN.
  private def assertReferences(source: EvidenceBackedJudgmentPacket, result: List[JsObject]): Unit =
    val graph = source.evidenceGraph
    val positions = source.resources.map(view => view.position.id.get -> view).toMap
    val transitions = source.assembly.transitions.map(edge => edge.evidence.id -> edge).toMap
    assertEquals(result.distinct, result)
    result.foreach { note =>
      assert(text(note).nonEmpty && !text(note).contains("<"))
      assertEquals(squares(note), squares(note).distinct)
      assert(squares(note).nonEmpty && squares(note).forall(_.matches("[a-h][1-8]")))
      val edge = transitions(occurrence(note))
      assert(graph.record(edge.evidence).exists(r => edge.matches(r) && graph.proofEligible(r)))
      val premises = (note \ "premise_ids").as[List[String]]
      assertEquals(premises.distinct, premises)
      assert(!premises.contains(edge.evidence.id)) // occurrence_id resolves this owner implicitly.
      premises.foreach(id => assert(graph.byId.get(id).exists(graph.proofEligible), s"unresolved premise: $id"))
      assertEquals(frames(note).map(frame => (frame \ "phase").as[String]), List("before", "after"))
      frames(note).zip(List(edge.from, edge.to)).foreach { case (frame, position) =>
        assertEquals((frame \ "position_id").as[String], position.id.get)
        val view = positions((frame \ "position_id").as[String])
        assertEquals(view.position, position)
        assert(!premises.contains(view.occurrence.id))
        assertEquals(view.occurrence.position, position)
        assert(graph.record(view.occurrence).exists(graph.proofEligible))
        assertEquals(squares(frame), squares(note))
        val ids = (frame \ "source_ids").as[List[String]]
        assertEquals(ids, ids.distinct.sorted)
        ids.foreach { id =>
          val resolved = view.records.filter(_.payload match
            case fact: RelationFactEvidence => fact.semanticId == id
            case _ => false)
          assertEquals(resolved.size, 1, s"unresolved source in ${position.id.get}: $id")
          assertEquals(resolved.head.ref.position, position)
          assert(graph.record(resolved.head.ref).contains(resolved.head))
          assert(graph.proofEligible(resolved.head))
        }
      }
    }

  private def withoutRecords(ctx: JudgmentAssemblyContext)(remove: EvidenceRecord => Boolean): JudgmentAssemblyContext =
    val kept = ctx.evidenceGraph.records.filterNot(remove)
    assert(kept.size < ctx.evidenceGraph.records.size, "mutation must remove an actual premise")
    val changed = ctx.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(kept))
    val source = packet(changed)
    assertReferences(source, MoveWorkProjection.fromPacket(source))
    changed

  private def without(ctx: JudgmentAssemblyContext, position: PositionNodeRef)(
      remove: RelationWitnessDetail => Boolean
  ): JudgmentAssemblyContext = withoutRecords(ctx) { record =>
    record.ref.position == position && (record.payload match
      case fact: RelationFactEvidence => remove(fact.detail)
      case _ => false)
  }

  private def withoutStatements(ctx: JudgmentAssemblyContext): JudgmentAssemblyContext =
    // Withhold the observation replay input while retaining certified edges and resources.
    val changed = ctx.copy(transitionReplays = Map.empty)
    val source = packet(changed)
    assertEquals(source.observations, Nil)
    assertReferences(source, MoveWorkProjection.fromPacket(source))
    changed

  test("typed projection is deterministic, read-only and delivered with resolvable public resource IDs"):
    val source = packet(queenSupport)
    val records = source.evidenceGraph.records
    val observations = source.observations
    val result = MoveWorkProjection.fromPacket(source)
    assertEquals(result, MoveWorkProjection.fromPacket(source))
    assertEquals(source.evidenceGraph.records, records)
    assertEquals(source.observations, observations)
    assert(result.nonEmpty)
    assertEquals(result.map(text).filter(Set(supportText, attackText)).toSet, Set(supportText, attackText))
    val wire = RuntimeProtocol.observationsJson(source)
    assertEquals((wire \ "highlights").as[List[JsObject]], result)
    val publicPositions = (wire \ "positions").as[List[JsObject]].map(p => (p \ "id").as[String] -> p).toMap
    result.flatMap(frames).foreach { frame =>
      val position = publicPositions((frame \ "position_id").as[String])
      val published = (position \\ "source_id").map(_.as[String]).toSet ++
        (position \\ "source_ids").flatMap(_.as[List[String]]).toSet
      assert((frame \ "source_ids").as[List[String]].forall(published))
    }
    assert(result.flatMap(frames).exists(f => (f \ "source_ids").as[List[String]].nonEmpty))
    // Validate the shared transport once per distinct output family, not inside every chess assertion.
    List(queenSupport, kingEntry, rookFile, pawnBlock, openedRay, advancingPawn,
      development, transposed, certified("castle"), certified("pinned-recapture")).foreach { ctx =>
      val owned = packet(ctx)
      assertReferences(owned, MoveWorkProjection.fromPacket(owned))
    }

  test("research reads a typed attack excluded by the display cap without changing public notes"):
    // An already exposed regression, not a new evaluation example.
    val prefix = List("d2d4", "g8f6", "c2c4", "c7c5", "d4d5", "e7e6", "b1c3", "e6d5")
    val history = IndependentCommentaryExamples.history.extend(prefix).toOption.get
    val ctx = assemble(history, "c3d5", List(List("c3d5", "f6d5", "c4d5")))
    val source = packet(ctx)
    val edge = ctx.playedTransition.get
    val published = MoveWorkProjection.fromPacket(source)
    val root = published.filter(occurrence(_) == edge.evidence.id)
    assert(root.nonEmpty)
    val research = MoveWorkProjection.relationsFromPacket(source)
    val hidden = research.find(n => occurrence(n) == edge.evidence.id &&
      (n \ "relation" \ "kind").as[String] == "attack" &&
      (n \ "relation" \ "target" \ "square").as[String] == "f6").get
    assertEquals((hidden \ "relation").as[JsObject], Json.obj("kind" -> "attack",
      "actor" -> Json.obj("side" -> "white", "role" -> "knight", "square" -> "d5"),
      "target" -> Json.obj("side" -> "black", "role" -> "knight", "square" -> "f6"), "gained" -> true))
    assert((hidden \ "candidate_index").as[Int] >= root.size)
    assertEquals((hidden \ "selection").as[String], "display_cap")
    assertEquals((hidden \ "selected_index").get, JsNull)
    research.foreach { row =>
      val note = Seq("relation", "candidate_index", "selection", "selected_index").foldLeft(row)(_ - _)
      assertReferences(source, List(note))
      (row \ "selected_index").asOpt[Int].foreach { index =>
        assertEquals(note, published.filter(occurrence(_) == occurrence(row))(index))
      }
    }
    assertEquals(MoveWorkProjection.fromPacket(source), published)
    val missing = without(ctx, edge.to) {
      case RelationWitnessDetail.GeometricControl(chess.Color.White, from, _, to) => from.key == "d5" && to.key == "f6"
      case _ => false
    }
    assert(!MoveWorkProjection.relationsFromPacket(packet(missing)).exists(n =>
      occurrence(n) == edge.evidence.id && text(n) == text(hidden)))
    assertEquals(played(missing), root)

  test("research retains distinct supporting actors removed by the shared display purpose"):
    val ctx = fromFen("3r3k/8/8/8/8/8/8/1N1R1N1K w - - 0 1", "d1d2")
    val source = packet(ctx)
    val research = MoveWorkProjection.relationsFromPacket(source)
    val support = research.filter(n => (n \ "relation" \ "kind").as[String] == "support")
    assertEquals(support.map(n => (n \ "relation" \ "actor" \ "square").as[String]).toSet, Set("b1", "f1"))
    assertEquals(support.map(n => (n \ "selection").as[String]).toSet, Set("selected", "purpose_dedup"))
    val selected = support.find(n => (n \ "selection").as[String] == "selected").get
    val omitted = support.find(n => (n \ "selection").as[String] == "purpose_dedup").get
    assertEquals((omitted \ "selected_index").get, JsNull)
    support.foreach { row =>
      assertEquals((row \ "relation" \ "target" \ "square").as[String], "d2")
      assertEquals((row \ "relation" \ "opposition").as[List[JsObject]],
        List(Json.obj("side" -> "black", "role" -> "rook", "square" -> "d8")))
      assertReferences(source, List(Seq("relation", "candidate_index", "selection", "selected_index").foldLeft(row)(_ - _)))
    }
    val published = MoveWorkProjection.fromPacket(source)
    assertEquals(text(published.filter(occurrence(_) == occurrence(selected))((selected \ "selected_index").as[Int])),
      text(selected))
    assert(!published.exists(text(_) == text(omitted)))

  test("resource-only R1 notes survive absent observations using the typed legal catalogue"):
    expect(rookFile, "Uses the open c-file: White rook on c1 controls c6, also supported by White queen on c7.",
      List("c1", "c6", "c7"))
    for ctx <- List(kingEntry, rookFile) do
      val resourceNotes = played(withoutStatements(ctx))
      assertEquals(resourceNotes.map(text), texts(ctx))
      assert(resourceNotes.forall(n => (n \ "premise_ids").as[List[String]].isEmpty))
    expect(withoutStatements(pawnBlock),
      "Occupies e5, blocking the advance of Black pawn on e6 to e5.", List("e5", "e6"))

  test("missing prior controls suppress the negative king-entry premise but unrelated after controls do not"):
    expect(kingEntry, kingText, List("g3", "f5", "f4"))
    val edge = kingEntry.playedTransition.get
    val before = without(kingEntry, edge.from) {
      case RelationWitnessDetail.GeometricControl(_, from, _, to) => from.key == "g2" && to.key == "h3"
      case _ => false
    }
    assert(!texts(before).contains(kingText))
    val unrelatedAfter = without(kingEntry, edge.to) {
      case RelationWitnessDetail.GeometricControl(_, from, _, to) => from.key == "g3" && to.key == "h4"
      case _ => false
    }
    assertEquals(texts(unrelatedAfter), texts(kingEntry))
    val missingPositive = without(kingEntry, edge.to) {
      case RelationWitnessDetail.GeometricControl(_, from, _, to) => from.key == "g3" && to.key == "f4"
      case _ => false
    }
    assert(!texts(missingPositive).contains(kingText))

  test("missing support premise removes only the dependent note and preserves the independent attack"):
    val note = expect(queenSupport, supportText, List("e2", "e4", "b7", "c5", "f6"))
    expect(queenSupport, attackText, List("e2", "b5"))
    val controls = packet(queenSupport).resources.find(_.position == queenSupport.playedTransition.get.to).get
      .records.collect {
        case EvidenceRecord(_, fact: RelationFactEvidence, _) if (fact.detail match
          case RelationWitnessDetail.GeometricControl(side, from, _, to) =>
            to.key == "e4" && (side == chess.Black || from.key == "e2")
          case _ => false) => fact.semanticId
      }
    assertEquals(controls.size, 4)
    assert(controls.forall((frames(note).last \ "source_ids").as[List[String]].contains))
    val changed = without(queenSupport, queenSupport.playedTransition.get.to) {
      case RelationWitnessDetail.GeometricControl(_, from, _, to) => from.key == "e2" && to.key == "e4"
      case _ => false
    }
    val remaining = texts(changed)
    assert(!remaining.contains(supportText))
    assert(remaining.contains(attackText))
    texts(queenSupport).filterNot(_ == supportText).foreach(t => assert(remaining.contains(t), t))

  test("partial opposition controls retain independently supported pressure clauses"):
    val changed = without(queenSupport, queenSupport.playedTransition.get.to) {
      case RelationWitnessDetail.GeometricControl(_, from, _, to) => from.key == "b7" && to.key == "e4"
      case _ => false
    }
    expect(changed,
      "White queen on e2 adds support to White pawn on e4, under attack from Black knight on c5, Black knight on f6.",
      List("e2", "e4", "c5", "f6"))
    assert(texts(changed).contains(attackText))

  test("opened-ray occupancy survives missing controls at its occupied endpoint"):
    val changed = without(withoutStatements(openedRay), openedRay.playedTransition.get.to) {
      case RelationWitnessDetail.GeometricControl(_, from, _, _) => from.key == "a5"
      case _ => false
    }
    expect(changed, "Clears a3, opening a line for White rook on a1 toward a4.", List("a1", "a3", "a4"))
    assert(!texts(changed).exists(_.endsWith("toward a5.")))

  test("pawn-block and current-file notes survive absent mover controls"):
    def noControls(ctx: JudgmentAssemblyContext) = without(withoutStatements(ctx), ctx.playedTransition.get.to) {
      case _: RelationWitnessDetail.GeometricControl => true
      case _ => false
    }
    expect(noControls(pawnBlock), "Occupies e5, blocking the advance of Black pawn on e6 to e5.", List("e5", "e6"))
    expect(noControls(rookFile), "Places the rook on the open c-file.", List("c1"))
    val missingAdvance = without(pawnBlock, pawnBlock.playedTransition.get.from) {
      case RelationWitnessDetail.PawnAdvanceAffordance(_, at, destination, _) => at.key == "e6" && destination.key == "e5"
      case _ => false
    }
    assert(!texts(missingAdvance).exists(_.contains("blocking the advance")))
    val missingFile = without(noControls(rookFile), rookFile.playedTransition.get.to) {
      case RelationWitnessDetail.PawnFileGroup(chess.Color.White, file, _) => file.key == "c"
      case _ => false
    }
    assert(!texts(missingFile).exists(_.contains("open c-file")))

  test("a pinned geometric recapturer does not become a legal recapture or safety claim"):
    val ctx = certified("pinned-recapture")
    val inventory = (RuntimeProtocol.observationsJson(packet(ctx)) \ "statements").as[List[JsObject]]
      .find(row => (row \ "kind").as[String] == "recapture_resources").get
    assert((inventory \ "geometric_recapturers").as[List[JsObject]].exists(r => (r \ "square").as[String] == "e7"))
    assertEquals((inventory \ "legal_recaptures").as[List[JsObject]], Nil)
    expect(ctx, "White rook on e1 now targets Black knight on e7.", List("e1", "e7"))
    assert(!texts(ctx).exists(t => t.toLowerCase.contains("safe") || t.toLowerCase.contains("recaptur")))

  test("castling preserves the admitted king destination and the separate rook movement"):
    val normal = certified("castle")
    expect(normal, "Castles, bringing the rook from h1 to f1.", List("e1", "g1", "h1", "f1"))
    val statement = packet(normal).observations.find(_.meaning.isInstanceOf[MoveObservationMeaning.Castling]).get
    assertEquals(statement.mover.to.key, "g1")

  test("development uses canonical history and is not inferred from a home-square departure"):
    expect(development, "Develops the knight from b1 to c3.", List("b1", "c3"))
    val unknown = fromFen(development.input.positionHistory.currentFen, "b1c3")
    assert(!texts(unknown).exists(_.startsWith("Develops")))
    assert(texts(unknown).exists(_.contains("adds support to White pawn on e4")))
    val returned = IndependentCommentaryExamples.history.extend(List("g1f3", "g8f6", "f3g1", "f6g8")).toOption.get
    val redeveloped = assemble(returned, "g1f3")
    assert(!texts(redeveloped).exists(_.startsWith("Develops")))
    List(unknown, redeveloped).foreach { ctx =>
      val source = packet(ctx)
      assertReferences(source, MoveWorkProjection.fromPacket(source))
    }

  test("passed-pawn wording distinguishes maintained, created and unknown prior state"):
    expect(advancingPawn, "Advances the passed pawn to a3.", List("a2", "a3"))
    val unknownBefore = without(advancingPawn, advancingPawn.playedTransition.get.from) {
      case RelationWitnessDetail.PawnPassage(_, square, _) => square.key == "a2"
      case _ => false
    }
    expect(unknownBefore, "The pawn on a3 is a passed pawn.", List("a2", "a3"))
    val full = resource("pawn-capture-clears-stationary-rook")
    // Remove the two pressure premises to expose the passed-pawn facet within R1's cap.
    val noEvasion = without(full, full.playedTransition.get.from) {
      case RelationWitnessDetail.GeometricControl(_, from, _, to) => from.key == "e7" && to.key == "e4"
      case _ => false
    }
    val created = without(noEvasion, full.playedTransition.get.to) {
      case RelationWitnessDetail.GeometricControl(_, from, _, to) => from.key == "e1" && to.key == "e7"
      case _ => false
    }
    expect(created, "Creates a passed pawn on d5.", List("e4", "d5"))
    val missingAfter = without(created, full.playedTransition.get.to) {
      case RelationWitnessDetail.PawnPassage(_, square, _) => square.key == "d5"
      case _ => false
    }
    assert(!texts(missingAfter).exists(_.contains("passed pawn")))
    expect(missingAfter, "Captures the pawn on d5.", List("e4", "d5"))
    expect(missingAfter, "Clears e4, opening a line for White rook on e1 toward e6.", List("e1", "e4", "e6"))

  test("a recapture keeps the preceding transition premise; unknown prehistory retains plain capture"):
    val history = IndependentCommentaryExamples.recaptureHistory
    val known = assemble(history, "d8d5")
    val unknown = fromFen(history.currentFen, "d8d5")
    expect(known, "Recaptures the pawn on d5.", List("d8", "d5"))
    expect(unknown, "Captures the pawn on d5.", List("d8", "d5"))
    // An in-packet recapture must cite both admitted transitions.
    val line = assemble(IndependentCommentaryExamples.history, "e2e4", List(List("e2e4", "d7d5", "e4d5", "d8d5")))
    val edge = line.transitions.find(_.moveUci == "d8d5").get
    val preceding = line.transitions.find(_.to == edge.from).get
    val note = notes(line).find(n => occurrence(n) == edge.evidence.id && text(n) == "Recaptures the pawn on d5.").get
    assert((note \ "premise_ids").as[List[String]].contains(preceding.evidence.id))
    List(known, unknown, line).foreach { ctx =>
      val source = packet(ctx)
      assertReferences(source, MoveWorkProjection.fromPacket(source))
    }

  test("same-FEN transpositions retain exact historical frames and shared prefixes appear only once"):
    val paths = List(SuppliedCommentaryExamples.pawnCapture, SuppliedCommentaryExamples.knightCapture)
    val body = RuntimeProtocol.observationsJson(packet(transposed))
    val wirePaths = (body \ "paths").as[List[JsObject]]
    val selected = paths.map(moves => wirePaths.find(p => (p \ "moves").as[List[String]] == moves).get)
    val ids = selected.map(p => (p \ "occurrence_ids").as[List[String]])
    assertEquals(ids.head.take(4), ids.last.take(4))
    val endpoints = ids.map(path => transposed.transitions.find(_.evidence.id == path.last).get.to)
    assertEquals(endpoints.map(_.fen).distinct.size, 1)
    assertEquals(endpoints.map(_.id).distinct.size, 2)
    val endpointIds = endpoints.flatMap(_.id).toSet
    val positions = (body \ "positions").as[List[JsObject]].filter(p => endpointIds((p \ "id").as[String]))
    assertEquals(positions.map(p => (p \ "fen").as[String]).distinct.size, 1)
    assertEquals(positions.map(p => (p \ "premise_id").as[String]).distinct.size, 2)
    val result = notes(transposed)
    ids.zip(endpoints).foreach { case (path, endpoint) =>
      val ending = result.filter(occurrence(_) == path.last)
      assert(ending.nonEmpty)
      ending.foreach(n => assertEquals((frames(n).last \ "position_id").as[String], endpoint.id.get))
    }
    assert(result.exists(n => occurrence(n) == ids.head(7) && text(n) == "Recaptures the pawn on e5."))
    assert(result.exists(n => occurrence(n) == ids.last(7) && text(n) == "Recaptures the knight on e5."))
    val repeated = assemble(SuppliedCommentaryExamples.history, "b8c6", paths ++ paths)
    assertEquals(notes(repeated), result)

  test("a missing historical position premise erases only that branch's dependent occurrence"):
    val last = transposed.transitions.filter(_.moveUci == "c7b7").head
    val original = notes(transposed)
    assert(original.exists(occurrence(_) == last.evidence.id))
    val missing = packet(transposed).resources.find(_.position == last.to).get.occurrence
    val changed = withoutRecords(transposed) { record =>
      record.ref == missing
    }
    assertEquals(notes(changed), original.filterNot(occurrence(_) == last.evidence.id))
