package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import play.api.libs.json.*
import lila.chessjudgment.model.judgment.RelationFactKind

/** Genuine completed native receipts exercise a bounded local value answer.
  * They do not establish the cause of the original root grade or best play.
  */
class FunctionalChoiceDeliveryTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef

  // Replay the finite receipt sequence once; no engine is started. The public
  // observation graph is large, so its preparation has its own explicit limit.
  override def munitTimeout = 5.minutes

  private def file(name: String): Path =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/" + name
    List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get

  private lazy val record =
    val path = file("functional-choice-native26-produced.json.gz")
    val bytes = Files.readAllBytes(path)
    val sha = MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
    assertEquals(sha, "5d0595d782c0f47499e17d8d3193b8e0598d34f94c122fb0b77d2a55ad6c8b2d")
    val stream = new GZIPInputStream(Files.newInputStream(path))
    try Json.parse(stream.readAllBytes()).as[JsObject] finally stream.close()

  private lazy val expected = Json.parse(Files.readAllBytes(
    file("functional-choice-native26-expectations.json"))).as[JsObject]

  private case class Fixture(state: CommentaryJobState, request: PositionCommentaryJobProtocol.PositionCommentaryJobRequest,
      packet: lila.chessjudgment.model.judgment.EvidenceBackedJudgmentPacket,
      paths: PreparedPaths, task: FunctionalChoiceReading.Task, pair: List[PreparedPath]):
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val notes = MoveWorkProjection.select(packet, paths).local
    def protect(answer: Answer, original: List[Section]) =
      MoveWorkProjection.protectRootUses(packet, paths, pair, positions, notes, answer, original)
    lazy val previous =
      val root = Continuation(pair.map(MoveRef(_, 0)))
      val initial = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison",
        List(Section(List(root)))).get.copy(completionSources = Some(List(root)))
      val proofs = FunctionalReplyReading.read(state.proposalEntries).filter(proof =>
        proof.owner.proposalId == task.resource.owner.proposalId &&
          proof.owner.comparison == task.resource.owner.comparison && proof.condition == task.resource.condition)
      val value = FunctionalReplyReading.withAnswer(packet, paths, pair, proofs, initial, protect)
      assert(value.core.flatMap(_.statements).exists(_.isInstanceOf[LegalReplyOption]))
      value
    def deliver(showOther: Boolean, before: Answer = previous,
        tasks: List[FunctionalChoiceReading.Task] = List(task)) =
      FunctionalChoiceReading.withAnswer(packet, paths, pair, tasks, before, protect, showOther)
    def summary(answer: Answer) = RootQuestionSummary.realize(paths, pair, answer).get
    def ref(value: JsObject): MoveRef =
      val path = paths.path(packet, (value \ "line_id").as[String]).get
      val offset = path.ids.indexOf((value \ "occurrence_id").as[String])
      assert(offset >= 0)
      MoveRef(path, offset)

  private lazy val actual: Fixture =
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get
    assertEquals(request.remainingBudgetMs, Some(180000))
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 600001L, request.engineProfile, investigateChoices = true,
        previewDepth = Some(8)), 1L, request.inputLines, request.proposals).toOption.get
    val serializer = PositionCommentaryJobProtocol.getClass.getDeclaredMethods.find(
      _.getName == "issuedEngineWorkJson").get
    serializer.setAccessible(true)
    val rows = (record \ "saved_reports").as[List[JsObject]]
    assertEquals(rows.size, 26)
    rows.zipWithIndex.foreach { (saved, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"no actual issued work before receipt $index")
      val work = serializer.invoke(PositionCommentaryJobProtocol,
        state.asInstanceOf[AwaitingEngineWork].issuedWork).asInstanceOf[JsObject]
      assertEquals(work, (saved \ "issued_work").as[JsObject],
        s"whole issued descriptor must match before genuine receipt $index is admitted")
      assertEquals((saved \ "report" \ "outcome" \ "kind").as[String], "completed")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    assert(state.isInstanceOf[CompletedCommentaryJob])
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val roots = Set("d2c4", "d2b1")
    val task = FunctionalChoiceReading.task(packet, paths, state.proposalEntries,
      state.responseSearches, roots, request.engineProfile.wireId, 16).get
    assert(task.complete)
    assertEquals(task.branches.map(value => value.rootMove -> value.firstReply).toSet, task.expectedReplies)
    assertEquals(task.expectedReplies.size, 6)
    val owner = task.resource.owner.answer.get
    val pair = List(owner.referenceLineId, owner.alternativeLineId).map(paths.path(packet, _).get)
    Fixture(state, request, packet, paths, task, pair)

  private def coreDecisions(answer: Answer) = answer.core.flatMap(_.statements).collect {
    case value: ResourceDecision => value.value
  }
  private def detailDecisions(answer: Answer) = answer.details.flatMap(_.statements).collect {
    case value: ResourceDecision => value.value
  }
  private def foreign(sections: List[Section], roots: List[PreparedPath]) = sections.flatMap(_.references)
    .map(_.path).distinct.filterNot(path => roots.exists(_ eq path))
  private def wireRefs(value: JsValue): Set[JsObject] = value match
    case obj: JsObject if obj.keys.contains("line_id") && obj.keys.contains("occurrence_id") => Set(obj)
    case obj: JsObject => obj.values.flatMap(wireRefs).toSet
    case array: JsArray => array.value.flatMap(wireRefs).toSet
    case _ => Set.empty
  private def covered(summary: JsObject, field: String) = (summary \ field).as[List[JsObject]]
    .flatMap(unit => (unit \ "covered_moves").as[List[JsObject]]).toSet

  test("actual native producer delivers one matched question, its current refusal and conditional recapture price"):
    val f = actual
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("f" * 32, f.request.requestId, 600001L, f.state))
    val primary = (wire \ "result" \ "selected_move_reviews").as[List[JsObject]].find(value =>
      (value \ "selection" \ "roles").as[List[String]].contains("played")).get
      .apply("commentary").as[JsObject].apply("primary").as[JsObject]
    assertEquals(primary, (expected \ "baseline_primary").as[JsObject],
      "new child values and their display must not rewrite any original root assessment field")
    val owner = f.task.resource.owner.answer.get
    val candidates = (wire \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
    val summary = candidates.filter(candidate => (candidate \ "selection").as[String] == "selected" &&
      (candidate \ "detail" \ "reference_line_id").asOpt[String].contains(owner.referenceLineId) &&
      (candidate \ "detail" \ "candidate_line_id").asOpt[String].contains(owner.alternativeLineId))
      .flatMap(candidate => (candidate \ "detail" \ "summary_question").asOpt[JsObject])
      .find(value => (value \ "question").as[String] == "functional_response").get
    val units = (summary \ "units").as[List[JsObject]]
    val details = (summary \ "detail_units").as[List[JsObject]]
    assert(units.size <= 8 && details.size <= 8)
    val rootIds = f.pair.map(_.line.ref.id).toSet
    List(units, details).foreach(layer => assert(wireRefs(Json.toJson(layer)).map(ref =>
      (ref \ "line_id").as[String]).diff(rootIds).size <= 5))
    val all = covered(summary, "units") ++ covered(summary, "detail_units")
    val old = (expected \ "original_core_covered").as[List[JsObject]] ++
      (expected \ "original_detail_covered").as[List[JsObject]]
    assert(old.forall(all), "the narrower local question must preserve the existing owned prices")
    val rootPrices = (expected \ "original_core_covered").as[List[JsObject]]
      .filter(ref => rootIds((ref \ "line_id").as[String]))
    assert(rootPrices.forall(covered(summary, "units")), "prior selected root prices stay in core")
    val moves = wireRefs(Json.toJson(units)).map(f.ref)
    assert(moves.exists(ref => ref.path.moves.take(4) == List("d2c4", "e8f6", "d1d6", "c6c7") && ref.offset == 3))
    val capture = moves.find(ref => ref.path.moves.take(5) ==
      List("d2c4", "e8f6", "d1d6", "c6d6", "c4d6") && ref.offset == 4).get
    assert(covered(summary, "units")(MoveRef(capture.path, 3).json))
    assert(covered(summary, "units")(capture.json))
    val originalPath = f.pair.find(_.moves.head == "d2c4").get
    val originalLoss = MoveRef(originalPath, 21)
    val oldCost = units.find(unit => (unit \ "covered_moves").as[List[JsObject]].contains(originalLoss.json)).get
    val context = List("d2c4", "e8f6", "d1d6", "c6c7", "a1d1", "b6b5")
    assertEquals(originalPath.moves.take(context.size), context)
    assert(oldCost("en").as[List[JsValue]].exists(_.asOpt[String].exists(_.contains("Separately, in the recorded"))))
    assert(wireRefs(oldCost("proof_moves")).contains(MoveRef(originalPath, 5).json),
      "the retained old forecast names its first real divergence from the current Qc7 path")
    assert(!wireRefs(oldCost("action_moves")).contains(MoveRef(originalPath, 5).json),
      "a position-only label must not select the old prefix as a new price lifetime")
    assert(covered(summary, "units")(MoveRef(originalPath, 22).json),
      "the separately labelled original Bxb7 recovery price remains in core")
    assert(units.flatMap(unit => unit("en").as[List[JsValue]]).exists(_.asOpt[String].exists(_.contains("moves the queen out of the attack"))))
    assert(units.flatMap(unit => unit("en").as[List[JsValue]]).exists(_.asOpt[String].exists(_.contains("keeps targeting White's rook on d6"))))

  test("both display choices preserve exact current value owners and close only the represented same-victim transaction"):
    val f = actual
    val originalRoot = f.task.resource.owner.answer
    List(false, true).foreach { showOther =>
      val answer = f.deliver(showOther)
      assertEquals(answer.question, "functional_response")
      val center = coreDecisions(answer)
      assertEquals(center.size, 2)
      assert(center.forall(value => value.branch.matchedReply && value.branch.firstReply == "e8f6"))
      assertEquals(center.map(_.branch.rootMove).toSet, Set("d2c4", "d2b1"))
      center.foreach { value =>
        assert(value.use eq value.branch.use.get)
        assert(value.resistance eq value.branch.response.get)
        assert(value.use.belongsTo(f.packet, f.paths) && value.resistance.belongsTo(f.packet, f.paths))
      }
      val useful = center.find(_.use.useful).get
      assertEquals(useful.other.nonEmpty, showOther)
      val reply = MoveRef(useful.path, 3)
      val originalUser = f.task.resource.actor.witness
      val user = f.paths.trackedPieceAfter(useful.path, originalUser, 3).flatten.get
      val queen = f.paths.identitiesAfter(f.packet, useful.path, 3).collectFirst {
        case (original, Some(Some(current))) if current.square.key == useful.path.moves(3).take(2) => original
      }.get
      val movedQueen = f.paths.trackedPieceAfter(useful.path, queen, 4).flatten.get
      assert(useful.replyRoles.contains(Action(reply, MoveWorkProjection.Detail.Evades(movedQueen, user))))
      assert(useful.replyRoles.contains(Action(reply, MoveWorkProjection.Detail.Attack(movedQueen, user, false))),
        "Qc7 keeps attacking the same original rook; it does not create that attack")
      assert(!useful.replyRoles.exists {
        case Action(_, MoveWorkProjection.Detail.Attack(_, _, true)) => true
        case _ => false
      })
      assert(useful.replyRoles.forall(_.references == List(reply)))
      assert(useful.replyEvidence.contains(useful.path.ids(3)))
      assert(useful.replyEvidence.contains(f.positions(useful.path.positions(3)).view.occurrence.id))
      assert(useful.replyEvidence.contains(f.positions(useful.path.positions(4)).view.occurrence.id))
      val missing = FunctionalChoiceReading.selectedReplyRoles(f.packet, f.paths, useful.path,
        f.task.resource.actor, f.positions - useful.path.positions(3))
      assertEquals(missing.statements, Nil)
      assertEquals(missing.evidence, Nil)
      val before = f.positions(useful.path.positions(3))
      val incomplete = new MoveWorkProjection.Position(before.view):
        override def complete(kind: RelationFactKind): Boolean =
          kind != RelationFactKind.GeometricControl && super.complete(kind)
      val unknown = FunctionalChoiceReading.selectedReplyRoles(f.packet, f.paths, useful.path,
        f.task.resource.actor, f.positions.updated(useful.path.positions(3), incomplete))
      assertEquals(unknown.statements, Nil, "available positive rays cannot replace a complete role inventory")
      assertEquals(unknown.evidence, Nil)
      val foreignPosition = FunctionalChoiceReading.selectedReplyRoles(f.packet, f.paths, useful.path,
        f.task.resource.actor, f.positions.updated(useful.path.positions(3), f.positions(useful.path.positions(4))))
      assertEquals(foreignPosition.statements, Nil, "a complete inventory from another position is not this reply's source")
      assertEquals(foreignPosition.evidence, Nil)
      val borrowed = FunctionalChoiceReading.selectedReplyRoles(f.packet, f.paths, useful.path,
        f.task.resource.actor.copy(square = "a1"), f.positions)
      assertEquals(borrowed.statements, Nil, "another friendly rook cannot own this response role")
      assertEquals(borrowed.evidence, Nil)
      val refuted = center.find(_.use.refuted).get
      assertEquals(refuted.other.map(ref => ref.path.moves(ref.offset)), Some("b1c3"))
      val third = detailDecisions(answer)
      assertEquals(third.size, 1)
      assertEquals(third.head.branch.rootMove -> third.head.branch.firstReply, "d2b1" -> "e8c7")
      assert(third.head.branch.firstPool.lines.head.evaluation.moves.head == third.head.branch.firstReply)
      val proof = answer.core.flatMap(_.statements).collect { case value: LegalReplyOption => value }.head
      val original = f.previous.core.flatMap(_.statements).collect { case value: LegalReplyOption => value }.head
      assertEquals(proof.proof, original.proof)
      assert(FunctionalReplyReading.valid(f.paths, proof) && FunctionalReplyReading.relevant(f.paths, f.pair, proof))
      assertEquals(proof.actionReferences, Nil)
      val owned = center.flatMap(_.resistance.comparison.pathsByMove.values).toSet
      assert(List(proof.reference, proof.alternative, proof.available).forall(owned))
      val required = List(MoveRef(proof.reference, 3), MoveRef(proof.available, 4), MoveRef(proof.alternative, 3))
      assert(required.forall(answer.core.flatMap(_.covered).toSet))
      val capturedQueen = proof.proof.action.target.get.witness
      List(proof.reference, proof.alternative).foreach { path =>
        assert(f.paths.continuationReadingFor(path).get.events.exists(event => event.plyOffset == 3 &&
          event.originalActor.contains(capturedQueen) && event.originalVictim.contains(f.task.resource.actor.witness)))
      }
      assert(f.paths.continuationReadingFor(proof.available).get.events.exists(event => event.plyOffset == 4 &&
        event.originalActor.contains(proof.proof.action.subject.get.witness) && event.originalVictim.contains(capturedQueen)))
      val sources = answer.completionSources.get
      assert(f.previous.completionSources.toList.flatten.forall(sources.contains))
      assert(!sources.flatMap(_.covered).exists(required.contains),
        "computed current capture prices cannot become fresh lifetime-account sources")
      assert(!sources.exists(_.isInstanceOf[RecordedContext]),
        "the separate recorded continuation label is never a new completion source")
      assert(answer.details.flatMap(_.covered).contains(MoveRef(third.head.path, 3)),
        "the other actual leader must be price-completed before being put in detail")
      assert(foreign(answer.core, f.pair).size <= 5 && foreign(answer.details, f.pair).size <= 5)
      val rendered = f.summary(answer)
      assert((rendered.json \ "units").as[List[JsObject]].size <= 8)
      assert((rendered.json \ "detail_units").as[List[JsObject]].size <= 8)
      assert(rendered.text.contains("Qc7") && rendered.text.contains("Nxd6") && rendered.text.contains("Nc3"), rendered.text)
      assertEquals(f.task.resource.owner.answer, originalRoot)
    }

  test("missing mechanical proof falls back to the wider completed answer; unfinished values cannot upgrade it"):
    val f = actual
    val previous = f.previous
    def withoutOption(section: Section): Section =
      val statements = section.statements.filterNot(_.isInstanceOf[LegalReplyOption])
      val refs = statements.flatMap(_.references).toSet
      section.copy(statements = statements, required = section.required.filter(refs), focus = section.focus.filter(f => refs(f.move)))
    val before = previous.copy(core = previous.core.map(withoutOption).filter(_.statements.nonEmpty),
      details = previous.details.map(withoutOption).filter(_.statements.nonEmpty),
      completionSources = previous.completionSources.map(_.filterNot(_.isInstanceOf[LegalReplyOption])))
    val fallback = f.deliver(showOther = true, before = before)
    assertEquals(coreDecisions(fallback).size, 3,
      "failure to rebase a mechanical example must not discard the already completed wider local values")
    assert(f.summary(fallback).text.contains("Nc3"))
    val missing = f.task.copy(branches = f.task.branches.map(branch =>
      if branch.rootMove == "d2b1" && branch.firstReply == "e8c7" then branch.copy(response = None) else branch))
    assert(!missing.complete)
    assert(f.deliver(showOther = true, tasks = List(missing)) eq previous)
    val otherRoot = f.task.branches.find(_.rootMove == "d2b1").get.use.get.reference
    assertNotEquals(otherRoot.line.ref.id, f.pair(1).line.ref.id)
    assert(FunctionalChoiceReading.withAnswer(f.packet, f.paths, List(f.pair.head, otherRoot),
      List(f.task), previous, f.protect) eq previous,
      "a same-root continuation cannot replace the exact original numerical root owner")
