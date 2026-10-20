package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import play.api.libs.json.*
import CommentaryEnglish.at

/** Only Re2 and the preregistered Qa4 screen evidence. No engine is run here. */
class PurposeComparisonTest extends munit.FunSuite:
  private def fixture(name: String): JsValue =
    val file = Path.of("../judgment-evaluation/fixtures/public-commentary-v6/" + name + ".json.gz")
    val stream = new GZIPInputStream(Files.newInputStream(file))
    try Json.parse(stream.readAllBytes()) finally stream.close()

  private lazy val screens = fixture("purpose-comparison-browser")
  private def replay(row: JsObject, finished: Boolean = true,
      onReport: CommentaryJobState => Unit = _ => (), budgetMs: Long = 10000L): CommentaryJobState =
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((row \ "request").get).getBytes(UTF_8)).toOption.get
    val saved = (row \ "saved_reports").as[List[JsObject]]
    val depth = (saved.head \ "issued_work" \ "search_limits" \ "depth").as[Int]
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(depth, budgetMs + 1L, request.engineProfile, investigateChoices = true,
        shortJob = true), 1L, request.inputLines, request.proposals).toOption.get
    saved.zipWithIndex.foreach { (item, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork])
      val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
      assertEquals(work.executionKeySha256, (item \ "issued_work" \ "execution_key_sha256").as[String])
      val raw = (item \ "report").as[JsObject] ++ Json.obj("work_id" -> work.workId)
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(Json.stringify(raw).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
      onReport(state)
    }
    if finished then assert(!state.isInstanceOf[AwaitingEngineWork])
    state

  private lazy val re2 = replay((fixture("core-idea-browser") \ "cases").as[List[JsObject]]
    .find(r => (r \ "id").as[String] == "re2").get)
  private lazy val qa4 = replay((screens \ "late_common_realization").as[JsObject])

  test("Re2 retains the observed rook coordination after the bishop exchange"):
    val packet = re2.observationsPacket.get
    val paths = re2.preparedPaths.get
    val intent = MoveIntent.read(packet, paths, re2.responseSearches).get
    val branch = intent.branches.find(_.path.moves.take(2) == List("e1e2", "f6d5")).get
    assertEquals(branch.path.moves.take(9),
      List("e1e2", "f6d5", "f4e5", "e7f6", "f3g3", "f6e5", "e2e5", "d5e7", "a1e1"))
    assertEquals(branch.realization, Some(8))
    assertEquals(branch.through, 8)
    assert(branch.retained)
    val answer = MoveIntent.summary(packet, paths, re2.responseSearches).get
    assert(answer.text.contains("establishes the prepared rook connection"), answer.text)
    val comparison = intent.comparison.get
    assert(comparison.completePurpose && intent.complete)
    assert(comparison.reference.attacks.exists(p => p.at == "f6" && p.role.name == "knight"))
    assert(comparison.rootUses.exists(_.references.contains(RootQuestionSummary.MoveRef(intent.reference, 0))))
    assert(intent.branches.exists(_.path eq intent.root), "the actual assessed root must survive child comparisons")
    val units = (answer.json \ "units").as[List[JsObject]]
    val covered = units.flatMap(u => (u \ "covered_moves").as[List[JsObject]])
    assert(branch.path.certified.get._2.material.get.events.filter(_.plyOffset <= 8).forall(e =>
      covered.contains(RootQuestionSummary.MoveRef(branch.path, e.plyOffset).json)))

  test("either root can supply the useful realization, and a later surviving contact cannot displace it"):
    (screens \ "selection_regressions").as[List[JsObject]].foreach { row =>
      val state = replay(row, finished = false)
      val packet = state.observationsPacket.get
      val paths = state.preparedPaths.get
      val intent = MoveIntent.read(packet, paths, state.responseSearches).get
      assertEquals(intent.question.action, "d4d5")
      assertEquals(intent.question.aim, MoveIntent.Aim.CoordinatedPressure)
      assert(intent.examples.forall(p => p.moves.size == 3 && p.ids == p.declared &&
        p.certified.get._2.material.get.events.isEmpty))
      assert(state.isInstanceOf[AwaitingEngineWork], "an admitted idea still needs its response evidence")
    }

  test("Qa4 keeps both actual roots, late common realization and the pawn that takes over after exchange"):
    val packet = qa4.observationsPacket.get
    val paths = qa4.preparedPaths.get
    val intent = MoveIntent.read(packet, paths, qa4.responseSearches).get
    assert(intent.complete)
    assert(intent.pair.forall(p => intent.branches.exists(_.path eq p)))
    val comparison = intent.comparison.get
    assert(comparison.completePurpose)
    assert(comparison.sharedPreparation.forall((_, offset) => offset >= 2))
    assert(comparison.sharedPreparation.size == 2)
    val successors = intent.branches.flatMap(_.successor)
    assert(successors.nonEmpty)
    assert(successors.forall(work => work.original.at == "c4" && work.actor.at == "d5" && work.continuing))
    assert(MoveIntent.summary(packet, paths, qa4.responseSearches).nonEmpty)

  test("without a witnessed idea the reply survey comes first and a legal attack is not the core explanation"):
    val state = replay((screens \ "unwitnessed_root").as[JsObject], finished = false)
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val intent = MoveIntent.read(packet, paths, state.responseSearches).get
    assert(!intent.supported)
    assert(MoveIntent.summary(packet, paths, state.responseSearches).isEmpty)
    val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
    assertEquals(work.searchAnchor.get.prefixMoves, List("b3a4"))
    val legal = packet.assembly.input.positionHistory.extend(List("b3a4")).toOption.get.currentLegalMoveUcis
    assertEquals(work.rootRestriction, EngineRootRestriction.Restricted(legal.sorted))
    // The new survey's execution key differs from the old post-Nb4 survey.
    // Stop here; the old report cannot certify this newly issued work.
    val saved = (screens \ "unwitnessed_root" \ "survey").as[JsObject]
    assert(work.executionKeySha256 != (saved \ "issued_work" \ "execution_key_sha256").as[String])

  test("a combined purpose answer still rejects an independently prepared owner"):
    val packet = re2.observationsPacket.get
    val paths = re2.preparedPaths.get
    val intent = MoveIntent.read(packet, paths, re2.responseSearches).get
    val foreign = ObservedLineProtocol.prepare(packet)
    val answer = CommentaryAnswer.Answer("functional_response",
      List(CommentaryAnswer.Section(List(CommentaryAnswer.ReferencePurpose(intent.comparison.get)))), Nil, Nil)
    assert(RootQuestionSummary.realize(foreign, intent.pair, answer).isEmpty)

  test("the actual Qa4 survey replaces Nb4 with d5 before testing the newly selected action"):
    val stages = scala.collection.mutable.ListBuffer.empty[CommentaryJobState]
    val row = (screens \ "survey_reselection").as[JsObject]
    val completed = replay(row, onReport = state => { stages += state; () })
    def reading(state: CommentaryJobState) = MoveIntent.read(state.observationsPacket.get,
      state.preparedPaths.get, state.responseSearches).get
    assertEquals(reading(stages.head).question.action, "d3b4")
    assert(!reading(stages.head).supported)
    val chosen = reading(stages(1))
    assertEquals(chosen.question.action, "d4d5")
    assert(chosen.supported && !chosen.complete)
    val work = stages(1).asInstanceOf[AwaitingEngineWork].issuedWork
    assertEquals(work.searchAnchor.get.prefixMoves, List("b3a4", "a6b7"))
    assert(work.rootRestriction match
      case EngineRootRestriction.Restricted(moves) => moves.contains("d4d5") && !moves.contains("d3b4")
      case _ => false)
    assert(reading(completed).complete)

  test("concision shares physical prices while preserving both complete screen accounts"):
    val comparisons = (screens \ "cases").as[List[JsObject]].map { row =>
      val state = replay(row)
      val paths = state.preparedPaths.get
      val packet = state.observationsPacket.get
      val answer = MoveIntent.summary(packet, paths, state.responseSearches).get
      val before = (row \ "snapshot" \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
        .find(c => (c \ "selection").as[String] == "selected").get
      val old = (before \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
      val current = (answer.json \ "units").as[List[JsObject]]
      for field <- List("covered_moves", "required_moves", "proof_moves", "action_moves", "recorded_end_moves") do
        def refs(units: List[JsObject]) = units.flatMap(u => (u \ field).asOpt[List[JsObject]].getOrElse(Nil)).toSet
        assertEquals(refs(current), refs(old), (row \ "id").as[String] + ": " + field)
      assert(current.forall(u => !(u \ "english_only").asOpt[Boolean].contains(true)))
      assert(answer.text.contains("share this cost"), answer.text)
      val intent = MoveIntent.read(packet, paths, state.responseSearches).get
      if (row \ "id").as[String] == "qa4" then
        val sections = MoveIntent.sections(intent)
        val pawns = sections.flatMap(_.statements).collect { case p: CommentaryAnswer.Material if
          p.cost.events.size == 2 && p.cost.events.forall(_.capture.exists(_.capturedRole.name == "pawn")) => p }
        assertEquals(pawns.size, 2)
        assertEquals(pawns.head.cost.commonExchanges, pawns.last.cost.commonExchanges)
        assert(intent.comparison.get.physicalLosses(pawns.head) != intent.comparison.get.physicalLosses(pawns.last))
        val group = CommentaryAnswer.Section(CommentaryAnswer.PreparedReply(intent.branches.last) :: pawns)
        assert(ReferencePurposeText.groupedCosts(group, ko = false).isEmpty,
          "equal pawn counts must not merge different original pawns")
      Json.obj("id" -> (row \ "id").as[String], "before" -> (before \ "detail" \ "summary_question").get,
        "after" -> answer.json, "english" -> answer.text)
    }
    val out = Path.of("../target/research-runs/concise-purpose-20261015/same-reports.json")
    Files.createDirectories(out.getParent)
    Files.writeString(out, Json.prettyPrint(JsArray(comparisons)))

  test("concision twenty-second requests retain the purpose agenda instead of reopening generic discovery"):
    val row = (fixture("concise-purpose-browser") \ "regressions").as[List[JsObject]].head
    // The old third work tested a generic unrestricted reply; it cannot be reused for the new question.
    val roots = row ++ Json.obj("saved_reports" -> (row \ "saved_reports").as[List[JsObject]].take(2))
    val state = replay(roots, finished = false, budgetMs = 20000L)
    val shorter = replay(roots, finished = false)
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val intent = MoveIntent.read(packet, paths, state.responseSearches).get
    assertEquals(intent.question.aim, MoveIntent.Aim.RookCoordination)
    assertEquals(intent.question.action, "a1e1")
    val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
    assertEquals(work.executionKeySha256, shorter.asInstanceOf[AwaitingEngineWork].issuedWork.executionKeySha256)
    assert(work.executionKeySha256 != (row \ "saved_reports").as[List[JsObject]].last
      .value("issued_work").as[JsObject].value("execution_key_sha256").as[String])

  test("concision joins the same observed outcome but preserves a later different realization"):
    val output = (fixture("concise-purpose-browser") \ "composition_regressions").as[List[JsObject]].map { row =>
      val state = replay(row, budgetMs = 20000L)
      val packet = state.observationsPacket.get
      val paths = state.preparedPaths.get
      val intent = MoveIntent.read(packet, paths, state.responseSearches).get
      val main = intent.branches.find(_.course == MoveIntent.Course.MainReply).get
      val peer = intent.branches.find(b => b.course != MoveIntent.Course.MainReply &&
        b.path.moves.take(3) == main.path.moves.take(3)).get
      val answer = MoveIntent.summary(packet, paths, state.responseSearches).get
      val source = (row \ "snapshot" \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
        .find(c => (c \ "selection").as[String] == "selected").get
      val old = (source \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
      val current = (answer.json \ "units").as[List[JsObject]]
      for field <- List("covered_moves", "required_moves", "proof_moves", "action_moves", "recorded_end_moves") do
        def refs(units: List[JsObject]) = units.flatMap(u => (u \ field).asOpt[List[JsObject]].getOrElse(Nil)).toSet
        assertEquals(refs(current), refs(old), (row \ "id").as[String] + ": " + field)
      assert(current.forall(u => !(u \ "english_only").asOpt[Boolean].contains(true)))
      if (row \ "id").as[String] == "qa4" then
        assert(MoveIntent.sameObservedOutcome(main, peer))
        assertEquals("taking over the pressure".r.findAllIn(answer.text).size, 1)
        assert(answer.text.contains("original d4 pawn") && answer.text.contains("original c6 pawn"), answer.text)
        assert(answer.text.contains("inferior to Nf4"), answer.text)
      else
        assert(!MoveIntent.sameObservedOutcome(main, peer))
        assert(main.realization.isEmpty && peer.realization.nonEmpty)
        assert(answer.text.contains("For the same choice") && answer.text.contains("establishes the prepared rook connection"))
      Json.obj("id" -> (row \ "id").as[String], "answer" -> answer.json, "english" -> answer.text)
    }
    Files.writeString(Path.of("../target/research-runs/concise-purpose-20261015/fresh-reports.json"),
      Json.prettyPrint(JsArray(output)))

  test("a continuous root function is explained once with its endpoint while changed functions remain"):
    val output = (fixture("concise-purpose-browser") \ "cases").as[List[JsObject]].map { row =>
      val state = replay(row, budgetMs = 20000L)
      val packet = state.observationsPacket.get
      val paths = state.preparedPaths.get
      val intent = MoveIntent.read(packet, paths, state.responseSearches).get
      val comparison = intent.comparison.get
      val answer = MoveIntent.summary(packet, paths, state.responseSearches).get
      val source = (row \ "snapshot" \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
        .find(c => (c \ "selection").as[String] == "selected").get
      val before = (source \ "detail" \ "summary_question").as[JsObject]
      val current = (answer.json \ "units").as[List[JsObject]]
      for field <- List("covered_moves", "required_moves", "proof_moves", "action_moves", "recorded_end_moves") do
        def refs(units: List[JsObject]) = units.flatMap(u => (u \ field).asOpt[List[JsObject]].getOrElse(Nil)).toSet
        assertEquals(refs(current), refs((before \ "units").as[List[JsObject]]), (row \ "id").as[String] + ": " + field)
      assert(current.forall(u => !(u \ "english_only").asOpt[Boolean].contains(true)))
      if (row \ "id").as[String] == "re2" then
        val arm = comparison.reference
        val target = arm.protections.find(_.target.at == "d4").get.target
        assert(comparison.continuedRootContact(arm, target, attack = false))
        assert(!comparison.continuedRootContact(arm, target, attack = true))
        val purpose = ReferencePurposeText.purpose(comparison, ko = true)
        assert(purpose.contains(RootQuestionSummary.MoveRef(arm.path, arm.through)))
        assert(purpose.collect { case s: String => s }.mkString.contains("까지 계속 지킵니다"))
        val aftermath = ReferencePurposeText.position(comparison, arm, ko = true,
          purposeAnnounced = true).collect { case s: String => s }.mkString
        assert(!aftermath.contains("백 e5 비숍은 백 d4 나이트를 지킵니다"), aftermath)
        val unintroduced = ReferencePurposeText.position(comparison, arm, ko = true).collect { case s: String => s }.mkString
        assert(unintroduced.contains("백 e5 비숍은 백 d4 나이트를 지킵니다"), unintroduced)
        assert(answer.text.contains("guard White's knight on d4") && answer.text.contains("guard continues through"))
        assert(answer.text.contains("establishes the prepared rook connection") && answer.text.contains("bishop"))
        val branch = intent.branches.find(b => b.realization.nonEmpty && b.retained).get
        val section = CommentaryAnswer.Section(List(CommentaryAnswer.PreparedReply(branch)))
        val standalone = MoveIntent.groupedReplies(section, ko = false).get.parts.collect { case s: String => s }.mkString
        val introduced = MoveIntent.groupedReplies(section, ko = false, Set(intent)).get.parts.collect { case s: String => s }.mkString
        assert(standalone.contains("front rook's pressure") && !introduced.contains("front rook's pressure"))
        assert(introduced.contains("establishes the prepared rook connection"))
        assertEquals(MoveIntent.groupedReplies(section, ko = false, Set(intent)).get.expressed,
          MoveIntent.groupedReplies(section, ko = false).get.expressed)
      else
        assert(!comparison.hasContinuedRootContact(comparison.reference),
          "the original Nf4 knight is exchanged, so its root function cannot be called continuous")
        assert(answer.text.contains("taking over the pressure") && answer.text.contains("original d4 pawn") &&
          answer.text.contains("original d3 knight"), answer.text)
      Json.obj("id" -> (row \ "id").as[String], "before" -> before, "after" -> answer.json, "english" -> answer.text)
    }
    val out = Path.of("../target/research-runs/function-continuity-20261015/same-reports.json")
    Files.createDirectories(out.getParent)
    Files.writeString(out, Json.prettyPrint(JsArray(output)))
