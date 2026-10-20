package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.EvidenceBackedJudgmentPacket
import lila.chessjudgment.model.line.EngineLine
import CommentaryAnswer.*
import CommentaryEnglish.at
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import play.api.libs.json.*

/** Legal replay boundaries and exact owned screen reports; no new engine run. */
class IdeaGeneralizationTest extends munit.FunSuite:
  private def fixture(name: String) =
    val stream = new GZIPInputStream(Files.newInputStream(Path.of(
      s"../judgment-evaluation/fixtures/public-commentary-v6/$name.json.gz")))
    try (Json.parse(stream.readAllBytes()) \ "cases").as[List[JsObject]] finally stream.close()
  private lazy val screenCases = fixture("idea-generalization-browser") ++ fixture("causal-function-browser") ++
    fixture("activity-browser") ++ fixture("hypothesis-browser") ++ fixture("priority-browser") ++
    fixture("reply-functions-browser") ++ fixture("counterplay-browser") ++ fixture("obligation-browser") ++
    fixture("preparation-browser") ++ fixture("prevention-browser") ++ fixture("resources-browser") ++
    fixture("quiet-resources-browser")

  private def replay(id: String, reportCount: Int = Int.MaxValue): List[CommentaryJobState] =
    val row = screenCases.find(r => (r \ "id").as[String] == id).get
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((row \ "request").get).getBytes(UTF_8)).toOption.get
    val reports = (row \ "saved_reports").as[List[JsObject]]
    val depth = (reports.head \ "issued_work" \ "search_limits" \ "depth").as[Int]
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(depth, 20001L, request.engineProfile, investigateChoices = true,
        shortJob = true), 1L, request.inputLines, request.proposals).toOption.get
    reports.take(reportCount).zipWithIndex.map { (item, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"$id completed before saved report ${index + 1}")
      val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
      assertEquals(work.executionKeySha256, (item \ "issued_work" \ "execution_key_sha256").as[String])
      val raw = (item \ "report").as[JsObject] ++ Json.obj("work_id" -> work.workId)
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(Json.stringify(raw).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
      state
    }

  private def resourceSnapshot(id: String, state: CommentaryJobState): JsObject =
    val row = screenCases.find(r => (r \ "id").as[String] == id).get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of(s"../target/research-runs/idea-generalization-resources-20261016/$id-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)), UTF_8)
    snapshot

  test("functional facts without a realized use cannot complete a purpose explanation"):
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(
      fen = "6k1/5ppp/8/8/8/8/5PPP/R3R1K1 w - - 0 1", playedMoveUci = "a1b1",
      variations = List(
        EngineLine(List("a1b1", "g8f8", "b1b2"), 100, None, 11),
        EngineLine(List("e1d1", "g8f8", "d1d2"), 140, None, 11)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val reading = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair)).get
    assert(reading.arms.forall(reading.hasFacts))
    assert(reading.arms.forall(a => a.rookFile.nonEmpty && !reading.hasPurpose(a)))
    assert(!reading.completePurpose)
    val answer = ReferencePurposeReading.answer(packet, paths, pair, reading, Some(_)).get
    val basic = RootQuestionSummary.realize(paths, pair, answer).get
    assert(basic.text.contains("open") && basic.text.contains("purpose is not established"), basic.text)

  test("resource identity keeps a checking tempo when the alternative moved its capture target"):
    val state = replay("kh1-resource-fresh", 1).last
    val snapshot = resourceSnapshot("kh1-resource-fresh", state)
    val proofs = FunctionalReplyReading.read(state.proposalEntries)
    assert(proofs.exists(p => p.effect.kind == "check" && p.action.target.exists(_.role == "queen")))
    val selected = (snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").asOpt[String].contains("selected")).get
    val text = (selected \ "detail" \ "summary").as[String]
    assert(text.contains("Bxc7") && text.contains("Nxf3") && text.contains("Nf3+") && text.contains("without a capture") &&
      !text.contains("purpose is not established"), text)

  test("resource support and conditional realization survive an independent reply survey"):
    val state = replay("qg2-first-independent").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val reading = MovePurpose.read(packet, paths, state.responseSearches, Nil).get
    val snapshot = resourceSnapshot("qg2-first-independent", state)
    val selected = (snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").asOpt[String].contains("selected")).get
    val text = (selected \ "detail" \ "summary").as[String]
    assert(reading.branches.exists(_.realizes) && reading.branches.exists(!_.realizes), reading.question.json.toString)
    assert(text.contains("Rh4") && text.contains("h3") && text.contains("R4f7"), text)
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val purpose = ReferencePurposeReading.read(packet, paths, pair, responses = Some(reading)).get
    assert(purpose.played.supportUse.nonEmpty && purpose.played.supportedArrival.nonEmpty,
      "the root queen must remain connected to the rook's later arrival and actual recapture")
    assert(text.contains("supports the rook's arrival on g4") && text.contains("recaptures") &&
      text.contains("original h2 pawn") && text.contains("e4"), text)
    assert((selected \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
      .forall(u => !(u \ "english_only").asOpt[Boolean].contains(true)))

  test("a quiet opposing resource can be met by the ally whose line the root cleared"):
    val state = replay("h4-quiet-resource-fresh", 2).last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val reading = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair)).get
    val answer = ReferencePurposeReading.answer(packet, paths, pair, reading, Some(_)).get
    val enriched = FunctionalReplyReading.withAnswer(packet, paths, pair,
      FunctionalReplyReading.read(state.proposalEntries), answer)
    val option = enriched.core.flatMap(_.statements).collectFirst {
      case value: LegalReplyOption if value.proof.reference == List("h2h4", "c8h3") => value
    }.get
    assertEquals(option.proof.option, List("h2h4", "c8h3", "h1h3"))
    assert(FunctionalReplyReading.valid(paths, option))
    assert(!FunctionalReplyReading.valid(paths, option.copy(proof = option.proof.copy(
      action = option.proof.action.copy(subject = Some(CommentaryFact.Piece("white", "bishop", "f1")))))))
    val row = screenCases.find(r => (r \ "id").as[String] == "h4-quiet-resource-fresh").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-quiet-resources-20261016/h4-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)), UTF_8)
    val selected = (snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").asOpt[String].contains("selected")).get
    val text = (selected \ "detail" \ "summary").as[String]
    assert(text.contains("Rxh3") && text.contains("Bh3") && text.contains("Be2"), text)
    assert(text.contains("bishop") && text.contains("pawn"), text)
    assert(text.contains("opening the kingside castling route") && text.contains("closing the castling route") &&
      text.contains("opens the same castling route") && !text.contains("purpose is not established"), text)

  test("castling preparation requires retained rights and an unattacked king route"):
    val placements = List("1N3PP1", "1N3Pn1")
    for (rank, rights) <- List((placements.head, "-"), (placements.last, "K")) do
      val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(
        fen = s"2bq1rk1/4bppp/1Pp2n2/2Pp4/1p6/$rank/2Q2P1P/B3KB1R w $rights - 0 17",
        playedMoveUci = "h2h4", variations = List(
          EngineLine(List("h2h4", "f6d7", "f1e2"), 0, None, 11),
          EngineLine(List("f1e2", "c8h3", "e2f1"), 0, None, 11)))).get
      val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
      val paths = ObservedLineProtocol.prepare(packet)
      val pair = ReferencePurposeReading.assessmentPair(packet, paths)
      val reading = ReferencePurposeReading.read(packet, paths, pair,
        MoveWorkProjection.select(packet, paths).rootUses(pair))
      assert(reading.forall(_.arms.forall(_.castlePreparation.isEmpty)),
        "a missing right or attacked route cannot establish castling preparation")

  test("the actual alternative's supported arrival outranks an unrelated later return attack"):
    val state = replay("qg2-quiet-resource-fresh").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val reading = MovePurpose.read(packet, paths, state.responseSearches, Nil).get
    assertEquals(reading.question.kind, "supported_advance")
    assertEquals(reading.question.user.at, "g3")
    assertEquals(reading.question.square, "g4")
    assert(reading.followsSupport)
    assertEquals(reading.branches.head.path.moves(1), "f4f7")
    assert(reading.branches.exists(_.realizes) && reading.branches.exists(!_.realizes))
    val row = screenCases.find(r => (r \ "id").as[String] == "qg2-quiet-resource-fresh").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-quiet-resources-20261016/qg2-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)), UTF_8)
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val purpose = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair), responses = Some(reading))
    val answer = purpose.flatMap(ReferencePurposeReading.answer(packet, paths, pair, _, Some(_), Some(reading)))
    val basic = answer.flatMap(RootQuestionSummary.realize(paths, pair, _))
    assert(basic.nonEmpty, s"purpose=${purpose.map(_.completePurpose)}, answer=${answer.map(_.core.flatMap(_.statements).map(_.getClass.getSimpleName))}")
    val selected = (snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").asOpt[String].contains("selected"))
    assert(selected.nonEmpty, basic.map(_.text).getOrElse("no basic account"))
    val text = (selected.get \ "detail" \ "summary").as[String]
    assert(text.contains("maintaining that support") && text.contains("R4f7") && text.contains("g6") &&
      text.contains("Rg4") && text.contains("followed later") && text.contains("exchanged") &&
      !text.contains("purpose is not established"), text)

  private def prepare(moves: List[String]) =
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(
      fen = "6k1/2p3b1/8/8/8/8/8/5K1R w - - 0 1", playedMoveUci = moves.head,
      variations = List(EngineLine(moves, 100, None, 11),
        EngineLine(List("h1h4", "g8f8", "h4f4", "f8e8"), 120, None, 11)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    (packet, ObservedLineProtocol.prepare(packet))

  test("a discovered prevented check remains connected to the actual pair's purpose"):
    val state = replay("ivanchuk-kh1-first-independent", 2).last
    assert(!state.isInstanceOf[AwaitingEngineWork],
      "the shared purpose reading already owns the actual pair and the conditional reply; a third survey is redundant")
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val proofs = FunctionalReplyReading.read(state.proposalEntries)
    val reading = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair)).get
    val previous = ReferencePurposeReading.answer(packet, paths, pair, reading, Some(_)).get
    val result = FunctionalReplyReading.withAnswer(packet, paths, pair, proofs, previous)
    val row = screenCases.find(r => (r \ "id").as[String] == "ivanchuk-kh1-first-independent").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-prevention-20261016/kh1-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)), UTF_8)
    assert(proofs.exists(_.effect.kind == "check"), "the prevention is already discovered")
    assert(result.core.flatMap(_.statements).exists {
      case value: LegalReplyOption => value.proof.effect.kind == "check" &&
        value.proof.action.target.exists(_.role == "queen")
      case _ => false
    },
      "a certified prevention must enrich the missing root purpose while retaining both actual choices")
    assert(result.core.flatMap(_.statements).exists {
      case ReferencePurpose(value) => value.conditionalPurposes.nonEmpty
      case _ => false
    }, "the conditional idea belongs to the missing purpose, not a replacement for the actual pair")
    val selected = (snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").asOpt[String].contains("selected")).get
    val question = selected \ "detail" \ "summary_question"
    assertEquals((question \ "question").as[String], "functional_response")
    assert((question \ "units").as[List[JsObject]].forall(u => !(u \ "english_only").asOpt[Boolean].contains(true)))
    val text = (selected \ "detail" \ "summary").as[String]
    assertEquals(pair.map(_.moves.head).toSet, Set("g1h1", "d1e2"),
      "retain the actual assessment pair instead of replacing Qe2 with the later hypothetical Nd4 inquiry")
    assert(text.contains("Bxc7") && text.contains("queen") && text.contains("Qe2") &&
      !text.contains("purpose is not established"), text)
    val option = result.core.flatMap(_.statements).collectFirst { case v: LegalReplyOption => v }.get
    assert(result.core.flatMap(_.covered).filter(_.path eq option.available).map(_.offset).toSet == Set(1, 2),
      "the conditional knight loss and queen capture must both remain accounted for")
    assert(!FunctionalReplyReading.valid(paths, option.copy(proof = option.proof.copy(
      option = option.proof.option.updated(2, "f4g5")))), "a different legal action cannot inherit the queen proof")

  test("a longer owned reply witness keeps its separate ranking receipt"):
    val state = replay("ferry-d6-prepared-live").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val projection = MoveWorkProjection.select(packet, paths)
    val proofs = TargetResponseInvestigation.firstTargets(packet, paths, state.responseInquiries, state.responseSearches)
    val proof = proofs.head
    val original = proof.fullPool.pathsByMove("b2b3")
    val witness = proof.fullPool.continuationWitness("b2b3", List(proof.captureDecline)).get
    assert(witness.moves.size > original.moves.size && witness.line.ref != original.line.ref)
    assert(witness eq proof.captureDecline.pathsByMove("b2b3"))
    assertEquals(proof.fullPool.continuationWitness("not-a-reply", state.responseSearches), None)
    val reading = ReferencePurposeReading.read(packet, paths, pair, projection.rootUses(pair)).get
    val previous = ReferencePurposeReading.answer(packet, paths, pair, reading, Some(_)).get
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val result = ResponsePurposeReading.withAnswer(packet, paths, pair, proofs, projection.local, previous,
      value => MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, projection.local, value),
      projection.rootUses)
    val choice = result.core.flatMap(_.statements).collectFirst { case value: FirstReplyChoice => value }.get
    assert(choice.preferred.path eq original, "ranking cannot be relabeled as the later restricted search")
    val activity = result.core.flatMap(_.statements).collect { case ActivityUse(value, _) => value }
      .find(_.path.moves(1) == "b2b3").get
    assert(activity.path eq witness, "the actual later path, not a spliced PV, owns the realized idea")
    assert(witness.moves.contains("d8d7") && witness.moves.contains("d7b7"))
    assert(!PurposeActivityReading.discoverReplies(packet, paths, witness, projection.rootUses(List(witness)))
      .exists(_.original.at == "d8"), "quiet coordination still needs a different function detector; do not invent an attack")
    val row = screenCases.find(r => (r \ "id").as[String] == "ferry-d6-prepared-live").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    val selected = (snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").asOpt[String].contains("selected")).get
    assert((selected \ "detail" \ "summary_question").asOpt[JsObject].nonEmpty)
    Files.writeString(Path.of("../target/research-runs/idea-generalization-prevention-20261016/d6-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)), UTF_8)

  test("linked preparations retain the settled activity after a temporary pawn contact"):
    val state = replay("ferry-d6-first-independent").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val uses = MoveWorkProjection.select(packet, paths).rootUses(pair)
    val root = pair.find(_.moves.head == "d7d6").get
    val candidates = PurposeActivityReading.discover(packet, paths, root, uses)
    val selected = PurposeActivityReading.choose(candidates)
    assertEquals(selected.map(_.original.at), Some("d8"),
      s"The pawn is exchanged; its departure and the bishop's departure enable the queen. ${candidates.map(a => (a.original.at, a.entry, a.use, a.seed.cause))}")
    assertEquals(selected.map(a => a.path.moves(a.use)), Some("d8d5"))
    val comparison = ReferencePurposeReading.read(packet, paths, pair, uses).get
    assert(comparison.played.through >= selected.get.use)
    assert(comparison.played.losses.exists(_.at == "d7"))
    assert(comparison.played.counterplay.exists(_.activity.recapturedGuard.exists(_.at == "e5")),
      "Nd3's defense and Nxe5's resulting knight placement are a functional reply, not an incidental guard")
    val search = state.responseSearches.find(_.coversAllLegalMoves).get
    val activities = search.pathsByMove.values.map(path => path -> PurposeActivityReading.discoverReplies(
      packet, paths, path, MoveWorkProjection.select(packet, paths).rootUses(List(path)))).toMap
    val replies = PurposeActivityReading.replyPair(packet, search, activities, comparison.played.activity).get
    assertEquals(replies.map(_.path.moves(1)), List("e1d3", "b2b3"),
      "b3 is the stronger distinct returned reply; do not freeze the old Nf3 selection")
    assertEquals(replies.head.original.at, "d8")
    assertEquals(replies.map(a => a.path.moves(a.use)), List("d8d5", "d8d7"))
    val notes = MoveWorkProjection.select(packet, paths).local
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    assert(ReferencePurposeReading.answer(packet, paths, pair, comparison, Some(_)).nonEmpty,
      "the selected preparation must close its own material account")
    val answer = ReferencePurposeReading.answer(packet, paths, pair, comparison,
      value => MoveWorkProjection.completeSelectedConditions(packet, paths, pair, positions, notes, value))
    assert(answer.nonEmpty, "mandatory conditions cannot discard the selected comparison")
    val rendered = answer.get.core.map(section => CommentaryEnglish.renderedSection(section))
    assert(RootQuestionSummary.realize(paths, pair, answer.get).nonEmpty,
      s"The selected chain must reach the basic account: ${rendered.map(r => (r.parts.size, r.parts.map {
        case move: RootQuestionSummary.MoveRef => move.path.moves(move.offset)
        case text: String => text
      }.mkString))}")
    val row = screenCases.find(r => (r \ "id").as[String] == "ferry-d6-first-independent").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    val question = (snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").asOpt[String].contains("selected")).get \ "detail" \ "summary_question"
    assertEquals((question \ "question").as[String], "selected_comparison")
    assert((question \ "units").as[List[JsObject]].forall(u => !(u \ "english_only").asOpt[Boolean].contains(true)),
      "the same preparation moves and recapture must reach both languages")
    Files.writeString(Path.of("../target/research-runs/idea-generalization-preparation-20261016/d6-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)), UTF_8)

  test("a returning blocker does not certify a continuous preparation chain"):
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(
      fen = "2rq1rk1/3pbppp/p3p3/1p1bP3/3P4/4Q3/PP1B1PPP/R3NRK1 b - - 0 16", playedMoveUci = "d7d6",
      variations = List(EngineLine(List("d7d6", "e1d3", "d6e5", "d3e5", "d5b7", "f1c1", "b7d5",
        "a2a3", "d5b7", "h2h3", "d8d5"), 100, None, 11),
        EngineLine(List("f7f6", "e5f6", "e7f6", "e1d3"), 120, None, 11)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val uses = MoveWorkProjection.select(packet, paths).rootUses(ReferencePurposeReading.assessmentPair(packet, paths))
    assert(!uses.flatMap(_._1.statements).exists {
      case route: ClearedRoute => route.originalUser.at == "d8" && route.preparations.nonEmpty
      case _ => false
    }, "the bishop leaves and restores the obstruction; its later clearance cannot borrow the earlier chain")

  test("check evasion keeps the actual comparison and the opposing pawn continuation"):
    val state = replay("kf8-first-independent", reportCount = 1).last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val root = paths.purposePosition(pair.head, 0).get
    assert(root.view.currentCheckSources(chess.Black).nonEmpty)
    val reading = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair))
    assert(reading.nonEmpty, "check evasion is an obligation of both real choices, not a reason to discard their comparison")
    assertEquals(reading.get.arms.map(_.evasion.map(_.kind)), List(Some("capture_checker"), Some("king_move")))
    assert(reading.get.played.through >= 8, "the pawn's discovered check and the king recapture remain inside the account")
    assert(reading.get.played.losses.exists(p => p.at == "f4" && p.role.name == "pawn"))
    val row = screenCases.find(r => (r \ "id").as[String] == "kf8-first-independent").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-obligations-20261016/kf8-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get,
        "snapshot" -> snapshot)), UTF_8)

  test("a strong opposing exchange remains a reply even when the original beneficiary disappears"):
    val state = replay("bd7-counterplay-live").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val result = MovePurpose.read(packet, paths, state.responseSearches, Nil)
    assert(result.nonEmpty, MovePurpose.question(packet, paths, state.responseSearches).map(_.json).toString)
    val reading = result.get
    assert(reading.search.coversAllLegalMoves)
    val surveyed = reading.search.lines.map(line => reading.search.pathsByMove(line.evaluation.moves.head))
    val activityNotes = MoveWorkProjection.select(packet, paths).rootUses(surveyed)
    val diagnostic = surveyed.map(path => (path.moves(1),
      PurposeActivityReading.discoverReplies(packet, paths, path, activityNotes).map(a =>
        (a.original.at, a.entry, a.use, a.seed.cause)),
      reading.search.pairAnswer(packet, surveyed.head.moves(1), path.moves(1)).map(_.qualityBand)))
    assertEquals(reading.branches.map(_.path.moves(1)), List("d2e3", "e1e7"),
      s"Qe3 is strongest; Rxe7 must retain opposing play. $diagnostic")
    assertEquals(reading.branches.last.purposeActivity.map(_.original.at), Some("f4"))
    assertEquals(reading.branches.last.purposeActivity.flatMap(_.seed.control).map(_.controller.at), Some("e7"))
    val row = screenCases.find(r => (r \ "id").as[String] == "bd7-counterplay-live").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-obligations-20261016/bd7-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)), UTF_8)

  test("quiet intermediate posts lead to an owned functional use without an added search"):
    val (packet, paths) = prepare(List("h1h3", "g8f8", "h3a3", "f8e8", "a3a7", "e8d8"))
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val uses = MoveWorkProjection.select(packet, paths).rootUses(pair)
    val routes = uses.flatMap(_._1.statements).collect { case route: PieceRoute => route }
    assert(routes.exists(_.offsets == List(0, 2, 4)))
    val question = paths.functionalPurpose.get
    assertEquals(question.kind, "piece_pressure", "the earlier a3 entry is only a step toward the useful a7 role")
    assertEquals(question.use, 4)
    assertEquals(question.plan.get.route, List(0, 2, 4))
    val reading = ReferencePurposeReading.read(packet, paths, pair, uses).get
    assert(reading.rootUses.exists {
      case route: PieceRoute => route.offsets == List(0, 2, 4)
      case _ => false
    })
    assert(reading.arms.find(_.path.moves.head == "h1h3").exists(_.through >= 4))
    assert(!reading.belongsTo(ObservedLineProtocol.prepare(packet)))

  test("returning to the original square cannot make a later route the root move's purpose"):
    val (packet, paths) = prepare(List("h1h3", "g8f8", "h3h1", "f8e8", "h1a1", "e8d8", "a1a7"))
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val routes = MoveWorkProjection.select(packet, paths).rootUses(pair).flatMap(_._1.statements)
      .collect { case route: PieceRoute if route.path.moves.head == "h1h3" => route }
    assertEquals(routes, Nil)
    assert(!paths.functionalPurpose.exists(_.plan.exists(!_.potential)))

  test("a reply survey preserves late defense when a later comparison no longer has the same threat"):
    val stages = replay("ng8-full-survey-final")
    val state = stages.last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    assert(!paths.functionalPurpose.exists(_.plan.exists(!_.potential)), "the short root PV lacks the useful route")
    val discovered = MovePurpose.question(packet, paths, state.responseSearches).get
    val reading = MovePurpose.read(packet, paths, state.responseSearches, Nil).get
    val plan = reading.witnessedPlan.get
    assertEquals(plan.targets.map(_.at), List("d5"))
    assert(plan.targets.forall(_.side == reading.question.actor.side))
    assertEquals(plan.path.moves(plan.use), "g8f6")
    assertEquals(discovered.plan, Some(plan), "work allocation and delivery select the same owned witness")
    assertEquals(plan.path.moves(1), "e2c3",
      "Bh3 removes the d5 attack in the Rf1 line; Nc3 still needs the defensive task")
    assert(reading.search.pathsByMove.values.exists(_ eq plan.path))
    assertEquals(MovePurpose.observedQuestion(packet, paths, state.responseSearches).get.plan, Some(plan))
    val restricted = state.responseSearches.filter(_.work.rootRestriction != EngineRootRestriction.Unrestricted)
    assert(!MovePurpose.candidate(packet, paths, restricted).exists(_.plan.exists(!_.potential)),
      "a forced response subset cannot discover the task it was asked to test")
    val independent = state.responseSearches.filter(_.work.rootRestriction == EngineRootRestriction.Unrestricted)
    val survey = MovePurpose.read(packet, paths, independent, Nil).get
    assert(survey.witnessedPlan.nonEmpty,
      "a complete reply survey can witness the purpose without a geometric challenge among its returned moves")
    assert(!(survey.witnessedPlan.get.path eq plan.path), "the same task can have another independently owned route")
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val comparison = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair), responses = Some(reading)).get
    assert(comparison.functional.exists(_.witnessedPlan.nonEmpty))

  test("quiet intermediate defense does not displace the later witnessed knight role"):
    val state = replay("nb1-survey-final").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val question = MovePurpose.question(packet, paths, state.responseSearches).get
    assertEquals(question.kind, "piece_pressure")
    val plan = question.plan.get
    assertEquals(plan.route.map(plan.path.moves), List("c3b1", "b1d2", "d2b3", "b3c5"))
    assert(plan.targets.exists(_.role.name == "bishop"))
    assert(plan.through >= plan.use)

  test("exchanged controllers and the surviving piece's later access remain meaningful when both roots transpose"):
    val state = replay("nxd4-after-delivery-fix").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val reading = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair)).get
    assert(reading.completePurpose && reading.sameAftermath)
    assert(reading.sharedControlUse.nonEmpty)
    assertEquals(reading.arms.map(_.through), List(6, 6))
    reading.arms.foreach { arm =>
      val use = arm.controlUse.get
      assertEquals(use.controller.at, "d4")
      assertEquals(use.controller.role.name, "knight")
      assertEquals(use.change, 0)
      assertEquals(use.originalUser.at, "c5")
      assertEquals(use.user.at, "e6")
      assertEquals(use.use, 6)
      assert(paths.trackedPieceAfter(arm.path, use.controller, use.use + 1).contains(None))
      assert(paths.trackedPieceAfter(arm.path, use.originalUser, use.use + 1).contains(Some(use.user)))
    }

  test("the same removed controller and later user share a purpose without claiming that different boards transpose"):
    val state = replay("nxd4-final-common").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val reading = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair)).get
    assert(!reading.sameAftermath)
    assert(reading.sharedControlUse.nonEmpty)
    assert(reading.rootUses.nonEmpty, "the other rook route remains a separate function")

  test("a quiet interposition is connected to the same defended pawn's actual capture in an owned reply"):
    val state = replay("be7-first-independent").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val survey = state.responseSearches.find(search => search.coversAllLegalMoves &&
      search.pathsByMove.values.exists(_.moves.take(3) == List("f6e7", "h7h6", "g5c5"))).get
    val branch = survey.pathsByMove.values.find(_.moves.take(3) == List("f6e7", "h7h6", "g5c5")).get
    val use = ReferencePurposeReading.controlUse(packet, paths, branch).get
    assert(!use.removed)
    assertEquals(use.controller.at, "f8")
    assertEquals(use.target.map(_.at), Some("c5"))
    assertEquals(use.originalUser.at, "g5")
    assertEquals(use.use, 2)
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("replay", "replay", 20001L, state))
    val selected = (snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").as[String] == "selected").get
    val units = (selected \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
    val purpose = (units.head \ "ko").as[List[JsValue]].collect { case JsString(s) => s }.mkString
    assert(purpose.contains("c5 폰에 대한 지원 경로") && purpose.contains("바뀐 방어를 이용"), purpose)
    assert(units.forall(u => !(u \ "english_only").asOpt[Boolean].contains(true)))
    val refs = units.flatMap(u => (u \ "covered_moves").as[List[JsObject]]).toSet
    val priced = paths.paths.find(_.moves.take(9) == List("f6e7", "h7h6", "g5c5", "e8e7",
      "e1e7", "c8d8", "a1e1", "f8e7", "c5e7")).get
    assert(refs(RootQuestionSummary.MoveRef(priced, 7).json) && refs(RootQuestionSummary.MoveRef(priced, 8).json),
      "the late bishop-for-rook capture and immediate recapture remain priced together")

  test("restoring the original defender before the target is used invalidates the root's causal claim"):
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(
      fen = "r1q1rbk1/2p2ppp/p4Bb1/2p3Q1/N1P5/1P3P2/P5PP/R3R1K1 w - - 0 26", playedMoveUci = "f6e7",
      variations = List(EngineLine(List("f6e7", "h7h6", "e7f6", "a6a5", "g5c5"), 0, None, 11),
        EngineLine(List("f6c3", "e8e1", "c3e1"), 0, None, 11)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val branch = paths.paths.find(_.moves.head == "f6e7").get
    assertEquals(ReferencePurposeReading.controlUse(packet, paths, branch), None)

  test("the first owned survey explains a declined recapture without issuing a redundant recheck"):
    val state = replay("gelfand-e5-first-pending").last
    assert(!state.isInstanceOf[AwaitingEngineWork])
    val inquiry = state.responseInquiries.find(_.question.rootMove == "e4e5").get
    assertEquals(inquiry.state, "answered")
    assert(inquiry.reading.exists(_.complete))
    assert(state.responseSearches.exists(_.work.workId == "work:1"), "the earlier survey is retained")
    assertEquals(state.responseSearches.size, 1)
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    assert(TargetResponseInvestigation.firstTargets(packet, paths, state.responseInquiries, state.responseSearches).nonEmpty)
    val row = screenCases.find(r => (r \ "id").as[String] == "gelfand-e5-first-pending").get
    val request = (row \ "request").as[JsObject]
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (request \ "request_id").as[String], 20001L, state))
    val selected = (snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").as[String] == "selected").get
    val units = (selected \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
    assert(units.size >= 4, "the actual reply contrast belongs in the basic explanation")
    val output = Path.of("../target/research-runs/idea-generalization-activity-20261016/e5-earlier-report-answer.json")
    Files.createDirectories(output.getParent)
    Files.writeString(output, Json.stringify(Json.obj("request" -> request, "snapshot" -> snapshot)))

  test("the same pawn sacrifice compares two strong captures by their beneficiary and recovery, not by capture labels"):
    val state = replay("e5-beneficiary-baseline").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val projection = MoveWorkProjection.select(packet, paths)
    val survey = state.responseSearches.find(_.coversAllLegalMoves).get
    val pawn = survey.pathsByMove("d6e5")
    val knight = survey.pathsByMove("d7e5")
    def activity(path: ObservedLineProtocol.PreparedPath) =
      PurposeActivityReading.discover(packet, paths, path, projection.rootUses(List(path)))
        .find(_.original.at == "c3").get
    val a = activity(pawn)
    val b = activity(knight)
    assert(a.sameTask(b) && a.differentOutcome(b))
    assertEquals(a.retreat.map(_._2.at), Some("f6"))
    assertEquals(a.recovery, None)
    assertEquals(b.recovery.map(_._2.at), Some("d6"))
    assertEquals(b.through, 4)
    assert(!b.belongsTo(ObservedLineProtocol.prepare(packet)))
    val row = screenCases.find(r => (r \ "id").as[String] == "e5-beneficiary-baseline").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    val selected = (snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").as[String] == "selected").get
    val units = (selected \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
    val replies = (units.last \ "ko").as[List[JsValue]]
    Files.writeString(Path.of("../target/research-runs/idea-generalization-priority-20261016/e5-selection-check.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)))
    val refs = replies.collect { case value: JsObject => value }.toSet
    assert(refs(RootQuestionSummary.MoveRef(pawn, 1).json))
    assert(refs(RootQuestionSummary.MoveRef(knight, 1).json))
    assert(refs(RootQuestionSummary.MoveRef(knight, 4).json))
    assert(!refs(RootQuestionSummary.MoveRef(survey.pathsByMove("f6f5"), 1).json))
    assert(units.forall(u => !(u \ "english_only").asOpt[Boolean].contains(true)))
    val output = Path.of("../target/research-runs/idea-generalization-activity-20261016/e5-same-report-answer.json")
    Files.createDirectories(output.getParent)
    Files.writeString(output, Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)))

  test("an intervening own plan cannot lend its later capture to the first activity"):
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(
      fen = "r1b3k1/pp1n3p/2pp1rp1/8/1PP1P3/2N3P1/P5P1/3RRBK1 w - - 1 22", playedMoveUci = "e4e5",
      variations = List(EngineLine(List("e4e5", "d7e5", "c3e4", "f6f7", "a2a4", "a7a5", "e4d6"), 80, None, 11),
        EngineLine(List("g3g4", "d7e5", "g4g5"), 10, None, 11)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val path = ReferencePurposeReading.assessmentPair(packet, paths).find(_.moves.head == "e4e5").get
    val activity = PurposeActivityReading.discover(packet, paths, path,
      MoveWorkProjection.select(packet, paths).rootUses(List(path))).find(_.original.at == "c3").get
    assertEquals(activity.recovery, None, "a4 starts another own plan before the eventual Nxd6")
    assertEquals(activity.through, 3)

  test("a supported piece remains the purpose through an exchange, instead of the recapturing pawn"):
    val state = replay("h3-first-independent").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val question = MovePurpose.question(packet, paths, state.responseSearches).get
    assertEquals(question.kind, "supported_advance")
    assertEquals(question.user.at, "e2")
    assertEquals(question.user.role.name, "bishop")
    assertEquals(question.path.moves(question.use), "e2g4")
    val reading = MovePurpose.read(packet, paths, state.responseSearches, Nil).get
    assertEquals(reading.branches.map(_.path.moves(1)).toSet, Set("e8e7", "h8g8"))
    val released = reading.branches.find(_.path.moves(1) == "h8g8").get.purposeActivity.get.continuation.get
    assertEquals(released._1.controller.at, "e6")
    assertEquals(released._1.originalUser.at, "d1")
    assertEquals(released._1.path.moves(released._1.use), "d1d5")
    assert(released._2.exists(_.role.name == "queen"))
    reading.branches.foreach { branch =>
      assertEquals(branch.path.moves(branch.use), "e2g4")
      assert(branch.realizes)
      assertEquals(branch.actor.role.name, "bishop")
      assert(branch.through > branch.use, "the beneficiary's exchange and recapture remain in its cost")
      assert(paths.trackedPieceAfter(branch.path, question.user, branch.through + 1).contains(None))
    }
    val row = screenCases.find(r => (r \ "id").as[String] == "h3-first-independent").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    val units = ((snapshot \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(c => (c \ "selection").as[String] == "selected").get \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
    assert(units.forall(u => !(u \ "english_only").asOpt[Boolean].contains(true)))
    val refs = units.flatMap(u => (u \ "covered_moves").as[List[JsObject]] ++
      (u \ "required_moves").as[List[JsObject]]).toSet
    reading.branches.foreach { branch =>
      assert(refs(RootQuestionSummary.MoveRef(branch.path, branch.use).json))
      assert(refs(RootQuestionSummary.MoveRef(branch.path, branch.through).json))
    }
    val output = Path.of("../target/research-runs/idea-generalization-activity-20261016/h3-same-report-answer.json")
    Files.writeString(output, Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)))

  test("support that the provider abandons before entry cannot be attributed to the root move"):
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(
      fen = "2r1k2r/1p3p1p/4bp2/p1q1p3/2P1P3/1PQ5/4BPPP/1R1R2K1 w k - 0 22", playedMoveUci = "h2h3",
      variations = List(EngineLine(List("h2h3", "h8g8", "h3h4", "e8e7", "e2g4"), 100, None, 11),
        EngineLine(List("h2h4", "e8g8", "d1d3"), 100, None, 11)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val path = ReferencePurposeReading.assessmentPair(packet, paths).find(_.moves.head == "h2h3").get
    assert(!PurposeActivityReading.supportedEntries(packet, paths, path).exists(_.original.at == "e2"))

  test("a quiet own plan after the exchange cannot borrow a later use of the vacated control"):
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(
      fen = "2r1k2r/1p3p1p/4bp2/p1q1p3/2P1P3/1PQ5/4BPPP/1R1R2K1 w k - 0 22", playedMoveUci = "h2h3",
      variations = List(EngineLine(List("h2h3", "h8g8", "e2g4", "e6g4", "h3g4", "g8g4", "g1h2", "h7h5", "d1d5"), 100, None, 11),
        EngineLine(List("h2h4", "e8g8", "d1d3"), 100, None, 11)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val path = ReferencePurposeReading.assessmentPair(packet, paths).find(_.moves.head == "h2h3").get
    val activity = PurposeActivityReading.discover(packet, paths, path, Nil).find(_.original.at == "e2").get
    assertEquals(activity.continuation, None)
    assertEquals(activity.through, 5)

  test("an independent reply survey can supply the supported beneficiary missing from the root PV"):
    val state = replay("h3-root-omits-bishop").last
    assert(!state.isInstanceOf[AwaitingEngineWork])
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val roots = ReferencePurposeReading.assessmentPair(packet, paths)
    assert(!roots.find(_.moves.head == "h2h3").get.moves.contains("e2g4"))
    val q = MovePurpose.question(packet, paths, state.responseSearches).get
    assertEquals(q.path.moves(q.use), "e2g4")
    assert(!roots.exists(_ eq q.path))
    assert(state.responseSearches.exists(s => s.coversAllLegalMoves && s.pathsByMove.values.exists(_ eq q.path)))
    val response = MovePurpose.read(packet, paths, state.responseSearches, Nil).get
    assertEquals(response.branches.flatMap(_.purposeActivity).map(_.original.at), List("e2"))
    val purpose = ReferencePurposeReading.read(packet, paths, roots,
      MoveWorkProjection.select(packet, paths).rootUses(roots), responses = Some(response)).get
    assertEquals(purpose.played.activity.map(_.original.at), Some("e2"))
    val restricted = state.responseSearches.filterNot(_.coversAllLegalMoves)
    assert(!MovePurpose.candidate(packet, paths, restricted).exists(_.user.role.name == "bishop"))
    val row = screenCases.find(r => (r \ "id").as[String] == "h3-root-omits-bishop").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-activity-20261016/h3-survey-answer.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)))

  test("an independently observed beneficiary outranks an earlier incidental duty"):
    val state = replay("rc7-first-survey").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val witnesses = state.responseSearches.filter(_.coversAllLegalMoves).flatMap(_.pathsByMove.values)
    val sources = (pair ++ witnesses).distinct
    val mechanisms = MoveWorkProjection.select(packet, paths).rootUses(sources)
    val readings = sources.flatMap(path => PurposeActivityReading.discover(packet, paths, path, mechanisms))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-priority-20261016/rc7-candidates.json"),
      Json.stringify(Json.toJson(readings.map(value => Json.obj("moves" -> value.path.moves,
        "original" -> value.original.at, "entry" -> value.entry, "use" -> value.use,
        "route" -> value.route, "mechanism" -> value.seed.mechanism.map(_.getClass.getSimpleName),
        "attacks" -> value.attacks.map(_.at), "guards" -> value.guards.map(_.at))))))
    val question = MovePurpose.question(packet, paths, state.responseSearches).get
    assertEquals(question.user.at, "b6", "the root frees the later knight route, not merely the first rook guard")
    assertEquals(question.purposeActivity.map(_.route.map(question.path.moves)),
      Some(List("b6c8", "c8d6", "d6f5")))
    val response = MovePurpose.read(packet, paths, state.responseSearches, Nil).get
    assert(response.branches.exists(_.purposeActivity.exists(_.original.at == "b6")))
    assertEquals(response.branches.find(_.path.moves(1) == "h3e3").get.activityRoute
      .map(response.branches.find(_.path.moves(1) == "h3e3").get.path.moves), List("b6c8", "c8d6"))
    val reading = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair), responses = Some(response)).get
    assertEquals(reading.played.activity.map(_.original.at), Some("b6"))
    assert(reading.completePurpose)
    val row = screenCases.find(r => (r \ "id").as[String] == "rc7-first-survey").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-priority-20261016/rc7-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)))

  test("the strongest reply can realize another root opportunity with another piece"):
    val state = replay("bf6-first-independent").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val survey = state.responseSearches.find(_.coversAllLegalMoves).get
    val sources = survey.lines.map(line => survey.pathsByMove(line.evaluation.moves.head))
    val mechanisms = MoveWorkProjection.select(packet, paths).rootUses(sources)
    val activities = sources.flatMap(path => PurposeActivityReading.discover(packet, paths, path, mechanisms))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-reply-functions-20261016/bf6-candidates.json"),
      Json.stringify(Json.toJson(activities.map(value => Json.obj("moves" -> value.path.moves,
        "original" -> value.original.at, "entry" -> value.entry, "use" -> value.use,
        "cause" -> value.seed.cause, "attacks" -> value.attacks.map(_.at), "through" -> value.through)))))
    val reading = MovePurpose.read(packet, paths, state.responseSearches, Nil).get
    assertEquals(reading.branches.map(_.path.moves(1)), List("d1c1", "d2f3"))
    assertEquals(reading.branches.flatMap(_.purposeActivity).map(_.original.at), List("h7", "g7"))
    assertEquals(reading.branches.last.purposeActivity.get.recovery.map(_._1), Some(8))
    assertEquals(reading.branches.last.through, 9, "the bishop exchange must close through the queen recapture")
    val row = screenCases.find(r => (r \ "id").as[String] == "bf6-first-independent").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-reply-functions-20261016/bf6-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)))

  test("the responding side keeps its exchange-enabled pawn break and the resulting price"):
    val state = replay("ahmed-bd7-first-independent").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val reading = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair)).get
    assert(reading.played.through >= 6, "Rxe7-Qxe7 must not end the explanation before f5-fxg6-Kxg6")
    assert(reading.played.losses.exists(p => p.at == "e1" && p.role.name == "rook"))
    assert(reading.played.losses.exists(p => p.at == "e7" && p.role.name == "knight"))
    assertEquals(reading.played.counterplay.flatMap(_.control).map(_.controller.at), Some("e7"))
    assertEquals(reading.played.counterplay.map(_.activity.original.at), Some("f4"))
    assertEquals(reading.played.counterplay.flatMap(_.activity.recapturingKing).map((offset, king) => (offset, king.at)),
      Some(6 -> "g6"))
    assertEquals(reading.replyDefense(reading.played).map(_.capture.offset), Some(4))
    val row = screenCases.find(r => (r \ "id").as[String] == "ahmed-bd7-first-independent").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-counterplay-20261016/bd7-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)))

  test("a separate own plan cannot borrow the later break from a reply exchange"):
    val moves = List("c6d7", "e1e7", "d6e7", "g3f2", "d7e6", "f4f5")
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(
      fen = "1r5r/p3npk1/2bq2p1/2pp4/3n1PP1/PpNP2B1/1P1Q2B1/4RRK1 b - - 0 29",
      playedMoveUci = moves.head, variations = List(EngineLine(moves, 0, None, 11)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val path = paths.paths.find(_.moves == moves).get
    assertEquals(ReferencePurposeReading.controlUse(packet, paths, path, start = 1), None)

  test("a surveyed beneficiary survives an actual comparison with a quiet rook alternative"):
    val state = replay("h3-hypothesis-live").last
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val question = MovePurpose.question(packet, paths, state.responseSearches)
    assertEquals(question.map(_.user.at), Some("e2"))
    val response = MovePurpose.read(packet, paths, state.responseSearches, Nil)
    assert(response.nonEmpty, "the owned independent survey already contains the bishop's actual activity")
    assert(response.get.branches.flatMap(_.purposeActivity).exists(_.original.at == "e2"))
    val purpose = ReferencePurposeReading.read(packet, paths, pair,
      MoveWorkProjection.select(packet, paths).rootUses(pair), responses = response)
    assert(purpose.nonEmpty, "an unknown other purpose must not discard an observed conditional purpose")
    val row = screenCases.find(r => (r \ "id").as[String] == "h3-hypothesis-live").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    Files.writeString(Path.of("../target/research-runs/idea-generalization-hypotheses-20261016/h3-live-reread.json"),
      Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)))
    val summaries = (snapshot \\ "summary_question").toList
    assert(summaries.nonEmpty, "the structured purpose and reply account must reach basic commentary")

  test("an unseen supported activity is tested against the actual own choice after an independent survey"):
    val stages = replay("h3-no-returned-bishop")
    val first = stages.head
    assert(!MoveIntent.discover(first.observationsPacket.get, first.preparedPaths.get, first.responseSearches)
      .exists(_.aim == MoveIntent.Aim.SupportedActivity), "the independent reply survey precedes a latent idea")
    val state = stages.last.asInstanceOf[AwaitingEngineWork]
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val idea = MoveIntent.read(packet, paths, state.responseSearches).get
    assertEquals(idea.question.action, "e2g4")
    assertEquals(idea.question.user.at, "e2")
    assertEquals(idea.question.target.at, "e6")
    assert(!idea.supported && !idea.investigated, "a legal example alone is not a sound plan")
    assertEquals(idea.branches.find(_.course == MoveIntent.Course.MainReply)
      .map(idea.newEntryControl).get.map(_.at), List("h5"))
    val work = state.issuedWork
    assertEquals(work.searchAnchor.map(_.prefixMoves), Some(List("h2h3", "h7h5")))
    assertEquals(work.rootRestriction, EngineRootRestriction.Restricted(List("b3b4", "e2g4")))
    assert(work.requiredDepth <= 18)
    val row = screenCases.find(r => (r \ "id").as[String] == "h3-no-returned-bishop").get
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(
      (row \ "job_id").as[String], (row \ "request" \ "request_id").as[String], 20001L, state))
    val output = Path.of("../target/research-runs/idea-generalization-hypotheses-20261016/h3-pending-action.json")
    Files.writeString(output, Json.stringify(Json.obj("request" -> (row \ "request").get, "snapshot" -> snapshot)))
