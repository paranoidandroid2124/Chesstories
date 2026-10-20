package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import play.api.libs.json.*
import CommentaryAnswer.*
import CommentaryEnglish.at
import RootQuestionSummary.MoveRef

/** Development and explicitly labelled post-heldout regression reports.
  * The first heldout failures stay failures in the research record; these
  * checks are not independent successes. Never borrow another issued PV.
  */
class GeneralizedCoreSelectionTest extends munit.FunSuite:
  private lazy val rows =
    List("generalized-core-development", "generalized-core-post-heldout").flatMap { fixture =>
      val name = s"judgment-evaluation/fixtures/public-commentary-v6/$fixture.json.gz"
      val file = List(Path.of(name), Path.of("..", name)).find(Files.isRegularFile(_)).get
      val stream = new GZIPInputStream(Files.newInputStream(file))
      try (Json.parse(stream.readAllBytes()) \ "cases").as[List[JsObject]] finally stream.close()
    }

  private def replay(id: String, assessmentOnly: Boolean = false): CommentaryJobState =
    val row = rows.find(r => (r \ "id").as[String] == id).get
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((row \ "request").get).getBytes(UTF_8)).toOption.get
    val budget = request.remainingBudgetMs.getOrElse(10000).toLong
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(12, budget + 1, request.engineProfile, investigateChoices = !assessmentOnly,
        shortJob = budget <= 10000), 1L, request.inputLines, request.proposals).toOption.get
    val serializer = PositionCommentaryJobProtocol.getClass.getDeclaredMethods.find(_.getName == "issuedEngineWorkJson").get
    serializer.setAccessible(true)
    val reports = (row \ "saved_reports").as[List[JsObject]]
    (if assessmentOnly then reports.take(2) else reports).zipWithIndex.foreach { (saved, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"$id ended before report $index")
      val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
      val encoded = serializer.invoke(PositionCommentaryJobProtocol, work).asInstanceOf[JsObject]
      def search(value: JsObject) = value - "work_id" - "execution_key_sha256" - "max_search_elapsed_ms"
      assertEquals(search(encoded), search((saved \ "issued_work").as[JsObject]), s"$id report $index")
      val raw = (saved \ "report").as[JsObject] ++ Json.obj("work_id" -> work.workId,
        "execution_key_sha256" -> work.executionKeySha256)
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(Json.stringify(raw).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    assert(!state.isInstanceOf[AwaitingEngineWork], s"$id needs an unrecorded search")
    state

  private def selected(state: CommentaryJobState) =
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.getOrElse(ObservedLineProtocol.prepare(packet))
    val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("test", "test", 10001L, state))
    val selection = (snapshot \ "observations" \ "selection").get
    val candidates = (selection \ "candidates").as[List[JsValue]]
    val detail = candidates.flatMap(c => (c \ "detail").asOpt[JsObject])
      .find(d => (d \ "summary_question").toOption.nonEmpty).get
    val question = (detail \ "summary_question").as[JsObject]
    assertEquals((question \ "question").as[String], "selected_comparison")
    val units = (question \ "units").as[List[JsValue]]
    assert(!units.exists(u => (u \ "english_only").asOpt[Boolean].contains(true)))
    val ko = units.map(u => (u \ "ko").as[List[JsValue]].map {
      case JsString(value) => value
      case ref =>
        val path = paths.paths.find(_.line.ref.id == (ref \ "line_id").as[String]).get
        val offset = path.ids.indexOf((ref \ "occurrence_id").as[String])
        paths.notation.move(path.moves.take(offset), path.moves(offset))
    }.mkString).mkString("\n\n")
    (question, ko)

  private def reading(state: CommentaryJobState) =
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.getOrElse(ObservedLineProtocol.prepare(packet))
    ReferencePurposeReading.read(packet, paths, ReferencePurposeReading.assessmentPair(packet, paths)).get

  test("a captured attacker cannot justify a later protection"):
    val value = reading(replay("historical-bd5"))
    assert(value.arms.exists(_.path.moves.head == "b3d5"))
    assert(!value.arms.flatMap(_.protections).flatMap(_.attackers).exists(p =>
      !p.side.white && p.role.name == "knight" && p.at == "e4"))

  test("an immediately recaptured pawn is not a lasting reply threat"):
    val state = replay("kozakov-f6")
    val value = reading(state)
    assertEquals(value.played.path.moves(1), "e5f6")
    assert(value.played.replyPressure.isEmpty)
    assert(value.reference.replyPressure.exists(_.targets.exists(_.at == "b4")))
    val (_, ko) = selected(state)
    assert(ko.contains("폰 한 쌍을 교환") && ko.contains("나이트 한 쌍을 교환"), ko)
    assert(!ko.contains("흑 c6 나이트의 공격을 피"), ko)

  test("the same first-reply pin is said once but proved in both actual choices"):
    val state = replay("ganguly-b4")
    val value = reading(state)
    val pressure = value.sharedReplyPressure.get
    assert(pressure.alignments.exists((front, rear) => front.at == "f6" && rear.at == "e7"))
    assert(pressure.identities.nonEmpty)
    assert(value.arms.forall(a => ReferencePurpose(value).references.contains(MoveRef(a.path, 1))))
    val (_, ko) = selected(state)
    assertEquals(ko.sliding("흑 f6 나이트가 뒤의 흑 e7 퀸에 핀".length).count(_ == "흑 f6 나이트가 뒤의 흑 e7 퀸에 핀"), 1, ko)
    assertEquals(ko.sliding("f5·f6".length).count(_ == "f5·f6"), 1, ko)
    assertEquals(ko.sliding("백 e2 폰".length).count(_ == "백 e2 폰"), 1, ko)
    List("f6 폰에는 현재 방어자가 없습니다", "흑 f8 룩은 흑 f6 폰을 지킵니다").foreach(s => assert(ko.contains(s), ko))

  test("an unknown root purpose does not let a later pawn plan replace the actual pair"):
    // Keep the historical unknown-purpose boundary for its root evidence.
    // The new preparation question issues different owned child work; old
    // incidental-pawn reports must not be submitted as answers to that work.
    val state = replay("kosintseva-rf3", assessmentOnly = true)
    val value = reading(state)
    assert(!value.completePurpose && !value.hasPurpose(value.played))
    assert(value.played.replyPressure.exists(_.targets.exists(_.at == "c2")))
    val (_, ko) = selected(state)
    List("c4", "Rf3", "Qc6", "백 c2 폰", "장기 목적은 제시된 수순에서 드러나지 않습니다").foreach(s => assert(ko.contains(s), ko))
    assert(!ko.contains("h3 진입"), ko)

  test("an existing knight route and its restored guard survive the new selection"):
    val (_, ko) = selected(replay("historical-ng8"))
    List("Ng8", "Nf6", "흑 d5 폰", "방어를 놓습니다").foreach(s => assert(ko.contains(s), ko))
    assert(!ko.contains("장기 목적은 제시된 수순에서 드러나지 않습니다"), ko)

  test("a delayed identical capture keeps its earlier activity and complete costs"):
    val state = replay("vaulin-bd7")
    val (_, ko) = selected(state)
    List("Bd7", "dxc4", "c8 칸을 비", "교환을 미룹니다", "룩", "나이트", "폰").foreach(s => assert(ko.contains(s), ko))

  test("declining a queen exchange keeps an owned alternative without replacing the assessment pair"):
    val state = replay("historical-qc2")
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val value = reading(state)
    val responses = MovePurpose.read(packet, paths, state.responseSearches, MoveWorkProjection.select(packet, paths).local).get
    val answer = ReferencePurposeReading.answer(packet, paths, pair, value, Some(_), Some(responses)).get
    val alternative = answer.core.flatMap(_.statements).collect { case r: ReferenceExchangeReply => r }.head
    assertEquals(alternative.branch.path.moves(1), "c7b8")
    assert(alternative.branch.activity.exists(s => !s.queensGone && s.mover.nonEmpty))
    val foreign = ObservedLineProtocol.prepare(packet)
    assert(RootQuestionSummary.realize(foreign, pair, answer).isEmpty)
    val (_, ko) = selected(state)
    List("f4", "Qc2", "Qxc2", "Qb8", "퀸").foreach(s => assert(ko.contains(s), ko))

  test("different white knights traded for the same black knight remain two whole exchanges"):
    val state = replay("kozakov-f6-cost-variation")
    val value = reading(state)
    val left = value.reference.losses.filter(_.role.name == "knight")
    val right = value.played.losses.filter(_.role.name == "knight")
    assertEquals(left.filterNot(_.side.white), right.filterNot(_.side.white))
    assertNotEquals(left.filter(_.side.white), right.filter(_.side.white))
    val (_, ko) = selected(state)
    List("백의 b5 나이트와 흑의 c6 나이트를 교환", "백의 f3 나이트와 흑의 c6 나이트를 교환").foreach(s => assert(ko.contains(s), ko))

  test("a checking first reply owns its introduction and reply once without dropping the pawn price"):
    val (_, ko) = selected(replay("historical-ke6"))
    assertEquals(ko.sliding("f5+".length).count(_ == "f5+"), 1, ko)
    List("Ke6", "Kf7", "gxf5", "체크한 폰을 잡", "f4 폰", "d7에 진출하는 룩").foreach(s => assert(ko.contains(s), ko))

  test("post-heldout: pawn release keeps the actual pair, exposed pawn, response contrast and final price"):
    val state = replay("giri-e5")
    val (question, ko) = selected(state)
    List("Qe7", "e5", "흑 c8 비숍", "h3 방향 길", "흑 f7 폰 공격선", "d5", "dxe5",
      "b7 폰을 내줍니다", "b7 폰이 b7에 남습니다", "퀸 한 쌍을 교환").foreach(s => assert(ko.contains(s), ko))
    assert(!ko.contains("c6 폰은 고립폰"), ko)
    val pair = ReferencePurposeReading.assessmentPair(state.observationsPacket.get, state.preparedPaths.get)
    val queen = pair.find(_.moves.head == "d8e7").get
    val paid = (question \ "units").as[List[JsObject]].flatMap(u => (u \ "covered_moves").as[List[JsObject]])
    assert(paid.contains(MoveRef(queen, 11).json), ko)
    assertEquals(queen.moves(11), "g2c6")
    val response = state.preparedPaths.get.paths.find(_.moves.take(3) == List("e6e5", "d4e5", "d8e7")).get
    assertEquals(response.moves(4), "b4c5")
    assert(paid.contains(MoveRef(response, 4).json), ko)

  test("post-heldout: a common clearance must retain who can use the route in each line"):
    val (_, ko) = selected(replay("gligoric-kg2"))
    List("Ne5", "Kg2", "백 룩이 그 길을 지나 d7", "흑 룩이 그 길을 지나 d1",
      "룩 한 쌍", "나이트", "비숍", "열린 d 파일").foreach(s => assert(ko.contains(s), ko))
    assert(!ko.contains("체크를 예방"), ko)

  test("post-heldout: an unchanged rook attack is not repeated as a new final-board function"):
    val (_, ko) = selected(replay("ferry-rd6"))
    List("Ra3", "Rd6", "Rc2", "a5", "b2 폰", "a6 폰", "열린 d 파일").foreach(s => assert(ko.contains(s), ko))
    assertEquals(ko.sliding("백 a6 룩".length).count(_ == "백 a6 룩"), 1, ko)

  test("post-heldout: a later recovery does not become an unbounded pawn-loss claim"):
    val (_, ko) = selected(replay("giri-e5-live-varying-replies"))
    List("Qe7", "e5", "d5", "dxe5", "Qe7까지는", "흑은 폰 한 개를 내줍니다",
      "흑 f5 폰이", "흑 e4 폰을 지킵니다").foreach(s => assert(ko.contains(s), ko))
    assert(!ko.contains("h3 방향 길"), ko) // The same bishop's actual Be6 already supplies this use.
    assert(!ko.contains("흑 d8 퀸의 공격을 피"), ko)
    assert(ko.sliding("e5 수순에서:".length).count(_ == "e5 수순에서:") <= 1, ko)

  test("post-heldout: the same physical pursuit in both roots is spoken once and retains both receipts"):
    val state = replay("kozakov-f6-na7-variation")
    val value = reading(state)
    val (question, ko) = selected(state)
    val left = MoveRef(value.reference.path, 0)
    val right = MoveRef(value.played.path, 4)
    assertEquals(left.path.moves(left.offset), "c6a7")
    assertEquals(right.path.moves(right.offset), "c6a7")
    assert(value.sameOriginalOccupants(left, right, List("d7", "b5", "a7")))
    assert(!value.sameOriginalOccupants(left, MoveRef(right.path, 5), List("d7", "b5", "a7")),
      "the escaped knight cannot still own its old square")
    assertEquals(ko.sliding("백 b5 나이트를 공격".length).count(_ == "백 b5 나이트를 공격"), 1, ko)
    List("Na7·f6 수순 모두", "백의 a3 비숍과 흑의 b4 나이트를 교환", "폰 한 쌍").foreach(s => assert(ko.contains(s), ko))
    assert(!ko.contains("흑 f7 폰을 남깁니다"), ko)
    val units = (question \ "units").as[List[JsObject]]
    val proved = units.flatMap(u => (u \ "proof_moves").as[List[JsObject]])
    List(left, MoveRef(left.path, 3), right, MoveRef(right.path, 5)).foreach(ref =>
      assert(proved.contains(ref.json), s"missing pursuit proof ${ref.json}: $ko"))

  test("post-heldout: an already proved late root use survives the local discovery cutoff with every capture price"):
    val state = replay("vaulin-bd7-a5-variation")
    val (question, ko) = selected(state)
    List("a5", "Bd7", "Ne1", "c8 칸을 비워", "Rd8", "흑 룩이 그 길을 지나 d8",
      "폰 두 쌍·룩 한 쌍을 교환", "흑의 c6 폰을 내줍니다",
      "Rxf8+에는 Qxf8로 체크한 룩을 잡습니다").foreach(s => assert(ko.contains(s), ko))
    assert(!ko.contains("장기 목적은 제시된 수순에서 드러나지 않습니다"), ko)
    val pair = ReferencePurposeReading.assessmentPair(state.observationsPacket.get, state.preparedPaths.get)
    val path = pair.find(_.moves.head == "c8d7").get
    assertEquals(path.moves(14), "a8d8")
    val units = (question \ "units").as[List[JsObject]]
    assert(units.flatMap(u => (u \ "proof_moves").as[List[JsObject]]).contains(MoveRef(path, 14).json))
    val covered = units.flatMap(u => (u \ "covered_moves").as[List[JsObject]])
    path.certified.get._2.material.get.events.filter(_.plyOffset <= 14).foreach(event =>
      assert(covered.contains(MoveRef(path, event.plyOffset).json), "late purpose lost an earlier capture price"))

  test("an already explained root does not expand to a distant incidental route and its extra prices"):
    val id = "giri-e5-late-incidental-rook"
    val (question, ko) = selected(replay(id))
    List("Qe7", "e5", "흑 c8 비숍", "d5", "dxe5", "흑 e4 폰을 지킵니다").foreach(s => assert(ko.contains(s), ko))
    assert(!ko.contains("Rad8"), ko)
    val source = rows.find(r => (r \ "id").as[String] == id).get
    val original = (source \ "snapshot" \ "observations" \ "selection" \ "candidates").as[List[JsValue]]
      .flatMap(c => (c \ "detail" \ "summary_question").asOpt[JsObject]).head
    def paid(q: JsObject) = (q \ "units").as[List[JsValue]].flatMap(u =>
      (u \ "covered_moves").as[List[JsValue]]).toSet
    assertEquals(paid(question), paid(original), "an incidental late route must neither add nor lose the selected prices")

  test("two attacks share one escape only when the intervening quiet attack stays explicit and ordered"):
    import MoveWorkProjection.Detail.*
    val state = replay("vaulin-bd7-a5-variation")
    val paths = state.preparedPaths.get
    val path = ReferencePurposeReading.assessmentPair(state.observationsPacket.get, paths).find(_.moves.head == "c8d7").get
    val pawn = paths.purposePosition(path, 12).get.occupant("e6").get
    val bishop = paths.purposePosition(path, 12).get.occupant("d5").get
    val queen = paths.purposePosition(path, 13).get.occupant("d6").get
    val escaped = paths.purposePosition(path, 14).get.occupant("b7").get
    val first = Action(MoveRef(path, 11), Attack(pawn, bishop, true))
    val next = Action(MoveRef(path, 12), Attack(queen, bishop, true))
    val escapes = List(Action(MoveRef(path, 13), Evades(escaped, pawn)), Action(MoveRef(path, 13), Evades(escaped, queen)))
    val statements = next :: first :: escapes
    val joined = CommentarySequenceText.pursuit(statements, next, ko = true).get
    assertEquals(joined.expressed.toSet, statements.toSet)
    assertEquals(joined.parts.collect { case ref: MoveRef => ref.offset }, List(11, 12, 13))
    assert(CommentarySequenceText.pursuit(statements.filterNot(_ == next), first, ko = true).isEmpty,
      "an unmentioned quiet intermezzo must not disappear inside an attack/escape summary")
