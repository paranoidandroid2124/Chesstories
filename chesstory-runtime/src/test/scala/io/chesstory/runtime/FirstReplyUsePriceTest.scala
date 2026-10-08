package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import play.api.libs.json.*

/** The unchanged actual reply opens a file used for a later capture. A
  * computed price alone is not permission to select another future episode. */
class FirstReplyUsePriceTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import RootQuestionSummary.MoveRef

  private lazy val fixture =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/first-reply-clearance-price-produced.json.gz"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()

  private lazy val state =
    val record = fixture \ "record"
    assertEquals((record \ "actual_engine_execution").as[Boolean], true)
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get
    var current = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 180001L, request.engineProfile, investigateChoices = true,
        previewDepth = Some(8)), 1L, request.inputLines, request.proposals).toOption.get
    val reports = (record \ "saved_reports").as[List[JsObject]]
    assertEquals(reports.size, 6)
    reports.zipWithIndex.foreach { (saved, index) =>
      val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("a" * 32, request.requestId, 180001L, current))
      assertEquals((wire \ "issued_engine_work").as[JsObject], (saved \ "issued_work").as[JsObject],
        "every whole original descriptor must match before its actual receipt is used")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      current = CommentaryJobReducer.submitIssuedWorkReport(current, report, index.toLong + 2L).toOption.get
    }
    assert(current.isInstanceOf[CompletedCommentaryJob])
    current

  private lazy val packet = state.observationsPacket.get
  private lazy val paths = state.preparedPaths.get
  private lazy val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
  private lazy val primary = (fixture \ "original_result" \ "selected_move_reviews").as[List[JsObject]]
    .find(value => (value \ "selection" \ "roles").as[List[String]].contains("played")).get \ "commentary" \ "primary"
  private lazy val pair = List("best_endpoint", "runner_up_endpoint").map(key =>
    paths.certifiedPath(packet, (primary \ key \ "moves").as[List[String]]).get)
  private lazy val routePath = pair.find(_.moves.head == "f5e6").get
  private lazy val expected = (fixture \ "expected_price" \ "moves_uci").as[List[String]]
  private lazy val notes = MoveWorkProjection.select(packet, paths).local
  private def piece(side: String, role: String, square: String) = CommentaryFact.Piece(side, role, square).witness
  private lazy val route = ClearedRoute(MoveRef(routePath, 1), piece("black", "pawn", "a4"),
    piece("black", "pawn", "b3"), piece("black", "rook", "a8"), MoveRef(routePath, 5),
    piece("black", "rook", "a2"), Nil, true, None)
  private def core(answer: Answer) = answer.core.flatMap(_.statements)
  private def protect(seed: List[Statement], computedReply: Boolean = false): Answer =
    val roots = Continuation(pair.map(MoveRef(_, 0)))
    val events = routePath.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 1)
    // The available background capture has its ordinary certified detail
    // account. An uncovered bare capture would independently fail delivery.
    val selected = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison",
      List(Section(List(roots) ++ seed ++
        Option.when(computedReply)(MaterialEvents(routePath, events)).toList)), List(Section(List(route)))).get
      .copy(comparison = Some(ComparisonScope(Set(piece("black", "rook", "a8")))),
        completionSources = Some(roots :: seed))
    MoveWorkProjection.protectRootUses(packet, paths, pair, positions, notes, selected,
      List(Section(List(route, Continuation(List(MoveRef(routePath, 6)))))))

  test("the actual compact comparison retains the selected first reply's opened-file capture price"):
    assertEquals(routePath.moves.take(6), expected)
    val requestId = (fixture \ "record" \ "request" \ "request_id").as[String]
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("a" * 32, requestId, 180001L, state))
    assertEquals((wire \ "result").get, (fixture \ "original_result").get,
      "closure must not rewrite the original numerical judgment, endpoints or evidence")
    val root = (wire \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
      .find(value => (value \ "detail" \ "type").asOpt[String].contains("root_comparison")).get \ "detail"
    val question = root \ "summary_question"
    assertEquals((question \ "question").as[String], "first_target_response")
    val covered = (question \ "units").as[List[JsObject]].flatMap(unit =>
      (unit \ "covered_moves").as[List[JsObject]])
    assert(covered.exists(ref => (ref \ "line_id").as[String] == routePath.line.ref.id &&
      (ref \ "occurrence_id").as[String] == routePath.ids(5)),
      "the already selected axb3 opens the a-file: its actual Rxa2 price must remain in basic")
    assert((root \ "summary").as[String].contains("Rxa2"))

  test("a newly computed first reply price does not select its later independent route use"):
    val answer = protect(Nil, computedReply = true)
    assert(!core(answer).contains(route))
    assert(answer.details.flatMap(_.statements).contains(route))

  test("an explicitly selected first reply closes only its existing route use and price"):
    val answer = protect(List(Continuation(List(MoveRef(routePath, 1)))))
    assert(core(answer).contains(route), "the previous exact reply/use pair is an independent obligation")
    assert(core(answer).flatMap(_.covered).contains(MoveRef(routePath, 5)))
    assert(!core(answer).contains(Continuation(List(MoveRef(routePath, 6)))),
      "the next bishop capture is available in detail, not a new use seeded by the added rook price")
    assert(answer.details.flatMap(_.statements).contains(Continuation(List(MoveRef(routePath, 6)))))
    assert(RootQuestionSummary.realize(paths, pair, answer).nonEmpty)
