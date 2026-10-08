package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import play.api.libs.json.*
import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*

/** The real v24 first four NB1 reports keep their execution keys and scores.
  * A declared legal Rd6 example supplies an explicit conditional context.
  * No Sections are manufactured: withMoveEffects runs the production bundle
  * assembly, fallback, functional replacement, protection and rendering.
  * This tests that seam, not automatic discovery or the whole score cause.
  */
class PhysicalBundleAssemblyTest extends munit.FunSuite:
  import CommentaryAnswer.LegalReplyOption
  import CommentaryFact.{Piece, Predicate}
  import CommentaryProposal.CompareMoves
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}

  private lazy val fixture: JsValue =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/physical-bundle-nb1-produced.json"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    Json.parse(Files.readString(path, UTF_8))

  private def sourcePacket(): EvidenceBackedJudgmentPacket =
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((fixture \ "request").get).getBytes(UTF_8)).toOption.get
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 180_001L, request.engineProfile, investigateChoices = true,
        previewDepth = Some(8)), 1L).toOption.get
    (fixture \ "saved_reports").as[List[JsObject]].zipWithIndex.foreach { (saved, index) =>
      val active = state.asInstanceOf[AwaitingEngineWork]
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      assertEquals(active.issuedWork.workId, report.reportedWorkId)
      assertEquals(active.issuedWork.executionKeySha256, report.reportedExecutionKeySha256,
        "the real prefix cannot be replayed against a differently owned work")
      state = CommentaryJobReducer.submitIssuedWorkReport(active, report, index.toLong + 2L).toOption.get
    }
    state.observationsPacket.get

  private case class Prepared(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], projection: CommentarySelection.Result, base: JsObject,
      proof: FunctionalReplyReading.Proof, originalComparison: CandidateComparisonFact):
    def statement: LegalReplyOption =
      def path(moves: List[String]) = paths.paths.find(_.moves == moves).get
      LegalReplyOption(path(proof.reference), path(proof.alternative), path(proof.option), proof)
    def project(candidate: JsObject = base): JsObject = projection.withMoveEffects(candidate,
      context = List(MoveWorkProjection.Context("", replyFunction = Some(proof))))

  private def prepare(): Prepared =
    val original = sourcePacket()
    val before = CommentarySelection.primaryComparisonRecords(original).head._2
    val example = fixture \ "conditional_example"
    val left = (example \ "reference").as[List[String]]
    val right = (example \ "alternative").as[List[String]]
    val option = (example \ "option").as[List[String]]
    val histories = List(left, right, option).map { moves =>
      val history = original.assembly.input.positionHistory.admitToFirstAutomaticTerminal(moves).toOption.get
      assertEquals(history.stepsAfter(original.assembly.input.positionHistory).get.map(_.uci), moves)
      history
    }
    val input = MoveReviewInputAdmission.retainLegalContinuations(original.assembly.input, histories)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(
      MoveReviewAssembler.assemble(input, Some(original.assembly))).get
    val after = CommentarySelection.primaryComparisonRecords(packet).head._2
    assertEquals(after, before, "adding a declared legal example cannot replace any numerical comparison")
    assertEquals(after.comparison.verdict, MoveChoiceVerdict.Mistake)
    assertEqualsDouble(after.comparison.candidateWinPercentDeltaForMover,
      (fixture \ "expected_primary" \ "delta" \ "candidate_win_percent_delta_for_mover").as[Double], 1e-9)
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = List(after.referenceLine, after.candidateLine).map(ref => paths.paths.find(_.line.ref == ref).get)
    assertEquals(pair.map(_.moves), List(
      (fixture \ "expected_primary" \ "reference_endpoint" \ "moves").as[List[String]],
      (fixture \ "expected_primary" \ "played_endpoint" \ "moves").as[List[String]]))
    val projection = MoveWorkProjection.select(packet, paths)
    val base = (projection.baseJson \ "candidates").as[List[JsObject]]
      .find(candidate => (candidate \ "detail" \ "type").asOpt[String].contains("root_comparison")).get
    val knight = Piece("white", "knight", "d2")
    val capture = Predicate("legal_capture", Some(knight), Some(Piece("black", "queen", "c6")))
    val proof = FunctionalReplyReading.Proof(FunctionalReplyReading.RootOwner("declared-frozen-nb1-rd6",
      CompareMoves("function_root", Nil, left.head, right.head)),
      Predicate("control", Some(knight), square = Some("d6")), left, right, option, capture, capture, Nil)
    Prepared(packet, paths, pair, projection, base, proof, before)

  private def units(result: JsObject, layer: String = "units"): List[JsObject] =
    (result \ "detail" \ "summary_question" \ layer).asOpt[List[JsObject]].getOrElse(Nil)
  private def refs(values: List[JsObject], field: String): List[JsObject] =
    values.flatMap(value => (value \ field).asOpt[List[JsObject]].getOrElse(Nil))
  private def offsets(values: List[JsObject], path: PreparedPath): Set[Int] = values
    .filter(value => (value \ "line_id").as[String] == path.line.ref.id)
    .map(value => path.ids.indexOf((value \ "occurrence_id").as[String])).toSet

  test("production physical bundles deliver the conditional centre with actual queen and relocated rook prices"):
    val value = prepare()
    assert(FunctionalReplyReading.valid(value.paths, value.statement))
    val result = value.project()
    assertEquals((result \ "detail" \ "summary_question" \ "question").asOpt[String],
      Some("functional_response"), "the real assembly seam must retain a deliverable compact answer")
    assertEquals((result \ "detail" \ "summary_unavailable").asOpt[JsObject], None)
    val core = units(result)
    val covered = refs(core, "covered_moves")
    assert(Set(13, 14).subsetOf(offsets(covered, value.pair.last)),
      "the Qd2 user's actual loss and its capturing rook's recovery remain visible")
    assert(Set(19, 20).subsetOf(offsets(covered, value.pair.head)),
      "the relocated Rd6/Ra6 user retains its original actual rook price")
    assertEquals(covered.distinct.size, covered.size, "actual captures have a single core owner")
    assert(covered.forall(ref => value.pair.exists(_.line.ref.id == (ref \ "line_id").as[String])),
      "Qxd6/Nxd6 in the declared example are not reported as actual captured material")
    val allCovered = covered ++ refs(units(result, "detail_units"), "covered_moves")
    value.pair.foreach { path =>
      val actual = path.certified.flatMap(_._2.material).get.captures.map(_.plyOffset).toSet
      assert(actual.subsetOf(offsets(allCovered, path)), "all original actual material remains recoverable")
    }
    val text = (result \ "detail" \ "summary").as[String]
    assert(text.contains("Qxd6") && text.contains("Nxd6"), text)
    List("forced", "winning", "compensates", "best move", "evaluation").foreach(word =>
      assert(!text.toLowerCase.contains(word), text))
    assertEquals(CommentarySelection.primaryComparisonRecords(value.packet).head._2, value.originalComparison)

  test("repeating production composition preserves the same core ownership and original mistake assessment"):
    val value = prepare()
    val first = value.project()
    val repeated = value.project(first)
    assertEquals((first \ "detail" \ "summary_question" \ "question").asOpt[String], Some("functional_response"))
    assertEquals((repeated \ "detail" \ "summary_question" \ "question").asOpt[String], Some("functional_response"))
    assertEquals(units(repeated), units(first),
      "the unclosed Rd6 episode cannot revive neighbours through a repeated physical assembly pass")
    assertEquals((repeated \ "detail" \ "comparison_evidence_id").as[String],
      (first \ "detail" \ "comparison_evidence_id").as[String])
    assertEquals(CommentarySelection.primaryComparisonRecords(value.packet).head._2, value.originalComparison)
