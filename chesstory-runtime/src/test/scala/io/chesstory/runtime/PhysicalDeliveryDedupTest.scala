package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import play.api.libs.json.*
import lila.chessjudgment.model.judgment.*

/** Actual main29 NC4 reports reproduce the repeated root contrast through
  * production assembly. Delivery may assign an exact Statement one owner;
  * it may not alter the numerical choice, drop prices or waive requirements.
  * No conditional proof, score or Section is injected into the first test.
  */
class PhysicalDeliveryDedupTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import CommentaryFact.Piece
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef

  private lazy val fixture: JsValue =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/physical-delivery-nc4-actual29.json"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    Json.parse(Files.readString(path, UTF_8))

  private def packet(): EvidenceBackedJudgmentPacket =
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
        "the actual prefix must retain its original owned work and execution scope")
      state = CommentaryJobReducer.submitIssuedWorkReport(active, report, index.toLong + 2L).toOption.get
    }
    state.observationsPacket.get

  private def units(question: JsValue, key: String): List[JsObject] =
    (question \ key).asOpt[List[JsObject]].getOrElse(Nil)
  private def refs(units: List[JsObject], key: String): Set[JsObject] =
    units.flatMap(unit => (unit \ key).asOpt[List[JsObject]].getOrElse(Nil)).toSet
  private def expected(key: String): Set[JsObject] = (fixture \ "baseline" \ key).as[List[JsObject]].toSet

  private def pair(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths): List[PreparedPath] =
    val comparison = CommentarySelection.primaryComparisonRecords(packet).head._2
    List(comparison.referenceLine, comparison.candidateLine).map(ref => paths.paths.find(_.line.ref == ref).get)

  test("actual NC4 production delivers an exact root contrast once without dropping any original price or requirement"):
    val source = packet()
    val comparison = CommentarySelection.primaryComparisonRecords(source).head._2
    assertEquals(comparison.comparison.verdict, MoveChoiceVerdict.PlayableLoss)
    assertEqualsDouble(comparison.comparison.candidateWinPercentDeltaForMover,
      (fixture \ "expected_primary" \ "delta" \ "candidate_win_percent_delta_for_mover").as[Double], 1e-9)
    val paths = ObservedLineProtocol.prepare(source)
    val currentPair = pair(source, paths)
    assertEquals(currentPair.map(_.moves), List(
      (fixture \ "expected_primary" \ "best_endpoint" \ "moves").as[List[String]],
      (fixture \ "expected_primary" \ "runner_up_endpoint" \ "moves").as[List[String]]))
    val projection = MoveWorkProjection.select(source, paths)
    val base = (projection.baseJson \ "candidates").as[List[JsObject]]
      .find(candidate => (candidate \ "detail" \ "type").asOpt[String].contains("root_comparison")).get
    val delivered = projection.withMoveEffects(base)
    val detail = delivered \ "detail"
    val question = (detail \ "summary_question").get
    assertEquals((question \ "question").as[String], (fixture \ "baseline" \ "question").as[String])
    assertEquals((detail \ "comparison_evidence_id").as[String],
      (fixture \ "baseline" \ "comparison_evidence_id").as[String])
    assertEquals((detail \ "reference_line_id").as[String], (fixture \ "baseline" \ "reference_line_id").as[String])
    assertEquals((detail \ "candidate_line_id").as[String], (fixture \ "baseline" \ "candidate_line_id").as[String])
    val core = units(question, "units")
    val secondary = units(question, "detail_units")
    assertEquals(refs(core, "covered_moves"), expected("core_covered"))
    assertEquals(refs(core, "required_moves"), expected("core_required"))
    assertEquals(refs(core, "proof_moves"), expected("core_proof"))
    assertEquals(refs(core, "focus"), expected("core_focus"))
    assertEquals(refs(secondary, "covered_moves"), expected("detail_covered"))
    assertEquals(refs(secondary, "required_moves"), expected("detail_required"))
    assertEquals(refs(secondary, "proof_moves"), expected("detail_proof"))
    core.foreach { unit =>
      val owned = (unit \ "proof_moves").as[List[JsObject]].toSet
      assert((unit \ "required_moves").asOpt[List[JsObject]].getOrElse(Nil).forall(owned),
        "requirements migrate only to a delivery unit that owns their reference")
      (unit \ "focus").asOpt[List[JsObject]].getOrElse(Nil).foreach { focus =>
        assert(owned(Json.obj("line_id" -> (focus \ "line_id").as[String],
          "occurrence_id" -> (focus \ "occurrence_id").as[String])))
      }
    }
    val covered = core.flatMap(unit => (unit \ "covered_moves").as[List[JsObject]])
    assertEquals(covered.distinct.size, covered.size)
    val text = (detail \ "summary").as[String]
    val contrast = (fixture \ "baseline" \ "exact_root_contrast").as[String]
    assertEquals((fixture \ "baseline" \ "text").as[String].sliding(contrast.length).count(_ == contrast), 3)
    assertEquals(text.sliding(contrast.length).count(_ == contrast), 1,
      "identical selected control is displayed once; the rest of the answer is still complete")
    assertEquals(CommentarySelection.primaryComparisonRecords(source).head._2, comparison)

  test("delivery deduplication cannot erase an original global requirement with a duplicate-only container"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val currentPair = pair(source, paths)
    val left = MoveRef(currentPair.head, 0)
    val right = MoveRef(currentPair.last, 0)
    val target = Piece("white", "pawn", "e4").witness
    val contrast = ControlContrast(left, Piece("white", "knight", "c4").witness,
      Some(target), "e4", false, right, Piece("white", "knight", "d2").witness,
      Some(target), "e4", true)
    val answer = Answer("selected_comparison", List(Section(List(contrast)),
      Section(List(contrast))), Nil, Nil)
    assert(RootQuestionSummary.realize(paths, currentPair, answer).nonEmpty)
    val missing = MoveRef(currentPair.head, currentPair.head.moves.size - 1)
    assert(!contrast.references.contains(missing))
    val unclosed = answer.copy(core = List(answer.core.head,
      answer.core.last.copy(required = List(missing))))
    assertEquals(RootQuestionSummary.realize(paths, currentPair, unclosed), None,
      "a required quiet reply remains missing even when its duplicate display container disappears")
