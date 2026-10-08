package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** The first two tests replay unchanged actual product receipts. The last
  * test uses explicitly synthetic, separately owned f4/h4 value receipts:
  * it tests ambiguity handling, not their engine strength or the book result. */
class DirectReserveRealizationTest extends munit.FunSuite:
  import CommentaryAnswer.{Answer, DirectReserveEntry, ReserveTempo}
  import CommentaryProposal.*
  import PrincipalVariationComparison.QualityBand

  private lazy val record =
    val relative = "judgment-evaluation/fixtures/public-commentary-v6/direct-reserve-realizations-h05-produced.json.gz"
    val path = List(Path.of(relative), Path.of("..", relative)).find(Files.isRegularFile(_)).get
    val stream = new GZIPInputStream(Files.newInputStream(path))
    try Json.parse(new String(stream.readAllBytes(), UTF_8)) finally stream.close()

  private lazy val actualState =
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 180001L, request.engineProfile,
        investigateChoices = true, previewDepth = Some(8)), 1L,
      request.inputLines, request.proposals).toOption.get
    val reports = (record \ "saved_reports").as[List[JsObject]]
    assertEquals(reports.size, 8)
    reports.zipWithIndex.foreach { (saved, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"no issued work for saved receipt $index")
      val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("d" * 32, request.requestId, 180001L, state))
      assertEquals((wire \ "issued_engine_work").as[JsObject], (saved \ "issued_work").as[JsObject],
        "entire work, including keys, limits and root ownership, must match before replay")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    assert(state.isInstanceOf[CompletedCommentaryJob])
    state

  private def selectedDirect(answer: Answer): List[DirectReserveEntry] =
    answer.core.flatMap(_.statements).collect { case value: DirectReserveEntry => value }

  private def actualContext() =
    val packet = actualState.observationsPacket.get
    val paths = actualState.preparedPaths.get
    val proofs = CommentaryIdea.directReserveEntries(actualState.proposalEntries).filter(_.verified)
    assertEquals(proofs.map(_.offset).sorted, List(18, 26, 30))
    val owner = proofs.minBy(_.offset).owner.answer
    val pair = List(owner.referenceLineId, owner.alternativeLineId).map(id =>
      paths.paths.find(_.line.ref.id == id).get)
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    (packet, paths, pair, proofs, previous)

  test("three actual current realizations retain the earliest owned f4 answer and the independent tempo answer"):
    val (packet, paths, pair, proofs, previous) = actualContext()
    for proof <- proofs do
      assert(RootSubjectComparison.currentDirectReserveEntry(paths, proof))
      val individual = RootSubjectComparison.withDirectReserveEntry(packet, paths, pair, List(proof), previous)
      assertEquals(selectedDirect(individual).size, 1)
      assert(RootSubjectComparison.validDirectReserveEntry(paths, selectedDirect(individual).head))
    val tempo = RootSubjectComparison.withReserveTempo(packet, paths, pair,
      CommentaryIdea.reserveTimings(actualState.proposalEntries), previous)
    assert(tempo.core.flatMap(_.statements).exists(_.isInstanceOf[ReserveTempo]))
    val selected = RootSubjectComparison.withDirectReserveEntry(packet, paths, pair, proofs, tempo)
    assert(selected.core.flatMap(_.statements).exists(_.isInstanceOf[ReserveTempo]))
    val direct = selectedDirect(selected)
    assertEquals(direct.size, 1)
    val retained = direct.head.proof
    assertEquals(retained.offset, 18)
    assertEquals(retained.owner, proofs.minBy(_.offset).owner)
    assertEquals(retained.owner.answer.workId, "work:1")
    assertEquals(retained.source.map(_.answer.workId), Some("work:2"))
    assertEquals(retained.testAnswer.map(_.workId), Some("work:4"))
    assertEquals(retained.testAnswer.map(_.executionKeySha256),
      Some("1e0014ed20f0f311ece0cef3a2223cc3c8eaa3a005dfc54f63dfc9bdcb26d234"))
    assertEquals(retained.ancestors, Nil)
    assert(RootSubjectComparison.validDirectReserveEntry(paths, direct.head))
    assert(RootQuestionSummary.realize(paths, pair, selected).nonEmpty)
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("d" * 32, "human-evaluation-H05", 180001L, actualState))
    val roots = (wire \ "observations" \ "selection" \ "candidates").as[List[JsObject]].filter(value =>
      (value \ "selection").as[String] == "selected" &&
        (value \ "detail" \ "type").asOpt[String].contains("root_comparison"))
    val text = roots.map(value => (value \ "detail" \ "summary").as[String]).mkString(" ")
    assert(text.contains("king manoeuvre"), text)
    assert(text.contains("Kf4 remains comparable to Ke4"), text)
    assert(!text.contains("Kf4 evaluates above Ke4"), text)
    val played = (wire \ "result" \ "selected_move_reviews").as[List[JsObject]]
      .find(value => (value \ "move_uci").as[String] == "g3g4").get
    val verdict = (played \ "commentary" \ "primary").as[JsObject]
    assertEquals((verdict \ "verdict_code").as[String], "mistake")
    assertEquals((verdict \ "delta" \ "candidate_win_percent_delta_for_mover").as[Double],
      -11.830322457665872)

  test("a Close actual child grade remains comparable despite its raw CP direction"):
    val (packet, paths, pair, proofs, previous) = actualContext()
    val proof = proofs.minBy(_.offset)
    assertEquals(proof.testAnswer.map(_.evaluationRelation), Some("worse"))
    assertEquals(proof.testAnswer.flatMap(_.qualityBand), Some(QualityBand.Close))
    val selected = RootSubjectComparison.withDirectReserveEntry(packet, paths, pair, List(proof), previous)
    val text = RootQuestionSummary.realize(paths, pair, selected).get.text
    assert(text.contains("Kf4 remains comparable to Ke4"), text)
    assert(!text.contains("Kf4 evaluates above Ke4"), text)
    // This mutation is explicitly synthetic wording evidence. The actual
    // fixture and actual proof remain unchanged; no engine claim is made.
    val separated = proof.copy(testAnswer = proof.testAnswer.map(_.copy(qualityBand = Some(QualityBand.Separated))))
    val stronger = RootSubjectComparison.withDirectReserveEntry(packet, paths, pair, List(separated), previous)
    assertEquals(selectedDirect(stronger).size, 1)
    val strongerText = RootQuestionSummary.realize(paths, pair, stronger).get.text
    assert(strongerText.contains("Kf4 evaluates above Ke4"), strongerText)

  private val fen = "8/5p2/8/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35"
  private def moves(value: String) = value.split(" ").toList
  private val actual = moves("g3g4 f7f6 f3e3 b4b3 e3f3 e5d6 f3f4 d6e6")
  private val late = moves("f3e3 f7f6 e3f3 e5d6 f3e2 d6e5 e2e3 e5d6 e3d2 b4a3 " +
    "b2a3 d6e5 d2e3 f6f5 e4f5 e5f5 e3f3 f5e5 g3g4")
  private val f4Otherwise = late ++ List("e5f6", "f3e4")
  private val f4Entry = late ++ List("e5f6", "f3f4")
  private val h4Wait = late.init ++ moves("f3g2 e5f5 g2h3 f5g5 g3g4")
  private val h4Otherwise = h4Wait ++ List("g5g6", "h3g3")
  private val h4Entry = h4Wait ++ List("g5g6", "h3h4")

  test("two separately valid f4 and h4 functions keep ambiguity instead of choosing a fabricated common answer"):
    val lines = List(actual, f4Otherwise, f4Entry, h4Otherwise, h4Entry)
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(line => paths.certifiedPath(packet, line).getOrElse(
      fail("the complete legal synthetic history was not certified: " + line.mkString(" "))))
    def receipt(left: Int, right: Int, offset: Int, id: String,
        relation: String = "equal", quality: QualityBand = QualityBand.Close) =
      CommentaryInvestigation.Answer(id, id + "-key", CommentaryEngineProfile.ServerAdmitted.wireId,
        16, Some(16), Some(16), relation, found(left).line.ref.id, found(right).line.ref.id,
        Json.obj(), Json.obj(), found(left).ids(offset), found(right).ids(offset), Some(quality))
    val batch = CommentaryDiscovery.discover(packet, paths,
      List(CommentaryDiscovery.Anchor(Nil, f4Otherwise.head)),
      List(actual, f4Otherwise).map(line => CommentaryDiscovery.SourcePath(line,
        CommentaryProposal.Source("user", "synthetic-distinct-function-test"))), Nil, pvGuided = true)
    val generated = batch.additions.find(_.request.requirements.exists {
      case test: CompareMoves => test.id == "reserve_entry_value" && test.prefixMovesUci == f4Entry.init &&
        test.referenceMove == "f3f4" && test.alternativeMove == "f3e4"
      case _ => false
    }).get
    val read = CommentaryFactReader.read(packet, paths, List(generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
        answer = Some(receipt(0, 1, 0, "synthetic-common-root", "better", QualityBand.Separated)))
      case reading if reading.requirementId == "reserve_entry_value" => reading.copy(state = "answered",
        answer = Some(receipt(2, 1, 20, "synthetic-f4-value")))
      case reading => reading
    }))).head
    val f4 = CommentaryIdea.directReserveEntries(List(read)).head
    val h4Owner = f4.owner.copy(proposalId = "synthetic-owned-h4")
    val h4 = f4.copy(owner = h4Owner, branch = h4Entry, offset = 22, square = "h4", evidence = Nil,
      test = CompareMoves("reserve_entry_value", h4Entry.init, "h3h4", "h3g3"),
      testAnswer = Some(receipt(4, 3, 24, "synthetic-h4-value")),
      source = Some(CommentaryIdea.AnsweredComparison(h4Owner.proposalId,
        CompareMoves("reserve_source", late.init, "f3g2", "g3g4"),
        receipt(3, 1, 18, "synthetic-h4-source"))), sourceArm = "reference", ancestors = Nil)
    val pair = found.take(2)
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    for proof <- List(f4, h4) do
      assertEquals(RootSubjectComparison.lastPawnWaitEntry(paths,
        found(if proof.square == "f4" then 2 else 4), proof.offset,
        proof.pawn.witness, proof.king.witness, proof.guard.witness).map(_._1), Some(proof.square))
      assert(RootSubjectComparison.currentDirectReserveEntry(paths, proof))
      val individual = RootSubjectComparison.withDirectReserveEntry(packet, paths, pair, List(proof), previous)
      assertEquals(selectedDirect(individual).map(_.proof.square), List(proof.square))
      assert(RootSubjectComparison.validDirectReserveEntry(paths, selectedDirect(individual).head))
    assertEquals(f4.owner.answer, h4.owner.answer, "the ambiguity must concern the same genuine root receipt")
    assertEquals(RootSubjectComparison.withDirectReserveEntry(packet, paths, pair, List(f4, h4), previous), previous,
      "valid different functions cannot be merged merely because their original pieces agree")
