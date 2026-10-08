package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Legal boards and explicitly synthetic comparison receipts test ownership,
  * refutation and delivery. Actual engine strength is checked separately. */
class ReplyConstraintTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import PrincipalVariationComparison.QualityBand

  private val fen = "rn1q1rk1/pb2bppp/1pp1pn2/3p4/2PPP3/5NP1/PPQN1PBP/R1B2RK1 b - - 0 9"
  private val lines = List("b8d7 e4e5 f6e8", "b8a6 f1d1 c6c5", "b8a6 e4e5 f6d7", "b8a6 e4e5 f6e8")
    .map(_.split(" ").toList)

  private def fixture(rootFen: String = fen, rootLines: List[List[String]] = lines) =
    val history = CanonicalPositionHistory.from(rootFen, Nil, rootFen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, rootLines.head.head,
      suppliedLines = rootLines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = rootLines.map(moves => paths.certifiedPath(packet, moves).get)
    def answer(left: Int, right: Int, offset: Int, relation: String, band: QualityBand) =
      CommentaryInvestigation.Answer("synthetic-test-" + offset, "synthetic-test-key-" + offset,
        CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), relation,
        found(left).line.ref.id, found(right).line.ref.id, Json.obj(), Json.obj(),
        found(left).ids(offset), found(right).ids(offset), Some(band))
    val root = answer(0, 1, 0, "better", QualityBand.Close)
    val child = answer(2, 3, 2, "worse", QualityBand.Separated)
    val batch = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, rootLines(1).head)),
      rootLines.take(2).map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "test-line"))), Nil,
      pvGuided = true)
    val generated = batch.additions.find(_.request.origin.question.producer == "C-reply-space").get
    val answered = generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "constraint_root" => reading.copy(state = "answered", answer = Some(root))
      case reading if reading.requirementId == "constraint_reply" => reading.copy(state = "answered", answer = Some(child))
      case reading => reading
    })
    val entry = CommentaryFactReader.read(packet, paths, List(answered)).head
    val proof = CommentaryIdea.replySpaces(List(entry)).head
    val pair = found.take(2)
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    (packet, paths, found, batch, entry, proof, previous)

  test("an observed retreat becomes a constrained-response question, not an intended preparation goal"):
    val (_, _, _, batch, entry, proof, _) = fixture()
    val context = entry.readings.find(_.requirementId == "constraint_context").get
    assertEquals(context.state, "supported")
    assertEquals(proof.square, "d7")
    assertEquals(proof.challenged, Piece("black", "knight", "f6"))
    assertEquals(proof.test.prefixMovesUci, List("b8a6", "e4e5"))
    assert(proof.verified)
    assert(!batch.additions.exists(entry => entry.request.requirements.exists {
      case compare: CompareMoves => compare.prefixMovesUci.isEmpty &&
        Set(compare.referenceMove, compare.alternativeMove) == Set("b8d7", "f6e8")
      case _ => false
    }), "the response to e5 is not the root's intended destination")

  test("the root-owned constraint and the same-position reply comparison lead the basic answer"):
    val (packet, paths, found, _, _, proof, previous) = fixture()
    val answer = RootSubjectComparison.withReplySpace(packet, paths, found.take(2), List(proof), previous)
    assertEquals(answer.question, "investigated_mechanism")
    val result = RootQuestionSummary.realize(paths, found.take(2), answer).get
    assert(result.text.startsWith("Nbd7 occupies d7"), result.text)
    assert(result.text.contains("against the same e5") && result.text.contains("Nd7 evaluates above Ne8"), result.text)
    assert(!result.text.contains("forced") && !result.text.contains("must") && !result.text.contains("blunder"), result.text)
    assertEquals((result.json \ "witness_line_ids").as[List[String]].toSet,
      found.drop(2).map(_.line.ref.id).toSet)
    assert(result.text.contains("attacks White's pawn on e5"), result.text)
    assert(result.text.contains("does not attack White's pawn on e5"), result.text)

  test("a geometrically available but weak, tied or untested retreat cannot own the basic answer"):
    val (packet, paths, found, _, _, proof, previous) = fixture()
    val unverified = List(proof.copy(testAnswer = None),
      proof.copy(testAnswer = proof.testAnswer.map(_.copy(evaluationRelation = "equal"))),
      proof.copy(testAnswer = proof.testAnswer.map(_.copy(evaluationRelation = "better"))),
      proof.copy(testAnswer = proof.testAnswer.map(_.copy(qualityBand = Some(QualityBand.Close)))))
    unverified.foreach { value =>
      assertEquals(RootSubjectComparison.withReplySpace(packet, paths, found.take(2), List(value), previous), previous)
    }

  test("the same constraint survives a color and rank reflection with different moves and target squares"):
    val mirroredFen = "r1b2rk1/ppqn1pbp/5np1/2ppp3/3P4/1PP1PN2/PB2BPPP/RN1Q1RK1 w - - 0 9"
    val mirroredLines = List("b1d2 e5e4 f3e1", "b1a3 f8d8 c3c4", "b1a3 e5e4 f3d2", "b1a3 e5e4 f3e1")
      .map(_.split(" ").toList)
    val (packet, paths, found, _, _, proof, previous) = fixture(mirroredFen, mirroredLines)
    assertEquals(proof.square, "d2")
    assertEquals(proof.challenged, Piece("white", "knight", "f3"))
    val answer = RootSubjectComparison.withReplySpace(packet, paths, found.take(2), List(proof), previous)
    assertEquals(answer.question, "investigated_mechanism")
    val result = RootQuestionSummary.realize(paths, found.take(2), answer).get
    assert(result.text.startsWith("Nbd2 occupies d2"), result.text)
    assert(result.text.contains("Nd2 evaluates above Ne1"), result.text)
    assert(result.text.contains("attacks Black's pawn on e4"), result.text)
    assert(result.text.contains("does not attack Black's pawn on e4"), result.text)

  test("a changed current reply withdraws the historical constraint even while declared facts survive"):
    val (_, paths, _, _, entry, _, _) = fixture()
    val changed = entry.copy(readings = entry.readings.map {
      case reading if reading.requirementId == "constraint_current_attack" =>
        reading.copy(state = "not_applicable", result = None)
      case reading => reading
    })
    assertEquals(CommentaryIdea.replySpaces(List(changed)), Nil)
    assert(paths.paths.nonEmpty)

  test("realization rejects a replaced physical knight or a child receipt from another occurrence"):
    val (packet, paths, found, _, _, proof, previous) = fixture()
    val answer = RootSubjectComparison.withReplySpace(packet, paths, found.take(2), List(proof), previous)
    val original = answer.core.head.statements.head.asInstanceOf[CommentaryAnswer.ReplyConstraint]
    val forged = List(original.copy(proof = proof.copy(challenged = proof.blocker)),
      original.copy(proof = proof.copy(testAnswer = proof.testAnswer.map(_.copy(referenceOccurrenceId = found.head.ids.head)))),
      original.copy(available = found.last))
    forged.foreach { value =>
      assertEquals(RootQuestionSummary.realize(paths, found.take(2),
        answer.copy(core = List(CommentaryAnswer.Section(List(value))))), None)
    }

  test("a different root pair cannot import this constraint from an equal-looking branch"):
    val (packet, paths, found, _, _, proof, previous) = fixture()
    assertEquals(RootSubjectComparison.withReplySpace(packet, paths,
      List(found.head, found.last), List(proof), previous), previous)

  test("the first root constraint is investigated before a later exchange in the other arm"):
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val profile = CommentaryEngineProfile.ServerAdmitted
    val start = CommentaryJobReducer.startJob(fen, Nil, fen, "b8d7",
      history.extend(List("b8d7")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L)
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    val rows = List(
      "c6c5 e4d5 e6d5 d4c5 e7c5 d2b3" -> 42,
      "b8a6 f1d1 c6c5" -> 43,
      "b8d7 e4e5 f6e8 c4d5 e6d5 f1e1" -> 88)
    def submit(state: AwaitingEngineWork, now: Long) = CommentaryJobReducer.submitIssuedWorkReport(state,
      EngineWorkReport.Completed(profile, state.issuedWork.workId, state.issuedWork.executionKeySha256,
        rows.map((moves, score) => ReportedEngineLineSuffix(moves.split(" ").toList,
          ReportedWhiteEngineScore.Centipawns(score), state.issuedWork.requiredDepth))), now).toOption.get
    val full = submit(start, 1L).asInstanceOf[AwaitingEngineWork]
    val next = submit(full, 2L)
    val work = next.asInstanceOf[AwaitingEngineWork].issuedWork
    assertEquals(work.searchAnchor.map(_.prefixMoves), Some(List("c6c5", "e4e5")))
    assertEquals(work.rootRestriction, EngineRootRestriction.Restricted(List("f6d7", "f6e8")))
