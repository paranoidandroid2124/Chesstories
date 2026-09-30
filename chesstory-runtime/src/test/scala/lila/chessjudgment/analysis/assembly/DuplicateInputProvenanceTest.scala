package lila.chessjudgment.analysis.assembly

import io.chesstory.runtime.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

class DuplicateInputProvenanceTest extends munit.FunSuite:
  private val initial = chess.variant.Standard.initialFen.value
  private val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
  private val brokenMoves = List("g1h3", "d7d5", "h3g5", "a1a8")
  private val repeated = SuppliedCommentaryLine(CommentaryLineSource.Study, brokenMoves)
  private val supplied = List(repeated, repeated.copy(source = CommentaryLineSource.PgnRecord), repeated,
    SuppliedCommentaryLine(CommentaryLineSource.Analysis, List("e2e4", "d7d5", "e4d5", "d8d5")))

  private def submit(state: CommentaryJobState, lines: List[List[String]]): CommentaryJobState =
    val awaiting = state.asInstanceOf[AwaitingEngineWork]
    val work = awaiting.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(awaiting,
      EngineWorkReport.Completed(work.engineProfile, work.workId, work.executionKeySha256,
        lines.zipWithIndex.map { case (moves, index) =>
          ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(30 - index * 10), work.requiredDepth)
        }), 1L).toOption.get

  test("repeated input occurrences retain their indexes through root and focus assembly without repeated calculations"):
    val root = CommentaryJobReducer.startJob(initial, Nil, initial, "g1h3",
      history.extend(List("g1h3")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, CommentaryEngineProfile.ServerAdmitted), 0L, supplied).toOption.get
    val focus = submit(root, List(List("e2e4", "e7e5", "g1f3"), List("d2d4", "d7d5"), List("b1c3", "g8f6")))
    val completed = submit(focus, List(List("e2e4", "e7e5", "g1f3"), List("g1h3", "d7d5", "h3g5")))
    assert(focus.isInstanceOf[AwaitingEngineWork])
    assert(completed.isInstanceOf[CompletedCommentaryJob])
    val states = List(root, focus, completed)
    states.foreach { state =>
      val assembly = state.observationsPacket.get.assembly
      assertEquals(assembly.input.suppliedLines.map(_.input), supplied)
      assertEquals(assembly.input.legalLines.count(_.moves == brokenMoves.take(3)), 1)
      val records = assembly.evidenceGraph.records
      assertEquals(records.map(_.ref.id).distinct.size, records.size)
      val json = PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("d" * 32, "duplicate-input-provenance", 10000L, state))
      assertEquals((json \ "observations" \ "input_issues").as[List[JsObject]],
        (0 to 2).toList.map(index => Json.obj("input_line_index" -> index,
          "accepted_ply_count" -> 3, "reason" -> "invalid_continuation")))
    }
    states.zip(states.tail).foreach { case (before, after) =>
      val previous = before.observationsPacket.get.assembly
      val current = after.observationsPacket.get.assembly
      previous.transitionReplays.foreach { case (id, replay) =>
        assert(replay.onlyTransition.get eq current.transitionReplays(id).onlyTransition.get)
      }
    }

  test("retained assembly admits an initial document once and rejects a conflicting indexed document"):
    val input = MoveReviewInputAdmission.admitIndependent(history, "g1h3", suppliedLines = supplied).get
    val withoutDocument = MoveReviewAssembler.assemble(MoveReviewInputAdmission.admitIndependent(history, "g1h3").get)
    val retained = MoveReviewAssembler.assemble(input, Some(withoutDocument))
    assertEquals(retained.input.suppliedLines.map(_.input), supplied)
    val repeatedAssembly = MoveReviewAssembler.assemble(input, Some(retained))
    assertEquals(repeatedAssembly.input.suppliedLines, retained.input.suppliedLines)
    val conflicting = MoveReviewInputAdmission.admitIndependent(history, "g1h3", suppliedLines = supplied.reverse).get
    intercept[IllegalArgumentException](MoveReviewAssembler.assemble(conflicting, Some(retained)))
    assertEquals(retained.input.suppliedLines.map(_.input), supplied)
