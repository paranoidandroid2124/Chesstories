package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, CanonicalPositionHistory, EngineLine }

class OccurrenceCommentaryTest extends munit.FunSuite:
  private val initial = chess.variant.Standard.initialFen.value
  private val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get

  test("first development retains its central control without an assessment or a full-line record"):
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(initial, "g1f3",
      List(EngineLine(List("g1f3", "g8f6"), 20, None, 16), EngineLine(List("e2e4"), 30, None, 16)))).get
    val ctx = MoveReviewAssembler.assemble(input)
    val removed = Set(EvidenceLayer.Line, EvidenceLayer.Eval, EvidenceLayer.RelativeAssessment,
      EvidenceLayer.CandidateComparison)
    val lower = ctx.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(ctx.evidenceGraph.records.filterNot(
      record => removed(record.ref.layer))))
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(lower).get
    assertEquals(packet.primary, None)
    val development = packet.observations.filter(o => o.transition.moveUci == "g1f3" && o.meaning.isInstanceOf[MoveObservationMeaning.Development])
    assertEquals(development.map(_.meaning), List(MoveObservationMeaning.Development(
      List(EvidenceSquare("d4"), EvidenceSquare("e5")))))
    assertEquals(development, OccurrenceCommentary.fromAssembly(ctx).filter(o => o.transition.moveUci == "g1f3" && o.meaning.isInstanceOf[MoveObservationMeaning.Development]))

    val withoutControl = lower.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(lower.evidenceGraph.records.filterNot(
      record => development.head.premises.tail.contains(record.ref))))
    assertEquals(OccurrenceCommentary.fromAssembly(withoutControl).filter(o => o.transition.moveUci == "g1f3" && o.meaning.isInstanceOf[MoveObservationMeaning.Development]).map(_.meaning),
      List(MoveObservationMeaning.Development(Nil)))

  test("a returned knight and a custom initial position do not certify first development"):
    val returned = history.extend(List("g1f3", "g8f6", "f3g1", "f6g8")).toOption.get
    val custom = CanonicalPositionHistory.from(returned.currentFen, Nil, returned.currentFen).toOption.get
    for source <- List(returned, custom) do
      val input = MoveReviewInputAdmission.admitIndependent(source, "g1f3").get
      assert(!OccurrenceCommentary.fromAssembly(MoveReviewAssembler.assemble(input)).exists(_.meaning.isInstanceOf[MoveObservationMeaning.Development]))

  test("adding a branch reuses retained canonical prefix calculations without merging later histories"):
    val before = MoveReviewAssembler.assemble(MoveReviewInputAdmission.admitIndependent(history, "g1f3").get)
    val expandedInput = MoveReviewInputAdmission.admitIndependent(history, "g1f3",
      List(List("g1f3", "g8f6", "d2d4"), List("g1f3", "d7d5", "d2d4"))).get
    val after = MoveReviewAssembler.assemble(expandedInput, Some(before))
    val id = before.playedTransition.get.evidence.id
    val oldTransition = before.transitionReplays(id).singleStepReplays.head._1
    val reused = after.transitionReplays(id).singleStepReplays.head._1
    assert(oldTransition eq reused)
    assertEquals(after.transitions.size, 5)
    assertEquals(after.transitions.filter(_.moveUci == "d2d4").map(_.to).distinct.size, 2)
    val cold = MoveReviewAssembler.assemble(after.input)
    assertEquals(after.positions, cold.positions)
    assertEquals(after.transitions, cold.transitions)
    assertEquals(after.lineTransitionIds, cold.lineTransitionIds)
    before.evidenceGraph.relationGraph.nodes.foreach { node =>
      assert(after.evidenceGraph.relationGraph.byEvidenceId(node.ref.id) eq node)
    }
    before.evidenceGraph.records.foreach { record =>
      assert(after.evidenceGraph.byId(record.ref.id) eq record)
    }
    assertEquals(OccurrenceCommentary.fromAssembly(after), OccurrenceCommentary.fromAssembly(cold))
    assertEquals(OccurrenceCommentary.fromAssembly(after).find(_.transition.evidence.id == id),
      OccurrenceCommentary.fromAssembly(before).headOption)
    val complete = OccurrenceCommentary.fromAssembly(after)
    val laterIds = after.transitions.filterNot(_.evidence.id == id).map(_.evidence.id).toSet
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(after).get
    assertEquals(packet.observationsFor(laterIds), complete.filter(o => laterIds(o.transition.evidence.id)))
    assertEquals(packet.observations, complete, "reading later occurrences first must not omit or reorder the root")

  test("re-ranking unchanged paths reuses physical evidence while replacing all old assessment records"):
    val paths = List(List("g1f3", "g8f6", "d2d4"), List("d2d4", "d7d5", "c2c4"))
    def input(scores: List[Int], depth: Int) =
      val roots = paths.zip(scores).map { (moves, score) =>
        history.admitToFirstAutomaticTerminal(moves).toOption.get
          .bind(CandidateLineEvaluation.EngineSearch(EngineLine(moves, score, None, depth))).get
      }
      MoveReviewInputAdmission.prepareFocusedMoveReview(history, "g1f3",
        history.extend(List("g1f3")).toOption.get.currentFen,
        lila.chessjudgment.model.line.PositionRuleAssessment.assess(history), roots, Nil).get match
          case PreparedFocusedPositionCommentary.MoveReview(review) => review.preparedInput
          case _ => fail("expected a move review")
    val before = MoveReviewAssembler.assemble(input(List(20, 40), 8))
    val after = MoveReviewAssembler.assemble(input(List(80, -50), 16), Some(before))
    val cold = MoveReviewAssembler.assemble(after.input)
    val assessmentLayers = Set(EvidenceLayer.Eval, EvidenceLayer.CandidateComparison, EvidenceLayer.RelativeAssessment)
    assert(after.evidenceGraph.relationGraph eq before.evidenceGraph.relationGraph)
    assertEquals(after.positions, cold.positions)
    assertEquals(after.transitions, cold.transitions)
    assertEquals(after.lines, cold.lines)
    assertEquals(after.lineTransitionIds, cold.lineTransitionIds)
    val oldAssessments = before.evidenceGraph.records.filter(r => assessmentLayers(r.ref.layer))
    assert(oldAssessments.nonEmpty)
    assert(oldAssessments.forall(r => !after.evidenceGraph.byId.get(r.ref.id).exists(_ eq r)))
    assert(oldAssessments.filter(_.ref.layer == EvidenceLayer.Eval)
      .forall(r => !after.evidenceGraph.byId.contains(r.ref.id)))
    before.evidenceGraph.records.filterNot(r => assessmentLayers(r.ref.layer)).foreach { record =>
      assert(after.evidenceGraph.byId(record.ref.id) eq record)
    }
    assertEquals(OccurrenceCommentary.fromAssembly(after), OccurrenceCommentary.fromAssembly(cold))
    assertEquals(EvidenceBackedJudgmentPacket.fromAssembly(after).get.primary,
      EvidenceBackedJudgmentPacket.fromAssembly(cold).get.primary)
    assert(after.evidenceGraph.records.forall(after.evidenceGraph.proofEligible))

    val independent = MoveReviewAssembler.assemble(
      MoveReviewInputAdmission.admitIndependent(history, "g1f3").get, Some(after))
    assert(independent.lines.isEmpty)
    assert(!independent.evidenceGraph.records.exists(r => assessmentLayers(r.ref.layer)))
    assertEquals(EvidenceBackedJudgmentPacket.fromAssembly(independent).get.primary, None)
    assertEquals(OccurrenceCommentary.fromAssembly(independent), OccurrenceCommentary.fromAssembly(after))

  test("retention rejects another history even when its board and played move agree"):
    val a = history.extend(List("g1f3", "g8f6", "f3g1", "f6g8")).toOption.get
    val b = history.extend(List("b1c3", "b8c6", "c3b1", "c6b8")).toOption.get
    assertEquals(a.currentFen, b.currentFen)
    val retained = MoveReviewAssembler.assemble(MoveReviewInputAdmission.admitIndependent(a, "e2e4").get)
    intercept[IllegalArgumentException] {
      MoveReviewAssembler.assemble(MoveReviewInputAdmission.admitIndependent(b, "e2e4").get, Some(retained))
    }
