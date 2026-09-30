package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.MoveReviewInputAdmission
import lila.chessjudgment.model.judgment.EvaluationSource
import lila.chessjudgment.model.line.CanonicalPositionHistory

class ConditionalLineRetentionTest extends munit.FunSuite:
  private val fen = chess.variant.Standard.initialFen.value
  private val initial = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get

  test("child legal continuations reuse exact ancestor steps without importing evaluations"):
    val root = initial.extend(List("e2e4", "e7e5")).toOption.get
    val child = root.extend(List("g1f3")).toOption.get
    val suffix = child.admitToFirstAutomaticTerminal(List("b8c6", "f1b5")).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(root, "g1f3").get
    val retained = MoveReviewInputAdmission.retainLegalContinuations(input, List(suffix, suffix))
    assertEquals(suffix.stepsAfter(root).get.map(_.uci), List("g1f3", "b8c6", "f1b5"))
    assertEquals(retained.legalLines.map(_.moves).count(_ == List("g1f3", "b8c6", "f1b5")), 1)
    assertEquals(retained.evaluationObservations, input.evaluationObservations)
    assertEquals(suffix.stepsAfter(child).get.map(_.uci), List("b8c6", "f1b5"))

  test("equal FEN with another move order is not the ancestor of a child continuation"):
    val root = initial.extend(List("g1f3", "g8f6", "b1c3", "b8c6")).toOption.get
    val transposed = initial.extend(List("b1c3", "b8c6", "g1f3", "g8f6")).toOption.get
    assertEquals(root.currentFen, transposed.currentFen)
    val suffix = root.extend(List("d2d3")).toOption.get.admitToFirstAutomaticTerminal(List("d7d6")).toOption.get
    assertEquals(suffix.stepsAfter(transposed), None)
    val input = MoveReviewInputAdmission.admitIndependent(transposed, "d2d3").get
    assertEquals(MoveReviewInputAdmission.retainLegalContinuations(input, List(suffix)), input)

  test("response source cannot enter the root evaluation admission path"):
    val input = MoveReviewInputAdmission.admitIndependent(initial, "g1f3").get
    intercept[IllegalArgumentException] {
      MoveReviewInputAdmission.retainAdmittedLines(input, Nil, EvaluationSource.ResponseComparison)
    }
