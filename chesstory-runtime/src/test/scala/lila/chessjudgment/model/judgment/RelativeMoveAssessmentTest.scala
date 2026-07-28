package lila.chessjudgment.model.judgment

class RelativeMoveAssessmentTest extends munit.FunSuite:

  test("actionable loss verdict is a closed model-level classification"):
    val expected = Map(
      MoveChoiceVerdict.ImprovesOnReference -> false,
      MoveChoiceVerdict.MatchesReference -> false,
      MoveChoiceVerdict.PlayableLoss -> false,
      MoveChoiceVerdict.Inaccuracy -> true,
      MoveChoiceVerdict.Mistake -> true,
      MoveChoiceVerdict.Blunder -> true
    )

    assertEquals(
      MoveChoiceVerdict.values.map(verdict => verdict -> verdict.isActionableLoss).toMap,
      expected
    )
