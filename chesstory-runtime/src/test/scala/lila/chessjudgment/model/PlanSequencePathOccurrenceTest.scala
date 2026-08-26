package lila.chessjudgment.model

import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind

class PlanSequencePathOccurrenceTest extends munit.FunSuite:

  private val repeatedEvent = PlanEventIdentity.from(
    rootMove = "g1f3",
    kind = PlanKind.OpeningDevelopment,
    actorRole = Some("knight"),
    targets = List("f3"),
    results = List("development")
  )

  private def occurrence(
      ply: Int,
      fenBefore: String,
      fenAfter: String
  ): PlanEventOccurrence =
    PlanEventOccurrence.from(
      event = repeatedEvent,
      moveUci = repeatedEvent.rootMove,
      ply = ply,
      fenBefore = fenBefore,
      fenAfter = fenAfter
    )

  test("the same plan event at different board occurrences remains distinct"):
    val first = occurrence(
      ply = 1,
      fenBefore = "8/8/8/8/8/8/8/6N1 w - - 0 1",
      fenAfter = "8/8/8/8/8/5N2/8/8 b - - 1 1"
    )
    val later = occurrence(
      ply = 5,
      fenBefore = "8/8/8/8/8/8/8/6N1 w - - 4 3",
      fenAfter = "8/8/8/8/8/5N2/8/8 b - - 5 3"
    )

    assertEquals(first.event, later.event)
    assertNotEquals(first.stableKey, later.stableKey)
    assertNotEquals(
      PlanSequencePathOccurrence.from(List(first)).stableKey,
      PlanSequencePathOccurrence.from(List(later)).stableKey
    )
    assertEquals(
      PlanSequencePathOccurrence.from(List(first, later)).events,
      List(first, later)
    )

  test("path identity preserves exact FEN state and event order"):
    val first = occurrence(
      ply = 1,
      fenBefore = "8/8/8/8/8/8/8/6N1 w - - 0 1",
      fenAfter = "8/8/8/8/8/5N2/8/8 b - - 1 1"
    )
    val differentClock = occurrence(
      ply = 1,
      fenBefore = "8/8/8/8/8/8/8/6N1 w - - 8 5",
      fenAfter = "8/8/8/8/8/5N2/8/8 b - - 9 5"
    )
    assertNotEquals(first.stableKey, differentClock.stableKey)

    val later = occurrence(
      ply = 5,
      fenBefore = "8/8/8/8/8/8/8/6N1 w - - 4 3",
      fenAfter = "8/8/8/8/8/5N2/8/8 b - - 5 3"
    )
    intercept[IllegalArgumentException] {
      PlanSequencePathOccurrence.from(List(later, first))
    }
    intercept[IllegalArgumentException] {
      PlanSequencePathOccurrence.from(List(first, first))
    }
