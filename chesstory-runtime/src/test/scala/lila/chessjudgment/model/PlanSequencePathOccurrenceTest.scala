package lila.chessjudgment.model

import lila.chessjudgment.model.judgment.{ CanonicalLineReplay, CanonicalRootLegalMove }
import lila.chessjudgment.model.line.CanonicalPositionHistory
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind

class PlanSequencePathOccurrenceTest extends munit.FunSuite:

  private val repeatedMovement: CanonicalRootLegalMove =
    val fen = "4k3/8/P7/8/8/8/8/4K3 w - - 0 1"
    val history = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .getOrElse(fail("expected a canonical plan-event root"))
    val extended = history
      .extend(List("a6a7"))
      .getOrElse(fail("expected a legal plan-event move"))
    CanonicalLineReplay
      .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .flatMap(_.onlyTransition.map(_.relationDelta.rootMove))
      .getOrElse(fail("expected the canonical legal-move relation"))

  private val repeatedEvent = PlanEventIdentity.fromCanonical(
    rootMove = "a6a7",
    kind = PlanKind.PasserConversion,
    actor = PlanActorOccurrence.certified(
      side = repeatedMovement.side,
      beforeRole = repeatedMovement.beforeRole.name,
      afterRole = repeatedMovement.afterRole.name,
      from = repeatedMovement.from.key,
      to = repeatedMovement.to.key,
      legalMoveSemanticId = repeatedMovement.fact.semanticId
    )
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
      fenBefore = "4k3/8/P7/8/8/8/8/4K3 w - - 0 1",
      fenAfter = "4k3/P7/8/8/8/8/8/4K3 b - - 0 1"
    )
    val later = occurrence(
      ply = 5,
      fenBefore = "4k3/8/P7/8/8/8/8/4K3 w - - 4 3",
      fenAfter = "4k3/P7/8/8/8/8/8/4K3 b - - 0 3"
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
      fenBefore = "4k3/8/P7/8/8/8/8/4K3 w - - 0 1",
      fenAfter = "4k3/P7/8/8/8/8/8/4K3 b - - 0 1"
    )
    val differentClock = occurrence(
      ply = 1,
      fenBefore = "4k3/8/P7/8/8/8/8/4K3 w - - 8 5",
      fenAfter = "4k3/P7/8/8/8/8/8/4K3 b - - 0 5"
    )
    assertNotEquals(first.stableKey, differentClock.stableKey)

    val later = occurrence(
      ply = 5,
      fenBefore = "4k3/8/P7/8/8/8/8/4K3 w - - 4 3",
      fenAfter = "4k3/P7/8/8/8/8/8/4K3 b - - 0 3"
    )
    intercept[IllegalArgumentException] {
      PlanSequencePathOccurrence.from(List(later, first))
    }
    intercept[IllegalArgumentException] {
      PlanSequencePathOccurrence.from(List(first, first))
    }
