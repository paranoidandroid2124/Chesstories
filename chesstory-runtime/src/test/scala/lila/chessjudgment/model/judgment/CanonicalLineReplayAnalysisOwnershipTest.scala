package lila.chessjudgment.model.judgment

import chess.variant.Standard
import lila.chessjudgment.model.line.CanonicalPositionHistory

class CanonicalLineReplayAnalysisOwnershipTest extends munit.FunSuite:

  private def twoPlyReplay: CanonicalLineReplay =
    val history = CanonicalPositionHistory
      .from(Standard.initialFen.value, Nil, Standard.initialFen.value)
      .flatMap(_.extend(List("e2e4", "e7e5")))
      .getOrElse(fail("expected a legal two-ply history"))
    CanonicalLineReplay
      .fromHistory(history.segmentReplaySteps)
      .getOrElse(fail("expected a canonical replay"))

  test("one replay shares each occurrence analysis across before and after queries"):
    val replay = twoPlyReplay
    val first :: second :: Nil = replay.replaySteps: @unchecked

    val firstBefore = replay.analysisBefore(first).getOrElse(fail("expected the root occurrence"))
    val firstAfter = replay.analysisAfter(first).getOrElse(fail("expected the first destination"))
    val secondBefore = replay.analysisBefore(second).getOrElse(fail("expected the second source"))
    val secondAfter = replay.analysisAfter(second).getOrElse(fail("expected the second destination"))

    assert(firstBefore eq replay.analysisBefore(first).getOrElse(fail("expected the cached root occurrence")))
    assert(firstAfter eq replay.analysisAfter(first).getOrElse(fail("expected the cached first destination")))
    assert(firstAfter eq secondBefore)
    assert(secondAfter eq replay.analysisAfter(second).getOrElse(fail("expected the cached second destination")))
    assertEquals(firstBefore.features.plyCount, first.ply - 1)
    assertEquals(firstAfter.features.plyCount, first.ply)
    assertEquals(secondAfter.features.plyCount, second.ply)

  test("rebasing changes only occurrence plies and shares the owned calculations"):
    val replay = twoPlyReplay
    assertEquals(replay.rebased(0), None)

    val rebased = replay.rebased(4).getOrElse(fail("expected a positive occurrence rebase"))
    val originalFirst = replay.replaySteps.head
    val rebasedFirst = rebased.replaySteps.head
    val originalBefore = replay.analysisBefore(originalFirst).getOrElse(fail("expected original source"))
    val originalAfter = replay.analysisAfter(originalFirst).getOrElse(fail("expected original destination"))
    val rebasedBefore = rebased.analysisBefore(rebasedFirst).getOrElse(fail("expected rebased source"))
    val rebasedAfter = rebased.analysisAfter(rebasedFirst).getOrElse(fail("expected rebased destination"))

    assertEquals(rebasedFirst.ply, 4)
    assertEquals(rebasedBefore.features.plyCount, 3)
    assertEquals(rebasedAfter.features.plyCount, 4)
    assert(rebasedBefore.position eq originalBefore.position)
    assert(rebasedAfter.position eq originalAfter.position)
    assert(rebasedBefore.actualLegalMoves eq originalBefore.actualLegalMoves)
    assert(rebasedAfter.boardRelations eq originalAfter.boardRelations)
