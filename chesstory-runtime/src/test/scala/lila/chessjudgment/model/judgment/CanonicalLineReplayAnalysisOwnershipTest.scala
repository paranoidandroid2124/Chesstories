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
    assertEquals(firstBefore.occurrence.plyCount, first.ply - 1)
    assertEquals(firstAfter.occurrence.plyCount, first.ply)
    assertEquals(secondAfter.occurrence.plyCount, second.ply)
