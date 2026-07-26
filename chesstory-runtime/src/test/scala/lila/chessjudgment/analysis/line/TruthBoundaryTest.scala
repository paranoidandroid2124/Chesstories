package lila.chessjudgment.analysis.line

import chess.variant.Standard
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.analysis.position.PositionAnalyzer
import lila.chessjudgment.model.strategic.EngineLine

class TruthBoundaryTest extends munit.FunSuite:

  test("material imbalance comes from board piece counts"):
    val balanced = PositionAnalyzer
      .extractFeatures(Standard.initialFen.value, plyCount = 0)
      .getOrElse(fail("expected initial-position features"))
    val extraWhiteKnight = PositionAnalyzer
      .extractFeatures("4k3/8/8/8/8/8/8/1N2K3 w - - 0 1", plyCount = 0)
      .getOrElse(fail("expected imbalanced-position features"))

    assertEquals(balanced.imbalance.whiteKnights, balanced.imbalance.blackKnights)
    assertEquals(extraWhiteKnight.imbalance.whiteKnights, 1)
    assertEquals(extraWhiteKnight.imbalance.blackKnights, 0)

  test("an exact named opening sequence is not itself semantic proof"):
    val afterE4 = PrincipalVariationEvidence
      .legalFenAfter(Standard.initialFen.value, "e2e4")
      .getOrElse(fail("expected 1. e4 to be legal"))
    val beforeQueenMove = PrincipalVariationEvidence
      .legalFenAfter(afterE4, "e7e5")
      .getOrElse(fail("expected 1... e5 to be legal"))
    val declaredLine = EngineLine(
      moves = List("d1h5", "b8c6", "f1c4", "g8f6", "h5f7"),
      scoreCp = 900,
      depth = 18
    )

    assertEquals(
      ForcedLineTruth.detect(beforeQueenMove, "d1h5", List(declaredLine)),
      None
    )
