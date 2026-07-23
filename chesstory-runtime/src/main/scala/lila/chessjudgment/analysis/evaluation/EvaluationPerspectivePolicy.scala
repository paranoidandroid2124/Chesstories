package lila.chessjudgment.analysis.evaluation

import chess.Color
import lila.chessjudgment.analysis.singlePosition.PvLine
import lila.chessjudgment.model.strategic.VariationLine

object EvaluationPerspectivePolicy:

  def sideToMoveScoreCp(sideToMove: Color, whitePovCp: Int): Int =
    if sideToMove.white then whitePovCp else -whitePovCp

  def sideToMoveMate(sideToMove: Color, whitePovMate: Option[Int]): Option[Int] =
    if sideToMove.white then whitePovMate else whitePovMate.map(-_)

  def sideToMovePvLines(sideToMove: Color, lines: List[VariationLine]): List[PvLine] =
    lines.map { line =>
      PvLine(
        moves = line.moves,
        sideRelativeEvalCp = sideToMoveScoreCp(sideToMove, line.scoreCp),
        mate = sideToMoveMate(sideToMove, line.mate),
        depth = line.depth
      )
    }
