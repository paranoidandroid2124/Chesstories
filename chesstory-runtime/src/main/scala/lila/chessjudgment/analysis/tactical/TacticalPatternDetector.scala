package lila.chessjudgment.analysis.tactical

import chess.Position

private[chessjudgment] trait TacticalPatternDetector:
  def id: String
  def requiresMate: Boolean
  def matches(before: Option[Position], after: Position, lastUci: String): Boolean
  def matchesWithContinuations(
      before: Option[Position],
      after: Position,
      lastUci: String,
      _continuations: List[List[String]]
  ): Boolean =
    val _ = _continuations
    matches(before, after, lastUci)
