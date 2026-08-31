package lila.chessjudgment.model.position

import chess.Color

/** Exact address metadata for one analyzed position occurrence. Board facts
  * remain owned by the closed relation inventory; this value carries no
  * strategic or positional feature labels.
  */
private[chessjudgment] final case class PositionOccurrenceState(
    fen: String,
    sideToMove: Color,
    plyCount: Int
)
