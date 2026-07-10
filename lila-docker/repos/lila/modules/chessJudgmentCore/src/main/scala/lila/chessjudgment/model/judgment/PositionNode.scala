package lila.chessjudgment.model.judgment

import chess.Color

case class PositionNodeRef(
    fen: String,
    ply: Int,
    sideToMove: Option[Color] = None,
    id: Option[String] = None
)

case class PositionNode(
    role: PositionNodeRole,
    ref: PositionNodeRef
)
