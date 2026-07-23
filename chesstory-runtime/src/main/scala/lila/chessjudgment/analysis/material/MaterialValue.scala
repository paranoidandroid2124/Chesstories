package lila.chessjudgment.analysis.material

import chess.{ Bishop, Board, Color, King, Knight, Pawn, Queen, Role, Rook }

object MaterialValue:

  def materialValueCp(role: Role): Int = role match
    case Pawn   => 100
    case Knight => 300
    case Bishop => 300
    case Rook   => 500
    case Queen  => 900
    case King   => 0

  def materialValueUnit(role: Role): Int =
    materialValueCp(role) / 100

  def tacticalValueCp(role: Role): Int = role match
    case Pawn   => 100
    case Knight => 320
    case Bishop => 330
    case Rook   => 500
    case Queen  => 900
    case King   => 10000

  def tacticalValueUnit(role: Role): Int =
    if role == King then 100 else tacticalValueCp(role) / 100

  def sideMaterialCp(board: Board, side: Color): Int =
    board.byPiece(side, Pawn).count * materialValueCp(Pawn) +
      board.byPiece(side, Knight).count * materialValueCp(Knight) +
      board.byPiece(side, Bishop).count * materialValueCp(Bishop) +
      board.byPiece(side, Rook).count * materialValueCp(Rook) +
      board.byPiece(side, Queen).count * materialValueCp(Queen)

  def materialBalanceCp(board: Board): Int =
    sideMaterialCp(board, Color.White) - sideMaterialCp(board, Color.Black)
