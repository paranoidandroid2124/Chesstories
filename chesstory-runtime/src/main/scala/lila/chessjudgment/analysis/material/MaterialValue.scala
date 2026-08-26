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

  def sideMaterialCp(board: Board, side: Color): Int =
    board.byPiece(side, Pawn).count * materialValueCp(Pawn) +
      board.byPiece(side, Knight).count * materialValueCp(Knight) +
      board.byPiece(side, Bishop).count * materialValueCp(Bishop) +
      board.byPiece(side, Rook).count * materialValueCp(Rook) +
      board.byPiece(side, Queen).count * materialValueCp(Queen)

  def materialBalanceCp(board: Board): Int =
    sideMaterialCp(board, Color.White) - sideMaterialCp(board, Color.Black)
