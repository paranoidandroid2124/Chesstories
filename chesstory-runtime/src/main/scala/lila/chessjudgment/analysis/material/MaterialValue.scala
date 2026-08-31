package lila.chessjudgment.analysis.material

import chess.{ Bishop, King, Knight, Pawn, Queen, Role, Rook }

object MaterialValue:

  def materialValueCp(role: Role): Int = role match
    case Pawn   => 100
    case Knight => 300
    case Bishop => 300
    case Rook   => 500
    case Queen  => 900
    case King   => 0
