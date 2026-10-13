import type { FEN, Uci } from './types';
import { castlingSide } from 'chessops/chess';
import { parseFen } from 'chessops/fen';
import { kingCastlesTo, makeUci, parseUci, opposite } from 'chessops/util';
import { setupPosition } from 'chessops/variant';
import type { Setup } from 'chessops/setup';

/** Convert tree castling notation only when the board identifies a legal castle. */
export function standardUci(beforeFen: FEN, value: Uci): Uci {
  if (!['e1h1', 'e1a1', 'e8h8', 'e8a8'].includes(value)) return value;
  const position = setupPosition('chess', parseFen(beforeFen).unwrap()).unwrap();
  const move = parseUci(value)!;
  const side = castlingSide(position, move);
  return side && 'from' in move && position.isLegal(move)
    ? (makeUci({ ...move, to: kingCastlesTo(position.turn, side) }) as Uci)
    : value;
}

/** A hypothetical passed turn, shared by threat search and its displayed line.
 * It has no legal game history; keep the position's clock out of move grading.
 */
export function threatSetup(fen: FEN): Setup {
  const setup = parseFen(fen).unwrap();
  setup.turn = opposite(setup.turn);
  setup.epSquare = undefined;
  if (setup.turn === 'white') setup.fullmoves += 1;
  return setup;
}

