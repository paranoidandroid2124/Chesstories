import { parsePgn, parseVariant, startingPosition } from 'chessops/pgn';
import { parseSan } from 'chessops/san';

/** The public import surface accepts one replayable game, bounded like the server. */
export function reviewPgnError(text: string): string | undefined {
  if (!text.trim()) return 'Paste a game before opening the board.';
  if (text.trim().length > 200000) return 'PGN must be 200,000 characters or fewer.';
  try {
    const game = parsePgn(text)[0];
    if (!game?.moves.children.length) return 'No moves found. Use the position field to open a FEN.';
    const variant = parseVariant(game.headers.get('Variant'));
    if (variant !== 'chess') return 'Use a standard chess or Chess960 PGN.';
    const start = startingPosition(game.headers).unwrap();
    const pending = game.moves.children.map(node => ({ node, position: start.clone() }));
    let count = 0;
    while (pending.length) {
      if (++count > 600) return 'PGN can contain up to 600 moves, including variations.';
      const { node, position } = pending.pop()!;
      const move = parseSan(position, node.data.san);
      if (!move) return 'Check the moves and starting position in this PGN.';
      position.play(move);
      for (const child of node.children) pending.push({ node: child, position: position.clone() });
    }
  } catch {
    return 'Check the moves and starting position in this PGN.';
  }
  return undefined;
}
