import { castlingSide } from 'chessops/chess';
import { parseFen } from 'chessops/fen';
import { kingCastlesTo, makeUci, parseUci } from 'chessops/util';
import { setupPosition } from 'chessops/variant';

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

export const plyColor = (ply: number): Color => (ply % 2 === 0 ? 'white' : 'black');

const ensureChildren = (node: Tree.NodeOptionalChildren): Tree.Node => {
  node.children ||= [];
  return node as Tree.Node;
};

export function treeReconstruct(parts: Tree.NodeOptionalChildren[], sidelines?: Tree.Node[][]): Tree.Node {
  const root = ensureChildren(parts[0]);
  root.id = '';
  let node = root;
  for (let i = 1; i < parts.length; i++) {
    const n = ensureChildren(parts[i]);
    const variations = sidelines ? sidelines[i] : [];
    node.children.unshift(n, ...variations);
    node = n;
  }
  return root;
}
