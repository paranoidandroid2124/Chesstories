import type { AnalyseData } from './interfaces';
import { makeFen } from 'chessops/fen';
import { makeSanAndPlay, parseSan } from 'chessops/san';
import { makeUci } from 'chessops';
import {
  parsePgn,
  parseVariant,
  startingPosition,
  type ChildNode,
  type PgnNodeData,
} from 'chessops/pgn';
import { IllegalSetup, type Position } from 'chessops/chess';
import { scalachessCharPair } from 'chessops/compat';
import { makeSquare } from 'chessops/util';

const pgnErrorMessages: Partial<Record<string, string>> = {
  [IllegalSetup.Empty]: 'empty board',
  [IllegalSetup.OppositeCheck]: 'king in check',
  [IllegalSetup.PawnsOnBackrank]: 'pawns on back rank',
  [IllegalSetup.Kings]: 'king(s) missing',
  [IllegalSetup.Variant]: 'invalid Variant header',
};

const readNode = (
  node: ChildNode<PgnNodeData>,
  pos: Position,
  ply: number,
  withChildren = true,
): Tree.Node => {
  const move = parseSan(pos, node.data.san);
  if (!move) throw new Error(`Can't play ${node.data.san} at move ${Math.ceil(ply / 2)}, ply ${ply}`);
  return {
    id: scalachessCharPair(move),
    ply,
    san: makeSanAndPlay(pos, move),
    fen: makeFen(pos.toSetup()),
    uci: makeUci(move),
    children: withChildren ? node.children.map(child => readNode(child, pos.clone(), ply + 1)) : [],
    check: pos.isCheck() ? makeSquare(pos.toSetup().board.kingOf(pos.turn)!) : undefined,
  };
};

export default function (pgn: string): Partial<AnalyseData> {
  const game = parsePgn(pgn)[0];
  const headers = new Map(Array.from(game.headers, ([key, value]) => [key.toLowerCase(), value]));
  const variantHeader = headers.get('variant')?.trim() ?? '';
  const rules = parseVariant(headers.get('variant'));
  if (rules && rules !== 'chess') throw new Error(IllegalSetup.Variant);
  const variant: Variant = /^(?:chess\s*960|fischer\s*random)$/i.test(variantHeader)
    ? {
        key: 'chess960',
        name: 'Chess960',
        short: '960',
      }
    : {
        key: 'standard',
        name: 'standard',
        short: 'standard',
      };
  const start = startingPosition(game.headers).unwrap();
  const fen = makeFen(start.toSetup());
  const initialPly = (start.toSetup().fullmoves - 1) * 2 + (start.turn === 'white' ? 0 : 1);
  const treeParts: Tree.Node[] = [
    {
      id: '',
      ply: initialPly,
      fen,
      children: [],
    },
  ];
  let tree = game.moves;
  const pos = start;
  const sidelines: Tree.Node[][] = [[]];
  let index = 0;
  while (tree.children.length) {
    const [mainline, ...variations] = tree.children;
    const ply = initialPly + index + 1;
    sidelines.push(variations.map(variation => readNode(variation, pos.clone(), ply)));
    treeParts.push(readNode(mainline, pos, ply, false));
    tree = mainline;
    index += 1;
  }
  const openingName = headers.get('opening');
  const eco = headers.get('eco');
  return {
    game: {
      fen,
      initialFen: fen,
      id: 'synthetic',
      opening: openingName ? {
        name: openingName,
        eco: eco || '?',
      } : undefined,
      status: { id: 20, name: 'started' },
      turns: treeParts.length,
      variant,
    },
    player: { color: 'white' },
    opponent: { color: 'black' },
    treeParts,
    sidelines,
    userAnalysis: true,
  };
}

export const renderPgnError = (error: string) => `Check the PGN: ${pgnErrorMessages[error] ?? error}`;
