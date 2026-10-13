import { Chess } from 'chessops/chess';
import { INITIAL_FEN, makeFen, parseFen } from 'chessops/fen';
import { PgnParser, Node, ChildNode, makePgn, type Game, type PgnNodeData } from 'chessops/pgn';
import { makeSan, parseSan } from 'chessops/san';
import { makeUci, parseUci } from 'chessops/util';
import { standardUci } from './review/chess';
import { moveReviewSubjectFromNodeList } from './review/moveReview';
import type { Tree } from './review/types';

export interface GameNode extends Tree.Node {
  path: string;
  data?: PgnNodeData;
  children: GameNode[];
}

export class GameRecord {
  readonly root: GameNode;
  readonly nodes = new Map<string, GameNode>();
  readonly recordedMoves: string[];

  constructor(private readonly pgn: Game<PgnNodeData>) {
    const variant = pgn.headers.get('Variant');
    if (variant && !['Standard', 'From Position', 'Chess'].includes(variant))
      throw Error('일반 체스 기보만 지원합니다.');
    const pos = position(pgn.headers.get('FEN') ?? INITIAL_FEN);
    this.root = { id: '', path: '', ply: (pos.fullmoves - 1) * 2 + Number(pos.turn === 'black'), fen: makeFen(pos.toSetup()), children: [] };
    this.nodes.set('', this.root);
    const visit = (source: Node<PgnNodeData>, parent: GameNode): void => {
      for (const child of source.children) {
        const before = position(parent.fen);
        const move = parseSan(before, child.data.san);
        if (!move) throw Error(`불법 수가 있습니다: ${Math.floor(parent.ply / 2) + 1}. ${child.data.san}`);
        const next = this.append(parent, makeUci(move), child.data);
        visit(child, next);
      }
    };
    visit(pgn.moves, this.root);
    this.recordedMoves = this.mainline().map(node => node.uci!);
  }

  static parse(input: string): GameRecord {
    const text = input.trim();
    if (!text || text.length > 1_000_000) throw Error('PGN 또는 FEN을 입력하세요. 최대 1MB입니다.');
    if (/^[prnbqkPRNBQK1-8/]+\s+[wb]\s/.test(text)) {
      position(text); // Validate before replacing the workspace.
      return new GameRecord({ headers: new Map([['SetUp', '1'], ['FEN', text], ['Result', '*']]), moves: new Node() });
    }
    const games: Game<PgnNodeData>[] = [];
    new PgnParser((game, error) => { if (error) throw error; games.push(game); }).parse(text);
    if (games.length !== 1) throw Error('기보 한 경기씩 불러오세요.');
    if (!games[0].moves.children.length && !games[0].headers.has('FEN')) throw Error('읽을 수 있는 수가 없습니다. PGN을 확인하세요.');
    return new GameRecord(games[0]);
  }

  static empty(): GameRecord { return GameRecord.parse(INITIAL_FEN); }

  append(parent: GameNode, uci: string, data?: PgnNodeData): GameNode {
    const pos = position(parent.fen), move = parseUci(uci);
    if (!move || !pos.isLegal(move)) throw Error('둘 수 없는 수입니다.');
    const standard = standardUci(parent.fen, uci);
    const existing = parent.children.find(child => child.uci === standard);
    if (existing) return existing;
    if (this.nodes.size > 600) throw Error('한 기보의 수와 변형은 합쳐 600수까지 지원합니다.');
    const san = makeSan(pos, move);
    pos.play(move);
    const id = parent.children.length.toString(36).padStart(2, '0');
    const child: GameNode = { id, path: parent.path + id, ply: parent.ply + 1,
      fen: makeFen(pos.toSetup()), uci: standard, san, data: { ...data, san }, children: [] };
    parent.children.push(child);
    this.nodes.set(child.path, child);
    return child;
  }

  ancestors(path: string): GameNode[] {
    const result = [this.root];
    for (let length = 2; length <= path.length; length += 2) {
      const node = this.nodes.get(path.slice(0, length));
      if (!node) throw Error('기보 위치를 찾을 수 없습니다.');
      result.push(node);
    }
    return result;
  }

  mainline(): GameNode[] {
    const result: GameNode[] = [];
    for (let node = this.root.children[0]; node; node = node.children[0]) result.push(node);
    return result;
  }

  subject(path: string) {
    return moveReviewSubjectFromNodeList('standard', path, this.ancestors(path), this.recordedMoves, 'analysis');
  }

  export(): string {
    const convert = (node: GameNode): Node<PgnNodeData> => {
      const target = node.data ? new ChildNode(node.data) : new Node<PgnNodeData>();
      target.children = node.children.map(child => convert(child) as ChildNode<PgnNodeData>);
      return target;
    };
    return makePgn({ ...this.pgn, moves: convert(this.root) });
  }

  title(): string {
    return [this.pgn.headers.get('White'), this.pgn.headers.get('Black')].filter(name => name && name !== '?').join(' – ');
  }
}

export function position(fen: string): Chess {
  return Chess.fromSetup(parseFen(fen).unwrap()).unwrap();
}
