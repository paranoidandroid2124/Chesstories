// Shared board, engine and commentary types for the standalone app.
export type { Color, Key } from '@lichess-org/chessground/types';
export type FEN = string;
export type Uci = string;
export type San = string;
export type VariantKey = string;

export namespace Tree {
  export type Path = string;
  export interface EvalScore { cp?: number; mate?: number }
  export interface PvData extends EvalScore {
    moves: string[];
    depth?: number;
    bound?: 'lowerbound' | 'upperbound';
  }
  export interface LocalEval extends EvalScore {
    fen: FEN;
    depth: number;
    seldepth?: number;
    nodes: number;
    millis: number;
    pvs: PvData[];
    bestmove?: Uci;
    ponder?: Uci;
    cloud?: false;
  }
  export interface NodeBase {
    id: string;
    ply: number;
    fen: FEN;
    uci?: Uci;
    san?: San;
  }
  export interface Node extends NodeBase { children: Node[] }
}
