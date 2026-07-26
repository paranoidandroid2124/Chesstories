export interface Hovering {
  fen: FEN;
  uci: Uci;
}

export interface ExplorerOpts {
  endpoint: string;
  tablebaseEndpoint: string;
  showRatings: boolean;
}

export interface ExplorerData {
  fen: FEN;
  moves: MoveStats[];
  isOpening?: true;
  tablebase?: true;
}

export interface OpeningData extends ExplorerData {
  white: number;
  black: number;
  draws: number;
  moves: OpeningMoveStats[];
  opening?: Opening;
}

export interface Opening {
  eco: string;
  name: string;
}

export type TablebaseCategory =
  | 'loss'
  | 'unknown'
  | 'syzygy-loss'
  | 'maybe-loss'
  | 'blessed-loss'
  | 'draw'
  | 'cursed-win'
  | 'maybe-win'
  | 'syzygy-win'
  | 'win';

export interface TablebaseData extends ExplorerData {
  moves: TablebaseMoveStats[];
  category: TablebaseCategory;
  dtm?: number;
  checkmate?: boolean;
  stalemate?: boolean;
}

export interface MoveStats {
  uci: Uci;
  san: San;
}

export interface OpeningMoveStats extends MoveStats {
  white: number;
  black: number;
  draws: number;
  averageRating?: number;
  averageOpponentRating?: number;
  performance?: number;
  opening?: Opening;
}
export interface TablebaseMoveStats extends MoveStats {
  dtz?: number;
  dtm?: number;
  dtw?: number;
  dtc?: number;
  checkmate?: boolean;
  stalemate?: boolean;
  insufficient_material?: boolean;
  zeroing?: boolean;
  category: TablebaseCategory;
}

export function isOpening(m: ExplorerData): m is OpeningData {
  return !!m.isOpening;
}
export function isTablebase(m: ExplorerData): m is TablebaseData {
  return !!m.tablebase;
}
