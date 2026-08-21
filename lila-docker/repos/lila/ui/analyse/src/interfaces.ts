import type { Player, Status } from 'lib/game';

import type { ExplorerOpts } from './explorer/interfaces';

import type { Coords, MoveEvent } from 'lib/prefs';
import type { MoveReviewMode } from './moveReview';

// similar, but not identical, to game/GameData
export interface AnalyseData {
  game: Game;
  player: Player;
  opponent: Player;
  orientation: Color;
  analysis?: Analysis;
  userAnalysis: boolean;
  sidelines?: Tree.Node[][];
  treeParts: Tree.NodeOptionalChildren[];
  pref: AnalysePref;
}

export interface AnalysePref {
  coords: Coords;
  rookCastle?: boolean;
  destination?: boolean;
  highlight?: boolean;
  showCaptured?: boolean;
  animationDuration?: number;
  moveEvent: MoveEvent;
}

export interface StudyChapterSummary {
  id: string;
  url?: string;
}

export interface StudyView {
  id: string;
  chapterId: string;
  name: string;
  chapterName: string;
  canWrite: boolean;
  chapters: StudyChapterSummary[];
  url?: string;
  visibility?: string;
}

// similar, but not identical, to game/Game
export interface Game {
  id: string;
  status: Status;
  turns: number;
  fen: FEN;
  startedAtTurn?: number;
  source?: string;
  variant: Variant;
  winner?: Color;
  moveCentis?: number[];
  initialFen?: string;
  opening?: Opening;
  threefold?: boolean;
}

export interface Opening {
  name: string;
  eco: string;
}

export interface Analysis {
  id: string;
  white: AnalysisSide;
  black: AnalysisSide;
  partial?: boolean;
}

export interface AnalysisSide {
  acpl: number;
  inaccuracy: number;
  mistake: number;
  blunder: number;
  accuracy: number;
}

export interface MoveReviewConfig {
  mode: MoveReviewMode;
}

export interface AnalyseOpts {
  element: HTMLElement;
  data: AnalyseData;
  userId?: string;
  hunter: boolean;
  explorer: ExplorerOpts;
  study?: StudyView;
  moveReview?: MoveReviewConfig;
  inlinePgn?: string;
  embed?: boolean;
}

export interface JustCaptured extends Piece {
  promoted?: boolean;
}

export type Conceal = false | 'conceal' | 'hide' | null;
export type ConcealOf = (isMainline: boolean) => (path: Tree.Path, node: Tree.Node) => Conceal;
