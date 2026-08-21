import type { Outcome } from 'chessops/types';
import type { Prop } from '../index';
import type { Feature } from '../device';
import type CevalCtrl from './ctrl';
import type { VNode } from 'snabbdom';
import {
  isMoveReviewEngineProfile,
  moveReviewEngineProfile,
  moveReviewEngineProfileSpec,
  type MoveReviewEngineProfile,
  type MoveReviewVariant,
} from './engines/moveReviewEngineProfiles';

export { isMoveReviewEngineProfile, moveReviewEngineProfile };
export type { MoveReviewEngineProfile, MoveReviewVariant };

export type WinningChances = number;
export type SearchBy = { movetime: number } | { depth: number } | { nodes: number };
export type Search = { by: SearchBy; multiPv: number; indeterminate?: boolean };
export type Millis = number;

export interface Work {
  variant: VariantKey;
  threads: number;
  hashSize: number | undefined;
  gameId: string | undefined; // send ucinewgame when changed
  stopRequested: boolean;

  path: string;
  search: SearchBy;
  multiPv: number;
  ply: number;
  threatMode: boolean;
  initialFen: string;
  currentFen: string;
  moves: string[];
  emit: (ev: Tree.LocalEval) => void;
}

export type MoveReviewWorkInput = Omit<
  Work,
  'variant' | 'threads' | 'hashSize' | 'gameId' | 'stopRequested' | 'search' | 'threatMode' | 'emit'
> & {
  variant: MoveReviewVariant;
  searchLimits: { depth: 16; nodes: 5_000_000 | 2_000_000; movetimeMs: 5_000 | 2_500 };
  rootMoves: Uci[];
  observe?: (nodes: number, engineTimeMs: number) => void;
  emit: (evaluation: Tree.LocalEval) => void;
};

type MoveReviewWork = Work & {
  readonly profile: MoveReviewEngineProfile;
  readonly searchLimits: MoveReviewWorkInput['searchLimits'];
  readonly rootMoves: Uci[];
  readonly observe?: MoveReviewWorkInput['observe'];
};

export function makeMoveReviewWork(
  profile: MoveReviewEngineProfile,
  input: MoveReviewWorkInput,
): MoveReviewWork {
  const spec = moveReviewEngineProfileSpec(profile);
  return {
    ...input,
    threads: spec.threads,
    hashSize: spec.hashSize,
    search: { depth: input.searchLimits.depth },
    threatMode: false,
    gameId: undefined,
    stopRequested: false,
    profile,
  };
}

export function isMoveReviewWork(work: Work | undefined): work is MoveReviewWork {
  return !!work && 'profile' in work && isMoveReviewEngineProfile(work.profile);
}

export interface BaseEngineInfo {
  id: string;
  name: string;
  short?: string;
  variants?: VariantKey[];
  minThreads?: number;
  maxThreads?: number;
  maxHash?: number;
  requires?: Feature[];
}

export interface BrowserEngineInfo extends BaseEngineInfo {
  tech: 'HCE' | 'NNUE';
  short: string;
  minMem?: number;
  assets: { root?: string; js?: string; wasm?: string; version?: string; nnue?: string[] };
  requires: Feature[];
  obsoletedBy?: Feature;
}

export type EngineInfo = BrowserEngineInfo;

export type EngineNotifier = (status?: {
  download?: { bytes: number; total: number };
  error?: string;
}) => void;

export enum CevalState {
  Initial,
  Loading,
  Idle,
  Computing,
  Failed,
}

export interface CevalEngine {
  getInfo(): EngineInfo;
  getState(): CevalState;
  start(work: Work): void;
  stop(): void;
  destroy(): void;
}

export interface MoveReviewEngine extends CevalEngine {
  readonly ready: Promise<boolean>;
}

export interface EvalMeta {
  path: string;
  threatMode: boolean;
}

export type Redraw = () => void;
export type Progress = (p?: { bytes: number; total: number }) => void;

export interface CustomCeval {
  search?: () => Search | Millis; // pass number as millis to cap user defined search
  pearlNode?: () => VNode | undefined;
  statusNode?: () => VNode | string | undefined;
}

export interface CevalOpts {
  variant: Variant;
  initialFen: string | undefined;
  emit: (ev: Tree.LocalEval, meta: EvalMeta) => void;
  onUciHover: (hovering: Hovering | null) => void;
  redraw: Redraw;
  onSelectEngine?: () => void;
  custom?: CustomCeval; // hides switch, threat, and go deeper buttons
}

export interface Hovering {
  fen: string;
  uci: string;
}

export interface PvBoard {
  fen: string;
  uci: string;
}

export interface Started {
  path: string;
  steps: Step[];
  gameId: string | undefined;
  threatMode: boolean;
}

export interface CevalHandler {
  ceval: CevalCtrl;
  nextNodeBest(): string | undefined;
  toggleThreatMode(v?: boolean): void;
  outcome(): Outcome | undefined;
  showEvalGauge: Prop<boolean>;
  ongoing: boolean;
  playUciList(uciList: string[]): void;
  getOrientation(): Color;
  threatMode(): boolean;
  getNode(): Tree.Node;
  clearCeval: () => void;
  startCeval: () => void;
  cevalEnabled: (enable?: boolean) => boolean | 'force';
}

export interface NodeEvals {
  client?: Tree.ClientEval;
  server?: Tree.ServerEval;
}

export interface Step {
  ply: number;
  fen: string;
  san?: string;
  uci?: string;
  threat?: Tree.ClientEval;
  ceval?: Tree.ClientEval;
}
