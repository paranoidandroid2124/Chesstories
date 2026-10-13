import type { Uci, Tree } from '../review/types';
export const moveReviewEngineProfile = 'sf18-smallnet-t2-h16-v1' as const;
export type MoveReviewEngineProfile = typeof moveReviewEngineProfile;
export type MoveReviewVariant = 'standard';

export function isMoveReviewEngineProfile(value: unknown): value is MoveReviewEngineProfile {
  return value === moveReviewEngineProfile;
}
export interface MoveReviewWorkInput {
  variant: MoveReviewVariant;
  path: string;
  multiPv: number;
  ply: number;
  initialFen: string;
  currentFen: string;
  moves: string[];
  searchLimits: { depth: number; nodes: number; movetimeMs: number };
  rootMoves: Uci[];
  observe?: (nodes: number, engineTimeMs: number) => void;
  emit: (evaluation: Tree.LocalEval) => void;
}

export interface Work extends MoveReviewWorkInput {
  readonly profile: MoveReviewEngineProfile;
  readonly threads: 2;
  readonly hashSize: 16;
  stopRequested: boolean;
}

export function makeMoveReviewWork(
  profile: MoveReviewEngineProfile,
  input: MoveReviewWorkInput,
): Work {
  return {
    ...input,
    threads: 2,
    hashSize: 16,
    stopRequested: false,
    profile,
  };
}


