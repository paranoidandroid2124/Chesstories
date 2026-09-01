import { moveReviewEngineProfile, type MoveReviewEngineProfile } from 'lib/ceval/types';
import type { MoveReviewSubject } from '../src/moveReview';

export const initialFen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1' as FEN;
export const beforeFen = 'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1' as FEN;
export const afterFen = 'rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2' as FEN;
export const bestFen = 'rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2' as FEN;
export const continuationFen = 'rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2' as FEN;
export const requestId = 'move-review-test';
export const jobId = 'a'.repeat(32);
export const judgmentRevision = 'chesstory.position-commentary.response.v6';
export const annotationPolicyRevision = 'chesstory.verdict-threshold-policy.v2';

export const subject: MoveReviewSubject = {
  variant: 'standard',
  initialFen,
  movePrefixUci: ['e2e4'],
  before: { path: 'aa', fen: beforeFen },
  played: { uci: 'e7e5', san: 'e5' },
  after: { path: 'aabb', fen: afterFen },
};

export function decodeContext(engineProfile: MoveReviewEngineProfile = moveReviewEngineProfile) {
  return { requestId, subject, engineProfile };
}

export function rawProgress(
  phase: 'root_search' | 'focus_comparison' | 'completed' | 'stopped',
  works = phase === 'root_search' ? 1 : 2,
  reports = phase === 'completed' || phase === 'stopped' ? works : works - 1,
): Record<string, unknown> {
  return {
    phase,
    legal_move_count: 20,
    root_candidate_lines_admitted: phase === 'root_search' ? 0 : 3,
    selected_commentaries_completed: phase === 'completed' ? 1 : 0,
    physical_works_issued: works,
    physical_reports_accepted: reports,
  };
}

export function rawIssuedWork(
  purpose: 'root_search' | 'focus_comparison' = 'root_search',
  engineProfile: MoveReviewEngineProfile = moveReviewEngineProfile,
): Record<string, unknown> {
  const focus = purpose === 'focus_comparison';
  return {
    work_id: purpose === 'root_search' ? 'work:0' : 'work:1',
    purpose,
    engine_profile: engineProfile,
    execution_key_sha256: purpose === 'root_search' ? 'a'.repeat(64) : 'b'.repeat(64),
    variant: 'standard',
    engine_position_initial_fen: initialFen,
    engine_position_moves_uci: ['e2e4'],
    search_fen: beforeFen,
    root_restriction: focus ? { kind: 'restricted', moves_uci: ['c7c5', 'e7e5'] } : { kind: 'unrestricted' },
    search_limits: {
      depth: 16,
      nodes: purpose === 'root_search' ? 5_000_000 : 2_000_000,
      movetime_ms: purpose === 'root_search' ? 5_000 : 2_500,
      multi_pv: purpose === 'root_search' ? 3 : 2,
    },
    max_search_elapsed_ms: purpose === 'root_search' ? 6_000 : 3_500,
  };
}

export function rawCommentary(overrides: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    primary: {
      kind: 'move_verdict',
      comparison_evidence_id: 'comparison.played-vs-best',
      verdict_code: 'inaccuracy',
      verdict_confidence: 'engine_backed',
      mover: 'black',
      delta: { kind: 'engine_evaluation', candidate_win_percent_delta_for_mover: -8 },
      reference_endpoint: {
        kind: 'engine_search',
        moves: ['c7c5', 'g1f3'],
        win_percent_for_mover: 54,
        depth: 16,
      },
      played_endpoint: {
        kind: 'engine_search',
        moves: ['e7e5', 'g1f3'],
        win_percent_for_mover: 46,
        depth: 16,
      },
    },
    ...overrides,
  };
}

export function rawResponse(overrides: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    schema_version: judgmentRevision,
    annotation_policy_revision: annotationPolicyRevision,
    request_id: requestId,
    job_id: jobId,
    engine_profile: moveReviewEngineProfile,
    variant: 'standard',
    current_fen: beforeFen,
    focus: { kind: 'played_move', played_move_uci: 'e7e5', resulting_fen: afterFen },
    progress: rawProgress('completed', 2, 2),
    result: {
      kind: 'selected_move_choices',
      selected_move_reviews: [
        {
          legal_move_index: 0,
          move_uci: 'c7c5',
          selection: { roles: ['best'], root_rank: 1 },
          line_insight: {
            endpoint: {
              kind: 'engine_search',
              moves: ['c7c5', 'g1f3'],
              win_percent_for_mover: 54,
              depth: 16,
            },
          },
        },
        {
          legal_move_index: 1,
          move_uci: 'e7e5',
          selection: { roles: ['played'] },
          commentary: rawCommentary(),
        },
      ],
    },
    ...overrides,
  };
}

export function rawSnapshot(
  state: 'awaiting_core' | 'awaiting_evidence' | 'completed' | 'stopped',
  options: { engineProfile?: MoveReviewEngineProfile } = {},
): Record<string, unknown> {
  if (state === 'completed')
    return { ...rawResponse(), engine_profile: options.engineProfile ?? moveReviewEngineProfile };
  const purpose = state === 'awaiting_core' ? 'root_search' : 'focus_comparison';
  const base = {
    schema_version: 'chesstory.position-commentary.job-status.v6',
    request_id: requestId,
    job_id: jobId,
    engine_profile: options.engineProfile ?? moveReviewEngineProfile,
    variant: 'standard',
    deadline_epoch_ms: 9_999_999_999_999,
    focus: { kind: 'played_move', played_move_uci: 'e7e5', resulting_fen: afterFen },
  };
  if (state === 'stopped')
    return {
      ...base,
      state: 'stopped',
      progress: rawProgress('stopped', 1, 1),
      stop_condition: 'engine_execution_failed',
    };
  return {
    ...base,
    state: 'awaiting_engine_work',
    progress: rawProgress(purpose, purpose === 'root_search' ? 1 : 2),
    issued_engine_work: rawIssuedWork(purpose, options.engineProfile),
  };
}
