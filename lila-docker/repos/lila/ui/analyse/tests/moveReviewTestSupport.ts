import { moveReviewEngineProfile, type MoveReviewEngineProfile } from 'lib/ceval/types';
import type { MoveReviewCompactReceipt, MoveReviewSubject } from '../src/moveReview';

export const initialFen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1' as FEN;
export const beforeFen = 'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1' as FEN;
export const afterFen = 'rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2' as FEN;
export const bestFen = 'rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2' as FEN;
export const continuationFen =
  'rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2' as FEN;
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

type RawSnapshotState = 'awaiting_core' | 'awaiting_evidence' | 'completed';

export function compactReceipt(workId: 'work:0' | 'work:1'): MoveReviewCompactReceipt {
  const comparison = workId === 'work:1';
  return {
    kind: 'completed',
    workId,
    executionKeySha256: comparison ? 'b'.repeat(64) : 'a'.repeat(64),
    rootRestriction: comparison
      ? { kind: 'restricted', movesUci: ['c7c5', 'e7e5'] }
      : { kind: 'unrestricted' },
    searchLimits: {
      depth: 16,
      nodes: comparison ? 2_000_000 : 5_000_000,
      movetimeMs: comparison ? 2_500 : 5_000,
      multiPv: 2,
    },
    maxSearchElapsedMs: comparison ? 3_500 : 6_000,
    completedDepth: 16,
    nodes: 100_000,
    engineTimeMs: 500,
    executorElapsedMs: 550,
  };
}

export function failedCompactReceipt(workId: 'work:0' | 'work:1'): MoveReviewCompactReceipt {
  const completed = compactReceipt(workId);
  return {
    kind: 'executor_failed',
    workId: completed.workId,
    executionKeySha256: completed.executionKeySha256,
    rootRestriction: completed.rootRestriction,
    searchLimits: completed.searchLimits,
    maxSearchElapsedMs: completed.maxSearchElapsedMs,
    executorElapsedMs: 1,
    observedNodes: 0,
    engineTimeMs: 0,
    failureCode: 'fixture_failure',
    diagnostic: 'Fixture engine failure.',
  };
}

export function decodeContext(
  reportedReceipts: readonly MoveReviewCompactReceipt[] = [compactReceipt('work:0'), compactReceipt('work:1')],
  engineProfile: MoveReviewEngineProfile = moveReviewEngineProfile,
) {
  return { requestId, subject, engineProfile, generation: 0, reportedReceipts };
}

export const playedMoveBudget = {
  maximum_physical_works: 2,
  maximum_total_nodes: 7_000_000,
  maximum_configured_engine_time_ms: 7_500,
  maximum_position_wall_ms: 20_000,
  finalization_reserve_ms: 2_000,
};

export function rawProgress(
  phase: 'root_search' | 'evidence_acquisition' | 'completed' | 'stopped',
  receipts: readonly MoveReviewCompactReceipt[],
  pending?: ReturnType<typeof rawIssuedWork>,
): Record<string, unknown> {
  const selectedCandidateCount = phase === 'completed' && receipts.length > 0 ? 2 : 0;
  return {
    phase,
    legal_move_count: 20,
    root_candidate_lines_admitted: receipts.length ? 1 : 0,
    selected_candidate_count: selectedCandidateCount,
    selected_commentaries_completed: selectedCandidateCount,
    selected_commentaries_abstained: 0,
    focus_coverage: receipts.length ? 'supplement' : 'not_requested',
    physical_works_issued: receipts.length + (pending ? 1 : 0),
    physical_reports_accepted: receipts.length,
    accepted_nodes: receipts.reduce(
      (sum, receipt) => sum + (receipt.kind === 'completed' ? receipt.nodes : 0),
      0,
    ),
    configured_engine_time_ms:
      receipts.reduce((sum, receipt) => sum + receipt.searchLimits.movetimeMs, 0) +
      (pending ? Number((pending.search_limits as Record<string, unknown>).movetime_ms) : 0),
  };
}

export function rawMetrics(receipts: readonly MoveReviewCompactReceipt[]): Record<string, unknown> {
  const engineTime = receipts.reduce((sum, receipt) => sum + receipt.engineTimeMs, 0);
  const executorTime = receipts.reduce((sum, receipt) => sum + receipt.executorElapsedMs, 0);
  const attemptedNodes = receipts.reduce(
    (sum, receipt) => sum + (receipt.kind === 'completed' ? receipt.nodes : receipt.observedNodes),
    0,
  );
  const completedNodes = receipts.reduce(
    (sum, receipt) => sum + (receipt.kind === 'completed' ? receipt.nodes : 0),
    0,
  );
  return {
    position_wall_ms: Math.max(1, engineTime, executorTime),
    queue_elapsed_ms: 0,
    report_reduction_elapsed_ms: 0,
    assembly_elapsed_ms: 0,
    engine_reported_time_ms: engineTime,
    executor_search_elapsed_ms: executorTime,
    attempted_nodes: attemptedNodes,
    total_nodes: completedNodes,
  };
}

export function rawCompactReceipt(receipt: MoveReviewCompactReceipt): Record<string, unknown> {
  const common = {
    work_id: receipt.workId,
    execution_key_sha256: receipt.executionKeySha256,
    root_restriction:
      receipt.rootRestriction.kind === 'unrestricted'
        ? { kind: 'unrestricted' }
        : { kind: 'restricted', moves_uci: receipt.rootRestriction.movesUci },
    search_limits: {
      depth: receipt.searchLimits.depth,
      nodes: receipt.searchLimits.nodes,
      movetime_ms: receipt.searchLimits.movetimeMs,
      multi_pv: receipt.searchLimits.multiPv,
    },
    max_search_elapsed_ms: receipt.maxSearchElapsedMs,
  };
  return receipt.kind === 'completed'
    ? {
        kind: receipt.kind,
        ...common,
        completed_depth: receipt.completedDepth,
        nodes: receipt.nodes,
        engine_time_ms: receipt.engineTimeMs,
        executor_elapsed_ms: receipt.executorElapsedMs,
      }
    : {
        kind: receipt.kind,
        ...common,
        executor_elapsed_ms: receipt.executorElapsedMs,
        observed_nodes: receipt.observedNodes,
        engine_time_ms: receipt.engineTimeMs,
        failure_code: receipt.failureCode,
        diagnostic: receipt.diagnostic,
      };
}

export function rawIssuedWork(
  workId: 'work:0' | 'work:1' = 'work:0',
  engineProfile: MoveReviewEngineProfile = moveReviewEngineProfile,
): Record<string, unknown> {
  const comparison = workId === 'work:1';
  return {
    work_id: workId,
    generation: 0,
    engine_profile: engineProfile,
    execution_key_sha256: comparison ? 'b'.repeat(64) : 'a'.repeat(64),
    variant: subject.variant,
    engine_position_initial_fen: initialFen,
    engine_position_moves_uci: ['e2e4'],
    search_fen: beforeFen,
    root_restriction: comparison
      ? { kind: 'restricted', moves_uci: ['c7c5', 'e7e5'] }
      : { kind: 'unrestricted' },
    search_limits: {
      depth: 16,
      nodes: comparison ? 2_000_000 : 5_000_000,
      movetime_ms: comparison ? 2_500 : 5_000,
      multi_pv: 2,
    },
    admission: { minimum_completed_depth: 16 },
    max_search_elapsed_ms: comparison ? 3_500 : 6_000,
  };
}

export function rawEpisode(
  role: 'reviewed' | 'reference',
  reason = false,
): Record<string, unknown> {
  const reviewed = role === 'reviewed';
  const move = reviewed ? 'e7e5' : 'c7c5';
  const fenAfter = reviewed ? afterFen : bestFen;
  const id = `line.${role}`;
  return {
    episode_id: id,
    line_evidence_id: id,
    role,
    line_authority: 'legal_replay_verified',
    root_move_uci: move,
    line_moves: [move],
    replay_steps: [
      {
        order: 0,
        ply: 1,
        move_uci: move,
        from: move.slice(0, 2),
        to: move.slice(2, 4),
        fen_before: beforeFen,
        fen_after: fenAfter,
      },
    ],
    events: [],
    material_captures: [],
    relation_facts: [],
    direct_cause_channels: [],
    consequences: [],
    causal_episodes: [],
    structural_transitions: [],
    ...(reason ? { selected_reason_order: 0 } : {}),
  };
}

export function rawCommentary(overrides: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    primary: {
      kind: 'move_verdict',
      comparison_evidence_id: 'comparison.played-vs-best',
      verdict_code: 'inaccuracy',
      verdict_symbol: '?!',
      verdict_confidence: 'engine_backed',
      mover: 'black',
      delta: { kind: 'engine_evaluation', candidate_win_percent_delta_for_mover: -8 },
      reference_endpoint: {
        kind: 'engine_search',
        moves: ['c7c5'],
        win_percent_for_mover: 54,
        depth: 16,
      },
      played_endpoint: {
        kind: 'engine_search',
        moves: ['e7e5'],
        win_percent_for_mover: 46,
        depth: 16,
      },
      primary_reason_evidence_id: 'line.reviewed',
      supporting_reason_evidence_ids: [],
      semantic_episode_ids: ['line.reviewed', 'line.reference'],
    },
    position_context: {
      board_evidence_id: 'board.before',
      authority: 'board_derived',
      fen: beforeFen,
    },
    semantic_episodes: [rawEpisode('reviewed', true), rawEpisode('reference')],
    ...overrides,
  };
}

export function rawResponse(overrides: Record<string, unknown> = {}): Record<string, unknown> {
  const receipts = [compactReceipt('work:0'), compactReceipt('work:1')];
  return {
    schema_version: judgmentRevision,
    annotation_policy_revision: annotationPolicyRevision,
    engine_profile: moveReviewEngineProfile,
    variant: subject.variant,
    request_id: requestId,
    job_id: jobId,
    generation: 0,
    current_fen: beforeFen,
    focus: { kind: 'played_move', played_move_uci: 'e7e5', resulting_fen: afterFen },
    budget: playedMoveBudget,
    progress: rawProgress('completed', receipts),
    metrics: rawMetrics(receipts),
    work_receipts: receipts.map(rawCompactReceipt),
    decision_trace: { events: [] },
    result: {
      kind: 'selected_move_choices',
      selected_move_reviews: [
        {
          legal_move_index: 0,
          move_uci: 'c7c5',
          selection: { kind: 'root_candidate', root_rank: 1 },
          commentary: {
            primary: {
              kind: 'single_candidate_insight',
              line_evidence_id: 'line.reference',
              move_uci: 'c7c5',
              line_moves: ['c7c5'],
              semantic_episode_ids: ['line.reference'],
            },
            position_context: {
              board_evidence_id: 'board.before',
              authority: 'board_derived',
              fen: beforeFen,
            },
            semantic_episodes: [{ ...rawEpisode('reference'), role: 'reviewed' }],
          },
        },
        {
          legal_move_index: 1,
          move_uci: 'e7e5',
          selection: { kind: 'played_focus', focus_coverage: 'supplement' },
          commentary: rawCommentary(),
        },
      ],
    },
    ...overrides,
  };
}

export function rawSnapshot(
  state: RawSnapshotState,
  options: { engineProfile?: MoveReviewEngineProfile } = {},
): Record<string, unknown> {
  if (state === 'completed')
    return { ...rawResponse(), engine_profile: options.engineProfile ?? moveReviewEngineProfile };
  const comparison = state === 'awaiting_evidence';
  const receipts = comparison ? [compactReceipt('work:0')] : [];
  const work = rawIssuedWork(comparison ? 'work:1' : 'work:0', options.engineProfile);
  return {
    schema_version: 'chesstory.position-commentary.job-status.v6',
    engine_profile: options.engineProfile ?? moveReviewEngineProfile,
    variant: subject.variant,
    request_id: requestId,
    job_id: jobId,
    generation: 0,
    state: 'awaiting_engine_work',
    deadline_epoch_ms: 1_900_000_000_000,
    focus: { kind: 'played_move', played_move_uci: 'e7e5', resulting_fen: afterFen },
    budget: playedMoveBudget,
    progress: rawProgress(comparison ? 'evidence_acquisition' : 'root_search', receipts, work),
    decision_trace: { events: [] },
    issued_engine_work: work,
  };
}
