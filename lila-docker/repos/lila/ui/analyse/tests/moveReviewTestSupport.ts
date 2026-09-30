import { moveReviewEngineProfile, type MoveReviewEngineProfile } from 'lib/ceval/types';
import {
  decodeMoveReviewSnapshot,
  type MoveReviewJobState,
  type MoveReviewSubject,
  type PositionResources,
} from '../src/moveReview';

export interface CertifiedObservationFixture {
  name: string;
  fen: FEN;
  move: Uci;
  response: Record<string, unknown>;
}

// Link the Bf3 producer's existing queen-ray evidence without generating chess facts in the test.
export function linkedHighlightFixture(original: CertifiedObservationFixture) {
  const fixture = structuredClone(original);
  const observations = fixture.response.observations as {
    paths: { occurrence_ids: string[]; position_ids: string[] }[];
    statements: { occurrence_id: string; premise_ids: string[] }[];
    positions: PositionResources[];
    highlights: unknown[];
  };
  const path = observations.paths[0];
  const highlight = {
    occurrence_id: path.occurrence_ids[0],
    text: 'The bishop move opens the queen’s diagonal to h2.',
    squares: ['c2', 'g2', 'h2'] as Key[],
    premise_ids: [...observations.statements[0].premise_ids],
    frames: (['before', 'after'] as const).map((phase, index) => {
      const position = observations.positions.find(p => p.id === path.position_ids[index])!;
      const ray = position.rays.lines.find(r => r.piece.square === 'c2' && r.squares.includes('h2'))!;
      return {
        phase,
        position_id: position.id,
        squares: (phase === 'before' ? ['c2', 'g2', 'h2'] : ['c2', 'h2']) as Key[],
        source_ids: [ray.source_id],
      };
    }),
  };
  observations.highlights = [highlight];
  return { fixture, highlight };
}

// Shared by the producer-fixture tests and the preview of the actual product view.
export function observationFixtureJob(fixture: CertifiedObservationFixture): MoveReviewJobState {
  const focus = fixture.response.focus as { resulting_fen: FEN };
  const paths = (
    fixture.response.observations as {
      paths: {
        kind: string;
        start_fen: FEN;
        moves: Uci[];
        sources?: ('pgn_record' | 'analysis' | 'study')[];
      }[];
    }
  ).paths;
  const inputLines = paths.flatMap(
    path => path.sources?.map(source => ({ source, moves: path.moves })) ?? [],
  );
  const history = paths.find(path => path.kind === 'history');
  const issued = fixture.response.issued_engine_work as
    | {
        purpose: string;
        engine_position_initial_fen: FEN;
        engine_position_moves_uci: Uci[];
        search_anchor?: { root_move: Uci; prefix_moves_uci?: Uci[] };
      }
    | undefined;
  let issuedPrefix = issued?.engine_position_moves_uci;
  if (issued?.purpose === 'response_comparison') {
    const prefix = issued.search_anchor?.prefix_moves_uci ?? [issued.search_anchor?.root_move];
    if (
      !issued.search_anchor ||
      !prefix.length ||
      prefix[0] !== issued.search_anchor.root_move ||
      !issuedPrefix ||
      issuedPrefix.length < prefix.length ||
      !prefix.every((move, index) => issuedPrefix![issuedPrefix!.length - prefix.length + index] === move)
    )
      throw new Error(`Invalid child-search history in fixture: ${fixture.name}`);
    issuedPrefix = issuedPrefix.slice(0, -prefix.length);
  }
  const subject: MoveReviewSubject = {
    variant: 'standard',
    initialFen: issued?.engine_position_initial_fen ?? history?.start_fen ?? fixture.fen,
    movePrefixUci: issuedPrefix ?? history?.moves.slice(0, -1) ?? [],
    before: { path: '', fen: fixture.fen },
    played: { uci: fixture.move, san: fixture.move as San },
    after: { path: 'aa', fen: focus.resulting_fen },
    ...(inputLines.length ? { inputLines } : {}),
  };
  const snapshot = decodeMoveReviewSnapshot(fixture.response, {
    requestId: fixture.response.request_id as string,
    subject,
  });
  if (!snapshot) throw new Error(`Rejected runtime observation fixture: ${fixture.name}`);
  if (snapshot.kind === 'completed') return { kind: 'completed', snapshot };
  if (snapshot.kind === 'position-action') return { kind: 'position-action', snapshot };
  if (snapshot.kind === 'abstained')
    return {
      kind: 'abstained',
      subject,
      stopCondition: snapshot.stopCondition,
      observations: snapshot.observations,
    };
  return {
    kind: 'loading',
    subject,
    observations: snapshot.observations,
    ...(snapshot.kind === 'awaiting-evidence' ? { assessment: snapshot.assessment } : {}),
    assessmentState: snapshot.assessmentState,
    assessmentScope: snapshot.assessmentScope,
  };
}

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
  phase: 'root_search' | 'focus_comparison' | 'investigation_comparison' | 'completed' | 'stopped',
  works = phase === 'root_search' ? 1 : 2,
  reports = phase === 'completed' || phase === 'stopped' ? works : works - 1,
): Record<string, unknown> {
  return {
    phase,
    legal_move_count: 20,
    root_candidate_lines_admitted: phase === 'root_search' ? 0 : 3,
    selected_commentaries_completed: phase === 'completed' || phase === 'investigation_comparison' ? 1 : 0,
    physical_works_issued: works,
    physical_reports_accepted: reports,
  };
}

export function rawIssuedWork(
  purpose: 'root_search' | 'focus_comparison' | 'investigation_comparison' = 'root_search',
  engineProfile: MoveReviewEngineProfile = moveReviewEngineProfile,
): Record<string, unknown> {
  const focus = purpose !== 'root_search';
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

export function rawInvestigation(workNumber = 2, needsRefresh = false): Record<string, unknown> {
  return {
    ...rawSnapshot('awaiting_evidence'),
    progress: rawProgress('investigation_comparison', workNumber + 1, workNumber),
    issued_engine_work: {
      ...rawIssuedWork('investigation_comparison'),
      work_id: `work:${workNumber}`,
      execution_key_sha256: String(workNumber + 1).repeat(64),
      root_restriction: {
        kind: 'restricted',
        moves_uci: workNumber === 2 ? ['c7c5', 'g8f6'] : ['e7e5', 'g8f6'],
      },
    },
    assessment: rawResponse().result,
    assessment_state: needsRefresh ? 'needs_refresh' : 'current',
    assessment_scope: 'root_search',
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
