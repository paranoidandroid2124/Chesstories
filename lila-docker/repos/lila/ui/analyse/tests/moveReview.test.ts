import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { moveReviewEngineProfile } from 'lib/ceval/types';
import {
  buildMoveReviewEngineWorkReport,
  buildMoveReviewJobRequest,
  decodeMoveReviewSnapshot,
  formatMoveReviewPercent,
  formatMoveReviewPercentagePointChange,
  moveReviewEngineOutcomeAtRequiredDepth,
  moveReviewProofById,
  moveReviewSubjectFromNodeList,
  normalizeMoveReviewLocale,
  selectedMoveReviewCandidate,
  type IssuedMoveReviewEngineWork,
  type MoveReviewSubject,
} from '../src/moveReview';
import {
  afterFen,
  beforeFen,
  decodeContext,
  initialFen,
  rawResponse,
  rawSnapshot,
  requestId,
  subject,
} from './moveReviewTestSupport';

type JsonObject = Record<string, unknown>;
const object = (value: unknown): JsonObject => value as JsonObject;

function typedSubject(before: FEN, played: Uci, after: FEN): MoveReviewSubject {
  return {
    variant: 'standard',
    initialFen: before,
    movePrefixUci: [],
    before: { path: '' as Tree.Path, fen: before },
    played: { uci: played, san: played as San },
    after: { path: 'aa' as Tree.Path, fen: after },
  };
}

test('emits the sole v6 standard played-focus request', () => {
  assert.deepEqual(buildMoveReviewJobRequest(requestId, subject, moveReviewEngineProfile), {
    schema_version: 'chesstory.position-commentary.job-request.v6',
    request_id: requestId,
    variant: 'standard',
    initial_fen: initialFen,
    move_prefix_uci: ['e2e4'],
    current_fen: beforeFen,
    focus: { kind: 'played_move', played_move_uci: 'e7e5', resulting_fen: afterFen },
    engine_profile: moveReviewEngineProfile,
  });
});

test('decodes the root and focused comparison work', () => {
  const root = decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext());
  assert.equal(root?.kind, 'awaiting-core');
  if (root?.kind !== 'awaiting-core') return;
  assert.equal(root.issuedEngineWork.purpose, 'root_search');
  assert.equal(root.issuedEngineWork.searchLimits.multiPv, 3);
  assert.deepEqual(root.issuedEngineWork.rootRestriction, { kind: 'unrestricted' });

  const focus = decodeMoveReviewSnapshot(rawSnapshot('awaiting_evidence'), decodeContext());
  assert.equal(focus?.kind, 'awaiting-evidence');
  if (focus?.kind !== 'awaiting-evidence') return;
  assert.equal(focus.issuedEngineWork.purpose, 'focus_comparison');
  assert.deepEqual(focus.issuedEngineWork.rootRestriction, {
    kind: 'restricted',
    movesUci: ['c7c5', 'e7e5'],
  });

  const stopped = rawSnapshot('stopped');
  stopped.stop_condition = 'search_budget_exhausted';
  object(stopped.progress).legal_move_count = 0;
  assert.equal(decodeMoveReviewSnapshot(stopped, decodeContext())?.kind, 'abstained');

  const causalWaves = structuredClone(stopped);
  object(causalWaves.progress).causal_waves = 0;
  assert.equal(decodeMoveReviewSnapshot(causalWaves, decodeContext()), undefined);

  const inventedStopCondition = structuredClone(stopped);
  inventedStopCondition.stop_condition = 'no_legal_moves';
  assert.equal(decodeMoveReviewSnapshot(inventedStopCondition, decodeContext()), undefined);
});

test('fails closed on unknown schemas, Chess960, execution drift, and illegal engine history', () => {
  const unsupported = rawSnapshot('awaiting_core');
  unsupported.schema_version = 'unsupported.position-commentary.job-status';
  assert.equal(decodeMoveReviewSnapshot(unsupported, decodeContext()), undefined);

  const chess960 = rawSnapshot('awaiting_core');
  chess960.variant = 'chess960';
  assert.equal(decodeMoveReviewSnapshot(chess960, decodeContext()), undefined);

  const drift = rawSnapshot('awaiting_core');
  object(drift.issued_engine_work).execution_key_sha256 = 'not-a-key';
  assert.equal(decodeMoveReviewSnapshot(drift, decodeContext()), undefined);

  const illegal = rawSnapshot('awaiting_evidence');
  object(illegal.issued_engine_work).engine_position_moves_uci = ['e2e5'];
  assert.equal(decodeMoveReviewSnapshot(illegal, decodeContext()), undefined);
});

test('projects an exact primary and its comparison proof', () => {
  const decoded = decodeMoveReviewSnapshot(rawResponse(), decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.uci, 'e7e5');
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  assert.equal(played.review.core.bestUci, 'c7c5');
  assert.equal(played.review.core.verdictSymbol, '?!');
  assert.equal(played.review.comparisonProof?.moves[0]?.uci, 'e7e5');
  assert.equal(
    moveReviewProofById(played.review, played.review.comparisonProof!.id),
    played.review.comparisonProof,
  );
});

test('rejects the retired occurrence-relations commentary field', () => {
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  const commentary = object(selected.find(review => object(review).commentary)?.commentary);
  commentary.occurrence_relations = [];
  assert.equal(decodeMoveReviewSnapshot(raw, decodeContext()), undefined);
});

test('preserves BestChoice and its transmitted runner-up comparison without synthesizing improves', () => {
  const raw = JSON.parse(
    readFileSync(
      new URL(
        '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/best-choice-produced.json',
        import.meta.url,
      ),
      'utf8',
    ),
  ) as JsonObject;
  const producedAfter = object(raw.focus).resulting_fen as FEN;
  const decoded = decodeMoveReviewSnapshot(raw, {
    requestId: 'request-best-choice',
    subject: typedSubject(initialFen, 'e2e4', producedAfter),
    engineProfile: moveReviewEngineProfile,
  });
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (played?.review.kind !== 'move-verdict') return;
  const core = played.review.core;
  assert.equal(core.kind, 'best-choice');
  if (core.kind !== 'best-choice') return;
  assert.equal(core.verdictSymbol, 'none');
  assert.equal(core.verdictCode, 'playable_loss');
  assert.deepEqual(core.bestChoice, {
    runnerUpVerdictCode: 'playable_loss',
    runnerUpUci: 'd2d4',
  });
  assert.equal(core.bestUci, 'e2e4');

  for (const mutate of [
    (primary: JsonObject) => (primary.runner_up_verdict_code = 'improves_on_reference'),
  ]) {
    const malformed = structuredClone(raw);
    const malformedResult = object(malformed.result);
    const malformedSelected = malformedResult.selected_move_reviews as JsonObject[];
    mutate(object(object(malformedSelected[0]!.commentary).primary));
    assert.equal(decodeMoveReviewSnapshot(malformed, decodeContext()), undefined);
  }
});

test('preserves forced single moves and explicit position actions', () => {
  const forced = rawResponse({
    progress: {
      phase: 'completed',
      legal_move_count: 1,
      root_candidate_lines_admitted: 1,
      selected_commentaries_completed: 0,
      physical_works_issued: 1,
      physical_reports_accepted: 1,
    },
    result: {
      kind: 'forced_single_move',
      move_uci: 'e7e5',
      supporting_endpoint: {
        kind: 'engine_search',
        moves: ['e7e5', 'g1f3'],
        win_percent_for_mover: 50,
        depth: 16,
      },
    },
  });
  const forcedDecoded = decodeMoveReviewSnapshot(forced, decodeContext());
  assert.equal(forcedDecoded?.kind, 'completed');
  if (forcedDecoded?.kind === 'completed')
    assert.equal(forcedDecoded.evidence.candidates[0]?.review.kind, 'forced-single-move');

  const terminal = rawResponse({
    progress: {
      phase: 'completed',
      legal_move_count: 1,
      root_candidate_lines_admitted: 0,
      selected_commentaries_completed: 0,
      physical_works_issued: 0,
      physical_reports_accepted: 0,
    },
    result: { kind: 'automatic_terminal', terminal: { kind: 'stalemate' } },
  });
  assert.deepEqual(
    (decodeMoveReviewSnapshot(terminal, decodeContext()) as { action?: unknown } | undefined)?.action,
    { kind: 'automatic-terminal', terminal: { kind: 'stalemate' } },
  );

  const draw = rawResponse({
    result: {
      kind: 'draw_claim_action',
      claims: [{ rule: 'threefold_repetition', availability: 'available_now' }],
    },
  });
  assert.equal(decodeMoveReviewSnapshot(draw, decodeContext())?.kind, 'position-action');
});

test('emits only the v6 work identity and admitted line suffixes in reports', () => {
  const decoded = decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext());
  assert.equal(decoded?.kind, 'awaiting-core');
  if (decoded?.kind !== 'awaiting-core') return;
  const work = decoded.issuedEngineWork;
  const report = buildMoveReviewEngineWorkReport(work, {
    kind: 'completed',
    completedDepth: 16,
    selectiveDepth: 20,
    nodes: 100_000,
    engineTimeMs: 500,
    executorElapsedMs: 550,
    bestmoveUci: 'c7c5',
    lineSuffixes: [
      { moves: ['c7c5'], depth: 16, whiteScore: { kind: 'cp', value: 20 } },
      { moves: ['e7e5'], depth: 16, whiteScore: { kind: 'cp', value: 10 } },
      { moves: ['g8f6'], depth: 16, whiteScore: { kind: 'cp', value: 5 } },
    ],
  });
  assert.deepEqual(Object.keys(report), [
    'schema_version',
    'engine_profile',
    'work_id',
    'execution_key_sha256',
    'outcome',
  ]);
  assert.deepEqual(Object.keys(object(report.outcome)), ['kind', 'line_suffixes']);
});

test('requires exact D15 and D16 browser bundles before reporting D16', () => {
  const decoded = decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext());
  assert.equal(decoded?.kind, 'awaiting-core');
  if (decoded?.kind !== 'awaiting-core') return;
  const work = decoded.issuedEngineWork;
  const pvs = [
    { moves: ['c7c5'], cp: 20 },
    { moves: ['e7e5'], cp: 10 },
    { moves: ['g8f6'], cp: 5 },
  ];
  const previous = {
    fen: beforeFen,
    depth: 15,
    nodes: 50_000,
    millis: 250,
    pvs,
  } as Tree.LocalEval;
  const current = {
    fen: beforeFen,
    depth: 16,
    nodes: 100_000,
    millis: 500,
    bestmove: 'c7c5',
    pvs,
  } as Tree.LocalEval;
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(work, current, undefined), undefined);
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(work, current, previous)?.kind, 'completed');
});

test('supports standard subjects only and keeps locale in frontend formatting', () => {
  const nodes = [
    { fen: initialFen },
    { id: 'aa', fen: beforeFen, uci: 'e2e4', san: 'e4' },
  ] as Tree.NodeBase[];
  assert.ok(moveReviewSubjectFromNodeList('standard', 'aa', nodes));
  assert.equal(moveReviewSubjectFromNodeList('chess960', 'aa', nodes), undefined);
  assert.equal(normalizeMoveReviewLocale('ko-KR'), 'ko-KR');
  assert.equal(formatMoveReviewPercent(54.25, 'en-US'), '54.3%');
  assert.equal(formatMoveReviewPercentagePointChange(-8, 'en-US'), '-8.0%p');
});

test('executor failure report does not echo metrics or diagnostics to the server', () => {
  const work = (
    decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext()) as {
      issuedEngineWork: IssuedMoveReviewEngineWork;
    }
  ).issuedEngineWork;
  const report = buildMoveReviewEngineWorkReport(work, {
    kind: 'executor_failed',
    executorElapsedMs: 10,
    observedNodes: 1,
    engineTimeMs: 5,
    failureCode: 'search_budget_exhausted',
    diagnostic: 'local detail',
  });
  assert.deepEqual(report.outcome, { kind: 'executor_failed', failure_code: 'search_budget_exhausted' });
});
