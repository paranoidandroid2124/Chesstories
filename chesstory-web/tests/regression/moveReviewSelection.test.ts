import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import {
  decodeMoveReviewSnapshot,
  moveReviewCopy,
  moveReviewObservations,
  type MoveReviewObservations,
  type MoveReviewJobState,
} from '../../src/review/moveReview';
import {
  commentarySceneFrame,
  decodeCommentaryEvaluation,
  decodeCommentarySelection,
  releaseCommentaryInvestigation,
  pvComparisonDetail,
  responsePoolMatchesClasses,
  targetResponseDetail,
  lineContinuationDetail,
  continuationComparisonDetail,
} from '../../src/review/moveReviewSelection';
import { initialFen, observationFixtureJob } from './moveReviewTestSupport';
import { createMoveReviewRuntimeSource, MoveReviewRuntimeError } from '../../src/review/moveReviewRuntimeSource';
import { MoveReviewCoordinator } from '../../src/review/moveReviewCoordinator';
import { moveReviewEngineProfile } from '../../src/engine/types';
import type { VNode } from 'snabbdom';
import type { MoveReviewPanelProps } from '../../src/review/view/moveReview';

type Wire = Record<string, any>;
const fixtureRoot = new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/',
  import.meta.url,
);

// Synthetic transport envelopes reuse safe existing observations, without claiming these scores were searched.
function progressiveEnvelopes() {
  const fixtures: Wire[] = JSON.parse(readFileSync(new URL('commentary-investigation-produced.json', fixtureRoot), 'utf8'));
  const finalTemplate = fixtures.find(row => row.response.annotation_policy_revision)!.response;
  const fixture = structuredClone(fixtures.find(row => row.name === 'awaiting-probe')!);
  const job = observationFixtureJob(fixture as any);
  assert.equal(job.kind, 'loading');
  if (job.kind !== 'loading') throw new Error('expected loading fixture');
  const context = { requestId: fixture.response.request_id, subject: job.subject };
  const pending = fixture.response;
  pending.verification = { state: 'pending', target_depth: 16, reason: 'initial_verification',
    trigger_work_ids: [], completed_depth: null, stop_reason: null };
  pending.issued_engine_work.purpose = pending.progress.phase = 'root_search';
  pending.issued_engine_work.root_restriction = { kind: 'unrestricted' };
  pending.issued_engine_work.search_limits = { depth: 16, nodes: 5_000_000, movetime_ms: 5_000, multi_pv: 3 };
  pending.issued_engine_work.max_search_elapsed_ms = 6_000;
  const first = structuredClone(pending);
  delete first.observations;
  delete first.assessment;
  delete first.assessment_state;
  delete first.assessment_scope;
  Object.assign(first.progress, { root_candidate_lines_admitted: 0, selected_commentaries_completed: 0,
    physical_works_issued: 1, physical_reports_accepted: 0 });
  first.issued_engine_work.work_id = 'work:0';
  first.issued_engine_work.execution_key_sha256 = '0'.repeat(64);
  Object.assign(first.issued_engine_work.search_limits, { depth: 8, nodes: 2_000_000, movetime_ms: 2_500 });
  first.issued_engine_work.max_search_elapsed_ms = 3_500;
  const done = { schema_version: finalTemplate.schema_version, annotation_policy_revision: finalTemplate.annotation_policy_revision,
    request_id: pending.request_id, job_id: pending.job_id, engine_profile: pending.engine_profile, variant: 'standard',
    current_fen: job.subject.before.fen, focus: pending.focus, observations: pending.observations,
    result: pending.assessment, assessment_state: 'current', assessment_scope: 'root_search',
    progress: { ...pending.progress, phase: 'completed', physical_reports_accepted: 2 },
    verification: { ...pending.verification, state: 'complete', completed_depth: 16 } };
  return { first, pending, done, context };
}

test('first assessment survives a pending root verification and terminal snapshots cannot claim pending work', () => {
  const { first, pending, done, context } = progressiveEnvelopes();
  assert.equal(decodeMoveReviewSnapshot(first, context)?.kind, 'awaiting-core');
  const snapshot = decodeMoveReviewSnapshot(pending, context);
  assert.equal(snapshot?.kind, 'awaiting-evidence');
  assert.ok(snapshot?.kind === 'awaiting-evidence' && snapshot.assessment);
  assert.equal(snapshot?.verification?.state, 'pending');
  assert.equal(decodeMoveReviewSnapshot(done, context)?.verification?.completedDepth, 16);
  for (const mutate of [
    (r: Wire) => { delete r.verification; },
    (r: Wire) => { r.verification.state = 'complete'; r.verification.completed_depth = 16; },
    (r: Wire) => { r.progress.selected_commentaries_completed = 0; },
    (r: Wire) => { r.verification.trigger_work_ids = ['work:1', 'work:1']; },
  ]) {
    const invalid = structuredClone(pending);
    mutate(invalid);
    assert.equal(decodeMoveReviewSnapshot(invalid, context), undefined);
  }
  for (const verification of [
    { ...done.verification, state: 'pending' },
    { ...done.verification, completed_depth: 8 },
    { ...done.verification, state: 'stopped', stop_reason: null },
  ]) assert.equal(decodeMoveReviewSnapshot({ ...done, verification }, context), undefined);
  const stopped = decodeMoveReviewSnapshot({ ...done,
    verification: { ...pending.verification, state: 'stopped', stop_reason: 'deadline_exceeded' } }, context);
  assert.equal(stopped?.kind, 'completed');
  assert.equal(stopped?.verification?.state, 'stopped');
});

test('the existing runtime source emits the first assessment before executing further work under the original deadline', async t => {
  const { first, pending, done, context } = progressiveEnvelopes();
  const messages = [first, pending, done];
  const events: string[] = [];
  const deadline = performance.now() + 10_000;
  const originalFetch = globalThis.fetch;
  t.after(() => { globalThis.fetch = originalFetch; });
  globalThis.fetch = async () => new Response(JSON.stringify(messages.shift()), {
    status: messages.length === 2 ? 201 : 200, headers: { 'Content-Type': 'application/json' },
  });
  const source = createMoveReviewRuntimeSource(async (work, _received, _signal, deadlineAtMs) => {
    events.push(`execute:${work.workId}`);
    assert.equal(deadlineAtMs, deadline);
    if (work.workId === 'work:1') assert.ok(events.includes('first-assessment'));
    return { kind: 'completed', completedDepth: work.searchLimits.depth, selectiveDepth: 0,
      nodes: 100, engineTimeMs: 1, executorElapsedMs: 1, bestmoveUci: 'e2e4',
      lineSuffixes: ['e2e4', 'd2d4', 'g1f3'].map(move => ({ moves: [move as Uci],
        depth: work.searchLimits.depth, whiteScore: { kind: 'cp' as const, value: 20 } })) };
  });
  await source.run({ ...context, engineProfile: pending.engine_profile, deadlineAtMs: deadline }, snapshot => {
    events.push(snapshot.kind === 'awaiting-evidence' && snapshot.assessment ? 'first-assessment' : snapshot.kind);
  }, new AbortController().signal);
  assert.deepEqual(events, ['awaiting-core', 'execute:work:0', 'first-assessment', 'execute:work:1', 'completed']);
  assert.equal(messages.length, 0);
});

test('the existing coordinator keeps the first assessment and ends pending verification on transport failure', async t => {
  const { pending, context } = progressiveEnvelopes();
  const originalWindow = Object.getOwnPropertyDescriptor(globalThis, 'window');
  let armed: (() => void) | undefined;
  Object.defineProperty(globalThis, 'window', { configurable: true, value: {
    setTimeout: (callback: () => void) => { armed = callback; return 1; }, clearTimeout: () => {},
  } });
  t.after(() => {
    if (originalWindow) Object.defineProperty(globalThis, 'window', originalWindow);
    else Reflect.deleteProperty(globalThis, 'window');
  });
  for (const code of ['deadline-exceeded', 'runtime-unavailable'] as const) {
    const states: MoveReviewJobState[] = [];
    const coordinator = new MoveReviewCoordinator('en-US', { engineProfile: pending.engine_profile,
      suspendLiveEngine: () => {}, resumeLiveEngine: () => {}, stateChanged: state => states.push(state),
      source: { run: async (request, emit) => {
        const snapshot = decodeMoveReviewSnapshot({ ...pending, request_id: request.requestId }, request)!;
        emit(snapshot);
        throw new MoveReviewRuntimeError(code, true);
      } },
    });
    try {
      coordinator.activate();
      coordinator.settle(context.subject);
      assert.ok(armed);
      armed();
      await new Promise<void>(resolve => setImmediate(resolve));
      const last = states.at(-1)!;
      assert.equal(last.kind, code === 'deadline-exceeded' ? 'abstained' : 'fault');
      assert.ok(last.assessment);
      assert.equal(last.verification?.state, 'stopped');
      assert.equal(last.verification?.stopReason, code === 'deadline-exceeded' ? 'deadline_exceeded' : 'runtime_unavailable');
    } finally { coordinator.destroy(); }
  }
});

test('runtime-produced completed comparisons pass the same decoder used by single moves and PGN review', () => {
  const fixtures = JSON.parse(
    readFileSync(new URL('commentary-selection-produced.json', fixtureRoot), 'utf8'),
  );
  assert.equal(fixtures.length, 2);
  for (const fixture of fixtures) {
    const job = observationFixtureJob(fixture);
    assert.equal(job.kind, 'completed');
    const observations = moveReviewObservations(job)!;
    const selection = observations.selection;
    assert.ok(selection, fixture.name);
    assert.equal(selection.scenes.length, 1);
    const comparison = selection.candidates.find(candidate => candidate.kind === 'comparison')!;
    assert.equal(
      comparison.detail.material_relation,
      fixture.name === 'material-counts-differ' ? 'different' : 'equal',
    );
    const frame = commentarySceneFrame(observations, selection.scenes[0])!;
    const path = observations.paths.find(path => path.id === frame.proofId)!;
    assert.equal(path.occurrenceIds[frame.ply - 1], selection.scenes[0].rootOccurrenceId);
    assert.equal(observations.owner?.jobId, fixture.response.job_id);
  }
});

test('runtime-produced investigation and refresh states retain linked inquiries', () => {
  const fixtures = JSON.parse(
    readFileSync(new URL('commentary-investigation-produced.json', fixtureRoot), 'utf8'),
  );
  assert.equal(fixtures.length, 49);
  for (const fixture of fixtures) {
    const observations = moveReviewObservations(observationFixtureJob(fixture))!;
    assert.ok(
      observations?.selection?.inquiries?.length || observations?.selection?.continuation_inquiries?.length,
      fixture.name,
    );
    for (const inquiry of observations.selection!.inquiries ?? []) {
      assert.ok(inquiry.anchor_occurrence_ids.includes(inquiry.root_occurrence_id));
      if (inquiry.answer) {
        const path = observations.paths.find(path => path.id === inquiry.answer!.alternative_line_id)!;
        assert.equal(path.occurrenceIds[0], inquiry.answer.alternative_occurrence_id);
        assert.equal(path.moves[0].uci, inquiry.alternative_move);
      }
    }
  }
});

function openingPvExample(name = 'opening-equal-alternative') {
  const fixtures = JSON.parse(
    readFileSync(new URL('commentary-investigation-produced.json', fixtureRoot), 'utf8'),
  );
  const fixture = fixtures.find((row: Wire) => row.name === name) as Wire;
  assert.ok(fixture, name);
  const response = fixture.response;
  const decode = () => moveReviewObservations(observationFixtureJob(fixture as any))!;
  return { fixture, response, decode };
}

test('verified work may deepen while the light root and focus keep their separate limits', () => {
  const fixtures: Wire[] = JSON.parse(
    readFileSync(new URL('commentary-investigation-produced.json', fixtureRoot), 'utf8'),
  );
  for (const purpose of ['investigation_comparison', 'response_comparison']) {
    const fixture = structuredClone(
      fixtures.find(row => row.response.issued_engine_work?.purpose === purpose),
    );
    assert.ok(fixture);
    const loading = observationFixtureJob(fixture as any);
    assert.ok(loading.kind === 'loading');
    const context = { requestId: fixture.response.request_id, subject: loading.subject };
    const decode = () => decodeMoveReviewSnapshot(fixture.response, context);
    for (const depth of [16, 24, 100, 15, 101, 16.5]) {
      fixture.response.issued_engine_work.search_limits.depth = depth;
      const snapshot = decode();
      if ([16, 24, 100].includes(depth)) {
        assert.ok(snapshot?.kind === 'awaiting-evidence', `${purpose} at ${depth}`);
        assert.equal(snapshot.issuedEngineWork.searchLimits.depth, depth);
      } else assert.equal(snapshot, undefined, `${purpose} at ${depth}`);
    }
    if (purpose !== 'investigation_comparison') continue;
    // Reuse only the existing position; these work envelopes are synthetic contract cases.
    delete fixture.response.assessment;
    delete fixture.response.assessment_state;
    delete fixture.response.assessment_scope;
    fixture.response.progress.selected_commentaries_completed = 0;
    const work = fixture.response.issued_engine_work;
    const alternative = work.root_restriction.moves_uci.find(
      (move: string) => move !== loading.subject.played.uci,
    );
    assert.ok(alternative);
    for (const fixedPurpose of ['root_search', 'focus_comparison']) {
      work.purpose = fixture.response.progress.phase = fixedPurpose;
      work.root_restriction =
        fixedPurpose === 'root_search'
          ? { kind: 'unrestricted' }
          : { kind: 'restricted', moves_uci: [loading.subject.played.uci, alternative] };
      work.search_limits.nodes = fixedPurpose === 'root_search' ? 5_000_000 : 2_000_000;
      work.search_limits.movetime_ms = fixedPurpose === 'root_search' ? 5_000 : 2_500;
      work.search_limits.multi_pv = fixedPurpose === 'root_search' ? 3 : 2;
      work.max_search_elapsed_ms = fixedPurpose === 'root_search' ? 6_000 : 3_500;
      work.search_limits.depth = 16;
      assert.ok(decode(), `${fixedPurpose} still accepts depth 16`);
      work.search_limits.depth = 24;
      assert.ok(decode(), `${fixedPurpose} can verify a deeper challenge`);
      work.search_limits.depth = 8;
      work.search_limits.nodes = 2_000_000;
      work.search_limits.movetime_ms = 2_500;
      work.max_search_elapsed_ms = 3_500;
      assert.ok(decode(), `${fixedPurpose} accepts the first-result limits`);
      work.search_limits.depth = 9;
      assert.equal(decode(), undefined, `${fixedPurpose} rejects an unissued depth shape`);
    }
  }
});

// A synthetic work envelope reuses exact board and occurrence references.
test('a larger verified focus allowance requires a matching exhausted focus receipt', () => {
  const { pending, context } = progressiveEnvelopes();
  const work = pending.issued_engine_work;
  const moves = [context.subject.played.uci, 'e2e4'];
  work.purpose = pending.progress.phase = 'focus_comparison';
  work.work_id = 'work:3';
  work.execution_key_sha256 = '3'.repeat(64);
  work.root_restriction = { kind: 'restricted', moves_uci: moves };
  work.search_limits.multi_pv = 2;
  pending.progress.physical_works_issued = 4;
  pending.progress.physical_reports_accepted = 3;
  const searches = pending.observations.selection.root_searches;
  const exhausted = { ...structuredClone(searches[0]), work_id: 'work:2',
    execution_key_sha256: '2'.repeat(64), purpose: 'focus_comparison', multi_pv: 2,
    root_restriction: structuredClone(work.root_restriction), state: 'stopped',
    stop_reason: 'search_budget_exhausted', candidates: [] };
  pending.observations.selection = { version: 2, candidates: [], scenes: [], root_searches: [exhausted] };
  const accepted = decodeMoveReviewSnapshot(pending, context);
  assert.equal(accepted?.kind, 'awaiting-evidence');
  assert.ok(accepted?.kind === 'awaiting-evidence' && accepted.issuedEngineWork.searchLimits.nodes === 5_000_000);
  for (const mutate of [
    (r: Wire) => { r.observations.selection.root_searches.pop(); },
    (r: Wire) => { r.observations.selection.root_searches.at(-1).stop_reason = 'cancelled'; },
    (r: Wire) => { r.observations.selection.root_searches.at(-1).required_depth = 17; },
    (r: Wire) => { r.observations.selection.root_searches.at(-1).purpose = 'investigation_comparison'; },
    (r: Wire) => { r.observations.selection.root_searches.at(-1).root_restriction.moves_uci[1] = 'd2d4'; },
    (r: Wire) => { r.issued_engine_work.search_limits.depth = 8; },
    (r: Wire) => { r.issued_engine_work.search_limits.movetime_ms = 10_000; },
  ]) {
    const invalid = structuredClone(pending);
    mutate(invalid);
    assert.equal(decodeMoveReviewSnapshot(invalid, context), undefined);
  }
});

// Synthetic proposal attribution and requirements reuse existing admitted observations.
// These checks do not claim that a generator discovered the question or executed a search.
function proposalEntry(requirements: Wire[]) {
  const source = (kind: string) => ({ kind, producer: 'transport-test', source_refs: [] });
  return {
    id: 'proposal:transport',
    admission: 'submitted',
    origin: {
      question: source('author'),
      candidates: source('user'),
      priority: source('generator'),
      evidence: source('none'),
    },
    requirements,
  };
}

function proposalReading(fields: Wire): Wire {
  return {
    stop_reason: null,
    answer: null,
    line_id: null,
    occurrence_id: null,
    position_id: null,
    work_id: null,
    ...fields,
  };
}

test('proposal groups waiting on a comparison retain the exact dependent work', () => {
  // Minimal transport regression for the live sacrifice response; no fixture or engine is loaded.
  const need = { kind: 'comparison', reason: 'comparison_not_answered', prefix_moves_uci: [],
    requirement_id: 'pair', horizon: null };
  const pending = { state: 'awaiting_engine', work_id: 'work:2',
    result: { state: 'partial', samples: [], needs: [need], proofs: [], reason: null } };
  const pair = proposalReading({ id: 'pair', kind: 'compare_moves', state: 'awaiting_engine',
    work_id: 'work:2', prefix_moves_uci: [], reference_move: 'e2e4', alternative_move: 'd2d4' });
  const fact = (id: string, ply: number) => proposalReading({ ...structuredClone(pending), id, kind: 'fact',
    path: { binding: 'returned', source: { kind: 'comparison', requirement_id: 'pair', arm: 'reference' }, extend: false },
    predicate: { kind: 'presence', subject: { side: 'white', role: 'pawn', square: 'e2' } },
    at: { kind: 'ply', ply_count: ply }, expected: 'present' });
  const requirements = [pair, fact('before', 0), fact('after', 1),
    proposalReading({ ...structuredClone(pending), id: 'order', kind: 'fact_comparison',
      left_requirement_id: 'before', right_requirement_id: 'after', relation: 'same' }),
    proposalReading({ ...structuredClone(pending), id: 'all', kind: 'fact_group', mode: 'all', members: ['order'] }),
    proposalReading({ ...structuredClone(pending), id: 'nested', kind: 'fact_group', mode: 'any', members: ['all'] })];
  const selection = { version: 1, candidates: [], scenes: [], local_expansions_used: 0,
    proposals: [proposalEntry(requirements)] };
  const observations: MoveReviewObservations = { paths: [], statements: [] };
  assert.ok(decodeCommentarySelection(selection, observations)?.proposals);
  for (const mutate of [
    (rows: Wire[]) => { rows[4].work_id = 'work:7'; },
    (rows: Wire[]) => { rows[0].work_id = 'work:7'; },
    (rows: Wire[]) => { rows[0].state = 'deferred'; rows[0].work_id = null; },
    (rows: Wire[]) => { rows[1].result.needs = []; rows[2].result.needs = []; },
    (rows: Wire[]) => {
      rows.splice(1, 0, { ...structuredClone(rows[0]), id: 'unrelated' });
      rows[2].path.source.requirement_id = rows[3].path.source.requirement_id = 'unrelated';
    },
  ]) {
    const forged = structuredClone(selection);
    mutate(forged.proposals[0]!.requirements);
    assert.equal(decodeCommentarySelection(forged, observations), undefined);
  }
});

function factProposalExample() {
  const value = example();
  const path = value.response.observations.paths.find((path: Wire) => path.moves[0] === 'e2e4');
  assert.ok(path);
  const fact = (id: string, ply: number) => {
    const sample = {
      id: `sample:${id}`,
      line_id: path.id,
      binding: 'route',
      moves_uci: ['e2e4'],
      from_ply: ply,
      through_ply: ply,
      position_ids: [path.position_ids[ply]],
      occurrence_ids: path.occurrence_ids.slice(0, 1),
      value: 'present',
      state: 'known',
      source_ids: [
        value.response.observations.positions.find((position: Wire) => position.id === path.position_ids[ply])
          .premise_id,
      ],
      reason: null,
    };
    return proposalReading({
      id,
      kind: 'fact',
      state: 'supported',
      path: { binding: 'route', source: { kind: 'declared', moves_uci: ['e2e4'] }, extend: false },
      predicate: { kind: 'presence', subject: { side: 'white', role: 'pawn', square: 'e2' } },
      at: { kind: 'ply', ply_count: ply },
      expected: 'present',
      result: {
        state: 'supported',
        samples: [sample],
        needs: [],
        reason: null,
        proofs: [{ bindings: { route: sample.moves_uci }, sample_ids: [sample.id], requirement_ids: [id] }],
      },
    });
  };
  const before = fact('before', 0),
    after = fact('after', 1);
  const comparison = proposalReading({
    id: 'order',
    kind: 'fact_comparison',
    state: 'supported',
    left_requirement_id: 'before',
    right_requirement_id: 'after',
    relation: 'before',
    result: {
      state: 'supported',
      samples: [],
      needs: [],
      reason: null,
      proofs: [
        {
          bindings: { route: ['e2e4'] },
          sample_ids: ['sample:before', 'sample:after'],
          requirement_ids: ['before', 'after', 'order'],
        },
      ],
    },
  });
  const group = proposalReading({
    id: 'both',
    kind: 'fact_group',
    state: 'supported',
    mode: 'all',
    members: ['before', 'order'],
    result: {
      ...structuredClone(comparison.result),
      proofs: [
        {
          ...structuredClone(comparison.result.proofs[0]),
          requirement_ids: ['before', 'after', 'order', 'both'],
        },
      ],
    },
  });
  value.response.observations.selection.proposals = [proposalEntry([before, after, comparison, group])];
  value.response.observations.selection.local_expansions_used = 0;
  return { ...value, path, before, after, comparison, group };
}

test('proposal facts preserve point evidence, temporal comparisons and compatible composition', () => {
  const value = factProposalExample();
  assert.deepEqual(value.decode().selection?.proposals, value.response.observations.selection.proposals);
  value.before.at = {
    kind: 'event',
    event: 'move',
    ordinal: 1,
    phase: 'before',
    from_ply: 0,
    through_ply: 1,
    actor: { side: 'white', role: 'pawn', square: 'e2' },
    square: 'e4',
  };
  const sample = value.before.result.samples[0];
  sample.moves_uci = ['e2e4'];
  sample.occurrence_ids = [value.path.occurrence_ids[0]];
  value.before.result.proofs[0].bindings.route = ['e2e4'];
  assert.ok(
    value.decode().selection,
    'before an event keeps the selecting edge while its position remains the earlier board',
  );
  value.before.at.event = 'capture';
  assert.equal(value.decode().selection, undefined, 'a destination selector belongs only to a move event');
  value.before.at.event = 'move';
  value.before.predicate = {
    kind: 'pawn_advance',
    subject: { side: 'white', role: 'pawn', square: 'e2' },
    square: 'e3',
  };
  assert.ok(value.decode().selection, 'an empty pawn passage uses the existing scoped fact contract');
  value.before.expected = 'legal';
  assert.equal(value.decode().selection, undefined, 'a pawn passage cannot claim move legality');
  value.before.expected = 'present';
  value.before.predicate.property = 'queen';
  assert.equal(value.decode().selection, undefined, 'pawn passage has no promotion or utility parameter');

  const scanned = factProposalExample();
  const selection = scanned.response.observations.selection;
  selection.proposals[0].admission = 'generated';
  assert.ok(scanned.decode().selection, 'generated requests retain the same individual proof contract');
  selection.proposals = [];
  const root = scanned.response.observations.positions.find((position: Wire) =>
    position.id === scanned.path.position_ids[0]);
  const legal = root.legal_moves.moves.map((move: Wire) => move.uci);
  const scan = {
    prefix_moves_uci: [], reference_move: 'e2e4', legal_moves: legal, examined_moves: [...legal],
    deferred_request_ids: [] as string[],
    pending_replay_prefixes: [] as string[][],
    state: 'complete', reason: null as string | null,
  };
  selection.discovery = { version: 1, input_key: 'discovery:transport', scans: [scan] };
  assert.deepEqual(scanned.decode().selection?.discovery, selection.discovery,
    'a completed scan with zero generated questions remains visible');
  scan.state = 'partial';
  scan.examined_moves = legal.slice(0, 1);
  scan.reason = 'local_expansion_limit';
  assert.deepEqual(scanned.decode().selection?.discovery, selection.discovery,
    'a partial scan retains its exact inspected moves');
  scan.state = 'complete';
  assert.equal(scanned.decode().selection, undefined, 'an incomplete scan cannot claim completion');
  scan.examined_moves = [...legal];
  scan.deferred_request_ids = ['discovery:deferred'];
  scan.reason = 'requirement_limit';
  assert.equal(scanned.decode().selection, undefined, 'a deferred question cannot disappear inside complete');
  scan.state = 'partial';
  assert.deepEqual(scanned.decode().selection?.discovery, selection.discovery);
  scan.deferred_request_ids = [];
  scan.pending_replay_prefixes = [['e2e4', 'e7e5']];
  scan.reason = 'response_probe_pending';
  assert.deepEqual(scanned.decode().selection?.discovery, selection.discovery,
    'the exact unprepared replay survives a partial scan');
  scan.state = 'complete';
  assert.equal(scanned.decode().selection, undefined, 'a pending replay prevents complete even after all legal moves were examined');
});

test('proposal compact samples preserve exact line ownership and the event selecting edge', () => {
  const value = factProposalExample();
  value.before.at = { kind: 'event', event: 'move', ordinal: 1, phase: 'before', from_ply: 0,
    through_ply: 1, actor: { side: 'white', role: 'pawn', square: 'e2' }, square: 'e4' };
  const original = value.decode();
  for (const reading of [value.before, value.after]) {
    const sample = reading.result.samples[0];
    sample.prefix_ply_count = sample.moves_uci.length;
    delete sample.moves_uci;
    delete sample.position_ids;
    delete sample.occurrence_ids;
  }
  const compact = value.decode();
  assert.ok(compact.selection);
  assert.deepEqual(compact.paths, original.paths);
  assert.deepEqual(compact.statements, original.statements);
  assert.deepEqual(compact.selection.candidates, original.selection!.candidates);
  const retained = compact.selection.proposals![0].requirements[0].result!.samples[0];
  assert.equal(retained.prefix_ply_count, 1);
  assert.equal(retained.through_ply, 0, 'the selecting edge extends beyond this earlier board');
  assert.equal('moves_uci' in retained, false, 'decoding does not recreate redundant arrays');
  for (const mutate of [
    (sample: Wire) => { sample.prefix_ply_count = 0; },
    (sample: Wire) => { sample.prefix_ply_count = value.path.moves.length + 1; },
    (sample: Wire) => { sample.prefix_ply_count = 1.5; },
    (sample: Wire) => { sample.line_id = 'foreign:line'; },
    (sample: Wire) => { sample.moves_uci = ['e2e4']; },
    (sample: Wire) => { sample.position_ids = [value.path.position_ids[0]]; },
    (sample: Wire) => { sample.occurrence_ids = [value.path.occurrence_ids[0]]; },
    (sample: Wire) => { sample.source_ids = ['foreign:source']; },
  ]) {
    const sample = value.before.result.samples[0];
    const saved = structuredClone(sample);
    mutate(sample);
    assert.equal(value.decode().selection, undefined, 'compact evidence must satisfy the same ownership checks');
    value.before.result.samples[0] = saved;
  }
});

test('proposal facts reject borrowed samples, cross-branch joins and unsupported refutations', () => {
  for (const mutate of [
    (v: ReturnType<typeof factProposalExample>) => {
      v.after.result.samples[0].position_ids = [v.path.position_ids[0]];
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.after.result.samples[0].source_ids = ['foreign:source'];
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.after.result.samples[0].occurrence_ids = [];
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.after.result.samples[0].value = 'best';
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.after.path.source.moves_uci = ['d2d4'];
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.comparison.left_requirement_id = 'both';
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.comparison.relation = 'after';
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.group.result.proofs[0].bindings.route = ['d2d4'];
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.group.result.proofs[0].requirement_ids = ['both'];
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.group.members = ['missing'];
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.before.state = v.before.result.state = 'refuted';
      v.before.result.samples = [];
      v.before.result.proofs = [];
    },
    (v: ReturnType<typeof factProposalExample>) => {
      v.before.result.samples[0].state = 'unavailable_input';
    },
  ]) {
    const value = factProposalExample();
    mutate(value);
    const decoded = value.decode();
    assert.equal(decoded.selection, undefined);
    assert.ok(decoded.paths.length, 'independent observations survive an invalid semantic view');
  }
});

test('proposal partial facts and stopped lifecycle preserve unresolved evidence without becoming false', () => {
  const value = factProposalExample();
  const reading = value.before;
  value.response.observations.selection.proposals[0].requirements = [reading];
  reading.path.extend = true;
  reading.state = 'stopped';
  reading.stop_reason = 'investigation_limit';
  reading.result = {
    state: 'partial',
    samples: [],
    proofs: [],
    reason: null,
    needs: [
      {
        kind: 'continuation',
        reason: 'path_end',
        prefix_moves_uci: ['e2e4'],
        requirement_id: null,
        horizon: 3,
      },
    ],
  };
  assert.equal(value.decode().selection?.proposals?.[0].requirements[0].result?.state, 'partial');
  reading.state = 'partial';
  reading.stop_reason = null;
  assert.ok(value.decode().selection);
  reading.result.state = 'refuted';
  reading.state = 'refuted';
  assert.equal(value.decode().selection, undefined);
});

test('proposal retirement distinguishes an absent event from unavailable applicability', () => {
  const value = factProposalExample().decode();
  const requirements = value.selection!.proposals![0].requirements;
  for (const reading of requirements.slice(0, 2)) {
    reading.state = 'not_applicable';
    reading.result = { state: 'not_applicable', samples: [], proofs: [], needs: [], reason: 'event_absent' };
  }
  requirements[1].result!.reason = 'premise_witness_unavailable';
  requirements[1].result!.needs = [{ kind: 'input', reason: 'premise_witness_unavailable',
    prefix_moves_uci: [], requirement_id: null, horizon: null }];
  releaseCommentaryInvestigation(value, 'cancelled');
  const outcome = value.selection!.questionOutcomes![0];
  assert.deepEqual(outcome.unresolved.map(reading => reading.id), ['after']);
  assert.equal(outcome.conditions[0].stop_reason, null);
  assert.equal(outcome.conditions[1].stop_reason, 'cancelled');
  assert.equal(outcome.needs[0].reason, 'premise_witness_unavailable');
  releaseCommentaryInvestigation(value, 'runtime_unavailable');
  assert.equal(value.selection!.questionOutcomes![0], outcome, 'retiring the same owner twice is harmless');
});

test('proposal facts use the named arm of one earlier engine comparison', () => {
  const value = inquiryExample(true);
  const pair = proposalReading({
    id: 'pair',
    kind: 'compare_moves',
    prefix_moves_uci: [],
    reference_move: value.inquiry.reference_move,
    alternative_move: value.inquiry.alternative_move,
    state: 'answered',
    answer: value.inquiry.answer,
    work_id: value.inquiry.answer.work_id,
  });
  const path = value.reference;
  const sample = {
    id: 'sample:pair',
    line_id: path.id,
    binding: 'route',
    moves_uci: path.moves,
    from_ply: 0,
    through_ply: 0,
    position_ids: [path.position_ids[0]],
    occurrence_ids: path.occurrence_ids,
    value: 'present',
    state: 'known',
    source_ids: [value.position.premise_id],
    reason: null,
  };
  const fact = proposalReading({
    id: 'pair-fact',
    kind: 'fact',
    state: 'supported',
    path: {
      binding: 'route',
      extend: false,
      source: { kind: 'comparison', requirement_id: 'pair', arm: 'reference' },
    },
    predicate: { kind: 'presence', subject: { side: 'white', role: 'pawn', square: 'e2' } },
    at: { kind: 'ply', ply_count: 0 },
    expected: 'present',
    result: {
      state: 'supported',
      samples: [sample],
      needs: [],
      reason: null,
      proofs: [{ bindings: { route: path.moves }, sample_ids: [sample.id], requirement_ids: ['pair-fact'] }],
    },
  });
  value.response.observations.selection.proposals = [proposalEntry([pair, fact])];
  value.response.observations.selection.local_expansions_used = 0;
  assert.ok(value.decode().selection);
  fact.path.source.arm = 'alternative';
  assert.equal(value.decode().selection, undefined);
  fact.path.source.arm = 'reference';
  value.response.observations.selection.proposals[0].requirements.reverse();
  assert.equal(value.decode().selection, undefined);
});

test('proposal Any preserves unresolved children while All requires a compatible supported proof for each', () => {
  const value = factProposalExample();
  const need = {
    kind: 'input',
    reason: 'resource_unavailable',
    prefix_moves_uci: [],
    requirement_id: 'after',
    horizon: 1,
  };
  value.after.state = 'partial';
  value.after.result = { state: 'partial', samples: [], proofs: [], needs: [need], reason: null };
  value.group.mode = 'any';
  value.group.members = ['before', 'after'];
  value.group.result = {
    state: 'supported',
    samples: [],
    needs: [need],
    reason: null,
    proofs: [{ ...structuredClone(value.before.result.proofs[0]), requirement_ids: ['before', 'both'] }],
  };
  value.response.observations.selection.proposals[0].requirements = [value.before, value.after, value.group];
  assert.equal(value.decode().selection?.proposals?.[0].requirements[2].result?.needs.length, 1);
  value.group.mode = 'all';
  assert.equal(value.decode().selection, undefined);
  value.group.state = value.group.result.state = 'partial';
  value.group.result.proofs = [];
  assert.ok(value.decode().selection);
  value.group.state = value.group.result.state = 'refuted';
  assert.equal(value.decode().selection, undefined, 'an unknown child is not a counterexample');
});

test('proposal different bindings explicitly preserve independent paths instead of joining incompatible futures', () => {
  const value = factProposalExample();
  const path = value.response.observations.paths.find((path: Wire) => path.moves[0] === 'd2d4');
  assert.ok(path);
  const position = value.response.observations.positions.find(
    (position: Wire) => position.id === path.position_ids[1],
  );
  value.after.path.binding = 'independent';
  value.after.path.source.moves_uci = ['d2d4'];
  Object.assign(value.after.result.samples[0], {
    line_id: path.id,
    binding: 'independent',
    moves_uci: ['d2d4'],
    position_ids: [position.id],
    occurrence_ids: [path.occurrence_ids[0]],
    source_ids: [position.premise_id],
  });
  value.after.result.proofs[0].bindings = { independent: ['d2d4'] };
  value.group.members = ['before', 'after'];
  value.group.result.proofs = [
    {
      bindings: { route: ['e2e4'], independent: ['d2d4'] },
      sample_ids: ['sample:before', 'sample:after'],
      requirement_ids: ['before', 'after', 'both'],
    },
  ];
  value.response.observations.selection.proposals[0].requirements = [value.before, value.after, value.group];
  assert.ok(value.decode().selection);
  value.after.path.binding = value.after.result.samples[0].binding = 'route';
  value.after.result.proofs[0].bindings = { route: ['d2d4'] };
  value.group.result.proofs[0].bindings = { route: ['e2e4'] };
  assert.equal(value.decode().selection, undefined);
});

test('proposal transport preserves separate observed, answered and unsupported requirements', () => {
  const value = openingPvExample();
  const selection = value.response.observations.selection;
  const inquiry = selection.inquiries.find((row: Wire) => row.answer);
  const path = value.response.observations.paths.find(
    (row: Wire) => row.id === inquiry.answer.reference_line_id,
  );
  selection.proposals = [
    proposalEntry([
      proposalReading({
        id: 'observed',
        kind: 'observe_move',
        prefix_moves_uci: [],
        move_uci: path.moves[0],
        state: 'observed',
        line_id: path.id,
        occurrence_id: path.occurrence_ids[0],
        position_id: path.position_ids[1],
      }),
      proposalReading({
        id: 'compared',
        kind: 'compare_moves',
        prefix_moves_uci: [],
        reference_move: inquiry.reference_move,
        alternative_move: inquiry.alternative_move,
        expected_relation: inquiry.answer.evaluation_relation,
        state: 'supported',
        answer: inquiry.answer,
        work_id: inquiry.answer.work_id,
      }),
      proposalReading({
        id: 'meaning',
        kind: 'uninterpreted',
        text: 'Does this prepare a pawn break?',
        state: 'unsupported_reading',
        stop_reason: 'uninterpreted_hypothesis',
      }),
    ]),
  ];
  selection.local_expansions_used = 0;
  const decoded = value.decode().selection!;
  assert.ok(decoded);
  assert.deepEqual(decoded.proposals, selection.proposals);
  assert.equal(decoded.local_expansions_used, 0);
  assert.deepEqual(
    decoded.proposals![0].requirements.map(row => row.state),
    ['observed', 'supported', 'unsupported_reading'],
  );
  const meanings = selection.proposals[0].requirements;
  meanings[1].state = 'refuted';
  assert.equal(
    value.decode().selection,
    undefined,
    'an opposite expected relation cannot retain supported evidence',
  );
  assert.ok(
    value.decode().statements.length,
    'independent board observations survive a rejected proposal view',
  );
});

test('proposal transport rejects unrelated observation edges and forged metadata or limits', () => {
  for (const mutation of [
    (selection: Wire) => {
      selection.proposals[0].requirements[0].move_uci = 'a1a8';
    },
    (selection: Wire) => {
      selection.proposals[0].requirements[0].position_id = 'missing-position';
    },
    (selection: Wire) => {
      selection.proposals[0].requirements[0].state = 'supported';
    },
    (selection: Wire) => {
      selection.proposals[0].requirements[0].state = 'partial';
    },
    (selection: Wire) => {
      selection.proposals[0].origin.question.version = null;
    },
    (selection: Wire) => {
      selection.local_expansions_used = 601;
    },
    (selection: Wire) => {
      selection.proposals.push(selection.proposals[0]);
    },
  ]) {
    const value = openingPvExample();
    const path = value.response.observations.paths.find(
      (row: Wire) => row.kind !== 'history' && row.moves.length,
    );
    const selection = value.response.observations.selection;
    selection.proposals = [
      proposalEntry([
        proposalReading({
          id: 'observe',
          kind: 'observe_move',
          prefix_moves_uci: [],
          move_uci: path.moves[0],
          state: 'observed',
          line_id: path.id,
          occurrence_id: path.occurrence_ids[0],
          position_id: path.position_ids[1],
        }),
      ]),
    ];
    selection.local_expansions_used = 0;
    assert.ok(value.decode().selection);
    mutation(selection);
    assert.equal(value.decode().selection, undefined);
    assert.ok(value.decode().statements.length);
  }
});

test('proposal child comparisons use the exact later turn and can own a restricted pair without a target question', () => {
  const value = openingPvExample('continuation-deep-complete');
  const observations = value.decode();
  const search = structuredClone(
    value.response.observations.selection.response_searches.find((row: Wire) => row.ply_offset === 2),
  );
  const [reference, alternative] = search.candidates;
  search.candidates = [reference, alternative];
  search.multi_pv = 2;
  search.root_restriction = { kind: 'restricted', moves_uci: [reference.move_uci, alternative.move_uci] };
  const answer = {
    work_id: search.work_id,
    execution_key_sha256: search.execution_key_sha256,
    engine_profile: search.engine_profile,
    required_depth: search.required_depth,
    reference_depth: reference.evaluation.depth,
    alternative_depth: alternative.evaluation.depth,
    evaluation_relation: 'worse',
    reference_line_id: reference.line_id,
    alternative_line_id: alternative.line_id,
    reference_occurrence_id: reference.occurrence_id,
    alternative_occurrence_id: alternative.occurrence_id,
    reference_evaluation: reference.evaluation,
    alternative_evaluation: alternative.evaluation,
  };
  const reading = proposalReading({
    id: 'child',
    kind: 'compare_moves',
    prefix_moves_uci: search.prefix_moves_uci,
    reference_move: reference.move_uci,
    alternative_move: alternative.move_uci,
    state: 'answered',
    answer,
    work_id: search.work_id,
  });
  const selection = {
    version: 1,
    candidates: [],
    scenes: [],
    response_searches: [search],
    proposals: [proposalEntry([reading])],
    local_expansions_used: 0,
  };
  const context = {
    rootFen: value.fixture.fen,
    playedMove: value.fixture.move as Uci,
    engineProfile: value.response.engine_profile,
  };
  assert.ok(decodeCommentarySelection(selection, observations, context));
  answer.evaluation_relation = 'better';
  assert.equal(
    decodeCommentarySelection(selection, observations, context),
    undefined,
    'White score increase is worse for the actual Black mover',
  );
  answer.evaluation_relation = 'worse';
  reading.prefix_moves_uci = search.prefix_moves_uci.slice(0, 1);
  assert.equal(
    decodeCommentarySelection(selection, observations, context),
    undefined,
    'a first-child prefix cannot own a later comparison',
  );
});

function questionComparisonExample(atRoot = false) {
  // This selection-unit check supplies only already-decoded observations it consumes.
  // The standard opening and invented scores do not read a fixture or claim a searched result.
  const childFen = 'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1';
  const rows = [
    ['e7e5', 'rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2', 20],
    ['c7c5', 'rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2', 50],
  ] as const;
  const startFen = atRoot ? childFen : initialFen;
  const prefix = atRoot ? [] : ['e2e4' as Uci];
  const paths = rows.map(([move, fen, score]) => {
    const evaluation = { source: atRoot ? 'investigation_comparison' as const : 'response_comparison' as const,
      kind: 'engine_search' as const, depth: 16, white_score: { kind: 'cp' as const, value: score as number } };
    return {
      id: `line:${move}`, kind: 'analysis' as const, startFen,
      moves: [...(atRoot ? [] : [{ uci: 'e2e4' as Uci, label: 'e4', fenAfter: childFen }]),
        { uci: move, label: move, fenAfter: fen }],
      occurrenceIds: [...(atRoot ? [] : ['root:e4']), `reply:${move}`],
      positionIds: [...(atRoot ? [] : ['position:root']), 'position:child', `position:${move}`],
      ...(atRoot ? { evaluations: [evaluation] } : {
        conditionalEvaluations: [{ searchPositionId: 'position:child', plyOffset: 1, evaluation }],
      }),
    };
  });
  const observations = { statements: [], paths,
    positions: [...(atRoot ? [] : [{ id: 'position:root', fen: initialFen }]), { id: 'position:child', fen: childFen },
      ...rows.map(([move, fen]) => ({ id: `position:${move}`, fen }))] } as unknown as MoveReviewObservations;
  const answer: Wire = {
    work_id: 'work:1', execution_key_sha256: '1'.repeat(64), engine_profile: moveReviewEngineProfile,
    required_depth: 16, reference_depth: 16, alternative_depth: 16, evaluation_relation: 'worse',
    reference_line_id: paths[0].id, alternative_line_id: paths[1].id,
    reference_occurrence_id: paths[0].occurrenceIds[prefix.length], alternative_occurrence_id: paths[1].occurrenceIds[prefix.length],
    reference_evaluation: paths[0].evaluations?.[0] ?? paths[0].conditionalEvaluations![0].evaluation,
    alternative_evaluation: paths[1].evaluations?.[0] ?? paths[1].conditionalEvaluations![0].evaluation,
  };
  const reading = proposalReading({ id: 'child', kind: 'compare_moves', prefix_moves_uci: prefix,
    reference_move: 'e7e5', alternative_move: 'c7c5', state: 'answered', answer, work_id: answer.work_id });
  const selection: Wire = { version: 1, candidates: [], scenes: [],
    proposals: [proposalEntry([reading])], local_expansions_used: 0 };
  const context = { rootFen: startFen, playedMove: (atRoot ? 'e7e5' : 'e2e4') as Uci, engineProfile: moveReviewEngineProfile };
  const candidate: Wire = {
    id: 'question:comparison', kind: 'comparison', state: 'ready', selection: 'selected',
    root_occurrence_id: paths[0].occurrenceIds[0],
    occurrence_ids: [...new Set(paths.flatMap(path => path.occurrenceIds))],
    position_ids: [...new Set(paths.flatMap(path => path.positionIds ?? []))],
    line_ids: paths.map(path => path.id), evidence_ids: [], source_refs: [],
    detail: {
      type: 'question_comparison', scope: 'same_position_pair',
      prefix_moves_uci: reading.prefix_moves_uci,
      reference_move: reading.reference_move, alternative_move: reading.alternative_move,
      perspective: 'black',
      question_refs: [{ proposal_id: selection.proposals[0].id, requirement_id: reading.id }],
      evaluation_relation: answer.evaluation_relation, stop_reason: null,
      text: 'At this exact position, the alternative has a lower score for Black than the reference move.',
    },
  };
  const scene = { id: 'scene:question', root_occurrence_id: candidate.root_occurrence_id,
    occurrence_ids: candidate.occurrence_ids, line_ids: candidate.line_ids,
    candidate_ids: [candidate.id], state: candidate.state };
  selection.candidates = [candidate];
  selection.scenes = [scene];
  return { observations, answer, reading, selection, context, candidate, scene, paths,
    decode: () => decodeCommentarySelection(selection, observations, context) };
}

function questionComparisonPanel(value: ReturnType<typeof questionComparisonExample>): MoveReviewPanelProps {
  const selection = value.decode();
  assert.ok(selection);
  // The view can render path frames without the optional square-resource inspector data.
  const { positions: _positions, ...observations } = value.observations;
  observations.selection = selection;
  const path = observations.paths[0];
  const panel: MoveReviewPanelProps = {
    job: { kind: 'loading', observations, subject: {
      variant: 'standard', initialFen: path.startFen, movePrefixUci: [],
      before: { path: '', fen: path.startFen },
      played: { uci: path.moves[0].uci, san: path.moves[0].label },
      after: { path: 'aa', fen: path.moves[0].fenAfter },
    } },
    view: { evidenceExpanded: false }, copy: moveReviewCopy('en-US'), locale: 'en-US',
    orientation: 'white', liveEnginePaused: false,
    actions: {
      selectCandidate: () => {}, toggleEvidence: () => {}, retry: () => {},
      previewFrame: frame => { panel.view.hoveredFrame = frame; },
      clearPreview: () => { panel.view.hoveredFrame = undefined; },
      toggleProof: id => { panel.view.expandedProofId = id; },
      pinFrame: frame => { panel.view.pinnedFrame = frame; },
      clearPin: () => { panel.view.pinnedFrame = undefined; },
    },
  };
  return panel;
}

function questionViewNodes(node: VNode, selector: string): VNode[] {
  return [node, ...(node.children ?? []).flatMap(child =>
    typeof child === 'object' && child ? questionViewNodes(child, selector) : [],
  )].filter(item => item.sel === selector);
}

function questionViewText(node: VNode): string {
  return [node.text ?? '', ...(node.children ?? []).map(child =>
    typeof child === 'object' && child ? questionViewText(child) : String(child ?? ''),
  )].join(' ').trim();
}

test('question comparison UI opens the exact decision and each branch, including a root comparison', async () => {
  const { renderMoveReview } = await import('../../src/review/view/moveReview');
  for (const atRoot of [false, true]) for (const locale of ['en-US', 'ko-KR'] as const) {
    const value = questionComparisonExample(atRoot);
    const ply = atRoot ? 1 : 2;
    value.paths[0].moves[ply - 1].label = 'e5';
    value.paths[1].moves[ply - 1].label = 'c5';
    const panel = questionComparisonPanel(value);
    panel.locale = locale;
    panel.copy = moveReviewCopy(locale);
    const tree = renderMoveReview(panel);
    const scene = questionViewNodes(tree, 'article.move-review__scene')[0];
    assert.ok(scene);
    const buttons = questionViewNodes(scene, 'button');
    assert.equal(buttons.length, 3);
    assert.match(questionViewText(buttons[1]), /^e5 /);
    assert.match(questionViewText(buttons[2]), /^c5 /);
    (buttons[0].data!.on!.click as () => void)();
    assert.deepEqual(panel.view.pinnedFrame, { proofId: value.paths[0].id, ply, phase: 'before' });
    for (const index of [0, 1]) {
      (buttons[index + 1].data!.on!.click as () => void)();
      assert.deepEqual(panel.view.pinnedFrame, { proofId: value.paths[index].id, ply, phase: 'after' });
      assert.equal(panel.view.expandedProofId, value.paths[index].id);
    }
    const observations = panel.job.observations!;
    observations.selection!.candidates[0].detail.type = 'observed_material_comparison';
    assert.deepEqual(commentarySceneFrame(observations, observations.selection!.scenes[0]),
      { proofId: value.paths[0].id, ply: 1, phase: 'after' }, 'ordinary scenes retain their occurrence anchor');
  }
});

test('question comparison UI retains the exact unanswered condition before and after retirement', async () => {
  const { renderMoveReview } = await import('../../src/review/view/moveReview');
  const value = questionComparisonExample();
  const proposal = value.selection.proposals[0];
  proposal.admission = 'generated';
  proposal.question_text = 'Does another reply change this choice?';
  proposal.answer_text = 'The full answer must not be repeated in the pending-question row.';
  proposal.requirements.push(proposalReading({ id: 'pending', kind: 'compare_moves',
    prefix_moves_uci: ['e2e4'], reference_move: 'e7e5', alternative_move: 'g8f6',
    state: 'awaiting_engine', work_id: 'work:2' }));
  const pending = structuredClone(value.candidate);
  Object.assign(pending, { id: 'question:pending', state: 'partial', selection: 'context',
    line_ids: [value.paths[0].id], occurrence_ids: value.paths[0].occurrenceIds,
    position_ids: value.paths[0].positionIds });
  Object.assign(pending.detail, { alternative_move: 'g8f6', evaluation_relation: null,
    question_refs: [{ proposal_id: proposal.id, requirement_id: 'pending' }],
    text: 'The other reply has not been compared.' });
  value.selection.candidates.push(pending);
  const peer = structuredClone(proposal);
  peer.id = 'proposal:peer';
  value.selection.proposals.push(peer);
  pending.detail.question_refs.push({ proposal_id: peer.id, requirement_id: 'pending' });
  proposal.requirements.push(proposalReading({ id: 'pending-other', kind: 'compare_moves',
    prefix_moves_uci: ['e2e4'], reference_move: 'e7e5', alternative_move: 'g8h6', state: 'deferred' }));
  const secondPending = structuredClone(pending);
  secondPending.id = 'question:pending-other';
  Object.assign(secondPending.detail, { alternative_move: 'g8h6',
    question_refs: [{ proposal_id: proposal.id, requirement_id: 'pending-other' }] });
  value.selection.candidates.push(secondPending);
  const panel = questionComparisonPanel(value);
  const observations = panel.job.observations!;
  const row = () => {
    const rows = questionViewNodes(renderMoveReview(panel), 'article.move-review__inquiry');
    assert.equal(rows.length, 2, 'both unanswered comparisons remain beside the answered sibling');
    const text = rows.map(questionViewText).join(' ');
    assert.equal(text.split(proposal.question_text).length - 1, 1, 'shared question text appears once');
    assert.ok(!text.includes(proposal.answer_text));
    return text;
  };
  assert.match(row(), /Comparing the two moves/);
  releaseCommentaryInvestigation(observations, 'cancelled');
  assert.equal(observations.selection!.proposals, undefined);
  assert.ok(panel.job.kind === 'loading');
  panel.job = { ...panel.job, kind: 'fault', message: 'Review interrupted', retryable: true };
  assert.match(row(), /still unanswered/);
  assert.ok(row().includes(panel.copy.stopReasons.cancelled));
  observations.selection!.candidates = observations.selection!.candidates.filter(candidate => candidate.id !== pending.id);
  const remaining = questionViewNodes(renderMoveReview(panel), 'article.move-review__inquiry');
  assert.equal(remaining.length, 1);
  assert.ok(questionViewText(remaining[0]).includes(proposal.question_text),
    'removing the first comparison lets the surviving row show the shared question again');
  panel.job.observationFrame = { proofId: value.paths[0].id, ply: 2, phase: 'after' };
  assert.equal(questionViewNodes(renderMoveReview(panel), 'article.move-review__inquiry').length, 0,
    'a different selected occurrence cannot display the old root question');
  panel.job.observationFrame = undefined;
  observations.selection!.candidates = observations.selection!.candidates.filter(candidate => candidate.id !== secondPending.id);
  assert.equal(questionViewNodes(renderMoveReview(panel), 'article.move-review__inquiry').length, 0,
    'retained outcomes alone do not revive a withdrawn question');
});

test('a reopened question retains its earlier exact search pool without borrowing the old answer', () => {
  const value = questionComparisonExample();
  const { answer, reading, paths, selection, observations } = value;
  const child = observations.positions!.find(position => position.id === 'position:child')!;
  child.legal_moves = { complete: true, moves: ['e7e5', 'c7c5', 'g8f6'].map(uci => ({ uci })) } as any;
  selection.response_searches = [{
    work_id: answer.work_id, execution_key_sha256: answer.execution_key_sha256,
    engine_profile: answer.engine_profile, required_depth: 16, multi_pv: 2,
    root_move: 'e2e4', root_move_occurrence_id: 'root:e4', child_position_id: child.id,
    prefix_moves_uci: ['e2e4'], ply_offset: 1,
    root_restriction: { kind: 'restricted', moves_uci: ['e7e5', 'c7c5'] },
    candidates: paths.map(path => ({ move_uci: path.moves[1].uci, line_id: path.id,
      occurrence_id: path.occurrenceIds[1], evaluation: path.conditionalEvaluations![0].evaluation })),
  }];
  assert.ok(value.decode());
  Object.assign(reading, { state: 'awaiting_engine', work_id: 'work:3', answer: null });
  selection.candidates = selection.scenes = [];
  for (const [work, depth] of [[0, 15], [2, 17]]) {
    const search = structuredClone(selection.response_searches[0]);
    Object.assign(search, { work_id: `work:${work}`, execution_key_sha256: String(work).repeat(64), required_depth: depth });
    search.candidates.forEach((candidate: Wire, index: number) => {
      candidate.evaluation.depth = depth;
      const prior = paths[index].conditionalEvaluations![0];
      paths[index].conditionalEvaluations!.push({ ...prior, evaluation: candidate.evaluation });
    });
    selection.response_searches.push(search);
  }
  const reopened = value.decode();
  assert.equal(reopened?.response_searches?.length, 3, 'two investigations and their conflict recheck retain exact owners');
  assert.equal(reopened?.response_searches?.[0].work_id, answer.work_id);
  assert.equal(reopened?.proposals?.[0].requirements[0].answer, null);
  reading.alternative_move = 'g8f6';
  assert.equal(value.decode(), undefined, 'another legal pair cannot own the old search');
  reading.alternative_move = 'c7c5';
  Object.assign(reading, { state: 'answered', answer });
  assert.equal(value.decode(), undefined, 'the old report cannot answer a newly identified work');
});

test('question comparison candidates reuse exact proposal answers and belong to selected scenes', () => {
  const value = questionComparisonExample();
  assert.equal(value.decode()?.candidates[0].detail.evaluation_relation, 'worse');
  const contextOnly = questionComparisonExample();
  contextOnly.candidate.selection = 'context';
  contextOnly.selection.scenes = [];
  assert.ok(contextOnly.decode(), 'a resolved pair may remain unselected context');
  const otherRoot = questionComparisonExample();
  otherRoot.observations.paths.push({ id: 'line:d4', kind: 'played', startFen: initialFen,
    moves: [{ uci: 'd2d4', label: 'd4', fenAfter: 'rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1' }],
    occurrenceIds: ['root:d4'], positionIds: ['position:root', 'position:d4'] });
  otherRoot.context.playedMove = 'd2d4';
  otherRoot.candidate.root_occurrence_id = otherRoot.scene.root_occurrence_id = 'root:d4';
  otherRoot.candidate.occurrence_ids = otherRoot.scene.occurrence_ids = ['root:d4', ...otherRoot.candidate.occurrence_ids];
  assert.ok(otherRoot.decode(), 'the displayed actual root can differ from the compared path root');
  for (const [score, relation] of [[-20, 'better'], [20, 'equal'], [50, 'worse']] as const) {
    value.answer.alternative_evaluation.white_score.value = score;
    value.answer.evaluation_relation = value.candidate.detail.evaluation_relation = relation;
    assert.equal(value.decode()?.candidates[0].detail.evaluation_relation, relation);
  }
  const peer = structuredClone(value.selection.proposals[0]);
  peer.id = 'proposal:peer';
  peer.requirements[0].expected_relation = 'better';
  peer.requirements[0].state = 'refuted';
  peer.requirements[0].answer = Object.fromEntries(Object.entries(peer.requirements[0].answer).reverse());
  value.selection.proposals.push(peer);
  value.candidate.detail.question_refs.push({ proposal_id: peer.id, requirement_id: peer.requirements[0].id });
  assert.ok(value.decode(), 'opposite expectations may share the same oriented answer');
  peer.requirements[0].work_id = peer.requirements[0].answer.work_id = 'work:999';
  assert.equal(value.decode(), undefined, 'refs cannot disagree about the answer owner');

  for (const mutate of [
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.detail.evaluation_relation = 'better'; },
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.detail.perspective = 'white'; },
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.detail.prefix_moves_uci = []; },
    (v: ReturnType<typeof questionComparisonExample>) => {
      [v.candidate.detail.reference_move, v.candidate.detail.alternative_move] =
        [v.reading.alternative_move, v.reading.reference_move];
    },
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.detail.question_refs[0].requirement_id = 'missing'; },
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.detail.question_refs.push(v.candidate.detail.question_refs[0]); },
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.detail.extra = true; },
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.detail.text = undefined; },
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.line_ids = [v.paths[0].id]; },
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.occurrence_ids = [v.candidate.root_occurrence_id]; },
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.position_ids = []; },
    (v: ReturnType<typeof questionComparisonExample>) => { v.selection.scenes = []; },
    (v: ReturnType<typeof questionComparisonExample>) => { v.candidate.selection = 'context'; },
  ]) {
    const invalid = questionComparisonExample();
    mutate(invalid);
    assert.equal(invalid.decode(), undefined, String(mutate));
  }
});

test('question comparison candidates retain unknown stop reasons without borrowing an answered ref', () => {
  const value = questionComparisonExample();
  const { selection, reading, candidate, paths } = value;
  delete selection.response_searches;
  Object.assign(reading, { state: 'stopped', stop_reason: 'investigation_limit', answer: null, work_id: null });
  Object.assign(candidate, { state: 'partial', selection: 'context', line_ids: [paths[0].id],
    occurrence_ids: paths[0].occurrenceIds, position_ids: paths[0].positionIds });
  Object.assign(candidate.detail, { evaluation_relation: null, stop_reason: 'investigation_limit',
    text: 'This pair remains untested because the investigation limit was reached.' });
  selection.scenes = [];
  assert.equal(value.decode()?.candidates[0].detail.stop_reason, 'investigation_limit');
  candidate.detail.stop_reason = null;
  assert.equal(value.decode(), undefined, 'the presentation preserves its referenced stop reason');
  candidate.detail.stop_reason = 'investigation_limit';
  candidate.selection = 'selected';
  assert.equal(value.decode(), undefined, 'an unknown comparison cannot be selected');
  candidate.selection = 'context';
  const peer = proposalEntry([proposalReading({ ...reading, id: 'answered', state: 'answered',
    stop_reason: null, work_id: value.answer.work_id, answer: value.answer })]);
  peer.id = 'proposal:answered';
  selection.proposals.push(peer);
  candidate.detail.question_refs.push({ proposal_id: peer.id, requirement_id: 'answered' });
  assert.equal(value.decode(), undefined, 'unknown and answered refs cannot be presented as one unresolved pair');
});

test('question comparison candidates retain root answers, exact positions and unknown states', () => {
  const value = questionComparisonExample(true);
  for (const [score, relation] of [[-20, 'better'], [20, 'equal'], [50, 'worse']] as const) {
    value.answer.alternative_evaluation.white_score.value = score;
    value.answer.evaluation_relation = value.candidate.detail.evaluation_relation = relation;
    assert.equal(value.decode()?.candidates[0].detail.evaluation_relation, relation);
  }
  value.candidate.selection = 'context';
  value.selection.scenes = [];
  assert.ok(value.decode(), 'a resolved root pair may remain context');

  const otherRoot = questionComparisonExample(true);
  otherRoot.observations.paths.push({ id: 'line:nf6', kind: 'played', startFen: otherRoot.context.rootFen,
    moves: [{ uci: 'g8f6', label: 'Nf6', fenAfter: 'rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2' }],
    occurrenceIds: ['root:nf6'], positionIds: ['position:child', 'position:nf6'] });
  otherRoot.context.playedMove = 'g8f6';
  otherRoot.candidate.root_occurrence_id = otherRoot.scene.root_occurrence_id = 'root:nf6';
  otherRoot.candidate.occurrence_ids = otherRoot.scene.occurrence_ids = ['root:nf6', ...otherRoot.candidate.occurrence_ids];
  const selected = otherRoot.decode();
  assert.ok(selected, 'the played move may lie outside the two compared root moves');
  assert.equal(selected.candidates[0].lineIds.length, 2);
  assert.equal(commentarySceneFrame(otherRoot.observations, selected.scenes[0])?.proofId, 'line:nf6');
  otherRoot.observations.positions!.push({ ...otherRoot.observations.positions![0], id: 'position:other-root' });
  otherRoot.paths.filter(path => otherRoot.candidate.line_ids.includes(path.id))
    .forEach(path => { path.positionIds[0] = 'position:other-root'; });
  otherRoot.candidate.position_ids = ['position:other-root', ...otherRoot.candidate.position_ids.filter((id: string) => id !== 'position:child')];
  assert.equal(otherRoot.decode(), undefined, 'equal FENs do not let the comparison borrow a different root position');

  const wrongFen = questionComparisonExample(true);
  wrongFen.observations.positions![0].fen = wrongFen.context.rootFen.replace(' 0 1', ' 1 1');
  assert.equal(wrongFen.decode(), undefined, 'the observed root FEN remains exact');
  const wrongOwner = questionComparisonExample(true);
  const peer = structuredClone(wrongOwner.selection.proposals[0]);
  peer.id = 'proposal:other-owner';
  peer.requirements[0].work_id = peer.requirements[0].answer.work_id = 'work:999';
  wrongOwner.selection.proposals.push(peer);
  wrongOwner.candidate.detail.question_refs.push({ proposal_id: peer.id, requirement_id: peer.requirements[0].id });
  assert.equal(wrongOwner.decode(), undefined, 'root refs cannot combine separately owned answers');
  const childScore = questionComparisonExample(true);
  childScore.answer.reference_evaluation.source = childScore.answer.alternative_evaluation.source = 'response_comparison';
  assert.equal(childScore.decode(), undefined, 'a child evaluation cannot own a root answer');

  const unknown = questionComparisonExample(true);
  Object.assign(unknown.reading, { state: 'stopped', stop_reason: 'investigation_limit', answer: null, work_id: null });
  Object.assign(unknown.candidate, { state: 'partial', selection: 'context', line_ids: [unknown.paths[0].id],
    occurrence_ids: unknown.paths[0].occurrenceIds, position_ids: unknown.paths[0].positionIds });
  Object.assign(unknown.candidate.detail, { evaluation_relation: null, stop_reason: 'investigation_limit',
    text: 'The relative value of these two root moves remains unverified.' });
  unknown.selection.scenes = [];
  assert.equal(unknown.decode()?.candidates[0].detail.stop_reason, 'investigation_limit');
  unknown.candidate.detail.stop_reason = null;
  assert.equal(unknown.decode(), undefined, 'an unknown root pair retains its referenced stop reason');
  unknown.candidate.detail.stop_reason = 'investigation_limit';
  unknown.candidate.selection = 'selected';
  assert.equal(unknown.decode(), undefined, 'an unknown root pair cannot be selected');
});

test('generated proposal text stays paired without replacing owned readings', () => {
  const value = questionComparisonExample(true);
  const proposal = value.selection.proposals[0];
  Object.assign(proposal, { admission: 'generated', question_text: 'How do these root moves compare?',
    answer_text: 'The answer follows the owned comparison reading.' });
  const decoded = value.decode();
  assert.equal(decoded?.proposals?.[0].question_text, proposal.question_text);
  assert.equal(decoded?.proposals?.[0].answer_text, proposal.answer_text);
  assert.equal(decoded?.proposals?.[0].requirements[0].answer?.evaluation_relation, 'worse');
  value.candidate.detail.evaluation_relation = 'better';
  assert.equal(value.decode(), undefined, 'presentation text cannot override the referenced comparison answer');
  value.candidate.detail.evaluation_relation = 'worse';

  for (const field of ['question_text', 'answer_text']) {
    const text = proposal[field];
    delete proposal[field];
    assert.equal(value.decode(), undefined, `${field}: its pair must be present`);
    for (const invalid of ['', 42, 'x'.repeat(16001)]) {
      proposal[field] = invalid;
      assert.equal(value.decode(), undefined, `${field}: invalid text`);
    }
    proposal[field] = text;
  }
  proposal.question_text = 'q'.repeat(16000);
  proposal.answer_text = 'a'.repeat(16000);
  assert.equal(value.decode()?.proposals?.[0].answer_text, proposal.answer_text);
  for (const admission of ['submitted', 'internal']) {
    proposal.admission = admission;
    assert.equal(value.decode(), undefined, `${admission} cannot carry generated presentation text`);
  }
  delete proposal.question_text;
  delete proposal.answer_text;
  for (const admission of ['submitted', 'internal', 'generated']) {
    proposal.admission = admission;
    assert.ok(value.decode(), `${admission} may omit both presentation fields`);
  }
});

test('proposal pending child work passes the actual job decoder only for its exact prefix and two moves', () => {
  const value = openingPvExample('continuation-deep-awaiting');
  const question = value.response.observations.selection.continuation_inquiries.find(
    (row: Wire) => row.state === 'awaiting_engine',
  );
  const work = value.response.issued_engine_work;
  const pair = question.legal_moves.slice(0, 2);
  work.root_restriction = { kind: 'restricted', moves_uci: pair };
  work.search_limits.multi_pv = 2;
  const reading = proposalReading({
    id: 'child-pending',
    kind: 'compare_moves',
    prefix_moves_uci: question.prefix_moves_uci,
    reference_move: pair[0],
    alternative_move: pair[1],
    state: 'awaiting_engine',
    work_id: work.work_id,
  });
  value.response.observations.selection = {
    version: 1,
    candidates: [],
    scenes: [],
    proposals: [proposalEntry([reading])],
    local_expansions_used: 0,
  };
  assert.equal(observationFixtureJob(value.fixture).kind, 'loading');
  reading.prefix_moves_uci = question.prefix_moves_uci.slice(0, 1);
  assert.throws(() => observationFixtureJob(value.fixture), /Rejected runtime observation fixture/);
  reading.prefix_moves_uci = question.prefix_moves_uci;
  reading.work_id = 'work:999';
  assert.throws(() => observationFixtureJob(value.fixture), /Rejected runtime observation fixture/);
});

test('proposal fact continuation needs own one exact existing child survey without a fabricated inquiry', () => {
  const value = openingPvExample('continuation-deep-awaiting');
  const question = value.response.observations.selection.continuation_inquiries.find(
    (row: Wire) => row.state === 'awaiting_engine',
  );
  const work = value.response.issued_engine_work;
  const prefix = question.prefix_moves_uci;
  const need = {
    kind: 'continuation',
    reason: 'path_end',
    prefix_moves_uci: prefix,
    requirement_id: null,
    horizon: prefix.length + 1,
  };
  const reading = proposalReading({
    id: 'fact-pending',
    kind: 'fact',
    state: 'awaiting_engine',
    work_id: work.work_id,
    path: { binding: 'route', source: { kind: 'declared', moves_uci: prefix }, extend: true },
    predicate: { kind: 'check', side: 'white' },
    at: { kind: 'ply', ply_count: prefix.length + 1 },
    expected: 'absent',
    result: { state: 'partial', samples: [], proofs: [], needs: [need], reason: null },
  });
  const group = proposalReading({
    id: 'fact-pending-group',
    kind: 'fact_group',
    mode: 'all',
    members: [reading.id],
    state: 'awaiting_engine',
    work_id: work.work_id,
    result: structuredClone(reading.result),
  });
  value.response.observations.selection = {
    version: 1,
    candidates: [],
    scenes: [],
    proposals: [proposalEntry([reading, group])],
    local_expansions_used: 0,
  };
  assert.equal(observationFixtureJob(value.fixture).kind, 'loading');
  group.work_id = 'work:999';
  assert.throws(() => observationFixtureJob(value.fixture), /Rejected runtime observation fixture/);
  group.work_id = work.work_id;
  need.prefix_moves_uci = prefix.slice(0, 1);
  assert.throws(() => observationFixtureJob(value.fixture), /Rejected runtime observation fixture/);
  need.prefix_moves_uci = prefix;
  reading.path.extend = false;
  assert.throws(() => observationFixtureJob(value.fixture), /Rejected runtime observation fixture/);
  reading.path.extend = true;
  reading.work_id = 'work:999';
  assert.throws(() => observationFixtureJob(value.fixture), /Rejected runtime observation fixture/);
});

test('proposal fact premise sources can only reuse that exact earlier premise witness', () => {
  const value = openingPvExample('continuation-support-frontier-complete');
  const question = value.response.observations.selection.continuation_inquiries.find((q: Wire) =>
    q.premises.some((p: Wire) => p.witness_line_ids.length),
  );
  const premise = question.premises.find((p: Wire) => p.witness_line_ids.length);
  const path = value.response.observations.paths.find(
    (path: Wire) => path.id === premise.witness_line_ids[0],
  );
  const root = value.response.observations.positions.find(
    (position: Wire) => position.id === path.position_ids[0],
  );
  const file = root.pawn_files.files.find((file: Wire) => file.file === 'a');
  const sample = {
    id: 'sample:premise',
    line_id: path.id,
    binding: 'route',
    moves_uci: path.moves,
    from_ply: 0,
    through_ply: 0,
    position_ids: [root.id],
    occurrence_ids: path.occurrence_ids,
    state: 'known',
    value: file.occupancy,
    source_ids: file.source_ids,
    reason: null,
  };
  const reading = proposalReading({
    id: 'premise-fact',
    kind: 'fact',
    state: 'observed',
    path: {
      binding: 'route',
      extend: false,
      source: { kind: 'premise', inquiry_id: question.id, premise_id: premise.id },
    },
    predicate: { kind: 'file_occupancy', property: 'a' },
    at: { kind: 'ply', ply_count: 0 },
    result: {
      state: 'observed',
      samples: [sample],
      needs: [],
      reason: null,
      proofs: [
        { bindings: { route: path.moves }, sample_ids: [sample.id], requirement_ids: ['premise-fact'] },
      ],
    },
  });
  value.response.observations.selection.proposals = [proposalEntry([reading])];
  value.response.observations.selection.local_expansions_used = 0;
  assert.ok(value.decode().selection);
  reading.path.source.premise_id = 'foreign:premise';
  assert.equal(value.decode().selection, undefined);
});

test('continuation premises preserve exact origins, individual observations and stable identities', () => {
  const pending = openingPvExample('continuation-support-frontier-awaiting');
  const completed = openingPvExample('continuation-support-frontier-complete');
  const before = pending
    .decode()
    .selection!.continuation_inquiries!.find(q => q.kinds.includes('support_application_not_observed'))!;
  const after = completed.decode().selection!.continuation_inquiries!.find(q => q.id === before.id)!;
  for (const premise of before.premises) {
    const retained = after.premises.find(value => value.id === premise.id)!;
    assert.ok(retained, premise.id);
    assert.equal(retained.kind, premise.kind);
    assert.equal(retained.requirement_id, premise.requirement_id);
    assert.deepEqual(retained.trigger_occurrence_ids, premise.trigger_occurrence_ids);
    assert.ok(premise.source_line_ids.every(id => retained.source_line_ids.includes(id)));
  }
  const observed = after.premises.find(p => p.kind === 'support_application_not_observed')!;
  assert.equal(observed.observation_state, 'observed');
  assert.ok(observed.witness_line_ids.length);
  assert.ok(
    observed.unresolved_source_line_ids.length,
    'a witness does not finish the other returned branches',
  );
  const raw = completed.response.observations.selection.continuation_inquiries.find(
    (q: Wire) => q.id === after.id,
  );
  assert.deepEqual(
    after.premises,
    raw.premises,
    'the decoder preserves per-premise bindings without flattening them',
  );
  assert.deepEqual([...new Set(after.premises.map(p => p.kind))].sort(), [...after.kinds].sort());
  assert.deepEqual(
    [...new Set(after.premises.flatMap(p => p.trigger_occurrence_ids))].sort(),
    [...after.trigger_occurrence_ids].sort(),
  );
});

test('retained material premises deliver unavailable context without claiming an observed outcome', () => {
  const sample = openingPvExample('continuation-material-context-unavailable');
  const observations = sample.decode();
  const inquiry = observations.selection!.continuation_inquiries!.find(q =>
    q.premises.some(p => p.kind === 'material_followup' && p.observation_state === 'unavailable'),
  )!;
  assert.ok(inquiry);
  const premise = inquiry.premises.find(p => p.kind === 'material_followup')!;
  assert.ok(premise.source_line_ids.length);
  assert.deepEqual(premise.witness_line_ids, []);
  assert.ok(premise.source_line_ids.every(id => observations.paths.some(path => path.id === id)));
  for (const claimed of ['observed', 'unobserved']) {
    const forged = openingPvExample('continuation-material-context-unavailable');
    const row = forged.response.observations.selection.continuation_inquiries.find(
      (q: Wire) => q.id === inquiry.id,
    );
    row.premises.find((p: Wire) => p.id === premise.id).observation_state = claimed;
    assert.equal(forged.decode().selection, undefined, claimed);
  }
});

test('root search records distinguish selected pools, returned candidates and stopped unbound reports', () => {
  const sample = openingPvExample();
  const observations = sample.decode();
  const searches = observations.selection!.root_searches!;
  assert.ok(searches.length);
  for (const search of searches) {
    const raw = sample.response.observations.selection.root_searches.find(
      (row: Wire) => row.work_id === search.work_id,
    );
    assert.deepEqual(search, raw);
    assert.ok(search.candidates.length <= search.multi_pv);
    for (const candidate of search.candidates) {
      assert.ok(!('evaluation' in candidate), 'scores stay on their existing admitted path');
      if (candidate.line_id !== null) {
        const path = observations.paths.find(path => path.id === candidate.line_id)!;
        assert.equal(path.moves[0].uci, candidate.move_uci);
        assert.equal(path.occurrenceIds[0], candidate.occurrence_id);
      }
    }
  }
  for (const mutation of [
    'wrong_root',
    'wrong_occurrence',
    'outside_pool',
    'completed_unbound',
    'half_binding',
    'duplicate_move',
  ]) {
    const forged = openingPvExample();
    const search = forged.response.observations.selection.root_searches.find(
      (s: Wire) => s.candidates.length,
    )!;
    if (mutation === 'wrong_root') search.root_position_id = 'unrelated-position';
    if (mutation === 'wrong_occurrence') search.candidates[0].occurrence_id = 'unrelated-occurrence';
    if (mutation === 'outside_pool') search.root_restriction = { kind: 'restricted', moves_uci: ['a1a8'] };
    if (mutation === 'completed_unbound') {
      search.candidates[0].line_id = null;
      search.candidates[0].occurrence_id = null;
    }
    if (mutation === 'half_binding') {
      search.state = 'stopped';
      search.stop_reason = 'invalid_engine_work_report';
      search.candidates[0].line_id = null;
    }
    if (mutation === 'duplicate_move') search.candidates[1] = { ...search.candidates[0] };
    assert.equal(forged.decode().selection, undefined, mutation);
  }
  const stopped = openingPvExample();
  const search = stopped.response.observations.selection.root_searches.find(
    (s: Wire) => s.candidates.length,
  )!;
  search.state = 'stopped';
  search.stop_reason = 'review_construction_failed';
  search.candidates[0].line_id = null;
  search.candidates[0].occurrence_id = null;
  const retained = stopped.decode().selection!.root_searches!.find(s => s.work_id === search.work_id)!;
  assert.equal(retained.state, 'stopped');
  assert.deepEqual(retained.candidates, search.candidates, 'a returned but unbound move remains visible');
});

test('premises reject reassigned kinds, triggers, origins and borrowed semantic observations', () => {
  const mutations: [string, string, (questions: Wire[], response: Wire) => void][] = [
    [
      'false unavailable',
      'continuation-danish-counterreply-awaiting',
      questions => {
        const q = questions.find(q => q.kinds.includes('material_followup'))!;
        q.premises.find((p: Wire) => p.kind === 'material_followup').observation_state = 'unavailable';
      },
    ],
    [
      'kind reassignment',
      'continuation-deep-complete',
      questions => {
        const q = questions.find(q =>
          q.premises.some((a: Wire) => q.premises.some((b: Wire) => a.kind !== b.kind)),
        )!;
        const a = q.premises[0],
          b = q.premises.find((p: Wire) => p.kind !== a.kind)!;
        [a.kind, b.kind] = [b.kind, a.kind];
      },
    ],
    [
      'trigger reassignment',
      'continuation-deep-complete',
      questions => {
        const q = questions.find(q =>
          q.premises.some((a: Wire) =>
            q.premises.some(
              (b: Wire) =>
                a.kind !== b.kind &&
                JSON.stringify(a.trigger_occurrence_ids) !== JSON.stringify(b.trigger_occurrence_ids),
            ),
          ),
        )!;
        const a = q.premises.find((a: Wire) =>
          q.premises.some(
            (b: Wire) =>
              a.kind !== b.kind &&
              JSON.stringify(a.trigger_occurrence_ids) !== JSON.stringify(b.trigger_occurrence_ids),
          ),
        )!;
        const b = q.premises.find(
          (b: Wire) =>
            a.kind !== b.kind &&
            JSON.stringify(a.trigger_occurrence_ids) !== JSON.stringify(b.trigger_occurrence_ids),
        )!;
        [a.trigger_occurrence_ids, b.trigger_occurrence_ids] = [
          b.trigger_occurrence_ids,
          a.trigger_occurrence_ids,
        ];
      },
    ],
    [
      'wrong origin',
      'continuation-support-frontier-complete',
      questions => {
        const q = questions.find(q => q.kinds.includes('support_application_not_observed'))!;
        const premise = q.premises.find((p: Wire) => p.kind === 'support_application_not_observed');
        premise.source_line_ids = [q.comparison_sources[premise.comparison_source_indices[0]].donor_line_id];
      },
    ],
    [
      'short peer as observed',
      'continuation-support-frontier-complete',
      questions => {
        const q = questions.find(q => q.kinds.includes('support_application_not_observed'))!;
        const premise = q.premises.find((p: Wire) => p.kind === 'support_application_not_observed');
        premise.witness_line_ids = [q.comparison_sources[premise.comparison_source_indices[0]].peer_line_id];
        premise.observation_state = 'observed';
      },
    ],
    [
      'unrelated witness',
      'continuation-support-frontier-complete',
      questions => {
        const q = questions.find(q => q.kinds.includes('support_application_not_observed'))!;
        const premise = q.premises.find((p: Wire) => p.kind === 'support_application_not_observed');
        premise.witness_line_ids = [q.comparison_sources[premise.comparison_source_indices[0]].donor_line_id];
      },
    ],
    [
      'wrong requirement',
      'continuation-support-frontier-complete',
      questions => {
        questions
          .find(q => q.kinds.includes('support_application_not_observed'))!
          .premises.find((p: Wire) => p.kind === 'support_application_not_observed').requirement_id =
          'unrelated-requirement';
      },
    ],
    [
      'false observed check',
      'continuation-check-endpoint-deferred',
      questions => {
        const q = questions.find(q => q.kinds.includes('check_response_not_observed'))!;
        const premise = q.premises.find((p: Wire) => p.kind === 'check_response_not_observed');
        premise.observation_state = 'observed';
        premise.witness_line_ids = [...premise.source_line_ids];
      },
    ],
  ];
  for (const [label, name, mutate] of mutations) {
    const forged = openingPvExample(name);
    mutate(forged.response.observations.selection.continuation_inquiries, forged.response);
    let selection;
    try {
      selection = forged.decode().selection;
    } catch (error) {
      assert.match(String(error), /Rejected runtime observation fixture/, label);
      continue;
    }
    assert.equal(selection, undefined, label);
  }
});

test('produced response comparisons keep child scores, physical identities and pinned recapture absence separate', () => {
  const pending = openingPvExample('response-awaiting');
  const answered = openingPvExample('response-answered');
  const observations = answered.decode();
  const inquiry = observations.selection!.response_inquiries!.find(inquiry => inquiry.state === 'answered')!;
  assert.equal(inquiry.related_inquiry_ids.length, 1);
  const peer = observations.selection!.response_inquiries!.find(
    peer => peer.id === inquiry.related_inquiry_ids[0],
  )!;
  assert.notEqual(peer.root_move, inquiry.root_move);
  assert.deepEqual(peer.original_target, inquiry.original_target);
  assert.equal(peer.root_occurrence_id, inquiry.root_occurrence_id);
  assert.deepEqual(peer.related_inquiry_ids, [inquiry.id]);
  assert.equal(inquiry.root_move, 'd2d3');
  assert.equal(inquiry.answer!.evaluation_relation, 'worse'); // Alternative +40 is worse for Black than -100.
  assert.deepEqual(answered.response.result, pending.response.assessment);
  for (const id of [inquiry.answer!.reference_line_id, inquiry.answer!.alternative_line_id]) {
    const path = observations.paths.find(path => path.id === id)!;
    assert.equal(path.positionIds![1], inquiry.answer!.search_position_id);
    assert.ok(
      path.conditionalEvaluations?.some(entry => entry.searchPositionId === inquiry.child_position_id),
    );
    assert.equal(path.evaluations?.some(e => e.source === 'response_comparison') ?? false, false);
  }
  const detail = observations
    .selection!.candidates.map(targetResponseDetail)
    .find(
      detail => detail?.response_inquiry_id === inquiry.id && detail.branches[0].observed_recapture.actor,
    )!;
  const capture = detail.branches[0];
  assert.equal(capture.target_state, 'captured');
  assert.equal(capture.observed_recapture.actor!.original.square, 'd2');
  assert.equal(capture.observed_recapture.actor!.current.square, 'e4');
  assert.equal(detail.original_target.square, 'e4');
  assert.equal(capture.observed_recapture.state, 'complete');
  assert.deepEqual(capture.observed_recapture.supported_targets, []);

  for (const name of ['response-observed', 'response-pinned-recapture']) {
    const selection = openingPvExample(name).decode().selection!;
    const reused = selection.response_inquiries!.find(inquiry =>
      name === 'response-pinned-recapture'
        ? inquiry.original_target.role === 'knight'
        : inquiry.state === 'observed',
    )!;
    if (name === 'response-observed') assert.equal(reused.answer, null);
    const candidate = selection.candidates.find(
      candidate => targetResponseDetail(candidate)?.response_inquiry_id === reused.id,
    )!;
    assert.equal(candidate.state, 'ready');
    if (name === 'response-pinned-recapture') {
      const branches = targetResponseDetail(candidate)!.branches;
      assert.equal(branches[0].recaptures.state, 'complete');
      assert.deepEqual(branches[0].recaptures.moves, []);
      assert.equal(branches[1].providers.state, 'complete');
      assert.ok(branches[1].providers.pieces.some(piece => piece.current.role === 'rook'));
    }
  }
  const stopped = openingPvExample('response-stopped').decode().selection!;
  const stoppedQuestion = stopped.response_inquiries!.find(
    inquiry => inquiry.state === 'stopped' && inquiry.root_move === 'd2d3',
  )!;
  assert.equal(stoppedQuestion.answer, null);
  const partial = stopped.candidates.find(
    candidate => targetResponseDetail(candidate)?.response_inquiry_id === stoppedQuestion.id,
  )!;
  assert.equal(partial.state, 'partial');
  const unfinishedCapture = targetResponseDetail(partial)!.branches[0];
  assert.equal(unfinishedCapture.recaptures.state, 'complete');
  assert.ok(unfinishedCapture.recaptures.moves.some(move => move.move_uci === 'd3e4'));
  assert.equal(unfinishedCapture.observed_recapture.state, 'not_observed');
  assert.deepEqual(unfinishedCapture.observed_recapture.supported_targets, []);
});

test('a target response accepts pressure at its exact child but rejects unrelated child control', () => {
  const example = openingPvExample('response-answered');
  const wire = example.response.observations;
  const question = wire.selection.response_inquiries.find((q: Wire) => q.root_move === 'd2d3');
  const projected = example.decode();
  const child = projected.positions.find(position => position.id === question.child_position_id)!;
  const attacking = child.controls.pieces.find(control => control.piece.side !== question.target.side &&
    control.squares.includes(question.target.square))!;
  const source = attacking.source_ids[attacking.squares.indexOf(question.target.square)];
  question.pressure_source_ids = [source];
  assert.ok(example.decode().selection?.response_inquiries?.find(q => q.id === question.id));
  const unrelated = child.controls.pieces.flatMap(control => control.source_ids.filter((_id, index) =>
    control.squares[index] !== question.target.square)).find(id => id !== source)!;
  question.pressure_source_ids = [unrelated];
  assert.equal(example.decode().selection, undefined);
});

test('response evidence cannot swap roots, score perspective, capture families or the pawn replacing a captured target', () => {
  const mutations: ((question: Wire, detail: Wire, observations: Wire) => void)[] = [
    question => {
      question.original_target.square = 'd2';
    },
    question => {
      question.related_inquiry_ids = [question.id];
    },
    question => {
      question.pressure_source_ids = [];
    },
    question => {
      question.capture_moves = [];
    },
    question => {
      question.decline_moves.push(question.reference_move);
    },
    question => {
      question.move_source_ids[question.reference_move] = '0'.repeat(64);
    },
    question => {
      question.answer.search_position_id = question.original_target.position_id;
    },
    question => {
      question.answer.ply_offset = 0;
    },
    question => {
      question.answer.evaluation_relation = 'better';
    },
    question => {
      question.answer.reference_occurrence_id = question.root_move_occurrence_id;
    },
    question => {
      question.answer.reference_evaluation.white_score.value = -99;
    },
    (_question, detail) => {
      detail.branches[0].observed_recapture.actor.original = structuredClone(detail.original_target);
    },
    (_question, detail) => {
      detail.branches[1].providers.source_ids = [];
    },
    (_question, detail) => {
      detail.branches[0].recaptures.moves = [];
    },
    (_question, detail) => {
      detail.branches[0].target_state = 'present';
      detail.branches[0].target = detail.branches[0].capturer;
    },
  ];
  for (const [index, mutate] of mutations.entries()) {
    const value = openingPvExample('response-answered');
    const raw = value.response.observations;
    const question = raw.selection.response_inquiries.find((inquiry: Wire) => inquiry.state === 'answered');
    const detail = raw.selection.candidates.find(
      (candidate: Wire) =>
        candidate.detail.response_inquiry_id === question.id &&
        candidate.detail.branches[0].observed_recapture.actor,
    ).detail;
    mutate(question, detail, raw);
    const decoded = value.decode();
    assert.ok(decoded.paths.length, String(index));
    assert.equal(decoded.selection, undefined, String(index));
  }
});

test('response work requires the exact child history and its admitted pending two-reply question', () => {
  for (const mutate of [
    (response: Wire) => {
      response.issued_engine_work.search_anchor.position_id =
        response.observations.selection.response_inquiries[0].original_target.position_id;
    },
    (response: Wire) => {
      response.issued_engine_work.purpose = 'investigation_comparison';
    },
    (response: Wire) => {
      delete response.issued_engine_work.search_anchor;
    },
    (response: Wire) => {
      response.observations.selection.response_inquiries = [];
    },
    (response: Wire) => {
      response.observations.selection.response_inquiries.find(
        (q: Wire) => q.state === 'awaiting_engine',
      ).state = 'deferred';
    },
    (response: Wire) => {
      response.issued_engine_work.engine_position_moves_uci.push('d8d7');
    },
  ]) {
    const value = openingPvExample('response-awaiting');
    assert.ok(value.decode().selection?.response_inquiries?.length);
    mutate(value.response);
    assert.throws(value.decode);
  }
});

function responseSearchExample() {
  const value = openingPvExample('response-answered');
  const selection = value.response.observations.selection;
  const question = selection.response_inquiries.find((question: Wire) => question.state === 'answered');
  for (const peer of selection.response_inquiries.filter((peer: Wire) => peer.id !== question.id)) {
    peer.answer = null;
    peer.state = 'observed';
    peer.stop_reason = null;
    delete peer.reference_selection;
    delete peer.alternative_selection;
  }
  const answer = question.answer;
  const search = {
    work_id: answer.work_id,
    execution_key_sha256: answer.execution_key_sha256,
    engine_profile: answer.engine_profile,
    required_depth: answer.required_depth,
    multi_pv: 2,
    root_move: question.root_move,
    root_move_occurrence_id: question.root_move_occurrence_id,
    child_position_id: question.child_position_id,
    root_restriction: { kind: 'restricted', moves_uci: [question.reference_move, question.alternative_move] },
    candidates: ['reference', 'alternative'].map(side => ({
      move_uci: question[`${side}_move`],
      line_id: answer[`${side}_line_id`],
      occurrence_id: answer[`${side}_occurrence_id`],
      evaluation: answer[`${side}_evaluation`],
    })),
  };
  selection.response_searches = [search];
  for (const side of ['reference', 'alternative'])
    question[`${side}_selection`] = {
      source: 'child_search',
      work_id: search.work_id,
      line_id: answer[`${side}_line_id`],
    };
  return { ...value, question, search };
}

test('produced continuation questions follow exact later choices and retain earlier bounded observations', () => {
  const pending = openingPvExample('continuation-deep-awaiting');
  const pendingSelection = pending.decode().selection!;
  const question = pendingSelection.continuation_inquiries!.find(
    question => question.state === 'awaiting_engine',
  )!;
  assert.deepEqual(question.prefix_moves_uci, ['d5c3', 'd1d3']);
  assert.deepEqual(
    pending.response.issued_engine_work.search_anchor.prefix_moves_uci,
    question.prefix_moves_uci,
  );
  const complete = openingPvExample('continuation-deep-complete');
  const observations = complete.decode(),
    selection = observations.selection!;
  assert.equal(selection.continuation_inquiries!.find(q => q.id === question.id)!.state, 'surveyed');
  const deep = selection.response_searches!.find(search => search.ply_offset === 2)!;
  assert.deepEqual(deep.prefix_moves_uci, question.prefix_moves_uci);
  assert.ok(
    deep.candidates.every(candidate => {
      const path = observations.paths.find(path => path.id === candidate.line_id)!;
      return (
        path.occurrenceIds[2] === candidate.occurrence_id &&
        path.conditionalEvaluations!.some(entry => entry.plyOffset === 2) &&
        !path.evaluations?.some(evaluation => evaluation.source === 'response_comparison')
      );
    }),
  );
  for (const candidate of pendingSelection.candidates.filter(candidate => lineContinuationDetail(candidate)))
    assert.ok(selection.candidates.some(later => later.id === candidate.id));
  const stopped = openingPvExample('continuation-deep-stopped').decode().selection!;
  assert.equal(stopped.continuation_inquiries!.find(q => q.id === question.id)!.state, 'stopped');
  const bare = openingPvExample('continuation-bare-capture').decode().selection!;
  assert.deepEqual(bare.continuation_inquiries![0].kinds, ['root_capture_response']);
  assert.deepEqual(bare.continuation_inquiries![0].observed_moves, []);
});

test('searched choices retain unresolved material branches and exact later questions instead of implying completion', () => {
  const value = openingPvExample('continuation-deep-complete');
  const selection = value.decode().selection!;
  const question = selection.continuation_inquiries!.find(
    q => q.state === 'surveyed' && q.unresolved_source_line_ids?.length && q.followup_question_ids?.length,
  )!;
  assert.ok(question, 'a searched choice still has unresolved material and further investigation');
  assert.ok(
    question.followup_question_ids!.every(id =>
      selection.continuation_inquiries!.some(
        next => next.id === id && next.prefix_moves_uci.length > question.prefix_moves_uci.length,
      ),
    ),
  );
  for (const change of ['omit_unresolved', 'borrow_line', 'self_followup', 'omit_followup', 'missing_half']) {
    const mutated = openingPvExample('continuation-deep-complete');
    const raw = mutated.response.observations.selection.continuation_inquiries.find(
      (q: Wire) => q.id === question.id,
    );
    if (change === 'omit_unresolved') raw.unresolved_source_line_ids = [];
    if (change === 'borrow_line')
      raw.unresolved_source_line_ids = [
        mutated.response.observations.paths.find((path: Wire) => !raw.source_line_ids.includes(path.id)).id,
      ];
    if (change === 'self_followup') raw.followup_question_ids = [raw.id];
    if (change === 'omit_followup') raw.followup_question_ids = [];
    if (change === 'missing_half') delete raw.unresolved_source_line_ids;
    assert.equal(mutated.decode().selection, undefined, change);
  }
});

test('short returned lines discover material and attacked-capturer choices without a supplied recovery line', () => {
  const pending = openingPvExample('continuation-discovery-awaiting');
  const observations = pending.decode();
  const selection = observations.selection!;
  const material = selection.continuation_inquiries!.find(q => q.kinds.includes('material_followup'))!;
  const capturer = selection.continuation_inquiries!.find(q => q.kinds.includes('capturer_reply'))!;
  assert.ok(material, 'a legal capture at the loss frontier supplies the question');
  assert.ok(capturer, 'opposing geometric control supplies the capturer choice');
  assert.deepEqual(material.comparison_sources, []);
  assert.deepEqual(capturer.comparison_sources, []);
  assert.ok(material.unresolved_source_line_ids?.length);
  assert.equal(
    selection.candidates
      .map(lineContinuationDetail)
      .filter(Boolean)
      .some(detail => detail!.capture_links.length || detail!.other_target_gains.length),
    false,
    'the question precedes an observed recovery',
  );
  for (const question of [material, capturer]) {
    const forged = openingPvExample('continuation-discovery-awaiting');
    const raw = forged.response.observations.selection.continuation_inquiries.find(
      (q: Wire) => q.id === question.id,
    );
    raw.kinds = [question === material ? 'capturer_reply' : 'material_followup'];
    assert.throws(
      forged.decode,
      /Rejected runtime observation fixture/,
      'a pending request cannot borrow another chess premise',
    );
  }
  const complete = openingPvExample('continuation-discovery-complete').decode().selection!;
  assert.equal(complete.continuation_inquiries!.find(q => q.id === material.id)!.state, 'surveyed');
  assert.ok(
    complete.candidates.map(lineContinuationDetail).some(detail => detail?.other_target_gains.length),
    'the requested continuation supplies a previously absent material observation',
  );
  const exchanged = complete.candidates
    .map(lineContinuationDetail)
    .find(detail => detail?.recovery_losses?.length)!;
  assert.ok(exchanged, 'the recovery performer can be captured as part of a balanced exchange');
  assert.deepEqual(exchanged.open_loss_event_ids, [], 'a balanced exchange has no active material deficit');
  assert.equal(
    complete
      .continuation_inquiries!.find(q => q.id === material.id)!
      .unresolved_source_line_ids!.includes(exchanged.line_id),
    false,
  );
  for (const change of ['removed_dependency', 'borrowed_dependency', 'reopened_loss']) {
    const forged = openingPvExample('continuation-discovery-complete');
    const detail = forged.response.observations.selection.candidates.find(
      (candidate: Wire) =>
        candidate.detail.type === 'line_continuation' && candidate.detail.line_id === exchanged.line_id,
    ).detail;
    if (change === 'removed_dependency') detail.recovery_losses = [];
    if (change === 'borrowed_dependency')
      detail.recovery_losses[0].later_loss_event_id = detail.recovery_losses[0].loss_event_id;
    if (change === 'reopened_loss') detail.open_loss_event_ids = [detail.recovery_losses[0].loss_event_id];
    assert.equal(forged.decode().selection, undefined, change);
  }
});

test('a Danish checking counterreply opens material investigation without a legal capture or supplied reply', () => {
  for (const name of [
    'continuation-danish-counterreply-awaiting',
    'continuation-danish-check-only-awaiting',
  ]) {
    const example = openingPvExample(name);
    const observations = example.decode();
    const question = observations.selection!.continuation_inquiries!.find(
      q => q.state === 'awaiting_engine',
    )!;
    assert.deepEqual(question.prefix_moves_uci, ['d5f7', 'e8f7', 'd1d8']);
    assert.ok(question.kinds.includes('material_followup'));
    assert.deepEqual(question.comparison_sources, []);
    assert.deepEqual(question.observed_moves, []);
    const position = observations.positions!.find(p => p.id === question.position_id)!;
    assert.equal(position.legal_moves.moves.find(m => m.uci === 'f8b4')!.gives_check, true);
    assert.equal(example.response.issued_engine_work.root_restriction.kind, 'unrestricted');
    if (name.includes('check-only')) {
      assert.ok(position.legal_moves.moves.every(m => m.captured_square === undefined));
      for (const missing of [false, true]) {
        const altered = openingPvExample(name);
        const point = altered.response.observations.positions.find((p: Wire) => p.id === position.id);
        for (const move of point.legal_moves.moves) {
          if (missing) delete move.gives_check;
          else move.gives_check = false;
        }
        assert.throws(
          altered.decode,
          /Rejected runtime observation fixture/,
          'a pending check-only premise requires positive check evidence',
        );
      }
    }
  }
});

test('continuation receipt, automatic trigger, physical capturer and later gain links reject borrowed evidence', () => {
  const mutations: ((response: Wire) => void)[] = [
    response => {
      response.observations.selection.continuation_inquiries[1].prefix_occurrence_ids.reverse();
    },
    response => {
      response.observations.selection.continuation_inquiries[1].kinds = ['root_capture_response'];
    },
    response => {
      response.observations.selection.continuation_inquiries[1].trigger_occurrence_ids = [
        response.observations.selection.continuation_inquiries[1].prefix_occurrence_ids[1],
      ];
    },
    response => {
      response.observations.selection.continuation_inquiries[1].search_work_ids = ['work:99'];
    },
    response => {
      response.observations.selection.response_searches.find((s: Wire) => s.ply_offset === 2).ply_offset = 1;
    },
    response => {
      response.observations.selection.response_searches.find(
        (s: Wire) => s.ply_offset === 2,
      ).candidates[0].occurrence_id =
        response.observations.selection.continuation_inquiries[1].root_occurrence_id;
    },
    response => {
      response.observations.selection.candidates.find(
        (c: Wire) => c.detail.capture_links?.length,
      ).detail.capture_links[0].elapsed_plies += 1;
    },
    response => {
      response.observations.selection.candidates.find(
        (c: Wire) => c.detail.capture_links?.length,
      ).detail.capture_links = [];
    },
    response => {
      const detail = response.observations.selection.candidates.find(
        (c: Wire) => c.detail.capture_links?.length,
      ).detail;
      detail.events[0].original_actor = detail.events[0].original_victim;
    },
    response => {
      const detail = response.observations.selection.candidates.find(
        (c: Wire) => c.detail.other_target_gains?.length,
      ).detail;
      detail.other_target_gains[0].gain_event_id = detail.other_target_gains[0].loss_event_id;
    },
    response => {
      const detail = response.observations.selection.candidates.find(
        (c: Wire) => c.detail.capture_links?.length,
      ).detail;
      detail.events[0].actor_end.state = 'present';
    },
  ];
  for (const [index, mutate] of mutations.entries()) {
    const value = openingPvExample('continuation-deep-complete');
    assert.ok(value.decode().selection);
    mutate(value.response);
    const observations = value.decode();
    assert.ok(observations.paths.length);
    assert.equal(observations.selection, undefined, String(index));
  }
});

test('paired continuation differences resolve physical subjects, exact capture points and complete support inventories', () => {
  for (const [name, kind] of [
    ['continuation-move-order', 'capture_timing'],
    ['continuation-move-order', 'legal_capture_availability'],
    ['response-answered', 'replacement_support'],
    ['response-stopped', 'root_actor_support'],
  ]) {
    const value = openingPvExample(name);
    const pair = value
      .decode()
      .selection!.candidates.map(continuationComparisonDetail)
      .find(detail => detail?.differences.some(difference => difference.kind === kind));
    assert.ok(pair, kind);
    const candidate = value.response.observations.selection.candidates.find(
      (c: Wire) =>
        c.detail.type === 'line_continuation_comparison' &&
        c.detail.differences.some((d: Wire) => d.kind === kind),
    );
    const difference = candidate.detail.differences.find((d: Wire) => d.kind === kind);
    if (kind === 'capture_timing') difference.left.event_id = difference.right.event_id;
    else if (kind === 'legal_capture_availability') difference.left.available = !difference.left.available;
    else difference.left.original_provider = difference.original_target;
    assert.equal(value.decode().selection, undefined, kind);
  }
  const value = openingPvExample('response-answered');
  const profile = value.response.observations.selection.candidates.find(
    (c: Wire) =>
      c.detail.type === 'line_continuation' &&
      c.detail.support_comparisons.some((s: Wire) => s.targets.length),
  );
  const comparison = profile.detail.support_comparisons.find((s: Wire) => s.targets.length);
  comparison.targets = [];
  assert.equal(
    value.decode().selection,
    undefined,
    'a complete observed positive support target cannot disappear',
  );
});

test('loss follow-ups distinguish another target gain from absence through the exact line end', () => {
  const value = openingPvExample('continuation-other-target');
  const pairs = value.decode().selection!.candidates.map(continuationComparisonDetail).filter(Boolean);
  const followups = pairs
    .flatMap(pair => pair!.differences)
    .filter(difference => difference.kind === 'loss_followup');
  assert.ok(followups.length);
  assert.ok(
    followups.some(
      difference => difference.left.other_target_gain_state !== difference.right.other_target_gain_state,
    ),
  );
  for (const change of ['scope', 'gain', 'identity', 'horizon']) {
    const value = openingPvExample('continuation-other-target');
    const pair = value.response.observations.selection.candidates.find((c: Wire) =>
      c.detail.differences?.some((d: Wire) => d.kind === 'loss_followup'),
    );
    const row = pair.detail.differences.find((d: Wire) => d.kind === 'loss_followup');
    if (change === 'scope') row.left.capturer_removal_state = 'impossible';
    if (change === 'gain')
      row.left.other_target_gain_state =
        row.left.other_target_gain_state === 'observed' ? 'not_observed_through_line_end' : 'observed';
    if (change === 'identity') row.original_actor = row.original_victim;
    if (change === 'horizon') row.left.observed_followup_plies += 1;
    assert.equal(value.decode().selection, undefined, change);
  }
});

test('capturing the recovery performer preserves the original loss question through its physical dependency', () => {
  const value = openingPvExample('continuation-recovery-relost');
  const selection = value.decode().selection!;
  const material = selection.continuation_inquiries!.find(
    q => q.kinds.includes('material_followup') && q.prefix_moves_uci.join(' ') === 'f3e5 d8d4 e5f3',
  )!;
  assert.equal(material.state, 'surveyed');
  const profile = selection.candidates
    .map(lineContinuationDetail)
    .find(detail =>
      detail?.recovery_losses?.some(link => detail.open_loss_event_ids?.includes(link.later_loss_event_id)),
    )!;
  assert.ok(profile, 'the canonical Kf1 Qe2+ Kxe2 branch contains the later queen loss');
  const link = profile.recovery_losses!.find(link =>
    profile.open_loss_event_ids!.includes(link.later_loss_event_id),
  )!;
  assert.equal(
    profile.events.find(event => event.id === link.later_loss_event_id)!.original_victim!.role,
    'queen',
  );
  assert.ok(
    !profile.open_loss_event_ids!.includes(link.loss_event_id),
    'the original pawn interval already closed',
  );
  assert.ok(
    material.unresolved_source_line_ids!.includes(profile.line_id),
    'the new loss keeps its original recovery premise unresolved',
  );
  const forged = openingPvExample('continuation-recovery-relost');
  const question = forged.response.observations.selection.continuation_inquiries.find(
    (q: Wire) => q.id === material.id,
  );
  question.unresolved_source_line_ids = question.unresolved_source_line_ids.filter(
    (id: string) => id !== profile.line_id,
  );
  assert.equal(
    forged.decode().selection,
    undefined,
    'a closed old interval alone cannot erase the later related deficit',
  );
});

test('a missing recovery-route class is searched in full and never compared using an older reference score', () => {
  const pending = openingPvExample('continuation-route-awaiting');
  const pendingSelection = pending.decode().selection!;
  const question = pendingSelection.continuation_inquiries!.find(
    q => q.route_challenge?.state === 'awaiting_engine',
  )!;
  assert.equal(
    question.state,
    'surveyed',
    'the completed full survey remains separate from its missing route sample',
  );
  assert.equal(
    question.route_challenge!.move_classes[0].moves_uci.length,
    8,
    'all legal Ne5 moves are included',
  );
  assert.equal(pending.response.issued_engine_work.root_restriction.moves_uci.length, 9);
  assert.equal(
    pending.response.issued_engine_work.search_limits.multi_pv,
    3,
    'scope size is not the number of returned PVs',
  );
  const sampled = openingPvExample('continuation-route-sampled');
  const sampleQ = sampled.decode().selection!.continuation_inquiries!.find(q => q.id === question.id)!;
  assert.equal(sampleQ.state, 'surveyed');
  assert.equal(sampleQ.route_challenge!.state, 'sampled');
  assert.equal(sampleQ.route_challenge!.move_classes[0].comparison, null, 'R is absent from this work');
  assert.equal(sampleQ.route_challenge!.stop_reason, 'investigation_limit');
  const compared = openingPvExample('continuation-route-compared');
  const comparedSelection = compared.decode().selection!;
  const comparedQ = comparedSelection.continuation_inquiries!.find(q => q.id === question.id)!;
  const route = comparedQ.route_challenge!,
    comparison = route.move_classes[0].comparison!;
  assert.equal(route.state, 'compared');
  const receipt = comparedSelection.response_searches!.find(search => search.work_id === comparison.work_id)!;
  assert.ok(receipt.candidates.some(candidate => candidate.line_id === comparison.reference_line_id));
  assert.equal(
    receipt.candidates.find(candidate => route.move_classes[0].moves_uci.includes(candidate.move_uci))!
      .line_id,
    comparison.challenge_line_id,
  );
  const cancelled = openingPvExample('continuation-route-cancelled')
    .decode()
    .selection!.continuation_inquiries!.find(q => q.id === question.id)!;
  assert.equal(cancelled.state, 'surveyed');
  assert.equal(cancelled.route_challenge!.state, 'stopped');
  for (const change of ['cut_class', 'source', 'old_work', 'wrong_representative', 'invented_rank']) {
    const forged = openingPvExample('continuation-route-compared');
    const raw = forged.response.observations.selection.continuation_inquiries.find(
      (q: Wire) => q.id === question.id,
    ).route_challenge;
    if (change === 'cut_class') raw.move_classes[0].moves_uci.pop();
    if (change === 'source') raw.move_classes[0].premise_occurrence_ids = [question.prefix_occurrence_ids[0]];
    if (change === 'old_work') raw.move_classes[0].comparison.work_id = raw.reference_work_id;
    if (change === 'wrong_representative')
      raw.move_classes[0].comparison.challenge_line_id = receipt.candidates[1].line_id;
    if (change === 'invented_rank') raw.best_reply = true;
    assert.equal(forged.decode().selection, undefined, change);
  }
  const borrowed = openingPvExample('continuation-route-sampled');
  const rawQ = borrowed.response.observations.selection.continuation_inquiries.find(
    (q: Wire) => q.id === question.id,
  ).route_challenge;
  const rawReceipt = borrowed.response.observations.selection.response_searches.find(
    (s: Wire) => s.work_id === rawQ.move_classes[0].sampled_work_ids[0],
  );
  rawQ.state = 'compared';
  rawQ.stop_reason = null;
  rawQ.move_classes[0].comparison = {
    work_id: rawReceipt.work_id,
    reference_line_id: rawQ.reference_line_id,
    challenge_line_id: rawReceipt.candidates[0].line_id,
  };
  assert.equal(
    borrowed.decode().selection,
    undefined,
    'the old R path is not an evaluation in this new work',
  );
  for (const change of ['cut_pool', 'one_pv_per_move']) {
    const forged = openingPvExample('continuation-route-awaiting');
    const work = forged.response.issued_engine_work;
    if (change === 'cut_pool') work.root_restriction.moves_uci.pop();
    else work.search_limits.multi_pv = work.root_restriction.moves_uci.length;
    assert.throws(forged.decode, /Rejected runtime observation fixture/, change);
  }
});

test('later king-capture constraints discover replacement choices and compare recapturers even under a quiet search leader', () => {
  const discovered = openingPvExample('continuation-support-discovery-awaiting');
  const first = discovered.decode().selection!;
  const question = first.continuation_inquiries!.find(q => q.kinds.includes('replacement_function_choice'))!;
  assert.deepEqual(question.prefix_moves_uci, ['g1h1', 'd5e4']);
  assert.equal(question.state, 'awaiting_engine');
  assert.ok(
    first.candidates.map(lineContinuationDetail).some(detail => detail?.support_applications?.length),
    'one recorded use supplies the question before another recapture line is available',
  );
  const missing = openingPvExample('continuation-support-class-awaiting');
  const missingQ = missing.decode().selection!.continuation_inquiries!.find(q => q.id === question.id)!;
  const classes = missingQ.route_challenge!.move_classes.filter(
    group => group.kind === 'replacement_recapture',
  );
  assert.deepEqual(classes.map(group => group.moves_uci).sort(), [['d2e4'], ['f3e4']]);
  assert.ok(classes.every(group => !group.sampled_work_ids.length));
  assert.equal(missingQ.state, 'surveyed');
  assert.deepEqual([...missing.response.issued_engine_work.root_restriction.moves_uci].sort(), [
    'd2e4',
    'f3e4',
    'h1h2',
  ]);
  const complete = openingPvExample('continuation-support-application');
  const selection = complete.decode().selection!;
  const q = selection.continuation_inquiries!.find(q => q.id === question.id)!;
  assert.equal(q.route_challenge!.state, 'compared');
  const search = selection.response_searches!.find(
    search =>
      search.work_id ===
      q.route_challenge!.move_classes.find(group => group.kind === 'replacement_recapture')!.comparison!
        .work_id,
  )!;
  assert.equal(search.candidates[0].move_uci, 'h1h2', 'the top returned line is a quiet king move');
  const recaptureLines = search.candidates
    .filter(candidate => ['d2e4', 'f3e4'].includes(candidate.move_uci))
    .map(candidate => candidate.line_id);
  const pair = selection.candidates.find(
    candidate =>
      candidate.lineIds.length === 2 &&
      recaptureLines.every(id => candidate.lineIds.includes(id)) &&
      continuationComparisonDetail(candidate),
  )!;
  assert.ok(pair, 'the two recapturers receive their own comparison beyond the quiet-leader star');
  const row = continuationComparisonDetail(pair)!.differences.find(
    difference => difference.kind === 'replacement_support' && difference.left.application,
  )!;
  assert.ok(row);
  if (row.kind !== 'replacement_support') return;
  assert.equal(row.original_target.square, 'g5');
  assert.deepEqual([row.left, row.right].map(point => point.application!.moves_uci.length).sort(), [0, 1]);
  for (const point of [row.left, row.right]) {
    assert.equal(point.application!.attacker.square, 'g6');
    assert.equal(point.application!.target.square, 'g5');
    assert.ok(point.ply_count > 3, 'the use lies beyond the immediate recapture');
  }
  for (const change of ['nonking', 'contact', 'capture', 'support', 'position', 'missing_side']) {
    const forged = openingPvExample('continuation-support-application');
    const difference = forged.response.observations.selection.candidates
      .find((candidate: Wire) => candidate.id === pair.id)
      .detail.differences.find(
        (difference: Wire) => difference.kind === 'replacement_support' && difference.left.application,
      );
    if (change === 'nonking')
      difference.left.application.original_attacker = difference.left.original_provider;
    if (change === 'contact')
      difference.left.application.contact_source_ids = difference.left.support.source_ids;
    if (change === 'capture')
      [difference.left.application.moves_uci, difference.right.application.moves_uci] = [
        difference.right.application.moves_uci,
        difference.left.application.moves_uci,
      ];
    if (change === 'support') difference.left.support = difference.right.support;
    if (change === 'position')
      difference.left.application.attacker.position_id = difference.right.position_id;
    if (change === 'missing_side') delete difference.left.application;
    assert.equal(forged.decode().selection, undefined, change);
  }
  for (const change of ['merge_recapturers', 'wrong_kind', 'false_use']) {
    const forged = openingPvExample('continuation-support-class-awaiting');
    const raw = forged.response.observations.selection;
    const route = raw.continuation_inquiries.find((q: Wire) => q.id === question.id).route_challenge;
    if (change === 'merge_recapturers') {
      route.move_classes[0].moves_uci = ['d2e4', 'f3e4'];
      route.move_classes.splice(1, 1);
    }
    if (change === 'wrong_kind') route.move_classes[0].kind = 'recovery_route';
    if (change === 'false_use')
      raw.candidates.find(
        (candidate: Wire) => candidate.detail.support_applications?.length,
      ).detail.support_applications[0].application.legal_inventory_complete = false;
    assert.throws(forged.decode, /Rejected runtime observation fixture/, change);
  }
});

test('a searched recapture remains unresolved when only its peer has a later support-use observation', () => {
  const example = openingPvExample('continuation-support-use-unobserved');
  const selection = example.decode().selection!;
  const q = selection.continuation_inquiries!.find(q => q.kinds.includes('replacement_function_choice'))!;
  assert.equal(q.state, 'surveyed');
  assert.equal(q.route_challenge!.state, 'compared');
  const search = selection.response_searches!.find(search => search.root_restriction.kind === 'restricted')!;
  const pawn = search.candidates.find(candidate => candidate.move_uci === 'f3e4')!;
  const knight = search.candidates.find(candidate => candidate.move_uci === 'd2e4')!;
  assert.ok(
    q.unresolved_source_line_ids!.includes(pawn.line_id),
    'a short pawn line cannot borrow the knight support use',
  );
  assert.ok(
    !q.unresolved_source_line_ids!.includes(knight.line_id),
    'the exact longer knight descendant is reused',
  );
  for (const change of ['hide_pawn', 'borrow_donor', 'premature_observed']) {
    const forged = openingPvExample('continuation-support-use-unobserved');
    const raw = forged.response.observations.selection;
    const inquiry = raw.continuation_inquiries.find((value: Wire) => value.id === q.id);
    if (change === 'hide_pawn')
      inquiry.unresolved_source_line_ids = inquiry.unresolved_source_line_ids.filter(
        (id: string) => id !== pawn.line_id,
      );
    if (change === 'premature_observed') inquiry.state = 'observed';
    if (change === 'borrow_donor') {
      const donor = raw.candidates.find(
        (candidate: Wire) => candidate.detail.support_applications?.length,
      ).detail;
      const peer = raw.candidates.find((candidate: Wire) => candidate.detail.line_id === pawn.line_id).detail;
      peer.support_applications = donor.support_applications;
    }
    assert.equal(forged.decode().selection, undefined, change);
  }
});

test('a support frontier keeps the returned donor, its exact later observation and the peer investigation separate', () => {
  const pending = openingPvExample('continuation-support-frontier-awaiting');
  const selection = pending.decode().selection!;
  const q = selection.continuation_inquiries!.find(q =>
    q.kinds.includes('support_application_not_observed'),
  )!;
  assert.equal(q.state, 'awaiting_engine');
  assert.deepEqual(q.prefix_moves_uci, ['g1h1', 'd5e4', 'f3e4']);
  const source = q.comparison_sources!.find(source => source.kind === 'support_application')!;
  assert.notEqual(
    source.donor_line_id,
    source.observation_line_id,
    'the returned knight line is shorter than its independent exact descendant',
  );
  assert.ok(
    !q.source_line_ids.includes(source.observation_line_id!),
    'the donor observation is not a peer continuation',
  );
  const search = selection.response_searches!.find(search => search.work_id === source.work_id)!;
  assert.equal(search.candidates[0].move_uci, 'h1h2');
  assert.ok(
    ![source.donor_line_id, source.peer_line_id].includes(search.candidates[0].line_id),
    'the recapturer pair is independent of the quiet leader',
  );
  assert.equal(
    pending.response.issued_engine_work.search_anchor.prefix_moves_uci.join(' '),
    q.prefix_moves_uci.join(' '),
  );
  const completed = openingPvExample('continuation-support-frontier-complete');
  const final = completed.decode().selection!;
  const done = final.continuation_inquiries!.find(value => value.id === q.id)!;
  assert.equal(done.state, 'surveyed');
  const finalPaths = completed.decode().paths;
  const unresolved = done.unresolved_source_line_ids!.map(id => finalPaths.find(path => path.id === id)!);
  assert.deepEqual(unresolved.map(path => path.moves[q.prefix_moves_uci.length].uci).sort(), [
    'h8g8',
    'h8h7',
  ]);
  assert.ok(
    !done.unresolved_source_line_ids!.includes(source.peer_line_id),
    'the exact longer Kg7 branch supplies the named use without completing the other branches',
  );
  for (const change of ['peer_as_observation', 'wrong_work', 'swap_returned', 'erase_kind']) {
    const forged = openingPvExample('continuation-support-frontier-awaiting');
    const inquiry = forged.response.observations.selection.continuation_inquiries.find(
      (value: Wire) => value.id === q.id,
    );
    const value = inquiry.comparison_sources[0];
    if (change === 'peer_as_observation') value.observation_line_id = value.peer_line_id;
    if (change === 'wrong_work') value.work_id = 'work:0';
    if (change === 'swap_returned')
      [value.donor_line_id, value.peer_line_id] = [value.peer_line_id, value.donor_line_id];
    if (change === 'erase_kind') {
      delete value.kind;
      value.observation_line_id = null;
    }
    assert.throws(forged.decode, /Rejected runtime observation fixture/, change);
  }
  const hidden = openingPvExample('continuation-support-frontier-complete');
  hidden.response.observations.selection.continuation_inquiries
    .find((value: Wire) => value.id === q.id)
    .unresolved_source_line_ids.pop();
  assert.equal(
    hidden.decode().selection,
    undefined,
    'a searched short branch does not lose its unobserved support use',
  );
});

test('one attacker links two observed recovery targets only at their exact shared branch point', () => {
  const sample = openingPvExample('continuation-other-target');
  const selection = sample.decode().selection!;
  const candidate = selection.candidates.find(candidate =>
    continuationComparisonDetail(candidate)?.differences.some(
      difference => difference.kind === 'loss_followup' && difference.shared_attackers?.length,
    ),
  )!;
  assert.ok(candidate, 'the Ruy branch connects Qd4 control to actual Ne5 and Pe4 captures');
  const difference = continuationComparisonDetail(candidate)!.differences.find(
    difference => difference.kind === 'loss_followup' && difference.shared_attackers?.length,
  )!;
  assert.equal(difference.kind, 'loss_followup');
  if (difference.kind !== 'loss_followup') return;
  const shared = difference.shared_attackers![0];
  assert.equal(shared.actor.role, 'queen');
  assert.equal(shared.actor.square, 'd4');
  assert.deepEqual([shared.left.target.square, shared.right.target.square].sort(), ['e4', 'e5']);
  for (const change of ['position', 'source', 'target', 'event', 'actor', 'forced']) {
    const forged = openingPvExample('continuation-other-target');
    const row = forged.response.observations.selection.candidates
      .find((value: Wire) => value.id === candidate.id)
      .detail.differences.find(
        (value: Wire) => value.kind === 'loss_followup' && value.shared_attackers?.length,
      ).shared_attackers[0];
    if (change === 'position') row.ply_count++;
    if (change === 'source') row.left.source_ids = row.right.source_ids;
    if (change === 'target') row.left.target = row.right.target;
    if (change === 'event') row.left.event_id = row.right.event_id;
    if (change === 'actor') row.actor.square = 'c5';
    if (change === 'forced') row.forced_fork = true;
    assert.equal(forged.decode().selection, undefined, change);
  }
});

test('vacated blockers expose a legal response without declaring a profitable capture or skipping the other arm', () => {
  const first = openingPvExample('continuation-exposure-awaiting');
  const selection = first.decode().selection!;
  assert.ok(
    selection.continuation_inquiries!.some(
      q => q.kinds.includes('capture_exposure_response') && q.state === 'awaiting_engine',
    ),
  );
  const profile = selection.candidates
    .map(lineContinuationDetail)
    .find(detail => detail?.capture_exposures?.length)!;
  const exposure = profile.capture_exposures![0];
  assert.ok(exposure.vacated_blockers.length > 0);
  assert.ok(exposure.moves_uci.length > 0);
  const other = openingPvExample('continuation-exposure-other-arm');
  const next = other.decode().selection!;
  const missing = next.continuation_inquiries!.find(q =>
    q.route_challenge?.move_classes.some(
      group => group.kind === 'exposed_capture' && !group.sampled_work_ids.length,
    ),
  )!;
  assert.ok(missing, 'an omitted exposed capture keeps its complete class');
  assert.equal(other.response.issued_engine_work.root_restriction.kind, 'unrestricted');
  assert.notEqual(
    other.response.issued_engine_work.search_anchor.position_id,
    missing.position_id,
    'the second selected arm is surveyed before spending the remaining slot on the first exposed class',
  );
  const completed = openingPvExample('continuation-exposure-complete').decode().selection!;
  assert.ok(
    completed.continuation_inquiries!.some(q => q.kinds.includes('exposure_prevention_choice')),
    'an actual exposed capture with an open loss revisits its preceding choice',
  );
  for (const change of ['ray', 'blocker', 'legal_move', 'position', 'profit']) {
    const forged = openingPvExample('continuation-exposure-awaiting');
    const value = forged.response.observations.selection.candidates.find(
      (candidate: Wire) => candidate.detail.line_id === profile.line_id,
    ).detail.capture_exposures[0];
    if (change === 'ray') value.ray_source_id = value.control_source_ids[0];
    if (change === 'blocker') value.vacated_blockers[0].before = value.target;
    if (change === 'legal_move') value.moves_uci = ['a1a8'];
    if (change === 'position') value.attacker.position_id = value.before_position_id;
    if (change === 'profit') value.profitable = true;
    assert.throws(forged.decode, /Rejected runtime observation fixture/, change);
  }
});

test('direct support needs an actual recapture, while another physical recapturer can answer the later question', () => {
  const first = openingPvExample('continuation-direct-support-awaiting');
  const selection = first.decode().selection!;
  const profile = selection.candidates
    .map(lineContinuationDetail)
    .find(detail => detail?.direct_support_recaptures?.length)!;
  assert.ok(profile, 'a witnessed supporting recapture supplies the original choice question');
  const use = profile.direct_support_recaptures![0];
  assert.equal(use.original_target.square, 'g5');
  assert.ok(selection.continuation_inquiries!.some(q => q.kinds.includes('replacement_function_choice')));
  const pending = openingPvExample('continuation-direct-support-frontier-awaiting').decode().selection!;
  const question = pending.continuation_inquiries!.find(q =>
    q.kinds.includes('support_application_not_observed'),
  )!;
  assert.equal(question.state, 'awaiting_engine');
  const lost = openingPvExample('continuation-direct-support-target-lost').decode();
  const lostQ = lost.selection!.continuation_inquiries!.find(q => q.id === question.id)!;
  assert.ok(
    lostQ.unresolved_source_line_ids!.some(id => {
      const detail = lost
        .selection!.candidates.map(lineContinuationDetail)
        .find(detail => detail?.line_id === id);
      return detail?.events.some(
        event =>
          event.original_victim?.square === 'g5' &&
          !detail.capture_links.some(link => link.loss_event_id === event.id),
      );
    }),
    'the target loss alone is not the supporting recapture',
  );
  const recovered = openingPvExample('continuation-direct-support-recaptured').decode().selection!;
  const answered = recovered.continuation_inquiries!.find(q => q.id === question.id)!;
  assert.ok(
    answered.source_line_ids.some(id => {
      const detail = recovered.candidates.map(lineContinuationDetail).find(detail => detail?.line_id === id);
      return detail?.events.some(
        loss =>
          loss.original_victim?.square === 'g5' &&
          detail.capture_links.some(link => {
            const recapture = detail.events.find(event => event.id === link.capture_event_id);
            return link.loss_event_id === loss.id && recapture?.original_actor?.square === 'd2';
          }),
      );
    }),
    'the exact pawn branch can recover through the other original knight',
  );
  for (const change of ['capturer', 'loss', 'support', 'provider', 'forced']) {
    const forged = openingPvExample('continuation-direct-support-awaiting');
    const value = forged.response.observations.selection.candidates.find(
      (candidate: Wire) => candidate.detail.line_id === profile.line_id,
    ).detail.direct_support_recaptures[0];
    if (change === 'capturer') value.capturer = value.target;
    if (change === 'loss') value.loss_event_id = value.recapture_event_id;
    if (change === 'support') value.support.source_ids = [];
    if (change === 'provider') value.original_provider = value.original_target;
    if (change === 'forced') value.forced = true;
    assert.throws(forged.decode, /Rejected runtime observation fixture/, change);
  }
});

test('the same potential recapturer retains or loses support for a different original friendly target', () => {
  const example = openingPvExample('continuation-support-application');
  const selection = example.decode().selection!;
  const pair = selection.candidates
    .map(continuationComparisonDetail)
    .find(detail =>
      detail?.differences.some(
        row =>
          row.kind === 'recapturer_support' &&
          row.original_subject.square === 'd2' &&
          row.original_target.square === 'f3',
      ),
    )!;
  assert.ok(pair);
  const row = pair.differences.find(
    row =>
      row.kind === 'recapturer_support' &&
      row.original_subject.square === 'd2' &&
      row.original_target.square === 'f3',
  )!;
  if (row.kind !== 'recapturer_support') return;
  assert.deepEqual(
    [row.left, row.right].map(side => [side.before.support.state, side.after.support.state]).sort(),
    [
      ['present', 'absent'],
      ['present', 'present'],
    ],
  );
  assert.ok(
    pair.continuation_candidate_ids.every(id => {
      const profile = lineContinuationDetail(selection.candidates.find(candidate => candidate.id === id)!)!;
      return !profile.support_comparisons.some(
        comparison =>
          row.matched_support_comparison_ids.includes(comparison.id) &&
          comparison.targets.some(target => target.original_target.square === 'f3'),
      );
    }),
    'the fixed-knight contrast is not limited to the replacement providers’ old target lists',
  );
  for (const change of ['before', 'source', 'subject', 'unchanged']) {
    const forged = openingPvExample('continuation-support-application');
    const value = forged.response.observations.selection.candidates
      .flatMap((candidate: Wire) => candidate.detail.differences ?? [])
      .find(
        (value: Wire) =>
          value.kind === 'recapturer_support' &&
          value.original_subject.square === 'd2' &&
          value.original_target.square === 'f3',
      );
    if (change === 'before') value.left.before = value.left.after;
    if (change === 'source') value.left.before.support.source_ids = [];
    if (change === 'subject') value.original_subject = value.original_target;
    if (change === 'unchanged') value.left.after.support = value.left.before.support;
    assert.equal(forged.decode().selection, undefined, change);
  }
});

test('a supplied support continuation cannot displace current replies, and a quiet single move can expose a capture', () => {
  for (const [name, prefix] of [
    ['continuation-supplied-support-current-reply', 'g1h1'],
    ['continuation-supplied-support-other-current-arm', 'g1h2'],
  ]) {
    const example = openingPvExample(name);
    const selection = example.decode().selection!;
    assert.deepEqual(example.response.issued_engine_work.search_anchor.prefix_moves_uci, prefix.split(' '));
    if (name.endsWith('other-current-arm'))
      assert.ok(
        selection.response_inquiries!.some(q => q.state === 'awaiting_engine' && q.root_move === 'g1h2'),
        'the other current root retains its pending response inquiry',
      );
    assert.ok(
      selection.candidates
        .map(lineContinuationDetail)
        .some(detail => detail?.direct_support_recaptures?.length),
    );
    assert.ok(
      selection.continuation_inquiries!.some(
        q => q.kinds.includes('replacement_function_choice') && q.state === 'deferred',
      ),
      'the supplied conditional premise remains available without outranking the current reply investigation',
    );
  }
  const quiet = openingPvExample('continuation-quiet-exposure-awaiting');
  const selection = quiet.decode().selection!;
  const q = selection.continuation_inquiries!.find(q => q.state === 'awaiting_engine')!;
  assert.deepEqual(q.prefix_moves_uci, ['d2c4']);
  assert.ok(q.kinds.includes('capture_exposure_response'));
  const profile = selection.candidates
    .map(lineContinuationDetail)
    .find(detail => detail?.capture_exposures?.some(exposure => exposure.moves_uci.includes('d8d1')))!;
  assert.deepEqual(
    profile.events,
    [],
    'the available capture is discovered without a supplied capture suffix',
  );
  assert.equal(profile.capture_exposures![0].prefix_moves_uci.length, 1);
});

test('functional comparisons share one unordered line pair while retaining their own exact left and right evidence', () => {
  const value = openingPvExample('continuation-deep-complete');
  const candidates = value.response.observations.selection.candidates as Wire[];
  const pairs = candidates.filter(candidate => candidate.detail.type === 'line_continuation_comparison');
  const keys = pairs.map(pair => [...pair.line_ids].sort().join('|'));
  assert.equal(new Set(keys).size, pairs.length, 'one card per physical line pair');
  const reversedParent = candidates.find(parent => parent.detail.continuation_comparison_ids?.length)!;
  assert.ok(reversedParent, 'the producer links a physical comparison to an assessment or response');
  const linked = pairs.find(pair => reversedParent.detail.continuation_comparison_ids.includes(pair.id))!;
  linked.line_ids.reverse();
  linked.detail.continuation_candidate_ids.reverse();
  for (const difference of linked.detail.differences) {
    [difference.left, difference.right] = [difference.right, difference.left];
    difference.matched_capture_event_ids?.reverse();
    for (const shared of difference.shared_attackers ?? [])
      [shared.left, shared.right] = [shared.right, shared.left];
  }
  assert.ok(
    value.decode().selection,
    'a coherent reversed pair is shared without reversing the parent assessment',
  );
  const unrelated = pairs.find(pair =>
    pair.line_ids.some((id: string) => !reversedParent.line_ids.includes(id)),
  )!;
  assert.ok(unrelated);
  reversedParent.detail.continuation_comparison_ids = [unrelated.id];
  assert.equal(value.decode().selection, undefined, 'another pair cannot be borrowed');
  reversedParent.detail.continuation_comparison_ids = [linked.id];
  linked.line_ids.reverse();
  assert.equal(value.decode().selection, undefined, 'the pair cannot reverse only its outer line IDs');
});

test('unobserved frontiers require one same-search comparison and do not become observed without the missing capture', () => {
  const fixtures = JSON.parse(
    readFileSync(new URL('commentary-investigation-produced.json', fixtureRoot), 'utf8'),
  ) as Wire[];
  const source = fixtures.find(
    fixture =>
      !fixture.response.issued_engine_work &&
      fixture.response.observations.selection.continuation_inquiries?.some(
        (q: Wire) => q.kinds.includes('continuation_not_observed') && q.state !== 'observed',
      ),
  )!;
  assert.ok(source, 'a stopped or completed producer retains an unobserved frontier');
  for (const change of ['work', 'donor', 'observed']) {
    const value = openingPvExample(source.name);
    assert.ok(value.decode().selection);
    const question = value.response.observations.selection.continuation_inquiries.find(
      (q: Wire) => q.kinds.includes('continuation_not_observed') && q.state !== 'observed',
    );
    assert.ok(question.comparison_sources.length);
    if (change === 'work') question.comparison_sources[0].work_id = 'work:99';
    if (change === 'donor')
      question.comparison_sources[0].donor_line_id = question.comparison_sources[0].peer_line_id;
    if (change === 'observed') {
      question.state = 'observed';
      question.stop_reason = null;
    }
    assert.equal(value.decode().selection, undefined, change);
  }
});

test('shared response searches bind selected replies to one child receipt and preserve finite search scope', () => {
  const accepted = responseSearchExample().decode().selection!;
  assert.equal(accepted.response_searches!.length, 1);
  assert.equal(accepted.response_inquiries!.find(q => q.answer)!.reference_selection!.source, 'child_search');
  const mutations: ((value: ReturnType<typeof responseSearchExample>) => void)[] = [
    ({ search }) => {
      search.child_position_id = 'another-child';
    },
    ({ search }) => {
      search.candidates.reverse();
    },
    ({ search }) => {
      search.candidates[0].occurrence_id = search.root_move_occurrence_id;
    },
    ({ search }) => {
      search.multi_pv = 3;
    },
    ({ search }) => {
      search.candidates[0].evaluation = { ...search.candidates[0].evaluation, depth: 15 };
    },
    ({ question }) => {
      question.reference_selection.work_id = 'work:99';
    },
    ({ question }) => {
      question.reference_selection.line_id = question.answer.alternative_line_id;
    },
    ({ question }) => {
      question.reference_selection.source = 'legal_seed';
    },
    ({ question }) => {
      delete question.reference_selection;
    },
    ({ search }) => {
      search.root_restriction.moves_uci.push('g8h8');
    },
    ({ response, search }) => {
      const path = response.observations.paths.find((path: Wire) => path.id === search.candidates[0].line_id);
      delete path.conditional_evaluations;
    },
  ];
  for (const [index, mutate] of mutations.entries()) {
    const value = responseSearchExample();
    mutate(value);
    const observations = value.decode();
    assert.ok(observations.paths.length);
    assert.equal(observations.selection, undefined, String(index));
  }
  const latest = responseSearchExample();
  const newer = structuredClone(latest.search);
  newer.work_id = 'work:3';
  newer.execution_key_sha256 = 'f'.repeat(64);
  newer.root_restriction.moves_uci = [...latest.question.capture_moves];
  newer.multi_pv = 1;
  newer.candidates = [newer.candidates[0]];
  latest.response.observations.selection.response_searches.push(newer);
  assert.equal(
    latest.decode().selection,
    undefined,
    'the newer complete class search owns its representative',
  );
  latest.question.reference_selection.work_id = newer.work_id;
  assert.ok(latest.decode().selection);
});

test('exact search prefixes preserve legacy one-ply receipts and reject mismatched offsets or history', () => {
  const legacy = responseSearchExample();
  assert.ok(legacy.decode().selection);
  const explicit = responseSearchExample();
  Object.assign(explicit.search, { prefix_moves_uci: [explicit.search.root_move], ply_offset: 1 });
  assert.ok(explicit.decode().selection);
  for (const mutate of [
    (search: Wire) => {
      search.prefix_moves_uci = [];
    },
    (search: Wire) => {
      search.prefix_moves_uci = ['f1d3'];
    },
    (search: Wire) => {
      search.prefix_moves_uci.push(search.candidates[0].move_uci);
    },
    (search: Wire) => {
      search.ply_offset = 2;
    },
    (search: Wire) => {
      delete search.ply_offset;
    },
    (search: Wire) => {
      delete search.prefix_moves_uci;
    },
  ]) {
    const value = responseSearchExample();
    Object.assign(value.search, { prefix_moves_uci: [value.search.root_move], ply_offset: 1 });
    mutate(value.search);
    assert.equal(value.decode().selection, undefined);
  }
  const pending = openingPvExample('response-awaiting');
  const anchor = pending.response.issued_engine_work.search_anchor;
  anchor.prefix_moves_uci = [anchor.root_move];
  assert.ok(pending.decode().selection);
  anchor.prefix_moves_uci.push('d5e4');
  assert.throws(pending.decode, 'an unrequested later position is not an immediate-response work');
});

test('conditional scores use their exact later position and suffix without changing the root assessment', () => {
  const value = responseSearchExample();
  const path = value.response.observations.paths.find(
    (path: Wire) => path.conditional_evaluations?.length && path.moves.length >= 3,
  );
  assert.ok(path);
  const entry = structuredClone(path.conditional_evaluations[0]);
  entry.ply_offset = 2;
  entry.search_position_id = path.position_ids[2];
  path.conditional_evaluations.push(entry);
  const originalResult = structuredClone(value.response.result);
  const observations = value.decode();
  assert.ok(observations.selection);
  assert.equal(
    observations.paths.find(candidate => candidate.id === path.id)!.conditionalEvaluations![1].plyOffset,
    2,
  );
  assert.deepEqual(value.response.result, originalResult);
  entry.search_position_id = path.position_ids[1];
  assert.equal(value.decode().selection, undefined);
});

test('child surveys and complete response classes admit their own MPV and reject arbitrary subsets', () => {
  for (const family of ['capture_moves', 'decline_moves', 'unrestricted']) {
    const value = openingPvExample('response-awaiting');
    const work = value.response.issued_engine_work;
    const question = value.response.observations.selection.response_inquiries.find(
      (q: Wire) => q.state === 'awaiting_engine',
    );
    work.root_restriction =
      family === 'unrestricted'
        ? { kind: 'unrestricted' }
        : { kind: 'restricted', moves_uci: question[family] };
    work.search_limits.multi_pv = Math.min(
      3,
      family === 'unrestricted'
        ? question.capture_moves.length + question.decline_moves.length
        : question[family].length,
    );
    assert.ok(value.decode().selection, family);
    work.search_limits.multi_pv = work.search_limits.multi_pv === 1 ? 2 : 1;
    assert.throws(value.decode, `${family} cannot misstate returned candidate count`);
  }
  const arbitrary = openingPvExample('response-awaiting');
  const question = arbitrary.response.observations.selection.response_inquiries.find(
    (q: Wire) => q.state === 'awaiting_engine',
  );
  arbitrary.response.issued_engine_work.root_restriction.moves_uci = question.decline_moves.slice(0, 3);
  arbitrary.response.issued_engine_work.search_limits.multi_pv = 3;
  assert.throws(arbitrary.decode, 'an arbitrary subset is not a complete response class');
});

test('a new legal representative does not erase the earlier bounded capture-versus-decline observation', () => {
  const value = openingPvExample('response-observed');
  const question = value.response.observations.selection.response_inquiries.find(
    (q: Wire) => q.state === 'observed',
  );
  const previous = question.alternative_move;
  question.alternative_move = question.decline_moves.find((move: string) => move !== previous);
  question.state = 'deferred';
  question.stop_reason = 'investigation_limit';
  for (const side of ['reference', 'alternative'])
    question[`${side}_selection`] = {
      source: 'legal_seed',
      work_id: null,
      line_id: null,
    };
  const selection = value.decode().selection!;
  assert.ok(selection);
  assert.ok(
    selection.candidates.some(candidate =>
      targetResponseDetail(candidate)?.branches.some(branch => branch.reply_move === previous),
    ),
  );
  question.state = 'observed';
  question.stop_reason = null;
  assert.equal(
    value.decode().selection,
    undefined,
    'an old complete pair cannot complete the new representatives',
  );
});

test('produced surveys retain an omitted class and replace a weak quiet reply without erasing its observation', () => {
  const awaiting = openingPvExample('response-class-awaiting');
  const pending = awaiting.decode().selection!;
  const question = pending.response_inquiries!.find(q => q.state === 'awaiting_engine')!;
  assert.equal(pending.response_searches!.length, 1);
  assert.ok(
    pending.response_searches![0].candidates.every(candidate =>
      question.decline_moves.includes(candidate.move_uci),
    ),
  );
  assert.deepEqual(awaiting.response.issued_engine_work.root_restriction.moves_uci, question.capture_moves);
  assert.equal(awaiting.response.issued_engine_work.search_limits.multi_pv, 1);
  assert.equal(question.answer, null);
  const completed = openingPvExample('response-observed').decode().selection!;
  const separate = completed.response_inquiries!.find(q => q.state === 'observed')!;
  assert.equal(completed.response_searches!.length, 2);
  assert.notEqual(separate.reference_selection!.work_id, separate.alternative_selection!.work_id);
  assert.equal(separate.answer, null, 'different searches do not supply a matched comparison');
  const quiet = openingPvExample('response-third-quiet').decode().selection!;
  const selected = quiet.response_inquiries!.find(q => q.root_move === 'd2d3')!;
  assert.equal(selected.alternative_move, 'g8h8');
  assert.equal(selected.alternative_selection!.source, 'child_search');
  assert.equal(selected.answer!.evaluation_relation, 'better');
  const readings = quiet.candidates
    .map(targetResponseDetail)
    .filter(detail => detail?.response_inquiry_id === selected.id);
  assert.ok(readings.some(detail => detail!.branches.some(branch => branch.reply_move === 'g8h8')));
  assert.ok(readings.some(detail => detail!.branches.some(branch => branch.reply_move === 'd8d7')));
});

test('one child pool includes every missing target class and rejects a partial or target-order-dependent pool', () => {
  const selection = openingPvExample('response-pinned-recapture').decode().selection!;
  const inquiries = selection.response_inquiries!.filter(inquiry => inquiry.root_move === 'e2e4');
  assert.equal(inquiries.length, 2);
  const captures = ['c5d4', 'e8e4'];
  assert.ok(responsePoolMatchesClasses(inquiries, captures));
  assert.ok(responsePoolMatchesClasses([...inquiries].reverse(), captures));
  assert.equal(responsePoolMatchesClasses(inquiries, ['c5d4', 'c5b6']), false);
  // Selection-policy input only: retain the existing quiet sample to model both capture classes omitted.
  const survey = {
    ...selection.response_searches![0],
    candidates: selection.response_searches![0].candidates.filter(candidate => candidate.move_uci === 'e8e7'),
  };
  assert.ok(responsePoolMatchesClasses(inquiries, captures, [survey]));
  assert.equal(responsePoolMatchesClasses(inquiries, ['c5d4'], [survey]), false);
  assert.equal(responsePoolMatchesClasses(inquiries, ['e8e4'], [survey]), false);
});

test('an equal engine comparison can expose bounded root differences without claiming an evaluation cause', () => {
  const value = openingPvExample();
  const observations = value.decode();
  const inquiry = observations.selection!.inquiries!.find(inquiry => inquiry.alternative_move === 'f1c4')!;
  assert.equal(inquiry.answer?.evaluation_relation, 'equal');
  assert.equal(inquiry.next_evidence, 'inspect_existing');
  assert.ok(inquiry.explanation_candidate_ids?.length);
  const candidate = observations.selection!.candidates.find(candidate =>
    inquiry.explanation_candidate_ids!.includes(candidate.id),
  )!;
  const detail = pvComparisonDetail(candidate)!;
  assert.equal(candidate.state, 'ready');
  assert.equal(candidate.selection, 'context');
  assert.equal(detail.scope, 'observed_lines');
  assert.equal(detail.material_relation, 'equal');
  assert.equal(detail.event_sequence_relation, 'same');
  assert.equal(detail.quality_band, 'close');
  assert.equal(detail.root_observation_relation, 'different');
  assert.deepEqual(
    detail.root_participants.map(piece => [piece.role, piece.square]),
    [['bishop', 'f1']],
  );
  const pending = openingPvExample('opening-pv-priority')
    .decode()
    .selection!.inquiries!.find(
      inquiry => inquiry.state === 'awaiting_engine' && inquiry.alternative_move === 'f1c4',
    )!;
  assert.equal(pending.alternative_move, 'f1c4');
  assert.ok(pending.ordering_candidate_ids?.length);
  assert.equal(pending.next_evidence, 'compare_roots');
});

test('PV facets, participants, receipt links, and inquiry next-evidence cannot be forged', () => {
  for (const mutate of [
    (candidate: Wire) => {
      candidate.detail.material_relation = 'different';
    },
    (candidate: Wire) => {
      candidate.detail.event_sequence_relation = 'different';
    },
    (candidate: Wire) => {
      candidate.detail.root_observation_relation = 'same';
    },
    (candidate: Wire) => {
      candidate.detail.quality_band = 'separated';
    },
    (candidate: Wire) => {
      candidate.detail.root_participants[0].square = 'b1';
    },
    (candidate: Wire) => {
      candidate.detail.root_difference_occurrence_ids.reverse();
    },
    (candidate: Wire) => {
      candidate.detail.answer.reference_evaluation.white_score.value += 1;
    },
    (candidate: Wire) => {
      candidate.detail.answer.work_id = 'work:99999';
    },
    (candidate: Wire) => {
      candidate.source_refs[0].source = 'focus_comparison';
    },
    (_: Wire, inquiry: Wire) => {
      inquiry.ordering_candidate_ids = ['foreign:candidate'];
    },
    (_: Wire, inquiry: Wire) => {
      inquiry.explanation_candidate_ids = ['foreign:candidate'];
    },
    (_: Wire, inquiry: Wire) => {
      inquiry.next_evidence = 'unresolved';
    },
    (_: Wire, inquiry: Wire) => {
      delete inquiry.explanation_candidate_ids;
    },
  ]) {
    const value = openingPvExample();
    const before = value.decode();
    const selection = value.response.observations.selection;
    const inquiry = selection.inquiries.find((row: Wire) => row.alternative_move === 'f1c4');
    const candidate = selection.candidates.find((row: Wire) =>
      inquiry.explanation_candidate_ids.includes(row.id),
    );
    mutate(candidate, inquiry);
    const after = value.decode();
    assert.equal(after.selection, undefined);
    assert.deepEqual(after.statements, before.statements);
    assert.deepEqual(after.paths, before.paths);
  }
});

test('PV quality bands distinguish small preference, mate distance, and saturated cp degradation', () => {
  for (const [kind, leftValue, rightValue, relation, band] of [
    ['cp', 0, 1, 'better', 'close'],
    ['cp', 0, 30, 'better', 'separated'],
    ['cp', 1000, 1500, 'better', 'separated'],
    ['mate', 5, 3, 'better', 'close'],
    ['mate', -3, -5, 'better', 'close'],
    ['mate', 5, -3, 'worse', 'separated'],
  ] as const) {
    // Synthetic evaluation observations on existing legal paths, not new engine results.
    const value = openingPvExample();
    const observations = value.decode();
    const inquiry = value.response.observations.selection.inquiries.find(
      (row: Wire) => row.alternative_move === 'f1c4',
    );
    const candidate = value.response.observations.selection.candidates.find((row: Wire) =>
      inquiry.explanation_candidate_ids.includes(row.id),
    );
    const answer = candidate.detail.answer;
    for (const [prefix, score] of [
      ['reference', leftValue],
      ['alternative', rightValue],
    ] as const) {
      const evaluation = answer[`${prefix}_evaluation`];
      evaluation.white_score = { kind, value: score };
      observations.paths.find(path => path.id === answer[`${prefix}_line_id`])!.evaluations = [
        structuredClone(evaluation),
      ];
    }
    answer.evaluation_relation = relation;
    candidate.detail.quality_band = band;
    const selection = { version: 1, candidates: [candidate], scenes: [] };
    const context = {
      rootFen: value.fixture.fen as FEN,
      playedMove: value.fixture.move as Uci,
      engineProfile: value.response.engine_profile,
    };
    assert.ok(
      decodeCommentarySelection(selection, observations, context),
      `${kind} ${leftValue}/${rightValue}`,
    );
    candidate.detail.quality_band = band === 'close' ? 'separated' : 'close';
    assert.equal(decodeCommentarySelection(selection, observations, context), undefined);
  }
});

// These transport tests attach a synthetic question to already produced paths.
// They do not assert that this question was automatically discovered or graded.
function example(file = 'best-choice-produced.json', ready = false) {
  const response = JSON.parse(readFileSync(new URL(file, fixtureRoot), 'utf8')) as Wire;
  const paths = response.observations.paths.filter((path: Wire) => path.line_summary);
  const left = paths[0] as Wire;
  const right = paths[1] as Wire;
  const unique = (items: string[]) => [...new Set(items)];
  const candidate: Wire = {
    id: 'comparison:test',
    kind: 'comparison',
    state: ready ? 'ready' : 'needs_explanation',
    selection: 'selected',
    root_occurrence_id: left.occurrence_ids[0],
    occurrence_ids: unique([...left.occurrence_ids, ...right.occurrence_ids]),
    position_ids: unique([...left.position_ids, ...right.position_ids]),
    line_ids: [left.id, right.id],
    evidence_ids: ['comparison-proof:test'],
    source_refs: [left, right].map(path => ({
      line_id: path.id,
      kind: path.sources ? 'supplied' : 'evaluation',
      source: path.sources?.[0] ?? 'root_search',
      accepted_ply_count: path.moves.length,
      stop: null,
    })),
    detail: {
      type: ready ? 'observed_material_comparison' : 'evaluation_comparison',
      scope: 'observed_lines',
      comparison_kind: 'played_vs_best',
      comparison_evidence_id: 'comparison-proof:test',
      material_relation: ready ? 'different' : 'equal',
      explanation_gap: ready ? null : 'beyond_material_scope',
      reference_line_id: left.id,
      candidate_line_id: right.id,
      missing_line_ids: [],
      perspective: left.line_summary.perspective,
      reference_net_cp: left.line_summary.material.net_cp,
      candidate_net_cp: right.line_summary.material.net_cp,
      text: 'Observed exchanges in these finite lines.',
    },
  };
  const scene: Wire = {
    id: 'scene:test',
    root_occurrence_id: candidate.root_occurrence_id,
    occurrence_ids: candidate.occurrence_ids,
    line_ids: candidate.line_ids,
    candidate_ids: [candidate.id],
    state: candidate.state,
  };
  response.observations.selection = { version: 1, candidates: [candidate], scenes: [scene] };
  const decode = (): MoveReviewObservations => {
    if (file === 'transposed-captures-produced.json') {
      const snapshot = decodeMoveReviewSnapshot(response, {
        requestId: response.request_id,
        subject: {
          variant: 'standard',
          initialFen,
          movePrefixUci:
            'e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4 g8f6 b1c3 a7a6 f1c4 e7e6 c4b3 b7b5 e1g1 f8e7 d1f3 d8c7 f3g3 e8g8 c1h6 f6e8 a1d1 c8d7 d4f3'.split(
              ' ',
            ),
          before: { path: '', fen: response.issued_engine_work.search_fen },
          played: { uci: response.focus.played_move_uci, san: 'Nc6' },
          after: { path: 'aa', fen: response.focus.resulting_fen },
          inputLines: paths
            .filter((path: Wire) => path.moves.length === 9)
            .map((path: Wire) => ({ source: 'analysis', moves: path.moves })),
        },
      });
      assert.ok(snapshot?.observations);
      return snapshot.observations;
    }
    return moveReviewObservations(
      observationFixtureJob({
        name: 'transport-only',
        fen: response.current_fen,
        move: response.focus.played_move_uci,
        response,
      }),
    )!;
  };
  return { response, candidate, scene, left, right, decode };
}

test('an unanswered comparison survives independently of existing positive board facts', () => {
  const value = example();
  const observations = value.decode();
  assert.ok(observations.statements.length > 0);
  assert.equal(observations.selection?.scenes[0].state, 'needs_explanation');
  assert.equal(observations.owner?.jobId, value.response.job_id);
  assert.equal(commentarySceneFrame(observations, observations.selection!.scenes[0])?.ply, 1);
  value.scene.state = 'ready';
  const changed = value.decode();
  assert.equal(changed.selection, undefined);
  assert.deepEqual(changed.statements, observations.statements);
});

test('invalid root, position, source, and path references discard only the optional selection', () => {
  for (const mutate of [
    (v: ReturnType<typeof example>) => {
      v.candidate.root_occurrence_id = 'foreign-job:occurrence';
    },
    (v: ReturnType<typeof example>) => {
      v.candidate.position_ids = ['same-fen-but-other-position'];
    },
    (v: ReturnType<typeof example>) => {
      v.candidate.line_ids.push('missing:path');
    },
    (v: ReturnType<typeof example>) => {
      v.candidate.source_refs[0].accepted_ply_count = 0;
    },
    (v: ReturnType<typeof example>) => {
      v.candidate.source_refs[0].kind = 'supplied';
      v.candidate.source_refs[0].source = 'study';
    },
    (v: ReturnType<typeof example>) => {
      v.scene.candidate_ids = ['foreign:candidate'];
    },
  ]) {
    const value = example();
    const before = value.decode();
    mutate(value);
    const after = value.decode();
    assert.equal(after.selection, undefined);
    assert.deepEqual(after.paths, before.paths);
    assert.deepEqual(after.statements, before.statements);
  }
});

test('the finite material question needs matching certified ledgers, not just matching net scores', () => {
  const value = example('transposed-captures-produced.json', true);
  const observations = value.decode();
  assert.equal(observations.selection?.scenes[0].state, 'ready');
  assert.equal(value.left.line_summary.material.net_cp, value.right.line_summary.material.net_cp);
  value.candidate.detail.reference_net_cp = 999;
  assert.equal(value.decode().selection, undefined);
  const sameLedger = example();
  sameLedger.candidate.state = sameLedger.scene.state = 'ready';
  sameLedger.candidate.detail.type = 'observed_material_comparison';
  assert.equal(sameLedger.decode().selection, undefined);
});

test('a missing comparison path remains partial with an exact actual anchor', () => {
  const value = example();
  // Withhold the reference only from selection, retaining its independent board facts.
  const missing = value.left.id;
  value.candidate.state = value.scene.state = 'partial';
  value.candidate.detail.material_relation = 'unavailable';
  value.candidate.detail.explanation_gap = 'missing_path';
  value.candidate.detail.reference_line_id = null;
  value.candidate.detail.missing_line_ids = [missing];
  for (const field of ['perspective', 'reference_net_cp', 'candidate_net_cp'])
    delete value.candidate.detail[field];
  value.candidate.line_ids = value.scene.line_ids = [value.right.id];
  value.candidate.position_ids = [...value.right.position_ids];
  value.candidate.occurrence_ids = value.scene.occurrence_ids = [
    ...new Set([value.candidate.root_occurrence_id, ...value.right.occurrence_ids]),
  ];
  value.candidate.source_refs = value.candidate.source_refs.slice(1);
  // The absent link is not an available path, even if another independent path holds the actual edge.
  const observations = value.decode();
  const copy: MoveReviewObservations = {
    ...observations,
    paths: observations.paths.filter(path => path.id !== missing),
  };
  const actualPath = observations.paths.find(path => path.id === missing)!;
  copy.paths.push({ ...actualPath, id: 'independent-actual-path' });
  const decoded = decodeCommentarySelection(value.response.observations.selection, copy);
  assert.equal(decoded?.scenes[0].state, 'partial');
  const frame = commentarySceneFrame(copy, decoded!.scenes[0])!;
  assert.equal(
    copy.paths.find(path => path.id === frame.proofId)!.occurrenceIds[frame.ply - 1],
    value.candidate.root_occurrence_id,
  );
  value.candidate.state = value.scene.state = 'ready';
  assert.equal(decodeCommentarySelection(value.response.observations.selection, copy), undefined);
});

test('equal final boards do not allow an event from another history to stand in for the referenced line', () => {
  const value = example('transposed-captures-produced.json', true);
  const observations = value.decode();
  const branches = observations.paths.filter(path => path.moves.length === 9);
  assert.equal(branches[0].moves.at(-1)!.fenAfter, branches[1].moves.at(-1)!.fenAfter);
  value.candidate.occurrence_ids.push(branches[1].occurrenceIds[5]);
  value.scene.occurrence_ids = value.candidate.occurrence_ids;
  assert.equal(value.decode().selection, undefined);
});

function inquiryExample(answered = false) {
  const value = example();
  const response = value.response;
  const paths = response.observations.paths as Wire[];
  const reference = paths.find(path => path.moves[0] === 'e2e4' && path.moves.length > 1)!;
  const alternative = paths.find(path => path.moves[0] === 'd2d4')!;
  const position = response.observations.positions.find((p: Wire) => p.id === reference.position_ids[0]);
  const legal = position.legal_moves.moves.find((m: Wire) => m.uci === 'd2d4');
  const evaluation = (value: number) => ({
    source: 'root_search',
    kind: 'engine_search',
    depth: 16,
    white_score: { kind: 'cp', value },
  });
  reference.evaluations = [evaluation(30)];
  alternative.evaluations = [evaluation(20)];
  const inquiry: Wire = {
    id: 'inquiry:contract',
    scope: 'root_choice',
    source: 'automatic_observation',
    root_occurrence_id: reference.occurrence_ids[0],
    position_id: position.id,
    anchor_occurrence_ids: [reference.occurrence_ids[0]],
    ordering_source_ids: [],
    reference_move: 'e2e4',
    alternative_move: 'd2d4',
    reasons: [
      {
        kind: 'engine_alternative',
        evidence_id: position.premise_id,
        source_id: legal.source_id,
        source_line_id: alternative.id,
        anchor_move: 'e2e4',
        actor: { position_id: position.id, side: 'white', role: 'pawn', square: 'd2' },
        capture: null,
        promotion_role: null,
        responds_to_check: false,
      },
    ],
    state: answered ? 'answered' : 'deferred',
    stop_reason: answered ? null : 'investigation_limit',
    answer: answered
      ? {
          work_id: 'work:1',
          execution_key_sha256: 'a'.repeat(64),
          engine_profile: response.engine_profile,
          required_depth: 16,
          reference_depth: 16,
          alternative_depth: 16,
          evaluation_relation: 'worse',
          reference_line_id: reference.id,
          alternative_line_id: alternative.id,
          reference_occurrence_id: reference.occurrence_ids[0],
          alternative_occurrence_id: alternative.occurrence_ids[0],
          reference_evaluation: evaluation(30),
          alternative_evaluation: evaluation(20),
        }
      : null,
    assessment_effect: 'unchanged',
  };
  response.observations.selection.inquiries = [inquiry];
  return { ...value, inquiry, reference, alternative, position };
}

test('literal unanswered inquiries and matching observed answers retain their distinct states', () => {
  for (const answered of [false, true]) {
    const value = inquiryExample(answered);
    const observations = value.decode();
    assert.equal(observations.selection?.inquiries?.[0].state, answered ? 'answered' : 'deferred');
    assert.equal(
      observations.selection?.inquiries?.[0].answer?.evaluation_relation,
      answered ? 'worse' : undefined,
    );
  }
});

test('inquiry root, legal reason, and evaluation mismatches discard only selection', () => {
  for (const mutate of [
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.position_id = v.reference.position_ids[1];
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.root_occurrence_id = v.alternative.occurrence_ids[0];
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.anchor_occurrence_ids.push(v.alternative.occurrence_ids[0]);
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.ordering_source_ids = [v.position.legal_moves.moves[0].source_id];
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.reasons[0].actor.square = 'e2';
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.reasons[0].source_id = 'b'.repeat(64);
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.reasons[0].evidence_id = 'foreign:owner';
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.reasons[0].responds_to_check = true;
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.reasons[0].kind = 'capture_option';
      v.inquiry.reasons[0].source_line_id = null;
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.answer.evaluation_relation = 'better';
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.answer.alternative_evaluation.white_score.value = 99;
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.answer.alternative_depth = 15;
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.answer.alternative_line_id = v.reference.id;
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.answer.alternative_occurrence_id = v.reference.occurrence_ids[0];
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.inquiry.answer.engine_profile = 'unissued-profile';
    },
    (v: ReturnType<typeof inquiryExample>) => {
      v.alternative.evaluations[0].depth = 0;
    },
  ]) {
    const value = inquiryExample(true);
    const before = value.decode();
    assert.ok(before.selection?.inquiries);
    mutate(value);
    const after = value.decode();
    assert.equal(after.selection, undefined);
    assert.deepEqual(after.statements, before.statements);
    assert.deepEqual(
      after.paths.map(path => path.moves),
      before.paths.map(path => path.moves),
    );
  }
});

test('inquiry stop states preserve reference changes and failed assessment refreshes', () => {
  const value = inquiryExample(true);
  value.inquiry.stop_reason = 'engine_unavailable';
  assert.equal(value.decode().selection, undefined);
  value.inquiry.assessment_effect = 'needs_refresh';
  assert.equal(value.decode().selection?.inquiries?.[0].state, 'answered');
  value.inquiry.stop_reason = 'reference_changed';
  assert.equal(value.decode().selection, undefined);
  value.inquiry.state = 'stopped';
  value.inquiry.answer = null;
  assert.equal(value.decode().selection?.inquiries?.[0].stop_reason, 'reference_changed');
  value.inquiry.state = 'awaiting_engine';
  value.inquiry.stop_reason = null;
  const verification = value.decode().selection?.inquiries?.[0];
  assert.equal(verification?.assessment_effect, 'needs_refresh');
  assert.equal(
    verification?.answer,
    null,
    'reference verification does not require a prior claim of superiority',
  );
});

test('child scores retain their search position and cannot become first-move evaluations', () => {
  for (const mutation of ['none', 'root', 'offset', 'position', 'source', 'duplicate']) {
    const value = example();
    const path = value.response.observations.paths.find(
      (path: Wire) => path.kind === 'analysis' && path.moves.length > 1,
    );
    const entry = {
      search_position_id: path.position_ids[1],
      ply_offset: 1,
      evaluation: {
        source: 'response_comparison',
        kind: 'engine_search',
        depth: 16,
        white_score: { kind: 'cp', value: -50 },
      },
    };
    path.conditional_evaluations = [entry];
    if (mutation === 'root') path.evaluations = [entry.evaluation];
    if (mutation === 'offset') entry.ply_offset = 0;
    if (mutation === 'position') entry.search_position_id = path.position_ids[0];
    if (mutation === 'source') entry.evaluation.source = 'root_search';
    if (mutation === 'duplicate') path.conditional_evaluations.push(structuredClone(entry));
    const observations = value.decode();
    assert.ok(observations.paths.length, mutation);
    if (mutation !== 'none') assert.equal(observations.selection, undefined, mutation);
    else {
      assert.ok(observations.selection);
      const admitted = observations.paths.find(row => row.id === path.id)!;
      assert.equal(admitted.conditionalEvaluations?.[0].searchPositionId, path.position_ids[1]);
      assert.equal(
        admitted.evaluations?.some(evaluation => evaluation.source === 'response_comparison') ?? false,
        false,
      );
    }
  }
});

test('an exact terminal observation cannot borrow a multi-ply engine path', () => {
  const value = {
    source: 'investigation_comparison',
    kind: 'exact_automatic_terminal',
    terminal: { kind: 'stalemate' },
  };
  assert.ok(decodeCommentaryEvaluation(value, 1));
  assert.equal(decodeCommentaryEvaluation(value, 2), undefined);
  assert.equal(
    decodeCommentaryEvaluation(
      {
        source: 'investigation_comparison',
        kind: 'engine_search',
        depth: 16,
        white_score: { kind: 'mate', value: 0 },
      },
      2,
    ),
    undefined,
  );
});
