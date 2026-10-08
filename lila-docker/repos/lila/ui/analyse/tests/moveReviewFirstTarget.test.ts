import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../src/moveReview';

/** The unmodified actual meaningv32 MOVED snapshot stopped before work:5.
 * Its pending first response has a legal missing capture and an already searched
 * leader, without a generated proposal owning this work. Mutations below test
 * client work admission only; they create no engine reports or chess judgments.
 */
function actualJob() {
  const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/first-target-sg41-moved-pending-produced.json.gz',
    import.meta.url,
  ))).toString('utf8'));
  const request = fixture.request;
  return { raw: fixture.snapshot, context: {
    requestId: request.request_id,
    subject: {
      variant: 'standard', initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
      before: { path: '', fen: request.current_fen },
      played: { uci: request.focus.played_move_uci, san: 'Nd5' },
      after: { path: 'aa', fen: request.focus.resulting_fen },
    } as MoveReviewSubject,
  } };
}

function actualMover(raw: any) {
  const inquiry = raw.observations.selection.response_inquiries.find((value: any) =>
    value.root_move === 'c3d5' && value.original_target.role === 'knight' &&
    value.original_target.square === 'c3' && value.target.square === 'd5');
  assert.ok(inquiry, 'the actual pending question owns the original c3 knight now on d5');
  return inquiry;
}

function reject(raw: any, context: ReturnType<typeof actualJob>['context'], label: string) {
  // Keep assertion diagnostics small: the actual snapshot contains many legal
  // paths, and rejection must not print or compare the entire snapshot object.
  assert.ok(decodeMoveReviewSnapshot(raw, context) === undefined, label);
}

test('an actual mover first target admits its missing capture and searched leader with the existing longer lease', () => {
  const { raw, context } = actualJob(), work = raw.issued_engine_work, inquiry = actualMover(raw);
  const originalAssessment = JSON.stringify(raw.assessment);
  assert.equal(work.work_id, 'work:5');
  assert.equal(work.execution_key_sha256,
    'cb7c6fb12b22629a030827225f8b97b38a52914da43b0fc1cd9c97ed30066a13');
  assert.ok(!raw.observations.selection.proposals.some((proposal: any) =>
    proposal.requirements.some((reading: any) => reading.kind === 'compare_moves' &&
      reading.state === 'awaiting_engine' && reading.work_id === work.work_id)),
  'the actual target question has no generated CompareMoves owner for pending work:5');
  assert.deepEqual(inquiry.capture_moves, ['c5d5']);
  assert.equal(inquiry.reference_selection.source, 'legal_seed');
  assert.equal(inquiry.alternative_move, 'f5e4');
  assert.equal(inquiry.alternative_selection.source, 'child_search');
  assert.equal(inquiry.alternative_selection.work_id, 'work:3');

  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'awaiting-evidence', 'admit the actual work, without completing it');
  assert.equal(decoded.issuedEngineWork.workId, 'work:5');
  assert.equal(decoded.issuedEngineWork.purpose, 'response_comparison');
  assert.deepEqual(decoded.issuedEngineWork.searchAnchor?.prefixMovesUci, ['c3d5']);
  assert.equal(decoded.issuedEngineWork.searchAnchor?.rootOccurrenceId, inquiry.root_move_occurrence_id);
  assert.equal(decoded.issuedEngineWork.searchAnchor?.positionId, inquiry.child_position_id);
  assert.deepEqual(decoded.issuedEngineWork.rootRestriction,
    { kind: 'restricted', movesUci: ['c5d5', 'f5e4'] });
  assert.deepEqual(decoded.issuedEngineWork.searchLimits,
    { depth: 16, nodes: 40_000_000, movetimeMs: 8_000, multiPv: 2 });
  assert.equal(decoded.issuedEngineWork.maxSearchElapsedMs, 9_000);
  assert.equal(decoded.assessment?.kind, 'completed');
  assert.equal(decoded.verification?.state, 'complete');
  assert.equal(decoded.verification?.completedDepth, 16);
  const kept = decoded.observations?.selection?.response_inquiries?.find(value => value.id === inquiry.id);
  assert.ok(kept, 'keep the owned target inquiry beside the unchanged root assessment');
  assert.equal(kept.state, 'awaiting_engine');
  assert.equal(kept.answer, null, 'work admission does not manufacture its comparison answer');
  assert.equal(JSON.stringify(raw.assessment), originalAssessment);
});

test('a longer actual-mover lease requires its currently awaiting inquiry rather than another target', () => {
  const { raw, context } = actualJob();
  for (const [label, mutate] of [
    ['mover inquiry absent', (changed: any) => {
      const id = actualMover(changed).id;
      changed.observations.selection.response_inquiries =
        changed.observations.selection.response_inquiries.filter((value: any) => value.id !== id);
      for (const value of changed.observations.selection.response_inquiries)
        value.related_inquiry_ids = value.related_inquiry_ids.filter((related: string) => related !== id);
    }],
    ['mover inquiry deferred', (changed: any) => {
      const owner = actualMover(changed);
      owner.state = 'deferred'; owner.stop_reason = 'investigation_limit';
    }],
    ['mover inquiry stopped', (changed: any) => {
      const owner = actualMover(changed);
      owner.state = 'stopped'; owner.stop_reason = 'search_budget_exhausted';
    }],
    ['foreign mover inquiry occurrence', (changed: any) => {
      actualMover(changed).root_move_occurrence_id = 'foreign-root-occurrence';
    }],
    ['foreign mover inquiry child', (changed: any) => {
      actualMover(changed).child_position_id = actualMover(changed).original_target.position_id;
    }],
  ] as [string, (changed: any) => void][]) {
    const changed = structuredClone(raw);
    mutate(changed);
    reject(changed, context, label);
  }
});

test('a real pending c2 target does not inherit the actual knight target longer allowance', () => {
  const { raw, context } = actualJob();
  const knight = actualMover(raw);
  knight.state = 'deferred'; knight.stop_reason = 'investigation_limit';
  const pawn = raw.observations.selection.response_inquiries.find((value: any) =>
    value.root_move === 'c3d5' && value.original_target.square === 'c2');
  assert.ok(pawn, 'use the independently present actual c2 pawn question');
  assert.equal(pawn.state, 'awaiting_engine');
  assert.deepEqual(pawn.capture_moves, ['c5c2']);
  raw.issued_engine_work.root_restriction.moves_uci = [pawn.reference_move, pawn.alternative_move];
  assert.deepEqual(raw.issued_engine_work.root_restriction.moves_uci, ['c5c2', 'f5e4']);
  reject(raw, context, 'an unchanged pending original c2 pawn cannot authorize the mover lease');

  // The same real target pair may still use the normal response allowance.
  // This is a client shape control, not an actual newly issued work or report.
  raw.issued_engine_work.search_limits.nodes = 2_000_000;
  raw.issued_engine_work.search_limits.movetime_ms = 2_500;
  raw.issued_engine_work.max_search_elapsed_ms = 3_500;
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'awaiting-evidence', 'ordinary legal target work remains admissible');
  assert.equal(decoded.issuedEngineWork.searchLimits.nodes, 2_000_000);
  assert.ok(decoded.observations?.selection?.response_inquiries?.some(value =>
    value.id === pawn.id && value.state === 'awaiting_engine' && value.answer === null));
});

test('an actual first target cannot borrow another prefix, root occurrence or child position', () => {
  const { raw, context } = actualJob();
  for (const [label, mutate] of [
    ['two-ply prefix', (work: any) => { work.search_anchor.prefix_moves_uci = ['c3d5', 'f5e4']; }],
    ['alternative root prefix', (work: any) => { work.search_anchor.prefix_moves_uci = ['a3a4']; }],
    ['foreign anchor occurrence', (work: any) => { work.search_anchor.root_occurrence_id = 'foreign-root-occurrence'; }],
    ['root position instead of exact child', (work: any) => {
      work.search_anchor.position_id = actualMover(raw).original_target.position_id;
    }],
  ] as [string, (work: any) => void][]) {
    const changed = structuredClone(raw);
    mutate(changed.issued_engine_work);
    reject(changed, context, label);
  }
});

test('the actual target authorizes only the exact longer profile and a validated response pair', () => {
  const { raw, context } = actualJob();
  for (const [label, mutate] of [
    ['mixed node budget', (work: any) => { work.search_limits.nodes = 20_000_000; }],
    ['mixed time budget', (work: any) => { work.search_limits.movetime_ms = 5_000; }],
    ['larger elapsed budget', (work: any) => { work.max_search_elapsed_ms = 9_001; }],
    ['smaller elapsed budget', (work: any) => { work.max_search_elapsed_ms = 6_000; }],
    ['shallow scope', (work: any) => { work.search_limits.depth = 8; }],
    ['one reply only', (work: any) => {
      work.root_restriction.moves_uci = ['c5d5']; work.search_limits.multi_pv = 1;
    }],
    ['not a legal response class pair', (work: any) => {
      work.root_restriction.moves_uci = ['f5e4', 'f5f4'];
    }],
    ['unrestricted scope', (work: any) => {
      work.root_restriction = { kind: 'unrestricted' }; work.search_limits.multi_pv = 3;
    }],
    ['focus scope', (work: any) => { work.purpose = 'focus_comparison'; }],
  ] as [string, (work: any) => void][]) {
    const changed = structuredClone(raw);
    mutate(changed.issued_engine_work);
    reject(changed, context, label);
  }
});
