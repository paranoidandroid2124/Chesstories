import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../src/moveReview';

/** These unchanged current snapshots replay 17 historical actual receipts.
 * Every whole issued work matched before submission; no new engine query or
 * numeric judgment was created. Mutations below test optional prose admission.
 */
function actual(suffix: 'HELD' | 'MOVED') {
  const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
    '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/first-target-basic-v35-produced.json.gz',
    import.meta.url,
  ))).toString('utf8'));
  assert.equal(fixture.new_engine_queries, 0);
  assert.equal(fixture.snapshot_mutation, false);
  const row = fixture.cases.find((value: any) => value.game_id === `FC-SG41-ND5-${suffix}`);
  assert.ok(row);
  assert.ok(row.result.work_checks.every((check: any) => check.exact_match && check.report_submitted));
  const request = row.request;
  return { raw: row.snapshot, context: { requestId: request.request_id, subject: {
    variant: 'standard', initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
    before: { path: '', fen: request.current_fen },
    played: { uci: request.focus.played_move_uci, san: 'Nd5' },
    after: { path: 'aa', fen: request.focus.resulting_fen },
  } as MoveReviewSubject } };
}

function answer(raw: any) {
  const candidate = raw.observations.selection.candidates.find((value: any) => value.selection === 'selected' &&
    value.detail.summary_question?.question === 'first_target_response');
  assert.ok(candidate, 'the actual root produced the completed first-target body');
  return { candidate, question: candidate.detail.summary_question };
}

function rejectSelection(raw: any, context: ReturnType<typeof actual>['context'], label: string) {
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed', `${label}: keep the independent original numerical review`);
  assert.equal(decoded.observations?.selection, undefined, `${label}: reject the optional invalid prose selection`);
}

test('both actual first-target bodies retain completed owners, first reply, price and original root assessment', () => {
  for (const suffix of ['HELD', 'MOVED'] as const) {
    const { raw, context } = actual(suffix), { candidate, question } = answer(raw);
    const original = JSON.stringify(raw.result);
    const decoded = decodeMoveReviewSnapshot(raw, context);
    assert.ok(decoded?.kind === 'completed');
    const admitted = decoded.observations?.selection?.candidates.find(value => value.id === candidate.id);
    assert.ok(admitted, `${suffix}: admit the actual price-closed first-target body`);
    assert.equal(admitted.detail.summary_question?.question, 'first_target_response');
    assert.deepEqual(admitted.detail.summary_question?.first_target_response, question.first_target_response);
    assert.ok(String(admitted.detail.summary).startsWith('After Nd5'));
    assert.deepEqual(admitted.detail.summary_question?.units, question.units);
    assert.deepEqual(admitted.detail.summary_question?.detail_units, question.detail_units);
    assert.equal(JSON.stringify(raw.result), original, 'first-child facts never rewrite the root grade');
  }
});

test('a first-target name alone cannot replace its exact five-field completed ownership', () => {
  const { raw, context } = actual('MOVED');
  for (const [label, mutate] of [
    ['missing metadata', (q: any) => { delete q.first_target_response; }],
    ['null metadata', (q: any) => { q.first_target_response = null; }],
    ['extra owner field', (q: any) => { q.first_target_response.assumed = true; }],
    ['another question with metadata', (q: any) => { q.question = 'selected_comparison'; }],
  ] as [string, (q: any) => void][]) {
    const changed = structuredClone(raw); mutate(answer(changed).question);
    rejectSelection(changed, context, label);
  }
});

test('first-target metadata cannot borrow another work, execution key or restricted pool', () => {
  const { raw, context } = actual('MOVED');
  for (const [label, mutate] of [
    ['foreign inquiry', (o: any) => { o.response_inquiry_id = 'another-inquiry'; }],
    ['foreign pool work', (o: any) => { o.full_pool_work_id = 'work:0'; }],
    ['foreign pool key', (o: any) => { o.full_pool_execution_key_sha256 = '0'.repeat(64); }],
    ['foreign pair work', (o: any) => { o.capture_decline_work_id = 'work:4'; }],
    ['foreign pair key', (o: any) => { o.capture_decline_execution_key_sha256 = '0'.repeat(64); }],
    ['restricted comparison as full pool', (o: any) => {
      o.full_pool_work_id = o.capture_decline_work_id;
      o.full_pool_execution_key_sha256 = o.capture_decline_execution_key_sha256;
    }],
  ] as [string, (o: any) => void][]) {
    const changed = structuredClone(raw); mutate(answer(changed).question.first_target_response);
    rejectSelection(changed, context, label);
  }
});

test('an observed, deferred or stopped mover inquiry does not certify a completed first-target body', () => {
  const { raw, context } = actual('MOVED');
  for (const state of ['observed', 'deferred', 'stopped']) {
    const changed = structuredClone(raw), owner = answer(changed).question.first_target_response;
    const inquiry = changed.observations.selection.response_inquiries.find((value: any) => value.id === owner.response_inquiry_id);
    assert.ok(inquiry);
    inquiry.state = state; inquiry.answer = null;
    inquiry.stop_reason = state === 'stopped' ? 'search_budget_exhausted' : null;
    rejectSelection(changed, context, state);
  }
});

test('a first-target body keeps the actual original mover, root occurrence and child', () => {
  const { raw, context } = actual('MOVED');
  for (const [label, mutate] of [
    ['foreign original target', (v: any) => { v.original_target.square = 'c2'; }],
    ['foreign current target', (v: any) => { v.target.square = 'd4'; }],
    ['foreign root occurrence', (v: any) => { v.root_move_occurrence_id = 'another-root'; }],
    ['foreign child', (v: any) => { v.child_position_id = 'another-child'; }],
  ] as [string, (v: any) => void][]) {
    const changed = structuredClone(raw), owner = answer(changed).question.first_target_response;
    mutate(changed.observations.selection.response_inquiries.find((value: any) => value.id === owner.response_inquiry_id));
    rejectSelection(changed, context, label);
  }
});

test('detail-only first response and own action cannot pass as the selected first-target core', () => {
  const { raw, context } = actual('MOVED'), { candidate, question } = answer(raw);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed' && decoded.observations?.selection);
  const paths = decoded.observations.paths;
  const hasReply = (unit: any) => unit.action_moves.some((ref: any) => {
    const path = paths.find(path => path.id === ref.line_id)!;
    return ['f5e4', 'd4e4'].includes(path.moves[path.occurrenceIds.indexOf(ref.occurrence_id)].uci);
  });
  const moved = question.units.filter(hasReply);
  assert.equal(moved.length, 2, 'move the real response and its protected price together');
  question.units = question.units.filter((unit: any) => !hasReply(unit));
  question.detail_units.push(...moved);
  assert.ok(question.detail_units.length <= 8, 'this control does not fail on the detail count cap');
  candidate.detail.summary = question.units.map((unit: any) => unit.en.map((part: any) => {
    if (typeof part === 'string') return part;
    const path = paths.find(path => path.id === part.line_id)!;
    return path.moves[path.occurrenceIds.indexOf(part.occurrence_id)].label;
  }).join('')).join(' ');
  rejectSelection(raw, context, 'the complete actual response was moved out of core');
});

test('a first-target label does not bypass the necessary fxe4 and Rxe4 material price', () => {
  const { raw, context } = actual('MOVED'), { candidate, question } = answer(raw);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed' && decoded.observations?.selection);
  const cost = question.units.filter((unit: any) => unit.role === 'cost');
  assert.equal(cost.length, 1, 'the actual protected support-use price owns both immediate captures');
  assert.equal(cost[0].covered_moves.length, 2);
  question.units = question.units.filter((unit: any) => unit.role !== 'cost');
  candidate.detail.summary = question.units.map((unit: any) => unit.en.map((part: any) => {
    if (typeof part === 'string') return part;
    const path = decoded.observations!.paths.find(path => path.id === part.line_id)!;
    return path.moves[path.occurrenceIds.indexOf(part.occurrence_id)].label;
  }).join('')).join(' ');
  rejectSelection(raw, context, 'selected captures have lost their own price');
});
