import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, moveReviewSan, type MoveReviewSubject } from '../src/moveReview';

// Exact final public response after all23 original completed reports; no edited engine output.
const bytes = readFileSync(new URL(
  '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/functional-choice-v9-completed-value-produced.json.gz',
  import.meta.url,
));
assert.equal(createHash('sha256').update(bytes).digest('hex'),
  'd037de2bb1cf71e46922a26a79b62d092a7677fc130272c453c58337454f9878');
const rawSnapshot = JSON.parse(gunzipSync(bytes).toString('utf8')).snapshot;
const requestBytes = readFileSync(new URL(
  '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/functional-choice-v8b-original-request-produced.json',
  import.meta.url,
));
assert.equal(createHash('sha256').update(requestBytes).digest('hex'),
  '3ce6ca9b2b1319439a3dcbdb8d1852565a5c458c388bc2b4191f308f0f4b2ab4');
const request = JSON.parse(requestBytes.toString('utf8'))[0].request;
const context = {
  requestId: request.request_id,
  subject: {
    variant: 'standard', initialFen: request.initial_fen, movePrefixUci: request.move_prefix_uci,
    before: { path: '', fen: request.current_fen },
    played: { uci: request.focus.played_move_uci,
      san: moveReviewSan(request.current_fen, request.focus.played_move_uci)! },
    after: { path: 'aa', fen: request.focus.resulting_fen },
  } as MoveReviewSubject,
};
function root(raw: any) {
  const roots = raw.observations.selection.candidates.filter((row: any) =>
    row.detail?.summary_question?.question === 'functional_response');
  assert.equal(roots.length, 1);
  return roots[0];
}
function question(raw: any) { return root(raw).detail.summary_question; }
function proof(unit: any): any[] { return unit.proof_moves ?? unit.en.filter((part: any) => typeof part !== 'string'); }
function witnessRefs(raw: any, units: any[]): any[] {
  const detail = root(raw).detail, roots = new Set([detail.reference_line_id, detail.candidate_line_id]);
  return [...new Map(units.flatMap(proof).filter((ref: any) => !roots.has(ref.line_id))
    .map((ref: any) => [ref.line_id, ref])).values()];
}
function addProof(unit: any, refs: any[]) {
  unit.proof_moves = [...new Map([...proof(unit), ...refs]
    .map((ref: any) => [`${ref.line_id}/${ref.occurrence_id}`, ref])).values()];
}

test('actual value answer keeps four core witnesses and three independent detail witnesses', () => {
  const raw = rawSnapshot, q = question(raw), result = JSON.stringify(raw.result);
  assert.equal(witnessRefs(raw, q.units).length, 4);
  assert.equal(witnessRefs(raw, q.detail_units).length, 3);
  assert.equal(q.witness_line_ids.length, 7);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(decoded?.kind === 'completed', 'the actual completed value answer reaches the public decoder');
  const kept = root({ observations: decoded.observations });
  assert.deepEqual(kept.detail.summary_question, q, 'retain the exact answer, required costs and detail proof');
  assert.match(kept.detail.summary, /Rd6 remains competitive alongside Ra7/);
  assert.match(kept.detail.summary, /prefer Qd3 to an immediate Rd6/);
  assert.match(kept.detail.summary, /Qxd6 captures the white rook on d6/);
  assert.equal(JSON.stringify(raw.result), result, 'local value receipts do not change the root result');
});

const mutations: [string, (raw: any) => void][] = [
  ['a sixth core witness cannot borrow unused detail capacity', raw => {
    const q = question(raw);
    addProof(q.units[0], witnessRefs(raw, q.detail_units).slice(0, 2));
    assert.equal(witnessRefs(raw, q.units).length, 6);
  }],
  ['a sixth detail witness cannot borrow unused core capacity', raw => {
    const q = question(raw);
    addProof(q.detail_units[0], witnessRefs(raw, q.units).slice(0, 3));
    assert.equal(witnessRefs(raw, q.detail_units).length, 6);
  }],
  ['a listed detail witness must remain referenced by its actual proof', raw => {
    question(raw).detail_units = [];
  }],
  ['the actual immediate rook loss still requires capture coverage', raw => {
    const q = question(raw), paths = raw.observations.paths;
    const priced = q.units.find((unit: any) => unit.covered_moves.some((ref: any) => {
      const path = paths.find((path: any) => path.id === ref.line_id);
      return path?.moves.slice(0, 4).join(' ') === 'd2b1 e8c7 d1d6 c6d6';
    }));
    assert.ok(priced, 'use the genuine Nb1/Nc7/Rd6/Qxd6 price owner');
    priced.covered_moves = [];
    priced.role = 'reply';
  }],
];
for (const [name, mutate] of mutations) test(name, () => {
  // Every control mutates only the small selected answer. Share the unchanged
  // large observation inventories rather than copying the whole native frame.
  const original = root(rawSnapshot), raw = { ...rawSnapshot, observations: {
    ...rawSnapshot.observations, selection: { ...rawSnapshot.observations.selection,
      candidates: rawSnapshot.observations.selection.candidates.map((candidate: any) =>
        candidate === original ? structuredClone(candidate) : candidate) },
  } }, result = JSON.stringify(raw.result);
  mutate(raw);
  const decoded = decodeMoveReviewSnapshot(raw, context);
  assert.ok(!decoded?.observations?.selection, name);
  assert.ok(decoded?.kind === 'completed', 'an invalid answer does not erase the independent root evaluation');
  assert.equal(JSON.stringify(raw.result), result);
});
