import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { test } from 'node:test';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../../src/review/moveReview';

const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/strategic-function-browser.json.gz',
  import.meta.url,
))).toString('utf8'));

function context(row: any) {
  const r = row.request;
  return { requestId: r.request_id, engineProfile: r.engine_profile, subject: {
    variant: 'standard', initialFen: r.initial_fen, movePrefixUci: r.move_prefix_uci,
    before: { path: '', fen: r.current_fen }, after: { path: 'aa', fen: r.focus.resulting_fen },
    played: { uci: r.focus.played_move_uci, san: row.san },
  } as MoveReviewSubject };
}

function selected(raw: any) {
  const candidate = raw.observations.selection.candidates.find((c: any) =>
    c.selection === 'selected' && c.detail.summary_question?.question === 'move_purpose');
  assert.ok(candidate);
  return candidate;
}

test('both actual ten-second screens deliver the owned exchange purpose and retain the evaluation', () => {
  assert.equal(fixture.cases.length, 2);
  for (const row of fixture.cases) {
    assert.ok(row.elapsed_ms < 10_000);
    assert.ok(row.depths.every((depth: number) => depth <= 18));
    const decoded = decodeMoveReviewSnapshot(row.snapshot, context(row));
    assert.equal(decoded?.kind, 'completed');
    const source = selected(row.snapshot);
    const shown = decoded!.observations!.selection!.candidates.find(c => c.id === source.id)!;
    assert.deepEqual(shown.detail.summary_question, source.detail.summary_question);
    const q = decoded!.observations!.selection!.purpose_question!;
    assert.equal(q.kind, 'exchange_activity');
    assert.ok(q.activity!.targets.length);
    assert.equal(q.square, row.san === 'Bd5' ? 'd5' : 'c2');
    assert.equal(shown.detail.summary_question!.move_purpose!.id, q.id);
    for (const pending of row.pending) {
      assert.equal(decodeMoveReviewSnapshot(pending, context(row))?.kind,
        !pending.assessment && pending.issued_engine_work.purpose === 'root_search' ? 'awaiting-core' : 'awaiting-evidence');
      const changed = structuredClone(pending);
      changed.issued_engine_work.search_limits.depth = 19;
      assert.equal(decodeMoveReviewSnapshot(changed, context(row)), undefined);
    }
  }
});

test('wrong targets, exchange offers, files, reply classes, and foreign work cannot substitute a task', () => {
  for (const row of fixture.cases) {
    const original = decodeMoveReviewSnapshot(row.snapshot, context(row))!;
    for (const mutate of [
      (q: any) => { q.activity.targets[0].square = 'a1'; },
      (q: any) => { q.activity.offers = []; },
      (q: any) => { q.square = 'e4'; q.use_move = q.use_move.slice(0, 2) + 'e4'; },
      (q: any) => { q.activity.open_file = 'h'; },
      (q: any) => { q.challenge_moves.pop(); },
    ]) {
      const raw = structuredClone(row.snapshot);
      // Mutate both copies so ownership equality cannot mask a missing board check.
      mutate(raw.observations.selection.purpose_question);
      mutate(selected(raw).detail.summary_question.move_purpose);
      const decoded = decodeMoveReviewSnapshot(raw, context(row))!;
      assert.equal(decoded.kind, 'completed');
      assert.equal(decoded.observations?.selection, undefined);
      assert.deepEqual(decoded.evidence, original.evidence);
    }
    const foreign = structuredClone(row.snapshot);
    selected(foreign).detail.summary_question.move_purpose.execution_key_sha256 = '0'.repeat(64);
    assert.equal(decodeMoveReviewSnapshot(foreign, context(row))?.observations?.selection, undefined);
  }
});

test('a purpose answer cannot omit the actual selected capture price', () => {
  const row = fixture.cases.find((r: any) => r.san === 'Bd5');
  const raw = structuredClone(row.snapshot);
  const cost = selected(raw).detail.summary_question.units.find((unit: any) => unit.covered_moves.length);
  assert.ok(cost, 'the actual Bd5 answer selects piece exchanges');
  cost.covered_moves = [];
  const decoded = decodeMoveReviewSnapshot(raw, context(row));
  assert.equal(decoded?.kind, 'completed');
  assert.equal(decoded.observations?.selection, undefined);
});
