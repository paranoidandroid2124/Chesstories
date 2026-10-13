import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../../src/review/moveReview';

// The two preregistered live runs; fixture replay does not measure latency or
// replace the semantic reading of the actual product screens.
const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/function-purpose-browser.json.gz',
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

function answer(raw: any) {
  const candidate = raw.observations.selection.candidates.find((c: any) =>
    c.selection === 'selected' && c.detail.summary_question?.question === 'move_purpose');
  assert.ok(candidate);
  return candidate;
}

test('one functional question owns discovery, reply work and the delivered core in both live cases', () => {
  assert.equal(fixture.cases.length, 2);
  for (const row of fixture.cases) {
    assert.ok(row.elapsed_ms < 10_000);
    assert.ok(row.depths.every((d: number) => d <= 18));
    const decoded = decodeMoveReviewSnapshot(row.snapshot, context(row));
    assert.equal(decoded?.kind, 'completed');
    const selected = answer(row.snapshot);
    const admitted = decoded?.observations?.selection?.candidates.find(c => c.id === selected.id);
    assert.deepEqual(admitted?.detail.summary_question, selected.detail.summary_question);
    const question = decoded!.observations!.selection!.purpose_question!;
    const root = decoded!.observations!.paths.find(p => p.id === question.source_line_id)!;
    assert.ok(root);
    // In the new case the root PV contains prevention, not b4's realization.
    if (row.san === 'a3') assert.ok(!root.moves.some(move => move.uci === question.use_move));
    const phases = row.saved_reports.map((s: any) => s.issued_work.purpose);
    assert.deepEqual(phases, row.san === 'Rd3' ?
      ['root_search', 'response_comparison', 'response_comparison'] : ['root_search', 'response_comparison']);
    const core = String(admitted!.detail.summary);
    for (const text of row.san === 'Rd3' ? ['through d2', 'pawn on e2', 'rook on e8', 'c2 and b2', 'Rc2 Rd1'] :
      ['control of b4', 'knight on c6', 'knight on d5', 'O-O b4', 'bishop on c5', 'a5 e4'])
      assert.ok(core.includes(text), `${row.id}: ${text}`);
  }
});

test('foreign function ownership and missing board evidence cannot erase or replace the root assessment', () => {
  for (const row of fixture.cases) {
    const original = decodeMoveReviewSnapshot(row.snapshot, context(row));
    assert.ok(original?.kind === 'completed');
    for (const mutate of [
      (raw: any) => { answer(raw).detail.summary_question.move_purpose.execution_key_sha256 = '0'.repeat(64); },
      (raw: any) => { answer(raw).detail.summary_question.move_purpose.id = 'unissued-purpose'; },
      (raw: any) => {
        const id = raw.observations.selection.purpose_question.child_position_id;
        raw.observations.positions.find((p: any) => p.id === id).controls.complete = false;
      },
    ]) {
      const raw = structuredClone(row.snapshot);
      mutate(raw);
      const decoded = decodeMoveReviewSnapshot(raw, context(row));
      assert.ok(decoded?.kind === 'completed');
      assert.equal(decoded.observations?.selection, undefined);
      assert.deepEqual(decoded.evidence, original.evidence);
    }
  }
});

test('issued purpose pools and missing-class work obey the same browser boundary and depth ceiling', () => {
  for (const row of fixture.cases) for (const pending of row.pending) {
    assert.equal(decodeMoveReviewSnapshot(pending, context(row))?.kind, 'awaiting-evidence');
    const changed = structuredClone(pending);
    changed.issued_engine_work.search_limits.depth = 19;
    assert.equal(decodeMoveReviewSnapshot(changed, context(row)), undefined);
  }
});
