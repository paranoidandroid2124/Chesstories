import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { gunzipSync } from 'node:zlib';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../../src/review/moveReview';

// Two live browser runs, using the product coordinator, decoder and view with
// Stockfish 18. These saved contracts are not a substitute for the screen run.
const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/purpose-delivery-browser.json.gz',
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
    c.selection === 'selected' && c.detail.summary_question?.question === 'exchange_function');
  assert.ok(candidate, 'a completed functional account must actually be delivered');
  return candidate;
}

test('both live ten-second cases deliver an owned functional answer with the original numerical result', () => {
  assert.equal(fixture.cases.length, 2);
  for (const row of fixture.cases) {
    assert.ok(row.elapsed_ms < 10_000);
    assert.ok(row.depths.every((depth: number) => depth <= 18));
    const candidate = answer(row.snapshot);
    const decoded = decodeMoveReviewSnapshot(row.snapshot, context(row));
    assert.ok(decoded?.kind === 'completed');
    const admitted = decoded?.observations?.selection?.candidates.find(c => c.id === candidate.id);
    assert.ok(admitted, row.id);
    assert.deepEqual(admitted.detail.summary_question, candidate.detail.summary_question);
    for (const source of row.snapshot.result.selected_move_reviews) {
      const actual = decoded.evidence.candidates.find(c => c.uci === source.move_uci);
      assert.ok(actual);
      assert.deepEqual(actual.roles, source.selection.roles);
      if (source.commentary?.primary.kind === 'move_verdict') {
        assert.ok(actual.review.kind === 'move-verdict' && actual.review.core.kind === 'move-verdict');
        assert.equal(actual.review.core.verdictCode, source.commentary.primary.verdict_code);
      }
    }
    const summary = String(admitted.detail.summary);
    for (const meaning of row.required_phrases) assert.ok(summary.includes(meaning), `${row.id}: ${meaning}`);
  }
});

test('a foreign reply owner or missing exchange price rejects prose while retaining the numerical review', () => {
  for (const row of fixture.cases) {
    const original = decodeMoveReviewSnapshot(row.snapshot, context(row));
    assert.ok(original?.kind === 'completed');
    for (const mutate of [
      (q: any) => { q.first_target_response.capture_decline_execution_key_sha256 = '0'.repeat(64); },
      (q: any) => {
        const cost = q.units.find((u: any) => u.covered_moves.length > 1);
        assert.ok(cost, 'this actual exchange has both capture and recapture prices');
        cost.covered_moves.shift();
      },
    ]) {
      const raw = structuredClone(row.snapshot);
      mutate(answer(raw).detail.summary_question);
      const decoded = decodeMoveReviewSnapshot(raw, context(row));
      assert.ok(decoded?.kind === 'completed');
      assert.equal(decoded?.observations?.selection, undefined);
      assert.deepEqual(decoded.evidence, original.evidence);
    }
  }
});

test('the actual short reply work is accepted and depth above eighteen is rejected', () => {
  for (const row of fixture.cases) {
    assert.equal(decodeMoveReviewSnapshot(row.pending, context(row))?.kind, 'awaiting-evidence');
    const changed = structuredClone(row.pending);
    changed.issued_engine_work.search_limits.depth = 19;
    assert.equal(decodeMoveReviewSnapshot(changed, context(row)), undefined);
  }
});
