import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { test } from 'node:test';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../src/review/moveReview';

// The real 7.34-second request completed but displayed no article. Preserve it
// unchanged so a new search cannot conceal a delivery regression.
const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../judgment-evaluation/fixtures/public-commentary-v6/generalized-reply-delivery.json.gz', import.meta.url,
))).toString('utf8'));
const r = fixture.request;
const context = { requestId: r.request_id, engineProfile: r.engine_profile, subject: {
  variant: 'standard', initialFen: r.initial_fen, movePrefixUci: r.move_prefix_uci,
  inputLines: r.input_lines, before: { path: '', fen: r.current_fen },
  after: { path: 'aa', fen: r.focus.resulting_fen }, played: { uci: r.focus.played_move_uci, san: 'e5' },
} as MoveReviewSubject };
function selected(raw: any) {
  return raw.observations.selection.candidates.find((c: any) =>
    c.selection === 'selected' && c.detail.type === 'root_comparison');
}

test('the actual root pair admits its completed reply comparison without replacing the pair', () => {
  const decoded = decodeMoveReviewSnapshot(fixture.snapshot, context);
  assert.equal(decoded?.kind, 'completed');
  const source = selected(fixture.snapshot);
  const shown = decoded!.observations!.selection!.candidates.find(c => c.id === source.id)!;
  assert.deepEqual(shown.detail.summary_question, source.detail.summary_question);
  assert.equal(shown.detail.summary_question!.witness_line_ids!.length, 2);
});

test('a reply supplement still requires the inquiry, its issued receipt and its capture account', () => {
  const mutations = [
    (raw: any) => { raw.observations.selection.response_inquiries = []; },
    (raw: any) => { raw.observations.selection.response_inquiries[0].answer.execution_key_sha256 = '0'.repeat(64); },
    (raw: any) => { selected(raw).detail.summary_question.units.find((u: any) => u.covered_moves.length).covered_moves = []; },
  ];
  for (const mutate of mutations) {
    const raw = structuredClone(fixture.snapshot);
    mutate(raw);
    assert.equal(decodeMoveReviewSnapshot(raw, context)?.observations?.selection, undefined);
  }
});
