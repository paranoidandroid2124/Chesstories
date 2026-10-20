import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { test } from 'node:test';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../src/review/moveReview';

// Actual 10-second UI jobs, including each issued work and its exact report.
// Replaying these is a delivery check, not another independent chess sample.
const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../judgment-evaluation/fixtures/public-commentary-v6/core-idea-browser.json.gz', import.meta.url,
))).toString('utf8'));
const rows = fixture.cases;
function context(row: any) {
  const r = row.request;
  return { requestId: r.request_id, engineProfile: r.engine_profile, subject: {
    variant: 'standard', initialFen: r.initial_fen, movePrefixUci: r.move_prefix_uci,
    inputLines: r.input_lines, before: { path: '', fen: r.current_fen },
    after: { path: 'aa', fen: r.focus.resulting_fen }, played: { uci: r.focus.played_move_uci, san: row.id },
  } as MoveReviewSubject };
}
function selected(raw: any) {
  return raw.observations.selection.candidates.find((c: any) =>
    c.selection === 'selected' && c.detail.type === 'root_comparison');
}

test('three actual screens deliver their owned idea, surveyed replies and costs', () => {
  assert.equal(rows.length, 3);
  for (const row of rows) {
    assert.ok(row.elapsed_ms <= 11000 && row.depths.every((d: number) => d <= 18));
    for (const pending of row.pending) assert.ok(decodeMoveReviewSnapshot(pending, context(row)));
    const decoded = decodeMoveReviewSnapshot(row.snapshot, context(row));
    assert.equal(decoded?.kind, 'completed');
    const source = selected(row.snapshot);
    const shown = decoded!.observations!.selection!.candidates.find(c => c.id === source.id)!;
    assert.deepEqual(shown.detail.summary_question, source.detail.summary_question);
    assert.equal(source.detail.summary_question.question, 'functional_response');
    assert.ok(source.detail.summary_question.units.every((u: any) => !u.english_only));
    assert.ok(source.detail.summary_question.units.some((u: any) => u.covered_moves.length));
  }
});

test('preparation work requires its exact root, observed use and issued search scope', () => {
  for (const row of rows) {
    const pending = row.pending.find((r: any) => r.observations?.selection.preparation_question);
    assert.ok(pending);
    for (const mutate of [
      (r: any) => { delete r.observations.selection.preparation_question; },
      (r: any) => { r.observations.selection.preparation_question.root_occurrence_id = 'foreign'; },
      (r: any) => { r.observations.selection.preparation_question.use_move = 'a1a8'; },
      (r: any) => { r.issued_engine_work.search_limits.depth = 19; },
      (r: any) => { r.issued_engine_work.root_restriction.moves_uci = ['a1a8']; },
    ]) {
      const invalid = structuredClone(pending);
      mutate(invalid);
      assert.equal(decodeMoveReviewSnapshot(invalid, context(row)), undefined);
    }
  }
  const row = rows.find((r: any) => r.id === 're2');
  const survey = row.pending.find((r: any) =>
    r.issued_engine_work.purpose === 'response_comparison' &&
    r.issued_engine_work.root_restriction.moves_uci.length > 3);
  assert.ok(survey);
  const inventedPair = structuredClone(survey);
  inventedPair.issued_engine_work.root_restriction.moves_uci =
    inventedPair.issued_engine_work.root_restriction.moves_uci.slice(0, 3);
  assert.equal(decodeMoveReviewSnapshot(inventedPair, context(row)), undefined,
    'a few legal moves cannot masquerade as the owned full-position survey');
});

test('the basic answer cannot borrow a foreign move or hide its exchange cost', () => {
  for (const row of rows) for (const mutate of [
    (r: any) => { selected(r).detail.summary_question.units[0].en.find((p: any) => typeof p !== 'string').occurrence_id = 'foreign'; },
    (r: any) => { selected(r).detail.summary_question.units.find((u: any) => u.covered_moves.length).covered_moves = []; },
  ]) {
    const invalid = structuredClone(row.snapshot);
    mutate(invalid);
    assert.equal(decodeMoveReviewSnapshot(invalid, context(row))?.observations?.selection, undefined);
  }
});
