import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { test } from 'node:test';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../src/review/moveReview';

const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../judgment-evaluation/fixtures/public-commentary-v6/purpose-comparison-browser.json.gz', import.meta.url,
))).toString('utf8'));
const rows = fixture.cases;
const currentRows = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../judgment-evaluation/fixtures/public-commentary-v6/concise-purpose-browser.json.gz', import.meta.url,
))).toString('utf8')).cases;
const continuityRows = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../judgment-evaluation/fixtures/public-commentary-v6/function-continuity-browser.json.gz', import.meta.url,
))).toString('utf8')).cases;
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

function assertScreens(screens: any[], maxElapsedMs: number) {
  assert.deepEqual(screens.map((r: any) => r.id).sort(), ['qa4', 're2']);
  for (const row of screens) {
    assert.ok(row.elapsed_ms <= maxElapsedMs && row.depths.every((d: number) => d <= 18));
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
}

test('both actual ten-second screens deliver the combined purpose and owned exchange costs', () => {
  assertScreens(rows, 11000);
});

test('both actual twenty-second screens deliver the composed purposes and complete costs', () => {
  assertScreens(currentRows, 21000);
  for (const row of currentRows) assert.ok(row.request.remaining_budget_ms <= 20000);
});

test('both actual screens retain bounded functions when purpose and aftermath share their explanation', () => {
  assertScreens(continuityRows, 21000);
  for (const row of continuityRows) assert.ok(row.request.remaining_budget_ms <= 20000);
});

test('embedding preparation in the pair account does not weaken ownership or cost rejection', () => {
  for (const row of [...rows, ...currentRows, ...continuityRows]) for (const mutate of [
    (r: any) => { selected(r).detail.summary_question.units[0].en.find((p: any) => typeof p !== 'string').occurrence_id = 'foreign'; },
    (r: any) => { selected(r).detail.summary_question.units.find((u: any) => u.covered_moves.length).covered_moves = []; },
  ]) {
    const invalid = structuredClone(row.snapshot);
    mutate(invalid);
    assert.equal(decodeMoveReviewSnapshot(invalid, context(row))?.observations?.selection, undefined);
  }
});
