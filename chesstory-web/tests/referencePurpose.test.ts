import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { test } from 'node:test';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../src/review/moveReview';

const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../judgment-evaluation/fixtures/public-commentary-v6/evaluation-reference-browser.json.gz',
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
    c.selection === 'selected' && c.detail.type === 'root_comparison' &&
    c.detail.summary_question?.question === 'selected_comparison');
  assert.ok(candidate, 'the actual assessment pair must own the basic answer');
  return candidate;
}

test('both actual screens retain their assessment pair and deliver the checked purpose', () => {
  assert.equal(fixture.cases.length, 2);
  for (const row of fixture.cases) {
    assert.ok(row.elapsed_ms < 10_000);
    assert.ok(row.depths.every((depth: number) => depth <= 18));
    const decoded = decodeMoveReviewSnapshot(row.snapshot, context(row));
    assert.equal(decoded?.kind, 'completed');
    const source = selected(row.snapshot);
    const shown = decoded!.observations!.selection!.candidates.find(c => c.id === source.id)!;
    assert.deepEqual(shown.detail.summary_question, source.detail.summary_question);
    const moves = new Set([source.detail.reference_line_id, source.detail.candidate_line_id].map(id =>
      row.snapshot.observations.paths.find((path: any) => path.id === id).moves[0]));
    assert.deepEqual(moves, new Set(row.id === 'existing' ? ['c7c6', 'b3d5'] : ['c6d4', 'e5e4']));
    for (const pending of row.pending) {
      const deep = structuredClone(pending);
      deep.issued_engine_work.search_limits.depth = 19;
      assert.equal(decodeMoveReviewSnapshot(deep, context(row)), undefined);
    }
  }
});

test('a foreign move occurrence cannot be substituted for the assessed root', () => {
  for (const row of fixture.cases) {
    const raw = structuredClone(row.snapshot);
    const question = selected(raw).detail.summary_question;
    const move = question.units[0].en.find((part: any) => typeof part !== 'string');
    move.occurrence_id = 'foreign-occurrence';
    const decoded = decodeMoveReviewSnapshot(raw, context(row));
    assert.equal(decoded?.kind, 'completed');
    assert.equal(decoded?.observations?.selection, undefined);
  }
});

test('positional comparison cannot hide the cost of the selected exchanges', () => {
  for (const row of fixture.cases) {
    const raw = structuredClone(row.snapshot);
    const cost = selected(raw).detail.summary_question.units.find((unit: any) => unit.covered_moves.length);
    assert.ok(cost);
    cost.covered_moves = [];
    assert.equal(decodeMoveReviewSnapshot(raw, context(row))?.observations?.selection, undefined);
  }
});
