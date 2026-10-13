import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { test } from 'node:test';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../../src/review/moveReview';

const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/choice-neutralization-browser.json.gz',
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
  assert.ok(candidate, 'the original purpose must survive delivery');
  return candidate;
}

test('both live screens deliver the original purpose and the same-work Bd5/Nd5 alternative', () => {
  assert.equal(fixture.cases.length, 2);
  for (const row of fixture.cases) {
    assert.ok(row.elapsed_ms < 10_000);
    assert.ok(row.depths.every((depth: number) => depth <= 18));
    const decoded = decodeMoveReviewSnapshot(row.snapshot, context(row));
    assert.equal(decoded?.kind, 'completed');
    const source = selected(row.snapshot);
    const shown = decoded!.observations!.selection!.candidates.find(c => c.id === source.id)!;
    assert.deepEqual(shown.detail.summary_question, source.detail.summary_question);
    const owner = source.detail.summary_question.move_purpose;
    assert.equal(owner.choice.alternative_move, row.id === 'existing' ? 'c3d5' : 'b6d5');
    const sourcePath = row.snapshot.observations.paths.find((p: any) => p.id === owner.source_line_id);
    assert.ok(sourcePath.evaluations.some((e: any) => e.source === 'investigation_comparison'),
      'a refreshed root PV must remain eligible as the purpose-response source');
    for (const pending of row.pending) {
      assert.equal(decodeMoveReviewSnapshot(pending, context(row))?.kind,
        !pending.assessment && pending.issued_engine_work.purpose === 'root_search' ? 'awaiting-core' : 'awaiting-evidence');
      const deep = structuredClone(pending);
      deep.issued_engine_work.search_limits.depth = 19;
      assert.equal(decodeMoveReviewSnapshot(deep, context(row)), undefined);
    }
  }
});

test('foreign comparisons, swapped paths and a different destination cannot own this purpose', () => {
  for (const row of fixture.cases) {
    const original = decodeMoveReviewSnapshot(row.snapshot, context(row))!;
    for (const mutate of [
      (choice: any) => { choice.execution_key_sha256 = '0'.repeat(64); },
      (choice: any) => { choice.line_ids.reverse(); },
      (choice: any) => { choice.alternative_move = choice.alternative_move.slice(0, 2) + 'e4'; },
      (choice: any) => { choice.line_ids[1] = choice.line_ids[0]; },
    ]) {
      const raw = structuredClone(row.snapshot);
      mutate(selected(raw).detail.summary_question.move_purpose.choice);
      const decoded = decodeMoveReviewSnapshot(raw, context(row))!;
      assert.equal(decoded.kind, 'completed');
      assert.equal(decoded.observations?.selection, undefined);
      assert.deepEqual(decoded.evidence, original.evidence);
    }
  }
});

test('neutralization and route explanations still owe every selected exchange cost', () => {
  for (const row of fixture.cases) {
    const raw = structuredClone(row.snapshot);
    const cost = selected(raw).detail.summary_question.units.find((unit: any) => unit.covered_moves.length);
    assert.ok(cost);
    cost.covered_moves = [];
    assert.equal(decodeMoveReviewSnapshot(raw, context(row))?.observations?.selection, undefined);
  }
});
