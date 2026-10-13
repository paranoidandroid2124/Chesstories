import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { test } from 'node:test';
import { decodeMoveReviewSnapshot, type MoveReviewSubject } from '../../src/review/moveReview';

const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../../judgment-evaluation/fixtures/public-commentary-v6/redeployment-purpose-browser.json.gz',
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

test('the two live ten-second screens retain their purpose, owned reply work, and original evaluation', () => {
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
    assert.equal(q.kind, 'piece_pressure');
    assert.deepEqual(q.plan!.targets.map(t => t.square).sort(), row.san === 'Ng8' ? ['e3', 'h2'] : ['e4']);
    assert.deepEqual(shown.detail.summary_question!.move_purpose!.id, q.id);
    assert.ok(String(shown.detail.summary).includes(row.san === 'Ng8' ? 'knight on c4' : 'f-file'));
    for (const pending of row.pending) {
      assert.equal(decodeMoveReviewSnapshot(pending, context(row))?.kind,
        pending.assessment ? 'awaiting-evidence' : 'awaiting-core');
      const changed = structuredClone(pending);
      changed.issued_engine_work.search_limits.depth = 19;
      assert.equal(decodeMoveReviewSnapshot(changed, context(row)), undefined);
    }
  }
});

test('a changed target, invented route, or foreign work cannot substitute another purpose', () => {
  for (const row of fixture.cases) {
    const original = decodeMoveReviewSnapshot(row.snapshot, context(row))!;
    for (const mutate of [
      (raw: any) => { raw.observations.selection.purpose_question.plan.targets[0].square = 'a1'; },
      (raw: any) => {
        const p = raw.observations.selection.purpose_question.plan;
        if (p.use < 0) p.squares[1] = p.squares[0];
        else p.route = [p.start, p.start + 1];
      },
      (raw: any) => { selected(raw).detail.summary_question.move_purpose.execution_key_sha256 = '0'.repeat(64); },
    ]) {
      const raw = structuredClone(row.snapshot);
      mutate(raw);
      const decoded = decodeMoveReviewSnapshot(raw, context(row))!;
      assert.equal(decoded.kind, 'completed');
      assert.equal(decoded.observations?.selection, undefined);
      assert.deepEqual(decoded.evidence, original.evidence);
    }
  }
});

test('shortening the answer does not license dropping a selected exchange price', () => {
  const row = fixture.cases.find((r: any) => r.san === 'Ng8');
  const raw = structuredClone(row.snapshot);
  const question = selected(raw).detail.summary_question;
  const cost = question.units.find((unit: any) => unit.covered_moves.length);
  assert.ok(cost, 'the actual Ng8 screen includes selected captures');
  cost.covered_moves = [];
  const decoded = decodeMoveReviewSnapshot(raw, context(row));
  assert.equal(decoded?.kind, 'completed');
  assert.equal(decoded.observations?.selection, undefined);
});
