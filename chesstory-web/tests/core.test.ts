import assert from 'node:assert/strict';
import { test } from 'node:test';
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { GameRecord } from '../src/game';
import { decodeMoveReviewSnapshot } from '../src/review/moveReview';
import { BrowserEngine } from '../src/engine/browserEngine';

test('PGN history, variations and comments survive a local save and a new branch', () => {
  const game = GameRecord.parse('[White "Player A"]\n[Black "Player B"]\n\n1. e4 {Center} e5 (1... c5 $1) 2. Nf3 *');
  const e4 = game.mainline()[0], nf3 = game.mainline()[2];
  assert.deepEqual(game.subject(nf3.path)?.movePrefixUci, ['e2e4', 'e7e5']);
  game.append(e4, 'c7c6');
  const restored = GameRecord.parse(game.export());
  assert.equal(restored.mainline()[0].data?.comments?.[0], 'Center');
  assert.deepEqual(restored.mainline()[0].children.map(n => n.uci), ['e7e5', 'c7c5', 'c7c6']);
  assert.deepEqual(restored.mainline()[0].children[1].data?.nags, [1]);
  assert.equal(restored.nodes.size, game.nodes.size);
});

test('illegal input is rejected; castling and underpromotion keep legal history', () => {
  assert.throws(() => GameRecord.parse('1. e4 e5 2. Qh5 Ke6 *'));
  assert.throws(() => GameRecord.parse('not a chess game'));
  assert.throws(() => GameRecord.parse('8/8/8/8/8/8/8/8 w - - 0 1'));
  const castle = GameRecord.parse('r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1');
  const after = castle.append(castle.root, 'e1h1');
  assert.equal(after.uci, 'e1g1');
  assert.equal(after.san, 'O-O');
  assert.equal(GameRecord.parse(castle.export()).mainline()[0].fen, after.fen);
  const promotion = GameRecord.parse('7k/P7/8/8/8/8/8/7K w - - 0 1');
  assert.equal(promotion.append(promotion.root, 'a7a8n').san, 'a8=N');
});

const fixture = JSON.parse(gunzipSync(readFileSync(new URL(
  '../../judgment-evaluation/fixtures/public-commentary-v6/evaluation-reference-browser.json.gz', import.meta.url,
))).toString());

function context(row: any) {
  const raw = row.request;
  return { requestId: raw.request_id, engineProfile: raw.engine_profile, subject: {
    variant: 'standard' as const, initialFen: raw.initial_fen, movePrefixUci: raw.move_prefix_uci,
    before: { path: '', fen: raw.current_fen }, after: { path: 'aa', fen: raw.focus.resulting_fen },
    played: { uci: raw.focus.played_move_uci, san: row.san },
  } };
}

test('the extracted decoder retains both real purpose explanations and rejects wrong ownership / D19', () => {
  for (const row of fixture.cases) {
    const decoded = decodeMoveReviewSnapshot(row.snapshot, context(row));
    assert.equal(decoded?.kind, 'completed');
    assert.ok(decoded?.observations?.selection);
    assert.equal(decodeMoveReviewSnapshot(row.snapshot, { ...context(row), requestId: 'foreign' }), undefined);
    for (const pending of row.pending) {
      assert.ok(decodeMoveReviewSnapshot(pending, context(row)));
      const tooDeep = structuredClone(pending); tooDeep.issued_engine_work.search_limits.depth = 19;
      assert.equal(decodeMoveReviewSnapshot(tooDeep, context(row)), undefined);
    }
  }
});

test('cancellation and expired leases finish without starting an engine', async () => {
  const row = fixture.cases[0], pending = decodeMoveReviewSnapshot(row.pending[0], context(row));
  assert.ok(pending && (pending.kind === 'awaiting-core' || pending.kind === 'awaiting-evidence'));
  const engine = new BrowserEngine(), abort = new AbortController(); abort.abort();
  const cancelled = await engine.execute(pending.issuedEngineWork, performance.now(), abort.signal);
  assert.equal(cancelled.kind, 'executor_failed');
  if (cancelled.kind === 'executor_failed') assert.equal(cancelled.failureCode, 'cancelled');
  const expired = await engine.execute(pending.issuedEngineWork, performance.now(), new AbortController().signal, performance.now() - 1);
  assert.equal(expired.kind, 'executor_failed');
  if (expired.kind === 'executor_failed') assert.equal(expired.failureCode, 'lease_expired');
  const deep = await engine.execute({ ...pending.issuedEngineWork, searchLimits: { ...pending.issuedEngineWork.searchLimits, depth: 19 } },
    performance.now(), new AbortController().signal);
  assert.equal(deep.kind, 'executor_failed');
  if (deep.kind === 'executor_failed') assert.equal(deep.failureCode, 'engine_failure');
  engine.destroy();
});
