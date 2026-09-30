import assert from 'node:assert/strict';
import { test } from 'node:test';
import { Protocol } from '../src/ceval/protocol';
import { makeMoveReviewWork, moveReviewEngineProfile, type Work } from '../src/ceval/types';

test('review engine uses standard castling for issued history, root restriction and PV, then restores live notation', () => {
  const commands: string[] = [];
  const protocol = new Protocol();
  protocol.connected(command => commands.push(command));
  protocol.received('uciok');
  protocol.received('readyok');
  const fen = 'r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1';
  const evaluations: Tree.LocalEval[] = [];
  const work = makeMoveReviewWork(moveReviewEngineProfile, {
    variant: 'standard',
    path: '',
    ply: 0,
    initialFen: fen,
    currentFen: fen,
    moves: [],
    multiPv: 2,
    searchLimits: { depth: 16, nodes: 2_000_000, movetimeMs: 2_500 },
    rootMoves: ['e1g1', 'e1c1'],
    emit: evaluation => evaluations.push(evaluation),
  });
  protocol.compute(work);
  const changed = commands.indexOf('setoption name UCI_Chess960 value false');
  assert(changed >= 0);
  assert.equal(commands.at(-1), 'isready');
  protocol.received('readyok');
  assert(commands.at(-1)?.endsWith('searchmoves e1g1 e1c1'));
  protocol.received('info depth 16 seldepth 16 multipv 1 score cp 30 nodes 10 time 1 pv e1g1');
  protocol.received('info depth 16 seldepth 16 multipv 2 score cp 20 nodes 10 time 1 pv e1c1');
  assert.deepEqual(
    evaluations.at(-1)?.pvs.map(pv => pv.moves),
    [['e1g1'], ['e1c1']],
  );
  protocol.received('bestmove e1g1');
  const live: Work = { ...work, search: { depth: 16 } };
  // Remove the dedicated-work marker: this is an ordinary analysis request.
  delete (live as Work & { profile?: string }).profile;
  protocol.compute(live);
  assert(commands.lastIndexOf('setoption name UCI_Chess960 value true') > changed);
});

test('cancelled review keeps final usage without admitting late PVs or mixing the next work', () => {
  const commands: string[] = [];
  const protocol = new Protocol();
  protocol.connected(command => commands.push(command));
  protocol.received('uciok');
  protocol.received('readyok');
  const fen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
  const oldUsage: Array<[number, number]> = [];
  const nextUsage: Array<[number, number]> = [];
  const oldEvals: Tree.LocalEval[] = [];
  const nextEvals: Tree.LocalEval[] = [];
  const work = (path: string, usage: Array<[number, number]>, evaluations: Tree.LocalEval[]) =>
    makeMoveReviewWork(moveReviewEngineProfile, {
      variant: 'standard',
      path,
      ply: 0,
      initialFen: fen,
      currentFen: fen,
      moves: [],
      multiPv: 1,
      searchLimits: { depth: 16, nodes: 2_000_000, movetimeMs: 2_500 },
      rootMoves: [],
      observe: (nodes, millis) => usage.push([nodes, millis]),
      emit: evaluation => evaluations.push(structuredClone(evaluation)),
    });
  const oldWork = work('old', oldUsage, oldEvals);
  protocol.compute(oldWork);
  for (const setting of ['Threads value 2', 'Hash value 16', 'MultiPV value 1'])
    assert(commands.includes(`setoption name ${setting}`), `first review must explicitly set ${setting}`);
  protocol.received('readyok');
  protocol.received('info depth 15 multipv 1 score cp 12 nodes 191876 time 146 pv e2e4');
  assert.equal(oldEvals.length, 1);

  protocol.compute(undefined);
  assert.equal(commands.at(-1), 'stop');
  assert.equal(oldWork.stopRequested, true);
  assert.equal(protocol.isComputing(), false);
  const nextWork = work('next', nextUsage, nextEvals);
  protocol.compute(nextWork);
  assert.equal(commands.at(-1), 'stop', 'the queued work cannot start before old bestmove');
  protocol.received('info depth 16 multipv 1 score cp 25 nodes 286932 time 204 pv d2d4');
  protocol.received('info nodes 287100');
  protocol.received('info time 205');
  assert.deepEqual(oldUsage.slice(-3), [
    [286932, 204],
    [287100, 0],
    [0, 205],
  ]);
  assert.equal(oldEvals.length, 1, 'post-stop info cannot emit a new evaluation');
  assert.deepEqual(oldEvals[0]?.pvs[0]?.moves, ['e2e4']);
  assert.equal(nextUsage.length, 0);
  assert.equal(nextEvals.length, 0);

  protocol.received('bestmove d2d4');
  assert.equal(oldEvals.at(-1)?.depth, 15, 'stop acknowledgement cannot promote the late depth16 PV');
  assert.deepEqual(oldEvals.at(-1)?.pvs[0]?.moves, ['e2e4']);
  assert.equal(commands.at(-1), 'isready');
  assert.equal(protocol.isComputing(), false);
  const oldUsageCount = oldUsage.length;
  protocol.received('info depth 16 nodes 999999 time 999 pv d2d4');
  assert.equal(oldUsage.length, oldUsageCount, 'old ownership ends at bestmove');
  assert.equal(nextUsage.length, 0, 'the readiness barrier does not own search statistics');
  protocol.received('readyok');
  assert(commands.at(-1)?.startsWith('go '));
  protocol.received('info depth 1 multipv 1 score cp 5 nodes 33 time 2 pv g1f3');
  assert.deepEqual(nextUsage, [[33, 2]]);
  assert.equal(oldUsage.length, oldUsageCount);
  assert.equal(nextEvals[0]?.depth, 1);
  assert.deepEqual(nextEvals[0]?.pvs[0]?.moves, ['g1f3']);
  protocol.received('bestmove g1f3');
  assert.equal(nextEvals.at(-1)?.nodes, 33);
});
