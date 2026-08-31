import assert from 'node:assert/strict';
import { test } from 'node:test';
import {
  isMoveReviewEngineProfile,
  isMoveReviewWork,
  makeMoveReviewWork,
  moveReviewEngineProfile,
  type Work,
} from '../src/ceval/types';
import {
  moveReviewEngineCapability,
  moveReviewEngineProfileSpec,
} from '../src/ceval/engines/moveReviewEngineProfiles';
import { Protocol } from '../src/ceval/protocol';

type MoveReviewEngineEnvironment = Parameters<typeof moveReviewEngineCapability>[0];

const capableBrowser: MoveReviewEngineEnvironment = {
  features: ['wasm', 'sharedMem', 'simd', 'dynamicImportFromWorker'],
  hardwareConcurrency: 8,
};

const startFen = 'rn2kbnr/ppp1pppp/3q4/3p4/8/3P4/PPP1PPPP/RNBQKBNR w KQkq - 2 3';

test('move review profile identity is the closed server-admitted singleton', () => {
  assert.equal(moveReviewEngineCapability(capableBrowser).profile, 'sf18-smallnet-t2-h16-v1');
  assert.equal(isMoveReviewEngineProfile(moveReviewEngineProfile), true);
  assert.equal(isMoveReviewEngineProfile('sf18-smallnet-single-t1-h16-v1'), false);
  assert.equal(isMoveReviewEngineProfile('sf18-smallnet-t1-h16-v1'), false);
});

test('the bundled t2 profile admits only the runtime-supported standard variant', () => {
  const manifest = moveReviewEngineProfileSpec();
  assert.deepEqual(manifest.variants, ['standard']);
  const capability = moveReviewEngineCapability(capableBrowser);
  assert.equal(capability.supported, true);
  if (!capability.supported) return;
  assert.equal(capability.profile, moveReviewEngineProfile);
  assert.equal(capability.info.minThreads, manifest.threads);
  assert.equal(capability.info.maxThreads, manifest.threads);
  assert.equal(capability.info.maxHash, manifest.hashSize);
  assert.deepEqual(capability.info.variants, manifest.variants);
});

test('the exact t2 profile fails closed without shared memory', () => {
  const noSharedMemory: MoveReviewEngineEnvironment = {
    features: ['wasm', 'simd', 'dynamicImportFromWorker'],
    hardwareConcurrency: 8,
  };
  const capability = moveReviewEngineCapability(noSharedMemory);
  assert.equal(capability.supported, false);
  assert.equal(capability.profile, moveReviewEngineProfile);
  if (capability.supported) return;
  assert.equal(capability.reason, 'missing-browser-feature');
});

test('move review work retains the exact profile and its protocol settings', () => {
  let emitted: Tree.LocalEval | undefined;
  const work = makeMoveReviewWork(moveReviewEngineProfile, {
    variant: 'standard',
    initialFen: startFen,
    currentFen: startFen,
    moves: [],
    path: 'move-review:test',
    ply: 4,
    multiPv: 2,
    searchLimits: { depth: 16, nodes: 2_000_000, movetimeMs: 2_500 },
    rootMoves: ['c7c5', 'e7e5'],
    emit: evaluation => (emitted = evaluation),
  });

  assert.equal(isMoveReviewWork(work), true);
  assert.equal(work.profile, moveReviewEngineProfile);
  assert.equal(work.variant, 'standard');
  assert.equal(work.threads, 2);
  assert.equal(work.hashSize, 16);
  assert.deepEqual(work.search, { depth: 16 });
  assert.deepEqual(work.searchLimits, { depth: 16, nodes: 2_000_000, movetimeMs: 2_500 });
  assert.deepEqual(work.rootMoves, ['c7c5', 'e7e5']);
  assert.equal(work.threatMode, false);

  const evaluation: Tree.LocalEval = { fen: startFen, depth: 16, nodes: 1, millis: 1, pvs: [] };
  work.emit(evaluation);
  assert.equal(emitted, evaluation);
});

test('protocol becomes ready only after readyok and applies the exact move review profile settings', async () => {
  const commands: string[] = [];
  const protocol = new Protocol();
  let ready = false;
  void protocol.ready.then(() => (ready = true));
  protocol.connected(command => commands.push(command));
  protocol.received('uciok');
  await Promise.resolve();
  assert.equal(ready, false);
  protocol.received('readyok');
  await protocol.ready;
  assert.equal(ready, true);

  const ordinaryWork: Work = {
    variant: 'standard',
    threads: 4,
    hashSize: 64,
    gameId: undefined,
    stopRequested: false,
    path: 'ordinary',
    search: { depth: 1 },
    multiPv: 1,
    ply: 0,
    threatMode: false,
    initialFen: startFen,
    currentFen: startFen,
    moves: [],
    emit: () => {},
  };
  protocol.compute(ordinaryWork);
  protocol.received('bestmove e2e4');

  const moveReviewWork = makeMoveReviewWork(moveReviewEngineProfile, {
    variant: 'standard',
    initialFen: startFen,
    currentFen: startFen,
    moves: [],
    path: 'move-review:test',
    ply: 4,
    multiPv: 2,
    searchLimits: { depth: 16, nodes: 5_000_000, movetimeMs: 5_000 },
    rootMoves: [],
    emit: () => {},
  });
  const commandStart = commands.length;
  protocol.compute(moveReviewWork);
  protocol.received('readyok');
  const moveReviewCommands = commands.slice(commandStart);

  assert.equal(moveReviewWork.variant, 'standard');
  assert.ok(moveReviewCommands.includes('setoption name Threads value 2'));
  assert.ok(moveReviewCommands.includes('setoption name Hash value 16'));
  assert.ok(moveReviewCommands.includes('setoption name MultiPV value 2'));
  assert.ok(moveReviewCommands.includes('setoption name Clear Hash'));
  assert.ok(moveReviewCommands.includes(`position fen ${startFen} moves`));
  assert.ok(moveReviewCommands.includes('go depth 16 nodes 5000000 movetime 5000'));
  assert.ok(commands.includes('go depth 1'));
});

test('comparison work appends exactly two issued searchmoves to the bounded go command', async () => {
  const commands: string[] = [];
  const evaluations: Tree.LocalEval[] = [];
  const observations: Array<[number, number]> = [];
  const protocol = new Protocol();
  protocol.connected(command => commands.push(command));
  protocol.received('uciok');
  protocol.received('readyok');
  await protocol.ready;

  const work = makeMoveReviewWork(moveReviewEngineProfile, {
    variant: 'standard',
    initialFen: startFen,
    currentFen: startFen,
    moves: [],
    path: 'move-review:comparison',
    ply: 4,
    multiPv: 2,
    searchLimits: { depth: 16, nodes: 2_000_000, movetimeMs: 2_500 },
    rootMoves: ['c7c5', 'e7e5'],
    observe: (nodes, engineTimeMs) => observations.push([nodes, engineTimeMs]),
    emit: evaluation => evaluations.push(evaluation),
  });
  protocol.compute(work);
  protocol.received('readyok');

  assert.ok(commands.includes('go depth 16 nodes 2000000 movetime 2500 searchmoves c7c5 e7e5'));
  protocol.received('info depth 1 seldepth 3 nodes 100 multipv 1 score cp 20 time 100 pv c7c5');
  assert.equal(evaluations.length, 0);
  protocol.received('info depth 1 seldepth 2 nodes 100 multipv 2 score cp 8 time 100 pv e7e5');
  assert.equal(evaluations.length, 1);
  assert.equal(evaluations[0]?.pvs.length, 2);

  protocol.received('info depth 2 seldepth 4 nodes 200 multipv 1 score cp 22 time 150 pv c7c5');
  protocol.received('info depth 2 seldepth 4 nodes 300 multipv 3 score cp 4 time 200 pv g8f6');
  protocol.received('info depth 2 seldepth 4 nodes 350 multipv 3 score cp 3 time 250 pv g8f6');
  assert.equal(evaluations.length, 1, 'duplicate out-of-range indices cannot fabricate a complete bundle');
  assert.deepEqual(observations.at(-1), [350, 250], 'partial lines still publish physical observations');
  protocol.received('info nodes 450');
  protocol.received('info time 350');
  assert.deepEqual(observations.slice(-2), [
    [450, 0],
    [0, 350],
  ]);
  protocol.received('info depth 2 seldepth 4 nodes 400 multipv 2 score cp 9 time 300 pv e7e5');
  assert.equal(evaluations.length, 2);
  assert.deepEqual(
    evaluations[1]?.pvs.map(pv => pv.moves[0]),
    ['c7c5', 'e7e5'],
  );

  for (const depth of [15, 16]) {
    protocol.received(`info depth ${depth} seldepth 20 nodes 100000 multipv 1 score cp 20 time 500 pv c7c5`);
    protocol.received(`info depth ${depth} seldepth 19 nodes 100000 multipv 2 score cp 8 time 500 pv e7e5`);
  }
  protocol.received('bestmove c7c5');
  assert.equal(evaluations.at(-1)?.depth, 16);
  assert.equal(evaluations.at(-1)?.bestmove, 'c7c5');
  assert.deepEqual(
    evaluations.at(-1)?.pvs.map(pv => pv.moves[0]),
    ['c7c5', 'e7e5'],
  );
});
