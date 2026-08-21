import assert from 'node:assert/strict';
import { test } from 'node:test';
import {
  isMoveReviewEngineProfile,
  isMoveReviewWork,
  makeMoveReviewWork,
  moveReviewEngineProfile,
  type MoveReviewEngineProfile,
  type Work,
} from '../src/ceval/types';
import {
  moveReviewEngineCapabilities,
  moveReviewEngineProfileSpec,
} from '../src/ceval/engines/moveReviewEngineProfiles';
import { Protocol } from '../src/ceval/protocol';

type MoveReviewEngineEnvironment = Parameters<typeof moveReviewEngineCapabilities>[0];
const moveReviewFallbackEngineProfile: MoveReviewEngineProfile = 'sf18-smallnet-single-t1-h16-v1';

const capableBrowser: MoveReviewEngineEnvironment = {
  features: ['wasm', 'sharedMem', 'simd', 'dynamicImportFromWorker'],
  hardwareConcurrency: 8,
};

const startFen = 'rn2kbnr/ppp1pppp/3q4/3p4/8/3P4/PPP1PPPP/RNBQKBNR w KQkq - 2 3';

test('move review profile identities are distinct and parse as a closed union', () => {
  assert.deepEqual(
    moveReviewEngineCapabilities(capableBrowser).map(capability => capability.profile),
    ['sf18-smallnet-t2-h16-v1', 'sf18-smallnet-single-t1-h16-v1'],
  );
  assert.equal(isMoveReviewEngineProfile(moveReviewEngineProfile), true);
  assert.equal(isMoveReviewEngineProfile(moveReviewFallbackEngineProfile), true);
  assert.equal(isMoveReviewEngineProfile('sf18-smallnet-t1-h16-v1'), false);
  assert.notEqual(moveReviewEngineProfile, moveReviewFallbackEngineProfile);
});

test('the bundled t2 profile admits standard and Chess960 on a capable browser', () => {
  const manifest = moveReviewEngineProfileSpec(moveReviewEngineProfile);
  assert.deepEqual(manifest.variants, ['standard', 'chess960']);
  const capability = moveReviewEngineCapabilities(capableBrowser)[0];
  assert.equal(capability.supported, true);
  if (!capability.supported) return;
  assert.equal(capability.profile, moveReviewEngineProfile);
  assert.equal(capability.info.minThreads, manifest.threads);
  assert.equal(capability.info.maxThreads, manifest.threads);
  assert.equal(capability.info.maxHash, manifest.hashSize);
  assert.deepEqual(capability.info.variants, manifest.variants);
});

test('the exact t1 artifact admits Standard and Chess960 without shared memory', () => {
  const manifest = moveReviewEngineProfileSpec(moveReviewFallbackEngineProfile);
  assert.equal(manifest.loader, 'stockfish-web-single-thread-worker');
  assert.deepEqual(manifest.variants, ['standard', 'chess960']);
  assert.deepEqual(manifest.assets, {
    root: 'npm/stockfish-web-move-review',
    js: 'sf_18_smallnet_single.js',
    wasm: 'sf_18_smallnet_single.wasm',
  });
  const singleThreadBrowser: MoveReviewEngineEnvironment = {
    features: ['wasm', 'simd'],
    hardwareConcurrency: 1,
  };
  const capability = moveReviewEngineCapabilities(singleThreadBrowser)[1];
  assert.equal(capability.supported, true);
  assert.equal(capability.profile, moveReviewFallbackEngineProfile);
});

test('selection falls back once from t2 to the exact t1 profile without shared memory', () => {
  const noSharedMemory: MoveReviewEngineEnvironment = {
    features: ['wasm', 'simd', 'dynamicImportFromWorker'],
    hardwareConcurrency: 8,
  };
  const capabilities = moveReviewEngineCapabilities(noSharedMemory);

  assert.equal(
    capabilities.find(capability => capability.supported)?.profile,
    moveReviewFallbackEngineProfile,
  );
  assert.deepEqual(
    capabilities.map(capability => [
      capability.profile,
      capability.supported ? 'supported' : capability.reason,
    ]),
    [
      [moveReviewEngineProfile, 'missing-browser-feature'],
      [moveReviewFallbackEngineProfile, 'supported'],
    ],
  );
});

test('move review work retains the selected profile and its protocol settings', () => {
  let emitted: Tree.LocalEval | undefined;
  const work = makeMoveReviewWork(moveReviewFallbackEngineProfile, {
    variant: 'chess960',
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
  assert.equal(work.profile, moveReviewFallbackEngineProfile);
  assert.equal(work.variant, 'chess960');
  assert.equal(work.threads, 1);
  assert.equal(work.hashSize, 16);
  assert.deepEqual(work.search, { depth: 16 });
  assert.deepEqual(work.searchLimits, { depth: 16, nodes: 2_000_000, movetimeMs: 2_500 });
  assert.deepEqual(work.rootMoves, ['c7c5', 'e7e5']);
  assert.equal(work.threatMode, false);

  const evaluation: Tree.LocalEval = { fen: startFen, depth: 16, nodes: 1, millis: 1, pvs: [] };
  work.emit(evaluation);
  assert.equal(emitted, evaluation);
});

test('protocol becomes ready only after readyok and applies each move review profile settings', async () => {
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

  const moveReviewWork = makeMoveReviewWork(moveReviewFallbackEngineProfile, {
    variant: 'chess960',
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

  assert.ok(moveReviewCommands.includes('setoption name UCI_Variant value chess960'));
  assert.ok(moveReviewCommands.includes('setoption name Threads value 1'));
  assert.ok(moveReviewCommands.includes('setoption name Hash value 16'));
  assert.ok(moveReviewCommands.includes('setoption name MultiPV value 2'));
  assert.ok(moveReviewCommands.includes('setoption name Clear Hash'));
  assert.ok(moveReviewCommands.includes(`position fen ${startFen} moves`));
  assert.ok(moveReviewCommands.includes('go depth 16 nodes 3750000 movetime 5000'));
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

  assert.ok(commands.includes('go depth 16 nodes 1500000 movetime 2500 searchmoves c7c5 e7e5'));
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
