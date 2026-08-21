import assert from 'node:assert/strict';
import { test } from 'node:test';
import { JSDOM } from 'jsdom';
import {
  moveReviewEngineCapabilities,
  moveReviewEngineProfile,
  type MoveReviewEngineSupportedCapability,
  type MoveReviewEngineProfile,
} from '../src/ceval/engines/moveReviewEngineProfiles';
import type { Engines } from '../src/ceval/engines/engines';
import type {
  CevalState,
  CevalOpts,
  EngineInfo,
  EngineNotifier,
  MoveReviewEngine,
  Work,
} from '../src/ceval/types';

const startFen = 'rn2kbnr/ppp1pppp/3q4/3p4/8/3P4/PPP1PPPP/RNBQKBNR w KQkq - 2 3';
const moveReviewFallbackEngineProfile: MoveReviewEngineProfile = 'sf18-smallnet-single-t1-h16-v1';
const evaluation: Tree.LocalEval = {
  fen: startFen,
  depth: 16,
  nodes: 1,
  millis: 1,
  pvs: [],
};

test('CevalCtrl owns move-review fallback and stale-emission boundaries', async t => {
  const globals = installDom();

  try {
    const { default: CevalCtrl } = await import('../src/ceval/ctrl');
    const capabilities = moveReviewEngineCapabilities({
      features: ['wasm', 'sharedMem', 'simd', 'dynamicImportFromWorker'],
      hardwareConcurrency: 8,
    }).filter((capability): capability is MoveReviewEngineSupportedCapability => capability.supported);

    await t.test('a failed t2 boot tries the exact t1 fallback once', async () => {
      const ctrl = new CevalCtrl(opts());
      const attempts: string[] = [];
      const workers: FakeMoveReviewEngine[] = [];
      const readyResults = [false, true];
      ctrl.engines = fakeEngines(capabilities, capability => {
        attempts.push(capability.profile);
        const worker = new FakeMoveReviewEngine(capability.info, readyResults.shift()!);
        workers.push(worker);
        return worker;
      });

      const result = await ctrl.preflightMoveReviewSelection(
        { variant: 'standard', signal: new AbortController().signal },
        () => {},
      );

      assert.deepEqual(result, { ok: true, profile: moveReviewFallbackEngineProfile });
      assert.deepEqual(attempts, [moveReviewEngineProfile, moveReviewFallbackEngineProfile]);
      assert.equal(workers[0].destroyCount, 1);
      assert.equal(workers[1].destroyCount, 0);

      ctrl.stopMoveReview();
      assert.equal(workers[1].destroyCount, 1);
    });

    await t.test('replaced and cancelled workers cannot emit review results', async () => {
      const ctrl = new CevalCtrl(opts());
      const workers: FakeMoveReviewEngine[] = [];
      ctrl.engines = fakeEngines(capabilities, capability => {
        const worker = new FakeMoveReviewEngine(capability.info, true);
        workers.push(worker);
        return worker;
      });

      const first = await ctrl.preflightMoveReviewSelection(
        { variant: 'standard', signal: new AbortController().signal },
        () => {},
      );
      assert.deepEqual(first, { ok: true, profile: moveReviewEngineProfile });
      const firstEmissions: Tree.LocalEval[] = [];
      assert.equal(ctrl.startMoveReview(moveReviewEngineProfile, workInput(firstEmissions)), true);
      const firstWork = workers[0].work!;

      const replacement = await ctrl.preflightMoveReviewSelection(
        { variant: 'standard', signal: new AbortController().signal },
        () => {},
      );
      assert.deepEqual(replacement, { ok: true, profile: moveReviewEngineProfile });
      firstWork.emit(evaluation);
      assert.equal(firstEmissions.length, 0);

      const replacementEmissions: Tree.LocalEval[] = [];
      assert.equal(ctrl.startMoveReview(moveReviewEngineProfile, workInput(replacementEmissions)), true);
      const replacementWork = workers[1].work!;
      replacementWork.emit(evaluation);
      assert.equal(replacementEmissions.length, 1);

      ctrl.stopMoveReview();
      replacementWork.emit(evaluation);
      assert.equal(replacementEmissions.length, 1);
    });
  } finally {
    globals.restore();
  }
});

class FakeMoveReviewEngine implements MoveReviewEngine {
  readonly ready: Promise<boolean>;
  work?: Work;
  destroyCount = 0;

  constructor(
    private readonly info: EngineInfo,
    ready: boolean,
  ) {
    this.ready = Promise.resolve(ready);
  }

  getInfo(): EngineInfo {
    return this.info;
  }

  getState(): CevalState {
    return 0 as CevalState;
  }

  start(work: Work): void {
    this.work = work;
  }

  stop(): void {}

  destroy(): void {
    this.destroyCount += 1;
  }
}

function fakeEngines(
  capabilities: readonly MoveReviewEngineSupportedCapability[],
  make: (capability: MoveReviewEngineSupportedCapability, status: EngineNotifier) => MoveReviewEngine,
): Engines {
  return {
    moveReviewCapabilities: () => capabilities,
    makeMoveReview: make,
  } as unknown as Engines;
}

function opts(): CevalOpts {
  return {
    variant: { key: 'standard' } as Variant,
    initialFen: undefined,
    emit: () => {},
    onUciHover: () => {},
    redraw: () => {},
  };
}

function workInput(emissions: Tree.LocalEval[]) {
  return {
    variant: 'standard' as const,
    initialFen: startFen,
    currentFen: startFen,
    moves: [],
    path: 'move-review:test',
    ply: 4,
    multiPv: 2,
    searchLimits: { depth: 16 as const, nodes: 5_000_000 as const, movetimeMs: 5_000 as const },
    rootMoves: [],
    emit: (evaluation: Tree.LocalEval) => emissions.push(evaluation),
  };
}

function installDom(): { restore: () => void } {
  const dom = new JSDOM('<!doctype html>', { pretendToBeVisual: true, url: 'https://chesstory.test/' });
  const originalWindow = Object.getOwnPropertyDescriptor(globalThis, 'window');
  const originalDocument = Object.getOwnPropertyDescriptor(globalThis, 'document');
  const originalNavigator = Object.getOwnPropertyDescriptor(globalThis, 'navigator');
  Object.defineProperty(globalThis, 'window', { configurable: true, value: dom.window });
  Object.defineProperty(globalThis, 'document', { configurable: true, value: dom.window.document });
  Object.defineProperty(globalThis, 'navigator', { configurable: true, value: dom.window.navigator });

  return {
    restore: () => {
      if (originalWindow) Object.defineProperty(globalThis, 'window', originalWindow);
      else Reflect.deleteProperty(globalThis, 'window');
      if (originalDocument) Object.defineProperty(globalThis, 'document', originalDocument);
      else Reflect.deleteProperty(globalThis, 'document');
      if (originalNavigator) Object.defineProperty(globalThis, 'navigator', originalNavigator);
      else Reflect.deleteProperty(globalThis, 'navigator');
      dom.window.close();
    },
  };
}
