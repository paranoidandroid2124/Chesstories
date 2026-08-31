import assert from 'node:assert/strict';
import { test } from 'node:test';
import { JSDOM } from 'jsdom';
import {
  moveReviewEngineCapability,
  moveReviewEngineProfile,
  type MoveReviewEngineSupportedCapability,
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
const evaluation: Tree.LocalEval = {
  fen: startFen,
  depth: 16,
  nodes: 1,
  millis: 1,
  pvs: [],
};

test('CevalCtrl owns exact-profile preflight and stale-emission boundaries', async t => {
  const globals = installDom();

  try {
    const { default: CevalCtrl } = await import('../src/ceval/ctrl');
    const capability = moveReviewEngineCapability({
      features: ['wasm', 'sharedMem', 'simd', 'dynamicImportFromWorker'],
      hardwareConcurrency: 8,
    });
    assert.equal(capability.supported, true);
    if (!capability.supported) return;

    await t.test('a failed exact-profile boot fails closed without a second attempt', async () => {
      const ctrl = new CevalCtrl(opts());
      const attempts: string[] = [];
      const workers: FakeMoveReviewEngine[] = [];
      ctrl.engines = fakeEngines(capability, selected => {
        attempts.push(selected.profile);
        const worker = new FakeMoveReviewEngine(selected.info, false);
        workers.push(worker);
        return worker;
      });

      const result = await ctrl.preflightMoveReviewSelection(
        { variant: 'standard', signal: new AbortController().signal },
        () => {},
      );

      assert.deepEqual(result, {
        ok: false,
        failures: [{ profile: moveReviewEngineProfile, reason: 'preflight-failed' }],
      });
      assert.deepEqual(attempts, [moveReviewEngineProfile]);
      assert.equal(workers[0].destroyCount, 1);
    });

    await t.test('replaced and cancelled workers cannot emit review results', async () => {
      const ctrl = new CevalCtrl(opts());
      const workers: FakeMoveReviewEngine[] = [];
      ctrl.engines = fakeEngines(capability, selected => {
        const worker = new FakeMoveReviewEngine(selected.info, true);
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
  capability: MoveReviewEngineSupportedCapability,
  make: (capability: MoveReviewEngineSupportedCapability, status: EngineNotifier) => MoveReviewEngine,
): Engines {
  return {
    moveReviewCapability: () => capability,
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
