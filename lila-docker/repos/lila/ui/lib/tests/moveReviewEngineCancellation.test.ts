import assert from 'node:assert/strict';
import { test } from 'node:test';
import { JSDOM } from 'jsdom';
import { bigFileStorage } from '../src/bigFileStorage';
import { makeMoveReviewWork, moveReviewEngineProfile, type BrowserEngineInfo } from '../src/ceval/types';

const engineInfo: BrowserEngineInfo = {
  id: 'move-review-cancel-test',
  name: 'Move Review cancellation test',
  short: 'test',
  tech: 'NNUE',
  requires: [],
  assets: {
    root: 'npm/stockfish-web',
    js: 'sf_18_smallnet.js',
  },
};

test('preflight abort cancels the t2 worker-script fetch', async () => {
  const originalFetch = globalThis.fetch;
  const originalSite = Object.getOwnPropertyDescriptor(globalThis, 'site');
  const originalWindow = Object.getOwnPropertyDescriptor(globalThis, 'window');
  const originalDocument = Object.getOwnPropertyDescriptor(globalThis, 'document');
  const originalNavigator = Object.getOwnPropertyDescriptor(globalThis, 'navigator');
  const dom = new JSDOM('<!doctype html>', { pretendToBeVisual: true });
  let fetchSignal: AbortSignal | null | undefined;
  Object.defineProperty(globalThis, 'window', {
    configurable: true,
    value: dom.window,
  });
  Object.defineProperty(globalThis, 'document', {
    configurable: true,
    value: dom.window.document,
  });
  Object.defineProperty(globalThis, 'navigator', {
    configurable: true,
    value: dom.window.navigator,
  });
  Object.defineProperty(globalThis, 'site', {
    configurable: true,
    value: { asset: { url: (path: string) => path } },
  });
  globalThis.fetch = ((_input, init) => {
    fetchSignal = init?.signal;
    return new Promise((_resolve, reject) =>
      fetchSignal?.addEventListener('abort', () => reject(fetchSignal?.reason), { once: true }),
    );
  }) as typeof fetch;

  try {
    const { StockfishWebEngine } = await import('../src/ceval/engines/stockfishWebEngine');
    const preflight = new AbortController();
    const engine = new StockfishWebEngine(engineInfo, undefined, preflight.signal);
    assert.equal(fetchSignal?.aborted, false);
    preflight.abort();
    assert.equal(fetchSignal?.aborted, true);
    assert.equal(await engine.ready, false);
  } finally {
    globalThis.fetch = originalFetch;
    if (originalSite) Object.defineProperty(globalThis, 'site', originalSite);
    else Reflect.deleteProperty(globalThis, 'site');
    if (originalWindow) Object.defineProperty(globalThis, 'window', originalWindow);
    else Reflect.deleteProperty(globalThis, 'window');
    if (originalDocument) Object.defineProperty(globalThis, 'document', originalDocument);
    else Reflect.deleteProperty(globalThis, 'document');
    if (originalNavigator) Object.defineProperty(globalThis, 'navigator', originalNavigator);
    else Reflect.deleteProperty(globalThis, 'navigator');
    dom.window.close();
  }
});

test('preflight abort cancels an NNUE request it started and evicts the failed entry', async () => {
  const originalXhr = Object.getOwnPropertyDescriptor(globalThis, 'XMLHttpRequest');
  Object.defineProperty(globalThis, 'XMLHttpRequest', {
    configurable: true,
    value: FakeXMLHttpRequest,
  });
  const storage = bigFileStorage();
  const url = 'https://example.test/move-review-cancel.nnue';

  try {
    const preflight = new AbortController();
    const first = storage.get(url, undefined, preflight.signal);
    const firstRequest = FakeXMLHttpRequest.latest!;
    preflight.abort();
    await assert.rejects(first, error => error instanceof DOMException && error.name === 'AbortError');
    assert.equal(firstRequest.aborted, true);

    const retry = storage.get(url);
    const retryRequest = FakeXMLHttpRequest.latest!;
    assert.notEqual(retryRequest, firstRequest);
    retryRequest.succeed(new Uint8Array([1, 2, 3]));
    assert.deepEqual(await retry, new Uint8Array([1, 2, 3]));
  } finally {
    await storage.delete(url);
    if (originalXhr) Object.defineProperty(globalThis, 'XMLHttpRequest', originalXhr);
    else Reflect.deleteProperty(globalThis, 'XMLHttpRequest');
  }
});

test('aborting a waiter does not cancel an NNUE fetch already owned by live ceval', async () => {
  const originalXhr = Object.getOwnPropertyDescriptor(globalThis, 'XMLHttpRequest');
  Object.defineProperty(globalThis, 'XMLHttpRequest', {
    configurable: true,
    value: FakeXMLHttpRequest,
  });
  const storage = bigFileStorage();
  const url = 'https://example.test/shared-live-ceval.nnue';

  try {
    const owner = storage.get(url);
    const request = FakeXMLHttpRequest.latest!;
    const preflight = new AbortController();
    const waiter = storage.get(url, undefined, preflight.signal);
    preflight.abort();
    await assert.rejects(waiter, error => error instanceof DOMException && error.name === 'AbortError');
    assert.equal(request.aborted, false);
    request.succeed(new Uint8Array([4, 5, 6]));
    assert.deepEqual(await owner, new Uint8Array([4, 5, 6]));
  } finally {
    await storage.delete(url);
    if (originalXhr) Object.defineProperty(globalThis, 'XMLHttpRequest', originalXhr);
    else Reflect.deleteProperty(globalThis, 'XMLHttpRequest');
  }
});

test('worker boot cancellation owns readiness and late modules', async t => {
  const dom = new JSDOM('<!doctype html>', { pretendToBeVisual: true });
  const globals = {
    window: dom.window,
    document: dom.window.document,
    navigator: dom.window.navigator,
    site: { asset: { url: (path: string) => path } },
    __testStockfishFactory: () => Promise.resolve({}),
  };
  const descriptors = Object.keys(globals).map(
    key => [key, Object.getOwnPropertyDescriptor(globalThis, key)] as const,
  );
  for (const [key, value] of Object.entries(globals))
    Object.defineProperty(globalThis, key, { configurable: true, writable: true, value });
  const originalFetch = globalThis.fetch;
  const originalCreateUrl = URL.createObjectURL;
  const originalRevokeUrl = URL.revokeObjectURL;
  const revoked: string[] = [];
  globalThis.fetch = (async () => new Response('mock worker')) as typeof fetch;
  URL.createObjectURL = () => 'data:text/javascript,export default () => globalThis.__testStockfishFactory()';
  URL.revokeObjectURL = url => revoked.push(url);

  try {
    const { StockfishWebEngine } = await import('../src/ceval/engines/stockfishWebEngine');
    const info: BrowserEngineInfo = { ...engineInfo, tech: 'HCE', minMem: 1 };
    const factory = (make: () => Promise<unknown>) =>
      Object.defineProperty(globalThis, '__testStockfishFactory', { configurable: true, value: make });
    const module = (replyReady: boolean) => {
      const waiting = Promise.withResolvers<void>();
      const commands: string[] = [];
      const worker = {
        listen: (_line: string): void => {},
        uci(command: string): void {
          commands.push(command);
          if (command === 'uci') queueMicrotask(() => worker.listen('uciok'));
          if (command === 'isready') {
            waiting.resolve();
            if (replyReady) queueMicrotask(() => worker.listen('readyok'));
          }
        },
      };
      return { worker, commands, waiting: waiting.promise };
    };

    await t.test(
      'destroy settles readiness before an in-flight module factory resolves',
      { timeout: 1000 },
      async () => {
        const entered = Promise.withResolvers<void>();
        const loading = Promise.withResolvers<unknown>();
        const fake = module(false);
        factory(() => {
          entered.resolve();
          return loading.promise;
        });
        const engine = new StockfishWebEngine(info);
        await entered.promise;
        engine.destroy();
        assert.equal(await engine.ready, false);
        assert.equal(engine.module, undefined);
        loading.resolve(fake.worker);
        await new Promise(resolve => setImmediate(resolve));
        assert.deepEqual(fake.commands, ['quit'], 'a late module is disposed without starting UCI');
        assert.equal(engine.module, undefined);
      },
    );

    for (const cancellation of ['destroy', 'abort'] as const)
      await t.test(
        `${cancellation} disposes queued work before ready and ignores late protocol messages`,
        { timeout: 1000 },
        async () => {
          const fake = module(false);
          factory(async () => fake.worker);
          const signal = new AbortController();
          const engine = new StockfishWebEngine(info, undefined, signal.signal);
          const evaluations: Tree.LocalEval[] = [];
          const fen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
          engine.start(
            makeMoveReviewWork(moveReviewEngineProfile, {
              variant: 'standard',
              path: '',
              ply: 0,
              initialFen: fen,
              currentFen: fen,
              moves: [],
              multiPv: 1,
              rootMoves: ['e2e4'],
              searchLimits: { depth: 16, nodes: 2_000_000, movetimeMs: 2_500 },
              emit: evaluation => evaluations.push(structuredClone(evaluation)),
            }),
          );
          await fake.waiting;
          const revokedBefore = revoked.length;
          if (cancellation === 'destroy') engine.destroy();
          else signal.abort();
          assert.equal(await engine.ready, false);
          const stoppedCommands = [...fake.commands];
          fake.worker.listen('readyok');
          fake.worker.listen('readyok');
          fake.worker.listen('info depth 16 multipv 1 score cp 12 nodes 20 time 1 pv e2e4');
          fake.worker.listen('bestmove e2e4');
          await new Promise(resolve => setImmediate(resolve));
          engine.destroy();
          assert.deepEqual(fake.commands, stoppedCommands, 'late messages cannot start disposed work');
          assert.deepEqual(evaluations, [], 'late messages cannot publish evaluations');
          assert.equal(engine.module, undefined);
          assert.equal(fake.commands.filter(command => command === 'quit').length, 1);
          assert.equal(revoked.length, revokedBefore + 1);
        },
      );

    await t.test(
      'normal readiness and restricted search still use the owned module',
      { timeout: 1000 },
      async () => {
        const fake = module(true);
        factory(async () => fake.worker);
        const engine = new StockfishWebEngine(info);
        try {
          assert.equal(await engine.ready, true);
          assert.equal(engine.module, fake.worker);
          const evaluations: Tree.LocalEval[] = [];
          const fen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
          engine.start(
            makeMoveReviewWork(moveReviewEngineProfile, {
              variant: 'standard',
              path: '',
              ply: 0,
              initialFen: fen,
              currentFen: fen,
              moves: [],
              multiPv: 1,
              rootMoves: ['e2e4'],
              searchLimits: { depth: 16, nodes: 2_000_000, movetimeMs: 2_500 },
              emit: evaluation => evaluations.push(structuredClone(evaluation)),
            }),
          );
          await new Promise(resolve => setImmediate(resolve));
          assert.equal(fake.commands.at(-1), 'go depth 16 nodes 2000000 movetime 2500 searchmoves e2e4');
          fake.worker.listen('info depth 16 multipv 1 score cp 12 nodes 20 time 1 pv e2e4');
          fake.worker.listen('bestmove e2e4');
          assert.equal(evaluations.at(-1)?.bestmove, 'e2e4');
          assert.equal(evaluations.at(-1)?.cp, 12);
        } finally {
          engine.destroy();
        }
        assert.equal(engine.module, undefined);
        assert.equal(fake.commands.filter(command => command === 'quit').length, 1);
      },
    );
  } finally {
    globalThis.fetch = originalFetch;
    URL.createObjectURL = originalCreateUrl;
    URL.revokeObjectURL = originalRevokeUrl;
    for (const [key, descriptor] of descriptors) {
      if (descriptor) Object.defineProperty(globalThis, key, descriptor);
      else Reflect.deleteProperty(globalThis, key);
    }
    dom.window.close();
  }
});

class FakeXMLHttpRequest {
  static latest: FakeXMLHttpRequest | undefined;

  status = 0;
  response: ArrayBuffer = new ArrayBuffer(0);
  responseType = '';
  aborted = false;
  onprogress: ((event: ProgressEvent) => void) | null = null;
  onerror: (() => void) | null = null;
  onabort: (() => void) | null = null;
  onload: (() => void) | null = null;

  constructor() {
    FakeXMLHttpRequest.latest = this;
  }

  open(): void {}
  send(): void {}

  abort(): void {
    this.aborted = true;
    this.onabort?.();
  }

  succeed(bytes: Uint8Array<ArrayBuffer>): void {
    this.status = 200;
    this.response = bytes.buffer;
    this.onload?.();
  }
}
