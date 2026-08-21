import assert from 'node:assert/strict';
import { test } from 'node:test';
import { JSDOM } from 'jsdom';
import { bigFileStorage } from '../src/bigFileStorage';
import type { BrowserEngineInfo } from '../src/ceval/types';

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
