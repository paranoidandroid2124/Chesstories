import assert from 'node:assert/strict';
import { test } from 'node:test';
import { JSDOM } from 'jsdom';
import { clearLegacyClientStorage } from '../src/legacyStorageCleanup';

test('legacy cleanup preserves sid and removes browser storage', async () => {
  const dom = new JSDOM('<!doctype html>', { url: 'https://chesstory.test/' });
  dom.window.document.cookie = 'sid=session; Path=/';
  dom.window.document.cookie = 'bg=dark; Path=/';
  dom.window.document.cookie = 'chesstory_cookie_consent=v1:p; Path=/';
  dom.window.localStorage.setItem('ceval.fen', 'position');
  dom.window.sessionStorage.setItem('analyse.import-recents.v1', 'pgn');
  const deletedDatabases: string[] = [];
  const removedFiles: Array<[string, boolean]> = [];
  Object.defineProperty(dom.window, 'indexedDB', {
    configurable: true,
    value: {
      deleteDatabase: (name: string) => {
        deletedDatabases.push(name);
        return {};
      },
    },
  });
  Object.defineProperty(dom.window.navigator, 'storage', {
    configurable: true,
    value: {
      getDirectory: async () => ({
        async *entries() {
          yield ['engine.nnue', { kind: 'file' }];
          yield ['old-cache', { kind: 'directory' }];
        },
        removeEntry: async (name: string, options: { recursive: boolean }) => {
          removedFiles.push([name, options.recursive]);
        },
      }),
    },
  });

  const originalWindow = Object.getOwnPropertyDescriptor(globalThis, 'window');
  Object.defineProperty(globalThis, 'window', { configurable: true, value: dom.window });

  try {
    await clearLegacyClientStorage();
    assert.equal(dom.window.document.cookie, 'sid=session');
    assert.equal(dom.window.localStorage.length, 0);
    assert.equal(dom.window.sessionStorage.length, 0);
    assert.deepEqual(deletedDatabases, [
      'analyse-collapse',
      'big-file',
      'ceval-wasm-cache--db',
      'lichess',
      'log--db',
    ]);
    assert.deepEqual(removedFiles, [
      ['engine.nnue', false],
      ['old-cache', true],
    ]);
  } finally {
    if (originalWindow) Object.defineProperty(globalThis, 'window', originalWindow);
    else delete (globalThis as any).window;
    dom.window.close();
  }
});
