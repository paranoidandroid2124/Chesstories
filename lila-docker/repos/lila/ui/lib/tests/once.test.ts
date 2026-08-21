import { describe, test, mock } from 'node:test';
import assert from 'node:assert/strict';
import { once, storage } from '../src/storage';

test('storage stays in memory and never calls browser storage', () => {
  const originalLocalStorage = Object.getOwnPropertyDescriptor(globalThis, 'localStorage');
  Object.defineProperty(globalThis, 'localStorage', {
    configurable: true,
    get: () => {
      throw new Error('browser storage must not be read');
    },
  });

  try {
    storage.set('test.memory-only', 'value');
    assert.equal(storage.get('test.memory-only'), 'value');
    storage.remove('test.memory-only');
    assert.equal(storage.get('test.memory-only'), null);
  } finally {
    if (originalLocalStorage) Object.defineProperty(globalThis, 'localStorage', originalLocalStorage);
    else delete (globalThis as any).localStorage;
  }
});

describe('test once', () => {
  test('once', async () => {
    assert.equal(once('foo'), true);

    assert.equal(once('foo'), false);
    assert.equal(once('foo'), false);

    mock.timers.enable({ apis: ['Date'], now: 1 });

    assert.equal(once('secs', { seconds: 1 }), true);
    assert.equal(once('secs', { seconds: 1 }), false, 'ohnoes');

    mock.timers.tick(1050);
    assert.equal(once('secs', { seconds: 1 }), true);

    mock.timers.reset();
  });
});
