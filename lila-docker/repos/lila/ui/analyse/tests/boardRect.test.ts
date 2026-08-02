import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { boardRectSig } from '../src/view/boardRect';

describe('board rect signature', () => {
  test('preserves sub-pixel changes that would be lost by integer rounding', () => {
    const base = boardRectSig({ left: 100.24, top: 50.12, width: 507.24, height: 507.24 });
    const shifted = boardRectSig({ left: 100.24, top: 50.12, width: 507.49, height: 507.24 });

    assert.notEqual(base, shifted);
  });
});
