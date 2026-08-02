import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { reviewStudyCreateGate } from '../src/pgnPipeline';

describe('PGN review-study creation gate', () => {
  test('gates review creation by PGN draft readiness', () => {
    for (const [draftStatus, disabled, buttonLabel, message] of [
      ['ready', true, 'Load game first', /load/i],
      ['current', false, 'Save as review study', undefined],
    ] as const) {
      const gate = reviewStudyCreateGate(draftStatus);
      assert.equal(gate.disabled, disabled);
      assert.equal(gate.buttonLabel, buttonLabel);
      if (message) assert.match(gate.message, message);
    }
  });
});
