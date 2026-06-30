import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { reviewStudyCreateGate } from '../src/pgnPipeline';

describe('PGN review-study creation gate', () => {
  test('blocks review creation while a different PGN draft is ready to load', () => {
    const gate = reviewStudyCreateGate('ready');

    assert.equal(gate.disabled, true);
    assert.equal(gate.buttonLabel, 'Load game first');
    assert.match(gate.message, /load/i);
  });

  test('allows review creation only when the PGN draft matches the loaded board', () => {
    const gate = reviewStudyCreateGate('current');

    assert.equal(gate.disabled, false);
    assert.equal(gate.buttonLabel, 'Save as review study');
  });
});
