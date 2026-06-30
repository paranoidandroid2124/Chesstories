import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { chesstoryBriefSections } from '../src/chesstoryBrief';

describe('chesstory brief scaffold', () => {
  test('keeps the explanation panel ordered around chess meaning instead of engine prose', () => {
    assert.deepEqual(
      chesstoryBriefSections().map(section => section.key),
      ['opening-idea', 'middlegame-plan', 'current-decision', 'better-plan', 'evidence'],
    );
  });

  test('marks every placeholder section as not filled until backend meaning data exists', () => {
    assert.ok(chesstoryBriefSections().every(section => section.pending));
  });

  test('anchors the scaffold in the opening-to-middlegame review thread', () => {
    const text = chesstoryBriefSections()
      .map(section => `${section.title} ${section.body}`)
      .join(' ');

    assert.match(text, /opening/i);
    assert.match(text, /middlegame/i);
    assert.match(text, /plan/i);
  });
});
