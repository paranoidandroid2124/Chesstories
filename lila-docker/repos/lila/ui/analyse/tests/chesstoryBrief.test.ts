import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import {
  chesstoryBriefSections,
  type ChesstoryMoveMeaningPayload,
} from '../src/chesstoryBrief';

describe('chesstory brief scaffold', () => {
  test('keeps the panel order stable while waiting for graph-approved payload', () => {
    const sections = chesstoryBriefSections();

    assert.deepEqual(
      sections.map(section => section.key),
      ['opening-idea', 'middlegame-plan', 'current-decision', 'better-plan', 'evidence'],
    );
    assert.ok(sections.every(section => section.pending));
  });

  test('does not turn move_semantics alone into player-facing prose', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'good', played_move: 'c4c5', reference_move: 'c4c5' },
      move_semantics: [
        {
          subject: 'played_move',
          idea: { code: 'pawn_break_timing', label: 'pawn break timing' },
          evidence: {
            has_carrier: true,
            proof_level: 'owned_cause',
            board_carriers: [{ role: 'target', kind: 'PlanSubject', value: 'break-file:c' }],
          },
        },
      ],
    });

    assert.ok(sections.every(section => section.pending));
  });

  test('uses only graph-approved llm_payload chains for filled cards', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'f8e7', reference_move: 'f8e7' },
      llm_payload: [
        {
          key: 'current-move-chain',
          current_move: 'f8e7',
          reference_move: 'f8e7',
          move_quality: 'good',
          subject: 'played_move',
          move_semantics: [
            {
              subject: 'played_move',
              move_uci: 'f8e7',
              idea_type: 'pawn_break_timing',
              idea: { code: 'pawn_break_timing', label: 'pawn break timing' },
              priority: 'main',
            },
          ],
          proof_levels: ['owned_cause'],
          carriers: [
            { role: 'actor', kind: 'Move', value: 'f8e7', from: 'f8', to: 'e7' },
            { role: 'actor', kind: 'Piece', value: 'bishop' },
            { role: 'target', kind: 'File', value: 'e' },
          ],
          pv: ['c2c3', 'e8g8'],
          consequence_carriers: [{ role: 'target', kind: 'PlanSubject', value: 'break-file:e' }],
          terminal_consequences: [],
          technique: [],
          player_facing_reason_allowed: true,
        },
      ],
    };

    const sections = chesstoryBriefSections(payload);
    const text = JSON.stringify(sections);

    assert.ok(sections.every(section => !section.pending));
    assert.match(text, /pawn break timing/);
    assert.match(text, /bishop f8-e7/);
    assert.match(text, /e-file break/);
    assert.match(text, /c2-c3/);
    assert.match(text, /owned cause/);
    assert.doesNotMatch(text, /This move handles|The position is asking|The comparison turns on/);
  });
});
