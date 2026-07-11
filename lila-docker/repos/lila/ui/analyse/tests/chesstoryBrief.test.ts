import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { chesstoryBriefSections, type ChesstoryMoveMeaningPayload } from '../src/chesstoryBrief';

describe('chesstory brief scaffold', () => {
  test('keeps the panel order stable while waiting for graph-approved payload', () => {
    const sections = chesstoryBriefSections();

    assert.deepEqual(
      sections.map(section => section.key),
      ['opening-idea', 'middlegame-plan', 'current-decision', 'better-plan', 'evidence'],
    );
    assert.ok(sections.every(section => section.pending));
  });

  test('does not turn raw semantics alone into player-facing prose', () => {
    const rawSemanticPayload = {
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
    };
    const sections = chesstoryBriefSections(rawSemanticPayload as ChesstoryMoveMeaningPayload);

    assert.ok(sections.every(section => section.pending));
  });

  test('does not present a reference chain as the played move', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'bad', played_move: 'b7b5', reference_move: 'e7e6' },
      idea_chains: [
        {
          key: 'current-move-chain',
          current_move: 'e7e6',
          reference_move: 'e7e6',
          move_quality: 'not_applicable',
          subject: 'reference_move',
          proof_levels: ['owned_cause'],
          carriers: [{ role: 'actor', kind: 'Move', value: 'e7e6', from: 'e7', to: 'e6' }],
          pv: ['e7e6'],
          consequence_carriers: [],
          terminal_consequences: [],
          technique: [],
          player_facing_reason_allowed: true,
        },
      ],
    });

    assert.ok(sections.every(section => section.pending));
  });

  test('uses only graph-approved idea chains for filled cards', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'f8e7', reference_move: 'f8e7' },
      idea_chains: [
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
            { role: 'attacker', kind: 'Square', value: 'g4' },
          ],
          pv: ['c2c3', 'e8g8'],
          consequence_carriers: [
            { role: 'target', kind: 'PlanSubject', value: 'break-file:e', label: 'e-file break' },
          ],
          terminal_consequences: [],
          technique: [],
          player_facing_reason_allowed: true,
        },
      ],
    };

    const sections = chesstoryBriefSections(payload);
    const text = JSON.stringify(sections);

    assert.ok(sections.every(section => !section.pending));
    assert.equal(sections[0].title, 'Idea chain');
    assert.match(sections[0].body, /^bishop f8-e7/);
    assert.match(text, /pawn break timing/);
    assert.match(text, /bishop f8-e7/);
    assert.match(text, /e-file break/);
    assert.match(text, /g4/);
    assert.match(text, /c2-c3/);
    assert.match(text, /owned cause/);
    assert.doesNotMatch(text, /This move handles|The position is asking|The comparison turns on/);
  });

  test('renders the owned principal plan event as one causal commentary spine', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'b7b5', reference_move: 'b7b5' },
      idea_chains: [
        {
          key: 'current-move-chain',
          current_move: 'b7b5',
          reference_move: 'b7b5',
          move_quality: 'good',
          subject: 'played_move',
          move_semantics: [
            {
              subject: 'played_move',
              move_uci: 'b7b5',
              idea: { code: 'long_diagonal_pressure', label: 'opens bishop diagonal from c8' },
              priority: 'supporting',
            },
            {
              subject: 'played_move',
              move_uci: 'b7b5',
              idea: { code: 'plan_continuity', label: 'continues the queenside plan' },
              priority: 'main',
              principal_plan_event: {
                goal: { theme: 'flank_infrastructure', kind: 'hook_creation' },
                state: { move_role: 'Execution' },
                means: {
                  root_move: 'b7b5',
                  actor: { piece: 'pawn', from: 'b7', to: 'b5' },
                  development_choices: [],
                  future_move: 'b5b4',
                },
                opponent_responses: [
                  { move: 'b2b4', outcome: 'Refuted' },
                  {
                    move: 'a2a3',
                    outcome: 'Realized',
                    realization_move: 'b5b4',
                    realization_match: 'ExactMove',
                  },
                  {
                    move: 'f4e3',
                    outcome: 'Realized',
                    realization_move: 'b5b4',
                    realization_match: 'ExactMove',
                  },
                ],
                results: [
                  { stage: 'direct', kind: 'MobilityGain', subjects: [] },
                  {
                    stage: 'direct',
                    kind: 'LineUnlockGain',
                    subjects: ['bishop:c8:line-unlock:by:b7b5:mobility+2'],
                    subject_labels: ['bishop on c8'],
                  },
                  {
                    stage: 'future',
                    kind: 'TargetPressureGain',
                    subjects: ['c3'],
                    subject_labels: ['c3'],
                  },
                ],
                future_causality: {
                  future_move: 'b5b4',
                  target_square: 'b4',
                  ply_offset: 8,
                  robustness: 'Conditional',
                  realized_replies: 2,
                  exact_replies: 2,
                  tested_replies: 3,
                },
              },
            },
          ],
          proof_levels: ['surface_evidence', 'owned_function'],
          carriers: [{ role: 'actor', kind: 'Move', value: 'b7b5', from: 'b7', to: 'b5' }],
          purpose_carriers: [{ role: 'target', kind: 'File', value: 'b' }],
          pv: ['b7b5', 'b2b4', 'a5b4', 'b5b4'],
          consequence_carriers: [{ role: 'target', kind: 'Square', value: 'b4' }],
          terminal_consequences: [],
          technique: [],
          player_facing_reason_allowed: true,
        },
      ],
    };

    const sections = chesstoryBriefSections(payload);
    const text = JSON.stringify(sections);

    assert.equal(sections[0].title, 'Main plan event');
    assert.match(sections[0].body, /b7-b5 is the execution step for hook creation \(flank infrastructure\)/);
    assert.match(sections[1].body, /pawn b7-b5.*b5-b4.*b4/);
    assert.match(sections[2].body, /line unlock gain for bishop on c8/);
    assert.match(sections[2].body, /target pressure gain for c3/);
    assert.match(sections[3].body, /b2-b4: continuation refuted/);
    assert.match(sections[3].body, /a2-a3: b5-b4 realized \(exact move\)/);
    assert.match(JSON.stringify(sections[3].items), /Main line: b7-b5 b2-b4 a5-b4 b5-b4/);
    assert.match(sections[4].body, /Realized replies: 2 of 3/);
    assert.doesNotMatch(text, /bishop:c8:line-unlock|principal_plan_id|QueensideAttack/);
  });

  test('does not render future results without public future causality', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'b7b5', reference_move: 'b7b5' },
      idea_chains: [
        {
          key: 'current-move-chain',
          current_move: 'b7b5',
          reference_move: 'b7b5',
          move_quality: 'good',
          subject: 'played_move',
          move_semantics: [
            {
              move_uci: 'b7b5',
              subject: 'played_move',
              principal_plan_event: {
                goal: { kind: 'hook_creation' },
                means: { root_move: 'b7b5' },
                results: [
                  { stage: 'direct', kind: 'LineUnlockGain' },
                  { stage: 'future', kind: 'TargetPressureGain', subject_labels: ['c3'] },
                ],
              },
            },
          ],
          proof_levels: ['owned_function'],
          carriers: [],
          pv: [],
          consequence_carriers: [],
          terminal_consequences: [],
          technique: [],
          player_facing_reason_allowed: true,
        },
      ],
    };

    const text = JSON.stringify(chesstoryBriefSections(payload));
    assert.match(text, /line unlock gain/);
    assert.doesNotMatch(text, /target pressure gain|c3|Later:/);
  });
});
