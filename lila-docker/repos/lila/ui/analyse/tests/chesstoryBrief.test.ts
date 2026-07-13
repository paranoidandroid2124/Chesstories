import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { chesstoryBriefSections, type ChesstoryMoveMeaningPayload } from '../src/chesstoryBrief';

const engineeringTerms =
  /ownership|membership|carrier|principal|causal proof|robustness|ply offset|owned cause|proof:/i;

describe('chesstory brief scaffold', () => {
  test('keeps the panel order stable while waiting for a core explanation', () => {
    const sections = chesstoryBriefSections();

    assert.deepEqual(
      sections.map(section => section.key),
      ['opening-idea', 'middlegame-plan', 'current-decision', 'better-plan', 'evidence'],
    );
    assert.ok(sections.every(section => section.pending));
  });

  test('does not turn the retired evidence-shaped payload into player-facing prose', () => {
    const oldPayload = {
      verdict: { move_quality: 'good', played_move: 'c4c5', reference_move: 'c4c5' },
      idea_chains: [
        {
          subject: 'played_move',
          move_semantics: [{ idea: { code: 'pawn_break_timing' } }],
          proof_levels: ['owned_cause'],
        },
      ],
    };

    const sections = chesstoryBriefSections(oldPayload as ChesstoryMoveMeaningPayload);
    assert.ok(sections.every(section => section.pending));
  });

  test('does not present the better choice as the played move', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'bad', played_move: 'b7b5', reference_move: 'e7e6' },
      explanations: [
        {
          move: 'e7e6',
          reference_move: 'e7e6',
          move_quality: 'good',
          role: 'better choice',
          ideas: [{ kind: 'piece_activity', name: 'piece activity' }],
          verification: ['verified cause'],
        },
      ],
    });

    assert.ok(sections.every(section => section.pending));
  });

  test('renders a core-selected chess explanation without reconstructing evidence', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'f8e7', reference_move: 'f8e7' },
      explanations: [
        {
          move: 'f8e7',
          reference_move: 'f8e7',
          move_quality: 'good',
          role: 'played move',
          ideas: [{ kind: 'pawn_break_timing', name: 'pawn break timing' }],
          verification: ['verified cause'],
          chess_relations: ['line access'],
          move_details: ['bishop f8-e7'],
          purposes: ['e-file break'],
          line: ['c2c3', 'e8g8'],
          results: ['pressure on the g4 square'],
          forced_results: [],
          techniques: [],
        },
      ],
    };

    const sections = chesstoryBriefSections(payload);
    const text = JSON.stringify(sections);

    assert.ok(sections.every(section => !section.pending));
    assert.equal(sections[0].title, 'Main idea');
    assert.match(text, /pawn break timing/);
    assert.match(text, /bishop f8-e7/);
    assert.match(text, /e-file break/);
    assert.match(text, /g4/);
    assert.match(text, /c2-c3 e8-g8/);
    assert.match(text, /verified cause/);
    assert.doesNotMatch(text, engineeringTerms);
  });

  test('renders the one core-selected multi-move plan in human move notation', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'b7b5', reference_move: 'b7b5' },
      explanations: [
        {
          move: 'b7b5',
          reference_move: 'b7b5',
          move_quality: 'good',
          role: 'played move',
          ideas: [
            { kind: 'long_diagonal_pressure', name: 'opens bishop diagonal from c8' },
            { kind: 'plan_continuity', name: 'continues the queenside plan' },
          ],
          verification: ['observed board effect', 'verified move function'],
          purposes: ['b-file advance', 'the b4 square'],
          line: ['b7b5', 'b2b4', 'a5b4', 'b5b4'],
          results: ['the b4 square'],
          plan: {
            goal: { theme: 'flank infrastructure', kind: 'hook creation' },
            move_role: 'execution',
            plan_change: 'starts the plan',
            targets: ['b4', 'c3'],
            method: {
              starting_move: {
                uci: 'b7b5',
                san: 'b5',
                notation: '10...b5',
                turn: { move_number: 10, side: 'black', notation: '10...' },
              },
              actor: { piece: 'pawn', from: 'b7', to: 'b5' },
              development: [],
              sequence: [
                { move: { uci: 'b7b5', san: 'b5', notation: '10...b5' }, connections: [] },
                {
                  move: { uci: 'b5b4', san: 'b4', notation: '14...b4' },
                  connections: ['coordinated flank advance'],
                },
              ],
            },
            opponent_replies: [
              {
                reply: { uci: 'b2b4', san: 'b4', notation: '11.b4' },
                effect_on_plan: 'the reply stops the plan',
              },
              {
                reply: { uci: 'a2a3', san: 'a3', notation: '11.a3' },
                effect_on_plan: 'the plan works',
                continuation: { uci: 'b5b4', san: 'b4', notation: '11...b4' },
                continuation_match: 'same move',
              },
            ],
            results: [
              { when: 'after the move', change: 'line unlock gain', subjects: ['bishop on c8'] },
              {
                when: 'later in the plan',
                change: 'target pressure gain',
                subjects: ['c3'],
                tested_reply_status: 'works against some tested replies',
              },
            ],
            continuation: {
              next_move: { uci: 'b5b4', san: 'b4', notation: '14...b4' },
              target: 'b4',
              connection: 'coordinated flank advance',
              tested_reply_status: 'works against some tested replies',
              successful_replies: 2,
              same_move_replies: 2,
              same_function_replies: 0,
              tested_replies: 3,
              expected_replies: 3,
            },
          },
        },
      ],
    };

    const sections = chesstoryBriefSections(payload);
    const text = JSON.stringify(sections);

    assert.equal(sections[0].title, 'Main plan');
    assert.match(
      sections[0].body,
      /10\.\.\.b5 serves as execution for hook creation \(flank infrastructure\)/,
    );
    assert.match(sections[1].body, /pawn b7-b5.*10\.\.\.b5 14\.\.\.b4/);
    assert.match(sections[2].body, /line unlock gain for bishop on c8/);
    assert.match(sections[2].body, /target pressure gain for c3/);
    assert.match(sections[3].body, /11\.b4: the reply stops the plan/);
    assert.match(sections[3].body, /11\.a3: the plan works; 11\.\.\.b4 follows with the same move/);
    assert.match(sections[4].body, /2 of 3 tested replies keep the plan working/);
    assert.match(sections[4].body, /next move 14\.\.\.b4 toward b4/);
    assert.doesNotMatch(text, engineeringTerms);
  });

  test('does not promote a separate tested-plan limit into the main explanation', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'b7b5', reference_move: 'b7b5' },
      explanations: [
        {
          move: 'b7b5',
          role: 'played move',
          ideas: [{ kind: 'flank_pawn_pressure', name: 'flank pawn pressure' }],
          verification: ['observed board effect'],
          results: ['space on the queenside'],
        },
      ],
      tested_plan_limits: [{ plan: { results: [{ change: 'target pressure gain on c3' }] } }],
    };

    const text = JSON.stringify(chesstoryBriefSections(payload));
    assert.match(text, /flank pawn pressure/);
    assert.doesNotMatch(text, /target pressure gain|c3/);
  });
});
