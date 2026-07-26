import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { chesstoryBriefSections, type ChesstoryMoveMeaningPayload } from '../src/chesstoryBrief';

const engineeringTerms =
  /ownership|membership|carrier|principal|causal proof|robustness|ply offset|owned cause|proof:/i;

describe('chesstory brief scaffold', () => {
  test('keeps an empty response pending', () => {
    const sections = chesstoryBriefSections();

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
          ideas: [
            {
              kind: 'pawn_break_timing',
              name: 'pawn break timing',
              subject: 'played_move',
              scope: 'played_transition',
              confidence: 'engine_backed',
              target: { squares: ['g4'] },
              evidence: {
                layers: ['line'],
                scopes: ['played_transition'],
                causes: ['line_access'],
              },
            },
          ],
          chess_relations: ['line access'],
          line: ['c2c3', 'e8g8'],
        },
      ],
    };

    const sections = chesstoryBriefSections(payload);
    const text = JSON.stringify(sections);

    assert.ok(sections.every(section => !section.pending));
    assert.match(text, /pawn break timing/);
    assert.match(text, /line access/);
    assert.match(text, /engine backed/);
    assert.match(text, /Evidence layer: line/);
    assert.match(text, /Cause: line access/);
    assert.match(text, /g4/);
    assert.match(text, /c2-c3 e8-g8/);
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
            {
              kind: 'long_diagonal_pressure',
              name: 'opens bishop diagonal from c8',
              plans: [
                {
                  goal: { theme: 'flank infrastructure', kind: 'hook creation' },
                  move: {
                    uci: 'b7b5',
                    san: 'b5',
                    notation: '10...b5',
                    turn: { move_number: 10, side: 'black', notation: '10...' },
                  },
                  move_role: 'execution',
                  transition: 'initiation',
                  actor: { piece: 'pawn', from: 'b7', to: 'b5' },
                  targets: ['b4', 'c3'],
                  results: [
                    { stage: 'immediate', kind: 'line_unlock', direction: 'gain', subjects: ['bishop on c8'] },
                    {
                      stage: 'future',
                      kind: 'target_pressure',
                      direction: 'gain',
                      subjects: ['c3'],
                      tested_reply_status: 'the continuation occurs against some tested replies',
                    },
                  ],
                  tested_continuation: {
                    next_move: { uci: 'b5b4', san: 'b4', notation: '14...b4' },
                    target: 'b4',
                    connection: 'coordinated flank advance',
                    status: 'the continuation occurs against some tested replies',
                    successful_replies: 2,
                    tested_replies: 3,
                    expected_replies: 3,
                  },
                },
              ],
              subject: 'played_move',
              scope: 'played_transition',
              confidence: 'engine_backed',
              evidence: { layers: ['plan'], scopes: ['played_transition'], causes: ['plan_execution'] },
            },
            { kind: 'plan_continuity', name: 'continues the queenside plan' },
          ],
          line: ['b7b5', 'b2b4', 'a5b4', 'b5b4'],
        },
      ],
    };

    const sections = chesstoryBriefSections(payload);
    const text = JSON.stringify(sections);

    assert.match(text, /hook creation/);
    assert.match(text, /10\.\.\.b5/);
    assert.match(text, /14\.\.\.b4/);
    assert.match(text, /bishop on c8/);
    assert.match(text, /gain target pressure for c3/);
    assert.match(text, /pawn b7-b5/);
    assert.match(text, /When the continuation occurs/);
    assert.match(text, /2 of 3 tested replies reach the continuation/);
    assert.doesNotMatch(text, /When the plan works|keep the plan working|: the plan works/);
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
        },
      ],
      tested_plan_limits: [{ plan: { results: [{ kind: 'target_pressure', subjects: ['c3'] }] } }],
    };

    const text = JSON.stringify(chesstoryBriefSections(payload));
    assert.match(text, /flank pawn pressure/);
    assert.doesNotMatch(text, /target pressure gain|c3/);
  });
});
