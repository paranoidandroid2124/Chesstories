import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import {
  chesstoryBriefSections,
  chesstoryLlmPayload,
  type ChesstoryMoveMeaningPayload,
} from '../src/chesstoryBrief';

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

  test('turns public move semantics into player-facing cards without internal labels', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'good', played_move: 'c4c5', reference_move: 'c4c5' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea_type: 'pawn_break_timing',
          idea: { code: 'pawn_break_timing', label: 'pawn break timing' },
          evidence: { has_carrier: true, proof_level: 'owned_cause' },
          target: { squares: ['c5', 'd6'], files: ['c'], pieces: [] },
        },
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'supporting',
          idea_type: 'counterplay_race',
          idea: { code: 'counterplay_race', label: 'counterplay race' },
          evidence: { has_carrier: true, proof_level: 'surface_evidence' },
          target: { squares: ['c5'], files: ['c'], pieces: [] },
        },
      ],
    });

    const text = JSON.stringify(sections);
    assert.match(text, /pawn break timing/);
    assert.match(text, /counterplay race/);
    assert.doesNotMatch(text, /owned_cause_linked|TensionBreakPolicyRoute|idea_type|support_level/);
    assert.ok(sections.every(section => !section.pending));
  });

  test('separates a bad move local idea from the actual failure and comparison', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'bad', played_move: 'e4g3', reference_move: 'e4f6' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'bad',
          priority: 'main',
          idea: { code: 'piece_activity', label: 'piece activity' },
          assessment: {
            is_local_idea: true,
            problem: { code: 'loses_activity', label: 'loses activity' },
            failure_family: { code: 'wrong_route', label: 'wrong route' },
          },
          target: { squares: ['e4', 'g3'], pieces: ['knight'] },
          evidence: { has_carrier: true, proof_level: 'surface_evidence' },
          comparison: {
            reference_move: 'e4f6',
            candidate_move: 'e4g3',
            lost_ideas: [{ code: 'outpost_route', label: 'outpost route' }],
            moves: [
              { role: 'played_move', uci: 'e4g3' },
              { role: 'best_move', uci: 'e4f6' },
            ],
          },
        },
      ],
    });

    const current = sections.find(section => section.key === 'current-decision');
    const plan = sections.find(section => section.key === 'middlegame-plan');
    const better = sections.find(section => section.key === 'better-plan');
    assert.match(plan?.body || '', /piece activity/);
    assert.equal(current?.tone, 'bad');
    assert.match(current?.body || '', /loses activity/);
    assert.match(better?.body || '', /outpost route/);
  });

  test('does not invent a useful local idea for a bad move without local-idea evidence', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'bad', played_move: 'e4g3', reference_move: 'e4f6' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'bad',
          priority: 'main',
          idea: { code: 'piece_activity', label: 'piece activity' },
          assessment: {
            move_quality: { code: 'bad', label: 'bad' },
            idea_quality: { code: 'failed', label: 'failed' },
            priority: { code: 'main', label: 'main' },
            is_verdict_reason: true,
            is_local_idea: false,
            problem: { code: 'loses_activity', label: 'loses activity' },
          },
          target: { squares: ['e4', 'g3'], pieces: ['knight'] },
        },
      ],
    });

    const plan = sections.find(section => section.key === 'middlegame-plan');
    const current = sections.find(section => section.key === 'current-decision');
    const text = JSON.stringify(sections);
    assert.equal(plan?.tone, 'bad');
    assert.doesNotMatch(text, /Useful idea inside the mistake|There may be a local idea/);
    assert.doesNotMatch(JSON.stringify(current?.items || []), /piece activity|knight|e4|g3/);
    assert.match(plan?.body || '', /No public local idea/);
  });

  test('ignores bad move local ideas without public evidence carriers', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'bad', played_move: 'e4g3', reference_move: 'e4f6' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'bad',
          priority: 'main',
          idea: { code: 'piece_activity', label: 'piece activity' },
          assessment: {
            move_quality: { code: 'bad', label: 'bad' },
            idea_quality: { code: 'failed', label: 'failed' },
            priority: { code: 'supporting', label: 'supporting' },
            is_local_idea: true,
            problem: { code: 'loses_activity', label: 'loses activity' },
          },
          evidence: { has_carrier: false, proof_level: 'none' },
          target: { squares: ['e4', 'g3'], pieces: ['knight'] },
        },
      ],
    });

    const plan = sections.find(section => section.key === 'middlegame-plan');
    assert.match(plan?.body || '', /No public local idea/);
    assert.deepEqual(plan?.items, []);
  });

  test('does not explain bad move problems or comparison from carrierless semantics', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'bad', played_move: 'e4g3', reference_move: 'e4f6' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'bad',
          priority: 'main',
          idea: { code: 'piece_activity', label: 'piece activity' },
          assessment: {
            problem: { code: 'loses_activity', label: 'loses activity' },
          },
          evidence: { has_carrier: false, proof_level: 'none' },
          comparison: {
            reference_move: 'e4f6',
            candidate_move: 'e4g3',
            lost_ideas: [{ code: 'outpost_route', label: 'outpost route' }],
          },
        },
      ],
    });

    const current = sections.find(section => section.key === 'current-decision');
    const better = sections.find(section => section.key === 'better-plan');
    assert.match(current?.body || '', /not enough public carrier evidence/);
    assert.deepEqual(current?.items, []);
    assert.doesNotMatch(better?.body || '', /outpost route/);
    assert.deepEqual(better?.items, []);
  });

  test('requires a public evidence carrier before writing board-evidence prose', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'good', played_move: 'c4c5', reference_move: 'c4c5' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'target_pressure', label: 'target pressure' },
          target: { squares: ['d6'], files: ['d'], pieces: [] },
          evidence: { has_carrier: false, proof_level: 'none' },
        },
      ],
    });

    const evidence = sections.find(section => section.key === 'evidence');
    const plan = sections.find(section => section.key === 'middlegame-plan');
    const current = sections.find(section => section.key === 'current-decision');
    assert.match(plan?.body || '', /No public carrier/);
    assert.match(current?.body || '', /not enough public carrier evidence/);
    assert.match(evidence?.body || '', /not enough public evidence/);
    assert.deepEqual(evidence?.items, []);
  });

  test('requires explicit evidence carriers before writing terminal or technique prose', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'good', played_move: 'g6g7', reference_move: 'g6g7' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'terminal_mate', label: 'mate' },
          terminal_consequences: [{ code: 'mate', label: 'mate' }],
          endgame_technique: {
            pattern_info: { code: 'lucena', label: 'Lucena' },
            rook_geometry: { code: 'king_cut_off', label: 'king cut off' },
            status_label: 'holding',
          },
        },
      ],
    });

    const text = JSON.stringify(sections);
    const evidence = sections.find(section => section.key === 'evidence');
    assert.doesNotMatch(text, /mate|Lucena|king cut off/);
    assert.match(evidence?.body || '', /not enough public evidence/);
    assert.deepEqual(evidence?.items, []);
  });

  test('keeps terminal and rook technique evidence in the LLM payload', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'g6g7', reference_move: 'g6g7' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'terminal_mate', label: 'mate' },
          evidence: { has_carrier: true, proof_level: 'terminal_proof' },
          terminal_consequences: [{ code: 'mate', label: 'mate' }],
          endgame_technique: {
            pattern_info: { code: 'lucena', label: 'Lucena' },
            rook_geometry: { code: 'king_cut_off', label: 'king cut off' },
            status_label: 'holding',
            squares: { required: ['e4'], maintained: ['e4'], broken: [] },
          },
        },
      ],
    };

    const llmPayload = JSON.stringify(chesstoryLlmPayload(payload));
    assert.match(llmPayload, /mate/);
    assert.match(llmPayload, /Lucena/);
    assert.match(llmPayload, /king cut off/);
  });
});
