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

  test('frames the empty state as position reading before coach prose', () => {
    const text = chesstoryBriefSections()
      .map(section => `${section.title} ${section.body}`)
      .join(' ');

    assert.match(text, /read/i);
    assert.match(text, /stockfish/i);
    assert.match(text, /coach explanation/i);
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
          evidence: { has_carrier: true, proof_level: 'owned_cause', source_ids: ['c4c5-break'] },
          target: { squares: ['c5', 'd6'], files: ['c'], pieces: [] },
        },
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'supporting',
          idea_type: 'counterplay_race',
          idea: { code: 'counterplay_race', label: 'counterplay race' },
          evidence: { has_carrier: true, proof_level: 'surface_evidence', source_ids: ['c4c5-race'] },
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

  test('does not invent public labels from legacy semantic strings', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'bad', played_move: 'e4g3', reference_move: 'e4f6' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'bad',
          priority: 'main',
          idea_type: 'TensionBreakPolicyRoute',
          assessment: { is_verdict_reason: true },
          problem: 'legacy problem',
          failure_family: 'legacy family',
          evidence: { has_carrier: true, proof_level: 'owned_cause', source_ids: ['legacy-carrier'] },
        },
      ],
    });

    const text = JSON.stringify(sections);
    assert.doesNotMatch(text, /TensionBreakPolicyRoute|legacy problem|legacy family/);
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
            is_verdict_reason: true,
            problem: { code: 'loses_activity', label: 'loses activity' },
            failure_family: { code: 'wrong_route', label: 'wrong route' },
          },
          target: { squares: ['e4', 'g3'], pieces: ['knight'] },
          evidence: { has_carrier: true, proof_level: 'surface_evidence', source_ids: ['e4g3-local'] },
          comparison: {
            reference_move: 'e4f6',
            candidate_move: 'e4g3',
            lost_ideas: [{ side: 'candidate', code: 'outpost_route', label: 'outpost route' }],
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

  test('keeps playable loss out of good-move framing', () => {
    const sections = chesstoryBriefSections({
      verdict: {
        verdict_code: 'playable_loss',
        move_quality: 'playable',
        played_move: 'e4g3',
        reference_move: 'e4f6',
      },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'playable',
          priority: 'main',
          idea: { code: 'piece_activity', label: 'piece activity' },
          assessment: {
            is_local_idea: true,
            is_verdict_reason: true,
            problem: { code: 'loses_activity', label: 'loses activity' },
          },
          target: { squares: ['e4', 'g3'], pieces: ['knight'] },
          evidence: { has_carrier: true, proof_level: 'owned_cause', source_ids: ['e4g3-local'] },
        },
        {
          subject: 'reference_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'outpost_route', label: 'outpost route' },
          target: { squares: ['f6'], pieces: ['knight'] },
          evidence: {
            has_carrier: true,
            proof_level: 'owned_cause',
            board_carriers: [{ role: 'target', kind: 'Square', value: 'f6' }],
          },
        },
      ],
    });

    const opening = sections.find(section => section.key === 'opening-idea');
    const plan = sections.find(section => section.key === 'middlegame-plan');
    const current = sections.find(section => section.key === 'current-decision');
    const evidence = sections.find(section => section.key === 'evidence');
    assert.match(opening?.body || '', /outpost route/);
    assert.doesNotMatch(`${opening?.body || ''} ${(opening?.items || []).join(' ')}`, /piece activity|e4|g3/);
    assert.equal(plan?.tone, 'bad');
    assert.match(plan?.body || '', /piece activity/);
    assert.equal(current?.title, 'What remains loose');
    assert.doesNotMatch(plan?.body || '', /This move handles/);
    assert.match(plan?.body || '', /does not fully meet/);
    assert.match(current?.body || '', /loses activity/);
    assert.match(JSON.stringify(evidence), /f6/);
    assert.doesNotMatch(JSON.stringify(evidence), /e4|g3/);
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
          evidence: { has_carrier: true, proof_level: 'owned_cause', source_ids: ['e4g3-failure'] },
        },
      ],
    });

    const plan = sections.find(section => section.key === 'middlegame-plan');
    const current = sections.find(section => section.key === 'current-decision');
    const text = JSON.stringify(sections);
    assert.equal(plan?.tone, 'bad');
    assert.doesNotMatch(text, /Useful idea inside the mistake|There may be a local idea/);
    assert.doesNotMatch(JSON.stringify(current?.items || []), /piece activity|knight|e4|g3/);
    assert.match(plan?.body || '', /does not reveal a clear useful idea/);
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

    assert.ok(sections.every(section => section.pending));
    assert.doesNotMatch(JSON.stringify(sections), /piece activity|loses activity|e4|g3|knight/);
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

    assert.ok(sections.every(section => section.pending));
    assert.doesNotMatch(JSON.stringify(sections), /loses activity|outpost route|e4g3|e4f6/);
  });

  test('does not invent a bad-move problem from carried semantics without a public problem', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'bad', played_move: 'e4g3', reference_move: 'e4f6' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'bad',
          priority: 'main',
          idea: { code: 'piece_activity', label: 'piece activity' },
          evidence: { has_carrier: true, proof_level: 'surface_evidence', source_ids: ['e4g3-carrier'] },
        },
      ],
    });

    const current = sections.find(section => section.key === 'current-decision');
    assert.match(current?.body || '', /lesson is not clear from the board/);
    assert.doesNotMatch(current?.body || '', /gives up more than its idea solves/);
  });

  test('uses only verdict-reason semantics for bad-move problem prose', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'bad', played_move: 'e4g3', reference_move: 'e4f6' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'bad',
          priority: 'main',
          idea: { code: 'piece_activity', label: 'piece activity' },
          assessment: {
            is_verdict_reason: false,
            problem: { code: 'loses_activity', label: 'loses activity' },
          },
          evidence: { has_carrier: true, proof_level: 'surface_evidence', source_ids: ['e4g3-problem'] },
        },
      ],
    });

    const current = sections.find(section => section.key === 'current-decision');
    assert.doesNotMatch(current?.body || '', /loses activity/);
    assert.deepEqual(current?.items, []);
  });

  test('does not let carrierless played semantics choose bad-move framing without a verdict', () => {
    const sections = chesstoryBriefSections({
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
        },
      ],
    });

    assert.ok(sections.every(section => section.pending));
    assert.doesNotMatch(JSON.stringify(sections), /loses activity|piece activity/);
  });

  test('does not let carried played semantics choose bad-move framing without a verdict', () => {
    const sections = chesstoryBriefSections({
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'bad',
          priority: 'main',
          idea: { code: 'piece_activity', label: 'piece activity' },
          assessment: {
            problem: { code: 'loses_activity', label: 'loses activity' },
          },
          evidence: { has_carrier: true, proof_level: 'owned_cause', source_ids: ['e4g3-carrier'] },
        },
      ],
    });

    const plan = sections.find(section => section.key === 'middlegame-plan');
    const current = sections.find(section => section.key === 'current-decision');
    assert.equal(plan?.tone, 'good');
    assert.equal(current?.tone, 'good');
    assert.doesNotMatch(current?.body || '', /loses activity/);
  });

  test('does not let played semantic quality override a good verdict', () => {
    const sections = chesstoryBriefSections({
      verdict: { verdict_code: 'matches_reference', move_quality: 'good', played_move: 'g1f3' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'bad',
          priority: 'main',
          idea: { code: 'piece_activity', label: 'piece activity' },
          evidence: { has_carrier: true, proof_level: 'owned_cause', source_ids: ['g1f3-carrier'] },
        },
      ],
    });

    const plan = sections.find(section => section.key === 'middlegame-plan');
    const current = sections.find(section => section.key === 'current-decision');
    assert.equal(plan?.tone, 'good');
    assert.equal(current?.title, 'Current decision');
  });

  test('does not turn reference-only comparison losses into played-move failures', () => {
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
          evidence: { has_carrier: true, proof_level: 'owned_cause', source_ids: ['e4g3-problem'] },
        },
        {
          subject: 'reference_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'outpost_route', label: 'outpost route' },
          evidence: { has_carrier: true, proof_level: 'owned_cause', source_ids: ['e4f6-route'] },
          comparison_loss: [{ side: 'reference', code: 'reference_only_tactic', label: 'reference-only tactic' }],
          comparison: {
            reference_move: 'e4f6',
            candidate_move: 'e4g3',
            lost_ideas: [{ side: 'reference', code: 'reference_only_tactic', label: 'reference-only tactic' }],
            moves: [
              { role: 'played_move', uci: 'e4g3' },
              { role: 'best_move', uci: 'e4f6' },
            ],
          },
        },
      ],
    });

    const better = sections.find(section => section.key === 'better-plan');
    assert.match(better?.body || '', /outpost route/);
    assert.doesNotMatch(better?.body || '', /reference-only tactic/);
    assert.doesNotMatch(JSON.stringify(better?.items || []), /reference-only tactic/);
  });

  test('surfaces PV continuation without burying it behind root move refs', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'd1g4', reference_move: 'd1g4' },
      move_semantics: [
        {
          move_uci: 'd1g4',
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'target_pressure', label: 'target pressure' },
          evidence: {
            has_carrier: true,
            proof_level: 'owned_cause',
            board_carriers: [
              { role: 'actor', kind: 'Move', value: 'd1g4', from: 'd1', to: 'g4' },
              { role: 'actor', kind: 'Move', value: 'd5g2', from: 'd5', to: 'g2' },
              { role: 'target', kind: 'Square', value: 'g7' },
            ],
          },
          terminal_consequences: [{ code: 'material_gain', label: 'material gain' }],
        },
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'supporting',
          idea: { code: 'piece_activity', label: 'piece activity' },
          evidence: {
            has_carrier: false,
            proof_level: 'none',
          },
          comparison: {
            moves: [
              { role: 'played_pv_1', uci: 'd5g2' },
              { role: 'played_pv_2', uci: 'g4g2' },
              { role: 'played_pv_3', uci: 'd5g2' },
              { role: 'played_pv_4', uci: 'g4g2' },
            ],
          },
        },
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'supporting',
          idea: { code: 'piece_activity', label: 'piece activity' },
          evidence: {
            has_carrier: true,
            proof_level: 'surface_evidence',
            board_carriers: [{ role: 'target', kind: 'Square', value: 'g7' }],
          },
          terminal_consequences: [{ code: 'material_loss', label: 'material loss' }],
        },
      ],
    };
    const sections = chesstoryBriefSections(payload);

    const opening = sections.find(section => section.key === 'opening-idea');
    const better = sections.find(section => section.key === 'better-plan');
    const current = sections.find(section => section.key === 'current-decision');
    const evidence = sections.find(section => section.key === 'evidence');
    assert.match(opening?.body || '', /g7/);
    assert.doesNotMatch(opening?.body || '', /target pressure around/);
    assert.doesNotMatch(opening?.body || '', /material gain around/);
    assert.match(current?.body || '', /changes d1-g4 and the g7 square; PV continues with d5-g2 and g4-g2, proving material gain/);
    assert.doesNotMatch(current?.body || '', /changes .*d5-g2.*;/);
    assert.doesNotMatch(current?.body || '', /material gain and material sacrifice/);
    assert.doesNotMatch(JSON.stringify(current?.items || []), /target pressure|piece activity/);
    assert.match(better?.body || '', /PV continues with d5-g2 and g4-g2/);
    assert.deepEqual(better?.items, ['PV continues with d5-g2 and g4-g2']);
    assert.doesNotMatch(better?.body || '', /played move d1g4|best move d1g4|d5g2|g4g2/);
    assert.equal(evidence?.body, 'The line wins material.');
    const llmPayload = JSON.stringify(chesstoryLlmPayload(payload));
    assert.equal(llmPayload.match(/PV continues/g)?.length, 1);
    assert.match(llmPayload, /The line wins material/);
    assert.doesNotMatch(llmPayload, /This move handles g7/);
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

    assert.ok(sections.every(section => section.pending));
    assert.doesNotMatch(JSON.stringify(sections), /target pressure|d6|c4c5/);
  });

  test('uses public board carriers when targets are absent', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'b8d7', reference_move: 'b8d7' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'piece_route', label: 'piece route' },
          evidence: {
            has_carrier: true,
            proof_level: 'surface_evidence',
            board_carriers: [
              { role: 'actor', kind: 'Move', value: 'b8d7', from: 'b8', to: 'd7' },
              { role: 'actor', kind: 'Piece', value: 'knight' },
              { role: 'source', kind: 'Square', value: 'b8' },
              { role: 'object', kind: 'Square', value: 'd7' },
            ],
          },
        },
      ],
    };
    const sections = chesstoryBriefSections(payload);

    const evidence = sections.find(section => section.key === 'evidence');
    const current = sections.find(section => section.key === 'current-decision');
    assert.match(current?.body || '', /changes knight b8-d7/);
    assert.doesNotMatch(current?.body || '', /not just a verdict/);
    assert.match(evidence?.body || '', /b8-d7/);
    assert.doesNotMatch(evidence?.body || '', /b8d7 b8-d7|, knight|and knight|, b8\b|and b8\b|, d7\b|and d7\b/);
    assert.deepEqual(chesstoryLlmPayload(payload), []);
  });

  test('does not let unrelated PV proof unlock carrier-only LLM prose', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'd1g4', reference_move: 'd1g4' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'target_pressure', label: 'target pressure' },
          evidence: {
            has_carrier: true,
            proof_level: 'surface_evidence',
            board_carriers: [{ role: 'target', kind: 'Square', value: 'g7' }],
          },
        },
        {
          subject: 'reference_move',
          move_quality: 'good',
          priority: 'supporting',
          idea: { code: 'piece_activity', label: 'piece activity' },
          evidence: { has_carrier: false, proof_level: 'none' },
          comparison: {
            moves: [
              { role: 'reference_pv_1', uci: 'd5g2' },
              { role: 'reference_pv_2', uci: 'g4g2' },
            ],
          },
        },
      ],
    };

    assert.deepEqual(chesstoryLlmPayload(payload), []);
  });

  test('keeps generic piece targets out of the position thread when a specific target exists', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'good', played_move: 'b7b5', reference_move: 'b7b5' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'pawn_break_timing', label: 'pawn break timing' },
          evidence: {
            has_carrier: true,
            proof_level: 'owned_cause',
            board_carriers: [
              { role: 'actor', kind: 'Move', value: 'b7b5', from: 'b7', to: 'b5' },
              { role: 'target', kind: 'Piece', value: 'pawn' },
              { role: 'target', kind: 'PlanSubject', value: 'break-file:b' },
              { role: 'target', kind: 'File', value: 'b' },
            ],
          },
        },
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'supporting',
          idea: { code: 'structure_shift', label: 'break file b created tension b5 c6' },
          evidence: {
            has_carrier: true,
            proof_level: 'surface_evidence',
            board_carriers: [
              { role: 'target', kind: 'PlanSubject', value: 'break-file:b' },
              { role: 'target', kind: 'PlanSubject', value: 'created-tension:b5-c6' },
            ],
          },
        },
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'supporting',
          idea: { code: 'line_unlock', label: 'line unlock' },
          evidence: {
            has_carrier: true,
            proof_level: 'surface_evidence',
            board_carriers: [
              { role: 'target', kind: 'PlanSubject', value: 'line-unlock:d8' },
              { role: 'target', kind: 'Square', value: 'd8' },
            ],
          },
        },
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'supporting',
          idea: { code: 'weak_squares', label: 'weak squares' },
          evidence: {
            has_carrier: true,
            proof_level: 'surface_evidence',
            board_carriers: [
              { role: 'target', kind: 'PlanSubject', value: 'weak-square:a3' },
              { role: 'target', kind: 'PlanSubject', value: 'weak-square:a4' },
              { role: 'target', kind: 'PlanSubject', value: 'weak-square:a5' },
            ],
          },
        },
      ],
    });

    const evidence = sections.find(section => section.key === 'evidence');
    const opening = sections.find(section => section.key === 'opening-idea');
    assert.match(opening?.body || '', /b-file break/);
    assert.match(opening?.body || '', /tension between b5 and c6/);
    assert.match(opening?.body || '', /line opening from d8/);
    assert.match(opening?.body || '', /weak squares a3, a4, and a5/);
    assert.doesNotMatch(opening?.body || '', /b-file break.*b-file/);
    assert.doesNotMatch(opening?.body || '', /line opening from d8.*d8/);
    assert.doesNotMatch(opening?.body || '', /weak square on a3.*weak square on a4/);
    assert.doesNotMatch(opening?.body || '', /b-file break.*pawn/);
    assert.match(evidence?.body || '', /b-file break.*b7-b5/);
    assert.doesNotMatch(evidence?.body || '', /, pawn|and pawn/);
    assert.doesNotMatch(evidence?.body || '', /b-file break.*b-file/);
    assert.doesNotMatch(evidence?.body || '', /line opening from d8.*d8/);
    assert.doesNotMatch(opening?.body || '', /break file b created tension/);
  });
  test('does not repeat a generic idea when a carrier already names it concretely', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'good', played_move: 'd5d6', reference_move: 'd5d6' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'supporting',
          idea: { code: 'passed_pawn_advance', label: 'passed pawn advance' },
          evidence: {
            has_carrier: true,
            proof_level: 'surface_evidence',
            board_carriers: [
              { role: 'actor', kind: 'Move', value: 'd5d6', from: 'd5', to: 'd6' },
              { role: 'target', kind: 'PlanSubject', value: 'passed-pawn-advanced:d5-d6:rank-6' },
              { role: 'target', kind: 'Square', value: 'd6' },
            ],
          },
        },
      ],
    });

    const opening = sections.find(section => section.key === 'opening-idea');
    assert.equal(opening?.body, 'The position is asking about passed pawn advance d5-d6.');
    assert.doesNotMatch(JSON.stringify(sections), /changes d5-d6, passed pawn advance|passed pawn advance d5-d6, d5-d6/);

    const llmPayload = chesstoryLlmPayload({
      verdict: { move_quality: 'good', played_move: 'd5d6', reference_move: 'd5d6' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'supporting',
          idea: { code: 'passed_pawn_advance', label: 'passed pawn advance' },
          evidence: {
            has_carrier: true,
            proof_level: 'surface_evidence',
            board_carriers: [
              { role: 'actor', kind: 'Move', value: 'd5d6', from: 'd5', to: 'd6' },
              { role: 'target', kind: 'PlanSubject', value: 'passed-pawn-advanced:d5-d6:rank-6' },
              { role: 'target', kind: 'PlanSubject', value: 'simplification,pawnbreakpreparation' },
              { role: 'target', kind: 'PlanSubject', value: 'line-unlock:e8' },
              { role: 'target', kind: 'PlanSubject', value: 'break-file:d' },
              { role: 'target', kind: 'PlanSubject', value: 'weak-square:e8' },
            ],
          },
        },
      ],
    });
    assert.deepEqual(llmPayload, []);
  });

  test('reads compound plan-subject carriers as chess words', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'good', played_move: 'd1g4', reference_move: 'd1g4' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'target_pressure', label: 'target pressure' },
          evidence: {
            has_carrier: true,
            proof_level: 'owned_cause',
            board_carriers: [
              { role: 'target', kind: 'PlanSubject', value: 'pin-pressure:g2-h1,weakpawnattack,openingdevelopment' },
              { role: 'target', kind: 'PlanSubject', value: 'pieceactivation,pawnbreakpreparation' },
              { role: 'target', kind: 'PlanSubject', value: 'simplification,pawnbreakpreparation' },
              { role: 'target', kind: 'PlanSubject', value: 'defender-move:g4' },
              { role: 'target', kind: 'Pawn', value: 'weak-pawn:b4' },
            ],
          },
        },
      ],
    });

    const text = JSON.stringify(sections);
    assert.match(text, /pin pressure along g2-h1/);
    assert.match(text, /weak pawn attack/);
    assert.match(text, /opening development/);
    assert.match(text, /piece development for the pawn break/);
    assert.match(text, /simplification for the pawn break/);
    assert.match(text, /defensive resource on g4/);
    assert.match(text, /weak pawn on b4/);
    assert.doesNotMatch(text, /pin-pressure|weakpawnattack|openingdevelopment|weak pawnattack|piece activation with pawn break preparation|simplification with pawn break preparation|defender move on|weak pawn b4/);
  });

  test('does not turn semantic targets into public board evidence without board carriers', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'd4d5', reference_move: 'd4d5' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'pawn_break_timing', label: 'pawn break timing' },
          target: { squares: ['d6'], files: ['d'], pieces: [] },
          evidence: { has_carrier: true, proof_level: 'surface_evidence', board_carriers: [] },
        },
      ],
    };
    const sections = chesstoryBriefSections(payload);

    const text = JSON.stringify(sections);
    assert.deepEqual(chesstoryLlmPayload(payload), []);
    assert.doesNotMatch(text, /d-file|d6/);
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
    assert.ok(sections.every(section => section.pending));
    assert.doesNotMatch(text, /mate|Lucena|king cut off/);
  });

  test('keeps terminal and rook technique evidence in the LLM payload', () => {
    const payload: ChesstoryMoveMeaningPayload = {
      verdict: { move_quality: 'good', played_move: 'g6g7', reference_move: 'g6g7' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'terminal_proof', label: 'terminal proof' },
          evidence: { has_carrier: true, proof_level: 'terminal_proof', source_ids: ['g6g7-terminal'] },
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
    assert.doesNotMatch(llmPayload, /terminal proof/);
    assert.match(llmPayload, /Lucena/);
    assert.match(llmPayload, /king cut off/);
  });

  test('reads good material loss as sacrifice instead of handled loss', () => {
    const sections = chesstoryBriefSections({
      verdict: { move_quality: 'good', played_move: 'd3c5', reference_move: 'd3c5' },
      move_semantics: [
        {
          subject: 'played_move',
          move_quality: 'good',
          priority: 'main',
          idea: { code: 'piece_route', label: 'piece route' },
          evidence: {
            has_carrier: true,
            proof_level: 'surface_evidence',
            board_carriers: [{ role: 'target', kind: 'PlanSubject', value: 'material-sacrifice:c5' }],
          },
          terminal_consequences: [{ code: 'material_loss', label: 'material loss' }],
        },
      ],
    });

    const text = JSON.stringify(sections);
    assert.match(text, /material sacrifice/);
    assert.match(text, /line confirms material sacrifice/);
    assert.doesNotMatch(text, /handles .*material loss|terminal result is material (?:loss|sacrifice)/);
  });
});
