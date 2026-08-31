import assert from 'node:assert/strict';
import { test } from 'node:test';
import { moveReviewEngineProfile } from 'lib/ceval/types';
import {
  buildMoveReviewEngineWorkReport,
  buildMoveReviewJobRequest,
  decodeMoveReviewSnapshot,
  formatMoveReviewPercent,
  formatMoveReviewPercentagePointChange,
  moveReviewEngineOutcomeAtRequiredDepth,
  moveReviewReasonText,
  moveReviewReasonRole,
  moveReviewSubjectFromNodeList,
  normalizeMoveReviewLocale,
  selectedMoveReviewCandidate,
  type IssuedMoveReviewEngineWork,
  type MoveReviewSubject,
} from '../src/moveReview';
import {
  afterFen,
  beforeFen,
  decodeContext,
  initialFen,
  rawCommentary,
  rawResponse,
  rawSnapshot,
  requestId,
  subject,
} from './moveReviewTestSupport';

type JsonObject = Record<string, unknown>;
const object = (value: unknown): JsonObject => value as JsonObject;
const hash = (digit: string): string => digit.repeat(64);

function typedSubject(before: FEN, played: Uci, after: FEN): MoveReviewSubject {
  return {
    variant: 'standard',
    initialFen: before,
    movePrefixUci: [],
    before: { path: '' as Tree.Path, fen: before },
    played: { uci: played, san: played as San },
    after: { path: 'aa' as Tree.Path, fen: after },
  };
}

function rawTypedResponse(
  focus: MoveReviewSubject,
  referenceMoves: Uci[],
  playedMoves: Uci[],
  facetKind: string,
  channel: JsonObject,
): JsonObject {
  const raw = rawResponse();
  raw.current_fen = focus.before.fen;
  raw.focus = { kind: 'played_move', played_move_uci: focus.played.uci, resulting_fen: focus.after.fen };
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  const endpoint = (moves: Uci[], winPercent: number) => ({
    kind: 'engine_search',
    moves,
    win_percent_for_mover: winPercent,
    depth: 16,
  });
  selected[0] = {
    legal_move_index: 0,
    move_uci: referenceMoves[0],
    selection: { roles: ['best'], root_rank: 1 },
    line_insight: { endpoint: endpoint(referenceMoves, 54) },
  };
  selected[1] = {
    legal_move_index: 1,
    move_uci: playedMoves[0],
    selection: { roles: ['played'] },
    commentary: rawCommentary({
      primary: {
        kind: 'move_verdict',
        comparison_evidence_id: 'comparison.played-vs-best',
        verdict_code: 'inaccuracy',
        verdict_confidence: 'engine_backed',
        mover: 'white',
        delta: { kind: 'engine_evaluation', candidate_win_percent_delta_for_mover: -8 },
        reference_endpoint: endpoint(referenceMoves, 54),
        played_endpoint: endpoint(playedMoves, 46),
      },
      causal_explanations: [
        {
          kind: 'single_cause',
          facets: [
            {
              facet_role: 'lead',
              cause_evidence_id: `cause.${facetKind}`,
              kind: facetKind,
              proof_confidence: 'legal_replay_verified',
              effect_mode: facetKind === 'wrong_move_order' ? 'alternative_resource' : 'played_value',
              exposure: 'primary',
              source_side: facetKind === 'wrong_move_order' ? 'reference' : 'candidate',
              comparison_kind: 'played_vs_best',
              channels: [channel],
            },
          ],
        },
      ],
    }),
  };
  return raw;
}

test('emits the sole v6 standard played-focus request', () => {
  assert.deepEqual(buildMoveReviewJobRequest(requestId, subject, moveReviewEngineProfile), {
    schema_version: 'chesstory.position-commentary.job-request.v6',
    request_id: requestId,
    variant: 'standard',
    initial_fen: initialFen,
    move_prefix_uci: ['e2e4'],
    current_fen: beforeFen,
    focus: { kind: 'played_move', played_move_uci: 'e7e5', resulting_fen: afterFen },
    engine_profile: moveReviewEngineProfile,
  });
});

test('decodes root, focused comparison, and arbitrary later causal work', () => {
  const root = decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext());
  assert.equal(root?.kind, 'awaiting-core');
  if (root?.kind !== 'awaiting-core') return;
  assert.equal(root.issuedEngineWork.purpose, 'root_search');
  assert.equal(root.issuedEngineWork.searchLimits.multiPv, 3);
  assert.deepEqual(root.issuedEngineWork.rootRestriction, { kind: 'unrestricted' });

  const focus = decodeMoveReviewSnapshot(rawSnapshot('awaiting_evidence'), decodeContext());
  assert.equal(focus?.kind, 'awaiting-evidence');
  if (focus?.kind !== 'awaiting-evidence') return;
  assert.equal(focus.issuedEngineWork.purpose, 'focus_comparison');
  assert.deepEqual(focus.issuedEngineWork.rootRestriction, {
    kind: 'restricted',
    movesUci: ['c7c5', 'e7e5'],
  });

  const causal = decodeMoveReviewSnapshot(rawSnapshot('awaiting_causal'), decodeContext());
  assert.equal(causal?.kind, 'awaiting-evidence');
  if (causal?.kind !== 'awaiting-evidence') return;
  assert.equal(causal.issuedEngineWork.workId, 'work:2');
  assert.equal(causal.issuedEngineWork.purpose, 'causal_probe');
  assert.equal(causal.issuedEngineWork.searchFen, afterFen);
});

test('fails closed on unknown schemas, Chess960, execution drift, and illegal engine history', () => {
  const unsupported = rawSnapshot('awaiting_core');
  unsupported.schema_version = 'unsupported.position-commentary.job-status';
  assert.equal(decodeMoveReviewSnapshot(unsupported, decodeContext()), undefined);

  const chess960 = rawSnapshot('awaiting_core');
  chess960.variant = 'chess960';
  assert.equal(decodeMoveReviewSnapshot(chess960, decodeContext()), undefined);

  const drift = rawSnapshot('awaiting_core');
  object(drift.issued_engine_work).execution_key_sha256 = 'not-a-key';
  assert.equal(decodeMoveReviewSnapshot(drift, decodeContext()), undefined);

  const illegal = rawSnapshot('awaiting_causal');
  object(illegal.issued_engine_work).engine_position_moves_uci = ['e2e5'];
  assert.equal(decodeMoveReviewSnapshot(illegal, decodeContext()), undefined);
});

test('projects the real primary → causal facet → channel → proof segment structure', () => {
  const decoded = decodeMoveReviewSnapshot(rawResponse(), decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  assert.equal(decoded.evidence.candidates.length, 2);
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.uci, 'e7e5');
  assert.equal(played?.label, 'e5');
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  assert.equal(played.review.core.bestUci, 'c7c5');
  assert.equal(played.review.core.verdictSymbol, '?!');
  assert.equal(played.review.reasons.length, 2);
  const cause = played.review.reasons.find(reason => reason.message.kind === 'causal');
  assert.ok(cause);
  assert.equal(cause.message.kind, 'causal');
  if (cause.message.kind !== 'causal') return;
  assert.equal(cause.message.causeKind, 'candidate_tactical_liability');
  assert.equal(cause.message.actor.side, 'black');
  assert.deepEqual(cause.message.witnesses, ['line:played']);
  assert.deepEqual(
    cause.message.proofSegment?.steps.map(step => step.role),
    ['root_action', 'terminal_event'],
  );
  assert.deepEqual(
    cause.proof.moves.map(move => move.uci),
    ['e7e5', 'g1f3'],
  );
  assert.equal(moveReviewReasonRole(played.review.core, cause.id), 'primary');
  const pv = played.review.reasons.find(reason => reason.message.kind === 'line');
  assert.ok(pv);
  assert.equal(moveReviewReasonRole(played.review.core, pv!.id), 'support');
});

test('preserves BestChoice and its transmitted runner-up comparison without synthesizing improves', () => {
  const raw = rawResponse();
  const result = object(raw.result);
  const selected = result.selected_move_reviews as JsonObject[];
  const combined = selected[1]!;
  combined.legal_move_index = 0;
  combined.selection = { roles: ['best', 'played'], root_rank: 1 };
  combined.commentary = rawCommentary({
    primary: {
      kind: 'best_choice',
      comparison_evidence_id: 'comparison.best-vs-runner-up',
      runner_up_verdict_code: 'inaccuracy',
      verdict_confidence: 'engine_backed',
      mover: 'black',
      delta: { kind: 'engine_evaluation', candidate_win_percent_delta_for_mover: 3.25 },
      best_endpoint: {
        kind: 'engine_search',
        moves: ['e7e5', 'g1f3'],
        win_percent_for_mover: 60,
        depth: 16,
      },
      runner_up_endpoint: {
        kind: 'engine_search',
        moves: ['c7c5', 'g1f3'],
        win_percent_for_mover: 52,
        depth: 16,
      },
      candidate_set: { type: 'narrow_choice' },
    },
  });
  result.selected_move_reviews = [combined];
  const decoded = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (played?.review.kind !== 'move-verdict') return;
  const core = played.review.core;
  assert.equal(core.kind, 'best-choice');
  if (core.kind !== 'best-choice') return;
  assert.equal(core.verdictSymbol, 'none');
  assert.equal(core.verdictCode, 'inaccuracy');
  assert.deepEqual(core.bestChoice, {
    runnerUpVerdictCode: 'inaccuracy',
    runnerUpUci: 'c7c5',
    candidateSet: 'narrow_choice',
  });
  assert.equal(core.winChance?.changePercentagePoints, 3.25);

  for (const mutate of [
    (primary: JsonObject) => (primary.runner_up_verdict_code = 'improves_on_reference'),
    (primary: JsonObject) => (primary.candidate_set = { type: 'invented_choice' }),
  ]) {
    const malformed = structuredClone(raw);
    const malformedResult = object(malformed.result);
    const malformedSelected = malformedResult.selected_move_reviews as JsonObject[];
    mutate(object(object(malformedSelected[0]!.commentary).primary));
    assert.equal(decodeMoveReviewSnapshot(malformed, decodeContext()), undefined);
  }
});

test('fails the whole commentary on malformed explanation, facet, or channel wire shapes', () => {
  const mutations: Array<(explanation: JsonObject, facet: JsonObject, channel: JsonObject) => void> = [
    explanation => {
      explanation.kind = 'invented_explanation';
    },
    (_explanation, facet) => {
      facet.kind = 'invented_cause';
    },
    (_explanation, facet) => {
      facet.extra = true;
    },
    (_explanation, _facet, channel) => {
      delete channel.witnesses;
    },
    (_explanation, _facet, channel) => {
      object(channel.actor).side = 'green';
    },
    (_explanation, _facet, channel) => {
      channel.horizon = 'ply:01';
    },
    (_explanation, _facet, channel) => {
      channel.direct_change = 'prevented';
    },
    (_explanation, _facet, channel) => {
      channel.direct_change = 'refuted';
    },
    (_explanation, _facet, channel) => {
      channel.direct_change = 'missed';
    },
    (_explanation, _facet, channel) => {
      channel.played_change = 'prevented';
    },
    (_explanation, _facet, channel) => {
      channel.played_change = 'refuted';
    },
    (_explanation, _facet, channel) => {
      const segment = object(channel.proof_segment);
      object((segment.steps as JsonObject[])[0]).role = 'invented_role';
    },
    (_explanation, _facet, channel) => {
      channel.extra = true;
    },
  ];
  for (const [index, mutate] of mutations.entries()) {
    const raw = rawResponse();
    const selected = object(raw.result).selected_move_reviews as JsonObject[];
    const commentary = object(selected[1]!.commentary);
    const explanation = (commentary.causal_explanations as JsonObject[])[0]!;
    const facet = (explanation.facets as JsonObject[])[0]!;
    const channel = (facet.channels as JsonObject[])[0]!;
    mutate(explanation, facet, channel);
    assert.equal(decodeMoveReviewSnapshot(raw, decodeContext()), undefined, `wire mutation ${index}`);
  }
});

test('accepts unbounded semantic IDs and more than eight public causal explanations', () => {
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  const commentary = object(selected[1]!.commentary);
  const template = (commentary.causal_explanations as JsonObject[])[0]!;
  commentary.causal_explanations = Array.from({ length: 9 }, (_, index) => {
    const explanation = structuredClone(template);
    const facet = (explanation.facets as JsonObject[])[0]!;
    facet.cause_evidence_id = `${'cause'.repeat(70)}:${index}`;
    const channel = (facet.channels as JsonObject[])[0]!;
    channel.channel_id = `${'channel'.repeat(50)}:${index}`;
    channel.causal_signature = `${'signature'.repeat(40)}:${index}`;
    return explanation;
  });
  const decoded = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (played?.review.kind !== 'move-verdict') return;
  const causal = played.review.reasons.filter(reason => reason.message.kind === 'causal');
  assert.equal(causal.length, 9);
  assert.ok(causal.every(reason => moveReviewReasonRole(played.review.core, reason.id) === 'proof-route'));
});

test('keeps two exact channels with one semantic signature as separate legal proofs', () => {
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  const commentary = selected[1]!.commentary as JsonObject;
  const explanation = (commentary.causal_explanations as JsonObject[])[0]!;
  const facet = (explanation.facets as JsonObject[])[0]!;
  const first = (facet.channels as JsonObject[])[0]!;
  const second = structuredClone(first);
  second.channel_id = 'cause-channel:second-exact-route';
  second.proof_line_moves = ['e7e5', 'f1c4'];
  const proof = second.proof_segment as JsonObject;
  (proof.steps as JsonObject[])[1]!.move_uci = 'f1c4';
  facet.channels = [first, second];

  const decoded = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  const causal = played.review.reasons.filter(reason => reason.message.kind === 'causal');
  assert.equal(causal.length, 2);
  assert.equal(new Set(causal.map(reason => reason.id)).size, 2);
  assert.deepEqual(
    causal.map(reason => reason.proof.moves.map(move => move.uci)),
    [
      ['e7e5', 'g1f3'],
      ['e7e5', 'f1c4'],
    ],
  );
  assert.ok(causal.every(reason => moveReviewReasonRole(played.review.core, reason.id) === 'proof-route'));
});

test('projects every resource proof path across both exact branch occurrences and rejects a partial path', () => {
  const root = '3q1rkr/5ppp/8/8/2B5/1Q6/8/3R2K1 w - - 0 1' as FEN;
  const referenceMoves = ['c4f7', 'f8f7', 'd1d8'] as Uci[];
  const playedMoves = ['d1d8', 'f8d8'] as Uci[];
  const referenceFens = [
    '3q1rkr/5Bpp/8/8/8/1Q6/8/3R2K1 b - - 0 1',
    '3q2kr/5rpp/8/8/8/1Q6/8/3R2K1 w - - 0 2',
    '3R2kr/5rpp/8/8/8/1Q6/8/6K1 b - - 0 2',
  ] as FEN[];
  const playedFens = [
    '3R1rkr/5ppp/8/8/2B5/1Q6/8/6K1 b - - 0 1',
    '3r2kr/5ppp/8/8/2B5/1Q6/8/6K1 w - - 0 2',
  ] as FEN[];
  const steps = (moves: Uci[], fens: FEN[], observed: boolean) =>
    moves.map((move, index) => ({
      step_index: index,
      provenance: observed && index === 0 ? 'observed_game_move' : 'certified_analysis_move',
      ply: index + 1,
      move_uci: move,
      fen_before: index === 0 ? root : fens[index - 1],
      fen_after: fens[index],
    }));
  const referenceBranchId = hash('1');
  const playedBranchId = hash('2');
  const proofPath = (id: string, suffix: string) => ({
    path_occurrence_id: id,
    premises: [
      {
        role: 'created_check_response',
        contract: 'created_check_response_inventory',
        result_id: `created_check_response_inventory:${hash('7')}`,
        source_premise_ids: [`created:${suffix}`],
        branch_id: referenceBranchId,
        branch_role: 'counterfactual_reference',
        step_index: 0,
      },
      {
        role: 'reference_capture_recapture',
        contract: 'capture_recapture_inventory',
        result_id: `capture_recapture_inventory:${hash('8')}`,
        source_premise_ids: [`reference:${suffix}`],
        branch_id: referenceBranchId,
        branch_role: 'counterfactual_reference',
        step_index: 2,
      },
      {
        role: 'played_capture_recapture',
        contract: 'capture_recapture_inventory',
        result_id: `capture_recapture_inventory:${hash('9')}`,
        source_premise_ids: [`played:${suffix}`],
        branch_id: playedBranchId,
        branch_role: 'observed_played_root',
        step_index: 0,
      },
    ],
    closed_absence_uses: [
      {
        use_id: hash(suffix),
        role: 'reference_recapture_absent',
        semantic_proof_id: hash('6'),
        issuer: 'position_relation_extractor.closed_relation_inventory',
        issuer_evidence_id: 'reference-line-evidence',
        issuer_occurrence_id: hash('5'),
        query: 'legal-capture:black:d8',
        branch_id: referenceBranchId,
        branch_role: 'counterfactual_reference',
        after_step_index: 2,
        position: { fen: referenceFens[2], ply: 3, scope: 'best_line' },
      },
    ],
  });
  const channel = {
    channel_id: 'typed.resource',
    causal_signature: 'typed.resource.signature',
    direct_change: 'occurred',
    played_change: 'missed',
    resource_differential_proof: {
      family: 'immediate_forced_reply_resource_differential',
      trigger_mechanism: 'forced_displacement',
      source_evidence_id: 'resource.source',
      semantic_id: hash('a'),
      occurrence_id: hash('b'),
      dependency_fingerprint: hash('c'),
      counterfactual_reference_branch: {
        branch_id: referenceBranchId,
        line_id: 'line.reference',
        line_role: 'best_reference',
        branch_role: 'counterfactual_reference',
        root_provenance: 'counterfactual_analyzed_root',
        line_rank: 1,
        root_move: referenceMoves[0],
        steps: steps(referenceMoves, referenceFens, false),
      },
      played_root_branch: {
        branch_id: playedBranchId,
        line_id: 'line.played',
        line_role: 'played',
        branch_role: 'observed_played_root',
        root_provenance: 'observed_game_root',
        line_rank: 1,
        root_move: playedMoves[0],
        steps: steps(playedMoves, playedFens, true),
      },
      proof_paths: [proofPath(hash('3'), '3'), proofPath(hash('4'), '4')],
      participants: {
        trigger: { side: 'white', from: 'c4', to: 'f7', piece_before: 'bishop', piece_after: 'bishop' },
        forced_reply: {
          side: 'black',
          from: 'f8',
          to: 'f7',
          piece_before: 'rook',
          piece_after: 'rook',
          move_uci: 'f8f7',
        },
        realizer: { side: 'white', from: 'd1', to: 'd8', piece_before: 'rook', piece_after: 'rook' },
        captured_target: { side: 'black', piece: 'queen', square: 'd8' },
        played_defense: {
          side: 'black',
          from: 'f8',
          to: 'd8',
          piece_before: 'rook',
          piece_after: 'rook',
          move_uci: 'f8d8',
        },
        disabled_defender: {
          side: 'black',
          piece: 'rook',
          square: 'f8',
        },
      },
      realizing_move: 'd1d8',
      played_root_branch_legal_defense_move: 'f8d8',
    },
  };
  const focus = typedSubject(root, playedMoves[0]!, playedFens[0]!);
  const decoded = decodeMoveReviewSnapshot(
    rawTypedResponse(focus, referenceMoves, playedMoves, 'wrong_move_order', channel),
    { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
  );
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  const reasons = played.review.reasons.filter(reason => reason.message.kind === 'resource-differential');
  assert.equal(reasons.length, 4);
  assert.ok(reasons.every(reason => moveReviewReasonRole(played.review.core, reason.id) === 'proof-route'));
  assert.deepEqual(
    reasons.map(reason => reason.message.kind === 'resource-differential' && reason.message.branch.role),
    ['counterfactual_reference', 'observed_played_root', 'counterfactual_reference', 'observed_played_root'],
  );
  assert.deepEqual(
    reasons.map(reason => reason.proof.moves.map(move => move.uci)),
    [referenceMoves, playedMoves, referenceMoves, playedMoves],
  );
  assert.deepEqual(
    reasons.map(reason => reason.message.kind === 'resource-differential' && reason.message.pathOccurrenceId),
    [hash('3'), hash('3'), hash('4'), hash('4')],
  );
  assert.ok(
    reasons.every(
      reason =>
        reason.message.kind === 'resource-differential' &&
        reason.message.counterpart.id !== reason.message.branch.id &&
        reason.message.branch.steps?.[0]?.provenance !== undefined &&
        reason.message.counterpart.steps?.[0]?.provenance !== undefined &&
        reason.message.triggerMechanism === 'forced_displacement' &&
        reason.message.disabledDefender.square === 'f8' &&
        reason.message.absence.issuerEvidenceId === 'reference-line-evidence' &&
        reason.message.absence.issuerOccurrenceId === hash('5') &&
        reason.message.absence.fen === referenceFens[2] &&
        reason.message.premises.length === 3,
    ),
  );

  const removalRoot = '7k/4p3/5n2/3r2B1/8/8/2B5/K2Q2R1 w - - 0 1' as FEN;
  const removalReferenceMoves = ['g5f6', 'e7f6', 'd1d5'] as Uci[];
  const removalPlayedMoves = ['d1d5', 'f6d5'] as Uci[];
  const removalReferenceFens = [
    '7k/4p3/5B2/3r4/8/8/2B5/K2Q2R1 b - - 0 1',
    '7k/8/5p2/3r4/8/8/2B5/K2Q2R1 w - - 0 2',
    '7k/8/5p2/3Q4/8/8/2B5/K5R1 b - - 0 2',
  ] as FEN[];
  const removalPlayedFens = [
    '7k/4p3/5n2/3Q2B1/8/8/2B5/K5R1 b - - 0 1',
    '7k/4p3/8/3n2B1/8/8/2B5/K5R1 w - - 0 2',
  ] as FEN[];
  const removalSteps = (moves: Uci[], fens: FEN[], observed: boolean) =>
    moves.map((move, index) => ({
      step_index: index,
      provenance: observed && index === 0 ? 'observed_game_move' : 'certified_analysis_move',
      ply: index + 1,
      move_uci: move,
      fen_before: index === 0 ? removalRoot : fens[index - 1],
      fen_after: fens[index],
    }));
  const removalChannel = structuredClone(channel);
  const removalProof = object(removalChannel.resource_differential_proof);
  removalProof.trigger_mechanism = 'forced_recapturer_removal';
  removalProof.semantic_id = hash('d');
  removalProof.occurrence_id = hash('e');
  removalProof.dependency_fingerprint = hash('f');
  const removalReference = object(removalProof.counterfactual_reference_branch);
  removalReference.root_move = removalReferenceMoves[0];
  removalReference.steps = removalSteps(removalReferenceMoves, removalReferenceFens, false);
  const removalPlayed = object(removalProof.played_root_branch);
  removalPlayed.root_move = removalPlayedMoves[0];
  removalPlayed.steps = removalSteps(removalPlayedMoves, removalPlayedFens, true);
  const removalPath = structuredClone(proofPath(hash('d'), 'd'));
  removalPath.premises.unshift({
    role: 'reference_root_capture',
    contract: 'capture_recapture_inventory',
    result_id: `capture_recapture_inventory:${hash('0')}`,
    source_premise_ids: ['root-capture:d'],
    branch_id: referenceBranchId,
    branch_role: 'counterfactual_reference',
    step_index: 0,
  });
  removalPath.closed_absence_uses[0]!.query = 'legal-capture:black:d5';
  removalPath.closed_absence_uses[0]!.position = {
    fen: removalReferenceFens[2],
    ply: 3,
    scope: 'best_line',
  };
  removalProof.proof_paths = [removalPath];
  removalProof.participants = {
    trigger: { side: 'white', from: 'g5', to: 'f6', piece_before: 'bishop', piece_after: 'bishop' },
    forced_reply: {
      side: 'black',
      from: 'e7',
      to: 'f6',
      piece_before: 'pawn',
      piece_after: 'pawn',
      move_uci: 'e7f6',
    },
    realizer: { side: 'white', from: 'd1', to: 'd5', piece_before: 'queen', piece_after: 'queen' },
    captured_target: { side: 'black', piece: 'rook', square: 'd5' },
    played_defense: {
      side: 'black',
      from: 'f6',
      to: 'd5',
      piece_before: 'knight',
      piece_after: 'knight',
      move_uci: 'f6d5',
    },
    disabled_defender: { side: 'black', piece: 'knight', square: 'f6' },
  };
  removalProof.realizing_move = 'd1d5';
  removalProof.played_root_branch_legal_defense_move = 'f6d5';
  const removalFocus = typedSubject(removalRoot, removalPlayedMoves[0]!, removalPlayedFens[0]!);
  const removalDecoded = decodeMoveReviewSnapshot(
    rawTypedResponse(
      removalFocus,
      removalReferenceMoves,
      removalPlayedMoves,
      'wrong_move_order',
      removalChannel,
    ),
    { requestId, subject: removalFocus, engineProfile: moveReviewEngineProfile },
  );
  assert.equal(removalDecoded?.kind, 'completed');
  if (removalDecoded?.kind !== 'completed') return;
  const removalCandidate = selectedMoveReviewCandidate(removalDecoded.evidence);
  assert.equal(removalCandidate?.review.kind, 'move-verdict');
  if (!removalCandidate || removalCandidate.review.kind !== 'move-verdict') return;
  const removalReasons = removalCandidate.review.reasons.filter(
    reason => reason.message.kind === 'resource-differential',
  );
  assert.equal(removalReasons.length, 2);
  assert.ok(
    removalReasons.every(
      reason =>
        reason.message.kind === 'resource-differential' &&
        reason.message.triggerMechanism === 'forced_recapturer_removal' &&
        reason.message.premises.map(premise => premise.role).join(',') ===
          'reference_root_capture,created_check_response,reference_capture_recapture,played_capture_recapture',
    ),
  );
  assert.match(
    moveReviewReasonText(removalReasons[0]!, removalCandidate, 'en-US'),
    /captures the knight on f6, forcing e7f6/,
  );
  assert.match(
    moveReviewReasonText(removalReasons[0]!, removalCandidate, 'en-US'),
    /counterfactual reference branch:.*g5.*f6.*In the observed played branch, d1d5.*f6d5/s,
  );
  assert.match(
    moveReviewReasonText(removalReasons[1]!, removalCandidate, 'en-US'),
    /observed played branch: d1d5.*f6d5.*In the counterfactual reference branch,.*g5.*f6/s,
  );

  const mismatchedRemoval = structuredClone(removalChannel);
  object(mismatchedRemoval.resource_differential_proof).trigger_mechanism = 'forced_displacement';
  assert.equal(
    decodeMoveReviewSnapshot(
      rawTypedResponse(
        removalFocus,
        removalReferenceMoves,
        removalPlayedMoves,
        'wrong_move_order',
        mismatchedRemoval,
      ),
      { requestId, subject: removalFocus, engineProfile: moveReviewEngineProfile },
    ),
    undefined,
  );

  const detachedForcedReply = structuredClone(removalChannel);
  const detachedProof = object(detachedForcedReply.resource_differential_proof);
  const detachedParticipants = object(detachedProof.participants);
  const detachedReply = object(detachedParticipants.forced_reply);
  detachedReply.to = 'e6';
  detachedReply.move_uci = 'e7e6';
  const detachedReference = object(detachedProof.counterfactual_reference_branch);
  object((detachedReference.steps as JsonObject[])[1]).move_uci = 'e7e6';
  assert.equal(
    decodeMoveReviewSnapshot(
      rawTypedResponse(
        removalFocus,
        removalReferenceMoves,
        removalPlayedMoves,
        'wrong_move_order',
        detachedForcedReply,
      ),
      { requestId, subject: removalFocus, engineProfile: moveReviewEngineProfile },
    ),
    undefined,
  );

  const partial = structuredClone(channel);
  const partialProof = object(partial.resource_differential_proof);
  delete object((partialProof.proof_paths as JsonObject[])[0]).closed_absence_uses;
  const rejected = decodeMoveReviewSnapshot(
    rawTypedResponse(focus, referenceMoves, playedMoves, 'wrong_move_order', partial),
    { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
  );
  assert.equal(rejected, undefined);

  const assertRequiredProofField = (mutate: (proof: JsonObject) => void) => {
    const invalid = structuredClone(channel);
    mutate(object(invalid.resource_differential_proof));
    const result = decodeMoveReviewSnapshot(
      rawTypedResponse(focus, referenceMoves, playedMoves, 'wrong_move_order', invalid),
      { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
    );
    assert.equal(result, undefined);
  };
  assertRequiredProofField(proof => delete proof.trigger_mechanism);
  assertRequiredProofField(proof => delete object(proof.participants).disabled_defender);
  assertRequiredProofField(proof => {
    const path = object((proof.proof_paths as JsonObject[])[0]);
    delete object((path.closed_absence_uses as JsonObject[])[0]).issuer_occurrence_id;
  });
  assertRequiredProofField(proof => {
    object(proof.participants).trigger = {
      side: 'white',
      from: 'c4',
      to: 'e6',
      piece_before: 'bishop',
      piece_after: 'bishop',
    };
  });
  assertRequiredProofField(proof => {
    object(object(proof.participants).captured_target).side = 'white';
  });
  assertRequiredProofField(proof => {
    object(object(proof.participants).played_defense).move_uci = 'f8e8';
  });
  assertRequiredProofField(proof => {
    proof.played_root_branch_legal_defense_move = 'f8e8';
    const playedDefense = object(object(proof.participants).played_defense);
    playedDefense.to = 'e8';
    playedDefense.move_uci = 'f8e8';
    object((object(proof.played_root_branch).steps as JsonObject[])[1]).move_uci = 'f8e8';
  });
  assertRequiredProofField(proof => {
    const path = object((proof.proof_paths as JsonObject[])[0]);
    object((path.closed_absence_uses as JsonObject[])[0]).query = 'legal-capture:black:e8';
  });
  assertRequiredProofField(proof => {
    const path = object((proof.proof_paths as JsonObject[])[0]);
    object(object((path.closed_absence_uses as JsonObject[])[0]).position).ply = 1;
  });
  assertRequiredProofField(proof => {
    const paths = proof.proof_paths as JsonObject[];
    object(paths[1]).path_occurrence_id = object(paths[0]).path_occurrence_id;
  });
  assertRequiredProofField(proof => {
    const paths = proof.proof_paths as JsonObject[];
    const firstAbsence = object((object(paths[0]).closed_absence_uses as JsonObject[])[0]);
    object((object(paths[1]).closed_absence_uses as JsonObject[])[0]).use_id = firstAbsence.use_id;
  });
  assertRequiredProofField(proof => {
    const reference = object(proof.counterfactual_reference_branch);
    const playedBranch = object(proof.played_root_branch);
    playedBranch.branch_id = reference.branch_id;
    for (const path of proof.proof_paths as JsonObject[]) {
      const premises = object(path).premises as JsonObject[];
      object(premises[2]).branch_id = reference.branch_id;
    }
  });
  assertRequiredProofField(proof => {
    object(object(proof.participants).realizer).piece_before = 'dragon';
  });
});

test('keeps every independent passed-pawn-result proof path while projecting the certified reply closure', () => {
  const root = '7k/8/P7/1P6/8/8/8/4K3 w - - 0 1' as FEN;
  const referenceMoves = ['b5b6', 'h8g8', 'b6b7'] as Uci[];
  const playedMoves = ['a6a7', 'h8g8', 'a7a8q'] as Uci[];
  const fens = [
    '7k/P7/8/1P6/8/8/8/4K3 b - - 0 1',
    '6k1/P7/8/1P6/8/8/8/4K3 w - - 1 2',
    'Q5k1/8/8/1P6/8/8/8/4K3 b - - 0 2',
  ] as FEN[];
  const line = { line_id: 'line.played', line_role: 'played', line_rank: 1, root_move: 'a6a7' };
  const replyLine = { line_id: 'line.reply.h8g8', line_role: 'alternative', line_rank: 1, root_move: 'h8g8' };
  const stepKey = (ply: number, move: Uci, before: FEN, after: FEN) => `${ply}:${move}:${before}:${after}`;
  const rootStepKey = stepKey(1, 'a6a7' as Uci, root, fens[0]!);
  const replyStepKey = stepKey(2, 'h8g8' as Uci, fens[0]!, fens[1]!);
  const resultStepKey = stepKey(3, 'a7a8q' as Uci, fens[1]!, fens[2]!);
  const expectedDependencyKey = 'expected-passed-pawn-result-dependency';
  const rootStep = {
    step_index: 0,
    step_key: rootStepKey,
    ply: 1,
    move_uci: 'a6a7',
    fen_before: root,
    fen_after: fens[0],
    line,
    provenance: 'observed_game_move',
  };
  const expectedSteps = [
    rootStep,
    {
      step_index: 1,
      step_key: resultStepKey,
      ply: 3,
      move_uci: 'a7a8q',
      fen_before: fens[1],
      fen_after: fens[2],
      line,
      provenance: 'certified_analysis_move',
      incoming_link: {
        kind: 'certified_causal_dependency',
        from_step_key: rootStepKey,
        to_step_key: resultStepKey,
        occurrence_link_key: expectedDependencyKey,
      },
    },
  ];
  const replySteps = [
    rootStep,
    {
      step_index: 1,
      step_key: replyStepKey,
      ply: 2,
      move_uci: 'h8g8',
      fen_before: fens[0],
      fen_after: fens[1],
      line: replyLine,
      provenance: 'certified_analysis_move',
      incoming_link: {
        kind: 'adjacent_legal_replay',
        from_step_key: rootStepKey,
        to_step_key: replyStepKey,
        occurrence_link_key: 'adjacent-reply-step',
      },
    },
    {
      step_index: 2,
      step_key: resultStepKey,
      ply: 3,
      move_uci: 'a7a8q',
      fen_before: fens[1],
      fen_after: fens[2],
      line: replyLine,
      provenance: 'certified_analysis_move',
      incoming_link: {
        kind: 'adjacent_legal_replay',
        from_step_key: replyStepKey,
        to_step_key: resultStepKey,
        occurrence_link_key: 'adjacent-result-step',
      },
    },
  ];
  const expectedBranchId = hash('1');
  const replyBranchId = hash('2');
  const path = (id: string, suffix: string) => ({
    path_occurrence_id: id,
    reply_branch_id: replyBranchId,
    realization_actor: {
      side: 'white',
      piece_before: 'pawn',
      piece_after: 'queen',
      from: 'a7',
      to: 'a8',
      legal_move_relation: hash('e'),
    },
    realization_move: 'a7a8q',
    realization_ply: 3,
    realization_match_kind: 'exact_move',
    premises: [
      {
        role: 'comparison_demand',
        lower_kind: 'played_vs_best_demand',
        lower_semantic_key: 'comparison-key',
        source_premise_ids: ['comparison.played-vs-best'],
        branch_id: expectedBranchId,
        branch_role: 'expected_result_route',
        related_branch_ids: [],
        from_step_index: 0,
        to_step_index: 0,
      },
      {
        role: 'expected_dependency',
        lower_kind: 'passed_pawn_result_dependency',
        lower_semantic_key: expectedDependencyKey,
        source_premise_ids: ['passed-pawn-result.event'],
        branch_id: expectedBranchId,
        branch_role: 'expected_result_route',
        related_branch_ids: [],
        from_step_index: 0,
        to_step_index: 1,
      },
      {
        role: 'expected_result',
        lower_kind: 'passed_pawn_result',
        lower_semantic_key: 'expected-result-key',
        source_premise_ids: ['passed-pawn-result.event'],
        branch_id: expectedBranchId,
        branch_role: 'expected_result_route',
        related_branch_ids: [],
        from_step_index: 1,
        to_step_index: 1,
      },
      {
        role: 'observed_dependency',
        lower_kind: 'observed_passed_pawn_result_dependency',
        lower_semantic_key: `observed-dependency:${suffix}`,
        source_premise_ids: ['passed-pawn-result.event'],
        branch_id: replyBranchId,
        branch_role: 'legal_reply',
        related_branch_ids: [],
        from_step_index: 0,
        to_step_index: 2,
      },
      {
        role: 'observed_result',
        lower_kind: 'observed_passed_pawn_result',
        lower_semantic_key: `observed-result:${suffix}`,
        source_premise_ids: ['passed-pawn-result.event'],
        branch_id: replyBranchId,
        branch_role: 'legal_reply',
        related_branch_ids: [],
        from_step_index: 2,
        to_step_index: 2,
      },
      {
        role: 'functional_match',
        lower_kind: 'passed_pawn_result_functional_match',
        lower_semantic_key: `functional-match:${suffix}`,
        source_premise_ids: ['passed-pawn-result.event'],
        branch_id: replyBranchId,
        branch_role: 'legal_reply',
        related_branch_ids: [expectedBranchId],
        from_step_index: 2,
        to_step_index: 2,
      },
    ],
    closure_use_ids: [hash(suffix)],
  });
  const channel = {
    channel_id: 'typed.passed-pawn-result',
    causal_signature: 'typed.passed-pawn-result.signature',
    direct_change: 'occurred',
    passed_pawn_result_proof: {
      contract: 'passed_pawn_result_under_closed_replies',
      source_evidence_id: 'passed-pawn-result.source',
      event_evidence_id: 'passed-pawn-result.event',
      comparison_evidence_id: 'comparison.played-vs-best',
      semantic_id: hash('a'),
      occurrence_id: hash('b'),
      dependency_fingerprint: hash('c'),
      consequence_kind: 'passed_pawn_progress',
      result_target_subjects: [
        `20:passed-pawn-promoted5:white2:a72:a8:relations:[removed:pawn_passage:${hash('f')}]:derived:[]`,
      ],
      root_actor: {
        side: 'white',
        piece_before: 'pawn',
        piece_after: 'pawn',
        from: 'a6',
        to: 'a7',
        legal_move_relation: hash('d'),
      },
      realizing_actor: {
        side: 'white',
        piece_before: 'pawn',
        piece_after: 'queen',
        from: 'a7',
        to: 'a8',
        legal_move_relation: hash('e'),
      },
      root_line: line,
      root_move: 'a6a7',
      root_ply: 1,
      realizing_move: 'a7a8q',
      realizing_ply: 3,
      result_ply_offset: 2,
      closed_legal_reply_inventory: {
        issuer: 'structural_delta.canonical_legal_reply_inventory',
        issuer_evidence_id: 'structural.delta.reply.inventory',
        coverage_issuer: 'passed_pawn_result_event.branch_complete_reply_coverage',
        coverage_evidence_id: 'passed-pawn-result.event',
        root_after: { fen: fens[0], ply: 1, scope: 'played_transition' },
        legal_reply_moves: ['h8g8'],
        branch_by_reply: [{ reply_move: 'h8g8', branch_id: replyBranchId }],
        certified_horizon_ply_offset: 2,
      },
      branches: [
        {
          branch_id: expectedBranchId,
          role: 'expected_result_route',
          line,
          root_provenance: 'observed_game_root',
          steps: expectedSteps,
        },
        {
          branch_id: replyBranchId,
          role: 'legal_reply',
          reply_move: 'h8g8',
          source_probe_id: 'probe.h8g8',
          line,
          root_provenance: 'observed_game_root',
          steps: replySteps,
        },
      ],
      proof_paths: [path(hash('3'), '3'), path(hash('4'), '4')],
      lower_premise_ids: [
        'comparison.played-vs-best',
        'passed-pawn-result.event',
        'structural.delta.reply.inventory',
      ],
    },
  };
  const focus = typedSubject(root, playedMoves[0]!, fens[0]!);
  const decoded = decodeMoveReviewSnapshot(
    rawTypedResponse(focus, referenceMoves, playedMoves, 'passed_pawn_result', channel),
    { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
  );
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  const reasons = played.review.reasons.filter(reason => reason.message.kind === 'passed-pawn-result');
  assert.equal(reasons.length, 2);
  assert.ok(reasons.every(reason => moveReviewReasonRole(played.review.core, reason.id) === 'proof-route'));
  assert.deepEqual(
    reasons.map(reason => reason.message.kind === 'passed-pawn-result' && reason.message.pathOccurrenceId),
    [hash('3'), hash('4')],
  );
  assert.deepEqual(
    reasons.map(reason => reason.proof.moves.map(move => move.uci)),
    [playedMoves, playedMoves],
  );
  assert.ok(
    reasons.every(
      reason =>
        reason.message.kind === 'passed-pawn-result' &&
        reason.message.replyBranch.id === replyBranchId &&
        reason.message.replyBranch.sourceProbeId === 'probe.h8g8' &&
        reason.message.replyBranch.steps?.[1]?.provenance === 'certified_analysis_move' &&
        reason.message.expectedBranches[0]?.id === expectedBranchId &&
        reason.message.expectedBranches[0]?.steps?.[1]?.provenance === 'certified_analysis_move' &&
        reason.message.replyClosure.legalReplyMoves[0] === 'h8g8' &&
        reason.message.replyClosure.issuer === 'structural_delta.canonical_legal_reply_inventory' &&
        reason.message.replyClosure.issuerEvidenceId === 'structural.delta.reply.inventory' &&
        reason.message.replyClosure.coverageIssuer ===
          'passed_pawn_result_event.branch_complete_reply_coverage' &&
        reason.message.replyClosure.coverageEvidenceId === 'passed-pawn-result.event' &&
        reason.message.replyClosure.certifiedHorizonPlyOffset === 2 &&
        reason.message.rootActor.from === 'a6' &&
        reason.message.realizingActor.to === 'a8' &&
        reason.message.pathRealizationActor.to === 'a8' &&
        reason.message.pathRealizationMove === 'a7a8q' &&
        reason.message.pathRealizationPly === 3 &&
        reason.message.pathRealizationMatchKind === 'exact_move' &&
        reason.message.expectedOccurrenceSteps.length === 2 &&
        reason.message.expectedOccurrenceSteps[1]?.incomingLink?.kind === 'certified_causal_dependency' &&
        reason.message.replyOccurrenceSteps[1]?.line.rootMove === 'h8g8' &&
        reason.message.occurrenceLinkKeys.length === 2,
    ),
  );

  for (const invalidate of [
    (inventory: JsonObject) => delete inventory.coverage_evidence_id,
    (inventory: JsonObject) => {
      inventory.coverage_issuer = 'passed_pawn_result_event.branch_complete_legal_reply_inventory';
      return true;
    },
    (inventory: JsonObject) => {
      inventory.coverage_evidence_id = 'different.passed-pawn-result.event';
      return true;
    },
  ]) {
    const malformed = structuredClone(channel);
    const proof = object(malformed.passed_pawn_result_proof);
    invalidate(object(proof.closed_legal_reply_inventory));
    const rejected = decodeMoveReviewSnapshot(
      rawTypedResponse(focus, referenceMoves, playedMoves, 'passed_pawn_result', malformed),
      { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
    );
    assert.equal(rejected, undefined);
  }

  const assertPassedPawnResultRejected = (malformed: JsonObject, fixture = 'passed-pawn-result fixture') => {
    const rejected = decodeMoveReviewSnapshot(
      rawTypedResponse(focus, referenceMoves, playedMoves, 'passed_pawn_result', malformed),
      { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
    );
    assert.equal(rejected, undefined, fixture);
  };
  const malformedProof = (mutate: (proof: JsonObject) => void): JsonObject => {
    const malformed = structuredClone(channel) as JsonObject;
    mutate(object(malformed.passed_pawn_result_proof));
    return malformed;
  };
  for (const [fixtureIndex, malformed] of [
    malformedProof(proof => {
      object((object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[])[2]).role =
        'invented_result';
    }),
    malformedProof(proof => {
      object((object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[])[1]).lower_kind =
        'guessed_dependency';
    }),
    malformedProof(proof => {
      const premise = object((object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[])[3]);
      premise.branch_id = expectedBranchId;
      premise.branch_role = 'expected_result_route';
    }),
    malformedProof(proof => {
      object((object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[])[3]).to_step_index = 1;
    }),
    malformedProof(proof => {
      object(
        (object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[])[5],
      ).related_branch_ids = [];
    }),
    malformedProof(proof => {
      object(
        (object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[])[0],
      ).source_premise_ids = ['foreign'];
    }),
    malformedProof(proof => {
      const expectedBranch = object((proof.branches as JsonObject[])[0]);
      const resultStep = object((expectedBranch.steps as JsonObject[])[1]);
      object(resultStep.incoming_link).to_step_key = 'forged-endpoint';
    }),
    malformedProof(proof => {
      const expectedBranch = object((proof.branches as JsonObject[])[0]);
      const resultStep = object((expectedBranch.steps as JsonObject[])[1]);
      resultStep.step_key = 'forged-step';
      object(resultStep.incoming_link).to_step_key = 'forged-step';
    }),
    malformedProof(proof => {
      const expectedBranch = object((proof.branches as JsonObject[])[0]);
      const resultStep = object((expectedBranch.steps as JsonObject[])[1]);
      resultStep.line = { ...object(resultStep.line), line_id: 'foreign.expected.line' };
    }),
    malformedProof(proof => {
      object((proof.branches as JsonObject[])[1]).root_provenance = 'counterfactual_analyzed_root';
    }),
    malformedProof(proof => {
      const reply = object((proof.branches as JsonObject[])[1]);
      const resultStep = object((reply.steps as JsonObject[])[2]);
      resultStep.line = { ...object(resultStep.line), line_id: 'foreign.reply.line' };
    }),
    malformedProof(proof => {
      const paths = proof.proof_paths as JsonObject[];
      object(paths[1]).path_occurrence_id = object(paths[0]).path_occurrence_id;
    }),
    malformedProof(proof => {
      object(object((proof.proof_paths as JsonObject[])[0]).realization_actor).side = 'black';
    }),
    malformedProof(proof => {
      const branches = proof.branches as JsonObject[];
      const duplicateExpected = structuredClone(branches[0]) as JsonObject;
      duplicateExpected.branch_id = hash('9');
      branches.push(duplicateExpected);
    }),
    malformedProof(proof => {
      const branches = proof.branches as JsonObject[];
      const secondReply = structuredClone(branches[1]) as JsonObject;
      secondReply.branch_id = hash('8');
      secondReply.reply_move = 'h8h7';
      secondReply.source_probe_id = 'probe.h8h7';
      const secondSteps = secondReply.steps as JsonObject[];
      const secondReplyFen = '8/P6k/8/1P6/8/8/8/4K3 w - - 1 2' as FEN;
      const secondResultFen = 'Q7/7k/8/1P6/8/8/8/4K3 b - - 0 2' as FEN;
      object(secondSteps[1]).move_uci = 'h8h7';
      object(secondSteps[1]).fen_after = secondReplyFen;
      object(secondSteps[2]).fen_before = secondReplyFen;
      object(secondSteps[2]).fen_after = secondResultFen;
      object(object(secondSteps[1]).line).root_move = 'h8h7';
      object(object(secondSteps[2]).line).root_move = 'h8h7';
      const secondReplyStepKey = stepKey(2, 'h8h7' as Uci, fens[0]!, secondReplyFen);
      const secondResultStepKey = stepKey(3, 'a7a8q' as Uci, secondReplyFen, secondResultFen);
      object(secondSteps[1]).step_key = secondReplyStepKey;
      object(secondSteps[2]).step_key = secondResultStepKey;
      object(object(secondSteps[1]).incoming_link).to_step_key = secondReplyStepKey;
      object(object(secondSteps[2]).incoming_link).from_step_key = secondReplyStepKey;
      object(object(secondSteps[2]).incoming_link).to_step_key = secondResultStepKey;
      branches.push(secondReply);
      const inventory = object(proof.closed_legal_reply_inventory);
      (inventory.legal_reply_moves as string[]).push('h8h7');
      (inventory.branch_by_reply as JsonObject[]).push({ reply_move: 'h8h7', branch_id: hash('8') });
    }),
  ].entries())
    assertPassedPawnResultRejected(malformed, `malformed passed-pawn-result proof ${fixtureIndex}`);
  assertPassedPawnResultRejected(
    { ...structuredClone(channel), played_change: 'missed' },
    'legacy played_change',
  );
});

test('rejects retired public commentary projections', () => {
  for (const [field, retired] of [
    ['structural_idea_units', []],
    ['responsibility_links', []],
  ] as const) {
    const raw = rawResponse();
    const selected = object(raw.result).selected_move_reviews as JsonObject[];
    const commentary = selected[1]!.commentary as JsonObject;
    commentary[field] = retired;
    assert.equal(decodeMoveReviewSnapshot(raw, decodeContext()), undefined, field);
  }
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  const commentary = selected[1]!.commentary as JsonObject;
  const explanation = (commentary.causal_explanations as JsonObject[])[0]!;
  const facet = (explanation.facets as JsonObject[])[0]!;
  facet.only_move_qualifiers = [];
  assert.equal(decodeMoveReviewSnapshot(raw, decodeContext()), undefined, 'only_move_qualifiers');
});

test('rejects an explicitly empty causal explanation list', () => {
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  selected[1]!.commentary = rawCommentary({ causal_explanations: [] });
  assert.equal(decodeMoveReviewSnapshot(raw, decodeContext()), undefined);
});

test('accepts a standard causal channel whose schema-optional proof segment is absent', () => {
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  const commentary = selected[1]!.commentary as JsonObject;
  const explanation = (commentary.causal_explanations as JsonObject[])[0]!;
  const facet = (explanation.facets as JsonObject[])[0]!;
  const channel = (facet.channels as JsonObject[])[0]!;
  delete channel.proof_segment;
  const decoded = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  const cause = played.review.reasons.find(reason => reason.message.kind === 'causal');
  assert.equal(cause?.message.kind, 'causal');
  if (cause?.message.kind !== 'causal') return;
  assert.equal(cause.message.proofSegment, undefined);
  assert.deepEqual(
    cause.proof.moves.map(step => step.uci),
    ['e7e5', 'g1f3'],
  );
});

test('rejects a proof segment whose root-relative move does not match its proof line', () => {
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  const commentary = selected[1]!.commentary as JsonObject;
  const explanation = (commentary.causal_explanations as JsonObject[])[0]!;
  const facet = (explanation.facets as JsonObject[])[0]!;
  const channel = (facet.channels as JsonObject[])[0]!;
  const proof = channel.proof_segment as JsonObject;
  (proof.steps as JsonObject[])[1]!.move_uci = 'b1c3';
  assert.equal(decodeMoveReviewSnapshot(raw, decodeContext()), undefined);
});

test('preserves forced single moves and explicit position actions', () => {
  const forced = rawResponse({
    progress: {
      phase: 'completed',
      legal_move_count: 1,
      root_candidate_lines_admitted: 1,
      selected_commentaries_completed: 0,
      physical_works_issued: 1,
      physical_reports_accepted: 1,
      causal_waves_completed: 0,
    },
    result: {
      kind: 'forced_single_move',
      move_uci: 'e7e5',
      supporting_endpoint: {
        kind: 'engine_search',
        moves: ['e7e5', 'g1f3'],
        win_percent_for_mover: 50,
        depth: 16,
      },
    },
  });
  const forcedDecoded = decodeMoveReviewSnapshot(forced, decodeContext());
  assert.equal(forcedDecoded?.kind, 'completed');
  if (forcedDecoded?.kind === 'completed')
    assert.equal(forcedDecoded.evidence.candidates[0]?.review.kind, 'forced-single-move');

  const terminal = rawResponse({
    progress: {
      phase: 'completed',
      legal_move_count: 1,
      root_candidate_lines_admitted: 0,
      selected_commentaries_completed: 0,
      physical_works_issued: 0,
      physical_reports_accepted: 0,
      causal_waves_completed: 0,
    },
    result: { kind: 'automatic_terminal', terminal: { kind: 'stalemate' } },
  });
  assert.deepEqual(
    (decodeMoveReviewSnapshot(terminal, decodeContext()) as { action?: unknown } | undefined)?.action,
    { kind: 'automatic-terminal', terminal: { kind: 'stalemate' } },
  );

  const draw = rawResponse({
    result: {
      kind: 'draw_claim_action',
      claims: [{ rule: 'threefold_repetition', availability: 'available_now' }],
    },
  });
  assert.equal(decodeMoveReviewSnapshot(draw, decodeContext())?.kind, 'position-action');
});

test('emits only the v6 work identity and admitted line suffixes in reports', () => {
  const decoded = decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext());
  assert.equal(decoded?.kind, 'awaiting-core');
  if (decoded?.kind !== 'awaiting-core') return;
  const work = decoded.issuedEngineWork;
  const report = buildMoveReviewEngineWorkReport(work, {
    kind: 'completed',
    completedDepth: 16,
    selectiveDepth: 20,
    nodes: 100_000,
    engineTimeMs: 500,
    executorElapsedMs: 550,
    bestmoveUci: 'c7c5',
    lineSuffixes: [
      { moves: ['c7c5'], depth: 16, whiteScore: { kind: 'cp', value: 20 } },
      { moves: ['e7e5'], depth: 16, whiteScore: { kind: 'cp', value: 10 } },
      { moves: ['g8f6'], depth: 16, whiteScore: { kind: 'cp', value: 5 } },
    ],
  });
  assert.deepEqual(Object.keys(report), [
    'schema_version',
    'engine_profile',
    'work_id',
    'execution_key_sha256',
    'outcome',
  ]);
  assert.deepEqual(Object.keys(object(report.outcome)), ['kind', 'line_suffixes']);
});

test('requires exact D15 and D16 browser bundles before reporting D16', () => {
  const decoded = decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext());
  assert.equal(decoded?.kind, 'awaiting-core');
  if (decoded?.kind !== 'awaiting-core') return;
  const work = decoded.issuedEngineWork;
  const pvs = [
    { moves: ['c7c5'], cp: 20 },
    { moves: ['e7e5'], cp: 10 },
    { moves: ['g8f6'], cp: 5 },
  ];
  const previous = {
    fen: beforeFen,
    depth: 15,
    nodes: 50_000,
    millis: 250,
    pvs,
  } as Tree.LocalEval;
  const current = {
    fen: beforeFen,
    depth: 16,
    nodes: 100_000,
    millis: 500,
    bestmove: 'c7c5',
    pvs,
  } as Tree.LocalEval;
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(work, current, undefined), undefined);
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(work, current, previous)?.kind, 'completed');
});

test('supports standard subjects only and keeps locale in frontend formatting', () => {
  const nodes = [
    { fen: initialFen },
    { id: 'aa', fen: beforeFen, uci: 'e2e4', san: 'e4' },
  ] as Tree.NodeBase[];
  assert.ok(moveReviewSubjectFromNodeList('standard', 'aa', nodes));
  assert.equal(moveReviewSubjectFromNodeList('chess960', 'aa', nodes), undefined);
  assert.equal(normalizeMoveReviewLocale('ko-KR'), 'ko-KR');
  assert.equal(formatMoveReviewPercent(54.25, 'en-US'), '54.3%');
  assert.equal(formatMoveReviewPercentagePointChange(-8, 'en-US'), '-8.0%p');
});

test('executor failure report does not echo metrics or diagnostics to the server', () => {
  const work = (
    decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext()) as {
      issuedEngineWork: IssuedMoveReviewEngineWork;
    }
  ).issuedEngineWork;
  const report = buildMoveReviewEngineWorkReport(work, {
    kind: 'executor_failed',
    executorElapsedMs: 10,
    observedNodes: 1,
    engineTimeMs: 5,
    failureCode: 'engine_failure',
    diagnostic: 'local detail',
  });
  assert.deepEqual(report.outcome, { kind: 'executor_failed', failure_code: 'engine_failure' });
});
