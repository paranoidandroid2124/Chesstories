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
          cause_evidence_id: `cause.${facetKind}`,
          kind: facetKind,
          exposure: facetKind === 'passed_pawn_progress' ? 'complementary' : 'primary',
          channels: [channel],
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

test('decodes the root and focused comparison work', () => {
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

  const stopped = rawSnapshot('stopped');
  object(stopped.progress).legal_move_count = 0;
  assert.equal(decodeMoveReviewSnapshot(stopped, decodeContext())?.kind, 'abstained');

  const causalWaves = structuredClone(stopped);
  object(causalWaves.progress).causal_waves = 0;
  assert.equal(decodeMoveReviewSnapshot(causalWaves, decodeContext()), undefined);

  const inventedStopCondition = structuredClone(stopped);
  inventedStopCondition.stop_condition = 'no_legal_moves';
  assert.equal(decodeMoveReviewSnapshot(inventedStopCondition, decodeContext()), undefined);
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

  const illegal = rawSnapshot('awaiting_evidence');
  object(illegal.issued_engine_work).engine_position_moves_uci = ['e2e5'];
  assert.equal(decodeMoveReviewSnapshot(illegal, decodeContext()), undefined);
});

test('projects an exact primary while withholding an unproved L2 cause', () => {
  const decoded = decodeMoveReviewSnapshot(rawResponse(), decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.uci, 'e7e5');
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  assert.equal(played.review.core.bestUci, 'c7c5');
  assert.equal(played.review.core.verdictSymbol, '?!');
  assert.equal(played.review.reasons.length, 1);
  const line = played.review.reasons[0]!;
  assert.equal(line.message.kind, 'line');
  assert.equal(moveReviewReasonRole(played.review.core, line.id), 'primary');
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
    },
  });
  delete object(combined.commentary).causal_explanations;
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
  });
  assert.equal(core.winChance?.changePercentagePoints, 3.25);

  for (const mutate of [
    (primary: JsonObject) => (primary.runner_up_verdict_code = 'improves_on_reference'),
  ]) {
    const malformed = structuredClone(raw);
    const malformedResult = object(malformed.result);
    const malformedSelected = malformedResult.selected_move_reviews as JsonObject[];
    mutate(object(object(malformedSelected[0]!.commentary).primary));
    assert.equal(decodeMoveReviewSnapshot(malformed, decodeContext()), undefined);
  }
});

test('projects every unique-check-reply and sole-recapturer-removal proof path across both exact branch occurrences', () => {
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
        issuer_evidence_id: 'reference-line-evidence',
        issuer_occurrence_id: hash(suffix),
        source_premise_ids: [`created:${suffix}`, 'reference-line-evidence', hash(suffix)].sort(),
        branch_id: referenceBranchId,
        branch_role: 'counterfactual_reference',
        step_index: 0,
      },
      {
        role: 'reference_capture_recapture',
        contract: 'capture_recapture_inventory',
        result_id: `capture_recapture_inventory:${hash('8')}`,
        issuer_evidence_id: 'reference-line-evidence',
        issuer_occurrence_id: hash(suffix === '3' ? '4' : '5'),
        source_premise_ids: [
          `reference:${suffix}`,
          'reference-line-evidence',
          hash(suffix === '3' ? '4' : '5'),
        ].sort(),
        branch_id: referenceBranchId,
        branch_role: 'counterfactual_reference',
        step_index: 2,
      },
      {
        role: 'played_capture_recapture',
        contract: 'capture_recapture_inventory',
        result_id: `capture_recapture_inventory:${hash('9')}`,
        issuer_evidence_id: 'played-line-evidence',
        issuer_occurrence_id: hash(suffix === '3' ? '5' : '6'),
        source_premise_ids: [
          `played:${suffix}`,
          'played-line-evidence',
          hash(suffix === '3' ? '5' : '6'),
        ].sort(),
        branch_id: playedBranchId,
        branch_role: 'played_root_analysis_continuation',
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
    unique_check_reply_defender_displacement_before_capture_proof: {
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
        branch_role: 'played_root_analysis_continuation',
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
  const reasons = played.review.reasons.filter(
    reason => reason.message.kind === 'unique-check-reply-defender-displacement-before-capture',
  );
  assert.equal(reasons.length, 4);
  assert.deepEqual(
    reasons.map(reason => moveReviewReasonRole(played.review.core, reason.id)),
    ['primary', 'proof-route', 'proof-route', 'proof-route'],
    'one server-primary Cause owns one UI anchor while every additional occurrence remains a proof route',
  );
  assert.deepEqual(
    reasons.map(
      reason =>
        reason.message.kind === 'unique-check-reply-defender-displacement-before-capture' &&
        reason.message.branch.role,
    ),
    [
      'counterfactual_reference',
      'played_root_analysis_continuation',
      'counterfactual_reference',
      'played_root_analysis_continuation',
    ],
  );
  assert.deepEqual(
    reasons.map(reason => reason.proof.moves.map(move => move.uci)),
    [referenceMoves, playedMoves, referenceMoves, playedMoves],
  );
  assert.deepEqual(
    reasons.map(
      reason =>
        reason.message.kind === 'unique-check-reply-defender-displacement-before-capture' &&
        reason.message.pathOccurrenceId,
    ),
    [hash('3'), hash('3'), hash('4'), hash('4')],
  );
  assert.ok(
    reasons.every(
      reason =>
        reason.message.kind === 'unique-check-reply-defender-displacement-before-capture' &&
        reason.message.counterpart.id !== reason.message.branch.id &&
        reason.message.branch.steps?.[0]?.provenance !== undefined &&
        reason.message.counterpart.steps?.[0]?.provenance !== undefined &&
        reason.message.disabledDefender.square === 'f8' &&
        reason.message.absence.issuerEvidenceId === 'reference-line-evidence' &&
        reason.message.absence.issuerOccurrenceId === hash('5') &&
        reason.message.absence.fen === referenceFens[2] &&
        reason.message.premises.length === 3 &&
        reason.message.premises.every(
          premise =>
            premise.issuerEvidenceId !== undefined &&
            premise.issuerOccurrenceId !== undefined &&
            premise.sourcePremiseIds.includes(premise.issuerEvidenceId) &&
            premise.sourcePremiseIds.includes(premise.issuerOccurrenceId),
        ),
    ),
  );
  const forgedObservedSuffixChannel = structuredClone(channel) as JsonObject;
  const forgedObservedSuffixProof = object(
    forgedObservedSuffixChannel.unique_check_reply_defender_displacement_before_capture_proof,
  );
  const forgedObservedSuffixSteps = object(forgedObservedSuffixProof.played_root_branch)
    .steps as JsonObject[];
  object(forgedObservedSuffixSteps[1]).provenance = 'observed_game_move';
  assert.equal(
    decodeMoveReviewSnapshot(
      rawTypedResponse(focus, referenceMoves, playedMoves, 'wrong_move_order', forgedObservedSuffixChannel),
      { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
    ),
    undefined,
    'only the played root may claim observed-game provenance',
  );
  const originalReasonIds = new Map(
    reasons.map(reason => [`${reason.message.pathOccurrenceId}:${reason.message.branch.id}`, reason.id]),
  );
  const expandedChannel = structuredClone(channel);
  const expandedProof = object(expandedChannel.unique_check_reply_defender_displacement_before_capture_proof);
  (expandedProof.proof_paths as JsonObject[]).unshift(proofPath(hash('2'), '2'));
  const expandedDecoded = decodeMoveReviewSnapshot(
    rawTypedResponse(focus, referenceMoves, playedMoves, 'wrong_move_order', expandedChannel),
    { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
  );
  assert.equal(expandedDecoded?.kind, 'completed');
  if (expandedDecoded?.kind !== 'completed') return;
  const expandedPlayed = selectedMoveReviewCandidate(expandedDecoded.evidence);
  assert.equal(expandedPlayed?.review.kind, 'move-verdict');
  if (!expandedPlayed || expandedPlayed.review.kind !== 'move-verdict') return;
  const expandedReasons = expandedPlayed.review.reasons.filter(
    reason => reason.message.kind === 'unique-check-reply-defender-displacement-before-capture',
  );
  assert.equal(expandedReasons.length, 6);
  for (const reason of expandedReasons) {
    const key = `${reason.message.pathOccurrenceId}:${reason.message.branch.id}`;
    const originalId = originalReasonIds.get(key);
    if (originalId) assert.equal(reason.id, originalId, `stable occurrence-owned reason id for ${key}`);
  }
  for (const exposure of [undefined, 'guessed_from_path_count', 'complementary'] as const) {
    const malformed = rawTypedResponse(focus, referenceMoves, playedMoves, 'wrong_move_order', channel);
    const selected = object(malformed.result).selected_move_reviews as JsonObject[];
    const facet = (object(selected[1]!.commentary).causal_explanations as JsonObject[])[0]!;
    if (exposure === undefined) delete facet.exposure;
    else facet.exposure = exposure;
    assert.equal(
      decodeMoveReviewSnapshot(malformed, {
        requestId,
        subject: focus,
        engineProfile: moveReviewEngineProfile,
      }),
      undefined,
      'the UI requires the server-owned Cause exposure value',
    );
  }
  const secondPrimaryChannel = structuredClone(channel);
  secondPrimaryChannel.channel_id = 'typed.unique-check-reply.second-cause';
  const secondPrimaryProof = object(
    secondPrimaryChannel.unique_check_reply_defender_displacement_before_capture_proof,
  );
  (secondPrimaryProof.proof_paths as JsonObject[]).forEach((path, index) => {
    path.path_occurrence_id = hash(String(index + 7));
  });
  const multiplePrimary = rawTypedResponse(focus, referenceMoves, playedMoves, 'wrong_move_order', channel);
  const multiplePrimarySelected = object(multiplePrimary.result).selected_move_reviews as JsonObject[];
  const multiplePrimaryCommentary = object(multiplePrimarySelected[1]!.commentary);
  (multiplePrimaryCommentary.causal_explanations as JsonObject[]).push({
    cause_evidence_id: 'cause.wrong_move_order.second',
    kind: 'wrong_move_order',
    exposure: 'primary',
    channels: [secondPrimaryChannel],
  });
  const multiplePrimaryDecoded = decodeMoveReviewSnapshot(multiplePrimary, {
    requestId,
    subject: focus,
    engineProfile: moveReviewEngineProfile,
  });
  assert.equal(multiplePrimaryDecoded?.kind, 'completed');
  if (multiplePrimaryDecoded?.kind === 'completed') {
    const multiplePrimaryPlayed = selectedMoveReviewCandidate(multiplePrimaryDecoded.evidence);
    assert.equal(multiplePrimaryPlayed?.review.kind, 'move-verdict');
    if (multiplePrimaryPlayed?.review.kind === 'move-verdict') {
      assert.equal(
        multiplePrimaryPlayed.review.core.reasonRefs.primary.length,
        2,
        'each server-primary Cause retains one UI anchor',
      );
      assert.equal(
        multiplePrimaryPlayed.review.reasons.filter(
          reason => moveReviewReasonRole(multiplePrimaryPlayed.review.core, reason.id) === 'proof-route',
        ).length,
        6,
        'all remaining branch and proof-path occurrences stay available as routes',
      );
    }
  }

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
  const defensePath = {
    path_occurrence_id: hash('3'),
    premises: [
      {
        role: 'reference_defender_removal',
        contract: 'capture_recapture_inventory',
        result_id: `capture_recapture_inventory:${hash('0')}`,
        issuer_evidence_id: 'reference-line-evidence',
        issuer_occurrence_id: hash('0'),
        source_premise_ids: ['reference-removal:d', 'reference-line-evidence', hash('0')].sort(),
        branch_id: referenceBranchId,
        branch_role: 'counterfactual_reference',
        step_index: 0,
      },
      {
        role: 'reference_later_exploit_inventory',
        contract: 'capture_recapture_inventory',
        result_id: `capture_recapture_inventory:${hash('1')}`,
        issuer_evidence_id: 'reference-line-evidence',
        issuer_occurrence_id: hash('1'),
        source_premise_ids: ['reference-exploit:d', 'reference-line-evidence', hash('1')].sort(),
        branch_id: referenceBranchId,
        branch_role: 'counterfactual_reference',
        step_index: 2,
      },
      {
        role: 'played_immediate_exploit_inventory',
        contract: 'capture_recapture_inventory',
        result_id: `capture_recapture_inventory:${hash('2')}`,
        issuer_evidence_id: 'played-line-evidence',
        issuer_occurrence_id: hash('2'),
        source_premise_ids: ['played-exploit:d', 'played-line-evidence', hash('2')].sort(),
        branch_id: playedBranchId,
        branch_role: 'played_root_analysis_continuation',
        step_index: 0,
      },
    ],
    closed_absence_uses: [
      {
        use_id: hash('4'),
        role: 'reference_replacement_recapture_absent',
        semantic_proof_id: hash('6'),
        issuer: 'position_relation_extractor.closed_relation_inventory',
        issuer_evidence_id: 'reference-line-evidence',
        issuer_occurrence_id: hash('5'),
        query: 'legal-capture:black:d5',
        branch_id: referenceBranchId,
        branch_role: 'counterfactual_reference',
        after_step_index: 2,
        position: { fen: removalReferenceFens[2], ply: 3, scope: 'best_line' },
      },
    ],
  };
  const secondDefensePath = structuredClone(defensePath);
  secondDefensePath.path_occurrence_id = hash('4');
  secondDefensePath.premises.forEach(premise => {
    premise.source_premise_ids = premise.source_premise_ids
      .map(id =>
        id === premise.issuer_evidence_id || id === premise.issuer_occurrence_id ? id : `${id}:independent`,
      )
      .sort();
  });
  secondDefensePath.closed_absence_uses[0]!.use_id = hash('8');
  secondDefensePath.closed_absence_uses[0]!.issuer_occurrence_id = hash('9');
  const defenseChannel = {
    channel_id: 'typed.sole-recapturer-removal-before-target-capture',
    sole_recapturer_removal_before_target_capture_proof: {
      source_evidence_id: 'defense-obligation.source',
      semantic_id: hash('d'),
      occurrence_id: hash('e'),
      dependency_fingerprint: hash('f'),
      counterfactual_reference_branch: {
        branch_id: referenceBranchId,
        line_id: 'line.reference',
        line_role: 'best_reference',
        branch_role: 'counterfactual_reference',
        root_provenance: 'counterfactual_analyzed_root',
        line_rank: 1,
        root_move: removalReferenceMoves[0],
        steps: removalSteps(removalReferenceMoves, removalReferenceFens, false),
      },
      played_root_branch: {
        branch_id: playedBranchId,
        line_id: 'line.played',
        line_role: 'played',
        branch_role: 'played_root_analysis_continuation',
        root_provenance: 'observed_game_root',
        line_rank: 1,
        root_move: removalPlayedMoves[0],
        steps: removalSteps(removalPlayedMoves, removalPlayedFens, true),
      },
      proof_paths: [defensePath, secondDefensePath],
      participants: {
        remover: { side: 'white', from: 'g5', to: 'f6', piece_before: 'bishop', piece_after: 'bishop' },
        removed_defender: { side: 'black', piece: 'knight', square: 'f6' },
        removal_recapture: {
          side: 'black',
          from: 'e7',
          to: 'f6',
          piece_before: 'pawn',
          piece_after: 'pawn',
          move_uci: 'e7f6',
        },
        later_exploit: {
          side: 'white',
          from: 'd1',
          to: 'd5',
          piece_before: 'queen',
          piece_after: 'queen',
        },
        captured_target: { side: 'black', piece: 'rook', square: 'd5' },
        played_sole_recapture: {
          side: 'black',
          from: 'f6',
          to: 'd5',
          piece_before: 'knight',
          piece_after: 'knight',
          move_uci: 'f6d5',
        },
      },
      later_exploit_move: 'd1d5',
      played_sole_recapture_move: 'f6d5',
    },
  };
  const removalFocus = typedSubject(removalRoot, removalPlayedMoves[0]!, removalPlayedFens[0]!);
  const removalDecoded = decodeMoveReviewSnapshot(
    rawTypedResponse(
      removalFocus,
      removalReferenceMoves,
      removalPlayedMoves,
      'wrong_move_order',
      defenseChannel,
    ),
    { requestId, subject: removalFocus, engineProfile: moveReviewEngineProfile },
  );
  assert.equal(removalDecoded?.kind, 'completed');
  if (removalDecoded?.kind !== 'completed') return;
  const removalCandidate = selectedMoveReviewCandidate(removalDecoded.evidence);
  assert.equal(removalCandidate?.review.kind, 'move-verdict');
  if (!removalCandidate || removalCandidate.review.kind !== 'move-verdict') return;
  const defenseReasons = removalCandidate.review.reasons.filter(
    reason => reason.message.kind === 'sole-recapturer-removal-before-target-capture',
  );
  assert.equal(defenseReasons.length, 4);
  assert.ok(
    defenseReasons.every(
      reason =>
        reason.message.kind === 'sole-recapturer-removal-before-target-capture' &&
        reason.message.premises.map(premise => premise.role).join(',') ===
          'reference_defender_removal,reference_later_exploit_inventory,played_immediate_exploit_inventory' &&
        reason.message.premises.every(
          premise =>
            premise.issuerEvidenceId !== undefined &&
            premise.issuerOccurrenceId !== undefined &&
            premise.sourcePremiseIds.includes(premise.issuerEvidenceId) &&
            premise.sourcePremiseIds.includes(premise.issuerOccurrenceId),
        ),
    ),
  );
  assert.deepEqual(
    defenseReasons.map(
      reason =>
        reason.message.kind === 'sole-recapturer-removal-before-target-capture' &&
        reason.message.pathOccurrenceId,
    ),
    [hash('3'), hash('3'), hash('4'), hash('4')],
  );
  assert.match(
    moveReviewReasonText(defenseReasons[0]!, removalCandidate, 'en-US'),
    /captures the knight on f6, is recaptured by e7f6/,
  );
  assert.match(
    moveReviewReasonText(defenseReasons[0]!, removalCandidate, 'en-US'),
    /counterfactual reference analysis,.*g5.*f6.*observed played root.*certified analysis continuation.*d1d5.*f6d5/s,
  );
  assert.match(
    moveReviewReasonText(defenseReasons[1]!, removalCandidate, 'en-US'),
    /observed played root.*certified analysis continuation.*d1d5.*f6d5.*counterfactual reference analysis,.*g5.*f6/s,
  );

  const mismatchedResourceEndpoint = rawTypedResponse(
    focus,
    referenceMoves,
    playedMoves,
    'wrong_move_order',
    channel,
  );
  const resourceSelected = object(mismatchedResourceEndpoint.result).selected_move_reviews as JsonObject[];
  resourceSelected[0]!.move_uci = 'c4b3';
  object(object(resourceSelected[0]!.line_insight).endpoint).moves = ['c4b3'];
  object(object(resourceSelected[1]!.commentary).primary).reference_endpoint = {
    kind: 'engine_search',
    moves: ['c4b3'],
    win_percent_for_mover: 54,
    depth: 16,
  };
  assert.equal(
    decodeMoveReviewSnapshot(mismatchedResourceEndpoint, {
      requestId,
      subject: focus,
      engineProfile: moveReviewEngineProfile,
    }),
    undefined,
    'resource proof must belong to the primary reference endpoint',
  );

  const mismatchedDefenseEndpoint = rawTypedResponse(
    removalFocus,
    removalReferenceMoves,
    removalPlayedMoves,
    'wrong_move_order',
    defenseChannel,
  );
  const defenseSelected = object(mismatchedDefenseEndpoint.result).selected_move_reviews as JsonObject[];
  defenseSelected[0]!.move_uci = 'g5h4';
  object(object(defenseSelected[0]!.line_insight).endpoint).moves = ['g5h4'];
  object(object(defenseSelected[1]!.commentary).primary).reference_endpoint = {
    kind: 'engine_search',
    moves: ['g5h4'],
    win_percent_for_mover: 54,
    depth: 16,
  };
  assert.equal(
    decodeMoveReviewSnapshot(mismatchedDefenseEndpoint, {
      requestId,
      subject: removalFocus,
      engineProfile: moveReviewEngineProfile,
    }),
    undefined,
    'defense proof must belong to the primary reference endpoint',
  );

  const assertInvalidDefense = (mutate: (proof: JsonObject) => void) => {
    const invalid = structuredClone(defenseChannel);
    mutate(object(invalid.sole_recapturer_removal_before_target_capture_proof));
    assert.equal(
      decodeMoveReviewSnapshot(
        rawTypedResponse(
          removalFocus,
          removalReferenceMoves,
          removalPlayedMoves,
          'wrong_move_order',
          invalid,
        ),
        { requestId, subject: removalFocus, engineProfile: moveReviewEngineProfile },
      ),
      undefined,
    );
  };
  assertInvalidDefense(proof => {
    object((object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[])[2]).branch_id =
      referenceBranchId;
  });
  assertInvalidDefense(proof => {
    const premises = object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[];
    object(premises[2]).result_id = object(premises[0]).result_id;
  });
  assertInvalidDefense(proof => {
    const premises = object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[];
    object(premises[0]).role = 'reference_later_exploit_inventory';
  });
  assertInvalidDefense(proof => {
    object(object(proof.participants).captured_target).square = 'd4';
  });
  assertInvalidDefense(proof => {
    const path = object((proof.proof_paths as JsonObject[])[0]);
    object((path.closed_absence_uses as JsonObject[])[0]).query = 'legal-capture:white:d5';
  });
  assertInvalidDefense(proof => {
    object(object(proof.participants).removal_recapture).move_uci = 'e7e6';
  });
  const mixedTypedFamilies = structuredClone(defenseChannel);
  mixedTypedFamilies.unique_check_reply_defender_displacement_before_capture_proof = structuredClone(
    channel.unique_check_reply_defender_displacement_before_capture_proof,
  );
  assert.equal(
    decodeMoveReviewSnapshot(
      rawTypedResponse(
        removalFocus,
        removalReferenceMoves,
        removalPlayedMoves,
        'wrong_move_order',
        mixedTypedFamilies,
      ),
      { requestId, subject: removalFocus, engineProfile: moveReviewEngineProfile },
    ),
    undefined,
  );
  assert.equal(
    decodeMoveReviewSnapshot(
      rawTypedResponse(
        removalFocus,
        removalReferenceMoves,
        removalPlayedMoves,
        'passed_pawn_progress',
        defenseChannel,
      ),
      { requestId, subject: removalFocus, engineProfile: moveReviewEngineProfile },
    ),
    undefined,
  );

  const partial = structuredClone(channel);
  const partialProof = object(partial.unique_check_reply_defender_displacement_before_capture_proof);
  delete object((partialProof.proof_paths as JsonObject[])[0]).closed_absence_uses;
  const rejected = decodeMoveReviewSnapshot(
    rawTypedResponse(focus, referenceMoves, playedMoves, 'wrong_move_order', partial),
    { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
  );
  assert.equal(rejected, undefined);

  const assertRequiredProofField = (mutate: (proof: JsonObject) => void) => {
    const invalid = structuredClone(channel);
    mutate(object(invalid.unique_check_reply_defender_displacement_before_capture_proof));
    const result = decodeMoveReviewSnapshot(
      rawTypedResponse(focus, referenceMoves, playedMoves, 'wrong_move_order', invalid),
      { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
    );
    assert.equal(result, undefined);
  };
  assertRequiredProofField(proof => delete object(proof.participants).disabled_defender);
  assertRequiredProofField(proof => {
    const path = object((proof.proof_paths as JsonObject[])[0]);
    delete object((path.closed_absence_uses as JsonObject[])[0]).issuer_occurrence_id;
  });
  assertRequiredProofField(proof => {
    const path = object((proof.proof_paths as JsonObject[])[0]);
    delete object((path.premises as JsonObject[])[0]).issuer_evidence_id;
  });
  assertRequiredProofField(proof => {
    const path = object((proof.proof_paths as JsonObject[])[0]);
    object((path.premises as JsonObject[])[0]).issuer_occurrence_id = hash('f');
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
  assertRequiredProofField(proof => {
    const premises = object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[];
    object(premises[0]).role = 'reference_capture_recapture';
  });
  assertRequiredProofField(proof => {
    object(object(proof.participants).disabled_defender).square = 'f7';
  });
  assertRequiredProofField(proof => {
    const path = object((proof.proof_paths as JsonObject[])[0]);
    object((path.closed_absence_uses as JsonObject[])[0]).query = 'legal-capture:white:d8';
  });
  assertRequiredProofField(proof => {
    object(object(proof.participants).played_defense).move_uci = 'f8f7';
  });
});

test('projects exact vacated-gate slider-capture paths and rejects transport-integrity faults or generic mixing', () => {
  const root = '7k/q7/8/8/8/8/N7/R6K w - - 0 1' as FEN;
  const referenceMoves = ['a2b4', 'h8g8', 'a1a7'] as Uci[];
  const playedMoves = ['h1h2', 'h8g8'] as Uci[];
  const referenceFens = [
    '7k/q7/8/8/1N6/8/8/R6K b - - 1 1',
    '6k1/q7/8/8/1N6/8/8/R6K w - - 2 2',
    '6k1/R7/8/8/1N6/8/8/7K b - - 0 2',
  ] as FEN[];
  const playedFens = ['7k/q7/8/8/8/8/N6K/R7 b - - 1 1', '6k1/q7/8/8/8/8/N6K/R7 w - - 2 2'] as FEN[];
  const steps = (moves: Uci[], fens: FEN[], observed: boolean) =>
    moves.map((move, index) => ({
      step_index: index,
      provenance: observed && index === 0 ? 'observed_game_move' : 'certified_analysis_move',
      ply: index + 1,
      move_uci: move,
      fen_before: index === 0 ? root : fens[index - 1],
      fen_after: fens[index],
    }));
  const referenceId = hash('1');
  const playedId = hash('2');
  const sourceIds = (occurrence: string, lower: string) => ['line.reference', occurrence, lower].sort();
  const position = (fen: FEN, ply: number, scope: 'best_line' | 'played_line') => ({ fen, ply, scope });
  const closure = (
    useId: string,
    role: string,
    issuer: string,
    issuerEvidenceId: string,
    issuerOccurrenceId: string,
    query: string,
    branchId: string,
    branchRole: string,
    afterStepIndex: number,
    fen: FEN,
    ply: number,
    scope: 'best_line' | 'played_line',
  ) => ({
    use_id: useId,
    role,
    semantic_proof_id: hash(useId[0] === 'a' ? 'a' : 'b'),
    issuer,
    issuer_evidence_id: issuerEvidenceId,
    issuer_occurrence_id: issuerOccurrenceId,
    query,
    branch_id: branchId,
    branch_role: branchRole,
    after_step_index: afterStepIndex,
    position: position(fen, ply, scope),
  });
  const pathUseId = (digit: string, suffix: string) => `${digit.repeat(63)}${suffix === 'one' ? '1' : '2'}`;
  const path = (pathId: string, suffix: string) => ({
    path_occurrence_id: pathId,
    premises: [
      {
        role: 'reference_root_slider_reach',
        contract: 'slider_reach_delta',
        result_id: `slider_reach_delta:${hash('3')}`,
        issuer_evidence_id: 'line.reference',
        issuer_occurrence_id: hash('4'),
        source_premise_ids: sourceIds(hash('4'), `lower.reach.${suffix}`),
        branch_id: referenceId,
        branch_role: 'counterfactual_reference',
        step_index: 0,
      },
      {
        role: 'reference_exploit_capture',
        contract: 'capture_recapture_inventory',
        result_id: `capture_recapture_inventory:${hash('5')}`,
        issuer_evidence_id: 'line.reference',
        issuer_occurrence_id: hash('6'),
        source_premise_ids: sourceIds(hash('6'), `lower.capture.${suffix}`),
        branch_id: referenceId,
        branch_role: 'counterfactual_reference',
        step_index: 2,
      },
    ],
    closed_absence_uses: [
      closure(
        pathUseId('7', suffix),
        'reference_immediate_recapture_absent',
        'position_relation_extractor.closed_relation_inventory',
        'line.reference',
        hash('8'),
        'legal-capture:black:a7',
        referenceId,
        'counterfactual_reference',
        2,
        referenceFens[2]!,
        3,
        'best_line',
      ),
      closure(
        pathUseId('9', suffix),
        'played_exploit_move_absent',
        'position_relation_extractor.closed_relation_inventory',
        'line.played',
        hash('a'),
        'legal-move-from-to:white:a1:a7',
        playedId,
        'played_root_analysis_continuation',
        1,
        playedFens[1]!,
        2,
        'played_line',
      ),
      closure(
        pathUseId('b', suffix),
        'played_replacement_capture_absent',
        'position_relation_extractor.closed_relation_inventory',
        'line.played',
        hash('a'),
        'legal-capture:white:a7',
        playedId,
        'played_root_analysis_continuation',
        1,
        playedFens[1]!,
        2,
        'played_line',
      ),
    ],
    closed_state_uses: [
      closure(
        pathUseId('c', suffix),
        'reference_intervening_slider_reach',
        'position_relation_extractor.closed_position_state_inventory',
        'line.reference',
        hash('d'),
        'slider-reach:white:rook@a1:north:[a2:empty,a7:enemy:queen]:black:queen@a7',
        referenceId,
        'counterfactual_reference',
        1,
        referenceFens[1]!,
        2,
        'best_line',
      ),
      closure(
        pathUseId('d', suffix),
        'reference_target_persistence',
        'position_relation_extractor.closed_position_state_inventory',
        'line.reference',
        hash('d'),
        'occupied-by:black:queen@a7',
        referenceId,
        'counterfactual_reference',
        0,
        referenceFens[0]!,
        1,
        'best_line',
      ),
      closure(
        pathUseId('e', suffix),
        'reference_target_persistence',
        'position_relation_extractor.closed_position_state_inventory',
        'line.reference',
        hash('d'),
        'occupied-by:black:queen@a7',
        referenceId,
        'counterfactual_reference',
        1,
        referenceFens[1]!,
        2,
        'best_line',
      ),
      closure(
        pathUseId('f', suffix),
        'played_slider_persistence',
        'position_relation_extractor.closed_position_state_inventory',
        'line.played',
        hash('a'),
        'occupied-by:white:rook@a1',
        playedId,
        'played_root_analysis_continuation',
        0,
        playedFens[0]!,
        1,
        'played_line',
      ),
      closure(
        pathUseId('0', suffix),
        'played_target_persistence',
        'position_relation_extractor.closed_position_state_inventory',
        'line.played',
        hash('a'),
        'occupied-by:black:queen@a7',
        playedId,
        'played_root_analysis_continuation',
        0,
        playedFens[0]!,
        1,
        'played_line',
      ),
      closure(
        pathUseId('1', suffix),
        'played_gate_blocker_persistence',
        'position_relation_extractor.closed_position_state_inventory',
        'line.played',
        hash('a'),
        'occupied-by:white:knight@a2',
        playedId,
        'played_root_analysis_continuation',
        0,
        playedFens[0]!,
        1,
        'played_line',
      ),
      closure(
        pathUseId('2', suffix),
        'played_slider_persistence',
        'position_relation_extractor.closed_position_state_inventory',
        'line.played',
        hash('a'),
        'occupied-by:white:rook@a1',
        playedId,
        'played_root_analysis_continuation',
        1,
        playedFens[1]!,
        2,
        'played_line',
      ),
      closure(
        pathUseId('3', suffix),
        'played_target_persistence',
        'position_relation_extractor.closed_position_state_inventory',
        'line.played',
        hash('a'),
        'occupied-by:black:queen@a7',
        playedId,
        'played_root_analysis_continuation',
        1,
        playedFens[1]!,
        2,
        'played_line',
      ),
      closure(
        pathUseId('4', suffix),
        'played_gate_blocker_persistence',
        'position_relation_extractor.closed_position_state_inventory',
        'line.played',
        hash('a'),
        'occupied-by:white:knight@a2',
        playedId,
        'played_root_analysis_continuation',
        1,
        playedFens[1]!,
        2,
        'played_line',
      ),
      closure(
        pathUseId('5', suffix),
        'played_blocked_slider_reach',
        'position_relation_extractor.closed_position_state_inventory',
        'line.played',
        hash('a'),
        'slider-reach:white:rook@a1:north:[a2:friendly:knight]:white:knight@a2',
        playedId,
        'played_root_analysis_continuation',
        1,
        playedFens[1]!,
        2,
        'played_line',
      ),
    ],
  });
  const channel = {
    channel_id: 'typed.vacated-gate-slider-capture',
    vacated_gate_enables_unrecapturable_slider_capture_proof: {
      source_evidence_id: 'direct.source',
      semantic_id: hash('1'),
      occurrence_id: hash('2'),
      dependency_fingerprint: hash('3'),
      counterfactual_reference_branch: {
        branch_id: referenceId,
        line_id: 'line.reference',
        line_role: 'best_reference',
        branch_role: 'counterfactual_reference',
        root_provenance: 'counterfactual_analyzed_root',
        line_rank: 1,
        root_move: referenceMoves[0],
        steps: steps(referenceMoves, referenceFens, false),
      },
      played_root_branch: {
        branch_id: playedId,
        line_id: 'line.played',
        line_role: 'played',
        branch_role: 'played_root_analysis_continuation',
        root_provenance: 'observed_game_root',
        line_rank: 1,
        root_move: playedMoves[0],
        steps: steps(playedMoves, playedFens, true),
      },
      proof_paths: [path(hash('1'), 'one'), path(hash('2'), 'two')],
      participants: {
        enabler: { side: 'white', from: 'a2', to: 'b4', piece_before: 'knight', piece_after: 'knight' },
        slider: { side: 'white', piece: 'rook', square: 'a1' },
        gate_blocker: { side: 'white', piece: 'knight', square: 'a2' },
        exploit: { side: 'white', from: 'a1', to: 'a7', piece_before: 'rook', piece_after: 'rook' },
        captured_target: { side: 'black', piece: 'queen', square: 'a7' },
      },
      exploit_move: 'a1a7',
    },
  };
  const focus = typedSubject(root, playedMoves[0], playedFens[0]!);
  const decode = (candidate: JsonObject) =>
    decodeMoveReviewSnapshot(
      rawTypedResponse(focus, referenceMoves, playedMoves, 'missed_tactical_resource', candidate),
      { ...decodeContext(), subject: focus },
    );
  const decoded = decode(channel);
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const reviewed = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(reviewed?.review.kind, 'move-verdict');
  if (reviewed?.review.kind !== 'move-verdict') return;
  const reasons = reviewed.review.reasons.filter(
    reason => reason.message.kind === 'vacated-gate-enables-unrecapturable-slider-capture',
  );
  assert.equal(reasons.length, 4, 'two proof paths retain both branch occurrences');
  assert.deepEqual(
    [
      ...new Set(
        reasons.map(
          reason =>
            reason.message.kind === 'vacated-gate-enables-unrecapturable-slider-capture' &&
            reason.message.exploitMove,
        ),
      ),
    ],
    ['a1a7'],
  );

  const longRoot = '7k/q7/8/8/8/8/N3P3/R6K w - - 0 1' as FEN;
  const longReferenceMoves = ['a2b4', 'h8g8', 'e2e3', 'g8h8', 'a1a7'] as Uci[];
  const longPlayedMoves = ['h1h2', 'h8g8', 'e2e3', 'g8h8'] as Uci[];
  const longReferenceFens = [
    '7k/q7/8/8/1N6/8/4P3/R6K b - - 1 1',
    '6k1/q7/8/8/1N6/8/4P3/R6K w - - 2 2',
    '6k1/q7/8/8/1N6/4P3/8/R6K b - - 0 2',
    '7k/q7/8/8/1N6/4P3/8/R6K w - - 1 3',
    '7k/R7/8/8/1N6/4P3/8/7K b - - 0 3',
  ] as FEN[];
  const longPlayedFens = [
    '7k/q7/8/8/8/8/N3P2K/R7 b - - 1 1',
    '6k1/q7/8/8/8/8/N3P2K/R7 w - - 2 2',
    '6k1/q7/8/8/8/4P3/N6K/R7 b - - 0 2',
    '7k/q7/8/8/8/4P3/N6K/R7 w - - 1 3',
  ] as FEN[];
  const longChannel = structuredClone(channel);
  const longProof = object(longChannel.vacated_gate_enables_unrecapturable_slider_capture_proof);
  object(longProof.counterfactual_reference_branch).steps = steps(
    longReferenceMoves,
    longReferenceFens,
    false,
  ).map(step => ({ ...step, fen_before: step.step_index === 0 ? longRoot : step.fen_before }));
  object(longProof.played_root_branch).steps = steps(longPlayedMoves, longPlayedFens, true).map(step => ({
    ...step,
    fen_before: step.step_index === 0 ? longRoot : step.fen_before,
  }));
  for (const [pathIndex, candidatePath] of (longProof.proof_paths as JsonObject[]).entries()) {
    const candidate = object(candidatePath);
    object((candidate.premises as JsonObject[])[1]).step_index = 4;
    const absences = candidate.closed_absence_uses as JsonObject[];
    object(absences[0]).after_step_index = 4;
    object(absences[0]).position = position(longReferenceFens[4]!, 5, 'best_line');
    for (const absence of absences.slice(1)) {
      object(absence).after_step_index = 3;
      object(absence).position = position(longPlayedFens[3]!, 4, 'played_line');
    }
    let useIndex = 0;
    const nextUseId = () => `${(useIndex++).toString(16).padStart(63, '0')}${pathIndex + 1}`;
    candidate.closed_state_uses = [
      ...Array.from({ length: 3 }, (_, offset) => {
        const stepIndex = offset + 1;
        return closure(
          nextUseId(),
          'reference_intervening_slider_reach',
          'position_relation_extractor.closed_position_state_inventory',
          'line.reference',
          hash('d'),
          'slider-reach:white:rook@a1:north:[a2:empty,a7:enemy:queen]:black:queen@a7',
          referenceId,
          'counterfactual_reference',
          stepIndex,
          longReferenceFens[stepIndex]!,
          stepIndex + 1,
          'best_line',
        );
      }),
      ...Array.from({ length: 4 }, (_, stepIndex) =>
        closure(
          nextUseId(),
          'reference_target_persistence',
          'position_relation_extractor.closed_position_state_inventory',
          'line.reference',
          hash('d'),
          'occupied-by:black:queen@a7',
          referenceId,
          'counterfactual_reference',
          stepIndex,
          longReferenceFens[stepIndex]!,
          stepIndex + 1,
          'best_line',
        ),
      ),
      ...Array.from({ length: 4 }, (_, stepIndex) =>
        [
          ['played_slider_persistence', 'occupied-by:white:rook@a1'],
          ['played_target_persistence', 'occupied-by:black:queen@a7'],
          ['played_gate_blocker_persistence', 'occupied-by:white:knight@a2'],
        ].map(([role, query]) =>
          closure(
            nextUseId(),
            role!,
            'position_relation_extractor.closed_position_state_inventory',
            'line.played',
            hash('a'),
            query!,
            playedId,
            'played_root_analysis_continuation',
            stepIndex,
            longPlayedFens[stepIndex]!,
            stepIndex + 1,
            'played_line',
          ),
        ),
      ).flat(),
      closure(
        nextUseId(),
        'played_blocked_slider_reach',
        'position_relation_extractor.closed_position_state_inventory',
        'line.played',
        hash('a'),
        'slider-reach:white:rook@a1:north:[a2:friendly:knight]:white:knight@a2',
        playedId,
        'played_root_analysis_continuation',
        3,
        longPlayedFens[3]!,
        4,
        'played_line',
      ),
    ];
  }
  const longFocus = typedSubject(longRoot, longPlayedMoves[0]!, longPlayedFens[0]!);
  const longDecoded = decodeMoveReviewSnapshot(
    rawTypedResponse(longFocus, longReferenceMoves, longPlayedMoves, 'missed_tactical_resource', longChannel),
    { ...decodeContext(), subject: longFocus },
  );
  assert.equal(longDecoded?.kind, 'completed');
  if (longDecoded?.kind === 'completed') {
    const longReviewed = selectedMoveReviewCandidate(longDecoded.evidence);
    assert.equal(longReviewed?.review.kind, 'move-verdict');
    if (longReviewed?.review.kind === 'move-verdict') {
      const longReasons = longReviewed.review.reasons.filter(
        reason => reason.message.kind === 'vacated-gate-enables-unrecapturable-slider-capture',
      );
      assert.equal(longReasons.length, 4, 'k=4 retains every path and both branch occurrences');
      assert.deepEqual(
        longReasons.map(reason => reason.proof.moves.map(move => move.uci)),
        [longReferenceMoves, longPlayedMoves, longReferenceMoves, longPlayedMoves],
      );
      assert.ok(
        longReasons.every(
          reason =>
            reason.message.kind === 'vacated-gate-enables-unrecapturable-slider-capture' &&
            reason.message.states.length === 20 &&
            reason.message.states.filter(state => state.role === 'reference_intervening_slider_reach')
              .length === 3,
        ),
      );
    }
  }

  const complementaryResource = rawTypedResponse(
    focus,
    referenceMoves,
    playedMoves,
    'missed_tactical_resource',
    channel,
  );
  const complementaryResourceSelected = object(complementaryResource.result)
    .selected_move_reviews as JsonObject[];
  const complementaryResourceFacet = (
    object(complementaryResourceSelected[1]!.commentary).causal_explanations as JsonObject[]
  )[0]!;
  complementaryResourceFacet.exposure = 'complementary';
  assert.equal(
    decodeMoveReviewSnapshot(complementaryResource, { ...decodeContext(), subject: focus }),
    undefined,
    'missed tactical resource rejects complementary exposure',
  );

  const duplicateFacetMetadata = rawTypedResponse(
    focus,
    referenceMoves,
    playedMoves,
    'missed_tactical_resource',
    channel,
  );
  const duplicateMetadataSelected = object(duplicateFacetMetadata.result)
    .selected_move_reviews as JsonObject[];
  const duplicateMetadataExplanation = (
    object(duplicateMetadataSelected[1]!.commentary).causal_explanations as JsonObject[]
  )[0]!;
  duplicateMetadataExplanation.proof_confidence = 'legal_replay_verified';
  assert.equal(
    decodeMoveReviewSnapshot(duplicateFacetMetadata, { ...decodeContext(), subject: focus }),
    undefined,
    'retired duplicate facet metadata is rejected',
  );

  const mutations: Array<(proof: JsonObject, channel: JsonObject) => void> = [
    proof => {
      const paths = proof.proof_paths as JsonObject[];
      const absences = paths[0]!.closed_absence_uses as JsonObject[];
      absences[1]!.branch_id = referenceId;
    },
    (_proof, mutatedChannel) => {
      mutatedChannel.actor = { move_uci: 'a2b4', side: 'white', piece: 'knight', from: 'a2', to: 'b4' };
    },
    proof => {
      const path = object((proof.proof_paths as JsonObject[])[0]);
      object((path.premises as JsonObject[])[0]).role = 'reference_exploit_capture';
    },
    proof => {
      object(object(proof.participants).enabler).piece_after = 'queen';
    },
    proof => {
      const path = object((proof.proof_paths as JsonObject[])[0]);
      object((path.closed_absence_uses as JsonObject[])[0]).query = 'legal-capture:white:a7';
    },
    proof => {
      const path = object((proof.proof_paths as JsonObject[])[0]);
      object((path.closed_state_uses as JsonObject[])[0]).after_step_index = 0;
    },
    proof => {
      const path = object((proof.proof_paths as JsonObject[])[0]);
      const states = path.closed_state_uses as JsonObject[];
      object(states[4]).query = 'slider-reach:white:rook@a1:north:[a3:friendly:bishop]:white:bishop@a3';
    },
    proof => {
      const path = object((proof.proof_paths as JsonObject[])[0]);
      object((path.closed_state_uses as JsonObject[])[0]).query =
        'slider-reach:white:rook@a1:east:[a7:enemy:queen]:black:queen@a7';
    },
  ];
  for (const [index, mutate] of mutations.entries()) {
    const malformed = structuredClone(channel);
    mutate(object(malformed.vacated_gate_enables_unrecapturable_slider_capture_proof), malformed);
    assert.equal(decode(malformed), undefined, `vacated-gate mutation ${index}`);
  }
  assert.equal(
    decode({
      channel_id: 'generic.missed-tactical-resource',
      actor: { move_uci: 'a2b4', side: 'white', piece: 'knight', from: 'a2', to: 'b4' },
      targets: [],
      mechanisms: [],
      consequences: [],
      witnesses: [],
      proof_line_moves: ['a2b4'],
    }),
    undefined,
    'missed tactical resource requires the typed vacated-gate proof',
  );
});

test('projects the exact occupation SquareReleaseRoute and rejects legacy or forged fields', () => {
  const root = '1r4k1/p1q2p1p/2BRbp1B/4p3/P1p4P/6P1/1P2PP1K/3R4 w - - 0 30' as FEN;
  const referenceMoves = ['c6g2', 'e6f5', 'd6c6'] as Uci[];
  const playedMoves = ['d1d2', 'e6f5'] as Uci[];
  const referenceFens = [
    '1r4k1/p1q2p1p/3Rbp1B/4p3/P1p4P/6P1/1P2PPBK/3R4 b - - 1 30',
    '1r4k1/p1q2p1p/3R1p1B/4pb2/P1p4P/6P1/1P2PPBK/3R4 w - - 2 31',
    '1r4k1/p1q2p1p/2R2p1B/4pb2/P1p4P/6P1/1P2PPBK/3R4 b - - 3 31',
  ] as FEN[];
  const playedFens = [
    '1r4k1/p1q2p1p/2BRbp1B/4p3/P1p4P/6P1/1P1RPP1K/8 b - - 1 30',
    '1r4k1/p1q2p1p/2BR1p1B/4pb2/P1p4P/6P1/1P1RPP1K/8 w - - 2 31',
  ] as FEN[];
  const steps = (moves: Uci[], fens: FEN[], observed: boolean) =>
    moves.map((move, index) => ({
      step_index: index,
      provenance: observed && index === 0 ? 'observed_game_move' : 'certified_analysis_move',
      ply: 59 + index,
      move_uci: move,
      fen_before: index === 0 ? root : fens[index - 1],
      fen_after: fens[index],
    }));
  const referenceId = hash('1');
  const playedId = hash('2');
  const releaser = { side: 'white', from: 'c6', to: 'g2', piece_before: 'bishop', piece_after: 'bishop' };
  const routeLeg = { side: 'white', from: 'd6', to: 'c6', piece_before: 'rook', piece_after: 'rook' };
  const legalMove = (role: string, move: Uci, movement: JsonObject, stepIndex: number, digit: string) => {
    const semanticId = hash(digit);
    const occurrenceId = hash(digit === '4' ? '5' : '7');
    return {
      role,
      contract: 'legal_move',
      move_uci: move,
      movement,
      movement_mode: 'controlled_destination',
      legal_move_semantic_id: semanticId,
      issuer_evidence_id: 'line.reference',
      issuer_occurrence_id: occurrenceId,
      source_premise_ids: ['line.reference', occurrenceId, `legal-move:${semanticId}`].sort(),
      branch_id: referenceId,
      branch_role: 'counterfactual_reference',
      step_index: stepIndex,
    };
  };
  const closure = (
    useId: string,
    role: string,
    issuer: string,
    query: string,
    branchId: string,
    branchRole: string,
    stepIndex: number,
    fen: FEN,
    scope: 'best_line' | 'played_line',
  ) => ({
    use_id: useId,
    role,
    semantic_proof_id: hash(branchId === referenceId ? 'a' : 'b'),
    issuer,
    issuer_evidence_id: branchId === referenceId ? 'line.reference' : 'line.played',
    issuer_occurrence_id: hash(branchId === referenceId ? 'c' : 'd'),
    query,
    branch_id: branchId,
    branch_role: branchRole,
    after_step_index: stepIndex,
    position: { fen, ply: 59 + stepIndex, scope },
  });
  const channel = {
    channel_id: 'typed.square-release-route-occupation',
    square_release_route_proof: {
      source_evidence_id: 'square-release.source',
      semantic_id: hash('e'),
      occurrence_id: hash('f'),
      dependency_fingerprint: hash('0'),
      counterfactual_reference_branch: {
        branch_id: referenceId,
        line_id: 'line.reference',
        line_role: 'best_reference',
        branch_role: 'counterfactual_reference',
        root_provenance: 'counterfactual_analyzed_root',
        line_rank: 1,
        root_move: referenceMoves[0],
        steps: steps(referenceMoves, referenceFens, false),
      },
      played_root_branch: {
        branch_id: playedId,
        line_id: 'line.played',
        line_role: 'played',
        branch_role: 'played_root_analysis_continuation',
        root_provenance: 'observed_game_root',
        line_rank: 1,
        root_move: playedMoves[0],
        steps: steps(playedMoves, playedFens, true),
      },
      proof_paths: [
        {
          path_occurrence_id: hash('3'),
          premises: [
            legalMove('reference_release_move', 'c6g2' as Uci, releaser, 0, '4'),
            legalMove('reference_route_move_0', 'd6c6' as Uci, routeLeg, 2, '6'),
          ],
          closed_absence_uses: [
            closure(
              hash('8'),
              'played_first_route_leg_absent',
              'position_relation_extractor.closed_relation_inventory',
              'legal-move-from-to:white:d6:c6',
              playedId,
              'played_root_analysis_continuation',
              1,
              playedFens[1]!,
              'played_line',
            ),
          ],
          closed_state_uses: [
            closure(
              hash('4'),
              'reference_vacancy',
              'position_relation_extractor.closed_position_state_inventory',
              'vacant:c6',
              referenceId,
              'counterfactual_reference',
              0,
              referenceFens[0]!,
              'best_line',
            ),
            closure(
              hash('6'),
              'reference_vacancy',
              'position_relation_extractor.closed_position_state_inventory',
              'vacant:c6',
              referenceId,
              'counterfactual_reference',
              1,
              referenceFens[1]!,
              'best_line',
            ),
            closure(
              hash('9'),
              'reference_route_piece_0',
              'position_relation_extractor.closed_position_state_inventory',
              'occupied-by:white:rook@c6',
              referenceId,
              'counterfactual_reference',
              2,
              referenceFens[2]!,
              'best_line',
            ),
            closure(
              hash('a'),
              'played_blocker_persistence',
              'position_relation_extractor.closed_position_state_inventory',
              'occupied-by:white:bishop@c6',
              playedId,
              'played_root_analysis_continuation',
              0,
              playedFens[0]!,
              'played_line',
            ),
            closure(
              hash('b'),
              'played_blocker_persistence',
              'position_relation_extractor.closed_position_state_inventory',
              'occupied-by:white:bishop@c6',
              playedId,
              'played_root_analysis_continuation',
              1,
              playedFens[1]!,
              'played_line',
            ),
            closure(
              hash('c'),
              'played_route_origin_persistence',
              'position_relation_extractor.closed_position_state_inventory',
              'occupied-by:white:rook@d6',
              playedId,
              'played_root_analysis_continuation',
              0,
              playedFens[0]!,
              'played_line',
            ),
            closure(
              hash('d'),
              'played_route_origin_persistence',
              'position_relation_extractor.closed_position_state_inventory',
              'occupied-by:white:rook@d6',
              playedId,
              'played_root_analysis_continuation',
              1,
              playedFens[1]!,
              'played_line',
            ),
          ],
        },
      ],
      participants: {
        releaser,
        released_blocker: { side: 'white', piece: 'bishop', square: 'c6' },
        route_piece: { side: 'white', piece: 'rook', square: 'd6' },
      },
      route: [{ ...routeLeg, move_uci: 'd6c6', step_index: 2 }],
      terminal_step_index: 2,
      terminal: { kind: 'occupation' },
    },
  };
  const focus = typedSubject(root, playedMoves[0], playedFens[0]!);
  const decode = (candidate: JsonObject, facet = 'missed_square_release') =>
    decodeMoveReviewSnapshot(rawTypedResponse(focus, referenceMoves, playedMoves, facet, candidate), {
      ...decodeContext(),
      subject: focus,
    });
  const decoded = decode(channel);
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const reviewed = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(reviewed?.review.kind, 'move-verdict');
  if (reviewed?.review.kind !== 'move-verdict') return;
  const reasons = reviewed.review.reasons.filter(reason => reason.message.kind === 'square-release-route');
  assert.equal(reasons.length, 2, 'the reference and Played occurrences stay distinct');
  const englishReason = moveReviewReasonText(reasons[0]!, reviewed, 'en-US');
  assert.match(englishReason, /vacating c6/);
  assert.match(englishReason, /same-piece route d6c6/);
  assert.match(moveReviewReasonText(reasons[0]!, reviewed, 'ko-KR'), /c6을 비우고/);

  const mutations: Array<(proof: JsonObject) => void> = [
    proof => {
      const premise = object((object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[])[1]);
      premise.source_premise_ids = (premise.source_premise_ids as string[]).filter(
        id => id !== `legal-move:${hash('6')}`,
      );
    },
    proof => {
      const path = object((proof.proof_paths as JsonObject[])[0]);
      object((path.closed_absence_uses as JsonObject[])[0]).query = 'legal-move-from-to:white:d6:d5';
    },
    proof => {
      object(object(proof.participants).route_piece).square = 'b6';
    },
    proof => {
      const path = object((proof.proof_paths as JsonObject[])[0]);
      object((path.premises as JsonObject[])[1]).capture = { square: 'c6', piece: 'bishop', side: 'white' };
    },
    proof => {
      const path = object((proof.proof_paths as JsonObject[])[0]);
      const [release, route] = path.premises as JsonObject[];
      const previous = route!.issuer_occurrence_id as string;
      route!.issuer_occurrence_id = release!.issuer_occurrence_id;
      route!.source_premise_ids = (route!.source_premise_ids as string[])
        .map(id => (id === previous ? (release!.issuer_occurrence_id as string) : id))
        .sort();
    },
    proof => {
      const participants = object(proof.participants);
      object(participants.releaser).from = 'b6';
      object(participants.released_blocker).square = 'b6';
      object((proof.route as JsonObject[])[0]).to = 'b6';
      const path = object((proof.proof_paths as JsonObject[])[0]);
      const [release, route] = path.premises as JsonObject[];
      object(release!.movement).from = 'b6';
      object(route!.movement).to = 'b6';
      object((path.closed_absence_uses as JsonObject[])[0]).query = 'legal-move-from-to:white:d6:b6';
      const states = path.closed_state_uses as JsonObject[];
      object(states[0]).query = 'vacant:b6';
      object(states[1]).query = 'vacant:b6';
      object(states[2]).query = 'occupied-by:white:rook@b6';
      object(states[3]).query = 'occupied-by:white:bishop@b6';
      object(states[4]).query = 'occupied-by:white:bishop@b6';
    },
    proof => {
      const participants = object(proof.participants);
      object(participants.releaser).piece_after = 'queen';
      const path = object((proof.proof_paths as JsonObject[])[0]);
      object(object((path.premises as JsonObject[])[0]).movement).piece_after = 'queen';
    },
    proof => {
      proof.unexpected_field = 'd6c6';
    },
    proof => {
      proof.terminal_reply_move = 'g8h8';
    },
  ];
  for (const [index, mutate] of mutations.entries()) {
    const malformed = structuredClone(channel);
    mutate(object(malformed.square_release_route_proof));
    assert.equal(decode(malformed), undefined, `square-release occupation mutation ${index}`);
  }
  assert.equal(
    decode(channel, 'missed_tactical_resource'),
    undefined,
    'the exact proof cannot change Cause kind',
  );
});

test('preserves every multi-leg CreatedCheck route path and labels five responses without forcedness', () => {
  const root = '7k/p2rb3/6q1/8/8/8/8/K1BR4 w - - 0 1' as FEN;
  const referenceMoves = ['c1g5', 'a7a6', 'd1c1', 'a6a5', 'c1c8', 'h8g7'] as Uci[];
  const playedMoves = ['a1a2', 'a7a6'] as Uci[];
  const referenceFens = [
    '7k/p2rb3/6q1/6B1/8/8/8/K2R4 b - - 1 1',
    '7k/3rb3/p5q1/6B1/8/8/8/K2R4 w - - 0 2',
    '7k/3rb3/p5q1/6B1/8/8/8/K1R5 b - - 1 2',
    '7k/3rb3/6q1/p5B1/8/8/8/K1R5 w - - 0 3',
    '2R4k/3rb3/6q1/p5B1/8/8/8/K7 b - - 1 3',
    '2R5/3rb1k1/6q1/p5B1/8/8/8/K7 w - - 2 4',
  ] as FEN[];
  const playedFens = [
    '7k/p2rb3/6q1/8/8/8/K7/2BR4 b - - 1 1',
    '7k/3rb3/p5q1/8/8/8/K7/2BR4 w - - 0 2',
  ] as FEN[];
  const stepWire = (moves: Uci[], fens: FEN[], observed: boolean) =>
    moves.map((move, index) => ({
      step_index: index,
      provenance: observed && index === 0 ? 'observed_game_move' : 'certified_analysis_move',
      ply: 1 + index,
      move_uci: move,
      fen_before: index === 0 ? root : fens[index - 1],
      fen_after: fens[index],
    }));
  const referenceId = hash('1');
  const playedId = hash('2');
  const releaser = { side: 'white', from: 'c1', to: 'g5', piece_before: 'bishop', piece_after: 'bishop' };
  const route = [
    { side: 'white', from: 'd1', to: 'c1', piece_before: 'rook', piece_after: 'rook' },
    { side: 'white', from: 'c1', to: 'c8', piece_before: 'rook', piece_after: 'rook' },
  ];
  const responseResources = [
    { side: 'black', from: 'd7', to: 'd8', piece_before: 'rook', piece_after: 'rook', move_uci: 'd7d8' },
    { side: 'black', from: 'e7', to: 'f8', piece_before: 'bishop', piece_after: 'bishop', move_uci: 'e7f8' },
    { side: 'black', from: 'g6', to: 'g8', piece_before: 'queen', piece_after: 'queen', move_uci: 'g6g8' },
    { side: 'black', from: 'h8', to: 'g7', piece_before: 'king', piece_after: 'king', move_uci: 'h8g7' },
    { side: 'black', from: 'h8', to: 'h7', piece_before: 'king', piece_after: 'king', move_uci: 'h8h7' },
  ];
  const legalMove = (
    role: string,
    move: Uci,
    movement: JsonObject,
    stepIndex: number,
    semanticDigit: string,
    occurrenceDigit: string,
  ) => {
    const semanticId = hash(semanticDigit);
    const occurrenceId = hash(occurrenceDigit);
    return {
      role,
      contract: 'legal_move',
      move_uci: move,
      movement,
      movement_mode: 'controlled_destination',
      legal_move_semantic_id: semanticId,
      issuer_evidence_id: 'line.reference',
      issuer_occurrence_id: occurrenceId,
      source_premise_ids: ['line.reference', occurrenceId, `legal-move:${semanticId}`].sort(),
      branch_id: referenceId,
      branch_role: 'counterfactual_reference',
      step_index: stepIndex,
    };
  };
  const verticalPremise = (resultDigit: string, occurrenceDigit: string) => ({
    role: 'reference_terminal_resource',
    contract: 'created_check_response_inventory',
    result_id: `created_check_response_inventory:${hash(resultDigit)}`,
    issuer_evidence_id: 'line.reference',
    issuer_occurrence_id: hash(occurrenceDigit),
    source_premise_ids: ['line.reference', hash(occurrenceDigit), `vertical.${resultDigit}`].sort(),
    branch_id: referenceId,
    branch_role: 'counterfactual_reference',
    step_index: 4,
  });
  const closure = (
    useId: string,
    role: string,
    issuer: string,
    query: string,
    branchId: string,
    branchRole: string,
    stepIndex: number,
    fen: FEN,
    scope: 'best_line' | 'played_line',
  ) => ({
    use_id: useId,
    role,
    semantic_proof_id: hash('f'),
    issuer,
    issuer_evidence_id: branchId === referenceId ? 'line.reference' : 'line.played',
    issuer_occurrence_id: hash(branchId === referenceId ? 'e' : 'd'),
    query,
    branch_id: branchId,
    branch_role: branchRole,
    after_step_index: stepIndex,
    position: { fen, ply: 1 + stepIndex, scope },
  });
  const closedAbsenceUses = [
    closure(
      hash('0'),
      'played_first_route_leg_absent',
      'position_relation_extractor.closed_relation_inventory',
      'legal-move-from-to:white:d1:c1',
      playedId,
      'played_root_analysis_continuation',
      1,
      playedFens[1]!,
      'played_line',
    ),
  ];
  const closedStateUses = [
    closure(
      hash('1'),
      'reference_vacancy',
      'position_relation_extractor.closed_position_state_inventory',
      'vacant:c1',
      referenceId,
      'counterfactual_reference',
      0,
      referenceFens[0]!,
      'best_line',
    ),
    closure(
      hash('2'),
      'reference_vacancy',
      'position_relation_extractor.closed_position_state_inventory',
      'vacant:c1',
      referenceId,
      'counterfactual_reference',
      1,
      referenceFens[1]!,
      'best_line',
    ),
    closure(
      hash('3'),
      'reference_route_piece_0',
      'position_relation_extractor.closed_position_state_inventory',
      'occupied-by:white:rook@c1',
      referenceId,
      'counterfactual_reference',
      2,
      referenceFens[2]!,
      'best_line',
    ),
    closure(
      hash('4'),
      'reference_route_piece_1',
      'position_relation_extractor.closed_position_state_inventory',
      'occupied-by:white:rook@c8',
      referenceId,
      'counterfactual_reference',
      4,
      referenceFens[4]!,
      'best_line',
    ),
    closure(
      hash('5'),
      'reference_route_persistence_0',
      'position_relation_extractor.closed_position_state_inventory',
      'occupied-by:white:rook@c1',
      referenceId,
      'counterfactual_reference',
      3,
      referenceFens[3]!,
      'best_line',
    ),
    closure(
      hash('6'),
      'played_blocker_persistence',
      'position_relation_extractor.closed_position_state_inventory',
      'occupied-by:white:bishop@c1',
      playedId,
      'played_root_analysis_continuation',
      0,
      playedFens[0]!,
      'played_line',
    ),
    closure(
      hash('7'),
      'played_blocker_persistence',
      'position_relation_extractor.closed_position_state_inventory',
      'occupied-by:white:bishop@c1',
      playedId,
      'played_root_analysis_continuation',
      1,
      playedFens[1]!,
      'played_line',
    ),
    closure(
      hash('8'),
      'played_route_origin_persistence',
      'position_relation_extractor.closed_position_state_inventory',
      'occupied-by:white:rook@d1',
      playedId,
      'played_root_analysis_continuation',
      0,
      playedFens[0]!,
      'played_line',
    ),
    closure(
      hash('9'),
      'played_route_origin_persistence',
      'position_relation_extractor.closed_position_state_inventory',
      'occupied-by:white:rook@d1',
      playedId,
      'played_root_analysis_continuation',
      1,
      playedFens[1]!,
      'played_line',
    ),
  ];
  const path = (id: string, vertical: JsonObject) => ({
    path_occurrence_id: id,
    premises: [
      vertical,
      legalMove('reference_release_move', 'c1g5' as Uci, releaser, 0, '0', '1'),
      legalMove('reference_route_move_0', 'd1c1' as Uci, route[0]!, 2, '2', '3'),
      legalMove('reference_route_move_1', 'c1c8' as Uci, route[1]!, 4, '4', '5'),
      legalMove(
        'reference_terminal_reply',
        'h8g7' as Uci,
        { side: 'black', from: 'h8', to: 'g7', piece_before: 'king', piece_after: 'king' },
        5,
        '6',
        '7',
      ),
    ],
    closed_absence_uses: structuredClone(closedAbsenceUses),
    closed_state_uses: structuredClone(closedStateUses),
  });
  const channel = {
    channel_id: 'typed.square-release-route-created-check',
    square_release_route_proof: {
      source_evidence_id: 'square-release.source',
      semantic_id: hash('a'),
      occurrence_id: hash('b'),
      dependency_fingerprint: hash('c'),
      counterfactual_reference_branch: {
        branch_id: referenceId,
        line_id: 'line.reference',
        line_role: 'best_reference',
        branch_role: 'counterfactual_reference',
        root_provenance: 'counterfactual_analyzed_root',
        line_rank: 1,
        root_move: referenceMoves[0],
        steps: stepWire(referenceMoves, referenceFens, false),
      },
      played_root_branch: {
        branch_id: playedId,
        line_id: 'line.played',
        line_role: 'played',
        branch_role: 'played_root_analysis_continuation',
        root_provenance: 'observed_game_root',
        line_rank: 1,
        root_move: playedMoves[0],
        steps: stepWire(playedMoves, playedFens, true),
      },
      proof_paths: [path(hash('3'), verticalPremise('a', '8')), path(hash('4'), verticalPremise('b', '9'))],
      participants: {
        releaser,
        released_blocker: { side: 'white', piece: 'bishop', square: 'c1' },
        route_piece: { side: 'white', piece: 'rook', square: 'd1' },
      },
      route: [
        { ...route[0], move_uci: 'd1c1', step_index: 2 },
        { ...route[1], move_uci: 'c1c8', step_index: 4 },
      ],
      terminal_step_index: 4,
      terminal: {
        kind: 'created_check',
        assertion_id: hash('9'),
        checked_side: 'black',
        king_square: 'h8',
        checkers: [{ piece: 'rook', square: 'c8' }],
        responses: responseResources.map((resource, index) => ({
          resource,
          modes: [index < 3 ? 'interpose' : 'king_move'],
        })),
        controlled_king_destinations: [],
        terminal_state: 'ongoing',
      },
      terminal_reply_move: 'h8g7',
    },
  };
  const focus = typedSubject(root, playedMoves[0]!, playedFens[0]!);
  const decode = (candidate: JsonObject) =>
    decodeMoveReviewSnapshot(
      rawTypedResponse(focus, referenceMoves, playedMoves, 'missed_square_release', candidate),
      {
        ...decodeContext(),
        subject: focus,
      },
    );
  const decoded = decode(channel);
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const reviewed = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(reviewed?.review.kind, 'move-verdict');
  if (reviewed?.review.kind !== 'move-verdict') return;
  const reasons = reviewed.review.reasons.filter(reason => reason.message.kind === 'square-release-route');
  assert.equal(reasons.length, 4, 'two independent proof paths retain both time-ordered branch occurrences');
  assert.deepEqual(
    reasons.map(reason =>
      reason.message.kind === 'square-release-route' ? reason.message.pathOccurrenceId : undefined,
    ),
    [hash('3'), hash('3'), hash('4'), hash('4')],
  );
  assert.ok(
    reasons.every(
      reason =>
        reason.message.kind === 'square-release-route' &&
        reason.message.terminal.kind === 'created-check' &&
        reason.message.terminal.responses.length === 5 &&
        reason.message.terminalReplyMove === 'h8g7' &&
        reason.message.states.some(state => state.role === 'reference_route_persistence_0'),
    ),
  );
  const english = moveReviewReasonText(reasons[0]!, reviewed, 'en-US');
  assert.match(english, /5 certified legal responses/);
  assert.doesNotMatch(english, /\b(?:unique|forced|only)\b/i);
  assert.doesNotMatch(moveReviewReasonText(reasons[0]!, reviewed, 'ko-KR'), /유일|강제/);

  const captureChannel = structuredClone(channel);
  const captureProof = object(captureChannel.square_release_route_proof);
  captureProof.terminal = {
    kind: 'capture',
    assertion_id: hash('9'),
    captured_target: { side: 'black', piece: 'queen', square: 'c8' },
    geometric_recapturers: [],
    legal_recaptures: [],
    restricted_recaptures: [],
  };
  for (const candidatePath of captureProof.proof_paths as JsonObject[]) {
    const premises = object(candidatePath).premises as JsonObject[];
    const terminalPremise = object(premises[0]);
    terminalPremise.contract = 'capture_recapture_inventory';
    terminalPremise.result_id = `capture_recapture_inventory:${
      terminalPremise.issuer_occurrence_id === hash('8') ? hash('a') : hash('b')
    }`;
    object(premises[3]).capture = { side: 'black', piece: 'queen', square: 'c8' };
  }
  const captured = decode(captureChannel);
  assert.equal(
    captured?.kind,
    'completed',
    'a capture terminal retains its actual next reply without recapture inference',
  );
  if (captured?.kind === 'completed') {
    const capturedReview = selectedMoveReviewCandidate(captured.evidence);
    assert.equal(capturedReview?.review.kind, 'move-verdict');
    if (capturedReview?.review.kind === 'move-verdict') {
      assert.equal(
        capturedReview.review.reasons.filter(
          reason =>
            reason.message.kind === 'square-release-route' && reason.message.terminal.kind === 'capture',
        ).length,
        4,
      );
    }
  }

  const mateChannel = structuredClone(channel);
  const mateProof = object(mateChannel.square_release_route_proof);
  delete mateProof.terminal_reply_move;
  object(mateProof.counterfactual_reference_branch).steps = (
    object(mateProof.counterfactual_reference_branch).steps as JsonObject[]
  ).slice(0, 5);
  const mateTerminal = object(mateProof.terminal);
  mateTerminal.responses = [];
  mateTerminal.terminal_state = 'checkmate';
  for (const candidatePath of mateProof.proof_paths as JsonObject[]) {
    const pathWire = object(candidatePath);
    pathWire.premises = (pathWire.premises as JsonObject[]).slice(0, 4);
  }
  const mateDecoded = decodeMoveReviewSnapshot(
    rawTypedResponse(focus, referenceMoves.slice(0, 5), playedMoves, 'missed_square_release', mateChannel),
    { ...decodeContext(), subject: focus },
  );
  assert.equal(mateDecoded?.kind, 'completed', 'checkmate terminates the route without a reply occurrence');

  const mutations: Array<(proof: JsonObject) => void> = [
    proof => delete proof.terminal_reply_move,
    proof => {
      proof.terminal_reply_move = 'h8h7';
    },
    proof => {
      object(proof.terminal).terminal_state = 'checkmate';
    },
    proof => {
      const pathWire = object((proof.proof_paths as JsonObject[])[0]);
      const premises = pathWire.premises as JsonObject[];
      [premises[0], premises[1]] = [premises[1]!, premises[0]!];
    },
    proof => {
      const pathWire = object((proof.proof_paths as JsonObject[])[0]);
      const states = pathWire.closed_state_uses as JsonObject[];
      pathWire.closed_state_uses = states.filter(state => state.role !== 'reference_route_persistence_0');
    },
    proof => {
      const reply = object((object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[])[4]);
      reply.move_uci = 'h8h7';
      object(reply.movement).to = 'h7';
    },
    proof => {
      const pathWire = object((proof.proof_paths as JsonObject[])[0]);
      object(object((pathWire.premises as JsonObject[])[4]).movement).side = 'white';
    },
  ];
  for (const [index, mutate] of mutations.entries()) {
    const malformed = structuredClone(channel);
    mutate(object(malformed.square_release_route_proof));
    assert.equal(decode(malformed), undefined, `created-check route mutation ${index}`);
  }
});

test('keeps every independent passed-pawn analysis-continuation proof path and its exact reply closure', () => {
  const root = '7k/8/P7/1P6/8/8/8/4K3 w - - 0 1' as FEN;
  const referenceMoves = ['b5b6', 'h8g8', 'b6b7'] as Uci[];
  const playedMoves = ['a6a7', 'h8g8', 'a7a8q'] as Uci[];
  const fens = [
    '7k/P7/8/1P6/8/8/8/4K3 b - - 0 1',
    '6k1/P7/8/1P6/8/8/8/4K3 w - - 1 2',
    'Q5k1/8/8/1P6/8/8/8/4K3 b - - 0 2',
  ] as FEN[];
  const line = { line_id: 'line.played', line_role: 'played', line_rank: 1, root_move: 'a6a7' };
  const stepKey = (ply: number, move: Uci, before: FEN, after: FEN) =>
    ply + ':' + move + ':' + before + ':' + after;
  const rootStepKey = stepKey(1, 'a6a7' as Uci, root, fens[0]!);
  const replyStepKey = stepKey(2, 'h8g8' as Uci, fens[0]!, fens[1]!);
  const resultStepKey = stepKey(3, 'a7a8q' as Uci, fens[1]!, fens[2]!);
  const analysisContinuationBranchId = hash('1');
  const steps = [
    {
      step_index: 0,
      step_key: rootStepKey,
      ply: 1,
      move_uci: 'a6a7',
      fen_before: root,
      fen_after: fens[0],
      line,
      provenance: 'observed_game_move',
    },
    {
      step_index: 1,
      step_key: replyStepKey,
      ply: 2,
      move_uci: 'h8g8',
      fen_before: fens[0],
      fen_after: fens[1],
      line,
      provenance: 'certified_analysis_move',
    },
    {
      step_index: 2,
      step_key: resultStepKey,
      ply: 3,
      move_uci: 'a7a8q',
      fen_before: fens[1],
      fen_after: fens[2],
      line,
      provenance: 'certified_analysis_move',
    },
  ];
  const path = (id: string, suffix: string, occurrence: string) => {
    const stateOwner = 'object-state-owner.' + suffix;
    return {
      path_occurrence_id: id,
      analysis_continuation_branch_id: analysisContinuationBranchId,
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
      premises: [
        {
          role: 'dependency',
          lower_kind: 'passed_pawn_progress_dependency',
          lower_semantic_key: 'analysis-continuation-dependency:' + suffix,
          source_premise_ids: [stateOwner, occurrence, 'passed-pawn-result.event'].sort(),
          branch_id: analysisContinuationBranchId,
          branch_role: 'played_root_analysis_continuation',
          from_step_index: 0,
          to_step_index: 2,
          dependency_proof: {
            dependency_kind: 'object_state_precondition',
            proof_kind: 'object_state',
            squares: [
              { role: 'root_from', square: 'a6' },
              { role: 'root_to', square: 'a7' },
              { role: 'future_from', square: 'a7' },
              { role: 'future_to', square: 'a8' },
            ],
            pieces: [
              { role: 'root_before', side: 'white', piece: 'pawn' },
              { role: 'tracked', side: 'white', piece: 'pawn' },
              { role: 'future_after', side: 'white', piece: 'queen' },
            ],
            relation_issuers: [],
            position_state_issuers: [
              {
                state: { kind: 'occupied_by', side: 'white', square: 'a7', piece: 'pawn' },
                semantic_proof_id: hash('7'),
                issuer_evidence_id: stateOwner,
                issuer_occurrence_id: occurrence,
                step_key: replyStepKey,
                ply: 2,
                move_uci: 'h8g8',
                fen_before: fens[0],
                fen_after: fens[1],
                line,
                scope: 'played_line',
              },
            ],
          },
        },
        {
          role: 'result',
          lower_kind: 'passed_pawn_progress',
          lower_semantic_key: 'analysis-result',
          source_premise_ids: ['passed-pawn-result.event'],
          branch_id: analysisContinuationBranchId,
          branch_role: 'played_root_analysis_continuation',
          from_step_index: 2,
          to_step_index: 2,
        },
      ],
      closure_use_ids: [hash(suffix)],
    };
  };
  const channel = {
    channel_id: 'typed.passed-pawn-result',
    passed_pawn_progress_realized_after_only_legal_reply_proof: {
      source_evidence_id: 'passed-pawn-result.source',
      event_evidence_id: 'passed-pawn-result.event',
      semantic_id: hash('a'),
      occurrence_id: hash('b'),
      dependency_fingerprint: hash('c'),
      result_target_subjects: [
        '20:passed-pawn-promoted5:white2:a72:a8:relations:[removed:pawn_passage:' +
          hash('f') +
          ']:derived:[]',
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
        issuer_evidence_id: 'structural.delta.reply.inventory',
        root_after: { fen: fens[0], ply: 1, scope: 'played_transition' },
        legal_reply_move: 'h8g8',
        analysis_continuation_branch_id: analysisContinuationBranchId,
      },
      branches: [
        {
          branch_id: analysisContinuationBranchId,
          role: 'played_root_analysis_continuation',
          reply_move: 'h8g8',
          source_occurrence_id: hash('6'),
          line,
          root_provenance: 'observed_game_root',
          steps,
        },
      ],
      proof_paths: [path(hash('3'), '3', hash('8')), path(hash('4'), '4', hash('9'))],
      lower_premise_ids: ['passed-pawn-result.event', 'structural.delta.reply.inventory'],
    },
  };
  const focus = typedSubject(root, playedMoves[0]!, fens[0]!);
  const decode = (candidate: JsonObject) =>
    decodeMoveReviewSnapshot(
      rawTypedResponse(focus, referenceMoves, playedMoves, 'passed_pawn_progress', candidate),
      { requestId, subject: focus, engineProfile: moveReviewEngineProfile },
    );
  const decoded = decode(channel);
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  const reasons = played.review.reasons.filter(
    reason => reason.message.kind === 'passed-pawn-progress-realized-after-only-legal-reply',
  );
  assert.equal(reasons.length, 2);
  assert.deepEqual(
    reasons.map(
      reason =>
        reason.message.kind === 'passed-pawn-progress-realized-after-only-legal-reply' &&
        reason.message.pathOccurrenceId,
    ),
    [hash('3'), hash('4')],
  );
  assert.deepEqual(
    reasons.map(reason => reason.proof.moves.map(move => move.uci)),
    [playedMoves, playedMoves],
  );
  assert.ok(
    reasons.every(reason => moveReviewReasonRole(played.review.core, reason.id) === 'support'),
    'a complementary Cause stays supporting regardless of independent proof-path count',
  );
  assert.ok(
    reasons.every(
      reason =>
        reason.message.kind === 'passed-pawn-progress-realized-after-only-legal-reply' &&
        reason.message.analysisContinuationBranch.id === analysisContinuationBranchId &&
        reason.message.analysisContinuationBranch.sourceOccurrenceId === hash('6') &&
        reason.message.analysisContinuationSteps[1]?.provenance === 'certified_analysis_move' &&
        reason.message.replyClosure.legalReplyMove === 'h8g8' &&
        reason.message.replyClosure.analysisContinuationBranchId === analysisContinuationBranchId &&
        reason.message.eventEvidenceId === 'passed-pawn-result.event' &&
        reason.message.pathRealizationMove === 'a7a8q' &&
        reason.message.pathRealizationPly === 3 &&
        reason.message.premises.filter(premise => premise.role === 'dependency').length === 1,
    ),
  );

  const primaryProgress = rawTypedResponse(
    focus,
    referenceMoves,
    playedMoves,
    'passed_pawn_progress',
    channel,
  );
  const primaryProgressSelected = object(primaryProgress.result).selected_move_reviews as JsonObject[];
  const primaryProgressFacet = (
    object(primaryProgressSelected[1]!.commentary).causal_explanations as JsonObject[]
  )[0]!;
  primaryProgressFacet.exposure = 'primary';
  assert.equal(
    decodeMoveReviewSnapshot(primaryProgress, {
      requestId,
      subject: focus,
      engineProfile: moveReviewEngineProfile,
    }),
    undefined,
    'passed pawn progress rejects primary exposure',
  );

  const withoutRejudgedManifest = structuredClone(channel) as JsonObject;
  const manifestPath = object(
    (
      object(withoutRejudgedManifest.passed_pawn_progress_realized_after_only_legal_reply_proof)
        .proof_paths as JsonObject[]
    )[0],
  );
  const manifestPremises = manifestPath.premises as JsonObject[];
  delete object(manifestPremises[0]).dependency_proof;
  manifestPath.premises = [manifestPremises[1]!, manifestPremises[0]!];
  (manifestPath.closure_use_ids as string[]).push(hash('5'));
  assert.equal(
    decode(withoutRejudgedManifest)?.kind,
    'completed',
    'the browser preserves supplied premise order, optional witnesses, and closure uses without re-adjudicating the family theorem',
  );

  const withoutRejudgedDependencyFamily = structuredClone(channel) as JsonObject;
  const dependencyPremises = object(
    (
      object(withoutRejudgedDependencyFamily.passed_pawn_progress_realized_after_only_legal_reply_proof)
        .proof_paths as JsonObject[]
    )[0],
  ).premises as JsonObject[];
  const dependencyProof = object(object(dependencyPremises[0]).dependency_proof);
  dependencyProof.proof_kind = 'line_access';
  dependencyProof.squares = [];
  dependencyProof.pieces = [];
  object((dependencyProof.position_state_issuers as JsonObject[])[0]).state = {
    kind: 'slider_reach',
    side: 'white',
    square: 'a1',
    piece: 'rook',
    file_step: 1,
    rank_step: 1,
    segment: [],
  };
  assert.equal(
    decode(withoutRejudgedDependencyFamily)?.kind,
    'completed',
    'the browser transports sealed dependency and ray fields without rebuilding their chess theorem',
  );

  const malformed = (mutate: (proof: JsonObject) => void): JsonObject => {
    const copy = structuredClone(channel) as JsonObject;
    mutate(object(copy.passed_pawn_progress_realized_after_only_legal_reply_proof));
    return copy;
  };
  const rejected = [
    malformed(proof => {
      const duplicate = structuredClone((proof.branches as JsonObject[])[0]!) as JsonObject;
      duplicate.branch_id = hash('2');
      (proof.branches as JsonObject[]).push(duplicate);
    }),
    malformed(proof => {
      object(proof.closed_legal_reply_inventory).analysis_continuation_branch_id = hash('2');
    }),
    malformed(proof => {
      object((proof.branches as JsonObject[])[0]).reply_move = 'h8h7';
    }),
    malformed(proof => {
      object(proof.root_line).line_role = 'best_reference';
    }),
    malformed(proof => {
      object(object((proof.branches as JsonObject[])[0]).line).line_role = 'alternative';
    }),
    malformed(proof => {
      object((proof.branches as JsonObject[])[0]).root_provenance = 'counterfactual_analyzed_root';
    }),
    malformed(proof => {
      object(object(proof.closed_legal_reply_inventory).root_after).scope = 'reference_transition';
    }),
    malformed(proof => {
      const premises = object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[];
      const issuers = object(object(premises[0]).dependency_proof).position_state_issuers as JsonObject[];
      object(issuers[0]).scope = 'candidate_line';
    }),
    malformed(proof => {
      const analysisSteps = object((proof.branches as JsonObject[])[0]).steps as JsonObject[];
      object(analysisSteps[1]).provenance = 'observed_game_move';
    }),
    malformed(proof => {
      const analysisSteps = object((proof.branches as JsonObject[])[0]).steps as JsonObject[];
      object(analysisSteps[2]).fen_before = root;
      object(analysisSteps[2]).step_key = stepKey(
        object(analysisSteps[2]).ply as number,
        object(analysisSteps[2]).move_uci as Uci,
        root,
        object(analysisSteps[2]).fen_after as FEN,
      );
    }),
    malformed(proof => {
      object((proof.proof_paths as JsonObject[])[0]).analysis_continuation_branch_id = hash('2');
    }),
    malformed(proof => {
      const premises = object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[];
      object(premises[0]).branch_id = hash('2');
    }),
    malformed(proof => {
      const premises = object((proof.proof_paths as JsonObject[])[0]).premises as JsonObject[];
      const issuers = object(object(premises[0]).dependency_proof).position_state_issuers as JsonObject[];
      object(issuers[0]).step_key = 'detached-occurrence';
    }),
    malformed(proof => {
      const paths = proof.proof_paths as JsonObject[];
      object(paths[1]).path_occurrence_id = object(paths[0]).path_occurrence_id;
    }),
    malformed(proof => {
      object((proof.proof_paths as JsonObject[])[0]).unexpected_projection = true;
    }),
    malformed(proof => {
      object(proof.root_actor).piece_after = 'queen';
    }),
    malformed(proof => {
      object(proof.realizing_actor).piece_after = 'rook';
    }),
    malformed(proof => {
      object(object((proof.proof_paths as JsonObject[])[0]).realization_actor).from = 'b7';
    }),
  ];
  rejected.forEach((candidate, index) =>
    assert.equal(decode(candidate), undefined, 'malformed singleton analysis-continuation proof ' + index),
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
  for (const retiredKind of ['material_swing', 'draw_resource']) {
    const raw = rawResponse();
    const selected = object(raw.result).selected_move_reviews as JsonObject[];
    const commentary = selected[1]!.commentary as JsonObject;
    commentary.causal_explanations = [
      {
        cause_evidence_id: 'retired.generic-cause',
        kind: retiredKind,
        channels: [{}],
      },
    ];
    assert.equal(decodeMoveReviewSnapshot(raw, decodeContext()), undefined, retiredKind);
  }
  const wrapped = rawResponse();
  const selected = object(wrapped.result).selected_move_reviews as JsonObject[];
  const commentary = selected[1]!.commentary as JsonObject;
  commentary.causal_explanations = [{ kind: 'single_cause', facets: [] }];
  assert.equal(decodeMoveReviewSnapshot(wrapped, decodeContext()), undefined, 'retired singleton wrapper');
});

test('rejects an explicitly empty causal explanation list', () => {
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  selected[1]!.commentary = rawCommentary({ causal_explanations: [] });
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
