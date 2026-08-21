import assert from 'node:assert/strict';
import { test } from 'node:test';
import { moveReviewEngineProfile } from 'lib/ceval/types';
import {
  MoveReviewMemoryLru,
  buildMoveReviewEngineWorkReport,
  buildMoveReviewJobRequest,
  decodeMoveReviewSnapshot,
  formatMoveReviewPercent,
  formatMoveReviewPercentagePointChange,
  moveReviewCacheKey,
  moveReviewEngineOutcomeAtRequiredDepth,
  moveReviewReasonRole,
  moveReviewSubjectFromNodeList,
  moveReviewSubjectKey,
  normalizeMoveReviewLocale,
  selectedMoveReviewCandidate,
  type IssuedMoveReviewEngineWork,
} from '../src/moveReview';
import {
  afterFen,
  annotationPolicyRevision,
  beforeFen,
  compactReceipt,
  decodeContext,
  initialFen,
  judgmentRevision,
  playedMoveBudget,
  rawCompactReceipt,
  rawMetrics,
  rawProgress,
  rawResponse,
  rawSnapshot,
  requestId,
  subject,
} from './moveReviewTestSupport';

type JsonObject = Record<string, unknown>;

const object = (value: unknown): JsonObject => value as JsonObject;
const objects = (value: unknown): JsonObject[] => value as JsonObject[];

function selectedLineRelation(order: number): JsonObject {
  return {
    order: 0,
    relation_evidence_id: 'relation.reviewed',
    kind: 'hanging_piece',
    detail_kind: 'hanging_piece',
    authority: 'board_derived',
    scope: 'played_line',
    line_moves: ['e7e5'],
    participants: [{ square: 'e5', participant_role: 'target' }],
    proof_atoms: [{ role: 'line_move', move_uci: 'e7e5' }],
    focus_squares: ['e5'],
    target_square: 'e5',
    parent_evidence_ids: ['line.reviewed'],
    selected_reason_order: order,
  };
}

function staticPinRelation(): JsonObject {
  return {
    order: 0,
    relation_evidence_id: 'relation.pin',
    kind: 'pin',
    detail_kind: 'pin',
    authority: 'board_derived',
    scope: 'before_position',
    line_moves: [],
    participants: [
      { square: 'e1', participant_role: 'attacker', piece_role: 'rook', color: 'white' },
      { square: 'e7', participant_role: 'blocker', piece_role: 'knight', color: 'black' },
      { square: 'e8', participant_role: 'target', piece_role: 'king', color: 'black' },
      { square: 'e7', participant_role: 'target', piece_role: 'knight', color: 'black' },
    ],
    proof_atoms: [
      {
        role: 'participant',
        square: 'e1',
        participant_role: 'attacker',
        piece_role: 'rook',
        color: 'white',
      },
      {
        role: 'participant',
        square: 'e7',
        participant_role: 'blocker',
        piece_role: 'knight',
        color: 'black',
      },
      {
        role: 'participant',
        square: 'e8',
        participant_role: 'target',
        piece_role: 'king',
        color: 'black',
      },
      {
        role: 'participant',
        square: 'e7',
        participant_role: 'target',
        piece_role: 'knight',
        color: 'black',
      },
      { role: 'focus', square: 'e1' },
      { role: 'focus', square: 'e7' },
      { role: 'focus', square: 'e8' },
      { role: 'target', square: 'e7' },
    ],
    focus_squares: ['e1', 'e7', 'e8'],
    target_square: 'e7',
    parent_evidence_ids: ['board.before'],
  };
}

function decodedWork(state: 'awaiting_core' | 'awaiting_evidence' = 'awaiting_core') {
  const decoded = decodeMoveReviewSnapshot(
    rawSnapshot(state),
    decodeContext(state === 'awaiting_core' ? [] : [compactReceipt('work:0')]),
  );
  assert.ok(decoded?.kind === (state === 'awaiting_core' ? 'awaiting-core' : 'awaiting-evidence'));
  return decoded.issuedEngineWork;
}

function evaluation(depth: 15 | 16): Tree.LocalEval {
  return {
    fen: beforeFen,
    depth,
    seldepth: depth + 4,
    nodes: depth === 16 ? 100_000 : 80_000,
    millis: depth === 16 ? 500 : 400,
    bestmove: 'c7c5',
    pvs: [
      { moves: ['c7c5', 'g1f3'], cp: 20, depth },
      { moves: ['e7e5', 'g1f3'], cp: 8, depth },
    ],
  };
}

test('binds P → M → Q and emits the exact v6 request', () => {
  const nodes: Tree.NodeBase[] = [
    { id: '', ply: 0, fen: initialFen },
    { id: 'aa', ply: 1, fen: beforeFen, uci: 'e2e4', san: 'e4' },
    { id: 'bb', ply: 2, fen: afterFen, uci: 'e7e5', san: 'backend-authored SAN' },
  ];
  const derived = moveReviewSubjectFromNodeList('standard', 'aabb', nodes);
  assert.equal(derived?.played.san, 'backend-authored SAN');
  assert.deepEqual(derived?.movePrefixUci, ['e2e4']);
  assert.equal(derived?.before.path, 'aa');
  assert.equal(moveReviewSubjectFromNodeList('fromPosition', 'aabb', nodes), undefined);

  assert.deepEqual(buildMoveReviewJobRequest(requestId, subject, moveReviewEngineProfile), {
    schema_version: 'chesstory.position-commentary.job-request.v6',
    engine_profile: moveReviewEngineProfile,
    request_id: requestId,
    variant: 'standard',
    initial_fen: initialFen,
    move_prefix_uci: ['e2e4'],
    current_fen: beforeFen,
    focus: { kind: 'played_move', played_move_uci: 'e7e5', resulting_fen: afterFen },
  });
});

test('decodes all exact root shapes and the exact comparison shape', () => {
  for (const multiPv of [1, 2, 3]) {
    const raw = rawSnapshot('awaiting_core');
    object(object(raw.issued_engine_work).search_limits).multi_pv = multiPv;
    const decoded = decodeMoveReviewSnapshot(raw, decodeContext([]));
    assert.equal(decoded?.kind, 'awaiting-core');
    if (decoded?.kind === 'awaiting-core') {
      assert.equal(decoded.issuedEngineWork.workId, 'work:0');
      assert.equal(decoded.issuedEngineWork.searchLimits.multiPv, multiPv);
      assert.deepEqual(decoded.issuedEngineWork.rootRestriction, { kind: 'unrestricted' });
      assert.deepEqual(decoded.issuedEngineWork.searchLimits, {
        depth: 16,
        nodes: 5_000_000,
        movetimeMs: 5_000,
        multiPv,
      });
      assert.equal(decoded.issuedEngineWork.maxSearchElapsedMs, 6_000);
    }
  }

  const comparison = decodeMoveReviewSnapshot(
    rawSnapshot('awaiting_evidence'),
    decodeContext([compactReceipt('work:0')]),
  );
  assert.equal(comparison?.kind, 'awaiting-evidence');
  if (comparison?.kind === 'awaiting-evidence') {
    assert.equal(comparison.issuedEngineWork.workId, 'work:1');
    assert.equal(comparison.issuedEngineWork.searchLimits.multiPv, 2);
    assert.deepEqual(comparison.issuedEngineWork.rootRestriction, {
      kind: 'restricted',
      movesUci: ['c7c5', 'e7e5'],
    });
    assert.deepEqual(comparison.issuedEngineWork.searchLimits, {
      depth: 16,
      nodes: 2_000_000,
      movetimeMs: 2_500,
      multiPv: 2,
    });
    assert.equal(comparison.issuedEngineWork.maxSearchElapsedMs, 3_500);
  }
});

test('rejects every one-field work-shape mismatch and retired aliases', () => {
  const cases: Array<(raw: JsonObject) => void> = [
    raw => {
      object(raw.progress).phase = 'evidence_acquisition';
    },
    raw => {
      object(raw.issued_engine_work).work_id = 'work:2';
    },
    raw => {
      object(raw.issued_engine_work).generation = 1;
    },
    raw => {
      object(raw.issued_engine_work).execution_key_sha256 = 'bad';
    },
    raw => {
      object(object(raw.issued_engine_work).search_limits).depth = 15;
    },
    raw => {
      object(object(raw.issued_engine_work).search_limits).nodes = 4_999_999;
    },
    raw => {
      object(object(raw.issued_engine_work).search_limits).movetime_ms = 4_999;
    },
    raw => {
      object(object(raw.issued_engine_work).search_limits).multi_pv = 4;
    },
    raw => {
      object(object(raw.issued_engine_work).admission).minimum_completed_depth = 15;
    },
    raw => {
      object(raw.issued_engine_work).max_search_elapsed_ms = 5_999;
    },
    raw => {
      object(raw.issued_engine_work).required_depth = 16;
    },
    raw => {
      object(raw.issued_engine_work).multi_pv = 2;
    },
  ];
  for (const mutate of cases) {
    const raw = rawSnapshot('awaiting_core');
    mutate(raw);
    assert.equal(decodeMoveReviewSnapshot(raw, decodeContext([])), undefined);
  }

  const comparisonMutations: Array<(raw: JsonObject) => void> = [
    raw => {
      object(object(raw.issued_engine_work).root_restriction).moves_uci = ['e7e5'];
    },
    raw => {
      object(object(raw.issued_engine_work).root_restriction).moves_uci = ['e7e5', 'e7e5'];
    },
    raw => {
      object(object(raw.issued_engine_work).root_restriction).moves_uci = ['c7c5', 'g8f6'];
    },
    raw => {
      object(raw.issued_engine_work).max_search_elapsed_ms = 6_000;
    },
  ];
  for (const mutate of comparisonMutations) {
    const raw = rawSnapshot('awaiting_evidence');
    mutate(raw);
    assert.equal(
      decodeMoveReviewSnapshot(raw, decodeContext([compactReceipt('work:0')])),
      undefined,
    );
  }
});

test('binds generation, ordered work, progress, budget, and terminal receipts to one lifecycle', () => {
  assert.equal(decodeMoveReviewSnapshot(rawSnapshot('awaiting_evidence'), decodeContext([])), undefined);

  const repeatedRoot = rawSnapshot('awaiting_core');
  repeatedRoot.progress = rawProgress(
    'root_search',
    [compactReceipt('work:0')],
    object(repeatedRoot.issued_engine_work),
  );
  assert.equal(
    decodeMoveReviewSnapshot(repeatedRoot, decodeContext([compactReceipt('work:0')])),
    undefined,
  );

  const generationDrift = rawSnapshot('awaiting_core');
  generationDrift.generation = 1;
  object(generationDrift.issued_engine_work).generation = 1;
  assert.equal(decodeMoveReviewSnapshot(generationDrift, decodeContext([])), undefined);

  const budgetDrift = rawSnapshot('awaiting_core');
  budgetDrift.budget = { ...playedMoveBudget, maximum_total_nodes: 6_999_999 };
  assert.equal(decodeMoveReviewSnapshot(budgetDrift, decodeContext([])), undefined);

  const progressDrift = rawSnapshot('awaiting_core');
  object(progressDrift.progress).physical_works_issued = 2;
  assert.equal(decodeMoveReviewSnapshot(progressDrift, decodeContext([])), undefined);

  const duplicatePendingKey = rawSnapshot('awaiting_evidence');
  object(duplicatePendingKey.issued_engine_work).execution_key_sha256 = 'a'.repeat(64);
  assert.equal(
    decodeMoveReviewSnapshot(duplicatePendingKey, decodeContext([compactReceipt('work:0')])),
    undefined,
  );

  const missingReceipt = rawResponse();
  (missingReceipt.work_receipts as unknown[]).pop();
  assert.equal(decodeMoveReviewSnapshot(missingReceipt, decodeContext()), undefined);

  const alteredReceipt = rawResponse();
  object((alteredReceipt.work_receipts as unknown[])[0]).execution_key_sha256 = 'c'.repeat(64);
  assert.equal(decodeMoveReviewSnapshot(alteredReceipt, decodeContext()), undefined);

  const extraReceipt = rawResponse();
  (extraReceipt.work_receipts as unknown[]).push(rawCompactReceipt(compactReceipt('work:1')));
  assert.equal(decodeMoveReviewSnapshot(extraReceipt, decodeContext()), undefined);

  const duplicateReceiptKey = rawResponse();
  const duplicateComparisonReceipt = {
    ...compactReceipt('work:1'),
    executionKeySha256: 'a'.repeat(64),
  };
  object((duplicateReceiptKey.work_receipts as unknown[])[1]).execution_key_sha256 = 'a'.repeat(64);
  assert.equal(
    decodeMoveReviewSnapshot(
      duplicateReceiptKey,
      decodeContext([compactReceipt('work:0'), duplicateComparisonReceipt]),
    ),
    undefined,
  );
});

test('projects v6 played verdicts without inventing SAN, tags, or background reasons', () => {
  const decoded = decodeMoveReviewSnapshot(rawSnapshot('completed'), decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (played?.review.kind !== 'move-verdict') return;
  assert.equal(played.review.core.verdictCode, 'inaccuracy');
  assert.equal(played.review.core.verdictSymbol, '?!');
  assert.deepEqual(played.review.core.winChance, {
    referencePercent: 54,
    playedPercent: 46,
    changePercentagePoints: -8,
  });
  assert.equal(moveReviewReasonRole(played.review.core, 'line.reviewed'), 'primary');
  assert.equal(moveReviewReasonRole(played.review.core, 'line.reference'), undefined);
  assert.equal(played.review.reasons.length, 1);
  assert.equal(played.review.reasons[0]?.proof.startFen, beforeFen);
  assert.deepEqual(played.review.reasons[0]?.proof.moves, [
    { uci: 'e7e5', label: 'e7e5', fenAfter: afterFen },
  ]);
  assert.equal(played.label, 'e5');
  assert.equal(selectedMoveReviewCandidate(decoded.evidence, 'c7c5')?.label, 'c7c5');
  assert.equal(
    selectedMoveReviewCandidate(decoded.evidence, 'c7c5')?.review.kind,
    'single-candidate-insight',
  );
  assert.deepEqual(
    decoded.evidence.candidates.map(candidate => [candidate.uci, candidate.roles]),
    [
      ['c7c5', ['best']],
      ['e7e5', ['played']],
    ],
  );
});

test('fails closed on v5, subject drift, selected-reason drift, and broken replay continuity', () => {
  const v5 = rawSnapshot('completed');
  v5.schema_version = 'chesstory.move-review.snapshot.v5';
  assert.equal(decodeMoveReviewSnapshot(v5, decodeContext()), undefined);

  const wrongFocus = rawSnapshot('completed');
  object(wrongFocus.focus).resulting_fen = beforeFen;
  assert.equal(decodeMoveReviewSnapshot(wrongFocus, decodeContext()), undefined);

  const wrongOrder = rawSnapshot('completed');
  const played = objects(object(wrongOrder.result).selected_move_reviews)[1]!;
  const playedEpisodes = object(played.commentary).semantic_episodes as JsonObject[];
  playedEpisodes[0]!.selected_reason_order = 1;
  assert.equal(decodeMoveReviewSnapshot(wrongOrder, decodeContext()), undefined);

  const duplicateCarrier = rawSnapshot('completed');
  const duplicatePlayed = objects(object(duplicateCarrier.result).selected_move_reviews)[1]!;
  const duplicateEpisode = (object(duplicatePlayed.commentary).semantic_episodes as JsonObject[])[0]!;
  duplicateEpisode.relation_facts = [
    { relation_evidence_id: 'line.reviewed', selected_reason_order: 0 },
  ];
  assert.equal(decodeMoveReviewSnapshot(duplicateCarrier, decodeContext()), undefined);

  const brokenReplay = rawSnapshot('completed');
  const brokenPlayed = objects(object(brokenReplay.result).selected_move_reviews)[1]!;
  const brokenEpisode = (object(brokenPlayed.commentary).semantic_episodes as JsonObject[])[0]!;
  objects(brokenEpisode.replay_steps)[0]!.fen_before = initialFen;
  assert.equal(decodeMoveReviewSnapshot(brokenReplay, decodeContext()), undefined);

  const mismatchedVerdict = rawSnapshot('completed');
  const mismatchedPlayed = objects(object(mismatchedVerdict.result).selected_move_reviews)[1]!;
  object(object(mismatchedPlayed.commentary).primary).verdict_symbol = '??';
  assert.equal(decodeMoveReviewSnapshot(mismatchedVerdict, decodeContext()), undefined);

  const mismatchedDelta = rawSnapshot('completed');
  const deltaPlayed = objects(object(mismatchedDelta.result).selected_move_reviews)[1]!;
  object(object(object(deltaPlayed.commentary).primary).delta).candidate_win_percent_delta_for_mover = -7;
  assert.equal(decodeMoveReviewSnapshot(mismatchedDelta, decodeContext()), undefined);

  const extra = rawSnapshot('completed');
  extra.content_locale = 'en-US';
  assert.equal(decodeMoveReviewSnapshot(extra, decodeContext()), undefined);
});

test('binds reviewed and reference episodes, proof prefixes, and selected reason ownership', () => {
  const swapped = rawResponse();
  const swappedPlayed = objects(object(swapped.result).selected_move_reviews)[1]!;
  object(swappedPlayed.commentary).semantic_episodes = [
    ...(object(swappedPlayed.commentary).semantic_episodes as unknown[]),
  ].reverse();
  assert.equal(decodeMoveReviewSnapshot(swapped, decodeContext()), undefined);

  const referenceOwned = rawResponse();
  const referencePlayed = objects(object(referenceOwned.result).selected_move_reviews)[1]!;
  const referenceEpisodes = object(referencePlayed.commentary).semantic_episodes as JsonObject[];
  delete referenceEpisodes[0]!.selected_reason_order;
  referenceEpisodes[1]!.selected_reason_order = 0;
  assert.equal(decodeMoveReviewSnapshot(referenceOwned, decodeContext()), undefined);

  const wrongBoardOwner = rawResponse();
  const wrongBoardPlayed = objects(object(wrongBoardOwner.result).selected_move_reviews)[1]!;
  object(object(wrongBoardPlayed.commentary).position_context).fen = initialFen;
  assert.equal(decodeMoveReviewSnapshot(wrongBoardOwner, decodeContext()), undefined);

  const nonPrefix = rawResponse();
  const nonPrefixPlayed = objects(object(nonPrefix.result).selected_move_reviews)[1]!;
  const nonPrefixEpisode = (object(nonPrefixPlayed.commentary).semantic_episodes as JsonObject[])[0]!;
  nonPrefixEpisode.line_moves = ['e7e5', 'g1f3'];
  (nonPrefixEpisode.replay_steps as unknown[]).push({
    order: 1,
    ply: 2,
    move_uci: 'g1f3',
    from: 'g1',
    to: 'f3',
    fen_before: afterFen,
    fen_after: 'rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2',
  });
  assert.equal(decodeMoveReviewSnapshot(nonPrefix, decodeContext()), undefined);

  const extraSelected = rawResponse();
  const extraPlayed = objects(object(extraSelected.result).selected_move_reviews)[1]!;
  const extraEpisode = (object(extraPlayed.commentary).semantic_episodes as JsonObject[])[0]!;
  extraEpisode.relation_facts = [selectedLineRelation(1)];
  assert.equal(decodeMoveReviewSnapshot(extraSelected, decodeContext()), undefined);

  const relationReason = rawResponse();
  const relationPlayed = objects(object(relationReason.result).selected_move_reviews)[1]!;
  const relationPrimary = object(object(relationPlayed.commentary).primary);
  const relationEpisode = (object(relationPlayed.commentary).semantic_episodes as JsonObject[])[0]!;
  delete relationEpisode.selected_reason_order;
  relationEpisode.relation_facts = [selectedLineRelation(0)];
  relationPrimary.primary_reason_evidence_id = 'relation.reviewed';
  const relationDecoded = decodeMoveReviewSnapshot(relationReason, decodeContext());
  assert.equal(relationDecoded?.kind, 'completed');

  for (const mutate of [
    (relation: JsonObject) => {
      object((relation.proof_atoms as unknown[])[0]).move_uci = 'g8f6';
    },
    (relation: JsonObject) => {
      delete object((relation.proof_atoms as unknown[])[0]).move_uci;
    },
    (relation: JsonObject) => {
      relation.kind = 'invented_relation';
    },
  ]) {
    const invalid = structuredClone(relationReason);
    const invalidPlayed = objects(object(invalid.result).selected_move_reviews)[1]!;
    const invalidEpisode = (object(invalidPlayed.commentary).semantic_episodes as JsonObject[])[0]!;
    mutate(object((invalidEpisode.relation_facts as unknown[])[0]));
    assert.equal(decodeMoveReviewSnapshot(invalid, decodeContext()), undefined);
  }

  const pinBeforeFen = '4k3/4n3/8/5p2/8/7Q/8/K3R3 w - - 0 1' as FEN;
  const pinAfterFen = '4k3/4n3/8/5Q2/8/8/8/K3R3 b - - 0 1' as FEN;
  const directCauseSubject: typeof subject = {
    ...subject,
    initialFen: pinBeforeFen,
    movePrefixUci: [],
    before: { ...subject.before, fen: pinBeforeFen },
    played: { uci: 'h3f5', san: 'Qxf5' },
    after: { ...subject.after, fen: pinAfterFen },
  };
  const directCauseContext = { ...decodeContext(), subject: directCauseSubject };
  const directCauseReason = rawResponse();
  directCauseReason.current_fen = pinBeforeFen;
  object(directCauseReason.focus).played_move_uci = 'h3f5';
  object(directCauseReason.focus).resulting_fen = pinAfterFen;
  const directCauseReviews = objects(object(directCauseReason.result).selected_move_reviews);
  for (const review of directCauseReviews) {
    const commentary = object(review.commentary);
    object(commentary.position_context).fen = pinBeforeFen;
    for (const episode of objects(commentary.semantic_episodes))
      object((episode.replay_steps as unknown[])[0]).fen_before = pinBeforeFen;
  }
  const causePlayed = directCauseReviews[1]!;
  causePlayed.move_uci = 'h3f5';
  const causePrimary = object(object(causePlayed.commentary).primary);
  causePrimary.mover = 'white';
  object(causePrimary.played_endpoint).moves = ['h3f5'];
  const causeEpisode = (object(causePlayed.commentary).semantic_episodes as JsonObject[])[0]!;
  causeEpisode.root_move_uci = 'h3f5';
  causeEpisode.line_moves = ['h3f5'];
  const causeReplay = object((causeEpisode.replay_steps as unknown[])[0]);
  Object.assign(causeReplay, {
    move_uci: 'h3f5',
    from: 'h3',
    to: 'f5',
    fen_before: pinBeforeFen,
    fen_after: pinAfterFen,
  });
  delete causeEpisode.selected_reason_order;
  causeEpisode.material_captures = [
    {
      order: 0,
      move_uci: 'h3f5',
      ply_offset: 0,
      side: 'white',
      attacker_role: 'queen',
      captured_role: 'pawn',
      from: 'h3',
      to: 'f5',
      target_square: 'f5',
      value_cp: 100,
      recapture: false,
    },
  ];
  causeEpisode.relation_facts = [staticPinRelation()];
  causeEpisode.direct_cause_channels = [
    {
      order: 0,
      cause_evidence_id: 'cause.pin',
      authority: 'mixed',
      source_line_evidence_id: 'line.reviewed',
      static_relation_evidence_id: 'relation.pin',
      proof: {
        kind: 'legal_response_excluded_by_existing_absolute_pin',
        response_from: 'e7',
        response_to: 'f5',
      },
      parent_evidence_ids: ['line.reviewed', 'relation.pin'],
      selected_reason_order: 0,
    },
  ];
  causePrimary.primary_reason_evidence_id = 'cause.pin';
  assert.equal(decodeMoveReviewSnapshot(directCauseReason, directCauseContext)?.kind, 'completed');

  for (const mutate of [
    (episode: JsonObject) => {
      object((episode.relation_facts as unknown[])[0]).parent_evidence_ids = ['board.other'];
    },
    (episode: JsonObject) => {
      object((episode.relation_facts as unknown[])[0]).participants = [{}];
    },
    (episode: JsonObject) => {
      object((episode.relation_facts as unknown[])[0]).proof_atoms = [{}];
    },
    (episode: JsonObject) => {
      delete objects(object((episode.relation_facts as unknown[])[0]).proof_atoms)[0]!.square;
    },
    (episode: JsonObject) => {
      const participants = objects(object((episode.relation_facts as unknown[])[0]).participants);
      participants.find(participant => participant.piece_role === 'king')!.participant_role = 'king';
    },
    (episode: JsonObject) => {
      const participants = objects(object((episode.relation_facts as unknown[])[0]).participants);
      participants.find(participant => participant.piece_role === 'king')!.square = 'e7';
    },
    (episode: JsonObject) => {
      const participants = objects(object((episode.relation_facts as unknown[])[0]).participants);
      participants.find(participant => participant.participant_role === 'blocker')!.piece_role = 'king';
    },
    (episode: JsonObject) => {
      episode.material_captures = [];
    },
    (episode: JsonObject) => {
      object((episode.material_captures as unknown[])[0]).order = 1;
    },
    (episode: JsonObject) => {
      object((episode.material_captures as unknown[])[0]).from = 'h2';
    },
    (episode: JsonObject) => {
      object((episode.material_captures as unknown[])[0]).to = 'f4';
    },
    (episode: JsonObject) => {
      object((episode.material_captures as unknown[])[0]).target_square = 'f4';
    },
    (episode: JsonObject) => {
      object((episode.direct_cause_channels as unknown[])[0]).cause_evidence_id = 'relation.pin';
    },
  ]) {
    const invalid = structuredClone(directCauseReason);
    const invalidPlayed = objects(object(invalid.result).selected_move_reviews)[1]!;
    const invalidEpisode = (object(invalidPlayed.commentary).semantic_episodes as JsonObject[])[0]!;
    mutate(invalidEpisode);
    assert.equal(decodeMoveReviewSnapshot(invalid, directCauseContext), undefined);
  }

  const referenceInsight = rawResponse();
  const best = objects(object(referenceInsight.result).selected_move_reviews)[0]!;
  const bestEpisode = (object(best.commentary).semantic_episodes as JsonObject[])[0]!;
  bestEpisode.role = 'reference';
  assert.equal(decodeMoveReviewSnapshot(referenceInsight, decodeContext()), undefined);
});

test('maps honest abstentions without fabricating a verdict', () => {
  const selected = rawResponse();
  const played = objects(object(selected.result).selected_move_reviews)[1]!;
  delete played.commentary;
  played.abstention = {
    stage: 'failure',
    predicate: 'focus_engine_execution_failed',
    stop_condition: 'engine_execution_failed',
  };
  const decoded = decodeMoveReviewSnapshot(selected, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind === 'completed')
    assert.equal(selectedMoveReviewCandidate(decoded.evidence)?.review.kind, 'abstained');

  const analysis = rawResponse({
    result: {
      kind: 'analysis_abstention',
      stage: 'fact_selection',
      predicate: 'candidate_search_depth_below_minimum',
      reason: 'insufficient_search_depth',
    },
  });
  assert.equal(decodeMoveReviewSnapshot(analysis, decodeContext())?.kind, 'abstained');
});

test('preserves an explicit forced-single-move result without inventing a verdict', () => {
  const raw = rawResponse({
    result: {
      kind: 'forced_single_move',
      move_uci: 'e7e5',
      supporting_endpoint: {
        kind: 'engine_search',
        moves: ['e7e5'],
        win_percent_for_mover: 50,
        depth: 16,
      },
      draw_claims: [{ rule: 'fifty_move_rule', availability: 'available_now' }],
    },
  });
  const decoded = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const candidate = selectedMoveReviewCandidate(decoded.evidence);
  assert.deepEqual(candidate?.roles, ['best', 'played']);
  assert.equal(candidate?.review.kind, 'forced-single-move');
  if (candidate?.review.kind === 'forced-single-move')
    assert.deepEqual(candidate.review.lineUcis, ['e7e5']);
  assert.deepEqual(decoded.evidence.drawClaims, [
    { rule: 'fifty_move_rule', availability: 'available_now' },
  ]);

  const terminal = decodeMoveReviewSnapshot(
    rawResponse({
      result: {
        kind: 'forced_single_move',
        move_uci: 'e7e5',
        supporting_endpoint: {
          kind: 'exact_automatic_terminal',
          moves: ['e7e5'],
          terminal: { kind: 'checkmate', winner: 'black' },
        },
      },
    }),
    decodeContext(),
  );
  assert.equal(terminal?.kind, 'completed');
  if (terminal?.kind === 'completed') {
    const terminalReview = selectedMoveReviewCandidate(terminal.evidence)?.review;
    assert.equal(terminalReview?.kind, 'forced-single-move');
    if (terminalReview?.kind === 'forced-single-move') {
      assert.deepEqual(terminalReview.lineUcis, ['e7e5']);
      assert.deepEqual(terminalReview.terminal, { kind: 'checkmate', winner: 'black' });
    }
  }
});

test('preserves exact terminal and draw-claim position actions', () => {
  const noWork = {
    progress: rawProgress('completed', []),
    metrics: rawMetrics([]),
    work_receipts: [],
  };
  const terminal = decodeMoveReviewSnapshot(
    rawResponse({ ...noWork, result: { kind: 'automatic_terminal', terminal: { kind: 'stalemate' } } }),
    decodeContext([]),
  );
  assert.deepEqual(terminal?.kind === 'position-action' ? terminal.action : undefined, {
    kind: 'automatic-terminal',
    terminal: { kind: 'stalemate' },
  });

  const draw = decodeMoveReviewSnapshot(
    rawResponse({
      ...noWork,
      result: {
        kind: 'draw_claim_action',
        claims: [
          { rule: 'threefold_repetition', availability: 'available_now' },
          {
            rule: 'fifty_move_rule',
            availability: 'available_by_declared_move',
            move_ucis: ['e7e5'],
          },
        ],
      },
    }),
    decodeContext([]),
  );
  assert.equal(draw?.kind, 'position-action');
  if (draw?.kind === 'position-action' && draw.action.kind === 'draw-claim')
    assert.deepEqual(draw.action.claims[1]?.moveUcis, ['e7e5']);
});

test('preserves draw claims that accompany selected move reviews', () => {
  const raw = rawSnapshot('completed');
  object(raw.result).draw_claims = [
    { rule: 'threefold_repetition', availability: 'available_now' },
  ];
  const decoded = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind === 'completed')
    assert.deepEqual(decoded.evidence.drawClaims, [
      { rule: 'threefold_repetition', availability: 'available_now' },
    ]);

  object(raw.result).draw_claims = [];
  assert.equal(decodeMoveReviewSnapshot(raw, decodeContext()), undefined);
});

test('keeps terminal endpoints as outcomes rather than synthesizing win percentages', () => {
  const raw = rawSnapshot('completed');
  const played = objects(object(raw.result).selected_move_reviews)[1]!;
  const primary = object(object(played.commentary).primary);
  primary.verdict_confidence = 'mixed_verified';
  primary.delta = { kind: 'outcome_only', candidate_win_percent_delta_for_mover: -100 };
  primary.played_endpoint = {
    kind: 'exact_automatic_terminal',
    moves: ['e7e5'],
    terminal: { kind: 'checkmate', winner: 'white' },
  };
  const decoded = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const candidate = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(candidate?.review.kind, 'move-verdict');
  if (candidate?.review.kind === 'move-verdict') {
    assert.equal(candidate.review.core.winChance, undefined);
    assert.deepEqual(candidate.review.core.reviewedTerminal, { kind: 'checkmate', winner: 'white' });
  }
});

test('requires exact D15 and D16 bundles and echoes the completed receipt', () => {
  const work = decodedWork();
  const current = evaluation(16);
  const previous = evaluation(15);
  const outcome = moveReviewEngineOutcomeAtRequiredDepth(work, current, previous, 550);
  assert.equal(outcome?.kind, 'completed');
  if (outcome?.kind !== 'completed') return;
  assert.equal(outcome.selectiveDepth, 20);
  assert.equal(outcome.previousIteration.depth, 15);
  assert.deepEqual(outcome.lineSuffixes.map(line => line.multipvIndex), [1, 2]);
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(work, current, undefined), undefined);
  assert.equal(
    moveReviewEngineOutcomeAtRequiredDepth(work, current, { ...previous, depth: 14 }),
    undefined,
  );
  assert.equal(
    moveReviewEngineOutcomeAtRequiredDepth(work, { ...current, nodes: 5_000_001 }, previous),
    undefined,
  );

  const report = buildMoveReviewEngineWorkReport(work, outcome);
  assert.equal(report.schema_version, 'chesstory.position-commentary.engine-work-report.v6');
  assert.equal(report.work_id, 'work:0');
  const receipt = object(object(report.outcome).receipt);
  assert.deepEqual(receipt.search_limits, {
    depth: 16,
    nodes: 5_000_000,
    movetime_ms: 5_000,
    multi_pv: 2,
  });
  assert.equal(receipt.max_search_elapsed_ms, 6_000);
  assert.equal(object(receipt.previous_iteration).depth, 15);
  assert.equal((object(receipt.previous_iteration).ordered_lines as unknown[]).length, 2);
});

test('preserves a Chess960 castling UCI in the exact engine receipt', () => {
  const fen = 'k7/8/8/8/8/8/8/4K1R1 w K - 0 1' as FEN;
  const work: IssuedMoveReviewEngineWork = {
    engineProfile: moveReviewEngineProfile,
    workId: 'work:0',
    generation: 0,
    executionKeySha256: 'a'.repeat(64),
    variant: 'chess960',
    enginePositionInitialFen: fen,
    enginePositionMovesUci: [],
    searchFen: fen,
    rootRestriction: { kind: 'unrestricted' },
    searchLimits: { depth: 16, nodes: 5_000_000, movetimeMs: 5_000, multiPv: 1 },
    maxSearchElapsedMs: 6_000,
  };
  const evaluationAt = (depth: 15 | 16): Tree.LocalEval => ({
    fen,
    depth,
    seldepth: depth + 2,
    nodes: depth === 16 ? 100_000 : 80_000,
    millis: depth === 16 ? 500 : 400,
    bestmove: depth === 16 ? 'e1g1' : undefined,
    pvs: [{ moves: ['e1g1'], cp: 25, depth }],
  });
  const outcome = moveReviewEngineOutcomeAtRequiredDepth(
    work,
    evaluationAt(16),
    evaluationAt(15),
    550,
  );
  assert.equal(outcome?.kind, 'completed');
  if (outcome?.kind === 'completed') assert.deepEqual(outcome.lineSuffixes[0]?.moves, ['e1g1']);
});

test('echoes a complete bounded executor failure receipt', () => {
  const work = decodedWork('awaiting_evidence');
  const report = buildMoveReviewEngineWorkReport(work, {
    kind: 'executor_failed',
    executorElapsedMs: 3_500,
    observedNodes: 1_500_000,
    engineTimeMs: 2_500,
    failureCode: 'engine_lease_expired',
    diagnostic: 'The issued browser-engine lease expired.',
  });
  assert.deepEqual(report, {
    schema_version: 'chesstory.position-commentary.engine-work-report.v6',
    engine_profile: work.engineProfile,
    work_id: 'work:1',
    generation: 0,
    execution_key_sha256: 'b'.repeat(64),
    outcome: {
      kind: 'executor_failed',
      max_search_elapsed_ms: 3_500,
      executor_elapsed_ms: 3_500,
      observed_nodes: 1_500_000,
      engine_time_ms: 2_500,
      failure_code: 'engine_lease_expired',
      diagnostic: 'The issued browser-engine lease expired.',
    },
  });
});

test('uses bounded LRU storage and revision-aware cache identities', () => {
  const cache = new MoveReviewMemoryLru<number>(2);
  cache.set('a', 1);
  cache.set('b', 2);
  assert.equal(cache.get('a'), 1);
  cache.set('c', 3);
  assert.equal(cache.get('b'), undefined);

  const identity = {
    engineProfile: moveReviewEngineProfile,
    judgmentRevision,
    annotationPolicyRevision,
  };
  assert.notEqual(
    moveReviewCacheKey(subject, identity),
    moveReviewCacheKey(subject, { ...identity, judgmentRevision: 'changed' }),
  );
  assert.match(moveReviewSubjectKey(subject), /e7e5/);
});

test('keeps locale ownership in frontend chrome formatting only', () => {
  assert.equal(normalizeMoveReviewLocale('ko-KR'), 'ko-KR');
  assert.equal(normalizeMoveReviewLocale('fr-FR'), 'en-US');
  assert.equal(formatMoveReviewPercent(12.34, 'en-US'), '12.3%');
  assert.equal(formatMoveReviewPercentagePointChange(-7.24, 'en-US'), '-7.2%p');
});
