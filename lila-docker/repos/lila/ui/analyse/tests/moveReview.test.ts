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
  decodeContext,
  initialFen,
  judgmentRevision,
  rawCommentary,
  rawResponse,
  rawSnapshot,
  requestId,
  subject,
} from './moveReviewTestSupport';

type JsonObject = Record<string, unknown>;
const object = (value: unknown): JsonObject => value as JsonObject;

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
  assert.equal(cause.message.causeKind, 'center_control_gain');
  assert.deepEqual(cause.proof.moves.map(move => move.uci), ['e7e5', 'g1f3']);
  assert.equal(moveReviewReasonRole(played.review.core, cause.id), 'primary');
  const pv = played.review.reasons.find(reason => reason.message.kind === 'line');
  assert.ok(pv);
  assert.equal(moveReviewReasonRole(played.review.core, pv!.id), 'support');
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
});

test('preserves exact only-move qualifiers and multi-liability links independent of input order', () => {
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  const commentary = selected[1]!.commentary as JsonObject;
  const template = (commentary.causal_explanations as JsonObject[])[0]!;
  const liabilityA = structuredClone(template);
  const liabilityAFacet = (liabilityA.facets as JsonObject[])[0]!;
  liabilityAFacet.cause_evidence_id = 'cause.liability.a';
  const liabilityAChannel = (liabilityAFacet.channels as JsonObject[])[0]!;
  liabilityAChannel.channel_id = 'cause-channel:liability-a';

  const liabilityB = structuredClone(liabilityA);
  const liabilityBFacet = (liabilityB.facets as JsonObject[])[0]!;
  liabilityBFacet.cause_evidence_id = 'cause.liability.b';
  const liabilityBChannel = (liabilityBFacet.channels as JsonObject[])[0]!;
  liabilityBChannel.channel_id = 'cause-channel:liability-b';

  const resource = structuredClone(template);
  const resourceFacet = (resource.facets as JsonObject[])[0]!;
  resourceFacet.cause_evidence_id = 'cause.resource';
  resourceFacet.effect_mode = 'alternative_resource';
  resourceFacet.source_side = 'reference';
  resourceFacet.event_move = 'c7c5';
  resourceFacet.only_move_qualifiers = [
    {
      comparison_evidence_id: 'comparison.played-vs-best',
      cause_evidence_id: 'cause.resource',
      reference_line_id: 'line.best.exact-b',
      reference_line_role: 'best_reference',
      reference_line_rank: 1,
      reference_line_root_move: 'c7c5',
      relation: 'same_channel_association',
    },
    {
      comparison_evidence_id: 'comparison.played-vs-best',
      cause_evidence_id: 'cause.resource',
      reference_line_id: 'line.best.exact-a',
      reference_line_role: 'best_reference',
      reference_line_rank: 1,
      reference_line_root_move: 'c7c5',
      relation: 'same_channel_association',
    },
  ];
  const resourceChannel = (resourceFacet.channels as JsonObject[])[0]!;
  resourceChannel.channel_id = 'cause-channel:resource';
  resourceChannel.causal_signature = 'cause.resource:pressure';
  resourceChannel.actor = {
    move_uci: 'c7c5', side: 'black', piece: 'pawn:c7', from: 'c7', to: 'c5',
  };
  resourceChannel.proof_line_moves = ['c7c5', 'g1f3'];
  const resourceProof = resourceChannel.proof_segment as JsonObject;
  (resourceProof.steps as JsonObject[])[0]!.move_uci = 'c7c5';

  commentary.causal_explanations = [resource, liabilityB, liabilityA];
  commentary.responsibility_links = [
    {
      resource_cause_evidence_id: 'cause.resource',
      liability_cause_evidence_ids: ['cause.liability.b', 'cause.liability.a'],
    },
  ];

  const decoded = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  assert.deepEqual(
    played.review.onlyMoveQualifiers.map(qualifier => qualifier.referenceLine.id),
    ['line.best.exact-a', 'line.best.exact-b'],
  );
  assert.deepEqual(played.review.responsibilityLinks, [
    {
      resourceCauseEvidenceId: 'cause.resource',
      liabilityCauseEvidenceIds: ['cause.liability.a', 'cause.liability.b'],
    },
  ]);
  assert.deepEqual(
    played.review.reasons
      .flatMap(reason => reason.message.kind === 'causal' ? [reason.message.causeEvidenceId] : [])
      .sort(),
    ['cause.liability.a', 'cause.liability.b', 'cause.resource'],
  );

  const permuted = structuredClone(raw);
  const permutedSelected = object(permuted.result).selected_move_reviews as JsonObject[];
  const permutedCommentary = permutedSelected[1]!.commentary as JsonObject;
  (permutedCommentary.causal_explanations as JsonObject[]).reverse();
  const permutedResource = (permutedCommentary.causal_explanations as JsonObject[])
    .find(explanation => ((explanation.facets as JsonObject[])[0]!.cause_evidence_id === 'cause.resource'))!;
  (((permutedResource.facets as JsonObject[])[0]!.only_move_qualifiers) as JsonObject[]).reverse();
  const permutedLink = (permutedCommentary.responsibility_links as JsonObject[])[0]!;
  (permutedLink.liability_cause_evidence_ids as string[]).reverse();
  const permutedDecoded = decodeMoveReviewSnapshot(permuted, decodeContext());
  assert.equal(permutedDecoded?.kind, 'completed');
  if (permutedDecoded?.kind !== 'completed') return;
  const permutedPlayed = selectedMoveReviewCandidate(permutedDecoded.evidence);
  assert.equal(permutedPlayed?.review.kind, 'move-verdict');
  if (!permutedPlayed || permutedPlayed.review.kind !== 'move-verdict') return;
  assert.deepEqual(permutedPlayed.review.onlyMoveQualifiers, played.review.onlyMoveQualifiers);
  assert.deepEqual(permutedPlayed.review.responsibilityLinks, played.review.responsibilityLinks);

  const foreignQualifier = structuredClone(raw);
  const foreignSelected = object(foreignQualifier.result).selected_move_reviews as JsonObject[];
  const foreignCommentary = foreignSelected[1]!.commentary as JsonObject;
  const foreignResource = (foreignCommentary.causal_explanations as JsonObject[])
    .find(explanation => ((explanation.facets as JsonObject[])[0]!.cause_evidence_id === 'cause.resource'))!;
  const foreignItem = (((foreignResource.facets as JsonObject[])[0]!.only_move_qualifiers) as JsonObject[])[0]!;
  foreignItem.cause_evidence_id = 'cause.liability.a';
  assert.equal(decodeMoveReviewSnapshot(foreignQualifier, decodeContext()), undefined);

  const danglingLink = structuredClone(raw);
  const danglingSelected = object(danglingLink.result).selected_move_reviews as JsonObject[];
  const danglingCommentary = danglingSelected[1]!.commentary as JsonObject;
  const danglingItem = (danglingCommentary.responsibility_links as JsonObject[])[0]!;
  danglingItem.liability_cause_evidence_ids = ['cause.missing'];
  assert.equal(decodeMoveReviewSnapshot(danglingLink, decodeContext()), undefined);
});

test('retains a verified PV when no exact causal channel is selected', () => {
  const raw = rawResponse();
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  selected[1]!.commentary = rawCommentary({ causal_explanations: [] });
  const decoded = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  assert.deepEqual(played.review.reasons.map(reason => reason.message.kind), ['line']);
  assert.equal(played.review.core.reasonRefs.primary, 'pv:e7e5');
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
  const decoded = decodeMoveReviewSnapshot(raw, decodeContext());
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (!played || played.review.kind !== 'move-verdict') return;
  assert.deepEqual(played.review.reasons.map(reason => reason.message.kind), ['line']);
});

test('preserves forced single moves and explicit position actions', () => {
  const forced = rawResponse({
    progress: {
      phase: 'completed', legal_move_count: 1, root_candidate_lines_admitted: 1,
      selected_commentaries_completed: 0, physical_works_issued: 1, physical_reports_accepted: 1,
      causal_waves_completed: 0,
    },
    result: {
      kind: 'forced_single_move',
      move_uci: 'e7e5',
      supporting_endpoint: {
        kind: 'engine_search', moves: ['e7e5', 'g1f3'], win_percent_for_mover: 50, depth: 16,
      },
      presentation: 'e7e5 is the only legal move.',
    },
  });
  const forcedDecoded = decodeMoveReviewSnapshot(forced, decodeContext());
  assert.equal(forcedDecoded?.kind, 'completed');
  if (forcedDecoded?.kind === 'completed')
    assert.equal(forcedDecoded.evidence.candidates[0]?.review.kind, 'forced-single-move');

  const terminal = rawResponse({
    progress: {
      phase: 'completed', legal_move_count: 1, root_candidate_lines_admitted: 0,
      selected_commentaries_completed: 0, physical_works_issued: 0, physical_reports_accepted: 0,
      causal_waves_completed: 0,
    },
    result: { kind: 'automatic_terminal', terminal: { kind: 'stalemate' }, presentation: 'Stalemate.' },
  });
  assert.deepEqual(
    (decodeMoveReviewSnapshot(terminal, decodeContext()) as { action?: unknown } | undefined)?.action,
    { kind: 'automatic-terminal', terminal: { kind: 'stalemate' } },
  );

  const draw = rawResponse({
    result: {
      kind: 'draw_claim_action',
      claims: [{ rule: 'threefold_repetition', availability: 'available_now' }],
      presentation: 'Draw claim.',
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
    'schema_version', 'engine_profile', 'work_id', 'execution_key_sha256', 'outcome',
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
    fen: beforeFen, depth: 15, nodes: 50_000, millis: 250, pvs,
  } as Tree.LocalEval;
  const current = {
    fen: beforeFen, depth: 16, nodes: 100_000, millis: 500, bestmove: 'c7c5', pvs,
  } as Tree.LocalEval;
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(work, current, undefined), undefined);
  assert.equal(moveReviewEngineOutcomeAtRequiredDepth(work, current, previous)?.kind, 'completed');
});

test('keeps cache identities versioned and LRU bounded', () => {
  const key = moveReviewCacheKey(subject, {
    engineProfile: moveReviewEngineProfile,
    judgmentRevision,
    annotationPolicyRevision,
  });
  assert.ok(key.includes(judgmentRevision));
  assert.ok(key.includes(moveReviewSubjectKey(subject)) === false);
  const lru = new MoveReviewMemoryLru<number>(2);
  lru.set('a', 1);
  lru.set('b', 2);
  assert.equal(lru.get('a'), 1);
  lru.set('c', 3);
  assert.equal(lru.get('b'), undefined);
  assert.equal(lru.size, 2);
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
  const work = (decodeMoveReviewSnapshot(rawSnapshot('awaiting_core'), decodeContext()) as {
    issuedEngineWork: IssuedMoveReviewEngineWork;
  }).issuedEngineWork;
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
