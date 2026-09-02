import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { moveReviewEngineProfile } from 'lib/ceval/types';
import {
  buildMoveReviewEngineWorkReport,
  buildMoveReviewJobRequest,
  decodeMoveReviewSnapshot,
  formatMoveReviewPercent,
  formatMoveReviewPercentagePointChange,
  moveReviewEngineOutcomeAtRequiredDepth,
  moveReviewOccurrenceBranchProof,
  moveReviewOccurrenceBranches,
  moveReviewOccurrenceProofPaths,
  moveReviewProofById,
  moveReviewSubjectFromNodeList,
  normalizeMoveReviewLocale,
  selectedMoveReviewCandidate,
  type IssuedMoveReviewEngineWork,
  type MoveReviewProofKind,
  type MoveReviewSubject,
} from '../src/moveReview';
import {
  afterFen,
  beforeFen,
  decodeContext,
  initialFen,
  rawResponse,
  rawSnapshot,
  requestId,
  subject,
} from './moveReviewTestSupport';

type JsonObject = Record<string, unknown>;
const object = (value: unknown): JsonObject => value as JsonObject;
const objects = (value: unknown): JsonObject[] => value as JsonObject[];
const hashId = (value: number): string => value.toString(16).padStart(64, '0');

const producerFixtureUrl = new URL(
  '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/occurrence-explanation-produced.json',
  import.meta.url,
);
const relocationProducerFixtureUrl = new URL(
  '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/relocation-enables-recapture-produced.json',
  import.meta.url,
);

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

function producedResponse(): JsonObject {
  return JSON.parse(readFileSync(producerFixtureUrl, 'utf8')) as JsonObject;
}

function producedCommentary(raw: JsonObject): JsonObject {
  const selected = object(raw.result).selected_move_reviews as JsonObject[];
  return object(selected.find(review => object(review).commentary)?.commentary);
}

function producedOccurrence(raw = producedResponse()): JsonObject {
  return objects(producedCommentary(raw).occurrence_explanations)[0]!;
}

function producedRelocationResponse(): JsonObject {
  return JSON.parse(readFileSync(relocationProducerFixtureUrl, 'utf8')) as JsonObject;
}

function producedRelocationOccurrence(): JsonObject {
  return producedOccurrence(producedRelocationResponse());
}

function producedDecodeContext(raw: JsonObject): ReturnType<typeof decodeContext> {
  const focus = object(raw.focus);
  return {
    requestId: raw.request_id as string,
    subject: typedSubject(raw.current_fen as FEN, focus.played_move_uci as Uci, focus.resulting_fen as FEN),
    engineProfile: moveReviewEngineProfile,
  };
}

function decodeProduced(raw: JsonObject) {
  return decodeMoveReviewSnapshot(raw, producedDecodeContext(raw));
}

function responseWithOccurrences(occurrences: JsonObject[]): JsonObject {
  const raw = producedResponse();
  producedCommentary(raw).occurrence_explanations = occurrences;
  return raw;
}

function copyBranch(source: JsonObject, branchRole: string, maximumSteps?: number): JsonObject {
  const branch = structuredClone(source);
  branch.branch_role = branchRole;
  if (maximumSteps !== undefined) branch.steps = objects(branch.steps).slice(0, maximumSteps);
  return branch;
}

function branchStep(branch: JsonObject, index: number): JsonObject {
  return objects(branch.steps)[index]!;
}

let fixtureSerial = 100;

function nextHash(): string {
  return hashId(fixtureSerial++);
}

function relationPremise(role: string, contract: string, branch: JsonObject, stepIndex: number): JsonObject {
  const issuerOccurrenceId = nextHash();
  return {
    role,
    contract,
    result_id: contract + ':' + nextHash(),
    issuer_evidence_id: 'schema-fixture',
    issuer_occurrence_id: issuerOccurrenceId,
    source_premise_ids: [issuerOccurrenceId, 'schema-fixture', 'source:' + nextHash()],
    branch_id: branch.branch_id,
    branch_role: branch.branch_role,
    step_index: stepIndex,
  };
}

function legalPremise(
  role: string,
  branch: JsonObject,
  stepIndex: number,
  movement: JsonObject,
  movementMode: string,
  capture?: JsonObject,
): JsonObject {
  const issuerOccurrenceId = nextHash();
  return {
    role,
    contract: 'legal_move',
    move_uci: branchStep(branch, stepIndex).move_uci,
    movement: structuredClone(movement),
    movement_mode: movementMode,
    legal_move_semantic_id: nextHash(),
    issuer_evidence_id: 'schema-fixture',
    issuer_occurrence_id: issuerOccurrenceId,
    source_premise_ids: [issuerOccurrenceId, 'schema-fixture', 'source:' + nextHash()],
    branch_id: branch.branch_id,
    branch_role: branch.branch_role,
    step_index: stepIndex,
    ...(capture ? { capture: structuredClone(capture) } : {}),
  };
}

function closureUse(
  role: string,
  issuer: string,
  query: string,
  branch: JsonObject,
  afterStepIndex: number,
): JsonObject {
  const step = branchStep(branch, afterStepIndex);
  return {
    use_id: nextHash(),
    role,
    semantic_proof_id: nextHash(),
    issuer,
    issuer_evidence_id: 'schema-fixture',
    issuer_occurrence_id: nextHash(),
    query,
    branch_id: branch.branch_id,
    branch_role: branch.branch_role,
    after_step_index: afterStepIndex,
    position: { fen: step.fen_after, ply: step.ply, scope: 'legal_line' },
  };
}

function proofBase(): JsonObject {
  return {
    source_evidence_id: 'schema-fixture',
    semantic_id: nextHash(),
    occurrence_id: nextHash(),
    dependency_fingerprint: nextHash(),
  };
}

function schemaOccurrence(proofKind: MoveReviewProofKind, multiplePaths = false): JsonObject {
  const source = producedOccurrence();
  if (proofKind === 'capture_exclusion_move_order') return structuredClone(source);
  if (proofKind === 'relocation_enables_recapture') return structuredClone(producedRelocationOccurrence());

  const captureProof = object(source.proof);
  const counterfactualSource = object(captureProof.vacating_branch);
  const observedSource = object(captureProof.immediate_capture_branch);
  const captureParticipants = object(captureProof.participants);
  const vacatingMove = object(captureParticipants.vacating_move);
  const deferredMove = object(captureParticipants.deferred_move);
  const captureReply = object(captureParticipants.capture_reply);
  const capturedTarget = object(captureParticipants.captured_target);
  const subjectOccurrence = structuredClone(object(source.subject_occurrence));
  const outer = (proof: JsonObject): JsonObject => ({
    cause_evidence_id: 'schema-fixture:' + proofKind,
    subject_occurrence: subjectOccurrence,
    proof_kind: proofKind,
    proof,
  });

  if (proofKind === 'unique_check_reply_defender_displacement_before_capture') {
    const displacement = copyBranch(counterfactualSource, 'displacement_then_capture', 3);
    const immediate = copyBranch(observedSource, 'immediate_capture_with_defender', 2);
    const path = {
      path_occurrence_id: nextHash(),
      premises: [
        relationPremise('displacement_check_response', 'created_check_response_inventory', displacement, 0),
        relationPremise('delayed_capture_recapture', 'capture_recapture_inventory', displacement, 2),
        relationPremise('immediate_capture_recapture', 'capture_recapture_inventory', immediate, 0),
      ],
      closed_absence_uses: [
        closureUse(
          'delayed_capture_recapture_absent',
          'position_relation_extractor.closed_relation_inventory',
          'legal-capture:black:d4',
          displacement,
          2,
        ),
      ],
      closed_state_uses: Array.from({ length: 4 }, (_, index) =>
        closureUse(
          index % 2 ? 'delayed_target_present' : 'delayed_capture_actor_present',
          'position_relation_extractor.closed_position_state_inventory',
          index % 2 ? 'occupied-by:white:pawn@d4' : 'occupied-by:black:pawn@e5',
          displacement,
          index % 3,
        ),
      ),
    };
    const legalReply = { ...captureReply, move_uci: branchStep(immediate, 1).move_uci };
    return outer({
      ...proofBase(),
      displacement_branch: displacement,
      immediate_capture_branch: immediate,
      proof_paths: [path],
      participants: {
        trigger: vacatingMove,
        forced_reply: legalReply,
        realizer: deferredMove,
        captured_target: capturedTarget,
        immediate_defense: legalReply,
        disabled_defender: capturedTarget,
      },
      realizing_move: branchStep(displacement, 2).move_uci,
      immediate_capture_branch_legal_defense_move: branchStep(immediate, 1).move_uci,
    });
  }

  if (proofKind === 'sole_recapturer_removal_before_target_capture') {
    const removal = copyBranch(counterfactualSource, 'removal_then_target_capture', 3);
    const immediate = copyBranch(observedSource, 'immediate_target_capture', 2);
    const path = {
      path_occurrence_id: nextHash(),
      premises: [
        relationPremise('defender_removal', 'capture_recapture_inventory', removal, 0),
        relationPremise('post_removal_target_capture_inventory', 'capture_recapture_inventory', removal, 2),
        relationPremise('immediate_target_capture_inventory', 'capture_recapture_inventory', immediate, 0),
      ],
      closed_absence_uses: [
        closureUse(
          'post_removal_replacement_recapture_absent',
          'position_relation_extractor.closed_relation_inventory',
          'legal-capture:black:d4',
          removal,
          2,
        ),
      ],
      closed_state_uses: Array.from({ length: 4 }, (_, index) =>
        closureUse(
          index % 2 ? 'post_removal_target_present' : 'post_removal_exploit_actor_present',
          'position_relation_extractor.closed_position_state_inventory',
          index % 2 ? 'occupied-by:white:pawn@d4' : 'occupied-by:white:pawn@b2',
          removal,
          index % 3,
        ),
      ),
    };
    const legalReply = { ...captureReply, move_uci: branchStep(immediate, 1).move_uci };
    return outer({
      ...proofBase(),
      removal_branch: removal,
      immediate_capture_branch: immediate,
      proof_paths: [path],
      participants: {
        remover: vacatingMove,
        removed_defender: capturedTarget,
        removal_recapture: legalReply,
        post_removal_target_capture: deferredMove,
        captured_target: capturedTarget,
        immediate_sole_recapture: legalReply,
      },
      post_removal_target_capture_move: branchStep(removal, 2).move_uci,
      immediate_sole_recapture_move: branchStep(immediate, 1).move_uci,
    });
  }

  if (proofKind === 'vacated_gate_enables_unrecapturable_slider_capture') {
    const vacated = copyBranch(counterfactualSource, 'gate_vacated_then_capture');
    const retained = copyBranch(observedSource, 'gate_retained');
    const absenceSpecs = [
      ['later_capture_immediate_recapture_absent', vacated],
      ['retained_gate_exploit_move_absent', retained],
      ['retained_gate_replacement_capture_absent', retained],
    ] as const;
    const stateRoles = [
      'vacated_gate_intervening_slider_reach',
      'vacated_gate_target_persistence',
      'vacated_gate_target_persistence',
      'retained_gate_slider_persistence',
      'retained_gate_target_persistence',
      'retained_gate_blocker_persistence',
      'retained_gate_slider_persistence',
      'retained_gate_target_persistence',
      'retained_gate_blocker_persistence',
      'retained_gate_blocked_slider_reach',
    ];
    const path = {
      path_occurrence_id: nextHash(),
      premises: [
        relationPremise('gate_vacating_slider_reach', 'slider_reach_delta', vacated, 0),
        relationPremise('later_slider_capture', 'capture_recapture_inventory', vacated, 2),
      ],
      closed_absence_uses: absenceSpecs.map(([role, branch]) =>
        closureUse(
          role,
          'position_relation_extractor.closed_relation_inventory',
          'legal-capture:black:d4',
          branch,
          branch === vacated ? 2 : 1,
        ),
      ),
      closed_state_uses: stateRoles.map((role, index) => {
        const branch = index < 3 ? vacated : retained;
        return closureUse(
          role,
          'position_relation_extractor.closed_position_state_inventory',
          'occupied-by:white:pawn@b2',
          branch,
          Math.min(index % 3, objects(branch.steps).length - 1),
        );
      }),
    };
    return outer({
      ...proofBase(),
      vacated_gate_branch: vacated,
      retained_gate_branch: retained,
      proof_paths: [path],
      participants: {
        enabler: vacatingMove,
        slider: capturedTarget,
        gate_blocker: capturedTarget,
        exploit: deferredMove,
        captured_target: capturedTarget,
      },
      exploit_move: branchStep(vacated, 2).move_uci,
    });
  }

  if (proofKind === 'square_release_route') {
    const released = copyBranch(counterfactualSource, 'released_square_route');
    const retained = copyBranch(observedSource, 'retained_blocker');
    const path = (pathIndex: number) => ({
      path_occurrence_id: nextHash(),
      premises: [
        legalPremise('release_move', released, 0, vacatingMove, 'pawn_advance'),
        legalPremise('route_move_0', released, 2, deferredMove, 'controlled_destination'),
      ],
      closed_absence_uses: [
        closureUse(
          'retained_blocker_first_route_leg_absent',
          'position_relation_extractor.closed_relation_inventory',
          'legal-move-from-to:white:b2:b4',
          retained,
          1,
        ),
      ],
      closed_state_uses: [
        'released_square_vacancy',
        'route_piece_' + pathIndex,
        'route_persistence_' + pathIndex,
        'retained_blocker_persistence',
        'retained_route_origin_persistence',
        'retained_blocker_persistence',
        'retained_route_origin_persistence',
      ].map((role, index) => {
        const branch = index < 3 ? released : retained;
        return closureUse(
          role,
          'position_relation_extractor.closed_position_state_inventory',
          role === 'released_square_vacancy' ? 'vacant:d4' : 'occupied-by:white:pawn@b2',
          branch,
          Math.min(index % 3, objects(branch.steps).length - 1),
        );
      }),
    });
    return outer({
      ...proofBase(),
      released_route_branch: released,
      retained_blocker_branch: retained,
      proof_paths: multiplePaths ? [path(0), path(1)] : [path(0)],
      participants: {
        releaser: vacatingMove,
        released_blocker: capturedTarget,
        route_piece: capturedTarget,
      },
      route: [
        {
          ...deferredMove,
          move_uci: branchStep(released, 2).move_uci,
          step_index: 2,
        },
      ],
      terminal_step_index: 2,
      terminal: { kind: 'occupation' },
    });
  }

  const branchId = nextHash();
  const line = {
    line_id: subjectOccurrence.line_id,
    root_move: subjectOccurrence.move_uci,
  };
  const sourceSteps = objects(observedSource.steps);
  const steps = sourceSteps.map((step, index) => ({
    ...structuredClone(step),
    step_key: 'step:' + index,
    line,
  }));
  const branch = {
    branch_id: branchId,
    role: 'observed_root_with_analyzed_continuation',
    reply_move: sourceSteps[1]!.move_uci,
    source_occurrence_id: nextHash(),
    line,
    line_owner_evidence_id: subjectOccurrence.line_owner_evidence_id,
    root_transition_evidence_id: subjectOccurrence.transition_evidence_id,
    root_provenance: 'observed_game_root',
    steps,
  };
  const actor = (movement: JsonObject) => ({
    side: movement.side,
    piece_before: movement.piece_before,
    piece_after: movement.piece_after,
    from: movement.from,
    to: movement.to,
    legal_move_relation: nextHash(),
  });
  const path = () => ({
    path_occurrence_id: nextHash(),
    analysis_continuation_branch_id: branchId,
    realization_actor: actor(captureReply),
    realization_move: sourceSteps[1]!.move_uci,
    realization_ply: sourceSteps[1]!.ply,
    premises: [
      {
        role: 'dependency',
        lower_kind: 'passed_pawn_progress_dependency',
        lower_semantic_key: 'dependency',
        source_premise_ids: ['dependency'],
        branch_id: branchId,
        branch_role: 'observed_root_with_analyzed_continuation',
        from_step_index: 0,
        to_step_index: 1,
        dependency_proof: {
          dependency_kind: 'object_state_precondition',
          proof_kind: 'object_state',
          squares: [{ role: 'root_from', square: deferredMove.from }],
          pieces: [
            {
              role: 'root_before',
              side: deferredMove.side,
              piece: deferredMove.piece_before,
            },
          ],
          relation_issuers: [],
          position_state_issuers: [],
        },
      },
      {
        role: 'result',
        lower_kind: 'passed_pawn_progress',
        lower_semantic_key: 'result',
        source_premise_ids: ['result'],
        branch_id: branchId,
        branch_role: 'observed_root_with_analyzed_continuation',
        from_step_index: 1,
        to_step_index: 1,
      },
    ],
    closure_use_ids: [nextHash()],
  });
  return outer({
    ...proofBase(),
    event_evidence_id: 'schema-fixture:event',
    result_target_subjects: ['schema-fixture:target'],
    root_actor: actor(deferredMove),
    realizing_actor: actor(captureReply),
    root_line: line,
    root_move: subjectOccurrence.move_uci,
    root_ply: subjectOccurrence.destination.ply,
    realizing_move: sourceSteps[1]!.move_uci,
    realizing_ply: sourceSteps[1]!.ply,
    result_ply_offset: 1,
    closed_legal_reply_inventory: {
      issuer_evidence_id: 'schema-fixture:reply',
      root_after: {
        fen: subjectOccurrence.destination.fen,
        ply: subjectOccurrence.destination.ply,
        scope: 'played_transition',
      },
      legal_reply_move: sourceSteps[1]!.move_uci,
      analysis_continuation_branch_id: branchId,
    },
    branches: [branch],
    proof_paths: multiplePaths ? [path(), path()] : [path()],
    lower_premise_ids: ['dependency', 'result'],
  });
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
  assert.deepEqual(played.review.explanations, []);
  assert.equal(played.review.comparisonProof?.moves[0]?.uci, 'e7e5');
  assert.equal(
    moveReviewProofById(played.review, played.review.comparisonProof!.id),
    played.review.comparisonProof,
  );
});

test('preserves BestChoice and its transmitted runner-up comparison without synthesizing improves', () => {
  const raw = JSON.parse(
    readFileSync(
      new URL(
        '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/best-choice-produced.json',
        import.meta.url,
      ),
      'utf8',
    ),
  ) as JsonObject;
  const producedAfter = object(raw.focus).resulting_fen as FEN;
  const decoded = decodeMoveReviewSnapshot(raw, {
    requestId: 'request-best-choice',
    subject: typedSubject(initialFen, 'e2e4', producedAfter),
    engineProfile: moveReviewEngineProfile,
  });
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (played?.review.kind !== 'move-verdict') return;
  const core = played.review.core;
  assert.equal(core.kind, 'best-choice');
  if (core.kind !== 'best-choice') return;
  assert.equal(core.verdictSymbol, 'none');
  assert.equal(core.verdictCode, 'playable_loss');
  assert.deepEqual(core.bestChoice, {
    runnerUpVerdictCode: 'playable_loss',
    runnerUpUci: 'd2d4',
  });
  assert.equal(core.bestUci, 'e2e4');

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

test('decodes the immutable Scala-produced occurrence fixture end to end', () => {
  const raw = producedResponse();
  const decoded = decodeProduced(raw);
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (played?.review.kind !== 'move-verdict') return;

  assert.equal(played.review.explanations.length, 1);
  const explanation = played.review.explanations[0]!;
  assert.equal(explanation.proofKind, 'capture_exclusion_move_order');
  assert.equal(explanation.id, `occurrence-explanation-${explanation.proof.occurrenceId}`);
  assert.equal(explanation.subjectOccurrence.moveUci, 'b2b4');
  assert.equal(explanation.subjectOccurrence.rootProvenance, 'observed_game_root');

  const branches = moveReviewOccurrenceBranches(explanation);
  assert.deepEqual(
    branches.map(branch => branch.rootProvenance),
    ['counterfactual_analyzed_root', 'observed_game_root'],
  );
  assert.deepEqual(
    branches.map(branch => branch.steps.map(step => step.provenance)),
    [Array(5).fill('certified_analysis_move'), ['observed_game_move', 'certified_analysis_move']],
  );

  const paths = moveReviewOccurrenceProofPaths(explanation);
  assert.equal(paths.length, 1);
  assert.equal(paths[0]?.pathOccurrenceId, explanation.proof.proofPaths[0]?.pathOccurrenceId);
  if (explanation.proofKind !== 'capture_exclusion_move_order') return;
  assert.deepEqual(
    explanation.proof.proofPaths[0].premises.map(premise => premise.role),
    ['vacating_move', 'immediate_deferred_move', 'immediate_capture_reply', 'later_deferred_move'],
  );
  assert.equal(explanation.proof.proofPaths[0].closedAbsenceUses.length, 2);
  assert.equal(explanation.proof.proofPaths[0].closedStateUses.length, 14);
  assert.equal(explanation.proof.participants.capturedTarget.square, 'd4');
  const branchProof = moveReviewOccurrenceBranchProof(explanation, 0)!;
  assert.equal(moveReviewProofById(played.review, branchProof.id)?.id, branchProof.id);
});

test('decodes the immutable Scala-produced relocation fixture with complete object continuity', () => {
  const raw = producedRelocationResponse();
  const decoded = decodeProduced(raw);
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (played?.review.kind !== 'move-verdict') return;
  const explanation = played.review.explanations[0]!;
  assert.equal(explanation.proofKind, 'relocation_enables_recapture');
  assert.equal(explanation.subjectOccurrence.moveUci, 'd7d5');
  if (explanation.proofKind !== 'relocation_enables_recapture') return;

  const branches = moveReviewOccurrenceBranches(explanation);
  assert.deepEqual(
    branches.map(branch => branch.steps.map(step => step.provenance)),
    [
      Array(5).fill('certified_analysis_move'),
      ['observed_game_move', ...Array(2).fill('certified_analysis_move')],
    ],
  );
  const path = explanation.proof.proofPaths[0];
  const continuity = path.premises.filter(premise => 'transitionKind' in premise);
  assert.ok(continuity.length > 9);
  assert.ok(continuity.some(premise => premise.transitionKind === 'secondary'));
  assert.deepEqual(
    path.premises
      .filter(premise => premise.contract === 'capture_recapture_inventory')
      .map(premise => premise.role),
    ['relocated_recapture_inventory', 'retained_recapture_inventory'],
  );
  assert.deepEqual(
    path.premises.filter(premise => premise.contract === 'legal_move').map(premise => premise.role),
    [
      'relocated_target_capture',
      'relocated_responder_recapture',
      'retained_target_capture',
      'retained_other_recapture',
    ],
  );
  const liveRoles = new Map(
    continuity.map(premise => [
      `${premise.role}:${premise.stepIndex}`,
      `${premise.before.square}-${premise.after.square}`,
    ]),
  );
  assert.equal(liveRoles.get('relocated_responder_continuity:0'), 'a8-d8');
  assert.equal(liveRoles.get('relocated_branch_target_continuity:0'), 'd7-d7');
  assert.equal(liveRoles.get('relocated_branch_target_continuity:2'), 'd7-d5');
  assert.equal(liveRoles.get('relocated_branch_attacker_continuity:1'), 'e5-e5');
  assert.equal(liveRoles.get('retained_responder_continuity:0'), 'a8-a8');
  assert.notDeepEqual(
    explanation.proof.participants.trackedResponderAtSeed,
    explanation.proof.participants.otherRecapturer,
  );
  assert.equal(
    path.closedStateUses.length,
    continuity.filter(premise => premise.transitionKind === 'retained').length,
  );
  assert.equal(explanation.proof.participants.capturedTarget.square, 'd5');
  assert.equal(explanation.proof.participants.recaptureSquare, 'd6');
  assert.notEqual(
    explanation.proof.participants.capturedTarget.square,
    explanation.proof.participants.recaptureSquare,
  );
  const branchProof = moveReviewOccurrenceBranchProof(explanation, 0)!;
  assert.deepEqual(branchProof.annotations, [
    { atPly: 1, shape: { kind: 'arrow', orig: 'a8', dest: 'd8', brush: 'blue' } },
    { atPly: 4, shape: { kind: 'arrow', orig: 'e5', dest: 'd6', brush: 'red' } },
    { atPly: 5, shape: { kind: 'arrow', orig: 'd8', dest: 'd6', brush: 'green' } },
  ]);
  assert.equal(moveReviewProofById(played.review, branchProof.id)?.id, branchProof.id);
});

test('fails closed when relocation continuity loses an occurrence, owner, transition, or endpoint', () => {
  const mutations: Array<(proof: JsonObject) => void> = [
    proof => {
      const premises = objects(object(objects(proof.proof_paths)[0]).premises);
      const index = premises.findIndex(
        premise => premise.role === 'relocated_branch_target_continuity' && premise.step_index === 2,
      );
      premises.splice(index, 1);
    },
    proof => {
      const premises = objects(object(objects(proof.proof_paths)[0]).premises);
      const source = premises.find(premise => premise.contract === 'object_continuity_step')!;
      const duplicate = structuredClone(source);
      const priorFootprint = duplicate.transition_footprint_id;
      duplicate.transition_footprint_id = hashId(999);
      duplicate.source_premise_ids = (duplicate.source_premise_ids as string[])
        .map(value =>
          value === `transition-footprint:${priorFootprint}`
            ? `transition-footprint:${duplicate.transition_footprint_id}`
            : value,
        )
        .sort();
      premises.push(duplicate);
    },
    proof => {
      const premises = objects(object(objects(proof.proof_paths)[0]).premises);
      const secondary = premises.find(premise => premise.transition_kind === 'secondary')!;
      delete secondary.selected_transition;
    },
    proof => {
      const premises = objects(object(objects(proof.proof_paths)[0]).premises);
      const targetCapture = premises.find(premise => premise.role === 'relocated_target_capture')!;
      targetCapture.capture = structuredClone(object(object(proof.participants).other_recapturer));
    },
    proof => {
      const premises = objects(object(objects(proof.proof_paths)[0]).premises);
      const recapture = premises.find(premise => premise.role === 'retained_other_recapture')!;
      object(recapture.movement).from = 'a1';
    },
    proof => {
      const premises = objects(object(objects(proof.proof_paths)[0]).premises);
      const continuity = premises.find(premise => premise.contract === 'object_continuity_step')!;
      continuity.source_premise_ids = (continuity.source_premise_ids as string[])
        .map(value => (value.startsWith('legal-move:') ? `legal-move:${hashId(997)}` : value))
        .sort();
    },
    proof => {
      const premises = objects(object(objects(proof.proof_paths)[0]).premises);
      const endpoint = premises.find(premise => premise.role === 'relocated_target_capture')!;
      endpoint.source_premise_ids = (endpoint.source_premise_ids as string[])
        .map(value => (value.startsWith('legal-move:') ? `legal-move:${hashId(996)}` : value))
        .sort();
    },
    proof => {
      const premises = objects(object(objects(proof.proof_paths)[0]).premises);
      const inventory = premises.find(premise => premise.role === 'relocated_recapture_inventory')!;
      inventory.step_index = Number(inventory.step_index) + 1;
    },
    proof => {
      const absence = objects(object(objects(proof.proof_paths)[0]).closed_absence_uses)[0]!;
      absence.query = 'legal-move-from-to:black:a1:a2';
    },
    proof => {
      const absence = objects(object(objects(proof.proof_paths)[0]).closed_absence_uses)[0]!;
      absence.after_step_index = Number(absence.after_step_index) + 1;
    },
    proof => {
      const premises = objects(object(objects(proof.proof_paths)[0]).premises);
      const recapture = premises.find(premise => premise.role === 'relocated_responder_recapture')!;
      recapture.capture = structuredClone(object(object(proof.participants).other_recapturer));
    },
    proof => {
      object(proof.participants).recapture_square = 'a1';
    },
    proof => {
      object(proof.participants).tracked_responder_at_seed = structuredClone(
        object(object(proof.participants).other_recapturer),
      );
    },
    proof => {
      const paths = objects(proof.proof_paths);
      const duplicate = structuredClone(paths[0]!);
      duplicate.path_occurrence_id = hashId(998);
      paths.push(duplicate);
    },
  ];

  for (const mutate of mutations) {
    const raw = producedRelocationResponse();
    mutate(object(producedOccurrence(raw).proof));
    assert.equal(decodeProduced(raw), undefined);
  }
});

test('decodes all seven proof kinds as their exact discriminated variants', () => {
  const proofKinds: MoveReviewProofKind[] = [
    'unique_check_reply_defender_displacement_before_capture',
    'sole_recapturer_removal_before_target_capture',
    'vacated_gate_enables_unrecapturable_slider_capture',
    'square_release_route',
    'capture_exclusion_move_order',
    'relocation_enables_recapture',
    'passed_pawn_progress_realized_after_only_legal_reply',
  ];

  for (const proofKind of proofKinds) {
    const raw =
      proofKind === 'relocation_enables_recapture'
        ? producedRelocationResponse()
        : responseWithOccurrences([schemaOccurrence(proofKind)]);
    const decoded = decodeProduced(raw);
    assert.equal(decoded?.kind, 'completed', proofKind);
    if (decoded?.kind !== 'completed') continue;
    const played = selectedMoveReviewCandidate(decoded.evidence);
    assert.equal(played?.review.kind, 'move-verdict', proofKind);
    if (played?.review.kind !== 'move-verdict') continue;
    assert.equal(played.review.explanations.length, 1, proofKind);
    assert.equal(played.review.explanations[0]?.proofKind, proofKind);
    assert.ok(moveReviewOccurrenceBranches(played.review.explanations[0]!).length >= 1);
    assert.ok(moveReviewOccurrenceProofPaths(played.review.explanations[0]!).length >= 1);
  }
});

test('preserves multiple independent occurrence explanations without semantic or line deduplication', () => {
  const first = producedOccurrence();
  const second = structuredClone(first);
  second.cause_evidence_id = 'second-independent-cause';
  const secondProof = object(second.proof);
  secondProof.occurrence_id = hashId(900);
  object(objects(secondProof.proof_paths)[0]).path_occurrence_id = hashId(901);

  const decoded = decodeProduced(responseWithOccurrences([first, second]));
  assert.equal(decoded?.kind, 'completed');
  if (decoded?.kind !== 'completed') return;
  const played = selectedMoveReviewCandidate(decoded.evidence);
  assert.equal(played?.review.kind, 'move-verdict');
  if (played?.review.kind !== 'move-verdict') return;

  assert.equal(played.review.explanations.length, 2);
  assert.deepEqual(
    played.review.explanations.map(explanation => explanation.id),
    [first, second].map(occurrence => `occurrence-explanation-${object(occurrence.proof).occurrence_id}`),
  );
  assert.deepEqual(
    played.review.explanations.map(explanation => explanation.causeEvidenceId),
    [first.cause_evidence_id, second.cause_evidence_id],
  );
  assert.equal(
    played.review.explanations[0]?.proof.semanticId,
    played.review.explanations[1]?.proof.semanticId,
  );
  assert.equal(
    moveReviewOccurrenceBranches(played.review.explanations[0]!)[0]?.lineId,
    moveReviewOccurrenceBranches(played.review.explanations[1]!)[0]?.lineId,
  );
  assert.notEqual(
    moveReviewOccurrenceProofPaths(played.review.explanations[0]!)[0]?.pathOccurrenceId,
    moveReviewOccurrenceProofPaths(played.review.explanations[1]!)[0]?.pathOccurrenceId,
  );
});

test('fails closed on cause, proof occurrence, and cross-proof path occurrence collisions', () => {
  const mutations: Array<(first: JsonObject, second: JsonObject) => void> = [
    (first, second) => {
      second.cause_evidence_id = first.cause_evidence_id;
    },
    (first, second) => {
      object(second.proof).occurrence_id = object(first.proof).occurrence_id;
    },
    (first, second) => {
      object(objects(object(second.proof).proof_paths)[0]).path_occurrence_id = object(
        objects(object(first.proof).proof_paths)[0],
      ).path_occurrence_id;
    },
  ];
  for (const mutate of mutations) {
    const first = producedOccurrence();
    const second = structuredClone(first);
    second.cause_evidence_id = 'second-independent-cause';
    object(second.proof).occurrence_id = hashId(910);
    object(objects(object(second.proof).proof_paths)[0]).path_occurrence_id = hashId(911);
    mutate(first, second);
    assert.equal(decodeProduced(responseWithOccurrences([first, second])), undefined);
  }
});

test('preserves every transmitted square-route and passed-pawn proof path in wire order', () => {
  for (const proofKind of [
    'square_release_route',
    'passed_pawn_progress_realized_after_only_legal_reply',
  ] as const) {
    const occurrence = schemaOccurrence(proofKind, true);
    const wirePathIds = objects(object(occurrence.proof).proof_paths).map(path => path.path_occurrence_id);
    const decoded = decodeProduced(responseWithOccurrences([occurrence]));
    assert.equal(decoded?.kind, 'completed', proofKind);
    if (decoded?.kind !== 'completed') continue;
    const played = selectedMoveReviewCandidate(decoded.evidence);
    assert.equal(played?.review.kind, 'move-verdict', proofKind);
    if (played?.review.kind !== 'move-verdict') continue;
    assert.deepEqual(
      moveReviewOccurrenceProofPaths(played.review.explanations[0]!).map(path => path.pathOccurrenceId),
      wirePathIds,
      proofKind,
    );
  }
});

test('fails closed on missing, unknown, extra, legacy, and uncertified occurrence fields', () => {
  const mutations: ((raw: JsonObject) => void)[] = [
    raw => {
      delete object(producedOccurrence(raw).proof).semantic_id;
    },
    raw => {
      object(producedOccurrence(raw).proof).extra_field = true;
    },
    raw => {
      producedOccurrence(raw).proof_kind = 'unknown_proof_kind';
    },
    raw => {
      delete branchStep(object(object(producedOccurrence(raw).proof).immediate_capture_branch), 0).provenance;
    },
    raw => {
      producedCommentary(raw).causal_explanations = [];
    },
    raw => {
      producedCommentary(raw).requested_explanations = [];
    },
    raw => {
      object(producedOccurrence(raw).proof).capture_exclusion_move_order_proof = object(
        producedOccurrence(raw).proof,
      );
    },
    raw => {
      producedCommentary(raw).occurrence_explanations = [];
    },
    raw => {
      const proof = object(producedOccurrence(raw).proof);
      const path = object(objects(proof.proof_paths)[0]);
      const premises = objects(path.premises);
      premises[0] = { ...premises[0], role: 'invented_role' };
    },
  ];

  for (const mutate of mutations) {
    const raw = producedResponse();
    mutate(raw);
    assert.equal(decodeProduced(raw), undefined);
  }
});
test('rejects retired presentation projections', () => {
  for (const field of ['structural_idea_units', 'responsibility_links'] as const) {
    const raw = producedResponse();
    producedCommentary(raw)[field] = [];
    assert.equal(decodeProduced(raw), undefined, field);
  }
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
