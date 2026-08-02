import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import {
  chesstoryBriefRequestIsActive,
  chesstoryBriefSections,
  decodeChesstoryMoveMeaningResponse,
  type ChesstoryBriefState,
} from '../src/chesstoryBrief';

const statusCounts = {
  selected: 0,
  dominated: 0,
  redundant: 0,
  diagnostic: 0,
  inferior: 0,
  admission_deferred: 0,
  rejected: 0,
  unproposed: 0,
  object_unready: 0,
};

const reasonCounts = {
  player_facing_selection: 0,
  dominated_fallback: 0,
  cross_comparison_redundancy: 0,
  certified_claim_deduplicated: 0,
  diagnostic_comparison: 0,
  inferior_alternative: 0,
  claim_admission_deferred: 0,
  claim_admission_rejected: 0,
  no_claim_proposal: 0,
  object_readiness_failed: 0,
};

const verdict = {
  comparison_kind: 'played_vs_best',
  mover: 'white',
  verdict_code: 'mistake',
  move_quality: 'bad',
  played_move: 'f3g5',
  reference_move: 'f3h4',
  candidate_win_percent_delta_for_mover: -18.5,
  win_percent_loss_for_mover: 18.5,
  outcome: null,
  mate: null,
};

function observations() {
  const carrierLine = line('line:played', 'f3g5', 'played', 0);
  const candidate = comparisonLine('f3g5', 'played', 0);
  const reference = comparisonLine('f3h4', 'best_reference', 0);
  const source = {
    id: 'source:strategic',
    producer: 'strategic_feature_producer',
    layer: 'strategic',
    scope: 'current_position',
    line: null,
  };
  const candidateCarrier = {
    id: 'carrier:candidate',
    producer: 'relative_move_producer',
    layer: 'candidate_comparison',
    scope: 'counterfactual',
    line: carrierLine,
  };
  return [
    {
      claim_id: 'claim:candidate',
      carrier: candidateCarrier,
      parents: [source],
      kind: 'candidate_comparison',
      comparison: {
        kind: 'played_vs_best',
        mover: 'white',
        reference,
        candidate,
        candidate_win_percent_delta_for_mover: -18.5,
        verdict: 'mistake',
        candidate_set_type: 'narrow_choice',
        recapture_resource: {
          target: 'e5',
          removed_occupant_role: 'pawn',
          reference_defender_role: 'rook',
          preserved_defender_role: 'queen',
          reference_root_move: 'f3h4',
          opponent_capture_move: 'e6e5',
          reference_recapture_move: 'f3e5',
        },
      },
    },
    {
      claim_id: 'claim:strategic',
      carrier: {
        id: 'carrier:strategic',
        producer: 'strategic_mechanism_producer',
        layer: 'strategic_mechanism',
        scope: 'current_position',
        line: carrierLine,
      },
      parents: [source, candidateCarrier],
      kind: 'strategic_mechanism',
      mechanism: {
        kind: 'target_pressure',
        signals: [
          {
            kind: 'strategic_fact',
            key: 'target:e5',
            source,
            strength: 2,
            axis: { kind: 'target', polarity: 'gain', key: 'target:e5' },
          },
        ],
        semantic_keys: [{ kind: 'strategic_kind', values: ['target_pressure'] }],
      },
    },
  ];
}

function line(id: string, rootMove: string, role: string, rank: number) {
  return { id, root_move: rootMove, role, rank };
}

function comparisonLine(rootMove: string, role: string, rank: number) {
  return { root_move: rootMove, role, rank };
}

function channel(
  signature: string,
  actor: { move: string; side: string; piece: string; from: string; to: string },
  target: { kind: string; key: string },
  mechanism: { kind: string; key: string },
  consequence: { kind: string; key: string },
  proof: string[] | null,
) {
  const eventLine = line(`line-${signature}`, actor.move, 'played', 0);
  return {
    causal_signature: signature,
    direct_change: 'occurred',
    played_change: signature === 'channel-a' ? 'lost' : 'refuted',
    carrier: {
      id: `carrier-${signature}`,
      producer: 'legal_line_producer',
      layer: 'line',
      scope: 'played_transition',
      line: eventLine,
    },
    provenance: [],
    actor,
    targets: [target],
    mechanisms: [mechanism],
    consequences: [consequence],
    witnesses: [],
    line: eventLine,
    horizon: null,
    proof_segment:
      proof === null
        ? null
        : {
            terminal_relation: 'realizes_plan_result',
            steps: proof.map((move, ply_offset) => ({
              ply_offset,
              move_uci: move,
              role:
                ply_offset === 0
                  ? 'root_action'
                  : ply_offset === proof.length - 1
                    ? 'terminal_event'
                    : 'causal_link',
            })),
          },
    effect_descriptor: {
      effect_scope: {
        primitive_kind: 'plan_result',
        target_signatures: [`${target.kind}:${target.key}`],
        plan_ids: [],
        plan_result_semantic_key: `plan-result:${signature}`,
        strategic_axes: [],
      },
      magnitude_status: 'not_applicable',
      measure: null,
      material_event_salience: null,
    },
    importance_effect: null,
    descriptor_ambiguous: false,
    proof_segment_ambiguous: false,
  };
}

function facet(id: string, ideaUnitId: string, kind: string, selectionOrder: number, channels: any[]) {
  const reference = line(`reference-${id}`, 'f3h4', 'best_reference', 0);
  const candidate = line(`candidate-${id}`, 'f3g5', 'played', 0);
  return {
    cause_evidence_id: id,
    item_role: 'cause_facet',
    idea_unit_id: ideaUnitId,
    comparison_exposure_rank: 0,
    selection_order: selectionOrder,
    host: { claim_id: `claim-${id}`, family: 'evaluation', tier: 'primary' },
    cause: {
      kind,
      effect_mode: kind === 'defensive_resource' ? 'alternative_resource' : 'played_liability',
      exposure: 'primary',
      source_side: kind === 'defensive_resource' ? 'reference' : 'candidate',
      direct_effect_admission: {
        status: 'restricted',
        causal_signatures: channels.map(item => item.causal_signature),
      },
      attribution: {
        kind: kind === 'defensive_resource' ? 'reference_creates_resource' : 'candidate_allows_liability',
        root_move_matched: true,
        direct_proof_eligible: true,
      },
    },
    comparison: {
      evidence_id: `comparison-${id}`,
      kind: 'played_vs_best',
      reference,
      candidate,
      event: candidate,
      compared_move: 'f3h4',
      verdict: 'mistake',
      mover: 'white',
      delta: {
        candidate_win_percent_delta_for_mover: -18.5,
        win_percent_loss_for_mover: 18.5,
      },
    },
    channels,
    only_move: null,
  };
}

function importance(selectedIds: string[], uniqueTop: string | null) {
  return {
    authority: 'root_owned_effect_partial_order',
    relation_policy_version: 'chesstory.direct-cause-importance.relation.v3',
    ordering_policy_version: 'chesstory.player-facing-idea-ordering.dominance-layers.v1',
    comparison_exposure_rank_meaning: 'comparison_exposure_authority',
    selected_cause_ids: selectedIds,
    frontier_cause_ids: selectedIds,
    dominated_within_domain_cause_ids: [],
    unique_top: uniqueTop,
    profile_summary: {
      measured_channels: 0,
      measured_causes: 0,
      fully_measured_causes: 0,
      unmeasured_causes: selectedIds.length,
    },
    profiles: [],
    relations: [],
    decisions: [],
    relation_summary: { comparable_profile_pairs: 0, incomparable_profile_pairs: 0 },
    unmeasured: [],
  };
}

function certifiedEnvelope(facets: any[], units: any[], uniqueTop: string | null, observations?: unknown[]) {
  const selectedIds = facets.map(item => item.cause_evidence_id);
  const leadIds = units.map(unit => unit.lead_cause_evidence_id);
  return {
    schema_version: 'chesstory.move-meaning.response.v3',
    request_id: 'ui-contract-test',
    ok: true,
    status: 'ready',
    availability: { state: 'ready', reason: null },
    probe_requests: [],
    move_review: {
      renderable: true,
      verdict,
      idea_status: 'certified',
      idea_status_detail: {
        authority: 'cause_disposition_ledger.v1',
        total_cause_count: selectedIds.length,
        selected_cause_ids: selectedIds,
        status_counts: { ...statusCounts, selected: selectedIds.length },
        reason_counts: { ...reasonCounts, player_facing_selection: selectedIds.length },
        abstention_codes: [],
      },
      ...(observations === undefined ? {} : { observations }),
      explanations: [
        {
          role: 'played move',
          move: 'f3g5',
          reference_move: 'f3h4',
          move_quality: 'bad',
          ideas: facets,
          idea_units: units,
          idea_importance: importance(leadIds, uniqueTop),
          importance: importance(selectedIds, uniqueTop),
        },
      ],
    },
  };
}

function verdictOnlyEnvelope(requestId = 'verdict-only') {
  return {
    schema_version: 'chesstory.move-meaning.response.v3',
    request_id: requestId,
    ok: true,
    status: 'ready',
    availability: { state: 'ready', reason: null },
    probe_requests: [],
    move_review: {
      renderable: true,
      verdict,
      idea_status: 'no_certified_differential_idea',
      idea_status_detail: {
        authority: 'cause_disposition_ledger.v1',
        total_cause_count: 0,
        selected_cause_ids: [],
        status_counts: statusCounts,
        reason_counts: reasonCounts,
        abstention_codes: ['no_relative_cause_generated'],
      },
      explanations: [],
      observations: [],
    },
  };
}

function decodeSuccess(raw: unknown) {
  const decoded = decodeChesstoryMoveMeaningResponse(raw);
  assert.ok(decoded?.ok);
  return decoded;
}

function readyState(payload: any): ChesstoryBriefState {
  return {
    kind: payload.idea_status === 'certified' ? 'ready-certified' : 'ready-verdict-only',
    key: 'test',
    payload,
  };
}

describe('active V3 Chesstory brief contract', () => {
  test('keeps in-flight reviews pending, preserves actionable withheld probes, and exposes unavailable states', () => {
    assert.ok(chesstoryBriefSections({ kind: 'requesting', key: 'test' }).every(section => section.pending));
    assert.ok(chesstoryBriefSections({ kind: 'probing', key: 'test' }).every(section => section.pending));
    assert.equal(chesstoryBriefSections({ kind: 'no-input' })[2].title, 'Choose a move');
    const withheld = {
      schema_version: 'chesstory.move-meaning.response.v3',
      request_id: null,
      ok: true,
      status: 'withheld',
      availability: { state: 'withheld', reason: 'insufficient_engine_depth' },
      probe_requests: [{ id: 'still-actionable' }],
      move_review: {
        renderable: false,
        verdict: null,
        idea_status: 'unavailable',
        idea_status_detail: {
          authority: 'cause_disposition_ledger.v1',
          total_cause_count: 0,
          selected_cause_ids: [],
          status_counts: statusCounts,
          reason_counts: reasonCounts,
          abstention_codes: ['no_relative_cause_generated'],
        },
        explanations: [],
        observations: [],
      },
    };
    const decoded = decodeSuccess(withheld);
    const withheldSections = chesstoryBriefSections({ kind: 'withheld', key: 'test' });
    assert.ok(withheldSections.every(section => !section.pending));
    assert.equal(withheldSections[2].title, 'Review withheld');
    assert.equal(chesstoryBriefSections({ kind: 'fault', key: 'test' })[2].title, 'Review unavailable');
    assert.equal(decoded.status, 'withheld');
    assert.equal(decoded.probe_requests.length, 1);
  });

  test('preserves a ready verdict when no differential Cause is certified', () => {
    const decoded = decodeSuccess(verdictOnlyEnvelope());
    const sections = chesstoryBriefSections(readyState(decoded.move_review));
    assert.ok(sections.every(section => !section.pending));
    assert.match(JSON.stringify(sections), /f3-g5 is classified as mistake/);
    assert.match(JSON.stringify(sections), /f3-h4 is the verified reference move/);
  });

  test('renders every channel as one indivisible actor-target-mechanism-consequence tuple', () => {
    const channels = [
      channel(
        'channel-a',
        { move: 'f3g5', side: 'white', piece: 'knight', from: 'f3', to: 'g5' },
        { kind: 'piece', key: 'black king' },
        { kind: 'relation', key: 'fork access' },
        { kind: 'consequence', key: 'black queen loss' },
        ['f3g5', 'g8h8'],
      ),
      channel(
        'channel-b',
        { move: 'f7f6', side: 'black', piece: 'pawn', from: 'f7', to: 'f6' },
        { kind: 'piece', key: 'white knight' },
        { kind: 'relation', key: 'pawn attack' },
        { kind: 'square', key: 'e6 retreat' },
        null,
      ),
    ];
    const idea = facet('cause-a', 'unit-a', 'candidate_tactical_liability', 0, channels);
    const unit = {
      idea_id: 'unit-a',
      kind: 'single_cause',
      lead_cause_evidence_id: 'cause-a',
      member_cause_evidence_ids: ['cause-a'],
      importance_layer: 0,
      priority_status: 'unmeasured',
      serialization_order: 0,
    };
    const decoded = decodeSuccess(certifiedEnvelope([idea], [unit], null));
    const channelSection = chesstoryBriefSections(readyState(decoded.move_review)).find(
      section => section.title === 'How each certified channel works',
    );
    assert.equal(channelSection?.items?.length, 2);
    const [first, second] = channelSection?.items || [];
    assert.match(first, /white knight f3-g5/);
    assert.match(first, /black king/);
    assert.match(first, /fork access/);
    assert.match(first, /black queen loss/);
    assert.doesNotMatch(first, /black pawn|piece white knight|pawn attack|e6 retreat/);
    assert.match(second, /black pawn f7-f6/);
    assert.match(second, /white knight/);
    assert.match(second, /pawn attack/);
    assert.match(second, /e6 retreat/);
    assert.doesNotMatch(second, /white knight f3-g5|black king|fork access|black queen loss/);
  });

  test('uses unit serialization order and never promotes the first item when unique_top is null', () => {
    const a = facet('cause-a', 'unit-a', 'candidate_tactical_liability', 1, [
      channel(
        'a-only',
        { move: 'f3g5', side: 'white', piece: 'knight', from: 'f3', to: 'g5' },
        { kind: 'square', key: 'g5' },
        { kind: 'relation', key: 'overextension' },
        { kind: 'consequence', key: 'lost tempo' },
        null,
      ),
    ]);
    const b = facet('cause-b', 'unit-b', 'defensive_resource', 0, [
      channel(
        'b-only',
        { move: 'f3h4', side: 'white', piece: 'knight', from: 'f3', to: 'h4' },
        { kind: 'square', key: 'h4' },
        { kind: 'relation', key: 'preserves route' },
        { kind: 'consequence', key: 'defensive resource' },
        null,
      ),
    ]);
    const units = [
      {
        idea_id: 'unit-a',
        kind: 'single_cause',
        lead_cause_evidence_id: 'cause-a',
        member_cause_evidence_ids: ['cause-a'],
        importance_layer: 0,
        priority_status: 'frontier',
        serialization_order: 1,
      },
      {
        idea_id: 'unit-b',
        kind: 'single_cause',
        lead_cause_evidence_id: 'cause-b',
        member_cause_evidence_ids: ['cause-b'],
        importance_layer: 0,
        priority_status: 'frontier',
        serialization_order: 0,
      },
    ];
    const decoded = decodeSuccess(certifiedEnvelope([a, b], units, null));
    const opening = chesstoryBriefSections(readyState(decoded.move_review))[0];
    assert.equal(opening.title, 'Certified differential ideas');
    assert.match(opening.body, /No single top idea was certified/);
    assert.doesNotMatch(JSON.stringify(opening), /Top —/);
    assert.match(opening.items?.[1] || '', /defensive resource/);
    assert.match(opening.items?.[2] || '', /candidate tactical liability/);
  });

  test('fails closed for representative invalid V3 schema and integrity drift', () => {
    const idea = facet('cause-a', 'unit-a', 'candidate_tactical_liability', 0, [
      channel(
        'channel-a',
        { move: 'f3g5', side: 'white', piece: 'knight', from: 'f3', to: 'g5' },
        { kind: 'square', key: 'g5' },
        { kind: 'relation', key: 'overextension' },
        { kind: 'consequence', key: 'lost tempo' },
        null,
      ),
    ]);
    const unit = {
      idea_id: 'unit-a',
      kind: 'single_cause',
      lead_cause_evidence_id: 'cause-a',
      member_cause_evidence_ids: ['cause-a'],
      importance_layer: 0,
      priority_status: 'unique_top',
      serialization_order: 0,
    };
    const valid = certifiedEnvelope([idea], [unit], 'cause-a', observations());
    const decoded = decodeSuccess(valid);
    assert.deepEqual(
      decoded.move_review.observations?.map(observation => observation.claim_id),
      ['claim:candidate', 'claim:strategic'],
    );
    const malformed = [
      () => {
        const value = structuredClone(valid) as any;
        value.schema_version = 'chesstory.move-meaning.response.v2';
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review = [value.move_review];
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review.tested_plan_limits = [];
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review.explanations[0].idea_units[0].member_cause_evidence_ids = ['cause-b'];
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review.explanations[0].idea_importance.unique_top = null;
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review.explanations[0].ideas[0].channels[0].effect_descriptor = null;
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review.explanations[0].ideas[0].name = 'borrowed semantic name';
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review.explanations[0].ideas[0].cause.direct_effect_admission.status = 'unresolved';
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review.observations[0].unexpected = true;
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review.observations[1].mechanism.signals[0].strength = '2';
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review.observations[0].carrier.producer = 'unknown_producer';
        return value;
      },
      () => {
        const value = structuredClone(valid) as any;
        value.move_review.observations[0].comparison.reference.root_move = 'not-uci';
        return value;
      },
    ];

    malformed.forEach(build => assert.equal(decodeChesstoryMoveMeaningResponse(build()), undefined));
  });

  test('recognizes the exact V3 error envelope and discards a stale response', () => {
    const error: unknown = {
      schema_version: 'chesstory.move-meaning.response.v3',
      request_id: 'failed-request',
      ok: false,
      status: 'error',
      error: 'move_review_not_buildable',
    };
    const decoded = decodeChesstoryMoveMeaningResponse(error);
    assert.ok(decoded);
    if (decoded.ok) assert.fail('expected a V3 error response');
    assert.equal(decoded.error, 'move_review_not_buildable');

    const current: ChesstoryBriefState = { kind: 'requesting', key: 'new-request' };
    const stale: ChesstoryBriefState = {
      kind: 'ready-verdict-only',
      key: 'old-request',
      payload: verdictOnlyEnvelope('old-response').move_review as any,
    };
    const shown = chesstoryBriefRequestIsActive(current, 1, 2, stale.key) ? stale : current;
    assert.equal(shown.kind, 'requesting');

    const ready = chesstoryBriefRequestIsActive(current, 2, 2, current.key)
      ? { ...stale, key: current.key }
      : current;
    assert.equal(ready.kind, 'ready-verdict-only');
  });
});
