type ChesstoryBriefSectionKey =
  | 'opening-idea'
  | 'middlegame-plan'
  | 'current-decision'
  | 'better-plan'
  | 'evidence';

export interface ChesstoryBriefSection {
  key: ChesstoryBriefSectionKey;
  title: string;
  body: string;
  pending: boolean;
  items?: string[];
  tone?: 'good' | 'bad' | 'neutral';
}

interface ChesstoryLineRef {
  id: string;
  root_move: string;
  role: string;
  rank: number;
}

interface ChesstoryEvidenceRef {
  id: string;
  producer: string;
  layer: string;
  scope: string;
  line: ChesstoryLineRef | null;
}

interface ChesstoryObjectRef {
  kind: string;
  key: string;
}

interface ChesstoryActor {
  move: string;
  side: string;
  piece: string;
  from: string;
  to: string;
}

interface ChesstoryProofSegment {
  terminal_relation: string;
  steps: Array<{ ply_offset: number; move_uci: string; role: string }>;
}

interface ChesstoryEffectScope {
  primitive_kind: string;
  target_signatures: string[];
  plan_ids: string[];
  strategic_axes: unknown[];
}

interface ChesstoryEffectDescriptor {
  effect_scope: ChesstoryEffectScope;
  magnitude_status: string;
  measure: unknown | null;
  material_event_salience: unknown | null;
}

interface ChesstoryCauseChannel {
  causal_signature: string;
  direct_change: string;
  played_change: string;
  carrier: ChesstoryEvidenceRef;
  provenance: ChesstoryEvidenceRef[];
  actor: ChesstoryActor;
  targets: ChesstoryObjectRef[];
  mechanisms: ChesstoryObjectRef[];
  consequences: ChesstoryObjectRef[];
  witnesses: ChesstoryObjectRef[];
  line: ChesstoryLineRef;
  horizon: string | null;
  proof_segment: ChesstoryProofSegment | null;
  effect_descriptor: ChesstoryEffectDescriptor;
  importance_effect: unknown | null;
  descriptor_ambiguous: boolean;
  proof_segment_ambiguous: boolean;
}

interface ChesstoryCauseFacet {
  cause_evidence_id: string;
  item_role: 'cause_facet';
  idea_unit_id: string;
  comparison_exposure_rank: number;
  selection_order: number;
  host: { claim_id: string; family: string; tier: string };
  cause: {
    kind: string;
    effect_mode: string;
    exposure: string;
    source_side: string;
    direct_effect_admission: { status: 'restricted'; causal_signatures: string[] };
    attribution: { kind: string; root_move_matched: true; direct_proof_eligible: true };
  };
  comparison: {
    evidence_id: string;
    kind: string;
    reference: ChesstoryLineRef;
    candidate: ChesstoryLineRef;
    event: ChesstoryLineRef;
    compared_move: string;
    verdict: string;
    mover: string;
    delta: {
      candidate_win_percent_delta_for_mover: number;
      win_percent_loss_for_mover: number;
    };
  };
  channels: ChesstoryCauseChannel[];
  only_move: {
    comparison_evidence_id: string;
    cause_evidence_id: string;
    reference: ChesstoryLineRef;
    relation: string;
    licenses_causal_because: boolean;
  } | null;
}

interface ChesstoryIdeaUnit {
  idea_id: string;
  kind: string;
  lead_cause_evidence_id: string;
  member_cause_evidence_ids: string[];
  importance_layer: number;
  priority_status: string;
  serialization_order: number;
}

interface ChesstoryImportanceProjection {
  authority: 'root_owned_effect_partial_order';
  relation_policy_version: 'chesstory.direct-cause-importance.relation.v3';
  ordering_policy_version: 'chesstory.player-facing-idea-ordering.dominance-layers.v1';
  comparison_exposure_rank_meaning: 'comparison_exposure_authority';
  selected_cause_ids: string[];
  frontier_cause_ids: string[];
  dominated_within_domain_cause_ids: string[];
  unique_top: string | null;
  profile_summary: {
    measured_channels: number;
    measured_causes: number;
    fully_measured_causes: number;
    unmeasured_causes: number;
  };
  profiles: unknown[];
  relations: unknown[];
  decisions: unknown[];
  relation_summary: { comparable_profile_pairs: number; incomparable_profile_pairs: number };
  unmeasured: unknown[];
}

interface ChesstoryExplanation {
  role: 'played move';
  move: string;
  reference_move: string;
  move_quality: string;
  ideas: ChesstoryCauseFacet[];
  idea_units: ChesstoryIdeaUnit[];
  idea_importance: ChesstoryImportanceProjection;
  importance: ChesstoryImportanceProjection;
}

interface ChesstoryVerdict {
  comparison_kind: string;
  mover: string;
  verdict_code: string;
  move_quality: string;
  played_move: string;
  reference_move: string;
  candidate_win_percent_delta_for_mover: number;
  win_percent_loss_for_mover: number;
  outcome: {
    state: string;
    certainty: string;
    reference_state: string | null;
    resistance: string | null;
  } | null;
  mate: {
    reference_for_mover: number | null;
    played_for_mover: number | null;
    distance_loss: number | null;
  } | null;
}

interface ChesstoryCauseDispositionSummary {
  authority: 'cause_disposition_ledger.v1';
  total_cause_count: number;
  selected_cause_ids: string[];
  status_counts: Record<string, number>;
  reason_counts: Record<string, number>;
  abstention_codes: string[];
}

export interface ChesstoryMoveMeaningPayload {
  renderable: boolean;
  verdict: ChesstoryVerdict | null;
  idea_status: 'certified' | 'no_certified_differential_idea' | 'unavailable';
  idea_status_detail: ChesstoryCauseDispositionSummary;
  explanations: ChesstoryExplanation[];
}

interface ChesstoryMoveMeaningSuccess {
  schema_version: 'chesstory.move-meaning.response.v3';
  request_id: string | null;
  ok: true;
  status: 'ready' | 'withheld';
  availability: {
    state: 'ready' | 'withheld';
    reason: 'insufficient_engine_depth' | null;
  };
  probe_requests: unknown[];
  move_review: ChesstoryMoveMeaningPayload;
}

interface ChesstoryMoveMeaningError {
  schema_version: 'chesstory.move-meaning.response.v3';
  request_id: string | null;
  ok: false;
  status: 'error';
  error: string;
}

export type ChesstoryMoveMeaningResponse = ChesstoryMoveMeaningSuccess | ChesstoryMoveMeaningError;

export function decodeChesstoryMoveMeaningResponse(raw: unknown): ChesstoryMoveMeaningResponse | undefined {
  if (!isObject(raw) || raw.schema_version !== 'chesstory.move-meaning.response.v3') return;
  if (raw.ok === false) {
    if (
      !hasExactKeys(raw, ['schema_version', 'request_id', 'ok', 'status', 'error']) ||
      raw.status !== 'error' ||
      !isNullableString(raw.request_id) ||
      !isNonEmptyString(raw.error)
    )
      return;
    return raw as unknown as ChesstoryMoveMeaningError;
  }
  if (
    raw.ok !== true ||
    !hasExactKeys(raw, [
      'schema_version',
      'request_id',
      'ok',
      'status',
      'availability',
      'probe_requests',
      'move_review',
    ]) ||
    !isNullableString(raw.request_id) ||
    !isAvailability(raw.availability) ||
    !Array.isArray(raw.probe_requests) ||
    !isMoveMeaningPayload(raw.move_review)
  )
    return;
  if (raw.status === 'ready') {
    if (raw.availability.state !== 'ready' || raw.availability.reason !== null || !raw.move_review.renderable)
      return;
  } else if (raw.status === 'withheld') {
    if (
      raw.availability.state !== 'withheld' ||
      raw.availability.reason !== 'insufficient_engine_depth' ||
      raw.move_review.renderable
    )
      return;
  } else return;
  return raw as unknown as ChesstoryMoveMeaningSuccess;
}

function isMoveMeaningPayload(value: unknown): value is ChesstoryMoveMeaningPayload {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['renderable', 'verdict', 'idea_status', 'idea_status_detail', 'explanations']) ||
    typeof value.renderable !== 'boolean' ||
    !(value.verdict === null || isVerdict(value.verdict)) ||
    !['certified', 'no_certified_differential_idea', 'unavailable'].includes(String(value.idea_status)) ||
    !isCauseDispositionSummary(value.idea_status_detail) ||
    !Array.isArray(value.explanations) ||
    !value.explanations.every(isExplanation) ||
    value.explanations.length > 1
  )
    return false;
  if (!value.renderable)
    return value.verdict === null && value.idea_status === 'unavailable' && value.explanations.length === 0;
  if (value.verdict === null || value.idea_status === 'unavailable') return false;
  if (value.idea_status === 'no_certified_differential_idea')
    return value.explanations.length === 0 && value.idea_status_detail.selected_cause_ids.length === 0;
  if (value.explanations.length !== 1 || value.idea_status_detail.selected_cause_ids.length === 0)
    return false;
  return sameStringSet(
    value.idea_status_detail.selected_cause_ids,
    value.explanations[0].ideas.map(idea => idea.cause_evidence_id),
  );
}

function isVerdict(value: unknown): value is ChesstoryVerdict {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'comparison_kind',
      'mover',
      'verdict_code',
      'move_quality',
      'played_move',
      'reference_move',
      'candidate_win_percent_delta_for_mover',
      'win_percent_loss_for_mover',
      'outcome',
      'mate',
    ]) ||
    ![
      value.comparison_kind,
      value.mover,
      value.verdict_code,
      value.move_quality,
      value.played_move,
      value.reference_move,
    ].every(isNonEmptyString) ||
    !isFiniteNumber(value.candidate_win_percent_delta_for_mover) ||
    !isFiniteNumber(value.win_percent_loss_for_mover)
  )
    return false;
  const outcome = value.outcome;
  if (
    outcome !== null &&
    (!isObject(outcome) ||
      !hasExactKeys(outcome, ['state', 'certainty', 'reference_state', 'resistance']) ||
      !isNonEmptyString(outcome.state) ||
      !isNonEmptyString(outcome.certainty) ||
      !isNullableString(outcome.reference_state) ||
      !isNullableString(outcome.resistance))
  )
    return false;
  const mate = value.mate;
  return (
    mate === null ||
    (isObject(mate) &&
      hasExactKeys(mate, ['reference_for_mover', 'played_for_mover', 'distance_loss']) &&
      isNullableNumber(mate.reference_for_mover) &&
      isNullableNumber(mate.played_for_mover) &&
      isNullableNumber(mate.distance_loss))
  );
}

function isCauseDispositionSummary(value: unknown): value is ChesstoryCauseDispositionSummary {
  return (
    isObject(value) &&
    hasExactKeys(value, [
      'authority',
      'total_cause_count',
      'selected_cause_ids',
      'status_counts',
      'reason_counts',
      'abstention_codes',
    ]) &&
    value.authority === 'cause_disposition_ledger.v1' &&
    isNonNegativeInteger(value.total_cause_count) &&
    isStringSet(value.selected_cause_ids) &&
    isObject(value.status_counts) &&
    Object.values(value.status_counts).every(isNonNegativeInteger) &&
    isObject(value.reason_counts) &&
    Object.values(value.reason_counts).every(isNonNegativeInteger) &&
    isStringSet(value.abstention_codes)
  );
}

function isExplanation(value: unknown): value is ChesstoryExplanation {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'role',
      'move',
      'reference_move',
      'move_quality',
      'ideas',
      'idea_units',
      'idea_importance',
      'importance',
    ]) ||
    value.role !== 'played move' ||
    !isNonEmptyString(value.move) ||
    !isNonEmptyString(value.reference_move) ||
    !isNonEmptyString(value.move_quality) ||
    !Array.isArray(value.ideas) ||
    value.ideas.length === 0 ||
    !value.ideas.every(isCauseFacet) ||
    !Array.isArray(value.idea_units) ||
    value.idea_units.length === 0 ||
    !value.idea_units.every(isIdeaUnit) ||
    !isImportanceProjection(value.idea_importance) ||
    !isImportanceProjection(value.importance)
  )
    return false;
  const ideas = value.ideas as ChesstoryCauseFacet[];
  const units = value.idea_units as ChesstoryIdeaUnit[];
  const causeIds = ideas.map(idea => idea.cause_evidence_id);
  const unitIds = units.map(unit => unit.idea_id);
  if (
    new Set(causeIds).size !== causeIds.length ||
    new Set(unitIds).size !== unitIds.length ||
    new Set(units.map(unit => unit.serialization_order)).size !== units.length ||
    new Set(ideas.map(idea => idea.selection_order)).size !== ideas.length ||
    !sameStringSet(value.importance.selected_cause_ids, causeIds)
  )
    return false;
  const allMembers = units.flatMap(unit => unit.member_cause_evidence_ids);
  if (!sameStringSet(allMembers, causeIds) || allMembers.length !== causeIds.length) return false;
  for (const unit of units) {
    const linked = ideas
      .filter(idea => idea.idea_unit_id === unit.idea_id)
      .map(idea => idea.cause_evidence_id);
    if (!sameStringSet(linked, unit.member_cause_evidence_ids)) return false;
  }
  const leads = units.map(unit => unit.lead_cause_evidence_id);
  if (!sameStringSet(value.idea_importance.selected_cause_ids, leads)) return false;
  const top = value.idea_importance.unique_top;
  const topUnits = units.filter(unit => unit.priority_status === 'unique_top');
  return top === null
    ? topUnits.length === 0
    : topUnits.length === 1 && topUnits[0].lead_cause_evidence_id === top;
}

function isCauseFacet(value: unknown): value is ChesstoryCauseFacet {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'cause_evidence_id',
      'item_role',
      'idea_unit_id',
      'comparison_exposure_rank',
      'selection_order',
      'host',
      'cause',
      'comparison',
      'channels',
      'only_move',
    ]) ||
    !isNonEmptyString(value.cause_evidence_id) ||
    value.item_role !== 'cause_facet' ||
    !isNonEmptyString(value.idea_unit_id) ||
    !isNonNegativeInteger(value.comparison_exposure_rank) ||
    !isNonNegativeInteger(value.selection_order) ||
    !isHost(value.host) ||
    !isCause(value.cause) ||
    !isComparison(value.comparison) ||
    !Array.isArray(value.channels) ||
    value.channels.length === 0 ||
    !value.channels.every(isCauseChannel) ||
    !(value.only_move === null || isOnlyMove(value.only_move))
  )
    return false;
  const signatures = value.channels.map(channel => channel.causal_signature);
  return new Set(signatures).size === signatures.length;
}

function isHost(value: unknown): boolean {
  return (
    isObject(value) &&
    hasExactKeys(value, ['claim_id', 'family', 'tier']) &&
    [value.claim_id, value.family, value.tier].every(isNonEmptyString)
  );
}

function isCause(value: unknown): boolean {
  return (
    isObject(value) &&
    hasExactKeys(value, [
      'kind',
      'effect_mode',
      'exposure',
      'source_side',
      'direct_effect_admission',
      'attribution',
    ]) &&
    [value.kind, value.effect_mode, value.exposure, value.source_side].every(isNonEmptyString) &&
    isObject(value.direct_effect_admission) &&
    hasExactKeys(value.direct_effect_admission, ['status', 'causal_signatures']) &&
    value.direct_effect_admission.status === 'restricted' &&
    isStringSet(value.direct_effect_admission.causal_signatures) &&
    value.direct_effect_admission.causal_signatures.length > 0 &&
    isObject(value.attribution) &&
    hasExactKeys(value.attribution, ['kind', 'root_move_matched', 'direct_proof_eligible']) &&
    isNonEmptyString(value.attribution.kind) &&
    value.attribution.root_move_matched === true &&
    value.attribution.direct_proof_eligible === true
  );
}

function isComparison(value: unknown): boolean {
  return (
    isObject(value) &&
    hasExactKeys(value, [
      'evidence_id',
      'kind',
      'reference',
      'candidate',
      'event',
      'compared_move',
      'verdict',
      'mover',
      'delta',
    ]) &&
    [value.evidence_id, value.kind, value.compared_move, value.verdict, value.mover].every(
      isNonEmptyString,
    ) &&
    isLineRef(value.reference) &&
    isLineRef(value.candidate) &&
    isLineRef(value.event) &&
    isObject(value.delta) &&
    hasExactKeys(value.delta, ['candidate_win_percent_delta_for_mover', 'win_percent_loss_for_mover']) &&
    isFiniteNumber(value.delta.candidate_win_percent_delta_for_mover) &&
    isFiniteNumber(value.delta.win_percent_loss_for_mover)
  );
}

function isCauseChannel(value: unknown): value is ChesstoryCauseChannel {
  return (
    isObject(value) &&
    hasExactKeys(value, [
      'causal_signature',
      'direct_change',
      'played_change',
      'carrier',
      'provenance',
      'actor',
      'targets',
      'mechanisms',
      'consequences',
      'witnesses',
      'line',
      'horizon',
      'proof_segment',
      'effect_descriptor',
      'importance_effect',
      'descriptor_ambiguous',
      'proof_segment_ambiguous',
    ]) &&
    [value.causal_signature, value.direct_change, value.played_change].every(isNonEmptyString) &&
    isEvidenceRef(value.carrier) &&
    Array.isArray(value.provenance) &&
    value.provenance.every(isEvidenceRef) &&
    isActor(value.actor) &&
    isObjectList(value.targets, true) &&
    isObjectList(value.mechanisms, true) &&
    isObjectList(value.consequences, true) &&
    isObjectList(value.witnesses, false) &&
    isLineRef(value.line) &&
    isNullableString(value.horizon) &&
    (value.proof_segment === null || isProofSegment(value.proof_segment)) &&
    isEffectDescriptor(value.effect_descriptor) &&
    (value.importance_effect === null || isObject(value.importance_effect)) &&
    typeof value.descriptor_ambiguous === 'boolean' &&
    typeof value.proof_segment_ambiguous === 'boolean' &&
    (!value.proof_segment_ambiguous || value.proof_segment === null)
  );
}

function isEvidenceRef(value: unknown): value is ChesstoryEvidenceRef {
  return (
    isObject(value) &&
    hasExactKeys(value, ['id', 'producer', 'layer', 'scope', 'line']) &&
    [value.id, value.producer, value.layer, value.scope].every(isNonEmptyString) &&
    (value.line === null || isLineRef(value.line))
  );
}

function isLineRef(value: unknown): value is ChesstoryLineRef {
  return (
    isObject(value) &&
    hasExactKeys(value, ['id', 'root_move', 'role', 'rank']) &&
    [value.id, value.root_move, value.role].every(isNonEmptyString) &&
    isNonNegativeInteger(value.rank)
  );
}

function isActor(value: unknown): value is ChesstoryActor {
  return (
    isObject(value) &&
    hasExactKeys(value, ['move', 'side', 'piece', 'from', 'to']) &&
    isNonEmptyString(value.move) &&
    isNonEmptyString(value.side) &&
    isNonEmptyString(value.piece) &&
    isNonEmptyString(value.from) &&
    isNonEmptyString(value.to)
  );
}

function isObjectList(value: unknown, required: boolean): value is ChesstoryObjectRef[] {
  return (
    Array.isArray(value) &&
    (!required || value.length > 0) &&
    value.every(
      item =>
        isObject(item) &&
        hasExactKeys(item, ['kind', 'key']) &&
        isNonEmptyString(item.kind) &&
        isNonEmptyString(item.key),
    )
  );
}

function isProofSegment(value: unknown): value is ChesstoryProofSegment {
  return (
    isObject(value) &&
    hasExactKeys(value, ['terminal_relation', 'steps']) &&
    isNonEmptyString(value.terminal_relation) &&
    Array.isArray(value.steps) &&
    value.steps.length > 0 &&
    value.steps.every(
      step =>
        isObject(step) &&
        hasExactKeys(step, ['ply_offset', 'move_uci', 'role']) &&
        isNonNegativeInteger(step.ply_offset) &&
        isNonEmptyString(step.move_uci) &&
        isNonEmptyString(step.role),
    )
  );
}

function isEffectScope(value: unknown): value is ChesstoryEffectScope {
  return (
    isObject(value) &&
    hasExactKeys(value, ['primitive_kind', 'target_signatures', 'plan_ids', 'strategic_axes']) &&
    isNonEmptyString(value.primitive_kind) &&
    isStringSet(value.target_signatures) &&
    isStringSet(value.plan_ids) &&
    Array.isArray(value.strategic_axes)
  );
}

function isEffectDescriptor(value: unknown): value is ChesstoryEffectDescriptor {
  return (
    isObject(value) &&
    hasExactKeys(value, ['effect_scope', 'magnitude_status', 'measure', 'material_event_salience']) &&
    isEffectScope(value.effect_scope) &&
    isNonEmptyString(value.magnitude_status) &&
    (value.measure === null || isObject(value.measure)) &&
    (value.material_event_salience === null || isObject(value.material_event_salience))
  );
}

function isOnlyMove(value: unknown): boolean {
  return (
    isObject(value) &&
    hasExactKeys(value, [
      'comparison_evidence_id',
      'cause_evidence_id',
      'reference',
      'relation',
      'licenses_causal_because',
    ]) &&
    [value.comparison_evidence_id, value.cause_evidence_id, value.relation].every(isNonEmptyString) &&
    isLineRef(value.reference) &&
    typeof value.licenses_causal_because === 'boolean'
  );
}

function isIdeaUnit(value: unknown): value is ChesstoryIdeaUnit {
  return (
    isObject(value) &&
    hasExactKeys(value, [
      'idea_id',
      'kind',
      'lead_cause_evidence_id',
      'member_cause_evidence_ids',
      'importance_layer',
      'priority_status',
      'serialization_order',
    ]) &&
    [value.idea_id, value.kind, value.lead_cause_evidence_id, value.priority_status].every(
      isNonEmptyString,
    ) &&
    isStringSet(value.member_cause_evidence_ids) &&
    value.member_cause_evidence_ids.length > 0 &&
    value.member_cause_evidence_ids.includes(value.lead_cause_evidence_id) &&
    isNonNegativeInteger(value.importance_layer) &&
    isNonNegativeInteger(value.serialization_order)
  );
}

function isImportanceProjection(value: unknown): value is ChesstoryImportanceProjection {
  return (
    isObject(value) &&
    hasExactKeys(value, [
      'authority',
      'relation_policy_version',
      'ordering_policy_version',
      'comparison_exposure_rank_meaning',
      'selected_cause_ids',
      'frontier_cause_ids',
      'dominated_within_domain_cause_ids',
      'unique_top',
      'profile_summary',
      'profiles',
      'relations',
      'decisions',
      'relation_summary',
      'unmeasured',
    ]) &&
    value.authority === 'root_owned_effect_partial_order' &&
    value.relation_policy_version === 'chesstory.direct-cause-importance.relation.v3' &&
    value.ordering_policy_version === 'chesstory.player-facing-idea-ordering.dominance-layers.v1' &&
    value.comparison_exposure_rank_meaning === 'comparison_exposure_authority' &&
    isStringSet(value.selected_cause_ids) &&
    isStringSet(value.frontier_cause_ids) &&
    isStringSet(value.dominated_within_domain_cause_ids) &&
    isNullableString(value.unique_top) &&
    isProfileSummary(value.profile_summary) &&
    Array.isArray(value.profiles) &&
    Array.isArray(value.relations) &&
    Array.isArray(value.decisions) &&
    isRelationSummary(value.relation_summary) &&
    Array.isArray(value.unmeasured) &&
    (value.unique_top === null || value.selected_cause_ids.includes(value.unique_top))
  );
}

function isProfileSummary(value: unknown): boolean {
  return (
    isObject(value) &&
    hasExactKeys(value, [
      'measured_channels',
      'measured_causes',
      'fully_measured_causes',
      'unmeasured_causes',
    ]) &&
    [
      value.measured_channels,
      value.measured_causes,
      value.fully_measured_causes,
      value.unmeasured_causes,
    ].every(isNonNegativeInteger)
  );
}

function isRelationSummary(value: unknown): boolean {
  return (
    isObject(value) &&
    hasExactKeys(value, ['comparable_profile_pairs', 'incomparable_profile_pairs']) &&
    isNonNegativeInteger(value.comparable_profile_pairs) &&
    isNonNegativeInteger(value.incomparable_profile_pairs)
  );
}

function isAvailability(value: unknown): value is ChesstoryMoveMeaningSuccess['availability'] {
  return (
    isObject(value) &&
    hasExactKeys(value, ['state', 'reason']) &&
    ['ready', 'withheld'].includes(String(value.state)) &&
    (value.reason === null || value.reason === 'insufficient_engine_depth')
  );
}

export function chesstoryBriefSections(payload?: ChesstoryMoveMeaningPayload): ChesstoryBriefSection[] {
  if (!payload?.renderable) return placeholderSections();
  const explanation = payload.explanations[0];
  if (explanation) return causeSections(payload, explanation);
  return payload.verdict ? verdictSections(payload) : placeholderSections();
}

function causeSections(
  payload: ChesstoryMoveMeaningPayload,
  explanation: ChesstoryExplanation,
): ChesstoryBriefSection[] {
  const ordered = orderedCauseFacets(explanation);
  const uniqueTop = explanation.idea_importance.unique_top;
  const top = uniqueTop ? ordered.find(({ idea }) => idea.cause_evidence_id === uniqueTop) : undefined;
  const move = moveLabel(explanation.move || payload.verdict?.played_move || '');
  const quality = verdictLabel(payload);
  const tone = verdictTone(payload.verdict, quality);
  const causeItems = ordered.map(
    ({ idea, unit }) =>
      `${top?.idea.cause_evidence_id === idea.cause_evidence_id ? 'Top — ' : ''}${labelCode(
        idea.cause.kind,
      )} (${labelCode(idea.cause.effect_mode)}; ${labelCode(unit.priority_status)})`,
  );
  const channelItems = ordered.flatMap(({ idea }) =>
    idea.channels.map(channel => atomicChannelLabel(idea, channel)),
  );
  const comparisonItems = ordered.map(({ idea }) => comparisonLabel(idea));
  const proofItems = ordered.flatMap(({ idea }) =>
    idea.channels.flatMap(channel => {
      const segment = channel.proof_segment;
      if (!segment) return [];
      return [
        `${labelCode(idea.cause.kind)} — ${segment.steps
          .map(step => moveLabel(step.move_uci))
          .join(' ')} (${labelCode(segment.terminal_relation)})`,
      ];
    }),
  );
  const evidenceItems = ordered.flatMap(({ idea }) =>
    idea.channels.map(channel => channelEvidenceLabel(idea, channel)),
  );
  const reference = moveLabel(explanation.reference_move || payload.verdict?.reference_move || '');

  return [
    {
      key: 'opening-idea',
      title: top ? 'Top differential idea' : 'Certified differential ideas',
      body: top
        ? `${move || 'The move'}: ${labelCode(top.idea.cause.kind)}.`
        : 'No single top idea was certified; the published ideas remain separately identified.',
      pending: false,
      items: [`Move verdict: ${quality}`, ...verdictOutcomeLabels(payload.verdict), ...causeItems],
      tone,
    },
    {
      key: 'middlegame-plan',
      title: 'How each certified channel works',
      body: channelItems.length
        ? 'Each line below keeps one actor, target, mechanism, and consequence together.'
        : 'No certified direct channel was published.',
      pending: false,
      items: channelItems,
      tone,
    },
    {
      key: 'current-decision',
      title: 'What was compared',
      body: comparisonItems.length
        ? 'Each differential idea keeps its own comparison viewpoint.'
        : 'No differential comparison was published.',
      pending: false,
      items: comparisonItems,
      tone,
    },
    {
      key: 'better-plan',
      title: 'Reference move and observed sequence',
      body: reference
        ? `${reference} is the published reference move.`
        : 'No different reference move is required.',
      pending: false,
      items: proofItems,
      tone: 'neutral',
    },
    {
      key: 'evidence',
      title: 'Published evidence scope',
      body: evidenceItems.length
        ? 'These labels are copied from each channel without combining their evidence.'
        : 'No additional evidence descriptor was published.',
      pending: false,
      items: evidenceItems,
    },
  ];
}

function orderedCauseFacets(
  explanation: ChesstoryExplanation,
): Array<{ idea: ChesstoryCauseFacet; unit: ChesstoryIdeaUnit }> {
  const ideasByUnit = new Map<string, ChesstoryCauseFacet[]>();
  for (const idea of explanation.ideas) {
    const ideas = ideasByUnit.get(idea.idea_unit_id) || [];
    ideas.push(idea);
    ideasByUnit.set(idea.idea_unit_id, ideas);
  }
  return [...explanation.idea_units]
    .sort((left, right) => left.serialization_order - right.serialization_order)
    .flatMap(unit =>
      (ideasByUnit.get(unit.idea_id) || [])
        .sort((left, right) => left.selection_order - right.selection_order)
        .map(idea => ({ idea, unit })),
    );
}

function atomicChannelLabel(idea: ChesstoryCauseFacet, channel: ChesstoryCauseChannel): string {
  const actor = actorLabel(channel.actor);
  const targets = objectLabels(channel.targets);
  const mechanisms = objectLabels(channel.mechanisms);
  const consequences = objectLabels(channel.consequences);
  return `${labelCode(idea.cause.kind)} — ${actor}; direct change: ${labelCode(
    channel.direct_change,
  )}; played change: ${labelCode(channel.played_change)}; target: ${targets}; mechanism: ${mechanisms}; consequence: ${consequences}`;
}

function comparisonLabel(idea: ChesstoryCauseFacet): string {
  const comparison = idea.comparison;
  return `${labelCode(idea.cause.kind)} — ${labelCode(comparison.kind)}: ${moveLabel(
    comparison.event.root_move,
  )} compared with ${moveLabel(comparison.compared_move)} (${labelCode(comparison.verdict)}; ${labelCode(
    idea.cause.source_side,
  )} source)`;
}

function channelEvidenceLabel(idea: ChesstoryCauseFacet, channel: ChesstoryCauseChannel): string {
  const descriptor = channel.effect_descriptor;
  const proof = descriptor.effect_scope.primitive_kind;
  const magnitude = descriptor.magnitude_status;
  const scope = `${labelCode(proof)}, ${labelCode(magnitude)}`;
  return `${labelCode(idea.cause.kind)} — ${labelCode(channel.carrier.layer)} / ${labelCode(
    channel.carrier.scope,
  )}; effect: ${scope}`;
}

function actorLabel(actor: ChesstoryActor): string {
  const route = moveLabel(`${actor.from}${actor.to}`);
  return [labelCode(actor.side), labelCode(actor.piece), route].join(' ');
}

function objectLabels(objects: ChesstoryObjectRef[]): string {
  return objects.map(object => `${labelCode(object.kind)} ${labelCode(object.key)}`).join(', ');
}

function verdictSections(payload: ChesstoryMoveMeaningPayload): ChesstoryBriefSection[] {
  const verdict = payload.verdict;
  const move = moveLabel(verdict?.played_move || '');
  const reference = moveLabel(verdict?.reference_move || '');
  const label = verdictLabel(payload);
  const facts = verdictOutcomeLabels(verdict);
  const tone = verdictTone(verdict, label);
  return [
    {
      key: 'opening-idea',
      title: 'Move verdict',
      body: `${move || 'The move'} is classified as ${label || 'engine assessed'}.`,
      pending: false,
      items: facts,
      tone,
    },
    {
      key: 'middlegame-plan',
      title: 'Engine outcome',
      body: facts.length ? joinHuman(facts) : 'The verdict is bound to the compared engine lines.',
      pending: false,
      items: facts,
      tone,
    },
    {
      key: 'current-decision',
      title: 'Current decision',
      body:
        reference && reference !== move
          ? `${reference} is the verified reference move.`
          : 'The played move matches the reference.',
      pending: false,
      tone,
    },
    {
      key: 'better-plan',
      title: 'Reference move',
      body: reference || 'No different reference move is required.',
      pending: false,
      tone: 'neutral',
    },
    {
      key: 'evidence',
      title: 'Why this verdict is shown',
      body: 'The verdict comes from the registered played/reference comparison.',
      pending: false,
    },
  ];
}

function verdictLabel(payload: ChesstoryMoveMeaningPayload): string {
  return labelCode(payload.verdict?.verdict_code || payload.verdict?.move_quality);
}

function verdictOutcomeLabels(verdict: ChesstoryVerdict | null): string[] {
  const outcome = verdict?.outcome;
  const mate = verdict?.mate;
  return uniqueLabels([
    outcome?.state ? `Outcome: ${labelCode(outcome.state)}` : '',
    outcome?.reference_state ? `Reference outcome: ${labelCode(outcome.reference_state)}` : '',
    outcome?.resistance ? `Resistance: ${labelCode(outcome.resistance)}` : '',
    mate?.played_for_mover !== null && mate?.played_for_mover !== undefined
      ? `Played mate score: ${mate.played_for_mover}`
      : '',
    mate?.reference_for_mover !== null && mate?.reference_for_mover !== undefined
      ? `Reference mate score: ${mate.reference_for_mover}`
      : '',
    mate?.distance_loss !== null && mate?.distance_loss !== undefined
      ? `Mate-distance loss: ${mate.distance_loss}`
      : '',
  ]);
}

function verdictTone(verdict: ChesstoryVerdict | null, label: string): 'good' | 'bad' | 'neutral' {
  const code = labelCode(verdict?.verdict_code || label);
  if (['inaccuracy', 'mistake', 'blunder', 'forced loss'].includes(code)) return 'bad';
  if (['improves on reference', 'matches reference', 'good', 'forced win'].includes(code)) return 'good';
  return 'neutral';
}

function placeholderSections(): ChesstoryBriefSection[] {
  return [
    {
      key: 'opening-idea',
      title: 'Opening idea',
      body: 'Chesstory reads the opening structure, tension, or tabiya behind this game.',
      pending: true,
    },
    {
      key: 'middlegame-plan',
      title: 'Plan created by the opening',
      body: 'Your review points to the pawn break, piece route, or target that guides the middlegame.',
      pending: true,
    },
    {
      key: 'current-decision',
      title: 'Current decision',
      body: 'Each move you review shows what the move means beyond the live Stockfish number.',
      pending: true,
    },
    {
      key: 'better-plan',
      title: 'Better plan / alternative',
      body: 'Coach explanation can turn this position read into deeper chess language.',
      pending: true,
    },
    {
      key: 'evidence',
      title: 'Board clues',
      body: 'Board clues, eval shifts, and candidate lines stay visible as the reference.',
      pending: true,
    },
  ];
}

function isObject(value: unknown): value is Record<string, any> {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function hasExactKeys(
  value: Record<string, unknown>,
  required: readonly string[],
  optional: readonly string[] = [],
): boolean {
  const allowed = new Set([...required, ...optional]);
  const keys = Object.keys(value);
  return required.every(key => key in value) && keys.every(key => allowed.has(key));
}

function isNonEmptyString(value: unknown): value is string {
  return typeof value === 'string' && value.trim().length > 0;
}

function isNullableString(value: unknown): value is string | null {
  return value === null || isNonEmptyString(value);
}

function isFiniteNumber(value: unknown): value is number {
  return typeof value === 'number' && Number.isFinite(value);
}

function isNullableNumber(value: unknown): value is number | null {
  return value === null || isFiniteNumber(value);
}

function isNonNegativeInteger(value: unknown): value is number {
  return Number.isInteger(value) && Number(value) >= 0;
}

function isStringSet(value: unknown): value is string[] {
  return Array.isArray(value) && value.every(isNonEmptyString) && new Set(value).size === value.length;
}

function sameStringSet(left: string[], right: string[]): boolean {
  return left.length === right.length && left.every(value => right.includes(value));
}

function labelCode(raw?: string): string {
  return (raw || '')
    .replace(/([a-z0-9])([A-Z])/g, '$1 $2')
    .replace(/[_-]+/g, ' ')
    .trim()
    .toLowerCase();
}

function moveLabel(uci: string): string {
  return uci.replace(/^([a-h][1-8])([a-h][1-8])([nbrq])?$/, '$1-$2$3');
}

function uniqueLabels(labels: string[]): string[] {
  return [...new Set(labels.map(label => label.trim()).filter(Boolean))];
}

function joinHuman(items: string[]): string {
  if (items.length <= 1) return items[0] || '';
  if (items.length === 2) return `${items[0]} and ${items[1]}`;
  return `${items.slice(0, -1).join(', ')}, and ${items[items.length - 1]}`;
}
