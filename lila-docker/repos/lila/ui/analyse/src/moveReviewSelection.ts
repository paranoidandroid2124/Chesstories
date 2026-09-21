import type { MoveReviewFrameSelection, MoveReviewObservations, ObservedLineSummary } from './moveReview';
import { parseFen } from 'chessops/fen';
import { parseSquare, parseUci, makeSquare, kingCastlesTo, rookCastlesTo } from 'chessops/util';
import { castlingSide } from 'chessops/chess';
import { setupPosition } from 'chessops/variant';
import { attacks } from 'chessops/attacks';
import { isMoveReviewEngineProfile } from 'lib/ceval/types';

export type CommentaryState = 'ready' | 'partial' | 'needs_explanation';

export interface CommentaryCandidate {
  id: string;
  kind: 'local_fact' | 'conditional_line' | 'comparison';
  state: CommentaryState;
  selection: 'selected' | 'display_cap' | 'display_representative' | 'context';
  rootOccurrenceId: string;
  occurrenceIds: string[];
  positionIds: string[];
  lineIds: string[];
  evidenceIds: string[];
  sourceRefs: {
    line_id: string;
    kind: 'supplied' | 'evaluation' | 'admitted';
    source: string;
    accepted_ply_count: number;
    stop: string | null;
  }[];
  detail: Record<string, unknown> & { type: string };
}

export interface CommentaryScene {
  id: string;
  rootOccurrenceId: string;
  occurrenceIds: string[];
  lineIds: string[];
  candidateIds: string[];
  state: CommentaryState;
}

export interface CommentarySelection {
  version: 1;
  candidates: CommentaryCandidate[];
  scenes: CommentaryScene[];
  inquiries?: CommentaryInquiry[];
  response_inquiries?: CommentaryResponseInquiry[];
  response_searches?: CommentaryResponseSearch[];
  root_searches?: CommentaryRootSearch[];
  continuation_inquiries?: CommentaryContinuationInquiry[];
  proposals?: CommentaryProposal[];
  /** Validated question decisions, without the samples/proofs used to admit the response. */
  questionOutcomes?: CommentaryQuestionOutcome[];
  local_expansions_used?: number;
  discovery?: CommentaryDiscovery;
}

export interface CommentaryDiscovery {
  version: 1;
  input_key: string;
  scans: {
    prefix_moves_uci: Uci[];
    reference_move: Uci;
    legal_moves: Uci[];
    examined_moves: Uci[];
    deferred_request_ids: string[];
    pending_replay_prefixes: Uci[][];
    state: 'complete' | 'partial' | 'unavailable' | 'not_applicable';
    reason: string | null;
  }[];
}

export interface CommentaryProposalSource {
  kind: 'user' | 'author' | 'model' | 'generator' | 'none';
  producer: string;
  version?: string;
  source_refs: string[];
}
export interface CommentaryProposal {
  id: string;
  admission: 'submitted' | 'internal' | 'generated';
  // Presentation only; validated requirements remain the source of evidence.
  question_text?: string;
  answer_text?: string;
  origin: Record<'question' | 'candidates' | 'priority' | 'evidence', CommentaryProposalSource>;
  requirements: CommentaryProposalReading[];
}
export interface CommentaryQuestionOutcome extends Pick<CommentaryProposal, 'id' | 'question_text' | 'answer_text'> {
  conditions: (Pick<CommentaryProposalReading, 'id' | 'state' | 'stop_reason'> & { reason: string | null })[];
  unresolved: Omit<CommentaryProposalReading, 'result' | 'answer'>[];
  needs: CommentaryFactResult['needs'];
}

/** Call only after response/work validation or after abandoning the owning investigation. */
export function releaseCommentaryInvestigation(observations: MoveReviewObservations, stopReason?: string): void {
  const selection = observations.selection;
  if (!selection || (!selection.proposals && !selection.root_searches)) return;
  const { proposals, root_searches: _searches, local_expansions_used: _expansions, ...retained } = selection;
  const resolved = (reading: CommentaryProposalReading) =>
    ['observed', 'answered', 'supported', 'refuted'].includes(reading.state) ||
    (reading.state === 'not_applicable' && reading.result?.reason === 'event_absent' && !reading.result.needs.length);
  observations.selection = {
    ...retained,
    ...(proposals && { questionOutcomes: proposals.map(proposal => ({
      id: proposal.id,
      ...(proposal.question_text && { question_text: proposal.question_text }),
      ...(proposal.answer_text && { answer_text: proposal.answer_text }),
      conditions: proposal.requirements.map(reading => ({
        id: reading.id, state: reading.state,
        stop_reason: reading.stop_reason ?? (!resolved(reading) ? stopReason ?? null : null),
        reason: reading.result?.reason ?? null,
      })),
      unresolved: proposal.requirements.filter(reading => !resolved(reading)).map(reading => {
        const { result: _result, answer: _answer, ...scope } = reading;
        return { ...scope, stop_reason: scope.stop_reason ?? stopReason ?? null };
      }),
      needs: proposal.requirements.flatMap(reading => reading.result?.needs ?? []),
    })) }),
  };
}
export type CommentaryFactPiece = Pick<CommentaryInquiryPiece, 'side' | 'role' | 'square'>;
export interface CommentaryFactPath {
  binding: string;
  extend: boolean;
  source:
    | { kind: 'declared'; moves_uci: Uci[] }
    | { kind: 'comparison'; requirement_id: string; arm: 'reference' | 'alternative' }
    | { kind: 'premise'; inquiry_id: string; premise_id: string };
}
export type CommentaryFactPoint =
  | { kind: 'ply'; ply_count: number }
  | {
      kind: 'event';
      event: 'move' | 'capture' | 'promotion' | 'check' | 'castling';
      ordinal: number;
      phase: 'before' | 'after';
      from_ply: number;
      through_ply: number;
      actor?: CommentaryFactPiece;
      target?: CommentaryFactPiece;
      square?: Key;
    };
export interface CommentaryFactPredicate {
  kind:
    | 'presence'
    | 'location'
    | 'role'
    | 'control'
    | 'support'
    | 'legal_move'
    | 'legal_capture'
    | 'check'
    | 'pawn_property'
    | 'pawn_advance'
    | 'file_occupancy'
    | 'direct_support_use';
  subject?: CommentaryFactPiece;
  target?: CommentaryFactPiece;
  square?: Key;
  side?: Color;
  property?: string;
}
export interface CommentaryFactSample {
  id: string;
  line_id: string;
  binding: string;
  /** Exact prefix of line_id; independent of the position range being read. */
  prefix_ply_count?: number;
  moves_uci?: Uci[];
  from_ply: number;
  through_ply: number;
  position_ids?: string[];
  occurrence_ids?: string[];
  value: string | null;
  state: 'known' | 'unavailable_input' | 'not_applicable';
  source_ids: string[];
  reason: string | null;
}
export interface CommentaryFactResult {
  state:
    | 'observed'
    | 'supported'
    | 'refuted'
    | 'partial'
    | 'unavailable_input'
    | 'not_applicable'
    | 'invalid_input';
  samples: CommentaryFactSample[];
  needs: {
    kind: 'input' | 'comparison' | 'continuation';
    reason: string;
    prefix_moves_uci: Uci[];
    requirement_id: string | null;
    horizon: number | null;
  }[];
  proofs: { bindings: Record<string, Uci[]>; sample_ids: string[]; requirement_ids: string[] }[];
  reason: string | null;
}
export type CommentaryProposalReading = {
  id: string;
  state:
    | 'deferred'
    | 'awaiting_engine'
    | 'observed'
    | 'answered'
    | 'supported'
    | 'refuted'
    | 'partial'
    | 'not_applicable'
    | 'invalid_input'
    | 'unavailable_input'
    | 'unsupported_reading'
    | 'stopped';
  stop_reason: string | null;
  answer: CommentaryInquiryAnswer | null;
  line_id: string | null;
  occurrence_id: string | null;
  position_id: string | null;
  work_id: string | null;
  result?: CommentaryFactResult;
} & (
  | { kind: 'observe_move'; prefix_moves_uci: Uci[]; move_uci: Uci }
  | {
      kind: 'compare_moves';
      prefix_moves_uci: Uci[];
      reference_move: Uci;
      alternative_move: Uci;
      expected_relation?: 'better' | 'equal' | 'worse';
    }
  | { kind: 'uninterpreted'; text: string }
  | {
      kind: 'fact';
      path: CommentaryFactPath;
      predicate: CommentaryFactPredicate;
      at: CommentaryFactPoint;
      through?: CommentaryFactPoint;
      expected?: string;
    }
  | {
      kind: 'fact_comparison';
      left_requirement_id: string;
      right_requirement_id: string;
      relation: 'same' | 'different' | 'before' | 'after' | 'same_time';
    }
  | { kind: 'fact_group'; mode: 'all' | 'any'; members: string[] }
);

type EvaluationSource =
  | 'direct_input'
  | 'root_search'
  | 'focus_comparison'
  | 'investigation_comparison'
  | 'response_comparison';
export type CommentaryEvaluation = { source: EvaluationSource } & (
  | { kind: 'engine_search'; depth: number; white_score: { kind: 'cp' | 'mate'; value: number } }
  | {
      kind: 'exact_automatic_terminal';
      terminal:
        | { kind: 'checkmate'; winner: Color }
        | {
            kind: 'stalemate' | 'insufficient_material' | 'fivefold_repetition' | 'seventy_five_move_rule';
          };
    }
);
export interface CommentaryInquiryPiece {
  position_id: string;
  side: Color;
  role: 'pawn' | 'knight' | 'bishop' | 'rook' | 'queen' | 'king';
  square: Key;
}
export interface CommentaryInquiryReason {
  kind:
    | 'engine_alternative'
    | 'capture_option'
    | 'same_destination'
    | 'same_actor_choice'
    | 'target_relocation'
    | 'attacker_capture'
    | 'target_support';
  evidence_id: string;
  source_id: string | null;
  source_line_id: string | null;
  anchor_move: Uci;
  actor: CommentaryInquiryPiece;
  capture: CommentaryInquiryPiece | null;
  promotion_role: 'queen' | 'rook' | 'bishop' | 'knight' | null;
  responds_to_check: boolean;
  source_name?: 'engine_pv' | 'legal_root' | 'attacked_target';
  target?: CommentaryInquiryPiece | null;
  pressure_source_ids?: string[];
}
export interface CommentaryInquiryAnswer {
  work_id: string;
  execution_key_sha256: string;
  engine_profile: string;
  required_depth: number;
  reference_depth: number | null;
  alternative_depth: number | null;
  evaluation_relation: 'better' | 'equal' | 'worse';
  reference_line_id: string;
  alternative_line_id: string;
  reference_occurrence_id: string;
  alternative_occurrence_id: string;
  reference_evaluation: CommentaryEvaluation;
  alternative_evaluation: CommentaryEvaluation;
}
export interface CommentaryResponseAnswer extends CommentaryInquiryAnswer {
  search_position_id: string;
  ply_offset: 1;
}
export interface CommentaryResponseSearch {
  work_id: string;
  execution_key_sha256: string;
  engine_profile: string;
  required_depth: number;
  multi_pv: number;
  root_move: Uci;
  root_move_occurrence_id: string;
  child_position_id: string;
  prefix_moves_uci?: Uci[];
  ply_offset?: number;
  root_restriction: { kind: 'unrestricted' } | { kind: 'restricted'; moves_uci: Uci[] };
  candidates: {
    move_uci: Uci;
    line_id: string;
    occurrence_id: string;
    evaluation: CommentaryEvaluation;
  }[];
}
export interface CommentaryRootSearch {
  work_id: string;
  execution_key_sha256: string;
  engine_profile: string;
  required_depth: number;
  multi_pv: number;
  purpose: 'root_search' | 'focus_comparison' | 'investigation_comparison';
  root_position_id: string;
  root_occurrence_id: string;
  root_restriction: CommentaryResponseSearch['root_restriction'];
  state: 'awaiting_engine' | 'completed' | 'stopped';
  stop_reason: string | null;
  candidates: { move_uci: Uci; line_id: string | null; occurrence_id: string | null }[];
}
export function responseSearchPrefix(search: CommentaryResponseSearch): Uci[] {
  return search.prefix_moves_uci ?? [search.root_move];
}

export function commentarySearchAnchorPath(
  observations: MoveReviewObservations,
  rootFen: FEN,
  rootMove: Uci,
  rootOccurrenceId: string,
  positionId: string,
  prefix: readonly Uci[],
) {
  if (!prefix.length || prefix[0] !== rootMove) return;
  const position = observations.positions?.find(position => position.id === positionId);
  return (
    position &&
    observations.paths.find(
      path =>
        path.kind !== 'history' &&
        path.startFen === rootFen &&
        observations.positions?.some(root => root.id === path.positionIds?.[0] && root.fen === rootFen) &&
        prefix.every((move, index) => path.moves[index]?.uci === move) &&
        path.occurrenceIds[0] === rootOccurrenceId &&
        path.positionIds?.[prefix.length] === positionId &&
        path.moves[prefix.length - 1]?.fenAfter === position.fen,
    )
  );
}

export function responseSearchMatchesInquiry(
  search: CommentaryResponseSearch,
  inquiry: CommentaryResponseInquiry,
): boolean {
  return (
    responseSearchPrefix(search).length === 1 &&
    search.root_move === inquiry.root_move &&
    search.root_move_occurrence_id === inquiry.root_move_occurrence_id &&
    search.child_position_id === inquiry.child_position_id
  );
}
export interface CommentaryResponseSelection {
  source: 'child_search' | 'root_pv_hint' | 'observed_line' | 'legal_seed';
  work_id: string | null;
  line_id: string | null;
}
export interface CommentaryResponseInquiry {
  id: string;
  scope: 'target_response';
  source: 'automatic_observation';
  root_occurrence_id: string;
  root_move: Uci;
  root_move_occurrence_id: string;
  child_position_id: string;
  original_target: CommentaryInquiryPiece;
  related_inquiry_ids: string[];
  target: CommentaryInquiryPiece;
  pressure_source_ids: string[];
  capture_moves: Uci[];
  decline_moves: Uci[];
  move_source_ids: Record<Uci, string>;
  reference_move: Uci;
  alternative_move: Uci;
  reference_selection?: CommentaryResponseSelection;
  alternative_selection?: CommentaryResponseSelection;
  state: 'deferred' | 'awaiting_engine' | 'answered' | 'observed' | 'stopped';
  stop_reason: string | null;
  answer: CommentaryResponseAnswer | null;
}

const continuationKinds = [
  'root_capture_response',
  'delayed_capturer_removal',
  'other_target_gain',
  'continuation_not_observed',
  'material_followup',
  'check_response_not_observed',
  'capturer_reply',
  'replacement_function_choice',
  'support_application_not_observed',
  'capture_exposure_response',
  'exposure_prevention_choice',
] as const;

export interface CommentaryContinuationComparisonSource {
  work_id: string;
  donor_line_id: string;
  peer_line_id: string;
  kind?: 'material_recovery' | 'support_application';
  observation_line_id?: string | null;
}

export interface CommentaryContinuationPremise {
  id: string;
  kind: (typeof continuationKinds)[number];
  trigger_occurrence_ids: string[];
  source_line_ids: string[];
  comparison_source_indices: number[];
  requirement_id: string | null;
  witness_line_ids: string[];
  unresolved_source_line_ids: string[];
  observation_state: 'observed' | 'unobserved' | 'unavailable' | 'not_applicable';
}

export interface CommentaryContinuationInquiry {
  id: string;
  scope: 'line_continuation';
  source: 'automatic_observation';
  root_occurrence_id: string;
  root_position_id: string;
  position_id: string;
  prefix_moves_uci: Uci[];
  prefix_occurrence_ids: string[];
  trigger_occurrence_ids: string[];
  observed_moves: Uci[];
  source_line_ids: string[];
  kinds: string[];
  premises: CommentaryContinuationPremise[];
  legal_moves: Uci[];
  move_source_ids: Record<Uci, string>;
  state: 'deferred' | 'awaiting_engine' | 'surveyed' | 'stopped' | 'forced' | 'observed';
  stop_reason: string | null;
  search_work_ids: string[];
  explanation_candidate_ids: string[];
  comparison_sources?: CommentaryContinuationComparisonSource[];
  unresolved_source_line_ids?: string[];
  followup_question_ids?: string[];
  route_challenge?: CommentaryRouteChallenge | null;
}

export interface CommentaryRouteChallenge {
  state: 'not_returned' | 'awaiting_engine' | 'sampled' | 'compared' | 'stopped';
  stop_reason: string | null;
  reference_work_id: string;
  reference_move: Uci;
  reference_line_id: string;
  move_classes: {
    kind?: 'recovery_route' | 'replacement_recapture' | 'exposed_capture';
    moves_uci: Uci[];
    premise_occurrence_ids: string[];
    source_line_ids: string[];
    sampled_work_ids: string[];
    comparison: { work_id: string; reference_line_id: string; challenge_line_id: string } | null;
  }[];
}

export function continuationRoutePoolMatches(
  inquiry: CommentaryContinuationInquiry,
  moves: Uci[],
  pending = false,
  referenceMove = inquiry.route_challenge?.reference_move,
): boolean {
  const route = inquiry.route_challenge;
  if (!route || !referenceMove || !moves.includes(referenceMove)) return false;
  const eligible = route.move_classes.filter(group =>
    pending ? !group.sampled_work_ids.length : group.moves_uci.every(move => moves.includes(move)),
  );
  const classes =
    pending && eligible.some(group => group.kind !== 'exposed_capture')
      ? eligible.filter(group => group.kind !== 'exposed_capture')
      : eligible;
  return classes.length > 0 && sameIds(moves, [referenceMove, ...classes.flatMap(group => group.moves_uci)]);
}

export function responseSearchMatchesContinuation(
  search: CommentaryResponseSearch,
  inquiry: CommentaryContinuationInquiry,
): boolean {
  return (
    search.root_move_occurrence_id === inquiry.prefix_occurrence_ids[0] &&
    search.child_position_id === inquiry.position_id &&
    responseSearchPrefix(search).length === inquiry.prefix_moves_uci.length &&
    responseSearchPrefix(search).every((move, index) => move === inquiry.prefix_moves_uci[index])
  );
}

// Both issued work and completed receipts must name entire reply classes at one child.
export function responsePoolMatchesClasses(
  inquiries: CommentaryResponseInquiry[],
  moves: Uci[],
  searches?: CommentaryResponseSearch[],
): boolean {
  if (!inquiries.length) return false;
  if (inquiries.some(inquiry => sameIds(moves, [inquiry.reference_move, inquiry.alternative_move])))
    return true;
  const classes = inquiries.flatMap(inquiry => [inquiry.capture_moves, inquiry.decline_moves]);
  if (!sameIds(moves, classes.filter(family => family.every(move => moves.includes(move))).flat()))
    return false;
  const covers = (search: CommentaryResponseSearch, family: Uci[]) =>
    search.root_restriction.kind === 'unrestricted' ||
    family.every(
      move =>
        search.root_restriction.kind === 'restricted' && search.root_restriction.moves_uci.includes(move),
    );
  if (!searches?.some(search => covers(search, classes[0].concat(classes[1])))) return true;
  const missing = classes.filter(
    family =>
      !searches.some(
        search =>
          covers(search, family) && search.candidates.some(candidate => family.includes(candidate.move_uci)),
      ),
  );
  return sameIds(moves, missing.flat());
}
export interface CommentaryTrackedPiece {
  original: CommentaryInquiryPiece;
  current: CommentaryInquiryPiece;
}
export interface CommentaryLineContinuationDetail {
  type: 'line_continuation';
  scope: 'observed_line';
  line_id: string;
  events: {
    id: string;
    ply_offset: number;
    occurrence_id: string;
    source_id: string;
    original_actor: CommentaryInquiryPiece | null;
    original_victim: CommentaryInquiryPiece | null;
    actor_end: {
      state: 'present' | 'captured' | 'unknown';
      position_id: string;
      piece: CommentaryInquiryPiece | null;
      removed_by_event_id: string | null;
    };
  }[];
  capture_links: { loss_event_id: string; capture_event_id: string; elapsed_plies: number }[];
  other_target_gains: { loss_event_id: string; gain_event_id: string }[];
  open_loss_event_ids?: string[];
  recovery_losses?: { loss_event_id: string; recovery_event_id: string; later_loss_event_id: string }[];
  support_comparisons: {
    id: string;
    kind: 'actor_continuation' | 'replacement_after_capture';
    state: 'complete' | 'unknown';
    before_actor: CommentaryInquiryPiece;
    after_actor: CommentaryInquiryPiece;
    before_position_id: string;
    after_position_id: string;
    trigger_occurrence_ids: string[];
    targets: {
      original_target: CommentaryInquiryPiece;
      before: ContinuationSupport;
      after: ContinuationSupport;
    }[];
  }[];
  support_applications?: {
    requirement_id: string | null;
    comparison_id: string;
    original_target: CommentaryInquiryPiece;
    original_provider: CommentaryInquiryPiece;
    ply_count: number;
    position_id: string;
    preceding_occurrence_id: string;
    support: ContinuationSupport;
    application: ContinuationKingCapture;
  }[];
  capture_exposures?: ContinuationCaptureExposure[];
  direct_support_recaptures?: ContinuationDirectSupportRecapture[];
  text: string;
}
interface ContinuationCaptureExposure {
  kind: 'capture_exposure';
  prefix_moves_uci: Uci[];
  occurrence_id: string;
  before_position_id: string;
  position_id: string;
  original_attacker: CommentaryInquiryPiece;
  attacker: CommentaryInquiryPiece;
  original_target: CommentaryInquiryPiece;
  target: CommentaryInquiryPiece;
  vacated_blockers: { original: CommentaryInquiryPiece; before: CommentaryInquiryPiece }[];
  ray_source_id: string;
  control_source_ids: string[];
  moves_uci: Uci[];
  source_ids: string[];
  legal_inventory_complete: true;
}
interface ContinuationDirectSupportRecapture {
  kind: 'direct_support_recapture';
  requirement_id: string;
  comparison_id: string;
  original_provider: CommentaryInquiryPiece;
  original_target: CommentaryInquiryPiece;
  original_capturer: CommentaryInquiryPiece;
  ply_count: number;
  position_id: string;
  provider: CommentaryInquiryPiece;
  target: CommentaryInquiryPiece;
  capturer: CommentaryInquiryPiece;
  support: ContinuationSupport;
  loss_event_id: string;
  recapture_event_id: string;
  loss_occurrence_id: string;
  recapture_occurrence_id: string;
  elapsed_plies: number;
}
interface ContinuationSupport {
  state: 'present' | 'absent' | 'actor_removed' | 'target_removed' | 'unknown';
  source_ids: string[];
}
export function lineContinuationDetail(
  candidate: CommentaryCandidate,
): CommentaryLineContinuationDetail | undefined {
  return candidate.kind === 'conditional_line' && candidate.detail.type === 'line_continuation'
    ? (candidate.detail as unknown as CommentaryLineContinuationDetail)
    : undefined;
}

function continuationRecoveries(
  detail: CommentaryLineContinuationDetail,
  lossId: string,
): CommentaryLineContinuationDetail['events'] {
  const ids = new Set([
    ...detail.capture_links.filter(link => link.loss_event_id === lossId).map(link => link.capture_event_id),
    ...detail.other_target_gains
      .filter(link => link.loss_event_id === lossId)
      .map(link => link.gain_event_id),
  ]);
  return detail.events.filter(event => ids.has(event.id));
}

function continuationLossIsOpen(
  path: MoveReviewObservations['paths'][number],
  loss: CommentaryLineContinuationDetail['events'][number],
): boolean {
  const summary = path.lineSummary;
  if (!summary?.material || !loss.original_victim) return false;
  const sign = loss.original_victim.side === summary.perspective ? 1 : -1;
  const interval = summary.material.events.filter(event => event.ply_offset >= loss.ply_offset);
  return interval.length > 0 && interval.every(event => event.balance_cp * sign < 0);
}

function continuationOpenLossIds(
  detail: CommentaryLineContinuationDetail,
  path: ObservationPath,
): Set<string> {
  // Relevance is a producer fact. Validate its ledger/dependencies, not a second discovery algorithm.
  return new Set(
    detail.open_loss_event_ids ??
      detail.events.filter(event => continuationLossIsOpen(path, event)).map(event => event.id),
  );
}

function continuationLossOrigins(detail: CommentaryLineContinuationDetail, lossId: string): Set<string> {
  const events = new Map(detail.events.map(event => [event.id, event]));
  const pending = [lossId],
    visited = new Set<string>();
  while (pending.length) {
    const id = pending.pop()!;
    if (visited.has(id)) continue;
    visited.add(id);
    pending.push(
      ...(detail.recovery_losses ?? [])
        .filter(link => link.later_loss_event_id === id)
        .map(link => link.loss_event_id),
    );
  }
  return new Set([...visited].map(id => events.get(id)!.occurrence_id));
}
function continuationTriggerOrigins(
  detail: CommentaryLineContinuationDetail,
  occurrences: string[],
): string[] {
  return occurrences.flatMap(occurrence => {
    const event = detail.events.find(event => event.occurrence_id === occurrence);
    return event ? [...continuationLossOrigins(detail, event.id)] : [occurrence];
  });
}
interface ContinuationComparisonPoint {
  line_id: string;
  ply_count: number;
  position_id: string;
  preceding_occurrence_id: string | null;
}
interface ContinuationLossPoint extends ContinuationComparisonPoint {
  loss_event_id: string;
  loss_ply_offset: number;
  observed_followup_plies: number;
  capturer_end: CommentaryLineContinuationDetail['events'][number]['actor_end'];
  capturer_removal_state: 'observed' | 'not_observed_through_line_end';
  capturer_removal_event_id: string | null;
  // Only gains linked to this loss interval, not every capture anywhere in the line.
  other_target_gain_state: 'observed' | 'not_observed_through_line_end';
  other_target_gain_event_ids: string[];
}
interface ContinuationKingCapture {
  kind: 'king_capture';
  original_attacker: CommentaryInquiryPiece;
  attacker: CommentaryInquiryPiece;
  target: CommentaryInquiryPiece;
  contact_source_ids: string[];
  moves_uci: Uci[];
  source_ids: string[];
  legal_inventory_complete: true;
}
export interface CommentaryContinuationComparisonDetail {
  type: 'line_continuation_comparison';
  scope: 'observed_line_pair';
  continuation_candidate_ids: string[];
  text: string;
  differences: (
    | {
        kind: 'recapturer_support';
        original_subject: CommentaryInquiryPiece;
        original_target: CommentaryInquiryPiece;
        matched_support_comparison_ids: string[];
        left: {
          before: ContinuationComparisonPoint & { support: ContinuationSupport };
          after: ContinuationComparisonPoint & { support: ContinuationSupport };
        };
        right: {
          before: ContinuationComparisonPoint & { support: ContinuationSupport };
          after: ContinuationComparisonPoint & { support: ContinuationSupport };
        };
      }
    | {
        kind: 'loss_followup';
        original_actor: CommentaryInquiryPiece;
        original_victim: CommentaryInquiryPiece;
        left: ContinuationLossPoint;
        right: ContinuationLossPoint;
        shared_attackers?: {
          original_actor: CommentaryInquiryPiece;
          actor: CommentaryInquiryPiece;
          position_id: string;
          ply_count: number;
          left: { event_id: string; target: CommentaryInquiryPiece; source_ids: string[] };
          right: { event_id: string; target: CommentaryInquiryPiece; source_ids: string[] };
        }[];
      }
    | {
        kind: 'capture_timing';
        original_actor: CommentaryInquiryPiece;
        original_victim: CommentaryInquiryPiece;
        left: ContinuationComparisonPoint & { event_id: string; source_id: string };
        right: ContinuationComparisonPoint & { event_id: string; source_id: string };
      }
    | {
        kind: 'legal_capture_availability';
        original_actor: CommentaryInquiryPiece;
        original_victim: CommentaryInquiryPiece;
        matched_capture_event_ids?: string[];
        matched_support_comparison_ids?: string[];
        left: ContinuationComparisonPoint & {
          available: boolean;
          moves_uci: Uci[];
          source_ids: string[];
          position_occurrence_id: string;
          legal_inventory_complete: true;
        };
        right: ContinuationComparisonPoint & {
          available: boolean;
          moves_uci: Uci[];
          source_ids: string[];
          position_occurrence_id: string;
          legal_inventory_complete: true;
        };
      }
    | {
        kind: 'replacement_support' | 'root_actor_support';
        original_subject: CommentaryInquiryPiece;
        original_target: CommentaryInquiryPiece;
        relation: 'preserved_with_different_provider' | 'different_support';
        left: ContinuationComparisonPoint & {
          original_provider: CommentaryInquiryPiece;
          support: ContinuationSupport;
          comparison_id: string;
          trigger_occurrence_ids: string[];
          application?: ContinuationKingCapture;
          target_capture_event_id?: string;
          direct_recapture?: { loss_event_id: string; recapture_event_id: string };
        };
        right: ContinuationComparisonPoint & {
          original_provider: CommentaryInquiryPiece;
          support: ContinuationSupport;
          comparison_id: string;
          trigger_occurrence_ids: string[];
          application?: ContinuationKingCapture;
          target_capture_event_id?: string;
          direct_recapture?: { loss_event_id: string; recapture_event_id: string };
        };
      }
  )[];
}
export function continuationComparisonDetail(
  candidate: CommentaryCandidate,
): CommentaryContinuationComparisonDetail | undefined {
  return candidate.kind === 'comparison' && candidate.detail.type === 'line_continuation_comparison'
    ? (candidate.detail as unknown as CommentaryContinuationComparisonDetail)
    : undefined;
}

export interface CommentaryTargetResponseDetail {
  type: 'target_response_comparison';
  scope: 'observed_lines';
  response_inquiry_id: string;
  original_target: CommentaryInquiryPiece;
  branches: {
    line_id: string;
    reply_move: Uci;
    reply_occurrence_id: string;
    position_id: string;
    target_state: 'present' | 'captured' | 'unknown';
    target: CommentaryInquiryPiece | null;
    providers: {
      state: 'complete' | 'unknown' | 'not_applicable';
      pieces: CommentaryTrackedPiece[];
      source_ids: string[];
    };
    capturer: CommentaryInquiryPiece | null;
    recaptures: {
      state: 'complete' | 'unknown' | 'not_applicable';
      moves: { move_uci: Uci; source_id: string; actor: CommentaryInquiryPiece }[];
    };
    observed_recapture: {
      state: 'complete' | 'unknown' | 'not_observed';
      occurrence_id: string | null;
      actor: CommentaryTrackedPiece | null;
      supported_targets: CommentaryTrackedPiece[];
      source_ids: string[];
    };
  }[];
  text: string;
}
export const targetResponseDetail = (
  candidate: CommentaryCandidate,
): CommentaryTargetResponseDetail | undefined =>
  candidate.kind === 'comparison' && candidate.detail.type === 'target_response_comparison'
    ? (candidate.detail as unknown as CommentaryTargetResponseDetail)
    : undefined;
export interface CommentaryPvComparisonDetail {
  type: 'pv_comparison';
  scope: 'observed_lines';
  answer: CommentaryInquiryAnswer;
  material_relation: 'equal' | 'different' | 'unavailable';
  event_sequence_relation: 'same' | 'different' | 'unavailable';
  root_observation_relation: 'same' | 'different' | 'unavailable';
  root_difference_occurrence_ids: string[];
  root_participants: CommentaryInquiryPiece[];
  quality_band: 'close' | 'separated';
  text: string;
}

// Only call on an admitted selection candidate; decoding below verifies every field and reference.
export const pvComparisonDetail = (
  candidate: CommentaryCandidate,
): CommentaryPvComparisonDetail | undefined =>
  candidate.kind === 'comparison' && candidate.detail.type === 'pv_comparison'
    ? (candidate.detail as unknown as CommentaryPvComparisonDetail)
    : undefined;

export interface CommentaryQuestionComparisonDetail {
  type: 'question_comparison';
  scope: 'same_position_pair';
  prefix_moves_uci: Uci[];
  reference_move: Uci;
  alternative_move: Uci;
  perspective: Color;
  question_refs: { proposal_id: string; requirement_id: string }[];
  evaluation_relation: 'better' | 'equal' | 'worse' | null;
  stop_reason: string | null;
  text: string;
}

export const questionComparisonDetail = (
  candidate: CommentaryCandidate,
): CommentaryQuestionComparisonDetail | undefined =>
  candidate.kind === 'comparison' && candidate.detail.type === 'question_comparison'
    ? (candidate.detail as unknown as CommentaryQuestionComparisonDetail)
    : undefined;

export interface CommentaryInquiry {
  id: string;
  scope: 'root_choice';
  source: 'automatic_observation';
  root_occurrence_id: string;
  anchor_occurrence_ids: string[];
  ordering_source_ids: string[];
  position_id: string;
  reference_move: Uci;
  alternative_move: Uci;
  reasons: CommentaryInquiryReason[];
  state: 'deferred' | 'awaiting_engine' | 'answered' | 'stopped';
  stop_reason: string | null;
  answer: CommentaryInquiryAnswer | null;
  assessment_effect?: 'unchanged' | 'refreshed' | 'needs_refresh';
  ordering_candidate_ids?: string[];
  explanation_candidate_ids?: string[];
  next_evidence?: 'inspect_existing' | 'compare_roots' | 'unresolved';
}
export interface CommentaryInquiryContext {
  rootFen: FEN;
  playedMove: Uci;
  engineProfile: string;
}

const object = (v: unknown): v is Record<string, unknown> =>
  !!v && typeof v === 'object' && !Array.isArray(v);
const id = (v: unknown): v is string => typeof v === 'string' && v.length > 0 && v.length <= 1024;
const ids = (v: unknown): v is string[] =>
  Array.isArray(v) && v.length <= 10000 && v.every(id) && new Set(v).size === v.length;
const state = (v: unknown): v is CommentaryState =>
  v === 'ready' || v === 'partial' || v === 'needs_explanation';
const exactKeys = (v: Record<string, unknown>, required: string[], optional: string[] = []): boolean =>
  required.every(k => k in v) && Object.keys(v).every(k => required.includes(k) || optional.includes(k));
const integer = (v: unknown, min: number, max: number): v is number =>
  Number.isSafeInteger(v) && (v as number) >= min && (v as number) <= max;
const evaluationSource = (v: unknown): v is EvaluationSource =>
  [
    'direct_input',
    'root_search',
    'focus_comparison',
    'investigation_comparison',
    'response_comparison',
  ].includes(String(v));

export function decodeCommentaryEvaluation(
  value: unknown,
  moveCount: number,
): CommentaryEvaluation | undefined {
  if (!object(value) || !evaluationSource(value.source)) return;
  if (value.kind === 'engine_search') {
    if (
      !exactKeys(value, ['source', 'kind', 'depth', 'white_score']) ||
      !integer(value.depth, 1, 100) ||
      !object(value.white_score) ||
      !exactKeys(value.white_score, ['kind', 'value']) ||
      !['cp', 'mate'].includes(String(value.white_score.kind)) ||
      !integer(value.white_score.value, -2147483648, 2147483647) ||
      (value.white_score.kind === 'mate' && value.white_score.value === 0)
    )
      return;
    return {
      source: value.source,
      kind: value.kind,
      depth: value.depth,
      white_score: { kind: value.white_score.kind as 'cp' | 'mate', value: value.white_score.value },
    };
  }
  if (
    value.kind !== 'exact_automatic_terminal' ||
    moveCount !== 1 ||
    !exactKeys(value, ['source', 'kind', 'terminal']) ||
    !object(value.terminal)
  )
    return;
  const terminal = value.terminal;
  if (terminal.kind === 'checkmate') {
    if (!exactKeys(terminal, ['kind', 'winner']) || !['white', 'black'].includes(String(terminal.winner)))
      return;
    return {
      source: value.source,
      kind: value.kind,
      terminal: { kind: 'checkmate', winner: terminal.winner as Color },
    };
  }
  if (
    !exactKeys(terminal, ['kind']) ||
    !['stalemate', 'insufficient_material', 'fivefold_repetition', 'seventy_five_move_rule'].includes(
      String(terminal.kind),
    )
  )
    return;
  return {
    source: value.source,
    kind: value.kind,
    terminal: {
      kind: terminal.kind as
        | 'stalemate'
        | 'insufficient_material'
        | 'fivefold_repetition'
        | 'seventy_five_move_rule',
    },
  };
}

const inquiryStops = [
  'engine_unavailable',
  'browser_unsupported',
  'deadline_exceeded',
  'cancelled',
  'search_budget_exhausted',
  'engine_execution_failed',
  'invalid_engine_work_report',
  'review_construction_failed',
  'move_review_preparation_failed',
  'repetition_history_unavailable',
  'reference_changed',
  'investigation_limit',
];

// Mirrors CandidateLineEvaluation.rankingScoreForMover solely to check the reported relation.
// This neither runs an engine nor authenticates its execution receipt.
function rankingScore(evaluation: CommentaryEvaluation, mover: Color): number {
  if (evaluation.kind === 'exact_automatic_terminal')
    return evaluation.terminal.kind === 'checkmate' ? (evaluation.terminal.winner === mover ? 101 : -1) : 50;
  const score = evaluation.white_score;
  const white =
    score.kind === 'mate'
      ? score.value > 0
        ? 100
        : 0
      : 50 + 50 * (2 / (1 + Math.exp(-0.00368208 * score.value)) - 1);
  const mate = score.value * (mover === 'white' ? 1 : -1);
  return (
    (mover === 'white' ? white : 100 - white) +
    (score.kind === 'mate' ? Math.sign(mate) / Math.max(Math.abs(mate), 1) : 0)
  );
}

function decodeComparisonAnswer(
  value: unknown,
  observations: MoveReviewObservations,
  context: CommentaryInquiryContext | undefined,
  positionId: string,
  referenceMove: Uci,
  alternativeMove: Uci,
  child?: { rootMove: Uci; rootOccurrenceId: string; prefixMoves?: Uci[]; scopeFields?: boolean },
): CommentaryInquiryAnswer | undefined {
  if (!context || !isMoveReviewEngineProfile(context.engineProfile)) return;
  const positionFen = child
    ? observations.positions?.find(position => position.id === positionId)?.fen
    : context.rootFen;
  if (!positionFen) return;
  const root = setupPosition('chess', parseFen(positionFen).unwrap()).unwrap();
  const offset = child ? (child.prefixMoves?.length ?? 1) : 0;
  const scopeFields = child && child.scopeFields !== false;
  const paths = new Map(observations.paths.map(path => [path.id, path]));
  const a = value;
  if (
    !object(a) ||
    !exactKeys(a, [
      'work_id',
      'execution_key_sha256',
      'engine_profile',
      'required_depth',
      'reference_depth',
      'alternative_depth',
      'evaluation_relation',
      'reference_line_id',
      'alternative_line_id',
      'reference_occurrence_id',
      'alternative_occurrence_id',
      'reference_evaluation',
      'alternative_evaluation',
      ...(scopeFields ? ['search_position_id', 'ply_offset'] : []),
    ]) ||
    typeof a.work_id !== 'string' ||
    !/^work:[0-9]+$/.test(a.work_id) ||
    typeof a.execution_key_sha256 !== 'string' ||
    !/^[a-f0-9]{64}$/.test(a.execution_key_sha256) ||
    a.engine_profile !== context.engineProfile ||
    !integer(a.required_depth, 1, 100) ||
    !id(a.reference_line_id) ||
    !id(a.alternative_line_id) ||
    a.reference_line_id === a.alternative_line_id ||
    (scopeFields && (a.search_position_id !== positionId || a.ply_offset !== offset))
  )
    return;
  const left = paths.get(a.reference_line_id),
    right = paths.get(a.alternative_line_id);
  if (
    !left ||
    !right ||
    [left, right].some(
      path =>
        path.startFen !== context.rootFen ||
        path.positionIds?.[offset] !== positionId ||
        path.kind === 'history' ||
        (child &&
          (path.moves[0]?.uci !== child.rootMove ||
            path.occurrenceIds[0] !== child.rootOccurrenceId ||
            path.moves[offset - 1]?.fenAfter !== positionFen ||
            child.prefixMoves?.some((move, index) => path.moves[index]?.uci !== move))),
    ) ||
    left.moves[offset]?.uci !== referenceMove ||
    right.moves[offset]?.uci !== alternativeMove ||
    a.reference_occurrence_id !== left.occurrenceIds[offset] ||
    a.alternative_occurrence_id !== right.occurrenceIds[offset]
  )
    return;
  const le = decodeCommentaryEvaluation(a.reference_evaluation, left.moves.length - offset);
  const re = decodeCommentaryEvaluation(a.alternative_evaluation, right.moves.length - offset);
  const ownsEvaluation = (path: typeof left, evaluation: CommentaryEvaluation) =>
    child
      ? evaluation.source === 'response_comparison' &&
        path.conditionalEvaluations?.some(
          entry =>
            entry.searchPositionId === positionId &&
            entry.plyOffset === offset &&
            JSON.stringify(entry.evaluation) === JSON.stringify(evaluation),
        )
      : evaluation.source !== 'response_comparison' &&
        path.evaluations?.some(entry => JSON.stringify(entry) === JSON.stringify(evaluation));
  if (
    !le ||
    !re ||
    le.source !== re.source ||
    le.source === 'direct_input' ||
    !ownsEvaluation(left, le) ||
    !ownsEvaluation(right, re)
  )
    return;
  const ld = le.kind === 'engine_search' ? le.depth : null;
  const rd = re.kind === 'engine_search' ? re.depth : null;
  const requiredDepth = a.required_depth;
  if (
    a.reference_depth !== ld ||
    a.alternative_depth !== rd ||
    (ld !== null && rd !== null && ld !== rd) ||
    [le, re].some(
      e =>
        e.kind === 'engine_search' &&
        (e.depth < requiredDepth || (e.white_score.kind !== 'mate' && e.depth < 8)),
    )
  )
    return;
  const difference = rankingScore(re, root.turn) - rankingScore(le, root.turn);
  if (a.evaluation_relation !== (difference > 0 ? 'better' : difference < 0 ? 'worse' : 'equal')) return;
  return {
    ...a,
    reference_evaluation: le,
    alternative_evaluation: re,
  } as unknown as CommentaryInquiryAnswer;
}

type ObservationPath = MoveReviewObservations['paths'][number];
const samePiece = (left: CommentaryInquiryPiece, right: CommentaryInquiryPiece): boolean =>
  left.position_id === right.position_id &&
  left.side === right.side &&
  left.role === right.role &&
  left.square === right.square;

// Identity is local to this exact root path. Promotions and castling move the same witnesses.
function trackedPieces(path: ObservationPath, ply: number): CommentaryTrackedPiece[] | undefined {
  const rootId = path.positionIds?.[0],
    positionId = path.positionIds?.[ply];
  if (!rootId || !positionId || ply > path.moves.length) return;
  const position = setupPosition('chess', parseFen(path.startFen).unwrap()).unwrap();
  const origins = new Map(
    [...position.board].map(([square, piece]) => [
      square,
      {
        position_id: rootId,
        side: piece.color,
        role: piece.role,
        square: makeSquare(square),
      } as CommentaryInquiryPiece,
    ]),
  );
  for (const entry of path.moves.slice(0, ply)) {
    const move = parseUci(entry.uci);
    if (!move || !('from' in move) || !position.isLegal(move)) return;
    const original = origins.get(move.from),
      piece = position.board.get(move.from)!;
    if (!original) return;
    const castle = castlingSide(position, move);
    origins.delete(move.from);
    if (castle) {
      const rookFrom = position.castles.rook[position.turn][castle];
      const rook = rookFrom === undefined ? undefined : origins.get(rookFrom);
      if (rookFrom === undefined || !rook) return;
      origins.delete(rookFrom);
      origins.set(kingCastlesTo(position.turn, castle), original);
      origins.set(rookCastlesTo(position.turn, castle), rook);
    } else {
      if (piece.role === 'pawn' && move.to === position.epSquare)
        origins.delete(move.to + (position.turn === 'white' ? -8 : 8));
      origins.delete(move.to);
      origins.set(move.to, original);
    }
    position.play(move);
  }
  return [...position.board].map(([square, piece]) => ({
    original: origins.get(square)!,
    current: {
      position_id: positionId,
      side: piece.color,
      role: piece.role,
      square: makeSquare(square),
    },
  }));
}

function decodeRootSearches(
  value: unknown,
  observations: MoveReviewObservations,
  context: CommentaryInquiryContext | undefined,
): CommentaryRootSearch[] | undefined {
  if (!Array.isArray(value) || value.length > 10000 || !context) return;
  const result: CommentaryRootSearch[] = [];
  for (const raw of value) {
    if (
      !object(raw) ||
      !exactKeys(raw, [
        'work_id',
        'execution_key_sha256',
        'engine_profile',
        'required_depth',
        'multi_pv',
        'purpose',
        'root_position_id',
        'root_occurrence_id',
        'root_restriction',
        'state',
        'stop_reason',
        'candidates',
      ]) ||
      typeof raw.work_id !== 'string' ||
      !/^work:[0-9]+$/.test(raw.work_id) ||
      typeof raw.execution_key_sha256 !== 'string' ||
      !/^[a-f0-9]{64}$/.test(raw.execution_key_sha256) ||
      raw.engine_profile !== context.engineProfile ||
      !integer(raw.required_depth, 1, 100) ||
      !integer(raw.multi_pv, 1, 3) ||
      !['root_search', 'focus_comparison', 'investigation_comparison'].includes(String(raw.purpose)) ||
      !id(raw.root_position_id) ||
      !id(raw.root_occurrence_id) ||
      !object(raw.root_restriction) ||
      !['awaiting_engine', 'completed', 'stopped'].includes(String(raw.state)) ||
      (raw.state === 'stopped'
        ? !inquiryStops.includes(String(raw.stop_reason))
        : raw.stop_reason !== null) ||
      !Array.isArray(raw.candidates) ||
      raw.candidates.length > raw.multi_pv ||
      (raw.state === 'awaiting_engine' && raw.candidates.length !== 0) ||
      (raw.state === 'completed' && raw.candidates.length === 0)
    )
      return;
    const root = observations.positions?.find(position => position.id === raw.root_position_id);
    if (
      !root?.legal_moves.complete ||
      root.fen !== context.rootFen ||
      !observations.paths.some(
        path =>
          path.kind !== 'history' &&
          path.startFen === context.rootFen &&
          path.positionIds?.[0] === root.id &&
          path.moves[0]?.uci === context.playedMove &&
          path.occurrenceIds[0] === raw.root_occurrence_id,
      )
    )
      return;
    const legal = root.legal_moves.moves.map(move => move.uci),
      restriction = raw.root_restriction;
    if (restriction.kind === 'unrestricted') {
      if (!exactKeys(restriction, ['kind']) || raw.multi_pv > legal.length) return;
    } else if (
      restriction.kind !== 'restricted' ||
      !exactKeys(restriction, ['kind', 'moves_uci']) ||
      !ids(restriction.moves_uci) ||
      !restriction.moves_uci.length ||
      restriction.moves_uci.length > 218 ||
      !restriction.moves_uci.every(move => legal.includes(move)) ||
      raw.multi_pv > restriction.moves_uci.length
    )
      return;
    for (const candidate of raw.candidates) {
      if (
        !object(candidate) ||
        !exactKeys(candidate, ['move_uci', 'line_id', 'occurrence_id']) ||
        !id(candidate.move_uci) ||
        !legal.includes(candidate.move_uci) ||
        (restriction.kind === 'restricted' &&
          !(restriction.moves_uci as string[]).includes(candidate.move_uci))
      )
        return;
      if (candidate.line_id === null && candidate.occurrence_id === null && raw.state === 'stopped') continue;
      if (!id(candidate.line_id) || !id(candidate.occurrence_id)) return;
      const path = observations.paths.find(path => path.id === candidate.line_id);
      if (
        !path ||
        path.kind === 'history' ||
        path.startFen !== context.rootFen ||
        path.positionIds?.[0] !== root.id ||
        path.moves[0]?.uci !== candidate.move_uci ||
        path.occurrenceIds[0] !== candidate.occurrence_id ||
        !legal.includes(candidate.move_uci) ||
        (restriction.kind === 'restricted' &&
          !(restriction.moves_uci as string[]).includes(candidate.move_uci)) ||
        !path.evaluations?.some(evaluation => evaluation.source === raw.purpose)
      )
        return;
    }
    if (new Set(raw.candidates.map(candidate => candidate.move_uci)).size !== raw.candidates.length) return;
    result.push(raw as unknown as CommentaryRootSearch);
  }
  if (
    new Set(result.map(search => search.work_id)).size !== result.length ||
    new Set(result.map(search => search.execution_key_sha256)).size !== result.length
  )
    return;
  return result;
}

function decodeResponseSearches(
  value: unknown,
  observations: MoveReviewObservations,
  context: CommentaryInquiryContext | undefined,
): CommentaryResponseSearch[] | undefined {
  // Two explorations may be followed by the producer's one conflict recheck.
  if (!Array.isArray(value) || value.length > 3 || !context) return;
  const result: CommentaryResponseSearch[] = [];
  for (const raw of value) {
    if (
      !object(raw) ||
      !exactKeys(
        raw,
        [
          'work_id',
          'execution_key_sha256',
          'engine_profile',
          'required_depth',
          'multi_pv',
          'root_move',
          'root_move_occurrence_id',
          'child_position_id',
          'root_restriction',
          'candidates',
        ],
        ['prefix_moves_uci', 'ply_offset'],
      ) ||
      typeof raw.work_id !== 'string' ||
      !/^work:[0-9]+$/.test(raw.work_id) ||
      typeof raw.execution_key_sha256 !== 'string' ||
      !/^[a-f0-9]{64}$/.test(raw.execution_key_sha256) ||
      raw.engine_profile !== context.engineProfile ||
      !integer(raw.required_depth, 1, 100) ||
      !integer(raw.multi_pv, 1, 3) ||
      !id(raw.root_move) ||
      !id(raw.root_move_occurrence_id) ||
      !id(raw.child_position_id) ||
      !object(raw.root_restriction) ||
      !Array.isArray(raw.candidates) ||
      raw.candidates.length !== raw.multi_pv
    )
      return;
    const prefix = raw.prefix_moves_uci === undefined ? [raw.root_move] : raw.prefix_moves_uci;
    if (
      !Array.isArray(prefix) ||
      !prefix.length ||
      prefix.length > 79 ||
      !prefix.every(move => typeof move === 'string' && /^[a-h][1-8][a-h][1-8][qrbn]?$/.test(move)) ||
      (raw.prefix_moves_uci === undefined) !== (raw.ply_offset === undefined) ||
      (raw.ply_offset !== undefined && raw.ply_offset !== prefix.length) ||
      !commentarySearchAnchorPath(
        observations,
        context.rootFen,
        raw.root_move,
        raw.root_move_occurrence_id,
        raw.child_position_id,
        prefix,
      )
    )
      return;
    const offset = prefix.length;
    const child = observations.positions?.find(position => position.id === raw.child_position_id);
    if (!child?.legal_moves.complete) return;
    const restriction = raw.root_restriction;
    const legal = child.legal_moves.moves.map(move => move.uci);
    if (restriction.kind === 'unrestricted') {
      if (!exactKeys(restriction, ['kind']) || raw.multi_pv !== Math.min(3, legal.length)) return;
    } else if (
      restriction.kind !== 'restricted' ||
      !exactKeys(restriction, ['kind', 'moves_uci']) ||
      !ids(restriction.moves_uci) ||
      !restriction.moves_uci.length ||
      restriction.moves_uci.length > 218 ||
      !restriction.moves_uci.every(move => legal.includes(move)) ||
      raw.multi_pv !== Math.min(3, restriction.moves_uci.length)
    )
      return;
    const mover = setupPosition('chess', parseFen(child.fen).unwrap()).unwrap().turn;
    const candidates: CommentaryResponseSearch['candidates'] = [];
    for (const candidate of raw.candidates) {
      if (
        !object(candidate) ||
        !exactKeys(candidate, ['move_uci', 'line_id', 'occurrence_id', 'evaluation']) ||
        !id(candidate.move_uci) ||
        !id(candidate.line_id) ||
        !id(candidate.occurrence_id)
      )
        return;
      const path = observations.paths.find(path => path.id === candidate.line_id);
      if (
        !path ||
        path.kind === 'history' ||
        path.startFen !== context.rootFen ||
        path.moves[0]?.uci !== raw.root_move ||
        path.occurrenceIds[0] !== raw.root_move_occurrence_id ||
        !prefix.every((move, index) => path.moves[index]?.uci === move) ||
        path.positionIds?.[offset] !== child.id ||
        path.moves[offset - 1]?.fenAfter !== child.fen ||
        path.moves[offset]?.uci !== candidate.move_uci ||
        path.occurrenceIds[offset] !== candidate.occurrence_id ||
        !legal.includes(candidate.move_uci) ||
        (restriction.kind === 'restricted' &&
          !(restriction.moves_uci as string[]).includes(candidate.move_uci))
      )
        return;
      const evaluation = decodeCommentaryEvaluation(candidate.evaluation, path.moves.length - offset);
      if (
        !evaluation ||
        evaluation.source !== 'response_comparison' ||
        (evaluation.kind === 'engine_search' &&
          (evaluation.depth < raw.required_depth ||
            (evaluation.white_score.kind !== 'mate' && evaluation.depth < 8))) ||
        !path.conditionalEvaluations?.some(
          entry =>
            entry.searchPositionId === child.id &&
            entry.plyOffset === offset &&
            JSON.stringify(entry.evaluation) === JSON.stringify(evaluation),
        )
      )
        return;
      candidates.push({ ...candidate, evaluation } as CommentaryResponseSearch['candidates'][number]);
    }
    if (
      new Set(candidates.map(candidate => candidate.move_uci)).size !== candidates.length ||
      new Set(
        candidates.flatMap(candidate =>
          candidate.evaluation.kind === 'engine_search' ? [candidate.evaluation.depth] : [],
        ),
      ).size > 1 ||
      candidates.some((candidate, index) => {
        if (!index) return false;
        const previous = candidates[index - 1];
        const difference =
          rankingScore(previous.evaluation, mover) - rankingScore(candidate.evaluation, mover);
        return difference < 0 || (difference === 0 && previous.move_uci > candidate.move_uci);
      })
    )
      return;
    result.push({ ...raw, candidates } as unknown as CommentaryResponseSearch);
  }
  if (
    new Set(result.map(search => search.work_id)).size !== result.length ||
    new Set(result.map(search => search.execution_key_sha256)).size !== result.length
  )
    return;
  return result;
}

function decodeResponseInquiries(
  value: unknown,
  observations: MoveReviewObservations,
  context: CommentaryInquiryContext | undefined,
  searches: CommentaryResponseSearch[] | undefined,
): CommentaryResponseInquiry[] | undefined {
  if (!Array.isArray(value) || value.length > 128 || !context) return;
  const result: CommentaryResponseInquiry[] = [];
  const root = setupPosition('chess', parseFen(context.rootFen).unwrap()).unwrap();
  for (const raw of value) {
    if (
      !object(raw) ||
      !exactKeys(
        raw,
        [
          'id',
          'scope',
          'source',
          'root_occurrence_id',
          'root_move',
          'root_move_occurrence_id',
          'child_position_id',
          'original_target',
          'related_inquiry_ids',
          'target',
          'pressure_source_ids',
          'capture_moves',
          'decline_moves',
          'move_source_ids',
          'reference_move',
          'alternative_move',
          'state',
          'stop_reason',
          'answer',
        ],
        ['reference_selection', 'alternative_selection'],
      ) ||
      !id(raw.id) ||
      raw.scope !== 'target_response' ||
      raw.source !== 'automatic_observation' ||
      !id(raw.root_occurrence_id) ||
      !id(raw.root_move_occurrence_id) ||
      !id(raw.child_position_id) ||
      !id(raw.root_move) ||
      !id(raw.reference_move) ||
      !id(raw.alternative_move) ||
      !ids(raw.related_inquiry_ids) ||
      raw.related_inquiry_ids.length > 1 ||
      !ids(raw.pressure_source_ids) ||
      !raw.pressure_source_ids.length ||
      !ids(raw.capture_moves) ||
      !raw.capture_moves.length ||
      !ids(raw.decline_moves) ||
      !raw.decline_moves.length ||
      !object(raw.move_source_ids) ||
      !['deferred', 'awaiting_engine', 'answered', 'observed', 'stopped'].includes(String(raw.state)) ||
      (raw.stop_reason !== null && !inquiryStops.includes(String(raw.stop_reason))) ||
      (raw.state === 'answered') !== (raw.answer !== null) ||
      (raw.state === 'answered' && raw.stop_reason !== null) ||
      (raw.state === 'observed' && raw.stop_reason !== null) ||
      (raw.state === 'awaiting_engine' && raw.stop_reason !== null) ||
      (raw.state === 'deferred' && raw.stop_reason !== null && raw.stop_reason !== 'investigation_limit') ||
      (raw.state === 'stopped' && raw.stop_reason === null)
    )
      return;
    const anchor = observations.paths.find(
      path =>
        path.kind !== 'history' &&
        path.startFen === context.rootFen &&
        path.moves[0]?.uci === raw.root_move &&
        path.occurrenceIds[0] === raw.root_move_occurrence_id &&
        path.positionIds?.[1] === raw.child_position_id,
    );
    if (!anchor) return;
    const position = observations.positions?.find(position => position.id === anchor.positionIds?.[0]);
    const child = observations.positions?.find(position => position.id === raw.child_position_id);
    if (
      !position ||
      position.fen !== context.rootFen ||
      !child ||
      child.fen !== anchor.moves[0].fenAfter ||
      !child.legal_moves.complete ||
      !observations.paths.some(
        path =>
          path.kind !== 'history' &&
          path.startFen === context.rootFen &&
          path.positionIds?.[0] === position.id &&
          path.occurrenceIds[0] === raw.root_occurrence_id &&
          path.moves[0]?.uci === context.playedMove,
      )
    )
      return;
    const original = raw.original_target,
      target = raw.target;
    if (
      !object(original) ||
      !exactKeys(original, ['position_id', 'side', 'role', 'square']) ||
      original.position_id !== position.id ||
      original.side !== root.turn ||
      original.role === 'king' ||
      typeof original.square !== 'string'
    )
      return;
    const square = parseSquare(original.square),
      occupant = square === undefined ? undefined : root.board.get(square);
    if (
      !occupant ||
      occupant.color !== original.side ||
      occupant.role !== original.role ||
      !object(target) ||
      !exactKeys(target, ['position_id', 'side', 'role', 'square'])
    )
      return;
    const tracked = trackedPieces(anchor, 1)?.find(piece =>
      samePiece(piece.original, original as unknown as CommentaryInquiryPiece),
    );
    if (!tracked || !samePiece(tracked.current, target as unknown as CommentaryInquiryPiece)) return;
    if (
      !raw.pressure_source_ids.every(source =>
        position.controls.pieces.some(
          control =>
            control.piece.side !== original.side &&
            control.source_ids.some(
              (entry, index) => entry === source && control.squares[index] === original.square,
            ),
        ) || child.controls.pieces.some(
          control => control.piece.side !== target.side && control.source_ids.some(
            (entry, index) => entry === source && control.squares[index] === target.square,
          ),
        ),
      )
    )
      return;
    const captures = child.legal_moves.moves.filter(move => move.captured_square === target.square);
    const declines = child.legal_moves.moves.filter(move => move.captured_square !== target.square);
    if (
      !sameIds(
        raw.capture_moves,
        captures.map(move => move.uci),
      ) ||
      !sameIds(
        raw.decline_moves,
        declines.map(move => move.uci),
      ) ||
      !raw.capture_moves.includes(raw.reference_move) ||
      !raw.decline_moves.includes(raw.alternative_move) ||
      !sameIds(
        Object.keys(raw.move_source_ids),
        child.legal_moves.moves.map(move => move.uci),
      ) ||
      !child.legal_moves.moves.every(
        move => (raw.move_source_ids as Record<string, unknown>)[move.uci] === move.source_id,
      )
    )
      return;
    const answer =
      raw.answer === null
        ? null
        : decodeComparisonAnswer(
            raw.answer,
            observations,
            context,
            child.id,
            raw.reference_move,
            raw.alternative_move,
            { rootMove: raw.root_move, rootOccurrenceId: raw.root_move_occurrence_id },
          );
    if (answer === undefined) return;
    const selections = ['reference_selection', 'alternative_selection'] as const;
    if (selections.some(key => key in raw)) {
      if (!selections.every(key => key in raw)) return;
      for (const [index, key] of selections.entries()) {
        const selection = raw[key];
        const move = index === 0 ? raw.reference_move : raw.alternative_move;
        const family = index === 0 ? raw.capture_moves : raw.decline_moves;
        if (!object(selection) || !exactKeys(selection, ['source', 'work_id', 'line_id'])) return;
        if (selection.source === 'legal_seed') {
          if (selection.work_id !== null || selection.line_id !== null) return;
          continue;
        }
        const path = observations.paths.find(path => path.id === selection.line_id);
        if (
          !path ||
          path.kind === 'history' ||
          path.startFen !== context.rootFen ||
          path.moves[0]?.uci !== raw.root_move ||
          path.occurrenceIds[0] !== raw.root_move_occurrence_id ||
          path.positionIds?.[1] !== child.id ||
          path.moves[1]?.uci !== move
        )
          return;
        if (selection.source === 'child_search') {
          const ranked = searches
            ?.filter(
              search =>
                responseSearchPrefix(search).length === 1 &&
                search.root_move === raw.root_move &&
                search.root_move_occurrence_id === raw.root_move_occurrence_id &&
                search.child_position_id === child.id &&
                search.candidates.some(candidate => family.includes(candidate.move_uci)),
            )
            .sort((left, right) => {
              const covers = (search: CommentaryResponseSearch) =>
                search.root_restriction.kind === 'unrestricted' ||
                family.every(
                  move =>
                    search.root_restriction.kind === 'restricted' &&
                    search.root_restriction.moves_uci.includes(move),
                );
              return (
                Number(covers(right)) - Number(covers(left)) ||
                Number(right.work_id.slice(5)) - Number(left.work_id.slice(5))
              );
            });
          const search = ranked?.[0];
          const best = search?.candidates.find(candidate => family.includes(candidate.move_uci));
          if (
            search?.work_id !== selection.work_id ||
            best?.move_uci !== move ||
            best.line_id !== selection.line_id
          )
            return;
        } else if (
          selection.work_id !== null ||
          !['root_pv_hint', 'observed_line'].includes(String(selection.source)) ||
          (selection.source === 'root_pv_hint' &&
            !path.evaluations?.some(evaluation => evaluation.source !== 'direct_input'))
        )
          return;
      }
    }
    if (answer && searches !== undefined) {
      const search = searches.find(search => search.work_id === answer.work_id);
      if (
        !search ||
        responseSearchPrefix(search).length !== 1 ||
        search.execution_key_sha256 !== answer.execution_key_sha256 ||
        search.engine_profile !== answer.engine_profile ||
        search.required_depth !== answer.required_depth ||
        search.child_position_id !== child.id ||
        search.root_move !== raw.root_move ||
        ![
          [answer.reference_line_id, answer.reference_evaluation],
          [answer.alternative_line_id, answer.alternative_evaluation],
        ].every(([line, evaluation]) =>
          search.candidates.some(
            candidate =>
              candidate.line_id === line &&
              JSON.stringify(candidate.evaluation) === JSON.stringify(evaluation),
          ),
        )
      )
        return;
    }
    result.push({ ...raw, answer } as unknown as CommentaryResponseInquiry);
  }
  if (
    new Set(result.map(inquiry => inquiry.id)).size !== result.length ||
    new Set(result.map(inquiry => `${inquiry.root_move_occurrence_id}:${inquiry.target.square}`)).size !==
      result.length
  )
    return;
  for (const inquiry of result) {
    const peers = result.filter(
      peer =>
        peer.root_occurrence_id === inquiry.root_occurrence_id &&
        peer.root_move !== inquiry.root_move &&
        samePiece(peer.original_target, inquiry.original_target),
    );
    if (
      !sameIds(
        inquiry.related_inquiry_ids,
        peers.map(peer => peer.id),
      ) ||
      !peers.every(peer => peer.related_inquiry_ids.includes(inquiry.id))
    )
      return;
  }
  return result;
}

function routeChallengeShape(value: unknown): boolean {
  if (value === undefined || value === null) return true;
  if (
    !object(value) ||
    !exactKeys(value, [
      'state',
      'stop_reason',
      'reference_work_id',
      'reference_move',
      'reference_line_id',
      'move_classes',
    ]) ||
    !['not_returned', 'awaiting_engine', 'sampled', 'compared', 'stopped'].includes(String(value.state)) ||
    !id(value.reference_work_id) ||
    !id(value.reference_move) ||
    !id(value.reference_line_id) ||
    (value.stop_reason !== null && !inquiryStops.includes(String(value.stop_reason))) ||
    (value.state === 'stopped' && value.stop_reason === null) ||
    (value.stop_reason !== null &&
      value.state !== 'stopped' &&
      !(
        ['sampled', 'not_returned'].includes(String(value.state)) &&
        value.stop_reason === 'investigation_limit'
      )) ||
    !Array.isArray(value.move_classes) ||
    !value.move_classes.length ||
    value.move_classes.length > 10000
  )
    return false;
  return value.move_classes.every(
    group =>
      object(group) &&
      exactKeys(
        group,
        ['moves_uci', 'premise_occurrence_ids', 'source_line_ids', 'sampled_work_ids', 'comparison'],
        ['kind'],
      ) &&
      (group.kind === undefined ||
        ['recovery_route', 'replacement_recapture', 'exposed_capture'].includes(String(group.kind))) &&
      ids(group.moves_uci) &&
      group.moves_uci.length > 0 &&
      ids(group.premise_occurrence_ids) &&
      group.premise_occurrence_ids.length > 0 &&
      ids(group.source_line_ids) &&
      group.source_line_ids.length > 0 &&
      ids(group.sampled_work_ids) &&
      (group.comparison === null ||
        (object(group.comparison) &&
          exactKeys(group.comparison, ['work_id', 'reference_line_id', 'challenge_line_id']) &&
          id(group.comparison.work_id) &&
          id(group.comparison.reference_line_id) &&
          id(group.comparison.challenge_line_id))),
  );
}

function continuationComparisonSourcesShape(
  value: unknown,
): value is CommentaryContinuationComparisonSource[] {
  return (
    Array.isArray(value) &&
    value.length <= 10000 &&
    value.every(
      source =>
        object(source) &&
        exactKeys(source, ['work_id', 'donor_line_id', 'peer_line_id'], ['kind', 'observation_line_id']) &&
        id(source.work_id) &&
        id(source.donor_line_id) &&
        id(source.peer_line_id) &&
        (source.kind === undefined ||
          ['material_recovery', 'support_application'].includes(String(source.kind))) &&
        ((source.kind ?? 'material_recovery') === 'support_application'
          ? id(source.observation_line_id)
          : source.observation_line_id === undefined || source.observation_line_id === null),
    ) &&
    new Set(value.map(continuationComparisonSourceKey)).size === value.length
  );
}

const continuationComparisonSourceKey = (source: CommentaryContinuationComparisonSource): string =>
  JSON.stringify([
    source.work_id,
    source.donor_line_id,
    source.peer_line_id,
    source.kind ?? 'material_recovery',
    source.observation_line_id ?? null,
  ]);

function continuationPremisesShape(value: unknown): value is CommentaryContinuationPremise[] {
  return (
    Array.isArray(value) &&
    value.length > 0 &&
    value.length <= 10000 &&
    value.every(
      premise =>
        object(premise) &&
        exactKeys(premise, [
          'id',
          'kind',
          'trigger_occurrence_ids',
          'source_line_ids',
          'comparison_source_indices',
          'requirement_id',
          'witness_line_ids',
          'unresolved_source_line_ids',
          'observation_state',
        ]) &&
        id(premise.id) &&
        continuationKinds.includes(premise.kind as CommentaryContinuationPremise['kind']) &&
        ids(premise.trigger_occurrence_ids) &&
        premise.trigger_occurrence_ids.length > 0 &&
        ids(premise.source_line_ids) &&
        premise.source_line_ids.length > 0 &&
        Array.isArray(premise.comparison_source_indices) &&
        premise.comparison_source_indices.length <= 10000 &&
        premise.comparison_source_indices.every(index => Number.isSafeInteger(index) && index >= 0) &&
        new Set(premise.comparison_source_indices).size === premise.comparison_source_indices.length &&
        (premise.requirement_id === null || id(premise.requirement_id)) &&
        ids(premise.witness_line_ids) &&
        ids(premise.unresolved_source_line_ids) &&
        ['observed', 'unobserved', 'unavailable', 'not_applicable'].includes(
          String(premise.observation_state),
        ),
    ) &&
    new Set(value.map(premise => premise.id)).size === value.length &&
    new Set(
      value.map(premise =>
        JSON.stringify([
          premise.kind,
          [...premise.trigger_occurrence_ids].sort(),
          [...premise.comparison_source_indices].sort((a, b) => a - b),
          premise.requirement_id,
        ]),
      ),
    ).size === value.length
  );
}

function decodeContinuationInquiries(
  value: unknown,
  observations: MoveReviewObservations,
  context: CommentaryInquiryContext | undefined,
  searches: CommentaryResponseSearch[],
): CommentaryContinuationInquiry[] | undefined {
  if (!Array.isArray(value) || value.length > 10000 || !context) return;
  const result: CommentaryContinuationInquiry[] = [];
  for (const raw of value) {
    if (
      !object(raw) ||
      !exactKeys(
        raw,
        [
          'id',
          'scope',
          'source',
          'root_occurrence_id',
          'root_position_id',
          'position_id',
          'prefix_moves_uci',
          'prefix_occurrence_ids',
          'trigger_occurrence_ids',
          'observed_moves',
          'source_line_ids',
          'kinds',
          'premises',
          'legal_moves',
          'move_source_ids',
          'state',
          'stop_reason',
          'search_work_ids',
          'explanation_candidate_ids',
        ],
        ['comparison_sources', 'unresolved_source_line_ids', 'followup_question_ids', 'route_challenge'],
      ) ||
      !id(raw.id) ||
      raw.scope !== 'line_continuation' ||
      raw.source !== 'automatic_observation' ||
      !id(raw.root_occurrence_id) ||
      !id(raw.root_position_id) ||
      !id(raw.position_id) ||
      !Array.isArray(raw.prefix_moves_uci) ||
      !raw.prefix_moves_uci.length ||
      raw.prefix_moves_uci.length > 79 ||
      !raw.prefix_moves_uci.every(
        move => typeof move === 'string' && /^[a-h][1-8][a-h][1-8][qrbn]?$/.test(move),
      ) ||
      !ids(raw.prefix_occurrence_ids) ||
      raw.prefix_occurrence_ids.length !== raw.prefix_moves_uci.length ||
      !ids(raw.trigger_occurrence_ids) ||
      !raw.trigger_occurrence_ids.length ||
      !ids(raw.observed_moves) ||
      !ids(raw.source_line_ids) ||
      !raw.source_line_ids.length ||
      !ids(raw.kinds) ||
      !raw.kinds.length ||
      !ids(raw.legal_moves) ||
      !raw.legal_moves.length ||
      !raw.kinds.every(kind => continuationKinds.includes(kind as CommentaryContinuationPremise['kind'])) ||
      !continuationPremisesShape(raw.premises) ||
      !object(raw.move_source_ids) ||
      !ids(raw.search_work_ids) ||
      !ids(raw.explanation_candidate_ids) ||
      (raw.unresolved_source_line_ids !== undefined && !ids(raw.unresolved_source_line_ids)) ||
      (raw.followup_question_ids !== undefined && !ids(raw.followup_question_ids)) ||
      (raw.unresolved_source_line_ids === undefined) !== (raw.followup_question_ids === undefined) ||
      !routeChallengeShape(raw.route_challenge) ||
      !['deferred', 'awaiting_engine', 'surveyed', 'stopped', 'forced', 'observed'].includes(
        String(raw.state),
      ) ||
      (raw.stop_reason !== null && !inquiryStops.includes(String(raw.stop_reason))) ||
      (['awaiting_engine', 'surveyed', 'forced', 'observed'].includes(String(raw.state)) &&
        raw.stop_reason !== null) ||
      (raw.state === 'stopped' && raw.stop_reason === null) ||
      (raw.state === 'deferred' && raw.stop_reason !== null && raw.stop_reason !== 'investigation_limit')
    )
      return;
    const prefix = raw.prefix_moves_uci as Uci[];
    const path = commentarySearchAnchorPath(
      observations,
      context.rootFen,
      prefix[0],
      raw.prefix_occurrence_ids[0],
      raw.position_id,
      prefix,
    );
    const position = observations.positions?.find(position => position.id === raw.position_id);
    if (
      !path ||
      !position?.legal_moves.complete ||
      path.positionIds?.[0] !== raw.root_position_id ||
      !raw.prefix_occurrence_ids.every((occurrence, index) => path.occurrenceIds[index] === occurrence) ||
      !observations.paths.some(
        actual =>
          actual.kind !== 'history' &&
          actual.startFen === context.rootFen &&
          actual.positionIds?.[0] === raw.root_position_id &&
          actual.moves[0]?.uci === context.playedMove &&
          actual.occurrenceIds[0] === raw.root_occurrence_id,
      ) ||
      !sameIds(
        raw.legal_moves,
        position.legal_moves.moves.map(move => move.uci),
      ) ||
      !sameIds(Object.keys(raw.move_source_ids), raw.legal_moves) ||
      !position.legal_moves.moves.every(
        move => (raw.move_source_ids as Record<string, unknown>)[move.uci] === move.source_id,
      )
    )
      return;
    const prefixIds = raw.prefix_occurrence_ids,
      legalMoves = raw.legal_moves,
      sourceLineIds = raw.source_line_ids;
    const comparisonSources = raw.comparison_sources ?? [];
    if (
      !continuationComparisonSourcesShape(comparisonSources) ||
      !sameIds(
        raw.kinds,
        raw.premises.map(premise => premise.kind),
      ) ||
      !sameIds(
        raw.trigger_occurrence_ids,
        raw.premises.flatMap(premise => premise.trigger_occurrence_ids),
      ) ||
      raw.premises.some(premise =>
        premise.comparison_source_indices.some(index => index >= comparisonSources.length),
      ) ||
      !sameIds(
        comparisonSources.map((_, index) => String(index)),
        raw.premises.flatMap(premise => premise.comparison_source_indices.map(String)),
      ) ||
      raw.premises.some(premise =>
        [...premise.source_line_ids, ...premise.witness_line_ids, ...premise.unresolved_source_line_ids].some(
          line => !sourceLineIds.includes(line),
        ),
      ) ||
      comparisonSources.some(source => (source.kind ?? 'material_recovery') === 'material_recovery') !==
        raw.kinds.includes('continuation_not_observed') ||
      comparisonSources.some(source => source.kind === 'support_application') !==
        raw.kinds.includes('support_application_not_observed')
    )
      return;
    const donorPaths = comparisonSources.flatMap(source =>
      [source.donor_line_id, source.observation_line_id].flatMap(
        id => observations.paths.find(path => path.id === id) ?? [],
      ),
    );
    const sourcePaths = raw.source_line_ids.map(id => observations.paths.find(path => path.id === id));
    if (
      sourcePaths.some(
        source =>
          !source ||
          source.kind === 'history' ||
          source.startFen !== context.rootFen ||
          source.positionIds?.[0] !== raw.root_position_id ||
          !prefix.every((move, index) => source.moves[index]?.uci === move) ||
          !prefixIds.every((occurrence, index) => source.occurrenceIds[index] === occurrence),
      ) ||
      !raw.trigger_occurrence_ids.every(
        occurrence =>
          sourcePaths.some(source => source!.occurrenceIds.includes(occurrence)) ||
          donorPaths.some(source => source?.occurrenceIds.includes(occurrence)),
      ) ||
      !sameIds(
        raw.observed_moves,
        sourcePaths.flatMap(source => source!.moves[prefix.length]?.uci ?? []),
      ) ||
      !raw.observed_moves.every(move => legalMoves.includes(move))
    )
      return;
    const inquiry = raw as unknown as CommentaryContinuationInquiry;
    const matching = searches.filter(search => responseSearchMatchesContinuation(search, inquiry));
    const surveyed = matching.some(
      search =>
        search.root_restriction.kind === 'unrestricted' ||
        legalMoves.every(
          move =>
            search.root_restriction.kind === 'restricted' && search.root_restriction.moves_uci.includes(move),
        ),
    );
    if (
      !sameIds(
        raw.search_work_ids,
        matching.map(search => search.work_id),
      ) ||
      (raw.state === 'surveyed') !== surveyed ||
      (raw.state === 'forced' && raw.legal_moves.length !== 1) ||
      (raw.state === 'awaiting_engine' && raw.legal_moves.length < 2)
    )
      return;
    result.push(inquiry);
  }
  if (
    new Set(result.map(inquiry => inquiry.id)).size !== result.length ||
    new Set(result.map(inquiry => JSON.stringify(inquiry.prefix_occurrence_ids))).size !== result.length
  )
    return;
  return result;
}

function decodeInquiries(
  value: unknown,
  observations: MoveReviewObservations,
  context: CommentaryInquiryContext | undefined,
  candidates: CommentaryCandidate[],
): CommentaryInquiry[] | undefined {
  // One original reference pool and at most one refreshed reference pool.
  if (
    !Array.isArray(value) ||
    value.length > 436 ||
    !context ||
    !isMoveReviewEngineProfile(context.engineProfile)
  )
    return;
  const root = setupPosition('chess', parseFen(context.rootFen).unwrap()).unwrap();
  const paths = new Map(observations.paths.map(path => [path.id, path]));
  const inquiries: CommentaryInquiry[] = [];
  for (const raw of value) {
    if (
      !object(raw) ||
      !exactKeys(
        raw,
        [
          'id',
          'scope',
          'source',
          'root_occurrence_id',
          'anchor_occurrence_ids',
          'ordering_source_ids',
          'position_id',
          'reference_move',
          'alternative_move',
          'reasons',
          'state',
          'stop_reason',
          'answer',
        ],
        ['assessment_effect', 'ordering_candidate_ids', 'explanation_candidate_ids', 'next_evidence'],
      ) ||
      !id(raw.id) ||
      raw.scope !== 'root_choice' ||
      raw.source !== 'automatic_observation' ||
      !id(raw.root_occurrence_id) ||
      !id(raw.position_id) ||
      !id(raw.reference_move) ||
      !id(raw.alternative_move) ||
      !ids(raw.anchor_occurrence_ids) ||
      !raw.anchor_occurrence_ids.includes(raw.root_occurrence_id) ||
      raw.anchor_occurrence_ids.length > 2 ||
      !ids(raw.ordering_source_ids) ||
      raw.ordering_source_ids.length > 8 ||
      raw.reference_move === raw.alternative_move ||
      raw.alternative_move === context.playedMove ||
      !['deferred', 'awaiting_engine', 'answered', 'stopped'].includes(String(raw.state)) ||
      (raw.assessment_effect !== undefined &&
        !['unchanged', 'refreshed', 'needs_refresh'].includes(String(raw.assessment_effect))) ||
      (raw.stop_reason !== null && !inquiryStops.includes(String(raw.stop_reason)) &&
        !(raw.state === 'deferred' && raw.stop_reason === 'comparison_not_current')) ||
      !Array.isArray(raw.reasons) ||
      !raw.reasons.length ||
      raw.reasons.length > 218
    )
      return;
    if (
      (raw.state === 'answered') !== (raw.answer !== null) ||
      (raw.state === 'stopped' && raw.stop_reason === null) ||
      (raw.state === 'awaiting_engine' && raw.stop_reason !== null) ||
      (raw.state === 'deferred' && raw.stop_reason !== null &&
        !['investigation_limit', 'comparison_not_current'].includes(String(raw.stop_reason))) ||
      (raw.stop_reason === 'reference_changed' && raw.state !== 'stopped') ||
      (raw.state === 'answered' &&
        raw.stop_reason !== null &&
        (raw.assessment_effect !== 'needs_refresh' ||
          ['reference_changed', 'investigation_limit'].includes(String(raw.stop_reason))))
    )
      return;
    const position = observations.positions?.find(position => position.id === raw.position_id);
    if (
      !position ||
      position.fen !== context.rootFen ||
      !position.legal_moves.complete ||
      !observations.paths.some(
        path =>
          path.kind !== 'history' &&
          path.startFen === context.rootFen &&
          path.positionIds?.[0] === raw.position_id &&
          path.occurrenceIds[0] === raw.root_occurrence_id &&
          path.moves[0]?.uci === context.playedMove,
      )
    )
      return;
    if (
      !raw.anchor_occurrence_ids.every(occurrence =>
        observations.paths.some(
          path =>
            path.kind !== 'history' &&
            path.startFen === context.rootFen &&
            path.positionIds?.[0] === position.id &&
            path.occurrenceIds[0] === occurrence &&
            [context.playedMove, raw.reference_move].includes(path.moves[0]?.uci),
        ),
      )
    )
      return;
    const legal = new Map(position.legal_moves.moves.map(move => [move.uci, move]));
    const reference = legal.get(raw.reference_move);
    const alternative = legal.get(raw.alternative_move);
    if (!reference || !alternative) return;
    if (
      raw.ordering_source_ids.length &&
      (alternative.captured_square !== undefined ||
        !raw.ordering_source_ids.every(source =>
          position.controls.pieces.some(
            control =>
              control.piece.side !== root.turn &&
              control.piece.role === 'pawn' &&
              control.source_ids.some(
                (id, index) => id === source && control.squares[index] === alternative.destination,
              ),
          ),
        ))
    )
      return;
    const validPiece = (value: unknown): value is CommentaryInquiryPiece => {
      if (
        !object(value) ||
        !exactKeys(value, ['position_id', 'side', 'role', 'square']) ||
        value.position_id !== position.id ||
        typeof value.square !== 'string'
      )
        return false;
      const square = parseSquare(value.square);
      const piece = square === undefined ? undefined : root.board.get(square);
      return !!piece && piece.color === value.side && piece.role === value.role;
    };
    const reasons: CommentaryInquiryReason[] = [];
    for (const reason of raw.reasons) {
      if (
        !object(reason) ||
        !exactKeys(
          reason,
          [
            'kind',
            'evidence_id',
            'source_id',
            'source_line_id',
            'anchor_move',
            'actor',
            'capture',
            'promotion_role',
            'responds_to_check',
          ],
          ['source_name', 'target', 'pressure_source_ids'],
        ) ||
        ![
          'engine_alternative',
          'capture_option',
          'same_destination',
          'same_actor_choice',
          'target_relocation',
          'attacker_capture',
          'target_support',
        ].includes(String(reason.kind)) ||
        reason.evidence_id !== position.premise_id ||
        reason.source_id !== alternative.source_id ||
        ![raw.reference_move, context.playedMove].includes(String(reason.anchor_move)) ||
        !validPiece(reason.actor) ||
        reason.actor.side !== root.turn ||
        reason.actor.square !== raw.alternative_move.slice(0, 2) ||
        reason.responds_to_check !== root.isCheck()
      )
        return;
      const parsed = parseUci(raw.alternative_move);
      if (
        !parsed ||
        !('from' in parsed) ||
        !root.isLegal(parsed) ||
        reason.promotion_role !== (parsed.promotion ?? null)
      )
        return;
      if (
        alternative.captured_square
          ? !validPiece(reason.capture) ||
            reason.capture.square !== alternative.captured_square ||
            reason.capture.side === root.turn ||
            reason.capture.role === 'king'
          : reason.capture !== null
      )
        return;
      const anchor = legal.get(String(reason.anchor_move));
      if (!anchor) return;
      if (reason.kind === 'engine_alternative') {
        if (!id(reason.source_line_id)) return;
        const path = paths.get(reason.source_line_id);
        if (
          !path ||
          path.startFen !== context.rootFen ||
          path.positionIds?.[0] !== position.id ||
          path.moves[0]?.uci !== raw.alternative_move ||
          !path.evaluations?.some(e => e.source !== 'direct_input')
        )
          return;
      } else if (reason.source_line_id !== null) return;
      if (reason.kind === 'capture_option' && reason.capture === null) return;
      const targeted = ['target_relocation', 'attacker_capture', 'target_support'].includes(
        String(reason.kind),
      );
      const extensions = ['source_name', 'target', 'pressure_source_ids'];
      if (targeted || extensions.some(key => key in reason)) {
        if (!extensions.every(key => key in reason) || !ids(reason.pressure_source_ids)) return;
        if (!targeted) {
          if (
            reason.source_name !== (reason.kind === 'engine_alternative' ? 'engine_pv' : 'legal_root') ||
            reason.target !== null ||
            reason.pressure_source_ids.length
          )
            return;
        } else {
          if (
            reason.source_name !== 'attacked_target' ||
            !validPiece(reason.target) ||
            reason.target.side !== root.turn ||
            reason.target.role === 'king' ||
            !reason.pressure_source_ids.length
          )
            return;
          const target = reason.target;
          const pressureIds = reason.pressure_source_ids;
          const pressures = position.controls.pieces.filter(
            control =>
              control.piece.side !== target.side &&
              control.source_ids.some(
                (source, index) => pressureIds.includes(source) && control.squares[index] === target.square,
              ),
          );
          if (
            !reason.pressure_source_ids.every(source =>
              pressures.some(control =>
                control.source_ids.some(
                  (entry, index) => entry === source && control.squares[index] === target.square,
                ),
              ),
            )
          )
            return;
          if (reason.kind === 'target_relocation' && reason.actor.square !== target.square) return;
          if (
            reason.kind === 'attacker_capture' &&
            (!reason.capture ||
              !pressures.some(
                control => control.piece.square === (reason.capture as CommentaryInquiryPiece).square,
              ))
          )
            return;
          if (reason.kind === 'target_support') {
            const targetSquare = parseSquare(target.square)!;
            const actorSquare = parseSquare(reason.actor.square)!;
            if (
              !position.controls.complete ||
              actorSquare === targetSquare ||
              attacks(root.board.get(actorSquare)!, actorSquare, root.board.occupied).has(targetSquare)
            )
              return;
            const after = root.clone();
            after.play(parsed);
            const destination = parseSquare(alternative.destination)!;
            const provider = after.board.get(destination);
            const survivor = after.board.get(targetSquare);
            if (
              !provider ||
              provider.color !== root.turn ||
              survivor?.color !== target.side ||
              survivor.role !== target.role ||
              !attacks(provider, destination, after.board.occupied).has(targetSquare)
            )
              return;
          }
        }
      }
      const anchorSquare = parseSquare(String(reason.anchor_move).slice(0, 2));
      const anchorActor = anchorSquare === undefined ? undefined : root.board.get(anchorSquare);
      if (
        reason.kind === 'same_destination' &&
        (alternative.destination !== anchor.destination ||
          reason.actor.square === String(reason.anchor_move).slice(0, 2) ||
          anchorActor?.role !== reason.actor.role)
      )
        return;
      if (
        reason.kind === 'same_actor_choice' &&
        (reason.actor.square !== String(reason.anchor_move).slice(0, 2) ||
          reason.actor.role === 'pawn' ||
          reason.capture !== null)
      )
        return;
      reasons.push(reason as unknown as CommentaryInquiryReason);
    }
    if (new Set(reasons.map(reason => JSON.stringify(reason))).size !== reasons.length) return;
    const answer =
      raw.answer === null
        ? null
        : decodeComparisonAnswer(
            raw.answer,
            observations,
            context,
            position.id,
            raw.reference_move,
            raw.alternative_move,
          );
    if (answer === undefined) return;
    const extras = ['ordering_candidate_ids', 'explanation_candidate_ids', 'next_evidence'];
    if (extras.some(key => key in raw)) {
      if (
        !extras.every(key => key in raw) ||
        !ids(raw.ordering_candidate_ids) ||
        !ids(raw.explanation_candidate_ids)
      )
        return;
      const sameRoot = (candidate: CommentaryCandidate) =>
        candidate.rootOccurrenceId === raw.root_occurrence_id;
      if (
        !raw.ordering_candidate_ids.every(candidateId => {
          const candidate = candidates.find(candidate => candidate.id === candidateId && sameRoot(candidate));
          const detail = candidate && pvComparisonDetail(candidate);
          return (
            detail?.material_relation === 'equal' &&
            detail.quality_band === 'close' &&
            detail.root_observation_relation === 'different' &&
            reasons.some(
              reason =>
                ['same_actor_choice', 'same_destination'].includes(reason.kind) &&
                reason.capture === null &&
                detail.root_participants.some(
                  piece =>
                    piece.position_id === reason.actor.position_id &&
                    piece.side === reason.actor.side &&
                    piece.role === reason.actor.role &&
                    piece.square === reason.actor.square,
                ),
            )
          );
        })
      )
        return;
      const matching = candidates.filter(candidate => {
        const detail = pvComparisonDetail(candidate);
        return (
          sameRoot(candidate) &&
          answer &&
          detail &&
          detail.answer.reference_line_id === answer.reference_line_id &&
          detail.answer.alternative_line_id === answer.alternative_line_id &&
          detail.answer.work_id === answer.work_id &&
          detail.answer.execution_key_sha256 === answer.execution_key_sha256
        );
      });
      if (
        !raw.explanation_candidate_ids.every(candidateId =>
          matching.some(candidate => candidate.id === candidateId),
        )
      )
        return;
      const expectedNext =
        raw.assessment_effect === 'needs_refresh'
          ? 'compare_roots'
          : answer
            ? matching.some(candidate => candidate.state === 'ready')
              ? 'inspect_existing'
              : 'unresolved'
            : raw.state === 'awaiting_engine' || raw.state === 'deferred'
              ? 'compare_roots'
              : 'unresolved';
      if (raw.next_evidence !== expectedNext) return;
    }
    inquiries.push({ ...raw, reasons, answer } as unknown as CommentaryInquiry);
  }
  if (
    new Set(inquiries.map(inquiry => inquiry.id)).size !== inquiries.length ||
    new Set(inquiries.map(inquiry => inquiry.reference_move)).size > 2 ||
    new Set(inquiries.map(inquiry => `${inquiry.reference_move}:${inquiry.alternative_move}`)).size !==
      inquiries.length
  )
    return;
  return inquiries;
}

const exchangeSignature = (summary: ObservedLineSummary): string =>
  JSON.stringify(
    summary
      .material!.events.flatMap(event => [
        ...(event.capture
          ? [`loss:${event.side === 'white' ? 'black' : 'white'}:${event.capture.role}`]
          : []),
        ...(event.promotion_gain_cp > 0
          ? [`promotion:${event.side}:${event.before_role}:${event.after_role}`]
          : []),
      ])
      .sort(),
  );

const uniqueSorted = (values: string[]): string[] => [...new Set(values)].sort();
const sameIds = (left: string[], right: string[]): boolean =>
  JSON.stringify(uniqueSorted(left)) === JSON.stringify(uniqueSorted(right));

function rootFactSignature(observations: MoveReviewObservations, occurrence: string): string[] {
  const piece = (value: { side: Color; role: string; square: string } | null) =>
    value && [value.side, value.role, value.square];
  return uniqueSorted(
    observations.statements.flatMap(statement => {
      if (statement.occurrenceId !== occurrence) return [];
      if (statement.kind === 'development')
        return [
          JSON.stringify([
            'development',
            statement.side,
            statement.role,
            statement.from,
            uniqueSorted(statement.centralSquares),
          ]),
        ];
      if (
        statement.kind !== 'piece_control' ||
        statement.before.state === 'unknown' ||
        statement.after.state === 'unknown' ||
        (statement.before.state !== 'present' && statement.after.state !== 'present')
      )
        return [];
      const move = observations.paths.find(path => path.occurrenceIds[0] === occurrence)?.moves[0];
      const parsed = move && parseUci(move.uci);
      const afterRole = parsed && 'from' in parsed ? (parsed.promotion ?? statement.role) : statement.role;
      const isMover = (
        value: { side: Color; role: string; square: string } | null,
        phase: 'before' | 'after',
      ) =>
        value?.side === statement.side &&
        value.role === (phase === 'before' ? statement.role : afterRole) &&
        value.square === (phase === 'before' ? statement.from : statement.to);
      if (
        ![statement.before.controller, statement.before.target].some(piece => isMover(piece, 'before')) &&
        ![statement.after.controller, statement.after.target].some(piece => isMover(piece, 'after'))
      )
        return [];
      const normalizeAfter = (value: { side: Color; role: string; square: string } | null) =>
        isMover(value, 'after') ? [statement.side, statement.role, statement.from] : piece(value);
      return [
        JSON.stringify([
          'control',
          [piece(statement.before.controller), piece(statement.before.target), statement.before.state],
          [
            normalizeAfter(statement.after.controller),
            normalizeAfter(statement.after.target),
            statement.after.state,
          ],
        ]),
      ];
    }),
  );
}

function eventSignature(summary: ObservedLineSummary): string {
  return JSON.stringify(
    summary.material!.events.map(event => [
      event.ply_offset,
      [event.side, event.from, event.to, event.before_role, event.after_role],
      event.capture ? [event.side, event.before_role, event.capture.role, event.capture.square] : null,
      event.promotion_gain_cp,
    ]),
  );
}

function qualityBand(
  left: CommentaryEvaluation,
  right: CommentaryEvaluation,
  mover: Color,
): 'close' | 'separated' {
  const winPercent = (evaluation: CommentaryEvaluation): number => {
    if (evaluation.kind === 'exact_automatic_terminal')
      return evaluation.terminal.kind === 'checkmate' ? (evaluation.terminal.winner === mover ? 100 : 0) : 50;
    if (evaluation.white_score.kind === 'mate')
      return evaluation.white_score.value > 0 === (mover === 'white') ? 100 : 0;
    return rankingScore(evaluation, mover);
  };
  const difference = Math.abs(winPercent(left) - winPercent(right));
  // VerdictThresholdPolicy in both directions: MatchesReference or PlayableLoss only.
  // Mate-distance differences alone are PlayableLoss; exact outcomes have no cp degradation field.
  const cpException =
    difference > 0 &&
    left.kind === 'engine_search' &&
    right.kind === 'engine_search' &&
    left.white_score.kind === 'cp' &&
    right.white_score.kind === 'cp' &&
    Math.abs(left.white_score.value - right.white_score.value) >= 500;
  return difference >= 2.5 || cpException ? 'separated' : 'close';
}

function decodePvComparison(
  raw: Record<string, unknown>,
  observations: MoveReviewObservations,
  context: CommentaryInquiryContext | undefined,
): CommentaryPvComparisonDetail | undefined {
  const detail = raw.detail;
  if (
    !context ||
    !object(detail) ||
    !exactKeys(
      detail,
      [
        'type',
        'scope',
        'answer',
        'material_relation',
        'event_sequence_relation',
        'root_observation_relation',
        'root_difference_occurrence_ids',
        'root_participants',
        'quality_band',
        'text',
      ],
      ['continuation_candidate_ids', 'continuation_comparison_ids'],
    ) ||
    detail.type !== 'pv_comparison' ||
    detail.scope !== 'observed_lines' ||
    raw.selection !== 'context' ||
    !object(detail.answer) ||
    !ids(raw.line_ids) ||
    raw.line_ids.length !== 2 ||
    !ids(detail.root_difference_occurrence_ids) ||
    !Array.isArray(detail.root_participants) ||
    typeof detail.text !== 'string' ||
    !detail.text.trim() ||
    detail.text.length > 1000
  )
    return;
  const paths = raw.line_ids.map(lineId => observations.paths.find(path => path.id === lineId)!);
  const [left, right] = paths;
  const positionId = left.positionIds?.[0];
  const rootPath = observations.paths.find(
    path =>
      path.kind !== 'history' &&
      path.occurrenceIds[0] === raw.root_occurrence_id &&
      path.moves[0]?.uci === context.playedMove &&
      path.positionIds?.[0] === positionId &&
      path.startFen === context.rootFen,
  );
  if (!rootPath || !positionId) return;
  const answer = decodeComparisonAnswer(
    detail.answer,
    observations,
    context,
    positionId,
    left.moves[0]?.uci,
    right.moves[0]?.uci,
  );
  if (!answer || answer.reference_line_id !== left.id || answer.alternative_line_id !== right.id) return;
  const available = new Set(observations.positions?.map(position => position.id));
  const material = paths.map(path =>
    path.positionIds?.length === path.moves.length + 1 &&
    path.positionIds.every(position => position !== null && available.has(position))
      ? path.lineSummary?.material
      : undefined,
  );
  const materialRelation = material.every(Boolean)
    ? exchangeSignature(left.lineSummary!) === exchangeSignature(right.lineSummary!)
      ? 'equal'
      : 'different'
    : 'unavailable';
  const eventRelation = material.every(Boolean)
    ? eventSignature(left.lineSummary!) === eventSignature(right.lineSummary!)
      ? 'same'
      : 'different'
    : 'unavailable';
  const signatures = [answer.reference_occurrence_id, answer.alternative_occurrence_id].map(occurrence =>
    rootFactSignature(observations, occurrence),
  );
  const rootRelation = signatures.every(signature => signature.length)
    ? JSON.stringify(signatures[0]) === JSON.stringify(signatures[1])
      ? 'same'
      : 'different'
    : 'unavailable';
  const differenceIds =
    rootRelation === 'different' ? [answer.reference_occurrence_id, answer.alternative_occurrence_id] : [];
  const root = setupPosition('chess', parseFen(context.rootFen).unwrap()).unwrap();
  const participants = differenceIds.map((_, index) => {
    const square = paths[index].moves[0].uci.slice(0, 2) as Key;
    const piece = root.board.get(parseSquare(square)!);
    return piece && { position_id: positionId, side: piece.color, role: piece.role, square };
  });
  if (participants.some(piece => !piece)) return;
  const expectedParticipants = [
    ...new Map(participants.map(piece => [JSON.stringify(piece), piece!])).values(),
  ];
  if (
    detail.material_relation !== materialRelation ||
    detail.event_sequence_relation !== eventRelation ||
    detail.root_observation_relation !== rootRelation ||
    JSON.stringify(detail.root_difference_occurrence_ids) !== JSON.stringify(differenceIds) ||
    detail.root_participants.length !== expectedParticipants.length ||
    !detail.root_participants.every((piece, index) => {
      const expected = expectedParticipants[index];
      return (
        object(piece) &&
        exactKeys(piece, ['position_id', 'side', 'role', 'square']) &&
        Object.keys(expected).every(key => piece[key] === expected[key as keyof typeof expected])
      );
    }) ||
    detail.quality_band !==
      qualityBand(answer.reference_evaluation, answer.alternative_evaluation, root.turn) ||
    raw.state !==
      ([materialRelation, eventRelation, rootRelation].includes('different') ? 'ready' : 'needs_explanation')
  )
    return;
  const sourceRefs = raw.source_refs as CommentaryCandidate['sourceRefs'];
  if (
    sourceRefs.length !== 2 ||
    !paths.every((path, index) =>
      sourceRefs.some(
        source =>
          source.line_id === path.id &&
          source.kind === 'evaluation' &&
          source.stop === null &&
          source.accepted_ply_count === path.moves.length &&
          source.source === [answer.reference_evaluation, answer.alternative_evaluation][index].source,
      ),
    ) ||
    !sameIds(
      raw.evidence_ids as string[],
      paths.flatMap(path => path.lineSummary?.premise_ids ?? []),
    )
  )
    return;
  return { ...detail, answer, root_participants: expectedParticipants } as CommentaryPvComparisonDetail;
}

function decodeTargetResponse(
  raw: Record<string, unknown>,
  observations: MoveReviewObservations,
  inquiries: CommentaryResponseInquiry[],
): CommentaryTargetResponseDetail | undefined {
  const detail = raw.detail;
  if (
    !object(detail) ||
    !exactKeys(
      detail,
      ['type', 'scope', 'response_inquiry_id', 'original_target', 'branches', 'text'],
      ['continuation_candidate_ids', 'continuation_comparison_ids'],
    ) ||
    detail.scope !== 'observed_lines' ||
    raw.selection !== 'context' ||
    !Array.isArray(detail.branches) ||
    detail.branches.length !== 2
  )
    return;
  const inquiry = inquiries.find(inquiry => inquiry.id === detail.response_inquiry_id);
  if (
    !inquiry ||
    raw.root_occurrence_id !== inquiry.root_occurrence_id ||
    !object(detail.original_target) ||
    !exactKeys(detail.original_target, ['position_id', 'side', 'role', 'square']) ||
    !samePiece(detail.original_target as unknown as CommentaryInquiryPiece, inquiry.original_target)
  )
    return;
  const equalPiece = (value: unknown, expected: CommentaryInquiryPiece | null): boolean =>
    expected === null
      ? value === null
      : object(value) &&
        exactKeys(value, ['position_id', 'side', 'role', 'square']) &&
        samePiece(value as unknown as CommentaryInquiryPiece, expected);
  const equalPairs = (value: unknown, expected: CommentaryTrackedPiece[]): boolean =>
    Array.isArray(value) &&
    value.length === expected.length &&
    value.every(
      piece =>
        object(piece) &&
        exactKeys(piece, ['original', 'current']) &&
        expected.some(
          other => equalPiece(piece.original, other.original) && equalPiece(piece.current, other.current),
        ),
    ) &&
    expected.every(other =>
      value.some(
        piece => equalPiece(piece.original, other.original) && equalPiece(piece.current, other.current),
      ),
    );
  const piecesAt = (path: ObservationPath, ply: number) => trackedPieces(path, ply) ?? [];
  let complete = true;
  const used: ObservationPath[] = [];
  for (const [index, branch] of detail.branches.entries()) {
    if (
      !object(branch) ||
      !exactKeys(branch, [
        'line_id',
        'reply_move',
        'reply_occurrence_id',
        'position_id',
        'target_state',
        'target',
        'providers',
        'capturer',
        'recaptures',
        'observed_recapture',
      ])
    )
      return;
    const path = observations.paths.find(path => path.id === branch.line_id);
    const reply = branch.reply_move;
    if (
      !path ||
      path.kind === 'history' ||
      path.moves[0]?.uci !== inquiry.root_move ||
      path.occurrenceIds[0] !== inquiry.root_move_occurrence_id ||
      path.positionIds?.[1] !== inquiry.child_position_id ||
      typeof reply !== 'string' ||
      !(index === 0 ? inquiry.capture_moves : inquiry.decline_moves).includes(reply) ||
      path.moves[1]?.uci !== reply ||
      branch.reply_move !== reply ||
      path.occurrenceIds[1] !== branch.reply_occurrence_id ||
      path.positionIds?.[2] !== branch.position_id
    )
      return;
    used.push(path);
    const pieces = piecesAt(path, 2),
      childPieces = piecesAt(path, 1);
    const position = observations.positions?.find(position => position.id === branch.position_id);
    const target = pieces.find(piece => samePiece(piece.original, inquiry.original_target))?.current ?? null;
    const targetState = target ? 'present' : 'captured';
    if (branch.target_state !== targetState || !equalPiece(branch.target, target)) return;
    const providers = branch.providers;
    if (
      !object(providers) ||
      !exactKeys(providers, ['state', 'pieces', 'source_ids']) ||
      !ids(providers.source_ids)
    )
      return;
    const providerState = !target ? 'not_applicable' : position?.controls.complete ? 'complete' : 'unknown';
    const providerPairs: CommentaryTrackedPiece[] = [],
      providerSources: string[] = [];
    if (target && providerState === 'complete')
      for (const control of position!.controls.pieces) {
        if (control.piece.side !== target.side) continue;
        const pair = pieces.find(
          piece =>
            piece.current.square === control.piece.square &&
            piece.current.side === control.piece.side &&
            piece.current.role === control.piece.role,
        );
        if (!pair) return;
        control.squares.forEach((square, index) => {
          if (square === target.square) {
            providerPairs.push(pair);
            providerSources.push(control.source_ids[index]);
          }
        });
      }
    if (
      providers.state !== providerState ||
      !equalPairs(providers.pieces, providerPairs) ||
      !sameIds(providers.source_ids, providerSources)
    )
      return;
    const actorBefore = childPieces.find(piece => piece.current.square === reply.slice(0, 2));
    const capturer =
      inquiry.capture_moves.includes(reply) && actorBefore
        ? (pieces.find(piece => samePiece(piece.original, actorBefore.original))?.current ?? null)
        : null;
    if (!equalPiece(branch.capturer, capturer)) return;
    const recaptures = branch.recaptures;
    if (!object(recaptures) || !exactKeys(recaptures, ['state', 'moves']) || !Array.isArray(recaptures.moves))
      return;
    const recaptureState = capturer
      ? position?.legal_moves.complete
        ? 'complete'
        : 'unknown'
      : 'not_applicable';
    const moves =
      recaptureState === 'complete'
        ? position!.legal_moves.moves.filter(move => move.captured_square === capturer!.square)
        : [];
    if (
      recaptures.state !== recaptureState ||
      recaptures.moves.length !== moves.length ||
      !sameIds(
        recaptures.moves.map(move => (object(move) ? String(move.move_uci) : '')),
        moves.map(move => move.uci),
      ) ||
      !recaptures.moves.every(
        move =>
          object(move) &&
          exactKeys(move, ['move_uci', 'source_id', 'actor']) &&
          moves.some(
            legal =>
              legal.uci === move.move_uci &&
              legal.source_id === move.source_id &&
              equalPiece(
                move.actor,
                pieces.find(piece => piece.current.square === legal.uci.slice(0, 2))?.current ?? null,
              ),
          ),
      )
    )
      return;
    const observed = branch.observed_recapture;
    if (
      !object(observed) ||
      !exactKeys(observed, ['state', 'occurrence_id', 'actor', 'supported_targets', 'source_ids']) ||
      !ids(observed.source_ids)
    )
      return;
    const next = path.moves[2]?.uci;
    const wasObserved = !!next && moves.some(move => move.uci === next);
    if (!wasObserved) {
      if (
        observed.state !== (recaptureState === 'unknown' ? 'unknown' : 'not_observed') ||
        observed.occurrence_id !== null ||
        observed.actor !== null ||
        !equalPairs(observed.supported_targets, []) ||
        observed.source_ids.length
      )
        return;
    } else {
      const after = piecesAt(path, 3),
        afterPosition = observations.positions?.find(position => position.id === path.positionIds?.[3]);
      const beforeActor = pieces.find(piece => piece.current.square === next.slice(0, 2));
      const actor = beforeActor && after.find(piece => samePiece(piece.original, beforeActor.original));
      const expectedState = actor && afterPosition?.controls.complete ? 'complete' : 'unknown';
      const targets: CommentaryTrackedPiece[] = [],
        sources: string[] = [];
      if (expectedState === 'complete')
        for (const control of afterPosition!.controls.pieces) {
          if (
            control.piece.square !== actor!.current.square ||
            control.piece.side !== actor!.current.side ||
            control.piece.role !== actor!.current.role
          )
            continue;
          control.squares.forEach((square, index) => {
            const supported = after.find(
              piece =>
                piece.current.square === square &&
                piece.current.side === actor!.current.side &&
                piece.current.role !== 'king' &&
                !samePiece(piece.original, actor!.original),
            );
            if (supported) {
              targets.push(supported);
              sources.push(control.source_ids[index]);
            }
          });
        }
      if (
        observed.state !== expectedState ||
        observed.occurrence_id !== path.occurrenceIds[2] ||
        (actor ? !equalPairs([observed.actor], [actor]) : observed.actor !== null) ||
        !equalPairs(observed.supported_targets, targets) ||
        !sameIds(observed.source_ids, sources)
      )
        return;
    }
    if (
      providerState === 'unknown' ||
      recaptureState === 'unknown' ||
      (moves.length > 0 && observed.state !== 'complete')
    )
      complete = false;
  }
  if (
    raw.state !== (complete ? 'ready' : 'partial') ||
    !sameIds(
      raw.line_ids as string[],
      used.map(path => path.id),
    ) ||
    !sameIds(
      raw.evidence_ids as string[],
      used.flatMap(path => path.lineSummary?.premise_ids ?? []),
    )
  )
    return;
  const sources = raw.source_refs as CommentaryCandidate['sourceRefs'];
  if (
    sources.length !== 2 ||
    !used.every(path =>
      sources.some(
        source =>
          source.line_id === path.id &&
          source.kind === 'admitted' &&
          source.source === 'legal_replay' &&
          source.stop === null &&
          source.accepted_ply_count === path.moves.length,
      ),
    )
  )
    return;
  return detail as unknown as CommentaryTargetResponseDetail;
}

function continuationSupportAt(
  observations: MoveReviewObservations,
  path: ObservationPath,
  actor: CommentaryInquiryPiece,
  target: CommentaryInquiryPiece,
  offset: number,
  pieces = trackedPieces(path, offset),
): ContinuationSupport {
  if (!pieces) return { state: 'unknown', source_ids: [] };
  const currentActor = pieces.find(piece => samePiece(piece.original, actor))?.current;
  const currentTarget = pieces.find(piece => samePiece(piece.original, target))?.current;
  if (!currentActor) return { state: 'actor_removed', source_ids: [] };
  if (!currentTarget) return { state: 'target_removed', source_ids: [] };
  const position = observations.positions?.find(position => position.id === path.positionIds![offset]);
  const controls = position?.controls.pieces.find(
    control =>
      control.piece.side === currentActor.side &&
      control.piece.role === currentActor.role &&
      control.piece.square === currentActor.square,
  );
  const sources =
    controls?.source_ids.filter((_source, index) => controls.squares[index] === currentTarget.square) ?? [];
  return {
    state: sources.length ? 'present' : position?.controls.complete ? 'absent' : 'unknown',
    source_ids: sources,
  };
}

// One lookup for the profile, pair, and unresolved-use boundaries; no new geometry or move search.
function kingCaptureAt(
  observations: MoveReviewObservations,
  path: ObservationPath,
  provider: CommentaryInquiryPiece,
  originalTarget: CommentaryInquiryPiece,
  originalKing: CommentaryInquiryPiece,
  count: number,
  pieces: CommentaryTrackedPiece[],
): { support: ContinuationSupport; application: ContinuationKingCapture } | undefined {
  const position = observations.positions?.find(position => position.id === path.positionIds![count]);
  if (!position?.legal_moves.complete || parseFen(position.fen).unwrap().turn !== originalKing.side) return;
  const king = pieces.find(piece => samePiece(piece.original, originalKing))?.current;
  const target = pieces.find(piece => samePiece(piece.original, originalTarget))?.current;
  if (!king || !target) return;
  const contact = continuationSupportAt(observations, path, originalKing, originalTarget, count, pieces);
  const support = continuationSupportAt(observations, path, provider, originalTarget, count, pieces);
  if (contact.state !== 'present' || support.state === 'unknown') return;
  const rows = position.legal_moves.moves.filter(
    move => move.uci.slice(0, 2) === king.square && move.captured_square === target.square,
  );
  return {
    support,
    application: {
      kind: 'king_capture',
      original_attacker: originalKing,
      attacker: king,
      target,
      contact_source_ids: contact.source_ids,
      moves_uci: rows.map(move => move.uci),
      source_ids: rows.map(move => move.source_id),
      legal_inventory_complete: true,
    },
  };
}

function kingCaptureKey(application: ContinuationKingCapture): string {
  const original = application.original_attacker,
    actor = application.attacker,
    target = application.target;
  return [
    original.position_id,
    original.side,
    original.role,
    original.square,
    actor.side,
    actor.role,
    actor.square,
    target.side,
    target.role,
    target.square,
  ].join(':');
}

function decodeKingCaptureApplication(
  value: unknown,
  observations: MoveReviewObservations,
  path: ObservationPath,
  comparison: CommentaryLineContinuationDetail['support_comparisons'][number],
  originalTarget: CommentaryInquiryPiece,
  count: number,
  piecesAt: (count: number) => CommentaryTrackedPiece[],
): ContinuationKingCapture | undefined {
  const isOriginal = (value: unknown): value is CommentaryInquiryPiece =>
    object(value) &&
    exactKeys(value, ['position_id', 'side', 'role', 'square']) &&
    piecesAt(0).some(piece => samePiece(piece.original, value as unknown as CommentaryInquiryPiece));
  if (
    !object(value) ||
    !exactKeys(value, [
      'kind',
      'original_attacker',
      'attacker',
      'target',
      'contact_source_ids',
      'moves_uci',
      'source_ids',
      'legal_inventory_complete',
    ]) ||
    value.kind !== 'king_capture' ||
    !isOriginal(value.original_attacker) ||
    value.original_attacker.role !== 'king' ||
    value.original_attacker.side === originalTarget.side ||
    !object(value.attacker) ||
    !exactKeys(value.attacker, ['position_id', 'side', 'role', 'square']) ||
    !object(value.target) ||
    !exactKeys(value.target, ['position_id', 'side', 'role', 'square']) ||
    !ids(value.contact_source_ids) ||
    !value.contact_source_ids.length ||
    !ids(value.moves_uci) ||
    !ids(value.source_ids) ||
    value.legal_inventory_complete !== true
  )
    return;
  const base = path.positionIds!.indexOf(comparison.after_position_id);
  if (base < 0 || count < base) return;
  const point = kingCaptureAt(
    observations,
    path,
    comparison.after_actor,
    originalTarget,
    value.original_attacker,
    count,
    piecesAt(count),
  );
  if (
    !point ||
    !samePiece(point.application.attacker, value.attacker as unknown as CommentaryInquiryPiece) ||
    !samePiece(point.application.target, value.target as unknown as CommentaryInquiryPiece) ||
    !sameIds(value.contact_source_ids, point.application.contact_source_ids) ||
    !sameIds(value.moves_uci, point.application.moves_uci) ||
    !sameIds(value.source_ids, point.application.source_ids)
  )
    return;
  for (let at = base; at <= count; at++) {
    const earlier = kingCaptureAt(
      observations,
      path,
      comparison.after_actor,
      originalTarget,
      value.original_attacker,
      at,
      piecesAt(at),
    );
    if (
      earlier &&
      kingCaptureKey(earlier.application) === kingCaptureKey(point.application) &&
      earlier.support.state === point.support.state &&
      earlier.application.moves_uci.length > 0 === point.application.moves_uci.length > 0
    )
      return at === count ? (value as unknown as ContinuationKingCapture) : undefined;
  }
  return;
}

function decodeLineContinuation(
  raw: Record<string, unknown>,
  observations: MoveReviewObservations,
): CommentaryLineContinuationDetail | undefined {
  const detail = raw.detail;
  if (
    !object(detail) ||
    !exactKeys(
      detail,
      [
        'type',
        'scope',
        'line_id',
        'events',
        'capture_links',
        'other_target_gains',
        'support_comparisons',
        'text',
      ],
      [
        'open_loss_event_ids',
        'recovery_losses',
        'support_applications',
        'capture_exposures',
        'direct_support_recaptures',
      ],
    ) ||
    detail.type !== 'line_continuation' ||
    detail.scope !== 'observed_line' ||
    raw.selection !== 'context' ||
    !ids(raw.line_ids) ||
    raw.line_ids.length !== 1 ||
    raw.line_ids[0] !== detail.line_id ||
    !Array.isArray(detail.events) ||
    !Array.isArray(detail.capture_links) ||
    !Array.isArray(detail.other_target_gains) ||
    !Array.isArray(detail.support_comparisons) ||
    (detail.open_loss_event_ids === undefined) !== (detail.recovery_losses === undefined) ||
    typeof detail.text !== 'string' ||
    !detail.text.trim() ||
    detail.text.length > 1000
  )
    return;
  const path = observations.paths.find(path => path.id === detail.line_id);
  const material = path?.lineSummary?.material;
  if (
    !path ||
    path.kind === 'history' ||
    !material ||
    !path.positionIds ||
    detail.events.length !== material.events.length
  )
    return;
  const snapshots = new Map<number, CommentaryTrackedPiece[] | undefined>();
  const at = (offset: number) => {
    if (!snapshots.has(offset)) snapshots.set(offset, trackedPieces(path, offset));
    return snapshots.get(offset);
  };
  const equalPiece = (value: unknown, expected: CommentaryInquiryPiece | null): boolean =>
    expected === null
      ? value === null
      : object(value) &&
        exactKeys(value, ['position_id', 'side', 'role', 'square']) &&
        samePiece(value as unknown as CommentaryInquiryPiece, expected);
  const origins = at(0);
  if (!origins) return;
  const isOrigin = (value: unknown): value is CommentaryInquiryPiece =>
    origins.some(piece => equalPiece(value, piece.original));
  const events: CommentaryLineContinuationDetail['events'] = [];
  for (const [index, value] of detail.events.entries()) {
    const physical = material.events[index];
    const before = at(physical.ply_offset);
    const legal = observations.positions
      ?.find(position => position.id === path.positionIds![physical.ply_offset])
      ?.legal_moves.moves.find(move => move.uci === physical.move_uci);
    const actor = before?.find(piece => piece.current.square === physical.from)?.original ?? null;
    const victim = physical.capture
      ? (before?.find(piece => piece.current.square === physical.capture!.square)?.original ?? null)
      : null;
    if (
      !object(value) ||
      !exactKeys(value, [
        'id',
        'ply_offset',
        'occurrence_id',
        'source_id',
        'original_actor',
        'original_victim',
        'actor_end',
      ]) ||
      !id(value.id) ||
      value.ply_offset !== physical.ply_offset ||
      value.occurrence_id !== physical.occurrence_id ||
      !legal ||
      value.source_id !== legal.source_id ||
      !equalPiece(value.original_actor, actor) ||
      !equalPiece(value.original_victim, victim) ||
      !object(value.actor_end) ||
      !exactKeys(value.actor_end, ['state', 'position_id', 'piece', 'removed_by_event_id']) ||
      value.actor_end.position_id !== path.positionIds[path.moves.length]
    )
      return;
    events.push(value as unknown as CommentaryLineContinuationDetail['events'][number]);
  }
  if (new Set(events.map(event => event.id)).size !== events.length) return;
  for (const event of events) {
    const current =
      event.original_actor &&
      at(path.moves.length)?.find(piece => samePiece(piece.original, event.original_actor!));
    const removed =
      event.original_actor &&
      events.find(
        later =>
          later.ply_offset > event.ply_offset &&
          later.original_victim &&
          samePiece(later.original_victim, event.original_actor!),
      );
    const expected = current ? 'present' : removed ? 'captured' : 'unknown';
    if (
      event.actor_end.state !== expected ||
      !equalPiece(event.actor_end.piece, current?.current ?? null) ||
      event.actor_end.removed_by_event_id !== (removed?.id ?? null)
    )
      return;
  }
  const byId = new Map(events.map(event => [event.id, event]));
  for (const link of detail.capture_links) {
    if (
      !object(link) ||
      !exactKeys(link, ['loss_event_id', 'capture_event_id', 'elapsed_plies']) ||
      !id(link.loss_event_id) ||
      !id(link.capture_event_id)
    )
      return;
    const loss = byId.get(link.loss_event_id),
      capture = byId.get(link.capture_event_id);
    if (
      !loss?.original_actor ||
      !loss.original_victim ||
      !capture?.original_victim ||
      capture.ply_offset <= loss.ply_offset ||
      link.elapsed_plies !== capture.ply_offset - loss.ply_offset ||
      !samePiece(loss.original_actor, capture.original_victim)
    )
      return;
  }
  const removedCapturers = events.filter(
    event => event.original_victim && event.actor_end.state === 'captured',
  );
  const captureLinks = detail.capture_links as CommentaryLineContinuationDetail['capture_links'];
  if (
    captureLinks.length !== removedCapturers.length ||
    removedCapturers.some(
      loss =>
        !captureLinks.some(
          link =>
            link.loss_event_id === loss.id && link.capture_event_id === loss.actor_end.removed_by_event_id,
        ),
    )
  )
    return;
  for (const link of detail.other_target_gains) {
    if (
      !object(link) ||
      !exactKeys(link, ['loss_event_id', 'gain_event_id']) ||
      !id(link.loss_event_id) ||
      !id(link.gain_event_id)
    )
      return;
    const loss = byId.get(link.loss_event_id),
      gain = byId.get(link.gain_event_id);
    if (
      !loss?.original_actor ||
      !loss.original_victim ||
      !gain?.original_victim ||
      gain.ply_offset <= loss.ply_offset ||
      gain.original_victim.side === loss.original_victim.side ||
      samePiece(loss.original_actor, gain.original_victim)
    )
      return;
    const interval = material.events.filter(
      event => event.ply_offset >= loss.ply_offset && event.ply_offset < gain.ply_offset,
    );
    const sign = loss.original_victim.side === path.lineSummary!.perspective ? 1 : -1;
    if (!interval.length || interval.some(event => event.balance_cp * sign >= 0)) return;
  }
  if (detail.open_loss_event_ids !== undefined) {
    if (
      !ids(detail.open_loss_event_ids) ||
      !Array.isArray(detail.recovery_losses) ||
      detail.open_loss_event_ids.some(id => {
        const loss = byId.get(id);
        return !loss?.original_actor || !continuationLossIsOpen(path, loss);
      })
    )
      return;
    if (
      new Set(detail.open_loss_event_ids.map(id => byId.get(id)!.original_victim!.side)).size !==
      detail.open_loss_event_ids.length
    )
      return;
    const gainLinks = detail.other_target_gains as CommentaryLineContinuationDetail['other_target_gains'];
    const expected = [
      ...captureLinks.map(link => [link.loss_event_id, link.capture_event_id]),
      ...gainLinks.map(link => [link.loss_event_id, link.gain_event_id]),
    ].flatMap(([lossId, recoveryId]) => {
      const recovery = byId.get(recoveryId)!;
      return recovery.actor_end.removed_by_event_id
        ? [JSON.stringify([lossId, recoveryId, recovery.actor_end.removed_by_event_id])]
        : [];
    });
    const received = detail.recovery_losses.map(link => {
      if (
        !object(link) ||
        !exactKeys(link, ['loss_event_id', 'recovery_event_id', 'later_loss_event_id']) ||
        !id(link.loss_event_id) ||
        !id(link.recovery_event_id) ||
        !id(link.later_loss_event_id)
      )
        return null;
      return JSON.stringify([link.loss_event_id, link.recovery_event_id, link.later_loss_event_id]);
    });
    if (
      received.some(link => link === null) ||
      new Set(received).size !== received.length ||
      !sameIds(received as string[], expected)
    )
      return;
  }
  const supportState = (
    actor: CommentaryInquiryPiece,
    target: CommentaryInquiryPiece,
    offset: number,
  ): ContinuationSupport => continuationSupportAt(observations, path, actor, target, offset, at(offset));
  for (const comparison of detail.support_comparisons) {
    if (
      !object(comparison) ||
      !exactKeys(comparison, [
        'id',
        'kind',
        'state',
        'before_actor',
        'after_actor',
        'before_position_id',
        'after_position_id',
        'trigger_occurrence_ids',
        'targets',
      ]) ||
      !id(comparison.id) ||
      !['actor_continuation', 'replacement_after_capture'].includes(String(comparison.kind)) ||
      !['complete', 'unknown'].includes(String(comparison.state)) ||
      !isOrigin(comparison.before_actor) ||
      !isOrigin(comparison.after_actor) ||
      !id(comparison.before_position_id) ||
      !id(comparison.after_position_id) ||
      !ids(comparison.trigger_occurrence_ids) ||
      !comparison.trigger_occurrence_ids.length ||
      !comparison.trigger_occurrence_ids.every(occurrence => path.occurrenceIds.includes(occurrence)) ||
      !Array.isArray(comparison.targets)
    )
      return;
    const before = path.positionIds.indexOf(comparison.before_position_id),
      after = path.positionIds.indexOf(comparison.after_position_id);
    if (before < 0 || after < before) return;
    const complete = [before, after].every(
      offset =>
        observations.positions?.find(position => position.id === path.positionIds![offset])?.controls
          .complete,
    );
    if (comparison.state !== (complete ? 'complete' : 'unknown')) return;
    if (
      comparison.kind === 'actor_continuation' &&
      (!samePiece(comparison.before_actor, comparison.after_actor) ||
        after !== path.moves.length ||
        !(
          (before === 0 &&
            comparison.before_actor.square === path.moves[0].uci.slice(0, 2) &&
            sameIds(comparison.trigger_occurrence_ids, [path.occurrenceIds[0]!])) ||
          events.some(
            event =>
              event.original_actor &&
              samePiece(event.original_actor, comparison.before_actor as CommentaryInquiryPiece) &&
              before === event.ply_offset + 1 &&
              sameIds(comparison.trigger_occurrence_ids as string[], [event.occurrence_id]),
          )
        ))
    )
      return;
    if (
      comparison.kind === 'replacement_after_capture' &&
      !detail.capture_links.some(value => {
        const link = value as CommentaryLineContinuationDetail['capture_links'][number];
        const loss = byId.get(link.loss_event_id)!,
          recovery = byId.get(link.capture_event_id)!;
        return (
          loss.original_victim &&
          recovery.original_actor &&
          loss.original_victim.side === recovery.original_actor.side &&
          samePiece(loss.original_victim, comparison.before_actor as CommentaryInquiryPiece) &&
          samePiece(recovery.original_actor, comparison.after_actor as CommentaryInquiryPiece) &&
          before === loss.ply_offset &&
          after === recovery.ply_offset + 1 &&
          sameIds(comparison.trigger_occurrence_ids as string[], [loss.occurrence_id, recovery.occurrence_id])
        );
      })
    )
      return;
    const expectedTargets = origins
      .filter(
        piece =>
          piece.original.side === (comparison.before_actor as CommentaryInquiryPiece).side &&
          piece.original.role !== 'king' &&
          !samePiece(piece.original, comparison.before_actor as CommentaryInquiryPiece) &&
          !samePiece(piece.original, comparison.after_actor as CommentaryInquiryPiece),
      )
      .filter(
        piece =>
          supportState(comparison.before_actor as CommentaryInquiryPiece, piece.original, before).state ===
            'present' ||
          supportState(comparison.after_actor as CommentaryInquiryPiece, piece.original, after).state ===
            'present',
      );
    if (comparison.targets.length !== expectedTargets.length) return;
    for (const target of comparison.targets) {
      if (
        !object(target) ||
        !exactKeys(target, ['original_target', 'before', 'after']) ||
        !isOrigin(target.original_target) ||
        !expectedTargets.some(piece =>
          samePiece(piece.original, target.original_target as CommentaryInquiryPiece),
        )
      )
        return;
      for (const [key, actor, offset] of [
        ['before', comparison.before_actor, before],
        ['after', comparison.after_actor, after],
      ] as const) {
        const value = target[key],
          expected = supportState(actor, target.original_target, offset);
        if (
          !object(value) ||
          !exactKeys(value, ['state', 'source_ids']) ||
          !ids(value.source_ids) ||
          value.state !== expected.state ||
          !sameIds(value.source_ids, expected.source_ids)
        )
          return;
      }
    }
    if (
      new Set(
        comparison.targets.map(target => JSON.stringify((target as Record<string, unknown>).original_target)),
      ).size !== comparison.targets.length
    )
      return;
  }
  if (
    [detail.capture_links, detail.other_target_gains, detail.support_comparisons].some(
      values => new Set(values.map(value => JSON.stringify(value))).size !== values.length,
    )
  )
    return;
  if (detail.support_applications !== undefined) {
    if (
      !Array.isArray(detail.support_applications) ||
      detail.support_applications.length > 10000 ||
      new Set(detail.support_applications.map(value => JSON.stringify(value))).size !==
        detail.support_applications.length
    )
      return;
    for (const value of detail.support_applications) {
      if (
        !object(value) ||
        !exactKeys(value, [
          'requirement_id',
          'comparison_id',
          'original_target',
          'original_provider',
          'ply_count',
          'position_id',
          'preceding_occurrence_id',
          'support',
          'application',
        ]) ||
        !isOrigin(value.original_target) ||
        !isOrigin(value.original_provider) ||
        !integer(value.ply_count, 1, path.moves.length) ||
        value.position_id !== path.positionIds[value.ply_count] ||
        value.preceding_occurrence_id !== path.occurrenceIds[value.ply_count - 1] ||
        !object(value.support) ||
        !exactKeys(value.support, ['state', 'source_ids']) ||
        value.support.state !== 'present' ||
        !ids(value.support.source_ids)
      )
        return;
      const comparison = (
        detail.support_comparisons as CommentaryLineContinuationDetail['support_comparisons']
      ).find(comparison => comparison.id === value.comparison_id);
      if (
        !comparison ||
        (comparison.kind === 'replacement_after_capture'
          ? !id(value.requirement_id)
          : value.requirement_id !== null) ||
        !samePiece(comparison.after_actor, value.original_provider) ||
        !comparison.targets.some(target =>
          samePiece(target.original_target, value.original_target as CommentaryInquiryPiece),
        )
      )
        return;
      const application = decodeKingCaptureApplication(
        value.application,
        observations,
        path,
        comparison,
        value.original_target,
        value.ply_count,
        count => at(count) ?? [],
      );
      const support = continuationSupportAt(
        observations,
        path,
        value.original_provider,
        value.original_target,
        value.ply_count,
        at(value.ply_count),
      );
      if (
        !application ||
        application.moves_uci.length ||
        support.state !== 'present' ||
        !sameIds(value.support.source_ids, support.source_ids)
      )
        return;
    }
  }
  const current = (original: CommentaryInquiryPiece, count: number) =>
    at(count)?.find(piece => samePiece(piece.original, original))?.current ?? null;
  for (const name of ['capture_exposures', 'direct_support_recaptures'] as const) {
    const values = detail[name];
    if (
      values !== undefined &&
      (!Array.isArray(values) ||
        values.length > 10000 ||
        new Set(values.map(value => JSON.stringify(value))).size !== values.length)
    )
      return;
  }
  for (const value of (detail.capture_exposures ?? []) as unknown[]) {
    if (
      !object(value) ||
      !exactKeys(value, [
        'kind',
        'prefix_moves_uci',
        'occurrence_id',
        'before_position_id',
        'position_id',
        'original_attacker',
        'attacker',
        'original_target',
        'target',
        'vacated_blockers',
        'ray_source_id',
        'control_source_ids',
        'moves_uci',
        'source_ids',
        'legal_inventory_complete',
      ]) ||
      value.kind !== 'capture_exposure' ||
      !Array.isArray(value.prefix_moves_uci) ||
      !integer(value.prefix_moves_uci.length, 1, path.moves.length) ||
      !value.prefix_moves_uci.every((move, index) => move === path.moves[index].uci) ||
      !isOrigin(value.original_attacker) ||
      !isOrigin(value.original_target) ||
      !Array.isArray(value.vacated_blockers) ||
      !value.vacated_blockers.length ||
      !ids(value.control_source_ids) ||
      !ids(value.moves_uci) ||
      !ids(value.source_ids) ||
      value.legal_inventory_complete !== true
    )
      return;
    const count = value.prefix_moves_uci.length;
    const before = observations.positions?.find(position => position.id === path.positionIds![count - 1]);
    const after = observations.positions?.find(position => position.id === path.positionIds![count]);
    if (!before || !after) return;
    const attacker = current(value.original_attacker, count),
      target = current(value.original_target, count);
    const oldAttacker = current(value.original_attacker, count - 1),
      oldTarget = current(value.original_target, count - 1);
    const ray = before.rays.lines.find(ray => ray.source_id === value.ray_source_id);
    if (
      !attacker ||
      !target ||
      !oldAttacker ||
      !oldTarget ||
      !ray ||
      value.occurrence_id !== path.occurrenceIds[count - 1] ||
      value.before_position_id !== before.id ||
      value.position_id !== after.id ||
      !equalPiece(value.attacker, attacker) ||
      !equalPiece(value.target, target) ||
      !equalPiece({ ...oldAttacker, position_id: after.id }, attacker) ||
      !equalPiece({ ...oldTarget, position_id: after.id }, target) ||
      ray.piece.side !== attacker.side ||
      ray.piece.role !== attacker.role ||
      ray.piece.square !== attacker.square ||
      attacker.side !== parseFen(after.fen).unwrap().turn ||
      target.side !== parseFen(before.fen).unwrap().turn ||
      target.role === 'king' ||
      !after.legal_moves.complete
    )
      return;
    const targetIndex = ray.occupants.findIndex(
      piece => piece.side === target.side && piece.role === target.role && piece.square === target.square,
    );
    const blockers = ray.occupants.slice(0, targetIndex);
    if (
      targetIndex < 1 ||
      blockers.length !== value.vacated_blockers.length ||
      !value.vacated_blockers.every(
        (blocker, index) =>
          object(blocker) &&
          exactKeys(blocker, ['original', 'before']) &&
          isOrigin(blocker.original) &&
          equalPiece(blocker.before, current(blocker.original, count - 1)) &&
          object(blocker.before) &&
          blocker.before.side === blockers[index].side &&
          blocker.before.role === blockers[index].role &&
          blocker.before.square === blockers[index].square &&
          !at(count)?.some(piece => piece.current.square === blockers[index].square),
      )
    )
      return;
    const controls = continuationSupportAt(
      observations,
      path,
      value.original_attacker,
      value.original_target,
      count,
      at(count),
    );
    const captures = after.legal_moves.moves.filter(
      move => move.uci.slice(0, 2) === attacker.square && move.captured_square === target.square,
    );
    if (
      controls.state !== 'present' ||
      !sameIds(value.control_source_ids, controls.source_ids) ||
      !captures.length ||
      !sameIds(
        value.moves_uci,
        captures.map(move => move.uci),
      ) ||
      !sameIds(
        value.source_ids,
        captures.map(move => move.source_id),
      )
    )
      return;
  }
  for (const value of (detail.direct_support_recaptures ?? []) as unknown[]) {
    if (
      !object(value) ||
      !exactKeys(value, [
        'kind',
        'requirement_id',
        'comparison_id',
        'original_provider',
        'original_target',
        'original_capturer',
        'ply_count',
        'position_id',
        'provider',
        'target',
        'capturer',
        'support',
        'loss_event_id',
        'recapture_event_id',
        'loss_occurrence_id',
        'recapture_occurrence_id',
        'elapsed_plies',
      ]) ||
      value.kind !== 'direct_support_recapture' ||
      !id(value.requirement_id) ||
      !isOrigin(value.original_provider) ||
      !isOrigin(value.original_target) ||
      !isOrigin(value.original_capturer) ||
      !integer(value.ply_count, 1, path.moves.length) ||
      !object(value.support) ||
      !exactKeys(value.support, ['state', 'source_ids']) ||
      value.support.state !== 'present' ||
      !ids(value.support.source_ids)
    )
      return;
    const comparison = (
      detail.support_comparisons as CommentaryLineContinuationDetail['support_comparisons']
    ).find(row => row.id === value.comparison_id);
    const loss = events.find(event => event.id === value.loss_event_id),
      recovery = events.find(event => event.id === value.recapture_event_id);
    const count = value.ply_count;
    if (
      !comparison ||
      comparison.kind !== 'replacement_after_capture' ||
      !samePiece(comparison.after_actor, value.original_provider) ||
      !comparison.targets.some(target =>
        samePiece(target.original_target, value.original_target as CommentaryInquiryPiece),
      ) ||
      count < path.positionIds.indexOf(comparison.after_position_id) ||
      !loss ||
      !recovery ||
      loss.ply_offset !== count ||
      recovery.ply_offset <= count ||
      !equalPiece(loss.original_actor, value.original_capturer) ||
      !equalPiece(loss.original_victim, value.original_target) ||
      !equalPiece(recovery.original_actor, value.original_provider) ||
      !equalPiece(recovery.original_victim, value.original_capturer) ||
      value.loss_occurrence_id !== loss.occurrence_id ||
      value.recapture_occurrence_id !== recovery.occurrence_id ||
      value.elapsed_plies !== recovery.ply_offset - count ||
      !(detail.capture_links as CommentaryLineContinuationDetail['capture_links']).some(
        link => link.loss_event_id === loss.id && link.capture_event_id === recovery.id,
      ) ||
      value.position_id !== path.positionIds[count] ||
      !equalPiece(value.provider, current(value.original_provider, count)) ||
      !equalPiece(value.target, current(value.original_target, count)) ||
      !equalPiece(value.capturer, current(value.original_capturer, count))
    )
      return;
    const provider = current(value.original_provider, count),
      target = current(value.original_target, count),
      capturer = current(value.original_capturer, count + 1);
    const support = continuationSupportAt(
      observations,
      path,
      value.original_provider,
      value.original_target,
      count,
      at(count),
    );
    if (
      !provider ||
      !target ||
      !capturer ||
      capturer.square !== target.square ||
      support.state !== 'present' ||
      !sameIds(value.support.source_ids, support.source_ids) ||
      !Array.from({ length: recovery.ply_offset - count + 1 }, (_, index) => count + index).every(at =>
        equalPiece(current(value.original_provider as CommentaryInquiryPiece, at), {
          ...provider,
          position_id: path.positionIds![at]!,
        }),
      ) ||
      !Array.from({ length: recovery.ply_offset - count }, (_, index) => count + index + 1).every(at =>
        equalPiece(current(value.original_capturer as CommentaryInquiryPiece, at), {
          ...capturer,
          position_id: path.positionIds![at]!,
        }),
      )
    )
      return;
  }
  const partial =
    events.some(event => event.actor_end.state === 'unknown') ||
    detail.support_comparisons.some(comparison => {
      const value = comparison as CommentaryLineContinuationDetail['support_comparisons'][number];
      return (
        value.state === 'unknown' ||
        value.targets.some(target => target.before.state === 'unknown' || target.after.state === 'unknown')
      );
    });
  if (raw.state !== (partial ? 'partial' : 'ready')) return;
  return detail as unknown as CommentaryLineContinuationDetail;
}

function validContinuationComparison(
  candidate: CommentaryCandidate,
  candidates: Map<string, CommentaryCandidate>,
  observations: MoveReviewObservations,
): boolean {
  const detail = candidate.detail;
  if (
    !exactKeys(detail, ['type', 'scope', 'continuation_candidate_ids', 'differences', 'text']) ||
    detail.scope !== 'observed_line_pair' ||
    candidate.state !== 'ready' ||
    candidate.selection !== 'context' ||
    candidate.lineIds.length !== 2 ||
    !ids(detail.continuation_candidate_ids) ||
    detail.continuation_candidate_ids.length !== 2 ||
    !Array.isArray(detail.differences) ||
    !detail.differences.length ||
    detail.differences.length > 10000 ||
    typeof detail.text !== 'string' ||
    !detail.text.trim() ||
    detail.text.length > 1000
  )
    return false;
  const profiles = detail.continuation_candidate_ids
    .map(id => candidates.get(id))
    .map(value => value && lineContinuationDetail(value));
  if (profiles.some((value, index) => !value || value.line_id !== candidate.lineIds[index])) return false;
  const paths = candidate.lineIds.map(id => observations.paths.find(path => path.id === id)!);
  if (paths[0].startFen !== paths[1].startFen || paths[0].positionIds?.[0] !== paths[1].positionIds?.[0])
    return false;
  const originalPieces = trackedPieces(paths[0], 0)!;
  const isOriginal = (value: unknown): value is CommentaryInquiryPiece =>
    object(value) &&
    exactKeys(value, ['position_id', 'side', 'role', 'square']) &&
    originalPieces.some(piece => samePiece(piece.original, value as unknown as CommentaryInquiryPiece));
  for (const difference of detail.differences) {
    if (!object(difference) || !object(difference.left) || !object(difference.right)) return false;
    const sides = [difference.left, difference.right];
    if (difference.kind === 'recapturer_support') {
      if (
        !exactKeys(difference, [
          'kind',
          'original_subject',
          'original_target',
          'matched_support_comparison_ids',
          'left',
          'right',
        ]) ||
        !isOriginal(difference.original_subject) ||
        !isOriginal(difference.original_target) ||
        difference.original_subject.side !== difference.original_target.side ||
        difference.original_target.role === 'king' ||
        samePiece(difference.original_subject, difference.original_target) ||
        !ids(difference.matched_support_comparison_ids) ||
        difference.matched_support_comparison_ids.length !== 2
      )
        return false;
      const anchorIds = difference.matched_support_comparison_ids;
      const anchors = profiles.map((profile, index) =>
        profile!.support_comparisons.find(row => row.id === anchorIds[index]),
      );
      if (
        anchors.some(row => !row || row.kind !== 'replacement_after_capture') ||
        !samePiece(anchors[0]!.before_actor, anchors[1]!.before_actor) ||
        !anchors.some(row =>
          samePiece(row!.after_actor, difference.original_subject as CommentaryInquiryPiece),
        )
      )
        return false;
      const counts = anchors.map((row, index) => paths[index].positionIds!.indexOf(row!.after_position_id));
      if (counts[0] % 2 !== counts[1] % 2) return false;
      const transitions: string[][] = [];
      for (const [index, side] of sides.entries()) {
        if (!exactKeys(side, ['before', 'after'])) return false;
        const states: string[] = [];
        for (const [step, point] of [side.before, side.after].entries()) {
          const count = counts[index] - 1 + step;
          if (
            !object(point) ||
            !exactKeys(point, [
              'line_id',
              'ply_count',
              'position_id',
              'preceding_occurrence_id',
              'support',
            ]) ||
            count < 1 ||
            point.line_id !== paths[index].id ||
            point.ply_count !== count ||
            point.position_id !== paths[index].positionIds![count] ||
            point.preceding_occurrence_id !== paths[index].occurrenceIds[count - 1] ||
            !object(point.support) ||
            !exactKeys(point.support, ['state', 'source_ids']) ||
            !ids(point.support.source_ids)
          )
            return false;
          const actual = continuationSupportAt(
            observations,
            paths[index],
            difference.original_subject,
            difference.original_target,
            count,
            trackedPieces(paths[index], count),
          );
          if (
            actual.state === 'unknown' ||
            point.support.state !== actual.state ||
            !sameIds(point.support.source_ids, actual.source_ids)
          )
            return false;
          states.push(actual.state);
        }
        transitions.push(states);
      }
      if (
        !transitions.flat().includes('present') ||
        !transitions.some(states => states[0] !== states[1]) ||
        transitions[0].join(':') === transitions[1].join(':')
      )
        return false;
      continue;
    }
    if (
      sides.some(
        (side, index) =>
          !integer(side.ply_count, 1, paths[index].moves.length) ||
          side.line_id !== paths[index].id ||
          side.position_id !== paths[index].positionIds?.[side.ply_count] ||
          side.preceding_occurrence_id !== paths[index].occurrenceIds[side.ply_count - 1],
      )
    )
      return false;
    if (
      difference.kind !== 'loss_followup' &&
      (sides[0].ply_count as number) % 2 !== (sides[1].ply_count as number) % 2
    )
      return false;
    const pointKeys = ['line_id', 'ply_count', 'position_id', 'preceding_occurrence_id'];
    if (difference.kind === 'loss_followup') {
      if (
        !exactKeys(
          difference,
          ['kind', 'original_actor', 'original_victim', 'left', 'right'],
          ['shared_attackers'],
        ) ||
        !isOriginal(difference.original_actor) ||
        !isOriginal(difference.original_victim)
      )
        return false;
      const signatures: string[] = [];
      for (const [index, side] of sides.entries()) {
        if (
          !exactKeys(side, [
            ...pointKeys,
            'loss_event_id',
            'loss_ply_offset',
            'observed_followup_plies',
            'capturer_end',
            'capturer_removal_state',
            'capturer_removal_event_id',
            'other_target_gain_state',
            'other_target_gain_event_ids',
          ]) ||
          side.ply_count !== paths[index].moves.length ||
          !id(side.loss_event_id) ||
          !ids(side.other_target_gain_event_ids)
        )
          return false;
        const profile = profiles[index]!,
          loss = profile.events.find(event => event.id === side.loss_event_id);
        if (
          !loss?.original_actor ||
          !loss.original_victim ||
          !samePiece(loss.original_actor, difference.original_actor) ||
          !samePiece(loss.original_victim, difference.original_victim) ||
          side.loss_ply_offset !== loss.ply_offset ||
          side.observed_followup_plies !== paths[index].moves.length - loss.ply_offset - 1 ||
          JSON.stringify(side.capturer_end) !== JSON.stringify(loss.actor_end) ||
          loss.actor_end.state === 'unknown' ||
          profile.events.some(
            (event, offset) =>
              !event.original_actor ||
              (paths[index].lineSummary!.material!.events[offset].capture && !event.original_victim),
          )
        )
          return false;
        const removals = profile.capture_links
          .filter(link => link.loss_event_id === loss.id)
          .map(link => link.capture_event_id);
        const gains = profile.other_target_gains
          .filter(link => link.loss_event_id === loss.id)
          .map(link => link.gain_event_id);
        if (
          removals.length > 1 ||
          side.capturer_removal_event_id !== (removals[0] ?? null) ||
          side.capturer_removal_state !== (removals.length ? 'observed' : 'not_observed_through_line_end') ||
          side.other_target_gain_state !== (gains.length ? 'observed' : 'not_observed_through_line_end') ||
          !sameIds(side.other_target_gain_event_ids, gains) ||
          (loss.actor_end.state === 'captured') !== removals.length > 0 ||
          loss.actor_end.removed_by_event_id !== (removals[0] ?? null)
        )
          return false;
        const physical = (ids: string[]) =>
          ids
            .map(id => {
              const event = profile.events.find(event => event.id === id)!;
              return JSON.stringify([event.original_actor, event.original_victim]);
            })
            .sort();
        signatures.push(JSON.stringify([physical(removals), physical(gains)]));
      }
      if (signatures[0] === signatures[1]) return false;
      if (difference.shared_attackers !== undefined) {
        const shared = difference.shared_attackers;
        if (
          !Array.isArray(shared) ||
          shared.length > originalPieces.length ||
          new Set(shared.map(row => JSON.stringify(row))).size !== shared.length
        )
          return false;
        let common = 0;
        while (
          common < paths[0].moves.length &&
          paths[0].occurrenceIds[common] === paths[1].occurrenceIds[common]
        )
          common++;
        const at = paths.map(path => trackedPieces(path, common) ?? []);
        for (const row of shared) {
          if (
            !object(row) ||
            !exactKeys(row, ['original_actor', 'actor', 'position_id', 'ply_count', 'left', 'right']) ||
            !isOriginal(row.original_actor) ||
            !object(row.actor) ||
            !exactKeys(row.actor, ['position_id', 'side', 'role', 'square']) ||
            row.ply_count !== common ||
            common >= Math.min(...paths.map(path => path.moves.length)) ||
            row.position_id !== paths[0].positionIds![common]
          )
            return false;
          const current = at[0].find(piece =>
            samePiece(piece.original, row.original_actor as CommentaryInquiryPiece),
          )?.current;
          if (!current || !samePiece(current, row.actor as unknown as CommentaryInquiryPiece)) return false;
          const targets: CommentaryInquiryPiece[] = [];
          for (const [index, key] of ['left', 'right'].entries()) {
            const branch = row[key],
              profile = profiles[index]!;
            if (
              !object(branch) ||
              !exactKeys(branch, ['event_id', 'target', 'source_ids']) ||
              !id(branch.event_id) ||
              !object(branch.target) ||
              !exactKeys(branch.target, ['position_id', 'side', 'role', 'square']) ||
              !ids(branch.source_ids) ||
              !branch.source_ids.length
            )
              return false;
            const event = continuationRecoveries(profile, sides[index].loss_event_id as string).filter(
              event =>
                event.ply_offset >= common &&
                event.original_actor &&
                samePiece(event.original_actor, row.original_actor as CommentaryInquiryPiece),
            )[0];
            const target =
              event?.original_victim &&
              at[index].find(piece => samePiece(piece.original, event.original_victim!))?.current;
            if (
              !event?.original_victim ||
              event.id !== branch.event_id ||
              !target ||
              target.side === current.side ||
              !samePiece(target, branch.target as unknown as CommentaryInquiryPiece)
            )
              return false;
            targets.push(event.original_victim);
            for (let offset = common; offset <= event.ply_offset; offset++) {
              const pieces = trackedPieces(paths[index], offset) ?? [];
              for (const [original, expected] of [
                [row.original_actor as CommentaryInquiryPiece, current],
                [event.original_victim, target],
              ]) {
                const actual = pieces.find(piece => samePiece(piece.original, original))?.current;
                if (
                  !actual ||
                  actual.side !== expected.side ||
                  actual.role !== expected.role ||
                  actual.square !== expected.square
                )
                  return false;
              }
            }
            const control = continuationSupportAt(
              observations,
              paths[index],
              row.original_actor as CommentaryInquiryPiece,
              event.original_victim,
              common,
              at[index],
            );
            if (control.state !== 'present' || !sameIds(branch.source_ids, control.source_ids)) return false;
          }
          if (samePiece(targets[0], targets[1])) return false;
        }
        if (
          new Set(shared.map(row => JSON.stringify((row as Record<string, unknown>).original_actor))).size !==
          shared.length
        )
          return false;
      }
    } else if (difference.kind === 'capture_timing' || difference.kind === 'legal_capture_availability') {
      if (
        !exactKeys(
          difference,
          ['kind', 'original_actor', 'original_victim', 'left', 'right'],
          difference.kind === 'legal_capture_availability'
            ? ['matched_capture_event_ids', 'matched_support_comparison_ids']
            : [],
        ) ||
        !isOriginal(difference.original_actor) ||
        !isOriginal(difference.original_victim)
      )
        return false;
      const events = sides.map((side, index) =>
        profiles[index]!.events.find(event => event.ply_offset + 1 === side.ply_count),
      );
      const supportAnchors = difference.matched_support_comparison_ids;
      if (
        !supportAnchors &&
        (events.some(event => !event?.original_actor || !event.original_victim) ||
          !samePiece(events[0]!.original_actor!, events[1]!.original_actor!) ||
          !samePiece(events[0]!.original_victim!, events[1]!.original_victim!))
      )
        return false;
      if (supportAnchors !== undefined) {
        if (
          difference.matched_capture_event_ids !== undefined ||
          !ids(supportAnchors) ||
          supportAnchors.length !== 2
        )
          return false;
        const anchors = profiles.map((profile, index) =>
          profile!.support_comparisons.find(row => row.id === supportAnchors[index]),
        );
        if (
          anchors.some(
            (row, index) =>
              !row ||
              row.kind !== 'replacement_after_capture' ||
              row.after_position_id !== sides[index].position_id,
          ) ||
          !samePiece(anchors[0]!.before_actor, anchors[1]!.before_actor) ||
          !profiles.some((profile, index) =>
            profile!.capture_exposures?.some(
              exposure =>
                exposure.position_id === sides[index].position_id &&
                samePiece(exposure.original_attacker, difference.original_actor as CommentaryInquiryPiece) &&
                samePiece(exposure.original_target, difference.original_victim as CommentaryInquiryPiece),
            ),
          )
        )
          return false;
      }
      if (difference.kind === 'capture_timing') {
        if (
          sides[0].ply_count === sides[1].ply_count ||
          !samePiece(events[0]!.original_actor!, difference.original_actor) ||
          !samePiece(events[0]!.original_victim!, difference.original_victim) ||
          sides.some(
            (side, index) =>
              !exactKeys(side, [...pointKeys, 'event_id', 'source_id']) ||
              side.event_id !== events[index]!.id ||
              side.source_id !== events[index]!.source_id,
          )
        )
          return false;
      } else {
        if (
          (!supportAnchors &&
            (!ids(difference.matched_capture_event_ids) ||
              !sameIds(
                difference.matched_capture_event_ids,
                events.map(event => event!.id),
              ))) ||
          sides[0].available === sides[1].available
        )
          return false;
        for (const [index, side] of sides.entries()) {
          if (
            !exactKeys(side, [
              ...pointKeys,
              'available',
              'moves_uci',
              'source_ids',
              'position_occurrence_id',
              'legal_inventory_complete',
            ]) ||
            typeof side.available !== 'boolean' ||
            !ids(side.moves_uci) ||
            !ids(side.source_ids) ||
            side.legal_inventory_complete !== true
          )
            return false;
          const position = observations.positions?.find(position => position.id === side.position_id);
          const pieces = trackedPieces(paths[index], side.ply_count as number);
          const actor = pieces?.find(piece =>
            samePiece(piece.original, difference.original_actor as CommentaryInquiryPiece),
          )?.current;
          const victim = pieces?.find(piece =>
            samePiece(piece.original, difference.original_victim as CommentaryInquiryPiece),
          )?.current;
          if (
            !position?.legal_moves.complete ||
            side.position_occurrence_id !== position.premise_id ||
            !actor ||
            !victim
          )
            return false;
          const rows = position.legal_moves.moves.filter(
            move => move.uci.slice(0, 2) === actor.square && move.captured_square === victim.square,
          );
          if (
            side.available !== rows.length > 0 ||
            !sameIds(
              side.moves_uci,
              rows.map(move => move.uci),
            ) ||
            !sameIds(
              side.source_ids,
              rows.map(move => move.source_id),
            )
          )
            return false;
        }
      }
    } else if (difference.kind === 'replacement_support' || difference.kind === 'root_actor_support') {
      if (
        !exactKeys(difference, [
          'kind',
          'original_subject',
          'original_target',
          'relation',
          'left',
          'right',
        ]) ||
        !isOriginal(difference.original_subject) ||
        !isOriginal(difference.original_target)
      )
        return false;
      const supportStates: ContinuationSupport[] = [];
      const applications: ContinuationKingCapture[] = [];
      const comparisons: CommentaryLineContinuationDetail['support_comparisons'] = [];
      for (const [index, side] of sides.entries()) {
        if (
          !exactKeys(
            side,
            [...pointKeys, 'original_provider', 'support', 'comparison_id', 'trigger_occurrence_ids'],
            ['application', 'target_capture_event_id', 'direct_recapture'],
          ) ||
          !isOriginal(side.original_provider) ||
          !ids(side.trigger_occurrence_ids) ||
          !object(side.support) ||
          !exactKeys(side.support, ['state', 'source_ids']) ||
          !ids(side.support.source_ids)
        )
          return false;
        const comparison = profiles[index]!.support_comparisons.find(row => row.id === side.comparison_id);
        if (
          !comparison ||
          comparison.kind !==
            (difference.kind === 'root_actor_support' ? 'actor_continuation' : 'replacement_after_capture') ||
          !samePiece(comparison.before_actor, difference.original_subject) ||
          !samePiece(comparison.after_actor, side.original_provider) ||
          (side.application === undefined &&
            side.target_capture_event_id === undefined &&
            comparison.after_position_id !== side.position_id) ||
          !sameIds(comparison.trigger_occurrence_ids, side.trigger_occurrence_ids) ||
          (difference.kind === 'root_actor_support' &&
            comparison.before_position_id !== paths[index].positionIds?.[0])
        )
          return false;
        comparisons.push(comparison);
        if (side.target_capture_event_id !== undefined) {
          const event = profiles[index]!.events.find(event => event.id === side.target_capture_event_id);
          if (
            difference.kind !== 'replacement_support' ||
            side.application !== undefined ||
            !event?.original_victim ||
            !event.original_actor ||
            event.ply_offset !== side.ply_count ||
            !samePiece(event.original_victim, difference.original_target) ||
            event.ply_offset < paths[index].positionIds!.indexOf(comparison.after_position_id)
          )
            return false;
        }
        if (side.direct_recapture !== undefined) {
          if (
            !object(side.direct_recapture) ||
            !exactKeys(side.direct_recapture, ['loss_event_id', 'recapture_event_id']) ||
            side.target_capture_event_id !== side.direct_recapture.loss_event_id ||
            !profiles[index]!.direct_support_recaptures?.some(
              use =>
                use.comparison_id === comparison.id &&
                use.loss_event_id === (side.direct_recapture as Record<string, unknown>).loss_event_id &&
                use.recapture_event_id ===
                  (side.direct_recapture as Record<string, unknown>).recapture_event_id,
            )
          )
            return false;
        }
        const snapshots = new Map<number, CommentaryTrackedPiece[]>();
        const piecesAt = (at: number) => {
          if (!snapshots.has(at)) snapshots.set(at, trackedPieces(paths[index], at) ?? []);
          return snapshots.get(at)!;
        };
        if (side.application !== undefined) {
          const application = decodeKingCaptureApplication(
            side.application,
            observations,
            paths[index],
            comparison,
            difference.original_target,
            side.ply_count as number,
            piecesAt,
          );
          if (!application) return false;
          applications.push(application);
        }
        const support = continuationSupportAt(
          observations,
          paths[index],
          side.original_provider,
          difference.original_target,
          side.ply_count as number,
          piecesAt(side.ply_count as number),
        );
        if (
          support.state === 'unknown' ||
          side.support.state !== support.state ||
          !sameIds(side.support.source_ids, support.source_ids)
        )
          return false;
        supportStates.push(support);
      }
      const sameState = supportStates[0].state === supportStates[1].state;
      if (sides.some(side => side.target_capture_event_id !== undefined)) {
        const losses = sides.map((side, index) =>
          profiles[index]!.events.find(event => event.id === side.target_capture_event_id),
        );
        if (
          !losses[0]?.original_actor ||
          !losses[1]?.original_actor ||
          !samePiece(losses[0].original_actor, losses[1].original_actor) ||
          !sides.some(side => side.direct_recapture)
        )
          return false;
      }
      if (
        !comparisons.some(comparison =>
          comparison.targets.some(target =>
            samePiece(target.original_target, difference.original_target as CommentaryInquiryPiece),
          ),
        ) ||
        (applications.length > 0 &&
          (applications.length !== 2 ||
            sameState ||
            applications.some(
              (application, index) =>
                supportStates[index].state === 'present' && application.moves_uci.length > 0,
            ) ||
            !samePiece(applications[0].original_attacker, applications[1].original_attacker) ||
            applications[0].attacker.square !== applications[1].attacker.square ||
            applications[0].attacker.role !== applications[1].attacker.role ||
            applications[0].target.square !== applications[1].target.square ||
            applications[0].target.role !== applications[1].target.role ||
            applications[0].moves_uci.length > 0 === applications[1].moves_uci.length > 0)) ||
        !supportStates.some(state => state.state === 'present') ||
        (sameState &&
          samePiece(
            sides[0].original_provider as CommentaryInquiryPiece,
            sides[1].original_provider as CommentaryInquiryPiece,
          )) ||
        difference.relation !== (sameState ? 'preserved_with_different_provider' : 'different_support')
      )
        return false;
    } else return false;
  }
  return new Set(detail.differences.map(value => JSON.stringify(value))).size === detail.differences.length;
}

const proposalId = (value: unknown): value is string =>
  typeof value === 'string' && /^[A-Za-z0-9._:-]{1,128}$/.test(value);
const factMoves = (value: unknown): value is Uci[] =>
  Array.isArray(value) &&
  value.length <= 80 &&
  value.every(move => typeof move === 'string' && /^[a-h][1-8][a-h][1-8][qrbn]?$/.test(move));
const factPly = (value: unknown): value is number =>
  Number.isInteger(value) && Number(value) >= 0 && Number(value) <= 80;
const factText = (value: unknown): value is string =>
  typeof value === 'string' && value.length > 0 && value.length <= 256;
const factSampleIds = (value: unknown): value is string[] =>
  Array.isArray(value) && value.length <= 360000 && value.every(id) && new Set(value).size === value.length;
const factPrefix = (prefix: Uci[], moves: Uci[]): boolean =>
  prefix.every((move, index) => moves[index] === move);
const factCompatible = (left: Uci[], right: Uci[]): boolean =>
  factPrefix(left, right) || factPrefix(right, left);
const factPiece = (value: unknown): value is CommentaryFactPiece =>
  object(value) &&
  exactKeys(value, ['side', 'role', 'square']) &&
  ['white', 'black'].includes(String(value.side)) &&
  ['pawn', 'knight', 'bishop', 'rook', 'queen', 'king'].includes(String(value.role)) &&
  typeof value.square === 'string' &&
  /^[a-h][1-8]$/.test(value.square);
function factPoint(value: unknown): value is CommentaryFactPoint {
  if (!object(value)) return false;
  if (value.kind === 'ply') return exactKeys(value, ['kind', 'ply_count']) && factPly(value.ply_count);
  return (
    value.kind === 'event' &&
    exactKeys(value, ['kind', 'event', 'ordinal', 'phase', 'from_ply', 'through_ply'], ['actor', 'target', 'square']) &&
    ['move', 'capture', 'promotion', 'check', 'castling'].includes(String(value.event)) &&
    factPly(value.ordinal) &&
    value.ordinal > 0 &&
    ['before', 'after'].includes(String(value.phase)) &&
    factPly(value.from_ply) &&
    factPly(value.through_ply) &&
    value.through_ply > value.from_ply &&
    (value.actor === undefined || factPiece(value.actor)) &&
    (value.target === undefined || (value.event === 'capture' && factPiece(value.target))) &&
    (value.square === undefined || (value.event === 'move' &&
      typeof value.square === 'string' && /^[a-h][1-8]$/.test(value.square)))
  );
}
// Value domains are a wire contract. Only the backend calculates the chess predicate.
function factValues(kind: string): string[] {
  switch (kind) {
    case 'presence':
      return ['present', 'removed'];
    case 'location':
      return ['removed', ...Array.from({ length: 64 }, (_, i) => makeSquare(i))];
    case 'role':
      return ['pawn', 'knight', 'bishop', 'rook', 'queen', 'king', 'removed'];
    case 'control':
      return ['present', 'absent', 'actor_removed'];
    case 'support':
      return ['present', 'absent', 'actor_removed', 'target_removed'];
    case 'legal_move':
      return ['legal', 'illegal', 'not_turn', 'actor_removed'];
    case 'legal_capture':
      return ['legal', 'illegal', 'not_turn', 'actor_removed', 'target_removed'];
    case 'check':
      return ['present', 'absent'];
    case 'pawn_property':
      return ['true', 'false', 'not_pawn', 'actor_removed'];
    case 'pawn_advance':
      return ['present', 'absent', 'not_pawn', 'actor_removed'];
    case 'file_occupancy':
      return ['open', 'white_pawns_only', 'black_pawns_only', 'both_sides'];
    case 'direct_support_use':
      return ['true', 'false'];
    default:
      return [];
  }
}
function factPredicate(value: unknown): value is CommentaryFactPredicate {
  if (!object(value) || typeof value.kind !== 'string') return false;
  const fields = ['presence', 'location', 'role'].includes(value.kind)
    ? ['subject']
    : ['control', 'legal_move', 'pawn_advance'].includes(value.kind)
      ? ['subject', 'square']
      : ['support', 'legal_capture', 'direct_support_use'].includes(value.kind)
        ? ['subject', 'target']
        : value.kind === 'check'
          ? ['side']
          : value.kind === 'pawn_property'
            ? ['subject', 'property']
            : value.kind === 'file_occupancy'
              ? ['property']
              : undefined;
  if (!fields || !exactKeys(value, ['kind', ...fields], value.kind === 'legal_move' ? ['property'] : []))
    return false;
  if (
    (value.subject !== undefined && !factPiece(value.subject)) ||
    (value.target !== undefined && !factPiece(value.target)) ||
    (value.square !== undefined &&
      (typeof value.square !== 'string' || !/^[a-h][1-8]$/.test(value.square))) ||
    (value.side !== undefined && !['white', 'black'].includes(String(value.side)))
  )
    return false;
  if (
    value.kind === 'legal_move' &&
    value.property !== undefined &&
    !['queen', 'rook', 'bishop', 'knight'].includes(String(value.property))
  )
    return false;
  if (value.kind === 'pawn_property' && !['passed', 'isolated', 'doubled'].includes(String(value.property)))
    return false;
  if (
    value.kind === 'file_occupancy' &&
    (typeof value.property !== 'string' || !/^[a-h]$/.test(value.property))
  )
    return false;
  if (factPiece(value.subject) && factPiece(value.target)) {
    if (value.kind === 'legal_capture') return value.subject.side !== value.target.side;
    return (
      value.subject.side === value.target.side &&
      (value.subject.role !== value.target.role || value.subject.square !== value.target.square)
    );
  }
  return true;
}
function factRequirementValid(value: Record<string, unknown>, prior: CommentaryProposalReading[]): boolean {
  if (value.kind === 'fact') {
    if (
      !object(value.path) ||
      !exactKeys(value.path, ['binding', 'source', 'extend']) ||
      !proposalId(value.path.binding) ||
      typeof value.path.extend !== 'boolean' ||
      !object(value.path.source) ||
      !factPredicate(value.predicate) ||
      !factPoint(value.at) ||
      (value.through !== undefined && (!factPoint(value.through) || value.expected === undefined)) ||
      (value.expected !== undefined && !factValues(value.predicate.kind).includes(String(value.expected)))
    )
      return false;
    const source = value.path.source;
    if (source.kind === 'declared')
      return exactKeys(source, ['kind', 'moves_uci']) && factMoves(source.moves_uci);
    if (source.kind === 'comparison')
      return (
        exactKeys(source, ['kind', 'requirement_id', 'arm']) &&
        ['reference', 'alternative'].includes(String(source.arm)) &&
        prior.some(row => row.kind === 'compare_moves' && row.id === source.requirement_id)
      );
    return (
      source.kind === 'premise' &&
      exactKeys(source, ['kind', 'inquiry_id', 'premise_id']) &&
      [source.inquiry_id, source.premise_id].every(
        v => typeof v === 'string' && v.length > 0 && v.length <= 512,
      )
    );
  }
  if (value.kind === 'fact_comparison') {
    const left = prior.find(row => row.id === value.left_requirement_id);
    const right = prior.find(row => row.id === value.right_requirement_id);
    return (
      left?.kind === 'fact' &&
      right?.kind === 'fact' &&
      left !== right &&
      !left.through &&
      !right.through &&
      (['same', 'different'].includes(String(value.relation))
        ? left.predicate.kind === right.predicate.kind
        : ['before', 'after', 'same_time'].includes(String(value.relation)) &&
          left.path.binding === right.path.binding)
    );
  }
  return (
    value.kind === 'fact_group' &&
    ['all', 'any'].includes(String(value.mode)) &&
    ids(value.members) &&
    value.members.length > 0 &&
    value.members.length <= 600 &&
    value.members.every(member =>
      prior.some(
        row =>
          row.id === member &&
          (row.kind === 'fact_comparison' ||
            row.kind === 'fact_group' ||
            (row.kind === 'fact' && row.expected !== undefined)),
      ),
    )
  );
}
function factSampleMoves(sample: CommentaryFactSample, observations: MoveReviewObservations): Uci[] {
  return sample.moves_uci ?? observations.paths.find(path => path.id === sample.line_id)!
    .moves.slice(0, sample.prefix_ply_count).map(move => move.uci);
}

function factResultValid(
  value: unknown,
  reading: CommentaryProposalReading,
  prior: CommentaryProposalReading[],
  observations: MoveReviewObservations,
  context: CommentaryInquiryContext | undefined,
): value is CommentaryFactResult {
  if (
    !object(value) ||
    !exactKeys(value, ['state', 'samples', 'needs', 'proofs', 'reason']) ||
    ![
      'observed',
      'supported',
      'refuted',
      'partial',
      'unavailable_input',
      'not_applicable',
      'invalid_input',
    ].includes(String(value.state)) ||
    (value.reason !== null && !factText(value.reason)) ||
    !Array.isArray(value.samples) ||
    value.samples.length > (reading.kind === 'fact' ? 600 : 0) ||
    !Array.isArray(value.needs) ||
    value.needs.length > 360000 ||
    !Array.isArray(value.proofs) ||
    value.proofs.length > 600 ||
    (!['stopped', 'awaiting_engine'].includes(reading.state) && reading.state !== value.state)
  )
    return false;
  const samples = value.samples as unknown[];
  const all = [...prior, reading];
  const dependencies = new Set<string>();
  const pending = [reading.id];
  while (pending.length) {
    const next = pending.pop()!;
    if (dependencies.has(next)) continue;
    dependencies.add(next);
    const row = all.find(row => row.id === next);
    if (row?.kind === 'fact_group') pending.push(...row.members);
    if (row?.kind === 'fact_comparison') pending.push(row.left_requirement_id, row.right_requirement_id);
  }
  if (
    reading.state === 'awaiting_engine' &&
    reading.kind !== 'fact' &&
    !prior.some(
      row =>
        dependencies.has(row.id) &&
        row.kind === 'fact' &&
        row.state === 'awaiting_engine' &&
        row.work_id === reading.work_id &&
        row.result?.needs.some(
          child =>
            (child.kind === 'continuation' ||
              (child.kind === 'comparison' && row.path.source.kind === 'comparison' &&
                row.path.source.requirement_id === child.requirement_id &&
                prior.some(pair => pair.kind === 'compare_moves' && pair.id === child.requirement_id &&
                  pair.state === 'awaiting_engine' && pair.work_id === reading.work_id))) &&
            (value.needs as CommentaryFactResult['needs']).some(
              need =>
                need.kind === child.kind &&
                need.reason === child.reason &&
                need.requirement_id === child.requirement_id &&
                need.horizon === child.horizon &&
                JSON.stringify(need.prefix_moves_uci) === JSON.stringify(child.prefix_moves_uci),
            ),
        ),
    )
  )
    return false;
  const sampleIndex = new Map(
    prior.flatMap(row => row.result?.samples.map(sample => [sample.id, sample] as const) ?? []),
  );
  for (const sample of samples) {
    const compact = object(sample) && Object.prototype.hasOwnProperty.call(sample, 'prefix_ply_count');
    if (
      !object(sample) ||
      !exactKeys(sample, [
        'id',
        'line_id',
        'binding',
        ...(compact ? ['prefix_ply_count'] : ['moves_uci', 'position_ids', 'occurrence_ids']),
        'from_ply',
        'through_ply',
        'value',
        'state',
        'source_ids',
        'reason',
      ]) ||
      !id(sample.id) ||
      !proposalId(sample.binding) ||
      (compact ? !factPly(sample.prefix_ply_count) : !factMoves(sample.moves_uci)) ||
      !factPly(sample.from_ply) ||
      !factPly(sample.through_ply) ||
      sample.from_ply > sample.through_ply ||
      Number(compact ? sample.prefix_ply_count : (sample.moves_uci as Uci[]).length) < sample.through_ply ||
      (!compact && (!ids(sample.position_ids) || !ids(sample.occurrence_ids))) ||
      !ids(sample.source_ids) ||
      !['known', 'unavailable_input', 'not_applicable'].includes(String(sample.state)) ||
      (sample.state === 'known'
        ? typeof sample.value !== 'string' || sample.reason !== null
        : sample.value !== null || !factText(sample.reason))
    )
      return false;
    const owner =
      reading.kind === 'fact'
        ? reading
        : prior.find(
            row =>
              dependencies.has(row.id) &&
              row.kind === 'fact' &&
              row.result?.samples.some(s => s.id === sample.id),
          );
    if (
      owner?.kind !== 'fact' ||
      sample.binding !== owner.path.binding ||
      (sample.state === 'known' && !factValues(owner.predicate.kind).includes(sample.value as string))
    )
      return false;
    if (
      owner.at.kind === 'ply' &&
      (sample.from_ply < owner.at.ply_count || (!owner.through && sample.from_ply !== owner.at.ply_count))
    )
      return false;
    if (owner.through?.kind === 'ply' && sample.through_ply > owner.through.ply_count) return false;
    const path = observations.paths.find(path => path.id === sample.line_id);
    const prefixLength = Number(compact ? sample.prefix_ply_count : (sample.moves_uci as Uci[]).length);
    if (
      !path ||
      !context ||
      path.kind === 'history' ||
      path.startFen !== context.rootFen ||
      prefixLength > path.moves.length ||
      !ids(path.positionIds?.slice(sample.from_ply, sample.through_ply + 1)) ||
      (!compact && (!factPrefix(sample.moves_uci as Uci[], path.moves.map(move => move.uci)) ||
        JSON.stringify(sample.position_ids) !== JSON.stringify(path.positionIds?.slice(sample.from_ply, sample.through_ply + 1))))
    )
      return false;
    const usedOccurrences = path.occurrenceIds.slice(0, prefixLength);
    if (!ids(usedOccurrences) || (!compact && JSON.stringify(sample.occurrence_ids) !== JSON.stringify(usedOccurrences)))
      return false;
    const usedPositionIds = path.positionIds?.slice(0, prefixLength + 1);
    const resources =
      observations.positions?.filter(position => usedPositionIds?.includes(position.id)) ?? [];
    const sources = new Set<string>([
      ...usedOccurrences.filter((v): v is string => v !== null),
      ...(path.lineSummary?.premise_ids ?? []),
    ]);
    for (const position of resources) {
      sources.add(position.premise_id);
      for (const ref of [
        ...position.controls.pieces.flatMap(p => p.source_ids),
        ...position.rays.lines.map(p => p.source_id),
        ...position.legal_moves.moves.map(p => p.source_id),
        ...position.pawn_states.pawns.flatMap(p => p.source_ids),
        ...position.pawn_files.files.flatMap(p => p.source_ids),
        ...position.pawn_advances.paths.map(p => p.source_id),
      ])
        sources.add(ref);
    }
    if (!sample.source_ids.every(ref => sources.has(ref))) return false;
    if (sampleIndex.has(sample.id)) return false;
    sampleIndex.set(sample.id, sample as unknown as CommentaryFactSample);
  }
  if (new Set(samples.map(s => (s as CommentaryFactSample).id)).size !== samples.length) return false;
  for (const need of value.needs as unknown[]) {
    if (
      !object(need) ||
      !exactKeys(need, ['kind', 'reason', 'prefix_moves_uci', 'requirement_id', 'horizon']) ||
      !['input', 'comparison', 'continuation'].includes(String(need.kind)) ||
      !factText(need.reason) ||
      !factMoves(need.prefix_moves_uci) ||
      (need.horizon !== null && !factPly(need.horizon)) ||
      (need.requirement_id !== null && !all.some(row => row.id === need.requirement_id)) ||
      (need.kind === 'comparison' &&
        !prior.some(row => row.kind === 'compare_moves' && row.id === need.requirement_id)) ||
      (need.kind === 'continuation' &&
        (need.prefix_moves_uci.length === 0 ||
          need.prefix_moves_uci.length >= 80 ||
          !factPly(need.horizon) ||
          need.horizon <= need.prefix_moves_uci.length ||
          !observations.paths.some(
            path =>
              path.kind !== 'history' &&
              path.startFen === context?.rootFen &&
              factPrefix(
                need.prefix_moves_uci as Uci[],
                path.moves.map(move => move.uci),
              ),
          )))
    )
      return false;
  }
  for (const proof of value.proofs as unknown[]) {
    if (
      !object(proof) ||
      !exactKeys(proof, ['bindings', 'sample_ids', 'requirement_ids']) ||
      !object(proof.bindings) ||
      Object.keys(proof.bindings).length > 600 ||
      !Object.entries(proof.bindings).every(([key, moves]) => proposalId(key) && factMoves(moves)) ||
      !factSampleIds(proof.sample_ids) ||
      !proof.sample_ids.length ||
      !ids(proof.requirement_ids) ||
      !proof.requirement_ids.includes(reading.id) ||
      !proof.requirement_ids.every(ref => dependencies.has(ref))
    )
      return false;
    const bindings = proof.bindings as Record<string, Uci[]>;
    const selected = proof.sample_ids.map(ref => sampleIndex.get(ref));
    if (
      selected.some(
        sample =>
          !sample ||
          sample.state !== 'known' ||
          !Object.prototype.hasOwnProperty.call(bindings, sample.binding) ||
          !factPrefix(factSampleMoves(sample, observations), bindings[sample.binding]),
      )
    )
      return false;
    if (
      !Object.entries(bindings).every(([name, moves]) =>
        selected.some(
          sample => sample?.binding === name && JSON.stringify(factSampleMoves(sample, observations)) === JSON.stringify(moves),
        ),
      )
    )
      return false;
    if (
      reading.kind === 'fact' &&
      (selected.some(
        sample =>
          !samples.some(s => (s as CommentaryFactSample).id === sample?.id) ||
          (reading.expected !== undefined && sample?.value !== reading.expected),
      ) ||
        proof.requirement_ids.length !== 1)
    )
      return false;
    if (
      reading.kind === 'fact' &&
      reading.at.kind === 'ply' &&
      (!reading.through || reading.through.kind === 'ply')
    ) {
      const start = reading.at.ply_count,
        end = reading.through?.ply_count ?? start;
      if (
        end < start ||
        Array.from({ length: end - start + 1 }, (_, index) => start + index).some(
          ply => !selected.some(sample => sample && sample.from_ply <= ply && sample.through_ply >= ply),
        )
      )
        return false;
    }
    if (reading.kind === 'fact_comparison') {
      const left = prior.find(row => row.id === reading.left_requirement_id)?.result?.samples ?? [];
      const right = prior.find(row => row.id === reading.right_requirement_id)?.result?.samples ?? [];
      if (
        selected.length !== 2 ||
        !selected.some(
          a =>
            left.some(s => s.id === a?.id) &&
            selected.some(
              b =>
                right.some(s => s.id === b?.id) &&
                a &&
                b &&
                (reading.relation === 'same'
                  ? a.value === b.value
                  : reading.relation === 'different'
                    ? a.value !== b.value
                    : reading.relation === 'before'
                      ? a.from_ply < b.from_ply
                      : reading.relation === 'after'
                        ? a.from_ply > b.from_ply
                        : a.from_ply === b.from_ply),
            ),
        )
      )
        return false;
    }
    if (reading.kind === 'fact_group') {
      const memberSupported = (member: string) =>
        prior
          .find(row => row.id === member && row.result?.state === 'supported')
          ?.result?.proofs.some(
            child =>
              child.requirement_ids.every(ref => (proof.requirement_ids as string[]).includes(ref)) &&
              child.sample_ids.every(ref => (proof.sample_ids as string[]).includes(ref)) &&
              Object.entries(child.bindings).every(
                ([name, moves]) => bindings[name] && factPrefix(moves, bindings[name]),
              ),
          );
      if (
        !(reading.mode === 'all'
          ? reading.members.every(memberSupported)
          : reading.members.some(memberSupported))
      )
        return false;
    }
  }
  if (value.state === 'refuted' && (value.proofs as unknown[]).length) return false;
  if (['supported', 'observed'].includes(String(value.state)) && !(value.proofs as unknown[]).length)
    return false;
  if (reading.kind === 'fact') {
    if (
      (value.state === 'observed' && reading.expected !== undefined) ||
      (value.state === 'supported' && reading.expected === undefined)
    )
      return false;
    if (
      value.state === 'refuted' &&
      (reading.path.extend ||
        reading.expected === undefined ||
        !samples.some(s => object(s) && s.state === 'known' && s.value !== reading.expected))
    )
      return false;
  } else if (value.state === 'observed') return false;
  if (value.state === 'refuted' && reading.kind === 'fact_group') {
    const children = reading.members.map(member => prior.find(row => row.id === member)?.result);
    if (
      !(reading.mode === 'any'
        ? children.every(child => child?.state === 'refuted')
        : children.some(child => child?.state === 'refuted') ||
          (children.every(child => child?.state === 'supported') && value.reason === 'incompatible_paths'))
    )
      return false;
  }
  if (
    value.state === 'refuted' &&
    reading.kind === 'fact_comparison' &&
    ![reading.left_requirement_id, reading.right_requirement_id].every(member => {
      const row = prior.find(row => row.id === member);
      return (
        row?.kind === 'fact' &&
        !row.path.extend &&
        row.result &&
        ['observed', 'supported', 'refuted'].includes(row.result.state) &&
        !row.result.needs.some(need => need.reason === 'witness_limit')
      );
    })
  )
    return false;
  return true;
}

function decodeProposals(
  value: unknown,
  observations: MoveReviewObservations,
  context: CommentaryInquiryContext | undefined,
): CommentaryProposal[] | undefined {
  if (!Array.isArray(value) || value.length > 600) return;
  const requestId = (value: unknown): value is string =>
    typeof value === 'string' && /^[A-Za-z0-9._:-]{1,128}$/.test(value);
  const boundedText = (value: unknown, maximum: number): value is string =>
    typeof value === 'string' && value.length > 0 && value.length <= maximum;
  const uci = (value: unknown): value is Uci =>
    typeof value === 'string' && /^[a-h][1-8][a-h][1-8][qrbn]?$/.test(value);
  const nullableId = (value: unknown) => value === null || id(value);
  const source = (value: unknown): value is CommentaryProposalSource =>
    object(value) &&
    exactKeys(value, ['kind', 'producer', 'source_refs'], ['version']) &&
    ['user', 'author', 'model', 'generator', 'none'].includes(String(value.kind)) &&
    boundedText(value.producer, 128) &&
    (value.version === undefined || boundedText(value.version, 128)) &&
    Array.isArray(value.source_refs) &&
    value.source_refs.length <= 64 &&
    value.source_refs.every(ref => boundedText(ref, 256));
  const proposals: CommentaryProposal[] = [];
  let requirementCount = 0;
  for (const raw of value) {
    if (
      !object(raw) ||
      !exactKeys(raw, ['id', 'admission', 'origin', 'requirements'], ['question_text', 'answer_text']) ||
      !requestId(raw.id) ||
      !['submitted', 'internal', 'generated'].includes(String(raw.admission)) ||
      (('question_text' in raw || 'answer_text' in raw) &&
        (raw.admission !== 'generated' || !boundedText(raw.question_text, 16000) || !boundedText(raw.answer_text, 16000))) ||
      !object(raw.origin) ||
      !exactKeys(raw.origin, ['question', 'candidates', 'priority', 'evidence']) ||
      !Object.values(raw.origin).every(source) ||
      !Array.isArray(raw.requirements) ||
      !raw.requirements.length ||
      (requirementCount += raw.requirements.length) > 600
    )
      return;
    const requirements: CommentaryProposalReading[] = [];
    for (const reading of raw.requirements) {
      if (!object(reading)) return;
      const semantic = ['fact', 'fact_comparison', 'fact_group'].includes(String(reading.kind));
      let decodedAnswer: CommentaryInquiryAnswer | null = null;
      const operationKeys =
        reading.kind === 'observe_move'
          ? ['prefix_moves_uci', 'move_uci']
          : reading.kind === 'compare_moves'
            ? ['prefix_moves_uci', 'reference_move', 'alternative_move']
            : reading.kind === 'uninterpreted'
              ? ['text']
              : reading.kind === 'fact'
                ? ['path', 'predicate', 'at']
                : reading.kind === 'fact_comparison'
                  ? ['left_requirement_id', 'right_requirement_id', 'relation']
                  : reading.kind === 'fact_group'
                    ? ['mode', 'members']
                    : undefined;
      if (
        !operationKeys ||
        !exactKeys(
          reading,
          [
            'id',
            'kind',
            'state',
            'stop_reason',
            'answer',
            'line_id',
            'occurrence_id',
            'position_id',
            'work_id',
            ...operationKeys,
          ],
          reading.kind === 'compare_moves'
            ? ['expected_relation']
            : semantic
              ? ['result', ...(reading.kind === 'fact' ? ['through', 'expected'] : [])]
              : [],
        ) ||
        !requestId(reading.id) ||
        ![
          'deferred',
          'awaiting_engine',
          'observed',
          'answered',
          'supported',
          'refuted',
          'partial',
          'not_applicable',
          'invalid_input',
          'unavailable_input',
          'unsupported_reading',
          'stopped',
        ].includes(String(reading.state)) ||
        (reading.stop_reason !== null && !boundedText(reading.stop_reason, 256)) ||
        ![reading.line_id, reading.occurrence_id, reading.position_id].every(nullableId) ||
        (reading.work_id !== null &&
          (typeof reading.work_id !== 'string' || !/^work:[0-9]+$/.test(reading.work_id)))
      )
        return;
      if (
        (reading.line_id !== null && !observations.paths.some(path => path.id === reading.line_id)) ||
        (reading.occurrence_id !== null &&
          !observations.paths.some(path => path.occurrenceIds.includes(reading.occurrence_id as string))) ||
        (reading.position_id !== null &&
          !observations.positions?.some(position => position.id === reading.position_id))
      )
        return;
      const resolvedPair =
        reading.kind === 'compare_moves' &&
        ['answered', 'supported', 'refuted'].includes(String(reading.state));
      if (
        reading.kind === 'observe_move' &&
        !['deferred', 'observed', 'invalid_input', 'unavailable_input', 'stopped'].includes(
          String(reading.state),
        )
      )
        return;
      if (
        reading.kind === 'compare_moves' &&
        ![
          'deferred',
          'awaiting_engine',
          'answered',
          'supported',
          'refuted',
          'invalid_input',
          'unavailable_input',
          'stopped',
        ].includes(String(reading.state))
      )
        return;
      if (
        resolvedPair !== (reading.answer !== null) ||
        (!semantic &&
          ['observed', 'answered', 'supported', 'refuted', 'awaiting_engine'].includes(
            String(reading.state),
          ) &&
          reading.stop_reason !== null) ||
        ((!semantic || reading.state === 'stopped' || reading.result === undefined) &&
          ['invalid_input', 'unavailable_input', 'unsupported_reading', 'stopped'].includes(
            String(reading.state),
          ) &&
          reading.stop_reason === null)
      )
        return;
      if (semantic) {
        if (
          !factRequirementValid(reading, requirements) ||
          reading.state === 'answered' ||
          reading.state === 'unsupported_reading' ||
          (reading.state === 'awaiting_engine' && reading.work_id === null) ||
          (reading.state === 'awaiting_engine' && reading.stop_reason !== null) ||
          (reading.result === undefined
            ? !['deferred', 'invalid_input', 'stopped'].includes(String(reading.state))
            : !factResultValid(
                reading.result,
                reading as unknown as CommentaryProposalReading,
                requirements,
                observations,
                context,
              ))
        )
          return;
      } else if (reading.kind === 'uninterpreted') {
        if (
          !boundedText(reading.text, 2000) ||
          reading.state !== 'unsupported_reading' ||
          [reading.answer, reading.line_id, reading.occurrence_id, reading.position_id, reading.work_id].some(
            value => value !== null,
          )
        )
          return;
      } else {
        const prefix = reading.prefix_moves_uci;
        if (!Array.isArray(prefix) || prefix.length > 79 || !prefix.every(uci)) return;
        if (reading.kind === 'observe_move') {
          if (
            !uci(reading.move_uci) ||
            resolvedPair ||
            reading.state === 'awaiting_engine' ||
            reading.work_id !== null
          )
            return;
          if (reading.state === 'observed') {
            const path = observations.paths.find(path => path.id === reading.line_id);
            if (
              !path ||
              !context ||
              path.kind === 'history' ||
              path.startFen !== context.rootFen ||
              !prefix.every((move, index) => path.moves[index]?.uci === move) ||
              path.moves[prefix.length]?.uci !== reading.move_uci ||
              path.occurrenceIds[prefix.length] !== reading.occurrence_id ||
              path.positionIds?.[prefix.length + 1] !== reading.position_id
            )
              return;
          }
        } else {
          if (
            !uci(reading.reference_move) ||
            !uci(reading.alternative_move) ||
            reading.reference_move === reading.alternative_move ||
            reading.state === 'observed' ||
            (reading.expected_relation !== undefined &&
              !['better', 'equal', 'worse'].includes(String(reading.expected_relation))) ||
            (reading.state === 'awaiting_engine' && reading.work_id === null)
          )
            return;
          if (resolvedPair) {
            if (!object(reading.answer)) return;
            const rawAnswer = reading.answer;
            const path = observations.paths.find(path => path.id === rawAnswer.reference_line_id);
            const positionId = path?.positionIds?.[prefix.length];
            if (
              !path ||
              !positionId ||
              !prefix.every((move, index) => path.moves[index]?.uci === move) ||
              (reading.position_id !== null && reading.position_id !== positionId)
            )
              return;
            const rootOccurrenceId = path.occurrenceIds[0];
            if (!rootOccurrenceId) return;
            const answer = decodeComparisonAnswer(
              reading.answer,
              observations,
              context,
              positionId,
              reading.reference_move,
              reading.alternative_move,
              prefix.length
                ? {
                    rootMove: prefix[0],
                    rootOccurrenceId,
                    prefixMoves: prefix,
                    scopeFields: false,
                  }
                : undefined,
            );
            const expectedState =
              reading.expected_relation === undefined
                ? 'answered'
                : reading.expected_relation === answer?.evaluation_relation
                  ? 'supported'
                  : 'refuted';
            if (!answer || answer.work_id !== reading.work_id || reading.state !== expectedState) return;
            decodedAnswer = answer;
          }
        }
      }
      requirements.push({ ...reading, answer: decodedAnswer } as unknown as CommentaryProposalReading);
    }
    if (new Set(requirements.map(reading => reading.id)).size !== requirements.length) return;
    proposals.push({ ...raw, requirements } as unknown as CommentaryProposal);
  }
  return new Set(proposals.map(proposal => proposal.id)).size === proposals.length ? proposals : undefined;
}

function proposalFactSourcesValid(
  proposals: CommentaryProposal[],
  observations: MoveReviewObservations,
  continuations: CommentaryContinuationInquiry[],
): boolean {
  return proposals.every(proposal =>
    proposal.requirements.every(reading => {
      if (reading.kind !== 'fact') return true;
      const source = reading.path.source;
      const supplied =
        source.kind === 'declared'
          ? [source.moves_uci]
          : source.kind === 'comparison'
            ? (() => {
                const pair = proposal.requirements.find(row => row.id === source.requirement_id);
                const answer = pair?.kind === 'compare_moves' ? pair.answer : undefined;
                const line = observations.paths.find(
                  path =>
                    path.id ===
                    (source.arm === 'reference' ? answer?.reference_line_id : answer?.alternative_line_id),
                );
                return line ? [line.moves.map(move => move.uci)] : [];
              })()
            : (() => {
                const premise = continuations
                  .find(row => row.id === source.inquiry_id)
                  ?.premises.find(row => row.id === source.premise_id);
                return observations.paths
                  .filter(path => premise?.witness_line_ids.includes(path.id))
                  .map(path => path.moves.map(move => move.uci));
              })();
      const sourcePrefix =
        source.kind === 'premise' && !supplied.length
          ? continuations.find(row => row.id === source.inquiry_id)?.prefix_moves_uci
          : undefined;
      return (
        (reading.result?.samples ?? []).every(sample =>
          supplied.some(
            moves =>
              (factPrefix(moves, factSampleMoves(sample, observations)) &&
                (reading.path.extend || factPrefix(factSampleMoves(sample, observations), moves))) ||
              (factPrefix(factSampleMoves(sample, observations), moves) &&
                reading.result?.needs.some(need => need.reason === 'declared_path_unavailable') &&
                !proposal.requirements.some(row =>
                  row.result?.proofs.some(proof => proof.sample_ids.includes(sample.id)),
                )),
          ),
        ) &&
        (reading.result?.needs ?? [])
          .filter(need => need.kind === 'continuation')
          .every(
            need =>
              reading.path.extend &&
              [...supplied, ...(sourcePrefix ? [sourcePrefix] : [])].some(moves =>
                factCompatible(moves, need.prefix_moves_uci),
              ),
          )
      );
    }),
  );
}

function proposalContainsResponsePool(
  proposals: CommentaryProposal[],
  search: CommentaryResponseSearch,
): boolean {
  if (search.root_restriction.kind !== 'restricted') return false;
  const pool = search.root_restriction.moves_uci;
  const prefix = responseSearchPrefix(search);
  return proposals.some(proposal =>
    proposal.requirements.some(
      reading =>
        reading.kind === 'compare_moves' &&
        // A question can reopen at a deeper depth. Its old, independently validated
        // search still belongs to this exact pool, but is not its current answer.
        reading.prefix_moves_uci.length === prefix.length &&
        reading.prefix_moves_uci.every((move, index) => move === prefix[index]) &&
        sameIds(pool, [reading.reference_move, reading.alternative_move]),
    ),
  );
}

function decodeDiscovery(
  value: unknown,
  observations: MoveReviewObservations,
  searches: CommentaryResponseSearch[],
): CommentaryDiscovery | undefined {
  if (
    !object(value) || !exactKeys(value, ['version', 'input_key', 'scans']) || value.version !== 1 ||
    !factText(value.input_key) || !Array.isArray(value.scans) || value.scans.length > 600
  ) return;
  const moves = (value: unknown): value is Uci[] =>
    Array.isArray(value) && value.length <= 218 && value.every(move => factMoves([move])) &&
    new Set(value).size === value.length;
  const anchors = new Set<string>();
  for (const scan of value.scans) {
    if (!object(scan) || !exactKeys(scan, ['prefix_moves_uci', 'reference_move', 'legal_moves',
      'examined_moves', 'deferred_request_ids', 'pending_replay_prefixes', 'state', 'reason'])) return;
    const prefix = scan.prefix_moves_uci, legal = scan.legal_moves, examined = scan.examined_moves;
    if (
      !factMoves(prefix) || prefix.length >= 80 || !factMoves([scan.reference_move]) || !moves(legal) ||
      !moves(examined) || !examined.every(move => legal.includes(move)) ||
      !Array.isArray(scan.deferred_request_ids) || !scan.deferred_request_ids.every(proposalId) ||
      new Set(scan.deferred_request_ids).size !== scan.deferred_request_ids.length ||
      !Array.isArray(scan.pending_replay_prefixes) || !scan.pending_replay_prefixes.every(pending =>
        factMoves(pending) && pending.length > prefix.length && factPrefix(prefix, pending)) ||
      new Set(scan.pending_replay_prefixes.map(pending => pending.join(' '))).size !== scan.pending_replay_prefixes.length ||
      !['complete', 'partial', 'unavailable', 'not_applicable'].includes(String(scan.state)) ||
      !(scan.reason === null || factText(scan.reason)) ||
      (scan.state === 'complete' && (!sameIds(examined, legal) || scan.deferred_request_ids.length > 0 ||
        scan.pending_replay_prefixes.length > 0))
    ) return;
    if (anchors.has(prefix.join(' '))) return;
    anchors.add(prefix.join(' '));
    if (prefix.length && !searches.some(search => {
      const searched = responseSearchPrefix(search);
      return searched.length === prefix.length && factPrefix(prefix, searched);
    })) return;
    const path = observations.paths.find(path => path.kind !== 'history' &&
      factPrefix(prefix, path.moves.map(move => move.uci)));
    const position = observations.positions?.find(position => position.id === path?.positionIds?.[prefix.length]);
    if ((legal.length || scan.state === 'complete') && (!position?.legal_moves.complete ||
      !sameIds(legal, position.legal_moves.moves.map(move => move.uci)) ||
      !legal.includes(scan.reference_move as Uci))) return;
  }
  return value as unknown as CommentaryDiscovery;
}

/** Selection never supplies board facts. All locations resolve inside one admitted response. */
export function decodeCommentarySelection(
  value: unknown,
  observations: MoveReviewObservations,
  context?: CommentaryInquiryContext,
): CommentarySelection | undefined {
  if (
    !object(value) ||
    !exactKeys(
      value,
      ['version', 'candidates', 'scenes'],
      [
        'inquiries',
        'response_inquiries',
        'response_searches',
        'root_searches',
        'continuation_inquiries',
        'proposals',
        'local_expansions_used',
        'discovery',
      ],
    ) ||
    value.version !== 1 ||
    !Array.isArray(value.candidates) ||
    value.candidates.length > 10000 ||
    !Array.isArray(value.scenes) ||
    value.scenes.length > 10000
  )
    return;
  const paths = new Map(observations.paths.map(p => [p.id, p]));
  const occurrences = new Set(observations.paths.flatMap(p => p.occurrenceIds).filter(id));
  const positions = new Set(observations.positions?.map(p => p.id));
  let responseSearches: CommentaryResponseSearch[] | undefined;
  let rootSearches: CommentaryRootSearch[] | undefined;
  let responseInquiries: CommentaryResponseInquiry[] | undefined;
  let continuationInquiries: CommentaryContinuationInquiry[] | undefined;
  let proposals: CommentaryProposal[] | undefined;
  let discovery: CommentaryDiscovery | undefined;
  if (value.discovery !== undefined &&
      (value.proposals === undefined || value.local_expansions_used === undefined)) return;
  if (value.proposals !== undefined || value.local_expansions_used !== undefined) {
    if (!integer(value.local_expansions_used, 0, 600)) return;
    try {
      proposals = decodeProposals(value.proposals, observations, context);
    } catch (_) {
      return;
    }
    if (!proposals) return;
  }
  if (value.root_searches !== undefined) {
    rootSearches = decodeRootSearches(value.root_searches, observations, context);
    if (!rootSearches) return;
  }
  if (value.response_searches !== undefined) {
    try {
      responseSearches = decodeResponseSearches(value.response_searches, observations, context);
    } catch (_) {
      return;
    }
    if (!responseSearches) return;
  }
  if (value.discovery !== undefined) {
    discovery = decodeDiscovery(value.discovery, observations, responseSearches ?? []);
    if (!discovery) return;
  }
  if (value.response_inquiries !== undefined) {
    try {
      responseInquiries = decodeResponseInquiries(
        value.response_inquiries,
        observations,
        context,
        responseSearches,
      );
    } catch (_) {
      return;
    }
    if (!responseInquiries) return;
  }
  if (value.continuation_inquiries !== undefined) {
    try {
      continuationInquiries = decodeContinuationInquiries(
        value.continuation_inquiries,
        observations,
        context,
        responseSearches ?? [],
      );
    } catch (_) {
      return;
    }
    if (!continuationInquiries) return;
  }
  if (
    responseSearches?.some(
      search =>
        search.root_restriction.kind === 'restricted' &&
        !responsePoolMatchesClasses(
          responseInquiries?.filter(inquiry => responseSearchMatchesInquiry(search, inquiry)) ?? [],
          search.root_restriction.moves_uci,
        ) &&
        !proposalContainsResponsePool(proposals ?? [], search) &&
        !continuationInquiries?.some(inquiry => {
          if (
            !responseSearchMatchesContinuation(search, inquiry) ||
            search.root_restriction.kind !== 'restricted'
          )
            return false;
          const priorFull = responseSearches!
            .filter(
              prior =>
                Number(prior.work_id.slice(5)) < Number(search.work_id.slice(5)) &&
                responseSearchMatchesContinuation(prior, inquiry) &&
                (prior.root_restriction.kind === 'unrestricted' ||
                  inquiry.legal_moves.every(
                    move =>
                      prior.root_restriction.kind === 'restricted' &&
                      prior.root_restriction.moves_uci.includes(move),
                  )),
            )
            .sort((left, right) => Number(right.work_id.slice(5)) - Number(left.work_id.slice(5)))[0];
          return (
            !!priorFull &&
            continuationRoutePoolMatches(
              inquiry,
              search.root_restriction.moves_uci,
              false,
              priorFull.candidates[0].move_uci,
            )
          );
        }),
    )
  )
    return;
  const candidates: CommentaryCandidate[] = [];
  for (const raw of value.candidates) {
    if (
      !object(raw) ||
      !exactKeys(
        raw,
        [
          'id',
          'kind',
          'state',
          'selection',
          'root_occurrence_id',
          'occurrence_ids',
          'position_ids',
          'line_ids',
          'evidence_ids',
          'source_refs',
          'detail',
        ],
        ['highlight'],
      ) ||
      !id(raw.id) ||
      !state(raw.state) ||
      !['local_fact', 'conditional_line', 'comparison'].includes(String(raw.kind)) ||
      !['selected', 'display_cap', 'display_representative', 'context'].includes(String(raw.selection)) ||
      !id(raw.root_occurrence_id) ||
      !occurrences.has(raw.root_occurrence_id) ||
      !ids(raw.occurrence_ids) ||
      !raw.occurrence_ids.includes(raw.root_occurrence_id) ||
      !raw.occurrence_ids.every(v => occurrences.has(v)) ||
      !ids(raw.position_ids) ||
      !raw.position_ids.every(v => positions.has(v)) ||
      !ids(raw.line_ids) ||
      !raw.line_ids.every(v => paths.has(v)) ||
      !ids(raw.evidence_ids) ||
      !Array.isArray(raw.source_refs) ||
      raw.source_refs.length > 600 ||
      !object(raw.detail) ||
      !id(raw.detail.type)
    )
      return;
    if (
      raw.detail.text !== undefined &&
      (typeof raw.detail.text !== 'string' || !raw.detail.text.trim() || raw.detail.text.length > 4000)
    )
      return;
    const usedPaths = raw.line_ids.map(v => paths.get(v)!);
    // A missing comparison path may leave only its actual decision anchor. It
    // must still be an admitted occurrence, never a fabricated missing edge.
    if (
      !raw.occurrence_ids.every(
        v => v === raw.root_occurrence_id || usedPaths.some(p => p.occurrenceIds.includes(v)),
      )
    )
      return;
    if (usedPaths.length && !raw.position_ids.every(v => usedPaths.some(p => p.positionIds?.includes(v))))
      return;
    const sourceRefs: CommentaryCandidate['sourceRefs'] = [];
    for (const source of raw.source_refs) {
      if (
        !object(source) ||
        !exactKeys(source, ['line_id', 'kind', 'source', 'accepted_ply_count', 'stop']) ||
        !id(source.line_id) ||
        !raw.line_ids.includes(source.line_id) ||
        !['supplied', 'evaluation', 'admitted'].includes(String(source.kind)) ||
        ![
          'pgn_record',
          'analysis',
          'study',
          'direct_input',
          'root_search',
          'focus_comparison',
          'investigation_comparison',
          'response_comparison',
          'legal_replay',
        ].includes(String(source.source)) ||
        !Number.isInteger(source.accepted_ply_count) ||
        (source.accepted_ply_count as number) < 0 ||
        (source.accepted_ply_count as number) > 600 ||
        (source.stop !== null &&
          !['invalid_continuation', 'automatic_terminal', 'input_limit'].includes(String(source.stop)))
      )
        return;
      if (
        source.kind === 'supplied' &&
        !paths.get(source.line_id)?.sources?.includes(source.source as 'pgn_record' | 'analysis' | 'study')
      )
        return;
      if ((source.accepted_ply_count as number) < paths.get(source.line_id)!.moves.length) return;
      sourceRefs.push(source as CommentaryCandidate['sourceRefs'][number]);
    }
    let decodedDetail = raw.detail;
    if (raw.kind === 'comparison' && raw.detail.type === 'question_comparison') {
      const detail = raw.detail, prefix = detail.prefix_moves_uci, refs = detail.question_refs,
        occurrenceIds = raw.occurrence_ids;
      if (
        !context || 'highlight' in raw ||
        !exactKeys(detail, ['type', 'scope', 'prefix_moves_uci', 'reference_move', 'alternative_move',
          'perspective', 'question_refs', 'evaluation_relation', 'stop_reason', 'text']) ||
        detail.scope !== 'same_position_pair' || !factMoves(prefix) || prefix.length >= 80 ||
        !factMoves([detail.reference_move, detail.alternative_move]) || detail.reference_move === detail.alternative_move ||
        !['white', 'black'].includes(String(detail.perspective)) ||
        typeof detail.text !== 'string' || !detail.text.length || detail.text.length > 4000 ||
        ![null, 'better', 'equal', 'worse'].includes(detail.evaluation_relation as null | string) ||
        !(detail.stop_reason === null || factText(detail.stop_reason)) ||
        !Array.isArray(refs) || !refs.length || refs.length > 600 ||
        !refs.every(ref => object(ref) && exactKeys(ref, ['proposal_id', 'requirement_id']) &&
          proposalId(ref.proposal_id) && proposalId(ref.requirement_id)) ||
        new Set(refs.map(ref => `${ref.proposal_id}/${ref.requirement_id}`)).size !== refs.length ||
        !['selected', 'context'].includes(String(raw.selection)) || !usedPaths.length
      ) return;
      const readings = refs.flatMap(ref => {
        const reading = proposals?.find(proposal => proposal.id === ref.proposal_id)?.requirements
          .find(reading => reading.id === ref.requirement_id);
        return reading?.kind === 'compare_moves' ? [reading] : [];
      });
      if (readings.length !== refs.length || readings.some(reading =>
        reading.prefix_moves_uci.length !== prefix.length || !factPrefix(prefix, reading.prefix_moves_uci) ||
        reading.reference_move !== detail.reference_move || reading.alternative_move !== detail.alternative_move ||
        reading.stop_reason !== detail.stop_reason
      )) return;
      const answer = readings[0].answer;
      if (readings.some(reading => answer === null ? reading.answer !== null :
        reading.answer === null || Object.entries(answer).some(([key, value]) =>
          JSON.stringify(reading.answer![key as keyof CommentaryInquiryAnswer]) !== JSON.stringify(value)))) return;
      const comparisonId = usedPaths[0].positionIds?.[prefix.length];
      const comparison = observations.positions?.find(position => position.id === comparisonId);
      const actualRootPath = observations.paths.find(path =>
        path.kind !== 'history' && path.startFen === context.rootFen && path.moves[0]?.uci === context.playedMove &&
        path.occurrenceIds[0] === raw.root_occurrence_id
      );
      if (!comparison || !raw.position_ids.includes(comparison.id) || !actualRootPath ||
        (!prefix.length && (comparison.id !== actualRootPath.positionIds?.[0] || comparison.fen !== context.rootFen)) ||
        usedPaths.some(path =>
        path.kind === 'history' || path.startFen !== context.rootFen ||
        path.positionIds?.[prefix.length] !== comparison.id ||
        !factPrefix(prefix, path.moves.map(move => move.uci))
      )) return;
      try {
        const position = setupPosition('chess', parseFen(comparison.fen).unwrap()).unwrap();
        if (detail.perspective !== position.turn || ![detail.reference_move, detail.alternative_move].every(uci => {
          const move = parseUci(uci as Uci);
          return move && position.isLegal(move);
        })) return;
      } catch (_) { return; }
      if (answer) {
        if (raw.state !== 'ready' || detail.evaluation_relation !== answer.evaluation_relation ||
          !sameIds(raw.line_ids, [answer.reference_line_id, answer.alternative_line_id]) ||
          ![answer.reference_occurrence_id, answer.alternative_occurrence_id].every(id => occurrenceIds.includes(id))) return;
      } else if (raw.state !== 'partial' || raw.selection !== 'context' || detail.evaluation_relation !== null) return;
    } else if (raw.kind === 'comparison' && raw.detail.type === 'line_continuation_comparison') {
      // Both referenced line profiles are decoded before the pair is validated below.
    } else if (raw.kind === 'comparison' && raw.detail.type === 'target_response_comparison') {
      try {
        const detail = decodeTargetResponse(raw, observations, responseInquiries ?? []);
        if (!detail) return;
        decodedDetail = detail as unknown as Record<string, unknown>;
      } catch (_) {
        return;
      }
    } else if (raw.kind === 'comparison' && raw.detail.type === 'pv_comparison') {
      try {
        const detail = decodePvComparison(raw, observations, context);
        if (!detail) return;
        decodedDetail = detail as unknown as Record<string, unknown>;
      } catch (_) {
        return;
      }
    } else if (raw.kind === 'comparison') {
      const detail = raw.detail;
      const reference = detail.reference_line_id;
      const candidate = detail.candidate_line_id;
      const available = (value: unknown): value is string | null =>
        value === null || (id(value) && (raw.line_ids as string[]).includes(value));
      if (
        !['evaluation_comparison', 'observed_material_comparison'].includes(raw.detail.type) ||
        !available(reference) ||
        !available(candidate) ||
        (reference !== null && reference === candidate) ||
        !ids(detail.missing_line_ids) ||
        detail.missing_line_ids.some(v => paths.has(v)) ||
        detail.missing_line_ids.length !== Number(reference === null) + Number(candidate === null) ||
        !id(detail.comparison_evidence_id) ||
        !raw.evidence_ids.includes(detail.comparison_evidence_id) ||
        !['played_vs_best', 'best_vs_second'].includes(String(detail.comparison_kind)) ||
        detail.scope !== 'observed_lines'
      )
        return;
      if ((reference === null || candidate === null) && raw.state !== 'partial') return;
      const expectedRelation =
        raw.state === 'ready' ? 'different' : raw.state === 'needs_explanation' ? 'equal' : 'unavailable';
      if (
        detail.material_relation !== expectedRelation ||
        (raw.state === 'ready' && detail.explanation_gap !== null) ||
        (raw.state === 'needs_explanation' && detail.explanation_gap !== 'beyond_material_scope') ||
        (raw.state === 'partial' &&
          !['missing_path', 'incomplete_line', 'material_evidence_unavailable'].includes(
            String(detail.explanation_gap),
          )) ||
        ((reference === null || candidate === null) && detail.explanation_gap !== 'missing_path')
      )
        return;
      // A score-only comparison is not a completed account of its chess reason.
      if ((detail.type === 'observed_material_comparison') !== (raw.state === 'ready')) return;
      const left = reference === null ? undefined : paths.get(reference)?.lineSummary;
      const right = candidate === null ? undefined : paths.get(candidate)?.lineSummary;
      if (raw.state !== 'partial' && (!left?.material || !right?.material)) return;
      if (
        raw.state !== 'partial' &&
        left &&
        right &&
        (exchangeSignature(left) === exchangeSignature(right)) !== (detail.material_relation === 'equal')
      )
        return;
      const hasMaterial = ['perspective', 'reference_net_cp', 'candidate_net_cp'].some(k => k in detail);
      if (
        hasMaterial &&
        (!left?.material ||
          !right?.material ||
          detail.perspective !== left.perspective ||
          left.perspective !== right.perspective ||
          detail.reference_net_cp !== left.material.net_cp ||
          detail.candidate_net_cp !== right.material.net_cp)
      )
        return;
      if (raw.state !== 'partial' && !hasMaterial) return;
      if (
        raw.state === 'ready' &&
        !raw.line_ids.every(lineId =>
          sourceRefs.some(
            source =>
              source.line_id === lineId &&
              (source.stop === null || source.stop === 'automatic_terminal') &&
              source.accepted_ply_count === paths.get(lineId)!.moves.length,
          ),
        )
      )
        return;
    } else if (raw.kind === 'conditional_line') {
      if (raw.detail.type === 'line_continuation') {
        try {
          const detail = decodeLineContinuation(raw, observations);
          if (!detail) return;
          decodedDetail = detail as unknown as Record<string, unknown>;
        } catch (_) {
          return;
        }
      } else if (
        raw.detail.type !== 'observed_line' ||
        raw.detail.scope !== 'observed_line' ||
        raw.line_ids.length !== 1 ||
        raw.selection !== 'context'
      )
        return;
    } else if (raw.state !== 'ready') {
      return;
    }
    candidates.push({
      id: raw.id,
      kind: raw.kind as CommentaryCandidate['kind'],
      state: raw.state,
      selection: raw.selection as CommentaryCandidate['selection'],
      rootOccurrenceId: raw.root_occurrence_id,
      occurrenceIds: raw.occurrence_ids,
      positionIds: raw.position_ids,
      lineIds: raw.line_ids,
      evidenceIds: raw.evidence_ids,
      sourceRefs,
      detail: decodedDetail as CommentaryCandidate['detail'],
    });
  }
  if (new Set(candidates.map(c => c.id)).size !== candidates.length) return;
  const byId = new Map(candidates.map(c => [c.id, c]));
  try {
    if (
      candidates.some(
        candidate =>
          continuationComparisonDetail(candidate) &&
          !validContinuationComparison(candidate, byId, observations),
      )
    )
      return;
  } catch (_) {
    return;
  }
  const linkedProfiles = (idsValue: unknown, lineIds: string[], root: string): boolean =>
    ids(idsValue) &&
    idsValue.every(id => {
      const candidate = byId.get(id);
      const detail = candidate && lineContinuationDetail(candidate);
      return (
        !!candidate && candidate.rootOccurrenceId === root && !!detail && lineIds.includes(detail.line_id)
      );
    });
  const linkedComparisons = (idsValue: unknown, lineIds: string[], root: string): boolean =>
    ids(idsValue) &&
    idsValue.every(id => {
      const candidate = byId.get(id);
      return (
        !!candidate &&
        candidate.rootOccurrenceId === root &&
        !!continuationComparisonDetail(candidate) &&
        candidate.lineIds.every(line => lineIds.includes(line))
      );
    });
  const continuationByLine = new Map(
    candidates.flatMap(candidate => {
      const detail = lineContinuationDetail(candidate);
      return detail ? [[detail.line_id, detail] as const] : [];
    }),
  );
  const continuationPaths = new Map(observations.paths.map(path => [path.id, path]));
  const continuationPieces = new Map<ObservationPath, Map<number, CommentaryTrackedPiece[]>>();
  const continuationPiecesAt = (path: ObservationPath, ply: number): CommentaryTrackedPiece[] => {
    const snapshots = continuationPieces.get(path) ?? new Map<number, CommentaryTrackedPiece[]>();
    if (!snapshots.has(ply)) snapshots.set(ply, trackedPieces(path, ply) ?? []);
    continuationPieces.set(path, snapshots);
    return snapshots.get(ply)!;
  };
  const followupWitnesses = (
    peer: ObservationPath,
    loss: CommentaryLineContinuationDetail['events'][number],
    recovery?: CommentaryLineContinuationDetail['events'][number],
  ): string[] =>
    [...continuationByLine.values()].flatMap(detail => {
      const path = continuationPaths.get(detail.line_id)!;
      if (!peer.occurrenceIds.every((id, index) => path.occurrenceIds[index] === id)) return [];
      const sameLoss = detail.events.find(
        event =>
          event.occurrence_id === loss.occurrence_id &&
          event.original_actor &&
          loss.original_actor &&
          samePiece(event.original_actor, loss.original_actor) &&
          event.original_victim &&
          loss.original_victim &&
          samePiece(event.original_victim, loss.original_victim),
      );
      if (!sameLoss) return [];
      const outcomes = continuationRecoveries(detail, sameLoss.id);
      const material = path.lineSummary?.material;
      const sign = sameLoss.original_victim!.side === path.lineSummary?.perspective ? 1 : -1;
      const observed = recovery
        ? detail.events.some(
            event =>
              event.ply_offset > sameLoss.ply_offset &&
              event.original_victim &&
              recovery.original_victim &&
              samePiece(event.original_victim, recovery.original_victim),
          ) ||
          (outcomes.length > 0 && !!material && material.net_cp * sign >= 0)
        : outcomes.some(event => event.ply_offset >= peer.moves.length);
      return observed ? [path.id] : [];
    });
  // Share the certified recapture family across discovery, scopes and transferred support premises.
  const recaptureSourcesAt = (sourceLineIds: string[], count: number) =>
    sourceLineIds.flatMap(lineId => {
      const detail = continuationByLine.get(lineId),
        path = continuationPaths.get(lineId);
      const position = observations.positions?.find(position => position.id === path?.positionIds?.[count]);
      if (!detail || !path || !position?.legal_moves.complete) return [];
      const pieces = continuationPiecesAt(path, count);
      return [...(detail.support_applications ?? []), ...(detail.direct_support_recaptures ?? [])].flatMap(
        use => {
          const comparison = detail.support_comparisons.find(
            comparison => comparison.id === use.comparison_id,
          )!;
          if (
            comparison.kind !== 'replacement_after_capture' ||
            path.positionIds!.indexOf(comparison.after_position_id) - 1 !== count
          )
            return [];
          const recovery = detail.events.find(
            event =>
              event.ply_offset === count &&
              event.original_actor &&
              samePiece(event.original_actor, comparison.after_actor),
          );
          const capturer =
            recovery?.original_victim &&
            pieces.find(piece => samePiece(piece.original, recovery.original_victim!))?.current;
          if (!capturer) return [];
          const rows = position.legal_moves.moves.filter(move => move.captured_square === capturer.square);
          const actors = rows.flatMap(
            move =>
              pieces.find(piece => piece.current.square === move.uci.slice(0, 2))?.original.square ?? [],
          );
          return actors.length === rows.length && new Set(actors).size >= 2
            ? [
                {
                  detail,
                  path,
                  recovery: recovery!,
                  use,
                  comparison,
                  moves: rows.map(move => move.uci),
                  triggers: continuationTriggerOrigins(detail, [
                    ...comparison.trigger_occurrence_ids,
                    'application' in use ? use.preceding_occurrence_id : use.recapture_occurrence_id,
                  ]),
                },
              ]
            : [];
        },
      );
    });
  const readSupportUse = (
    path: ObservationPath,
    source: ReturnType<typeof recaptureSourcesAt>[number],
    count: number,
  ): boolean | undefined => {
    const detail = continuationByLine.get(path.id);
    const comparison = detail?.support_comparisons.find(
      comparison =>
        comparison.kind === 'replacement_after_capture' &&
        samePiece(comparison.before_actor, source.comparison.before_actor) &&
        path.positionIds!.indexOf(comparison.after_position_id) === count + 1,
    );
    if (!detail || !comparison) return;
    if (!('application' in source.use)) {
      const use = source.use;
      const losses = detail.events.filter(event => event.ply_offset > count && event.original_victim);
      const targetLoss = losses.find(event => samePiece(event.original_victim!, use.original_target));
      return (
        losses.some(
          event =>
            samePiece(event.original_victim!, comparison.after_actor) &&
            (!targetLoss || event.ply_offset < targetLoss.ply_offset),
        ) ||
        (!!targetLoss &&
          (!targetLoss.original_actor ||
            !samePiece(targetLoss.original_actor, use.original_capturer) ||
            detail.capture_links.some(link => link.loss_event_id === targetLoss.id)))
      );
    }
    const use = source.use;
    if (
      detail.events.some(
        event =>
          event.ply_offset > count &&
          event.original_victim &&
          (samePiece(event.original_victim, source.use.original_target) ||
            samePiece(event.original_victim, comparison.after_actor)),
      )
    )
      return true;
    const base = path.positionIds!.indexOf(comparison.after_position_id);
    return Array.from({ length: path.moves.length - base + 1 }, (_, offset) => base + offset).some(at => {
      const point = kingCaptureAt(
        observations,
        path,
        comparison.after_actor,
        source.use.original_target,
        use.application.original_attacker,
        at,
        continuationPiecesAt(path, at),
      );
      return point && kingCaptureKey(point.application) === kingCaptureKey(use.application);
    });
  };
  // Several premises can reference the same admitted use. Reuse its bounded
  // path observation rather than replaying the same support relation per premise.
  const supportObservations = new Map<string, Map<object, boolean | undefined>>();
  const observesSupportUse = (
    path: ObservationPath,
    source: ReturnType<typeof recaptureSourcesAt>[number],
    count: number,
  ): boolean | undefined => {
    const key = `${path.id}:${count}`;
    const values = supportObservations.get(key) ?? new Map<object, boolean | undefined>();
    if (!values.has(source.use)) values.set(source.use, readSupportUse(path, source, count));
    supportObservations.set(key, values);
    return values.get(source.use);
  };
  const continuationTriggersValid = (inquiry: CommentaryContinuationInquiry): boolean => {
    const recaptureSources = recaptureSourcesAt(inquiry.source_line_ids, inquiry.prefix_moves_uci.length);
    const route = inquiry.route_challenge;
    if (route) {
      const searches = (responseSearches ?? [])
        .filter(search => responseSearchMatchesContinuation(search, inquiry))
        .sort((left, right) => Number(left.work_id.slice(5)) - Number(right.work_id.slice(5)));
      const reference = [...searches]
        .reverse()
        .find(
          search =>
            search.root_restriction.kind === 'unrestricted' ||
            inquiry.legal_moves.every(
              move =>
                search.root_restriction.kind === 'restricted' &&
                search.root_restriction.moves_uci.includes(move),
            ),
        );
      if (
        !reference ||
        inquiry.state !== 'surveyed' ||
        reference.work_id !== route.reference_work_id ||
        reference.candidates[0].move_uci !== route.reference_move ||
        reference.candidates[0].line_id !== route.reference_line_id
      )
        return false;
      // Derive complete legal experiments from the already certified physical route.
      const expected = new Map<string, { moves: Uci[]; occurrences: string[]; lines: string[] }>();
      for (const lineId of inquiry.source_line_ids) {
        const detail = continuationByLine.get(lineId),
          path = continuationPaths.get(lineId);
        if (!detail || !path) continue;
        const count = inquiry.prefix_moves_uci.length;
        const pieces = (at: number) => continuationPiecesAt(path, at);
        const current = (actor: CommentaryInquiryPiece, at: number) =>
          pieces(at).find(piece => samePiece(piece.original, actor))?.current;
        const recoveries = new Set([
          ...detail.capture_links.map(link => link.capture_event_id),
          ...detail.other_target_gains.map(link => link.gain_event_id),
        ]);
        const rows = observations.positions!.find(position => position.id === inquiry.position_id)!
          .legal_moves.moves;
        for (const event of detail.events.filter(
          event =>
            event.ply_offset >= count &&
            recoveries.has(event.id) &&
            inquiry.trigger_occurrence_ids.includes(event.occurrence_id),
        )) {
          const performer = event.original_actor && current(event.original_actor, count);
          const victim = event.original_victim && current(event.original_victim, count);
          const stationary =
            victim &&
            Array.from({ length: event.ply_offset - count + 1 }, (_, index) =>
              current(event.original_victim!, count + index),
            ).every(
              piece =>
                piece &&
                piece.side === victim.side &&
                piece.role === victim.role &&
                piece.square === victim.square,
            );
          const moves = rows
            .filter(
              row =>
                (performer && row.captured_square === performer.square) ||
                (stationary && row.uci.slice(0, 2) === victim.square),
            )
            .map(row => row.uci)
            .sort();
          if (!moves.length) continue;
          const key = `recovery_route:${moves.join(' ')}`,
            group = expected.get(key) ?? { moves, occurrences: [], lines: [] };
          group.occurrences.push(event.occurrence_id);
          group.lines.push(lineId);
          expected.set(key, group);
        }
      }
      for (const source of recaptureSources.filter(source =>
        source.triggers.every(id => inquiry.trigger_occurrence_ids.includes(id)),
      )) {
        for (const move of source.moves) {
          const key = `replacement_recapture:${move}`;
          const group = expected.get(key) ?? { moves: [move], occurrences: [], lines: [] };
          group.occurrences.push(source.recovery.occurrence_id);
          group.lines.push(source.path.id);
          expected.set(key, group);
        }
      }
      for (const lineId of inquiry.source_line_ids) {
        const detail = continuationByLine.get(lineId);
        for (const exposure of detail?.capture_exposures ?? []) {
          if (
            exposure.prefix_moves_uci.length !== inquiry.prefix_moves_uci.length ||
            !exposure.prefix_moves_uci.every((move, index) => move === inquiry.prefix_moves_uci[index]) ||
            !inquiry.trigger_occurrence_ids.includes(exposure.occurrence_id)
          )
            continue;
          const moves = [...exposure.moves_uci].sort(),
            key = `exposed_capture:${moves.join(' ')}`;
          const group = expected.get(key) ?? { moves, occurrences: [], lines: [] };
          group.occurrences.push(exposure.occurrence_id);
          group.lines.push(lineId);
          expected.set(key, group);
        }
      }
      const classKey = (group: CommentaryRouteChallenge['move_classes'][number]) =>
        `${group.kind ?? 'recovery_route'}:${[...group.moves_uci].sort().join(' ')}`;
      if (
        route.move_classes.length !== expected.size ||
        new Set(route.move_classes.map(classKey)).size !== expected.size
      )
        return false;
      for (const group of route.move_classes) {
        const actual = expected.get(classKey(group));
        if (
          !actual ||
          !sameIds(group.premise_occurrence_ids, actual.occurrences) ||
          !sameIds(group.source_line_ids, actual.lines)
        )
          return false;
        const eligible = searches.filter(
          search =>
            (search.root_restriction.kind === 'unrestricted' ||
              group.moves_uci.every(
                move =>
                  search.root_restriction.kind === 'restricted' &&
                  search.root_restriction.moves_uci.includes(move),
              )) &&
            search.candidates.some(candidate => group.moves_uci.includes(candidate.move_uci)),
        );
        if (
          !sameIds(
            group.sampled_work_ids,
            eligible.map(search => search.work_id),
          )
        )
          return false;
        const paired = group.moves_uci.includes(route.reference_move)
          ? undefined
          : [...eligible]
              .reverse()
              .find(search =>
                search.candidates.some(candidate => candidate.move_uci === route.reference_move),
              );
        const challenger = paired?.candidates.find(candidate => group.moves_uci.includes(candidate.move_uci));
        const leader = paired?.candidates.find(candidate => candidate.move_uci === route.reference_move);
        if (paired && challenger && leader) {
          if (
            !group.comparison ||
            group.comparison.work_id !== paired.work_id ||
            group.comparison.reference_line_id !== leader.line_id ||
            group.comparison.challenge_line_id !== challenger.line_id
          )
            return false;
        } else if (group.comparison !== null) return false;
      }
      const missing = route.move_classes.some(group => !group.sampled_work_ids.length);
      const nonself = route.move_classes.filter(group => !group.moves_uci.includes(route.reference_move));
      const computed = missing
        ? 'not_returned'
        : nonself.length > 0 && nonself.every(group => group.comparison)
          ? 'compared'
          : 'sampled';
      if (['awaiting_engine', 'stopped'].includes(route.state) ? !missing : route.state !== computed)
        return false;
      if (
        !['awaiting_engine', 'stopped'].includes(route.state) &&
        route.stop_reason !==
          (missing || nonself.some(group => !group.comparison) ? 'investigation_limit' : null)
      )
        return false;
    }
    const proposals: {
      kind: string;
      triggers: string[];
      sourceLineId: string;
      comparisonSource?: CommentaryContinuationComparisonSource;
      support?: ReturnType<typeof recaptureSourcesAt>[number];
      witnessLineIds?: string[];
      unavailable?: boolean;
    }[] = [];
    if (inquiry.kinds.includes('check_response_not_observed')) {
      const position = observations.positions?.find(position => position.id === inquiry.position_id);
      const setup = position && parseFen(position.fen).unwrap();
      const king = setup?.board.kingOf(setup.turn);
      const count = inquiry.prefix_moves_uci.length;
      const sources = inquiry.source_line_ids.map(id => continuationPaths.get(id)!);
      const endpoints = sources.filter(path => path.moves.length === count);
      if (
        !position?.legal_moves.complete ||
        king === undefined ||
        !endpoints.length ||
        !position.controls.pieces.some(
          control => control.piece.side !== setup!.turn && control.squares.includes(makeSquare(king)),
        )
      )
        return false;
      proposals.push(
        ...endpoints.map(path => ({
          kind: 'check_response_not_observed',
          triggers: [inquiry.prefix_occurrence_ids[count - 1]],
          sourceLineId: path.id,
          witnessLineIds: sources.filter(source => source.moves.length > count).map(source => source.id),
        })),
      );
    }
    for (const id of inquiry.explanation_candidate_ids) {
      const detail = lineContinuationDetail(byId.get(id)!);
      if (!detail) continue;
      const events = new Map(detail.events.map(event => [event.id, event]));
      const withOrigins = (occurrences: string[]): string[] =>
        continuationTriggerOrigins(detail, occurrences);
      const rootCapture = detail.events.find(event => event.ply_offset === 0 && event.original_victim);
      if (
        inquiry.prefix_moves_uci.length === 1 &&
        rootCapture &&
        !detail.capture_links.some(link => link.loss_event_id === rootCapture.id && link.elapsed_plies === 1)
      )
        proposals.push({
          kind: 'root_capture_response',
          triggers: withOrigins([rootCapture.occurrence_id]),
          sourceLineId: detail.line_id,
        });
      for (const link of detail.capture_links.filter(link => link.elapsed_plies > 1)) {
        const premises = new Set([link.loss_event_id]),
          queue = [link.loss_event_id];
        while (queue.length) {
          const event = queue.shift()!;
          for (const earlier of detail.capture_links.filter(link => link.capture_event_id === event)) {
            if (!premises.has(earlier.loss_event_id)) {
              premises.add(earlier.loss_event_id);
              queue.push(earlier.loss_event_id);
            }
          }
        }
        const lossEvents = [...premises].map(id => events.get(id)!),
          capture = events.get(link.capture_event_id)!;
        if (
          inquiry.prefix_moves_uci.length === capture.ply_offset - 1 ||
          lossEvents.some(event => inquiry.prefix_moves_uci.length === event.ply_offset + 1)
        )
          proposals.push({
            kind: 'delayed_capturer_removal',
            triggers: withOrigins([...lossEvents.map(event => event.occurrence_id), capture.occurrence_id]),
            sourceLineId: detail.line_id,
          });
      }
      for (const link of detail.other_target_gains) {
        const loss = events.get(link.loss_event_id)!,
          gain = events.get(link.gain_event_id)!;
        if (
          inquiry.prefix_moves_uci.length === loss.ply_offset + 1 ||
          inquiry.prefix_moves_uci.length === gain.ply_offset - 1
        )
          proposals.push({
            kind: 'other_target_gain',
            triggers: withOrigins([loss.occurrence_id, gain.occurrence_id]),
            sourceLineId: detail.line_id,
          });
      }
      const path = continuationPaths.get(detail.line_id)!;
      for (const exposure of detail.capture_exposures ?? []) {
        const count = exposure.prefix_moves_uci.length;
        if (inquiry.prefix_moves_uci.length === count)
          proposals.push({
            kind: 'capture_exposure_response',
            triggers: [exposure.occurrence_id],
            sourceLineId: detail.line_id,
          });
        const losses = detail.events.filter(
          event =>
            event.ply_offset === count &&
            event.original_actor &&
            event.original_victim &&
            samePiece(event.original_actor, exposure.original_attacker) &&
            samePiece(event.original_victim, exposure.original_target) &&
            continuationOpenLossIds(detail, path).has(event.id),
        );
        if (count > 1 && inquiry.prefix_moves_uci.length === count - 1 && losses.length)
          proposals.push({
            kind: 'exposure_prevention_choice',
            triggers: [exposure.occurrence_id, ...losses.map(event => event.occurrence_id)],
            sourceLineId: detail.line_id,
          });
      }
      const position = observations.positions?.find(position => position.id === inquiry.position_id);
      for (const source of recaptureSources.filter(source => source.detail === detail))
        proposals.push({
          kind: 'replacement_function_choice',
          triggers: source.triggers,
          sourceLineId: detail.line_id,
          support: source,
        });
      const returned = responseSearches?.some(
        search =>
          search.candidates.length > 1 &&
          search.candidates.some(
            candidate =>
              candidate.line_id === path.id && candidate.evaluation.kind !== 'exact_automatic_terminal',
          ),
      );
      if (
        returned &&
        path.moves.length === inquiry.prefix_moves_uci.length &&
        path.occurrenceIds.every((id, index) => id === inquiry.prefix_occurrence_ids[index]) &&
        path.lineSummary?.ending.kind === 'line_end' &&
        position?.legal_moves.complete
      ) {
        const side = parseFen(position.fen).unwrap().turn;
        const openLosses = continuationOpenLossIds(detail, path);
        for (const loss of detail.events.filter(event => openLosses.has(event.id))) {
          const triggers = [
            ...continuationLossOrigins(detail, loss.id),
            path.occurrenceIds[path.occurrenceIds.length - 1]!,
          ];
          if (
            loss.original_victim?.side === side &&
            position.legal_moves.moves.some(move => move.captured_square || move.gives_check === true)
          ) {
            proposals.push({
              kind: 'material_followup',
              triggers,
              sourceLineId: detail.line_id,
              witnessLineIds: followupWitnesses(path, loss),
            });
          }
          const actor = loss.actor_end.piece;
          if (
            loss.actor_end.state === 'present' &&
            actor?.side === side &&
            position.controls.pieces.some(
              control => control.piece.side !== side && control.squares.includes(actor.square),
            )
          )
            proposals.push({ kind: 'capturer_reply', triggers, sourceLineId: detail.line_id });
        }
      }
    }
    // A retained origin can outlive its material profile. Keep that absence explicit;
    // it cannot supply a witness or bypass an available profile's physical binding.
    for (const premise of inquiry.premises.filter(premise => premise.kind === 'material_followup')) {
      for (const lineId of premise.source_line_ids) {
        const path = continuationPaths.get(lineId)!;
        if (
          !continuationByLine.has(lineId) &&
          !path.lineSummary?.material &&
          path.occurrenceIds.length === inquiry.prefix_occurrence_ids.length &&
          premise.trigger_occurrence_ids.includes(path.occurrenceIds[path.occurrenceIds.length - 1]!) &&
          premise.trigger_occurrence_ids.every(id => path.occurrenceIds.includes(id)) &&
          responseSearches?.some(
            search =>
              search.candidates.length > 1 &&
              search.candidates.some(candidate => candidate.line_id === lineId),
          )
        )
          proposals.push({
            kind: premise.kind,
            triggers: premise.trigger_occurrence_ids,
            sourceLineId: lineId,
            unavailable: true,
          });
      }
    }
    for (const source of inquiry.comparison_sources ?? []) {
      const search = responseSearches?.find(search => search.work_id === source.work_id);
      const donor = continuationByLine.get(source.donor_line_id);
      const peerProfile = continuationByLine.get(source.peer_line_id);
      const donorPath = continuationPaths.get(source.donor_line_id);
      const peer = continuationPaths.get(source.peer_line_id);
      if (
        !search ||
        !donorPath ||
        !peer ||
        source.donor_line_id === source.peer_line_id ||
        (source.kind !== 'support_application' &&
          ![source.donor_line_id, source.peer_line_id].includes(search.candidates[0].line_id)) ||
        ![source.donor_line_id, source.peer_line_id].every(line =>
          search.candidates.some(candidate => candidate.line_id === line),
        ) ||
        search.candidates.find(candidate => candidate.line_id === source.peer_line_id)!.evaluation.kind ===
          'exact_automatic_terminal'
      )
        return false;
      const prefix = responseSearchPrefix(search),
        n = prefix.length;
      if (
        peer.moves.length <= n ||
        inquiry.prefix_moves_uci.length !== peer.moves.length ||
        !peer.moves.every((move, index) => move.uci === inquiry.prefix_moves_uci[index]) ||
        !prefix.every(
          (move, index) =>
            peer.moves[index]?.uci === move && peer.occurrenceIds[index] === donorPath.occurrenceIds[index],
        ) ||
        peer.lineSummary?.ending.kind !== 'line_end' ||
        !inquiry.source_line_ids.includes(peer.id)
      )
        return false;
      let common = 0;
      while (
        common < donorPath.occurrenceIds.length &&
        donorPath.occurrenceIds[common] === peer.occurrenceIds[common]
      )
        common++;
      if (common < 1 || common >= donorPath.moves.length || common >= peer.moves.length) return false;
      const observation = source.observation_line_id
        ? continuationPaths.get(source.observation_line_id)
        : undefined;
      if (
        source.kind === 'support_application' &&
        (!observation ||
          common !== n ||
          !donorPath.occurrenceIds.every((id, index) => observation.occurrenceIds[index] === id))
      )
        return false;
      const missingProfiles = [donorPath, peer, ...(observation ? [observation] : [])].filter(
        path => !continuationByLine.has(path.id),
      );
      if (missingProfiles.length) {
        if (missingProfiles.some(path => path.lineSummary?.material)) return false;
        const kind =
          source.kind === 'support_application'
            ? 'support_application_not_observed'
            : 'continuation_not_observed';
        proposals.push(
          ...inquiry.premises
            .filter(
              premise =>
                premise.kind === kind &&
                premise.source_line_ids.includes(peer.id) &&
                premise.comparison_source_indices.some(
                  index =>
                    continuationComparisonSourceKey(inquiry.comparison_sources![index]) ===
                    continuationComparisonSourceKey(source),
                ),
            )
            .map(premise => ({
              kind,
              triggers: premise.trigger_occurrence_ids,
              sourceLineId: peer.id,
              comparisonSource: source,
              unavailable: true,
            })),
        );
        continue;
      }
      if (source.kind === 'support_application') {
        const premises = recaptureSourcesAt([observation!.id], n).filter(premise => {
          const family = search.candidates.filter(candidate => premise.moves.includes(candidate.move_uci));
          return (
            [donorPath.id, peer.id].every(id => family.some(candidate => candidate.line_id === id)) &&
            [donorPath.id, peer.id].includes(family[0]?.line_id) &&
            observesSupportUse(peer, premise, n) !== undefined
          );
        });
        if (!premises.length) return false;
        proposals.push(
          ...premises.map(premise => ({
            kind: 'support_application_not_observed',
            triggers: [...premise.triggers, peer.occurrenceIds[peer.occurrenceIds.length - 1]!],
            sourceLineId: peer.id,
            comparisonSource: source,
            support: premise,
            witnessLineIds: [...continuationPaths.values()]
              .filter(
                path =>
                  peer.occurrenceIds.every((id, index) => path.occurrenceIds[index] === id) &&
                  observesSupportUse(path, premise, n) === true,
              )
              .map(path => path.id),
          })),
        );
        continue;
      }
      const linked = donor!.events.flatMap(loss => {
        const peerLoss = peerProfile!.events.find(event => event.occurrence_id === loss.occurrence_id);
        if (
          loss.ply_offset >= common ||
          !loss.original_actor ||
          !loss.original_victim ||
          !peerLoss?.original_actor ||
          !peerLoss.original_victim ||
          !samePiece(peerLoss.original_actor, loss.original_actor) ||
          !samePiece(peerLoss.original_victim, loss.original_victim) ||
          !['present', 'captured'].includes(peerLoss.actor_end.state)
        )
          return [];
        return continuationRecoveries(donor!, loss.id)
          .filter(capture => capture.ply_offset > loss.ply_offset + 1 && capture.ply_offset >= common)
          .map(capture => ({
            loss,
            capture,
            triggers: [
              loss.occurrence_id,
              capture.occurrence_id,
              peer.occurrenceIds[peer.occurrenceIds.length - 1]!,
            ],
          }));
      });
      if (!linked.length) return false;
      proposals.push(
        ...linked.map(({ triggers, loss, capture }) => ({
          kind: 'continuation_not_observed',
          triggers,
          sourceLineId: peer.id,
          comparisonSource: source,
          witnessLineIds: followupWitnesses(peer, loss, capture),
        })),
      );
    }
    const premiseProposals = new Map<CommentaryContinuationPremise, typeof proposals>();
    for (const premise of inquiry.premises) {
      const sources = premise.comparison_source_indices.map(index => inquiry.comparison_sources![index]);
      const matching = proposals.filter(
        proposal =>
          proposal.kind === premise.kind &&
          sameIds(proposal.triggers, premise.trigger_occurrence_ids) &&
          premise.source_line_ids.includes(proposal.sourceLineId) &&
          (proposal.comparisonSource
            ? sources.length === 1 &&
              continuationComparisonSourceKey(sources[0]) ===
                continuationComparisonSourceKey(proposal.comparisonSource)
            : sources.length === 0) &&
          (proposal.support ? proposal.support.use.requirement_id === premise.requirement_id : true),
      );
      if (
        !matching.length ||
        !sameIds(
          premise.source_line_ids,
          matching.map(proposal => proposal.sourceLineId),
        ) ||
        ([
          'replacement_function_choice',
          'support_application_not_observed',
          'continuation_not_observed',
        ].includes(premise.kind)
          ? premise.requirement_id === null
          : premise.requirement_id !== null)
      )
        return false;
      premiseProposals.set(premise, matching);
      const semantic = [
        'check_response_not_observed',
        'material_followup',
        'continuation_not_observed',
        'support_application_not_observed',
      ].includes(premise.kind);
      const witnesses = matching.flatMap(proposal => proposal.witnessLineIds ?? []);
      const observed = matching.every(proposal => (proposal.witnessLineIds?.length ?? 0) > 0);
      if (
        !sameIds(premise.witness_line_ids, witnesses) ||
        premise.observation_state !==
          (matching.some(proposal => proposal.unavailable)
            ? 'unavailable'
            : semantic
              ? observed
                ? 'observed'
                : 'unobserved'
              : 'not_applicable')
      )
        return false;
    }
    if (
      inquiry.state === 'observed' &&
      !inquiry.premises.every(premise => premise.observation_state === 'observed')
    )
      return false;
    {
      const returned = (responseSearches ?? [])
        .filter(search => responseSearchMatchesContinuation(search, inquiry))
        .flatMap(search => search.candidates.map(candidate => continuationPaths.get(candidate.line_id)!));
      const roots = [
        ...returned,
        ...observations.paths.filter(
          path =>
            ((inquiry.source_line_ids.includes(path.id) &&
              path.occurrenceIds.length === inquiry.prefix_occurrence_ids.length) ||
              (inquiry.legal_moves.length === 1 &&
                inquiry.observed_moves.length > 0 &&
                path.occurrenceIds.length > inquiry.prefix_occurrence_ids.length)) &&
            inquiry.prefix_occurrence_ids.every((id, index) => path.occurrenceIds[index] === id),
        ),
      ];
      const descendants = observations.paths.filter(
        path =>
          path.kind !== 'history' &&
          path.lineSummary &&
          roots.some(root => root.occurrenceIds.every((id, index) => path.occurrenceIds[index] === id)),
      );
      const ends = descendants.filter(
        path =>
          !descendants.some(
            other =>
              other.moves.length > path.moves.length &&
              path.occurrenceIds.every((id, index) => other.occurrenceIds[index] === id),
          ),
      );
      for (const premise of inquiry.premises) {
        const ownProposals = premiseProposals.get(premise)!;
        const unfinished = (premise.kind === 'check_response_not_observed' ? [] : ends)
          .filter(path => {
            const detail = continuationByLine.get(path.id);
            const related = new Set(premise.trigger_occurrence_ids);
            for (const lineId of premise.source_line_ids) {
              for (const exposure of continuationByLine.get(lineId)?.capture_exposures ?? []) {
                if (
                  exposure.prefix_moves_uci.length !== inquiry.prefix_moves_uci.length ||
                  !exposure.prefix_moves_uci.every(
                    (move, index) => move === inquiry.prefix_moves_uci[index],
                  ) ||
                  !premise.trigger_occurrence_ids.includes(exposure.occurrence_id)
                )
                  continue;
                for (const capture of detail?.events ?? []) {
                  if (
                    capture.ply_offset === inquiry.prefix_moves_uci.length &&
                    capture.original_actor &&
                    capture.original_victim &&
                    samePiece(capture.original_actor, exposure.original_attacker) &&
                    samePiece(capture.original_victim, exposure.original_target)
                  )
                    related.add(capture.occurrence_id);
                }
              }
            }
            return (
              detail &&
              [...continuationOpenLossIds(detail, path)].some(id =>
                [...continuationLossOrigins(detail, id)].some(occurrence => related.has(occurrence)),
              )
            );
          })
          .map(path => path.id);
        const unfinishedFunctions = ends
          .filter(path =>
            ownProposals.some(proposal => {
              if (!proposal.support) return false;
              const peer =
                proposal.comparisonSource && continuationPaths.get(proposal.comparisonSource.peer_line_id)!;
              return (
                (!peer || peer.occurrenceIds.every((id, index) => path.occurrenceIds[index] === id)) &&
                observesSupportUse(
                  path,
                  proposal.support,
                  proposal.comparisonSource
                    ? responseSearchPrefix(
                        responseSearches!.find(
                          search => search.work_id === proposal.comparisonSource!.work_id,
                        )!,
                      ).length
                    : inquiry.prefix_moves_uci.length,
                ) === false
              );
            }),
          )
          .map(path => path.id);
        const missing = ownProposals.flatMap(proposal =>
          proposal.comparisonSource && !proposal.witnessLineIds?.length
            ? [proposal.comparisonSource.peer_line_id]
            : [],
        );
        const unchecked =
          premise.kind === 'check_response_not_observed' && !premise.witness_line_ids.length
            ? premise.source_line_ids
            : [];
        if (
          !sameIds(premise.unresolved_source_line_ids, [
            ...missing,
            ...unfinished,
            ...unfinishedFunctions,
            ...unchecked,
          ])
        )
          return false;
      }
      const losses = new Set(
        inquiry.trigger_occurrence_ids.filter(id =>
          [...continuationByLine.values()].some(detail =>
            detail.events.some(event => event.occurrence_id === id && event.original_victim),
          ),
        ),
      );
      const followups = continuationInquiries!
        .filter(
          next =>
            next.prefix_occurrence_ids.length > inquiry.prefix_occurrence_ids.length &&
            inquiry.prefix_occurrence_ids.every((id, index) => next.prefix_occurrence_ids[index] === id) &&
            next.trigger_occurrence_ids.some(id => losses.has(id)),
        )
        .map(next => next.id);
      if (
        inquiry.unresolved_source_line_ids !== undefined &&
        (!sameIds(
          inquiry.unresolved_source_line_ids,
          inquiry.premises.flatMap(premise => premise.unresolved_source_line_ids),
        ) ||
          !sameIds(inquiry.followup_question_ids!, followups))
      )
        return false;
    }
    const witnessed = proposals.filter(
      proposal =>
        inquiry.kinds.includes(proposal.kind) &&
        proposal.triggers.every(id => inquiry.trigger_occurrence_ids.includes(id)),
    );
    return (
      sameIds(
        inquiry.kinds,
        witnessed.map(proposal => proposal.kind),
      ) &&
      sameIds(
        inquiry.trigger_occurrence_ids,
        witnessed.flatMap(proposal => proposal.triggers),
      )
    );
  };
  if (
    candidates.some(
      candidate =>
        candidate.detail.continuation_candidate_ids !== undefined &&
        (!['pv_comparison', 'target_response_comparison', 'line_continuation_comparison'].includes(
          candidate.detail.type,
        ) ||
          !linkedProfiles(
            candidate.detail.continuation_candidate_ids,
            candidate.lineIds,
            candidate.rootOccurrenceId,
          )),
    ) ||
    candidates.some(
      candidate =>
        candidate.detail.continuation_comparison_ids !== undefined &&
        (!['pv_comparison', 'target_response_comparison'].includes(candidate.detail.type) ||
          !linkedComparisons(
            candidate.detail.continuation_comparison_ids,
            candidate.lineIds,
            candidate.rootOccurrenceId,
          )),
    ) ||
    continuationInquiries?.some(
      inquiry =>
        !inquiry.explanation_candidate_ids.every(
          id =>
            linkedProfiles(
              [id],
              [
                ...inquiry.source_line_ids,
                ...(inquiry.comparison_sources ?? []).flatMap(source => source.observation_line_id ?? []),
              ],
              inquiry.root_occurrence_id,
            ) || linkedComparisons([id], inquiry.source_line_ids, inquiry.root_occurrence_id),
        ) || !continuationTriggersValid(inquiry),
    )
  )
    return;
  const scenes: CommentaryScene[] = [];
  for (const raw of value.scenes) {
    if (
      !object(raw) ||
      !exactKeys(raw, ['id', 'root_occurrence_id', 'occurrence_ids', 'line_ids', 'candidate_ids', 'state']) ||
      !id(raw.id) ||
      !id(raw.root_occurrence_id) ||
      !occurrences.has(raw.root_occurrence_id) ||
      !state(raw.state) ||
      !ids(raw.occurrence_ids) ||
      !raw.occurrence_ids.includes(raw.root_occurrence_id) ||
      !raw.occurrence_ids.every(v => occurrences.has(v)) ||
      !ids(raw.line_ids) ||
      !raw.line_ids.every(v => paths.has(v)) ||
      !ids(raw.candidate_ids) ||
      !raw.candidate_ids.length ||
      !raw.candidate_ids.every(v => byId.has(v))
    )
      return;
    const members = raw.candidate_ids.map(v => byId.get(v)!);
    const occurrenceIds = raw.occurrence_ids;
    const lineIds = raw.line_ids;
    const comparisons = members.filter(c => c.kind === 'comparison');
    if (
      !members.every(
        c =>
          c.occurrenceIds.every(v => occurrenceIds.includes(v)) && c.lineIds.every(v => lineIds.includes(v)),
      ) ||
      comparisons.length !== 1 ||
      comparisons[0].rootOccurrenceId !== raw.root_occurrence_id ||
      comparisons[0].state !== raw.state ||
      (comparisons[0].detail.type === 'question_comparison' && comparisons[0].selection !== 'selected')
    )
      return;
    scenes.push({
      id: raw.id,
      rootOccurrenceId: raw.root_occurrence_id,
      occurrenceIds: raw.occurrence_ids,
      lineIds: raw.line_ids,
      candidateIds: raw.candidate_ids,
      state: raw.state,
    });
  }
  if (new Set(scenes.map(s => s.id)).size !== scenes.length) return;
  if (candidates.some(candidate => candidate.detail.type === 'question_comparison' &&
    candidate.selection === 'selected' && !scenes.some(scene => scene.candidateIds.includes(candidate.id)))) return;
  const responseFields = {
    ...(discovery === undefined ? {} : { discovery }),
    ...(proposals === undefined
      ? {}
      : { proposals, local_expansions_used: value.local_expansions_used as number }),
    ...(rootSearches === undefined ? {} : { root_searches: rootSearches }),
    ...(responseInquiries === undefined ? {} : { response_inquiries: responseInquiries }),
    ...(responseSearches === undefined ? {} : { response_searches: responseSearches }),
    ...(continuationInquiries === undefined ? {} : { continuation_inquiries: continuationInquiries }),
  };
  if (
    responseInquiries?.some(
      inquiry =>
        inquiry.state === 'observed' &&
        !candidates.some(candidate => {
          const detail = targetResponseDetail(candidate);
          return (
            candidate.state === 'ready' &&
            detail?.response_inquiry_id === inquiry.id &&
            detail.branches[0]?.reply_move === inquiry.reference_move &&
            detail.branches[1]?.reply_move === inquiry.alternative_move
          );
        }),
    )
  )
    return;
  if (!proposalFactSourcesValid(proposals ?? [], observations, continuationInquiries ?? [])) return;
  if (value.inquiries === undefined) return { version: 1, candidates, scenes, ...responseFields };
  try {
    const inquiries = decodeInquiries(value.inquiries, observations, context, candidates);
    return inquiries ? { version: 1, candidates, scenes, inquiries, ...responseFields } : undefined;
  } catch (_) {
    return;
  }
}

export function commentarySceneFrame(
  observations: MoveReviewObservations,
  scene: CommentaryScene,
): MoveReviewFrameSelection | undefined {
  const comparison = observations.selection?.candidates.find(candidate =>
    scene.candidateIds.includes(candidate.id) && questionComparisonDetail(candidate),
  );
  if (comparison) {
    const ply = questionComparisonDetail(comparison)!.prefix_moves_uci.length + 1;
    const path = observations.paths.find(path => comparison.lineIds.includes(path.id) && path.moves.length >= ply);
    return path ? { proofId: path.id, ply, phase: 'before' } : undefined;
  }
  // Prefer the submitted record when the same historical edge is also in an engine line.
  const paths = observations.paths.filter(p => scene.lineIds.includes(p.id));
  const path =
    paths.find(p => p.sources?.includes('pgn_record') && p.occurrenceIds.includes(scene.rootOccurrenceId)) ??
    paths.find(p => p.occurrenceIds.includes(scene.rootOccurrenceId)) ??
    observations.paths.find(p => p.occurrenceIds.includes(scene.rootOccurrenceId));
  if (!path) return;
  return { proofId: path.id, ply: path.occurrenceIds.indexOf(scene.rootOccurrenceId) + 1, phase: 'after' };
}
