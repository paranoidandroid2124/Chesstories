import type { FEN, Uci, Color, Key } from './types';
import type { MoveReviewFrameSelection, MoveReviewObservations, ObservedLineSummary, PositionResources } from './moveReview';
import { parseFen } from 'chessops/fen';
import { parseSquare, parseUci, makeSquare, kingCastlesTo, rookCastlesTo } from 'chessops/util';
import { castlingSide } from 'chessops/chess';
import { setupPosition } from 'chessops/variant';
import { attacks } from 'chessops/attacks';
import { isMoveReviewEngineProfile } from '../engine/types';

export interface CommentaryEffect {
  scope: 'pair_difference' | 'line_history' | 'check_sequence' |
    'support_handoff' | 'support_use' | 'support_retention' | 'piece_route' | 'blocker_handoff' | 'clearance_use' |
    'pawn_option' | 'capture_choice' | 'advance_choice' | 'relative_pin' | 'capture_release';
  line_ids: string[];
  prefix_moves_uci: Uci[];
  through_plies: number[];
  attack_ply?: number;
  actor: CommentaryInquiryPiece;
  target: CommentaryInquiryPiece | null;
  square: Key;
  occurrence_ids: string[];
  evidence_ids: string[];
}

export interface CommentaryExchangeSummary {
  line_id: string;
  retained_line_id: string;
  role: 'knight' | 'bishop' | 'rook' | 'queen';
  recapturer_role: CommentaryInquiryPiece['role'];
  exchange_occurrence_ids: string[];
  retention_occurrence_ids: string[];
}

export interface CommentaryQuestionMove {
  line_id: string;
  occurrence_id: string;
}

export interface CommentaryQuestionFocus extends CommentaryQuestionMove {
  phase: 'before' | 'after';
  position_id: string;
  squares: Key[];
  source_ids: string[];
}

export interface CommentaryQuestionUnit {
  role: 'condition' | 'reply' | 'cost';
  en: (string | CommentaryQuestionMove)[];
  ko: (string | CommentaryQuestionMove)[];
  covered_moves: CommentaryQuestionMove[];
  proof_moves?: CommentaryQuestionMove[];
  /** Actual actions owned by the typed answer; position-only proof may still have a SAN link. */
  action_moves?: CommentaryQuestionMove[];
  required_moves?: CommentaryQuestionMove[];
  recorded_end_moves?: CommentaryQuestionMove[];
  focus?: CommentaryQuestionFocus[];
  english_only?: true;
}

export interface CommentaryQuestionSummary {
  schema_version?: 2;
  question: 'provider_transfer' | 'support_and_interposition' | 'paired_support_use' |
    'escape_or_recapture' | 'capture_now_or_after_attack' | 'attack_or_immediate_exchange' | 'counter_capture_comparison' |
    'occupied_pawn_destination' | 'controlled_destination' | 'opening_counterplay' |
    'observed_move_order' | 'observed_arrangement_convergence' | 'check_status_comparison' | 'deferred_pawn_attack' | 'contested_pair_response' | 'observed_route_use' | 'selected_target_response' | 'attacker_removal_or_recapture' |
    'observed_exchange' | 'observed_terminal_comparison' | 'selected_comparison' | 'current_defense' | 'observed_capture_target' | 'investigated_mechanism' | 'functional_response' | 'first_target_response' | 'exchange_function' | 'move_purpose';
  move_purpose?: CommentaryPurposeQuestion & { work_id: string; execution_key_sha256: string };
  units: CommentaryQuestionUnit[];
  detail_units?: CommentaryQuestionUnit[];
  detail_moves: CommentaryQuestionMove[];
  witness_line_ids?: string[];
  retained_choice_line_id?: string;
  first_target_response?: {
    response_inquiry_id: string;
    full_pool_work_id: string;
    full_pool_execution_key_sha256: string;
    capture_decline_work_id: string;
    capture_decline_execution_key_sha256: string;
  };
}

export interface CommentarySummaryUnavailable {
  question: string;
  reason: 'answer_not_delivered';
}

export interface CommentaryCandidate {
  id: string;
  kind: 'local_fact' | 'conditional_line' | 'comparison';
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
  detail: Record<string, unknown> & { type: string; effects?: CommentaryEffect[];
    summary_exchange?: CommentaryExchangeSummary; summary_question?: CommentaryQuestionSummary;
    summary_unavailable?: CommentarySummaryUnavailable };
}

export interface CommentaryScene {
  id: string;
  rootOccurrenceId: string;
  occurrenceIds: string[];
  lineIds: string[];
  candidateIds: string[];
}

export interface CommentarySelection {
  version: 2;
  candidates: CommentaryCandidate[];
  scenes: CommentaryScene[];
  inquiries?: CommentaryInquiry[];
  response_inquiries?: CommentaryResponseInquiry[];
  response_searches?: CommentaryResponseSearch[];
  purpose_question?: CommentaryPurposeQuestion;
  preparation_question?: CommentaryPreparationQuestion;
  root_searches?: CommentaryRootSearch[];
  continuation_inquiries?: CommentaryContinuationInquiry[];
  proposals?: CommentaryProposal[];
  /** Validated question decisions, without the samples/proofs used to admit the response. */
  questionOutcomes?: CommentaryQuestionOutcome[];
  local_expansions_used?: number;
  discovery?: CommentaryDiscovery;
}

/** A preparatory action owns a tested use after one reply. Its legal examples
 * propose the idea; the child scores never replace the actual root assessment. */
export interface CommentaryPreparationQuestion {
  id: string;
  root_move: Uci;
  root_occurrence_id: string;
  source_line_id: string;
  reference_line_id: string;
  use_move: Uci;
  example_line_ids: string[];
}

function decodePreparationQuestion(raw: unknown, observations: MoveReviewObservations,
  playedMove?: Uci): CommentaryPreparationQuestion | undefined {
  if (!object(raw) || !exactKeys(raw, ['id', 'root_move', 'root_occurrence_id', 'source_line_id',
    'reference_line_id', 'use_move', 'example_line_ids']) || raw.root_move !== playedMove ||
    !['id', 'root_occurrence_id', 'source_line_id', 'reference_line_id'].every(key => id(raw[key])) ||
    typeof raw.use_move !== 'string' || !/^[a-h][1-8][a-h][1-8]$/.test(raw.use_move) ||
    !ids(raw.example_line_ids) || raw.example_line_ids.length < 1 || raw.example_line_ids.length > 2) return;
  const root = observations.paths.find(p => p.id === raw.source_line_id);
  const reference = observations.paths.find(p => p.id === raw.reference_line_id);
  if (!root || !reference || root.moves.length < 3 || root.moves[0].uci !== playedMove ||
    root.occurrenceIds[0] !== raw.root_occurrence_id || reference.moves[0]?.uci === playedMove ||
    reference.startFen !== root.startFen || reference.positionIds?.[0] !== root.positionIds?.[0] ||
    ![root, reference].every(p => p.evaluations?.some(e =>
      ['root_search', 'focus_comparison', 'investigation_comparison'].includes(e.source))) ||
    !observations.positions?.some(p => p.id === root.positionIds?.[2] && p.legal_moves.complete &&
      p.legal_moves.moves.some(m => m.uci === raw.use_move))) return;
  const examples = raw.example_line_ids.map(line => observations.paths.find(p => p.id === line));
  if (!examples.every(p => p?.lineSummary?.material && [root, reference].some(owner =>
    p.startFen === owner.startFen && p.positionIds?.[0] === owner.positionIds?.[0] &&
    p.occurrenceIds[0] === owner.occurrenceIds[0] && p.moves[0]?.uci === owner.moves[0]?.uci)) ||
    !examples[0] || !root.moves.slice(0, 2).every((move, n) => examples[0]!.moves[n]?.uci === move.uci) ||
    examples[0].moves[2]?.uci !== raw.use_move) return;
  return raw as unknown as CommentaryPreparationQuestion;
}

export function preparationResponseMovesMatch(question: CommentaryPreparationQuestion,
  observations: MoveReviewObservations, prefix: readonly Uci[], moves: readonly Uci[]): boolean {
  const root = observations.paths.find(p => p.id === question.source_line_id);
  const reference = observations.paths.find(p => p.id === question.reference_line_id);
  if (!root || !reference || prefix[0] !== question.root_move || prefix.length < 1 || prefix.length > 2 ||
    !prefix.every((move, n) => root.moves[n]?.uci === move)) return false;
  const at = observations.positions?.find(p => p.id === root.positionIds?.[prefix.length]);
  if (!at?.legal_moves.complete) return false;
  const legal = at.legal_moves.moves.map(m => m.uci);
  const expected = prefix.length === 2
    ? [question.use_move, root.moves[2].uci, ...[reference.moves[0].uci].filter(move => legal.includes(move))]
    : legal;
  const unique = [...new Set(expected)].sort();
  return unique.length >= 2 && (prefix.length === 1 || unique.length <= 3) && moves.length === unique.length &&
    moves.every(move => legal.includes(move)) && JSON.stringify([...moves].sort()) === JSON.stringify(unique);
}

interface CommentaryActivityPlan {
  entry: number;
  route: number[];
  cause: 'cleared_square' | 'cleared_route' | 'new_support' | 'relocation' |
    'reply_exchange' | 'reply_block' | 'reply_activity';
  user: Pick<CommentaryInquiryPiece, 'side' | 'role' | 'square'>;
  preparations?: number[];
  control?: {
    controller: Pick<CommentaryInquiryPiece, 'side' | 'role' | 'square'>;
    change: number;
    removed: boolean;
  };
}

export interface CommentaryPurposeQuestion {
  id: string;
  root_move: Uci;
  root_occurrence_id: string;
  child_position_id: string;
  source_line_id: string;
  use_move: Uci;
  kind: 'supported_advance' | 'entry_route' | 'piece_pressure' | 'support_release' | 'exchange_activity' | 'enabled_activity';
  square: string;
  challenge_moves: Uci[];
  activity_plan?: CommentaryActivityPlan;
  reply_activities?: (CommentaryActivityPlan & { line_id: string })[];
  prepared_choice?: { line_id: string; targets: Pick<CommentaryInquiryPiece, 'side' | 'role' | 'square'>[] };
  activity?: {
    targets: Pick<CommentaryInquiryPiece, 'side' | 'role' | 'square'>[];
    offers: Uci[];
    open_file: string | null;
  };
  support?: {
    asset: Pick<CommentaryInquiryPiece, 'side' | 'role' | 'square'>;
    guards: string[];
    blockader: string;
    release: { rook: string; move: Uci; entry: string; clearance: string[] };
  };
  plan?: {
    line_id: string;
    start: number;
    use: number;
    join: number;
    route: number[];
    targets: Pick<CommentaryInquiryPiece, 'side' | 'role' | 'square'>[];
    squares?: string[];
    clearance?: string[];
  };
}

function decodePurposeQuestion(raw: unknown, observations: MoveReviewObservations,
  playedMove?: Uci, owned = false): CommentaryPurposeQuestion | undefined {
  if (!object(raw) || !exactKeys(raw, ['id', 'root_move', 'root_occurrence_id', 'child_position_id',
    'source_line_id', 'use_move', 'kind', 'square', 'challenge_moves',
      ...(owned ? ['work_id', 'execution_key_sha256'] : [])], ['plan', 'support', 'activity', 'activity_plan', 'reply_activities', ...(owned ? ['choice', 'prepared_choice'] : [])]) ||
    !['id', 'root_occurrence_id', 'child_position_id', 'source_line_id'].every(field => id(raw[field])) ||
    raw.root_move !== playedMove || !['supported_advance', 'entry_route', 'piece_pressure', 'support_release', 'exchange_activity', 'enabled_activity'].includes(String(raw.kind)) ||
    typeof raw.square !== 'string' || !/^[a-h][1-8]$/.test(raw.square) ||
    typeof raw.use_move !== 'string' || !/^[a-h][1-8][a-h][1-8]$/.test(raw.use_move) ||
    raw.use_move.slice(2, 4) !== raw.square || !Array.isArray(raw.challenge_moves) || !raw.challenge_moves.length ||
    new Set(raw.challenge_moves).size !== raw.challenge_moves.length) return;
  const path = observations.paths.find(path => path.id === raw.source_line_id &&
    path.moves[0]?.uci === playedMove && path.occurrenceIds[0] === raw.root_occurrence_id &&
    path.positionIds?.[1] === raw.child_position_id);
  const before = path && observations.positions?.find(p => p.id === path.positionIds?.[0]);
  const after = path && observations.positions?.find(p => p.id === raw.child_position_id);
  if (!path || !before?.controls.complete || !after?.controls.complete || !after.legal_moves.complete ||
    !raw.challenge_moves.every(move => after.legal_moves.moves.some(row => row.uci === move))) return;
  const actor = after.controls.pieces.find(row => row.piece.square === playedMove?.slice(2, 4));
  if (raw.prepared_choice !== undefined) {
    const choice = raw.prepared_choice;
    const plan = object(raw.plan) ? raw.plan : undefined;
    const usePath = plan ? observations.paths.find(p => p.id === plan.line_id) : path;
    const use = usePath?.moves.findIndex(move => move.uci === raw.use_move) ?? -1;
    const user = usePath && use >= 2 && trackedPieces(usePath, use)
      ?.find(p => p.current?.square === String(raw.use_move).slice(0, 2))?.original;
    if (!owned || !user || !object(choice) || !exactKeys(choice, ['line_id', 'targets']) ||
      !id(choice.line_id) || !Array.isArray(choice.targets) || !choice.targets.length) return;
    const peer = observations.paths.find(p => p.id === choice.line_id && p.positionIds?.[0] === path.positionIds?.[0] &&
      p.moves[0]?.uci !== playedMove && p.moves[0]?.uci === user.square + raw.square);
    const end = peer && observations.positions?.find(p => p.id === peer.positionIds?.[1]);
    const old = before.controls.pieces.find(row => row.piece.side === user.side &&
      row.piece.role === user.role && row.piece.square === user.square);
    const mover = end?.controls.pieces.find(row => row.piece.side === user.side &&
      row.piece.role === user.role && row.piece.square === raw.square);
    if (!old || !end?.controls.complete || !mover) return;
    const targets = before.controls.pieces.filter(row => row.piece.side !== user.side &&
      !old.squares.includes(row.piece.square) && mover.squares.includes(row.piece.square) &&
      end.controls.pieces.some(current => current.piece.square === row.piece.square &&
        current.piece.side === row.piece.side && current.piece.role === row.piece.role)).map(row => row.piece);
    if (choice.targets.length !== targets.length || new Set(choice.targets.map(p =>
      object(p) ? `${p.side}:${p.role}:${p.square}` : '')).size !== targets.length || !choice.targets.every(p => object(p) &&
      exactKeys(p, ['side', 'role', 'square']) && targets.some(t => t.side === p.side &&
        t.role === p.role && t.square === p.square))) return;
  }
  function newlySupported(line: ObservationPath, original: CommentaryInquiryPiece, destination: Key, through: number): boolean {
    const provider = before?.controls.pieces.find(row => row.piece.square === playedMove?.slice(0, 2));
    if (!actor || !provider || original.side !== actor.piece.side || original.square === provider.piece.square ||
      through < 3 || through > 18) return false;
    let direct = !provider.squares.includes(destination), following = !provider.squares.includes(original.square);
    for (let count = 1; count <= through; count++) {
      const board = observations.positions?.find(p => p.id === line.positionIds?.[count]);
      const pieces = trackedPieces(line, count);
      const supporter = pieces?.find(p => p.original.square === provider.piece.square &&
        p.original.side === provider.piece.side && p.original.role === provider.piece.role)?.current;
      const user = pieces?.find(p => samePiece(p.original, original))?.current;
      if (!board?.controls.complete || !supporter || !user || supporter.square !== actor.piece.square ||
        supporter.role !== actor.piece.role || supporter.side !== actor.piece.side) return false;
      const controls = board.controls.pieces.find(row => row.piece.square === supporter.square &&
        row.piece.role === supporter.role && row.piece.side === supporter.side)?.squares;
      direct &&= Boolean(controls?.includes(destination));
      following &&= Boolean(controls?.includes(user.square));
      if (count === through && user.square !== destination) return false;
    }
    return direct || following;
  }
  function validActivity(plan: unknown, path: MoveReviewObservations['paths'][number], useMove: string, reply = false): boolean {
    const opposing = object(plan) && ['reply_exchange', 'reply_block', 'reply_activity'].includes(String(plan.cause));
    if (!actor || !before || !object(plan) ||
      !exactKeys(plan, ['entry', 'route', 'cause', 'user', ...(reply ? ['line_id'] : [])], ['control', 'preparations']) ||
      !Number.isInteger(plan.entry) || Number(plan.entry) < (opposing ? 1 : 2) ||
      !Array.isArray(plan.route) || !plan.route.length || !plan.route.every(n =>
        Number.isInteger(n) && n >= 0 && n < 18 && n % 2 === (opposing ? 1 : 0)) ||
      !object(plan.user) || !exactKeys(plan.user, ['side', 'role', 'square']) ||
      !['cleared_square', 'cleared_route', 'new_support', ...(reply ?
        ['relocation', 'reply_exchange', 'reply_block', 'reply_activity'] : [])].includes(String(plan.cause))) return false;
    const relocating = plan.cause === 'relocation';
    if (plan.preparations !== undefined && (plan.cause !== 'cleared_route' ||
      !Array.isArray(plan.preparations) || plan.preparations.length < 2 || plan.preparations[0] !== 0 ||
      !plan.preparations.every((n, i, values) => Number.isInteger(n) && n >= 0 && n < Number(plan.entry) &&
        (i === 0 || n > values[i - 1])))) return false;
    if (relocating ? plan.route[0] !== 0 || plan.route[plan.route.length - 1] !== plan.entry :
      plan.route[0] !== plan.entry) return false;
    const user = plan.user;
    const initial = before.controls.pieces.find(row => row.piece.square === user.square &&
      row.piece.role === user.role && row.piece.side === user.side &&
      (opposing ? row.piece.side !== actor.piece.side : row.piece.side === actor.piece.side) &&
      (relocating ? row.piece.square === playedMove?.slice(0, 2) : row.piece.square !== playedMove?.slice(0, 2)));
    const rootMove = before.legal_moves?.moves.find(row => row.uci === playedMove);
    if (!initial || !rootMove || !relocating && !opposing && rootMove.captured_square &&
      plan.preparations === undefined) return false;
    const original = { ...initial.piece, position_id: before.id };
    const resource = (count: number) => observations.positions?.find(p => p.id === path.positionIds?.[count]);
    const current = (count: number) => trackedPieces(path, count)?.find(p => samePiece(p.original, original))?.current;
    const entry = plan.entry as number, use = plan.route[plan.route.length - 1] as number;
    const destination = path.moves[entry]?.uci.slice(2, 4) as Key;
    const exactRoute = path.moves.flatMap((move, offset) => offset >= (relocating ? 0 : entry) && offset <= use &&
      current(offset)?.square === move.uci.slice(0, 2) ? [offset] : []);
    if (!destination || path.moves[use]?.uci !== useMove ||
      JSON.stringify(exactRoute) !== JSON.stringify(plan.route)) return false;
    if (opposing && plan.cause !== 'reply_activity') {
      const control = plan.control;
      if (!object(control) || !exactKeys(control, ['controller', 'change', 'removed']) ||
        !object(control.controller) || !exactKeys(control.controller, ['side', 'role', 'square']) ||
        !Number.isInteger(control.change) || Number(control.change) < 1 || Number(control.change) >= entry ||
        control.removed !== (plan.cause === 'reply_exchange')) return false;
      const subject = control.controller;
      const controller = before.controls.pieces.find(row => row.piece.side === subject.side &&
        row.piece.role === subject.role && row.piece.square === subject.square &&
        row.piece.side === actor.piece.side);
      if (!controller) return false;
      const identity = { ...controller.piece, position_id: before.id };
      const defender = (count: number) => trackedPieces(path, count)?.find(p => samePiece(p.original, identity))?.current;
      const controls = (count: number) => resource(count)?.controls.pieces.find(row => {
        const piece = defender(count);
        return piece && row.piece.square === piece.square && row.piece.role === piece.role && row.piece.side === piece.side;
      })?.squares.includes(destination);
      if (!resource(1)?.controls.complete || !controls(1)) return false;
      const change = Number(control.change), captured = resource(change)?.legal_moves.moves
        .find(row => row.uci === path.moves[change]?.uci)?.captured_square;
      if (control.removed) {
        if (!defender(change) || captured !== defender(change)?.square || defender(change + 1)) return false;
        for (let count = 1; count < entry; count++) {
          const board = resource(count), move = board?.legal_moves.moves.find(row => row.uci === path.moves[count]?.uci);
          const moving = board?.controls.pieces.find(row => row.piece.square === path.moves[count]?.uci.slice(0, 2));
          const king = board?.controls.pieces.find(row => row.piece.side === moving?.piece.side && row.piece.role === 'king');
          const checked = king && board?.controls.pieces.some(row => row.piece.side !== king.piece.side && row.squares.includes(king.piece.square));
          if (!board?.controls.complete || !board.legal_moves.complete || !move || !move.captured_square && !checked) return false;
        }
      } else if (change !== 1 || !defender(2)) return false;
      for (let count = change + 1; count <= entry; count++) {
        if (!resource(count)?.controls.complete || controls(count) || control.removed && defender(count)) return false;
      }
    } else if (plan.control !== undefined || opposing && (entry !== 1 || use !== 1)) return false;
    for (let count = 1; count <= use + 1; count++) {
      const board = resource(count), piece = current(count);
      if (!board?.controls.complete || !piece || (relocating || count > entry) && piece.square === original.square) return false;
      if (count <= entry && ['cleared_square', 'cleared_route'].includes(String(plan.cause)) &&
        board.controls.pieces.some(row => row.piece.square === playedMove?.slice(0, 2))) return false;
    }
    if (plan.cause === 'cleared_square' && destination !== playedMove?.slice(0, 2)) return false;
    if (plan.cause === 'cleared_route') {
      const from = current(entry)?.square, cleared = playedMove?.slice(0, 2);
      if (!from || !cleared || !['bishop', 'rook', 'queen'].includes(original.role)) return false;
      const dx = destination.charCodeAt(0) - from.charCodeAt(0), dy = Number(destination[1]) - Number(from[1]);
      const bx = cleared.charCodeAt(0) - from.charCodeAt(0), by = Number(cleared[1]) - Number(from[1]);
      if (!(dx === 0 || dy === 0 || Math.abs(dx) === Math.abs(dy)) || dx * by !== dy * bx ||
        Math.sign(bx) !== Math.sign(dx) || Math.sign(by) !== Math.sign(dy) ||
        Math.max(Math.abs(bx), Math.abs(by)) >= Math.max(Math.abs(dx), Math.abs(dy)) || !(bx || by)) return false;
      if (plan.preparations !== undefined) {
        const corridor = Array.from({ length: Math.max(Math.abs(dx), Math.abs(dy)) }, (_, n) =>
          String.fromCharCode(from.charCodeAt(0) + Math.sign(dx) * (n + 1)) +
            (Number(from[1]) + Math.sign(dy) * (n + 1)));
        const blockers = before.controls.pieces.filter(row => corridor.includes(row.piece.square));
        if (!blockers.some(row => row.piece.square === cleared) ||
          blockers.some(row => row.piece.side !== original.side)) return false;
        const identities = blockers.map(row => ({ ...row.piece, position_id: before.id }));
        const blockingPiece = (identity: typeof original, count: number) =>
          trackedPieces(path, count)?.find(p => samePiece(p.original, identity))?.current;
        const inRoute = (identity: typeof original, count: number) => {
          const piece = blockingPiece(identity, count);
          return Boolean(piece && corridor.includes(piece.square));
        };
        for (let count = 0; count <= entry; count++) {
          const board = resource(count);
          if (current(count)?.square !== from || !board?.controls.complete ||
            board.controls.pieces.some(row => corridor.includes(row.piece.square) &&
              !identities.some(identity => {
                const piece = blockingPiece(identity, count);
                return piece && piece.square === row.piece.square && piece.role === row.piece.role &&
                  piece.side === row.piece.side;
              }))) return false;
        }
        for (const identity of identities) {
          let left = false;
          for (let count = 1; count <= entry; count++) {
            if (!inRoute(identity, count)) left = true;
            else if (left) return false;
          }
          if (!left) return false;
        }
        const actual = Array.from({ length: entry }, (_, n) => n).filter(n => identities.some(identity => {
          const before = blockingPiece(identity, n), after = blockingPiece(identity, n + 1);
          return inRoute(identity, n) && before && (!after || before.square !== after.square ||
            before.role !== after.role || before.side !== after.side);
        }));
        if (JSON.stringify(actual) !== JSON.stringify(plan.preparations)) return false;
      }
    }
    if (plan.cause === 'new_support' && !newlySupported(path, original, destination, entry + 1)) return false;
    const end = resource(use + 1), moved = current(use + 1);
    const control = end?.controls.pieces.find(row => moved && row.piece.square === moved.square &&
      row.piece.role === moved.role && row.piece.side === moved.side);
    const meaningful = control && trackedPieces(path, use + 1)?.some(piece => piece.current &&
      !samePiece(piece.original, original) && control.squares.includes(piece.current.square) &&
      !initial.squares.includes(piece.original.square) && (piece.current.side !== original.side ||
        end?.controls.pieces.some(row => row.piece.side !== original.side && row.squares.includes(piece.current!.square))));
    return Boolean(meaningful || resource(use)?.legal_moves?.moves.find(row => row.uci === useMove)?.captured_square);
  }
  if (raw.reply_activities !== undefined) {
    if (!Array.isArray(raw.reply_activities) || raw.reply_activities.length < 1 || raw.reply_activities.length > 2 ||
      new Set(raw.reply_activities.map(a => object(a) && a.line_id)).size !== raw.reply_activities.length ||
      !raw.reply_activities.every(a => {
        if (!object(a) || !Array.isArray(a.route)) return false;
        const line = observations.paths.find(p => p.id === a.line_id && p.moves[0]?.uci === playedMove &&
          p.occurrenceIds[0] === raw.root_occurrence_id && p.positionIds?.[1] === raw.child_position_id);
        const use = line?.moves[a.route[a.route.length - 1]]?.uci;
        return line && use && validActivity(a, line, use, true);
      })) return;
  }
  if (raw.kind === 'enabled_activity') {
    if (!actor || raw.plan !== undefined || raw.support !== undefined || raw.activity !== undefined ||
      !validActivity(raw.activity_plan, path, raw.use_move)) return;
    return raw as unknown as CommentaryPurposeQuestion;
  }
  if (raw.activity_plan !== undefined) return;
  if (raw.kind === 'exchange_activity') {
    if (!actor || ['pawn', 'king'].includes(actor.piece.role) || raw.plan !== undefined ||
      raw.support !== undefined || !object(raw.activity) ||
      !exactKeys(raw.activity, ['targets', 'offers', 'open_file']) ||
      raw.use_move !== playedMove || !Array.isArray(raw.activity.targets) || !Array.isArray(raw.activity.offers)) return;
    const original = before.controls.pieces.find(row => row.piece.square === playedMove!.slice(0, 2));
    if (!original || before.controls.pieces.some(row => row.piece.square === raw.square)) return;
    const targets = after.controls.pieces.filter(row => row.piece.side !== actor.piece.side &&
      !['pawn', 'king'].includes(row.piece.role) && actor.squares.includes(row.piece.square) &&
      !original.squares.includes(row.piece.square)).map(row => row.piece).sort((a, b) => a.square.localeCompare(b.square));
    const offers = after.legal_moves.moves.filter(row => row.captured_square === actor.piece.square &&
      after.controls.pieces.some(p => p.piece.square === row.uci.slice(0, 2) && p.piece.role === actor.piece.role)).map(row => row.uci).sort();
    const challenges = after.legal_moves.moves.filter(row => !row.captured_square &&
      targets.some(p => p.square === row.uci.slice(0, 2))).map(row => row.uci).sort();
    const file = ['rook', 'queen'].includes(actor.piece.role) &&
      targets.some(p => p.role === actor.piece.role && p.square[0] === actor.piece.square[0]) &&
      !after.controls.pieces.some(row => row.piece.role === 'pawn' && row.piece.square[0] === actor.piece.square[0])
      ? actor.piece.square[0] : null;
    if (!offers.length || !targets.length || targets.length !== raw.activity.targets.length ||
      !raw.activity.targets.every(t => object(t) && exactKeys(t, ['side', 'role', 'square']) &&
        targets.some(p => p.side === t.side && p.role === t.role && p.square === t.square)) ||
      new Set(raw.activity.targets.map(t => (t as { square: string }).square)).size !== targets.length ||
      JSON.stringify(raw.activity.offers) !== JSON.stringify(offers) ||
      JSON.stringify([...raw.challenge_moves].sort()) !== JSON.stringify(challenges) || raw.activity.open_file !== file) return;
    return raw as unknown as CommentaryPurposeQuestion;
  }
  if (raw.activity !== undefined) return;
  if (raw.kind === 'support_release') {
    const support = raw.support;
    if (raw.plan !== undefined || !actor || actor.piece.role === 'pawn' || !object(support) ||
      !exactKeys(support, ['asset', 'guards', 'blockader', 'release']) || !object(support.asset) ||
      !exactKeys(support.asset, ['side', 'role', 'square']) || !Array.isArray(support.guards) ||
      !object(support.release) || !exactKeys(support.release, ['rook', 'move', 'entry', 'clearance'])) return;
    const task = support as unknown as NonNullable<CommentaryPurposeQuestion['support']>;
    const asset = after.controls.pieces.find(row => row.piece.square === task.asset.square)?.piece;
    const original = before.controls.pieces.find(row => row.piece.square === playedMove?.slice(0, 2));
    const white = actor.piece.side === 'white', pawn = task.asset.square;
    const front = pawn[0] + String(Number(pawn[1]) + (white ? 1 : -1));
    const guards = after.controls.pieces.filter(row => row.piece.side === actor.piece.side && row.squares.includes(pawn));
    if (!asset || asset.role !== 'pawn' || asset.side !== actor.piece.side ||
      task.asset.role !== asset.role || task.asset.side !== asset.side ||
      !before.controls.pieces.some(row => row.piece.square === pawn && row.piece.role === 'pawn' && row.piece.side === asset.side) ||
      (white ? pawn[1] < '5' : pawn[1] > '4') ||
      after.controls.pieces.some(row => row.piece.side !== asset.side && row.piece.role === 'pawn' &&
        Math.abs(row.piece.square.charCodeAt(0) - pawn.charCodeAt(0)) <= 1 &&
        (white ? row.piece.square[1] > pawn[1] : row.piece.square[1] < pawn[1])) ||
      task.blockader !== front || !after.controls.pieces.some(row => row.piece.square === front && row.piece.side !== asset.side) ||
      JSON.stringify(task.guards) !== JSON.stringify(guards.map(row => row.piece.square).sort()) ||
      !guards.some(row => ['rook', 'queen'].includes(row.piece.role)) || !original || original.squares.includes(pawn)) return;
    const distance = (a: string, b: string) => Math.max(Math.abs(a.charCodeAt(0) - b.charCodeAt(0)), Math.abs(+a[1] - +b[1]));
    const current = playedMove!.slice(2, 4), pending = raw.square !== current;
    if (pending ? actor.piece.role !== 'king' || raw.use_move !== current + raw.square ||
      distance(original.piece.square, raw.square) <= 1 || distance(raw.square, pawn) !== 1 ||
      !actor.squares.includes(raw.square as Key) || after.controls.pieces.some(row => row.piece.square === raw.square ||
        row.piece.side !== actor.piece.side && row.squares.includes(raw.square as Key)) :
      raw.use_move !== playedMove || !actor.squares.includes(pawn)) return;
    const release = task.release;
    const rook = after.controls.pieces.find(row => row.piece.square === release.rook);
    if (typeof release.move !== 'string' || !/^[a-h][1-8][a-h][1-8]$/.test(release.move) ||
      !rook || rook.piece.role !== 'rook' || rook.piece.side !== asset.side || rook === actor ||
      !(guards.includes(rook) || release.rook[0] === pawn[0]) || !before.legal_moves.complete ||
      !before.legal_moves.moves.some(row => row.uci === release.move && !row.captured_square) ||
      release.move.slice(0, 2) !== release.rook || release.move[1] !== release.move[3] || release.move[0] === release.move[2] ||
      !rook.squares.includes(release.move.slice(2, 4) as Key) ||
      after.controls.pieces.some(row => row.piece.square === release.move.slice(2, 4)) ||
      release.entry !== release.move[2] + (white ? '7' : '2') || release.entry === release.move.slice(2, 4)) return;
    const clearance = after.controls.pieces.filter(row => row.piece.square[0] === release.entry[0] &&
      +row.piece.square[1] >= Math.min(+release.move[3], +release.entry[1]) &&
      +row.piece.square[1] <= Math.max(+release.move[3], +release.entry[1]));
    if (clearance.some(row => row.piece.role !== 'king' || row.piece.side !== asset.side) ||
      JSON.stringify(release.clearance) !== JSON.stringify(clearance.map(row => row.piece.square).sort())) return;
    const root = setupPosition('chess', parseFen(after.fen).unwrap()).unwrap();
    const watched = [...new Set([pawn, current, raw.square])];
    const controllers = (pos: typeof root, square: string) => [...pos.board].filter(([from, piece]) =>
      piece.color !== actor.piece.side && attacks(piece, from, pos.board.occupied).has(parseSquare(square)!)).map(([from]) => makeSquare(from));
    const expected = after.legal_moves.moves.filter(row => {
      const next = root.clone(); next.play(parseUci(row.uci)!);
      return next.isCheck() || watched.some(square => row.uci.slice(2, 4) === square ||
        controllers(next, square).some(origin => !controllers(root, square).includes(
          (origin === row.uci.slice(2, 4) ? row.uci.slice(0, 2) : origin) as typeof origin)));
    }).map(row => row.uci).sort();
    if (JSON.stringify([...raw.challenge_moves].sort()) !== JSON.stringify(expected)) return;
    return raw as unknown as CommentaryPurposeQuestion;
  }
  if (raw.support !== undefined) return;
  if (raw.kind === 'piece_pressure') {
    const plan = raw.plan;
    if (!actor || ['pawn', 'king'].includes(actor.piece.role) || !object(plan) ||
      !exactKeys(plan, ['line_id', 'start', 'use', 'join', 'route', 'targets'], ['squares', 'clearance']) || !id(plan.line_id) ||
      !Array.isArray(plan.route) || !Array.isArray(plan.targets) || !plan.targets.length) return;
    if (plan.use === -1) {
      if (actor.piece.role !== 'knight' || plan.line_id !== path.id || plan.start !== 0 || plan.join !== 1 ||
        plan.route.length || !Array.isArray(plan.squares) || plan.squares.length !== 3 ||
        !plan.squares.every(square => typeof square === 'string' && /^[a-h][1-8]$/.test(square)) ||
        plan.squares[0] !== playedMove?.slice(2, 4) || plan.squares.slice(1).join('') !== raw.use_move ||
        !Array.isArray(plan.clearance)) return;
      const squares = plan.squares as Key[], root = setupPosition('chess', parseFen(after.fen).unwrap()).unwrap();
      const reaches = (from: string, to: string) => attacks({ color: actor.piece.side, role: 'knight' },
        parseSquare(from)!, root.board.occupied).has(parseSquare(to)!);
      const via = root.board.get(parseSquare(squares[1])!);
      const enemies = (at: string) => after.controls.pieces.filter(row => row.piece.side !== actor.piece.side &&
        row.squares.includes(at as Key));
      const original = before.controls.pieces.find(row => row.piece.square === playedMove?.slice(0, 2));
      const targets = after.controls.pieces.filter(row => row.piece.side !== actor.piece.side &&
        reaches(squares[2], row.piece.square) && !original?.squares.includes(row.piece.square) &&
        !actor.squares.includes(row.piece.square)).map(row => row.piece);
      if (!reaches(squares[0], squares[1]) || !reaches(squares[1], squares[2]) ||
        squares[1] === playedMove?.slice(0, 2) || squares[2] === squares[0] || squares[2] === playedMove?.slice(0, 2) ||
        root.board.get(parseSquare(squares[2])!) || (via && (via.color !== actor.piece.side || via.role !== 'king')) ||
        JSON.stringify(plan.clearance) !== JSON.stringify(via ? [squares[1]] : []) ||
        enemies(squares[1]).some(row => row.piece.role === 'pawn') ||
        enemies(squares[2]).some(row => row.piece.role !== 'knight' || !targets.some(t => t.square === row.piece.square)) ||
        targets.length !== plan.targets.length || !plan.targets.every(t => object(t) &&
          exactKeys(t, ['side', 'role', 'square']) && targets.some(p => p.square === t.square && p.role === t.role && p.side === t.side))) return;
      return raw as unknown as CommentaryPurposeQuestion;
    }
    if (plan.squares !== undefined || plan.clearance !== undefined ||
      ![plan.start, plan.use, plan.join].every(Number.isInteger) ||
      (plan.start as number) < 0 || (plan.start as number) > 4 || (plan.start as number) % 2 ||
      (plan.use as number) <= (plan.start as number) || (plan.use as number) >= 18 ||
      (plan.use as number) % 2 || (plan.join as number) < 1 || (plan.join as number) > 5 ||
      plan.route.length < 2) return;
    const seed = observations.paths.find(p => p.id === plan.line_id);
    if (!seed || seed.moves[plan.start as number]?.uci !== playedMove ||
      seed.moves[plan.use as number]?.uci !== raw.use_move) return;
    const resource = (p: ObservationPath, n: number) => observations.positions?.find(r => r.id === p.positionIds?.[n]);
    const left = resource(path, plan.join as number), right = resource(seed, plan.join as number);
    if (!left || !right || left.fen.split(' ').slice(0, 4).join(' ') !== right.fen.split(' ').slice(0, 4).join(' ') ||
      (seed !== path && (plan.join as number) <= (plan.start as number))) return;
    const rootPiece = before.controls.pieces.find(row => row.piece.square === playedMove?.slice(0, 2))?.piece;
    const original = rootPiece && { ...rootPiece, position_id: before.id };
    const tracking = trackedPieces(seed, (plan.use as number) + 1);
    const mover = original && tracking?.find(p => samePiece(p.original, original))?.current;
    const end = resource(seed, (plan.use as number) + 1);
    const control = end?.controls.pieces.find(row => mover && samePiece({ ...row.piece, position_id: end.id }, mover));
    const exactRoute = seed.moves.flatMap((move, offset) => offset >= (plan.start as number) && offset <= (plan.use as number) &&
      trackedPieces(seed, offset)?.some(piece => original && samePiece(piece.original, original) &&
        piece.current?.square === move.uci.slice(0, 2)) ? [offset] : []);
    if (!original || !mover || !end?.controls.complete || !control ||
      JSON.stringify(exactRoute) !== JSON.stringify(plan.route) ||
      !plan.targets.every(target => object(target) && exactKeys(target, ['side', 'role', 'square']) &&
        seed.moves.slice(0, (plan.use as number) + 1).every(move =>
          move.uci.slice(0, 2) !== target.square && move.uci.slice(2, 4) !== target.square) &&
        tracking?.some(piece => samePiece(piece.original, { ...target, position_id: before.id } as CommentaryInquiryPiece) &&
          piece.current && control.squares.includes(piece.current.square) &&
          !actor.squares.includes(piece.original.square) &&
          (piece.original.side !== original.side ?
            !before.controls.pieces.find(row => samePiece({ ...row.piece, position_id: before.id }, original))
              ?.squares.includes(piece.original.square) :
            !samePiece(piece.original, original) && end.controls.pieces.some(row =>
              row.piece.side !== original.side && row.squares.includes(piece.current!.square)))))) return;
    return raw as unknown as CommentaryPurposeQuestion;
  }
  if (raw.plan !== undefined) return;
  if (!actor || raw.kind !== 'supported_advance' && !actor.squares.includes(raw.square as Key)) return;
  if (raw.kind === 'entry_route' && raw.use_move.slice(0, 2) !== playedMove?.slice(2, 4)) return;
  if (raw.kind === 'supported_advance' && !path.moves.some(move => move.uci === raw.use_move) &&
    !(before.legal_moves.complete && before.legal_moves.moves.some(row => row.uci === raw.use_move &&
      !row.captured_square && before.controls.pieces.some(piece => piece.piece.role === 'pawn' &&
        piece.piece.side === actor.piece.side && piece.piece.square === row.uci.slice(0, 2))))) return;
  if (raw.kind === 'supported_advance') {
    const useMove = raw.use_move;
    const use = path.moves.findIndex(move => move.uci === useMove);
    if (use >= 0) {
      const user = trackedPieces(path, use)?.find(piece => piece.current?.square === useMove.slice(0, 2));
      if (!user || user.original.side !== actor.piece.side || user.original.square === playedMove?.slice(0, 2)) return;
      // A witnessed piece entry needs its continuing physical supporter.
      // The independent reader reconstructs this rather than trusting a task label.
      if (use < 2 || use >= 18 || use % 2 || !newlySupported(path, user.original, raw.square as Key, use + 1)) return;
    } else if (!actor.squares.includes(raw.square as Key) || before.controls.pieces.some(row =>
      row.piece.square === playedMove?.slice(0, 2) && row.squares.includes(raw.square as Key))) return;
  }
  return raw as unknown as CommentaryPurposeQuestion;
}

interface PurposeReplyContext {
  observations: MoveReviewObservations; searches: CommentaryResponseSearch[]; roots?: CommentaryRootSearch[];
  workId: string; engineProfile: string; requiredDepth: number;
}

function rootReplyWitnesses(root: { root_move: Uci; root_occurrence_id: string; child_position_id: string },
  context: PurposeReplyContext): ObservationPath[] {
  return (context.roots ?? []).filter(work => Number(work.work_id.slice(5)) < Number(context.workId.slice(5)) &&
    work.state === 'completed' && work.stop_reason === null && work.engine_profile === context.engineProfile &&
    work.required_depth >= context.requiredDepth && work.root_occurrence_id === root.root_occurrence_id)
    .flatMap(work => work.candidates.filter(candidate => candidate.move_uci === root.root_move).flatMap(candidate => {
      const path = context.observations.paths.find(path => path.id === candidate.line_id &&
        path.moves[0]?.uci === root.root_move && path.occurrenceIds[0] === root.root_occurrence_id &&
        candidate.occurrence_id === path.occurrenceIds[0] && path.positionIds?.[1] === root.child_position_id);
      return path ? [path] : [];
    }));
}

// A past search remains a valid receipt when the current purpose changes.
// This admits only the root PV's reply versus the strongest independently
// surveyed other reply. Explaining a purpose still needs its separate checks.
function retainedRootReplyPoolMatches(search: CommentaryResponseSearch,
  observations: MoveReviewObservations, searches: CommentaryResponseSearch[], roots: CommentaryRootSearch[]): boolean {
  if (search.root_restriction.kind !== 'restricted' || search.root_restriction.moves_uci.length !== 2 ||
    search.prefix_moves_uci?.length !== 1 || search.candidates.length !== 2 ||
    !sameIds(search.root_restriction.moves_uci, search.candidates.map(candidate => candidate.move_uci))) return false;
  const pool = [...searches].reverse().find(prior => Number(prior.work_id.slice(5)) < Number(search.work_id.slice(5)) &&
    prior.root_restriction.kind === 'unrestricted' && prior.root_move === search.root_move &&
    prior.root_move_occurrence_id === search.root_move_occurrence_id &&
    prior.child_position_id === search.child_position_id && prior.prefix_moves_uci?.length === 1 &&
    prior.engine_profile === search.engine_profile && prior.required_depth >= search.required_depth);
  if (!pool) return false;
  const moves = search.root_restriction.moves_uci;
  return rootReplyWitnesses({root_move:search.root_move, root_occurrence_id:search.root_move_occurrence_id,
    child_position_id:search.child_position_id}, {observations, searches, roots, workId:search.work_id,
    engineProfile:search.engine_profile, requiredDepth:search.required_depth}).some(path => {
      const witness = path.moves[1]?.uci;
      const other = pool.candidates.find(candidate => candidate.move_uci !== witness)?.move_uci;
      return !!witness && !!other && sameIds(moves, [witness, other]);
    });
}

export function purposeResponseMovesMatch(question: CommentaryPurposeQuestion, moves: readonly Uci[],
  legalMoves: readonly Uci[], context?: PurposeReplyContext): boolean {
  if (moves.length < 2 || !moves.every(move => legalMoves.includes(move))) return false;
  if ([question.challenge_moves, legalMoves.filter(move => !question.challenge_moves.includes(move))].some(group =>
    group.length > 0 && group.every(move => moves.includes(move)) && moves.some(move => !group.includes(move))))
    return true;
  return purposeWitnessMovesMatch(question, moves, context);
}

function purposeWitnessMovesMatch(question: CommentaryPurposeQuestion, moves: readonly Uci[],
  context?: PurposeReplyContext): boolean {
  if (!context || moves.length !== 2) return false;
  const earlier = (workId: string) => Number(workId.slice(5)) < Number(context.workId.slice(5));
  const pool = [...context.searches].reverse().find(search => earlier(search.work_id) &&
    search.root_restriction.kind === 'unrestricted' && search.root_move === question.root_move &&
    search.root_move_occurrence_id === question.root_occurrence_id &&
    search.child_position_id === question.child_position_id && search.prefix_moves_uci?.length === 1 &&
    search.engine_profile === context.engineProfile && search.required_depth >= context.requiredDepth);
  const currentSource = context.observations.paths.find(path => path.id ===
    (question.plan?.line_id ?? question.source_line_id) && path.moves[0]?.uci === question.root_move &&
    path.occurrenceIds[0] === question.root_occurrence_id && path.positionIds?.[1] === question.child_position_id &&
    path.moves.slice(2).some(move => move.uci === question.use_move));
  if (!pool || !currentSource) return false;
  const sources = [currentSource];
  // A later root recheck can replace the PV without changing the supported
  // physical action. Validate an earlier request against its earlier owned
  // witness, not the new PV's first reply. Rebuild the support proof and match
  // the original beneficiary; a matching move string alone is insufficient.
  if (question.kind === 'supported_advance' && !question.plan && !question.activity_plan) {
    const beneficiary = (path: ObservationPath) => {
      const use = path.moves.findIndex(move => move.uci === question.use_move);
      return use >= 2 ? trackedPieces(path, use)?.find(piece =>
        piece.current?.square === question.use_move.slice(0, 2))?.original : undefined;
    };
    const original = beneficiary(currentSource);
    for (const path of rootReplyWitnesses(question, context)) {
      const user = beneficiary(path);
      if (original && user && samePiece(original, user) &&
        decodePurposeQuestion({ ...question, source_line_id: path.id }, context.observations,
          question.root_move, 'work_id' in question)) sources.push(path);
    }
  }
  const present = pool.candidates.map(candidate => candidate.move_uci);
  const classesReturned = present.some(move => question.challenge_moves.includes(move)) &&
    present.some(move => !question.challenge_moves.includes(move));
  // The witnessed response is compared with the strongest independent other reply.
  // This is a purpose question even when both replies share the same geometric class.
  return sources.some(source => {
    const rootWitness = source.evaluations?.some(value => ['root_search', 'focus_comparison'].includes(value.source)) ||
      context.roots?.some(work => earlier(work.work_id) && work.purpose === 'investigation_comparison' &&
        work.state === 'completed' && work.stop_reason === null && work.engine_profile === context.engineProfile &&
        work.required_depth >= context.requiredDepth && work.root_occurrence_id === question.root_occurrence_id &&
        work.candidates.some(candidate => candidate.line_id === source.id &&
          candidate.move_uci === question.root_move && candidate.occurrence_id === source.occurrenceIds[0]));
    if (!rootWitness && !classesReturned) return false;
    const witness = source.moves[1]?.uci;
    const other = present.find(move => move !== witness);
    return !!witness && !!other && sameIds([...moves], [witness, other]);
  });
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
  proposal_ownership?: {
    work_id: string;
    execution_key_sha256: string;
    question_refs: { proposal_id: string; requirement_id: string }[];
  };
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

// The reviewed mover's missing class is compared with the strongest returned
// whole-pool reply at the same child; top-N omission does not rank that class.
export function reviewedMoverResponsePoolMatches(
  inquiries: CommentaryResponseInquiry[],
  moves: Uci[],
  searches: CommentaryResponseSearch[] | undefined,
  playedMove: Uci,
): boolean {
  const inquiry = inquiries.find(inquiry => inquiry.root_move === playedMove &&
    inquiry.original_target.square === playedMove.slice(0, 2) &&
    inquiry.target.square === playedMove.slice(2, 4));
  if (!inquiry) return false;
  const owned = searches?.filter(search => responseSearchMatchesInquiry(search, inquiry)) ?? [];
  const full = owned.filter(search => search.root_restriction.kind === 'unrestricted')
    .sort((left, right) => Number(right.work_id.slice(5)) - Number(left.work_id.slice(5)))[0];
  if (!full) return moves.length === 2 && sameIds(moves, [inquiry.reference_move, inquiry.alternative_move]);
  const missing = [inquiry.capture_moves, inquiry.decline_moves].filter(family => !owned.some(search =>
    (search.root_restriction.kind === 'unrestricted' ||
      family.every(move => search.root_restriction.kind === 'restricted' &&
        search.root_restriction.moves_uci.includes(move))) &&
    search.candidates.some(candidate => family.includes(candidate.move_uci))));
  if (!missing.length)
    return moves.length === 2 && sameIds(moves, [inquiry.reference_move, inquiry.alternative_move]);
  const leader = full.candidates[0]?.move_uci;
  return !!leader && sameIds(moves, [...missing.flat(), leader]);
}

// Both issued work and completed receipts must name entire reply classes at one child.
export function responsePoolMatchesClasses(
  inquiries: CommentaryResponseInquiry[],
  moves: Uci[],
  searches?: CommentaryResponseSearch[],
  playedMove?: Uci,
): boolean {
  if (!inquiries.length) return false;
  if (playedMove && reviewedMoverResponsePoolMatches(inquiries, moves, searches, playedMove)) return true;
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
interface ContinuationSupportComparisonPoint extends ContinuationComparisonPoint {
  original_provider: CommentaryInquiryPiece;
  support: ContinuationSupport;
  comparison_id: string;
  trigger_occurrence_ids: string[];
  application?: ContinuationKingCapture;
  target_capture_event_id?: string;
  direct_recapture?: { loss_event_id: string; recapture_event_id: string };
}
interface ContinuationCaptureAvailabilityPoint extends ContinuationComparisonPoint {
  available: boolean;
  moves_uci: Uci[];
  source_ids: string[];
  position_occurrence_id: string;
  legal_inventory_complete: true;
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
        left: ContinuationCaptureAvailabilityPoint;
        right: ContinuationCaptureAvailabilityPoint;
      }
    | {
        kind: 'replacement_support' | 'root_actor_support';
        original_subject: CommentaryInquiryPiece;
        original_target: CommentaryInquiryPiece;
        relation: 'preserved_with_different_provider' | 'different_support';
        left: ContinuationSupportComparisonPoint;
        right: ContinuationSupportComparisonPoint;
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

function targetResponseComplete(detail: CommentaryTargetResponseDetail, observations: MoveReviewObservations): boolean {
  return detail.branches.every(branch => {
    const next = observations.paths.find(path => path.id === branch.line_id)?.moves[2]?.uci;
    return branch.providers.state !== 'unknown' &&
    branch.recaptures.state !== 'unknown' &&
    (!branch.recaptures.moves.length || branch.observed_recapture.state === 'complete' ||
      branch.observed_recapture.state === 'not_observed' && !!next &&
        !branch.recaptures.moves.some(move => move.move_uci === next));
  });
}

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

export function questionFocusFrame(
  focus: CommentaryQuestionFocus,
  observations: MoveReviewObservations,
): MoveReviewFrameSelection | undefined {
  const path = observations.paths.find(path => path.id === focus.line_id);
  const offset = path?.occurrenceIds.indexOf(focus.occurrence_id) ?? -1;
  if (!path || offset < 0 || path.positionIds?.[offset + (focus.phase === 'after' ? 1 : 0)] !== focus.position_id) return;
  const highlight = observations.highlights?.find(highlight => highlight.occurrenceId === focus.occurrence_id &&
    highlight.frames?.some(frame => frame.phase === focus.phase && frame.positionId === focus.position_id &&
      JSON.stringify(frame.squares) === JSON.stringify(focus.squares) &&
      JSON.stringify(frame.sourceIds) === JSON.stringify(focus.source_ids)));
  if (highlight) return { proofId: path.id, ply: offset + 1, phase: focus.phase, highlightKey: JSON.stringify(highlight) };
  return;
}

function decodeQuestionSummary(value: unknown, usedPaths: ObservationPath[], evidence: string[],
  summaryText: unknown, sources: CommentaryCandidate['sourceRefs'], observations: MoveReviewObservations,
  proposals?: CommentaryProposal[], playedMove?: Uci, responseInquiries: CommentaryResponseInquiry[] = [],
  responseSearches: CommentaryResponseSearch[] = [], rootSearches: CommentaryRootSearch[] = [],
  purposeQuestion?: CommentaryPurposeQuestion): CommentaryQuestionSummary | undefined {
  if (!object(value)) return;
  const selectedAnswer = ['observed_exchange', 'observed_terminal_comparison', 'selected_comparison'].includes(String(value.question));
  const typedAnswer = value.schema_version === 2;
  const firstResponseAnswer = ['first_target_response', 'exchange_function'].includes(String(value.question));
  const purposeAnswer = value.question === 'move_purpose' ||
    typedAnswer && value.question === 'selected_comparison' && value.move_purpose !== undefined;
  if (value.question === 'current_defense' && !typedAnswer) return;
  const commonAnswer = typedAnswer || selectedAnswer;
  const terminal = (path: ObservationPath) => {
    const ending = path.lineSummary?.ending;
    return !!ending && ['checkmate', 'stalemate', 'insufficient_material', 'fivefold_repetition', 'seventy_five_move_rule']
      .includes(ending.kind) && ending.ply_offset === path.moves.length - 1 &&
      ending.occurrence_id === path.occurrenceIds[ending.ply_offset];
  };
  if (!exactKeys(value, ['question', 'units', 'detail_moves'], ['schema_version', 'witness_line_ids', 'detail_units', 'first_target_response', 'retained_choice_line_id', 'move_purpose']) ||
    (value.schema_version !== undefined && value.schema_version !== 2) ||
    !['provider_transfer', 'support_and_interposition', 'paired_support_use',
      'escape_or_recapture', 'capture_now_or_after_attack', 'attack_or_immediate_exchange', 'counter_capture_comparison',
      'occupied_pawn_destination', 'controlled_destination', 'opening_counterplay',
      'observed_move_order', 'observed_arrangement_convergence', 'check_status_comparison', 'deferred_pawn_attack', 'contested_pair_response', 'observed_route_use', 'selected_target_response', 'attacker_removal_or_recapture',
      'observed_exchange', 'observed_terminal_comparison', 'selected_comparison', 'current_defense', 'observed_capture_target', 'investigated_mechanism', 'functional_response', 'first_target_response', 'exchange_function', 'move_purpose'].includes(String(value.question)) ||
    !Array.isArray(value.units) || !value.units.length || value.units.length > 8 ||
    (value.detail_units !== undefined && (!Array.isArray(value.detail_units) || value.detail_units.length > 8)) ||
    !Array.isArray(value.detail_moves) || value.detail_moves.length > 80 || usedPaths.length !== 2 ||
    usedPaths[0].startFen !== usedPaths[1].startFen || usedPaths[0].positionIds?.[0] !== usedPaths[1].positionIds?.[0] ||
    usedPaths[0].moves[0]?.uci === usedPaths[1].moves[0]?.uci || usedPaths.some(path =>
      !path.lineSummary?.material || (path.lineSummary.ending.kind !== 'line_end' && !(commonAnswer && terminal(path))) ||
      !path.lineSummary.premise_ids.every(id => evidence.includes(id)) ||
      (!commonAnswer && value.question !== 'deferred_pawn_attack' && path.lineSummary.material.events.some(event => event.promotion_gain_cp)) ||
      !sources.some(source => source.line_id === path.id &&
        (source.stop === null || (commonAnswer && terminal(path) && source.stop === 'automatic_terminal')) &&
        source.accepted_ply_count === path.moves.length))) return;
  if (value.question === 'observed_terminal_comparison' && !usedPaths.some(terminal)) return;
  const firstOwner = value.first_target_response;
  if (firstResponseAnswer ?
    !typedAnswer || !object(firstOwner) || !exactKeys(firstOwner, ['response_inquiry_id', 'full_pool_work_id',
      'full_pool_execution_key_sha256', 'capture_decline_work_id', 'capture_decline_execution_key_sha256']) ||
      !['response_inquiry_id', 'full_pool_work_id', 'capture_decline_work_id'].every(field => id(firstOwner[field])) ||
      !['full_pool_execution_key_sha256', 'capture_decline_execution_key_sha256'].every(field =>
        typeof firstOwner[field] === 'string' && /^[a-f0-9]{64}$/.test(firstOwner[field] as string)) :
    firstOwner !== undefined) return;
  // A first-target body belongs to the reviewed mover and its completed child
  // comparison. A question name cannot grant an unrelated path this ownership.
  const firstTarget = firstResponseAnswer && typedAnswer && playedMove ?
    responseInquiries.find(inquiry => {
      const primary = usedPaths.find(path => path.moves[0]?.uci === playedMove);
      return primary && object(firstOwner) && inquiry.id === firstOwner.response_inquiry_id &&
        inquiry.state === 'answered' && inquiry.stop_reason === null && inquiry.answer &&
        inquiry.root_move === playedMove && inquiry.root_occurrence_id === primary.occurrenceIds[0] &&
        inquiry.root_move_occurrence_id === primary.occurrenceIds[0] &&
        inquiry.child_position_id === primary.positionIds?.[1] &&
        inquiry.original_target.square === playedMove.slice(0, 2) &&
        inquiry.target.square === playedMove.slice(2, 4);
    }) : undefined;
  const firstPair = firstTarget?.answer && object(firstOwner) ? responseSearches.find(search =>
    responseSearchMatchesInquiry(search, firstTarget) && search.work_id === firstTarget.answer!.work_id &&
    search.execution_key_sha256 === firstTarget.answer!.execution_key_sha256 &&
    search.work_id === firstOwner.capture_decline_work_id &&
    search.execution_key_sha256 === firstOwner.capture_decline_execution_key_sha256 &&
    search.engine_profile === firstTarget.answer!.engine_profile &&
    search.required_depth === firstTarget.answer!.required_depth &&
    [firstTarget.answer!.reference_line_id, firstTarget.answer!.alternative_line_id].every(line =>
      search.candidates.some(candidate => candidate.line_id === line))) : undefined;
  const firstPool = firstTarget && firstPair ? [...responseSearches].reverse().filter(search =>
    responseSearchMatchesInquiry(search, firstTarget) && search.root_restriction.kind === 'unrestricted' &&
    search.engine_profile === firstPair.engine_profile && search.required_depth >= firstPair.required_depth)
    .sort((left, right) => right.required_depth - left.required_depth)[0] : undefined;
  const firstLeader = firstPool?.candidates[0];
  const firstReplyPath = firstLeader && firstPair?.candidates.find(candidate => candidate.move_uci === firstLeader.move_uci);
  if (firstResponseAnswer) {
    if (!object(firstOwner) || !firstTarget || !firstPool || !firstPair || !firstReplyPath ||
      firstPool.work_id !== firstOwner.full_pool_work_id ||
      firstPool.execution_key_sha256 !== firstOwner.full_pool_execution_key_sha256) return;
    const selection = firstLeader!.move_uci === firstTarget.reference_move ? firstTarget.reference_selection :
      firstLeader!.move_uci === firstTarget.alternative_move ? firstTarget.alternative_selection : undefined;
    const selected = selection?.source === 'child_search' && responseSearches.find(search =>
      responseSearchMatchesInquiry(search, firstTarget) && search.work_id === selection.work_id &&
      search.candidates.some(candidate => candidate.move_uci === firstLeader!.move_uci && candidate.line_id === selection.line_id));
    const leaderPath = observations.paths.find(path => path.id === firstLeader!.line_id);
    const pairPath = observations.paths.find(path => path.id === firstReplyPath.line_id);
    const selectedPath = selected && observations.paths.find(path => path.id === selection!.line_id);
    if (!leaderPath || !pairPath || !selectedPath || leaderPath.moves.length < 2 ||
      ![pairPath, selectedPath].every(path => path.moves.length >= 2 && [0, 1].every(index =>
        path.moves[index].uci === leaderPath.moves[index].uci &&
        path.occurrenceIds[index] === leaderPath.occurrenceIds[index] &&
        path.positionIds?.[index + 1] === leaderPath.positionIds?.[index + 1]))) return;
  }
  const conditionalPaths = new Set<string>();
  if (purposeAnswer) {
    const purpose = typedAnswer && decodePurposeQuestion(value.move_purpose, observations, playedMove, true);
    const owner = value.move_purpose;
    if (!purpose || !object(owner) || !purposeQuestion ||
      !(Object.keys(purposeQuestion) as (keyof CommentaryPurposeQuestion)[]).every(key =>
        JSON.stringify(purpose[key]) === JSON.stringify(purposeQuestion[key]))) return;
    if (purpose.prepared_choice && !usedPaths.some(path => path.id === purpose.prepared_choice?.line_id)) return;
    const search = responseSearches.find(search => search.work_id === owner.work_id &&
      search.execution_key_sha256 === owner.execution_key_sha256 &&
      search.root_move === purpose.root_move && search.root_move_occurrence_id === purpose.root_occurrence_id &&
      search.child_position_id === purpose.child_position_id && search.prefix_moves_uci?.length === 1);
    if (!search) return;
    const contrasted = search.candidates.some(c => purpose.challenge_moves.includes(c.move_uci)) &&
      search.candidates.some(c => !purpose.challenge_moves.includes(c.move_uci));
    const child = observations.positions?.find(p => p.id === search.child_position_id);
    const fullSurvey = search.root_restriction.kind === 'unrestricted' ||
      child?.legal_moves?.complete && sameIds(search.root_restriction.moves_uci,
        child.legal_moves.moves.map(move => move.uci));
    const witness = purpose.plan ? purpose.plan.use >= 0 && purpose.plan.line_id : purpose.source_line_id;
    const surveyed = value.question === 'selected_comparison' && fullSurvey && search.candidates.length >= 2 &&
      search.candidates.some(c => c.line_id === witness && observations.paths.some(path =>
        path.id === c.line_id && path.moves.slice(2).some(move => move.uci === purpose.use_move)));
    // A full survey can select different activities for its strongest pair.
    // An already owned recheck can also report an activity on just one path.
    const activitiesOwned = Boolean(purpose.reply_activities &&
      purpose.reply_activities.every(a => search.candidates.some(c => c.line_id === a.line_id)));
    const activitiesSurveyed = value.question === 'selected_comparison' && fullSurvey && activitiesOwned &&
      purpose.reply_activities?.length === 2 && purpose.reply_activities[0].line_id === search.candidates[0]?.line_id;
    const witnessed = value.question === 'selected_comparison' && search.root_restriction.kind === 'restricted' &&
      search.candidates.length === 2 &&
      sameIds(search.candidates.map(candidate => candidate.move_uci), search.root_restriction.moves_uci) &&
      purposeWitnessMovesMatch(purpose, search.root_restriction.moves_uci, {
        observations, searches: responseSearches, roots: rootSearches, workId: search.work_id,
        engineProfile: search.engine_profile, requiredDepth: search.required_depth,
      });
    if (!contrasted && !surveyed && !activitiesSurveyed && !witnessed ||
      purpose.kind === 'enabled_activity' && !surveyed && !activitiesSurveyed && !witnessed) return;
    // Returned activities keep their exact recheck owner. They cannot replace
    // the independent survey and original witness that authorized this pair.
    if (purpose.reply_activities && !(activitiesOwned && (activitiesSurveyed || witnessed))) return;
    for (const candidate of search.candidates) conditionalPaths.add(candidate.line_id);
    if (owner.choice !== undefined) {
      const choice = owner.choice;
      if (purpose.kind !== 'exchange_activity' || !object(choice) ||
        !exactKeys(choice, ['alternative_move', 'work_id', 'execution_key_sha256', 'line_ids']) ||
        typeof choice.alternative_move !== 'string' || choice.alternative_move === playedMove ||
        choice.alternative_move.slice(2, 4) !== purpose.square ||
        !Array.isArray(choice.line_ids) || choice.line_ids.length !== 2 || new Set(choice.line_ids).size !== 2) return;
      const rootWork = rootSearches.find(work => work.work_id === choice.work_id &&
        work.execution_key_sha256 === choice.execution_key_sha256 && work.state === 'completed' &&
        work.stop_reason === null && work.engine_profile === search.engine_profile &&
        work.required_depth >= search.required_depth && Number(work.work_id.slice(5)) < Number(search.work_id.slice(5)) &&
        work.root_position_id === usedPaths[0].positionIds?.[0]);
      const rootPosition = observations.positions?.find(p => p.id === usedPaths[0].positionIds?.[0]);
      const alternative = rootPosition?.legal_moves?.moves.find(row => row.uci === choice.alternative_move);
      const alternativeActor = rootPosition?.controls.pieces.find(row => row.piece.square ===
        String(choice.alternative_move).slice(0, 2))?.piece;
      if (!rootWork || !alternative || alternative.captured_square || !alternativeActor ||
        !['bishop', 'knight', 'rook', 'queen'].includes(alternativeActor.role) ||
        alternativeActor.square === playedMove?.slice(0, 2)) return;
      const choiceLineIds = choice.line_ids;
      for (let i = 0; i < 2; i++) {
        const path = observations.paths.find(path => path.id === choiceLineIds[i]);
        const root = i === 0 ? playedMove : choice.alternative_move;
        if (!path || path.moves[0]?.uci !== root ||
          !rootWork.candidates.some(candidate => candidate.move_uci === root && candidate.line_id === path.id &&
            candidate.occurrence_id === path.occurrenceIds[0])) return;
        conditionalPaths.add(path.id);
      }
    }
  } else if (value.move_purpose !== undefined) return;
  if (value.retained_choice_line_id !== undefined) {
    const path = observations.paths.find(path => path.id === value.retained_choice_line_id);
    if (value.question !== 'exchange_function' || !typedAnswer || !firstPair || !path ||
      usedPaths.some(root => root.moves[0]?.uci === path.moves[0]?.uci) ||
      !rootSearches.some(search => search.state === 'completed' && search.stop_reason === null &&
        search.purpose === 'root_search' && search.root_restriction.kind === 'unrestricted' &&
        search.engine_profile === firstPair.engine_profile && search.required_depth >= firstPair.required_depth &&
        search.root_position_id === usedPaths[0].positionIds?.[0] &&
        search.root_occurrence_id === firstTarget!.root_occurrence_id &&
        search.candidates.some(candidate => candidate.line_id === path.id &&
          candidate.move_uci === path.moves[0]?.uci && candidate.occurrence_id === path.occurrenceIds[0]))) return;
    conditionalPaths.add(path.id);
  }
  // Other selected questions can carry a conditional proof in detail. A
  // different label alone cannot move its foreign root into the core account.
  const conditionalDetailIds = new Set<string>(Array.isArray(value.detail_units) ? value.detail_units
    .filter(object).flatMap(unit => Array.isArray(unit.proof_moves) ? unit.proof_moves.filter(object)
      .filter(ref => id(ref.line_id)).map(ref => ref.line_id as string) : []) : []);
  if (typedAnswer) {
    type Fact = Extract<CommentaryProposalReading, { kind: 'fact' }>;
    const condition = (path: ObservationPath, predicate: CommentaryFactPredicate): string | undefined => {
      const original = predicate.subject, resource = observations.positions?.find(p => p.id === path.positionIds?.[1]);
      const current = original && trackedPieces(path, 1)?.find(piece =>
        piece.original.side === original.side && piece.original.role === original.role &&
        piece.original.square === original.square)?.current;
      if (predicate.kind === 'location') return current?.square;
      if (predicate.kind === 'control' && current && predicate.square && resource?.controls.complete) {
        const row = resource.controls.pieces.find(row => row.piece.side === current.side &&
          row.piece.role === current.role && row.piece.square === current.square);
        if (row) return row.squares.includes(predicate.square) ? 'present' : 'absent';
      }
      return;
    };
    for (const proposal of proposals ?? []) {
      if (proposal.admission !== 'generated' || proposal.origin.question.producer !== 'C-reply-function') continue;
      const facts = proposal.requirements.filter((reading): reading is Fact => reading.kind === 'fact');
      const fact = (id: string) => facts.find(reading => reading.id === id);
      const root = proposal.requirements.find(reading => reading.id === 'function_root'),
        context = proposal.requirements.find(reading => reading.id === 'function_context'),
        live = fact('function_live'), left = fact('function_effect_reference'), right = fact('function_effect_alternative');
      if (root?.kind !== 'compare_moves' || root.prefix_moves_uci.length ||
        !playedMove || ![root.reference_move, root.alternative_move].includes(playedMove) ||
        context?.kind !== 'fact_group' || context.mode !== 'all' || context.state !== 'supported' ||
        !context.members.includes('function_live') || context.members.some(id =>
          proposal.requirements.find(reading => reading.id === id)?.state !== 'supported') ||
        !live || live.path.source.kind !== 'declared' || live.path.source.moves_uci.length !== 1 ||
        live.path.source.moves_uci[0] !== root.reference_move || live.at.kind !== 'ply' || live.at.ply_count !== 1 ||
        !left || !right || left.path.source.kind !== 'declared' || right.path.source.kind !== 'declared') continue;
      const l = left.path.source.moves_uci, r = right.path.source.moves_uci;
      if (l.length < 2 || l.length !== r.length || l[0] !== root.reference_move || r[0] !== root.alternative_move ||
        !l.slice(1).every((move, index) => r[index + 1] === move)) continue;
      const primary = usedPaths.find(path => path.moves[0]?.uci === root.reference_move),
        peer = usedPaths.find(path => path !== primary), peerFact = fact('function_root_alternative');
      if (!primary || !peer || !peerFact || live.expected === undefined || peerFact.expected === undefined ||
        JSON.stringify(live.predicate) !== JSON.stringify(peerFact.predicate) ||
        condition(primary, live.predicate) !== live.expected || condition(peer, live.predicate) !== peerFact.expected ||
        live.expected === peerFact.expected) continue;
      for (const reading of facts.filter(fact => ['function_effect_reference', 'function_effect_alternative',
        'function_option', 'function_price'].includes(fact.id))) {
        if (reading.state !== 'supported' || reading.path.source.kind !== 'declared') continue;
        const moves = reading.path.source.moves_uci;
        for (const path of observations.paths) if (path.startFen === primary.startFen &&
          path.positionIds?.[0] === primary.positionIds?.[0] &&
          (value.question === 'functional_response' || conditionalDetailIds.has(path.id)) &&
          moves.every((move, index) => path.moves[index]?.uci === move)) conditionalPaths.add(path.id);
      }
    }
  }
  // A completed first-target inquiry may supplement the actual root pair.
  // Its issued reply paths keep their own receipt; sharing the root is not enough.
  const replyWitnesses = new Set<string>();
  if (typedAnswer && value.question === 'selected_comparison' && playedMove) {
    const primary = usedPaths.find(path => path.moves[0]?.uci === playedMove);
    for (const inquiry of responseInquiries) {
      if (!primary || inquiry.state !== 'answered' || inquiry.stop_reason !== null || !inquiry.answer ||
        inquiry.root_move !== playedMove || inquiry.root_occurrence_id !== primary.occurrenceIds[0] ||
        inquiry.root_move_occurrence_id !== primary.occurrenceIds[0] ||
        inquiry.child_position_id !== primary.positionIds?.[1] ||
        inquiry.original_target.square !== playedMove.slice(0, 2) ||
        inquiry.target.square !== playedMove.slice(2, 4)) continue;
      const answer = inquiry.answer;
      const search = responseSearches.find(search => responseSearchMatchesInquiry(search, inquiry) &&
        search.work_id === answer.work_id && search.execution_key_sha256 === answer.execution_key_sha256 &&
        search.engine_profile === answer.engine_profile && search.required_depth === answer.required_depth);
      const lines = [answer.reference_line_id, answer.alternative_line_id];
      if (search && lines.every(line => search.candidates.some(candidate => candidate.line_id === line))) {
        lines.forEach(line => replyWitnesses.add(line));
        // A functional comparison may use another returned peer of this same
        // independently searched pool. The old capture/noncapture labels do not
        // own the only meaningful pair, but a shared root alone grants nothing.
        const pool = [...responseSearches].reverse().filter(pool =>
          responseSearchMatchesInquiry(pool, inquiry) && pool.root_restriction.kind === 'unrestricted' &&
          pool.engine_profile === search.engine_profile && pool.required_depth >= search.required_depth)
          .sort((a, b) => b.required_depth - a.required_depth)[0];
        const leader = pool?.candidates[0];
        const selection = leader?.move_uci === inquiry.reference_move ? inquiry.reference_selection :
          leader?.move_uci === inquiry.alternative_move ? inquiry.alternative_selection : undefined;
        if (pool && leader && selection?.source === 'child_search' && responseSearches.some(origin =>
          responseSearchMatchesInquiry(origin, inquiry) && origin.work_id === selection.work_id &&
          origin.engine_profile === pool.engine_profile && origin.candidates.some(candidate =>
            candidate.move_uci === leader.move_uci && candidate.line_id === selection.line_id)))
          pool.candidates.forEach(candidate => replyWitnesses.add(candidate.line_id));
      }
    }
  }
  const summaryPaths = [...usedPaths];
  if (value.witness_line_ids !== undefined) {
    const acceptsWitnesses = value.question === 'attacker_removal_or_recapture' ||
      (typedAnswer && (['investigated_mechanism', 'observed_terminal_comparison', 'functional_response'].includes(String(value.question)) ||
        firstResponseAnswer && !!firstReplyPath ||
        conditionalPaths.size > 0));
    if ((!acceptsWitnesses && !replyWitnesses.size) || !ids(value.witness_line_ids) ||
      // Each delivery layer has its own five-path proof budget. The exact
      // referenced union is checked after admitting the units below.
      !value.witness_line_ids.length || value.witness_line_ids.length > (typedAnswer ? 10 : 5)) return;
    for (const lineId of value.witness_line_ids) {
      const path = observations.paths.find(path => path.id === lineId);
      // Retain the current evaluation pair. A supplementary path only witnesses
      // an actual legal response after one of those same root transitions.
      if (!path || (!acceptsWitnesses && !replyWitnesses.has(lineId)) ||
        summaryPaths.includes(path) || !path.lineSummary?.material ||
        (purposeAnswer && !conditionalPaths.has(path.id) &&
          (!object(value.move_purpose) || value.move_purpose.source_line_id !== path.id)) ||
        (path.lineSummary.ending.kind !== 'line_end' && !(typedAnswer && terminal(path))) ||
        !path.lineSummary.premise_ids.every(id => evidence.includes(id)) ||
        (!typedAnswer && path.lineSummary.material.events.some(event => event.promotion_gain_cp)) ||
        !usedPaths.some(primary => primary.startFen === path.startFen &&
          primary.positionIds?.[0] === path.positionIds?.[0] &&
          (primary.occurrenceIds[0] === path.occurrenceIds[0] && primary.moves[0]?.uci === path.moves[0]?.uci ||
            conditionalPaths.has(path.id)))) return;
      summaryPaths.push(path);
    }
  }
  const move = (raw: unknown): raw is CommentaryQuestionMove => object(raw) &&
    exactKeys(raw, ['line_id', 'occurrence_id']) && id(raw.line_id) && id(raw.occurrence_id) &&
    evidence.includes(raw.occurrence_id) && summaryPaths.some(path => path.id === raw.line_id && path.occurrenceIds.includes(raw.occurrence_id as string));
  const parts = (raw: unknown): raw is (string | CommentaryQuestionMove)[] => Array.isArray(raw) &&
    raw.length > 0 && raw.length <= 80 && raw.every(part => typeof part === 'string' ? part.length > 0 && part.length <= 2000 : move(part));
  const key = (ref: CommentaryQuestionMove) => `${ref.line_id}/${ref.occurrence_id}`;
  const refs = (parts: (string | CommentaryQuestionMove)[]) => parts.filter((p): p is CommentaryQuestionMove => typeof p !== 'string');
  const proofRefs = (unit: CommentaryQuestionUnit) => unit.proof_moves ?? refs(unit.en);
  const accountRefs = (unit: CommentaryQuestionUnit) => unit.action_moves ?? (unit.proof_moves === undefined ? refs(unit.en) :
    [...new Map([...proofRefs(unit), ...refs(unit.en)].map(ref => [key(ref), ref])).values()]);
  const signature = (moves: CommentaryQuestionMove[]) => moves.map(key).sort().join('|');
  if (!value.detail_moves.every(move) || new Set(value.detail_moves.map(key)).size !== value.detail_moves.length) return;
  for (const unit of [...value.units, ...((value.detail_units as unknown[] | undefined) ?? [])]) {
    if (!object(unit) || !exactKeys(unit, ['role', 'en', 'ko', 'covered_moves'], ['proof_moves', 'action_moves', 'required_moves', 'recorded_end_moves', 'focus', 'english_only']) ||
      !['condition', 'reply', 'cost'].includes(String(unit.role)) || !parts(unit.en) || !parts(unit.ko) ||
      !Array.isArray(unit.covered_moves) || unit.covered_moves.length > 80 || !unit.covered_moves.every(move) ||
      signature(refs(unit.en)) !== signature(refs(unit.ko)) || !refs(unit.en).length ||
      new Set(unit.covered_moves.map(key)).size !== unit.covered_moves.length ||
      (unit.role === 'cost') !== (unit.covered_moves.length > 0) ||
      (unit.english_only !== undefined && (unit.english_only !== true || JSON.stringify(unit.en) !== JSON.stringify(unit.ko)))) return;
    if (unit.proof_moves !== undefined && (!Array.isArray(unit.proof_moves) || !unit.proof_moves.length ||
      unit.proof_moves.length > summaryPaths.reduce((size, path) => size + path.occurrenceIds.length, 0) ||
      !unit.proof_moves.every(move) ||
      new Set(unit.proof_moves.map(key)).size !== unit.proof_moves.length)) return;
    const proof = unit.proof_moves as CommentaryQuestionMove[] | undefined;
    const owned = new Set((proof ?? refs(unit.en)).map(key));
    if (unit.action_moves !== undefined && (!typedAnswer || !proof || !Array.isArray(unit.action_moves) ||
      unit.action_moves.length > proof.length || !unit.action_moves.every(move) ||
      new Set(unit.action_moves.map(key)).size !== unit.action_moves.length ||
      unit.action_moves.some(ref => !owned.has(key(ref))) ||
      unit.covered_moves.some(ref => !(unit.action_moves as CommentaryQuestionMove[]).some(other => key(other) === key(ref))))) return;
    if (proof && (refs(unit.en).some(ref => !owned.has(key(ref)) &&
      (!proof.some(other => other.line_id === ref.line_id) ||
        summaryPaths.find(path => path.id === ref.line_id)!.occurrenceIds[0] !== ref.occurrence_id)) ||
      unit.covered_moves.some(ref => !owned.has(key(ref))))) return;
    if (unit.required_moves !== undefined && (!Array.isArray(unit.required_moves) || unit.required_moves.length > 80 ||
      !unit.required_moves.every(move) || new Set(unit.required_moves.map(key)).size !== unit.required_moves.length ||
      unit.required_moves.some(ref => !owned.has(key(ref))))) return;
    if (unit.recorded_end_moves !== undefined && (!Array.isArray(unit.recorded_end_moves) || unit.recorded_end_moves.length > 80 ||
      !unit.recorded_end_moves.every(move) || new Set(unit.recorded_end_moves.map(key)).size !== unit.recorded_end_moves.length ||
      unit.recorded_end_moves.some(ref => {
        const path = summaryPaths.find(path => path.id === ref.line_id)!, ending = path.lineSummary!.ending;
        return !owned.has(key(ref)) || ending.kind !== 'line_end' || ending.ply_offset !== path.moves.length - 1 ||
          ending.occurrence_id !== ref.occurrence_id || path.occurrenceIds[path.moves.length - 1] !== ref.occurrence_id;
      }))) return;
    if (unit.focus !== undefined) {
      if (!Array.isArray(unit.focus) || unit.focus.length > 80) return;
      const assigned = new Set<string>();
      for (const focus of unit.focus) {
        if (!object(focus) || !exactKeys(focus, ['line_id', 'occurrence_id', 'phase', 'position_id', 'squares', 'source_ids']) ||
          !move({ line_id: focus.line_id, occurrence_id: focus.occurrence_id }) ||
          !['before', 'after'].includes(String(focus.phase)) || !id(focus.position_id) ||
          !ids(focus.squares) || !focus.squares.length || focus.squares.length > 64 ||
          !focus.squares.every(square => /^[a-h][1-8]$/.test(square)) || !ids(focus.source_ids)) return;
        const ref = focus as unknown as CommentaryQuestionFocus;
        if (!owned.has(key(ref)) || assigned.has(key(ref)) || !questionFocusFrame(ref, observations)) return;
        assigned.add(key(ref));
      }
    }
  }
  const result = value as unknown as CommentaryQuestionSummary, mentioned = result.units.flatMap(accountRefs);
  const witnessIds = (units: CommentaryQuestionUnit[], extra: CommentaryQuestionMove[] = []) => new Set(
    [...units.flatMap(unit => [...proofRefs(unit), ...refs(unit.en), ...refs(unit.ko),
      ...unit.covered_moves, ...(unit.required_moves ?? []), ...(unit.recorded_end_moves ?? []),
      ...(unit.focus ?? [])]), ...extra].map(ref => ref.line_id)
      .filter(lineId => !usedPaths.some(path => path.id === lineId)),
  );
  const coreWitnesses = witnessIds(result.units),
    detailWitnesses = witnessIds(result.detail_units ?? [], result.detail_moves),
    referencedWitnesses = new Set([...coreWitnesses, ...detailWitnesses]);
  if (coreWitnesses.size > 5 || detailWitnesses.size > 5 ||
    !sameIds(result.witness_line_ids ?? [], [...referencedWitnesses])) return;
  const text = result.units.map(unit => unit.en.map(part => typeof part === 'string' ? part : (() => {
    const path = summaryPaths.find(path => path.id === part.line_id)!;
    return path.moves[path.occurrenceIds.indexOf(part.occurrence_id)].label;
  })()).join('')).join(' ');
  if (text !== summaryText || text.length > 4000 || usedPaths.some(path => !result.units.flatMap(unit => refs(unit.en)).some(ref =>
    ref.line_id === path.id && ref.occurrence_id === path.occurrenceIds[0]))) return;
  const delivered = (units: CommentaryQuestionUnit[], extra: CommentaryQuestionMove[] = []): boolean => {
    const mentioned = [...units.flatMap(accountRefs), ...extra],
      recordedEnds = new Set(units.flatMap(unit => unit.recorded_end_moves ?? []).map(key));
    if (units.some(unit => unit.covered_moves.some(ref => !proofRefs(unit).some(other => key(ref) === key(other))))) return false;
    for (const path of summaryPaths) {
      const visible = mentioned.filter(ref => ref.line_id === path.id),
        covered = [...new Map(units.flatMap(unit => unit.covered_moves).filter(ref => ref.line_id === path.id)
          .map(ref => [key(ref), ref])).values()],
        material = path.lineSummary!.material!.events,
        has = (offset: number) => visible.some(ref => ref.occurrence_id === path.occurrenceIds[offset]);
      if (covered.some(ref =>
        !visible.some(other => key(other) === key(ref)) || !material.some(event => event.capture && event.occurrence_id === ref.occurrence_id))) return false;
      for (const ref of visible) {
        const offset = path.occurrenceIds.indexOf(ref.occurrence_id),
          capture = material.find(event => event.ply_offset === offset && event.capture),
          previous = material.find(event => event.ply_offset === offset - 1 && event.capture),
          next = material.find(event => event.ply_offset === offset + 1 && event.capture),
          ended = commonAnswer && terminal(path) && offset === path.lineSummary!.ending.ply_offset;
        if (capture && !covered.some(other => key(other) === key(ref))) return false;
        if (!ended && !recordedEnds.has(key(ref)) && !has(offset + 1) &&
          setupPosition('chess', parseFen(path.moves[offset].fenAfter).unwrap()).unwrap().isCheck()) return false;
        if (capture?.capture?.recapture === 'proven' && previous && previous.side !== capture.side &&
          capture.capture.square === path.moves[offset - 1]?.uci.slice(2, 4) && !has(offset - 1)) return false;
        if (capture && next?.capture?.recapture === 'proven' && next.side !== capture.side &&
          next.capture.square === path.moves[offset].uci.slice(2, 4) && !has(offset + 1)) return false;
      }
    }
    return true;
  };
  if (firstResponseAnswer) {
    const observed = observations.paths.find(path => path.id === firstReplyPath!.line_id);
    const core = result.units.flatMap(accountRefs);
    if (!observed || observed.moves.length < 3 || !summaryPaths.some(path =>
      path.moves.length >= 3 && path.moves.slice(0, 3).every((move, index) => move.uci === observed.moves[index]?.uci &&
        path.occurrenceIds[index] === observed.occurrenceIds[index] &&
        path.positionIds?.[index + 1] === observed.positionIds?.[index + 1]) &&
      [1, 2].every(offset => core.some(ref => ref.line_id === path.id &&
        ref.occurrence_id === path.occurrenceIds[offset])))) return;
  }
  if (result.detail_units && !delivered(result.detail_units, typedAnswer ? result.detail_moves : [])) return;
  if (commonAnswer || result.question === 'selected_target_response' || result.question === 'attacker_removal_or_recapture') {
    // The producer owns the selected physical question and its closed account.
    // Position proof can have a SAN link without selecting that transition's
    // capture. Actual roots, check replies and recaptures still close the account.
    const roots: CommentaryQuestionMove[] = [];
    if (typedAnswer) for (const path of usedPaths) {
      const occurrence = path.occurrenceIds[0];
      if (!id(occurrence)) return;
      roots.push({ line_id: path.id, occurrence_id: occurrence });
    }
    // A typed position witness may describe an available conditional reply.
    // Its certified proof must be shown, while only selected actual actions
    // enter the capture/check account above.
    const witnessed = typedAnswer ? result.units.flatMap(proofRefs) : mentioned;
    const conditionalDetail = typedAnswer ? (result.detail_units ?? []).flatMap(proofRefs) : [];
    if ((!commonAnswer && result.detail_moves.length) || !delivered(result.units, roots) ||
      summaryPaths.some(path => !witnessed.some(ref => ref.line_id === path.id) &&
        !(typedAnswer && conditionalDetail.some(ref => ref.line_id === path.id)))) return;
    return result;
  }
  // Unversioned summaries retain their original motif-specific compatibility checks.
  // Version 2 has one producer-selected account; Unit grouping does not prove its meaning.
  const alignment = ['observed_move_order', 'observed_arrangement_convergence', 'observed_route_use'].includes(result.question);
  const ends = new Map<string, number>();
  for (const path of usedPaths) {
    const core = mentioned.filter(ref => ref.line_id === path.id),
      covered = result.units.flatMap(unit => unit.covered_moves).filter(ref => ref.line_id === path.id),
      detail = result.detail_moves.filter(ref => ref.line_id === path.id),
      all = [...core, ...covered, ...(alignment ? detail : [])], end = Math.max(...all.map(ref => path.occurrenceIds.indexOf(ref.occurrence_id))),
      materialEvents = path.lineSummary!.material!.events,
      // This question states a material result across the entire supplied line.
      captures = materialEvents.filter(event => event.capture &&
        (result.question === 'occupied_pawn_destination' || event.ply_offset <= end));
    ends.set(path.id, end);
    if (alignment && (detail.length !== end + 1 || path.occurrenceIds.slice(0, end + 1).some(id =>
      !detail.some(ref => ref.occurrence_id === id)))) return;
    const materialQuestion = ['paired_support_use', 'escape_or_recapture',
      'capture_now_or_after_attack', 'attack_or_immediate_exchange', 'counter_capture_comparison', 'occupied_pawn_destination', 'opening_counterplay',
      'check_status_comparison', 'deferred_pawn_attack', 'contested_pair_response', 'observed_route_use'].includes(result.question);
    for (let offset = 0; offset <= end; offset++) {
      if (!/[+#]$/.test(path.moves[offset].label)) continue;
      const matches = (ref: CommentaryQuestionMove, index: number) =>
        ref.line_id === path.id && ref.occurrence_id === path.occurrenceIds[index];
      const answeredInUnit = ['escape_or_recapture', 'opening_counterplay', 'check_status_comparison', 'deferred_pawn_attack', 'contested_pair_response'].includes(result.question) &&
        result.units.some(unit => [offset, offset + 1].every(index => accountRefs(unit).some(ref => matches(ref, index))));
      const neutralCheck = result.question === 'check_status_comparison' && captures.some(first => first.ply_offset === offset &&
        captures.some(second => second.ply_offset === offset + 1 && second.side !== first.side &&
          second.capture!.role === first.capture!.role && second.capture!.recapture === 'proven'));
      const pairedInDetail = (result.question === 'counter_capture_comparison' || neutralCheck) &&
        [offset, offset + 1].every(index => result.detail_moves.some(ref => matches(ref, index)) &&
          !core.some(ref => matches(ref, index)) && (neutralCheck || !captures.some(event => event.ply_offset === index)));
      if (!answeredInUnit && !pairedInDetail) return;
    }
    if (materialQuestion) {
      // A countercapture account follows the original taker through its removal,
      // including an intervening gain or trade. Its last link cannot set its scope.
      if (result.question === 'counter_capture_comparison' && captures.length) {
        const first = captures[0], captor = trackedPieces(path, first.ply_offset)?.find(piece =>
          piece.current.square === path.moves[first.ply_offset].uci.slice(0, 2))?.original;
        if (!captor || materialEvents.some(event => event.capture && event.ply_offset > end &&
          trackedPieces(path, event.ply_offset)?.some(piece => piece.current.square === event.capture!.square &&
            samePiece(piece.original, captor)))) return;
      }
      // Truncating the declared prefix must not hide its immediate recapture.
      // The actual capture square distinguishes an unrelated countercapture.
      const lastCapture = captures.find(event => event.ply_offset === end),
        nextCapture = path.lineSummary!.material!.events.find(event => event.ply_offset === end + 1 && event.capture);
      if (lastCapture && nextCapture?.capture?.recapture === 'proven' && nextCapture.side !== lastCapture.side &&
        nextCapture.capture.square === path.moves[end].uci.slice(2, 4)) return;
      if (new Set(covered.map(key)).size !== covered.length ||
        captures.length !== covered.length || captures.some(event => !covered.some(ref => ref.occurrence_id === event.occurrence_id)) ||
        covered.some(ref => !core.concat(result.detail_moves).some(other => key(other) === key(ref)))) return;
    } else if (covered.length || captures.length) return;
    if (result.question === 'paired_support_use') {
      if (!covered.length) return;
      const balance = new Map<string, number>();
      for (const event of captures) balance.set(event.capture!.role,
        (balance.get(event.capture!.role) ?? 0) + (event.side === 'white' ? 1 : -1));
      if ([...balance.values()].some(value => value !== 0)) return;
    }
  }
  if (result.question === 'observed_route_use') {
    const states = usedPaths.map(path => Array.from({ length: path.moves.length + 1 }, (_, count) => trackedPieces(path, count))),
      roots = usedPaths.map((path, n) => states[n][0]?.find(p => p.current.square === path.moves[0].uci.slice(0, 2))?.original);
    const validRoute = roots.some(focus => focus && (() => {
      const routes = usedPaths.map((path, n) => path.moves.flatMap((move, offset) => {
        const before = states[n][offset]?.find(p => samePiece(p.original, focus)),
          after = states[n][offset + 1]?.find(p => samePiece(p.original, focus));
        return before && after && before.current.square !== after.current.square ? [{ offset, square: after.current.square, uci: move.uci }] : [];
      }));
      const meetings = routes[0].flatMap(left => routes[1].filter(right => left.square === right.square).map(right => [left.offset, right.offset])),
        first = meetings.filter(pair => !meetings.some(other => other[0] <= pair[0] && other[1] <= pair[1] &&
          (other[0] < pair[0] || other[1] < pair[1])));
      if (first.length !== 1) return false;
      const arrivals = first[0], prefixes = routes.map((route, n) => route.filter(step => step.offset <= arrivals[n]));
      if (prefixes[0].map(s => s.uci).join(' ') === prefixes[1].map(s => s.uci).join(' ')) return false;
      const exchanged = usedPaths.map((path, n) => {
        const events = path.lineSummary!.material!.events, arrival = arrivals[n],
          gain = events.find(e => e.ply_offset === arrival && e.capture),
          loss = events.find(e => e.ply_offset === arrival + 1 && e.capture?.recapture === 'proven');
        if (!gain?.capture || !loss?.capture || gain.side !== focus.side || gain.capture.role !== focus.role) return;
        const taken = states[n][arrival]?.find(p => p.current.square === gain.capture!.square)?.original,
          removed = states[n][arrival + 1]?.find(p => p.current.square === loss.capture!.square)?.original;
        return taken && removed && samePiece(removed, focus) ? taken : undefined;
      });
      const sameExchange = roots.every(root => root && samePiece(root, focus)) && exchanged.every(Boolean) &&
        samePiece(exchanged[0]!, exchanged[1]!);
      const mover = (n: number, offset: number) => states[n][offset]?.find(p =>
        p.current.square === usedPaths[n].moves[offset]?.uci.slice(0, 2))?.original,
        control = (n: number, count: number, actor: CommentaryInquiryPiece, target: CommentaryInquiryPiece) =>
          continuationSupportAt(observations, usedPaths[n], actor, target, count, states[n][count]).state;
      const notPinned = (n: number, count: number, piece = focus) => {
        const resource = observations.positions?.find(p => p.id === usedPaths[n].positionIds![count]),
          current = states[n][count]?.find(p => samePiece(p.original, piece))?.current;
        return !!current && !!resource?.rays.complete && !resource.rays.lines.some(ray =>
          ray.piece.side !== current.side && ray.occupants[0]?.square === current.square &&
          ray.occupants[1]?.role === 'king' && ray.occupants[1].side === current.side);
      };
      const subjects: CommentaryInquiryPiece[] = [];
      const own = (piece: CommentaryInquiryPiece | undefined) => {
        if (piece && !subjects.some(p => samePiece(p, piece))) subjects.push(piece);
      };
      roots.forEach(own);
      for (const [n, path] of usedPaths.entries()) {
        for (const { offset } of prefixes[n]) {
          const prior = mover(n, offset - 1), reply = mover(n, offset + 1);
          if (prior && prior.side !== focus.side && control(n, offset - 1, prior, focus) === 'absent' &&
            control(n, offset, prior, focus) === 'present' && control(n, offset + 1, prior, focus) === 'absent') own(prior);
          if (reply && reply.side !== focus.side && control(n, offset, focus, reply) === 'absent' &&
            control(n, offset + 1, focus, reply) === 'present' && notPinned(n, offset + 1) &&
            control(n, offset + 2, focus, reply) === 'absent') own(reply);
        }
        const targets = (states[n][0] ?? []).map(p => p.original).filter(p => p.side !== focus.side &&
          control(n, arrivals[n], focus, p) === 'absent' && control(n, arrivals[n] + 1, focus, p) === 'present' &&
          notPinned(n, arrivals[n] + 1));
        if (targets.every(p => p.role === 'pawn')) targets.forEach(own);
        const next = routes[n].find(step => step.offset > arrivals[n]),
          gain = path.lineSummary!.material!.events.find(e => e.capture && e.ply_offset === next?.offset),
          taken = gain && states[n][gain.ply_offset]?.find(p => p.current.square === gain.capture!.square)?.original;
        if (taken && Array.from({ length: gain!.ply_offset - arrivals[n] }, (_, i) => arrivals[n] + i + 1)
          .every(count => control(n, count, focus, taken) === 'present' && notPinned(n, count))) own(taken);
      }
      return usedPaths.every((path, n) => {
        const visible = (offset: number) => mentioned.some(ref => ref.line_id === path.id && ref.occurrence_id === path.occurrenceIds[offset]),
          captures = path.lineSummary!.material!.events.filter(event => event.capture && event.ply_offset <= ends.get(path.id)!),
          together = (...offsets: number[]) => result.units.some(unit => offsets.every(offset => accountRefs(unit).some(ref =>
            ref.line_id === path.id && ref.occurrence_id === path.occurrenceIds[offset]))),
          actor = (offset: number) => mover(n, offset),
          contact = (count: number, actor: CommentaryInquiryPiece, target: CommentaryInquiryPiece) =>
            control(n, count, actor, target);
        const unpinned = (count: number) => notPinned(n, count);
        const interactionsKept = prefixes[n].every(({ offset }) => {
          if (!visible(offset)) return false;
          const prior = actor(offset - 1), reply = actor(offset + 1);
          if (prior && prior.side !== focus.side && contact(offset - 1, prior, focus) === 'absent' &&
            contact(offset, prior, focus) === 'present' && contact(offset + 1, prior, focus) === 'absent' &&
            !together(offset - 1, offset)) return false;
          if (reply && reply.side !== focus.side && contact(offset, focus, reply) === 'absent' &&
            contact(offset + 1, focus, reply) === 'present' && unpinned(offset + 1) &&
            contact(offset + 2, focus, reply) === 'absent' && !together(offset, offset + 1)) return false;
          return true;
        });
        const victim = (event: (typeof captures)[number]) =>
          states[n][event.ply_offset]?.find(p => p.current.square === event.capture!.square)?.original,
          stationary = (piece: CommentaryInquiryPiece, from: number, through: number) => {
            const square = states[n][from]?.find(p => samePiece(p.original, piece))?.current.square;
            return !!square && states[n].slice(from, through + 1).every(state =>
              state?.find(p => samePiece(p.original, piece))?.current.square === square);
          },
          laterLoss = path.lineSummary!.material!.events.find(event => event.capture && event.ply_offset > arrivals[n] &&
            victim(event) && samePiece(victim(event)!, focus) && stationary(focus, arrivals[n] + 1, event.ply_offset));
        if (laterLoss && laterLoss.ply_offset > ends.get(path.id)!) return false;
        const groups: Array<typeof captures> = [];
        for (const event of captures) {
          const taken = victim(event), prior = groups.find(group => {
            const last = group[group.length - 1], mover = actor(last.ply_offset);
            return taken && mover && samePiece(taken, mover) && stationary(mover, last.ply_offset + 1, event.ply_offset);
          });
          if (prior) prior.push(event); else groups.push([event]);
        }
        const independent = (group: typeof captures) => {
          if (group.length !== 2 || group.some(e => visible(e.ply_offset)) ||
            group[1].ply_offset !== group[0].ply_offset + 1 || group[0].side === group[1].side ||
            group[0].capture!.role !== group[1].capture!.role || group[1].capture!.recapture !== 'proven' ||
            usedPaths.some(p => p.positionIds?.some(id => {
              const resource = observations.positions?.find(position => position.id === id);
              return !resource?.controls.complete || !resource.rays.complete;
            }))) return false;
          const pieces = (events: typeof captures) => events.flatMap(e => [actor(e.ply_offset), victim(e)])
            .filter((p): p is CommentaryInquiryPiece => !!p),
            participants = pieces(group),
            owned = subjects.concat(pieces(path.lineSummary!.material!.events.filter(e => e.capture &&
              e.ply_offset >= group[0].ply_offset && !group.includes(e))));
          if (participants.length !== 4 || participants.some(p => owned.some(q => samePiece(p, q)))) return false;
          const squares = new Set(prefixes[n].flatMap(({ offset }) => {
            const from = states[n][offset]?.find(p => samePiece(p.original, focus))?.current,
              to = states[n][offset + 1]?.find(p => samePiece(p.original, focus))?.current.square,
              resource = observations.positions?.find(p => p.id === path.positionIds![offset]);
            if (!from || !to || !resource) return [];
            return [from.square, to, ...resource.rays.lines.filter(ray => ray.piece.square === from.square &&
              ray.piece.side === from.side && ray.piece.role === from.role && ray.squares.includes(to))
              .flatMap(ray => ray.squares.slice(0, ray.squares.indexOf(to)))];
          }));
          for (let count = group[0].ply_offset; count <= group[1].ply_offset + 1; count++) {
            const resource = observations.positions!.find(p => p.id === path.positionIds![count])!,
              current = (piece: CommentaryInquiryPiece) => states[n][count]?.find(p => samePiece(p.original, piece))?.current;
            for (const p of participants) {
              const piece = current(p);
              if (!piece) continue;
              const controls = resource.controls.pieces.find(row => row.piece.square === piece.square &&
                row.piece.side === piece.side && row.piece.role === piece.role);
              if (squares.has(piece.square) || controls?.squares.some(square => squares.has(square)) ||
                owned.some(q => current(q) && (contact(count, p, q) !== 'absent' || contact(count, q, p) !== 'absent'))) return false;
            }
            if (count <= group[1].ply_offset && owned.some(p => owned.some(q => !samePiece(p, q) && current(p) && current(q) &&
              (contact(count, p, q) !== contact(count + 1, p, q) || notPinned(n, count, p) !== notPinned(n, count + 1, p))))) return false;
          }
          return true;
        };
        const chained = groups.every(group => group.length >= 2 &&
          (together(...group.map(event => event.ply_offset)) || independent(group)));
        const next = prefixes[n].find(step => step.offset > 0),
          use = captures.find(event => event.ply_offset === next?.offset && event.capture!.role === 'pawn'),
          target = use && states[n][use.ply_offset]?.find(p => p.current.square === use.capture!.square)?.original,
          balance = new Map<string, number>();
        for (const event of captures) balance.set(event.capture!.role,
          (balance.get(event.capture!.role) ?? 0) + (event.side === focus.side ? 1 : -1));
        const compound = sameExchange && !!use && !!target && contact(0, focus, target) === 'absent' &&
          Array.from({ length: use.ply_offset }, (_, i) => i + 1).every(count =>
            contact(count, focus, target) === 'present' && unpinned(count)) &&
          [...balance.values()].every(n => n === 0) && ends.get(path.id)! <= arrivals[n] + 2 &&
          together(...captures.map(event => event.ply_offset)) && captures.every(event =>
            [arrivals[n], arrivals[n] + 1].includes(event.ply_offset) || event.capture!.role === 'pawn' &&
            (event.ply_offset < arrivals[n] ? [focus, exchanged[n]!].some(p => {
              const mover = actor(event.ply_offset); return mover && samePiece(mover, p);
            }) : event.ply_offset === arrivals[n] + 2 && event.side === focus.side));
        return arrivals[n] <= ends.get(path.id)! && interactionsKept &&
          (arrivals[n] === ends.get(path.id)! || visible(arrivals[n] + 1)) &&
          (chained || compound);
      });
    })());
    if (!validRoute) return;
  }
  if (result.question === 'contested_pair_response') {
    const costs = result.units.filter(unit => unit.role === 'cost');
    if (!costs.length || costs.some(unit => {
      if (unit.covered_moves.some(move => !proofRefs(unit).some(ref => key(ref) === key(move)))) return true;
      return usedPaths.some(path => {
        const moves = unit.covered_moves.filter(ref => ref.line_id === path.id);
        if (!moves.length) return false;
        if (moves.length !== 2) return true;
        const start = path.occurrenceIds.indexOf(moves[0].occurrence_id), end = path.occurrenceIds.indexOf(moves[1].occurrence_id);
        return end !== start + 1 ||
          !path.lineSummary!.material!.events.some(event => event.ply_offset === end && event.capture?.recapture === 'proven');
      });
    })) return;
  }
  if (result.question === 'deferred_pawn_attack') {
    const promotions = usedPaths.flatMap(path => path.lineSummary!.material!.events.filter(event => event.promotion_gain_cp)
      .map(event => ({ path, event })));
    if (promotions.length !== 1) return;
    const { path, event } = promotions[0], at = event.ply_offset,
      removal = path.lineSummary!.material!.events.find(row => row.ply_offset === at + 1),
      actor = trackedPieces(path, at)?.find(piece => piece.current.square === event.from),
      promoted = actor && trackedPieces(path, at + 1)?.find(piece => samePiece(piece.original, actor.original)),
      remaining = trackedPieces(path, at + 2), peer = usedPaths.find(other => other !== path)!,
      peerRoot = trackedPieces(peer, 0)?.find(piece => piece.current.square === peer.moves[0].uci.slice(0, 2));
    const names = (ref: CommentaryQuestionMove, index: number) => ref.line_id === path.id && ref.occurrence_id === path.occurrenceIds[index];
    if (!actor || actor.original.role !== 'pawn' || !promoted || promoted.current.role !== event.after_role ||
      !remaining || remaining.some(piece => samePiece(piece.original, actor.original)) ||
      !peerRoot || !samePiece(peerRoot.original, actor.original) || !event.capture || event.before_role !== 'pawn' ||
      !removal?.capture || removal.capture.role !== event.after_role || removal.capture.recapture !== 'proven' ||
      !result.units.some(unit => [at, at + 1].every(index => accountRefs(unit).some(ref => names(ref, index)) &&
        unit.covered_moves.some(ref => names(ref, index))))) return;
    // Keep the intervening loss and the immediate countercapture visible,
    // even when the caller also supplies a longer continuation.
    const next = path.lineSummary!.material!.events.find(row => row.ply_offset === at + 2 && row.capture);
    const through = next ? at + 2 : at + 1;
    if (path.lineSummary!.material!.events.filter(row => row.capture && row.ply_offset <= through).some(row =>
      !mentioned.some(ref => names(ref, row.ply_offset)) ||
      !result.units.some(unit => unit.covered_moves.some(ref => names(ref, row.ply_offset))))) return;
    const captures = peer.lineSummary!.material!.events.filter(row => row.capture), balance = new Map<string, number>();
    for (const row of captures) balance.set(row.capture!.role, (balance.get(row.capture!.role) ?? 0) + (row.side === 'white' ? 1 : -1));
    if ([...balance.values()].some(count => count !== 0) || captures.some(row => !result.units.some(unit =>
      unit.covered_moves.some(ref => ref.line_id === peer.id && ref.occurrence_id === row.occurrence_id)))) return;
  }
  if (result.question === 'observed_arrangement_convergence') {
    const end = usedPaths.map(path => ends.get(path.id)!),
      boards = usedPaths.map(path => path.moves.map(move => move.fenAfter.split(' ').slice(0, 4).join(' '))),
      meetings = boards[0].flatMap((board, i) => boards[1].flatMap((other, j) => board === other ? [[i, j]] : []));
    const order = (a: number[], b: number[]) => Math.max(...a) - Math.max(...b) || a[0] + a[1] - b[0] - b[1];
    const first = meetings.sort(order)[0];
    if (!first || order(first, end) !== 0 || meetings.filter(pair => order(pair, first) === 0).length !== 1) return;
    if (usedPaths.some((path, n) => !mentioned.some(ref =>
      ref.line_id === path.id && ref.occurrence_id === path.occurrenceIds[end[n]])) ||
      boards[0][end[0]] !== boards[1][end[1]]) return;
  }
  if (result.question === 'observed_move_order') {
    for (const [n, path] of usedPaths.entries()) {
      const peer = usedPaths[1 - n], end = ends.get(path.id)!,
        other = path.moves.findIndex((move, index) => index > 0 && move.uci === peer.moves[0].uci),
        remaining = trackedPieces(path, end + 1);
      if (other < 1 || end !== other + 1 || !remaining || !mentioned.some(ref =>
        ref.line_id === path.id && ref.occurrence_id === path.occurrenceIds[other]) || !mentioned.some(ref =>
        ref.line_id === path.id && path.occurrenceIds.indexOf(ref.occurrence_id) % 2 === 1)) return;
      for (const root of usedPaths) {
        const actor = trackedPieces(root, 1)?.find(piece => piece.original.square === root.moves[0].uci.slice(0, 2));
        const held = actor && remaining.find(piece => samePiece(piece.original, actor.original));
        if (!actor || !held || actor.current.square !== held.current.square || actor.current.role !== held.current.role) return;
      }
    }
  }
  return result;
}

function decodeExchangeSummary(value: unknown, usedPaths: ObservationPath[], evidence: string[]): CommentaryExchangeSummary | undefined {
  if (!object(value) || !exactKeys(value, ['line_id', 'retained_line_id', 'role', 'recapturer_role',
    'exchange_occurrence_ids', 'retention_occurrence_ids']) || !id(value.line_id) || !id(value.retained_line_id) ||
    value.line_id === value.retained_line_id || !['knight', 'bishop', 'rook', 'queen'].includes(String(value.role)) ||
    !ids(value.exchange_occurrence_ids) || !ids(value.retention_occurrence_ids)) return;
  const exchange = usedPaths.find(path => path.id === value.line_id),
    retained = usedPaths.find(path => path.id === value.retained_line_id);
  const neutral = (path: ObservationPath) => {
    const summary = path.lineSummary, material = summary?.material;
    if (!material || summary?.ending.kind !== 'line_end' || path.moves.length < 3 ||
      !summary.premise_ids.every(id => evidence.includes(id)) || material.events.some(event => event.promotion_gain_cp)) return false;
    const captures = material.events.filter(event => event.capture);
    return captures.every((event, index) => index % 2 === 1 || !!captures[index + 1] &&
      captures[index + 1].ply_offset === event.ply_offset + 1 && captures[index + 1].side !== event.side &&
      captures[index + 1].capture!.role === event.capture!.role && captures[index + 1].capture!.recapture === 'proven');
  };
  if (usedPaths.length !== 2 || !exchange || !retained || !neutral(exchange) || !neutral(retained) ||
    exchange.startFen !== retained.startFen || exchange.positionIds?.[0] !== retained.positionIds?.[0] ||
    exchange.moves[0].uci === retained.moves[0].uci ||
    exchange.moves[0].uci.slice(0, 2) !== retained.moves[0].uci.slice(0, 2) || /[+#]$/.test(exchange.moves[2].label) ||
    value.exchange_occurrence_ids.length !== 3 || value.retention_occurrence_ids.length !== 2 ||
    !value.exchange_occurrence_ids.every((id, index) => id === exchange.occurrenceIds[index] && evidence.includes(id)) ||
    value.retention_occurrence_ids[0] !== retained.occurrenceIds[0] ||
    value.retention_occurrence_ids[1] !== retained.occurrenceIds[retained.occurrenceIds.length - 1] ||
    !value.retention_occurrence_ids.every(id => evidence.includes(id))) return;
  const captures = exchange.lineSummary!.material!.events.filter(event => event.capture),
    [loss, recovery] = captures;
  if (loss?.ply_offset !== 1 || recovery?.ply_offset !== 2 || loss.before_role !== value.role ||
    loss.capture!.role !== value.role || recovery.before_role !== value.recapturer_role) return;
  const original = trackedPieces(exchange, 0),
    actor = original?.find(piece => piece.original.square === exchange.moves[0].uci.slice(0, 2))?.original,
    target = original?.find(piece => piece.original.square === exchange.moves[1].uci.slice(0, 2))?.original,
    afterLoss = trackedPieces(exchange, 2), afterRecovery = trackedPieces(exchange, 3),
    remaining = trackedPieces(retained, retained.moves.length);
  if (!actor || !target || actor.role !== value.role || target.role !== value.role || actor.side === target.side ||
    !afterLoss || !afterRecovery || !remaining || afterLoss.some(piece => samePiece(piece.original, actor)) ||
    !afterLoss.some(piece => samePiece(piece.original, target)) ||
    [actor, target].some(witness => afterRecovery.some(piece => samePiece(piece.original, witness)) ||
      !remaining.some(piece => samePiece(piece.original, witness)))) return;
  return value as unknown as CommentaryExchangeSummary;
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
  proposals: CommentaryProposal[],
  rootSearches: CommentaryRootSearch[],
): CommentaryResponseSearch[] | undefined {
  // This is only a parsing envelope. The ordinary five and generic task's
  // 24 slots, plus exact declared family dependencies, are checked below.
  // Each of the at most 600 declarations can license no more than three
  // physical receipts; aliases never create another declaration or slot.
  if (!Array.isArray(value) || value.length > 1829 || !context) return;
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
        ['prefix_moves_uci', 'ply_offset', 'proposal_ownership'],
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
    const ownership = raw.proposal_ownership;
    if (ownership !== undefined && (
      !object(ownership) || !exactKeys(ownership, ['work_id', 'execution_key_sha256', 'question_refs']) ||
      ownership.work_id !== raw.work_id || ownership.execution_key_sha256 !== raw.execution_key_sha256 ||
      !Array.isArray(ownership.question_refs) || !ownership.question_refs.length || ownership.question_refs.length > 600 ||
      !ownership.question_refs.every(ref => object(ref) && exactKeys(ref, ['proposal_id', 'requirement_id']) &&
        proposalId(ref.proposal_id) && proposalId(ref.requirement_id)) ||
      new Set(ownership.question_refs.map(ref => `${ref.proposal_id}/${ref.requirement_id}`)).size !== ownership.question_refs.length
    )) return;
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
  // A shared physical comparison may also own ordinary inverse/alias
  // questions. Its exact functional owner grants this slot; all aliases
  // still pass proposalContainsResponsePool below before selection survives.
  if (!declaredFamilyReceiptCapacityMatches(result, proposals, rootSearches, observations, context)) return;
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
      !ids(raw.pressure_source_ids) ||
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
    const pressureSources = [
      ...position.controls.pieces.flatMap(control => control.piece.side === original.side ? [] :
        control.source_ids.filter((_, index) => control.squares[index] === original.square)),
      ...child.controls.pieces.flatMap(control => control.piece.side === target.side ? [] :
        control.source_ids.filter((_, index) => control.squares[index] === target.square)),
    ];
    const captures = child.legal_moves.moves.filter(move => move.captured_square === target.square);
    const declines = child.legal_moves.moves.filter(move => move.captured_square !== target.square);
    if (
      !sameIds(raw.pressure_source_ids, pressureSources) ||
      (!pressureSources.length && !captures.some(move => move.captured_square !== move.destination))
    )
      return;
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
          if (best?.move_uci !== move) return;
          const claimed = ranked?.find(search => search.work_id === selection.work_id &&
            (search.root_restriction.kind === 'unrestricted' || family.every(move =>
              search.root_restriction.kind === 'restricted' && search.root_restriction.moves_uci.includes(move))));
          const originalChoice = claimed?.candidates.find(candidate => family.includes(candidate.move_uci));
          // Retain the exact choice origin when a newer owned receipt still
          // selects the same class member. The answer owns its separate scores.
          if ((search?.work_id !== selection.work_id || best.line_id !== selection.line_id) &&
            (originalChoice?.move_uci !== move || originalChoice.line_id !== selection.line_id)) return;
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
        ['assessment_effect', 'ordering_candidate_ids', 'explanation_candidate_ids'],
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
          reason.actor.square === String(reason.anchor_move).slice(0, 2) || !anchorActor)
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
    const extras = ['ordering_candidate_ids', 'explanation_candidate_ids'];
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
      ['continuation_candidate_ids', 'continuation_comparison_ids', 'effects'],
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
      qualityBand(answer.reference_evaluation, answer.alternative_evaluation, root.turn)
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
  }
  if (
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

function isPresentContinuationSupport(value: unknown): value is ContinuationSupport & { state: 'present' } {
  return (
    object(value) &&
    exactKeys(value, ['state', 'source_ids']) &&
    value.state === 'present' &&
    ids(value.source_ids)
  );
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
        !isPresentContinuationSupport(value.support)
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
      !isPresentContinuationSupport(value.support)
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

function qualifiedPositionFactSources(position: PositionResources): string[] {
  const suffix = `:evidence:position-occurrence:${position.id}`;
  if (!position.premise_id.endsWith(suffix)) return [];
  const namespace = position.premise_id.slice(0, -suffix.length);
  const rows: [string, string[]][] = [
    ['geometric_control', position.controls.pieces.flatMap(p => p.source_ids)],
    ['ray_barrier', position.rays.lines.map(p => p.source_id)],
    ['legal_move', position.legal_moves.moves.map(p => p.source_id)],
    ['pawn_file_group', position.pawn_files.files.flatMap(p => p.source_ids)],
    ['pawn_advance_affordance', position.pawn_advances.paths.map(p => p.source_id)],
  ];
  // Pawn-state rows mix several fact kinds and cannot license a typed alias.
  return rows.flatMap(([kind, refs]) => refs.filter(ref => /^[a-f0-9]{64}$/.test(ref))
    .map(ref => `${namespace}:evidence:relation:position:${position.id}:${kind}:${ref}`));
}

function factQualifiedPositionIds(
  sample: CommentaryFactSample, predicate: CommentaryFactPredicate,
  path: ObservationPath, observations: MoveReviewObservations,
): Set<string> {
  const positions = new Set(path.positionIds?.slice(sample.from_ply, sample.through_ply + 1)
    .filter((id): id is string => id !== null));
  if (predicate.kind !== 'direct_support_use' || sample.value !== 'true' || !path.lineSummary?.material)
    return positions;
  const originId = path.positionIds?.[0];
  if (!originId || !predicate.subject || !predicate.target) return new Set();
  const providerOrigin = { ...predicate.subject, position_id: originId },
    targetOrigin = { ...predicate.target, position_id: originId };
  const snapshots = new Map<number, CommentaryTrackedPiece[] | undefined>();
  const at = (count: number) => {
    if (!snapshots.has(count)) snapshots.set(count, trackedPieces(path, count));
    return snapshots.get(count);
  };
  const current = (original: CommentaryInquiryPiece, count: number) =>
    at(count)?.find(piece => samePiece(piece.original, original))?.current;
  const captures = path.lineSummary.material.events.filter(event =>
    event.capture && event.ply_offset < sample.through_ply);
  for (const loss of captures) {
    const target = current(targetOrigin, loss.ply_offset),
      provider = current(providerOrigin, loss.ply_offset),
      capturer = at(loss.ply_offset)?.find(piece => piece.current.square === loss.from)?.original;
    if (!target || !provider || !capturer || loss.capture!.square !== target.square) continue;
    const takenBy = current(capturer, loss.ply_offset + 1);
    const lossPosition = observations.positions?.find(position => position.id === path.positionIds![loss.ply_offset]);
    if (!takenBy || takenBy.square !== target.square || !lossPosition?.controls.complete ||
        !lossPosition.legal_moves.complete || continuationSupportAt(observations, path, providerOrigin,
          targetOrigin, loss.ply_offset, at(loss.ply_offset)).state !== 'present') continue;
    for (const recovery of captures.filter(event => event.ply_offset > loss.ply_offset)) {
      const lossId = path.positionIds?.[loss.ply_offset], recoveryId = path.positionIds?.[recovery.ply_offset];
      if (!lossId || !recoveryId) continue;
      const actor = at(recovery.ply_offset)?.find(piece => piece.current.square === recovery.from)?.original,
        victim = at(recovery.ply_offset)?.find(piece => piece.current.square === recovery.capture!.square)?.original;
      if (!actor || !victim || !samePiece(actor, providerOrigin) || !samePiece(victim, capturer) ||
          !observations.positions?.find(position => position.id === path.positionIds![recovery.ply_offset])?.legal_moves.complete ||
          !Array.from({ length: recovery.ply_offset - loss.ply_offset + 1 }, (_, index) => loss.ply_offset + index)
            .every(count => { const piece = current(providerOrigin, count), id = path.positionIds?.[count]; return piece && id &&
              samePiece(piece, { ...provider, position_id: id }); }) ||
          !Array.from({ length: recovery.ply_offset - loss.ply_offset }, (_, index) => loss.ply_offset + index + 1)
            .every(count => { const piece = current(capturer, count), id = path.positionIds?.[count]; return piece && id &&
              samePiece(piece, { ...takenBy, position_id: id }); })) continue;
      // A compound use owns its observed support/loss and recapture edges, not the whole earlier history.
      positions.add(lossId);
      positions.add(recoveryId);
    }
  }
  return positions;
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
    const qualifiedPositions = factQualifiedPositionIds(sample as unknown as CommentaryFactSample,
      owner.predicate, path, observations);
    for (const position of resources.filter(position => qualifiedPositions.has(position.id)))
      for (const ref of qualifiedPositionFactSources(position)) sources.add(ref);
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
    value.source_refs.every(ref => boundedText(ref, 512));
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

export function reserveResponsePoolMatches(
  proposal: CommentaryProposal,
  reading: CommentaryProposalReading,
  pool: string[],
  observations: MoveReviewObservations,
  positionId: string,
): boolean {
  if (proposal.admission !== 'generated' || reading.kind !== 'compare_moves') return false;
  const hold = proposal.origin.question.producer === 'C-reserve-hold' && reading.id === 'reserve_hold';
  const counter = proposal.origin.question.producer === 'C-reserve-entry' && reading.id === 'reserve_counter';
  if (!hold && !counter) return false;
  const position = observations.positions?.find(position => position.id === positionId);
  if (!position?.legal_moves.complete) return false;
  const side = parseFen(position.fen).unwrap().turn;
  const pieces = position.controls.pieces.map(control => control.piece);
  const king = pieces.find(piece => piece.side === side && piece.role === 'king');
  const pawn = pieces.find(piece => piece.side === side && piece.role === 'pawn' &&
    piece.square === reading.alternative_move.slice(0, 2));
  if (!king || !pawn || reading.reference_move.slice(0, 2) !== king.square ||
    ![reading.reference_move, reading.alternative_move].every(uci =>
      position.legal_moves.moves.some(move => move.uci === uci))) return false;
  const members = new Set<string>(pieces.filter(piece => piece.side === side && piece.role === (hold ? 'king' : 'pawn'))
    .map(piece => piece.square));
  const expected = new Set([hold ? reading.alternative_move : reading.reference_move, ...position.legal_moves.moves
    .filter(move => members.has(move.uci.slice(0, 2))).map(move => move.uci)]);
  return expected.size === pool.length && pool.every(move => expected.has(move));
}

/** Issuing and retaining a functional survey use the same task contract.
 * A retained survey may still belong to its original seed pair after its
 * results have selected a different competitor; the receipt is not an answer
 * to that seed pair. Position/history ownership is checked by the caller. */
export function functionalResponseSurveyMatches(
  proposal: CommentaryProposal,
  reading: CommentaryProposalReading,
  prefix: string[],
  rootOccurrenceId: string,
): boolean {
  if (proposal.admission !== 'generated' || proposal.origin.question.kind !== 'generator' ||
      proposal.origin.question.producer !== 'C-reply-function' ||
      (prefix.length !== 2 && prefix.length !== 3) || reading.kind !== 'compare_moves' ||
      !reading.id.startsWith('function_value_') ||
      !factPrefix(prefix, reading.prefix_moves_uci) || reading.prefix_moves_uci.length !== prefix.length)
    return false;
  const members = ['function_before', 'function_root_reference', 'function_root_alternative',
    'function_owned', 'function_changed', 'function_live', 'function_resource'];
  const task = proposal.requirements.find(row => row.id === 'function_task_context');
  const root = proposal.requirements.find(row => row.id === 'function_root');
  const resource = proposal.requirements.find(row => row.id === 'function_resource');
  if (task?.kind !== 'fact_group' || task.mode !== 'all' || task.state !== 'supported' ||
      task.members.length !== members.length || !task.members.every((id, index) => id === members[index]) ||
      !members.every(id => proposal.requirements.some(row => row.id === id && row.state === 'supported')) ||
      root?.kind !== 'compare_moves' || root.state !== 'answered' || !root.answer ||
      root.prefix_moves_uci.length !== 0 ||
      ![root.reference_move, root.alternative_move].includes(prefix[0]) ||
      (prefix[0] === root.reference_move ? root.answer.reference_occurrence_id :
        root.answer.alternative_occurrence_id) !== rootOccurrenceId ||
      resource?.kind !== 'fact' || resource.predicate.kind !== 'legal_move' ||
      !resource.predicate.subject || !resource.predicate.square) return false;
  const actor = resource.predicate.subject, square = resource.predicate.square;
  return proposal.requirements.some(row => row.kind === 'fact' && row.state === 'supported' &&
    row.at.kind === 'ply' && row.at.ply_count === prefix.length && row.path.source.kind === 'declared' &&
    row.path.source.moves_uci.length === prefix.length && factPrefix(prefix, row.path.source.moves_uci) &&
    row.predicate.subject?.side === actor.side && row.predicate.subject?.role === actor.role &&
    row.predicate.subject?.square === actor.square &&
    (prefix.length === 2 ? row.id.startsWith('function_value_legal:') &&
      row.predicate.kind === 'legal_move' && row.predicate.square === square && row.expected === 'legal' :
      row.id.startsWith('function_value_present:') && row.predicate.kind === 'presence' && row.expected === 'present'));
}

/** The reserve source owns an exact deferred decision. Its current pawn,
 * legal action and all context facts must survive before a full survey can
 * be issued or retained. A later arbitrary generated pair grants no slot. */
export function anchoredResponseSurveyMatches(
  proposal: CommentaryProposal,
  reading: CommentaryProposalReading,
  prefix: string[],
  rootOccurrenceId: string,
): boolean {
  const family = proposal.origin.question.producer === 'C-reserve-use';
  if (proposal.admission !== 'generated' || proposal.origin.question.kind !== 'generator' ||
      (!family && proposal.origin.question.producer !== 'C-reserve-timing') || reading.kind !== 'compare_moves' ||
      !reading.id.startsWith('anchored_value_') || prefix.length < (family ? 1 : 2) ||
      prefix.length !== reading.prefix_moves_uci.length || !factPrefix(prefix, reading.prefix_moves_uci)) return false;
  const root = proposal.requirements.find(row => row.id === 'reserve_root');
  const timing = proposal.requirements.find(row => row.id === 'reserve_timing');
  const context = proposal.requirements.find(row => row.id === (family ? 'reserve_context' : 'timing_context'));
  const before = proposal.requirements.find(row => row.id === 'reserve_before');
  const rootReply = family && prefix.length === 1 &&
    ['anchored_value_response:', 'anchored_value_reply:', 'anchored_value_capture:']
      .some(role => reading.id.startsWith(role));
  const expected = family ? ['reserve_before', 'reserve_after', 'reserve_kept'] :
    ['reserve_before', 'reserve_after', 'reserve_kept', 'timing_available',
      'timing_kept', 'timing_current', 'timing_common_reply'];
  if (root?.kind !== 'compare_moves' || root.state !== 'answered' || !root.answer ||
      root.prefix_moves_uci.length || root.answer.evaluation_relation !== 'better' ||
      root.answer.reference_depth === null || root.answer.reference_depth !== root.answer.alternative_depth ||
      root.answer.reference_depth < root.answer.required_depth ||
      (rootReply ? ![root.reference_move, root.alternative_move].includes(prefix[0]) :
        prefix[0] !== root.alternative_move) ||
      (rootReply && prefix[0] === root.reference_move ? root.answer.reference_occurrence_id :
        root.answer.alternative_occurrence_id) !== rootOccurrenceId ||
      before?.kind !== 'fact' || before.predicate.subject?.role !== 'pawn' ||
      before.predicate.subject.square !== root.reference_move.slice(0, 2) ||
      qualityBand(root.answer.reference_evaluation, root.answer.alternative_evaluation,
        before.predicate.subject.side) !== 'separated' ||
      (!family && (timing?.kind !== 'compare_moves' || timing.prefix_moves_uci.length !== 2 ||
        timing.prefix_moves_uci[0] !== root.alternative_move || timing.reference_move !== root.reference_move)) ||
      context?.kind !== 'fact_group' || context.mode !== 'all' || context.state !== 'supported' ||
      !sameIds(context.members, expected) ||
      !expected.every(id => proposal.requirements.some(row => row.id === id && row.state === 'supported')))
    return false;
  const samePawn = (a: CommentaryFactPiece, b: CommentaryFactPiece | undefined) =>
    !!b && a.side === b.side && a.role === b.role && a.square === b.square;
  const own = family ? reading.id.startsWith('anchored_value_own:') && prefix.length % 2 === 0 :
    timing?.kind === 'compare_moves' && prefix.length === timing.prefix_moves_uci.length &&
      factPrefix(prefix, timing.prefix_moves_uci);
  const prior = family ? prefix.slice(0, -1) : timing?.kind === 'compare_moves' ? timing.prefix_moves_uci : [];
  const response = rootReply || (prefix.length === prior.length + 1 && prefix.length % 2 === 1 &&
    factPrefix(prior, prefix) && proposal.requirements.some(row =>
      row.kind === 'compare_moves' && row.id.startsWith('anchored_value_own:') && row.state === 'answered' &&
      !!row.answer && row.prefix_moves_uci.length === prior.length &&
      factPrefix(prior, row.prefix_moves_uci) &&
      [row.reference_move, row.alternative_move].includes(prefix[prefix.length - 1])));
  if (!own && !response) return false;
  const beforeSubject = before.predicate.subject;
  return proposal.requirements.some(group => {
    if (group.kind !== 'fact_group' || !group.id.startsWith('anchored_value_context:') ||
        group.mode !== 'all' || group.state !== 'supported' || group.members.length !== 3) return false;
    const facts = group.members.map(id => proposal.requirements.find(row => row.id === id));
    if (!facts.every(row => row?.kind === 'fact' && row.state === 'supported' && row.at.kind === 'ply' &&
        row.at.ply_count === prefix.length && row.path.source.kind === 'declared' &&
        row.path.source.moves_uci.length === prefix.length && factPrefix(prefix, row.path.source.moves_uci)))
      return false;
    const presence = facts.find(row => row?.kind === 'fact' && row.id.startsWith('anchored_value_present:'));
    const location = facts.find(row => row?.kind === 'fact' && row.id.startsWith('anchored_value_location:'));
    const legal = facts.find(row => row?.kind === 'fact' && row.id.startsWith('anchored_value_legal:'));
    if (presence?.kind !== 'fact' || presence.predicate.kind !== 'presence' || presence.expected !== 'present' ||
        presence.predicate.subject?.role !== 'pawn' ||
        presence.predicate.subject.square !== root.reference_move.slice(0, 2) ||
        !samePawn(presence.predicate.subject, before.predicate.subject) ||
        location?.kind !== 'fact' || location.predicate.kind !== 'location' || !location.expected ||
        !samePawn(presence.predicate.subject, location.predicate.subject) ||
        legal?.kind !== 'fact' || legal.predicate.kind !== 'legal_move' || legal.expected !== 'legal' ||
        legal.predicate.square !== reading.reference_move.slice(2, 4) || !legal.predicate.subject) return false;
    if (rootReply && (location.expected !== (prefix[0] === root.reference_move ?
        root.reference_move.slice(2, 4) : beforeSubject.square) ||
        legal.predicate.subject.side === beforeSubject.side ||
        legal.predicate.subject.square !== reading.reference_move.slice(0, 2))) return false;
    return !own || (samePawn(legal.predicate.subject, presence.predicate.subject) &&
      reading.reference_move === root.reference_move && location.expected === reading.reference_move.slice(0, 2));
  });
}

export function ownedChoiceSurveyMatches(proposal: CommentaryProposal, reading: CommentaryProposalReading,
  prefix: string[], rootOccurrenceId: string): boolean {
  return functionalResponseSurveyMatches(proposal, reading, prefix, rootOccurrenceId) ||
    anchoredResponseSurveyMatches(proposal, reading, prefix, rootOccurrenceId);
}

// A verified divergent child remains a dependency of its two owned parent
// measurements. This grants a ledger slot, never an earlier numerical value.
function ownedChoiceConflictReceiptMatches(
  child: CommentaryResponseSearch,
  searches: CommentaryResponseSearch[],
  proposals: CommentaryProposal[],
  observations: MoveReviewObservations,
  accept?: (proposal: CommentaryProposal, reading: CommentaryProposalReading,
    pool: CommentaryResponseSearch, pair: CommentaryResponseSearch) => boolean,
): boolean {
  const completedDepth = (search: CommentaryResponseSearch): number | undefined => {
    const depths = search.candidates.map(candidate => candidate.evaluation.kind === 'engine_search'
      ? candidate.evaluation.depth : undefined);
    const first = depths[0];
    return typeof first === 'number' && depths.every(depth => depth === first) ? first : undefined;
  };
  return child.proposal_ownership?.question_refs.some(ref => {
    const proposal = proposals.find(row => row.id === ref.proposal_id);
    const reading = proposal?.requirements.find(row => row.id === ref.requirement_id);
    if (!proposal || proposal.admission !== 'generated' ||
      proposal.origin.question.kind !== 'generator' ||
      proposal.origin.question.producer !== 'C-comparison-conflict' ||
      reading?.kind !== 'compare_moves' || reading.id !== 'resolve_comparison_conflict' ||
      !['answered', 'supported', 'refuted'].includes(reading.state) ||
      reading.work_id !== child.work_id || !reading.answer ||
      reading.answer.work_id !== child.work_id ||
      reading.answer.execution_key_sha256 !== child.execution_key_sha256 ||
      child.root_restriction.kind !== 'restricted' ||
      !sameIds(child.root_restriction.moves_uci, [reading.reference_move, reading.alternative_move]) ||
      !factPrefix(reading.prefix_moves_uci, responseSearchPrefix(child)) ||
      reading.prefix_moves_uci.length !== responseSearchPrefix(child).length) return false;
    const refs = proposal.origin.question.source_refs;
    if (refs.length !== 2 || new Set(refs).size !== 2 ||
      ![proposal.origin.candidates, proposal.origin.evidence].every(source =>
        source.kind === 'generator' && source.producer === 'C-comparison-conflict' &&
        source.source_refs.length === refs.length && source.source_refs.every((id, index) => id === refs[index])))
      return false;
    const sources = refs.map(id => searches.find(search => search.work_id === id));
    if (!sources.every((search): search is CommentaryResponseSearch => !!search)) return false;
    if (sources.some(source => Number(source.work_id.slice(5)) >= Number(child.work_id.slice(5)))) return false;
    const pool = sources.find(search => search.root_restriction.kind === 'unrestricted');
    const pair = sources.find(search => search.root_restriction.kind === 'restricted');
    if (!pool || !pair || pair.root_restriction.kind !== 'restricted' ||
      pair.root_restriction.moves_uci.length !== 2 ||
      !pair.root_restriction.moves_uci.includes(pool.candidates[0].move_uci)) return false;
    const pairMoves = pair.root_restriction.moves_uci;
    const prefix = responseSearchPrefix(pool), level = completedDepth(pool);
    if (level === undefined || child.required_depth < level ||
      sources.some(search => search.engine_profile !== child.engine_profile ||
        search.root_move !== child.root_move || search.root_move_occurrence_id !== child.root_move_occurrence_id ||
        search.child_position_id !== pool.child_position_id || completedDepth(search) !== level ||
        responseSearchPrefix(search).length !== prefix.length || !factPrefix(prefix, responseSearchPrefix(search))))
      return false;
    const parentOwned = pair.proposal_ownership?.question_refs.some(parentRef => {
      const parent = proposals.find(row => row.id === parentRef.proposal_id);
      const question = parent?.requirements.find(row => row.id === parentRef.requirement_id);
      return !!parent && question?.kind === 'compare_moves' &&
        sameIds(pairMoves, [question.reference_move, question.alternative_move]) &&
        ownedChoiceSurveyMatches(parent, question, prefix, pool.root_move_occurrence_id) &&
        pool.proposal_ownership?.question_refs.some(poolRef => {
          const seed = parent.requirements.find(row => row.id === poolRef.requirement_id);
          return poolRef.proposal_id === parent.id && !!seed &&
            ownedChoiceSurveyMatches(parent, seed, prefix, pool.root_move_occurrence_id);
        });
    });
    if (!parentOwned) return false;
    const divergent = sources[0].candidates.some(first => {
      const second = sources[1].candidates.find(candidate => candidate.move_uci === first.move_uci);
      if (!second || !pairMoves.includes(first.move_uci) ||
        qualityBand(first.evaluation, second.evaluation, 'white') !== 'separated') return false;
      const left = observations.paths.find(path => path.id === first.line_id);
      const right = observations.paths.find(path => path.id === second.line_id);
      if (!left || !right) return false;
      let shared = 0;
      while (shared < Math.min(left.moves.length, right.moves.length) &&
        left.moves[shared].uci === right.moves[shared].uci) shared++;
      return shared > prefix.length && shared === reading.prefix_moves_uci.length &&
        factPrefix(reading.prefix_moves_uci, left.moves.map(move => move.uci)) &&
        left.occurrenceIds.slice(0, shared).every((id, index) => id === right.occurrenceIds[index]) &&
        left.positionIds?.[shared] === child.child_position_id &&
        right.positionIds?.[shared] === child.child_position_id &&
        left.moves[shared]?.uci === reading.reference_move &&
        right.moves[shared]?.uci === reading.alternative_move;
    });
    return divergent && (!accept || accept(proposal, reading, pool, pair));
  }) ?? false;
}

/** Extra capacity is owned by an exact reserve family, not by its name or
 * the number of aliases. Keep every measurement: a verified divergent
 * child retains its two historical parents instead of rewriting their
 * values or pretending they answer the current comparison. */
function declaredFamilyReceiptCapacityMatches(
  searches: CommentaryResponseSearch[],
  proposals: CommentaryProposal[],
  roots: CommentaryRootSearch[],
  observations: MoveReviewObservations,
  context: CommentaryInquiryContext,
): boolean {
  const familyKeys = new Map<CommentaryProposal, string | undefined>();
  const familyKey = (proposal: CommentaryProposal): string | undefined => {
    if (familyKeys.has(proposal)) return familyKeys.get(proposal);
    const root = proposal.requirements.find(row => row.id === 'reserve_root');
    const before = proposal.requirements.find(row => row.id === 'reserve_before');
    const answer = root?.kind === 'compare_moves' ? root.answer : undefined;
    const receipt = answer && roots.find(row => row.work_id === answer.work_id &&
      row.execution_key_sha256 === answer.execution_key_sha256 && row.state === 'completed' &&
      row.engine_profile === answer.engine_profile && row.required_depth === answer.required_depth);
    const reference = answer && observations.paths.find(path => path.id === answer.reference_line_id);
    const alternative = answer && observations.paths.find(path => path.id === answer.alternative_line_id);
    let key: string | undefined;
    if (proposal.admission === 'generated' && proposal.origin.question.kind === 'generator' &&
      proposal.origin.question.producer === 'C-reserve-use' && root?.kind === 'compare_moves' &&
      root.state === 'answered' && !root.prefix_moves_uci.length && answer && receipt &&
      before?.kind === 'fact' && before.state === 'supported' && before.at.kind === 'ply' &&
      before.at.ply_count === 0 && before.path.source.kind === 'declared' &&
      sameIds(before.path.source.moves_uci, [root.reference_move]) &&
      before.predicate.kind === 'location' && before.predicate.subject?.role === 'pawn' &&
      before.expected === before.predicate.subject.square &&
      reference && alternative && reference.startFen === context.rootFen && alternative.startFen === context.rootFen &&
      reference.positionIds?.[0] === receipt.root_position_id &&
      alternative.positionIds?.[0] === receipt.root_position_id &&
      reference.moves[0]?.uci === root.reference_move && alternative.moves[0]?.uci === root.alternative_move &&
      reference.occurrenceIds[0] === answer.reference_occurrence_id &&
      alternative.occurrenceIds[0] === answer.alternative_occurrence_id &&
      [reference, alternative].every(path => receipt.candidates.some(candidate =>
        candidate.line_id === path.id && candidate.occurrence_id === path.occurrenceIds[0] &&
        candidate.move_uci === path.moves[0]?.uci))) {
      key = JSON.stringify([receipt.work_id, receipt.execution_key_sha256, receipt.root_position_id,
        root.reference_move, root.alternative_move, answer.reference_occurrence_id,
        answer.alternative_occurrence_id, before.predicate.subject.side,
        before.predicate.subject.role, before.predicate.subject.square]);
    }
    familyKeys.set(proposal, key);
    return key;
  };
  const owners = new Map<CommentaryResponseSearch, { family: string; question: string }[]>();
  for (const search of searches) {
    const prefix = responseSearchPrefix(search);
    const path = commentarySearchAnchorPath(observations, context.rootFen, search.root_move,
      search.root_move_occurrence_id, search.child_position_id, prefix);
    if (!path) return false;
    const options: { family: string; question: string }[] = [];
    for (const ref of search.proposal_ownership?.question_refs ?? []) {
      const proposal = proposals.find(row => row.id === ref.proposal_id);
      const reading = proposal?.requirements.find(row => row.id === ref.requirement_id);
      if (proposal?.origin.question.producer !== 'C-reserve-use' || reading?.kind !== 'compare_moves') continue;
      const family = familyKey(proposal);
      if (!family || !anchoredResponseSurveyMatches(proposal, reading, prefix,
        search.root_move_occurrence_id)) return false;
      // Exact occurrences and positions distinguish a repeated FEN reached
      // through another history. Inverse aliases share the same physical pair.
      options.push({ family, question: JSON.stringify([family, prefix,
        path.occurrenceIds.slice(0, prefix.length), path.positionIds?.slice(0, prefix.length + 1),
        [reading.reference_move, reading.alternative_move].sort()]) });
    }
    owners.set(search, options);
  }
  const dependencies = new Set<CommentaryResponseSearch>();
  const conflictSlots = new Set<string>();
  for (const child of searches) {
    const valid = ownedChoiceConflictReceiptMatches(child, searches, proposals, observations,
      (_proposal, reading, pool, pair) => {
        if (reading.kind !== 'compare_moves') return false;
        const common = owners.get(pair)?.find(owner => owners.get(pool)?.some(row => row.family === owner.family));
        if (!common) return false;
        const path = commentarySearchAnchorPath(observations, context.rootFen, child.root_move,
          child.root_move_occurrence_id, child.child_position_id, responseSearchPrefix(child));
        if (!path) return false;
        const slot = JSON.stringify([common.family,
          [pool, pair].map(row => [row.work_id, row.execution_key_sha256]), responseSearchPrefix(child),
          path.occurrenceIds.slice(0, responseSearchPrefix(child).length),
          path.positionIds?.slice(0, responseSearchPrefix(child).length + 1),
          [reading.reference_move, reading.alternative_move].sort()]);
        if (conflictSlots.has(slot)) return false;
        conflictSlots.add(slot);
        dependencies.add(pool); dependencies.add(pair); dependencies.add(child);
        return true;
      });
    if (!valid && child.proposal_ownership?.question_refs.some(ref => {
      const proposal = proposals.find(row => row.id === ref.proposal_id);
      return proposal?.origin.question.producer === 'C-comparison-conflict' &&
        proposal.origin.question.source_refs.some(id => {
          const source = searches.find(row => row.work_id === id);
          return !!source && !!owners.get(source)?.length;
        });
    })) return false;
  }
  const normal = searches.filter(search => !dependencies.has(search) && owners.get(search)!.length);
  // Match physical rows to declaration slots. This avoids an alias-order
  // preference while still rejecting a second survey/pair of the same question.
  const assigned = new Map<string, CommentaryResponseSearch>();
  const assign = (search: CommentaryResponseSearch, seen: Set<string>): boolean => {
    for (const owner of owners.get(search)!) {
      const slot = `${owner.question}/${search.root_restriction.kind}`;
      if (seen.has(slot)) continue;
      seen.add(slot);
      const previous = assigned.get(slot);
      if (!previous || assign(previous, seen)) { assigned.set(slot, search); return true; }
    }
    return false;
  };
  if (!normal.every(search => assign(search, new Set()))) return false;
  const other = searches.filter(search => !dependencies.has(search) && !owners.get(search)!.length);
  const generic = other.filter(search => search.proposal_ownership?.question_refs.some(ref => {
    const proposal = proposals.find(row => row.id === ref.proposal_id);
    const reading = proposal?.requirements.find(row => row.id === ref.requirement_id);
    return !!proposal && !!reading && ownedChoiceSurveyMatches(proposal, reading,
      responseSearchPrefix(search), search.root_move_occurrence_id);
  }) || ownedChoiceConflictReceiptMatches(search, searches, proposals, observations));
  return generic.length <= 24 && other.length - generic.length <= 5 &&
    (normal.length > 0 || dependencies.size > 0 || searches.length <= 29);
}

export function proposalContainsResponsePool(
  proposals: CommentaryProposal[],
  search: CommentaryResponseSearch,
  observations?: MoveReviewObservations,
): boolean {
  if (search.root_restriction.kind === 'unrestricted') {
    const ownership = search.proposal_ownership, prefix = responseSearchPrefix(search);
    const position = observations?.positions?.find(position => position.id === search.child_position_id);
    const choiceOwner = ownership?.question_refs.some(ref => {
      const proposal = proposals.find(proposal => proposal.id === ref.proposal_id);
      const reading = proposal?.requirements.find(row => row.id === ref.requirement_id);
      return !!proposal && !!reading && ownedChoiceSurveyMatches(proposal, reading, prefix,
        search.root_move_occurrence_id);
    });
    // A first-reply survey belongs to the issued questions even if its top
    // results omit one named move. That omission cannot answer the pair.
    return !!ownership && ownership.work_id === search.work_id &&
      ownership.execution_key_sha256 === search.execution_key_sha256 &&
      !!position?.legal_moves.complete && position.legal_moves.moves.length >= 2 &&
      search.multi_pv === Math.min(3, position.legal_moves.moves.length) &&
      ownership.question_refs.length > 0 &&
      new Set(ownership.question_refs.map(ref => `${ref.proposal_id}/${ref.requirement_id}`)).size ===
        ownership.question_refs.length &&
      ownership.question_refs.every(ref => {
        const proposal = proposals.find(proposal => proposal.id === ref.proposal_id);
        const reading = proposal?.requirements.find(reading => reading.id === ref.requirement_id);
        return proposal?.admission === 'generated' && reading?.kind === 'compare_moves' &&
          reading.prefix_moves_uci.length === prefix.length && factPrefix(prefix, reading.prefix_moves_uci) &&
          (prefix.length === 1 || choiceOwner) &&
          reading.reference_move !== reading.alternative_move &&
          [reading.reference_move, reading.alternative_move].every(move =>
            position.legal_moves.moves.some(row => row.uci === move));
      });
  }
  if (search.root_restriction.kind !== 'restricted') return false;
  const pool = search.root_restriction.moves_uci;
  const prefix = responseSearchPrefix(search);
  if (search.proposal_ownership !== undefined) {
    // Issued owners certify this retained work, not the sufficiency of today's answers.
    const ownership = search.proposal_ownership;
    const issued = ownership.question_refs.flatMap(ref => {
      const reading = proposals.find(proposal => proposal.id === ref.proposal_id)?.requirements
        .find(reading => reading.id === ref.requirement_id);
      return reading?.kind === 'compare_moves' ? [reading] : [];
    });
    return ownership.work_id === search.work_id && ownership.execution_key_sha256 === search.execution_key_sha256 &&
      issued.length > 0 && issued.length === ownership.question_refs.length &&
      new Set(ownership.question_refs.map(ref => `${ref.proposal_id}/${ref.requirement_id}`)).size === issued.length &&
      issued.every(reading => reading.prefix_moves_uci.length === prefix.length &&
        reading.prefix_moves_uci.every((move, index) => move === prefix[index]) &&
        reading.reference_move === issued[0].reference_move) &&
      ((pool.length >= 2 && pool.length <= 3 &&
        sameIds(pool, [...new Set(issued.flatMap(reading => [reading.reference_move, reading.alternative_move]))])) ||
        !!observations && ownership.question_refs.every(ref => {
          const proposal = proposals.find(proposal => proposal.id === ref.proposal_id)!;
          const reading = proposal.requirements.find(reading => reading.id === ref.requirement_id)!;
          return reserveResponsePoolMatches(proposal, reading, pool, observations, search.child_position_id);
        }));
  }
  const pairs = proposals.flatMap(proposal =>
    proposal.requirements.filter(
      (reading): reading is CommentaryProposalReading & { kind: 'compare_moves' } =>
        reading.kind === 'compare_moves' &&
        reading.prefix_moves_uci.length === prefix.length &&
        reading.prefix_moves_uci.every((move, index) => move === prefix[index]),
    ),
  );
  const owned = pairs.filter(reading => reading.work_id === search.work_id);
  if (owned.length) {
    const moves = [...new Set(owned.flatMap(reading => [reading.reference_move, reading.alternative_move]))];
    return (
      owned.every(reading =>
        ['answered', 'supported', 'refuted'].includes(reading.state) &&
        reading.stop_reason === null && reading.answer?.work_id === search.work_id) &&
      moves.length >= 2 && moves.length <= 3 && sameIds(pool, moves)
    );
  }
  // A question can reopen at a deeper depth. Its old, independently validated
  // search still belongs to this exact pair, but is not its current answer.
  return pairs.some(reading => sameIds(pool, [reading.reference_move, reading.alternative_move]));
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
        'purpose_question',
        'preparation_question',
        'root_searches',
        'continuation_inquiries',
        'proposals',
        'local_expansions_used',
        'discovery',
      ],
    ) ||
    value.version !== 2 ||
    !Array.isArray(value.candidates) ||
    value.candidates.length > 10000 ||
    !Array.isArray(value.scenes) ||
    value.scenes.length > 10000
  )
    return;
  const paths = new Map(observations.paths.map(p => [p.id, p]));
  const purposeQuestion = value.purpose_question === undefined ? undefined :
    decodePurposeQuestion(value.purpose_question, observations, context?.playedMove);
  if (value.purpose_question !== undefined && !purposeQuestion) return;
  const preparationQuestion = value.preparation_question === undefined ? undefined :
    decodePreparationQuestion(value.preparation_question, observations, context?.playedMove);
  if (value.preparation_question !== undefined && !preparationQuestion) return;
  const occurrences = new Set(observations.paths.flatMap(p => p.occurrenceIds).filter(id));
  const positions = new Map(observations.positions?.map(p => [p.id, p] as const));
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
      responseSearches = decodeResponseSearches(value.response_searches, observations, context, proposals ?? [], rootSearches ?? []);
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
    responseSearches?.some(search => search.proposal_ownership !== undefined &&
      !proposalContainsResponsePool(proposals ?? [], search, observations)) ||
    responseSearches?.some(
      search =>
        search.root_restriction.kind === 'restricted' &&
        !responsePoolMatchesClasses(
          responseInquiries?.filter(inquiry => responseSearchMatchesInquiry(search, inquiry)) ?? [],
          search.root_restriction.moves_uci,
          responseSearches.filter(prior => Number(prior.work_id.slice(5)) < Number(search.work_id.slice(5))),
          context?.playedMove,
        ) &&
        !proposalContainsResponsePool(proposals ?? [], search, observations) &&
        !retainedRootReplyPoolMatches(search, observations, responseSearches, rootSearches ?? []) &&
        !(preparationQuestion && search.root_move === preparationQuestion.root_move &&
          search.root_move_occurrence_id === preparationQuestion.root_occurrence_id &&
          preparationResponseMovesMatch(preparationQuestion, observations,
            search.prefix_moves_uci ?? [search.root_move], search.root_restriction.moves_uci)) &&
        !(purposeQuestion && search.root_move === purposeQuestion.root_move &&
          search.root_move_occurrence_id === purposeQuestion.root_occurrence_id &&
          search.child_position_id === purposeQuestion.child_position_id && search.prefix_moves_uci?.length === 1 &&
          purposeResponseMovesMatch(purposeQuestion, search.root_restriction.moves_uci,
            positions.get(search.child_position_id)?.legal_moves.moves.map(row => row.uci) ?? [], {
              observations, searches: responseSearches, roots: rootSearches, workId: search.work_id,
              engineProfile: search.engine_profile, requiredDepth: search.required_depth,
            }) &&
          (observations.paths.some(path => path.id === purposeQuestion.source_line_id &&
            (path.evaluations?.some(value => ['root_search', 'focus_comparison'].includes(value.source)) ||
              rootSearches?.some(work => work.purpose === 'investigation_comparison' && work.state === 'completed' &&
                work.stop_reason === null && work.engine_profile === search.engine_profile &&
                work.required_depth >= search.required_depth &&
                Number(work.work_id.slice(5)) < Number(search.work_id.slice(5)) &&
                work.root_occurrence_id === purposeQuestion.root_occurrence_id &&
                work.candidates.some(candidate => candidate.line_id === path.id &&
                  candidate.move_uci === purposeQuestion.root_move && candidate.occurrence_id === path.occurrenceIds[0]))) &&
            path.moves[1] && search.root_restriction.kind === 'restricted' &&
            search.root_restriction.moves_uci.includes(path.moves[1].uci)) ||
          responseSearches.some(prior => Number(prior.work_id.slice(5)) < Number(search.work_id.slice(5)) &&
            prior.root_restriction.kind === 'unrestricted' && prior.root_move === search.root_move &&
            prior.root_move_occurrence_id === search.root_move_occurrence_id &&
            prior.child_position_id === search.child_position_id && prior.prefix_moves_uci?.length === 1))) &&
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
    if (raw.detail.summary !== undefined &&
      (typeof raw.detail.summary !== 'string' || !raw.detail.summary.trim() || raw.detail.summary.length > 4000))
      return;
    if ((raw.detail.summary_exchange !== undefined || raw.detail.summary_question !== undefined || raw.detail.summary_unavailable !== undefined) &&
      (raw.kind !== 'comparison' || raw.detail.type !== 'root_comparison')) return;
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
          'perspective', 'question_refs', 'evaluation_relation', 'stop_reason', 'text'], ['effects', 'summary']) ||
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
        if (detail.evaluation_relation !== answer.evaluation_relation ||
          !sameIds(raw.line_ids, [answer.reference_line_id, answer.alternative_line_id]) ||
          ![answer.reference_occurrence_id, answer.alternative_occurrence_id].every(id => occurrenceIds.includes(id))) return;
      } else if (raw.selection !== 'context' || detail.evaluation_relation !== null) return;
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
        raw.detail.type !== 'root_comparison' ||
        !exactKeys(detail, ['type', 'scope', 'comparison_evidence_id', 'comparison_kind',
          'reference_line_id', 'candidate_line_id', 'missing_line_ids', 'material_relation',
          'material_unavailable_reason', 'text'], ['perspective', 'reference_net_cp', 'candidate_net_cp', 'effects', 'summary', 'summary_kind', 'summary_exchange', 'summary_question', 'summary_unavailable']) ||
        (detail.summary_unavailable !== undefined &&
          (!object(detail.summary_unavailable) || !exactKeys(detail.summary_unavailable, ['question', 'reason']) ||
            !id(detail.summary_unavailable.question) || !detail.summary_unavailable.question.trim() ||
            detail.summary_unavailable.reason !== 'answer_not_delivered' ||
            ['summary', 'summary_kind', 'summary_exchange', 'summary_question'].some(key => key in detail))) ||
        (detail.summary_kind !== undefined &&
          (!['observed_convergence', 'exchange_retention', 'question_answer'].includes(String(detail.summary_kind)) || typeof detail.summary !== 'string')) ||
        ((detail.summary_kind === 'exchange_retention') !== (detail.summary_exchange !== undefined)) ||
        ((detail.summary_kind === 'question_answer') !== (detail.summary_question !== undefined)) ||
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
      if (detail.summary_exchange !== undefined) {
        try {
          const exchange = decodeExchangeSummary(detail.summary_exchange, usedPaths, raw.evidence_ids);
          if (!exchange || detail.material_relation === 'unavailable') return;
          decodedDetail = { ...detail, summary_exchange: exchange };
        } catch (_) { return; }
      }
      if (detail.summary_question !== undefined) {
        const question = decodeQuestionSummary(detail.summary_question, usedPaths, raw.evidence_ids, detail.summary, sourceRefs, observations, proposals, context?.playedMove, responseInquiries, responseSearches, rootSearches, purposeQuestion);
        if (!question || detail.material_relation === 'unavailable') return;
        decodedDetail = { ...detail, summary_question: question };
      }
      const unavailable = detail.material_relation === 'unavailable';
      if (
        !['different', 'equal', 'unavailable'].includes(String(detail.material_relation)) ||
        (!unavailable && detail.material_unavailable_reason !== null) ||
        (unavailable && !['missing_path', 'incomplete_line', 'material_evidence_unavailable'].includes(
          String(detail.material_unavailable_reason),
        )) ||
        ((reference === null || candidate === null) && detail.material_unavailable_reason !== 'missing_path') ||
        (reference !== null && candidate !== null && detail.material_unavailable_reason === 'missing_path')
      )
        return;
      const left = reference === null ? undefined : paths.get(reference)?.lineSummary;
      const right = candidate === null ? undefined : paths.get(candidate)?.lineSummary;
      if (!unavailable && (!left?.material || !right?.material)) return;
      if (
        !unavailable &&
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
      if (unavailable ? hasMaterial : !hasMaterial) return;
      if (
        !unavailable &&
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
    }
    candidates.push({
      id: raw.id,
      kind: raw.kind as CommentaryCandidate['kind'],
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
  const withEffects = candidates.filter(candidate => candidate.detail.effects !== undefined);
  if (withEffects.length) {
    const observationSources = new Map<string, Set<string>>();
    const statementsAt = new Map<string, MoveReviewObservations['statements']>();
    for (const statement of observations.statements) {
      const statements = statementsAt.get(statement.occurrenceId) ?? [];
      statements.push(statement);
      statementsAt.set(statement.occurrenceId, statements);
      const sources = observationSources.get(statement.occurrenceId) ?? new Set<string>();
      for (const id of statement.premiseIds) sources.add(id);
      if (statement.kind === 'piece_control')
        for (const id of [...statement.before.premiseIds, ...statement.after.premiseIds]) sources.add(id);
      observationSources.set(statement.occurrenceId, sources);
    }
    const positionSources = new Map<string, Set<string>>();
    const sourcesAt = (id: string): Set<string> | undefined => {
      const cached = positionSources.get(id);
      if (cached) return cached;
      const position = positions.get(id);
      if (!position) return;
      const sources = new Set([
        position.premise_id,
        ...position.controls.pieces.flatMap(piece => piece.source_ids),
        ...position.rays.lines.map(ray => ray.source_id),
        ...position.legal_moves.moves.map(move => move.source_id),
        ...position.pawn_states.pawns.flatMap(pawn => pawn.source_ids),
        ...position.pawn_files.files.flatMap(file => file.source_ids),
        ...position.pawn_advances.paths.map(advance => advance.source_id),
      ]);
      positionSources.set(id, sources);
      return sources;
    };
    const proposalById = new Map(proposals?.map(proposal => [proposal.id, proposal]));
    const selectedContact = (candidate: CommentaryCandidate, path: ObservationPath, ply: number,
      actor: CommentaryInquiryPiece, target: CommentaryInquiryPiece, square: Key, evidenceIds: string[]): boolean => {
      if (candidate.detail.type !== 'question_comparison') return false;
      const refs = candidate.detail.question_refs as { proposal_id: string; requirement_id: string }[];
      type Fact = Extract<CommentaryProposalReading, { kind: 'fact' }>;
      const physical = (piece: CommentaryFactPiece | undefined, original: CommentaryInquiryPiece) =>
        !!piece && piece.side === original.side && piece.role === original.role && piece.square === original.square;
      const sampled = (fact: Fact, value: string, line?: string) => fact.result?.samples.some(sample =>
        sample.state === 'known' && sample.value === value && sample.from_ply === ply &&
        sample.through_ply === ply && (line === undefined || sample.line_id === line));
      return refs.some(ref => {
        const proposal = proposalById.get(ref.proposal_id);
        const comparison = proposal?.requirements.find(reading => reading.id === ref.requirement_id);
        if (comparison?.kind !== 'compare_moves' || !comparison.answer) return false;
        const answer = comparison.answer;
        const facts = proposal!.requirements.filter((reading): reading is Fact =>
          reading.kind === 'fact' && ['supported', 'observed'].includes(reading.state) &&
          !!reading.result && !reading.result.needs.length);
        // The selected comparison must re-confirm the same causal event on this
        // exact returned arm. A fact from an older or longer line cannot own it.
        return facts.some(current => {
          const source = current.path.source;
          if (source.kind !== 'comparison' || source.requirement_id !== ref.requirement_id ||
            (source.arm === 'reference' ? answer.reference_line_id : answer.alternative_line_id) !== path.id ||
            current.at.kind !== 'event' || current.at.phase !== 'after' ||
            current.at.from_ply !== ply - 1 || current.at.through_ply !== ply ||
            current.predicate.kind !== 'location' || current.expected === undefined ||
            !sampled(current, current.expected, path.id)) return false;
          return facts.some(cause => {
            const declared = cause.path.source;
            if (declared.kind !== 'declared' || declared.moves_uci.length !== ply ||
              !factPrefix(declared.moves_uci, path.moves.map(move => move.uci)) ||
              JSON.stringify(cause.at) !== JSON.stringify(current.at) ||
              JSON.stringify(cause.through) !== JSON.stringify(current.through) ||
              JSON.stringify(cause.predicate) !== JSON.stringify(current.predicate) ||
              cause.expected !== current.expected || !sampled(cause, current.expected!)) return false;
            const atContact = (fact: Fact) => fact.path.binding === cause.path.binding &&
              fact.path.source.kind === 'declared' &&
              JSON.stringify(fact.path.source.moves_uci) === JSON.stringify(declared.moves_uci) &&
              fact.at.kind === 'ply' && fact.at.ply_count === ply;
            if (facts.some(fact => atContact(fact) && fact.predicate.kind === 'location' &&
              physical(fact.predicate.subject, target) && sampled(fact, square)) &&
              facts.some(fact => atContact(fact) && fact.predicate.kind === 'control' &&
                physical(fact.predicate.subject, actor) && fact.predicate.square === square &&
                sampled(fact, 'present'))) return true;
            // The same selected cause may also own a new attack by its actual mover.
            // A stationary beneficiary or an attack already present cannot use this path.
            if (!physical(cause.predicate.subject, actor) || cause.at.kind !== 'event' ||
              (cause.at.actor !== undefined && !physical(cause.at.actor, actor))) return false;
            const before = continuationPiecesAt(path, ply - 1), after = continuationPiecesAt(path, ply);
            const actorBefore = before.find(piece => samePiece(piece.original, actor))?.current;
            const actorAfter = after.find(piece => samePiece(piece.original, actor))?.current;
            const targetBefore = before.find(piece => samePiece(piece.original, target))?.current;
            const targetAfter = after.find(piece => samePiece(piece.original, target))?.current;
            if (!actorBefore || !actorAfter || !targetBefore || !targetAfter || targetAfter.square !== square ||
              path.moves[ply - 1]?.uci.slice(0, 2) !== actorBefore.square || cause.expected !== actorAfter.square) return false;
            const beforePosition = positions.get(path.positionIds?.[ply - 1] ?? '');
            const afterPosition = positions.get(path.positionIds?.[ply] ?? '');
            const beforeControl = beforePosition?.controls.pieces.find(row => physical(row.piece, actorBefore));
            const afterControl = afterPosition?.controls.pieces.find(row => physical(row.piece, actorAfter));
            const contact = afterControl?.squares.indexOf(targetAfter.square) ?? -1;
            return !!beforePosition?.controls.complete && !beforeControl?.squares.includes(targetBefore.square) &&
              !!afterPosition && contact >= 0 && evidenceIds.includes(beforePosition.premise_id) &&
              evidenceIds.includes(afterPosition.premise_id) && evidenceIds.includes(afterControl!.source_ids[contact]);
          });
        });
      });
    };
    try {
      for (const candidate of withEffects) {
        const effects = candidate.detail.effects;
        if (!context || candidate.kind !== 'comparison' ||
          !['root_comparison', 'pv_comparison', 'question_comparison'].includes(candidate.detail.type) ||
          !Array.isArray(effects) || !effects.length || effects.length > 600) return;
        const functionUses: { effect: CommentaryEffect; lossOffset?: number }[] = [];
        // Future attacks need a functional use or an exactly owned, selected contact.
        const dependencyOrder = (effect: CommentaryEffect) => effect.attack_ply !== undefined ? 2 :
          effect.scope === 'check_sequence' ? 1 : 0;
        for (const effect of [...effects].sort((a, b) => dependencyOrder(a) - dependencyOrder(b))) {
          let functionalUse = false;
          let supportLossOffset: number | undefined;
          if (!object(effect) || !exactKeys(effect, ['scope', 'line_ids', 'prefix_moves_uci', 'through_plies',
            'actor', 'target', 'square', 'occurrence_ids', 'evidence_ids', ...('attack_ply' in effect ? ['attack_ply'] : [])]) ||
            !['pair_difference', 'line_history', 'check_sequence', 'support_handoff', 'support_use', 'support_retention',
              'piece_route', 'blocker_handoff', 'clearance_use', 'pawn_option', 'capture_choice',
              'advance_choice', 'relative_pin', 'capture_release'].includes(effect.scope) ||
            !ids(effect.line_ids) || !effect.line_ids.every(line => candidate.lineIds.includes(line)) ||
            !factMoves(effect.prefix_moves_uci) || effect.prefix_moves_uci.length >= 80 ||
            !Array.isArray(effect.through_plies) || effect.through_plies.length !== effect.line_ids.length ||
            !object(effect.actor) || !exactKeys(effect.actor, ['position_id', 'side', 'role', 'square']) ||
            (effect.target !== null && (!object(effect.target) ||
              !exactKeys(effect.target, ['position_id', 'side', 'role', 'square']))) ||
            typeof effect.square !== 'string' || parseSquare(effect.square) === undefined ||
            !ids(effect.occurrence_ids) || !effect.occurrence_ids.length ||
            !ids(effect.evidence_ids) || !effect.evidence_ids.length) return;
          const prefix = effect.prefix_moves_uci, index = prefix.length;
          if ('attack_ply' in effect && (effect.scope !== 'line_history' ||
            !integer(effect.attack_ply, index + 2, 599))) return;
          const expectedPrefix = candidate.detail.type === 'question_comparison'
            ? (candidate.detail.prefix_moves_uci as Uci[]) : [];
          const pair = effect.scope === 'pair_difference', checkSequence = effect.scope === 'check_sequence';
          const choice = ['pawn_option', 'capture_choice', 'advance_choice'].includes(effect.scope);
          if (JSON.stringify(prefix) !== JSON.stringify(expectedPrefix) ||
            (pair || choice ? effect.line_ids.length !== 2 || !sameIds(effect.line_ids, candidate.lineIds)
              : effect.line_ids.length !== 1) ||
            (!pair && effect.scope !== 'pawn_option' && effect.target === null)) return;
          const used = effect.line_ids.map(id => paths.get(id)!);
          if (used.some((path, n) => path.kind === 'history' || path.startFen !== context.rootFen ||
            !factPrefix(prefix, path.moves.map(move => move.uci)) ||
            path.positionIds?.[index] !== used[0].positionIds?.[index] ||
            path.occurrenceIds.slice(0, index).some((id, at) => id !== used[0].occurrenceIds[at]) ||
            !integer(effect.through_plies[n], index + 1, path.moves.length) ||
            (pair && ![index + 1, index + 2].includes(effect.through_plies[n])))) return;
          if (pair && (effect.through_plies.filter(ply => ply === index + 2).length > 1 ||
            (effect.target !== null && effect.through_plies.some(ply => ply !== index + 1)))) return;
          const anchor = index + (pair || choice || effect.scope === 'capture_release' ? 0 : 1);
          const pieces = continuationPiecesAt(used[0], anchor);
          const actor = pieces.find(piece => samePiece(piece.current, effect.actor));
          const target = effect.target === null ? undefined :
            pieces.find(piece => samePiece(piece.current, effect.target!));
          if (!actor || (effect.target !== null
            ? !target || (pair && target.current.square !== effect.square) ||
              (!checkSequence && samePiece(target.original, actor.original))
            : pieces.some(piece => piece.current.square === effect.square))) return;
          const controlled = (path: ObservationPath, count: number, reverse = false): boolean | undefined => {
            const at = continuationPiecesAt(path, count);
            const currentActor = at.find(piece => samePiece(piece.original,
              reverse ? target!.original : actor.original))?.current;
            const currentTarget = target && at.find(piece => samePiece(piece.original,
              reverse ? actor.original : target.original))?.current;
            if (!currentActor || (target ? !currentTarget : at.some(piece => piece.current.square === effect.square))) return;
            const square = currentTarget?.square ?? effect.square;
            const resource = positions.get(path.positionIds?.[count] ?? '');
            const control = resource?.controls.pieces.find(row => row.piece.side === currentActor.side &&
              row.piece.role === currentActor.role && row.piece.square === currentActor.square);
            return control?.squares.includes(square) ? true : resource?.controls.complete ? false : undefined;
          };
          if (choice || effect.scope === 'relative_pin' || effect.scope === 'capture_release') {
            // These claims own their witnesses only; they do not certify a future attack's functional parent.
            const at = (path: ObservationPath, original: CommentaryInquiryPiece, count: number) =>
              continuationPiecesAt(path, count).find(piece => samePiece(piece.original, original))?.current;
            const located = (left: { side: Color; role: string; square: string } | undefined,
              right: { side: Color; role: string; square: string } | undefined) =>
              !!left && !!right && left.side === right.side && left.role === right.role && left.square === right.square;
            const position = (path: ObservationPath, count: number) => positions.get(path.positionIds?.[count] ?? '');
            const legal = (path: ObservationPath, count: number) => {
              const resource = position(path, count);
              return resource?.legal_moves.complete ? resource.legal_moves.moves.find(move =>
                move.uci === path.moves[count]?.uci && effect.evidence_ids.includes(move.source_id)) : undefined;
            };
            const control = (path: ObservationPath, count: number, provider: CommentaryInquiryPiece, square: Key) => {
              const piece = at(path, provider, count);
              const row = position(path, count)?.controls.pieces.find(row => located(row.piece, piece));
              const offset = row?.squares.indexOf(square) ?? -1;
              return offset >= 0 && effect.evidence_ids.includes(row!.source_ids[offset]);
            };
            const advance = (path: ObservationPath, count: number, original: CommentaryInquiryPiece,
              destination: Key, traversed: Key[]) => {
              const piece = at(path, original, count);
              return position(path, count)?.pawn_advances.paths.some(row => located(row.piece, piece) &&
                row.destination === destination && JSON.stringify(row.traversed_squares) === JSON.stringify(traversed) &&
                effect.evidence_ids.includes(row.source_id));
            };
            if (!sameIds(effect.occurrence_ids, [...new Set(used.flatMap((path, n) =>
              path.occurrenceIds.slice(index, effect.through_plies[n]).map(id => id ?? '')))])) return;
            for (const [n, path] of used.entries()) {
              if (!legal(path, index)) return;
              for (let count = index; count <= effect.through_plies[n]; count++)
                if (!position(path, count) || !effect.evidence_ids.includes(position(path, count)!.premise_id)) return;
            }
            if (effect.scope === 'pawn_option') {
              const [kept, changed] = used, through = effect.through_plies[0], last = through - 1;
              const home = actor.current.side === 'white' ? '2' : '7';
              const step = `${actor.current.square[0]}${actor.current.side === 'white' ? '3' : '6'}` as Key;
              const destination = `${actor.current.square[0]}${actor.current.side === 'white' ? '4' : '5'}` as Key;
              const changedMove = legal(changed, index)!;
              if (actor.current.role !== 'pawn' || actor.current.square[1] !== home || effect.square !== destination ||
                effect.through_plies[1] !== index + 1 ||
                changedMove.uci !== actor.current.square + step || changedMove.captured_square !== undefined ||
                !located(at(kept, actor.original, index + 1), actor.current) ||
                !advance(kept, index, actor.original, destination, [step, destination]) ||
                !advance(kept, index + 1, actor.original, destination, [step, destination])) return;
              if (through === index + 1) {
                if (target) return;
              } else {
                const move = legal(kept, last), finalActor = at(kept, actor.original, through);
                if (!move || move.uci !== actor.current.square + destination || move.captured_square !== undefined ||
                  finalActor?.role !== 'pawn' || finalActor.square !== destination ||
                  !advance(kept, last, actor.original, destination, [step, destination])) return;
                for (let count = index + 1; count <= last; count++)
                  if (!located(at(kept, actor.original, count), actor.current)) return;
                if (target) {
                  const supported = at(kept, target.original, through);
                  if (target.current.side !== actor.current.side || !supported ||
                    !control(kept, through, actor.original, supported.square)) return;
                }
              }
            } else if (effect.scope === 'capture_choice') {
              const [kept, changed] = used, victim = target!;
              const first = legal(kept, index)!, second = legal(changed, index)!;
              const capturer = continuationPiecesAt(kept, index).find(piece => piece.current.square === first.uci.slice(0, 2));
              const after = capturer && at(kept, capturer.original, index + 1);
              if (effect.through_plies.some(ply => ply !== index + 1) ||
                actor.current.side === victim.current.side || effect.square !== victim.current.square ||
                first.captured_square !== effect.square || second.captured_square !== effect.square ||
                first.destination !== effect.square || second.destination !== effect.square ||
                second.uci.slice(0, 2) !== actor.current.square || !capturer ||
                samePiece(capturer.original, actor.original) || capturer.current.side !== actor.current.side ||
                !after || after.square !== effect.square ||
                !located(at(kept, actor.original, index + 1), actor.current) ||
                used.some(path => at(path, victim.original, index + 1)) ||
                !control(kept, index + 1, actor.original, after.square)) return;
            } else if (effect.scope === 'advance_choice') {
              const [blocked, usedAdvance] = used, blocker = target!;
              const destination = effect.square;
              const available = position(blocked, index)?.pawn_advances.paths.find(row =>
                located(row.piece, actor.current) && row.destination === destination &&
                effect.evidence_ids.includes(row.source_id));
              if (!available) return;
              const traversed = available.traversed_squares;
              const blockingMove = legal(blocked, index)!, reply = legal(usedAdvance, index + 1);
              const stopped = position(blocked, index + 1)!;
              if (actor.current.role !== 'pawn' || blocker.current.role !== 'pawn' ||
                actor.current.side === blocker.current.side ||
                effect.through_plies[0] !== index + 1 || effect.through_plies[1] !== index + 2 ||
                blockingMove.uci.slice(0, 2) !== blocker.current.square ||
                blockingMove.destination !== destination || blockingMove.captured_square !== undefined ||
                at(blocked, blocker.original, index + 1)?.square !== destination ||
                !located(at(blocked, actor.original, index + 1), actor.current) ||
                !stopped.pawn_advances.complete || stopped.pawn_advances.paths.some(row =>
                  located(row.piece, actor.current) && row.destination === destination) ||
                !advance(blocked, index, actor.original, destination, traversed) ||
                !located(at(usedAdvance, actor.original, index + 1), actor.current) ||
                !located(at(usedAdvance, blocker.original, index + 2), blocker.current) ||
                !advance(usedAdvance, index + 1, actor.original, destination, traversed) ||
                !reply || reply.uci !== actor.current.square + destination || reply.captured_square !== undefined ||
                at(usedAdvance, actor.original, index + 2)?.square !== destination) return;
            } else if (effect.scope === 'relative_pin') {
              const path = used[0], through = effect.through_plies[0], front = target!;
              const rear = pieces.find(piece => piece.current.square === effect.square);
              const value = { pawn: 1, knight: 3, bishop: 3, rook: 5, queen: 9, king: 100 };
              if (!['bishop', 'rook', 'queen'].includes(actor.current.role) || !rear ||
                rear.current.role === 'king' || front.current.role === 'king' || front.current.side === actor.current.side ||
                rear.current.side !== front.current.side || value[rear.current.role] <= value[front.current.role]) return;
              const aligned = (count: number): boolean | undefined => {
                const resource = position(path, count);
                if (!resource?.rays.complete) return;
                const pinner = at(path, actor.original, count), pinned = at(path, front.original, count),
                  behind = at(path, rear.original, count);
                if (!pinner || !pinned || !behind || behind.role === 'king' ||
                  value[behind.role] <= value[pinned.role]) return false;
                const ray = resource.rays.lines.find(ray => located(ray.piece, pinner) &&
                  located(ray.occupants[0], pinned) && located(ray.occupants[1], behind));
                return ray ? effect.evidence_ids.includes(ray.source_id) ? true : undefined : false;
              };
              for (let count = anchor; count <= through; count++) {
                const present = aligned(count);
                if (present === undefined || (!present && (count === anchor || count !== through))) return;
                if (count === through && present && through !== path.moves.length) return;
              }
              if (through > anchor && !legal(path, through - 1)) return;
            } else {
              const path = used[0], victim = target!, through = effect.through_plies[0];
              const releasedAt = index + 1;
              const move = legal(path, index)!, before = position(path, index)!, after = position(path, releasedAt)!;
              const king = pieces.find(piece => piece.current.square === move.uci.slice(0, 2));
              const kingAfter = king && at(path, king.original, releasedAt);
              if (![releasedAt, releasedAt + 1].includes(through) || actor.current.role === 'king' || victim.current.role === 'king' ||
                actor.current.side === victim.current.side || effect.square !== victim.current.square ||
                (actor.current.role === 'pawn' && ['1', '8'].includes(effect.square[1])) ||
                !king || king.current.role !== 'king' || king.current.side !== actor.current.side || !kingAfter ||
                move.captured_square !== undefined ||
                Math.max(Math.abs(king.current.square.charCodeAt(0) - kingAfter.square.charCodeAt(0)),
                  Math.abs(Number(king.current.square[1]) - Number(kingAfter.square[1]))) !== 1 ||
                !located(at(path, actor.original, releasedAt), actor.current) ||
                !located(at(path, victim.original, releasedAt), victim.current) || !before.rays.complete ||
                !control(path, index, actor.original, effect.square) ||
                !control(path, releasedAt, actor.original, effect.square)) return;
              if (through === releasedAt + 1) {
                const reply = legal(path, releasedAt), movedTarget = at(path, victim.original, through);
                if (!reply || reply.uci.slice(0, 2) !== victim.current.square || !movedTarget ||
                  movedTarget.square !== reply.destination) return;
              }
              const ray = before.rays.lines.find(ray => ray.piece.side !== actor.current.side &&
                located(ray.occupants[0], actor.current) && located(ray.occupants[1], king.current) &&
                effect.evidence_ids.includes(ray.source_id));
              if (!ray || ray.piece.square === effect.square || ray.squares.includes(effect.square)) return;
              const pinner = pieces.find(piece => located(piece.current, ray.piece));
              if (!pinner || !located(at(path, pinner.original, releasedAt), pinner.current)) return;
              // A frozen ordinary capture tests only own-king safety. It never changes turn or creates a line.
              const frozenCapture = (resource: NonNullable<ReturnType<typeof position>>) => {
                const frozen = setupPosition('chess', parseFen(resource.fen).unwrap()).unwrap();
                const from = parseSquare(actor.current.square)!, to = parseSquare(effect.square)!;
                const piece = frozen.board.get(from), captured = frozen.board.get(to);
                if (!piece || !captured || piece.color !== actor.current.side || piece.role !== actor.current.role ||
                  captured.color !== victim.current.side || captured.role !== victim.current.role ||
                  !attacks(piece, from, frozen.board.occupied).has(to)) return;
                frozen.board.take(from);
                frozen.board.set(to, piece);
                const square = frozen.board.kingOf(actor.current.side);
                return square === undefined ? undefined : frozen.kingAttackers(square,
                  actor.current.side === 'white' ? 'black' : 'white', frozen.board.occupied);
              };
              const exposed = frozenCapture(before), released = frozenCapture(after);
              if (!exposed?.has(parseSquare(pinner.current.square)!) || !released?.isEmpty()) return;
            }
          } else if (effect.attack_ply !== undefined) {
            const path = used[0], attackPly = effect.attack_ply, through = effect.through_plies[0];
            const beforeAttack = continuationPiecesAt(path, attackPly - 1);
            const mover = beforeAttack.find(piece => piece.current.square === path.moves[attackPly - 1]?.uci.slice(0, 2));
            const afterAttack = continuationPiecesAt(path, attackPly);
            const attacker = afterAttack.find(piece => samePiece(piece.original, actor.original));
            const victim = afterAttack.find(piece => samePiece(piece.original, target!.original));
            const position = positions.get(path.positionIds?.[attackPly] ?? '');
            const control = position?.controls.pieces.find(row => attacker && row.piece.side === attacker.current.side &&
              row.piece.role === attacker.current.role && row.piece.square === attacker.current.square);
            const controlIndex = victim ? control?.squares.indexOf(victim.current.square) : undefined;
            if (!target || actor.current.side === target.current.side || effect.square !== target.current.square ||
              through !== path.moves.length || attackPly >= through ||
              controlled(path, attackPly) !== true || controlIndex === undefined || controlIndex < 0 ||
              !position || !effect.evidence_ids.includes(position.premise_id) ||
              !effect.evidence_ids.includes(control!.source_ids[controlIndex]) ||
              !effect.occurrence_ids.includes(path.occurrenceIds[attackPly - 1]!) ||
              (attackPly !== index + 2 && !functionUses.some(({ effect: parent }) =>
                parent.line_ids[0] === path.id && parent.through_plies[0] === attackPly &&
                JSON.stringify(parent.prefix_moves_uci) === JSON.stringify(prefix) &&
                // An evading slider may regain its attack when another piece
                // opens the line. Its validated use belongs to that same slider.
                (parent.scope === 'line_history' && parent.target?.side !== parent.actor.side
                  ? samePiece(parent.actor, actor.current)
                  : !!mover && samePiece(mover.original, actor.original)) &&
                parent.evidence_ids.every(id => effect.evidence_ids.includes(id))) &&
                !selectedContact(candidate, path, attackPly, actor.original, target.original,
                  victim!.current.square, effect.evidence_ids))) return;
          } else if (pair) {
            const relations = used.map(path => controlled(path, index + 1));
            if (relations.some(value => value === undefined) || relations[0] === relations[1]) return;
            for (const [n, path] of used.entries()) {
              if (effect.through_plies[n] !== index + 2) continue;
              const before = continuationPiecesAt(path, index + 1).find(piece =>
                piece.current.square === path.moves[index + 1].uci.slice(0, 2));
              if (!before || continuationPiecesAt(path, index + 2).find(piece =>
                samePiece(piece.original, before.original))?.current.square !== effect.square) return;
            }
          } else if (checkSequence) {
            const path = used[0], through = effect.through_plies[0];
            if (!target || target.current.role !== 'king' || target.current.square !== effect.square ||
              !(statementsAt.get(path.occurrenceIds[index]!) ?? []).some(statement =>
                statement.side === actor.current.side && statement.to === actor.current.square &&
                continuationPiecesAt(path, index).some(piece => samePiece(piece.original, actor.original) &&
                  piece.current.square === statement.from))) return;
            const eligible = new Set([index, index + 1]);
            // The first uninterrupted capture transaction can contain a recapture
            // check. A later transaction cannot donate a check to this move.
            const material = path.lineSummary?.material;
            const captures = material?.events.filter(event => event.capture) ?? [];
            if (index === 0 && captures.length >= 2 && captures[0].ply_offset <= 1) {
              const chain = [captures[0]];
              for (const capture of captures.slice(1)) {
                if (capture.ply_offset !== chain[chain.length - 1].ply_offset + 1) break;
                chain.push(capture);
              }
              if (chain.length >= 2 && !material!.events.some(event => event.promotion_gain_cp > 0 &&
                event.ply_offset <= chain[chain.length - 1].ply_offset))
                for (const capture of chain) eligible.add(capture.ply_offset);
            }
            const selected = (offset: number) => statementsAt.get(path.occurrenceIds[offset]!) ?? [];
            for (const { effect: parent, lossOffset } of functionUses)
              if (parent.line_ids[0] === path.id && JSON.stringify(parent.prefix_moves_uci) === JSON.stringify(prefix)) {
                if (parent.through_plies[0] <= through) eligible.add(parent.through_plies[0] - 1);
                // The independently validated defense also owns the check at its exact target's loss.
                if (lossOffset !== undefined && lossOffset < through) eligible.add(lossOffset);
              }
            for (const start of [...eligible])
              for (let offset = start; offset < through && selected(offset).some(row => row.kind === 'check_responses'); offset += 2)
                eligible.add(offset);
            const checks = [...eligible].filter(offset => offset < through).flatMap(offset =>
              selected(offset).filter(statement => statement.kind === 'check_responses').map(statement =>
                ({ offset, statement })));
            const responses = [...new Set([index, ...checks.map(row => row.offset + 1)])]
              .filter(offset => offset < path.moves.length).flatMap(offset =>
                selected(offset).filter(statement => statement.kind === 'check_response' &&
                  statement.response.resource.move_uci === path.moves[offset].uci).map(statement =>
                    ({ offset, statement })));
            const rows = [...responses, ...checks].sort((a, b) => a.offset - b.offset);
            if (!rows.length || rows.some(row => row.offset >= through) ||
              through !== Math.max(index, ...rows.map(row => row.offset)) + 1 ||
              !sameIds(effect.occurrence_ids, [...new Set([path.occurrenceIds[index]!,
                ...rows.map(row => path.occurrenceIds[row.offset]!)])]) ||
              !rows.every(row => row.statement.premiseIds.every(id => effect.evidence_ids.includes(id)))) return;
            const first = rows[0].statement;
            const kingSide = first.kind === 'check_responses' ? first.checked_side :
              first.kind === 'check_response' ? first.responding_side : undefined;
            if (target.current.side !== kingSide) return;
          } else if (['support_handoff', 'support_use', 'support_retention', 'piece_route', 'blocker_handoff', 'clearance_use'].includes(effect.scope)) {
            functionalUse = effect.scope !== 'support_retention';
            const path = used[0], through = effect.through_plies[0], last = through - 1, related = target!;
            const at = (original: CommentaryInquiryPiece, count: number) =>
              continuationPiecesAt(path, count).find(piece => samePiece(piece.original, original))?.current;
            const located = (left: { side: Color; role: string; square: string } | undefined,
              right: { side: Color; role: string; square: string } | undefined) =>
              !!left && !!right && left.side === right.side && left.role === right.role && left.square === right.square;
            const position = (count: number) => positions.get(path.positionIds?.[count] ?? '');
            const legal = (count: number) => {
              const resource = position(count);
              const move = resource?.legal_moves.complete && resource.legal_moves.moves.find(move =>
                move.uci === path.moves[count]?.uci && effect.evidence_ids.includes(move.source_id));
              return move || undefined;
            };
            const rootMove = legal(index), finalMove = legal(last), originalActor = at(actor.original, index);
            if (through <= anchor || !rootMove || !finalMove || !originalActor ||
              path.moves[index].uci.slice(0, 2) !== originalActor.square ||
              rootMove.destination !== actor.current.square ||
              !sameIds(effect.occurrence_ids, path.occurrenceIds.slice(index, through).map(id => id ?? '')) ||
              !path.lineSummary?.premise_ids.length ||
              !path.lineSummary.premise_ids.every(id => effect.evidence_ids.includes(id))) return;
            for (let count = index; count <= through; count++)
              if (!position(count) || !effect.evidence_ids.includes(position(count)!.premise_id)) return;
            const rayAt = (count: number, original: CommentaryInquiryPiece, square: string) => {
              const resource = position(count), piece = at(original, count);
              return resource?.rays.complete ? resource.rays.lines.find(ray => located(ray.piece, piece) &&
                ray.squares.includes(square as Key) && effect.evidence_ids.includes(ray.source_id)) : undefined;
            };
            const controlAt = (original: CommentaryInquiryPiece, count: number, square: string): boolean | undefined => {
              const resource = position(count), piece = at(original, count);
              if (!resource || !piece) return;
              const control = resource.controls.pieces.find(row => located(row.piece, piece));
              const offset = control?.squares.indexOf(square as Key) ?? -1;
              return offset >= 0
                ? effect.evidence_ids.includes(control!.source_ids[offset]) ? true : undefined
                : resource.controls.complete ? false : undefined;
            };
            const support = (original: CommentaryInquiryPiece, count: number) => {
              const value = continuationSupportAt(observations, path, original, related.original, count,
                continuationPiecesAt(path, count));
              return value.source_ids.every(id => effect.evidence_ids.includes(id)) ? value.state : 'unknown';
            };
            if (effect.scope === 'support_retention') {
              if (actor.current.side !== related.current.side || effect.square !== related.current.square ||
                support(actor.original, index) !== 'absent') return;
              const previous = continuationPiecesAt(path, last).find(piece =>
                piece.current.square === path.moves[last].uci.slice(0, 2));
              const departed = previous && at(previous.original, through);
              if (!previous || !departed || previous.current.side !== actor.current.side ||
                samePiece(previous.original, actor.original) || samePiece(previous.original, related.original) ||
                located(previous.current, departed) || finalMove.destination !== departed.square) return;
              // The newly added support remains when another actual provider leaves.
              // This relation alone makes no claim about a capture or its legality.
              for (let count = anchor; count <= through; count++)
                if (support(actor.original, count) !== 'present') return;
              for (let count = index; count <= last; count++)
                if (support(previous.original, count) !== 'present') return;
              if (support(previous.original, through) !== 'absent') return;
            } else if (effect.scope === 'support_handoff' || effect.scope === 'support_use') {
              const usedSupport = effect.scope === 'support_use';
              if (actor.current.side !== related.current.side || (!usedSupport && effect.square !== related.current.square)) return;
              const material = path.lineSummary.material;
              const recovery = material?.events.find(event => event.ply_offset === last && event.capture);
              const beforeRecovery = continuationPiecesAt(path, last);
              const provider = beforeRecovery.find(piece => piece.current.square === path.moves[last].uci.slice(0, 2));
              const capturer = recovery?.capture && beforeRecovery.find(piece =>
                piece.current.square === recovery.capture!.square);
              if (!recovery || !provider || !capturer || provider.current.side !== related.current.side ||
                capturer.current.side === related.current.side ||
                finalMove.captured_square !== capturer.current.square ||
                finalMove.destination !== at(provider.original, through)?.square) return;
              const loss = material!.events.find(event => event.ply_offset >= anchor && event.ply_offset < last &&
                event.capture && at(related.original, event.ply_offset)?.square === event.capture.square &&
                at(capturer.original, event.ply_offset)?.square === event.from);
              if (!loss || at(related.original, loss.ply_offset + 1)) return;
              supportLossOffset = loss.ply_offset;
              const lossMove = legal(loss.ply_offset), capturedAt = loss.ply_offset + 1;
              const providerAfterLoss = at(provider.original, capturedAt), enemyAfterLoss = at(capturer.original, capturedAt);
              const immediate = position(capturedAt);
              // A later unpin cannot certify that this defense already worked at the loss.
              const immediateRecapture = immediate?.legal_moves.complete && immediate.legal_moves.moves.find(move =>
                move.uci.slice(0, 2) === providerAfterLoss?.square && move.captured_square === enemyAfterLoss?.square &&
                move.destination === enemyAfterLoss?.square && effect.evidence_ids.includes(move.source_id));
              if (!lossMove || lossMove.captured_square !== loss.capture!.square || !immediateRecapture) return;
              for (let count = capturedAt; count <= last; count++)
                if (!located(at(provider.original, count), providerAfterLoss) ||
                  !located(at(capturer.original, count), enemyAfterLoss)) return;
              if (usedSupport) {
                if (!samePiece(provider.original, actor.original) || loss.capture!.square !== effect.square ||
                  lossMove.destination !== effect.square || finalMove.destination !== effect.square) return;
                for (let count = anchor; count <= loss.ply_offset; count++)
                  if (!located(at(provider.original, count), actor.current)) return;
                const oldTarget = at(related.original, index);
                // A moving friendly target may keep using the same stationary provider.
                // An empty-square preparation can also incidentally support the
                // target elsewhere. Its route need not keep that other support.
                const gainedSupport = oldTarget && controlAt(provider.original, index, oldTarget.square) === false &&
                  controlAt(provider.original, anchor, related.current.square) === true &&
                  Array.from({ length: loss.ply_offset - anchor + 1 }, (_, offset) => anchor + offset)
                    .every(count => support(provider.original, count) === 'present');
                if (!gainedSupport) {
                  // A future target cannot borrow control of a square occupied by a different piece.
                  if (related.current.square === effect.square ||
                    controlAt(provider.original, index, effect.square) !== false) return;
                  let arrived = false;
                  for (let count = anchor; count <= loss.ply_offset; count++) {
                    const victim = at(related.original, count);
                    if (!victim || controlAt(provider.original, count, effect.square) !== true) return;
                    if (victim.square === effect.square) arrived = true;
                    if (arrived ? victim.square !== effect.square || support(provider.original, count) !== 'present'
                      : continuationPiecesAt(path, count).some(piece => piece.current.square === effect.square)) return;
                  }
                  if (!arrived) return;
                }
              } else {
                for (let count = anchor; count <= loss.ply_offset; count++)
                  if (support(provider.original, count) !== 'present') return;
                const previous = continuationPiecesAt(path, index).filter(piece =>
                  piece.current.side === related.current.side && !samePiece(piece.original, provider.original) &&
                  !samePiece(piece.original, related.original) && support(piece.original, index) === 'present');
                const handedOff = previous.some(old => {
                  let departure: number | undefined;
                  for (let count = anchor; count <= loss.ply_offset; count++) {
                    const state = support(old.original, count);
                    const absent = state === 'absent' || state === 'actor_removed';
                    if (departure !== undefined && !absent) return false;
                    const before = at(old.original, count - 1);
                    if (departure === undefined && absent && before &&
                      support(old.original, count - 1) === 'present' && !located(before, at(old.original, count)))
                      departure = count;
                  }
                  return departure !== undefined && (samePiece(provider.original, actor.original) ||
                    (samePiece(old.original, actor.original) && departure === anchor));
                });
                if (!handedOff) return;
              }
            } else if (effect.scope === 'piece_route') {
              const finalActor = at(actor.original, through), finalTarget = at(related.original, through);
              if (!finalActor || !finalTarget || finalActor.role !== actor.current.role ||
                finalActor.side !== actor.current.side || finalActor.square !== effect.square ||
                finalMove.destination !== effect.square || path.moves[last].uci.slice(0, 2) !== actor.current.square) return;
              // This is the current piece's next move, not a later journey through several moves.
              for (let count = anchor; count <= last; count++)
                if (!located(at(actor.original, count), actor.current)) return;
              const controlChange = (reverse: boolean, before: 'present' | 'absent', after: 'present' | 'absent') =>
                (statementsAt.get(path.occurrenceIds[last]!) ?? []).some(statement => {
                  if (statement.kind !== 'piece_control' || statement.before.state !== before || statement.after.state !== after)
                    return false;
                  const controller = reverse ? related.original : actor.original;
                  const victim = reverse ? actor.original : related.original;
                  if (!located(statement.before.controller ?? undefined, at(controller, last)) ||
                    !located(statement.before.target ?? undefined, at(victim, last)) ||
                    !located(statement.after.controller ?? undefined, at(controller, through)) ||
                    !located(statement.after.target ?? undefined, at(victim, through))) return false;
                  // Effect references use public relation IDs; typed observations retain packet refs.
                  // Resolve only the exact owned position and the same controller/target relation.
                  const aliases = [last, through].flatMap(count => {
                    const resource = position(count)!, piece = at(controller, count), target = at(victim, count);
                    const control = resource.controls.pieces.find(row => located(row.piece, piece));
                    const offset = target ? control?.squares.indexOf(target.square) ?? -1 : -1;
                    const suffix = `:position-occurrence:${resource.id}`;
                    if (offset < 0 || !resource.premise_id.endsWith(suffix)) return [];
                    const source = control!.source_ids[offset];
                    return effect.evidence_ids.includes(source)
                      ? [`${resource.premise_id.slice(0, -suffix.length)}:relation:position:${resource.id}:geometric_control:${source}`]
                      : [];
                  });
                  return [...statement.premiseIds, ...statement.before.premiseIds, ...statement.after.premiseIds]
                    .every(id => effect.evidence_ids.includes(id) || aliases.includes(id));
                });
              const beforeTarget = at(related.original, last)!;
              const gained = controlAt(actor.original, last, beforeTarget.square) === false &&
                controlAt(actor.original, through, finalTarget.square) === true;
              const friendly = actor.current.side === related.current.side;
              const defended = !friendly || position(through)!.controls.pieces.some(control => {
                const offset = control.squares.indexOf(finalTarget.square);
                return control.piece.side !== finalTarget.side && offset >= 0 &&
                  effect.evidence_ids.includes(control.source_ids[offset]);
              });
              const evaded = !friendly && controlAt(related.original, last, actor.current.square) === true &&
                controlAt(related.original, through, finalActor.square) === false && controlChange(true, 'present', 'absent');
              const newRole = finalTarget.role !== 'king' && controlChange(false, 'absent', 'present');
              if (!(gained && defended && newRole) && !evaded) return;
            } else if (effect.scope === 'blocker_handoff') {
              if (!['bishop', 'rook', 'queen'].includes(related.current.role)) return;
              const beforeRay = rayAt(index, related.original, effect.square);
              const beforeSquares = beforeRay?.squares.slice(0, beforeRay.squares.indexOf(effect.square));
              if (!beforeRay || !beforeSquares || beforeSquares.includes(originalActor.square) ||
                !beforeSquares.includes(actor.current.square)) return;
              const oldBlockers = continuationPiecesAt(path, index).filter(piece => beforeSquares.includes(piece.current.square));
              const arriving = continuationPiecesAt(path, last).find(piece => piece.current.square === path.moves[last].uci.slice(0, 2));
              if (!oldBlockers.length || !arriving || samePiece(arriving.original, actor.original) ||
                arriving.current.side === related.current.side || finalMove.destination !== effect.square ||
                at(arriving.original, through)?.square !== effect.square) return;
              for (let count = anchor; count <= through; count++)
                if (!located(at(actor.original, count), actor.current) ||
                  !located(at(related.original, count), related.current)) return;
              if (!rayAt(through, related.original, effect.square)) return;
              let departure: number | undefined;
              for (let count = anchor; count <= through; count++) {
                const occupants = continuationPiecesAt(path, count).filter(piece => beforeSquares.includes(piece.current.square));
                const sole = occupants.length === 1 && samePiece(occupants[0].original, actor.original);
                if (departure === undefined && count < through && sole) departure = count;
                if (departure !== undefined) {
                  const resource = position(count)!;
                  const control = resource.controls.pieces.find(row => located(row.piece, related.current));
                  if (!sole || !resource.controls.complete || control?.squares.includes(effect.square)) return;
                }
              }
              if (departure === undefined) return;
            } else {
              // The same physical pawn may have promoted before using the
              // cleared line. Its role at the actual use owns the geometry.
              if (actor.current.side !== related.current.side ||
                !['bishop', 'rook', 'queen'].includes(at(related.original, through)?.role ?? '') ||
                originalActor.square !== effect.square || actor.current.square === effect.square ||
                path.moves[last].uci.slice(0, 2) !== at(related.original, last)?.square ||
                finalMove.destination !== at(related.original, through)?.square) return;
              for (let count = anchor; count <= through; count++)
                if (continuationPiecesAt(path, count).some(piece => piece.current.square === effect.square)) return;
              const clearThroughout = (squares: Key[]) => {
                for (let count = anchor; count < through; count++)
                  if (continuationPiecesAt(path, count).some(piece => squares.includes(piece.current.square) &&
                    !samePiece(piece.original, related.original))) return false;
                return true;
              };
              const ray = rayAt(last, related.original, finalMove.destination);
              const traversed = ray?.squares.slice(0, ray.squares.indexOf(finalMove.destination));
              const crossed = traversed?.includes(effect.square) && clearThroughout(traversed);
              const resource = position(through)!;
              const controller = resource.controls.pieces.find(control => located(control.piece, at(related.original, through)));
              const usedAfter = !crossed && resource.controls.complete && controller?.squares.some((square, offset) => {
                if (!effect.evidence_ids.includes(controller.source_ids[offset]) ||
                  !continuationPiecesAt(path, through).some(piece => piece.current.square === square)) return false;
                const afterRay = rayAt(through, related.original, square);
                const opened = afterRay?.squares.slice(0, afterRay.squares.indexOf(square));
                return !!opened?.includes(effect.square) && clearThroughout(opened) &&
                  !continuationPiecesAt(path, through).some(piece => opened.includes(piece.current.square));
              });
              if (!crossed && !usedAfter) return;
            }
          } else {
            const path = used[0], through = effect.through_plies[0], victim = target!;
            const at = (original: CommentaryInquiryPiece, count: number) =>
              continuationPiecesAt(path, count).find(piece => samePiece(piece.original, original))?.current;
            const moved = (original: CommentaryInquiryPiece, offset: number) => {
              const before = at(original, offset), after = at(original, offset + 1);
              return !!before && !!after && (before.square !== after.square || before.role !== after.role);
            };
            const located = (piece: { side: Color; role: string; square: string } | null,
              expected: CommentaryInquiryPiece | undefined) => !!piece && !!expected &&
              piece.side === expected.side && piece.role === expected.role && piece.square === expected.square;
            const controlFact = (offset: number, controller: CommentaryInquiryPiece,
              receiving: CommentaryInquiryPiece, afterState: 'present' | 'absent') =>
              statementsAt.get(path.occurrenceIds[offset]!)?.find(statement =>
                statement.kind === 'piece_control' && statement.after.state === afterState &&
                ['present', 'absent'].includes(statement.before.state) &&
                located(statement.before.controller, at(controller, offset)) &&
                located(statement.before.target, at(receiving, offset)) &&
                located(statement.after.controller, at(controller, offset + 1)) &&
                located(statement.after.target, at(receiving, offset + 1)));
            const validHistory = (): boolean => {
              const friendly = actor.current.side === victim.current.side;
              const targetSquare = victim.current.square === effect.square;
              // Existing attack histories still own the entire supplied line.
              if (!friendly && targetSquare && controlled(path, anchor) === true && through === path.moves.length)
                return true;
              functionalUse = true;
              if (friendly && targetSquare) {
                const fact = controlFact(index, actor.original, victim.original, 'present');
                return through === anchor && controlled(path, anchor) === true && fact?.kind === 'piece_control' &&
                  (fact.before.state === 'absent' || moved(actor.original, index) || moved(victim.original, index)) &&
                  sameIds(effect.occurrence_ids, [path.occurrenceIds[index]!]);
              }
              if (!friendly && targetSquare) {
                const fact = controlFact(index, victim.original, actor.original, 'absent');
                if (!moved(actor.original, index) || controlled(path, index, true) !== true ||
                  controlled(path, anchor, true) !== false || fact?.kind !== 'piece_control' ||
                  fact.before.state !== 'present') return false;
                const offsets = [index];
                if (through > anchor) {
                  const last = controlFact(through - 1, actor.original, victim.original, 'present');
                  if (controlled(path, through) !== true || last?.kind !== 'piece_control' ||
                    (last.before.state !== 'absent' && !moved(actor.original, through - 1))) return false;
                  for (let offset = anchor; offset < through; offset++)
                    if (moved(actor.original, offset)) offsets.push(offset);
                  offsets.push(through - 1);
                }
                return sameIds(effect.occurrence_ids, [...new Set(offsets.map(offset => path.occurrenceIds[offset]!))]);
              }
              if (!friendly || targetSquare || through <= anchor ||
                at(actor.original, index)?.square !== effect.square || actor.current.square === effect.square ||
                path.moves[index].uci.slice(0, 2) !== effect.square) return false;
              for (let count = anchor; count < through; count++) {
                const position = positions.get(path.positionIds?.[count] ?? '');
                if (!position || !effect.evidence_ids.includes(position.premise_id) ||
                  continuationPiecesAt(path, count).some(piece => piece.current.square === effect.square)) return false;
              }
              return at(victim.original, through)?.square === effect.square &&
                path.moves[through - 1].uci.slice(0, 2) === at(victim.original, through - 1)?.square &&
                sameIds(effect.occurrence_ids, [path.occurrenceIds[index]!, path.occurrenceIds[through - 1]!]);
            };
            if (!validHistory()) return;
          }
          const occurrences = new Set(used.flatMap((path, n) => path.occurrenceIds.slice(index, effect.through_plies[n])));
          if (!effect.occurrence_ids.every(id => occurrences.has(id) && effect.evidence_ids.includes(id)) ||
            !used.every(path => effect.occurrence_ids.includes(path.occurrenceIds[index]!))) return;
          const unresolved = new Set(effect.evidence_ids);
          for (const [n, path] of used.entries()) {
            for (const id of path.lineSummary?.premise_ids ?? []) unresolved.delete(id);
            for (const occurrence of path.occurrenceIds.slice(0, effect.through_plies[n])) {
              if (!occurrence) return;
              unresolved.delete(occurrence);
              for (const id of observationSources.get(occurrence) ?? []) unresolved.delete(id);
            }
            for (const id of path.positionIds?.slice(0, effect.through_plies[n] + 1) ?? []) {
              if (!unresolved.size) break;
              const sources = id ? sourcesAt(id) : undefined;
              if (sources) for (const ref of unresolved) if (sources.has(ref)) unresolved.delete(ref);
            }
          }
          if (unresolved.size) return;
          if (functionalUse) functionUses.push({ effect: effect as unknown as CommentaryEffect, lossOffset: supportLossOffset });
        }
      }
    } catch (_) { return; }
  }
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
      !exactKeys(raw, ['id', 'root_occurrence_id', 'occurrence_ids', 'line_ids', 'candidate_ids']) ||
      !id(raw.id) ||
      !id(raw.root_occurrence_id) ||
      !occurrences.has(raw.root_occurrence_id) ||
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
      (comparisons[0].detail.type === 'question_comparison' && comparisons[0].selection !== 'selected')
    )
      return;
    scenes.push({
      id: raw.id,
      rootOccurrenceId: raw.root_occurrence_id,
      occurrenceIds: raw.occurrence_ids,
      lineIds: raw.line_ids,
      candidateIds: raw.candidate_ids,
    });
  }
  if (new Set(scenes.map(s => s.id)).size !== scenes.length) return;
  if (candidates.some(candidate => candidate.detail.type === 'question_comparison' &&
    candidate.selection === 'selected' && !scenes.some(scene => scene.candidateIds.includes(candidate.id)))) return;
  const responseFields = {
    ...(purposeQuestion === undefined ? {} : { purpose_question: purposeQuestion }),
    ...(preparationQuestion === undefined ? {} : { preparation_question: preparationQuestion }),
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
            detail && targetResponseComplete(detail, observations) &&
            detail?.response_inquiry_id === inquiry.id &&
            detail.branches[0]?.reply_move === inquiry.reference_move &&
            detail.branches[1]?.reply_move === inquiry.alternative_move
          );
        }),
    )
  )
    return;
  if (!proposalFactSourcesValid(proposals ?? [], observations, continuationInquiries ?? [])) return;
  if (value.inquiries === undefined) return { version: 2, candidates, scenes, ...responseFields };
  try {
    const inquiries = decodeInquiries(value.inquiries, observations, context, candidates);
    return inquiries ? { version: 2, candidates, scenes, inquiries, ...responseFields } : undefined;
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
