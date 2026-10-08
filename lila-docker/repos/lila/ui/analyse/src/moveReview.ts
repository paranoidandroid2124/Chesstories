import { isMoveReviewEngineProfile, type MoveReviewEngineProfile } from 'lib/ceval/types';
import { threatSetup } from 'lib/ceval/util';
import { makeFen, parseFen } from 'chessops/fen';
import { makeSan, makeSanAndPlay } from 'chessops/san';
import { parseSquare, parseUci } from 'chessops/util';
import { setupPosition } from 'chessops/variant';
import { standardUci } from './util';
import {
  decodeCommentaryEvaluation,
  decodeCommentarySelection,
  continuationRoutePoolMatches,
  responsePoolMatchesClasses,
  reviewedMoverResponsePoolMatches,
  responseSearchMatchesInquiry,
  reserveResponsePoolMatches,
  commentarySearchAnchorPath,
  type CommentaryEvaluation,
  type CommentarySelection,
} from './moveReviewSelection';

const moveReviewJobRequestSchema = 'chesstory.position-commentary.job-request.v6' as const;
const moveReviewJobStatusSchema = 'chesstory.position-commentary.job-status.v6' as const;
const moveReviewResponseSchema = 'chesstory.position-commentary.response.v6' as const;
const moveReviewEngineWorkReportSchema = 'chesstory.position-commentary.engine-work-report.v6' as const;
const moveReviewAnnotationPolicyRevision = 'chesstory.verdict-threshold-policy.v2' as const;

export type MoveReviewMode = 'off' | 'runtime';
export type MoveReviewLocale = 'ko-KR' | 'en-US';
export type MoveReviewVerdictSymbol = 'none' | '?!' | '?' | '??';
export type MoveReviewVerdictCode =
  | 'improves_on_reference'
  | 'matches_reference'
  | 'playable_loss'
  | 'inaccuracy'
  | 'mistake'
  | 'blunder';
export type MoveReviewCandidateRole = 'best' | 'played' | 'alternative';

export interface MoveReviewSubject {
  variant: 'standard';
  initialFen: FEN;
  movePrefixUci: Uci[];
  before: { path: Tree.Path; fen: FEN };
  played: { uci: Uci; san: San };
  after: { path: Tree.Path; fen: FEN };
  inputLines?: { source: MoveReviewLineSource; moves: Uci[] }[];
}

export interface MoveReviewRecord {
  key: string;
  subject: MoveReviewSubject;
  subjects?: readonly MoveReviewSubject[];
}

export type MoveReviewLineSource = 'pgn_record' | 'analysis' | 'study';
type MoveReviewInputIssue = {
  inputLineIndex: number;
  acceptedPlyCount: number;
  reason: 'invalid_continuation' | 'automatic_terminal' | 'input_limit';
};

interface MoveReviewWinChance {
  referencePercent: number;
  playedPercent: number;
  changePercentagePoints: number;
}

interface MoveReviewCoreFields {
  verdictRef: string;
  verdictSymbol: MoveReviewVerdictSymbol;
  playedUci: Uci;
  bestUci: Uci;
  winChance?: MoveReviewWinChance;
  referenceTerminal?: MoveReviewAutomaticTerminal;
  reviewedTerminal?: MoveReviewAutomaticTerminal;
}

export type MoveReviewCore = MoveReviewCoreFields &
  (
    | { kind: 'move-verdict'; verdictCode: MoveReviewVerdictCode }
    | {
        kind: 'best-choice';
        bestChoice: {
          runnerUpVerdictCode: Exclude<MoveReviewVerdictCode, 'improves_on_reference'>;
          runnerUpUci: Uci;
        };
      }
  );

interface MoveReviewProofMove {
  uci: Uci;
  label: string;
  fenAfter: FEN;
}

export interface MoveReviewProof {
  id: string;
  startFen: FEN;
  moves: MoveReviewProofMove[];
}

type ObservedPieceRole = 'pawn' | 'knight' | 'bishop' | 'rook' | 'queen' | 'king';
export interface ObservedPiece {
  side: Color;
  role: ObservedPieceRole;
  square: Key;
}

interface PieceControlPoint {
  controller: ObservedPiece | null;
  target: ObservedPiece | null;
  state: 'present' | 'absent' | 'unknown' | 'removed';
  premiseIds: string[];
}

type LocatedPiece = Pick<ObservedPiece, 'role' | 'square'>;
export interface LegalResource {
  movement: {
    side: Color;
    from: Key;
    to: Key;
    before_role: ObservedPieceRole;
    after_role: ObservedPieceRole;
  };
  move_uci: Uci;
  capture: ObservedPiece | null;
}

export type MovementResource = {
  destination: Key;
  target: { kind: 'empty' } | { kind: 'friendly' | 'enemy'; role: ObservedPieceRole };
} & (
  | { mode: 'controlled_destination' | 'pawn_advance' | 'pawn_double_advance' }
  | { mode: 'en_passant_capture'; captured_pawn: ObservedPiece }
);

interface RestrictedRecapture {
  piece: LocatedPiece;
  resource: MovementResource;
  king_square: Key;
  post_move_controllers: LocatedPiece[];
  absolute_pin_paths: {
    pinner: LocatedPiece;
    pinned: LocatedPiece;
    king_square: Key;
    geometry: { direction: { file_step: number; rank_step: number }; squares: Key[] };
  }[];
}

type CheckResponseMode = 'king_move' | 'capture_checker' | 'interpose';
interface CheckResponse {
  resource: LegalResource;
  modes: CheckResponseMode[];
}

export interface PawnState {
  side: Color;
  square: Key;
  doubled: boolean;
  isolated: boolean;
  passed: boolean;
  geometrically_protected_passer: boolean;
  front_square: Key | null;
  front_occupant: ObservedPiece | null;
  component_pawns: Key[];
  connections: {
    peer: Key;
    kind: 'geometric_support' | 'phalanx';
    direction: 'supports' | 'supported_by' | 'peer';
  }[];
  enemy_pawn_contacts: Key[];
}

type PawnFacet =
  | 'doubled'
  | 'isolated'
  | 'passed'
  | 'geometrically_protected_passer'
  | 'front_state'
  | 'component'
  | 'connections'
  | 'enemy_pawn_contacts'
  | 'removed';

export type StructuralSubject =
  | { kind: 'open_file'; file: string }
  | { kind: 'semi_open_file'; side: Color; file: string }
  | {
      kind: 'pawn_tension_created' | 'pawn_tension_resolved' | 'passed_pawn_promoted';
      side: Color;
      from: Key;
      to: Key;
    }
  | { kind: 'passed_pawn_created' | 'passed_pawn_lost'; side: Color; square: Key }
  | {
      kind: 'passed_pawn_advanced' | 'passed_status_created';
      side: Color;
      from: Key;
      to: Key;
      relative_rank: number;
    };

type StructuralChangeKind =
  | 'open_file_established'
  | 'semi_open_file_established'
  | 'pawn_tension_created'
  | 'pawn_tension_resolution'
  | 'passed_pawn_progress'
  | 'passed_pawn_status_removed';

export type MoveReviewObservation = {
  occurrenceId: string;
  side: Color;
  role: ObservedPieceRole;
  from: string;
  to: string;
  premiseIds: string[];
} & (
  | {
      kind: 'capture';
      capturedRole: ObservedPieceRole;
      capturedSquare: string;
      recapture: 'proven' | 'excluded' | 'unknown';
    }
  | { kind: 'development'; centralSquares: string[] }
  | {
      kind: 'piece_control';
      relation: 'support' | 'attack';
      before: PieceControlPoint;
      after: PieceControlPoint;
    }
  | {
      kind: 'recapture_resources';
      phase: 'after';
      captured: ObservedPiece;
      geometric_recapturers: LocatedPiece[];
      legal_recaptures: LegalResource[];
      restricted_recaptures: RestrictedRecapture[];
    }
  | {
      kind: 'check_responses';
      phase: 'after';
      checked_side: Color;
      king_square: Key;
      checkers: LocatedPiece[];
      responses: CheckResponse[];
      controlled_king_destinations: { resource: MovementResource; controllers: LocatedPiece[] }[];
      terminal: 'ongoing' | 'checkmate';
    }
  | {
      kind: 'check_response';
      phase: 'before';
      responding_side: Color;
      king_square: Key;
      checkers: LocatedPiece[];
      response: CheckResponse;
    }
  | {
      kind: 'pawn_structure';
      phase: 'transition';
      before: PawnState | null;
      after: PawnState | null;
      changed_facets: PawnFacet[];
    }
  | { kind: 'stalemate'; phase: 'after'; stalled_side: Color; king_square: Key }
  | { kind: 'castling'; rook: { from: Key; to: Key } }
  | { kind: 'promotion'; promoted_role: ObservedPieceRole }
  | {
      kind: 'structural_change';
      phase: 'transition';
      changes: { kind: StructuralChangeKind; subjects: StructuralSubject[]; targets: StructuralSubject[] }[];
    }
);

interface HighlightFrame {
  phase: 'before' | 'after';
  positionId: string;
  squares: Key[];
  sourceIds: string[];
}

export interface MoveReviewObservations {
  /** These references are local to this admitted response, never a global FEN catalogue. */
  owner?: { requestId: string; jobId: string };
  selection?: CommentarySelection;
  statements: MoveReviewObservation[];
  highlights?: ({ occurrenceId: string; text: string; squares: Key[] } & (
    | { premiseIds?: never; frames?: never }
    | {
        premiseIds: string[];
        frames: HighlightFrame[];
      }
  ))[];
  positions?: PositionResources[];
  paths: (MoveReviewProof & {
    kind: 'played' | 'analysis' | 'history';
    occurrenceIds: (string | null)[];
    positionIds?: (string | null)[];
    lineSummary?: ObservedLineSummary;
    sources?: MoveReviewLineSource[];
    evaluations?: CommentaryEvaluation[];
    conditionalEvaluations?: {
      searchPositionId: string;
      plyOffset: number;
      evaluation: CommentaryEvaluation;
    }[];
  })[];
  inputIssues?: MoveReviewInputIssue[];
}

export interface PositionResources {
  // A source ID identifies a fact only together with this historical position ID.
  id: string;
  fen: FEN;
  premise_id: string;
  controls: {
    complete: boolean;
    pieces: { piece: ObservedPiece; squares: Key[]; source_ids: string[] }[];
  };
  rays: {
    complete: boolean;
    lines: { piece: ObservedPiece; squares: Key[]; occupants: ObservedPiece[]; source_id: string }[];
  };
  legal_moves: {
    complete: boolean;
    // Castling UCI can name the rook square; destination is the actual mover square.
    moves: { uci: Uci; destination: Key; captured_square?: Key; gives_check?: boolean; source_id: string }[];
  };
  pawn_states: {
    complete: boolean;
    pawns: {
      square: Key;
      passed: boolean | null;
      isolated: boolean | null;
      doubled: boolean | null;
      source_ids: string[];
    }[];
  };
  pawn_files: {
    complete: boolean;
    files: {
      file: string;
      occupancy: 'open' | 'white_pawns_only' | 'black_pawns_only' | 'both_sides';
      source_ids: string[];
    }[];
  };
  pawn_advances: {
    complete: boolean;
    paths: {
      piece: ObservedPiece;
      destination: Key;
      traversed_squares: Key[];
      source_id: string;
    }[];
  };
}

interface ObservedLineLocation {
  ply_offset: number;
  occurrence_id: string;
  move_uci: Uci;
}

export interface ObservedLineSummary {
  scope: 'observed_line';
  perspective: Color;
  premise_ids: string[];
  events: (ObservedLineLocation & {
    kind:
      | 'capture'
      | 'recapture'
      | 'check_evasion'
      | 'castling'
      | 'check'
      | 'mate'
      | 'stalemate'
      | 'promotion';
    side?: Color;
    piece_role?: ObservedPieceRole;
    target_role?: ObservedPieceRole;
    square?: Key;
  })[];
  material?: {
    events: (ObservedLineLocation & {
      side: Color;
      from: Key;
      to: Key;
      before_role: ObservedPieceRole;
      after_role: ObservedPieceRole;
      capture?: {
        role: ObservedPieceRole;
        square: Key;
        value_cp: number;
        recapture: 'proven' | 'excluded' | 'unknown';
      };
      promotion_gain_cp: number;
      delta_cp: number;
      balance_cp: number;
    })[];
    net_cp: number;
    max_gain_cp: number;
    max_loss_cp: number;
    recovered_after_loss: boolean;
  };
  ending: { kind: 'line_end' | MoveReviewAutomaticTerminal['kind']; ply_offset: number; occurrence_id: string };
}

export interface MoveReviewCandidate {
  uci: Uci;
  label: string;
  roles: MoveReviewCandidateRole[];
  winPercent?: number;
  review: MoveReviewCandidateReview;
}

export type MoveReviewCandidateReview =
  | {
      kind: 'move-verdict';
      core: MoveReviewCore;
      comparisonProofs: MoveReviewProof[];
    }
  | { kind: 'single-candidate-insight'; proof: MoveReviewProof }
  | { kind: 'forced-single-move'; lineUcis: Uci[]; terminal?: MoveReviewAutomaticTerminal }
  | { kind: 'abstained' };
export interface MoveReviewEvidence {
  candidates: MoveReviewCandidate[];
  drawClaims?: MoveReviewDrawClaim[];
}

export type MoveReviewAutomaticTerminal =
  | { kind: 'checkmate'; winner: 'white' | 'black' }
  | { kind: 'stalemate' | 'insufficient_material' | 'fivefold_repetition' | 'seventy_five_move_rule' };

export interface MoveReviewDrawClaim {
  rule: 'threefold_repetition' | 'fifty_move_rule';
  availability: 'available_now' | 'available_by_declared_move';
  moveUcis?: Uci[];
}

export type MoveReviewPositionAction =
  | { kind: 'automatic-terminal'; terminal: MoveReviewAutomaticTerminal }
  | { kind: 'draw-claim'; claims: MoveReviewDrawClaim[] };

export interface IssuedMoveReviewEngineWork {
  engineProfile: MoveReviewEngineProfile;
  workId: string;
  purpose: 'root_search' | 'focus_comparison' | 'investigation_comparison' | 'response_comparison' | 'threat_probe';
  executionKeySha256: string;
  variant: 'standard';
  enginePositionInitialFen: FEN;
  enginePositionMovesUci: Uci[];
  searchFen: FEN;
  searchAnchor?: { positionId: string; rootMove: Uci; rootOccurrenceId: string; prefixMovesUci: Uci[] };
  threatOriginPrefixUci?: Uci[];
  rootRestriction: { kind: 'unrestricted' } | { kind: 'restricted'; movesUci: Uci[] };
  searchLimits: {
    depth: number;
    nodes: 250_000_000 | 40_000_000 | 20_000_000 | 5_000_000 | 2_000_000 | 250_000;
    movetimeMs: 120_000 | 8_000 | 5_000 | 2_500 | 750;
    multiPv: number;
  };
  maxSearchElapsedMs: number;
}

export type MoveReviewSearchScope = Pick<
  IssuedMoveReviewEngineWork,
  | 'enginePositionInitialFen'
  | 'enginePositionMovesUci'
  | 'searchFen'
  | 'searchAnchor'
  | 'rootRestriction'
  | 'searchLimits'
  | 'maxSearchElapsedMs'
>;

type MoveReviewWhiteScore = { kind: 'cp'; value: number } | { kind: 'mate'; value: number };

interface MoveReviewLineSuffix {
  moves: Uci[];
  depth: number;
  whiteScore: MoveReviewWhiteScore;
}

export type MoveReviewEngineFailure =
  | 'cancelled'
  | 'lease_expired'
  | 'search_budget_exhausted'
  | 'engine_failure'
  | 'engine_unavailable'
  | 'browser_unsupported';

export type MoveReviewEngineOutcome =
  | {
      kind: 'completed';
      completedDepth: number;
      selectiveDepth: number;
      nodes: number;
      engineTimeMs: number;
      executorElapsedMs: number;
      bestmoveUci: Uci;
      lineSuffixes: MoveReviewLineSuffix[];
    }
  | {
      kind: 'executor_failed';
      executorElapsedMs: number;
      observedNodes: number;
      engineTimeMs: number;
      failureCode: MoveReviewEngineFailure;
      diagnostic: string;
    };

export interface MoveReviewVerification {
  state: 'pending' | 'complete' | 'stopped' | 'not_required';
  targetDepth: number;
  reason: 'initial_verification' | 'child_challenge';
  triggerWorkIds: string[];
  completedDepth?: number;
  stopReason?: string;
}

interface MoveReviewSnapshotCommon {
  requestId: string;
  jobId: string;
  engineProfile: MoveReviewEngineProfile;
  judgmentRevision: string;
  annotationPolicyRevision: string;
  subject: MoveReviewSubject;
  observations?: MoveReviewObservations;
  assessmentState?: 'current' | 'needs_refresh';
  assessmentScope?: 'root_search' | 'investigation_pair';
  verification?: MoveReviewVerification;
}

export type MoveReviewAssessment =
  | { kind: 'completed'; evidence: MoveReviewEvidence }
  | { kind: 'position-action'; action: MoveReviewPositionAction };

export type MoveReviewStopCondition =
  | 'deadline_exceeded'
  | 'cancelled'
  | 'search_budget_exhausted'
  | 'engine_execution_failed'
  | 'invalid_engine_work_report'
  | 'review_construction_failed'
  | 'move_review_preparation_failed'
  | 'repetition_history_unavailable'
  | 'engine_unavailable'
  | 'browser_unsupported';

export type MoveReviewSnapshot =
  | (MoveReviewSnapshotCommon & { kind: 'awaiting-core'; issuedEngineWork: IssuedMoveReviewEngineWork })
  | (MoveReviewSnapshotCommon & {
      kind: 'awaiting-evidence';
      issuedEngineWork: IssuedMoveReviewEngineWork;
      assessment?: MoveReviewAssessment;
    })
  | (MoveReviewSnapshotCommon & { kind: 'completed'; evidence: MoveReviewEvidence })
  | (MoveReviewSnapshotCommon & { kind: 'position-action'; action: MoveReviewPositionAction })
  | (MoveReviewSnapshotCommon & { kind: 'abstained'; stopCondition: MoveReviewStopCondition });

export type MoveReviewJobState = (
  | { kind: 'idle'; reason: 'root' | 'disabled' }
  | { kind: 'unsearched'; subject: MoveReviewSubject; observationsPending: boolean }
  | { kind: 'loading'; subject: MoveReviewSubject }
  | { kind: 'completed'; snapshot: Extract<MoveReviewSnapshot, { kind: 'completed' }> }
  | { kind: 'position-action'; snapshot: Extract<MoveReviewSnapshot, { kind: 'position-action' }> }
  | { kind: 'abstained'; subject: MoveReviewSubject; stopCondition: MoveReviewStopCondition }
  | { kind: 'fault'; subject: MoveReviewSubject; message: string; retryable: boolean }
) & {
  observations?: MoveReviewObservations;
  assessment?: MoveReviewAssessment;
  assessmentState?: MoveReviewSnapshotCommon['assessmentState'];
  assessmentScope?: MoveReviewSnapshotCommon['assessmentScope'];
  verification?: MoveReviewVerification;
  observationFrame?: MoveReviewFrameSelection | null;
  recordReview?: {
    status?: 'running' | 'finished' | 'paused' | 'blocked';
    total: number;
    /** Includes previews and assessments retained after a stop; overlaps completed and stopped. */
    assessed: number;
    /** Ended jobs with no interrupted assessment verification; individual questions may remain unanswered. */
    completed: number;
    stopped: number;
    scenes: { path: Tree.Path; label: string }[];
  };
};

export function moveReviewObservations(job: MoveReviewJobState): MoveReviewObservations | undefined {
  return (
    job.observations ??
    (job.kind === 'completed' || job.kind === 'position-action' ? job.snapshot.observations : undefined)
  );
}

export interface MoveReviewFrameSelection {
  proofId: string;
  ply: number;
  phase?: 'before' | 'after';
  observationKey?: string;
  highlightKey?: string;
  inspectSquare?: Key;
  textAnchor?: string;
  choosePosition?: boolean;
}

export interface MoveReviewViewState {
  selectedCandidateUci?: Uci;
  evidenceExpanded: boolean;
  expandedProofId?: string;
  hoveredFrame?: MoveReviewFrameSelection;
  pinnedFrame?: MoveReviewFrameSelection;
  boardFrame?: MoveReviewFrameSelection;
}

export function updateMoveReviewViewForJob(
  view: MoveReviewViewState,
  previous: MoveReviewJobState,
  next: MoveReviewJobState,
): void {
  const before = moveReviewObservations(previous)?.owner, after = moveReviewObservations(next)?.owner;
  if (before?.requestId === after?.requestId && before?.jobId === after?.jobId) return;
  // A retry can reuse the same subject, line IDs and plies for a different runtime job.
  view.hoveredFrame = view.pinnedFrame = view.boardFrame = undefined;
}

export interface MoveReviewCopy {
  title: string;
  idleTitle: string;
  idleBody: string;
  analysing: string;
  completed: string;
  liveEnginePaused: string;
  unavailable: string;
  unsupported: string;
  retry: string;
  gradeNotRequested: string;
  requestGrade: string;
  observationsLoading: string;
  observationsMissing: string;
  best: string;
  played: string;
  alternative: string;
  candidateMoves: string;
  lineInsight: string;
  forcedSingleMove: string;
  candidateUnavailable: string;
  analysisStopped: string;
  stopReasons: Record<MoveReviewStopCondition, string>;
  observationsTitle: string;
  workTitle: string;
  comparisonUnavailable: string;
  pathLabels: Record<'played' | 'analysis' | 'history', string>;
  lineSourceLabels: Record<MoveReviewLineSource, string>;
  inputIssueText: Record<MoveReviewInputIssue['reason'], string>;
  roleLabels: Record<ObservedPieceRole, string>;
  captureText: string;
  recaptureText: string;
  recaptureUnknown: string;
  developmentText: string;
  centralControlText: string;
  frameLabels: Record<'before' | 'after', string>;
  controlRelationLabels: Record<'support' | 'attack', string>;
  controlStateLabels: Record<PieceControlPoint['state'], string>;
  controlChangeLabels: Record<'created' | 'retained' | 'lost' | 'removed', string>;
  illegalRecaptureText: string;
  removedPiece: string;
  observationDetails: string;
  resourceLabels: Record<
    | 'square'
    | 'choose'
    | 'controls'
    | 'outgoingControls'
    | 'legalMoves'
    | 'noLegalMoves'
    | 'pawnState'
    | 'rookFile'
    | 'openFile'
    | 'semiOpenFile'
    | 'ownPawnFile'
    | 'unconfirmed'
    | 'friendlySupport'
    | 'capturedOn'
    | 'opposingControl'
    | 'rays'
    | 'advances'
    | 'first'
    | 'behind'
    | 'path'
    | 'none'
    | 'partial'
    | 'unavailable'
    | 'legal'
    | 'illegal'
    | 'not_turn'
    | 'unknown'
    | 'relationships',
    string
  >;
  lineLabels: Record<
    | 'title'
    | 'material'
    | 'units'
    | 'delta'
    | 'balance'
    | 'net'
    | 'maximum'
    | 'minimum'
    | 'recovered'
    | 'noMaterialEvents'
    | 'missingMaterial'
    | 'missingSummary'
    | 'limit'
    | 'line_end'
    | 'checkmate'
    | 'stalemate'
    | 'promotionGain'
    | 'value',
    string
  >;
  lineEventLabels: Record<ObservedLineSummary['events'][number]['kind'], string>;
  lineEventText: Record<Exclude<ObservedLineSummary['events'][number]['kind'], 'capture'>, string>;
  observationLabels: Record<
    Exclude<MoveReviewObservation['kind'], 'capture' | 'development' | 'piece_control'>,
    string
  >;
  observationFields: Record<
    | 'none'
    | 'yes'
    | 'no'
    | 'geometric'
    | 'legal'
    | 'restricted'
    | 'captured'
    | 'checkers'
    | 'king'
    | 'relatedTargets'
    | 'pins'
    | 'controlledDestinations'
    | 'frontSquare'
    | 'frontOccupant'
    | 'component'
    | 'connections'
    | 'enemyPawnContacts'
    | 'ongoing'
    | 'empty'
    | 'friendly'
    | 'enemy'
    | 'relativeRank',
    string
  >;
  checkModeLabels: Record<CheckResponseMode, string>;
  movementModeLabels: Record<MovementResource['mode'], string>;
  pawnFacetLabels: Record<PawnFacet, string>;
  pawnConnectionLabels: Record<PawnState['connections'][number]['direction'], string>;
  structuralChangeLabels: Record<StructuralChangeKind, string>;
  structuralSubjectLabels: Record<StructuralSubject['kind'], string>;
  stalemateText: string;
  selectedReference: string;
  terminalResult: string;
  drawClaimAvailable: string;
  terminalLabels: Record<MoveReviewAutomaticTerminal['kind'], string>;
  drawRuleLabels: Record<MoveReviewDrawClaim['rule'], string>;
  colorLabels: Record<'white' | 'black', string>;
  drawAvailabilityLabels: Record<MoveReviewDrawClaim['availability'], string>;
  runnerUp: string;
  proofStep: string;
  addToStudy: string;
  addedToStudy: string;
  viewAddedLine: string;
  showEvidence: string;
  hideEvidence: string;
  proofBoard: string;
  winChance: string;
  winChanceChange: string;
  verdictLabels: Record<MoveReviewVerdictSymbol, string>;
  verdictCodeLabels: Record<MoveReviewVerdictCode, string>;
}

interface MoveReviewDecodeContext {
  requestId: string;
  subject: MoveReviewSubject;
  engineProfile?: MoveReviewEngineProfile;
  jobId?: string;
}

// The backend's maximum job lifetime is a safety limit, not a shared game budget or analysis target.
export const moveReviewBudgets = { singleMs: 10_000, jobSafetyMs: 180_000 } as const;

export interface MoveReviewSourceRequest {
  requestId: string;
  subject: MoveReviewSubject;
  engineProfile: MoveReviewEngineProfile;
  /** Fixed job deadline on the browser's monotonic clock, including server queue and transport. */
  deadlineAtMs?: number;
}

export interface MoveReviewSource {
  run(
    request: MoveReviewSourceRequest,
    emit: (snapshot: MoveReviewSnapshot) => void,
    signal: AbortSignal,
  ): Promise<void>;
}

const verdictSymbolByCode: Record<MoveReviewVerdictCode, MoveReviewVerdictSymbol> = {
  improves_on_reference: 'none',
  matches_reference: 'none',
  playable_loss: 'none',
  inaccuracy: '?!',
  mistake: '?',
  blunder: '??',
};

const copies: Record<MoveReviewLocale, MoveReviewCopy> = {
  'ko-KR': {
    title: '직전 수 리뷰',
    idleTitle: '아직 리뷰할 수가 없습니다',
    idleBody: '수를 두거나 기보에서 수를 선택하세요.',
    analysing: '직전 수를 분석하고 있습니다…',
    completed: '직전 수 리뷰가 완료되었습니다.',
    liveEnginePaused: '리뷰가 끝날 때까지 실시간 엔진을 잠시 멈췄습니다.',
    unavailable: '직전 수 리뷰를 불러오지 못했습니다.',
    unsupported: '엔진 분석을 시작하지 못했습니다.',
    retry: '다시 시도',
    gradeNotRequested: '이 수의 평가는 요청하지 않았습니다.',
    requestGrade: '이 수 평가하기',
    observationsLoading: '입력된 수순의 관찰을 불러오고 있습니다…',
    observationsMissing: '이 수의 입력 수순 관찰은 아직 없습니다.',
    best: '초기 검색 1순위',
    played: '둔 수',
    alternative: '대안',
    candidateMoves: '후보 수',
    lineInsight: '검증된 후보 수순',
    forcedSingleMove: '강제된 단일 수',
    candidateUnavailable: '이 후보 수의 분석이 보류되었습니다.',
    analysisStopped: '분석 중단',
    observationsTitle: '확인된 수의 내용',
    workTitle: '기물의 역할',
    comparisonUnavailable: '비교 판정이 제공되지 않았습니다.',
    pathLabels: { played: '선택한 수', analysis: '분석 가지', history: '직전 수부터' },
    lineSourceLabels: {
      pgn_record: '입력 기보의 대국 수순',
      analysis: '입력된 분석 가지',
      study: 'Study 수순 · 실전 여부 미확인',
    },
    inputIssueText: {
      invalid_continuation:
        '입력 수순 {line}: {ply}수까지 보존했습니다. 그다음 수의 연속성을 검증하지 못했습니다.',
      automatic_terminal: '입력 수순 {line}: {ply}수에서 대국이 자동 종료되어 이후 수를 제외했습니다.',
      input_limit: '입력 수순 {line}: 수순 처리 한도에 도달하여 {ply}수까지 보존했습니다.',
    },
    roleLabels: { pawn: '폰', knight: '나이트', bishop: '비숍', rook: '룩', queen: '퀸', king: '킹' },
    captureText: '{side} {role}: {square}의 상대 {captured} 포획.',
    recaptureText: '직전에 포획한 상대 기물을 되잡습니다.',
    recaptureUnknown: '이번 포획이 재포획인지는 확인되지 않았습니다.',
    developmentText: '{side} {role}: 시작 칸 {from}을 처음 떠나 {to}로 이동합니다.',
    centralControlText: '이 위치에서 중앙의 {squares} 칸을 지배합니다.',
    frameLabels: { before: '수 이전', after: '수 이후' },
    controlRelationLabels: { support: '기하적 지원', attack: '기하적 공격' },
    controlStateLabels: { present: '성립', absent: '성립하지 않음', unknown: '미확인', removed: '기물 제거' },
    controlChangeLabels: {
      created: '관계가 생겼습니다',
      retained: '관계가 유지됩니다',
      lost: '관계가 사라졌습니다',
      removed: '관계가 기물 제거로 끝났습니다',
    },
    removedPiece: '제거된 기물',
    observationDetails: '폰·구조 변화 상세 ({count})',
    resourceLabels: {
      square: '살펴볼 칸',
      choose: '칸 선택',
      controls: '이 칸을 통제하는 기물',
      outgoingControls: '선택 기물이 통제하는 칸',
      legalMoves: '선택 기물의 합법적인 이동',
      noLegalMoves: '이 기물은 현재 합법적인 이동이 없습니다.',
      pawnState: '현재 폰 상태',
      rookFile: '이 룩이 놓인 파일',
      openFile: '열린 파일: 양측 폰이 없습니다.',
      semiOpenFile: '이 룩 쪽에 반열린 파일: 상대 폰만 있습니다.',
      ownPawnFile: '이 룩 쪽의 폰이 있는 파일입니다.',
      unconfirmed: '미확인',
      friendlySupport: '같은 편 기물의 기하적 지원',
      capturedOn: '포획되는 칸',
      opposingControl: '상대 기물의 통제',
      rays: '관련 광선',
      advances: '관련 폰 전진 경로',
      first: '첫 장애물',
      behind: '그 뒤의 기물 (거리순)',
      path: '경로',
      none: '없음',
      partial: '일부 근거만 확인됨. 다른 자원의 부재는 확인되지 않았습니다.',
      unavailable: '이 위치의 근거가 없습니다.',
      legal: '합법적인 이동',
      illegal: '현재 합법적이지 않은 이동',
      not_turn: '이 기물 쪽의 차례가 아닙니다. 합법성은 판정하지 않았습니다.',
      unknown: '합법성 미확인',
      relationships: '기물 관계',
    },
    lineLabels: {
      title: '이 수순에서 관찰된 결과',
      material: '물질 변화',
      units: '수순 시작 대비 {side}의 물질 변화입니다. 폰 하나를 1로 환산하며 엔진 평가가 아닙니다.',
      delta: '이번 변화',
      balance: '누적 변화',
      net: '끝에서의 변화',
      maximum: '가장 높았던 변화',
      minimum: '가장 낮았던 변화',
      recovered:
        '이 수순 중 손실 이후 누적 변화가 0 이상으로 돌아온 시점이 있습니다. 이후 다시 잃을 수 있습니다.',
      noMaterialEvents: '이 수순에 물질 변화 사건이 없습니다.',
      missingMaterial: '이 수순의 물질 변화 근거가 없습니다.',
      missingSummary: '이 수순의 결과 근거가 없습니다.',
      limit:
        '표시한 수순 안에서의 결과입니다. 최선 응수 아래 강제되는 이득이나 영구적 이득을 뜻하지 않습니다.',
      line_end: '여기서 입력·분석된 수순이 끝납니다.',
      checkmate: '수순 끝에서 체크메이트입니다.',
      stalemate: '수순 끝에서 스테일메이트입니다.',
      promotionGain: '승격에 의한 물질 증가',
      value: '기물 가치',
    },
    lineEventLabels: {
      capture: '포획',
      recapture: '즉시 재포획',
      check_evasion: '체크 응수',
      castling: '캐슬링',
      check: '체크',
      mate: '체크메이트',
      stalemate: '스테일메이트',
      promotion: '승격',
    },
    lineEventText: {
      recapture: '{side} {role}: {square}의 상대 {captured} 재포획.',
      check_evasion: '{side}: 체크를 해소합니다.',
      castling: '{side}: 캐슬링하여 킹을 {square}에 둡니다.',
      check: '{side}: {square}의 상대 킹에게 체크.',
      mate: '{side}: {square}의 상대 킹을 체크메이트합니다.',
      stalemate: '{side}: {square}의 상대 킹을 스테일메이트 상태로 만듭니다.',
      promotion: '{side}: {square}에서 {captured} 승격.',
    },
    observationLabels: {
      recapture_resources: '재포획 자원',
      check_responses: '체크와 합법 응수',
      check_response: '선택한 체크 응수',
      pawn_structure: '폰 구조 변화',
      stalemate: '스테일메이트',
      castling: '캐슬링',
      promotion: '승격',
      structural_change: '구조 변화 근거',
    },
    illegalRecaptureText:
      '{piece}의 {square} 재포획은 {king}의 킹을 {controllers}의 공격에 노출하므로 합법적인 수가 아닙니다.',
    observationFields: {
      none: '없음',
      yes: '예',
      no: '아니오',
      geometric: '기하적 재포획 기물',
      legal: '합법 응수',
      restricted: '제한된 응수',
      captured: '잡힌 기물',
      checkers: '체크 기물',
      king: '킹',
      relatedTargets: '관련 대상',
      pins: '절대 핀 경로',
      controlledDestinations: '통제된 킹 이동칸',
      frontSquare: '앞칸',
      frontOccupant: '앞칸 기물',
      component: '연결된 폰 집합',
      connections: '폰 연결',
      enemyPawnContacts: '상대 폰 접점',
      ongoing: '체크메이트 아님',
      empty: '빈 칸',
      friendly: '자기 기물',
      enemy: '상대 기물',
      relativeRank: '자기 진영 기준 랭크',
    },
    checkModeLabels: { king_move: '킹 이동', capture_checker: '체크 기물 포획', interpose: '사이막기' },
    movementModeLabels: {
      controlled_destination: '기하적으로 통제한 칸',
      pawn_advance: '폰 전진',
      pawn_double_advance: '폰 두 칸 전진',
      en_passant_capture: '앙파상 포획',
    },
    pawnFacetLabels: {
      doubled: '더블폰',
      isolated: '고립폰',
      passed: '통과폰',
      geometrically_protected_passer: '기하적으로 지원받는 통과폰',
      front_state: '앞칸 상태',
      component: '연결된 폰 집합',
      connections: '폰 연결',
      enemy_pawn_contacts: '상대 폰 접점',
      removed: '폰 제거',
    },
    pawnConnectionLabels: {
      supports: '기하적으로 지원함',
      supported_by: '기하적 지원을 받음',
      peer: '나란히 위치함',
    },
    structuralChangeLabels: {
      open_file_established: '열린 파일 생성',
      semi_open_file_established: '반열린 파일 생성',
      pawn_tension_created: '폰 긴장 생성',
      pawn_tension_resolution: '폰 긴장 해소',
      passed_pawn_progress: '통과폰 변화',
      passed_pawn_status_removed: '통과폰 지위 상실',
    },
    structuralSubjectLabels: {
      open_file: '열린 파일',
      semi_open_file: '반열린 파일',
      pawn_tension_created: '폰 긴장 생성',
      pawn_tension_resolved: '폰 긴장 해소',
      passed_pawn_created: '통과폰 생성',
      passed_pawn_lost: '통과폰 상실',
      passed_pawn_advanced: '통과폰 전진',
      passed_status_created: '통과폰 지위 생성',
      passed_pawn_promoted: '통과폰 승격',
    },
    stalemateText: '{side} 킹 {square}: 체크 상태가 아니며 합법 수가 없습니다. 스테일메이트입니다.',
    stopReasons: {
      engine_unavailable: '엔진 파일을 불러오거나 시작하지 못했습니다.',
      browser_unsupported: '이 브라우저 환경은 필요한 엔진 실행 조건을 충족하지 못합니다.',
      deadline_exceeded: '이 착수의 분석 실행 한도에 도달했습니다.',
      cancelled: '분석이 취소됐습니다.',
      search_budget_exhausted: '이 분석 작업의 실행 한도에 도달했습니다.',
      engine_execution_failed: '엔진 실행 중 오류가 발생했습니다.',
      invalid_engine_work_report: '받은 엔진 결과가 요청한 분석 조건과 맞지 않습니다.',
      review_construction_failed: '받은 자료로 평가 판정을 구성하지 못했습니다.',
      move_review_preparation_failed: '평가를 비교하는 데 필요한 수순을 준비하지 못했습니다.',
      repetition_history_unavailable: '반복 여부를 확인할 이전 기보가 없습니다.',
    },
    selectedReference: '이번 분석에서 가장 높게 평가한 수입니다',
    terminalResult: '종료된 포지션',
    drawClaimAvailable: '무승부 청구 가능',
    terminalLabels: {
      checkmate: '체크메이트',
      stalemate: '스테일메이트',
      insufficient_material: '기물 부족 무승부',
      fivefold_repetition: '5회 동형반복',
      seventy_five_move_rule: '75수 규칙',
    },
    drawRuleLabels: { threefold_repetition: '3회 동형반복', fifty_move_rule: '50수 규칙' },
    colorLabels: { white: '백', black: '흑' },
    drawAvailabilityLabels: {
      available_now: '지금 청구 가능',
      available_by_declared_move: '지정 수로 청구 가능',
    },
    runnerUp: '초기 검색 2순위',
    proofStep: '단계',
    addToStudy: 'Study에 수순 추가',
    addedToStudy: 'Study에 수순을 추가했습니다.',
    viewAddedLine: '추가한 수순 보기',
    showEvidence: '근거 펼치기',
    hideEvidence: '근거 접기',
    proofBoard: '근거 수순 미니보드',
    winChance: '승리 가능성',
    winChanceChange: '검토 수의 기준 대비 변화',
    verdictLabels: { none: '주석 없음', '?!': '의심스러운 수', '?': '실수', '??': '큰 실수' },
    verdictCodeLabels: {
      improves_on_reference: '기준 수보다 나은 수',
      matches_reference: '기준 수와 동등한 수',
      playable_loss: '충분히 둘 만한 수',
      inaccuracy: '부정확한 수',
      mistake: '실수',
      blunder: '큰 실수',
    },
  },
  'en-US': {
    title: 'Last move review',
    idleTitle: 'No move to review yet',
    idleBody: 'Make a move or select one in the notation.',
    analysing: 'Reviewing the last move…',
    completed: 'Last move review complete.',
    liveEnginePaused: 'The live engine is paused until this review finishes.',
    unavailable: 'The last move review is unavailable.',
    unsupported: 'Engine analysis could not start.',
    retry: 'Retry',
    gradeNotRequested: 'This move has not been graded.',
    requestGrade: 'Analyze this move',
    observationsLoading: 'Loading observations for the supplied moves…',
    observationsMissing: 'No supplied observations are available for this move.',
    best: 'Initial search #1',
    played: 'Played',
    alternative: 'Alternative',
    candidateMoves: 'Candidate moves',
    lineInsight: 'Verified candidate line',
    forcedSingleMove: 'Forced single move',
    candidateUnavailable: 'Analysis was withheld for this candidate.',
    analysisStopped: 'Analysis stopped',
    observationsTitle: 'Verified move observations',
    workTitle: 'Piece roles',
    comparisonUnavailable: 'No comparison verdict was provided.',
    pathLabels: { played: 'Selected move', analysis: 'Analysis branch', history: 'From the preceding move' },
    lineSourceLabels: {
      pgn_record: 'Game line in the imported PGN',
      analysis: 'Submitted analysis branch',
      study: 'Study line · game origin unverified',
    },
    inputIssueText: {
      invalid_continuation: 'Input line {line}: retained the valid continuation. The next move could not be admitted.',
      automatic_terminal:
        'Input line {line}: the game ended automatically; later moves were excluded.',
      input_limit: 'Input line {line}: reached the input processing limit; retained the valid continuation.',
    },
    roleLabels: {
      pawn: 'pawn',
      knight: 'knight',
      bishop: 'bishop',
      rook: 'rook',
      queen: 'queen',
      king: 'king',
    },
    captureText: '{side} {role} captures the {captured} on {square}.',
    recaptureText: 'It recaptures the opposing piece that just captured.',
    recaptureUnknown: 'Whether this capture was a recapture is unknown.',
    developmentText: '{side} {role} leaves its starting square {from} for {to} for the first time.',
    centralControlText: 'From here it controls the central squares {squares}.',
    frameLabels: { before: 'Before move', after: 'After move' },
    controlRelationLabels: { support: 'Geometric support', attack: 'Geometric attack' },
    controlStateLabels: {
      present: 'Present',
      absent: 'Absent',
      unknown: 'Unverified',
      removed: 'Piece removed',
    },
    removedPiece: 'Removed piece',
    controlChangeLabels: {
      created: 'relationship created',
      retained: 'relationship retained',
      lost: 'relationship lost',
      removed: 'relationship ended with piece removal',
    },
    observationDetails: 'Pawn and structural details ({count})',
    resourceLabels: {
      square: 'Square to inspect',
      choose: 'Choose a square',
      controls: 'Pieces controlling this square',
      outgoingControls: 'Squares controlled by this piece',
      legalMoves: 'Legal moves of this piece',
      noLegalMoves: 'This piece has no legal move in this position.',
      pawnState: 'Current pawn state',
      rookFile: 'This rook’s file',
      openFile: 'Open file: neither side has a pawn on it.',
      semiOpenFile: 'Semi-open for this rook’s side: only opposing pawns are on it.',
      ownPawnFile: 'This rook’s side has pawns on the file.',
      unconfirmed: 'Not established',
      friendlySupport: 'Geometric support from friendly pieces',
      capturedOn: 'Captured on',
      opposingControl: 'Control by opposing pieces',
      rays: 'Related rays',
      advances: 'Related pawn advance paths',
      first: 'First blocker',
      behind: 'Pieces behind it (nearest first)',
      path: 'Path',
      none: 'None',
      partial: 'Only partial evidence is available. Other resources have not been ruled out.',
      unavailable: 'No evidence is available for this position.',
      legal: 'Legal move',
      illegal: 'Move is not legal in this position',
      not_turn: 'Not this piece’s turn. Legality has not been assessed.',
      unknown: 'Legality unknown',
      relationships: 'Piece relationships',
    },
    lineLabels: {
      title: 'Within this line',
      material: 'Material change',
      units:
        'Material changes for {side}, relative to the line’s start, in pawn units. These are not engine evaluations.',
      delta: 'Change at this move',
      balance: 'Running change',
      net: 'Change at line end',
      maximum: 'Highest running change',
      minimum: 'Lowest running change',
      recovered:
        'After a loss, the running change returned to at least zero within this line. Further losses may follow.',
      noMaterialEvents: 'No material-changing events in this line.',
      missingMaterial: 'Material evidence is unavailable for this line.',
      missingSummary: 'Result evidence is unavailable for this line.',
      limit:
        'These results were observed within the displayed line. They do not establish a forced or lasting gain against best play.',
      line_end: 'The supplied or analyzed line stops here.',
      checkmate: 'The line ends in checkmate.',
      stalemate: 'The line ends in stalemate.',
      promotionGain: 'Material added by promotion',
      value: 'Piece value',
    },
    lineEventLabels: {
      capture: 'Capture',
      recapture: 'Immediate recapture',
      check_evasion: 'Check response',
      castling: 'Castling',
      check: 'Check',
      mate: 'Checkmate',
      stalemate: 'Stalemate',
      promotion: 'Promotion',
    },
    lineEventText: {
      recapture: '{side} {role} recaptures the {captured} on {square}.',
      check_evasion: '{side} gets out of check.',
      castling: '{side} castles, placing the king on {square}.',
      check: '{side} gives check to the king on {square}.',
      mate: '{side} checkmates the king on {square}.',
      stalemate: '{side} stalemates the opposing king on {square}.',
      promotion: '{side} promotes to a {captured} on {square}.',
    },
    observationLabels: {
      recapture_resources: 'Recapture resources',
      check_responses: 'Check and legal responses',
      check_response: 'Selected check response',
      pawn_structure: 'Pawn structure change',
      stalemate: 'Stalemate',
      castling: 'Castling',
      promotion: 'Promotion',
      structural_change: 'Structural evidence',
    },
    illegalRecaptureText:
      'If {piece} recaptures on {square}, {controllers} would attack its king on {king}; this recapture is illegal.',
    observationFields: {
      none: 'None',
      yes: 'Yes',
      no: 'No',
      geometric: 'Geometric recapturers',
      legal: 'Legal responses',
      restricted: 'Restricted responses',
      captured: 'Captured piece',
      checkers: 'Checking pieces',
      king: 'King',
      relatedTargets: 'Related targets',
      pins: 'Absolute pin paths',
      controlledDestinations: 'Controlled king destinations',
      frontSquare: 'Front square',
      frontOccupant: 'Front occupant',
      component: 'Connected pawn group',
      connections: 'Pawn connections',
      enemyPawnContacts: 'Enemy pawn contacts',
      ongoing: 'Not checkmate',
      empty: 'Empty square',
      friendly: 'Friendly piece',
      enemy: 'Enemy piece',
      relativeRank: 'Relative rank',
    },
    checkModeLabels: { king_move: 'King move', capture_checker: 'Capture checker', interpose: 'Interpose' },
    movementModeLabels: {
      controlled_destination: 'Geometrically controlled destination',
      pawn_advance: 'Pawn advance',
      pawn_double_advance: 'Double pawn advance',
      en_passant_capture: 'En passant capture',
    },
    pawnFacetLabels: {
      doubled: 'Doubled',
      isolated: 'Isolated',
      passed: 'Passed',
      geometrically_protected_passer: 'Geometrically protected passer',
      front_state: 'Front square state',
      component: 'Connected pawn group',
      connections: 'Pawn connections',
      enemy_pawn_contacts: 'Enemy pawn contacts',
      removed: 'Pawn removed',
    },
    pawnConnectionLabels: {
      supports: 'Geometrically supports',
      supported_by: 'Geometrically supported by',
      peer: 'In phalanx with',
    },
    structuralChangeLabels: {
      open_file_established: 'Open file established',
      semi_open_file_established: 'Semi-open file established',
      pawn_tension_created: 'Pawn tension created',
      pawn_tension_resolution: 'Pawn tension resolved',
      passed_pawn_progress: 'Passed pawn change',
      passed_pawn_status_removed: 'Passed status removed',
    },
    structuralSubjectLabels: {
      open_file: 'Open file',
      semi_open_file: 'Semi-open file',
      pawn_tension_created: 'Pawn tension created',
      pawn_tension_resolved: 'Pawn tension resolved',
      passed_pawn_created: 'Passed pawn created',
      passed_pawn_lost: 'Passed pawn lost',
      passed_pawn_advanced: 'Passed pawn advanced',
      passed_status_created: 'Passed status created',
      passed_pawn_promoted: 'Passed pawn promoted',
    },
    stalemateText: '{side} king on {square} is not in check and has no legal moves. Stalemate.',
    stopReasons: {
      engine_unavailable: 'The engine files could not be loaded or started.',
      browser_unsupported: 'This browser environment does not meet the engine requirements.',
      deadline_exceeded: 'This move reached its analysis time limit.',
      cancelled: 'Analysis was cancelled.',
      search_budget_exhausted: 'This analysis work reached its execution limit.',
      engine_execution_failed: 'The engine failed during analysis.',
      invalid_engine_work_report: 'The engine result did not match the requested analysis conditions.',
      review_construction_failed: 'The available evidence could not support an evaluation verdict.',
      move_review_preparation_failed: 'The lines needed for the evaluation comparison could not be prepared.',
      repetition_history_unavailable: 'The earlier moves needed to check repetition are unavailable.',
    },
    selectedReference: 'The highest-rated move in this analysis',
    terminalResult: 'Terminal position',
    drawClaimAvailable: 'Draw claim available',
    terminalLabels: {
      checkmate: 'Checkmate',
      stalemate: 'Stalemate',
      insufficient_material: 'Insufficient material',
      fivefold_repetition: 'Fivefold repetition',
      seventy_five_move_rule: 'Seventy-five-move rule',
    },
    drawRuleLabels: { threefold_repetition: 'Threefold repetition', fifty_move_rule: 'Fifty-move rule' },
    colorLabels: { white: 'White', black: 'Black' },
    drawAvailabilityLabels: {
      available_now: 'Available now',
      available_by_declared_move: 'Available by declared move',
    },
    runnerUp: 'Initial search #2',
    proofStep: 'Step',
    addToStudy: 'Add line to Study',
    addedToStudy: 'Line added to Study.',
    viewAddedLine: 'View added line',
    showEvidence: 'Show evidence',
    hideEvidence: 'Hide evidence',
    proofBoard: 'Proof line mini-board',
    winChance: 'Win chance',
    winChanceChange: 'Reviewed change from reference',
    verdictLabels: { none: 'No annotation', '?!': 'Dubious', '?': 'Mistake', '??': 'Blunder' },
    verdictCodeLabels: {
      improves_on_reference: 'Improves on the reference',
      matches_reference: 'Matches the reference',
      playable_loss: 'Playable',
      inaccuracy: 'Inaccuracy',
      mistake: 'Mistake',
      blunder: 'Blunder',
    },
  },
};

export function normalizeMoveReviewLocale(locale?: string): MoveReviewLocale {
  const requested = locale || (typeof document === 'undefined' ? undefined : document.documentElement.lang) ||
    (typeof navigator === 'undefined' ? undefined : navigator.language);
  return requested?.toLowerCase().startsWith('ko') ? 'ko-KR' : 'en-US';
}

export function moveReviewCopy(locale: MoveReviewLocale): MoveReviewCopy {
  return copies[locale];
}

export function moveReviewVerdictLabel(symbol: MoveReviewVerdictSymbol, copy: MoveReviewCopy): string {
  return copy.verdictLabels[symbol];
}

export function moveReviewVerdictCodeLabel(code: MoveReviewVerdictCode, copy: MoveReviewCopy): string {
  return copy.verdictCodeLabels[code];
}

export function formatMoveReviewPercent(value: number, locale: MoveReviewLocale): string {
  return `${new Intl.NumberFormat(locale, { minimumFractionDigits: 1, maximumFractionDigits: 1 }).format(value)}%`;
}

export function formatMoveReviewPercentagePointChange(value: number, locale: MoveReviewLocale): string {
  const amount = new Intl.NumberFormat(locale, {
    minimumFractionDigits: 1,
    maximumFractionDigits: 1,
    signDisplay: 'always',
  }).format(value);
  return `${amount}%p`;
}

export function moveReviewSubjectFromNodeList(
  variant: VariantKey,
  currentPath: Tree.Path,
  nodeList: readonly Tree.NodeBase[],
  recordedMoves?: readonly Uci[],
  source: 'analysis' | 'study' = 'analysis',
): MoveReviewSubject | undefined {
  if (variant !== 'standard' || nodeList.length < 2) return;
  const initial = nodeList[0];
  const before = nodeList[nodeList.length - 2];
  const after = nodeList[nodeList.length - 1];
  const playedUci = before && standardReviewUci(before.fen, after?.uci);
  const playedSan = san(after?.san);
  if (
    !initial ||
    !before ||
    !after ||
    !after.id ||
    !playedUci ||
    !playedSan ||
    !currentPath.endsWith(after.id)
  )
    return;
  const prefix = nodeList
    .slice(1, -1)
    .map((node, index) => standardReviewUci(nodeList[index]!.fen, node.uci));
  if (!prefix.every((move): move is Uci => !!move)) return;
  const inputLines: NonNullable<MoveReviewSubject['inputLines']> = [];
  const record =
    recordedMoves && prefix.every((move, index) => recordedMoves[index] === move)
      ? recordedMoves.slice(prefix.length)
      : undefined;
  if (record?.length) inputLines.push({ source: 'pgn_record', moves: record });
  const visit = (node: Tree.Node, moves: Uci[]): void => {
    for (const child of node.children) {
      if (!child.uci) continue;
      const path = [...moves, standardReviewUci(node.fen, child.uci)!];
      if (child.children.length) visit(child, path);
      else if (
        !inputLines[0] ||
        inputLines[0].source !== 'pgn_record' ||
        path.length > inputLines[0].moves.length ||
        !path.every((move, index) => inputLines[0]!.moves[index] === move)
      )
        inputLines.push({ source, moves: path });
    }
  };
  if ('children' in before) visit(before as Tree.Node, []);
  return {
    variant: 'standard',
    initialFen: initial.fen,
    movePrefixUci: prefix,
    before: { path: currentPath.slice(0, -after.id.length), fen: before.fen },
    played: { uci: playedUci, san: playedSan },
    after: { path: currentPath, fen: after.fen },
    ...(inputLines.length ? { inputLines } : {}),
  };
}

export function moveReviewRecordFromTree(
  variant: VariantKey,
  root: Tree.Node,
  recordedMoves: readonly Uci[] | undefined,
  chapter: string,
  source: 'analysis' | 'study' = 'study',
): MoveReviewRecord | undefined {
  const first =
    root.children.find(node => standardReviewUci(root.fen, node.uci) === recordedMoves?.[0]) ??
    root.children[0];
  if (!first) return;
  const subject = moveReviewSubjectFromNodeList(variant, first.id, [root, first], recordedMoves, source);
  if (!subject) return;
  const subjects: MoveReviewSubject[] = [subject];
  let parent = first;
  let path = first.id;
  const prefix = [subject.played.uci];
  while (subjects.length < 600) {
    const next = recordedMoves
      ? parent.children.find(node => standardReviewUci(parent.fen, node.uci) === recordedMoves[prefix.length])
      : parent.children[0];
    if (!next) break;
    const move = standardReviewUci(parent.fen, next.uci);
    const label = san(next.san);
    if (!move || !label) break;
    subjects.push({
      variant: 'standard',
      initialFen: root.fen,
      movePrefixUci: [...prefix],
      before: { path, fen: parent.fen },
      played: { uci: move, san: label },
      after: { path: path + next.id, fen: next.fen },
    });
    prefix.push(move);
    path += next.id;
    parent = next;
  }
  const revision = (node: Tree.Node): unknown[] => [node.id, node.uci, node.fen, node.children.map(revision)];
  return { key: JSON.stringify([chapter, moveReviewSubjectKey(subject), revision(root)]), subject, subjects };
}

/** Input revision is owned by the record; grades still belong to one historical edge. */
export function moveReviewFocusKey(subject: MoveReviewSubject): string {
  return JSON.stringify([
    subject.initialFen,
    subject.movePrefixUci,
    subject.before,
    subject.played.uci,
    subject.after,
  ]);
}

export function moveReviewFrameForSubject(
  observations: MoveReviewObservations,
  origin: MoveReviewSubject,
  selected: MoveReviewSubject,
): MoveReviewFrameSelection | undefined {
  if (
    origin.initialFen !== selected.initialFen ||
    !origin.movePrefixUci.every((move, i) => selected.movePrefixUci[i] === move)
  )
    return;
  const suffix = [...selected.movePrefixUci.slice(origin.movePrefixUci.length), selected.played.uci];
  for (const path of observations.paths) {
    const index = suffix.length - 1;
    if (
      path.startFen !== origin.before.fen ||
      !path.occurrenceIds[index] ||
      !suffix.every((move, i) => path.moves[i]?.uci === move)
    )
      continue;
    const before = index === 0 ? path.startFen : path.moves[index - 1]!.fenAfter;
    if (before === selected.before.fen && path.moves[index]!.fenAfter === selected.after.fen)
      return { proofId: path.id, ply: index + 1, phase: 'after' };
  }
  return;
}

function standardReviewUci(beforeFen: FEN, raw: unknown): Uci | undefined {
  const value = uci(raw);
  return value ? standardUci(beforeFen, value) : undefined;
}

export function buildMoveReviewJobRequest(
  requestId: string,
  subject: MoveReviewSubject,
  engineProfile: MoveReviewEngineProfile,
  remainingBudgetMs?: number,
): Record<string, unknown> {
  return {
    schema_version: moveReviewJobRequestSchema,
    request_id: requestId,
    variant: 'standard',
    initial_fen: subject.initialFen,
    move_prefix_uci: subject.movePrefixUci,
    current_fen: subject.before.fen,
    focus: {
      kind: 'played_move',
      played_move_uci: subject.played.uci,
      resulting_fen: subject.after.fen,
    },
    engine_profile: engineProfile,
    ...(remainingBudgetMs === undefined ? {} : { remaining_budget_ms: remainingBudgetMs }),
    ...(subject.inputLines?.length ? { input_lines: subject.inputLines } : {}),
  };
}

export function buildMoveReviewEngineWorkReport(
  work: IssuedMoveReviewEngineWork,
  outcome: MoveReviewEngineOutcome,
): Record<string, unknown> {
  return {
    schema_version: moveReviewEngineWorkReportSchema,
    engine_profile: work.engineProfile,
    work_id: work.workId,
    execution_key_sha256: work.executionKeySha256,
    outcome:
      outcome.kind === 'executor_failed'
        ? { kind: outcome.kind, failure_code: outcome.failureCode }
        : {
            kind: outcome.kind,
            line_suffixes: outcome.lineSuffixes.map(line => ({
              moves: line.moves,
              depth: line.depth,
              white_score: line.whiteScore,
            })),
          },
  };
}

export function moveReviewEngineOutcomeAtRequiredDepth(
  work: MoveReviewSearchScope,
  evaluation: Tree.LocalEval,
  previousEvaluation: Tree.LocalEval | undefined,
  executorElapsedMs = evaluation.millis,
): MoveReviewEngineOutcome | undefined {
  if (
    evaluation.fen !== work.searchFen ||
    evaluation.depth !== work.searchLimits.depth ||
    !Number.isSafeInteger(evaluation.nodes) ||
    evaluation.nodes < 1 ||
    evaluation.nodes > work.searchLimits.nodes ||
    !Number.isSafeInteger(evaluation.millis) ||
    evaluation.millis < 0 ||
    evaluation.millis > work.searchLimits.movetimeMs ||
    !Number.isSafeInteger(executorElapsedMs) ||
    executorElapsedMs < 0 ||
    executorElapsedMs > work.maxSearchElapsedMs ||
    !uci(evaluation.bestmove)
  )
    return;
  const projected = projectExactEvaluationLines(work, evaluation, work.searchLimits.depth);
  const previous = previousEvaluation
    ? projectExactEvaluationLines(work, previousEvaluation, work.searchLimits.depth - 1)
    : undefined;
  if (!projected || !previous || previousEvaluation?.depth !== work.searchLimits.depth - 1) return;
  if (!unique(projected.map(line => line.moves[0])) || !unique(previous.map(line => line.moves[0]))) return;
  if (evaluation.bestmove !== projected[0]?.moves[0]) return;
  return {
    kind: 'completed',
    completedDepth: evaluation.depth,
    selectiveDepth: evaluation.seldepth ?? 0,
    nodes: evaluation.nodes,
    engineTimeMs: evaluation.millis,
    executorElapsedMs,
    bestmoveUci: evaluation.bestmove,
    lineSuffixes: projected,
  };
}

function projectExactEvaluationLines(
  work: MoveReviewSearchScope,
  evaluation: Tree.LocalEval,
  requiredDepth: number,
): MoveReviewLineSuffix[] | undefined {
  if (
    evaluation.fen !== work.searchFen ||
    evaluation.depth !== requiredDepth ||
    evaluation.pvs.length < work.searchLimits.multiPv
  )
    return;
  const projected = evaluation.pvs.slice(0, work.searchLimits.multiPv).map(pv => {
    const depth = pv.depth ?? evaluation.depth;
    if (
      pv.bound !== undefined ||
      depth !== requiredDepth ||
      !validUciMoves(pv.moves, 1, 80 - (work.searchAnchor?.prefixMovesUci.length ?? 0)) ||
      (work.rootRestriction.kind === 'restricted' && !work.rootRestriction.movesUci.includes(pv.moves[0]!))
    )
      return;
    const whiteScore = projectLocalEvalScore(pv);
    return whiteScore && lineLegallyReplays(work, pv.moves)
      ? { moves: [...pv.moves], depth, whiteScore }
      : undefined;
  });
  return projected.every((line): line is MoveReviewLineSuffix => !!line) ? projected : undefined;
}

function lineLegallyReplays(work: MoveReviewSearchScope, lineMoves: readonly Uci[]): boolean {
  return (
    replayFen(work.enginePositionInitialFen, [...work.enginePositionMovesUci, ...lineMoves]) !== undefined
  );
}

export function moveReviewSubjectKey(subject: MoveReviewSubject): string {
  return JSON.stringify([
    subject.variant,
    subject.initialFen,
    subject.movePrefixUci,
    subject.before.path,
    subject.before.fen,
    subject.played.uci,
    subject.played.san,
    subject.after.path,
    subject.after.fen,
    subject.inputLines,
  ]);
}

export function decodeMoveReviewSnapshot(
  raw: unknown,
  context: MoveReviewDecodeContext,
): MoveReviewSnapshot | undefined {
  if (!isObject(raw)) return;
  const snapshot =
    raw.schema_version === moveReviewJobStatusSchema
      ? projectJobStatus(raw, context)
      : raw.schema_version === moveReviewResponseSchema
        ? projectCompleted(raw, context)
        : undefined;
  if (snapshot) shareSnapshotStrings(snapshot);
  return snapshot;
}

// Repeated occurrence and evidence IDs dominate decoded report memory. Share only
// identical string values within this snapshot; objects retain their exact owners.
// The pool itself is discarded, so it cannot keep old reports alive.
function shareSnapshotStrings(snapshot: MoveReviewSnapshot): void {
  const strings = new Map<string, string>();
  const seen = new Set<object>([snapshot.subject]);
  const visit = (value: unknown): void => {
    if (!value || typeof value !== 'object' || seen.has(value) || Object.isFrozen(value)) return;
    seen.add(value);
    const fields = value as Record<string, unknown>;
    for (const key of Object.keys(fields)) {
      const child = fields[key];
      if (typeof child !== 'string') visit(child);
      else {
        const shared = strings.get(child);
        if (shared === undefined) strings.set(child, child);
        else fields[key] = shared;
      }
    }
  };
  visit(snapshot);
}

export function selectedMoveReviewCandidate(
  evidence: MoveReviewEvidence,
  selectedUci?: Uci,
): MoveReviewCandidate | undefined {
  return (
    evidence.candidates.find(candidate => candidate.uci === selectedUci) ??
    evidence.candidates.find(candidate => candidate.roles.includes('played'))
  );
}

function projectJobStatus(
  raw: Record<string, unknown>,
  context: MoveReviewDecodeContext,
): MoveReviewSnapshot | undefined {
  const common = projectCommon(raw, context);
  if (
    !common ||
    nonNegativeInteger(raw.deadline_epoch_ms) === undefined ||
    !wireFocusMatches(raw.focus, context.subject)
  )
    return;
  if (raw.state === 'stopped') {
    if (
      !hasExactKeys(raw, [
        'schema_version',
        'request_id',
        'job_id',
        'engine_profile',
        'variant',
        'deadline_epoch_ms',
        'focus',
        'state',
        'progress',
        'stop_condition',
        ...(raw.observations === undefined ? [] : ['observations']),
        ...(raw.verification === undefined ? [] : ['verification']),
      ]) ||
      !isMoveReviewStopCondition(raw.stop_condition) ||
      common.verification?.state === 'pending' ||
      !validProgress(raw.progress, 'stopped')
    )
      return;
    return { ...common, kind: 'abstained', stopCondition: raw.stop_condition };
  }
  if (
    raw.state !== 'awaiting_engine_work' ||
    !hasExactKeys(raw, [
      'schema_version',
      'request_id',
      'job_id',
      'engine_profile',
      'variant',
      'deadline_epoch_ms',
      'focus',
      'state',
      'progress',
      'issued_engine_work',
      ...(raw.observations === undefined ? [] : ['observations']),
      ...(raw.assessment === undefined ? [] : ['assessment']),
      ...(raw.assessment_state === undefined ? [] : ['assessment_state']),
      ...(raw.assessment_scope === undefined ? [] : ['assessment_scope']),
      ...(raw.verification === undefined ? [] : ['verification']),
    ])
  )
    return;
  const work = projectIssuedEngineWork(
    raw.issued_engine_work,
    common.engineProfile,
    context.subject,
    common.observations,
    common.verification,
  );
  if (!work || !validProgress(raw.progress, work.purpose)) return;
  const assessment = raw.assessment === undefined ? undefined : projectAssessment(raw.assessment, common);
  if (
    (raw.assessment !== undefined && !assessment) ||
    (work.purpose === 'investigation_comparison' || work.purpose === 'response_comparison' || work.purpose === 'threat_probe'
      ? !assessment || !isObject(raw.progress) || raw.progress.selected_commentaries_completed !== 1
      : assessment && (!common.verification || common.verification.state !== 'pending' ||
          !isObject(raw.progress) || raw.progress.selected_commentaries_completed !== 1))
  )
    return;
  return {
    ...common,
    kind: work.purpose === 'root_search' && !assessment ? 'awaiting-core' : 'awaiting-evidence',
    issuedEngineWork: work,
    ...(assessment ? { assessment } : {}),
  };
}

function projectCommon(
  raw: Record<string, unknown>,
  context: MoveReviewDecodeContext,
): MoveReviewSnapshotCommon | undefined {
  const jobId = typeof raw.job_id === 'string' && jobIdPattern.test(raw.job_id) ? raw.job_id : undefined;
  const profile = isMoveReviewEngineProfile(raw.engine_profile) ? raw.engine_profile : undefined;
  if (
    raw.request_id !== context.requestId ||
    !requestIdPattern.test(context.requestId) ||
    !jobId ||
    !profile ||
    raw.variant !== 'standard' ||
    context.subject.variant !== 'standard' ||
    (context.jobId !== undefined && context.jobId !== jobId) ||
    (context.engineProfile !== undefined && context.engineProfile !== profile) ||
    (raw.assessment_state !== undefined &&
      raw.assessment_state !== 'current' &&
      raw.assessment_state !== 'needs_refresh') ||
    (raw.assessment_scope !== undefined &&
      raw.assessment_scope !== 'root_search' &&
      raw.assessment_scope !== 'investigation_pair')
  )
    return;
  const observations =
    raw.observations === undefined
      ? undefined
      : projectObservations(raw.observations, context.subject, profile);
  if (raw.observations !== undefined && !observations) return;
  if (observations) observations.owner = { requestId: context.requestId, jobId };
  const verification = raw.verification === undefined ? undefined : projectVerification(raw.verification);
  if (raw.verification !== undefined && !verification) return;
  return {
    requestId: context.requestId,
    jobId,
    engineProfile: profile,
    judgmentRevision: moveReviewResponseSchema,
    annotationPolicyRevision: moveReviewAnnotationPolicyRevision,
    subject: context.subject,
    ...(observations ? { observations } : {}),
    ...(raw.assessment_state === undefined ? {} : { assessmentState: raw.assessment_state }),
    ...(raw.assessment_scope === undefined ? {} : { assessmentScope: raw.assessment_scope }),
    ...(verification ? { verification } : {}),
  };
}

function projectVerification(value: unknown): MoveReviewVerification | undefined {
  if (!isObject(value) || !hasExactKeys(value,
    ['state', 'target_depth', 'reason', 'trigger_work_ids', 'completed_depth', 'stop_reason']) ||
    !['pending', 'complete', 'stopped', 'not_required'].includes(String(value.state)) ||
    !['initial_verification', 'child_challenge'].includes(String(value.reason)) ||
    integerInRange(value.target_depth, 8, 100) === undefined ||
    !Array.isArray(value.trigger_work_ids) || value.trigger_work_ids.length > 32 ||
    !unique(value.trigger_work_ids) ||
    !value.trigger_work_ids.every(id => typeof id === 'string' && /^work:[0-9]+$/.test(id)) ||
    (value.completed_depth !== null && integerInRange(value.completed_depth, 8, 100) === undefined) ||
    (value.state === 'complete' && (value.completed_depth === null || Number(value.completed_depth) < Number(value.target_depth))) ||
    (value.state === 'stopped' ? typeof value.stop_reason !== 'string' || !value.stop_reason.length : value.stop_reason !== null))
    return;
  return { state: value.state as MoveReviewVerification['state'], targetDepth: Number(value.target_depth),
    reason: value.reason as MoveReviewVerification['reason'], triggerWorkIds: value.trigger_work_ids,
    ...(value.completed_depth === null ? {} : { completedDepth: Number(value.completed_depth) }),
    ...(value.stop_reason === null ? {} : { stopReason: String(value.stop_reason) }) };
}

function wireFocusMatches(value: unknown, subject: MoveReviewSubject): boolean {
  return (
    isObject(value) &&
    hasExactKeys(value, ['kind', 'played_move_uci', 'resulting_fen']) &&
    value.kind === 'played_move' &&
    value.played_move_uci === subject.played.uci &&
    value.resulting_fen === subject.after.fen
  );
}

function validProgress(value: unknown, phase: string): boolean {
  const minimumLegalMoves = phase === 'stopped' ? 0 : 1;
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'phase',
      'legal_move_count',
      'root_candidate_lines_admitted',
      'selected_commentaries_completed',
      'physical_works_issued',
      'physical_reports_accepted',
    ]) ||
    value.phase !== phase ||
    integerInRange(value.legal_move_count, minimumLegalMoves, 218) === undefined ||
    integerInRange(value.root_candidate_lines_admitted, 0, 3) === undefined ||
    integerInRange(value.selected_commentaries_completed, 0, 1) === undefined ||
    integerInRange(value.physical_works_issued, 0, 32) === undefined ||
    integerInRange(value.physical_reports_accepted, 0, 32) === undefined
  )
    return false;
  const issued = value.physical_works_issued as number;
  const accepted = value.physical_reports_accepted as number;
  return phase === 'completed' || phase === 'stopped' ? accepted <= issued : issued === accepted + 1;
}

function isMoveReviewStopCondition(value: unknown): value is MoveReviewStopCondition {
  return (
    typeof value === 'string' && Object.prototype.hasOwnProperty.call(copies['en-US'].stopReasons, value)
  );
}

function projectIssuedEngineWork(
  value: unknown,
  profile: MoveReviewEngineProfile,
  subject: MoveReviewSubject,
  observations: MoveReviewObservations | undefined,
  contextVerification: MoveReviewVerification | undefined,
): IssuedMoveReviewEngineWork | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'work_id',
      'purpose',
      'engine_profile',
      'execution_key_sha256',
      'variant',
      'engine_position_initial_fen',
      'engine_position_moves_uci',
      'search_fen',
      'root_restriction',
      'search_limits',
      'max_search_elapsed_ms',
      ...(value.search_anchor === undefined ? [] : ['search_anchor']),
      ...(value.threat_origin_prefix_uci === undefined ? [] : ['threat_origin_prefix_uci']),
    ]) ||
    typeof value.work_id !== 'string' ||
    !workIdPattern.test(value.work_id) ||
    (value.purpose !== 'root_search' &&
      value.purpose !== 'focus_comparison' &&
      value.purpose !== 'investigation_comparison' &&
      value.purpose !== 'response_comparison' &&
      value.purpose !== 'threat_probe') ||
    value.engine_profile !== profile ||
    typeof value.execution_key_sha256 !== 'string' ||
    !sha256Pattern.test(value.execution_key_sha256) ||
    value.variant !== 'standard' ||
    !validUciMoves(value.engine_position_moves_uci, 0, value.purpose === 'response_comparison' ? 679 : 600) ||
    !fenText(value.search_fen) ||
    !isObject(value.root_restriction) ||
    !isObject(value.search_limits) ||
    !hasExactKeys(value.search_limits, ['depth', 'nodes', 'movetime_ms', 'multi_pv'])
  )
    return;
  const threat = value.purpose === 'threat_probe';
  const threatPrefix = value.threat_origin_prefix_uci;
  if (threat) {
    if (!validUciMoves(threatPrefix, 0, 1) || (threatPrefix.length === 1 && threatPrefix[0] !== subject.played.uci)) return;
    const originFen = threatPrefix.length ? subject.after.fen : subject.before.fen;
    const setup = parseFen(originFen);
    if (setup.isErr) return;
    const origin = setupPosition('chess', setup.value);
    if (origin.isErr || origin.value.isCheck() || origin.value.isEnd()) return;
    const expected = makeFen(threatSetup(originFen));
    // A passed turn has its own isolated engine origin, never a fictional move in the game history.
    if (value.engine_position_initial_fen !== expected || value.engine_position_moves_uci.length !== 0 ||
        value.search_fen !== expected) return;
  } else if (threatPrefix !== undefined || value.engine_position_initial_fen !== subject.initialFen ||
      !arrayPrefix(subject.movePrefixUci, value.engine_position_moves_uci) ||
      replayFen(subject.initialFen, value.engine_position_moves_uci) !== value.search_fen) return;
  const limits = {
    depth: integerInRange(value.search_limits.depth, 1, 100),
    nodes: integerInRange(value.search_limits.nodes, 1, 250_000_000),
    movetimeMs: integerInRange(value.search_limits.movetime_ms, 1, 120_000),
    multiPv: integerInRange(value.search_limits.multi_pv, 1, 3),
  };
  if (Object.values(limits).some(limit => limit === undefined)) return;
  const unrestricted =
    hasExactKeys(value.root_restriction, ['kind']) && value.root_restriction.kind === 'unrestricted';
  const restrictedMoves =
    hasExactKeys(value.root_restriction, ['kind', 'moves_uci']) &&
    value.root_restriction.kind === 'restricted' &&
    validUciMoves(
      value.root_restriction.moves_uci,
      value.purpose === 'response_comparison' ? 1 : 2,
      value.purpose === 'focus_comparison' ? 2 : 218,
    ) &&
    unique(value.root_restriction.moves_uci)
      ? value.root_restriction.moves_uci
      : undefined;
  const rootOrFocusPosition =
    arrayEquals(value.engine_position_moves_uci, subject.movePrefixUci) &&
    value.search_fen === subject.before.fen;
  const anchor = value.search_anchor;
  const anchorPrefix = isObject(anchor) ? (anchor.prefix_moves_uci ?? [anchor.root_move]) : undefined;
  const proposalComparisonMoves = new Set(
    value.purpose === 'response_comparison' && Array.isArray(anchorPrefix)
      ? observations?.selection?.proposals?.flatMap(proposal =>
          proposal.requirements.flatMap(reading =>
            reading.kind === 'compare_moves' &&
            reading.state === 'awaiting_engine' &&
            reading.work_id === value.work_id &&
            arrayEquals(reading.prefix_moves_uci, anchorPrefix)
              ? [reading.reference_move, reading.alternative_move]
              : [],
          ),
        ) ?? []
      : [],
  );
  // Reserve questions survey an entire legal response class. A sampled weak
  // retreat or quiet pawn reply cannot stand for a stronger class member.
  const reserveClassPool = restrictedMoves && isObject(anchor) && Array.isArray(anchorPrefix) &&
    observations?.selection?.proposals?.some(proposal =>
      proposal.requirements.some(reading => {
        if (reading.kind !== 'compare_moves' ||
          reading.state !== 'awaiting_engine' || reading.work_id !== value.work_id ||
          !arrayEquals(reading.prefix_moves_uci, anchorPrefix)) return false;
        return typeof anchor.position_id === 'string' &&
          reserveResponsePoolMatches(proposal, reading, restrictedMoves, observations, anchor.position_id) &&
          limits.multiPv === Math.min(3, restrictedMoves.length);
      }),
    );
  const firstReplySurvey = unrestricted && Array.isArray(anchorPrefix) && anchorPrefix.length === 1 &&
    isObject(anchor) && observations?.positions?.some(position =>
      position.id === anchor.position_id && position.legal_moves.complete &&
      position.legal_moves.moves.length >= 2 && limits.multiPv === Math.min(3, position.legal_moves.moves.length) &&
      observations.selection?.proposals?.some(proposal => proposal.admission === 'generated' &&
        proposal.requirements.some(reading => reading.kind === 'compare_moves' &&
          reading.state === 'awaiting_engine' && reading.work_id === value.work_id &&
          arrayEquals(reading.prefix_moves_uci, anchorPrefix) && reading.reference_move !== reading.alternative_move &&
          [reading.reference_move, reading.alternative_move].every(move =>
            position.legal_moves.moves.some(row => row.uci === move)))));
  const responsePosition =
    value.purpose === 'response_comparison' &&
    isObject(anchor) &&
    hasExactKeys(anchor, [
      'position_id',
      'root_move',
      'root_occurrence_id',
      ...(anchor.prefix_moves_uci === undefined ? [] : ['prefix_moves_uci']),
    ]) &&
    semanticId(anchor.position_id) &&
    semanticId(anchor.root_occurrence_id) &&
    typeof anchor.root_move === 'string' &&
    validUciMoves(anchorPrefix, 1, 79) &&
    anchorPrefix[0] === anchor.root_move &&
    arrayEquals(value.engine_position_moves_uci, [...subject.movePrefixUci, ...anchorPrefix]) &&
    observations?.positions?.some(
      position => position.id === anchor.position_id && position.fen === value.search_fen,
    ) &&
    commentarySearchAnchorPath(
      observations,
      subject.before.fen,
      anchor.root_move,
      anchor.root_occurrence_id as string,
      anchor.position_id as string,
      anchorPrefix,
    ) &&
    (firstReplySurvey || (anchorPrefix.length === 1 &&
      observations.selection?.response_inquiries?.some(
        inquiry =>
          inquiry.state === 'awaiting_engine' &&
          inquiry.root_move === anchor.root_move &&
          inquiry.root_move_occurrence_id === anchor.root_occurrence_id &&
          inquiry.child_position_id === anchor.position_id &&
          ((unrestricted &&
            limits.multiPv === Math.min(3, inquiry.capture_moves.length + inquiry.decline_moves.length)) ||
            (restrictedMoves &&
              responsePoolMatchesClasses(
                observations.selection!.response_inquiries!.filter(
                  peer =>
                    peer.root_move === anchor.root_move &&
                    peer.root_move_occurrence_id === anchor.root_occurrence_id &&
                    peer.child_position_id === anchor.position_id,
                ),
                restrictedMoves,
                observations.selection!.response_searches?.filter(search =>
                  responseSearchMatchesInquiry(search, inquiry),
                ),
                subject.played.uci,
              ) &&
              limits.multiPv === Math.min(3, restrictedMoves.length))),
      )) ||
      observations.selection?.continuation_inquiries?.some(
        inquiry =>
          inquiry.position_id === anchor.position_id &&
          inquiry.prefix_occurrence_ids[0] === anchor.root_occurrence_id &&
          arrayEquals(inquiry.prefix_moves_uci, anchorPrefix) &&
          inquiry.legal_moves.length >= 2 &&
          ((inquiry.state === 'awaiting_engine' &&
            unrestricted &&
            limits.multiPv === Math.min(3, inquiry.legal_moves.length)) ||
            (inquiry.route_challenge?.state === 'awaiting_engine' &&
              restrictedMoves &&
              continuationRoutePoolMatches(inquiry, restrictedMoves, true) &&
              limits.multiPv === Math.min(3, restrictedMoves.length))),
      ) ||
      reserveClassPool ||
      // One bounded work can answer several pairs at this exact prefix.
      (proposalComparisonMoves.size >= 2 &&
        proposalComparisonMoves.size <= 3 &&
        restrictedMoves?.length === proposalComparisonMoves.size &&
        restrictedMoves.every(move => proposalComparisonMoves.has(move)) &&
        limits.multiPv === Math.min(3, restrictedMoves.length)) ||
      observations.selection?.proposals?.some(proposal =>
        proposal.requirements.some(
          reading =>
            reading.kind === 'fact' &&
            reading.state === 'awaiting_engine' &&
            reading.work_id === value.work_id &&
            unrestricted &&
            reading.result?.needs.some(
              need =>
                need.kind === 'continuation' &&
                arrayEquals(need.prefix_moves_uci, anchorPrefix) &&
                need.horizon !== null &&
                need.horizon > anchorPrefix.length,
            ) &&
            observations.positions?.some(
              position =>
                position.id === anchor.position_id &&
                position.legal_moves.complete &&
                position.legal_moves.moves.length >= 2 &&
                limits.multiPv === Math.min(3, position.legal_moves.moves.length),
            ),
        ),
      ));
  const selectedFirstReply = value.purpose === 'response_comparison' &&
    Array.isArray(anchorPrefix) && anchorPrefix.length === 1 && restrictedMoves &&
    ((restrictedMoves.length === 2 && observations?.selection?.proposals?.some(proposal =>
      proposal.admission === 'generated' &&
      proposal.requirements.some(reading => reading.kind === 'compare_moves' &&
        reading.state === 'awaiting_engine' && reading.work_id === value.work_id &&
        arrayEquals(reading.prefix_moves_uci, anchorPrefix) &&
        restrictedMoves.includes(reading.reference_move) && restrictedMoves.includes(reading.alternative_move)))) ||
      (restrictedMoves.length >= 2 && restrictedMoves.length <= 3 &&
        anchorPrefix[0] === subject.played.uci && observations?.selection?.response_inquiries?.some(inquiry =>
          inquiry.state === 'awaiting_engine' && inquiry.root_move === subject.played.uci &&
          inquiry.root_move_occurrence_id === anchor.root_occurrence_id &&
          inquiry.child_position_id === anchor.position_id &&
          inquiry.original_target.square === subject.played.uci.slice(0, 2) &&
          inquiry.target.square === subject.played.uci.slice(2, 4) &&
          reviewedMoverResponsePoolMatches([inquiry], restrictedMoves,
            observations.selection?.response_searches, subject.played.uci))));
  if (value.purpose !== 'response_comparison' && anchor !== undefined) return;
  const stoppedFocus = value.purpose === 'focus_comparison' && restrictedMoves &&
    observations?.selection?.root_searches?.some(search => search.purpose === 'focus_comparison' &&
      search.state === 'stopped' && search.stop_reason === 'search_budget_exhausted' &&
      search.required_depth === limits.depth && search.root_restriction.kind === 'restricted' &&
      arrayEquals(search.root_restriction.moves_uci, restrictedMoves));
  const inquiryPrefix = anchorPrefix === undefined ? [] : Array.isArray(anchorPrefix) ? anchorPrefix : undefined;
  const selectedInquiry = value.purpose !== 'focus_comparison' && inquiryPrefix && restrictedMoves &&
    observations?.selection?.proposals?.some(proposal => proposal.requirements.some(reading =>
      reading.kind === 'compare_moves' && reading.state === 'awaiting_engine' &&
      reading.work_id === value.work_id && arrayEquals(reading.prefix_moves_uci, inquiryPrefix) &&
      restrictedMoves.includes(reading.reference_move) && restrictedMoves.includes(reading.alternative_move)));
  const shapeValid =
    (threat && unrestricted && limits.depth === 8 && limits.nodes === 250_000 &&
      limits.movetimeMs === 750 && limits.multiPv! <= 2 && value.max_search_elapsed_ms === 1_250) ||
    (value.purpose === 'root_search' &&
      rootOrFocusPosition &&
      unrestricted &&
      ((limits.depth === 8 && limits.nodes === 2_000_000 && limits.movetimeMs === 2_500 && value.max_search_elapsed_ms === 3_500) ||
        (limits.depth! >= 16 && limits.nodes === 5_000_000 && limits.movetimeMs === 5_000 && value.max_search_elapsed_ms === 6_000) ||
        (limits.depth! >= 16 &&
          ((limits.nodes === 40_000_000 && limits.movetimeMs === 8_000 && value.max_search_elapsed_ms === 9_000) ||
            (limits.nodes === 250_000_000 && limits.movetimeMs === 120_000 && value.max_search_elapsed_ms === 121_000)) &&
          contextVerification?.state === 'pending' &&
          contextVerification.reason === 'child_challenge' && contextVerification.targetDepth === limits.depth))) ||
    ((value.purpose === 'focus_comparison' ||
      value.purpose === 'investigation_comparison' ||
      value.purpose === 'response_comparison') &&
      (value.purpose === 'response_comparison' ? responsePosition : rootOrFocusPosition) &&
      (value.purpose === 'response_comparison' || restrictedMoves) &&
      (value.purpose !== 'focus_comparison' || restrictedMoves?.includes(subject.played.uci)) &&
      (!restrictedMoves ||
        restrictedMoves.every(
          move =>
            replayFen(subject.initialFen, [...(value.engine_position_moves_uci as Uci[]), move]) !==
            undefined,
        )) &&
      (limits.depth! >= 16 || (value.purpose === 'focus_comparison' && limits.depth === 8)) &&
      ((limits.nodes === 2_000_000 && limits.movetimeMs === 2_500 && value.max_search_elapsed_ms === 3_500) ||
        (limits.depth! >= 16 && (selectedInquiry || stoppedFocus) && limits.nodes === 20_000_000 && limits.movetimeMs === 2_500 &&
          value.max_search_elapsed_ms === 3_500) ||
        (selectedFirstReply && limits.nodes === 40_000_000 && limits.movetimeMs === 8_000 &&
          value.max_search_elapsed_ms === 9_000) ||
        (limits.depth! >= 16 && stoppedFocus && limits.nodes === 40_000_000 && limits.movetimeMs === 8_000 &&
          value.max_search_elapsed_ms === 9_000) ||
        (limits.depth! >= 16 && (limits.nodes === 5_000_000 || limits.nodes === 20_000_000) &&
          limits.movetimeMs === 5_000 && value.max_search_elapsed_ms === 6_000 &&
          // A selected inquiry may use more nodes inside the same five seconds
          // for its direct reply or one server-scheduled retry. Position, legal roots,
          // depth and MultiPV remain subject to the checks above.
          (value.purpose !== 'focus_comparison' || stoppedFocus))) &&
      (value.purpose === 'response_comparison' ||
        (restrictedMoves && limits.multiPv === Math.min(3, restrictedMoves.length))));
  if (!shapeValid) return;
  return {
    engineProfile: profile,
    workId: value.work_id,
    purpose: value.purpose,
    executionKeySha256: value.execution_key_sha256,
    variant: 'standard',
    enginePositionInitialFen: value.engine_position_initial_fen as FEN,
    enginePositionMovesUci: [...value.engine_position_moves_uci],
    searchFen: value.search_fen as FEN,
    ...(threat ? { threatOriginPrefixUci: [...(threatPrefix as Uci[])] } : {}),
    ...(responsePosition
      ? {
          searchAnchor: {
            positionId: anchor.position_id as string,
            rootMove: anchor.root_move as Uci,
            rootOccurrenceId: anchor.root_occurrence_id as string,
            prefixMovesUci: [...(anchorPrefix as Uci[])],
          },
        }
      : {}),
    rootRestriction: restrictedMoves
      ? { kind: 'restricted', movesUci: [...restrictedMoves] }
      : { kind: 'unrestricted' },
    searchLimits: {
      depth: limits.depth!,
      nodes: limits.nodes as IssuedMoveReviewEngineWork['searchLimits']['nodes'],
      movetimeMs: limits.movetimeMs as IssuedMoveReviewEngineWork['searchLimits']['movetimeMs'],
      multiPv: limits.multiPv!,
    },
    maxSearchElapsedMs: value.max_search_elapsed_ms as number,
  };
}

function projectCompleted(
  raw: Record<string, unknown>,
  context: MoveReviewDecodeContext,
): MoveReviewSnapshot | undefined {
  if (
    !hasExactKeys(raw, [
      'schema_version',
      'annotation_policy_revision',
      'request_id',
      'job_id',
      'engine_profile',
      'variant',
      'current_fen',
      'focus',
      'progress',
      'result',
      ...(raw.observations === undefined ? [] : ['observations']),
      ...(raw.assessment_state === undefined ? [] : ['assessment_state']),
      ...(raw.assessment_scope === undefined ? [] : ['assessment_scope']),
      ...(raw.verification === undefined ? [] : ['verification']),
    ]) ||
    raw.annotation_policy_revision !== moveReviewAnnotationPolicyRevision ||
    raw.current_fen !== context.subject.before.fen ||
    !wireFocusMatches(raw.focus, context.subject) ||
    !validProgress(raw.progress, 'completed')
  )
    return;
  const common = projectCommon(raw, context);
  if (!common || common.verification?.state === 'pending') return;
  const assessment = projectAssessment(raw.result, common);
  return assessment ? { ...common, ...assessment } : undefined;
}

function projectAssessment(
  result: unknown,
  common: MoveReviewSnapshotCommon,
): MoveReviewAssessment | undefined {
  if (!isObject(result)) return;
  if (result.kind === 'move_observations') {
    if (!common.observations || !hasOnlyKeys(result, ['kind', 'draw_claims'], ['kind'])) return;
    const drawClaims = result.draw_claims === undefined ? undefined : projectDrawClaims(result.draw_claims);
    if (result.draw_claims !== undefined && !drawClaims) return;
    return {
      kind: 'completed',
      evidence: { candidates: [], ...(drawClaims ? { drawClaims } : {}) },
    };
  }
  const action = projectPositionAction(result);
  if (action) return { kind: 'position-action', action };
  if (result.kind === 'forced_single_move') return projectForcedSingleMove(result, common.subject);
  if (
    result.kind !== 'selected_move_choices' ||
    !hasOnlyKeys(
      result,
      ['kind', 'selected_move_reviews', 'draw_claims'],
      ['kind', 'selected_move_reviews'],
    ) ||
    !Array.isArray(result.selected_move_reviews) ||
    result.selected_move_reviews.length < 1 ||
    result.selected_move_reviews.length > 2
  )
    return;
  const candidates = result.selected_move_reviews.map(value => projectSelectedReview(value, common.subject));
  if (!candidates.every((candidate): candidate is MoveReviewCandidate => !!candidate)) return;
  const played = candidates.filter(candidate => candidate.roles.includes('played'));
  const best = candidates.filter(candidate => candidate.roles.includes('best'));
  if (
    played.length !== 1 ||
    best.length !== 1 ||
    played[0]?.uci !== common.subject.played.uci ||
    !unique(candidates.map(candidate => candidate.uci))
  )
    return;
  const bestRow = result.selected_move_reviews[candidates.indexOf(best[0]!)];
  if (
    common.assessmentScope !== 'investigation_pair' &&
    (!isObject(bestRow) || !isObject(bestRow.selection) || bestRow.selection.root_rank !== 1)
  )
    return;
  const drawClaims = Object.prototype.hasOwnProperty.call(result, 'draw_claims')
    ? projectDrawClaims(result.draw_claims)
    : undefined;
  if (Object.prototype.hasOwnProperty.call(result, 'draw_claims') && !drawClaims) return;
  for (const candidate of candidates) {
    if (candidate.review.kind !== 'move-verdict') continue;
    if (candidate.review.core.bestUci !== best[0]!.uci) return;
    candidate.winPercent = candidate.review.core.winChance?.playedPercent;
    if (candidate.review.core.winChance && best[0]!.winPercent === undefined)
      best[0]!.winPercent = candidate.review.core.winChance.referencePercent;
  }
  return {
    kind: 'completed',
    evidence: { candidates, ...(drawClaims ? { drawClaims } : {}) },
  };
}

function projectPositionAction(value: Record<string, unknown>): MoveReviewPositionAction | undefined {
  if (value.kind === 'automatic_terminal') {
    if (!hasExactKeys(value, ['kind', 'terminal'])) return;
    const terminal = projectAutomaticTerminal(value.terminal);
    return terminal ? { kind: 'automatic-terminal', terminal } : undefined;
  }
  if (value.kind === 'draw_claim_action') {
    if (!hasExactKeys(value, ['kind', 'claims'])) return;
    const claims = projectDrawClaims(value.claims);
    return claims ? { kind: 'draw-claim', claims } : undefined;
  }
  return;
}

function projectForcedSingleMove(
  value: Record<string, unknown>,
  subject: MoveReviewSubject,
): MoveReviewAssessment | undefined {
  if (
    !hasOnlyKeys(
      value,
      ['kind', 'move_uci', 'supporting_endpoint', 'draw_claims'],
      ['kind', 'move_uci', 'supporting_endpoint'],
    )
  )
    return;
  const move = uci(value.move_uci);
  const endpoint = projectEndpoint(value.supporting_endpoint);
  const claims = Object.prototype.hasOwnProperty.call(value, 'draw_claims')
    ? projectDrawClaims(value.draw_claims)
    : undefined;
  if (!move || move !== subject.played.uci || !endpoint || endpoint.moves[0] !== move) return;
  return {
    kind: 'completed',
    evidence: {
      ...(claims ? { drawClaims: claims } : {}),
      candidates: [
        {
          uci: move,
          label: subject.played.san,
          roles: ['best', 'played'],
          winPercent: endpoint.winPercent,
          review: {
            kind: 'forced-single-move',
            lineUcis: endpoint.moves,
            ...(endpoint.terminal ? { terminal: endpoint.terminal } : {}),
          },
        },
      ],
    },
  };
}

function projectSelectedReview(value: unknown, subject: MoveReviewSubject): MoveReviewCandidate | undefined {
  if (
    !isObject(value) ||
    integerInRange(value.legal_move_index, 0, 217) === undefined ||
    !uci(value.move_uci) ||
    !isObject(value.selection) ||
    !hasOnlyKeys(value.selection, ['roles', 'root_rank'], ['roles']) ||
    !Array.isArray(value.selection.roles)
  )
    return;
  const roles = value.selection.roles.filter(
    (role): role is MoveReviewCandidateRole => role === 'best' || role === 'played' || role === 'alternative',
  );
  if (
    roles.length !== value.selection.roles.length ||
    !unique(roles) ||
    roles.length < 1 ||
    roles.includes('played') !== (value.move_uci === subject.played.uci) ||
    (Object.prototype.hasOwnProperty.call(value.selection, 'root_rank') &&
      integerInRange(value.selection.root_rank, 1, 3) === undefined)
  )
    return;
  let review: MoveReviewCandidateReview | undefined;
  if (hasExactKeys(value, ['legal_move_index', 'move_uci', 'selection', 'commentary']))
    review = projectCommentary(value.commentary, value.move_uci as Uci, subject);
  else if (hasExactKeys(value, ['legal_move_index', 'move_uci', 'selection', 'line_insight']))
    review = projectLineInsight(value.line_insight, value.move_uci as Uci, subject.before.fen);
  if (!review) return;
  return {
    uci: value.move_uci as Uci,
    label: moveReviewSan(subject.before.fen, value.move_uci as Uci),
    roles,
    review,
  };
}

interface ProjectedEndpoint {
  moves: Uci[];
  winPercent?: number;
  terminal?: MoveReviewAutomaticTerminal;
}

function projectLineInsight(value: unknown, move: Uci, startFen: FEN): MoveReviewCandidateReview | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['endpoint'])) return;
  const endpoint = projectEndpoint(value.endpoint);
  if (!endpoint || endpoint.moves[0] !== move) return;
  const proof = buildProof(`line:${move}`, startFen, endpoint.moves);
  return proof ? { kind: 'single-candidate-insight', proof } : undefined;
}

function projectCommentary(
  value: unknown,
  move: Uci,
  subject: MoveReviewSubject,
): MoveReviewCandidateReview | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['primary']) || !isObject(value.primary)) return;
  const primary = value.primary;
  let verdictRef: string | undefined;
  let verdictCode: MoveReviewVerdictCode | undefined;
  let referenceEndpoint: ProjectedEndpoint | undefined;
  let reviewedEndpoint: ProjectedEndpoint | undefined;
  let winChance: MoveReviewWinChance | undefined;
  let bestChoice: Extract<MoveReviewCore, { kind: 'best-choice' }>['bestChoice'] | undefined;
  if (primary.kind === 'move_verdict') {
    if (
      !hasExactKeys(primary, [
        'kind',
        'comparison_evidence_id',
        'verdict_code',
        'verdict_confidence',
        'mover',
        'delta',
        'reference_endpoint',
        'played_endpoint',
      ])
    )
      return;
    verdictRef = semanticId(primary.comparison_evidence_id);
    verdictCode = moveReviewVerdictCode(primary.verdict_code);
    referenceEndpoint = projectEndpoint(primary.reference_endpoint);
    reviewedEndpoint = projectEndpoint(primary.played_endpoint);
    const delta = projectDelta(primary.delta);
    if (
      !verdictRef ||
      !verdictCode ||
      !referenceEndpoint ||
      !reviewedEndpoint ||
      reviewedEndpoint.moves[0] !== move ||
      delta === undefined
    )
      return;
    if (referenceEndpoint.winPercent !== undefined && reviewedEndpoint.winPercent !== undefined)
      winChance = {
        referencePercent: referenceEndpoint.winPercent,
        playedPercent: reviewedEndpoint.winPercent,
        changePercentagePoints: delta,
      };
  } else if (primary.kind === 'best_choice') {
    if (
      !hasExactKeys(primary, [
        'kind',
        'comparison_evidence_id',
        'runner_up_verdict_code',
        'verdict_confidence',
        'mover',
        'delta',
        'best_endpoint',
        'runner_up_endpoint',
      ])
    )
      return;
    verdictRef = semanticId(primary.comparison_evidence_id);
    reviewedEndpoint = projectEndpoint(primary.best_endpoint);
    referenceEndpoint = projectEndpoint(primary.runner_up_endpoint);
    const runnerUpVerdictCode = moveReviewVerdictCode(primary.runner_up_verdict_code);
    const delta = projectDelta(primary.delta);
    const runnerUpUci = referenceEndpoint?.moves[0];
    if (
      !verdictRef ||
      !reviewedEndpoint ||
      !referenceEndpoint ||
      reviewedEndpoint.moves[0] !== move ||
      !runnerUpUci ||
      runnerUpUci === move ||
      !runnerUpVerdictCode ||
      runnerUpVerdictCode === 'improves_on_reference' ||
      delta === undefined
    )
      return;
    bestChoice = { runnerUpVerdictCode, runnerUpUci };
    if (reviewedEndpoint.winPercent !== undefined && referenceEndpoint.winPercent !== undefined)
      winChance = {
        referencePercent: referenceEndpoint.winPercent,
        playedPercent: reviewedEndpoint.winPercent,
        changePercentagePoints: -delta,
      };
  } else return;
  const bestUci = primary.kind === 'best_choice' ? reviewedEndpoint?.moves[0] : referenceEndpoint?.moves[0];
  if (!bestUci || !referenceEndpoint || !reviewedEndpoint || !verdictRef) return;
  const comparisonProof = buildProof(`pv:${move}`, subject.before.fen, reviewedEndpoint.moves);
  if (!comparisonProof) return;
  const comparisonProofs = [comparisonProof];
  if (bestChoice) {
    const referenceProof = buildProof(`pv:${move}:reference`, subject.before.fen, referenceEndpoint.moves);
    if (!referenceProof) return;
    comparisonProofs.push(referenceProof);
  }
  const coreFields: MoveReviewCoreFields = {
    verdictRef,
    verdictSymbol: verdictCode ? verdictSymbolByCode[verdictCode] : 'none',
    playedUci: move,
    bestUci,
    ...(winChance ? { winChance } : {}),
    ...(referenceEndpoint.terminal ? { referenceTerminal: referenceEndpoint.terminal } : {}),
    ...(reviewedEndpoint.terminal ? { reviewedTerminal: reviewedEndpoint.terminal } : {}),
  };
  const core: MoveReviewCore | undefined = bestChoice
    ? { ...coreFields, kind: 'best-choice', bestChoice }
    : verdictCode
      ? { ...coreFields, kind: 'move-verdict', verdictCode }
      : undefined;
  if (!core) return;
  return {
    kind: 'move-verdict',
    core,
    comparisonProofs,
  };
}

export function moveReviewProofById(
  review: MoveReviewCandidateReview,
  proofId: string,
): MoveReviewProof | undefined {
  if (review.kind === 'single-candidate-insight')
    return review.proof.id === proofId ? review.proof : undefined;
  if (review.kind !== 'move-verdict') return;
  return review.comparisonProofs.find(proof => proof.id === proofId);
}

function projectObservations(
  value: unknown,
  subject: MoveReviewSubject,
  engineProfile: string,
): MoveReviewObservations | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'statements',
      'paths',
      ...(value.positions !== undefined ? ['positions'] : []),
      ...(value.input_issues !== undefined ? ['input_issues'] : []),
      ...(value.highlights !== undefined ? ['highlights'] : []),
      ...(value.selection !== undefined ? ['selection'] : []),
    ]) ||
    !Array.isArray(value.paths) ||
    !Array.isArray(value.statements)
  )
    return;
  const paths: MoveReviewObservations['paths'] = [];
  let selectionEvidenceValid = true;
  const occurrences = new Map<string, { uci: Uci; beforeFen: FEN; afterFen: FEN }>();
  for (const raw of value.paths) {
    if (
      !isObject(raw) ||
      !hasExactKeys(raw, [
        'id',
        'kind',
        'start_fen',
        'moves',
        'occurrence_ids',
        ...(value.positions !== undefined ? ['position_ids'] : []),
        ...(raw.line_summary !== undefined ? ['line_summary'] : []),
        ...(raw.sources !== undefined ? ['sources'] : []),
        ...(raw.evaluations !== undefined ? ['evaluations'] : []),
        ...(raw.conditional_evaluations !== undefined ? ['conditional_evaluations'] : []),
      ]) ||
      !semanticId(raw.id) ||
      !fenText(raw.start_fen) ||
      !validUciMoves(raw.moves, 1, 600) ||
      !Array.isArray(raw.occurrence_ids) ||
      raw.occurrence_ids.length !== raw.moves.length ||
      (value.positions !== undefined &&
        (!Array.isArray(raw.position_ids) ||
          raw.position_ids.length !== raw.moves.length + 1 ||
          !raw.position_ids.every(id => id === null || semanticId(id)))) ||
      (raw.kind !== 'played' && raw.kind !== 'analysis' && raw.kind !== 'history')
    )
      return;
    if (raw.kind === 'history') {
      if (
        subject.movePrefixUci.length === 0 ||
        raw.moves.length !== 2 ||
        raw.moves[0] !== subject.movePrefixUci[subject.movePrefixUci.length - 1] ||
        raw.moves[1] !== subject.played.uci ||
        replayFen(subject.initialFen, subject.movePrefixUci.slice(0, -1)) !== raw.start_fen ||
        raw.occurrence_ids[0] !== null
      )
        return;
    } else if (
      raw.start_fen !== subject.before.fen ||
      (raw.kind === 'played' && (raw.moves.length !== 1 || raw.moves[0] !== subject.played.uci))
    )
      return;
    const proof = buildProof(raw.id as string, raw.start_fen as FEN, raw.moves);
    if (!proof) return;
    for (const [index, id] of raw.occurrence_ids.entries()) {
      if (id === null && raw.kind === 'history' && index === 0) continue;
      if (!semanticId(id)) return;
      const occurrence = {
        uci: proof.moves[index]!.uci,
        beforeFen: index === 0 ? proof.startFen : proof.moves[index - 1]!.fenAfter,
        afterFen: proof.moves[index]!.fenAfter,
      };
      const previous = occurrences.get(id as string);
      if (
        previous &&
        (previous.uci !== occurrence.uci ||
          previous.beforeFen !== occurrence.beforeFen ||
          previous.afterFen !== occurrence.afterFen)
      )
        return;
      occurrences.set(id as string, occurrence);
    }
    const sources = raw.sources;
    if (
      sources !== undefined &&
      (!Array.isArray(sources) ||
        !sources.length ||
        !unique(sources) ||
        !sources.every(source =>
          subject.inputLines?.some(
            line =>
              line.source === source && proof.moves.every((move, index) => line.moves[index] === move.uci),
          ),
        ))
    )
      return;
    const evaluations =
      Array.isArray(raw.evaluations) && raw.evaluations.length > 0 && raw.evaluations.length <= 600
        ? raw.evaluations.map(value => decodeCommentaryEvaluation(value, proof.moves.length))
        : undefined;
    const validEvaluations = evaluations?.every(
      (value): value is CommentaryEvaluation => value !== undefined && value.source !== 'response_comparison',
    );
    if (raw.evaluations !== undefined && !validEvaluations) selectionEvidenceValid = false;
    const conditionalEvaluations =
      Array.isArray(raw.conditional_evaluations) &&
      raw.conditional_evaluations.length > 0 &&
      raw.conditional_evaluations.length <= 2
        ? raw.conditional_evaluations.map(value => {
            if (
              !isObject(value) ||
              !hasExactKeys(value, ['search_position_id', 'ply_offset', 'evaluation']) ||
              raw.kind === 'history' ||
              proof.moves.length < 2 ||
              integerInRange(value.ply_offset, 1, proof.moves.length - 1) === undefined ||
              !semanticId(value.search_position_id) ||
              !Array.isArray(raw.position_ids) ||
              raw.position_ids[value.ply_offset as number] !== value.search_position_id
            )
              return;
            const offset = value.ply_offset as number;
            const evaluation = decodeCommentaryEvaluation(value.evaluation, proof.moves.length - offset);
            return evaluation?.source === 'response_comparison'
              ? { searchPositionId: value.search_position_id as string, plyOffset: offset, evaluation }
              : undefined;
          })
        : undefined;
    const validConditionalEvaluations =
      conditionalEvaluations?.every(value => value !== undefined) &&
      new Set(conditionalEvaluations.map(value => JSON.stringify(value))).size ===
        conditionalEvaluations.length;
    if (raw.conditional_evaluations !== undefined && !validConditionalEvaluations)
      selectionEvidenceValid = false;
    paths.push({
      ...proof,
      kind: raw.kind,
      occurrenceIds: raw.occurrence_ids as (string | null)[],
      ...(value.positions !== undefined ? { positionIds: raw.position_ids as (string | null)[] } : {}),
      ...(sources ? { sources: sources as MoveReviewLineSource[] } : {}),
      ...(validEvaluations ? { evaluations: evaluations as CommentaryEvaluation[] } : {}),
      ...(validConditionalEvaluations
        ? {
            conditionalEvaluations: conditionalEvaluations as NonNullable<
              MoveReviewObservations['paths'][number]['conditionalEvaluations']
            >,
          }
        : {}),
    });
  }
  if (!unique(paths.map(path => path.id))) return;
  const statements: MoveReviewObservation[] = [];
  const square = (v: unknown): v is Key => typeof v === 'string' && /^[a-h][1-8]$/.test(v);
  const role = (v: unknown): v is ObservedPieceRole =>
    typeof v === 'string' && Object.prototype.hasOwnProperty.call(copies['en-US'].roleLabels, v);
  const piece = (v: unknown): v is ObservedPiece =>
    isObject(v) &&
    hasExactKeys(v, ['side', 'role', 'square']) &&
    (v.side === 'white' || v.side === 'black') &&
    role(v.role) &&
    square(v.square);
  const side = (v: unknown): v is Color => v === 'white' || v === 'black';
  const list = <T>(v: unknown, valid: (item: unknown) => item is T): v is T[] =>
    Array.isArray(v) && v.every(valid);
  const locatedPiece = (v: unknown): v is LocatedPiece =>
    isObject(v) && hasExactKeys(v, ['role', 'square']) && role(v.role) && square(v.square);
  const legalResource = (v: unknown): v is LegalResource => {
    if (!isObject(v) || !hasExactKeys(v, ['movement', 'move_uci', 'capture'])) return false;
    const m = v.movement;
    return (
      isObject(m) &&
      hasExactKeys(m, ['side', 'from', 'to', 'before_role', 'after_role']) &&
      side(m.side) &&
      square(m.from) &&
      square(m.to) &&
      role(m.before_role) &&
      role(m.after_role) &&
      validUciMoves([v.move_uci], 1, 1) &&
      (v.move_uci as string).slice(0, 4) === m.from + m.to &&
      (v.capture === null || piece(v.capture))
    );
  };
  const movementResource = (v: unknown): v is MovementResource => {
    if (
      !isObject(v) ||
      !hasExactKeys(v, [
        'destination',
        'target',
        'mode',
        ...(v.mode === 'en_passant_capture' ? ['captured_pawn'] : []),
      ]) ||
      !square(v.destination)
    )
      return false;
    const target = v.target;
    return (
      isObject(target) &&
      (target.kind === 'empty'
        ? hasExactKeys(target, ['kind'])
        : hasExactKeys(target, ['kind', 'role']) &&
          (target.kind === 'friendly' || target.kind === 'enemy') &&
          role(target.role)) &&
      (v.mode === 'en_passant_capture'
        ? piece(v.captured_pawn) && v.captured_pawn.role === 'pawn'
        : v.mode === 'controlled_destination' ||
          v.mode === 'pawn_advance' ||
          v.mode === 'pawn_double_advance')
    );
  };
  const pinPath = (v: unknown): v is RestrictedRecapture['absolute_pin_paths'][number] => {
    if (
      !isObject(v) ||
      !hasExactKeys(v, ['pinner', 'pinned', 'king_square', 'geometry']) ||
      !locatedPiece(v.pinner) ||
      !locatedPiece(v.pinned) ||
      !square(v.king_square)
    )
      return false;
    const g = v.geometry;
    if (
      !isObject(g) ||
      !hasExactKeys(g, ['direction', 'squares']) ||
      !list(g.squares, square) ||
      !g.squares.length ||
      !unique(g.squares)
    )
      return false;
    const d = g.direction;
    return (
      isObject(d) &&
      hasExactKeys(d, ['file_step', 'rank_step']) &&
      integerInRange(d.file_step, -1, 1) !== undefined &&
      integerInRange(d.rank_step, -1, 1) !== undefined &&
      (d.file_step !== 0 || d.rank_step !== 0)
    );
  };
  const restrictedRecapture = (v: unknown): v is RestrictedRecapture =>
    isObject(v) &&
    hasExactKeys(v, ['piece', 'resource', 'king_square', 'post_move_controllers', 'absolute_pin_paths']) &&
    locatedPiece(v.piece) &&
    movementResource(v.resource) &&
    square(v.king_square) &&
    list(v.post_move_controllers, locatedPiece) &&
    list(v.absolute_pin_paths, pinPath);
  const checkResponse = (v: unknown): v is CheckResponse =>
    isObject(v) &&
    hasExactKeys(v, ['resource', 'modes']) &&
    legalResource(v.resource) &&
    Array.isArray(v.modes) &&
    v.modes.length > 0 &&
    unique(v.modes) &&
    v.modes.every(mode => mode === 'king_move' || mode === 'capture_checker' || mode === 'interpose');
  const controlledDestination = (
    v: unknown,
  ): v is Extract<
    MoveReviewObservation,
    { kind: 'check_responses' }
  >['controlled_king_destinations'][number] =>
    isObject(v) &&
    hasExactKeys(v, ['resource', 'controllers']) &&
    movementResource(v.resource) &&
    list(v.controllers, locatedPiece);
  const pawnState = (v: unknown): v is PawnState =>
    isObject(v) &&
    hasExactKeys(v, [
      'side',
      'square',
      'doubled',
      'isolated',
      'passed',
      'geometrically_protected_passer',
      'front_square',
      'front_occupant',
      'component_pawns',
      'connections',
      'enemy_pawn_contacts',
    ]) &&
    side(v.side) &&
    square(v.square) &&
    ['doubled', 'isolated', 'passed', 'geometrically_protected_passer'].every(
      key => typeof v[key] === 'boolean',
    ) &&
    (v.front_square === null || square(v.front_square)) &&
    (v.front_occupant === null || piece(v.front_occupant)) &&
    list(v.component_pawns, square) &&
    unique(v.component_pawns) &&
    list(v.enemy_pawn_contacts, square) &&
    unique(v.enemy_pawn_contacts) &&
    Array.isArray(v.connections) &&
    v.connections.every(
      c =>
        isObject(c) &&
        hasExactKeys(c, ['peer', 'kind', 'direction']) &&
        square(c.peer) &&
        (c.kind === 'geometric_support'
          ? c.direction === 'supports' || c.direction === 'supported_by'
          : c.kind === 'phalanx' && c.direction === 'peer'),
    );
  const structuralSubject = (v: unknown): v is StructuralSubject => {
    if (!isObject(v)) return false;
    const file = typeof v.file === 'string' && /^[a-h]$/.test(v.file);
    if (v.kind === 'open_file') return hasExactKeys(v, ['kind', 'file']) && file;
    if (!side(v.side)) return false;
    if (v.kind === 'semi_open_file') return hasExactKeys(v, ['kind', 'side', 'file']) && file;
    if (v.kind === 'passed_pawn_created' || v.kind === 'passed_pawn_lost')
      return hasExactKeys(v, ['kind', 'side', 'square']) && square(v.square);
    if (!square(v.from) || !square(v.to)) return false;
    if (v.kind === 'passed_pawn_advanced' || v.kind === 'passed_status_created')
      return (
        hasExactKeys(v, ['kind', 'side', 'from', 'to', 'relative_rank']) &&
        integerInRange(v.relative_rank, 1, 8) !== undefined
      );
    return (
      hasExactKeys(v, ['kind', 'side', 'from', 'to']) &&
      (v.kind === 'pawn_tension_created' ||
        v.kind === 'pawn_tension_resolved' ||
        v.kind === 'passed_pawn_promoted')
    );
  };
  const structuralChange = (
    v: unknown,
  ): v is Extract<MoveReviewObservation, { kind: 'structural_change' }>['changes'][number] =>
    isObject(v) &&
    hasExactKeys(v, ['kind', 'subjects', 'targets']) &&
    typeof v.kind === 'string' &&
    Object.prototype.hasOwnProperty.call(copies['en-US'].structuralChangeLabels, v.kind) &&
    list(v.subjects, structuralSubject) &&
    list(v.targets, structuralSubject);
  const controlPoint = (v: unknown): PieceControlPoint | undefined => {
    if (
      !isObject(v) ||
      !hasExactKeys(v, ['controller', 'target', 'state', 'premise_ids']) ||
      (v.controller !== null && !piece(v.controller)) ||
      (v.target !== null && !piece(v.target)) ||
      !Array.isArray(v.premise_ids) ||
      !v.premise_ids.every(id => !!semanticId(id)) ||
      !unique(v.premise_ids) ||
      (v.state !== 'present' && v.state !== 'absent' && v.state !== 'unknown' && v.state !== 'removed') ||
      (v.state === 'removed'
        ? v.controller !== null && v.target !== null
        : v.controller === null || v.target === null) ||
      (v.state === 'unknown' ? v.premise_ids.length !== 0 : v.premise_ids.length === 0)
    )
      return;
    return {
      controller: v.controller,
      target: v.target,
      state: v.state,
      premiseIds: v.premise_ids as string[],
    };
  };
  const commonKeys = ['occurrence_id', 'side', 'role', 'from', 'to', 'premise_ids', 'kind'];
  for (const raw of value.statements) {
    if (
      !isObject(raw) ||
      !semanticId(raw.occurrence_id) ||
      (raw.side !== 'white' && raw.side !== 'black') ||
      !role(raw.role) ||
      !square(raw.from) ||
      !square(raw.to) ||
      !Array.isArray(raw.premise_ids) ||
      raw.premise_ids.length === 0 ||
      !raw.premise_ids.every(id => !!semanticId(id)) ||
      !unique(raw.premise_ids) ||
      occurrences.get(raw.occurrence_id as string)?.uci.slice(0, 4) !== raw.from + raw.to
    )
      return;
    const common = {
      occurrenceId: raw.occurrence_id as string,
      side: raw.side as Color,
      role: raw.role,
      from: raw.from,
      to: raw.to,
      premiseIds: raw.premise_ids as string[],
    };
    if (raw.kind === 'capture') {
      if (
        !hasExactKeys(raw, [...commonKeys, 'captured_role', 'captured_square', 'recapture']) ||
        !role(raw.captured_role) ||
        !square(raw.captured_square) ||
        (raw.recapture !== 'proven' && raw.recapture !== 'excluded' && raw.recapture !== 'unknown')
      )
        return;
      statements.push({
        ...common,
        side: raw.side,
        kind: 'capture',
        capturedRole: raw.captured_role,
        capturedSquare: raw.captured_square,
        recapture: raw.recapture,
      });
    } else if (raw.kind === 'development') {
      if (
        !hasExactKeys(raw, [...commonKeys, 'central_squares']) ||
        !Array.isArray(raw.central_squares) ||
        !unique(raw.central_squares) ||
        !raw.central_squares.every(v => typeof v === 'string' && ['d4', 'e4', 'd5', 'e5'].includes(v))
      )
        return;
      statements.push({
        ...common,
        side: raw.side,
        kind: 'development',
        centralSquares: raw.central_squares as string[],
      });
    } else if (raw.kind === 'piece_control') {
      if (
        !hasExactKeys(raw, [...commonKeys, 'relation', 'before', 'after']) ||
        (raw.relation !== 'support' && raw.relation !== 'attack')
      )
        return;
      const before = controlPoint(raw.before);
      const after = controlPoint(raw.after);
      if (!before || !after || !(before.controller ?? after.controller) || !(before.target ?? after.target))
        return;
      for (const point of [before, after]) {
        if (
          point.controller &&
          point.target &&
          (point.controller.side === point.target.side) !== (raw.relation === 'support')
        )
          return;
      }
      statements.push({ ...common, kind: 'piece_control', relation: raw.relation, before, after });
    } else if (raw.kind === 'recapture_resources') {
      if (
        !hasExactKeys(raw, [
          ...commonKeys,
          'phase',
          'captured',
          'geometric_recapturers',
          'legal_recaptures',
          'restricted_recaptures',
        ]) ||
        raw.phase !== 'after' ||
        !piece(raw.captured) ||
        !list(raw.geometric_recapturers, locatedPiece) ||
        !list(raw.legal_recaptures, legalResource) ||
        !list(raw.restricted_recaptures, restrictedRecapture)
      )
        return;
      statements.push({
        ...common,
        kind: raw.kind,
        phase: raw.phase,
        captured: raw.captured,
        geometric_recapturers: raw.geometric_recapturers,
        legal_recaptures: raw.legal_recaptures,
        restricted_recaptures: raw.restricted_recaptures,
      });
    } else if (raw.kind === 'check_responses') {
      if (
        !hasExactKeys(raw, [
          ...commonKeys,
          'phase',
          'checked_side',
          'king_square',
          'checkers',
          'responses',
          'controlled_king_destinations',
          'terminal',
        ]) ||
        raw.phase !== 'after' ||
        !side(raw.checked_side) ||
        !square(raw.king_square) ||
        !list(raw.checkers, locatedPiece) ||
        !raw.checkers.length ||
        !list(raw.responses, checkResponse) ||
        !list(raw.controlled_king_destinations, controlledDestination) ||
        (raw.terminal !== 'ongoing' && raw.terminal !== 'checkmate')
      )
        return;
      statements.push({
        ...common,
        kind: raw.kind,
        phase: raw.phase,
        checked_side: raw.checked_side,
        king_square: raw.king_square,
        checkers: raw.checkers,
        responses: raw.responses,
        controlled_king_destinations: raw.controlled_king_destinations,
        terminal: raw.terminal,
      });
    } else if (raw.kind === 'check_response') {
      if (
        !hasExactKeys(raw, [
          ...commonKeys,
          'phase',
          'responding_side',
          'king_square',
          'checkers',
          'response',
        ]) ||
        raw.phase !== 'before' ||
        !side(raw.responding_side) ||
        !square(raw.king_square) ||
        !list(raw.checkers, locatedPiece) ||
        !raw.checkers.length ||
        !checkResponse(raw.response)
      )
        return;
      statements.push({
        ...common,
        kind: raw.kind,
        phase: raw.phase,
        responding_side: raw.responding_side,
        king_square: raw.king_square,
        checkers: raw.checkers,
        response: raw.response,
      });
    } else if (raw.kind === 'pawn_structure') {
      if (
        !hasExactKeys(raw, [...commonKeys, 'phase', 'before', 'after', 'changed_facets']) ||
        raw.phase !== 'transition' ||
        (raw.before !== null && !pawnState(raw.before)) ||
        (raw.after !== null && !pawnState(raw.after)) ||
        (raw.before === null && raw.after === null) ||
        !Array.isArray(raw.changed_facets) ||
        !raw.changed_facets.length ||
        !unique(raw.changed_facets) ||
        !raw.changed_facets.every(
          facet =>
            typeof facet === 'string' &&
            Object.prototype.hasOwnProperty.call(copies['en-US'].pawnFacetLabels, facet),
        )
      )
        return;
      statements.push({
        ...common,
        kind: raw.kind,
        phase: raw.phase,
        before: raw.before,
        after: raw.after,
        changed_facets: raw.changed_facets as PawnFacet[],
      });
    } else if (raw.kind === 'stalemate') {
      if (
        !hasExactKeys(raw, [...commonKeys, 'phase', 'stalled_side', 'king_square']) ||
        raw.phase !== 'after' ||
        !side(raw.stalled_side) ||
        !square(raw.king_square)
      )
        return;
      statements.push({
        ...common,
        kind: raw.kind,
        phase: raw.phase,
        stalled_side: raw.stalled_side,
        king_square: raw.king_square,
      });
    } else if (raw.kind === 'castling') {
      if (
        !hasExactKeys(raw, [...commonKeys, 'rook']) ||
        !isObject(raw.rook) ||
        !hasExactKeys(raw.rook, ['from', 'to']) ||
        !square(raw.rook.from) ||
        !square(raw.rook.to)
      )
        return;
      statements.push({ ...common, kind: raw.kind, rook: { from: raw.rook.from, to: raw.rook.to } });
    } else if (raw.kind === 'promotion') {
      if (
        !hasExactKeys(raw, [...commonKeys, 'promoted_role']) ||
        !role(raw.promoted_role) ||
        raw.promoted_role === 'pawn' ||
        raw.promoted_role === 'king'
      )
        return;
      statements.push({ ...common, kind: raw.kind, promoted_role: raw.promoted_role });
    } else if (raw.kind === 'structural_change') {
      if (
        !hasExactKeys(raw, [...commonKeys, 'phase', 'changes']) ||
        raw.phase !== 'transition' ||
        !list(raw.changes, structuralChange)
      )
        return;
      statements.push({ ...common, kind: raw.kind, phase: raw.phase, changes: raw.changes });
    } else return;
  }
  const inputIssues: MoveReviewInputIssue[] = [];
  if (value.input_issues !== undefined) {
    if (!Array.isArray(value.input_issues)) return;
    for (const raw of value.input_issues) {
      if (!isObject(raw) || !hasExactKeys(raw, ['input_line_index', 'accepted_ply_count', 'reason'])) return;
      const index = integerInRange(raw.input_line_index, 0, (subject.inputLines?.length ?? 0) - 1);
      if (index === undefined) return;
      const count = integerInRange(raw.accepted_ply_count, 0, subject.inputLines![index]!.moves.length - 1);
      if (
        count === undefined ||
        (raw.reason !== 'invalid_continuation' &&
          raw.reason !== 'automatic_terminal' &&
          raw.reason !== 'input_limit')
      )
        return;
      inputIssues.push({ inputLineIndex: index, acceptedPlyCount: count, reason: raw.reason });
    }
    if (!unique(inputIssues.map(issue => issue.inputLineIndex))) return;
  }
  const ids = (v: unknown): v is string[] =>
    Array.isArray(v) && v.length > 0 && v.every(id => !!semanticId(id)) && unique(v);
  const squares = (v: unknown): v is Key[] => list(v, square) && unique(v);
  for (const [index, rawPath] of value.paths.entries()) {
    const raw = (rawPath as Record<string, unknown>).line_summary;
    if (raw === undefined) continue;
    const path = paths[index]!;
    const owns = (v: Record<string, unknown>): boolean => {
      const offset = integerInRange(v.ply_offset, 0, path.moves.length - 1);
      return (
        offset !== undefined &&
        !!semanticId(v.occurrence_id) &&
        path.occurrenceIds[offset] === v.occurrence_id &&
        (v.move_uci === undefined || path.moves[offset]!.uci === v.move_uci)
      );
    };
    const locationKeys = ['ply_offset', 'occurrence_id', 'move_uci'];
    if (
      !isObject(raw) ||
      !hasExactKeys(raw, [
        'scope',
        'perspective',
        'premise_ids',
        'events',
        'ending',
        ...(raw.material !== undefined ? ['material'] : []),
      ]) ||
      raw.scope !== 'observed_line' ||
      !side(raw.perspective) ||
      raw.perspective !== parseFen(path.startFen).unwrap().turn ||
      !ids(raw.premise_ids) ||
      raw.premise_ids.length !== 1 ||
      !Array.isArray(raw.events) ||
      !raw.events.every(
        event =>
          isObject(event) &&
          hasExactKeys(event, [
            'kind',
            ...locationKeys,
            ...['side', 'piece_role', 'target_role', 'square'].filter(key => event[key] !== undefined),
          ]) &&
          owns(event) &&
          validUciMoves([event.move_uci], 1, 1) &&
          [
            'capture',
            'recapture',
            'check_evasion',
            'castling',
            'check',
            'mate',
            'stalemate',
            'promotion',
          ].includes(event.kind as string) &&
          (event.side === undefined || side(event.side)) &&
          (event.piece_role === undefined || role(event.piece_role)) &&
          (event.target_role === undefined || role(event.target_role)) &&
          (event.square === undefined || square(event.square)),
      ) ||
      !isObject(raw.ending) ||
      !hasExactKeys(raw.ending, ['kind', 'ply_offset', 'occurrence_id']) ||
      !owns(raw.ending) ||
      raw.ending.ply_offset !== path.moves.length - 1 ||
      !['line_end', 'checkmate', 'stalemate', 'insufficient_material', 'fivefold_repetition',
        'seventy_five_move_rule'].includes(raw.ending.kind as string)
    )
      return;
    const m = raw.material;
    if (
      m !== undefined &&
      (!isObject(m) ||
        !hasExactKeys(m, ['events', 'net_cp', 'max_gain_cp', 'max_loss_cp', 'recovered_after_loss']) ||
        !Number.isSafeInteger(m.net_cp) ||
        !Number.isSafeInteger(m.max_gain_cp) ||
        (m.max_gain_cp as number) < 0 ||
        !Number.isSafeInteger(m.max_loss_cp) ||
        (m.max_loss_cp as number) > 0 ||
        typeof m.recovered_after_loss !== 'boolean' ||
        !Array.isArray(m.events) ||
        !m.events.every(event => {
          if (
            !isObject(event) ||
            !hasExactKeys(event, [
              ...locationKeys,
              'side',
              'from',
              'to',
              'before_role',
              'after_role',
              'promotion_gain_cp',
              'delta_cp',
              'balance_cp',
              ...(event.capture !== undefined ? ['capture'] : []),
            ]) ||
            !owns(event) ||
            !validUciMoves([event.move_uci], 1, 1) ||
            !side(event.side) ||
            !square(event.from) ||
            !square(event.to) ||
            (event.move_uci as string).slice(0, 4) !== event.from + event.to ||
            !role(event.before_role) ||
            !role(event.after_role) ||
            !Number.isSafeInteger(event.promotion_gain_cp) ||
            (event.promotion_gain_cp as number) < 0 ||
            !Number.isSafeInteger(event.delta_cp) ||
            !Number.isSafeInteger(event.balance_cp)
          )
            return false;
          const c = event.capture;
          return (
            c === undefined ||
            (isObject(c) &&
              hasExactKeys(c, ['role', 'square', 'value_cp', 'recapture']) &&
              role(c.role) &&
              square(c.square) &&
              Number.isSafeInteger(c.value_cp) &&
              (c.value_cp as number) > 0 &&
              ['proven', 'excluded', 'unknown'].includes(c.recapture as string))
          );
        }))
    )
      return;
    if (isObject(m)) {
      const events = m.events as NonNullable<ObservedLineSummary['material']>['events'];
      let balance = 0,
        maxGain = 0,
        maxLoss = 0,
        wasNegative = false,
        recovered = false;
      for (const event of events) {
        const delta = (event.capture?.value_cp ?? 0) + event.promotion_gain_cp;
        const signed = event.side === raw.perspective ? delta : -delta;
        balance += signed;
        if (!Number.isSafeInteger(balance) || event.delta_cp !== signed || event.balance_cp !== balance)
          return;
        maxGain = Math.max(maxGain, balance);
        maxLoss = Math.min(maxLoss, balance);
        recovered ||= wasNegative && balance >= 0;
        wasNegative ||= balance < 0;
      }
      if (
        events.some((event, i) => i > 0 && event.ply_offset <= events[i - 1]!.ply_offset) ||
        m.net_cp !== balance ||
        m.max_gain_cp !== maxGain ||
        m.max_loss_cp !== maxLoss ||
        m.recovered_after_loss !== recovered
      )
        return;
    }
    path.lineSummary = raw as unknown as ObservedLineSummary;
  }
  const sourceId = (v: unknown): v is string => typeof v === 'string' && sha256Pattern.test(v);
  const sourceIds = (v: unknown): v is string[] => list(v, sourceId) && v.length > 0 && unique(v);
  const positionResources = (v: unknown): v is PositionResources => {
    if (
      !isObject(v) ||
      !hasExactKeys(v, [
        'id',
        'fen',
        'premise_id',
        'controls',
        'rays',
        'pawn_advances',
        'legal_moves',
        'pawn_states',
        'pawn_files',
      ]) ||
      !semanticId(v.id) ||
      !fenText(v.fen) ||
      !semanticId(v.premise_id)
    )
      return false;
    const parsed = parseFen(v.fen as string);
    if (parsed.isErr) return false;
    const board = parsed.value.board;
    const occupied = (p: ObservedPiece): boolean => {
      const actual = board.get(parseSquare(p.square)!);
      return actual?.color === p.side && actual.role === p.role;
    };
    const c = v.controls,
      r = v.rays,
      l = v.legal_moves,
      ps = v.pawn_states,
      pf = v.pawn_files,
      a = v.pawn_advances;
    const pawnSquares = Array.from({ length: 64 }, (_, i) => i).filter(i => board.get(i)?.role === 'pawn');
    const nullableBoolean = (value: unknown) => value === null || typeof value === 'boolean';
    return (
      isObject(ps) &&
      hasExactKeys(ps, ['complete', 'pawns']) &&
      typeof ps.complete === 'boolean' &&
      Array.isArray(ps.pawns) &&
      ps.pawns.length === pawnSquares.length &&
      ps.pawns.every(
        entry =>
          isObject(entry) &&
          hasExactKeys(entry, ['square', 'passed', 'isolated', 'doubled', 'source_ids']) &&
          square(entry.square) &&
          board.get(parseSquare(entry.square)!)?.role === 'pawn' &&
          nullableBoolean(entry.passed) &&
          nullableBoolean(entry.isolated) &&
          nullableBoolean(entry.doubled) &&
          list(entry.source_ids, sourceId) &&
          unique(entry.source_ids) &&
          [entry.passed, entry.isolated, entry.doubled].some(x => x !== null) === entry.source_ids.length > 0,
      ) &&
      unique(ps.pawns.map(entry => entry.square)) &&
      ps.complete ===
        ps.pawns.every(entry => entry.passed !== null && entry.isolated !== null && entry.doubled !== null) &&
      isObject(pf) &&
      hasExactKeys(pf, ['complete', 'files']) &&
      typeof pf.complete === 'boolean' &&
      Array.isArray(pf.files) &&
      pf.files.length <= 8 &&
      (pf.complete ? pf.files.length === 8 : pf.files.length < 8) &&
      pf.files.every(
        entry =>
          isObject(entry) &&
          hasExactKeys(entry, ['file', 'occupancy', 'source_ids']) &&
          typeof entry.file === 'string' &&
          /^[a-h]$/.test(entry.file) &&
          ['open', 'white_pawns_only', 'black_pawns_only', 'both_sides'].includes(
            entry.occupancy as string,
          ) &&
          sourceIds(entry.source_ids) &&
          entry.source_ids.length === 2,
      ) &&
      unique(pf.files.map(entry => entry.file)) &&
      isObject(l) &&
      hasExactKeys(l, ['complete', 'moves']) &&
      typeof l.complete === 'boolean' &&
      Array.isArray(l.moves) &&
      l.moves.every(entry => {
        if (
          !isObject(entry) ||
          !hasExactKeys(entry, [
            'uci',
            'destination',
            'source_id',
            ...(entry.captured_square === undefined ? [] : ['captured_square']),
            ...(entry.gives_check === undefined ? [] : ['gives_check']),
          ]) ||
          (entry.gives_check !== undefined && typeof entry.gives_check !== 'boolean') ||
          !validUciMoves([entry.uci], 1, 1) ||
          !square(entry.destination) ||
          !sourceId(entry.source_id)
        )
          return false;
        const from = (entry.uci as string).slice(0, 2),
          to = (entry.uci as string).slice(2, 4),
          actor = board.get(parseSquare(from)!);
        if (actor?.color !== parsed.value.turn) return false;
        if (entry.captured_square !== undefined) {
          if (!square(entry.captured_square)) return false;
          const captured = board.get(parseSquare(entry.captured_square)!);
          if (!captured || captured.color === actor.color) return false;
          // Bind supplied occupancy; the server owns capture/en-passant legality.
          if (
            entry.captured_square !== entry.destination &&
            (actor.role !== 'pawn' ||
              captured.role !== 'pawn' ||
              board.has(parseSquare(entry.destination)!) ||
              entry.captured_square !== `${to[0]}${from[1]}`)
          )
            return false;
        }
        if (entry.destination === to) return true;
        // A castling UCI may target the rook; this checks the supplied mapping,
        // without regenerating move legality or a second legal catalogue.
        const rook = board.get(parseSquare(to)!);
        return (
          actor.role === 'king' &&
          rook?.role === 'rook' &&
          rook.color === actor.color &&
          from[1] === to[1] &&
          entry.destination === `${to[0] > from[0] ? 'g' : 'c'}${from[1]}`
        );
      }) &&
      unique(l.moves.map(entry => entry.uci)) &&
      unique(l.moves.map(entry => entry.source_id)) &&
      isObject(c) &&
      hasExactKeys(c, ['complete', 'pieces']) &&
      typeof c.complete === 'boolean' &&
      Array.isArray(c.pieces) &&
      c.pieces.every(
        entry =>
          isObject(entry) &&
          hasExactKeys(entry, ['piece', 'squares', 'source_ids']) &&
          piece(entry.piece) &&
          occupied(entry.piece) &&
          squares(entry.squares) &&
          sourceIds(entry.source_ids) &&
          entry.source_ids.length === entry.squares.length,
      ) &&
      unique(
        c.pieces.map(entry => (entry as PositionResources['controls']['pieces'][number]).piece.square),
      ) &&
      isObject(r) &&
      hasExactKeys(r, ['complete', 'lines']) &&
      typeof r.complete === 'boolean' &&
      Array.isArray(r.lines) &&
      r.lines.every(entry => {
        if (
          !isObject(entry) ||
          !hasExactKeys(entry, ['piece', 'squares', 'occupants', 'source_id']) ||
          !piece(entry.piece) ||
          !['bishop', 'rook', 'queen'].includes(entry.piece.role) ||
          !occupied(entry.piece) ||
          !squares(entry.squares) ||
          !entry.squares.length ||
          !list(entry.occupants, piece) ||
          !sourceId(entry.source_id)
        )
          return false;
        // Check supplied occupancy and order, without deriving rays or attacks in the client.
        const occupiedSquares = entry.squares.filter(key => board.has(parseSquare(key)!));
        return (
          entry.occupants.length === occupiedSquares.length &&
          entry.occupants.every((p, i) => occupied(p) && p.square === occupiedSquares[i])
        );
      }) &&
      isObject(a) &&
      hasExactKeys(a, ['complete', 'paths']) &&
      typeof a.complete === 'boolean' &&
      Array.isArray(a.paths) &&
      a.paths.every(
        entry =>
          isObject(entry) &&
          hasExactKeys(entry, ['piece', 'destination', 'traversed_squares', 'source_id']) &&
          piece(entry.piece) &&
          entry.piece.role === 'pawn' &&
          occupied(entry.piece) &&
          square(entry.destination) &&
          squares(entry.traversed_squares) &&
          entry.traversed_squares.length >= 1 &&
          entry.traversed_squares.length <= 2 &&
          entry.traversed_squares[entry.traversed_squares.length - 1] === entry.destination &&
          entry.traversed_squares.every(key => !board.has(parseSquare(key)!)) &&
          sourceId(entry.source_id),
      )
    );
  };
  let positions: PositionResources[] | undefined;
  const occurrencePositions = new Map<string, (string | null)[]>();
  if (value.positions !== undefined) {
    if (!list(value.positions, positionResources) || !unique(value.positions.map(p => p.id))) return;
    positions = value.positions;
    const byId = new Map(positions.map(p => [p.id, p]));
    const incomingOccurrences = new Map<string, string>();
    for (const path of paths) {
      for (const [i, id] of path.positionIds!.entries()) {
        if (id === null) continue;
        if (byId.get(id)?.fen !== (i === 0 ? path.startFen : path.moves[i - 1]!.fenAfter)) return;
        const incoming = path.occurrenceIds[i - 1];
        // Root positions and the unknown first history edge supply no incoming identity.
        if (incoming) {
          const previous = incomingOccurrences.get(id);
          if (previous !== undefined && previous !== incoming) return;
          incomingOccurrences.set(id, incoming);
        }
      }
      for (const [i, id] of path.occurrenceIds.entries()) {
        if (id === null) continue;
        const pair = path.positionIds!.slice(i, i + 2);
        const previous = occurrencePositions.get(id);
        if (previous && previous.some((p, i) => p !== pair[i])) return;
        occurrencePositions.set(id, pair);
      }
    }
  }
  // Presentation selection is optional. A damaged highlight must not erase
  // independently decoded facts or historical paths.
  const highlights: NonNullable<MoveReviewObservations['highlights']> = [];
  const seen = new Set<string>();
  const byId = new Map(positions?.map(p => [p.id, p]));
  const positionSources = new Map<string, Set<string>>();
  const premises = new Map(Array.from(occurrences.keys(), id => [id, new Set([id])]));
  for (const statement of statements) {
    const refs = premises.get(statement.occurrenceId)!;
    for (const id of statement.premiseIds) refs.add(id);
    if (statement.kind === 'piece_control')
      for (const id of [...statement.before.premiseIds, ...statement.after.premiseIds]) refs.add(id);
  }
  for (const [occurrenceId, pair] of occurrencePositions)
    for (const id of pair) {
      const position = id ? byId.get(id) : undefined;
      if (position) premises.get(occurrenceId)!.add(position.premise_id);
    }
  if (Array.isArray(value.highlights))
    for (const raw of value.highlights) {
      const linked = isObject(raw) && ('premise_ids' in raw || 'frames' in raw);
      if (
        !isObject(raw) ||
        !hasExactKeys(raw, [
          'occurrence_id',
          'text',
          'squares',
          ...(linked ? ['premise_ids', 'frames'] : []),
        ]) ||
        typeof raw.occurrence_id !== 'string' ||
        !occurrences.has(raw.occurrence_id) ||
        typeof raw.text !== 'string' ||
        !raw.text.trim() ||
        raw.text.length > 1000 ||
        !squares(raw.squares) ||
        raw.squares.length === 0 ||
        raw.squares.length > 64
      )
        continue;
      const highlight = { occurrenceId: raw.occurrence_id, text: raw.text, squares: raw.squares };
      const frames: HighlightFrame[] = [];
      if (linked) {
        if (
          !Array.isArray(raw.premise_ids) ||
          raw.premise_ids.length > 64 ||
          !unique(raw.premise_ids) ||
          !raw.premise_ids.every(id => premises.get(raw.occurrence_id as string)!.has(id)) ||
          !Array.isArray(raw.frames) ||
          !raw.frames.length ||
          raw.frames.length > 2
        )
          continue;
        for (const frame of raw.frames) {
          if (
            !isObject(frame) ||
            !hasExactKeys(frame, ['phase', 'position_id', 'squares', 'source_ids']) ||
            (frame.phase !== 'before' && frame.phase !== 'after') ||
            typeof frame.position_id !== 'string' ||
            occurrencePositions.get(raw.occurrence_id)?.[frame.phase === 'before' ? 0 : 1] !==
              frame.position_id ||
            !squares(frame.squares) ||
            !frame.squares.length ||
            frame.squares.length > 64 ||
            !frame.squares.every(key => highlight.squares.includes(key)) ||
            !list(frame.source_ids, sourceId) ||
            !unique(frame.source_ids)
          )
            break;
          const position = byId.get(frame.position_id);
          if (!position) break;
          // Resource hashes are local to this exact historical position, never a global catalogue.
          let sources = positionSources.get(position.id);
          if (!sources) {
            sources = new Set([
              ...position.controls.pieces.flatMap(p => p.source_ids),
              ...position.rays.lines.map(r => r.source_id),
              ...position.legal_moves.moves.map(m => m.source_id),
              ...position.pawn_states.pawns.flatMap(p => p.source_ids),
              ...position.pawn_files.files.flatMap(f => f.source_ids),
              ...position.pawn_advances.paths.map(p => p.source_id),
            ]);
            positionSources.set(position.id, sources);
          }
          if (!frame.source_ids.every(id => sources.has(id))) break;
          frames.push({
            phase: frame.phase,
            positionId: frame.position_id,
            squares: frame.squares,
            sourceIds: frame.source_ids,
          });
        }
        if (frames.length !== raw.frames.length || !unique(frames.map(frame => frame.phase))) continue;
      }
      const key = `${raw.occurrence_id}:${raw.text}`;
      if (seen.has(key)) continue;
      seen.add(key);
      highlights.push(linked ? { ...highlight, premiseIds: raw.premise_ids as string[], frames } : highlight);
    }
  const result: MoveReviewObservations = {
    paths,
    statements,
    ...(highlights.length ? { highlights } : {}),
    ...(positions ? { positions } : {}),
    ...(inputIssues.length ? { inputIssues } : {}),
  };
  if (value.selection !== undefined && selectionEvidenceValid)
    result.selection = decodeCommentarySelection(value.selection, result, {
      rootFen: subject.before.fen,
      playedMove: subject.played.uci,
      engineProfile,
    });
  return result;
}

export function moveReviewSan(fen: FEN, uci: Uci): string {
  try {
    const position = setupPosition('chess', parseFen(fen).unwrap()).unwrap();
    const move = parseUci(uci);
    return move && position.isLegal(move) ? makeSan(position, move) : '?';
  } catch (_) {
    return '?';
  }
}

export function moveReviewSans(fen: FEN, moves: readonly Uci[]): string[] {
  return buildProof('display', fen, moves)?.moves.map(move => move.label) ?? [];
}

function buildProof(id: string, startFen: FEN, moves: readonly Uci[]): MoveReviewProof | undefined {
  if (!semanticId(id) || moves.length < 1 || moves.length > 600) return;
  try {
    const position = setupPosition('chess', parseFen(startFen).unwrap()).unwrap();
    const proofMoves: MoveReviewProofMove[] = [];
    for (const rawMove of moves) {
      const move = parseUci(rawMove);
      if (!move || !position.isLegal(move)) return;
      const label = makeSanAndPlay(position, move);
      proofMoves.push({ uci: rawMove, label, fenAfter: makeFen(position.toSetup()) });
    }
    return { id, startFen, moves: proofMoves };
  } catch (_) {
    return;
  }
}

function projectEndpoint(value: unknown): ProjectedEndpoint | undefined {
  if (!isObject(value) || !validUciMoves(value.moves, 1, 80)) return;
  if (value.kind === 'engine_search') {
    const winPercent = finiteNumber(value.win_percent_for_mover, 0, 100);
    const depth = integerInRange(value.depth, 1, 100);
    return winPercent !== undefined && depth !== undefined
      ? { moves: [...value.moves], winPercent }
      : undefined;
  }
  if (value.kind === 'exact_automatic_terminal') {
    const terminal = projectAutomaticTerminal(value.terminal);
    return terminal ? { moves: [...value.moves], terminal } : undefined;
  }
  return;
}

function projectDelta(value: unknown): number | undefined {
  if (!isObject(value) || (value.kind !== 'engine_evaluation' && value.kind !== 'outcome_only')) return;
  return finiteNumber(value.candidate_win_percent_delta_for_mover, -100, 100);
}

function projectAutomaticTerminal(value: unknown): MoveReviewAutomaticTerminal | undefined {
  if (!isObject(value) || typeof value.kind !== 'string') return;
  if (value.kind === 'checkmate')
    return value.winner === 'white' || value.winner === 'black'
      ? { kind: 'checkmate', winner: value.winner }
      : undefined;
  if (
    value.kind === 'stalemate' ||
    value.kind === 'insufficient_material' ||
    value.kind === 'fivefold_repetition' ||
    value.kind === 'seventy_five_move_rule'
  )
    return { kind: value.kind };
  return;
}

function projectDrawClaims(value: unknown): MoveReviewDrawClaim[] | undefined {
  if (!Array.isArray(value) || value.length < 1 || value.length > 4) return;
  const claims = value.map(projectDrawClaim);
  return claims.every((claim): claim is MoveReviewDrawClaim => !!claim) ? claims : undefined;
}

function projectDrawClaim(value: unknown): MoveReviewDrawClaim | undefined {
  if (
    !isObject(value) ||
    (value.rule !== 'threefold_repetition' && value.rule !== 'fifty_move_rule') ||
    (value.availability !== 'available_now' && value.availability !== 'available_by_declared_move')
  )
    return;
  if (value.availability === 'available_now')
    return hasExactKeys(value, ['rule', 'availability'])
      ? { rule: value.rule, availability: value.availability }
      : undefined;
  return hasExactKeys(value, ['rule', 'availability', 'move_ucis']) && validUciMoves(value.move_ucis, 1, 218)
    ? { rule: value.rule, availability: value.availability, moveUcis: [...value.move_ucis] }
    : undefined;
}

function replayFen(initialFen: FEN, moves: readonly Uci[]): FEN | undefined {
  try {
    const position = setupPosition('chess', parseFen(initialFen).unwrap()).unwrap();
    for (const rawMove of moves) {
      const move = parseUci(rawMove);
      if (!move || !position.isLegal(move)) return;
      position.play(move);
    }
    return makeFen(position.toSetup());
  } catch (_) {
    return;
  }
}

function moveReviewVerdictCode(value: unknown): MoveReviewVerdictCode | undefined {
  return typeof value === 'string' && Object.prototype.hasOwnProperty.call(verdictSymbolByCode, value)
    ? (value as MoveReviewVerdictCode)
    : undefined;
}

function projectLocalEvalScore(value: EvalScore): MoveReviewWhiteScore | undefined {
  if (value.cp !== undefined && value.mate === undefined)
    return Number.isSafeInteger(value.cp) ? { kind: 'cp', value: value.cp } : undefined;
  if (value.mate !== undefined && value.cp === undefined)
    return Number.isSafeInteger(value.mate) && value.mate !== 0
      ? { kind: 'mate', value: value.mate }
      : undefined;
  return;
}

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function hasExactKeys(value: Record<string, unknown>, required: readonly string[]): boolean {
  const keys = Object.keys(value);
  return (
    keys.length === required.length &&
    required.every(keyName => Object.prototype.hasOwnProperty.call(value, keyName))
  );
}

function hasOnlyKeys(
  value: Record<string, unknown>,
  allowed: readonly string[],
  required: readonly string[] = allowed,
): boolean {
  const keys = Object.keys(value);
  return (
    keys.every(keyName => allowed.includes(keyName)) && required.every(keyName => keys.includes(keyName))
  );
}

function arrayEquals<T>(first: readonly T[], second: readonly T[]): boolean {
  return first.length === second.length && first.every((value, index) => value === second[index]);
}

function arrayPrefix<T>(prefix: readonly T[], values: readonly T[]): boolean {
  return prefix.length <= values.length && prefix.every((value, index) => value === values[index]);
}

function unique<T>(values: readonly T[]): boolean {
  return new Set(values).size === values.length;
}

function semanticId(value: unknown): string | undefined {
  return typeof value === 'string' && value.length >= 1 ? value : undefined;
}

function fenText(value: unknown): FEN | undefined {
  return typeof value === 'string' && value.length >= 1 && value.length <= 128 ? (value as FEN) : undefined;
}

function san(value: unknown): San | undefined {
  return typeof value === 'string' && value.trim().length > 0 && value.length <= 64
    ? (value as San)
    : undefined;
}

function uci(value: unknown): Uci | undefined {
  return typeof value === 'string' && uciPattern.test(value) ? (value as Uci) : undefined;
}

function validUciMoves(value: unknown, minimum: number, maximum = Number.MAX_SAFE_INTEGER): value is Uci[] {
  return (
    Array.isArray(value) &&
    value.length >= minimum &&
    value.length <= maximum &&
    value.every(move => !!uci(move))
  );
}

function nonNegativeInteger(value: unknown): number | undefined {
  return integerInRange(value, 0, Number.MAX_SAFE_INTEGER);
}

function integerInRange(value: unknown, minimum: number, maximum: number): number | undefined {
  return typeof value === 'number' && Number.isSafeInteger(value) && value >= minimum && value <= maximum
    ? value
    : undefined;
}

function finiteNumber(value: unknown, minimum: number, maximum: number): number | undefined {
  return typeof value === 'number' && Number.isFinite(value) && value >= minimum && value <= maximum
    ? value
    : undefined;
}

const requestIdPattern = /^[A-Za-z0-9._:-]{1,128}$/;
const jobIdPattern = /^[A-Za-z0-9_-]{32}$/;
const workIdPattern = /^work:[0-9]+$/;
const sha256Pattern = /^[a-f0-9]{64}$/;
const uciPattern = /^[a-h][1-8][a-h][1-8][qrbn]?$/;
