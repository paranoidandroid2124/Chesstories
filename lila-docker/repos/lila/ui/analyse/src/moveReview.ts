import {
  isMoveReviewEngineProfile,
  type MoveReviewEngineProfile,
  type MoveReviewVariant,
} from 'lib/ceval/types';
import { makeFen, parseFen } from 'chessops/fen';
import { parseUci } from 'chessops/util';
import { setupPosition } from 'chessops/variant';

const moveReviewJobRequestSchema = 'chesstory.position-commentary.job-request.v6' as const;
const moveReviewJobStatusSchema = 'chesstory.position-commentary.job-status.v6' as const;
const moveReviewResponseSchema = 'chesstory.position-commentary.response.v6' as const;
const moveReviewEngineWorkReportSchema =
  'chesstory.position-commentary.engine-work-report.v6' as const;
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
export type MoveReviewReasonRole = 'primary' | 'support';
type MoveReviewBrush = 'green' | 'blue' | 'yellow' | 'red';
type MoveReviewMessageKey = 'move_review.reason.for_candidate';

export interface MoveReviewSubject {
  variant: MoveReviewVariant;
  initialFen: FEN;
  movePrefixUci: Uci[];
  before: { path: Tree.Path; fen: FEN };
  played: { uci: Uci; san: San };
  after: { path: Tree.Path; fen: FEN };
}

interface MoveReviewWinChance {
  referencePercent: number;
  playedPercent: number;
  changePercentagePoints: number;
}

interface MoveReviewReasonRefs {
  primary?: string;
  support: string[];
}

export interface MoveReviewCore {
  verdictRef: string;
  verdictCode: MoveReviewVerdictCode;
  verdictSymbol: MoveReviewVerdictSymbol;
  playedUci: Uci;
  bestUci: Uci;
  winChance?: MoveReviewWinChance;
  referenceTerminal?: MoveReviewAutomaticTerminal;
  reviewedTerminal?: MoveReviewAutomaticTerminal;
  reasonRefs: MoveReviewReasonRefs;
}

export type MoveReviewAnnotationShape =
  | { kind: 'arrow'; orig: Key; dest: Key; brush: MoveReviewBrush }
  | { kind: 'square'; key: Key; brush: MoveReviewBrush };

interface MoveReviewAnnotation {
  atPly: number;
  shape: MoveReviewAnnotationShape;
}

interface MoveReviewProofMove {
  uci: Uci;
  label: string;
  fenAfter: FEN;
}

export interface MoveReviewProof {
  id: string;
  startFen: FEN;
  moves: MoveReviewProofMove[];
  annotations: MoveReviewAnnotation[];
}

export interface MoveReviewCandidate {
  uci: Uci;
  label: string;
  roles: MoveReviewCandidateRole[];
  winPercent?: number;
  review: MoveReviewCandidateReview;
}

export interface MoveReviewReason {
  id: string;
  messageKey: MoveReviewMessageKey;
  messageSlots: { candidateUci: Uci };
  proof: MoveReviewProof;
}

export type MoveReviewCandidateReview =
  | { kind: 'move-verdict'; core: MoveReviewCore; reasons: MoveReviewReason[] }
  | { kind: 'single-candidate-insight'; proof: MoveReviewProof }
  | {
      kind: 'forced-single-move';
      lineUcis: Uci[];
      terminal?: MoveReviewAutomaticTerminal;
    }
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
  generation: number;
  executionKeySha256: string;
  variant: MoveReviewVariant;
  enginePositionInitialFen: FEN;
  enginePositionMovesUci: Uci[];
  searchFen: FEN;
  rootRestriction: { kind: 'unrestricted' } | { kind: 'restricted'; movesUci: Uci[] };
  searchLimits: {
    depth: 16;
    nodes: 5_000_000 | 2_000_000;
    movetimeMs: 5_000 | 2_500;
    multiPv: number;
  };
  maxSearchElapsedMs: 6_000 | 3_500;
}

type MoveReviewWhiteScore = { kind: 'cp'; value: number } | { kind: 'mate'; value: number };

interface MoveReviewLineSuffix {
  multipvIndex: number;
  moves: Uci[];
  depth: number;
  whiteScore: MoveReviewWhiteScore;
  terminalEndpoint: MoveReviewTerminalEndpoint;
}

interface MoveReviewPreviousIteration {
  depth: 15;
  orderedLines: MoveReviewLineSuffix[];
}

type MoveReviewTerminalEndpoint =
  | { kind: 'none' }
  | { kind: 'checkmate'; winner: 'white' | 'black' }
  | {
      kind: 'stalemate' | 'insufficient_material' | 'fivefold_repetition' | 'seventy_five_move_rule';
    };

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
      previousIteration: MoveReviewPreviousIteration;
    }
  | {
      kind: 'executor_failed';
      executorElapsedMs: number;
      observedNodes: number;
      engineTimeMs: number;
      failureCode: string;
      diagnostic: string;
    };

export type MoveReviewCompactReceipt =
  | {
      kind: 'completed';
      workId: string;
      executionKeySha256: string;
      rootRestriction: IssuedMoveReviewEngineWork['rootRestriction'];
      searchLimits: IssuedMoveReviewEngineWork['searchLimits'];
      maxSearchElapsedMs: number;
      completedDepth: number;
      nodes: number;
      engineTimeMs: number;
      executorElapsedMs: number;
    }
  | {
      kind: 'executor_failed';
      workId: string;
      executionKeySha256: string;
      rootRestriction: IssuedMoveReviewEngineWork['rootRestriction'];
      searchLimits: IssuedMoveReviewEngineWork['searchLimits'];
      maxSearchElapsedMs: number;
      executorElapsedMs: number;
      observedNodes: number;
      engineTimeMs: number;
      failureCode: string;
      diagnostic: string;
    };

interface MoveReviewSnapshotCommon {
  requestId: string;
  jobId: string;
  engineProfile: MoveReviewEngineProfile;
  judgmentRevision: string;
  annotationPolicyRevision: string;
  generation: number;
  subject: MoveReviewSubject;
}

export type MoveReviewSnapshot =
  | (MoveReviewSnapshotCommon & {
      kind: 'awaiting-core';
      issuedEngineWork: IssuedMoveReviewEngineWork;
    })
  | (MoveReviewSnapshotCommon & {
      kind: 'awaiting-evidence';
      issuedEngineWork: IssuedMoveReviewEngineWork;
    })
  | (MoveReviewSnapshotCommon & {
      kind: 'completed';
      evidence: MoveReviewEvidence;
    })
  | (MoveReviewSnapshotCommon & {
      kind: 'position-action';
      action: MoveReviewPositionAction;
    })
  | (MoveReviewSnapshotCommon & {
      kind: 'abstained';
    });

export type MoveReviewJobState =
  | { kind: 'idle'; reason: 'root' | 'disabled' }
  | { kind: 'loading'; subject: MoveReviewSubject }
  | { kind: 'completed'; snapshot: Extract<MoveReviewSnapshot, { kind: 'completed' }> }
  | { kind: 'position-action'; snapshot: Extract<MoveReviewSnapshot, { kind: 'position-action' }> }
  | { kind: 'abstained'; subject: MoveReviewSubject }
  | {
      kind: 'fault';
      subject: MoveReviewSubject;
      message: string;
      retryable: boolean;
    }
  | {
      kind: 'unsupported';
      subject: MoveReviewSubject;
      reason: 'engine-unavailable' | 'browser-unsupported';
      message: string;
    };

export interface MoveReviewFrameSelection {
  proofId: string;
  ply: number;
}

export interface MoveReviewViewState {
  selectedCandidateUci?: Uci;
  evidenceExpanded: boolean;
  expandedReasonId?: string;
  hoveredFrame?: MoveReviewFrameSelection;
  pinnedFrame?: MoveReviewFrameSelection;
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
  engineUnavailable: string;
  browserUnsupported: string;
  retry: string;
  best: string;
  played: string;
  alternative: string;
  candidateMoves: string;
  noVerifiedReason: string;
  noPrimaryReason: string;
  lineInsight: string;
  forcedSingleMove: string;
  candidateUnavailable: string;
  analysisWithheld: string;
  terminalResult: string;
  drawClaimAvailable: string;
  terminalLabels: Record<MoveReviewAutomaticTerminal['kind'], string>;
  drawRuleLabels: Record<MoveReviewDrawClaim['rule'], string>;
  colorLabels: Record<'white' | 'black', string>;
  drawAvailabilityLabels: Record<MoveReviewDrawClaim['availability'], string>;
  primaryReason: string;
  supportingReason: string;
  reasonForCandidate: string;
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
  judgmentRevision?: string;
  annotationPolicyRevision?: string;
  generation?: number;
  reportedReceipts?: readonly MoveReviewCompactReceipt[];
  submittedReceipt?: MoveReviewCompactReceipt;
}

export interface MoveReviewSourceRequest {
  requestId: string;
  subject: MoveReviewSubject;
  engineProfile: MoveReviewEngineProfile;
}

export interface MoveReviewSource {
  run(
    request: MoveReviewSourceRequest,
    emit: (snapshot: MoveReviewSnapshot) => void,
    signal: AbortSignal,
  ): Promise<void>;
}

const requestIdPattern = /^[A-Za-z0-9._:-]{1,128}$/;
const jobIdPattern = /^[A-Za-z0-9_-]{32}$/;
const workIdPattern = /^work:[0-1]$/;
const sha256Pattern = /^[a-f0-9]{64}$/;
const squarePattern = /^[a-h][1-8]$/;
const uciPattern = /^[a-h][1-8][a-h][1-8][qrbn]?$/;
const verdictSymbolByCode: Record<MoveReviewVerdictCode, MoveReviewVerdictSymbol> = {
  improves_on_reference: 'none',
  matches_reference: 'none',
  playable_loss: 'none',
  inaccuracy: '?!',
  mistake: '?',
  blunder: '??',
};
const verdictConfidences = new Set(['engine_backed', 'legal_replay_verified', 'mixed_verified']);
const pieceRoles = new Set(['pawn', 'knight', 'bishop', 'rook', 'queen', 'king']);
const relationParticipantRoles = new Set([
  'attacker',
  'defender',
  'target',
  'blocker',
  'beneficiary',
  'king',
  'mover',
  'bait',
  'lured',
  'other',
]);
const relationProofRoles = new Set(['participant', 'line_move', 'focus', 'target']);
const relationFactKinds = new Set([
  'defender_trade',
  'deflection',
  'discovered_attack',
  'double_check',
  'back_rank_mate',
  'fork',
  'hanging_piece',
  'decoy',
  'interference',
  'clearance',
  'xray',
  'battery',
  'pin',
  'skewer',
  'domination',
  'trapped_piece',
]);
const moveReviewAbstentionStages = new Set(['fact_selection', 'need_selection', 'projection', 'failure']);
const moveReviewAbstentionPredicates = new Set([
  'no_projectable_semantic_fact',
  'no_stable_pairwise_primary',
  'semantic_projection_failed',
  'semantic_pipeline_internal_invariant',
  'comparison_anchor_rank_unstable_candidate_unsearched',
  'candidate_search_depth_below_minimum',
  'candidate_pairwise_stability_withheld',
  'comparison_anchor_mismatch',
  'focus_engine_execution_failed',
  'focus_comparison_budget_exhausted',
  'focus_comparison_need_unavailable',
]);
const moveReviewStopConditions = new Set([
  'budget_exhausted',
  'engine_execution_failed',
  'review_construction_failed',
  'insufficient_search_depth',
  'unstable_engine_result',
  'evidence_unavailable',
  'single_candidate_fact_unavailable',
  'projection_unavailable',
]);
const stoppedJobConditions = new Set([
  'deadline_exceeded',
  'budget_exhausted',
  'cancelled',
  'engine_execution_failed',
  'invalid_engine_work_report',
  'review_construction_failed',
  'move_review_preparation_failed',
]);
const generationInvalidatingStopConditions = new Set([
  'deadline_exceeded',
  'budget_exhausted',
  'cancelled',
  'invalid_engine_work_report',
  'review_construction_failed',
]);

const copies: Record<MoveReviewLocale, MoveReviewCopy> = {
  'ko-KR': {
    title: '직전 수 리뷰',
    idleTitle: '아직 리뷰할 수가 없습니다',
    idleBody: '수를 두거나 기보에서 수를 선택하세요.',
    analysing: '직전 수를 분석하고 있습니다…',
    completed: '직전 수 리뷰가 완료되었습니다.',
    liveEnginePaused: '리뷰가 끝날 때까지 실시간 엔진을 잠시 멈췄습니다.',
    unavailable: '직전 수 리뷰를 불러오지 못했습니다.',
    unsupported: '직전 수 리뷰를 실행할 수 없습니다.',
    engineUnavailable: '엔진 파일을 불러오거나 시작하지 못했습니다.',
    browserUnsupported: '이 브라우저에는 필요한 WebAssembly 기능이 없습니다.',
    retry: '다시 시도',
    best: '최선 수',
    played: '둔 수',
    alternative: '대안',
    candidateMoves: '후보 수',
    noVerifiedReason: '검증된 이유가 제공되지 않았습니다.',
    noPrimaryReason: '주요 이유가 제공되지 않았습니다.',
    lineInsight: '검증된 후보 수순',
    forcedSingleMove: '강제된 단일 수',
    candidateUnavailable: '이 후보 수의 분석이 보류되었습니다.',
    analysisWithheld: '검증 가능한 판정을 만들지 못해 분석을 보류했습니다.',
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
    primaryReason: '주요 이유',
    supportingReason: '보조 이유',
    reasonForCandidate: '후보 수에 대한 검증된 이유',
    proofStep: '단계',
    addToStudy: 'Study에 수순 추가',
    addedToStudy: 'Study에 수순을 추가했습니다.',
    viewAddedLine: '추가한 수순 보기',
    showEvidence: '근거 펼치기',
    hideEvidence: '근거 접기',
    proofBoard: '근거 수순 미니보드',
    winChance: '기준 수 → 검토 수 승리 가능성',
    winChanceChange: '검토 수의 기준 대비 변화',
    verdictLabels: {
      none: '주석 없음',
      '?!': '의심스러운 수',
      '?': '실수',
      '??': '큰 실수',
    },
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
    unsupported: 'Last move review cannot run.',
    engineUnavailable: 'The review engine could not be loaded or started.',
    browserUnsupported: 'This browser is missing the required WebAssembly features.',
    retry: 'Retry',
    best: 'Best',
    played: 'Played',
    alternative: 'Alternative',
    candidateMoves: 'Candidate moves',
    noVerifiedReason: 'No verified reason was provided.',
    noPrimaryReason: 'No primary reason was provided.',
    lineInsight: 'Verified candidate line',
    forcedSingleMove: 'Forced single move',
    candidateUnavailable: 'Analysis was withheld for this candidate.',
    analysisWithheld: 'Analysis was withheld because no verified judgment could be produced.',
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
    primaryReason: 'Primary reason',
    supportingReason: 'Supporting reason',
    reasonForCandidate: 'Verified reason for candidate',
    proofStep: 'Step',
    addToStudy: 'Add line to Study',
    addedToStudy: 'Line added to Study.',
    viewAddedLine: 'View added line',
    showEvidence: 'Show evidence',
    hideEvidence: 'Hide evidence',
    proofBoard: 'Proof line mini-board',
    winChance: 'Reference → reviewed win chance',
    winChanceChange: 'Reviewed change from reference',
    verdictLabels: {
      none: 'No annotation',
      '?!': 'Dubious',
      '?': 'Mistake',
      '??': 'Blunder',
    },
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
  const requested = locale ?? (typeof navigator === 'undefined' ? undefined : navigator.language);
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

export function moveReviewReasonRole(
  core: MoveReviewCore,
  reasonId: string,
): MoveReviewReasonRole | undefined {
  if (core.reasonRefs.primary === reasonId) return 'primary';
  if (core.reasonRefs.support.includes(reasonId)) return 'support';
  return;
}

export function formatMoveReviewPercent(value: number, locale: MoveReviewLocale): string {
  return `${new Intl.NumberFormat(locale, {
    minimumFractionDigits: 1,
    maximumFractionDigits: 1,
  }).format(value)}%`;
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
): MoveReviewSubject | undefined {
  if ((variant !== 'standard' && variant !== 'chess960') || nodeList.length < 2) return;
  const initial = nodeList[0];
  const before = nodeList[nodeList.length - 2];
  const after = nodeList[nodeList.length - 1];
  const playedUci = uci(after?.uci);
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
  const prefixNodes = nodeList.slice(1, -1);
  const movePrefixUci = prefixNodes.map(node => uci(node.uci));
  if (!movePrefixUci.every((move): move is Uci => !!move)) return;
  return {
    variant,
    initialFen: initial.fen,
    movePrefixUci,
    before: { path: currentPath.slice(0, -after.id.length), fen: before.fen },
    played: { uci: playedUci, san: playedSan },
    after: { path: currentPath, fen: after.fen },
  };
}

export function buildMoveReviewJobRequest(
  requestId: string,
  subject: MoveReviewSubject,
  engineProfile: MoveReviewEngineProfile,
): Record<string, unknown> {
  return {
    schema_version: moveReviewJobRequestSchema,
    engine_profile: engineProfile,
    request_id: requestId,
    variant: subject.variant,
    initial_fen: subject.initialFen,
    move_prefix_uci: subject.movePrefixUci,
    current_fen: subject.before.fen,
    focus: {
      kind: 'played_move',
      played_move_uci: subject.played.uci,
      resulting_fen: subject.after.fen,
    },
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
    generation: work.generation,
    execution_key_sha256: work.executionKeySha256,
    outcome:
      outcome.kind === 'executor_failed'
        ? {
            kind: outcome.kind,
            max_search_elapsed_ms: work.maxSearchElapsedMs,
            executor_elapsed_ms: outcome.executorElapsedMs,
            observed_nodes: outcome.observedNodes,
            engine_time_ms: outcome.engineTimeMs,
            failure_code: outcome.failureCode,
            diagnostic: outcome.diagnostic,
          }
          : {
            kind: outcome.kind,
            receipt: {
              root_restriction:
                work.rootRestriction.kind === 'unrestricted'
                  ? { kind: 'unrestricted' }
                  : { kind: 'restricted', moves_uci: work.rootRestriction.movesUci },
              search_limits: {
                depth: work.searchLimits.depth,
                nodes: work.searchLimits.nodes,
                movetime_ms: work.searchLimits.movetimeMs,
                multi_pv: work.searchLimits.multiPv,
              },
              admission: { minimum_completed_depth: work.searchLimits.depth },
              max_search_elapsed_ms: work.maxSearchElapsedMs,
              completed_depth: outcome.completedDepth,
              selective_depth: outcome.selectiveDepth,
              nodes: outcome.nodes,
              engine_time_ms: outcome.engineTimeMs,
              executor_elapsed_ms: outcome.executorElapsedMs,
              bestmove_uci: outcome.bestmoveUci,
              selected_source: 'final_bestmove_exact',
              line_suffixes: outcome.lineSuffixes.map(line => ({
                multipv_index: line.multipvIndex,
                moves: line.moves,
                depth: line.depth,
                white_score: line.whiteScore,
                bound: 'exact',
                terminal_endpoint: line.terminalEndpoint,
              })),
              previous_iteration: {
                depth: outcome.previousIteration.depth,
                ordered_lines: outcome.previousIteration.orderedLines.map(line => ({
                  multipv_index: line.multipvIndex,
                  move_uci: line.moves[0],
                  moves: line.moves,
                  white_score: line.whiteScore,
                  bound: 'exact',
                  terminal_endpoint: line.terminalEndpoint,
                })),
              },
            },
          },
  };
}

export function moveReviewCompactReceipt(
  work: IssuedMoveReviewEngineWork,
  outcome: MoveReviewEngineOutcome,
): MoveReviewCompactReceipt {
  const rootRestriction: IssuedMoveReviewEngineWork['rootRestriction'] =
    work.rootRestriction.kind === 'unrestricted'
      ? { kind: 'unrestricted' }
      : { kind: 'restricted', movesUci: [...work.rootRestriction.movesUci] };
  const common = {
    workId: work.workId,
    executionKeySha256: work.executionKeySha256,
    rootRestriction,
    searchLimits: { ...work.searchLimits },
    maxSearchElapsedMs: work.maxSearchElapsedMs,
  };
  return outcome.kind === 'completed'
    ? {
        ...common,
        kind: outcome.kind,
        completedDepth: outcome.completedDepth,
        nodes: outcome.nodes,
        engineTimeMs: outcome.engineTimeMs,
        executorElapsedMs: outcome.executorElapsedMs,
      }
    : {
        ...common,
        kind: outcome.kind,
        executorElapsedMs: outcome.executorElapsedMs,
        observedNodes: outcome.observedNodes,
        engineTimeMs: outcome.engineTimeMs,
        failureCode: outcome.failureCode,
        diagnostic: outcome.diagnostic,
      };
}

export function moveReviewEngineOutcomeAtRequiredDepth(
  work: IssuedMoveReviewEngineWork,
  evaluation: Tree.LocalEval,
  previousEvaluation: Tree.LocalEval | undefined,
  executorElapsedMs = evaluation.millis,
): MoveReviewEngineOutcome | undefined {
  if (
    evaluation.fen !== work.searchFen ||
    !Number.isSafeInteger(evaluation.depth) ||
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
    evaluation.pvs.length < work.searchLimits.multiPv ||
    !uci(evaluation.bestmove)
  )
    return;
  const projected = projectExactEvaluationLines(work, evaluation, work.searchLimits.depth);
  const previous = previousEvaluation
    ? projectExactEvaluationLines(work, previousEvaluation, work.searchLimits.depth - 1)
    : undefined;
  if (!projected || !previous || previousEvaluation?.depth !== work.searchLimits.depth - 1) return;
  if (work.searchLimits.multiPv > 1 && !unique(projected.map(line => line.moves[0]))) return;
  if (work.searchLimits.multiPv > 1 && !unique(previous.map(line => line.moves[0]))) return;
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
    previousIteration: { depth: 15, orderedLines: previous },
  };
}

function projectExactEvaluationLines(
  work: IssuedMoveReviewEngineWork,
  evaluation: Tree.LocalEval,
  requiredDepth: number,
): MoveReviewLineSuffix[] | undefined {
  if (
    evaluation.fen !== work.searchFen ||
    evaluation.depth !== requiredDepth ||
    evaluation.pvs.length < work.searchLimits.multiPv
  )
    return;
  const projected = evaluation.pvs.slice(0, work.searchLimits.multiPv).map((pv, index) => {
    const depth = pv.depth ?? evaluation.depth;
    if (
      pv.bound !== undefined ||
      depth !== requiredDepth ||
      !validUciMoves(pv.moves, 1, 80) ||
      (work.rootRestriction.kind === 'restricted' &&
        !work.rootRestriction.movesUci.includes(pv.moves[0]!))
    )
      return;
    const whiteScore = projectLocalEvalScore(pv);
    const terminalEndpoint = projectLineTerminalEndpoint(work, pv.moves);
    return whiteScore && terminalEndpoint
      ? {
          multipvIndex: index + 1,
          moves: [...pv.moves],
          depth,
          whiteScore,
          terminalEndpoint,
        }
      : undefined;
  });
  return projected.every((line): line is MoveReviewLineSuffix => !!line) ? projected : undefined;
}

function projectLineTerminalEndpoint(
  work: IssuedMoveReviewEngineWork,
  lineMoves: readonly Uci[],
): MoveReviewTerminalEndpoint | undefined {
  try {
    const setup = parseFen(work.enginePositionInitialFen).unwrap();
    const position = setupPosition('chess', setup).unwrap();
    const repetitions = new Map<string, number>();
    const recordPosition = (): void => {
      const key = repetitionPositionKey(makeFen(position.toSetup()));
      repetitions.set(key, (repetitions.get(key) ?? 0) + 1);
    };
    const endpoint = (): MoveReviewTerminalEndpoint => {
      if (position.isCheckmate())
        return { kind: 'checkmate', winner: position.turn === 'white' ? 'black' : 'white' };
      if (position.isStalemate()) return { kind: 'stalemate' };
      if (position.isInsufficientMaterial()) return { kind: 'insufficient_material' };
      if ((repetitions.get(repetitionPositionKey(makeFen(position.toSetup()))) ?? 0) >= 5)
        return { kind: 'fivefold_repetition' };
      if (position.halfmoves >= 150) return { kind: 'seventy_five_move_rule' };
      return { kind: 'none' };
    };
    recordPosition();
    for (const rawMove of work.enginePositionMovesUci) {
      if (endpoint().kind !== 'none') return;
      const move = parseUci(rawMove);
      if (!move || !position.isLegal(move)) return;
      position.play(move);
      recordPosition();
    }
    if (makeFen(position.toSetup()) !== work.searchFen || endpoint().kind !== 'none') return;
    for (const rawMove of lineMoves) {
      if (endpoint().kind !== 'none') return;
      const move = parseUci(rawMove);
      if (!move || !position.isLegal(move)) return;
      position.play(move);
      recordPosition();
    }
    return endpoint();
  } catch (_) {
    return;
  }
}

function repetitionPositionKey(fen: string): string {
  return fen.split(' ', 4).join(' ');
}

export function moveReviewCacheKey(
  subject: MoveReviewSubject,
  identity: {
    engineProfile: MoveReviewEngineProfile;
    judgmentRevision: string;
    annotationPolicyRevision: string;
  },
): string {
  return JSON.stringify([
    moveReviewResponseSchema,
    identity.judgmentRevision,
    identity.annotationPolicyRevision,
    identity.engineProfile,
    moveReviewSubjectKey(subject),
  ]);
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
  ]);
}

export class MoveReviewMemoryLru<Value> {
  private readonly entries = new Map<string, Value>();

  constructor(private readonly capacity = 64) {
    if (!Number.isSafeInteger(capacity) || capacity < 1)
      throw new RangeError('LRU capacity must be positive');
  }

  get size(): number {
    return this.entries.size;
  }

  get(key: string): Value | undefined {
    const value = this.entries.get(key);
    if (value === undefined) return;
    this.entries.delete(key);
    this.entries.set(key, value);
    return value;
  }

  set(key: string, value: Value): void {
    this.entries.delete(key);
    this.entries.set(key, value);
    const oldest = this.entries.keys().next().value;
    if (this.entries.size > this.capacity && oldest !== undefined) this.entries.delete(oldest);
  }
}

export function decodeMoveReviewSnapshot(
  raw: unknown,
  context: MoveReviewDecodeContext,
): MoveReviewSnapshot | undefined {
  if (!isObject(raw)) return;
  if (raw.schema_version === moveReviewJobStatusSchema)
    return raw.state === 'stopped' ? projectStoppedSnapshot(raw, context) : projectAwaitingSnapshot(raw, context);
  if (raw.schema_version === moveReviewResponseSchema) return projectCompletedSnapshot(raw, context);
  return;
}

function validPlayedMoveBudget(value: unknown): boolean {
  return (
    isObject(value) &&
    hasExactKeys(value, [
      'maximum_physical_works',
      'maximum_total_nodes',
      'maximum_configured_engine_time_ms',
      'maximum_position_wall_ms',
      'finalization_reserve_ms',
    ]) &&
    value.maximum_physical_works === 2 &&
    value.maximum_total_nodes === 7_000_000 &&
    value.maximum_configured_engine_time_ms === 7_500 &&
    value.maximum_position_wall_ms === 20_000 &&
    value.finalization_reserve_ms === 2_000
  );
}

function validDecisionTraceEnvelope(value: unknown): boolean {
  return isObject(value) && hasExactKeys(value, ['events']) && Array.isArray(value.events);
}

function receiptNodes(receipt: MoveReviewCompactReceipt): number {
  return receipt.kind === 'completed' ? receipt.nodes : 0;
}

function attemptedReceiptNodes(receipt: MoveReviewCompactReceipt): number {
  return receipt.kind === 'completed' ? receipt.nodes : receipt.observedNodes;
}

function validLifecycleProgress(
  value: unknown,
  phase: 'root_search' | 'evidence_acquisition' | 'completed' | 'stopped',
  receipts: readonly MoveReviewCompactReceipt[],
  pending?: IssuedMoveReviewEngineWork,
): boolean {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'phase',
      'legal_move_count',
      'root_candidate_lines_admitted',
      'selected_candidate_count',
      'selected_commentaries_completed',
      'selected_commentaries_abstained',
      'focus_coverage',
      'physical_works_issued',
      'physical_reports_accepted',
      'accepted_nodes',
      'configured_engine_time_ms',
    ]) ||
    value.phase !== phase ||
    integerInRange(value.legal_move_count, 0, 2_147_483_647) === undefined ||
    integerInRange(value.root_candidate_lines_admitted, 0, 3) === undefined ||
    integerInRange(value.selected_candidate_count, 0, 4) === undefined ||
    integerInRange(value.selected_commentaries_completed, 0, 4) === undefined ||
    integerInRange(value.selected_commentaries_abstained, 0, 4) === undefined ||
    (value.focus_coverage !== 'not_requested' &&
      value.focus_coverage !== 'root_bundle' &&
      value.focus_coverage !== 'supplement' &&
      value.focus_coverage !== 'unavailable')
  )
    return false;
  const configuredEngineTime = [...receipts.map(receipt => receipt.searchLimits.movetimeMs), pending?.searchLimits.movetimeMs ?? 0].reduce(
    (sum, milliseconds) => sum + milliseconds,
    0,
  );
  return (
    value.physical_works_issued === receipts.length + (pending ? 1 : 0) &&
    value.physical_reports_accepted === receipts.length &&
    value.accepted_nodes === receipts.reduce((sum, receipt) => sum + receiptNodes(receipt), 0) &&
    value.configured_engine_time_ms === configuredEngineTime &&
    (value.selected_commentaries_completed as number) + (value.selected_commentaries_abstained as number) <=
      (value.selected_candidate_count as number)
  );
}

function validTerminalMetrics(value: unknown, receipts: readonly MoveReviewCompactReceipt[]): boolean {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'position_wall_ms',
      'queue_elapsed_ms',
      'report_reduction_elapsed_ms',
      'assembly_elapsed_ms',
      'engine_reported_time_ms',
      'executor_search_elapsed_ms',
      'attempted_nodes',
      'total_nodes',
    ]) ||
    integerInRange(value.position_wall_ms, 1, 20_000) === undefined ||
    ['queue_elapsed_ms', 'report_reduction_elapsed_ms', 'assembly_elapsed_ms'].some(
      key => integerInRange(value[key], 0, 20_000) === undefined,
    )
  )
    return false;
  const engineTime = receipts.reduce((sum, receipt) => sum + receipt.engineTimeMs, 0);
  const executorTime = receipts.reduce((sum, receipt) => sum + receipt.executorElapsedMs, 0);
  const attemptedNodes = receipts.reduce((sum, receipt) => sum + attemptedReceiptNodes(receipt), 0);
  const completedNodes = receipts.reduce((sum, receipt) => sum + receiptNodes(receipt), 0);
  return (
    value.engine_reported_time_ms === engineTime &&
    value.executor_search_elapsed_ms === executorTime &&
    value.attempted_nodes === attemptedNodes &&
    value.total_nodes === completedNodes &&
    engineTime <= (value.position_wall_ms as number) &&
    executorTime <= (value.position_wall_ms as number)
  );
}

function receiptMatchesExpected(value: unknown, expected: MoveReviewCompactReceipt): boolean {
  if (!isObject(value)) return false;
  const common =
    value.work_id === expected.workId &&
    value.execution_key_sha256 === expected.executionKeySha256 &&
    value.max_search_elapsed_ms === expected.maxSearchElapsedMs &&
    isObject(value.root_restriction) &&
    (expected.rootRestriction.kind === 'unrestricted'
      ? hasExactKeys(value.root_restriction, ['kind']) && value.root_restriction.kind === 'unrestricted'
      : hasExactKeys(value.root_restriction, ['kind', 'moves_uci']) &&
        value.root_restriction.kind === 'restricted' &&
        Array.isArray(value.root_restriction.moves_uci) &&
        arrayEquals(value.root_restriction.moves_uci as unknown[], expected.rootRestriction.movesUci)) &&
    isObject(value.search_limits) &&
    hasExactKeys(value.search_limits, ['depth', 'nodes', 'movetime_ms', 'multi_pv']) &&
    value.search_limits.depth === expected.searchLimits.depth &&
    value.search_limits.nodes === expected.searchLimits.nodes &&
    value.search_limits.movetime_ms === expected.searchLimits.movetimeMs &&
    value.search_limits.multi_pv === expected.searchLimits.multiPv;
  if (!common) return false;
  if (expected.kind === 'completed')
    return (
      hasExactKeys(value, [
        'kind',
        'work_id',
        'execution_key_sha256',
        'completed_depth',
        'nodes',
        'engine_time_ms',
        'root_restriction',
        'search_limits',
        'max_search_elapsed_ms',
        'executor_elapsed_ms',
      ]) &&
      value.kind === expected.kind &&
      value.completed_depth === expected.completedDepth &&
      value.nodes === expected.nodes &&
      value.engine_time_ms === expected.engineTimeMs &&
      value.executor_elapsed_ms === expected.executorElapsedMs
    );
  return (
    hasExactKeys(value, [
      'kind',
      'work_id',
      'execution_key_sha256',
      'root_restriction',
      'search_limits',
      'max_search_elapsed_ms',
      'executor_elapsed_ms',
      'observed_nodes',
      'engine_time_ms',
      'failure_code',
      'diagnostic',
    ]) &&
    value.kind === expected.kind &&
    value.executor_elapsed_ms === expected.executorElapsedMs &&
    value.observed_nodes === expected.observedNodes &&
    value.engine_time_ms === expected.engineTimeMs &&
    value.failure_code === expected.failureCode &&
    value.diagnostic === expected.diagnostic
  );
}

function validTerminalLifecycle(
  raw: Record<string, unknown>,
  phase: 'completed' | 'stopped',
  receiptCandidates: readonly (readonly MoveReviewCompactReceipt[])[],
): boolean {
  const actualReceipts = raw.work_receipts;
  if (
    !validPlayedMoveBudget(raw.budget) ||
    !validDecisionTraceEnvelope(raw.decision_trace) ||
    !Array.isArray(actualReceipts)
  )
    return false;
  return receiptCandidates.some(receipts => {
    const workIds = receipts.map(receipt => receipt.workId);
    const executionKeys = receipts.map(receipt => receipt.executionKeySha256);
    return (
      unique(workIds) &&
      unique(executionKeys) &&
      validLifecycleProgress(raw.progress, phase, receipts) &&
      validTerminalMetrics(raw.metrics, receipts) &&
      actualReceipts.length === receipts.length &&
      receipts.every(expected => {
        const matches = actualReceipts.filter(
          value => isObject(value) && value.work_id === expected.workId,
        );
        return matches.length === 1 && receiptMatchesExpected(matches[0], expected);
      })
    );
  });
}

function projectStoppedSnapshot(
  raw: Record<string, unknown>,
  context: MoveReviewDecodeContext,
): MoveReviewSnapshot | undefined {
  const generation = nonNegativeInteger(raw.generation);
  const priorReceipts = context.reportedReceipts ?? [];
  const receiptsWithSubmission = context.submittedReceipt
    ? [...priorReceipts, context.submittedReceipt]
    : priorReceipts;
  const generationAdvanced =
    context.generation !== undefined && generation === context.generation + 1;
  const receiptCandidates = generationAdvanced
    ? [priorReceipts, receiptsWithSubmission]
    : [receiptsWithSubmission];
  if (
    !hasOnlyKeys(
      raw,
      [
        'schema_version',
        'engine_profile',
        'variant',
        'request_id',
        'job_id',
        'generation',
        'state',
        'deadline_epoch_ms',
        'focus',
        'budget',
        'progress',
        'metrics',
        'work_receipts',
        'decision_trace',
        'stop_condition',
        'budget_exhaustion',
      ],
      [
        'schema_version',
        'engine_profile',
        'variant',
        'request_id',
        'job_id',
        'generation',
        'state',
        'deadline_epoch_ms',
        'focus',
        'budget',
        'progress',
        'metrics',
        'work_receipts',
        'decision_trace',
        'stop_condition',
      ],
    ) ||
    raw.state !== 'stopped' ||
    generation === undefined ||
    nonNegativeInteger(raw.deadline_epoch_ms) === undefined ||
    !wireFocusMatches(raw.focus, context.subject) ||
    !validTerminalLifecycle(raw, 'stopped', receiptCandidates) ||
    !isMember(raw.stop_condition, stoppedJobConditions) ||
    (generationAdvanced &&
      (!context.submittedReceipt || !isMember(raw.stop_condition, generationInvalidatingStopConditions))) ||
    (raw.stop_condition === 'budget_exhausted') !==
      Object.prototype.hasOwnProperty.call(raw, 'budget_exhaustion') ||
    (Object.prototype.hasOwnProperty.call(raw, 'budget_exhaustion') && !isObject(raw.budget_exhaustion))
  )
    return;
  const common = projectSnapshotCommon(raw, context, false, generationAdvanced);
  return common ? { ...common, kind: 'abstained' } : undefined;
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

function projectAwaitingSnapshot(
  raw: Record<string, unknown>,
  context: MoveReviewDecodeContext,
): MoveReviewSnapshot | undefined {
  return (
    hasExactKeys(raw, [
      'schema_version',
      'engine_profile',
      'variant',
      'request_id',
      'job_id',
      'generation',
      'state',
      'deadline_epoch_ms',
      'focus',
      'budget',
      'progress',
      'decision_trace',
      'issued_engine_work',
    ]) &&
    raw.state === 'awaiting_engine_work'
      ? projectAwaitingEngineWork(raw, context)
      : undefined
  );
}

function projectAwaitingEngineWork(
  raw: Record<string, unknown>,
  context: MoveReviewDecodeContext,
): MoveReviewSnapshot | undefined {
  const common = projectSnapshotCommon(raw, context, false);
  const generation = nonNegativeInteger(raw.generation);
  if (
    !common ||
    generation === undefined ||
    nonNegativeInteger(raw.deadline_epoch_ms) === undefined ||
    !wireFocusMatches(raw.focus, context.subject) ||
    !validPlayedMoveBudget(raw.budget) ||
    !validDecisionTraceEnvelope(raw.decision_trace)
  )
    return;
  const issuedEngineWork = projectIssuedEngineWork(
    raw.issued_engine_work,
    common.engineProfile,
    context.subject,
    generation,
  );
  if (!issuedEngineWork) return;
  const receipts = context.submittedReceipt
    ? [...(context.reportedReceipts ?? []), context.submittedReceipt]
    : (context.reportedReceipts ?? []);
  const expectedWorkId =
    receipts.length === 0
      ? 'work:0'
      : receipts.length === 1 && receipts[0]?.kind === 'completed'
        ? 'work:1'
        : undefined;
  if (
    issuedEngineWork.workId !== expectedWorkId ||
    receipts.some(receipt => receipt.executionKeySha256 === issuedEngineWork.executionKeySha256) ||
    (receipts.length > 0 && context.generation === undefined) ||
    !validLifecycleProgress(
      raw.progress,
      issuedEngineWork.workId === 'work:0' ? 'root_search' : 'evidence_acquisition',
      receipts,
      issuedEngineWork,
    )
  )
    return;
  if (issuedEngineWork.workId === 'work:0')
    return { ...common, kind: 'awaiting-core', issuedEngineWork };
  if (issuedEngineWork.workId === 'work:1')
    return { ...common, kind: 'awaiting-evidence', issuedEngineWork };
  return;
}

function projectCompletedSnapshot(
  raw: Record<string, unknown>,
  context: MoveReviewDecodeContext,
): MoveReviewSnapshot | undefined {
  if (
    !hasExactKeys(raw, [
      'schema_version',
      'annotation_policy_revision',
      'engine_profile',
      'variant',
      'request_id',
      'job_id',
      'generation',
      'current_fen',
      'focus',
      'budget',
      'progress',
      'metrics',
      'work_receipts',
      'decision_trace',
      'result',
    ]) ||
    raw.annotation_policy_revision !== moveReviewAnnotationPolicyRevision ||
    raw.current_fen !== context.subject.before.fen ||
    !wireFocusMatches(raw.focus, context.subject) ||
    !validTerminalLifecycle(raw, 'completed', [
      context.submittedReceipt
        ? [...(context.reportedReceipts ?? []), context.submittedReceipt]
        : (context.reportedReceipts ?? []),
    ])
  )
    return;
  const common = projectSnapshotCommon(raw, context, true);
  if (!common || !isObject(raw.result)) return;
  if (raw.result.kind === 'automatic_terminal') {
    if (!hasExactKeys(raw.result, ['kind', 'terminal'])) return;
    const terminal = projectAutomaticTerminal(raw.result.terminal);
    return terminal
      ? { ...common, kind: 'position-action', action: { kind: 'automatic-terminal', terminal } }
      : undefined;
  }
  if (raw.result.kind === 'draw_claim_action') {
    if (!hasExactKeys(raw.result, ['kind', 'claims'])) return;
    const claims = projectDrawClaims(raw.result.claims);
    return claims
      ? { ...common, kind: 'position-action', action: { kind: 'draw-claim', claims } }
      : undefined;
  }
  if (raw.result.kind === 'analysis_abstention')
    return validAnalysisAbstention(raw.result) ? { ...common, kind: 'abstained' } : undefined;
  if (raw.result.kind === 'forced_single_move') {
    const drawClaims = Object.prototype.hasOwnProperty.call(raw.result, 'draw_claims')
      ? projectDrawClaims(raw.result.draw_claims)
      : undefined;
    if (
      !hasOnlyKeys(
        raw.result,
        ['kind', 'move_uci', 'supporting_endpoint', 'draw_claims'],
        ['kind', 'move_uci', 'supporting_endpoint'],
      ) ||
      raw.result.move_uci !== context.subject.played.uci ||
      (Object.prototype.hasOwnProperty.call(raw.result, 'draw_claims') && !drawClaims)
    )
      return;
    const endpoint = projectEndpoint(raw.result.supporting_endpoint);
    if (!endpoint || endpoint.moves[0] !== context.subject.played.uci) return;
    return {
      ...common,
      kind: 'completed',
      evidence: {
        ...(drawClaims ? { drawClaims } : {}),
        candidates: [
          {
            uci: context.subject.played.uci,
            label: context.subject.played.san,
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
  if (
    raw.result.kind !== 'selected_move_choices' ||
    !hasOnlyKeys(raw.result, ['kind', 'selected_move_reviews', 'draw_claims'], ['kind', 'selected_move_reviews']) ||
    !Array.isArray(raw.result.selected_move_reviews) ||
    raw.result.selected_move_reviews.length < 1 ||
    raw.result.selected_move_reviews.length > 4
  )
    return;
  const drawClaims = Object.prototype.hasOwnProperty.call(raw.result, 'draw_claims')
    ? projectDrawClaims(raw.result.draw_claims)
    : undefined;
  if (Object.prototype.hasOwnProperty.call(raw.result, 'draw_claims') && !drawClaims) return;
  const candidates = raw.result.selected_move_reviews.map(value =>
    projectSelectedReview(value, context.subject),
  );
  if (!candidates.every((candidate): candidate is MoveReviewCandidate => !!candidate)) return;
  if (!unique(candidates.map(candidate => candidate.uci))) return;
  const played = candidates.filter(candidate => candidate.roles.includes('played'));
  const best = candidates.filter(candidate => candidate.roles.includes('best'));
  if (played.length !== 1 || best.length !== 1 || played[0]?.uci !== context.subject.played.uci) return;
  const bestUci = best[0]!.uci;
  for (const candidate of candidates) {
    if (candidate.review.kind !== 'move-verdict') continue;
    if (candidate.review.core.bestUci !== bestUci) return;
    candidate.winPercent = candidate.review.core.winChance?.playedPercent;
    if (candidate.review.core.winChance && best[0]!.winPercent === undefined)
      best[0]!.winPercent = candidate.review.core.winChance.referencePercent;
  }
  return {
    ...common,
    kind: 'completed',
    evidence: { candidates, ...(drawClaims ? { drawClaims } : {}) },
  };
}

function validAnalysisAbstention(value: Record<string, unknown>): boolean {
  if (
    !hasOnlyKeys(
      value,
      ['kind', 'stage', 'predicate', 'reason', 'budget_exhaustion'],
      ['kind', 'stage', 'predicate', 'reason'],
    )
  )
    return false;
  if (value.reason === 'budget_exhausted')
    return (
      value.stage === 'need_selection' &&
      value.predicate === 'root_search_budget_exhausted' &&
      isObject(value.budget_exhaustion)
    );
  if (Object.prototype.hasOwnProperty.call(value, 'budget_exhaustion')) return false;
  if (value.reason === 'insufficient_search_depth')
    return value.stage === 'fact_selection' && value.predicate === 'candidate_search_depth_below_minimum';
  return (
    value.reason === 'unstable_engine_result' &&
    value.stage === 'candidate_admission' &&
    value.predicate === 'root_candidate_stability_withheld'
  );
}

function projectSnapshotCommon(
  raw: Record<string, unknown>,
  context: MoveReviewDecodeContext,
  completed: boolean,
  generationAdvanced = false,
): MoveReviewSnapshotCommon | undefined {
  if (requestId(raw.request_id) !== context.requestId || !requestId(context.requestId)) return;
  const jobId = projectJobId(raw.job_id);
  const engineProfile = isMoveReviewEngineProfile(raw.engine_profile) ? raw.engine_profile : undefined;
  const generation = nonNegativeInteger(raw.generation);
  if (
    !jobId ||
    !engineProfile ||
    generation === undefined ||
    raw.variant !== context.subject.variant ||
    (context.jobId !== undefined && context.jobId !== jobId) ||
    (context.engineProfile !== undefined && context.engineProfile !== engineProfile) ||
    (context.generation !== undefined &&
      generation !== context.generation + (generationAdvanced ? 1 : 0)) ||
    (context.judgmentRevision !== undefined && context.judgmentRevision !== moveReviewResponseSchema) ||
    (context.annotationPolicyRevision !== undefined &&
      context.annotationPolicyRevision !== moveReviewAnnotationPolicyRevision) ||
    (completed && raw.annotation_policy_revision !== moveReviewAnnotationPolicyRevision)
  )
    return;
  return {
    requestId: context.requestId,
    jobId,
    engineProfile,
    judgmentRevision: moveReviewResponseSchema,
    annotationPolicyRevision: moveReviewAnnotationPolicyRevision,
    generation,
    subject: context.subject,
  };
}

function wireFocusMatches(value: unknown, expected: MoveReviewSubject): boolean {
  return (
    isObject(value) &&
    hasExactKeys(value, ['kind', 'played_move_uci', 'resulting_fen']) &&
    value.kind === 'played_move' &&
    value.played_move_uci === expected.played.uci &&
    value.resulting_fen === expected.after.fen
  );
}

function projectIssuedEngineWork(
  value: unknown,
  expectedProfile: MoveReviewEngineProfile,
  subject: MoveReviewSubject,
  expectedGeneration: number,
): IssuedMoveReviewEngineWork | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'work_id',
      'generation',
      'engine_profile',
      'execution_key_sha256',
      'variant',
      'engine_position_initial_fen',
      'engine_position_moves_uci',
      'search_fen',
      'root_restriction',
      'search_limits',
      'admission',
      'max_search_elapsed_ms',
    ]) ||
    !workIdPattern.test(typeof value.work_id === 'string' ? value.work_id : '') ||
    value.generation !== expectedGeneration ||
    value.engine_profile !== expectedProfile ||
    !sha256Pattern.test(typeof value.execution_key_sha256 === 'string' ? value.execution_key_sha256 : '') ||
    value.variant !== subject.variant ||
    value.engine_position_initial_fen !== subject.initialFen ||
    !validUciMoves(value.engine_position_moves_uci, 0, 600) ||
    !arrayEquals(value.engine_position_moves_uci, subject.movePrefixUci) ||
    value.search_fen !== subject.before.fen ||
    !isObject(value.root_restriction) ||
    !isObject(value.search_limits) ||
    !hasExactKeys(value.search_limits, ['depth', 'nodes', 'movetime_ms', 'multi_pv']) ||
    !isObject(value.admission) ||
    !hasExactKeys(value.admission, ['minimum_completed_depth']) ||
    value.admission.minimum_completed_depth !== 16 ||
    value.search_limits.depth !== 16
  )
    return;
  const rootShape =
    value.work_id === 'work:0' &&
    hasExactKeys(value.root_restriction, ['kind']) &&
    value.root_restriction.kind === 'unrestricted' &&
    value.search_limits.nodes === 5_000_000 &&
    value.search_limits.movetime_ms === 5_000 &&
    integerInRange(value.search_limits.multi_pv, 1, 3) !== undefined &&
    value.max_search_elapsed_ms === 6_000;
  const comparisonShape =
    value.work_id === 'work:1' &&
    hasExactKeys(value.root_restriction, ['kind', 'moves_uci']) &&
    value.root_restriction.kind === 'restricted' &&
    validUciMoves(value.root_restriction.moves_uci, 2, 2) &&
    unique(value.root_restriction.moves_uci) &&
    value.root_restriction.moves_uci.includes(subject.played.uci) &&
    value.search_limits.nodes === 2_000_000 &&
    value.search_limits.movetime_ms === 2_500 &&
    value.search_limits.multi_pv === 2 &&
    value.max_search_elapsed_ms === 3_500;
  if (!rootShape && !comparisonShape) return;
  const restrictedMoves = value.root_restriction.moves_uci;
  return {
    engineProfile: expectedProfile,
    workId: value.work_id as string,
    generation: expectedGeneration,
    executionKeySha256: value.execution_key_sha256 as string,
    variant: subject.variant,
    enginePositionInitialFen: subject.initialFen,
    enginePositionMovesUci: [...value.engine_position_moves_uci],
    searchFen: subject.before.fen,
    rootRestriction: rootShape
      ? { kind: 'unrestricted' }
      : { kind: 'restricted', movesUci: [...(restrictedMoves as Uci[])] },
    searchLimits: {
      depth: 16,
      nodes: value.search_limits.nodes as 5_000_000 | 2_000_000,
      movetimeMs: value.search_limits.movetime_ms as 5_000 | 2_500,
      multiPv: value.search_limits.multi_pv as number,
    },
    maxSearchElapsedMs: value.max_search_elapsed_ms as 6_000 | 3_500,
  };
}

interface ProjectedEndpoint {
  kind: 'engine_search' | 'exact_automatic_terminal';
  moves: Uci[];
  winPercent?: number;
  terminal?: MoveReviewAutomaticTerminal;
}

interface ProjectedEpisode {
  id: string;
  role: 'reviewed' | 'reference';
  rootMove: Uci;
  lineMoves: Uci[];
  proof: MoveReviewProof;
  selectedCarriers: Array<{ id: string; order: number }>;
}

function projectSelectedReview(
  value: unknown,
  subject: MoveReviewSubject,
): MoveReviewCandidate | undefined {
  if (
    !isObject(value) ||
    integerInRange(value.legal_move_index, 0, 2_147_483_647) === undefined ||
    !uci(value.move_uci) ||
    !isObject(value.selection)
  )
    return;
  const move = value.move_uci as Uci;
  const roles = projectSelectionRoles(value.selection);
  if (!roles || roles.includes('played') !== (move === subject.played.uci)) return;

  let review: MoveReviewCandidateReview | undefined;
  if (hasExactKeys(value, ['legal_move_index', 'move_uci', 'selection', 'commentary']))
    review = projectMoveCommentary(value.commentary, move, roles, subject);
  else if (
    hasExactKeys(value, ['legal_move_index', 'move_uci', 'selection', 'abstention']) &&
    validMoveReviewAbstention(value.abstention)
  )
    review = { kind: 'abstained' };
  if (!review) return;
  return {
    uci: move,
    label: move === subject.played.uci ? subject.played.san : move,
    roles,
    review,
  };
}

function projectSelectionRoles(value: Record<string, unknown>): MoveReviewCandidateRole[] | undefined {
  if (value.kind === 'root_candidate' && hasExactKeys(value, ['kind', 'root_rank'])) {
    const rank = integerInRange(value.root_rank, 1, 3);
    return rank === undefined ? undefined : [rank === 1 ? 'best' : 'alternative'];
  }
  if (value.kind === 'single_line_candidate' && hasExactKeys(value, ['kind'])) return ['best'];
  if (
    value.kind === 'played_focus' &&
    hasExactKeys(value, ['kind', 'focus_coverage']) &&
    (value.focus_coverage === 'supplement' || value.focus_coverage === 'unavailable')
  )
    return ['played'];
  if (
    value.kind === 'root_candidate_and_played_focus' &&
    hasExactKeys(value, ['kind', 'root_rank', 'focus_coverage']) &&
    value.focus_coverage === 'root_bundle'
  ) {
    const rank = integerInRange(value.root_rank, 1, 3);
    return rank === undefined ? undefined : [rank === 1 ? 'best' : 'alternative', 'played'];
  }
  if (
    value.kind === 'single_line_candidate_and_played_focus' &&
    hasExactKeys(value, ['kind', 'focus_coverage']) &&
    value.focus_coverage === 'root_bundle'
  )
    return ['best', 'played'];
  return;
}

function validMoveReviewAbstention(value: unknown): boolean {
  if (
    !isObject(value) ||
    !hasOnlyKeys(
      value,
      ['stage', 'predicate', 'budget_exhaustion', 'stop_condition'],
      ['stage', 'predicate', 'stop_condition'],
    ) ||
    !isMember(value.stage, moveReviewAbstentionStages) ||
    !isMember(value.predicate, moveReviewAbstentionPredicates) ||
    !isMember(value.stop_condition, moveReviewStopConditions)
  )
    return false;
  const budget = value.predicate === 'focus_comparison_budget_exhausted';
  return budget
    ? value.stage === 'need_selection' &&
        value.stop_condition === 'budget_exhausted' &&
        isObject(value.budget_exhaustion)
    : value.stop_condition !== 'budget_exhausted' &&
        !Object.prototype.hasOwnProperty.call(value, 'budget_exhaustion');
}

function projectMoveCommentary(
  value: unknown,
  move: Uci,
  roles: MoveReviewCandidateRole[],
  subject: MoveReviewSubject,
): MoveReviewCandidateReview | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['primary', 'position_context', 'semantic_episodes']) ||
    !isObject(value.position_context) ||
    !Array.isArray(value.semantic_episodes) ||
    value.semantic_episodes.length < 1 ||
    value.semantic_episodes.length > 2
  )
    return;
  const boardEvidenceId = projectPositionContext(value.position_context, subject);
  if (!boardEvidenceId) return;
  const episodes = value.semantic_episodes.map(rawEpisode =>
    projectSemanticEpisode(rawEpisode, subject, boardEvidenceId),
  );
  if (!episodes.every((episode): episode is ProjectedEpisode => !!episode) || !unique(episodes.map(e => e.id)))
    return;
  const primary = value.primary;
  if (!isObject(primary)) return;
  if (primary.kind === 'single_candidate_insight') {
    const semanticEpisodeIds = primary.semantic_episode_ids;
    if (
      !roles.includes('best') ||
      !hasExactKeys(primary, [
        'kind',
        'line_evidence_id',
        'move_uci',
        'line_moves',
        'semantic_episode_ids',
      ]) ||
      primary.move_uci !== move ||
      !validUciMoves(primary.line_moves, 1, 6) ||
      !Array.isArray(semanticEpisodeIds) ||
      semanticEpisodeIds.length !== 1
    )
      return;
    const episode = episodes.find(candidate => candidate.id === semanticEpisodeIds[0]);
    if (
      !episode ||
      episodes.length !== 1 ||
      episode.role !== 'reviewed' ||
      episode.rootMove !== move ||
      episode.selectedCarriers.length !== 0 ||
      primary.line_evidence_id !== episode.id ||
      !arrayEquals(primary.line_moves, episode.proof.moves.map(proofMove => proofMove.uci))
    )
      return;
    return { kind: 'single-candidate-insight', proof: episode.proof };
  }
  if (primary.kind !== 'move_verdict' || roles.includes('best')) return;
  if (
    episodes.length !== 2 ||
    episodes[0]?.role !== 'reviewed' ||
    episodes[1]?.role !== 'reference'
  )
    return;
  return projectMoveVerdict(primary, episodes, move);
}

function projectMoveVerdict(
  primary: Record<string, unknown>,
  episodes: ProjectedEpisode[],
  reviewedUci: Uci,
): Extract<MoveReviewCandidateReview, { kind: 'move-verdict' }> | undefined {
  if (
    !hasOnlyKeys(
      primary,
      [
        'kind',
        'comparison_evidence_id',
        'verdict_code',
        'verdict_symbol',
        'verdict_confidence',
        'mover',
        'delta',
        'reference_endpoint',
        'played_endpoint',
        'primary_reason_evidence_id',
        'supporting_reason_evidence_ids',
        'semantic_episode_ids',
      ],
      [
        'kind',
        'comparison_evidence_id',
        'verdict_code',
        'verdict_symbol',
        'verdict_confidence',
        'mover',
        'delta',
        'reference_endpoint',
        'played_endpoint',
        'supporting_reason_evidence_ids',
        'semantic_episode_ids',
      ],
    ) ||
    typeof primary.verdict_code !== 'string' ||
    !Object.prototype.hasOwnProperty.call(verdictSymbolByCode, primary.verdict_code) ||
    verdictSymbolByCode[primary.verdict_code as MoveReviewVerdictCode] !== primary.verdict_symbol ||
    !isMember(primary.verdict_confidence, verdictConfidences) ||
    (primary.mover !== 'white' && primary.mover !== 'black') ||
    !semanticId(primary.comparison_evidence_id) ||
    !Array.isArray(primary.semantic_episode_ids) ||
    primary.semantic_episode_ids.length !== 2 ||
    !arrayEquals(primary.semantic_episode_ids, episodes.map(episode => episode.id))
  )
    return;
  const reference = projectEndpoint(primary.reference_endpoint);
  const played = projectEndpoint(primary.played_endpoint);
  const reviewedEpisode = episodes[0]!;
  const referenceEpisode = episodes[1]!;
  if (
    !reference ||
    !played ||
    played.moves[0] !== reviewedUci ||
    reviewedEpisode.rootMove !== reviewedUci ||
    referenceEpisode.rootMove !== reference.moves[0] ||
    !arrayPrefix(reviewedEpisode.lineMoves, played.moves) ||
    !arrayPrefix(referenceEpisode.lineMoves, reference.moves)
  )
    return;
  if (!isObject(primary.delta) || !hasExactKeys(primary.delta, ['kind', 'candidate_win_percent_delta_for_mover']))
    return;
  const delta = finiteNumber(primary.delta.candidate_win_percent_delta_for_mover, -100, 100);
  if (delta === undefined) return;
  const bothEngine = reference.kind === 'engine_search' && played.kind === 'engine_search';
  const bothTerminal =
    reference.kind === 'exact_automatic_terminal' && played.kind === 'exact_automatic_terminal';
  if (
    (bothEngine &&
      (primary.verdict_confidence !== 'engine_backed' || primary.delta.kind !== 'engine_evaluation')) ||
    (bothTerminal &&
      (primary.verdict_confidence !== 'legal_replay_verified' || primary.delta.kind !== 'outcome_only')) ||
    (!bothEngine &&
      !bothTerminal &&
      (primary.verdict_confidence !== 'mixed_verified' || primary.delta.kind !== 'outcome_only'))
  )
    return;
  if (
    bothEngine &&
    Math.abs(played.winPercent! - reference.winPercent! - delta) > Number.EPSILON * 100
  )
    return;
  const primaryReason = Object.prototype.hasOwnProperty.call(primary, 'primary_reason_evidence_id')
    ? semanticId(primary.primary_reason_evidence_id)
    : undefined;
  const support = projectSemanticIdArray(primary.supporting_reason_evidence_ids, 2);
  if (!support || (!primaryReason && support.length > 0)) return;
  const reasonIds = [...(primaryReason ? [primaryReason] : []), ...support];
  if (!unique(reasonIds)) return;
  const selectedCarriers = [...reviewedEpisode.selectedCarriers].sort((left, right) => left.order - right.order);
  if (
    referenceEpisode.selectedCarriers.length !== 0 ||
    selectedCarriers.some((carrier, order) => carrier.order !== order) ||
    !arrayEquals(
      selectedCarriers.map(carrier => carrier.id),
      reasonIds,
    )
  )
    return;
  const reasons = selectedCarriers.map(carrier => projectEpisodeReason(carrier.id, reviewedEpisode));
  let winChance: MoveReviewWinChance | undefined;
  if (reference.winPercent !== undefined && played.winPercent !== undefined) {
    winChance = {
      referencePercent: reference.winPercent,
      playedPercent: played.winPercent,
      changePercentagePoints: delta,
    };
  }
  return {
    kind: 'move-verdict',
    core: {
      verdictRef: primary.comparison_evidence_id as string,
      verdictCode: primary.verdict_code as MoveReviewVerdictCode,
      verdictSymbol: primary.verdict_symbol as MoveReviewVerdictSymbol,
      playedUci: reviewedUci,
      bestUci: reference.moves[0]!,
      ...(winChance ? { winChance } : {}),
      ...(reference.terminal ? { referenceTerminal: reference.terminal } : {}),
      ...(played.terminal ? { reviewedTerminal: played.terminal } : {}),
      reasonRefs: { ...(primaryReason ? { primary: primaryReason } : {}), support },
    },
    reasons,
  };
}

function projectEndpoint(value: unknown): ProjectedEndpoint | undefined {
  if (!isObject(value) || !validUciMoves(value.moves, 1, 80)) return;
  if (value.kind === 'engine_search') {
    if (
      !hasOnlyKeys(
        value,
        ['kind', 'moves', 'win_percent_for_mover', 'depth', 'mate_forecast_white_pov'],
        ['kind', 'moves', 'win_percent_for_mover', 'depth'],
      )
    )
      return;
    const winPercent = finiteNumber(value.win_percent_for_mover, 0, 100);
    const depth = integerInRange(value.depth, 0, 100);
    const hasMate = Object.prototype.hasOwnProperty.call(value, 'mate_forecast_white_pov');
    const mate = hasMate
      ? integerInRange(value.mate_forecast_white_pov, -1_000, 1_000)
      : undefined;
    if (
      winPercent === undefined ||
      depth === undefined ||
      (hasMate && (!mate || mate === 0)) ||
      (!hasMate && depth < 8)
    )
      return;
    return { kind: 'engine_search', moves: [...value.moves], winPercent };
  }
  if (value.kind === 'exact_automatic_terminal') {
    if (!hasExactKeys(value, ['kind', 'moves', 'terminal'])) return;
    const terminal = projectAutomaticTerminal(value.terminal);
    return terminal ? { kind: 'exact_automatic_terminal', moves: [...value.moves], terminal } : undefined;
  }
  return;
}

function projectAutomaticTerminal(value: unknown): MoveReviewAutomaticTerminal | undefined {
  if (!isObject(value)) return;
  if (value.kind === 'checkmate')
    return hasExactKeys(value, ['kind', 'winner']) && (value.winner === 'white' || value.winner === 'black')
      ? { kind: 'checkmate', winner: value.winner }
      : undefined;
  return hasExactKeys(value, ['kind']) &&
    (value.kind === 'stalemate' ||
      value.kind === 'insufficient_material' ||
      value.kind === 'fivefold_repetition' ||
      value.kind === 'seventy_five_move_rule')
    ? { kind: value.kind }
    : undefined;
}

function projectDrawClaim(value: unknown): MoveReviewDrawClaim | undefined {
  if (!isObject(value)) return;
  if (
    value.availability === 'available_now' &&
    hasExactKeys(value, ['rule', 'availability']) &&
    (value.rule === 'threefold_repetition' || value.rule === 'fifty_move_rule')
  )
    return { rule: value.rule, availability: value.availability };
  if (
    value.availability === 'available_by_declared_move' &&
    hasExactKeys(value, ['rule', 'availability', 'move_ucis']) &&
    (value.rule === 'threefold_repetition' || value.rule === 'fifty_move_rule') &&
    validUciMoves(value.move_ucis, 1, 256) &&
    unique(value.move_ucis)
  )
    return { rule: value.rule, availability: value.availability, moveUcis: [...value.move_ucis] };
  return;
}

function projectDrawClaims(value: unknown): MoveReviewDrawClaim[] | undefined {
  if (!Array.isArray(value) || value.length < 1 || value.length > 4) return;
  const claims = value.map(projectDrawClaim);
  return claims.every((claim): claim is MoveReviewDrawClaim => !!claim) ? claims : undefined;
}

function projectPositionContext(value: unknown, subject: MoveReviewSubject): string | undefined {
  if (
    !isObject(value) ||
    value.authority !== 'board_derived' ||
    value.fen !== subject.before.fen
  )
    return;
  return semanticId(value.board_evidence_id);
}

function projectSemanticEpisode(
  value: unknown,
  subject: MoveReviewSubject,
  boardEvidenceId: string,
): ProjectedEpisode | undefined {
  if (
    !isObject(value) ||
    !hasOnlyKeys(
      value,
      [
        'episode_id',
        'line_evidence_id',
        'role',
        'line_authority',
        'root_move_uci',
        'line_moves',
        'replay_steps',
        'events',
        'material_captures',
        'relation_facts',
        'direct_cause_channels',
        'material_summary',
        'selected_reason_order',
        'consequences',
        'causal_episodes',
        'structural_transitions',
      ],
      [
        'episode_id',
        'line_evidence_id',
        'role',
        'line_authority',
        'root_move_uci',
        'line_moves',
        'replay_steps',
        'events',
        'material_captures',
        'relation_facts',
        'direct_cause_channels',
        'consequences',
        'causal_episodes',
        'structural_transitions',
      ],
    ) ||
    value.line_authority !== 'legal_replay_verified' ||
    (value.role !== 'reviewed' && value.role !== 'reference') ||
    !semanticId(value.episode_id) ||
    value.line_evidence_id !== value.episode_id ||
    !validUciMoves(value.line_moves, 1, 6) ||
    !Array.isArray(value.replay_steps) ||
    value.replay_steps.length !== value.line_moves.length ||
    !Array.isArray(value.material_captures) ||
    value.material_captures.length > 6 ||
    !value.material_captures.every((capture, order) =>
      validMaterialCapture(capture, order, value.line_moves as Uci[]),
    ) ||
    !Array.isArray(value.relation_facts) ||
    !Array.isArray(value.direct_cause_channels)
  )
    return;
  const replayMoves: MoveReviewProofMove[] = [];
  let expectedBefore: FEN = subject.before.fen;
  for (let index = 0; index < value.replay_steps.length; index++) {
    const step = value.replay_steps[index];
    if (
      !isObject(step) ||
      !hasOnlyKeys(
        step,
        ['order', 'ply', 'move_uci', 'from', 'to', 'fen_before', 'fen_after', 'related_objects'],
        ['order', 'ply', 'move_uci', 'from', 'to', 'fen_before', 'fen_after'],
      ) ||
      step.order !== index ||
      step.move_uci !== value.line_moves[index] ||
      step.fen_before !== expectedBefore ||
      !fenText(step.fen_after)
    )
      return;
    replayMoves.push({ uci: step.move_uci as Uci, label: step.move_uci as string, fenAfter: step.fen_after as FEN });
    expectedBefore = step.fen_after as FEN;
  }
  const rootMove = uci(value.root_move_uci);
  if (!rootMove || rootMove !== replayMoves[0]?.uci) return;
  const id = value.episode_id as string;
  const relationIds = value.relation_facts.map(relation =>
    isObject(relation) ? semanticId(relation.relation_evidence_id) : undefined,
  );
  const causeIds = value.direct_cause_channels.map(cause =>
    isObject(cause) ? semanticId(cause.cause_evidence_id) : undefined,
  );
  if (
    !relationIds.every((relationId): relationId is string => !!relationId) ||
    !causeIds.every((causeId): causeId is string => !!causeId) ||
    !unique([id, ...relationIds, ...causeIds])
  )
    return;
  const selectedCarriers = projectSelectedReasonCarriers(
    value,
    id,
    value.line_moves as Uci[],
    boardEvidenceId,
  );
  if (!selectedCarriers) return;
  return {
    id,
    role: value.role as 'reviewed' | 'reference',
    rootMove,
    lineMoves: [...(value.line_moves as Uci[])],
    proof: { id, startFen: subject.before.fen, moves: replayMoves, annotations: [] },
    selectedCarriers,
  };
}

function projectEpisodeReason(
  reasonId: string,
  owner: ProjectedEpisode,
): MoveReviewReason {
  return {
    id: reasonId,
    messageKey: 'move_review.reason.for_candidate',
    messageSlots: { candidateUci: owner.rootMove },
    proof: { ...owner.proof, id: reasonId },
  };
}

function projectSelectedReasonCarriers(
  episode: Record<string, unknown>,
  lineEvidenceId: string,
  lineMoves: Uci[],
  boardEvidenceId: string,
): Array<{ id: string; order: number }> | undefined {
  const carriers: Array<{ id: string; order: number }> = [];
  if (Object.prototype.hasOwnProperty.call(episode, 'selected_reason_order')) {
    const order = integerInRange(episode.selected_reason_order, 0, 2);
    if (order === undefined) return;
    carriers.push({ id: lineEvidenceId, order });
  }
  const relations = episode.relation_facts as unknown[];
  for (const [relationOrder, value] of relations.entries()) {
    if (!isObject(value) || !Object.prototype.hasOwnProperty.call(value, 'selected_reason_order')) continue;
    const id = semanticId(value.relation_evidence_id);
    const order = integerInRange(value.selected_reason_order, 0, 2);
    if (
      !id ||
      order === undefined ||
      value.order !== relationOrder ||
      !validRelationCarrier(value, lineEvidenceId, lineMoves, true)
    )
      return;
    carriers.push({ id, order });
  }
  for (const [causeOrder, value] of (episode.direct_cause_channels as unknown[]).entries()) {
    if (!isObject(value) || !Object.prototype.hasOwnProperty.call(value, 'selected_reason_order')) continue;
    const id = semanticId(value.cause_evidence_id);
    const staticRelationId = semanticId(value.static_relation_evidence_id);
    const order = integerInRange(value.selected_reason_order, 0, 2);
    const staticRelation = relations.find(
      relation => isObject(relation) && relation.relation_evidence_id === staticRelationId,
    );
    if (
      !id ||
      !staticRelationId ||
      order === undefined ||
      !hasOnlyKeys(
        value,
        [
          'order',
          'cause_evidence_id',
          'authority',
          'source_line_evidence_id',
          'static_relation_evidence_id',
          'proof',
          'parent_evidence_ids',
          'selected_reason_order',
        ],
      ) ||
      value.order !== causeOrder ||
      value.authority !== 'mixed' ||
      value.source_line_evidence_id !== lineEvidenceId ||
      !isObject(staticRelation) ||
      staticRelation.order !== relations.indexOf(staticRelation) ||
      !validRelationCarrier(staticRelation, lineEvidenceId, lineMoves, false) ||
      staticRelation.kind !== 'pin' ||
      staticRelation.detail_kind !== 'pin' ||
      staticRelation.scope !== 'before_position' ||
      (staticRelation.parent_evidence_ids as unknown[])[0] !== boardEvidenceId ||
      !validDirectCauseProof(
        value.proof,
        staticRelation,
        lineMoves[0],
        episode.material_captures as unknown[],
      ) ||
      !Array.isArray(value.parent_evidence_ids) ||
      value.parent_evidence_ids.length !== 2 ||
      !unique(value.parent_evidence_ids) ||
      !value.parent_evidence_ids.includes(lineEvidenceId) ||
      !value.parent_evidence_ids.includes(staticRelationId)
    )
      return;
    carriers.push({ id, order });
  }
  return unique(carriers.map(carrier => carrier.id)) && unique(carriers.map(carrier => carrier.order))
    ? carriers
    : undefined;
}

function validRelationCarrier(
  value: Record<string, unknown>,
  lineEvidenceId: string,
  episodeLineMoves: Uci[],
  selected: boolean,
): boolean {
  if (
    !hasOnlyKeys(
      value,
      [
        'order',
        'relation_evidence_id',
        'kind',
        'detail_kind',
        'authority',
        'scope',
        'line_moves',
        'participants',
        'proof_atoms',
        'focus_squares',
        'target_square',
        'selected_reason_order',
        'parent_evidence_ids',
      ],
      [
        'order',
        'relation_evidence_id',
        'kind',
        'detail_kind',
        'authority',
        'scope',
        'line_moves',
        'participants',
        'proof_atoms',
        'focus_squares',
        'parent_evidence_ids',
      ],
    ) ||
    integerInRange(value.order, 0, 255) === undefined ||
    !semanticId(value.relation_evidence_id) ||
    !isMember(value.kind, relationFactKinds) ||
    !semanticId(value.detail_kind) ||
    (value.authority !== 'board_derived' && value.authority !== 'mixed') ||
    (value.scope !== 'before_position' && value.scope !== 'played_line' && value.scope !== 'best_line') ||
    !Array.isArray(value.participants) ||
    value.participants.length < 1 ||
    value.participants.length > 64 ||
    !value.participants.every(validRelationParticipant) ||
    !Array.isArray(value.proof_atoms) ||
    value.proof_atoms.length < 1 ||
    value.proof_atoms.length > 128 ||
    !value.proof_atoms.every(atom => validRelationProofAtom(atom, value.line_moves as Uci[])) ||
    !Array.isArray(value.focus_squares) ||
    value.focus_squares.length > 64 ||
    !value.focus_squares.every(square => typeof square === 'string' && squarePattern.test(square)) ||
    !Array.isArray(value.parent_evidence_ids) ||
    value.parent_evidence_ids.length !== 1 ||
    !semanticId(value.parent_evidence_ids[0]) ||
    (Object.prototype.hasOwnProperty.call(value, 'target_square') &&
      (typeof value.target_square !== 'string' || !squarePattern.test(value.target_square)))
  )
    return false;
  if (value.scope === 'before_position') {
    if (value.authority !== 'board_derived' || !validUciMoves(value.line_moves, 0, 0)) return false;
  } else if (
    !validUciMoves(value.line_moves, 1, 3) ||
    !arrayPrefix(value.line_moves, episodeLineMoves) ||
    value.parent_evidence_ids[0] !== lineEvidenceId
  )
    return false;
  return (
    !selected ||
    (value.authority === 'board_derived' &&
      value.scope === 'played_line' &&
      integerInRange(value.selected_reason_order, 0, 2) !== undefined)
  );
}

function validDirectCauseProof(
  value: unknown,
  staticRelation: Record<string, unknown>,
  rootMove: Uci,
  materialCaptures: unknown[],
): boolean {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['kind', 'response_from', 'response_to']) ||
    value.kind !== 'legal_response_excluded_by_existing_absolute_pin' ||
    typeof value.response_from !== 'string' ||
    !squarePattern.test(value.response_from) ||
    typeof value.response_to !== 'string' ||
    !squarePattern.test(value.response_to) ||
    value.response_from === value.response_to ||
    staticRelation.target_square !== value.response_from ||
    value.response_to !== rootMove.slice(2, 4)
  )
    return false;
  const participants = staticRelation.participants as Record<string, unknown>[];
  const attacker = participants.find(
    participant =>
      participant.participant_role === 'attacker' &&
      (participant.piece_role === 'bishop' ||
        participant.piece_role === 'rook' ||
        participant.piece_role === 'queen') &&
      (participant.color === 'white' || participant.color === 'black'),
  );
  const blocker = participants.find(
    participant =>
      participant.participant_role === 'blocker' &&
      participant.square === value.response_from &&
      Object.prototype.hasOwnProperty.call(participant, 'piece_role') &&
      participant.piece_role !== 'king' &&
      (participant.color === 'white' || participant.color === 'black'),
  );
  const king = participants.find(
    participant =>
      participant.participant_role === 'target' &&
      participant.square !== value.response_from &&
      participant.piece_role === 'king' &&
      (participant.color === 'white' || participant.color === 'black'),
  );
  const rootCapture = materialCaptures.find(
    (capture): capture is Record<string, unknown> =>
      isObject(capture) &&
      capture.ply_offset === 0 &&
      capture.move_uci === rootMove &&
      capture.to === value.response_to &&
      capture.target_square === value.response_to,
  );
  return (
    !!attacker &&
    !!blocker &&
    !!king &&
    !!rootCapture &&
    attacker.color !== blocker.color &&
    blocker.color === king.color &&
    rootCapture.side === attacker.color
  );
}

function validRelationParticipant(value: unknown): boolean {
  return (
    isObject(value) &&
    hasOnlyKeys(
      value,
      ['square', 'participant_role', 'piece_role', 'color'],
      ['square', 'participant_role'],
    ) &&
    typeof value.square === 'string' &&
    squarePattern.test(value.square) &&
    isMember(value.participant_role, relationParticipantRoles) &&
    (!Object.prototype.hasOwnProperty.call(value, 'piece_role') || isMember(value.piece_role, pieceRoles)) &&
    (!Object.prototype.hasOwnProperty.call(value, 'color') ||
      value.color === 'white' ||
      value.color === 'black')
  );
}

function validRelationProofAtom(value: unknown, relationMoves: Uci[]): boolean {
  if (
    !isObject(value) ||
    !hasOnlyKeys(
      value,
      ['role', 'square', 'move_uci', 'participant_role', 'piece_role', 'label', 'color'],
      ['role'],
    ) ||
    !isMember(value.role, relationProofRoles) ||
    (Object.prototype.hasOwnProperty.call(value, 'square') &&
      (typeof value.square !== 'string' || !squarePattern.test(value.square))) ||
    (Object.prototype.hasOwnProperty.call(value, 'move_uci') &&
      (!uci(value.move_uci) || !relationMoves.includes(value.move_uci as Uci))) ||
    (Object.prototype.hasOwnProperty.call(value, 'participant_role') &&
      !isMember(value.participant_role, relationParticipantRoles)) ||
    (Object.prototype.hasOwnProperty.call(value, 'piece_role') && !isMember(value.piece_role, pieceRoles)) ||
    (Object.prototype.hasOwnProperty.call(value, 'label') && !semanticId(value.label)) ||
    (Object.prototype.hasOwnProperty.call(value, 'color') &&
      value.color !== 'white' &&
      value.color !== 'black')
  )
    return false;
  if (value.role === 'line_move') return Object.prototype.hasOwnProperty.call(value, 'move_uci');
  if (value.role === 'participant')
    return (
      Object.prototype.hasOwnProperty.call(value, 'square') &&
      Object.prototype.hasOwnProperty.call(value, 'participant_role')
    );
  return Object.prototype.hasOwnProperty.call(value, 'square');
}

function validMaterialCapture(
  value: unknown,
  order: number,
  lineMoves: Uci[],
): value is Record<string, unknown> {
  return (
    isObject(value) &&
    hasExactKeys(value, [
      'order',
      'move_uci',
      'ply_offset',
      'side',
      'attacker_role',
      'captured_role',
      'from',
      'to',
      'target_square',
      'value_cp',
      'recapture',
    ]) &&
    value.order === order &&
    integerInRange(value.ply_offset, 0, lineMoves.length - 1) !== undefined &&
    value.move_uci === lineMoves[value.ply_offset as number] &&
    (value.side === 'white' || value.side === 'black') &&
    isMember(value.attacker_role, pieceRoles) &&
    isMember(value.captured_role, pieceRoles) &&
    value.from === (value.move_uci as string).slice(0, 2) &&
    value.to === (value.move_uci as string).slice(2, 4) &&
    typeof value.target_square === 'string' &&
    squarePattern.test(value.target_square) &&
    integerInRange(value.value_cp, 0, 20_000) !== undefined &&
    typeof value.recapture === 'boolean'
  );
}

function projectSemanticIdArray(value: unknown, maximumLength: number): string[] | undefined {
  if (!Array.isArray(value) || value.length > maximumLength) return;
  const ids = value.map(semanticId);
  return ids.every((id): id is string => !!id) && unique(ids) ? ids : undefined;
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
  return keys.every(keyName => allowed.includes(keyName)) && required.every(keyName => keys.includes(keyName));
}

function isMember<T extends string>(value: unknown, values: ReadonlySet<T>): value is T {
  return typeof value === 'string' && values.has(value as T);
}

function semanticId(value: unknown): string | undefined {
  return typeof value === 'string' && value.length >= 1 && value.length <= 256 && utf8Bytes(value) <= 256
    ? value
    : undefined;
}

function requestId(value: unknown): string | undefined {
  return typeof value === 'string' && requestIdPattern.test(value) ? value : undefined;
}

function projectJobId(value: unknown): string | undefined {
  return typeof value === 'string' && jobIdPattern.test(value) ? value : undefined;
}

function utf8Bytes(value: string): number {
  return new TextEncoder().encode(value).length;
}

function fenText(value: unknown): FEN | undefined {
  return typeof value === 'string' && value.trim().length > 0 && value.length <= 128 && utf8Bytes(value) <= 128
    ? (value as FEN)
    : undefined;
}

function san(value: unknown): San | undefined {
  return typeof value === 'string' && value.trim().length > 0 && value.length <= 64
    ? (value as San)
    : undefined;
}

function uci(value: unknown): Uci | undefined {
  return typeof value === 'string' && uciPattern.test(value) ? (value as Uci) : undefined;
}

function validUciMoves(
  value: unknown,
  minimumLength: number,
  maximumLength = Number.MAX_SAFE_INTEGER,
): value is Uci[] {
  return (
    Array.isArray(value) &&
    value.length >= minimumLength &&
    value.length <= maximumLength &&
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

function projectLocalEvalScore(value: EvalScore): MoveReviewWhiteScore | undefined {
  if (value.cp !== undefined && value.mate === undefined) {
    return Number.isSafeInteger(value.cp) ? { kind: 'cp', value: value.cp } : undefined;
  }
  if (value.mate !== undefined && value.cp === undefined) {
    return Number.isSafeInteger(value.mate) && value.mate !== 0
      ? { kind: 'mate', value: value.mate }
      : undefined;
  }
  return;
}
