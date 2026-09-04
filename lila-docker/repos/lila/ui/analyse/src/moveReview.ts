import { isMoveReviewEngineProfile, type MoveReviewEngineProfile } from 'lib/ceval/types';
import { makeFen, parseFen } from 'chessops/fen';
import { parseUci } from 'chessops/util';
import { setupPosition } from 'chessops/variant';

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
}

interface MoveReviewWinChance {
  referencePercent: number;
  playedPercent: number;
  changePercentagePoints: number;
}

interface MoveReviewCoreFields {
  verdictRef: string;
  verdictCode: MoveReviewVerdictCode;
  verdictSymbol: MoveReviewVerdictSymbol;
  playedUci: Uci;
  bestUci: Uci;
  winChance?: MoveReviewWinChance;
  referenceTerminal?: MoveReviewAutomaticTerminal;
  reviewedTerminal?: MoveReviewAutomaticTerminal;
}

export type MoveReviewCore = MoveReviewCoreFields &
  (
    | { kind: 'move-verdict' }
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
      comparisonProof?: MoveReviewProof;
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
  purpose: 'root_search' | 'focus_comparison';
  executionKeySha256: string;
  variant: 'standard';
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
  maxSearchElapsedMs: number;
}

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
  | 'engine_failure';

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

interface MoveReviewSnapshotCommon {
  requestId: string;
  jobId: string;
  engineProfile: MoveReviewEngineProfile;
  judgmentRevision: string;
  annotationPolicyRevision: string;
  subject: MoveReviewSubject;
}

export type MoveReviewSnapshot =
  | (MoveReviewSnapshotCommon & { kind: 'awaiting-core'; issuedEngineWork: IssuedMoveReviewEngineWork })
  | (MoveReviewSnapshotCommon & { kind: 'awaiting-evidence'; issuedEngineWork: IssuedMoveReviewEngineWork })
  | (MoveReviewSnapshotCommon & { kind: 'completed'; evidence: MoveReviewEvidence })
  | (MoveReviewSnapshotCommon & { kind: 'position-action'; action: MoveReviewPositionAction })
  | (MoveReviewSnapshotCommon & { kind: 'abstained' });

export type MoveReviewJobState =
  | { kind: 'idle'; reason: 'root' | 'disabled' }
  | { kind: 'loading'; subject: MoveReviewSubject }
  | { kind: 'completed'; snapshot: Extract<MoveReviewSnapshot, { kind: 'completed' }> }
  | { kind: 'position-action'; snapshot: Extract<MoveReviewSnapshot, { kind: 'position-action' }> }
  | { kind: 'abstained'; subject: MoveReviewSubject }
  | { kind: 'fault'; subject: MoveReviewSubject; message: string; retryable: boolean }
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
  expandedProofId?: string;
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
    unsupported: '직전 수 리뷰를 실행할 수 없습니다.',
    engineUnavailable: '엔진 파일을 불러오거나 시작하지 못했습니다.',
    browserUnsupported: '이 브라우저에는 필요한 WebAssembly 기능이 없습니다.',
    retry: '다시 시도',
    best: '최선 수',
    played: '둔 수',
    alternative: '대안',
    candidateMoves: '후보 수',
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
    runnerUp: '차선 수',
    proofStep: '단계',
    addToStudy: 'Study에 수순 추가',
    addedToStudy: 'Study에 수순을 추가했습니다.',
    viewAddedLine: '추가한 수순 보기',
    showEvidence: '근거 펼치기',
    hideEvidence: '근거 접기',
    proofBoard: '근거 수순 미니보드',
    winChance: '기준 수 → 검토 수 승리 가능성',
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
    unsupported: 'Last move review cannot run.',
    engineUnavailable: 'The review engine could not be loaded or started.',
    browserUnsupported: 'This browser is missing the required WebAssembly features.',
    retry: 'Retry',
    best: 'Best',
    played: 'Played',
    alternative: 'Alternative',
    candidateMoves: 'Candidate moves',
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
    runnerUp: 'Runner-up',
    proofStep: 'Step',
    addToStudy: 'Add line to Study',
    addedToStudy: 'Line added to Study.',
    viewAddedLine: 'View added line',
    showEvidence: 'Show evidence',
    hideEvidence: 'Hide evidence',
    proofBoard: 'Proof line mini-board',
    winChance: 'Reference → reviewed win chance',
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
): MoveReviewSubject | undefined {
  if (variant !== 'standard' || nodeList.length < 2) return;
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
  const prefix = nodeList.slice(1, -1).map(node => uci(node.uci));
  if (!prefix.every((move): move is Uci => !!move)) return;
  return {
    variant: 'standard',
    initialFen: initial.fen,
    movePrefixUci: prefix,
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
  work: IssuedMoveReviewEngineWork,
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
  const projected = evaluation.pvs.slice(0, work.searchLimits.multiPv).map(pv => {
    const depth = pv.depth ?? evaluation.depth;
    if (
      pv.bound !== undefined ||
      depth !== requiredDepth ||
      !validUciMoves(pv.moves, 1, 80) ||
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

function lineLegallyReplays(work: IssuedMoveReviewEngineWork, lineMoves: readonly Uci[]): boolean {
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
  ]);
}

export function decodeMoveReviewSnapshot(
  raw: unknown,
  context: MoveReviewDecodeContext,
): MoveReviewSnapshot | undefined {
  if (!isObject(raw)) return;
  if (raw.schema_version === moveReviewJobStatusSchema) return projectJobStatus(raw, context);
  if (raw.schema_version === moveReviewResponseSchema) return projectCompleted(raw, context);
  return;
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
      ]) ||
      !isMoveReviewStopCondition(raw.stop_condition) ||
      !validProgress(raw.progress, 'stopped')
    )
      return;
    return { ...common, kind: 'abstained' };
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
    ])
  )
    return;
  const work = projectIssuedEngineWork(raw.issued_engine_work, common.engineProfile, context.subject);
  if (!work || !validProgress(raw.progress, work.purpose)) return;
  return {
    ...common,
    kind: work.purpose === 'root_search' ? 'awaiting-core' : 'awaiting-evidence',
    issuedEngineWork: work,
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
    (context.engineProfile !== undefined && context.engineProfile !== profile)
  )
    return;
  return {
    requestId: context.requestId,
    jobId,
    engineProfile: profile,
    judgmentRevision: moveReviewResponseSchema,
    annotationPolicyRevision: moveReviewAnnotationPolicyRevision,
    subject: context.subject,
  };
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

function isMoveReviewStopCondition(value: unknown): boolean {
  return (
    value === 'deadline_exceeded' ||
    value === 'cancelled' ||
    value === 'search_budget_exhausted' ||
    value === 'engine_execution_failed' ||
    value === 'invalid_engine_work_report' ||
    value === 'review_construction_failed' ||
    value === 'move_review_preparation_failed' ||
    value === 'repetition_history_unavailable'
  );
}

function projectIssuedEngineWork(
  value: unknown,
  profile: MoveReviewEngineProfile,
  subject: MoveReviewSubject,
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
    ]) ||
    typeof value.work_id !== 'string' ||
    !workIdPattern.test(value.work_id) ||
    (value.purpose !== 'root_search' && value.purpose !== 'focus_comparison') ||
    value.engine_profile !== profile ||
    typeof value.execution_key_sha256 !== 'string' ||
    !sha256Pattern.test(value.execution_key_sha256) ||
    value.variant !== 'standard' ||
    value.engine_position_initial_fen !== subject.initialFen ||
    !validUciMoves(value.engine_position_moves_uci, 0, 600) ||
    !arrayPrefix(subject.movePrefixUci, value.engine_position_moves_uci) ||
    !fenText(value.search_fen) ||
    replayFen(subject.initialFen, value.engine_position_moves_uci) !== value.search_fen ||
    !isObject(value.root_restriction) ||
    !isObject(value.search_limits) ||
    !hasExactKeys(value.search_limits, ['depth', 'nodes', 'movetime_ms', 'multi_pv'])
  )
    return;
  const limits = {
    depth: integerInRange(value.search_limits.depth, 1, 100),
    nodes: integerInRange(value.search_limits.nodes, 1, 5_000_000),
    movetimeMs: integerInRange(value.search_limits.movetime_ms, 1, 5_000),
    multiPv: integerInRange(value.search_limits.multi_pv, 1, 3),
  };
  if (Object.values(limits).some(limit => limit === undefined)) return;
  const unrestricted =
    hasExactKeys(value.root_restriction, ['kind']) && value.root_restriction.kind === 'unrestricted';
  const restrictedMoves =
    hasExactKeys(value.root_restriction, ['kind', 'moves_uci']) &&
    value.root_restriction.kind === 'restricted' &&
    validUciMoves(value.root_restriction.moves_uci, 2, 2) &&
    unique(value.root_restriction.moves_uci)
      ? value.root_restriction.moves_uci
      : undefined;
  const rootOrFocusPosition =
    arrayEquals(value.engine_position_moves_uci, subject.movePrefixUci) &&
    value.search_fen === subject.before.fen;
  const shapeValid =
    (value.purpose === 'root_search' &&
      rootOrFocusPosition &&
      unrestricted &&
      limits.depth === 16 &&
      limits.nodes === 5_000_000 &&
      limits.movetimeMs === 5_000 &&
      value.max_search_elapsed_ms === 6_000) ||
    (value.purpose === 'focus_comparison' &&
      rootOrFocusPosition &&
      restrictedMoves?.includes(subject.played.uci) &&
      limits.depth === 16 &&
      limits.nodes === 2_000_000 &&
      limits.movetimeMs === 2_500 &&
      limits.multiPv === 2 &&
      value.max_search_elapsed_ms === 3_500);
  if (!shapeValid) return;
  return {
    engineProfile: profile,
    workId: value.work_id,
    purpose: value.purpose,
    executionKeySha256: value.execution_key_sha256,
    variant: 'standard',
    enginePositionInitialFen: subject.initialFen,
    enginePositionMovesUci: [...value.engine_position_moves_uci],
    searchFen: value.search_fen as FEN,
    rootRestriction: restrictedMoves
      ? { kind: 'restricted', movesUci: [...restrictedMoves] }
      : { kind: 'unrestricted' },
    searchLimits: {
      depth: limits.depth as 16,
      nodes: limits.nodes as 5_000_000 | 2_000_000,
      movetimeMs: limits.movetimeMs as 5_000 | 2_500,
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
    ]) ||
    raw.annotation_policy_revision !== moveReviewAnnotationPolicyRevision ||
    raw.current_fen !== context.subject.before.fen ||
    !wireFocusMatches(raw.focus, context.subject) ||
    !validProgress(raw.progress, 'completed')
  )
    return;
  const common = projectCommon(raw, context);
  if (!common || !isObject(raw.result)) return;
  const action = projectPositionAction(raw.result);
  if (action) return { ...common, kind: 'position-action', action };
  if (raw.result.kind === 'forced_single_move') return projectForcedSingleMove(raw.result, common);
  if (
    raw.result.kind !== 'selected_move_choices' ||
    !hasOnlyKeys(
      raw.result,
      ['kind', 'selected_move_reviews', 'draw_claims'],
      ['kind', 'selected_move_reviews'],
    ) ||
    !Array.isArray(raw.result.selected_move_reviews) ||
    raw.result.selected_move_reviews.length < 1 ||
    raw.result.selected_move_reviews.length > 2
  )
    return;
  const candidates = raw.result.selected_move_reviews.map(value =>
    projectSelectedReview(value, context.subject),
  );
  if (!candidates.every((candidate): candidate is MoveReviewCandidate => !!candidate)) return;
  const played = candidates.filter(candidate => candidate.roles.includes('played'));
  const best = candidates.filter(candidate => candidate.roles.includes('best'));
  if (
    played.length !== 1 ||
    best.length !== 1 ||
    played[0]?.uci !== context.subject.played.uci ||
    !unique(candidates.map(candidate => candidate.uci))
  )
    return;
  const drawClaims = Object.prototype.hasOwnProperty.call(raw.result, 'draw_claims')
    ? projectDrawClaims(raw.result.draw_claims)
    : undefined;
  if (Object.prototype.hasOwnProperty.call(raw.result, 'draw_claims') && !drawClaims) return;
  for (const candidate of candidates) {
    if (candidate.review.kind !== 'move-verdict') continue;
    if (candidate.review.core.bestUci !== best[0]!.uci) return;
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
  common: MoveReviewSnapshotCommon,
): MoveReviewSnapshot | undefined {
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
  if (!move || move !== common.subject.played.uci || !endpoint || endpoint.moves[0] !== move) return;
  return {
    ...common,
    kind: 'completed',
    evidence: {
      ...(claims ? { drawClaims: claims } : {}),
      candidates: [
        {
          uci: move,
          label: common.subject.played.san,
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
    label: value.move_uci === subject.played.uci ? subject.played.san : (value.move_uci as string),
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
    verdictCode = runnerUpVerdictCode;
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
        changePercentagePoints: delta,
      };
  } else return;
  const bestUci = primary.kind === 'best_choice' ? reviewedEndpoint?.moves[0] : referenceEndpoint?.moves[0];
  if (!bestUci || !verdictCode || !referenceEndpoint || !reviewedEndpoint) return;
  const comparisonProof = buildProof(`pv:${move}`, subject.before.fen, reviewedEndpoint.moves);
  const coreFields: MoveReviewCoreFields = {
    verdictRef,
    verdictCode,
    verdictSymbol: bestChoice ? 'none' : verdictSymbolByCode[verdictCode],
    playedUci: move,
    bestUci,
    ...(winChance ? { winChance } : {}),
    ...(referenceEndpoint.terminal ? { referenceTerminal: referenceEndpoint.terminal } : {}),
    ...(reviewedEndpoint.terminal ? { reviewedTerminal: reviewedEndpoint.terminal } : {}),
  };
  const core: MoveReviewCore = bestChoice
    ? { ...coreFields, kind: 'best-choice', bestChoice }
    : { ...coreFields, kind: 'move-verdict' };
  return {
    kind: 'move-verdict',
    core,
    ...(comparisonProof ? { comparisonProof } : {}),
  };
}

export function moveReviewProofById(
  review: MoveReviewCandidateReview,
  proofId: string,
): MoveReviewProof | undefined {
  if (review.kind === 'single-candidate-insight')
    return review.proof.id === proofId ? review.proof : undefined;
  if (review.kind !== 'move-verdict') return;
  if (review.comparisonProof?.id === proofId) return review.comparisonProof;
  return;
}
function buildProof(id: string, startFen: FEN, moves: readonly Uci[]): MoveReviewProof | undefined {
  if (!semanticId(id) || moves.length < 1 || moves.length > 80) return;
  try {
    const position = setupPosition('chess', parseFen(startFen).unwrap()).unwrap();
    const proofMoves: MoveReviewProofMove[] = [];
    for (const rawMove of moves) {
      const move = parseUci(rawMove);
      if (!move || !position.isLegal(move)) return;
      position.play(move);
      proofMoves.push({ uci: rawMove, label: rawMove, fenAfter: makeFen(position.toSetup()) });
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
