import { isMoveReviewEngineProfile, type MoveReviewEngineProfile } from 'lib/ceval/types';
import { makeFen, parseFen } from 'chessops/fen';
import { parseSquare, parseUci } from 'chessops/util';
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
export type MoveReviewReasonRole = 'primary' | 'support' | 'proof-route';
export type MoveReviewCandidateSetType = 'only_move' | 'narrow_choice' | 'style_choice';
type MoveReviewDirectCausalChange = 'occurred' | 'maintained' | 'lost';
type MoveReviewPlayerFacingCausalChange = MoveReviewDirectCausalChange | 'missed';
type MoveReviewBrush = 'green' | 'blue' | 'yellow' | 'red';

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

interface MoveReviewReasonRefs {
  primary?: string;
  support: string[];
  routes: string[];
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
  reasonRefs: MoveReviewReasonRefs;
}

export type MoveReviewCore = MoveReviewCoreFields &
  (
    | { kind: 'move-verdict' }
    | {
        kind: 'best-choice';
        bestChoice: {
          runnerUpVerdictCode: Exclude<MoveReviewVerdictCode, 'improves_on_reference'>;
          runnerUpUci: Uci;
          candidateSet: MoveReviewCandidateSetType;
        };
      }
  );

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
  messageSlots: { candidateUci: Uci };
  message: MoveReviewReasonMessage;
  proof: MoveReviewProof;
}

interface MoveReviewTypedBranch {
  id: string;
  role: 'counterfactual_reference' | 'observed_played_root' | 'expected_result_route' | 'legal_reply';
  provenance: 'counterfactual_analyzed_root' | 'observed_game_root';
  lineId: string;
  lineRole: 'played' | 'best_reference' | 'alternative' | 'branch_reply';
  lineRank: number;
  rootMove: Uci;
  sourceProbeId?: string;
  steps?: MoveReviewWireStep[];
}

type MoveReviewPieceRole = 'pawn' | 'knight' | 'bishop' | 'rook' | 'queen' | 'king';

interface MoveReviewTypedActor {
  side: 'white' | 'black';
  from: Key;
  to: Key;
  pieceBefore: MoveReviewPieceRole;
  pieceAfter: MoveReviewPieceRole;
  moveUci?: Uci;
  legalMoveRelation?: string;
}

type MoveReviewPassedPawnDependencyKind =
  | 'object_state_precondition'
  | 'line_access_precondition'
  | 'response_continuation_precondition';

type MoveReviewPassedPawnDependencyProofKind =
  | 'object_state'
  | 'line_access'
  | 'pawn_break_follow_up'
  | 'capture_follow_up';

type MoveReviewPassedPawnRelationKind =
  | 'slider_reach_delta'
  | 'pawn_topology_transition'
  | 'capture_recapture_inventory';

interface MoveReviewPassedPawnDependencyProof {
  dependencyKind: MoveReviewPassedPawnDependencyKind;
  proofKind: MoveReviewPassedPawnDependencyProofKind;
  squares: { role: string; square: Key }[];
  pieces: { role: string; side: 'white' | 'black'; piece: MoveReviewPieceRole }[];
  relationIssuers: {
    contract: MoveReviewPassedPawnRelationKind;
    relationKind: MoveReviewPassedPawnRelationKind;
    resultKey: string;
    occurrenceId: string;
    stepKey: string;
    sourcePremiseIds: string[];
  }[];
}

interface MoveReviewTypedPremise {
  role: string;
  contract: string;
  resultId: string;
  sourcePremiseIds: string[];
  branchId: string;
  branchRole: string;
  stepIndex?: number;
  relatedBranchIds?: string[];
  fromStepIndex?: number;
  toStepIndex?: number;
  dependencyProof?: MoveReviewPassedPawnDependencyProof;
}

interface MoveReviewTypedLine {
  id: string;
  role: 'played' | 'best_reference' | 'alternative' | 'branch_reply';
  rank: number;
  rootMove: Uci;
}

export type MoveReviewReasonMessage =
  | { kind: 'line'; moves: Uci[] }
  | {
      kind: 'causal';
      causeEvidenceId: string;
      causeKind: string;
      effectMode: 'played_liability' | 'alternative_resource' | 'played_value';
      directChange: MoveReviewDirectCausalChange;
      playedChange: MoveReviewPlayerFacingCausalChange;
      actor: { moveUci: Uci; side: 'white' | 'black'; piece: string; from: Key; to: Key };
      targets: string[];
      mechanisms: string[];
      consequences: string[];
      witnesses: string[];
      horizon?: string;
      proofSegment?: {
        terminalRelation: 'produces_line_consequence' | 'is_root_line_event' | 'instantiates_relation';
        steps: Array<{
          plyOffset: number;
          moveUci: Uci;
          role: 'root_action' | 'causal_link' | 'terminal_event';
        }>;
      };
    }
  | {
      kind: 'resource-differential';
      channelId: string;
      causalSignature: string;
      causeEvidenceId: string;
      causeKind: string;
      effectMode: 'played_liability' | 'alternative_resource' | 'played_value';
      directChange: 'occurred';
      playedChange: 'missed';
      family: 'immediate_forced_reply_resource_differential';
      sourceEvidenceId: string;
      semanticId: string;
      occurrenceId: string;
      dependencyFingerprint: string;
      pathOccurrenceId: string;
      branch: MoveReviewTypedBranch;
      counterpart: MoveReviewTypedBranch;
      trigger: MoveReviewTypedActor;
      forcedReply: MoveReviewTypedActor & { moveUci: Uci };
      realizer: MoveReviewTypedActor;
      realizingMove: Uci;
      capturedTarget: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      playedDefender: MoveReviewTypedActor & { moveUci: Uci };
      disabledDefender: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      premises: MoveReviewTypedPremise[];
      absence: {
        useId: string;
        semanticProofId: string;
        issuer: 'position_relation_extractor.closed_relation_inventory';
        issuerEvidenceId: string;
        issuerOccurrenceId: string;
        query: string;
        branchId: string;
        afterStepIndex: number;
        fen: FEN;
        ply: number;
        scope: 'best_line';
      };
      triggerMechanism: 'forced_displacement';
    }
  | {
      kind: 'defense-obligation-change';
      channelId: string;
      causalSignature: string;
      causeEvidenceId: string;
      causeKind: string;
      effectMode: 'played_liability' | 'alternative_resource' | 'played_value';
      directChange: 'occurred';
      playedChange: 'missed';
      contract: 'defense_obligation_change';
      mechanism: 'sole_recapturer_removal';
      sourceEvidenceId: string;
      semanticId: string;
      occurrenceId: string;
      dependencyFingerprint: string;
      pathOccurrenceId: string;
      branch: MoveReviewTypedBranch;
      counterpart: MoveReviewTypedBranch;
      remover: MoveReviewTypedActor;
      removedDefender: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      removalRecapture: MoveReviewTypedActor & { moveUci: Uci };
      laterExploit: MoveReviewTypedActor;
      capturedTarget: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      playedSoleRecapture: MoveReviewTypedActor & { moveUci: Uci };
      laterExploitMove: Uci;
      playedSoleRecaptureMove: Uci;
      premises: MoveReviewTypedPremise[];
      absence: {
        useId: string;
        role: 'reference_replacement_recapture_absent';
        semanticProofId: string;
        issuer: 'position_relation_extractor.closed_relation_inventory';
        issuerEvidenceId: string;
        issuerOccurrenceId: string;
        query: string;
        branchId: string;
        afterStepIndex: 2;
        fen: FEN;
        ply: number;
        scope: 'best_line';
      };
    }
  | {
      kind: 'passed-pawn-result';
      channelId: string;
      causalSignature: string;
      causeEvidenceId: string;
      causeKind: string;
      effectMode: 'played_liability' | 'alternative_resource' | 'played_value';
      directChange: 'occurred';
      contract: 'passed_pawn_result_under_closed_replies';
      sourceEvidenceId: string;
      eventEvidenceId: string;
      comparisonEvidenceId: string;
      semanticId: string;
      occurrenceId: string;
      dependencyFingerprint: string;
      pathOccurrenceId: string;
      consequenceKind: 'passed_pawn_progress';
      resultTargetSubjects: string[];
      rootActor: MoveReviewTypedActor;
      realizingActor: MoveReviewTypedActor;
      rootLine: MoveReviewTypedLine;
      rootMove: Uci;
      rootPly: number;
      replyMove: Uci;
      realizingMove: Uci;
      realizingPly: number;
      resultPlyOffset: number;
      pathRealizationActor: MoveReviewTypedActor;
      pathRealizationMove: Uci;
      pathRealizationPly: number;
      pathRealizationMatchKind: 'exact_move' | 'equivalent_function';
      replyBranch: MoveReviewTypedBranch;
      expectedBranches: MoveReviewTypedBranch[];
      replyOccurrenceSteps: MoveReviewPassedPawnResultWireStep[];
      expectedOccurrenceSteps: MoveReviewPassedPawnResultWireStep[];
      premises: MoveReviewTypedPremise[];
      closureUseIds: string[];
      lowerPremiseIds: string[];
      occurrenceLinkKeys: string[];
      replyClosure: {
        issuer: 'structural_delta.canonical_legal_reply_inventory';
        issuerEvidenceId: string;
        coverageIssuer: 'passed_pawn_result_event.branch_complete_reply_coverage';
        coverageEvidenceId: string;
        rootAfter: { fen: FEN; ply: number; scope: string };
        legalReplyMoves: Uci[];
        branchByReply: Array<{ move: Uci; branchId: string }>;
        certifiedHorizonPlyOffset: number;
      };
    };

export type MoveReviewCandidateReview =
  | {
      kind: 'move-verdict';
      core: MoveReviewCore;
      reasons: MoveReviewReason[];
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
  purpose: 'root_search' | 'focus_comparison' | 'causal_probe';
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
      failureCode: string;
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
  proofRouteReason: string;
  runnerUp: string;
  candidateSetLabels: Record<MoveReviewCandidateSetType, string>;
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
    noVerifiedReason: '판정은 가능하지만, 기준 수와의 차이를 설명할 원인은 검증되지 않았습니다.',
    noPrimaryReason: '핵심 근거는 검증되지 않았습니다.',
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
    primaryReason: '핵심 근거',
    supportingReason: '보조 근거',
    proofRouteReason: '검증 경로',
    runnerUp: '차선 수',
    candidateSetLabels: {
      only_move: '유일 수',
      narrow_choice: '좁은 선택지의 최선 수',
      style_choice: '스타일 선택의 선호 수',
    },
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
    noVerifiedReason:
      'The verdict is available, but no cause for the difference from the reference move was verified.',
    noPrimaryReason: 'No primary evidence was verified.',
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
    primaryReason: 'Primary evidence',
    supportingReason: 'Supporting evidence',
    proofRouteReason: 'Proof route',
    runnerUp: 'Runner-up',
    candidateSetLabels: {
      only_move: 'Only move',
      narrow_choice: 'Best of a narrow choice',
      style_choice: 'Preferred style choice',
    },
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

export function moveReviewReasonText(
  reason: MoveReviewReason,
  candidate: MoveReviewCandidate,
  locale: MoveReviewLocale,
): string {
  const message = reason.message;
  if (message.kind === 'line') {
    const moves = [candidate.label, ...message.moves.slice(1)].join(' ');
    return locale === 'ko-KR' ? `검증 수순: ${moves}.` : `Verified line: ${moves}.`;
  }
  if (message.kind === 'causal') {
    const targets = message.targets.join(', ');
    const mechanisms = message.mechanisms.join(', ');
    const consequences = message.consequences.join(', ');
    return locale === 'ko-KR'
      ? `[${message.causeKind}] ${message.actor.moveUci}: ${message.playedChange}; 대상 ${targets}; 기제 ${mechanisms}; 귀결 ${consequences}.`
      : `[${message.causeKind}] ${message.actor.moveUci}: ${message.playedChange}; targets ${targets}; mechanisms ${mechanisms}; consequences ${consequences}.`;
  }
  if (message.kind === 'resource-differential') {
    const premiseLabels: Record<MoveReviewTypedPremise['role'], [string, string]> = {
      created_check_response: ['강제 체크 응수', 'forced check reply'],
      reference_capture_recapture: ['기준 재포획', 'reference recapture'],
      played_capture_recapture: ['실전 재포획', 'played recapture'],
      expected_dependency: ['예상 의존', 'expected dependency'],
      expected_result: ['예상 결과', 'expected result'],
      observed_dependency: ['관측 의존', 'observed dependency'],
      observed_result: ['관측 결과', 'observed result'],
      functional_match: ['기능 일치', 'functional match'],
      comparison_demand: ['비교 요구', 'comparison demand'],
    };
    const premises = message.premises
      .map(premise => {
        const identity = premise.resultId.split(':').pop()?.slice(0, 8) || premise.resultId.slice(0, 8);
        const label = premiseLabels[premise.role][locale === 'ko-KR' ? 0 : 1];
        return `${label}:${identity}`;
      })
      .join(', ');
    const disabled =
      locale === 'ko-KR'
        ? `; 비활성화된 ${message.disabledDefender.piece} ${message.disabledDefender.square}`
        : `; disabled ${message.disabledDefender.piece} on ${message.disabledDefender.square}`;
    const mechanismLabel =
      locale === 'ko-KR' ? '강제 응수에 의한 수비자 이동' : 'forced defender displacement';
    const mechanism = locale === 'ko-KR' ? `; 기제 ${mechanismLabel}` : `; mechanism ${mechanismLabel}`;
    const absenceIssuer =
      locale === 'ko-KR'
        ? `폐쇄 관계 인벤토리 (${message.absence.issuerEvidenceId})`
        : `closed relation inventory (${message.absence.issuerEvidenceId})`;
    const referenceLead =
      locale === 'ko-KR'
        ? `${message.trigger.from}의 ${message.trigger.pieceBefore}가 ${message.trigger.to}로 이동해 ${message.forcedReply.moveUci} 응수를 강제하고`
        : `the ${message.trigger.pieceBefore} moves ${message.trigger.from}–${message.trigger.to}, forcing ${message.forcedReply.moveUci}, then`;
    const referenceSummary =
      locale === 'ko-KR'
        ? `${referenceLead} ${message.realizingMove}의 ${message.realizer.pieceBefore}가 ${message.capturedTarget.square}의 ${message.capturedTarget.piece}을 잡습니다`
        : `${referenceLead} ${message.realizingMove} by the ${message.realizer.pieceBefore} captures the ${message.capturedTarget.piece} on ${message.capturedTarget.square}`;
    const playedSummary =
      locale === 'ko-KR'
        ? `${message.realizingMove}의 ${message.realizer.pieceBefore}가 ${message.capturedTarget.square}의 ${message.capturedTarget.piece}을 잡지만 ${message.playedDefender.moveUci}의 ${message.playedDefender.pieceBefore}가 같은 칸에서 재포획합니다`
        : `${message.realizingMove} by the ${message.realizer.pieceBefore} captures the ${message.capturedTarget.piece} on ${message.capturedTarget.square}, but ${message.playedDefender.moveUci} by the ${message.playedDefender.pieceBefore} recaptures on that square`;
    const occurrenceComparison =
      message.branch.role === 'counterfactual_reference'
        ? locale === 'ko-KR'
          ? `반사실 기준 가지: ${referenceSummary}. 실제 둔 수 가지에서는 ${playedSummary}`
          : `counterfactual reference branch: ${referenceSummary}. In the observed played branch, ${playedSummary}`
        : locale === 'ko-KR'
          ? `실제 둔 수 가지: ${playedSummary}. 반사실 기준 가지에서는 ${referenceSummary}`
          : `observed played branch: ${playedSummary}. In the counterfactual reference branch, ${referenceSummary}`;
    return locale === 'ko-KR'
      ? `[${message.causeKind}] ${occurrenceComparison}. 경로 ${message.pathOccurrenceId.slice(0, 8)}는 ${absenceIssuer}가 ${message.absence.fen} (ply ${message.absence.ply})에서 인증한 부재 ${message.absence.query}와 하위 증거 ${premises}를 대조합니다${disabled}${mechanism}.`
      : `[${message.causeKind}] ${occurrenceComparison}. Path ${message.pathOccurrenceId.slice(0, 8)} contrasts absence ${message.absence.query}, certified by the ${absenceIssuer} at ${message.absence.fen} (ply ${message.absence.ply}), with lower proofs ${premises}${disabled}${mechanism}.`;
  }
  if (message.kind === 'defense-obligation-change') {
    const premiseLabels: Record<string, [string, string]> = {
      reference_defender_removal: ['기준 수비자 제거', 'reference defender removal'],
      reference_later_exploit_inventory: ['기준 후속 포획', 'reference later exploit'],
      played_immediate_exploit_inventory: ['실전 즉시 포획', 'played immediate exploit'],
    };
    const premises = message.premises
      .map(premise => {
        const identity = premise.resultId.split(':').pop()?.slice(0, 8) || premise.resultId.slice(0, 8);
        return `${premiseLabels[premise.role]![locale === 'ko-KR' ? 0 : 1]}:${identity}`;
      })
      .join(', ');
    const reference =
      locale === 'ko-KR'
        ? `반사실 기준 가지에서 ${message.remover.from}의 ${message.remover.pieceBefore}가 ${message.removedDefender.square}의 ${message.removedDefender.piece}을 잡고, ${message.removalRecapture.moveUci}로 재포획된 뒤 ${message.laterExploitMove}가 ${message.capturedTarget.square}의 ${message.capturedTarget.piece}을 잡습니다`
        : `in the counterfactual reference branch, the ${message.remover.pieceBefore} from ${message.remover.from} captures the ${message.removedDefender.piece} on ${message.removedDefender.square}, is recaptured by ${message.removalRecapture.moveUci}, and ${message.laterExploitMove} then captures the ${message.capturedTarget.piece} on ${message.capturedTarget.square}`;
    const played =
      locale === 'ko-KR'
        ? `실전 가지에서는 같은 ${message.laterExploitMove}에 ${message.removedDefender.square}의 ${message.removedDefender.piece}이 ${message.playedSoleRecaptureMove}로 재포획합니다`
        : `in the observed played branch, the same ${message.laterExploitMove} is recaptured by the ${message.removedDefender.piece} from ${message.removedDefender.square} with ${message.playedSoleRecaptureMove}`;
    const occurrenceComparison =
      message.branch.role === 'counterfactual_reference'
        ? `${reference}; ${played}`
        : `${played}; ${reference}`;
    return locale === 'ko-KR'
      ? `[${message.causeKind}] ${occurrenceComparison}. 경로 ${message.pathOccurrenceId.slice(0, 8)}는 ${message.absence.issuerEvidenceId}가 ${message.absence.fen} (ply ${message.absence.ply})에서 발급한 ${message.absence.query} 부재와 하위 증거 ${premises}를 보존합니다.`
      : `[${message.causeKind}] ${occurrenceComparison}. Path ${message.pathOccurrenceId.slice(0, 8)} retains absence ${message.absence.query}, issued by ${message.absence.issuerEvidenceId} at ${message.absence.fen} (ply ${message.absence.ply}), and lower proofs ${premises}.`;
  }
  const targets = message.resultTargetSubjects.join(', ');
  const premises = message.premises.map(premise => premise.resultId.slice(0, 8)).join(', ');
  const links = message.occurrenceLinkKeys.map(id => id.slice(0, 8)).join(', ');
  return locale === 'ko-KR'
    ? `[${message.causeKind}] ${message.rootActor.from}의 ${message.rootActor.pieceBefore}가 ${message.rootActor.to}로 간 뒤 ${message.replyMove} 응수에서도 ${message.pathRealizationActor.from}의 ${message.pathRealizationActor.pieceBefore}가 ${message.pathRealizationMove}로 ${targets}의 ${message.consequenceKind} 결과를 실현합니다 (ply ${message.pathRealizationPly}, ${message.pathRealizationMatchKind}; ${message.replyClosure.issuer} / ${message.replyClosure.issuerEvidenceId} 정본이 ${message.replyClosure.rootAfter.fen}에서 응수 ${message.replyClosure.legalReplyMoves.join(', ')}를 발급하고 ${message.replyClosure.coverageIssuer} / ${message.replyClosure.coverageEvidenceId}가 가지 완결을 인증함, horizon ${message.replyClosure.certifiedHorizonPlyOffset} ply, 경로 전제 ${premises}, 인과 링크 ${links}, 독립 경로 ${message.pathOccurrenceId.slice(0, 8)}).`
    : `[${message.causeKind}] After the ${message.rootActor.pieceBefore} moves ${message.rootActor.from}–${message.rootActor.to}, the ${message.pathRealizationActor.pieceBefore} from ${message.pathRealizationActor.from} plays ${message.pathRealizationMove} to realize ${message.consequenceKind} for ${targets} against ${message.replyMove} (ply ${message.pathRealizationPly}, ${message.pathRealizationMatchKind}; ${message.replyClosure.issuer} / ${message.replyClosure.issuerEvidenceId} issues replies ${message.replyClosure.legalReplyMoves.join(', ')} at ${message.replyClosure.rootAfter.fen}; ${message.replyClosure.coverageIssuer} / ${message.replyClosure.coverageEvidenceId} certifies complete branch coverage; horizon ${message.replyClosure.certifiedHorizonPlyOffset} ply; path premises ${premises}; causal links ${links}; independent path ${message.pathOccurrenceId.slice(0, 8)}).`;
}

export function moveReviewReasonRole(
  core: MoveReviewCore,
  reasonId: string,
): MoveReviewReasonRole | undefined {
  if (core.reasonRefs.primary === reasonId) return 'primary';
  if (core.reasonRefs.support.includes(reasonId)) return 'support';
  if (core.reasonRefs.routes.includes(reasonId)) return 'proof-route';
  return;
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
      typeof raw.stop_condition !== 'string' ||
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
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'phase',
      'legal_move_count',
      'root_candidate_lines_admitted',
      'selected_commentaries_completed',
      'physical_works_issued',
      'physical_reports_accepted',
      'causal_waves_completed',
    ]) ||
    value.phase !== phase ||
    integerInRange(value.legal_move_count, 1, 218) === undefined ||
    integerInRange(value.root_candidate_lines_admitted, 0, 3) === undefined ||
    integerInRange(value.selected_commentaries_completed, 0, 1) === undefined ||
    integerInRange(value.physical_works_issued, 0, 32) === undefined ||
    integerInRange(value.physical_reports_accepted, 0, 32) === undefined ||
    integerInRange(value.causal_waves_completed, 0, 3) === undefined
  )
    return false;
  const issued = value.physical_works_issued as number;
  const accepted = value.physical_reports_accepted as number;
  return phase === 'completed' || phase === 'stopped' ? accepted <= issued : issued === accepted + 1;
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
    (value.purpose !== 'root_search' &&
      value.purpose !== 'focus_comparison' &&
      value.purpose !== 'causal_probe') ||
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
      value.max_search_elapsed_ms === 3_500) ||
    (value.purpose === 'causal_probe' &&
      value.engine_position_moves_uci.length > subject.movePrefixUci.length &&
      unrestricted &&
      limits.nodes === 2_000_000 &&
      limits.movetimeMs === 2_500 &&
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
  const proof = buildProof(`line:${move}`, startFen, endpoint.moves, []);
  return proof ? { kind: 'single-candidate-insight', proof } : undefined;
}

function projectCommentary(
  value: unknown,
  move: Uci,
  subject: MoveReviewSubject,
): MoveReviewCandidateReview | undefined {
  if (
    !isObject(value) ||
    !hasOnlyKeys(value, ['primary', 'causal_explanations'], ['primary']) ||
    !isObject(value.primary)
  )
    return;
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
        'candidate_set',
      ])
    )
      return;
    verdictRef = semanticId(primary.comparison_evidence_id);
    reviewedEndpoint = projectEndpoint(primary.best_endpoint);
    referenceEndpoint = projectEndpoint(primary.runner_up_endpoint);
    const runnerUpVerdictCode = moveReviewVerdictCode(primary.runner_up_verdict_code);
    const candidateSet = projectCandidateSet(primary.candidate_set);
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
      !candidateSet ||
      delta === undefined
    )
      return;
    bestChoice = { runnerUpVerdictCode, runnerUpUci, candidateSet };
    if (reviewedEndpoint.winPercent !== undefined && referenceEndpoint.winPercent !== undefined)
      winChance = {
        referencePercent: referenceEndpoint.winPercent,
        playedPercent: reviewedEndpoint.winPercent,
        changePercentagePoints: delta,
      };
  } else return;
  const bestUci = primary.kind === 'best_choice' ? reviewedEndpoint?.moves[0] : referenceEndpoint?.moves[0];
  if (!bestUci || !verdictCode || !referenceEndpoint || !reviewedEndpoint) return;
  const causalTransport = projectCausalTransport(
    value.causal_explanations,
    verdictRef,
    move,
    subject.before.fen,
  );
  if (!causalTransport) return;
  const projectedReasons = causalTransport.reasons;
  const lineProof = buildProof(`pv:${move}`, subject.before.fen, reviewedEndpoint.moves, []);
  const lineReason: MoveReviewReason | undefined = lineProof
    ? {
        id: `pv:${move}`,
        messageSlots: { candidateUci: move },
        message: { kind: 'line', moves: reviewedEndpoint.moves },
        proof: lineProof,
      }
    : undefined;
  const reasons = [...projectedReasons.map(reason => reason.reason), ...(lineReason ? [lineReason] : [])];
  if (reasons.length < 1 || !unique(reasons.map(reason => reason.id))) return;
  const causalPrimaryIds = projectedReasons
    .filter(reason => reason.role === 'primary')
    .map(reason => reason.reason.id);
  const routes = projectedReasons
    .filter(reason => reason.role === 'proof-route')
    .map(reason => reason.reason.id);
  if (causalPrimaryIds.length > 1) routes.push(...causalPrimaryIds);
  const primaryReason = causalPrimaryIds.length === 1 ? causalPrimaryIds[0] : lineReason?.id;
  const support = projectedReasons
    .filter(reason => reason.role === 'support')
    .map(reason => reason.reason.id);
  if (lineReason && lineReason.id !== primaryReason) support.push(lineReason.id);
  const coreFields: MoveReviewCoreFields = {
    verdictRef,
    verdictCode,
    verdictSymbol: bestChoice ? 'none' : verdictSymbolByCode[verdictCode],
    playedUci: move,
    bestUci,
    ...(winChance ? { winChance } : {}),
    ...(referenceEndpoint.terminal ? { referenceTerminal: referenceEndpoint.terminal } : {}),
    ...(reviewedEndpoint.terminal ? { reviewedTerminal: reviewedEndpoint.terminal } : {}),
    reasonRefs: { ...(primaryReason ? { primary: primaryReason } : {}), support, routes },
  };
  const core: MoveReviewCore = bestChoice
    ? { ...coreFields, kind: 'best-choice', bestChoice }
    : { ...coreFields, kind: 'move-verdict' };
  return {
    kind: 'move-verdict',
    core,
    reasons,
  };
}

interface ProjectedCausalReason {
  role: MoveReviewReasonRole;
  reason: MoveReviewReason;
}

type MoveReviewCauseEffectMode = 'played_liability' | 'alternative_resource' | 'played_value';

interface ProjectedCausalTransport {
  reasons: ProjectedCausalReason[];
}

interface CausalFacetMetadata {
  role: 'lead' | 'supporting';
  causeEvidenceId: string;
  causeKind: string;
  effectMode: MoveReviewCauseEffectMode;
  exposure: 'primary' | 'complementary';
  channels: unknown[];
}

function projectCausalTransport(
  value: unknown,
  comparisonEvidenceId: string,
  candidateMove: Uci,
  startFen: FEN,
): ProjectedCausalTransport | undefined {
  if (value === undefined) return { reasons: [] };
  if (!Array.isArray(value) || value.length < 1) return;
  const reasons: ProjectedCausalReason[] = [];
  const causeEvidenceIds = new Set<string>();
  for (const explanation of value) {
    if (
      !isObject(explanation) ||
      !hasExactKeys(explanation, ['kind', 'facets']) ||
      !Array.isArray(explanation.facets)
    )
      return;
    const facets = explanation.facets.map(projectCausalFacetMetadata);
    if (!facets.every((facet): facet is CausalFacetMetadata => !!facet)) return;
    if (explanation.kind === 'single_cause') {
      if (facets.length !== 1 || facets[0]!.role !== 'lead') return;
    } else if (explanation.kind === 'exact_pvb_responsibility') {
      if (
        facets.length < 2 ||
        facets[0]!.role !== 'lead' ||
        facets[0]!.effectMode !== 'played_liability' ||
        facets[0]!.exposure !== 'primary' ||
        facets
          .slice(1)
          .some(
            facet =>
              facet.role !== 'supporting' ||
              facet.effectMode !== 'alternative_resource' ||
              facet.exposure !== 'complementary',
          )
      )
        return;
    } else return;
    for (const facet of facets) {
      if (causeEvidenceIds.has(facet.causeEvidenceId)) return;
      causeEvidenceIds.add(facet.causeEvidenceId);
      const facetReasons: MoveReviewReason[] = [];
      for (const channel of facet.channels) {
        const projected = projectCausalChannel(
          channel,
          facet.causeEvidenceId,
          facet.causeKind,
          facet.effectMode,
          comparisonEvidenceId,
          candidateMove,
          startFen,
        );
        if (!projected) return;
        facetReasons.push(...projected);
      }
      const role: MoveReviewReasonRole =
        facetReasons.length > 1 ? 'proof-route' : facet.role === 'lead' ? 'primary' : 'support';
      reasons.push(...facetReasons.map(reason => ({ role, reason })));
    }
  }
  return { reasons };
}

function projectCausalFacetMetadata(value: unknown): CausalFacetMetadata | undefined {
  if (!isObject(value)) return;
  const role = value.facet_role;
  const causeEvidenceId = nonEmptyWireString(value.cause_evidence_id);
  const effectMode = value.effect_mode;
  const exposure = value.exposure;
  if (
    (role !== 'lead' && role !== 'supporting') ||
    !causeEvidenceId ||
    (effectMode !== 'played_liability' &&
      effectMode !== 'alternative_resource' &&
      effectMode !== 'played_value') ||
    (exposure !== 'primary' && exposure !== 'complementary') ||
    !Array.isArray(value.channels) ||
    value.channels.length < 1
  )
    return;
  if (value.kind === 'wrong_move_order') {
    if (
      !hasExactKeys(value, [
        'facet_role',
        'cause_evidence_id',
        'kind',
        'proof_confidence',
        'effect_mode',
        'exposure',
        'source_side',
        'comparison_kind',
        'channels',
      ]) ||
      value.proof_confidence !== 'legal_replay_verified' ||
      effectMode !== 'alternative_resource' ||
      exposure !== 'primary' ||
      value.source_side !== 'reference' ||
      value.comparison_kind !== 'played_vs_best'
    )
      return;
  } else if (value.kind === 'passed_pawn_result') {
    if (
      !hasExactKeys(value, [
        'facet_role',
        'cause_evidence_id',
        'kind',
        'proof_confidence',
        'effect_mode',
        'exposure',
        'source_side',
        'comparison_kind',
        'channels',
      ]) ||
      value.proof_confidence !== 'legal_replay_verified' ||
      (value.source_side !== 'reference' && value.source_side !== 'candidate') ||
      value.comparison_kind !== 'played_vs_best' ||
      (value.source_side === 'candidate'
        ? effectMode !== 'played_value'
        : effectMode !== 'alternative_resource')
    )
      return;
  } else {
    if (
      !hasExactKeys(value, [
        'facet_role',
        'cause_evidence_id',
        'kind',
        'proof_confidence',
        'effect_mode',
        'exposure',
        'source_side',
        'event_move',
        'comparison_kind',
        'channels',
      ]) ||
      !standardCauseKind(value.kind) ||
      !standardProofConfidence(value.proof_confidence) ||
      !standardSourceSide(value.source_side) ||
      !uci(value.event_move) ||
      !standardComparisonKind(value.comparison_kind)
    )
      return;
  }
  return {
    role,
    causeEvidenceId,
    causeKind: value.kind,
    effectMode,
    exposure,
    channels: value.channels,
  };
}

function projectCausalChannel(
  value: unknown,
  causeEvidenceId: string,
  causeKind: string,
  effectMode: MoveReviewCauseEffectMode,
  comparisonEvidenceId: string,
  candidateMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  if (!isObject(value) || !semanticId(value.channel_id) || !semanticId(value.causal_signature)) return;
  if (Object.prototype.hasOwnProperty.call(value, 'resource_differential_proof')) {
    if (causeKind !== 'wrong_move_order') return;
    return projectResourceDifferential(
      value,
      causeEvidenceId,
      causeKind,
      effectMode,
      candidateMove,
      startFen,
    );
  }
  if (Object.prototype.hasOwnProperty.call(value, 'defense_obligation_change_proof')) {
    if (causeKind !== 'wrong_move_order') return;
    return projectDefenseObligationChange(
      value,
      causeEvidenceId,
      causeKind,
      effectMode,
      candidateMove,
      startFen,
    );
  }
  if (Object.prototype.hasOwnProperty.call(value, 'passed_pawn_result_proof'))
    return projectPassedPawnResult(
      value,
      causeEvidenceId,
      causeKind,
      effectMode,
      comparisonEvidenceId,
      candidateMove,
      startFen,
    );
  if (
    !hasOnlyKeys(
      value,
      [
        'channel_id',
        'causal_signature',
        'direct_change',
        'played_change',
        'actor',
        'targets',
        'mechanisms',
        'consequences',
        'witnesses',
        'proof_line_moves',
        'horizon',
        'proof_segment',
      ],
      [
        'channel_id',
        'causal_signature',
        'direct_change',
        'played_change',
        'actor',
        'targets',
        'mechanisms',
        'consequences',
        'witnesses',
        'proof_line_moves',
      ],
    )
  )
    return;
  if (
    !directCausalChange(value.direct_change) ||
    !playerFacingCausalChange(value.played_change) ||
    !isObject(value.actor) ||
    !hasExactKeys(value.actor, ['move_uci', 'side', 'piece', 'from', 'to']) ||
    !uci(value.actor.move_uci) ||
    (value.actor.side !== 'white' && value.actor.side !== 'black') ||
    !nonEmptyWireString(value.actor.piece) ||
    !key(value.actor.from) ||
    !key(value.actor.to) ||
    !Array.isArray(value.targets) ||
    !Array.isArray(value.mechanisms) ||
    !Array.isArray(value.consequences) ||
    !Array.isArray(value.witnesses) ||
    !validUciMoves(value.proof_line_moves, 1, 80) ||
    (value.horizon !== undefined &&
      (typeof value.horizon !== 'string' || !/^ply:(0|[1-9][0-9]*)$/.test(value.horizon)))
  )
    return;
  const proofLine = value.proof_line_moves;
  let proofSegment: Extract<MoveReviewReasonMessage, { kind: 'causal' }>['proofSegment'];
  let proofMoves = proofLine;
  if (value.proof_segment !== undefined) {
    if (
      !isObject(value.proof_segment) ||
      !hasExactKeys(value.proof_segment, ['terminal_relation', 'steps']) ||
      !terminalRelation(value.proof_segment.terminal_relation) ||
      !Array.isArray(value.proof_segment.steps) ||
      value.proof_segment.steps.length < 1 ||
      value.proof_segment.steps.length > 80
    )
      return;
    const steps = value.proof_segment.steps.map(step => {
      if (
        !isObject(step) ||
        !hasExactKeys(step, ['ply_offset', 'move_uci', 'role']) ||
        !uci(step.move_uci) ||
        !proofStepRole(step.role)
      )
        return;
      const offset = integerInRange(step.ply_offset, 0, proofLine.length - 1);
      return offset !== undefined && proofLine[offset] === step.move_uci
        ? { plyOffset: offset, moveUci: step.move_uci as Uci, role: step.role }
        : undefined;
    });
    if (!steps.every((step): step is NonNullable<typeof step> => !!step)) return;
    const offsets = steps.map(step => step.plyOffset);
    if (
      offsets[0] !== 0 ||
      !unique(offsets) ||
      !arrayEquals(
        offsets,
        [...offsets].sort((a, b) => a - b),
      )
    )
      return;
    proofMoves = proofLine.slice(0, offsets[offsets.length - 1]! + 1);
    proofSegment = { terminalRelation: value.proof_segment.terminal_relation, steps };
  }
  const actorMove = value.actor.move_uci as Uci;
  if (proofMoves[0] !== actorMove) return;
  const targets = projectChessObjects(value.targets);
  const mechanisms = projectChessObjects(value.mechanisms);
  const consequences = projectChessObjects(value.consequences);
  const witnesses = projectChessObjects(value.witnesses);
  if (!targets || !mechanisms || !consequences || !witnesses) return;
  const annotations: MoveReviewAnnotation[] = [
    {
      atPly: 1,
      shape: { kind: 'arrow', orig: value.actor.from as Key, dest: value.actor.to as Key, brush: 'green' },
    },
    ...value.targets.flatMap(target =>
      isObject(target) && target.kind === 'square' && key(target.key)
        ? [
            {
              atPly: proofMoves.length,
              shape: { kind: 'square' as const, key: target.key as Key, brush: 'yellow' as const },
            },
          ]
        : [],
    ),
  ];
  const id = value.channel_id as string;
  const proof = buildProof(`cause:${id}`, startFen, proofMoves, annotations);
  if (!proof) return;
  return [
    {
      id,
      messageSlots: { candidateUci: candidateMove },
      message: {
        kind: 'causal',
        causeEvidenceId,
        causeKind,
        effectMode,
        directChange: value.direct_change,
        playedChange: value.played_change,
        actor: {
          moveUci: actorMove,
          side: value.actor.side,
          piece: value.actor.piece as string,
          from: value.actor.from as Key,
          to: value.actor.to as Key,
        },
        targets,
        mechanisms,
        consequences,
        witnesses,
        ...(value.horizon !== undefined ? { horizon: value.horizon } : {}),
        ...(proofSegment ? { proofSegment } : {}),
      },
      proof,
    },
  ];
}

interface MoveReviewWireStep {
  index: number;
  ply: number;
  move: Uci;
  fenBefore: FEN;
  fenAfter: FEN;
  provenance: 'observed_game_move' | 'certified_analysis_move';
  stepKey?: string;
  line?: MoveReviewTypedLine;
  incomingLink?: {
    kind: 'adjacent_legal_replay' | 'certified_causal_dependency';
    fromStepKey: string;
    toStepKey: string;
    occurrenceLinkKey: string;
  };
}

interface MoveReviewPassedPawnResultWireStep extends MoveReviewWireStep {
  stepKey: string;
  line: MoveReviewTypedLine;
}

interface MoveReviewPassedPawnResultWireBranch extends MoveReviewWireBranch {
  steps: MoveReviewPassedPawnResultWireStep[];
}

interface MoveReviewWireBranch extends MoveReviewTypedBranch {
  steps: MoveReviewWireStep[];
  replyMove?: Uci;
  sourceProbeId?: string;
}

type MoveReviewResourcePath = {
  id: string;
  premises: MoveReviewTypedPremise[];
  absence: Extract<MoveReviewReasonMessage, { kind: 'resource-differential' }>['absence'];
};

type MoveReviewDefenseObligationPath = {
  id: string;
  premises: MoveReviewTypedPremise[];
  absence: Extract<MoveReviewReasonMessage, { kind: 'defense-obligation-change' }>['absence'];
};

type MoveReviewPassedPawnResultPath = {
  id: string;
  branch: MoveReviewPassedPawnResultWireBranch & { replyMove: Uci };
  realizationActor: MoveReviewTypedActor;
  realizationMove: Uci;
  realizationPly: number;
  realizationMatchKind: 'exact_move' | 'equivalent_function';
  premises: MoveReviewTypedPremise[];
  closureUseIds: string[];
};

function typedChannelProof(
  channel: Record<string, unknown>,
  key: 'resource_differential_proof' | 'defense_obligation_change_proof' | 'passed_pawn_result_proof',
): Record<string, unknown> | undefined {
  return hasExactKeys(channel, ['channel_id', 'causal_signature', 'direct_change', 'played_change', key]) &&
    channel.direct_change === 'occurred' &&
    isObject(channel[key])
    ? channel[key]
    : undefined;
}

function typedPassedPawnResultChannelProof(
  channel: Record<string, unknown>,
): Record<string, unknown> | undefined {
  return hasExactKeys(channel, [
    'channel_id',
    'causal_signature',
    'direct_change',
    'passed_pawn_result_proof',
  ]) &&
    channel.direct_change === 'occurred' &&
    isObject(channel.passed_pawn_result_proof)
    ? channel.passed_pawn_result_proof
    : undefined;
}

function projectResourceDifferential(
  channel: Record<string, unknown>,
  causeEvidenceId: string,
  causeKind: string,
  effectMode: MoveReviewCauseEffectMode,
  candidateMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  const wire = typedChannelProof(channel, 'resource_differential_proof');
  if (!wire || channel.played_change !== 'missed') return;
  if (
    !hasExactKeys(wire, [
      'family',
      'trigger_mechanism',
      'source_evidence_id',
      'semantic_id',
      'occurrence_id',
      'dependency_fingerprint',
      'counterfactual_reference_branch',
      'played_root_branch',
      'proof_paths',
      'participants',
      'realizing_move',
      'played_root_branch_legal_defense_move',
    ]) ||
    wire.family !== 'immediate_forced_reply_resource_differential' ||
    wire.trigger_mechanism !== 'forced_displacement' ||
    !nonEmptyWireString(wire.source_evidence_id) ||
    !typedHash(wire.semantic_id) ||
    !typedHash(wire.occurrence_id) ||
    !typedHash(wire.dependency_fingerprint) ||
    !isObject(wire.participants) ||
    !Array.isArray(wire.proof_paths) ||
    wire.proof_paths.length < 1
  )
    return;
  const reference = projectTypedBranch(
    wire.counterfactual_reference_branch,
    'counterfactual_reference',
    startFen,
  );
  const played = projectTypedBranch(wire.played_root_branch, 'observed_played_root', startFen);
  const realizingMove = uci(wire.realizing_move);
  const defenseMove = uci(wire.played_root_branch_legal_defense_move);
  const participants = projectResourceParticipants(wire.participants);
  if (
    !reference ||
    !played ||
    reference.id === played.id ||
    reference.rootMove === played.rootMove ||
    reference.steps[0]?.ply !== played.steps[0]?.ply ||
    reference.steps[0]!.ply < 1 ||
    played.rootMove !== candidateMove ||
    !realizingMove ||
    !defenseMove ||
    !participants ||
    reference.steps[2]?.move !== realizingMove ||
    played.steps[0]?.move !== realizingMove ||
    played.rootMove !== realizingMove ||
    typedActorMove(participants.realizer) !== realizingMove ||
    typedActorMove(participants.trigger) !== reference.steps[0]!.move ||
    played.steps[1]?.move !== defenseMove ||
    participants.forcedReply.moveUci !== reference.steps[1]?.move ||
    typedActorMove(participants.forcedReply) !== participants.forcedReply.moveUci ||
    participants.playedDefender.moveUci !== defenseMove ||
    typedActorMove(participants.playedDefender) !== participants.playedDefender.moveUci ||
    participants.trigger.side !== participants.realizer.side ||
    participants.trigger.side !== fenSideToMove(startFen) ||
    participants.forcedReply.side !== participants.playedDefender.side ||
    participants.forcedReply.side !== participants.disabledDefender.side ||
    participants.forcedReply.side !== participants.capturedTarget.side ||
    participants.trigger.side === participants.forcedReply.side ||
    participants.capturedTarget.square !== participants.realizer.to ||
    participants.playedDefender.to !== participants.realizer.to ||
    participants.disabledDefender.side !== participants.playedDefender.side ||
    participants.disabledDefender.piece !== participants.playedDefender.pieceBefore ||
    participants.disabledDefender.square !== participants.playedDefender.from
  )
    return;
  const mechanismConsistent =
    participants.disabledDefender.piece === participants.forcedReply.pieceBefore &&
    participants.disabledDefender.square === participants.forcedReply.from;
  if (!mechanismConsistent) return;
  const exactPaths = wire.proof_paths.map(path => projectResourcePath(path, reference.id, played.id));
  if (
    !exactPaths.every((path): path is MoveReviewResourcePath => !!path) ||
    !canonicalStrings(exactPaths.map(path => path.id)) ||
    !unique(exactPaths.map(path => path.absence.useId)) ||
    exactPaths.some(
      path =>
        path.absence.query !==
          `legal-capture:${participants.capturedTarget.side}:${participants.realizer.to}` ||
        path.absence.fen !== reference.steps[2]!.fenAfter ||
        path.absence.ply !== reference.steps[2]!.ply,
    )
  )
    return;
  return exactPaths.flatMap((path, pathIndex) =>
    [reference, played].map(branch => {
      const proof = proofFromWireSteps(`cause:${path.id}:${pathIndex}:${branch.id}`, startFen, branch.steps)!;
      const counterpart = branch === reference ? played : reference;
      return {
        id: proof.id,
        messageSlots: { candidateUci: candidateMove },
        message: {
          kind: 'resource-differential' as const,
          channelId: channel.channel_id as string,
          causalSignature: channel.causal_signature as string,
          causeEvidenceId,
          causeKind,
          effectMode,
          directChange: 'occurred',
          playedChange: 'missed',
          family: 'immediate_forced_reply_resource_differential',
          sourceEvidenceId: wire.source_evidence_id as string,
          semanticId: wire.semantic_id as string,
          occurrenceId: wire.occurrence_id as string,
          dependencyFingerprint: wire.dependency_fingerprint as string,
          pathOccurrenceId: path.id,
          branch: wireBranchIdentity(branch),
          counterpart: wireBranchIdentity(counterpart),
          trigger: participants.trigger,
          forcedReply: participants.forcedReply,
          realizer: participants.realizer,
          realizingMove,
          capturedTarget: participants.capturedTarget,
          playedDefender: participants.playedDefender,
          disabledDefender: participants.disabledDefender,
          premises: path.premises,
          absence: path.absence,
          triggerMechanism: 'forced_displacement',
        },
        proof,
      };
    }),
  );
}

function projectDefenseObligationChange(
  channel: Record<string, unknown>,
  causeEvidenceId: string,
  causeKind: string,
  effectMode: MoveReviewCauseEffectMode,
  candidateMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  const wire = typedChannelProof(channel, 'defense_obligation_change_proof');
  if (
    !wire ||
    channel.played_change !== 'missed' ||
    !hasExactKeys(wire, [
      'contract',
      'mechanism',
      'source_evidence_id',
      'semantic_id',
      'occurrence_id',
      'dependency_fingerprint',
      'counterfactual_reference_branch',
      'played_root_branch',
      'proof_paths',
      'participants',
      'later_exploit_move',
      'played_sole_recapture_move',
    ]) ||
    wire.contract !== 'defense_obligation_change' ||
    wire.mechanism !== 'sole_recapturer_removal' ||
    !nonEmptyWireString(wire.source_evidence_id) ||
    !typedHash(wire.semantic_id) ||
    !typedHash(wire.occurrence_id) ||
    !typedHash(wire.dependency_fingerprint) ||
    !Array.isArray(wire.proof_paths) ||
    wire.proof_paths.length < 1
  )
    return;
  const reference = projectTypedBranch(
    wire.counterfactual_reference_branch,
    'counterfactual_reference',
    startFen,
  );
  const played = projectTypedBranch(wire.played_root_branch, 'observed_played_root', startFen);
  const laterExploitMove = uci(wire.later_exploit_move);
  const playedSoleRecaptureMove = uci(wire.played_sole_recapture_move);
  const participants = projectDefenseObligationParticipants(wire.participants);
  if (
    !reference ||
    !played ||
    reference.id === played.id ||
    reference.rootMove === played.rootMove ||
    reference.steps[0]?.ply !== played.steps[0]?.ply ||
    reference.steps[0]!.ply < 1 ||
    played.rootMove !== candidateMove ||
    !laterExploitMove ||
    !playedSoleRecaptureMove ||
    !participants ||
    reference.steps[2]?.move !== laterExploitMove ||
    played.steps[0]?.move !== laterExploitMove ||
    played.rootMove !== laterExploitMove ||
    reference.steps[1]?.move !== participants.removalRecapture.moveUci ||
    played.steps[1]?.move !== playedSoleRecaptureMove ||
    participants.playedSoleRecapture.moveUci !== playedSoleRecaptureMove ||
    typedActorMove(participants.remover) !== reference.steps[0]!.move ||
    typedActorMove(participants.removalRecapture) !== participants.removalRecapture.moveUci ||
    typedActorMove(participants.laterExploit) !== laterExploitMove ||
    typedActorMove(participants.playedSoleRecapture) !== playedSoleRecaptureMove ||
    participants.remover.side !== participants.laterExploit.side ||
    participants.remover.side !== fenSideToMove(startFen) ||
    participants.remover.side === participants.removedDefender.side ||
    participants.removedDefender.side !== participants.removalRecapture.side ||
    participants.removedDefender.side !== participants.capturedTarget.side ||
    participants.removedDefender.side !== participants.playedSoleRecapture.side ||
    participants.remover.to !== participants.removedDefender.square ||
    participants.removalRecapture.to !== participants.remover.to ||
    participants.laterExploit.to !== participants.capturedTarget.square ||
    participants.playedSoleRecapture.from !== participants.removedDefender.square ||
    participants.playedSoleRecapture.pieceBefore !== participants.removedDefender.piece ||
    participants.playedSoleRecapture.to !== participants.laterExploit.to
  )
    return;
  const paths = wire.proof_paths.map(path => projectDefenseObligationPath(path, reference, played));
  if (
    !paths.every((path): path is MoveReviewDefenseObligationPath => !!path) ||
    !canonicalStrings(paths.map(path => path.id)) ||
    !unique(paths.map(path => path.absence.useId)) ||
    paths.some(
      path =>
        path.absence.query !==
          `legal-capture:${participants.removedDefender.side}:${participants.laterExploit.to}` ||
        path.absence.fen !== reference.steps[2]!.fenAfter ||
        path.absence.ply !== reference.steps[2]!.ply,
    )
  )
    return;
  return paths.flatMap((path, pathIndex) =>
    [reference, played].map((branch, branchIndex) => {
      const proof = proofFromWireSteps(
        `cause:${path.id}:${pathIndex}:${branchIndex}:${branch.id}`,
        startFen,
        branch.steps,
      )!;
      const counterpart = branch === reference ? played : reference;
      return {
        id: proof.id,
        messageSlots: { candidateUci: candidateMove },
        message: {
          kind: 'defense-obligation-change' as const,
          channelId: channel.channel_id as string,
          causalSignature: channel.causal_signature as string,
          causeEvidenceId,
          causeKind,
          effectMode,
          directChange: 'occurred' as const,
          playedChange: 'missed' as const,
          contract: 'defense_obligation_change' as const,
          mechanism: 'sole_recapturer_removal' as const,
          sourceEvidenceId: wire.source_evidence_id as string,
          semanticId: wire.semantic_id as string,
          occurrenceId: wire.occurrence_id as string,
          dependencyFingerprint: wire.dependency_fingerprint as string,
          pathOccurrenceId: path.id,
          branch: wireBranchIdentity(branch),
          counterpart: wireBranchIdentity(counterpart),
          ...participants,
          laterExploitMove,
          playedSoleRecaptureMove,
          premises: path.premises,
          absence: path.absence,
        },
        proof,
      };
    }),
  );
}

function projectDefenseObligationPath(
  value: unknown,
  reference: MoveReviewWireBranch,
  played: MoveReviewWireBranch,
): MoveReviewDefenseObligationPath | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['path_occurrence_id', 'premises', 'closed_absence_uses']) ||
    !typedHash(value.path_occurrence_id) ||
    !Array.isArray(value.premises) ||
    value.premises.length !== 3 ||
    !Array.isArray(value.closed_absence_uses) ||
    value.closed_absence_uses.length !== 1
  )
    return;
  const expected = [
    ['reference_defender_removal', reference.id, 'counterfactual_reference', 0],
    ['reference_later_exploit_inventory', reference.id, 'counterfactual_reference', 2],
    ['played_immediate_exploit_inventory', played.id, 'observed_played_root', 0],
  ] as const;
  const premises = value.premises.map((premise, index) => projectTypedPremise(premise, expected[index]![1]));
  if (
    !premises.every((premise): premise is MoveReviewTypedPremise => !!premise) ||
    !unique(premises.map(premise => premise.resultId)) ||
    premises.some((premise, index) => {
      const [role, , branchRole, stepIndex] = expected[index]!;
      return (
        premise.role !== role ||
        premise.contract !== 'capture_recapture_inventory' ||
        !premise.resultId.startsWith('capture_recapture_inventory:') ||
        premise.branchRole !== branchRole ||
        premise.stepIndex !== stepIndex
      );
    })
  )
    return;
  const absence = value.closed_absence_uses[0];
  if (
    !isObject(absence) ||
    !hasExactKeys(absence, [
      'use_id',
      'role',
      'semantic_proof_id',
      'issuer',
      'issuer_evidence_id',
      'issuer_occurrence_id',
      'query',
      'branch_id',
      'branch_role',
      'after_step_index',
      'position',
    ]) ||
    absence.role !== 'reference_replacement_recapture_absent' ||
    absence.issuer !== 'position_relation_extractor.closed_relation_inventory' ||
    !typedHash(absence.use_id) ||
    !typedHash(absence.semantic_proof_id) ||
    !nonEmptyWireString(absence.issuer_evidence_id) ||
    !typedHash(absence.issuer_occurrence_id) ||
    typeof absence.query !== 'string' ||
    !/^legal-capture:(white|black):[a-h][1-8]$/.test(absence.query) ||
    absence.branch_id !== reference.id ||
    absence.branch_role !== 'counterfactual_reference' ||
    absence.after_step_index !== 2
  )
    return;
  const position = isObject(absence.position) ? absence.position : undefined;
  const fen = position ? fenText(position.fen) : undefined;
  const ply = position ? nonNegativeInteger(position.ply) : undefined;
  if (
    !position ||
    !hasExactKeys(position, ['fen', 'ply', 'scope']) ||
    !fen ||
    ply === undefined ||
    position.scope !== 'best_line'
  )
    return;
  return {
    id: value.path_occurrence_id as string,
    premises,
    absence: {
      useId: absence.use_id as string,
      role: 'reference_replacement_recapture_absent',
      semanticProofId: absence.semantic_proof_id as string,
      issuer: 'position_relation_extractor.closed_relation_inventory',
      issuerEvidenceId: absence.issuer_evidence_id as string,
      issuerOccurrenceId: absence.issuer_occurrence_id as string,
      query: absence.query,
      branchId: reference.id,
      afterStepIndex: 2,
      fen,
      ply,
      scope: 'best_line',
    },
  };
}

function projectDefenseObligationParticipants(value: unknown):
  | {
      remover: MoveReviewTypedActor;
      removedDefender: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      removalRecapture: MoveReviewTypedActor & { moveUci: Uci };
      laterExploit: MoveReviewTypedActor;
      capturedTarget: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      playedSoleRecapture: MoveReviewTypedActor & { moveUci: Uci };
    }
  | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'remover',
      'removed_defender',
      'removal_recapture',
      'later_exploit',
      'captured_target',
      'played_sole_recapture',
    ])
  )
    return;
  const remover = projectTypedActor(value.remover);
  const removedDefender = projectColoredPiece(value.removed_defender);
  const removalRecapture = projectTypedActor(value.removal_recapture, true);
  const laterExploit = projectTypedActor(value.later_exploit);
  const capturedTarget = projectColoredPiece(value.captured_target);
  const playedSoleRecapture = projectTypedActor(value.played_sole_recapture, true);
  return remover &&
    removedDefender &&
    removalRecapture?.moveUci &&
    laterExploit &&
    capturedTarget &&
    playedSoleRecapture?.moveUci
    ? {
        remover,
        removedDefender,
        removalRecapture: { ...removalRecapture, moveUci: removalRecapture.moveUci },
        laterExploit,
        capturedTarget,
        playedSoleRecapture: { ...playedSoleRecapture, moveUci: playedSoleRecapture.moveUci },
      }
    : undefined;
}

function projectColoredPiece(
  value: unknown,
): { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key } | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['side', 'piece', 'square']) ||
    (value.side !== 'white' && value.side !== 'black') ||
    !pieceRole(value.piece) ||
    !key(value.square)
  )
    return;
  return { side: value.side, piece: value.piece as MoveReviewPieceRole, square: value.square as Key };
}

function projectResourcePath(
  value: unknown,
  referenceBranchId: string,
  playedBranchId: string,
): MoveReviewResourcePath | undefined {
  const expected = [
    [
      'created_check_response',
      'created_check_response_inventory',
      referenceBranchId,
      'counterfactual_reference',
      0,
    ],
    [
      'reference_capture_recapture',
      'capture_recapture_inventory',
      referenceBranchId,
      'counterfactual_reference',
      2,
    ],
    ['played_capture_recapture', 'capture_recapture_inventory', playedBranchId, 'observed_played_root', 0],
  ] as const;
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['path_occurrence_id', 'premises', 'closed_absence_uses']) ||
    !typedHash(value.path_occurrence_id) ||
    !Array.isArray(value.premises) ||
    value.premises.length !== expected.length ||
    !Array.isArray(value.closed_absence_uses) ||
    value.closed_absence_uses.length !== 1
  )
    return;
  const premises = value.premises.map((premise, index) => projectTypedPremise(premise, expected[index]![2]));
  if (
    !premises.every((premise): premise is MoveReviewTypedPremise => !!premise) ||
    premises.some((premise, index) => {
      const [role, contract, , branchRole, stepIndex] = expected[index]!;
      return (
        premise.role !== role ||
        premise.contract !== contract ||
        !premise.resultId.startsWith(`${contract}:`) ||
        premise.branchRole !== branchRole ||
        premise.stepIndex !== stepIndex
      );
    })
  )
    return;
  const absence = value.closed_absence_uses[0];
  if (
    !isObject(absence) ||
    !hasExactKeys(absence, [
      'use_id',
      'role',
      'semantic_proof_id',
      'issuer',
      'issuer_evidence_id',
      'issuer_occurrence_id',
      'query',
      'branch_id',
      'branch_role',
      'after_step_index',
      'position',
    ]) ||
    absence.role !== 'reference_recapture_absent' ||
    absence.issuer !== 'position_relation_extractor.closed_relation_inventory' ||
    !typedHash(absence.use_id) ||
    !typedHash(absence.semantic_proof_id) ||
    !nonEmptyWireString(absence.issuer_evidence_id) ||
    !typedHash(absence.issuer_occurrence_id) ||
    absence.branch_id !== referenceBranchId ||
    absence.branch_role !== 'counterfactual_reference' ||
    absence.after_step_index !== 2 ||
    typeof absence.query !== 'string' ||
    !/^legal-capture:(white|black):[a-h][1-8]$/.test(absence.query)
  )
    return;
  const position = isObject(absence.position) ? absence.position : undefined;
  const fen = position ? fenText(position.fen) : undefined;
  const ply = position ? nonNegativeInteger(position.ply) : undefined;
  if (
    !position ||
    !hasExactKeys(position, ['fen', 'ply', 'scope']) ||
    !fen ||
    ply === undefined ||
    position.scope !== 'best_line'
  )
    return;
  return {
    id: value.path_occurrence_id as string,
    premises,
    absence: {
      useId: absence.use_id as string,
      semanticProofId: absence.semantic_proof_id as string,
      issuer: absence.issuer,
      issuerEvidenceId: absence.issuer_evidence_id as string,
      issuerOccurrenceId: absence.issuer_occurrence_id as string,
      query: absence.query,
      branchId: absence.branch_id as string,
      afterStepIndex: 2,
      fen,
      ply,
      scope: 'best_line',
    },
  };
}

function projectResourceParticipants(value: unknown):
  | {
      trigger: MoveReviewTypedActor;
      forcedReply: MoveReviewTypedActor & { moveUci: Uci };
      realizer: MoveReviewTypedActor;
      capturedTarget: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      playedDefender: MoveReviewTypedActor & { moveUci: Uci };
      disabledDefender: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
    }
  | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'trigger',
      'forced_reply',
      'realizer',
      'captured_target',
      'played_defense',
      'disabled_defender',
    ])
  )
    return;
  const trigger = projectTypedActor(value.trigger);
  const forcedReply = projectTypedActor(value.forced_reply, true);
  const realizer = projectTypedActor(value.realizer);
  const playedDefender = projectTypedActor(value.played_defense, true);
  const target = value.captured_target;
  const defender = value.disabled_defender;
  if (
    !trigger ||
    !forcedReply?.moveUci ||
    !realizer ||
    !playedDefender?.moveUci ||
    !isObject(target) ||
    !hasExactKeys(target, ['side', 'piece', 'square']) ||
    (target.side !== 'white' && target.side !== 'black') ||
    !pieceRole(target.piece) ||
    !key(target.square) ||
    !isObject(defender) ||
    !hasExactKeys(defender, ['side', 'piece', 'square']) ||
    (defender.side !== 'white' && defender.side !== 'black') ||
    !pieceRole(defender.piece) ||
    !key(defender.square)
  )
    return;
  return {
    trigger,
    forcedReply: { ...forcedReply, moveUci: forcedReply.moveUci },
    realizer,
    capturedTarget: {
      side: target.side,
      piece: target.piece as MoveReviewPieceRole,
      square: target.square as Key,
    },
    playedDefender: { ...playedDefender, moveUci: playedDefender.moveUci },
    disabledDefender: {
      side: defender.side,
      piece: defender.piece as MoveReviewPieceRole,
      square: defender.square as Key,
    },
  };
}

function projectTypedActor(
  value: unknown,
  moveRequired = false,
  legalRelationRequired = false,
): MoveReviewTypedActor | undefined {
  const keys = [
    'side',
    'from',
    'to',
    'piece_before',
    'piece_after',
    ...(moveRequired ? ['move_uci'] : []),
    ...(legalRelationRequired ? ['legal_move_relation'] : []),
  ];
  if (
    !isObject(value) ||
    !hasExactKeys(value, keys) ||
    (value.side !== 'white' && value.side !== 'black') ||
    !key(value.from) ||
    !key(value.to) ||
    !pieceRole(value.piece_before) ||
    !pieceRole(value.piece_after)
  )
    return;
  const move = moveRequired ? uci(value.move_uci) : undefined;
  const legalRelation = legalRelationRequired ? typedHash(value.legal_move_relation) : undefined;
  if ((moveRequired && !move) || (legalRelationRequired && !legalRelation)) return;
  return {
    side: value.side,
    from: value.from as Key,
    to: value.to as Key,
    pieceBefore: value.piece_before as MoveReviewPieceRole,
    pieceAfter: value.piece_after as MoveReviewPieceRole,
    ...(move ? { moveUci: move } : {}),
    ...(legalRelation ? { legalMoveRelation: legalRelation } : {}),
  };
}

function projectTypedPremise(value: unknown, branchId: string): MoveReviewTypedPremise | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'role',
      'contract',
      'result_id',
      'source_premise_ids',
      'branch_id',
      'branch_role',
      'step_index',
    ]) ||
    !nonEmptyWireString(value.role) ||
    !nonEmptyWireString(value.contract) ||
    !/^(created_check_response_inventory|capture_recapture_inventory):[0-9a-f]{64}$/.test(
      nonEmptyWireString(value.result_id) ?? '',
    ) ||
    value.branch_id !== branchId ||
    !nonEmptyWireString(value.branch_role) ||
    !canonicalWireStrings(value.source_premise_ids, 1)
  )
    return;
  const stepIndex = nonNegativeInteger(value.step_index);
  return stepIndex === undefined
    ? undefined
    : {
        role: value.role as string,
        contract: value.contract as string,
        resultId: value.result_id as string,
        sourcePremiseIds: [...value.source_premise_ids],
        branchId,
        branchRole: value.branch_role as string,
        stepIndex,
      };
}

function projectPassedPawnResult(
  channel: Record<string, unknown>,
  causeEvidenceId: string,
  causeKind: string,
  effectMode: MoveReviewCauseEffectMode,
  comparisonEvidenceId: string,
  candidateMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  const wire = typedPassedPawnResultChannelProof(channel);
  if (!wire) return;
  if (
    !hasExactKeys(wire, [
      'contract',
      'source_evidence_id',
      'event_evidence_id',
      'comparison_evidence_id',
      'semantic_id',
      'occurrence_id',
      'dependency_fingerprint',
      'consequence_kind',
      'result_target_subjects',
      'root_actor',
      'realizing_actor',
      'root_line',
      'root_move',
      'root_ply',
      'realizing_move',
      'realizing_ply',
      'result_ply_offset',
      'closed_legal_reply_inventory',
      'branches',
      'proof_paths',
      'lower_premise_ids',
    ]) ||
    wire.contract !== 'passed_pawn_result_under_closed_replies' ||
    !nonEmptyWireString(wire.source_evidence_id) ||
    !nonEmptyWireString(wire.event_evidence_id) ||
    wire.comparison_evidence_id !== comparisonEvidenceId ||
    wire.consequence_kind !== 'passed_pawn_progress' ||
    !typedHash(wire.semantic_id) ||
    !typedHash(wire.occurrence_id) ||
    !typedHash(wire.dependency_fingerprint) ||
    !canonicalWireStrings(wire.result_target_subjects, 1, passedPawnResultSubject) ||
    !canonicalWireStrings(wire.lower_premise_ids, 1) ||
    !Array.isArray(wire.branches) ||
    wire.branches.length < 2 ||
    !Array.isArray(wire.proof_paths) ||
    wire.proof_paths.length < 1
  )
    return;
  const rootMove = uci(wire.root_move);
  const realizingMove = uci(wire.realizing_move);
  const rootActor = projectTypedActor(wire.root_actor, false, true);
  const realizingActor = projectTypedActor(wire.realizing_actor, false, true);
  const rootLine = projectPassedPawnResultLine(wire.root_line);
  const rootPly = nonNegativeInteger(wire.root_ply);
  const realizingPly = nonNegativeInteger(wire.realizing_ply);
  const resultPlyOffset = nonNegativeInteger(wire.result_ply_offset);
  const inventory = projectPassedPawnResultInventory(wire.closed_legal_reply_inventory);
  if (
    !rootMove ||
    rootMove !== candidateMove ||
    !realizingMove ||
    !rootActor ||
    !realizingActor ||
    !rootLine ||
    rootLine.rootMove !== rootMove ||
    inventory?.rootAfter.scope !== passedPawnResultTransitionScope(rootLine.role) ||
    rootPly === undefined ||
    rootPly < 1 ||
    realizingPly === undefined ||
    resultPlyOffset === undefined ||
    resultPlyOffset < 1 ||
    !inventory ||
    inventory.coverageEvidenceId !== wire.event_evidence_id ||
    !(wire.lower_premise_ids as string[]).includes(inventory.coverageEvidenceId) ||
    !(wire.lower_premise_ids as string[]).includes(inventory.issuerEvidenceId) ||
    !(wire.lower_premise_ids as string[]).includes(comparisonEvidenceId) ||
    typedActorMove(rootActor) !== rootMove ||
    typedActorMove(realizingActor) !== realizingMove ||
    rootActor.side !== fenSideToMove(startFen) ||
    realizingActor.side !== rootActor.side
  )
    return;
  const branches = wire.branches.map(branch => projectTypedBranch(branch, undefined, startFen));
  if (
    !branches.every(
      (branch): branch is MoveReviewPassedPawnResultWireBranch =>
        !!branch && branch.steps.every(isPassedPawnResultWireStep),
    ) ||
    !canonicalStrings(branches.map(branch => branch.id)) ||
    branches.some(
      branch =>
        branch.rootMove !== rootMove ||
        branch.steps[0]?.move !== rootMove ||
        branch.lineId !== rootLine.id ||
        branch.lineRole !== rootLine.role ||
        branch.lineRank !== rootLine.rank ||
        branch.provenance !== branches[0]?.provenance,
    )
  )
    return;
  const expected = branches.filter(branch => branch.role === 'expected_result_route');
  const replies = branches.filter(
    (
      branch,
    ): branch is MoveReviewPassedPawnResultWireBranch & {
      role: 'legal_reply';
      replyMove: Uci;
      sourceProbeId: string;
    } => branch.role === 'legal_reply' && !!branch.replyMove && !!branch.sourceProbeId,
  );
  const expectedBranch = expected[0];
  if (
    expected.length !== 1 ||
    !expectedBranch ||
    expectedBranch.steps[0]?.ply !== rootPly ||
    expectedBranch.steps[expectedBranch.steps.length - 1]?.move !== realizingMove ||
    expectedBranch.steps[expectedBranch.steps.length - 1]?.ply !== realizingPly ||
    realizingPly - rootPly !== resultPlyOffset ||
    replies.length !== inventory.bindings.length ||
    inventory.bindings.some(
      binding => !replies.some(reply => reply.replyMove === binding.move && reply.id === binding.branchId),
    ) ||
    branches.some(
      branch =>
        branch.steps[0]?.fenAfter !== inventory.rootAfter.fen ||
        branch.steps[0]!.ply !== inventory.rootAfter.ply,
    ) ||
    replies.some(
      reply =>
        reply.steps.length < 2 ||
        reply.steps[reply.steps.length - 1]!.ply - reply.steps[0]!.ply > inventory.horizon,
    )
  )
    return;
  const branchById = new Map(branches.map(branch => [branch.id, branch]));
  const paths = wire.proof_paths.map(path =>
    projectPassedPawnResultPath(
      path,
      branchById,
      expectedBranch,
      wire.event_evidence_id as string,
      comparisonEvidenceId,
    ),
  );
  if (
    !paths.every((path): path is MoveReviewPassedPawnResultPath => !!path) ||
    !canonicalStrings(paths.map(path => path.id)) ||
    !unique(paths.flatMap(path => path.closureUseIds)) ||
    paths.some(path => {
      const offset = path.realizationPly - path.branch.steps[0]!.ply;
      return offset < 1 || offset > inventory.horizon;
    }) ||
    replies.some(reply => !paths.some(path => path.branch.id === reply.id))
  )
    return;
  return paths.map((path, pathIndex) => {
    const proof = proofFromWireSteps(`cause:${path.id}:${pathIndex}`, startFen, path.branch.steps)!;
    return {
      id: proof.id,
      messageSlots: { candidateUci: candidateMove },
      message: {
        kind: 'passed-pawn-result' as const,
        channelId: channel.channel_id as string,
        causalSignature: channel.causal_signature as string,
        causeEvidenceId,
        causeKind,
        effectMode,
        directChange: 'occurred',
        contract: 'passed_pawn_result_under_closed_replies',
        sourceEvidenceId: wire.source_evidence_id as string,
        eventEvidenceId: wire.event_evidence_id as string,
        comparisonEvidenceId: wire.comparison_evidence_id as string,
        semanticId: wire.semantic_id as string,
        occurrenceId: wire.occurrence_id as string,
        dependencyFingerprint: wire.dependency_fingerprint as string,
        pathOccurrenceId: path.id,
        consequenceKind: 'passed_pawn_progress',
        resultTargetSubjects: [...(wire.result_target_subjects as string[])],
        rootActor,
        realizingActor,
        rootLine,
        rootMove,
        rootPly,
        replyMove: path.branch.replyMove,
        realizingMove,
        realizingPly,
        resultPlyOffset,
        pathRealizationActor: path.realizationActor,
        pathRealizationMove: path.realizationMove,
        pathRealizationPly: path.realizationPly,
        pathRealizationMatchKind: path.realizationMatchKind,
        replyBranch: wireBranchIdentity(path.branch),
        expectedBranches: expected.map(wireBranchIdentity),
        replyOccurrenceSteps: path.branch.steps,
        expectedOccurrenceSteps: expectedBranch.steps,
        premises: path.premises,
        closureUseIds: path.closureUseIds,
        lowerPremiseIds: [...(wire.lower_premise_ids as string[])],
        occurrenceLinkKeys: path.branch.steps.flatMap(step =>
          step.incomingLink ? [step.incomingLink.occurrenceLinkKey] : [],
        ),
        replyClosure: {
          issuer: inventory.issuer,
          issuerEvidenceId: inventory.issuerEvidenceId,
          coverageIssuer: inventory.coverageIssuer,
          coverageEvidenceId: inventory.coverageEvidenceId,
          rootAfter: inventory.rootAfter,
          legalReplyMoves: inventory.legalReplyMoves,
          branchByReply: inventory.bindings,
          certifiedHorizonPlyOffset: inventory.horizon,
        },
      },
      proof,
    };
  });
}

function projectPassedPawnResultInventory(value: unknown):
  | {
      issuer: 'structural_delta.canonical_legal_reply_inventory';
      issuerEvidenceId: string;
      coverageIssuer: 'passed_pawn_result_event.branch_complete_reply_coverage';
      coverageEvidenceId: string;
      rootAfter: { fen: FEN; ply: number; scope: string };
      legalReplyMoves: Uci[];
      bindings: Array<{ move: Uci; branchId: string }>;
      horizon: number;
    }
  | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'issuer',
      'issuer_evidence_id',
      'coverage_issuer',
      'coverage_evidence_id',
      'root_after',
      'legal_reply_moves',
      'branch_by_reply',
      'certified_horizon_ply_offset',
    ]) ||
    value.issuer !== 'structural_delta.canonical_legal_reply_inventory' ||
    !nonEmptyWireString(value.issuer_evidence_id) ||
    value.coverage_issuer !== 'passed_pawn_result_event.branch_complete_reply_coverage' ||
    !nonEmptyWireString(value.coverage_evidence_id) ||
    !isObject(value.root_after) ||
    !hasExactKeys(value.root_after, ['fen', 'ply', 'scope']) ||
    !validUciMoves(value.legal_reply_moves, 1) ||
    !canonicalStrings(value.legal_reply_moves) ||
    !Array.isArray(value.branch_by_reply) ||
    value.branch_by_reply.length !== value.legal_reply_moves.length
  )
    return;
  const rootAfterFen = fenText(value.root_after.fen);
  const rootAfterPly = nonNegativeInteger(value.root_after.ply);
  const rootAfterScope = nonEmptyWireString(value.root_after.scope);
  const horizon = integerInRange(value.certified_horizon_ply_offset, 1, Number.MAX_SAFE_INTEGER);
  if (!rootAfterFen || rootAfterPly === undefined || !rootAfterScope || horizon === undefined) return;
  const bindings = value.branch_by_reply.map(binding => {
    if (!isObject(binding) || !hasExactKeys(binding, ['reply_move', 'branch_id'])) return;
    const move = uci(binding.reply_move);
    const branchId = typedHash(binding.branch_id);
    return move && branchId ? { move, branchId } : undefined;
  });
  return bindings.every((binding): binding is { move: Uci; branchId: string } => !!binding) &&
    unique(bindings.map(binding => binding.move)) &&
    unique(bindings.map(binding => binding.branchId)) &&
    value.legal_reply_moves.every((move, index) => bindings[index]?.move === move)
    ? {
        issuer: value.issuer,
        issuerEvidenceId: value.issuer_evidence_id as string,
        coverageIssuer: value.coverage_issuer,
        coverageEvidenceId: value.coverage_evidence_id as string,
        rootAfter: { fen: rootAfterFen, ply: rootAfterPly, scope: rootAfterScope },
        legalReplyMoves: [...value.legal_reply_moves],
        bindings,
        horizon,
      }
    : undefined;
}

function projectTypedBranch(
  value: unknown,
  boundedCausalRole: 'counterfactual_reference' | 'observed_played_root' | undefined,
  startFen: FEN,
): MoveReviewWireBranch | undefined {
  const boundedCausal = boundedCausalRole !== undefined;
  if (
    !isObject(value) ||
    !(boundedCausal
      ? hasExactKeys(value, [
          'branch_id',
          'line_id',
          'line_role',
          'branch_role',
          'root_provenance',
          'line_rank',
          'root_move',
          'steps',
        ])
      : hasOnlyKeys(
          value,
          ['branch_id', 'role', 'reply_move', 'source_probe_id', 'line', 'root_provenance', 'steps'],
          ['branch_id', 'role', 'line', 'root_provenance', 'steps'],
        )) ||
    !typedHash(value.branch_id) ||
    (value.root_provenance !== 'counterfactual_analyzed_root' &&
      value.root_provenance !== 'observed_game_root') ||
    !Array.isArray(value.steps)
  )
    return;
  const line = projectPassedPawnResultLine(
    boundedCausal
      ? {
          line_id: value.line_id,
          line_role: value.line_role,
          line_rank: value.line_rank,
          root_move: value.root_move,
        }
      : value.line,
  );
  if (!line) return;
  const steps = value.steps.map(step => projectWireStep(step, !boundedCausal));
  if (
    !steps.every((step): step is MoveReviewWireStep => !!step) ||
    (!boundedCausal && !steps.every(isPassedPawnResultWireStep)) ||
    !wireStepsHaveOrderedOccurrences(steps, startFen) ||
    steps[0]?.move !== line.rootMove
  )
    return;
  if (boundedCausal) {
    const reference = boundedCausalRole === 'counterfactual_reference';
    if (
      value.branch_role !== boundedCausalRole ||
      line.role !== (reference ? 'best_reference' : 'played') ||
      value.root_provenance !== (reference ? 'counterfactual_analyzed_root' : 'observed_game_root') ||
      steps.length !== (reference ? 3 : 2) ||
      !wireStepsAreContinuous(steps, startFen) ||
      (reference
        ? steps.some(step => step.provenance !== 'certified_analysis_move')
        : steps[0]?.provenance !== 'observed_game_move' || steps[1]?.provenance !== 'certified_analysis_move')
    )
      return;
    return {
      id: value.branch_id as string,
      role: boundedCausalRole,
      provenance: value.root_provenance,
      lineId: line.id,
      lineRole: line.role,
      lineRank: line.rank,
      rootMove: line.rootMove,
      steps,
    };
  }
  if (value.role !== 'expected_result_route' && value.role !== 'legal_reply') return;
  const occurrenceSteps = steps as MoveReviewPassedPawnResultWireStep[];
  const observedRoot = line.role === 'played';
  if (
    !unique(occurrenceSteps.map(step => step.stepKey)) ||
    value.root_provenance !== (observedRoot ? 'observed_game_root' : 'counterfactual_analyzed_root') ||
    occurrenceSteps[0]?.provenance !== (observedRoot ? 'observed_game_move' : 'certified_analysis_move') ||
    occurrenceSteps.slice(1).some(step => step.provenance !== 'certified_analysis_move') ||
    !samePassedPawnResultLine(occurrenceSteps[0]?.line, line) ||
    occurrenceSteps.slice(1).some(step => !samePassedPawnResultLine(step.line, occurrenceSteps[1]!.line)) ||
    occurrenceSteps
      .slice(1)
      .some(
        (step, index) =>
          step.incomingLink?.fromStepKey !== occurrenceSteps[index]!.stepKey ||
          step.incomingLink?.toStepKey !== step.stepKey,
      )
  )
    return;
  if (value.role === 'expected_result_route') {
    if (
      Object.prototype.hasOwnProperty.call(value, 'reply_move') ||
      Object.prototype.hasOwnProperty.call(value, 'source_probe_id') ||
      occurrenceSteps.slice(1).some(step => !samePassedPawnResultLine(step.line, line)) ||
      occurrenceSteps.slice(1).some(step => step.incomingLink?.kind !== 'certified_causal_dependency')
    )
      return;
    return {
      id: value.branch_id as string,
      role: value.role,
      provenance: value.root_provenance,
      lineId: line.id,
      lineRole: line.role,
      lineRank: line.rank,
      rootMove: line.rootMove,
      steps: occurrenceSteps,
    };
  }
  const replyMove = uci(value.reply_move);
  const sourceProbeId = nonEmptyWireString(value.source_probe_id);
  if (
    !replyMove ||
    !sourceProbeId ||
    occurrenceSteps[1]?.move !== replyMove ||
    !wireStepsAreContinuous(occurrenceSteps, startFen) ||
    occurrenceSteps.slice(1).some(step => step.incomingLink?.kind !== 'adjacent_legal_replay') ||
    occurrenceSteps.slice(1).some(step => step.line.rootMove !== replyMove)
  )
    return;
  return {
    id: value.branch_id as string,
    role: value.role,
    provenance: value.root_provenance,
    lineId: line.id,
    lineRole: line.role,
    lineRank: line.rank,
    rootMove: line.rootMove,
    steps: occurrenceSteps,
    replyMove,
    sourceProbeId,
  };
}

function projectPassedPawnResultPath(
  value: unknown,
  branches: Map<string, MoveReviewPassedPawnResultWireBranch>,
  expectedBranch: MoveReviewPassedPawnResultWireBranch,
  eventEvidenceId: string,
  comparisonEvidenceId: string,
): MoveReviewPassedPawnResultPath | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'path_occurrence_id',
      'reply_branch_id',
      'realization_actor',
      'realization_move',
      'realization_ply',
      'realization_match_kind',
      'premises',
      'closure_use_ids',
    ]) ||
    !typedHash(value.path_occurrence_id) ||
    !typedHash(value.reply_branch_id) ||
    !Array.isArray(value.premises) ||
    value.premises.length < 1 ||
    !validWireStrings(value.closure_use_ids, 1, typedHash) ||
    value.closure_use_ids.length !== 1
  )
    return;
  const branch = branches.get(value.reply_branch_id as string);
  const realizationActor = projectTypedActor(value.realization_actor, false, true);
  const realizationMove = uci(value.realization_move);
  const realizationPly = nonNegativeInteger(value.realization_ply);
  const realizationMatchKind =
    value.realization_match_kind === 'exact_move' || value.realization_match_kind === 'equivalent_function'
      ? value.realization_match_kind
      : undefined;
  const premises = value.premises.map(premise => projectPassedPawnResultPremise(premise, branches));
  const realizationStepIndices =
    branch?.steps.flatMap((step, index) =>
      step.move === realizationMove && step.ply === realizationPly ? [index] : [],
    ) ?? [];
  const realizationStep =
    realizationStepIndices.length === 1 ? branch?.steps[realizationStepIndices[0]!] : undefined;
  if (
    !branch?.replyMove ||
    branch.role !== 'legal_reply' ||
    !realizationActor ||
    !realizationMove ||
    realizationPly === undefined ||
    !realizationMatchKind ||
    typedActorMove(realizationActor) !== realizationMove ||
    realizationStepIndices.length !== 1 ||
    realizationActor.side !== fenSideToMove(realizationStep!.fenBefore) ||
    !premises.every((premise): premise is MoveReviewTypedPremise => !!premise) ||
    !validPassedPawnResultPremiseManifest(
      premises,
      branch,
      expectedBranch,
      realizationStepIndices[0]!,
      realizationMove,
      realizationPly,
      realizationMatchKind,
      eventEvidenceId,
      comparisonEvidenceId,
    )
  )
    return;
  return {
    id: value.path_occurrence_id as string,
    branch: branch as MoveReviewPassedPawnResultWireBranch & { replyMove: Uci },
    realizationActor,
    realizationMove,
    realizationPly,
    realizationMatchKind,
    premises,
    closureUseIds: [...value.closure_use_ids],
  };
}

function projectPassedPawnResultPremise(
  value: unknown,
  branches: Map<string, MoveReviewWireBranch>,
): MoveReviewTypedPremise | undefined {
  if (
    !isObject(value) ||
    !hasOnlyKeys(
      value,
      [
        'role',
        'lower_kind',
        'lower_semantic_key',
        'source_premise_ids',
        'branch_id',
        'branch_role',
        'related_branch_ids',
        'from_step_index',
        'to_step_index',
        'dependency_proof',
      ],
      [
        'role',
        'lower_kind',
        'lower_semantic_key',
        'source_premise_ids',
        'branch_id',
        'branch_role',
        'related_branch_ids',
        'from_step_index',
        'to_step_index',
      ],
    ) ||
    !nonEmptyWireString(value.role) ||
    !nonEmptyWireString(value.lower_kind) ||
    !nonEmptyWireString(value.lower_semantic_key) ||
    !typedHash(value.branch_id) ||
    branches.get(value.branch_id as string)?.role !== value.branch_role ||
    !canonicalWireStrings(value.source_premise_ids, 1) ||
    !canonicalWireStrings(value.related_branch_ids, 0, typedHash) ||
    (value.related_branch_ids as string[]).some(
      branchId => branchId === value.branch_id || !branches.has(branchId),
    )
  )
    return;
  const dependencyPremise =
    value.lower_kind === 'passed_pawn_result_dependency' ||
    value.lower_kind === 'observed_passed_pawn_result_dependency';
  const hasDependencyProof = Object.prototype.hasOwnProperty.call(value, 'dependency_proof');
  const dependencyProof = hasDependencyProof
    ? projectPassedPawnResultDependencyProof(value.dependency_proof)
    : undefined;
  if (dependencyPremise !== hasDependencyProof || (hasDependencyProof && !dependencyProof)) return;
  const from = nonNegativeInteger(value.from_step_index);
  const to = nonNegativeInteger(value.to_step_index);
  const branch = branches.get(value.branch_id as string);
  return from === undefined || to === undefined || from > to || !branch?.steps[from] || !branch.steps[to]
    ? undefined
    : {
        role: value.role as string,
        contract: value.lower_kind as string,
        resultId: value.lower_semantic_key as string,
        sourcePremiseIds: [...value.source_premise_ids],
        branchId: value.branch_id as string,
        branchRole: value.branch_role as string,
        relatedBranchIds: [...value.related_branch_ids],
        fromStepIndex: from,
        toStepIndex: to,
        ...(dependencyProof ? { dependencyProof } : {}),
      };
}

function projectPassedPawnResultDependencyProof(
  value: unknown,
): MoveReviewPassedPawnDependencyProof | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['dependency_kind', 'proof_kind', 'squares', 'pieces', 'relation_issuers']) ||
    !Array.isArray(value.squares) ||
    !Array.isArray(value.pieces) ||
    !Array.isArray(value.relation_issuers)
  )
    return;

  const dependencyKind = passedPawnDependencyKind(value.dependency_kind);
  const proofKind = passedPawnDependencyProofKind(value.proof_kind);
  const squares = value.squares.map(square => {
    if (
      !isObject(square) ||
      !hasExactKeys(square, ['role', 'square']) ||
      !nonEmptyWireString(square.role) ||
      !key(square.square)
    )
      return;
    return { role: square.role as string, square: square.square as Key };
  });
  const pieces = value.pieces.map(piece => {
    if (
      !isObject(piece) ||
      !hasExactKeys(piece, ['role', 'side', 'piece']) ||
      !nonEmptyWireString(piece.role) ||
      (piece.side !== 'white' && piece.side !== 'black') ||
      !pieceRole(piece.piece)
    )
      return;
    return {
      role: piece.role as string,
      side: piece.side as 'white' | 'black',
      piece: piece.piece as MoveReviewPieceRole,
    };
  });
  const relationIssuers = value.relation_issuers.map(issuer => {
    if (
      !isObject(issuer) ||
      !hasExactKeys(issuer, [
        'contract',
        'relation_kind',
        'result_key',
        'occurrence_id',
        'step_key',
        'source_premise_ids',
      ])
    )
      return;
    const contract = passedPawnRelationKind(issuer.contract);
    const relationKind = passedPawnRelationKind(issuer.relation_kind);
    if (
      !contract ||
      relationKind !== contract ||
      typeof issuer.result_key !== 'string' ||
      !issuer.result_key.startsWith(`${contract}:`) ||
      !typedHash(issuer.result_key.slice(contract.length + 1)) ||
      !typedHash(issuer.occurrence_id) ||
      !nonEmptyWireString(issuer.step_key) ||
      !canonicalWireStrings(issuer.source_premise_ids, 1)
    )
      return;
    return {
      contract,
      relationKind,
      resultKey: issuer.result_key,
      occurrenceId: issuer.occurrence_id as string,
      stepKey: issuer.step_key as string,
      sourcePremiseIds: [...issuer.source_premise_ids],
    };
  });
  if (
    !dependencyKind ||
    !proofKind ||
    !squares.every((square): square is NonNullable<typeof square> => !!square) ||
    !pieces.every((piece): piece is NonNullable<typeof piece> => !!piece) ||
    !relationIssuers.every((issuer): issuer is NonNullable<typeof issuer> => !!issuer) ||
    !unique(squares.map(square => `${square.role}:${square.square}`)) ||
    !unique(pieces.map(piece => piece.role)) ||
    !validPassedPawnDependencyProofShape(dependencyKind, proofKind, squares, pieces, relationIssuers)
  )
    return;
  return { dependencyKind, proofKind, squares, pieces, relationIssuers };
}

function validPassedPawnDependencyProofShape(
  dependencyKind: MoveReviewPassedPawnDependencyKind,
  proofKind: MoveReviewPassedPawnDependencyProofKind,
  squares: { role: string; square: Key }[],
  pieces: { role: string; side: 'white' | 'black'; piece: MoveReviewPieceRole }[],
  issuers: MoveReviewPassedPawnDependencyProof['relationIssuers'],
): boolean {
  const squareRoles = squares.map(square => square.role);
  const pieceRoles = pieces.map(piece => piece.role);
  const exactRoles = (actual: string[], expected: string[]): boolean =>
    actual.length === expected.length && unique(actual) && expected.every(role => actual.includes(role));
  const exactIssuer = (kind: MoveReviewPassedPawnRelationKind): boolean =>
    issuers.length === 1 && issuers[0]!.contract === kind;
  const piece = (role: string) => pieces.find(witness => witness.role === role);

  switch (proofKind) {
    case 'object_state':
      return (
        dependencyKind === 'object_state_precondition' &&
        exactRoles(squareRoles, ['root_from', 'root_to', 'future_from', 'future_to']) &&
        exactRoles(pieceRoles, ['root_before', 'tracked', 'future_after']) &&
        pieces.every(witness => witness.side === pieces[0]!.side) &&
        issuers.length === 0
      );
    case 'line_access':
      return (
        dependencyKind === 'line_access_precondition' &&
        squareRoles.filter(role => role === 'vacated_gate').length >= 1 &&
        squareRoles.filter(role => role === 'enabled_from').length === 1 &&
        squareRoles.filter(role => role === 'enabled_to').length === 1 &&
        squareRoles.every(
          role => role === 'vacated_gate' || role === 'enabled_from' || role === 'enabled_to',
        ) &&
        exactRoles(pieceRoles, ['enabled_piece']) &&
        exactIssuer('slider_reach_delta')
      );
    case 'pawn_break_follow_up': {
      const trigger = piece('trigger_pawn');
      const responder = piece('responder_pawn');
      const followUp = piece('follow_up_pawn');
      return (
        dependencyKind === 'response_continuation_precondition' &&
        exactRoles(squareRoles, [
          'reply_from',
          'reply_to',
          'follow_up_from',
          'follow_up_to',
          'released_passed_pawn',
        ]) &&
        exactRoles(pieceRoles, ['trigger_pawn', 'responder_pawn', 'follow_up_pawn']) &&
        !!trigger &&
        !!responder &&
        !!followUp &&
        trigger.piece === 'pawn' &&
        responder.piece === 'pawn' &&
        followUp.piece === 'pawn' &&
        trigger.side === followUp.side &&
        trigger.side !== responder.side &&
        exactIssuer('pawn_topology_transition')
      );
    }
    case 'capture_follow_up': {
      const trigger = piece('trigger_piece');
      const responder = piece('responder_piece');
      const followUp = piece('follow_up_piece');
      return (
        dependencyKind === 'response_continuation_precondition' &&
        exactRoles(squareRoles, ['reply_from', 'reply_to', 'follow_up_from', 'follow_up_to']) &&
        exactRoles(pieceRoles, ['trigger_piece', 'responder_piece', 'follow_up_piece']) &&
        !!trigger &&
        !!responder &&
        !!followUp &&
        trigger.side === followUp.side &&
        trigger.side !== responder.side &&
        exactIssuer('capture_recapture_inventory')
      );
    }
  }
}

function passedPawnDependencyKind(value: unknown): MoveReviewPassedPawnDependencyKind | undefined {
  return value === 'object_state_precondition' ||
    value === 'line_access_precondition' ||
    value === 'response_continuation_precondition'
    ? value
    : undefined;
}

function passedPawnDependencyProofKind(value: unknown): MoveReviewPassedPawnDependencyProofKind | undefined {
  return value === 'object_state' ||
    value === 'line_access' ||
    value === 'pawn_break_follow_up' ||
    value === 'capture_follow_up'
    ? value
    : undefined;
}

function passedPawnRelationKind(value: unknown): MoveReviewPassedPawnRelationKind | undefined {
  return value === 'slider_reach_delta' ||
    value === 'pawn_topology_transition' ||
    value === 'capture_recapture_inventory'
    ? value
    : undefined;
}

function validPassedPawnResultPremiseManifest(
  premises: MoveReviewTypedPremise[],
  replyBranch: MoveReviewPassedPawnResultWireBranch & { replyMove?: Uci },
  expectedBranch: MoveReviewPassedPawnResultWireBranch,
  realizationStepIndex: number,
  realizationMove: Uci,
  realizationPly: number,
  realizationMatchKind: 'exact_move' | 'equivalent_function',
  eventEvidenceId: string,
  comparisonEvidenceId: string,
): boolean {
  const replyMove = replyBranch.replyMove;
  const expectedLastIndex = expectedBranch.steps.length - 1;
  const expectedLast = expectedBranch.steps[expectedLastIndex];
  const replyRoot = replyBranch.steps[0];
  const expectedRoot = expectedBranch.steps[0];
  if (!replyMove || !expectedLast || !replyRoot || !expectedRoot) return false;

  const exactMove =
    realizationMove === expectedLast.move &&
    realizationPly - replyRoot.ply === expectedLast.ply - expectedRoot.ply;
  if ((realizationMatchKind === 'exact_move') !== exactMove) return false;

  const exact = (
    premise: MoveReviewTypedPremise | undefined,
    role: string,
    contract: string,
    branch: MoveReviewWireBranch,
    from: number,
    to: number,
    relatedBranchIds: string[],
    requiredOwner: string,
  ): boolean =>
    !!premise &&
    premise.role === role &&
    premise.contract === contract &&
    premise.branchId === branch.id &&
    premise.branchRole === branch.role &&
    premise.fromStepIndex === from &&
    premise.toStepIndex === to &&
    JSON.stringify(premise.relatedBranchIds) === JSON.stringify(relatedBranchIds) &&
    premise.sourcePremiseIds.includes(requiredOwner);

  const sole = (role: string): MoveReviewTypedPremise | undefined => {
    const matches = premises.filter(premise => premise.role === role);
    return matches.length === 1 ? matches[0] : undefined;
  };
  const comparison = sole('comparison_demand');
  const expectedResult = sole('expected_result');
  const observedResult = sole('observed_result');
  const functionalMatch = sole('functional_match');
  if (
    !exact(
      comparison,
      'comparison_demand',
      'played_vs_best_demand',
      expectedBranch,
      0,
      0,
      [],
      comparisonEvidenceId,
    ) ||
    !exact(
      expectedResult,
      'expected_result',
      'passed_pawn_result',
      expectedBranch,
      expectedLastIndex,
      expectedLastIndex,
      [],
      eventEvidenceId,
    ) ||
    !exact(
      observedResult,
      'observed_result',
      'observed_passed_pawn_result',
      replyBranch,
      realizationStepIndex,
      realizationStepIndex,
      [],
      eventEvidenceId,
    ) ||
    !exact(
      functionalMatch,
      'functional_match',
      'passed_pawn_result_functional_match',
      replyBranch,
      realizationStepIndex,
      realizationStepIndex,
      [expectedBranch.id],
      eventEvidenceId,
    ) ||
    !!comparison?.dependencyProof ||
    !!expectedResult?.dependencyProof ||
    !!observedResult?.dependencyProof ||
    !!functionalMatch?.dependencyProof
  )
    return false;

  const ownsDependencyProof = (premise: MoveReviewTypedPremise): boolean =>
    !!premise.dependencyProof &&
    premise.dependencyProof.relationIssuers.every(issuer =>
      [issuer.occurrenceId, ...issuer.sourcePremiseIds].every(source =>
        premise.sourcePremiseIds.includes(source),
      ),
    );
  const bindsDependencyOccurrence = (
    premise: MoveReviewTypedPremise,
    branch: MoveReviewPassedPawnResultWireBranch,
  ): boolean => {
    const proof = premise.dependencyProof;
    const fromIndex = premise.fromStepIndex;
    const toIndex = premise.toStepIndex;
    if (!proof || fromIndex === undefined || toIndex === undefined) return false;
    const issuer = proof.relationIssuers[0];
    const from = passedPawnDependencyActor(branch.steps[fromIndex]);
    const to = passedPawnDependencyActor(branch.steps[toIndex]);
    if (!from || !to || from.side !== to.side) return false;
    const square = (role: string): Key | undefined =>
      proof.squares.find(witness => witness.role === role)?.square;
    const piece = (role: string) => proof.pieces.find(witness => witness.role === role);
    const actorPiece = (role: string, actor: PassedPawnDependencyActor, after = false): boolean => {
      const witness = piece(role);
      const actual = after ? actor.after : actor.before;
      return !!witness && witness.side === actual.side && witness.piece === actual.piece;
    };

    switch (proof.proofKind) {
      case 'object_state':
        return (
          square('root_from') === from.from &&
          square('root_to') === from.to &&
          square('future_from') === to.from &&
          square('future_to') === to.to &&
          from.to === to.from &&
          actorPiece('root_before', from) &&
          actorPiece('tracked', from, true) &&
          actorPiece('tracked', to) &&
          actorPiece('future_after', to, true)
        );
      case 'line_access':
        return (
          issuer?.stepKey === branch.steps[fromIndex]?.stepKey &&
          square('enabled_from') === to.from &&
          square('enabled_to') === to.to &&
          actorPiece('enabled_piece', to) &&
          proof.squares
            .filter(witness => witness.role === 'vacated_gate')
            .every(witness => squareVacatedByStep(branch.steps[fromIndex]!, witness.square))
        );
      case 'pawn_break_follow_up':
      case 'capture_follow_up': {
        const reply = passedPawnDependencyActor(branch.steps[fromIndex + 1]);
        const triggerRole = proof.proofKind === 'pawn_break_follow_up' ? 'trigger_pawn' : 'trigger_piece';
        const responderRole =
          proof.proofKind === 'pawn_break_follow_up' ? 'responder_pawn' : 'responder_piece';
        const followUpRole =
          proof.proofKind === 'pawn_break_follow_up' ? 'follow_up_pawn' : 'follow_up_piece';
        return (
          !!reply &&
          issuer?.stepKey === branch.steps[fromIndex + 1]?.stepKey &&
          toIndex > fromIndex + 1 &&
          (proof.proofKind !== 'capture_follow_up' || toIndex === fromIndex + 2) &&
          square('reply_from') === reply.from &&
          square('reply_to') === reply.to &&
          reply.side !== from.side &&
          square('follow_up_from') === to.from &&
          square('follow_up_to') === to.to &&
          (proof.proofKind !== 'pawn_break_follow_up' || square('released_passed_pawn') === to.from) &&
          actorPiece(triggerRole, from) &&
          actorPiece(responderRole, reply) &&
          actorPiece(followUpRole, to)
        );
      }
    }
  };
  const expectedDependencies = premises.filter(premise => premise.role === 'expected_dependency');
  const observedDependencies = premises.filter(premise => premise.role === 'observed_dependency');
  if (
    expectedDependencies.length !== expectedLastIndex ||
    expectedDependencies.length < 1 ||
    observedDependencies.length < 1 ||
    expectedDependencies.some(
      (premise, index) =>
        !exact(
          premise,
          'expected_dependency',
          'passed_pawn_result_dependency',
          expectedBranch,
          index,
          index + 1,
          [],
          eventEvidenceId,
        ) ||
        !ownsDependencyProof(premise) ||
        !bindsDependencyOccurrence(premise, expectedBranch) ||
        premise.resultId !== expectedBranch.steps[index + 1]!.incomingLink?.occurrenceLinkKey,
    ) ||
    observedDependencies.some(
      (premise, position) =>
        premise.contract !== 'observed_passed_pawn_result_dependency' ||
        premise.branchId !== replyBranch.id ||
        premise.branchRole !== replyBranch.role ||
        premise.relatedBranchIds?.length !== 0 ||
        !ownsDependencyProof(premise) ||
        !bindsDependencyOccurrence(premise, replyBranch) ||
        !premise.sourcePremiseIds.includes(eventEvidenceId) ||
        premise.fromStepIndex === undefined ||
        premise.toStepIndex === undefined ||
        premise.fromStepIndex >= premise.toStepIndex ||
        (position === 0
          ? premise.fromStepIndex !== 0
          : premise.fromStepIndex !== observedDependencies[position - 1]!.toStepIndex),
    ) ||
    observedDependencies[observedDependencies.length - 1]!.toStepIndex !== realizationStepIndex
  )
    return false;

  const exactOrder = [
    comparison!,
    ...expectedDependencies,
    expectedResult!,
    ...observedDependencies,
    observedResult!,
    functionalMatch!,
  ];
  return (
    premises.length === exactOrder.length && premises.every((premise, index) => premise === exactOrder[index])
  );
}

interface PassedPawnDependencyActor {
  from: Key;
  to: Key;
  side: 'white' | 'black';
  before: { side: 'white' | 'black'; piece: MoveReviewPieceRole };
  after: { side: 'white' | 'black'; piece: MoveReviewPieceRole };
}

function passedPawnDependencyActor(
  step: MoveReviewWireStep | undefined,
): PassedPawnDependencyActor | undefined {
  if (!step) return;
  const from = step.move.slice(0, 2) as Key;
  const to = step.move.slice(2, 4) as Key;
  const side = fenSideToMove(step.fenBefore);
  const before = fenPieceAt(step.fenBefore, from);
  const after = fenPieceAt(step.fenAfter, to);
  return side && before?.side === side && after?.side === side
    ? { from, to, side, before, after }
    : undefined;
}

function squareVacatedByStep(step: MoveReviewWireStep, square: Key): boolean {
  return !!fenPieceAt(step.fenBefore, square) && !fenPieceAt(step.fenAfter, square);
}

function fenPieceAt(
  fen: FEN,
  square: Key,
): { side: 'white' | 'black'; piece: MoveReviewPieceRole } | undefined {
  try {
    const parsed = parseSquare(square);
    const piece = parsed === undefined ? undefined : parseFen(fen).unwrap().board.get(parsed);
    return piece ? { side: piece.color, piece: piece.role } : undefined;
  } catch (_) {
    return;
  }
}

function typedActorMove(actor: MoveReviewTypedActor): Uci | undefined {
  if (actor.pieceBefore !== 'pawn')
    return actor.pieceAfter === actor.pieceBefore ? uci(`${actor.from}${actor.to}`) : undefined;
  if (actor.pieceAfter === 'pawn') return uci(`${actor.from}${actor.to}`);
  const promotion: Partial<Record<MoveReviewPieceRole, string>> = {
    queen: 'q',
    rook: 'r',
    bishop: 'b',
    knight: 'n',
  };
  const suffix = promotion[actor.pieceAfter];
  return suffix ? uci(`${actor.from}${actor.to}${suffix}`) : undefined;
}

function passedPawnResultTransitionScope(role: MoveReviewTypedLine['role']): string {
  switch (role) {
    case 'played':
      return 'played_transition';
    case 'best_reference':
      return 'reference_transition';
    case 'alternative':
      return 'alternative_transition';
    case 'branch_reply':
      return 'branch_reply_line';
  }
}

function isPassedPawnResultWireStep(step: MoveReviewWireStep): step is MoveReviewPassedPawnResultWireStep {
  return !!step.stepKey && !!step.line;
}

function samePassedPawnResultLine(
  left: MoveReviewTypedLine | undefined,
  right: MoveReviewTypedLine | undefined,
): boolean {
  return (
    !!left &&
    !!right &&
    left.id === right.id &&
    left.role === right.role &&
    left.rank === right.rank &&
    left.rootMove === right.rootMove
  );
}

function causalStepKey(ply: number, move: Uci, fenBefore: FEN, fenAfter: FEN): string {
  const normalizeFen = (fen: FEN): string => fen.trim().split(/\s+/).filter(Boolean).join(' ');
  return `${ply}:${move}:${normalizeFen(fenBefore)}:${normalizeFen(fenAfter)}`;
}

function projectWireStep(value: unknown, withOccurrenceProof: boolean): MoveReviewWireStep | undefined {
  const required = ['step_index', 'ply', 'move_uci', 'fen_before', 'fen_after', 'provenance'];
  if (
    !isObject(value) ||
    !(withOccurrenceProof
      ? hasOnlyKeys(
          value,
          [...required, 'step_key', 'line', 'incoming_link'],
          [...required, 'step_key', 'line'],
        )
      : hasExactKeys(value, ['step_index', 'provenance', 'ply', 'move_uci', 'fen_before', 'fen_after'])) ||
    (value.provenance !== 'observed_game_move' && value.provenance !== 'certified_analysis_move')
  )
    return;
  const index = nonNegativeInteger(value.step_index);
  const stepKey = withOccurrenceProof ? nonEmptyWireString(value.step_key) : undefined;
  const ply = nonNegativeInteger(value.ply);
  const move = uci(value.move_uci);
  const fenBefore = fenText(value.fen_before);
  const fenAfter = fenText(value.fen_after);
  const line = withOccurrenceProof ? projectPassedPawnResultLine(value.line) : undefined;
  if (
    index === undefined ||
    ply === undefined ||
    !move ||
    !fenBefore ||
    !fenAfter ||
    (withOccurrenceProof && (!stepKey || !line || stepKey !== causalStepKey(ply, move, fenBefore, fenAfter)))
  )
    return;
  if (withOccurrenceProof) {
    const hasLink = Object.prototype.hasOwnProperty.call(value, 'incoming_link');
    if ((index === 0 && hasLink) || (index > 0 && !validCausalOccurrenceLink(value.incoming_link))) return;
  }
  const incomingLink =
    withOccurrenceProof && index > 0 && isObject(value.incoming_link)
      ? {
          kind: value.incoming_link.kind as 'adjacent_legal_replay' | 'certified_causal_dependency',
          fromStepKey: value.incoming_link.from_step_key as string,
          toStepKey: value.incoming_link.to_step_key as string,
          occurrenceLinkKey: value.incoming_link.occurrence_link_key as string,
        }
      : undefined;
  return {
    index,
    ply,
    move,
    fenBefore,
    fenAfter,
    provenance: value.provenance,
    ...(stepKey ? { stepKey } : {}),
    ...(line ? { line } : {}),
    ...(incomingLink ? { incomingLink } : {}),
  };
}

function projectPassedPawnResultLine(value: unknown): MoveReviewTypedLine | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['line_id', 'line_role', 'line_rank', 'root_move']) ||
    !nonEmptyWireString(value.line_id) ||
    (value.line_role === 'played' ||
      value.line_role === 'best_reference' ||
      value.line_role === 'alternative' ||
      value.line_role === 'branch_reply') === false ||
    integerInRange(value.line_rank, 1, Number.MAX_SAFE_INTEGER) === undefined ||
    !uci(value.root_move)
  )
    return;
  return {
    id: value.line_id as string,
    role: value.line_role as MoveReviewTypedLine['role'],
    rank: value.line_rank as number,
    rootMove: value.root_move as Uci,
  };
}

function validCausalOccurrenceLink(value: unknown): boolean {
  return (
    isObject(value) &&
    hasExactKeys(value, ['kind', 'from_step_key', 'to_step_key', 'occurrence_link_key']) &&
    (value.kind === 'adjacent_legal_replay' || value.kind === 'certified_causal_dependency') &&
    !!nonEmptyWireString(value.from_step_key) &&
    !!nonEmptyWireString(value.to_step_key) &&
    !!nonEmptyWireString(value.occurrence_link_key)
  );
}

function proofFromWireSteps(
  id: string,
  startFen: FEN,
  steps: MoveReviewWireStep[],
): MoveReviewProof | undefined {
  if (!semanticId(id) || !wireStepsAreContinuous(steps, startFen) || steps.length > 80) return;
  return {
    id,
    startFen,
    moves: steps.map(step => ({ uci: step.move, label: step.move, fenAfter: step.fenAfter })),
    annotations: [],
  };
}

function wireStepsAreContinuous(steps: MoveReviewWireStep[], startFen: FEN): boolean {
  return (
    wireStepsHaveOrderedOccurrences(steps, startFen) &&
    steps.every(
      (step, index) =>
        index === 0 ||
        (step.fenBefore === steps[index - 1]!.fenAfter && step.ply === steps[index - 1]!.ply + 1),
    )
  );
}

function wireStepsHaveOrderedOccurrences(steps: MoveReviewWireStep[], startFen: FEN): boolean {
  return (
    steps.length > 0 &&
    steps[0]!.fenBefore === startFen &&
    steps.every((step, index) => step.index === index && (index === 0 || step.ply > steps[index - 1]!.ply))
  );
}

function wireBranchIdentity(branch: MoveReviewWireBranch): MoveReviewTypedBranch {
  return {
    id: branch.id,
    role: branch.role,
    provenance: branch.provenance,
    lineId: branch.lineId,
    lineRole: branch.lineRole,
    lineRank: branch.lineRank,
    rootMove: branch.rootMove,
    ...(branch.sourceProbeId ? { sourceProbeId: branch.sourceProbeId } : {}),
    steps: [...branch.steps],
  };
}

function typedHash(value: unknown): string | undefined {
  return typeof value === 'string' && sha256Pattern.test(value) ? value : undefined;
}

function validWireStrings(
  value: unknown,
  minimum: number,
  validator: (item: unknown) => string | undefined = nonEmptyWireString,
): value is string[] {
  return (
    Array.isArray(value) && value.length >= minimum && value.every(item => !!validator(item)) && unique(value)
  );
}

function projectChessObjects(value: unknown[]): string[] | undefined {
  const projected = value.map(item =>
    isObject(item) &&
    hasExactKeys(item, ['kind', 'key']) &&
    chessObjectKind(item.kind) &&
    nonEmptyWireString(item.key)
      ? `${item.kind}:${item.key}`
      : undefined,
  );
  return projected.every((item): item is string => !!item) ? projected : undefined;
}

function buildProof(
  id: string,
  startFen: FEN,
  moves: readonly Uci[],
  annotations: MoveReviewAnnotation[],
): MoveReviewProof | undefined {
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
    if (annotations.some(annotation => annotation.atPly < 1 || annotation.atPly > proofMoves.length)) return;
    return { id, startFen, moves: proofMoves, annotations };
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

function projectCandidateSet(value: unknown): MoveReviewCandidateSetType | undefined {
  return isObject(value) && hasExactKeys(value, ['type']) && candidateSetType(value.type)
    ? value.type
    : undefined;
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

function directCausalChange(value: unknown): value is MoveReviewDirectCausalChange {
  return value === 'occurred' || value === 'maintained' || value === 'lost';
}

function playerFacingCausalChange(value: unknown): value is MoveReviewPlayerFacingCausalChange {
  return directCausalChange(value) || value === 'missed';
}

function terminalRelation(
  value: unknown,
): value is 'produces_line_consequence' | 'is_root_line_event' | 'instantiates_relation' {
  return (
    value === 'produces_line_consequence' ||
    value === 'is_root_line_event' ||
    value === 'instantiates_relation'
  );
}

function proofStepRole(value: unknown): value is 'root_action' | 'causal_link' | 'terminal_event' {
  return value === 'root_action' || value === 'causal_link' || value === 'terminal_event';
}

function chessObjectKind(value: unknown): value is string {
  return (
    value === 'move' ||
    value === 'piece' ||
    value === 'side' ||
    value === 'square' ||
    value === 'file' ||
    value === 'relation' ||
    value === 'line' ||
    value === 'mechanism' ||
    value === 'consequence'
  );
}

function candidateSetType(value: unknown): value is MoveReviewCandidateSetType {
  return value === 'only_move' || value === 'narrow_choice' || value === 'style_choice';
}

function standardCauseKind(value: unknown): value is string {
  return (
    value === 'missed_tactical_resource' ||
    value === 'tactical_refutation_of_played' ||
    value === 'candidate_tactical_liability' ||
    value === 'recapture_recovery_window' ||
    value === 'draw_resource' ||
    value === 'king_forcing' ||
    value === 'material_swing'
  );
}

function standardProofConfidence(value: unknown): boolean {
  return (
    value === 'legal_replay_verified' ||
    value === 'engine_backed' ||
    value === 'board_derived' ||
    value === 'mixed'
  );
}

function standardSourceSide(value: unknown): boolean {
  return value === 'reference' || value === 'candidate' || value === 'shared' || value === 'mixed';
}

function standardComparisonKind(value: unknown): boolean {
  return (
    value === 'played_vs_best' ||
    value === 'best_vs_second' ||
    value === 'played_vs_alternative' ||
    value === 'reference_vs_alternative'
  );
}

function nonEmptyWireString(value: unknown): string | undefined {
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

function key(value: unknown): Key | undefined {
  return typeof value === 'string' && squarePattern.test(value) ? (value as Key) : undefined;
}

function fenSideToMove(fen: FEN): 'white' | 'black' | undefined {
  const activeColor = fen.trim().split(/\s+/)[1];
  return activeColor === 'w' ? 'white' : activeColor === 'b' ? 'black' : undefined;
}

function pieceRole(value: unknown): MoveReviewPieceRole | undefined {
  return value === 'pawn' ||
    value === 'knight' ||
    value === 'bishop' ||
    value === 'rook' ||
    value === 'queen' ||
    value === 'king'
    ? value
    : undefined;
}

function passedPawnResultSubject(value: unknown): string | undefined {
  return typeof value === 'string' && passedPawnResultSubjectPattern.test(value) ? value : undefined;
}

function validUciMoves(value: unknown, minimum: number, maximum = Number.MAX_SAFE_INTEGER): value is Uci[] {
  return (
    Array.isArray(value) &&
    value.length >= minimum &&
    value.length <= maximum &&
    value.every(move => !!uci(move))
  );
}

function canonicalStrings(values: readonly string[]): boolean {
  return unique(values) && values.every((value, index) => index === 0 || values[index - 1]! <= value);
}

function canonicalWireStrings(
  value: unknown,
  minimum: number,
  validator: (item: unknown) => string | undefined = nonEmptyWireString,
): value is string[] {
  return validWireStrings(value, minimum, validator) && canonicalStrings(value);
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
const passedPawnResultSubjectPattern =
  /^(19:passed-pawn-created5:(white|black)2:[a-h][1-8]|20:passed-pawn-advanced5:(white|black)2:[a-h][1-8]2:[a-h][1-8]1:[1-8]|21:passed-status-created5:(white|black)2:[a-h][1-8]2:[a-h][1-8]1:[1-8]|20:passed-pawn-promoted5:(white|black)2:[a-h][1-8]2:[a-h][1-8]):relations:\[(established|removed):pawn_passage:[0-9a-f]{64}(,(established|removed):pawn_passage:[0-9a-f]{64})*\]:derived:\[\]$/;
const squarePattern = /^[a-h][1-8]$/;
const uciPattern = /^[a-h][1-8][a-h][1-8][qrbn]?$/;
