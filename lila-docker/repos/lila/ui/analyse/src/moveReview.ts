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
export type MoveReviewReasonRole = 'primary' | 'support' | 'proof-route';
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
  primary: string[];
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
  role: 'counterfactual_reference' | 'played_root_analysis_continuation';
  provenance: 'counterfactual_analyzed_root' | 'observed_game_root';
  lineId: string;
  lineRole: 'played' | 'best_reference' | 'alternative';
  lineRank: number;
  rootMove: Uci;
  sourceOccurrenceId?: string;
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

type MoveReviewPassedPawnLineScope = 'played_line';

type MoveReviewPassedPawnPositionState =
  | { kind: 'occupied_by'; side: 'white' | 'black'; square: Key; piece: MoveReviewPieceRole }
  | {
      kind: 'slider_reach';
      side: 'white' | 'black';
      square: Key;
      piece: 'bishop' | 'rook' | 'queen';
      fileStep: number;
      rankStep: number;
      segment: { square: Key; target: 'empty' | 'friendly' | 'enemy'; occupantPiece?: MoveReviewPieceRole }[];
    }
  | { kind: 'pawn_topology'; side: 'white' | 'black'; square: Key; passed: boolean };

interface MoveReviewPassedPawnDependencyProof {
  dependencyKind: MoveReviewPassedPawnDependencyKind;
  proofKind: MoveReviewPassedPawnDependencyProofKind;
  squares: { role: string; square: Key }[];
  pieces: { role: string; side: 'white' | 'black'; piece: MoveReviewPieceRole }[];
  relationIssuers: {
    contract: MoveReviewPassedPawnRelationKind;
    resultKey: string;
    occurrenceId: string;
    stepKey: string;
    sourcePremiseIds: string[];
    issuerEvidenceId: string;
    line: MoveReviewTypedLine & { role: 'played' };
    scope: MoveReviewPassedPawnLineScope;
  }[];
  positionStateIssuers: {
    state: MoveReviewPassedPawnPositionState;
    semanticProofId: string;
    issuerEvidenceId: string;
    issuerOccurrenceId: string;
    stepKey: string;
    ply: number;
    move: Uci;
    fenBefore: FEN;
    fenAfter: FEN;
    line: MoveReviewTypedLine & { role: 'played' };
    scope: MoveReviewPassedPawnLineScope;
  }[];
}

interface MoveReviewTypedPremise {
  role: string;
  contract: string;
  resultId: string;
  sourcePremiseIds: string[];
  issuerEvidenceId?: string;
  issuerOccurrenceId?: string;
  branchId: string;
  branchRole: string;
  stepIndex?: number;
  relatedBranchIds?: string[];
  fromStepIndex?: number;
  toStepIndex?: number;
  dependencyProof?: MoveReviewPassedPawnDependencyProof;
}

interface MoveReviewLegalMovePremise {
  role: string;
  contract: 'legal_move';
  moveUci: Uci;
  movement: MoveReviewTypedActor;
  movementMode: 'controlled_destination' | 'pawn_advance' | 'pawn_double_advance' | 'castling';
  legalMoveSemanticId: string;
  capture?: { square: Key; piece: MoveReviewPieceRole; side: 'white' | 'black' };
  issuerEvidenceId: string;
  issuerOccurrenceId: string;
  sourcePremiseIds: string[];
  branchId: string;
  branchRole: 'counterfactual_reference' | 'played_root_analysis_continuation';
  stepIndex: number;
}

type MoveReviewCaptureExclusionMoveOrderPremises = [
  MoveReviewLegalMovePremise & {
    role: 'reference_vacating_move';
    branchRole: 'counterfactual_reference';
    stepIndex: 0;
  },
  MoveReviewLegalMovePremise & {
    role: 'played_deferred_move';
    branchRole: 'played_root_analysis_continuation';
    stepIndex: 0;
  },
  MoveReviewLegalMovePremise & {
    role: 'played_capture_reply';
    branchRole: 'played_root_analysis_continuation';
    stepIndex: 1;
    capture: NonNullable<MoveReviewLegalMovePremise['capture']>;
  },
  MoveReviewLegalMovePremise & {
    role: 'reference_deferred_move';
    branchRole: 'counterfactual_reference';
  },
];

interface MoveReviewPieceWitness {
  piece: MoveReviewPieceRole;
  square: Key;
}

interface MoveReviewSquareReleaseRouteStep extends MoveReviewTypedActor {
  moveUci: Uci;
  stepIndex: number;
}

interface MoveReviewSquareReleaseRouteResource extends MoveReviewTypedActor {
  moveUci: Uci;
  capture?: { square: Key; piece: MoveReviewPieceRole; side: 'white' | 'black' };
}

type MoveReviewSquareReleaseRouteTerminal =
  | { kind: 'occupation' }
  | {
      kind: 'capture';
      assertionId: string;
      capturedTarget: { square: Key; piece: MoveReviewPieceRole; side: 'white' | 'black' };
      geometricRecapturers: MoveReviewPieceWitness[];
      legalRecaptures: MoveReviewSquareReleaseRouteResource[];
      restrictedRecaptures: {
        piece: MoveReviewPieceWitness;
        destination: Key;
        kingSquare: Key;
        postMoveControllers: MoveReviewPieceWitness[];
      }[];
    }
  | {
      kind: 'created-check';
      assertionId: string;
      checkedSide: 'white' | 'black';
      kingSquare: Key;
      checkers: MoveReviewPieceWitness[];
      responses: {
        resource: MoveReviewSquareReleaseRouteResource;
        modes: ('capture_checker' | 'interpose' | 'king_move')[];
      }[];
      controlledKingDestinations: { destination: Key; controllers: MoveReviewPieceWitness[] }[];
      terminalState: 'ongoing' | 'checkmate';
    };

type MoveReviewSquareReleaseRoutePremise = MoveReviewTypedPremise | MoveReviewLegalMovePremise;

interface MoveReviewTypedLine {
  id: string;
  role: 'played' | 'best_reference' | 'alternative';
  rank: number;
  rootMove: Uci;
}

export type MoveReviewReasonMessage =
  | { kind: 'line'; moves: Uci[] }
  | {
      kind: 'unique-check-reply-defender-displacement-before-capture';
      channelId: string;
      causeEvidenceId: string;
      causeKind: 'wrong_move_order';
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
    }
  | {
      kind: 'sole-recapturer-removal-before-target-capture';
      channelId: string;
      causeEvidenceId: string;
      causeKind: 'wrong_move_order';
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
      kind: 'capture-exclusion-move-order';
      channelId: string;
      causeEvidenceId: string;
      causeKind: 'wrong_move_order';
      sourceEvidenceId: string;
      semanticId: string;
      occurrenceId: string;
      dependencyFingerprint: string;
      pathOccurrenceId: string;
      branch: MoveReviewTypedBranch;
      counterpart: MoveReviewTypedBranch;
      premises: MoveReviewCaptureExclusionMoveOrderPremises;
      absences: MoveReviewCausalClosureUse[];
      states: MoveReviewCausalClosureUse[];
    }
  | {
      kind: 'vacated-gate-enables-unrecapturable-slider-capture';
      channelId: string;
      causeEvidenceId: string;
      causeKind: 'missed_tactical_resource';
      sourceEvidenceId: string;
      semanticId: string;
      occurrenceId: string;
      dependencyFingerprint: string;
      pathOccurrenceId: string;
      branch: MoveReviewTypedBranch;
      counterpart: MoveReviewTypedBranch;
      enabler: MoveReviewTypedActor;
      slider: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      gateBlocker: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      exploit: MoveReviewTypedActor;
      capturedTarget: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      exploitMove: Uci;
      premises: MoveReviewTypedPremise[];
      absences: MoveReviewCausalClosureUse[];
      states: MoveReviewCausalClosureUse[];
    }
  | {
      kind: 'square-release-route';
      channelId: string;
      causeEvidenceId: string;
      causeKind: 'missed_square_release';
      sourceEvidenceId: string;
      semanticId: string;
      occurrenceId: string;
      dependencyFingerprint: string;
      pathOccurrenceId: string;
      branch: MoveReviewTypedBranch;
      counterpart: MoveReviewTypedBranch;
      releaser: MoveReviewTypedActor;
      releasedBlocker: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      routePiece: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      route: MoveReviewSquareReleaseRouteStep[];
      terminalStepIndex: number;
      terminal: MoveReviewSquareReleaseRouteTerminal;
      terminalReplyMove?: Uci;
      premises: MoveReviewSquareReleaseRoutePremise[];
      absences: MoveReviewCausalClosureUse[];
      states: MoveReviewCausalClosureUse[];
    }
  | {
      kind: 'passed-pawn-progress-realized-after-only-legal-reply';
      channelId: string;
      causeEvidenceId: string;
      causeKind: 'passed_pawn_progress';
      sourceEvidenceId: string;
      eventEvidenceId: string;
      semanticId: string;
      occurrenceId: string;
      dependencyFingerprint: string;
      pathOccurrenceId: string;
      resultTargetSubjects: string[];
      rootActor: MoveReviewTypedActor;
      realizingActor: MoveReviewTypedActor;
      rootLine: MoveReviewTypedLine & { role: 'played' };
      rootMove: Uci;
      rootPly: number;
      replyMove: Uci;
      realizingMove: Uci;
      realizingPly: number;
      resultPlyOffset: number;
      pathRealizationActor: MoveReviewTypedActor;
      pathRealizationMove: Uci;
      pathRealizationPly: number;
      analysisContinuationBranch: MoveReviewTypedBranch & {
        role: 'played_root_analysis_continuation';
        provenance: 'observed_game_root';
        lineRole: 'played';
      };
      analysisContinuationSteps: MoveReviewPassedPawnProgressWireStep[];
      premises: MoveReviewTypedPremise[];
      closureUseIds: string[];
      lowerPremiseIds: string[];
      replyClosure: {
        issuerEvidenceId: string;
        rootAfter: { fen: FEN; ply: number; scope: 'played_transition' };
        legalReplyMove: Uci;
        analysisContinuationBranchId: string;
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
  if (message.kind === 'unique-check-reply-defender-displacement-before-capture') {
    const premiseLabels: Record<MoveReviewTypedPremise['role'], [string, string]> = {
      created_check_response: ['강제 체크 응수', 'forced check reply'],
      reference_capture_recapture: ['기준 재포획', 'reference recapture'],
      played_capture_recapture: ['실전 재포획', 'played recapture'],
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
          ? `반사실 기준 분석: ${referenceSummary}. 관측된 실전 첫 수 이후 인증 분석 후속 수순에서는 ${playedSummary}`
          : `counterfactual reference analysis: ${referenceSummary}. From the observed played root, the certified analysis continuation shows ${playedSummary}`
        : locale === 'ko-KR'
          ? `관측된 실전 첫 수 이후 인증 분석 후속 수순: ${playedSummary}. 반사실 기준 분석에서는 ${referenceSummary}`
          : `from the observed played root, the certified analysis continuation shows ${playedSummary}. In the counterfactual reference analysis, ${referenceSummary}`;
    return locale === 'ko-KR'
      ? `[${message.causeKind}] ${occurrenceComparison}. 경로 ${message.pathOccurrenceId.slice(0, 8)}는 ${absenceIssuer}가 ${message.absence.fen} (ply ${message.absence.ply})에서 인증한 부재 ${message.absence.query}와 하위 증거 ${premises}를 대조합니다${disabled}${mechanism}.`
      : `[${message.causeKind}] ${occurrenceComparison}. Path ${message.pathOccurrenceId.slice(0, 8)} contrasts absence ${message.absence.query}, certified by the ${absenceIssuer} at ${message.absence.fen} (ply ${message.absence.ply}), with lower proofs ${premises}${disabled}${mechanism}.`;
  }
  if (message.kind === 'sole-recapturer-removal-before-target-capture') {
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
        ? `반사실 기준 분석에서 ${message.remover.from}의 ${message.remover.pieceBefore}가 ${message.removedDefender.square}의 ${message.removedDefender.piece}을 잡고, ${message.removalRecapture.moveUci}로 재포획된 뒤 ${message.laterExploitMove}가 ${message.capturedTarget.square}의 ${message.capturedTarget.piece}을 잡습니다`
        : `in the counterfactual reference analysis, the ${message.remover.pieceBefore} from ${message.remover.from} captures the ${message.removedDefender.piece} on ${message.removedDefender.square}, is recaptured by ${message.removalRecapture.moveUci}, and ${message.laterExploitMove} then captures the ${message.capturedTarget.piece} on ${message.capturedTarget.square}`;
    const played =
      locale === 'ko-KR'
        ? `관측된 실전 첫 수 이후 인증 분석 후속 수순에서는 같은 ${message.laterExploitMove}에 ${message.removedDefender.square}의 ${message.removedDefender.piece}이 ${message.playedSoleRecaptureMove}로 재포획합니다`
        : `from the observed played root, the certified analysis continuation shows the same ${message.laterExploitMove} recaptured by the ${message.removedDefender.piece} from ${message.removedDefender.square} with ${message.playedSoleRecaptureMove}`;
    const occurrenceComparison =
      message.branch.role === 'counterfactual_reference'
        ? `${reference}; ${played}`
        : `${played}; ${reference}`;
    return locale === 'ko-KR'
      ? `[${message.causeKind}] ${occurrenceComparison}. 경로 ${message.pathOccurrenceId.slice(0, 8)}는 ${message.absence.issuerEvidenceId}가 ${message.absence.fen} (ply ${message.absence.ply})에서 발급한 ${message.absence.query} 부재와 하위 증거 ${premises}를 보존합니다.`
      : `[${message.causeKind}] ${occurrenceComparison}. Path ${message.pathOccurrenceId.slice(0, 8)} retains absence ${message.absence.query}, issued by ${message.absence.issuerEvidenceId} at ${message.absence.fen} (ply ${message.absence.ply}), and lower proofs ${premises}.`;
  }
  if (message.kind === 'capture-exclusion-move-order') {
    const [vacatingMove, deferredMove, captureReply] = message.premises;
    const capturedTarget = captureReply.capture;
    const reference =
      locale === 'ko-KR'
        ? `반사실 기준 분석에서 ${vacatingMove.moveUci}가 ${capturedTarget.square}의 ${capturedTarget.piece}을 먼저 옮긴 뒤 ${deferredMove.moveUci}가 나중에 실행됩니다`
        : `in the counterfactual reference analysis, ${vacatingMove.moveUci} first moves the ${capturedTarget.piece} from ${capturedTarget.square}, and ${deferredMove.moveUci} is played later`;
    const played =
      locale === 'ko-KR'
        ? `관측된 실전 첫 수 ${deferredMove.moveUci} 뒤 인증 분석 응수 ${captureReply.moveUci}가 그 기물을 ${capturedTarget.square}에서 잡습니다`
        : `after the observed root ${deferredMove.moveUci}, the certified analysis reply ${captureReply.moveUci} captures that piece on ${capturedTarget.square}`;
    const occurrenceComparison =
      message.branch.role === 'counterfactual_reference'
        ? `${reference}; ${played}`
        : `${played}; ${reference}`;
    return locale === 'ko-KR'
      ? `[${message.causeKind}] ${occurrenceComparison}. 경로 ${message.pathOccurrenceId.slice(0, 8)}는 A 직후와 later-B 직후의 동일 포획 응수 부재 ${message.absences.length}개와 대상·포획자·deferred mover의 폐쇄 상태 ${message.states.length}개를 보존합니다.`
      : `[${message.causeKind}] ${occurrenceComparison}. Path ${message.pathOccurrenceId.slice(0, 8)} retains ${message.absences.length} endpoint absences for the same capture reply and ${message.states.length} closed target, capturer, and deferred-mover states.`;
  }
  if (message.kind === 'vacated-gate-enables-unrecapturable-slider-capture') {
    const reference =
      locale === 'ko-KR'
        ? `반사실 기준 분석에서 ${message.enabler.from}의 ${message.enabler.pieceBefore}가 게이트를 비운 뒤 ${message.exploitMove}로 ${message.capturedTarget.square}의 ${message.capturedTarget.piece}을 잡습니다`
        : `in the counterfactual reference analysis, the ${message.enabler.pieceBefore} vacates ${message.enabler.from}, and ${message.exploitMove} then captures the ${message.capturedTarget.piece} on ${message.capturedTarget.square}`;
    const played =
      locale === 'ko-KR'
        ? `관측된 실전 첫 수 이후 인증 분석 후속 수순에서는 ${message.gateBlocker.square}의 ${message.gateBlocker.piece}이 남아 같은 ${message.exploitMove} 자원이 없습니다`
        : `from the observed played root, the certified analysis continuation keeps the ${message.gateBlocker.piece} on ${message.gateBlocker.square}, so the same ${message.exploitMove} resource is absent`;
    const occurrenceComparison =
      message.branch.role === 'counterfactual_reference'
        ? `${reference}; ${played}`
        : `${played}; ${reference}`;
    return locale === 'ko-KR'
      ? `[${message.causeKind}] ${occurrenceComparison}. 경로 ${message.pathOccurrenceId.slice(0, 8)}는 관계 전제 ${message.premises.length}개, 폐쇄 부재 ${message.absences.length}개, 폐쇄 상태 ${message.states.length}개를 정확한 발생 위치에 보존합니다.`
      : `[${message.causeKind}] ${occurrenceComparison}. Path ${message.pathOccurrenceId.slice(0, 8)} retains ${message.premises.length} relation premises, ${message.absences.length} closed absences, and ${message.states.length} closed states at their exact occurrences.`;
  }
  if (message.kind === 'square-release-route') {
    const routeMoves = message.route.map(step => step.moveUci).join(' → ');
    const terminal =
      message.terminal.kind === 'occupation'
        ? locale === 'ko-KR'
          ? `해방된 ${message.releaser.from} 점유로 끝납니다`
          : `ends by occupying the released square ${message.releaser.from}`
        : message.terminal.kind === 'capture'
          ? locale === 'ko-KR'
            ? `보존 수순 인덱스 ${message.terminalStepIndex}의 ${message.terminal.capturedTarget.square} 포획으로 끝나며, 인증된 합법 재포획 자원은 ${message.terminal.legalRecaptures.length}개입니다`
            : `ends with the capture on ${message.terminal.capturedTarget.square} at retained step index ${message.terminalStepIndex}, with ${message.terminal.legalRecaptures.length} certified legal recapture resources`
          : locale === 'ko-KR'
            ? `보존 수순 인덱스 ${message.terminalStepIndex}에서 체크를 만들며, 인증된 합법 응수는 ${message.terminal.responses.length}개이고 상태는 ${message.terminal.terminalState}입니다`
            : `creates check at retained step index ${message.terminalStepIndex}, with ${message.terminal.responses.length} certified legal responses and terminal state ${message.terminal.terminalState}`;
    const reply = message.terminalReplyMove
      ? locale === 'ko-KR'
        ? `; 기준 가지의 다음 실제 응수는 ${message.terminalReplyMove}입니다`
        : `; the reference branch then records the actual reply ${message.terminalReplyMove}`
      : '';
    const reference =
      locale === 'ko-KR'
        ? `반사실 기준 분석에서 ${message.releaser.from}의 ${message.releaser.pieceBefore}가 ${message.releaser.to}로 이동해 ${message.releaser.from}을 비우고, 동일한 ${message.routePiece.piece}의 인증 경로 ${routeMoves}가 이어져 ${terminal}${reply}`
        : `in the counterfactual reference analysis, the ${message.releaser.pieceBefore} moves ${message.releaser.from}–${message.releaser.to}, vacating ${message.releaser.from}, and the certified same-piece route ${routeMoves} ${terminal}${reply}`;
    const played =
      locale === 'ko-KR'
        ? `관측된 실전 첫 수 이후 인증 분석 후속 수순에서는 ${message.releasedBlocker.square}의 ${message.releasedBlocker.piece}과 ${message.routePiece.square}의 ${message.routePiece.piece}이 첫 경로 수 직전까지 계속 남고, 정확한 첫 경로 수 ${message.route[0]!.moveUci}가 없습니다`
        : `from the observed played root, the certified analysis continuation retains the ${message.releasedBlocker.piece} on ${message.releasedBlocker.square} and the ${message.routePiece.piece} on ${message.routePiece.square} through the pre-route occurrence, and the exact first route move ${message.route[0]!.moveUci} is absent`;
    const occurrenceComparison =
      message.branch.role === 'counterfactual_reference'
        ? `${reference}; ${played}`
        : `${played}; ${reference}`;
    const lowerPremises = message.premises
      .map(premise =>
        isSquareReleaseRouteLegalPremise(premise)
          ? `${premise.role}:${premise.moveUci}:${premise.legalMoveSemanticId.slice(0, 8)}`
          : `${premise.role}:${premise.resultId.slice(0, 8)}`,
      )
      .join(', ');
    return locale === 'ko-KR'
      ? `[${message.causeKind}] ${occurrenceComparison}. 경로 ${message.pathOccurrenceId.slice(0, 8)}는 순서 있는 하위 전제 ${lowerPremises}, 폐쇄 부재 ${message.absences[0]!.query}, 폐쇄 상태 ${message.states.map(state => state.query).join(', ')}를 정확한 발생 위치에 보존합니다.`
      : `[${message.causeKind}] ${occurrenceComparison}. Path ${message.pathOccurrenceId.slice(0, 8)} retains ordered lower premises ${lowerPremises}, closed absence ${message.absences[0]!.query}, and closed states ${message.states.map(state => state.query).join(', ')} at their exact occurrences.`;
  }
  const targets = message.resultTargetSubjects.join(', ');
  const premises = message.premises.map(premise => premise.resultId.slice(0, 8)).join(', ');
  return locale === 'ko-KR'
    ? `[${message.causeKind}] 관측된 실전 첫 수 ${message.rootActor.from}–${message.rootActor.to} 뒤 인증 분석 후속 수순의 유일 합법 응수 ${message.replyMove}와 ${message.pathRealizationMove}에서 ${targets}의 통과폰 진행 결과가 확인됩니다 (ply ${message.pathRealizationPly}; ${message.replyClosure.issuerEvidenceId} 인벤토리가 ${message.replyClosure.rootAfter.fen}에서 그 응수를 인증하고, ${message.eventEvidenceId}가 분석 결과 occurrence를 소유함, 경로 전제 ${premises}, 독립 경로 ${message.pathOccurrenceId.slice(0, 8)}).`
    : `[${message.causeKind}] From the observed played root ${message.rootActor.from}–${message.rootActor.to}, the certified analysis continuation through the only legal reply ${message.replyMove} and ${message.pathRealizationMove} establishes the passed-pawn progress result for ${targets} (ply ${message.pathRealizationPly}; inventory ${message.replyClosure.issuerEvidenceId} certifies that reply at ${message.replyClosure.rootAfter.fen}; ${message.eventEvidenceId} owns the analysis-result occurrence; path premises ${premises}; independent path ${message.pathOccurrenceId.slice(0, 8)}).`;
}

export function moveReviewReasonRole(
  core: MoveReviewCore,
  reasonId: string,
): MoveReviewReasonRole | undefined {
  if (core.reasonRefs.primary.includes(reasonId)) return 'primary';
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
  const causalTransport = projectCausalTransport(
    value.causal_explanations,
    move,
    bestUci,
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
  const primaryReasons = projectedReasons
    .filter(reason => reason.role === 'primary')
    .map(reason => reason.reason.id);
  const routes = projectedReasons
    .filter(reason => reason.role === 'proof-route')
    .map(reason => reason.reason.id);
  if (primaryReasons.length < 1 && lineReason) primaryReasons.push(lineReason.id);
  const support = projectedReasons
    .filter(reason => reason.role === 'support')
    .map(reason => reason.reason.id);
  if (lineReason && !primaryReasons.includes(lineReason.id)) support.push(lineReason.id);
  const coreFields: MoveReviewCoreFields = {
    verdictRef,
    verdictCode,
    verdictSymbol: bestChoice ? 'none' : verdictSymbolByCode[verdictCode],
    playedUci: move,
    bestUci,
    ...(winChance ? { winChance } : {}),
    ...(referenceEndpoint.terminal ? { referenceTerminal: referenceEndpoint.terminal } : {}),
    ...(reviewedEndpoint.terminal ? { reviewedTerminal: reviewedEndpoint.terminal } : {}),
    reasonRefs: { primary: primaryReasons, support, routes },
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

type MoveReviewPublicCauseKind =
  | 'wrong_move_order'
  | 'missed_tactical_resource'
  | 'missed_square_release'
  | 'passed_pawn_progress';

interface ProjectedCausalTransport {
  reasons: ProjectedCausalReason[];
}

type CausalFacetMetadata = {
  causeEvidenceId: string;
  channels: unknown[];
} & (
  | {
      causeKind: 'wrong_move_order' | 'missed_tactical_resource' | 'missed_square_release';
      exposure: 'primary';
    }
  | {
      causeKind: 'passed_pawn_progress';
      exposure: 'complementary';
    }
);

function projectCausalTransport(
  value: unknown,
  candidateMove: Uci,
  bestMove: Uci,
  startFen: FEN,
): ProjectedCausalTransport | undefined {
  if (value === undefined) return { reasons: [] };
  if (!Array.isArray(value) || value.length < 1) return;
  const reasons: ProjectedCausalReason[] = [];
  const causeEvidenceIds = new Set<string>();
  const channelIds = new Set<string>();
  for (const valueFacet of value) {
    const facet = projectCausalFacetMetadata(valueFacet);
    if (!facet) return;
    if (causeEvidenceIds.has(facet.causeEvidenceId)) return;
    causeEvidenceIds.add(facet.causeEvidenceId);
    const facetReasons: MoveReviewReason[] = [];
    for (const channel of facet.channels) {
      if (
        !isObject(channel) ||
        !semanticId(channel.channel_id) ||
        channelIds.has(channel.channel_id as string)
      )
        return;
      channelIds.add(channel.channel_id as string);
      const projected = projectCausalChannel(
        channel,
        facet.causeEvidenceId,
        facet.causeKind,
        candidateMove,
        bestMove,
        startFen,
      );
      if (!projected) return;
      facetReasons.push(...projected);
    }
    reasons.push(
      ...facetReasons.map((reason, index) => ({
        role:
          facet.exposure === 'primary'
            ? index === 0
              ? ('primary' as const)
              : ('proof-route' as const)
            : ('support' as const),
        reason,
      })),
    );
  }
  return { reasons };
}

function projectCausalFacetMetadata(value: unknown): CausalFacetMetadata | undefined {
  if (!isObject(value)) return;
  const causeEvidenceId = nonEmptyWireString(value.cause_evidence_id);
  if (!causeEvidenceId || !Array.isArray(value.channels) || value.channels.length < 1) return;
  if (!hasExactKeys(value, ['cause_evidence_id', 'kind', 'exposure', 'channels'])) return;
  if (value.kind === 'passed_pawn_progress') {
    if (value.exposure !== 'complementary') return;
    return { causeEvidenceId, causeKind: value.kind, exposure: value.exposure, channels: value.channels };
  }
  if (
    value.kind === 'wrong_move_order' ||
    value.kind === 'missed_tactical_resource' ||
    value.kind === 'missed_square_release'
  ) {
    if (value.exposure !== 'primary') return;
    return { causeEvidenceId, causeKind: value.kind, exposure: value.exposure, channels: value.channels };
  }
  return;
}

function projectCausalChannel(
  value: unknown,
  causeEvidenceId: string,
  causeKind: MoveReviewPublicCauseKind,
  candidateMove: Uci,
  bestMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  if (!isObject(value) || !semanticId(value.channel_id)) return;
  if (
    Object.prototype.hasOwnProperty.call(
      value,
      'unique_check_reply_defender_displacement_before_capture_proof',
    )
  ) {
    if (causeKind !== 'wrong_move_order') return;
    return projectUniqueCheckReplyDefenderDisplacementBeforeCapture(
      value,
      causeEvidenceId,
      candidateMove,
      bestMove,
      startFen,
    );
  }
  if (Object.prototype.hasOwnProperty.call(value, 'sole_recapturer_removal_before_target_capture_proof')) {
    if (causeKind !== 'wrong_move_order') return;
    return projectSoleRecapturerRemovalBeforeTargetCapture(
      value,
      causeEvidenceId,
      candidateMove,
      bestMove,
      startFen,
    );
  }
  if (Object.prototype.hasOwnProperty.call(value, 'capture_exclusion_move_order_proof')) {
    if (causeKind !== 'wrong_move_order') return;
    return projectCaptureExclusionMoveOrder(value, causeEvidenceId, candidateMove, bestMove, startFen);
  }
  if (
    Object.prototype.hasOwnProperty.call(value, 'vacated_gate_enables_unrecapturable_slider_capture_proof')
  ) {
    if (causeKind !== 'missed_tactical_resource') return;
    return projectVacatedGateEnablesUnrecapturableSliderCapture(
      value,
      causeEvidenceId,
      candidateMove,
      bestMove,
      startFen,
    );
  }
  if (Object.prototype.hasOwnProperty.call(value, 'square_release_route_proof')) {
    if (causeKind !== 'missed_square_release') return;
    return projectSquareReleaseRoute(value, causeEvidenceId, candidateMove, bestMove, startFen);
  }
  if (
    Object.prototype.hasOwnProperty.call(value, 'passed_pawn_progress_realized_after_only_legal_reply_proof')
  ) {
    if (causeKind !== 'passed_pawn_progress') return;
    return projectPassedPawnProgressRealizedAfterOnlyLegalReply(
      value,
      causeEvidenceId,
      candidateMove,
      startFen,
    );
  }
  return;
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
}

interface MoveReviewPassedPawnProgressWireStep extends MoveReviewWireStep {
  stepKey: string;
  line: MoveReviewTypedLine & { role: 'played' };
}

interface MoveReviewPassedPawnProgressWireBranch extends MoveReviewWireBranch {
  role: 'played_root_analysis_continuation';
  provenance: 'observed_game_root';
  lineRole: 'played';
  steps: MoveReviewPassedPawnProgressWireStep[];
}

interface MoveReviewWireBranch extends MoveReviewTypedBranch {
  steps: MoveReviewWireStep[];
  replyMove?: Uci;
  sourceOccurrenceId?: string;
}

type MoveReviewUniqueCheckReplyDefenderDisplacementBeforeCapturePath = {
  id: string;
  premises: MoveReviewTypedPremise[];
  absence: Extract<
    MoveReviewReasonMessage,
    { kind: 'unique-check-reply-defender-displacement-before-capture' }
  >['absence'];
};

type MoveReviewSoleRecapturerRemovalBeforeTargetCapturePath = {
  id: string;
  premises: MoveReviewTypedPremise[];
  absence: Extract<
    MoveReviewReasonMessage,
    { kind: 'sole-recapturer-removal-before-target-capture' }
  >['absence'];
};

interface MoveReviewCausalClosureUse {
  useId: string;
  role: string;
  semanticProofId: string;
  issuer:
    | 'position_relation_extractor.closed_relation_inventory'
    | 'position_relation_extractor.closed_position_state_inventory';
  issuerEvidenceId: string;
  issuerOccurrenceId: string;
  query: string;
  branchId: string;
  branchRole: 'counterfactual_reference' | 'played_root_analysis_continuation';
  afterStepIndex: number;
  fen: FEN;
  ply: number;
  scope: 'best_line' | 'played_line';
}

interface MoveReviewCausalPath<Premise> {
  id: string;
  premises: Premise[];
  absences: MoveReviewCausalClosureUse[];
  states: MoveReviewCausalClosureUse[];
}

type MoveReviewRelationCausalPath = MoveReviewCausalPath<MoveReviewTypedPremise>;
type MoveReviewSquareReleaseRoutePath = MoveReviewCausalPath<MoveReviewSquareReleaseRoutePremise>;
type MoveReviewCaptureExclusionMoveOrderPath = Omit<
  MoveReviewCausalPath<MoveReviewLegalMovePremise>,
  'premises'
> & { premises: MoveReviewCaptureExclusionMoveOrderPremises };

type MoveReviewPassedPawnProgressRealizedAfterOnlyLegalReplyPath = {
  id: string;
  branch: MoveReviewPassedPawnProgressWireBranch & { replyMove: Uci };
  realizationActor: MoveReviewTypedActor;
  realizationMove: Uci;
  realizationPly: number;
  premises: MoveReviewTypedPremise[];
  closureUseIds: string[];
};

type MoveReviewTypedProofKey =
  | 'unique_check_reply_defender_displacement_before_capture_proof'
  | 'sole_recapturer_removal_before_target_capture_proof'
  | 'capture_exclusion_move_order_proof'
  | 'vacated_gate_enables_unrecapturable_slider_capture_proof'
  | 'square_release_route_proof'
  | 'passed_pawn_progress_realized_after_only_legal_reply_proof';

function typedChannelProof(
  channel: Record<string, unknown>,
  key: MoveReviewTypedProofKey,
): Record<string, unknown> | undefined {
  return hasExactKeys(channel, ['channel_id', key]) && isObject(channel[key]) ? channel[key] : undefined;
}

function projectVariableTwoBranchProof(
  channel: Record<string, unknown>,
  key: MoveReviewTypedProofKey,
  extraFields: string[],
  candidateMove: Uci,
  bestMove: Uci,
  startFen: FEN,
  optionalFields: string[] = [],
):
  | {
      wire: Record<string, unknown> & { proof_paths: unknown[] };
      reference: MoveReviewWireBranch;
      played: MoveReviewWireBranch;
    }
  | undefined {
  const wire = typedChannelProof(channel, key);
  const requiredFields = [
    'source_evidence_id',
    'semantic_id',
    'occurrence_id',
    'dependency_fingerprint',
    'counterfactual_reference_branch',
    'played_root_branch',
    'proof_paths',
    ...extraFields,
  ];
  if (
    !wire ||
    !hasOnlyKeys(wire, [...requiredFields, ...optionalFields], requiredFields) ||
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
    true,
  );
  const played = projectTypedBranch(
    wire.played_root_branch,
    'played_root_analysis_continuation',
    startFen,
    true,
  );
  return !reference ||
    !played ||
    reference.id === played.id ||
    reference.rootMove !== bestMove ||
    played.rootMove !== candidateMove
    ? undefined
    : { wire: wire as Record<string, unknown> & { proof_paths: unknown[] }, reference, played };
}

function projectUniqueCheckReplyDefenderDisplacementBeforeCapture(
  channel: Record<string, unknown>,
  causeEvidenceId: string,
  candidateMove: Uci,
  bestMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  const wire = typedChannelProof(channel, 'unique_check_reply_defender_displacement_before_capture_proof');
  if (!wire) return;
  if (
    !hasExactKeys(wire, [
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
  const played = projectTypedBranch(wire.played_root_branch, 'played_root_analysis_continuation', startFen);
  const realizingMove = uci(wire.realizing_move);
  const defenseMove = uci(wire.played_root_branch_legal_defense_move);
  const participants = projectUniqueCheckReplyDefenderDisplacementBeforeCaptureParticipants(
    wire.participants,
  );
  if (
    !reference ||
    !played ||
    reference.id === played.id ||
    reference.rootMove !== bestMove ||
    played.rootMove !== candidateMove ||
    !realizingMove ||
    !defenseMove ||
    !participants ||
    reference.steps[2]?.move !== realizingMove ||
    played.steps[0]?.move !== realizingMove ||
    played.steps[1]?.move !== defenseMove ||
    participants.forcedReply.moveUci !== reference.steps[1]?.move ||
    participants.playedDefender.moveUci !== defenseMove ||
    !typedActorMatchesMove(participants.trigger, reference.steps[0]?.move) ||
    !typedActorMatchesMove(participants.forcedReply, reference.steps[1]?.move) ||
    !typedActorMatchesMove(participants.realizer, realizingMove) ||
    !typedActorMatchesMove(participants.playedDefender, defenseMove) ||
    participants.trigger.side !== participants.realizer.side ||
    participants.forcedReply.side === participants.trigger.side ||
    participants.playedDefender.side !== participants.forcedReply.side ||
    participants.capturedTarget.side !== participants.forcedReply.side ||
    participants.capturedTarget.square !== participants.realizer.to ||
    participants.playedDefender.to !== participants.realizer.to ||
    participants.disabledDefender.side !== participants.forcedReply.side ||
    participants.disabledDefender.piece !== participants.forcedReply.pieceBefore ||
    participants.disabledDefender.square !== participants.forcedReply.from ||
    participants.disabledDefender.side !== participants.playedDefender.side ||
    participants.disabledDefender.piece !== participants.playedDefender.pieceBefore ||
    participants.disabledDefender.square !== participants.playedDefender.from
  )
    return;
  const exactPaths = wire.proof_paths.map(path =>
    projectUniqueCheckReplyDefenderDisplacementBeforeCapturePath(
      path,
      reference,
      played,
      participants.capturedTarget,
    ),
  );
  if (
    !exactPaths.every(
      (path): path is MoveReviewUniqueCheckReplyDefenderDisplacementBeforeCapturePath => !!path,
    ) ||
    !canonicalStrings(exactPaths.map(path => path.id)) ||
    !unique(exactPaths.map(path => path.absence.useId))
  )
    return;
  return exactPaths.flatMap(path =>
    [reference, played].map(branch => {
      const proof = proofFromWireSteps(`cause:${path.id}:${branch.id}`, startFen, branch.steps)!;
      const counterpart = branch === reference ? played : reference;
      return {
        id: proof.id,
        messageSlots: { candidateUci: candidateMove },
        message: {
          kind: 'unique-check-reply-defender-displacement-before-capture' as const,
          channelId: channel.channel_id as string,
          causeEvidenceId,
          causeKind: 'wrong_move_order' as const,
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
        },
        proof,
      };
    }),
  );
}

function projectSoleRecapturerRemovalBeforeTargetCapture(
  channel: Record<string, unknown>,
  causeEvidenceId: string,
  candidateMove: Uci,
  bestMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  const wire = typedChannelProof(channel, 'sole_recapturer_removal_before_target_capture_proof');
  if (
    !wire ||
    !hasExactKeys(wire, [
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
  const played = projectTypedBranch(wire.played_root_branch, 'played_root_analysis_continuation', startFen);
  const laterExploitMove = uci(wire.later_exploit_move);
  const playedSoleRecaptureMove = uci(wire.played_sole_recapture_move);
  const participants = projectSoleRecapturerRemovalBeforeTargetCaptureParticipants(wire.participants);
  if (
    !reference ||
    !played ||
    reference.id === played.id ||
    reference.rootMove !== bestMove ||
    played.rootMove !== candidateMove ||
    !laterExploitMove ||
    !playedSoleRecaptureMove ||
    !participants ||
    reference.steps[2]?.move !== laterExploitMove ||
    played.steps[0]?.move !== laterExploitMove ||
    participants.removalRecapture.moveUci !== reference.steps[1]?.move ||
    participants.playedSoleRecapture.moveUci !== playedSoleRecaptureMove ||
    played.steps[1]?.move !== playedSoleRecaptureMove ||
    !typedActorMatchesMove(participants.remover, reference.steps[0]?.move) ||
    !typedActorMatchesMove(participants.removalRecapture, reference.steps[1]?.move) ||
    !typedActorMatchesMove(participants.laterExploit, laterExploitMove) ||
    !typedActorMatchesMove(participants.playedSoleRecapture, playedSoleRecaptureMove) ||
    participants.remover.side === participants.removedDefender.side ||
    participants.remover.to !== participants.removedDefender.square ||
    participants.removalRecapture.side !== participants.removedDefender.side ||
    participants.removalRecapture.to !== participants.remover.to ||
    participants.laterExploit.side !== participants.remover.side ||
    participants.capturedTarget.side !== participants.removedDefender.side ||
    participants.laterExploit.to !== participants.capturedTarget.square ||
    participants.playedSoleRecapture.side !== participants.removedDefender.side ||
    participants.playedSoleRecapture.pieceBefore !== participants.removedDefender.piece ||
    participants.playedSoleRecapture.from !== participants.removedDefender.square ||
    participants.playedSoleRecapture.to !== participants.laterExploit.to
  )
    return;
  const paths = wire.proof_paths.map(path =>
    projectSoleRecapturerRemovalBeforeTargetCapturePath(
      path,
      reference,
      played,
      participants.removedDefender,
      participants.capturedTarget,
    ),
  );
  if (
    !paths.every((path): path is MoveReviewSoleRecapturerRemovalBeforeTargetCapturePath => !!path) ||
    !canonicalStrings(paths.map(path => path.id)) ||
    !unique(paths.map(path => path.absence.useId))
  )
    return;
  return paths.flatMap(path =>
    [reference, played].map(branch => {
      const proof = proofFromWireSteps(`cause:${path.id}:${branch.id}`, startFen, branch.steps)!;
      const counterpart = branch === reference ? played : reference;
      return {
        id: proof.id,
        messageSlots: { candidateUci: candidateMove },
        message: {
          kind: 'sole-recapturer-removal-before-target-capture' as const,
          channelId: channel.channel_id as string,
          causeEvidenceId,
          causeKind: 'wrong_move_order' as const,
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

function projectSoleRecapturerRemovalBeforeTargetCapturePath(
  value: unknown,
  reference: MoveReviewWireBranch,
  played: MoveReviewWireBranch,
  removedDefender: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key },
  capturedTarget: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key },
): MoveReviewSoleRecapturerRemovalBeforeTargetCapturePath | undefined {
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
  const premises = value.premises.map(premise => projectTypedPremise(premise, [reference, played]));
  if (
    !premises.every((premise): premise is MoveReviewTypedPremise => !!premise) ||
    !unique(premises.map(premise => premise.resultId)) ||
    !unique(premises.map(premise => premise.issuerOccurrenceId!)) ||
    !matchesTypedPremiseOccurrence(
      premises[0]!,
      'reference_defender_removal',
      'capture_recapture_inventory',
      reference,
      0,
    ) ||
    !matchesTypedPremiseOccurrence(
      premises[1]!,
      'reference_later_exploit_inventory',
      'capture_recapture_inventory',
      reference,
      2,
    ) ||
    !matchesTypedPremiseOccurrence(
      premises[2]!,
      'played_immediate_exploit_inventory',
      'capture_recapture_inventory',
      played,
      0,
    )
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
    absence.query !== `legal-capture:${removedDefender.side}:${capturedTarget.square}` ||
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
    position.scope !== 'best_line' ||
    fen !== reference.steps[2]?.fenAfter ||
    ply !== reference.steps[2]?.ply
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

function projectSoleRecapturerRemovalBeforeTargetCaptureParticipants(value: unknown):
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

function projectCaptureExclusionMoveOrder(
  channel: Record<string, unknown>,
  causeEvidenceId: string,
  candidateMove: Uci,
  bestMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  const projected = projectVariableTwoBranchProof(
    channel,
    'capture_exclusion_move_order_proof',
    [],
    candidateMove,
    bestMove,
    startFen,
  );
  if (!projected) return;
  const { wire, reference, played } = projected;
  if (reference.steps.length % 2 !== 1 || played.steps.length !== 2 || wire.proof_paths.length !== 1) return;
  const path = projectCaptureExclusionMoveOrderPath(wire.proof_paths[0], reference, played);
  if (!path) return;
  return [reference, played].map(branch => {
    const proof = proofFromWireSteps(`cause:${path.id}:${branch.id}`, startFen, branch.steps)!;
    return {
      id: proof.id,
      messageSlots: { candidateUci: candidateMove },
      message: {
        kind: 'capture-exclusion-move-order' as const,
        channelId: channel.channel_id as string,
        causeEvidenceId,
        causeKind: 'wrong_move_order' as const,
        sourceEvidenceId: wire.source_evidence_id as string,
        semanticId: wire.semantic_id as string,
        occurrenceId: wire.occurrence_id as string,
        dependencyFingerprint: wire.dependency_fingerprint as string,
        pathOccurrenceId: path.id,
        branch: wireBranchIdentity(branch),
        counterpart: wireBranchIdentity(branch === reference ? played : reference),
        premises: path.premises,
        absences: path.absences,
        states: path.states,
      },
      proof,
    };
  });
}

function projectCaptureExclusionMoveOrderPath(
  value: unknown,
  reference: MoveReviewWireBranch,
  played: MoveReviewWireBranch,
): MoveReviewCaptureExclusionMoveOrderPath | undefined {
  const path = projectCausalPath(
    value,
    reference,
    played,
    { premises: 4, absences: 2, minStates: 8 },
    premise => {
      if (!isObject(premise)) return;
      const branch =
        premise.branch_id === reference.id && premise.branch_role === reference.role
          ? reference
          : premise.branch_id === played.id && premise.branch_role === played.role
            ? played
            : undefined;
      return branch ? projectLegalMovePremise(premise, branch) : undefined;
    },
    premise => `${premise.legalMoveSemanticId}:${premise.branchId}:${premise.stepIndex}`,
    premise => premise.issuerOccurrenceId,
  );
  if (!path) return;
  const [vacating, playedDeferred, playedReply, referenceDeferred] = path.premises;
  const deferredIndex = reference.steps.length - 1;
  const capturedTarget = playedReply?.capture;
  const sameDeferredCapture = playedDeferred?.capture
    ? sameColoredPiece(playedDeferred.capture, referenceDeferred?.capture)
    : referenceDeferred?.capture === undefined;
  if (
    !vacating ||
    !playedDeferred ||
    !playedReply ||
    !referenceDeferred ||
    vacating.role !== 'reference_vacating_move' ||
    vacating.branchId !== reference.id ||
    vacating.branchRole !== reference.role ||
    vacating.stepIndex !== 0 ||
    playedDeferred.role !== 'played_deferred_move' ||
    playedDeferred.branchId !== played.id ||
    playedDeferred.branchRole !== played.role ||
    playedDeferred.stepIndex !== 0 ||
    playedReply.role !== 'played_capture_reply' ||
    playedReply.branchId !== played.id ||
    playedReply.branchRole !== played.role ||
    playedReply.stepIndex !== 1 ||
    referenceDeferred.role !== 'reference_deferred_move' ||
    referenceDeferred.branchId !== reference.id ||
    referenceDeferred.branchRole !== reference.role ||
    referenceDeferred.stepIndex !== deferredIndex ||
    deferredIndex < 2 ||
    deferredIndex % 2 !== 0 ||
    playedDeferred.legalMoveSemanticId !== referenceDeferred.legalMoveSemanticId ||
    playedDeferred.moveUci !== referenceDeferred.moveUci ||
    !sameTypedActor(playedDeferred.movement, referenceDeferred.movement) ||
    playedDeferred.movementMode !== referenceDeferred.movementMode ||
    !sameDeferredCapture ||
    !capturedTarget ||
    capturedTarget.square !== playedReply.movement.to ||
    capturedTarget.square !== vacating.movement.from ||
    capturedTarget.side !== vacating.movement.side ||
    capturedTarget.piece !== vacating.movement.pieceBefore ||
    vacating.movement.side !== playedDeferred.movement.side ||
    playedReply.movement.side === playedDeferred.movement.side
  )
    return;
  const replyQuery = `legal-move-from-to:${playedReply.movement.side}:${playedReply.movement.from}:${playedReply.movement.to}`;
  if (
    path.absences.some(
      (absence, index) =>
        absence.role !== 'reference_capture_reply_absent' ||
        absence.branchId !== reference.id ||
        absence.branchRole !== reference.role ||
        absence.afterStepIndex !== [0, deferredIndex][index] ||
        absence.query !== replyQuery,
    )
  )
    return;
  const expectedStates = Array.from({ length: deferredIndex + 1 }, (_, index) => [
    {
      role: 'reference_vacated_target',
      stepIndex: index,
      query: `vacant:${capturedTarget.square}`,
    },
    {
      role: 'reference_reply_actor',
      stepIndex: index,
      query: `occupied-by:${playedReply.movement.side}:${playedReply.movement.pieceBefore}@${playedReply.movement.from}`,
    },
    ...(index < deferredIndex
      ? [
          {
            role: 'reference_deferred_actor',
            stepIndex: index,
            query: `occupied-by:${playedDeferred.movement.side}:${playedDeferred.movement.pieceBefore}@${playedDeferred.movement.from}`,
          },
        ]
      : []),
  ]).flat();
  if (
    path.states.length !== expectedStates.length ||
    path.states.some((state, index) => {
      const expected = expectedStates[index]!;
      return (
        state.role !== expected.role ||
        state.branchId !== reference.id ||
        state.branchRole !== reference.role ||
        state.afterStepIndex !== expected.stepIndex ||
        state.query !== expected.query
      );
    })
  )
    return;
  return path as MoveReviewCaptureExclusionMoveOrderPath;
}

function projectVacatedGateEnablesUnrecapturableSliderCapture(
  channel: Record<string, unknown>,
  causeEvidenceId: string,
  candidateMove: Uci,
  bestMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  const projected = projectVariableTwoBranchProof(
    channel,
    'vacated_gate_enables_unrecapturable_slider_capture_proof',
    ['participants', 'exploit_move'],
    candidateMove,
    bestMove,
    startFen,
  );
  if (!projected) return;
  const { wire, reference, played } = projected;
  const participants = projectVacatedGateEnablesUnrecapturableSliderCaptureParticipants(wire.participants);
  const exploitMove = uci(wire.exploit_move);
  if (!participants || !exploitMove) return;
  const paths = wire.proof_paths.map(path =>
    projectVacatedGateEnablesUnrecapturableSliderCapturePath(
      path,
      reference,
      played,
      participants,
      exploitMove,
    ),
  );
  if (
    !paths.every((path): path is MoveReviewRelationCausalPath => !!path) ||
    !canonicalStrings(paths.map(path => path.id)) ||
    !unique(paths.flatMap(path => [...path.absences, ...path.states]).map(use => use.useId))
  ) {
    return;
  }
  return paths.flatMap(path =>
    [reference, played].map(branch => {
      const proof = proofFromWireSteps(`cause:${path.id}:${branch.id}`, startFen, branch.steps)!;
      return {
        id: proof.id,
        messageSlots: { candidateUci: candidateMove },
        message: {
          kind: 'vacated-gate-enables-unrecapturable-slider-capture' as const,
          channelId: channel.channel_id as string,
          causeEvidenceId,
          causeKind: 'missed_tactical_resource' as const,
          sourceEvidenceId: wire.source_evidence_id as string,
          semanticId: wire.semantic_id as string,
          occurrenceId: wire.occurrence_id as string,
          dependencyFingerprint: wire.dependency_fingerprint as string,
          pathOccurrenceId: path.id,
          branch: wireBranchIdentity(branch),
          counterpart: wireBranchIdentity(branch === reference ? played : reference),
          ...participants,
          exploitMove,
          premises: path.premises,
          absences: path.absences,
          states: path.states,
        },
        proof,
      };
    }),
  );
}

interface MoveReviewVacatedGateParticipants {
  enabler: MoveReviewTypedActor;
  slider: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
  gateBlocker: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
  exploit: MoveReviewTypedActor;
  capturedTarget: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
}

function projectVacatedGateEnablesUnrecapturableSliderCaptureParticipants(
  value: unknown,
): MoveReviewVacatedGateParticipants | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['enabler', 'slider', 'gate_blocker', 'exploit', 'captured_target'])
  )
    return;
  const enabler = projectTypedActor(value.enabler);
  const slider = projectColoredPiece(value.slider);
  const gateBlocker = projectColoredPiece(value.gate_blocker);
  const exploit = projectTypedActor(value.exploit);
  const capturedTarget = projectColoredPiece(value.captured_target);
  return enabler && slider && gateBlocker && exploit && capturedTarget
    ? { enabler, slider, gateBlocker, exploit, capturedTarget }
    : undefined;
}

function projectVacatedGateEnablesUnrecapturableSliderCapturePath(
  value: unknown,
  reference: MoveReviewWireBranch,
  played: MoveReviewWireBranch,
  participants: MoveReviewVacatedGateParticipants,
  exploitMove: Uci,
): MoveReviewRelationCausalPath | undefined {
  const path = projectCausalPath(
    value,
    reference,
    played,
    { premises: 2, absences: 3, minStates: 10 },
    premise => projectVacatedGateEnablesUnrecapturableSliderCapturePremise(premise, [reference, played]),
    premise => premise.resultId,
    premise => premise.issuerOccurrenceId!,
  );
  if (!path) return;
  const exploitIndex = reference.steps.length - 1;
  const rootPremise = path.premises[0]!;
  const exploitPremise = path.premises[1]!;
  const { enabler, slider, gateBlocker, exploit, capturedTarget } = participants;
  if (
    !matchesTypedPremiseOccurrence(
      rootPremise,
      'reference_root_slider_reach',
      'slider_reach_delta',
      reference,
      0,
    ) ||
    !matchesTypedPremiseOccurrence(
      exploitPremise,
      'reference_exploit_capture',
      'capture_recapture_inventory',
      reference,
      exploitIndex,
    ) ||
    played.steps.length !== exploitIndex ||
    reference.steps
      .slice(1, exploitIndex)
      .some((step, index) => step.move !== played.steps[index + 1]?.move) ||
    reference.steps[exploitIndex]?.move !== exploitMove ||
    !typedActorMatchesMove(enabler, reference.steps[0]?.move) ||
    !typedActorMatchesMove(exploit, exploitMove) ||
    enabler.side !== slider.side ||
    gateBlocker.side !== enabler.side ||
    gateBlocker.piece !== enabler.pieceBefore ||
    gateBlocker.square !== enabler.from ||
    exploit.side !== slider.side ||
    exploit.from !== slider.square ||
    exploit.pieceBefore !== slider.piece ||
    exploit.pieceAfter !== slider.piece ||
    exploit.to !== capturedTarget.square ||
    capturedTarget.side === slider.side ||
    path.states.length !== exploitIndex * 5
  )
    return;
  const preExploitIndex = exploitIndex - 1;
  const sliderQueryPrefix = `slider-reach:${slider.side}:${slider.piece}@${slider.square}:`;
  const expectedStates = [
    ...Array.from({ length: exploitIndex - 1 }, (_, offset) => ({
      role: 'reference_intervening_slider_reach',
      branchId: reference.id,
      branchRole: reference.role,
      afterStepIndex: offset + 1,
      query: sliderQueryPrefix,
      prefix: true,
    })),
    ...Array.from({ length: exploitIndex }, (_, stepIndex) => ({
      role: 'reference_target_persistence',
      branchId: reference.id,
      branchRole: reference.role,
      afterStepIndex: stepIndex,
      query: `occupied-by:${capturedTarget.side}:${capturedTarget.piece}@${capturedTarget.square}`,
      prefix: false,
    })),
    ...Array.from({ length: exploitIndex }, (_, stepIndex) => [
      {
        role: 'played_slider_persistence',
        branchId: played.id,
        branchRole: played.role,
        afterStepIndex: stepIndex,
        query: `occupied-by:${slider.side}:${slider.piece}@${slider.square}`,
        prefix: false,
      },
      {
        role: 'played_target_persistence',
        branchId: played.id,
        branchRole: played.role,
        afterStepIndex: stepIndex,
        query: `occupied-by:${capturedTarget.side}:${capturedTarget.piece}@${capturedTarget.square}`,
        prefix: false,
      },
      {
        role: 'played_gate_blocker_persistence',
        branchId: played.id,
        branchRole: played.role,
        afterStepIndex: stepIndex,
        query: `occupied-by:${gateBlocker.side}:${gateBlocker.piece}@${gateBlocker.square}`,
        prefix: false,
      },
    ]).flat(),
    {
      role: 'played_blocked_slider_reach',
      branchId: played.id,
      branchRole: played.role,
      afterStepIndex: preExploitIndex,
      query: sliderQueryPrefix,
      prefix: true,
    },
  ];
  const expectedAbsences = [
    {
      role: 'reference_immediate_recapture_absent',
      branchId: reference.id,
      branchRole: reference.role,
      afterStepIndex: exploitIndex,
      query: `legal-capture:${capturedTarget.side}:${capturedTarget.square}`,
    },
    {
      role: 'played_exploit_move_absent',
      branchId: played.id,
      branchRole: played.role,
      afterStepIndex: preExploitIndex,
      query: `legal-move-from-to:${slider.side}:${slider.square}:${capturedTarget.square}`,
    },
    {
      role: 'played_replacement_capture_absent',
      branchId: played.id,
      branchRole: played.role,
      afterStepIndex: preExploitIndex,
      query: `legal-capture:${slider.side}:${capturedTarget.square}`,
    },
  ];
  const referenceReachStates = path.states.slice(0, exploitIndex - 1);
  const playedBlockedReach = path.states[path.states.length - 1]!;
  const reachDirection = (query: string): string => query.slice(sliderQueryPrefix.length).split(':', 1)[0]!;
  const reachesTarget = (query: string): boolean =>
    query.includes(`[${capturedTarget.square}:`) || query.includes(`,${capturedTarget.square}:`);
  const direction = reachDirection(referenceReachStates[0]!.query);
  const exactGateSuffix = `:${gateBlocker.side}:${gateBlocker.piece}@${gateBlocker.square}`;
  if (
    !direction ||
    referenceReachStates.some(
      state =>
        reachDirection(state.query) !== direction ||
        !reachesTarget(state.query) ||
        state.query.endsWith(exactGateSuffix),
    ) ||
    reachDirection(playedBlockedReach.query) !== direction ||
    reachesTarget(playedBlockedReach.query) ||
    !playedBlockedReach.query.endsWith(exactGateSuffix) ||
    path.absences.some((absence, index) => {
      const expected = expectedAbsences[index]!;
      return (
        absence.role !== expected.role ||
        absence.branchId !== expected.branchId ||
        absence.branchRole !== expected.branchRole ||
        absence.afterStepIndex !== expected.afterStepIndex ||
        absence.query !== expected.query
      );
    }) ||
    path.states.some((state, index) => {
      const expected = expectedStates[index]!;
      return (
        state.role !== expected.role ||
        state.branchId !== expected.branchId ||
        state.branchRole !== expected.branchRole ||
        state.afterStepIndex !== expected.afterStepIndex ||
        (expected.prefix ? !state.query.startsWith(expected.query) : state.query !== expected.query)
      );
    })
  )
    return;
  return path;
}

function projectVacatedGateEnablesUnrecapturableSliderCapturePremise(
  value: unknown,
  branches: readonly MoveReviewWireBranch[],
): MoveReviewTypedPremise | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'role',
      'contract',
      'result_id',
      'issuer_evidence_id',
      'issuer_occurrence_id',
      'source_premise_ids',
      'branch_id',
      'branch_role',
      'step_index',
    ]) ||
    (value.role !== 'reference_root_slider_reach' && value.role !== 'reference_exploit_capture') ||
    (value.contract !== 'slider_reach_delta' && value.contract !== 'capture_recapture_inventory') ||
    typeof value.result_id !== 'string' ||
    !value.result_id.startsWith(`${value.contract}:`) ||
    !typedHash(value.result_id.slice(value.contract.length + 1)) ||
    !nonEmptyWireString(value.issuer_evidence_id) ||
    !typedHash(value.issuer_occurrence_id) ||
    !canonicalWireStrings(value.source_premise_ids, 3) ||
    !(value.source_premise_ids as string[]).includes(value.issuer_evidence_id as string) ||
    !(value.source_premise_ids as string[]).includes(value.issuer_occurrence_id as string)
  )
    return;
  const branch = branches.find(candidate => candidate.id === value.branch_id);
  const stepIndex = nonNegativeInteger(value.step_index);
  return !branch || branch.role !== value.branch_role || stepIndex === undefined || !branch.steps[stepIndex]
    ? undefined
    : {
        role: value.role,
        contract: value.contract,
        resultId: value.result_id,
        issuerEvidenceId: value.issuer_evidence_id as string,
        issuerOccurrenceId: value.issuer_occurrence_id as string,
        sourcePremiseIds: [...value.source_premise_ids],
        branchId: branch.id,
        branchRole: branch.role,
        stepIndex,
      };
}

function projectCausalClosureUse(
  value: unknown,
  reference: MoveReviewWireBranch,
  played: MoveReviewWireBranch,
  kind: 'absence' | 'state',
): MoveReviewCausalClosureUse | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
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
    !typedHash(value.use_id) ||
    !nonEmptyWireString(value.role) ||
    !typedHash(value.semantic_proof_id) ||
    value.issuer !==
      (kind === 'absence'
        ? 'position_relation_extractor.closed_relation_inventory'
        : 'position_relation_extractor.closed_position_state_inventory') ||
    !nonEmptyWireString(value.issuer_evidence_id) ||
    !typedHash(value.issuer_occurrence_id) ||
    !nonEmptyWireString(value.query)
  )
    return;
  const branch =
    value.branch_id === reference.id ? reference : value.branch_id === played.id ? played : undefined;
  const stepIndex = nonNegativeInteger(value.after_step_index);
  const position = isObject(value.position) ? value.position : undefined;
  const fen = position ? fenText(position.fen) : undefined;
  const ply = position ? nonNegativeInteger(position.ply) : undefined;
  const scope = branch === reference ? 'best_line' : 'played_line';
  if (
    !branch ||
    stepIndex === undefined ||
    !branch.steps[stepIndex] ||
    value.branch_role !== branch.role ||
    !position ||
    !hasExactKeys(position, ['fen', 'ply', 'scope']) ||
    !fen ||
    ply === undefined ||
    position.scope !== scope ||
    fen !== branch.steps[stepIndex]!.fenAfter ||
    ply !== branch.steps[stepIndex]!.ply
  )
    return;
  return {
    useId: value.use_id as string,
    role: value.role as string,
    semanticProofId: value.semantic_proof_id as string,
    issuer: value.issuer as MoveReviewCausalClosureUse['issuer'],
    issuerEvidenceId: value.issuer_evidence_id as string,
    issuerOccurrenceId: value.issuer_occurrence_id as string,
    query: value.query as string,
    branchId: branch.id,
    branchRole: branch.role as 'counterfactual_reference' | 'played_root_analysis_continuation',
    afterStepIndex: stepIndex,
    fen,
    ply,
    scope,
  };
}

function projectCausalPath<Premise>(
  value: unknown,
  reference: MoveReviewWireBranch,
  played: MoveReviewWireBranch,
  counts: { premises: number; absences: number } & (
    | { states: number; minStates?: never }
    | { minStates: number; states?: never }
  ),
  projectPremise: (value: unknown) => Premise | undefined,
  premiseId: (premise: Premise) => string,
  premiseOccurrenceId: (premise: Premise) => string,
): MoveReviewCausalPath<Premise> | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['path_occurrence_id', 'premises', 'closed_absence_uses', 'closed_state_uses']) ||
    !typedHash(value.path_occurrence_id) ||
    !Array.isArray(value.premises) ||
    value.premises.length !== counts.premises ||
    !Array.isArray(value.closed_absence_uses) ||
    value.closed_absence_uses.length !== counts.absences ||
    !Array.isArray(value.closed_state_uses) ||
    (counts.states !== undefined
      ? value.closed_state_uses.length !== counts.states
      : value.closed_state_uses.length < counts.minStates)
  )
    return;
  const projectedPremises = value.premises.map(projectPremise);
  const absences = value.closed_absence_uses.map(use =>
    projectCausalClosureUse(use, reference, played, 'absence'),
  );
  const states = value.closed_state_uses.map(use => projectCausalClosureUse(use, reference, played, 'state'));
  if (projectedPremises.some(premise => !premise) || absences.some(use => !use) || states.some(use => !use))
    return;
  const premises = projectedPremises as Premise[];
  const exactAbsences = absences as MoveReviewCausalClosureUse[];
  const exactStates = states as MoveReviewCausalClosureUse[];
  if (
    !unique(premises.map(premiseId)) ||
    !unique(premises.map(premiseOccurrenceId)) ||
    !unique([...exactAbsences, ...exactStates].map(use => use.useId))
  )
    return;
  return {
    id: value.path_occurrence_id as string,
    premises,
    absences: exactAbsences,
    states: exactStates,
  };
}

function projectSquareReleaseRoute(
  channel: Record<string, unknown>,
  causeEvidenceId: string,
  candidateMove: Uci,
  bestMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  const projected = projectVariableTwoBranchProof(
    channel,
    'square_release_route_proof',
    ['participants', 'route', 'terminal_step_index', 'terminal'],
    candidateMove,
    bestMove,
    startFen,
    ['terminal_reply_move'],
  );
  if (!projected) return;
  const { wire, reference, played } = projected;
  const participants = projectSquareReleaseRouteParticipants(wire.participants);
  const projectedRoute = Array.isArray(wire.route) ? wire.route.map(projectSquareReleaseRouteStep) : [];
  if (
    !participants ||
    !projectedRoute.length ||
    !projectedRoute.every((step): step is MoveReviewSquareReleaseRouteStep => !!step)
  )
    return;
  const route = projectedRoute;
  const terminalStepIndex = nonNegativeInteger(wire.terminal_step_index);
  const terminal = projectSquareReleaseRouteTerminal(wire.terminal, route[route.length - 1]!);
  const hasTerminalReply = Object.prototype.hasOwnProperty.call(wire, 'terminal_reply_move');
  const terminalReplyMove = hasTerminalReply ? uci(wire.terminal_reply_move) : undefined;
  const { releaser, releasedBlocker, routePiece } = participants;
  const firstRoute = route[0]!;
  const lastRoute = route[route.length - 1]!;
  const needsReply =
    !!terminal &&
    (terminal.kind === 'capture' ||
      (terminal.kind === 'created-check' && terminal.terminalState === 'ongoing'));
  if (
    terminalStepIndex === undefined ||
    !terminal ||
    hasTerminalReply !== needsReply ||
    (hasTerminalReply && !terminalReplyMove) ||
    releasedBlocker.side !== releaser.side ||
    releasedBlocker.piece !== releaser.pieceBefore ||
    releasedBlocker.square !== releaser.from ||
    routePiece.side !== firstRoute.side ||
    routePiece.piece !== firstRoute.pieceBefore ||
    routePiece.square !== firstRoute.from ||
    firstRoute.side !== releaser.side ||
    firstRoute.to !== releaser.from ||
    firstRoute.stepIndex < 2 ||
    firstRoute.stepIndex !== played.steps.length ||
    route.some((step, index) =>
      index === 0
        ? false
        : step.stepIndex <= route[index - 1]!.stepIndex ||
          step.side !== route[index - 1]!.side ||
          step.from !== route[index - 1]!.to ||
          step.pieceBefore !== route[index - 1]!.pieceAfter,
    ) ||
    terminalStepIndex !== lastRoute.stepIndex ||
    reference.steps.length !== terminalStepIndex + (terminalReplyMove ? 2 : 1) ||
    route.some(step => reference.steps[step.stepIndex]?.move !== step.moveUci) ||
    !typedActorMatchesMove(releaser, reference.steps[0]?.move) ||
    (terminalReplyMove && reference.steps[terminalStepIndex + 1]?.move !== terminalReplyMove) ||
    (terminal.kind === 'occupation' ? route.length !== 1 : route.length < 2)
  )
    return;
  const paths = wire.proof_paths.map(path =>
    projectSquareReleaseRoutePath(
      path,
      reference,
      played,
      releaser,
      releasedBlocker,
      routePiece,
      route,
      terminal,
      terminalReplyMove,
    ),
  );
  if (
    !paths.every((path): path is MoveReviewSquareReleaseRoutePath => !!path) ||
    !canonicalStrings(paths.map(path => path.id))
  )
    return;
  return paths.flatMap(path =>
    [reference, played].map(branch => {
      const proof = proofFromWireSteps(`cause:${path.id}:${branch.id}`, startFen, branch.steps)!;
      return {
        id: proof.id,
        messageSlots: { candidateUci: candidateMove },
        message: {
          kind: 'square-release-route' as const,
          channelId: channel.channel_id as string,
          causeEvidenceId,
          causeKind: 'missed_square_release' as const,
          sourceEvidenceId: wire.source_evidence_id as string,
          semanticId: wire.semantic_id as string,
          occurrenceId: wire.occurrence_id as string,
          dependencyFingerprint: wire.dependency_fingerprint as string,
          pathOccurrenceId: path.id,
          branch: wireBranchIdentity(branch),
          counterpart: wireBranchIdentity(branch === reference ? played : reference),
          releaser,
          releasedBlocker,
          routePiece,
          route,
          terminalStepIndex,
          terminal,
          ...(terminalReplyMove ? { terminalReplyMove } : {}),
          premises: path.premises,
          absences: path.absences,
          states: path.states,
        },
        proof,
      };
    }),
  );
}

function projectSquareReleaseRouteParticipants(value: unknown):
  | {
      releaser: MoveReviewTypedActor;
      releasedBlocker: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
      routePiece: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key };
    }
  | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['releaser', 'released_blocker', 'route_piece'])) return;
  const releaser = projectTypedActor(value.releaser);
  const releasedBlocker = projectColoredPiece(value.released_blocker);
  const routePiece = projectColoredPiece(value.route_piece);
  return releaser && releasedBlocker && routePiece ? { releaser, releasedBlocker, routePiece } : undefined;
}

function projectSquareReleaseRoutePath(
  value: unknown,
  reference: MoveReviewWireBranch,
  played: MoveReviewWireBranch,
  releaser: MoveReviewTypedActor,
  releasedBlocker: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key },
  routePiece: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key },
  route: MoveReviewSquareReleaseRouteStep[],
  terminal: MoveReviewSquareReleaseRouteTerminal,
  terminalReplyMove: Uci | undefined,
): MoveReviewSquareReleaseRoutePath | undefined {
  const firstRouteIndex = route[0]!.stepIndex;
  const terminalStepIndex = route[route.length - 1]!.stepIndex;
  const verticalPremiseCount = terminal.kind === 'occupation' ? 0 : 1;
  const premiseCount = verticalPremiseCount + 1 + route.length + (terminalReplyMove ? 1 : 0);
  const persistenceCount = route
    .slice(0, -1)
    .reduce((count, step, index) => count + route[index + 1]!.stepIndex - step.stepIndex - 1, 0);
  const path = projectCausalPath(
    value,
    reference,
    played,
    { premises: premiseCount, absences: 1, states: firstRouteIndex * 3 + route.length + persistenceCount },
    premise =>
      isObject(premise) && premise.contract === 'legal_move'
        ? projectLegalMovePremise(premise, reference)
        : projectTypedPremise(premise, [reference]),
    premise => (isSquareReleaseRouteLegalPremise(premise) ? premise.legalMoveSemanticId : premise.resultId),
    premise => premise.issuerOccurrenceId!,
  );
  if (!path) return;
  let premiseIndex = 0;
  const terminalPremise = terminal.kind === 'occupation' ? undefined : path.premises[premiseIndex++];
  const release = path.premises[premiseIndex++];
  const routePremises = path.premises.slice(premiseIndex, premiseIndex + route.length);
  premiseIndex += route.length;
  const replyPremise = terminalReplyMove ? path.premises[premiseIndex++] : undefined;
  const firstRoutePremise = routePremises[0];
  const finalRoutePremise = routePremises[routePremises.length - 1];
  if (
    !release ||
    !isSquareReleaseRouteLegalPremise(release) ||
    release.role !== 'reference_release_move' ||
    release.stepIndex !== 0 ||
    release.moveUci !== reference.steps[0]?.move ||
    !sameTypedActor(release.movement, releaser) ||
    premiseIndex !== path.premises.length ||
    routePremises.some((premise, routeIndex) => {
      const step = route[routeIndex]!;
      return (
        !isSquareReleaseRouteLegalPremise(premise) ||
        premise.role !== `reference_route_move_${routeIndex}` ||
        premise.stepIndex !== step.stepIndex ||
        premise.moveUci !== step.moveUci ||
        !sameTypedActor(premise.movement, step) ||
        (routeIndex === 0 && premise.capture !== undefined)
      );
    }) ||
    !squareReleaseRouteTerminalPremiseMatches(terminalPremise, terminal, reference, terminalStepIndex) ||
    !squareReleaseRouteReplyPremiseMatches(
      replyPremise,
      terminal,
      terminalReplyMove,
      reference,
      terminalStepIndex,
    ) ||
    (terminal.kind === 'occupation' &&
      (!firstRoutePremise ||
        !isSquareReleaseRouteLegalPremise(firstRoutePremise) ||
        firstRoutePremise.capture !== undefined)) ||
    (terminal.kind === 'capture' &&
      (!finalRoutePremise ||
        !isSquareReleaseRouteLegalPremise(finalRoutePremise) ||
        !sameColoredPiece(finalRoutePremise.capture, terminal.capturedTarget)))
  )
    return;
  const absence = path.absences[0]!;
  const expectedStates = [
    ...Array.from({ length: firstRouteIndex }, (_, stepIndex) => ({
      role: 'reference_vacancy',
      branchId: reference.id,
      branchRole: reference.role,
      stepIndex,
      query: `vacant:${releaser.from}`,
    })),
    ...route.map((step, routeIndex) => ({
      role: `reference_route_piece_${routeIndex}`,
      branchId: reference.id,
      branchRole: reference.role,
      stepIndex: step.stepIndex,
      query: `occupied-by:${step.side}:${step.pieceAfter}@${step.to}`,
    })),
    ...route.slice(0, -1).flatMap((step, routeIndex) =>
      Array.from({ length: route[routeIndex + 1]!.stepIndex - step.stepIndex - 1 }, (_, offset) => ({
        role: `reference_route_persistence_${routeIndex}`,
        branchId: reference.id,
        branchRole: reference.role,
        stepIndex: step.stepIndex + offset + 1,
        query: `occupied-by:${step.side}:${step.pieceAfter}@${step.to}`,
      })),
    ),
    ...Array.from({ length: firstRouteIndex }, (_, stepIndex) => ({
      role: 'played_blocker_persistence',
      branchId: played.id,
      branchRole: played.role,
      stepIndex,
      query: `occupied-by:${releasedBlocker.side}:${releasedBlocker.piece}@${releasedBlocker.square}`,
    })),
    ...Array.from({ length: firstRouteIndex }, (_, stepIndex) => ({
      role: 'played_route_origin_persistence',
      branchId: played.id,
      branchRole: played.role,
      stepIndex,
      query: `occupied-by:${routePiece.side}:${routePiece.piece}@${routePiece.square}`,
    })),
  ];
  if (
    absence.role !== 'played_first_route_leg_absent' ||
    absence.branchId !== played.id ||
    absence.branchRole !== played.role ||
    absence.afterStepIndex !== firstRouteIndex - 1 ||
    absence.query !== `legal-move-from-to:${route[0]!.side}:${route[0]!.from}:${route[0]!.to}` ||
    path.states.some((state, index) => {
      const expected = expectedStates[index]!;
      return (
        state.role !== expected.role ||
        state.branchId !== expected.branchId ||
        state.branchRole !== expected.branchRole ||
        state.afterStepIndex !== expected.stepIndex ||
        state.query !== expected.query
      );
    })
  )
    return;
  return path;
}

function isSquareReleaseRouteLegalPremise(
  premise: MoveReviewSquareReleaseRoutePremise,
): premise is MoveReviewLegalMovePremise {
  return 'legalMoveSemanticId' in premise;
}

function squareReleaseRouteTerminalPremiseMatches(
  premise: MoveReviewSquareReleaseRoutePremise | undefined,
  terminal: MoveReviewSquareReleaseRouteTerminal,
  reference: MoveReviewWireBranch,
  terminalStepIndex: number,
): boolean {
  if (terminal.kind === 'occupation') return premise === undefined;
  const contract =
    terminal.kind === 'capture' ? 'capture_recapture_inventory' : 'created_check_response_inventory';
  return (
    !!premise &&
    !isSquareReleaseRouteLegalPremise(premise) &&
    premise.contract === contract &&
    matchesTypedPremiseOccurrence(
      premise,
      'reference_terminal_resource',
      contract,
      reference,
      terminalStepIndex,
    ) &&
    premise.resultId.slice(contract.length + 1) !== terminal.assertionId
  );
}

function squareReleaseRouteReplyPremiseMatches(
  premise: MoveReviewSquareReleaseRoutePremise | undefined,
  terminal: MoveReviewSquareReleaseRouteTerminal,
  replyMove: Uci | undefined,
  reference: MoveReviewWireBranch,
  terminalStepIndex: number,
): boolean {
  if (!replyMove) return premise === undefined;
  if (
    !premise ||
    !isSquareReleaseRouteLegalPremise(premise) ||
    premise.role !== 'reference_terminal_reply' ||
    premise.stepIndex !== terminalStepIndex + 1 ||
    premise.moveUci !== replyMove ||
    reference.steps[premise.stepIndex]?.move !== replyMove ||
    (terminal.kind === 'capture' && premise.movement.side !== terminal.capturedTarget.side) ||
    (terminal.kind === 'created-check' && premise.movement.side !== terminal.checkedSide)
  )
    return false;
  return (
    terminal.kind !== 'created-check' ||
    terminal.responses.some(response =>
      sameSquareReleaseRouteResource(response.resource, {
        ...premise.movement,
        moveUci: premise.moveUci,
        ...(premise.capture ? { capture: premise.capture } : {}),
      }),
    )
  );
}

function projectSquareReleaseRouteStep(value: unknown): MoveReviewSquareReleaseRouteStep | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['side', 'from', 'to', 'piece_before', 'piece_after', 'move_uci', 'step_index'])
  )
    return;
  const actor = projectTypedActor({
    side: value.side,
    from: value.from,
    to: value.to,
    piece_before: value.piece_before,
    piece_after: value.piece_after,
  });
  const moveUci = uci(value.move_uci);
  const stepIndex = nonNegativeInteger(value.step_index);
  return actor && moveUci && stepIndex !== undefined && typedActorMatchesMove(actor, moveUci)
    ? { ...actor, moveUci, stepIndex }
    : undefined;
}

function projectSquareReleaseRouteTerminal(
  value: unknown,
  lastRoute: MoveReviewSquareReleaseRouteStep,
): MoveReviewSquareReleaseRouteTerminal | undefined {
  if (!isObject(value) || typeof value.kind !== 'string') return;
  if (value.kind === 'occupation') return hasExactKeys(value, ['kind']) ? { kind: 'occupation' } : undefined;
  if (value.kind === 'capture') {
    if (
      !hasExactKeys(value, [
        'kind',
        'assertion_id',
        'captured_target',
        'geometric_recapturers',
        'legal_recaptures',
        'restricted_recaptures',
      ]) ||
      !typedHash(value.assertion_id) ||
      !Array.isArray(value.geometric_recapturers) ||
      !Array.isArray(value.legal_recaptures) ||
      !Array.isArray(value.restricted_recaptures)
    )
      return;
    const capturedTarget = projectColoredPiece(value.captured_target);
    const geometricRecapturers = value.geometric_recapturers.map(projectPieceWitness);
    const legalRecaptures = value.legal_recaptures.map(projectSquareReleaseRouteResource);
    const restrictedRecaptures = value.restricted_recaptures.map(restriction => {
      if (
        !isObject(restriction) ||
        !hasExactKeys(restriction, ['piece', 'destination', 'king_square', 'post_move_controllers']) ||
        !key(restriction.destination) ||
        !key(restriction.king_square) ||
        !Array.isArray(restriction.post_move_controllers)
      )
        return;
      const piece = projectPieceWitness(restriction.piece);
      const controllers = restriction.post_move_controllers.map(projectPieceWitness);
      return piece &&
        controllers.length > 0 &&
        controllers.every((controller): controller is MoveReviewPieceWitness => !!controller) &&
        canonicalStrings(controllers.map(pieceWitnessKey))
        ? {
            piece,
            destination: restriction.destination as Key,
            kingSquare: restriction.king_square as Key,
            postMoveControllers: controllers,
          }
        : undefined;
    });
    if (
      !capturedTarget ||
      capturedTarget.side === lastRoute.side ||
      capturedTarget.square !== lastRoute.to ||
      !geometricRecapturers.every((piece): piece is MoveReviewPieceWitness => !!piece) ||
      !canonicalStrings(geometricRecapturers.map(pieceWitnessKey)) ||
      !legalRecaptures.every((resource): resource is MoveReviewSquareReleaseRouteResource => !!resource) ||
      !canonicalStrings(legalRecaptures.map(squareReleaseRouteResourceKey)) ||
      !restrictedRecaptures.every(
        (restriction): restriction is NonNullable<(typeof restrictedRecaptures)[number]> => !!restriction,
      ) ||
      legalRecaptures.some(
        resource =>
          resource.side !== capturedTarget.side ||
          resource.to !== lastRoute.to ||
          !geometricRecapturers.some(
            piece => piece.square === resource.from && piece.piece === resource.pieceBefore,
          ) ||
          !sameColoredPiece(resource.capture, {
            side: lastRoute.side,
            piece: lastRoute.pieceAfter,
            square: lastRoute.to,
          }),
      ) ||
      restrictedRecaptures.some(
        restriction =>
          restriction.destination !== lastRoute.to ||
          !geometricRecapturers.some(
            piece => piece.square === restriction.piece.square && piece.piece === restriction.piece.piece,
          ),
      )
    )
      return;
    return {
      kind: 'capture',
      assertionId: value.assertion_id as string,
      capturedTarget,
      geometricRecapturers,
      legalRecaptures,
      restrictedRecaptures,
    };
  }
  if (
    value.kind !== 'created_check' ||
    !hasExactKeys(value, [
      'kind',
      'assertion_id',
      'checked_side',
      'king_square',
      'checkers',
      'responses',
      'controlled_king_destinations',
      'terminal_state',
    ]) ||
    !typedHash(value.assertion_id) ||
    (value.checked_side !== 'white' && value.checked_side !== 'black') ||
    value.checked_side === lastRoute.side ||
    !key(value.king_square) ||
    (value.terminal_state !== 'ongoing' && value.terminal_state !== 'checkmate') ||
    !Array.isArray(value.checkers) ||
    !Array.isArray(value.responses) ||
    !Array.isArray(value.controlled_king_destinations)
  )
    return;
  const checkers = value.checkers.map(projectPieceWitness);
  const responses = value.responses.map(projectSquareReleaseRouteCheckResponse);
  const controlledKingDestinations = value.controlled_king_destinations.map(destination => {
    if (
      !isObject(destination) ||
      !hasExactKeys(destination, ['destination', 'controllers']) ||
      !key(destination.destination) ||
      !Array.isArray(destination.controllers)
    )
      return;
    const controllers = destination.controllers.map(projectPieceWitness);
    return controllers.length > 0 &&
      controllers.every((controller): controller is MoveReviewPieceWitness => !!controller) &&
      canonicalStrings(controllers.map(pieceWitnessKey))
      ? { destination: destination.destination as Key, controllers }
      : undefined;
  });
  if (
    checkers.length < 1 ||
    !checkers.every((piece): piece is MoveReviewPieceWitness => !!piece) ||
    !canonicalStrings(checkers.map(pieceWitnessKey)) ||
    !responses.every((response): response is NonNullable<(typeof responses)[number]> => !!response) ||
    !canonicalStrings(responses.map(squareReleaseRouteCheckResponseKey)) ||
    responses.some(response => response.resource.side !== value.checked_side) ||
    !controlledKingDestinations.every(
      (destination): destination is NonNullable<(typeof controlledKingDestinations)[number]> => !!destination,
    ) ||
    !canonicalStrings(controlledKingDestinations.map(destination => destination.destination)) ||
    (value.terminal_state === 'checkmate') !== (responses.length === 0)
  )
    return;
  return {
    kind: 'created-check',
    assertionId: value.assertion_id as string,
    checkedSide: value.checked_side,
    kingSquare: value.king_square as Key,
    checkers,
    responses,
    controlledKingDestinations,
    terminalState: value.terminal_state,
  };
}

function projectSquareReleaseRouteCheckResponse(
  value: unknown,
): Extract<MoveReviewSquareReleaseRouteTerminal, { kind: 'created-check' }>['responses'][number] | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['resource', 'modes']) ||
    !Array.isArray(value.modes) ||
    value.modes.length < 1 ||
    !value.modes.every(mode => mode === 'capture_checker' || mode === 'interpose' || mode === 'king_move') ||
    !canonicalStrings(value.modes)
  )
    return;
  const resource = projectSquareReleaseRouteResource(value.resource);
  return resource
    ? {
        resource,
        modes: value.modes as ('capture_checker' | 'interpose' | 'king_move')[],
      }
    : undefined;
}

function projectSquareReleaseRouteResource(value: unknown): MoveReviewSquareReleaseRouteResource | undefined {
  if (
    !isObject(value) ||
    !hasOnlyKeys(
      value,
      ['side', 'from', 'to', 'piece_before', 'piece_after', 'move_uci', 'capture'],
      ['side', 'from', 'to', 'piece_before', 'piece_after', 'move_uci'],
    )
  )
    return;
  const actor = projectTypedActor({
    side: value.side,
    from: value.from,
    to: value.to,
    piece_before: value.piece_before,
    piece_after: value.piece_after,
  });
  const moveUci = uci(value.move_uci);
  const hasCapture = Object.prototype.hasOwnProperty.call(value, 'capture');
  const capture = hasCapture ? projectColoredPiece(value.capture) : undefined;
  return actor && moveUci && typedActorMatchesMove(actor, moveUci) && (!hasCapture || capture)
    ? { ...actor, moveUci, ...(capture ? { capture } : {}) }
    : undefined;
}

function projectPieceWitness(value: unknown): MoveReviewPieceWitness | undefined {
  return isObject(value) &&
    hasExactKeys(value, ['piece', 'square']) &&
    pieceRole(value.piece) &&
    key(value.square)
    ? { piece: value.piece as MoveReviewPieceRole, square: value.square as Key }
    : undefined;
}

function pieceWitnessKey(piece: MoveReviewPieceWitness): string {
  return `${piece.square}:${piece.piece}`;
}

function squareReleaseRouteResourceKey(resource: MoveReviewSquareReleaseRouteResource): string {
  const capture = resource.capture
    ? `${resource.capture.side}:${resource.capture.piece}@${resource.capture.square}`
    : 'quiet';
  return `${resource.side}:${resource.from}:${resource.to}:${resource.pieceBefore}:${resource.pieceAfter}:${resource.moveUci}:${capture}`;
}

function squareReleaseRouteCheckResponseKey(
  response: Extract<MoveReviewSquareReleaseRouteTerminal, { kind: 'created-check' }>['responses'][number],
): string {
  return `${squareReleaseRouteResourceKey(response.resource)}:${response.modes.join(',')}`;
}

function sameSquareReleaseRouteResource(
  left: MoveReviewSquareReleaseRouteResource,
  right: MoveReviewSquareReleaseRouteResource,
): boolean {
  return (
    left.moveUci === right.moveUci &&
    sameTypedActor(left, right) &&
    ((!left.capture && !right.capture) || sameColoredPiece(left.capture, right.capture))
  );
}

function sameColoredPiece(
  left: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key } | undefined,
  right: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key } | undefined,
): boolean {
  return (
    !!left &&
    !!right &&
    left.side === right.side &&
    left.piece === right.piece &&
    left.square === right.square
  );
}

function projectLegalMovePremise(
  value: unknown,
  branch: MoveReviewWireBranch,
): MoveReviewLegalMovePremise | undefined {
  if (
    !isObject(value) ||
    !hasOnlyKeys(
      value,
      [
        'role',
        'contract',
        'move_uci',
        'movement',
        'movement_mode',
        'legal_move_semantic_id',
        'issuer_evidence_id',
        'issuer_occurrence_id',
        'source_premise_ids',
        'branch_id',
        'branch_role',
        'step_index',
        'capture',
      ],
      [
        'role',
        'contract',
        'move_uci',
        'movement',
        'movement_mode',
        'legal_move_semantic_id',
        'issuer_evidence_id',
        'issuer_occurrence_id',
        'source_premise_ids',
        'branch_id',
        'branch_role',
        'step_index',
      ],
    ) ||
    !nonEmptyWireString(value.role) ||
    value.contract !== 'legal_move' ||
    !typedHash(value.legal_move_semantic_id) ||
    !nonEmptyWireString(value.issuer_evidence_id) ||
    !typedHash(value.issuer_occurrence_id) ||
    !canonicalWireStrings(value.source_premise_ids, 3) ||
    !(value.source_premise_ids as string[]).includes(value.issuer_evidence_id as string) ||
    !(value.source_premise_ids as string[]).includes(value.issuer_occurrence_id as string) ||
    !(value.source_premise_ids as string[]).includes(`legal-move:${value.legal_move_semantic_id}`) ||
    value.branch_id !== branch.id ||
    value.branch_role !== branch.role ||
    !legalMovementMode(value.movement_mode)
  )
    return;
  const move = uci(value.move_uci);
  const movement = projectTypedActor(value.movement);
  const stepIndex = nonNegativeInteger(value.step_index);
  const capture = Object.prototype.hasOwnProperty.call(value, 'capture')
    ? projectColoredPiece(value.capture)
    : undefined;
  if (
    !move ||
    !movement ||
    stepIndex === undefined ||
    branch.steps[stepIndex]?.move !== move ||
    !typedActorMatchesMove(movement, move) ||
    (Object.prototype.hasOwnProperty.call(value, 'capture') && !capture)
  )
    return;
  return {
    role: value.role as string,
    contract: 'legal_move',
    moveUci: move,
    movement,
    movementMode: value.movement_mode,
    legalMoveSemanticId: value.legal_move_semantic_id as string,
    ...(capture ? { capture } : {}),
    issuerEvidenceId: value.issuer_evidence_id as string,
    issuerOccurrenceId: value.issuer_occurrence_id as string,
    sourcePremiseIds: [...value.source_premise_ids],
    branchId: branch.id,
    branchRole: branch.role,
    stepIndex,
  };
}

function legalMovementMode(value: unknown): value is MoveReviewLegalMovePremise['movementMode'] {
  return (
    value === 'controlled_destination' ||
    value === 'pawn_advance' ||
    value === 'pawn_double_advance' ||
    value === 'castling'
  );
}

function sameTypedActor(left: MoveReviewTypedActor, right: MoveReviewTypedActor): boolean {
  return (
    left.side === right.side &&
    left.from === right.from &&
    left.to === right.to &&
    left.pieceBefore === right.pieceBefore &&
    left.pieceAfter === right.pieceAfter
  );
}

function typedActorMatchesMove(actor: MoveReviewTypedActor, move: Uci | undefined): boolean {
  if (!move || move.slice(0, 2) !== actor.from || move.slice(2, 4) !== actor.to) return false;
  const promotion =
    move[4] === 'q'
      ? 'queen'
      : move[4] === 'r'
        ? 'rook'
        : move[4] === 'b'
          ? 'bishop'
          : move[4] === 'n'
            ? 'knight'
            : undefined;
  return move.length === 5
    ? actor.pieceBefore === 'pawn' && actor.pieceAfter === promotion
    : actor.pieceBefore === actor.pieceAfter;
}

function matchesTypedPremiseOccurrence(
  premise: MoveReviewTypedPremise,
  role: string,
  contract: string,
  branch: MoveReviewWireBranch,
  stepIndex: number,
): boolean {
  return (
    premise.role === role &&
    premise.contract === contract &&
    premise.branchId === branch.id &&
    premise.branchRole === branch.role &&
    premise.stepIndex === stepIndex
  );
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

function projectUniqueCheckReplyDefenderDisplacementBeforeCapturePath(
  value: unknown,
  reference: MoveReviewWireBranch,
  played: MoveReviewWireBranch,
  capturedTarget: { side: 'white' | 'black'; piece: MoveReviewPieceRole; square: Key },
): MoveReviewUniqueCheckReplyDefenderDisplacementBeforeCapturePath | undefined {
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
  const premises = value.premises.map(premise => projectTypedPremise(premise, [reference, played]));
  if (
    !premises.every((premise): premise is MoveReviewTypedPremise => !!premise) ||
    !unique(premises.map(premise => premise.resultId)) ||
    !unique(premises.map(premise => premise.issuerOccurrenceId!)) ||
    !matchesTypedPremiseOccurrence(
      premises[0]!,
      'created_check_response',
      'created_check_response_inventory',
      reference,
      0,
    ) ||
    !matchesTypedPremiseOccurrence(
      premises[1]!,
      'reference_capture_recapture',
      'capture_recapture_inventory',
      reference,
      2,
    ) ||
    !matchesTypedPremiseOccurrence(
      premises[2]!,
      'played_capture_recapture',
      'capture_recapture_inventory',
      played,
      0,
    )
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
    absence.branch_id !== reference.id ||
    absence.branch_role !== 'counterfactual_reference' ||
    absence.after_step_index !== 2 ||
    absence.query !== `legal-capture:${capturedTarget.side}:${capturedTarget.square}`
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
    position.scope !== 'best_line' ||
    fen !== reference.steps[2]?.fenAfter ||
    ply !== reference.steps[2]?.ply
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

function projectUniqueCheckReplyDefenderDisplacementBeforeCaptureParticipants(value: unknown):
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
  const actor: MoveReviewTypedActor = {
    side: value.side,
    from: value.from as Key,
    to: value.to as Key,
    pieceBefore: value.piece_before as MoveReviewPieceRole,
    pieceAfter: value.piece_after as MoveReviewPieceRole,
    ...(move ? { moveUci: move } : {}),
    ...(legalRelation ? { legalMoveRelation: legalRelation } : {}),
  };
  return move && !typedActorMatchesMove(actor, move) ? undefined : actor;
}

function projectTypedPremise(
  value: unknown,
  branches: readonly MoveReviewWireBranch[],
): MoveReviewTypedPremise | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'role',
      'contract',
      'result_id',
      'issuer_evidence_id',
      'issuer_occurrence_id',
      'source_premise_ids',
      'branch_id',
      'branch_role',
      'step_index',
    ]) ||
    !nonEmptyWireString(value.role) ||
    (value.contract !== 'created_check_response_inventory' &&
      value.contract !== 'capture_recapture_inventory') ||
    typeof value.result_id !== 'string' ||
    !value.result_id.startsWith(`${value.contract}:`) ||
    !typedHash(value.result_id.slice(value.contract.length + 1)) ||
    !nonEmptyWireString(value.issuer_evidence_id) ||
    !typedHash(value.issuer_occurrence_id) ||
    !nonEmptyWireString(value.branch_role) ||
    !canonicalWireStrings(value.source_premise_ids, 3) ||
    !(value.source_premise_ids as string[]).includes(value.issuer_evidence_id as string) ||
    !(value.source_premise_ids as string[]).includes(value.issuer_occurrence_id as string)
  )
    return;
  const branch = branches.find(candidate => candidate.id === value.branch_id);
  const stepIndex = nonNegativeInteger(value.step_index);
  return !branch || branch.role !== value.branch_role || stepIndex === undefined || !branch.steps[stepIndex]
    ? undefined
    : {
        role: value.role as string,
        contract: value.contract as string,
        resultId: value.result_id as string,
        issuerEvidenceId: value.issuer_evidence_id as string,
        issuerOccurrenceId: value.issuer_occurrence_id as string,
        sourcePremiseIds: [...value.source_premise_ids],
        branchId: branch.id,
        branchRole: value.branch_role as string,
        stepIndex,
      };
}

function projectPassedPawnProgressRealizedAfterOnlyLegalReply(
  channel: Record<string, unknown>,
  causeEvidenceId: string,
  candidateMove: Uci,
  startFen: FEN,
): MoveReviewReason[] | undefined {
  const wire = typedChannelProof(channel, 'passed_pawn_progress_realized_after_only_legal_reply_proof');
  if (!wire) return;
  if (
    !hasExactKeys(wire, [
      'source_evidence_id',
      'event_evidence_id',
      'semantic_id',
      'occurrence_id',
      'dependency_fingerprint',
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
    !nonEmptyWireString(wire.source_evidence_id) ||
    !nonEmptyWireString(wire.event_evidence_id) ||
    !typedHash(wire.semantic_id) ||
    !typedHash(wire.occurrence_id) ||
    !typedHash(wire.dependency_fingerprint) ||
    !canonicalWireStrings(wire.result_target_subjects, 1, passedPawnResultSubject) ||
    !canonicalWireStrings(wire.lower_premise_ids, 1) ||
    !Array.isArray(wire.branches) ||
    wire.branches.length !== 1 ||
    !Array.isArray(wire.proof_paths) ||
    wire.proof_paths.length < 1
  )
    return;
  const rootMove = uci(wire.root_move);
  const realizingMove = uci(wire.realizing_move);
  const rootActor = projectTypedActor(wire.root_actor, false, true);
  const realizingActor = projectTypedActor(wire.realizing_actor, false, true);
  const rootLine = projectTypedLine(wire.root_line);
  const rootPly = nonNegativeInteger(wire.root_ply);
  const realizingPly = nonNegativeInteger(wire.realizing_ply);
  const resultPlyOffset = nonNegativeInteger(wire.result_ply_offset);
  const inventory = projectPassedPawnProgressRealizedAfterOnlyLegalReplyInventory(
    wire.closed_legal_reply_inventory,
  );
  if (
    !rootMove ||
    rootMove !== candidateMove ||
    !realizingMove ||
    !rootActor ||
    !realizingActor ||
    !typedActorMatchesMove(rootActor, rootMove) ||
    !typedActorMatchesMove(realizingActor, realizingMove) ||
    !rootLine ||
    rootLine.role !== 'played' ||
    rootLine.rootMove !== rootMove ||
    inventory?.rootAfter.scope !== 'played_transition' ||
    rootPly === undefined ||
    rootPly < 1 ||
    realizingPly === undefined ||
    resultPlyOffset === undefined ||
    resultPlyOffset < 1 ||
    !inventory ||
    !(wire.lower_premise_ids as string[]).includes(wire.event_evidence_id as string) ||
    !(wire.lower_premise_ids as string[]).includes(inventory.issuerEvidenceId)
  )
    return;
  const branches = wire.branches.map(branch => projectTypedBranch(branch, undefined, startFen));
  if (
    !branches.every(
      (branch): branch is MoveReviewPassedPawnProgressWireBranch =>
        !!branch &&
        branch.role === 'played_root_analysis_continuation' &&
        branch.provenance === 'observed_game_root' &&
        branch.lineRole === 'played' &&
        branch.steps.every(isPassedPawnResultWireStep),
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
  const analysisContinuation = branches[0];
  const continuationLast = analysisContinuation?.steps[analysisContinuation.steps.length - 1];
  if (
    !analysisContinuation ||
    analysisContinuation.role !== 'played_root_analysis_continuation' ||
    !analysisContinuation.replyMove ||
    !analysisContinuation.sourceOccurrenceId ||
    analysisContinuation.steps.length < 2 ||
    analysisContinuation.steps[0]?.ply !== rootPly ||
    continuationLast?.move !== realizingMove ||
    continuationLast.ply !== realizingPly ||
    realizingPly - rootPly !== resultPlyOffset ||
    analysisContinuation.id !== inventory.analysisContinuationBranchId ||
    analysisContinuation.replyMove !== inventory.legalReplyMove ||
    analysisContinuation.steps[1]?.move !== inventory.legalReplyMove ||
    analysisContinuation.steps[0]?.fenAfter !== inventory.rootAfter.fen ||
    analysisContinuation.steps[0]?.ply !== inventory.rootAfter.ply
  )
    return;
  const branchById = new Map(branches.map(branch => [branch.id, branch]));
  const paths = wire.proof_paths.map(path =>
    projectPassedPawnProgressRealizedAfterOnlyLegalReplyPath(path, branchById),
  );
  if (
    !paths.every((path): path is MoveReviewPassedPawnProgressRealizedAfterOnlyLegalReplyPath => !!path) ||
    !canonicalStrings(paths.map(path => path.id)) ||
    !unique(paths.flatMap(path => path.closureUseIds)) ||
    paths.some(
      path =>
        path.branch.id !== analysisContinuation.id ||
        path.realizationMove !== realizingMove ||
        path.realizationPly !== realizingPly ||
        !sameTypedActor(path.realizationActor, realizingActor),
    )
  )
    return;
  return paths.map(path => {
    const proof = proofFromWireSteps(`cause:${path.id}:${path.branch.id}`, startFen, path.branch.steps)!;
    return {
      id: proof.id,
      messageSlots: { candidateUci: candidateMove },
      message: {
        kind: 'passed-pawn-progress-realized-after-only-legal-reply' as const,
        channelId: channel.channel_id as string,
        causeEvidenceId,
        causeKind: 'passed_pawn_progress' as const,
        sourceEvidenceId: wire.source_evidence_id as string,
        eventEvidenceId: wire.event_evidence_id as string,
        semanticId: wire.semantic_id as string,
        occurrenceId: wire.occurrence_id as string,
        dependencyFingerprint: wire.dependency_fingerprint as string,
        pathOccurrenceId: path.id,
        resultTargetSubjects: [...(wire.result_target_subjects as string[])],
        rootActor,
        realizingActor,
        rootLine: { ...rootLine, role: 'played' as const },
        rootMove,
        rootPly,
        replyMove: path.branch.replyMove,
        realizingMove,
        realizingPly,
        resultPlyOffset,
        pathRealizationActor: path.realizationActor,
        pathRealizationMove: path.realizationMove,
        pathRealizationPly: path.realizationPly,
        analysisContinuationBranch: wireBranchIdentity(path.branch),
        analysisContinuationSteps: path.branch.steps,
        premises: path.premises,
        closureUseIds: path.closureUseIds,
        lowerPremiseIds: [...(wire.lower_premise_ids as string[])],
        replyClosure: {
          issuerEvidenceId: inventory.issuerEvidenceId,
          rootAfter: inventory.rootAfter,
          legalReplyMove: inventory.legalReplyMove,
          analysisContinuationBranchId: inventory.analysisContinuationBranchId,
        },
      },
      proof,
    };
  });
}

function projectPassedPawnProgressRealizedAfterOnlyLegalReplyInventory(value: unknown):
  | {
      issuerEvidenceId: string;
      rootAfter: { fen: FEN; ply: number; scope: 'played_transition' };
      legalReplyMove: Uci;
      analysisContinuationBranchId: string;
    }
  | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'issuer_evidence_id',
      'root_after',
      'legal_reply_move',
      'analysis_continuation_branch_id',
    ]) ||
    !nonEmptyWireString(value.issuer_evidence_id) ||
    !isObject(value.root_after) ||
    !hasExactKeys(value.root_after, ['fen', 'ply', 'scope'])
  )
    return;
  const rootAfterFen = fenText(value.root_after.fen);
  const rootAfterPly = nonNegativeInteger(value.root_after.ply);
  const rootAfterScope = value.root_after.scope;
  const legalReplyMove = uci(value.legal_reply_move);
  const analysisContinuationBranchId = typedHash(value.analysis_continuation_branch_id);
  return !rootAfterFen ||
    rootAfterPly === undefined ||
    rootAfterPly < 1 ||
    rootAfterScope !== 'played_transition' ||
    !legalReplyMove ||
    !analysisContinuationBranchId
    ? undefined
    : {
        issuerEvidenceId: value.issuer_evidence_id as string,
        rootAfter: { fen: rootAfterFen, ply: rootAfterPly, scope: 'played_transition' },
        legalReplyMove,
        analysisContinuationBranchId,
      };
}

function projectTypedBranch(
  value: unknown,
  boundedCausalRole: 'counterfactual_reference' | 'played_root_analysis_continuation' | undefined,
  startFen: FEN,
  variableBoundedLength = false,
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
          ['branch_id', 'role', 'reply_move', 'source_occurrence_id', 'line', 'root_provenance', 'steps'],
          ['branch_id', 'role', 'reply_move', 'source_occurrence_id', 'line', 'root_provenance', 'steps'],
        )) ||
    !typedHash(value.branch_id) ||
    (value.root_provenance !== 'counterfactual_analyzed_root' &&
      value.root_provenance !== 'observed_game_root') ||
    !Array.isArray(value.steps)
  )
    return;
  const line = projectTypedLine(
    boundedCausal
      ? {
          line_id: value.line_id,
          line_role: value.line_role,
          line_rank: value.line_rank,
          root_move: value.root_move,
        }
      : value.line,
  );
  if (!line || (!boundedCausal && line.role !== 'played')) return;
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
    const lengthIsExact = variableBoundedLength
      ? steps.length >= (reference ? 3 : 2)
      : steps.length === (reference ? 3 : 2);
    if (
      value.branch_role !== boundedCausalRole ||
      line.role !== (reference ? 'best_reference' : 'played') ||
      value.root_provenance !== (reference ? 'counterfactual_analyzed_root' : 'observed_game_root') ||
      !lengthIsExact ||
      !wireStepsAreContinuous(steps, startFen) ||
      (reference
        ? steps.some(step => step.provenance !== 'certified_analysis_move')
        : steps[0]?.provenance !== 'observed_game_move' ||
          steps.slice(1).some(step => step.provenance !== 'certified_analysis_move'))
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
  if (value.role !== 'played_root_analysis_continuation') return;
  const occurrenceSteps = steps as MoveReviewPassedPawnProgressWireStep[];
  if (
    !unique(occurrenceSteps.map(step => step.stepKey)) ||
    value.root_provenance !== 'observed_game_root' ||
    occurrenceSteps[0]?.provenance !== 'observed_game_move' ||
    occurrenceSteps.slice(1).some(step => step.provenance !== 'certified_analysis_move') ||
    occurrenceSteps.some(step => !samePassedPawnResultLine(step.line, line)) ||
    occurrenceSteps
      .slice(1)
      .some(
        (step, index) =>
          step.ply !== occurrenceSteps[index]!.ply + 1 || step.fenBefore !== occurrenceSteps[index]!.fenAfter,
      )
  )
    return;
  const replyMove = uci(value.reply_move);
  const sourceOccurrenceId = typedHash(value.source_occurrence_id);
  if (
    !replyMove ||
    !sourceOccurrenceId ||
    occurrenceSteps[1]?.move !== replyMove ||
    !wireStepsAreContinuous(occurrenceSteps, startFen)
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
    sourceOccurrenceId,
  };
}

function projectPassedPawnProgressRealizedAfterOnlyLegalReplyPath(
  value: unknown,
  branches: Map<string, MoveReviewPassedPawnProgressWireBranch>,
): MoveReviewPassedPawnProgressRealizedAfterOnlyLegalReplyPath | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'path_occurrence_id',
      'analysis_continuation_branch_id',
      'realization_actor',
      'realization_move',
      'realization_ply',
      'premises',
      'closure_use_ids',
    ]) ||
    !typedHash(value.path_occurrence_id) ||
    !typedHash(value.analysis_continuation_branch_id) ||
    !Array.isArray(value.premises) ||
    !validWireStrings(value.closure_use_ids, 1, typedHash)
  )
    return;
  const branch = branches.get(value.analysis_continuation_branch_id as string);
  const realizationActor = projectTypedActor(value.realization_actor, false, true);
  const realizationMove = uci(value.realization_move);
  const realizationPly = nonNegativeInteger(value.realization_ply);
  const premises = value.premises.map(premise => projectPassedPawnProgressPremise(premise, branches));
  const realizationStepIndices =
    branch?.steps.flatMap((step, index) =>
      step.move === realizationMove && step.ply === realizationPly ? [index] : [],
    ) ?? [];
  if (
    !branch?.replyMove ||
    branch.role !== 'played_root_analysis_continuation' ||
    !realizationActor ||
    !realizationMove ||
    !typedActorMatchesMove(realizationActor, realizationMove) ||
    realizationPly === undefined ||
    realizationStepIndices.length !== 1 ||
    !premises.every((premise): premise is MoveReviewTypedPremise => !!premise) ||
    !passedPawnPremisesBelongToAnalysisContinuation(premises, branch)
  )
    return;
  return {
    id: value.path_occurrence_id as string,
    branch: branch as MoveReviewPassedPawnProgressWireBranch & { replyMove: Uci },
    realizationActor,
    realizationMove,
    realizationPly,
    premises,
    closureUseIds: [...value.closure_use_ids],
  };
}

function projectPassedPawnProgressPremise(
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
        'from_step_index',
        'to_step_index',
      ],
    ) ||
    (value.role !== 'dependency' && value.role !== 'result') ||
    (value.lower_kind !== 'passed_pawn_progress_dependency' && value.lower_kind !== 'passed_pawn_progress') ||
    !nonEmptyWireString(value.lower_semantic_key) ||
    !typedHash(value.branch_id) ||
    branches.get(value.branch_id as string)?.role !== value.branch_role ||
    value.branch_role !== 'played_root_analysis_continuation' ||
    !canonicalWireStrings(value.source_premise_ids, 1)
  )
    return;
  const hasDependencyProof = Object.prototype.hasOwnProperty.call(value, 'dependency_proof');
  const dependencyProof = hasDependencyProof
    ? projectPassedPawnProgressDependencyProof(value.dependency_proof)
    : undefined;
  if (hasDependencyProof && !dependencyProof) return;
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
        fromStepIndex: from,
        toStepIndex: to,
        ...(dependencyProof ? { dependencyProof } : {}),
      };
}

function projectPassedPawnProgressDependencyProof(
  value: unknown,
): MoveReviewPassedPawnDependencyProof | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'dependency_kind',
      'proof_kind',
      'squares',
      'pieces',
      'relation_issuers',
      'position_state_issuers',
    ]) ||
    !Array.isArray(value.squares) ||
    !Array.isArray(value.pieces) ||
    !Array.isArray(value.relation_issuers) ||
    !Array.isArray(value.position_state_issuers)
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
        'result_key',
        'occurrence_id',
        'step_key',
        'source_premise_ids',
        'issuer_evidence_id',
        'line',
        'scope',
      ])
    )
      return;
    const contract = passedPawnRelationKind(issuer.contract);
    const line = projectTypedLine(issuer.line);
    if (
      !contract ||
      typeof issuer.result_key !== 'string' ||
      !issuer.result_key.startsWith(`${contract}:`) ||
      !typedHash(issuer.result_key.slice(contract.length + 1)) ||
      !typedHash(issuer.occurrence_id) ||
      !nonEmptyWireString(issuer.step_key) ||
      !canonicalWireStrings(issuer.source_premise_ids, 1) ||
      !nonEmptyWireString(issuer.issuer_evidence_id) ||
      !line ||
      line.role !== 'played' ||
      issuer.scope !== 'played_line'
    )
      return;
    return {
      contract,
      resultKey: issuer.result_key,
      occurrenceId: issuer.occurrence_id as string,
      stepKey: issuer.step_key as string,
      sourcePremiseIds: [...issuer.source_premise_ids],
      issuerEvidenceId: issuer.issuer_evidence_id as string,
      line: { ...line, role: 'played' as const },
      scope: 'played_line' as const,
    };
  });
  const positionStateIssuers = value.position_state_issuers.map(issuer => {
    if (!isObject(issuer)) return;
    const state = projectPassedPawnPositionState(issuer.state);
    const ply = nonNegativeInteger(issuer.ply);
    const move = uci(issuer.move_uci);
    const fenBefore = fenText(issuer.fen_before);
    const fenAfter = fenText(issuer.fen_after);
    if (
      !hasExactKeys(issuer, [
        'state',
        'semantic_proof_id',
        'issuer_evidence_id',
        'issuer_occurrence_id',
        'step_key',
        'ply',
        'move_uci',
        'fen_before',
        'fen_after',
        'line',
        'scope',
      ]) ||
      !state ||
      !typedHash(issuer.semantic_proof_id) ||
      !nonEmptyWireString(issuer.issuer_evidence_id) ||
      !typedHash(issuer.issuer_occurrence_id) ||
      !nonEmptyWireString(issuer.step_key) ||
      ply === undefined ||
      ply === 0 ||
      !move ||
      !fenBefore ||
      !fenAfter ||
      issuer.scope !== 'played_line'
    )
      return;
    const line = projectTypedLine(issuer.line);
    if (!line || line.role !== 'played' || issuer.step_key !== causalStepKey(ply, move, fenBefore, fenAfter))
      return;
    return {
      state,
      semanticProofId: issuer.semantic_proof_id as string,
      issuerEvidenceId: issuer.issuer_evidence_id as string,
      issuerOccurrenceId: issuer.issuer_occurrence_id as string,
      stepKey: issuer.step_key as string,
      ply,
      move,
      fenBefore,
      fenAfter,
      line: { ...line, role: 'played' as const },
      scope: 'played_line' as const,
    };
  });
  if (
    !dependencyKind ||
    !proofKind ||
    !squares.every((square): square is NonNullable<typeof square> => !!square) ||
    !pieces.every((piece): piece is NonNullable<typeof piece> => !!piece) ||
    !relationIssuers.every((issuer): issuer is NonNullable<typeof issuer> => !!issuer) ||
    !positionStateIssuers.every((issuer): issuer is NonNullable<typeof issuer> => !!issuer) ||
    !unique(squares.map(square => `${square.role}:${square.square}`)) ||
    !unique(pieces.map(piece => `${piece.role}:${piece.side}:${piece.piece}`))
  )
    return;
  return { dependencyKind, proofKind, squares, pieces, relationIssuers, positionStateIssuers };
}

function projectPassedPawnPositionState(value: unknown): MoveReviewPassedPawnPositionState | undefined {
  if (!isObject(value) || (value.side !== 'white' && value.side !== 'black') || !key(value.square)) return;
  const role = pieceRole(value.piece);
  if (value.kind === 'occupied_by')
    return hasExactKeys(value, ['kind', 'side', 'square', 'piece']) && role
      ? { kind: 'occupied_by', side: value.side, square: value.square as Key, piece: role }
      : undefined;
  if (value.kind === 'pawn_topology')
    return hasExactKeys(value, ['kind', 'side', 'square', 'passed']) && typeof value.passed === 'boolean'
      ? { kind: 'pawn_topology', side: value.side, square: value.square as Key, passed: value.passed }
      : undefined;
  if (
    value.kind !== 'slider_reach' ||
    !hasExactKeys(value, ['kind', 'side', 'square', 'piece', 'file_step', 'rank_step', 'segment']) ||
    (role !== 'bishop' && role !== 'rook' && role !== 'queen') ||
    !Number.isInteger(value.file_step) ||
    !Number.isInteger(value.rank_step) ||
    Math.abs(value.file_step as number) > 1 ||
    Math.abs(value.rank_step as number) > 1 ||
    (value.file_step === 0 && value.rank_step === 0) ||
    !Array.isArray(value.segment)
  )
    return;
  const segment = value.segment.map(item => {
    if (
      !isObject(item) ||
      !hasExactKeys(item, ['square', 'target', 'occupant_piece']) ||
      !key(item.square) ||
      (item.target !== 'empty' && item.target !== 'friendly' && item.target !== 'enemy')
    )
      return;
    const occupantPiece = pieceRole(item.occupant_piece);
    if (
      (item.target === 'empty') !== (item.occupant_piece === null) ||
      (item.target !== 'empty' && !occupantPiece)
    )
      return;
    return {
      square: item.square as Key,
      target: item.target as 'empty' | 'friendly' | 'enemy',
      ...(occupantPiece ? { occupantPiece } : {}),
    };
  });
  if (!segment.every((item): item is NonNullable<typeof item> => !!item)) return;
  return {
    kind: 'slider_reach',
    side: value.side,
    square: value.square as Key,
    piece: role,
    fileStep: value.file_step as number,
    rankStep: value.rank_step as number,
    segment,
  };
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

function passedPawnPremisesBelongToAnalysisContinuation(
  premises: MoveReviewTypedPremise[],
  analysisContinuation: MoveReviewPassedPawnProgressWireBranch,
): boolean {
  return premises.every(
    premise =>
      premise.branchId === analysisContinuation.id &&
      premise.branchRole === analysisContinuation.role &&
      premise.fromStepIndex !== undefined &&
      premise.toStepIndex !== undefined &&
      !!analysisContinuation.steps[premise.fromStepIndex] &&
      !!analysisContinuation.steps[premise.toStepIndex],
  );
}

function isPassedPawnResultWireStep(step: MoveReviewWireStep): step is MoveReviewPassedPawnProgressWireStep {
  return !!step.stepKey && step.line?.role === 'played';
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
      ? hasExactKeys(value, [...required, 'step_key', 'line'])
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
  const line = withOccurrenceProof ? projectTypedLine(value.line) : undefined;
  if (
    index === undefined ||
    ply === undefined ||
    !move ||
    !fenBefore ||
    !fenAfter ||
    (withOccurrenceProof && (!stepKey || !line || stepKey !== causalStepKey(ply, move, fenBefore, fenAfter)))
  )
    return;
  return {
    index,
    ply,
    move,
    fenBefore,
    fenAfter,
    provenance: value.provenance,
    ...(stepKey ? { stepKey } : {}),
    ...(line ? { line } : {}),
  };
}

function projectTypedLine(value: unknown): MoveReviewTypedLine | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['line_id', 'line_role', 'line_rank', 'root_move']) ||
    !nonEmptyWireString(value.line_id) ||
    (value.line_role === 'played' ||
      value.line_role === 'best_reference' ||
      value.line_role === 'alternative') === false ||
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

function proofFromWireSteps(
  id: string,
  startFen: FEN,
  steps: MoveReviewWireStep[],
): MoveReviewProof | undefined {
  if (!semanticId(id) || !wireStepsAreContinuous(steps, startFen)) return;
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

function wireBranchIdentity<Branch extends MoveReviewWireBranch>(
  branch: Branch,
): MoveReviewTypedBranch & {
  role: Branch['role'];
  provenance: Branch['provenance'];
  lineRole: Branch['lineRole'];
} {
  return {
    id: branch.id,
    role: branch.role,
    provenance: branch.provenance,
    lineId: branch.lineId,
    lineRole: branch.lineRole,
    lineRank: branch.lineRank,
    rootMove: branch.rootMove,
    ...(branch.sourceOccurrenceId ? { sourceOccurrenceId: branch.sourceOccurrenceId } : {}),
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
  return nonEmptyWireString(value);
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
const squarePattern = /^[a-h][1-8]$/;
const uciPattern = /^[a-h][1-8][a-h][1-8][qrbn]?$/;
