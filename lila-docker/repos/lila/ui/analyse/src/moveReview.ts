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

export type MoveReviewProofKind =
  | 'unique_check_reply_defender_displacement_before_capture'
  | 'sole_recapturer_removal_before_target_capture'
  | 'vacated_gate_enables_unrecapturable_slider_capture'
  | 'square_release_route'
  | 'capture_exclusion_move_order'
  | 'relocation_enables_recapture'
  | 'passed_pawn_progress_realized_after_only_legal_reply';

export type MoveReviewRootProvenance = 'counterfactual_analyzed_root' | 'observed_game_root';
export type MoveReviewStepProvenance = 'observed_game_move' | 'certified_analysis_move';
export type MoveReviewPieceRole = 'pawn' | 'knight' | 'bishop' | 'rook' | 'queen' | 'king';
export type MoveReviewColor = 'white' | 'black';

export interface MoveReviewOccurrencePosition {
  fen: FEN;
  ply: number;
}

export interface MoveReviewSubjectOccurrence {
  occurrenceId: string;
  lineOwnerEvidenceId: string;
  transitionEvidenceId: string;
  moveUci: Uci;
  rootProvenance: 'observed_game_root';
  lineId: string;
  start: MoveReviewOccurrencePosition;
  destination: MoveReviewOccurrencePosition;
}

export interface MoveReviewOccurrenceStep {
  stepIndex: number;
  provenance: MoveReviewStepProvenance;
  ply: number;
  moveUci: Uci;
  fenBefore: FEN;
  fenAfter: FEN;
}

export type MoveReviewOccurrenceBranchRole =
  | 'displacement_then_capture'
  | 'immediate_capture_with_defender'
  | 'removal_then_target_capture'
  | 'immediate_target_capture'
  | 'gate_vacated_then_capture'
  | 'gate_retained'
  | 'released_square_route'
  | 'retained_blocker'
  | 'vacating_then_deferred'
  | 'immediate_deferred_capture'
  | 'relocated_responder'
  | 'retained_responder';

export interface MoveReviewOccurrenceBranch<
  Role extends MoveReviewOccurrenceBranchRole = MoveReviewOccurrenceBranchRole,
> {
  branchId: string;
  lineId: string;
  lineOwnerEvidenceId: string;
  rootTransitionEvidenceId: string;
  branchRole: Role;
  rootProvenance: MoveReviewRootProvenance;
  rootMove: Uci;
  steps: MoveReviewOccurrenceStep[];
}

export interface MoveReviewMovementWitness {
  side: MoveReviewColor;
  from: Key;
  to: Key;
  pieceBefore: MoveReviewPieceRole;
  pieceAfter: MoveReviewPieceRole;
}

export interface MoveReviewLegalResourceWitness extends MoveReviewMovementWitness {
  moveUci: Uci;
}

export interface MoveReviewPieceWitness {
  piece: MoveReviewPieceRole;
  square: Key;
}

export interface MoveReviewColoredPieceWitness extends MoveReviewPieceWitness {
  side: MoveReviewColor;
}

export interface MoveReviewRelationPremiseUse<
  Role extends string = string,
  Contract extends string = string,
  BranchRole extends MoveReviewOccurrenceBranchRole = MoveReviewOccurrenceBranchRole,
> {
  role: Role;
  contract: Contract;
  resultId: string;
  issuerEvidenceId: string;
  issuerOccurrenceId: string;
  sourcePremiseIds: string[];
  branchId: string;
  branchRole: BranchRole;
  stepIndex: number;
}

export type MoveReviewMovementMode =
  | 'controlled_destination'
  | 'pawn_advance'
  | 'pawn_double_advance'
  | 'castling';

export interface MoveReviewLegalMovePremiseUse<
  Role extends string = string,
  BranchRole extends MoveReviewOccurrenceBranchRole = MoveReviewOccurrenceBranchRole,
> {
  role: Role;
  contract: 'legal_move';
  moveUci: Uci;
  movement: MoveReviewMovementWitness;
  movementMode: MoveReviewMovementMode;
  legalMoveSemanticId: string;
  issuerEvidenceId: string;
  issuerOccurrenceId: string;
  sourcePremiseIds: string[];
  branchId: string;
  branchRole: BranchRole;
  stepIndex: number;
  capture?: MoveReviewColoredPieceWitness;
}

type MoveReviewRelocationContinuityRole =
  | 'relocated_branch_target_continuity'
  | 'retained_branch_target_continuity'
  | 'relocated_responder_continuity'
  | 'retained_responder_continuity'
  | 'relocated_branch_attacker_continuity'
  | 'retained_branch_attacker_continuity';

interface MoveReviewObjectContinuityPremiseUse<
  Role extends string = string,
  BranchRole extends MoveReviewOccurrenceBranchRole = MoveReviewOccurrenceBranchRole,
> {
  role: Role;
  contract: 'object_continuity_step';
  transitionKind: 'retained' | 'primary' | 'secondary';
  overallMoveUci: Uci;
  before: MoveReviewColoredPieceWitness;
  after: MoveReviewColoredPieceWitness;
  selectedTransition?: MoveReviewMovementWitness;
  legalMoveSemanticId: string;
  transitionFootprintId: string;
  issuerEvidenceId: string;
  issuerOccurrenceId: string;
  sourcePremiseIds: string[];
  branchId: string;
  branchRole: BranchRole;
  stepIndex: number;
}

type MoveReviewRelocationContinuityPremiseUse = MoveReviewObjectContinuityPremiseUse<
  MoveReviewRelocationContinuityRole,
  'relocated_responder' | 'retained_responder'
>;

export type MoveReviewClosureIssuer =
  | 'position_relation_extractor.closed_relation_inventory'
  | 'position_relation_extractor.closed_position_state_inventory';

export interface MoveReviewClosureUse<
  Role extends string = string,
  Issuer extends MoveReviewClosureIssuer = MoveReviewClosureIssuer,
  BranchRole extends MoveReviewOccurrenceBranchRole = MoveReviewOccurrenceBranchRole,
> {
  useId: string;
  role: Role;
  semanticProofId: string;
  issuer: Issuer;
  issuerEvidenceId: string;
  issuerOccurrenceId: string;
  query: string;
  branchId: string;
  branchRole: BranchRole;
  afterStepIndex: number;
  position: MoveReviewOccurrencePosition & { scope: 'legal_line' };
}

export interface MoveReviewBoundedProofPath<
  Premise extends
    | MoveReviewRelationPremiseUse
    | MoveReviewLegalMovePremiseUse
    | MoveReviewObjectContinuityPremiseUse =
    | MoveReviewRelationPremiseUse
    | MoveReviewLegalMovePremiseUse
    | MoveReviewObjectContinuityPremiseUse,
  Absence extends MoveReviewClosureUse = MoveReviewClosureUse,
  State extends MoveReviewClosureUse = MoveReviewClosureUse,
> {
  pathOccurrenceId: string;
  premises: Premise[];
  closedAbsenceUses: Absence[];
  closedStateUses: State[];
}

interface MoveReviewOccurrenceProofBase {
  sourceEvidenceId: string;
  semanticId: string;
  occurrenceId: string;
  dependencyFingerprint: string;
}

type MoveReviewUniquePremise =
  | MoveReviewRelationPremiseUse<
      'displacement_check_response',
      'created_check_response_inventory',
      'displacement_then_capture'
    >
  | MoveReviewRelationPremiseUse<
      'delayed_capture_recapture',
      'capture_recapture_inventory',
      'displacement_then_capture'
    >
  | MoveReviewRelationPremiseUse<
      'immediate_capture_recapture',
      'capture_recapture_inventory',
      'immediate_capture_with_defender'
    >;

export interface MoveReviewUniqueCheckReplyProof extends MoveReviewOccurrenceProofBase {
  displacementBranch: MoveReviewOccurrenceBranch<'displacement_then_capture'>;
  immediateCaptureBranch: MoveReviewOccurrenceBranch<'immediate_capture_with_defender'>;
  proofPaths: [MoveReviewBoundedProofPath<MoveReviewUniquePremise>];
  participants: {
    trigger: MoveReviewMovementWitness;
    forcedReply: MoveReviewLegalResourceWitness;
    realizer: MoveReviewMovementWitness;
    capturedTarget: MoveReviewColoredPieceWitness;
    immediateDefense: MoveReviewLegalResourceWitness;
    disabledDefender: MoveReviewColoredPieceWitness;
  };
  realizingMove: Uci;
  immediateCaptureBranchLegalDefenseMove: Uci;
}

type MoveReviewSolePremise =
  | MoveReviewRelationPremiseUse<
      'defender_removal',
      'capture_recapture_inventory',
      'removal_then_target_capture'
    >
  | MoveReviewRelationPremiseUse<
      'post_removal_target_capture_inventory',
      'capture_recapture_inventory',
      'removal_then_target_capture'
    >
  | MoveReviewRelationPremiseUse<
      'immediate_target_capture_inventory',
      'capture_recapture_inventory',
      'immediate_target_capture'
    >;

export interface MoveReviewSoleRecapturerProof extends MoveReviewOccurrenceProofBase {
  removalBranch: MoveReviewOccurrenceBranch<'removal_then_target_capture'>;
  immediateCaptureBranch: MoveReviewOccurrenceBranch<'immediate_target_capture'>;
  proofPaths: [MoveReviewBoundedProofPath<MoveReviewSolePremise>];
  participants: {
    remover: MoveReviewMovementWitness;
    removedDefender: MoveReviewColoredPieceWitness;
    removalRecapture: MoveReviewLegalResourceWitness;
    postRemovalTargetCapture: MoveReviewMovementWitness;
    capturedTarget: MoveReviewColoredPieceWitness;
    immediateSoleRecapture: MoveReviewLegalResourceWitness;
  };
  postRemovalTargetCaptureMove: Uci;
  immediateSoleRecaptureMove: Uci;
}

type MoveReviewVacatedGatePremise =
  | MoveReviewRelationPremiseUse<
      'gate_vacating_slider_reach',
      'slider_reach_delta',
      'gate_vacated_then_capture'
    >
  | MoveReviewRelationPremiseUse<
      'later_slider_capture',
      'capture_recapture_inventory',
      'gate_vacated_then_capture'
    >;

export interface MoveReviewVacatedGateProof extends MoveReviewOccurrenceProofBase {
  vacatedGateBranch: MoveReviewOccurrenceBranch<'gate_vacated_then_capture'>;
  retainedGateBranch: MoveReviewOccurrenceBranch<'gate_retained'>;
  proofPaths: [MoveReviewBoundedProofPath<MoveReviewVacatedGatePremise>];
  participants: {
    enabler: MoveReviewMovementWitness;
    slider: MoveReviewColoredPieceWitness;
    gateBlocker: MoveReviewColoredPieceWitness;
    exploit: MoveReviewMovementWitness;
    capturedTarget: MoveReviewColoredPieceWitness;
  };
  exploitMove: Uci;
}

export interface MoveReviewRouteResource extends MoveReviewLegalResourceWitness {
  capture?: MoveReviewColoredPieceWitness;
}

export interface MoveReviewSquareReleaseRouteStep extends MoveReviewMovementWitness {
  moveUci: Uci;
  stepIndex: number;
}

export type MoveReviewSquareReleaseTerminal =
  | { kind: 'occupation' }
  | {
      kind: 'capture';
      assertionId: string;
      capturedTarget: MoveReviewColoredPieceWitness;
      geometricRecapturers: MoveReviewPieceWitness[];
      legalRecaptures: MoveReviewRouteResource[];
      restrictedRecaptures: {
        piece: MoveReviewPieceWitness;
        destination: Key;
        kingSquare: Key;
        postMoveControllers: MoveReviewPieceWitness[];
      }[];
    }
  | {
      kind: 'created_check';
      assertionId: string;
      checkedSide: MoveReviewColor;
      kingSquare: Key;
      checkers: MoveReviewPieceWitness[];
      responses: {
        resource: MoveReviewRouteResource;
        modes: ('capture_checker' | 'interpose' | 'king_move')[];
      }[];
      controlledKingDestinations: { destination: Key; controllers: MoveReviewPieceWitness[] }[];
      terminalState: 'ongoing' | 'checkmate';
    };

type MoveReviewSquareReleasePremise =
  | MoveReviewRelationPremiseUse<
      'terminal_resource',
      'capture_recapture_inventory' | 'created_check_response_inventory',
      'released_square_route'
    >
  | MoveReviewLegalMovePremiseUse<string, 'released_square_route'>;

export interface MoveReviewSquareReleaseProof extends MoveReviewOccurrenceProofBase {
  releasedRouteBranch: MoveReviewOccurrenceBranch<'released_square_route'>;
  retainedBlockerBranch: MoveReviewOccurrenceBranch<'retained_blocker'>;
  proofPaths: MoveReviewBoundedProofPath<MoveReviewSquareReleasePremise>[];
  participants: {
    releaser: MoveReviewMovementWitness;
    releasedBlocker: MoveReviewColoredPieceWitness;
    routePiece: MoveReviewColoredPieceWitness;
  };
  route: MoveReviewSquareReleaseRouteStep[];
  terminalStepIndex: number;
  terminalReplyMove?: Uci;
  terminal: MoveReviewSquareReleaseTerminal;
}

type MoveReviewCaptureExclusionPremise = MoveReviewLegalMovePremiseUse<
  'vacating_move' | 'immediate_deferred_move' | 'immediate_capture_reply' | 'later_deferred_move',
  'vacating_then_deferred' | 'immediate_deferred_capture'
>;

export interface MoveReviewCaptureExclusionProof extends MoveReviewOccurrenceProofBase {
  vacatingBranch: MoveReviewOccurrenceBranch<'vacating_then_deferred'>;
  immediateCaptureBranch: MoveReviewOccurrenceBranch<'immediate_deferred_capture'>;
  proofPaths: [MoveReviewBoundedProofPath<MoveReviewCaptureExclusionPremise>];
  participants: {
    vacatingMove: MoveReviewMovementWitness;
    deferredMove: MoveReviewMovementWitness;
    captureReply: MoveReviewMovementWitness;
    capturedTarget: MoveReviewColoredPieceWitness;
  };
  laterDeferredStepIndex: number;
}

type MoveReviewRelocationRecapturePremise =
  | MoveReviewRelationPremiseUse<
      'relocated_recapture_inventory',
      'capture_recapture_inventory',
      'relocated_responder'
    >
  | MoveReviewRelationPremiseUse<
      'retained_recapture_inventory',
      'capture_recapture_inventory',
      'retained_responder'
    >
  | MoveReviewLegalMovePremiseUse<
      'relocated_target_capture' | 'relocated_responder_recapture',
      'relocated_responder'
    >
  | MoveReviewLegalMovePremiseUse<
      'retained_target_capture' | 'retained_other_recapture',
      'retained_responder'
    >
  | MoveReviewRelocationContinuityPremiseUse;

interface MoveReviewRelocationEnablesRecaptureProof extends MoveReviewOccurrenceProofBase {
  relocatedResponderBranch: MoveReviewOccurrenceBranch<'relocated_responder'>;
  retainedResponderBranch: MoveReviewOccurrenceBranch<'retained_responder'>;
  proofPaths: [MoveReviewBoundedProofPath<MoveReviewRelocationRecapturePremise>];
  participants: {
    attackerAtCommonRoot: MoveReviewColoredPieceWitness;
    attackerAtCapture: MoveReviewColoredPieceWitness;
    targetAtCommonRoot: MoveReviewColoredPieceWitness;
    capturedTarget: MoveReviewColoredPieceWitness;
    recaptureSquare: Key;
    trackedResponderAtSeed: MoveReviewColoredPieceWitness;
    trackedResponderAtStaging: MoveReviewColoredPieceWitness;
    otherRecapturer: MoveReviewColoredPieceWitness;
  };
  relocation: { movement: MoveReviewMovementWitness; moveUci: Uci; stepIndex: number };
  targetCapture: {
    movement: MoveReviewMovementWitness;
    moveUci: Uci;
    relocatedStepIndex: number;
    retainedStepIndex: number;
  };
  relocatedResponderRecapture: { movement: MoveReviewMovementWitness; moveUci: Uci; stepIndex: number };
  retainedOtherRecapture: { movement: MoveReviewMovementWitness; moveUci: Uci; stepIndex: number };
}

export interface MoveReviewPassedPawnLine {
  lineId: string;
  rootMove: Uci;
}

export interface MoveReviewPassedPawnActor extends MoveReviewMovementWitness {
  legalMoveRelation: string;
}

export interface MoveReviewPassedPawnStep {
  stepIndex: number;
  stepKey: string;
  ply: number;
  moveUci: Uci;
  fenBefore: FEN;
  fenAfter: FEN;
  line: MoveReviewPassedPawnLine;
  provenance: MoveReviewStepProvenance;
}

export interface MoveReviewPassedPawnBranch {
  branchId: string;
  branchRole: 'observed_root_with_analyzed_continuation';
  replyMove: Uci;
  sourceOccurrenceId: string;
  line: MoveReviewPassedPawnLine;
  lineOwnerEvidenceId: string;
  rootTransitionEvidenceId: string;
  rootProvenance: 'observed_game_root';
  steps: MoveReviewPassedPawnStep[];
}

export type MoveReviewPassedPawnDependencySquareRole =
  | 'root_from'
  | 'root_to'
  | 'future_from'
  | 'future_to'
  | 'vacated_gate'
  | 'enabled_from'
  | 'enabled_to'
  | 'reply_from'
  | 'reply_to'
  | 'follow_up_from'
  | 'follow_up_to'
  | 'released_passed_pawn';

export type MoveReviewPassedPawnDependencyPieceRole =
  | 'root_before'
  | 'tracked'
  | 'future_after'
  | 'enabled_piece'
  | 'trigger_pawn'
  | 'responder_pawn'
  | 'follow_up_pawn'
  | 'trigger_piece'
  | 'responder_piece'
  | 'follow_up_piece';

export type MoveReviewPassedPawnPositionState =
  | { kind: 'occupied_by'; side: MoveReviewColor; square: Key; piece: MoveReviewPieceRole }
  | {
      kind: 'slider_reach';
      side: MoveReviewColor;
      square: Key;
      piece: 'bishop' | 'rook' | 'queen';
      fileStep: number;
      rankStep: number;
      segment: (
        | { square: Key; target: 'empty'; occupantPiece: null }
        | { square: Key; target: 'friendly' | 'enemy'; occupantPiece: MoveReviewPieceRole }
      )[];
    }
  | { kind: 'pawn_topology'; side: MoveReviewColor; square: Key; passed: boolean };

export type MoveReviewPassedPawnRelationIssuer = {
  occurrenceId: string;
  stepKey: string;
  sourcePremiseIds: string[];
  issuerEvidenceId: string;
  line: MoveReviewPassedPawnLine;
  scope: 'legal_line';
} & (
  | { contract: 'slider_reach_delta'; resultKey: string }
  | { contract: 'pawn_topology_transition'; resultKey: string }
  | { contract: 'capture_recapture_inventory'; resultKey: string }
);

export interface MoveReviewPassedPawnPositionStateIssuer {
  state: MoveReviewPassedPawnPositionState;
  semanticProofId: string;
  issuerEvidenceId: string;
  issuerOccurrenceId: string;
  stepKey: string;
  ply: number;
  moveUci: Uci;
  fenBefore: FEN;
  fenAfter: FEN;
  line: MoveReviewPassedPawnLine;
  scope: 'legal_line';
}

interface MoveReviewPassedPawnDependencyPayload {
  squares: { role: MoveReviewPassedPawnDependencySquareRole; square: Key }[];
  pieces: {
    role: MoveReviewPassedPawnDependencyPieceRole;
    side: MoveReviewColor;
    piece: MoveReviewPieceRole;
  }[];
  relationIssuers: MoveReviewPassedPawnRelationIssuer[];
  positionStateIssuers: MoveReviewPassedPawnPositionStateIssuer[];
}

export type MoveReviewPassedPawnDependencyProof = MoveReviewPassedPawnDependencyPayload &
  (
    | { dependencyKind: 'object_state_precondition'; proofKind: 'object_state' }
    | { dependencyKind: 'line_access_precondition'; proofKind: 'line_access' }
    | {
        dependencyKind: 'response_continuation_precondition';
        proofKind: 'pawn_break_follow_up' | 'capture_follow_up';
      }
  );

interface MoveReviewPassedPawnPremiseCommon {
  lowerSemanticKey: string;
  sourcePremiseIds: string[];
  branchId: string;
  branchRole: 'observed_root_with_analyzed_continuation';
  fromStepIndex: number;
  toStepIndex: number;
}

export type MoveReviewPassedPawnPremise =
  | (MoveReviewPassedPawnPremiseCommon & {
      role: 'dependency';
      lowerKind: 'passed_pawn_progress_dependency';
      dependencyProof: MoveReviewPassedPawnDependencyProof;
    })
  | (MoveReviewPassedPawnPremiseCommon & {
      role: 'result';
      lowerKind: 'passed_pawn_progress';
    });

export interface MoveReviewPassedPawnProofPath {
  pathOccurrenceId: string;
  analysisContinuationBranchId: string;
  realizationActor: MoveReviewPassedPawnActor;
  realizationMove: Uci;
  realizationPly: number;
  premises: MoveReviewPassedPawnPremise[];
  closureUseIds: string[];
}

export interface MoveReviewPassedPawnProgressProof extends MoveReviewOccurrenceProofBase {
  eventEvidenceId: string;
  resultTargetSubjects: string[];
  rootActor: MoveReviewPassedPawnActor;
  realizingActor: MoveReviewPassedPawnActor;
  rootLine: MoveReviewPassedPawnLine;
  rootMove: Uci;
  rootPly: number;
  realizingMove: Uci;
  realizingPly: number;
  resultPlyOffset: number;
  closedLegalReplyInventory: {
    issuerEvidenceId: string;
    rootAfter: MoveReviewOccurrencePosition & { scope: 'played_transition' };
    legalReplyMove: Uci;
    analysisContinuationBranchId: string;
  };
  branches: [MoveReviewPassedPawnBranch];
  proofPaths: MoveReviewPassedPawnProofPath[];
  lowerPremiseIds: string[];
}

export type MoveReviewOccurrenceProof =
  | MoveReviewUniqueCheckReplyProof
  | MoveReviewSoleRecapturerProof
  | MoveReviewVacatedGateProof
  | MoveReviewSquareReleaseProof
  | MoveReviewCaptureExclusionProof
  | MoveReviewRelocationEnablesRecaptureProof
  | MoveReviewPassedPawnProgressProof;

interface MoveReviewOccurrenceExplanationBase {
  id: string;
  causeEvidenceId: string;
  subjectOccurrence: MoveReviewSubjectOccurrence;
}

export type MoveReviewOccurrenceExplanation = MoveReviewOccurrenceExplanationBase &
  (
    | {
        proofKind: 'unique_check_reply_defender_displacement_before_capture';
        proof: MoveReviewUniqueCheckReplyProof;
      }
    | {
        proofKind: 'sole_recapturer_removal_before_target_capture';
        proof: MoveReviewSoleRecapturerProof;
      }
    | {
        proofKind: 'vacated_gate_enables_unrecapturable_slider_capture';
        proof: MoveReviewVacatedGateProof;
      }
    | { proofKind: 'square_release_route'; proof: MoveReviewSquareReleaseProof }
    | { proofKind: 'capture_exclusion_move_order'; proof: MoveReviewCaptureExclusionProof }
    | {
        proofKind: 'relocation_enables_recapture';
        proof: MoveReviewRelocationEnablesRecaptureProof;
      }
    | {
        proofKind: 'passed_pawn_progress_realized_after_only_legal_reply';
        proof: MoveReviewPassedPawnProgressProof;
      }
  );

export type MoveReviewAnyProofPath = MoveReviewBoundedProofPath | MoveReviewPassedPawnProofPath;
export type MoveReviewAnyBranch = MoveReviewOccurrenceBranch | MoveReviewPassedPawnBranch;

export type MoveReviewCandidateReview =
  | {
      kind: 'move-verdict';
      core: MoveReviewCore;
      explanations: MoveReviewOccurrenceExplanation[];
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
  noVerifiedExplanation: string;
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
  proofPath: string;
  actualMove: string;
  analyzedAlternative: string;
  analysisContinuation: string;
  familyLabels: Record<MoveReviewProofKind, string>;
  structureLabels: {
    participants: string;
    laterConsumer: string;
    provenanceDetails: string;
    premise: string;
    move: string;
    side: string;
    piece: string;
    from: string;
    to: string;
    square: string;
    capture: string;
    afterStep: string;
    positionPly: string;
    contract: string;
    result: string;
    route: string;
    reply: string;
    yes: string;
    no: string;
    causeEvidenceId: string;
    proofOccurrenceId: string;
    subjectOccurrenceId: string;
    semanticId: string;
    sourceEvidenceId: string;
    dependencyFingerprint: string;
    branchId: string;
    lineId: string;
    pathOccurrenceId: string;
    closureUseId: string;
    pieceLabels: Record<MoveReviewPieceRole, string>;
    roleLabels: Record<string, string>;
    contractLabels: Record<string, string>;
  };
  premises: string;
  closedAbsence: string;
  closedState: string;
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
    noVerifiedExplanation: '판정은 가능하지만, 기준 수와의 차이를 설명할 원인은 검증되지 않았습니다.',
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
    proofPath: '근거 경로',
    actualMove: '실제 수',
    analyzedAlternative: '분석 대안',
    analysisContinuation: '분석 후속',
    familyLabels: {
      unique_check_reply_defender_displacement_before_capture: '유일 체크 응수로 수비수 이동 후 포획',
      sole_recapturer_removal_before_target_capture: '유일 재포획 기물 제거 후 목표 포획',
      vacated_gate_enables_unrecapturable_slider_capture: '관문 비움 후 재포획 불가 장거리 포획',
      square_release_route: '칸 해방 후 기물 경로',
      capture_exclusion_move_order: '포획 응수를 배제한 수순',
      relocation_enables_recapture: '기물 이동으로 가능해진 재포획',
      passed_pawn_progress_realized_after_only_legal_reply: '유일 합법 응수 후 통과폰 전진',
    },
    structureLabels: {
      participants: '관련 기물',
      laterConsumer: '후속 소비',
      provenanceDetails: '인증 출처 상세',
      premise: '전제',
      move: '수',
      side: '진영',
      piece: '기물',
      from: '출발 칸',
      to: '도착 칸',
      square: '칸',
      capture: '포획 대상',
      afterStep: '기준 단계',
      positionPly: '포지션 ply',
      contract: '인증 관계',
      result: '결과 근거',
      route: '경로',
      reply: '성립한 응수',
      yes: '예',
      no: '아니요',
      causeEvidenceId: '원인 근거 ID',
      proofOccurrenceId: '증명 occurrence ID',
      subjectOccurrenceId: '대상 occurrence ID',
      semanticId: 'semantic ID',
      sourceEvidenceId: '출처 근거 ID',
      dependencyFingerprint: '의존 지문',
      branchId: '분기 ID',
      lineId: '수순 ID',
      pathOccurrenceId: '경로 occurrence ID',
      closureUseId: '폐쇄 근거 ID',
      pieceLabels: {
        pawn: '폰',
        knight: '나이트',
        bishop: '비숍',
        rook: '룩',
        queen: '퀸',
        king: '킹',
      },
      roleLabels: {
        trigger: '시작 기물',
        forcedReply: '강제 응수',
        realizer: '후속 기물',
        capturedTarget: '포획 대상',
        immediateDefense: '즉시 방어',
        disabledDefender: '이동한 수비 기물',
        remover: '제거 기물',
        removedDefender: '제거된 수비 기물',
        removalRecapture: '제거 후 재포획',
        postRemovalTargetCapture: '후속 포획',
        immediateSoleRecapture: '즉시 재포획',
        enabler: '경로를 연 기물',
        slider: '장거리 기물',
        gateBlocker: '관문 차단 기물',
        exploit: '후속 기물',
        releaser: '칸을 비운 기물',
        releasedBlocker: '이동한 차단 기물',
        routePiece: '경로 기물',
        vacatingMove: '칸을 비운 기물',
        deferredMove: '후속 기물',
        captureReply: '포획 응수',
        attackerAtCommonRoot: '공통 시작의 포획 기물',
        attackerAtCapture: '포획 시점의 기물',
        targetAtCommonRoot: '공통 시작의 포획 대상',
        recaptureSquare: '재포획 칸',
        trackedResponderAtSeed: '이동 전 재포획 기물',
        trackedResponderAtStaging: '이동 후 재포획 기물',
        otherRecapturer: '다른 재포획 기물',
        rootActor: '실제 수 기물',
        realizingActor: '후속 기물',
        displacement_check_response: '체크 응수',
        delayed_capture_recapture: '후속 재포획',
        immediate_capture_recapture: '즉시 재포획',
        defender_removal: '수비 기물 제거',
        post_removal_target_capture_inventory: '제거 후 목표 포획',
        immediate_target_capture_inventory: '즉시 목표 포획',
        gate_vacating_slider_reach: '장거리 경로 개방',
        later_slider_capture: '후속 장거리 포획',
        terminal_resource: '종단 자원',
        release_move: '칸 해방 수',
        terminal_reply: '종단 응수',
        vacating_move: '칸 비움',
        immediate_deferred_move: '즉시 후속 수',
        immediate_capture_reply: '즉시 포획 응수',
        later_deferred_move: '나중 후속 수',
        relocated_recapture_inventory: '이동 기물 재포획 관계',
        retained_recapture_inventory: '유지선 재포획 관계',
        relocated_target_capture: '이동선 목표 포획',
        relocated_responder_recapture: '이동 기물 재포획',
        retained_target_capture: '유지선 목표 포획',
        retained_other_recapture: '유지선 다른 재포획',
        relocated_branch_target_continuity: '이동선 목표 기물 연속성',
        retained_branch_target_continuity: '유지선 목표 기물 연속성',
        relocated_responder_continuity: '이동 기물 연속성',
        retained_responder_continuity: '유지 기물 연속성',
        relocated_branch_attacker_continuity: '이동선 포획 기물 연속성',
        retained_branch_attacker_continuity: '유지선 포획 기물 연속성',
        retained: '제자리 유지',
        primary: '주 이동',
        secondary: '동반 이동',
        dependency: '의존 근거',
        result: '결과 근거',
        delayed_capture_recapture_absent: '후속 재포획 부재',
        post_removal_replacement_recapture_absent: '대체 재포획 부재',
        later_capture_immediate_recapture_absent: '즉시 재포획 부재',
        retained_gate_exploit_move_absent: '관문 유지 시 후속 수 부재',
        retained_gate_replacement_capture_absent: '관문 유지 시 대체 포획 부재',
        retained_blocker_first_route_leg_absent: '차단 유지 시 첫 경로 수 부재',
        capture_reply_absent: '포획 응수 부재',
        retained_seed_recapture_absent: '유지선 출발칸 재포획 부재',
        delayed_capture_actor_present: '후속 기물 유지',
        delayed_target_present: '목표 기물 유지',
        post_removal_exploit_actor_present: '후속 기물 유지',
        post_removal_target_present: '목표 기물 유지',
        vacated_gate_intervening_slider_reach: '관문 해방 경로',
        vacated_gate_target_persistence: '관문 해방 목표 유지',
        retained_gate_slider_persistence: '관문 유지 장거리 기물',
        retained_gate_target_persistence: '관문 유지 목표',
        retained_gate_blocker_persistence: '관문 차단 기물 유지',
        retained_gate_blocked_slider_reach: '관문 유지 차단 경로',
        released_square_vacancy: '해방된 칸 비움',
        retained_blocker_persistence: '차단 기물 유지',
        retained_route_origin_persistence: '경로 출발점 유지',
        vacated_target: '비워진 목표 칸',
        reply_actor: '응수 기물 유지',
        deferred_actor: '후속 기물 유지',
      },
      contractLabels: {
        created_check_response_inventory: '체크 응수',
        capture_recapture_inventory: '포획·재포획',
        slider_reach_delta: '장거리 경로 변화',
        legal_move: '합법 수',
        object_continuity_step: '기물 연속성',
        pawn_topology_transition: '폰 구조 변화',
      },
    },
    premises: '전제',
    closedAbsence: '폐쇄 부재',
    closedState: '폐쇄 상태',
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
    noVerifiedExplanation:
      'The verdict is available, but no cause for the difference from the reference move was verified.',
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
    proofPath: 'Proof path',
    actualMove: 'Actual move',
    analyzedAlternative: 'Analyzed alternative',
    analysisContinuation: 'Analysis continuation',
    familyLabels: {
      unique_check_reply_defender_displacement_before_capture:
        'Unique-check-reply defender displacement before capture',
      sole_recapturer_removal_before_target_capture: 'Sole-recapturer removal before target capture',
      vacated_gate_enables_unrecapturable_slider_capture:
        'Unrecapturable slider capture through a vacated gate',
      square_release_route: 'Route through a released square',
      capture_exclusion_move_order: 'Move order excluding the capture reply',
      relocation_enables_recapture: 'Recapture enabled by relocation',
      passed_pawn_progress_realized_after_only_legal_reply: 'Passed-pawn progress after the only legal reply',
    },
    structureLabels: {
      participants: 'Pieces and targets',
      laterConsumer: 'Later consumer',
      provenanceDetails: 'Certified provenance details',
      premise: 'Premise',
      move: 'Move',
      side: 'Side',
      piece: 'Piece',
      from: 'From',
      to: 'To',
      square: 'Square',
      capture: 'Captured target',
      afterStep: 'After step',
      positionPly: 'Position ply',
      contract: 'Certified relation',
      result: 'Result',
      route: 'Route',
      reply: 'Certified reply',
      yes: 'Yes',
      no: 'No',
      causeEvidenceId: 'Cause evidence ID',
      proofOccurrenceId: 'Proof occurrence ID',
      subjectOccurrenceId: 'Subject occurrence ID',
      semanticId: 'Semantic ID',
      sourceEvidenceId: 'Source evidence ID',
      dependencyFingerprint: 'Dependency fingerprint',
      branchId: 'Branch ID',
      lineId: 'Line ID',
      pathOccurrenceId: 'Path occurrence ID',
      closureUseId: 'Closure use ID',
      pieceLabels: {
        pawn: 'Pawn',
        knight: 'Knight',
        bishop: 'Bishop',
        rook: 'Rook',
        queen: 'Queen',
        king: 'King',
      },
      roleLabels: {
        trigger: 'Trigger piece',
        forcedReply: 'Forced reply',
        realizer: 'Later piece',
        capturedTarget: 'Captured target',
        immediateDefense: 'Immediate defense',
        disabledDefender: 'Displaced defender',
        remover: 'Removing piece',
        removedDefender: 'Removed defender',
        removalRecapture: 'Removal recapture',
        postRemovalTargetCapture: 'Later capture',
        immediateSoleRecapture: 'Immediate recapture',
        enabler: 'Enabling piece',
        slider: 'Slider',
        gateBlocker: 'Gate blocker',
        exploit: 'Later piece',
        releaser: 'Releasing piece',
        releasedBlocker: 'Displaced blocker',
        routePiece: 'Route piece',
        vacatingMove: 'Vacating piece',
        deferredMove: 'Deferred piece',
        captureReply: 'Capture reply',
        attackerAtCommonRoot: 'Capturing piece at common root',
        attackerAtCapture: 'Capturing piece at capture',
        targetAtCommonRoot: 'Target at common root',
        recaptureSquare: 'Recapture square',
        trackedResponderAtSeed: 'Recapturing piece before relocation',
        trackedResponderAtStaging: 'Recapturing piece after relocation',
        otherRecapturer: 'Other recapturing piece',
        rootActor: 'Actual-move piece',
        realizingActor: 'Later piece',
        displacement_check_response: 'Check reply',
        delayed_capture_recapture: 'Later recapture',
        immediate_capture_recapture: 'Immediate recapture',
        defender_removal: 'Defender removal',
        post_removal_target_capture_inventory: 'Later target capture',
        immediate_target_capture_inventory: 'Immediate target capture',
        gate_vacating_slider_reach: 'Opened slider route',
        later_slider_capture: 'Later slider capture',
        terminal_resource: 'Terminal resource',
        release_move: 'Release move',
        terminal_reply: 'Terminal reply',
        vacating_move: 'Vacating move',
        immediate_deferred_move: 'Immediate deferred move',
        immediate_capture_reply: 'Immediate capture reply',
        later_deferred_move: 'Later deferred move',
        relocated_recapture_inventory: 'Relocated-piece recapture',
        retained_recapture_inventory: 'Retained-line recapture',
        relocated_target_capture: 'Relocated-line target capture',
        relocated_responder_recapture: 'Relocated-piece recapture',
        retained_target_capture: 'Retained-line target capture',
        retained_other_recapture: 'Retained-line other recapture',
        relocated_branch_target_continuity: 'Relocated-line target continuity',
        retained_branch_target_continuity: 'Retained-line target continuity',
        relocated_responder_continuity: 'Relocated-piece continuity',
        retained_responder_continuity: 'Retained-piece continuity',
        relocated_branch_attacker_continuity: 'Relocated-line attacker continuity',
        retained_branch_attacker_continuity: 'Retained-line attacker continuity',
        retained: 'Retained',
        primary: 'Primary move',
        secondary: 'Secondary move',
        dependency: 'Dependency',
        result: 'Result',
        delayed_capture_recapture_absent: 'Later recapture unavailable',
        post_removal_replacement_recapture_absent: 'Replacement recapture unavailable',
        later_capture_immediate_recapture_absent: 'Immediate recapture unavailable',
        retained_gate_exploit_move_absent: 'Later move unavailable with gate retained',
        retained_gate_replacement_capture_absent: 'Replacement capture unavailable with gate retained',
        retained_blocker_first_route_leg_absent: 'First route move unavailable with blocker retained',
        capture_reply_absent: 'Capture reply unavailable',
        retained_seed_recapture_absent: 'Retained seed-square recapture unavailable',
        delayed_capture_actor_present: 'Later piece remains',
        delayed_target_present: 'Target remains',
        post_removal_exploit_actor_present: 'Later piece remains',
        post_removal_target_present: 'Target remains',
        vacated_gate_intervening_slider_reach: 'Opened slider route',
        vacated_gate_target_persistence: 'Target remains after gate opens',
        retained_gate_slider_persistence: 'Slider remains with gate retained',
        retained_gate_target_persistence: 'Target remains with gate retained',
        retained_gate_blocker_persistence: 'Gate blocker remains',
        retained_gate_blocked_slider_reach: 'Slider route remains blocked',
        released_square_vacancy: 'Released square remains empty',
        retained_blocker_persistence: 'Blocker remains',
        retained_route_origin_persistence: 'Route origin remains',
        vacated_target: 'Vacated target square',
        reply_actor: 'Reply piece remains',
        deferred_actor: 'Deferred piece remains',
      },
      contractLabels: {
        created_check_response_inventory: 'Check replies',
        capture_recapture_inventory: 'Capture and recapture',
        slider_reach_delta: 'Slider reach change',
        legal_move: 'Legal move',
        object_continuity_step: 'Piece continuity',
        pawn_topology_transition: 'Pawn structure change',
      },
    },
    premises: 'Premises',
    closedAbsence: 'Closed absence',
    closedState: 'Closed state',
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
    !hasOnlyKeys(value, ['primary', 'occurrence_explanations'], ['primary']) ||
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
  const explanations = projectOccurrenceExplanations(value.occurrence_explanations, move, subject);
  if (!explanations) return;
  const comparisonProof = buildProof(`pv:${move}`, subject.before.fen, reviewedEndpoint.moves, []);
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
    explanations,
    ...(comparisonProof ? { comparisonProof } : {}),
  };
}

function projectOccurrenceExplanations(
  value: unknown,
  candidateMove: Uci,
  subject: MoveReviewSubject,
): MoveReviewOccurrenceExplanation[] | undefined {
  if (value === undefined) return [];
  if (!Array.isArray(value) || value.length < 1) return;
  const explanations = value.map(item => projectOccurrenceExplanation(item, candidateMove, subject));
  if (!explanations.every((item): item is MoveReviewOccurrenceExplanation => !!item)) return;
  const pathOccurrenceIds = explanations.flatMap(explanation =>
    moveReviewOccurrenceProofPaths(explanation).map(path => path.pathOccurrenceId),
  );
  return unique(explanations.map(item => item.causeEvidenceId)) &&
    unique(explanations.map(item => item.proof.occurrenceId)) &&
    unique(pathOccurrenceIds)
    ? explanations
    : undefined;
}

function projectOccurrenceExplanation(
  value: unknown,
  candidateMove: Uci,
  subject: MoveReviewSubject,
): MoveReviewOccurrenceExplanation | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['cause_evidence_id', 'subject_occurrence', 'proof_kind', 'proof'])
  )
    return;
  const causeEvidenceId = nonEmptyWireString(value.cause_evidence_id);
  const proofKind = occurrenceProofKind(value.proof_kind);
  const subjectOccurrence = projectSubjectOccurrence(value.subject_occurrence, candidateMove, subject);
  if (!causeEvidenceId || !proofKind || !subjectOccurrence) return;

  switch (proofKind) {
    case 'unique_check_reply_defender_displacement_before_capture': {
      const proof = projectUniqueCheckReplyProof(value.proof);
      const explanation = proof
        ? {
            id: occurrenceExplanationId(proof.occurrenceId),
            causeEvidenceId,
            subjectOccurrence,
            proofKind,
            proof,
          }
        : undefined;
      return explanation && occurrenceProofOwnsSubject(explanation) ? explanation : undefined;
    }
    case 'sole_recapturer_removal_before_target_capture': {
      const proof = projectSoleRecapturerProof(value.proof);
      const explanation = proof
        ? {
            id: occurrenceExplanationId(proof.occurrenceId),
            causeEvidenceId,
            subjectOccurrence,
            proofKind,
            proof,
          }
        : undefined;
      return explanation && occurrenceProofOwnsSubject(explanation) ? explanation : undefined;
    }
    case 'vacated_gate_enables_unrecapturable_slider_capture': {
      const proof = projectVacatedGateProof(value.proof);
      const explanation = proof
        ? {
            id: occurrenceExplanationId(proof.occurrenceId),
            causeEvidenceId,
            subjectOccurrence,
            proofKind,
            proof,
          }
        : undefined;
      return explanation && occurrenceProofOwnsSubject(explanation) ? explanation : undefined;
    }
    case 'square_release_route': {
      const proof = projectSquareReleaseProof(value.proof);
      const explanation = proof
        ? {
            id: occurrenceExplanationId(proof.occurrenceId),
            causeEvidenceId,
            subjectOccurrence,
            proofKind,
            proof,
          }
        : undefined;
      return explanation && occurrenceProofOwnsSubject(explanation) ? explanation : undefined;
    }
    case 'capture_exclusion_move_order': {
      const proof = projectCaptureExclusionProof(value.proof);
      const explanation = proof
        ? {
            id: occurrenceExplanationId(proof.occurrenceId),
            causeEvidenceId,
            subjectOccurrence,
            proofKind,
            proof,
          }
        : undefined;
      return explanation && occurrenceProofOwnsSubject(explanation) ? explanation : undefined;
    }
    case 'relocation_enables_recapture': {
      const proof = projectRelocationEnablesRecaptureProof(value.proof);
      const explanation = proof
        ? {
            id: occurrenceExplanationId(proof.occurrenceId),
            causeEvidenceId,
            subjectOccurrence,
            proofKind,
            proof,
          }
        : undefined;
      return explanation && occurrenceProofOwnsSubject(explanation) ? explanation : undefined;
    }
    case 'passed_pawn_progress_realized_after_only_legal_reply': {
      const proof = projectPassedPawnProgressProof(value.proof);
      const explanation = proof
        ? {
            id: occurrenceExplanationId(proof.occurrenceId),
            causeEvidenceId,
            subjectOccurrence,
            proofKind,
            proof,
          }
        : undefined;
      return explanation && occurrenceProofOwnsSubject(explanation) ? explanation : undefined;
    }
  }
}

function occurrenceExplanationId(proofOccurrenceId: string): string {
  return `occurrence-explanation-${proofOccurrenceId}`;
}

function occurrenceProofKind(value: unknown): MoveReviewProofKind | undefined {
  return value === 'unique_check_reply_defender_displacement_before_capture' ||
    value === 'sole_recapturer_removal_before_target_capture' ||
    value === 'vacated_gate_enables_unrecapturable_slider_capture' ||
    value === 'square_release_route' ||
    value === 'capture_exclusion_move_order' ||
    value === 'relocation_enables_recapture' ||
    value === 'passed_pawn_progress_realized_after_only_legal_reply'
    ? value
    : undefined;
}

function projectSubjectOccurrence(
  value: unknown,
  candidateMove: Uci,
  subject: MoveReviewSubject,
): MoveReviewSubjectOccurrence | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'occurrence_id',
      'line_owner_evidence_id',
      'transition_evidence_id',
      'move_uci',
      'root_provenance',
      'line_id',
      'start',
      'destination',
    ])
  )
    return;
  const occurrenceId = nonEmptyWireString(value.occurrence_id);
  const lineOwnerEvidenceId = nonEmptyWireString(value.line_owner_evidence_id);
  const transitionEvidenceId = nonEmptyWireString(value.transition_evidence_id);
  const moveUci = uci(value.move_uci);
  const lineId = nonEmptyWireString(value.line_id);
  const start = projectOccurrencePosition(value.start, false);
  const destination = projectOccurrencePosition(value.destination, false);
  if (
    !occurrenceId ||
    !lineOwnerEvidenceId ||
    !transitionEvidenceId ||
    !moveUci ||
    !lineId ||
    !start ||
    !destination ||
    value.root_provenance !== 'observed_game_root' ||
    moveUci !== candidateMove ||
    start.fen !== subject.before.fen ||
    destination.fen !== subject.after.fen ||
    destination.ply !== start.ply + 1
  )
    return;
  return {
    occurrenceId,
    lineOwnerEvidenceId,
    transitionEvidenceId,
    moveUci,
    rootProvenance: value.root_provenance,
    lineId,
    start,
    destination,
  };
}

function projectOccurrencePosition(
  value: unknown,
  positivePly: boolean,
): MoveReviewOccurrencePosition | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['fen', 'ply'])) return;
  const fen = fenText(value.fen);
  const ply = nonNegativeInteger(value.ply);
  return fen && ply !== undefined && (!positivePly || ply >= 1) ? { fen, ply } : undefined;
}

function projectOccurrenceProofBase(
  value: Record<string, unknown>,
): MoveReviewOccurrenceProofBase | undefined {
  const sourceEvidenceId = nonEmptyWireString(value.source_evidence_id);
  const semanticId = typedHash(value.semantic_id);
  const occurrenceId = typedHash(value.occurrence_id);
  const dependencyFingerprint = typedHash(value.dependency_fingerprint);
  return sourceEvidenceId && semanticId && occurrenceId && dependencyFingerprint
    ? { sourceEvidenceId, semanticId, occurrenceId, dependencyFingerprint }
    : undefined;
}

function projectUniqueCheckReplyProof(value: unknown): MoveReviewUniqueCheckReplyProof | undefined {
  const keys = [
    'source_evidence_id',
    'semantic_id',
    'occurrence_id',
    'dependency_fingerprint',
    'displacement_branch',
    'immediate_capture_branch',
    'proof_paths',
    'participants',
    'realizing_move',
    'immediate_capture_branch_legal_defense_move',
  ];
  if (!isObject(value) || !hasExactKeys(value, keys)) return;
  const base = projectOccurrenceProofBase(value);
  const displacementBranch = projectOccurrenceBranch(
    value.displacement_branch,
    'displacement_then_capture',
    3,
    3,
  );
  const immediateCaptureBranch = projectOccurrenceBranch(
    value.immediate_capture_branch,
    'immediate_capture_with_defender',
    2,
    2,
  );
  const paths = projectArray(value.proof_paths, 1, 1, projectUniqueCheckReplyPath);
  const participants = projectUniqueCheckReplyParticipants(value.participants);
  const realizingMove = uci(value.realizing_move);
  const immediateCaptureBranchLegalDefenseMove = uci(value.immediate_capture_branch_legal_defense_move);
  if (
    !base ||
    !displacementBranch ||
    !immediateCaptureBranch ||
    !paths ||
    !participants ||
    !realizingMove ||
    !immediateCaptureBranchLegalDefenseMove ||
    displacementBranch.branchId === immediateCaptureBranch.branchId ||
    !boundedPathsFitBranches(paths, [displacementBranch, immediateCaptureBranch])
  )
    return;
  return {
    ...base,
    displacementBranch,
    immediateCaptureBranch,
    proofPaths: paths as [MoveReviewBoundedProofPath<MoveReviewUniquePremise>],
    participants,
    realizingMove,
    immediateCaptureBranchLegalDefenseMove,
  };
}

function projectUniqueCheckReplyParticipants(
  value: unknown,
): MoveReviewUniqueCheckReplyProof['participants'] | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'trigger',
      'forced_reply',
      'realizer',
      'captured_target',
      'immediate_defense',
      'disabled_defender',
    ])
  )
    return;
  const trigger = projectMovementWitness(value.trigger);
  const forcedReply = projectLegalResourceWitness(value.forced_reply);
  const realizer = projectMovementWitness(value.realizer);
  const capturedTarget = projectColoredPieceWitness(value.captured_target);
  const immediateDefense = projectLegalResourceWitness(value.immediate_defense);
  const disabledDefender = projectColoredPieceWitness(value.disabled_defender);
  return trigger && forcedReply && realizer && capturedTarget && immediateDefense && disabledDefender
    ? { trigger, forcedReply, realizer, capturedTarget, immediateDefense, disabledDefender }
    : undefined;
}

function projectUniqueCheckReplyPath(
  value: unknown,
): MoveReviewBoundedProofPath<MoveReviewUniquePremise> | undefined {
  const shell = projectBoundedPathShell(value, 3, 1, 4, undefined, 3);
  if (!shell) return;
  const premiseSpecs = [
    ['displacement_check_response', 'created_check_response_inventory', 'displacement_then_capture', 0],
    ['delayed_capture_recapture', 'capture_recapture_inventory', 'displacement_then_capture', 2],
    ['immediate_capture_recapture', 'capture_recapture_inventory', 'immediate_capture_with_defender', 0],
  ] as const;
  const premises = shell.premises.map((item, index) => {
    const spec = premiseSpecs[index]!;
    return projectRelationPremise(item, spec[0], spec[1], spec[2], step => step === spec[3]);
  });
  const absence = projectClosureUse(shell.closedAbsenceUses[0], {
    role: 'delayed_capture_recapture_absent',
    issuer: 'position_relation_extractor.closed_relation_inventory',
    branchRoles: ['displacement_then_capture'],
    query: legalCaptureQuery,
    afterStep: step => step === 2,
  });
  const states = shell.closedStateUses.map(item =>
    projectClosureUse(item, {
      role: ['delayed_capture_actor_present', 'delayed_target_present'],
      issuer: 'position_relation_extractor.closed_position_state_inventory',
      branchRoles: ['displacement_then_capture'],
      query: occupiedByQuery,
    }),
  );
  if (
    !premises.every((item): item is MoveReviewUniquePremise => !!item) ||
    !absence ||
    !states.every((item): item is MoveReviewClosureUse => !!item) ||
    !unique(states.map(item => item.useId))
  )
    return;
  return {
    pathOccurrenceId: shell.pathOccurrenceId,
    premises,
    closedAbsenceUses: [absence],
    closedStateUses: states,
  };
}

function projectSoleRecapturerProof(value: unknown): MoveReviewSoleRecapturerProof | undefined {
  const keys = [
    'source_evidence_id',
    'semantic_id',
    'occurrence_id',
    'dependency_fingerprint',
    'removal_branch',
    'immediate_capture_branch',
    'proof_paths',
    'participants',
    'post_removal_target_capture_move',
    'immediate_sole_recapture_move',
  ];
  if (!isObject(value) || !hasExactKeys(value, keys)) return;
  const base = projectOccurrenceProofBase(value);
  const removalBranch = projectOccurrenceBranch(value.removal_branch, 'removal_then_target_capture', 3, 3);
  const immediateCaptureBranch = projectOccurrenceBranch(
    value.immediate_capture_branch,
    'immediate_target_capture',
    2,
    2,
  );
  const paths = projectArray(value.proof_paths, 1, 1, projectSoleRecapturerPath);
  const participants = projectSoleRecapturerParticipants(value.participants);
  const postRemovalTargetCaptureMove = uci(value.post_removal_target_capture_move);
  const immediateSoleRecaptureMove = uci(value.immediate_sole_recapture_move);
  if (
    !base ||
    !removalBranch ||
    !immediateCaptureBranch ||
    !paths ||
    !participants ||
    !postRemovalTargetCaptureMove ||
    !immediateSoleRecaptureMove ||
    removalBranch.branchId === immediateCaptureBranch.branchId ||
    !boundedPathsFitBranches(paths, [removalBranch, immediateCaptureBranch])
  )
    return;
  return {
    ...base,
    removalBranch,
    immediateCaptureBranch,
    proofPaths: paths as [MoveReviewBoundedProofPath<MoveReviewSolePremise>],
    participants,
    postRemovalTargetCaptureMove,
    immediateSoleRecaptureMove,
  };
}

function projectSoleRecapturerParticipants(
  value: unknown,
): MoveReviewSoleRecapturerProof['participants'] | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'remover',
      'removed_defender',
      'removal_recapture',
      'post_removal_target_capture',
      'captured_target',
      'immediate_sole_recapture',
    ])
  )
    return;
  const remover = projectMovementWitness(value.remover);
  const removedDefender = projectColoredPieceWitness(value.removed_defender);
  const removalRecapture = projectLegalResourceWitness(value.removal_recapture);
  const postRemovalTargetCapture = projectMovementWitness(value.post_removal_target_capture);
  const capturedTarget = projectColoredPieceWitness(value.captured_target);
  const immediateSoleRecapture = projectLegalResourceWitness(value.immediate_sole_recapture);
  return remover &&
    removedDefender &&
    removalRecapture &&
    postRemovalTargetCapture &&
    capturedTarget &&
    immediateSoleRecapture
    ? {
        remover,
        removedDefender,
        removalRecapture,
        postRemovalTargetCapture,
        capturedTarget,
        immediateSoleRecapture,
      }
    : undefined;
}

function projectSoleRecapturerPath(
  value: unknown,
): MoveReviewBoundedProofPath<MoveReviewSolePremise> | undefined {
  const shell = projectBoundedPathShell(value, 3, 1, 4, undefined, 3);
  if (!shell) return;
  const premiseSpecs = [
    ['defender_removal', 'removal_then_target_capture', 0],
    ['post_removal_target_capture_inventory', 'removal_then_target_capture', 2],
    ['immediate_target_capture_inventory', 'immediate_target_capture', 0],
  ] as const;
  const premises = shell.premises.map((item, index) => {
    const spec = premiseSpecs[index]!;
    return projectRelationPremise(
      item,
      spec[0],
      'capture_recapture_inventory',
      spec[1],
      step => step === spec[2],
    );
  });
  const absence = projectClosureUse(shell.closedAbsenceUses[0], {
    role: 'post_removal_replacement_recapture_absent',
    issuer: 'position_relation_extractor.closed_relation_inventory',
    branchRoles: ['removal_then_target_capture'],
    query: legalCaptureQuery,
    afterStep: step => step === 2,
  });
  const states = shell.closedStateUses.map(item =>
    projectClosureUse(item, {
      role: ['post_removal_exploit_actor_present', 'post_removal_target_present'],
      issuer: 'position_relation_extractor.closed_position_state_inventory',
      branchRoles: ['removal_then_target_capture'],
      query: occupiedByQuery,
    }),
  );
  if (
    !premises.every((item): item is MoveReviewSolePremise => !!item) ||
    !absence ||
    !states.every((item): item is MoveReviewClosureUse => !!item) ||
    !unique(states.map(item => item.useId))
  )
    return;
  return {
    pathOccurrenceId: shell.pathOccurrenceId,
    premises,
    closedAbsenceUses: [absence],
    closedStateUses: states,
  };
}

function projectVacatedGateProof(value: unknown): MoveReviewVacatedGateProof | undefined {
  const keys = [
    'source_evidence_id',
    'semantic_id',
    'occurrence_id',
    'dependency_fingerprint',
    'vacated_gate_branch',
    'retained_gate_branch',
    'proof_paths',
    'participants',
    'exploit_move',
  ];
  if (!isObject(value) || !hasExactKeys(value, keys)) return;
  const base = projectOccurrenceProofBase(value);
  const vacatedGateBranch = projectOccurrenceBranch(
    value.vacated_gate_branch,
    'gate_vacated_then_capture',
    3,
  );
  const retainedGateBranch = projectOccurrenceBranch(value.retained_gate_branch, 'gate_retained', 2);
  const paths = projectArray(value.proof_paths, 1, 1, projectVacatedGatePath);
  const participants = projectVacatedGateParticipants(value.participants);
  const exploitMove = uci(value.exploit_move);
  if (
    !base ||
    !vacatedGateBranch ||
    !retainedGateBranch ||
    !paths ||
    !participants ||
    !exploitMove ||
    vacatedGateBranch.branchId === retainedGateBranch.branchId ||
    !boundedPathsFitBranches(paths, [vacatedGateBranch, retainedGateBranch])
  )
    return;
  return {
    ...base,
    vacatedGateBranch,
    retainedGateBranch,
    proofPaths: paths as [MoveReviewBoundedProofPath<MoveReviewVacatedGatePremise>],
    participants,
    exploitMove,
  };
}

function projectVacatedGateParticipants(
  value: unknown,
): MoveReviewVacatedGateProof['participants'] | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['enabler', 'slider', 'gate_blocker', 'exploit', 'captured_target'])
  )
    return;
  const enabler = projectMovementWitness(value.enabler);
  const slider = projectColoredPieceWitness(value.slider);
  const gateBlocker = projectColoredPieceWitness(value.gate_blocker);
  const exploit = projectMovementWitness(value.exploit);
  const capturedTarget = projectColoredPieceWitness(value.captured_target);
  return enabler && slider && gateBlocker && exploit && capturedTarget
    ? { enabler, slider, gateBlocker, exploit, capturedTarget }
    : undefined;
}

function projectVacatedGatePath(
  value: unknown,
): MoveReviewBoundedProofPath<MoveReviewVacatedGatePremise> | undefined {
  const shell = projectBoundedPathShell(value, 2, 3, 10, undefined, 2);
  if (!shell) return;
  const first = projectRelationPremise(
    shell.premises[0],
    'gate_vacating_slider_reach',
    'slider_reach_delta',
    'gate_vacated_then_capture',
    step => step === 0,
  );
  const second = projectRelationPremise(
    shell.premises[1],
    'later_slider_capture',
    'capture_recapture_inventory',
    'gate_vacated_then_capture',
    step => step >= 2,
  );
  const absenceSpecs = [
    ['later_capture_immediate_recapture_absent', 'gate_vacated_then_capture'],
    ['retained_gate_exploit_move_absent', 'gate_retained'],
    ['retained_gate_replacement_capture_absent', 'gate_retained'],
  ] as const;
  const absences = shell.closedAbsenceUses.map((item, index) => {
    const spec = absenceSpecs[index]!;
    return projectClosureUse(item, {
      role: spec[0],
      issuer: 'position_relation_extractor.closed_relation_inventory',
      branchRoles: [spec[1]],
      query: legalCaptureOrMoveQuery,
      afterStep: step => step >= 1,
    });
  });
  const states = shell.closedStateUses.map(item =>
    projectClosureUse(item, {
      role: [
        'vacated_gate_intervening_slider_reach',
        'vacated_gate_target_persistence',
        'retained_gate_slider_persistence',
        'retained_gate_target_persistence',
        'retained_gate_blocker_persistence',
        'retained_gate_blocked_slider_reach',
      ],
      issuer: 'position_relation_extractor.closed_position_state_inventory',
      branchRoles: ['gate_vacated_then_capture', 'gate_retained'],
      query: /^(occupied-by|slider-reach):.+$/,
    }),
  );
  if (
    !first ||
    !second ||
    !absences.every((item): item is MoveReviewClosureUse => !!item) ||
    !states.every((item): item is MoveReviewClosureUse => !!item) ||
    !unique([...absences, ...states].map(item => item.useId))
  )
    return;
  return {
    pathOccurrenceId: shell.pathOccurrenceId,
    premises: [first, second],
    closedAbsenceUses: absences,
    closedStateUses: states,
  };
}

function projectSquareReleaseProof(value: unknown): MoveReviewSquareReleaseProof | undefined {
  const required = [
    'source_evidence_id',
    'semantic_id',
    'occurrence_id',
    'dependency_fingerprint',
    'released_route_branch',
    'retained_blocker_branch',
    'proof_paths',
    'participants',
    'route',
    'terminal_step_index',
    'terminal',
  ];
  if (!isObject(value) || !hasOnlyKeys(value, [...required, 'terminal_reply_move'], required)) return;
  const base = projectOccurrenceProofBase(value);
  const releasedRouteBranch = projectOccurrenceBranch(
    value.released_route_branch,
    'released_square_route',
    3,
  );
  const retainedBlockerBranch = projectOccurrenceBranch(value.retained_blocker_branch, 'retained_blocker', 2);
  const proofPaths = projectArray(value.proof_paths, 1, undefined, projectSquareReleasePath);
  const participants = projectSquareReleaseParticipants(value.participants);
  const route = projectArray(value.route, 1, undefined, projectSquareReleaseRouteStep);
  const terminalStepIndex = nonNegativeInteger(value.terminal_step_index);
  const terminalReplyMove = Object.prototype.hasOwnProperty.call(value, 'terminal_reply_move')
    ? uci(value.terminal_reply_move)
    : undefined;
  const terminal = projectSquareReleaseTerminal(value.terminal);
  if (
    !base ||
    !releasedRouteBranch ||
    !retainedBlockerBranch ||
    !proofPaths ||
    !participants ||
    !route ||
    terminalStepIndex === undefined ||
    terminalStepIndex < 2 ||
    (Object.prototype.hasOwnProperty.call(value, 'terminal_reply_move') && !terminalReplyMove) ||
    !terminal ||
    releasedRouteBranch.branchId === retainedBlockerBranch.branchId ||
    !unique(proofPaths.map(path => path.pathOccurrenceId)) ||
    !boundedPathsFitBranches(proofPaths, [releasedRouteBranch, retainedBlockerBranch])
  )
    return;
  return {
    ...base,
    releasedRouteBranch,
    retainedBlockerBranch,
    proofPaths,
    participants,
    route,
    terminalStepIndex,
    ...(terminalReplyMove ? { terminalReplyMove } : {}),
    terminal,
  };
}

function projectSquareReleaseParticipants(
  value: unknown,
): MoveReviewSquareReleaseProof['participants'] | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['releaser', 'released_blocker', 'route_piece'])) return;
  const releaser = projectMovementWitness(value.releaser);
  const releasedBlocker = projectColoredPieceWitness(value.released_blocker);
  const routePiece = projectColoredPieceWitness(value.route_piece);
  return releaser && releasedBlocker && routePiece ? { releaser, releasedBlocker, routePiece } : undefined;
}

function projectSquareReleasePath(
  value: unknown,
): MoveReviewBoundedProofPath<MoveReviewSquareReleasePremise> | undefined {
  const shell = projectBoundedPathShell(value, 2, 1, 7);
  if (!shell) return;
  const premises = shell.premises.map(item =>
    isObject(item) && item.contract === 'legal_move'
      ? projectLegalMovePremise(
          item,
          role => /^(release_move|route_move_[0-9]+|terminal_reply)$/.test(role),
          ['released_square_route'],
        )
      : projectRelationPremise(
          item,
          'terminal_resource',
          ['capture_recapture_inventory', 'created_check_response_inventory'],
          'released_square_route',
          step => step >= 2,
        ),
  );
  const absence = projectClosureUse(shell.closedAbsenceUses[0], {
    role: 'retained_blocker_first_route_leg_absent',
    issuer: 'position_relation_extractor.closed_relation_inventory',
    branchRoles: ['retained_blocker'],
    query: legalMoveQuery,
    afterStep: step => step >= 1,
  });
  const states = shell.closedStateUses.map(item =>
    projectClosureUse(item, {
      role: role =>
        /^(released_square_vacancy|route_piece_[0-9]+|route_persistence_[0-9]+|retained_blocker_persistence|retained_route_origin_persistence)$/.test(
          role,
        ),
      issuer: 'position_relation_extractor.closed_position_state_inventory',
      branchRoles: ['released_square_route', 'retained_blocker'],
      query: vacantOrOccupiedQuery,
    }),
  );
  if (
    !premises.every((item): item is MoveReviewSquareReleasePremise => !!item) ||
    !absence ||
    !states.every((item): item is MoveReviewClosureUse => !!item) ||
    !unique(states.map(item => item.useId))
  )
    return;
  return {
    pathOccurrenceId: shell.pathOccurrenceId,
    premises,
    closedAbsenceUses: [absence],
    closedStateUses: states,
  };
}

function projectSquareReleaseRouteStep(value: unknown): MoveReviewSquareReleaseRouteStep | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['side', 'from', 'to', 'piece_before', 'piece_after', 'move_uci', 'step_index'])
  )
    return;
  const movement = projectMovementWitness({
    side: value.side,
    from: value.from,
    to: value.to,
    piece_before: value.piece_before,
    piece_after: value.piece_after,
  });
  const moveUci = uci(value.move_uci);
  const stepIndex = nonNegativeInteger(value.step_index);
  return movement && moveUci && stepIndex !== undefined && stepIndex >= 2
    ? { ...movement, moveUci, stepIndex }
    : undefined;
}

function projectSquareReleaseTerminal(value: unknown): MoveReviewSquareReleaseTerminal | undefined {
  if (!isObject(value)) return;
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
      ])
    )
      return;
    const assertionId = typedHash(value.assertion_id);
    const capturedTarget = projectColoredPieceWitness(value.captured_target);
    const geometricRecapturers = projectArray(value.geometric_recapturers, 0, undefined, projectPieceWitness);
    const legalRecaptures = projectArray(value.legal_recaptures, 0, undefined, projectRouteResource);
    const restrictedRecaptures = projectArray(
      value.restricted_recaptures,
      0,
      undefined,
      projectRestrictedRecapture,
    );
    return assertionId && capturedTarget && geometricRecapturers && legalRecaptures && restrictedRecaptures
      ? {
          kind: 'capture',
          assertionId,
          capturedTarget,
          geometricRecapturers,
          legalRecaptures,
          restrictedRecaptures,
        }
      : undefined;
  }
  if (value.kind === 'created_check') {
    if (
      !hasExactKeys(value, [
        'kind',
        'assertion_id',
        'checked_side',
        'king_square',
        'checkers',
        'responses',
        'controlled_king_destinations',
        'terminal_state',
      ])
    )
      return;
    const assertionId = typedHash(value.assertion_id);
    const checkedSide = wireColor(value.checked_side);
    const kingSquare = key(value.king_square);
    const checkers = projectArray(value.checkers, 1, undefined, projectPieceWitness);
    const responses = projectArray(value.responses, 0, undefined, projectCheckResponse);
    const controlledKingDestinations = projectArray(
      value.controlled_king_destinations,
      0,
      undefined,
      projectControlledKingDestination,
    );
    const terminalState =
      value.terminal_state === 'ongoing' || value.terminal_state === 'checkmate'
        ? value.terminal_state
        : undefined;
    return assertionId &&
      checkedSide &&
      kingSquare &&
      checkers &&
      responses &&
      controlledKingDestinations &&
      terminalState
      ? {
          kind: 'created_check',
          assertionId,
          checkedSide,
          kingSquare,
          checkers,
          responses,
          controlledKingDestinations,
          terminalState,
        }
      : undefined;
  }
  return;
}

function projectRouteResource(value: unknown): MoveReviewRouteResource | undefined {
  if (!isObject(value)) return;
  const required = ['side', 'from', 'to', 'piece_before', 'piece_after', 'move_uci'];
  if (!hasOnlyKeys(value, [...required, 'capture'], required)) return;
  const resource = projectLegalResourceWitness(Object.fromEntries(required.map(name => [name, value[name]])));
  const capture = Object.prototype.hasOwnProperty.call(value, 'capture')
    ? projectColoredPieceWitness(value.capture)
    : undefined;
  if (!resource || (Object.prototype.hasOwnProperty.call(value, 'capture') && !capture)) return;
  return { ...resource, ...(capture ? { capture } : {}) };
}

function projectRestrictedRecapture(
  value: unknown,
): Extract<MoveReviewSquareReleaseTerminal, { kind: 'capture' }>['restrictedRecaptures'][number] | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['piece', 'destination', 'king_square', 'post_move_controllers'])
  )
    return;
  const piece = projectPieceWitness(value.piece);
  const destination = key(value.destination);
  const kingSquare = key(value.king_square);
  const postMoveControllers = projectArray(value.post_move_controllers, 1, undefined, projectPieceWitness);
  return piece && destination && kingSquare && postMoveControllers
    ? { piece, destination, kingSquare, postMoveControllers }
    : undefined;
}

function projectCheckResponse(
  value: unknown,
): Extract<MoveReviewSquareReleaseTerminal, { kind: 'created_check' }>['responses'][number] | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['resource', 'modes'])) return;
  const resource = projectRouteResource(value.resource);
  if (!Array.isArray(value.modes) || value.modes.length < 1) return;
  const modes = value.modes.filter(
    (mode): mode is 'king_move' | 'capture_checker' | 'interpose' =>
      mode === 'king_move' || mode === 'capture_checker' || mode === 'interpose',
  );
  return resource && modes.length === value.modes.length && unique(modes) ? { resource, modes } : undefined;
}

function projectControlledKingDestination(
  value: unknown,
):
  | Extract<MoveReviewSquareReleaseTerminal, { kind: 'created_check' }>['controlledKingDestinations'][number]
  | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['destination', 'controllers'])) return;
  const destination = key(value.destination);
  const controllers = projectArray(value.controllers, 1, undefined, projectPieceWitness);
  return destination && controllers ? { destination, controllers } : undefined;
}

function projectCaptureExclusionProof(value: unknown): MoveReviewCaptureExclusionProof | undefined {
  const keys = [
    'source_evidence_id',
    'semantic_id',
    'occurrence_id',
    'dependency_fingerprint',
    'vacating_branch',
    'immediate_capture_branch',
    'proof_paths',
    'participants',
    'later_deferred_step_index',
  ];
  if (!isObject(value) || !hasExactKeys(value, keys)) return;
  const base = projectOccurrenceProofBase(value);
  const vacatingBranch = projectOccurrenceBranch(value.vacating_branch, 'vacating_then_deferred', 3);
  const immediateCaptureBranch = projectOccurrenceBranch(
    value.immediate_capture_branch,
    'immediate_deferred_capture',
    2,
    2,
  );
  const paths = projectArray(value.proof_paths, 1, 1, projectCaptureExclusionPath);
  const participants = projectCaptureExclusionParticipants(value.participants);
  const laterDeferredStepIndex = nonNegativeInteger(value.later_deferred_step_index);
  if (
    !base ||
    !vacatingBranch ||
    !immediateCaptureBranch ||
    !paths ||
    !participants ||
    laterDeferredStepIndex === undefined ||
    laterDeferredStepIndex < 2 ||
    vacatingBranch.branchId === immediateCaptureBranch.branchId ||
    !boundedPathsFitBranches(paths, [vacatingBranch, immediateCaptureBranch])
  )
    return;
  return {
    ...base,
    vacatingBranch,
    immediateCaptureBranch,
    proofPaths: paths as [MoveReviewBoundedProofPath<MoveReviewCaptureExclusionPremise>],
    participants,
    laterDeferredStepIndex,
  };
}

function projectCaptureExclusionParticipants(
  value: unknown,
): MoveReviewCaptureExclusionProof['participants'] | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['vacating_move', 'deferred_move', 'capture_reply', 'captured_target'])
  )
    return;
  const vacatingMove = projectMovementWitness(value.vacating_move);
  const deferredMove = projectMovementWitness(value.deferred_move);
  const captureReply = projectMovementWitness(value.capture_reply);
  const capturedTarget = projectColoredPieceWitness(value.captured_target);
  return vacatingMove && deferredMove && captureReply && capturedTarget
    ? { vacatingMove, deferredMove, captureReply, capturedTarget }
    : undefined;
}

function projectCaptureExclusionPath(
  value: unknown,
): MoveReviewBoundedProofPath<MoveReviewCaptureExclusionPremise> | undefined {
  const shell = projectBoundedPathShell(value, 4, 2, 8, undefined, 4);
  if (!shell) return;
  const specs = [
    ['vacating_move', 'vacating_then_deferred', 0, false],
    ['immediate_deferred_move', 'immediate_deferred_capture', 0, false],
    ['immediate_capture_reply', 'immediate_deferred_capture', 1, true],
    ['later_deferred_move', 'vacating_then_deferred', 2, false],
  ] as const;
  const premises = shell.premises.map((item, index) => {
    const spec = specs[index]!;
    return projectLegalMovePremise(
      item,
      role => role === spec[0],
      [spec[1]],
      step => (index === 3 ? step >= spec[2] : step === spec[2]),
      spec[3],
    );
  });
  const absences = shell.closedAbsenceUses.map(item =>
    projectClosureUse(item, {
      role: 'capture_reply_absent',
      issuer: 'position_relation_extractor.closed_relation_inventory',
      branchRoles: ['vacating_then_deferred'],
      query: legalMoveQuery,
    }),
  );
  const states = shell.closedStateUses.map(item =>
    projectClosureUse(item, {
      role: ['vacated_target', 'reply_actor', 'deferred_actor'],
      issuer: 'position_relation_extractor.closed_position_state_inventory',
      branchRoles: ['vacating_then_deferred'],
      query: vacantOrOccupiedQuery,
    }),
  );
  if (
    !premises.every((item): item is MoveReviewCaptureExclusionPremise => !!item) ||
    !absences.every((item): item is MoveReviewClosureUse => !!item) ||
    !states.every((item): item is MoveReviewClosureUse => !!item) ||
    !unique([...absences, ...states].map(item => item.useId))
  )
    return;
  return {
    pathOccurrenceId: shell.pathOccurrenceId,
    premises,
    closedAbsenceUses: absences,
    closedStateUses: states,
  };
}

function projectRelocationEnablesRecaptureProof(
  value: unknown,
): MoveReviewRelocationEnablesRecaptureProof | undefined {
  const keys = [
    'source_evidence_id',
    'semantic_id',
    'occurrence_id',
    'dependency_fingerprint',
    'relocated_responder_branch',
    'retained_responder_branch',
    'proof_paths',
    'participants',
    'relocation',
    'target_capture',
    'relocated_responder_recapture',
    'retained_other_recapture',
  ];
  if (!isObject(value) || !hasExactKeys(value, keys)) return;
  const base = projectOccurrenceProofBase(value);
  const relocatedResponderBranch = projectOccurrenceBranch(
    value.relocated_responder_branch,
    'relocated_responder',
    2,
  );
  const retainedResponderBranch = projectOccurrenceBranch(
    value.retained_responder_branch,
    'retained_responder',
    2,
  );
  const proofPaths = projectArray(value.proof_paths, 1, 1, projectRelocationRecapturePath);
  const participants = projectRelocationRecaptureParticipants(value.participants);
  const relocation = projectRelocationRecaptureTransition(value.relocation);
  const targetCapture = projectRelocationMatchedCapture(value.target_capture);
  const relocatedResponderRecapture = projectRelocationRecaptureTransition(
    value.relocated_responder_recapture,
  );
  const retainedOtherRecapture = projectRelocationRecaptureTransition(value.retained_other_recapture);
  if (
    !base ||
    !relocatedResponderBranch ||
    !retainedResponderBranch ||
    !proofPaths ||
    !participants ||
    !relocation ||
    !targetCapture ||
    !relocatedResponderRecapture ||
    !retainedOtherRecapture ||
    relocatedResponderBranch.branchId === retainedResponderBranch.branchId ||
    !boundedPathsFitBranches(proofPaths, [relocatedResponderBranch, retainedResponderBranch]) ||
    !relocationTransportReferencesAreClosed(
      proofPaths[0]!,
      relocatedResponderBranch,
      retainedResponderBranch,
      participants,
      relocation,
      targetCapture,
      relocatedResponderRecapture,
      retainedOtherRecapture,
    )
  )
    return;
  return {
    ...base,
    relocatedResponderBranch,
    retainedResponderBranch,
    proofPaths: [proofPaths[0]!],
    participants,
    relocation,
    targetCapture,
    relocatedResponderRecapture,
    retainedOtherRecapture,
  };
}

function projectRelocationRecaptureParticipants(
  value: unknown,
): MoveReviewRelocationEnablesRecaptureProof['participants'] | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'attacker_at_common_root',
      'attacker_at_capture',
      'target_at_common_root',
      'captured_target',
      'recapture_square',
      'tracked_responder_at_seed',
      'tracked_responder_at_staging',
      'other_recapturer',
    ])
  )
    return;
  const attackerAtCommonRoot = projectColoredPieceWitness(value.attacker_at_common_root);
  const attackerAtCapture = projectColoredPieceWitness(value.attacker_at_capture);
  const targetAtCommonRoot = projectColoredPieceWitness(value.target_at_common_root);
  const capturedTarget = projectColoredPieceWitness(value.captured_target);
  const recaptureSquare = key(value.recapture_square);
  const trackedResponderAtSeed = projectColoredPieceWitness(value.tracked_responder_at_seed);
  const trackedResponderAtStaging = projectColoredPieceWitness(value.tracked_responder_at_staging);
  const otherRecapturer = projectColoredPieceWitness(value.other_recapturer);
  return attackerAtCommonRoot &&
    attackerAtCapture &&
    targetAtCommonRoot &&
    capturedTarget &&
    recaptureSquare &&
    trackedResponderAtSeed &&
    trackedResponderAtStaging &&
    otherRecapturer
    ? {
        attackerAtCommonRoot,
        attackerAtCapture,
        targetAtCommonRoot,
        capturedTarget,
        recaptureSquare,
        trackedResponderAtSeed,
        trackedResponderAtStaging,
        otherRecapturer,
      }
    : undefined;
}

function projectRelocationRecaptureTransition(
  value: unknown,
): MoveReviewRelocationEnablesRecaptureProof['relocation'] | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['movement', 'move_uci', 'step_index'])) return;
  const movement = projectMovementWitness(value.movement);
  const moveUci = uci(value.move_uci);
  const stepIndex = nonNegativeInteger(value.step_index);
  return movement && moveUci && stepIndex !== undefined ? { movement, moveUci, stepIndex } : undefined;
}

function projectRelocationMatchedCapture(
  value: unknown,
): MoveReviewRelocationEnablesRecaptureProof['targetCapture'] | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['movement', 'move_uci', 'relocated_step_index', 'retained_step_index'])
  )
    return;
  const movement = projectMovementWitness(value.movement);
  const moveUci = uci(value.move_uci);
  const relocatedStepIndex = nonNegativeInteger(value.relocated_step_index);
  const retainedStepIndex = nonNegativeInteger(value.retained_step_index);
  return movement && moveUci && relocatedStepIndex !== undefined && retainedStepIndex !== undefined
    ? { movement, moveUci, relocatedStepIndex, retainedStepIndex }
    : undefined;
}

function projectRelocationRecapturePath(
  value: unknown,
): MoveReviewBoundedProofPath<MoveReviewRelocationRecapturePremise> | undefined {
  const shell = projectBoundedPathShell(value, 0, 1, 0);
  if (!shell) return;
  const premises = shell.premises.map(projectRelocationRecapturePremise);
  if (!premises.every((item): item is MoveReviewRelocationRecapturePremise => !!item)) return;
  const exactRoles = [
    'relocated_recapture_inventory',
    'retained_recapture_inventory',
    'relocated_target_capture',
    'relocated_responder_recapture',
    'retained_target_capture',
    'retained_other_recapture',
  ];
  const continuity = premises.filter(
    (item): item is MoveReviewRelocationContinuityPremiseUse => item.contract === 'object_continuity_step',
  );
  const continuityBindings = continuity.map(
    item => `${item.role}|${item.branchId}|${item.stepIndex}|${item.issuerOccurrenceId}`,
  );
  if (
    !exactRoles.every(role => premises.filter(item => item.role === role).length === 1) ||
    !unique(continuityBindings)
  )
    return;
  const absence = projectClosureUse(shell.closedAbsenceUses[0], {
    role: 'retained_seed_recapture_absent',
    issuer: 'position_relation_extractor.closed_relation_inventory',
    branchRoles: ['retained_responder'],
    query: legalMoveQuery,
  });
  const states = shell.closedStateUses.map(item =>
    projectClosureUse(item, {
      role: [
        'relocated_branch_target_continuity',
        'retained_branch_target_continuity',
        'relocated_responder_continuity',
        'retained_responder_continuity',
        'relocated_branch_attacker_continuity',
        'retained_branch_attacker_continuity',
      ],
      issuer: 'position_relation_extractor.closed_position_state_inventory',
      branchRoles: ['relocated_responder', 'retained_responder'],
      query: occupiedByQuery,
    }),
  );
  const retained = continuity.filter(item => item.transitionKind === 'retained');
  if (
    !absence ||
    !states.every((item): item is MoveReviewClosureUse => !!item) ||
    !unique([absence, ...states].map(item => item.useId)) ||
    states.length !== retained.length ||
    !retained.every(
      premise =>
        states.filter(
          state =>
            state.role === premise.role &&
            state.branchId === premise.branchId &&
            state.branchRole === premise.branchRole &&
            state.afterStepIndex === premise.stepIndex &&
            state.query ===
              `occupied-by:${premise.after.side}:${premise.after.piece}@${premise.after.square}`,
        ).length === 1,
    )
  )
    return;
  return {
    pathOccurrenceId: shell.pathOccurrenceId,
    premises,
    closedAbsenceUses: [absence],
    closedStateUses: states,
  };
}

function projectRelocationRecapturePremise(value: unknown): MoveReviewRelocationRecapturePremise | undefined {
  if (!isObject(value)) return;
  switch (value.role) {
    case 'relocated_recapture_inventory':
      return projectRelationPremise(
        value,
        value.role,
        'capture_recapture_inventory',
        'relocated_responder',
        () => true,
      );
    case 'retained_recapture_inventory':
      return projectRelationPremise(
        value,
        value.role,
        'capture_recapture_inventory',
        'retained_responder',
        () => true,
      );
    case 'relocated_target_capture':
      return projectLegalMovePremise<'relocated_target_capture', 'relocated_responder'>(
        value,
        role => role === 'relocated_target_capture',
        ['relocated_responder'],
        () => true,
        true,
      );
    case 'relocated_responder_recapture':
      return projectLegalMovePremise<'relocated_responder_recapture', 'relocated_responder'>(
        value,
        role => role === 'relocated_responder_recapture',
        ['relocated_responder'],
        () => true,
        true,
      );
    case 'retained_target_capture':
      return projectLegalMovePremise<'retained_target_capture', 'retained_responder'>(
        value,
        role => role === 'retained_target_capture',
        ['retained_responder'],
        () => true,
        true,
      );
    case 'retained_other_recapture':
      return projectLegalMovePremise<'retained_other_recapture', 'retained_responder'>(
        value,
        role => role === 'retained_other_recapture',
        ['retained_responder'],
        () => true,
        true,
      );
    case 'relocated_branch_target_continuity':
    case 'retained_branch_target_continuity':
    case 'relocated_responder_continuity':
    case 'retained_responder_continuity':
    case 'relocated_branch_attacker_continuity':
    case 'retained_branch_attacker_continuity':
      return projectRelocationContinuityPremise(value);
    default:
      return;
  }
}

function projectRelocationContinuityPremise(
  value: Record<string, unknown>,
): MoveReviewRelocationContinuityPremiseUse | undefined {
  const required = [
    'role',
    'contract',
    'transition_kind',
    'overall_move_uci',
    'before',
    'after',
    'legal_move_semantic_id',
    'transition_footprint_id',
    'issuer_evidence_id',
    'issuer_occurrence_id',
    'source_premise_ids',
    'branch_id',
    'branch_role',
    'step_index',
  ];
  if (!hasOnlyKeys(value, [...required, 'selected_transition'], required)) return;
  const role = [
    'relocated_branch_target_continuity',
    'retained_branch_target_continuity',
    'relocated_responder_continuity',
    'retained_responder_continuity',
    'relocated_branch_attacker_continuity',
    'retained_branch_attacker_continuity',
  ].find(item => item === value.role) as MoveReviewRelocationContinuityRole | undefined;
  const transitionKind = ['retained', 'primary', 'secondary'].find(item => item === value.transition_kind) as
    | MoveReviewRelocationContinuityPremiseUse['transitionKind']
    | undefined;
  const overallMoveUci = uci(value.overall_move_uci);
  const before = projectColoredPieceWitness(value.before);
  const after = projectColoredPieceWitness(value.after);
  const selectedTransition = Object.prototype.hasOwnProperty.call(value, 'selected_transition')
    ? projectMovementWitness(value.selected_transition)
    : undefined;
  const legalMoveSemanticId = typedHash(value.legal_move_semantic_id);
  const transitionFootprintId = typedHash(value.transition_footprint_id);
  const issuerEvidenceId = nonEmptyWireString(value.issuer_evidence_id);
  const issuerOccurrenceId = typedHash(value.issuer_occurrence_id);
  const sourcePremiseIds = wireUniqueStrings(value.source_premise_ids, 4);
  const branchId = typedHash(value.branch_id);
  const branchRole =
    value.branch_role === 'relocated_responder' || value.branch_role === 'retained_responder'
      ? value.branch_role
      : undefined;
  const stepIndex = nonNegativeInteger(value.step_index);
  const roleMatchesBranch =
    !!role &&
    !!branchRole &&
    ((role.startsWith('relocated_') && branchRole === 'relocated_responder') ||
      (role.startsWith('retained_') && branchRole === 'retained_responder'));
  const transitionIsExact =
    !!transitionKind &&
    !!before &&
    !!after &&
    (transitionKind === 'retained'
      ? !Object.prototype.hasOwnProperty.call(value, 'selected_transition') &&
        coloredPieceEquals(before, after)
      : !!selectedTransition &&
        selectedTransition.side === before.side &&
        selectedTransition.from === before.square &&
        selectedTransition.pieceBefore === before.piece &&
        selectedTransition.to === after.square &&
        selectedTransition.pieceAfter === after.piece);
  return role &&
    value.contract === 'object_continuity_step' &&
    transitionKind &&
    overallMoveUci &&
    before &&
    after &&
    legalMoveSemanticId &&
    transitionFootprintId &&
    issuerEvidenceId &&
    issuerOccurrenceId &&
    sourcePremiseIds &&
    sourcePremiseIds.length === 4 &&
    sourcePremiseIds.every(
      (value, index) =>
        value ===
        [
          issuerEvidenceId,
          issuerOccurrenceId,
          `legal-move:${legalMoveSemanticId}`,
          `transition-footprint:${transitionFootprintId}`,
        ].sort()[index],
    ) &&
    branchId &&
    branchRole &&
    stepIndex !== undefined &&
    roleMatchesBranch &&
    transitionIsExact
    ? {
        role,
        contract: 'object_continuity_step',
        transitionKind,
        overallMoveUci,
        before,
        after,
        ...(selectedTransition ? { selectedTransition } : {}),
        legalMoveSemanticId,
        transitionFootprintId,
        issuerEvidenceId,
        issuerOccurrenceId,
        sourcePremiseIds,
        branchId,
        branchRole,
        stepIndex,
      }
    : undefined;
}

function coloredPieceEquals(
  first: MoveReviewColoredPieceWitness,
  second: MoveReviewColoredPieceWitness,
): boolean {
  return first.side === second.side && first.piece === second.piece && first.square === second.square;
}

function movementWitnessEquals(first: MoveReviewMovementWitness, second: MoveReviewMovementWitness): boolean {
  return (
    first.side === second.side &&
    first.pieceBefore === second.pieceBefore &&
    first.pieceAfter === second.pieceAfter &&
    first.from === second.from &&
    first.to === second.to
  );
}

function relocationTransportReferencesAreClosed(
  path: MoveReviewBoundedProofPath<MoveReviewRelocationRecapturePremise>,
  relocatedBranch: MoveReviewOccurrenceBranch<'relocated_responder'>,
  retainedBranch: MoveReviewOccurrenceBranch<'retained_responder'>,
  participants: MoveReviewRelocationEnablesRecaptureProof['participants'],
  relocation: MoveReviewRelocationEnablesRecaptureProof['relocation'],
  targetCapture: MoveReviewRelocationEnablesRecaptureProof['targetCapture'],
  relocatedRecapture: MoveReviewRelocationEnablesRecaptureProof['relocatedResponderRecapture'],
  retainedRecapture: MoveReviewRelocationEnablesRecaptureProof['retainedOtherRecapture'],
): boolean {
  const continuity = path.premises.filter(
    (premise): premise is MoveReviewRelocationContinuityPremiseUse =>
      premise.contract === 'object_continuity_step',
  );
  const relationReference = (
    role: 'relocated_recapture_inventory' | 'retained_recapture_inventory',
    branch: MoveReviewOccurrenceBranch,
    stepIndex: number,
  ): boolean => {
    const matches = path.premises.filter(
      (
        premise,
      ): premise is Extract<
        MoveReviewRelocationRecapturePremise,
        { contract: 'capture_recapture_inventory' }
      > => premise.contract === 'capture_recapture_inventory' && premise.role === role,
    );
    return (
      matches.length === 1 &&
      matches[0]!.branchId === branch.branchId &&
      matches[0]!.branchRole === branch.branchRole &&
      matches[0]!.stepIndex === stepIndex
    );
  };
  const continuityReference = (
    role: MoveReviewRelocationContinuityRole,
    branch: MoveReviewOccurrenceBranch,
    untilExclusive: number,
    rootPiece: MoveReviewColoredPieceWitness,
    endpointPiece: MoveReviewColoredPieceWitness,
  ): boolean => {
    const uses = continuity.filter(premise => premise.role === role);
    const endpointReferencesMatch =
      untilExclusive === 0
        ? coloredPieceEquals(rootPiece, endpointPiece)
        : !!uses.length &&
          coloredPieceEquals(uses[0]!.before, rootPiece) &&
          coloredPieceEquals(uses[uses.length - 1]!.after, endpointPiece);
    return (
      uses.length === untilExclusive &&
      uses.every(
        (premise, index) =>
          premise.stepIndex === index &&
          premise.branchId === branch.branchId &&
          premise.branchRole === branch.branchRole,
      ) &&
      endpointReferencesMatch
    );
  };
  const relocatedResponder = continuity.filter(premise => premise.role === 'relocated_responder_continuity');
  const relocationUses = relocatedResponder.filter(
    premise =>
      premise.stepIndex === relocation.stepIndex &&
      premise.overallMoveUci === relocation.moveUci &&
      premise.selectedTransition !== undefined &&
      movementWitnessEquals(premise.selectedTransition, relocation.movement),
  );
  const legalEndpoint = (
    role: string,
    branch: MoveReviewOccurrenceBranch,
    stepIndex: number,
    movement: MoveReviewMovementWitness,
    moveUci: Uci,
    captured?: MoveReviewColoredPieceWitness,
  ): boolean => {
    const matches = path.premises.filter(
      (premise): premise is Extract<MoveReviewRelocationRecapturePremise, { contract: 'legal_move' }> =>
        premise.contract === 'legal_move' && premise.role === role,
    );
    const premise = matches[0];
    const legalOwners = premise
      ? [
          premise.issuerEvidenceId,
          premise.issuerOccurrenceId,
          `legal-move:${premise.legalMoveSemanticId}`,
        ].sort()
      : [];
    return (
      matches.length === 1 &&
      premise?.branchId === branch.branchId &&
      premise.branchRole === branch.branchRole &&
      premise.stepIndex === stepIndex &&
      premise.moveUci === moveUci &&
      movementWitnessEquals(premise.movement, movement) &&
      !!premise.capture &&
      premise.sourcePremiseIds.length === legalOwners.length &&
      premise.sourcePremiseIds.every((value, index) => value === legalOwners[index]) &&
      (!captured || coloredPieceEquals(premise.capture, captured))
    );
  };
  const recapturedAttacker: MoveReviewColoredPieceWitness = {
    side: targetCapture.movement.side,
    piece: targetCapture.movement.pieceAfter,
    square: participants.recaptureSquare,
  };

  return (
    legalEndpoint(
      'relocated_target_capture',
      relocatedBranch,
      targetCapture.relocatedStepIndex,
      targetCapture.movement,
      targetCapture.moveUci,
      participants.capturedTarget,
    ) &&
    relationReference('relocated_recapture_inventory', relocatedBranch, targetCapture.relocatedStepIndex) &&
    relationReference('retained_recapture_inventory', retainedBranch, targetCapture.retainedStepIndex) &&
    legalEndpoint(
      'retained_target_capture',
      retainedBranch,
      targetCapture.retainedStepIndex,
      targetCapture.movement,
      targetCapture.moveUci,
      participants.capturedTarget,
    ) &&
    legalEndpoint(
      'relocated_responder_recapture',
      relocatedBranch,
      relocatedRecapture.stepIndex,
      relocatedRecapture.movement,
      relocatedRecapture.moveUci,
      recapturedAttacker,
    ) &&
    legalEndpoint(
      'retained_other_recapture',
      retainedBranch,
      retainedRecapture.stepIndex,
      retainedRecapture.movement,
      retainedRecapture.moveUci,
      recapturedAttacker,
    ) &&
    path.closedAbsenceUses.length === 1 &&
    path.closedAbsenceUses[0]!.branchId === retainedBranch.branchId &&
    path.closedAbsenceUses[0]!.branchRole === retainedBranch.branchRole &&
    path.closedAbsenceUses[0]!.afterStepIndex === targetCapture.retainedStepIndex &&
    path.closedAbsenceUses[0]!.query ===
      `legal-move-from-to:${participants.trackedResponderAtSeed.side}:${participants.trackedResponderAtSeed.square}:${participants.recaptureSquare}` &&
    targetCapture.movement.to === participants.recaptureSquare &&
    relocatedRecapture.movement.to === participants.recaptureSquare &&
    retainedRecapture.movement.to === participants.recaptureSquare &&
    continuityReference(
      'relocated_branch_target_continuity',
      relocatedBranch,
      targetCapture.relocatedStepIndex,
      participants.targetAtCommonRoot,
      participants.capturedTarget,
    ) &&
    continuityReference(
      'retained_branch_target_continuity',
      retainedBranch,
      targetCapture.retainedStepIndex,
      participants.targetAtCommonRoot,
      participants.capturedTarget,
    ) &&
    continuityReference(
      'relocated_branch_attacker_continuity',
      relocatedBranch,
      targetCapture.relocatedStepIndex,
      participants.attackerAtCommonRoot,
      participants.attackerAtCapture,
    ) &&
    continuityReference(
      'retained_branch_attacker_continuity',
      retainedBranch,
      targetCapture.retainedStepIndex,
      participants.attackerAtCommonRoot,
      participants.attackerAtCapture,
    ) &&
    continuityReference(
      'relocated_responder_continuity',
      relocatedBranch,
      relocatedRecapture.stepIndex,
      participants.trackedResponderAtSeed,
      participants.trackedResponderAtStaging,
    ) &&
    continuityReference(
      'retained_responder_continuity',
      retainedBranch,
      retainedRecapture.stepIndex,
      participants.trackedResponderAtSeed,
      participants.trackedResponderAtSeed,
    ) &&
    relocationUses.length === 1
  );
}

function projectPassedPawnProgressProof(value: unknown): MoveReviewPassedPawnProgressProof | undefined {
  const keys = [
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
  ];
  if (!isObject(value) || !hasExactKeys(value, keys)) return;
  const base = projectOccurrenceProofBase(value);
  const eventEvidenceId = nonEmptyWireString(value.event_evidence_id);
  const resultTargetSubjects = wireUniqueStrings(value.result_target_subjects, 1);
  const rootActor = projectPassedPawnActor(value.root_actor);
  const realizingActor = projectPassedPawnActor(value.realizing_actor);
  const rootLine = projectPassedPawnLine(value.root_line);
  const rootMove = uci(value.root_move);
  const rootPly = positiveInteger(value.root_ply);
  const realizingMove = uci(value.realizing_move);
  const realizingPly = positiveInteger(value.realizing_ply);
  const resultPlyOffset = positiveInteger(value.result_ply_offset);
  const closedLegalReplyInventory = projectPassedPawnClosedReplyInventory(value.closed_legal_reply_inventory);
  const branches = projectArray(value.branches, 1, 1, projectPassedPawnBranch);
  const proofPaths = projectArray(value.proof_paths, 1, undefined, projectPassedPawnPath);
  const lowerPremiseIds = wireUniqueStrings(value.lower_premise_ids, 1);
  if (
    !base ||
    !eventEvidenceId ||
    !resultTargetSubjects ||
    !rootActor ||
    !realizingActor ||
    !rootLine ||
    !rootMove ||
    rootPly === undefined ||
    !realizingMove ||
    realizingPly === undefined ||
    resultPlyOffset === undefined ||
    !closedLegalReplyInventory ||
    !branches ||
    !proofPaths ||
    !lowerPremiseIds ||
    rootLine.rootMove !== rootMove ||
    !unique(proofPaths.map(path => path.pathOccurrenceId)) ||
    !passedPawnPathsFitBranch(proofPaths, branches[0]!) ||
    closedLegalReplyInventory.analysisContinuationBranchId !== branches[0]!.branchId
  )
    return;
  return {
    ...base,
    eventEvidenceId,
    resultTargetSubjects,
    rootActor,
    realizingActor,
    rootLine,
    rootMove,
    rootPly,
    realizingMove,
    realizingPly,
    resultPlyOffset,
    closedLegalReplyInventory,
    branches: branches as [MoveReviewPassedPawnBranch],
    proofPaths,
    lowerPremiseIds,
  };
}

function projectPassedPawnActor(value: unknown): MoveReviewPassedPawnActor | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['side', 'piece_before', 'piece_after', 'from', 'to', 'legal_move_relation'])
  )
    return;
  const side = wireColor(value.side);
  const pieceBefore = pieceRole(value.piece_before);
  const pieceAfter = pieceRole(value.piece_after);
  const from = key(value.from);
  const to = key(value.to);
  const legalMoveRelation = typedHash(value.legal_move_relation);
  return side && pieceBefore && pieceAfter && from && to && legalMoveRelation
    ? { side, pieceBefore, pieceAfter, from, to, legalMoveRelation }
    : undefined;
}

function projectPassedPawnLine(value: unknown): MoveReviewPassedPawnLine | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['line_id', 'root_move'])) return;
  const lineId = nonEmptyWireString(value.line_id);
  const rootMove = uci(value.root_move);
  return lineId && rootMove ? { lineId, rootMove } : undefined;
}

function projectPassedPawnClosedReplyInventory(
  value: unknown,
): MoveReviewPassedPawnProgressProof['closedLegalReplyInventory'] | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'issuer_evidence_id',
      'root_after',
      'legal_reply_move',
      'analysis_continuation_branch_id',
    ]) ||
    !isObject(value.root_after) ||
    !hasExactKeys(value.root_after, ['fen', 'ply', 'scope']) ||
    value.root_after.scope !== 'played_transition'
  )
    return;
  const issuerEvidenceId = nonEmptyWireString(value.issuer_evidence_id);
  const rootPosition = projectOccurrencePosition(
    { fen: value.root_after.fen, ply: value.root_after.ply },
    true,
  );
  const legalReplyMove = uci(value.legal_reply_move);
  const analysisContinuationBranchId = typedHash(value.analysis_continuation_branch_id);
  return issuerEvidenceId && rootPosition && legalReplyMove && analysisContinuationBranchId
    ? {
        issuerEvidenceId,
        rootAfter: { ...rootPosition, scope: value.root_after.scope },
        legalReplyMove,
        analysisContinuationBranchId,
      }
    : undefined;
}

function projectPassedPawnBranch(value: unknown): MoveReviewPassedPawnBranch | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'branch_id',
      'role',
      'reply_move',
      'source_occurrence_id',
      'line',
      'line_owner_evidence_id',
      'root_transition_evidence_id',
      'root_provenance',
      'steps',
    ]) ||
    value.role !== 'observed_root_with_analyzed_continuation' ||
    value.root_provenance !== 'observed_game_root'
  )
    return;
  const branchId = typedHash(value.branch_id);
  const replyMove = uci(value.reply_move);
  const sourceOccurrenceId = typedHash(value.source_occurrence_id);
  const line = projectPassedPawnLine(value.line);
  const lineOwnerEvidenceId = nonEmptyWireString(value.line_owner_evidence_id);
  const rootTransitionEvidenceId = nonEmptyWireString(value.root_transition_evidence_id);
  const steps = projectArray(value.steps, 2, undefined, projectPassedPawnStep);
  if (
    !branchId ||
    !replyMove ||
    !sourceOccurrenceId ||
    !line ||
    !lineOwnerEvidenceId ||
    !rootTransitionEvidenceId ||
    !steps ||
    !occurrenceStepsAreValid(steps, value.root_provenance, line.rootMove) ||
    !steps.every(step => step.line.lineId === line.lineId && step.line.rootMove === line.rootMove)
  )
    return;
  return {
    branchId,
    branchRole: value.role,
    replyMove,
    sourceOccurrenceId,
    line,
    lineOwnerEvidenceId,
    rootTransitionEvidenceId,
    rootProvenance: value.root_provenance,
    steps,
  };
}

function projectPassedPawnStep(value: unknown): MoveReviewPassedPawnStep | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'step_index',
      'step_key',
      'ply',
      'move_uci',
      'fen_before',
      'fen_after',
      'line',
      'provenance',
    ])
  )
    return;
  const stepIndex = nonNegativeInteger(value.step_index);
  const stepKey = nonEmptyWireString(value.step_key);
  const ply = positiveInteger(value.ply);
  const moveUci = uci(value.move_uci);
  const fenBefore = fenText(value.fen_before);
  const fenAfter = fenText(value.fen_after);
  const line = projectPassedPawnLine(value.line);
  const provenance = stepProvenance(value.provenance);
  return stepIndex !== undefined &&
    stepKey &&
    ply !== undefined &&
    moveUci &&
    fenBefore &&
    fenAfter &&
    line &&
    provenance
    ? { stepIndex, stepKey, ply, moveUci, fenBefore, fenAfter, line, provenance }
    : undefined;
}

function projectPassedPawnPath(value: unknown): MoveReviewPassedPawnProofPath | undefined {
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
    ])
  )
    return;
  const pathOccurrenceId = typedHash(value.path_occurrence_id);
  const analysisContinuationBranchId = typedHash(value.analysis_continuation_branch_id);
  const realizationActor = projectPassedPawnActor(value.realization_actor);
  const realizationMove = uci(value.realization_move);
  const realizationPly = positiveInteger(value.realization_ply);
  const premises = projectArray(value.premises, 1, undefined, projectPassedPawnPremise);
  const closureUseIds = wireUniqueStrings(value.closure_use_ids, 1, typedHash);
  return pathOccurrenceId &&
    analysisContinuationBranchId &&
    realizationActor &&
    realizationMove &&
    realizationPly !== undefined &&
    premises &&
    closureUseIds
    ? {
        pathOccurrenceId,
        analysisContinuationBranchId,
        realizationActor,
        realizationMove,
        realizationPly,
        premises,
        closureUseIds,
      }
    : undefined;
}

function projectPassedPawnPremise(value: unknown): MoveReviewPassedPawnPremise | undefined {
  const required = [
    'role',
    'lower_kind',
    'lower_semantic_key',
    'source_premise_ids',
    'branch_id',
    'branch_role',
    'from_step_index',
    'to_step_index',
  ];
  if (!isObject(value) || !hasOnlyKeys(value, [...required, 'dependency_proof'], required)) return;
  const lowerSemanticKey = nonEmptyWireString(value.lower_semantic_key);
  const sourcePremiseIds = wireUniqueStrings(value.source_premise_ids, 1);
  const branchId = typedHash(value.branch_id);
  const fromStepIndex = nonNegativeInteger(value.from_step_index);
  const toStepIndex = nonNegativeInteger(value.to_step_index);
  if (
    !lowerSemanticKey ||
    !sourcePremiseIds ||
    !branchId ||
    value.branch_role !== 'observed_root_with_analyzed_continuation' ||
    fromStepIndex === undefined ||
    toStepIndex === undefined
  )
    return;
  const common = {
    lowerSemanticKey,
    sourcePremiseIds,
    branchId,
    branchRole: 'observed_root_with_analyzed_continuation' as const,
    fromStepIndex,
    toStepIndex,
  };
  if (
    value.role === 'dependency' &&
    value.lower_kind === 'passed_pawn_progress_dependency' &&
    Object.prototype.hasOwnProperty.call(value, 'dependency_proof')
  ) {
    const dependencyProof = projectPassedPawnDependencyProof(value.dependency_proof);
    return dependencyProof
      ? { ...common, role: value.role, lowerKind: value.lower_kind, dependencyProof }
      : undefined;
  }
  return value.role === 'result' &&
    value.lower_kind === 'passed_pawn_progress' &&
    !Object.prototype.hasOwnProperty.call(value, 'dependency_proof')
    ? { ...common, role: value.role, lowerKind: value.lower_kind }
    : undefined;
}

function projectPassedPawnDependencyProof(value: unknown): MoveReviewPassedPawnDependencyProof | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'dependency_kind',
      'proof_kind',
      'squares',
      'pieces',
      'relation_issuers',
      'position_state_issuers',
    ])
  )
    return;
  const pairIsValid =
    (value.dependency_kind === 'object_state_precondition' && value.proof_kind === 'object_state') ||
    (value.dependency_kind === 'line_access_precondition' && value.proof_kind === 'line_access') ||
    (value.dependency_kind === 'response_continuation_precondition' &&
      (value.proof_kind === 'pawn_break_follow_up' || value.proof_kind === 'capture_follow_up'));
  const squares = projectArray(value.squares, 0, undefined, projectPassedPawnDependencySquare);
  const pieces = projectArray(value.pieces, 0, undefined, projectPassedPawnDependencyPiece);
  const relationIssuers = projectArray(value.relation_issuers, 0, undefined, projectPassedPawnRelationIssuer);
  const positionStateIssuers = projectArray(
    value.position_state_issuers,
    0,
    undefined,
    projectPassedPawnPositionStateIssuer,
  );
  if (!pairIsValid || !squares || !pieces || !relationIssuers || !positionStateIssuers) return;
  return {
    dependencyKind: value.dependency_kind,
    proofKind: value.proof_kind,
    squares,
    pieces,
    relationIssuers,
    positionStateIssuers,
  } as MoveReviewPassedPawnDependencyProof;
}

function projectPassedPawnDependencySquare(
  value: unknown,
): MoveReviewPassedPawnDependencyProof['squares'][number] | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['role', 'square'])) return;
  const roles: MoveReviewPassedPawnDependencySquareRole[] = [
    'root_from',
    'root_to',
    'future_from',
    'future_to',
    'vacated_gate',
    'enabled_from',
    'enabled_to',
    'reply_from',
    'reply_to',
    'follow_up_from',
    'follow_up_to',
    'released_passed_pawn',
  ];
  const role = roles.find(item => item === value.role);
  const square = key(value.square);
  return role && square ? { role, square } : undefined;
}

function projectPassedPawnDependencyPiece(
  value: unknown,
): MoveReviewPassedPawnDependencyProof['pieces'][number] | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['role', 'side', 'piece'])) return;
  const roles: MoveReviewPassedPawnDependencyPieceRole[] = [
    'root_before',
    'tracked',
    'future_after',
    'enabled_piece',
    'trigger_pawn',
    'responder_pawn',
    'follow_up_pawn',
    'trigger_piece',
    'responder_piece',
    'follow_up_piece',
  ];
  const role = roles.find(item => item === value.role);
  const side = wireColor(value.side);
  const piece = pieceRole(value.piece);
  return role && side && piece ? { role, side, piece } : undefined;
}

function projectPassedPawnRelationIssuer(value: unknown): MoveReviewPassedPawnRelationIssuer | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
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
  const contract =
    value.contract === 'slider_reach_delta' ||
    value.contract === 'pawn_topology_transition' ||
    value.contract === 'capture_recapture_inventory'
      ? value.contract
      : undefined;
  const resultKey = nonEmptyWireString(value.result_key);
  const occurrenceId = typedHash(value.occurrence_id);
  const stepKey = nonEmptyWireString(value.step_key);
  const sourcePremiseIds = wireUniqueStrings(value.source_premise_ids, 1);
  const issuerEvidenceId = nonEmptyWireString(value.issuer_evidence_id);
  const line = projectPassedPawnLine(value.line);
  return contract &&
    resultKey &&
    resultIdMatchesContract(resultKey, contract) &&
    occurrenceId &&
    stepKey &&
    sourcePremiseIds &&
    issuerEvidenceId &&
    line &&
    value.scope === 'legal_line'
    ? {
        contract,
        resultKey,
        occurrenceId,
        stepKey,
        sourcePremiseIds,
        issuerEvidenceId,
        line,
        scope: value.scope,
      }
    : undefined;
}

function projectPassedPawnPositionStateIssuer(
  value: unknown,
): MoveReviewPassedPawnPositionStateIssuer | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
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
    ])
  )
    return;
  const state = projectPassedPawnPositionState(value.state);
  const semanticProofId = typedHash(value.semantic_proof_id);
  const issuerEvidenceId = nonEmptyWireString(value.issuer_evidence_id);
  const issuerOccurrenceId = typedHash(value.issuer_occurrence_id);
  const stepKey = nonEmptyWireString(value.step_key);
  const ply = positiveInteger(value.ply);
  const moveUci = uci(value.move_uci);
  const fenBefore = fenText(value.fen_before);
  const fenAfter = fenText(value.fen_after);
  const line = projectPassedPawnLine(value.line);
  return state &&
    semanticProofId &&
    issuerEvidenceId &&
    issuerOccurrenceId &&
    stepKey &&
    ply !== undefined &&
    moveUci &&
    fenBefore &&
    fenAfter &&
    line &&
    value.scope === 'legal_line'
    ? {
        state,
        semanticProofId,
        issuerEvidenceId,
        issuerOccurrenceId,
        stepKey,
        ply,
        moveUci,
        fenBefore,
        fenAfter,
        line,
        scope: value.scope,
      }
    : undefined;
}

function projectPassedPawnPositionState(value: unknown): MoveReviewPassedPawnPositionState | undefined {
  if (!isObject(value)) return;
  const side = wireColor(value.side);
  const square = key(value.square);
  if (value.kind === 'occupied_by') {
    if (!hasExactKeys(value, ['kind', 'side', 'square', 'piece'])) return;
    const piece = pieceRole(value.piece);
    return side && square && piece ? { kind: value.kind, side, square, piece } : undefined;
  }
  if (value.kind === 'pawn_topology') {
    if (!hasExactKeys(value, ['kind', 'side', 'square', 'passed'])) return;
    return side && square && typeof value.passed === 'boolean'
      ? { kind: value.kind, side, square, passed: value.passed }
      : undefined;
  }
  if (value.kind !== 'slider_reach') return;
  if (!hasExactKeys(value, ['kind', 'side', 'square', 'piece', 'file_step', 'rank_step', 'segment'])) return;
  const piece =
    value.piece === 'bishop' || value.piece === 'rook' || value.piece === 'queen' ? value.piece : undefined;
  const fileStep = integerInRange(value.file_step, -1, 1);
  const rankStep = integerInRange(value.rank_step, -1, 1);
  const segment = projectArray(value.segment, 0, undefined, projectPassedPawnSliderSegment);
  return side &&
    square &&
    piece &&
    fileStep !== undefined &&
    rankStep !== undefined &&
    (fileStep !== 0 || rankStep !== 0) &&
    segment
    ? { kind: value.kind, side, square, piece, fileStep, rankStep, segment }
    : undefined;
}

function projectPassedPawnSliderSegment(
  value: unknown,
): Extract<MoveReviewPassedPawnPositionState, { kind: 'slider_reach' }>['segment'][number] | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['square', 'target', 'occupant_piece'])) return;
  const square = key(value.square);
  if (!square) return;
  if (value.target === 'empty' && value.occupant_piece === null)
    return { square, target: value.target, occupantPiece: null };
  const occupantPiece = pieceRole(value.occupant_piece);
  return (value.target === 'friendly' || value.target === 'enemy') && occupantPiece
    ? { square, target: value.target, occupantPiece }
    : undefined;
}

function projectOccurrenceBranch<Role extends MoveReviewOccurrenceBranchRole>(
  value: unknown,
  branchRole: Role,
  minimumSteps: number,
  maximumSteps?: number,
): MoveReviewOccurrenceBranch<Role> | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, [
      'branch_id',
      'line_id',
      'line_owner_evidence_id',
      'root_transition_evidence_id',
      'branch_role',
      'root_provenance',
      'root_move',
      'steps',
    ]) ||
    value.branch_role !== branchRole
  )
    return;
  const branchId = typedHash(value.branch_id);
  const lineId = nonEmptyWireString(value.line_id);
  const lineOwnerEvidenceId = nonEmptyWireString(value.line_owner_evidence_id);
  const rootTransitionEvidenceId = nonEmptyWireString(value.root_transition_evidence_id);
  const rootProvenance = projectRootProvenance(value.root_provenance);
  const rootMove = uci(value.root_move);
  const steps = projectArray(value.steps, minimumSteps, maximumSteps, projectOccurrenceStep);
  if (
    !branchId ||
    !lineId ||
    !lineOwnerEvidenceId ||
    !rootTransitionEvidenceId ||
    !rootProvenance ||
    !rootMove ||
    !steps ||
    !occurrenceStepsAreValid(steps, rootProvenance, rootMove)
  )
    return;
  return {
    branchId,
    lineId,
    lineOwnerEvidenceId,
    rootTransitionEvidenceId,
    branchRole,
    rootProvenance,
    rootMove,
    steps,
  };
}

function projectOccurrenceStep(value: unknown): MoveReviewOccurrenceStep | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['step_index', 'provenance', 'ply', 'move_uci', 'fen_before', 'fen_after'])
  )
    return;
  const stepIndex = nonNegativeInteger(value.step_index);
  const provenance = stepProvenance(value.provenance);
  const ply = positiveInteger(value.ply);
  const moveUci = uci(value.move_uci);
  const fenBefore = fenText(value.fen_before);
  const fenAfter = fenText(value.fen_after);
  return stepIndex !== undefined && provenance && ply !== undefined && moveUci && fenBefore && fenAfter
    ? { stepIndex, provenance, ply, moveUci, fenBefore, fenAfter }
    : undefined;
}

function occurrenceStepsAreValid(
  steps: readonly (MoveReviewOccurrenceStep | MoveReviewPassedPawnStep)[],
  rootProvenance: MoveReviewRootProvenance,
  rootMove: Uci,
): boolean {
  return (
    steps.length > 0 &&
    steps[0]!.moveUci === rootMove &&
    steps.every(
      (step, index) =>
        step.stepIndex === index &&
        (index === 0 ||
          (step.ply === steps[index - 1]!.ply + 1 && step.fenBefore === steps[index - 1]!.fenAfter)),
    ) &&
    (rootProvenance === 'observed_game_root'
      ? steps[0]!.provenance === 'observed_game_move' &&
        steps.slice(1).every(step => step.provenance === 'certified_analysis_move')
      : steps.every(step => step.provenance === 'certified_analysis_move'))
  );
}

function projectRootProvenance(value: unknown): MoveReviewRootProvenance | undefined {
  return value === 'counterfactual_analyzed_root' || value === 'observed_game_root' ? value : undefined;
}

function stepProvenance(value: unknown): MoveReviewStepProvenance | undefined {
  return value === 'observed_game_move' || value === 'certified_analysis_move' ? value : undefined;
}

interface ProjectedBoundedPathShell {
  pathOccurrenceId: string;
  premises: unknown[];
  closedAbsenceUses: unknown[];
  closedStateUses: unknown[];
}

function projectBoundedPathShell(
  value: unknown,
  minimumPremiseCount: number,
  absenceCount: number,
  minimumStateCount: number,
  maximumStateCount?: number,
  maximumPremiseCount?: number,
): ProjectedBoundedPathShell | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['path_occurrence_id', 'premises', 'closed_absence_uses', 'closed_state_uses'])
  )
    return;
  const pathOccurrenceId = typedHash(value.path_occurrence_id);
  if (
    !pathOccurrenceId ||
    !Array.isArray(value.premises) ||
    value.premises.length < minimumPremiseCount ||
    (maximumPremiseCount !== undefined && value.premises.length > maximumPremiseCount) ||
    !Array.isArray(value.closed_absence_uses) ||
    value.closed_absence_uses.length !== absenceCount ||
    !Array.isArray(value.closed_state_uses) ||
    value.closed_state_uses.length < minimumStateCount ||
    (maximumStateCount !== undefined && value.closed_state_uses.length > maximumStateCount)
  )
    return;
  return {
    pathOccurrenceId,
    premises: value.premises,
    closedAbsenceUses: value.closed_absence_uses,
    closedStateUses: value.closed_state_uses,
  };
}

function projectRelationPremise<
  Role extends string,
  Contract extends string,
  BranchRole extends MoveReviewOccurrenceBranchRole,
>(
  value: unknown,
  role: Role,
  contract: Contract | readonly Contract[],
  branchRole: BranchRole,
  stepIsValid: (step: number) => boolean,
): MoveReviewRelationPremiseUse<Role, Contract, BranchRole> | undefined {
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
    value.role !== role ||
    value.branch_role !== branchRole
  )
    return;
  const contracts = Array.isArray(contract) ? contract : [contract];
  const selectedContract = contracts.find(item => item === value.contract);
  const resultId = nonEmptyWireString(value.result_id);
  const issuerEvidenceId = nonEmptyWireString(value.issuer_evidence_id);
  const issuerOccurrenceId = typedHash(value.issuer_occurrence_id);
  const sourcePremiseIds = wireUniqueStrings(value.source_premise_ids, 3);
  const branchId = typedHash(value.branch_id);
  const stepIndex = nonNegativeInteger(value.step_index);
  return selectedContract &&
    resultId &&
    resultIdMatchesContract(resultId, selectedContract) &&
    issuerEvidenceId &&
    issuerOccurrenceId &&
    sourcePremiseIds &&
    branchId &&
    stepIndex !== undefined &&
    stepIsValid(stepIndex)
    ? {
        role,
        contract: selectedContract,
        resultId,
        issuerEvidenceId,
        issuerOccurrenceId,
        sourcePremiseIds,
        branchId,
        branchRole,
        stepIndex,
      }
    : undefined;
}

function projectLegalMovePremise<Role extends string, BranchRole extends MoveReviewOccurrenceBranchRole>(
  value: unknown,
  roleIsValid: (role: string) => boolean,
  branchRoles: readonly BranchRole[],
  stepIsValid: (step: number) => boolean = () => true,
  captureRequired = false,
): MoveReviewLegalMovePremiseUse<Role, BranchRole> | undefined {
  const required = [
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
  ];
  if (
    !isObject(value) ||
    !hasOnlyKeys(value, [...required, 'capture'], required) ||
    value.contract !== 'legal_move'
  )
    return;
  const role = nonEmptyWireString(value.role);
  const branchRole = branchRoles.find(item => item === value.branch_role);
  const moveUci = uci(value.move_uci);
  const movement = projectMovementWitness(value.movement);
  const movementMode = projectMovementMode(value.movement_mode);
  const legalMoveSemanticId = typedHash(value.legal_move_semantic_id);
  const issuerEvidenceId = nonEmptyWireString(value.issuer_evidence_id);
  const issuerOccurrenceId = typedHash(value.issuer_occurrence_id);
  const sourcePremiseIds = wireUniqueStrings(value.source_premise_ids, 3);
  const branchId = typedHash(value.branch_id);
  const stepIndex = nonNegativeInteger(value.step_index);
  const hasCapture = Object.prototype.hasOwnProperty.call(value, 'capture');
  const capture = hasCapture ? projectColoredPieceWitness(value.capture) : undefined;
  if (
    !role ||
    !roleIsValid(role) ||
    !branchRole ||
    !moveUci ||
    !movement ||
    !movementMode ||
    !legalMoveSemanticId ||
    !issuerEvidenceId ||
    !issuerOccurrenceId ||
    !sourcePremiseIds ||
    !branchId ||
    stepIndex === undefined ||
    !stepIsValid(stepIndex) ||
    (hasCapture && !capture) ||
    (captureRequired && !capture)
  )
    return;
  return {
    role: role as Role,
    contract: value.contract,
    moveUci,
    movement,
    movementMode,
    legalMoveSemanticId,
    issuerEvidenceId,
    issuerOccurrenceId,
    sourcePremiseIds,
    branchId,
    branchRole,
    stepIndex,
    ...(capture ? { capture } : {}),
  };
}

interface ClosureProjectionSpec {
  role: string | readonly string[] | ((role: string) => boolean);
  issuer: MoveReviewClosureIssuer;
  branchRoles: readonly MoveReviewOccurrenceBranchRole[];
  query: RegExp;
  afterStep?: (step: number) => boolean;
}

function projectClosureUse(value: unknown, spec: ClosureProjectionSpec): MoveReviewClosureUse | undefined {
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
    value.issuer !== spec.issuer ||
    !isObject(value.position) ||
    !hasExactKeys(value.position, ['fen', 'ply', 'scope']) ||
    value.position.scope !== 'legal_line'
  )
    return;
  const useId = typedHash(value.use_id);
  const role = nonEmptyWireString(value.role);
  const semanticProofId = typedHash(value.semantic_proof_id);
  const issuerEvidenceId = nonEmptyWireString(value.issuer_evidence_id);
  const issuerOccurrenceId = typedHash(value.issuer_occurrence_id);
  const query = nonEmptyWireString(value.query);
  const branchId = typedHash(value.branch_id);
  const branchRole = spec.branchRoles.find(item => item === value.branch_role);
  const afterStepIndex = nonNegativeInteger(value.after_step_index);
  const position = projectOccurrencePosition({ fen: value.position.fen, ply: value.position.ply }, true);
  const roleIsValid =
    typeof spec.role === 'function'
      ? role !== undefined && spec.role(role)
      : Array.isArray(spec.role)
        ? role !== undefined && spec.role.includes(role)
        : role === spec.role;
  return useId &&
    role &&
    roleIsValid &&
    semanticProofId &&
    issuerEvidenceId &&
    issuerOccurrenceId &&
    query &&
    spec.query.test(query) &&
    branchId &&
    branchRole &&
    afterStepIndex !== undefined &&
    (spec.afterStep?.(afterStepIndex) ?? true) &&
    position
    ? {
        useId,
        role,
        semanticProofId,
        issuer: spec.issuer,
        issuerEvidenceId,
        issuerOccurrenceId,
        query,
        branchId,
        branchRole,
        afterStepIndex,
        position: { ...position, scope: value.position.scope },
      }
    : undefined;
}

function projectMovementWitness(value: unknown): MoveReviewMovementWitness | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['side', 'from', 'to', 'piece_before', 'piece_after'])) return;
  const side = wireColor(value.side);
  const from = key(value.from);
  const to = key(value.to);
  const pieceBefore = pieceRole(value.piece_before);
  const pieceAfter = pieceRole(value.piece_after);
  return side && from && to && pieceBefore && pieceAfter
    ? { side, from, to, pieceBefore, pieceAfter }
    : undefined;
}

function projectLegalResourceWitness(value: unknown): MoveReviewLegalResourceWitness | undefined {
  if (
    !isObject(value) ||
    !hasExactKeys(value, ['side', 'from', 'to', 'piece_before', 'piece_after', 'move_uci'])
  )
    return;
  const movement = projectMovementWitness({
    side: value.side,
    from: value.from,
    to: value.to,
    piece_before: value.piece_before,
    piece_after: value.piece_after,
  });
  const moveUci = uci(value.move_uci);
  return movement && moveUci ? { ...movement, moveUci } : undefined;
}

function projectPieceWitness(value: unknown): MoveReviewPieceWitness | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['piece', 'square'])) return;
  const piece = pieceRole(value.piece);
  const square = key(value.square);
  return piece && square ? { piece, square } : undefined;
}

function projectColoredPieceWitness(value: unknown): MoveReviewColoredPieceWitness | undefined {
  if (!isObject(value) || !hasExactKeys(value, ['side', 'piece', 'square'])) return;
  const side = wireColor(value.side);
  const piece = pieceRole(value.piece);
  const square = key(value.square);
  return side && piece && square ? { side, piece, square } : undefined;
}

function projectMovementMode(value: unknown): MoveReviewMovementMode | undefined {
  return value === 'controlled_destination' ||
    value === 'pawn_advance' ||
    value === 'pawn_double_advance' ||
    value === 'castling'
    ? value
    : undefined;
}

function boundedPathsFitBranches(
  paths: readonly MoveReviewBoundedProofPath[],
  branches: readonly MoveReviewOccurrenceBranch[],
): boolean {
  const branchById = new Map(branches.map(branch => [branch.branchId, branch]));
  return paths.every(path =>
    [...path.premises, ...path.closedAbsenceUses, ...path.closedStateUses].every(item => {
      const branch = branchById.get(item.branchId);
      const stepIndex = 'stepIndex' in item ? item.stepIndex : item.afterStepIndex;
      const step = branch?.steps[stepIndex];
      const move =
        'moveUci' in item ? item.moveUci : 'overallMoveUci' in item ? item.overallMoveUci : undefined;
      return branch?.branchRole === item.branchRole && !!step && (!move || step.moveUci === move);
    }),
  );
}

function passedPawnPathsFitBranch(
  paths: readonly MoveReviewPassedPawnProofPath[],
  branch: MoveReviewPassedPawnBranch,
): boolean {
  return paths.every(
    path =>
      path.analysisContinuationBranchId === branch.branchId &&
      path.premises.every(
        premise =>
          premise.branchId === branch.branchId &&
          premise.branchRole === branch.branchRole &&
          !!branch.steps[premise.fromStepIndex] &&
          !!branch.steps[premise.toStepIndex],
      ),
  );
}

function occurrenceProofOwnsSubject(explanation: MoveReviewOccurrenceExplanation): boolean {
  const subject = explanation.subjectOccurrence;
  const branches = moveReviewOccurrenceBranches(explanation);
  const observed = branches.filter(branch => branch.rootProvenance === 'observed_game_root');
  if (observed.length !== 1) return false;
  const branch = observed[0]!;
  const first = branch.steps[0]!;
  const lineId = 'lineId' in branch ? branch.lineId : branch.line.lineId;
  const rootMove = 'rootMove' in branch ? branch.rootMove : branch.line.rootMove;
  return (
    lineId === subject.lineId &&
    branch.lineOwnerEvidenceId === subject.lineOwnerEvidenceId &&
    branch.rootTransitionEvidenceId === subject.transitionEvidenceId &&
    rootMove === subject.moveUci &&
    first.moveUci === subject.moveUci &&
    first.ply === subject.destination.ply &&
    first.fenBefore === subject.start.fen &&
    first.fenAfter === subject.destination.fen &&
    branches.every(item => item.steps[0]?.fenBefore === subject.start.fen)
  );
}

export function moveReviewOccurrenceBranches(
  explanation: MoveReviewOccurrenceExplanation,
): MoveReviewAnyBranch[] {
  switch (explanation.proofKind) {
    case 'unique_check_reply_defender_displacement_before_capture':
      return [explanation.proof.displacementBranch, explanation.proof.immediateCaptureBranch];
    case 'sole_recapturer_removal_before_target_capture':
      return [explanation.proof.removalBranch, explanation.proof.immediateCaptureBranch];
    case 'vacated_gate_enables_unrecapturable_slider_capture':
      return [explanation.proof.vacatedGateBranch, explanation.proof.retainedGateBranch];
    case 'square_release_route':
      return [explanation.proof.releasedRouteBranch, explanation.proof.retainedBlockerBranch];
    case 'capture_exclusion_move_order':
      return [explanation.proof.vacatingBranch, explanation.proof.immediateCaptureBranch];
    case 'relocation_enables_recapture':
      return [explanation.proof.relocatedResponderBranch, explanation.proof.retainedResponderBranch];
    case 'passed_pawn_progress_realized_after_only_legal_reply':
      return explanation.proof.branches;
  }
}

export function moveReviewOccurrenceProofPaths(
  explanation: MoveReviewOccurrenceExplanation,
): MoveReviewAnyProofPath[] {
  return explanation.proof.proofPaths;
}

export function moveReviewOccurrenceBranchProof(
  explanation: MoveReviewOccurrenceExplanation,
  branchIndex: number,
): MoveReviewProof | undefined {
  const branch = moveReviewOccurrenceBranches(explanation)[branchIndex];
  if (!branch) return;
  const annotations: MoveReviewAnnotation[] =
    explanation.proofKind === 'relocation_enables_recapture'
      ? branchIndex === 0
        ? [
            {
              atPly: explanation.proof.relocation.stepIndex + 1,
              shape: {
                kind: 'arrow',
                orig: explanation.proof.relocation.movement.from,
                dest: explanation.proof.relocation.movement.to,
                brush: 'blue',
              },
            },
            {
              atPly: explanation.proof.targetCapture.relocatedStepIndex + 1,
              shape: {
                kind: 'arrow',
                orig: explanation.proof.targetCapture.movement.from,
                dest: explanation.proof.targetCapture.movement.to,
                brush: 'red',
              },
            },
            {
              atPly: explanation.proof.relocatedResponderRecapture.stepIndex + 1,
              shape: {
                kind: 'arrow',
                orig: explanation.proof.relocatedResponderRecapture.movement.from,
                dest: explanation.proof.relocatedResponderRecapture.movement.to,
                brush: 'green',
              },
            },
          ]
        : [
            {
              atPly: explanation.proof.targetCapture.retainedStepIndex + 1,
              shape: {
                kind: 'arrow',
                orig: explanation.proof.targetCapture.movement.from,
                dest: explanation.proof.targetCapture.movement.to,
                brush: 'red',
              },
            },
            {
              atPly: explanation.proof.retainedOtherRecapture.stepIndex + 1,
              shape: {
                kind: 'arrow',
                orig: explanation.proof.retainedOtherRecapture.movement.from,
                dest: explanation.proof.retainedOtherRecapture.movement.to,
                brush: 'green',
              },
            },
          ]
      : [];
  return proofFromOccurrenceSteps(explanation.id + '-branch-' + branchIndex, branch.steps, annotations);
}

export function moveReviewProofById(
  review: MoveReviewCandidateReview,
  proofId: string,
): MoveReviewProof | undefined {
  if (review.kind === 'single-candidate-insight')
    return review.proof.id === proofId ? review.proof : undefined;
  if (review.kind !== 'move-verdict') return;
  if (review.comparisonProof?.id === proofId) return review.comparisonProof;
  for (const explanation of review.explanations)
    for (const [index] of moveReviewOccurrenceBranches(explanation).entries()) {
      const proof = moveReviewOccurrenceBranchProof(explanation, index);
      if (proof?.id === proofId) return proof;
    }
  return;
}

function proofFromOccurrenceSteps(
  id: string,
  steps: readonly (MoveReviewOccurrenceStep | MoveReviewPassedPawnStep)[],
  annotations: MoveReviewAnnotation[],
): MoveReviewProof | undefined {
  if (!semanticId(id) || steps.length < 1) return;
  return {
    id,
    startFen: steps[0]!.fenBefore,
    moves: steps.map(step => ({
      uci: step.moveUci,
      label: step.moveUci,
      fenAfter: step.fenAfter,
    })),
    annotations,
  };
}

function projectArray<T>(
  value: unknown,
  minimum: number,
  maximum: number | undefined,
  project: (item: unknown) => T | undefined,
): T[] | undefined {
  if (!Array.isArray(value) || value.length < minimum || (maximum !== undefined && value.length > maximum))
    return;
  const projected = value.map(project);
  return projected.every((item): item is T => item !== undefined) ? projected : undefined;
}

function wireColor(value: unknown): MoveReviewColor | undefined {
  return value === 'white' || value === 'black' ? value : undefined;
}

function wireUniqueStrings(
  value: unknown,
  minimum: number,
  validator: (item: unknown) => string | undefined = nonEmptyWireString,
): string[] | undefined {
  if (!Array.isArray(value) || value.length < minimum) return;
  const strings = value.map(validator);
  return strings.every((item): item is string => !!item) && unique(strings) ? strings : undefined;
}

function typedHash(value: unknown): string | undefined {
  return typeof value === 'string' && sha256Pattern.test(value) ? value : undefined;
}

function positiveInteger(value: unknown): number | undefined {
  const integer = nonNegativeInteger(value);
  return integer !== undefined && integer >= 1 ? integer : undefined;
}

function resultIdMatchesContract(value: string, contract: string): boolean {
  return new RegExp('^' + contract + ':[0-9a-f]{64}$').test(value);
}

const legalCaptureQuery = /^legal-capture:(white|black):[a-h][1-8]$/;
const legalMoveQuery = /^legal-move-from-to:(white|black):[a-h][1-8]:[a-h][1-8]$/;
const legalCaptureOrMoveQuery =
  /^(legal-capture:(white|black):[a-h][1-8]|legal-move-from-to:(white|black):[a-h][1-8]:[a-h][1-8])$/;
const occupiedByQuery = /^occupied-by:(white|black):(pawn|knight|bishop|rook|queen|king)@[a-h][1-8]$/;
const vacantOrOccupiedQuery =
  /^(vacant:[a-h][1-8]|occupied-by:(white|black):(pawn|knight|bishop|rook|queen|king)@[a-h][1-8])$/;
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
const squarePattern = /^[a-h][1-8]$/;
const uciPattern = /^[a-h][1-8][a-h][1-8][qrbn]?$/;
