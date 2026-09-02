import type { DrawShape } from '@lichess-org/chessground/draw';
import { renderBoardPreview } from 'lib/view/boardPreview';
import { hl, type VNode } from 'lib/view/snabbdom';
import {
  formatMoveReviewPercent,
  formatMoveReviewPercentagePointChange,
  moveReviewOccurrenceBranchProof,
  moveReviewOccurrenceBranches,
  moveReviewOccurrenceProofPaths,
  moveReviewVerdictCodeLabel,
  selectedMoveReviewCandidate,
  type MoveReviewAnnotationShape,
  type MoveReviewAnyBranch,
  type MoveReviewAnyProofPath,
  type MoveReviewCandidate,
  type MoveReviewCandidateRole,
  type MoveReviewCandidateReview,
  type MoveReviewClosureUse,
  type MoveReviewColoredPieceWitness,
  type MoveReviewCopy,
  type MoveReviewCore,
  type MoveReviewDrawClaim,
  type MoveReviewEvidence,
  type MoveReviewFrameSelection,
  type MoveReviewJobState,
  type MoveReviewLocale,
  type MoveReviewMovementWitness,
  type MoveReviewOccurrenceExplanation,
  type MoveReviewPassedPawnPositionState,
  type MoveReviewProof,
  type MoveReviewVerdictSymbol,
  type MoveReviewViewState,
} from '../moveReview';

type MoveReviewVerdictTone = 'neutral' | 'inaccuracy' | 'mistake' | 'blunder';

interface MoveReviewPanelActions {
  selectCandidate(uci: Uci): void;
  toggleEvidence(): void;
  toggleProof(proofId: string): void;
  previewFrame(frame: MoveReviewFrameSelection): void;
  clearPreview(): void;
  pinFrame(frame: MoveReviewFrameSelection): void;
  clearPin(): void;
  retry(): void;
  addProof(proofId: string): void;
  viewAddedLine(proofId: string): void;
}

export interface MoveReviewPanelProps {
  job: MoveReviewJobState;
  view: MoveReviewViewState;
  copy: MoveReviewCopy;
  locale: MoveReviewLocale;
  orientation: Color;
  canWrite: boolean;
  liveEnginePaused: boolean;
  addedProofId?: string;
  actions: MoveReviewPanelActions;
}

export function renderMoveReview(props: MoveReviewPanelProps): VNode {
  const titleId = 'analyse-move-review-title';
  return hl(
    'section.analyse__move-review',
    {
      attrs: {
        'aria-labelledby': titleId,
        'data-state': props.job.kind,
        lang: props.locale,
      },
    },
    [
      hl('header.move-review__header', [
        hl(`h2#${titleId}`, props.copy.title),
        hl(
          'span.move-review__status',
          { attrs: { role: 'status', 'aria-live': 'polite', 'aria-atomic': 'true' } },
          moveReviewStateLabel(props.job, props.copy),
        ),
      ]),
      props.liveEnginePaused && hl('div.move-review__live-engine-note', props.copy.liveEnginePaused),
      renderPanelContent(props),
    ],
  );
}

export function renderMoveReviewNotationBadge(
  symbol: MoveReviewVerdictSymbol,
  accessibleLabel: string,
): VNode | undefined {
  if (symbol === 'none') return;
  const tone = moveReviewVerdictTone(symbol);
  return hl(
    `span.move-review__notation-badge.move-review__notation-badge--${tone}`,
    {
      attrs: {
        title: accessibleLabel,
        'aria-label': accessibleLabel,
      },
    },
    symbol,
  );
}

function moveReviewVerdictTone(symbol: MoveReviewVerdictSymbol): MoveReviewVerdictTone {
  switch (symbol) {
    case 'none':
      return 'neutral';
    case '?!':
      return 'inaccuracy';
    case '?':
      return 'mistake';
    case '??':
      return 'blunder';
  }
}

function moveReviewStateLabel(job: MoveReviewJobState, copy: MoveReviewCopy): string {
  switch (job.kind) {
    case 'idle':
      return '';
    case 'loading':
      return copy.analysing;
    case 'completed':
    case 'position-action':
      return copy.completed;
    case 'abstained':
      return copy.analysisWithheld;
    case 'fault':
    case 'unsupported':
      return '';
  }
}

function renderPanelContent(props: MoveReviewPanelProps): VNode {
  const { job, copy } = props;
  switch (job.kind) {
    case 'idle':
      return renderState('idle', copy.idleTitle, copy.idleBody);
    case 'loading':
      return renderLoadingState(copy.analysing);
    case 'completed':
      return renderReviewBody(props, job.snapshot.evidence);
    case 'position-action':
      return renderPositionAction(job.snapshot.action, props);
    case 'abstained':
      return renderState('abstained', copy.unavailable, copy.analysisWithheld);
    case 'fault':
      return renderFailureState('error', copy.unavailable, job.message, job.retryable, props);
    case 'unsupported':
      return renderFailureState(
        'unsupported',
        copy.unsupported,
        job.message,
        job.reason === 'engine-unavailable',
        props,
      );
  }
}

function renderPositionAction(
  action: Extract<MoveReviewJobState, { kind: 'position-action' }>['snapshot']['action'],
  props: MoveReviewPanelProps,
): VNode {
  if (action.kind === 'automatic-terminal') {
    const terminal = action.terminal;
    const detail =
      terminal.kind === 'checkmate'
        ? `${props.copy.terminalLabels.checkmate} · ${props.copy.colorLabels[terminal.winner]}`
        : props.copy.terminalLabels[terminal.kind];
    return renderState('completed', props.copy.terminalResult, detail);
  }
  return renderState('completed', props.copy.drawClaimAvailable, drawClaimSummary(action.claims, props.copy));
}

function drawClaimSummary(claims: MoveReviewDrawClaim[], copy: MoveReviewCopy): string {
  return claims
    .map(claim => `${copy.drawRuleLabels[claim.rule]} · ${copy.drawAvailabilityLabels[claim.availability]}`)
    .join(' · ');
}

function renderState(kind: string, title: string, body: string): VNode {
  return hl(`div.move-review__state.move-review__state--${kind}`, [hl('h3', title), hl('p', body)]);
}

function renderLoadingState(label: string): VNode {
  return hl('div.move-review__state.move-review__state--loading', [
    hl('h3', label),
    hl('div.move-review__loading-line', { attrs: { 'aria-hidden': 'true' } }),
  ]);
}

function renderFailureState(
  kind: 'error' | 'unsupported',
  title: string,
  body: string,
  retryable: boolean,
  props: MoveReviewPanelProps,
): VNode {
  return hl(
    `div.move-review__state.move-review__state--${kind}`,
    { attrs: { role: kind === 'error' ? 'alert' : 'status', 'aria-atomic': 'true' } },
    [
      hl('h3', title),
      hl('p', body),
      retryable &&
        hl('div.move-review__state-actions', [
          hl(
            'button.button.button-thin.move-review__retry',
            {
              attrs: { type: 'button' },
              on: { click: props.actions.retry },
            },
            props.copy.retry,
          ),
        ]),
    ],
  );
}

function renderReviewBody(props: MoveReviewPanelProps, evidence: MoveReviewEvidence): VNode {
  const played = evidence.candidates.find(candidate => candidate.roles.includes('played'))!;
  const best = evidence.candidates.find(candidate => candidate.roles.includes('best'))!;
  const review = played.review;
  const core = review.kind === 'move-verdict' ? review.core : undefined;
  const verdictLabel = core
    ? moveReviewVerdictCodeLabel(core.verdictCode, props.copy)
    : review.kind === 'single-candidate-insight'
      ? props.copy.lineInsight
      : review.kind === 'forced-single-move'
        ? props.copy.forcedSingleMove
        : props.copy.unavailable;
  const hasBadge = !!core && core.verdictSymbol !== 'none';
  const tone = core ? moveReviewVerdictTone(core.verdictSymbol) : 'neutral';
  return hl('div.move-review__body', [
    hl(
      'section.move-review__summary',
      {
        class: {
          'has-no-badge': !hasBadge,
        },
        attrs: { 'aria-label': `${verdictLabel}: ${played.label}` },
      },
      [
        hasBadge &&
          hl(
            `span.move-review__verdict-badge.move-review__verdict-badge--${tone}`,
            { attrs: { 'aria-hidden': 'true' } },
            core!.verdictSymbol,
          ),
        hl('div.move-review__summary-copy', [
          hl('span.move-review__eyebrow', played.label),
          hl('h3', verdictLabel),
          hl('p.move-review__core-comparison', renderCoreComparison(played, best, core, props.copy)),
          review.kind === 'move-verdict' && !review.explanations.length
            ? hl('p', props.copy.noVerifiedExplanation)
            : review.kind === 'abstained'
              ? hl('p', props.copy.candidateUnavailable)
              : undefined,
        ]),
      ],
    ),
    core?.winChance && renderMetrics(core.winChance, props),
    core && (core.referenceTerminal || core.reviewedTerminal) && renderTerminalOutcomes(core, props),
    evidence.drawClaims &&
      renderState(
        'completed',
        props.copy.drawClaimAvailable,
        drawClaimSummary(evidence.drawClaims, props.copy),
      ),
    renderEvidenceDisclosure(props, evidence),
  ]);
}

function renderCoreComparison(
  played: MoveReviewCandidate,
  best: MoveReviewCandidate,
  core: MoveReviewCore | undefined,
  copy: MoveReviewCopy,
): string {
  if (core?.kind === 'best-choice')
    return `${copy.best} ${played.label} · ${copy.runnerUp} ${core.bestChoice.runnerUpUci}: ${moveReviewVerdictCodeLabel(
      core.bestChoice.runnerUpVerdictCode,
      copy,
    )}`;
  return played.uci === best.uci
    ? `${copy.played} + ${copy.best} · ${played.label}`
    : `${copy.played} ${played.label} · ${copy.best} ${best.label}`;
}

function renderMetrics(
  winChance: NonNullable<MoveReviewCore['winChance']>,
  props: MoveReviewPanelProps,
): VNode {
  return hl('dl.move-review__metrics', [
    hl('div', [
      hl('dt.move-review__metric-label', props.copy.winChance),
      hl(
        'dd',
        `${formatMoveReviewPercent(winChance.referencePercent, props.locale)} → ${formatMoveReviewPercent(
          winChance.playedPercent,
          props.locale,
        )}`,
      ),
    ]),
    hl('div', [
      hl('dt.move-review__metric-label', props.copy.winChanceChange),
      hl('dd', formatMoveReviewPercentagePointChange(winChance.changePercentagePoints, props.locale)),
    ]),
  ]);
}

function renderTerminalOutcomes(core: MoveReviewCore, props: MoveReviewPanelProps): VNode {
  const label = (terminal: NonNullable<MoveReviewCore['referenceTerminal']>): string =>
    terminal.kind === 'checkmate'
      ? `${props.copy.terminalLabels.checkmate} · ${props.copy.colorLabels[terminal.winner]}`
      : props.copy.terminalLabels[terminal.kind];
  return hl('dl.move-review__outcomes', [
    core.referenceTerminal &&
      hl('div', [
        hl('dt', core.kind === 'best-choice' ? props.copy.runnerUp : props.copy.best),
        hl('dd', label(core.referenceTerminal)),
      ]),
    core.reviewedTerminal &&
      hl('div', [
        hl('dt', core.kind === 'best-choice' ? props.copy.best : props.copy.played),
        hl('dd', label(core.reviewedTerminal)),
      ]),
  ]);
}

function renderEvidenceDisclosure(props: MoveReviewPanelProps, evidence: MoveReviewEvidence): VNode {
  const expanded = props.view.evidenceExpanded;
  const contentId = 'move-review-evidence';
  return hl('div.move-review__evidence-disclosure', [
    hl(
      'button.button.button-thin.move-review__evidence-toggle',
      {
        attrs: {
          type: 'button',
          'aria-expanded': expanded ? 'true' : 'false',
          'aria-controls': contentId,
        },
        on: { click: props.actions.toggleEvidence },
      },
      expanded ? props.copy.hideEvidence : props.copy.showEvidence,
    ),
    hl(
      `div#${contentId}.move-review__evidence`,
      { attrs: { hidden: !expanded } },
      expanded ? renderEvidence(props, evidence) : [],
    ),
  ]);
}

function renderEvidence(props: MoveReviewPanelProps, evidence: MoveReviewEvidence): VNode[] {
  const selected = selectedMoveReviewCandidate(evidence, props.view.selectedCandidateUci)!;
  return [renderCandidates(evidence.candidates, selected, props), renderCandidateReview(selected, props)];
}

function renderCandidateReview(candidate: MoveReviewCandidate, props: MoveReviewPanelProps): VNode {
  const review = candidate.review;
  if (review.kind === 'abstained') return renderCandidateMessage(candidate, props.copy.candidateUnavailable);
  if (review.kind === 'forced-single-move') return renderForcedSingleMove(candidate, review, props);
  if (review.kind === 'single-candidate-insight') return renderInsight(candidate, review.proof, props);
  return renderStructuredReview(candidate, review, props);
}

function renderForcedSingleMove(
  candidate: MoveReviewCandidate,
  review: Extract<MoveReviewCandidate['review'], { kind: 'forced-single-move' }>,
  props: MoveReviewPanelProps,
): VNode {
  const terminal = review.terminal;
  const terminalLabel = terminal
    ? terminal.kind === 'checkmate'
      ? `${props.copy.terminalLabels.checkmate} · ${props.copy.colorLabels[terminal.winner]}`
      : props.copy.terminalLabels[terminal.kind]
    : undefined;
  return hl(
    'section#move-review-selected-candidate.move-review__section',
    {
      attrs: {
        role: 'tabpanel',
        'aria-labelledby': `move-review-candidate-${candidate.uci}`,
      },
    },
    [
      hl('p.move-review__candidate-message', props.copy.forcedSingleMove),
      hl('p', [hl('code', review.lineUcis.join(' '))]),
      terminalLabel && hl('p', `${props.copy.terminalResult}: ${terminalLabel}`),
    ],
  );
}

function renderCandidateMessage(candidate: MoveReviewCandidate, message: string): VNode {
  return hl(
    'section#move-review-selected-candidate.move-review__section',
    {
      attrs: {
        role: 'tabpanel',
        'aria-labelledby': `move-review-candidate-${candidate.uci}`,
      },
    },
    [hl('p.move-review__candidate-message', message)],
  );
}

function renderInsight(
  candidate: MoveReviewCandidate,
  proof: MoveReviewProof,
  props: MoveReviewPanelProps,
): VNode {
  const expanded = props.view.expandedProofId === proof.id;
  const contentId = `move-review-proof-${proof.id}`;
  return hl(
    'section#move-review-selected-candidate.move-review__section',
    {
      attrs: {
        role: 'tabpanel',
        'aria-labelledby': `move-review-candidate-${candidate.uci}`,
      },
    },
    [
      hl('article.move-review__proof-entry', [
        hl(
          'button.move-review__proof-entry-button',
          {
            attrs: {
              type: 'button',
              'aria-expanded': expanded ? 'true' : 'false',
              ...(expanded ? { 'aria-controls': contentId } : {}),
            },
            on: { click: () => props.actions.toggleProof(proof.id) },
          },
          [
            hl('span.move-review__proof-entry-copy', [
              hl('strong', props.copy.lineInsight),
              hl('span', candidate.label),
            ]),
            hl('span.move-review__proof-entry-chevron', { attrs: { 'aria-hidden': 'true' } }, '⌄'),
          ],
        ),
        expanded && renderProof(proof, contentId, props),
      ]),
    ],
  );
}

function renderCandidates(
  candidates: MoveReviewCandidate[],
  selected: MoveReviewCandidate,
  props: MoveReviewPanelProps,
): VNode {
  const panelId = 'move-review-selected-candidate';
  return hl('section.move-review__section', [
    hl(
      'div.move-review__candidates',
      { attrs: { role: 'tablist', 'aria-label': props.copy.candidateMoves } },
      candidates.map((candidate, index) => {
        const active = candidate.uci === selected.uci;
        return hl(
          'button.move-review__candidate',
          {
            key: candidate.uci,
            attrs: {
              type: 'button',
              role: 'tab',
              id: `move-review-candidate-${candidate.uci}`,
              'aria-selected': active ? 'true' : 'false',
              'aria-controls': panelId,
              tabindex: active ? '0' : '-1',
            },
            on: {
              click: () => props.actions.selectCandidate(candidate.uci),
              keydown: event => moveCandidateSelection(event, index, candidates, props.actions),
            },
          },
          [
            hl('span.move-review__candidate-role', candidateRoleLabel(candidate.roles, props.copy)),
            hl('span.move-review__candidate-san', candidate.label),
            candidate.winPercent !== undefined &&
              hl(
                'span.move-review__candidate-win',
                formatMoveReviewPercent(candidate.winPercent, props.locale),
              ),
          ],
        );
      }),
    ),
  ]);
}

function moveCandidateSelection(
  event: KeyboardEvent,
  index: number,
  candidates: MoveReviewCandidate[],
  actions: MoveReviewPanelActions,
): void {
  let target: number | undefined;
  if (event.key === 'ArrowLeft' || event.key === 'ArrowUp')
    target = (index - 1 + candidates.length) % candidates.length;
  else if (event.key === 'ArrowRight' || event.key === 'ArrowDown') target = (index + 1) % candidates.length;
  else if (event.key === 'Home') target = 0;
  else if (event.key === 'End') target = candidates.length - 1;
  if (target === undefined) return;
  event.preventDefault();
  const candidate = candidates[target];
  const tabs = (event.currentTarget as HTMLElement).parentElement?.querySelectorAll<HTMLElement>(
    '[role="tab"]',
  );
  tabs?.[target]?.focus();
  actions.selectCandidate(candidate.uci);
}

function candidateRoleLabel(roles: MoveReviewCandidateRole[], copy: MoveReviewCopy): string {
  return roles.map(role => copy[role]).join(' + ');
}

type MoveReviewVerdictReview = Extract<MoveReviewCandidateReview, { kind: 'move-verdict' }>;

function renderStructuredReview(
  candidate: MoveReviewCandidate,
  review: MoveReviewVerdictReview,
  props: MoveReviewPanelProps,
): VNode {
  const panelId = 'move-review-selected-candidate';
  return hl(
    `section#${panelId}.move-review__section`,
    {
      attrs: {
        role: 'tabpanel',
        'aria-labelledby': `move-review-candidate-${candidate.uci}`,
      },
    },
    [
      review.comparisonProof && renderAssessmentProof(review.comparisonProof, candidate, props),
      ...review.explanations.map((explanation, index) =>
        renderOccurrenceExplanation(explanation, index, props),
      ),
      !review.comparisonProof &&
        !review.explanations.length &&
        hl('p.move-review__empty-explanation', props.copy.noVerifiedExplanation),
    ],
  );
}

function renderAssessmentProof(
  proof: MoveReviewProof,
  candidate: MoveReviewCandidate,
  props: MoveReviewPanelProps,
): VNode {
  const expanded = props.view.expandedProofId === proof.id;
  const contentId = `move-review-proof-${proof.id}`;
  return hl('article.move-review__proof-entry', { key: proof.id }, [
    hl(
      'button.move-review__proof-entry-button',
      {
        attrs: {
          type: 'button',
          'aria-expanded': expanded ? 'true' : 'false',
          ...(expanded ? { 'aria-controls': contentId } : {}),
        },
        on: { click: () => props.actions.toggleProof(proof.id) },
      },
      [
        hl('span.move-review__proof-entry-copy', [
          hl('strong', props.copy.lineInsight),
          hl('span.move-review__proof-entry-summary', [hl('code', candidate.label)]),
        ]),
        hl('span.move-review__proof-entry-chevron', { attrs: { 'aria-hidden': 'true' } }, '⌄'),
      ],
    ),
    expanded && renderProof(proof, contentId, props),
  ]);
}

function renderOccurrenceExplanation(
  explanation: MoveReviewOccurrenceExplanation,
  index: number,
  props: MoveReviewPanelProps,
): VNode {
  const expanded = props.view.expandedProofId === explanation.id;
  const contentId = `move-review-proof-${explanation.id}`;
  return hl('article.move-review__proof-entry', { key: explanation.id }, [
    hl(
      'button.move-review__proof-entry-button',
      {
        attrs: {
          type: 'button',
          'aria-expanded': expanded ? 'true' : 'false',
          ...(expanded ? { 'aria-controls': contentId } : {}),
        },
        on: { click: () => props.actions.toggleProof(explanation.id) },
      },
      [
        hl('span.move-review__proof-entry-copy', [
          hl('strong', props.copy.familyLabels[explanation.proofKind]),
          hl('span.move-review__proof-entry-summary', [
            hl('span', `${props.copy.proofPath} ${index + 1}`),
            hl('code.move-review__typed-value.is-move', explanation.subjectOccurrence.moveUci),
          ]),
        ]),
        hl('span.move-review__proof-entry-chevron', { attrs: { 'aria-hidden': 'true' } }, '⌄'),
      ],
    ),
    expanded && renderOccurrenceExplanationDetails(explanation, contentId, props),
  ]);
}

function renderOccurrenceExplanationDetails(
  explanation: MoveReviewOccurrenceExplanation,
  contentId: string,
  props: MoveReviewPanelProps,
): VNode {
  const paths = moveReviewOccurrenceProofPaths(explanation);
  const branches = moveReviewOccurrenceBranches(explanation);
  return hl(`div#${contentId}.move-review__typed-path`, [
    hl(
      'div.move-review__branches',
      branches.map((branch, index) => renderOccurrenceBranch(explanation, branch, index, props)),
    ),
    renderOccurrenceParticipants(explanation, props.copy),
    ...paths.map((path, index) => renderOccurrenceProofPath(path, index, props.copy)),
    renderLaterConsumer(explanation, props.copy),
    renderOccurrenceProvenance(explanation, branches, paths, props.copy),
  ]);
}

function renderOccurrenceProofPath(path: MoveReviewAnyProofPath, index: number, copy: MoveReviewCopy): VNode {
  const absences = 'closedAbsenceUses' in path ? path.closedAbsenceUses : [];
  const states = 'closedStateUses' in path ? path.closedStateUses : [];
  return hl('section.move-review__proof-path.move-review__typed-section', { key: path.pathOccurrenceId }, [
    hl('h4', `${copy.proofPath} ${index + 1}`),
    'realizationActor' in path
      ? hl('div.move-review__path-realization', [
          renderFact(copy.structureLabels.move, path.realizationMove, 'move'),
          renderWitness(path.realizationActor, copy),
        ])
      : undefined,
    hl('section.move-review__premises', [
      hl('h5', copy.premises),
      hl(
        'ol.move-review__typed-list',
        path.premises.map((premise, premiseIndex) => hl('li', [renderPremise(premise, premiseIndex, copy)])),
      ),
    ]),
    absences.length ? renderClosures(copy.closedAbsence, absences, 'closed-absence', copy) : undefined,
    states.length ? renderClosures(copy.closedState, states, 'closed-state', copy) : undefined,
  ]);
}

function renderOccurrenceBranch(
  explanation: MoveReviewOccurrenceExplanation,
  branch: MoveReviewAnyBranch,
  index: number,
  props: MoveReviewPanelProps,
): VNode {
  const proof = moveReviewOccurrenceBranchProof(explanation, index);
  const rootMove = 'rootMove' in branch ? branch.rootMove : branch.line.rootMove;
  return hl('section.move-review__branch', { key: branch.branchId }, [
    hl('header.move-review__branch-header', [
      hl('h4', branchLabel(branch.rootProvenance, props.copy)),
      hl('code.move-review__typed-value.is-move', rootMove),
    ]),
    proof && renderProof(proof, undefined, props, branch),
  ]);
}

function branchLabel(provenance: MoveReviewAnyBranch['rootProvenance'], copy: MoveReviewCopy): string {
  return provenance === 'observed_game_root'
    ? `${copy.actualMove} · ${copy.analysisContinuation}`
    : copy.analyzedAlternative;
}

type MoveReviewParticipant = MoveReviewMovementWitness | MoveReviewColoredPieceWitness | Key;
type MoveReviewPremise = MoveReviewAnyProofPath['premises'][number];

function renderOccurrenceParticipants(
  explanation: MoveReviewOccurrenceExplanation,
  copy: MoveReviewCopy,
): VNode {
  const participants =
    explanation.proofKind === 'passed_pawn_progress_realized_after_only_legal_reply'
      ? { rootActor: explanation.proof.rootActor, realizingActor: explanation.proof.realizingActor }
      : explanation.proof.participants;
  return hl('section.move-review__participants.move-review__typed-section', [
    hl('h4', copy.structureLabels.participants),
    hl(
      'div.move-review__participant-list',
      (Object.entries(participants) as [string, MoveReviewParticipant][]).map(([role, participant]) =>
        hl('section.move-review__participant', [
          hl('h5', structureRoleLabel(role, copy, copy.structureLabels.participants)),
          typeof participant === 'string'
            ? renderFact(copy.structureLabels.square, participant, 'square')
            : renderWitness(participant, copy),
        ]),
      ),
    ),
  ]);
}

function renderWitness(value: Exclude<MoveReviewParticipant, Key>, copy: MoveReviewCopy): VNode {
  const labels = copy.structureLabels;
  return 'from' in value
    ? renderFacts([
        renderFact(labels.side, copy.colorLabels[value.side]),
        renderFact(
          labels.piece,
          value.pieceBefore === value.pieceAfter
            ? labels.pieceLabels[value.pieceBefore]
            : `${labels.pieceLabels[value.pieceBefore]} → ${labels.pieceLabels[value.pieceAfter]}`,
          'piece',
        ),
        renderFact(labels.from, value.from, 'square'),
        renderFact(labels.to, value.to, 'square'),
      ])
    : renderFacts([
        renderFact(labels.side, copy.colorLabels[value.side]),
        renderFact(labels.piece, labels.pieceLabels[value.piece], 'piece'),
        renderFact(labels.square, value.square, 'square'),
      ]);
}

function renderPremise(premise: MoveReviewPremise, index: number, copy: MoveReviewCopy): VNode {
  const labels = copy.structureLabels;
  if ('lowerKind' in premise) {
    const dependency = premise.role === 'dependency' ? premise.dependencyProof : undefined;
    return hl('section.move-review__premise', [
      hl('h6', structureRoleLabel(premise.role, copy, `${labels.premise} ${index + 1}`)),
      renderFacts([
        renderFact(labels.afterStep, `${premise.fromStepIndex + 1} → ${premise.toStepIndex + 1}`),
      ]),
      dependency && renderPassedPawnDependency(dependency, copy),
    ]);
  }
  if ('transitionKind' in premise)
    return hl('section.move-review__premise', [
      hl('h6', structureRoleLabel(premise.role, copy, `${labels.premise} ${index + 1}`)),
      renderFacts([
        renderFact(labels.contract, labels.contractLabels[premise.contract] ?? labels.contract),
        renderFact(labels.result, structureRoleLabel(premise.transitionKind, copy, premise.transitionKind)),
        renderFact(labels.move, premise.overallMoveUci, 'move'),
        renderFact(labels.afterStep, premise.stepIndex + 1),
      ]),
      hl('div.move-review__continuity-before', [hl('h6', labels.from), renderWitness(premise.before, copy)]),
      hl('div.move-review__continuity-after', [hl('h6', labels.to), renderWitness(premise.after, copy)]),
      premise.selectedTransition
        ? hl('div.move-review__continuity-transition', [
            hl('h6', labels.move),
            renderWitness(premise.selectedTransition, copy),
          ])
        : undefined,
    ]);
  const contract = labels.contractLabels[premise.contract] ?? labels.contract;
  if ('movement' in premise)
    return hl('section.move-review__premise', [
      hl('h6', structureRoleLabel(premise.role, copy, `${labels.premise} ${index + 1}`)),
      renderFacts([
        renderFact(labels.contract, contract),
        renderFact(labels.move, premise.moveUci, 'move'),
        renderFact(labels.afterStep, premise.stepIndex + 1),
      ]),
      renderWitness(premise.movement, copy),
      premise.capture
        ? hl('div.move-review__premise-capture', [
            hl('h6', labels.capture),
            renderWitness(premise.capture, copy),
          ])
        : undefined,
    ]);
  return hl('section.move-review__premise', [
    hl('h6', structureRoleLabel(premise.role, copy, `${labels.premise} ${index + 1}`)),
    renderFacts([renderFact(labels.contract, contract), renderFact(labels.afterStep, premise.stepIndex + 1)]),
  ]);
}

function renderPassedPawnDependency(
  dependency: Extract<MoveReviewPremise, { role: 'dependency' }>['dependencyProof'],
  copy: MoveReviewCopy,
): VNode {
  const labels = copy.structureLabels;
  return hl('div.move-review__dependency', [
    dependency.squares.length
      ? hl(
          'ol.move-review__typed-list',
          dependency.squares.map(item => hl('li', [renderFact(labels.square, item.square, 'square')])),
        )
      : undefined,
    dependency.pieces.length
      ? hl(
          'ol.move-review__typed-list',
          dependency.pieces.map(item =>
            hl('li', [
              renderFacts([
                renderFact(labels.side, copy.colorLabels[item.side]),
                renderFact(labels.piece, labels.pieceLabels[item.piece], 'piece'),
              ]),
            ]),
          ),
        )
      : undefined,
    ...dependency.positionStateIssuers.map(item => renderPositionState(item.state, copy)),
  ]);
}

function renderPositionState(state: MoveReviewPassedPawnPositionState, copy: MoveReviewCopy): VNode {
  const labels = copy.structureLabels;
  if (state.kind === 'pawn_topology')
    return renderFacts([
      renderFact(labels.side, copy.colorLabels[state.side]),
      renderFact(labels.piece, labels.pieceLabels.pawn, 'piece'),
      renderFact(labels.square, state.square, 'square'),
      renderFact(labels.result, state.passed ? labels.yes : labels.no),
    ]);
  const witness: MoveReviewColoredPieceWitness = {
    side: state.side,
    piece: state.piece,
    square: state.square,
  };
  return hl('div.move-review__position-state', [
    renderWitness(witness, copy),
    state.kind === 'slider_reach' && state.segment.length
      ? hl(
          'ol.move-review__typed-list',
          state.segment.map(item =>
            hl('li', [
              renderFacts([
                renderFact(labels.square, item.square, 'square'),
                ...(item.occupantPiece
                  ? [renderFact(labels.piece, labels.pieceLabels[item.occupantPiece], 'piece')]
                  : []),
              ]),
            ]),
          ),
        )
      : undefined,
  ]);
}

function renderClosures(
  label: string,
  closures: MoveReviewClosureUse[],
  className: 'closed-absence' | 'closed-state',
  copy: MoveReviewCopy,
): VNode {
  return hl(`section.move-review__${className}`, [
    hl('h5', label),
    hl(
      'ol.move-review__typed-list',
      closures.map(closure =>
        hl('li', [
          hl('strong', structureRoleLabel(closure.role, copy, label)),
          renderFacts([
            renderFact(copy.structureLabels.afterStep, closure.afterStepIndex + 1),
            renderFact(copy.structureLabels.positionPly, closure.position.ply),
          ]),
        ]),
      ),
    ),
  ]);
}

function renderLaterConsumer(explanation: MoveReviewOccurrenceExplanation, copy: MoveReviewCopy): VNode {
  const labels = copy.structureLabels;
  switch (explanation.proofKind) {
    case 'unique_check_reply_defender_displacement_before_capture':
      return renderConsumer(explanation.proof.realizingMove, explanation.proof.participants.realizer, copy);
    case 'sole_recapturer_removal_before_target_capture':
      return renderConsumer(
        explanation.proof.postRemovalTargetCaptureMove,
        explanation.proof.participants.postRemovalTargetCapture,
        copy,
      );
    case 'vacated_gate_enables_unrecapturable_slider_capture':
      return renderConsumer(explanation.proof.exploitMove, explanation.proof.participants.exploit, copy);
    case 'square_release_route':
      return hl('section.move-review__later-consumer.move-review__typed-section', [
        hl('h4', labels.laterConsumer),
        hl(
          'ol.move-review__typed-list',
          explanation.proof.route.map(step =>
            hl('li', [renderFact(labels.move, step.moveUci, 'move'), renderWitness(step, copy)]),
          ),
        ),
        explanation.proof.terminalReplyMove
          ? renderFact(labels.move, explanation.proof.terminalReplyMove, 'move')
          : undefined,
      ]);
    case 'capture_exclusion_move_order':
      return hl('section.move-review__later-consumer.move-review__typed-section', [
        hl('h4', labels.laterConsumer),
        renderFact(labels.afterStep, explanation.proof.laterDeferredStepIndex + 1),
        renderWitness(explanation.proof.participants.deferredMove, copy),
      ]);
    case 'relocation_enables_recapture':
      return hl('section.move-review__later-consumer.move-review__typed-section', [
        hl('h4', labels.laterConsumer),
        hl('h5', structureRoleLabel('relocated_target_capture', copy, labels.capture)),
        renderFact(labels.move, explanation.proof.targetCapture.moveUci, 'move'),
        renderWitness(explanation.proof.targetCapture.movement, copy),
        hl('h5', structureRoleLabel('relocated_responder_recapture', copy, labels.reply)),
        renderFact(labels.move, explanation.proof.relocatedResponderRecapture.moveUci, 'move'),
        renderWitness(explanation.proof.relocatedResponderRecapture.movement, copy),
      ]);
    case 'passed_pawn_progress_realized_after_only_legal_reply':
      return hl('section.move-review__later-consumer.move-review__typed-section', [
        hl('h4', labels.laterConsumer),
        renderFact(labels.reply, explanation.proof.closedLegalReplyInventory.legalReplyMove, 'move'),
        renderFact(labels.move, explanation.proof.realizingMove, 'move'),
        renderWitness(explanation.proof.realizingActor, copy),
      ]);
  }
}

function renderConsumer(move: Uci, participant: MoveReviewMovementWitness, copy: MoveReviewCopy): VNode {
  return hl('section.move-review__later-consumer.move-review__typed-section', [
    hl('h4', copy.structureLabels.laterConsumer),
    renderFact(copy.structureLabels.move, move, 'move'),
    renderWitness(participant, copy),
  ]);
}

function renderOccurrenceProvenance(
  explanation: MoveReviewOccurrenceExplanation,
  branches: MoveReviewAnyBranch[],
  paths: MoveReviewAnyProofPath[],
  copy: MoveReviewCopy,
): VNode {
  const labels = copy.structureLabels;
  return hl('details.move-review__provenance', [
    hl('summary', labels.provenanceDetails),
    renderFacts([
      renderFact(labels.causeEvidenceId, explanation.causeEvidenceId),
      renderFact(labels.proofOccurrenceId, explanation.proof.occurrenceId),
      renderFact(labels.subjectOccurrenceId, explanation.subjectOccurrence.occurrenceId),
      renderFact(labels.semanticId, explanation.proof.semanticId),
      renderFact(labels.sourceEvidenceId, explanation.proof.sourceEvidenceId),
      renderFact(labels.dependencyFingerprint, explanation.proof.dependencyFingerprint),
    ]),
    ...branches.map((branch, index) =>
      hl('section', [
        hl('h5', `${branchLabel(branch.rootProvenance, copy)} ${index + 1}`),
        renderFacts([
          renderFact(labels.branchId, branch.branchId),
          renderFact(labels.lineId, 'lineId' in branch ? branch.lineId : branch.line.lineId),
        ]),
      ]),
    ),
    ...paths.flatMap((path, index) => [
      renderFact(`${labels.pathOccurrenceId} ${index + 1}`, path.pathOccurrenceId),
      ...('closureUseIds' in path
        ? path.closureUseIds.map((id, closureIndex) =>
            renderFact(`${labels.closureUseId} ${closureIndex + 1}`, id),
          )
        : [...path.closedAbsenceUses, ...path.closedStateUses].map((closure, closureIndex) =>
            renderFact(`${labels.closureUseId} ${closureIndex + 1}`, closure.useId),
          )),
    ]),
  ]);
}

function structureRoleLabel(role: string, copy: MoveReviewCopy, fallback: string): string {
  const exact = copy.structureLabels.roleLabels[role];
  if (exact) return exact;
  if (role.startsWith('route_move_')) return copy.structureLabels.route;
  if (role.startsWith('route_piece_') || role.startsWith('route_persistence_'))
    return copy.structureLabels.route;
  return fallback;
}

function renderFacts(items: VNode[]): VNode {
  return hl('dl.move-review__typed-fields', items);
}

function renderFact(label: string, value: string | number, tone?: 'move' | 'square' | 'piece'): VNode {
  return hl('div.move-review__typed-field', [
    hl('dt', label),
    hl('dd', [hl(`code.move-review__typed-value${tone ? `.is-${tone}` : ''}`, String(value))]),
  ]);
}

function renderProof(
  proof: MoveReviewProof,
  contentId: string | undefined,
  props: MoveReviewPanelProps,
  branch?: MoveReviewAnyBranch,
): VNode {
  const frame = activeFrame(proof, props.view);
  const pinned = props.view.pinnedFrame;
  const board = proofBoard(proof, frame.ply);
  const added = props.addedProofId === proof.id;
  return hl(
    'div.move-review__proof',
    {
      attrs: { ...(contentId ? { id: contentId } : {}), 'aria-keyshortcuts': 'Escape' },
      on: {
        keydown: event => {
          if (event.key !== 'Escape') return;
          event.preventDefault();
          event.stopPropagation();
          props.actions.clearPreview();
          props.actions.clearPin();
        },
      },
    },
    [
      hl('div.move-review__proof-copy', [
        hl(
          'div.move-review__proof-moves',
          { on: { mouseleave: props.actions.clearPreview } },
          renderProofMoves(proof, frame, pinned, props, branch),
        ),
        props.canWrite &&
          !added &&
          hl('div.move-review__proof-actions', [
            hl(
              'button.button.button-thin.move-review__add',
              {
                attrs: { type: 'button' },
                on: { click: () => props.actions.addProof(proof.id) },
              },
              props.copy.addToStudy,
            ),
          ]),
        added &&
          hl('div.move-review__added', { attrs: { role: 'status', 'aria-live': 'polite' } }, [
            props.copy.addedToStudy,
            hl('div.move-review__added-actions', [
              hl(
                'button.button.button-thin.move-review__view-added',
                {
                  attrs: { type: 'button' },
                  on: { click: () => props.actions.viewAddedLine(proof.id) },
                },
                props.copy.viewAddedLine,
              ),
            ]),
          ]),
      ]),
      hl('figure.move-review__proof-visual', [
        hl('figcaption', `${props.copy.proofBoard}: ${proofFrameLabel(proof, frame.ply, props.copy)}`),
        renderBoardPreview(board, props.orientation, 'div.move-review__proof-board'),
      ]),
    ],
  );
}

function renderProofMoves(
  proof: MoveReviewProof,
  active: MoveReviewFrameSelection,
  pinned: MoveReviewFrameSelection | undefined,
  props: MoveReviewPanelProps,
  branch?: MoveReviewAnyBranch,
): VNode[] {
  return proof.moves.map((move, index) => {
    const ply = index + 1;
    const frame = { proofId: proof.id, ply };
    const isActive = active.ply === ply;
    const isPinned = pinned?.proofId === proof.id && pinned.ply === ply;
    const step = `${props.copy.proofStep} ${ply}`;
    const stage = branch && proofMoveStage(branch, ply, props.copy);
    return hl('span.move-review__proof-move', [
      stage && hl('span.move-review__proof-stage', stage),
      hl('span.move-review__proof-index', String(ply)),
      hl(
        'button.move-review__proof-san',
        {
          key: `${proof.id}:${ply}`,
          class: { 'is-previewed': isActive && !isPinned },
          attrs: {
            type: 'button',
            'aria-pressed': isPinned ? 'true' : 'false',
            'aria-label': `${step}: ${move.label}`,
          },
          on: {
            mouseenter: () => props.actions.previewFrame(frame),
            focus: () => props.actions.previewFrame(frame),
            blur: props.actions.clearPreview,
            click: () => props.actions.pinFrame(frame),
          },
        },
        move.label,
      ),
    ]);
  });
}

function proofMoveStage(branch: MoveReviewAnyBranch, ply: number, copy: MoveReviewCopy): string | undefined {
  const provenance = branch.steps?.[ply - 1]?.provenance;
  if (provenance === 'observed_game_move') return copy.actualMove;
  if (provenance === 'certified_analysis_move')
    return branch.rootProvenance === 'counterfactual_analyzed_root'
      ? copy.analyzedAlternative
      : copy.analysisContinuation;
  return undefined;
}

function activeFrame(proof: MoveReviewProof, view: MoveReviewViewState): MoveReviewFrameSelection {
  const valid = (frame?: MoveReviewFrameSelection): frame is MoveReviewFrameSelection =>
    frame?.proofId === proof.id && frame.ply >= 1 && frame.ply <= proof.moves.length;
  if (valid(view.hoveredFrame)) return view.hoveredFrame;
  if (valid(view.pinnedFrame)) return view.pinnedFrame;
  return { proofId: proof.id, ply: 1 };
}

function proofBoard(proof: MoveReviewProof, ply: number): { fen: FEN; uci?: Uci; shapes: DrawShape[] } {
  const move = proof.moves[ply - 1]!;
  return {
    fen: move.fenAfter,
    uci: move.uci,
    shapes: proof.annotations
      .filter(annotation => annotation.atPly === ply)
      .map(annotation => annotationDrawShape(annotation.shape)),
  };
}

function annotationDrawShape(shape: MoveReviewAnnotationShape): DrawShape {
  return shape.kind === 'arrow'
    ? { orig: shape.orig, dest: shape.dest, brush: shape.brush }
    : { orig: shape.key, brush: shape.brush };
}

function proofFrameLabel(proof: MoveReviewProof, ply: number, copy: MoveReviewCopy): string {
  return `${copy.proofStep} ${ply}: ${proof.moves[ply - 1]!.label}`;
}
