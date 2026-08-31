import type { DrawShape } from '@lichess-org/chessground/draw';
import { renderBoardPreview } from 'lib/view/boardPreview';
import { hl, type LooseVNodes, type VNode } from 'lib/view/snabbdom';
import {
  formatMoveReviewPercent,
  formatMoveReviewPercentagePointChange,
  moveReviewReasonRole,
  moveReviewReasonText,
  moveReviewVerdictCodeLabel,
  selectedMoveReviewCandidate,
  type MoveReviewAnnotationShape,
  type MoveReviewCandidate,
  type MoveReviewCandidateRole,
  type MoveReviewCopy,
  type MoveReviewCore,
  type MoveReviewDrawClaim,
  type MoveReviewEvidence,
  type MoveReviewFrameSelection,
  type MoveReviewJobState,
  type MoveReviewLocale,
  type MoveReviewProof,
  type MoveReviewReason,
  type MoveReviewReasonRole,
  type MoveReviewVerdictSymbol,
  type MoveReviewViewState,
} from '../moveReview';

type MoveReviewVerdictTone = 'neutral' | 'inaccuracy' | 'mistake' | 'blunder';

interface MoveReviewPanelActions {
  selectCandidate(uci: Uci): void;
  toggleEvidence(): void;
  toggleReason(reasonId: string): void;
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
  return renderState(
    'completed',
    props.copy.drawClaimAvailable,
    drawClaimSummary(action.claims, props.copy),
  );
}

function drawClaimSummary(
  claims: MoveReviewDrawClaim[],
  copy: MoveReviewCopy,
): string {
  return claims
    .map(
      claim => `${copy.drawRuleLabels[claim.rule]} · ${copy.drawAvailabilityLabels[claim.availability]}`,
    )
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

function renderReviewBody(
  props: MoveReviewPanelProps,
  evidence: MoveReviewEvidence,
): VNode {
  const played = evidence.candidates.find(candidate => candidate.roles.includes('played'))!;
  const best = evidence.candidates.find(candidate => candidate.roles.includes('best'))!;
  const review = played.review;
  const core = review.kind === 'move-verdict' ? review.core : undefined;
  const primary =
    review.kind === 'move-verdict' && review.core.reasonRefs.primary
      ? review.reasons.find(reason => reason.id === review.core.reasonRefs.primary)
      : undefined;
  const verdictLabel = core
    ? core.kind === 'best-choice'
      ? props.copy.candidateSetLabels[core.bestChoice.candidateSet]
      : moveReviewVerdictCodeLabel(core.verdictCode, props.copy)
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
          primary
            ? hl('div.move-review__primary-reason', [
                hl('span', moveReviewReasonText(primary, played, props.locale)),
              ])
            : review.kind === 'move-verdict'
              ? hl('p', review.reasons.length ? props.copy.noPrimaryReason : props.copy.noVerifiedReason)
              : review.kind === 'abstained'
                ? hl('p', props.copy.candidateUnavailable)
                : undefined,
        ]),
      ],
    ),
    core?.winChance && renderMetrics(core.winChance, props),
    core &&
      (core.referenceTerminal || core.reviewedTerminal) &&
      renderTerminalOutcomes(core, props),
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

function renderMetrics(winChance: NonNullable<MoveReviewCore['winChance']>, props: MoveReviewPanelProps): VNode {
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
    core.referenceTerminal && hl('div', [
      hl('dt', core.kind === 'best-choice' ? props.copy.runnerUp : props.copy.best),
      hl('dd', label(core.referenceTerminal)),
    ]),
    core.reviewedTerminal && hl('div', [
      hl('dt', core.kind === 'best-choice' ? props.copy.best : props.copy.played),
      hl('dd', label(core.reviewedTerminal)),
    ]),
  ]);
}

function renderEvidenceDisclosure(
  props: MoveReviewPanelProps,
  evidence: MoveReviewEvidence,
): VNode {
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

function renderEvidence(
  props: MoveReviewPanelProps,
  evidence: MoveReviewEvidence,
): VNode[] {
  const selected = selectedMoveReviewCandidate(evidence, props.view.selectedCandidateUci)!;
  return [
    renderCandidates(evidence.candidates, selected, props),
    renderCandidateReview(selected, props),
  ];
}

function renderCandidateReview(candidate: MoveReviewCandidate, props: MoveReviewPanelProps): VNode {
  const review = candidate.review;
  if (review.kind === 'abstained') return renderCandidateMessage(candidate, props.copy.candidateUnavailable);
  if (review.kind === 'forced-single-move') return renderForcedSingleMove(candidate, review, props);
  if (review.kind === 'single-candidate-insight') return renderInsight(candidate, review.proof, props);
  const reasonById = new Map(review.reasons.map(reason => [reason.id, reason]));
  const orderedRefs = [
    ...(review.core.reasonRefs.primary ? [review.core.reasonRefs.primary] : []),
    ...review.core.reasonRefs.routes,
    ...review.core.reasonRefs.support,
  ];
  const reasons = orderedRefs
    .map(ref => reasonById.get(ref))
    .filter((reason): reason is MoveReviewReason => reason?.messageSlots.candidateUci === candidate.uci);
  return renderReasons(review.core, candidate, reasons, props);
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
      hl('p.move-review__no-reason', props.copy.forcedSingleMove),
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
    [hl('p.move-review__no-reason', message)],
  );
}

function renderInsight(
  candidate: MoveReviewCandidate,
  proof: MoveReviewProof,
  props: MoveReviewPanelProps,
): VNode {
  const expanded = props.view.expandedReasonId === proof.id;
  const contentId = `move-review-reason-${proof.id}`;
  return hl(
    'section#move-review-selected-candidate.move-review__section',
    {
      attrs: {
        role: 'tabpanel',
        'aria-labelledby': `move-review-candidate-${candidate.uci}`,
      },
    },
    [
      hl('article.move-review__reason', [
        hl(
          'button.move-review__reason-button',
          {
            attrs: {
              type: 'button',
              'aria-expanded': expanded ? 'true' : 'false',
              ...(expanded ? { 'aria-controls': contentId } : {}),
            },
            on: { click: () => props.actions.toggleReason(proof.id) },
          },
          [
            hl('span.move-review__reason-copy', [
              hl('strong', props.copy.lineInsight),
              hl('span', candidate.label),
            ]),
            hl('span.move-review__reason-chevron', { attrs: { 'aria-hidden': 'true' } }, '⌄'),
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

function renderReasons(
  core: MoveReviewCore,
  candidate: MoveReviewCandidate,
  reasons: MoveReviewReason[],
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
      reasons.length
        ? hl(
            'div.move-review__reasons',
            reasons.map(reason =>
              renderReason(reason, moveReviewReasonRole(core, reason.id)!, candidate, props),
            ),
          )
        : hl('p.move-review__no-reason', props.copy.noVerifiedReason),
    ],
  );
}

function renderReason(
  reason: MoveReviewReason,
  role: MoveReviewReasonRole,
  candidate: MoveReviewCandidate,
  props: MoveReviewPanelProps,
): VNode {
  const expanded = props.view.expandedReasonId === reason.id;
  const contentId = `move-review-reason-${reason.id}`;
  return hl('article.move-review__reason', { key: reason.id }, [
    hl(
      'button.move-review__reason-button',
      {
        attrs: {
          type: 'button',
          'aria-expanded': expanded ? 'true' : 'false',
          ...(expanded ? { 'aria-controls': contentId } : {}),
        },
        on: { click: () => props.actions.toggleReason(reason.id) },
      },
      [
        hl('span.move-review__reason-copy', [
          hl('strong', reasonRoleLabel(role, props.copy)),
          hl('span', moveReviewReasonText(reason, candidate, props.locale)),
        ]),
        hl('span.move-review__reason-chevron', { attrs: { 'aria-hidden': 'true' } }, '⌄'),
      ],
    ),
    expanded && renderProof(reason.proof, contentId, props),
  ]);
}

function reasonRoleLabel(role: MoveReviewReasonRole, copy: MoveReviewCopy): string {
  switch (role) {
    case 'primary':
      return copy.primaryReason;
    case 'support':
      return copy.supportingReason;
    case 'proof-route':
      return copy.proofRouteReason;
  }
}

function renderProof(proof: MoveReviewProof, contentId: string, props: MoveReviewPanelProps): VNode {
  const frame = activeFrame(proof, props.view);
  const pinned = props.view.pinnedFrame;
  const board = proofBoard(proof, frame.ply);
  const added = props.addedProofId === proof.id;
  return hl(
    'div.move-review__proof',
    {
      attrs: { id: contentId, 'aria-keyshortcuts': 'Escape' },
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
          renderProofMoves(proof, frame, pinned, props),
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
): LooseVNodes {
  return proof.moves.map((move, index) => {
    const ply = index + 1;
    const frame = { proofId: proof.id, ply };
    const isActive = active.ply === ply;
    const isPinned = pinned?.proofId === proof.id && pinned.ply === ply;
    const step = `${props.copy.proofStep} ${ply}`;
    return [
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
    ];
  });
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
