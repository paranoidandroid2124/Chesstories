import assert from 'node:assert/strict';
import { test } from 'node:test';
import type { VNode } from 'snabbdom';
import {
  moveReviewCopy,
  type MoveReviewJobState,
  type MoveReviewLocale,
  type MoveReviewProof,
  type MoveReviewReason,
  type MoveReviewVerdictSymbol,
  type MoveReviewViewState,
} from '../src/moveReview';
import {
  renderMoveReview,
  renderMoveReviewNotationBadge,
  type MoveReviewPanelProps,
} from '../src/view/moveReview';

type CompletedJob = Extract<MoveReviewJobState, { kind: 'completed' }>;
type MoveReviewPanelActions = MoveReviewPanelProps['actions'];

const noopActions: MoveReviewPanelActions = {
  selectCandidate: () => {},
  toggleEvidence: () => {},
  toggleReason: () => {},
  previewFrame: () => {},
  clearPreview: () => {},
  pinFrame: () => {},
  clearPin: () => {},
  retry: () => {},
  addProof: () => {},
  viewAddedLine: () => {},
};

const beforeFen = '8/8/8/8/8/8/4K3/7k w - - 0 1' as FEN;
const firstFen = '8/8/8/8/8/4K3/8/7k b - - 1 1' as FEN;
const secondFen = '8/8/8/8/8/4K3/6k1/8 w - - 2 2' as FEN;
const playedUci = 'e2e3' as Uci;

function makeProof(id: string): MoveReviewProof {
  return {
    id,
    startFen: beforeFen,
    moves: [
      { uci: playedUci, label: 'e2e3', fenAfter: firstFen },
      { uci: 'h1g2' as Uci, label: 'h1g2', fenAfter: secondFen },
    ],
    annotations: [],
  };
}

function makeReason(id: string): MoveReviewReason {
  return {
    id,
    messageKey: 'move_review.reason.for_candidate',
    messageSlots: { candidateUci: playedUci },
    proof: makeProof(id),
  };
}

const primaryReason = makeReason('reason.primary');
const supportingReason = makeReason('reason.support');

const completedJob: CompletedJob = {
  kind: 'completed',
  snapshot: {
    kind: 'completed',
    requestId: 'view-test',
    jobId: 'a'.repeat(32),
    engineProfile: 'sf18-smallnet-t2-h16-v1',
    judgmentRevision: 'chesstory.position-commentary.response.v6',
    annotationPolicyRevision: 'chesstory.verdict-threshold-policy.v2',
    generation: 0,
    subject: {
      variant: 'standard',
      initialFen: beforeFen,
      movePrefixUci: [],
      before: { path: '' as Tree.Path, fen: beforeFen },
      played: { uci: playedUci, san: 'Ke3' as San },
      after: { path: 'aa' as Tree.Path, fen: firstFen },
    },
    evidence: {
      candidates: [
        {
          uci: 'e2f2' as Uci,
          label: 'e2f2',
          roles: ['best'],
          winPercent: 53.25,
          review: { kind: 'single-candidate-insight', proof: makeProof('line.best') },
        },
        {
          uci: playedUci,
          label: 'Ke3',
          roles: ['played'],
          winPercent: 49.75,
          review: {
            kind: 'move-verdict',
            core: {
              verdictRef: 'verdict.view-test',
              verdictCode: 'inaccuracy',
              verdictSymbol: '?!',
              playedUci,
              bestUci: 'e2f2' as Uci,
              winChance: {
                referencePercent: 53.25,
                playedPercent: 49.75,
                changePercentagePoints: -3.5,
              },
              reasonRefs: { primary: primaryReason.id, support: [supportingReason.id] },
            },
            reasons: [supportingReason, primaryReason],
          },
        },
      ],
    },
  },
};

function completedWith(symbol: MoveReviewVerdictSymbol): CompletedJob {
  const verdictCode =
    symbol === 'none'
      ? ('matches_reference' as const)
      : symbol === '?!'
        ? ('inaccuracy' as const)
        : symbol === '?'
          ? ('mistake' as const)
          : ('blunder' as const);
  const candidates = completedJob.snapshot.evidence.candidates.map(candidate =>
    candidate.review.kind === 'move-verdict'
      ? {
          ...candidate,
          review: {
            ...candidate.review,
            core: { ...candidate.review.core, verdictCode, verdictSymbol: symbol },
          },
        }
      : candidate,
  );
  return {
    ...completedJob,
    snapshot: {
      ...completedJob.snapshot,
      evidence: { candidates },
    },
  };
}

function props(
  options: {
    job?: MoveReviewJobState;
    locale?: MoveReviewLocale;
    canWrite?: boolean;
    addedProofId?: string;
    view?: Partial<MoveReviewViewState>;
    actions?: Partial<MoveReviewPanelActions>;
  } = {},
): MoveReviewPanelProps {
  const locale = options.locale ?? 'en-US';
  return {
    job: options.job ?? completedJob,
    view: {
      selectedCandidateUci: playedUci,
      evidenceExpanded: false,
      ...options.view,
    },
    copy: moveReviewCopy(locale),
    locale,
    orientation: 'white',
    canWrite: options.canWrite ?? false,
    liveEnginePaused: false,
    addedProofId: options.addedProofId,
    actions: { ...noopActions, ...options.actions },
  };
}

function descendants(root: VNode): VNode[] {
  const result: VNode[] = [];
  const visit = (node: VNode) => {
    result.push(node);
    for (const child of node.children || [])
      if (typeof child === 'object' && child && 'sel' in child) visit(child as VNode);
  };
  visit(root);
  return result;
}

function renderedText(root: VNode): string {
  return descendants(root)
    .map(node => node.text)
    .filter((text): text is string => typeof text === 'string')
    .join(' ');
}

function findNode(root: VNode, selector: string): VNode {
  const found = descendants(root).find(node => node.sel === selector);
  assert.ok(found, `Expected ${selector}`);
  return found;
}

function findNodes(root: VNode, selector: string): VNode[] {
  return descendants(root).filter(node => node.sel === selector);
}

interface BoardConfig {
  fen: FEN;
  lastMove?: Key[];
  orientation: Color;
}

function renderedBoardConfig(root: VNode): BoardConfig {
  const board = findNode(root, 'div.cg-wrap.is2d');
  let captured: BoardConfig | undefined;
  const fake = {
    elm: { _cg: { set: (config: BoardConfig) => (captured = config) } },
  } as unknown as VNode;
  const update = board.data?.hook?.update;
  assert.ok(update);
  update(fake, fake);
  assert.ok(captured);
  return captured;
}

test('renders only the four v6 verdict symbols and their honest tones', () => {
  const verdicts: Array<[MoveReviewVerdictSymbol, string | undefined, string, string]> = [
    ['none', undefined, 'Matches the reference', 'No annotation'],
    ['?!', 'inaccuracy', 'Inaccuracy', 'Dubious'],
    ['?', 'mistake', 'Mistake', 'Mistake'],
    ['??', 'blunder', 'Blunder', 'Blunder'],
  ];
  for (const [symbol, tone, summaryLabel, notationLabel] of verdicts) {
    const panel = renderMoveReview(props({ job: completedWith(symbol) }));
    const summary = findNode(panel, 'section.move-review__summary');
    assert.equal(summary.data?.attrs?.['aria-label'], `${summaryLabel}: Ke3`);
    assert.match(renderedText(summary), new RegExp(summaryLabel));
    const notation = renderMoveReviewNotationBadge(symbol, notationLabel);
    if (!tone) assert.equal(notation, undefined);
    else {
      assert.equal(notation?.sel, `span.move-review__notation-badge.move-review__notation-badge--${tone}`);
      assert.equal(renderedText(notation!), symbol);
    }
  }
});

test('renders v6 candidate labels and reference-to-reviewed metrics without inferred tags', () => {
  const panel = renderMoveReview(props({ view: { evidenceExpanded: true } }));
  const candidates = findNodes(panel, 'button.move-review__candidate');
  assert.equal(candidates.length, 2);
  assert.equal(renderedText(candidates[0]!), 'Best e2f2 53.3%');
  assert.equal(renderedText(candidates[1]!), 'Played Ke3 49.8%');
  assert.match(renderedText(findNode(panel, 'dl.move-review__metrics')), /53\.3% → 49\.8%/);
  assert.match(renderedText(panel), /-3\.5%p/);
  assert.equal(findNodes(panel, 'span.move-review__display-tag').length, 0);
});

test('renders draw claims alongside selected candidate reviews', () => {
  const job: CompletedJob = {
    ...completedJob,
    snapshot: {
      ...completedJob.snapshot,
      evidence: {
        ...completedJob.snapshot.evidence,
        drawClaims: [{ rule: 'threefold_repetition', availability: 'available_now' }],
      },
    },
  };
  const copy = moveReviewCopy('en-US');
  const text = renderedText(renderMoveReview(props({ job })));
  assert.ok(text.includes(copy.drawClaimAvailable));
  assert.ok(text.includes(copy.drawRuleLabels.threefold_repetition));
});

test('renders a forced single move terminal as an exact line outcome', () => {
  const job: CompletedJob = {
    ...completedJob,
    snapshot: {
      ...completedJob.snapshot,
      evidence: {
        candidates: [
          {
            uci: playedUci,
            label: 'Ke3',
            roles: ['best', 'played'],
            review: {
              kind: 'forced-single-move',
              lineUcis: [playedUci, 'h1g2' as Uci],
              terminal: { kind: 'checkmate', winner: 'black' },
            },
          },
        ],
      },
    },
  };
  const panel = renderMoveReview(props({ job, view: { evidenceExpanded: true } }));
  assert.match(renderedText(panel), /Forced single move/);
  assert.match(renderedText(panel), /e2e3 h1g2/);
  assert.match(renderedText(panel), /Terminal position: Checkmate · Black/);
  assert.equal(findNodes(panel, 'span.move-review__verdict-badge').length, 0);
});

test('uses selected reason order and UCI proof labels without browser SAN synthesis', () => {
  const panel = renderMoveReview(
    props({
      canWrite: true,
      view: { evidenceExpanded: true, expandedReasonId: primaryReason.id },
    }),
  );
  const reasons = findNodes(panel, 'button.move-review__reason-button').map(renderedText);
  assert.equal(reasons.length, 2);
  assert.match(reasons[0]!, /^Primary reason/);
  assert.match(reasons[1]!, /^Supporting reason/);
  const moves = findNodes(panel, 'button.move-review__proof-san');
  assert.deepEqual(moves.map(renderedText), ['e2e3', 'h1g2']);
  assert.deepEqual(
    moves.map(node => node.data?.attrs?.['aria-label']),
    ['Step 1: e2e3', 'Step 2: h1g2'],
  );
  assert.equal(renderedBoardConfig(panel).fen, firstFen);
});

test('preserves the best candidate insight independently from the played verdict', () => {
  const panel = renderMoveReview(
    props({
      view: {
        selectedCandidateUci: 'e2f2',
        evidenceExpanded: true,
        expandedReasonId: 'line.best',
      },
    }),
  );
  assert.match(renderedText(findNode(panel, 'button.move-review__reason-button')), /Verified candidate line/);
  assert.deepEqual(findNodes(panel, 'button.move-review__proof-san').map(renderedText), ['e2e3', 'h1g2']);
});

test('keeps candidates visible when the played review is honestly withheld', () => {
  const candidates = completedJob.snapshot.evidence.candidates.map(candidate =>
    candidate.roles.includes('played') ? { ...candidate, review: { kind: 'abstained' as const } } : candidate,
  );
  const job: CompletedJob = {
    ...completedJob,
    snapshot: { ...completedJob.snapshot, evidence: { candidates } },
  };
  const panel = renderMoveReview(props({ job, view: { evidenceExpanded: true } }));
  assert.match(renderedText(findNode(panel, 'section.move-review__summary')), /unavailable/i);
  assert.equal(findNodes(panel, 'button.move-review__candidate').length, 2);
  assert.equal(findNodes(panel, 'span.move-review__verdict-badge').length, 0);
});

test('renders exact terminal and draw-claim position actions without a verdict badge', () => {
  const common = {
    requestId: 'view-test',
    jobId: 'a'.repeat(32),
    engineProfile: 'sf18-smallnet-t2-h16-v1' as const,
    judgmentRevision: 'chesstory.position-commentary.response.v6',
    annotationPolicyRevision: 'chesstory.verdict-threshold-policy.v2',
    generation: 0,
    subject: completedJob.snapshot.subject,
  };
  const terminal = renderMoveReview(
    props({
      job: {
        kind: 'position-action',
        snapshot: {
          ...common,
          kind: 'position-action',
          action: { kind: 'automatic-terminal', terminal: { kind: 'checkmate', winner: 'white' } },
        },
      },
    }),
  );
  assert.match(renderedText(terminal), /Terminal position Checkmate · White/);
  assert.equal(findNodes(terminal, 'span.move-review__verdict-badge').length, 0);

  const draw = renderMoveReview(
    props({
      job: {
        kind: 'position-action',
        snapshot: {
          ...common,
          kind: 'position-action',
          action: {
            kind: 'draw-claim',
            claims: [{ rule: 'threefold_repetition', availability: 'available_now' }],
          },
        },
      },
    }),
  );
  assert.match(renderedText(draw), /Draw claim available Threefold repetition · Available now/);
});

test('keeps proof navigation and Study actions separate', () => {
  const calls: string[] = [];
  const actions: Partial<MoveReviewPanelActions> = {
    previewFrame: frame => calls.push(`preview:${frame.proofId}:${frame.ply}`),
    pinFrame: frame => calls.push(`pin:${frame.proofId}:${frame.ply}`),
    addProof: proofId => calls.push(`add:${proofId}`),
  };
  const panel = renderMoveReview(
    props({
      canWrite: true,
      actions,
      view: { evidenceExpanded: true, expandedReasonId: primaryReason.id },
    }),
  );
  const moves = findNodes(panel, 'button.move-review__proof-san');
  (moves[0]!.data?.on?.mouseenter as () => void)();
  (moves[1]!.data?.on?.click as () => void)();
  (findNode(panel, 'button.button.button-thin.move-review__add').data?.on?.click as () => void)();
  assert.deepEqual(calls, [
    'preview:reason.primary:1',
    'pin:reason.primary:2',
    'add:reason.primary',
  ]);

  const pinned = renderMoveReview(
    props({
      view: {
        evidenceExpanded: true,
        expandedReasonId: primaryReason.id,
        pinnedFrame: { proofId: primaryReason.id, ply: 2 },
      },
    }),
  );
  assert.match(renderedText(findNode(pinned, 'figcaption')), /Step 2: h1g2/);
  assert.equal(renderedBoardConfig(pinned).fen, secondFen);
});

test('renders loading, retryable faults, and unsupported states honestly', () => {
  const subject = completedJob.snapshot.subject;
  const loading = renderMoveReview(props({ job: { kind: 'loading', subject } }));
  assert.match(renderedText(loading), /Reviewing the last move/);

  const abstained = renderMoveReview(props({ job: { kind: 'abstained', subject } }));
  assert.match(renderedText(abstained), /withheld because no verified judgment/i);

  let retries = 0;
  const fault = renderMoveReview(
    props({
      job: { kind: 'fault', subject, message: 'Evidence unavailable.', retryable: true },
      actions: { retry: () => retries++ },
    }),
  );
  (findNode(fault, 'button.button.button-thin.move-review__retry').data?.on?.click as () => void)();
  assert.equal(retries, 1);

  const unsupported = renderMoveReview(
    props({
      job: {
        kind: 'unsupported',
        subject,
        reason: 'browser-unsupported',
        message: moveReviewCopy('en-US').browserUnsupported,
      },
    }),
  );
  assert.equal(findNodes(unsupported, 'button.button.button-thin.move-review__retry').length, 0);
});

test('supports roving candidate focus and clamps stale proof frames', () => {
  let selected: Uci | undefined;
  let focused = false;
  const panel = renderMoveReview(
    props({
      view: { evidenceExpanded: true },
      actions: { selectCandidate: value => (selected = value) },
    }),
  );
  const candidates = findNodes(panel, 'button.move-review__candidate');
  const event = {
    key: 'ArrowLeft',
    preventDefault: () => {},
    currentTarget: {
      parentElement: {
        querySelectorAll: () => [{ focus: () => (focused = true) }, { focus: () => {} }],
      },
    },
  } as unknown as KeyboardEvent;
  (candidates[1]!.data?.on?.keydown as (event: KeyboardEvent) => void)(event);
  assert.equal(selected, 'e2f2');
  assert.equal(focused, true);

  const stale = renderMoveReview(
    props({
      view: {
        evidenceExpanded: true,
        expandedReasonId: primaryReason.id,
        pinnedFrame: { proofId: primaryReason.id, ply: 99 },
      },
    }),
  );
  assert.match(renderedText(findNode(stale, 'figcaption')), /Step 1: e2e3/);
});
