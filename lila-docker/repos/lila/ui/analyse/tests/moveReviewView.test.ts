import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import { moveReviewEngineProfile } from 'lib/ceval/types';
import type { VNode } from 'snabbdom';
import {
  decodeMoveReviewSnapshot,
  moveReviewCopy,
  type MoveReviewCandidateReview,
  type MoveReviewJobState,
  type MoveReviewLocale,
  type MoveReviewSubject,
  type MoveReviewVerdictSymbol,
  type MoveReviewViewState,
} from '../src/moveReview';
import {
  renderMoveReview,
  renderMoveReviewNotationBadge,
  type MoveReviewPanelProps,
} from '../src/view/moveReview';
import { rawResponse } from './moveReviewTestSupport';

type JsonObject = Record<string, unknown>;
type CompletedJob = Extract<MoveReviewJobState, { kind: 'completed' }>;
type MoveReviewPanelActions = MoveReviewPanelProps['actions'];
type VerdictReview = Extract<MoveReviewCandidateReview, { kind: 'move-verdict' }>;

const object = (value: unknown): JsonObject => value as JsonObject;
const bestChoiceFixtureUrl = new URL(
  '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/best-choice-produced.json',
  import.meta.url,
);

const noopActions: MoveReviewPanelActions = {
  selectCandidate: () => {},
  toggleEvidence: () => {},
  toggleProof: () => {},
  previewFrame: () => {},
  clearPreview: () => {},
  pinFrame: () => {},
  clearPin: () => {},
  retry: () => {},
  addProof: () => {},
  viewAddedLine: () => {},
};

function fixture(url: URL): JsonObject {
  return JSON.parse(readFileSync(url, 'utf8')) as JsonObject;
}

function subjectFor(raw: JsonObject): MoveReviewSubject {
  const focus = object(raw.focus);
  const before = raw.current_fen as FEN;
  const played = focus.played_move_uci as Uci;
  const after = focus.resulting_fen as FEN;
  return {
    variant: 'standard',
    initialFen: before,
    movePrefixUci: [],
    before: { path: '' as Tree.Path, fen: before },
    played: { uci: played, san: played as San },
    after: { path: 'aa' as Tree.Path, fen: after },
  };
}

function completedFrom(raw: JsonObject): CompletedJob {
  const subject = subjectFor(raw);
  const snapshot = decodeMoveReviewSnapshot(raw, {
    requestId: raw.request_id as string,
    subject,
    engineProfile: moveReviewEngineProfile,
  });
  assert.equal(snapshot?.kind, 'completed');
  if (snapshot?.kind !== 'completed') throw new Error('Expected completed producer fixture');
  return { kind: 'completed', snapshot };
}

const completedJob = completedFrom(rawResponse() as JsonObject);
const playedCandidate = completedJob.snapshot.evidence.candidates.find(candidate =>
  candidate.roles.includes('played'),
);
assert.ok(playedCandidate);
const playedUci = playedCandidate.uci;

function verdictReview(job: CompletedJob): VerdictReview {
  const candidate = job.snapshot.evidence.candidates.find(item => item.roles.includes('played'));
  assert.equal(candidate?.review.kind, 'move-verdict');
  if (candidate?.review.kind !== 'move-verdict') throw new Error('Expected verdict review');
  return candidate.review;
}

function completedWith(symbol: MoveReviewVerdictSymbol): CompletedJob {
  const job = structuredClone(completedJob);
  const review = verdictReview(job);
  review.core.verdictSymbol = symbol;
  review.core.verdictCode =
    symbol === 'none'
      ? 'matches_reference'
      : symbol === '?!'
        ? 'inaccuracy'
        : symbol === '?'
          ? 'mistake'
          : 'blunder';
  return job;
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
    .filter((value): value is string => typeof value === 'string')
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
  drawable?: { autoShapes?: Array<{ orig: Key; dest?: Key; brush: string }> };
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

test('renders only the four v6 verdict symbols and their tones', () => {
  const cases: Array<[MoveReviewVerdictSymbol, string | undefined]> = [
    ['none', undefined],
    ['?!', 'inaccuracy'],
    ['?', 'mistake'],
    ['??', 'blunder'],
  ];
  for (const [symbol, tone] of cases) {
    const panel = renderMoveReview(props({ job: completedWith(symbol) }));
    assert.match(renderedText(findNode(panel, 'section.move-review__summary')), /e5/);
    const notation = renderMoveReviewNotationBadge(symbol, symbol);
    if (!tone) assert.equal(notation, undefined);
    else {
      assert.equal(notation?.sel, `span.move-review__notation-badge.move-review__notation-badge--${tone}`);
      assert.equal(renderedText(notation!), symbol);
    }
  }
});

test('keeps played, best, verdict, and metrics in the Assessment surface', () => {
  const panel = renderMoveReview(props({ view: { evidenceExpanded: true } }));
  const copy = moveReviewCopy('en-US');
  const candidates = findNodes(panel, 'button.move-review__candidate');
  assert.equal(candidates.length, 2);
  assert.ok(candidates.some(candidate => renderedText(candidate).includes(`${copy.best} c7c5`)));
  assert.ok(candidates.some(candidate => renderedText(candidate).includes(`${copy.played} e7e5`)));
  assert.match(renderedText(findNode(panel, 'section.move-review__summary')), /Inaccuracy/);
  assert.match(renderedText(findNode(panel, 'dl.move-review__metrics')), /54\.0% → 46\.0%/);
  assert.match(renderedText(panel), /-8\.0%p/);
});

test('renders the immutable BestChoice producer fixture with its runner-up', () => {
  const panel = renderMoveReview(props({ job: completedFrom(fixture(bestChoiceFixtureUrl)) }));
  const summary = findNode(panel, 'section.move-review__summary');
  assert.match(renderedText(summary), /Best .* · Runner-up .*:/);
  assert.equal(findNodes(summary, 'span.move-review__verdict-badge').length, 0);
});

test('keeps proof navigation, mini-board frames, and Study actions intact', () => {
  const calls: string[] = [];
  const proofId = `pv:${playedUci}`;
  const panel = renderMoveReview(
    props({
      canWrite: true,
      actions: {
        previewFrame: frame => calls.push(`preview:${frame.proofId}:${frame.ply}`),
        pinFrame: frame => calls.push(`pin:${frame.proofId}:${frame.ply}`),
        addProof: proofId => calls.push(`add:${proofId}`),
      },
      view: { evidenceExpanded: true, expandedProofId: proofId },
    }),
  );
  const moves = findNodes(panel, 'button.move-review__proof-san');
  assert.equal(renderedText(moves[0]!), 'e7e5');
  assert.equal(renderedText(moves[1]!), 'g1f3');
  (moves[0]!.data?.on?.mouseenter as () => void)();
  (moves[1]!.data?.on?.click as () => void)();
  const add = findNodes(panel, 'button.button.button-thin.move-review__add');
  assert.equal(add.length, 1);
  (add[0]!.data?.on?.click as () => void)();
  assert.deepEqual(calls, [`preview:${proofId}:1`, `pin:${proofId}:2`, `add:${proofId}`]);
  assert.deepEqual(renderedBoardConfig(panel).lastMove, ['e7', 'e5']);

  const pinned = renderMoveReview(
    props({
      view: {
        evidenceExpanded: true,
        expandedProofId: proofId,
        pinnedFrame: { proofId, ply: 2 },
      },
    }),
  );
  assert.match(renderedText(findNode(pinned, 'figcaption')), /Step 2: g1f3/);
  assert.deepEqual(renderedBoardConfig(pinned).lastMove, ['g1', 'f3']);
});

test('renders transmitted draw claims alongside Assessment', () => {
  const job = structuredClone(completedJob);
  job.snapshot.evidence.drawClaims = [{ rule: 'threefold_repetition', availability: 'available_now' }];
  const text = renderedText(renderMoveReview(props({ job })));
  assert.match(text, /Draw claim available/);
  assert.match(text, /Threefold repetition · Available now/);
});

test('supports roving candidate focus without changing chess meaning', () => {
  let selected: Uci | undefined;
  let focused = false;
  const panel = renderMoveReview(
    props({
      view: { evidenceExpanded: true },
      actions: { selectCandidate: uci => (selected = uci) },
    }),
  );
  const candidates = findNodes(panel, 'button.move-review__candidate');
  const tabs = [{ focus: () => {} }, { focus: () => (focused = true) }];
  const event = {
    key: 'ArrowRight',
    preventDefault: () => {},
    currentTarget: { parentElement: { querySelectorAll: () => tabs } },
  } as unknown as KeyboardEvent;
  (candidates[0]!.data?.on?.keydown as (value: KeyboardEvent) => void)(event);
  assert.equal(selected, playedUci);
  assert.equal(focused, true);
});

test('falls back to the first certified frame when a pinned frame is stale', () => {
  const proofId = `pv:${playedUci}`;
  const panel = renderMoveReview(
    props({
      view: {
        evidenceExpanded: true,
        expandedProofId: proofId,
        pinnedFrame: { proofId, ply: 99 },
      },
    }),
  );
  assert.match(renderedText(findNode(panel, 'figcaption')), /Step 1: e7e5/);
  assert.deepEqual(renderedBoardConfig(panel).lastMove, ['e7', 'e5']);
});

test('offers the added Study line without duplicating its add action', () => {
  const proofId = `pv:${playedUci}`;
  let viewed: string | undefined;
  const panel = renderMoveReview(
    props({
      canWrite: true,
      addedProofId: proofId,
      actions: { viewAddedLine: id => (viewed = id) },
      view: { evidenceExpanded: true, expandedProofId: proofId },
    }),
  );
  assert.match(renderedText(panel), /Line added to Study/);
  assert.equal(findNodes(panel, 'button.button.button-thin.move-review__add').length, 0);
  const viewButton = findNode(panel, 'button.button.button-thin.move-review__view-added');
  (viewButton.data?.on?.click as () => void)();
  assert.equal(viewed, proofId);
});

test('keeps forced and position-action outcomes separate from verdict reviews', () => {
  const forced = structuredClone(completedJob);
  const candidate = forced.snapshot.evidence.candidates.find(item => item.roles.includes('played'))!;
  candidate.review = {
    kind: 'forced-single-move',
    lineUcis: [playedUci, 'e5d6' as Uci],
    terminal: { kind: 'checkmate', winner: 'black' },
  };
  const forcedPanel = renderMoveReview(props({ job: forced, view: { evidenceExpanded: true } }));
  assert.match(renderedText(forcedPanel), /Forced single move/);
  assert.match(renderedText(forcedPanel), /Terminal position: Checkmate · Black/);

  const { evidence: _evidence, ...common } = completedJob.snapshot;
  const action: MoveReviewJobState = {
    kind: 'position-action',
    snapshot: {
      ...common,
      kind: 'position-action',
      action: { kind: 'automatic-terminal', terminal: { kind: 'stalemate' } },
    },
  };
  const actionPanel = renderMoveReview(props({ job: action }));
  assert.match(renderedText(actionPanel), /Terminal position.*Stalemate/);
  assert.equal(findNodes(actionPanel, 'button.move-review__proof-entry-button').length, 0);
});

test('renders loading, fault, unsupported, and abstained states honestly', () => {
  const subject = completedJob.snapshot.subject;
  assert.match(renderedText(renderMoveReview(props({ job: { kind: 'loading', subject } }))), /Reviewing/);
  assert.match(
    renderedText(
      renderMoveReview(
        props({ job: { kind: 'fault', subject, message: 'transport failed', retryable: true } }),
      ),
    ),
    /transport failed.*Retry/,
  );
  assert.match(
    renderedText(
      renderMoveReview(
        props({
          job: { kind: 'unsupported', subject, reason: 'browser-unsupported', message: 'unsupported' },
        }),
      ),
    ),
    /unsupported/,
  );
  assert.match(
    renderedText(renderMoveReview(props({ job: { kind: 'abstained', subject } }))),
    /withheld because no verified judgment/i,
  );
});
