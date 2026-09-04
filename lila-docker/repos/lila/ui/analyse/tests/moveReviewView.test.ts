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

type JsonObject = Record<string, unknown>;
type CompletedJob = Extract<MoveReviewJobState, { kind: 'completed' }>;
type MoveReviewPanelActions = MoveReviewPanelProps['actions'];
type VerdictReview = Extract<MoveReviewCandidateReview, { kind: 'move-verdict' }>;

const object = (value: unknown): JsonObject => value as JsonObject;
const objects = (value: unknown): JsonObject[] => value as JsonObject[];

const occurrenceFixtureUrl = new URL(
  '../../../../../../judgment-evaluation/fixtures/public-commentary-v6/relocation-enables-recapture-produced.json',
  import.meta.url,
);
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

function fixture(url = occurrenceFixtureUrl): JsonObject {
  return JSON.parse(readFileSync(url, 'utf8')) as JsonObject;
}

function commentary(raw: JsonObject): JsonObject {
  const reviews = objects(object(raw.result).selected_move_reviews);
  const review = reviews.find(item => object(item).commentary);
  assert.ok(review);
  return object(review.commentary);
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

const completedJob = completedFrom(fixture());
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

const producerRelationId = verdictReview(completedJob).relations[0]!.id;
const producerFirstBranchProofId = `${producerRelationId}-branch-0`;

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

function renderedTextWithout(root: VNode, selector: string): string {
  const values: string[] = [];
  const visit = (node: VNode) => {
    if (node.sel === selector) return;
    if (typeof node.text === 'string') values.push(node.text);
    for (const child of node.children || [])
      if (typeof child === 'object' && child && 'sel' in child) visit(child as VNode);
  };
  visit(root);
  return values.join(' ');
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
    assert.match(renderedText(findNode(panel, 'section.move-review__summary')), /d7d5/);
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
  assert.ok(candidates.some(candidate => renderedText(candidate).includes(`${copy.best} a8b8`)));
  assert.ok(candidates.some(candidate => renderedText(candidate).includes(`${copy.played} d7d5`)));
  assert.match(renderedText(findNode(panel, 'section.move-review__summary')), /Mistake/);
  assert.match(renderedText(findNode(panel, 'dl.move-review__metrics')), /67\.6% → 50\.0%/);
  assert.match(renderedText(panel), /-17\.6%p/);
});

test('renders the immutable BestChoice producer fixture with its runner-up', () => {
  const panel = renderMoveReview(props({ job: completedFrom(fixture(bestChoiceFixtureUrl)) }));
  const summary = findNode(panel, 'section.move-review__summary');
  assert.match(renderedText(summary), /Best .* · Runner-up .*:/);
  assert.equal(findNodes(summary, 'span.move-review__verdict-badge').length, 0);
});

test('renders the Scala-produced relocation relation as continuity, branches, and board arrows', () => {
  const job = completedFrom(fixture());
  const review = verdictReview(job);
  const relation = review.relations[0]!;
  assert.equal(relation.relationKind, 'relocation_enables_recapture');
  if (relation.relationKind !== 'relocation_enables_recapture') return;
  const panel = renderMoveReview(
    props({
      job,
      view: {
        selectedCandidateUci: relation.subjectOccurrence.moveUci,
        evidenceExpanded: true,
        expandedProofId: relation.id,
      },
    }),
  );
  const text = renderedText(panel);
  assert.match(text, /Piece continuity/);
  assert.match(text, /Secondary move/);
  assert.match(text, /e8c8/);
  assert.match(text, /e5d6/);
  assert.match(text, /d8d6/);
  assert.match(text, /e7d6/);
  assert.deepEqual(findNodes(panel, 'header.move-review__branch-header').map(renderedText), [
    'Analyzed alternative e8c8',
    'Actual move · Analysis continuation d7d5',
  ]);
  assert.deepEqual(
    findNodes(panel, 'button.move-review__proof-entry-button').map(button =>
      renderedText(findNode(button, 'strong')),
    ),
    ['Verified candidate line', moveReviewCopy('en-US').familyLabels.relocation_enables_recapture],
  );
  const path = relation.proof.proofPaths[0];
  assert.equal(
    findNodes(panel, 'section.move-review__participant').length,
    Object.keys(relation.proof.participants).length,
  );
  assert.equal(findNodes(panel, 'section.move-review__premise').length, path.premises.length);
  assert.equal(
    findNodes(findNode(panel, 'section.move-review__closed-absence'), 'li').length,
    path.closedAbsenceUses.length,
  );
  assert.equal(
    findNodes(findNode(panel, 'section.move-review__closed-state'), 'li').length,
    path.closedStateUses.length,
  );
  assert.deepEqual(renderedBoardConfig(panel).drawable?.autoShapes, [
    { orig: 'a8', dest: 'd8', brush: 'blue' },
  ]);
});

test('omits a move stage when the transmitted step provenance is absent', () => {
  const job = structuredClone(completedJob);
  const relation = verdictReview(job).relations[0]!;
  delete (relation.proof.retainedResponderBranch.steps[0] as { provenance?: unknown }).provenance;

  const panel = renderMoveReview(
    props({ job, view: { evidenceExpanded: true, expandedProofId: relation.id } }),
  );
  const stages = findNodes(panel, 'span.move-review__proof-stage').map(renderedText);
  assert.equal(stages.filter(stage => stage === 'Actual move').length, 0);
  assert.equal(stages.filter(stage => stage === 'Analyzed alternative').length, 5);
  assert.equal(stages.filter(stage => stage === 'Analysis continuation').length, 2);
});

test('preserves multiple independent occurrence relations in wire order', () => {
  const raw = fixture();
  const relations = objects(commentary(raw).occurrence_relations);
  const second = structuredClone(relations[0]!);
  const secondProof = object(second.proof);
  secondProof.source_evidence_id = 'second-independent-relation-source';
  secondProof.occurrence_id = 'e'.repeat(64);
  object(objects(secondProof.proof_paths)[0]).path_occurrence_id = 'f'.repeat(64);
  commentary(raw).occurrence_relations = [relations[0]!, second];

  const panel = renderMoveReview(props({ job: completedFrom(raw), view: { evidenceExpanded: true } }));
  assert.deepEqual(
    findNodes(panel, 'button.move-review__proof-entry-button').map(button =>
      renderedText(findNode(button, 'strong')),
    ),
    [
      'Verified candidate line',
      moveReviewCopy('en-US').familyLabels.relocation_enables_recapture,
      moveReviewCopy('en-US').familyLabels.relocation_enables_recapture,
    ],
  );
  assert.deepEqual(
    findNodes(panel, 'article.move-review__proof-entry').map(entry => entry.key),
    [
      `pv:${playedUci}`,
      `occurrence-relation-${object(relations[0]!.proof).source_evidence_id}`,
      `occurrence-relation-${secondProof.source_evidence_id}`,
    ],
  );
});

test('keeps Assessment comparison proof and occurrence relation branches separate', () => {
  const assessment = renderMoveReview(
    props({ view: { evidenceExpanded: true, expandedProofId: `pv:${playedUci}` } }),
  );
  assert.equal(findNodes(assessment, 'div.cg-wrap.is2d').length, 1);
  assert.equal(findNodes(assessment, 'section.move-review__branch').length, 0);

  const relation = renderMoveReview(
    props({ view: { evidenceExpanded: true, expandedProofId: producerRelationId } }),
  );
  assert.equal(findNodes(relation, 'div.cg-wrap.is2d').length, 2);
  assert.equal(findNodes(relation, 'section.move-review__branch').length, 2);
});

test('keeps proof navigation, mini-board frames, and Study actions intact', () => {
  const calls: string[] = [];
  const panel = renderMoveReview(
    props({
      canWrite: true,
      actions: {
        previewFrame: frame => calls.push(`preview:${frame.proofId}:${frame.ply}`),
        pinFrame: frame => calls.push(`pin:${frame.proofId}:${frame.ply}`),
        addProof: proofId => calls.push(`add:${proofId}`),
      },
      view: { evidenceExpanded: true, expandedProofId: producerRelationId },
    }),
  );
  const moves = findNodes(panel, 'button.move-review__proof-san');
  assert.equal(renderedText(moves[0]!), 'e8c8');
  assert.equal(renderedText(moves[1]!), 'a2a3');
  (moves[0]!.data?.on?.mouseenter as () => void)();
  (moves[1]!.data?.on?.click as () => void)();
  const add = findNodes(panel, 'button.button.button-thin.move-review__add');
  assert.equal(add.length, 2);
  (add[0]!.data?.on?.click as () => void)();
  assert.deepEqual(calls, [
    `preview:${producerFirstBranchProofId}:1`,
    `pin:${producerFirstBranchProofId}:2`,
    `add:${producerFirstBranchProofId}`,
  ]);
  assert.deepEqual(renderedBoardConfig(panel).lastMove, ['e8', 'c8']);

  const pinned = renderMoveReview(
    props({
      view: {
        evidenceExpanded: true,
        expandedProofId: producerRelationId,
        pinnedFrame: { proofId: producerFirstBranchProofId, ply: 2 },
      },
    }),
  );
  assert.match(renderedText(findNode(pinned, 'figcaption')), /Step 2: a2a3/);
  assert.deepEqual(renderedBoardConfig(pinned).lastMove, ['a2', 'a3']);
});

test('preserves certified proof annotations on candidate mini-boards', () => {
  const job = structuredClone(completedJob);
  const candidate = job.snapshot.evidence.candidates.find(item => item.roles.includes('best'));
  assert.equal(candidate?.review.kind, 'single-candidate-insight');
  if (candidate?.review.kind !== 'single-candidate-insight') return;
  candidate.review.proof.annotations = [
    { atPly: 1, shape: { kind: 'arrow', orig: 'd4', dest: 'd5', brush: 'green' } },
    { atPly: 1, shape: { kind: 'square', key: 'd5', brush: 'blue' } },
  ];
  const panel = renderMoveReview(
    props({
      job,
      view: {
        evidenceExpanded: true,
        selectedCandidateUci: candidate.uci,
        expandedProofId: candidate.review.proof.id,
      },
    }),
  );
  assert.deepEqual(renderedBoardConfig(panel).drawable?.autoShapes, [
    { orig: 'd4', dest: 'd5', brush: 'green' },
    { orig: 'd5', brush: 'blue' },
  ]);
});

test('shows a verdict-only response without inventing an occurrence relation', () => {
  const job = structuredClone(completedJob);
  verdictReview(job).relations = [];
  const panel = renderMoveReview(props({ job, view: { evidenceExpanded: true } }));
  assert.match(renderedText(panel), /no occurrence relation was provided/i);
  assert.equal(findNodes(panel, 'section.move-review__branch').length, 0);
});

test('limits Korean relation copy to finite structural labels', () => {
  const panel = renderMoveReview(
    props({
      locale: 'ko-KR',
      view: { evidenceExpanded: true, expandedProofId: producerRelationId },
    }),
  );
  const text = renderedText(panel);
  assert.match(text, /근거 경로 1/);
  assert.match(text, /분석 대안 e8c8/);
  assert.match(text, /실제 수 · 분석 후속 d7d5/);
  assert.match(text, /전제/);
  assert.match(text, /폐쇄 부재/);
  assert.match(text, /폐쇄 상태/);
  assert.deepEqual(moveReviewCopy('ko-KR').familyLabels, {
    relocation_enables_recapture: '기물 이동으로 가능해진 재포획',
  });
  assert.deepEqual(moveReviewCopy('en-US').familyLabels, {
    relocation_enables_recapture: 'Recapture enabled by relocation',
  });
  assert.equal('dependency' in moveReviewCopy('ko-KR').structureLabels, false);
  assert.equal('dependency' in moveReviewCopy('en-US').structureLabels, false);
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
  assert.match(renderedText(findNode(panel, 'figcaption')), /Step 1: d7d5/);
  assert.deepEqual(renderedBoardConfig(panel).lastMove, ['d7', 'd5']);
});

test('offers the added Study line without duplicating its add action', () => {
  const proofId = producerFirstBranchProofId;
  let viewed: string | undefined;
  const panel = renderMoveReview(
    props({
      canWrite: true,
      addedProofId: proofId,
      actions: { viewAddedLine: id => (viewed = id) },
      view: { evidenceExpanded: true, expandedProofId: producerRelationId },
    }),
  );
  assert.match(renderedText(panel), /Line added to Study/);
  assert.equal(findNodes(panel, 'button.button.button-thin.move-review__add').length, 1);
  const viewButton = findNode(panel, 'button.button.button-thin.move-review__view-added');
  (viewButton.data?.on?.click as () => void)();
  assert.equal(viewed, proofId);
});

test('keeps forced and position-action outcomes separate from occurrence relations', () => {
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
